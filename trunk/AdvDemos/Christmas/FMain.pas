{: GLScene Christmas 2002 "ScreenSaver".<p>

   Won't save your screen from anything though ;)<p>

   The scene is made up from a few meshes, some GLScene objects, several
   lens-flares and particle effects components, and a 2 text. BASS is used
   for the sound part (3D positionned fire loop, and mp3 playback).<br>
   Wrapped gifts appear around christmas every year.<p>

   Assembled from bits from the web, should be royalty free, but I don't have
   the means to check... so if you have clues about any of them:<p>

   Models: from 3DCafe.com<br>
   Textures: various origins, some from 3dtextures.fr.st, others made by me<br>
   Music: unknown origin, was in a "royalty free" download package<br>

   http://glscene.org
}
unit FMain;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, GLScene, GLVectorFileObjects, GLObjects, GLWin32Viewer,
  GLTexture, ExtCtrls, GLCadencer, GLSkydome, GLParticleFX, VectorGeometry,
  GLLensFlare, GLSound, GLSMBASS, GLBitmapFont, GLWindowsFont, GLHUDObjects,
  ScreenSaver, GLShadowPlane, GLFile3DS, GLGeomObjects, GLMaterial,
  GLCoordinates, BaseClasses, GLCrossPlatform, GLColor, GLFileWAV;

type
  TMain = class(TForm)
    Scene: TGLScene;
    Viewer: TGLSceneViewer;
    GLCamera: TGLCamera;
    DCFirTree: TGLDummyCube;
    FFFirTree: TGLFreeForm;
    LSRoom: TGLLightSource;
    POFirTree2: TGLProxyObject;
    POFirTree3: TGLProxyObject;
    Timer: TTimer;
    GLCadencer: TGLCadencer;
    DCCameraTarget: TGLDummyCube;
    FFFirePlace: TGLFreeForm;
    GLMaterialLibrary: TGLMaterialLibrary;
    LSFire: TGLLightSource;
    PFXFire: TGLPolygonPFXManager;
    DCFireSource: TGLDummyCube;
    ParticleFXRenderer: TGLParticleFXRenderer;
    CYLog: TGLCylinder;
    DCLensFlares: TGLDummyCube;
    GLLensFlare1: TGLLensFlare;
    GLLensFlare2: TGLLensFlare;
    GLLensFlare3: TGLLensFlare;
    GLLensFlare4: TGLLensFlare;
    GLSMBASS: TGLSMBASS;
    GLSoundLibrary: TGLSoundLibrary;
    DCDecoWhite: TGLDummyCube;
    DCTemplates: TGLDummyCube;
    SPWhiteBall: TGLSphere;
    GLProxyObject1: TGLProxyObject;
    SPGoldBall: TGLSphere;
    GLProxyObject2: TGLProxyObject;
    DCDecoGold: TGLDummyCube;
    GLProxyObject3: TGLProxyObject;
    LSFireLens: TGLLensFlare;
    GLLensFlare5: TGLLensFlare;
    GLProxyObject4: TGLProxyObject;
    GLProxyObject5: TGLProxyObject;
    GLProxyObject6: TGLProxyObject;
    GLLensFlare6: TGLLensFlare;
    PFXTree: TGLPolygonPFXManager;
    FTCountDown: TGLFlatText;
    GLWindowsBitmapFont: TGLWindowsBitmapFont;
    GLCube1: TGLCube;
    DCGifts: TGLDummyCube;
    GLCube2: TGLCube;
    HSGLScene: TGLHUDSprite;
    ScreenSaver: TScreenSaver;
    GLShadowPlane: TGLShadowPlane;
    DCTree: TGLDummyCube;
    GLCube3: TGLCube;
    GLCube4: TGLCube;
    DCFire: TGLDummyCube;
    procedure FormCreate(Sender: TObject);
    procedure ViewerMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ViewerMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure TimerTimer(Sender: TObject);
    procedure GLCadencerProgress(Sender: TObject; const deltaTime,
      newTime: Double);
    procedure FormResize(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure ViewerDblClick(Sender: TObject);
    procedure ScreenSaverCloseQuery(Sender: TObject;
      var CanClose: Boolean);
    procedure ScreenSaverPreview(Sender: TObject; previewHwnd: HWND);
    procedure ScreenSaverExecute(Sender: TObject);
    procedure ScreenSaverPropertiesRequested(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    mx, my : Integer;
    fireLight : Single;
    inPreview, inSaver : Boolean;
    bStream : Cardinal;
  end;

var
  Main: TMain;

implementation

{$R *.dfm}

uses Jpeg, Bass;

procedure TMain.FormCreate(Sender: TObject);
begin
   Randomize;
   SetCurrentDir(ExtractFilePath(Application.ExeName)+'data');
   FFFirTree.LoadFromFile('firtree.3ds');
   FFFirePlace.LoadFromFile('fireplace.3ds');
   fireLight:=0.5;
end;

procedure TMain.ViewerMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
   mx:=x; my:=y;
end;

procedure TMain.ViewerMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
   if Shift=[ssLeft] then begin
      GLCamera.MoveAroundTarget(my-y, mx-x);
      mx:=x; my:=y;
   end;
end;

procedure TMain.TimerTimer(Sender: TObject);
var
   i : Integer;
   t : TDateTime;
   buf : String;
   y, m, d : Word;
begin
   Caption:=Format('%.1f FPS', [Viewer.FramesPerSecond]);
   Viewer.ResetPerformanceMonitor;

   if GLSMBASS.Active and (bStream=0) then begin
      bStream:=BASS_StreamCreateFile(false, PAnsiChar('Jingle_Bells_64.mp3'), 0, 0, BASS_STREAM_AUTOFREE);
      BASS_ChannelPlay(bStream, False);
   end;

   DecodeDate(Now, y, m, d);
   t:=EncodeDate(y, 12, 25)-Now;
   if t<0 then begin
      FTCountDown.Text:='Merry Christmas!';
   end;
   if (t<1) and (t>-1) then
      DCGifts.Visible:=True;
   if t>=2 then begin
      buf:=IntToStr(Trunc(t))+' days, ';
      i:=Round(Frac(t)*24);
      if i>1 then
         buf:=buf+IntToStr(i)+' hours...'
      else buf:=buf+IntToStr(i)+' hour...';
      FTCountDown.Text:=buf;
   end else begin
      t:=t*24;
      if t>1 then begin
         buf:=IntToStr(Trunc(t))+' hours, ';
         i:=Round(Frac(t)*60);
         if i>1 then
            buf:=buf+IntToStr(i)+' minutes...'
         else buf:=buf+IntToStr(i)+' minute...';
         FTCountDown.Text:=buf;
      end else begin
         t:=t*60;
         FTCountDown.Text:= IntToStr(Trunc(t))+' minutes, '
                           +IntToStr(Round(Frac(t)*60))+' seconds...';
      end;
   end;
end;

procedure TMain.GLCadencerProgress(Sender: TObject; const deltaTime,
  newTime: Double);
begin
   Viewer.Invalidate;
   fireLight:=ClampValue(fireLight+Random*0.4-0.2, 0, 1);
   LSFire.Diffuse.Color:=VectorLerp(clrYellow, VectorMake(0.5, 0, 0, 1),
                                    fireLight);
   LSFire.Position.Y:=fireLight*0.1;

   if inPreview then
      HSGLScene.Visible:=False;
   with HSGLScene do if Visible then begin
      with Material.FrontProperties.Diffuse do begin
         Alpha:=Alpha-deltaTime*0.15;
         if Alpha<0.01 then
            Visible:=False;
      end;
   end;
end;

procedure TMain.FormResize(Sender: TObject);
begin
   GLCamera.SceneScale:=Width/640;
   with HSGLScene do if Visible then
      Position.X:=Self.Width-200;
   if (Width>=Screen.Width) then
      ViewerDblClick(Self);
end;

procedure TMain.FormKeyPress(Sender: TObject; var Key: Char);
begin
   Key:=#0;
   Application.Terminate;
end;

procedure TMain.ViewerDblClick(Sender: TObject);
begin
   if (not inPreview) and (not inSaver) and (not Application.Terminated) and (BorderStyle<>bsNone) then begin
      BorderStyle:=bsNone;
      FormStyle:=fsStayOnTop;
      Align:=alClient;
   end;
end;

procedure TMain.ScreenSaverCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
   Application.Terminate;
   CanClose:=True;
end;

procedure TMain.ScreenSaverPreview(Sender: TObject; previewHwnd: HWND);
begin
   inPreview:=True;
end;

procedure TMain.ScreenSaverExecute(Sender: TObject);
begin
   inSaver:=True;
end;

procedure TMain.ScreenSaverPropertiesRequested(Sender: TObject);
begin
   ShowMessage( 'GLScene Christmas 2002'#13#10#13#10
               +'http://glscene.org');
end;

end.
