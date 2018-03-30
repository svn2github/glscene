unit Unit1;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Classes,
  System.Math,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.ExtCtrls,


  GLVectorTypes,
  GLCadencer,
  GLScene,
  GLWin32Viewer,
  GLTexture,
  GLFileTGA,
  GLObjects,
  GLShadowVolume,
  GLVectorGeometry,
  GLProjectedTextures,
  GLMaterial,
  GLCoordinates,
  GLCrossPlatform,
  GLBaseClasses;

type
  TForm1 = class(TForm)
    viewer: TGLSceneViewer;
    scene: TGLScene;
    GLCadencer1: TGLCadencer;
    camera: TGLCamera;
    GLPlane1: TGLPlane;
    GLPlane2: TGLPlane;
    GLPlane3: TGLPlane;
    scenery: TGLDummyCube;
    GLSphere1: TGLSphere;
    matLib: TGLMaterialLibrary;
    Timer1: TTimer;
    Light: TGLDummyCube;
    GLLightSource1: TGLLightSource;
    ShadowVol: TGLShadowVolume;
    GLCube1: TGLCube;
    GLCube2: TGLCube;
    light2: TGLDummyCube;
    GLSphere2: TGLSphere;
    GLSphere3: TGLSphere;
    procedure GLCadencer1Progress(Sender: TObject; const deltaTime,
      newTime: Double);
    procedure FormCreate(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure ViewerMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ViewerMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ViewerMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure FormMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
  private
     
  public
     
  end;


var
  Form1: TForm1;
  projLight: TGLProjectedTextures;
  ang: single;
  mx, my, mk: integer;
  emitter1, emitter2: TGLTextureEmitter;

implementation

{$R *.DFM}

procedure TForm1.GLCadencer1Progress(Sender: TObject; const deltaTime,
  newTime: Double);
begin
     ang:= ang + deltatime*20;

     //Move or rotate?
     Light.Position.Y:= sin(DegToRadian(ang));
     light.position.x:= cos(DegToRadian(ang));

     //!!! why is this happening? (if this line is removed, matrix is not updated)
     if light2.AbsoluteMatrix.X.X = 666 then exit;

     light2.pitch(deltatime*20);

     viewer.invalidate;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
     matLib.Materials[0].Material.Texture.Image.LoadFromFile('projector.tga');
     matLib.Materials[1].Material.Texture.Image.LoadFromFile('flare.bmp');

     projLight:= TGLProjectedTextures(scene.Objects.AddNewChild(TGLProjectedTextures));
     emitter1:= TGLTextureEmitter(Light.AddNewChild(TGLTextureEmitter));
     emitter1.Material.MaterialLibrary:= matLib;
     emitter1.Material.LibMaterialName:= 'spot';
     emitter1.Name:= 'emitter1';

     emitter2:= TGLTextureEmitter(Light2.AddNewChild(TGLTextureEmitter));
     emitter2.Material.MaterialLibrary:= matLib;
     emitter2.Material.LibMaterialName:= 'spot2';
     emitter2.name:= 'emitter2';

     projLight.Emitters.AddEmitter(emitter1);
     projLight.Emitters.AddEmitter(emitter2);

     //play around with the rendering order
     scenery.MoveTo(ShadowVol);
     shadowVol.MoveTo(projLight);
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
     form1.caption:= format('Projected Textures %f', [viewer.framespersecond]);
     viewer.resetperformancemonitor;
end;

procedure TForm1.ViewerMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
     mk:= 1;
     mx:= x;
     my:= y;
end;

procedure TForm1.ViewerMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
     mk:= 0;
end;

procedure TForm1.ViewerMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
     if mk <> 1 then exit;
     if shift = [ssLeft] then
          Camera.MoveAroundTarget(my-y, mx-x)
     else if shift = [ssRight] then
          Camera.AdjustDistanceToTarget(1.0 + (y - my) / 100);
     mx:= x;
     my:= y;
end;

procedure TForm1.FormMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  Camera.AdjustDistanceToTarget(Power(1.1, WheelDelta/120));
end;

end.
