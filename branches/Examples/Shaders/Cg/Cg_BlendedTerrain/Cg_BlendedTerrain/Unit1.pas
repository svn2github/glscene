unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages,
  System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.Imaging.JPEG,
  //
  GLCadencer, GLTexture, GLCgShader, GLScene,
  GLTerrainRenderer, GLHeightData, GLHUDObjects,  //Gaps in terrain mesh!
  GLWin32Viewer, GLAsyncTimer, GLNavigator,
  GLBitmapFont, GLWindowsFont, GLUserShader, GLMaterial,
  GLCoordinates, GLCrossPlatform, GLBaseClasses, GLRenderContextInfo, OpenGLTokens;

type
  TForm1 = class(TForm)
    GLScene1: TGLScene;
    GLSceneViewer1: TGLSceneViewer;
    GLCustomHDS1: TGLCustomHDS;
    Camera: TGLCamera;
    Terrain: TGLTerrainRenderer;
    CgBlendedTexture: TCgShader;
    MLTerrain: TGLMaterialLibrary;
    GLCadencer1: TGLCadencer;
    AsyncTimer1: TGLAsyncTimer;
    GLNavigator1: TGLNavigator;
    GLHUDText1: TGLHUDText;
    GLWindowsBitmapFont1: TGLWindowsBitmapFont;
    GLUserInterface1: TGLUserInterface;
    GLUserShader1: TGLUserShader;
    procedure GLCustomHDS1StartPreparingData(heightData: TGLHeightData);
    procedure FormCreate(Sender: TObject);
    procedure GLCadencer1Progress(Sender: TObject; const deltaTime,
      newTime: Double);
    procedure AsyncTimer1Timer(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure TerrainGetTerrainBounds(var l, t, r, b: Single);
    procedure FormKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure CgBlendedTextureUnApplyFP(CgProgram: TCgProgram);
    procedure CgBlendedTextureApplyFP(CgProgram: TCgProgram;
      Sender: TObject);
    procedure GLUserShader1DoApply(Sender: TObject;
      var rci: TGLRenderContextInfo);
    procedure GLUserShader1DoUnApply(Sender: TObject; Pass: Integer;
      var rci: TGLRenderContextInfo; var Continue: Boolean);
  private
     
    FHFFData : TStream;
  public
     
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

uses
  GLApplicationFileIO, GLKeyboard, CgGL, OpenGL1x;

procedure TForm1.FormCreate(Sender: TObject);
begin
  SetCurrentDir(ExtractFilePath(Application.ExeName)+'media');
  FHFFData:=CreateFileStream('volcano.hff', fmOpenRead);
  with MLTerrain do begin
    AddTextureMaterial('BlendMap','volcano_BM.jpg');
    AddTextureMaterial('Sand','sand.jpg');
    AddTextureMaterial('Grass','grass.jpg');
    AddTextureMaterial('Rock','rock.jpg');
    with AddTextureMaterial('LightMap','volcano_LM.jpg') do begin
      TextureScale.SetPoint(1,-1,1);
      Material.MaterialOptions:=[moNoLighting];
      Material.Texture.TextureMode:=tmReplace;
    end;
  end;

  GLUserInterface1.MouseLookActive:=True;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  GLUserInterface1.MouseLookActive:=False;

  FHFFData.Free;
end;

procedure TForm1.GLCustomHDS1StartPreparingData(heightData: TGLHeightData);
var
  oldType : TGLHeightDataType;
  i,j,xl,yt,s : Integer;
  h : Word;
begin
  if Assigned(FHFFData) then begin;
    xl:=heightData.XLeft;
    yt:=heightData.YTop;
    if xl<0 then xl:=xl+1024;
    if yt<0 then yt:=yt+1024;
    s:=heightData.Size;
    oldType:=heightData.DataType;
    heightData.Allocate(hdtSmallInt);
    for j:=0 to s-1 do begin
      FHFFData.Position:=2*(xl+(j+yt)*1024)+64;
      for i:=0 to s-1 do begin
        FHFFData.Read(h,2);
        heightData.SmallIntRaster[j][i]:=h-32768;
      end;
    end;
    if oldType<>hdtSmallInt then
      heightData.DataType:=oldType;
    heightData.DataState:=hdsReady;
  end else
    heightData.DataState:=hdsNone;
end;

procedure TForm1.TerrainGetTerrainBounds(var l, t, r,
  b: Single);
begin
  l:=0;
  t:=1024;
  r:=1024;
  b:=0;
end;

procedure TForm1.GLCadencer1Progress(Sender: TObject; const deltaTime,
  newTime: Double);
var
   speed : Single;
begin
  speed:=25;
  if IsKeyDown(VK_SHIFT) then begin
    speed:=speed*10;
  end;
  if IsKeyDown(VK_UP) or IsKeyDown('W') or IsKeyDown('Z') then
    GLNavigator1.MoveForward(deltaTime*speed)
  else if IsKeyDown(VK_DOWN) or IsKeyDown('S') then
    GLNavigator1.MoveForward(-deltaTime*speed);
  if IsKeyDown(VK_LEFT) or IsKeyDown('A') or IsKeyDown('Q') then
    GLNavigator1.StrafeHorizontal(-deltaTime*speed)
  else if IsKeyDown(VK_RIGHT) or IsKeyDown('D') then
    GLNavigator1.StrafeHorizontal(deltaTime*speed);
  if IsKeyDown(VK_NEXT) then
    GLNavigator1.StrafeVertical(-deltaTime*speed)
  else if IsKeyDown(VK_PRIOR) then
    GLNavigator1.StrafeVertical(deltaTime*speed);
  if Camera.Position.Z<Terrain.InterpolatedHeight(Camera.Position.AsVector)+3 then
    Camera.Position.Z:=Terrain.InterpolatedHeight(Camera.Position.AsVector)+3;

  GLUserInterface1.MouseUpdate;
  GLUserInterface1.MouseLook;

  GLSceneViewer1.Invalidate;
end;

procedure TForm1.AsyncTimer1Timer(Sender: TObject);
begin
  GLHUDText1.Text:='Blended Terrain - '+GLSceneViewer1.FramesPerSecondText;
  GLSceneViewer1.ResetPerformanceMonitor;
end;

procedure TForm1.FormKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  case Key of
    27 : Form1.Close;
  end;
end;

procedure TForm1.CgBlendedTextureApplyFP(CgProgram: TCgProgram;
  Sender: TObject);
begin
  with CGProgram.ParamByName('blendmap') do begin
    SetAsTexture2D(MLTerrain.LibMaterialByName('BlendMap').Material.Texture.Handle);
    EnableTexture;
  end;
  with CGProgram.ParamByName('channel1') do begin
    SetAsTexture2D(MLTerrain.LibMaterialByName('Sand').Material.Texture.Handle);
    EnableTexture;
  end;
  with CGProgram.ParamByName('channel2') do begin
    SetAsTexture2D(MLTerrain.LibMaterialByName('Grass').Material.Texture.Handle);
    EnableTexture;
  end;
  with CGProgram.ParamByName('channel3') do begin
    SetAsTexture2D(MLTerrain.LibMaterialByName('Rock').Material.Texture.Handle);
    EnableTexture;
  end;
end;

procedure TForm1.CgBlendedTextureUnApplyFP(CgProgram: TCgProgram);
begin
  CGProgram.ParamByName('blendmap').DisableTexture;
  CGProgram.ParamByName('channel1').DisableTexture;
  CGProgram.ParamByName('channel2').DisableTexture;
  CGProgram.ParamByName('channel3').DisableTexture;
end;

procedure TForm1.GLUserShader1DoApply(Sender: TObject;
  var rci: TGLRenderContextInfo);
begin
  if GLSceneViewer1.Buffer.LimitOf[limNbTextureUnits]<4 then
    GLUserShader1.Enabled:=False;

  CgBlendedTexture.Apply(rci, Sender);
end;

procedure TForm1.GLUserShader1DoUnApply(Sender: TObject; Pass: Integer;
  var rci: TGLRenderContextInfo; var Continue: Boolean);
begin
  if Pass = 1 then begin
    CgBlendedTexture.UnApply(rci);
    MLTerrain.LibMaterialByName('LightMap').Apply(rci);

    glPushAttrib(GL_ENABLE_BIT or GL_DEPTH_BUFFER_BIT or GL_COLOR_BUFFER_BIT);
    glDepthFunc(GL_EQUAL);
    glEnable(GL_BLEND);
    glBlendFunc(GL_DST_COLOR, GL_ZERO);

    Continue:=True;
    Exit;
  end;

  glPopAttrib;

  MLTerrain.LibMaterialByName('LightMap').UnApply(rci);

  Continue:=False;
end;

end.
