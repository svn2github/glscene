unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages,
  System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms,
  Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.Imaging.Jpeg,
  // GLS
  GLScene, GLObjects, GLCadencer, GLTexture, GLWin32Viewer,
  GLVectorGeometry, GLAsyncTimer, GLUtils, GLHUDObjects, GLBitmapFont,
  GLWindowsFont, GLKeyboard, XOpenGL, GLHeightData, GLTerrainRenderer,
  GLVectorTypes, GLMaterial, GLCoordinates, GLCrossPlatform, GLBaseClasses,
  // Graphics32
  GR32, GR32_Blend;

type
  TForm1 = class(TForm)
    GLSceneViewer1: TGLSceneViewer;
    GLScene1: TGLScene;
    GLMaterialLibrary1: TGLMaterialLibrary;
    GLCadencer1: TGLCadencer;
    Camera: TGLCamera;
    GLLightSource1: TGLLightSource;
    Scene: TGLDummyCube;
    AsyncTimer1: TGLAsyncTimer;
    GLWindowsBitmapFont1: TGLWindowsBitmapFont;
    fpscounter: TGLHUDText;
    Cam: TGLDummyCube;
    GLTerrainRenderer1: TGLTerrainRenderer;
    GLBitmapHDS1: TGLBitmapHDS;
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure FormMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure GLCadencer1Progress(Sender: TObject;
      const deltaTime, newTime: Double);
    procedure FormCreate(Sender: TObject);
    procedure AsyncTimer1Timer(Sender: TObject);
    procedure GLSceneViewer1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure GLSceneViewer1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure GLSceneViewer1MouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure CheckCameraControls(deltaTime, newTime: Double);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure explpxcombine(F: TColor32; var B: TColor32; M: TColor32);
    procedure GLTerrainRenderer1GetTerrainBounds(var l, t, r, B: Single);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

const
  movespeed = 15;
  camspeed = 3.5;
  cMapSize = 256;
  texsize = 512;

var
  Form1: TForm1;
  mx, my: Integer;
  angx, angy: Single;
  cx, cy: Integer;
  px, py: Integer;
  pressedl, pressedr, pressedm: Boolean;
  bufang, campos: TVector;
  lerpcounter, controlcounter: Single;

  terrain, explosion, terrtex, hole: TBitmap32;

  FMin, FMax: TAffineVector;

implementation

{$R *.dfm}

procedure UVPlanarMapping(Vector: TVector3f; min, max: TAffineVector;
  var U, V: Single);
var
  P, center: TAffineVector;
begin
  center := VectorScale(VectorAdd(max, min), 0.5);

  P := VectorSubtract(Vector, center);
  U := (P.X / (max.X - min.X)) + 0.5;
  V := -((P.Z / (max.Z - min.Z)) + 0.5);
end;

procedure TForm1.explpxcombine(F: TColor32; var B: TColor32; M: TColor32);
begin
  B := ColorModulate(F, B);
end;

procedure TForm1.FormKeyPress(Sender: TObject; var Key: Char);
begin
  if Key = #27 then
    close;
end;

procedure TForm1.CheckCameraControls(deltaTime, newTime: Double);
var
  bufv: TVector;
  ps: TPoint;
  curspeed: Single;

begin
  if (newTime - controlcounter) > 0.001 then
  begin
    controlcounter := newTime;

    if (iskeydown(vk_shift)) or (pressedm) then
      curspeed := movespeed / 5
    else
      curspeed := movespeed;

    if iskeydown('W') then
      campos.Z := -curspeed;
    if iskeydown('S') then
      campos.Z := curspeed;
    if iskeydown('A') then
      campos.X := curspeed;
    if iskeydown('D') then
      campos.X := -curspeed;
    if iskeydown('R') then
      campos.Y := curspeed;
    if iskeydown('F') then
      campos.Y := -curspeed;

    Cam.Move(campos.Z * deltaTime);
    Cam.Lift(campos.Y * deltaTime);
    Cam.Slide(campos.X * deltaTime);

    if (abs(bufang.X) >= 0.001) or (abs(bufang.Y) >= 0.001) then
    begin
      Cam.Turn(bufang.X);
      Camera.Pitch(bufang.Y);
    end;
  end;

  if (newTime - lerpcounter) < 0.01 then
    exit;
  lerpcounter := newTime;

  if not vectorisnull(campos) then
  begin
    bufv := NullhmgVector;
    campos := vectorlerp(campos, bufv, 0.1);
  end;

  if (abs(bufang.X) >= 0.001) or (abs(bufang.Y) >= 0.001) then
  begin
    bufv := NullhmgVector;
    bufang := vectorlerp(bufang, bufv, 0.2);
  end;

  ps.X := cx;
  ps.Y := cy;

  // if cx=glsceneviewer1.Width-1 then ps.x:=1 else if cx=0 then ps.x:=glsceneviewer1.Width-2; //for fullscreen

  if (pressedr) then
  begin
    // if (ps.x=1) or (ps.x=glsceneviewer1.Width-2) then setcursorpos (ps.x,ps.y); //for fullscreen
    angx := -(cx - px) * camspeed * 0.019;
    angy := (cy - py) * camspeed * 0.019;
    bufang := vectormake(angx, angy, 0);
  end;

  px := ps.X;
  py := ps.Y;
end;

procedure TForm1.GLCadencer1Progress(Sender: TObject;
  const deltaTime, newTime: Double);
begin
  CheckCameraControls(deltaTime, newTime);
  GLSceneViewer1.Invalidate;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  SetCurrentDir(ExtractFileDir(ParamStr(0)));

  terrain := TBitmap32.create;
  explosion := TBitmap32.create;
  terrtex := TBitmap32.create;
  hole := TBitmap32.create;

  explosion.LoadFromFile('crater.bmp');
  terrain.LoadFromFile('terrain.bmp');
  terrtex.LoadFromFile('snow512.jpg');
  hole.LoadFromFile('hole.bmp');

  explosion.DrawMode := dmCustom;
  explosion.OnPixelCombine := explpxcombine;
  hole.DrawMode := dmCustom;
  hole.OnPixelCombine := explpxcombine;

  GLBitmapHDS1.Picture.LoadFromFile('terrain.bmp');
  terrain.DrawTo(GLBitmapHDS1.Picture.Bitmap.canvas.handle, 0, 0);

  GLMaterialLibrary1.LibMaterialByName('Terrain')
    .Material.Texture.Image.GetBitmap32.Assign(terrtex);

  FMin := GLTerrainRenderer1.Position.AsAffineVector;
  FMax := affinevectormake(cMapSize * GLTerrainRenderer1.scale.X, 0,
    cMapSize * GLTerrainRenderer1.scale.Y);

  cx := GLSceneViewer1.Width div 2;
  cy := GLSceneViewer1.height div 2;

  setcursorpos(cx, cy);
end;

procedure TForm1.AsyncTimer1Timer(Sender: TObject);
begin
  Caption := FloatToStr(GLSceneViewer1.FramesPerSecond);
  GLSceneViewer1.ResetPerformanceMonitor;
end;

procedure TForm1.GLSceneViewer1MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Button = TMouseButton(mbleft) then
    pressedl := false;
  if Button = TMouseButton(mbright) then
    pressedr := false;
  if Button = TMouseButton(mbMiddle) then
    pressedm := false;
end;

procedure TForm1.GLSceneViewer1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  rayStart, rayVector, iPoint, iNormal: TVector;

  U, V: Single;

  texx, texy, terx, tery: Integer;

begin
  if Button = TMouseButton(mbleft) then
    pressedl := true;
  if Button = TMouseButton(mbright) then
    pressedr := true;
  if Button = TMouseButton(mbMiddle) then
    pressedm := true;

  px := X;
  py := Y;

  if pressedl then
  begin
    SetVector(rayStart, Camera.AbsolutePosition);
    SetVector(rayVector, GLSceneViewer1.Buffer.ScreenToVector
      (affinevectormake(px, GLSceneViewer1.height - py, 0)));
    NormalizeVector(rayVector);

    if GLTerrainRenderer1.RayCastIntersect(rayStart, rayVector, @iPoint,
      @iNormal) then
    begin
      UVPlanarMapping(affinevectormake(iPoint), FMin, FMax, U, V);

      texx := Trunc(texsize * U);
      texy := Trunc(texsize * V);

      terx := Trunc(cMapSize * U);
      tery := Trunc(cMapSize * V);

      explosion.DrawTo(terrain, rect(terx - 50, tery - 50, terx + 50,
        tery + 50));

      hole.Rotate90;
      hole.DrawTo(terrtex, rect(texx - 100, texy - 100, texx + 100,
        texy + 100));

      terrain.DrawTo(GLBitmapHDS1.Picture.Bitmap.canvas.handle, 0, 0);

      GLMaterialLibrary1.LibMaterialByName('Terrain')
        .Material.Texture.Image.GetBitmap32.Assign(terrtex);
      GLMaterialLibrary1.LibMaterialByName('Terrain')
        .Material.Texture.Image.NotifyChange(self);
      GLBitmapHDS1.MarkDirty(rect(terx - 50, tery - 50, terx + 50, tery + 50));
    end;
  end;
end;

procedure TForm1.GLSceneViewer1MouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
begin
  cx := X;
  cy := Y;
end;

procedure TForm1.FormMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
var
  curspeed: Single;
begin
  if (iskeydown(vk_shift) or (pressedm)) then
    curspeed := abs((movespeed / 5) * (WheelDelta / 50))
  else
    curspeed := abs(movespeed * (WheelDelta / 50));

  campos.Y := curspeed * sign(WheelDelta);
end;

procedure TForm1.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  AsyncTimer1.enabled := false;
end;

procedure TForm1.GLTerrainRenderer1GetTerrainBounds(var l, t, r, B: Single);
begin
  l := 0;
  t := cMapSize;
  r := cMapSize;
  B := 0;
end;

end.
