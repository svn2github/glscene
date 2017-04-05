unit Unit1;

interface

uses
  Winapi.Windows,
  System.SysUtils,
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Imaging.jpeg,
  Vcl.ExtCtrls,

  GLScene,
  GLState,
  GLTerrainRenderer,
  GLFireFX,
  GLTexture,
  GLCadencer,
  GLVectorFileObjects,
  GLObjects,
  GLSkydome,
  GLHeightData,
  GLMaterial,
  GLWin32Viewer,
  GLFile3DS,
  GLKeyboard,
  GLCoordinates,
  GLCrossPlatform,
  GLBaseClasses;

type
  tsubmarine = record
    x, y, z, dx, dy, dz: single;
    behaviour: (moving, waiting);
  end;

  TMainForm = class(TForm)
    GLSceneViewer1: TGLSceneViewer;
    GLBitmapHDS1: TGLBitmapHDS;
    GLScene1: TGLScene;
    GLCamera1: TGLCamera;
    DummyCube1: TGLDummyCube;
    TerrainRenderer1: TGLTerrainRenderer;
    Timer1: TTimer;
    GLCadencer1: TGLCadencer;
    GLMaterialLibrary1: TGLMaterialLibrary;
    FreeForm1: TGLFreeForm;
    GLFireFXManager1: TGLFireFXManager;
    DummyCube2: TGLDummyCube;
    GLCamera2: TGLCamera;
    FreeForm2: TGLFreeForm;
    FreeForm3: TGLFreeForm;
    GLFireFXManager2: TGLFireFXManager;
    DummyCube3: TGLDummyCube;
    GLFireFXManager3: TGLFireFXManager;
    DummyCube4: TGLDummyCube;
    FreeForm4: TGLFreeForm;
    FreeForm5: TGLFreeForm;
    DummyCube5: TGLDummyCube;
    FreeForm6: TGLFreeForm;
    FreeForm7: TGLFreeForm;
    DummyCube6: TGLDummyCube;
    FreeForm8: TGLFreeForm;
    FreeForm9: TGLFreeForm;
    elisa1: TGLFreeForm;
    elisa2: TGLFreeForm;
    elisa3: TGLFreeForm;
    procedure GLSceneViewer1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; x, y: Integer);
    procedure GLSceneViewer1MouseMove(Sender: TObject; Shift: TShiftState;
      x, y: Integer);
    procedure Timer1Timer(Sender: TObject);
    procedure GLCadencer1Progress(Sender: TObject;
      const deltaTime, newTime: Double);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure checkai;
  private
  public
    mx, my: Integer;
    fullScreen: Boolean;
    FCamHeight: single;
  end;

var
  sub, fatsub: tsubmarine;
  MainForm: TMainForm;
  dspeed: single;

implementation

{$R *.DFM}

procedure TMainForm.FormCreate(Sender: TObject);
begin
  fullScreen := false;
  dspeed := 0;
  SetCurrentDir(ExtractFilePath(Application.ExeName) + '\models');
  FreeForm8.LoadFromFile('fatsub.3ds');
  FreeForm9.LoadFromFile('kokpit2.3ds');
  FreeForm8.Material.Texture.Image.LoadFromFile('podmorni.jpg');
  FreeForm9.Material.Texture.Image.LoadFromFile('podmorni.jpg');

  FreeForm1.LoadFromFile('podmor.3ds');
  FreeForm2.LoadFromFile('elisa.3ds');
  FreeForm3.LoadFromFile('kokpit.3ds');
  FreeForm1.Material.Texture.Image.LoadFromFile('podmorni.jpg');
  FreeForm2.Material.Texture.Image.LoadFromFile('podmorni.jpg');
  FreeForm3.Material.Texture.Image.LoadFromFile('podmorni.jpg');

  FreeForm4.LoadFromFile('tstaklo.3ds');
  FreeForm4.Material.Texture.Image.LoadFromFile('snow512.jpg');
  FreeForm5.LoadFromFile('toranj.3ds');
  FreeForm5.Material.Texture.Image.LoadFromFile('toranj.jpg');

  FreeForm6.LoadFromFile('tstaklo.3ds');
  FreeForm6.Material.Texture.Image.LoadFromFile('snow512.jpg');
  FreeForm7.LoadFromFile('toranj.3ds');
  FreeForm7.Material.Texture.Image.LoadFromFile('toranj.jpg');
  elisa1.LoadFromFile('elisa.3ds');
  elisa1.Material.Texture.Image.LoadFromFile('podmorni.jpg');
  elisa2.LoadFromFile('elisa.3ds');
  elisa2.Material.Texture.Image.LoadFromFile('podmorni.jpg');
  elisa3.LoadFromFile('elisa.3ds');
  elisa3.Material.Texture.Image.LoadFromFile('podmorni.jpg');

  // 8 MB height data cache
  // Note this is the data size in terms of elevation samples, it does not
  // take into account all the data required/allocated by the renderer
  GLBitmapHDS1.MaxPoolSize := 8 * 1024 * 1024; // bilo 8
  // specify height map data
  GLBitmapHDS1.Picture.LoadFromFile('terrain.bmp');
  // load the texture maps
  GLMaterialLibrary1.Materials[0].Material.Texture.Image.LoadFromFile
    ('snow512.jpg');
  GLMaterialLibrary1.Materials[1].Material.Texture.Image.LoadFromFile
    ('detailmap.jpg');
  // apply texture map scale (our heightmap size is 256)
  TerrainRenderer1.TilesPerTexture := 256 / TerrainRenderer1.TileSize;
  // Could've been done at design time, but it the it hurts the eyes ;)
  GLSceneViewer1.Buffer.BackgroundColor := clBlack;
  // Move camera starting point to an interesting hand-picked location
  { DummyCube1.Position.X:=570;
    DummyCube1.Position.Z:=-385; }
  DummyCube1.Turn(90);
  // Initial camera height offset (controled with pageUp/pageDown)
  FCamHeight := 10;
  { with skydome1 do begin
    Bands[1].StopColor.AsWinColor:=RGB(0, 0, 16);
    Bands[1].StartColor.AsWinColor:=RGB(0, 0, 8);
    Bands[0].StopColor.AsWinColor:=RGB(0, 0, 8);
    Bands[0].StartColor.AsWinColor:=RGB(0, 0, 0);
    with Stars do begin
    AddRandomStars(700, clWhite, True);   // many white stars
    AddRandomStars(100, RGB(255, 200, 200), True);  // some redish ones
    AddRandomStars(100, RGB(200, 200, 255), True);  // some blueish ones
    AddRandomStars(100, RGB(255, 255, 200), True);  // some yellowish ones
    end; }
  GLSceneViewer1.Buffer.BackgroundColor := rgb(0, 0, 160);
  with GLSceneViewer1.Buffer.FogEnvironment do
  begin
    FogColor.AsWinColor := rgb(0, 0, 160);
    FogStart := -FogStart; // Fog is used to make things darker
  end;
end;

procedure TMainForm.GLCadencer1Progress(Sender: TObject;
  const deltaTime, newTime: Double);
var
  speed: single;
begin
  // checkai;
  // handle keypresses

  { if IsKeyDown(VK_SHIFT) then
    speed:=300*deltaTime
    else } speed := dspeed * 30 * deltaTime + dspeed;
  FreeForm2.Roll(speed * 20);
  elisa1.Roll(20);
  elisa2.Roll(20);
  elisa3.Roll(20);
  DummyCube2.Roll(speed * 20);
  FreeForm8.Turn(0.08);
  FreeForm8.Move(-0.2);
  // zoom pri kretanju
  GLCamera1.FocalLength := 50 - dspeed * 7;
  if GLCamera1.FocalLength < 20 then
    GLCamera1.FocalLength := 20;

  // with GLCamera1.Position do begin
  DummyCube1.Translate(FreeForm1.direction.z * speed, -FreeForm1.direction.y *
    speed, -FreeForm1.direction.x * speed);
  if IsKeyDown(VK_UP) then
  begin
    FreeForm1.Pitch(0.5);
    GLCamera1.Pitch(0.5);
    // GLCamera1.MoveAroundTarget(-1, 0);
  end;
  if IsKeyDown(VK_DOWN) then
  begin
    FreeForm1.Pitch(-0.5);
    GLCamera1.Pitch(-0.5);
    // GLCamera1.MoveAroundTarget(1, 0);
  end;
  if IsKeyDown(VK_LEFT) then
  begin
    // DummyCube1.Translate(-X*speed, 0, -Z*speed);
    // freeform1.Turn(-1);
    FreeForm1.Turn(-0.5);
    GLCamera1.Turn(0.5);
    // GLCamera1.MoveAroundTarget(0, 1);
  end;
  if IsKeyDown(VK_RIGHT) then
  begin
    // DummyCube1.Translate(X*speed, 0, Z*speed);
    // freeform1.Turn(1);
    FreeForm1.Turn(0.5);
    GLCamera1.Turn(-0.5);
    // GLCamera1.MoveAroundTarget(0, -1);
  end;
  if IsKeyDown(',') then
  begin
    FreeForm1.Roll(-0.5);
  end;
  if IsKeyDown('.') then
  begin
    FreeForm1.Roll(0.5);
  end;
  if IsKeyDown('a') then
    if dspeed < 2 then
      dspeed := dspeed + 0.01;
  if IsKeyDown('z') then
    if dspeed > -0.5 then
      dspeed := dspeed - 0.01;

  { if IsKeyDown(VK_PRIOR) then
    FCamHeight:=FCamHeight+10*speed;
    if IsKeyDown(VK_NEXT) then
    FCamHeight:=FCamHeight-10*speed; }
  if IsKeyDown(VK_ESCAPE) then
    Close;
  // end;
  // don't drop through terrain!

  with DummyCube1.Position do
    if y < TerrainRenderer1.InterpolatedHeight(AsVector) then
      y := TerrainRenderer1.InterpolatedHeight(AsVector) + FCamHeight;

end;

// Standard mouse rotation & FPS code below

procedure TMainForm.GLSceneViewer1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; x, y: Integer);
begin
  mx := x;
  my := y;
end;

procedure TMainForm.GLSceneViewer1MouseMove(Sender: TObject; Shift: TShiftState;
  x, y: Integer);
begin
  if ssLeft in Shift then
  begin
    GLCamera1.MoveAroundTarget(my - y, mx - x);
    mx := x;
    my := y;
  end;
end;

procedure TMainForm.Timer1Timer(Sender: TObject);
begin
  GLSceneViewer1.ResetPerformanceMonitor;
end;

procedure TMainForm.FormKeyPress(Sender: TObject; var Key: Char);
begin
  case Key of
    'm':
      begin
        if FreeForm1.Material.Texture.MappingMode = tmmuser then
          FreeForm1.Material.Texture.MappingMode := tmmCubeMapNormal
        else
          FreeForm1.Material.Texture.MappingMode := tmmuser;
      end;

    'c':
      begin
        if GLSceneViewer1.camera = GLCamera1 then
        begin
          GLSceneViewer1.camera := GLCamera2;
          // freeform1.visible:=false;
          // glfirefxmanager1.Disabled :=true;
          FreeForm1.NormalsOrientation := mnoInvert;
        end
        else
        begin
          GLSceneViewer1.camera := GLCamera1;
          // freeform1.visible:=true;
          FreeForm1.NormalsOrientation := mnoDefault;
          // glfirefxmanager1.Disabled :=false;
        end;
      end;
    'w', 'W':
      with GLMaterialLibrary1.Materials[0].Material do
      begin
        if PolygonMode = pmLines then
          PolygonMode := pmFill
        else
          PolygonMode := pmLines;
      end;
    '+':
      if GLCamera1.DepthOfView < 2000 then
      begin
        GLCamera1.DepthOfView := GLCamera1.DepthOfView * 1.2;
        with GLSceneViewer1.Buffer.FogEnvironment do
        begin
          FogEnd := FogEnd * 1.2;
          FogStart := FogStart * 1.2;
        end;
      end;
    '-':
      if GLCamera1.DepthOfView > 300 then
      begin
        GLCamera1.DepthOfView := GLCamera1.DepthOfView / 1.2;
        with GLSceneViewer1.Buffer.FogEnvironment do
        begin
          FogEnd := FogEnd / 1.2;
          FogStart := FogStart / 1.2;
        end;
      end;
    '*':
      with TerrainRenderer1 do
        if CLODPrecision > 20 then
          CLODPrecision := Round(CLODPrecision * 0.8);
    '/':
      with TerrainRenderer1 do
        if CLODPrecision < 1000 then
          CLODPrecision := Round(CLODPrecision * 1.2);
    '8':
      with TerrainRenderer1 do
        if QualityDistance > 40 then
          QualityDistance := Round(QualityDistance * 0.8);
    '9':
      with TerrainRenderer1 do
        if QualityDistance < 1000 then
          QualityDistance := Round(QualityDistance * 1.2);
  end;

  Key := #0;
end;

procedure TMainForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  GLCadencer1.Enabled := false;
  Timer1.Enabled := false;
end;

procedure TMainForm.checkai;
begin
  sub.x := FreeForm1.Position.x;
  sub.y := FreeForm1.Position.y;
  sub.z := FreeForm1.Position.z;
  sub.dx := FreeForm1.direction.x;
  sub.dy := FreeForm1.direction.y;
  sub.dz := FreeForm1.direction.z;

  fatsub.x := FreeForm8.Position.x;
  fatsub.y := FreeForm8.Position.y;
  fatsub.z := FreeForm8.Position.z;
  fatsub.dx := FreeForm8.direction.x;
  fatsub.dy := FreeForm8.direction.y;
  fatsub.dz := FreeForm8.direction.z;
end;

end.
