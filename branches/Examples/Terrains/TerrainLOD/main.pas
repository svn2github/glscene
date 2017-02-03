unit main;
{ =============================================================================
  Terrain Testing Project
  ============================================================================== }

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Variants,
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.ExtCtrls,
  Vcl.StdCtrls,
  Vcl.Imaging.JPeg,

  // GLS
  GLWin32Viewer,
  GLTexture,
  GLScene,
  GLTerrainRenderer,
  GLObjects,
  GLCadencer,
  GLHeightData,
  GLNavigator,
  GLBitmapFont,
  GLWindowsFont,
  GLHUDObjects,
  GLMaterial,
  GLCoordinates,
  GLCrossPlatform,
  GLBaseClasses,
  GLKeyBoard,
  GLState;

const
  cMapSize = 256; // size of terrain map data

type
  TMap = array [0 .. cMapSize, 0 .. cMapSize] of Byte;

  TfrmMain = class(TForm)
    GLScene1: TGLScene;
    GLMaterialLibrary1: TGLMaterialLibrary;
    GLSceneViewer1: TGLSceneViewer;
    Panel1: TPanel;
    GLCamera1: TGLCamera;
    cam: TGLDummyCube;
    dcCentre: TGLDummyCube;
    GLLightSource1: TGLLightSource;
    GLTerrainRenderer1: TGLTerrainRenderer;
    GLCadencer1: TGLCadencer;
    GLCustomHDS1: TGLCustomHDS;
    Timer1: TTimer;
    GLHUDText1: TGLHUDText;
    GLWindowsBitmapFont1: TGLWindowsBitmapFont;
    chkWireFrame: TCheckBox;
    Label1: TLabel;
    Button1: TButton;
    GLCube1: TGLCube;
    Button2: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure GLTerrainRenderer1GetTerrainBounds(var l, t, r, b: Single);
    procedure GLCadencer1Progress(Sender: TObject;
      const deltaTime, newTime: Double);
    procedure GLSceneViewer1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure GLSceneViewer1MouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure GLCustomHDS1StartPreparingData(heightData: TGLHeightData);
    procedure Timer1Timer(Sender: TObject);
    procedure chkWireFrameClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    { Private declarations }
    mx: Integer;
    my: Integer;
    FCamHeight: Single;
    map: TMap;
    procedure LoadMap;
  public
    { Public declarations }
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.dfm}

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  // terrainrenderer up and direction vector set at design time so working in positive TR x y coords
  // that is y heads off in world -z direction and x heads off in positive x direction. z is up of course with TR
  // cube placed at centre of terrain z = -128, x = 128

  GLCustomHDS1.MaxPoolSize := 8 * 1024 * 1024;
  GLMaterialLibrary1.Materials[1].Material.Texture.Image.LoadFromFile
    ('pattern.jpg');
  GLMaterialLibrary1.Materials[1].TextureScale.Y := -1; // flip bitmap image
  // GLMaterialLibrary1.Materials[1].Material.Texture.Image.LoadFromFile('detailmap.jpg');

  // one texture to cover whole terrain
  GLTerrainRenderer1.TilesPerTexture := cMapSize / GLTerrainRenderer1.TileSize;
  FCamHeight := 50;
  cam.Position.X := cMapSize / 2; // half way across
  cam.Position.Y := FCamHeight;
  LoadMap;
  chkWireFrameClick(Sender);
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  //
end;

procedure TfrmMain.LoadMap;
var
  bmp: TBitmap;
  X, Y: Integer;
begin

  // Load the image. It should be cMapsize and preferably grayscale.
  bmp := TBitmap.Create;
  bmp.LoadFromFile('terrain.bmp');
  for X := 0 to cMapSize - 1 do
    for Y := 0 to cMapSize - 1 do
      map[X, Y] := bmp.Canvas.Pixels[X, Y] mod $100;

  bmp.Free;
end;

procedure TfrmMain.GLTerrainRenderer1GetTerrainBounds(var l, t, r, b: Single);
begin
  // reduce scope of terrain
  l := -1;
  t := cMapSize + 1;
  r := cMapSize + 1;
  b := -1;

end;

procedure TfrmMain.GLCadencer1Progress(Sender: TObject;
  const deltaTime, newTime: Double);
var
  speed: Single;
begin
  // handle keypresses
  if IsKeyDown(VK_SHIFT) then
    speed := 5 * deltaTime
  else
    speed := deltaTime;

  with GLCamera1.Position do
  begin
    if (IsKeyDown(VK_RIGHT) or IsKeyDown('D')) then
      cam.Translate(Z * speed, 0, -X * speed);
    if (IsKeyDown(VK_LEFT) or IsKeyDown('A')) then
      cam.Translate(-Z * speed, 0, X * speed);
    if IsKeyDown(VK_UP) or IsKeyDown('W') then
      cam.Translate(-X * speed, 0, -Z * speed);
    if IsKeyDown(VK_DOWN) or IsKeyDown('S') then
      cam.Translate(X * speed, 0, Z * speed);

    if IsKeyDown(VK_PRIOR) or IsKeyDown('Q') then
    begin
      FCamHeight := FCamHeight + 10 * speed;
      cam.Position.Y := FCamHeight;
    end;

    if IsKeyDown(VK_NEXT) or IsKeyDown('E') then
    begin
      FCamHeight := FCamHeight - 10 * speed;
      cam.Position.Y := FCamHeight;
    end;

    if IsKeyDown(VK_ESCAPE) then
      Close;
  end;

  // don't drop through terrain!
  // with cam.Position do
  // Y:=GLTerrainRenderer1.InterpolatedHeight(AsVector)+FCamHeight;

end;

procedure TfrmMain.GLSceneViewer1MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  mx := X;
  my := Y;
end;

procedure TfrmMain.GLSceneViewer1MouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
begin
  if ssLeft in Shift then
  begin
    GLCamera1.MoveAroundTarget((my - Y) * 0.5, (mx - X) * 0.5);
    mx := X;
    my := Y;
  end;
end;

procedure TfrmMain.GLCustomHDS1StartPreparingData(heightData: TGLHeightData);
var
  rasterLine: GLHeightData.PSmallIntArray;
  b: SmallInt;
  X, Y: Integer;
begin

  b := 128; // 128 = one unit

  heightData.DataState := hdsPreparing;
  with heightData do
  begin
    if (XLeft < 0) or (XLeft > cMapSize - 1) or (YTop < 0) or
      (YTop > cMapSize - 1) then
      heightData.DataState := hdsNone
    else
    begin
      Allocate(hdtSmallInt);
      for Y := YTop to YTop + Size - 1 do
      begin
        rasterLine := SmallIntRaster[Y - YTop];
        for X := XLeft to XLeft + Size - 1 do
          rasterLine[X - XLeft] := map[X, Y] * 20;
      end;
      DataState := hdsReady;
    end;
  end;
end;

procedure TfrmMain.Timer1Timer(Sender: TObject);
begin
  GLHUDText1.Text := Format('%.1f FPS - %d', [GLSceneViewer1.FramesPerSecond,
    GLTerrainRenderer1.LastTriangleCount]);
  GLSceneViewer1.ResetPerformanceMonitor;
end;

procedure TfrmMain.chkWireFrameClick(Sender: TObject);
begin
  if chkWireFrame.Checked then
  begin
    GLTerrainRenderer1.Material.LibMaterialName := '';
    GLTerrainRenderer1.Material.PolygonMode := pmLines;
  end
  else
  begin
    GLTerrainRenderer1.Material.LibMaterialName := 'tex1';
    GLTerrainRenderer1.Material.PolygonMode := pmFill;
  end;
end;

procedure TfrmMain.Button1Click(Sender: TObject);
begin
  // you can also change size of roam trianle by setting CLODPrecision
  // smaller value to reduce popping effect
  GLTerrainRenderer1.QualityDistance := GLTerrainRenderer1.TileSize;
  GLCustomHDS1.MarkDirty;
  GLSceneViewer1.SetFocus;
end;

procedure TfrmMain.Button2Click(Sender: TObject);
begin
  // enlarge terrain via scaling
  GLTerrainRenderer1.Scale.X := 4;
  GLTerrainRenderer1.Scale.Y := 4;
  GLCustomHDS1.MarkDirty;
  GLSceneViewer1.SetFocus;
end;

end.
