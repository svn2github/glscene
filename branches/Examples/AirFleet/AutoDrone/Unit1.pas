unit Unit1;

interface

uses
  Winapi.OpenGL,
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
  Vcl.StdCtrls,
  Vcl.Menus,

  GLAsyncTimer,
  GLDCE,
  GLScene,
  GLObjects,
  GLCadencer,
  GLWin32Viewer,
  GLTexture,
  GLHeightData,
  GLTerrainRenderer,
  GLVectorFileObjects,
  GLBitmapFont,
  GLWindowsFont,
  GLHUDObjects,
  GLCrossPlatform,
  GLMaterial,
  GLCoordinates,
  GLBaseClasses,
  GLRenderContextInfo,
  GLState,
  GLSkydome,
  GLFullScreenViewer,
  GLFileMD2,
  GLFile3DS,
  GLVectorGeometry,
  GLKeyboard,
  GLProxyObjects,
  GLEllipseCollision,
  GLSkyBox,
  GLTexCombineShader,
  GLHeightTileFileHDS,
  GLLensFlare,
  GLCollision,
  GLGeomObjects,
  GLFireFX,
  GLParticleFX,
  GLPerlinPFX,
  GLNavigator,
  GLVectorTypes,
  GLFileTGA,
  GLFileJpeg,
  IdBaseComponent,
  IdCoder,
  IdCoder3to4,
  IdCoderMIME;

type
  TForm1 = class(TForm)
    Scene: TGLScene;
    Cad1: TGLCadencer;
    Light01: TGLLightSource;
    Panel1: TPanel;
    MLSkyBox: TGLMaterialLibrary;
    Viewer1: TGLSceneViewer;
    Terrain1: TGLTerrainRenderer;
    HTF: TGLHeightTileFileHDS;
    DroneMaterial: TGLMaterialLibrary;
    GLMaterialLibrary1: TGLMaterialLibrary;
    Button1: TButton;
    FPSl: TLabel;
    IDDecoder: TIdDecoderMIME;
    OD: TOpenDialog;
    FPSTimer: TGLAsyncTimer;
    CamType: TRadioGroup;
    Cam1: TGLCamera;
    Cam2: TGLCamera;
    Cam3: TGLCamera;
    cam_cube: TGLDummyCube;
    PopupMenu1: TPopupMenu;
    procedure FormCreate(Sender: TObject);
    procedure l(Sender: TObject; const deltaTime, newTime: Double);
    procedure FillFreeFormArray(filename: string; Decoder: TIdDecoderMIME);
    procedure LoadTerain;
    procedure Button1Click(Sender: TObject);
    procedure FPSTimerTimer(Sender: TObject);
    procedure CamTypeClick(Sender: TObject);
  public
    procedure HandleKeys(deltaTime: single);
    function GetDirVec: TVector3f;
  end;

type
  TFFormArray = array of TGlFreeForm;

  TFFrm = record
    X: integer;
    Y: integer;
    Z: integer;
    sX: integer;
    sY: integer;
    sZ: integer;
    ModelName: string;
  end;

var
  Form1: TForm1;
  Creature: TGLFreeForm;
  Objects: array of TFFrm;
  Obstacles: TFFormArray;
  Goal: TGLSphere;
  CameraType: Byte;
  lastx, lasty: integer;

implementation

const
  speed = 3;
  k = 0.1;
  FreeCamera = 0;
  DroneCamera = 1;
  ThirdPersonCamera = 2;

{$R *.dfm}

procedure SetTexImageName(ml: TGLMaterialLibrary;
  const matName, filename: string);
var
  libMat: TGLLibMaterial;
  img: TGLPicFileImage;
begin
  libMat := ml.LibMaterialByName(matName);
  libMat.Material.Texture.ImageClassName := TGLPicFileImage.ClassName;
  img := TGLPicFileImage(libMat.Material.Texture.Image);
  img.PictureFileName := filename;
end;

procedure TForm1.FormCreate(Sender: TObject);
var
  i: integer;
  // Obst: TGlFreeForm;
begin
  SetCurrentDir(ExtractFilePath(ParamStr(0)));
  //////////////////// Terrains
  HTF.HTFFileName := 'Resourses\Textures\Terrains\desert.htf';
  HTF.MaxPoolSize := 16 * 1024 * 1024;
  Terrain1.Material.Texture.Disabled := False;
  Terrain1.Material.LibMaterialName := 'ground';
  Terrain1.Direction.SetVector(0, 1, 0);
  GLMaterialLibrary1.Materials[0].Material.Texture.Image.LoadFromFile('Resourses\Textures\Terrains\texture.jpg');
  GLMaterialLibrary1.Materials[1].Material.Texture.Image.LoadFromFile('Resourses\Textures\Terrains\detail_map.jpg');
  Terrain1.TilesPerTexture := 256 / Terrain1.TileSize;
  Terrain1.Scale.SetVector(1, 1, 0.25);
  /////////////////////// SkyBox
  SetTexImageName(MLSkyBox, 'north', 'Resourses\Textures\UnderWaterMap\North.jpg');
  SetTexImageName(MLSkyBox, 'east', 'Resourses\Textures\UnderWaterMap\East.jpg');
  SetTexImageName(MLSkyBox, 'south', 'Resourses\Textures\UnderWaterMap\South.jpg');
  SetTexImageName(MLSkyBox, 'west', 'Resourses\Textures\UnderWaterMap\West.jpg');
  SetTexImageName(MLSkyBox, 'top', 'Resourses\Textures\UnderWaterMap\Top.jpg');
  ////////////////////// Creature
  Creature := TGlFreeForm(Scene.Objects.AddNewChild(TGLFreeForm));
  Light01.MoveTo(Creature);
  Creature.Position.SetPoint(0, 0, 0);
  Creature.Direction.SetVector(0, 0, 1);
  Creature.AutoScaling.SetPoint(1, 1, 1);
  Creature.LoadFromFile('Resourses\Models\Drone.3ds');
  Creature.Material.Texture.Disabled := False;
  Creature.MaterialLibrary := DroneMaterial;
  Creature.Material.Texture.Image.LoadFromFile('Resourses\Textures\Drone_textures.tga');
  ////////////////////// Goal
  Goal := TGLSphere(Scene.Objects.AddNewChild(TGLSphere));
  Goal.Radius := 0.25;
  Goal.Position.SetPoint(Random(30), Random(30), Random(30));
  Goal.Material.FrontProperties.Emission.Green := 1;
  ///////////////////// Navigator
  CameraType := 0;
  Cam2.TargetObject := Creature;
  Cam2.MoveTo(Creature);
end;

procedure TForm1.l(Sender: TObject; const deltaTime, newTime: Double);
begin
  ///////////////////////////////////////////// Terrain1
  if Terrain1.CLODPrecision > 20 then
    Terrain1.CLODPrecision := Round(Terrain1.CLODPrecision * 0.8);
  ///////////////////////////////////////////// GetDirVec
  if VectorLength(VectorCombine(Creature.Position.AsVector,
    VectorNegate(Goal.Position.AsVector), 1, 1)) > 1 then
  begin
    Creature.Direction.SetVector(GetDirVec);
    Creature.Move(2 * deltaTime);
  end; // if
  /////////////////////////////////////////////// HandleKeys
  HandleKeys(deltaTime);
  Viewer1.Invalidate;

  Cam2.MoveTo(Creature);
  Cam2.Direction.SetVector(-Creature.Direction.X, -Creature.Direction.Y,
    -Creature.Direction.Z);
end;

procedure TForm1.HandleKeys(deltaTime: single);
var
  MoveCount: single;
  v: TVector;
begin
  with Mouse.CursorPos do
    if (X <> lastx) or (Y <> lasty) then
    begin
      if IsKeyDown(VK_LBUTTON) then
      begin
        cam_cube.Turn((X - lastx) / 2);
        Cam1.Pitch((lasty - Y) / 2);
      end;
      lastx := X;
      lasty := Y;
    end;

  if IsKeyDown(VK_SHIFT) then
    MoveCount := 3 * speed * 4 * deltaTime
  else
    MoveCount := 3 * speed * deltaTime;

  if IsKeyDown(VK_ESCAPE) then
    Close;
  if CameraType = 0 then
  begin
    if IsKeyDown(ord('W')) then
      cam_cube.Position.AddScaledVector(MoveCount, Cam1.AbsoluteDirection);
    if IsKeyDown(ord('S')) then
      cam_cube.Position.AddScaledVector(-MoveCount, Cam1.AbsoluteDirection);
    if IsKeyDown(ord('D')) then
      cam_cube.Slide(MoveCount);
    if IsKeyDown(ord('A')) then
      cam_cube.Slide(-MoveCount);
    if IsKeyDown('+') then
      cam_cube.Lift(MoveCount / 2);
    if IsKeyDown('-') then
      cam_cube.Lift(-MoveCount / 2);
  end;
  if IsKeyDown(ord('T')) then
  begin
    Creature.Position.SetPoint(0, 0, 0);
    Randomize;
    Goal.Position.SetPoint(Random(30), Random(30), Random(30));
  end;

  if IsKeyDown(ord('C')) then
  begin
    if CameraType = 2 then
      CameraType := 0
    else
      inc(CameraType);
    CamType.ItemIndex := CameraType;
    case CameraType of
      0:
        begin
          Viewer1.Camera := Cam1;
        end;
      1:
        begin
          Viewer1.Camera := Cam2;
        end;
       2:
        begin
          Viewer1.Camera := Cam3;
        end;
    end;
  end;
end;

function TForm1.GetDirVec: TVector3f;
var
  GoalVec, DirVec, ObstacleVec: TVector3f;
  dist: single;
  i: integer;
  X, Y, Z: single;
  Obst: TGlFreeForm;
begin
  SetVector(GoalVec, VectorCombine(Goal.Position.AsVector,
    VectorNegate(Creature.Position.AsVector), 1, 1));
  DirVec := VectorNormalize(GoalVec);

  for i := 0 to Length(Obstacles) - 1 do
  begin
    Obst := Obstacles[i];
    SetVector(ObstacleVec, VectorCombine(Creature.Position.AsVector,
      VectorNegate(Obst.Position.AsVector), 1, 1));
    /// /
    X := Creature.Scale.X;
    Y := Creature.Scale.Y;
    Z := Creature.Scale.Z;
    dist := VectorLength(ObstacleVec) - max(max(X, Y), Z);
    X := Obst.Scale.X;
    Y := Obst.Scale.Y;
    Z := Obst.Scale.Z;
    dist := dist - max(max(X, Y), Z);
    /// /
    NormalizeVector(ObstacleVec);
    ScaleVector(ObstacleVec, k / (dist * dist));
    DirVec := VectorCombine(DirVec, ObstacleVec, 1, 1);
  end;

  NormalizeVector(DirVec);
  Result := DirVec;
end;

procedure TForm1.FillFreeFormArray(filename: string; Decoder: TIdDecoderMIME);

var
  Cord: array [1 .. 6] of integer;

  procedure DivString(s: string);
  var
    cnt: Byte;
  begin
    cnt := 0;
    while (s <> '') and (pos(' ', s) <> 0) do
    begin
      inc(cnt);
      Cord[cnt] := strtoint(copy(s, 1, pos(' ', s) - 1));
      delete(s, 1, pos(' ', s));
    end;
    if s <> '' then
    begin
      inc(cnt);
      Cord[cnt] := strtoint(s);
    end;
  end;

  procedure ClearList;
  var
    i: integer;
  begin
    for i := 0 to Length(Obstacles) - 1 do
      Obstacles[i].Free;
  end;

var
  f: textfile;
  tmp: string;
  n, i: integer;

begin
  ClearList;
  assignfile(f, filename);
  reset(f);
  Readln(f, tmp);
  n := strtoint(Decoder.DecodeString(tmp));
  SetLength(Objects, n);
  for i := 0 to n - 1 do
  begin
    Readln(f, tmp);
    tmp := Decoder.DecodeString(tmp);
    DivString(tmp);
    Objects[i].X := Cord[1];
    Objects[i].Y := Cord[2];
    Objects[i].Z := Cord[3];
    Objects[i].sX := Cord[4];
    Objects[i].sY := Cord[5];
    Objects[i].sZ := Cord[6];
    Readln(f, tmp);
    tmp := Decoder.DecodeString(tmp);
    Objects[i].ModelName := tmp;
  end;
  CloseFile(f);
  LoadTerain;
end;

procedure TForm1.LoadTerain;
var
  i: integer;
  Path: string;
begin
  SetLength(Obstacles, Length(Objects));
  for i := 0 to Length(Objects) - 1 do // Создание объектов
  begin
    Obstacles[i] := TGlFreeForm(Scene.Objects.AddNewChild(TGlFreeForm));
    // тут сам.
    Path := ExtractFilePath(OD.filename) + Objects[i].ModelName;
    if not FileExists(Path) then
    begin
      Path := ExtractFilePath(Application.ExeName) + 'Resourses\Models\' +
        Objects[i].ModelName;
      if not FileExists(Path) then
      begin
        showmessage('No Models');
        exit;
      end;
    end;
    Obstacles[i].LoadFromFile(Path);
    // Obstacles[i].Position.SetPoint(Random(30), Random(30), Random(30));
    Obstacles[i].Position.SetPoint(Objects[i].X, Objects[i].Y, Objects[i].Z);
    Obstacles[i].AutoScaling.SetPoint(Objects[i].sX, Objects[i].sY,
      Objects[i].sZ);

  end;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  if OD.Execute then
    FillFreeFormArray(OD.filename, IDDecoder);
end;

procedure TForm1.FPSTimerTimer(Sender: TObject);
begin
  /// ////////////////////////////////////////////////////////////////////////////// FPS
  FPSl.caption := 'FPS: ' + IntToStr(Round(Viewer1.FramesPerSecond));
  Viewer1.ResetPerformanceMonitor;
end;

procedure TForm1.CamTypeClick(Sender: TObject);
begin
  CameraType := CamType.ItemIndex;
  case CameraType of
    0:
      Cam1.TargetObject := nil;
    1:
      begin
        Cam1.TargetObject := Creature; //Cam2 ???
        Cam1.MoveTo(Creature);         //Cam2 ???
      end;
    2:
      begin
      (*
        Cam3.TargetObject := Creature;
        Cam3.MoveTo(Creature);
       *)
      end;

  end;
end;

end.
