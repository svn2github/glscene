unit UnitMain888;

interface

uses
  Winapi.Windows,
  System.SysUtils,
  System.Classes,
  System.Math,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.StdCtrls,
  Vcl.ComCtrls,
  Vcl.Grids,
  // GLS
  GLScene, GLObjects,
  GLCadencer,
  GLWin32Viewer,
  GLVectorFileObjects,
  GLTexture,
  GLNavigator,
  GLGeomObjects,
  GLDCE, GLSkydome,
  GLGraph,
  GLCoordinates,
  GLCrossPlatform,
  GLBaseClasses;

type
  TFormShoot = class(TForm)
    GLScene: TGLScene;
    SceneViewer: TGLSceneViewer;
    Cadencer: TGLCadencer;
    Camera: TGLCamera;
    World: TGLDummyCube;
    Map: TGLFreeForm;
    Actor: TGLActor;
    Sphere: TGLSphere;
    CameraCube: TGLDummyCube;
    GLDCEManager1: TGLDCEManager;
    Sky: TGLEarthSkyDome;
    TargetPoint: TGLSphere;
    Light: TGLLightSource;
    Button2: TButton;
    S1: TGLSphere;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    Label2: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    TabSheet3: TTabSheet;
    Label1: TLabel;
    Wall: TGLFreeForm;
    GLDummyCube2: TGLDummyCube;
    VP1: TGLSphere;
    VP2: TGLSphere;
    VP3: TGLSphere;
    StringGrid1: TStringGrid;
    VP4: TGLSphere;
    VP5: TGLSphere;
    Label6: TLabel;
    Label7: TLabel;
    VP6: TGLSphere;
    VP7: TGLSphere;
    VP8: TGLSphere;
    Label8: TLabel;
    Label9: TLabel;
    VPstart: TGLSphere;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    VV: TGLSphere;
    TabSheet4: TTabSheet;
    Label15: TLabel;
    Label16: TLabel;
    Label17: TLabel;
    Label18: TLabel;
    Label19: TLabel;
    Label20: TLabel;
    Label21: TLabel;
    Label22: TLabel;
    Label23: TLabel;
    Label24: TLabel;
    VP9: TGLSphere;
    Label3: TLabel;
    GLLines1: TGLLines;
    VP10: TGLSphere;
    VP11: TGLSphere;
    VP12: TGLSphere;
    VP13: TGLSphere;
    VP14: TGLSphere;
    GLC: TGLCube;
    procedure FormCreate(Sender: TObject);
    procedure FormMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure SceneViewerMouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure CadencerProgress(Sender: TObject;
      const deltaTime, newTime: Double);
    procedure SceneViewerMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);

    procedure PlayerBehaviours0Collision(Sender: TObject;
      ObjectCollided: TGLBaseSceneObject; CollisionInfo: TDCECollision);
    procedure S1Behaviours10Collision(Sender: TObject;
      ObjectCollided: TGLBaseSceneObject; CollisionInfo: TDCECollision);
    procedure Button2Click(Sender: TObject);
    procedure FormActivate(Sender: TObject);

  private
    { Private declarations }
    oldPick: TGLCustomSceneObject;
  public
    mpX, mpY: Integer;
    LastX, LastY: Integer;
    mX, mY: Integer;

    tX, tY, tZ: Single;
    // rX,rY,rZ   : Single;
    t1X, t1Y, t1Z: Single;
    t: Integer;
    ass: Single;
    colll: Boolean;
    fs: TextFile;
    t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14: Integer;
    start, finish: Integer;
    c, v: Integer;
    vp1vd, vp2vd, vp3vd, vp4vd, vp5vd, vp6vd, vp7vd, vp8vd, vp9vd, vp10vd,
      vp11vd, vp12vd, vp13vd, vp14vd, vp999vd: Single;
    vp1vf, vp2vf, vp3vf, vp4vf, vp5vf, vp6vf, vp7vf, vp8vf, vp9vf, vp10vf,
      vp11vf, vp12vf, vp13vf, vp14vf: Single;
    t1t, t2t, t3t, t4t, t5t, t6t, t7t, t8t, t9t, t10t, t11t, t12t, t13t,
      t14t: Boolean;
    procedure Lo;
    procedure La;
  end;

var
  FormShoot: TFormShoot;

const
  MIN_DISTANCE = 1.25;

implementation

{$R *.dfm}

uses
  Vcl.Imaging.Jpeg,
  // GLS
  GLFile3DS,
  GLFileMD2,
  GLFileTGA,
  GLVectorGeometry,
  GLVectorTypes,
  GLKeyboard;

procedure TFormShoot.FormCreate(Sender: TObject);
begin
  colll := false;
  t1t := true;
  t2t := false;
  t3t := false;
  t4t := false;
  t5t := false;
  t6t := false;
  t7t := false;
  t8t := false;
  t9t := false;
  t10t := false;
  t11t := false;
  t12t := false;
  t13t := false;
  t14t := false;

  Label2.Caption := '               ';

  tX := Sphere.Position.X;
  tY := Sphere.Position.Y;
  tZ := Sphere.Position.z;

  // Грузим карту
  Map.LoadFromFile('MapsObjects\mapmain.3ds');
  Map.PitchAngle := 90;
  // Грузим текстуру карты
  Map.Material.Texture.Image.LoadFromFile('MapsObjects\sand_cav.tga');
  // Готовим карту к работе с DCE
  Map.BuildOctree;

  Wall.LoadFromFile('MapsObjects\Wall.3ds');
  Wall.PitchAngle := 90;
  Wall.BuildOctree;

  // Грузим модель персонажа
  Actor.LoadFromFile('Players\waste.md2');
  // Грузим текстуру персонажа
  Actor.Material.Texture.Image.LoadFromFile('Players\waste.jpg');
  // Грузим установки анимации персонажа
  Actor.Animations.LoadFromFile('Players\Quake2Animations.aaf');
  // Устанавливаем масштаб персонажа
  Actor.Scale.SetVector(0.05, 0.05, 0.05, 0);
  // Устанавливаем настройки анимации персонажа
  Actor.AnimationMode := aamLoop;
  Actor.SwitchToAnimation('stand');
  Actor.FrameInterpolation := afpLinear;

  Sphere.Scale.Assign(GetOrCreateDCEDynamic(Sphere).Size);
  GetOrCreateDCEDynamic(Sphere).OnCollision := PlayerBehaviours0Collision;
  GetOrCreateDCEDynamic(S1).OnCollision := S1Behaviours10Collision;
end;

procedure TFormShoot.La;
begin

  if t1t = true then
  begin
    if t1 = 0 then
    begin

      VV.Position.X := TargetPoint.Position.X;
      VV.Position.Y := TargetPoint.Position.Y;
      VV.Position.z := TargetPoint.Position.z;
      // Actor.Tag:=0;
    end;
    if t1 = 1 then
    begin
      VV.Position.X := VP1.Position.X;
      VV.Position.Y := VP1.Position.Y;
      VV.Position.z := VP1.Position.z;
    end;
    if t1 = 2 then
    begin
      VV.Position.X := VP2.Position.X;
      VV.Position.Y := VP2.Position.Y;
      VV.Position.z := VP2.Position.z;
    end;
    if t1 = 3 then
    begin
      VV.Position.X := VP3.Position.X;
      VV.Position.Y := VP3.Position.Y;
      VV.Position.z := VP3.Position.z;
    end;
    if t1 = 4 then
    begin
      VV.Position.X := VP4.Position.X;
      VV.Position.Y := VP4.Position.Y;
      VV.Position.z := VP4.Position.z;
    end;
    if t1 = 5 then
    begin
      VV.Position.X := VP5.Position.X;
      VV.Position.Y := VP5.Position.Y;
      VV.Position.z := VP5.Position.z;
    end;
    if t1 = 6 then
    begin
      VV.Position.X := VP6.Position.X;
      VV.Position.Y := VP6.Position.Y;
      VV.Position.z := VP6.Position.z;
    end;
    if t1 = 7 then
    begin
      VV.Position.X := VP7.Position.X;
      VV.Position.Y := VP7.Position.Y;
      VV.Position.z := VP7.Position.z;
    end;
    if t1 = 8 then
    begin
      VV.Position.X := VP8.Position.X;
      VV.Position.Y := VP8.Position.Y;
      VV.Position.z := VP8.Position.z;
    end;
    if t1 = 9 then
    begin
      VV.Position.X := VP9.Position.X;
      VV.Position.Y := VP9.Position.Y;
      VV.Position.z := VP9.Position.z;
    end;
    if t1 = 10 then
    begin
      VV.Position.X := VP10.Position.X;
      VV.Position.Y := VP10.Position.Y;
      VV.Position.z := VP10.Position.z;
    end;
    if t1 = 11 then
    begin
      VV.Position.X := VP11.Position.X;
      VV.Position.Y := VP11.Position.Y;
      VV.Position.z := VP11.Position.z;
    end;
    if t1 = 12 then
    begin
      VV.Position.X := VP12.Position.X;
      VV.Position.Y := VP12.Position.Y;
      VV.Position.z := VP12.Position.z;
    end;
    if t1 = 13 then
    begin
      VV.Position.X := VP13.Position.X;
      VV.Position.Y := VP13.Position.Y;
      VV.Position.z := VP13.Position.z;
    end;
    if t1 = 14 then
    begin
      VV.Position.X := VP14.Position.X;
      VV.Position.Y := VP14.Position.Y;
      VV.Position.z := VP14.Position.z;
    end;
  end;
  if VectorDistance(Sphere.Position.AsVector, VV.Position.AsVector) <= MIN_DISTANCE
  then
  begin
    t1t := false;

    if t2 = 0 then
    begin
      VV.Position.X := TargetPoint.Position.X;
      VV.Position.Y := TargetPoint.Position.Y;
      VV.Position.z := TargetPoint.Position.z;

    end;
    if t2 > 0 then
    begin
      t2t := true;

    end;
  end;

  if t2t = true then
  begin
    if t2 = 0 then
    begin
      VV.Position.X := TargetPoint.Position.X;
      VV.Position.Y := TargetPoint.Position.Y;
      VV.Position.z := TargetPoint.Position.z;
    end;
    if t2 = 1 then
    begin
      VV.Position.X := VP1.Position.X;
      VV.Position.Y := VP1.Position.Y;
      VV.Position.z := VP1.Position.z;
    end;
    if t2 = 2 then
    begin
      VV.Position.X := VP2.Position.X;
      VV.Position.Y := VP2.Position.Y;
      VV.Position.z := VP2.Position.z;
    end;
    if t2 = 3 then
    begin
      VV.Position.X := VP3.Position.X;
      VV.Position.Y := VP3.Position.Y;
      VV.Position.z := VP3.Position.z;
    end;
    if t2 = 4 then
    begin
      VV.Position.X := VP4.Position.X;
      VV.Position.Y := VP4.Position.Y;
      VV.Position.z := VP4.Position.z;
    end;
    if t2 = 5 then
    begin
      VV.Position.X := VP5.Position.X;
      VV.Position.Y := VP5.Position.Y;
      VV.Position.z := VP5.Position.z;
    end;
    if t2 = 6 then
    begin
      VV.Position.X := VP6.Position.X;
      VV.Position.Y := VP6.Position.Y;
      VV.Position.z := VP6.Position.z;
    end;
    if t2 = 7 then
    begin
      VV.Position.X := VP7.Position.X;
      VV.Position.Y := VP7.Position.Y;
      VV.Position.z := VP7.Position.z;
    end;
    if t2 = 8 then
    begin
      VV.Position.X := VP8.Position.X;
      VV.Position.Y := VP8.Position.Y;
      VV.Position.z := VP8.Position.z;
    end;
    if t2 = 9 then
    begin
      VV.Position.X := VP9.Position.X;
      VV.Position.Y := VP9.Position.Y;
      VV.Position.z := VP9.Position.z;
    end;
    if t2 = 10 then
    begin
      VV.Position.X := VP10.Position.X;
      VV.Position.Y := VP10.Position.Y;
      VV.Position.z := VP10.Position.z;
    end;
    if t2 = 11 then
    begin
      VV.Position.X := VP11.Position.X;
      VV.Position.Y := VP11.Position.Y;
      VV.Position.z := VP11.Position.z;
    end;
    if t2 = 12 then
    begin
      VV.Position.X := VP12.Position.X;
      VV.Position.Y := VP12.Position.Y;
      VV.Position.z := VP12.Position.z;
    end;
    if t2 = 13 then
    begin
      VV.Position.X := VP13.Position.X;
      VV.Position.Y := VP13.Position.Y;
      VV.Position.z := VP13.Position.z;
    end;
    if t2 = 14 then
    begin
      VV.Position.X := VP14.Position.X;
      VV.Position.Y := VP14.Position.Y;
      VV.Position.z := VP14.Position.z;
    end;
  end;
  if VectorDistance(Sphere.Position.AsVector, VV.Position.AsVector) <= MIN_DISTANCE
  then
  begin
    t1t := false;
    t2t := false;
    if t3 = 0 then
    begin
      VV.Position.X := TargetPoint.Position.X;
      VV.Position.Y := TargetPoint.Position.Y;
      VV.Position.z := TargetPoint.Position.z;
      t3t := false;
    end;
    if t3 > 0 then
    begin
      t3t := true;
    end;
  end;

  if t3t = true then
  begin
    if t3 = 0 then
    begin
      VV.Position.X := TargetPoint.Position.X;
      VV.Position.Y := TargetPoint.Position.Y;
      VV.Position.z := TargetPoint.Position.z;
      t3t := false;
    end;
    if t3 = 1 then
    begin
      VV.Position.X := VP1.Position.X;
      VV.Position.Y := VP1.Position.Y;
      VV.Position.z := VP1.Position.z;
    end;
    if t3 = 2 then
    begin
      VV.Position.X := VP2.Position.X;
      VV.Position.Y := VP2.Position.Y;
      VV.Position.z := VP2.Position.z;
    end;
    if t3 = 3 then
    begin
      VV.Position.X := VP3.Position.X;
      VV.Position.Y := VP3.Position.Y;
      VV.Position.z := VP3.Position.z;
    end;
    if t3 = 4 then
    begin
      VV.Position.X := VP4.Position.X;
      VV.Position.Y := VP4.Position.Y;
      VV.Position.z := VP4.Position.z;
    end;
    if t3 = 5 then
    begin
      VV.Position.X := VP5.Position.X;
      VV.Position.Y := VP5.Position.Y;
      VV.Position.z := VP5.Position.z;
    end;
    if t3 = 6 then
    begin
      VV.Position.X := VP6.Position.X;
      VV.Position.Y := VP6.Position.Y;
      VV.Position.z := VP6.Position.z;
    end;
    if t3 = 7 then
    begin
      VV.Position.X := VP7.Position.X;
      VV.Position.Y := VP7.Position.Y;
      VV.Position.z := VP7.Position.z;
    end;
    if t3 = 8 then
    begin
      VV.Position.X := VP8.Position.X;
      VV.Position.Y := VP8.Position.Y;
      VV.Position.z := VP8.Position.z;
    end;
    if t3 = 9 then
    begin
      VV.Position.X := VP9.Position.X;
      VV.Position.Y := VP9.Position.Y;
      VV.Position.z := VP9.Position.z;
    end;
    if t3 = 10 then
    begin
      VV.Position.X := VP10.Position.X;
      VV.Position.Y := VP10.Position.Y;
      VV.Position.z := VP10.Position.z;
    end;
    if t3 = 11 then
    begin
      VV.Position.X := VP11.Position.X;
      VV.Position.Y := VP11.Position.Y;
      VV.Position.z := VP11.Position.z;
    end;
    if t3 = 12 then
    begin
      VV.Position.X := VP12.Position.X;
      VV.Position.Y := VP12.Position.Y;
      VV.Position.z := VP12.Position.z;
    end;
    if t3 = 13 then
    begin
      VV.Position.X := VP13.Position.X;
      VV.Position.Y := VP13.Position.Y;
      VV.Position.z := VP13.Position.z;
    end;
    if t3 = 14 then
    begin
      VV.Position.X := VP14.Position.X;
      VV.Position.Y := VP14.Position.Y;
      VV.Position.z := VP14.Position.z;
    end;
  end;
  if VectorDistance(Sphere.Position.AsVector, VV.Position.AsVector) <= MIN_DISTANCE
  then
  begin
    t1t := false;
    t2t := false;
    t3t := false;

    if t4 = 0 then
    begin
      VV.Position.X := TargetPoint.Position.X;
      VV.Position.Y := TargetPoint.Position.Y;
      VV.Position.z := TargetPoint.Position.z;
      t4t := false;
    end;
    if t4 > 0 then
    begin
      t4t := true;
    end;
  end;

  if t4t = true then
  begin
    if t4 = 0 then
    begin
      VV.Position.X := TargetPoint.Position.X;
      VV.Position.Y := TargetPoint.Position.Y;
      VV.Position.z := TargetPoint.Position.z;
    end;
    if t4 = 1 then
    begin
      VV.Position.X := VP1.Position.X;
      VV.Position.Y := VP1.Position.Y;
      VV.Position.z := VP1.Position.z;
    end;
    if t4 = 2 then
    begin
      VV.Position.X := VP2.Position.X;
      VV.Position.Y := VP2.Position.Y;
      VV.Position.z := VP2.Position.z;
    end;
    if t4 = 3 then
    begin
      VV.Position.X := VP3.Position.X;
      VV.Position.Y := VP3.Position.Y;
      VV.Position.z := VP3.Position.z;
    end;
    if t4 = 4 then
    begin
      VV.Position.X := VP4.Position.X;
      VV.Position.Y := VP4.Position.Y;
      VV.Position.z := VP4.Position.z;
    end;
    if t4 = 5 then
    begin
      VV.Position.X := VP5.Position.X;
      VV.Position.Y := VP5.Position.Y;
      VV.Position.z := VP5.Position.z;
    end;
    if t4 = 6 then
    begin
      VV.Position.X := VP6.Position.X;
      VV.Position.Y := VP6.Position.Y;
      VV.Position.z := VP6.Position.z;
    end;
    if t4 = 7 then
    begin
      VV.Position.X := VP7.Position.X;
      VV.Position.Y := VP7.Position.Y;
      VV.Position.z := VP7.Position.z;
    end;
    if t4 = 8 then
    begin
      VV.Position.X := VP8.Position.X;
      VV.Position.Y := VP8.Position.Y;
      VV.Position.z := VP8.Position.z;
    end;
    if t4 = 9 then
    begin
      VV.Position.X := VP9.Position.X;
      VV.Position.Y := VP9.Position.Y;
      VV.Position.z := VP9.Position.z;
    end;
    if t4 = 10 then
    begin
      VV.Position.X := VP10.Position.X;
      VV.Position.Y := VP10.Position.Y;
      VV.Position.z := VP10.Position.z;
    end;
    if t4 = 11 then
    begin
      VV.Position.X := VP11.Position.X;
      VV.Position.Y := VP11.Position.Y;
      VV.Position.z := VP11.Position.z;
    end;
    if t4 = 12 then
    begin
      VV.Position.X := VP12.Position.X;
      VV.Position.Y := VP12.Position.Y;
      VV.Position.z := VP12.Position.z;
    end;
    if t4 = 13 then
    begin
      VV.Position.X := VP13.Position.X;
      VV.Position.Y := VP13.Position.Y;
      VV.Position.z := VP13.Position.z;
    end;
    if t4 = 14 then
    begin
      VV.Position.X := VP14.Position.X;
      VV.Position.Y := VP14.Position.Y;
      VV.Position.z := VP14.Position.z;
    end;
  end;
  if VectorDistance(Sphere.Position.AsVector, VV.Position.AsVector) <= MIN_DISTANCE
  then
  begin
    t1t := false;
    t2t := false;
    t3t := false;
    t4t := false;

    if t5 = 0 then
    begin
      VV.Position.X := TargetPoint.Position.X;
      VV.Position.Y := TargetPoint.Position.Y;
      VV.Position.z := TargetPoint.Position.z;
      t5t := false;
    end;
    if t5 > 0 then
    begin
      t5t := true;
    end;
  end;

  if t5t = true then
  begin
    if t5 = 0 then
    begin
      VV.Position.X := TargetPoint.Position.X;
      VV.Position.Y := TargetPoint.Position.Y;
      VV.Position.z := TargetPoint.Position.z;
    end;
    if t5 = 1 then
    begin
      VV.Position.X := VP1.Position.X;
      VV.Position.Y := VP1.Position.Y;
      VV.Position.z := VP1.Position.z;
    end;
    if t5 = 2 then
    begin
      VV.Position.X := VP2.Position.X;
      VV.Position.Y := VP2.Position.Y;
      VV.Position.z := VP2.Position.z;
    end;
    if t5 = 3 then
    begin
      VV.Position.X := VP3.Position.X;
      VV.Position.Y := VP3.Position.Y;
      VV.Position.z := VP3.Position.z;
    end;
    if t5 = 4 then
    begin
      VV.Position.X := VP4.Position.X;
      VV.Position.Y := VP4.Position.Y;
      VV.Position.z := VP4.Position.z;
    end;
    if t5 = 5 then
    begin
      VV.Position.X := VP5.Position.X;
      VV.Position.Y := VP5.Position.Y;
      VV.Position.z := VP5.Position.z;
    end;
    if t5 = 6 then
    begin
      VV.Position.X := VP6.Position.X;
      VV.Position.Y := VP6.Position.Y;
      VV.Position.z := VP6.Position.z;
    end;
    if t5 = 7 then
    begin
      VV.Position.X := VP7.Position.X;
      VV.Position.Y := VP7.Position.Y;
      VV.Position.z := VP7.Position.z;
    end;
    if t5 = 8 then
    begin
      VV.Position.X := VP8.Position.X;
      VV.Position.Y := VP8.Position.Y;
      VV.Position.z := VP8.Position.z;
    end;
    if t5 = 9 then
    begin
      VV.Position.X := VP9.Position.X;
      VV.Position.Y := VP9.Position.Y;
      VV.Position.z := VP9.Position.z;
    end;
    if t5 = 10 then
    begin
      VV.Position.X := VP10.Position.X;
      VV.Position.Y := VP10.Position.Y;
      VV.Position.z := VP10.Position.z;
    end;
    if t5 = 11 then
    begin
      VV.Position.X := VP11.Position.X;
      VV.Position.Y := VP11.Position.Y;
      VV.Position.z := VP11.Position.z;
    end;
    if t5 = 12 then
    begin
      VV.Position.X := VP12.Position.X;
      VV.Position.Y := VP12.Position.Y;
      VV.Position.z := VP12.Position.z;
    end;
    if t5 = 13 then
    begin
      VV.Position.X := VP13.Position.X;
      VV.Position.Y := VP13.Position.Y;
      VV.Position.z := VP13.Position.z;
    end;
    if t5 = 14 then
    begin
      VV.Position.X := VP14.Position.X;
      VV.Position.Y := VP14.Position.Y;
      VV.Position.z := VP14.Position.z;
    end;
  end;
  if VectorDistance(Sphere.Position.AsVector, VV.Position.AsVector) <= MIN_DISTANCE
  then
  begin
    t1t := false;
    t2t := false;
    t3t := false;
    t4t := false;
    t5t := false;
    if t6 = 0 then
    begin
      VV.Position.X := TargetPoint.Position.X;
      VV.Position.Y := TargetPoint.Position.Y;
      VV.Position.z := TargetPoint.Position.z;
      t6t := false;
    end;
    if t6 > 0 then
    begin
      t6t := true;
    end;
  end;

  if t6t = true then
  begin
    if t6 = 0 then
    begin
      VV.Position.X := TargetPoint.Position.X;
      VV.Position.Y := TargetPoint.Position.Y;
      VV.Position.z := TargetPoint.Position.z;
    end;
    if t6 = 1 then
    begin
      VV.Position.X := VP1.Position.X;
      VV.Position.Y := VP1.Position.Y;
      VV.Position.z := VP1.Position.z;
    end;
    if t6 = 2 then
    begin
      VV.Position.X := VP2.Position.X;
      VV.Position.Y := VP2.Position.Y;
      VV.Position.z := VP2.Position.z;
    end;
    if t6 = 3 then
    begin
      VV.Position.X := VP3.Position.X;
      VV.Position.Y := VP3.Position.Y;
      VV.Position.z := VP3.Position.z;
    end;
    if t6 = 4 then
    begin
      VV.Position.X := VP4.Position.X;
      VV.Position.Y := VP4.Position.Y;
      VV.Position.z := VP4.Position.z;
    end;
    if t6 = 5 then
    begin
      VV.Position.X := VP5.Position.X;
      VV.Position.Y := VP5.Position.Y;
      VV.Position.z := VP5.Position.z;
    end;
    if t6 = 6 then
    begin
      VV.Position.X := VP6.Position.X;
      VV.Position.Y := VP6.Position.Y;
      VV.Position.z := VP6.Position.z;
    end;
    if t6 = 7 then
    begin
      VV.Position.X := VP7.Position.X;
      VV.Position.Y := VP7.Position.Y;
      VV.Position.z := VP7.Position.z;
    end;
    if t6 = 8 then
    begin
      VV.Position.X := VP8.Position.X;
      VV.Position.Y := VP8.Position.Y;
      VV.Position.z := VP8.Position.z;
    end;
    if t6 = 9 then
    begin
      VV.Position.X := VP9.Position.X;
      VV.Position.Y := VP9.Position.Y;
      VV.Position.z := VP9.Position.z;
    end;
    if t6 = 10 then
    begin
      VV.Position.X := VP10.Position.X;
      VV.Position.Y := VP10.Position.Y;
      VV.Position.z := VP10.Position.z;
    end;
    if t6 = 11 then
    begin
      VV.Position.X := VP11.Position.X;
      VV.Position.Y := VP11.Position.Y;
      VV.Position.z := VP11.Position.z;
    end;
    if t6 = 12 then
    begin
      VV.Position.X := VP12.Position.X;
      VV.Position.Y := VP12.Position.Y;
      VV.Position.z := VP12.Position.z;
    end;
    if t6 = 13 then
    begin
      VV.Position.X := VP13.Position.X;
      VV.Position.Y := VP13.Position.Y;
      VV.Position.z := VP13.Position.z;
    end;
    if t6 = 14 then
    begin
      VV.Position.X := VP14.Position.X;
      VV.Position.Y := VP14.Position.Y;
      VV.Position.z := VP14.Position.z;
    end;
  end;
  if VectorDistance(Sphere.Position.AsVector, VV.Position.AsVector) <= MIN_DISTANCE
  then
  begin
    t1t := false;
    t2t := false;
    t3t := false;
    t4t := false;
    t5t := false;
    t6t := false;
    if t7 = 0 then
    begin
      VV.Position.X := TargetPoint.Position.X;
      VV.Position.Y := TargetPoint.Position.Y;
      VV.Position.z := TargetPoint.Position.z;
      t7t := false;
    end;
    if t7 > 0 then
    begin
      t7t := true;
    end;
  end;

  if t7t = true then
  begin
    if t7 = 0 then
    begin
      VV.Position.X := TargetPoint.Position.X;
      VV.Position.Y := TargetPoint.Position.Y;
      VV.Position.z := TargetPoint.Position.z;
    end;
    if t7 = 1 then
    begin
      VV.Position.X := VP1.Position.X;
      VV.Position.Y := VP1.Position.Y;
      VV.Position.z := VP1.Position.z;
    end;
    if t7 = 2 then
    begin
      VV.Position.X := VP2.Position.X;
      VV.Position.Y := VP2.Position.Y;
      VV.Position.z := VP2.Position.z;
    end;
    if t7 = 3 then
    begin
      VV.Position.X := VP3.Position.X;
      VV.Position.Y := VP3.Position.Y;
      VV.Position.z := VP3.Position.z;
    end;
    if t7 = 4 then
    begin
      VV.Position.X := VP4.Position.X;
      VV.Position.Y := VP4.Position.Y;
      VV.Position.z := VP4.Position.z;
    end;
    if t7 = 5 then
    begin
      VV.Position.X := VP5.Position.X;
      VV.Position.Y := VP5.Position.Y;
      VV.Position.z := VP5.Position.z;
    end;
    if t7 = 6 then
    begin
      VV.Position.X := VP6.Position.X;
      VV.Position.Y := VP6.Position.Y;
      VV.Position.z := VP6.Position.z;
    end;
    if t7 = 7 then
    begin
      VV.Position.X := VP7.Position.X;
      VV.Position.Y := VP7.Position.Y;
      VV.Position.z := VP7.Position.z;
    end;
    if t7 = 8 then
    begin
      VV.Position.X := VP8.Position.X;
      VV.Position.Y := VP8.Position.Y;
      VV.Position.z := VP8.Position.z;
    end;
    if t7 = 9 then
    begin
      VV.Position.X := VP9.Position.X;
      VV.Position.Y := VP9.Position.Y;
      VV.Position.z := VP9.Position.z;
    end;
    if t7 = 10 then
    begin
      VV.Position.X := VP10.Position.X;
      VV.Position.Y := VP10.Position.Y;
      VV.Position.z := VP10.Position.z;
    end;
    if t7 = 11 then
    begin
      VV.Position.X := VP11.Position.X;
      VV.Position.Y := VP11.Position.Y;
      VV.Position.z := VP11.Position.z;
    end;
    if t7 = 12 then
    begin
      VV.Position.X := VP12.Position.X;
      VV.Position.Y := VP12.Position.Y;
      VV.Position.z := VP12.Position.z;
    end;
    if t7 = 13 then
    begin
      VV.Position.X := VP13.Position.X;
      VV.Position.Y := VP13.Position.Y;
      VV.Position.z := VP13.Position.z;
    end;
    if t7 = 14 then
    begin
      VV.Position.X := VP14.Position.X;
      VV.Position.Y := VP14.Position.Y;
      VV.Position.z := VP14.Position.z;
    end;
  end;
  if VectorDistance(Sphere.Position.AsVector, VV.Position.AsVector) <= MIN_DISTANCE
  then
  begin
    t1t := false;
    t2t := false;
    t3t := false;
    t4t := false;
    t5t := false;
    t6t := false;
    t7t := false;
    if t8 = 0 then
    begin
      VV.Position.X := TargetPoint.Position.X;
      VV.Position.Y := TargetPoint.Position.Y;
      VV.Position.z := TargetPoint.Position.z;
      t8t := false;
    end;
    if t8 > 0 then
    begin
      t8t := true;
    end;
  end;

  if t8t = true then
  begin
    if t8 = 0 then
    begin
      VV.Position.X := TargetPoint.Position.X;
      VV.Position.Y := TargetPoint.Position.Y;
      VV.Position.z := TargetPoint.Position.z;
    end;
    if t8 = 1 then
    begin
      VV.Position.X := VP1.Position.X;
      VV.Position.Y := VP1.Position.Y;
      VV.Position.z := VP1.Position.z;
    end;
    if t8 = 2 then
    begin
      VV.Position.X := VP2.Position.X;
      VV.Position.Y := VP2.Position.Y;
      VV.Position.z := VP2.Position.z;
    end;
    if t8 = 3 then
    begin
      VV.Position.X := VP3.Position.X;
      VV.Position.Y := VP3.Position.Y;
      VV.Position.z := VP3.Position.z;
    end;
    if t8 = 4 then
    begin
      VV.Position.X := VP4.Position.X;
      VV.Position.Y := VP4.Position.Y;
      VV.Position.z := VP4.Position.z;
    end;
    if t8 = 5 then
    begin
      VV.Position.X := VP5.Position.X;
      VV.Position.Y := VP5.Position.Y;
      VV.Position.z := VP5.Position.z;
    end;
    if t8 = 6 then
    begin
      VV.Position.X := VP6.Position.X;
      VV.Position.Y := VP6.Position.Y;
      VV.Position.z := VP6.Position.z;
    end;
    if t8 = 7 then
    begin
      VV.Position.X := VP7.Position.X;
      VV.Position.Y := VP7.Position.Y;
      VV.Position.z := VP7.Position.z;
    end;
    if t8 = 8 then
    begin
      VV.Position.X := VP8.Position.X;
      VV.Position.Y := VP8.Position.Y;
      VV.Position.z := VP8.Position.z;
    end;
    if t8 = 9 then
    begin
      VV.Position.X := VP9.Position.X;
      VV.Position.Y := VP9.Position.Y;
      VV.Position.z := VP9.Position.z;
    end;
    if t8 = 10 then
    begin
      VV.Position.X := VP10.Position.X;
      VV.Position.Y := VP10.Position.Y;
      VV.Position.z := VP10.Position.z;
    end;
    if t8 = 11 then
    begin
      VV.Position.X := VP11.Position.X;
      VV.Position.Y := VP11.Position.Y;
      VV.Position.z := VP11.Position.z;
    end;
    if t8 = 12 then
    begin
      VV.Position.X := VP12.Position.X;
      VV.Position.Y := VP12.Position.Y;
      VV.Position.z := VP12.Position.z;
    end;
    if t8 = 13 then
    begin
      VV.Position.X := VP13.Position.X;
      VV.Position.Y := VP13.Position.Y;
      VV.Position.z := VP13.Position.z;
    end;
    if t8 = 14 then
    begin
      VV.Position.X := VP14.Position.X;
      VV.Position.Y := VP14.Position.Y;
      VV.Position.z := VP14.Position.z;
    end;
  end;
  if VectorDistance(Sphere.Position.AsVector, VV.Position.AsVector) <= MIN_DISTANCE
  then
  begin
    t1t := false;
    t2t := false;
    t3t := false;
    t4t := false;
    t5t := false;
    t6t := false;
    t7t := false;
    t8t := false;
    if t9 = 0 then
    begin
      VV.Position.X := TargetPoint.Position.X;
      VV.Position.Y := TargetPoint.Position.Y;
      VV.Position.z := TargetPoint.Position.z;
      t9t := false;
    end;
    if t9 > 0 then
    begin
      // t9t:=true;
    end;
  end;

end;

procedure TFormShoot.Lo;
const
  N = 140; { кол-во вершин графа }
var
  incite: Boolean;
  Map: array [1 .. N, 1 .. N] of Integer;
  // Карта.map[i,j] не 0,если
  // точки i и j соединены

  road: array [1 .. N] of Integer;
  // Дорога - номера точек карты

  incl: array [1 .. N] of Boolean; // incl[1]равен TRUE,если точка
  // с номером i включена в road

  start, finish: Integer;
  // Начальная и конечная точки

  found: Boolean;
  len: Integer; // длина найденного (минимального) маршрута
  c_len: Integer; // длина текущего (формируемого) маршрута
  i, j: Integer;

  procedure step(s, f, p: Integer);
  var
    c: Integer;
    i: Integer;

  begin

    if s = f then
    begin

      len := c_len; { сохраним длину найденного маршрута }

      { вывод найденного маршрута }

      // Label1.caption := ', длина:' + IntToStr(len)+#13 ;

      if road[1] > 0 then
      begin
        t1 := road[1];
      end;
      if road[1] = 0 then
      begin
        t1 := 0;
      end;
      if road[2] > 0 then
      begin
        t2 := road[2];
      end;
      if road[2] = 0 then
      begin
        t2 := 0;
      end;
      if road[3] > 0 then
      begin
        t3 := road[3];
      end;
      if road[3] = 0 then
      begin
        t3 := 0;
      end;

      if road[4] > 0 then
      begin
        t4 := road[4];
      end;
      if road[4] = 0 then
      begin
        t4 := 0;
      end;
      if road[5] > 0 then
      begin
        t5 := road[5];
      end;
      if road[5] = 0 then
      begin
        t5 := 0;
      end;
      if road[6] > 0 then
      begin
        t6 := road[6];
      end;
      if road[6] = 0 then
      begin
        t6 := 0;
      end;

      if road[7] > 0 then
      begin
        t7 := road[7];
      end;
      if road[7] = 0 then
      begin
        t7 := 0;
      end;
      if road[8] > 0 then
      begin
        t8 := road[8];
      end;
      if road[8] = 0 then
      begin
        t8 := 0;
      end;
      if road[9] > 0 then
      begin
        t9 := road[9];
      end;
      if road[9] = 0 then
      begin
        t9 := 0;
      end;
      if road[10] > 0 then
      begin
        t10 := road[10];
      end;
      if road[10] = 0 then
      begin
        t10 := 0;
      end;

      if road[11] > 0 then
      begin
        t11 := road[11];
      end;
      if road[11] = 0 then
      begin
        t11 := 0;
      end;
      if road[12] > 0 then
      begin
        t12 := road[12];
      end;
      if road[12] = 0 then
      begin
        t12 := 0;
      end;
      if road[13] > 0 then
      begin
        t13 := road[13];
      end;
      if road[13] = 0 then
      begin
        t13 := 0;
      end;
      if road[14] > 0 then
      begin
        t14 := road[14];
      end;
      if road[14] = 0 then
      begin
        t14 := 0;
      end;

    end;

    if s <> f then
    begin

      { выбираем очередную точку }
      for c := 1 to N do { проверяем все вершины }
        if (Map[s, c] <> 0) and (not incite) and
          ((len = 0) or (c_len + Map[s, c] < len)) then
        begin
          // точка соединена с текущей, но не включена в маршрут
          road[p] := c; { добавим вершину в    путь }

          incl[c] := true; { пометим вершину как включенную }

          c_len := c_len + Map[s, c];
          step(c, f, p + 1);

          incite := false;
          road[p] := 0;
          c_len := c_len - Map[s, c];

        end;
    end;
  end;

begin;

  VPstart.Position.X := Sphere.Position.X;
  VPstart.Position.Y := Sphere.Position.Y;
  VPstart.Position.z := Sphere.Position.z;
  Actor.Tag := 1;
  t1t := true;
  t2t := false;
  t3t := false;
  t4t := false;
  t5t := false;
  t6t := false;
  t7t := false;
  { инициализация массивов }
  for i := 1 to N do
    road[i] := 0;

  for i := 1 to N do
    incl[i] := false;

  { ввод описания карты из SrtingGrid.Cells }
  for i := 1 to N do
    for j := 1 to N do
      if StringGrid1.Cells[i, j] <> '' then
        Map[i, j] := StrToInt(StringGrid1.Cells[i, j])
      else
        Map[i, j] := 0;

  len := 0; // длина найденного (минимального) маршрута
  c_len := 0; // длина текущего (формируемого) маршрута

  // Lo(start, finish);
  start := c;
  finish := v;

  road[1] := start; { внесем точку в маршрут }
  incl[start] := true; { пометим ее как включенную }

  step(start, finish, 2); { ищем вторую точку маршрута }

  // проверим, найден ли хотя бы один    путь
  if not found then
    // Label1.caption := 'Указанные точки не соединены!';

    Label6.Caption := 'start := ' + FloatTostr(start);
  Label7.Caption := 'start := ' + FloatTostr(finish);

end;

procedure TFormShoot.PlayerBehaviours0Collision(Sender: TObject;
  ObjectCollided: TGLBaseSceneObject; CollisionInfo: TDCECollision);
var
  v: TAffineVector;
begin

  if ObjectCollided.Tag = 20 then
  begin
    Label2.Caption := 'Столкнулись         ';
    Actor.Tag := 0;

    if Actor.CurrentAnimation <> 'stand' then
    begin
      Actor.SwitchToAnimation('stand');
    end;

  end;

end;

procedure TFormShoot.S1Behaviours10Collision(Sender: TObject;
  ObjectCollided: TGLBaseSceneObject; CollisionInfo: TDCECollision);
var
  v: TAffineVector;
begin

  if ObjectCollided.Tag = 10 then
  begin
    // Label2.Caption:='Столкнулись 999   ';

  end;

end;

procedure TFormShoot.FormMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  // Меняем расстояние от камеры до игрока колёсиком мыши
  Camera.AdjustDistanceToTarget(Power(1.5, WheelDelta / 120));
end;

procedure TFormShoot.SceneViewerMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
begin

  LastX := X;
  LastY := Y;
end;

procedure TFormShoot.CadencerProgress(Sender: TObject;
  const deltaTime, newTime: Double);

var
  Force: TVector3f;
  mp: TPoint;

  procedure step1;

  begin
    start := c;
    finish := v;

    Label15.Caption := ' vp999vd := ' + FloatTostr(vp999vd);
    Label16.Caption := ' vp1vd := ' + FloatTostr(vp1vd);
    Label17.Caption := ' vp2vd := ' + FloatTostr(vp2vd);
    Label18.Caption := ' vp3vd := ' + FloatTostr(vp3vd);
    Label19.Caption := ' vp4vd := ' + FloatTostr(vp4vd);
    Label20.Caption := ' vp5vd := ' + FloatTostr(vp5vd);
    Label21.Caption := ' vp6vd := ' + FloatTostr(vp6vd);
    Label22.Caption := ' vp7vd := ' + FloatTostr(vp7vd);
    Label23.Caption := ' vp8vd := ' + FloatTostr(vp8vd);

    Label8.Caption := 'T1 ' + FloatTostr(t1);
    Label11.Caption := 'T2 ' + FloatTostr(t2);
    Label12.Caption := 'T3 ' + FloatTostr(t3);
    Label13.Caption := 'T4 ' + FloatTostr(t4);
    Label14.Caption := 'T5 ' + FloatTostr(t5);
    vp1vd := VectorDistance(VP1.Position.AsVector, VPstart.Position.AsVector);
    vp2vd := VectorDistance(VP2.Position.AsVector, VPstart.Position.AsVector);
    vp3vd := VectorDistance(VP3.Position.AsVector, VPstart.Position.AsVector);
    vp4vd := VectorDistance(VP4.Position.AsVector, VPstart.Position.AsVector);
    vp5vd := VectorDistance(VP5.Position.AsVector, VPstart.Position.AsVector);
    vp6vd := VectorDistance(VP6.Position.AsVector, VPstart.Position.AsVector);
    vp7vd := VectorDistance(VP7.Position.AsVector, VPstart.Position.AsVector);
    vp8vd := VectorDistance(VP8.Position.AsVector, VPstart.Position.AsVector);
    vp9vd := VectorDistance(VP9.Position.AsVector, VPstart.Position.AsVector);
    vp10vd := VectorDistance(VP10.Position.AsVector, VPstart.Position.AsVector);
    vp11vd := VectorDistance(VP11.Position.AsVector, VPstart.Position.AsVector);
    vp12vd := VectorDistance(VP12.Position.AsVector, VPstart.Position.AsVector);
    vp13vd := VectorDistance(VP13.Position.AsVector, VPstart.Position.AsVector);
    vp14vd := VectorDistance(VP14.Position.AsVector, VPstart.Position.AsVector);

    vp999vd := VectorDistance(Sphere.Position.AsVector, GLC.Position.AsVector);
    // vp888vd:=VectorDistance(TargetPointSphere.Position.AsVector, VPstart.Position.AsVector);

    vp1vf := VectorDistance(VP1.Position.AsVector,
      TargetPoint.Position.AsVector);
    vp2vf := VectorDistance(VP2.Position.AsVector,
      TargetPoint.Position.AsVector);
    vp3vf := VectorDistance(VP3.Position.AsVector,
      TargetPoint.Position.AsVector);
    vp4vf := VectorDistance(VP4.Position.AsVector,
      TargetPoint.Position.AsVector);
    vp5vf := VectorDistance(VP5.Position.AsVector,
      TargetPoint.Position.AsVector);
    vp6vf := VectorDistance(VP6.Position.AsVector,
      TargetPoint.Position.AsVector);
    vp7vf := VectorDistance(VP7.Position.AsVector,
      TargetPoint.Position.AsVector);
    vp8vf := VectorDistance(VP8.Position.AsVector,
      TargetPoint.Position.AsVector);
    vp9vf := VectorDistance(VP9.Position.AsVector,
      TargetPoint.Position.AsVector);
    vp10vf := VectorDistance(VP10.Position.AsVector,
      TargetPoint.Position.AsVector);
    vp11vf := VectorDistance(VP11.Position.AsVector,
      TargetPoint.Position.AsVector);
    vp12vf := VectorDistance(VP12.Position.AsVector,
      TargetPoint.Position.AsVector);
    vp13vf := VectorDistance(VP13.Position.AsVector,
      TargetPoint.Position.AsVector);
    vp14vf := VectorDistance(VP14.Position.AsVector,
      TargetPoint.Position.AsVector);

    c := 0;
    v := 0;
    VV.Position.X := TargetPoint.Position.X;
    VV.Position.Y := TargetPoint.Position.Y;
    VV.Position.z := TargetPoint.Position.z;

    if vp999vd < 35 then
    begin

      if (vp1vd < vp2vd) and (vp1vd < vp3vd) and (vp1vd < vp4vd) and
        (vp1vd < vp5vd) and (vp1vd < vp6vd) and (vp1vd < vp7vd) and
        (vp1vd < vp8vd) and (vp1vd < vp9vd) then
      begin
        Label10.Caption := 'первая ближнея ';
        c := 1;
      end;

      if (vp2vd < vp1vd) and (vp2vd < vp3vd) and (vp2vd < vp4vd) and
        (vp2vd < vp5vd) and (vp2vd < vp6vd) and (vp2vd < vp7vd) and
        (vp2vd < vp8vd) and (vp2vd < vp9vd) then
      begin
        Label10.Caption := 'вторая ближнея ';
        c := 2;
      end;

      if (vp3vd < vp1vd) and (vp3vd < vp2vd) and (vp3vd < vp4vd) and
        (vp3vd < vp5vd) and (vp3vd < vp6vd) and (vp3vd < vp7vd) and
        (vp3vd < vp8vd) and (vp3vd < vp9vd) then
      begin
        Label10.Caption := 'третья ближнея ';
        c := 3;
      end;

      if (vp4vd < vp1vd) and (vp4vd < vp2vd) and (vp4vd < vp3vd) and
        (vp4vd < vp5vd) and (vp4vd < vp6vd) and (vp4vd < vp7vd) and
        (vp4vd < vp8vd) and (vp4vd < vp9vd) then
      begin
        Label10.Caption := 'четвертая ближнея ';
        c := 4;
      end;

      if (vp5vd < vp1vd) and (vp5vd < vp2vd) and (vp5vd < vp3vd) and
        (vp5vd < vp4vd) and (vp5vd < vp6vd) and (vp5vd < vp7vd) and
        (vp5vd < vp8vd) and (vp5vd < vp9vd) then
      begin
        Label10.Caption := 'пятая ближнея ';
        c := 5;
      end;

      if (vp6vd < vp1vd) and (vp6vd < vp2vd) and (vp6vd < vp3vd) and
        (vp6vd < vp4vd) and (vp6vd < vp5vd) and (vp6vd < vp7vd) and
        (vp6vd < vp8vd) and (vp6vd < vp9vd) then
      begin
        Label10.Caption := 'шестая ближнея ';
        c := 6;
      end;

      if (vp7vd < vp1vd) and (vp7vd < vp2vd) and (vp7vd < vp3vd) and
        (vp7vd < vp4vd) and (vp7vd < vp5vd) and (vp7vd < vp6vd) and
        (vp7vd < vp8vd) and (vp7vd < vp9vd) then
      begin
        Label10.Caption := 'седьмая ближнея ';
        c := 7;
      end;

      if (vp8vd < vp1vd) and (vp8vd < vp2vd) and (vp8vd < vp3vd) and
        (vp8vd < vp4vd) and (vp8vd < vp5vd) and (vp8vd < vp6vd) and
        (vp8vd < vp7vd) and (vp8vd < vp9vd) then
      begin
        Label10.Caption := 'восьмая ближнея ';
        c := 8;
      end;

      if (vp9vd < vp1vd) and (vp9vd < vp2vd) and (vp9vd < vp3vd) and
        (vp9vd < vp4vd) and (vp9vd < vp5vd) and (vp9vd < vp6vd) and
        (vp9vd < vp7vd) and (vp9vd < vp8vd) then
      begin
        Label10.Caption := 'девятая ближнея ';
        c := 9;
      end;

      if (vp10vd < vp1vd) and (vp10vd < vp2vd) and (vp10vd < vp3vd) and
        (vp10vd < vp4vd) and (vp10vd < vp5vd) and (vp10vd < vp6vd) and
        (vp10vd < vp7vd) and (vp10vd < vp8vd) and (vp10vd < vp9vd) then
      begin
        Label10.Caption := 'девятая ближнея ';
        c := 10;
      end;

      if (vp11vd < vp1vd) and (vp11vd < vp2vd) and (vp11vd < vp3vd) and
        (vp11vd < vp4vd) and (vp11vd < vp5vd) and (vp11vd < vp6vd) and
        (vp11vd < vp7vd) and (vp11vd < vp8vd) and (vp11vd < vp9vd) and
        (vp11vd < vp10vd) then
      begin
        Label10.Caption := 'одинацатая ближнея ';
        c := 11;
      end;

      if (vp12vd < vp1vd) and (vp12vd < vp2vd) and (vp12vd < vp3vd) and
        (vp12vd < vp4vd) and (vp12vd < vp5vd) and (vp12vd < vp6vd) and
        (vp12vd < vp7vd) and (vp12vd < vp8vd) and (vp12vd < vp9vd) and
        (vp12vd < vp10vd) and (vp12vd < vp11vd) then
      begin
        Label10.Caption := 'двенадцатая ближнея ';
        c := 12;
      end;

      if (vp13vd < vp1vd) and (vp13vd < vp2vd) and (vp13vd < vp3vd) and
        (vp13vd < vp4vd) and (vp13vd < vp5vd) and (vp13vd < vp6vd) and
        (vp13vd < vp7vd) and (vp13vd < vp8vd) and (vp13vd < vp9vd) and
        (vp13vd < vp10vd) and (vp13vd < vp11vd) and (vp13vd < vp12vd) then
      begin
        Label10.Caption := 'тренадцатая ближнея ';
        c := 13;
      end;

      if (vp14vd < vp1vd) and (vp14vd < vp2vd) and (vp14vd < vp3vd) and
        (vp14vd < vp4vd) and (vp14vd < vp5vd) and (vp14vd < vp6vd) and
        (vp14vd < vp7vd) and (vp14vd < vp8vd) and (vp14vd < vp9vd) and
        (vp14vd < vp10vd) and (vp14vd < vp11vd) and (vp14vd < vp12vd) and
        (vp14vd < vp13vd) then
      begin
        Label10.Caption := 'четырнацатая ближнея ';
        c := 14;
      end;

    end;

    if (vp1vf < vp2vf) and (vp1vf < vp3vf) and (vp1vf < vp4vf) and
      (vp1vf < vp5vf) and (vp1vf < vp6vf) and (vp1vf < vp7vf) and
      (vp1vf < vp8vf) and (vp1vf < vp9vf) then
    begin
      v := 1;
    end;

    if (vp2vf < vp1vf) and (vp2vf < vp3vf) and (vp2vf < vp4vf) and
      (vp2vf < vp5vf) and (vp2vf < vp6vf) and (vp2vf < vp7vf) and
      (vp2vf < vp8vf) and (vp2vf < vp9vf) then
    begin
      v := 2;
    end;

    if (vp3vf < vp1vf) and (vp3vf < vp2vf) and (vp3vf < vp4vf) and
      (vp3vf < vp5vf) and (vp3vf < vp6vf) and (vp3vf < vp7vf) and
      (vp3vf < vp8vf) and (vp3vf < vp9vf) then
    begin
      v := 3;
    end;
    if (vp4vf < vp1vf) and (vp4vf < vp2vf) and (vp4vf < vp3vf) and
      (vp4vf < vp5vf) and (vp4vf < vp6vf) and (vp4vf < vp7vf) and
      (vp4vf < vp8vf) and (vp4vf < vp9vf) then
    begin
      v := 4;
    end;
    if (vp5vf < vp1vf) and (vp5vf < vp2vf) and (vp5vf < vp3vf) and
      (vp5vf < vp4vf) and (vp5vf < vp6vf) and (vp5vf < vp7vf) and
      (vp5vf < vp8vf) and (vp5vf < vp9vf) then
    begin
      v := 5;
    end;

    if (vp6vf < vp1vf) and (vp6vf < vp2vf) and (vp6vf < vp3vf) and
      (vp6vf < vp4vf) and (vp6vf < vp5vf) and (vp6vf < vp7vf) and
      (vp6vf < vp8vf) and (vp6vf < vp9vf) then
    begin
      v := 6;
    end;
    if (vp7vf < vp1vf) and (vp7vf < vp2vf) and (vp7vf < vp3vf) and
      (vp7vf < vp4vf) and (vp7vf < vp5vf) and (vp7vf < vp6vf) and
      (vp7vf < vp8vf) and (vp7vf < vp9vf) then
    begin
      v := 7;
    end;

    if (vp8vf < vp1vf) and (vp8vf < vp2vf) and (vp8vf < vp3vf) and
      (vp8vf < vp4vf) and (vp8vf < vp5vf) and (vp8vf < vp6vf) and
      (vp8vf < vp7vf) and (vp8vf < vp9vf) then
    begin
      v := 8;
    end;

    if (vp9vf < vp1vf) and (vp9vf < vp2vf) and (vp9vf < vp3vf) and
      (vp9vf < vp4vf) and (vp9vf < vp5vf) and (vp9vf < vp6vf) and
      (vp9vf < vp7vf) and (vp9vf < vp8vf) then
    begin
      v := 9;
    end;
    if (vp10vf < vp1vf) and (vp10vf < vp2vf) and (vp10vf < vp3vf) and
      (vp10vf < vp4vf) and (vp10vf < vp5vf) and (vp10vf < vp6vf) and
      (vp10vf < vp7vf) and (vp10vf < vp8vf) and (vp10vf < vp9vf) then
    begin
      v := 10;
    end;
    if (vp11vf < vp1vf) and (vp11vf < vp2vf) and (vp11vf < vp3vf) and
      (vp11vf < vp4vf) and (vp11vf < vp5vf) and (vp11vf < vp6vf) and
      (vp11vf < vp7vf) and (vp11vf < vp8vf) and (vp11vf < vp9vf) and
      (vp11vf < vp10vf) then
    begin
      v := 11;
    end;
    if (vp12vf < vp1vf) and (vp12vf < vp2vf) and (vp12vf < vp3vf) and
      (vp12vf < vp4vf) and (vp12vf < vp5vf) and (vp12vf < vp6vf) and
      (vp12vf < vp7vf) and (vp12vf < vp8vf) and (vp12vf < vp9vf) and
      (vp12vf < vp10vf) and (vp12vf < vp11vf) then
    begin
      v := 12;
    end;
    if (vp13vf < vp1vf) and (vp13vf < vp2vf) and (vp13vf < vp3vf) and
      (vp13vf < vp4vf) and (vp13vf < vp5vf) and (vp13vf < vp6vf) and
      (vp13vf < vp7vf) and (vp13vf < vp8vf) and (vp13vf < vp9vf) and
      (vp13vf < vp10vf) and (vp13vf < vp11vf) and (vp13vf < vp12vf) then
    begin
      v := 13;
    end;
    if (vp14vf < vp1vf) and (vp14vf < vp2vf) and (vp14vf < vp3vf) and
      (vp14vf < vp4vf) and (vp14vf < vp5vf) and (vp14vf < vp6vf) and
      (vp14vf < vp7vf) and (vp14vf < vp8vf) and (vp14vf < vp9vf) and
      (vp14vf < vp10vf) and (vp14vf < vp11vf) and (vp14vf < vp12vf) and
      (vp14vf < vp13vf) then
    begin
      v := 14;
    end;
  end;

begin
  tX := Sphere.Position.X;
  tY := Sphere.Position.Y;
  tZ := Sphere.Position.z;
  step1;

  ass := VectorDistance(TargetPoint.Position.AsVector,
    Sphere.Position.AsVector);
  Label3.Caption := 'Дистанция между TargetPoint и актером ' + FloatTostr(ass);

  Label4.Caption := 'актер posi_X ' + FloatTostr(tX);
  Label5.Caption := 'актер posi_Z ' + FloatTostr(tZ);
  La;

  GetCursorPos(mp);
  mp := SceneViewer.ScreenToClient(mp);
  if IsKeyDown(VK_RBUTTON) then
  begin
    Camera.MoveAroundTarget((mY - mp.Y) * 0.5, (mX - mp.X) * 0.5);
    mX := mp.X;
    mY := mp.Y;
  end;
  mX := LastX;
  mY := LastY;

  if VectorDistance(Sphere.Position.AsVector, TargetPoint.Position.AsVector) <= MIN_DISTANCE
  then
  begin
    Actor.Tag := 0;
    VPstart.Position.X := Sphere.Position.X;
    VPstart.Position.Y := Sphere.Position.Y;
    VPstart.Position.z := Sphere.Position.z;
    if Actor.CurrentAnimation <> 'stand' then
    begin
      Actor.SwitchToAnimation('stand');
    end;
  end;

  // Движение персонажа к цели
  if Actor.Tag <> 0 then
  begin

    if VectorDistance(Sphere.Position.AsVector, VV.Position.AsVector) <= MIN_DISTANCE
    then
    begin
      Actor.Tag := 0;
      if Actor.CurrentAnimation <> 'stand' then
        Actor.SwitchToAnimation('stand');

    end
    else
    begin
      if Actor.Tag = 1 then
      begin

      end;

      if Actor.CurrentAnimation <> 'run' then
        Actor.SwitchToAnimation('run');

      // Sphere.PointTo(vv.Position.AsVector, NullHmgVector); // Указываем направление
      if vp999vd < 35 then
      begin
        if colll = true then
        begin
          Actor.Tag := 0;
          Lo;

          Label24.Caption := 'colll=true ';
        end;

        Sphere.PointTo(VV.Position.AsVector, NullHmgVector);
        step1;
        Force := AffineVectorMake(0, 0, 108);
        // Actor.Tag := 1;
        colll := false;
      end;
      if vp999vd > 35 then
      begin
        colll := true;
        step1;
        Force := AffineVectorMake(0, 0, 108); // Сила, с которой мы двигаемся
        Sphere.PointTo(VV.Position.AsVector, NullHmgVector);
      end;
      GetOrCreateDCEDynamic(Sphere).ApplyAccel(Force); // Применяем силу
    end;
  end;
  CameraCube.Position := Sphere.Position; // Ставим камеру на место
  if IsKeyDown(' ') then
  begin
    Force := AffineVectorMake(0, 5000, 0);
    // Сила, с которой мы прыгаем. Не правда ли много %)
    GetOrCreateDCEDynamic(Sphere).ApplyAccel(Force); // Применяем силу
  end;
  SceneViewer.Invalidate;
end;

procedure TFormShoot.SceneViewerMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  rayStart, rayVector, iPoint, iNormal: TVector;
  pick: TGLCustomSceneObject;
begin
  if Actor.Tag = 0 then
  begin
    if Button = TMouseButton.mbLeft then
    begin
      Actor.Tag := 0;

      // Устанавливаем вектор - текущее положение камеры
      SetVector(rayStart, Camera.AbsolutePosition);
      // Устанавливаем вектор - текущее положение курсора
      SetVector(rayVector, SceneViewer.Buffer.ScreenToVector
        (AffineVectorMake(LastX, SceneViewer.Height - LastY, 0)));
      // Получаем вектор iPoint, который является точкой, куда нажали мышью :)
      if Map.OctreeRayCastIntersect(rayStart, VectorNormalize(rayVector),
        @iPoint, @iNormal) then
      begin
        // Ставим туда цель
        TargetPoint.Visible := true;
        TargetPoint.Position.AsVector := iPoint;
        TargetPoint.Direction.AsVector := VectorNormalize(iNormal);
        // Уговариваем персонажа идти
        pick := (SceneViewer.Buffer.GetPickedObject(X, Y)
          as TGLCustomSceneObject);
        Label1.Caption := '' + pick.Name;

      end;
      if (pick.Name = 'Map') then
      begin

        Label1.Caption := 'Бежать';
      end;
    end;
  end;

  if Button = TMouseButton.mbRight then
  begin

  end;
end;

procedure TFormShoot.Button2Click(Sender: TObject);

begin
  Lo;

end;

procedure TFormShoot.FormActivate(Sender: TObject);
var
  i: Integer;
begin
  // нумерация строк
  for i := 1 to 14 do
    StringGrid1.Cells[0, i] := IntToStr(i); // нумерация колонок

  for i := 1 to 14 do
    StringGrid1.Cells[i, 0] := IntToStr(i);

  // описание предопределенной карты
  StringGrid1.Cells[1, 2] := '10';
  StringGrid1.Cells[2, 1] := '10';
  StringGrid1.Cells[2, 3] := '10';
  StringGrid1.Cells[3, 2] := '10';
  StringGrid1.Cells[3, 4] := '10';
  StringGrid1.Cells[4, 3] := '10';
  StringGrid1.Cells[4, 5] := '10';
  StringGrid1.Cells[5, 4] := '10';
  StringGrid1.Cells[5, 6] := '10';
  StringGrid1.Cells[6, 5] := '10';
  StringGrid1.Cells[6, 7] := '10';
  StringGrid1.Cells[7, 6] := '10';
  StringGrid1.Cells[7, 8] := '10';
  StringGrid1.Cells[8, 7] := '10';
  StringGrid1.Cells[8, 1] := '30';
  StringGrid1.Cells[1, 8] := '30';
  StringGrid1.Cells[1, 9] := '10';
  StringGrid1.Cells[9, 1] := '10';
  StringGrid1.Cells[9, 8] := '10';
  StringGrid1.Cells[8, 9] := '10';
  StringGrid1.Cells[10, 6] := '10';
  StringGrid1.Cells[6, 10] := '10';
  StringGrid1.Cells[10, 4] := '10';
  StringGrid1.Cells[4, 10] := '10';
  StringGrid1.Cells[10, 5] := '10';
  StringGrid1.Cells[5, 10] := '10';
  StringGrid1.Cells[5, 11] := '10';
  StringGrid1.Cells[11, 5] := '10';
  StringGrid1.Cells[11, 12] := '10';
  StringGrid1.Cells[12, 11] := '10';
  StringGrid1.Cells[12, 13] := '10';
  StringGrid1.Cells[13, 12] := '10';
  StringGrid1.Cells[13, 14] := '10';
  StringGrid1.Cells[14, 13] := '10';

end;

end.
