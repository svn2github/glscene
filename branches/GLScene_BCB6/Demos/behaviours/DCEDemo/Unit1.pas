{: This is a basic use for the Dynamic Collision Engine (DCE) by Locas Goiareb.<p>

     The engine pretty much works by creating a TGLDCEManager, and several
     TGLBDCEBody behaviours on the objects that should interact. Each object
     can be either an ellipsoid, cube, freeForm or terrain, have different
     sizes and friction, respond differently to collisions, etc.

     This means your next FPS project is pretty much done: All you have to do
     is keep loading object files into freeForms and letting DCE do the trick
     for you. The only "real" code in this demo is inside the onProgress event
     of the cadencer, that takes care of input. 

}
unit Unit1;

interface

uses
  glDCE,
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, GLWin32Viewer, StdCtrls, ExtCtrls, GLHeightData, GLScene,
  GLObjects, GLTerrainRenderer, GLGeomObjects, GLMisc, GLVectorFileObjects,
  GLCadencer, vectorGeometry, glFile3ds, jpeg, keyboard;

type
  TForm1 = class(TForm)
    GLCadencer1: TGLCadencer;
    GLBitmapHDS1: TGLBitmapHDS;
    GLSceneViewer1: TGLSceneViewer;
    Panel1: TPanel;
    cbEnemy: TCheckBox;
    cbTerrain: TCheckBox;
    cbEllipse: TCheckBox;
    cbIce: TCheckBox;
    cbBeer: TCheckBox;
    cbMap: TCheckBox;
    lbCollided: TLabel;
    lbFPS: TLabel;
    Memo1: TMemo;
    GLScene1: TGLScene;
    Beer: TGLFreeForm;
    normal: TGLArrowLine;
    Map: TGLFreeForm;
    Ice: TGLFreeForm;
    Ellipsoid: TGLSphere;
    Enemy: TGLSphere;
    Player: TGLDummyCube;
    plBody: TGLSphere;
    GLCamera1: TGLCamera;
    GLLightSource1: TGLLightSource;
    Terrain: TGLTerrainRenderer;
    DCEManager1: TGLDCEManager;
    Timer1: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure GLCadencer1Progress(Sender: TObject; const deltaTime,
      newTime: Double);
    procedure DCEManager1Collision(Sender: TObject; object1,
      object2: TGLBaseSceneObject; CollisionInfo: TDCECollision);
    procedure Timer1Timer(Sender: TObject);
    procedure GLSceneViewer1MouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FormKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure PlayerBehaviours0Collision(Sender: TObject;
      ObjectCollided: TGLBaseSceneObject; CollisionInfo: TDCECollision);
  private
    { Private declarations }
  public
    { Public declarations }

    mx, my : Integer;
    LastEnemyPos: TVector;
  end;

var
  Form1: TForm1;
  CanJump: Boolean = True;
  EnemyDistance: Single = 0;
  EnemyTime: Single = 0;
  EnemyStuck: Single = 1;

implementation

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
  //Load room
  Map.LoadFromFile('..\..\media\Map.3ds');
  Map.Up.SetVector(0,1,0);
  Map.BuildOctree;

  //Load "frozen lake"
  Ice.LoadFromFile('..\..\media\beer.3ds');
  Ice.Up.SetVector(0,1,0);
  Ice.BuildOctree;

  //Load Beer
  Beer.LoadFromFile('..\..\media\beer.3ds');
  Beer.BuildOctree;

  //Load Terrain
  GLBitmapHDS1.MaxPoolSize:=8*1024*1024;
  GLBitmapHDS1.Picture.LoadFromFile('..\..\media\terrain.bmp');
  Terrain.Material.Texture.Image.LoadFromFile('..\..\media\snow512.jpg');
  Terrain.Material.Texture.Disabled := False;
  Terrain.TilesPerTexture:=256/Terrain.TileSize;

  //Used to calculate enemy jumps
  LastEnemyPos := Enemy.AbsolutePosition;
end;

procedure TForm1.GLCadencer1Progress(Sender: TObject; const deltaTime,
  newTime: Double);
var Force, Rotate: TAffineVector;
    Dist: Single;
const
  cForce: Single = 80;
  aForce: Single = 90;

begin

  Force := NullVector;
  if IsKeyDown('w') then Force[2] := cForce;
  if IsKeyDown('s') then Force[2] := -cForce;
  if IsKeyDown('a') then Force[0] := cForce;
  if IsKeyDown('d') then Force[0] := -cForce;

  //Gravity forces
  if IsKeyDown('x') then Force[1] := -cForce;
  if IsKeyDown('g') then DCEManager1.Gravity.Y := 0
  else DCEManager1.Gravity.Y := -9;

  //Rotation
  Rotate := NullVector;
  if IsKeyDown(VK_RIGHT) then Rotate[1] := aForce * deltaTime;
  if IsKeyDown(VK_LEFT) then Rotate[1] := -aForce * deltaTime;
  if IsKeyDown(VK_UP) then Rotate[2] :=aForce * deltaTime;
  if IsKeyDown(VK_DOWN) then Rotate[2] :=-aForce * deltaTime;

  //Move player
  Player.Turn(Rotate[1]);
  TGLBDCEBody(Player.Behaviours.Behaviour[0]).AddForce(Force);

  //Move Enemy
  EnemyTime := EnemyTime + (deltaTime * 0.5);
  Dist := VectorDistance(Enemy.AbsolutePosition,LastEnemyPos);
  EnemyDistance := EnemyDistance + Dist;
  if EnemyDistance / EnemyTime < 5 then EnemyStuck := EnemyStuck - deltaTime;
  LastEnemyPos := Enemy.AbsolutePosition;

  //Check if the enemy is stucked and jump
  if EnemyStuck < 0 then
  begin
    EnemyStuck := 1;
    EnemyTime := 0;
    EnemyDistance := 0;
    TGLBDCEBody(Enemy.Behaviours.Behaviour[0]).Jump(1.3,20);
  end;

  Enemy.PointTo(Player,VectorMake(0,0,0,0));
  Force := AffineVectorMake(0,0,20);
  TGLBDCEBody(Enemy.Behaviours.Behaviour[0]).AddForce(Force);
end;

procedure TForm1.DCEManager1Collision(Sender: TObject; object1,
  object2: TGLBaseSceneObject; CollisionInfo: TDCECollision);
begin
  //Example on how to use the Manager collision event
  if object1.Name = 'Player' then
  begin
    if object2.Name = 'Map' then cbMap.Checked := True;
    if object2.Name = 'Beer' then cbBeer.Checked := True;
    if object2.Name = 'Ice' then cbIce.Checked := True;
    if object2.Name = 'Ellipsoid' then cbEllipse.Checked := True;
    if object2.Name = 'Terrain' then cbTerrain.Checked := True;
    if object2.Name = 'Enemy' then cbEnemy.Checked := True;
  end;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  cbMap.Checked := False;
  cbBeer.Checked := False;
  cbIce.Checked := False;
  cbEllipse.Checked := False;
  cbTerrain.Checked := False;
  cbEnemy.Checked := False;

  lbFPS.Caption := Format('FPS: %.3f',[GLSceneViewer1.FramesPerSecond]);
  GLSceneViewer1.ResetPerformanceMonitor;
end;

procedure TForm1.GLSceneViewer1MouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  if ssLeft in Shift then
  begin
    Player.Turn(-(mx-x));
    GLCamera1.MoveAroundTarget(my-y,0);
  end;
  mx:=x;
  my:=y;
end;

procedure TForm1.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  //Jumping
  if (Key = VK_SPACE) and (CanJump) then
  begin
    TGLBDCEBody(Player.Behaviours.Behaviour[0]).Jump(1.3,20);
    CanJump := False;
  end;
end;

procedure TForm1.FormKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
   CanJump := True;
end;

procedure TForm1.PlayerBehaviours0Collision(Sender: TObject;
  ObjectCollided: TGLBaseSceneObject; CollisionInfo: TDCECollision);
begin
  //Example on how to use the Behaviour collision event
  lbCollided.Caption := ObjectCollided.Name;
  if CollisionInfo.Nearest then
  begin
    normal.AbsoluteDirection := VectorMake(CollisionInfo.Normal);
    normal.AbsolutePosition := VectorMake(CollisionInfo.Point);
  end;
end;

end.
