unit main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls,
   
  GLScene, GLVectorFileObjects, GLMaterial, GLObjects, GLWin32Viewer,
  MD3Helper, GLCadencer, GLGraph, GLVectorGeometry, GLCollision, GLTexture,
  GLCelShader, GLHiddenLineShader, GLHUDObjects, GLBitmapFont, GLWindowsFont,
  GLCoordinates, GLCrossPlatform, GLBaseClasses;

const
  aamType: array [false .. true] of TGLActorAnimationMode = (aamLoop,
    aamLoopBackward);

type
  TForm1 = class(TForm)
    GLScene1: TGLScene;
    GLSceneViewer1: TGLSceneViewer;
    Actor: TGLDummyCube;
    GLCamera1: TGLCamera;
    GLCadencer1: TGLCadencer;
    GLLightSource1: TGLLightSource;
    GLPlane1: TGLPlane;
    GLXYZGrid1: TGLXYZGrid;
    Map: TGLDummyCube;
    GLCube1: TGLCube;
    GLCube2: TGLCube;
    GLCube3: TGLCube;
    GLCube4: TGLCube;
    CollisionManager1: TGLCollisionManager;
    GLCamera2: TGLCamera;
    GLMemoryViewer1: TGLMemoryViewer;
    GLDummyCube2: TGLDummyCube;
    GLSphere1: TGLSphere;
    GLCube5: TGLCube;
    Panel1: TPanel;
    PaintBox1: TPaintBox;
    Font: TGLWindowsBitmapFont;
    Help: TGLHUDText;
    Timer1: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure GLCadencer1Progress(Sender: TObject;
      const deltaTime, newTime: Double);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure SetLegAnimation(an: String);
    procedure SetTorsoAnimation(an: String);
    procedure MakeRotation;
    procedure MakeAnimations;
    procedure GLSceneViewer1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure GLSceneViewer1MouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure GLActor1EndFrameReached(Sender: TObject);
    procedure CollisionManager1Collision(Sender: TObject;
      object1, object2: TGLBaseSceneObject);
    procedure GLMemoryViewer1BeforeRender(Sender: TObject);
    procedure GLMemoryViewer1AfterRender(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
     
  public
     
    Act: TGLMD3Actor;
    Beh: TGLBCollision;
    mx, my: Integer;
    Angel: Double;

    Keys: Array [0 .. 255] of boolean;

    HeadRot, HeadRotN: Double;
    Jump, Run, Crouch, Back, Walk, Fire, Rotate, Attack, CollLegs: boolean;
  end;

var
  Form1: TForm1;

const
  HELP_TEXT = 'Cursor keys - walk'#13#10 + 'Shift - run'#13#10 +
    'Enter - jump'#13#10 + 'Space - fire'#13#10 + 'Ctrl - crouch';

implementation

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
var
  i: Longint;

begin
  for i := 0 to 255 do
    Keys[i] := false;
  Act := TGLMD3Actor.Create(Actor);
  Act.CharacterModel := 'doom';
  Act.Animation := 'stand';
  Act.WeaponModel := 'mp5k';
  Act.Legs.OnEndFrameReached := GLActor1EndFrameReached;

  Beh := TGLBCollision.Create(Act.Legs.Behaviours);
  Beh.BoundingMode := cbmSphere;
  Beh.Manager := CollisionManager1;

  Act.Legs.Behaviours.Add(Beh);
  Act.Legs.Name := 'Repa';

  Crouch := false;
  Back := false;
  Walk := false;
  Fire := false;
  Angel := 0;
  Jump := false;
  HeadRotN := 0;
  HeadRot := 0;

  GLCamera1.TargetObject := Act.Head;
  GLCamera2.MoveTo(Act.Head);
  GLDummyCube2.Position.Z := GLCamera2.Position.Z;

  Help.Text := HELP_TEXT;
end;

procedure TForm1.SetLegAnimation(an: String);
begin
  if AnsiUpperCase(Act.Legs.CurrentAnimation) <> AnsiUpperCase(an) then
    Act.Legs.SwitchToAnimation(an);
end;

procedure TForm1.SetTorsoAnimation(an: String);
begin
  if AnsiUpperCase(Act.Torso.CurrentAnimation) <> AnsiUpperCase(an) then
    Act.Torso.SwitchToAnimation(an);
end;

procedure TForm1.MakeAnimations;
begin
  Act.Legs.AnimationMode := aamType[Back];
  Act.Torso.AnimationMode := aamType[Back];
  Act.Head.AnimationMode := aamType[Back];
  if Jump then
    SetLegAnimation('LEGS_JUMP')
  else if Walk then
    if Crouch then
      SetLegAnimation('LEGS_WALKCR')
    else if Run then
      SetLegAnimation('LEGS_RUN')
    else
      SetLegAnimation('LEGS_WALK')
  else if Crouch then
    SetLegAnimation('LEGS_IDLECR')
  else if Rotate then
    SetLegAnimation('LEGS_TURN') // LEGS_TURN
  else
    SetLegAnimation('LEGS_IDLE');

  if Attack then
    SetTorsoAnimation('TORSO_ATTACK')
  else
    SetTorsoAnimation('TORSO_STAND');
end;

procedure TForm1.MakeRotation;
begin
  Act.Legs.RollAngle := Angel;
  Act.Head.Roll(HeadRot);
end;

procedure TForm1.GLCadencer1Progress(Sender: TObject;
  const deltaTime, newTime: Double);
var
  px, py, dx, dy, X, Y: Real;
  bmp: TBitmap;
begin
  dx := 0;
  dy := 0;

  px := Act.Legs.Position.X;
  py := Act.Legs.Position.Y;

  Walk := false;
  Back := false;
  Rotate := false;
  Run := false;
  CollLegs := false;

  if Keys[vk_up] then
  begin
    Walk := true;
    Back := false;
  end;

  if Keys[vk_down] then
  begin
    Walk := true;
    Back := true;
  end;

  if Keys[vk_left] then
  begin
    Angel := Angel + 120 * deltaTime;;
    Rotate := true;
  end;

  if Keys[vk_right] then
  begin
    Angel := Angel - 120 * deltaTime;;
    Rotate := true;
  end;

  Attack := Keys[vk_space];

  if Keys[65] then
  begin
    Rotate := true;
    dy := 5;
  end;

  if Keys[68] then
  begin
    Rotate := true;
    dy := -5;
  end;

  Crouch := Keys[VK_CONTROL];

  if Walk then
    if Back then
      dx := -5
    else
      dx := 5;

  if Keys[vk_shift] then
  begin
    dx := dx * 2;
    dy := dy * 2;
    Run := true;
  end;

  if Keys[VK_RETURN] then
  begin
    Jump := true;
  end;

  if HeadRotN = 0 then
    HeadRotN := 1;
  HeadRot := HeadRot + 30 * HeadRotN * deltaTime;

  if Abs(HeadRot) > 20 then
  begin
    HeadRot := 20 * HeadRotN;
    HeadRotN := -HeadRotN;
  end;
  Act.HeadRot := HeadRot;

  MakeRotation;
  MakeAnimations;

  if (dx <> 0) or (dy <> 0) then
  begin
    dx := 10 * dx * deltaTime;
    dy := 10 * dy * deltaTime;

    X := dx * cos(DegToRadian(Angel)) + dy * cos(DegToRadian(Angel + 90));
    Y := dx * sin(DegToRadian(Angel)) + dy * sin(DegToRadian(Angel + 90));;

    Act.Legs.Position.X := Act.Legs.Position.X + X;
    Act.Legs.Position.Y := Act.Legs.Position.Y + Y;
    CollisionManager1.CheckCollisions;

    if CollLegs then
    begin
      Act.Legs.Position.X := px;
      Act.Legs.Position.Y := py;
    end;
  end;

  Act.Progress(deltaTime);

  GLMemoryViewer1.render;
  bmp := GLMemoryViewer1.Buffer.CreateSnapShotBitmap;
  PaintBox1.Canvas.Draw(0, 0, bmp);
  bmp.Free;
end;

procedure TForm1.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  Keys[Key] := true;
end;

procedure TForm1.FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  Keys[Key] := false;
end;

procedure TForm1.GLSceneViewer1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  mx := X;
  my := Y;
end;

procedure TForm1.GLSceneViewer1MouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
begin
  if ssLeft in Shift then
    GLCamera1.MoveAroundTarget(my - Y, mx - X);

  mx := X;
  my := Y;
end;

procedure TForm1.GLActor1EndFrameReached(Sender: TObject);
begin
  Jump := false;
end;

procedure TForm1.CollisionManager1Collision(Sender: TObject;
  object1, object2: TGLBaseSceneObject);
begin
  CollLegs := true;
end;

procedure TForm1.GLMemoryViewer1BeforeRender(Sender: TObject);
begin
  GLSphere1.Material.FrontProperties.Diffuse.AsWinColor := clGreen;
  Actor.Visible := false;
  Help.Visible := false;
end;

procedure TForm1.GLMemoryViewer1AfterRender(Sender: TObject);
begin
  GLSphere1.Material.FrontProperties.Diffuse.AsWinColor := clRed;
  Actor.Visible := true;
  Help.Visible := true;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  Caption := Format('%.1f FPS', [GLSceneViewer1.FramesPerSecond]);
  GLSceneViewer1.ResetPerformanceMonitor;
end;

end.
