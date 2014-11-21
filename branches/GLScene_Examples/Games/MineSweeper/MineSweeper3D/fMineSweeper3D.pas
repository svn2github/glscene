unit fMineSweeper3D;

interface

uses
  Windows, Messages, SysUtils, Types, Variants, Math, Jpeg, Classes, Actions, Graphics,
  Controls, Forms, ActnList, Menus, StdCtrls, ExtCtrls, StrUtils, Dialogs,
  ShellAPI, IniFiles,
  //GLS
  GLScene, GLObjects, GLGeomObjects, GLWin32Viewer, GLNavigator, GLCadencer, GLTexture,
  GLColor, GLVectorGeometry, GLSpaceText, uMinesClasses, mmSystem, GLFireFX,
  GLContext, GLSound, MPlayer, GLCrossPlatform, GLMaterial, GLBaseClasses,
  GLCoordinates;

type
  T3DSquare = class;

  TfrmMineSweeper3D = class(TForm)
    GLScene1: TGLScene;
    GLSceneViewer_Main: TGLSceneViewer;
    GLCadencer_Main: TGLCadencer;
    GLMaterialLibrary_Digits: TGLMaterialLibrary;
    Image1: TImage;
    Timer1: TTimer;
    Label_GameState: TLabel;
    Label_MinesLeft: TLabel;
    Label_GameTime: TLabel;
    Timer_GameTimer: TTimer;
    ActionList1: TActionList;
    Action_Restart: TAction;
    MainMenu1: TMainMenu;
    Game1: TMenuItem;
    Action_Close: TAction;
    Exit1: TMenuItem;
    N1: TMenuItem;
    Restart1: TMenuItem;
    N2: TMenuItem;
    Action_Beginner: TAction;
    Action_Intermediate: TAction;
    Action_Advanced: TAction;
    Beginner1: TMenuItem;
    Advanced1: TMenuItem;
    Advanced2: TMenuItem;
    GLFireFXManager1: TGLFireFXManager;
    Opt: TMenuItem;
    Action_AntiAliasing: TAction;
    Antialiasing1: TMenuItem;
    Action_DebugEasy: TAction;
    Debug1: TMenuItem;
    Help1: TMenuItem;
    Action_About: TAction;
    About1: TMenuItem;
    N3: TMenuItem;
    Action_Help: TAction;
    Help2: TMenuItem;
    Action_Sound: TAction;
    N4: TMenuItem;
    Sound1: TMenuItem;
    Action_SafeFirstReveal: TAction;
    Firstturnissafe1: TMenuItem;
    N5: TMenuItem;
    Action_DebugHard: TAction;
    Debughard1: TMenuItem;
    N6: TMenuItem;
    Label_FrameRate: TLabel;
    Alwaysinvalidate1: TMenuItem;
    Label_IntersectTime: TLabel;
    Plane_PlayingField: TGLPlane;
    DummyCube_PlayingBox: TGLDummyCube;
    GLCamera_MainCamera: TGLCamera;
    Lines1: TGLLines;
    GLLightSource1: TGLLightSource;
    procedure GLSceneViewer_MainMouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure FormCreate(Sender: TObject);
    procedure GLSceneViewer_MainMouseUp(Sender: TObject; Button: TGLMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Timer1Timer(Sender: TObject);
    procedure Timer_GameTimerTimer(Sender: TObject);
    procedure GLCadencerProgress_SHAKING(Sender: TObject; const deltaTime,
      newTime: Double);
    procedure Action_RestartExecute(Sender: TObject);
    procedure Action_CloseExecute(Sender: TObject);
    procedure Action_BeginnerExecute(Sender: TObject);
    procedure Action_IntermediateExecute(Sender: TObject);
    procedure Action_AdvancedExecute(Sender: TObject);
    procedure Label_GameStateClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure Action_AntiAliasingExecute(Sender: TObject);
    procedure Action_DebugEasyExecute(Sender: TObject);
    procedure Action_AboutExecute(Sender: TObject);
    procedure Action_HelpExecute(Sender: TObject);
    procedure Action_SoundExecute(Sender: TObject);
    procedure Action_SafeFirstRevealExecute(Sender: TObject);
    procedure Action_DebugHardExecute(Sender: TObject);
    procedure InvalidateProgress(Sender: TObject; const deltaTime,
      newTime: Double);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure Action_AlwaysInvalidateExecute(Sender: TObject);
  private
    { Private declarations }
    FPrecisionTimer : Int64;
    FTotalTimerTime : double;
    FTimerHits : Int64;
    FSpecialCount : integer;

    FRecordHitLine : boolean;
  public
    { Public declarations }
	  oldPick : T3DSquare;
    MouseX, MouseY : integer;
    ArenaList : TList;
    Mines : TMines;
    SquaresUpdated: integer;

    StopShake : single;
    ShakeDX, ShakeDY : single;
    ShakeTurn, ShakeTwist : single;

    TotalVictoryRotation : single;
    ImplicitProgress : boolean;
    RenderCount : integer;
    ProgressCount : integer;

    procedure ShakeItUp;
    function GetPicked3DSquare(x,y : integer) : T3DSquare;
    procedure CreateArena;
    procedure CreateFont;
    procedure Render(OnlyChanged : boolean);
    procedure MyPlaySound(FileName : string);

    procedure EnforceOptions;
    procedure SaveOptions;
    procedure ReadOptions;

    // EVENTS TO SHOW PROGRESS
    procedure RecedeProgress(Sender: TObject; const deltaTime,
      newTime: Double);

    procedure TwistProgress(Sender: TObject; const deltaTime,
      newTime: Double);

    procedure StartMyTimer;
    procedure StopMyTimer(s : string);

    // EVENTS FOR THE MINES CLASS
    procedure GameStarted(Sender: TObject);
    procedure GameWon(Sender: TObject);
    procedure GameLost(Sender: TObject);
    procedure GameReset(Sender: TObject);
    procedure RevealedFailed(Sender: TObject);
  end;

  T3DSquare = class
    Parts : TList;

    Pawn : TGLCustomSceneObject;
    Flag : TGLCustomSceneObject;
    Mine : TGLCustomSceneObject;
    InfoField : TGLCustomSceneObject;

    FallSpeed : single;
    X,Y : integer;
    Revealed : boolean;
    Square : TSquare;
    TimeDelay : cardinal;

    TurnedBefore : boolean;

    procedure Reveal;
    procedure Flagged;

    procedure Selected;
    procedure HoverOver;
    procedure ResetAppearance;

    constructor Create(Target : TGLCustomSceneObject; x,y : integer; px, py : single);
    destructor Destroy;override;
  end;

var
  frmMineSweeper3D: TfrmMineSweeper3D;

implementation

uses fAboutCambrianLabs;

{$R *.dfm}

procedure TfrmMineSweeper3D.FormCreate(Sender: TObject);
begin
  Randomize;

  Mines := TMines.Create;
  Mines.UseWarnings := false;
  Mines.OnGameWon := GameWon;
  Mines.OnGameLost := GameLost;
  Mines.OnGameStarted := GameStarted;
  Mines.OnGameReset := GameReset;
  Mines.OnRevealedFailed := RevealedFailed;
  ImplicitProgress := false;

  ArenaList := TList.Create;

  Plane_PlayingField.Visible := false;
  CreateFont;
  Action_Beginner.Execute;

  FSpecialCount := 0;
  FTotalTimerTime := 0;
  FTimerHits := 0;

  ReadOptions;
end;

procedure TfrmMineSweeper3D.CreateArena;
var
  x,y,i : integer;
  px,py : single;
  My3DSquare : T3DSquare;
begin
  Mines.BuildRandomSameSize;

  for i := 0 to ArenaList.Count - 1 do
    T3DSquare(ArenaList[i]).Free;

  ArenaList.Clear;

  Plane_PlayingField.Width := Mines.CountX;
  Plane_PlayingField.Height := Mines.CountY;

  for x := 0 to Mines.CountX-1 do
    for y := 0 to Mines.CountY-1 do
    begin
      px := x - Plane_PlayingField.Width / 2;
      py := y - Plane_PlayingField.Height / 2;

      My3DSquare := T3DSquare.Create(DummyCube_PlayingBox, x,y, px, py);
      My3DSquare.Square := Mines.GetSquareForXY(x,y);
      ArenaList.Add(My3DSquare);
      My3DSquare.Square.Data := My3DSquare;
    end;
end;

function TfrmMineSweeper3D.GetPicked3DSquare(x, y: integer): T3DSquare;
var
  tempPick : TGLCustomSceneObject;
  rayStart, rayVector, iPoint, iNormal : TVector;

  function Hits3DSquare(My3DSquare : T3DSquare) : boolean;
  begin
    // The RayCastIntersect method is very bad, it assumes that all objects are
    // spheres. It should be overriden for basic objects, like planes and cubes?
    result :=
      (My3DSquare.InfoField.Visible and My3DSquare.InfoField.RayCastIntersect(rayStart, rayVector, @iPoint, @iNormal)) or
      (My3DSquare.Pawn.Visible and My3DSquare.Pawn.RayCastIntersect(rayStart, rayVector, @iPoint, @iNormal)) or
      (My3DSquare.Flag.Visible and My3DSquare.Flag.RayCastIntersect(rayStart, rayVector, @iPoint, @iNormal));
  end;

  function FindPick : T3DSquare;
  var
    i : integer;
  begin
    temppick:=TGLCustomSceneObject(GLScene1.RayCastIntersect(rayStart, rayVector, @iPoint, @iNormal));

    if tempPick=nil then
    begin
      inc(FSpecialCount);
      temppick:=TGLCustomSceneObject(GLSceneViewer_Main.Buffer.GetPickedObject(x, y));
    end;

    if tempPick<>nil then
    begin
      result := T3DSquare(tempPick.Tag);
    end else
      result := nil;
  end;
begin
  SetVector(rayStart, GLCamera_MainCamera.AbsolutePosition);
  SetVector(rayVector, GLSceneViewer_Main.Buffer.ScreenToVector(AffineVectorMake(x, GLSceneViewer_Main.Height-y, 0)));
  NormalizeVector(rayVector);

  if FRecordHitLine then
  begin
    Lines1.Nodes.Clear;
    Lines1.AddNode(rayStart);
    Lines1.AddNode(VectorAdd(rayStart, VectorScale(rayVector, 20)));
    FRecordHitLine := false;
  end;

  // Can we use the old pick?
  if Assigned(oldPick) and Hits3DSquare(oldPick) then
    result := oldPick
  else
    result := FindPick;
end;

var
  oldShift : TShiftState;
procedure TfrmMineSweeper3D.GLSceneViewer_MainMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var
  dx, dy : Integer;
	My3DSquare : T3DSquare;
begin
  StartMyTimer;

  if ssCtrl	in Shift then
  begin
    //How much the mouse has moved since last time
    dx:=MouseX-x; dy:=MouseY-y;
    GLCamera_MainCamera.MoveAroundTarget(dy, dx);
  end else
  begin
    if not (Mines.GameState in [gsWon, gsLost]) then
    begin
      My3DSquare := GetPicked3DSquare(x,y);

      if Assigned(My3DSquare) then
      begin
        if ((My3DSquare <> oldPick) or (oldShift <> Shift)) then
        begin
          Mines.TurnOfSignals;

          if ssShift in Shift then
            My3DSquare.Square.SignalWide
          else
            My3DSquare.Square.Signal;

          Render(true);
        end;
      end else
      begin
        Mines.TurnOfSignals;
        Render(true);
      end;

      oldPick := My3DSquare;
      oldShift := Shift;
    end;
  end;

  //Reset the mouse position
  MouseX:=X;
  MouseY:=Y;

  if ImplicitProgress then
  begin
    GLCadencer_Main.Progress;
    inc(ProgressCount);
  end;

  StopMyTimer('Mouse move');
end;

procedure TfrmMineSweeper3D.GLSceneViewer_MainMouseUp(Sender: TObject;
  Button: TGLMouseButton; Shift: TShiftState; X, Y: Integer);
var
  My3DSquare : T3DSquare;
  ClickedName : string;
  ActionDesc : string;
begin
  if Mines.GameState in [gsWon, gsLost] then
    exit;

  //FRecordHitLine := true;

  My3DSquare := GetPicked3DSquare(x,y);

	if Assigned(My3DSquare) then
  begin
    if Button = mbRight then
      My3DSquare.Square.ClickedFlag
    else
    begin
      if ssShift in Shift then
        My3DSquare.Square.ClickedRevealWide
      else
        My3DSquare.Square.ClickedReveal(false);
    end;

    Render(true);
  end;
end;

procedure TfrmMineSweeper3D.TwistProgress(Sender: TObject; const deltaTime,
  newTime: Double);
var
  obj : TGLCustomSceneObject;
  dR : single;
begin
  obj := TGLCustomSceneObject(Sender);

  dR := sin(newTime*2)/4;
  TotalVictoryRotation := TotalVictoryRotation + dR;
  obj.Roll(dR);

  GLSceneViewer_Main.Invalidate;
end;

procedure TfrmMineSweeper3D.RecedeProgress(Sender: TObject; const deltaTime,
  newTime: Double);
var
  obj : TGLCustomSceneObject;
  My3DSquare : T3DSquare;
  procedure Finished;
  begin
    obj.Visible := false;
    obj.OnProgress := nil;
  end;
begin
  obj := TGLCustomSceneObject(Sender);
  My3DSquare := T3DSquare(obj.Tag);

  if TimeGetTime<My3DSquare.TimeDelay then
    exit;

  if Obj.Position.Z<-0.8 then
  begin
    Finished;
  end else
  if Obj.Position.Z>10 then
  begin
    Finished;
  end else
  begin
    if not My3DSquare.InfoField.Visible then
      My3DSquare.InfoField.Visible := true;

    My3DSquare.TurnedBefore := true;

    My3DSquare.FallSpeed := My3DSquare.FallSpeed - 3 * deltaTime;

    obj.Position.Z := obj.Position.Z + My3DSquare.FallSpeed*deltaTime;
    obj.Turn(deltaTime*180);
    obj.Roll(deltaTime*180*0.5);
  end;
end;

{ T3DSquare }

constructor T3DSquare.Create(Target : TGLCustomSceneObject; x,y : integer; px, py : single);
  const
    cubeMargin = 0.05;
    cubeDepth = 0.05;
    cubeZ = 0.1;

  function CreatePawn(x, y: integer) : TGLCustomSceneObject;
  var
    Cube : TGLCube;
    mx, my : single;
  begin
    cube:=TGLCube(Target.AddNewChild(TGLCube));
    with Cube do
    begin
      mx := 0.5;
      my := 0.5;

      Position.X := px + mx;
      Position.Y := py + my;
      Position.Z := cubeZ;

      CubeWidth:= 1-cubeMargin;
      CubeHeight:= 1-cubeMargin;
      CubeDepth:= 0.1;//1-cubeMargin;

      with Material.FrontProperties do
        Diffuse.Color := clrGrayText;

      Name := Format('Pawn_%d_%d',[x,y]);
    end;

    result := Cube;
  end;

  function CreateMine(x,y : integer) : TGLCustomSceneObject;
  var
    Sphere : TGLSphere;
  begin
    Sphere := TGLSphere(Target.AddNewChild(TGLSphere));

    with Sphere do
    begin
      Position.X := px + 0.5;
      Position.Y := py + 0.5;
      Position.Z := 0.35;
      Stacks := 7;
      Slices := 7;
      Radius := 0.35;
      Material.FrontProperties.Emission.Color := clrRed;

      Visible := false;
    end;

    result := Sphere;
  end;


  function CreateInfoField(x, y: integer) : TGLCustomSceneObject;
  var
    Plane : TGLPlane;
    mx, my : Single;
  begin
    Plane:=TGLPlane(Target.AddNewChild(TGLPlane));
    with Plane do
    begin
      mx := 0.5;
      my := 0.5;

      Position.X := px + mx;
      Position.Y := py + my;
      Position.Z := cubeZ-cubeDepth;

      Name := Format('InfoField_%d_%d',[x,y]);
    end;

    result := Plane;
  end;

  const
    coneHeight = 0.6;

  function CreateFlag(x,y : integer) : TGLCustomSceneObject;
  var
    Cone : TGLCone;
    mx, my : single;
  begin
    Cone:=TGLCone(Target.AddNewChild(TGLCone));

    with Cone do
    begin
      mx := 0.5;
      my := 0.5;

      Position.X := px + mx;
      Position.Y := py + my;
      Position.Z := cubeZ+coneHeight/2+0.1;

      Slices := 4;
      Stacks := 1;
      BottomRadius := 0.3;
      Height := coneHeight;
      Roll(90);
      Pitch(-90);

      with Material.FrontProperties do
        Diffuse.Color := clrRed;

      Name := Format('Cone_%d_%d',[x,y]);
    end;

    result := Cone;

    Cone.Visible := false;
  end;

begin
  Parts := TList.Create;
  FallSpeed := 0;

  self.x := x;
  self.y := y;

  Pawn := CreatePawn(x,y);
  Parts.Add(Pawn);
  Pawn.Tag := Integer(self);

  Flag := CreateFlag(x,y);
  Parts.Add(Flag);
  Flag.Tag := Integer(self);

  InfoField := CreateInfoField(x,y);
  Parts.Add(InfoField);
  InfoField.Tag := Integer(self);
  InfoField.Visible := false;

  Mine := CreateMine(x,y);
  //Parts.Add(Mine);
  Mine.Tag := Integer(self);//}
end;

destructor T3DSquare.Destroy;
var
  i : integer;
begin
  for i := 0 to Parts.Count - 1 do
    TObject(Parts[i]).Free;

  Mine.Free;
  Parts.Free;

  inherited;
end;

procedure T3DSquare.Flagged;
begin
  if Revealed then exit;
  
  Flag.Visible := not Flag.Visible;
end;

procedure T3DSquare.HoverOver;
var
  i : integer;
begin
  if not Revealed then
    for i := 0 to Parts.Count - 1 do
      TGLCustomSceneObject(Parts[i]).Material.FrontProperties.Emission.Color:=clrYellowGreen;
end;

procedure T3DSquare.ResetAppearance;
var
  i : integer;
begin
  for i := 0 to Parts.Count - 1 do
    TGLCustomSceneObject(Parts[i]).Material.FrontProperties.Emission.Color:=clrBlack;
end;

procedure T3DSquare.Reveal;
begin
  if Revealed then exit;

  if not Flag.Visible then
  begin
    TimeDelay := timeGetTime + trunc(Square.RevealRecurseLevel*1000*0.1);
    Pawn.OnProgress := frmMineSweeper3D.RecedeProgress;
  end;

  Revealed := true;

  ResetAppearance;

  InfoField.Material.MaterialLibrary := frmMineSweeper3D.GLMaterialLibrary_Digits;

  if Square.Mine then
    InfoField.Material.LibMaterialName := 'Text_M'
  else
    InfoField.Material.LibMaterialName := 'Digit_'+inttostr(Square.NeighbouringMines);
end;

procedure T3DSquare.Selected;
var
  i : integer;
begin
{  for i := 0 to Parts.Count - 1 do
    TGLCustomSceneObject(Parts[i]).Material.FrontProperties.Emission.Color:=clrYellow;//}
end;

procedure TfrmMineSweeper3D.Timer1Timer(Sender: TObject);
begin
   Label_FrameRate.Caption:=Format('%.1f fps', [GLSceneViewer_Main.FramesPerSecond]);
   GLSceneViewer_Main.ResetPerformanceMonitor;
end;

procedure TfrmMineSweeper3D.CreateFont;
var
  h,w,i : integer;
  s : string;
  procedure ResetCanvas;
  begin
    with Image1.Canvas do
    begin
      Brush.Color := clSilver;
      FillRect(Image1.Canvas.ClipRect);
    end;
  end;
  procedure CenterText(s : string);
  begin
    with Image1.Canvas do
      TextOut((w-TextWidth(s)) div 2,1,s);
  end;
begin
   // prepare images to merge in the multitexture
  with GLMaterialLibrary_Digits do
  begin
    Image1.Canvas.Font.Name := 'System';
    Image1.Canvas.Font.Style := [fsBold];

    w := image1.Width;
    h := image1.Height;

    ResetCanvas;
//    Materials[0].Material.Texture.Image.Assign(Image1.Picture);

    for i := 0 to 8 do
    begin
      ResetCanvas;
      //Image1.Picture.LoadFromFile('..\..\media\ashwood.jpg');

      with Image1.Canvas do
      begin
        if i = 0 then
          s := ''
        else
          s := inttostr(i);

        if i=1 then Font.Color := clBlue;
        if i=2 then Font.Color := clGreen;
        if i=3 then Font.Color := clRed;
        if i=4 then Font.Color := clNavy;
        if i=5 then Font.Color := clTeal;
        if i=6 then Font.Color := clTeal;
        if i=7 then Font.Color := clTeal;
        if i=8 then Font.Color := clTeal;

        //TextOut((w-TextWidth(s)) div 2,1,s);
        CenterText(s);
      end;

      Materials[i].Material.Texture.Image.Assign(Image1.Picture);
    end;

    ResetCanvas;
    CenterText('M');
    Materials.GetLibMaterialByName('Text_M').Material.Texture.Image.Assign(Image1.Picture);
  end;
end;

procedure TfrmMineSweeper3D.Render(OnlyChanged : boolean);
  procedure StandardSetting(My3DSquare : T3DSquare);
  begin
    My3DSquare.ResetAppearance;
    My3DSquare.Flag.Visible := false;
  end;
var
  i : integer;
  Square : TSquare;
  My3DSquare : T3DSquare;
  GLBFireFX : TGLBFireFX;
begin
  SquaresUpdated := 0;
  for i := 0 to Mines.SquareList.Count-1 do
  begin
    Square := Mines.SquareList[i];

    if OnlyChanged and not Square.Changed then
      continue;

    inc(SquaresUpdated);

    My3DSquare := T3DSquare(Square.Data);

    case Square.Status of
      ssHidden :
      begin
        StandardSetting(My3DSquare);
      end;

      ssFlag :
      begin
        StandardSetting(My3DSquare);
        My3DSquare.Flag.Visible := true;
      end;

      ssQuestion :
      begin
        StandardSetting(My3DSquare);
        //sPanel.Caption := '?';
      end;

      ssRevealed :
      begin
        My3DSquare.Reveal;
      end;

      ssExploded :
      begin
        My3DSquare.Mine.Visible := true;
        GLBFireFX := TGLBFireFX.Create(My3DSquare.Mine.Effects);
        GLBFireFX.Manager := GLFireFXManager1;

        //My3DSquare.Mine.Effects.Add(GLBFireFX);
      end;

      ssErrorFlag :
      begin
        My3DSquare.Pawn.Material.MaterialLibrary := GLMaterialLibrary_Digits;
        My3DSquare.Pawn.Material.LibMaterialName := 'NotAMine';
        {sPanel.Font.Color := clRed;
        sPanel.Caption := 'f';//}
      end;
    end;

    if Square.Signalled then
      My3DSquare.HoverOver;

{    if Square.Signalled then
      sPanel.BevelOuter := bvNone;//}

    Square.Changed := false;
  end;

  // Force a progress to fire (avoids a lot of jerko-vision)
  if ImplicitProgress and (SquaresUpdated>0) then
  begin
    GLCadencer_Main.Progress;
    inc(ProgressCount);
  end;

  inc(RenderCount);
  //Label_GameState.Caption := IntToStr(RenderCount);

end;

procedure TfrmMineSweeper3D.GameLost(Sender: TObject);
begin
  Label_GameState.Caption := 'You failed!';
  Label_GameState.Font.Color := clRed;

  Render(true);
  ShakeItUp;
  MyPlaySound('Sounds\YouLose.wav');
  oldPick := nil;
end;

procedure TfrmMineSweeper3D.GameReset(Sender: TObject);
begin
  Label_GameState.Caption := 'Waiting';
  Label_GameState.Font.Color := clBlack;

  DummyCube_PlayingBox.OnProgress := nil;
  DummyCube_PlayingBox.Roll(-TotalVictoryRotation);
  TotalVictoryRotation := 0;
  RenderCount := 0;
  ProgressCount := 0;
  oldPick := nil;

{  Renderer.Prepare;
  Renderer.Render(false);
  ResizeToFitPanel;//}
end;

procedure TfrmMineSweeper3D.GameStarted(Sender: TObject);
begin
  MyPlaySound('Sounds\GameStart.wav');
  Label_GameState.Caption := 'Running!';
  Label_GameState.Font.Color := clBlack;
end;

procedure TfrmMineSweeper3D.GameWon(Sender: TObject);
begin
  Label_GameState.Caption := 'You WON!';
  Label_GameState.Font.Color := clGreen;

  TotalVictoryRotation := 0;
  DummyCube_PlayingBox.OnProgress := TwistProgress;
  oldPick := nil;

  MyPlaySound('Sounds\YouWin.wav');
end;

procedure TfrmMineSweeper3D.Timer_GameTimerTimer(Sender: TObject);
var
  s : string;
  MinesLeft : integer;
begin
  s := Format('000%0.0f',[Mines.GameTimePassed]);
  Label_GameTime.Caption := RightStr(s,3);

  MinesLeft := Mines.MineCount - Mines.FlagsPlaced;
  if MinesLeft>= 0 then                                                              
  begin
    s := Format('000%d',[MinesLeft]);
    Label_MinesLeft.Caption := RightStr(s,3);
  end else
  begin
    s := Format('00%d',[abs(MinesLeft)]);
    Label_MinesLeft.Caption := '-'+RightStr(s,2);
  end;

  if MinesLeft= 0 then
    Label_MinesLeft.Font.Color := clGreen
  else if MinesLeft < 0 then
    Label_MinesLeft.Font.Color := clRed
  else
    Label_MinesLeft.Font.Color := clBlack;
end;

procedure TfrmMineSweeper3D.GLCadencerProgress_SHAKING(Sender: TObject;
  const deltaTime, newTime: Double);
begin
  // Some shaking!
  DummyCube_PlayingBox.Position.X := DummyCube_PlayingBox.Position.X - ShakeDX;
  DummyCube_PlayingBox.Position.X := DummyCube_PlayingBox.Position.Y - ShakeDY;

  DummyCube_PlayingBox.Roll(-ShakeTwist);
  DummyCube_PlayingBox.Turn(-ShakeTurn);

  if timeGetTime>=StopShake then
  begin
    GLCadencer_Main.OnProgress := InvalidateProgress;
  end else
  begin
    ShakeDX := random*0.1;
    ShakeDY := random*0.1;
    ShakeTurn := random*15;
    ShakeTwist := random*15;

    DummyCube_PlayingBox.Turn(ShakeTurn);
    DummyCube_PlayingBox.Roll(ShakeTwist);

    DummyCube_PlayingBox.Position.X := DummyCube_PlayingBox.Position.X + ShakeDX;
    DummyCube_PlayingBox.Position.X := DummyCube_PlayingBox.Position.Y + ShakeDY;
  end;
end;

procedure TfrmMineSweeper3D.ShakeItUp;
begin
  // Initiate the shaking!
  ShakeDX := 0;
  ShakeDY := 0;
  ShakeTurn := 0;
  ShakeTwist := 0;

  StopShake := timeGetTime+1500;
  GLCadencer_Main.OnProgress := GLCadencerProgress_SHAKING;
end;

procedure TfrmMineSweeper3D.Action_RestartExecute(Sender: TObject);
begin
  CreateArena;
  StopShake := 0;
end;

procedure TfrmMineSweeper3D.Action_CloseExecute(Sender: TObject);
begin
  Close;
end;

procedure TfrmMineSweeper3D.Action_DebugEasyExecute(Sender: TObject);
begin
  Mines.BuildRandomMap(2,9,9);
  GLCamera_MainCamera.Position.Z := 9;
  CreateArena;
end;

procedure TfrmMineSweeper3D.Action_DebugHardExecute(Sender: TObject);
begin
  Mines.BuildRandomMap(60,9,9);
  GLCamera_MainCamera.Position.Z := 9;
  CreateArena;
end;

procedure TfrmMineSweeper3D.Action_BeginnerExecute(Sender: TObject);
begin
  Mines.BuildRandomMapSpecified(msBeginner);
  GLCamera_MainCamera.Position.Z := 9;
  CreateArena;
end;

procedure TfrmMineSweeper3D.Action_IntermediateExecute(Sender: TObject);
begin
  Mines.BuildRandomMapSpecified(msIntermediate);
  GLCamera_MainCamera.Position.Z := 16;
  CreateArena;
end;

procedure TfrmMineSweeper3D.Action_AdvancedExecute(Sender: TObject);
begin
  Mines.BuildRandomMapSpecified(msAdvanced);
  CreateArena;
end;

procedure TfrmMineSweeper3D.Label_GameStateClick(Sender: TObject);
begin
  CreateArena;
end;

procedure TfrmMineSweeper3D.FormResize(Sender: TObject);
begin
   // This lines take cares of auto-zooming.
   // magic numbers explanation :
   //  333 is a form width where things looks good when focal length is 50,
   //  ie. when form width is 333, uses 50 as focal length,
   //      when form is 666, uses 100, etc...
   GLCamera_MainCamera.FocalLength:=Width*40/446;
end;

procedure TfrmMineSweeper3D.FormMouseWheel(Sender: TObject;
  Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint;
  var Handled: Boolean);
begin
	GLCamera_MainCamera.AdjustDistanceToTarget(Power(1.1, WheelDelta/120));
end;

procedure TfrmMineSweeper3D.Action_AboutExecute(Sender: TObject);
begin
  //ShowMessage('No about box yet!');
  frmAboutCambrianLabs.ShowModal;
end;

procedure TfrmMineSweeper3D.Action_HelpExecute(Sender: TObject);
var
  s : string;
begin
  s := GetCurrentDir+'\help\MineSweeper3d.html';

  if not FileExists(s) then
    MessageDlg('Can''t find helpfile!', mtError, [mbOK], 0)
  else
    ShellExecute(Handle, 'open',
      pchar(s),nil,nil, SW_SHOWNORMAL);
end;

procedure TfrmMineSweeper3D.RevealedFailed(Sender: TObject);
begin
  MyPlaySound('Sounds\CantTurn.wav');
end;

procedure TfrmMineSweeper3D.MyPlaySound(FileName: string);
begin
  if Action_Sound.Checked then
    PlaySound(PChar(FileName),0,SND_ASYNC);
end;

procedure TfrmMineSweeper3D.Action_SoundExecute(Sender: TObject);
begin
  EnforceOptions;
  SaveOptions;
end;

procedure TfrmMineSweeper3D.Action_SafeFirstRevealExecute(Sender: TObject);
begin
  EnforceOptions;
  SaveOptions;
end;

procedure TfrmMineSweeper3D.Action_AntiAliasingExecute(Sender: TObject);
begin
  EnforceOptions;
  SaveOptions;
end;

procedure TfrmMineSweeper3D.EnforceOptions;
begin
  Mines.SafeFirstReveal := Action_SafeFirstReveal.Checked;

  if Action_AntiAliasing.Checked then
    GLSceneViewer_Main.Buffer.AntiAliasing := aa2x
  else
    GLSceneViewer_Main.Buffer.AntiAliasing := aaNone;
end;

procedure TfrmMineSweeper3D.SaveOptions;
var
  IniFile : TIniFile;
begin
  IniFile := TIniFile.Create(GetCurrentDir+'\MineSweeper3D.ini');
  IniFile.WriteBool('Options','AntiAliasing',Action_AntiAliasing.Checked);
  IniFile.WriteBool('Options','Sound',Action_Sound.Checked);
  IniFile.WriteBool('Options','FirstTurnIsSafe',Action_SafeFirstReveal.Checked);

  IniFile.Free;
end;

procedure TfrmMineSweeper3D.ReadOptions;
var
  IniFile : TIniFile;
begin
  IniFile := TIniFile.Create(GetCurrentDir+'\MineSweeper3D.ini');

  Action_AntiAliasing.Checked := IniFile.ReadBool('Options','AntiAliasing',true);
  Action_Sound.Checked := IniFile.ReadBool('Options','Sound',true);
  Action_SafeFirstReveal.Checked := IniFile.ReadBool('Options','FirstTurnIsSafe',true);
  IniFile.Free;

  EnforceOptions;
end;

procedure TfrmMineSweeper3D.InvalidateProgress(Sender: TObject;
  const deltaTime, newTime: Double);
begin
  GLSceneViewer_Main.Invalidate;
end;

procedure TfrmMineSweeper3D.FormKeyPress(Sender: TObject; var Key: Char);
begin
  if Key=#27 then close;
end;

procedure TfrmMineSweeper3D.Action_AlwaysInvalidateExecute(
  Sender: TObject);
begin
  //
end;

procedure TfrmMineSweeper3D.StartMyTimer;
begin
  FPrecisionTimer := StartPrecisionTimer;
end;

procedure TfrmMineSweeper3D.StopMyTimer(s: string);
var
  ThisTime : double;
begin
  ThisTime := StopPrecisionTimer(FPrecisionTimer);

  FTotalTimerTime := FTotalTimerTime + ThisTime;
  inc(FTimerHits);

  Label_IntersectTime.Caption:=Format('%s: %.3f ms (%.3f ms)', [s, ThisTime*1000, FTotalTimerTime/FTimerHits*1000]);
  if FSpecialCount>0 then
    Label_IntersectTime.Caption:=Label_IntersectTime.Caption+Format('[%d]',[FSpecialCount]);
end;

end.
