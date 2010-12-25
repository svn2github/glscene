{ : Walk and carry demo.<p>

  <b>History : </b><font size=-1><ul>
  <li>25/12/10 - Dev - Created
  </ul>
}

unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, GLNGDManager, GLMaterial, GLCadencer, GLWin32Viewer, GLCrossPlatform,
  BaseClasses, GLScene, GLGeomObjects, GLBitmapFont, GLWindowsFont, GLObjects,
  GLCoordinates, GLFile3DS, GLFileJPEG, GLNavigator, GLKeyboard, vectorgeometry,
  vectortypes, StdCtrls, GLVectorFileObjects, ExtCtrls, GLColor, GLHUDObjects,
  GLTexture, JPEG, math;

type

  TMap = record
    mdl: TGLFreeForm;
    dyn: boolean;
  end;

  TForm1 = class(TForm)
    GLScene1: TGLScene;
    GLSceneViewer1: TGLSceneViewer;
    GLNGDManager1: TGLNGDManager;
    GLCadencer1: TGLCadencer;
    GLMaterialLibrary1: TGLMaterialLibrary;
    GLNGDJointList1: TGLNGDJointList;
    Player_Cam: TGLCamera;
    Cam_Cube: TGLDummyCube;
    Player_Cube: TGLDummyCube;
    GLRenderPoint1: TGLRenderPoint;
    Scene_Objects: TGLDummyCube;
    Game_Menu: TGLDummyCube;
    OpenDialog1: TOpenDialog;
    GLWindowsBitmapFont1: TGLWindowsBitmapFont;
    Player_Capsule: TGLCapsule;
    GLLightSource1: TGLLightSource;
    GLCube2: TGLCube;
    GLNavigator1: TGLNavigator;
    GLUserInterface1: TGLUserInterface;
    GLNavigator2: TGLNavigator;
    TIPickTimer: TTimer;
    GLHUDSprite1: TGLHUDSprite;
    GLSphere1: TGLSphere;
    Memo1: TMemo;
    GLCube3: TGLCube;
    Jump_Timer: TTimer;
    Jump_V_timer: TTimer;
    GLHUDText1: TGLHUDText;
    Timer_OnVelocity: TTimer;
    On_Drop: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure GLCadencer1Progress(Sender: TObject;
      const deltaTime, newTime: Double);
    procedure TIPickTimerTimer(Sender: TObject);
    procedure GLSceneViewer1MouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure Jump_V_timerTimer(Sender: TObject);
    procedure Jump_TimerTimer(Sender: TObject);
    procedure Timer_OnVelocityTimer(Sender: TObject);
    procedure On_DropTimer(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    procedure Menu_Load;
    procedure Game_Load;
  end;

var
  Form1: TForm1;
  maps: array of TMap;
  currentPick: TGLCustomSceneObject;

  FPickedSceneObject: TGLBaseSceneObject;
  NGDDynamicBehav: TGLNGDDynamic;
  point3d, FPaneNormal: TVector;
  M_X, M_Y: Integer;
  Maps_Count: Integer;
  OnAir, OnGround: boolean;
  OnMouse_Click, OnDrop, OnPick: boolean;

implementation

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
  // Switch to English keyboard leyout
  LoadKeyboardLayout('00000409', KLF_ACTIVATE);
  Menu_Load;

  GLHUDText1.Text :=
    'Walk - WASD'+#13+
    'Jump - Space'+#13+
    'Pick/Drop - Left mouse'+#13+
    'Push - Right mouse';
end;

procedure TForm1.GLCadencer1Progress(Sender: TObject;
  const deltaTime, newTime: Double);
var
  I, D: TVector4f;
  F: TVector4f;
  peak_Up, peak_Down: boolean;
  ray, GP3D, result_GP3D: TVector4f;
  j: Integer;
  point2d, GotoPoint3d: TVector;
  key_space: boolean;
  cp: TPoint;
  visible_cursor: boolean;
  ScObj, FScObj: TGLBaseSceneObject;
  targetColor: TColorVector;
  picked: TGLCustomSceneObject;
  mm: TMatrix4f;

begin
  GetCursorPos(cp);
  cp := GLSceneViewer1.ScreenToClient(cp);

  for j := 0 to Scene_Objects.Count - 1 do
  begin
    with Scene_Objects[j] as TGLCustomSceneObject do
    begin
      if Tag = 1 then
        Material.FrontProperties.Emission.Color := clrBlack;
    end;
  end;

  if Assigned(currentPick) then
    if currentPick.Tag = 1 then
    begin
      with currentPick do
      begin
        if currentPick.DistanceTo(Player_Capsule) <= 3.5 then
        begin
          targetColor := clrRed;
        end;
        // Set new color at 66% between current and target color
        with Material.FrontProperties.Emission do
          Color := targetColor; // VectorLerp(targetColor, Color, 0.66)
      end;
    end;

  GLUserInterface1.MouseLook;
  GLUserInterface1.MouseUpdate;

  TIPickTimer.Enabled := True;

  Player_Cube.Position := Player_Capsule.Position;

  if Assigned(NGDDynamicBehav) then
    NGDDynamicBehav.UpVector := False;

  if Assigned(NGDDynamicBehav) and OnMouse_Click then
  begin

    GP3D[0] := Player_Cube.AbsolutePosition[0] +
      Player_Cube.AbsoluteDirection[0] * -2;
    GP3D[1] := Player_Capsule.AbsolutePosition[1] + 1.6 +
      Player_Cam.AbsoluteDirection[1];
    GP3D[2] := Player_Cube.AbsolutePosition[2] +
      Player_Cube.AbsoluteDirection[2] * -2;

    if (roundto((point3d[0]), -3) <> roundto((GP3D[0]), -3)) or
      (roundto((point3d[1]), -3) <> roundto((GP3D[1]), -3)) or
      (roundto((point3d[2]), -3) <> roundto(GP3D[2], -3)) then
    begin
      GP3D[0] := point3d[0] - GP3D[0];
      GP3D[1] := point3d[1] - GP3D[1];
      GP3D[2] := point3d[2] - GP3D[2];
      NormalizeVector(GP3D);

      GotoPoint3d[0] := point3d[0] + GP3D[0] * -6 * deltaTime;
      GotoPoint3d[1] := point3d[1] + GP3D[1] * -6 * deltaTime;
      GotoPoint3d[2] := point3d[2] + GP3D[2] * -6 * deltaTime;

      point3d := GotoPoint3d;
    end
    else
    begin
      GotoPoint3d[0] := Player_Cube.AbsolutePosition[0] +
        Player_Cube.AbsoluteDirection[0] * -2;
      GotoPoint3d[1] := Player_Capsule.AbsolutePosition[1] + 1.6 +
        Player_Cam.AbsoluteDirection[1];
      GotoPoint3d[2] := Player_Cube.AbsolutePosition[2] +
        Player_Cube.AbsoluteDirection[2] * -2;
    end;

    NGDDynamicBehav.Pick(GotoPoint3d, pmMove);
  end;

  if IsKeyDown(VK_LBUTTON) then
  begin
    point3d := VectorMake(GLSceneViewer1.Buffer.PixelRayToWorld(cp.X, cp.Y));
    if not(OnMouse_Click) and OnPick then
    begin
      FPickedSceneObject := GLSceneViewer1.Buffer.GetPickedObject(cp.X, cp.Y);
      if Assigned(FPickedSceneObject) then
        if FPickedSceneObject.DistanceTo(Player_Capsule) <= 3.5 then
        begin
          if Assigned(FPickedSceneObject) then
          // If the user click on a glSceneObject
          begin
            NGDDynamicBehav := FPickedSceneObject.Behaviours.GetByClass
              (TGLNGDDynamic) as TGLNGDDynamic;
            if Assigned(NGDDynamicBehav) then
            begin
              // point3d is the global space point of the body to attach
              NGDDynamicBehav.Pick(point3d, pmAttach);
              OnMouse_Click := True;
              GotoPoint3d := point3d;
              On_Drop.Tag := 1;
              On_Drop.Enabled := True;
            end
            else
              FPickedSceneObject := nil;
            // We save the normal to create a plane parallel to camera in mouseMove Event.
            FPaneNormal := Player_Cam.AbsoluteDirection;
          end;
        end;
    end
    else
    begin
      if Assigned(NGDDynamicBehav) and OnDrop then
      begin
        NGDDynamicBehav.Pick(point3d, pmDetach);
        NGDDynamicBehav := nil;
        FPickedSceneObject := nil;
        OnMouse_Click := False;
        On_Drop.Tag := 0;
        On_Drop.Enabled := True;
      end;
    end;
  end;

  if Assigned(FPickedSceneObject) then
  begin
    ScObj := FPickedSceneObject;
  end;

  if not(OnMouse_Click) then
  begin
    // Detach the body
    if Assigned(NGDDynamicBehav) then
    begin
      Timer_OnVelocity.Enabled := True;
    end;
  end;

  if Assigned(NGDDynamicBehav) and IsKeyDown(VK_RBUTTON) then
  begin
    NGDDynamicBehav.UseVelocity := True;
    I := Player_Cam.AbsoluteDirection;
    I[0] := I[0] * 30;
    I[1] := I[1] * 30;
    I[2] := I[2] * 30;
    NGDDynamicBehav.Velocity.AsVector := I;

    OnMouse_Click := False;
    On_Drop.Tag := 0;
    On_Drop.Enabled := True;
  end;

  for j := 0 to Maps_Count - 1 do
  begin
    OnGround := maps[j].mdl.OctreeAABBIntersect
      (GLCube3.AxisAlignedBoundingBox(), GLCube3.AbsoluteMatrix,
      GLCube3.InvAbsoluteMatrix);
    if OnGround then
      break;
  end;

  if IsKeyDown(VK_SPACE) and OnGround and not(OnAir) then
  begin
    with TGLNGDDynamic(Player_Capsule.Behaviours.GetByClass(TGLNGDDynamic)) do
    begin
      I := Player_Cube.AbsoluteDirection;
      UseVelocity := True;
      if IsKeyDown('w') then
      begin
        Velocity.X := -I[0] * 6;
        Velocity.Y := 7;
        Velocity.Z := -I[2] * 6;
      end
      else
        Velocity.Y := 7;
      OnGround := False;
    end;
    Jump_V_timer.Enabled := True;
    OnAir := True;
    Jump_Timer.Enabled := True;
  end;

  if Assigned(currentPick) then
    if currentPick.Material.FrontProperties.Emission.Color[0] = 1 then
    begin
      GLHUDSprite1.Visible := True;
    end
    else
    begin
      GLHUDSprite1.Visible := False;
    end;

  if IsKeyDown('w') and OnGround and not(IsKeyDown(VK_SPACE)) then
  begin
    with TGLNGDDynamic(Player_Capsule.Behaviours.GetByClass(TGLNGDDynamic)) do
    begin
      I := Player_Cube.AbsoluteDirection;

      UseVelocity := True;

      Velocity.X := -I[0] * 4;
      Velocity.Y := AppliedVelocity.Y;
      Velocity.Z := -I[2] * 4;
    end;
  end;

  if IsKeyDown('s') and OnGround then
  begin
    with TGLNGDDynamic(Player_Capsule.Behaviours.GetByClass(TGLNGDDynamic)) do
    begin
      I := Player_Cube.AbsoluteDirection;

      UseVelocity := True;

      Velocity.X := I[0] * 4;
      Velocity.Y := AppliedVelocity.Y;
      Velocity.Z := I[2] * 4;
    end;
  end;

  if IsKeyDown('a') and OnGround then
  begin
    with TGLNGDDynamic(Player_Capsule.Behaviours.GetByClass(TGLNGDDynamic)) do
    begin
      I := Player_Cam.AbsoluteRight;

      UseVelocity := True;

      Velocity.X := I[0] * 4;
      Velocity.Y := AppliedVelocity.Y;
      Velocity.Z := I[2] * 4;
    end;
  end;

  if IsKeyDown('d') and OnGround then
  begin
    with TGLNGDDynamic(Player_Capsule.Behaviours.GetByClass(TGLNGDDynamic)) do
    begin
      I := Player_Cam.AbsoluteLeft;

      UseVelocity := True;

      Velocity.X := I[0] * 4;
      Velocity.Y := AppliedVelocity.Y;
      Velocity.Z := I[2] * 4;
    end;
  end;

  if IsKeyDown('d') and IsKeyDown('w') and OnGround then
  begin
    with TGLNGDDynamic(Player_Capsule.Behaviours.GetByClass(TGLNGDDynamic)) do
    begin
      I := VectorAdd(Player_Cube.AbsoluteLeft, Player_Cube.AbsoluteDirection);

      UseVelocity := True;

      Velocity.X := -I[0] * 4;
      Velocity.Y := AppliedVelocity.Y;
      Velocity.Z := -I[2] * 4;
    end;
  end;

  if IsKeyDown('a') and IsKeyDown('w') and OnGround then
  begin
    with TGLNGDDynamic(Player_Capsule.Behaviours.GetByClass(TGLNGDDynamic)) do
    begin
      I := VectorAdd(Player_Cube.AbsoluteRight, Player_Cube.AbsoluteDirection);

      UseVelocity := True;

      Velocity.X := -I[0] * 4;
      Velocity.Y := AppliedVelocity.Y;
      Velocity.Z := -I[2] * 4;
    end;
  end;

  if IsKeyDown('a') and IsKeyDown('s') and OnGround then
  begin
    with TGLNGDDynamic(Player_Capsule.Behaviours.GetByClass(TGLNGDDynamic)) do
    begin
      I := VectorAdd(Player_Cube.AbsoluteLeft, Player_Cube.AbsoluteDirection);

      UseVelocity := True;

      Velocity.X := I[0] * 4;
      Velocity.Y := AppliedVelocity.Y;
      Velocity.Z := I[2] * 4;
    end;
  end;

  if IsKeyDown('d') and IsKeyDown('s') and OnGround then
  begin
    with TGLNGDDynamic(Player_Capsule.Behaviours.GetByClass(TGLNGDDynamic)) do
    begin
      I := VectorAdd(Player_Cube.AbsoluteRight, Player_Cube.AbsoluteDirection);

      UseVelocity := True;

      Velocity.X := I[0] * 4;
      Velocity.Y := AppliedVelocity.Y;
      Velocity.Z := I[2] * 4;
    end;
  end;

  if not(OnGround) and not(OnAir) then
    with TGLNGDDynamic(Player_Capsule.Behaviours.GetByClass(TGLNGDDynamic)) do
    begin
      if UseVelocity then
      begin
        Velocity.AsVector := VectorMake(0, 0, 0, 0);
        UseVelocity := False;
      end
      else
        UseVelocity := False;
    end;

  if not(IsKeyDown('w')) and not(IsKeyDown('s')) and not(IsKeyDown('a')) and
    not(IsKeyDown('d')) and not(IsKeyDown(VK_SPACE)) then
    with TGLNGDDynamic(Player_Capsule.Behaviours.GetByClass(TGLNGDDynamic)) do
    begin
      if UseVelocity then
      begin
        Velocity.AsVector := VectorMake(0, 0, 0, 0);
        UseVelocity := False;
      end
      else
        UseVelocity := False;
    end;

  GLSceneViewer1.Invalidate;

  GLNGDManager1.Step(deltaTime);

  if Player_Capsule.Position.Y < -10 then
  begin
    with TGLNGDDynamic(Player_Capsule.Behaviours.GetByClass(TGLNGDDynamic)) do
    begin
      mm := NewtonBodyMatrix;
      mm[3] := VectorMake(0, 0, 0, 1);
      NewtonBodyMatrix := mm;
    end;
  end;

  if IsKeyDown(VK_ESCAPE) then
    close;
end;

procedure TForm1.GLSceneViewer1MouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
begin
  M_X := X;
  M_Y := Y;
end;

procedure TForm1.Menu_Load;
begin
  Form1.Width := 1280;
  Form1.Height := 1024;

  GLHUDSprite1.Position.X := 640;
  GLHUDSprite1.Position.Y := 512;

  GLSceneViewer1.Height := 1024;
  GLSceneViewer1.Width := 1280;

  (*
    GLMaterialLibrary1.AddTextureMaterial('brick_b','brick_b.jpg');
    with GLMaterialLibrary1.Materials.GetLibMaterialByName('brick_b') do
    begin
    Material.Texture.Disabled:=false;
    end;

    GLMaterialLibrary1.AddTextureMaterial('mat1','Finishes.jpg');
    with GLMaterialLibrary1.Materials.GetLibMaterialByName('mat1') do
    begin
    Material.Texture.Disabled:=false;
    end;
  *)

  OnPick := True;
  GLUserInterface1.MouseLookActivate;
  Game_Load;
end;

procedure TForm1.Game_Load;
var
  X, j, dyns: Integer;
  Models: string;
  PX, PY, PZ: Integer;
  RX, RY, RZ: Integer;
  DX, DY, DZ: Integer;
  Nam, end1, end2: Integer;
  b_ti1, b_ti2, b_ti3, ML_col: Integer;
begin
  SetCurrentDir('..\..\Media\');
  // Memo1.Lines.Clear;
  // memo1.Lines.LoadFromFile('level.txt');
  SetLength(maps, Memo1.Lines.Count);
  For X := 0 to Memo1.Lines.Count - 1 do
  begin
    if copy(Memo1.Lines.Strings[X], 0, 8) = '[Model]=' then
    begin
      Models := Memo1.Lines.Strings[X];
      for j := 0 to Length(Models) do
      begin
        if copy(Models, j, 3) = 'PX_' then
          PX := j + 3;
        if copy(Models, j, 3) = 'PY_' then
          PY := j + 3;
        if copy(Models, j, 3) = 'PZ_' then
          PZ := j + 3;

        if copy(Models, j, 3) = 'RX_' then
          RX := j + 3;
        if copy(Models, j, 3) = 'RY_' then
          RY := j + 3;
        if copy(Models, j, 3) = 'RZ_' then
          RZ := j + 3;

        if copy(Models, j, 3) = 'DX_' then
          DX := j + 3;
        if copy(Models, j, 3) = 'DY_' then
          DY := j + 3;
        if copy(Models, j, 3) = 'DZ_' then
          DZ := j + 3;

        if copy(Models, j, 7) = '[Name]=' then
          Nam := j + 7;
        if copy(Models, j, 10) = '[Dynamic]=' then
          dyns := j + 10;

        if copy(Models, j, 1) = ':' then
          end1 := j - 1;

        if copy(Models, j, 3) = 'DM=' then
          b_ti1 := j + 3;
        if copy(Models, j, 3) = 'NM=' then
          b_ti2 := j + 3;
        if copy(Models, j, 3) = 'LM=' then
          b_ti3 := j + 3;

        if copy(Models, j, 1) = ';' then
          end2 := j - 1;
      end;

      if StrToBool(copy(Models, dyns, b_ti1 - dyns - 4 { end1-dyns+1 } ))
        = False then
      begin
        with maps[X] do
        begin
          mdl := TGLFreeForm.CreateAsChild(Scene_Objects);

          mdl.MaterialLibrary := GLMaterialLibrary1;

          mdl.LoadFromFile(copy(Models, Nam, dyns - Nam - 10));

          mdl.Position.X := StrToFloat(copy(Models, PX, PY - PX - 3));
          mdl.Position.Y := StrToFloat(copy(Models, PY, PZ - PY - 3));
          mdl.Position.Z := StrToFloat(copy(Models, PZ, RX - PZ - 3));

          mdl.RollAngle := StrToFloat(copy(Models, RX, RY - RX - 3));
          mdl.TurnAngle := StrToFloat(copy(Models, RY, RZ - RY - 3));
          mdl.PitchAngle := StrToFloat(copy(Models, RZ, DX - RZ - 3));

          mdl.Direction.X := StrToFloat(copy(Models, DX, DY - DX - 3));
          mdl.Direction.Y := StrToFloat(copy(Models, DY, DZ - DY - 3));
          mdl.Direction.Z := StrToFloat(copy(Models, DZ, Nam - DZ - 7));

          mdl.BuildOctree();

          mdl.Behaviours.GetOrCreate(TGLNGDStatic);

          Maps_Count := Maps_Count + 1;

          with TGLNGDStatic(mdl.Behaviours.GetByClass(TGLNGDStatic)) do
          begin
            Manager := GLNGDManager1;
          end;
        end;
      end
      else
      begin
        with maps[X] do
        begin
          mdl := TGLFreeForm.CreateAsChild(Scene_Objects);

          mdl.LoadFromFile(copy(Models, Nam, dyns - Nam - 10));

          mdl.Position.X := StrToFloat(copy(Models, PX, PY - PX - 3));
          mdl.Position.Y := StrToFloat(copy(Models, PY, PZ - PY - 3));
          mdl.Position.Z := StrToFloat(copy(Models, PZ, RX - PZ - 3));

          mdl.RollAngle := StrToFloat(copy(Models, RX, RY - RX - 3));
          mdl.TurnAngle := StrToFloat(copy(Models, RY, RZ - RY - 3));
          mdl.PitchAngle := StrToFloat(copy(Models, RZ, DX - RZ - 3));

          mdl.Direction.X := StrToFloat(copy(Models, DX, DY - DX - 3));
          mdl.Direction.Y := StrToFloat(copy(Models, DY, DZ - DY - 3));
          mdl.Direction.Z := StrToFloat(copy(Models, DZ, Nam - DZ - 7));

          Maps_Count := Maps_Count + 1;

          mdl.Behaviours.GetOrCreate(TGLNGDDynamic);

          with TGLNGDDynamic(mdl.Behaviours.GetByClass(TGLNGDDynamic)) do
          begin
            Manager := GLNGDManager1;
          end;

          mdl.Tag := 1;

          mdl.OnProgress := GLCube2.OnProgress;

          mdl.BuildOctree();
        end;
      end;
    end;
  end;
end;

procedure TForm1.Jump_TimerTimer(Sender: TObject);
begin
  OnAir := False;
  Jump_Timer.Enabled := False;
end;

procedure TForm1.Jump_V_timerTimer(Sender: TObject);
begin
  with TGLNGDDynamic(Player_Capsule.Behaviours.GetByClass(TGLNGDDynamic)) do
  begin
    Velocity.Y := 0;
    UseVelocity := False;
  end;
  Jump_V_timer.Enabled := False;
end;

procedure TForm1.On_DropTimer(Sender: TObject);
begin
  if On_Drop.Tag = 1 then
  begin
    OnDrop := True;
    OnPick := False;
    On_Drop.Enabled := False;
  end
  else
  begin
    OnDrop := False;
    OnPick := True;
    On_Drop.Enabled := False;
  end;
end;

procedure TForm1.Timer_OnVelocityTimer(Sender: TObject);
begin
  NGDDynamicBehav.UseVelocity := False;
  NGDDynamicBehav.Velocity.AsVector := VectorMake(0, 0, 0, 1);
  NGDDynamicBehav.Pick(point3d, pmDetach);
  NGDDynamicBehav := nil;
  FPickedSceneObject := nil;
  Timer_OnVelocity.Enabled := False;
  On_Drop.Tag := 0;
  On_Drop.Enabled := True;
end;

procedure TForm1.TIPickTimerTimer(Sender: TObject);
var
  cp: TPoint;
begin
  GetCursorPos(cp);
  cp := GLSceneViewer1.ScreenToClient(cp);
  currentPick := (GLSceneViewer1.Buffer.GetPickedObject(cp.X, cp.Y)
    as TGLCustomSceneObject);
  TIPickTimer.Enabled := False;
end;

end.
