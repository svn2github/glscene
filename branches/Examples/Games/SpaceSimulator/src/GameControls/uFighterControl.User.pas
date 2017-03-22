{TdfUserControl - класс, отвечающий за контроль пользовалем корабл€}

unit uFighterControl.User;

interface

uses
  Windows,
  System.SysUtils,
  System.IniFiles,
  System.Contnrs,
  System.Math,

  GLScene,
  GLKeyboard,
  GLVectorGeometry,
  GLVectorTypes,
  GLHUDObjects,
  GLObjects,
  GLMaterial,
  GLTexture,
  GLFilePNG,

  uSpacefighter,
  uLog,
  dfKeyboardExt,
  uFighterControl,
  uSpeedIndicator,
  uFonts,
  uTarget,
  uGameObject;

const
  C_HUD_PATH = 'data\hud\';
  C_USERCONTROL_KEYBIND_FILE = 'data\userkeys.ini';
  C_USERCONTROL_KEYBIND_SECTION = 'Keys';
  C_WEAPON_FOCUS_RANGE = 600;
  C_DIRARROWS_COUNT = 5; //”казывает количество стрелок, рисуемых как направление движени€
  C_DIRARROWS_DISTANCEBTWN = 350; //ћножитель дл€ разброса (друг от друга) направл€ющих стрелок
  C_DIRARROW_SCALE_X = 0.5;
  C_DIRARROW_SCALE_Y = 0.5;
  C_RETURN_CAMERA_SPEED = 4;
  C_RETURN_CAMERA_MINDISTANCE = 0.3;
  C_RETURN_ROTATE_TIME = 1;
  C_RETURN_ROTATE_MINDISTANCE = 0.05;
  C_ACTION_COUNT = 5;
  C_ACTION_NAMES: array[0..C_ACTION_COUNT - 1] of String =
    ('Shoot', 'SwitchModes', 'AccelUp', 'AccelDown', 'NextTarget');

  {True указывает, что данное действие следует отслеживать непрерывно и асинхронно
   с помощью IsKeyDown().

   False указывает, что должно быть отслежено нажатие и отпуск клавиши,
   например при переключении режимов полета, включении карты и т.д.}
  C_ACTION_ASYNC: array[0..C_ACTION_COUNT - 1] of Boolean = (True, False, True, True, False);
  C_CAMERA_DODGE = 5; //ѕоказывает, насколько сильно камера отклон€етс€ при поворотах истребител€
  C_CAMERA_NORMPOS_UP = 5; //¬ысота камеры над истребителем
  C_CAMERA_NORMPOS_DIST = - 45; //ќтдаление камеры от истребител€
  C_SPACEFIGHTER_MAXROLL = 45;

type
  TdfKeyBind = record
    sAction: String;
    iKey: Integer; //≈сли используетс€ char, то выставл€ем iKey в -1
    cKey, cKey2: Char;
    pAction: procedure of object;
    bPressed, bPressed2: Boolean;
  end;

  TdfUserControl = class (TdfFighterControl)
  private
    FViewerW, FViewerH: Integer;
    FCenterX, FCenterY: Integer;
    FCamera: TGLCamera;
    FKeyBinds: array of TdfKeyBind;
    FHUDDirections: TGLDummyCube;
    FCursorSprite: TGLHUDSprite;
    FCamNormPos: TAffineVector;
    FMouseMode: Boolean;
    FToMainPlane: Boolean; //True, если надо повернуть корабль в основную плоскость
    FToNormPos: Boolean;   //Ќадо ли вернуть камеру в нормальное положение
    FTransVec, FUpVec: TAffineVector;
    Ft, FRollAngle: Single;

//    FEnemyList: TObjectList;
    FTarget: TdfTarget;
    FCur: Integer;
//    FSelectedObject: TdfGameObject;

    FSpeedIndicator: TdfSpeedIndicator;
    FX, FY: Integer; //MousePos
    FDeltaTime: Double;
    {часто используемые переменные в функци€х класса}
    dmx, dmy: Single;
    sf_dir, cam_dir: TAffineVector;
    rayStart, rayDir: TVector;
    intPoint: TVector;
    tmpInt: Integer;
    procedure AddDirectionsMaterial(aName, aTextureName: String);
    procedure CheckKeys();
  protected
    procedure Shoot; override;
    procedure SetMouseMovement; override;
    procedure Accelerate; override;
    procedure Brake; override;
    procedure NextTarget; override;
    procedure SwitchFlightModes;
//    procedure RotateSpaceFighter;
  public
    constructor Create(aSpacefighter: TdfSpaceFighter;
      aHUDRoot: TGLBaseSceneObject); virtual;
    procedure Update(deltaTime: Double; X, Y: Integer); override;
    procedure LoadKeyBindsFromFile(aFile: String = C_USERCONTROL_KEYBIND_FILE);
    procedure SaveKeyBindsToFile(aFile: String = C_USERCONTROL_KEYBIND_FILE);

//    procedure SetEnemyList(aList: TObjectList); override;

    property Target: TdfTarget read FTarget write FTarget;
    property MouseMode: Boolean read FMouseMode write FMouseMode;
    procedure ResetParams;
  end;

//=====================================================================
implementation
//=====================================================================

uses
  uGLSceneObjects, uTweener;

{ TdfUserControl }

procedure TdfUserControl.Accelerate();
begin
  FSpaceFighter.Accelerate(FDeltaTime);
  FSpeedIndicator.Speed := FSpaceFighter.Speed;
end;

procedure TdfUserControl.AddDirectionsMaterial(aName, aTextureName: String);
begin
  if not Assigned(dfGLSceneObjects.MatLibrary.LibMaterialByName(aName)) then
    with dfGLSceneObjects.MatLibrary.Materials.Add do
    begin
      Name := aName;
      with Material do
      begin
        Texture.Image.LoadFromFile(C_HUD_PATH + aTextureName);
        Texture.Enabled := True;
        Texture.TextureMode := tmModulate;
        Texture.TextureWrap := twNone;
        BlendingMode := bmTransparency;
        FrontProperties.Diffuse.SetColor(0, 1, 0, 0.7);
        MaterialOptions := [moIgnoreFog, moNoLighting];
      end;
    end;
end;

procedure TdfUserControl.Brake();
begin
  FSpaceFighter.Accelerate(-FDeltaTime);
  FSpeedIndicator.Speed := FSpaceFighter.Speed;
end;

procedure TdfUserControl.CheckKeys();
var
  i: Integer;
  Flag: Boolean;
begin
  for i := 0 to Length(FKeyBinds) - 1 do
  begin
    Flag := False;

    with FKeyBinds[i] do
      if C_ACTION_ASYNC[i] then
      begin
        if iKey <> -1 then
          Flag := IsKeyDown(iKey)
        else
          Flag := IsKeyDown(cKey) or IsKeyDown(cKey2);
      end
      else
      begin
        if iKey <> -1 then
          Flag := IsKeyPressed(iKey, @bPressed)
        else
          Flag := IsKeyPressed(cKey, @bPressed) or IsKeyPressed(cKey2, @bPressed2);
      end;

    if Flag then
      if Assigned(FKeyBinds[i].pAction) then
        FKeyBinds[i].pAction();
  end;
end;

constructor TdfUserControl.Create(aSpacefighter: TdfSpaceFighter;
  aHUDRoot: TGLBaseSceneObject);
var
  w, h: Single;
  i: Integer;
begin
  inherited Create;
  if not Assigned(aSpacefighter) then
    logWriteError('TdfUserControl: spacefighter pointer is nil!');
  if not Assigned(dfGLSceneObjects.Camera) then
    logWriteError('TdfUserControl: camera pointer is nil!');

  FSpaceFighter := aSpaceFighter;
  FCamera := dfGLSceneObjects.Camera;
  FCamera.MoveTo(FSpaceFighter);

  FCursorSprite := TGLHUDSprite.CreateAsChild(aHUDRoot);
  with FCursorSprite.Material do
  begin
    Texture.Image.LoadFromFile(C_HUD_PATH + 'cursor1.png');
    Texture.Enabled := True;
    Texture.TextureMode := tmModulate;
    BlendingMode := bmTransparency;
    MaterialOptions := [moNoLighting, moIgnoreFog];
    FrontProperties.Diffuse.SetColor(0,1,0, 0.7);
  end;
  FCursorSprite.SetSize(32, 32);

  FSpeedIndicator := TdfSpeedIndicator.CreateAsChild(aHUDRoot);
  FSpeedIndicator.AddMaterialToCells('cell.png');
  FSpeedIndicator.MaxSpeed := FSpaceFighter.MaxSpeed;
  FSpeedIndicator.AddFontToText(GetFont(C_FONT_1));

  FHUDDirections := TGLDummyCube.CreateAsChild(aHUDRoot);
  AddDirectionsMaterial('direction', 'direction2.png');
  with dfGLSceneObjects.MatLibrary.LibMaterialByName('direction').Material.Texture.Image do
  begin
    w := Width * C_DIRARROW_SCALE_X;
    h := Height * C_DIRARROW_SCALE_Y;
  end;
  for i := 0 to C_DIRARROWS_COUNT - 1 do
    with TGLHUDSprite.CreateAsChild(FHUDDirections) do
    begin
      Material.MaterialLibrary := dfGLSceneObjects.MatLibrary;
      Material.LibMaterialName := 'direction';
      Width := w;
      Height := h;
    end;

  FTarget := TdfTarget.CreateAsChild(aHUDRoot);
  with FTarget do
  begin
    Target := nil;
    Chaser := FSpaceFighter;
    AddMaterialToFrame('target.png');
    AddMaterialToAim('targetShoot.png');
    AddMaterialToLifeBar('lifebar.png');
    AddMaterialToDirection('dirtotarget.png');
    AddFontToDistance(GetFont(C_FONT_1));
  end;

  FViewerW := dfGLSceneObjects.Viewer.Width;
  FViewerH := dfGLSceneObjects.Viewer.Height;
  FCenterX := FViewerW div 2;
  FCenterY := FViewerH div 2;

  with FSpaceFighter do
    FCamNormPos := VectorAdd(
      VectorScale(Direction.AsAffineVector, C_CAMERA_NORMPOS_DIST),
      VectorScale(Up.AsAffineVector, C_CAMERA_NORMPOS_UP));



  SetLength(FKeyBinds, C_ACTION_COUNT);
  LoadKeyBindsFromFile();

  FEnabled := False;
end;

procedure TdfUserControl.LoadKeyBindsFromFile(aFile: String);
var
  Ini: TIniFile;
  str1: String;
  i: Integer;
begin
  if FileExists(aFile) then
  begin
    Ini := TIniFile.Create(aFile);
    for i := 0 to Length(C_ACTION_NAMES) - 1 do
    begin
      //„итаем из ини-файда
      str1 := Ini.ReadString(C_USERCONTROL_KEYBIND_SECTION, C_ACTION_NAMES[i], '');
      if Length(str1) = 0 then
        logWriteError('TdfUserControl: keybind for ' + C_ACTION_NAMES[i]
          + ' not found in file ' + aFile)
      else
      begin
        FKeyBinds[i].sAction := C_ACTION_NAMES[i];
        FKeyBinds[i].iKey := KeyNameToVirtualKeyCode(str1);
        //KeyNameToVirtualKeyCode возвращает -1, если найти спец-клавишу с таким
        //именем не удалось. «начит - перед нам символ
        if FKeyBinds[i].iKey = -1 then
        begin
          FKeyBinds[i].cKey := str1[1];
          //≈сли длина больше, то принимаем второй символ в качестве второй клавиши
          if Length(str1) > 1 then
            FKeyBinds[i].cKey2 := str1[2];
        end;
      end;

      case i of
        0: {Shoot}       FKeyBinds[i].pAction := Self.Shoot;
        1: {SwitchModes} FKeyBinds[i].pAction := Self.SwitchFlightModes;
        2: {AccelUp}     FKeyBinds[i].pAction := Self.Accelerate;
        3: {AccelDown}   FKeyBinds[i].pAction := Self.Brake;
        4: {NextTarget}  FKeyBinds[i].pAction := Self.NextTarget;
      end;
    end;
    Ini.Free;
  end
  else
    logWriteError('TdfUserControl: file "' + aFile + '" not found');
end;

procedure TdfUserControl.NextTarget;
begin
  inherited;
  FCur := (FCur + 1) mod (FSpaceFighter.ObjectsAround.Count);
  if Assigned(FSpaceFighter.ObjectsAround[FCur]) then
    FTarget.Target := TdfGameObject(FSpaceFighter.ObjectsAround[FCur]);
end;

procedure TdfUserControl.ResetParams;
begin
  MouseMode := False;
  FSpeedIndicator.Speed := 0;
  FCamera.Position.AsAffineVector := FCamNormPos;
  FCur := -1;
end;

{
const
  C_SPEED = 20;

procedure TdfUserControl.RotateSpaceFighter;
begin
  if IsKeyDown(VK_LEFT) then
    FSpaceFighter.Fighter.Roll(FdeltaTime * C_SPEED)
  else
    if IsKeyDown(VK_RIGHT) then
      FSpaceFighter.Fighter.Roll(-FdeltaTime * C_SPEED);

  if IsKeyDown(VK_UP) then
    FSpaceFighter.Fighter.Pitch(FdeltaTime * C_SPEED)
  else
    if IsKeyDown(VK_DOWN) then
      FSpaceFighter.Fighter.Pitch(-FdeltaTime * C_SPEED);

  if IsKeyDown(VK_NUMPAD8) then
  begin
    AddVector(FCamNormPos, VectorScale(YVector, FdeltaTime * C_SPEED));
    FCamera.Position.AsAffineVector := FCamNormPos;
  end
  else
    if IsKeyDown(VK_NUMPAD2) then
    begin
      AddVector(FCamNormPos, VectorScale(YVector, - FdeltaTime * C_SPEED));
      FCamera.Position.AsAffineVector := FCamNormPos;
    end;

  if IsKeyDown(VK_NUMPAD1) then
  begin
    AddVector(FCamNormPos, VectorScale(ZVector, - FdeltaTime * C_SPEED));
    FCamera.Position.AsAffineVector := FCamNormPos;
  end
  else
    if IsKeyDown(VK_NUMPAD3) then
    begin
      AddVector(FCamNormPos, VectorScale(ZVector, FdeltaTime * C_SPEED));
      FCamera.Position.AsAffineVector := FCamNormPos;
    end;
end;    }

procedure TdfUserControl.SaveKeyBindsToFile(aFile: String);
var
  i: Integer;
  Ini: TIniFile;
begin
  ini := TIniFile.Create(aFile);

  for i := 0 to Length(FKeyBinds) - 1 do
    if FKeyBinds[i].iKey <> -1 then
      Ini.WriteString(C_USERCONTROL_KEYBIND_SECTION, FKeyBinds[i].sAction,
                              VirtualKeyCodeToKeyName(FKeyBinds[i].iKey))
    else
      Ini.WriteString(C_USERCONTROL_KEYBIND_SECTION, FKeyBinds[i].sAction, FKeyBinds[i].cKey);

  ini.Free;
end;

//procedure TdfUserControl.SetEnemyList(aList: TObjectList);
//begin
//  inherited;
////  FEnemyList := aList;
//end;

procedure TdfUserControl.SetMouseMovement();
var
  i: Integer;
begin
    //Ћетаем с помощью мышки
  if FMouseMode then
  begin
    dmx :=   (FX - FCenterX) / FCenterX;
    dmy := - (FY - FCenterY) / FCenterY;

    sf_dir := VectorAdd(VectorScale(AffineVectorMake(FCamera.AbsoluteLeft), dmx),
                        VectorScale(AffineVectorMake(FCamera.AbsoluteUp)  , dmy));
    cam_dir := VectorAdd(VectorScale(AffineVectorMake(FCamera.LeftVector), - 2 * dmx),
                        VectorScale(AffineVectorMake(FCamera.Up.AsVector), 2.5 * dmy));
    FCamera.Position.AsAffineVector :=
      VectorAdd(FCamNormPos, VectorScale(cam_dir, C_CAMERA_DODGE));
    ScaleVector(sf_dir, FDeltaTime);
    with FSpaceFighter do
    begin
      Direction.AsAffineVector := VectorAdd(Direction.AsAffineVector, sf_dir);
      Direction.Normalize;
    end;

    FSpaceFighter.Fighter.RollAngle := dmx * C_SPACEFIGHTER_MAXROLL;

    //ƒелаем dir arrows - стрелки, обозначающие направление движени€ корабл€
    FHUDDirections.Visible := True;
    tmpInt := (Round((dmy * dmy + dmx * dmx) * 10)) div 1;
    for i := 0 to FhudDirections.Count - 1 do
      with TGLHudSprite(FhudDirections.Children[i]) do
      begin
        Visible := i < tmpInt;
        Rotation := (180 / pi) *
          Arccos(VectorAngleCosine(YVector, AffineVectorMake(dmx, dmy, 0)));
        if (dmx > 0) then
          Rotation := - Rotation;
        Position.AsAffineVector :=
          VectorAdd(AffineVectormake(FCenterX, FCenterY, 0),
                    VectorScale(AffineVectorMake(dmx, -dmy, 0), C_DIRARROWS_DISTANCEBTWN * i / C_DIRARROWS_COUNT));
      end;
  end
  else
  begin
    FHUDDirections.Visible := False;
  end;
end;

procedure TdfUserControl.Shoot;
var
  i: Integer;
begin
  rayStart := FCamera.AbsolutePosition;
  with dfGLSceneObjects.Viewer do
    rayDir := Buffer.ScreenToVector(FX, Height - FY);
  RayCastPlaneIntersect(rayStart, rayDir,
                        VectorAdd(VectorScale(FCamera.AbsoluteDirection, C_WEAPON_FOCUS_RANGE),
                          FCamera.AbsolutePosition),
                        VectorNegate(FCamera.AbsoluteDirection), @intPoint);

  for i := 0 to FSpaceFighter.WeaponsCount - 1 do
    with FSpaceFighter.Weapons[i] do
    begin
      rayDir := VectorSubtract(intPoint, AbsolutePosition);
      Fire(AffineVectorMake(rayDir));
    end;
end;

procedure TdfUserControl.SwitchFlightModes;
begin
  FMouseMode := not FMouseMode;
  if not FMouseMode then
  begin
    //»нициировать возврат камеры в центр
    FToNormPos := True;
    FTransVec := VectorSubtract(FCamNormPos, FCamera.Position.AsAffineVector);
    FRollAngle := FSpaceFighter.Fighter.RollAngle;

    //»нициировать возврат корабл€ в основную плоскость
    FToMainPlane := True;
    FUpVec := FSpaceFighter.Up.AsAffineVector;
    FT := 0;
  end;
end;

procedure TdfUserControl.Update(deltaTime: Double; X, Y: Integer);
begin
  if not FEnabled then
    Exit;
  FDeltaTime := deltaTime;
  FX := X;
  FY := Y;
  FCursorSprite.Position.SetPoint(X, Y, 0);

  if Assigned(FSpaceFighter) then
  begin
    SetMouseMovement();
    CheckKeys();
//    if Assigned(FSelectedObject) then
      FTarget.Update(deltaTime);
      if Assigned(FTarget.Target) then
        if not FTarget.Target.Visible  then
          NextTarget();
    
//    RotateSpaceFighter();

    {¬озвращаем камеру}
    if FToNormPos then
      if (VectorDistance(FCamera.Position.AsAffineVector, FCamNormPos) < C_RETURN_CAMERA_MINDISTANCE)
        or (VectorLength(FTransVec) < C_RETURN_CAMERA_MINDISTANCE) then
      begin
        FCamera.Position.AsAffineVector := FCamNormPos;
        FToNormPos := False;
      end
      else
        FCamera.Position.AddScaledVector(deltaTime * C_RETURN_CAMERA_SPEED, FTransVec);

    {¬озвращаем корабль в основную плоскость и убираем крен}
    if FToMainPlane then
    begin
      Ft := Ft + deltaTime;
      if (Ft >= C_RETURN_ROTATE_TIME) or
         (Abs(VectorDotProduct(YVector, FSpaceFighter.Up.AsAffineVector) - 1) < 0.001) then
      begin
        FSpaceFighter.Up.AsAffineVector := YVector;
        FSpaceFighter.Fighter.RollAngle := 0;
        Ft := 0;
        FToMainPlane := False;
      end
      else
      begin
        FSpaceFighter.Up.AsAffineVector := VectorAngleLerp(FUpVec, YVector,
         Ft / C_RETURN_ROTATE_TIME);
        FSpaceFighter.Fighter.RollAngle := FRollAngle * (C_RETURN_ROTATE_TIME - Ft);
      end;
    end;

  end;
end;

end.
