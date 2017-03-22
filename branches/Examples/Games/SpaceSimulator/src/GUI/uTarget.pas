unit uTarget;

interface

uses
  System.SysUtils,
  System.Classes,

  GLScene,
  GLObjects,
  GLHUDObjects,
  GLMaterial,
  GLTexture,
  GLWindowsFont,
  GLVectorGeometry,
  GLVectorTypes,
  GLCrossPlatform,
  GLFilePNG,
  uGameObject,
  uSpacefighter;

const
  C_HUD_PATH = 'data\hud\';
  C_DISTANCE_OFFSET_X = 0;
  C_DISTANCE_OFFSET_Y = -25;
  C_DISTANCE_SCALE = 0.5;
  C_LIFEBAR_OFFSET_X = 0;
  C_LIFEBAR_OFFSET_Y = 25;
//  C_LIFEBAR_MAXWIDTH = 60;

type
  TdfTarget = class (TGLDummyCube)
  private
    FTarget: TdfGameObject; //Объект, на который все это нацелено
    FChaser: TdfGameObject; //Объект, который получает информацию, преследователь
    FFrame: TGLHUDSprite; //Рамка вокруг объекта
    FAim: TGLHUDSprite;   //Точка вероятного перехвата - куда стрелять
    FLifeBar: TGLHUDSprite; //Указатель "жизни" объекта
    FDirection: TGLHUDSprite; //Если объект вне экрана, то рисуется указатель на него
    FDistance: TGLHUDText; //Дистанция до объекта
    FLifeBarBaseWidth: Single;
    FActive: Boolean;
    FDistanceValue: Single;
    intPoint, targetDir: TVector;
    targetDir2: TVector2f;
    wndCenterX, wndCenterY: Integer;
    FNoAim, FNoLifeBar: Boolean;
//    FTime, t: Single;
    function GetInterceptionPoint(q: Integer; posObject, posWeapon,
      velObject: TVector; velWeapon: Single): TVector;
    procedure SetActive(const Value: Boolean);
    procedure SetChaser(const Value: TdfGameObject);
    procedure SetTarget(const Value: TdfGameObject);
    procedure SetNeutralTarget();
    procedure SetAllyTarget();
    procedure SetEnemyTarget();
  public
    constructor CreateAsChild(aParentOwner: TGLBaseSceneObject); reintroduce;
    property Target: TdfGameObject read FTarget write SetTarget;
    property Chaser: TdfGameObject read FChaser write SetChaser;
    property Active: Boolean read FActive write SetActive;
//    property TimeToUpdateDistance: Single read FTime write FTime;
    property Frame: TGLHUDSprite read FFrame write FFrame;
    property Aim: TGLHUDSprite read FAim write FAim;
    property LifeBar: TGLHUDSprite read FLifeBar write FLifeBar;
    property Distance: TGLHUDText read FDistance write FDistance;
    property DirectionToTarget: TGLHUDSprite read FDirection write FDirection;
    procedure AddMaterialToFrame(aTextureName: String);
    procedure AddMaterialToAim(aTextureName: String);
    procedure AddMaterialToLifeBar(aTextureName: String);
    procedure AddMaterialToDirection(aTextureName: String);
    procedure AddFontToDistance(aFont: TGLWindowsBitmapFont);
    procedure Update(deltaTime: Double);
  end;

//===============================================================
implementation
//===============================================================

uses
  uGLSceneObjects;

{ TdfTarget }

procedure TdfTarget.AddFontToDistance(aFont: TGLWindowsBitmapFont);
begin
  FDistance.BitmapFont := aFont;
  aFont.EnsureChars('0', '9');
end;

procedure TdfTarget.AddMaterialToAim(aTextureName: String);
var
  w, h: Single;
begin
  if not Assigned(dfGLSceneObjects.MatLibrary.LibMaterialByName(aTextureName)) then
    with dfGLSceneObjects.MatLibrary.Materials.Add do
    begin
      Name := aTextureName;
      with Material do
      begin
        Texture.Image.LoadFromFile(C_HUD_PATH + aTextureName);
        w := Texture.Image.Width;
        h := Texture.Image.Height;
        Texture.Enabled := True;
        Texture.TextureMode := tmModulate;
        Texture.TextureWrap := twNone;
        BlendingMode := bmTransparency;
        MaterialOptions := [moIgnoreFog, moNoLighting];
        FrontProperties.Diffuse.SetColor(1, 0, 0);
      end;
    end;
  FAim.Material.MaterialLibrary := dfGLSceneObjects.MatLibrary;
  FAim.Material.LibMaterialName := aTextureName;
  FAim.Width := w;
  FAim.Height := h;
end;

procedure TdfTarget.AddMaterialToDirection(aTextureName: String);
var
  w, h: Single;
begin
  if not Assigned(dfGLSceneObjects.MatLibrary.LibMaterialByName(aTextureName)) then
    with dfGLSceneObjects.MatLibrary.Materials.Add do
    begin
      Name := aTextureName;
      with Material do
      begin
        Texture.Image.LoadFromFile(C_HUD_PATH + aTextureName);
        w := Texture.Image.Width;
        h := Texture.Image.Height;
        Texture.Enabled := True;
        Texture.TextureMode := tmModulate;
        Texture.TextureWrap := twNone;
        BlendingMode := bmTransparency;
        MaterialOptions := [moIgnoreFog, moNoLighting];
        FrontProperties.Diffuse.SetColor(1, 0, 0, 0.7);
      end;
    end;
  FDirection.Material.MaterialLibrary := dfGLSceneObjects.MatLibrary;
  FDirection.Material.LibMaterialName := aTextureName;
  FDirection.Width := w;
  FDirection.Height := h;
end;

procedure TdfTarget.AddMaterialToFrame(aTextureName: String);
var
  w, h: Single;
begin
  if not Assigned(dfGLSceneObjects.MatLibrary.LibMaterialByName(aTextureName)) then
    with dfGLSceneObjects.MatLibrary.Materials.Add do
    begin
      Name := aTextureName;
      with Material do
      begin
        Texture.Image.LoadFromFile(C_HUD_PATH + aTextureName);
        w := Texture.Image.Width;
        h := Texture.Image.Height;
        Texture.Enabled := True;
        Texture.TextureMode := tmModulate;
        Texture.TextureWrap := twNone;
        BlendingMode := bmTransparency;
        MaterialOptions := [moIgnoreFog, moNoLighting];
        FrontProperties.Diffuse.SetColor(1, 0, 0);
      end;
    end;
  FFrame.Material.MaterialLibrary := dfGLSceneObjects.MatLibrary;
  FFrame.Material.LibMaterialName := aTextureName;
  FFrame.Width := w;
  FFrame.Height := h;
end;

procedure TdfTarget.AddMaterialToLifeBar(aTextureName: String);
var
  w, h: Single;
begin
  if not Assigned(dfGLSceneObjects.MatLibrary.LibMaterialByName(aTextureName)) then
    with dfGLSceneObjects.MatLibrary.Materials.Add do
    begin
      Name := aTextureName;
      with Material do
      begin
        Texture.Image.LoadFromFile(C_HUD_PATH + aTextureName);
        Texture.Enabled := True;
        w := Texture.Image.Width;
        h := Texture.Image.Height;
        Texture.TextureMode := tmModulate;
        Texture.TextureWrap := twNone;
        BlendingMode := bmTransparency;
        MaterialOptions := [moIgnoreFog, moNoLighting];
        FrontProperties.Diffuse.SetColor(1, 1, 1);
      end;
    end;
  FLifeBar.Material.MaterialLibrary := dfGLSceneObjects.MatLibrary;
  FLifeBar.Material.LibMaterialName := aTextureName;
  FLifeBar.Width := w;
  FLifeBar.Height := h;
  FLifeBarBaseWidth := FLifeBar.Width;
end;

constructor TdfTarget.CreateAsChild(aParentOwner: TGLBaseSceneObject);
begin
  inherited;
  FFrame := TGLHUDSprite.CreateAsChild(Self);
  FAim := TGLHUDSprite.CreateAsChild(Self);
  FLifeBar := TGLHUDSprite.CreateAsChild(Self);
  FDirection := TGLHUDSprite.CreateAsChild(Self);
  FDistance := TGLHUDText.CreateAsChild(Self);

  wndCenterX := dfGLSceneObjects.Viewer.Width div 2;
  wndCenterY := dfGLSceneObjects.Viewer.Height div 2;

  FDistance.Alignment := taCenter;
  FDistance.Layout := tlCenter;
  FDistance.ModulateColor.SetColor(1.0, 0.0, 0.0);
  FDistance.Scale.Scale(C_DISTANCE_SCALE);
end;

function TdfTarget.GetInterceptionPoint(q: Integer; posObject, posWeapon,
  velObject: TVector; velWeapon: Single): TVector;
var
   invSpeed, t: Single;
   targEst: TVector;
begin
   invSpeed := 1 / (velWeapon + 0.1);
   t := FDistanceValue * invSpeed;
   if q >= 1 then
   begin
     targEst := VectorCombine(posObject, velObject, 1, t);
     t := VectorDistance(posWeapon, targEst) * invSpeed;
     if q >= 2 then
     begin
       targEst := VectorCombine(posObject, velObject, 1, t);
       t := VectorDistance(posWeapon, targEst) * invSpeed;
     end;
   end;
   Result := VectorCombine(posObject, velObject, 1, t);
end;

procedure TdfTarget.SetActive(const Value: Boolean);
begin
  FActive := Value;

  FFrame.Visible := Value;
  FDirection.Visible := Value;
  FDistance.Visible := Value;
  FAim.Visible := Value;
  FLifeBar.Visible := Value;
end;

procedure TdfTarget.SetAllyTarget;
begin
  FFrame.Material.GetActualPrimaryMaterial.FrontProperties.Diffuse.SetColor(0,1,0);
  FDirection.Material.GetActualPrimaryMaterial.FrontProperties.Diffuse.SetColor(0,1,0);
  FDistance.ModulateColor.SetColor(0,1,0);
//  FAim.Visible := False;
//  FLifeBar.Visible := True;
  FNoAim := True;
  FNoLifeBar := False;
end;

procedure TdfTarget.SetEnemyTarget;
begin
  FFrame.Material.GetActualPrimaryMaterial.FrontProperties.Diffuse.SetColor(1,0,0);
  FDirection.Material.GetActualPrimaryMaterial.FrontProperties.Diffuse.SetColor(1,0,0);
  FDistance.ModulateColor.SetColor(1,0,0);
//  FAim.Visible := True;
//  FLifeBar.Visible := True;
  FNoAim := False;
  FNoLifeBar := False;
end;

procedure TdfTarget.SetNeutralTarget;
begin
  FFrame.Material.GetActualPrimaryMaterial.FrontProperties.Diffuse.SetColor(1,1,1);
  FDirection.Material.GetActualPrimaryMaterial.FrontProperties.Diffuse.SetColor(1,1,1);
  FDistance.ModulateColor.SetColor(1,0,0);
//  FAim.Visible := False;
//  FLifeBar.Visible := False;
  FNoAim := True;
  FNoLifeBar := True;
end;

procedure TdfTarget.SetChaser(const Value: TdfGameObject);
begin
  FChaser := Value;
  Active := Assigned(FTarget) and Assigned (FChaser);
end;

procedure TdfTarget.SetTarget(const Value: TdfGameObject);
begin
  FTarget := Value;
  Active := Assigned(FTarget) and Assigned (FChaser);

  if Assigned(FTarget) then
    with FFrame.Material.GetActualPrimaryMaterial.FrontProperties.Diffuse do
      case FTarget.GroupID of

        C_GROUP_OBJECT: SetNeutralTarget;
        C_GROUP_NEUTRALS: SetNeutralTarget;
        C_GROUP_ALLIES: SetAllyTarget;
        C_GROUP_ENEMIES: SetEnemyTarget;
      end;

end;

procedure TdfTarget.Update(deltaTime: Double);

  function IsValueBetween(aValue, aMin, aMax: Single): Boolean;
  begin
    Result := (aValue > aMin) and (aValue < aMax);
  end;

begin
  if FActive and Assigned(FTarget) and Assigned(FChaser) then
  begin
    {Рамка}
    FFrame.Position.AsVector :=
      dfGLSceneObjects.Viewer.Buffer.WorldToScreen(FTarget.AbsolutePosition);
    FFrame.Position.Y := dfGLSceneObjects.Viewer.Height - FFrame.Position.Y;

    {Дистанция}
    FDistanceValue := VectorDistance(FChaser.AbsolutePosition, FTarget.AbsolutePosition);
    FDistance.Text := FloatToStrF(FDistanceValue, ffGeneral, 4, 3);
    FDistance.Position := FFrame.Position;
    FDistance.Position.X := FDistance.Position.X + C_DISTANCE_OFFSET_X;
    FDistance.Position.Y := FDistance.Position.Y + C_DISTANCE_OFFSET_Y;

    {Лайфбар}
    if not FNoLifeBar then
    begin
      FLifeBar.Visible := True;
      FLifeBar.Position := FFrame.Position;
      FLifeBar.Position.X := FLifeBar.Position.X + C_LIFEBAR_OFFSET_X;
      FLifeBar.Position.Y := FLifeBar.Position.Y + C_LIFEBAR_OFFSET_Y;
      FLifeBar.Width := FLifeBarBaseWidth * FTarget.Health / FTarget.MaxHealth;
    end
    else
      FLifeBar.Visible := False;

    {Прицел}
    if not FNoAim then
    begin
      FAim.Visible := FDistanceValue < TdfSpaceFighter(FChaser).MaxWeaponDistance;
      intPoint := GetInterceptionPoint(2, FTarget.AbsolutePosition,
        FChaser.AbsolutePosition, VectorScale(FTarget.AbsoluteDirection, FTarget.Speed),
          TdfSpaceFighter(FChaser).AverageWeaponSpeed);
      FAim.Position.AsVector := dfGLSceneObjects.Viewer.Buffer.WorldToScreen(intPoint);
      FAim.Position.Y := dfGLSceneObjects.Viewer.Height - FAim.Position.Y;
    end
    else
      FAim.Visible := False;

    {Направление}
    FDirection.Visible := True;
    targetDir := VectorSubtract(FTarget.AbsolutePosition, dfGLSceneObjects.Camera.AbsolutePosition);
    NormalizeVector(targetDir);
    if VectorDotProduct(dfGLSceneObjects.Camera.AbsoluteDirection, targetDir) > 0 then
    begin
      if IsValueBetween(FFrame.Position.X, 0, dfGLSceneObjects.Viewer.Width)
        and IsValueBetween(FFrame.Position.Y, 0, dfGLSceneObjects.Viewer.Height) then
        FDirection.Visible := False
      else
      begin
        FDirection.Position.X := ClampValue(FFrame.Position.X,
          FDirection.Width / 2, dfGLSceneObjects.Viewer.Width - FDirection.Width / 2);
        FDirection.Position.Y := ClampValue(FFrame.Position.Y,
          FDirection.Height / 2, dfGLSceneObjects.Viewer.Height - FDirection.Height / 2);
      end;
    end
    else
    begin
      targetDir2.X := - (FFrame.Position.X - wndCenterX);
      targetDir2.Y := - (FFrame.Position.Y - wndCenterY);
      NormalizeVector(targetDir2);
      ScaleVector(targetDir2, 1000);
      FDirection.Position.X := ClampValue(targetDir2.X,
        FDirection.Width / 2, dfGLSceneObjects.Viewer.Width - FDirection.Width / 2);
      FDirection.Position.Y := ClampValue(targetDir2.Y,
        FDirection.Height / 2, dfGLSceneObjects.Viewer.Height - FDirection.Height / 2);
    end;
  end
  else
    if not Assigned(FTarget) then
      Active := False;

end;

end.
