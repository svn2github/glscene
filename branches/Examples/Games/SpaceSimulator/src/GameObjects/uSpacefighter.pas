unit uSpacefighter;

interface

uses
  IniFiles, SysUtils, Contnrs,

  GLScene, GLObjects, GLVectorFileObjects, GLMaterial, GLTexture,
  GLFile3DS, GLVectorGeometry, GLVectorTypes, GLFilePNG,

  uGameObject, uLog, uEngine, uWeapon, uSimplePhysics, uDebugInfo;

const
  C_SPACESHIPS_PATH = 'data\spaceships\';

type
  TdfSpaceFighter = class (TdfGameObject)
  protected
    FMatLib: TGLMaterialLibrary;

    FFighter: TGLFreeForm;
    FShipName, FShipDesc: String;

    FEngines: array of TdfEngine;
    FWeapons: array of TdfWeapon;

    FEnginesCount, FWeaponsCount: Integer;

    FAverageWeaponSpeed, FMaxWeaponDistanse: Single;

    FIntObjects: TObjectList;

    FPhys: TdfPhys;

    function GetEngine(aIndex: Integer): TdfEngine;
    procedure SetEngine(aIndex: Integer; const aEngine: TdfEngine);

    function GetWeapon(aIndex: Integer): TdfWeapon;
    procedure SetWeapon(aIndex: Integer; const aWeapon: TdfWeapon);

    procedure SetFighterMaterial(texturePath: String); virtual;
    procedure SetVisible(aValue: Boolean); override;
  public
    constructor CreateAsChild(aParent: TGLBaseSceneObject); reintroduce;
    destructor Destroy; override;

    procedure Update(deltaTime: Double); override;

    procedure Accelerate(Value: Single);

    procedure LoadFromFile(aFile: String);

    property ShipName: String read FShipName;
    property ShipDescription: String read FShipDesc;

    property Fighter: TGLFreeForm read FFighter write FFighter;

    property Engines[Index: Integer]: TdfEngine read GetEngine write SetEngine;
    property EnginesCount: Integer read FEnginesCount;
    property Weapons[Index: Integer]: TdfWeapon read GetWeapon write SetWeapon;
    property WeaponsCount: Integer read FWeaponsCount;

    property AverageWeaponSpeed: Single read FAverageWeaponSpeed;
    property MaxWeaponDistance: Single read FMaxWeaponDistanse;

    property ObjectsAround: TObjectList read FIntObjects write FIntObjects;
    property Phys: TdfPhys read FPhys write FPhys;
  end;

implementation

uses
  uGLSceneObjects;

{ TdfSpaceFighter }

procedure TdfSpaceFighter.Accelerate(Value: Single);
begin
  FSpeed := FSpeed + FMaxAccel * Value;
  if FSpeed > FMaxSpeed then
    FSpeed := FMaxSpeed
  else
    if FSpeed < 0.0 then
     FSpeed := 0.0;
end;

constructor TdfSpaceFighter.CreateAsChild(aParent: TGLBaseSceneObject);
begin
  inherited;
  FMatLib := dfGLSceneObjects.MatLibrary;
  FFighter := TGLFreeForm.CreateAsChild(Self);
  FIntObjects := TObjectList.Create(False);
end;

destructor TdfSpaceFighter.Destroy;
begin
  FIntObjects.Free;
  inherited;
end;


function TdfSpaceFighter.GetEngine(aIndex: Integer): TdfEngine;
begin
  Result := FEngines[aIndex];
end;

function TdfSpaceFighter.GetWeapon(aIndex: Integer): TdfWeapon;
begin
  Result := FWeapons[aIndex];
end;

const
  //Имена секций ini-файла
  C_MAIN = 'Main';
  C_ENGINE = 'Engine';
  C_WEAPON = 'Weapon';

  C_DEF_STRING = '<no value>';

procedure TdfSpaceFighter.LoadFromFile(aFile: String);
var
  Ini: TIniFile;
  i, iUserGroup: Integer;
  oX, oY, oZ: Single; //offsets
  meshFile, textureFile: String;
  pfxType: Integer;
  SummSpeed: Single;
begin
  if FileExists(C_SPACESHIPS_PATH + aFile) then
  begin
    Ini := TIniFile.Create(C_SPACESHIPS_PATH + aFile);
    {Базовые параметры }
    FShipName := Ini.ReadString(C_MAIN, 'Name', C_DEF_STRING);
    FShipDesc := Ini.ReadString(C_MAIN, 'Desc', C_DEF_STRING);

    FMaxSpeed := Ini.ReadFloat(C_MAIN, 'MaxSpeed', 0.0);
    FMaxAccel := Ini.ReadFloat(C_MAIN, 'MaxAccel', 0.0);
    FMaxHealth := Ini.ReadFloat(C_MAIN, 'MaxHealth', 1.0);
    FHealth := FMaxHealth;

    FEnginesCount := Ini.ReadInteger(C_MAIN, 'EnginesCount', 0);
    if FEnginesCount <= 0 then
      logWriteError('TdfSpaceFighter: Engines count less then 1 in ' + aFile);
    FWeaponsCount := Ini.ReadInteger(C_MAIN, 'WeaponsCount', 0);
    if FWeaponsCount <= 0 then
      logWriteError('TdfSpaceFighter: Weapons count less then 1 in ' + aFile);
    SetLength(FEngines, FEnginesCount);
    SetLength(FWeapons, FWeaponsCount);

    meshFile := Ini.ReadString(C_MAIN, 'MeshFile', C_DEF_STRING);
    if meshFile = C_DEF_STRING then
      logWriteError('TdfSpaceFighter: no "MeshFile" property in ' + aFile);
    textureFile := Ini.ReadString(C_MAIN, 'TextureFile', C_DEF_STRING);
    if textureFile = C_DEF_STRING then
      logWriteError('TdfSpaceFighter: no "TextureFile" property in ' + aFile);
    SetFighterMaterial(textureFile);
    FFighter.UseMeshMaterials := False;
    FFighter.LoadFromFile(C_SPACESHIPS_PATH + meshFile);

    for i := 0 to FEnginesCount - 1 do
    begin
      FEngines[i] := TdfEngine.CreateAsChild(FFighter);
      oX := Ini.ReadFloat(C_ENGINE + IntToStr(i), 'OffsetX', 0.0);
      oY := Ini.ReadFloat(C_ENGINE + IntToStr(i), 'OffsetY', 0.0);
      oZ := Ini.ReadFloat(C_ENGINE + IntToStr(i), 'OffsetZ', 0.0);
      pfxType := Ini.ReadInteger(C_ENGINE + IntToStr(i), 'PFXType', 0);
      FEngines[i].Position.SetPoint(oX, oY, oZ);
      if pfxType >= uGLSceneObjects.C_ENGINEFX_COUNT then
        logWriteError('TdfSpaceFighter: No ParticleFXManager with index ' +
          IntToStr(pfxType) + '. Last index is ' +
          IntToStr(uGLSceneObjects.C_ENGINEFX_COUNT))
      else
        FEngines[i].SetEffect(dfGLSceneObjects.EnginesFX[pfxType]);
    end;

    {Устанавливаем физику}
    FPhys :=  dfPhysics.AddPhysToObject(Self, psSphere);
    with FPhys do
    begin
      BSRadius := FFighter.BoundingSphereRadius;
      UserType := C_PHYS_DESTROYABLE;
      UserGroup := GetFreeUserGroup();
      iUserGroup := UserGroup;
    end;

    {Оружие}
    SummSpeed := 0;
    FMaxWeaponDistanse := 0;
    for i := 0 to FWeaponsCount - 1 do
    begin
      FWeapons[i] := TdfWeapon.CreateAsChild(FFighter);
      oX := Ini.ReadFloat(C_WEAPON + IntToStr(i), 'OffsetX', 0.0);
      oY := Ini.ReadFloat(C_WEAPON + IntToStr(i), 'OffsetY', 0.0);
      oZ := Ini.ReadFloat(C_WEAPON + IntToStr(i), 'OffsetZ', 0.0);
      FWeapons[i].Position.SetPoint(oX, oY, oZ);
      FWeapons[i].SetPhysUserGroup(iUserGroup);
      FWeapons[i].LoadFromFile(Ini.ReadString(C_WEAPON + IntToStr(i), 'FileName', C_DEF_STRING));
      SummSpeed := SummSpeed + FWeapons[i].Velocity;
      if FWeapons[i].Distance > FMaxWeaponDistanse then
        FMaxWeaponDistanse := FWeapons[i].Distance;
    end;
    FAverageWeaponSpeed := SummSpeed / FWeaponsCount;

    logWriteMessage('TdfSpaceFighter: Spacefighter "' + FShipName + '" loaded from file ' + aFile);
    Ini.Free;
  end
  else
    logWriteError('TdfSpaceFighter: file ' + aFile + ' not found!');
end;

procedure TdfSpaceFighter.SetEngine(aIndex: Integer; const aEngine: TdfEngine);
begin
  FEngines[aIndex] := aEngine;
end;

procedure TdfSpaceFighter.SetFighterMaterial(texturePath: String);
begin
  if not Assigned(FMatLib.LibMaterialByName(FShipName)) then
    with FMatLib.Materials.Add do
    begin
      Name := FShipName;
      with Material do
      begin
        if texturePath <> '' then
        begin
          Texture.Image.LoadFromFile(C_SPACESHIPS_PATH + texturePath);
          Texture.Enabled := True;
          Texture.TextureMode := tmReplace;
        end;
        BlendingMode := bmOpaque;
        MaterialOptions := [moNoLighting];
        FrontProperties.Diffuse.SetColor(1.0, 1.0, 1.0);
      end;
    end;
  FFighter.Material.MaterialLibrary := FMatLib;
  FFighter.Material.LibMaterialName := FShipName;
end;

procedure TdfSpaceFighter.SetVisible(aValue: Boolean);
var
  i: Integer;
begin
  inherited;
  for i := 0 to FEnginesCount - 1 do
    FEngines[i].Visible := aValue;
end;

procedure TdfSpaceFighter.SetWeapon(aIndex: Integer; const aWeapon: TdfWeapon);
begin
  FWeapons[aIndex] := aWeapon;
end;

procedure TdfSpaceFighter.Update(deltaTime: Double);
var
  i: Integer;
begin
  inherited;
  Self.Move(deltaTime * Speed);
  for i := 0 to FWeaponsCount - 1 do
    FWeapons[i].Update(deltaTime);
end;

end.
