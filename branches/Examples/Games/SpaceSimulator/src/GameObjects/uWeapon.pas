unit uWeapon;

{TODO: аккумулятор, процедура стрельбы, апдейт, установка эффектов на оружие и пули}

interface

uses
  System.SysUtils,
  System.IniFiles,

  GLScene,
  GLObjects,
  GLParticleFX,
  GLVectorGeometry,
  GLVectorTypes,
  GLTexture,
  GLFilePNG,
  GLMaterial,
  uBulletAccum;

const
  C_WEAPONS_PATH = 'data\weapons\';
  C_DEF_CAPACITY = 30;

type
  TdfWeapon = class(TGLDummyCube)
  private
    FWClass: Integer; //Weapon Class
    FName, FDesc: String;
    FMinDamage, FMaxDamage, FDistance,
    FVelocity, FFireRate: Single;
    FPFXType: Integer;
    FIsShooting: Boolean;
    FTimePerShot: Single; //Буквально - время на один выстрел, 1 / FireRate
    FDeltaShot: Single; //локальная переменная для учета TimePerShot
    FBulletAccum: TdfBulletAccum;
    FBullet: TdfBullet;
    FFireDirection: TAffineVector;
    FUserGroup: Integer;
    procedure LaunchBullet;
  public
    constructor CreateAsChild(aParentOwner: TGLBaseSceneObject); reintroduce;
    procedure LoadFromFile(aFile: String);
    property WClass: Integer read FWClass;
    property WName: String read FName;
    property Description: String read FDesc;
    property MinDamage: Single read FMinDamage;
    property MaxDamage: Single read FMaxDamage;
    property Distance:  Single read FDistance;
    property Velocity:  Single read FVelocity;
    property FireRate:  Single read FFireRate;
    property PFXType: Integer read FPFXType;
    procedure SetEffect(aPFXManager: TGLParticleFXManager);
//    procedure SetBulletAcum(aParentOwner: TGLBaseSceneObject;
//      aCapacity: Integer = C_DEF_CAPACITY);
    procedure SetBulletEffect(aPFXManager: TGLParticleFXManager);
    procedure Fire(aDirection: TAffineVector);
    procedure Update(deltaTime: Double);
    procedure ReleaseAllBullets();
    procedure SetPhysUserGroup(aGroup: Integer);
  end;

//========================================================================
implementation
//========================================================================

uses
  uLog, uGLSceneObjects;

{ TdfWeapon }

constructor TdfWeapon.CreateAsChild(aParentOwner: TGLBaseSceneObject);
begin
  inherited;
  FBulletAccum := TdfBulletAccum.CreateAsChild(dfGLSceneObjects.Scene.Objects);
//  VisibleAtRunTime := True;
end;

const
  C_MAIN = 'Main';
  C_GRAPHIC = 'Graphic';
  C_TTX = 'TTX';
  C_DEF_STRING = '<no value>';

procedure TdfWeapon.Fire(aDirection: TAffineVector);
begin
  FFireDirection := aDirection;
  FIsShooting := True;
  {Включить менеджер частиц}
end;

procedure TdfWeapon.LaunchBullet;
begin
  FBullet := FBulletAccum.GetFreeBullet();
  FBullet.Direction.AsAffineVector := FFireDirection;
  FBullet.Position.AsVector := Self.AbsolutePosition;
  FBullet.LaunchedPosition := FBullet.Position.AsVector;
  FBullet.Damage := FMinDamage + (FMaxDamage - FMinDamage) * Random(100)/100;
end;

procedure TdfWeapon.LoadFromFile(aFile: String);

  procedure AddBulletMaterial(aName, aTexturePath: String; aColor: TVector);
  begin
    if Assigned(dfGLSceneObjects.MatLibrary.LibMaterialByName(aName)) then
      Exit;
    with dfGLSceneObjects.MatLibrary.Materials.Add do
    begin
      Name := aName;
      with Material do
      begin
        Texture.Image.LoadFromFile(C_WEAPONS_PATH + aTexturePath);
        Texture.Enabled := True;
        Texture.TextureMode := tmModulate;
        BlendingMode := bmTransparency;
        MaterialOptions := [moNoLighting];
        FrontProperties.Diffuse.Color := aColor;
        DepthProperties.DepthTest := True;
        DepthProperties.DepthWrite := False;
      end;
    end;
  end;

var
  Ini: TIniFile;
  colorVec: TVector;
  w, h: Single;
  accumCapacity: Integer;
begin
  if FileExists(C_WEAPONS_PATH + aFile) then
  begin
    Ini := TIniFile.Create(C_WEAPONS_PATH + aFile);

    {Main}
    FWClass := Ini.ReadInteger(C_MAIN, 'Class', -1);
    FName := Ini.ReadString(C_MAIN, 'Name', C_DEF_STRING);
    FDesc := Ini.ReadString(C_MAIN, 'Desc', C_DEF_STRING);

    {TTX}
    FMinDamage := Ini.ReadFloat(C_TTX, 'MinDamage', 0.0);
    FMaxDamage := Ini.ReadFloat(C_TTX, 'MaxDamage', 0.0);
    FDistance  := Ini.ReadFloat(C_TTX, 'Distance', 0.0);
    FVelocity  := Ini.ReadFloat(C_TTX, 'Velocity', 0.0);
    FFireRate  := Ini.ReadFloat(C_TTX, 'FireRate', 0.0);

    FTimePerShot := 1 / FFireRate;

    //Рассчитываем емкость аккумулятора
    //Даже если ошибемся - не беда, он умеет расширяться
    accumCapacity := Round((FDistance / FVelocity) * FFireRate) + 1;

    {Graphic}
    FPFXType := Ini.ReadInteger(C_GRAPHIC, 'PFXType', 0);

    colorVec.X := Ini.ReadFloat(C_GRAPHIC, 'ColorR', 1.0);
    colorVec.Y := Ini.ReadFloat(C_GRAPHIC, 'ColorG', 1.0);
    colorVec.Z := Ini.ReadFloat(C_GRAPHIC, 'ColorB', 1.0);
    colorVec.Z := Ini.ReadFloat(C_GRAPHIC, 'ColorA', 1.0);

    AddBulletMaterial('bullet'+IntToStr(PFXType),
      Ini.ReadString(C_GRAPHIC, 'TextureFile', C_DEF_STRING),
      colorVec);
    FBulletAccum.Init(accumCapacity, dfGLSceneObjects.MatLibrary, 'bullet'+IntToStr(PFXType), FUserGroup);

    w := Ini.ReadFloat(C_GRAPHIC, 'Width', 16.0);
    h := Ini.ReadFloat(C_GRAPHIC, 'Height', 16.0);

    FBulletAccum.Bullet.SetSize(w, h);
    FBulletAccum.BulletsChanged;

    logWriteMessage('TdfWeapon: Weapon "' + FName + '" loaded from file ' + aFile);
    Ini.Free;
  end
  else
    logWriteError('TdfWeapon: File ' + aFile + ' not found');
end;

procedure TdfWeapon.ReleaseAllBullets;
begin
  FBulletAccum.FreeAll();
end;

procedure TdfWeapon.SetBulletEffect(aPFXManager: TGLParticleFXManager);
begin
  //*
end;

procedure TdfWeapon.SetEffect(aPFXManager: TGLParticleFXManager);
begin
  //*
end;

procedure TdfWeapon.SetPhysUserGroup(aGroup: Integer);
begin
  FUserGroup := aGroup;
end;

procedure TdfWeapon.Update(deltaTime: Double);
var
  i: Integer;
begin
  if FIsShooting then
  begin
    FDeltaShot := FDeltaShot + deltaTime;
    if FDeltaShot >= FTimePerShot then
    begin
      LaunchBullet();
      FIsShooting := False;
      FDeltaShot := 0;
    end;
  end
  else if FDeltaShot < FTimePerShot then
    FDeltaShot := FDeltaShot + deltaTime;

  //обновляем движение снарядов
  for i := 0 to FBulletAccum.Count - 1 do
    with TdfBullet(FBulletAccum[i]) do
      if Used then
      begin
        Move(deltaTime * Velocity);
        if VectorDistance(AbsolutePosition, LaunchedPosition) > Distance then
          FBulletAccum.FreeBullet(i);
      end;

end;

end.
