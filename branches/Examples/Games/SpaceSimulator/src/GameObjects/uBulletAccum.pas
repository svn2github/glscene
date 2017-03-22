{Классы аккумулятора для каких-либо объектов}
unit uBulletAccum;

interface

uses
  GLScene, GLObjects, GLMaterial, GLVectorGeometry, GLVectorTypes,

  uSimplePhysics;

type

  TdfBullet = class(TGLSprite)
  private
    FUsed: Boolean;
    FLaunchedPos: TVector;
    FPhys: TdfPhys;
    FDamage: Single;
    procedure SetUsed(const aUsed: Boolean);
  public
    constructor CreateAsChild(aParentOwner: TGLBaseSceneObject); reintroduce;

    property Used: Boolean read FUsed write SetUsed;
    property LaunchedPosition: TVector read FLaunchedPos write FLaunchedPos;
    property Phys: TdfPhys read FPhys write FPhys;
    property Damage: Single read FDamage write FDamage;
  end;

  TdfBulletAccum = class (TGLDummyCube)
  private
    FFreeBullet: TdfBullet;
    function GetBullet: TdfBullet;
  public
    constructor CreateAsChild(aParentOwner: TGLBaseSceneObject); reintroduce;

    procedure Init(aCapacity: Integer; aMatLib: TGLMaterialLibrary;
      aMatName: String; aPhysUserGroup: Integer);
    procedure Expand();

    //Используется для указания свойств пули
    property Bullet: TdfBullet read GetBullet;
    //После указания свойств необходимо вызвать эту процедуру
    procedure BulletsChanged;

    function GetFreeBullet: TdfBullet;
    procedure FreeBullet(aBullet: TdfBullet); overload;
    procedure FreeBullet(aIndex: Integer); overload;
    procedure FreeAll();
  end;

implementation

uses
  uDebugInfo;

{ TdfBullet }

constructor TdfBullet.CreateAsChild(aParentOwner: TGLBaseSceneObject);
begin
  inherited;
  FPhys := dfPhysics.AddPhysToObject(Self, psSphere);

  Used := False;
end;

procedure TdfBullet.SetUsed(const aUsed: Boolean);
begin
  FUsed := aUsed;
  Visible := aUsed;
  FPhys.Enabled := aUsed;
end;

{ TdfBulletAccum }

procedure TdfBulletAccum.BulletsChanged;
var
  i: Integer;
begin
  for i := 1 to Count - 1 do
    with TdfBullet(Children[i]) do
    begin
      Width := TdfBullet(Self.Children[0]).Width;
      Height := TdfBullet(Self.Children[0]).Height;
      Rotation := TdfBullet(Self.Children[0]).Rotation;
      Effects.Assign(TdfBullet(Self.Children[0]).Effects);
    end;
end;

constructor TdfBulletAccum.CreateAsChild(aParentOwner: TGLBaseSceneObject);
begin
  inherited;
end;

procedure TdfBulletAccum.Expand;
var
  newSize, i: Integer;
begin
  newSize := Count div 2;
  if newSize < 8 then
    newSize := 8;
  for i := 0 to newSize - 1 do
    with TdfBullet.CreateAsChild(Self) do
    begin
      Material.MaterialLibrary := TdfBullet(Self.Children[0]).Material.MaterialLibrary;
      Material.LibMaterialName := TdfBullet(Self.Children[0]).Material.LibMaterialName;
    end;
end;

procedure TdfBulletAccum.FreeAll;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    TdfBullet(Children[i]).Used := False;
end;

procedure TdfBulletAccum.FreeBullet(aIndex: Integer);
begin
  if aIndex < Count then
    FreeBullet(TdfBullet(Children[aIndex]));
end;

procedure TdfBulletAccum.FreeBullet(aBullet: TdfBullet);
begin
  aBullet.Used := False;
end;

function TdfBulletAccum.GetBullet: TdfBullet;
begin
  Result := TdfBullet(Children[0]);
end;

function TdfBulletAccum.GetFreeBullet: TdfBullet;
var
  i: Integer;
begin
  FFreeBullet := nil;
  for i := 0 to Count - 1 do
  begin
    FFreeBullet := TdfBullet(Children[i]);
    if not FFreeBullet.Used then
    begin
      FFreeBullet.Used := True;
      Result := FFreeBullet;
      Exit;
    end;
  end;
  i := Count - 1;
  Expand();
  FFreeBullet := TdfBullet(Children[i + 1]);
  FFreeBullet.Used := True;
  Result := FFreeBullet;
end;

procedure TdfBulletAccum.Init(aCapacity: Integer; aMatLib: TGLMaterialLibrary;
  aMatName: String; aPhysUserGroup: Integer);
var
  i: Integer;
begin
  for i := 0 to aCapacity - 1 do
    with TdfBullet.CreateAsChild(Self) do
    begin
      Material.MaterialLibrary := aMatLib;
      Material.LibMaterialName := aMatName;
      Phys.UserGroup := aPhysUserGroup;
      Phys.UserType := C_PHYS_BULLET;
    end;
end;

end.
