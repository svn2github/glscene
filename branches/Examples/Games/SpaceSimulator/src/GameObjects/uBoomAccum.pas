unit uBoomAccum;

interface

uses
  GLScene, GLObjects, GLVectorGeometry, GLVectorTypes, GLParticleFX;

type

  TdfBoomProperties = record
    sPositionDispersion, sParticleInterval, sVelocityDispersion: Single;
  end;

  TdfBoom = class (TGLDummyCube)
  private
    FUsed: Boolean;
    FTime, FTimeToDie: Single;
    procedure SetUsed(const Value: Boolean);
  public
    constructor CreateAsChild(aParentOwner: TGLBaseSceneObject); reintroduce;

    property Used: Boolean read FUsed write SetUsed;

    procedure Update(deltaTime: Double);
  end;

  TdfBoomAccum = class (TGLDummyCube)
  private
    {+debug}
//    iCapacity, iUsed: Integer;
    {-debug}
    FFreeBoom: TdfBoom;
    FProp: TdfBoomProperties;
    FPFX: TGLParticleFXManager;
    procedure AddBoomEffect(aObj: TdfBoom);
  public
    procedure Init(aCapacity: Integer; aPFXMan: TGLParticleFXManager;
      aProp: TdfBoomProperties);
    procedure Expand();

    procedure SetBoom(aPos: TAffineVector; aTime: Single);
    function GetFreeBoom: TdfBoom;

    procedure Update(deltaTime: Double);
  end;

implementation

uses
  uDebugInfo;

{ TdfBoom }

constructor TdfBoom.CreateAsChild(aParentOwner: TGLBaseSceneObject);
begin
  inherited;
  FTime := 0;
  FTimeToDie := 0;
  Used := False;
end;

procedure TdfBoom.SetUsed(const Value: Boolean);
begin
  FUsed := Value;
  Visible := Value;
  if Effects.Count > 0 then
    TGLSourcePFXEffect(Effects[0]).Enabled := Value;
end;

procedure TdfBoom.Update(deltaTime: Double);
begin
  FTime := FTime + deltaTime;
  if FTime >= FTimeToDie then
  begin
    FTime := 0;
    FTimeToDie := 0;
    Used := False;
  end
  else
  begin
    //* Что-то делаем
  end;
end;

{ TdfBoomAccum }

procedure TdfBoomAccum.AddBoomEffect(aObj: TdfBoom);
begin
  with TGLSourcePFXEffect(aObj.Effects.GetOrCreate(TGLSourcePFXEffect)) do
  begin
    Manager := FPFX;
    Enabled := True;
    PositionDispersion := FProp.sPositionDispersion; //1.2;
    ParticleInterval := FProp.sParticleInterval; //0.01;
    VelocityDispersion := FProp.sVelocityDispersion; //10.2;
  end;
end;

procedure TdfBoomAccum.Expand;
var
  newSize, i: Integer;
begin
  newSize := Count div 2;
  if newSize < 8 then
    newSize := 8;
  for i := 0 to newSize - 1 do
  begin
    FFreeBoom := TdfBoom.CreateAsChild(Self);
    AddBoomEffect(FFreeBoom);
    FFreeBoom.Used := False;
  end;
//  dfDebugInfo.UpdateParam(iCapacity, Self.Count);
end;

function TdfBoomAccum.GetFreeBoom: TdfBoom;
var
  i: Integer;
begin
  FFreeBoom := nil;
  for i := 0 to Count - 1 do
  begin
    FFreeBoom := TdfBoom(Children[i]);
    if not FFreeBoom.Used then
    begin
      FFreeBoom.Used := True;
      Result := FFreeBoom;
      Exit;
    end;
  end;
  i := Count - 1;
  Expand();
  FFreeBoom := TdfBoom(Children[i + 1]);
  FFreeBoom.Used := True;
  Result := FFreeBoom;
end;

procedure TdfBoomAccum.Init(aCapacity: Integer; aPFXMan: TGLParticleFXManager;
  aProp: TdfBoomProperties);
var
  i: Integer;
begin
  FPFX := aPFXMan;
  FProp := aProp;
  for i := 0 to aCapacity - 1 do
  begin
    FFreeBoom := TdfBoom.CreateAsChild(Self);
    AddBoomEffect(FFreeBoom);
    FFreeBoom.Used := False;
  end;

  {+debug}
//  iCapacity := dfDebugInfo.AddNewString('Аккумулятор взрывов - всего объектов');
//  iUsed := dfDebugInfo.AddNewString('Аккумулятор взрывов - используется');
  {-debug}

//  dfDebugInfo.UpdateParam(iCapacity, Self.Count);
end;

procedure TdfBoomAccum.SetBoom(aPos: TAffineVector; aTime: Single);
var
  Boom: TdfBoom;
begin
  Boom := GetFreeBoom();
  Boom.FTime := 0;
  Boom.FTimeToDie := aTime;
  Boom.Position.SetPoint(aPos);
  Boom.Used := True;
end;

procedure TdfBoomAccum.Update(deltaTime: Double);
var
  i: Integer;
//  a: Integer;
begin
//  a := 0;
  for i := 0 to Count - 1 do
    with TdfBoom(Children[i]) do
      if Used then
      begin
//        Inc(a);
        Update(deltaTime);
      end;
//  dfDebugInfo.UpdateParam(iUsed, a);
end;

end.
