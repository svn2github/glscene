unit uGameObject;

interface

uses
  GLScene, GLObjects;

const
  C_GROUP_OBJECT = -1;
  C_GROUP_NEUTRALS = 0;
  C_GROUP_ALLIES = 1;
  C_GROUP_ENEMIES = 2;

type
  TdfGameObject = class;

  TdfDieNotify = procedure(Sender: TdfGameObject) of object;

  TdfGameObject = class (TGLDummyCube)
  private
    FOnDie: TdfDieNotify;
  protected
    FHealth, FMaxHealth: Single;
    FSpeed, FMaxSpeed, FMaxAccel: Single;
    FGroupID: Integer;
    procedure SetHealth(const Value: Single); virtual;
  public
    property Health: Single read FHealth write SetHealth;
    property MaxHealth: Single read FMaxHealth;

    property Speed: Single read FSpeed write FSpeed;
    property MaxSpeed: Single read FMaxSpeed write FMaxSpeed;
    property MaxAccelerate: Single read FMaxAccel write FMaxAccel;
    property GroupID: Integer read FGroupID write FGroupID;

    property OnDie: TdfDieNotify read FOnDie write FOnDie;

    procedure Update(deltaTime: Double); virtual;
    procedure TakeDamage(aDamage: Single);

    procedure ResetParams();
  end;

implementation

{ TdfGameObject }

procedure TdfGameObject.ResetParams;
begin
  Position.SetPoint(0, 0, 0);
  ResetRotations();
  Speed := 0;
  Health := MaxHealth;
end;

procedure TdfGameObject.SetHealth(const Value: Single);
begin
  FHealth := Value;
  if FHealth <= 0 then
    if Assigned(FOnDie) then
      FOnDie(Self);
end;

procedure TdfGameObject.TakeDamage(aDamage: Single);
begin
  Health := Health - aDamage;
end;

procedure TdfGameObject.Update(deltaTime: Double);
begin
  //нифига
end;

end.
