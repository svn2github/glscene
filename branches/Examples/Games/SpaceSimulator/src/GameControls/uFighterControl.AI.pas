unit uFighterControl.AI;

interface

uses
  GLVectorGeometry, GLVectorTypes,

  uSpaceFighter, uFighterControl;


const
  C_SPACEFIGHTER_MAXROLL = 45;

type
  TdfAIControl = class(TdfFighterControl)
  private
    FT: Single;
    FDeltaTime: Single;
//    FX, FY: Integer;
    dmx, dmy: Single;
    sf_dir: TAffineVector;
  protected
    FEnabled: Boolean;
    FSpaceFighter: TdfSpaceFighter;
    procedure Shoot; override;
    procedure SetMouseMovement; override;
    procedure Accelerate; override;
    procedure Brake; override;
    procedure NextTarget; override;
  public
    constructor Create(aSpacefighter: TdfSpaceFighter);
    property Enabled: Boolean read FEnabled write FEnabled;

    procedure Update(deltaTime: Double; X, Y: Integer); override;
  end;

implementation

{ TdfAIControl }

procedure TdfAIControl.Accelerate;
begin
  inherited;
  FSpaceFighter.Accelerate(FDeltaTime);
end;

procedure TdfAIControl.Brake;
begin
  inherited;
  FSpaceFighter.Accelerate(-FDeltaTime);
end;

constructor TdfAIControl.Create(aSpacefighter: TdfSpaceFighter);
begin
  inherited Create;
  FSpaceFighter := aSpacefighter;
end;

procedure TdfAIControl.NextTarget;
begin
  inherited;
end;

procedure TdfAIControl.SetMouseMovement;
begin
  inherited;
  sf_dir := VectorAdd(VectorScale(AffineVectorMake(FSpaceFighter.AbsoluteLeft), dmx),
                      VectorScale(AffineVectorMake(FSpaceFighter.AbsoluteUp)  , dmy));
  ScaleVector(sf_dir, FDeltaTime);
  with FSpaceFighter do
  begin
    Direction.AsAffineVector := VectorAdd(Direction.AsAffineVector, sf_dir);
    Direction.Normalize;
  end;

//  FSpaceFighter.Fighter.RollAngle := dmx * C_SPACEFIGHTER_MAXROLL;
end;

procedure TdfAIControl.Shoot;
begin
  inherited;

end;

procedure TdfAIControl.Update(deltaTime: Double; X, Y: Integer);
begin
  inherited;
  if not FEnabled then
    Exit;

  SetMouseMovement;
  FDeltaTime := deltaTime;
  FT := FT + FDeltaTime;
  if FT > 2.0 then
  begin
    FT := 0;
    dmx := (100 - Random(200)) / 100;
    dmy := (100 - Random(200)) / 100;
    if Random(10) > 4 then
      Accelerate()
    else
      Brake();
    if FSpaceFighter.Speed < 15 then
      FSpaceFighter.Speed := 15;
  end;
end;

end.
