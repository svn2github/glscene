//
// VXScene Component Library, based on GLScene http://glscene.sourceforge.net
//
{
  Standard TVXBehaviour subclasses for GLScene<p>
}
unit VXS.Behaviours;

interface

{$I VXScene.inc}

uses
  System.Classes,
  System.SysUtils,

  VXS.VectorTypes,
  VXS.Scene,
  VXS.VectorGeometry,
  VXS.XCollection,
  VXS.BaseClasses,
  VXS.Coordinates;

type
  (* Holds parameters for TVXScene basic damping model.
    Damping is modeled by calculating a force from the speed, this force
    can then be transformed to an acceleration is you know the object's mass.
    Formulas :
    damping = constant + linear * Speed + quadratic * Speed^2
    accel = damping / Mass
    That's just basic physics :). A note on the components :
    constant : use it for solid friction (will stop abruptly an object after
      decreasing its speed.
    linear : linear friction damping.
    quadratic : expresses viscosity. *)
  TVXDamping = class(TVXUpdateAbleObject)
  private
    FConstant: single;
    FLinear: single;
    FQuadratic: single;
  protected
  public
    constructor Create(aOwner: TPersistent); override;
    destructor Destroy; override;
    procedure WriteToFiler(writer: TWriter);
    procedure ReadFromFiler(reader: TReader);
    procedure Assign(Source: TPersistent); override;
      { Calculates attenuated speed over deltaTime.<p>
            Integration step is 0.01 sec, and the following formula is applied
            at each step: constant+linear*speed+quadratic*speed^2 }
    function Calculate(speed, deltaTime: double): double;
    // Returns a "[constant; linear; quadractic]" string
    function AsString(const damping: TVXDamping): string;
    { Sets all damping parameters in a single call. }
    procedure SetDamping(const constant: single = 0; const linear: single = 0;
      const quadratic: single = 0);
  published
    property Constant: single read FConstant write FConstant;
    property Linear: single read FLinear write FLinear;
    property Quadratic: single read FQuadratic write FQuadratic;
  end;

  { Simple translation and rotation Inertia behaviour.
    Stores translation and rotation speeds, to which you can apply
    accelerations.<p>
    Note that the rotation model is not physical, so feel free to contribute
    a "realworld" inertia class with realistic, axis-free, rotation inertia
    if this approximation does not suits your needs :). }
  TVXBInertia = class(TVXBehaviour)
  private
    FMass: single;
    FTranslationSpeed: TVXCoordinates;
    FTurnSpeed, FRollSpeed, FPitchSpeed: single;
    FTranslationDamping, FRotationDamping: TVXDamping;
    FDampingEnabled: boolean;
  protected
    procedure SetTranslationSpeed(const val: TVXCoordinates);
    procedure SetTranslationDamping(const val: TVXDamping);
    procedure SetRotationDamping(const val: TVXDamping);
    procedure WriteToFiler(writer: TWriter); override;
    procedure ReadFromFiler(reader: TReader); override;
  public
    constructor Create(aOwner: TXCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    class function FriendlyName: string; override;
    class function FriendlyDescription: string; override;
    class function UniqueItem: boolean; override;
    procedure DoProgress(const progressTime: TVXProgressTimes); override;
    { Adds time-proportionned acceleration to the speed. }
    procedure ApplyTranslationAcceleration(const deltaTime: double;
      const accel: TVector);
    { Applies a timed force to the inertia. If Mass is null, nothing is done. }
    procedure ApplyForce(const deltaTime: double; const force: TVector);
    { Applies a timed torque to the inertia (yuck!).
      This gets a "yuck!" because it is as false as the rest of the rotation  model. }
    procedure ApplyTorque(const deltaTime: double;
      const turnTorque, rollTorque, pitchTorque: single);
    { Inverts the translation vector. }
    procedure MirrorTranslation;
    { Bounce speed as if hitting a surface.
      restitution is the coefficient of restituted energy (1=no energy loss,
      0=no bounce). The normal is NOT assumed to be normalized. }
    procedure SurfaceBounce(const surfaceNormal: TVector; restitution: single);
  published
    property Mass: single read FMass write FMass;
    property TranslationSpeed: TVXCoordinates
      read FTranslationSpeed write SetTranslationSpeed;
    property TurnSpeed: single read FTurnSpeed write FTurnSpeed;
    property RollSpeed: single read FRollSpeed write FRollSpeed;
    property PitchSpeed: single read FPitchSpeed write FPitchSpeed;
    { Enable/Disable damping (damping has a high cpu-cycle cost).
      Damping is enabled by default. }
    property DampingEnabled: boolean read FDampingEnabled write FDampingEnabled;
    { Damping applied to translation speed.
      Note that it is not "exactly" applied, ie. if damping would stop
      your object after 0.5 time unit, and your progression steps are
      of 1 time unit, there will be an integration error of 0.5 time unit. }
    property TranslationDamping: TVXDamping read FTranslationDamping write SetTranslationDamping;
      { Damping applied to rotation speed (yuck!).
        Well, this one is not "exact", like TranslationDamping, and neither
        it is "physical" since I'm reusing the mass and... and... well don't
        show this to your science teacher 8).
        Anyway that's easier to use than the realworld formulas, calculated
        faster, and properly used can give a good illusion of reality. }
    property RotationDamping: TVXDamping read FRotationDamping write SetRotationDamping;
  end;

  { Applies a constant acceleration to a TVXBInertia.<p> }
  TVXBAcceleration = class(TVXBehaviour)
  private
    FAcceleration: TVXCoordinates;
  protected
    procedure SetAcceleration(const val: TVXCoordinates);
    procedure WriteToFiler(writer: TWriter); override;
    procedure ReadFromFiler(reader: TReader); override;
  public
    constructor Create(aOwner: TXCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    class function FriendlyName: string; override;
    class function FriendlyDescription: string; override;
    class function UniqueItem: boolean; override;
    procedure DoProgress(const progressTime: TVXProgressTimes); override;
  published
    property Acceleration: TVXCoordinates read FAcceleration write FAcceleration;
  end;

{ Returns or creates the TVXBInertia within the given behaviours.<p>
  This helper function is convenient way to access a TVXBInertia. }
function GetInertia(const AVXSceneObject: TVXBaseSceneObject): TVXBInertia;
function GetOrCreateInertia(behaviours: TVXBehaviours): TVXBInertia; overload;
function GetOrCreateInertia(obj: TVXBaseSceneObject): TVXBInertia; overload;

{ Returns or creates the TVXBAcceleration within the given behaviours.<p>
  This helper function is convenient way to access a TVXBAcceleration. }
function GetOrCreateAcceleration(behaviours: TVXBehaviours): TVXBAcceleration;
  overload;
function GetOrCreateAcceleration(obj: TVXBaseSceneObject): TVXBAcceleration; overload;

// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------

function GetInertia(const AVXSceneObject: TVXBaseSceneObject): TVXBInertia;
var
  i: integer;
begin
  i := AVXSceneObject.behaviours.IndexOfClass(TVXBInertia);
  if i >= 0 then
    Result := TVXBInertia(AVXSceneObject.behaviours[i])
  else
    Result := nil;
end;

function GetOrCreateInertia(behaviours: TVXBehaviours): TVXBInertia;
var
  i: integer;
begin
  i := behaviours.IndexOfClass(TVXBInertia);
  if i >= 0 then
    Result := TVXBInertia(behaviours[i])
  else
    Result := TVXBInertia.Create(behaviours);
end;

function GetOrCreateInertia(obj: TVXBaseSceneObject): TVXBInertia;
begin
  Result := GetOrCreateInertia(obj.Behaviours);
end;

function GetOrCreateAcceleration(behaviours: TVXBehaviours): TVXBAcceleration;
var
  i: integer;
begin
  i := behaviours.IndexOfClass(TVXBAcceleration);
  if i >= 0 then
    Result := TVXBAcceleration(behaviours[i])
  else
    Result := TVXBAcceleration.Create(behaviours);
end;

function GetOrCreateAcceleration(obj: TVXBaseSceneObject): TVXBAcceleration;
begin
  Result := GetOrCreateAcceleration(obj.Behaviours);
end;

// ------------------
// ------------------ TVXDamping ------------------
// ------------------

constructor TVXDamping.Create(aOwner: TPersistent);
begin
  inherited Create(AOwner);
end;

destructor TVXDamping.Destroy;
begin
  inherited Destroy;
end;

procedure TVXDamping.Assign(Source: TPersistent);
begin
  if Source is TVXDamping then
  begin
    FConstant := TVXDamping(Source).Constant;
    FLinear := TVXDamping(Source).Linear;
    FQuadratic := TVXDamping(Source).Quadratic;
  end
  else
    inherited Assign(Source);
end;

procedure TVXDamping.WriteToFiler(writer: TWriter);
var
  writeStuff: boolean;
begin
  with writer do
  begin
    WriteInteger(0); // Archive Version 0
    writeStuff := (FConstant <> 0) or (FLinear <> 0) or (FQuadratic <> 0);
    WriteBoolean(writeStuff);
    if writeStuff then
    begin
      WriteFloat(FConstant);
      WriteFloat(FLinear);
      WriteFloat(FQuadratic);
    end;
  end;
end;

procedure TVXDamping.ReadFromFiler(reader: TReader);
begin
  with reader do
  begin
    ReadInteger; // ignore Archive Version
    if ReadBoolean then
    begin
      FConstant := ReadFloat;
      FLinear := ReadFloat;
      FQuadratic := ReadFloat;
    end
    else
    begin
      FConstant := 0;
      FLinear := 0;
      FQuadratic := 0;
    end;
  end;
end;

function TVXDamping.Calculate(speed, deltaTime: double): double;
var
  dt: double;
begin
  while deltaTime > 0 do
  begin
    if deltaTime > 0.01 then
    begin
      dt := 0.01;
      deltaTime := deltaTime - 0.01;
    end
    else
    begin
      dt := deltaTime;
      deltaTime := 0;
    end;
    speed := speed - dt * ((FQuadratic * speed + FLinear) * speed + FConstant);
  end;
  Result := speed;
end;

function TVXDamping.AsString(const damping: TVXDamping): string;
begin
  Result := Format('[%f; %f; %f]', [Constant, Linear, Quadratic]);
end;

procedure TVXDamping.SetDamping(const constant: single = 0;
  const linear: single = 0; const quadratic: single = 0);
begin
  FConstant := constant;
  FLinear := linear;
  FQuadratic := quadratic;
end;

// ------------------
// ------------------ TVXBInertia ------------------
// ------------------

constructor TVXBInertia.Create(aOwner: TXCollection);
begin
  inherited Create(aOwner);
  FTranslationSpeed := TVXCoordinates.CreateInitialized(Self, NullHmgVector, csVector);
  FMass := 1;
  FDampingEnabled := True;
  FTranslationDamping := TVXDamping.Create(Self);
  FRotationDamping := TVXDamping.Create(Self);
end;

destructor TVXBInertia.Destroy;
begin
  FRotationDamping.Free;
  FTranslationDamping.Free;
  FTranslationSpeed.Free;
  inherited Destroy;
end;

procedure TVXBInertia.Assign(Source: TPersistent);
begin
  if Source.ClassType = Self.ClassType then
  begin
    FMass := TVXBInertia(Source).Mass;
    FTranslationSpeed.Assign(TVXBInertia(Source).FTranslationSpeed);
    FTurnSpeed := TVXBInertia(Source).TurnSpeed;
    FRollSpeed := TVXBInertia(Source).RollSpeed;
    FPitchSpeed := TVXBInertia(Source).PitchSpeed;
    FDampingEnabled := TVXBInertia(Source).DampingEnabled;
    FTranslationDamping.Assign(TVXBInertia(Source).TranslationDamping);
    FRotationDamping.Assign(TVXBInertia(Source).RotationDamping);
  end;
  inherited Assign(Source);
end;

procedure TVXBInertia.WriteToFiler(writer: TWriter);
begin
  inherited;
  with writer do
  begin
    WriteInteger(0); // Archive Version 0
    WriteFloat(FMass);
    FTranslationSpeed.WriteToFiler(writer);
    WriteFloat(FTurnSpeed);
    WriteFloat(FRollSpeed);
    WriteFloat(FPitchSpeed);
    WriteBoolean(FDampingEnabled);
    FTranslationDamping.WriteToFiler(writer);
    FRotationDamping.WriteToFiler(writer);
  end;
end;

procedure TVXBInertia.ReadFromFiler(reader: TReader);
begin
  inherited;
  with reader do
  begin
    ReadInteger; // ignore archiveVersion
    FMass := ReadFloat;
    FTranslationSpeed.ReadFromFiler(reader);
    FTurnSpeed := ReadFloat;
    FRollSpeed := ReadFloat;
    FPitchSpeed := ReadFloat;
    FDampingEnabled := ReadBoolean;
    FTranslationDamping.ReadFromFiler(reader);
    FRotationDamping.ReadFromFiler(reader);
  end;
end;

procedure TVXBInertia.SetTranslationSpeed(const val: TVXCoordinates);
begin
  FTranslationSpeed.Assign(val);
end;

procedure TVXBInertia.SetTranslationDamping(const val: TVXDamping);
begin
  FTranslationDamping.Assign(val);
end;

procedure TVXBInertia.SetRotationDamping(const val: TVXDamping);
begin
  FRotationDamping.Assign(val);
end;

class function TVXBInertia.FriendlyName: string;
begin
  Result := 'Simple Inertia';
end;

class function TVXBInertia.FriendlyDescription: string;
begin
  Result := 'A simple translation and rotation inertia';
end;

class function TVXBInertia.UniqueItem: boolean;
begin
  Result := True;
end;

procedure TVXBInertia.DoProgress(const progressTime: TVXProgressTimes);
var
  trnVector: TVector;
  speed, newSpeed: double;

  procedure ApplyRotationDamping(var rotationSpeed: single);
  begin
    if rotationSpeed > 0 then
    begin
      rotationSpeed := RotationDamping.Calculate(rotationSpeed, progressTime.deltaTime);
      if rotationSpeed <= 0 then
        rotationSpeed := 0;
    end
    else
    begin
      rotationSpeed := -RotationDamping.Calculate(-rotationSpeed, progressTime.deltaTime);
      if rotationSpeed >= 0 then
        rotationSpeed := 0;
    end;
  end;

begin
  // Apply damping to speed
  if DampingEnabled then
  begin
    // Translation damping
    speed := TranslationSpeed.VectorLength;
    if speed > 0 then
    begin
      newSpeed := TranslationDamping.Calculate(speed, progressTime.deltaTime);
      if newSpeed <= 0 then
      begin
        trnVector := NullHmgVector;
        TranslationSpeed.AsVector := trnVector;
      end
      else
      begin
        TranslationSpeed.Scale(newSpeed / Speed);
        SetVector(trnVector, TranslationSpeed.AsVector);
      end;
    end
    else
      SetVector(trnVector, NullHmgVector);
    // Rotation damping (yuck!)
    ApplyRotationDamping(FTurnSpeed);
    ApplyRotationDamping(FRollSpeed);
    ApplyRotationDamping(FPitchSpeed);
  end
  else
    SetVector(trnVector, TranslationSpeed.AsVector);
  // Apply speed to object
  with OwnerBaseSceneObject do
    with progressTime do
    begin
      Position.AddScaledVector(deltaTime, trnVector);
      TurnAngle := TurnAngle + TurnSpeed * deltaTime;
      RollAngle := RollAngle + RollSpeed * deltaTime;
      PitchAngle := PitchAngle + PitchSpeed * deltaTime;
    end;
end;

procedure TVXBInertia.ApplyTranslationAcceleration(const deltaTime: double;
  const accel: TVector);
begin
  FTranslationSpeed.AsVector := VectorCombine(FTranslationSpeed.AsVector,
    accel, 1, deltaTime);
end;

procedure TVXBInertia.ApplyForce(const deltaTime: double; const force: TVector);
begin
  if Mass <> 0 then
    FTranslationSpeed.AsVector :=
      VectorCombine(FTranslationSpeed.AsVector, force, 1, deltaTime / Mass);
end;

procedure TVXBInertia.ApplyTorque(const deltaTime: double;
  const turnTorque, rollTorque, pitchTorque: single);
var
  factor: double;
begin
  if Mass <> 0 then
  begin
    factor := deltaTime / Mass;
    FTurnSpeed := FTurnSpeed + turnTorque * factor;
    FRollSpeed := FRollSpeed + rollTorque * factor;
    FPitchSpeed := FPitchSpeed + pitchTorque * factor;
  end;
end;

procedure TVXBInertia.MirrorTranslation;
begin
  FTranslationSpeed.Invert;
end;

procedure TVXBInertia.SurfaceBounce(const surfaceNormal: TVector; restitution: single);
var
  f: single;
begin
  // does the current speed vector comply?
  f := VectorDotProduct(FTranslationSpeed.AsVector, surfaceNormal);
  if f < 0 then
  begin
    // remove the non-complying part of the speed vector
    FTranslationSpeed.AddScaledVector(-f / VectorNorm(surfaceNormal) *
      (1 + restitution), surfaceNormal);
  end;
end;

// ------------------
// ------------------ TVXBAcceleration ------------------
// ------------------

constructor TVXBAcceleration.Create(aOwner: TXCollection);
begin
  inherited;
  if aOwner <> nil then
    if not (csReading in TComponent(aOwner.Owner).ComponentState) then
      GetOrCreateInertia(TVXBehaviours(aOwner));
  FAcceleration := TVXCoordinates.CreateInitialized(Self, NullHmgVector, csVector);
end;

destructor TVXBAcceleration.Destroy;
begin
  inherited;
  FAcceleration.Free;
end;

procedure TVXBAcceleration.Assign(Source: TPersistent);
begin
  if Source.ClassType = Self.ClassType then
  begin
    FAcceleration.Assign(TVXBAcceleration(Source).FAcceleration);
  end;
  inherited Assign(Source);
end;

procedure TVXBAcceleration.WriteToFiler(writer: TWriter);
begin
  inherited;
  with writer do
  begin
    WriteInteger(0); // Archive Version 0
    FAcceleration.WriteToFiler(writer);
  end;
end;

procedure TVXBAcceleration.ReadFromFiler(reader: TReader);
begin
  inherited;
  with reader do
  begin
    ReadInteger; // ignore archiveVersion
    FAcceleration.ReadFromFiler(reader);
  end;
end;

procedure TVXBAcceleration.SetAcceleration(const val: TVXCoordinates);
begin
  FAcceleration.Assign(val);
end;

class function TVXBAcceleration.FriendlyName: string;
begin
  Result := 'Simple Acceleration';
end;

class function TVXBAcceleration.FriendlyDescription: string;
begin
  Result := 'A simple and constant acceleration';
end;

class function TVXBAcceleration.UniqueItem: boolean;
begin
  Result := False;
end;

procedure TVXBAcceleration.DoProgress(const progressTime: TVXProgressTimes);
var
  i: integer;
  Inertia: TVXBInertia;
begin
  i := Owner.IndexOfClass(TVXBInertia);
  if i >= 0 then
  begin
    Inertia := TVXBInertia(Owner[i]);
    Inertia.ApplyTranslationAcceleration(progressTime.deltaTime,
      FAcceleration.DirectVector);
  end
  else
  begin
    TVXBInertia.Create(Owner);
    //on next progress event this exception won't be raised, because TVXBInertia will be created again
    raise Exception.Create(ClassName + ' requires ' + TVXBInertia.ClassName +
      '! (' + TVXBInertia.ClassName + ' was added to the Behaviours again)');
  end;
end;

// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------

  // class registrations
  RegisterXCollectionItemClass(TVXBInertia);
  RegisterXCollectionItemClass(TVXBAcceleration);

finalization

  UnregisterXCollectionItemClass(TVXBInertia);
  UnregisterXCollectionItemClass(TVXBAcceleration);

end.

