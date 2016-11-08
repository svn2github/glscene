//
// VKScene Component Library, based on GLScene http://glscene.sourceforge.net 
//
{
  Standard TVKBehaviour subclasses for GLScene<p>
}
unit VKS.Behaviours;

interface

{$I VKScene.inc}

uses
  System.Classes, System.SysUtils,
  //VKS
  VKS.VectorTypes,
  VKS.Scene,
  VKS.VectorGeometry,
  VKS.XCollection,
  VKS.BaseClasses,
  VKS.Coordinates;

type

  // TVKDamping

  { Holds parameters for TVKScene basic damping model.
    Damping is modeled by calculating a force from the speed, this force
    can then be transformed to an acceleration is you know the object's mass.
    Formulas : 
    damping = constant + linear * Speed + quadratic * Speed^2
    accel = damping / Mass
    That's just basic physics :). A note on the components :
    constant : use it for solid friction (will stop abruptly an object after
      decreasing its speed.
    linear : linear friction damping.
    quadratic : expresses viscosity.
     }
  TVKDamping = class(TVKUpdateAbleObject)
  private
    { Private Declarations }
    FConstant: single;
    FLinear: single;
    FQuadratic: single;

  protected
    { Protected Declarations }

  public
    { Public Declarations }
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
    function AsString(const damping: TVKDamping): string;
    { Sets all damping parameters in a single call. }
    procedure SetDamping(const constant: single = 0; const linear: single = 0;
      const quadratic: single = 0);

  published
    { Published Declarations }
    property Constant: single read FConstant write FConstant;
    property Linear: single read FLinear write FLinear;
    property Quadratic: single read FQuadratic write FQuadratic;
  end;

  // TVKBInertia

  { Simple translation and rotation Inertia behaviour.
    Stores translation and rotation speeds, to which you can apply
    accelerations.<p>
    Note that the rotation model is not physical, so feel free to contribute
    a "realworld" inertia class with realistic, axis-free, rotation inertia
    if this approximation does not suits your needs :). }
  TVKBInertia = class(TVKBehaviour)
  private
    { Private Declarations }
    FMass: single;
    FTranslationSpeed: TVKCoordinates;
    FTurnSpeed, FRollSpeed, FPitchSpeed: single;
    FTranslationDamping, FRotationDamping: TVKDamping;
    FDampingEnabled: boolean;

  protected
    { Protected Declarations }
    procedure SetTranslationSpeed(const val: TVKCoordinates);
    procedure SetTranslationDamping(const val: TVKDamping);
    procedure SetRotationDamping(const val: TVKDamping);

    procedure WriteToFiler(writer: TWriter); override;
    procedure ReadFromFiler(reader: TReader); override;

  public
    { Public Declarations }
    constructor Create(aOwner: TVKXCollection); override;
    destructor Destroy; override;

    procedure Assign(Source: TPersistent); override;

    class function FriendlyName: string; override;
    class function FriendlyDescription: string; override;
    class function UniqueItem: boolean; override;

    procedure DoProgress(const progressTime: TProgressTimes); override;

    { Adds time-proportionned acceleration to the speed. }
    procedure ApplyTranslationAcceleration(const deltaTime: double;
      const accel: TVector);
      { Applies a timed force to the inertia.<p>
        If Mass is null, nothing is done. }
    procedure ApplyForce(const deltaTime: double; const force: TVector);
      { Applies a timed torque to the inertia (yuck!).<p>
        This gets a "yuck!" because it is as false as the rest of the
        rotation  model. }
    procedure ApplyTorque(const deltaTime: double;
      const turnTorque, rollTorque, pitchTorque: single);
    { Inverts the translation vector.<p> }
    procedure MirrorTranslation;
         { Bounce speed as if hitting a surface.<p>
            restitution is the coefficient of restituted energy (1=no energy loss,
            0=no bounce). The normal is NOT assumed to be normalized. }
    procedure SurfaceBounce(const surfaceNormal: TVector; restitution: single);

  published
    { Published Declarations }
    property Mass: single read FMass write FMass;
    property TranslationSpeed: TVKCoordinates
      read FTranslationSpeed write SetTranslationSpeed;
    property TurnSpeed: single read FTurnSpeed write FTurnSpeed;
    property RollSpeed: single read FRollSpeed write FRollSpeed;
    property PitchSpeed: single read FPitchSpeed write FPitchSpeed;

      { Enable/Disable damping (damping has a high cpu-cycle cost).<p>
        Damping is enabled by default. }
    property DampingEnabled: boolean read FDampingEnabled write FDampingEnabled;
      { Damping applied to translation speed.
        Note that it is not "exactly" applied, ie. if damping would stop
        your object after 0.5 time unit, and your progression steps are
        of 1 time unit, there will be an integration error of 0.5 time unit. }
    property TranslationDamping: TVKDamping read FTranslationDamping
      write SetTranslationDamping;
      { Damping applied to rotation speed (yuck!).
        Well, this one is not "exact", like TranslationDamping, and neither
        it is "physical" since I'm reusing the mass and... and... well don't
        show this to your science teacher 8).
        Anyway that's easier to use than the realworld formulas, calculated
        faster, and properly used can give a good illusion of reality. }
    property RotationDamping: TVKDamping read FRotationDamping write SetRotationDamping;
  end;

  // TVKBAcceleration

  { Applies a constant acceleration to a TVKBInertia.<p> }
  TVKBAcceleration = class(TVKBehaviour)
  private
    { Private Declarations }
    FAcceleration: TVKCoordinates;

  protected
    { Protected Declarations }
    procedure SetAcceleration(const val: TVKCoordinates);

    procedure WriteToFiler(writer: TWriter); override;
    procedure ReadFromFiler(reader: TReader); override;

  public
    { Public Declarations }
    constructor Create(aOwner: TVKXCollection); override;
    destructor Destroy; override;

    procedure Assign(Source: TPersistent); override;

    class function FriendlyName: string; override;
    class function FriendlyDescription: string; override;
    class function UniqueItem: boolean; override;

    procedure DoProgress(const progressTime: TProgressTimes); override;

  published
    { Published Declarations }
    property Acceleration: TVKCoordinates read FAcceleration write FAcceleration;
  end;

{ Returns or creates the TVKBInertia within the given behaviours.<p>
  This helper function is convenient way to access a TVKBInertia. }
function GetInertia(const AGLSceneObject: TVKBaseSceneObject): TVKBInertia;
function GetOrCreateInertia(behaviours: TVKBehaviours): TVKBInertia; overload;
function GetOrCreateInertia(obj: TVKBaseSceneObject): TVKBInertia; overload;

{ Returns or creates the TVKBAcceleration within the given behaviours.<p>
  This helper function is convenient way to access a TVKBAcceleration. }
function GetOrCreateAcceleration(behaviours: TVKBehaviours): TVKBAcceleration;
  overload;
function GetOrCreateAcceleration(obj: TVKBaseSceneObject): TVKBAcceleration; overload;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

// GetInertia

function GetInertia(const AGLSceneObject: TVKBaseSceneObject): TVKBInertia;
var
  i: integer;
begin
  i := AGLSceneObject.behaviours.IndexOfClass(TVKBInertia);
  if i >= 0 then
    Result := TVKBInertia(AGLSceneObject.behaviours[i])
  else
    Result := nil;
end;

// GetOrCreateInertia (TVKBehaviours)

function GetOrCreateInertia(behaviours: TVKBehaviours): TVKBInertia;
var
  i: integer;
begin
  i := behaviours.IndexOfClass(TVKBInertia);
  if i >= 0 then
    Result := TVKBInertia(behaviours[i])
  else
    Result := TVKBInertia.Create(behaviours);
end;

// GetOrCreateInertia (TVKBaseSceneObject)

function GetOrCreateInertia(obj: TVKBaseSceneObject): TVKBInertia;
begin
  Result := GetOrCreateInertia(obj.Behaviours);
end;

// GetOrCreateAcceleration (TVKBehaviours)

function GetOrCreateAcceleration(behaviours: TVKBehaviours): TVKBAcceleration;
var
  i: integer;
begin
  i := behaviours.IndexOfClass(TVKBAcceleration);
  if i >= 0 then
    Result := TVKBAcceleration(behaviours[i])
  else
    Result := TVKBAcceleration.Create(behaviours);
end;

// GetOrCreateAcceleration (TVKBaseSceneObject)

function GetOrCreateAcceleration(obj: TVKBaseSceneObject): TVKBAcceleration;
begin
  Result := GetOrCreateAcceleration(obj.Behaviours);
end;

// ------------------
// ------------------ TVKDamping ------------------
// ------------------

// Create

constructor TVKDamping.Create(aOwner: TPersistent);
begin
  inherited Create(AOwner);
end;

destructor TVKDamping.Destroy;
begin
  inherited Destroy;
end;

// Assign

procedure TVKDamping.Assign(Source: TPersistent);
begin
  if Source is TVKDamping then
  begin
    FConstant := TVKDamping(Source).Constant;
    FLinear := TVKDamping(Source).Linear;
    FQuadratic := TVKDamping(Source).Quadratic;
  end
  else
    inherited Assign(Source);
end;

// WriteToFiler

procedure TVKDamping.WriteToFiler(writer: TWriter);
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

// ReadFromFiler

procedure TVKDamping.ReadFromFiler(reader: TReader);
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

// Calculate

function TVKDamping.Calculate(speed, deltaTime: double): double;
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

// DampingAsString

function TVKDamping.AsString(const damping: TVKDamping): string;
begin
  Result := Format('[%f; %f; %f]', [Constant, Linear, Quadratic]);
end;

// SetDamping

procedure TVKDamping.SetDamping(const constant: single = 0;
  const linear: single = 0; const quadratic: single = 0);
begin
  FConstant := constant;
  FLinear := linear;
  FQuadratic := quadratic;
end;

// ------------------
// ------------------ TVKBInertia ------------------
// ------------------

// Create

constructor TVKBInertia.Create(aOwner: TVKXCollection);
begin
  inherited Create(aOwner);
  FTranslationSpeed := TVKCoordinates.CreateInitialized(Self, NullHmgVector, csVector);
  FMass := 1;
  FDampingEnabled := True;
  FTranslationDamping := TVKDamping.Create(Self);
  FRotationDamping := TVKDamping.Create(Self);
end;

// Destroy

destructor TVKBInertia.Destroy;
begin
  FRotationDamping.Free;
  FTranslationDamping.Free;
  FTranslationSpeed.Free;
  inherited Destroy;
end;

// Assign

procedure TVKBInertia.Assign(Source: TPersistent);
begin
  if Source.ClassType = Self.ClassType then
  begin
    FMass := TVKBInertia(Source).Mass;
    FTranslationSpeed.Assign(TVKBInertia(Source).FTranslationSpeed);
    FTurnSpeed := TVKBInertia(Source).TurnSpeed;
    FRollSpeed := TVKBInertia(Source).RollSpeed;
    FPitchSpeed := TVKBInertia(Source).PitchSpeed;
    FDampingEnabled := TVKBInertia(Source).DampingEnabled;
    FTranslationDamping.Assign(TVKBInertia(Source).TranslationDamping);
    FRotationDamping.Assign(TVKBInertia(Source).RotationDamping);
  end;
  inherited Assign(Source);
end;

// WriteToFiler

procedure TVKBInertia.WriteToFiler(writer: TWriter);
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

// ReadFromFiler

procedure TVKBInertia.ReadFromFiler(reader: TReader);
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

// SetTranslationSpeed

procedure TVKBInertia.SetTranslationSpeed(const val: TVKCoordinates);
begin
  FTranslationSpeed.Assign(val);
end;

// SetTranslationDamping

procedure TVKBInertia.SetTranslationDamping(const val: TVKDamping);
begin
  FTranslationDamping.Assign(val);
end;

// SetRotationDamping

procedure TVKBInertia.SetRotationDamping(const val: TVKDamping);
begin
  FRotationDamping.Assign(val);
end;

// FriendlyName

class function TVKBInertia.FriendlyName: string;
begin
  Result := 'Simple Inertia';
end;

// FriendlyDescription

class function TVKBInertia.FriendlyDescription: string;
begin
  Result := 'A simple translation and rotation inertia';
end;

// UniqueBehaviour

class function TVKBInertia.UniqueItem: boolean;
begin
  Result := True;
end;

// DoProgress

procedure TVKBInertia.DoProgress(const progressTime: TProgressTimes);
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

// ApplyTranslationAcceleration

procedure TVKBInertia.ApplyTranslationAcceleration(const deltaTime: double;
  const accel: TVector);
begin
  FTranslationSpeed.AsVector := VectorCombine(FTranslationSpeed.AsVector,
    accel, 1, deltaTime);
end;

// ApplyForce

procedure TVKBInertia.ApplyForce(const deltaTime: double; const force: TVector);
begin
  if Mass <> 0 then
    FTranslationSpeed.AsVector :=
      VectorCombine(FTranslationSpeed.AsVector, force, 1, deltaTime / Mass);
end;

// ApplyTorque

procedure TVKBInertia.ApplyTorque(const deltaTime: double;
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

// MirrorTranslation

procedure TVKBInertia.MirrorTranslation;
begin
  FTranslationSpeed.Invert;
end;

// SurfaceBounce

procedure TVKBInertia.SurfaceBounce(const surfaceNormal: TVector; restitution: single);
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
// ------------------ TVKBAcceleration ------------------
// ------------------

// Create

constructor TVKBAcceleration.Create(aOwner: TVKXCollection);
begin
  inherited;
  if aOwner <> nil then
    if not (csReading in TComponent(aOwner.Owner).ComponentState) then
      GetOrCreateInertia(TVKBehaviours(aOwner));
  FAcceleration := TVKCoordinates.CreateInitialized(Self, NullHmgVector, csVector);
end;

// Destroy

destructor TVKBAcceleration.Destroy;
begin
  inherited;
  FAcceleration.Free;
end;

// Assign

procedure TVKBAcceleration.Assign(Source: TPersistent);
begin
  if Source.ClassType = Self.ClassType then
  begin
    FAcceleration.Assign(TVKBAcceleration(Source).FAcceleration);
  end;
  inherited Assign(Source);
end;

// WriteToFiler

procedure TVKBAcceleration.WriteToFiler(writer: TWriter);
begin
  inherited;
  with writer do
  begin
    WriteInteger(0); // Archive Version 0
    FAcceleration.WriteToFiler(writer);
  end;
end;

// ReadFromFiler

procedure TVKBAcceleration.ReadFromFiler(reader: TReader);
begin
  inherited;
  with reader do
  begin
    ReadInteger; // ignore archiveVersion
    FAcceleration.ReadFromFiler(reader);
  end;
end;

// SetAcceleration

procedure TVKBAcceleration.SetAcceleration(const val: TVKCoordinates);
begin
  FAcceleration.Assign(val);
end;

// FriendlyName

class function TVKBAcceleration.FriendlyName: string;
begin
  Result := 'Simple Acceleration';
end;

// FriendlyDescription

class function TVKBAcceleration.FriendlyDescription: string;
begin
  Result := 'A simple and constant acceleration';
end;

// UniqueBehaviour

class function TVKBAcceleration.UniqueItem: boolean;
begin
  Result := False;
end;

// DoProgress

procedure TVKBAcceleration.DoProgress(const progressTime: TProgressTimes);
var
  i: integer;
  Inertia: TVKBInertia;
begin
  i := Owner.IndexOfClass(TVKBInertia);
  if i >= 0 then
  begin
    Inertia := TVKBInertia(Owner[i]);
    Inertia.ApplyTranslationAcceleration(progressTime.deltaTime,
      FAcceleration.DirectVector);
  end
  else
  begin
    TVKBInertia.Create(Owner);
    //on next progress event this exception won't be raised, because TVKBInertia will be created again
    raise Exception.Create(ClassName + ' requires ' + TVKBInertia.ClassName +
      '! (' + TVKBInertia.ClassName + ' was added to the Behaviours again)');
  end;
end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization
  // ------------------------------------------------------------------
  // ------------------------------------------------------------------
  // ------------------------------------------------------------------

  // class registrations
  RegisterXCollectionItemClass(TVKBInertia);
  RegisterXCollectionItemClass(TVKBAcceleration);

finalization

  UnregisterXCollectionItemClass(TVKBInertia);
  UnregisterXCollectionItemClass(TVKBAcceleration);

end.

