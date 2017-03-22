{-----------------------------------------------------------------------------
 Unit Name: uVehicle

 Author:    Panagiotis Kakaletris (Orchestraman)

 Purpose:
  Implements Object Steerining Behaviours for GLScene OpenGL library.
 Bibliography:
  Based on "Steering Behaviors For Autonomous Characters" by Craig Reynolds.
  Visit http://www.red3d.com/cwr/steer/ for more information.
 Notes: Collision Code is based in GLFPSCollision unit part of GLScene OGL Library.
 History:
   5/jul/2004 - Orchestraman - First Creation
-----------------------------------------------------------------------------}

unit uVehicle;

interface

uses
  Classes, Contnrs,
   
  GLVectorGeometry, GLVectorTypes, GLScene, GLXCollection, GLCoordinates,
  GLBehaviours, GLCollision, GLCadencer, GLVectorFileObjects, GLBaseClasses,
  GLManager;

type
  TSteeringBehaviours = (sbhSeek, sbhFlee, sbhPursuit, sbhEvasion,
                          sbhOffsetPursuit, sbhArrival, sbhObstacleAvoidance,
                          sbhWander);
  TGLSteeringBehaviours = set of TSteeringBehaviours;

  TGLBVehicle = class;
  TGLVehicleManager = class;

  TBaseSteerBehaviour = class;
  TSteerBehaviourClass = class of TBaseSteerBehaviour;

  // TBaseSteerBehaviour
  //
  { Base Class for implementing Steering Behaviours}
  TBaseSteerBehaviour = class(TComponent)
    private
      FVehicle: TGLBVehicle;
      FSteerRatio: Single;

    protected
      procedure SetVehicle(const AValue: TGLBVehicle); virtual;

    public
      constructor Create(AOwner: TComponent); override;
      procedure ApplySteerForce; virtual; abstract;
      property Vehicle: TGLBVehicle read FVehicle write SetVehicle;
      property Ratio: Single read FSteerRatio write FSteerRatio;

  end;

  // TWanderSteer
  //
  { Implementation of Wander Steering Behaviour}
  TWanderSteer = class(TBaseSteerBehaviour)
    private
      FWanderModifier: TVector;
      FRate,
      FStrength: Double;

    protected
      procedure SetVehicle(const AValue: TGLBVehicle); override;

    public
      constructor Create(AOwner: TComponent); override;
      procedure ApplySteerForce; override;
      property Rate: Double read FRate write FRate;
      property Strength: Double read FStrength write FStrength;
      property WanderModifier: TVector read FWanderModifier write FWanderModifier;

   end;

  // TSeekSteer
  //
  { Implementation of Seek Steering Behaviour}
  TSeekSteer = class(TBaseSteerBehaviour)
    private
      FTarget: TGLBaseSceneObject;
      FTurnRate: Single;
      procedure SetTarget(const Value: TGLBaseSceneObject);

    protected
      procedure Notification(AComponent: TComponent;
        Operation: TOperation); override;

    public
      constructor Create(AOwner: TComponent); override;
      procedure ApplySteerForce; override;
      property Target: TGLBaseSceneObject read FTarget write SetTarget;

  end;

  // TFleeSteer
  //
  TFleeSteer = class(TBaseSteerBehaviour)
    private
      FTarget: TGLBaseSceneObject;
      procedure SetTarget(const Value: TGLBaseSceneObject);

    protected
      procedure Notification(AComponent: TComponent;
        Operation: TOperation); override;

    public
      constructor Create(AOwner: TComponent); override;
      procedure ApplySteerForce; override;
      property Target: TGLBaseSceneObject read FTarget write SetTarget;

  end;

  // TPursueSteer
  //
  TPursueSteer = class(TBaseSteerBehaviour)
    private
      FTarget: TGLBaseSceneObject;
      procedure SetTarget(const Value: TGLBaseSceneObject);

    protected
      procedure Notification(AComponent: TComponent;
        Operation: TOperation); override;

    public
      constructor Create(AOwner: TComponent); override;
      procedure ApplySteerForce; override;
      property Target: TGLBaseSceneObject read FTarget write SetTarget;

  end;

  // TWorldCollisionSteer
  //
  TWorldCollisionSteer = class(TBaseSteerBehaviour)
  private
    FMap: TGLFreeForm;
    FCollided: Boolean;
    oldPosition,
    velocity: TVector;
    FTurnRate: Single;
    procedure SetMap(const Value: TGLFreeForm);

  protected
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
    function SphereSweepAndSlide(freeform:TGLFreeform; SphereStart: TVector;
                var Velocity, newPosition: TVector; sphereRadius: single): boolean;
    procedure SetVehicle(const AValue: TGLBVehicle); override;

  public
    constructor Create(AOwner: TComponent); override;
    procedure ApplySteerForce; override;
    property Map: TGLFreeForm read FMap write SetMap;
    property Collided: Boolean read FCollided;
    property TurnRate: Single read FTurnRate write FTurnRate;

  end;

  // TGLBVehicle
  //
  TGLBVehicle = class(TGLBehaviour)
    private
       
      FSteerUpdateInterval: Double;
      FMass: Integer;
      FSpeed,
      FMaxForce,
      FMaxSpeed: Double;
      FUp,
      FVelocity,
      FAccumulator: TGLCoordinates;
      FProgressTime: TProgressTimes;
      FAccumulatedTime: Double;
      FManager: TGLVehicleManager;
      FGroupIndex: Integer;
      FManagerName: String; // NOT persistent, temporarily used for persistence
      FSteerBehaviours: TObjectList;
      FGLSteeringBehaviours: TGLSteeringBehaviours;
      FSeekSteer: TSeekSteer;
      FWanderSteer: TWanderSteer;
      FPursueSteer: TPursueSteer;
      FFleeSteer: TFleeSteer;
      FWorldCollisionSteer: TWorldCollisionSteer;

      FCollisionObject: TGLBaseSceneObject;

    protected
      { Protected Declarations }
      procedure SetGLSteeringBehaviours(const Value: TGLSteeringBehaviours);
      procedure SetManager(const Value: TGLVehicleManager);
      procedure SetGroupIndex(const Value: Integer);
      function GetVelocity: TGLCoordinates;
      procedure SetVelocity(const Value: TGLCoordinates);
      function GetSpeed: Double;
      procedure SetSpeed(const Value: Double);

      procedure WriteToFiler(writer: TWriter); override;
      procedure ReadFromFiler(reader: TReader); override;
      procedure Loaded; override;

    public
       
      constructor Create(aOwner : TGLXCollection); override;
      destructor Destroy; override;

      procedure Assign(Source: TPersistent); override;

      class function FriendlyName : String; override;
      class function FriendlyDescription : String; override;

      procedure DoProgress(const progressTime : TProgressTimes); override;
      procedure DoSteering;

      property ProgressTime: TProgressTimes read FProgressTime write FProgressTime;
      property AccumulatedTime: Double read FAccumulatedTime write FAccumulatedTime;

      property CollisionObject: TGLBaseSceneObject read FCollisionObject write FCollisionObject;
      property Accumulator: TGLCoordinates read FAccumulator;

      property Flee: TFleeSteer read FFleeSteer write FFleeSteer;
      property Seek: TSeekSteer read FSeekSteer write FSeekSteer;
      property Pursue: TPursueSteer read FPursueSteer write FPursueSteer;
      property Wander: TWanderSteer read FWanderSteer write FWanderSteer;
      property WorldCollision: TWorldCollisionSteer read FWorldCollisionSteer write FWorldCollisionSteer;

    published
       
      property Manager: TGLVehicleManager read FManager write SetManager;
      property GroupIndex: Integer read FGroupIndex write SetGroupIndex;
      property Mass: Integer read FMass write FMass;
//      property Velocity: TGLCoordinates read GetVelocity write SetVelocity;
      property MaxForce: Double read FMaxForce write FMaxForce;
      property MaxSpeed: Double read FMaxSpeed write FMaxSpeed;
      property Speed: Double read GetSpeed write SetSpeed;
      property SteeringBehaviours: TGLSteeringBehaviours read FGLSteeringBehaviours
                                    write SetGLSteeringBehaviours;
      property SteerUpdateInterval: Double read FSteerUpdateInterval write FSteerUpdateInterval;
      property SteerBehaviours: TObjectList read FSteerBehaviours write FSteerBehaviours;
      property Up: TGLCoordinates read FUp write FUp;

  end;

    // TGLVehicleManager
  //
  { Manager που διαχειρίζεται τα Vehicles}
  TGLVehicleManager = class(TComponent)
  private
     
    FSteerInterval: Double;
    FClients: TList;
    FCadencer: TGLCadencer;
    FWorldCollisionMap: TGLFreeForm;
    procedure SetCadencer(const Value: TGLCadencer);
    function GetCadencer: TGLCadencer;
    procedure SetSteerInterval(const Value: Double);
    procedure SetWorldCollisionMap(const Value: TGLFreeForm);

  protected
    { Protected Declarations }
    procedure RegisterClient(aClient: TGLBVehicle);
    procedure DeRegisterClient(aClient: TGLBVehicle);
    procedure DeRegisterAllClients;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;

  public
     
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure DoSteering;
    property Clients: TList read FClients;

  published
     
    property Cadencer: TGLCadencer read GetCadencer write SetCadencer;
    property SteerInterval: Double read FSteerInterval write SetSteerInterval;
    property WorldCollisionMap: TGLFreeForm read FWorldCollisionMap write SetWorldCollisionMap;
  end;


{: Returns or creates the TGLBVehicle within the given behaviours.<p>
	This helper function is convenient way to access a TGLBVehicle. }
function GetOrCreateVehicle(behaviours: TGLBehaviours): TGLBVehicle; overload;
{: Returns or creates the TGLBVehicle within the given object's behaviours.<p>
	This helper function is convenient way to access a TGLBVehicle. }
function GetOrCreateVehicle(obj: TGLBaseSceneObject): TGLBVehicle; overload;

implementation

uses
  SysUtils, Math;


// GetOrCreateVehicle (TGLBehaviours)
//
function GetOrCreateVehicle(behaviours: TGLBehaviours): TGLBVehicle;
var
  i: Integer;
begin
	i := behaviours.IndexOfClass(TGLBVehicle);
	if i >= 0 then
		Result := TGLBVehicle(behaviours[i])
	else Result := TGLBVehicle.Create(behaviours);
end;

// GetOrCreateVehicle (TGLBaseSceneObject)
//
function GetOrCreateVehicle(obj: TGLBaseSceneObject): TGLBVehicle;
begin
	Result := GetOrCreateVehicle(obj.Behaviours);
end;





{ TGLVehicleManager }

// TGLVehicleManager.Create
//
constructor TGLVehicleManager.Create(AOwner: TComponent);
begin
	inherited Create(AOwner);
  FClients := TList.Create;
  RegisterManager(Self);
  FSteerInterval := 0;
end;

// TGLVehicleManager.Destroy
//
destructor TGLVehicleManager.Destroy;
begin
  if Assigned(FCadencer) then
    FCadencer.RemoveFreeNotification(Self);
  FCadencer := nil;
	DeRegisterAllClients;
  DeRegisterManager(Self);
  FClients.Free;
	inherited Destroy;
end;

// TGLVehicleManager.DeRegisterAllClients
//
procedure TGLVehicleManager.DeRegisterAllClients;
var
  i: Integer;
begin
  // Fast deregistration
  for i:=0 to FClients.Count-1 do
    TGLBVehicle(FClients[i]).FManager := nil;
  FClients.Clear;
end;

// TGLVehicleManager.DeRegisterClient
//
procedure TGLVehicleManager.DeRegisterClient(aClient: TGLBVehicle);
begin
  if Assigned(aClient) then begin
    aClient.FManager := nil;
    FClients.Remove(aClient);
  end;
end;

// TGLVehicleManager.RegisterClient
//
procedure TGLVehicleManager.RegisterClient(aClient: TGLBVehicle);
begin
  if Assigned(aClient) then
    if FClients.IndexOf(aClient) < 0 then begin
      FClients.Add(aClient);
      aClient.FManager := Self;
    end;
end;

// TGLVehicleManager.DoSteering
//
procedure TGLVehicleManager.DoSteering;
var
  I: Integer;
begin
  for I := 0 to FClients.Count-1 do
    TGLBVehicle(FClients[I]).DoSteering;
end;


{ TGLBVehicle }

// TGLBVehicle.Create
//
constructor TGLBVehicle.Create(aOwner: TGLXCollection);
begin
  inherited Create(aOwner);

  FSteerUpdateInterval := 0;
  FAccumulatedTime := 0;
  FMass := 10;
  FSpeed := 1;
  FMaxForce := 1;
  FMaxSpeed := 1;

  FUp := TGLCoordinates.CreateInitialized(Self, VectorMake(0, 1, 0), csVector);

  FVelocity := TGLCoordinates.CreateInitialized(Self, VectorMake(1, 0, 1), csVector);
  FVelocity.Normalize;

  FAccumulator := TGLCoordinates.CreateInitialized(Self, VectorMake(1, 0, 1), csVector);
  FSteerBehaviours := TObjectList.Create(True);


  FWanderSteer := TWanderSteer.Create(nil);
  FWanderSteer.Vehicle := Self;
  FSteerBehaviours.Add(FWanderSteer);

  FSeekSteer := TSeekSteer.Create(nil);
  FSeekSteer.Vehicle := Self;
  FSteerBehaviours.Add(FSeekSteer);

  FFleeSteer := TFleeSteer.Create(nil);
  FFleeSteer.Vehicle := Self;
  FSteerBehaviours.Add(FFleeSteer);

  FPursueSteer := TPursueSteer.Create(nil);
  FFleeSteer.Vehicle := Self;
  FSteerBehaviours.Add(FPursueSteer);


end;

// TGLBVehicle.Destroy
//
destructor TGLBVehicle.Destroy;
begin
  Manager := nil;
  FreeAndNil(FSteerBehaviours);
  FWanderSteer := nil;
  FSeekSteer := nil;
  FPursueSteer := nil;
  FWorldCollisionSteer := nil;
  FreeAndNil(FAccumulator);
  FreeAndNil(FUp);
  inherited Destroy;
end;

// TGLBVehicle.SetManager
//
procedure TGLBVehicle.SetManager(const Value: TGLVehicleManager);
begin
  if Value <> FManager then begin
    if Assigned(FManager) then
      FManager.DeRegisterClient(Self);
    if Assigned(Value) then begin
      Value.RegisterClient(Self);
      Self.SteerUpdateInterval := Value.SteerInterval;

      FWorldCollisionSteer := TWorldCollisionSteer.Create(nil);
      FWorldCollisionSteer.Vehicle := Self;
      FWorldCollisionSteer.Map := Value.WorldCollisionMap;
      FSteerBehaviours.Add(FWorldCollisionSteer);
    end;
  end;
end;

// TGLBVehicle.SetGroupIndex
//
procedure TGLBVehicle.SetGroupIndex(const Value: Integer);
begin
  FGroupIndex := Value;
end;

// TGLBVehicle.FriendlyName
//
class function TGLBVehicle.FriendlyName: String;
begin
  Result := 'Steering';
end;

class function TGLBVehicle.FriendlyDescription: String;
begin
  Result:='Steering-behaviour registration';
end;

// TGLBVehicle.Assign
//
procedure TGLBVehicle.Assign(Source: TPersistent);
begin
  if Source is TGLBVehicle then begin
    Manager := TGLBVehicle(Source).Manager;
    Mass := TGLBVehicle(Source).Mass;
    Speed := TGLBVehicle(Source).Speed;
    MaxForce := TGLBVehicle(Source).MaxForce;
    MaxSpeed := TGLBVehicle(Source).MaxSpeed;
    GroupIndex := TGLBVehicle(Source).GroupIndex;
  end;
  inherited Assign(Source);
end;

// TGLBVehicle.Loaded
//
{ Κάνει register το steering behaviour στον πρώτο διαθέσιμο steering Manager που
  θα βρεί στην φόρμα.}
procedure TGLBVehicle.Loaded;
var
  mng: TComponent;
begin
  inherited;
  if FManagerName <> '' then begin
    mng := FindManager(TGLVehicleManager, FManagerName);
    if Assigned(mng) then
      Manager := TGLVehicleManager(mng);
    FManagerName:='';
  end;
end;

// TGLBVehicle.WriteToFiler
//
procedure TGLBVehicle.WriteToFiler(writer: TWriter);
begin
  with writer do begin
    WriteInteger(1); // ArchiveVersion 1, added FGroupIndex
    if Assigned(FManager) then
       WriteString(FManager.GetNamePath)
    else WriteString('');
    WriteInteger(FGroupIndex);
    WriteInteger(FMass);
    WriteFloat(FSpeed);
    WriteFloat(FMaxForce);
    WriteFloat(FMaxSpeed);
    FVelocity.WriteToFiler(writer);
  end;
end;

// TGLBVehicle.ReadFromFiler
//
procedure TGLBVehicle.ReadFromFiler(reader: TReader);
var
  archiveVersion: Integer;
begin
  with reader do begin
    archiveVersion := ReadInteger;
    Assert(archiveVersion in [0..1]);
    FManagerName := ReadString;
    Manager:=nil;
    if archiveVersion >= 1 then
      FGroupIndex := ReadInteger
    else FGroupIndex := 0;
    FMass := ReadInteger;
    FSpeed := ReadFloat;
    FMaxForce := ReadFloat;
    FMaxSpeed := ReadFloat;
    FVelocity.ReadFromFiler(reader);
  end;
end;

// TGLBVehicle.GetVelocity
//
function TGLBVehicle.GetVelocity: TGLCoordinates;
begin
  Result := FVelocity;
end;

// TGLBVehicle.SetVelocity
//
procedure TGLBVehicle.SetVelocity(const Value: TGLCoordinates);
begin
  FVelocity := Value;
end;

// TGLBVehicle.GetSpeed
//
function TGLBVehicle.GetSpeed: Double;
begin
  Result := FSpeed;
end;

// TGLBVehicle.SetSpeed
//
procedure TGLBVehicle.SetSpeed(const Value: Double);
begin
  FSpeed := Value;
end;

// TGLBVehicle.DoSteering
//
procedure TGLBVehicle.DoSteering;
var
  acceleration: Double;
  newLeft: TVector;
begin
  if AccumulatedTime < SteerUpdateInterval then exit;
  FAccumulator.SetVector(OwnerBaseSceneObject.Direction.AsVector);
  FAccumulator.Normalize;
  //FAccumulator.AsVector := NullHmgVector;
  //FAccumulator.Scale(Speed * AccumulatedTime);

  with OwnerBaseSceneObject do begin

    //Εκτελώ το Collision.
    FWorldCollisionSteer.ApplySteerForce;
    if not FWorldCollisionSteer.Collided then begin
      FSeekSteer.ApplySteerForce;
      FWanderSteer.ApplySteerForce;
      FFleeSteer.ApplySteerForce;
    end
    else begin
      FWanderSteer.WanderModifier := OwnerBaseSceneObject.Direction.AsVector;
    end;

    Direction.AddScaledVector(AccumulatedTime, FAccumulator.AsVector);

    //Υπολογίζω τη δνση του Up Vector για να μήν γέρνει το αντικείμενο κατά τη στροφή του.
    VectorCrossProduct(VectorNormalize(Direction.DirectVector), FUp.DirectVector, newLeft);
    Up.AsVector := VectorCrossProduct(VectorNormalize(Direction.DirectVector), newLeft);

    acceleration := 1 / Mass;
    speed := Lerp(speed, MaxSpeed, acceleration);

    Move(speed * AccumulatedTime);

  end;
  AccumulatedTime := 0;
end;

// TGLVehicleManager.Notification
//
procedure TGLVehicleManager.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  if (Operation = opRemove) and (AComponent = Cadencer) then
    Cadencer := nil
  else
    if (Operation = opRemove) and (AComponent = FWorldCollisionMap) then begin
      FWorldCollisionMap.RemoveFreeNotification(Self);
      FWorldCollisionMap := nil;
    end
    else inherited;
end;

procedure TGLVehicleManager.SetCadencer(const Value: TGLCadencer);
begin
  if FCadencer = Value then exit;

  if Assigned(FCadencer) then
    FCadencer.RemoveFreeNotification(Self);

  FCadencer := Value;
  
  if FCadencer <> nil then
    FCadencer.FreeNotification(Self);
end;

function TGLVehicleManager.GetCadencer: TGLCadencer;
begin
  Result := FCadencer;
end;

{ TBaseSteerBehaviour }

constructor TBaseSteerBehaviour.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FVehicle := nil;
  FSteerRatio := 1;
end;

procedure TBaseSteerBehaviour.SetVehicle(const AValue: TGLBVehicle);
begin
  FVehicle := AValue;
end;

{ TWanderSteer }

procedure TWanderSteer.ApplySteerForce;
var
  vWander: TVector;
  vStrength: TVector;
  vDesiredDirection: TVector;
const
  c2PI = 2 * pi;
begin
  with vehicle do begin
    MakeVector(vWander, VectorAdd(VectorMake(cos(random * c2PI) * FRate,
      ClampValue(cos(random * c2Pi) * FRate, -0.01 * FRate, 0.01 * FRate), cos(random * c2PI) * FRate), FWanderModifier));                                     // Φτιάχνω τυχαίο δ/σμα μετατόπισης.
    NormalizeVector(vWander);                                                   // Κανονικοποιώ στην μονάδα.
    ScaleVector(vWander, 10);                                          // Κάνω scale στο WanderRate.
    FWanderModifier := vWander;

    MakeVector(vStrength, OwnerBaseSceneObject.Direction.AsVector);
    NormalizeVector(vStrength);
    ScaleVector(vStrength, FStrength);

    VectorAdd(vStrength, vWander, vDesiredDirection);
    NormalizeVector(vDesiredDirection);

    VectorSubtract(vDesiredDirection, OwnerBaseSceneObject.Direction.AsVector, vDesiredDirection);
    //NormalizeVector(vDesiredDirection);

    FAccumulator.AddScaledVector(Ratio, vDesiredDirection);

  end;
end;

// TGLBVehicle.SetGLSteeringBehaviours
//
procedure TGLBVehicle.SetGLSteeringBehaviours(
  const Value: TGLSteeringBehaviours);
begin
  FGLSteeringBehaviours := Value;

end;

// TGLVehicleManager.SetSteerInterval
//
procedure TGLVehicleManager.SetSteerInterval(const Value: Double);
var
  I: Integer;
begin
  FSteerInterval := Value;
  for I := 0 to FClients.Count - 1 do
    TGLBVehicle(FClients.Items[I]).SteerUpdateInterval := FSteerInterval;
end;

// TGLBVehicle.DoProgress
//
procedure TGLBVehicle.DoProgress(const progressTime: TProgressTimes);
begin
  FProgressTime := progressTime;
  AccumulatedTime := AccumulatedTime + progressTime.deltaTime;
end;

constructor TWanderSteer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FRate := 1;
  FStrength := 1;
end;

{ TSeekSteer }

// TSeekSteer.ApplySteerForce
//
procedure TSeekSteer.ApplySteerForce;
var
  vDesiredDirection: TVector;
  vDistance: TVector;
  lDistance: Single;
begin
  if Assigned(FTarget) then
    with FVehicle do begin
      vDesiredDirection := VectorNormalize(VectorSubtract(OwnerBaseSceneObject.Position.AsVector,
                                  FTarget.Position.AsVector));

      vDistance := VectorSubtract(OwnerBaseSceneObject.Direction.AsVector,
                                  vDesiredDirection);
      lDistance := VectorLength(vDistance);
      FAccumulator.AddScaledVector(10 * FTurnRate * lDistance * Ratio, VectorNormalize(vDistance));
    end;
end;

// TSeekSteer.Create
//
constructor TSeekSteer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FTurnRate := 0.3;
end;

// TSeekSteer.Notification
//
procedure TSeekSteer.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  if (Operation = opRemove) and (AComponent = FTarget) then begin
    AComponent.RemoveFreeNotification(Self);
    FTarget := nil;
  end
  else
    inherited;
end;

// TSeekSteer.SetTarget
//
procedure TSeekSteer.SetTarget(const Value: TGLBaseSceneObject);
begin
  if Assigned(FTarget) then
    FTarget.RemoveFreeNotification(Self);
  FTarget := Value;
  if Assigned(FTarget) then
    FTarget.FreeNotification(Self);
end;

// TWanderSteer.SetVehicle
//
procedure TWanderSteer.SetVehicle(const AValue: TGLBVehicle);
begin
  inherited SetVehicle(AValue);
  SetVector(FWanderModifier, Vehicle.OwnerBaseSceneObject.Direction.AsVector);
end;

{ TFleeSteer }

// TFleeSteer.ApplySteerForce
//
procedure TFleeSteer.ApplySteerForce;
var
  vDesiredDirection: TVector;
begin
  if Assigned(FTarget) then
    with FVehicle do begin
      vDesiredDirection := VectorNegate(VectorNormalize(VectorSubtract(
                                  OwnerBaseSceneObject.Position.AsVector,
                                  FTarget.Position.AsVector)));
      FAccumulator.AddScaledVector(0.3 * Speed * Ratio * VectorLength(VectorSubtract(
                                      OwnerBaseSceneObject.Direction.AsVector,
                                      vDesiredDirection)),
                                      VectorNormalize(
                                      VectorSubtract(
                                      OwnerBaseSceneObject.Direction.AsVector,
                                      vDesiredDirection)));
    end;
end;

// TFleeSteer.Create
//
constructor TFleeSteer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

end;

// TFleeSteer.Notification
//
procedure TFleeSteer.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  if (Operation = opRemove) and (AComponent = FTarget) then begin
    AComponent.RemoveFreeNotification(Self);
    FTarget := nil;
  end
  else
    inherited;
end;

// TFleeSteer.SetTarget
//
procedure TFleeSteer.SetTarget(const Value: TGLBaseSceneObject);
begin
  if Assigned(FTarget) then
    FTarget.RemoveFreeNotification(Self);
  FTarget := Value;
  if Assigned(FTarget) then
    FTarget.FreeNotification(Self);
end;

{ TPursueSteer }

// TPursueSteer.ApplySteerForce
//
procedure TPursueSteer.ApplySteerForce;
var
  vDesiredDirection: TVector;
  vDistance: TVector;
  lDistance: Single;
begin
  if Assigned(FTarget) then
    with FVehicle do begin
      vDesiredDirection := VectorNormalize(VectorSubtract(OwnerBaseSceneObject.Position.AsVector,
                                  FTarget.LocalToAbsolute(FTarget.FindChild('GLDummyCube2', true).Position.AsVector)));

      FTarget.FindChild('GLDummyCube2', true).Position.Z := 1 - 1 * VectorDotProduct(OwnerBaseSceneObject.Direction.AsVector, FTarget.Direction.AsVector) / VectorDistance(OwnerBaseSceneObject.Position.AsVector, FTarget.Position.AsVector);

      vDistance := VectorSubtract(OwnerBaseSceneObject.Direction.AsVector,
                                  vDesiredDirection);
      lDistance := VectorLength(vDistance);
      FAccumulator.AddScaledVector(Speed * Ratio * lDistance, VectorNormalize(vDistance));
      //Ratio := Ratio -  0.00005;
    end;
end;

// TPursueSteer.Create
//
constructor TPursueSteer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

end;

// TPursueSteer.Notification
//
procedure TPursueSteer.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  if (Operation = opRemove) and (AComponent = FTarget) then begin
    AComponent.RemoveFreeNotification(Self);
    FTarget := nil;
  end
  else
    inherited;
end;

// TPursueSteer.SetTarget
//
procedure TPursueSteer.SetTarget(const Value: TGLBaseSceneObject);
begin
  if Assigned(FTarget) then
    FTarget.RemoveFreeNotification(Self);
  FTarget := Value;
  if Assigned(FTarget) then
    FTarget.FreeNotification(Self);
end;

{ TWorldCollisionSteer }

function TWorldCollisionSteer.SphereSweepAndSlide(freeform:TGLFreeform;
  SphereStart:TVector;
  var Velocity,newPosition:TVector; sphereRadius: single): boolean;
var
  oldPosition, ray:TVector;
  vel,slidedistance:Single;
  intPoint,intNormal:TVector;
  newDirection, newRay,collisionPosition, pointOnSphere,point2OnSphere:TVector;
  i:integer;
  SphereRadiusRel: single;
begin
  SphereRadiusRel := SphereRadius/freeform.Scale.x; // μπορεί να γίνει Scale.y, or Scale.z Υποθέτοντας ότι είναι τα ίδια.

  oldPosition := SphereStart;

  result := true;

  //Δ/νση στην οποία κινείται η σφάιρα.
  ray := VectorSubtract(newPosition,oldPosition);

//  ray := Velocity;
//  newPosition := VectorAdd(newPosition,ray);
  //Ταχύτητα της σφαίρας. Μέτρο του διανύσματος της θέσης με την προηγούμενη θέση.
  //Το κάνω έτσι για να μήν εξαρτώνται οι υπολογισμοί απο την ταχύτητα του επεξεργαστή.
  vel := VectorLength(ray);

  //αν η σφαίρα δεν κινείται τότε δεν χρειάζεται να κάνω τίποτα.
  // διαφορετικά εκτελώ μέχρι 7 loops

  if vel > 0 then
  for i := 0 to 6 do
  begin
    //Αν υπάρχει intersection χρειάζονται επιπλέον υπολογισμοί.
    if (freeform.OctreeSphereSweepIntersect(oldPosition,ray,vel,SphereRadiusRel,@intPoint,@intNormal)) then
    begin
      if VectorDistance2(oldPosition,intPoint) <= sqr(SphereRadius) then
      begin
        //Η σφαίρα διασταυρώνεται με κάποιο τρίγωνο.
        intNormal := VectorScale(VectorSubtract(oldPosition, intPoint), 1.0001);
      end
      else
      begin
        //αν η σφαίρα δεν διασταυρώνεται με κάποιο τρίγωνο.
        //intNormal := VectorSubtract(oldPosition,intPoint);  //Δεν είναι σωστό αλλά δουλεύει καλά για μικρά time steps.
        //intNormal := VectorScale(VectorNormalize(intNormal), SphereRadius + 0.0001);
        if RayCastSphereInterSect(intPoint,VectorNormalize(VectorNegate(ray)), oldPosition,SphereRadius,PointOnSphere, Point2OnSphere) > 0 then
          intNormal := VectorScale(VectorSubtract(oldPosition, PointOnSphere), 1.0001)
          //intNormal := VectorScale(VectorNormalize(VectorSubtract(oldPosition, PointOnSphere)), SphereRadius + 0.001) //VectorDistance(oldPosition, PointOnSphere));
        else
        begin
//          Assert(False);  //Αυτό δεν θα συμβεί ποτέ, μόνο για debuging.
          intNormal := VectorScale(VectorSubtract(oldPosition,intPoint), 1.0001);
        end;

      end;

      //υπολογισμός του κέντρου της σφαίρας όταν συμβεί collision.
      collisionPosition := VectorAdd(intPoint, intNormal);
      oldPosition := collisionPosition;

      //Υπολογισμός της απόστασης που δεν διανύθηκε εξαιτίας του εμποδίου.
      newRay := VectorSubtract(newPosition, collisionPosition);

      //Υπολογισμός της νέας δ/νσης αν χτυπησει σε κάποιο εμπόδιο.
      newDirection := VectorCrossProduct(intNormal, VectorCrossProduct(newRay, intNormal));
      if VectorNorm(NewDirection) > 0 then
        NormalizeVector(newDirection);

      //υπολογισμός της απόστασης που πρέπει να κυλίσει (εξαρτάται απο το collision plane και το collision ray)
      SlideDistance := vectorDotProduct(newRay, newDirection);

      //υπολογισμός τριβής κατά την κίνηση με το εμπόδιο. (δεν είναι σωστό φυσικά)
//      if abs(SlideDistance) < 10 * deltaTime then SlideDistance := 0;
      ScaleVector(newDirection, SlideDistance);

      //υπολογισμός της νέας θέσης στην οποία κατευθύνεται η σφαίρα.
      newPosition := VectorAdd(collisionPosition, newDirection);
      ray := newDirection;
      vel := VectorLength(ray);

      if i=6 then
      begin
        newPosition := oldPosition;
        break;
      end;

      //ελέγχω για πολύ μικρές κινήσεις (πχ. όταν κολήσει σε μιά γωνία)
      if vel < 1E-10 then
      begin
        newPosition := oldPosition;
        break;
      end;

    end
    else //δεν έγινε collision οπότε τερματίζω το loop.
    begin
      if i = 0 then result:= false;
      Break;
    end;
  end; //τέλος i loop
  Velocity := Ray; //η δ/νση της νέας ταχύτητας.
end;

// TWorldCollisionSteer.ApplySteerForce
//
procedure TWorldCollisionSteer.ApplySteerForce;
var
  vDesiredDirection,
  vDistance,
  newPosition: TVector;
  lDistance: single;
begin
  FCollided := False;
  if not Assigned(FMap) then exit;

  newPosition := FVehicle.OwnerBaseSceneObject.Position.AsVector;

  FCollided := SphereSweepAndSlide(FMap, oldPosition, velocity, newPosition,
    FVehicle.OwnerBaseSceneObject.boundingSphereRadius + 2.3);

  oldPosition := newPosition;

  if FCollided then
  with FVehicle do begin
    vDesiredDirection := VectorNormalize(VectorSubtract(OwnerBaseSceneObject.Position.AsVector,
                                newPosition));

    vDistance := VectorSubtract(OwnerBaseSceneObject.Direction.AsVector,
                                vDesiredDirection);
    lDistance := VectorLength(vDistance);

    //Οταν γίνεται collision αφαιρώ 5% απο την ταχύτητα της σφαίρας.
    Speed := Speed * 0.9;
    FAccumulator.AddScaledVector(10 * FTurnRate * VectorLength(VectorSubtract(newPosition, FVehicle.OwnerBaseSceneObject.Position.AsVector)), VectorNormalize(VectorSubtract(newPosition, FVehicle.OwnerBaseSceneObject.Position.AsVector)));
  end;


//  if FCollided then begin
//    FVehicle.FAccumulator.AddScaledVector(4, VectorNormalize(VectorSubtract(newPosition, FVehicle.OwnerBaseSceneObject.Position.AsVector)));
//    FVehicle.Speed := FVehicle.Speed * 0.95;
//  end;

end;

// TWorldCollisionSteer.Create
//
constructor TWorldCollisionSteer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FMap := nil;
  velocity := NullHmgVector;
  FTurnRate := 0.3;
end;

// TWorldCollisionSteer.Notification
//
procedure TWorldCollisionSteer.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  if (Operation = opRemove) and (AComponent = FMap) then begin
    AComponent.RemoveFreeNotification(Self);
    FMap := nil;
  end
  else
    inherited;
end;

// TWorldCollisionSteer.SetMap
//
procedure TWorldCollisionSteer.SetMap(const Value: TGLFreeForm);
begin
  if Assigned(FMap) then
    FMap.RemoveFreeNotification(Self);

  FMap := Value;

  if Assigned(FMap) and (FMap <> nil) then
    FMap.FreeNotification(Self);
end;

// TGLVehicleManager.SetWorldCollisionMap
//
procedure TGLVehicleManager.SetWorldCollisionMap(const Value: TGLFreeForm);
begin
  if Assigned(FWorldCollisionMap) then begin
    FWorldCollisionMap.RemoveFreeNotification(Self);
    FWorldCollisionMap := nil;
  end;

  FWorldCollisionMap := Value;

  if FWorldCollisionMap <> nil then
    FWorldCollisionMap.FreeNotification(Self);
end;

procedure TWorldCollisionSteer.SetVehicle(const AValue: TGLBVehicle);
begin
  inherited;
  oldPosition := FVehicle.OwnerBaseSceneObject.Position.AsVector;
end;

initialization
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

// class registrations
RegisterXCollectionItemClass(TGLBVehicle);

end.
