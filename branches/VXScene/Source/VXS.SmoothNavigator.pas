//
// VXScene Component Library, based on GLScene http://glscene.sourceforge.net
//
{
   An extention of TVXNavigator, which allows to move objects with inertia
   Note: it is not completely FPS-independant. Only Moving code is, but
   MoveAroundTarget, Turn[Vertical/Horizontal] and AdjustDistanceTo[..] is not.

   Don't know why, but when I make their code identical, these function stop
   working completely. So you probably have to call the AutoScaleParameters
   procedure once in a while for it to adjust to the current framerate.

    TODO:
      1) Scale "Old values" too, when callin the Scale parameter procedure to
         avoid the temporary "freeze" of controls.
      2) AddImpulse procedures.

}

unit VXS.SmoothNavigator;

interface

{$I VXScene.inc}

uses
  System.Classes,

  VXS.VectorTypes,
  VXS.Navigator,
  VXS.VectorGeometry,
  VXS.Scene,
  VXS.CrossPlatform,
  VXS.Coordinates,
  VXS.Screen,
  VXS.PersistentClasses,
  VXS.XCollection;

type

  { Includes a basic set of parameters that control the smoothness of movement. }
  TVXNavigatorAbstractParameters = class(TPersistent)
  private
    FOwner: TPersistent;
    FInertia: Single;
    FSpeed: Single;
    FCutoff: Single;
    function StoreCutoff: Boolean;
  protected
    function StoreInertia: Boolean; virtual;
    function StoreSpeed: Boolean; virtual;
    function GetOwner: TPersistent; override;
  public
    constructor Create(AOwner: TPersistent); virtual;
    procedure Assign(Source: TPersistent); override;
    procedure ScaleParameters(const Value: Single); virtual;
  published
    property Inertia: Single read FInertia write FInertia stored StoreInertia;
    property Speed: Single read FSpeed write FSpeed stored StoreSpeed;
    property Cutoff: Single read FCutoff write FCutoff stored StoreCutoff; 
  end;

  TVXSmoothNavigator = class;

  { Includes a basic set of parameters that control the smoothness of movement }
  TVXNavigatorSmoothChangeItem = class(TXCollectionItem)
  private
    FInertia: Single;
    FSpeed: Single;
    FEnabled: Boolean;
    FSpeedLimit: Single;
    FCutoff: Double;
    function StoreInertia: Boolean;
    function StoreSpeed: Boolean;
    function StoreSpeedLimit: Boolean;
    function StoreCutoff: Boolean;
  protected
    function GetNavigator: TVXSmoothNavigator;
  public
    { Returns False if there was no change. }
    function Proceed(ADeltaTime: Double): Boolean; virtual; abstract;
    constructor Create(aOwner: TXCollection); override;
    procedure Assign(Source: TPersistent); override;
    procedure ScaleParameters(const Value: Single); virtual;
    procedure ResetTargetValue(); virtual; abstract;
  published
    property Inertia: Single read FInertia write FInertia stored StoreInertia;
    property Speed: Single read FSpeed write FSpeed stored StoreSpeed;
    property SpeedLimit: Single read FSpeedLimit write FSpeedLimit stored StoreSpeedLimit;
    property Cutoff: Double read FCutoff write FCutoff stored StoreCutoff;
    property Enabled: Boolean read FEnabled write FEnabled default True;
  end;

  TVXNavigatorSmoothChangeSingle = class;
  TVXNavigatorSmoothChangeSingleGetEvent = function(const ASender: TVXNavigatorSmoothChangeSingle): Single of object;
  TVXNavigatorSmoothChangeSingleSetEvent = procedure(const ASender: TVXNavigatorSmoothChangeSingle; const AValue: Single) of object;

  { Smoothly change any Single value, so it will become TargetValue in the end.  }
  TVXNavigatorSmoothChangeSingle = class(TVXNavigatorSmoothChangeItem)
  private
    FTargetValue: Single;
    FOnGetCurrentValue: TVXNavigatorSmoothChangeSingleGetEvent;
    FOnSetCurrentValue: TVXNavigatorSmoothChangeSingleSetEvent;
  public
    class function FriendlyName: string; override;
    function Proceed(ADeltaTime: Double): Boolean; override;
    procedure Assign(Source: TPersistent); override;
    procedure ResetTargetValue(); override;
  published
    property TargetValue: Single read FTargetValue write FTargetValue;
    property OnGetCurrentValue: TVXNavigatorSmoothChangeSingleGetEvent read FOnGetCurrentValue write FOnGetCurrentValue;
    property OnSetCurrentValue: TVXNavigatorSmoothChangeSingleSetEvent read FOnSetCurrentValue write FOnSetCurrentValue;
  end;

  TVXNavigatorSmoothChangeVector = class;
  TVXNavigatorSmoothChangeVectorGetEvent = function(const ASender: TVXNavigatorSmoothChangeVector): TVector of object;
  TVXNavigatorSmoothChangeVectorSetEvent = procedure(const ASender: TVXNavigatorSmoothChangeVector; const AValue: TVector) of object;

  { Smoothly change any Vector4f value, so it will become TargetValue in the end.  }
  TVXNavigatorSmoothChangeVector = class(TVXNavigatorSmoothChangeItem)
  private
    FTargetValue: TVXCoordinates;
    FOnGetCurrentValue: TVXNavigatorSmoothChangeVectorGetEvent;
    FOnSetCurrentValue: TVXNavigatorSmoothChangeVectorSetEvent;
    procedure SetTargetValue(const Value: TVXCoordinates);
  public
    class function FriendlyName: string; override;
    function Proceed(ADeltaTime: Double): Boolean; override;
    procedure Assign(Source: TPersistent); override;
    constructor Create(aOwner: TXCollection); override;
    destructor Destroy; override;
    procedure ResetTargetValue(); override;
  published
    property TargetValue: TVXCoordinates read FTargetValue write SetTargetValue;
    property OnGetCurrentValue: TVXNavigatorSmoothChangeVectorGetEvent read FOnGetCurrentValue write FOnGetCurrentValue;
    property OnSetCurrentValue: TVXNavigatorSmoothChangeVectorSetEvent read FOnSetCurrentValue write FOnSetCurrentValue;
  end;

  TVXNavigatorSmoothChangeItemClass = class of TVXNavigatorSmoothChangeItem;

  { XCollection of TVXNavigatorSmoothChangeItem. }
  TVXNavigatorSmoothChangeItems = class(TXCollection)
  private
    function GetItems(const Index : Integer): TVXNavigatorSmoothChangeItem;
    procedure SetItems(const Index : Integer; const Value: TVXNavigatorSmoothChangeItem);
  protected
    procedure DoProceed(ADeltaTime: Double);
  public
    function Add(AClass : TVXNavigatorSmoothChangeItemClass): TVXNavigatorSmoothChangeItem;
    function CanAdd(AClass: TXCollectionItemClass): Boolean; override;
    class function ItemsClass: TXCollectionItemClass; override;
    property Items[const Index : Integer]: TVXNavigatorSmoothChangeItem read GetItems write
            SetItems; default;
  end;

  { This is wrapper for all parameters that affect how the AdjustDisanceTo[...] methods work }
  TVXNavigatorAdjustDistanceParameters = class(TVXNavigatorAbstractParameters)
  private
    FOldDistanceRatio: Single;
    FImpulseSpeed: Single;
    function StoreImpulseSpeed: Boolean;
  public
    constructor Create(AOwner: TPersistent); override;
    procedure Assign(Source: TPersistent); override;
    procedure ScaleParameters(const Value: Single); override;

    procedure AddImpulse(const Impulse: Single); virtual;
  published
    property ImpulseSpeed: Single read FImpulseSpeed write FImpulseSpeed stored StoreImpulseSpeed;
  end;

  { This is a wrapper for all parameters that affect how the AdjustDisanceTo[...]Ex methods work
     You need to set the TargetObject and desired distance to it,
     then call AdjustDisanceTo[...]Ex() in your Cadencer.OnProgress code. }
  TVXNavigatorAdjustDistanceParametersEx = class(TVXNavigatorAbstractParameters)
  private
    FSpeedLimit: Single;
    FTargetDistance: Single;
    function StoreSpeedLimit: Boolean;
    function StoreTargetDistance: Boolean;
  protected
    function StoreSpeed: Boolean; override;
    function StoreInertia: Boolean; override;
  public
    constructor Create(AOwner: TPersistent); override;
    procedure Assign(Source: TPersistent); override;
  published
    property TargetDistance: Single read FTargetDistance write FTargetDistance stored StoreTargetDistance;
    property SpeedLimit: Single read FSpeedLimit write FSpeedLimit stored StoreSpeedLimit;
  end;

  { This is a wrapper for all parameters that affect the smoothness of movement }
  TVXNavigatorInertiaParameters = class(TPersistent)
  private
    FOwner: TPersistent;
    OldTurnHorizontalAngle: Single;
    OldTurnVerticalAngle: Single;
    OldMoveForwardDistance: Single;
    OldStrafeHorizontalDistance: Single;
    OldStrafeVerticalDistance: Single;
    FTurnInertia: Single;
    FTurnSpeed: Single;
    FTurnMaxAngle: Single;
    FMovementAcceleration: Single;
    FMovementInertia: Single;
    FMovementSpeed: Single;
    function StoreTurnMaxAngle: Boolean;
    function StoreMovementAcceleration: Boolean;
    function StoreMovementInertia: Boolean;
    function StoreMovementSpeed: Boolean;
    function StoreTurnInertia: Boolean;
    function StoreTurnSpeed: Boolean;
  protected
    function GetOwner: TPersistent; override;
  public
    constructor Create(AOwner: TPersistent); virtual;
    procedure Assign(Source: TPersistent); override;
    procedure ScaleParameters(const Value: Single); virtual;
  published
    property MovementAcceleration: Single read FMovementAcceleration write FMovementAcceleration stored StoreMovementAcceleration;
    property MovementInertia: Single read FMovementInertia write FMovementInertia stored StoreMovementInertia;
    property MovementSpeed: Single read FMovementSpeed write FMovementSpeed stored StoreMovementSpeed;

    property TurnMaxAngle: Single read FTurnMaxAngle write FTurnMaxAngle stored StoreTurnMaxAngle;
    property TurnInertia: Single read FTurnInertia write FTurnInertia stored StoreTurnInertia;
    property TurnSpeed: Single read FTurnSpeed write FTurnSpeed stored StoreTurnSpeed;
  end;


  { This is a wrapper for all general inertia parameters.
     These properties mean that if ExpectedMaxFPS is 100, FAutoScaleMin is 0.1,
     FAutoScaleMax is 0.75 then the "safe range" for it to change is [10..75].
     If these bounds are violated, then ExpectedMaxFPS is automaticly increased
     or decreased by AutoScaleMult. }
  TVXNavigatorGeneralParameters = class(TPersistent)
  private
    FOwner: TPersistent;
    FAutoScaleMin: Single;
    FAutoScaleMax: Single;
    FAutoScaleMult: Single;
    function StoreAutoScaleMax: Boolean;
    function StoreAutoScaleMin: Boolean;
    function StoreAutoScaleMult: Boolean;
  protected
    function GetOwner: TPersistent; override;
  public
    constructor Create(AOwner: TPersistent); virtual;
    procedure Assign(Source: TPersistent); override;
  published
    property AutoScaleMin: Single read FAutoScaleMin write FAutoScaleMin stored StoreAutoScaleMin;
    property AutoScaleMax: Single read FAutoScaleMax write FAutoScaleMax stored StoreAutoScaleMax;
    property AutoScaleMult: Single read FAutoScaleMult write FAutoScaleMult stored StoreAutoScaleMult;
  end;


  { This is a wrapper for all parameters that effect how the TVXBaseSceneObject.MoveObjectAround() procedure works}
  TVXNavigatorMoveAroundParameters = class(TPersistent)
  private
    FOwner: TPersistent;
    FTargetObject: TVXBaseSceneObject;
    FOldPitchInertiaAngle : Single;
    FOldTurnInertiaAngle  : Single;
    FPitchSpeed : Single;
    FTurnSpeed  : Single;
    FInertia          : Single;
    FMaxAngle         : Single;
    FCutoff: Double;
    function StoreInertia: Boolean;
    function StoreMaxAngle: Boolean;
    function StorePitchSpeed: Boolean;
    function StoreTurnSpeed: Boolean;
    procedure SetTargetObject(const Value: TVXBaseSceneObject);
    function StoreCutoff: Boolean;
  protected
    function GetOwner: TPersistent; override;
  public
    constructor Create(AOwner: TPersistent); virtual;
    procedure Assign(Source: TPersistent); override;
    procedure ScaleParameters(const Value: Single); virtual;
  published
    property Inertia: Single read FInertia write FInertia stored StoreInertia;
    property MaxAngle: Single read FMaxAngle write FMaxAngle stored StoreMaxAngle;
    property PitchSpeed: Single read FPitchSpeed write FPitchSpeed stored StorePitchSpeed;
    property TurnSpeed: Single read FTurnSpeed write FTurnSpeed stored StoreTurnSpeed;
    property TargetObject: TVXBaseSceneObject read FTargetObject write SetTargetObject;
    property Cutoff: Double read FCutoff write FCutoff stored StoreCutoff;    
  end;


  { This is the component for moving a TVXBaseSceneObject, and all
       classes based on it, this includes all the objects from the Scene Editor.
     It uses complex smoothing algorithms, most of which are FPS-dependant.
     Make sure your limit your FPS and set MaxExpectedDeltaTime to a value
     that is aproximatly 5 times less than your usual deltatime. }
  TVXSmoothNavigator = class(TVXNavigator)
  private
    FMaxExpectedDeltaTime: Double;
    FInertiaParams: TVXNavigatorInertiaParameters;
    FGeneralParams: TVXNavigatorGeneralParameters;
    FMoveAroundParams: TVXNavigatorMoveAroundParameters;
    FAdjustDistanceParams: TVXNavigatorAdjustDistanceParameters;
    FAdjustDistanceParamsEx: TVXNavigatorAdjustDistanceParametersEx;
    FCustomAnimatedItems: TVXNavigatorSmoothChangeItems;
    procedure SetInertiaParams(const Value: TVXNavigatorInertiaParameters);
    function StoreMaxExpectedDeltaTime: Boolean;
    procedure SetGeneralParams(const Value: TVXNavigatorGeneralParameters);
    procedure SetMoveAroundParams(const Value: TVXNavigatorMoveAroundParameters);
    procedure SetAdjustDistanceParams(const Value: TVXNavigatorAdjustDistanceParameters);
    procedure SetAdjustDistanceParamsEx(
      const Value: TVXNavigatorAdjustDistanceParametersEx);
    procedure SetCustomAnimatedItems(
      const Value: TVXNavigatorSmoothChangeItems);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    // Constructors-destructors.
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    // From TVXNavigator. Probably, should not be public.
    procedure SetObject(Value: TVXBaseSceneObject); override;
    // Uses InertiaParams.
    procedure TurnHorizontal(Angle: Single; ADeltaTime: Double); virtual;
    procedure TurnVertical(Angle: Single; ADeltaTime: Double); virtual;
    procedure FlyForward(const Plus, Minus: Boolean; ADeltaTime: Double; const Accelerate: Boolean = False); virtual;
    procedure MoveForward(const Plus, Minus: Boolean; ADeltaTime: Double; const Accelerate: Boolean = False); virtual;
    procedure StrafeHorizontal(const Plus, Minus: Boolean; ADeltaTime: Double; const Accelerate: Boolean = False); virtual;
    procedure StrafeVertical(const Plus, Minus: Boolean; ADeltaTime: Double; const Accelerate: Boolean = False); virtual;
    // Uses MoveAroundParams. Returns True, if object was actually moved.
    function MoveAroundTarget(const PitchDelta, TurnDelta : Single; const ADeltaTime: Double): Boolean; virtual;
    function MoveObjectAround(const AObject: TVXBaseSceneObject; PitchDelta, TurnDelta : Single; ADeltaTime: Double): Boolean; virtual;
    // Uses AdjustDistanceParams.
    function AdjustDistanceToPoint(const  APoint: TVector; const DistanceRatio : Single; ADeltaTime: Double): Boolean; virtual;
    function AdjustDistanceToTarget(const DistanceRatio : Single; const ADeltaTime: Double): Boolean; virtual;
    // Uses AdjustDistanceParamsEx.
    function AdjustDistanceToPointEx(const  APoint: TVector; ADeltaTime: Double): Boolean; virtual;
    function AdjustDistanceToTargetEx(const ADeltaTime: Double): Boolean; virtual;
    // Uses CustomAnimatedItems.
    procedure AnimateCustomItems(const ADeltaTime: Double); virtual;
    // Uses GeneralParams.
      { In ScaleParameters, Value should be around 1. }
    procedure ScaleParameters(const Value: Single); virtual;
    procedure AutoScaleParameters(const FPS: Single); virtual;
    procedure AutoScaleParametersUp(const FPS: Single); virtual;
  published
    property MaxExpectedDeltaTime: Double read FMaxExpectedDeltaTime write FMaxExpectedDeltaTime stored StoreMaxExpectedDeltaTime;
    property InertiaParams: TVXNavigatorInertiaParameters read FInertiaParams write SetInertiaParams;
    property GeneralParams: TVXNavigatorGeneralParameters read FGeneralParams write SetGeneralParams;
    property MoveAroundParams: TVXNavigatorMoveAroundParameters read FMoveAroundParams write SetMoveAroundParams;
    property AdjustDistanceParams: TVXNavigatorAdjustDistanceParameters read FAdjustDistanceParams write SetAdjustDistanceParams;
    property AdjustDistanceParamsEx: TVXNavigatorAdjustDistanceParametersEx read FAdjustDistanceParamsEx write SetAdjustDistanceParamsEx;
    property CustomAnimatedItems: TVXNavigatorSmoothChangeItems read FCustomAnimatedItems write SetCustomAnimatedItems;
  end;


  { This is the component which reads the userinput and transform it into action.
	    Mouselook(ADeltaTime: double) : handles mouse look... Should be called
                           in the Cadencer event. (Though it works everywhere!)
	   The four properties to get you started are:
	    InvertMouse     : Inverts the mouse Y axis.
	    AutoUpdateMouse : If enabled (by defaul), than handles all mouse updates.
	    GLNavigator     : The Navigator which receives the user movement.
	    GLVertNavigator : The Navigator which if set receives the vertical user
                           movement. Used mostly for cameras.... }
  TVXSmoothUserInterface = class(TComponent)
  private
    FAutoUpdateMouse: Boolean;
    FMouseLookActive: Boolean;
    FSmoothNavigator: TVXSmoothNavigator;
    FSmoothVertNavigator: TVXSmoothNavigator;
    FInvertMouse: Boolean;
    FOriginalMousePos: TVXCoordinates2;
    procedure SetSmoothNavigator(const Value: TVXSmoothNavigator); virtual;
    procedure SetOriginalMousePos(const Value: TVXCoordinates2); virtual;
    procedure SetSmoothVertNavigator(const Value: TVXSmoothNavigator); virtual;
    procedure SetMouseLookActive(const Value: Boolean); virtual;
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure TurnHorizontal(const Angle : Single; const ADeltaTime: Double); virtual;
    procedure TurnVertical(const Angle : Single; const ADeltaTime: Double); virtual;
    procedure MouseLookActiveToggle; virtual;

    function MouseLook(const ADeltaTime: Double): Boolean; overload;
    function MouseLook(const NewXY: TVXPoint; const ADeltaTime: Double): Boolean; overload;
    function MouseLook(const NewX, NewY: Integer; const ADeltaTime: Double): Boolean; overload;
  published
    property AutoUpdateMouse: Boolean read FAutoUpdateMouse write FAutoUpdateMouse default True;
    property MouseLookActive: Boolean read FMouseLookActive write SetMouseLookActive default False;
    property SmoothVertNavigator: TVXSmoothNavigator read FSmoothVertNavigator write SetSmoothVertNavigator;
    property SmoothNavigator: TVXSmoothNavigator read FSmoothNavigator write SetSmoothNavigator;
    property InvertMouse: Boolean read FInvertMouse write FInvertMouse default False;
    property OriginalMousePos: TVXCoordinates2 read FOriginalMousePos write SetOriginalMousePos;
  end;

//-----------------------------------------------------------
implementation
//-----------------------------------------------------------

const
  EPS =  0.001;
  EPS2 = 0.0001;
  EPS8 = 0.00000001;

{ TVXSmoothNavigator }

constructor TVXSmoothNavigator.Create(AOwner: TComponent);
begin
  inherited;
  FMaxExpectedDeltaTime := 0.001;
  FInertiaParams := TVXNavigatorInertiaParameters.Create(Self);
  FGeneralParams := TVXNavigatorGeneralParameters.Create(Self);
  FMoveAroundParams := TVXNavigatorMoveAroundParameters.Create(Self);
  FAdjustDistanceParams := TVXNavigatorAdjustDistanceParameters.Create(Self);
  FAdjustDistanceParamsEx := TVXNavigatorAdjustDistanceParametersEx.Create(Self);
  FCustomAnimatedItems := TVXNavigatorSmoothChangeItems.Create(Self);
end;

destructor TVXSmoothNavigator.Destroy;
begin
  FInertiaParams.Free;
  FGeneralParams.Free;
  FMoveAroundParams.Free;
  FAdjustDistanceParams.Free;
  FAdjustDistanceParamsEx.Free;
  FCustomAnimatedItems.Free;
  inherited;
end;

procedure TVXSmoothNavigator.SetInertiaParams(
  const Value: TVXNavigatorInertiaParameters);
begin
  FInertiaParams.Assign(Value);
end;

procedure TVXSmoothNavigator.TurnHorizontal(Angle: Single; ADeltaTime: Double);
var
  FinalAngle: Single;
begin
  with FInertiaParams do
  begin
    FinalAngle := 0;
    Angle := Angle * FTurnSpeed;
    while ADeltaTime > FMaxExpectedDeltaTime do
    begin
      Angle := ClampValue((Angle * FMaxExpectedDeltaTime + OldTurnHorizontalAngle * FTurnInertia) / (FTurnInertia + 1), -FTurnMaxAngle, FTurnMaxAngle);
      OldTurnHorizontalAngle := Angle;
      ADeltaTime := ADeltaTime - FMaxExpectedDeltaTime;
      FinalAngle := FinalAngle + Angle;
    end;
  end;

  if (Abs(FinalAngle) > EPS) then
    inherited TurnHorizontal(FinalAngle);
end;

procedure TVXSmoothNavigator.TurnVertical(Angle: Single; ADeltaTime: Double);
var
  FinalAngle: Single;
begin
  with FInertiaParams do
  begin
    FinalAngle := 0;
    Angle := Angle * FTurnSpeed;
    while ADeltaTime > FMaxExpectedDeltaTime do
    begin
      Angle := ClampValue((Angle * FMaxExpectedDeltaTime + OldTurnVerticalAngle * FTurnInertia) / (FTurnInertia + 1), -FTurnMaxAngle, FTurnMaxAngle);
      OldTurnVerticalAngle := Angle;
      ADeltaTime := ADeltaTime - FMaxExpectedDeltaTime;
      FinalAngle := FinalAngle + Angle;
    end;
  end;

  if (Abs(FinalAngle) > EPS) then
    inherited TurnVertical(FinalAngle);
end;


procedure TVXSmoothNavigator.MoveForward(const Plus, Minus: Boolean; ADeltaTime: Double; const Accelerate: Boolean = False);
var
  FinalDistance: Single;
  Distance:      Single;
begin
  with FInertiaParams do
  begin
    if Plus then
      Distance := FMovementSpeed
    else if Minus then
      Distance := -FMovementSpeed
    else
      Distance := 0;

    if Accelerate then
      Distance := Distance * FMovementAcceleration;

    FinalDistance := 0;

    while ADeltaTime > FMaxExpectedDeltaTime do
    begin
      OldMoveForwardDistance := (Distance * FMaxExpectedDeltaTime + OldMoveForwardDistance * FMovementInertia) / (FMovementInertia + 1);
      ADeltaTime := ADeltaTime - FMaxExpectedDeltaTime;
      FinalDistance := FinalDistance + OldMoveForwardDistance;
    end;
  end;

  if Abs(FinalDistance) > EPS then
    inherited MoveForward(FinalDistance);
end;

procedure TVXSmoothNavigator.FlyForward(const Plus, Minus: Boolean; ADeltaTime: Double; const Accelerate: Boolean = False);
var
  FinalDistance: Single;
  Distance:      Single;
begin
  with FInertiaParams do
  begin
    if Plus then
      Distance := FMovementSpeed
    else if Minus then
      Distance := -FMovementSpeed
    else
      Distance := 0;

    if Accelerate then
      Distance := Distance * FMovementAcceleration;

    FinalDistance := 0;

    while ADeltaTime > FMaxExpectedDeltaTime do
    begin
      OldMoveForwardDistance := (Distance * FMaxExpectedDeltaTime + OldMoveForwardDistance * FMovementInertia) / (FMovementInertia + 1);
      ADeltaTime := ADeltaTime - FMaxExpectedDeltaTime;
      FinalDistance := FinalDistance + OldMoveForwardDistance;
    end;
  end;

  if Abs(FinalDistance) > EPS then
    inherited FlyForward(FinalDistance);
end;

procedure TVXSmoothNavigator.StrafeHorizontal(const Plus, Minus: Boolean; ADeltaTime: Double; const Accelerate: Boolean = False);
var
  FinalDistance: Single;
  Distance:      Single;
begin
  with FInertiaParams do
  begin
    if Plus then
      Distance := FMovementSpeed
    else if Minus then
      Distance := -FMovementSpeed
    else
      Distance := 0;

    if Accelerate then
      Distance := Distance * FMovementAcceleration;

    FinalDistance := 0;

    while ADeltaTime > FMaxExpectedDeltaTime do
    begin
      OldStrafeHorizontalDistance := (Distance * FMaxExpectedDeltaTime + OldStrafeHorizontalDistance * FMovementInertia) / (FMovementInertia + 1);
      ADeltaTime := ADeltaTime - FMaxExpectedDeltaTime;
      FinalDistance := FinalDistance + OldStrafeHorizontalDistance;
    end;
  end;

  if Abs(FinalDistance) > EPS then
    inherited StrafeHorizontal(FinalDistance);
end;

procedure TVXSmoothNavigator.StrafeVertical(const Plus, Minus: Boolean; ADeltaTime: Double; const Accelerate: Boolean = False);
var
  FinalDistance: Single;
  Distance:      Single;
begin
  with FInertiaParams do
  begin
    if Plus then
      Distance := FMovementSpeed
    else if Minus then
      Distance := -FMovementSpeed
    else
      Distance := 0;

    if Accelerate then
      Distance := Distance * FMovementAcceleration;

    FinalDistance := 0;

    while ADeltaTime > FMaxExpectedDeltaTime do
    begin
      OldStrafeVerticalDistance := (Distance * FMaxExpectedDeltaTime + OldStrafeVerticalDistance * FMovementInertia) / (FMovementInertia + 1);
      ADeltaTime := ADeltaTime - FMaxExpectedDeltaTime;
      FinalDistance := FinalDistance + OldStrafeVerticalDistance;
    end;
  end;

  if Abs(FinalDistance) > EPS then
    inherited StrafeVertical(FinalDistance);
end;

procedure TVXSmoothNavigator.AutoScaleParameters(const FPS: Single);
begin
  with FGeneralParams do
  begin
    if FPS > FAutoScaleMax / FMaxExpectedDeltatime then
      ScaleParameters(FAutoScaleMult)
    else if FPS < FAutoScaleMin / FMaxExpectedDeltatime then
      ScaleParameters(1/FAutoScaleMult);
  end;
end;


procedure TVXSmoothNavigator.AutoScaleParametersUp(const FPS: Single);
begin
  with FGeneralParams do
  begin
    if FPS > FAutoScaleMax / FMaxExpectedDeltatime then
      ScaleParameters(FAutoScaleMult)
  end;
end;

procedure TVXSmoothNavigator.ScaleParameters(const Value: Single);
begin
  Assert(Value > 0);
  FMaxExpectedDeltatime := FMaxExpectedDeltatime / Value;
  FInertiaParams.ScaleParameters(Value);
  FMoveAroundParams.ScaleParameters(Value);
  FAdjustDistanceParams.ScaleParameters(Value);
end;

function TVXSmoothNavigator.StoreMaxExpectedDeltaTime: Boolean;
begin
  Result := Abs(FMaxExpectedDeltaTime - 0.001) > EPS2;
end;

procedure TVXSmoothNavigator.SetGeneralParams(
  const Value: TVXNavigatorGeneralParameters);
begin
  FGeneralParams.Assign(Value);
end;

procedure TVXSmoothNavigator.SetMoveAroundParams(
  const Value: TVXNavigatorMoveAroundParameters);
begin
  FMoveAroundParams.Assign(Value);
end;

procedure TVXSmoothNavigator.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if Operation = opRemove then
  begin
    if AComponent = FMoveAroundParams.FTargetObject then
      FMoveAroundParams.FTargetObject := nil;
  end;
end;

procedure TVXSmoothNavigator.SetObject(Value: TVXBaseSceneObject);
var
  I: Integer;
begin
  inherited;
  // Try to detect a TargetObject.
  if Value <> nil then
    if FMoveAroundParams.TargetObject = nil then
    begin
      // May be it is a camera...
      if Value is TVXCamera then
        FMoveAroundParams.TargetObject := TVXCamera(Value).TargetObject
      else
      begin
        // May be it has camera children...
        if Value.Count <> 0 then
          for I := 0 to Value.Count - 1 do
            if Value.Children[I] is TVXCamera then
            begin
              FMoveAroundParams.TargetObject := TVXCamera(Value.Children[I]).TargetObject;
              Exit;
            end;
      end;
    end;
end;

function TVXSmoothNavigator.MoveAroundTarget(const PitchDelta, TurnDelta: Single;
  const ADeltaTime: Double): Boolean;
begin
  Result := MoveObjectAround(FMoveAroundParams.FTargetObject, PitchDelta, TurnDelta, ADeltaTime);
end;

function TVXSmoothNavigator.MoveObjectAround(
  const AObject: TVXBaseSceneObject; PitchDelta, TurnDelta: Single;
  ADeltaTime: Double): Boolean;
var
  FinalPitch: Single;
  FinalTurn:  Single;

  lUp: TVector;
begin
  Result := False;
  FinalPitch := 0;
  FinalTurn := 0;
  with FMoveAroundParams do
  begin
    PitchDelta := PitchDelta * FPitchSpeed;
    TurnDelta := TurnDelta * FTurnSpeed;

    while ADeltaTime > FMaxExpectedDeltatime do
    begin
      PitchDelta := ClampValue((PitchDelta * FMaxExpectedDeltatime + FOldPitchInertiaAngle * FInertia) / (FInertia + 1), - FMaxAngle, FMaxAngle);
      FOldPitchInertiaAngle := PitchDelta;
      FinalPitch := FinalPitch + PitchDelta;
      TurnDelta := ClampValue((TurnDelta * FMaxExpectedDeltatime + FOldTurnInertiaAngle * FInertia) / (FInertia + 1), - FMaxAngle, FMaxAngle);
      FOldTurnInertiaAngle := TurnDelta;
      FinalTurn := FinalTurn + TurnDelta;

      ADeltaTime := ADeltaTime - FMaxExpectedDeltatime;
    end;

    if UseVirtualUp then
      lUp := VirtualUp.AsVector
    else
      lUp := MovingObject.AbsoluteUp;

    if (Abs(FinalPitch) > FCutOff) or (Abs(FinalTurn) > FCutOff) then
    begin
      MovingObject.AbsolutePosition := VXS.VectorGeometry.MoveObjectAround(
        MovingObject.AbsolutePosition, lUp, AObject.AbsolutePosition, FinalPitch, FinalTurn);
      Result := True;
    end;
  end;
end;


function TVXSmoothNavigator.AdjustDistanceToPoint(const APoint: TVector;
  const DistanceRatio: Single; ADeltaTime: Double): Boolean;

  // Based on TVXCamera.AdjustDistanceToTarget
  procedure DoAdjustDistanceToPoint(const DistanceRatio: Single);
  var
    vect: TVector;
  begin
    vect := VectorSubtract(MovingObject.AbsolutePosition, APoint);
    ScaleVector(vect, (distanceRatio - 1));
    AddVector(vect, MovingObject.AbsolutePosition);
    if Assigned(MovingObject.Parent) then
       vect := MovingObject.Parent.AbsoluteToLocal(vect);
    MovingObject.Position.AsVector := vect;
    Result := True;
  end;

var
  FinalDistanceRatio: Single;
  TempDistanceRatio:  Single;
begin
  with FAdjustDistanceParams do
  begin
    TempDistanceRatio := DistanceRatio * FSpeed;
    FinalDistanceRatio := 0;
    while ADeltaTime > FMaxExpectedDeltaTime do
    begin
      TempDistanceRatio := (TempDistanceRatio * FMaxExpectedDeltaTime + FOldDistanceRatio * FInertia) / (FInertia + 1);
      FOldDistanceRatio := TempDistanceRatio;
      ADeltaTime := ADeltaTime - FMaxExpectedDeltaTime;
      FinalDistanceRatio := FinalDistanceRatio + FOldDistanceRatio / FMaxExpectedDeltaTime;
    end;

    if Abs(FinalDistanceRatio) > FCutoff then
    begin
      if FinalDistanceRatio > 0 then
        DoAdjustDistanceToPoint(1 / (1 + FinalDistanceRatio))
      else
        DoAdjustDistanceToPoint(1 * (1 - FinalDistanceRatio))
    end
    else
      Result := False;
  end;
end;

function TVXSmoothNavigator.AdjustDistanceToTarget(const DistanceRatio: Single;
  const ADeltaTime: Double): Boolean;
begin
  Assert(FMoveAroundParams.FTargetObject <> nil);
  Result := AdjustDistanceToPoint(FMoveAroundParams.FTargetObject.AbsolutePosition,
                        DistanceRatio, ADeltaTime);
end;

procedure TVXSmoothNavigator.SetAdjustDistanceParams(
  const Value: TVXNavigatorAdjustDistanceParameters);
begin
  FAdjustDistanceParams.Assign(Value);
end;

function TVXSmoothNavigator.AdjustDistanceToPointEx(const APoint: TVector;
  ADeltaTime: Double): Boolean;

var
  lAbsolutePosition: TVector;
  lCurrentDistance: Single;
  lDistanceDifference, lTempCurrentDistance: Single;

  procedure DoAdjustDistanceToPoint(const DistanceValue: Single);
  var
    vect: TVector;
  begin
    vect := VectorSubtract(APoint, lAbsolutePosition);
    NormalizeVector(vect);
    ScaleVector(vect, DistanceValue);
    MovingObject.AbsolutePosition := VectorAdd(lAbsolutePosition, vect);
    Result := True;
  end;

begin
  lAbsolutePosition := MovingObject.AbsolutePosition;
  lCurrentDistance := VectorDistance(lAbsolutePosition, APoint);
  lDistanceDifference := lCurrentDistance - FAdjustDistanceParamsEx.FTargetDistance;

  with FAdjustDistanceParamsEx do
  begin
    lTempCurrentDistance := 0;
    while ADeltaTime > FMaxExpectedDeltaTime do
    begin
      lTempCurrentDistance := (FSpeed * FMaxExpectedDeltaTime * lDistanceDifference * FInertia) / (FInertia + 1);
//      lTempCurrentDistance := (FSpeed * FMaxExpectedDeltaTime + lDistanceDifference * FInertia) / (FInertia + 1);-  this also works, but a bit different.
      ADeltaTime := ADeltaTime - FMaxExpectedDeltaTime;
    end;
    
    lTempCurrentDistance :=  ClampValue(lTempCurrentDistance, -FSpeedLimit * ADeltaTime, FSpeedLimit * ADeltaTime);

    if Abs(lTempCurrentDistance) > FCutoff then
      DoAdjustDistanceToPoint(lTempCurrentDistance)
    else
      Result := False;
  end;
end;

function TVXSmoothNavigator.AdjustDistanceToTargetEx(
  const ADeltaTime: Double): Boolean;
begin
  Assert(FMoveAroundParams.FTargetObject <> nil);
  Result := AdjustDistanceToPointEx(FMoveAroundParams.FTargetObject.AbsolutePosition,
                          ADeltaTime);
end;

procedure TVXSmoothNavigator.SetAdjustDistanceParamsEx(
  const Value: TVXNavigatorAdjustDistanceParametersEx);
begin
  FAdjustDistanceParamsEx.Assign(Value);
end;

procedure TVXSmoothNavigator.AnimateCustomItems(const ADeltaTime: Double);
begin
  FCustomAnimatedItems.DoProceed(ADeltaTime);
end;

procedure TVXSmoothNavigator.SetCustomAnimatedItems(
  const Value: TVXNavigatorSmoothChangeItems);
begin
  FCustomAnimatedItems.Assign(Value);
end;

{ TVXSmoothUserInterface }

function TVXSmoothUserInterface.MouseLook(
  const ADeltaTime: Double): Boolean;
var
  MousePos: TVXPoint;
begin
  Assert(FAutoUpdateMouse, 'AutoUpdateMouse must be True to use this function');
  if FMouseLookActive then
  begin
    GLGetCursorPos(MousePos);
    Result := Mouselook(MousePos.X, MousePos.Y, ADeltaTime);
    GLSetCursorPos(Round(OriginalMousePos.X), Round(OriginalMousePos.Y));
  end
  else
    Result := False;
end;

function TVXSmoothUserInterface.Mouselook(const NewX, NewY: Integer; const ADeltaTime: Double): Boolean;
var
  DeltaX, DeltaY: Single;
begin
  Result := False;
  if FMouseLookActive then
  begin
    Deltax := (NewX - FOriginalMousePos.X);
    Deltay := (FOriginalMousePos.Y - NewY);

    if InvertMouse then
      DeltaY := -DeltaY;

    SmoothNavigator.TurnHorizontal(DeltaX, ADeltaTime);
    SmoothNavigator.TurnVertical(DeltaY, ADeltaTime);

    Result := (DeltaX <> 0) or (DeltaY <> 0);
  end;
end;


function TVXSmoothUserInterface.MouseLook(const NewXY: TVXPoint; const ADeltaTime: Double): Boolean;
begin
  Result := Mouselook(NewXY.X, NewXY.Y, ADeltaTime);
end;

constructor TVXSmoothUserInterface.Create(AOwner: TComponent);
begin
  inherited;
  FMouseLookActive := False;
  FAutoUpdateMouse := True;
  FOriginalMousePos := TVXCoordinates2.CreateInitialized(Self,
                             VectorMake(GLGetScreenWidth div 2,
                             GLGetScreenHeight div 2, 0, 0), csPoint2D);
end;

procedure TVXSmoothUserInterface.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) then
  begin
    if AComponent = FSmoothNavigator then
      FSmoothNavigator := nil;
    if AComponent = FSmoothVertNavigator then
      FSmoothNavigator := nil;
  end;
end;

procedure TVXSmoothUserInterface.SetSmoothNavigator(
  const Value: TVXSmoothNavigator);
begin
  if FSmoothNavigator <> nil then
    FSmoothNavigator.RemoveFreeNotification(Self);

  FSmoothNavigator := Value;

  if FSmoothNavigator <> nil then
    FSmoothNavigator.FreeNotification(Self);
end;

destructor TVXSmoothUserInterface.Destroy;
begin
  FOriginalMousePos.Destroy;
  inherited;
end;

procedure TVXSmoothUserInterface.SetOriginalMousePos(
  const Value: TVXCoordinates2);
begin
  FOriginalMousePos.Assign(Value);
end;

procedure TVXSmoothUserInterface.SetSmoothVertNavigator(
  const Value: TVXSmoothNavigator);
begin
  if FSmoothVertNavigator <> nil then
    FSmoothVertNavigator.RemoveFreeNotification(Self);

  FSmoothVertNavigator := Value;

  if FSmoothVertNavigator <> nil then
    FSmoothVertNavigator.FreeNotification(Self);
end;

procedure TVXSmoothUserInterface.MouseLookActiveToggle;
begin
  if FMouseLookActive then
    SetMouseLookActive(False)
  else
    SetMouseLookActive(True)
end;

procedure TVXSmoothUserInterface.SetMouseLookActive(const Value: Boolean);
var
  MousePos: TVXPoint;
begin
  if FMouseLookActive = Value then Exit;
  FMouseLookActive := Value;
  if FMouseLookActive then
  begin
    if FAutoUpdateMouse then
    begin
      GLGetCursorPos(MousePos);
      FOriginalMousePos.SetPoint2D(MousePos.X, MousePos.Y);
      GLShowCursor(False);
    end;
  end
  else
  begin
    if FAutoUpdateMouse then
      GLShowCursor(True);
  end;
end;

procedure TVXSmoothUserInterface.TurnHorizontal(const Angle: Single;
  const ADeltaTime: Double);
begin
  FSmoothNavigator.TurnHorizontal(Angle, ADeltaTime);
end;

procedure TVXSmoothUserInterface.TurnVertical(const Angle: Single;
  const ADeltaTime: Double);
begin
  if Assigned(FSmoothNavigator) then
    FSmoothNavigator.TurnVertical(Angle, ADeltaTime)
  else
    FSmoothVertNavigator.TurnVertical(Angle, ADeltaTime);
end;

{ TVXNavigatorInertiaParameters }

procedure TVXNavigatorInertiaParameters.Assign(Source: TPersistent);
begin
  if Source is TVXNavigatorInertiaParameters then
  begin
    FMovementAcceleration := TVXNavigatorInertiaParameters(Source).FMovementAcceleration;
    FMovementInertia := TVXNavigatorInertiaParameters(Source).FMovementInertia;
    FMovementSpeed := TVXNavigatorInertiaParameters(Source).FMovementSpeed;
    FTurnMaxAngle := TVXNavigatorInertiaParameters(Source).FTurnMaxAngle;
    FTurnInertia := TVXNavigatorInertiaParameters(Source).FTurnInertia;
    FTurnSpeed := TVXNavigatorInertiaParameters(Source).FTurnSpeed;
  end
  else
    inherited; //to the pit of doom ;)
end;

constructor TVXNavigatorInertiaParameters.Create(AOwner: TPersistent);
begin
  FOwner := AOwner;

  FTurnInertia := 150;
  FTurnSpeed := 50;
  FTurnMaxAngle := 0.5;

  FMovementAcceleration := 7;
  FMovementInertia := 200;
  FMovementSpeed := 200;
end;

function TVXNavigatorInertiaParameters.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

procedure TVXNavigatorInertiaParameters.ScaleParameters(
  const Value: Single);
begin
  Assert(Value > 0);

  if Value > 1 then
  begin
    FMovementInertia := FMovementInertia * PowerSingle(2, 1 / Value);
    FTurnInertia := FTurnInertia * PowerSingle(2, 1 / Value);
  end
  else
  begin
    FMovementInertia := FMovementInertia / PowerSingle(2, Value);
    FTurnInertia := FTurnInertia / PowerSingle(2, Value);
  end;
  FTurnMaxAngle := FTurnMaxAngle / Value;
  FTurnSpeed := FTurnSpeed * Value;
end;

function TVXNavigatorInertiaParameters.StoreTurnMaxAngle: Boolean;
begin
  Result := Abs(FTurnMaxAngle - 0.5) > EPS;
end;

function TVXNavigatorInertiaParameters.StoreMovementAcceleration: Boolean;
begin
  Result := Abs(FMovementAcceleration - 7) > EPS;
end;

function TVXNavigatorInertiaParameters.StoreMovementInertia: Boolean;
begin
  Result := Abs(FMovementInertia - 200) > EPS;
end;

function TVXNavigatorInertiaParameters.StoreMovementSpeed: Boolean;
begin
  Result := Abs(FMovementSpeed - 200) > EPS;
end;

function TVXNavigatorInertiaParameters.StoreTurnInertia: Boolean;
begin
  Result := Abs(FTurnInertia - 150) > EPS;
end;

function TVXNavigatorInertiaParameters.StoreTurnSpeed: Boolean;
begin
  Result := Abs(FTurnSpeed - 50) > EPS;
end;

{ TVXNavigatorGeneralParameters }

procedure TVXNavigatorGeneralParameters.Assign(Source: TPersistent);
begin
  if Source is TVXNavigatorGeneralParameters then
  begin
    FAutoScaleMin := TVXNavigatorGeneralParameters(Source).FAutoScaleMin;
    FAutoScaleMax := TVXNavigatorGeneralParameters(Source).FAutoScaleMax;
    FAutoScaleMult := TVXNavigatorGeneralParameters(Source).FAutoScaleMult;
  end
  else
    inherited; //die!
end;

constructor TVXNavigatorGeneralParameters.Create(AOwner: TPersistent);
begin
  FOwner := AOwner;
  FAutoScaleMin := 0.1;
  FAutoScaleMax := 0.75;
  FAutoScaleMult := 2;
end;

function TVXNavigatorGeneralParameters.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

function TVXNavigatorGeneralParameters.StoreAutoScaleMax: Boolean;
begin
  Result := Abs(FAutoScaleMax - 0.75) > EPS;
end;

function TVXNavigatorGeneralParameters.StoreAutoScaleMin: Boolean;
begin
  Result := Abs(FAutoScaleMin - 0.1) > EPS;
end;

function TVXNavigatorGeneralParameters.StoreAutoScaleMult: Boolean;
begin
  Result := Abs(FAutoScaleMult - 2) > EPS;
end;

{ TVXNavigatorMoveAroundParameters }

procedure TVXNavigatorMoveAroundParameters.Assign(Source: TPersistent);
begin
  if Source is TVXNavigatorMoveAroundParameters then
  begin
    FMaxAngle := TVXNavigatorMoveAroundParameters(Source).FMaxAngle;
    FInertia :=  TVXNavigatorMoveAroundParameters(Source).FInertia;
    FPitchSpeed :=  TVXNavigatorMoveAroundParameters(Source).FPitchSpeed;
    FTurnSpeed :=  TVXNavigatorMoveAroundParameters(Source).FTurnSpeed;
    FCutoff :=  TVXNavigatorMoveAroundParameters(Source).FCutoff;
    SetTargetObject(TVXNavigatorMoveAroundParameters(Source).FTargetObject);
  end
  else
    inherited; //die
end;

constructor TVXNavigatorMoveAroundParameters.Create(AOwner: TPersistent);
begin
  FOwner := AOwner;
  FPitchSpeed := 500;
  FTurnSpeed  := 500;
  FInertia    := 65;
  FMaxAngle   := 1.5;
  FCutoff     := EPS2;
end;

function TVXNavigatorMoveAroundParameters.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

procedure TVXNavigatorMoveAroundParameters.ScaleParameters(
  const Value: Single);
begin
  Assert(Value > 0);

  if Value < 1 then
    FInertia := FInertia / PowerSingle(2, Value)
  else
    FInertia := FInertia * PowerSingle(2, 1 / Value);

  FMaxAngle := FMaxAngle / Value;
  FPitchSpeed := FPitchSpeed * Value;
  FTurnSpeed := FTurnSpeed * Value;
end;

procedure TVXNavigatorMoveAroundParameters.SetTargetObject(
  const Value: TVXBaseSceneObject);
begin
  if FTargetObject <> nil then
    if FOwner is TVXSmoothNavigator then
      FTargetObject.RemoveFreeNotification(TVXSmoothNavigator(FOwner));

  FTargetObject := Value;

  if FTargetObject <> nil then
    if FOwner is TVXSmoothNavigator then
      FTargetObject.FreeNotification(TVXSmoothNavigator(FOwner));
end;

function TVXNavigatorMoveAroundParameters.StoreCutoff: Boolean;
begin
  Result := Abs(FCutoff - EPS2) > EPS8;
end;

function TVXNavigatorMoveAroundParameters.StoreInertia: Boolean;
begin
  Result := Abs(FInertia - 65) > EPS;
end;

function TVXNavigatorMoveAroundParameters.StoreMaxAngle: Boolean;
begin
  Result := Abs(FMaxAngle - 1.5) > EPS;
end;

function TVXNavigatorMoveAroundParameters.StorePitchSpeed: Boolean;
begin
  Result := Abs(FPitchSpeed - 500) > EPS;
end;

function TVXNavigatorMoveAroundParameters.StoreTurnSpeed: Boolean;
begin
  Result := Abs(FTurnSpeed - 500) > EPS;
end;

{ TVXNavigatorAdjustDistanceParameters }

procedure TVXNavigatorAdjustDistanceParameters.AddImpulse(
  const Impulse: Single);
begin
  FOldDistanceRatio := FOldDistanceRatio + Impulse * FSpeed / FInertia * FImpulseSpeed;
end;

procedure TVXNavigatorAdjustDistanceParameters.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TVXNavigatorAdjustDistanceParameters then
  begin
    FImpulseSpeed := TVXNavigatorAdjustDistanceParameters(Source).FImpulseSpeed;
  end;
end;

constructor TVXNavigatorAdjustDistanceParameters.Create(
  AOwner: TPersistent);
begin
  inherited;
  FImpulseSpeed := 0.02;
end;


procedure TVXNavigatorAdjustDistanceParameters.ScaleParameters(
  const Value: Single);
begin
  inherited;
  FImpulseSpeed := FImpulseSpeed / Value;
end;

function TVXNavigatorAdjustDistanceParameters.StoreImpulseSpeed: Boolean;
begin
  Result := Abs(FImpulseSpeed - 0.02) > EPS;
end;

{ TVXNavigatorAbstractParameters }


procedure TVXNavigatorAbstractParameters.Assign(Source: TPersistent);
begin
  if Source is TVXNavigatorAbstractParameters then
  begin
    FInertia := TVXNavigatorAbstractParameters(Source).FInertia;
    FSpeed :=   TVXNavigatorAbstractParameters(Source).FSpeed;
    FCutoff :=  TVXNavigatorAbstractParameters(Source).FCutoff;
  end
  else
    inherited; //to the pit of doom ;)
end;

constructor TVXNavigatorAbstractParameters.Create(
  AOwner: TPersistent);
begin
  FOwner := AOwner;
  FInertia := 100;
  FSpeed := 0.005;
  FCutoff := EPS;
end;

function TVXNavigatorAbstractParameters.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

procedure TVXNavigatorAbstractParameters.ScaleParameters(
  const Value: Single);
begin
  Assert(Value > 0);

  if Value < 1 then
    FInertia := FInertia / PowerSingle(2, Value)
  else
    FInertia := FInertia * PowerSingle(2, 1 / Value);
end;

function TVXNavigatorAbstractParameters.StoreCutoff: Boolean;
begin
  Result := Abs(FCutoff - EPS) > EPS2;
end;

function TVXNavigatorAbstractParameters.StoreInertia: Boolean;
begin
  Result := Abs(FInertia - 100) > EPS;
end;

function TVXNavigatorAbstractParameters.StoreSpeed: Boolean;
begin
  Result := Abs(FSpeed - 0.005) > EPS2;
end;

{ TVXNavigatorAdjustDistanceParametersEx }

procedure TVXNavigatorAdjustDistanceParametersEx.Assign(
  Source: TPersistent);
begin
  if Source is TVXNavigatorAdjustDistanceParametersEx then
  begin
    FTargetDistance := TVXNavigatorAdjustDistanceParametersEx(Source).FTargetDistance;
    FSpeedLimit := TVXNavigatorAdjustDistanceParametersEx(Source).FSpeedLimit;
  end
  else
    inherited;
end;

constructor TVXNavigatorAdjustDistanceParametersEx.Create(
  AOwner: TPersistent);
begin
  inherited;
  FInertia := 0.5;
  FTargetDistance := 100;
  FSpeed := 100;
  FSpeedLimit := 20000;
end;

function TVXNavigatorAdjustDistanceParametersEx.StoreInertia: Boolean;
begin
  Result := Abs(FInertia - 0.5) > EPS2;
end;

function TVXNavigatorAdjustDistanceParametersEx.StoreSpeed: Boolean;
begin
  Result := Abs(FSpeed - 100) > EPS2;
end;

function TVXNavigatorAdjustDistanceParametersEx.StoreSpeedLimit: Boolean;
begin
  Result := Abs(FSpeedLimit - 20000) > EPS2;
end;

function TVXNavigatorAdjustDistanceParametersEx.StoreTargetDistance: Boolean;
begin
  Result := Abs(FTargetDistance - 100) > EPS2;
end;

{ TVXNavigatorSmoothChangeItem }

procedure TVXNavigatorSmoothChangeItem.Assign(Source: TPersistent);
begin
  inherited Assign(Source);

  if Source is TVXNavigatorSmoothChangeItem then
  begin
    FInertia :=    TVXNavigatorSmoothChangeItem(Source).FInertia;
    FSpeed :=      TVXNavigatorSmoothChangeItem(Source).FSpeed;
    FSpeedLimit := TVXNavigatorSmoothChangeItem(Source).FSpeedLimit;
    FCutoff :=     TVXNavigatorSmoothChangeItem(Source).FCutoff;
    FEnabled :=    TVXNavigatorSmoothChangeItem(Source).FEnabled;
  end;
end;

constructor TVXNavigatorSmoothChangeItem.Create(aOwner: TXCollection);
begin
  inherited;
  FInertia := 1;
  FSpeed := 5.5;
  FSpeedLimit := 20000;
  FCutoff := EPS;
  FEnabled := True;
end;

function TVXNavigatorSmoothChangeItem.GetNavigator: TVXSmoothNavigator;
begin
  Result := TVXSmoothNavigator(TVXNavigatorSmoothChangeItems(GetOwner).Owner);
end;

procedure TVXNavigatorSmoothChangeItem.ScaleParameters(
  const Value: Single);
begin
  Assert(Value > 0);

  if Value < 1 then
    FInertia := FInertia / PowerSingle(2, Value)
  else
    FInertia := FInertia * PowerSingle(2, 1 / Value);
end;

function TVXNavigatorSmoothChangeItem.StoreCutoff: Boolean;
begin
  Result := Abs(FCutoff - EPS) > EPS8;
end;

function TVXNavigatorSmoothChangeItem.StoreInertia: Boolean;
begin
  Result := Abs(FInertia - 1) > EPS;
end;

function TVXNavigatorSmoothChangeItem.StoreSpeed: Boolean;
begin
  Result := Abs(FSpeed - 5.5) > EPS2;
end;

function TVXNavigatorSmoothChangeItem.StoreSpeedLimit: Boolean;
begin
  Result := Abs(FSpeedLimit - 20000) > EPS2;
end;

{ TVXNavigatorSmoothChangeItems }

function TVXNavigatorSmoothChangeItems.Add(AClass : TVXNavigatorSmoothChangeItemClass): TVXNavigatorSmoothChangeItem;
begin
  Result := AClass.Create(Self);
end;

function TVXNavigatorSmoothChangeItems.CanAdd(AClass: TXCollectionItemClass): Boolean;
begin
  Result := AClass.InheritsFrom(TVXNavigatorSmoothChangeItem);
end;

procedure TVXNavigatorSmoothChangeItems.DoProceed(ADeltaTime: Double);
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    GetItems(I).Proceed(ADeltaTime);
end;

function TVXNavigatorSmoothChangeItems.GetItems(const Index : Integer): TVXNavigatorSmoothChangeItem;
begin
  Result := TVXNavigatorSmoothChangeItem(inherited GetItems(Index));
end;

class function TVXNavigatorSmoothChangeItems.ItemsClass: TXCollectionItemClass;
begin
  Result := TVXNavigatorSmoothChangeItem;
end;

procedure TVXNavigatorSmoothChangeItems.SetItems(const Index : Integer; const Value:
        TVXNavigatorSmoothChangeItem);
begin
  GetItems(Index).Assign(Value);
end;

{ TVXNavigatorSmoothChangeSingle }

procedure TVXNavigatorSmoothChangeSingle.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  
  if Source is TVXNavigatorSmoothChangeVector then
  begin
    FTargetValue := TVXNavigatorSmoothChangeSingle(Source).TargetValue;
    FOnGetCurrentValue := TVXNavigatorSmoothChangeSingle(Source).FOnGetCurrentValue;
    FOnSetCurrentValue := TVXNavigatorSmoothChangeSingle(Source).FOnSetCurrentValue;
  end;
end;

class function TVXNavigatorSmoothChangeSingle.FriendlyName: string;
begin
  Result := 'Navigator SmoothChange Single';
end;

function TVXNavigatorSmoothChangeSingle.Proceed(ADeltaTime: Double): Boolean;
var
  lCurrentValue: Single;
  lCurrentDifference: Single;
  lTotalDistanceToTravelThisTime, lDistanceToTravelThisTime: Single;
  lMaxExpectedDeltaTime: Double;

begin
  Result := False;
  if not FEnabled then Exit;
  if not Assigned(FOnGetCurrentValue) then Exit;
  if not Assigned(FOnSetCurrentValue) then Exit;

  lMaxExpectedDeltaTime := GetNavigator.FMaxExpectedDeltaTime;
  lCurrentValue := FOnGetCurrentValue(Self);
  lCurrentDifference := FTargetValue - lCurrentValue;

  lTotalDistanceToTravelThisTime := 0;

  while ADeltaTime > lMaxExpectedDeltaTime do
  begin
    lDistanceToTravelThisTime := MinFloat((lCurrentDifference * ADeltaTime * FSpeed * FInertia) / (FInertia + 1), FSpeedLimit);
//  lDistanceToTravelThisTime := (lCurrentDistance * ADeltaTime + FSpeed * FInertia) / (FInertia + 1);-  this also works, but a bit different.

    lCurrentDifference := lCurrentDifference - lDistanceToTravelThisTime;
    lTotalDistanceToTravelThisTime := lTotalDistanceToTravelThisTime + lDistanceToTravelThisTime;
    ADeltaTime := ADeltaTime - lMaxExpectedDeltaTime;
  end;

  if Abs(lTotalDistanceToTravelThisTime) > FCutoff then
  begin
    FOnSetCurrentValue(Self, lCurrentValue + lTotalDistanceToTravelThisTime);
    Result := True;
  end;  
end;

procedure TVXNavigatorSmoothChangeSingle.ResetTargetValue;
begin
  FTargetValue := FOnGetCurrentValue(Self);
end;

{ TVXNavigatorSmoothChangeVector }

procedure TVXNavigatorSmoothChangeVector.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  
  if Source is TVXNavigatorSmoothChangeVector then
  begin
    FTargetValue.Assign(TVXNavigatorSmoothChangeVector(Source).TargetValue);
    FOnGetCurrentValue := TVXNavigatorSmoothChangeVector(Source).FOnGetCurrentValue;
    FOnSetCurrentValue := TVXNavigatorSmoothChangeVector(Source).FOnSetCurrentValue;
  end;
end;

constructor TVXNavigatorSmoothChangeVector.Create(aOwner: TXCollection);
begin
  inherited;
  FTargetValue := TVXCoordinates.CreateInitialized(Self, NullHmgVector, csVector);
end;

destructor TVXNavigatorSmoothChangeVector.Destroy;
begin
  FTargetValue.Free;
  inherited;
end;

class function TVXNavigatorSmoothChangeVector.FriendlyName: string;
begin
  Result := 'Navigator SmoothChange Vector';
end;

function TVXNavigatorSmoothChangeVector.Proceed(ADeltaTime: Double): Boolean;
var
  lAbsolutePosition: TVector;
  lCurrentDistance: Single;
  lTotalDistanceToTravelThisTime, lDistanceToTravelThisTime: Single;
  lMaxExpectedDeltaTime: Double;

  procedure DoAdjustDistanceToPoint();
  var
    vect: TVector;
  begin
    vect := VectorScale(VectorNormalize(VectorSubtract(FTargetValue.DirectVector, lAbsolutePosition)), lTotalDistanceToTravelThisTime);
    AddVector(vect, lAbsolutePosition);

    // Did we go too far?
    if VectorDistance(vect, FTargetValue.DirectVector) > VectorDistance(lAbsolutePosition, FTargetValue.DirectVector) then
      vect := FTargetValue.DirectVector;

    FOnSetCurrentValue(Self, vect);
    Result := True;
  end;

begin
  Result := False;
  if not FEnabled then Exit;
  if not Assigned(FOnGetCurrentValue) then Exit;
  if not Assigned(FOnSetCurrentValue) then Exit;

  lMaxExpectedDeltaTime := GetNavigator.FMaxExpectedDeltaTime;
  lAbsolutePosition := FOnGetCurrentValue(Self);
  lCurrentDistance := VectorDistance(lAbsolutePosition, FTargetValue.DirectVector);

  lTotalDistanceToTravelThisTime := 0;


  while ADeltaTime > lMaxExpectedDeltaTime do
  begin
    lDistanceToTravelThisTime := MinFloat((lCurrentDistance * ADeltaTime * FSpeed * FInertia) / (FInertia + 1), FSpeedLimit);
//  lDistanceToTravelThisTime := (lCurrentDistance * ADeltaTime + FSpeed * FInertia) / (FInertia + 1);-  this also works, but a bit different.

    lCurrentDistance := lCurrentDistance - lDistanceToTravelThisTime;
    lTotalDistanceToTravelThisTime := lTotalDistanceToTravelThisTime + lDistanceToTravelThisTime;
    ADeltaTime := ADeltaTime - lMaxExpectedDeltaTime;
  end;

  if Abs(lTotalDistanceToTravelThisTime) > FCutoff then
    DoAdjustDistanceToPoint();
end;

procedure TVXNavigatorSmoothChangeVector.ResetTargetValue;
begin
  FTargetValue.DirectVector := FOnGetCurrentValue(Self);
end;

procedure TVXNavigatorSmoothChangeVector.SetTargetValue(
  const Value: TVXCoordinates);
begin
  FTargetValue.Assign(Value);
end;

initialization
  RegisterClasses([
      TVXSmoothNavigator, TVXSmoothUserInterface,
      TVXNavigatorInertiaParameters, TVXNavigatorGeneralParameters,
      TVXNavigatorMoveAroundParameters,
      TVXNavigatorAdjustDistanceParameters, TVXNavigatorAdjustDistanceParametersEx
                   ]);

  RegisterXCollectionItemClass(TVXNavigatorSmoothChangeSingle);
  RegisterXCollectionItemClass(TVXNavigatorSmoothChangeVector);
end.


