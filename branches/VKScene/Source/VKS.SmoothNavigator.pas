//
// This unit is part of the GLScene Project   
//
{: VKS.SmoothNavigator<p>

     An extention of TVKNavigator, which allows to move objects with inertia
   Note: it is not completely FPS-independant. Only Moving code is, but
   MoveAroundTarget, Turn[Vertical/Horizontal] and AdjustDistanceTo[..] is not.

     Don't know why, but when I make their code identical, these function stop
   working completely. So you probably have to call the AutoScaleParameters
   procedure once in a while for it to adjust to the current framerate.
   If someone knows a better way to solve this issue, please contact me via
   glscene newsgroups.<p>


   <b>History : </b><font size=-1><ul>
      <li>30/06/11 - DaStr - Converted many procedures to functions
                             Bugfixed Assign() in some places
                             Added "Cutoff" property instead of fixed EPS values
      <li>02/06/11 - DaStr - DeltaTime is now Double, like in Cadencer
                             Added CustomAnimatedItems
      <li>28/05/11 - DaStr - Added the AdjustDistanceTo[..]Ex procedures
      <li>25/02/07 - DaStr - Added the AdjustDistanceTo[..] procedures
      <li>23/02/07 - DaStr - Initial version (contributed to GLScene)


    TODO:
      1) Scale "Old values" too, when callin the Scale parameter procedure to
         avoid the temporary "freeze" of controls.
      2) AddImpulse procedures.



    Previous version history:
        v1.0    10 December  '2005  Creation
        v1.0.2  11 December  '2005  TurnMaxAngle added
        v1.1    04 March     '2006  Inertia became FPS-independant
                                    TVKSmoothNavigatorParameters added
        v1.1.6  18 February  '2007  Merged with GLInertedUserInterface.pas
                                    All parameters moved into separate classes
                                    Added MoveAroudTargetWithInertia
        v1.2    23 February  '2007  Finally made it trully FPS-independant
                                    Added default values to every property
                                    Contributed to GLScene
}

unit VKS.SmoothNavigator;

interface

{$I VKScene.inc}

uses
  System.Classes,
   
  VKS.Navigator, VKS.VectorGeometry, VKS.Scene, VKS.CrossPlatform, VKS.Coordinates,
  VKS.Screen, VKS.XCollection;

type

  {: TVKNavigatorAdjustDistanceParameters includes a basic set of parameters
     that control the smoothness of movement.<p>
  }
  TVKNavigatorAbstractParameters = class(TPersistent)
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

  TVKSmoothNavigator = class;

  {: TVKNavigatorSmoothChangeItem includes a basic set of parameters
     that control the smoothness of movement.<p>
  }
  TVKNavigatorSmoothChangeItem = class(TXCollectionItem)
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
    function GetNavigator: TVKSmoothNavigator;
  public
    {: Returns False if there was no change. }
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

  TVKNavigatorSmoothChangeSingle = class;
  TVKNavigatorSmoothChangeSingleGetEvent = function(const ASender: TVKNavigatorSmoothChangeSingle): Single of object;
  TVKNavigatorSmoothChangeSingleSetEvent = procedure(const ASender: TVKNavigatorSmoothChangeSingle; const AValue: Single) of object;

  {: Smoothly change any Single value, so it will become TargetValue in the end.<p> }
  TVKNavigatorSmoothChangeSingle = class(TVKNavigatorSmoothChangeItem)
  private
    FTargetValue: Single;
    FOnGetCurrentValue: TVKNavigatorSmoothChangeSingleGetEvent;
    FOnSetCurrentValue: TVKNavigatorSmoothChangeSingleSetEvent;
  public
    class function FriendlyName: string; override;
    function Proceed(ADeltaTime: Double): Boolean; override;
    procedure Assign(Source: TPersistent); override;
    procedure ResetTargetValue(); override;    
  published
    property TargetValue: Single read FTargetValue write FTargetValue;
    property OnGetCurrentValue: TVKNavigatorSmoothChangeSingleGetEvent read FOnGetCurrentValue write FOnGetCurrentValue;
    property OnSetCurrentValue: TVKNavigatorSmoothChangeSingleSetEvent read FOnSetCurrentValue write FOnSetCurrentValue;
  end;

  TVKNavigatorSmoothChangeVector = class;
  TVKNavigatorSmoothChangeVectorGetEvent = function(const ASender: TVKNavigatorSmoothChangeVector): TVector of object;
  TVKNavigatorSmoothChangeVectorSetEvent = procedure(const ASender: TVKNavigatorSmoothChangeVector; const AValue: TVector) of object;

  {: Smoothly change any Vector4f value, so it will become TargetValue in the end.<p> }
  TVKNavigatorSmoothChangeVector = class(TVKNavigatorSmoothChangeItem)
  private
    FTargetValue: TVKCoordinates;
    FOnGetCurrentValue: TVKNavigatorSmoothChangeVectorGetEvent;
    FOnSetCurrentValue: TVKNavigatorSmoothChangeVectorSetEvent;
    procedure SetTargetValue(const Value: TVKCoordinates);
  public
    class function FriendlyName: string; override;
    function Proceed(ADeltaTime: Double): Boolean; override;
    procedure Assign(Source: TPersistent); override;
    constructor Create(aOwner: TXCollection); override;
    destructor Destroy; override;
    procedure ResetTargetValue(); override;
  published
    property TargetValue: TVKCoordinates read FTargetValue write SetTargetValue;
    property OnGetCurrentValue: TVKNavigatorSmoothChangeVectorGetEvent read FOnGetCurrentValue write FOnGetCurrentValue;
    property OnSetCurrentValue: TVKNavigatorSmoothChangeVectorSetEvent read FOnSetCurrentValue write FOnSetCurrentValue;
  end;

  TVKNavigatorSmoothChangeItemClass = class of TVKNavigatorSmoothChangeItem;

  {: XCollection of TVKNavigatorSmoothChangeItem. }
  TVKNavigatorSmoothChangeItems = class(TXCollection)
  private
    function GetItems(const Index : Integer): TVKNavigatorSmoothChangeItem;
    procedure SetItems(const Index : Integer; const Value: TVKNavigatorSmoothChangeItem);
  protected
    procedure DoProceed(ADeltaTime: Double);
  public
    function Add(AClass : TVKNavigatorSmoothChangeItemClass): TVKNavigatorSmoothChangeItem;
    function CanAdd(AClass: TXCollectionItemClass): Boolean; override;
    class function ItemsClass: TXCollectionItemClass; override;
    property Items[const Index : Integer]: TVKNavigatorSmoothChangeItem read GetItems write
            SetItems; default;
  end;

  {: TVKNavigatorAdjustDistanceParameters is wrapper for all parameters that
       affect how the AdjustDisanceTo[...] methods work<p>
  }
  TVKNavigatorAdjustDistanceParameters = class(TVKNavigatorAbstractParameters)
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

  {: TVKNavigatorAdjustDistanceParameters is wrapper for all parameters that
       affect how the AdjustDisanceTo[...]Ex methods work<p>

     You need to set the TargetObject and desired distance to it,
     then call AdjustDisanceTo[...]Ex() in your Cadencer.OnProgress code.
  }
  TVKNavigatorAdjustDistanceParametersEx = class(TVKNavigatorAbstractParameters)
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

  {: TVKNavigatorInertiaParameters is wrapper for all parameters that affect the
       smoothness of movement<p>
  }
  TVKNavigatorInertiaParameters = class(TPersistent)
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


  {: TVKNavigatorGeneralParameters is a wrapper for all general inertia parameters.

     These properties mean that if ExpectedMaxFPS is 100, FAutoScaleMin is 0.1,
     FAutoScaleMax is 0.75 then the "safe range" for it to change is [10..75].
     If these bounds are violated, then ExpectedMaxFPS is automaticly increased
     or decreased by AutoScaleMult.
  }
  TVKNavigatorGeneralParameters = class(TPersistent)
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


  {: TVKNavigatorMoveAroundParameters is a wrapper for all parameters that
      effect how the TVKBaseSceneObject.MoveObjectAround() procedure works
  }
  TVKNavigatorMoveAroundParameters = class(TPersistent)
  private
    FOwner: TPersistent;
    FTargetObject: TVKBaseSceneObject;

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
    procedure SetTargetObject(const Value: TVKBaseSceneObject);
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
    property TargetObject: TVKBaseSceneObject read FTargetObject write SetTargetObject;
    property Cutoff: Double read FCutoff write FCutoff stored StoreCutoff;    
  end;


  // TVKSmoothNavigator
  //
  {: TVKSmoothNavigator is the component for moving a TVKBaseSceneObject, and all
       classes based on it, this includes all the objects from the Scene Editor.<p>

     It uses complex smoothing algorithms, most of which are FPS-dependant.
     Make sure your limit your FPS and set MaxExpectedDeltaTime to a value
     that is aproximatly 5 times less than your usual deltatime.
  }
  TVKSmoothNavigator = class(TVKNavigator)
  private
    FMaxExpectedDeltaTime: Double;
    FInertiaParams: TVKNavigatorInertiaParameters;
    FGeneralParams: TVKNavigatorGeneralParameters;
    FMoveAroundParams: TVKNavigatorMoveAroundParameters;
    FAdjustDistanceParams: TVKNavigatorAdjustDistanceParameters;
    FAdjustDistanceParamsEx: TVKNavigatorAdjustDistanceParametersEx;
    FCustomAnimatedItems: TVKNavigatorSmoothChangeItems;
    procedure SetInertiaParams(const Value: TVKNavigatorInertiaParameters);
    function StoreMaxExpectedDeltaTime: Boolean;
    procedure SetGeneralParams(const Value: TVKNavigatorGeneralParameters);
    procedure SetMoveAroundParams(const Value: TVKNavigatorMoveAroundParameters);
    procedure SetAdjustDistanceParams(const Value: TVKNavigatorAdjustDistanceParameters);
    procedure SetAdjustDistanceParamsEx(
      const Value: TVKNavigatorAdjustDistanceParametersEx);
    procedure SetCustomAnimatedItems(
      const Value: TVKNavigatorSmoothChangeItems);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    //: Constructors-destructors.
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    //: From TVKNavigator. Probably, should not be public.
    procedure SetObject(Value: TVKBaseSceneObject); override;

    //: Uses InertiaParams.
    procedure TurnHorizontal(Angle: Single; ADeltaTime: Double); virtual;
    procedure TurnVertical(Angle: Single; ADeltaTime: Double); virtual;
    procedure FlyForward(const Plus, Minus: Boolean; ADeltaTime: Double; const Accelerate: Boolean = False); virtual;
    procedure MoveForward(const Plus, Minus: Boolean; ADeltaTime: Double; const Accelerate: Boolean = False); virtual;
    procedure StrafeHorizontal(const Plus, Minus: Boolean; ADeltaTime: Double; const Accelerate: Boolean = False); virtual;
    procedure StrafeVertical(const Plus, Minus: Boolean; ADeltaTime: Double; const Accelerate: Boolean = False); virtual;

    //: Uses MoveAroundParams. Returns True, if object was actually moved.
    function MoveAroundTarget(const PitchDelta, TurnDelta : Single; const ADeltaTime: Double): Boolean; virtual;
    function MoveObjectAround(const AObject: TVKBaseSceneObject; PitchDelta, TurnDelta : Single; ADeltaTime: Double): Boolean; virtual;

    //: Uses AdjustDistanceParams.
    function AdjustDistanceToPoint(const  APoint: TVector; const DistanceRatio : Single; ADeltaTime: Double): Boolean; virtual;
    function AdjustDistanceToTarget(const DistanceRatio : Single; const ADeltaTime: Double): Boolean; virtual;

    //: Uses AdjustDistanceParamsEx.
    function AdjustDistanceToPointEx(const  APoint: TVector; ADeltaTime: Double): Boolean; virtual;
    function AdjustDistanceToTargetEx(const ADeltaTime: Double): Boolean; virtual;

    //: Uses CustomAnimatedItems.
    procedure AnimateCustomItems(const ADeltaTime: Double); virtual;

    //: Uses GeneralParams.
      {: In ScaleParameters, Value should be around 1. }
    procedure ScaleParameters(const Value: Single); virtual;
    procedure AutoScaleParameters(const FPS: Single); virtual;
    procedure AutoScaleParametersUp(const FPS: Single); virtual;
  published
    property MaxExpectedDeltaTime: Double read FMaxExpectedDeltaTime write FMaxExpectedDeltaTime stored StoreMaxExpectedDeltaTime;
    property InertiaParams: TVKNavigatorInertiaParameters read FInertiaParams write SetInertiaParams;
    property GeneralParams: TVKNavigatorGeneralParameters read FGeneralParams write SetGeneralParams;
    property MoveAroundParams: TVKNavigatorMoveAroundParameters read FMoveAroundParams write SetMoveAroundParams;
    property AdjustDistanceParams: TVKNavigatorAdjustDistanceParameters read FAdjustDistanceParams write SetAdjustDistanceParams;
    property AdjustDistanceParamsEx: TVKNavigatorAdjustDistanceParametersEx read FAdjustDistanceParamsEx write SetAdjustDistanceParamsEx;
    property CustomAnimatedItems: TVKNavigatorSmoothChangeItems read FCustomAnimatedItems write SetCustomAnimatedItems;
  end;


  // TVKSmoothUserInterface
  //
  {: TVKSmoothUserInterface is the component which reads the userinput and transform it into action.<p>
      <ul>
	   <li>Mouselook(ADeltaTime: double) : handles mouse look... Should be called
                           in the Cadencer event. (Though it works everywhere!)
      </ul>
	   The four properties to get you started are:
      <ul>
	   <li>InvertMouse     : Inverts the mouse Y axis.
	   <li>AutoUpdateMouse : If enabled (by defaul), than handles all mouse updates.
	   <li>GLNavigator     : The Navigator which receives the user movement.
	   <li>GLVertNavigator : The Navigator which if set receives the vertical user
                           movement. Used mostly for cameras....
      </ul>
   }
  TVKSmoothUserInterface = class(TComponent)
  private
    FAutoUpdateMouse: Boolean;
    FMouseLookActive: Boolean;
    FSmoothNavigator: TVKSmoothNavigator;
    FSmoothVertNavigator: TVKSmoothNavigator;
    FInvertMouse: Boolean;
    FOriginalMousePos: TVKCoordinates2;
    procedure SetSmoothNavigator(const Value: TVKSmoothNavigator); virtual;
    procedure SetOriginalMousePos(const Value: TVKCoordinates2); virtual;
    procedure SetSmoothVertNavigator(const Value: TVKSmoothNavigator); virtual;
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
    function MouseLook(const NewXY: TVKPoint; const ADeltaTime: Double): Boolean; overload;
    function MouseLook(const NewX, NewY: Integer; const ADeltaTime: Double): Boolean; overload;
  published
    property AutoUpdateMouse: Boolean read FAutoUpdateMouse write FAutoUpdateMouse default True;
    property MouseLookActive: Boolean read FMouseLookActive write SetMouseLookActive default False;
    property SmoothVertNavigator: TVKSmoothNavigator read FSmoothVertNavigator write SetSmoothVertNavigator;
    property SmoothNavigator: TVKSmoothNavigator read FSmoothNavigator write SetSmoothNavigator;
    property InvertMouse: Boolean read FInvertMouse write FInvertMouse default False;
    property OriginalMousePos: TVKCoordinates2 read FOriginalMousePos write SetOriginalMousePos;
  end;

implementation

const
  EPS =  0.001;
  EPS2 = 0.0001;
  EPS8 = 0.00000001;

{ TVKSmoothNavigator }

constructor TVKSmoothNavigator.Create(AOwner: TComponent);
begin
  inherited;
  FMaxExpectedDeltaTime := 0.001;
  FInertiaParams := TVKNavigatorInertiaParameters.Create(Self);
  FGeneralParams := TVKNavigatorGeneralParameters.Create(Self);
  FMoveAroundParams := TVKNavigatorMoveAroundParameters.Create(Self);
  FAdjustDistanceParams := TVKNavigatorAdjustDistanceParameters.Create(Self);
  FAdjustDistanceParamsEx := TVKNavigatorAdjustDistanceParametersEx.Create(Self);
  FCustomAnimatedItems := TVKNavigatorSmoothChangeItems.Create(Self);
end;

destructor TVKSmoothNavigator.Destroy;
begin
  FInertiaParams.Free;
  FGeneralParams.Free;
  FMoveAroundParams.Free;
  FAdjustDistanceParams.Free;
  FAdjustDistanceParamsEx.Free;
  FCustomAnimatedItems.Free;
  inherited;
end;

procedure TVKSmoothNavigator.SetInertiaParams(
  const Value: TVKNavigatorInertiaParameters);
begin
  FInertiaParams.Assign(Value);
end;

procedure TVKSmoothNavigator.TurnHorizontal(Angle: Single; ADeltaTime: Double);
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

procedure TVKSmoothNavigator.TurnVertical(Angle: Single; ADeltaTime: Double);
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


procedure TVKSmoothNavigator.MoveForward(const Plus, Minus: Boolean; ADeltaTime: Double; const Accelerate: Boolean = False);
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

procedure TVKSmoothNavigator.FlyForward(const Plus, Minus: Boolean; ADeltaTime: Double; const Accelerate: Boolean = False);
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

procedure TVKSmoothNavigator.StrafeHorizontal(const Plus, Minus: Boolean; ADeltaTime: Double; const Accelerate: Boolean = False);
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

procedure TVKSmoothNavigator.StrafeVertical(const Plus, Minus: Boolean; ADeltaTime: Double; const Accelerate: Boolean = False);
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

procedure TVKSmoothNavigator.AutoScaleParameters(const FPS: Single);
begin
  with FGeneralParams do
  begin
    if FPS > FAutoScaleMax / FMaxExpectedDeltatime then
      ScaleParameters(FAutoScaleMult)
    else if FPS < FAutoScaleMin / FMaxExpectedDeltatime then
      ScaleParameters(1/FAutoScaleMult);
  end;
end;


procedure TVKSmoothNavigator.AutoScaleParametersUp(const FPS: Single);
begin
  with FGeneralParams do
  begin
    if FPS > FAutoScaleMax / FMaxExpectedDeltatime then
      ScaleParameters(FAutoScaleMult)
  end;
end;

procedure TVKSmoothNavigator.ScaleParameters(const Value: Single);
begin
  Assert(Value > 0);
  FMaxExpectedDeltatime := FMaxExpectedDeltatime / Value;
  FInertiaParams.ScaleParameters(Value);
  FMoveAroundParams.ScaleParameters(Value);
  FAdjustDistanceParams.ScaleParameters(Value);
end;

function TVKSmoothNavigator.StoreMaxExpectedDeltaTime: Boolean;
begin
  Result := Abs(FMaxExpectedDeltaTime - 0.001) > EPS2;
end;

procedure TVKSmoothNavigator.SetGeneralParams(
  const Value: TVKNavigatorGeneralParameters);
begin
  FGeneralParams.Assign(Value);
end;

procedure TVKSmoothNavigator.SetMoveAroundParams(
  const Value: TVKNavigatorMoveAroundParameters);
begin
  FMoveAroundParams.Assign(Value);
end;

procedure TVKSmoothNavigator.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if Operation = opRemove then
  begin
    if AComponent = FMoveAroundParams.FTargetObject then
      FMoveAroundParams.FTargetObject := nil;
  end;
end;

procedure TVKSmoothNavigator.SetObject(Value: TVKBaseSceneObject);
var
  I: Integer;
begin
  inherited;
  // Try to detect a TargetObject.
  if Value <> nil then
    if FMoveAroundParams.TargetObject = nil then
    begin
      // May be it is a camera...
      if Value is TVKCamera then
        FMoveAroundParams.TargetObject := TVKCamera(Value).TargetObject
      else
      begin
        // May be it has camera children...
        if Value.Count <> 0 then
          for I := 0 to Value.Count - 1 do
            if Value.Children[I] is TVKCamera then
            begin
              FMoveAroundParams.TargetObject := TVKCamera(Value.Children[I]).TargetObject;
              Exit;
            end;
      end;
    end;
end;

function TVKSmoothNavigator.MoveAroundTarget(const PitchDelta, TurnDelta: Single;
  const ADeltaTime: Double): Boolean;
begin
  Result := MoveObjectAround(FMoveAroundParams.FTargetObject, PitchDelta, TurnDelta, ADeltaTime);
end;

function TVKSmoothNavigator.MoveObjectAround(
  const AObject: TVKBaseSceneObject; PitchDelta, TurnDelta: Single;
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
      MovingObject.AbsolutePosition := VKS.VectorGeometry.MoveObjectAround(
        MovingObject.AbsolutePosition, lUp, AObject.AbsolutePosition, FinalPitch, FinalTurn);
      Result := True;
    end;
  end;
end;


function TVKSmoothNavigator.AdjustDistanceToPoint(const APoint: TVector;
  const DistanceRatio: Single; ADeltaTime: Double): Boolean;

  // Based on TVKCamera.AdjustDistanceToTarget
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

function TVKSmoothNavigator.AdjustDistanceToTarget(const DistanceRatio: Single;
  const ADeltaTime: Double): Boolean;
begin
  Assert(FMoveAroundParams.FTargetObject <> nil);
  Result := AdjustDistanceToPoint(FMoveAroundParams.FTargetObject.AbsolutePosition,
                        DistanceRatio, ADeltaTime);
end;

procedure TVKSmoothNavigator.SetAdjustDistanceParams(
  const Value: TVKNavigatorAdjustDistanceParameters);
begin
  FAdjustDistanceParams.Assign(Value);
end;

function TVKSmoothNavigator.AdjustDistanceToPointEx(const APoint: TVector;
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

function TVKSmoothNavigator.AdjustDistanceToTargetEx(
  const ADeltaTime: Double): Boolean;
begin
  Assert(FMoveAroundParams.FTargetObject <> nil);
  Result := AdjustDistanceToPointEx(FMoveAroundParams.FTargetObject.AbsolutePosition,
                          ADeltaTime);
end;

procedure TVKSmoothNavigator.SetAdjustDistanceParamsEx(
  const Value: TVKNavigatorAdjustDistanceParametersEx);
begin
  FAdjustDistanceParamsEx.Assign(Value);
end;

procedure TVKSmoothNavigator.AnimateCustomItems(const ADeltaTime: Double);
begin
  FCustomAnimatedItems.DoProceed(ADeltaTime);
end;

procedure TVKSmoothNavigator.SetCustomAnimatedItems(
  const Value: TVKNavigatorSmoothChangeItems);
begin
  FCustomAnimatedItems.Assign(Value);
end;

{ TVKSmoothUserInterface }

function TVKSmoothUserInterface.MouseLook(
  const ADeltaTime: Double): Boolean;
var
  MousePos: TVKPoint;
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

function TVKSmoothUserInterface.Mouselook(const NewX, NewY: Integer; const ADeltaTime: Double): Boolean;
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


function TVKSmoothUserInterface.MouseLook(const NewXY: TVKPoint; const ADeltaTime: Double): Boolean;
begin
  Result := Mouselook(NewXY.X, NewXY.Y, ADeltaTime);
end;

constructor TVKSmoothUserInterface.Create(AOwner: TComponent);
begin
  inherited;
  FMouseLookActive := False;
  FAutoUpdateMouse := True;
  FOriginalMousePos := TVKCoordinates2.CreateInitialized(Self,
                             VectorMake(GLGetScreenWidth div 2,
                             GLGetScreenHeight div 2, 0, 0), csPoint2D);
end;

procedure TVKSmoothUserInterface.Notification(AComponent: TComponent;
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

procedure TVKSmoothUserInterface.SetSmoothNavigator(
  const Value: TVKSmoothNavigator);
begin
  if FSmoothNavigator <> nil then
    FSmoothNavigator.RemoveFreeNotification(Self);

  FSmoothNavigator := Value;

  if FSmoothNavigator <> nil then
    FSmoothNavigator.FreeNotification(Self);
end;

destructor TVKSmoothUserInterface.Destroy;
begin
  FOriginalMousePos.Destroy;
  inherited;
end;

procedure TVKSmoothUserInterface.SetOriginalMousePos(
  const Value: TVKCoordinates2);
begin
  FOriginalMousePos.Assign(Value);
end;

procedure TVKSmoothUserInterface.SetSmoothVertNavigator(
  const Value: TVKSmoothNavigator);
begin
  if FSmoothVertNavigator <> nil then
    FSmoothVertNavigator.RemoveFreeNotification(Self);

  FSmoothVertNavigator := Value;

  if FSmoothVertNavigator <> nil then
    FSmoothVertNavigator.FreeNotification(Self);
end;

procedure TVKSmoothUserInterface.MouseLookActiveToggle;
begin
  if FMouseLookActive then
    SetMouseLookActive(False)
  else
    SetMouseLookActive(True)
end;

procedure TVKSmoothUserInterface.SetMouseLookActive(const Value: Boolean);
var
  MousePos: TVKPoint;
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

procedure TVKSmoothUserInterface.TurnHorizontal(const Angle: Single;
  const ADeltaTime: Double);
begin
  FSmoothNavigator.TurnHorizontal(Angle, ADeltaTime);
end;

procedure TVKSmoothUserInterface.TurnVertical(const Angle: Single;
  const ADeltaTime: Double);
begin
  if Assigned(FSmoothNavigator) then
    FSmoothNavigator.TurnVertical(Angle, ADeltaTime)
  else
    FSmoothVertNavigator.TurnVertical(Angle, ADeltaTime);
end;

{ TVKNavigatorInertiaParameters }

procedure TVKNavigatorInertiaParameters.Assign(Source: TPersistent);
begin
  if Source is TVKNavigatorInertiaParameters then
  begin
    FMovementAcceleration := TVKNavigatorInertiaParameters(Source).FMovementAcceleration;
    FMovementInertia := TVKNavigatorInertiaParameters(Source).FMovementInertia;
    FMovementSpeed := TVKNavigatorInertiaParameters(Source).FMovementSpeed;
    FTurnMaxAngle := TVKNavigatorInertiaParameters(Source).FTurnMaxAngle;
    FTurnInertia := TVKNavigatorInertiaParameters(Source).FTurnInertia;
    FTurnSpeed := TVKNavigatorInertiaParameters(Source).FTurnSpeed;
  end
  else
    inherited; //to the pit of doom ;)
end;

constructor TVKNavigatorInertiaParameters.Create(AOwner: TPersistent);
begin
  FOwner := AOwner;

  FTurnInertia := 150;
  FTurnSpeed := 50;
  FTurnMaxAngle := 0.5;

  FMovementAcceleration := 7;
  FMovementInertia := 200;
  FMovementSpeed := 200;
end;

function TVKNavigatorInertiaParameters.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

procedure TVKNavigatorInertiaParameters.ScaleParameters(
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

function TVKNavigatorInertiaParameters.StoreTurnMaxAngle: Boolean;
begin
  Result := Abs(FTurnMaxAngle - 0.5) > EPS;
end;

function TVKNavigatorInertiaParameters.StoreMovementAcceleration: Boolean;
begin
  Result := Abs(FMovementAcceleration - 7) > EPS;
end;

function TVKNavigatorInertiaParameters.StoreMovementInertia: Boolean;
begin
  Result := Abs(FMovementInertia - 200) > EPS;
end;

function TVKNavigatorInertiaParameters.StoreMovementSpeed: Boolean;
begin
  Result := Abs(FMovementSpeed - 200) > EPS;
end;

function TVKNavigatorInertiaParameters.StoreTurnInertia: Boolean;
begin
  Result := Abs(FTurnInertia - 150) > EPS;
end;

function TVKNavigatorInertiaParameters.StoreTurnSpeed: Boolean;
begin
  Result := Abs(FTurnSpeed - 50) > EPS;
end;

{ TVKNavigatorGeneralParameters }

procedure TVKNavigatorGeneralParameters.Assign(Source: TPersistent);
begin
  if Source is TVKNavigatorGeneralParameters then
  begin
    FAutoScaleMin := TVKNavigatorGeneralParameters(Source).FAutoScaleMin;
    FAutoScaleMax := TVKNavigatorGeneralParameters(Source).FAutoScaleMax;
    FAutoScaleMult := TVKNavigatorGeneralParameters(Source).FAutoScaleMult;
  end
  else
    inherited; //die!
end;

constructor TVKNavigatorGeneralParameters.Create(AOwner: TPersistent);
begin
  FOwner := AOwner;
  FAutoScaleMin := 0.1;
  FAutoScaleMax := 0.75;
  FAutoScaleMult := 2;
end;

function TVKNavigatorGeneralParameters.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

function TVKNavigatorGeneralParameters.StoreAutoScaleMax: Boolean;
begin
  Result := Abs(FAutoScaleMax - 0.75) > EPS;
end;

function TVKNavigatorGeneralParameters.StoreAutoScaleMin: Boolean;
begin
  Result := Abs(FAutoScaleMin - 0.1) > EPS;
end;

function TVKNavigatorGeneralParameters.StoreAutoScaleMult: Boolean;
begin
  Result := Abs(FAutoScaleMult - 2) > EPS;
end;

{ TVKNavigatorMoveAroundParameters }

procedure TVKNavigatorMoveAroundParameters.Assign(Source: TPersistent);
begin
  if Source is TVKNavigatorMoveAroundParameters then
  begin
    FMaxAngle := TVKNavigatorMoveAroundParameters(Source).FMaxAngle;
    FInertia :=  TVKNavigatorMoveAroundParameters(Source).FInertia;
    FPitchSpeed :=  TVKNavigatorMoveAroundParameters(Source).FPitchSpeed;
    FTurnSpeed :=  TVKNavigatorMoveAroundParameters(Source).FTurnSpeed;
    FCutoff :=  TVKNavigatorMoveAroundParameters(Source).FCutoff;
    SetTargetObject(TVKNavigatorMoveAroundParameters(Source).FTargetObject);
  end
  else
    inherited; //die
end;

constructor TVKNavigatorMoveAroundParameters.Create(AOwner: TPersistent);
begin
  FOwner := AOwner;
  FPitchSpeed := 500;
  FTurnSpeed  := 500;
  FInertia    := 65;
  FMaxAngle   := 1.5;
  FCutoff     := EPS2;
end;

function TVKNavigatorMoveAroundParameters.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

procedure TVKNavigatorMoveAroundParameters.ScaleParameters(
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

procedure TVKNavigatorMoveAroundParameters.SetTargetObject(
  const Value: TVKBaseSceneObject);
begin
  if FTargetObject <> nil then
    if FOwner is TVKSmoothNavigator then
      FTargetObject.RemoveFreeNotification(TVKSmoothNavigator(FOwner));

  FTargetObject := Value;

  if FTargetObject <> nil then
    if FOwner is TVKSmoothNavigator then
      FTargetObject.FreeNotification(TVKSmoothNavigator(FOwner));
end;

function TVKNavigatorMoveAroundParameters.StoreCutoff: Boolean;
begin
  Result := Abs(FCutoff - EPS2) > EPS8;
end;

function TVKNavigatorMoveAroundParameters.StoreInertia: Boolean;
begin
  Result := Abs(FInertia - 65) > EPS;
end;

function TVKNavigatorMoveAroundParameters.StoreMaxAngle: Boolean;
begin
  Result := Abs(FMaxAngle - 1.5) > EPS;
end;

function TVKNavigatorMoveAroundParameters.StorePitchSpeed: Boolean;
begin
  Result := Abs(FPitchSpeed - 500) > EPS;
end;

function TVKNavigatorMoveAroundParameters.StoreTurnSpeed: Boolean;
begin
  Result := Abs(FTurnSpeed - 500) > EPS;
end;

{ TVKNavigatorAdjustDistanceParameters }

procedure TVKNavigatorAdjustDistanceParameters.AddImpulse(
  const Impulse: Single);
begin
  FOldDistanceRatio := FOldDistanceRatio + Impulse * FSpeed / FInertia * FImpulseSpeed;
end;

procedure TVKNavigatorAdjustDistanceParameters.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TVKNavigatorAdjustDistanceParameters then
  begin
    FImpulseSpeed := TVKNavigatorAdjustDistanceParameters(Source).FImpulseSpeed;
  end;
end;

constructor TVKNavigatorAdjustDistanceParameters.Create(
  AOwner: TPersistent);
begin
  inherited;
  FImpulseSpeed := 0.02;
end;


procedure TVKNavigatorAdjustDistanceParameters.ScaleParameters(
  const Value: Single);
begin
  inherited;
  FImpulseSpeed := FImpulseSpeed / Value;
end;

function TVKNavigatorAdjustDistanceParameters.StoreImpulseSpeed: Boolean;
begin
  Result := Abs(FImpulseSpeed - 0.02) > EPS;
end;

{ TVKNavigatorAbstractParameters }


procedure TVKNavigatorAbstractParameters.Assign(Source: TPersistent);
begin
  if Source is TVKNavigatorAbstractParameters then
  begin
    FInertia := TVKNavigatorAbstractParameters(Source).FInertia;
    FSpeed :=   TVKNavigatorAbstractParameters(Source).FSpeed;
    FCutoff :=  TVKNavigatorAbstractParameters(Source).FCutoff;
  end
  else
    inherited; //to the pit of doom ;)
end;

constructor TVKNavigatorAbstractParameters.Create(
  AOwner: TPersistent);
begin
  FOwner := AOwner;
  FInertia := 100;
  FSpeed := 0.005;
  FCutoff := EPS;
end;

function TVKNavigatorAbstractParameters.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

procedure TVKNavigatorAbstractParameters.ScaleParameters(
  const Value: Single);
begin
  Assert(Value > 0);

  if Value < 1 then
    FInertia := FInertia / PowerSingle(2, Value)
  else
    FInertia := FInertia * PowerSingle(2, 1 / Value);
end;

function TVKNavigatorAbstractParameters.StoreCutoff: Boolean;
begin
  Result := Abs(FCutoff - EPS) > EPS2;
end;

function TVKNavigatorAbstractParameters.StoreInertia: Boolean;
begin
  Result := Abs(FInertia - 100) > EPS;
end;

function TVKNavigatorAbstractParameters.StoreSpeed: Boolean;
begin
  Result := Abs(FSpeed - 0.005) > EPS2;
end;

{ TVKNavigatorAdjustDistanceParametersEx }

procedure TVKNavigatorAdjustDistanceParametersEx.Assign(
  Source: TPersistent);
begin
  if Source is TVKNavigatorAdjustDistanceParametersEx then
  begin
    FTargetDistance := TVKNavigatorAdjustDistanceParametersEx(Source).FTargetDistance;
    FSpeedLimit := TVKNavigatorAdjustDistanceParametersEx(Source).FSpeedLimit;
  end
  else
    inherited;
end;

constructor TVKNavigatorAdjustDistanceParametersEx.Create(
  AOwner: TPersistent);
begin
  inherited;
  FInertia := 0.5;
  FTargetDistance := 100;
  FSpeed := 100;
  FSpeedLimit := 20000;
end;

function TVKNavigatorAdjustDistanceParametersEx.StoreInertia: Boolean;
begin
  Result := Abs(FInertia - 0.5) > EPS2;
end;

function TVKNavigatorAdjustDistanceParametersEx.StoreSpeed: Boolean;
begin
  Result := Abs(FSpeed - 100) > EPS2;
end;

function TVKNavigatorAdjustDistanceParametersEx.StoreSpeedLimit: Boolean;
begin
  Result := Abs(FSpeedLimit - 20000) > EPS2;
end;

function TVKNavigatorAdjustDistanceParametersEx.StoreTargetDistance: Boolean;
begin
  Result := Abs(FTargetDistance - 100) > EPS2;
end;

{ TVKNavigatorSmoothChangeItem }

procedure TVKNavigatorSmoothChangeItem.Assign(Source: TPersistent);
begin
  inherited Assign(Source);

  if Source is TVKNavigatorSmoothChangeItem then
  begin
    FInertia :=    TVKNavigatorSmoothChangeItem(Source).FInertia;
    FSpeed :=      TVKNavigatorSmoothChangeItem(Source).FSpeed;
    FSpeedLimit := TVKNavigatorSmoothChangeItem(Source).FSpeedLimit;
    FCutoff :=     TVKNavigatorSmoothChangeItem(Source).FCutoff;
    FEnabled :=    TVKNavigatorSmoothChangeItem(Source).FEnabled;
  end;
end;

constructor TVKNavigatorSmoothChangeItem.Create(aOwner: TXCollection);
begin
  inherited;
  FInertia := 1;
  FSpeed := 5.5;
  FSpeedLimit := 20000;
  FCutoff := EPS;
  FEnabled := True;
end;

function TVKNavigatorSmoothChangeItem.GetNavigator: TVKSmoothNavigator;
begin
  Result := TVKSmoothNavigator(TVKNavigatorSmoothChangeItems(GetOwner).Owner);
end;

procedure TVKNavigatorSmoothChangeItem.ScaleParameters(
  const Value: Single);
begin
  Assert(Value > 0);

  if Value < 1 then
    FInertia := FInertia / PowerSingle(2, Value)
  else
    FInertia := FInertia * PowerSingle(2, 1 / Value);
end;

function TVKNavigatorSmoothChangeItem.StoreCutoff: Boolean;
begin
  Result := Abs(FCutoff - EPS) > EPS8;
end;

function TVKNavigatorSmoothChangeItem.StoreInertia: Boolean;
begin
  Result := Abs(FInertia - 1) > EPS;
end;

function TVKNavigatorSmoothChangeItem.StoreSpeed: Boolean;
begin
  Result := Abs(FSpeed - 5.5) > EPS2;
end;

function TVKNavigatorSmoothChangeItem.StoreSpeedLimit: Boolean;
begin
  Result := Abs(FSpeedLimit - 20000) > EPS2;
end;

{ TVKNavigatorSmoothChangeItems }

function TVKNavigatorSmoothChangeItems.Add(AClass : TVKNavigatorSmoothChangeItemClass): TVKNavigatorSmoothChangeItem;
begin
  Result := AClass.Create(Self);
end;

function TVKNavigatorSmoothChangeItems.CanAdd(AClass: TXCollectionItemClass): Boolean;
begin
  Result := AClass.InheritsFrom(TVKNavigatorSmoothChangeItem);
end;

procedure TVKNavigatorSmoothChangeItems.DoProceed(ADeltaTime: Double);
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    GetItems(I).Proceed(ADeltaTime);
end;

function TVKNavigatorSmoothChangeItems.GetItems(const Index : Integer): TVKNavigatorSmoothChangeItem;
begin
  Result := TVKNavigatorSmoothChangeItem(inherited GetItems(Index));
end;

class function TVKNavigatorSmoothChangeItems.ItemsClass: TXCollectionItemClass;
begin
  Result := TVKNavigatorSmoothChangeItem;
end;

procedure TVKNavigatorSmoothChangeItems.SetItems(const Index : Integer; const Value:
        TVKNavigatorSmoothChangeItem);
begin
  GetItems(Index).Assign(Value);
end;

{ TVKNavigatorSmoothChangeSingle }

procedure TVKNavigatorSmoothChangeSingle.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  
  if Source is TVKNavigatorSmoothChangeVector then
  begin
    FTargetValue := TVKNavigatorSmoothChangeSingle(Source).TargetValue;
    FOnGetCurrentValue := TVKNavigatorSmoothChangeSingle(Source).FOnGetCurrentValue;
    FOnSetCurrentValue := TVKNavigatorSmoothChangeSingle(Source).FOnSetCurrentValue;
  end;
end;

class function TVKNavigatorSmoothChangeSingle.FriendlyName: string;
begin
  Result := 'Navigator SmoothChange Single';
end;

function TVKNavigatorSmoothChangeSingle.Proceed(ADeltaTime: Double): Boolean;
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

procedure TVKNavigatorSmoothChangeSingle.ResetTargetValue;
begin
  FTargetValue := FOnGetCurrentValue(Self);
end;

{ TVKNavigatorSmoothChangeVector }

procedure TVKNavigatorSmoothChangeVector.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  
  if Source is TVKNavigatorSmoothChangeVector then
  begin
    FTargetValue.Assign(TVKNavigatorSmoothChangeVector(Source).TargetValue);
    FOnGetCurrentValue := TVKNavigatorSmoothChangeVector(Source).FOnGetCurrentValue;
    FOnSetCurrentValue := TVKNavigatorSmoothChangeVector(Source).FOnSetCurrentValue;
  end;
end;

constructor TVKNavigatorSmoothChangeVector.Create(aOwner: TXCollection);
begin
  inherited;
  FTargetValue := TVKCoordinates.CreateInitialized(Self, NullHmgVector, csVector);
end;

destructor TVKNavigatorSmoothChangeVector.Destroy;
begin
  FTargetValue.Free;
  inherited;
end;

class function TVKNavigatorSmoothChangeVector.FriendlyName: string;
begin
  Result := 'Navigator SmoothChange Vector';
end;

function TVKNavigatorSmoothChangeVector.Proceed(ADeltaTime: Double): Boolean;
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

procedure TVKNavigatorSmoothChangeVector.ResetTargetValue;
begin
  FTargetValue.DirectVector := FOnGetCurrentValue(Self);
end;

procedure TVKNavigatorSmoothChangeVector.SetTargetValue(
  const Value: TVKCoordinates);
begin
  FTargetValue.Assign(Value);
end;

initialization
  RegisterClasses([
      TVKSmoothNavigator, TVKSmoothUserInterface,
      TVKNavigatorInertiaParameters, TVKNavigatorGeneralParameters,
      TVKNavigatorMoveAroundParameters,
      TVKNavigatorAdjustDistanceParameters, TVKNavigatorAdjustDistanceParametersEx
                   ]);

  RegisterXCollectionItemClass(TVKNavigatorSmoothChangeSingle);
  RegisterXCollectionItemClass(TVKNavigatorSmoothChangeVector);
end.


