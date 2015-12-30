//
// This unit is part of the DGLEngine Project, http://DGLEngine.org
//
{ : DGLScene<p>

  Base classes and structures for DGLEngine.<p>

  <b>Historique : </b><font size=-1><ul>
  <li>21/12/15 - JD -  Imported From GLScene
  </ul></font>
}
unit DGLScene;

interface

{$I DGLEngine.inc}

uses
  Winapi.Windows,
  System.Classes, System.SysUtils, System.Math,
  VCL.Graphics, VCL.Controls,

  // DGLE
  DGLSLog, DGLResStrings,
  dglOpenGL, DGLCrossPlatform, DGLTypes,
  DGLContext, DGLState, DGLPipeLineTransformation, DGLRenderContextInfo, //DGLXOpenGL,

  DGLBaseClasses,
  DGLPersistentClasses,
  DGLXCollection,
  DGLApplicationFileIO,
  DGLUtils,

  DGLVectorMaths,
  DGLVectorTypes,
  DGLVectorLists,
  DGLGeometryBB,
  DGLCoordinates,

  DGLGraphics,
  DGLColor,
  DGLTextureFormat,
  DGLMaterial,
  DGLShader,

  DGLSilhouette;
  //DGLSelection;


Const
  GLSCENE_REVISION = '$Revision: 1$';
  GLSCENE_VERSION  = '2.0.0.%s';

Type
  // TDGLProxyObjectOption
  //
  { : Defines which features are taken from the master object. }
  TDGLProxyObjectOption  = (pooEffects, pooObjects, pooTransformation);
  TDGLProxyObjectOptions = set of TDGLProxyObjectOption;

const
  cDefaultProxyOptions = [pooEffects, pooObjects, pooTransformation];

Type
  // TObjectChanges
  //
  // used to decribe only the changes in an object,
  // which have to be reflected in the scene
  TObjectChange  = (ocTransformation, ocAbsoluteMatrix, ocInvAbsoluteMatrix, ocStructure);
  TObjectChanges = set of TObjectChange;

  TObjectBBChange  = (oBBcChild, oBBcStructure);
  TObjectBBChanges = set of TObjectBBChange;

  // TDirectRenderEvent
  //
  { : Event for user-specific rendering in a TDGLDirectOpenGL object. }
  TDirectRenderEvent = procedure(Sender: TObject; var rci: TRenderContextInfo) of object;



  TDGLProxyObjectClass = class of TDGLProxyObject;

  TDGLScene = class;
  TDGLSceneBuffer = class;
  TDGLBaseSceneObject   = class;
  TDGLSceneObjectClass  = class of TDGLBaseSceneObject;
  TDGLCustomSceneObject = class;
  TDGLBehaviour          = class;
  TDGLBehaviourClass     = class of TDGLBehaviour;
  TDGLBehaviours         = class;

  TDGLObjectEffect      = class;
  TDGLObjectEffectClass = class of TDGLObjectEffect;
  TDGLObjectEffects     = class;


  // IGLInitializable
  //
  { : Interface to objects that need initialization<p> }
  IGLInitializable = interface
    ['{EA40AE8E-79B3-42F5-ADF1-7A901B665E12}']
    procedure InitializeObject(ASender: TObject; const ARci: TRenderContextInfo);
  end;

  // TDGLInitializableObjectList
  //
  { : Just a list of objects that support IGLInitializable.<p> }
  TDGLInitializableObjectList = class(TList)
  private
    function GetItems(const Index: Integer): IGLInitializable;
    procedure PutItems(const Index: Integer; const Value: IGLInitializable);
  public
    function Add(const Item: IGLInitializable): Integer;
    property Items[const Index: Integer]: IGLInitializable read GetItems write PutItems; default;
  end;



  // TDGLBaseSceneObject
  //
  { : Base class for all scene objects.<p>
    A scene object is part of scene hierarchy (each scene object can have
    multiple children), this hierarchy primarily defines transformations
    (each child coordinates are relative to its parent), but is also used
    for depth-sorting, bounding and visibility culling purposes.<p>
    Subclasses implement either visual scene objects (that are made to be
    visible at runtime, like a Cube) or structural objects (that influence
    rendering or are used for varied structural manipulations,
    like the ProxyObject).<p>
    To add children at runtime, use the AddNewChild method of TDGLBaseSceneObject;
    other children manipulations methods and properties are provided (to browse,
    move and delete them). Using the regular TComponent methods is not
    encouraged. }

  TDGLBaseSceneObject = class(TDGLCoordinatesUpdateAbleComponent)
  private
    { Private Declarations }
    FAbsoluteMatrix, FInvAbsoluteMatrix: PMatrix;
    FLocalMatrix:                        PMatrix;
    FObjectStyle:                        TDGLObjectStyles;
  //  FListHandle: TDGLListHandle; // created on 1st use DEPRECATED

    // FVertices : TVectorList;
    // FNormals  : TAffineVectorList;
    // FIndices  : TIntegerList;
    // FTexCoord : TVectorList;
    // FTangents : TVectorList;
    // FBinormals : TAffineVectorList;

    FPosition:       TDGLCoordinates;
    FDirection, FUp: TDGLCoordinates;
    FScaling:        TDGLCoordinates;
    FChanges:        TObjectChanges;
    FParent:         TDGLBaseSceneObject;
    FScene:          TDGLScene;

    FBBChanges:                    TObjectBBChanges;
    FBoundingBoxPersonalUnscaled:  THmgBoundingBox;
    FBoundingBoxOfChildren:        THmgBoundingBox;
    FBoundingBoxIncludingChildren: THmgBoundingBox;

    FChildren:          TDGLPersistentObjectList; // created on 1st use
    FVisible:           Boolean;
    FUpdateCount:       Integer;
    FShowAxes:          Boolean;
    FRotation:          TDGLCoordinates; // current rotation angles
    FIsCalculating:     Boolean;
    FObjectsSorting:    TDGLObjectsSorting;
    FVisibilityCulling: TDGLVisibilityCulling;
    FOnProgress:        TDGLProgressEvent;
    FOnAddedToParent:   TNotifyEvent;
    FGLBehaviours:      TDGLBehaviours;
    FGLObjectEffects:   TDGLObjectEffects;
    FPickable:          Boolean;
    FOnPicked:          TNotifyEvent;

    FTagObject: TObject;
    FTagFloat:  Single;

    // FOriginalFiler: TFiler;   //used to allow persistent events in behaviours & effects
    { If somebody could look at DefineProperties, ReadBehaviours, ReadEffects and verify code
      is safe to use then it could be uncommented }

    function Get(Index: Integer): TDGLBaseSceneObject;
    function GetCount: Integer;
    function GetIndex: Integer;
    procedure SetParent(const val: TDGLBaseSceneObject);
    procedure SetIndex(aValue: Integer);
    procedure SetDirection(AVector: TDGLCoordinates);
    procedure SetUp(AVector: TDGLCoordinates);
    function GetMatrix: TMatrix;
    procedure SetMatrix(const aValue: TMatrix);
    procedure SetPosition(APosition: TDGLCoordinates);

    procedure SetPitchAngle(aValue: Single);
    procedure SetRollAngle(aValue: Single);
    procedure SetTurnAngle(aValue: Single);
    procedure SetRotation(aRotation: TDGLCoordinates);
    function GetPitchAngle: Single;
    function GetTurnAngle: Single;
    function GetRollAngle: Single;

    procedure SetShowAxes(aValue: Boolean);
    procedure SetScaling(aValue: TDGLCoordinates);
    procedure SetObjectsSorting(const val: TDGLObjectsSorting);
    procedure SetVisibilityCulling(const val: TDGLVisibilityCulling);
    procedure SetBehaviours(const val: TDGLBehaviours);
    function GetBehaviours: TDGLBehaviours;
    procedure SetEffects(const val: TDGLObjectEffects);
    function GetEffects: TDGLObjectEffects;

    function GetAbsoluteAffineScale: TAffineVector;
    function GetAbsoluteScale: TVector;
    procedure SetAbsoluteAffineScale(const Value: TAffineVector);
    procedure SetAbsoluteScale(const Value: TVector);

    function GetAbsoluteMatrix: TMatrix;
    procedure SetAbsoluteMatrix(const Value: TMatrix);
    procedure SetBBChanges(const Value: TObjectBBChanges);
  protected
    { Protected Declarations }
    procedure Loaded; override;
    procedure SetScene(const Value: TDGLScene); virtual;

    procedure DefineProperties(Filer: TFiler); override;
    procedure WriteBehaviours(stream: TStream);
    procedure ReadBehaviours(stream: TStream);
    procedure WriteEffects(stream: TStream);
    procedure ReadEffects(stream: TStream);
    procedure WriteRotations(stream: TStream);
    procedure ReadRotations(stream: TStream);

    function GetVisible: Boolean; virtual;
    function GetPickable: Boolean; virtual;
    procedure SetVisible(aValue: Boolean); virtual;
    procedure SetPickable(aValue: Boolean); virtual;

    procedure SetAbsolutePosition(const v: TVector);
    function GetAbsolutePosition: TVector;
    procedure SetAbsoluteUp(const v: TVector);
    function GetAbsoluteUp: TVector;
    procedure SetAbsoluteDirection(const v: TVector);
    function GetAbsoluteDirection: TVector;

    function GetAbsoluteAffinePosition: TAffineVector;
    procedure SetAbsoluteAffinePosition(const Value: TAffineVector);
    procedure SetAbsoluteAffineUp(const v: TAffineVector);
    function GetAbsoluteAffineUp: TAffineVector;
    procedure SetAbsoluteAffineDirection(const v: TAffineVector);
    function GetAbsoluteAffineDirection: TAffineVector;

    procedure RecTransformationChanged;

    procedure DrawAxes(var rci: TRenderContextInfo; pattern: Word);
    procedure GetChildren(AProc: TGetChildProc; Root: TComponent); override;
    // : Should the object be considered as blended for sorting purposes?
    function Blended: Boolean; virtual;
    procedure RebuildMatrix;
    procedure SetName(const NewName: TComponentName); override;
    procedure SetParentComponent(Value: TComponent); override;
    procedure DestroyHandle; dynamic;
    procedure DestroyHandles;
    procedure DeleteChildCameras;
    procedure DoOnAddedToParent; virtual;

    { : Used to re-calculate BoundingBoxes every time we need it.
      GetLocalUnscaleBB() must return the local BB, not the axis-aligned one.

      By default it is calculated from AxisAlignedBoundingBoxUnscaled and
      BarycenterAbsolutePosition, but for most objects there is a more
      efficient method, that's why it is virtual. }
    procedure CalculateBoundingBoxPersonalUnscaled(var ANewBoundingBox: THmgBoundingBox); virtual;
  public
    { Public Declarations }
    constructor Create(AOwner: TComponent); override;
    constructor CreateAsChild(aParentOwner: TDGLBaseSceneObject);
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;

    { : Controls and adjusts internal optimizations based on object's style.<p>
      Advanced user only. }
    property ObjectStyle: TDGLObjectStyles read FObjectStyle write FObjectStyle;

    { : Returns the handle to the object's build list.<p>
      Use with caution! Some objects don't support buildlists! }
    function GetHandle(var rci: TRenderContextInfo): Cardinal; virtual;
    function ListHandleAllocated: Boolean;

    { : The local transformation (relative to parent).<p>
      If you're *sure* the local matrix is up-to-date, you may use LocalMatrix
      for quicker access. }
    property Matrix: TMatrix read GetMatrix write SetMatrix;
    { : See Matrix. }
    function MatrixAsAddress: PMatrix;
    { : Holds the local transformation (relative to parent).<p>
      If you're not *sure* the local matrix is up-to-date, use Matrix property. }
    property LocalMatrix: PMatrix read FLocalMatrix;
    { : Forces the local matrix to the specified value.<p>
      AbsoluteMatrix, InverseMatrix, etc. will honour that change, but
      may become invalid if the specified matrix isn't orthonormal (can
      be used for specific rendering or projection effects).<br>
      The local matrix will be reset by the next TransformationChanged,
      position or attitude change. }
    procedure ForceLocalMatrix(const aMatrix: TMatrix);

    { : See AbsoluteMatrix. }
    function AbsoluteMatrixAsAddress: PMatrix;
    { : Holds the absolute transformation matrix.<p>
      If you're not *sure* the absolute matrix is up-to-date,
      use the AbsoluteMatrix property, this one may be nil... }
    property DirectAbsoluteMatrix: PMatrix read FAbsoluteMatrix;

    { : Calculates the object's absolute inverse matrix.<p>
      Multiplying an absolute coordinate with this matrix gives a local coordinate.<p>
      The current implem uses transposition(AbsoluteMatrix), which is true
      unless you're using some scaling... }
    function InvAbsoluteMatrix: TMatrix;
    { : See InvAbsoluteMatrix. }
    function InvAbsoluteMatrixAsAddress: PMatrix;

    { : The object's absolute matrix by composing all local matrices.<p>
      Multiplying a local coordinate with this matrix gives an absolute coordinate. }
    property AbsoluteMatrix: TMatrix read GetAbsoluteMatrix write SetAbsoluteMatrix;

    { : Direction vector in absolute coordinates. }
    property AbsoluteDirection: TVector read GetAbsoluteDirection write SetAbsoluteDirection;
    property AbsoluteAffineDirection: TAffineVector read GetAbsoluteAffineDirection write SetAbsoluteAffineDirection;

    { : Scale vector in absolute coordinates.
      Warning: SetAbsoluteScale() does not work correctly at the moment. }
    property AbsoluteScale: TVector read GetAbsoluteScale write SetAbsoluteScale;
    property AbsoluteAffineScale: TAffineVector read GetAbsoluteAffineScale write SetAbsoluteAffineScale;

    { : Up vector in absolute coordinates. }
    property AbsoluteUp: TVector read GetAbsoluteUp write SetAbsoluteUp;
    property AbsoluteAffineUp: TAffineVector read GetAbsoluteAffineUp write SetAbsoluteAffineUp;

    { : Calculate the right vector in absolute coordinates. }
    function AbsoluteRight: TVector;

    { : Calculate the left vector in absolute coordinates. }
    function AbsoluteLeft: TVector;

    { : Computes and allows to set the object's absolute coordinates.<p> }
    property AbsolutePosition: TVector read GetAbsolutePosition write SetAbsolutePosition;
    property AbsoluteAffinePosition: TAffineVector read GetAbsoluteAffinePosition write SetAbsoluteAffinePosition;
    function AbsolutePositionAsAddress: PVector;

    { : Returns the Absolute X Vector expressed in local coordinates. }
    function AbsoluteXVector: TVector;
    { : Returns the Absolute Y Vector expressed in local coordinates. }
    function AbsoluteYVector: TVector;
    { : Returns the Absolute Z Vector expressed in local coordinates. }
    function AbsoluteZVector: TVector;

    { : Converts a vector/point from absolute coordinates to local coordinates.<p> }
    function AbsoluteToLocal(const v: TVector): TVector; overload;
    { : Converts a vector from absolute coordinates to local coordinates.<p> }
    function AbsoluteToLocal(const v: TAffineVector): TAffineVector; overload;
    { : Converts a vector/point from local coordinates to absolute coordinates.<p> }
    function LocalToAbsolute(const v: TVector): TVector; overload;
    { : Converts a vector from local coordinates to absolute coordinates.<p> }
    function LocalToAbsolute(const v: TAffineVector): TAffineVector; overload;

    { : Returns the Right vector (based on Up and Direction) }
    function Right: TVector;
    { : Returns the Left vector (based on Up and Direction) }
    function LeftVector: TVector;

    { : Returns the Right vector (based on Up and Direction) }
    function AffineRight: TAffineVector;
    { : Returns the Left vector (based on Up and Direction) }
    function AffineLeftVector: TAffineVector;

    { : Calculates the object's square distance to a point/object.<p>
      pt is assumed to be in absolute coordinates,
      AbsolutePosition is considered as being the object position. }
    function SqrDistanceTo(anObject: TDGLBaseSceneObject): Single; overload;
    function SqrDistanceTo(const pt: TVector): Single; overload;
    function SqrDistanceTo(const pt: TAffineVector): Single; overload;

    { : Computes the object's distance to a point/object.<p>
      Only objects AbsolutePositions are considered. }
    function DistanceTo(anObject: TDGLBaseSceneObject): Single; overload;
    function DistanceTo(const pt: TAffineVector): Single; overload;
    function DistanceTo(const pt: TVector): Single; overload;

    { : Calculates the object's barycenter in absolute coordinates.<p>
      Default behaviour is to consider Barycenter=AbsolutePosition
      (whatever the number of children).<br>
      SubClasses where AbsolutePosition is not the barycenter should
      override this method as it is used for distance calculation, during
      rendering for instance, and may lead to visual inconsistencies. }
    function BarycenterAbsolutePosition: TVector; virtual;
    { : Calculates the object's barycenter distance to a point.<p> }
    function BarycenterSqrDistanceTo(const pt: TVector): Single;

    { : Shall returns the object's axis aligned extensions.<p>
      The dimensions are measured from object center and are expressed
      <i>with</i> scale accounted for, in the object's coordinates
      (not in absolute coordinates).<p>
      Default value is half the object's Scale.<br> }
    function AxisAlignedDimensions: TVector; virtual;
    function AxisAlignedDimensionsUnscaled: TVector; virtual;

    { : Calculates and return the AABB for the object.<p>
      The AABB is currently calculated from the BB.
      There is <b>no</b> caching scheme for them. }
    function AxisAlignedBoundingBox(const AIncludeChilden: Boolean = True): TAABB;
    function AxisAlignedBoundingBoxUnscaled(const AIncludeChilden: Boolean = True): TAABB;
    function AxisAlignedBoundingBoxAbsolute(const AIncludeChilden: Boolean = True; const AUseBaryCenter: Boolean = False): TAABB;

    { : Advanced AABB functions that use a caching scheme.
      Also they include children and use BaryCenter. }
    function AxisAlignedBoundingBoxEx: TAABB;
    function AxisAlignedBoundingBoxAbsoluteEx: TAABB;

    { : Calculates and return the Bounding Box for the object.<p>
      The BB is calculated <b>each</b> time this method is invoked,
      based on the AxisAlignedDimensions of the object and that of its
      children.
      There is <b>no</b> caching scheme for them. }
    function BoundingBox(const AIncludeChilden: Boolean = True; const AUseBaryCenter: Boolean = False): THmgBoundingBox;
    function BoundingBoxUnscaled(const AIncludeChilden: Boolean = True; const AUseBaryCenter: Boolean = False): THmgBoundingBox;
    function BoundingBoxAbsolute(const AIncludeChilden: Boolean = True; const AUseBaryCenter: Boolean = False): THmgBoundingBox;

    { : Advanced BB functions that use a caching scheme.
      Also they include children and use BaryCenter. }
    function BoundingBoxPersonalUnscaledEx: THmgBoundingBox;
    function BoundingBoxOfChildrenEx: THmgBoundingBox;
    function BoundingBoxIncludingChildrenEx: THmgBoundingBox;

    { : Max distance of corners of the BoundingBox. }
    function BoundingSphereRadius: Single;
    function BoundingSphereRadiusUnscaled: Single;

    { : Indicates if a point is within an object.<p>
      Given coordinate is an absolute coordinate.<br>
      Linear or surfacic objects shall always return False.<p>
      Default value is based on AxisAlignedDimension and a cube bounding. }
    function PointInObject(const point: TVector): Boolean; virtual;
    { : Request to determine an intersection with a casted ray.<p>
      Given coordinates & vector are in absolute coordinates, rayVector
      must be normalized.<br>
      rayStart may be a point inside the object, allowing retrieval of
      the multiple intersects of the ray.<p>
      When intersectXXX parameters are nil (default) implementation should
      take advantage of this to optimize calculus, if not, and an intersect
      is found, non nil parameters should be defined.<p>
      The intersectNormal needs NOT be normalized by the implementations.<p>
      Default value is based on bounding sphere. }
    function RayCastIntersect(const rayStart, rayVector: TVector; intersectPoint: PVector = nil; intersectNormal: PVector = nil): Boolean; virtual;

    { : Request to generate silhouette outlines.<p>
      Default implementation assumes the objects is a sphere of
      AxisAlignedDimensionUnscaled size. Subclasses may choose to return
      nil instead, which will be understood as an empty silhouette. }
    function GenerateSilhouette(const silhouetteParameters: TDGLSilhouetteParameters): TDGLSilhouette; virtual;

    property Children[Index: Integer]: TDGLBaseSceneObject read Get; default;
    property Count: Integer read GetCount;
    property Index: Integer read GetIndex write SetIndex;
    // : Create a new scene object and add it to this object as new child
    function AddNewChild(AChild: TDGLSceneObjectClass): TDGLBaseSceneObject; dynamic;
    // : Create a new scene object and add it to this object as first child
    function AddNewChildFirst(AChild: TDGLSceneObjectClass): TDGLBaseSceneObject; dynamic;
    procedure AddChild(AChild: TDGLBaseSceneObject); dynamic;

    function GetOrCreateBehaviour(aBehaviour: TDGLBehaviourClass): TDGLBehaviour;
    function AddNewBehaviour(aBehaviour: TDGLBehaviourClass): TDGLBehaviour;

    function GetOrCreateEffect(anEffect: TDGLObjectEffectClass): TDGLObjectEffect;
    function AddNewEffect(anEffect: TDGLObjectEffectClass): TDGLObjectEffect;

    function HasSubChildren: Boolean;
    procedure DeleteChildren; dynamic;
    procedure Insert(AIndex: Integer; AChild: TDGLBaseSceneObject); dynamic;
    { : Takes a scene object out of the child list, but doesn't destroy it.<p>
      If 'KeepChildren' is true its children will be kept as new children
      in this scene object. }
    procedure Remove(AChild: TDGLBaseSceneObject; keepChildren: Boolean); dynamic;
    function IndexOfChild(AChild: TDGLBaseSceneObject): Integer;
    function FindChild(const aName: string; ownChildrenOnly: Boolean): TDGLBaseSceneObject;
    { : The "safe" version of this procedure checks if indexes are inside
      the list. If not, no exception if raised. }
    procedure ExchangeChildrenSafe(anIndex1, anIndex2: Integer);
    { : The "regular" version of this procedure does not perform any checks
      and calls FChildren.Exchange directly. User should/can perform range
      checks manualy. }
    procedure ExchangeChildren(anIndex1, anIndex2: Integer);
    { : These procedures are safe. }
    procedure MoveChildUp(anIndex: Integer);
    procedure MoveChildDown(anIndex: Integer);
    procedure MoveChildFirst(anIndex: Integer);
    procedure MoveChildLast(anIndex: Integer);

    procedure DoProgress(const progressTime: TProgressTimes); override;
    procedure MoveTo(newParent: TDGLBaseSceneObject); dynamic;
    procedure MoveUp;
    procedure MoveDown;
    procedure MoveFirst;
    procedure MoveLast;
    procedure BeginUpdate; virtual;
    procedure EndUpdate; virtual;
    { : Make object-specific geometry description here.<p>
      Subclasses should MAINTAIN OpenGL states (restore the states if
      they were altered). }
    procedure BuildList(var rci: TRenderContextInfo); virtual;
    function GetParentComponent: TComponent; override;
    function HasParent: Boolean; override;
    function IsUpdating: Boolean;
    // : Moves the object along the Up vector (move up/down)
    procedure Lift(ADistance: Single);
    // : Moves the object along the direction vector
    procedure Move(ADistance: Single);
    // : Translates the object
    procedure Translate(tx, ty, tz: Single);
    procedure MoveObjectAround(anObject: TDGLBaseSceneObject; pitchDelta, turnDelta: Single);
    procedure MoveObjectAllAround(anObject: TDGLBaseSceneObject; pitchDelta, turnDelta: Single);
    procedure Pitch(angle: Single);
    procedure Roll(angle: Single);
    procedure Turn(angle: Single);

    { : Sets all rotations to zero and restores default Direction/Up.<p>
      Using this function then applying roll/pitch/turn in the order that
      suits you, you can give an "absolute" meaning to rotation angles
      (they are still applied locally though).<br>
      Scale and Position are not affected. }
    procedure ResetRotations;
    { : Reset rotations and applies them back in the specified order. }
    procedure ResetAndPitchTurnRoll(const degX, degY, degZ: Single);

    { : Applies rotations around absolute X, Y and Z axis.<p> }
    procedure RotateAbsolute(const rx, ry, rz: Single); overload;
    { : Applies rotations around the absolute given vector (angle in degrees).<p> }
    procedure RotateAbsolute(const axis: TAffineVector; angle: Single); overload;
    // : Moves camera along the right vector (move left and right)
    procedure Slide(ADistance: Single);
    // : Orients the object toward a target object
    procedure PointTo(const ATargetObject: TDGLBaseSceneObject; const AUpVector: TVector); overload;
    // : Orients the object toward a target absolute position
    procedure PointTo(const AAbsolutePosition, AUpVector: TVector); overload;

    procedure Render(var ARci: TRenderContextInfo);
    procedure DoRender(var ARci: TRenderContextInfo; ARenderSelf, ARenderChildren: Boolean); virtual;
    procedure RenderChildren(firstChildIndex, lastChildIndex: Integer; var rci: TRenderContextInfo); virtual;

    procedure StructureChanged; dynamic;
    procedure ClearStructureChanged;

    // : Recalculate an orthonormal system
    procedure CoordinateChanged(Sender: TDGLCustomCoordinates); override;
    procedure TransformationChanged;
    procedure NotifyChange(Sender: TObject); override;

    property Rotation: TDGLCoordinates read FRotation write SetRotation;
    property PitchAngle: Single read GetPitchAngle write SetPitchAngle;
    property RollAngle: Single read GetRollAngle write SetRollAngle;
    property TurnAngle: Single read GetTurnAngle write SetTurnAngle;

    property ShowAxes: Boolean read FShowAxes write SetShowAxes default False;

    property Changes: TObjectChanges read FChanges;
    property BBChanges: TObjectBBChanges read FBBChanges write SetBBChanges;

    property Parent: TDGLBaseSceneObject read FParent write SetParent;
    property Position: TDGLCoordinates read FPosition write SetPosition;
    property Direction: TDGLCoordinates read FDirection write SetDirection;
    property Up: TDGLCoordinates read FUp write SetUp;
    property Scale: TDGLCoordinates read FScaling write SetScaling;
    property Scene: TDGLScene read FScene;
    property Visible: Boolean read FVisible write SetVisible default True;
    property Pickable: Boolean read FPickable write SetPickable default True;
    property ObjectsSorting: TDGLObjectsSorting read FObjectsSorting write SetObjectsSorting default osInherited;
    property VisibilityCulling: TDGLVisibilityCulling read FVisibilityCulling write SetVisibilityCulling default vcInherited;
    property OnProgress: TDGLProgressEvent read FOnProgress write FOnProgress;
    property OnPicked: TNotifyEvent read FOnPicked write FOnPicked;
    property OnAddedToParent: TNotifyEvent read FOnAddedToParent write FOnAddedToParent;
    property Behaviours: TDGLBehaviours read GetBehaviours write SetBehaviours stored False;
    property Effects: TDGLObjectEffects read GetEffects write SetEffects stored False;

    property TagObject: TObject read FTagObject write FTagObject;
  published
    { Published Declarations }
    property TagFloat: Single read FTagFloat write FTagFloat;

  end;

  // TDGLSceneRootObject
  //
  { : This class shall be used only as a hierarchy root.<p>
    It exists only as a container and shall never be rotated/scaled etc. as
    the class type is used in parenting optimizations.<p>
    Shall never implement or add any functionality, the "Create" override
    only take cares of disabling the build list. }
  TDGLSceneRootObject = class(TDGLBaseSceneObject)
  public
    { Public Declarations }
    constructor Create(AOwner: TComponent); override;
  end;
  // TDGLBaseBehaviour
  //
  { : Base class for implementing behaviours in TDGLScene.<p>
    Behaviours are regrouped in a collection attached to a TDGLBaseSceneObject,
    and are part of the "Progress" chain of events. Behaviours allows clean
    application of time-based alterations to objects (movements, shape or
    texture changes...).<p>
    Since behaviours are implemented as classes, there are basicly two kinds
    of strategies for subclasses :<ul>
    <li>stand-alone : the subclass does it all, and holds all necessary data
    (covers animation, inertia etc.)
    <li>proxy : the subclass is an interface to and external, shared operator
    (like gravity, force-field effects etc.)
    </ul><br>
    Some behaviours may be cooperative (like force-fields affects inertia)
    or unique (e.g. only one inertia behaviour per object).<p>
    NOTES :<ul>
    <li>Don't forget to override the ReadFromFiler/WriteToFiler persistence
    methods if you add data in a subclass !
    <li>Subclasses must be registered using the RegisterXCollectionItemClass
    function
    </ul> }
  TDGLBaseBehaviour = class(TDGLXCollectionItem)
  protected
    { Protected Declarations }
    procedure SetName(const val: string); override;

    { : Override this function to write subclass data. }
    procedure WriteToFiler(writer: TWriter); override;
    { : Override this function to read subclass data. }
    procedure ReadFromFiler(reader: TReader); override;

    { : Returns the TDGLBaseSceneObject on which the behaviour should be applied.<p>
      Does NOT check for nil owners. }
    function OwnerBaseSceneObject: TDGLBaseSceneObject;

  public
    { Public Declarations }
    constructor Create(AOwner: TDGLXCollection); override;
    destructor Destroy; override;

    procedure DoProgress(const progressTime: TProgressTimes); virtual;
  end;

  // TDGLBehaviour
  //
  { : Ancestor for non-rendering behaviours.<p>
    This class shall never receive any properties, it's just here to differentiate
    rendereing and non-rendering behaviours. Rendereing behaviours are named
    "TDGLObjectEffect", non-rendering effects (like inertia) are simply named
    "TDGLBehaviour". }

  TDGLBehaviour = class(TDGLBaseBehaviour)
  end;

  // TDGLBehaviours
  //
  { : Holds a list of TDGLBehaviour objects.<p>
    This object expects itself to be owned by a TDGLBaseSceneObject.<p>
    As a TDGLXCollection (and contrary to a TCollection), this list can contain
    objects of varying class, the only constraint being that they should all
    be TDGLBehaviour subclasses. }
  TDGLBehaviours = class(TDGLXCollection)
  protected
    { Protected Declarations }
    function GetBehaviour(Index: Integer): TDGLBehaviour;

  public
    { Public Declarations }
    constructor Create(AOwner: TPersistent); override;

    function GetNamePath: string; override;

    class function ItemsClass: TDGLXCollectionItemClass; override;

    property Behaviour[index: Integer]: TDGLBehaviour read GetBehaviour; default;

    function CanAdd(aClass: TDGLXCollectionItemClass): Boolean; override;

    procedure DoProgress(const progressTimes: TProgressTimes);
  end;

  // TDGLObjectEffect
  //
  { : A rendering effect that can be applied to SceneObjects.<p>
    ObjectEffect is a subclass of behaviour that gets a chance to Render
    an object-related special effect.<p>
    TDGLObjectEffect should not be used as base class for custom effects,
    instead you should use the following base classes :<ul>
    <li>TDGLObjectPreEffect is rendered before owner object render
    <li>TDGLObjectPostEffect is rendered after the owner object render
    <li>TDGLObjectAfterEffect is rendered at the end of the scene rendering
    </ul><br>NOTES :<ul>
    <li>Don't forget to override the ReadFromFiler/WriteToFiler persistence
    methods if you add data in a subclass !
    <li>Subclasses must be registered using the RegisterXCollectionItemClass
    function
    </ul> }
  // TDGLObjectEffectClass = class of TDGLObjectEffect;

  TDGLObjectEffect = class(TDGLBaseBehaviour)
  protected
    { Protected Declarations }
    { : Override this function to write subclass data. }
    procedure WriteToFiler(writer: TWriter); override;
    { : Override this function to read subclass data. }
    procedure ReadFromFiler(reader: TReader); override;

  public
    { Public Declarations }
    procedure Render(var rci: TRenderContextInfo); virtual;
  end;

  // TDGLObjectPreEffect
  //
  { : An object effect that gets rendered before owner object's render.<p>
    The current OpenGL matrices and material are that of the owner object. }
  TDGLObjectPreEffect = class(TDGLObjectEffect)
  end;

  // TDGLObjectPostEffect
  //
  { : An object effect that gets rendered after owner object's render.<p>
    The current OpenGL matrices and material are that of the owner object. }
  TDGLObjectPostEffect = class(TDGLObjectEffect)
  end;

  // TDGLObjectAfterEffect
  //
  { : An object effect that gets rendered at scene's end.<p>
    No particular OpenGL matrices or material should be assumed. }
  TDGLObjectAfterEffect = class(TDGLObjectEffect)
  end;

  // TDGLObjectEffects
  //
  { : Holds a list of object effects.<p>
    This object expects itself to be owned by a TDGLBaseSceneObject.<p> }
  TDGLObjectEffects = class(TDGLXCollection)
  protected
    { Protected Declarations }
    function GetEffect(Index: Integer): TDGLObjectEffect;

  public
    { Public Declarations }
    constructor Create(AOwner: TPersistent); override;

    function GetNamePath: string; override;

    class function ItemsClass: TDGLXCollectionItemClass; override;

    property ObjectEffect[index: Integer]: TDGLObjectEffect read GetEffect; default;

    function CanAdd(aClass: TDGLXCollectionItemClass): Boolean; override;

    procedure DoProgress(const progressTime: TProgressTimes);
    procedure RenderPreEffects(var rci: TRenderContextInfo);
    { : Also take care of registering after effects with the GLSceneViewer. }
    procedure RenderPostEffects(var rci: TRenderContextInfo);
  end;

  // TDGLCustomSceneObject
  //
  { : Extended base scene object class with a material property.<p>
    The material allows defining a color and texture for the object,
    see TDGLMaterial. }
  TDGLCustomSceneObject = class(TDGLBaseSceneObject)
  private
    { Private Declarations }
    FShader: TDGLLibShader;
    FHint:     string;

  protected
    { Protected Declarations }
    function Blended: Boolean; override;

    procedure SetShader(aValue: TDGLLibShader);
    procedure DestroyHandle; override;
    procedure Loaded; override;

  public
    { Public Declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Assign(Source: TPersistent); override;
    procedure DoRender(var ARci: TRenderContextInfo; ARenderSelf, ARenderChildren: Boolean); override;

    property Shader: TDGLLibShader read FShader write SetShader;
    property Hint: string read FHint write FHint;
  end;

  // TDGImmaterialSceneObject
  //
  { : Base class for objects that do not have a published "material".<p>
    Note that the material is available in public properties, but isn't
    applied automatically before invoking BuildList.<br>
    Subclassing should be reserved to structural objects and objects that
    have no material of their own. }
  TDGImmaterialSceneObject = class(TDGLCustomSceneObject)
  public
    { Public Declarations }
    procedure DoRender(var ARci: TRenderContextInfo; ARenderSelf, ARenderChildren: Boolean); override;

  published
    { Published Declarations }
    property ObjectsSorting;
    property VisibilityCulling;
    property Direction;
    property PitchAngle;
    property Position;
    property RollAngle;
    property Scale;
    property ShowAxes;
    property TurnAngle;
    property Up;
    property Visible;
    property Pickable;
    property OnProgress;
    property OnPicked;
    property Behaviours;
    property Effects;
    property Hint;
  end;

  // TDGLCameraInvariantObject
  //
  { : Base class for camera invariant objects.<p>
    Camera invariant objects bypass camera settings, such as camera
    position (object is always centered on camera) or camera orientation
    (object always has same orientation as camera). }
  TDGLCameraInvariantObject = class(TDGImmaterialSceneObject)
  private
    { Private Declarations }
    FCamInvarianceMode: TDGLCameraInvarianceMode;

  protected
    { Protected Declarations }
    procedure SetCamInvarianceMode(const val: TDGLCameraInvarianceMode);

    property CamInvarianceMode: TDGLCameraInvarianceMode read FCamInvarianceMode write SetCamInvarianceMode;

  public
    { Public Declarations }
    constructor Create(AOwner: TComponent); override;

    procedure Assign(Source: TPersistent); override;
    procedure DoRender(var ARci: TRenderContextInfo; ARenderSelf, ARenderChildren: Boolean); override;
  end;


  // TOnCustomPerspective
  //
  TOnCustomPerspective = procedure(const viewport: TRectangle; width, height: Integer; DPI: Integer; var viewPortRadius: Single) of object;

  // TDGLCamera
  //
  { : Camera object.<p>
    This object is commonly referred by TDGLSceneViewer and defines a position,
    direction, focal length, depth of view... all the properties needed for
    defining a point of view and optical characteristics. }
  TDGLCamera = class(TDGLBaseSceneObject)
  private
    { Private Declarations }
    FFocalLength:         Single;
    FDepthOfView:         Single;
    FNearPlane:           Single; // nearest distance to the camera
    FNearPlaneBias:       Single; // scaling bias applied to near plane
    FViewPortRadius:      Single; // viewport bounding radius per distance unit
    FTargetObject:        TDGLBaseSceneObject;
    FLastDirection:       TVector; // Not persistent
    FCameraStyle:         TDGLCameraStyle;
    FKeepFOVMode:         TDGLCameraKeepFOVMode;
    FSceneScale:          Single;
    FDeferredApply:       TNotifyEvent;
    FOnCustomPerspective: TOnCustomPerspective;
    FDesign:              Boolean;
    FFOVY, FFOVX:         Double;

  protected
    { Protected Declarations }
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure SetTargetObject(const val: TDGLBaseSceneObject);
    procedure SetDepthOfView(AValue: Single);
    procedure SetFocalLength(AValue: Single);
    procedure SetCameraStyle(const val: TDGLCameraStyle);
    procedure SetKeepFOVMode(const val: TDGLCameraKeepFOVMode);
    procedure SetSceneScale(value: Single);
    function StoreSceneScale: Boolean;
    procedure SetNearPlaneBias(value: Single);
    function StoreNearPlaneBias: Boolean;

  public
    { Public Declarations }
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;

    { : Nearest clipping plane for the frustum.<p>
      This value depends on the FocalLength and DepthOfView fields and
      is calculated to minimize Z-Buffer crawling as suggested by the
      OpenGL documentation. }
    property NearPlane: Single read FNearPlane;

    // : Apply camera transformation
    procedure Apply;
    procedure DoRender(var ARci: TRenderContextInfo; ARenderSelf, ARenderChildren: Boolean); override;
    function RayCastIntersect(const rayStart, rayVector: TVector; intersectPoint: PVector = nil; intersectNormal: PVector = nil): Boolean; override;

    procedure ApplyPerspective(const AViewport: TRectangle; AWidth, AHeight: Integer; ADPI: Integer);
    procedure AutoLeveling(Factor: Single);
    procedure Reset(aSceneBuffer: TDGLSceneBuffer);
    // : Position the camera so that the whole scene can be seen
    procedure ZoomAll(aSceneBuffer: TDGLSceneBuffer);

    procedure RotateObject(obj: TDGLBaseSceneObject; pitchDelta, turnDelta: Single; rollDelta: Single = 0);
    procedure RotateTarget(pitchDelta, turnDelta: Single; rollDelta: Single = 0);

    { : Change camera's position to make it move around its target.<p>
      If TargetObject is nil, nothing happens. This method helps in quickly
      implementing camera controls. Camera's Up and Direction properties
      are unchanged.<br>
      Angle deltas are in degrees, camera parent's coordinates should be identity.<p>
      Tip : make the camera a child of a "target" dummycube and make
      it a target the dummycube. Now, to pan across the scene, just move
      the dummycube, to change viewing angle, use this method. }
    procedure MoveAroundTarget(pitchDelta, turnDelta: Single);
    { : Change camera's position to make it move all around its target.<p>
      If TargetObject is nil, nothing happens. This method helps in quickly
      implementing camera controls. Camera's Up and Direction properties
      are changed.<br>
      Angle deltas are in degrees.<p> }
    procedure MoveAllAroundTarget(pitchDelta, turnDelta: Single);
    { : Moves the camera in eye space coordinates. }
    procedure MoveInEyeSpace(forwardDistance, rightDistance, upDistance: Single);
    { : Moves the target in eye space coordinates. }
    procedure MoveTargetInEyeSpace(forwardDistance, rightDistance, upDistance: Single);
    { : Computes the absolute vector corresponding to the eye-space translations. }
    function AbsoluteEyeSpaceVector(forwardDistance, rightDistance, upDistance: Single): TVector;
    { : Adjusts distance from camera to target by applying a ratio.<p>
      If TargetObject is nil, nothing happens. This method helps in quickly
      implementing camera controls. Only the camera's position is changed. }
    procedure AdjustDistanceToTarget(distanceRatio: Single);
    { : Returns the distance from camera to target.<p>
      If TargetObject is nil, returns 1. }
    function DistanceToTarget: Single;
    { : Computes the absolute normalized vector to the camera target.<p>
      If no target is defined, AbsoluteDirection is returned. }
    function AbsoluteVectorToTarget: TVector;
    { : Computes the absolute normalized right vector to the camera target.<p>
      If no target is defined, AbsoluteRight is returned. }
    function AbsoluteRightVectorToTarget: TVector;
    { : Computes the absolute normalized up vector to the camera target.<p>
      If no target is defined, AbsoluteUpt is returned. }
    function AbsoluteUpVectorToTarget: TVector;
    { : Calculate an absolute translation vector from a screen vector.<p>
      Ratio is applied to both screen delta, planeNormal should be the
      translation plane's normal. }
    function ScreenDeltaToVector(deltaX, deltaY: Integer; ratio: Single; const planeNormal: TVector): TVector;
    { : Same as ScreenDeltaToVector but optimized for XY plane. }
    function ScreenDeltaToVectorXY(deltaX, deltaY: Integer; ratio: Single): TVector;
    { : Same as ScreenDeltaToVector but optimized for XZ plane. }
    function ScreenDeltaToVectorXZ(deltaX, deltaY: Integer; ratio: Single): TVector;
    { : Same as ScreenDeltaToVector but optimized for YZ plane. }
    function ScreenDeltaToVectorYZ(deltaX, deltaY: Integer; ratio: Single): TVector;
    { : Returns true if a point is in front of the camera. }
    function PointInFront(const point: TVector): Boolean; overload;
    { : Calculates the field of view in degrees, given a viewport dimension
      (width or height). F.i. you may wish to use the minimum of the two. }
    function GetFieldOfView(const AViewportDimension: Single): Single;
    { : Sets the FocalLength in degrees, given a field of view and a viewport
      dimension (width or height). }
    procedure SetFieldOfView(const AFieldOfView, AViewportDimension: Single);
  published
    { Published Declarations }
    { : Depth of field/view.<p>
      Adjusts the maximum distance, beyond which objects will be clipped
      (ie. not visisble).<p>
      You must adjust this value if you are experiencing disappearing
      objects (increase the value) of Z-Buffer crawling (decrease the
      value). Z-Buffer crawling happens when depth of view is too large
      and the Z-Buffer precision cannot account for all that depth
      accurately : objects farther overlap closer objects and vice-versa.<p>
      Note that this value is ignored in cSOrtho2D mode. }
    property DepthOfView: Single read FDepthOfView write SetDepthOfView;
    { : Focal Length of the camera.<p>
      Adjusting this value allows for lens zooming effects (use SceneScale
      for linear zooming). This property affects near/far planes clipping. }
    property FocalLength: Single read FFocalLength write SetFocalLength;
    { : Scene scaling for camera point.<p>
      This is a linear 2D scaling of the camera's output, allows for
      linear zooming (use FocalLength for lens zooming). }
    property SceneScale: Single read FSceneScale write SetSceneScale stored StoreSceneScale;
    { : Scaling bias applied to near-plane calculation.<p>
      Values inferior to one will move the nearplane nearer, and also
      reduce medium/long range Z-Buffer precision, values superior
      to one will move the nearplane farther, and also improve medium/long
      range Z-Buffer precision. }
    property NearPlaneBias: Single read FNearPlaneBias write SetNearPlaneBias stored StoreNearPlaneBias;
    { : If set, camera will point to this object.<p>
      When camera is pointing an object, the Direction vector is ignored
      and the Up vector is used as an absolute vector to the up. }
    property TargetObject: TDGLBaseSceneObject read FTargetObject write SetTargetObject;
    { : Adjust the camera style.<p>
      Three styles are available :<ul>
      <li>csPerspective, the default value for perspective projection
      <li>csOrthogonal, for orthogonal (or isometric) projection.
      <li>csOrtho2D, setups orthogonal 2D projection in which 1 unit
      (in x or y) represents 1 pixel.
      <li>csInfinitePerspective, for perspective view without depth limit.
      <li>csKeepCamAnglePerspective, for perspective view with keeping aspect on view resize.
      <li>csCustom, setup is deferred to the OnCustomPerspective event.
      </ul> }
    property CameraStyle: TDGLCameraStyle read FCameraStyle write SetCameraStyle default csPerspective;

    { : Keep camera angle mode. <p>
      When CameraStyle is csKeepCamAnglePerspective, select which camera angle you want to keep.
      <li>kaHeight, for Keep Height oriented camera angle
      <li>kaWidth,  for Keep Width oriented camera angle
    }
    property KeepFOVMode: TDGLCameraKeepFOVMode read FKeepFOVMode write SetKeepFOVMode default ckmHorizontalFOV;

    { : Custom perspective event.<p>
      This event allows you to specify your custom perpective, either
      with a glFrustrum, a glOrtho or whatever method suits you.<br>
      You must compute viewPortRadius for culling to work.<br>
      This event is only called if CameraStyle is csCustom. }
    property OnCustomPerspective: TOnCustomPerspective read FOnCustomPerspective write FOnCustomPerspective;

    property Position;
    property Direction;
    property Up;
    property OnProgress;
  end;

  // TDGLSceneObject
  //
  { : Base class for standard scene objects.<p>
    Publishes the Material property. }
  TDGLSceneObject = class(TDGLCustomSceneObject)
  published
    { Published Declarations }
    property Shader;
    property ObjectsSorting;
    property VisibilityCulling;
    property Direction;
    property PitchAngle;
    property Position;
    property RollAngle;
    property Scale;
    property ShowAxes;
    property TurnAngle;
    property Up;
    property Visible;
    property Pickable;
    property OnProgress;
    property OnPicked;
    property Behaviours;
    property Effects;
    property Hint;
  end;

  // TDGLRenderPoint
  //
  { : Scene object that allows other objects to issue rendering at some point.<p>
    This object is used to specify a render point for which other components
    have (rendering) tasks to perform. It doesn't render anything itself
    and is invisible, but other components can register and be notified
    when the point is reached in the rendering phase.<br>
    Callbacks must be explicitly unregistered. }
  TDGLRenderPoint = class(TDGImmaterialSceneObject)
  private
    { Private Declarations }
    FCallBacks:     array of TDirectRenderEvent;
    FFreeCallBacks: array of TNotifyEvent;

  protected
    { Protected Declarations }

  public
    { Public Declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure BuildList(var rci: TRenderContextInfo); override;

    procedure RegisterCallBack(renderEvent: TDirectRenderEvent; renderPointFreed: TNotifyEvent);
    procedure UnRegisterCallBack(renderEvent: TDirectRenderEvent);
    procedure Clear;

  published
    { Published Declarations }
  end;

  // TDGLProxyObject
  //
  { : A full proxy object.<p>
    This object literally uses another object's Render method to do its own
    rendering, however, it has a coordinate system and a life of its own.<br>
    Use it for duplicates of an object. }
  TDGLProxyObject = class(TDGLBaseSceneObject)
  private
    { Private Declarations }
    FMasterObject: TDGLBaseSceneObject;
    FProxyOptions: TDGLProxyObjectOptions;

  protected
    { Protected Declarations }
    FRendering: Boolean;

    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure SetMasterObject(const val: TDGLBaseSceneObject); virtual;
    procedure SetProxyOptions(const val: TDGLProxyObjectOptions);

  public
    { Public Declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Assign(Source: TPersistent); override;
    procedure DoRender(var ARci: TRenderContextInfo; ARenderSelf, ARenderChildren: Boolean); override;

    function BarycenterAbsolutePosition: TVector; override;
    function AxisAlignedDimensions: TVector; override;
    function AxisAlignedDimensionsUnscaled: TVector; override;
    function RayCastIntersect(const rayStart, rayVector: TVector; intersectPoint: PVector = nil; intersectNormal: PVector = nil): Boolean; override;
    function GenerateSilhouette(const silhouetteParameters: TDGLSilhouetteParameters): TDGLSilhouette; override;

  published
    { Published Declarations }
    { : Specifies the Master object which will be proxy'ed. }
    property MasterObject: TDGLBaseSceneObject read FMasterObject write SetMasterObject;
    { : Specifies how and what is proxy'ed. }
    property ProxyOptions: TDGLProxyObjectOptions read FProxyOptions write SetProxyOptions default cDefaultProxyOptions;

    property ObjectsSorting;
    property Direction;
    property PitchAngle;
    property Position;
    property RollAngle;
    property Scale;
    property ShowAxes;
    property TurnAngle;
    property Up;
    property Visible;
    property Pickable;
    property OnProgress;
    property OnPicked;
    property Behaviours;
  end;


  // TDGLDirectOpenGL
  //
  { : Provides a way to issue direct OpenGL calls during the rendering.<p>
    You can use this object to do your specific rendering task in its OnRender
    event. The OpenGL calls shall restore the OpenGL states they found when
    entering, or exclusively use the GLMisc utility functions to alter the
    states.<br> }
  TDGLDirectOpenGL = class(TDGImmaterialSceneObject)
  private
    { Private Declarations }
    FUseBuildList: Boolean;
    FOnRender:     TDirectRenderEvent;
    FBlend:        Boolean;

  protected
    { Protected Declarations }
    procedure SetUseBuildList(const val: Boolean);
    function Blended: Boolean; override;
    procedure SetBlend(const val: Boolean);
  public
    { Public Declarations }
    constructor Create(AOwner: TComponent); override;

    procedure Assign(Source: TPersistent); override;
    procedure BuildList(var rci: TRenderContextInfo); override;

    function AxisAlignedDimensionsUnscaled: TVector; override;
  published
    { Published Declarations }
    { : Specifies if a build list be made.<p>
      If True, GLScene will generate a build list (OpenGL-side cache),
      ie. OnRender will only be invoked once for the first render, or after
      a StructureChanged call. This is suitable for "static" geometry and
      will usually speed up rendering of things that don't change.<br>
      If false, OnRender will be invoked for each render. This is suitable
      for dynamic geometry (things that change often or constantly). }
    property UseBuildList: Boolean read FUseBuildList write SetUseBuildList;
    { : Place your specific OpenGL code here.<p>
      The OpenGL calls shall restore the OpenGL states they found when
      entering, or exclusively use the GLMisc utility functions to alter
      the states.<br> }
    property OnRender: TDirectRenderEvent read FOnRender write FOnRender;
    { : Defines if the object uses blending.<p>
      This property will allow direct opengl objects to be flagged as
      blended for object sorting purposes.<br> }
    property Blend: Boolean read FBlend write SetBlend;
  end;



  // TDGLLightSource
  //
  { : Standard light source.<p>
    The standard GLScene light source covers spotlights, omnidirectionnal and
    parallel sources (see TLightStyle).<br>
    Lights are colored, have distance attenuation parameters and are turned
    on/off through their Shining property.<p>
    Lightsources are managed in a specific object by the TDGLScene for rendering
    purposes. The maximum number of light source in a scene is limited by the
    OpenGL implementation (8 lights are supported under most ICDs), though the
    more light you use, the slower rendering may get. If you want to render
    many more light/lightsource, you may have to resort to other techniques
    like lightmapping. }
  TDGLLightSource = class(TDGLBaseSceneObject)
  private
    { Private Declarations }
    FLightID:                                                     Cardinal;
    FSpotDirection:                                               TDGLCoordinates;
    FSpotExponent, FSpotCutOff:                                   Single;
    FConstAttenuation, FLinearAttenuation, FQuadraticAttenuation: Single;
    FShining:                                                     Boolean;
    FAmbient, FDiffuse, FSpecular:                                TDGLColor;
    FLightStyle:                                                  TLightStyle;

  protected
    { Protected Declarations }
    procedure SetAmbient(AValue: TDGLColor);
    procedure SetDiffuse(AValue: TDGLColor);
    procedure SetSpecular(AValue: TDGLColor);
    procedure SetConstAttenuation(AValue: Single);
    procedure SetLinearAttenuation(AValue: Single);
    procedure SetQuadraticAttenuation(AValue: Single);
    procedure SetShining(AValue: Boolean);
    procedure SetSpotDirection(AVector: TDGLCoordinates);
    procedure SetSpotExponent(AValue: Single);
    procedure SetSpotCutOff(const val: Single);
    procedure SetLightStyle(const val: TLightStyle);

  public
    { Public Declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure DoRender(var ARci: TRenderContextInfo; ARenderSelf, ARenderChildren: Boolean); override;
    // : light sources have different handle types than normal scene objects
    function GetHandle(var rci: TRenderContextInfo): Cardinal; override;
    function RayCastIntersect(const rayStart, rayVector: TVector; intersectPoint: PVector = nil; intersectNormal: PVector = nil): Boolean; override;
    procedure CoordinateChanged(Sender: TDGLCustomCoordinates); override;
    function GenerateSilhouette(const silhouetteParameters: TDGLSilhouetteParameters): TDGLSilhouette; override;

    property LightID: Cardinal read FLightID;

    function Attenuated: Boolean;

  published
    { Published Declarations }
    property Ambient:              TDGLColor read FAmbient write SetAmbient;
    property ConstAttenuation:     Single read FConstAttenuation write SetConstAttenuation;
    property Diffuse:              TDGLColor read FDiffuse write SetDiffuse;
    property LinearAttenuation:    Single read FLinearAttenuation write SetLinearAttenuation;
    property QuadraticAttenuation: Single read FQuadraticAttenuation write SetQuadraticAttenuation;
    property Position;
    property LightStyle:    TLightStyle read FLightStyle write SetLightStyle default lsSpot;
    property Shining:       Boolean read FShining write SetShining default True;
    property Specular:      TDGLColor read FSpecular write SetSpecular;
    property SpotCutOff:    Single read FSpotCutOff write SetSpotCutOff;
    property SpotDirection: TDGLCoordinates read FSpotDirection write SetSpotDirection;
    property SpotExponent:  Single read FSpotExponent write SetSpotExponent;
    property OnProgress;
  end;

  // TDGLScene
  //
  { : Scene object.<p>
    The scene contains the scene description (lights, geometry...), which is
    basicly a hierarchical scene graph made of TDGLBaseSceneObject. It will
    usually contain one or more TDGLCamera object, which can be referred by
    a Viewer component for rendering purposes.<p>
    The scene's objects can be accessed directly from Delphi code (as regular
    components), but those are edited with a specific editor (double-click
    on the TDGLScene component at design-time to invoke it). To add objects
    at runtime, use the AddNewChild method of TDGLBaseSceneObject. }
  TDGLScene = class(TDGLUpdateAbleComponent)
  private
    { Private Declarations }
    FUpdateCount:          Integer;
    FObjects:              TDGLSceneRootObject;
    FBaseContext:          TDGLContext; // reference, not owned!
    FLights, FBuffers:     TDGLPersistentObjectList;
    FCurrenTDGLCamera:     TDGLCamera;
    FCurrentBuffer:        TDGLSceneBuffer;
    FObjectsSorting:       TDGLObjectsSorting;
    FVisibilityCulling:    TDGLVisibilityCulling;
    FOnBeforeProgress:     TDGLProgressEvent;
    FOnProgress:           TDGLProgressEvent;
    FCurrentDeltaTime:     Double;
    FInitializableObjects: TDGLInitializableObjectList;

  protected
    { Protected Declarations }
    procedure AddLight(aLight: TDGLLightSource);
    procedure RemoveLight(aLight: TDGLLightSource);
    // : Adds all lights in the subtree (anObj included)
    procedure AddLights(anObj: TDGLBaseSceneObject);
    // : Removes all lights in the subtree (anObj included)
    procedure RemoveLights(anObj: TDGLBaseSceneObject);

    procedure GetChildren(AProc: TGetChildProc; Root: TComponent); override;
    procedure SetChildOrder(AChild: TComponent; Order: Integer); override;
    procedure SetObjectsSorting(const val: TDGLObjectsSorting);
    procedure SetVisibilityCulling(const val: TDGLVisibilityCulling);

    procedure ReadState(Reader: TReader); override;
  public
    { Public Declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure BeginUpdate;
    procedure EndUpdate;
    function IsUpdating: Boolean;

    procedure AddBuffer(aBuffer: TDGLSceneBuffer);
    procedure RemoveBuffer(aBuffer: TDGLSceneBuffer);
    procedure SetupLights(maxLights: Integer);
    procedure NotifyChange(Sender: TObject); override;
    procedure Progress(const deltaTime, newTime: Double);

    function FindSceneObject(const AName: string): TDGLBaseSceneObject;
    { : Calculates, finds and returns the first object intercepted by the ray.<p>
      Returns nil if no intersection was found. This function will be
      accurate only for objects that overrided their RayCastIntersect
      method with accurate code, otherwise, bounding sphere intersections
      will be returned. }
    function RayCastIntersect(const rayStart, rayVector: TVector; intersectPoint: PVector = nil; intersectNormal: PVector = nil): TDGLBaseSceneObject; virtual;

    procedure ShutdownAllLights;

    { : Saves the scene to a file (recommended extension : .GLS) }
    procedure SaveToFile(const fileName: string);
    { : Load the scene from a file.<p>
      Existing objects/lights/cameras are freed, then the file is loaded.<br>
      Delphi's IDE is not handling this behaviour properly yet, ie. if
      you load a scene in the IDE, objects will be properly loaded, but
      no declare will be placed in the code. }
    procedure LoadFromFile(const fileName: string);

    procedure SaveToStream(aStream: TStream);
    procedure LoadFromStream(aStream: TStream);

    { : Saves the scene to a text file }
    procedure SaveToTextFile(const fileName: string);
    { : Load the scene from a text files.<p>
      See LoadFromFile for details. }
    procedure LoadFromTextFile(const fileName: string);

    property CurrenTDGLCamera: TDGLCamera read FCurrenTDGLCamera;
    property Lights: TDGLPersistentObjectList read FLights;
    property Objects: TDGLSceneRootObject read FObjects;
    property CurrentBuffer: TDGLSceneBuffer read FCurrentBuffer;

    { : List of objects that request to be initialized when rendering context is active.<p>
      They are removed automaticly from this list once initialized. }
    property InitializableObjects: TDGLInitializableObjectList read FInitializableObjects;
    property CurrentDeltaTime: Double read FCurrentDeltaTime;
  published
    { Published Declarations }
    { : Defines default ObjectSorting option for scene objects. }
    property ObjectsSorting: TDGLObjectsSorting read FObjectsSorting write SetObjectsSorting default osRenderBlendedLast;
    { : Defines default VisibilityCulling option for scene objects. }
    property VisibilityCulling: TDGLVisibilityCulling read FVisibilityCulling write SetVisibilityCulling default vcNone;
    property OnBeforeProgress:  TDGLProgressEvent read FOnBeforeProgress write FOnBeforeProgress;
    property OnProgress:        TDGLProgressEvent read FOnProgress write FOnProgress;
  end;


  // TDGLFogEnvironment ------> MOVE TO DGLEnvironment
  //
  { : Parameters for fog environment in a scene.<p>
    The fog descibed by this object is a distance-based fog, ie. the "intensity"
    of the fog is given by a formula depending solely on the distance, this
    intensity is used for blending to a fixed color. }
  TDGLFogEnvironment = class(TDGLUpdateAbleObject)
  private
    { Private Declarations }
    FSceneBuffer:       TDGLSceneBuffer;
    FFogColor:          TDGLColor; // alpha value means the fog density
    FFogStart, FFogEnd: Single;
    FFogMode:           TFogMode;
    FFogDistance:       TFogDistance;

  protected
    { Protected Declarations }
    procedure SetFogColor(Value: TDGLColor);
    procedure SetFogStart(Value: Single);
    procedure SetFogEnd(Value: Single);
    procedure SetFogMode(Value: TFogMode);
    procedure SetFogDistance(const val: TFogDistance);

  public
    { Public Declarations }
    constructor Create(AOwner: TPersistent); override;
    destructor Destroy; override;

    procedure ApplyFog;
    procedure Assign(Source: TPersistent); override;

    function IsAtDefaultValues: Boolean;

  published
    { Published Declarations }
    { : Color of the fog when it is at 100% intensity. }
    property FogColor: TDGLColor read FFogColor write SetFogColor;
    { : Minimum distance for fog, what is closer is not affected. }
    property FogStart: Single read FFogStart write SetFogStart;
    { : Maximum distance for fog, what is farther is at 100% fog intensity. }
    property FogEnd: Single read FFogEnd write SetFogEnd;
    { : The formula used for converting distance to fog intensity. }
    property FogMode: TFogMode read FFogMode write SetFogMode default fmLinear;
    { : Adjusts the formula used for calculating fog distances.<p>
      This option is honoured if and only if the OpenGL ICD supports the
      GL_NV_fog_distance extension, otherwise, it is ignored.<ul>
      <li>fdDefault: let OpenGL use its default formula
      <li>fdEyeRadial: uses radial "true" distance (best quality)
      <li>fdEyePlane: uses the distance to the projection plane
      (same as Z-Buffer, faster)
      </ul> }
    property FogDistance: TFogDistance read FFogDistance write SetFogDistance default fdDefault;
  end;


  // TDGLSceneBuffer
  //
  { : Encapsulates an OpenGL frame/rendering buffer.<p> }
  TDGLSceneBuffer = class(TDGLUpdateAbleObject)
  private
    { Private Declarations }
    // Internal state
    FRendering:              Boolean;
    FRenderingContext:       TDGLContext;
    FAfterRenderEffects:     TDGLPersistentObjectList;
      //FViewMatrixStack:        array of TMatrix;
      //FProjectionMatrixStack:  array of TMatrix;
    FBaseProjectionMatrix:   TMatrix;
    FCameraAbsolutePosition: TVector;
    FViewPort:               TRectangle;
   // FSelector:               TDGLBaseSelectTechnique;

    // Options & User Properties
    FFaceCulling, FFogEnable, FLighting: Boolean;
    FDepthTest:                          Boolean;
    FBackgroundColor:                    TColor;
    FBackgroundAlpha:                    Single;
    FAmbientColor:                       TDGLColor;
    FAntiAliasing:                       TDGLAntiAliasing;
    FDepthPrecision:                     TDGLDepthPrecision;
    FColorDepth:                         TDGLColorDepth;
    FContextOptions:                     TContextOptions;
    FShadeModel:                         TDGLShadeModel;
    FRenderDPI:                          Integer;
    FFogEnvironment:                     TDGLFogEnvironment;
    FAccumBufferBits:                    Integer;
    FLayer:                              TDGLContextLayer;

    // Cameras
    FCamera: TDGLCamera;

    // Freezing
    FFreezeBuffer:    Pointer;
    FFreezed:         Boolean;
    FFreezedViewPort: TRectangle;

    // Monitoring
    FFrameCount:       Longint;
    FFramesPerSecond:  Single;
    FFirstPerfCounter: Int64;
    FLastFrameTime:    Single;

    // Events
    FOnChange:           TNotifyEvent;
    FOnStructuralChange: TNotifyEvent;
    FOnPrepareGLContext: TNotifyEvent;

    FBeforeRender:       TNotifyEvent;
    FViewerBeforeRender: TNotifyEvent;
    FPostRender:         TNotifyEvent;
    FAfterRender:        TNotifyEvent;
    FInitiateRendering:  TDirectRenderEvent;
    FWrapUpRendering:    TDirectRenderEvent;
    procedure SetLayer(const Value: TDGLContextLayer);

  protected
    { Protected Declarations }
    procedure SetBackgroundColor(AColor: TColor);
    procedure SetBackgroundAlpha(alpha: Single);
    procedure SetAmbientColor(AColor: TDGLColor);
    function GetLimit(Which: TLimitType): Integer;
    procedure SetCamera(ACamera: TDGLCamera);
    procedure SetContextOptions(Options: TContextOptions);
    procedure SetDepthTest(AValue: Boolean);
    procedure SetFaceCulling(AValue: Boolean);
    procedure SetLighting(AValue: Boolean);
    procedure SetAntiAliasing(const val: TDGLAntiAliasing);
    procedure SetDepthPrecision(const val: TDGLDepthPrecision);
    procedure SetColorDepth(const val: TDGLColorDepth);
    procedure SetShadeModel(const val: TDGLShadeModel);
    procedure SetFogEnable(AValue: Boolean);
    procedure SeTDGLFogEnvironment(AValue: TDGLFogEnvironment);
    function StoreFog: Boolean;
    procedure SetAccumBufferBits(const val: Integer);

    procedure PrepareRenderingMatrices(const aViewPort: TRectangle; resolution: Integer; pickingRect: PGLRect = nil);
    procedure DoBaseRender(const aViewPort: TRectangle; resolution: Integer; drawState: TDrawState; baseObject: TDGLBaseSceneObject);

    procedure SetupRenderingContext(context: TDGLContext);
    procedure SetupRCOptions(context: TDGLContext);
    procedure PrepareGLContext;

    procedure DoChange;
    procedure DoStructuralChange;

    // : DPI for current/last render
    property RenderDPI: Integer read FRenderDPI;

    property OnPrepareGLContext: TNotifyEvent read FOnPrepareGLContext write FOnPrepareGLContext;

  public
    { Public Declarations }
    constructor Create(AOwner: TPersistent); override;
    destructor Destroy; override;

    procedure NotifyChange(Sender: TObject); override;

    procedure CreateRC(AWindowHandle: HWND; memoryContext: Boolean; BufferCount: Integer = 1); overload;
    procedure ClearBuffers;
    procedure DestroyRC;
    function RCInstantiated: Boolean;
    procedure Resize(newLeft, newTop, newWidth, newHeight: Integer);
    // : Indicates hardware acceleration support
    // function Acceleration: TDGLContextAcceleration;

    // : ViewPort for current/last render
    property ViewPort: TRectangle read FViewPort;

    // : Fills the PickList with objects in Rect area
//    procedure PickObjects(const rect: TGLRect; pickList: TDGLPickList; objectCountGuess: Integer);
    { : Returns a PickList with objects in Rect area.<p>
      Returned list should be freed by caller.<br>
      Objects are sorted by depth (nearest objects first). }
//    function GetPickedObjects(const rect: TGLRect; objectCountGuess: Integer = 64): TDGLPickList;
    // : Returns the nearest object at x, y coordinates or nil if there is none
//    function GetPickedObject(x, y: Integer): TDGLBaseSceneObject;

    // : Returns the color of the pixel at x, y in the frame buffer
    function GetPixelColor(x, y: Integer): TColor;
    { : Returns the raw depth (Z buffer) of the pixel at x, y in the frame buffer.<p>
      This value does not map to the actual eye-object distance, but to
      a depth buffer value in the [0; 1] range. }
    function GetPixelDepth(x, y: Integer): Single;
    { : Converts a raw depth (Z buffer value) to frustrum distance.
      This calculation is only accurate for the pixel at the centre of the viewer,
      because it does not take into account that the corners of the frustrum
      are further from the eye than its centre. }
    function PixelDepthToDistance(aDepth: Single): Single;
    { : Converts a raw depth (Z buffer value) to world distance.
      It also compensates for the fact that the corners of the frustrum
      are further from the eye, than its centre. }
    function PixelToDistance(x, y: Integer): Single;
    { : Design time notification }
    procedure NotifyMouseMove(Shift: TShiftState; x, y: Integer);

    { : Renders the scene on the viewer.<p>
      You do not need to call this method, unless you explicitly want a
      render at a specific time. If you just want the control to get
      refreshed, use Invalidate instead. }
    procedure Render(baseObject: TDGLBaseSceneObject); overload;
    procedure Render; overload;
    procedure RenderScene(aScene: TDGLScene; const viewPortSizeX, viewPortSizeY: Integer; drawState: TDrawState; baseObject: TDGLBaseSceneObject);
    { : Render the scene to a bitmap at given DPI.<p>
      DPI = "dots per inch".<p>
      The "magic" DPI of the screen is 96 under Windows. }
    procedure RenderToBitmap(ABitmap: TDGLBitmap; DPI: Integer = 0);
    { : Render the scene to a bitmap at given DPI and saves it to a file.<p>
      DPI = "dots per inch".<p>
      The "magic" DPI of the screen is 96 under Windows. }
    procedure RenderToFile(const AFile: string; DPI: Integer = 0); overload;
    { : Renders to bitmap of given size, then saves it to a file.<p>
      DPI is adjusted to make the bitmap similar to the viewer. }
    procedure RenderToFile(const AFile: string; bmpWidth, bmpHeight: Integer); overload;
    { : Creates a TDGLBitmap32 that is a snapshot of current OpenGL content.<p>
      When possible, use this function instead of RenderToBitmap, it won't
      request a redraw and will be significantly faster.<p>
      The returned TDGLBitmap32 should be freed by calling code. }
    function CreateSnapShot: TDGLImage;
    { : Creates a VCL bitmap that is a snapshot of current OpenGL content.<p> }
    function CreateSnapShotBitmap: TDGLBitmap;
//    procedure CopyToTexture(aTexture: TDGLTexture); overload;
//    procedure CopyToTexture(aTexture: TDGLTexture; xSrc, ySrc, AWidth, AHeight: Integer; xDest, yDest: Integer; glCubeFace: TGLEnum = 0); overload;
    { : Save as raw float data to a file }
    procedure SaveAsFloatToFile(const aFilename: string);
    { : Event reserved for viewer-specific uses.<br> }
    property ViewerBeforeRender: TNotifyEvent read FViewerBeforeRender write FViewerBeforeRender stored False;
    procedure SetViewPort(x, y, W, H: Integer);
    function Width: Integer;
    function Height: Integer;

    { : Indicates if the Viewer is "frozen". }
    property Freezed: Boolean read FFreezed;
    { : Freezes rendering leaving the last rendered scene on the buffer. This
      is usefull in windowed applications for temporarily stoping rendering
      (when moving the window, for example). }
    procedure Freeze;
    { : Restarts rendering after it was freezed. }
    procedure Melt;

    { : Displays a window with info on current OpenGL ICD and context. }
    procedure ShowInfo(Modal: Boolean = False);

    { : Currently Rendering? }
    property Rendering: Boolean read FRendering;

    { : Adjusts background alpha channel. }
    property BackgroundAlpha: Single read FBackgroundAlpha write SetBackgroundAlpha;
    { : Returns the projection matrix in use or used for the last rendering. }
    function ProjectionMatrix: TMatrix; //deprecated;
    { : Returns the view matrix in use or used for the last rendering. }
    function ViewMatrix: TMatrix; //deprecated;
    function ModelMatrix: TMatrix; //deprecated;

    { : Returns the base projection matrix in use or used for the last rendering.<p>
      The "base" projection is (as of now) either identity or the pick
      matrix, ie. it is the matrix on which the perspective or orthogonal
      matrix gets applied. }
    property BaseProjectionMatrix: TMatrix read FBaseProjectionMatrix;

    { : Back up current View matrix and replace it with newMatrix.<p>
      This method has no effect on the OpenGL matrix, only on the Buffer's
      matrix, and is intended for special effects rendering. }
    procedure PushViewMatrix(const newMatrix: TMatrix); //deprecated;
    { : Restore a View matrix previously pushed. }
    procedure PopViewMatrix; //deprecated;

    procedure PushProjectionMatrix(const newMatrix: TMatrix); //deprecated;
    procedure PopProjectionMatrix; //deprecated;

    { : Converts a screen pixel coordinate into 3D coordinates for orthogonal projection.<p>
      This function accepts standard canvas coordinates, with (0,0) being
      the top left corner, and returns, when the camera is in orthogonal
      mode, the corresponding 3D world point that is in the camera's plane. }
    function OrthoScreenToWorld(screenX, screenY: Integer): TAffineVector; overload;
    { : Converts a screen coordinate into world (3D) coordinates.<p>
      This methods wraps a call to gluUnProject.<p>
      Note that screen coord (0,0) is the lower left corner. }
    function ScreenToWorld(const aPoint: TAffineVector): TAffineVector; overload;
    function ScreenToWorld(const aPoint: TVector): TVector; overload;
    { : Converts a screen pixel coordinate into 3D world coordinates.<p>
      This function accepts standard canvas coordinates, with (0,0) being
      the top left corner. }
    function ScreenToWorld(screenX, screenY: Integer): TAffineVector; overload;
    { : Converts an absolute world coordinate into screen coordinate.<p>
      This methods wraps a call to gluProject.<p>
      Note that screen coord (0,0) is the lower left corner. }
    function WorldToScreen(const aPoint: TAffineVector): TAffineVector; overload;
    function WorldToScreen(const aPoint: TVector): TVector; overload;
    { : Converts a set of point absolute world coordinates into screen coordinates.<p> }
    procedure WorldToScreen(points: PVector; nbPoints: Integer); overload;
    { : Calculates the 3D vector corresponding to a 2D screen coordinate.<p>
      The vector originates from the camera's absolute position and is
      expressed in absolute coordinates.<p>
      Note that screen coord (0,0) is the lower left corner. }
    function ScreenToVector(const aPoint: TAffineVector): TAffineVector; overload;
    function ScreenToVector(const aPoint: TVector): TVector; overload;
    function ScreenToVector(const x, y: Integer): TVector; overload;
    { : Calculates the 2D screen coordinate of a vector from the camera's
      absolute position and is expressed in absolute coordinates.<p>
      Note that screen coord (0,0) is the lower left corner. }
    function VectorToScreen(const VectToCam: TAffineVector): TAffineVector;
    { : Calculates intersection between a plane and screen vector.<p>
      If an intersection is found, returns True and places result in
      intersectPoint. }
    function ScreenVectorIntersectWithPlane(const aScreenPoint: TVector; const planePoint, planeNormal: TVector; var intersectPoint: TVector): Boolean;
    { : Calculates intersection between plane XY and screen vector.<p>
      If an intersection is found, returns True and places result in
      intersectPoint. }
    function ScreenVectorIntersectWithPlaneXY(const aScreenPoint: TVector; const z: Single; var intersectPoint: TVector): Boolean;
    { : Calculates intersection between plane YZ and screen vector.<p>
      If an intersection is found, returns True and places result in
      intersectPoint. }
    function ScreenVectorIntersectWithPlaneYZ(const aScreenPoint: TVector; const x: Single; var intersectPoint: TVector): Boolean;
    { : Calculates intersection between plane XZ and screen vector.<p>
      If an intersection is found, returns True and places result in
      intersectPoint. }
    function ScreenVectorIntersectWithPlaneXZ(const aScreenPoint: TVector; const y: Single; var intersectPoint: TVector): Boolean;
    { : Calculates a 3D coordinate from screen position and ZBuffer.<p>
      This function returns a world absolute coordinate from a 2D point
      in the viewer, the depth being extracted from the ZBuffer data
      (DepthTesting and ZBuffer must be enabled for this function to work).<br>
      Note that ZBuffer precision is not linear and can be quite low on
      some boards (either from compression or resolution approximations). }
    function PixelRayToWorld(x, y: Integer): TAffineVector;
    { : Time (in second) spent to issue rendering order for the last frame.<p>
      Be aware that since execution by the hardware isn't synchronous,
      this value may not be an accurate measurement of the time it took
      to render the last frame, it's a measurement of only the time it
      took to issue rendering orders. }
    property LastFrameTime: Single read FLastFrameTime;
    { : Current FramesPerSecond rendering speed.<p>
      You must keep the renderer busy to get accurate figures from this
      property.<br>
      This is an average value, to reset the counter, call
      ResetPerfomanceMonitor. }
    property FramesPerSecond: Single read FFramesPerSecond;
    { : Resets the perfomance monitor and begin a new statistics set.<p>
      See FramesPerSecond. }
    procedure ResetPerformanceMonitor;

    { : Retrieve one of the OpenGL limits for the current viewer.<p>
      Limits include max texture size, OpenGL stack depth, etc. }
    property LimitOf[Which: TLimitType]: Integer read GetLimit;
    { : Current rendering context.<p>
      The context is a wrapper around platform-specific contexts
      (see TDGLContext) and takes care of context activation and handle
      management. }
    property RenderingContext: TDGLContext read FRenderingContext;
    { : The camera from which the scene is rendered.<p>
      A camera is an object you can add and define in a TDGLScene component. }
    property Camera: TDGLCamera read FCamera write SetCamera;
    { : Specifies the layer plane that the rendering context is bound to. }
    property Layer: TDGLContextLayer read FLayer write SetLayer default clMainPlane;
  published
    { Published Declarations }
    { : Fog environment options.<p>
      See TDGLFogEnvironment. }
    property FogEnvironment: TDGLFogEnvironment read FFogEnvironment write SeTDGLFogEnvironment stored StoreFog;
    { : Color used for filling the background prior to any rendering. }
    property BackgroundColor: TColor read FBackgroundColor write SetBackgroundColor default clBtnFace;
    { : Scene ambient color vector.<p>
      This ambient color is defined independantly from all lightsources,
      which can have their own ambient components. }
    property AmbientColor: TDGLColor read FAmbientColor write SetAmbientColor;

    { : Context options allows to setup specifics of the rendering context.<p>
      Not all contexts support all options. }
    property ContextOptions: TContextOptions read FContextOptions write SetContextOptions default [roDoubleBuffer, roRenderToWindow, roDebugContext];
    { : Number of precision bits for the accumulation buffer. }
    property AccumBufferBits: Integer read FAccumBufferBits write SetAccumBufferBits default 0;
    { : DepthTest enabling.<p>
      When DepthTest is enabled, objects closer to the camera will hide
      farther ones (via use of Z-Buffering).<br>
      When DepthTest is disabled, the latest objects drawn/rendered overlap
      all previous objects, whatever their distance to the camera.<br>
      Even when DepthTest is enabled, objects may chose to ignore depth
      testing through the osIgnoreDepthBuffer of their ObjectStyle property. }
    property DepthTest: Boolean read FDepthTest write SetDepthTest default True;
    { : Enable or disable face culling in the renderer.<p>
      Face culling is used in hidden faces removal algorithms : each face
      is given a normal or 'outside' direction. When face culling is enabled,
      only faces whose normal points towards the observer are rendered. }
    property FaceCulling: Boolean read FFaceCulling write SetFaceCulling default True;
    { : Toggle to enable or disable the fog settings. }
    property FogEnable: Boolean read FFogEnable write SetFogEnable default False;
    { : Toggle to enable or disable lighting calculations.<p>
      When lighting is enabled, objects will be lit according to lightsources,
      when lighting is disabled, objects are rendered in their own colors,
      without any shading.<p>
      Lighting does NOT generate shadows in OpenGL. }
    property Lighting: Boolean read FLighting write SetLighting default True;
    { : AntiAliasing option.<p>
      Ignored if not hardware supported, currently based on ARB_multisample. }
    property AntiAliasing: TDGLAntiAliasing read FAntiAliasing write SetAntiAliasing default aaDefault;
    { : Depth buffer precision.<p>
      Default is highest available (below and including 24 bits) }
    property DepthPrecision: TDGLDepthPrecision read FDepthPrecision write SetDepthPrecision default dpDefault;
    { : Color buffer depth.<p>
      Default depth buffer is highest available (below and including 24 bits) }
    property ColorDepth: TDGLColorDepth read FColorDepth write SetColorDepth default cdDefault;
    { : Shade model.<p>
      Default is "Smooth".<p> }
    property ShadeModel: TDGLShadeModel read FShadeModel write SetShadeModel default smDefault;

    { : Indicates a change in the scene or buffer options.<p>
      A simple re-render is enough to take into account the changes. }
    property OnChange: TNotifyEvent read FOnChange write FOnChange stored False;
    { : Indicates a structural change in the scene or buffer options.<p>
      A reconstruction of the RC is necessary to take into account the
      changes (this may lead to a driver switch or lengthy operations). }
    property OnStructuralChange: TNotifyEvent read FOnStructuralChange write FOnStructuralChange stored False;

    { : Triggered before the scene's objects get rendered.<p>
      You may use this event to execute your own OpenGL rendering
      (usually background stuff). }
    property BeforeRender: TNotifyEvent read FBeforeRender write FBeforeRender stored False;
    { : Triggered after BeforeRender, before rendering objects.<p>
      This one is fired after the rci has been initialized and can be used
      to alter it or perform early renderings that require an rci,
      the Sender is the buffer. }
    property InitiateRendering: TDirectRenderEvent read FInitiateRendering write FInitiateRendering stored False;
    { : Triggered after rendering all scene objects, before PostRender.<p>
      This is the last point after which the rci becomes unavailable,
      the Sender is the buffer. }
    property WrapUpRendering: TDirectRenderEvent read FWrapUpRendering write FWrapUpRendering stored False;
    { : Triggered just after all the scene's objects have been rendered.<p>
      The OpenGL context is still active in this event, and you may use it
      to execute your own OpenGL rendering (usually for HUD, 2D overlays
      or after effects).<p> }
    property PostRender: TNotifyEvent read FPostRender write FPostRender stored False;
    { : Called after rendering.<p>
      You cannot issue OpenGL calls in this event, if you want to do your own
      OpenGL stuff, use the PostRender event. }
    property AfterRender: TNotifyEvent read FAfterRender write FAfterRender stored False;
  end;

  TInvokeInfoForm = procedure(aSceneBuffer: TDGLSceneBuffer; Modal: Boolean);

  { : Register an event handler triggered by any TDGLBaseSceneObject Name change.<p>
    *INCOMPLETE*, currently allows for only 1 (one) event, and is used by
    GLSceneEdit in the IDE. }
procedure RegisterGLBaseSceneObjectNameChangeEvent(notifyEvent: TNotifyEvent);
{ : Deregister an event handler triggered by any TDGLBaseSceneObject Name change.<p>
  See RegisterGLBaseSceneObjectNameChangeEvent. }
procedure DeRegisterGLBaseSceneObjectNameChangeEvent(notifyEvent: TNotifyEvent);
{ : Register an event handler triggered by any TDGLBehaviour Name change.<p>
  *INCOMPLETE*, currently allows for only 1 (one) event, and is used by
  FBehavioursEditor in the IDE. }
procedure RegisterGLBehaviourNameChangeEvent(notifyEvent: TNotifyEvent);
{ : Deregister an event handler triggered by any TDGLBaseSceneObject Name change.<p>
  See RegisterGLBaseSceneObjectNameChangeEvent. }
procedure DeRegisterGLBehaviourNameChangeEvent(notifyEvent: TNotifyEvent);

{ : Issues OpenGL calls for drawing X, Y, Z axes in a standard style. }
procedure AxesBuildList(var rci: TRenderContextInfo; pattern: Word; AxisLen: Single);

{ : Registers the procedure call used to invoke the info form. }
procedure RegisterInfoForm(infoForm: TInvokeInfoForm);
procedure InvokeInfoForm(aSceneBuffer: TDGLSceneBuffer; Modal: Boolean);

function GetCurrentRenderingObject: TDGLBaseSceneObject;

// ------------------------------------------------------------------------------
// ------------------------------------------------------------------------------
// ------------------------------------------------------------------------------
implementation

// ------------------------------------------------------------------------------
// ------------------------------------------------------------------------------
// ------------------------------------------------------------------------------

var
  vCounterFrequency: Int64;
  vInfoForm: TInvokeInfoForm = nil;
  vGLBaseSceneObjectNameChangeEvent: TNotifyEvent;
  vGLBehaviourNameChangeEvent:       TNotifyEvent;


threadvar vCurrentRenderingObject: TDGLBaseSceneObject;


// ------------------
{ Helpers Functions }
{$IFDEF GLS_REGION}{$REGION 'Helpers Functions'}{$ENDIF}

function GetCurrentRenderingObject: TDGLBaseSceneObject;
begin
  Result := vCurrentRenderingObject;
end;

procedure AxesBuildList(var rci: TRenderContextInfo; pattern: Word; AxisLen: Single);
begin
  {$IFDEF GLS_OPENGL_DEBUG}
  if GL.GREMEDY_string_marker then
    GL.StringMarkerGREMEDY(13, 'AxesBuildList');
  {$ENDIF}
  with rci.GLStates do
  begin
    // Disable(stLighting);
    if not rci.ignoreBlendingRequests then
    begin
      Enable(stBlend);
      SetBlendFunc(bfSrcAlpha, bfOneMinusSrcAlpha);
    end;
    LineWidth := 1;
    // Enable(stLineStipple);
    // LineStippleFactor := 1;
    // LineStipplePattern := Pattern;
    DepthWriteMask := True;
    DepthFunc      := cfLEqual;
    if rci.bufferDepthTest then
      Enable(stDepthTest);
  end;
  // glBegin(GL_LINES);
  // glColor3f(0.5, 0.0, 0.0);
  // glVertex3f(0, 0, 0);
  // glVertex3f(-AxisLen, 0, 0);
  // glColor3f(1.0, 0.0, 0.0);
  // glVertex3f(0, 0, 0);
  // glVertex3f(AxisLen, 0, 0);
  // glColor3f(0.0, 0.5, 0.0);
  // glVertex3f(0, 0, 0);
  // glVertex3f(0, -AxisLen, 0);
  // glColor3f(0.0, 1.0, 0.0);
  // glVertex3f(0, 0, 0);
  // glVertex3f(0, AxisLen, 0);
  // glColor3f(0.0, 0.0, 0.5);
  // glVertex3f(0, 0, 0);
  // glVertex3f(0, 0, -AxisLen);
  // glColor3f(0.0, 0.0, 1.0);
  // glVertex3f(0, 0, 0);
  // glVertex3f(0, 0, AxisLen);
  // glEnd;
end;


procedure RegisterInfoForm(infoForm: TInvokeInfoForm);
begin
  vInfoForm := infoForm;
end;

procedure InvokeInfoForm(aSceneBuffer: TDGLSceneBuffer; Modal: Boolean);
begin
  if Assigned(vInfoForm) then
    vInfoForm(aSceneBuffer, Modal)
  else
    InformationDlg('InfoForm not available.');
end;

procedure RegisterGLBaseSceneObjectNameChangeEvent(notifyEvent: TNotifyEvent);
begin
  vGLBaseSceneObjectNameChangeEvent := notifyEvent;
end;

procedure DeRegisterGLBaseSceneObjectNameChangeEvent(notifyEvent: TNotifyEvent);
begin
  vGLBaseSceneObjectNameChangeEvent := nil;
end;

procedure RegisterGLBehaviourNameChangeEvent(notifyEvent: TNotifyEvent);
begin
  vGLBehaviourNameChangeEvent := notifyEvent;
end;

procedure DeRegisterGLBehaviourNameChangeEvent(notifyEvent: TNotifyEvent);
begin
  vGLBehaviourNameChangeEvent := nil;
end;

{$IFDEF GLS_REGIONS}{$ENDREGION}{$ENDIF}

// ------------------
{ TDGLBaseSceneObject }
{$IFDEF GLS_REGIONS}{$REGION 'TDGLBaseSceneObject'}{$ENDIF}

constructor TDGLBaseSceneObject.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FObjectStyle := [];
  FChanges     := [ocTransformation, ocStructure, ocAbsoluteMatrix, ocInvAbsoluteMatrix];
  FPosition    := TDGLCoordinates.CreateInitialized(Self, NullHmgPoint, csPoint);
  FRotation    := TDGLCoordinates.CreateInitialized(Self, NullHmgVector, csVector);
  FDirection   := TDGLCoordinates.CreateInitialized(Self, ZHmgVector, csVector);
  FUp          := TDGLCoordinates.CreateInitialized(Self, YHmgVector, csVector);
  FScaling     := TDGLCoordinates.CreateInitialized(Self, XYZHmgVector, csVector);
  GetMem(FLocalMatrix, SizeOf(TMatrix));
  FLocalMatrix^      := IdentityHmgMatrix;
  FVisible           := True;
  FPickable          := True;
  FObjectsSorting    := osInherited;
  FVisibilityCulling := vcInherited;

  FBBChanges                    := [oBBcChild, oBBcStructure];
  FBoundingBoxPersonalUnscaled  := NullBoundingBox;
  FBoundingBoxOfChildren        := NullBoundingBox;
  FBoundingBoxIncludingChildren := NullBoundingBox;
end;

constructor TDGLBaseSceneObject.CreateAsChild(aParentOwner: TDGLBaseSceneObject);
begin
  Create(aParentOwner);
  aParentOwner.AddChild(Self);
end;

destructor TDGLBaseSceneObject.Destroy;
begin
  DeleteChildCameras;
  if assigned(FLocalMatrix) then
    FreeMem(FLocalMatrix, SizeOf(TMatrix));
  if assigned(FAbsoluteMatrix) then
    // This bug have coming surely from a bad commit file.
    FreeMem(FAbsoluteMatrix, SizeOf(TMatrix) * 2);
  // k00m memory fix and remove some leak of the old version.
  FGLObjectEffects.Free;
  FGLBehaviours.Free;
  //FListHandle.Free;
  FPosition.Free;
  FRotation.Free;
  FDirection.Free;
  FUp.Free;
  FScaling.Free;
  if assigned(FParent) then
    FParent.Remove(Self, False);
  if assigned(FChildren) then
  begin
    DeleteChildren;
    FChildren.Free;
  end;
  inherited Destroy;
end;

function TDGLBaseSceneObject.GetHandle(var rci: TRenderContextInfo): Cardinal;
begin
//  if not assigned(FListHandle) then
//    FListHandle := TDGLListHandle.Create;
//  Result        := FListHandle.Handle;
//  if Result = 0 then
//    Result := FListHandle.AllocateHandle;
//
//  if ocStructure in FChanges then
//  begin
//    ClearStructureChanged;
//    FListHandle.NotifyChangesOfData;
//  end;
//
//  if FListHandle.IsDataNeedUpdate then
//  begin
//    rci.GLStates.NewList(Result, GL_COMPILE);
//    try
//      BuildList(rci);
//    finally
//      rci.GLStates.EndList;
//    end;
//    FListHandle.NotifyDataUpdated;
//  end;
result:=0;
end;

function TDGLBaseSceneObject.ListHandleAllocated: Boolean;
begin
  Result := false;//assigned(FListHandle) and (FListHandle.Handle <> 0) and not(ocStructure in FChanges);
end;

procedure TDGLBaseSceneObject.DestroyHandle;
begin
//  if assigned(FListHandle) then
//    FListHandle.DestroyHandle;
end;

procedure TDGLBaseSceneObject.DestroyHandles;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    Children[i].DestroyHandles;
  DestroyHandle;
end;

procedure TDGLBaseSceneObject.SetBBChanges(const Value: TObjectBBChanges);
begin
  if Value <> FBBChanges then
  begin
    FBBChanges := Value;
    if assigned(FParent) then
      FParent.BBChanges := FParent.BBChanges + [oBBcChild];
  end;
end;

function TDGLBaseSceneObject.Blended: Boolean;
begin
  Result := False;
end;

procedure TDGLBaseSceneObject.BeginUpdate;
begin
  Inc(FUpdateCount);
end;

procedure TDGLBaseSceneObject.EndUpdate;
begin
  if FUpdateCount > 0 then
  begin
    Dec(FUpdateCount);
    if FUpdateCount = 0 then
      NotifyChange(Self);
  end
  else
    Assert(False, glsUnBalancedBeginEndUpdate);
end;

procedure TDGLBaseSceneObject.BuildList(var rci: TRenderContextInfo);
begin
  // nothing
end;

procedure TDGLBaseSceneObject.DeleteChildCameras;
var
  i:     Integer;
  child: TDGLBaseSceneObject;
begin
  i := 0;
  if assigned(FChildren) then
    while i < FChildren.Count do
    begin
      child := TDGLBaseSceneObject(FChildren.List^[i]);
      child.DeleteChildCameras;
      if child is TDGLCamera then
      begin
        Remove(child, True);
        child.Free;
      end
      else
        Inc(i);
    end;
end;

procedure TDGLBaseSceneObject.DeleteChildren;
var
  child: TDGLBaseSceneObject;
begin
  DeleteChildCameras;
  if assigned(FScene) then
  //  FScene.RemoveLights(Self);
  if assigned(FChildren) then
    while FChildren.Count > 0 do
    begin
      child         := TDGLBaseSceneObject(FChildren.Pop);
      child.FParent := nil;
      child.Free;
    end;
  BBChanges := BBChanges + [oBBcChild];
end;

procedure TDGLBaseSceneObject.Loaded;
begin
  inherited;
  FPosition.W := 1;
  if assigned(FGLBehaviours) then
    FGLBehaviours.Loaded;
  if assigned(FGLObjectEffects) then
    FGLObjectEffects.Loaded;
end;

procedure TDGLBaseSceneObject.DefineProperties(Filer: TFiler);
begin
  inherited;
  { FOriginalFiler := Filer; }

  Filer.DefineBinaryProperty('BehavioursData', ReadBehaviours, WriteBehaviours, (assigned(FGLBehaviours) and (FGLBehaviours.Count > 0)));
  Filer.DefineBinaryProperty('EffectsData', ReadEffects, WriteEffects, (assigned(FGLObjectEffects) and (FGLObjectEffects.Count > 0)));
  { FOriginalFiler:=nil; }
end;

procedure TDGLBaseSceneObject.WriteBehaviours(stream: TStream);
var
  writer: TWriter;
begin
  writer := TWriter.Create(stream, 16384);
  try
    Behaviours.WriteToFiler(writer);
  finally
    writer.Free;
  end;
end;

procedure TDGLBaseSceneObject.ReadBehaviours(stream: TStream);
var
  reader: TReader;
begin
  reader := TReader.Create(stream, 16384);
  { with TReader(FOriginalFiler) do }
  try
    { reader.Root                 := Root;
      reader.OnError              := OnError;
      reader.OnFindMethod         := OnFindMethod;
      reader.OnSetName            := OnSetName;
      reader.OnReferenceName      := OnReferenceName;
      reader.OnAncestorNotFound   := OnAncestorNotFound;
      reader.OnCreateComponent    := OnCreateComponent;
      reader.OnFindComponentClass := OnFindComponentClass; }
    Behaviours.ReadFromFiler(reader);
  finally
    reader.Free;
  end;
end;

procedure TDGLBaseSceneObject.WriteEffects(stream: TStream);
var
  writer: TWriter;
begin
  writer := TWriter.Create(stream, 16384);
  try
    Effects.WriteToFiler(writer);
  finally
    writer.Free;
  end;
end;

procedure TDGLBaseSceneObject.ReadEffects(stream: TStream);
var
  reader: TReader;
begin
  reader := TReader.Create(stream, 16384);
  { with TReader(FOriginalFiler) do }
  try
    { reader.Root                 := Root;
      reader.OnError              := OnError;
      reader.OnFindMethod         := OnFindMethod;
      reader.OnSetName            := OnSetName;
      reader.OnReferenceName      := OnReferenceName;
      reader.OnAncestorNotFound   := OnAncestorNotFound;
      reader.OnCreateComponent    := OnCreateComponent;
      reader.OnFindComponentClass := OnFindComponentClass; }
    Effects.ReadFromFiler(reader);
  finally
    reader.Free;
  end;
end;

procedure TDGLBaseSceneObject.WriteRotations(stream: TStream);
begin
  stream.Write(FRotation.AsAddress^, 3 * SizeOf(TGLFloat));
end;

procedure TDGLBaseSceneObject.ReadRotations(stream: TStream);
begin
  stream.Read(FRotation.AsAddress^, 3 * SizeOf(TGLFloat));
end;

procedure TDGLBaseSceneObject.DrawAxes(var rci: TRenderContextInfo; pattern: Word);
begin
  AxesBuildList(rci, pattern, rci.rcci.farClippingDistance - rci.rcci.nearClippingDistance);
end;

procedure TDGLBaseSceneObject.GetChildren(AProc: TGetChildProc; Root: TComponent);
var
  i: Integer;
begin
  if assigned(FChildren) then
    for i := 0 to FChildren.Count - 1 do
      if not IsSubComponent(TComponent(FChildren.List^[i])) then
        AProc(TComponent(FChildren.List^[i]));
end;

function TDGLBaseSceneObject.Get(Index: Integer): TDGLBaseSceneObject;
begin
  if assigned(FChildren) then
    Result := TDGLBaseSceneObject(FChildren[Index])
  else
    Result := nil;
end;

function TDGLBaseSceneObject.GetCount: Integer;
begin
  if assigned(FChildren) then
    Result := FChildren.Count
  else
    Result := 0;
end;

function TDGLBaseSceneObject.HasSubChildren: Boolean;
var
  i: Integer;
begin
  Result := False;
  if Count <> 0 then
    for i := 0 to Count - 1 do
      if IsSubComponent(Children[i]) then
      begin
        Result := True;
        Exit;
      end;
end;

procedure TDGLBaseSceneObject.AddChild(AChild: TDGLBaseSceneObject);
begin
  if assigned(FScene) then
    FScene.AddLights(AChild);
  if not assigned(FChildren) then
    FChildren := TDGLPersistentObjectList.Create;
  FChildren.Add(AChild);
  AChild.FParent := Self;
  AChild.SetScene(FScene);
  TransformationChanged;
  AChild.TransformationChanged;
  AChild.DoOnAddedToParent;
  BBChanges := BBChanges + [oBBcChild];
end;

function TDGLBaseSceneObject.AddNewChild(AChild: TDGLSceneObjectClass): TDGLBaseSceneObject;
begin
  Result := AChild.Create(Owner);
  AddChild(Result);
end;

function TDGLBaseSceneObject.AddNewChildFirst(AChild: TDGLSceneObjectClass): TDGLBaseSceneObject;
begin
  Result := AChild.Create(Owner);
  Insert(0, Result);
end;

function TDGLBaseSceneObject.GetOrCreateBehaviour(aBehaviour: TDGLBehaviourClass): TDGLBehaviour;
begin
  Result := TDGLBehaviour(Behaviours.GetOrCreate(aBehaviour));
end;

function TDGLBaseSceneObject.AddNewBehaviour(aBehaviour: TDGLBehaviourClass): TDGLBehaviour;
begin
  Assert(Behaviours.CanAdd(aBehaviour));
  Result := aBehaviour.Create(Behaviours)
end;

function TDGLBaseSceneObject.GetOrCreateEffect(anEffect: TDGLObjectEffectClass): TDGLObjectEffect;
begin
  Result := TDGLObjectEffect(Effects.GetOrCreate(anEffect));
end;

function TDGLBaseSceneObject.AddNewEffect(anEffect: TDGLObjectEffectClass): TDGLObjectEffect;
begin
  Assert(Effects.CanAdd(anEffect));
  Result := anEffect.Create(Effects)
end;

procedure TDGLBaseSceneObject.RebuildMatrix;
begin
  if ocTransformation in Changes then
  begin
    VectorScale(LeftVector, Scale.X, FLocalMatrix^.v[0]);
    VectorScale(FUp.AsVector, Scale.Y, FLocalMatrix^.v[1]);
    VectorScale(FDirection.AsVector, Scale.Z, FLocalMatrix^.v[2]);
    SetVector(FLocalMatrix^.v[3], FPosition.AsVector);
    Exclude(FChanges, ocTransformation);
    Include(FChanges, ocAbsoluteMatrix);
    Include(FChanges, ocInvAbsoluteMatrix);
  end;
end;

procedure TDGLBaseSceneObject.ForceLocalMatrix(const aMatrix: TMatrix);
begin
  FLocalMatrix^ := aMatrix;
  Exclude(FChanges, ocTransformation);
  Include(FChanges, ocAbsoluteMatrix);
  Include(FChanges, ocInvAbsoluteMatrix);
end;

function TDGLBaseSceneObject.AbsoluteMatrixAsAddress: PMatrix;
begin
  if ocAbsoluteMatrix in FChanges then
  begin
    RebuildMatrix;
    if not assigned(FAbsoluteMatrix) then
    begin
      GetMem(FAbsoluteMatrix, SizeOf(TMatrix) * 2);
      FInvAbsoluteMatrix := PMatrix(PtrUInt(FAbsoluteMatrix) + SizeOf(TMatrix));
    end;
    if assigned(Parent) and (not(Parent is TDGLSceneRootObject)) then
    begin
      MatrixMultiply(FLocalMatrix^, TDGLBaseSceneObject(Parent).AbsoluteMatrixAsAddress^, FAbsoluteMatrix^);
    end
    else
      FAbsoluteMatrix^ := FLocalMatrix^;
    Exclude(FChanges, ocAbsoluteMatrix);
    Include(FChanges, ocInvAbsoluteMatrix);
  end;
  Result := FAbsoluteMatrix;
end;

function TDGLBaseSceneObject.InvAbsoluteMatrix: TMatrix;
begin
  Result := InvAbsoluteMatrixAsAddress^;
end;

function TDGLBaseSceneObject.InvAbsoluteMatrixAsAddress: PMatrix;
begin
  if ocInvAbsoluteMatrix in FChanges then
  begin
    if VectorEquals(Scale.DirectVector, XYZHmgVector) then
    begin
      if not assigned(FAbsoluteMatrix) then
      begin
        GetMem(FAbsoluteMatrix, SizeOf(TMatrix) * 2);
        FInvAbsoluteMatrix := PMatrix(PtrUInt(FAbsoluteMatrix) + SizeOf(TMatrix));
      end;
      RebuildMatrix;
      if Parent <> nil then
        FInvAbsoluteMatrix^ := MatrixMultiply(Parent.InvAbsoluteMatrixAsAddress^, AnglePreservingMatrixInvert(FLocalMatrix^))
      else
        FInvAbsoluteMatrix^ := AnglePreservingMatrixInvert(FLocalMatrix^);
    end
    else
    begin
      FInvAbsoluteMatrix^ := AbsoluteMatrixAsAddress^;
      InvertMatrix(FInvAbsoluteMatrix^);
    end;
    Exclude(FChanges, ocInvAbsoluteMatrix);
  end;
  Result := FInvAbsoluteMatrix;
end;

function TDGLBaseSceneObject.GetAbsoluteMatrix: TMatrix;
begin
  Result := AbsoluteMatrixAsAddress^;
end;

procedure TDGLBaseSceneObject.SetAbsoluteMatrix(const Value: TMatrix);
begin
  if not MatrixEquals(Value, FAbsoluteMatrix^) then
  begin
    FAbsoluteMatrix^ := Value;
    if Parent <> nil then
      SetMatrix(MatrixMultiply(FAbsoluteMatrix^, Parent.InvAbsoluteMatrixAsAddress^))
    else
      SetMatrix(Value);
  end;
end;

function TDGLBaseSceneObject.GetAbsoluteDirection: TVector;
begin
  Result := VectorNormalize(AbsoluteMatrixAsAddress^.v[2]);
end;

procedure TDGLBaseSceneObject.SetAbsoluteDirection(const v: TVector);
begin
  if Parent <> nil then
    Direction.AsVector := Parent.AbsoluteToLocal(v)
  else
    Direction.AsVector := v;
end;

function TDGLBaseSceneObject.GetAbsoluteScale: TVector;
begin
  Result.v[0] := AbsoluteMatrixAsAddress^.v[0].v[0];
  Result.v[1] := AbsoluteMatrixAsAddress^.v[1].v[1];
  Result.v[2] := AbsoluteMatrixAsAddress^.v[2].v[2];

  Result.v[3] := 0;
end;

procedure TDGLBaseSceneObject.SetAbsoluteScale(const Value: TVector);
begin
  if Parent <> nil then
    Scale.AsVector := Parent.AbsoluteToLocal(Value)
  else
    Scale.AsVector := Value;
end;

function TDGLBaseSceneObject.GetAbsoluteUp: TVector;
begin
  Result := VectorNormalize(AbsoluteMatrixAsAddress^.v[1]);
end;

procedure TDGLBaseSceneObject.SetAbsoluteUp(const v: TVector);
begin
  if Parent <> nil then
    Up.AsVector := Parent.AbsoluteToLocal(v)
  else
    Up.AsVector := v;
end;

function TDGLBaseSceneObject.AbsoluteRight: TVector;
begin
  Result := VectorNormalize(AbsoluteMatrixAsAddress^.v[0]);
end;

function TDGLBaseSceneObject.AbsoluteLeft: TVector;
begin
  Result := VectorNegate(AbsoluteRight);
end;

function TDGLBaseSceneObject.GetAbsolutePosition: TVector;
begin
  Result := AbsoluteMatrixAsAddress^.v[3];
end;

procedure TDGLBaseSceneObject.SetAbsolutePosition(const v: TVector);
begin
  if assigned(Parent) then
    Position.AsVector := Parent.AbsoluteToLocal(v)
  else
    Position.AsVector := v;
end;

function TDGLBaseSceneObject.AbsolutePositionAsAddress: PVector;
begin
  Result := @AbsoluteMatrixAsAddress^.v[3];
end;

function TDGLBaseSceneObject.AbsoluteXVector: TVector;
begin
  AbsoluteMatrixAsAddress;
  SetVector(Result, PAffineVector(@FAbsoluteMatrix.v[0])^);
end;

function TDGLBaseSceneObject.AbsoluteYVector: TVector;
begin
  AbsoluteMatrixAsAddress;
  SetVector(Result, PAffineVector(@FAbsoluteMatrix.v[1])^);
end;

function TDGLBaseSceneObject.AbsoluteZVector: TVector;
begin
  AbsoluteMatrixAsAddress;
  SetVector(Result, PAffineVector(@FAbsoluteMatrix.v[2])^);
end;

function TDGLBaseSceneObject.AbsoluteToLocal(const v: TVector): TVector;
begin
  Result := VectorTransform(v, InvAbsoluteMatrixAsAddress^);
end;

function TDGLBaseSceneObject.AbsoluteToLocal(const v: TAffineVector): TAffineVector;
begin
  Result := VectorTransform(v, InvAbsoluteMatrixAsAddress^);
end;

function TDGLBaseSceneObject.LocalToAbsolute(const v: TVector): TVector;
begin
  Result := VectorTransform(v, AbsoluteMatrixAsAddress^);
end;

function TDGLBaseSceneObject.LocalToAbsolute(const v: TAffineVector): TAffineVector;
begin
  Result := VectorTransform(v, AbsoluteMatrixAsAddress^);
end;

function TDGLBaseSceneObject.Right: TVector;
begin
  Result := VectorCrossProduct(FDirection.AsVector, FUp.AsVector);
end;

function TDGLBaseSceneObject.LeftVector: TVector;
begin
  Result := VectorCrossProduct(FUp.AsVector, FDirection.AsVector);
end;

function TDGLBaseSceneObject.BarycenterAbsolutePosition: TVector;
begin
  Result := AbsolutePosition;
end;

function TDGLBaseSceneObject.SqrDistanceTo(anObject: TDGLBaseSceneObject): Single;
begin
  if assigned(anObject) then
    Result := VectorDistance2(AbsolutePosition, anObject.AbsolutePosition)
  else
    Result := 0;
end;

function TDGLBaseSceneObject.SqrDistanceTo(const pt: TVector): Single;
begin
  Result := VectorDistance2(pt, AbsolutePosition);
end;

function TDGLBaseSceneObject.DistanceTo(anObject: TDGLBaseSceneObject): Single;
begin
  if assigned(anObject) then
    Result := VectorDistance(AbsolutePosition, anObject.AbsolutePosition)
  else
    Result := 0;
end;

function TDGLBaseSceneObject.DistanceTo(const pt: TVector): Single;
begin
  Result := VectorDistance(AbsolutePosition, pt);
end;

function TDGLBaseSceneObject.BarycenterSqrDistanceTo(const pt: TVector): Single;
var
  d: TVector;
begin
  d      := BarycenterAbsolutePosition;
  Result := VectorDistance2(d, pt);
end;

function TDGLBaseSceneObject.AxisAlignedDimensions: TVector;
begin
  Result := AxisAlignedDimensionsUnscaled();
  ScaleVector(Result, Scale.AsVector);
end;

function TDGLBaseSceneObject.AxisAlignedDimensionsUnscaled: TVector;
begin
  Result.v[0] := 0.5;
  Result.v[1] := 0.5;
  Result.v[2] := 0.5;
  Result.v[3] := 0;
end;

function TDGLBaseSceneObject.AxisAlignedBoundingBox(const AIncludeChilden: Boolean): TAABB;
var
  i:     Integer;
  aabb:  TAABB;
  child: TDGLBaseSceneObject;
begin
  SetAABB(Result, AxisAlignedDimensionsUnscaled);
  // not tested for child objects
  if AIncludeChilden and assigned(FChildren) then
  begin
    for i := 0 to FChildren.Count - 1 do
    begin
      child := TDGLBaseSceneObject(FChildren.List^[i]);
      aabb  := child.AxisAlignedBoundingBoxUnscaled(AIncludeChilden);
      AABBTransform(aabb, child.Matrix);
      AddAABB(Result, aabb);
    end;
  end;
  AABBScale(Result, Scale.AsAffineVector);
end;

function TDGLBaseSceneObject.AxisAlignedBoundingBoxUnscaled(const AIncludeChilden: Boolean): TAABB;
var
  i:    Integer;
  aabb: TAABB;
begin
  SetAABB(Result, AxisAlignedDimensionsUnscaled);
  // not tested for child objects
  if AIncludeChilden and assigned(FChildren) then
  begin
    for i := 0 to FChildren.Count - 1 do
    begin
      aabb := TDGLBaseSceneObject(FChildren.List^[i]).AxisAlignedBoundingBoxUnscaled(AIncludeChilden);
      AABBTransform(aabb, TDGLBaseSceneObject(FChildren.List^[i]).Matrix);
      AddAABB(Result, aabb);
    end;
  end;
end;

function TDGLBaseSceneObject.AxisAlignedBoundingBoxAbsolute(const AIncludeChilden: Boolean; const AUseBaryCenter: Boolean): TAABB;
begin
  Result := BBToAABB(BoundingBoxAbsolute(AIncludeChilden, AUseBaryCenter));
end;

function TDGLBaseSceneObject.BoundingBox(const AIncludeChilden: Boolean; const AUseBaryCenter: Boolean): THmgBoundingBox;
var
  CurrentBaryOffset: TVector;
begin
  Result := AABBToBB(AxisAlignedBoundingBox(AIncludeChilden));

  if AUseBaryCenter then
  begin
    CurrentBaryOffset := VectorSubtract(AbsoluteToLocal(BarycenterAbsolutePosition), Position.AsVector);
    OffsetBBPoint(Result, CurrentBaryOffset);
  end;
end;

function TDGLBaseSceneObject.BoundingBoxUnscaled(const AIncludeChilden: Boolean; const AUseBaryCenter: Boolean): THmgBoundingBox;
var
  CurrentBaryOffset: TVector;
begin
  Result := AABBToBB(AxisAlignedBoundingBoxUnscaled(AIncludeChilden));

  if AUseBaryCenter then
  begin
    CurrentBaryOffset := VectorSubtract(AbsoluteToLocal(BarycenterAbsolutePosition), Position.AsVector);
    OffsetBBPoint(Result, CurrentBaryOffset);
  end;
end;

function TDGLBaseSceneObject.BoundingBoxAbsolute(const AIncludeChilden: Boolean; const AUseBaryCenter: Boolean): THmgBoundingBox;
var
  i:                 Integer;
  CurrentBaryOffset: TVector;
begin
  Result           := BoundingBoxUnscaled(AIncludeChilden, False);
  for i            := 0 to 7 do
    Result.BBox[i] := LocalToAbsolute(Result.BBox[i]);

  if AUseBaryCenter then
  begin
    CurrentBaryOffset := VectorSubtract(BarycenterAbsolutePosition, AbsolutePosition);
    OffsetBBPoint(Result, CurrentBaryOffset);
  end;
end;

function TDGLBaseSceneObject.BoundingSphereRadius: Single;
begin
  Result := VectorLength(AxisAlignedDimensions);
end;

function TDGLBaseSceneObject.BoundingSphereRadiusUnscaled: Single;
begin
  Result := VectorLength(AxisAlignedDimensionsUnscaled);
end;

function TDGLBaseSceneObject.PointInObject(const point: TVector): Boolean;
var
  localPt, dim: TVector;
begin
  dim     := AxisAlignedDimensions;
  localPt := VectorTransform(point, InvAbsoluteMatrix);
  Result  := (Abs(localPt.v[0] * Scale.X) <= dim.v[0]) and (Abs(localPt.v[1] * Scale.Y) <= dim.v[1]) and (Abs(localPt.v[2] * Scale.Z) <= dim.v[2]);
end;

procedure TDGLBaseSceneObject.CalculateBoundingBoxPersonalUnscaled(var ANewBoundingBox: THmgBoundingBox);
begin
  // Using the standard method to get the local BB.
  ANewBoundingBox := AABBToBB(AxisAlignedBoundingBoxUnscaled(False));
  OffsetBBPoint(ANewBoundingBox, AbsoluteToLocal(BarycenterAbsolutePosition));
end;

function TDGLBaseSceneObject.BoundingBoxPersonalUnscaledEx: THmgBoundingBox;
begin
  if oBBcStructure in FBBChanges then
  begin
    CalculateBoundingBoxPersonalUnscaled(FBoundingBoxPersonalUnscaled);
    Exclude(FBBChanges, oBBcStructure);
  end;
  Result := FBoundingBoxPersonalUnscaled;
end;

function TDGLBaseSceneObject.AxisAlignedBoundingBoxAbsoluteEx: TAABB;
var
  pBB: THmgBoundingBox;
begin
  pBB := BoundingBoxIncludingChildrenEx;
  BBTransform(pBB, AbsoluteMatrix);
  Result := BBToAABB(pBB);
end;

function TDGLBaseSceneObject.AxisAlignedBoundingBoxEx: TAABB;
begin
  Result := BBToAABB(BoundingBoxIncludingChildrenEx);
  AABBScale(Result, Scale.AsAffineVector);
end;

function TDGLBaseSceneObject.BoundingBoxOfChildrenEx: THmgBoundingBox;
var
  i:   Integer;
  pBB: THmgBoundingBox;
begin
  if oBBcChild in FBBChanges then
  begin
    // Computing
    FBoundingBoxOfChildren := NullBoundingBox;
    if assigned(FChildren) then
    begin
      for i := 0 to FChildren.Count - 1 do
      begin
        pBB := TDGLBaseSceneObject(FChildren.List^[i]).BoundingBoxIncludingChildrenEx;
        if not BoundingBoxesAreEqual(@pBB, @NullBoundingBox) then
        begin
          // transformation with local matrix
          BBTransform(pBB, TDGLBaseSceneObject(FChildren.List^[i]).Matrix);
          if BoundingBoxesAreEqual(@FBoundingBoxOfChildren, @NullBoundingBox) then
            FBoundingBoxOfChildren := pBB
          else
            AddBB(FBoundingBoxOfChildren, pBB);
        end;
      end;
    end;
    Exclude(FBBChanges, oBBcChild);
  end;
  Result := FBoundingBoxOfChildren;
end;

function TDGLBaseSceneObject.BoundingBoxIncludingChildrenEx: THmgBoundingBox;
var
  pBB: THmgBoundingBox;
begin
  if (oBBcStructure in FBBChanges) or (oBBcChild in FBBChanges) then
  begin
    pBB := BoundingBoxPersonalUnscaledEx;
    if BoundingBoxesAreEqual(@pBB, @NullBoundingBox) then
      FBoundingBoxIncludingChildren := BoundingBoxOfChildrenEx
    else
    begin
      FBoundingBoxIncludingChildren := pBB;
      pBB                           := BoundingBoxOfChildrenEx;
      if not BoundingBoxesAreEqual(@pBB, @NullBoundingBox) then
        AddBB(FBoundingBoxIncludingChildren, pBB);
    end;
  end;
  Result := FBoundingBoxIncludingChildren;
end;

function TDGLBaseSceneObject.RayCastIntersect(const rayStart, rayVector: TVector; intersectPoint: PVector = nil; intersectNormal: PVector = nil): Boolean;
var
  i1, i2, absPos: TVector;
begin
  SetVector(absPos, AbsolutePosition);
  if RayCastSphereIntersect(rayStart, rayVector, absPos, BoundingSphereRadius, i1, i2) > 0 then
  begin
    Result := True;
    if assigned(intersectPoint) then
      SetVector(intersectPoint^, i1);
    if assigned(intersectNormal) then
    begin
      SubtractVector(i1, absPos);
      NormalizeVector(i1);
      SetVector(intersectNormal^, i1);
    end;
  end
  else
    Result := False;
end;

function TDGLBaseSceneObject.GenerateSilhouette(const silhouetteParameters: TDGLSilhouetteParameters): TDGLSilhouette;
const
  cNbSegments = 21;
var
  i, j:                        Integer;
  d, r, vr, s, c, angleFactor: Single;
  sVec, tVec:                  TAffineVector;
begin
  r := BoundingSphereRadiusUnscaled;
  d := VectorLength(silhouetteParameters.SeenFrom);
  // determine visible radius
  case silhouetteParameters.Style of
    ssOmni:
      vr := SphereVisibleRadius(d, r);
    ssParallel:
      vr := r;
  else
    Assert(False);
    vr := r;
  end;
  // determine a local orthonormal matrix, viewer-oriented
  sVec := VectorCrossProduct(silhouetteParameters.SeenFrom, XVector);
  if VectorLength(sVec) < 1E-3 then
    sVec := VectorCrossProduct(silhouetteParameters.SeenFrom, YVector);
  tVec   := VectorCrossProduct(silhouetteParameters.SeenFrom, sVec);
  NormalizeVector(sVec);
  NormalizeVector(tVec);
  // generate the silhouette (outline and capping)
  Result      := TDGLSilhouette.Create;
  angleFactor := (2 * PI) / cNbSegments;
  vr          := vr * 0.98;
  for i       := 0 to cNbSegments - 1 do
  begin
    SinCosine(i * angleFactor, vr, s, c);
    Result.Vertices.AddPoint(VectorCombine(sVec, tVec, s, c));
    j := (i + 1) mod cNbSegments;
    Result.Indices.Add(i, j);
    if silhouetteParameters.CappingRequired then
      Result.CapIndices.Add(cNbSegments, i, j)
  end;
  if silhouetteParameters.CappingRequired then
    Result.Vertices.Add(NullHmgPoint);
end;

procedure TDGLBaseSceneObject.Assign(Source: TPersistent);
var
  i:               Integer;
  child, newChild: TDGLBaseSceneObject;
begin
  if assigned(Source) and (Source is TDGLBaseSceneObject) then
  begin
    DestroyHandles;
    FVisible := TDGLBaseSceneObject(Source).FVisible;
    TDGLBaseSceneObject(Source).RebuildMatrix;
    SetMatrix(TDGLBaseSceneObject(Source).FLocalMatrix^);
    FShowAxes          := TDGLBaseSceneObject(Source).FShowAxes;
    FObjectsSorting    := TDGLBaseSceneObject(Source).FObjectsSorting;
    FVisibilityCulling := TDGLBaseSceneObject(Source).FVisibilityCulling;
    FRotation.Assign(TDGLBaseSceneObject(Source).FRotation);
    DeleteChildren;
    if assigned(Scene) then
      Scene.BeginUpdate;
    if assigned(TDGLBaseSceneObject(Source).FChildren) then
    begin
      for i := 0 to TDGLBaseSceneObject(Source).FChildren.Count - 1 do
      begin
        child    := TDGLBaseSceneObject(TDGLBaseSceneObject(Source).FChildren[i]);
        newChild := AddNewChild(TDGLSceneObjectClass(child.ClassType));
        newChild.Assign(child);
      end;
    end;
    if assigned(Scene) then
      Scene.EndUpdate;
    OnProgress := TDGLBaseSceneObject(Source).OnProgress;
    if assigned(TDGLBaseSceneObject(Source).FGLBehaviours) then
      Behaviours.Assign(TDGLBaseSceneObject(Source).Behaviours)
    else
      FreeAndNil(FGLBehaviours);
    if assigned(TDGLBaseSceneObject(Source).FGLObjectEffects) then
      Effects.Assign(TDGLBaseSceneObject(Source).Effects)
    else
      FreeAndNil(FGLObjectEffects);
    Tag       := TDGLBaseSceneObject(Source).Tag;
    FTagFloat := TDGLBaseSceneObject(Source).FTagFloat;
  end
  else
    inherited Assign(Source);
end;

function TDGLBaseSceneObject.IsUpdating: Boolean;
begin
  Result := (FUpdateCount <> 0) or (csReading in ComponentState);
end;

function TDGLBaseSceneObject.GetParentComponent: TComponent;
begin
  if FParent is TDGLSceneRootObject then
    Result := FScene
  else
    Result := FParent;
end;

function TDGLBaseSceneObject.HasParent: Boolean;
begin
  Result := assigned(FParent);
end;

procedure TDGLBaseSceneObject.Lift(ADistance: Single);
begin
  FPosition.AddScaledVector(ADistance, FUp.AsVector);
  TransformationChanged;
end;

procedure TDGLBaseSceneObject.Move(ADistance: Single);
begin
  FPosition.AddScaledVector(ADistance, FDirection.AsVector);
  TransformationChanged;
end;

procedure TDGLBaseSceneObject.Slide(ADistance: Single);
begin
  FPosition.AddScaledVector(ADistance, Right);
  TransformationChanged;
end;

procedure TDGLBaseSceneObject.ResetRotations;
begin
  FillChar(FLocalMatrix^, SizeOf(TMatrix), 0);
  FLocalMatrix^.v[0].v[0] := Scale.DirectX;
  FLocalMatrix^.v[1].v[1] := Scale.DirectY;
  FLocalMatrix^.v[2].v[2] := Scale.DirectZ;
  SetVector(FLocalMatrix^.v[3], Position.DirectVector);
  FRotation.DirectVector  := NullHmgPoint;
  FDirection.DirectVector := ZHmgVector;
  FUp.DirectVector        := YHmgVector;
  TransformationChanged;
  Exclude(FChanges, ocTransformation);
end;

procedure TDGLBaseSceneObject.ResetAndPitchTurnRoll(const degX, degY, degZ: Single);
var
  rotMatrix: TMatrix;
  v:         TVector;
begin
  ResetRotations;
  // set DegX (Pitch)
  rotMatrix := CreateRotationMatrix(Right, degX * cPIdiv180);
  v         := VectorTransform(FUp.AsVector, rotMatrix);
  NormalizeVector(v);
  FUp.DirectVector := v;
  v                := VectorTransform(FDirection.AsVector, rotMatrix);
  NormalizeVector(v);
  FDirection.DirectVector := v;
  FRotation.DirectX       := NormalizeDegAngle(degX);
  // set DegY (Turn)
  rotMatrix := CreateRotationMatrix(FUp.AsVector, degY * cPIdiv180);
  v         := VectorTransform(FUp.AsVector, rotMatrix);
  NormalizeVector(v);
  FUp.DirectVector := v;
  v                := VectorTransform(FDirection.AsVector, rotMatrix);
  NormalizeVector(v);
  FDirection.DirectVector := v;
  FRotation.DirectY       := NormalizeDegAngle(degY);
  // set DegZ (Roll)
  rotMatrix := CreateRotationMatrix(Direction.AsVector, degZ * cPIdiv180);
  v         := VectorTransform(FUp.AsVector, rotMatrix);
  NormalizeVector(v);
  FUp.DirectVector := v;
  v                := VectorTransform(FDirection.AsVector, rotMatrix);
  NormalizeVector(v);
  FDirection.DirectVector := v;
  FRotation.DirectZ       := NormalizeDegAngle(degZ);
  TransformationChanged;
  NotifyChange(Self);
end;

procedure TDGLBaseSceneObject.RotateAbsolute(const rx, ry, rz: Single);
var
  resMat: TMatrix;
  v:      TAffineVector;
begin
  resMat := Matrix;
  // No we build rotation matrices and use them to rotate the obj
  if rx <> 0 then
  begin
    SetVector(v, AbsoluteToLocal(XVector));
    resMat := MatrixMultiply(CreateRotationMatrix(v, -DegToRadian(rx)), resMat);
  end;
  if ry <> 0 then
  begin
    SetVector(v, AbsoluteToLocal(YVector));
    resMat := MatrixMultiply(CreateRotationMatrix(v, -DegToRadian(ry)), resMat);
  end;
  if rz <> 0 then
  begin
    SetVector(v, AbsoluteToLocal(ZVector));
    resMat := MatrixMultiply(CreateRotationMatrix(v, -DegToRadian(rz)), resMat);
  end;
  Matrix := resMat;
end;

procedure TDGLBaseSceneObject.RotateAbsolute(const axis: TAffineVector; angle: Single);
var
  v: TAffineVector;
begin
  if angle <> 0 then
  begin
    SetVector(v, AbsoluteToLocal(axis));
    Matrix := MatrixMultiply(CreateRotationMatrix(v, DegToRadian(angle)), Matrix);
  end;
end;

procedure TDGLBaseSceneObject.Pitch(angle: Single);
var
  r:           Single;
  rightVector: TVector;
begin
  FIsCalculating := True;
  try
    angle       := -DegToRad(angle);
    rightVector := Right;
    FUp.Rotate(rightVector, angle);
    FUp.Normalize;
    FDirection.Rotate(rightVector, angle);
    FDirection.Normalize;
    r := -RadToDeg(ArcTan2(FDirection.Y, VectorLength(FDirection.X, FDirection.Z)));
    if FDirection.X < 0 then
      if FDirection.Y < 0 then
        r := 180 - r
      else
        r       := -180 - r;
    FRotation.X := r;
  finally
    FIsCalculating := False;
  end;
  TransformationChanged;
end;

procedure TDGLBaseSceneObject.SetPitchAngle(aValue: Single);
var
  diff:      Single;
  rotMatrix: TMatrix;
begin
  if aValue <> FRotation.X then
  begin
    if not(csLoading in ComponentState) then
    begin
      FIsCalculating := True;
      try
        diff             := DegToRadian(FRotation.X - aValue);
        rotMatrix        := CreateRotationMatrix(Right, diff);
        FUp.DirectVector := VectorTransform(FUp.AsVector, rotMatrix);
        FUp.Normalize;
        FDirection.DirectVector := VectorTransform(FDirection.AsVector, rotMatrix);
        FDirection.Normalize;
        TransformationChanged;
      finally
        FIsCalculating := False;
      end;
    end;
    FRotation.DirectX := NormalizeDegAngle(aValue);
  end;
end;

procedure TDGLBaseSceneObject.Roll(angle: Single);
var
  r:                            Single;
  rightVector, directionVector: TVector;
begin
  FIsCalculating := True;
  try
    angle           := DegToRadian(angle);
    directionVector := Direction.AsVector;
    FUp.Rotate(directionVector, angle);
    FUp.Normalize;
    FDirection.Rotate(directionVector, angle);
    FDirection.Normalize;

    // calculate new rotation angle from vectors
    rightVector := Right;
    r           := -RadToDeg(ArcTan2(rightVector.v[1], VectorLength(rightVector.v[0], rightVector.v[2])));
    if rightVector.v[0] < 0 then
      if rightVector.v[1] < 0 then
        r := 180 - r
      else
        r       := -180 - r;
    FRotation.Z := r;
  finally
    FIsCalculating := False;
  end;
  TransformationChanged;
end;

procedure TDGLBaseSceneObject.SetRollAngle(aValue: Single);
var
  diff:      Single;
  rotMatrix: TMatrix;
begin
  if aValue <> FRotation.Z then
  begin
    if not(csLoading in ComponentState) then
    begin
      FIsCalculating := True;
      try
        diff             := DegToRadian(FRotation.Z - aValue);
        rotMatrix        := CreateRotationMatrix(Direction.AsVector, diff);
        FUp.DirectVector := VectorTransform(FUp.AsVector, rotMatrix);
        FUp.Normalize;
        FDirection.DirectVector := VectorTransform(FDirection.AsVector, rotMatrix);
        FDirection.Normalize;
        TransformationChanged;
      finally
        FIsCalculating := False;
      end;
    end;
    FRotation.DirectZ := NormalizeDegAngle(aValue);
  end;
end;

procedure TDGLBaseSceneObject.Turn(angle: Single);
var
  r:        Single;
  upVector: TVector;
begin
  FIsCalculating := True;
  try
    angle    := DegToRadian(angle);
    upVector := Up.AsVector;
    FUp.Rotate(upVector, angle);
    FUp.Normalize;
    FDirection.Rotate(upVector, angle);
    FDirection.Normalize;
    r := -RadToDeg(ArcTan2(FDirection.X, VectorLength(FDirection.Y, FDirection.Z)));
    if FDirection.X < 0 then
      if FDirection.Y < 0 then
        r := 180 - r
      else
        r       := -180 - r;
    FRotation.Y := r;
  finally
    FIsCalculating := False;
  end;
  TransformationChanged;
end;

procedure TDGLBaseSceneObject.SetTurnAngle(aValue: Single);
var
  diff:      Single;
  rotMatrix: TMatrix;
begin
  if aValue <> FRotation.Y then
  begin
    if not(csLoading in ComponentState) then
    begin
      FIsCalculating := True;
      try
        diff             := DegToRadian(FRotation.Y - aValue);
        rotMatrix        := CreateRotationMatrix(Up.AsVector, diff);
        FUp.DirectVector := VectorTransform(FUp.AsVector, rotMatrix);
        FUp.Normalize;
        FDirection.DirectVector := VectorTransform(FDirection.AsVector, rotMatrix);
        FDirection.Normalize;
        TransformationChanged;
      finally
        FIsCalculating := False;
      end;
    end;
    FRotation.DirectY := NormalizeDegAngle(aValue);
  end;
end;

procedure TDGLBaseSceneObject.SetRotation(aRotation: TDGLCoordinates);
begin
  FRotation.Assign(aRotation);
  TransformationChanged;
end;

function TDGLBaseSceneObject.GetPitchAngle: Single;
begin
  Result := FRotation.X;
end;

function TDGLBaseSceneObject.GetTurnAngle: Single;
begin
  Result := FRotation.Y;
end;

function TDGLBaseSceneObject.GetRollAngle: Single;
begin
  Result := FRotation.Z;
end;

procedure TDGLBaseSceneObject.PointTo(const ATargetObject: TDGLBaseSceneObject; const AUpVector: TVector);
begin
  PointTo(ATargetObject.AbsolutePosition, AUpVector);
end;

procedure TDGLBaseSceneObject.PointTo(const AAbsolutePosition, AUpVector: TVector);
var
  absDir, absRight, absUp: TVector;
begin
  // first compute absolute attitude for pointing
  absDir := VectorSubtract(AAbsolutePosition, Self.AbsolutePosition);
  NormalizeVector(absDir);
  absRight := VectorCrossProduct(absDir, AUpVector);
  NormalizeVector(absRight);
  absUp := VectorCrossProduct(absRight, absDir);
  // convert absolute to local and adjust object
  if Parent <> nil then
  begin
    FDirection.AsVector := Parent.AbsoluteToLocal(absDir);
    FUp.AsVector        := Parent.AbsoluteToLocal(absUp);
  end
  else
  begin
    FDirection.AsVector := absDir;
    FUp.AsVector        := absUp;
  end;
  TransformationChanged
end;

procedure TDGLBaseSceneObject.SetShowAxes(aValue: Boolean);
begin
  if FShowAxes <> aValue then
  begin
    FShowAxes := aValue;
    NotifyChange(Self);
  end;
end;

procedure TDGLBaseSceneObject.SetScaling(aValue: TDGLCoordinates);
begin
  FScaling.Assign(aValue);
  TransformationChanged;
end;

procedure TDGLBaseSceneObject.SetName(const NewName: TComponentName);
begin
  if Name <> NewName then
  begin
    inherited SetName(NewName);
    if assigned(vGLBaseSceneObjectNameChangeEvent) then
      vGLBaseSceneObjectNameChangeEvent(Self);
  end;
end;

procedure TDGLBaseSceneObject.SetParent(const val: TDGLBaseSceneObject);
begin
  MoveTo(val);
end;

function TDGLBaseSceneObject.GetIndex: Integer;
begin
  if assigned(FParent) then
    Result := FParent.FChildren.IndexOf(Self)
  else
    Result := -1;
end;

procedure TDGLBaseSceneObject.SetIndex(aValue: Integer);
var
  LCount:       Integer;
  parentBackup: TDGLBaseSceneObject;
begin
  if assigned(FParent) then
  begin
    if aValue < 0 then
      aValue := 0;
    LCount   := FParent.Count;
    if aValue >= LCount then
      aValue := LCount - 1;
    if aValue <> Index then
    begin
      if assigned(FScene) then
        FScene.BeginUpdate;
      parentBackup := FParent;
      parentBackup.Remove(Self, False);
      parentBackup.Insert(aValue, Self);
      if assigned(FScene) then
        FScene.EndUpdate;
    end;
  end;
end;

procedure TDGLBaseSceneObject.SetParentComponent(Value: TComponent);
begin
  inherited;
  if Value = FParent then
    Exit;

  if Value is TDGLScene then
    SetParent(TDGLScene(Value).Objects)
  else if Value is TDGLBaseSceneObject then
    SetParent(TDGLBaseSceneObject(Value))
  else
    SetParent(nil);
end;

procedure TDGLBaseSceneObject.StructureChanged;
begin
  if not(ocStructure in FChanges) then
  begin
    Include(FChanges, ocStructure);
    NotifyChange(Self);
  end
  else if osDirectDraw in ObjectStyle then
    NotifyChange(Self);
end;

procedure TDGLBaseSceneObject.ClearStructureChanged;
begin
  Exclude(FChanges, ocStructure);
  SetBBChanges(BBChanges + [oBBcStructure]);
end;

procedure TDGLBaseSceneObject.RecTransformationChanged;
var
  i:      Integer;
  List:   PPointerObjectList;
  matSet: TObjectChanges;
begin
  matSet := [ocAbsoluteMatrix, ocInvAbsoluteMatrix];
  if matSet * FChanges <> matSet then
  begin
    FChanges := FChanges + matSet;
    if assigned(FChildren) then
    begin
      List  := FChildren.List;
      for i := 0 to FChildren.Count - 1 do
        TDGLBaseSceneObject(List^[i]).RecTransformationChanged;
    end;
  end;
end;

procedure TDGLBaseSceneObject.TransformationChanged;
begin
  if not(ocTransformation in FChanges) then
  begin
    Include(FChanges, ocTransformation);
    RecTransformationChanged;
    if not(csLoading in ComponentState) then
      NotifyChange(Self);
  end;
end;

procedure TDGLBaseSceneObject.MoveTo(newParent: TDGLBaseSceneObject);
begin
  if newParent = FParent then
    Exit;
  if assigned(FParent) then
  begin
    FParent.Remove(Self, False);
    FParent := nil;
  end;
  if assigned(newParent) then
    newParent.AddChild(Self)
  else
    SetScene(nil);
end;

procedure TDGLBaseSceneObject.MoveUp;
begin
  if assigned(Parent) then
    Parent.MoveChildUp(Parent.IndexOfChild(Self));
end;

procedure TDGLBaseSceneObject.MoveDown;
begin
  if assigned(Parent) then
    Parent.MoveChildDown(Parent.IndexOfChild(Self));
end;

procedure TDGLBaseSceneObject.MoveFirst;
begin
  if assigned(Parent) then
    Parent.MoveChildFirst(Parent.IndexOfChild(Self));
end;

procedure TDGLBaseSceneObject.MoveLast;
begin
  if assigned(Parent) then
    Parent.MoveChildLast(Parent.IndexOfChild(Self));
end;

procedure TDGLBaseSceneObject.MoveObjectAround(anObject: TDGLBaseSceneObject; pitchDelta, turnDelta: Single);
var
  originalT2C, normalT2C, normalCameraRight, newPos: TVector;
  pitchNow, dist:                                    Single;
begin
  if assigned(anObject) then
  begin
    // normalT2C points away from the direction the camera is looking
    originalT2C := VectorSubtract(AbsolutePosition, anObject.AbsolutePosition);
    SetVector(normalT2C, originalT2C);
    dist := VectorLength(normalT2C);
    NormalizeVector(normalT2C);
    // normalRight points to the camera's right
    // the camera is pitching around this axis.
    normalCameraRight := VectorCrossProduct(AbsoluteUp, normalT2C);
    if VectorLength(normalCameraRight) < 0.001 then
      SetVector(normalCameraRight, XVector) // arbitrary vector
    else
      NormalizeVector(normalCameraRight);
    // calculate the current pitch.
    // 0 is looking down and PI is looking up
    pitchNow := ArcCos(VectorDotProduct(AbsoluteUp, normalT2C));
    pitchNow := ClampValue(pitchNow + DegToRad(pitchDelta), 0 + 0.025, PI - 0.025);
    // create a new vector pointing up and then rotate it down
    // into the new position
    SetVector(normalT2C, AbsoluteUp);
    RotateVector(normalT2C, normalCameraRight, -pitchNow);
    RotateVector(normalT2C, AbsoluteUp, -DegToRadian(turnDelta));
    ScaleVector(normalT2C, dist);
    newPos := VectorAdd(AbsolutePosition, VectorSubtract(normalT2C, originalT2C));
    if assigned(Parent) then
      newPos          := Parent.AbsoluteToLocal(newPos);
    Position.AsVector := newPos;
  end;
end;

procedure TDGLBaseSceneObject.MoveObjectAllAround(anObject: TDGLBaseSceneObject; pitchDelta, turnDelta: Single);
var
  upVector:    TVector;
  lookat:      TVector;
  rightVector: TVector;
  tempvector:  TVector;
  T2C:         TVector;

begin

  // if camera has got a target
  if assigned(anObject) then
  begin
    // vector camera to target
    lookat := VectorNormalize(VectorSubtract(anObject.AbsolutePosition, AbsolutePosition));
    // camera up vector
    upVector := VectorNormalize(AbsoluteUp);

    // if upvector and lookat vector are colinear, it is necessary to compute new up vector
    if Abs(VectorDotProduct(lookat, upVector)) > 0.99 then
    begin
      // X or Y vector use to generate upvector
      SetVector(tempvector, 1, 0, 0);
      // if lookat is colinear to X vector use Y vector to generate upvector
      if Abs(VectorDotProduct(tempvector, lookat)) > 0.99 then
      begin
        SetVector(tempvector, 0, 1, 0);
      end;
      upVector    := VectorCrossProduct(tempvector, lookat);
      rightVector := VectorCrossProduct(lookat, upVector);
    end
    else
    begin
      rightVector := VectorCrossProduct(lookat, upVector);
      upVector    := VectorCrossProduct(rightVector, lookat);
    end;
    // now the up right and lookat vector are orthogonal

    // vector Target to camera
    T2C := VectorSubtract(AbsolutePosition, anObject.AbsolutePosition);
    RotateVector(T2C, rightVector, DegToRadian(-pitchDelta));
    RotateVector(T2C, upVector, DegToRadian(-turnDelta));
    AbsolutePosition := VectorAdd(anObject.AbsolutePosition, T2C);

    // now update new up vector
    RotateVector(upVector, rightVector, DegToRadian(-pitchDelta));
    AbsoluteUp        := upVector;
    AbsoluteDirection := VectorSubtract(anObject.AbsolutePosition, AbsolutePosition);

  end;
end;

procedure TDGLBaseSceneObject.CoordinateChanged(Sender: TDGLCustomCoordinates);
var
  rightVector: TVector;
begin
  if FIsCalculating then
    Exit;
  FIsCalculating := True;
  try
    if Sender = FDirection then
    begin
      if FDirection.VectorLength = 0 then
        FDirection.DirectVector := ZHmgVector;
      FDirection.Normalize;
      // adjust up vector
      rightVector := VectorCrossProduct(FDirection.AsVector, FUp.AsVector);
      // Rightvector is zero if direction changed exactly by 90 degrees,
      // in this case assume a default vector
      if VectorLength(rightVector) < 1E-5 then
      begin
        rightVector := VectorCrossProduct(ZHmgVector, FUp.AsVector);
        if VectorLength(rightVector) < 1E-5 then
          rightVector := VectorCrossProduct(XHmgVector, FUp.AsVector);
      end;
      FUp.DirectVector := VectorCrossProduct(rightVector, FDirection.AsVector);
      FUp.Normalize;
    end
    else if Sender = FUp then
    begin
      if FUp.VectorLength = 0 then
        FUp.DirectVector := YHmgVector;
      FUp.Normalize;
      // adjust up vector
      rightVector := VectorCrossProduct(FDirection.AsVector, FUp.AsVector);
      // Rightvector is zero if direction changed exactly by 90 degrees,
      // in this case assume a default vector
      if VectorLength(rightVector) < 1E-5 then
      begin
        rightVector := VectorCrossProduct(ZHmgVector, FUp.AsVector);
        if VectorLength(rightVector) < 1E-5 then
          rightVector := VectorCrossProduct(XHmgVector, FUp.AsVector);
      end;
      FDirection.DirectVector := VectorCrossProduct(FUp.AsVector, rightVector);
      FDirection.Normalize;
    end;
    TransformationChanged;
  finally
    FIsCalculating := False;
  end;
end;

procedure TDGLBaseSceneObject.DoProgress(const progressTime: TProgressTimes);
var
  i: Integer;
begin
  if assigned(FChildren) then
    for i := FChildren.Count - 1 downto 0 do
      TDGLBaseSceneObject(FChildren.List^[i]).DoProgress(progressTime);
  if assigned(FGLBehaviours) then
    FGLBehaviours.DoProgress(progressTime);
  if assigned(FGLObjectEffects) then
    FGLObjectEffects.DoProgress(progressTime);
  if assigned(FOnProgress) then
    with progressTime do
      FOnProgress(Self, deltaTime, newTime);
end;

procedure TDGLBaseSceneObject.Insert(AIndex: Integer; AChild: TDGLBaseSceneObject);
begin
  if not assigned(FChildren) then
    FChildren := TDGLPersistentObjectList.Create;
  with FChildren do
  begin
    if assigned(AChild.FParent) then
      AChild.FParent.Remove(AChild, False);
    Insert(AIndex, AChild);
  end;
  AChild.FParent := Self;
  if AChild.FScene <> FScene then
    AChild.DestroyHandles;
  AChild.SetScene(FScene);
  if assigned(FScene) then
    FScene.AddLights(AChild);
  AChild.TransformationChanged;

  AChild.DoOnAddedToParent;
end;

procedure TDGLBaseSceneObject.Remove(AChild: TDGLBaseSceneObject; keepChildren: Boolean);
var
  i: Integer;
begin
  if not assigned(FChildren) then
    Exit;
  if AChild.Parent = Self then
  begin
    if assigned(FScene) then
      FScene.RemoveLights(AChild);
    if AChild.Owner = Self then
      RemoveComponent(AChild);
    FChildren.Remove(AChild);
    AChild.FParent := nil;
    if keepChildren then
    begin
      BeginUpdate;
      if AChild.Count <> 0 then
        for i := AChild.Count - 1 downto 0 do
          if not IsSubComponent(AChild.Children[i]) then
            AChild.Children[i].MoveTo(Self);
      EndUpdate;
    end
    else
      NotifyChange(Self);
  end;
end;

function TDGLBaseSceneObject.IndexOfChild(AChild: TDGLBaseSceneObject): Integer;
begin
  if assigned(FChildren) then
    Result := FChildren.IndexOf(AChild)
  else
    Result := -1;
end;

function TDGLBaseSceneObject.FindChild(const aName: string; ownChildrenOnly: Boolean): TDGLBaseSceneObject;
var
  i:   Integer;
  res: TDGLBaseSceneObject;
begin
  res    := nil;
  Result := nil;
  if not assigned(FChildren) then
    Exit;
  for i := 0 to FChildren.Count - 1 do
  begin
    if CompareText(TDGLBaseSceneObject(FChildren[i]).Name, aName) = 0 then
    begin
      res := TDGLBaseSceneObject(FChildren[i]);
      Break;
    end;
  end;
  if not ownChildrenOnly then
  begin
    for i := 0 to FChildren.Count - 1 do
      with TDGLBaseSceneObject(FChildren[i]) do
      begin
        Result := FindChild(aName, ownChildrenOnly);
        if assigned(Result) then
          Break;
      end;
  end;
  if not assigned(Result) then
    Result := res;
end;

procedure TDGLBaseSceneObject.ExchangeChildren(anIndex1, anIndex2: Integer);
begin
  Assert(assigned(FChildren), 'No children found!');
  FChildren.Exchange(anIndex1, anIndex2);
  NotifyChange(Self);
end;

procedure TDGLBaseSceneObject.ExchangeChildrenSafe(anIndex1, anIndex2: Integer);
begin
  Assert(assigned(FChildren), 'No children found!');
  if (anIndex1 < FChildren.Count) and (anIndex2 < FChildren.Count) and (anIndex1 > -1) and (anIndex2 > -1) and (anIndex1 <> anIndex2) then
  begin
    FChildren.Exchange(anIndex1, anIndex2);
    NotifyChange(Self);
  end;
end;

procedure TDGLBaseSceneObject.MoveChildUp(anIndex: Integer);
begin
  Assert(assigned(FChildren), 'No children found!');
  if anIndex > 0 then
  begin
    FChildren.Exchange(anIndex, anIndex - 1);
    NotifyChange(Self);
  end;
end;

procedure TDGLBaseSceneObject.MoveChildDown(anIndex: Integer);
begin
  Assert(assigned(FChildren), 'No children found!');
  if anIndex < FChildren.Count - 1 then
  begin
    FChildren.Exchange(anIndex, anIndex + 1);
    NotifyChange(Self);
  end;
end;

procedure TDGLBaseSceneObject.MoveChildFirst(anIndex: Integer);
begin
  Assert(assigned(FChildren), 'No children found!');
  if anIndex <> 0 then
  begin
    FChildren.Move(anIndex, 0);
    NotifyChange(Self);
  end;
end;

procedure TDGLBaseSceneObject.MoveChildLast(anIndex: Integer);
begin
  Assert(assigned(FChildren), 'No children found!');
  if anIndex <> FChildren.Count - 1 then
  begin
    FChildren.Move(anIndex, FChildren.Count - 1);
    NotifyChange(Self);
  end;
end;

procedure TDGLBaseSceneObject.Render(var ARci: TRenderContextInfo);
var
  shouldRenderSelf, shouldRenderChildren: Boolean;
  aabb:                                   TAABB;
  master:                                 TObject;
begin
  {$IFDEF GLS_OPENGL_DEBUG}
  if GL.GREMEDY_string_marker then
    GL.StringMarkerGREMEDY(Length(Name) + Length('.Render'), PGLChar(TDGLString(Name + '.Render')));
  {$ENDIF}
  if (ARci.drawState = dsPicking) and not FPickable then
    Exit;
  // visibility culling determination
  if ARci.VisibilityCulling in [vcObjectBased, vcHierarchical] then
  begin
    if ARci.VisibilityCulling = vcObjectBased then
    begin
      shouldRenderSelf     := (osNoVisibilityCulling in ObjectStyle) or (not IsVolumeClipped(BarycenterAbsolutePosition, BoundingSphereRadius, ARci.rcci.frustum));
      shouldRenderChildren := assigned(FChildren);
    end
    else
    begin // vcHierarchical
      aabb                 := AxisAlignedBoundingBox;
      shouldRenderSelf     := (osNoVisibilityCulling in ObjectStyle) or (not IsVolumeClipped(aabb.min, aabb.max, ARci.rcci.frustum));
      shouldRenderChildren := shouldRenderSelf and assigned(FChildren);
    end;
    if not(shouldRenderSelf or shouldRenderChildren) then
      Exit;
  end
  else
  begin
    Assert(ARci.VisibilityCulling in [vcNone, vcInherited], 'Unknown visibility culling option');
    shouldRenderSelf     := True;
    shouldRenderChildren := assigned(FChildren);
  end;

  // Prepare Matrix and PickList stuff
  ARci.PipelineTransformation.Push;
  if ocTransformation in FChanges then
    RebuildMatrix;

  if ARci.proxySubObject then
    ARci.PipelineTransformation.ModelMatrix := MatrixMultiply(LocalMatrix^, ARci.PipelineTransformation.ModelMatrix)
  else
    ARci.PipelineTransformation.ModelMatrix := AbsoluteMatrix;

  master := nil;
  if ARci.drawState = dsPicking then
  begin
  //  if ARci.proxySubObject then
  //    master := TDGLSceneBuffer(ARci.buffer).FSelector.CurrentObject;
  //  TDGLSceneBuffer(ARci.buffer).FSelector.CurrentObject := Self;
  end;

  // Start rendering
  if shouldRenderSelf then
  begin
    vCurrentRenderingObject := Self;
    {$IFNDEF GLS_OPTIMIZATIONS}
    if FShowAxes then
      DrawAxes(ARci, $CCCC);
    {$ENDIF}
    if assigned(FGLObjectEffects) and (FGLObjectEffects.Count > 0) then
    begin
      ARci.PipelineTransformation.Push;
      FGLObjectEffects.RenderPreEffects(ARci);
      ARci.PipelineTransformation.Pop;

      ARci.PipelineTransformation.Push;
      if osIgnoreDepthBuffer in ObjectStyle then
      begin
        ARci.GLStates.Disable(stDepthTest);
        DoRender(ARci, True, shouldRenderChildren);
        ARci.GLStates.Enable(stDepthTest);
      end
      else
        DoRender(ARci, True, shouldRenderChildren);

      FGLObjectEffects.RenderPostEffects(ARci);
      ARci.PipelineTransformation.Pop;
    end
    else
    begin
      if osIgnoreDepthBuffer in ObjectStyle then
      begin
        ARci.GLStates.Disable(stDepthTest);
        DoRender(ARci, True, shouldRenderChildren);
        ARci.GLStates.Enable(stDepthTest);
      end
      else
        DoRender(ARci, True, shouldRenderChildren);

    end;
    vCurrentRenderingObject := nil;
  end
  else
  begin
    if (osIgnoreDepthBuffer in ObjectStyle) and TDGLSceneBuffer(ARci.buffer).DepthTest then
    begin
      ARci.GLStates.Disable(stDepthTest);
      DoRender(ARci, False, shouldRenderChildren);
      ARci.GLStates.Enable(stDepthTest);
    end
    else
      DoRender(ARci, False, shouldRenderChildren);
  end;
  // Pop Name & Matrix
  if assigned(master) then
//    TDGLSceneBuffer(ARci.buffer).FSelector.CurrentObject := master;
  ARci.PipelineTransformation.Pop;
end;

procedure TDGLBaseSceneObject.DoRender(var ARci: TRenderContextInfo; ARenderSelf, ARenderChildren: Boolean);
begin
  // start rendering self
  if ARenderSelf then
  begin
    if (osDirectDraw in ObjectStyle) or ARci.amalgamating then
      BuildList(ARci)
//    else
//      ARci.GLStates.CallList(GetHandle(ARci));
  end;
  // start rendering children (if any)
  if ARenderChildren then
    Self.RenderChildren(0, Count - 1, ARci);
end;

procedure TDGLBaseSceneObject.RenderChildren(firstChildIndex, lastChildIndex: Integer; var rci: TRenderContextInfo);
var
  i:          Integer;
  objList:    TDGLPersistentObjectList;
  distList:   TSingleList;
  plist:      PPointerObjectList;
  obj:        TDGLBaseSceneObject;
  oldSorting: TDGLObjectsSorting;
  oldCulling: TDGLVisibilityCulling;
begin
  if not assigned(FChildren) then
    Exit;
  oldCulling := rci.VisibilityCulling;
  if Self.VisibilityCulling <> vcInherited then
    rci.VisibilityCulling := Self.VisibilityCulling;
  if lastChildIndex = firstChildIndex then
  begin
    obj := TDGLBaseSceneObject(FChildren.List^[firstChildIndex]);
    if obj.Visible then
      obj.Render(rci)
  end
  else if lastChildIndex > firstChildIndex then
  begin
    oldSorting := rci.ObjectsSorting;
    if Self.ObjectsSorting <> osInherited then
      rci.ObjectsSorting := Self.ObjectsSorting;
    case rci.ObjectsSorting of
      osNone:
        begin
          plist := FChildren.List;
          for i := firstChildIndex to lastChildIndex do
          begin
            obj := TDGLBaseSceneObject(plist^[i]);
            if obj.Visible then
              obj.Render(rci);
          end;
        end;
      osRenderFarthestFirst, osRenderBlendedLast, osRenderNearestFirst:
        begin
          distList             := TSingleList.Create;
          objList              := TDGLPersistentObjectList.Create;
          distList.GrowthDelta := lastChildIndex + 1; // no reallocations
          objList.GrowthDelta  := distList.GrowthDelta;
          try
            case rci.ObjectsSorting of
              osRenderBlendedLast:
                // render opaque stuff
                for i := firstChildIndex to lastChildIndex do
                begin
                  obj := TDGLBaseSceneObject(FChildren.List^[i]);
                  if obj.Visible then
                  begin
                    if not obj.Blended then
                      obj.Render(rci)
                    else
                    begin
                      objList.Add(obj);
                      distList.Add(1 + obj.BarycenterSqrDistanceTo(rci.cameraPosition));
                    end;
                  end;
                end;
              osRenderFarthestFirst:
                for i := firstChildIndex to lastChildIndex do
                begin
                  obj := TDGLBaseSceneObject(FChildren.List^[i]);
                  if obj.Visible then
                  begin
                    objList.Add(obj);
                    distList.Add(1 + obj.BarycenterSqrDistanceTo(rci.cameraPosition));
                  end;
                end;
              osRenderNearestFirst:
                for i := firstChildIndex to lastChildIndex do
                begin
                  obj := TDGLBaseSceneObject(FChildren.List^[i]);
                  if obj.Visible then
                  begin
                    objList.Add(obj);
                    distList.Add(-1 - obj.BarycenterSqrDistanceTo(rci.cameraPosition));
                  end;
                end;
            else
              Assert(False);
            end;
            if distList.Count > 0 then
            begin
              if distList.Count > 1 then
                FastQuickSortLists(0, distList.Count - 1, distList, objList);
              plist := objList.List;
              for i := objList.Count - 1 downto 0 do
                TDGLBaseSceneObject(plist^[i]).Render(rci);
            end;
          finally
            objList.Free;
            distList.Free;
          end;
        end;
    else
      Assert(False);
    end;
    rci.ObjectsSorting := oldSorting;
  end;
  rci.VisibilityCulling := oldCulling;
end;

procedure TDGLBaseSceneObject.NotifyChange(Sender: TObject);
begin
  if assigned(FScene) and (not IsUpdating) then
    FScene.NotifyChange(Self);
end;

function TDGLBaseSceneObject.GetMatrix: TMatrix;
begin
  RebuildMatrix;
  Result := FLocalMatrix^;
end;

function TDGLBaseSceneObject.MatrixAsAddress: PMatrix;
begin
  RebuildMatrix;
  Result := FLocalMatrix;
end;

procedure TDGLBaseSceneObject.SetMatrix(const aValue: TMatrix);
begin
  FLocalMatrix^           := aValue;
  FDirection.DirectVector := VectorNormalize(FLocalMatrix^.v[2]);
  FUp.DirectVector        := VectorNormalize(FLocalMatrix^.v[1]);
  Scale.SetVector(VectorLength(FLocalMatrix^.v[0]), VectorLength(FLocalMatrix^.v[1]), VectorLength(FLocalMatrix^.v[2]), 0);
  FPosition.DirectVector := FLocalMatrix^.v[3];
  TransformationChanged;
end;

procedure TDGLBaseSceneObject.SetPosition(APosition: TDGLCoordinates);
begin
  FPosition.SetPoint(APosition.DirectX, APosition.DirectY, APosition.DirectZ);
end;

procedure TDGLBaseSceneObject.SetDirection(AVector: TDGLCoordinates);
begin
  if not VectorIsNull(AVector.DirectVector) then
    FDirection.SetVector(AVector.DirectX, AVector.DirectY, AVector.DirectZ);
end;

procedure TDGLBaseSceneObject.SetUp(AVector: TDGLCoordinates);
begin
  if not VectorIsNull(AVector.DirectVector) then
    FUp.SetVector(AVector.DirectX, AVector.DirectY, AVector.DirectZ);
end;

function TDGLBaseSceneObject.GetVisible: Boolean;
begin
  Result := FVisible;
end;

function TDGLBaseSceneObject.GetPickable: Boolean;
begin
  Result := FPickable;
end;

procedure TDGLBaseSceneObject.SetVisible(aValue: Boolean);
begin
  if FVisible <> aValue then
  begin
    FVisible := aValue;
    NotifyChange(Self);
  end;
end;

procedure TDGLBaseSceneObject.SetPickable(aValue: Boolean);
begin
  if FPickable <> aValue then
  begin
    FPickable := aValue;
    NotifyChange(Self);
  end;
end;

procedure TDGLBaseSceneObject.SetObjectsSorting(const val: TDGLObjectsSorting);
begin
  if FObjectsSorting <> val then
  begin
    FObjectsSorting := val;
    NotifyChange(Self);
  end;
end;

procedure TDGLBaseSceneObject.SetVisibilityCulling(const val: TDGLVisibilityCulling);
begin
  if FVisibilityCulling <> val then
  begin
    FVisibilityCulling := val;
    NotifyChange(Self);
  end;
end;

procedure TDGLBaseSceneObject.SetBehaviours(const val: TDGLBehaviours);
begin
  Behaviours.Assign(val);
end;

function TDGLBaseSceneObject.GetBehaviours: TDGLBehaviours;
begin
  if not assigned(FGLBehaviours) then
    FGLBehaviours := TDGLBehaviours.Create(Self);
  Result          := FGLBehaviours;
end;

procedure TDGLBaseSceneObject.SetEffects(const val: TDGLObjectEffects);
begin
  Effects.Assign(val);
end;

function TDGLBaseSceneObject.GetEffects: TDGLObjectEffects;
begin
  if not assigned(FGLObjectEffects) then
    FGLObjectEffects := TDGLObjectEffects.Create(Self);
  Result             := FGLObjectEffects;
end;

procedure TDGLBaseSceneObject.SetScene(const Value: TDGLScene);
var
  i: Integer;
begin
  if Value <> FScene then
  begin
    // must be freed, the new scene may be using a non-compatible RC
//    if FScene <> nil then
//      DestroyHandles;
    FScene := Value;
    // propagate for childs
    if assigned(FChildren) then
      for i := 0 to FChildren.Count - 1 do
        Children[i].SetScene(FScene);
  end;
end;

procedure TDGLBaseSceneObject.Translate(tx, ty, tz: Single);
begin
  FPosition.Translate(AffineVectorMake(tx, ty, tz));
end;

function TDGLBaseSceneObject.GetAbsoluteAffinePosition: TAffineVector;
var
  temp: TVector;
begin
  temp   := GetAbsolutePosition;
  Result := AffineVectorMake(temp.v[0], temp.v[1], temp.v[2]);
end;

function TDGLBaseSceneObject.GetAbsoluteAffineDirection: TAffineVector;
var
  temp: TVector;
begin
  temp   := GetAbsoluteDirection;
  Result := AffineVectorMake(temp.v[0], temp.v[1], temp.v[2]);
end;

function TDGLBaseSceneObject.GetAbsoluteAffineUp: TAffineVector;
var
  temp: TVector;
begin
  temp   := GetAbsoluteUp;
  Result := AffineVectorMake(temp.v[0], temp.v[1], temp.v[2]);
end;

procedure TDGLBaseSceneObject.SetAbsoluteAffinePosition(const Value: TAffineVector);
begin
  SetAbsolutePosition(VectorMake(Value, 1));
end;

procedure TDGLBaseSceneObject.SetAbsoluteAffineUp(const v: TAffineVector);
begin
  SetAbsoluteUp(VectorMake(v, 1));
end;

procedure TDGLBaseSceneObject.SetAbsoluteAffineDirection(const v: TAffineVector);
begin
  SetAbsoluteDirection(VectorMake(v, 1));
end;

function TDGLBaseSceneObject.AffineLeftVector: TAffineVector;
begin
  Result := AffineVectorMake(LeftVector);
end;

function TDGLBaseSceneObject.AffineRight: TAffineVector;
begin
  Result := AffineVectorMake(Right);
end;

function TDGLBaseSceneObject.DistanceTo(const pt: TAffineVector): Single;
begin
  Result := VectorDistance(AbsoluteAffinePosition, pt);
end;

function TDGLBaseSceneObject.SqrDistanceTo(const pt: TAffineVector): Single;
begin
  Result := VectorDistance2(AbsoluteAffinePosition, pt);
end;

procedure TDGLBaseSceneObject.DoOnAddedToParent;
begin
  if assigned(FOnAddedToParent) then
    FOnAddedToParent(Self);
end;

function TDGLBaseSceneObject.GetAbsoluteAffineScale: TAffineVector;
begin
  Result := AffineVectorMake(GetAbsoluteScale);
end;

procedure TDGLBaseSceneObject.SetAbsoluteAffineScale(const Value: TAffineVector);
begin
  SetAbsoluteScale(VectorMake(Value, GetAbsoluteScale.v[3]));
end;

{$IFDEF GLS_REGIONS}{$ENDREGION}{$ENDIF}

// ------------------
{ TDGLSceneRootObject }
{$IFDEF GLS_REGIONS}{$REGION 'TDGLSceneRootObject'}{$ENDIF}

constructor TDGLSceneRootObject.Create(AOwner: TComponent);
begin
  Assert(AOwner is TDGLScene);
  inherited Create(AOwner);
  ObjectStyle := ObjectStyle + [osDirectDraw];
  FScene      := TDGLScene(AOwner);
end;

{$IFDEF GLS_REGIONS}{$ENDREGION}{$ENDIF}

// ------------------
{ TDGLBaseBehaviour }
{$IFDEF GLS_REGIONS}{$REGION 'TDGLBaseBehaviour'}{$ENDIF}

constructor TDGLBaseBehaviour.Create(AOwner: TDGLXCollection);
begin
  inherited Create(AOwner);
  // nothing more, yet
end;

destructor TDGLBaseBehaviour.Destroy;
begin
  // nothing more, yet
  inherited Destroy;
end;

procedure TDGLBaseBehaviour.SetName(const val: string);
begin
  inherited SetName(val);
  if assigned(vGLBehaviourNameChangeEvent) then
    vGLBehaviourNameChangeEvent(Self);
end;

procedure TDGLBaseBehaviour.WriteToFiler(writer: TWriter);
begin
  inherited;

  with writer do
  begin
    WriteInteger(0); // Archive Version 0
    // nothing more, yet
  end;
end;

procedure TDGLBaseBehaviour.ReadFromFiler(reader: TReader);
begin
  if Owner.ArchiveVersion > 0 then
    inherited;

  with reader do
  begin
    if ReadInteger <> 0 then
      Assert(False);
    // nothing more, yet
  end;
end;

function TDGLBaseBehaviour.OwnerBaseSceneObject: TDGLBaseSceneObject;
begin
  Result := TDGLBaseSceneObject(Owner.Owner);
end;

procedure TDGLBaseBehaviour.DoProgress(const progressTime: TProgressTimes);
begin
  // does nothing
end;

{$IFDEF GLS_REGIONS}{$ENDREGION}{$ENDIF}

// ------------------
{ TDGLBehaviour }
{$IFDEF GLS_REGIONS}{$REGION 'TDGLBehaviour'}{$ENDIF}

constructor TDGLBehaviours.Create(AOwner: TPersistent);
begin
  Assert(AOwner is TDGLBaseSceneObject);
  inherited Create(AOwner);
end;

function TDGLBehaviours.GetNamePath: string;
var
  s: string;
begin
  Result := ClassName;
  if GetOwner = nil then
    Exit;
  s := GetOwner.GetNamePath;
  if s = '' then
    Exit;
  Result := s + '.Behaviours';
end;

class function TDGLBehaviours.ItemsClass: TDGLXCollectionItemClass;
begin
  Result := TDGLBehaviour;
end;

function TDGLBehaviours.GetBehaviour(Index: Integer): TDGLBehaviour;
begin
  Result := TDGLBehaviour(Items[index]);
end;

function TDGLBehaviours.CanAdd(aClass: TDGLXCollectionItemClass): Boolean;
begin
  Result := (not aClass.InheritsFrom(TDGLObjectEffect)) and (inherited CanAdd(aClass));
end;

procedure TDGLBehaviours.DoProgress(const progressTimes: TProgressTimes);
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    TDGLBehaviour(Items[i]).DoProgress(progressTimes);
end;

{$IFDEF GLS_REGIONS}{$ENDREGION}{$ENDIF}

// ------------------
{ TDGLObjectEffect }
{$IFDEF GLS_REGIONS}{$REGION 'TDGLObjectEffect'}{$ENDIF}

procedure TDGLObjectEffect.WriteToFiler(writer: TWriter);
begin
  inherited;
  with writer do
  begin
    WriteInteger(0); // Archive Version 0
    // nothing more, yet
  end;
end;

procedure TDGLObjectEffect.ReadFromFiler(reader: TReader);
begin
  if Owner.ArchiveVersion > 0 then
    inherited;

  with reader do
  begin
    if ReadInteger <> 0 then
      Assert(False);
    // nothing more, yet
  end;
end;

procedure TDGLObjectEffect.Render(var rci: TRenderContextInfo);
begin
  // nothing here, this implem is just to avoid "abstract error"
end;

{$IFDEF GLS_REGIONS}{$ENDREGION}{$ENDIF}

// ------------------
{ TDGLObjectEffects }
{$IFDEF GLS_REGIONS}{$REGION 'TDGLObjectEffects'}{$ENDIF}

constructor TDGLObjectEffects.Create(AOwner: TPersistent);
begin
  Assert(AOwner is TDGLBaseSceneObject);
  inherited Create(AOwner);
end;

function TDGLObjectEffects.GetNamePath: string;
var
  s: string;
begin
  Result := ClassName;
  if GetOwner = nil then
    Exit;
  s := GetOwner.GetNamePath;
  if s = '' then
    Exit;
  Result := s + '.Effects';
end;

class function TDGLObjectEffects.ItemsClass: TDGLXCollectionItemClass;
begin
  Result := TDGLObjectEffect;
end;

function TDGLObjectEffects.GetEffect(Index: Integer): TDGLObjectEffect;
begin
  Result := TDGLObjectEffect(Items[index]);
end;

function TDGLObjectEffects.CanAdd(aClass: TDGLXCollectionItemClass): Boolean;
begin
  Result := (aClass.InheritsFrom(TDGLObjectEffect)) and (inherited CanAdd(aClass));
end;

procedure TDGLObjectEffects.DoProgress(const progressTime: TProgressTimes);
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    TDGLObjectEffect(Items[i]).DoProgress(progressTime);
end;

procedure TDGLObjectEffects.RenderPreEffects(var rci: TRenderContextInfo);
var
  i:      Integer;
  effect: TDGLObjectEffect;
begin
  for i := 0 to Count - 1 do
  begin
    effect := TDGLObjectEffect(Items[i]);
    if effect is TDGLObjectPreEffect then
      effect.Render(rci);
  end;
end;

procedure TDGLObjectEffects.RenderPostEffects(var rci: TRenderContextInfo);
var
  i:      Integer;
  effect: TDGLObjectEffect;
begin
  for i := 0 to Count - 1 do
  begin
    effect := TDGLObjectEffect(Items[i]);
    if effect is TDGLObjectPostEffect then
      effect.Render(rci)
    else if assigned(rci.afterRenderEffects) and (effect is TDGLObjectAfterEffect) then
      rci.afterRenderEffects.Add(effect);
  end;
end;

{$IFDEF GLS_REGIONS}{$ENDREGION}{$ENDIF}

// ------------------
{ TDGLCustomSceneObject }
{$IFDEF GLS_REGIONS}{$REGION 'TDGLCustomSceneObject'}{$ENDIF}

constructor TDGLCustomSceneObject.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FShader := TDGLLibShader.Create(nil);
end;

destructor TDGLCustomSceneObject.Destroy;
begin
  inherited Destroy;
  FShader.Free;
end;

procedure TDGLCustomSceneObject.Assign(Source: TPersistent);
begin
  if Source is TDGLCustomSceneObject then
  begin
    FShader.Assign(TDGLCustomSceneObject(Source).FShader);
    FHint := TDGLCustomSceneObject(Source).FHint;
  end;
  inherited Assign(Source);
end;

function TDGLCustomSceneObject.Blended: Boolean;
begin
  Result := Shader.ShaderModel.Material.Blended;
end;

procedure TDGLCustomSceneObject.Loaded;
begin
  inherited;
//  FMaterial.Loaded;
end;

procedure TDGLCustomSceneObject.SetShader(aValue: TDGLLibShader);
begin
  FShader.Assign(aValue);
  NotifyChange(Self);
end;

procedure TDGLCustomSceneObject.DestroyHandle;
begin
  inherited;
//  FMaterial.DestroyHandles;
end;

procedure TDGLCustomSceneObject.DoRender(var ARci: TRenderContextInfo; ARenderSelf, ARenderChildren: Boolean);
begin
  // start rendering self
  if ARenderSelf then
    if ARci.ignoreMaterials then
      if (osDirectDraw in ObjectStyle) or ARci.amalgamating then
        BuildList(ARci)
//      else
//        ARci.GLStates.CallList(GetHandle(ARci))
    else
    begin
      FShader.Apply(ARci);
      repeat
        if (osDirectDraw in ObjectStyle) or ARci.amalgamating then
          BuildList(ARci)
//        else
//          ARci.GLStates.CallList(GetHandle(ARci));
      until not FShader.UnApply(ARci);
    end;
  // start rendering children (if any)
  if ARenderChildren then
    Self.RenderChildren(0, Count - 1, ARci);
end;

{$IFDEF GLS_REGIONS}{$ENDREGION}{$ENDIF}

// ------------------
{ TDGImmaterialSceneObject }
{$IFDEF GLS_REGIONS}{$REGION 'TDGImmaterialSceneObject'}{$ENDIF}

procedure TDGImmaterialSceneObject.DoRender(var ARci: TRenderContextInfo; ARenderSelf, ARenderChildren: Boolean);
begin
  // start rendering self
  if ARenderSelf then
  begin
    if (osDirectDraw in ObjectStyle) or ARci.amalgamating then
      BuildList(ARci)
//    else
//      ARci.GLStates.CallList(GetHandle(ARci));
  end;
  // start rendering children (if any)
  if ARenderChildren then
    Self.RenderChildren(0, Count - 1, ARci);
end;

{$IFDEF GLS_REGIONS}{$ENDREGION}{$ENDIF}

// ------------------
{ TDGLCameraInvariantObject }
{$IFDEF GLS_REGIONS}{$REGION 'TDGLCameraInvariantObject'}{$ENDIF}

constructor TDGLCameraInvariantObject.Create(AOwner: TComponent);
begin
  inherited;
  FCamInvarianceMode := cimNone;
end;

procedure TDGLCameraInvariantObject.Assign(Source: TPersistent);
begin
  if Source is TDGLCameraInvariantObject then
  begin
    FCamInvarianceMode := TDGLCameraInvariantObject(Source).FCamInvarianceMode;
  end;
  inherited Assign(Source);
end;

procedure TDGLCameraInvariantObject.DoRender(var ARci: TRenderContextInfo; ARenderSelf, ARenderChildren: Boolean);
begin
  if CamInvarianceMode <> cimNone then
    with ARci.PipelineTransformation do
    begin
      Push;
      try
        // prepare
        case CamInvarianceMode of
          cimPosition:
            begin
              ViewMatrix := MatrixMultiply(CreateTranslationMatrix(ARci.cameraPosition), ARci.PipelineTransformation.ViewMatrix);
            end;
          cimOrientation:
            begin
              // makes the coordinates system more 'intuitive' (Z+ forward)
              ViewMatrix := CreateScaleMatrix(Vector3fMake(1, -1, -1))
            end;
        else
          Assert(False);
        end;
        // Apply local transform
        ModelMatrix := LocalMatrix^;

        if ARenderSelf then
        begin
          if (osDirectDraw in ObjectStyle) or ARci.amalgamating then
            BuildList(ARci)
//          else
//            ARci.GLStates.CallList(GetHandle(ARci));
        end;
        if ARenderChildren then
          Self.RenderChildren(0, Count - 1, ARci);
      finally
        Pop;
      end;
    end
  else
    inherited;
end;

procedure TDGLCameraInvariantObject.SetCamInvarianceMode(const val: TDGLCameraInvarianceMode);
begin
  if FCamInvarianceMode <> val then
  begin
    FCamInvarianceMode := val;
    NotifyChange(Self);
  end;
end;

{$IFDEF GLS_REGIONS}{$ENDREGION}{$ENDIF}

// ------------------
{ TDGLCamera }
{$IFDEF GLS_REGIONS}{$REGION 'TDGLCamera'}{$ENDIF}

constructor TDGLCamera.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  FFocalLength   := 50;
  FDepthOfView   := 100;
  FNearPlaneBias := 1;
  FDirection.Initialize(VectorMake(0, 0, -1, 0));
  FCameraStyle := csPerspective;
  FSceneScale  := 1;
  FDesign      := False;
  FFOVY        := -1;
  FKeepFOVMode := ckmHorizontalFOV;
end;

destructor TDGLCamera.Destroy;
begin
  TargetObject := nil;
  inherited;
end;

procedure TDGLCamera.Assign(Source: TPersistent);
var
  cam: TDGLCamera;
  dir: TVector;
begin
  if Assigned(Source) then
  begin
    inherited Assign(Source);

    if Source is TDGLCamera then
    begin
      cam := TDGLCamera(Source);
      SetDepthOfView(cam.DepthOfView);
      SetFocalLength(cam.FocalLength);
      SetCameraStyle(cam.CameraStyle);
      SetSceneScale(cam.SceneScale);
      SetNearPlaneBias(cam.NearPlaneBias);
      SetScene(cam.Scene);
      SetKeepFOVMode(cam.FKeepFOVMode);

      if Parent <> nil then
      begin
        SetTargetObject(cam.TargetObject);
      end
      else // Design camera
      begin
        Position.AsVector := cam.AbsolutePosition;
        if Assigned(cam.TargetObject) then
        begin
          VectorSubtract(cam.TargetObject.AbsolutePosition, AbsolutePosition, dir);
          NormalizeVector(dir);
          Direction.AsVector := dir;
        end;
      end;
    end;
  end;
end;

function TDGLCamera.AbsoluteVectorToTarget: TVector;
begin
  if TargetObject <> nil then
  begin
    VectorSubtract(TargetObject.AbsolutePosition, AbsolutePosition, Result);
    NormalizeVector(Result);
  end
  else
    Result := AbsoluteDirection;
end;

function TDGLCamera.AbsoluteRightVectorToTarget: TVector;
begin
  if TargetObject <> nil then
  begin
    VectorSubtract(TargetObject.AbsolutePosition, AbsolutePosition, Result);
    Result := VectorCrossProduct(Result, AbsoluteUp);
    NormalizeVector(Result);
  end
  else
    Result := AbsoluteRight;
end;

function TDGLCamera.AbsoluteUpVectorToTarget: TVector;
begin
  if TargetObject <> nil then
    Result := VectorCrossProduct(AbsoluteRightVectorToTarget, AbsoluteVectorToTarget)
  else
    Result := AbsoluteUp;
end;

procedure TDGLCamera.Apply;
var
  v, d, v2: TVector;
  absPos:   TVector;
  LM, mat:  TMatrix;
begin
  if Assigned(FDeferredApply) then
    FDeferredApply(Self)
  else
  begin
    if Assigned(FTargetObject) then
    begin
      v      := TargetObject.AbsolutePosition;
      absPos := AbsolutePosition;
      VectorSubtract(v, absPos, d);
      NormalizeVector(d);
      FLastDirection := d;
      LM             := CreateLookAtMatrix(absPos, v, Up.AsVector);
    end
    else
    begin
      if Assigned(Parent) then
        mat := Parent.AbsoluteMatrix
      else
        mat          := IdentityHmgMatrix;
      absPos         := AbsolutePosition;
      v              := VectorTransform(Direction.AsVector, mat);
      FLastDirection := v;
      d              := VectorTransform(Up.AsVector, mat);
      v2             := VectorAdd(absPos, v);
      LM             := CreateLookAtMatrix(absPos, v2, d);
    end;
    with CurrentDGLContext.PipelineTransformation do
      ViewMatrix := MatrixMultiply(LM, ViewMatrix);
    ClearStructureChanged;
  end;
end;

procedure TDGLCamera.ApplyPerspective(const AViewport: TRectangle; AWidth, AHeight: Integer; ADPI: Integer);
var
  vLeft, vRight, vBottom, vTop, vFar: Single;
  MaxDim, ratio, f:                   Double;
  xmax, ymax:                         Double;
  mat:                                TMatrix;
const
  cEpsilon: Single = 1E-4;

  function IsPerspective(CamStyle: TDGLCameraStyle): Boolean;
  begin
    Result := CamStyle in [csPerspective, csInfinitePerspective, csPerspectiveKeepFOV];
  end;

begin
  if (AWidth <= 0) or (AHeight <= 0) then
    Exit;

  if CameraStyle = csOrtho2D then
  begin
    vLeft      := 0;
    vRight     := AWidth;
    vBottom    := 0;
    vTop       := AHeight;
    FNearPlane := -1;
    vFar       := 1;
    mat        := CreateOrthoMatrix(vLeft, vRight, vBottom, vTop, FNearPlane, vFar);
    with CurrentDGLContext.PipelineTransformation do
      ProjectionMatrix := MatrixMultiply(mat, ProjectionMatrix);
    FViewPortRadius    := VectorLength(AWidth, AHeight) / 2;
  end
  else if CameraStyle = csCustom then
  begin
    FViewPortRadius := VectorLength(AWidth, AHeight) / 2;
    if Assigned(FOnCustomPerspective) then
      FOnCustomPerspective(AViewport, AWidth, AHeight, ADPI, FViewPortRadius);
  end
  else
  begin
    // determine biggest dimension and resolution (height or width)
    MaxDim := AWidth;
    if AHeight > MaxDim then
      MaxDim := AHeight;

    // calculate near plane distance and extensions;
    // Scene ratio is determined by the window ratio. The viewport is just a
    // specific part of the entire window and has therefore no influence on the
    // scene ratio. What we need to know, though, is the ratio between the window
    // borders (left, top, right and bottom) and the viewport borders.
    // Note: viewport.top is actually bottom, because the window (and viewport) origin
    // in OGL is the lower left corner

    if IsPerspective(CameraStyle) then
      f := FNearPlaneBias / (AWidth * FSceneScale)
    else
      f := 100 * FNearPlaneBias / (FocalLength * AWidth * FSceneScale);

    // calculate window/viewport ratio for right extent
    ratio := (2 * AViewport.width + 2 * AViewport.Left - AWidth) * f;
    // calculate aspect ratio correct right value of the view frustum and take
    // the window/viewport ratio also into account
    vRight := ratio * AWidth / (2 * MaxDim);

    // the same goes here for the other three extents
    // left extent:
    ratio := (AWidth - 2 * AViewport.Left) * f;
    vLeft := -ratio * AWidth / (2 * MaxDim);

    if IsPerspective(CameraStyle) then
      f := FNearPlaneBias / (AHeight * FSceneScale)
    else
      f := 100 * FNearPlaneBias / (FocalLength * AHeight * FSceneScale);

    // top extent (keep in mind the origin is left lower corner):
    ratio := (2 * AViewport.height + 2 * AViewport.Top - AHeight) * f;
    vTop  := ratio * AHeight / (2 * MaxDim);

    // bottom extent:
    ratio   := (AHeight - 2 * AViewport.Top) * f;
    vBottom := -ratio * AHeight / (2 * MaxDim);

    FNearPlane := FFocalLength * 2 * ADPI / (25.4 * MaxDim) * FNearPlaneBias;
    vFar       := FNearPlane + FDepthOfView;

    // finally create view frustum (perspective or orthogonal)
    case CameraStyle of
      csPerspective:
        begin
          mat := CreateMatrixFromFrustum(vLeft, vRight, vBottom, vTop, FNearPlane, vFar);
        end;
      csPerspectiveKeepFOV:
        begin
          if FFOVY < 0 then // Need Update FOV
          begin
            FFOVY := ArcTan2(vTop - vBottom, 2 * FNearPlane) * 2;
            FFOVX := ArcTan2(vRight - vLeft, 2 * FNearPlane) * 2;
          end;

          case FKeepFOVMode of
            ckmVerticalFOV:
              begin
                ymax := FNearPlane * Tan(FFOVY / 2);
                xmax := ymax * AWidth / AHeight;
              end;
            ckmHorizontalFOV:
              begin
                xmax := FNearPlane * Tan(FFOVX / 2);
                ymax := xmax * AHeight / AWidth;
              end;
          else
            begin
              xmax := 0;
              ymax := 0;
              Assert(False, 'Unknown keep camera angle mode');
            end;
          end;
          mat := CreateMatrixFromFrustum(-xmax, xmax, -ymax, ymax, FNearPlane, vFar);
        end;
      csInfinitePerspective:
        begin
          mat           := IdentityHmgMatrix;
          mat.v[0].v[0] := 2 * FNearPlane / (vRight - vLeft);
          mat.v[1].v[1] := 2 * FNearPlane / (vTop - vBottom);
          mat.v[2].v[0] := (vRight + vLeft) / (vRight - vLeft);
          mat.v[2].v[1] := (vTop + vBottom) / (vTop - vBottom);
          mat.v[2].v[2] := cEpsilon - 1;
          mat.v[2].v[3] := -1;
          mat.v[3].v[2] := FNearPlane * (cEpsilon - 2);
          mat.v[3].v[3] := 0;
        end;
      csOrthogonal:
        begin
          mat := CreateOrthoMatrix(vLeft, vRight, vBottom, vTop, FNearPlane, vFar);
        end;
    else
      Assert(False);
    end;

    with CurrentDGLContext.PipelineTransformation do
      ProjectionMatrix := MatrixMultiply(mat, ProjectionMatrix);

    FViewPortRadius := VectorLength(vRight, vTop) / FNearPlane;
  end;
end;

procedure TDGLCamera.AutoLeveling(Factor: Single);
var
  rightVector, rotAxis: TVector;
  angle:                Single;
begin
  angle   := RadToDeg(arccos(VectorDotProduct(FUp.AsVector, YVector)));
  rotAxis := VectorCrossProduct(YHmgVector, FUp.AsVector);
  if (angle > 1) and (VectorLength(rotAxis) > 0) then
  begin
    rightVector := VectorCrossProduct(FDirection.AsVector, FUp.AsVector);
    FUp.Rotate(AffineVectorMake(rotAxis), angle / (10 * Factor));
    FUp.Normalize;
    // adjust local coordinates
    FDirection.DirectVector := VectorCrossProduct(FUp.AsVector, rightVector);
    FRotation.Z             := -RadToDeg(ArcTan2(rightVector.v[1], VectorLength(rightVector.v[0], rightVector.v[2])));
  end;
end;

procedure TDGLCamera.Notification(AComponent: TComponent; Operation: TOperation);
begin
  if (Operation = opRemove) and (AComponent = FTargetObject) then
    TargetObject := nil;
  inherited;
end;

procedure TDGLCamera.SetTargetObject(const val: TDGLBaseSceneObject);
begin
  if (FTargetObject <> val) then
  begin
    if Assigned(FTargetObject) then
      FTargetObject.RemoveFreeNotification(Self);
    FTargetObject := val;
    if Assigned(FTargetObject) then
      FTargetObject.FreeNotification(Self);
    if not(csLoading in ComponentState) then
      TransformationChanged;
  end;
end;

procedure TDGLCamera.Reset(aSceneBuffer: TDGLSceneBuffer);
var
  Extent: Single;
begin
  FRotation.Z  := 0;
  FFocalLength := 50;
  with aSceneBuffer do
  begin
    ApplyPerspective(FViewport, FViewport.width, FViewport.height, FRenderDPI);
    FUp.DirectVector := YHmgVector;
    if FViewport.height < FViewport.width then
      Extent := FViewport.height * 0.25
    else
      Extent := FViewport.width * 0.25;
  end;
  FPosition.SetPoint(0, 0, FNearPlane * Extent);
  FDirection.SetVector(0, 0, -1, 0);
  TransformationChanged;
end;

procedure TDGLCamera.ZoomAll(aSceneBuffer: TDGLSceneBuffer);
var
  Extent: Single;
begin
  with aSceneBuffer do
  begin
    if FViewport.height < FViewport.width then
      Extent := FViewport.height * 0.25
    else
      Extent               := FViewport.width * 0.25;
    FPosition.DirectVector := NullHmgPoint;
    Move(-FNearPlane * Extent);
    // let the camera look at the scene center
    FDirection.SetVector(-FPosition.X, -FPosition.Y, -FPosition.Z, 0);
  end;
end;

procedure TDGLCamera.RotateObject(obj: TDGLBaseSceneObject; pitchDelta, turnDelta: Single; rollDelta: Single = 0);
var
  resMat:            TMatrix;
  vDir, vUp, vRight: TVector;
  v:                 TAffineVector;
  position1:         TVector;
  Scale1:            TVector;
begin
  // First we need to compute the actual camera's vectors, which may not be
  // directly available if we're in "targeting" mode
  vUp := AbsoluteUp;
  if TargetObject <> nil then
  begin
    vDir   := AbsoluteVectorToTarget;
    vRight := VectorCrossProduct(vDir, vUp);
    vUp    := VectorCrossProduct(vRight, vDir);
  end
  else
  begin
    vDir   := AbsoluteDirection;
    vRight := VectorCrossProduct(vDir, vUp);
  end;

  // save scale & position info
  Scale1    := obj.Scale.AsVector;
  position1 := obj.Position.AsVector;
  resMat    := obj.Matrix;
  // get rid of scaling & location info
  NormalizeMatrix(resMat);
  // Now we build rotation matrices and use them to rotate the obj
  if rollDelta <> 0 then
  begin
    SetVector(v, obj.AbsoluteToLocal(vDir));
    resMat := MatrixMultiply(CreateRotationMatrix(v, DegToRadian(rollDelta)), resMat);
  end;
  if turnDelta <> 0 then
  begin
    SetVector(v, obj.AbsoluteToLocal(vUp));
    resMat := MatrixMultiply(CreateRotationMatrix(v, DegToRadian(turnDelta)), resMat);
  end;
  if pitchDelta <> 0 then
  begin
    SetVector(v, obj.AbsoluteToLocal(vRight));
    resMat := MatrixMultiply(CreateRotationMatrix(v, DegToRadian(pitchDelta)), resMat);
  end;
  obj.Matrix := resMat;
  // restore scaling & rotation info
  obj.Scale.AsVector    := Scale1;
  obj.Position.AsVector := position1;
end;

procedure TDGLCamera.RotateTarget(pitchDelta, turnDelta: Single; rollDelta: Single = 0);
begin
  if Assigned(FTargetObject) then
    RotateObject(FTargetObject, pitchDelta, turnDelta, rollDelta)
end;

procedure TDGLCamera.MoveAroundTarget(pitchDelta, turnDelta: Single);
begin
  MoveObjectAround(FTargetObject, pitchDelta, turnDelta);
end;

procedure TDGLCamera.MoveAllAroundTarget(pitchDelta, turnDelta: Single);
begin
  MoveObjectAllAround(FTargetObject, pitchDelta, turnDelta);
end;

procedure TDGLCamera.MoveInEyeSpace(forwardDistance, rightDistance, upDistance: Single);
var
  trVector: TVector;
begin
  trVector := AbsoluteEyeSpaceVector(forwardDistance, rightDistance, upDistance);
  if Assigned(Parent) then
    Position.Translate(Parent.AbsoluteToLocal(trVector))
  else
    Position.Translate(trVector);
end;

procedure TDGLCamera.MoveTargetInEyeSpace(forwardDistance, rightDistance, upDistance: Single);
var
  trVector: TVector;
begin
  if TargetObject <> nil then
  begin
    trVector := AbsoluteEyeSpaceVector(forwardDistance, rightDistance, upDistance);
    TargetObject.Position.Translate(TargetObject.Parent.AbsoluteToLocal(trVector));
  end;
end;

function TDGLCamera.AbsoluteEyeSpaceVector(forwardDistance, rightDistance, upDistance: Single): TVector;
begin
  Result := NullHmgVector;
  if forwardDistance <> 0 then
    CombineVector(Result, AbsoluteVectorToTarget, forwardDistance);
  if rightDistance <> 0 then
    CombineVector(Result, AbsoluteRightVectorToTarget, rightDistance);
  if upDistance <> 0 then
    CombineVector(Result, AbsoluteUpVectorToTarget, upDistance);
end;

procedure TDGLCamera.AdjustDistanceToTarget(distanceRatio: Single);
var
  vect: TVector;
begin
  if Assigned(FTargetObject) then
  begin
    // calculate vector from target to camera in absolute coordinates
    vect := VectorSubtract(AbsolutePosition, TargetObject.AbsolutePosition);
    // ratio -> translation vector
    ScaleVector(vect, -(1 - distanceRatio));
    AddVector(vect, AbsolutePosition);
    if Assigned(Parent) then
      vect            := Parent.AbsoluteToLocal(vect);
    Position.AsVector := vect;
  end;
end;

function TDGLCamera.DistanceToTarget: Single;
var
  vect: TVector;
begin
  if Assigned(FTargetObject) then
  begin
    vect   := VectorSubtract(AbsolutePosition, TargetObject.AbsolutePosition);
    Result := VectorLength(vect);
  end
  else
    Result := 1;
end;

function TDGLCamera.ScreenDeltaToVector(deltaX, deltaY: Integer; ratio: Single; const planeNormal: TVector): TVector;
var
  screenY, screenX:           TVector;
  screenYoutOfPlaneComponent: Single;
begin
  // calculate projection of direction vector on the plane
  if Assigned(FTargetObject) then
    screenY := VectorSubtract(TargetObject.AbsolutePosition, AbsolutePosition)
  else
    screenY                  := Direction.AsVector;
  screenYoutOfPlaneComponent := VectorDotProduct(screenY, planeNormal);
  screenY                    := VectorCombine(screenY, planeNormal, 1, -screenYoutOfPlaneComponent);
  NormalizeVector(screenY);
  // calc the screenX vector
  screenX := VectorCrossProduct(screenY, planeNormal);
  // and here, we're done
  Result := VectorCombine(screenX, screenY, deltaX * ratio, deltaY * ratio);
end;

function TDGLCamera.ScreenDeltaToVectorXY(deltaX, deltaY: Integer; ratio: Single): TVector;
var
  screenY:     TVector;
  dxr, dyr, d: Single;
begin
  // calculate projection of direction vector on the plane
  if Assigned(FTargetObject) then
    screenY := VectorSubtract(TargetObject.AbsolutePosition, AbsolutePosition)
  else
    screenY := Direction.AsVector;
  d         := VectorLength(screenY.v[0], screenY.v[1]);
  if d <= 1E-10 then
    d := ratio
  else
    d := ratio / d;
  // and here, we're done
  dxr         := deltaX * d;
  dyr         := deltaY * d;
  Result.v[0] := screenY.v[1] * dxr + screenY.v[0] * dyr;
  Result.v[1] := screenY.v[1] * dyr - screenY.v[0] * dxr;
  Result.v[2] := 0;
  Result.v[3] := 0;
end;

function TDGLCamera.ScreenDeltaToVectorXZ(deltaX, deltaY: Integer; ratio: Single): TVector;
var
  screenY:     TVector;
  d, dxr, dzr: Single;
begin
  // calculate the projection of direction vector on the plane
  if Assigned(FTargetObject) then
    screenY := VectorSubtract(TargetObject.AbsolutePosition, AbsolutePosition)
  else
    screenY := Direction.AsVector;
  d         := VectorLength(screenY.v[0], screenY.v[2]);
  if d <= 1E-10 then
    d := ratio
  else
    d         := ratio / d;
  dxr         := deltaX * d;
  dzr         := deltaY * d;
  Result.v[0] := -screenY.v[2] * dxr + screenY.v[0] * dzr;
  Result.v[1] := 0;
  Result.v[2] := screenY.v[2] * dzr + screenY.v[0] * dxr;
  Result.v[3] := 0;
end;

function TDGLCamera.ScreenDeltaToVectorYZ(deltaX, deltaY: Integer; ratio: Single): TVector;
var
  screenY:     TVector;
  d, dyr, dzr: Single;
begin
  // calculate the projection of direction vector on the plane
  if Assigned(FTargetObject) then
    screenY := VectorSubtract(TargetObject.AbsolutePosition, AbsolutePosition)
  else
    screenY := Direction.AsVector;
  d         := VectorLength(screenY.v[1], screenY.v[2]);
  if d <= 1E-10 then
    d := ratio
  else
    d         := ratio / d;
  dyr         := deltaX * d;
  dzr         := deltaY * d;
  Result.v[0] := 0;
  Result.v[1] := screenY.v[2] * dyr + screenY.v[1] * dzr;
  Result.v[2] := screenY.v[2] * dzr - screenY.v[1] * dyr;
  Result.v[3] := 0;
end;

function TDGLCamera.PointInFront(const point: TVector): Boolean;
begin
  Result := PointIsInHalfSpace(point, AbsolutePosition, AbsoluteDirection);
end;

procedure TDGLCamera.SetDepthOfView(AValue: Single);
begin
  if FDepthOfView <> AValue then
  begin
    FDepthOfView := AValue;
    FFOVY        := -1;
    if not(csLoading in ComponentState) then
      TransformationChanged;
  end;
end;

procedure TDGLCamera.SetFocalLength(AValue: Single);
begin
  if AValue <= 0 then
    AValue := 1;
  if FFocalLength <> AValue then
  begin
    FFocalLength := AValue;
    FFOVY        := -1;
    if not(csLoading in ComponentState) then
      TransformationChanged;
  end;
end;

function TDGLCamera.GetFieldOfView(const AViewportDimension: Single): Single;
begin
  if FFocalLength = 0 then
    Result := 0
  else
    Result := RadToDeg(2 * ArcTan2(AViewportDimension * 0.5, FFocalLength));
end;

procedure TDGLCamera.SetFieldOfView(const AFieldOfView, AViewportDimension: Single);
begin
  FocalLength := AViewportDimension / (2 * Tan(DegToRadian(AFieldOfView / 2)));
end;

procedure TDGLCamera.SetCameraStyle(const val: TDGLCameraStyle);
begin
  if FCameraStyle <> val then
  begin
    FCameraStyle := val;
    FFOVY        := -1;
    NotifyChange(Self);
  end;
end;

procedure TDGLCamera.SetKeepFOVMode(const val: TDGLCameraKeepFOVMode);
begin
  if FKeepFOVMode <> val then
  begin
    FKeepFOVMode := val;
    FFOVY        := -1;
    if FCameraStyle = csPerspectiveKeepFOV then
      NotifyChange(Self);
  end;
end;

procedure TDGLCamera.SetSceneScale(value: Single);
begin
  if value = 0 then
    value := 1;
  if FSceneScale <> value then
  begin
    FSceneScale := value;
    FFOVY       := -1;
    NotifyChange(Self);
  end;
end;

function TDGLCamera.StoreSceneScale: Boolean;
begin
  Result := (FSceneScale <> 1);
end;

procedure TDGLCamera.SetNearPlaneBias(value: Single);
begin
  if value <= 0 then
    value := 1;
  if FNearPlaneBias <> value then
  begin
    FNearPlaneBias := value;
    FFOVY          := -1;
    NotifyChange(Self);
  end;
end;

function TDGLCamera.StoreNearPlaneBias: Boolean;
begin
  Result := (FNearPlaneBias <> 1);
end;

procedure TDGLCamera.DoRender(var ARci: TRenderContextInfo; ARenderSelf, ARenderChildren: Boolean);
begin
  if ARenderChildren and (Count > 0) then
    Self.RenderChildren(0, Count - 1, ARci);
end;

function TDGLCamera.RayCastIntersect(const rayStart, rayVector: TVector; intersectPoint: PVector = nil; intersectNormal: PVector = nil): Boolean;
begin
  Result := False;
end;

{$IFDEF GLS_REGIONS}{$ENDREGION}{$ENDIF}

// ------------------
{ TDGLRenderPoint }
{$IFDEF GLS_REGIONS}{$REGION 'TDGLRenderPoint'}{$ENDIF}

constructor TDGLRenderPoint.Create(AOwner: TComponent);
begin
  inherited;
  ObjectStyle := ObjectStyle + [osDirectDraw];
end;

destructor TDGLRenderPoint.Destroy;
begin
  Clear;
  inherited;
end;

procedure TDGLRenderPoint.BuildList(var rci: TRenderContextInfo);
var
  i: Integer;
begin
  for i := 0 to High(FCallBacks) do
    FCallBacks[i](Self, rci);
end;

procedure TDGLRenderPoint.RegisterCallBack(renderEvent: TDirectRenderEvent; renderPointFreed: TNotifyEvent);
var
  n: Integer;
begin
  n := Length(FCallBacks);
  SetLength(FCallBacks, n + 1);
  SetLength(FFreeCallBacks, n + 1);
  FCallBacks[n]     := renderEvent;
  FFreeCallBacks[n] := renderPointFreed;
end;

procedure TDGLRenderPoint.UnRegisterCallBack(renderEvent: TDirectRenderEvent);
type
  TEventContainer = record
    event: TDirectRenderEvent;
  end;
var
  i, j, n:                     Integer;
  refContainer, listContainer: TEventContainer;
begin
  refContainer.event := renderEvent;
  n                  := Length(FCallBacks);
  for i              := 0 to n - 1 do
  begin
    listContainer.event := FCallBacks[i];
    if CompareMem(@listContainer, @refContainer, SizeOf(TEventContainer)) then
    begin
      for j := i + 1 to n - 1 do
      begin
        FCallBacks[j - 1]     := FCallBacks[j];
        FFreeCallBacks[j - 1] := FFreeCallBacks[j];
      end;
      SetLength(FCallBacks, n - 1);
      SetLength(FFreeCallBacks, n - 1);
      Break;
    end;
  end;
end;

procedure TDGLRenderPoint.Clear;
begin
  while Length(FCallBacks) > 0 do
  begin
    FFreeCallBacks[High(FCallBacks)](Self);
    SetLength(FCallBacks, Length(FCallBacks) - 1);
  end;
end;


{$IFDEF GLS_REGIONS}{$ENDREGION}{$ENDIF}

// ------------------
{ TDGLProxyObject }
{$IFDEF GLS_REGIONS}{$REGION 'TDGLProxyObject'}{$ENDIF}

constructor TDGLProxyObject.Create(AOwner: TComponent);
begin
  inherited;
  FProxyOptions := cDefaultProxyOptions;
end;

destructor TDGLProxyObject.Destroy;
begin
  SetMasterObject(nil);
  inherited;
end;

procedure TDGLProxyObject.Assign(Source: TPersistent);
begin
  if Source is TDGLProxyObject then
  begin
    SetMasterObject(TDGLProxyObject(Source).MasterObject);
  end;
  inherited Assign(Source);
end;

procedure TDGLProxyObject.DoRender(var ARci: TRenderContextInfo; ARenderSelf, ARenderChildren: Boolean);
var
  gotMaster, masterGotEffects, oldProxySubObject: Boolean;
begin
  if FRendering then
    Exit;
  FRendering := True;
  try
    gotMaster        := assigned(FMasterObject);
    masterGotEffects := gotMaster and (pooEffects in FProxyOptions) and (FMasterObject.Effects.Count > 0);
    if gotMaster then
    begin
      if pooObjects in FProxyOptions then
      begin
        oldProxySubObject   := ARci.proxySubObject;
        ARci.proxySubObject := True;
        if pooTransformation in FProxyOptions then
          with ARci.PipelineTransformation do
            ModelMatrix := MatrixMultiply(FMasterObject.Matrix, ModelMatrix);
        FMasterObject.DoRender(ARci, ARenderSelf, (FMasterObject.Count > 0));
        ARci.proxySubObject := oldProxySubObject;
      end;
    end;
    // now render self stuff (our children, our effects, etc.)
    if ARenderChildren and (Count > 0) then
      Self.RenderChildren(0, Count - 1, ARci);
    if masterGotEffects then
      FMasterObject.Effects.RenderPostEffects(ARci);
  finally
    FRendering := False;
  end;
  ClearStructureChanged;
end;

function TDGLProxyObject.AxisAlignedDimensions: TVector;
begin
  If assigned(FMasterObject) then
  begin
    Result := FMasterObject.AxisAlignedDimensionsUnscaled;
    If (pooTransformation in ProxyOptions) then
      ScaleVector(Result, FMasterObject.Scale.AsVector)
    else
      ScaleVector(Result, Scale.AsVector);
  end
  else
    Result := inherited AxisAlignedDimensions;
end;

function TDGLProxyObject.AxisAlignedDimensionsUnscaled: TVector;
begin
  if assigned(FMasterObject) then
  begin
    Result := FMasterObject.AxisAlignedDimensionsUnscaled;
  end
  else
    Result := inherited AxisAlignedDimensionsUnscaled;
end;

function TDGLProxyObject.BarycenterAbsolutePosition: TVector;
var
  lAdjustVector: TVector;
begin
  if assigned(FMasterObject) then
  begin
    // Not entirely correct, but better than nothing...
    lAdjustVector     := VectorSubtract(FMasterObject.BarycenterAbsolutePosition, FMasterObject.AbsolutePosition);
    Position.AsVector := VectorAdd(Position.AsVector, lAdjustVector);
    Result            := AbsolutePosition;
    Position.AsVector := VectorSubtract(Position.AsVector, lAdjustVector);
  end
  else
    Result := inherited BarycenterAbsolutePosition;
end;

procedure TDGLProxyObject.Notification(AComponent: TComponent; Operation: TOperation);
begin
  if (Operation = opRemove) and (AComponent = FMasterObject) then
    MasterObject := nil;
  inherited;
end;

procedure TDGLProxyObject.SetMasterObject(const val: TDGLBaseSceneObject);
begin
  if FMasterObject <> val then
  begin
    if assigned(FMasterObject) then
      FMasterObject.RemoveFreeNotification(Self);
    FMasterObject := val;
    if assigned(FMasterObject) then
      FMasterObject.FreeNotification(Self);
    StructureChanged;
  end;
end;

procedure TDGLProxyObject.SetProxyOptions(const val: TDGLProxyObjectOptions);
begin
  if FProxyOptions <> val then
  begin
    FProxyOptions := val;
    StructureChanged;
  end;
end;

function TDGLProxyObject.RayCastIntersect(const rayStart, rayVector: TVector; intersectPoint: PVector = nil; intersectNormal: PVector = nil): Boolean;
var
  localRayStart, localRayVector: TVector;
begin
  if assigned(MasterObject) then
  begin
    SetVector(localRayStart, AbsoluteToLocal(rayStart));
    SetVector(localRayStart, MasterObject.LocalToAbsolute(localRayStart));
    SetVector(localRayVector, AbsoluteToLocal(rayVector));
    SetVector(localRayVector, MasterObject.LocalToAbsolute(localRayVector));
    NormalizeVector(localRayVector);

    Result := MasterObject.RayCastIntersect(localRayStart, localRayVector, intersectPoint, intersectNormal);
    if Result then
    begin
      if assigned(intersectPoint) then
      begin
        SetVector(intersectPoint^, MasterObject.AbsoluteToLocal(intersectPoint^));
        SetVector(intersectPoint^, LocalToAbsolute(intersectPoint^));
      end;
      if assigned(intersectNormal) then
      begin
        SetVector(intersectNormal^, MasterObject.AbsoluteToLocal(intersectNormal^));
        SetVector(intersectNormal^, LocalToAbsolute(intersectNormal^));
      end;
    end;
  end
  else
    Result := False;
end;

function TDGLProxyObject.GenerateSilhouette(const silhouetteParameters: TDGLSilhouetteParameters): TDGLSilhouette;
begin
  if assigned(MasterObject) then
    Result := MasterObject.GenerateSilhouette(silhouetteParameters)
  else
    Result := nil;
end;

{$IFDEF GLS_REGIONS}{$ENDREGION}{$ENDIF}

// ------------------
{ TDGLDirectOpenGL }
{$IFDEF GLS_REGIONS}{$REGION 'TDGLDirectOpenGL'}{$ENDIF}

constructor TDGLDirectOpenGL.Create(AOwner: TComponent);
begin
  inherited;
  ObjectStyle := ObjectStyle + [osDirectDraw];
  FBlend      := False;
end;

procedure TDGLDirectOpenGL.Assign(Source: TPersistent);
begin
  if Source is TDGLDirectOpenGL then
  begin
    UseBuildList := TDGLDirectOpenGL(Source).UseBuildList;
    FOnRender    := TDGLDirectOpenGL(Source).FOnRender;
    FBlend       := TDGLDirectOpenGL(Source).Blend;
  end;
  inherited Assign(Source);
end;

procedure TDGLDirectOpenGL.BuildList(var rci: TRenderContextInfo);
begin
  if Assigned(FOnRender) then
  begin
//    xgl.MapTexCoordToMain; // single texturing by default
    OnRender(Self, rci);
  end;
end;

function TDGLDirectOpenGL.AxisAlignedDimensionsUnscaled: TVector;
begin
  Result := NullHmgPoint;
end;

procedure TDGLDirectOpenGL.SetUseBuildList(const val: Boolean);
begin
  if val <> FUseBuildList then
  begin
    FUseBuildList := val;
    if val then
      ObjectStyle := ObjectStyle - [osDirectDraw]
    else
      ObjectStyle := ObjectStyle + [osDirectDraw];
  end;
end;

function TDGLDirectOpenGL.Blended: Boolean;
begin
  Result := FBlend;
end;

procedure TDGLDirectOpenGL.SetBlend(const val: Boolean);
begin
  if val <> FBlend then
  begin
    FBlend := val;
    StructureChanged;
  end;
end;

{$IFDEF GLS_REGIONS}{$ENDREGION}{$ENDIF}

// ------------------
{ TDGLLightSource }
{$IFDEF GLS_REGIONS}{$REGION 'TDGLLightSource'}{$ENDIF}

constructor TDGLLightSource.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FShining              := True;
  FSpotDirection        := TDGLCoordinates.CreateInitialized(Self, VectorMake(0, 0, -1, 0), csVector);
  FConstAttenuation     := 1;
  FLinearAttenuation    := 0;
  FQuadraticAttenuation := 0;
  FSpotCutOff           := 180;
  FSpotExponent         := 0;
  FLightStyle           := lsSpot;
  FAmbient              := TDGLColor.Create(Self);
  FDiffuse              := TDGLColor.Create(Self);
  FDiffuse.Initialize(clrWhite);
  FSpecular := TDGLColor.Create(Self);
end;

destructor TDGLLightSource.Destroy;
begin
  FSpotDirection.Free;
  FAmbient.Free;
  FDiffuse.Free;
  FSpecular.Free;
  inherited Destroy;
end;

procedure TDGLLightSource.DoRender(var ARci: TRenderContextInfo; ARenderSelf, ARenderChildren: Boolean);
begin
  if ARenderChildren and Assigned(FChildren) then
    Self.RenderChildren(0, Count - 1, ARci);
end;

function TDGLLightSource.RayCastIntersect(const rayStart, rayVector: TVector; intersectPoint: PVector = nil; intersectNormal: PVector = nil): Boolean;
begin
  Result := False;
end;

procedure TDGLLightSource.CoordinateChanged(Sender: TDGLCustomCoordinates);
begin
  inherited;
  if Sender = FSpotDirection then
    TransformationChanged;
end;

function TDGLLightSource.GenerateSilhouette(const silhouetteParameters: TDGLSilhouetteParameters): TDGLSilhouette;
begin
  Result := nil;
end;

function TDGLLightSource.GetHandle(var rci: TRenderContextInfo): Cardinal;
begin
  Result := 0;
end;

procedure TDGLLightSource.SetShining(AValue: Boolean);
begin
  if AValue <> FShining then
  begin
    FShining := AValue;
    NotifyChange(Self);
  end;
end;

procedure TDGLLightSource.SetSpotDirection(AVector: TDGLCoordinates);
begin
  FSpotDirection.DirectVector := AVector.AsVector;
  FSpotDirection.W            := 0;
  NotifyChange(Self);
end;

procedure TDGLLightSource.SetSpotExponent(AValue: Single);
begin
  if FSpotExponent <> AValue then
  begin
    FSpotExponent := AValue;
    NotifyChange(Self);
  end;
end;

procedure TDGLLightSource.SetSpotCutOff(const val: Single);
begin
  if FSpotCutOff <> val then
  begin
    if ((val >= 0) and (val <= 90)) or (val = 180) then
    begin
      FSpotCutOff := val;
      NotifyChange(Self);
    end;
  end;
end;

procedure TDGLLightSource.SetLightStyle(const val: TLightStyle);
begin
  if FLightStyle <> val then
  begin
    FLightStyle := val;
    NotifyChange(Self);
  end;
end;

procedure TDGLLightSource.SetAmbient(AValue: TDGLColor);
begin
  FAmbient.Color := AValue.Color;
  NotifyChange(Self);
end;

procedure TDGLLightSource.SetDiffuse(AValue: TDGLColor);
begin
  FDiffuse.Color := AValue.Color;
  NotifyChange(Self);
end;

procedure TDGLLightSource.SetSpecular(AValue: TDGLColor);
begin
  FSpecular.Color := AValue.Color;
  NotifyChange(Self);
end;

procedure TDGLLightSource.SetConstAttenuation(AValue: Single);
begin
  if FConstAttenuation <> AValue then
  begin
    FConstAttenuation := AValue;
    NotifyChange(Self);
  end;
end;

procedure TDGLLightSource.SetLinearAttenuation(AValue: Single);
begin
  if FLinearAttenuation <> AValue then
  begin
    FLinearAttenuation := AValue;
    NotifyChange(Self);
  end;
end;

procedure TDGLLightSource.SetQuadraticAttenuation(AValue: Single);
begin
  if FQuadraticAttenuation <> AValue then
  begin
    FQuadraticAttenuation := AValue;
    NotifyChange(Self);
  end;
end;

function TDGLLightSource.Attenuated: Boolean;
begin
  Result := (LightStyle <> lsParallel) and ((ConstAttenuation <> 1) or (LinearAttenuation <> 0) or (QuadraticAttenuation <> 0));
end;

{$IFDEF GLS_REGIONS}{$ENDREGION}{$ENDIF}

// ------------------
{ TDGLScene }
{$IFDEF GLS_REGIONS}{$REGION 'TDGLScene'}{$ENDIF}


constructor TDGLScene.Create(AOwner: TComponent);
begin
  inherited;
  // root creation
  FCurrentBuffer     := nil;
  FObjects           := TDGLSceneRootObject.Create(Self);
  FObjects.Name      := 'ObjectRoot';
  FLights            := TDGLPersistentObjectList.Create;
  FObjectsSorting    := osRenderBlendedLast;
  FVisibilityCulling := vcNone;
  // actual maximum number of lights is stored in TDGLSceneViewer
  FLights.Count         := 8;
  FInitializableObjects := TDGLInitializableObjectList.Create;
end;

destructor TDGLScene.Destroy;
begin
  InitializableObjects.Free;
  FObjects.DestroyHandles;
  FLights.Free;
  FObjects.Free;
  if Assigned(FBuffers) then
    FreeAndNil(FBuffers);
  inherited Destroy;
end;

procedure TDGLScene.AddLight(aLight: TDGLLightSource);
var
  i: Integer;
begin
  for i := 0 to FLights.Count - 1 do
    if FLights.List^[i] = nil then
    begin
      FLights.List^[i] := aLight;
      aLight.FLightID  := i;
      Break;
    end;
end;

procedure TDGLScene.RemoveLight(aLight: TDGLLightSource);
var
  idx: Integer;
begin
  idx := FLights.IndexOf(aLight);
  if idx >= 0 then
    FLights[idx] := nil;
end;

procedure TDGLScene.AddLights(anObj: TDGLBaseSceneObject);
var
  i: Integer;
begin
  if anObj is TDGLLightSource then
    AddLight(TDGLLightSource(anObj));
  for i := 0 to anObj.Count - 1 do
    AddLights(anObj.Children[i]);
end;

procedure TDGLScene.RemoveLights(anObj: TDGLBaseSceneObject);
var
  i: Integer;
begin
  if anObj is TDGLLightSource then
    RemoveLight(TDGLLightSource(anObj));
  for i := 0 to anObj.Count - 1 do
    RemoveLights(anObj.Children[i]);
end;

procedure TDGLScene.ShutdownAllLights;

  procedure DoShutdownLight(Obj: TDGLBaseSceneObject);
  var
    i: Integer;
  begin
    if Obj is TDGLLightSource then
      TDGLLightSource(Obj).Shining := False;
    for i                         := 0 to Obj.Count - 1 do
      DoShutdownLight(Obj[i]);
  end;

begin
  DoShutdownLight(FObjects);
end;

procedure TDGLScene.AddBuffer(aBuffer: TDGLSceneBuffer);
begin
  if not Assigned(FBuffers) then
    FBuffers := TDGLPersistentObjectList.Create;
  if FBuffers.IndexOf(aBuffer) < 0 then
  begin
    FBuffers.Add(aBuffer);
    if FBaseContext = nil then
      FBaseContext := TDGLSceneBuffer(FBuffers[0]).RenderingContext;
    if (FBuffers.Count > 1) and Assigned(FBaseContext) then
      aBuffer.RenderingContext.ShareLists(FBaseContext);
  end;
end;

procedure TDGLScene.RemoveBuffer(aBuffer: TDGLSceneBuffer);
var
  i: Integer;
begin
  if Assigned(FBuffers) then
  begin
    i := FBuffers.IndexOf(aBuffer);
    if i >= 0 then
    begin
      if FBuffers.Count = 1 then
      begin
        FreeAndNil(FBuffers);
        FBaseContext := nil;
      end
      else
      begin
        FBuffers.Delete(i);
        FBaseContext := TDGLSceneBuffer(FBuffers[0]).RenderingContext;
      end;
    end;
  end;
end;

procedure TDGLScene.GetChildren(AProc: TGetChildProc; Root: TComponent);
begin
  FObjects.GetChildren(AProc, Root);
end;

procedure TDGLScene.SetChildOrder(AChild: TComponent; Order: Integer);
begin
  (AChild as TDGLBaseSceneObject).Index := Order;
end;

function TDGLScene.IsUpdating: Boolean;
begin
  Result := (FUpdateCount <> 0) or (csLoading in ComponentState) or (csDestroying in ComponentState);
end;

procedure TDGLScene.BeginUpdate;
begin
  Inc(FUpdateCount);
end;

procedure TDGLScene.EndUpdate;
begin
  Assert(FUpdateCount > 0);
  Dec(FUpdateCount);
  if FUpdateCount = 0 then
    NotifyChange(Self);
end;

procedure TDGLScene.SetObjectsSorting(const val: TDGLObjectsSorting);
begin
  if FObjectsSorting <> val then
  begin
    if val = osInherited then
      FObjectsSorting := osRenderBlendedLast
    else
      FObjectsSorting := val;
    NotifyChange(Self);
  end;
end;

procedure TDGLScene.SetVisibilityCulling(const val: TDGLVisibilityCulling);
begin
  if FVisibilityCulling <> val then
  begin
    if val = vcInherited then
      FVisibilityCulling := vcNone
    else
      FVisibilityCulling := val;
    NotifyChange(Self);
  end;
end;

procedure TDGLScene.ReadState(Reader: TReader);
var
  SaveRoot: TComponent;
begin
  SaveRoot := Reader.Root;
  try
    if Owner <> nil then
      Reader.Root := Owner;
    inherited;
  finally
    Reader.Root := SaveRoot;
  end;
end;

procedure TDGLScene.Progress(const deltaTime, newTime: Double);
var
  pt: TProgressTimes;
begin
  pt.deltaTime      := deltaTime;
  pt.newTime        := newTime;
  FCurrentDeltaTime := deltaTime;
  if Assigned(FOnBeforeProgress) then
    FOnBeforeProgress(Self, deltaTime, newTime);
  FObjects.DoProgress(pt);
  if Assigned(FOnProgress) then
    FOnProgress(Self, deltaTime, newTime);
end;

procedure TDGLScene.SaveToFile(const fileName: string);
var
  stream: TStream;
begin
  stream := CreateFileStream(fileName, fmCreate);
  try
    SaveToStream(stream);
  finally
    stream.Free;
  end;
end;

procedure TDGLScene.LoadFromFile(const fileName: string);

  procedure CheckResFileStream(stream: TStream);
  var
    N: Integer;
    B: Byte;
  begin
    N := stream.Position;
    stream.Read(B, Sizeof(B));
    stream.Position := N;
    if B = $FF then
      stream.ReadResHeader;
  end;

var
  stream: TStream;
begin
  stream := CreateFileStream(fileName, fmOpenRead);
  try
    CheckResFileStream(stream);
    LoadFromStream(stream);
  finally
    stream.Free;
  end;
end;

procedure TDGLScene.SaveToTextFile(const fileName: string);
var
  mem: TMemoryStream;
  fil: TStream;
begin
  mem := TMemoryStream.Create;
  fil := CreateFileStream(fileName, fmCreate);
  try
    SaveToStream(mem);
    mem.Position := 0;
    ObjectBinaryToText(mem, fil);
  finally
    fil.Free;
    mem.Free;
  end;
end;

procedure TDGLScene.LoadFromTextFile(const fileName: string);
var
  mem: TMemoryStream;
  fil: TStream;
begin
  mem := TMemoryStream.Create;
  fil := CreateFileStream(fileName, fmOpenRead);
  try
    ObjectTextToBinary(fil, mem);
    mem.Position := 0;
    LoadFromStream(mem);
  finally
    fil.Free;
    mem.Free;
  end;
end;

procedure TDGLScene.LoadFromStream(aStream: TStream);
var
  fixups: TStringList;
  i:      Integer;
  Obj:    TDGLBaseSceneObject;
begin
  fixups := TStringList.Create;
  try
    if Assigned(FBuffers) then
    begin
      for i := 0 to FBuffers.Count - 1 do
        fixups.AddObject(TDGLSceneBuffer(FBuffers[i]).Camera.Name, FBuffers[i]);
    end;
    ShutdownAllLights;
    // will remove Viewer from FBuffers
    Objects.DeleteChildren;
    aStream.ReadComponent(Self);
    for i := 0 to fixups.Count - 1 do
    begin
      Obj := FindSceneObject(fixups[i]);
      if Obj is TDGLCamera then
        TDGLSceneBuffer(fixups.Objects[i]).Camera := TDGLCamera(Obj)
      else { can assign default camera (if existing, of course) instead }
          ;
    end;
  finally
    fixups.Free;
  end;
end;

procedure TDGLScene.SaveToStream(aStream: TStream);
begin
  aStream.WriteComponent(Self);
end;

function TDGLScene.FindSceneObject(const AName: string): TDGLBaseSceneObject;
begin
  Result := FObjects.FindChild(AName, False);
end;

function TDGLScene.RayCastIntersect(const rayStart, rayVector: TVector; intersectPoint: PVector = nil; intersectNormal: PVector = nil): TDGLBaseSceneObject;
var
  bestDist2:       Single;
  bestHit:         TDGLBaseSceneObject;
  iPoint, iNormal: TVector;
  pINormal:        PVector;

  function RecursiveDive(baseObject: TDGLBaseSceneObject): TDGLBaseSceneObject;
  var
    i:           Integer;
    curObj:      TDGLBaseSceneObject;
    dist2:       Single;
    fNear, fFar: Single;
  begin
    Result := nil;
    for i  := 0 to baseObject.Count - 1 do
    begin
      curObj := baseObject.Children[i];
      if curObj.Visible then
      begin
        if RayCastAABBIntersect(rayStart, rayVector, curObj.AxisAlignedBoundingBoxAbsoluteEx, fNear, fFar) then
        begin
          if fNear * fNear > bestDist2 then
          begin
            if not PointInAABB(rayStart, curObj.AxisAlignedBoundingBoxAbsoluteEx) then
              continue;
          end;
          if curObj.RayCastIntersect(rayStart, rayVector, @iPoint, pINormal) then
          begin
            dist2 := VectorDistance2(rayStart, iPoint);
            if dist2 < bestDist2 then
            begin
              bestHit   := curObj;
              bestDist2 := dist2;
              if Assigned(intersectPoint) then
                intersectPoint^ := iPoint;
              if Assigned(intersectNormal) then
                intersectNormal^ := iNormal;
            end;
          end;
          RecursiveDive(curObj);
        end;
      end;
    end;
  end;

begin
  bestDist2 := 1E20;
  bestHit   := nil;
  if Assigned(intersectNormal) then
    pINormal := @iNormal
  else
    pINormal := nil;
  RecursiveDive(Objects);
  Result := bestHit;
end;

procedure TDGLScene.NotifyChange(Sender: TObject);
var
  i: Integer;
begin
  if (not IsUpdating) and Assigned(FBuffers) then
    for i := 0 to FBuffers.Count - 1 do
      TDGLSceneBuffer(FBuffers[i]).NotifyChange(Self);
end;

procedure TDGLScene.SetupLights(maxLights: Integer);
var
  i:           Integer;
  lightSource: TDGLLightSource;
  nbLights:    Integer;
  lPos:        TVector;
begin
  nbLights := FLights.Count;
  if nbLights > maxLights then
    nbLights := maxLights;
  // setup all light sources
  with CurrentDGLContext.GLStates, CurrentDGLContext.PipelineTransformation do
  begin
    for i := 0 to nbLights - 1 do
    begin
      lightSource := TDGLLightSource(FLights[i]);
      if Assigned(lightSource) then
        with lightSource do
        begin
          LightEnabling[FLightID] := Shining;
          if Shining then
          begin
            if FixedFunctionPipeLight then
            begin
              RebuildMatrix;
              if LightStyle in [lsParallel, lsParallelSpot] then
              begin
                ModelMatrix := AbsoluteMatrix;
//                glLightfv(GL_LIGHT0 + FLightID, GL_POSITION, SpotDirection.AsAddress);
              end
              else
              begin
                ModelMatrix := Parent.AbsoluteMatrix;
//                glLightfv(GL_LIGHT0 + FLightID, GL_POSITION, Position.AsAddress);
              end;
              if LightStyle in [lsSpot, lsParallelSpot] then
              begin
                if FSpotCutOff <> 180 then
//                  glLightfv(GL_LIGHT0 + FLightID, GL_SPOT_DIRECTION, FSpotDirection.AsAddress);
              end;
            end;

            lPos := lightSource.AbsolutePosition;
            if LightStyle in [lsParallel, lsParallelSpot] then
              lPos.V[3] := 0.0
            else
              lPos.V[3]                  := 1.0;
            LightPosition[FLightID]      := lPos;
            LightSpotDirection[FLightID] := lightSource.SpotDirection.AsAffineVector;

            LightAmbient[FLightID]  := FAmbient.Color;
            LightDiffuse[FLightID]  := FDiffuse.Color;
            LightSpecular[FLightID] := FSpecular.Color;

            LightConstantAtten[FLightID]  := FConstAttenuation;
            LightLinearAtten[FLightID]    := FLinearAttenuation;
            LightQuadraticAtten[FLightID] := FQuadraticAttenuation;

            LightSpotExponent[FLightID] := FSpotExponent;
            LightSpotCutoff[FLightID]   := FSpotCutOff;
          end;
        end
      else
        LightEnabling[i] := False;
    end;
    // turn off other lights
    for i              := nbLights to maxLights - 1 do
      LightEnabling[i] := False;
    ModelMatrix        := IdentityHmgMatrix;
  end;
end;

{$IFDEF GLS_REGIONS}{$ENDREGION}{$ENDIF}

// ------------------
{ TDGLFogEnvironment }
{$IFDEF GLS_REGIONS}{$REGION 'TDGLFogEnvironment'}{$ENDIF}

// Note: The fog implementation is not conformal with the rest of the scene management
// because it is Shader bound not scene bound.

constructor TDGLFogEnvironment.Create(AOwner: TPersistent);
begin
  inherited;
  FSceneBuffer := (AOwner as TDGLSceneBuffer);
  FFogColor    := TDGLColor.CreateInitialized(Self, clrBlack);
  FFogMode     := fmLinear;
  FFogStart    := 10;
  FFogEnd      := 1000;
  FFogDistance := fdDefault;
end;

destructor TDGLFogEnvironment.Destroy;
begin
  FFogColor.Free;
  inherited Destroy;
end;

procedure TDGLFogEnvironment.SetFogColor(Value: TDGLColor);
begin
  if Assigned(Value) then
  begin
    FFogColor.Assign(Value);
    NotifyChange(Self);
  end;
end;

procedure TDGLFogEnvironment.SetFogStart(Value: Single);
begin
  if Value <> FFogStart then
  begin
    FFogStart := Value;
    NotifyChange(Self);
  end;
end;

procedure TDGLFogEnvironment.SetFogEnd(Value: Single);
begin
  if Value <> FFogEnd then
  begin
    FFogEnd := Value;
    NotifyChange(Self);
  end;
end;

procedure TDGLFogEnvironment.Assign(Source: TPersistent);
begin
  if Source is TDGLFogEnvironment then
  begin
    FFogColor.Assign(TDGLFogEnvironment(Source).FFogColor);
    FFogStart    := TDGLFogEnvironment(Source).FFogStart;
    FFogEnd      := TDGLFogEnvironment(Source).FFogEnd;
    FFogMode     := TDGLFogEnvironment(Source).FFogMode;
    FFogDistance := TDGLFogEnvironment(Source).FFogDistance;
    NotifyChange(Self);
  end;
  inherited;
end;

function TDGLFogEnvironment.IsAtDefaultValues: Boolean;
begin
  Result := VectorEquals(FogColor.Color, FogColor.DefaultColor) and (FogStart = 10) and (FogEnd = 1000) and (FogMode = fmLinear) and (FogDistance = fdDefault);
end;

procedure TDGLFogEnvironment.SetFogMode(Value: TFogMode);
begin
  if Value <> FFogMode then
  begin
    FFogMode := Value;
    NotifyChange(Self);
  end;
end;

procedure TDGLFogEnvironment.SetFogDistance(const val: TFogDistance);
begin
  if val <> FFogDistance then
  begin
    FFogDistance := val;
    NotifyChange(Self);
  end;
end;

var
  vImplemDependantFogDistanceDefault: Integer = -1;

procedure TDGLFogEnvironment.ApplyFog;
//var
//  tempActivation: Boolean;
begin
//  with FSceneBuffer do
//  begin
//    if not Assigned(FRenderingContext) then
//      Exit;
//    tempActivation := not FRenderingContext.Active;
//    if tempActivation then
//      FRenderingContext.Activate;
//  end;

//  case FFogMode of
//    fmLinear:
////      GLFogi(GL_FOG_MODE, GL_LINEAR);
//    fmExp:
//      begin
//        GLFogi(GL_FOG_MODE, GL_EXP);
//        GLFogf(GL_FOG_DENSITY, FFogColor.alpha);
//      end;
//    fmExp2:
//      begin
//        GLFogi(GL_FOG_MODE, GL_EXP2);
//        GLFogf(GL_FOG_DENSITY, FFogColor.alpha);
//      end;
//  end;
//  GLFogfv(GL_FOG_COLOR, FFogColor.AsAddress);
//  GLFogf(GL_FOG_START, FFogStart);
//  GLFogf(GL_FOG_END, FFogEnd);
//  if dglCheckExtension('GL.NV_fog_distance') then
//  begin
//    case FogDistance of
//      fdDefault:
//        begin
//          if vImplemDependantFogDistanceDefault = -1 then
//            glGetIntegerv(GL_FOG_DISTANCE_MODE_NV, @vImplemDependantFogDistanceDefault)
////          else
////            GLFogi(GL_FOG_DISTANCE_MODE_NV, vImplemDependantFogDistanceDefault);
//        end;
//      fdEyePlane:
//        GLFogi(GL_FOG_DISTANCE_MODE_NV, GL_EYE_PLANE_ABSOLUTE_NV);
//      fdEyeRadial:
//        GLFogi(GL_FOG_DISTANCE_MODE_NV, GL_EYE_RADIAL_NV);
//    else
//      Assert(False);
//    end;
//  end;
//
//  if tempActivation then
//    FSceneBuffer.RenderingContext.Deactivate;
end;

{$IFDEF GLS_REGIONS}{$ENDREGION}{$ENDIF}

// ------------------
{ TDGLSceneBuffer }
{$IFDEF GLS_REGIONS}{$REGION 'TDGLSceneBuffer'}{$ENDIF}

constructor TDGLSceneBuffer.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner);

  // initialize private state variables
  FFogEnvironment     := TDGLFogEnvironment.Create(Self);
  FBackgroundColor    := clBtnFace;
  FBackgroundAlpha    := 1;
  FAmbientColor       := TDGLColor.CreateInitialized(Self, clrGray20);
  FDepthTest          := True;
  FFaceCulling        := True;
  FLighting           := True;
  FAntiAliasing       := aaDefault;
  FDepthPrecision     := dpDefault;
  FColorDepth         := cdDefault;
  FShadeModel         := smDefault;
  FFogEnable          := False;
  FLayer              := clMainPlane;
  FAfterRenderEffects := TDGLPersistentObjectList.Create;

  FContextOptions := [roDoubleBuffer, roRenderToWindow, roDebugContext];

  ResetPerformanceMonitor;
end;

destructor TDGLSceneBuffer.Destroy;
begin
  Melt;
  DestroyRC;
  FAmbientColor.Free;
  FAfterRenderEffects.Free;
  FFogEnvironment.Free;
  inherited Destroy;
end;

procedure TDGLSceneBuffer.PrepareGLContext;
begin
  if Assigned(FOnPrepareGLContext) then
    FOnPrepareGLContext(Self);
end;

procedure TDGLSceneBuffer.SetupRCOptions(context: TDGLContext);
const
  cColorDepthToColorBits: array [cdDefault .. cdFloat128bits] of Integer = (24, 8, 16, 24, 64, 128); // float_type
  cDepthPrecisionToDepthBits: array [dpDefault .. dp32bits] of Integer   = (24, 16, 24, 32);
var
  locOptions:                                 TDGLRCOptions;
  locStencilBits, locAlphaBits, locColorBits: Integer;
begin
  locOptions := [];

  if roDoubleBuffer in ContextOptions then
    locOptions := locOptions + [rcoDoubleBuffered];
  if roStereo in ContextOptions then
    locOptions := locOptions + [rcoStereo];
  if roDebugContext in ContextOptions then
    locOptions := locOptions + [rcoDebug];
  if roOpenGL_ES2_Context in ContextOptions then
    locOptions := locOptions + [rcoOGL_ES];
  if roNoColorBuffer in ContextOptions then
    locColorBits := 0
  else
    locColorBits := cColorDepthToColorBits[ColorDepth];
  if roStencilBuffer in ContextOptions then
    locStencilBits := 8
  else
    locStencilBits := 0;
  if roDestinationAlpha in ContextOptions then
    locAlphaBits := 8
  else
    locAlphaBits := 0;
  with context do
  begin
    Options                 := locOptions;
    ColorBits               := locColorBits;
    DepthBits               := cDepthPrecisionToDepthBits[DepthPrecision];
    StencilBits             := locStencilBits;
    AlphaBits               := locAlphaBits;
    AccumBits               := AccumBufferBits;
    AuxBuffers              := 0;
    AntiAliasing            := Self.AntiAliasing;
    Layer                   := Self.Layer;
//    GLStates.ForwardContext := roForwardContext in ContextOptions;
    PrepareGLContext;
  end;
end;

procedure TDGLSceneBuffer.CreateRC(AWindowHandle: HWND; memoryContext: Boolean; BufferCount: Integer);
begin
  DestroyRC;
  FRendering := True;

  try
    // will be freed in DestroyWindowHandle
    FRenderingContext := DGLContextManager.CreateContext;
    if not Assigned(FRenderingContext) then
      raise Exception.Create('Failed to create RenderingContext.');
    SetupRCOptions(FRenderingContext);

    if Assigned(FCamera) and Assigned(FCamera.FScene) then
      FCamera.FScene.AddBuffer(Self);

    with FRenderingContext do
    begin
      try
        if memoryContext then
          CreateMemoryContext(AWindowHandle, FViewPort.Width, FViewPort.Height, BufferCount)
        else
          CreateContext(AWindowHandle);
      except
        FreeAndNil(FRenderingContext);
        raise;
      end;
    end;
    FRenderingContext.Activate;
    try
      // this one should NOT be replaced with an assert
      if not GL_VERSION_3_3 then
      begin
        DGLSLogger.LogFatalError(glsWrongVersion);
        Abort;
      end;
      // define viewport, this is necessary because the first WM_SIZE message
      // is posted before the rendering context has been created
      FRenderingContext.GLStates.ViewPort := Vector4iMake(FViewPort.Left, FViewPort.Top, FViewPort.Width, FViewPort.Height);
      // set up initial context states
      SetupRenderingContext(FRenderingContext);
      FRenderingContext.GLStates.ColorClearValue := ConvertWinColor(FBackgroundColor);
    finally
      FRenderingContext.Deactivate;
    end;
  finally
    FRendering := False;
  end;
end;

procedure TDGLSceneBuffer.DestroyRC;
begin
  if Assigned(FRenderingContext) then
  begin
    Melt;
    // for some obscure reason, Mesa3D doesn't like this call... any help welcome
//    FreeAndNil(FSelector);
    FreeAndNil(FRenderingContext);
    if Assigned(FCamera) and Assigned(FCamera.FScene) then
      FCamera.FScene.RemoveBuffer(Self);
  end;
end;

function TDGLSceneBuffer.RCInstantiated: Boolean;
begin
  Result := Assigned(FRenderingContext);
end;

procedure TDGLSceneBuffer.Resize(newLeft, newTop, newWidth, newHeight: Integer);
begin
  if newWidth < 1 then
    newWidth := 1;
  if newHeight < 1 then
    newHeight      := 1;
  FViewPort.Left   := newLeft;
  FViewPort.Top    := newTop;
  FViewPort.Width  := newWidth;
  FViewPort.Height := newHeight;
  if Assigned(FRenderingContext) then
  begin
    FRenderingContext.Activate;
    try
      // Part of workaround for MS OpenGL "black borders" bug
      FRenderingContext.GLStates.ViewPort := Vector4iMake(FViewPort.Left, FViewPort.Top, FViewPort.Width, FViewPort.Height);
    finally
      FRenderingContext.Deactivate;
    end;
  end;
end;

// Acceleration
//

// function TDGLSceneBuffer.Acceleration: TDGLContextAcceleration;
// begin
// if Assigned(FRenderingContext) then
// Result := FRenderingContext.Acceleration
// else
// Result := chaUnknown;
// end;

// SetupRenderingContext
//

procedure TDGLSceneBuffer.SetupRenderingContext(context: TDGLContext);

  procedure SetState(bool: Boolean; csState: TDGLState);
  begin
    case bool of
      True:
        context.GLStates.PerformEnable(csState);
      False:
        context.GLStates.PerformDisable(csState);
    end;
  end;

//var
//  LColorDepth: Cardinal;
begin
  if not Assigned(context) then Exit;

//  if not(roForwardContext in ContextOptions) then
//  begin
////    GLLightModelfv(GL_LIGHT_MODEL_AMBIENT, FAmbientColor.AsAddress);
//    if roTwoSideLighting in FContextOptions then
//      GLLightModeli(GL_LIGHT_MODEL_TWO_SIDE, GL_TRUE)
//    else
//      GLLightModeli(GL_LIGHT_MODEL_TWO_SIDE, GL_FALSE);
//    GLHint(GL_PERSPECTIVE_CORRECTION_HINT, GL_NICEST);
//    case ShadeModel of
//      smDefault, smSmooth:
//        GLShadeModel(GL_SMOOTH);
//      smFlat:
//        GLShadeModel(GL_FLAT);
//    else
//      Assert(False, glsErrorEx + glsUnknownType);
//    end;
//  end;

  with context.GLStates do
  begin
    // Enable(stNormalize);
    SetState(DepthTest, stDepthTest);
    SetState(FaceCulling, stCullFace);
    // SetState(Lighting, stLighting);
    // SetState(FogEnable, stFog);
    if dglCheckExtension('ARB_depth_clamp') then  Disable(stDepthClamp);
//    if not(roForwardContext in ContextOptions) then
//    begin
//      glGetIntegerv(GL_BLUE_BITS, @LColorDepth); // could've used red or green too
//      SetState((LColorDepth < 8), stDither);
//    end;
    //ResetAllGLTextureMatrix;
  end;
end;


function TDGLSceneBuffer.GetLimit(Which: TLimitType): Integer;
var
  VP: array [0 .. 1] of Double;
begin
  case Which of
//    limClipPlanes:
//      glGetIntegerv(GL_MAX_CLIP_PLANES, @Result);
//    limEvalOrder:
//      glGetIntegerv(GL_MAX_EVAL_ORDER, @Result);
//    limLights:
//      glGetIntegerv(GL_MAX_LIGHTS, @Result);
//    limListNesting:
//      glGetIntegerv(GL_MAX_LIST_NESTING, @Result);
//    limModelViewStack:
//      glGetIntegerv(GL_MAX_MODELVIEW_STACK_DEPTH, @Result);
//    limNameStack:
//      glGetIntegerv(GL_MAX_NAME_STACK_DEPTH, @Result);
//    limPixelMapTable:
//      glGetIntegerv(GL_MAX_PIXEL_MAP_TABLE, @Result);
//    limProjectionStack:
//      glGetIntegerv(GL_MAX_PROJECTION_STACK_DEPTH, @Result);
    limTextureSize:
      glGetIntegerv(GL_MAX_TEXTURE_SIZE, @Result);
//    limTextureStack:
//      glGetIntegerv(GL_MAX_TEXTURE_STACK_DEPTH, @Result);
    limViewportDims:
      begin
        GLGetDoublev(GL_MAX_VIEWPORT_DIMS, @VP);
        if VP[0] > VP[1] then
          Result := Round(VP[0])
        else
          Result := Round(VP[1]);
      end;
//    limAccumAlphaBits:
//      glGetIntegerv(GL_ACCUM_ALPHA_BITS, @Result);
//    limAccumBlueBits:
//      glGetIntegerv(GL_ACCUM_BLUE_BITS, @Result);
//    limAccumGreenBits:
//      glGetIntegerv(GL_ACCUM_GREEN_BITS, @Result);
//    limAccumRedBits:
//      glGetIntegerv(GL_ACCUM_RED_BITS, @Result);
//    limAlphaBits:
//      glGetIntegerv(GL_ALPHA_BITS, @Result);
//    limAuxBuffers:
//      glGetIntegerv(GL_AUX_BUFFERS, @Result);
//    limDepthBits:
//      glGetIntegerv(GL_DEPTH_BITS, @Result);
//    limStencilBits:
//      glGetIntegerv(GL_STENCIL_BITS, @Result);
//    limBlueBits:
//      glGetIntegerv(GL_BLUE_BITS, @Result);
//    limGreenBits:
//      glGetIntegerv(GL_GREEN_BITS, @Result);
//    limRedBits:
//      glGetIntegerv(GL_RED_BITS, @Result);
//    limIndexBits:
//      glGetIntegerv(GL_INDEX_BITS, @Result);
    limStereo:
      glGetIntegerv(GL_STEREO, @Result);
    limDoubleBuffer:
      glGetIntegerv(GL_DOUBLEBUFFER, @Result);
    limSubpixelBits:
      glGetIntegerv(GL_SUBPIXEL_BITS, @Result);
    limNbTextureUnits:
      if dglCheckExtension('ARB_multitexture') then
        glGetIntegerv(GL_MAX_TEXTURE_UNITS_ARB, @Result)
      else
        Result := 1;
  else
    Result := 0;
  end;
end;

procedure TDGLSceneBuffer.RenderToFile(const AFile: string; DPI: Integer);
var
  ABitmap:     TDGLBitmap;
  saveAllowed: Boolean;
  fileName:    string;
begin
  Assert((not FRendering), glsAlreadyRendering);
  ABitmap := TDGLBitmap.Create;
  try
    ABitmap.Width       := FViewPort.Width;
    ABitmap.Height      := FViewPort.Height;
    ABitmap.PixelFormat := glpf24Bit;
    RenderToBitmap(ABitmap, DPI);
    fileName := AFile;
    if fileName = '' then
      saveAllowed := SavePictureDialog(fileName)
    else
      saveAllowed := True;
    if saveAllowed then
    begin
      if FileExists(fileName) then
        saveAllowed := QuestionDlg(Format('Overwrite file %s?', [fileName]));
      if saveAllowed then
        ABitmap.SaveToFile(fileName);
    end;
  finally
    ABitmap.Free;
  end;
end;

procedure TDGLSceneBuffer.RenderToFile(const AFile: string; bmpWidth, bmpHeight: Integer);
var
  ABitmap:     TDGLBitmap;
  saveAllowed: Boolean;
  fileName:    string;
begin
  Assert((not FRendering), glsAlreadyRendering);
  ABitmap := TDGLBitmap.Create;
  try
    ABitmap.Width       := bmpWidth;
    ABitmap.Height      := bmpHeight;
    ABitmap.PixelFormat := glpf24Bit;
    RenderToBitmap(ABitmap, (GetDeviceLogicalPixelsX(Cardinal(ABitmap.Canvas.Handle)) * bmpWidth) div FViewPort.Width);
    fileName := AFile;
    if fileName = '' then
      saveAllowed := SavePictureDialog(fileName)
    else
      saveAllowed := True;
    if saveAllowed then
    begin
      if FileExists(fileName) then
        saveAllowed := QuestionDlg(Format('Overwrite file %s?', [fileName]));
      if saveAllowed then
        ABitmap.SaveToFile(fileName);
    end;
  finally
    ABitmap.Free;
  end;
end;

function TDGLSceneBuffer.CreateSnapShot: TDGLBitmap32;
begin
  Result        := TDGLBitmap32.Create;
  Result.Width  := FViewPort.Width;
  Result.Height := FViewPort.Height;
  if Assigned(Camera) and Assigned(Camera.Scene) then
  begin
    FRenderingContext.Activate;
    try
      Result.ReadPixels(rect(0, 0, FViewPort.Width, FViewPort.Height));
    finally
      FRenderingContext.Deactivate;
    end;
  end;
end;

function TDGLSceneBuffer.CreateSnapShotBitmap: TDGLBitmap;
var
  bmp32: TDGLBitmap32;
begin
  bmp32 := CreateSnapShot;
  try
    Result := bmp32.Create32BitsBitmap;
  finally
    bmp32.Free;
  end;
end;

// CopyToTexture
//

//procedure TDGLSceneBuffer.CopyToTexture(aTexture: TDGLTexture);
//begin
//  CopyToTexture(aTexture, 0, 0, Width, Height, 0, 0);
//end;

// CopyToTexture
//

//procedure TDGLSceneBuffer.CopyToTexture(aTexture: TDGLTexture; xSrc, ySrc, AWidth, AHeight: Integer; xDest, yDest: Integer; glCubeFace: TGLEnum = 0);
//var
//  bindTarget: TDGLTextureTarget;
//begin
//  if RenderingContext <> nil then
//  begin
//    RenderingContext.Activate;
//    try
//      if not(aTexture.Image is TDGLBlankImage) then
//        aTexture.ImageClassName := TDGLBlankImage.ClassName;
//      if aTexture.Image.Width <> AWidth then
//        TDGLBlankImage(aTexture.Image).Width := AWidth;
//      if aTexture.Image.Height <> AHeight then
//        TDGLBlankImage(aTexture.Image).Height := AHeight;
//      if aTexture.Image.Depth <> 0 then
//        TDGLBlankImage(aTexture.Image).Depth := 0;
//      if TDGLBlankImage(aTexture.Image).CubeMap <> (glCubeFace > 0) then
//        TDGLBlankImage(aTexture.Image).CubeMap := (glCubeFace > 0);
//
//      bindTarget                                              := aTexture.Image.NativeTextureTarget;
//      RenderingContext.GLStates.TextureBinding[0, bindTarget] := aTexture.Handle;
//      if glCubeFace > 0 then
//        glCopyTexSubImage2D(glCubeFace, 0, xDest, yDest, xSrc, ySrc, AWidth, AHeight)
//      else
//        glCopyTexSubImage2D(DecodeGLTextureTarget(bindTarget), 0, xDest, yDest, xSrc, ySrc, AWidth, AHeight)
//    finally
//      RenderingContext.Deactivate;
//    end;
//  end;
//end;

procedure TDGLSceneBuffer.SaveAsFloatToFile(const aFilename: string);
var
  Data:     Pointer;
  DataSize: Integer;
  stream:   TMemoryStream;
const
  FloatSize = 4;
begin
  if Assigned(Camera) and Assigned(Camera.Scene) then
  begin
    DataSize := Width * Height * FloatSize * FloatSize;
    GetMem(Data, DataSize);
    FRenderingContext.Activate;
    try
      glReadPixels(0, 0, Width, Height, GL_RGBA, GL_FLOAT, Data);
      CheckOpenGLError;

      stream := TMemoryStream.Create;
      try
        stream.Write(Data^, DataSize);
        stream.SaveToFile(aFilename);
      finally
        stream.Free;
      end;
    finally
      FRenderingContext.Deactivate;
      FreeMem(Data);
    end;
  end;
end;

procedure TDGLSceneBuffer.SetViewPort(x, y, W, H: Integer);
begin
  with FViewPort do
  begin
    Left   := x;
    Top    := y;
    Width  := W;
    Height := H;
  end;
  NotifyChange(Self);
end;

function TDGLSceneBuffer.Width: Integer;
begin
  Result := FViewPort.Width;
end;

function TDGLSceneBuffer.Height: Integer;
begin
  Result := FViewPort.Height;
end;

procedure TDGLSceneBuffer.Freeze;
begin
  if Freezed then
    Exit;
  if RenderingContext = nil then
    Exit;
  Render;
  FFreezed := True;
  RenderingContext.Activate;
  try
    FFreezeBuffer := AllocMem(FViewPort.Width * FViewPort.Height * 4);
    glReadPixels(0, 0, FViewPort.Width, FViewPort.Height, GL_RGBA, GL_UNSIGNED_BYTE, FFreezeBuffer);
    FFreezedViewPort := FViewPort;
  finally
    RenderingContext.Deactivate;
  end;
end;

procedure TDGLSceneBuffer.Melt;
begin
  if not Freezed then
    Exit;
  FreeMem(FFreezeBuffer);
  FFreezeBuffer := nil;
  FFreezed      := False;
end;

procedure TDGLSceneBuffer.RenderToBitmap(ABitmap: TDGLBitmap; DPI: Integer);
var
  nativeContext: TDGLContext;
  aColorBits:    Integer;
begin
  Assert((not FRendering), glsAlreadyRendering);
  FRendering    := True;
  nativeContext := RenderingContext;
  try
    aColorBits := PixelFormatToColorBits(ABitmap.PixelFormat);
    if aColorBits < 8 then
      aColorBits      := 8;
    FRenderingContext := DGLContextManager.CreateContext;
    SetupRCOptions(FRenderingContext);
    with FRenderingContext do
    begin
      Options      := []; // no such things for bitmap rendering
      ColorBits    := aColorBits; // honour Bitmap's pixel depth
      AntiAliasing := aaNone; // no AA for bitmap rendering
      CreateContext(ABitmap.Canvas.Handle);
    end;
    try
      FRenderingContext.Activate;
      try
        SetupRenderingContext(FRenderingContext);
        FRenderingContext.GLStates.ColorClearValue := ConvertWinColor(FBackgroundColor);
        // set the desired viewport and limit output to this rectangle
        with FViewPort do
        begin
          Left                                := 0;
          Top                                 := 0;
          Width                               := ABitmap.Width;
          Height                              := ABitmap.Height;
          FRenderingContext.GLStates.ViewPort := Vector4iMake(Left, Top, Width, Height);
        end;
        ClearBuffers;
        FRenderDPI := DPI;
        if FRenderDPI = 0 then
          FRenderDPI := GetDeviceLogicalPixelsX(ABitmap.Canvas.Handle);
        // render
        DoBaseRender(FViewPort, FRenderDPI, dsPrinting, nil);
        if nativeContext <> nil then
          FViewPort := TRectangle(nativeContext.GLStates.ViewPort);
        glFinish;
      finally
        FRenderingContext.Deactivate;
      end;
    finally
      FRenderingContext.Free;
    end;
  finally
    FRenderingContext := nativeContext;
    FRendering        := False;
  end;
  if Assigned(FAfterRender) then
    if Owner is TComponent then
      if not(csDesigning in TComponent(Owner).ComponentState) then
        FAfterRender(Self);
end;

procedure TDGLSceneBuffer.ShowInfo(Modal: Boolean);
begin
  if not Assigned(FRenderingContext) then
    Exit;
  // most info is available with active context only
  FRenderingContext.Activate;
  try
    InvokeInfoForm(Self, Modal);
  finally
    FRenderingContext.Deactivate;
  end;
end;

procedure TDGLSceneBuffer.ResetPerformanceMonitor;
begin
  FFramesPerSecond  := 0;
  FFrameCount       := 0;
  FFirstPerfCounter := 0;
end;

procedure TDGLSceneBuffer.PushViewMatrix(const newMatrix: TMatrix);
var
  N: PTransformationRec;
begin
//  TTransformationRec = record
//    FStates: TGLPipelineTransformationStates;
//    FModelMatrix: TMatrix;
//    FViewMatrix: TMatrix;
//    FProjectionMatrix: TMatrix;
//    FInvModelMatrix: TMatrix;
//    FNormalModelMatrix: TAffineMatrix;
//    FModelViewMatrix: TMatrix;
//    FInvModelViewMatrix: TMatrix;
//    FViewProjectionMatrix: TMatrix;
//    FFrustum: TFrustum;
//  end;


//  N := Length(FViewMatrixStack);
//  SetLength(FViewMatrixStack, N + 1);
//  FViewMatrixStack[N]                                := RenderingContext.PipelineTransformation.ViewMatrix;
//  RenderingContext.PipelineTransformation.ViewMatrix := newMatrix;
  n:=new(PTransformationRec);
  with n^ do
  begin
   FStates := [trsViewProjChanged];
   FViewMatrix := newMatrix;
  end;
  RenderingContext.PipelineTransformation.Push(n);
end;

procedure TDGLSceneBuffer.PopViewMatrix;
//var
//  N: Integer;
begin
//  N := High(FViewMatrixStack);
//  Assert(N >= 0, 'Unbalanced PopViewMatrix');
//  RenderingContext.PipelineTransformation.ViewMatrix := FViewMatrixStack[N];
//  SetLength(FViewMatrixStack, N);
  RenderingContext.PipelineTransformation.Pop;
end;

procedure TDGLSceneBuffer.PushProjectionMatrix(const newMatrix: TMatrix);
//var
//  N: Integer;
var
  N: PTransformationRec;
begin
//  N := Length(FProjectionMatrixStack);
//  SetLength(FProjectionMatrixStack, N + 1);
//  FProjectionMatrixStack[N]                                := RenderingContext.PipelineTransformation.ProjectionMatrix;
//  RenderingContext.PipelineTransformation.ProjectionMatrix := newMatrix;
  n:=new(PTransformationRec);
  with n^ do
  begin
   FStates := [trsViewProjChanged];
   FProjectionMatrix := newMatrix;
  end;
  RenderingContext.PipelineTransformation.Push(n);
end;

procedure TDGLSceneBuffer.PopProjectionMatrix;
//var
//  N: Integer;
begin
//  N := High(FProjectionMatrixStack);
//  Assert(N >= 0, 'Unbalanced PopProjectionMatrix');
//  RenderingContext.PipelineTransformation.ProjectionMatrix := FProjectionMatrixStack[N];
//  SetLength(FProjectionMatrixStack, N);
  RenderingContext.PipelineTransformation.Pop;
end;

function TDGLSceneBuffer.ProjectionMatrix;
begin
  Result := RenderingContext.PipelineTransformation.ProjectionMatrix;
end;

function TDGLSceneBuffer.ViewMatrix: TMatrix;
begin
  Result := RenderingContext.PipelineTransformation.ViewMatrix;
end;

function TDGLSceneBuffer.ModelMatrix: TMatrix;
begin
  Result := RenderingContext.PipelineTransformation.ModelMatrix;
end;

function TDGLSceneBuffer.OrthoScreenToWorld(screenX, screenY: Integer): TAffineVector;
var
  camPos, camUp, camRight: TAffineVector;
  f:                       Single;
begin
  if Assigned(FCamera) then
  begin
    SetVector(camPos, FCameraAbsolutePosition);
    if Camera.TargetObject <> nil then
    begin
      SetVector(camUp, FCamera.AbsoluteUpVectorToTarget);
      SetVector(camRight, FCamera.AbsoluteRightVectorToTarget);
    end
    else
    begin
      SetVector(camUp, Camera.AbsoluteUp);
      SetVector(camRight, Camera.AbsoluteRight);
    end;
    f := 100 * FCamera.NearPlaneBias / (FCamera.FocalLength * FCamera.SceneScale);
    if FViewPort.Width > FViewPort.Height then
      f := f / FViewPort.Width
    else
      f := f / FViewPort.Height;
    SetVector(Result, VectorCombine3(camPos, camUp, camRight, 1, (screenY - (FViewPort.Height div 2)) * f, (screenX - (FViewPort.Width div 2)) * f));
  end
  else
    Result := NullVector;
end;

function TDGLSceneBuffer.ScreenToWorld(const aPoint: TAffineVector): TAffineVector;
var
  rslt: TVector;
begin
  if Assigned(FCamera) and UnProject(VectorMake(aPoint), RenderingContext.PipelineTransformation.ViewProjectionMatrix, PHomogeneousIntVector(@FViewPort)^, rslt) then
    Result := Vector3fMake(rslt)
  else
    Result := aPoint;
end;

function TDGLSceneBuffer.ScreenToWorld(const aPoint: TVector): TVector;
begin
  MakePoint(Result, ScreenToWorld(AffineVectorMake(aPoint)));
end;

function TDGLSceneBuffer.ScreenToWorld(screenX, screenY: Integer): TAffineVector;
begin
  Result := ScreenToWorld(AffineVectorMake(screenX, FViewPort.Height - screenY, 0));
end;

function TDGLSceneBuffer.WorldToScreen(const aPoint: TAffineVector): TAffineVector;
var
  rslt: TVector;
begin
  RenderingContext.Activate;
  try
    PrepareRenderingMatrices(FViewPort, FRenderDPI);
    if Assigned(FCamera) and Project(VectorMake(aPoint), RenderingContext.PipelineTransformation.ViewProjectionMatrix, TVector4i(FViewPort), rslt) then
      Result := Vector3fMake(rslt)
    else
      Result := aPoint;
  finally
    RenderingContext.Deactivate;
  end;
end;

function TDGLSceneBuffer.WorldToScreen(const aPoint: TVector): TVector;
begin
  SetVector(Result, WorldToScreen(AffineVectorMake(aPoint)));
end;

procedure TDGLSceneBuffer.WorldToScreen(points: PVector; nbPoints: Integer);
var
  i: Integer;
begin
  if Assigned(FCamera) then
  begin
    for i := nbPoints - 1 downto 0 do
    begin
      Project(points^, RenderingContext.PipelineTransformation.ViewProjectionMatrix, PHomogeneousIntVector(@FViewPort)^, points^);
      Inc(points);
    end;
  end;
end;

function TDGLSceneBuffer.ScreenToVector(const aPoint: TAffineVector): TAffineVector;
begin
  Result := VectorSubtract(ScreenToWorld(aPoint), PAffineVector(@FCameraAbsolutePosition)^);
end;

function TDGLSceneBuffer.ScreenToVector(const aPoint: TVector): TVector;
begin
  SetVector(Result, VectorSubtract(ScreenToWorld(aPoint), FCameraAbsolutePosition));
  Result.V[3] := 0;
end;

function TDGLSceneBuffer.ScreenToVector(const x, y: Integer): TVector;
var
  av: TAffineVector;
begin
  av.V[0] := x;
  av.V[1] := y;
  av.V[2] := 0;
  SetVector(Result, ScreenToVector(av));
end;

function TDGLSceneBuffer.VectorToScreen(const VectToCam: TAffineVector): TAffineVector;
begin
  Result := WorldToScreen(VectorAdd(VectToCam, PAffineVector(@FCameraAbsolutePosition)^));
end;

function TDGLSceneBuffer.ScreenVectorIntersectWithPlane(const aScreenPoint: TVector; const planePoint, planeNormal: TVector; var intersectPoint: TVector): Boolean;
var
  V: TVector;
begin
  if Assigned(FCamera) then
  begin
    SetVector(V, ScreenToVector(aScreenPoint));
    Result              := RayCastPlaneIntersect(FCameraAbsolutePosition, V, planePoint, planeNormal, @intersectPoint);
    intersectPoint.V[3] := 1;
  end
  else
    Result := False;
end;

function TDGLSceneBuffer.ScreenVectorIntersectWithPlaneXY(const aScreenPoint: TVector; const z: Single; var intersectPoint: TVector): Boolean;
begin
  Result              := ScreenVectorIntersectWithPlane(aScreenPoint, VectorMake(0, 0, z), ZHmgVector, intersectPoint);
  intersectPoint.V[3] := 0;
end;

function TDGLSceneBuffer.ScreenVectorIntersectWithPlaneYZ(const aScreenPoint: TVector; const x: Single; var intersectPoint: TVector): Boolean;
begin
  Result              := ScreenVectorIntersectWithPlane(aScreenPoint, VectorMake(x, 0, 0), XHmgVector, intersectPoint);
  intersectPoint.V[3] := 0;
end;

function TDGLSceneBuffer.ScreenVectorIntersectWithPlaneXZ(const aScreenPoint: TVector; const y: Single; var intersectPoint: TVector): Boolean;
begin
  Result              := ScreenVectorIntersectWithPlane(aScreenPoint, VectorMake(0, y, 0), YHmgVector, intersectPoint);
  intersectPoint.V[3] := 0;
end;

function TDGLSceneBuffer.PixelRayToWorld(x, y: Integer): TAffineVector;
var
  dov, np, fp, z, dst, wrpdst: Single;
  vec, cam, targ, rayhit, pix: TAffineVector;
  camAng:                      real;
begin
  if Camera.CameraStyle = csOrtho2D then
    dov := 2
  else
    dov := Camera.DepthOfView;
  np    := Camera.NearPlane;
  fp    := Camera.NearPlane + dov;
  z     := GetPixelDepth(x, y);
  dst   := (fp * np) / (fp - z * dov); // calc from z-buffer value to world depth
  // ------------------------
  // z:=1-(fp/d-1)/(fp/np-1);  //calc from world depth to z-buffer value
  // ------------------------
  vec.V[0] := x;
  vec.V[1] := FViewPort.Height - y;
  vec.V[2] := 0;
  vec      := ScreenToVector(vec);
  NormalizeVector(vec);
  SetVector(cam, Camera.AbsolutePosition);
  // targ:=Camera.TargetObject.Position.AsAffineVector;
  // SubtractVector(targ,cam);
  pix.V[0] := FViewPort.Width * 0.5;
  pix.V[1] := FViewPort.Height * 0.5;
  pix.V[2] := 0;
  targ     := Self.ScreenToVector(pix);

  camAng := VectorAngleCosine(targ, vec);
  wrpdst := dst / camAng;
  rayhit := cam;
  CombineVector(rayhit, vec, wrpdst);
  Result := rayhit;
end;

procedure TDGLSceneBuffer.ClearBuffers;
var
  bufferBits: TGLBitfield;
begin
  if roNoDepthBufferClear in ContextOptions then
    bufferBits := 0
  else
  begin
    bufferBits                               := GL_DEPTH_BUFFER_BIT;
    CurrentDGLContext.GLStates.DepthWriteMask := True;
  end;
  if ContextOptions * [roNoColorBuffer, roNoColorBufferClear] = [] then
  begin
    bufferBits := bufferBits or GL_COLOR_BUFFER_BIT;
    CurrentDGLContext.GLStates.SetColorMask(cAllColorComponents);
  end;
  if roStencilBuffer in ContextOptions then
  begin
    bufferBits := bufferBits or GL_STENCIL_BUFFER_BIT;
  end;
  glClear(bufferBits);
end;

procedure TDGLSceneBuffer.NotifyChange(Sender: TObject);
begin
  DoChange;
end;

// PickObjects
//

//procedure TDGLSceneBuffer.PickObjects(const rect: TGLRect; pickList: TDGLPickList; objectCountGuess: Integer);
//var
//  i:   Integer;
//  Obj: TDGLBaseSceneObject;
//begin
//  if not Assigned(FCamera) then
//    Exit;
//  Assert((not FRendering), glsAlreadyRendering);
//  Assert(Assigned(pickList));
//  FRenderingContext.Activate;
//  FRendering := True;
//  try
//    // Create best selector which techniques is hardware can do
//    if not Assigned(FSelector) then
//      FSelector := GetBestSelectorClass.Create;
//
////    xgl.MapTexCoordToNull; // turn off
//    PrepareRenderingMatrices(FViewPort, RenderDPI, @rect);
//    FSelector.Hits := -1;
//    if objectCountGuess > 0 then
//      FSelector.objectCountGuess := objectCountGuess;
//    repeat
//      FSelector.Start;
//      // render the scene (in select mode, nothing is drawn)
//      FRenderDPI := 96;
//      if Assigned(FCamera) and Assigned(FCamera.FScene) then
//        RenderScene(FCamera.FScene, FViewPort.Width, FViewPort.Height, dsPicking, nil);
//    until FSelector.Stop;
//    FSelector.FillPickingList(pickList);
//    for i := 0 to pickList.Count - 1 do
//    begin
//      Obj := TDGLBaseSceneObject(pickList[i]);
//      if Assigned(Obj.FOnPicked) then
//        Obj.FOnPicked(Obj);
//    end;
//  finally
//    FRendering := False;
//    FRenderingContext.Deactivate;
//  end;
//end;

// GetPickedObjects
//

//function TDGLSceneBuffer.GetPickedObjects(const rect: TGLRect; objectCountGuess: Integer = 64): TDGLPickList;
//begin
//  Result := TDGLPickList.Create(psMinDepth);
//  PickObjects(rect, Result, objectCountGuess);
//end;

// GetPickedObject
//

//function TDGLSceneBuffer.GetPickedObject(x, y: Integer): TDGLBaseSceneObject;
//var
//  pkList: TDGLPickList;
//begin
//  pkList := GetPickedObjects(rect(x - 1, y - 1, x + 1, y + 1));
//  try
//    if pkList.Count > 0 then
//      Result := TDGLBaseSceneObject(pkList.Hit[0])
//    else
//      Result := nil;
//  finally
//    pkList.Free;
//  end;
//end;

function TDGLSceneBuffer.GetPixelColor(x, y: Integer): TColor;
var
  buf: array [0 .. 2] of Byte;
begin
  if not Assigned(FCamera) then
  begin
    Result := 0;
    Exit;
  end;
  FRenderingContext.Activate;
  try
    glReadPixels(x, FViewPort.Height - y, 1, 1, GL_RGB, GL_UNSIGNED_BYTE, @buf[0]);
  finally
    FRenderingContext.Deactivate;
  end;
  Result := RGB(buf[0], buf[1], buf[2]);
end;

function TDGLSceneBuffer.GetPixelDepth(x, y: Integer): Single;
begin
  if not Assigned(FCamera) then
  begin
    Result := 0;
    Exit;
  end;
  FRenderingContext.Activate;
  try
    glReadPixels(x, FViewPort.Height - y, 1, 1, GL_DEPTH_COMPONENT, GL_FLOAT, @Result);
  finally
    FRenderingContext.Deactivate;
  end;
end;

function TDGLSceneBuffer.PixelDepthToDistance(aDepth: Single): Single;
var
  dov, np, fp: Single;
begin
  if Camera.CameraStyle = csOrtho2D then
    dov := 2
  else
    dov  := Camera.DepthOfView; // Depth of View (from np to fp)
  np     := Camera.NearPlane; // Near plane distance
  fp     := np + dov; // Far plane distance
  Result := (fp * np) / (fp - aDepth * dov);
  // calculate world distance from z-buffer value
end;

function TDGLSceneBuffer.PixelToDistance(x, y: Integer): Single;
var
  z, dov, np, fp, dst, camAng: Single;
  norm, coord, vec:            TAffineVector;
begin
  z := GetPixelDepth(x, y);
  if Camera.CameraStyle = csOrtho2D then
    dov := 2
  else
    dov := Camera.DepthOfView; // Depth of View (from np to fp)
  np    := Camera.NearPlane; // Near plane distance
  fp    := np + dov; // Far plane distance
  dst   := (np * fp) / (fp - z * dov);
  // calculate from z-buffer value to frustrum depth
  coord.V[0] := x;
  coord.V[1] := y;
  vec        := Self.ScreenToVector(coord); // get the pixel vector
  coord.V[0] := FViewPort.Width div 2;
  coord.V[1] := FViewPort.Height div 2;
  norm       := Self.ScreenToVector(coord); // get the absolute camera direction
  camAng     := VectorAngleCosine(norm, vec);
  Result     := dst / camAng; // compensate for flat frustrum face
end;

procedure TDGLSceneBuffer.NotifyMouseMove(Shift: TShiftState; x, y: Integer);
begin
  // Nothing
end;

procedure TDGLSceneBuffer.PrepareRenderingMatrices(const aViewPort: TRectangle; resolution: Integer; pickingRect: PGLRect = nil);
begin
  RenderingContext.PipelineTransformation.IdentityAll;
  // setup projection matrix
  if Assigned(pickingRect) then
  begin
    CurrentDGLContext.PipelineTransformation.ProjectionMatrix := CreatePickMatrix((pickingRect^.Left + pickingRect^.Right) div 2, FViewPort.Height - ((pickingRect^.Top + pickingRect^.Bottom) div 2), Abs(pickingRect^.Right - pickingRect^.Left),
      Abs(pickingRect^.Bottom - pickingRect^.Top), TVector4i(FViewPort));
  end;
  FBaseProjectionMatrix := CurrentDGLContext.PipelineTransformation.ProjectionMatrix;

  if Assigned(FCamera) then
  begin
    FCamera.Scene.FCurrenTDGLCamera := FCamera;
    // apply camera perpective
    FCamera.ApplyPerspective(aViewPort, FViewPort.Width, FViewPort.Height, resolution);
    // setup model view matrix
    // apply camera transformation (viewpoint)
    FCamera.Apply;
    FCameraAbsolutePosition := FCamera.AbsolutePosition;
  end;
end;

procedure TDGLSceneBuffer.DoBaseRender(const aViewPort: TRectangle; resolution: Integer; drawState: TDrawState; baseObject: TDGLBaseSceneObject);
begin
  with RenderingContext.GLStates do
  begin
    PrepareRenderingMatrices(aViewPort, resolution);

 //     xgl.MapTexCoordToNull; // force XGL rebind
//      xgl.MapTexCoordToMain;


    if Assigned(FViewerBeforeRender) and (drawState <> dsPrinting) then
      FViewerBeforeRender(Self);
    if Assigned(FBeforeRender) then
      if Owner is TComponent then
        if not(csDesigning in TComponent(Owner).ComponentState) then
          FBeforeRender(Self);

    if Assigned(FCamera) and Assigned(FCamera.FScene) then
    begin
      with FCamera.FScene do
      begin
//        SetupLights(maxLights);
//
//          if FogEnable then
//          begin
//            Enable(stFog);
//            FogEnvironment.ApplyFog;
//          end
//          else
//            Disable(stFog);


        RenderScene(FCamera.FScene, aViewPort.Width, aViewPort.Height, drawState, baseObject);
      end;
    end;
    if Assigned(FPostRender) then
      if Owner is TComponent then
        if not(csDesigning in TComponent(Owner).ComponentState) then
          FPostRender(Self);
  end;
//  Assert(Length(FViewMatrixStack) = 0, 'Unbalance Push/PopViewMatrix.');
//  Assert(Length(FProjectionMatrixStack) = 0, 'Unbalance Push/PopProjectionMatrix.');
end;

procedure TDGLSceneBuffer.Render;
begin
  Render(nil);
end;

procedure TDGLSceneBuffer.Render(baseObject: TDGLBaseSceneObject);
var
  perfCounter, framePerf: Int64;
begin
  if FRendering then
    Exit;
  if not Assigned(FRenderingContext) then
    Exit;

  if Freezed and (FFreezeBuffer <> nil) then
  begin
    RenderingContext.Activate;
    try
      RenderingContext.GLStates.ColorClearValue := ConvertWinColor(FBackgroundColor, FBackgroundAlpha);
      ClearBuffers;
//      GLMatrixMode(GL_PROJECTION);
//      GLLoadIdentity;
//      GLMatrixMode(GL_MODELVIEW);
//      GLLoadIdentity;
//      GLRasterPos2f(-1, -1);
//      GLDrawPixels(FFreezedViewPort.Width, FFreezedViewPort.Height, GL_RGBA, GL_UNSIGNED_BYTE, FFreezeBuffer);
      if not(roNoSwapBuffers in ContextOptions) then  RenderingContext.SwapBuffers;
    finally
      RenderingContext.Deactivate;
    end;
    Exit;
  end;

  QueryPerformanceCounter(framePerf);

  if Assigned(FCamera) and Assigned(FCamera.FScene) then
  begin
    FCamera.AbsoluteMatrixAsAddress;
    FCamera.FScene.AddBuffer(Self);
  end;

  FRendering := True;
  try
    FRenderingContext.Activate;
    try
      if FFrameCount = 0 then
        QueryPerformanceCounter(FFirstPerfCounter);

      FRenderDPI := 96; // default value for screen
      ClearOpenGLError;
      SetupRenderingContext(FRenderingContext);
      // clear the buffers
      FRenderingContext.GLStates.ColorClearValue := ConvertWinColor(FBackgroundColor, FBackgroundAlpha);
      ClearBuffers;
      CheckOpenGLError;
      // render
      DoBaseRender(FViewPort, RenderDPI, dsRendering, baseObject);

      if not(roNoSwapBuffers in ContextOptions) then
        RenderingContext.SwapBuffers;

      // yes, calculate average frames per second...
      Inc(FFrameCount);
      QueryPerformanceCounter(perfCounter);
      FLastFrameTime := (perfCounter - framePerf) / vCounterFrequency;
      Dec(perfCounter, FFirstPerfCounter);
      if perfCounter > 0 then
        FFramesPerSecond := (FFrameCount * vCounterFrequency) / perfCounter;
      CheckOpenGLError;
    finally
      FRenderingContext.Deactivate;
    end;
    if Assigned(FAfterRender) and (Owner is TComponent) then
      if not(csDesigning in TComponent(Owner).ComponentState) then
        FAfterRender(Self);
  finally
    FRendering := False;
  end;
end;

procedure TDGLSceneBuffer.RenderScene(aScene: TDGLScene; const viewPortSizeX, viewPortSizeY: Integer; drawState: TDrawState; baseObject: TDGLBaseSceneObject);

var
  i:           Integer;
  rci:         TRenderContextInfo;
  rightVector: TVector;
begin
  FAfterRenderEffects.Clear;
  aScene.FCurrentBuffer := Self;
  FillChar(rci, Sizeof(rci), 0);
  rci.Scene              := aScene;
  rci.Buffer             := Self;
  rci.afterRenderEffects := FAfterRenderEffects;
  rci.ObjectsSorting     := aScene.ObjectsSorting;
  rci.VisibilityCulling  := aScene.VisibilityCulling;
  rci.bufferFaceCull     := FFaceCulling;
  rci.bufferLighting     := FLighting;
  rci.bufferFog          := FFogEnable;
  rci.bufferDepthTest    := FDepthTest;
  rci.drawState          := drawState;
  rci.sceneAmbientColor  := FAmbientColor.Color;
  rci.primitiveMask      := cAllMeshPrimitive;
  with FCamera do
  begin
    rci.cameraPosition  := FCameraAbsolutePosition;
    rci.cameraDirection := FLastDirection;
    NormalizeVector(rci.cameraDirection);
    rci.cameraDirection.V[3] := 0;
    rightVector              := VectorCrossProduct(rci.cameraDirection, Up.AsVector);
    rci.cameraUp             := VectorCrossProduct(rightVector, rci.cameraDirection);
    NormalizeVector(rci.cameraUp);

    with rci.rcci do
    begin
      origin               := rci.cameraPosition;
      clippingDirection    := rci.cameraDirection;
      viewPortRadius       := FViewPortRadius;
      nearClippingDistance := FNearPlane;
      farClippingDistance  := FNearPlane + FDepthOfView;
      frustum              := RenderingContext.PipelineTransformation.frustum;
    end;
  end;
  rci.viewPortSize.cx        := viewPortSizeX;
  rci.viewPortSize.cy        := viewPortSizeY;
  rci.RenderDPI              := FRenderDPI;
  rci.GLStates               := RenderingContext.GLStates;
  rci.PipelineTransformation := RenderingContext.PipelineTransformation;
  rci.proxySubObject         := False;
  rci.ignoreMaterials        := (roNoColorBuffer in FContextOptions) or (rci.drawState = dsPicking);
  rci.amalgamating           := rci.drawState = dsPicking;
  rci.GLStates.SeTDGLColorWriting(not rci.ignoreMaterials);
  if Assigned(FInitiateRendering) then
    FInitiateRendering(Self, rci);

  if aScene.InitializableObjects.Count <> 0 then
  begin
    // First initialize all objects and delete them from the list.
    for i := aScene.InitializableObjects.Count - 1 downto 0 do
    begin
      aScene.InitializableObjects.Items[i].InitializeObject( { Self? } aScene, rci);
      aScene.InitializableObjects.Delete(i);
    end;
  end;

  if RenderingContext.IsPreparationNeed then
    RenderingContext.PrepareHandlesData;

  if baseObject = nil then
  begin
    aScene.Objects.Render(rci);
  end
  else
    baseObject.Render(rci);
  rci.GLStates.SeTDGLColorWriting(True);
  with FAfterRenderEffects do
    if Count > 0 then
      for i := 0 to Count - 1 do
        TDGLObjectAfterEffect(Items[i]).Render(rci);
  if Assigned(FWrapUpRendering) then
    FWrapUpRendering(Self, rci);
end;

procedure TDGLSceneBuffer.SetBackgroundColor(AColor: TColor);
begin
  if FBackgroundColor <> AColor then
  begin
    FBackgroundColor := AColor;
    NotifyChange(Self);
  end;
end;

procedure TDGLSceneBuffer.SetBackgroundAlpha(alpha: Single);
begin
  if FBackgroundAlpha <> alpha then
  begin
    FBackgroundAlpha := alpha;
    NotifyChange(Self);
  end;
end;

procedure TDGLSceneBuffer.SetAmbientColor(AColor: TDGLColor);
begin
  FAmbientColor.Assign(AColor);
end;

procedure TDGLSceneBuffer.SetCamera(ACamera: TDGLCamera);
begin
  if FCamera <> ACamera then
  begin
    if Assigned(FCamera) then
    begin
      if Assigned(FCamera.FScene) then
        FCamera.FScene.RemoveBuffer(Self);
      FCamera := nil;
    end;
    if Assigned(ACamera) and Assigned(ACamera.FScene) then
    begin
      FCamera := ACamera;
      FCamera.TransformationChanged;
    end;
    NotifyChange(Self);
  end;
end;

procedure TDGLSceneBuffer.SetContextOptions(Options: TContextOptions);
begin
  if FContextOptions <> Options then
  begin
    FContextOptions := Options;
    DoStructuralChange;
  end;
end;

procedure TDGLSceneBuffer.SetDepthTest(AValue: Boolean);
begin
  if FDepthTest <> AValue then
  begin
    FDepthTest := AValue;
    NotifyChange(Self);
  end;
end;

procedure TDGLSceneBuffer.SetFaceCulling(AValue: Boolean);
begin
  if FFaceCulling <> AValue then
  begin
    FFaceCulling := AValue;
    NotifyChange(Self);
  end;
end;

procedure TDGLSceneBuffer.SetLayer(const Value: TDGLContextLayer);
begin
  if FLayer <> Value then
  begin
    FLayer := Value;
    DoStructuralChange;
  end;
end;

procedure TDGLSceneBuffer.SetLighting(AValue: Boolean);
begin
  if FLighting <> AValue then
  begin
    FLighting := AValue;
    NotifyChange(Self);
  end;
end;

procedure TDGLSceneBuffer.SetAntiAliasing(const val: TDGLAntiAliasing);
begin
  if FAntiAliasing <> val then
  begin
    FAntiAliasing := val;
    DoStructuralChange;
  end;
end;

procedure TDGLSceneBuffer.SetDepthPrecision(const val: TDGLDepthPrecision);
begin
  if FDepthPrecision <> val then
  begin
    FDepthPrecision := val;
    DoStructuralChange;
  end;
end;

procedure TDGLSceneBuffer.SetColorDepth(const val: TDGLColorDepth);
begin
  if FColorDepth <> val then
  begin
    FColorDepth := val;
    DoStructuralChange;
  end;
end;

procedure TDGLSceneBuffer.SetShadeModel(const val: TDGLShadeModel);
begin
  if FShadeModel <> val then
  begin
    FShadeModel := val;
    NotifyChange(Self);
  end;
end;

procedure TDGLSceneBuffer.SetFogEnable(AValue: Boolean);
begin
  if FFogEnable <> AValue then
  begin
    FFogEnable := AValue;
    NotifyChange(Self);
  end;
end;

procedure TDGLSceneBuffer.SetDGLFogEnvironment(AValue: TDGLFogEnvironment);
begin
  FFogEnvironment.Assign(AValue);
  NotifyChange(Self);
end;

function TDGLSceneBuffer.StoreFog: Boolean;
begin
  Result := (not FFogEnvironment.IsAtDefaultValues);
end;

procedure TDGLSceneBuffer.SetAccumBufferBits(const val: Integer);
begin
  if FAccumBufferBits <> val then
  begin
    FAccumBufferBits := val;
    DoStructuralChange;
  end;
end;

procedure TDGLSceneBuffer.DoChange;
begin
  if (not FRendering) and Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TDGLSceneBuffer.DoStructuralChange;
var
  bCall: Boolean;
begin
  if Assigned(Owner) then
    bCall := not(csLoading in TComponent(GetOwner).ComponentState)
  else
    bCall := True;
  if bCall and Assigned(FOnStructuralChange) then
    FOnStructuralChange(Self);
end;

{$IFDEF GLS_REGIONS}{$ENDREGION}{$ENDIF}

// ------------------
{ TDGLInitializableObjectList }
{$IFDEF GLS_REGIONS}{$REGION 'TDGLInitializableObjectList'}{$ENDIF}


function TDGLInitializableObjectList.Add(const Item: IGLInitializable): Integer;
begin
  Result := inherited Add(Pointer(Item));
end;


function TDGLInitializableObjectList.GetItems(const Index: Integer): IGLInitializable;
begin
  Result := IGLInitializable(inherited Get(Index));
end;

procedure TDGLInitializableObjectList.PutItems(const Index: Integer; const Value: IGLInitializable);
begin
  inherited Put(Index, Pointer(Value));
end;

{$IFDEF GLS_REGIONS}{$ENDREGION}{$ENDIF}

// ------------------------------------------------------------------------------
// ------------------------------------------------------------------------------
// ------------------------------------------------------------------------------
initialization

// ------------------------------------------------------------------------------
// ------------------------------------------------------------------------------
// ------------------------------------------------------------------------------

RegisterClasses([TDGLLightSource, TDGLCamera, TDGLScene, TDGLDirectOpenGL]);
//TDGLMemoryViewer
// TDGLRenderPoint, TDGLProxyObject,

// preparation for high resolution timer
QueryPerformanceFrequency(vCounterFrequency);

end.
