//
// VXScene Component Library, based on GLScene http://glscene.sourceforge.net
//
{
  Base classes and structures for VXScene.
}

unit VXS.Scene;

interface

{$I VXScene.inc}

uses
  Winapi.Windows,
  Winapi.OpenGL,
  Winapi.OpenGLext,
  System.Classes,
  System.SysUtils,
  System.UITypes,
  System.Math,
  FMX.Graphics,
  FMX.Controls,
  FMX.Types,
  FMX.Dialogs,

  VXS.OpenGL,
  VXS.Strings,
  VXS.Context,
  VXS.VectorGeometry,
  VXS.XCollection,
  VXS.Silhouette,
  VXS.PersistentClasses,
  VXS.PipeLineTransformation,
  VXS.State,
  VXS.Graphics,
  VXS.GeometryBB,
  VXS.CrossPlatform,
  VXS.VectorLists,
  VXS.Texture,
  VXS.Color,
  VXS.BaseClasses,
  VXS.Coordinates,
  VXS.RenderContextInfo,
  VXS.Material,
  VXS.TextureFormat,
  VXS.Selection,

  VXS.VectorTypes,
  VXS.ApplicationFileIO,
  VXS.Utils,
  VXS.XOpenGL;

type
  { Defines which features are taken from the master object. }
  TVXProxyObjectOption = (pooEffects, pooObjects, pooTransformation);
  TVXProxyObjectOptions = set of TVXProxyObjectOption;

  TVXCameraInvarianceMode = (cimNone, cimPosition, cimOrientation);

  TVXSceneViewerMode = (svmDisabled, svmDefault, svmNavigation, svmGizmo);

const
  cDefaultProxyOptions = [pooEffects, pooObjects, pooTransformation];
  VXSCENE_REVISION = '$Revision: 7100$';
  VXSCENE_VERSION = '1.8.0.%s';

type

  TNormalDirection = (ndInside, ndOutside);

  (* Used to describe only the changes in an object,
    which have to be reflected in the scene *)
  TObjectChange = (ocTransformation, ocAbsoluteMatrix, ocInvAbsoluteMatrix, ocStructure);
  TObjectChanges = set of TObjectChange;

  TObjectBBChange = (oBBcChild, oBBcStructure);
  TObjectBBChanges = set of TObjectBBChange;

  { Flags for design notification }
  TSceneOperation = (soAdd, soRemove, soMove, soRename, soSelect, soBeginUpdate, soEndUpdate);

  { Options for the rendering context.
    roSoftwareMode: force software rendering.
    roDoubleBuffer: enables double-buffering.
    roRenderToWindows: ignored (legacy).
    roTwoSideLighting: enables two-side lighting model.
    roStereo: enables stereo support in the driver 
	  (dunno if it works, I don't have a stereo device to test...)
    roDestinationAlpha: request an Alpha channel for the rendered output
    roNoColorBuffer: don't request a color buffer (color depth setting ignored)
    roNoColorBufferClear: do not clear the color buffer automatically, if the
    whole viewer is fully repainted each frame, this can improve framerate
    roNoSwapBuffers: don't perform RenderingContext.SwapBuffers after rendering
    roNoDepthBufferClear: do not clear the depth buffer automatically. Useful for
    early-z culling.
    roForwardContext: force OpenVX forward context }
  TContextOption = (roSoftwareMode, roDoubleBuffer, roStencilBuffer, roRenderToWindow, roTwoSideLighting, roStereo,
    roDestinationAlpha, roNoColorBuffer, roNoColorBufferClear, roNoSwapBuffers, roNoDepthBufferClear, roDebugContext,
    roForwardContext, roOpenVX_ES2_Context);
  TContextOptions = set of TContextOption;

  { IDs for limit determination }
  TLimitType = (limClipPlanes, limEvalOrder, limLights, limListNesting, limModelViewStack, limNameStack, limPixelMapTable,
    limProjectionStack, limTextureSize, limTextureStack, limViewportDims, limAccumAlphaBits, limAccumBlueBits,
    limAccumGreenBits, limAccumRedBits, limAlphaBits, limAuxBuffers, limBlueBits, limGreenBits, limRedBits, limIndexBits,
    limStereo, limDoubleBuffer, limSubpixelBits, limDepthBits, limStencilBits, limNbTextureUnits);

  TVXBaseSceneObject = class;
  TVXSceneObjectClass = class of TVXBaseSceneObject;
  TVXCustomSceneObject = class;
  TVXScene = class;
  TVXBehaviour = class;
  TVXBehaviourClass = class of TVXBehaviour;
  TVXBehaviours = class;
  TVXObjectEffect = class;
  TVXObjectEffectClass = class of TVXObjectEffect;
  TVXObjectEffects = class;
  TVXSceneBuffer = class;

  { Possible styles/options for objects.
    Allowed styles are:
    osDirectDraw : object shall not make use of compiled call lists, but issue
    direct calls each time a render should be performed.
    osIgnoreDepthBuffer : object is rendered with depth test disabled,
    this is true for its children too.
    osNoVisibilityCulling : whatever the VisibilityCulling setting,
    it will be ignored and the object rendered }
  TVXObjectStyle = (osDirectDraw, osIgnoreDepthBuffer, osNoVisibilityCulling);
  TVXObjectStyles = set of TVXObjectStyle;

  { Interface to objects that need initialization }
  IGLInitializable = interface
    ['{EA40AE8E-79B3-42F5-ADF1-7A901B665E12}']
    procedure InitializeObject(ASender: TObject; const ARci: TVXRenderContextInfo);
  end;

  { Just a list of objects that support IGLInitializable. }
  TVXInitializableObjectList = class(TList)
  private
    function GetItems(const Index: Integer): IGLInitializable;
    procedure PutItems(const Index: Integer; const Value: IGLInitializable);
  public
    function Add(const Item: IGLInitializable): Integer;
    property Items[const Index: Integer]: IGLInitializable read GetItems write PutItems; default;
  end;

  { Base class for all scene objects.
    A scene object is part of scene hierarchy (each scene object can have
    multiple children), this hierarchy primarily defines transformations
    (each child coordinates are relative to its parent), but is also used
    for depth-sorting, bounding and visibility culling purposes.
    Subclasses implement either visual scene objects (that are made to be
    visible at runtime, like a Cube) or structural objects (that influence
    rendering or are used for varied structural manipulations,
    like the ProxyObject).
    To add children at runtime, use the AddNewChild method of TVXBaseSceneObject;
    other children manipulations methods and properties are provided (to browse,
    move and delete them). Using the regular TComponent methods is not
    encouraged. }
  TVXBaseSceneObject = class(TVXCoordinatesUpdateAbleComponent)
  private
    FAbsoluteMatrix, FInvAbsoluteMatrix: TMatrix;
    FLocalMatrix: TMatrix;
    FObjectStyle: TVXObjectStyles;
    FPosition: TVXCoordinates;
    FDirection, FUp: TVXCoordinates;
    FScaling: TVXCoordinates;
    FChanges: TObjectChanges;
    FParent: TVXBaseSceneObject;
    FScene: TVXScene;
    FBBChanges: TObjectBBChanges;
    FBoundingBoxPersonalUnscaled: THmgBoundingBox;
    FBoundingBoxOfChildren: THmgBoundingBox;
    FBoundingBoxIncludingChildren: THmgBoundingBox;
    FChildren: TPersistentObjectList; // created on 1st use
    FVisible: Boolean;
    FUpdateCount: Integer;
    FShowAxes: Boolean;
    FRotation: TVXCoordinates; // current rotation angles
    FIsCalculating: Boolean;
    FObjectsSorting: TVXObjectsSorting;
    FVisibilityCulling: TVXVisibilityCulling;
    FOnProgress: TVXProgressEvent;
    FOnAddedToParent: TNotifyEvent;
    FGLBehaviours: TVXBehaviours;
    FGLObjectEffects: TVXObjectEffects;
    FPickable: Boolean;
    FOnPicked: TNotifyEvent;
    FTagObject: TObject;
    FTagFloat: Single;

    objList: TPersistentObjectList;
    distList: TSingleList;
    // FOriginalFiler: TFiler;   //used to allow persistent events in behaviours & effects
    { If somebody could look at DefineProperties, ReadBehaviours, ReadEffects
      and verify code is safe to use then it could be uncommented }
    function Get(Index: Integer): TVXBaseSceneObject; inline;
    function GetCount: Integer; inline;
    function GetIndex: Integer; inline;
    procedure SetParent(const val: TVXBaseSceneObject); inline;
    procedure SetIndex(aValue: Integer);
    procedure SetDirection(AVector: TVXCoordinates);
    procedure SetUp(AVector: TVXCoordinates);
    function GetMatrix: PMatrix; inline;
    procedure SetPosition(APosition: TVXCoordinates);
    procedure SetPitchAngle(AValue: Single);
    procedure SetRollAngle(AValue: Single);
    procedure SetTurnAngle(AValue: Single);
    procedure SetRotation(aRotation: TVXCoordinates);
    function GetPitchAngle: Single; inline;
    function GetTurnAngle: Single; inline;
    function GetRollAngle: Single; inline;
    procedure SetShowAxes(AValue: Boolean);
    procedure SetScaling(AValue: TVXCoordinates);
    procedure SetObjectsSorting(const val: TVXObjectsSorting);
    procedure SetVisibilityCulling(const val: TVXVisibilityCulling);
    procedure SetBehaviours(const val: TVXBehaviours);
    function GetBehaviours: TVXBehaviours;
    procedure SetEffects(const val: TVXObjectEffects);
    function GetEffects: TVXObjectEffects;
    function GetAbsoluteAffineScale: TAffineVector;
    function GetAbsoluteScale: TVector;
    procedure SetAbsoluteAffineScale(const Value: TAffineVector);
    procedure SetAbsoluteScale(const Value: TVector);
    function GetAbsoluteMatrix: TMatrix; inline;
    procedure SetAbsoluteMatrix(const Value: TMatrix);
    procedure SetBBChanges(const Value: TObjectBBChanges);
    function GetDirectAbsoluteMatrix: PMatrix;
    function GetLocalMatrix: PMatrix; inline;
  protected
    FListHandle: TVXListHandle;
    procedure Loaded; override;
    procedure SetScene(const Value: TVXScene); virtual;
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
    function GetAbsolutePosition: TVector; inline;
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
    procedure RecTransformationChanged; inline;
    procedure DrawAxes(var rci: TVXRenderContextInfo; pattern: Word);
    procedure GetChildren(AProc: TGetChildProc; Root: TComponent); override;
    // Should the object be considered as blended for sorting purposes?
    function Blended: Boolean; virtual;
    procedure RebuildMatrix;
    procedure SetName(const NewName: TComponentName); override;
    procedure SetParentComponent(Value: TComponent); override;
    procedure DestroyHandle; virtual;
    procedure DestroyHandles;
    procedure DeleteChildCameras;
    procedure DoOnAddedToParent; virtual;
    { Used to re-calculate BoundingBoxes every time we need it.
      GetLocalUnscaleBB() must return the local BB, not the axis-aligned one.
      By default it is calculated from AxisAlignedBoundingBoxUnscaled and
      BarycenterAbsolutePosition, but for most objects there is a more
      efficient method, that's why it is virtual. }
    procedure CalculateBoundingBoxPersonalUnscaled(var ANewBoundingBox: THmgBoundingBox); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    constructor CreateAsChild(aParentOwner: TVXBaseSceneObject);
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    { Controls and adjusts internal optimizations based on object's style.
      Advanced user only. }
    property ObjectStyle: TVXObjectStyles read FObjectStyle write FObjectStyle;
    { Returns the handle to the object's build list.
      Use with caution! Some objects don't support buildlists! }
    function GetHandle(var rci: TVXRenderContextInfo): Cardinal;
    function ListHandleAllocated: Boolean; inline;
    { The local transformation (relative to parent).
      If you're *sure* the local matrix is up-to-date, you may use LocalMatrix
      for quicker access. }
    procedure SetMatrix(const aValue: TMatrix); inline;
    property Matrix: PMatrix read GetMatrix;
    { Holds the local transformation (relative to parent).
      If you're not *sure* the local matrix is up-to-date, use Matrix property. }
    property LocalMatrix: PMatrix read GetLocalMatrix;
    { Forces the local matrix to the specified value.
      AbsoluteMatrix, InverseMatrix, etc. will honour that change, but
      may become invalid if the specified matrix isn't orthonormal (can
      be used for specific rendering or projection effects).
      The local matrix will be reset by the next TransformationChanged,
      position or attitude change. }
    procedure ForceLocalMatrix(const aMatrix: TMatrix); inline;
    { See AbsoluteMatrix. }
    function AbsoluteMatrixAsAddress: PMatrix;
    { Holds the absolute transformation matrix.
      If you're not *sure* the absolute matrix is up-to-date,
      use the AbsoluteMatrix property, this one may be nil... }
    property DirectAbsoluteMatrix: PMatrix read GetDirectAbsoluteMatrix;
    { Calculates the object's absolute inverse matrix.
      Multiplying an absolute coordinate with this matrix gives a local coordinate.
      The current implem uses transposition(AbsoluteMatrix), which is true
      unless you're using some scaling... }
    function InvAbsoluteMatrix: TMatrix; inline;
    { See InvAbsoluteMatrix. }
    function InvAbsoluteMatrixAsAddress: PMatrix;
    { The object's absolute matrix by composing all local matrices.
      Multiplying a local coordinate with this matrix gives an absolute coordinate. }
    property AbsoluteMatrix: TMatrix read GetAbsoluteMatrix write SetAbsoluteMatrix;
    { Direction vector in absolute coordinates. }
    property AbsoluteDirection: TVector read GetAbsoluteDirection write SetAbsoluteDirection;
    property AbsoluteAffineDirection: TAffineVector read GetAbsoluteAffineDirection write SetAbsoluteAffineDirection;
    { Scale vector in absolute coordinates.
      Warning: SetAbsoluteScale() does not work correctly at the moment. }
    property AbsoluteScale: TVector read GetAbsoluteScale write SetAbsoluteScale;
    property AbsoluteAffineScale: TAffineVector read GetAbsoluteAffineScale write SetAbsoluteAffineScale;
    { Up vector in absolute coordinates. }
    property AbsoluteUp: TVector read GetAbsoluteUp write SetAbsoluteUp;
    property AbsoluteAffineUp: TAffineVector read GetAbsoluteAffineUp write SetAbsoluteAffineUp;
    { Calculate the right vector in absolute coordinates. }
    function AbsoluteRight: TVector;
    { Calculate the left vector in absolute coordinates. }
    function AbsoluteLeft: TVector;
    { Computes and allows to set the object's absolute coordinates. }
    property AbsolutePosition: TVector read GetAbsolutePosition write SetAbsolutePosition;
    property AbsoluteAffinePosition: TAffineVector read GetAbsoluteAffinePosition write SetAbsoluteAffinePosition;
    function AbsolutePositionAsAddress: PVector;
    { Returns the Absolute X Vector expressed in local coordinates. }
    function AbsoluteXVector: TVector;
    { Returns the Absolute Y Vector expressed in local coordinates. }
    function AbsoluteYVector: TVector;
    { Returns the Absolute Z Vector expressed in local coordinates. }
    function AbsoluteZVector: TVector;
    { Converts a vector/point from absolute coordinates to local coordinates. }
    function AbsoluteToLocal(const v: TVector): TVector; overload;
    { Converts a vector from absolute coordinates to local coordinates. }
    function AbsoluteToLocal(const v: TAffineVector): TAffineVector; overload;
    { Converts a vector/point from local coordinates to absolute coordinates. }
    function LocalToAbsolute(const v: TVector): TVector; overload;
    { Converts a vector from local coordinates to absolute coordinates. }
    function LocalToAbsolute(const v: TAffineVector): TAffineVector; overload;
    { Returns the Right vector (based on Up and Direction) }
    function Right: TVector; inline;
    { Returns the Left vector (based on Up and Direction) }
    function LeftVector: TVector; inline;
    { Returns the Right vector (based on Up and Direction) }
    function AffineRight: TAffineVector; inline;
    { Returns the Left vector (based on Up and Direction) }
    function AffineLeftVector: TAffineVector; inline;
    { Calculates the object's square distance to a point/object.
      pt is assumed to be in absolute coordinates,
      AbsolutePosition is considered as being the object position. }
    function SqrDistanceTo(anObject: TVXBaseSceneObject): Single; overload;
    function SqrDistanceTo(const pt: TVector): Single; overload;
    function SqrDistanceTo(const pt: TAffineVector): Single; overload;
    { Computes the object's distance to a point/object.
      Only objects AbsolutePositions are considered. }
    function DistanceTo(anObject: TVXBaseSceneObject): Single; overload;
    function DistanceTo(const pt: TAffineVector): Single; overload;
    function DistanceTo(const pt: TVector): Single; overload;
    { Calculates the object's barycenter in absolute coordinates.
      Default behaviour is to consider Barycenter=AbsolutePosition
      (whatever the number of children).
      SubClasses where AbsolutePosition is not the barycenter should
      override this method as it is used for distance calculation, during
      rendering for instance, and may lead to visual inconsistencies. }
    function BarycenterAbsolutePosition: TVector; virtual;
    { Calculates the object's barycenter distance to a point. }
    function BarycenterSqrDistanceTo(const pt: TVector): Single;
    { Shall returns the object's axis aligned extensions.
      The dimensions are measured from object center and are expressed
      <i>with</i> scale accounted for, in the object's coordinates
      (not in absolute coordinates).
      Default value is half the object's Scale. }
    function AxisAlignedDimensions: TVector; virtual;
    function AxisAlignedDimensionsUnscaled: TVector; virtual;
    { Calculates and return the AABB for the object.
      The AABB is currently calculated from the BB.
      There is  no  caching scheme for them. }
    function AxisAlignedBoundingBox(const AIncludeChilden: Boolean = True): TAABB;
    function AxisAlignedBoundingBoxUnscaled(const AIncludeChilden: Boolean = True): TAABB;
    function AxisAlignedBoundingBoxAbsolute(const AIncludeChilden: Boolean = True;
      const AUseBaryCenter: Boolean = False): TAABB;
    { Advanced AABB functions that use a caching scheme.
      Also they include children and use BaryCenter. }
    function AxisAlignedBoundingBoxEx: TAABB;
    function AxisAlignedBoundingBoxAbsoluteEx: TAABB;
    { Calculates and return the Bounding Box for the object.
      The BB is calculated  each  time this method is invoked,
      based on the AxisAlignedDimensions of the object and that of its
      children.
      There is  no  caching scheme for them. }
    function BoundingBox(const AIncludeChilden: Boolean = True; const AUseBaryCenter: Boolean = False): THmgBoundingBox;
    function BoundingBoxUnscaled(const AIncludeChilden: Boolean = True; const AUseBaryCenter: Boolean = False): THmgBoundingBox;
    function BoundingBoxAbsolute(const AIncludeChilden: Boolean = True; const AUseBaryCenter: Boolean = False): THmgBoundingBox;
    { Advanced BB functions that use a caching scheme.
      Also they include children and use BaryCenter. }
    function BoundingBoxPersonalUnscaledEx: THmgBoundingBox;
    function BoundingBoxOfChildrenEx: THmgBoundingBox;
    function BoundingBoxIncludingChildrenEx: THmgBoundingBox;
    { Max distance of corners of the BoundingBox. }
    function BoundingSphereRadius: Single; inline;
    function BoundingSphereRadiusUnscaled: Single; inline;
    { Indicates if a point is within an object.
      Given coordinate is an absolute coordinate.
      Linear or surfacic objects shall always return False.
      Default value is based on AxisAlignedDimension and a cube bounding. }
    function PointInObject(const point: TVector): Boolean; virtual;
    { Request to determine an intersection with a casted ray.
      Given coordinates & vector are in absolute coordinates, rayVector
      must be normalized.
      rayStart may be a point inside the object, allowing retrieval of
      the multiple intersects of the ray.
      When intersectXXX parameters are nil (default) implementation should
      take advantage of this to optimize calculus, if not, and an intersect
      is found, non nil parameters should be defined.
      The intersectNormal needs NOT be normalized by the implementations.
      Default value is based on bounding sphere. }
    function RayCastIntersect(const rayStart, rayVector: TVector; intersectPoint: PVector = nil; intersectNormal: PVector = nil)
      : Boolean; virtual;
    { Request to generate silhouette outlines.
      Default implementation assumes the objects is a sphere of
      AxisAlignedDimensionUnscaled size. Subclasses may choose to return
      nil instead, which will be understood as an empty silhouette. }
    function GenerateSilhouette(const silhouetteParameters: TVXSilhouetteParameters): TVXSilhouette; virtual;
    property Children[Index: Integer]: TVXBaseSceneObject read Get; default;
    property Count: Integer read GetCount;
    property Index: Integer read GetIndex write SetIndex;
    // Create a new scene object and add it to this object as new child
    function AddNewChild(AChild: TVXSceneObjectClass): TVXBaseSceneObject; virtual;
    // Create a new scene object and add it to this object as first child
    function AddNewChildFirst(AChild: TVXSceneObjectClass): TVXBaseSceneObject; virtual;
    procedure AddChild(AChild: TVXBaseSceneObject); virtual;
    function GetOrCreateBehaviour(aBehaviour: TVXBehaviourClass): TVXBehaviour;
    function AddNewBehaviour(aBehaviour: TVXBehaviourClass): TVXBehaviour;
    function GetOrCreateEffect(anEffect: TVXObjectEffectClass): TVXObjectEffect;
    function AddNewEffect(anEffect: TVXObjectEffectClass): TVXObjectEffect;
    function HasSubChildren: Boolean;
    procedure DeleteChildren; virtual;
    procedure Insert(AIndex: Integer; AChild: TVXBaseSceneObject); virtual;
    { Takes a scene object out of the child list, but doesn't destroy it.
      If 'KeepChildren' is true its children will be kept as new children
      in this scene object. }
    procedure Remove(AChild: TVXBaseSceneObject; keepChildren: Boolean); virtual;
    function IndexOfChild(AChild: TVXBaseSceneObject): Integer;
    function FindChild(const aName: string; ownChildrenOnly: Boolean): TVXBaseSceneObject;
    { The "safe" version of this procedure checks if indexes are inside
      the list. If not, no exception if raised. }
    procedure ExchangeChildrenSafe(anIndex1, anIndex2: Integer);
    { The "regular" version of this procedure does not perform any checks
      and calls FChildren.Exchange directly. User should/can perform range
      checks manualy. }
    procedure ExchangeChildren(anIndex1, anIndex2: Integer);
    { These procedures are safe. }
    procedure MoveChildUp(anIndex: Integer);
    procedure MoveChildDown(anIndex: Integer);
    procedure MoveChildFirst(anIndex: Integer);
    procedure MoveChildLast(anIndex: Integer);
    procedure DoProgress(const progressTime: TVXProgressTimes); override;
    procedure MoveTo(newParent: TVXBaseSceneObject); virtual;
    procedure MoveUp;
    procedure MoveDown;
    procedure MoveFirst;
    procedure MoveLast;
    procedure BeginUpdate; inline;
    procedure EndUpdate; inline;
    { Make object-specific geometry description here.
      Subclasses should MAINTAIN OpenVX states (restore the states if
      they were altered). }
    procedure BuildList(var rci: TVXRenderContextInfo); virtual;
    function GetParentComponent: TComponent; override;
    function HasParent: Boolean; override; final;
    function IsUpdating: Boolean; inline;
    // Moves the object along the Up vector (move up/down)
    procedure Lift(ADistance: Single);
    // Moves the object along the direction vector
    procedure Move(ADistance: Single);
    // Translates the object
    procedure Translate(tx, ty, tz: Single);
    procedure MoveObjectAround(anObject: TVXBaseSceneObject; pitchDelta, turnDelta: Single);
    procedure MoveObjectAllAround(anObject: TVXBaseSceneObject; pitchDelta, turnDelta: Single);
    procedure Pitch(angle: Single);
    procedure Roll(angle: Single);
    procedure Turn(angle: Single);
    { Sets all rotations to zero and restores default Direction/Up.
      Using this function then applying roll/pitch/turn in the order that
      suits you, you can give an "absolute" meaning to rotation angles
      (they are still applied locally though).
      Scale and Position are not affected. }
    procedure ResetRotations;
    { Reset rotations and applies them back in the specified order. }
    procedure ResetAndPitchTurnRoll(const degX, degY, degZ: Single);
    { Applies rotations around absolute X, Y and Z axis. }
    procedure RotateAbsolute(const rx, ry, rz: Single); overload;
    { Applies rotations around the absolute given vector (angle in degrees). }
    procedure RotateAbsolute(const axis: TAffineVector; angle: Single); overload;
    // Moves camera along the right vector (move left and right)
    procedure Slide(ADistance: Single);
    // Orients the object toward a target object
    procedure PointTo(const ATargetObject: TVXBaseSceneObject; const AUpVector: TVector); overload;
    // Orients the object toward a target absolute position
    procedure PointTo(const AAbsolutePosition, AUpVector: TVector); overload;
    procedure Render(var ARci: TVXRenderContextInfo);
    procedure DoRender(var ARci: TVXRenderContextInfo; ARenderSelf, ARenderChildren: Boolean); virtual;
    procedure RenderChildren(firstChildIndex, lastChildIndex: Integer; var rci: TVXRenderContextInfo);
    procedure StructureChanged; virtual;
    procedure ClearStructureChanged; inline;
    { Recalculate an orthonormal system }
    procedure CoordinateChanged(Sender: TVXCustomCoordinates); override;
    procedure TransformationChanged; inline;
    procedure NotifyChange(Sender: TObject); override;
    property Rotation: TVXCoordinates read FRotation write SetRotation;
    property PitchAngle: Single read GetPitchAngle write SetPitchAngle;
    property RollAngle: Single read GetRollAngle write SetRollAngle;
    property TurnAngle: Single read GetTurnAngle write SetTurnAngle;
    property ShowAxes: Boolean read FShowAxes write SetShowAxes default False;
    property Changes: TObjectChanges read FChanges;
    property BBChanges: TObjectBBChanges read FBBChanges write SetBBChanges;
    property Parent: TVXBaseSceneObject read FParent write SetParent;
    property Position: TVXCoordinates read FPosition write SetPosition;
    property Direction: TVXCoordinates read FDirection write SetDirection;
    property Up: TVXCoordinates read FUp write SetUp;
    property Scale: TVXCoordinates read FScaling write SetScaling;
    property Scene: TVXScene read FScene;
    property Visible: Boolean read FVisible write SetVisible default True;
    property Pickable: Boolean read FPickable write SetPickable default True;
    property ObjectsSorting: TVXObjectsSorting read FObjectsSorting write SetObjectsSorting default osInherited;
    property VisibilityCulling: TVXVisibilityCulling read FVisibilityCulling write SetVisibilityCulling default vcInherited;
    property OnProgress: TVXProgressEvent read FOnProgress write FOnProgress;
    property OnPicked: TNotifyEvent read FOnPicked write FOnPicked;
    property OnAddedToParent: TNotifyEvent read FOnAddedToParent write FOnAddedToParent;
    property Behaviours: TVXBehaviours read GetBehaviours write SetBehaviours stored False;
    property Effects: TVXObjectEffects read GetEffects write SetEffects stored False;
    property TagObject: TObject read FTagObject write FTagObject;
  published
    property TagFloat: Single read FTagFloat write FTagFloat;
  end;

  { Base class for implementing behaviours in TVXScene.
    Behaviours are regrouped in a collection attached to a TVXBaseSceneObject,
    and are part of the "Progress" chain of events. Behaviours allows clean
    application of time-based alterations to objects (movements, shape or
    texture changes...).
    Since behaviours are implemented as classes, there are basicly two kinds
    of strategies for subclasses :
    stand-alone : the subclass does it all, and holds all necessary data
    (covers animation, inertia etc.)
    proxy : the subclass is an interface to and external, shared operator
    (like gravity, force-field effects etc.)

    Some behaviours may be cooperative (like force-fields affects inertia)
    or unique (e.g. only one inertia behaviour per object).
    NOTES : Don't forget to override the ReadFromFiler/WriteToFiler persistence
    methods if you add data in a subclass !
    Subclasses must be registered using the RegisterXCollectionItemClass function }
  TVXBaseBehaviour = class(TXCollectionItem)
  protected
    procedure SetName(const val: string); override;
    { Override this function to write subclass data. }
    procedure WriteToFiler(writer: TWriter); override;
    { Override this function to read subclass data. }
    procedure ReadFromFiler(reader: TReader); override;
    { Returns the TVXBaseSceneObject on which the behaviour should be applied.
      Does NOT check for nil owners. }
    function OwnerBaseSceneObject: TVXBaseSceneObject;
  public
    constructor Create(AOwner: TXCollection); override;
    destructor Destroy; override;
    procedure DoProgress(const progressTime: TVXProgressTimes); virtual;
  end;

  { Ancestor for non-rendering behaviours.
    This class shall never receive any properties, it's just here to differentiate
    rendereing and non-rendering behaviours. Rendereing behaviours are named
    "TVXObjectEffect", non-rendering effects (like inertia) are simply named
    "TVXBehaviour". }
  TVXBehaviour = class(TVXBaseBehaviour)
  end;

  { Holds a list of TVXBehaviour objects.
    This object expects itself to be owned by a TVXBaseSceneObject.
    As a TXCollection (and contrary to a TCollection), this list can contain
    objects of varying class, the only constraint being that they should all
    be TVXBehaviour subclasses. }
  TVXBehaviours = class(TXCollection)
  protected
    function GetBehaviour(Index: Integer): TVXBehaviour;
  public
    constructor Create(AOwner: TPersistent); override;
    function GetNamePath: string; override;
    class function ItemsClass: TXCollectionItemClass; override;
    property Behaviour[index: Integer]: TVXBehaviour read GetBehaviour; default;
    function CanAdd(aClass: TXCollectionItemClass): Boolean; override;
    procedure DoProgress(const progressTimes: TVXProgressTimes); inline;
  end;

  { A rendering effect that can be applied to SceneObjects.
    ObjectEffect is a subclass of behaviour that gets a chance to Render
    an object-related special effect.
    TVXObjectEffect should not be used as base class for custom effects,
    instead you should use the following base classes :
    TVXObjectPreEffect is rendered before owner object render
    TVXObjectPostEffect is rendered after the owner object render
    TVXObjectAfterEffect is rendered at the end of the scene rendering
    NOTES :
    Don't forget to override the ReadFromFiler/WriteToFiler persistence
    methods if you add data in a subclass !
    Subclasses must be registered using the RegisterXCollectionItemClass
    function }

  // TVXObjectEffectClass = class of TVXObjectEffect;

  TVXObjectEffect = class(TVXBaseBehaviour)
  protected
    { Override this function to write subclass data. }
    procedure WriteToFiler(writer: TWriter); override;
    { Override this function to read subclass data. }
    procedure ReadFromFiler(reader: TReader); override;
  public
    procedure Render(var rci: TVXRenderContextInfo); virtual;
  end;

  { An object effect that gets rendered before owner object's render.
    The current OpenVX matrices and material are that of the owner object. }
  TVXObjectPreEffect = class(TVXObjectEffect)
  end;

  { An object effect that gets rendered after owner object's render.
    The current OpenVX matrices and material are that of the owner object. }
  TVXObjectPostEffect = class(TVXObjectEffect)
  end;

  { An object effect that gets rendered at scene's end.
    No particular OpenVX matrices or material should be assumed. }
  TVXObjectAfterEffect = class(TVXObjectEffect)
  end;

  { Holds a list of object effects.
    This object expects itself to be owned by a TVXBaseSceneObject. }
  TVXObjectEffects = class(TXCollection)
  protected
    function GetEffect(Index: Integer): TVXObjectEffect;
  public
    constructor Create(AOwner: TPersistent); override;
    function GetNamePath: string; override;
    class function ItemsClass: TXCollectionItemClass; override;
    property ObjectEffect[index: Integer]: TVXObjectEffect read GetEffect; default;
    function CanAdd(aClass: TXCollectionItemClass): Boolean; override;
    procedure DoProgress(const progressTime: TVXProgressTimes);
    procedure RenderPreEffects(var rci: TVXRenderContextInfo); inline;
    { Also take care of registering after effects with the VXSceneViewer. }
    procedure RenderPostEffects(var rci: TVXRenderContextInfo); inline;
  end;

  { Extended base scene object class with a material property.
    The material allows defining a color and texture for the object, see TVXMaterial. }
  TVXCustomSceneObject = class(TVXBaseSceneObject)
  private
    FMaterial: TVXMaterial;
    FHint: string;
  protected
    function Blended: Boolean; override;
    procedure SetVKMaterial(aValue: TVXMaterial); inline;
    procedure DestroyHandle; override;
    procedure Loaded; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure DoRender(var ARci: TVXRenderContextInfo; ARenderSelf, ARenderChildren: Boolean); override;
    property Material: TVXMaterial read FMaterial write SetVKMaterial;
    property Hint: string read FHint write FHint;
  end;

  { This class shall be used only as a hierarchy root.
    It exists only as a container and shall never be rotated/scaled etc. as
    the class type is used in parenting optimizations.
    Shall never implement or add any functionality, the "Create" override
    only take cares of disabling the build list. }
  TVXSceneRootObject = class(TVXBaseSceneObject)
  public
    constructor Create(AOwner: TComponent); override;
  end;

  { Base class for objects that do not have a published "material".
    Note that the material is available in public properties, but isn't
    applied automatically before invoking BuildList.
    Subclassing should be reserved to structural objects and objects that
    have no material of their own. }
  TVXImmaterialSceneObject = class(TVXCustomSceneObject)
  public
    procedure DoRender(var ARci: TVXRenderContextInfo; ARenderSelf, ARenderChildren: Boolean); override;
  published
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

  { Base class for camera invariant objects.
    Camera invariant objects bypass camera settings, such as camera
    position (object is always centered on camera) or camera orientation
    (object always has same orientation as camera). }
  TVXCameraInvariantObject = class(TVXImmaterialSceneObject)
  private
    FCamInvarianceMode: TVXCameraInvarianceMode;
  protected
    procedure SetCamInvarianceMode(const val: TVXCameraInvarianceMode);
    property CamInvarianceMode: TVXCameraInvarianceMode read FCamInvarianceMode write SetCamInvarianceMode;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Assign(Source: TPersistent); override;
    procedure DoRender(var ARci: TVXRenderContextInfo; ARenderSelf, ARenderChildren: Boolean); override;
  end;

  { Base class for standard scene objects. Publishes the Material property. }
  TVXSceneObject = class(TVXCustomSceneObject)
  published
    property Material;
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

  { Event for user-specific rendering in a TVXDirectOpenVX object. }
  TDirectRenderEvent = procedure(Sender: TObject; var rci: TVXRenderContextInfo) of object;

  { Provides a way to issue direct OpenVX calls during the rendering.
    You can use this object to do your specific rendering task in its OnRender
    event. The OpenVX calls shall restore the OpenVX states they found when
    entering, or exclusively use the GLMisc utility functions to alter the
    states. }
  TVXDirectOpenVX = class(TVXImmaterialSceneObject)
  private
    FUseBuildList: Boolean;
    FOnRender: TDirectRenderEvent;
    FBlend: Boolean;
  protected
    procedure SetUseBuildList(const val: Boolean);
    function Blended: Boolean; override;
    procedure SetBlend(const val: Boolean);
  public
    constructor Create(AOwner: TComponent); override;
    procedure Assign(Source: TPersistent); override;
    procedure BuildList(var rci: TVXRenderContextInfo); override;
    function AxisAlignedDimensionsUnscaled: TVector; override;
  published
    { Specifies if a build list be made.
      If True, VXScene will generate a build list (side cache),
      ie. OnRender will only be invoked once for the first render, or after
      a StructureChanged call. This is suitable for "static" geometry and
      will usually speed up rendering of things that don't change.
      If false, OnRender will be invoked for each render. This is suitable
      for dynamic geometry (things that change often or constantly). }
    property UseBuildList: Boolean read FUseBuildList write SetUseBuildList;
    { Place your specific OpenVX code here.
      The OpenVX calls shall restore the OpenVX states they found when
      entering, or exclusively use the GLMisc utility functions to alter
      the states. }
    property OnRender: TDirectRenderEvent read FOnRender write FOnRender;
    { Defines if the object uses blending.
      This property will allow direct OpenVX objects to be flagged as
      blended for object sorting purposes. }
    property Blend: Boolean read FBlend write SetBlend;
  end;

  { Scene object that allows other objects to issue rendering at some point.
    This object is used to specify a render point for which other components
    have (rendering) tasks to perform. It doesn't render anything itself
    and is invisible, but other components can register and be notified
    when the point is reached in the rendering phase.
    Callbacks must be explicitly unregistered. }
  TVXRenderPoint = class(TVXImmaterialSceneObject)
  private
    FCallBacks: array of TDirectRenderEvent;
    FFreeCallBacks: array of TNotifyEvent;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure BuildList(var rci: TVXRenderContextInfo); override;
    procedure RegisterCallBack(renderEvent: TDirectRenderEvent; renderPointFreed: TNotifyEvent);
    procedure UnRegisterCallBack(renderEvent: TDirectRenderEvent);
    procedure Clear;
  end;

  { A full proxy object.
    This object literally uses another object's Render method to do its own
    rendering, however, it has a coordinate system and a life of its own.
    Use it for duplicates of an object. }
  TVXProxyObject = class(TVXBaseSceneObject)
  private
    FMasterObject: TVXBaseSceneObject;
    FProxyOptions: TVXProxyObjectOptions;
  protected
    FRendering: Boolean;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure SetMasterObject(const val: TVXBaseSceneObject); virtual;
    procedure SetProxyOptions(const val: TVXProxyObjectOptions);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure DoRender(var ARci: TVXRenderContextInfo; ARenderSelf, ARenderChildren: Boolean); override;
    function BarycenterAbsolutePosition: TVector; override;
    function AxisAlignedDimensions: TVector; override;
    function AxisAlignedDimensionsUnscaled: TVector; override;
    function RayCastIntersect(const rayStart, rayVector: TVector; intersectPoint: PVector = nil; intersectNormal: PVector = nil)
      : Boolean; override;
    function GenerateSilhouette(const silhouetteParameters: TVXSilhouetteParameters): TVXSilhouette; override;
  published
    { Specifies the Master object which will be proxy'ed. }
    property MasterObject: TVXBaseSceneObject read FMasterObject write SetMasterObject;
    { Specifies how and what is proxy'ed. }
    property ProxyOptions: TVXProxyObjectOptions read FProxyOptions write SetProxyOptions default cDefaultProxyOptions;
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

  TVXProxyObjectClass = class of TVXProxyObject;

  { Defines the various styles for lightsources.
    lsSpot : a spot light, oriented and with a cutoff zone (note that if
    cutoff is 180, the spot is rendered as an omni source)
    lsOmni : an omnidirectionnal source, punctual and sending light in
    all directions uniformously
    lsParallel : a parallel light, oriented as the light source is (this
    type of light can help speed up rendering) }
  TLightStyle = (lsSpot, lsOmni, lsParallel, lsParallelSpot);

  { Standard light source.
    The standard light source covers spotlights, omnidirectionnal and
    parallel sources (see TLightStyle).
    Lights are colored, have distance attenuation parameters and are turned
    on/off through their Shining property.
    Lightsources are managed in a specific object by the TVXScene for rendering
    purposes. The maximum number of light source in a scene is limited by the
    OpenVX implementation (8 lights are supported under most ICDs), though the
    more light you use, the slower rendering may get. If you want to render
    many more light/lightsource, you may have to resort to other techniques
    like lightmapping. }
  TVXLightSource = class(TVXBaseSceneObject)
  private
    FLightID: Cardinal;
    FSpotDirection: TVXCoordinates;
    FSpotExponent, FSpotCutOff: Single;
    FConstAttenuation, FLinearAttenuation, FQuadraticAttenuation: Single;
    FShining: Boolean;
    FAmbient, FDiffuse, FSpecular: TVXColor;
    FLightStyle: TLightStyle;
  protected
    procedure SetAmbient(aValue: TVXColor);
    procedure SetDiffuse(aValue: TVXColor);
    procedure SetSpecular(aValue: TVXColor);
    procedure SetConstAttenuation(aValue: Single);
    procedure SetLinearAttenuation(aValue: Single);
    procedure SetQuadraticAttenuation(aValue: Single);
    procedure SetShining(aValue: Boolean);
    procedure SetSpotDirection(AVector: TVXCoordinates);
    procedure SetSpotExponent(aValue: Single);
    procedure SetSpotCutOff(const val: Single);
    procedure SetLightStyle(const val: TLightStyle);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure DoRender(var ARci: TVXRenderContextInfo; ARenderSelf, ARenderChildren: Boolean); override;
    // light sources have different handle types than normal scene objects
    function RayCastIntersect(const rayStart, rayVector: TVector; intersectPoint: PVector = nil; intersectNormal: PVector = nil)
      : Boolean; override;
    procedure CoordinateChanged(Sender: TVXCustomCoordinates); override;
    function GenerateSilhouette(const silhouetteParameters: TVXSilhouetteParameters): TVXSilhouette; override;
    property LightID: Cardinal read FLightID;
    function Attenuated: Boolean;
  published
    property Ambient: TVXColor read FAmbient write SetAmbient;
    property ConstAttenuation: Single read FConstAttenuation write SetConstAttenuation;
    property Diffuse: TVXColor read FDiffuse write SetDiffuse;
    property LinearAttenuation: Single read FLinearAttenuation write SetLinearAttenuation;
    property QuadraticAttenuation: Single read FQuadraticAttenuation write SetQuadraticAttenuation;
    property Position;
    property LightStyle: TLightStyle read FLightStyle write SetLightStyle default lsSpot;
    property Shining: Boolean read FShining write SetShining default True;
    property Specular: TVXColor read FSpecular write SetSpecular;
    property SpotCutOff: Single read FSpotCutOff write SetSpotCutOff;
    property SpotDirection: TVXCoordinates read FSpotDirection write SetSpotDirection;
    property SpotExponent: Single read FSpotExponent write SetSpotExponent;
    property OnProgress;
  end;

  TVXCameraStyle = (csPerspective, csOrthogonal, csOrtho2D, csCustom, csInfinitePerspective, csPerspectiveKeepFOV);

  TVXCameraKeepFOVMode = (ckmHorizontalFOV, ckmVerticalFOV);

  TOnCustomPerspective = procedure(const viewport: TRectangle; width, height: Integer; DPI: Integer; var viewPortRadius: Single)
    of object;

  { Camera object.
    This object is commonly referred by TVXSceneViewer and defines a position,
    direction, focal length, depth of view... all the properties needed for
    defining a point of view and optical characteristics. }
  TVXCamera = class(TVXBaseSceneObject)
  private
    FFocalLength: Single;
    FDepthOfView: Single;
    FNearPlane: Single; // nearest distance to the camera
    FNearPlaneBias: Single; // scaling bias applied to near plane
    FViewPortRadius: Single; // viewport bounding radius per distance unit
    FTargetObject: TVXBaseSceneObject;
    FLastDirection: TVector; // Not persistent
    FCameraStyle: TVXCameraStyle;
    FKeepFOVMode: TVXCameraKeepFOVMode;
    FSceneScale: Single;
    FDeferredApply: TNotifyEvent;
    FOnCustomPerspective: TOnCustomPerspective;
    FDesign: Boolean;
    FFOVY, FFOVX: Double;
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure SetTargetObject(const val: TVXBaseSceneObject);
    procedure SetDepthOfView(aValue: Single);
    procedure SetFocalLength(aValue: Single);
    procedure SetCameraStyle(const val: TVXCameraStyle);
    procedure SetKeepFOVMode(const val: TVXCameraKeepFOVMode);
    procedure SetSceneScale(Value: Single);
    function StoreSceneScale: Boolean;
    procedure SetNearPlaneBias(Value: Single);
    function StoreNearPlaneBias: Boolean;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    { Nearest clipping plane for the frustum.
      This value depends on the FocalLength and DepthOfView fields and
      is calculated to minimize Z-Buffer crawling as suggested by the OpenVX documentation. }
    property NearPlane: Single read FNearPlane;

    // Apply camera transformation
    procedure Apply;
    procedure DoRender(var ARci: TVXRenderContextInfo; ARenderSelf, ARenderChildren: Boolean); override;
    function RayCastIntersect(const rayStart, rayVector: TVector; intersectPoint: PVector = nil; intersectNormal: PVector = nil)
      : Boolean; override;
    procedure ApplyPerspective(const AViewport: TRectangle; AWidth, AHeight: Integer; ADPI: Integer);
    procedure AutoLeveling(Factor: Single);
    procedure Reset(aSceneBuffer: TVXSceneBuffer);
    // Position the camera so that the whole scene can be seen
    procedure ZoomAll(aSceneBuffer: TVXSceneBuffer);
    procedure RotateObject(obj: TVXBaseSceneObject; pitchDelta, turnDelta: Single; rollDelta: Single = 0);
    procedure RotateTarget(pitchDelta, turnDelta: Single; rollDelta: Single = 0);
    { Change camera's position to make it move around its target.
      If TargetObject is nil, nothing happens. This method helps in quickly
      implementing camera controls. Camera's Up and Direction properties are unchanged.
      Angle deltas are in degrees, camera parent's coordinates should be identity.
      Tip : make the camera a child of a "target" dummycube and make
      it a target the dummycube. Now, to pan across the scene, just move
      the dummycube, to change viewing angle, use this method. }
    procedure MoveAroundTarget(pitchDelta, turnDelta: Single);
    { Change camera's position to make it move all around its target.
      If TargetObject is nil, nothing happens. This method helps in quickly
      implementing camera controls. Camera's Up and Direction properties are changed.
      Angle deltas are in degrees. }
    procedure MoveAllAroundTarget(pitchDelta, turnDelta: Single);
    { Moves the camera in eye space coordinates. }
    procedure MoveInEyeSpace(forwardDistance, rightDistance, upDistance: Single);
    { Moves the target in eye space coordinates. }
    procedure MoveTargetInEyeSpace(forwardDistance, rightDistance, upDistance: Single);
    { Computes the absolute vector corresponding to the eye-space translations. }
    function AbsoluteEyeSpaceVector(forwardDistance, rightDistance, upDistance: Single): TVector;
    { Adjusts distance from camera to target by applying a ratio.
      If TargetObject is nil, nothing happens. This method helps in quickly
      implementing camera controls. Only the camera's position is changed. }
    procedure AdjustDistanceToTarget(distanceRatio: Single);
    { Returns the distance from camera to target.
      If TargetObject is nil, returns 1. }
    function DistanceToTarget: Single;
    { Computes the absolute normalized vector to the camera target.
      If no target is defined, AbsoluteDirection is returned. }
    function AbsoluteVectorToTarget: TVector;
    { Computes the absolute normalized right vector to the camera target.
      If no target is defined, AbsoluteRight is returned. }
    function AbsoluteRightVectorToTarget: TVector;
    { Computes the absolute normalized up vector to the camera target.
      If no target is defined, AbsoluteUpt is returned. }
    function AbsoluteUpVectorToTarget: TVector;
    { Calculate an absolute translation vector from a screen vector.
      Ratio is applied to both screen delta, planeNormal should be the
      translation plane's normal. }
    function ScreenDeltaToVector(deltaX, deltaY: Integer; ratio: Single; const planeNormal: TVector): TVector;
    { Same as ScreenDeltaToVector but optimized for XY plane. }
    function ScreenDeltaToVectorXY(deltaX, deltaY: Integer; ratio: Single): TVector;
    { Same as ScreenDeltaToVector but optimized for XZ plane. }
    function ScreenDeltaToVectorXZ(deltaX, deltaY: Integer; ratio: Single): TVector;
    { Same as ScreenDeltaToVector but optimized for YZ plane. }
    function ScreenDeltaToVectorYZ(deltaX, deltaY: Integer; ratio: Single): TVector;
    { Returns true if a point is in front of the camera. }
    function PointInFront(const point: TVector): Boolean; overload;
    { Calculates the field of view in degrees, given a viewport dimension
      (width or height). F.i. you may wish to use the minimum of the two. }
    function GetFieldOfView(const AViewportDimension: Single): Single;
    { Sets the FocalLength in degrees, given a field of view and a viewport
      dimension (width or height). }
    procedure SetFieldOfView(const AFieldOfView, AViewportDimension: Single);
  published
    { Depth of field/view.
      Adjusts the maximum distance, beyond which objects will be clipped
      (ie. not visisble).
      You must adjust this value if you are experiencing disappearing
      objects (increase the value) of Z-Buffer crawling (decrease the
      value). Z-Buffer crawling happens when depth of view is too large
      and the Z-Buffer precision cannot account for all that depth
      accurately : objects farther overlap closer objects and vice-versa.
      Note that this value is ignored in cSOrtho2D mode. }
    property DepthOfView: Single read FDepthOfView write SetDepthOfView;
    { Focal Length of the camera.
      Adjusting this value allows for lens zooming effects (use SceneScale
      for linear zooming). This property affects near/far planes clipping. }
    property FocalLength: Single read FFocalLength write SetFocalLength;
    { Scene scaling for camera point.
      This is a linear 2D scaling of the camera's output, allows for
      linear zooming (use FocalLength for lens zooming). }
    property SceneScale: Single read FSceneScale write SetSceneScale stored StoreSceneScale;
    { Scaling bias applied to near-plane calculation.
      Values inferior to one will move the nearplane nearer, and also
      reduce medium/long range Z-Buffer precision, values superior
      to one will move the nearplane farther, and also improve medium/long
      range Z-Buffer precision. }
    property NearPlaneBias: Single read FNearPlaneBias write SetNearPlaneBias stored StoreNearPlaneBias;
    { If set, camera will point to this object.
      When camera is pointing an object, the Direction vector is ignored
      and the Up vector is used as an absolute vector to the up. }
    property TargetObject: TVXBaseSceneObject read FTargetObject write SetTargetObject;
    { Adjust the camera style.
      Three styles are available :
      csPerspective, the default value for perspective projection
      csOrthogonal, for orthogonal (or isometric) projection.
      csOrtho2D, setups orthogonal 2D projection in which 1 unit
      (in x or y) represents 1 pixel.
      csInfinitePerspective, for perspective view without depth limit.
      csKeepCamAnglePerspective, for perspective view with keeping aspect on view resize.
      csCustom, setup is deferred to the OnCustomPerspective event. }
    property CameraStyle: TVXCameraStyle read FCameraStyle write SetCameraStyle default csPerspective;
    { Keep camera angle mode.
      When CameraStyle is csKeepCamAnglePerspective, select which camera angle you want to keep.
      kaHeight, for Keep Height oriented camera angle
      kaWidth,  for Keep Width oriented camera angle }
    property KeepFOVMode: TVXCameraKeepFOVMode read FKeepFOVMode write SetKeepFOVMode default ckmHorizontalFOV;
    { Custom perspective event.
      This event allows you to specify your custom perpective, either
      with a glFrustrum, a glOrtho or whatever method suits you.
      You must compute viewPortRadius for culling to work.
      This event is only called if CameraStyle is csCustom. }
    property OnCustomPerspective: TOnCustomPerspective read FOnCustomPerspective write FOnCustomPerspective;
    property Position;
    property Direction;
    property Up;
    property OnProgress;
  end;

  { Scene object.
    The scene contains the scene description (lights, geometry...), which is
    basicly a hierarchical scene graph made of TVXBaseSceneObject. It will
    usually contain one or more TVXCamera object, which can be referred by
    a Viewer component for rendering purposes.
    The scene's objects can be accessed directly from Delphi code (as regular
    components), but those are edited with a specific editor (double-click
    on the TVXScene component at design-time to invoke it). To add objects
    at runtime, use the AddNewChild method of TVXBaseSceneObject. }
  TVXScene = class(TVXUpdateAbleComponent)
  private
    FUpdateCount: Integer;
    FObjects: TVXSceneRootObject;
    FBaseContext: TVXContext; // reference, not owned!
    FLights, FBuffers: TPersistentObjectList;
    FCurrentCamera: TVXCamera;
    FCurrentBuffer: TVXSceneBuffer;
    FObjectsSorting: TVXObjectsSorting;
    FVisibilityCulling: TVXVisibilityCulling;
    FOnBeforeProgress: TVXProgressEvent;
    FOnProgress: TVXProgressEvent;
    FCurrentDeltaTime: Double;
    FInitializableObjects: TVXInitializableObjectList;
  protected
    procedure AddLight(aLight: TVXLightSource);
    procedure RemoveLight(aLight: TVXLightSource);
    // Adds all lights in the subtree (anObj included)
    procedure AddLights(anObj: TVXBaseSceneObject);
    // Removes all lights in the subtree (anObj included)
    procedure RemoveLights(anObj: TVXBaseSceneObject);
    procedure GetChildren(AProc: TGetChildProc; Root: TComponent); override;
    procedure SetChildOrder(AChild: TComponent; Order: Integer); override;
    procedure SetObjectsSorting(const val: TVXObjectsSorting);
    procedure SetVisibilityCulling(const val: TVXVisibilityCulling);
    procedure ReadState(reader: TReader); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure BeginUpdate;
    procedure EndUpdate;
    function IsUpdating: Boolean;
    procedure AddBuffer(aBuffer: TVXSceneBuffer);
    procedure RemoveBuffer(aBuffer: TVXSceneBuffer);
    procedure SetupLights(maxLights: Integer);
    procedure NotifyChange(Sender: TObject); override;
    procedure Progress(const deltaTime, newTime: Double);
    function FindSceneObject(const aName: string): TVXBaseSceneObject;
    { Calculates, finds and returns the first object intercepted by the ray.
      Returns nil if no intersection was found. This function will be
      accurate only for objects that overrided their RayCastIntersect
      method with accurate code, otherwise, bounding sphere intersections
      will be returned. }
    function RayCastIntersect(const rayStart, rayVector: TVector; intersectPoint: PVector = nil; intersectNormal: PVector = nil)
      : TVXBaseSceneObject; virtual;
    procedure ShutdownAllLights;
    { Saves the scene to a file (recommended extension : .GLS) }
    procedure SaveToFile(const fileName: string);
    { Load the scene from a file.
      Existing objects/lights/cameras are freed, then the file is loaded.
      Delphi's IDE is not handling this behaviour properly yet, ie. if
      you load a scene in the IDE, objects will be properly loaded, but
      no declare will be placed in the code. }
    procedure LoadFromFile(const fileName: string);
    procedure SaveToStream(aStream: TStream);
    procedure LoadFromStream(aStream: TStream);
    { Saves the scene to a text file }
    procedure SaveToTextFile(const fileName: string);
    { Load the scene from a text files.
      See LoadFromFile for details. }
    procedure LoadFromTextFile(const fileName: string);
    property CurrentCamera: TVXCamera read FCurrentCamera;
    property Lights: TPersistentObjectList read FLights;
    property Objects: TVXSceneRootObject read FObjects;
    property CurrentBuffer: TVXSceneBuffer read FCurrentBuffer;
    { List of objects that request to be initialized when rendering context is active.
      They are removed automaticly from this list once initialized. }
    property InitializableObjects: TVXInitializableObjectList read FInitializableObjects;
    property CurrentDeltaTime: Double read FCurrentDeltaTime;
  published
    { Defines default ObjectSorting option for scene objects. }
    property ObjectsSorting: TVXObjectsSorting read FObjectsSorting write SetObjectsSorting default osRenderBlendedLast;
    { Defines default VisibilityCulling option for scene objects. }
    property VisibilityCulling: TVXVisibilityCulling read FVisibilityCulling write SetVisibilityCulling default vcNone;
    property OnBeforeProgress: TVXProgressEvent read FOnBeforeProgress write FOnBeforeProgress;
    property OnProgress: TVXProgressEvent read FOnProgress write FOnProgress;
  end;

  TFogMode = (fmLinear, fmExp, fmExp2);

  { Fog distance calculation mode.
    fdDefault: let OpenVX use its default formula
    fdEyeRadial: uses radial "true" distance (best quality)
    fdEyePlane: uses the distance to the projection plane (same as Z-Buffer, faster)
    Requires support of GL_NV_fog_distance extension, otherwise, it is ignored. }
  TFogDistance = (fdDefault, fdEyeRadial, fdEyePlane);

  { Parameters for fog environment in a scene.
    The fog descibed by this object is a distance-based fog, ie. the "intensity"
    of the fog is given by a formula depending solely on the distance, this
    intensity is used for blending to a fixed color. }
  TVXFogEnvironment = class(TVXUpdateAbleObject)
  private
    FSceneBuffer: TVXSceneBuffer;
    FFogColor: TVXColor; // alpha value means the fog density
    FFogStart, FFogEnd: Single;
    FFogMode: TFogMode;
    FFogDistance: TFogDistance;
  protected
    procedure SetFogColor(Value: TVXColor);
    procedure SetFogStart(Value: Single);
    procedure SetFogEnd(Value: Single);
    procedure SetFogMode(Value: TFogMode);
    procedure SetFogDistance(const val: TFogDistance);
  public
    constructor Create(AOwner: TPersistent); override;
    destructor Destroy; override;
    procedure ApplyFog;
    procedure Assign(Source: TPersistent); override;
    function IsAtDefaultValues: Boolean;
  published
    { Color of the fog when it is at 100% intensity. }
    property FogColor: TVXColor read FFogColor write SetFogColor;
    { Minimum distance for fog, what is closer is not affected. }
    property FogStart: Single read FFogStart write SetFogStart;
    { Maximum distance for fog, what is farther is at 100% fog intensity. }
    property FogEnd: Single read FFogEnd write SetFogEnd;
    { The formula used for converting distance to fog intensity. }
    property FogMode: TFogMode read FFogMode write SetFogMode default fmLinear;
    { Adjusts the formula used for calculating fog distances.
      This option is honoured if and only if the OpenVX ICD supports the
      GL_NV_fog_distance extension, otherwise, it is ignored.
      fdDefault: let OpenVX use its default formula
      fdEyeRadial: uses radial "true" distance (best quality)
      fdEyePlane: uses the distance to the projection plane  (same as Z-Buffer, faster) }
    property FogDistance: TFogDistance read FFogDistance write SetFogDistance default fdDefault;
  end;

  TVXDepthPrecision = (dpDefault, dp16bits, dp24bits, dp32bits);

  TVXColorDepth = (cdDefault, cd8bits, cd16bits, cd24bits, cdFloat64bits, cdFloat128bits); // float_type

  TVXShadeModel = (smDefault, smSmooth, smFlat);

  { Encapsulates an OpenVX frame/rendering buffer. }
  TVXSceneBuffer = class(TVXUpdateAbleObject)
  private
    // Internal state
    FRendering: Boolean;
    FRenderingContext: TVXContext;
    FAfterRenderEffects: TPersistentObjectList;
    FViewMatrixStack: array of TMatrix;
    FProjectionMatrixStack: array of TMatrix;
    FBaseProjectionMatrix: TMatrix;
    FCameraAbsolutePosition: TVector;
    FViewPort: TRectangle;
    FSelector: TVXBaseSelectTechnique;
    // Options & User Properties
    FFaceCulling, FFogEnable, FLighting: Boolean;
    FDepthTest: Boolean;
    FBackgroundColor: TColor;
    FBackgroundAlpha: Single;
    FAmbientColor: TVXColor;
    FAntiAliasing: TVXAntiAliasing;
    FDepthPrecision: TVXDepthPrecision;
    FColorDepth: TVXColorDepth;
    FContextOptions: TContextOptions;
    FShadeModel: TVXShadeModel;
    FRenderDPI: Integer;
    FFogEnvironment: TVXFogEnvironment;
    FAccumBufferBits: Integer;
    FLayer: TVXContextLayer;
    // Cameras
    FCamera: TVXCamera;
    // Freezing
    FFreezeBuffer: Pointer;
    FFreezed: Boolean;
    FFreezedViewPort: TRectangle;
    // Monitoring
    FFrameCount: Longint;
    FFramesPerSecond: Single;
    FFirstPerfCounter: Int64;
    FLastFrameTime: Single;
    // Events
    FOnChange: TNotifyEvent;
    FOnStructuralChange: TNotifyEvent;
    FOnPrepareGLContext: TNotifyEvent;
    FBeforeRender: TNotifyEvent;
    FViewerBeforeRender: TNotifyEvent;
    FPostRender: TNotifyEvent;
    FAfterRender: TNotifyEvent;
    FInitiateRendering: TDirectRenderEvent;
    FWrapUpRendering: TDirectRenderEvent;
    procedure SetLayer(const Value: TVXContextLayer);
  protected
    procedure SetBackgroundColor(AColor: TColor);
    procedure SetBackgroundAlpha(alpha: Single);
    procedure SetAmbientColor(AColor: TVXColor);
    function GetLimit(Which: TLimitType): Integer;
    procedure SetCamera(ACamera: TVXCamera);
    procedure SetContextOptions(Options: TContextOptions);
    procedure SetDepthTest(aValue: Boolean);
    procedure SetFaceCulling(aValue: Boolean);
    procedure SetLighting(aValue: Boolean);
    procedure SetAntiAliasing(const val: TVXAntiAliasing);
    procedure SetDepthPrecision(const val: TVXDepthPrecision);
    procedure SetColorDepth(const val: TVXColorDepth);
    procedure SetShadeModel(const val: TVXShadeModel);
    procedure SetFogEnable(aValue: Boolean);
    procedure SetFogEnvironment(aValue: TVXFogEnvironment);
    function StoreFog: Boolean;
    procedure SetAccumBufferBits(const val: Integer);
    procedure PrepareRenderingMatrices(const AViewport: TRectangle; resolution: Integer; pickingRect: PGLRect = nil);
    procedure DoBaseRender(const AViewport: TRectangle; resolution: Integer; drawState: TVXDrawState;
      baseObject: TVXBaseSceneObject);
    procedure SetupRenderingContext(Context: TVXContext);
    procedure SetupRCOptions(Context: TVXContext);
    procedure PrepareGLContext;
    procedure DoChange;
    procedure DoStructuralChange;
    // DPI for current/last render
    property RenderDPI: Integer read FRenderDPI;
    property OnPrepareGLContext: TNotifyEvent read FOnPrepareGLContext write FOnPrepareGLContext;
  public
    constructor Create(AOwner: TPersistent); override;
    destructor Destroy; override;
    procedure NotifyChange(Sender: TObject); override;
    procedure CreateRC(AWindowHandle: THandle; memoryContext: Boolean; // in VCL -> HWND
      BufferCount: Integer = 1); overload;
    procedure ClearBuffers;
    procedure DestroyRC;
    function RCInstantiated: Boolean;
    procedure Resize(newLeft, newTop, newWidth, newHeight: Integer);
    // Indicates hardware acceleration support
    function Acceleration: TVXContextAcceleration;
    // ViewPort for current/last render
    property viewport: TRectangle read FViewPort;
    // Fills the PickList with objects in Rect area
    procedure PickObjects(const rect: TVXRect; pickList: TVXPickList; objectCountGuess: Integer);
    { Returns a PickList with objects in Rect area.
      Returned list should be freed by caller.
      Objects are sorted by depth (nearest objects first). }
    function GetPickedObjects(const rect: TVXRect; objectCountGuess: Integer = 64): TVXPickList;
    // Returns the nearest object at x, y coordinates or nil if there is none
    function GetPickedObject(x, y: Integer): TVXBaseSceneObject;
    // Returns the color of the pixel at x, y in the frame buffer
    function GetPixelColor(x, y: Integer): TColor;
    { Returns the raw depth (Z buffer) of the pixel at x, y in the frame buffer.
      This value does not map to the actual eye-object distance, but to
      a depth buffer value in the [0; 1] range. }
    function GetPixelDepth(x, y: Integer): Single;
    { Converts a raw depth (Z buffer value) to frustrum distance.
      This calculation is only accurate for the pixel at the centre of the viewer,
      because it does not take into account that the corners of the frustrum
      are further from the eye than its centre. }
    function PixelDepthToDistance(aDepth: Single): Single;
    { Converts a raw depth (Z buffer value) to world distance.
      It also compensates for the fact that the corners of the frustrum
      are further from the eye, than its centre. }
    function PixelToDistance(x, y: Integer): Single;
    { Design time notification }
    procedure NotifyMouseMove(Shift: TShiftState; x, y: Single);
    { Renders the scene on the viewer.
      You do not need to call this method, unless you explicitly want a
      render at a specific time. If you just want the control to get
      refreshed, use Invalidate instead. }
    procedure Render(baseObject: TVXBaseSceneObject); overload;
    procedure Render; overload;
    procedure RenderScene(aScene: TVXScene; const viewPortSizeX, viewPortSizeY: Integer; drawState: TVXDrawState;
      baseObject: TVXBaseSceneObject);
    { Render the scene to a bitmap at given DPI.
      DPI = "dots per inch".
      The "magic" DPI of the screen is 96 under Windows. }
    procedure RenderToBitmap(ABitmap: TBitmap; DPI: Integer = 0);
    { Render the scene to a bitmap at given DPI and saves it to a file.
      DPI = "dots per inch".
      The "magic" DPI of the screen is 96 under Windows. }
    procedure RenderToFile(const AFile: string; DPI: Integer = 0); overload;
    { Renders to bitmap of given size, then saves it to a file.
      DPI is adjusted to make the bitmap similar to the viewer. }
    procedure RenderToFile(const AFile: string; bmpWidth, bmpHeight: Integer); overload;
    { Creates a TVXBitmap32 that is a snapshot of current OpenVX content.
      When possible, use this function instead of RenderToBitmap, it won't
      request a redraw and will be significantly faster.
      The returned TVXBitmap32 should be freed by calling code. }
    function CreateSnapShot: TVXImage;
    { Creates a FMX bitmap that is a snapshot of current OpenVX content. }
    function CreateSnapShotBitmap: TBitmap;
    procedure CopyToTexture(aTexture: TVXTexture); overload;
    procedure CopyToTexture(aTexture: TVXTexture; xSrc, ySrc, AWidth, AHeight: Integer; xDest, yDest: Integer;
      glCubeFace: GLEnum = 0); overload;
    { Save as raw float data to a file }
    procedure SaveAsFloatToFile(const aFilename: string);
    { Event reserved for viewer-specific uses. }
    property ViewerBeforeRender: TNotifyEvent read FViewerBeforeRender write FViewerBeforeRender stored False;
    procedure SetViewPort(x, y, W, H: Integer);
    function width: Integer;
    function height: Integer;
    { Indicates if the Viewer is "frozen". }
    property Freezed: Boolean read FFreezed;
    { Freezes rendering leaving the last rendered scene on the buffer. This
      is usefull in windowed applications for temporarily stopping rendering
      (when moving the window, for example). }
    procedure Freeze;
    { Restarts rendering after it was freezed. }
    procedure Melt;
    { Displays a window with info on current OpenVX ICD and context. }
    procedure ShowInfo(Modal: Boolean = False);
    { Currently Rendering? }
    property Rendering: Boolean read FRendering;
    { Adjusts background alpha channel. }
    property BackgroundAlpha: Single read FBackgroundAlpha write SetBackgroundAlpha;
    { Returns the projection matrix in use or used for the last rendering. }
    function ProjectionMatrix: TMatrix; deprecated;
    { Returns the view matrix in use or used for the last rendering. }
    function ViewMatrix: TMatrix; deprecated;
    function ModelMatrix: TMatrix; deprecated;
    { Returns the base projection matrix in use or used for the last rendering.
      The "base" projection is (as of now) either identity or the pick
      matrix, ie. it is the matrix on which the perspective or orthogonal
      matrix gets applied. }
    property BaseProjectionMatrix: TMatrix read FBaseProjectionMatrix;
    { Back up current View matrix and replace it with newMatrix.
      This method has no effect on theOpenVX matrix, only on the Buffer's
      matrix, and is intended for special effects rendering. }
    procedure PushViewMatrix(const newMatrix: TMatrix); deprecated;
    { Restore a View matrix previously pushed. }
    procedure PopViewMatrix; deprecated;
    procedure PushProjectionMatrix(const newMatrix: TMatrix); deprecated;
    procedure PopProjectionMatrix; deprecated;
    { Converts a screen pixel coordinate into 3D coordinates for orthogonal projection.
      This function accepts standard canvas coordinates, with (0,0) being
      the top left corner, and returns, when the camera is in orthogonal
      mode, the corresponding 3D world point that is in the camera's plane. }
    function OrthoScreenToWorld(screenX, screenY: Integer): TAffineVector; overload;
    { Converts a screen coordinate into world (3D) coordinates.
      This methods wraps a call to gluUnProject.
      Note that screen coord (0,0) is the lower left corner. }
    function ScreenToWorld(const aPoint: TAffineVector): TAffineVector; overload;
    function ScreenToWorld(const aPoint: TVector): TVector; overload;
    { Converts a screen pixel coordinate into 3D world coordinates.
      This function accepts standard canvas coordinates, with (0,0) being
      the top left corner. }
    function ScreenToWorld(screenX, screenY: Integer): TAffineVector; overload;
    { Converts an absolute world coordinate into screen coordinate.
      This methods wraps a call to gluProject.
      Note that screen coord (0,0) is the lower left corner. }
    function WorldToScreen(const aPoint: TAffineVector): TAffineVector; overload;
    function WorldToScreen(const aPoint: TVector): TVector; overload;
    { Converts a set of point absolute world coordinates into screen coordinates. }
    procedure WorldToScreen(points: PVector; nbPoints: Integer); overload;
    { Calculates the 3D vector corresponding to a 2D screen coordinate.
      The vector originates from the camera's absolute position and is
      expressed in absolute coordinates.
      Note that screen coord (0,0) is the lower left corner. }
    function ScreenToVector(const aPoint: TAffineVector): TAffineVector; overload;
    function ScreenToVector(const aPoint: TVector): TVector; overload;
    function ScreenToVector(const x, y: Integer): TVector; overload;
    { Calculates the 2D screen coordinate of a vector from the camera's
      absolute position and is expressed in absolute coordinates.
      Note that screen coord (0,0) is the lower left corner. }
    function VectorToScreen(const VectToCam: TAffineVector): TAffineVector;
    { Calculates intersection between a plane and screen vector.
      If an intersection is found, returns True and places result in
      intersectPoint. }
    function ScreenVectorIntersectWithPlane(const aScreenPoint: TVector; const planePoint, planeNormal: TVector;
      var intersectPoint: TVector): Boolean;
    { Calculates intersection between plane XY and screen vector.
      If an intersection is found, returns True and places result in
      intersectPoint. }
    function ScreenVectorIntersectWithPlaneXY(const aScreenPoint: TVector; const z: Single;
      var intersectPoint: TVector): Boolean;
    { Calculates intersection between plane YZ and screen vector.
      If an intersection is found, returns True and places result in
      intersectPoint. }
    function ScreenVectorIntersectWithPlaneYZ(const aScreenPoint: TVector; const x: Single;
      var intersectPoint: TVector): Boolean;
    { Calculates intersection between plane XZ and screen vector.
      If an intersection is found, returns True and places result in
      intersectPoint. }
    function ScreenVectorIntersectWithPlaneXZ(const aScreenPoint: TVector; const y: Single;
      var intersectPoint: TVector): Boolean;
    { Calculates a 3D coordinate from screen position and ZBuffer.
      This function returns a world absolute coordinate from a 2D point
      in the viewer, the depth being extracted from the ZBuffer data
      (DepthTesting and ZBuffer must be enabled for this function to work).
      Note that ZBuffer precision is not linear and can be quite low on
      some boards (either from compression or resolution approximations). }
    function PixelRayToWorld(x, y: Integer): TAffineVector;
    { Time (in second) spent to issue rendering order for the last frame.
      Be aware that since execution by the hardware isn't synchronous,
      this value may not be an accurate measurement of the time it took
      to render the last frame, it's a measurement of only the time it
      took to issue rendering orders. }
    property LastFrameTime: Single read FLastFrameTime;
    { Current FramesPerSecond rendering speed.
      You must keep the renderer busy to get accurate figures from this
      property.
      This is an average value, to reset the counter, call
      ResetPerfomanceMonitor. }
    property FramesPerSecond: Single read FFramesPerSecond;
    { Resets the perfomance monitor and begin a new statistics set.
      See FramesPerSecond. }
    procedure ResetPerformanceMonitor;
    { Retrieve one of the OpenVX limits for the current viewer.
      Limits include max texture size, OpenVX stack depth, etc. }
    property LimitOf[Which: TLimitType]: Integer read GetLimit;
    { Current rendering context.
      The context is a wrapper around platform-specific contexts
      (see TVXContext) and takes care of context activation and handle
      management. }
    property RenderingContext: TVXContext read FRenderingContext;
    { The camera from which the scene is rendered.
      A camera is an object you can add and define in a TVXScene component. }
    property Camera: TVXCamera read FCamera write SetCamera;
    { Specifies the layer plane that the rendering context is bound to. }
    property Layer: TVXContextLayer read FLayer write SetLayer default clMainPlane;
  published
    { Fog environment options. See TVXFogEnvironment. }
    property FogEnvironment: TVXFogEnvironment read FFogEnvironment write SetFogEnvironment stored StoreFog;
    { Color used for filling the background prior to any rendering. }
    property BackgroundColor: TColor read FBackgroundColor write SetBackgroundColor default TColors.SysBtnFace;
    { Scene ambient color vector.
      This ambient color is defined independantly from all lightsources,
      which can have their own ambient components. }
    property AmbientColor: TVXColor read FAmbientColor write SetAmbientColor;
    { Context options allows to setup specifics of the rendering context.
      Not all contexts support all options. }
    property ContextOptions: TContextOptions read FContextOptions write SetContextOptions
      default [roDoubleBuffer, roRenderToWindow, roDebugContext];
    { Number of precision bits for the accumulation buffer. }
    property AccumBufferBits: Integer read FAccumBufferBits write SetAccumBufferBits default 0;
    { DepthTest enabling.
      When DepthTest is enabled, objects closer to the camera will hide
      farther ones (via use of Z-Buffering).
      When DepthTest is disabled, the latest objects drawn/rendered overlap
      all previous objects, whatever their distance to the camera.
      Even when DepthTest is enabled, objects may chose to ignore depth
      testing through the osIgnoreDepthBuffer of their ObjectStyle property. }
    property DepthTest: Boolean read FDepthTest write SetDepthTest default True;
    { Enable or disable face culling in the renderer.
      Face culling is used in hidden faces removal algorithms : each face
      is given a normal or 'outside' direction. When face culling is enabled,
      only faces whose normal points towards the observer are rendered. }
    property FaceCulling: Boolean read FFaceCulling write SetFaceCulling default True;
    { Toggle to enable or disable the fog settings. }
    property FogEnable: Boolean read FFogEnable write SetFogEnable default False;
    { Toggle to enable or disable lighting calculations.
      When lighting is enabled, objects will be lit according to lightsources,
      when lighting is disabled, objects are rendered in their own colors,
      without any shading.
      Lighting does NOT generate shadows in OpenVX. }
    property Lighting: Boolean read FLighting write SetLighting default True;
    { AntiAliasing option.
      Ignored if not hardware supported, currently based on ARB_multisample. }
    property AntiAliasing: TVXAntiAliasing read FAntiAliasing write SetAntiAliasing default aaDefault;
    { Depth buffer precision.
      Default is highest available (below and including 24 bits) }
    property DepthPrecision: TVXDepthPrecision read FDepthPrecision write SetDepthPrecision default dpDefault;
    { Color buffer depth.
      Default depth buffer is highest available (below and including 24 bits) }
    property ColorDepth: TVXColorDepth read FColorDepth write SetColorDepth default cdDefault;
    { Shade model. Default is "Smooth". }
    property ShadeModel: TVXShadeModel read FShadeModel write SetShadeModel default smDefault;
    { Indicates a change in the scene or buffer options.
      A simple re-render is enough to take into account the changes. }
    property OnChange: TNotifyEvent read FOnChange write FOnChange stored False;
    { Indicates a structural change in the scene or buffer options.
      A reconstruction of the RC is necessary to take into account the
      changes (this may lead to a driver switch or lengthy operations). }
    property OnStructuralChange: TNotifyEvent read FOnStructuralChange write FOnStructuralChange stored False;
    { Triggered before the scene's objects get rendered.
      You may use this event to execute your own OpenVX rendering
      (usually background stuff). }
    property BeforeRender: TNotifyEvent read FBeforeRender write FBeforeRender stored False;
    { Triggered after BeforeRender, before rendering objects.
      This one is fired after the rci has been initialized and can be used
      to alter it or perform early renderings that require an rci,
      the Sender is the buffer. }
    property InitiateRendering: TDirectRenderEvent read FInitiateRendering write FInitiateRendering stored False;
    { Triggered after rendering all scene objects, before PostRender.
      This is the last point after which the rci becomes unavailable,
      the Sender is the buffer. }
    property WrapUpRendering: TDirectRenderEvent read FWrapUpRendering write FWrapUpRendering stored False;
    { Triggered just after all the scene's objects have been rendered.
      The OpenVX context is still active in this event, and you may use it
      to execute your own OpenVX rendering (usually for HUD, 2D overlays
      or after effects). }
    property PostRender: TNotifyEvent read FPostRender write FPostRender stored False;
    { Called after rendering.
      You cannot issue OpenVX calls in this event, if you want to do your own
      OpenVX stuff, use the PostRender event. }
    property AfterRender: TNotifyEvent read FAfterRender write FAfterRender stored False;
  end;

  { Base class for non-visual viewer.
    Non-visual viewer may actually render visuals, but they are non-visual
    (ie. non interactive) at design time. Such viewers include memory
    or full-screen viewers. }
  TVXNonVisualViewer = class(TComponent)
  private
    FBuffer: TVXSceneBuffer;
    FWidth, FHeight: Integer;
    FCubeMapRotIdx: Integer;
    FCubeMapZNear, FCubeMapZFar: Single;
    FCubeMapTranslation: TAffineVector;
    // FCreateTexture : Boolean;
  protected
    procedure SetBeforeRender(const val: TNotifyEvent);
    function GetBeforeRender: TNotifyEvent;
    procedure SetPostRender(const val: TNotifyEvent);
    function GetPostRender: TNotifyEvent;
    procedure SetAfterRender(const val: TNotifyEvent);
    function GetAfterRender: TNotifyEvent;
    procedure SetCamera(const val: TVXCamera);
    function GetCamera: TVXCamera;
    procedure SetBuffer(const val: TVXSceneBuffer);
    procedure SetWidth(const val: Integer);
    procedure SetHeight(const val: Integer);
    procedure SetupCubeMapCamera(Sender: TObject);
    procedure DoOnPrepareVXContext(Sender: TObject);
    procedure PrepareVXContext; virtual;
    procedure DoBufferChange(Sender: TObject); virtual;
    procedure DoBufferStructuralChange(Sender: TObject); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure Render(baseObject: TVXBaseSceneObject = nil); virtual; abstract;
    procedure CopyToTexture(aTexture: TVXTexture); overload; virtual;
    procedure CopyToTexture(aTexture: TVXTexture; xSrc, ySrc, width, height: Integer; xDest, yDest: Integer); overload;
    { CopyToTexture for Multiple-Render-Target }
    procedure CopyToTextureMRT(aTexture: TVXTexture; BufferIndex: Integer); overload; virtual;
    procedure CopyToTextureMRT(aTexture: TVXTexture; xSrc, ySrc, width, height: Integer; xDest, yDest: Integer;
      BufferIndex: Integer); overload;
    { Renders the 6 texture maps from a scene.
      The viewer is used to render the 6 images, one for each face
      of the cube, from the absolute position of the camera.
      This does NOT alter the content of the Pictures in the image,
      and will only change or define the content of textures as
      registered by OpenVX. }
    procedure RenderCubeMapTextures(cubeMapTexture: TVXTexture; zNear: Single = 0; zFar: Single = 0);
  published
    { Camera from which the scene is rendered. }
    property Camera: TVXCamera read GetCamera write SetCamera;
    property width: Integer read FWidth write SetWidth default 256;
    property height: Integer read FHeight write SetHeight default 256;
    { Triggered before the scene's objects get rendered.
      You may use this event to execute your own OpenVX rendering. }
    property BeforeRender: TNotifyEvent read GetBeforeRender write SetBeforeRender;
    { Triggered just after all the scene's objects have been rendered.
      The OpenVX context is still active in this event, and you may use it
      to execute your own OpenVX rendering. }
    property PostRender: TNotifyEvent read GetPostRender write SetPostRender;
    { Called after rendering.
      You cannot issue OpenVX calls in this event, if you want to do your own
      OpenVX stuff, use the PostRender event. }
    property AfterRender: TNotifyEvent read GetAfterRender write SetAfterRender;
    { Access to buffer properties. }
    property Buffer: TVXSceneBuffer read FBuffer write SetBuffer;
  end;

  { Component to render a scene to memory only.
    This component curently requires that the OpenVX ICD supports the
    WGL_ARB_pbuffer extension (indirectly). }
  TVXMemoryViewer = class(TVXNonVisualViewer)
  private
    FBufferCount: Integer;
    procedure SetBufferCount(const Value: Integer);
  public
    constructor Create(AOwner: TComponent); override;
    procedure InstantiateRenderingContext;
    procedure Render(baseObject: TVXBaseSceneObject = nil); override;
  published
    { Set BufferCount > 1 for multiple render targets.
      Users should check if the corresponding extension (GL_ATI_draw_buffers)
      is supported. Current hardware limit is BufferCount = 4. }
    property BufferCount: Integer read FBufferCount write SetBufferCount default 1;
  end;

  TInvokeInfoForm = procedure(aSceneBuffer: TVXSceneBuffer; Modal: Boolean);

  { Register an event handler triggered by any TVXBaseSceneObject Name change.
    *INCOMPLETE*, currently allows for only 1 (one) event, and is used by
    VXSceneEdit in the IDE. }
procedure RegisterGLBaseSceneObjectNameChangeEvent(notifyEvent: TNotifyEvent);
{ Deregister an event handler triggered by any TVXBaseSceneObject Name change.
  See RegisterVKBaseSceneObjectNameChangeEvent. }
procedure DeRegisterGLBaseSceneObjectNameChangeEvent(notifyEvent: TNotifyEvent);
{ Register an event handler triggered by any TVXBehaviour Name change.
  *INCOMPLETE*, currently allows for only 1 (one) event, and is used by
  FBehavioursEditor in the IDE. }
procedure RegisterGLBehaviourNameChangeEvent(notifyEvent: TNotifyEvent);
{ Deregister an event handler triggered by any TVXBaseSceneObject Name change.
  See RegisterVKBaseSceneObjectNameChangeEvent. }
procedure DeRegisterGLBehaviourNameChangeEvent(notifyEvent: TNotifyEvent);

{ Issues OpenVX calls for drawing X, Y, Z axes in a standard style. }
procedure AxesBuildList(var rci: TVXRenderContextInfo; pattern: Word; AxisLen: Single);

{ Registers the procedure call used to invoke the info form. }
procedure RegisterInfoForm(infoForm: TInvokeInfoForm);
procedure InvokeInfoForm(aSceneBuffer: TVXSceneBuffer; Modal: Boolean);

function GetCurrentRenderingObject: TVXBaseSceneObject;

var
  vCounterFrequency: Int64;
{$IFNDEF USE_MULTITHREAD}

var
{$ELSE}
  threadvar
{$ENDIF}
    vCurrentRenderingObject: TVXBaseSceneObject;

  // ------------------------------------------------------------------------------
implementation

// ------------------------------------------------------------------------------

function GetCurrentRenderingObject: TVXBaseSceneObject;
begin
  Result := vCurrentRenderingObject;
end;

procedure AxesBuildList(var rci: TVXRenderContextInfo; pattern: Word; AxisLen: Single);
begin
{$IFDEF USE_OPENGL_DEBUG}
  if GL.GREMEDY_string_marker then
    GL.StringMarkerGREMEDY(13, 'AxesBuildList');
{$ENDIF}
  with rci.VXStates do
  begin
    Disable(stLighting);
    if not rci.ignoreBlendingRequests then
    begin
      Enable(stBlend);
      SetBlendFunc(bfSrcAlpha, bfOneMinusSrcAlpha);
    end;
    LineWidth := 1;
    Enable(stLineStipple);
    LineStippleFactor := 1;
    LineStipplePattern := pattern;
    DepthWriteMask := False;
    DepthFunc := cfLEqual;
    if rci.bufferDepthTest then
      Enable(stDepthTest);
  end;
  glBegin(GL_LINES);
  glColor3f(0.5, 0.0, 0.0);
  glVertex3f(0, 0, 0);
  glVertex3f(-AxisLen, 0, 0);
  glColor3f(1.0, 0.0, 0.0);
  glVertex3f(0, 0, 0);
  glVertex3f(AxisLen, 0, 0);
  glColor3f(0.0, 0.5, 0.0);
  glVertex3f(0, 0, 0);
  glVertex3f(0, -AxisLen, 0);
  glColor3f(0.0, 1.0, 0.0);
  glVertex3f(0, 0, 0);
  glVertex3f(0, AxisLen, 0);
  glColor3f(0.0, 0.0, 0.5);
  glVertex3f(0, 0, 0);
  glVertex3f(0, 0, -AxisLen);
  glColor3f(0.0, 0.0, 1.0);
  glVertex3f(0, 0, 0);
  glVertex3f(0, 0, AxisLen);
  glEnd;
end;

var
  vInfoForm: TInvokeInfoForm = nil;

procedure RegisterInfoForm(infoForm: TInvokeInfoForm);
begin
  vInfoForm := infoForm;
end;

procedure InvokeInfoForm(aSceneBuffer: TVXSceneBuffer; Modal: Boolean);
begin
  if Assigned(vInfoForm) then
    vInfoForm(aSceneBuffer, Modal)
  else
    InformationDlg('InfoForm not available.');
end;

// ------------------ internal global routines ----------------------------------

var
  vBaseSceneObjectNameChangeEvent: TNotifyEvent;
  vBehaviourNameChangeEvent: TNotifyEvent;

procedure RegisterGLBaseSceneObjectNameChangeEvent(notifyEvent: TNotifyEvent);
begin
  vBaseSceneObjectNameChangeEvent := notifyEvent;
end;

procedure DeRegisterGLBaseSceneObjectNameChangeEvent(notifyEvent: TNotifyEvent);
begin
  vBaseSceneObjectNameChangeEvent := nil;
end;

procedure RegisterGLBehaviourNameChangeEvent(notifyEvent: TNotifyEvent);
begin
  vBehaviourNameChangeEvent := notifyEvent;
end;

procedure DeRegisterGLBehaviourNameChangeEvent(notifyEvent: TNotifyEvent);
begin
  vBehaviourNameChangeEvent := nil;
end;

// ------------------
// ------------------ TVXBaseSceneObject ------------------
// ------------------

constructor TVXBaseSceneObject.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FListHandle := TVXListHandle.Create;
  FObjectStyle := [];
  FChanges := [ocTransformation, ocStructure, ocAbsoluteMatrix, ocInvAbsoluteMatrix];
  FPosition := TVXCoordinates.CreateInitialized(Self, NullHmgPoint, csPoint);
  FRotation := TVXCoordinates.CreateInitialized(Self, NullHmgVector, csVector);
  FDirection := TVXCoordinates.CreateInitialized(Self, ZHmgVector, csVector);
  FUp := TVXCoordinates.CreateInitialized(Self, YHmgVector, csVector);
  FScaling := TVXCoordinates.CreateInitialized(Self, XYZHmgVector, csVector);
  FLocalMatrix := IdentityHmgMatrix;
  FVisible := True;
  FPickable := True;
  FObjectsSorting := osInherited;
  FVisibilityCulling := vcInherited;
  FChildren := TPersistentObjectList.Create;

  FBBChanges := [oBBcChild, oBBcStructure];
  FBoundingBoxPersonalUnscaled := NullBoundingBox;
  FBoundingBoxOfChildren := NullBoundingBox;
  FBoundingBoxIncludingChildren := NullBoundingBox;

  distList := TSingleList.Create;
  objList := TPersistentObjectList.Create;

end;

constructor TVXBaseSceneObject.CreateAsChild(aParentOwner: TVXBaseSceneObject);
begin
  Create(aParentOwner);
  aParentOwner.AddChild(Self);
end;

destructor TVXBaseSceneObject.Destroy;
begin
  DeleteChildCameras;
  // k00m memory fix and remove some leak of the old version.
  FGLObjectEffects.Free;
  FGLBehaviours.Free;
  FListHandle.Free;
  FPosition.Free;
  FRotation.Free;
  FDirection.Free;
  FUp.Free;
  FScaling.Free;
  if Assigned(FParent) then
    FParent.Remove(Self, False);
  DeleteChildren;
  FChildren.Free;
  objList.Free;
  distList.Free;

  inherited Destroy;
end;

function TVXBaseSceneObject.GetHandle(var rci: TVXRenderContextInfo): Cardinal;
begin

  // Special case.. dirty trixxors
  if not Assigned(FListHandle) then
  begin
    Result := 0;
    Exit;
  end;

  Result := FListHandle.Handle;
  if Result = 0 then
    Result := FListHandle.AllocateHandle;

  if ocStructure in FChanges then
  begin
    ClearStructureChanged;
    FListHandle.NotifyChangesOfData;
  end;

  if FListHandle.IsDataNeedUpdate then
  begin
    rci.VXStates.NewList(Result, GL_COMPILE);
    // try
    BuildList(rci);
    // finally
    rci.VXStates.EndList;
    // end;
    FListHandle.NotifyDataUpdated;
  end;
end;

function TVXBaseSceneObject.ListHandleAllocated: Boolean;
begin
  Result := Assigned(FListHandle) and (FListHandle.Handle <> 0) and not(ocStructure in FChanges);
end;

procedure TVXBaseSceneObject.DestroyHandle;
begin
  if Assigned(FListHandle) then
    FListHandle.DestroyHandle;
end;

procedure TVXBaseSceneObject.DestroyHandles;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    Children[i].DestroyHandles;
  DestroyHandle;
end;

procedure TVXBaseSceneObject.SetBBChanges(const Value: TObjectBBChanges);
begin
  if Value <> FBBChanges then
  begin
    FBBChanges := Value;
    if Assigned(FParent) then
      FParent.BBChanges := FParent.BBChanges + [oBBcChild];
  end;
end;

function TVXBaseSceneObject.Blended: Boolean;
begin
  Result := False;
end;

procedure TVXBaseSceneObject.BeginUpdate;
begin
  Inc(FUpdateCount);
end;

procedure TVXBaseSceneObject.EndUpdate;
begin
  if FUpdateCount > 0 then
  begin
    Dec(FUpdateCount);
    if FUpdateCount = 0 then
      NotifyChange(Self);
  end
  else
    Assert(False, strUnBalancedBeginEndUpdate);
end;

procedure TVXBaseSceneObject.BuildList(var rci: TVXRenderContextInfo);
begin
  // nothing
end;

procedure TVXBaseSceneObject.DeleteChildCameras;
var
  i: Integer;
  child: TVXBaseSceneObject;
begin
  i := 0;
  while i < FChildren.Count do
  begin
    child := TVXBaseSceneObject(FChildren.List^[i]);
    child.DeleteChildCameras;
    if child is TVXCamera then
    begin
      Remove(child, True);
      child.Free;
    end
    else
      Inc(i);
  end;
end;

procedure TVXBaseSceneObject.DeleteChildren;
var
  child: TVXBaseSceneObject;
begin
  DeleteChildCameras;
  if Assigned(FScene) then
    FScene.RemoveLights(Self);
  while FChildren.Count > 0 do
  begin
    child := TVXBaseSceneObject(FChildren.Pop);
    child.FParent := nil;
    child.Free;
  end;
  BBChanges := BBChanges + [oBBcChild];
end;

procedure TVXBaseSceneObject.Loaded;
begin
  inherited;
  FPosition.W := 1;
  if Assigned(FGLBehaviours) then
    FGLBehaviours.Loaded;
  if Assigned(FGLObjectEffects) then
    FGLObjectEffects.Loaded;
end;

procedure TVXBaseSceneObject.DefineProperties(Filer: TFiler);
begin
  inherited;
  { FOriginalFiler := Filer; }

  Filer.DefineBinaryProperty('BehavioursData', ReadBehaviours, WriteBehaviours,
    (Assigned(FGLBehaviours) and (FGLBehaviours.Count > 0)));
  Filer.DefineBinaryProperty('EffectsData', ReadEffects, WriteEffects,
    (Assigned(FGLObjectEffects) and (FGLObjectEffects.Count > 0)));
  { FOriginalFiler:=nil; }
end;

procedure TVXBaseSceneObject.WriteBehaviours(stream: TStream);
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

procedure TVXBaseSceneObject.ReadBehaviours(stream: TStream);
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

procedure TVXBaseSceneObject.WriteEffects(stream: TStream);
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

procedure TVXBaseSceneObject.ReadEffects(stream: TStream);
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

procedure TVXBaseSceneObject.WriteRotations(stream: TStream);
begin
  stream.Write(FRotation.AsAddress^, 3 * SizeOf(Single));
end;

procedure TVXBaseSceneObject.ReadRotations(stream: TStream);
begin
  stream.Read(FRotation.AsAddress^, 3 * SizeOf(Single));
end;

procedure TVXBaseSceneObject.DrawAxes(var rci: TVXRenderContextInfo; pattern: Word);
begin
  AxesBuildList(rci, pattern, rci.rcci.farClippingDistance - rci.rcci.nearClippingDistance);
end;

procedure TVXBaseSceneObject.GetChildren(AProc: TGetChildProc; Root: TComponent);
var
  i: Integer;
begin
  for i := 0 to FChildren.Count - 1 do
    if not IsSubComponent(TComponent(FChildren.List^[i])) then
      AProc(TComponent(FChildren.List^[i]));
end;

function TVXBaseSceneObject.Get(Index: Integer): TVXBaseSceneObject;
begin
  Result := TVXBaseSceneObject(FChildren[Index]);
end;

function TVXBaseSceneObject.GetCount: Integer;
begin
  Result := FChildren.Count;
end;

function TVXBaseSceneObject.GetDirectAbsoluteMatrix: PMatrix;
begin
  Result := @FAbsoluteMatrix;
end;

function TVXBaseSceneObject.HasSubChildren: Boolean;
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

procedure TVXBaseSceneObject.AddChild(AChild: TVXBaseSceneObject);
begin
  if Assigned(FScene) then
    FScene.AddLights(AChild);
  FChildren.Add(AChild);
  AChild.FParent := Self;
  AChild.SetScene(FScene);
  TransformationChanged;
  AChild.TransformationChanged;
  AChild.DoOnAddedToParent;
  BBChanges := BBChanges + [oBBcChild];
end;

function TVXBaseSceneObject.AddNewChild(AChild: TVXSceneObjectClass): TVXBaseSceneObject;
begin
  Result := AChild.Create(Owner);
  AddChild(Result);
end;

function TVXBaseSceneObject.AddNewChildFirst(AChild: TVXSceneObjectClass): TVXBaseSceneObject;
begin
  Result := AChild.Create(Owner);
  Insert(0, Result);
end;

function TVXBaseSceneObject.GetOrCreateBehaviour(aBehaviour: TVXBehaviourClass): TVXBehaviour;
begin
  Result := TVXBehaviour(Behaviours.GetOrCreate(aBehaviour));
end;

function TVXBaseSceneObject.AddNewBehaviour(aBehaviour: TVXBehaviourClass): TVXBehaviour;
begin
  Assert(Behaviours.CanAdd(aBehaviour));
  Result := aBehaviour.Create(Behaviours)
end;

function TVXBaseSceneObject.GetOrCreateEffect(anEffect: TVXObjectEffectClass): TVXObjectEffect;
begin
  Result := TVXObjectEffect(Effects.GetOrCreate(anEffect));
end;

function TVXBaseSceneObject.AddNewEffect(anEffect: TVXObjectEffectClass): TVXObjectEffect;
begin
  Assert(Effects.CanAdd(anEffect));
  Result := anEffect.Create(Effects)
end;

procedure TVXBaseSceneObject.RebuildMatrix;
begin
  if ocTransformation in Changes then
  begin
    VectorScale(LeftVector, Scale.x, FLocalMatrix.x);
    VectorScale(FUp.AsVector, Scale.y, FLocalMatrix.y);
    VectorScale(FDirection.AsVector, Scale.z, FLocalMatrix.z);
    SetVector(FLocalMatrix.W, FPosition.AsVector);
    Exclude(FChanges, ocTransformation);
    Include(FChanges, ocAbsoluteMatrix);
    Include(FChanges, ocInvAbsoluteMatrix);
  end;
end;

procedure TVXBaseSceneObject.ForceLocalMatrix(const aMatrix: TMatrix);
begin
  FLocalMatrix := aMatrix;
  Exclude(FChanges, ocTransformation);
  Include(FChanges, ocAbsoluteMatrix);
  Include(FChanges, ocInvAbsoluteMatrix);
end;

function TVXBaseSceneObject.AbsoluteMatrixAsAddress: PMatrix;
begin
  if ocAbsoluteMatrix in FChanges then
  begin
    RebuildMatrix;
    if Assigned(Parent) { and (not (Parent is TVXSceneRootObject)) } then
    begin
      MatrixMultiply(FLocalMatrix, TVXBaseSceneObject(Parent).AbsoluteMatrixAsAddress^, FAbsoluteMatrix);
    end
    else
      FAbsoluteMatrix := FLocalMatrix;
    Exclude(FChanges, ocAbsoluteMatrix);
    Include(FChanges, ocInvAbsoluteMatrix);
  end;
  Result := @FAbsoluteMatrix;
end;

function TVXBaseSceneObject.InvAbsoluteMatrix: TMatrix;
begin
  Result := InvAbsoluteMatrixAsAddress^;
end;

function TVXBaseSceneObject.InvAbsoluteMatrixAsAddress: PMatrix;
begin
  if ocInvAbsoluteMatrix in FChanges then
  begin
    if VectorEquals(Scale.DirectVector, XYZHmgVector) then
    begin
      RebuildMatrix;
      if Parent <> nil then
        FInvAbsoluteMatrix := MatrixMultiply(Parent.InvAbsoluteMatrixAsAddress^, AnglePreservingMatrixInvert(FLocalMatrix))
      else
        FInvAbsoluteMatrix := AnglePreservingMatrixInvert(FLocalMatrix);
    end
    else
    begin
      FInvAbsoluteMatrix := AbsoluteMatrixAsAddress^;
      InvertMatrix(FInvAbsoluteMatrix);
    end;
    Exclude(FChanges, ocInvAbsoluteMatrix);
  end;
  Result := @FInvAbsoluteMatrix;
end;

function TVXBaseSceneObject.GetAbsoluteMatrix: TMatrix;
begin
  Result := AbsoluteMatrixAsAddress^;
end;

procedure TVXBaseSceneObject.SetAbsoluteMatrix(const Value: TMatrix);
begin
  if not MatrixEquals(Value, FAbsoluteMatrix) then
  begin
    FAbsoluteMatrix := Value;
    if Parent <> nil then
      SetMatrix(MatrixMultiply(FAbsoluteMatrix, Parent.InvAbsoluteMatrixAsAddress^))
    else
      SetMatrix(Value);
  end;
end;

function TVXBaseSceneObject.GetAbsoluteDirection: TVector;
begin
  Result := VectorNormalize(AbsoluteMatrixAsAddress^.z);
end;

procedure TVXBaseSceneObject.SetAbsoluteDirection(const v: TVector);
begin
  if Parent <> nil then
    Direction.AsVector := Parent.AbsoluteToLocal(v)
  else
    Direction.AsVector := v;
end;

function TVXBaseSceneObject.GetAbsoluteScale: TVector;
begin
  Result.x := AbsoluteMatrixAsAddress^.x.x;
  Result.y := AbsoluteMatrixAsAddress^.y.y;
  Result.z := AbsoluteMatrixAsAddress^.z.z;

  Result.W := 0;
end;

procedure TVXBaseSceneObject.SetAbsoluteScale(const Value: TVector);
begin
  if Parent <> nil then
    Scale.AsVector := Parent.AbsoluteToLocal(Value)
  else
    Scale.AsVector := Value;
end;

function TVXBaseSceneObject.GetAbsoluteUp: TVector;
begin
  Result := VectorNormalize(AbsoluteMatrixAsAddress^.y);
end;

procedure TVXBaseSceneObject.SetAbsoluteUp(const v: TVector);
begin
  if Parent <> nil then
    Up.AsVector := Parent.AbsoluteToLocal(v)
  else
    Up.AsVector := v;
end;

function TVXBaseSceneObject.AbsoluteRight: TVector;
begin
  Result := VectorNormalize(AbsoluteMatrixAsAddress^.x);
end;

function TVXBaseSceneObject.AbsoluteLeft: TVector;
begin
  Result := VectorNegate(AbsoluteRight);
end;

function TVXBaseSceneObject.GetAbsolutePosition: TVector;
begin
  Result := AbsoluteMatrixAsAddress^.W;
end;

procedure TVXBaseSceneObject.SetAbsolutePosition(const v: TVector);
begin
  if Assigned(Parent) then
    Position.AsVector := Parent.AbsoluteToLocal(v)
  else
    Position.AsVector := v;
end;

function TVXBaseSceneObject.AbsolutePositionAsAddress: PVector;
begin
  Result := @AbsoluteMatrixAsAddress^.W;
end;

function TVXBaseSceneObject.AbsoluteXVector: TVector;
begin
  AbsoluteMatrixAsAddress;
  SetVector(Result, PAffineVector(@FAbsoluteMatrix.x)^);
end;

function TVXBaseSceneObject.AbsoluteYVector: TVector;
begin
  AbsoluteMatrixAsAddress;
  SetVector(Result, PAffineVector(@FAbsoluteMatrix.y)^);
end;

function TVXBaseSceneObject.AbsoluteZVector: TVector;
begin
  AbsoluteMatrixAsAddress;
  SetVector(Result, PAffineVector(@FAbsoluteMatrix.z)^);
end;

function TVXBaseSceneObject.AbsoluteToLocal(const v: TVector): TVector;
begin
  Result := VectorTransform(v, InvAbsoluteMatrixAsAddress^);
end;

function TVXBaseSceneObject.AbsoluteToLocal(const v: TAffineVector): TAffineVector;
begin
  Result := VectorTransform(v, InvAbsoluteMatrixAsAddress^);
end;

function TVXBaseSceneObject.LocalToAbsolute(const v: TVector): TVector;
begin
  Result := VectorTransform(v, AbsoluteMatrixAsAddress^);
end;

function TVXBaseSceneObject.LocalToAbsolute(const v: TAffineVector): TAffineVector;
begin
  Result := VectorTransform(v, AbsoluteMatrixAsAddress^);
end;

function TVXBaseSceneObject.Right: TVector;
begin
  Result := VectorCrossProduct(FDirection.AsVector, FUp.AsVector);
end;

function TVXBaseSceneObject.LeftVector: TVector;
begin
  Result := VectorCrossProduct(FUp.AsVector, FDirection.AsVector);
end;

function TVXBaseSceneObject.BarycenterAbsolutePosition: TVector;
begin
  Result := AbsolutePosition;
end;

function TVXBaseSceneObject.SqrDistanceTo(anObject: TVXBaseSceneObject): Single;
begin
  if Assigned(anObject) then
    Result := VectorDistance2(AbsolutePosition, anObject.AbsolutePosition)
  else
    Result := 0;
end;

function TVXBaseSceneObject.SqrDistanceTo(const pt: TVector): Single;
begin
  Result := VectorDistance2(pt, AbsolutePosition);
end;

function TVXBaseSceneObject.DistanceTo(anObject: TVXBaseSceneObject): Single;
begin
  if Assigned(anObject) then
    Result := VectorDistance(AbsolutePosition, anObject.AbsolutePosition)
  else
    Result := 0;
end;

function TVXBaseSceneObject.DistanceTo(const pt: TVector): Single;
begin
  Result := VectorDistance(AbsolutePosition, pt);
end;

function TVXBaseSceneObject.BarycenterSqrDistanceTo(const pt: TVector): Single;
var
  d: TVector;
begin
  d := BarycenterAbsolutePosition;
  Result := VectorDistance2(d, pt);
end;

function TVXBaseSceneObject.AxisAlignedDimensions: TVector;
begin
  Result := AxisAlignedDimensionsUnscaled();
  ScaleVector(Result, Scale.AsVector);
end;

function TVXBaseSceneObject.AxisAlignedDimensionsUnscaled: TVector;
begin
  Result.x := 0.5;
  Result.y := 0.5;
  Result.z := 0.5;
  Result.W := 0;
end;

function TVXBaseSceneObject.AxisAlignedBoundingBox(const AIncludeChilden: Boolean): TAABB;
var
  i: Integer;
  aabb: TAABB;
  child: TVXBaseSceneObject;
begin
  SetAABB(Result, AxisAlignedDimensionsUnscaled);
  // not tested for child objects
  if AIncludeChilden then
  begin
    for i := 0 to FChildren.Count - 1 do
    begin
      child := TVXBaseSceneObject(FChildren.List^[i]);
      aabb := child.AxisAlignedBoundingBoxUnscaled(AIncludeChilden);
      AABBTransform(aabb, child.Matrix^);
      AddAABB(Result, aabb);
    end;
  end;
  AABBScale(Result, Scale.AsAffineVector);
end;

function TVXBaseSceneObject.AxisAlignedBoundingBoxUnscaled(const AIncludeChilden: Boolean): TAABB;
var
  i: Integer;
  aabb: TAABB;
begin
  SetAABB(Result, AxisAlignedDimensionsUnscaled);
  // not tested for child objects
  if AIncludeChilden then
  begin
    for i := 0 to FChildren.Count - 1 do
    begin
      aabb := TVXBaseSceneObject(FChildren.List^[i]).AxisAlignedBoundingBoxUnscaled(AIncludeChilden);
      AABBTransform(aabb, TVXBaseSceneObject(FChildren.List^[i]).Matrix^);
      AddAABB(Result, aabb);
    end;
  end;
end;

function TVXBaseSceneObject.AxisAlignedBoundingBoxAbsolute(const AIncludeChilden: Boolean;
  const AUseBaryCenter: Boolean): TAABB;
begin
  Result := BBToAABB(BoundingBoxAbsolute(AIncludeChilden, AUseBaryCenter));
end;

function TVXBaseSceneObject.BoundingBox(const AIncludeChilden: Boolean; const AUseBaryCenter: Boolean): THmgBoundingBox;
var
  CurrentBaryOffset: TVector;
begin
  Result := AABBToBB(AxisAlignedBoundingBox(AIncludeChilden));

  // DaStr: code not tested...
  if AUseBaryCenter then
  begin
    CurrentBaryOffset := VectorSubtract(AbsoluteToLocal(BarycenterAbsolutePosition), Position.AsVector);
    OffsetBBPoint(Result, CurrentBaryOffset);
  end;
end;

function TVXBaseSceneObject.BoundingBoxUnscaled(const AIncludeChilden: Boolean; const AUseBaryCenter: Boolean): THmgBoundingBox;
var
  CurrentBaryOffset: TVector;
begin
  Result := AABBToBB(AxisAlignedBoundingBoxUnscaled(AIncludeChilden));

  // DaStr: code not tested...
  if AUseBaryCenter then
  begin
    CurrentBaryOffset := VectorSubtract(AbsoluteToLocal(BarycenterAbsolutePosition), Position.AsVector);
    OffsetBBPoint(Result, CurrentBaryOffset);
  end;
end;

function TVXBaseSceneObject.BoundingBoxAbsolute(const AIncludeChilden: Boolean; const AUseBaryCenter: Boolean): THmgBoundingBox;
var
  i: Integer;
  CurrentBaryOffset: TVector;
begin
  Result := BoundingBoxUnscaled(AIncludeChilden, False);
  for i := 0 to 7 do
    Result.BBox[i] := LocalToAbsolute(Result.BBox[i]);

  if AUseBaryCenter then
  begin
    CurrentBaryOffset := VectorSubtract(BarycenterAbsolutePosition, AbsolutePosition);
    OffsetBBPoint(Result, CurrentBaryOffset);
  end;
end;

function TVXBaseSceneObject.BoundingSphereRadius: Single;
begin
  Result := VectorLength(AxisAlignedDimensions);
end;

function TVXBaseSceneObject.BoundingSphereRadiusUnscaled: Single;
begin
  Result := VectorLength(AxisAlignedDimensionsUnscaled);
end;

function TVXBaseSceneObject.PointInObject(const point: TVector): Boolean;
var
  localPt, dim: TVector;
begin
  dim := AxisAlignedDimensions;
  localPt := VectorTransform(point, InvAbsoluteMatrix);
  Result := (Abs(localPt.x * Scale.x) <= dim.x) and (Abs(localPt.y * Scale.y) <= dim.y) and (Abs(localPt.z * Scale.z) <= dim.z);
end;

procedure TVXBaseSceneObject.CalculateBoundingBoxPersonalUnscaled(var ANewBoundingBox: THmgBoundingBox);
begin
  // Using the standard method to get the local BB.
  ANewBoundingBox := AABBToBB(AxisAlignedBoundingBoxUnscaled(False));
  OffsetBBPoint(ANewBoundingBox, AbsoluteToLocal(BarycenterAbsolutePosition));
end;

function TVXBaseSceneObject.BoundingBoxPersonalUnscaledEx: THmgBoundingBox;
begin
  if oBBcStructure in FBBChanges then
  begin
    CalculateBoundingBoxPersonalUnscaled(FBoundingBoxPersonalUnscaled);
    Exclude(FBBChanges, oBBcStructure);
  end;
  Result := FBoundingBoxPersonalUnscaled;
end;

function TVXBaseSceneObject.AxisAlignedBoundingBoxAbsoluteEx: TAABB;
var
  pBB: THmgBoundingBox;
begin
  pBB := BoundingBoxIncludingChildrenEx;
  BBTransform(pBB, AbsoluteMatrix);
  Result := BBToAABB(pBB);
end;

function TVXBaseSceneObject.AxisAlignedBoundingBoxEx: TAABB;
begin
  Result := BBToAABB(BoundingBoxIncludingChildrenEx);
  AABBScale(Result, Scale.AsAffineVector);
end;

function TVXBaseSceneObject.BoundingBoxOfChildrenEx: THmgBoundingBox;
var
  i: Integer;
  pBB: THmgBoundingBox;
begin
  if oBBcChild in FBBChanges then
  begin
    // Computing
    FBoundingBoxOfChildren := NullBoundingBox;
    for i := 0 to FChildren.Count - 1 do
    begin
      pBB := TVXBaseSceneObject(FChildren.List^[i]).BoundingBoxIncludingChildrenEx;
      if not BoundingBoxesAreEqual(@pBB, @NullBoundingBox) then
      begin
        // transformation with local matrix
        BBTransform(pBB, TVXBaseSceneObject(FChildren.List^[i]).Matrix^);
        if BoundingBoxesAreEqual(@FBoundingBoxOfChildren, @NullBoundingBox) then
          FBoundingBoxOfChildren := pBB
        else
          AddBB(FBoundingBoxOfChildren, pBB);
      end;
    end;
    Exclude(FBBChanges, oBBcChild);
  end;
  Result := FBoundingBoxOfChildren;
end;

function TVXBaseSceneObject.BoundingBoxIncludingChildrenEx: THmgBoundingBox;
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
      pBB := BoundingBoxOfChildrenEx;
      if not BoundingBoxesAreEqual(@pBB, @NullBoundingBox) then
        AddBB(FBoundingBoxIncludingChildren, pBB);
    end;
  end;
  Result := FBoundingBoxIncludingChildren;
end;

function TVXBaseSceneObject.RayCastIntersect(const rayStart, rayVector: TVector; intersectPoint: PVector = nil;
  intersectNormal: PVector = nil): Boolean;
var
  i1, i2, absPos: TVector;
begin
  SetVector(absPos, AbsolutePosition);
  if RayCastSphereIntersect(rayStart, rayVector, absPos, BoundingSphereRadius, i1, i2) > 0 then
  begin
    Result := True;
    if Assigned(intersectPoint) then
      SetVector(intersectPoint^, i1);
    if Assigned(intersectNormal) then
    begin
      SubtractVector(i1, absPos);
      NormalizeVector(i1);
      SetVector(intersectNormal^, i1);
    end;
  end
  else
    Result := False;
end;

function TVXBaseSceneObject.GenerateSilhouette(const silhouetteParameters: TVXSilhouetteParameters): TVXSilhouette;
const
  cNbSegments = 21;
var
  i, j: Integer;
  d, r, vr, s, c, angleFactor: Single;
  sVec, tVec: TAffineVector;
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
  tVec := VectorCrossProduct(silhouetteParameters.SeenFrom, sVec);
  NormalizeVector(sVec);
  NormalizeVector(tVec);
  // generate the silhouette (outline and capping)
  Result := TVXSilhouette.Create;
  angleFactor := (2 * PI) / cNbSegments;
  vr := vr * 0.98;
  for i := 0 to cNbSegments - 1 do
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

procedure TVXBaseSceneObject.Assign(Source: TPersistent);
var
  i: Integer;
  child, newChild: TVXBaseSceneObject;
begin
  if Assigned(Source) and (Source is TVXBaseSceneObject) then
  begin
    DestroyHandles;
    FVisible := TVXBaseSceneObject(Source).FVisible;
    TVXBaseSceneObject(Source).RebuildMatrix;
    SetMatrix(TVXBaseSceneObject(Source).FLocalMatrix);
    FShowAxes := TVXBaseSceneObject(Source).FShowAxes;
    FObjectsSorting := TVXBaseSceneObject(Source).FObjectsSorting;
    FVisibilityCulling := TVXBaseSceneObject(Source).FVisibilityCulling;
    FRotation.Assign(TVXBaseSceneObject(Source).FRotation);
    DeleteChildren;
    if Assigned(Scene) then
      Scene.BeginUpdate;
    if Assigned(TVXBaseSceneObject(Source).FChildren) then
    begin
      for i := 0 to TVXBaseSceneObject(Source).FChildren.Count - 1 do
      begin
        child := TVXBaseSceneObject(TVXBaseSceneObject(Source).FChildren[i]);
        newChild := AddNewChild(TVXSceneObjectClass(child.ClassType));
        newChild.Assign(child);
      end;
    end;
    if Assigned(Scene) then
      Scene.EndUpdate;
    OnProgress := TVXBaseSceneObject(Source).OnProgress;
    if Assigned(TVXBaseSceneObject(Source).FGLBehaviours) then
      Behaviours.Assign(TVXBaseSceneObject(Source).Behaviours)
    else
      FreeAndNil(FGLBehaviours);
    if Assigned(TVXBaseSceneObject(Source).FGLObjectEffects) then
      Effects.Assign(TVXBaseSceneObject(Source).Effects)
    else
      FreeAndNil(FGLObjectEffects);
    Tag := TVXBaseSceneObject(Source).Tag;
    FTagFloat := TVXBaseSceneObject(Source).FTagFloat;
  end
  else
    inherited Assign(Source);
end;

function TVXBaseSceneObject.IsUpdating: Boolean;
begin
  Result := (FUpdateCount <> 0) or (csReading in ComponentState);
end;

function TVXBaseSceneObject.GetParentComponent: TComponent;
begin
  if FParent is TVXSceneRootObject then
    Result := FScene
  else
    Result := FParent;
end;

function TVXBaseSceneObject.HasParent: Boolean;
begin
  Result := Assigned(FParent);
end;

procedure TVXBaseSceneObject.Lift(ADistance: Single);
begin
  FPosition.AddScaledVector(ADistance, FUp.AsVector);
  TransformationChanged;
end;

procedure TVXBaseSceneObject.Move(ADistance: Single);
begin
  FPosition.AddScaledVector(ADistance, FDirection.AsVector);
  TransformationChanged;
end;

procedure TVXBaseSceneObject.Slide(ADistance: Single);
begin
  FPosition.AddScaledVector(ADistance, Right);
  TransformationChanged;
end;

procedure TVXBaseSceneObject.ResetRotations;
begin
  FillChar(FLocalMatrix, SizeOf(TMatrix), 0);
  FLocalMatrix.x.x := Scale.DirectX;
  FLocalMatrix.y.y := Scale.DirectY;
  FLocalMatrix.z.z := Scale.DirectZ;
  SetVector(FLocalMatrix.W, Position.DirectVector);
  FRotation.DirectVector := NullHmgPoint;
  FDirection.DirectVector := ZHmgVector;
  FUp.DirectVector := YHmgVector;
  TransformationChanged;
  Exclude(FChanges, ocTransformation);
end;

procedure TVXBaseSceneObject.ResetAndPitchTurnRoll(const degX, degY, degZ: Single);
var
  rotMatrix: TMatrix;
  v: TVector;
begin
  ResetRotations;
  // set DegX (Pitch)
  rotMatrix := CreateRotationMatrix(Right, degX * cPIdiv180);
  v := VectorTransform(FUp.AsVector, rotMatrix);
  NormalizeVector(v);
  FUp.DirectVector := v;
  v := VectorTransform(FDirection.AsVector, rotMatrix);
  NormalizeVector(v);
  FDirection.DirectVector := v;
  FRotation.DirectX := NormalizeDegAngle(degX);
  // set DegY (Turn)
  rotMatrix := CreateRotationMatrix(FUp.AsVector, degY * cPIdiv180);
  v := VectorTransform(FUp.AsVector, rotMatrix);
  NormalizeVector(v);
  FUp.DirectVector := v;
  v := VectorTransform(FDirection.AsVector, rotMatrix);
  NormalizeVector(v);
  FDirection.DirectVector := v;
  FRotation.DirectY := NormalizeDegAngle(degY);
  // set DegZ (Roll)
  rotMatrix := CreateRotationMatrix(Direction.AsVector, degZ * cPIdiv180);
  v := VectorTransform(FUp.AsVector, rotMatrix);
  NormalizeVector(v);
  FUp.DirectVector := v;
  v := VectorTransform(FDirection.AsVector, rotMatrix);
  NormalizeVector(v);
  FDirection.DirectVector := v;
  FRotation.DirectZ := NormalizeDegAngle(degZ);
  TransformationChanged;
  NotifyChange(Self);
end;

procedure TVXBaseSceneObject.RotateAbsolute(const rx, ry, rz: Single);
var
  resMat: TMatrix;
  v: TAffineVector;
begin
  resMat := Matrix^;
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
  SetMatrix(resMat);
end;

procedure TVXBaseSceneObject.RotateAbsolute(const axis: TAffineVector; angle: Single);
var
  v: TAffineVector;
begin
  if angle <> 0 then
  begin
    SetVector(v, AbsoluteToLocal(axis));
    SetMatrix(MatrixMultiply(CreateRotationMatrix(v, DegToRadian(angle)), Matrix^));
  end;
end;

procedure TVXBaseSceneObject.Pitch(angle: Single);
var
  r: Single;
  rightVector: TVector;
begin
  FIsCalculating := True;
  try
    angle := -DegToRad(angle);
    rightVector := Right;
    FUp.Rotate(rightVector, angle);
    FUp.Normalize;
    FDirection.Rotate(rightVector, angle);
    FDirection.Normalize;
    r := -RadToDeg(ArcTan2(FDirection.y, VectorLength(FDirection.x, FDirection.z)));
    if FDirection.x < 0 then
      if FDirection.y < 0 then
        r := 180 - r
      else
        r := -180 - r;
    FRotation.x := r;
  finally
    FIsCalculating := False;
  end;
  TransformationChanged;
end;

procedure TVXBaseSceneObject.SetPitchAngle(aValue: Single);
var
  diff: Single;
  rotMatrix: TMatrix;
begin
  if aValue <> FRotation.x then
  begin
    if not(csLoading in ComponentState) then
    begin
      FIsCalculating := True;
      // try
      diff := DegToRadian(FRotation.x - aValue);
      rotMatrix := CreateRotationMatrix(Right, diff);
      FUp.DirectVector := VectorTransform(FUp.AsVector, rotMatrix);
      FUp.Normalize;
      FDirection.DirectVector := VectorTransform(FDirection.AsVector, rotMatrix);
      FDirection.Normalize;
      TransformationChanged;
      // finally
      FIsCalculating := False;
      // end;
    end;
    FRotation.DirectX := NormalizeDegAngle(aValue);
  end;
end;

// Roll
//

procedure TVXBaseSceneObject.Roll(angle: Single);
var
  r: Single;
  rightVector, directionVector: TVector;
begin
  FIsCalculating := True;
  try
    angle := DegToRadian(angle);
    directionVector := Direction.AsVector;
    FUp.Rotate(directionVector, angle);
    FUp.Normalize;
    FDirection.Rotate(directionVector, angle);
    FDirection.Normalize;

    // calculate new rotation angle from vectors
    rightVector := Right;
    r := -RadToDeg(ArcTan2(rightVector.y, VectorLength(rightVector.x, rightVector.z)));
    if rightVector.x < 0 then
      if rightVector.y < 0 then
        r := 180 - r
      else
        r := -180 - r;
    FRotation.z := r;
  finally
    FIsCalculating := False;
  end;
  TransformationChanged;
end;

procedure TVXBaseSceneObject.SetRollAngle(aValue: Single);
var
  diff: Single;
  rotMatrix: TMatrix;
begin
  if aValue <> FRotation.z then
  begin
    if not(csLoading in ComponentState) then
    begin
      FIsCalculating := True;
      // try
      diff := DegToRadian(FRotation.z - aValue);
      rotMatrix := CreateRotationMatrix(Direction.AsVector, diff);
      FUp.DirectVector := VectorTransform(FUp.AsVector, rotMatrix);
      FUp.Normalize;
      FDirection.DirectVector := VectorTransform(FDirection.AsVector, rotMatrix);
      FDirection.Normalize;
      TransformationChanged;
      // finally
      FIsCalculating := False;
      // end;
    end;
    FRotation.DirectZ := NormalizeDegAngle(aValue);
  end;
end;

procedure TVXBaseSceneObject.Turn(angle: Single);
var
  r: Single;
  upVector: TVector;
begin
  FIsCalculating := True;
  try
    angle := DegToRadian(angle);
    upVector := Up.AsVector;
    FUp.Rotate(upVector, angle);
    FUp.Normalize;
    FDirection.Rotate(upVector, angle);
    FDirection.Normalize;
    r := -RadToDeg(ArcTan2(FDirection.x, VectorLength(FDirection.y, FDirection.z)));
    if FDirection.x < 0 then
      if FDirection.y < 0 then
        r := 180 - r
      else
        r := -180 - r;
    FRotation.y := r;
  finally
    FIsCalculating := False;
  end;
  TransformationChanged;
end;

procedure TVXBaseSceneObject.SetTurnAngle(aValue: Single);
var
  diff: Single;
  rotMatrix: TMatrix;
begin
  if aValue <> FRotation.y then
  begin
    if not(csLoading in ComponentState) then
    begin
      FIsCalculating := True;
      // try
      diff := DegToRadian(FRotation.y - aValue);
      rotMatrix := CreateRotationMatrix(Up.AsVector, diff);
      FUp.DirectVector := VectorTransform(FUp.AsVector, rotMatrix);
      FUp.Normalize;
      FDirection.DirectVector := VectorTransform(FDirection.AsVector, rotMatrix);
      FDirection.Normalize;
      TransformationChanged;
      // finally
      FIsCalculating := False;
      // end;
    end;
    FRotation.DirectY := NormalizeDegAngle(aValue);
  end;
end;

procedure TVXBaseSceneObject.SetRotation(aRotation: TVXCoordinates);
begin
  FRotation.Assign(aRotation);
  TransformationChanged;
end;

function TVXBaseSceneObject.GetPitchAngle: Single;
begin
  Result := FRotation.x;
end;

function TVXBaseSceneObject.GetTurnAngle: Single;
begin
  Result := FRotation.y;
end;

function TVXBaseSceneObject.GetRollAngle: Single;
begin
  Result := FRotation.z;
end;

procedure TVXBaseSceneObject.PointTo(const ATargetObject: TVXBaseSceneObject; const AUpVector: TVector);
begin
  PointTo(ATargetObject.AbsolutePosition, AUpVector);
end;

procedure TVXBaseSceneObject.PointTo(const AAbsolutePosition, AUpVector: TVector);
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
    FUp.AsVector := Parent.AbsoluteToLocal(absUp);
  end
  else
  begin
    FDirection.AsVector := absDir;
    FUp.AsVector := absUp;
  end;
  TransformationChanged
end;

procedure TVXBaseSceneObject.SetShowAxes(aValue: Boolean);
begin
  if FShowAxes <> aValue then
  begin
    FShowAxes := aValue;
    NotifyChange(Self);
  end;
end;

procedure TVXBaseSceneObject.SetScaling(aValue: TVXCoordinates);
begin
  FScaling.Assign(aValue);
  TransformationChanged;
end;

procedure TVXBaseSceneObject.SetName(const NewName: TComponentName);
begin
  if Name <> NewName then
  begin
    inherited SetName(NewName);
    if Assigned(vBaseSceneObjectNameChangeEvent) then
      vBaseSceneObjectNameChangeEvent(Self);
  end;
end;

procedure TVXBaseSceneObject.SetParent(const val: TVXBaseSceneObject);
begin
  MoveTo(val);
end;

function TVXBaseSceneObject.GetIndex: Integer;
begin
  if Assigned(FParent) then
    Result := FParent.FChildren.IndexOf(Self)
  else
    Result := -1;
end;

function TVXBaseSceneObject.GetLocalMatrix: PMatrix;
begin
  Result := @FLocalMatrix;
end;

procedure TVXBaseSceneObject.SetIndex(aValue: Integer);
var
  LCount: Integer;
  parentBackup: TVXBaseSceneObject;
begin
  if Assigned(FParent) then
  begin
    if aValue < 0 then
      aValue := 0;
    LCount := FParent.Count;
    if aValue >= LCount then
      aValue := LCount - 1;
    if aValue <> Index then
    begin
      if Assigned(FScene) then
        FScene.BeginUpdate;
      parentBackup := FParent;
      parentBackup.Remove(Self, False);
      parentBackup.Insert(aValue, Self);
      if Assigned(FScene) then
        FScene.EndUpdate;
    end;
  end;
end;

procedure TVXBaseSceneObject.SetParentComponent(Value: TComponent);
begin
  inherited;
  if Value = FParent then
    Exit;

  if Value is TVXScene then
    SetParent(TVXScene(Value).Objects)
  else if Value is TVXBaseSceneObject then
    SetParent(TVXBaseSceneObject(Value))
  else
    SetParent(nil);
end;

procedure TVXBaseSceneObject.StructureChanged;
begin
  if not(ocStructure in FChanges) then
  begin
    Include(FChanges, ocStructure);
    NotifyChange(Self);
  end
  else if osDirectDraw in ObjectStyle then
    NotifyChange(Self);
end;

procedure TVXBaseSceneObject.ClearStructureChanged;
begin
  Exclude(FChanges, ocStructure);
  SetBBChanges(BBChanges + [oBBcStructure]);
end;

procedure TVXBaseSceneObject.RecTransformationChanged;
var
  i: Integer;
  List: PPointerObjectList;
  matSet: TObjectChanges;
begin
  matSet := [ocAbsoluteMatrix, ocInvAbsoluteMatrix];
  if matSet * FChanges <> matSet then
  begin
    FChanges := FChanges + matSet;
    List := FChildren.List;
    for i := 0 to FChildren.Count - 1 do
      TVXBaseSceneObject(List^[i]).RecTransformationChanged;
  end;
end;

procedure TVXBaseSceneObject.TransformationChanged;
begin
  if not(ocTransformation in FChanges) then
  begin
    Include(FChanges, ocTransformation);
    RecTransformationChanged;
    if not(csLoading in ComponentState) then
      NotifyChange(Self);
  end;
end;

procedure TVXBaseSceneObject.MoveTo(newParent: TVXBaseSceneObject);
begin
  if newParent = FParent then
    Exit;
  if Assigned(FParent) then
  begin
    FParent.Remove(Self, False);
    FParent := nil;
  end;
  if Assigned(newParent) then
    newParent.AddChild(Self)
  else
    SetScene(nil);
end;

procedure TVXBaseSceneObject.MoveUp;
begin
  if Assigned(Parent) then
    Parent.MoveChildUp(Parent.IndexOfChild(Self));
end;

procedure TVXBaseSceneObject.MoveDown;
begin
  if Assigned(Parent) then
    Parent.MoveChildDown(Parent.IndexOfChild(Self));
end;

procedure TVXBaseSceneObject.MoveFirst;
begin
  if Assigned(Parent) then
    Parent.MoveChildFirst(Parent.IndexOfChild(Self));
end;

procedure TVXBaseSceneObject.MoveLast;
begin
  if Assigned(Parent) then
    Parent.MoveChildLast(Parent.IndexOfChild(Self));
end;

procedure TVXBaseSceneObject.MoveObjectAround(anObject: TVXBaseSceneObject; pitchDelta, turnDelta: Single);
var
  originalT2C, normalT2C, normalCameraRight, newPos: TVector;
  pitchNow, dist: Single;
begin
  if Assigned(anObject) then
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
    // creates a new vector pointing up and then rotate it down
    // into the new position
    SetVector(normalT2C, AbsoluteUp);
    RotateVector(normalT2C, normalCameraRight, -pitchNow);
    RotateVector(normalT2C, AbsoluteUp, -DegToRadian(turnDelta));
    ScaleVector(normalT2C, dist);
    newPos := VectorAdd(AbsolutePosition, VectorSubtract(normalT2C, originalT2C));
    if Assigned(Parent) then
      newPos := Parent.AbsoluteToLocal(newPos);
    Position.AsVector := newPos;
  end;
end;

procedure TVXBaseSceneObject.MoveObjectAllAround(anObject: TVXBaseSceneObject; pitchDelta, turnDelta: Single);
var
  upVector: TVector;
  lookat: TVector;
  rightVector: TVector;
  tempvector: TVector;
  T2C: TVector;

begin

  // if camera has got a target
  if Assigned(anObject) then
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
      upVector := VectorCrossProduct(tempvector, lookat);
      rightVector := VectorCrossProduct(lookat, upVector);
    end
    else
    begin
      rightVector := VectorCrossProduct(lookat, upVector);
      upVector := VectorCrossProduct(rightVector, lookat);
    end;
    // now the up right and lookat vector are orthogonal

    // vector Target to camera
    T2C := VectorSubtract(AbsolutePosition, anObject.AbsolutePosition);
    RotateVector(T2C, rightVector, DegToRadian(-pitchDelta));
    RotateVector(T2C, upVector, DegToRadian(-turnDelta));
    AbsolutePosition := VectorAdd(anObject.AbsolutePosition, T2C);

    // now update new up vector
    RotateVector(upVector, rightVector, DegToRadian(-pitchDelta));
    AbsoluteUp := upVector;
    AbsoluteDirection := VectorSubtract(anObject.AbsolutePosition, AbsolutePosition);

  end;
end;

procedure TVXBaseSceneObject.CoordinateChanged(Sender: TVXCustomCoordinates);
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

procedure TVXBaseSceneObject.DoProgress(const progressTime: TVXProgressTimes);
var
  i: Integer;
begin
  for i := FChildren.Count - 1 downto 0 do
    TVXBaseSceneObject(FChildren.List^[i]).DoProgress(progressTime);
  if Assigned(FGLBehaviours) then
    FGLBehaviours.DoProgress(progressTime);
  if Assigned(FGLObjectEffects) then
    FGLObjectEffects.DoProgress(progressTime);
  if Assigned(FOnProgress) then
    with progressTime do
      FOnProgress(Self, deltaTime, newTime);
end;

procedure TVXBaseSceneObject.Insert(AIndex: Integer; AChild: TVXBaseSceneObject);
begin
  with FChildren do
  begin
    if Assigned(AChild.FParent) then
      AChild.FParent.Remove(AChild, False);
    Insert(AIndex, AChild);
  end;
  AChild.FParent := Self;
  if AChild.FScene <> FScene then
    AChild.DestroyHandles;
  AChild.SetScene(FScene);
  if Assigned(FScene) then
    FScene.AddLights(AChild);
  AChild.TransformationChanged;

  AChild.DoOnAddedToParent;
end;

procedure TVXBaseSceneObject.Remove(AChild: TVXBaseSceneObject; keepChildren: Boolean);
var
  i: Integer;
begin
  if not Assigned(FChildren) then
    Exit;
  if AChild.Parent = Self then
  begin
    if Assigned(FScene) then
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

function TVXBaseSceneObject.IndexOfChild(AChild: TVXBaseSceneObject): Integer;
begin
  Result := FChildren.IndexOf(AChild)
end;

function TVXBaseSceneObject.FindChild(const aName: string; ownChildrenOnly: Boolean): TVXBaseSceneObject;
var
  i: Integer;
  res: TVXBaseSceneObject;
begin
  res := nil;
  Result := nil;
  for i := 0 to FChildren.Count - 1 do
  begin
    if CompareText(TVXBaseSceneObject(FChildren[i]).Name, aName) = 0 then
    begin
      res := TVXBaseSceneObject(FChildren[i]);
      Break;
    end;
  end;
  if not ownChildrenOnly then
  begin
    for i := 0 to FChildren.Count - 1 do
      with TVXBaseSceneObject(FChildren[i]) do
      begin
        Result := FindChild(aName, ownChildrenOnly);
        if Assigned(Result) then
          Break;
      end;
  end;
  if not Assigned(Result) then
    Result := res;
end;

procedure TVXBaseSceneObject.ExchangeChildren(anIndex1, anIndex2: Integer);
begin
  Assert(Assigned(FChildren), 'No children found!');
  FChildren.Exchange(anIndex1, anIndex2);
  NotifyChange(Self);
end;

procedure TVXBaseSceneObject.ExchangeChildrenSafe(anIndex1, anIndex2: Integer);
begin
  Assert(Assigned(FChildren), 'No children found!');
  if (anIndex1 < FChildren.Count) and (anIndex2 < FChildren.Count) and (anIndex1 > -1) and (anIndex2 > -1) and
    (anIndex1 <> anIndex2) then
  begin
    FChildren.Exchange(anIndex1, anIndex2);
    NotifyChange(Self);
  end;
end;

procedure TVXBaseSceneObject.MoveChildUp(anIndex: Integer);
begin
  Assert(Assigned(FChildren), 'No children found!');
  if anIndex > 0 then
  begin
    FChildren.Exchange(anIndex, anIndex - 1);
    NotifyChange(Self);
  end;
end;

procedure TVXBaseSceneObject.MoveChildDown(anIndex: Integer);
begin
  Assert(Assigned(FChildren), 'No children found!');
  if anIndex < FChildren.Count - 1 then
  begin
    FChildren.Exchange(anIndex, anIndex + 1);
    NotifyChange(Self);
  end;
end;

procedure TVXBaseSceneObject.MoveChildFirst(anIndex: Integer);
begin
  Assert(Assigned(FChildren), 'No children found!');
  if anIndex <> 0 then
  begin
    FChildren.Move(anIndex, 0);
    NotifyChange(Self);
  end;
end;

procedure TVXBaseSceneObject.MoveChildLast(anIndex: Integer);
begin
  Assert(Assigned(FChildren), 'No children found!');
  if anIndex <> FChildren.Count - 1 then
  begin
    FChildren.Move(anIndex, FChildren.Count - 1);
    NotifyChange(Self);
  end;
end;

procedure TVXBaseSceneObject.Render(var ARci: TVXRenderContextInfo);
var
  shouldRenderSelf, shouldRenderChildren: Boolean;
  aabb: TAABB;
  master: TObject;
begin
{$IFDEF USE_OPENGL_DEBUG}
  if GL.GREMEDY_string_marker then
    GL.StringMarkerGREMEDY(Length(Name) + Length('.Render'), PGLChar(String(Name + '.Render')));
{$ENDIF}
  if (ARci.drawState = dsPicking) and not FPickable then
    Exit;
  // visibility culling determination
  if ARci.VisibilityCulling in [vcObjectBased, vcHierarchical] then
  begin
    if ARci.VisibilityCulling = vcObjectBased then
    begin
      shouldRenderSelf := (osNoVisibilityCulling in ObjectStyle) or
        (not IsVolumeClipped(BarycenterAbsolutePosition, BoundingSphereRadius, ARci.rcci.frustum));
      shouldRenderChildren := Assigned(FChildren);
    end
    else
    begin // vcHierarchical
      aabb := AxisAlignedBoundingBox;
      shouldRenderSelf := (osNoVisibilityCulling in ObjectStyle) or
        (not IsVolumeClipped(aabb.min, aabb.max, ARci.rcci.frustum));
      shouldRenderChildren := shouldRenderSelf and Assigned(FChildren);
    end;
    if not(shouldRenderSelf or shouldRenderChildren) then
      Exit;
  end
  else
  begin
    Assert(ARci.VisibilityCulling in [vcNone, vcInherited], 'Unknown visibility culling option');
    shouldRenderSelf := True;
    shouldRenderChildren := FChildren.Count > 0;
  end;

  // Prepare Matrix and PickList stuff
  ARci.PipeLineTransformation.Push;

  if ocTransformation in FChanges then
    RebuildMatrix;

  if ARci.proxySubObject then
    ARci.PipeLineTransformation.SetModelMatrix(MatrixMultiply(LocalMatrix^, ARci.PipeLineTransformation.ModelMatrix^))
  else
    ARci.PipeLineTransformation.SetModelMatrix(AbsoluteMatrix);

  master := nil;
  if ARci.drawState = dsPicking then
  begin
    if ARci.proxySubObject then
      master := TVXSceneBuffer(ARci.Buffer).FSelector.CurrentObject;
    TVXSceneBuffer(ARci.Buffer).FSelector.CurrentObject := Self;
  end;

  // Start rendering
  if shouldRenderSelf then
  begin
    vCurrentRenderingObject := Self;
{$IFNDEF USE_OPTIMIZATIONS}
    if FShowAxes then
      DrawAxes(ARci, $CCCC);
{$ENDIF}
    if Assigned(FGLObjectEffects) and (FGLObjectEffects.Count > 0) then
    begin
      ARci.PipeLineTransformation.Push;
      FGLObjectEffects.RenderPreEffects(ARci);
      ARci.PipeLineTransformation.Pop;

      ARci.PipeLineTransformation.Push;
      if osIgnoreDepthBuffer in ObjectStyle then
      begin
        ARci.VXStates.Disable(stDepthTest);
        DoRender(ARci, True, shouldRenderChildren);
        ARci.VXStates.Enable(stDepthTest);
      end
      else
        DoRender(ARci, True, shouldRenderChildren);

      FGLObjectEffects.RenderPostEffects(ARci);
      ARci.PipeLineTransformation.Pop;
    end
    else
    begin
      if osIgnoreDepthBuffer in ObjectStyle then
      begin
        ARci.VXStates.Disable(stDepthTest);
        DoRender(ARci, True, shouldRenderChildren);
        ARci.VXStates.Enable(stDepthTest);
      end
      else
        DoRender(ARci, True, shouldRenderChildren);
    end;
    vCurrentRenderingObject := nil;
  end
  else
  begin
    if (osIgnoreDepthBuffer in ObjectStyle) and TVXSceneBuffer(ARci.Buffer).DepthTest then
    begin
      ARci.VXStates.Disable(stDepthTest);
      DoRender(ARci, False, shouldRenderChildren);
      ARci.VXStates.Enable(stDepthTest);
    end
    else
      DoRender(ARci, False, shouldRenderChildren);
  end;
  // Pop Name & Matrix
  if Assigned(master) then
    TVXSceneBuffer(ARci.Buffer).FSelector.CurrentObject := master;
  ARci.PipeLineTransformation.Pop;
end;

procedure TVXBaseSceneObject.DoRender(var ARci: TVXRenderContextInfo; ARenderSelf, ARenderChildren: Boolean);
begin
  // start rendering self
  if ARenderSelf then
  begin
    if (osDirectDraw in ObjectStyle) or ARci.amalgamating then
      BuildList(ARci)
    else
      ARci.VXStates.CallList(GetHandle(ARci));
  end;
  // start rendering children (if any)
  if ARenderChildren then
    Self.RenderChildren(0, Count - 1, ARci);
end;

procedure TVXBaseSceneObject.RenderChildren(firstChildIndex, lastChildIndex: Integer; var rci: TVXRenderContextInfo);
var
  i: Integer;

  plist: PPointerObjectList;
  obj: TVXBaseSceneObject;
  oldSorting: TVXObjectsSorting;
  oldCulling: TVXVisibilityCulling;
begin

  oldCulling := rci.VisibilityCulling;
  if Self.VisibilityCulling <> vcInherited then
    rci.VisibilityCulling := Self.VisibilityCulling;
  if lastChildIndex = firstChildIndex then
  begin
    obj := TVXBaseSceneObject(FChildren.List^[firstChildIndex]);
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
            obj := TVXBaseSceneObject(plist^[i]);
            if obj.Visible then
              obj.Render(rci);
          end;
        end;
      osRenderFarthestFirst, osRenderBlendedLast, osRenderNearestFirst:
        begin
          distList.Flush;
          objList.Count := 0;
          distList.GrowthDelta := lastChildIndex + 1; // no reallocations
          objList.GrowthDelta := distList.GrowthDelta;
          // try
          case rci.ObjectsSorting of
            osRenderBlendedLast:
              // render opaque stuff
              for i := firstChildIndex to lastChildIndex do
              begin
                obj := TVXBaseSceneObject(FChildren.List^[i]);
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
                obj := TVXBaseSceneObject(FChildren.List^[i]);
                if obj.Visible then
                begin
                  objList.Add(obj);
                  distList.Add(1 + obj.BarycenterSqrDistanceTo(rci.cameraPosition));
                end;
              end;
            osRenderNearestFirst:
              for i := firstChildIndex to lastChildIndex do
              begin
                obj := TVXBaseSceneObject(FChildren.List^[i]);
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
              TVXBaseSceneObject(plist^[i]).Render(rci);
          end;
          // finally
          // end;
        end;
    else
      Assert(False);
    end;
    rci.ObjectsSorting := oldSorting;
  end;
  rci.VisibilityCulling := oldCulling;
end;

procedure TVXBaseSceneObject.NotifyChange(Sender: TObject);
begin
  if Assigned(FScene) and (not IsUpdating) then
    FScene.NotifyChange(Self);
end;

function TVXBaseSceneObject.GetMatrix: PMatrix;
begin
  RebuildMatrix;
  Result := @FLocalMatrix;
end;

procedure TVXBaseSceneObject.SetMatrix(const aValue: TMatrix);
begin
  FLocalMatrix := aValue;
  FDirection.DirectVector := VectorNormalize(FLocalMatrix.z);
  FUp.DirectVector := VectorNormalize(FLocalMatrix.y);
  Scale.SetVector(VectorLength(FLocalMatrix.x), VectorLength(FLocalMatrix.y), VectorLength(FLocalMatrix.z), 0);
  FPosition.DirectVector := FLocalMatrix.W;
  TransformationChanged;
end;

procedure TVXBaseSceneObject.SetPosition(APosition: TVXCoordinates);
begin
  FPosition.SetPoint(APosition.DirectX, APosition.DirectY, APosition.DirectZ);
end;

procedure TVXBaseSceneObject.SetDirection(AVector: TVXCoordinates);
begin
  if not VectorIsNull(AVector.DirectVector) then
    FDirection.SetVector(AVector.DirectX, AVector.DirectY, AVector.DirectZ);
end;

procedure TVXBaseSceneObject.SetUp(AVector: TVXCoordinates);
begin
  if not VectorIsNull(AVector.DirectVector) then
    FUp.SetVector(AVector.DirectX, AVector.DirectY, AVector.DirectZ);
end;

function TVXBaseSceneObject.GetVisible: Boolean;
begin
  Result := FVisible;
end;

function TVXBaseSceneObject.GetPickable: Boolean;
begin
  Result := FPickable;
end;

procedure TVXBaseSceneObject.SetVisible(aValue: Boolean);
begin
  if FVisible <> aValue then
  begin
    FVisible := aValue;
    NotifyChange(Self);
  end;
end;

procedure TVXBaseSceneObject.SetPickable(aValue: Boolean);
begin
  if FPickable <> aValue then
  begin
    FPickable := aValue;
    NotifyChange(Self);
  end;
end;

procedure TVXBaseSceneObject.SetObjectsSorting(const val: TVXObjectsSorting);
begin
  if FObjectsSorting <> val then
  begin
    FObjectsSorting := val;
    NotifyChange(Self);
  end;
end;

procedure TVXBaseSceneObject.SetVisibilityCulling(const val: TVXVisibilityCulling);
begin
  if FVisibilityCulling <> val then
  begin
    FVisibilityCulling := val;
    NotifyChange(Self);
  end;
end;

procedure TVXBaseSceneObject.SetBehaviours(const val: TVXBehaviours);
begin
  Behaviours.Assign(val);
end;

function TVXBaseSceneObject.GetBehaviours: TVXBehaviours;
begin
  if not Assigned(FGLBehaviours) then
    FGLBehaviours := TVXBehaviours.Create(Self);
  Result := FGLBehaviours;
end;

procedure TVXBaseSceneObject.SetEffects(const val: TVXObjectEffects);
begin
  Effects.Assign(val);
end;

function TVXBaseSceneObject.GetEffects: TVXObjectEffects;
begin
  if not Assigned(FGLObjectEffects) then
    FGLObjectEffects := TVXObjectEffects.Create(Self);
  Result := FGLObjectEffects;
end;

procedure TVXBaseSceneObject.SetScene(const Value: TVXScene);
var
  i: Integer;
begin
  if Value <> FScene then
  begin
    // must be freed, the new scene may be using a non-compatible RC
    if FScene <> nil then
      DestroyHandles;
    FScene := Value;
    // propagate for childs
    if Assigned(FChildren) then
      for i := 0 to FChildren.Count - 1 do
        Children[i].SetScene(FScene);
  end;
end;

procedure TVXBaseSceneObject.Translate(tx, ty, tz: Single);
begin
  FPosition.Translate(AffineVectorMake(tx, ty, tz));
end;

function TVXBaseSceneObject.GetAbsoluteAffinePosition: TAffineVector;
var
  temp: TVector;
begin
  temp := GetAbsolutePosition;
  Result := AffineVectorMake(temp.x, temp.y, temp.z);
end;

function TVXBaseSceneObject.GetAbsoluteAffineDirection: TAffineVector;
var
  temp: TVector;
begin
  temp := GetAbsoluteDirection;
  Result := AffineVectorMake(temp.x, temp.y, temp.z);
end;

function TVXBaseSceneObject.GetAbsoluteAffineUp: TAffineVector;
var
  temp: TVector;
begin
  temp := GetAbsoluteUp;
  Result := AffineVectorMake(temp.x, temp.y, temp.z);
end;

procedure TVXBaseSceneObject.SetAbsoluteAffinePosition(const Value: TAffineVector);
begin
  SetAbsolutePosition(VectorMake(Value, 1));
end;

procedure TVXBaseSceneObject.SetAbsoluteAffineUp(const v: TAffineVector);
begin
  SetAbsoluteUp(VectorMake(v, 1));
end;

procedure TVXBaseSceneObject.SetAbsoluteAffineDirection(const v: TAffineVector);
begin
  SetAbsoluteDirection(VectorMake(v, 1));
end;

function TVXBaseSceneObject.AffineLeftVector: TAffineVector;
begin
  Result := AffineVectorMake(LeftVector);
end;

function TVXBaseSceneObject.AffineRight: TAffineVector;
begin
  Result := AffineVectorMake(Right);
end;

function TVXBaseSceneObject.DistanceTo(const pt: TAffineVector): Single;
begin
  Result := VectorDistance(AbsoluteAffinePosition, pt);
end;

function TVXBaseSceneObject.SqrDistanceTo(const pt: TAffineVector): Single;
begin
  Result := VectorDistance2(AbsoluteAffinePosition, pt);
end;

procedure TVXBaseSceneObject.DoOnAddedToParent;
begin
  if Assigned(FOnAddedToParent) then
    FOnAddedToParent(Self);
end;

function TVXBaseSceneObject.GetAbsoluteAffineScale: TAffineVector;
begin
  Result := AffineVectorMake(GetAbsoluteScale);
end;

procedure TVXBaseSceneObject.SetAbsoluteAffineScale(const Value: TAffineVector);
begin
  SetAbsoluteScale(VectorMake(Value, GetAbsoluteScale.W));
end;

// ------------------
// ------------------ TVXBaseBehaviour ------------------
// ------------------

constructor TVXBaseBehaviour.Create(AOwner: TXCollection);
begin
  inherited Create(AOwner);
  // nothing more, yet
end;

destructor TVXBaseBehaviour.Destroy;
begin
  // nothing more, yet
  inherited Destroy;
end;

procedure TVXBaseBehaviour.SetName(const val: string);
begin
  inherited SetName(val);
  if Assigned(vBehaviourNameChangeEvent) then
    vBehaviourNameChangeEvent(Self);
end;

procedure TVXBaseBehaviour.WriteToFiler(writer: TWriter);
begin
  inherited;

  with writer do
  begin
    WriteInteger(0); // Archive Version 0
    // nothing more, yet
  end;
end;

procedure TVXBaseBehaviour.ReadFromFiler(reader: TReader);
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

function TVXBaseBehaviour.OwnerBaseSceneObject: TVXBaseSceneObject;
begin
  Result := TVXBaseSceneObject(Owner.Owner);
end;

procedure TVXBaseBehaviour.DoProgress(const progressTime: TVXProgressTimes);
begin
  // does nothing
end;

// ------------------
// ------------------ TVXBehaviours ------------------
// ------------------

// Create
//

constructor TVXBehaviours.Create(AOwner: TPersistent);
begin
  Assert(AOwner is TVXBaseSceneObject);
  inherited Create(AOwner);
end;

function TVXBehaviours.GetNamePath: string;
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

class function TVXBehaviours.ItemsClass: TXCollectionItemClass;
begin
  Result := TVXBehaviour;
end;

function TVXBehaviours.GetBehaviour(Index: Integer): TVXBehaviour;
begin
  Result := TVXBehaviour(Items[index]);
end;

function TVXBehaviours.CanAdd(aClass: TXCollectionItemClass): Boolean;
begin
  Result := (not aClass.InheritsFrom(TVXObjectEffect)) and (inherited CanAdd(aClass));
end;

procedure TVXBehaviours.DoProgress(const progressTimes: TVXProgressTimes);
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    TVXBehaviour(Items[i]).DoProgress(progressTimes);
end;

// ------------------
// ------------------ TVXObjectEffect ------------------
// ------------------

procedure TVXObjectEffect.WriteToFiler(writer: TWriter);
begin
  inherited;
  with writer do
  begin
    WriteInteger(0); // Archive Version 0
    // nothing more, yet
  end;
end;

procedure TVXObjectEffect.ReadFromFiler(reader: TReader);
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

procedure TVXObjectEffect.Render(var rci: TVXRenderContextInfo);
begin
  // nothing here, this implem is just to avoid "abstract error"
end;

// ------------------
// ------------------ TVXObjectEffects ------------------
// ------------------

constructor TVXObjectEffects.Create(AOwner: TPersistent);
begin
  Assert(AOwner is TVXBaseSceneObject);
  inherited Create(AOwner);
end;

function TVXObjectEffects.GetNamePath: string;
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

class function TVXObjectEffects.ItemsClass: TXCollectionItemClass;
begin
  Result := TVXObjectEffect;
end;

function TVXObjectEffects.GetEffect(Index: Integer): TVXObjectEffect;
begin
  Result := TVXObjectEffect(Items[index]);
end;

function TVXObjectEffects.CanAdd(aClass: TXCollectionItemClass): Boolean;
begin
  Result := (aClass.InheritsFrom(TVXObjectEffect)) and (inherited CanAdd(aClass));
end;

procedure TVXObjectEffects.DoProgress(const progressTime: TVXProgressTimes);
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    TVXObjectEffect(Items[i]).DoProgress(progressTime);
end;

procedure TVXObjectEffects.RenderPreEffects(var rci: TVXRenderContextInfo);
var
  i: Integer;
  effect: TVXObjectEffect;
begin
  for i := 0 to Count - 1 do
  begin
    effect := TVXObjectEffect(Items[i]);
    if effect is TVXObjectPreEffect then
      effect.Render(rci);
  end;
end;

procedure TVXObjectEffects.RenderPostEffects(var rci: TVXRenderContextInfo);
var
  i: Integer;
  effect: TVXObjectEffect;
begin
  for i := 0 to Count - 1 do
  begin
    effect := TVXObjectEffect(Items[i]);
    if effect is TVXObjectPostEffect then
      effect.Render(rci)
    else if Assigned(rci.afterRenderEffects) and (effect is TVXObjectAfterEffect) then
      rci.afterRenderEffects.Add(effect);
  end;
end;

// ------------------
// ------------------ TVXCustomSceneObject ------------------
// ------------------

constructor TVXCustomSceneObject.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FMaterial := TVXMaterial.Create(Self);
end;

destructor TVXCustomSceneObject.Destroy;
begin
  inherited Destroy;
  FMaterial.Free;
end;

procedure TVXCustomSceneObject.Assign(Source: TPersistent);
begin
  if Source is TVXCustomSceneObject then
  begin
    FMaterial.Assign(TVXCustomSceneObject(Source).FMaterial);
    FHint := TVXCustomSceneObject(Source).FHint;
  end;
  inherited Assign(Source);
end;

function TVXCustomSceneObject.Blended: Boolean;
begin
  Result := Material.Blended;
end;

procedure TVXCustomSceneObject.Loaded;
begin
  inherited;
  FMaterial.Loaded;
end;

procedure TVXCustomSceneObject.SetVKMaterial(aValue: TVXMaterial);
begin
  FMaterial.Assign(aValue);
  NotifyChange(Self);
end;

procedure TVXCustomSceneObject.DestroyHandle;
begin
  inherited;
  FMaterial.DestroyHandles;
end;

procedure TVXCustomSceneObject.DoRender(var ARci: TVXRenderContextInfo; ARenderSelf, ARenderChildren: Boolean);
begin
  // start rendering self
  if ARenderSelf then
    if ARci.ignoreMaterials then
      if (osDirectDraw in ObjectStyle) or ARci.amalgamating then
        BuildList(ARci)
      else
        ARci.VXStates.CallList(GetHandle(ARci))
    else
    begin
      FMaterial.Apply(ARci);
      repeat
        if (osDirectDraw in ObjectStyle) or ARci.amalgamating then
          BuildList(ARci)
        else
          ARci.VXStates.CallList(GetHandle(ARci));
      until not FMaterial.UnApply(ARci);
    end;
  // start rendering children (if any)
  if ARenderChildren then
    Self.RenderChildren(0, Count - 1, ARci);
end;

// ------------------
// ------------------ TVXSceneRootObject ------------------
// ------------------

constructor TVXSceneRootObject.Create(AOwner: TComponent);
begin
  Assert(AOwner is TVXScene);
  inherited Create(AOwner);
  ObjectStyle := ObjectStyle + [osDirectDraw];
  FScene := TVXScene(AOwner);
end;

// ------------------
// ------------------ TVXCamera ------------------
// ------------------

constructor TVXCamera.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FFocalLength := 50;
  FDepthOfView := 100;
  FNearPlaneBias := 1;
  FDirection.Initialize(VectorMake(0, 0, -1, 0));
  FCameraStyle := csPerspective;
  FSceneScale := 1;
  FDesign := False;
  FFOVY := -1;
  FKeepFOVMode := ckmHorizontalFOV;
end;

destructor TVXCamera.Destroy;
begin
  TargetObject := nil;
  inherited;
end;

procedure TVXCamera.Assign(Source: TPersistent);
var
  cam: TVXCamera;
  dir: TVector;
begin
  if Assigned(Source) then
  begin
    inherited Assign(Source);

    if Source is TVXCamera then
    begin
      cam := TVXCamera(Source);
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

function TVXCamera.AbsoluteVectorToTarget: TVector;
begin
  if TargetObject <> nil then
  begin
    VectorSubtract(TargetObject.AbsolutePosition, AbsolutePosition, Result);
    NormalizeVector(Result);
  end
  else
    Result := AbsoluteDirection;
end;

function TVXCamera.AbsoluteRightVectorToTarget: TVector;
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

function TVXCamera.AbsoluteUpVectorToTarget: TVector;
begin
  if TargetObject <> nil then
    Result := VectorCrossProduct(AbsoluteRightVectorToTarget, AbsoluteVectorToTarget)
  else
    Result := AbsoluteUp;
end;

// Apply
//

procedure TVXCamera.Apply;
var
  v, d, v2: TVector;
  absPos: TVector;
  LM, mat: TMatrix;
begin
  if Assigned(FDeferredApply) then
    FDeferredApply(Self)
  else
  begin
    if Assigned(FTargetObject) then
    begin
      v := TargetObject.AbsolutePosition;
      absPos := AbsolutePosition;
      VectorSubtract(v, absPos, d);
      NormalizeVector(d);
      FLastDirection := d;
      LM := CreateLookAtMatrix(absPos, v, Up.AsVector);
    end
    else
    begin
      if Assigned(Parent) then
        mat := Parent.AbsoluteMatrix
      else
        mat := IdentityHmgMatrix;
      absPos := AbsolutePosition;
      v := VectorTransform(Direction.AsVector, mat);
      FLastDirection := v;
      d := VectorTransform(Up.AsVector, mat);
      v2 := VectorAdd(absPos, v);
      LM := CreateLookAtMatrix(absPos, v2, d);
    end;
    with CurrentVXContext.PipeLineTransformation do
      SetViewMatrix(MatrixMultiply(LM, ViewMatrix^));
    ClearStructureChanged;
  end;
end;

procedure TVXCamera.ApplyPerspective(const AViewport: TRectangle; AWidth, AHeight: Integer; ADPI: Integer);
var
  vLeft, vRight, vBottom, vTop, vFar: Single;
  MaxDim, ratio, f: Double;
  xmax, ymax: Double;
  mat: TMatrix;
const
  cEpsilon: Single = 1E-4;

  function IsPerspective(CamStyle: TVXCameraStyle): Boolean;
  begin
    Result := CamStyle in [csPerspective, csInfinitePerspective, csPerspectiveKeepFOV];
  end;

begin
  if (AWidth <= 0) or (AHeight <= 0) then
    Exit;

  if CameraStyle = csOrtho2D then
  begin
    vLeft := 0;
    vRight := AWidth;
    vBottom := 0;
    vTop := AHeight;
    FNearPlane := -1;
    vFar := 1;
    mat := CreateOrthoMatrix(vLeft, vRight, vBottom, vTop, FNearPlane, vFar);
    with CurrentVXContext.PipeLineTransformation do
      SetProjectionMatrix(MatrixMultiply(mat, ProjectionMatrix^));
    FViewPortRadius := VectorLength(AWidth, AHeight) / 2;
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
    vTop := ratio * AHeight / (2 * MaxDim);

    // bottom extent:
    ratio := (AHeight - 2 * AViewport.Top) * f;
    vBottom := -ratio * AHeight / (2 * MaxDim);

    FNearPlane := FFocalLength * 2 * ADPI / (25.4 * MaxDim) * FNearPlaneBias;
    vFar := FNearPlane + FDepthOfView;

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
          mat := IdentityHmgMatrix;
          mat.x.x := 2 * FNearPlane / (vRight - vLeft);
          mat.y.y := 2 * FNearPlane / (vTop - vBottom);
          mat.z.x := (vRight + vLeft) / (vRight - vLeft);
          mat.z.y := (vTop + vBottom) / (vTop - vBottom);
          mat.z.z := cEpsilon - 1;
          mat.z.W := -1;
          mat.W.z := FNearPlane * (cEpsilon - 2);
          mat.W.W := 0;
        end;
      csOrthogonal:
        begin
          mat := CreateOrthoMatrix(vLeft, vRight, vBottom, vTop, FNearPlane, vFar);
        end;
    else
      Assert(False);
    end;

    with CurrentVXContext.PipeLineTransformation do
      SetProjectionMatrix(MatrixMultiply(mat, ProjectionMatrix^));
    FViewPortRadius := VectorLength(vRight, vTop) / FNearPlane
  end;
end;

// ------------------------------------------------------------------------------

procedure TVXCamera.AutoLeveling(Factor: Single);
var
  rightVector, rotAxis: TVector;
  angle: Single;
begin
  angle := RadianToDeg(ArcCosine(VectorDotProduct(FUp.AsVector, YVector)));
  rotAxis := VectorCrossProduct(YHmgVector, FUp.AsVector);
  if (angle > 1) and (VectorLength(rotAxis) > 0) then
  begin
    rightVector := VectorCrossProduct(FDirection.AsVector, FUp.AsVector);
    FUp.Rotate(AffineVectorMake(rotAxis), angle / (10 * Factor));
    FUp.Normalize;
    // adjust local coordinates
    FDirection.DirectVector := VectorCrossProduct(FUp.AsVector, rightVector);
    FRotation.z := -RadToDeg(ArcTan2(rightVector.y, VectorLength(rightVector.x, rightVector.z)));
  end;
end;

// ------------------------------------------------------------------------------
procedure TVXCamera.Notification(AComponent: TComponent; Operation: TOperation);
begin
  if (Operation = opRemove) and (AComponent = FTargetObject) then
    TargetObject := nil;
  inherited;
end;

procedure TVXCamera.SetTargetObject(const val: TVXBaseSceneObject);
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

procedure TVXCamera.Reset(aSceneBuffer: TVXSceneBuffer);
var
  Extent: Single;
begin
  FRotation.z := 0;
  FFocalLength := 50;
  with aSceneBuffer do
  begin
    ApplyPerspective(FViewPort, FViewPort.width, FViewPort.height, FRenderDPI);
    FUp.DirectVector := YHmgVector;
    if FViewPort.height < FViewPort.width then
      Extent := FViewPort.height * 0.25
    else
      Extent := FViewPort.width * 0.25;
  end;
  FPosition.SetPoint(0, 0, FNearPlane * Extent);
  FDirection.SetVector(0, 0, -1, 0);
  TransformationChanged;
end;

procedure TVXCamera.ZoomAll(aSceneBuffer: TVXSceneBuffer);
var
  Extent: Single;
begin
  with aSceneBuffer do
  begin
    if FViewPort.height < FViewPort.width then
      Extent := FViewPort.height * 0.25
    else
      Extent := FViewPort.width * 0.25;
    FPosition.DirectVector := NullHmgPoint;
    Move(-FNearPlane * Extent);
    // let the camera look at the scene center
    FDirection.SetVector(-FPosition.x, -FPosition.y, -FPosition.z, 0);
  end;
end;

procedure TVXCamera.RotateObject(obj: TVXBaseSceneObject; pitchDelta, turnDelta: Single; rollDelta: Single = 0);
var
  resMat: TMatrix;
  vDir, vUp, vRight: TVector;
  v: TAffineVector;
  position1: TVector;
  Scale1: TVector;
begin
  // First we need to compute the actual camera's vectors, which may not be
  // directly available if we're in "targeting" mode
  vUp := AbsoluteUp;
  if TargetObject <> nil then
  begin
    vDir := AbsoluteVectorToTarget;
    vRight := VectorCrossProduct(vDir, vUp);
    vUp := VectorCrossProduct(vRight, vDir);
  end
  else
  begin
    vDir := AbsoluteDirection;
    vRight := VectorCrossProduct(vDir, vUp);
  end;

  // save scale & position info
  Scale1 := obj.Scale.AsVector;
  position1 := obj.Position.AsVector;
  resMat := obj.Matrix^;
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
  obj.SetMatrix(resMat);
  // restore scaling & rotation info
  obj.Scale.AsVector := Scale1;
  obj.Position.AsVector := position1;
end;

procedure TVXCamera.RotateTarget(pitchDelta, turnDelta: Single; rollDelta: Single = 0);
begin
  if Assigned(FTargetObject) then
    RotateObject(FTargetObject, pitchDelta, turnDelta, rollDelta)
end;

procedure TVXCamera.MoveAroundTarget(pitchDelta, turnDelta: Single);
begin
  MoveObjectAround(FTargetObject, pitchDelta, turnDelta);
end;

procedure TVXCamera.MoveAllAroundTarget(pitchDelta, turnDelta: Single);
begin
  MoveObjectAllAround(FTargetObject, pitchDelta, turnDelta);
end;

procedure TVXCamera.MoveInEyeSpace(forwardDistance, rightDistance, upDistance: Single);
var
  trVector: TVector;
begin
  trVector := AbsoluteEyeSpaceVector(forwardDistance, rightDistance, upDistance);
  if Assigned(Parent) then
    Position.Translate(Parent.AbsoluteToLocal(trVector))
  else
    Position.Translate(trVector);
end;

procedure TVXCamera.MoveTargetInEyeSpace(forwardDistance, rightDistance, upDistance: Single);
var
  trVector: TVector;
begin
  if TargetObject <> nil then
  begin
    trVector := AbsoluteEyeSpaceVector(forwardDistance, rightDistance, upDistance);
    TargetObject.Position.Translate(TargetObject.Parent.AbsoluteToLocal(trVector));
  end;
end;

function TVXCamera.AbsoluteEyeSpaceVector(forwardDistance, rightDistance, upDistance: Single): TVector;
begin
  Result := NullHmgVector;
  if forwardDistance <> 0 then
    CombineVector(Result, AbsoluteVectorToTarget, forwardDistance);
  if rightDistance <> 0 then
    CombineVector(Result, AbsoluteRightVectorToTarget, rightDistance);
  if upDistance <> 0 then
    CombineVector(Result, AbsoluteUpVectorToTarget, upDistance);
end;

procedure TVXCamera.AdjustDistanceToTarget(distanceRatio: Single);
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
      vect := Parent.AbsoluteToLocal(vect);
    Position.AsVector := vect;
  end;
end;

function TVXCamera.DistanceToTarget: Single;
var
  vect: TVector;
begin
  if Assigned(FTargetObject) then
  begin
    vect := VectorSubtract(AbsolutePosition, TargetObject.AbsolutePosition);
    Result := VectorLength(vect);
  end
  else
    Result := 1;
end;

function TVXCamera.ScreenDeltaToVector(deltaX, deltaY: Integer; ratio: Single; const planeNormal: TVector): TVector;
var
  screenY, screenX: TVector;
  screenYoutOfPlaneComponent: Single;
begin
  // calculate projection of direction vector on the plane
  if Assigned(FTargetObject) then
    screenY := VectorSubtract(TargetObject.AbsolutePosition, AbsolutePosition)
  else
    screenY := Direction.AsVector;
  screenYoutOfPlaneComponent := VectorDotProduct(screenY, planeNormal);
  screenY := VectorCombine(screenY, planeNormal, 1, -screenYoutOfPlaneComponent);
  NormalizeVector(screenY);
  // calc the screenX vector
  screenX := VectorCrossProduct(screenY, planeNormal);
  // and here, we're done
  Result := VectorCombine(screenX, screenY, deltaX * ratio, deltaY * ratio);
end;

function TVXCamera.ScreenDeltaToVectorXY(deltaX, deltaY: Integer; ratio: Single): TVector;
var
  screenY: TVector;
  dxr, dyr, d: Single;
begin
  // calculate projection of direction vector on the plane
  if Assigned(FTargetObject) then
    screenY := VectorSubtract(TargetObject.AbsolutePosition, AbsolutePosition)
  else
    screenY := Direction.AsVector;
  d := VectorLength(screenY.x, screenY.y);
  if d <= 1E-10 then
    d := ratio
  else
    d := ratio / d;
  // and here, we're done
  dxr := deltaX * d;
  dyr := deltaY * d;
  Result.x := screenY.y * dxr + screenY.x * dyr;
  Result.y := screenY.y * dyr - screenY.x * dxr;
  Result.z := 0;
  Result.W := 0;
end;

function TVXCamera.ScreenDeltaToVectorXZ(deltaX, deltaY: Integer; ratio: Single): TVector;
var
  screenY: TVector;
  d, dxr, dzr: Single;
begin
  // calculate the projection of direction vector on the plane
  if Assigned(FTargetObject) then
    screenY := VectorSubtract(TargetObject.AbsolutePosition, AbsolutePosition)
  else
    screenY := Direction.AsVector;
  d := VectorLength(screenY.x, screenY.z);
  if d <= 1E-10 then
    d := ratio
  else
    d := ratio / d;
  dxr := deltaX * d;
  dzr := deltaY * d;
  Result.x := -screenY.z * dxr + screenY.x * dzr;
  Result.y := 0;
  Result.z := screenY.z * dzr + screenY.x * dxr;
  Result.W := 0;
end;

function TVXCamera.ScreenDeltaToVectorYZ(deltaX, deltaY: Integer; ratio: Single): TVector;
var
  screenY: TVector;
  d, dyr, dzr: Single;
begin
  // calculate the projection of direction vector on the plane
  if Assigned(FTargetObject) then
    screenY := VectorSubtract(TargetObject.AbsolutePosition, AbsolutePosition)
  else
    screenY := Direction.AsVector;
  d := VectorLength(screenY.y, screenY.z);
  if d <= 1E-10 then
    d := ratio
  else
    d := ratio / d;
  dyr := deltaX * d;
  dzr := deltaY * d;
  Result.x := 0;
  Result.y := screenY.z * dyr + screenY.y * dzr;
  Result.z := screenY.z * dzr - screenY.y * dyr;
  Result.W := 0;
end;

function TVXCamera.PointInFront(const point: TVector): Boolean;
begin
  Result := PointIsInHalfSpace(point, AbsolutePosition, AbsoluteDirection);
end;

procedure TVXCamera.SetDepthOfView(aValue: Single);
begin
  if FDepthOfView <> aValue then
  begin
    FDepthOfView := aValue;
    FFOVY := -1;
    if not(csLoading in ComponentState) then
      TransformationChanged;
  end;
end;

procedure TVXCamera.SetFocalLength(aValue: Single);
begin
  if aValue <= 0 then
    aValue := 1;
  if FFocalLength <> aValue then
  begin
    FFocalLength := aValue;
    FFOVY := -1;
    if not(csLoading in ComponentState) then
      TransformationChanged;
  end;
end;

function TVXCamera.GetFieldOfView(const AViewportDimension: Single): Single;
begin
  if FFocalLength = 0 then
    Result := 0
  else
    Result := RadToDeg(2 * ArcTan2(AViewportDimension * 0.5, FFocalLength));
end;

procedure TVXCamera.SetFieldOfView(const AFieldOfView, AViewportDimension: Single);
begin
  FocalLength := AViewportDimension / (2 * Tan(DegToRadian(AFieldOfView / 2)));
end;

procedure TVXCamera.SetCameraStyle(const val: TVXCameraStyle);
begin
  if FCameraStyle <> val then
  begin
    FCameraStyle := val;
    FFOVY := -1;
    NotifyChange(Self);
  end;
end;

procedure TVXCamera.SetKeepFOVMode(const val: TVXCameraKeepFOVMode);
begin
  if FKeepFOVMode <> val then
  begin
    FKeepFOVMode := val;
    FFOVY := -1;
    if FCameraStyle = csPerspectiveKeepFOV then
      NotifyChange(Self);
  end;
end;

procedure TVXCamera.SetSceneScale(Value: Single);
begin
  if Value = 0 then
    Value := 1;
  if FSceneScale <> Value then
  begin
    FSceneScale := Value;
    FFOVY := -1;
    NotifyChange(Self);
  end;
end;

function TVXCamera.StoreSceneScale: Boolean;
begin
  Result := (FSceneScale <> 1);
end;

procedure TVXCamera.SetNearPlaneBias(Value: Single);
begin
  if Value <= 0 then
    Value := 1;
  if FNearPlaneBias <> Value then
  begin
    FNearPlaneBias := Value;
    FFOVY := -1;
    NotifyChange(Self);
  end;
end;

function TVXCamera.StoreNearPlaneBias: Boolean;
begin
  Result := (FNearPlaneBias <> 1);
end;

procedure TVXCamera.DoRender(var ARci: TVXRenderContextInfo; ARenderSelf, ARenderChildren: Boolean);
begin
  if ARenderChildren and (Count > 0) then
    Self.RenderChildren(0, Count - 1, ARci);
end;

function TVXCamera.RayCastIntersect(const rayStart, rayVector: TVector; intersectPoint: PVector = nil;
  intersectNormal: PVector = nil): Boolean;
begin
  Result := False;
end;

// ------------------
// ------------------ TVXImmaterialSceneObject ------------------
// ------------------

procedure TVXImmaterialSceneObject.DoRender(var ARci: TVXRenderContextInfo; ARenderSelf, ARenderChildren: Boolean);
begin
  // start rendering self
  if ARenderSelf then
  begin
    if (osDirectDraw in ObjectStyle) or ARci.amalgamating then
      BuildList(ARci)
    else
      ARci.VXStates.CallList(GetHandle(ARci));
  end;
  // start rendering children (if any)
  if ARenderChildren then
    Self.RenderChildren(0, Count - 1, ARci);
end;

// ------------------
// ------------------ TVXCameraInvariantObject ------------------
// ------------------

constructor TVXCameraInvariantObject.Create(AOwner: TComponent);
begin
  inherited;
  FCamInvarianceMode := cimNone;
end;

procedure TVXCameraInvariantObject.Assign(Source: TPersistent);
begin
  if Source is TVXCameraInvariantObject then
  begin
    FCamInvarianceMode := TVXCameraInvariantObject(Source).FCamInvarianceMode;
  end;
  inherited Assign(Source);
end;

procedure TVXCameraInvariantObject.DoRender(var ARci: TVXRenderContextInfo; ARenderSelf, ARenderChildren: Boolean);
begin
  if CamInvarianceMode <> cimNone then
    with ARci.PipeLineTransformation do
    begin
      Push;
      // try
      // prepare
      case CamInvarianceMode of
        cimPosition:
          begin
            SetViewMatrix(MatrixMultiply(CreateTranslationMatrix(ARci.cameraPosition),
              ARci.PipeLineTransformation.ViewMatrix^));
          end;
        cimOrientation:
          begin
            // makes the coordinates system more 'intuitive' (Z+ forward)
            SetViewMatrix(CreateScaleMatrix(Vector3fMake(1, -1, -1)))
          end;
      else
        Assert(False);
      end;
      // Apply local transform
      SetModelMatrix(LocalMatrix^);

      if ARenderSelf then
      begin
        if (osDirectDraw in ObjectStyle) or ARci.amalgamating then
          BuildList(ARci)
        else
          ARci.VXStates.CallList(GetHandle(ARci));
      end;
      if ARenderChildren then
        Self.RenderChildren(0, Count - 1, ARci);
      // finally
      Pop;
      // end;
    end
  else
    inherited;
end;

procedure TVXCameraInvariantObject.SetCamInvarianceMode(const val: TVXCameraInvarianceMode);
begin
  if FCamInvarianceMode <> val then
  begin
    FCamInvarianceMode := val;
    NotifyChange(Self);
  end;
end;

// ------------------
// ------------------ TxDirectOpenVX ------------------
// ------------------

constructor TVXDirectOpenVX.Create(AOwner: TComponent);
begin
  inherited;
  ObjectStyle := ObjectStyle + [osDirectDraw];
  FBlend := False;
end;

procedure TVXDirectOpenVX.Assign(Source: TPersistent);
begin
  if Source is TVXDirectOpenVX then
  begin
    UseBuildList := TVXDirectOpenVX(Source).UseBuildList;
    FOnRender := TVXDirectOpenVX(Source).FOnRender;
    FBlend := TVXDirectOpenVX(Source).Blend;
  end;
  inherited Assign(Source);
end;

procedure TVXDirectOpenVX.BuildList(var rci: TVXRenderContextInfo);
begin
  if Assigned(FOnRender) then
  begin
    xglMapTexCoordToMain; // single texturing by default
    OnRender(Self, rci);
  end;
end;

function TVXDirectOpenVX.AxisAlignedDimensionsUnscaled: TVector;
begin
  Result := NullHmgPoint;
end;

procedure TVXDirectOpenVX.SetUseBuildList(const val: Boolean);
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

function TVXDirectOpenVX.Blended: Boolean;
begin
  Result := FBlend;
end;

procedure TVXDirectOpenVX.SetBlend(const val: Boolean);
begin
  if val <> FBlend then
  begin
    FBlend := val;
    StructureChanged;
  end;
end;

// ------------------
// ------------------ TVXRenderPoint ------------------
// ------------------

constructor TVXRenderPoint.Create(AOwner: TComponent);
begin
  inherited;
  ObjectStyle := ObjectStyle + [osDirectDraw];
end;

destructor TVXRenderPoint.Destroy;
begin
  Clear;
  inherited;
end;

procedure TVXRenderPoint.BuildList(var rci: TVXRenderContextInfo);
var
  i: Integer;
begin
  for i := 0 to High(FCallBacks) do
    FCallBacks[i](Self, rci);
end;

procedure TVXRenderPoint.RegisterCallBack(renderEvent: TDirectRenderEvent; renderPointFreed: TNotifyEvent);
var
  n: Integer;
begin
  n := Length(FCallBacks);
  SetLength(FCallBacks, n + 1);
  SetLength(FFreeCallBacks, n + 1);
  FCallBacks[n] := renderEvent;
  FFreeCallBacks[n] := renderPointFreed;
end;

procedure TVXRenderPoint.UnRegisterCallBack(renderEvent: TDirectRenderEvent);
type
  TEventContainer = record
    event: TDirectRenderEvent;
  end;
var
  i, j, n: Integer;
  refContainer, listContainer: TEventContainer;
begin
  refContainer.event := renderEvent;
  n := Length(FCallBacks);
  for i := 0 to n - 1 do
  begin
    listContainer.event := FCallBacks[i];
    if CompareMem(@listContainer, @refContainer, SizeOf(TEventContainer)) then
    begin
      for j := i + 1 to n - 1 do
      begin
        FCallBacks[j - 1] := FCallBacks[j];
        FFreeCallBacks[j - 1] := FFreeCallBacks[j];
      end;
      SetLength(FCallBacks, n - 1);
      SetLength(FFreeCallBacks, n - 1);
      Break;
    end;
  end;
end;

procedure TVXRenderPoint.Clear;
begin
  while Length(FCallBacks) > 0 do
  begin
    FFreeCallBacks[High(FCallBacks)](Self);
    SetLength(FCallBacks, Length(FCallBacks) - 1);
  end;
end;

// ------------------
// ------------------ TVXProxyObject ------------------
// ------------------

constructor TVXProxyObject.Create(AOwner: TComponent);
begin
  inherited;
  FProxyOptions := cDefaultProxyOptions;
end;

destructor TVXProxyObject.Destroy;
begin
  SetMasterObject(nil);
  inherited;
end;

procedure TVXProxyObject.Assign(Source: TPersistent);
begin
  if Source is TVXProxyObject then
  begin
    SetMasterObject(TVXProxyObject(Source).MasterObject);
  end;
  inherited Assign(Source);
end;

procedure TVXProxyObject.DoRender(var ARci: TVXRenderContextInfo; ARenderSelf, ARenderChildren: Boolean);
var
  gotMaster, masterGotEffects, oldProxySubObject: Boolean;
begin
  if FRendering then
    Exit;
  FRendering := True;
  try
    gotMaster := Assigned(FMasterObject);
    masterGotEffects := gotMaster and (pooEffects in FProxyOptions) and (FMasterObject.Effects.Count > 0);
    if gotMaster then
    begin
      if pooObjects in FProxyOptions then
      begin
        oldProxySubObject := ARci.proxySubObject;
        ARci.proxySubObject := True;
        if pooTransformation in FProxyOptions then
          with ARci.PipeLineTransformation do
            SetModelMatrix(MatrixMultiply(FMasterObject.Matrix^, ModelMatrix^));
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

function TVXProxyObject.AxisAlignedDimensions: TVector;
begin
  If Assigned(FMasterObject) then
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

function TVXProxyObject.AxisAlignedDimensionsUnscaled: TVector;
begin
  if Assigned(FMasterObject) then
  begin
    Result := FMasterObject.AxisAlignedDimensionsUnscaled;
  end
  else
    Result := inherited AxisAlignedDimensionsUnscaled;
end;

function TVXProxyObject.BarycenterAbsolutePosition: TVector;
var
  lAdjustVector: TVector;
begin
  if Assigned(FMasterObject) then
  begin
    // Not entirely correct, but better than nothing...
    lAdjustVector := VectorSubtract(FMasterObject.BarycenterAbsolutePosition, FMasterObject.AbsolutePosition);
    Position.AsVector := VectorAdd(Position.AsVector, lAdjustVector);
    Result := AbsolutePosition;
    Position.AsVector := VectorSubtract(Position.AsVector, lAdjustVector);
  end
  else
    Result := inherited BarycenterAbsolutePosition;
end;

procedure TVXProxyObject.Notification(AComponent: TComponent; Operation: TOperation);
begin
  if (Operation = opRemove) and (AComponent = FMasterObject) then
    MasterObject := nil;
  inherited;
end;

procedure TVXProxyObject.SetMasterObject(const val: TVXBaseSceneObject);
begin
  if FMasterObject <> val then
  begin
    if Assigned(FMasterObject) then
      FMasterObject.RemoveFreeNotification(Self);
    FMasterObject := val;
    if Assigned(FMasterObject) then
      FMasterObject.FreeNotification(Self);
    StructureChanged;
  end;
end;

procedure TVXProxyObject.SetProxyOptions(const val: TVXProxyObjectOptions);
begin
  if FProxyOptions <> val then
  begin
    FProxyOptions := val;
    StructureChanged;
  end;
end;

function TVXProxyObject.RayCastIntersect(const rayStart, rayVector: TVector; intersectPoint: PVector = nil;
  intersectNormal: PVector = nil): Boolean;
var
  localRayStart, localRayVector: TVector;
begin
  if Assigned(MasterObject) then
  begin
    SetVector(localRayStart, AbsoluteToLocal(rayStart));
    SetVector(localRayStart, MasterObject.LocalToAbsolute(localRayStart));
    SetVector(localRayVector, AbsoluteToLocal(rayVector));
    SetVector(localRayVector, MasterObject.LocalToAbsolute(localRayVector));
    NormalizeVector(localRayVector);

    Result := MasterObject.RayCastIntersect(localRayStart, localRayVector, intersectPoint, intersectNormal);
    if Result then
    begin
      if Assigned(intersectPoint) then
      begin
        SetVector(intersectPoint^, MasterObject.AbsoluteToLocal(intersectPoint^));
        SetVector(intersectPoint^, LocalToAbsolute(intersectPoint^));
      end;
      if Assigned(intersectNormal) then
      begin
        SetVector(intersectNormal^, MasterObject.AbsoluteToLocal(intersectNormal^));
        SetVector(intersectNormal^, LocalToAbsolute(intersectNormal^));
      end;
    end;
  end
  else
    Result := False;
end;

function TVXProxyObject.GenerateSilhouette(const silhouetteParameters: TVXSilhouetteParameters): TVXSilhouette;
begin
  if Assigned(MasterObject) then
    Result := MasterObject.GenerateSilhouette(silhouetteParameters)
  else
    Result := nil;
end;

// ------------------
// ------------------ TxLightSource ------------------
// ------------------

constructor TVXLightSource.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FListHandle := nil;
  FShining := True;
  FSpotDirection := TVXCoordinates.CreateInitialized(Self, VectorMake(0, 0, -1, 0), csVector);
  FConstAttenuation := 1;
  FLinearAttenuation := 0;
  FQuadraticAttenuation := 0;
  FSpotCutOff := 180;
  FSpotExponent := 0;
  FLightStyle := lsSpot;
  FAmbient := TVXColor.Create(Self);
  FDiffuse := TVXColor.Create(Self);
  FDiffuse.Initialize(clrWhite);
  FSpecular := TVXColor.Create(Self);
end;

destructor TVXLightSource.Destroy;
begin
  FSpotDirection.Free;
  FAmbient.Free;
  FDiffuse.Free;
  FSpecular.Free;
  inherited Destroy;
end;

procedure TVXLightSource.DoRender(var ARci: TVXRenderContextInfo; ARenderSelf, ARenderChildren: Boolean);
begin
  if ARenderChildren and Assigned(FChildren) then
    Self.RenderChildren(0, Count - 1, ARci);
end;

function TVXLightSource.RayCastIntersect(const rayStart, rayVector: TVector; intersectPoint: PVector = nil;
  intersectNormal: PVector = nil): Boolean;
begin
  Result := False;
end;

procedure TVXLightSource.CoordinateChanged(Sender: TVXCustomCoordinates);
begin
  inherited;
  if Sender = FSpotDirection then
    TransformationChanged;
end;

function TVXLightSource.GenerateSilhouette(const silhouetteParameters: TVXSilhouetteParameters): TVXSilhouette;
begin
  Result := nil;
end;

procedure TVXLightSource.SetShining(aValue: Boolean);
begin
  if aValue <> FShining then
  begin
    FShining := aValue;
    NotifyChange(Self);
  end;
end;

procedure TVXLightSource.SetSpotDirection(AVector: TVXCoordinates);
begin
  FSpotDirection.DirectVector := AVector.AsVector;
  FSpotDirection.W := 0;
  NotifyChange(Self);
end;

procedure TVXLightSource.SetSpotExponent(aValue: Single);
begin
  if FSpotExponent <> aValue then
  begin
    FSpotExponent := aValue;
    NotifyChange(Self);
  end;
end;

procedure TVXLightSource.SetSpotCutOff(const val: Single);
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

procedure TVXLightSource.SetLightStyle(const val: TLightStyle);
begin
  if FLightStyle <> val then
  begin
    FLightStyle := val;
    NotifyChange(Self);
  end;
end;

procedure TVXLightSource.SetAmbient(aValue: TVXColor);
begin
  FAmbient.Color := aValue.Color;
  NotifyChange(Self);
end;

procedure TVXLightSource.SetDiffuse(aValue: TVXColor);
begin
  FDiffuse.Color := aValue.Color;
  NotifyChange(Self);
end;

procedure TVXLightSource.SetSpecular(aValue: TVXColor);
begin
  FSpecular.Color := aValue.Color;
  NotifyChange(Self);
end;

procedure TVXLightSource.SetConstAttenuation(aValue: Single);
begin
  if FConstAttenuation <> aValue then
  begin
    FConstAttenuation := aValue;
    NotifyChange(Self);
  end;
end;

procedure TVXLightSource.SetLinearAttenuation(aValue: Single);
begin
  if FLinearAttenuation <> aValue then
  begin
    FLinearAttenuation := aValue;
    NotifyChange(Self);
  end;
end;

procedure TVXLightSource.SetQuadraticAttenuation(aValue: Single);
begin
  if FQuadraticAttenuation <> aValue then
  begin
    FQuadraticAttenuation := aValue;
    NotifyChange(Self);
  end;
end;

function TVXLightSource.Attenuated: Boolean;
begin
  Result := (LightStyle <> lsParallel) and ((ConstAttenuation <> 1) or (LinearAttenuation <> 0) or (QuadraticAttenuation <> 0));
end;

// ------------------
// ------------------ TVXScene ------------------
// ------------------

constructor TVXScene.Create(AOwner: TComponent);
begin
  inherited;
  // root creation
  FCurrentBuffer := nil;
  FObjects := TVXSceneRootObject.Create(Self);
  FObjects.Name := 'ObjectRoot';
  FLights := TPersistentObjectList.Create;
  FObjectsSorting := osRenderBlendedLast;
  FVisibilityCulling := vcNone;
  // actual maximum number of lights is stored in TVXSceneViewer
  FLights.Count := 8;
  FInitializableObjects := TVXInitializableObjectList.Create;
end;

destructor TVXScene.Destroy;
begin
  InitializableObjects.Free;
  FObjects.DestroyHandles;
  FLights.Free;
  FObjects.Free;
  if Assigned(FBuffers) then
    FreeAndNil(FBuffers);
  inherited Destroy;
end;

procedure TVXScene.AddLight(aLight: TVXLightSource);
var
  i: Integer;
begin
  for i := 0 to FLights.Count - 1 do
    if FLights.List^[i] = nil then
    begin
      FLights.List^[i] := aLight;
      aLight.FLightID := i;
      Break;
    end;
end;

procedure TVXScene.RemoveLight(aLight: TVXLightSource);
var
  idx: Integer;
begin
  idx := FLights.IndexOf(aLight);
  if idx >= 0 then
    FLights[idx] := nil;
end;

procedure TVXScene.AddLights(anObj: TVXBaseSceneObject);
var
  i: Integer;
begin
  if anObj is TVXLightSource then
    AddLight(TVXLightSource(anObj));
  for i := 0 to anObj.Count - 1 do
    AddLights(anObj.Children[i]);
end;

procedure TVXScene.RemoveLights(anObj: TVXBaseSceneObject);
var
  i: Integer;
begin
  if anObj is TVXLightSource then
    RemoveLight(TVXLightSource(anObj));
  for i := 0 to anObj.Count - 1 do
    RemoveLights(anObj.Children[i]);
end;

procedure TVXScene.ShutdownAllLights;

  procedure DoShutdownLight(obj: TVXBaseSceneObject);
  var
    i: Integer;
  begin
    if obj is TVXLightSource then
      TVXLightSource(obj).Shining := False;
    for i := 0 to obj.Count - 1 do
      DoShutdownLight(obj[i]);
  end;

begin
  DoShutdownLight(FObjects);
end;

procedure TVXScene.AddBuffer(aBuffer: TVXSceneBuffer);
begin
  if not Assigned(FBuffers) then
    FBuffers := TPersistentObjectList.Create;
  if FBuffers.IndexOf(aBuffer) < 0 then
  begin
    FBuffers.Add(aBuffer);
    if FBaseContext = nil then
      FBaseContext := TVXSceneBuffer(FBuffers[0]).RenderingContext;
    if (FBuffers.Count > 1) and Assigned(FBaseContext) then
      aBuffer.RenderingContext.ShareLists(FBaseContext);
  end;
end;

procedure TVXScene.RemoveBuffer(aBuffer: TVXSceneBuffer);
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
        FBaseContext := TVXSceneBuffer(FBuffers[0]).RenderingContext;
      end;
    end;
  end;
end;

procedure TVXScene.GetChildren(AProc: TGetChildProc; Root: TComponent);
begin
  FObjects.GetChildren(AProc, Root);
end;

procedure TVXScene.SetChildOrder(AChild: TComponent; Order: Integer);
begin
  (AChild as TVXBaseSceneObject).Index := Order;
end;

function TVXScene.IsUpdating: Boolean;
begin
  Result := (FUpdateCount <> 0) or (csLoading in ComponentState) or (csDestroying in ComponentState);
end;

procedure TVXScene.BeginUpdate;
begin
  Inc(FUpdateCount);
end;

procedure TVXScene.EndUpdate;
begin
  Assert(FUpdateCount > 0);
  Dec(FUpdateCount);
  if FUpdateCount = 0 then
    NotifyChange(Self);
end;

procedure TVXScene.SetObjectsSorting(const val: TVXObjectsSorting);
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

procedure TVXScene.SetVisibilityCulling(const val: TVXVisibilityCulling);
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

procedure TVXScene.ReadState(reader: TReader);
var
  SaveRoot: TComponent;
begin
  SaveRoot := reader.Root;
  try
    if Owner <> nil then
      reader.Root := Owner;
    inherited;
  finally
    reader.Root := SaveRoot;
  end;
end;

procedure TVXScene.Progress(const deltaTime, newTime: Double);
var
  pt: TVXProgressTimes;
begin
  pt.deltaTime := deltaTime;
  pt.newTime := newTime;
  FCurrentDeltaTime := deltaTime;
  if Assigned(FOnBeforeProgress) then
    FOnBeforeProgress(Self, deltaTime, newTime);
  FObjects.DoProgress(pt);
  if Assigned(FOnProgress) then
    FOnProgress(Self, deltaTime, newTime);
end;

procedure TVXScene.SaveToFile(const fileName: string);
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

procedure TVXScene.LoadFromFile(const fileName: string);

  procedure CheckResFileStream(stream: TStream);
  var
    n: Integer;
    B: Byte;
  begin
    n := stream.Position;
    stream.Read(B, SizeOf(B));
    stream.Position := n;
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

procedure TVXScene.SaveToTextFile(const fileName: string);
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

procedure TVXScene.LoadFromTextFile(const fileName: string);
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

procedure TVXScene.LoadFromStream(aStream: TStream);
var
  fixups: TStringList;
  i: Integer;
  obj: TVXBaseSceneObject;
begin
  fixups := TStringList.Create;
  try
    if Assigned(FBuffers) then
    begin
      for i := 0 to FBuffers.Count - 1 do
        fixups.AddObject(TVXSceneBuffer(FBuffers[i]).Camera.Name, FBuffers[i]);
    end;
    ShutdownAllLights;
    // will remove Viewer from FBuffers
    Objects.DeleteChildren;
    aStream.ReadComponent(Self);
    for i := 0 to fixups.Count - 1 do
    begin
      obj := FindSceneObject(fixups[i]);
      if obj is TVXCamera then
        TVXSceneBuffer(fixups.Objects[i]).Camera := TVXCamera(obj)
      else { can assign default camera (if existing, of course) instead }
          ;
    end;
  finally
    fixups.Free;
  end;
end;

procedure TVXScene.SaveToStream(aStream: TStream);
begin
  aStream.WriteComponent(Self);
end;

function TVXScene.FindSceneObject(const aName: string): TVXBaseSceneObject;
begin
  Result := FObjects.FindChild(aName, False);
end;

function TVXScene.RayCastIntersect(const rayStart, rayVector: TVector; intersectPoint: PVector = nil;
  intersectNormal: PVector = nil): TVXBaseSceneObject;
var
  bestDist2: Single;
  bestHit: TVXBaseSceneObject;
  iPoint, iNormal: TVector;
  pINormal: PVector;

  function RecursiveDive(baseObject: TVXBaseSceneObject): TVXBaseSceneObject;
  var
    i: Integer;
    curObj: TVXBaseSceneObject;
    dist2: Single;
    fNear, fFar: Single;
  begin
    Result := nil;
    for i := 0 to baseObject.Count - 1 do
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
              bestHit := curObj;
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
  bestHit := nil;
  if Assigned(intersectNormal) then
    pINormal := @iNormal
  else
    pINormal := nil;
  RecursiveDive(Objects);
  Result := bestHit;
end;

procedure TVXScene.NotifyChange(Sender: TObject);
var
  i: Integer;
begin
  if (not IsUpdating) and Assigned(FBuffers) then
    for i := 0 to FBuffers.Count - 1 do
      TVXSceneBuffer(FBuffers[i]).NotifyChange(Self);
end;

procedure TVXScene.SetupLights(maxLights: Integer);
var
  i: Integer;
  lightSource: TVXLightSource;
  nbLights: Integer;
  lPos: TVector;
begin
  nbLights := FLights.Count;
  if nbLights > maxLights then
    nbLights := maxLights;
  // setup all light sources
  with CurrentVXContext.VXStates, CurrentVXContext.PipeLineTransformation do
  begin
    for i := 0 to nbLights - 1 do
    begin
      lightSource := TVXLightSource(FLights[i]);
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
                SetModelMatrix(AbsoluteMatrix);
                glLightfv(GL_LIGHT0 + FLightID, GL_POSITION, SpotDirection.AsAddress);
              end
              else
              begin
                SetModelMatrix(Parent.AbsoluteMatrix);
                glLightfv(GL_LIGHT0 + FLightID, GL_POSITION, Position.AsAddress);
              end;
              if LightStyle in [lsSpot, lsParallelSpot] then
              begin
                if FSpotCutOff <> 180 then
                  glLightfv(GL_LIGHT0 + FLightID, GL_SPOT_DIRECTION, FSpotDirection.AsAddress);
              end;
            end;

            lPos := lightSource.AbsolutePosition;
            if LightStyle in [lsParallel, lsParallelSpot] then
              lPos.W := 0.0
            else
              lPos.W := 1.0;
            LightPosition[FLightID] := lPos;
            LightSpotDirection[FLightID] := lightSource.SpotDirection.AsAffineVector;

            LightAmbient[FLightID] := FAmbient.Color;
            LightDiffuse[FLightID] := FDiffuse.Color;
            LightSpecular[FLightID] := FSpecular.Color;

            LightConstantAtten[FLightID] := FConstAttenuation;
            LightLinearAtten[FLightID] := FLinearAttenuation;
            LightQuadraticAtten[FLightID] := FQuadraticAttenuation;

            LightSpotExponent[FLightID] := FSpotExponent;
            LightSpotCutoff[FLightID] := FSpotCutOff;
          end;
        end
      else
        LightEnabling[i] := False;
    end;
    // turn off other lights
    for i := nbLights to maxLights - 1 do
      LightEnabling[i] := False;
    SetModelMatrix(IdentityHmgMatrix);
  end;
end;

// ------------------
// ------------------ TVXFogEnvironment ------------------
// ------------------

// Note: The fog implementation is not conformal with the rest of the scene management
// because it is viewer bound not scene bound.

constructor TVXFogEnvironment.Create(AOwner: TPersistent);
begin
  inherited;
  FSceneBuffer := (AOwner as TVXSceneBuffer);
  FFogColor := TVXColor.CreateInitialized(Self, clrBlack);
  FFogMode := fmLinear;
  FFogStart := 10;
  FFogEnd := 1000;
  FFogDistance := fdDefault;
end;

destructor TVXFogEnvironment.Destroy;
begin
  FFogColor.Free;
  inherited Destroy;
end;

procedure TVXFogEnvironment.SetFogColor(Value: TVXColor);
begin
  if Assigned(Value) then
  begin
    FFogColor.Assign(Value);
    NotifyChange(Self);
  end;
end;

procedure TVXFogEnvironment.SetFogStart(Value: Single);
begin
  if Value <> FFogStart then
  begin
    FFogStart := Value;
    NotifyChange(Self);
  end;
end;

procedure TVXFogEnvironment.SetFogEnd(Value: Single);
begin
  if Value <> FFogEnd then
  begin
    FFogEnd := Value;
    NotifyChange(Self);
  end;
end;

procedure TVXFogEnvironment.Assign(Source: TPersistent);
begin
  if Source is TVXFogEnvironment then
  begin
    FFogColor.Assign(TVXFogEnvironment(Source).FFogColor);
    FFogStart := TVXFogEnvironment(Source).FFogStart;
    FFogEnd := TVXFogEnvironment(Source).FFogEnd;
    FFogMode := TVXFogEnvironment(Source).FFogMode;
    FFogDistance := TVXFogEnvironment(Source).FFogDistance;
    NotifyChange(Self);
  end;
  inherited;
end;

function TVXFogEnvironment.IsAtDefaultValues: Boolean;
begin
  Result := VectorEquals(FogColor.Color, FogColor.DefaultColor) and (FogStart = 10) and (FogEnd = 1000) and (FogMode = fmLinear)
    and (FogDistance = fdDefault);
end;

procedure TVXFogEnvironment.SetFogMode(Value: TFogMode);
begin
  if Value <> FFogMode then
  begin
    FFogMode := Value;
    NotifyChange(Self);
  end;
end;

procedure TVXFogEnvironment.SetFogDistance(const val: TFogDistance);
begin
  if val <> FFogDistance then
  begin
    FFogDistance := val;
    NotifyChange(Self);
  end;
end;

var
  vImplemDependantFogDistanceDefault: Integer = -1;

procedure TVXFogEnvironment.ApplyFog;
var
  tempActivation: Boolean;
begin
  with FSceneBuffer do
  begin
    if not Assigned(FRenderingContext) then
      Exit;
    tempActivation := not FRenderingContext.Active;
    if tempActivation then
      FRenderingContext.Activate;
  end;

  case FFogMode of
    fmLinear:
      glFogi(GL_FOG_MODE, GL_LINEAR);
    fmExp:
      begin
        glFogi(GL_FOG_MODE, GL_EXP);
        glFogf(GL_FOG_DENSITY, FFogColor.alpha);
      end;
    fmExp2:
      begin
        glFogi(GL_FOG_MODE, GL_EXP2);
        glFogf(GL_FOG_DENSITY, FFogColor.alpha);
      end;
  end;
  glFogfv(GL_FOG_COLOR, FFogColor.AsAddress);
  glFogf(GL_FOG_START, FFogStart);
  glFogf(GL_FOG_END, FFogEnd);
  if GL_NV_fog_distance then
  begin
    case FogDistance of
      fdDefault:
        begin
          if vImplemDependantFogDistanceDefault = -1 then
            glGetIntegerv(Cardinal(GL_NV_fog_distance), // GL_FOG_DISTANCE_MODE_NV,
              @vImplemDependantFogDistanceDefault)
          else
            glFogi(Cardinal(GL_NV_fog_distance), vImplemDependantFogDistanceDefault);
        end;
      fdEyePlane:
        glFogi(Cardinal(GL_NV_fog_distance), GL_EYE_PLANE_ABSOLUTE_NV);
      fdEyeRadial:
        glFogi(GL_FOG_DISTANCE_MODE_NV, GL_EYE_RADIAL_NV);
    else
      Assert(False);
    end;
  end;

  if tempActivation then
    FSceneBuffer.RenderingContext.Deactivate;
end;

// ------------------
// ------------------ TVXSceneBuffer ------------------
// ------------------

constructor TVXSceneBuffer.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner);

  // initialize private state variables
  FFogEnvironment := TVXFogEnvironment.Create(Self);
  FBackgroundColor := TColors.SysBtnFace;
  FBackgroundAlpha := 1;
  FAmbientColor := TVXColor.CreateInitialized(Self, clrGray20);
  FDepthTest := True;
  FFaceCulling := True;
  FLighting := True;
  FAntiAliasing := aaDefault;
  FDepthPrecision := dpDefault;
  FColorDepth := cdDefault;
  FShadeModel := smDefault;
  FFogEnable := False;
  FLayer := clMainPlane;
  FAfterRenderEffects := TPersistentObjectList.Create;
  FContextOptions := [roDoubleBuffer, roRenderToWindow, roDebugContext];
  ResetPerformanceMonitor;
end;

destructor TVXSceneBuffer.Destroy;
begin
  Melt;
  DestroyRC;
  FAmbientColor.Free;
  FAfterRenderEffects.Free;
  FFogEnvironment.Free;
  inherited Destroy;
end;

procedure TVXSceneBuffer.PrepareGLContext;
begin
  if Assigned(FOnPrepareGLContext) then
    FOnPrepareGLContext(Self);
end;

procedure TVXSceneBuffer.SetupRCOptions(Context: TVXContext);
const
  cColorDepthToColorBits: array [cdDefault .. cdFloat128bits] of Integer = (24, 8, 16, 24, 64, 128); // float_type
  cDepthPrecisionToDepthBits: array [dpDefault .. dp32bits] of Integer = (24, 16, 24, 32);
var
  locOptions: TVXRCOptions;
  locStencilBits, locAlphaBits, locColorBits: Integer;
begin
  locOptions := [];

  if roDoubleBuffer in ContextOptions then
    locOptions := locOptions + [rcoDoubleBuffered];
  if roStereo in ContextOptions then
    locOptions := locOptions + [rcoStereo];
  if roDebugContext in ContextOptions then
    locOptions := locOptions + [rcoDebug];
  if roOpenVX_ES2_Context in ContextOptions then
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
  with Context do
  begin
    if roSoftwareMode in ContextOptions then
      Acceleration := chaSoftware
    else
      Acceleration := chaHardware;
    Options := locOptions;
    ColorBits := locColorBits;
    DepthBits := cDepthPrecisionToDepthBits[DepthPrecision];
    StencilBits := locStencilBits;
    AlphaBits := locAlphaBits;
    AccumBits := AccumBufferBits;
    AuxBuffers := 0;
    AntiAliasing := Self.AntiAliasing;
    Layer := Self.Layer;
    { VXStates.ForwardContext := roForwardContext in ContextOptions; }
    PrepareGLContext;
  end;
end;

procedure TVXSceneBuffer.CreateRC(AWindowHandle: THandle; memoryContext: Boolean; BufferCount: Integer);
begin
  DestroyRC;
  FRendering := True;

  try
    // will be freed in DestroyWindowHandle
    FRenderingContext := VXContextManager.CreateContext;
    if not Assigned(FRenderingContext) then
      raise Exception.Create('Failed to create RenderingContext.');
    SetupRCOptions(FRenderingContext);

    if Assigned(FCamera) and Assigned(FCamera.FScene) then
      FCamera.FScene.AddBuffer(Self);

    with FRenderingContext do
    begin
      try
        if memoryContext then
          CreateMemoryContext(AWindowHandle, FViewPort.width, FViewPort.height, BufferCount)
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
      if (GL_VERSION < 1.1) then
      begin
        ShowMessage(strWrongVersion);
        Abort;
      end;
      // define viewport, this is necessary because the first WM_SIZE message
      // is posted before the rendering context has been created
      FRenderingContext.VXStates.viewport := Vector4iMake(FViewPort.Left, FViewPort.Top, FViewPort.width, FViewPort.height);
      // set up initial context states
      SetupRenderingContext(FRenderingContext);
      FRenderingContext.VXStates.ColorClearValue := ConvertWinColor(FBackgroundColor);
    finally
      FRenderingContext.Deactivate;
    end;
  finally
    FRendering := False;
  end;
end;

procedure TVXSceneBuffer.DestroyRC;
begin
  if Assigned(FRenderingContext) then
  begin
    Melt;
    // for some obscure reason, Mesa3D doesn't like this call... any help welcome
    FreeAndNil(FSelector);
    FreeAndNil(FRenderingContext);
    if Assigned(FCamera) and Assigned(FCamera.FScene) then
      FCamera.FScene.RemoveBuffer(Self);
  end;
end;

function TVXSceneBuffer.RCInstantiated: Boolean;
begin
  Result := Assigned(FRenderingContext);
end;

procedure TVXSceneBuffer.Resize(newLeft, newTop, newWidth, newHeight: Integer);
begin
  if newWidth < 1 then
    newWidth := 1;
  if newHeight < 1 then
    newHeight := 1;
  FViewPort.Left := newLeft;
  FViewPort.Top := newTop;
  FViewPort.width := newWidth;
  FViewPort.height := newHeight;
  if Assigned(FRenderingContext) then
  begin
    FRenderingContext.Activate;
    try
      // Part of workaround for MS OpenGL "black borders" bug
      FRenderingContext.VXStates.viewport := Vector4iMake(FViewPort.Left, FViewPort.Top, FViewPort.width, FViewPort.height);
    finally
      FRenderingContext.Deactivate;
    end;
  end;
end;

function TVXSceneBuffer.Acceleration: TVXContextAcceleration;
begin
  if Assigned(FRenderingContext) then
    Result := FRenderingContext.Acceleration
  else
    Result := chaUnknown;
end;

procedure TVXSceneBuffer.SetupRenderingContext(Context: TVXContext);

  procedure SetState(bool: Boolean; csState: TVXState);
  begin
    case bool of
      True:
        Context.VXStates.PerformEnable(csState);
      False:
        Context.VXStates.PerformDisable(csState);
    end;
  end;

var
  LColorDepth: Cardinal;
begin
  if not Assigned(Context) then
    Exit;

  if not(roForwardContext in ContextOptions) then
  begin
    glLightModelfv(GL_LIGHT_MODEL_AMBIENT, FAmbientColor.AsAddress);
    if roTwoSideLighting in FContextOptions then
      glLightModeli(GL_LIGHT_MODEL_TWO_SIDE, GL_TRUE)
    else
      glLightModeli(GL_LIGHT_MODEL_TWO_SIDE, GL_FALSE);
    glHint(GL_PERSPECTIVE_CORRECTION_HINT, GL_NICEST);
    case ShadeModel of
      smDefault, smSmooth:
        glShadeModel(GL_SMOOTH);
      smFlat:
        glShadeModel(GL_FLAT);
    else
      Assert(False, strErrorEx + strUnknownType);
    end;
  end;

  with Context.VXStates do
  begin
    Enable(stNormalize);
    SetState(DepthTest, stDepthTest);
    SetState(FaceCulling, stCullFace);
    SetState(Lighting, stLighting);
    SetState(FogEnable, stFog);
    if GL_ARB_depth_clamp then
      Disable(stDepthClamp);
    if not(roForwardContext in ContextOptions) then
    begin
      glGetIntegerv(GL_BLUE_BITS, @LColorDepth); // could've used red or green too
      SetState((LColorDepth < 8), stDither);
    end;
    ResetAllTextureMatrix;
  end;
end;

function TVXSceneBuffer.GetLimit(Which: TLimitType): Integer;
var
  VP: array [0 .. 1] of Double;
begin
  case Which of
    limClipPlanes:
      glGetIntegerv(GL_MAX_CLIP_PLANES, @Result);
    limEvalOrder:
      glGetIntegerv(GL_MAX_EVAL_ORDER, @Result);
    limLights:
      glGetIntegerv(GL_MAX_LIGHTS, @Result);
    limListNesting:
      glGetIntegerv(GL_MAX_LIST_NESTING, @Result);
    limModelViewStack:
      glGetIntegerv(GL_MAX_MODELVIEW_STACK_DEPTH, @Result);
    limNameStack:
      glGetIntegerv(GL_MAX_NAME_STACK_DEPTH, @Result);
    limPixelMapTable:
      glGetIntegerv(GL_MAX_PIXEL_MAP_TABLE, @Result);
    limProjectionStack:
      glGetIntegerv(GL_MAX_PROJECTION_STACK_DEPTH, @Result);
    limTextureSize:
      glGetIntegerv(GL_MAX_TEXTURE_SIZE, @Result);
    limTextureStack:
      glGetIntegerv(GL_MAX_TEXTURE_STACK_DEPTH, @Result);
    limViewportDims:
      begin
        glGetDoublev(GL_MAX_VIEWPORT_DIMS, @VP);
        if VP[0] > VP[1] then
          Result := Round(VP[0])
        else
          Result := Round(VP[1]);
      end;
    limAccumAlphaBits:
      glGetIntegerv(GL_ACCUM_ALPHA_BITS, @Result);
    limAccumBlueBits:
      glGetIntegerv(GL_ACCUM_BLUE_BITS, @Result);
    limAccumGreenBits:
      glGetIntegerv(GL_ACCUM_GREEN_BITS, @Result);
    limAccumRedBits:
      glGetIntegerv(GL_ACCUM_RED_BITS, @Result);
    limAlphaBits:
      glGetIntegerv(GL_ALPHA_BITS, @Result);
    limAuxBuffers:
      glGetIntegerv(GL_AUX_BUFFERS, @Result);
    limDepthBits:
      glGetIntegerv(GL_DEPTH_BITS, @Result);
    limStencilBits:
      glGetIntegerv(GL_STENCIL_BITS, @Result);
    limBlueBits:
      glGetIntegerv(GL_BLUE_BITS, @Result);
    limGreenBits:
      glGetIntegerv(GL_GREEN_BITS, @Result);
    limRedBits:
      glGetIntegerv(GL_RED_BITS, @Result);
    limIndexBits:
      glGetIntegerv(GL_INDEX_BITS, @Result);
    limStereo:
      glGetIntegerv(GL_STEREO, @Result);
    limDoubleBuffer:
      glGetIntegerv(GL_DOUBLEBUFFER, @Result);
    limSubpixelBits:
      glGetIntegerv(GL_SUBPIXEL_BITS, @Result);
    limNbTextureUnits:
      if GL_ARB_multitexture then
        glGetIntegerv(GL_MAX_TEXTURE_UNITS_ARB, @Result)
      else
        Result := 1;
  else
    Result := 0;
  end;
end;

procedure TVXSceneBuffer.RenderToFile(const AFile: string; DPI: Integer);
var
  ABitmap: TBitmap;
  saveAllowed: Boolean;
  fileName: string;
begin
  Assert((not FRendering), strAlreadyRendering);
  ABitmap := TBitmap.Create;
  try
    ABitmap.width := FViewPort.width;
    ABitmap.height := FViewPort.height;
    { TODO -oPW : E2129 Cannot assign to a read-only property }
    // aBitmap.PixelFormat := glpf24Bit;
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

procedure TVXSceneBuffer.RenderToFile(const AFile: string; bmpWidth, bmpHeight: Integer);
var
  ABitmap: TBitmap;
  saveAllowed: Boolean;
  fileName: string;
begin
  Assert((not FRendering), strAlreadyRendering);
  ABitmap := TBitmap.Create;
  try
    ABitmap.width := bmpWidth;
    ABitmap.height := bmpHeight;
    { TODO -oPW : E2129 Cannot assign to a read-only property }
    (* GLS-> aBitmap.PixelFormat := glpf24Bit; *)
    RenderToBitmap(ABitmap,
      // GLS-> (GetDeviceLogicalPixelsX(Cardinal(ABitmap.Canvas.Handle)) * bmpWidth) div
      (GetDeviceLogicalPixelsX(ABitmap.Handle) * bmpWidth) div FViewPort.width);
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

function TVXSceneBuffer.CreateSnapShot: TVXBitmap32;
begin
  Result := TVXBitmap32.Create;
  Result.width := FViewPort.width;
  Result.height := FViewPort.height;
  if Assigned(Camera) and Assigned(Camera.Scene) then
  begin
    FRenderingContext.Activate;
    try
      Result.ReadPixels(rect(0, 0, FViewPort.width, FViewPort.height));
    finally
      FRenderingContext.Deactivate;
    end;
  end;
end;

function TVXSceneBuffer.CreateSnapShotBitmap: TBitmap;
var
  bmp32: TVXBitmap32;
begin
  bmp32 := CreateSnapShot;
  try
    Result := bmp32.Create32BitsBitmap;
  finally
    bmp32.Free;
  end;
end;

procedure TVXSceneBuffer.CopyToTexture(aTexture: TVXTexture);
begin
  CopyToTexture(aTexture, 0, 0, width, height, 0, 0);
end;

procedure TVXSceneBuffer.CopyToTexture(aTexture: TVXTexture; xSrc, ySrc, AWidth, AHeight: Integer; xDest, yDest: Integer;
  glCubeFace: GLEnum = 0);
var
  bindTarget: TVXTextureTarget;
begin
  if RenderingContext <> nil then
  begin
    RenderingContext.Activate;
    try
      if not(aTexture.Image is TVXBlankImage) then
        aTexture.ImageClassName := TVXBlankImage.ClassName;
      if aTexture.Image.width <> AWidth then
        TVXBlankImage(aTexture.Image).width := AWidth;
      if aTexture.Image.height <> AHeight then
        TVXBlankImage(aTexture.Image).height := AHeight;
      if aTexture.Image.Depth <> 0 then
        TVXBlankImage(aTexture.Image).Depth := 0;
      if TVXBlankImage(aTexture.Image).CubeMap <> (glCubeFace > 0) then
        TVXBlankImage(aTexture.Image).CubeMap := (glCubeFace > 0);

      bindTarget := aTexture.Image.NativeTextureTarget;
      RenderingContext.VXStates.TextureBinding[0, bindTarget] := aTexture.Handle;
      if glCubeFace > 0 then
        glCopyTexSubImage2D(glCubeFace, 0, xDest, yDest, xSrc, ySrc, AWidth, AHeight)
      else
        glCopyTexSubImage2D(DecodeTextureTarget(bindTarget), 0, xDest, yDest, xSrc, ySrc, AWidth, AHeight)
    finally
      RenderingContext.Deactivate;
    end;
  end;
end;

procedure TVXSceneBuffer.SaveAsFloatToFile(const aFilename: string);
var
  Data: Pointer;
  DataSize: Integer;
  stream: TMemoryStream;
const
  FloatSize = 4;
begin
  if Assigned(Camera) and Assigned(Camera.Scene) then
  begin
    DataSize := width * height * FloatSize * FloatSize;
    GetMem(Data, DataSize);
    FRenderingContext.Activate;
    try
      glReadPixels(0, 0, width, height, GL_RGBA, GL_FLOAT, Data);
      glGetError;

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

procedure TVXSceneBuffer.SetViewPort(x, y, W, H: Integer);
begin
  with FViewPort do
  begin
    Left := x;
    Top := y;
    width := W;
    height := H;
  end;
  NotifyChange(Self);
end;

function TVXSceneBuffer.width: Integer;
begin
  Result := FViewPort.width;
end;

function TVXSceneBuffer.height: Integer;
begin
  Result := FViewPort.height;
end;

procedure TVXSceneBuffer.Freeze;
begin
  if Freezed then
    Exit;
  if RenderingContext = nil then
    Exit;
  Render;
  FFreezed := True;
  RenderingContext.Activate;
  try
    FFreezeBuffer := AllocMem(FViewPort.width * FViewPort.height * 4);
    glReadPixels(0, 0, FViewPort.width, FViewPort.height, GL_RGBA, GL_UNSIGNED_BYTE, FFreezeBuffer);
    FFreezedViewPort := FViewPort;
  finally
    RenderingContext.Deactivate;
  end;
end;

procedure TVXSceneBuffer.Melt;
begin
  if not Freezed then
    Exit;
  FreeMem(FFreezeBuffer);
  FFreezeBuffer := nil;
  FFreezed := False;
end;

procedure TVXSceneBuffer.RenderToBitmap(ABitmap: TBitmap; DPI: Integer);
var
  nativeContext: TVXContext;
  aColorBits: Integer;
begin
  Assert((not FRendering), strAlreadyRendering);
  FRendering := True;
  nativeContext := RenderingContext;
  try
    aColorBits := PixelFormatToColorBits(ABitmap.PixelFormat);
    if aColorBits < 8 then
      aColorBits := 8;
    FRenderingContext := VXContextManager.CreateContext;
    SetupRCOptions(FRenderingContext);
    with FRenderingContext do
    begin
      Options := []; // no such things for bitmap rendering
      ColorBits := aColorBits; // honour Bitmap's pixel depth
      AntiAliasing := aaNone; // no AA for bitmap rendering
      CreateContext(ABitmap.Handle); // CreateContext(ABitmap.Canvas.Handle);
    end;
    try
      FRenderingContext.Activate;
      try
        SetupRenderingContext(FRenderingContext);
        FRenderingContext.VXStates.ColorClearValue := ConvertWinColor(FBackgroundColor);
        // set the desired viewport and limit output to this rectangle
        with FViewPort do
        begin
          Left := 0;
          Top := 0;
          width := ABitmap.width;
          height := ABitmap.height;
          FRenderingContext.VXStates.viewport := Vector4iMake(Left, Top, width, height);
        end;
        ClearBuffers;
        FRenderDPI := DPI;
        if FRenderDPI = 0 then
          FRenderDPI := GetDeviceLogicalPixelsX(ABitmap.Handle);
        // render
        DoBaseRender(FViewPort, FRenderDPI, dsPrinting, nil);
        if nativeContext <> nil then
          FViewPort := TRectangle(nativeContext.VXStates.viewport);
        glFinish;
      finally
        FRenderingContext.Deactivate;
      end;
    finally
      FRenderingContext.Free;
    end;
  finally
    FRenderingContext := nativeContext;
    FRendering := False;
  end;
  if Assigned(FAfterRender) then
    if Owner is TComponent then
      if not(csDesigning in TComponent(Owner).ComponentState) then
        FAfterRender(Self);
end;

procedure TVXSceneBuffer.ShowInfo(Modal: Boolean);
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

procedure TVXSceneBuffer.ResetPerformanceMonitor;
begin
  FFramesPerSecond := 0;
  FFrameCount := 0;
  FFirstPerfCounter := 0;
end;

procedure TVXSceneBuffer.PushViewMatrix(const newMatrix: TMatrix);
var
  n: Integer;
begin
  n := Length(FViewMatrixStack);
  SetLength(FViewMatrixStack, n + 1);
  FViewMatrixStack[n] := RenderingContext.PipeLineTransformation.ViewMatrix^;
  RenderingContext.PipeLineTransformation.SetViewMatrix(newMatrix);
end;

procedure TVXSceneBuffer.PopViewMatrix;
var
  n: Integer;
begin
  n := High(FViewMatrixStack);
  Assert(n >= 0, 'Unbalanced PopViewMatrix');
  RenderingContext.PipeLineTransformation.SetViewMatrix(FViewMatrixStack[n]);
  SetLength(FViewMatrixStack, n);
end;

procedure TVXSceneBuffer.PushProjectionMatrix(const newMatrix: TMatrix);
var
  n: Integer;
begin
  n := Length(FProjectionMatrixStack);
  SetLength(FProjectionMatrixStack, n + 1);
  FProjectionMatrixStack[n] := RenderingContext.PipeLineTransformation.ProjectionMatrix^;
  RenderingContext.PipeLineTransformation.SetProjectionMatrix(newMatrix);
end;

procedure TVXSceneBuffer.PopProjectionMatrix;
var
  n: Integer;
begin
  n := High(FProjectionMatrixStack);
  Assert(n >= 0, 'Unbalanced PopProjectionMatrix');
  RenderingContext.PipeLineTransformation.SetProjectionMatrix(FProjectionMatrixStack[n]);
  SetLength(FProjectionMatrixStack, n);
end;

function TVXSceneBuffer.ProjectionMatrix;
begin
  Result := RenderingContext.PipeLineTransformation.ProjectionMatrix^;
end;

function TVXSceneBuffer.ViewMatrix: TMatrix;
begin
  Result := RenderingContext.PipeLineTransformation.ViewMatrix^;
end;

function TVXSceneBuffer.ModelMatrix: TMatrix;
begin
  Result := RenderingContext.PipeLineTransformation.ModelMatrix^;
end;

function TVXSceneBuffer.OrthoScreenToWorld(screenX, screenY: Integer): TAffineVector;
var
  camPos, camUp, camRight: TAffineVector;
  f: Single;
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
    if FViewPort.width > FViewPort.height then
      f := f / FViewPort.width
    else
      f := f / FViewPort.height;
    SetVector(Result, VectorCombine3(camPos, camUp, camRight, 1, (screenY - (FViewPort.height div 2)) * f,
      (screenX - (FViewPort.width div 2)) * f));
  end
  else
    Result := NullVector;
end;

function TVXSceneBuffer.ScreenToWorld(const aPoint: TAffineVector): TAffineVector;
var
  rslt: TVector;
begin
  if Assigned(FCamera) and UnProject(VectorMake(aPoint), RenderingContext.PipeLineTransformation.ViewProjectionMatrix^,
    PHomogeneousIntVector(@FViewPort)^, rslt) then
    Result := Vector3fMake(rslt)
  else
    Result := aPoint;
end;

function TVXSceneBuffer.ScreenToWorld(const aPoint: TVector): TVector;
begin
  MakePoint(Result, ScreenToWorld(AffineVectorMake(aPoint)));
end;

function TVXSceneBuffer.ScreenToWorld(screenX, screenY: Integer): TAffineVector;
begin
  Result := ScreenToWorld(AffineVectorMake(screenX, FViewPort.height - screenY, 0));
end;

function TVXSceneBuffer.WorldToScreen(const aPoint: TAffineVector): TAffineVector;
var
  rslt: TVector;
begin
  RenderingContext.Activate;
  try
    PrepareRenderingMatrices(FViewPort, FRenderDPI);
    if Assigned(FCamera) and Project(VectorMake(aPoint), RenderingContext.PipeLineTransformation.ViewProjectionMatrix^,
      TVector4i(FViewPort), rslt) then
      Result := Vector3fMake(rslt)
    else
      Result := aPoint;
  finally
    RenderingContext.Deactivate;
  end;
end;

function TVXSceneBuffer.WorldToScreen(const aPoint: TVector): TVector;
begin
  SetVector(Result, WorldToScreen(AffineVectorMake(aPoint)));
end;

procedure TVXSceneBuffer.WorldToScreen(points: PVector; nbPoints: Integer);
var
  i: Integer;
begin
  if Assigned(FCamera) then
  begin
    for i := nbPoints - 1 downto 0 do
    begin
      Project(points^, RenderingContext.PipeLineTransformation.ViewProjectionMatrix^,
        PHomogeneousIntVector(@FViewPort)^, points^);
      Inc(points);
    end;
  end;
end;

function TVXSceneBuffer.ScreenToVector(const aPoint: TAffineVector): TAffineVector;
begin
  Result := VectorSubtract(ScreenToWorld(aPoint), PAffineVector(@FCameraAbsolutePosition)^);
end;

function TVXSceneBuffer.ScreenToVector(const aPoint: TVector): TVector;
begin
  SetVector(Result, VectorSubtract(ScreenToWorld(aPoint), FCameraAbsolutePosition));
  Result.W := 0;
end;

function TVXSceneBuffer.ScreenToVector(const x, y: Integer): TVector;
var
  av: TAffineVector;
begin
  av.x := x;
  av.y := y;
  av.z := 0;
  SetVector(Result, ScreenToVector(av));
end;

function TVXSceneBuffer.VectorToScreen(const VectToCam: TAffineVector): TAffineVector;
begin
  Result := WorldToScreen(VectorAdd(VectToCam, PAffineVector(@FCameraAbsolutePosition)^));
end;

function TVXSceneBuffer.ScreenVectorIntersectWithPlane(const aScreenPoint: TVector; const planePoint, planeNormal: TVector;
  var intersectPoint: TVector): Boolean;
var
  v: TVector;
begin
  if Assigned(FCamera) then
  begin
    SetVector(v, ScreenToVector(aScreenPoint));
    Result := RayCastPlaneIntersect(FCameraAbsolutePosition, v, planePoint, planeNormal, @intersectPoint);
    intersectPoint.W := 1;
  end
  else
    Result := False;
end;

function TVXSceneBuffer.ScreenVectorIntersectWithPlaneXY(const aScreenPoint: TVector; const z: Single;
  var intersectPoint: TVector): Boolean;
begin
  Result := ScreenVectorIntersectWithPlane(aScreenPoint, VectorMake(0, 0, z), ZHmgVector, intersectPoint);
  intersectPoint.W := 0;
end;

function TVXSceneBuffer.ScreenVectorIntersectWithPlaneYZ(const aScreenPoint: TVector; const x: Single;
  var intersectPoint: TVector): Boolean;
begin
  Result := ScreenVectorIntersectWithPlane(aScreenPoint, VectorMake(x, 0, 0), XHmgVector, intersectPoint);
  intersectPoint.W := 0;
end;

function TVXSceneBuffer.ScreenVectorIntersectWithPlaneXZ(const aScreenPoint: TVector; const y: Single;
  var intersectPoint: TVector): Boolean;
begin
  Result := ScreenVectorIntersectWithPlane(aScreenPoint, VectorMake(0, y, 0), YHmgVector, intersectPoint);
  intersectPoint.W := 0;
end;

function TVXSceneBuffer.PixelRayToWorld(x, y: Integer): TAffineVector;
var
  dov, np, fp, z, dst, wrpdst: Single;
  vec, cam, targ, rayhit, pix: TAffineVector;
  camAng: real;
begin
  if Camera.CameraStyle = csOrtho2D then
    dov := 2
  else
    dov := Camera.DepthOfView;
  np := Camera.NearPlane;
  fp := Camera.NearPlane + dov;
  z := GetPixelDepth(x, y);
  dst := (fp * np) / (fp - z * dov); // calc from z-buffer value to world depth
  // ------------------------
  // z:=1-(fp/d-1)/(fp/np-1);  //calc from world depth to z-buffer value
  // ------------------------
  vec.x := x;
  vec.y := FViewPort.height - y;
  vec.z := 0;
  vec := ScreenToVector(vec);
  NormalizeVector(vec);
  SetVector(cam, Camera.AbsolutePosition);
  // targ:=Camera.TargetObject.Position.AsAffineVector;
  // SubtractVector(targ,cam);
  pix.x := FViewPort.width * 0.5;
  pix.y := FViewPort.height * 0.5;
  pix.z := 0;
  targ := Self.ScreenToVector(pix);

  camAng := VectorAngleCosine(targ, vec);
  wrpdst := dst / camAng;
  rayhit := cam;
  CombineVector(rayhit, vec, wrpdst);
  Result := rayhit;
end;

// ClearBuffers
//

procedure TVXSceneBuffer.ClearBuffers;
var
  bufferBits: GLbitfield;
begin
  if roNoDepthBufferClear in ContextOptions then
    bufferBits := 0
  else
  begin
    bufferBits := GL_DEPTH_BUFFER_BIT;
    CurrentVXContext.VXStates.DepthWriteMask := True;
  end;
  if ContextOptions * [roNoColorBuffer, roNoColorBufferClear] = [] then
  begin
    bufferBits := bufferBits or GL_COLOR_BUFFER_BIT;
    CurrentVXContext.VXStates.SetColorMask(cAllColorComponents);
  end;
  if roStencilBuffer in ContextOptions then
  begin
    bufferBits := bufferBits or GL_STENCIL_BUFFER_BIT;
  end;
  if bufferBits <> 0 then
    glClear(bufferBits);
end;

procedure TVXSceneBuffer.NotifyChange(Sender: TObject);
begin
  DoChange;
end;

procedure TVXSceneBuffer.PickObjects(const rect: TVXRect; pickList: TVXPickList; objectCountGuess: Integer);
var
  i: Integer;
  obj: TVXBaseSceneObject;
begin
  if not Assigned(FCamera) then
    Exit;
  Assert((not FRendering), strAlreadyRendering);
  Assert(Assigned(pickList));
  FRenderingContext.Activate;
  FRendering := True;
  try
    // Creates best selector which techniques is hardware can do
    if not Assigned(FSelector) then
      FSelector := GetBestSelectorClass.Create;

    xglMapTexCoordToNull; // turn off
    PrepareRenderingMatrices(FViewPort, RenderDPI, @rect);
    FSelector.Hits := -1;
    if objectCountGuess > 0 then
      FSelector.objectCountGuess := objectCountGuess;
    repeat
      FSelector.Start;
      // render the scene (in select mode, nothing is drawn)
      FRenderDPI := 96;
      if Assigned(FCamera) and Assigned(FCamera.FScene) then
        RenderScene(FCamera.FScene, FViewPort.width, FViewPort.height, dsPicking, nil);
    until FSelector.Stop;
    FSelector.FillPickingList(pickList);
    for i := 0 to pickList.Count - 1 do
    begin
      obj := TVXBaseSceneObject(pickList[i]);
      if Assigned(obj.FOnPicked) then
        obj.FOnPicked(obj);
    end;
  finally
    FRendering := False;
    FRenderingContext.Deactivate;
  end;
end;

function TVXSceneBuffer.GetPickedObjects(const rect: TVXRect; objectCountGuess: Integer = 64): TVXPickList;
begin
  Result := TVXPickList.Create(psMinDepth);
  PickObjects(rect, Result, objectCountGuess);
end;

function TVXSceneBuffer.GetPickedObject(x, y: Integer): TVXBaseSceneObject;
var
  pkList: TVXPickList;
begin
  pkList := GetPickedObjects(rect(x - 1, y - 1, x + 1, y + 1));
  try
    if pkList.Count > 0 then
      Result := TVXBaseSceneObject(pkList.Hit[0])
    else
      Result := nil;
  finally
    pkList.Free;
  end;
end;

function TVXSceneBuffer.GetPixelColor(x, y: Integer): TColor;
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
    glReadPixels(x, FViewPort.height - y, 1, 1, GL_RGB, GL_UNSIGNED_BYTE, @buf[0]);
  finally
    FRenderingContext.Deactivate;
  end;
  Result := RGB(buf[0], buf[1], buf[2]);
end;

function TVXSceneBuffer.GetPixelDepth(x, y: Integer): Single;
begin
  if not Assigned(FCamera) then
  begin
    Result := 0;
    Exit;
  end;
  FRenderingContext.Activate;
  try
    glReadPixels(x, FViewPort.height - y, 1, 1, GL_DEPTH_COMPONENT, GL_FLOAT, @Result);
  finally
    FRenderingContext.Deactivate;
  end;
end;

function TVXSceneBuffer.PixelDepthToDistance(aDepth: Single): Single;
var
  dov, np, fp: Single;
begin
  if Camera.CameraStyle = csOrtho2D then
    dov := 2
  else
    dov := Camera.DepthOfView; // Depth of View (from np to fp)
  np := Camera.NearPlane; // Near plane distance
  fp := np + dov; // Far plane distance
  Result := (fp * np) / (fp - aDepth * dov);
  // calculate world distance from z-buffer value
end;

function TVXSceneBuffer.PixelToDistance(x, y: Integer): Single;
var
  z, dov, np, fp, dst, camAng: Single;
  norm, coord, vec: TAffineVector;
begin
  z := GetPixelDepth(x, y);
  if Camera.CameraStyle = csOrtho2D then
    dov := 2
  else
    dov := Camera.DepthOfView; // Depth of View (from np to fp)
  np := Camera.NearPlane; // Near plane distance
  fp := np + dov; // Far plane distance
  dst := (np * fp) / (fp - z * dov);
  // calculate from z-buffer value to frustrum depth
  coord.x := x;
  coord.y := y;
  vec := Self.ScreenToVector(coord); // get the pixel vector
  coord.x := FViewPort.width div 2;
  coord.y := FViewPort.height div 2;
  norm := Self.ScreenToVector(coord); // get the absolute camera direction
  camAng := VectorAngleCosine(norm, vec);
  Result := dst / camAng; // compensate for flat frustrum face
end;

procedure TVXSceneBuffer.NotifyMouseMove(Shift: TShiftState; x, y: Single);
begin
  // Nothing
end;

procedure TVXSceneBuffer.PrepareRenderingMatrices(const AViewport: TRectangle; resolution: Integer; pickingRect: PGLRect = nil);
begin
  RenderingContext.PipeLineTransformation.IdentityAll;
  // setup projection matrix
  if Assigned(pickingRect) then
  begin
    CurrentVXContext.PipeLineTransformation.SetProjectionMatrix(CreatePickMatrix((pickingRect^.Left + pickingRect^.Right) div 2,
      FViewPort.height - ((pickingRect^.Top + pickingRect^.Bottom) div 2), Abs(pickingRect^.Right - pickingRect^.Left),
      Abs(pickingRect^.Bottom - pickingRect^.Top), TVector4i(FViewPort)));
  end;
  FBaseProjectionMatrix := CurrentVXContext.PipeLineTransformation.ProjectionMatrix^;

  if Assigned(FCamera) then
  begin
    FCamera.Scene.FCurrentCamera := FCamera;
    // apply camera perpective
    FCamera.ApplyPerspective(AViewport, FViewPort.width, FViewPort.height, resolution);
    // setup model view matrix
    // apply camera transformation (viewpoint)
    FCamera.Apply;
    FCameraAbsolutePosition := FCamera.AbsolutePosition;
  end;
end;

procedure TVXSceneBuffer.DoBaseRender(const AViewport: TRectangle; resolution: Integer; drawState: TVXDrawState;
  baseObject: TVXBaseSceneObject);
begin
  with RenderingContext.VXStates do
  begin
    PrepareRenderingMatrices(AViewport, resolution);
    { if not ForwardContext then }
    begin
      xglMapTexCoordToNull; // force XGL rebind
      xglMapTexCoordToMain;
    end;

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
        SetupLights(maxLights);
        { if not ForwardContext then }
        begin
          if FogEnable then
          begin
            Enable(stFog);
            FogEnvironment.ApplyFog;
          end
          else
            Disable(stFog);
        end;

        RenderScene(FCamera.FScene, AViewport.width, AViewport.height, drawState, baseObject);
      end;
    end;
    if Assigned(FPostRender) then
      if Owner is TComponent then
        if not(csDesigning in TComponent(Owner).ComponentState) then
          FPostRender(Self);
  end;
  Assert(Length(FViewMatrixStack) = 0, 'Unbalance Push/PopViewMatrix.');
  Assert(Length(FProjectionMatrixStack) = 0, 'Unbalance Push/PopProjectionMatrix.');
end;

procedure TVXSceneBuffer.Render;
begin
  Render(nil);
end;

procedure TVXSceneBuffer.Render(baseObject: TVXBaseSceneObject);
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
      RenderingContext.VXStates.ColorClearValue := ConvertWinColor(FBackgroundColor, FBackgroundAlpha);
      ClearBuffers;
      glMatrixMode(GL_PROJECTION);
      glLoadIdentity;
      glMatrixMode(GL_MODELVIEW);
      glLoadIdentity;
      glRasterPos2f(-1, -1);
      glDrawPixels(FFreezedViewPort.width, FFreezedViewPort.height, GL_RGBA, GL_UNSIGNED_BYTE, FFreezeBuffer);
      if not(roNoSwapBuffers in ContextOptions) then
        RenderingContext.SwapBuffers;
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
      ClearGLError;
      SetupRenderingContext(FRenderingContext);
      // clear the buffers
      FRenderingContext.VXStates.ColorClearValue := ConvertWinColor(FBackgroundColor, FBackgroundAlpha);
      ClearBuffers;
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

procedure TVXSceneBuffer.RenderScene(aScene: TVXScene; const viewPortSizeX, viewPortSizeY: Integer; drawState: TVXDrawState;
  baseObject: TVXBaseSceneObject);

var
  i: Integer;
  rci: TVXRenderContextInfo;
  rightVector: TVector;
begin
  FAfterRenderEffects.Clear;
  aScene.FCurrentBuffer := Self;
  FillChar(rci, SizeOf(rci), 0);
  rci.Scene := aScene;
  rci.Buffer := Self;
  rci.afterRenderEffects := FAfterRenderEffects;
  rci.ObjectsSorting := aScene.ObjectsSorting;
  rci.VisibilityCulling := aScene.VisibilityCulling;
  rci.bufferFaceCull := FFaceCulling;
  rci.bufferLighting := FLighting;
  rci.bufferFog := FFogEnable;
  rci.bufferDepthTest := FDepthTest;
  rci.drawState := drawState;
  rci.sceneAmbientColor := FAmbientColor.Color;
  rci.primitiveMask := cAllMeshPrimitive;
  with FCamera do
  begin
    rci.cameraPosition := FCameraAbsolutePosition;
    rci.cameraDirection := FLastDirection;
    NormalizeVector(rci.cameraDirection);
    rci.cameraDirection.W := 0;
    rightVector := VectorCrossProduct(rci.cameraDirection, Up.AsVector);
    rci.cameraUp := VectorCrossProduct(rightVector, rci.cameraDirection);
    NormalizeVector(rci.cameraUp);

    with rci.rcci do
    begin
      origin := rci.cameraPosition;
      clippingDirection := rci.cameraDirection;
      viewPortRadius := FViewPortRadius;
      nearClippingDistance := FNearPlane;
      farClippingDistance := FNearPlane + FDepthOfView;
      frustum := RenderingContext.PipeLineTransformation.frustum;
    end;
  end;
  rci.viewPortSize.cx := viewPortSizeX;
  rci.viewPortSize.cy := viewPortSizeY;
  rci.RenderDPI := FRenderDPI;
  rci.VXStates := RenderingContext.VXStates;
  rci.PipeLineTransformation := RenderingContext.PipeLineTransformation;
  rci.proxySubObject := False;
  rci.ignoreMaterials := (roNoColorBuffer in FContextOptions) or (rci.drawState = dsPicking);
  rci.amalgamating := rci.drawState = dsPicking;
  rci.VXStates.SetColorWriting(not rci.ignoreMaterials);
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

  if RenderingContext.IsPraparationNeed then
    RenderingContext.PrepareHandlesData;

  if baseObject = nil then
  begin
    aScene.Objects.Render(rci);
  end
  else
    baseObject.Render(rci);
  rci.VXStates.SetColorWriting(True);
  with FAfterRenderEffects do
    if Count > 0 then
      for i := 0 to Count - 1 do
        TVXObjectAfterEffect(Items[i]).Render(rci);
  if Assigned(FWrapUpRendering) then
    FWrapUpRendering(Self, rci);
end;

procedure TVXSceneBuffer.SetBackgroundColor(AColor: TColor);
begin
  if FBackgroundColor <> AColor then
  begin
    FBackgroundColor := AColor;
    NotifyChange(Self);
  end;
end;

procedure TVXSceneBuffer.SetBackgroundAlpha(alpha: Single);
begin
  if FBackgroundAlpha <> alpha then
  begin
    FBackgroundAlpha := alpha;
    NotifyChange(Self);
  end;
end;

procedure TVXSceneBuffer.SetAmbientColor(AColor: TVXColor);
begin
  FAmbientColor.Assign(AColor);
end;

procedure TVXSceneBuffer.SetCamera(ACamera: TVXCamera);
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

procedure TVXSceneBuffer.SetContextOptions(Options: TContextOptions);
begin
  if FContextOptions <> Options then
  begin
    FContextOptions := Options;
    DoStructuralChange;
  end;
end;

procedure TVXSceneBuffer.SetDepthTest(aValue: Boolean);
begin
  if FDepthTest <> aValue then
  begin
    FDepthTest := aValue;
    NotifyChange(Self);
  end;
end;

procedure TVXSceneBuffer.SetFaceCulling(aValue: Boolean);
begin
  if FFaceCulling <> aValue then
  begin
    FFaceCulling := aValue;
    NotifyChange(Self);
  end;
end;

procedure TVXSceneBuffer.SetLayer(const Value: TVXContextLayer);
begin
  if FLayer <> Value then
  begin
    FLayer := Value;
    DoStructuralChange;
  end;
end;

procedure TVXSceneBuffer.SetLighting(aValue: Boolean);
begin
  if FLighting <> aValue then
  begin
    FLighting := aValue;
    NotifyChange(Self);
  end;
end;

procedure TVXSceneBuffer.SetAntiAliasing(const val: TVXAntiAliasing);
begin
  if FAntiAliasing <> val then
  begin
    FAntiAliasing := val;
    DoStructuralChange;
  end;
end;

procedure TVXSceneBuffer.SetDepthPrecision(const val: TVXDepthPrecision);
begin
  if FDepthPrecision <> val then
  begin
    FDepthPrecision := val;
    DoStructuralChange;
  end;
end;

procedure TVXSceneBuffer.SetColorDepth(const val: TVXColorDepth);
begin
  if FColorDepth <> val then
  begin
    FColorDepth := val;
    DoStructuralChange;
  end;
end;

procedure TVXSceneBuffer.SetShadeModel(const val: TVXShadeModel);
begin
  if FShadeModel <> val then
  begin
    FShadeModel := val;
    NotifyChange(Self);
  end;
end;

procedure TVXSceneBuffer.SetFogEnable(aValue: Boolean);
begin
  if FFogEnable <> aValue then
  begin
    FFogEnable := aValue;
    NotifyChange(Self);
  end;
end;

procedure TVXSceneBuffer.SetFogEnvironment(aValue: TVXFogEnvironment);
begin
  FFogEnvironment.Assign(aValue);
  NotifyChange(Self);
end;

function TVXSceneBuffer.StoreFog: Boolean;
begin
  Result := (not FFogEnvironment.IsAtDefaultValues);
end;

procedure TVXSceneBuffer.SetAccumBufferBits(const val: Integer);
begin
  if FAccumBufferBits <> val then
  begin
    FAccumBufferBits := val;
    DoStructuralChange;
  end;
end;

procedure TVXSceneBuffer.DoChange;
begin
  if (not FRendering) and Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TVXSceneBuffer.DoStructuralChange;
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

// ------------------
// ------------------ TVXNonVisualViewer ------------------
// ------------------

constructor TVXNonVisualViewer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FWidth := 256;
  FHeight := 256;
  FBuffer := TVXSceneBuffer.Create(Self);
  FBuffer.OnChange := DoBufferChange;
  FBuffer.OnStructuralChange := DoBufferStructuralChange;
  FBuffer.OnPrepareGLContext := DoOnPrepareVXContext;
end;

destructor TVXNonVisualViewer.Destroy;
begin
  FBuffer.Free;
  inherited Destroy;
end;

procedure TVXNonVisualViewer.Notification(AComponent: TComponent; Operation: TOperation);
begin
  if (Operation = opRemove) and (AComponent = Camera) then
    Camera := nil;
  inherited;
end;

procedure TVXNonVisualViewer.CopyToTexture(aTexture: TVXTexture);
begin
  CopyToTexture(aTexture, 0, 0, width, height, 0, 0);
end;

procedure TVXNonVisualViewer.CopyToTexture(aTexture: TVXTexture; xSrc, ySrc, width, height: Integer; xDest, yDest: Integer);
begin
  Buffer.CopyToTexture(aTexture, xSrc, ySrc, width, height, xDest, yDest);
end;

procedure TVXNonVisualViewer.CopyToTextureMRT(aTexture: TVXTexture; BufferIndex: Integer);
begin
  CopyToTextureMRT(aTexture, 0, 0, width, height, 0, 0, BufferIndex);
end;

procedure TVXNonVisualViewer.CopyToTextureMRT(aTexture: TVXTexture; xSrc, ySrc, width, height, xDest, yDest,
  BufferIndex: Integer);
var
  target, Handle: Integer;
  buf: Pointer;
  createTexture: Boolean;

  procedure CreateNewTexture;
  begin
    GetMem(buf, width * height * 4);
    try // float_type
      glReadPixels(0, 0, width, height, GL_RGBA, GL_UNSIGNED_BYTE, buf);
      case aTexture.MinFilter of
        miNearest, miLinear:
          glTexImage2d(target, 0, aTexture.OpenVXTextureFormat, width, height, 0, GL_RGBA, GL_UNSIGNED_BYTE, buf);
      else
        if GL_SGIS_generate_mipmap and (target = GL_TEXTURE_2D) then
        begin
          // hardware-accelerated when supported
          glTexParameteri(target, GL_GENERATE_MIPMAP_SGIS, GL_TRUE);
          glTexImage2d(target, 0, aTexture.OpenVXTextureFormat, width, height, 0, GL_RGBA, GL_UNSIGNED_BYTE, buf);
        end
        else
        begin
          glTexImage2d(target, 0, aTexture.OpenVXTextureFormat, width, height, 0, GL_RGBA, GL_UNSIGNED_BYTE, buf);
          glGenerateMipmap(target);
        end;
      end;
    finally
      FreeMem(buf);
    end;
  end;

begin
  if Buffer.RenderingContext <> nil then
  begin
    Buffer.RenderingContext.Activate;
    try
      target := DecodeTextureTarget(aTexture.Image.NativeTextureTarget);

      createTexture := True;

      if aTexture.IsFloatType then
      begin // float_type special treatment
        createTexture := False;
        Handle := aTexture.Handle;
      end
      else if (target <> GL_TEXTURE_CUBE_MAP_ARB) or (FCubeMapRotIdx = 0) then
      begin
        createTexture := not aTexture.IsHandleAllocated;
        if createTexture then
          Handle := aTexture.AllocateHandle
        else
          Handle := aTexture.Handle;
      end
      else
        Handle := aTexture.Handle;

      // For MRT
      glReadBuffer(MRT_BUFFERS[BufferIndex]);

      Buffer.RenderingContext.VXStates.TextureBinding[0, EncodeTextureTarget(target)] := Handle;

      if target = GL_TEXTURE_CUBE_MAP_ARB then
        target := GL_TEXTURE_CUBE_MAP_POSITIVE_X_ARB + FCubeMapRotIdx;

      if createTexture then
        CreateNewTexture
      else
        glCopyTexSubImage2D(target, 0, xDest, yDest, xSrc, ySrc, width, height);

      ClearGLError;
    finally
      Buffer.RenderingContext.Deactivate;
    end;
  end;
end;

procedure TVXNonVisualViewer.SetupCubeMapCamera(Sender: TObject);

{
  const
  cFaceMat: array[0..5] of TMatrix =
  (
  (X: (X:0; Y:0; Z:-1; W:0);
  Y: (X:0; Y:-1; Z:0; W:0);
  Z: (X:-1; Y:0; Z:0; W:0);
  W: (X:0; Y:0; Z:0; W:1)),
  (X:(X:2.4335928828e-08; Y:0; Z:1; W:0);
  Y:(X:0; Y:-1; Z:0; W:0);
  Z:(X:1; Y:0; Z:-2.4335928828e-08; W:0);
  W:(X:0; Y:0; Z:0; W:1)),
  (X:(X:1; Y:1.2167964414e-08; Z:-1.4805936071e-16; W:0);
  Y:(X:0; Y:-1.2167964414e-08; Z:-1; W:0);
  Z:(X:-1.2167964414e-08; Y:1; Z:-1.2167964414e-08; W:0);
  W:(X:0; Y:0; Z:0; W:1)),
  (X:(X:1; Y:-1.2167964414e-08; Z:-1.4805936071e-16; W:0);
  Y:(X:0; Y:-1.2167964414e-08; Z:1; W:0);
  Z:(X:-1.2167964414e-08; Y:-1; Z:-1.2167964414e-08; W:0);
  W:(X:0; Y:0; Z:0; W:1)),
  (X:(X:1; Y:0; Z:-1.2167964414e-08; W:0);
  Y:(X:0; Y:-1; Z:0; W:0);
  Z:(X:-1.2167964414e-08; Y:0; Z:-1; W:0);
  W:(X:0; Y:0; Z:0; W:1)),
  (X:(X:-1; Y:0; Z:-1.2167964414e-08; W:0);
  Y:(X:0; Y:-1; Z:0; W:0);
  Z:(X:-1.2167964414e-08; Y:0; Z:1; W:0);
  W:(X:0; Y:0; Z:0; W:1))
  );
}

var
  TM: TMatrix;
begin
  // Setup appropriate FOV
  with CurrentVXContext.PipeLineTransformation do
  begin
    SetProjectionMatrix(CreatePerspectiveMatrix(90, 1, FCubeMapZNear, FCubeMapZFar));
    TM := CreateTranslationMatrix(FCubeMapTranslation);
    { SetViewMatrix(MatrixMultiply(cFaceMat[FCubeMapRotIdx], TM)); }
  end;
end;

procedure TVXNonVisualViewer.RenderCubeMapTextures(cubeMapTexture: TVXTexture; zNear: Single = 0; zFar: Single = 0);
var
  oldEvent: TNotifyEvent;
begin
  Assert((width = height), 'Memory Viewer must render to a square!');
  Assert(Assigned(FBuffer.FCamera), 'Camera not specified');
  Assert(Assigned(cubeMapTexture), 'Texture not specified');

  if zFar <= 0 then
    zFar := FBuffer.FCamera.DepthOfView;
  if zNear <= 0 then
    zNear := zFar * 0.001;

  oldEvent := FBuffer.FCamera.FDeferredApply;
  FBuffer.FCamera.FDeferredApply := SetupCubeMapCamera;
  FCubeMapZNear := zNear;
  FCubeMapZFar := zFar;
  VectorScale(FBuffer.FCamera.AbsolutePosition, -1, FCubeMapTranslation);
  try
    FCubeMapRotIdx := 0;
    while FCubeMapRotIdx < 6 do
    begin
      Render;
      Buffer.CopyToTexture(cubeMapTexture, 0, 0, width, height, 0, 0, GL_TEXTURE_CUBE_MAP_POSITIVE_X + FCubeMapRotIdx);
      Inc(FCubeMapRotIdx);
    end;
  finally
    FBuffer.FCamera.FDeferredApply := oldEvent;
  end;
end;

procedure TVXNonVisualViewer.SetBeforeRender(const val: TNotifyEvent);
begin
  FBuffer.BeforeRender := val;
end;

function TVXNonVisualViewer.GetBeforeRender: TNotifyEvent;
begin
  Result := FBuffer.BeforeRender;
end;

procedure TVXNonVisualViewer.SetPostRender(const val: TNotifyEvent);
begin
  FBuffer.PostRender := val;
end;

function TVXNonVisualViewer.GetPostRender: TNotifyEvent;
begin
  Result := FBuffer.PostRender;
end;

procedure TVXNonVisualViewer.SetAfterRender(const val: TNotifyEvent);
begin
  FBuffer.AfterRender := val;
end;

function TVXNonVisualViewer.GetAfterRender: TNotifyEvent;
begin
  Result := FBuffer.AfterRender;
end;

procedure TVXNonVisualViewer.SetCamera(const val: TVXCamera);
begin
  FBuffer.Camera := val;
end;

function TVXNonVisualViewer.GetCamera: TVXCamera;
begin
  Result := FBuffer.Camera;
end;

procedure TVXNonVisualViewer.SetBuffer(const val: TVXSceneBuffer);
begin
  FBuffer.Assign(val);
end;

procedure TVXNonVisualViewer.DoOnPrepareVXContext(Sender: TObject);
begin
  PrepareVXContext;
end;

procedure TVXNonVisualViewer.PrepareVXContext;
begin
  // nothing, reserved for subclasses
end;

procedure TVXNonVisualViewer.DoBufferChange(Sender: TObject);
begin
  // nothing, reserved for subclasses
end;

procedure TVXNonVisualViewer.DoBufferStructuralChange(Sender: TObject);
begin
  FBuffer.DestroyRC;
end;

procedure TVXNonVisualViewer.SetWidth(const val: Integer);
begin
  if val <> FWidth then
  begin
    FWidth := val;
    if FWidth < 1 then
      FWidth := 1;
    DoBufferStructuralChange(Self);
  end;
end;

procedure TVXNonVisualViewer.SetHeight(const val: Integer);
begin
  if val <> FHeight then
  begin
    FHeight := val;
    if FHeight < 1 then
      FHeight := 1;
    DoBufferStructuralChange(Self);
  end;
end;

// ------------------
// ------------------ TVXMemoryViewer ------------------
// ------------------

constructor TVXMemoryViewer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  width := 256;
  height := 256;
  FBufferCount := 1;
end;

procedure TVXMemoryViewer.InstantiateRenderingContext;
begin
  if FBuffer.RenderingContext = nil then
  begin
    FBuffer.SetViewPort(0, 0, width, height);
    FBuffer.CreateRC(HWND(0), True, FBufferCount);
  end;
end;

procedure TVXMemoryViewer.Render(baseObject: TVXBaseSceneObject = nil);
begin
  InstantiateRenderingContext;
  FBuffer.Render(baseObject);
end;

procedure TVXMemoryViewer.SetBufferCount(const Value: Integer);
// var
// MaxAxuBufCount : integer;
const
  MaxAxuBufCount = 4; // Current hardware limit = 4
begin
  if FBufferCount = Value then
    Exit;
  FBufferCount := Value;

  if FBufferCount < 1 then
    FBufferCount := 1;

  if FBufferCount > MaxAxuBufCount then
    FBufferCount := MaxAxuBufCount;

  // Request a new Instantiation of RC on next render
  FBuffer.DestroyRC;
end;

// ------------------
// ------------------ TVXInitializableObjectList ------------------
// ------------------

function TVXInitializableObjectList.Add(const Item: IGLInitializable): Integer;
begin
  Result := inherited Add(Pointer(Item));
end;

function TVXInitializableObjectList.GetItems(const Index: Integer): IGLInitializable;
begin
  Result := IGLInitializable(inherited Get(Index));
end;

procedure TVXInitializableObjectList.PutItems(const Index: Integer; const Value: IGLInitializable);
begin
  inherited Put(Index, Pointer(Value));
end;

// ------------------------------------------------------------------------------
initialization

// ------------------------------------------------------------------------------

RegisterClasses([TVXLightSource, TVXCamera, TVXProxyObject, TVXScene, TVXDirectOpenVX, TVXRenderPoint, TVXMemoryViewer]);

// preparation for high resolution timer
QueryPerformanceFrequency(vCounterFrequency);

end.
