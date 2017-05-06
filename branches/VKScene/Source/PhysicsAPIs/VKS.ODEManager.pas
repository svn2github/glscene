//
// VKScene Component Library, based on GLScene http://glscene.sourceforge.net
//
{
  An ODE Manager for GLScene.
  Notes:
  This code is still under development so any part of it may change at anytime.
}

unit VKS.ODEManager;

interface

{$I VKScene.inc}

uses
  Winapi.OpenGL,
  Winapi.OpenGLext,
  System.Classes,
  System.SysUtils,

  ODEImport,
  VKS.ODEGL,
  VKS.Scene,
  VKS.VectorGeometry,
  VKS.Texture,
  VKS.XOpenGL,
  VKS.Objects,
  VKS.XCollection,
  VKS.PersistentClasses,
  VKS.VectorLists,
  VKS.Color,
  VKS.Coordinates,
  VKS.RenderContextInfo,
  VKS.Manager,
  VKS.State;

type

  TODECustomCollisionEvent = procedure(Geom1, Geom2: PdxGeom) of object;

  TODECollisionEvent = procedure(Sender: TObject; Object1, Object2: TObject;
    var Contact: TdContact; var HandleCollision: Boolean) of object;

  TODEObjectCollisionEvent = procedure(Sender: TObject; Object2: TObject;
    var Contact: TdContact; var HandleCollision: Boolean) of object;

  TODECollisionSurfaceMode = (csmMu2, csmFDir1, csmBounce, csmSoftERP,
    csmSoftCFM, csmMotion1, csmMotion2, csmSlip1, csmSlip2);
  TSurfaceModes = set of TODECollisionSurfaceMode;

  TODESolverMethod = (osmDefault, osmStepFast, osmQuickStep);

  TODEElements = class;
  TVKODEBehaviour = class;
  TODEElementBase = class;
  TODEJointBase = class;

  TVKODEManager = class(TComponent)
  private
    FWorld: PdxWorld;
    FSpace: PdxSpace;
    FContactGroup: TdJointGroupID;
    FGravity: TVKCoordinates;
    FOnCollision: TODECollisionEvent;
    FOnCustomCollision: TODECustomCollisionEvent;
    FNumContactJoints, FMaxContacts: Integer;
    FODEBehaviours: TPersistentObjectList;
    FRFContactList: TList;
    FIterations: Integer;
    FSolver: TODESolverMethod;
    FContacts: array of TdContact;
    FContactGeoms: array of TdContactGeom;
    FRenderPoint: TVKRenderPoint;
    FVisible, FVisibleAtRunTime: Boolean;
    FGeomColorDynD, FGeomColorDynE, FGeomColorStat: TVKColor;
  protected
    procedure Loaded; override;
    procedure CalcContact(Object1, Object2: TObject; var Contact: TdContact);
    procedure Collision(g1, g2: PdxGeom);
    procedure GravityChange(Sender: TObject);
    procedure SetMaxContacts(const Value: Integer);
    procedure SetGravity(Value: TVKCoordinates);
    procedure SetIterations(const val: Integer);
    function GetODEBehaviour(index: Integer): TVKODEBehaviour;
    procedure RegisterODEBehaviour(ODEBehaviour: TVKODEBehaviour);
    procedure UnregisterODEBehaviour(ODEBehaviour: TVKODEBehaviour);
    procedure SetRenderPoint(const Value: TVKRenderPoint);
    procedure RenderEvent(Sender: TObject; var rci: TVKRenderContextInfo);
    procedure RenderPointFreed(Sender: TObject);
    procedure SetVisible(const Value: Boolean);
    procedure SetVisibleAtRunTime(const Value: Boolean);
    procedure SetGeomColorDynE(const Value: TVKColor);
    procedure GeomColorChangeDynE(Sender: TObject);
    procedure SetGeomColorDynD(const Value: TVKColor);
    procedure GeomColorChangeDynD(Sender: TObject);
    procedure SetGeomColorStat(const Value: TVKColor);
    procedure GeomColorChangeStat(Sender: TObject);
    property ODEBehaviours[index: Integer]: TVKODEBehaviour read GetODEBehaviour;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Step(deltaTime: double);
    procedure NotifyChange(Sender: TObject);
    property World: PdxWorld read FWorld;
    property Space: PdxSpace read FSpace;
    property ContactGroup: TdJointGroupID read FContactGroup;
    property NumContactJoints: Integer read FNumContactJoints;
  published
    property Gravity: TVKCoordinates read FGravity write SetGravity;
    property OnCollision: TODECollisionEvent read FOnCollision
      write FOnCollision;
    property OnCustomCollision: TODECustomCollisionEvent read FOnCustomCollision
      write FOnCustomCollision;
    property Solver: TODESolverMethod read FSolver write FSolver;
    property Iterations: Integer read FIterations write SetIterations;
    property MaxContacts: Integer read FMaxContacts write SetMaxContacts;
    property RenderPoint: TVKRenderPoint read FRenderPoint write SetRenderPoint;
    property Visible: Boolean read FVisible write SetVisible;
    property VisibleAtRunTime: Boolean read FVisibleAtRunTime
      write SetVisibleAtRunTime;
    property GeomColorDynD: TVKColor read FGeomColorDynD write SetGeomColorDynD;
    property GeomColorDynE: TVKColor read FGeomColorDynE write SetGeomColorDynE;
    property GeomColorStat: TVKColor read FGeomColorStat write SetGeomColorStat;
  end;

  TODECollisionSurface = class(TPersistent)
  private
    FOwner: TPersistent;
    FSurfaceParams: TdSurfaceParameters;
    FRFCoeff: Single;
    FRFEnabled: Boolean;
  protected
    procedure WriteToFiler(writer: TWriter);
    procedure ReadFromFiler(reader: TReader);
    function GetSurfaceMode: TSurfaceModes;
    function GetMu: TdReal;
    function GetMu2: TdReal;
    function GetBounce: TdReal;
    function GetBounce_Vel: TdReal;
    function GetSoftERP: TdReal;
    function GetSoftCFM: TdReal;
    function GetMotion1: TdReal;
    function GetMotion2: TdReal;
    function GetSlip1: TdReal;
    function GetSlip2: TdReal;
    procedure SetSurfaceMode(Value: TSurfaceModes);
    procedure SetMu(Value: TdReal);
    procedure SetMu2(Value: TdReal);
    procedure SetBounce(Value: TdReal);
    procedure SetBounce_Vel(Value: TdReal);
    procedure SetSoftERP(Value: TdReal);
    procedure SetSoftCFM(Value: TdReal);
    procedure SetMotion1(Value: TdReal);
    procedure SetMotion2(Value: TdReal);
    procedure SetSlip1(Value: TdReal);
    procedure SetSlip2(Value: TdReal);
  public
    constructor Create(AOwner: TPersistent);
    function GetOwner: TPersistent; override;
    procedure Assign(Source: TPersistent); override;
  published
    property RollingFrictionCoeff: Single read FRFCoeff write FRFCoeff;
    property RollingFrictionEnabled: Boolean read FRFEnabled write FRFEnabled;
    property SurfaceMode: TSurfaceModes read GetSurfaceMode
      write SetSurfaceMode;
    property Mu: TdReal read GetMu write SetMu;
    property Mu2: TdReal read GetMu2 write SetMu2;
    property Bounce: TdReal read GetBounce write SetBounce;
    property Bounce_Vel: TdReal read GetBounce_Vel write SetBounce_Vel;
    property SoftERP: TdReal read GetSoftERP write SetSoftERP;
    property SoftCFM: TdReal read GetSoftCFM write SetSoftCFM;
    property Motion1: TdReal read GetMotion1 write SetMotion1;
    property Motion2: TdReal read GetMotion2 write SetMotion2;
    property Slip1: TdReal read GetSlip1 write SetSlip1;
    property Slip2: TdReal read GetSlip2 write SetSlip2;
  end;

  TODEElementClass = class of TODEElementBase;

  { Basis structures for behaviour style implementations. }
  TVKODEBehaviour = class(TVKBehaviour)
  private
    FManager: TVKODEManager;
    FManagerName: String;
    FSurface: TODECollisionSurface;
    FOnCollision: TODEObjectCollisionEvent;
    FInitialized: Boolean;
    FOwnerBaseSceneObject: TVKBaseSceneObject;
  protected
    procedure Initialize; virtual;
    procedure Finalize; virtual;
    procedure WriteToFiler(writer: TWriter); override;
    procedure ReadFromFiler(reader: TReader); override;
    procedure Loaded; override;
    procedure SetManager(Value: TVKODEManager);
    procedure SetSurface(Value: TODECollisionSurface);
    function GetAbsoluteMatrix: TMatrix;
  public
    constructor Create(AOwner: TVKXCollection); override;
    destructor Destroy; override;
    procedure NotifyChange(Sender: TObject);
    procedure Render(var rci: TVKRenderContextInfo); virtual;
    procedure Reinitialize;
    property Initialized: Boolean read FInitialized;
    property AbsoluteMatrix: TMatrix read GetAbsoluteMatrix;
  published
    property Manager: TVKODEManager read FManager write SetManager;
    property Surface: TODECollisionSurface read FSurface write SetSurface;
    property OnCollision: TODEObjectCollisionEvent read FOnCollision
      write FOnCollision;
  end;

  TVKODEDynamic = class(TVKODEBehaviour)
  private
    FBody: PdxBody;
    FMass: TdMass;
    FElements: TODEElements;
    FEnabled: Boolean;
    FJointRegister: TList;
  protected
    procedure Initialize; override;
    procedure Finalize; override;
    procedure WriteToFiler(writer: TWriter); override;
    procedure ReadFromFiler(reader: TReader); override;
    procedure SetMass(const Value: TdMass);
    function GetMass: TdMass;
    procedure AlignBodyToMatrix(Mat: TMatrix);
    procedure SetEnabled(const Value: Boolean);
    function GetEnabled: Boolean;
    procedure RegisterJoint(Joint: TODEJointBase);
    procedure UnregisterJoint(Joint: TODEJointBase);
  public
    constructor Create(AOwner: TVKXCollection); override;
    destructor Destroy; override;
    procedure Render(var rci: TVKRenderContextInfo); override;
    class function FriendlyName: String; override;
    class function UniqueItem: Boolean; override;
    function AddNewElement(AChild: TODEElementClass): TODEElementBase; dynamic;
    procedure AlignObject;
    function CalculateMass: TdMass;
    procedure CalibrateCenterOfMass;
    procedure AddForce(Force: TAffineVector);
    procedure AddForceAtPos(Force, Pos: TAffineVector);
    procedure AddForceAtRelPos(Force, Pos: TAffineVector);
    procedure AddRelForce(Force: TAffineVector);
    procedure AddRelForceAtPos(Force, Pos: TAffineVector);
    procedure AddRelForceAtRelPos(Force, Pos: TAffineVector);
    procedure AddTorque(Torque: TAffineVector);
    procedure AddRelTorque(Torque: TAffineVector);
    property Body: PdxBody read FBody;
    property Mass: TdMass read GetMass write SetMass;
  published
    property Elements: TODEElements read FElements;
    property Enabled: Boolean read GetEnabled write SetEnabled;
  end;

  TVKODEStatic = class(TVKODEBehaviour)
  private
    FElements: TODEElements;
  protected
    procedure Initialize; override;
    procedure Finalize; override;
    procedure WriteToFiler(writer: TWriter); override;
    procedure ReadFromFiler(reader: TReader); override;
    procedure AlignElements;
  public
    constructor Create(AOwner: TVKXCollection); override;
    destructor Destroy; override;
    procedure Render(var rci: TVKRenderContextInfo); override;
    class function FriendlyName: String; override;
    class function UniqueItem: Boolean; override;
    function AddNewElement(AChild: TODEElementClass): TODEElementBase; dynamic;
  published
    property Elements: TODEElements read FElements;
  end;

  TODEElements = class(TVKXCollection)
  private
    function GetElement(index: Integer): TODEElementBase;
  public
    destructor Destroy; override;
    class function ItemsClass: TVKXCollectionItemClass; override;
    procedure Initialize;
    procedure Finalize;
    procedure NotifyChange(Sender: TObject);
    procedure Render(var rci: TVKRenderContextInfo);
    property Element[index: Integer]: TODEElementBase read GetElement;
  end;

  TODEElementBase = class(TVKXCollectionItem)
  private
    FMass: TdMass;
    FDensity: TdReal;
    FGeomTransform, FGeomElement: PdxGeom;
    FPosition, FDirection, FUp: TVKCoordinates;
    FLocalMatrix: TMatrix;
    FRealignODE, FInitialized, FDynamic, FIsCalculating: Boolean;
  protected
    procedure Initialize; virtual;
    procedure Finalize; virtual;
    function CalculateMass: TdMass; virtual;
    procedure ODERebuild; virtual;
    procedure NotifyChange(Sender: TObject);
    procedure CoordinateChanged(Sender: TObject);
    procedure WriteToFiler(writer: TWriter); override;
    procedure ReadFromFiler(reader: TReader); override;
    function IsODEInitialized: Boolean;
    procedure AlignGeomElementToMatrix(Mat: TMatrix); virtual;
    procedure SetGeomElement(aGeom: PdxGeom);
    procedure RebuildMatrix;
    procedure RebuildVectors;
    procedure SetDensity(const Value: TdReal);
    procedure SetMatrix(const Value: TMatrix);
    function GetMatrix: TMatrix;
    procedure SetPosition(const Value: TVKCoordinates);
    procedure SetDirection(const Value: TVKCoordinates);
    procedure SetUp(const Value: TVKCoordinates);
  public
    constructor Create(AOwner: TVKXCollection); override;
    destructor Destroy; override;
    procedure Render(var rci: TVKRenderContextInfo); virtual;
    function AbsoluteMatrix: TMatrix;
    function AbsolutePosition: TAffineVector;
    property Matrix: TMatrix read GetMatrix write SetMatrix;
    property GeomTransform: PdxGeom read FGeomTransform;
    property Geom: PdxGeom read FGeomElement;
    property Initialized: Boolean read FInitialized;
  published
    property Density: TdReal read FDensity write SetDensity;
    property Position: TVKCoordinates read FPosition write SetPosition;
    property Direction: TVKCoordinates read FDirection write SetDirection;
    property Up: TVKCoordinates read FUp write SetUp;
  end;

  { ODE box implementation. }
  TODEElementBox = class(TODEElementBase)
  private
    FBoxWidth, FBoxHeight, FBoxDepth: TdReal;
  protected
    procedure Initialize; override;
    function CalculateMass: TdMass; override;
    procedure ODERebuild; override;
    procedure WriteToFiler(writer: TWriter); override;
    procedure ReadFromFiler(reader: TReader); override;
    function GetBoxWidth: TdReal;
    function GetBoxHeight: TdReal;
    function GetBoxDepth: TdReal;
    procedure SetBoxWidth(const Value: TdReal);
    procedure SetBoxHeight(const Value: TdReal);
    procedure SetBoxDepth(const Value: TdReal);
  public
    constructor Create(AOwner: TVKXCollection); override;
    procedure Render(var rci: TVKRenderContextInfo); override;
    class function FriendlyName: String; override;
    class function FriendlyDescription: String; override;
    class function ItemCategory: String; override;
  published
    property BoxWidth: TdReal read GetBoxWidth write SetBoxWidth;
    property BoxHeight: TdReal read GetBoxHeight write SetBoxHeight;
    property BoxDepth: TdReal read GetBoxDepth write SetBoxDepth;
  end;

  { ODE sphere implementation. }
  TODEElementSphere = class(TODEElementBase)
  private
    FRadius: TdReal;
  protected
    procedure Initialize; override;
    function CalculateMass: TdMass; override;
    procedure ODERebuild; override;
    procedure WriteToFiler(writer: TWriter); override;
    procedure ReadFromFiler(reader: TReader); override;
    function GetRadius: TdReal;
    procedure SetRadius(const Value: TdReal);
  public
    constructor Create(AOwner: TVKXCollection); override;
    procedure Render(var rci: TVKRenderContextInfo); override;
    class function FriendlyName: String; override;
    class function FriendlyDescription: String; override;
    class function ItemCategory: String; override;
  published
    property Radius: TdReal read GetRadius write SetRadius;
  end;

  { ODE capped cylinder implementation. }
  TODEElementCapsule = class(TODEElementBase)
  private
    FRadius, FLength: TdReal;
  protected
    procedure Initialize; override;
    function CalculateMass: TdMass; override;
    procedure ODERebuild; override;
    procedure WriteToFiler(writer: TWriter); override;
    procedure ReadFromFiler(reader: TReader); override;
    function GetRadius: TdReal;
    function GetLength: TdReal;
    procedure SetRadius(const Value: TdReal);
    procedure SetLength(const Value: TdReal);
  public
    constructor Create(AOwner: TVKXCollection); override;
    procedure Render(var rci: TVKRenderContextInfo); override;
    class function FriendlyName: String; override;
    class function FriendlyDescription: String; override;
    class function ItemCategory: String; override;
  published
    property Radius: TdReal read GetRadius write SetRadius;
    property Length: TdReal read GetLength write SetLength;
  end;

  { ODE cylinder implementation. }
  TODEElementCylinder = class(TODEElementBase)
  private
    FRadius, FLength: TdReal;
  protected
    procedure Initialize; override;
    function CalculateMass: TdMass; override;
    procedure ODERebuild; override;
    procedure WriteToFiler(writer: TWriter); override;
    procedure ReadFromFiler(reader: TReader); override;
    function GetRadius: TdReal;
    function GetLength: TdReal;
    procedure SetRadius(const Value: TdReal);
    procedure SetLength(const Value: TdReal);
  public
    constructor Create(AOwner: TVKXCollection); override;
    procedure Render(var rci: TVKRenderContextInfo); override;
    class function FriendlyName: String; override;
    class function FriendlyDescription: String; override;
    class function ItemCategory: String; override;
  published
    property Radius: TdReal read GetRadius write SetRadius;
    property Length: TdReal read GetLength write SetLength;
  end;

  { ODE tri-mesh implementation. }
  TODEElementTriMesh = class(TODEElementBase)
  private
    FTriMeshData: PdxTriMeshData;
    FVertices: TAffineVectorList;
    FIndices: TIntegerList;
  protected
    procedure Initialize; override;
    procedure Finalize; override;
    function CalculateMass: TdMass; override;
    procedure WriteToFiler(writer: TWriter); override;
    procedure ReadFromFiler(reader: TReader); override;
    procedure SetVertices(const Value: TAffineVectorList);
    procedure SetIndices(const Value: TIntegerList);
  public
    constructor Create(AOwner: TVKXCollection); override;
    destructor Destroy; override;
    class function FriendlyName: String; override;
    class function FriendlyDescription: String; override;
    class function ItemCategory: String; override;
    procedure RefreshTriMeshData;
    property Vertices: TAffineVectorList read FVertices write SetVertices;
    property Indices: TIntegerList read FIndices write SetIndices;
  end;

  { ODE plane implementation. }
  TODEElementPlane = class(TODEElementBase)
  protected
    procedure Initialize; override;
    procedure WriteToFiler(writer: TWriter); override;
    procedure ReadFromFiler(reader: TReader); override;
    procedure AlignGeomElementToMatrix(Mat: TMatrix); override;
  public
    class function FriendlyName: String; override;
    class function FriendlyDescription: String; override;
    class function ItemCategory: String; override;
    class function CanAddTo(collection: TVKXCollection): Boolean; override;
  end;

  { An XCollection decendant for ODE Joints. }
  TODEJoints = class(TVKXCollection)
  protected
    function GetJoint(index: Integer): TODEJointBase;
  public
    class function ItemsClass: TVKXCollectionItemClass; override;
    procedure Initialize;
    procedure Finalize;
    property Joint[index: Integer]: TODEJointBase read GetJoint; default;
  end;

  { Component front-end for storing ODE Joints. }
  TVKODEJointList = class(TComponent)
  private
    FJoints: TODEJoints;
  protected
    procedure WriteJoints(stream: TStream);
    procedure ReadJoints(stream: TStream);
    procedure DefineProperties(Filer: TFiler); override;
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Joints: TODEJoints read FJoints;
  end;

  TJointOption = (joBothObjectsMustBeAssigned);
  TJointOptions = set of TJointOption;

  { Base structures for ODE Joints. }
  TODEJointBase = class(TVKXCollectionItem)
  private
    FJointID: TdJointID;
    FObject1, FObject2: TVKBaseSceneObject;
    FManager: TVKODEManager;
    FObject1Name, FObject2Name, FManagerName: String;
    FInitialized, FEnabled: Boolean;
    FJointOptions: TJointOptions;
  protected
    procedure WriteToFiler(writer: TWriter); override;
    procedure ReadFromFiler(reader: TReader); override;
    procedure Loaded; override;
    function IsODEInitialized: Boolean;
    procedure RegisterJointWithObject(Obj: TVKBaseSceneObject);
    procedure UnregisterJointWithObject(Obj: TVKBaseSceneObject);
    procedure Attach;
    procedure SetManager(const Value: TVKODEManager);
    procedure SetObject1(const Value: TVKBaseSceneObject);
    procedure SetObject2(const Value: TVKBaseSceneObject);
    procedure SetEnabled(const Value: Boolean);
    procedure SetJointOptions(const Value: TJointOptions);
    property JointOptions: TJointOptions read FJointOptions
      write SetJointOptions;
  public
    constructor Create(AOwner: TVKXCollection); override;
    destructor Destroy; override;
    procedure StructureChanged; virtual;
    procedure Initialize; virtual;
    procedure Finalize; virtual;
    function IsAttached: Boolean;
    procedure DoLoaded;
    property JointID: TdJointID read FJointID;
    property Initialized: Boolean read FInitialized;
  published
    property Manager: TVKODEManager read FManager write SetManager;
    property Object1: TVKBaseSceneObject read FObject1 write SetObject1;
    property Object2: TVKBaseSceneObject read FObject2 write SetObject2;
    property Enabled: Boolean read FEnabled write SetEnabled;
  end;

  TODESetParamCallback = function(Param: Integer; const Value: TdReal)
    : Boolean of object;
  TODEGetParamCallback = function(Param: Integer; var Value: TdReal)
    : Boolean of object;

  TODEJointParams = class(TPersistent)
  private
    FOwner: TPersistent;
    FSetCallback: TODESetParamCallback;
    FGetCallback: TODEGetParamCallback;
    FLoStop, FHiStop, FVel, FFMax, FFudgeFactor, FBounce, FCFM, FStopERP,
      FStopCFM, FSuspensionERP, FSuspensionCFM: TdReal;
    FFlagLoStop, FFlagHiStop, FFlagVel, FFlagFMax, FFlagFudgeFactor,
      FFlagBounce, FFlagCFM, FFlagStopERP, FFlagStopCFM, FFlagSuspensionERP,
      FFlagSuspensionCFM: Boolean;
  protected
    function GetLoStop: TdReal;
    function GetHiStop: TdReal;
    function GetVel: TdReal;
    function GetFMax: TdReal;
    function GetFudgeFactor: TdReal;
    function GetBounce: TdReal;
    function GetCFM: TdReal;
    function GetStopERP: TdReal;
    function GetStopCFM: TdReal;
    function GetSuspensionERP: TdReal;
    function GetSuspensionCFM: TdReal;
    procedure SetLoStop(const Value: TdReal);
    procedure SetHiStop(const Value: TdReal);
    procedure SetVel(const Value: TdReal);
    procedure SetFMax(const Value: TdReal);
    procedure SetFudgeFactor(const Value: TdReal);
    procedure SetBounce(const Value: TdReal);
    procedure SetCFM(const Value: TdReal);
    procedure SetStopERP(const Value: TdReal);
    procedure SetStopCFM(const Value: TdReal);
    procedure SetSuspensionERP(const Value: TdReal);
    procedure SetSuspensionCFM(const Value: TdReal);
    procedure WriteToFiler(writer: TWriter);
    procedure ReadFromFiler(reader: TReader);
  public
    constructor Create(AOwner: TPersistent);
    function GetOwner: TPersistent; override;
    procedure Assign(Source: TPersistent); override;
    procedure ApplyFlagged;
    property SetCallback: TODESetParamCallback read FSetCallback
      write FSetCallback;
    property GetCallback: TODEGetParamCallback read FGetCallback
      write FGetCallback;
  published
    property LoStop: TdReal read GetLoStop write SetLoStop;
    property HiStop: TdReal read GetHiStop write SetHiStop;
    property Vel: TdReal read GetVel write SetVel;
    property FMax: TdReal read GetFMax write SetFMax;
    property FudgeFactor: TdReal read GetFudgeFactor write SetFudgeFactor;
    property Bounce: TdReal read GetBounce write SetBounce;
    property CFM: TdReal read GetCFM write SetCFM;
    property StopERP: TdReal read GetStopERP write SetStopERP;
    property StopCFM: TdReal read GetStopCFM write SetStopCFM;
    property SuspensionERP: TdReal read GetSuspensionERP write SetSuspensionERP;
    property SuspensionCFM: TdReal read GetSuspensionCFM write SetSuspensionCFM;
  end;

  { ODE hinge joint implementation. }
  TGLODEJointHinge = class(TODEJointBase)
  private
    FAnchor, FAxis: TVKCoordinates;
    FAxisParams: TODEJointParams;
  protected
    procedure WriteToFiler(writer: TWriter); override;
    procedure ReadFromFiler(reader: TReader); override;
    procedure SetAnchor(const Value: TVKCoordinates);
    procedure SetAxis(const Value: TVKCoordinates);
    procedure AnchorChange(Sender: TObject);
    procedure AxisChange(Sender: TObject);
    procedure SetAxisParams(const Value: TODEJointParams);
    function SetAxisParam(Param: Integer; const Value: TdReal): Boolean;
    function GetAxisParam(Param: Integer; var Value: TdReal): Boolean;
  public
    constructor Create(AOwner: TVKXCollection); override;
    destructor Destroy; override;
    procedure StructureChanged; override;
    procedure Initialize; override;
    class function FriendlyName: String; override;
    class function FriendlyDescription: String; override;
  published
    property Anchor: TVKCoordinates read FAnchor write SetAnchor;
    property Axis: TVKCoordinates read FAxis write SetAxis;
    property AxisParams: TODEJointParams read FAxisParams write SetAxisParams;
  end;

  { ODE ball joint implementation. }
  TODEJointBall = class(TODEJointBase)
  private
    FAnchor: TVKCoordinates;
  protected
    procedure WriteToFiler(writer: TWriter); override;
    procedure ReadFromFiler(reader: TReader); override;
    procedure SetAnchor(const Value: TVKCoordinates);
    procedure AnchorChange(Sender: TObject);
  public
    constructor Create(AOwner: TVKXCollection); override;
    destructor Destroy; override;
    procedure StructureChanged; override;
    procedure Initialize; override;
    class function FriendlyName: String; override;
    class function FriendlyDescription: String; override;
  published
    property Anchor: TVKCoordinates read FAnchor write SetAnchor;
  end;

  { ODE slider joint implementation. }
  TODEJointSlider = class(TODEJointBase)
  private
    FAxis: TVKCoordinates;
    FAxisParams: TODEJointParams;
  protected
    procedure WriteToFiler(writer: TWriter); override;
    procedure ReadFromFiler(reader: TReader); override;
    procedure SetAxis(const Value: TVKCoordinates);
    procedure AxisChange(Sender: TObject);
    procedure SetAxisParams(const Value: TODEJointParams);
    function SetAxisParam(Param: Integer; const Value: TdReal): Boolean;
    function GetAxisParam(Param: Integer; var Value: TdReal): Boolean;
  public
    constructor Create(AOwner: TVKXCollection); override;
    destructor Destroy; override;
    procedure StructureChanged; override;
    procedure Initialize; override;
    class function FriendlyName: String; override;
    class function FriendlyDescription: String; override;
  published
    property Axis: TVKCoordinates read FAxis write SetAxis;
    property AxisParams: TODEJointParams read FAxisParams write SetAxisParams;
  end;

  { ODE fixed joint implementation. }
  TODEJointFixed = class(TODEJointBase)
  protected
    procedure WriteToFiler(writer: TWriter); override;
    procedure ReadFromFiler(reader: TReader); override;
  public
    class function FriendlyName: String; override;
    class function FriendlyDescription: String; override;
    procedure Initialize; override;
  end;

  { ODE hinge2 joint implementation. }
  TGLODEJointHinge2 = class(TODEJointBase)
  private
    FAnchor, FAxis1, FAxis2: TVKCoordinates;
    FAxis1Params, FAxis2Params: TODEJointParams;
  protected
    procedure WriteToFiler(writer: TWriter); override;
    procedure ReadFromFiler(reader: TReader); override;
    procedure SetAnchor(const Value: TVKCoordinates);
    procedure SetAxis1(const Value: TVKCoordinates);
    procedure SetAxis2(const Value: TVKCoordinates);
    procedure AnchorChange(Sender: TObject);
    procedure Axis1Change(Sender: TObject);
    procedure Axis2Change(Sender: TObject);
    procedure SetAxis1Params(const Value: TODEJointParams);
    procedure SetAxis2Params(const Value: TODEJointParams);
    function SetAxis1Param(Param: Integer; const Value: TdReal): Boolean;
    function SetAxis2Param(Param: Integer; const Value: TdReal): Boolean;
    function GetAxis1Param(Param: Integer; var Value: TdReal): Boolean;
    function GetAxis2Param(Param: Integer; var Value: TdReal): Boolean;
  public
    constructor Create(AOwner: TVKXCollection); override;
    destructor Destroy; override;
    procedure StructureChanged; override;
    procedure Initialize; override;
    class function FriendlyName: String; override;
    class function FriendlyDescription: String; override;
  published
    property Anchor: TVKCoordinates read FAnchor write SetAnchor;
    property Axis1: TVKCoordinates read FAxis1 write SetAxis1;
    property Axis2: TVKCoordinates read FAxis2 write SetAxis2;
    property Axis1Params: TODEJointParams read FAxis1Params
      write SetAxis1Params;
    property Axis2Params: TODEJointParams read FAxis2Params
      write SetAxis2Params;
  end;

  { ODE universal joint implementation. }
  TODEJointUniversal = class(TODEJointBase)
  private
    FAnchor, FAxis1, FAxis2: TVKCoordinates;
    FAxis1Params, FAxis2Params: TODEJointParams;
  protected
    procedure WriteToFiler(writer: TWriter); override;
    procedure ReadFromFiler(reader: TReader); override;
    procedure SetAnchor(const Value: TVKCoordinates);
    procedure SetAxis1(const Value: TVKCoordinates);
    procedure SetAxis2(const Value: TVKCoordinates);
    procedure AnchorChange(Sender: TObject);
    procedure Axis1Change(Sender: TObject);
    procedure Axis2Change(Sender: TObject);
    procedure SetAxis1Params(const Value: TODEJointParams);
    procedure SetAxis2Params(const Value: TODEJointParams);
    function SetAxis1Param(Param: Integer; const Value: TdReal): Boolean;
    function SetAxis2Param(Param: Integer; const Value: TdReal): Boolean;
    function GetAxis1Param(Param: Integer; var Value: TdReal): Boolean;
    function GetAxis2Param(Param: Integer; var Value: TdReal): Boolean;
  public
    constructor Create(AOwner: TVKXCollection); override;
    destructor Destroy; override;
    procedure Initialize; override;
    procedure StructureChanged; override;
    class function FriendlyName: String; override;
    class function FriendlyDescription: String; override;
  published
    property Anchor: TVKCoordinates read FAnchor write SetAnchor;
    property Axis1: TVKCoordinates read FAxis1 write SetAxis1;
    property Axis2: TVKCoordinates read FAxis2 write SetAxis2;
    property Axis1Params: TODEJointParams read FAxis1Params
      write SetAxis1Params;
    property Axis2Params: TODEJointParams read FAxis2Params
      write SetAxis2Params;
  end;

{ ODE nearCallBack, throws near callback to the collision procedure
  of the ODE manager linked by the Data pointer. }
procedure nearCallBack(Data: Pointer; o1, o2: PdxGeom); cdecl;
{ Helper functions for extracting data from objects with different
  inheritance. }
function GetBodyFromObject(anObject: TObject): PdxBody;
function GetBodyFromVKSceneObject(anObject: TVKBaseSceneObject): PdxBody;
function GetSurfaceFromObject(anObject: TObject): TODECollisionSurface;

// GLODEObject register methods (used for joint object persistence)
procedure RegisterVKSceneObject(anObject: TVKBaseSceneObject);
procedure UnregisterVKSceneObject(anObject: TVKBaseSceneObject);
function GetVKSceneObject(anObjectName: String): TVKBaseSceneObject;

// Get and GetOrCreate functions for ode behaviours
function GetOdeStatic(Obj: TVKBaseSceneObject): TVKODEStatic;
function GetOrCreateOdeStatic(Obj: TVKBaseSceneObject): TVKODEStatic;
function GetOdeDynamic(Obj: TVKBaseSceneObject): TVKODEDynamic;
function GetOrCreateOdeDynamic(Obj: TVKBaseSceneObject): TVKODEDynamic;

var
  vGLODEObjectRegister: TList;

//=====================================================================
implementation
//=====================================================================

uses
  VKS.Context;

procedure nearCallBack(Data: Pointer; o1, o2: PdxGeom); cdecl;
begin
  TVKODEManager(Data).Collision(o1, o2);
end;

function GetBodyFromObject(anObject: TObject): PdxBody;
begin
  Result := nil;
  if Assigned(anObject) then
    if anObject is TVKODEDynamic then
      Result := TVKODEDynamic(anObject).Body;
end;

function GetBodyFromVKSceneObject(anObject: TVKBaseSceneObject): PdxBody;
var
  temp: TVKODEDynamic;
begin
  Result := nil;
  if Assigned(anObject) then
  begin
    temp := TVKODEDynamic(anObject.Behaviours.GetByClass(TVKODEDynamic));
    if temp <> nil then
      Result := temp.Body;
  end;
end;

function GetSurfaceFromObject(anObject: TObject): TODECollisionSurface;
var
  ODEBehaviour: TVKODEBehaviour;
begin
  Result := nil;
  if Assigned(anObject) then
    if anObject is TVKODEBehaviour then
      Result := TVKODEBehaviour(anObject).Surface
    else
    begin
      if (anObject is TVKBaseSceneObject) then
      begin
        ODEBehaviour := TVKODEBehaviour(TVKBaseSceneObject(anObject)
          .Behaviours.GetByClass(TVKODEBehaviour));
        if Assigned(ODEBehaviour) then
          Result := ODEBehaviour.Surface
      end;
    end;
end;

function IsGLODEObject(Obj: TVKBaseSceneObject): Boolean;
var
  temp: TVKODEDynamic;
begin
  Result := False;
  if Assigned(Obj) then
  begin
    temp := TVKODEDynamic(Obj.Behaviours.GetByClass(TVKODEDynamic));
    Result := Assigned(temp);
  end;
end;

procedure RegisterVKSceneObject(anObject: TVKBaseSceneObject);
begin
  if vGLODEObjectRegister.IndexOf(anObject) = -1 then
    vGLODEObjectRegister.Add(anObject);
end;

procedure UnregisterVKSceneObject(anObject: TVKBaseSceneObject);
begin
  vGLODEObjectRegister.Remove(anObject);
end;

function GetVKSceneObject(anObjectName: String): TVKBaseSceneObject;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to vGLODEObjectRegister.Count - 1 do
    if TVKBaseSceneObject(vGLODEObjectRegister[i]).GetNamePath = anObjectName
    then
    begin
      Result := vGLODEObjectRegister[i];
      Exit;
    end;
end;

function GetOdeStatic(Obj: TVKBaseSceneObject): TVKODEStatic;
begin
  Result := TVKODEStatic(Obj.Behaviours.GetByClass(TVKODEStatic));
end;

function GetOrCreateOdeStatic(Obj: TVKBaseSceneObject): TVKODEStatic;
begin
  Result := TVKODEStatic(Obj.GetOrCreateBehaviour(TVKODEStatic));
end;

function GetOdeDynamic(Obj: TVKBaseSceneObject): TVKODEDynamic;
begin
  Result := TVKODEDynamic(Obj.Behaviours.GetByClass(TVKODEDynamic));
end;

function GetOrCreateOdeDynamic(Obj: TVKBaseSceneObject): TVKODEDynamic;
begin
  Result := TVKODEDynamic(Obj.GetOrCreateBehaviour(TVKODEDynamic));
end;

// ---------------
// --------------- TVKODEManager ---------------
// ---------------

constructor TVKODEManager.Create(AOwner: TComponent);
begin
  FWorld := nil;
  if not InitODE('') then
    raise Exception.Create('ODE failed to initialize.');

  inherited;

  FODEBehaviours := TPersistentObjectList.Create;
  FRFContactList := TList.Create;

  FGravity := TVKCoordinates.CreateInitialized(Self, NullHmgPoint, csVector);
  FGravity.OnNotifyChange := GravityChange;

  FSolver := osmDefault;
  FIterations := 3;
  MaxContacts := 8;

  if not(csDesigning in ComponentState) then
  begin
    FWorld := dWorldCreate;
    FSpace := dHashSpaceCreate(nil);
    dWorldSetCFM(FWorld, 1E-5);
    dWorldSetQuickStepNumIterations(FWorld, FIterations);
    FContactGroup := dJointGroupCreate(100);
  end;

  FGeomColorDynD := TVKColor.CreateInitialized(Self, clrRed,
    GeomColorChangeDynD);
  FGeomColorDynE := TVKColor.CreateInitialized(Self, clrLime,
    GeomColorChangeDynE);
  FGeomColorStat := TVKColor.CreateInitialized(Self, clrBlue,
    GeomColorChangeStat);

  RegisterManager(Self);
end;

destructor TVKODEManager.Destroy;
begin
  RenderPoint := nil;

  // Unregister everything
  while FODEBehaviours.Count > 0 do
    ODEBehaviours[0].Manager := nil;

  // Clean up everything
  FODEBehaviours.Free;
  FGravity.Free;
  FRFContactList.Free;

  if Assigned(FWorld) then
  begin
    dJointGroupEmpty(FContactGroup);
    dJointGroupDestroy(FContactGroup);
    dSpaceDestroy(FSpace);
    dWorldDestroy(FWorld);
  end;

  FGeomColorDynD.Free;
  FGeomColorDynE.Free;
  FGeomColorStat.Free;

  DeregisterManager(Self);
  inherited Destroy;
end;

procedure TVKODEManager.RegisterODEBehaviour(ODEBehaviour: TVKODEBehaviour);
begin
  FODEBehaviours.Add(ODEBehaviour);
end;

procedure TVKODEManager.UnregisterODEBehaviour(ODEBehaviour: TVKODEBehaviour);
begin
  FODEBehaviours.Remove(ODEBehaviour);
end;

procedure TVKODEManager.Loaded;
begin
  GravityChange(Self);
end;

procedure TVKODEManager.SetGravity(Value: TVKCoordinates);
begin
  FGravity.SetPoint(Value.DirectX, Value.DirectY, Value.DirectZ);
end;

procedure TVKODEManager.GravityChange(Sender: TObject);
begin
  if Assigned(FWorld) then
    dWorldSetGravity(FWorld, FGravity.X, FGravity.Y, FGravity.Z);
end;

procedure TVKODEManager.CalcContact(Object1, Object2: TObject;
  var Contact: TdContact);
var
  Surface1, Surface2: TODECollisionSurface;
  Body1, Body2: PdxBody;
begin
  Surface1 := GetSurfaceFromObject(Object1);
  Surface2 := GetSurfaceFromObject(Object2);
  if not(Assigned(Surface1) and Assigned(Surface2)) then
    Exit;

  with Contact.Surface do
  begin
    // Average the involved contact information and assign it to the contact.
    // Better methods for contact calculation will be looked into in the future.
    mode := Surface1.FSurfaceParams.mode or Surface2.FSurfaceParams.mode;
    Mu := (Surface1.Mu + Surface2.Mu) * 0.5;
    Mu2 := (Surface1.Mu2 + Surface2.Mu2) * 0.5;
    Bounce := (Surface1.Bounce + Surface2.Bounce) * 0.5;
    Bounce_Vel := (Surface1.Bounce_Vel + Surface2.Bounce_Vel) * 0.5;
    soft_erp := (Surface1.SoftERP + Surface2.SoftERP) * 0.5;
    soft_cfm := (Surface1.SoftCFM + Surface2.SoftCFM) * 0.5;
    Motion1 := (Surface1.Motion1 + Surface2.Motion1) * 0.5;
    Motion2 := (Surface1.Motion2 + Surface2.Motion2) * 0.5;
    Slip1 := (Surface1.Slip1 + Surface2.Slip1) * 0.5;
    Slip2 := (Surface1.Slip2 + Surface2.Slip2) * 0.5;
  end;

  // Rolling friction
  Body1 := GetBodyFromObject(Object1);
  Body2 := GetBodyFromObject(Object2);
  if (Surface1.RollingFrictionEnabled) and Assigned(Body1) then
    FRFContactList.Add(Object1);
  if (Surface2.RollingFrictionEnabled) and Assigned(Body2) then
    FRFContactList.Add(Object2);
end;

procedure TVKODEManager.Collision(g1, g2: PdxGeom);
var
  i, flags, num_contacts: Integer;
  Obj1, Obj2: Pointer;
  b1, b2: PdxBody;
  Joint: TdJointID;
  HandleCollision: Boolean;
begin
  // Check for custom collision handling event
  if Assigned(FOnCustomCollision) then
  begin
    FOnCustomCollision(g1, g2);
    Exit;
  end;

  Obj1 := dGeomGetData(g1);
  Obj2 := dGeomGetData(g2);
  b1 := dGeomGetBody(g1);
  b2 := dGeomGetBody(g2);

  // don't create contact between static objects
  if not Assigned(b1) and not Assigned(b2) then
    Exit;

  if Assigned(b1) and Assigned(b2) then
    if dAreConnected(b1, b2) = 1 then
      Exit;

  // Get the collisions
  flags := $0000FFFF and FMaxContacts;
  num_contacts := dCollide(g1, g2, flags, FContactGeoms[0],
    SizeOf(TdContactGeom));

  // Set up the initial contact info
  for i := 0 to num_contacts - 1 do
  begin
    FContacts[i].Geom := FContactGeoms[i];
  end;

  for i := 0 to num_contacts - 1 do
  begin
    HandleCollision := True;

    if Assigned(Obj1) and Assigned(Obj2) then
    begin
      // Calculate the contact based on Obj1 and Obj2 surface info
      CalcContact(Obj1, Obj2, FContacts[i]);
      if Assigned(FOnCollision) then
      begin
        // Fire the Scene level OnCollision event for last minute
        // customization to the contact before the contact joint
        // is created
        FOnCollision(Self, Obj1, Obj2, FContacts[i], HandleCollision);
      end;
      // Fire the OnCollision event for each object
      if TObject(Obj1) is TVKODEBehaviour then
        if Assigned(TVKODEBehaviour(Obj1).FOnCollision) then
          TVKODEBehaviour(Obj1).FOnCollision(Self, Obj2, FContacts[i],
            HandleCollision);
      if TObject(Obj2) is TVKODEBehaviour then
        if Assigned(TVKODEBehaviour(Obj2).FOnCollision) then
          TVKODEBehaviour(Obj2).FOnCollision(Self, Obj1, FContacts[i],
            HandleCollision);
    end
    else
    begin
      // Default surface values
      FContacts[i].Surface.Mu := 1000;
    end;
    if HandleCollision then
    begin
      // Create and assign the contact joint
      Joint := dJointCreateContact(FWorld, FContactGroup, @FContacts[i]);
      dJointAttach(Joint, b1, b2);
      // Increment the number of contact joints this step
      Inc(FNumContactJoints);
    end;
  end;
end;

procedure TVKODEManager.Step(deltaTime: double);
var
  i: Integer;
  vec: PdVector3;
  Body: PdxBody;
  Coeff: Single;
begin
  if not Assigned(World) then
    Exit;

  // Reset the contact joint counter
  FNumContactJoints := 0;

  // Align static elements to their GLScene parent objects
  for i := 0 to FODEBehaviours.Count - 1 do
    if ODEBehaviours[i] is TVKODEStatic then
      if ODEBehaviours[i].Initialized then
        TVKODEStatic(ODEBehaviours[i]).AlignElements;

  // Run ODE collisions and step the scene
  dSpaceCollide(FSpace, Self, nearCallBack);
  case FSolver of
    osmDefault:
      dWorldStep(FWorld, deltaTime);
    osmStepFast:
      dWorldStepFast1(FWorld, deltaTime, FIterations);
    osmQuickStep:
      dWorldQuickStep(FWorld, deltaTime);
  end;
  dJointGroupEmpty(FContactGroup);

  // Align dynamic objects to their ODE bodies
  for i := 0 to FODEBehaviours.Count - 1 do
    if ODEBehaviours[i] is TVKODEDynamic then
      if ODEBehaviours[i].Initialized then
        TVKODEDynamic(ODEBehaviours[i]).AlignObject;

  // Process rolling friction
  Coeff := 0;
  Body := nil;
  while FRFContactList.Count > 0 do
  begin
    if TObject(FRFContactList[0]) is TVKODEDynamic then
    begin
      Body := TVKODEDynamic(FRFContactList[0]).Body;
      Coeff := 1 - (TVKODEDynamic(FRFContactList[0])
        .Surface.RollingFrictionCoeff / TVKODEDynamic(FRFContactList[0])
        .Mass.Mass);
    end;
    vec := dBodyGetAngularVel(Body);
    dBodySetAngularVel(Body, vec[0] * Coeff, vec[1] * Coeff, vec[2] * Coeff);
    FRFContactList.Delete(0);
  end;
end;

procedure TVKODEManager.NotifyChange(Sender: TObject);
begin
  if Assigned(RenderPoint) then
    RenderPoint.StructureChanged;
end;

procedure TVKODEManager.SetIterations(const val: Integer);
begin
  FIterations := val;
  if Assigned(FWorld) then
    dWorldSetQuickStepNumIterations(FWorld, FIterations);
end;

procedure TVKODEManager.SetMaxContacts(const Value: Integer);
begin
  if Value <> FMaxContacts then
  begin
    FMaxContacts := Value;
    SetLength(FContacts, FMaxContacts);
    SetLength(FContactGeoms, FMaxContacts);
  end;
end;

function TVKODEManager.GetODEBehaviour(index: Integer): TVKODEBehaviour;
begin
  Result := TVKODEBehaviour(FODEBehaviours[index]);
end;

procedure TVKODEManager.SetRenderPoint(const Value: TVKRenderPoint);
begin
  if FRenderPoint <> Value then
  begin
    if Assigned(FRenderPoint) then
      FRenderPoint.UnRegisterCallBack(RenderEvent);
    FRenderPoint := Value;
    if Assigned(FRenderPoint) then
      FRenderPoint.RegisterCallBack(RenderEvent, RenderPointFreed);
  end;
end;

procedure TVKODEManager.RenderEvent(Sender: TObject;
  var rci: TVKRenderContextInfo);
var
  i: Integer;
begin
  if not Visible then
    Exit;
  if not(csDesigning in ComponentState) then
    if not VisibleAtRunTime then
      Exit;

  rci.VKStates.Disable(stLighting);
  rci.VKStates.Enable(stPolygonOffsetLine);
  rci.VKStates.SetPolygonOffset(1, 2);

  for i := 0 to FODEBehaviours.Count - 1 do
  begin
    if ODEBehaviours[i] is TVKODEDynamic then
      if TVKODEDynamic(ODEBehaviours[i]).GetEnabled then
        glColor4fv(GeomColorDynE.AsAddress)
      else
        glColor4fv(GeomColorDynD.AsAddress)
    else
      glColor4fv(GeomColorStat.AsAddress);

    ODEBehaviours[i].Render(rci);
  end;
end;

procedure TVKODEManager.RenderPointFreed(Sender: TObject);
begin
  FRenderPoint := nil;
end;

procedure TVKODEManager.SetVisible(const Value: Boolean);
begin
  if Value <> FVisible then
  begin
    FVisible := Value;
    NotifyChange(Self);
  end;
end;

procedure TVKODEManager.SetVisibleAtRunTime(const Value: Boolean);
begin
  if Value <> FVisibleAtRunTime then
  begin
    FVisibleAtRunTime := Value;
    NotifyChange(Self);
  end;
end;

procedure TVKODEManager.SetGeomColorDynD(const Value: TVKColor);
begin
  FGeomColorDynD.Assign(Value);
  NotifyChange(Self);
end;

procedure TVKODEManager.GeomColorChangeDynD(Sender: TObject);
begin
  NotifyChange(Self);
end;

procedure TVKODEManager.SetGeomColorDynE(const Value: TVKColor);
begin
  FGeomColorDynE.Assign(Value);
  NotifyChange(Self);
end;

procedure TVKODEManager.GeomColorChangeDynE(Sender: TObject);
begin
  NotifyChange(Self);
end;

procedure TVKODEManager.SetGeomColorStat(const Value: TVKColor);
begin
  FGeomColorStat.Assign(Value);
  NotifyChange(Self);
end;

procedure TVKODEManager.GeomColorChangeStat(Sender: TObject);
begin
  NotifyChange(Self);
end;

// ---------------
// --------------- TODECollisionSurface ---------------
// ---------------

constructor TODECollisionSurface.Create(AOwner: TPersistent);
begin
  inherited Create;
  FOwner := AOwner;
  Mu := 1000;
  RollingFrictionEnabled := False;
  RollingFrictionCoeff := 0.001; // Larger Coeff = more friction
end;

function TODECollisionSurface.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

procedure TODECollisionSurface.Assign(Source: TPersistent);
begin
  inherited;
  if not Assigned(Source) then
    Exit;
  if Source is TODECollisionSurface then
  begin
    RollingFrictionCoeff := TODECollisionSurface(Source).RollingFrictionCoeff;
    RollingFrictionEnabled := TODECollisionSurface(Source)
      .RollingFrictionEnabled;
    SurfaceMode := TODECollisionSurface(Source).SurfaceMode;
    Mu := TODECollisionSurface(Source).Mu;
    Mu2 := TODECollisionSurface(Source).Mu2;
    Bounce := TODECollisionSurface(Source).Bounce;
    Bounce_Vel := TODECollisionSurface(Source).Bounce_Vel;
    SoftERP := TODECollisionSurface(Source).SoftERP;
    SoftCFM := TODECollisionSurface(Source).SoftCFM;
    Motion1 := TODECollisionSurface(Source).Motion1;
    Motion2 := TODECollisionSurface(Source).Motion2;
    Slip1 := TODECollisionSurface(Source).Slip1;
    Slip2 := TODECollisionSurface(Source).Slip2;
  end;
end;

procedure TODECollisionSurface.WriteToFiler(writer: TWriter);
var
  mode: TSurfaceModes;
begin
  with writer do
  begin
    WriteInteger(0);
    WriteFloat(RollingFrictionCoeff);
    WriteBoolean(RollingFrictionEnabled);
    mode := SurfaceMode;
    Write(mode, SizeOf(TSurfaceModes));
    WriteFloat(Mu);
    WriteFloat(Mu2);
    WriteFloat(Bounce);
    WriteFloat(Bounce_Vel);
    WriteFloat(SoftERP);
    WriteFloat(SoftCFM);
    WriteFloat(Motion1);
    WriteFloat(Motion2);
    WriteFloat(Slip1);
    WriteFloat(Slip2);
  end;
end;

procedure TODECollisionSurface.ReadFromFiler(reader: TReader);
var
  archiveVersion: Integer;
  mode: TSurfaceModes;
begin
  with reader do
  begin
    archiveVersion := ReadInteger;
    Assert(archiveVersion = 0);
    RollingFrictionCoeff := ReadFloat;
    RollingFrictionEnabled := ReadBoolean;
    Read(mode, SizeOf(TSurfaceModes));
    SurfaceMode := mode;
    Mu := ReadFloat;
    Mu2 := ReadFloat;
    Bounce := ReadFloat;
    Bounce_Vel := ReadFloat;
    SoftERP := ReadFloat;
    SoftCFM := ReadFloat;
    Motion1 := ReadFloat;
    Motion2 := ReadFloat;
    Slip1 := ReadFloat;
    Slip2 := ReadFloat;
  end;
end;

// GetSurfaceMode
//
function TODECollisionSurface.GetSurfaceMode: TSurfaceModes;
var
  ASurfaceModes: TSurfaceModes;
begin
  ASurfaceModes := [];
  if (FSurfaceParams.mode and dContactSlip2) <> 0 then
    ASurfaceModes := ASurfaceModes + [csmSlip2];
  if (FSurfaceParams.mode and dContactSlip1) <> 0 then
    ASurfaceModes := ASurfaceModes + [csmSlip1];
  if (FSurfaceParams.mode and dContactMotion2) <> 0 then
    ASurfaceModes := ASurfaceModes + [csmMotion2];
  if (FSurfaceParams.mode and dContactMotion1) <> 0 then
    ASurfaceModes := ASurfaceModes + [csmMotion1];
  if (FSurfaceParams.mode and dContactSoftCFM) <> 0 then
    ASurfaceModes := ASurfaceModes + [csmSoftCFM];
  if (FSurfaceParams.mode and dContactSoftERP) <> 0 then
    ASurfaceModes := ASurfaceModes + [csmSoftERP];
  if (FSurfaceParams.mode and dContactBounce) <> 0 then
    ASurfaceModes := ASurfaceModes + [csmBounce];
  if (FSurfaceParams.mode and dContactFDir1) <> 0 then
    ASurfaceModes := ASurfaceModes + [csmFDir1];
  if (FSurfaceParams.mode and dContactMu2) <> 0 then
    ASurfaceModes := ASurfaceModes + [csmMu2];
  Result := ASurfaceModes;
end;

// SetSurfaceMode
//
procedure TODECollisionSurface.SetSurfaceMode(Value: TSurfaceModes);
var
  AMode: Integer;
begin
  AMode := 0;
  if csmSlip2 in Value then
    AMode := AMode or dContactSlip2;
  if csmSlip1 in Value then
    AMode := AMode or dContactSlip1;
  if csmMotion2 in Value then
    AMode := AMode or dContactMotion2;
  if csmMotion1 in Value then
    AMode := AMode or dContactMotion1;
  if csmSoftCFM in Value then
    AMode := AMode or dContactSoftCFM;
  if csmSoftERP in Value then
    AMode := AMode or dContactSoftERP;
  if csmBounce in Value then
    AMode := AMode or dContactBounce;
  if csmFDir1 in Value then
    AMode := AMode or dContactFDir1;
  if csmMu2 in Value then
    AMode := AMode or dContactMu2;
  FSurfaceParams.mode := AMode;
end;

// CollisionSurface Property methods
//
function TODECollisionSurface.GetMu: TdReal;
begin
  Result := FSurfaceParams.Mu;
end;

function TODECollisionSurface.GetMu2: TdReal;
begin
  Result := FSurfaceParams.Mu2;
end;

function TODECollisionSurface.GetBounce: TdReal;
begin
  Result := FSurfaceParams.Bounce;
end;

function TODECollisionSurface.GetBounce_Vel: TdReal;
begin
  Result := FSurfaceParams.Bounce_Vel;
end;

function TODECollisionSurface.GetSoftERP: TdReal;
begin
  Result := FSurfaceParams.soft_erp;
end;

function TODECollisionSurface.GetSoftCFM: TdReal;
begin
  Result := FSurfaceParams.soft_cfm;
end;

function TODECollisionSurface.GetMotion1: TdReal;
begin
  Result := FSurfaceParams.Motion1;
end;

function TODECollisionSurface.GetMotion2: TdReal;
begin
  Result := FSurfaceParams.Motion2;
end;

function TODECollisionSurface.GetSlip1: TdReal;
begin
  Result := FSurfaceParams.Slip1;
end;

function TODECollisionSurface.GetSlip2: TdReal;
begin
  Result := FSurfaceParams.Slip2;
end;

procedure TODECollisionSurface.SetMu(Value: TdReal);
begin
  FSurfaceParams.Mu := Value;
end;

procedure TODECollisionSurface.SetMu2(Value: TdReal);
begin
  FSurfaceParams.Mu2 := Value;
end;

procedure TODECollisionSurface.SetBounce(Value: TdReal);
begin
  FSurfaceParams.Bounce := Value;
end;

procedure TODECollisionSurface.SetBounce_Vel(Value: TdReal);
begin
  FSurfaceParams.Bounce_Vel := Value;
end;

procedure TODECollisionSurface.SetSoftERP(Value: TdReal);
begin
  FSurfaceParams.soft_erp := Value;
end;

procedure TODECollisionSurface.SetSoftCFM(Value: TdReal);
begin
  FSurfaceParams.soft_cfm := Value;
end;

procedure TODECollisionSurface.SetMotion1(Value: TdReal);
begin
  FSurfaceParams.Motion1 := Value;
end;

procedure TODECollisionSurface.SetMotion2(Value: TdReal);
begin
  FSurfaceParams.Motion2 := Value;
end;

procedure TODECollisionSurface.SetSlip1(Value: TdReal);
begin
  FSurfaceParams.Slip1 := Value;
end;

procedure TODECollisionSurface.SetSlip2(Value: TdReal);
begin
  FSurfaceParams.Slip2 := Value;
end;


// ---------------
// --------------- TVKODEBehaviour --------------
// ---------------

// Create
//
constructor TVKODEBehaviour.Create(AOwner: TVKXCollection);
begin
  inherited;
  FSurface := TODECollisionSurface.Create(Self);
  FInitialized := False;
  FOwnerBaseSceneObject := OwnerBaseSceneObject;
  if Assigned(FOwnerBaseSceneObject) then
    RegisterVKSceneObject(OwnerBaseSceneObject);
end;

// Destroy
//
destructor TVKODEBehaviour.Destroy;
begin
  if Assigned(Manager) then
    Manager := nil;
  if Assigned(FOwnerBaseSceneObject) then
    UnregisterVKSceneObject(FOwnerBaseSceneObject);
  FSurface.Free;
  inherited;
end;

// Initialize
//
procedure TVKODEBehaviour.Initialize;
begin
  FInitialized := True;
end;

// Finalize
//
procedure TVKODEBehaviour.Finalize;
begin
  FInitialized := False;
end;

// Reinitialize
//
procedure TVKODEBehaviour.Reinitialize;
begin
  if Initialized then
    Finalize;
  Initialize;
end;

// WriteToFiler
//
procedure TVKODEBehaviour.WriteToFiler(writer: TWriter);
begin
  inherited;
  with writer do
  begin
    WriteInteger(0); // Archive version
    if Assigned(FManager) then
      WriteString(FManager.GetNamePath)
    else
      WriteString('');
    Surface.WriteToFiler(writer);
  end;
end;

// ReadFromFiler
//
procedure TVKODEBehaviour.ReadFromFiler(reader: TReader);
begin
  inherited;
  with reader do
  begin
    Assert(ReadInteger = 0); // Archive version
    FManagerName := ReadString;
    Surface.ReadFromFiler(reader);
  end;
end;

// Loaded
//
procedure TVKODEBehaviour.Loaded;
var
  mng: TComponent;
begin
  inherited;
  if FManagerName <> '' then
  begin
    mng := FindManager(TVKODEManager, FManagerName);
    if Assigned(mng) then
      Manager := TVKODEManager(mng);
    FManagerName := '';
  end
end;

// Render
//
procedure TVKODEBehaviour.Render(var rci: TVKRenderContextInfo);
begin
  // virtual
end;

// NotifyChange
//
procedure TVKODEBehaviour.NotifyChange(Sender: TObject);
begin
  if Assigned(Manager) then
    Manager.NotifyChange(Self);
end;

// SetManager
//
procedure TVKODEBehaviour.SetManager(Value: TVKODEManager);
begin
  if FManager <> Value then
  begin
    if Assigned(FManager) then
    begin
      if Initialized then
        Finalize;
      FManager.UnregisterODEBehaviour(Self);
    end;
    FManager := Value;
    if Assigned(FManager) then
    begin
      if not(csDesigning in TComponent(Owner.Owner).ComponentState) then
      // mrqzzz moved here
        Initialize;
      FManager.RegisterODEBehaviour(Self);
    end;
  end;
end;

// SetSurface
//
procedure TVKODEBehaviour.SetSurface(Value: TODECollisionSurface);
begin
  FSurface.Assign(Value);
end;

// GetAbsoluteMatrix
//
function TVKODEBehaviour.GetAbsoluteMatrix: TMatrix;
begin
  Result := IdentityHMGMatrix;
  if Assigned(Owner.Owner) then
    if Owner.Owner is TVKBaseSceneObject then
      Result := TVKBaseSceneObject(Owner.Owner).AbsoluteMatrix;
end;


// ---------------
// --------------- TVKODEDynamic ---------------
// ---------------

// Create
//
constructor TVKODEDynamic.Create(AOwner: TVKXCollection);
begin
  inherited;
  FElements := TODEElements.Create(Self);
  FJointRegister := TList.Create;
  FEnabled := True;
end;

// Destroy
//
destructor TVKODEDynamic.Destroy;
begin
  FElements.Free;
  FJointRegister.Free;
  inherited;
end;

// Render
//
procedure TVKODEDynamic.Render(var rci: TVKRenderContextInfo);
var
  Mat: TMatrix;
begin
  if Assigned(Owner.Owner) then
  begin
    rci.PipelineTransformation.Push;
    Mat := TVKBaseSceneObject(Owner.Owner).AbsoluteMatrix;
    rci.PipelineTransformation.ModelMatrix := Mat;
  end;

  Elements.Render(rci);

  if Assigned(Owner.Owner) then
    rci.PipelineTransformation.Pop;
end;

// FriendlyName
//
class function TVKODEDynamic.FriendlyName: String;
begin
  Result := 'ODE Dynamic';
end;

// Initialize
//
procedure TVKODEDynamic.Initialize;
var
  i: Integer;
begin
  if (not Assigned(Manager)) or Assigned(FBody) or (FInitialized) then
    Exit;
  if not Assigned(Manager.World) then
    Exit;

  FBody := dBodyCreate(Manager.World);
  AlignBodyToMatrix(OwnerBaseSceneObject.AbsoluteMatrix);
  dMassSetZero(FMass);
  FElements.Initialize;
  CalculateMass;
  CalibrateCenterOfMass;
  if (FMass.Mass > 0) and (FBody <> nil) then // mrqzzz
    dBodySetMass(FBody, @FMass);
  Enabled := FEnabled;

  for i := 0 to FJointRegister.Count - 1 do
    TODEJointBase(FJointRegister[i]).Attach;

  inherited;
end;

// Finalize
//
procedure TVKODEDynamic.Finalize;
var
  i: Integer;
begin
  if not FInitialized then
    Exit;
  FElements.Finalize;
  if Assigned(FBody) then
  begin
    dBodyDestroy(FBody);
    FBody := nil;
  end;
  dMassSetZero(FMass);
  for i := 0 to FJointRegister.Count - 1 do
    TODEJointBase(FJointRegister[i]).Attach;
  inherited;
end;

// WriteToFiler
//
procedure TVKODEDynamic.WriteToFiler(writer: TWriter);
begin
  inherited;
  with writer do
  begin
    WriteInteger(1); // Archive version
    FElements.WriteToFiler(writer);
    writer.WriteBoolean(FEnabled);
  end;
end;

// ReadFromFiler
//
procedure TVKODEDynamic.ReadFromFiler(reader: TReader);
var
  archiveVersion: Integer;
begin
  inherited;
  with reader do
  begin
    archiveVersion := ReadInteger;
    Assert((archiveVersion >= 0) and (archiveVersion <= 1)); // Archive version

    // version 0
    FElements.ReadFromFiler(reader);

    // version 1
    if archiveVersion >= 1 then
    begin
      FEnabled := ReadBoolean;
    end;
  end;
end;

// RegisterJoint
//
procedure TVKODEDynamic.RegisterJoint(Joint: TODEJointBase);
begin
  if FJointRegister.IndexOf(Joint) = -1 then
    FJointRegister.Add(Joint);
end;

// UnregisterJoint
//
procedure TVKODEDynamic.UnregisterJoint(Joint: TODEJointBase);
begin
  if FJointRegister.IndexOf(Joint) > -1 then
    FJointRegister.Remove(Joint);
end;

// AddNewElement
//
function TVKODEDynamic.AddNewElement(AChild: TODEElementClass): TODEElementBase;
var
  calcmass: TdMass;
begin
  Result := AChild.Create(FElements);
  // FElements.Add(Result);
  Result.Initialize;
  calcmass := CalculateMass;
  if (calcmass.Mass > 0) and (FBody <> nil) then // mrqzzz
    dBodySetMass(FBody, @calcmass);
end;

// AlignObject
//
procedure TVKODEDynamic.AlignObject;
var
  Pos: PdVector3;
  R: PdMatrix3;
  m: TMatrix;
begin
  Pos := dBodyGetPosition(Body);
  R := dBodyGetRotation(Body);
  ODERToVKSceneMatrix(m, R^, Pos^);
  if OwnerBaseSceneObject.Parent is TVKBaseSceneObject then
    m := MatrixMultiply(m, OwnerBaseSceneObject.Parent.InvAbsoluteMatrix);
  OwnerBaseSceneObject.Matrix := m;
end;

// AlignBodyToMatrix
//
procedure TVKODEDynamic.AlignBodyToMatrix(Mat: TMatrix);
var
  R: TdMatrix3;
begin
  if not Assigned(FBody) then
    Exit;
  R[0] := Mat.X.X;
  R[1] := Mat.Y.X;
  R[2] := Mat.Z.X;
  R[3] := 0;
  R[4] := Mat.X.Y;
  R[5] := Mat.Y.Y;
  R[6] := Mat.Z.Y;
  R[7] := 0;
  R[8] := Mat.X.Z;
  R[9] := Mat.Y.Z;
  R[10] := Mat.Z.Z;
  R[11] := 0;
  dBodySetRotation(FBody, R);
  dBodySetPosition(FBody, Mat.W.X, Mat.W.Y, Mat.W.Z);
end;

// CalculateMass
//
function TVKODEDynamic.CalculateMass: TdMass;
var
  i: Integer;
  m: TdMass;
begin
  dMassSetZero(FMass);
  for i := 0 to Elements.Count - 1 do
  begin
    m := TODEElementBase(Elements[i]).CalculateMass;
    dMassAdd(FMass, m);
  end;
  Result := FMass;
end;

// CalibrateCenterOfMass
//
procedure TVKODEDynamic.CalibrateCenterOfMass;
var
  Pos: TAffineVector;
begin
  SetAffineVector(Pos, FMass.c[0], FMass.c[1], FMass.c[2]);
  NegateVector(Pos);
  dMassTranslate(FMass, Pos.X, Pos.Y, Pos.Z);
end;

// GetMass
//
function TVKODEDynamic.GetMass: TdMass;
begin
  dBodyGetMass(FBody, FMass);
  Result := FMass;
end;

// SetMass
//
procedure TVKODEDynamic.SetMass(const Value: TdMass);
begin
  FMass := Value;
  if FMass.Mass > 0 then
    dBodySetMass(FBody, @FMass);
end;

// UniqueItem
//
class function TVKODEDynamic.UniqueItem: Boolean;
begin
  Result := True;
end;

// SetEnabled
//
procedure TVKODEDynamic.SetEnabled(const Value: Boolean);
begin
  FEnabled := Value;
  if Assigned(FBody) then
  begin
    if FEnabled then
      dBodyEnable(FBody)
    else
      dBodyDisable(FBody);
  end;
end;

// GetEnabled
//
function TVKODEDynamic.GetEnabled: Boolean;
begin
  if Assigned(FBody) then
    FEnabled := (dBodyIsEnabled(FBody) = 1);
  Result := FEnabled;
end;

// AddForce
//
procedure TVKODEDynamic.AddForce(Force: TAffineVector);
begin
  if Assigned(FBody) then
    dBodyAddForce(FBody, Force.X, Force.Y, Force.Z);
end;

// AddlForceAtPos
//
procedure TVKODEDynamic.AddForceAtPos(Force, Pos: TAffineVector);
begin
  if Assigned(FBody) then
    dBodyAddForceAtPos(FBody, Force.X, Force.Y, Force.Z, Pos.X, Pos.Y, Pos.Z);
end;

// AddForceAtRelPos
//
procedure TVKODEDynamic.AddForceAtRelPos(Force, Pos: TAffineVector);
begin
  if Assigned(FBody) then
    dBodyAddForceAtRelPos(FBody, Force.X, Force.Y, Force.Z, Pos.X,
      Pos.Y, Pos.Z);
end;

// AddRelForce
//
procedure TVKODEDynamic.AddRelForce(Force: TAffineVector);
begin
  if Assigned(FBody) then
    dBodyAddRelForce(FBody, Force.X, Force.Y, Force.Z);
end;

// AddRelForceAtPos
//
procedure TVKODEDynamic.AddRelForceAtPos(Force, Pos: TAffineVector);
begin
  if Assigned(FBody) then
    dBodyAddForceAtPos(FBody, Force.X, Force.Y, Force.Z, Pos.X, Pos.Y, Pos.Z);
end;

// AddRelForceAtRelPos
//
procedure TVKODEDynamic.AddRelForceAtRelPos(Force, Pos: TAffineVector);
begin
  if Assigned(FBody) then
    dBodyAddRelForceAtRelPos(FBody, Force.X, Force.Y, Force.Z, Pos.X,
      Pos.Y, Pos.Z);
end;

// AddTorque
//
procedure TVKODEDynamic.AddTorque(Torque: TAffineVector);
begin
  if Assigned(FBody) then
    dBodyAddTorque(FBody, Torque.X, Torque.Y, Torque.Z);
end;

// AddRelTorque
//
procedure TVKODEDynamic.AddRelTorque(Torque: TAffineVector);
begin
  if Assigned(FBody) then
    dBodyAddRelTorque(FBody, Torque.X, Torque.Y, Torque.Z);
end;


// ---------------
// --------------- TVKODEStatic ---------------
// ---------------

// Create
//
constructor TVKODEStatic.Create(AOwner: TVKXCollection);
begin
  inherited;
  FElements := TODEElements.Create(Self);
end;

// Destroy
//
destructor TVKODEStatic.Destroy;
begin
  FElements.Free;
  inherited;
end;

// Render
//
procedure TVKODEStatic.Render(var rci: TVKRenderContextInfo);
var
  Mat: TMatrix;
begin
  if Assigned(Owner.Owner) then
  begin
    rci.PipelineTransformation.Push;
    Mat := TVKBaseSceneObject(Owner.Owner).AbsoluteMatrix;
    rci.PipelineTransformation.ModelMatrix := Mat;
  end;

  Elements.Render(rci);

  if Assigned(Owner.Owner) then
    rci.PipelineTransformation.Pop;
end;

// FriendlyName
//
class function TVKODEStatic.FriendlyName: String;
begin
  Result := 'ODE Static';
end;

// UniqueItem
//
class function TVKODEStatic.UniqueItem: Boolean;
begin
  Result := True;
end;

// Initialize
//
procedure TVKODEStatic.Initialize;
begin
  if (not Assigned(Manager)) or (FInitialized) then
    Exit;
  if not Assigned(Manager.Space) then
    Exit;

  FElements.Initialize;

  inherited;
end;

// Finalize
//
procedure TVKODEStatic.Finalize;
begin
  if not FInitialized then
    Exit;
  FElements.Finalize;

  inherited;
end;

// WriteToFiler
//
procedure TVKODEStatic.WriteToFiler(writer: TWriter);
begin
  inherited;
  with writer do
  begin
    WriteInteger(0); // Archive version
    FElements.WriteToFiler(writer);
  end;
end;

// ReadFromFiler
//
procedure TVKODEStatic.ReadFromFiler(reader: TReader);
begin
  inherited;
  with reader do
  begin
    Assert(ReadInteger = 0); // Archive version
    FElements.ReadFromFiler(reader);
  end;
end;

// AddNewElement
//
function TVKODEStatic.AddNewElement(AChild: TODEElementClass): TODEElementBase;
begin
  Result := nil;
  if not Assigned(Manager) then
    Exit;
  Result := AChild.Create(FElements);
  FElements.Add(Result);
  Result.Initialize;
end;

// AlignElements
//
procedure TVKODEStatic.AlignElements;
var
  i: Integer;
begin
  if not FInitialized then
    Exit;

  for i := 0 to FElements.Count - 1 do
    TODEElementBase(FElements[i]).AlignGeomElementToMatrix
      (TODEElementBase(FElements[i]).AbsoluteMatrix);
end;


// ---------------
// --------------- TODEElements ---------------
// ---------------

// Destroy
//
destructor TODEElements.Destroy;
begin
  Finalize;
  inherited;
end;

// GetElement
//
function TODEElements.GetElement(index: Integer): TODEElementBase;
begin
  Result := TODEElementBase(Items[index]);
end;

// ItemsClass
//
class function TODEElements.ItemsClass: TVKXCollectionItemClass;
begin
  Result := TODEElementBase;
end;

// Initialize
//
procedure TODEElements.Initialize;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    TODEElementBase(Items[i]).Initialize;
end;

// Deintialize
//
procedure TODEElements.Finalize;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    TODEElementBase(Items[i]).Finalize;
end;

// Render
//
procedure TODEElements.Render(var rci: TVKRenderContextInfo);
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    TODEElementBase(Items[i]).Render(rci);
end;

// NotifyChange
//
procedure TODEElements.NotifyChange(Sender: TObject);
begin
  if Assigned(Owner) then
    if Owner is TVKODEBehaviour then
      TVKODEBehaviour(Owner).NotifyChange(Self);
end;


// ---------------
// --------------- TODEElementBase ---------------
// ---------------

// Create
//
constructor TODEElementBase.Create(AOwner: TVKXCollection);
begin
  inherited;
  FPosition := TVKCoordinates.CreateInitialized(Self, NullHmgPoint, csPoint);
  FPosition.OnNotifyChange := NotifyChange;
  FDirection := TVKCoordinates.CreateInitialized(Self, ZHmgVector, csVector);
  FDirection.OnNotifyChange := CoordinateChanged;
  FUp := TVKCoordinates.CreateInitialized(Self, YHmgVector, csVector);
  FUp.OnNotifyChange := CoordinateChanged;
  FDensity := 1;
  FInitialized := False;
  FDynamic := (Owner.Owner is TVKODEDynamic);
  FLocalMatrix := IdentityHMGMatrix;
  FIsCalculating := False;
end;

// Destroy
//
destructor TODEElementBase.Destroy;
begin
  if FInitialized then
    Finalize;
  FPosition.Free;
  FDirection.Free;
  FUp.Free;
  inherited;
end;

// Render
//
procedure TODEElementBase.Render(var rci: TVKRenderContextInfo);
begin
  // Override this procedure with element drawing OpenGL code
end;

// Initialize
//
procedure TODEElementBase.Initialize;
var
  Manager: TVKODEManager;
  Body: PdxBody;
begin
  Manager := nil;
  Body := nil;

  if Owner.Owner is TVKODEBehaviour then
    Manager := TVKODEBehaviour(Owner.Owner).Manager;
  if not Assigned(Manager) then
    Exit;

  if FDynamic then
  begin
    if Owner.Owner is TVKODEDynamic then
      Body := TVKODEDynamic(Owner.Owner).Body;
    if not Assigned(Body) then
      Exit;
  end;

  if not Assigned(Manager.World) then
    Exit;

  if FDynamic then
  begin
    FGeomTransform := dCreateGeomTransform(Manager.Space);
    dGeomSetBody(FGeomTransform, Body);
    dGeomTransformSetCleanup(FGeomTransform, 0);
    dGeomTransformSetGeom(FGeomTransform, FGeomElement);
    dGeomSetData(FGeomTransform, Owner.Owner);
    AlignGeomElementToMatrix(FLocalMatrix);
  end
  else
  begin
    dSpaceAdd(Manager.Space, FGeomElement);
    dGeomSetData(FGeomElement, Owner.Owner);
    AlignGeomElementToMatrix(AbsoluteMatrix);
  end;

  FInitialized := True;
end;

// Finalize
//
procedure TODEElementBase.Finalize;
begin
  if not FInitialized then
    Exit;
  if Assigned(FGeomTransform) then
  begin
    dGeomDestroy(FGeomTransform);
    FGeomTransform := nil;
  end;
  if Assigned(FGeomElement) then
  begin
    dGeomDestroy(FGeomElement);
    FGeomElement := nil;
  end;
  FInitialized := False;
end;

// WriteToFiler
//
procedure TODEElementBase.WriteToFiler(writer: TWriter);
begin
  inherited;
  with writer do
  begin
    WriteInteger(0); // Archive version
    FPosition.WriteToFiler(writer);
    FDirection.WriteToFiler(writer);
    FUp.WriteToFiler(writer);
    WriteFloat(Density);
  end;
end;

// ReadFromFiler
//
procedure TODEElementBase.ReadFromFiler(reader: TReader);
begin
  inherited;
  with reader do
  begin
    Assert(ReadInteger = 0); // Archive version
    FPosition.ReadFromFiler(reader);
    FDirection.ReadFromFiler(reader);
    FUp.ReadFromFiler(reader);
    Density := ReadFloat;
  end;
  NotifyChange(Self);
end;

// AbsoluteMatrix
//
function TODEElementBase.AbsoluteMatrix: TMatrix;
var
  Mat: TMatrix;
begin
  Mat := IdentityHMGMatrix;
  if Owner.Owner is TVKODEBehaviour then
    Mat := TVKODEBehaviour(Owner.Owner).AbsoluteMatrix;
  Result := MatrixMultiply(Mat, FLocalMatrix);
end;

// AbsolutePosition
//
function TODEElementBase.AbsolutePosition: TAffineVector;
begin
  Result := AffineVectorMake(AbsoluteMatrix.W);
end;

// AlignGeomElementToMatrix
//
procedure TODEElementBase.AlignGeomElementToMatrix(Mat: TMatrix);
var
  R: TdMatrix3;
begin
  if not Assigned(FGeomElement) then
    Exit;
  dGeomSetPosition(FGeomElement, Mat.W.X, Mat.W.Y, Mat.W.Z);
  R[0] := Mat.X.X;
  R[1] := Mat.Y.X;
  R[2] := Mat.Z.X;
  R[3] := 0;
  R[4] := Mat.X.Y;
  R[5] := Mat.Y.Y;
  R[6] := Mat.Z.Y;
  R[7] := 0;
  R[8] := Mat.X.Z;
  R[9] := Mat.Y.Z;
  R[10] := Mat.Z.Z;
  R[11] := 0;
  dGeomSetRotation(FGeomElement, R);
  FRealignODE := False;
end;

// SetGeomElement
//
procedure TODEElementBase.SetGeomElement(aGeom: PdxGeom);
begin
  FGeomElement := aGeom;
end;

// IsODEInitialized
//
function TODEElementBase.IsODEInitialized: Boolean;
var
  Manager: TVKODEManager;
begin
  Result := False;
  Manager := nil;
  if Owner.Owner is TVKODEBehaviour then
    Manager := TVKODEBehaviour(Owner.Owner).Manager;
  if not Assigned(Manager) then
    Exit;
  Result := Assigned(Manager.Space);
end;

// CalculateMass
//
function TODEElementBase.CalculateMass: TdMass;
var
  R: TdMatrix3;
begin
  R[0] := FLocalMatrix.X.X;
  R[1] := FLocalMatrix.Y.X;
  R[2] := FLocalMatrix.Z.X;
  R[3] := 0;
  R[4] := FLocalMatrix.X.Y;
  R[5] := FLocalMatrix.Y.Y;
  R[6] := FLocalMatrix.Z.Y;
  R[7] := 0;
  R[8] := FLocalMatrix.X.Z;
  R[9] := FLocalMatrix.Y.Z;
  R[10] := FLocalMatrix.Z.Z;
  R[11] := 0;
  dMassRotate(FMass, R);
  dMassTranslate(FMass, FLocalMatrix.W.X, FLocalMatrix.W.Y, FLocalMatrix.W.Z);
  Result := FMass;
end;

// CoordinateChanged
//
procedure TODEElementBase.CoordinateChanged(Sender: TObject);
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
      rightVector := VectorCrossProduct(FDirection.AsVector, FUp.AsVector);
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
      rightVector := VectorCrossProduct(FDirection.AsVector, FUp.AsVector);
      if VectorLength(rightVector) < 1E-5 then
      begin
        rightVector := VectorCrossProduct(ZHmgVector, FUp.AsVector);
        if VectorLength(rightVector) < 1E-5 then
          rightVector := VectorCrossProduct(XHmgVector, FUp.AsVector);
      end;
      FDirection.DirectVector := VectorCrossProduct(FUp.AsVector, rightVector);
      FDirection.Normalize;
    end;
    NotifyChange(Self);
  finally
    FIsCalculating := False;
  end;
end;

// NotifyChange
//
procedure TODEElementBase.NotifyChange(Sender: TObject);
begin
  RebuildMatrix;
  ODERebuild;
end;

// GetMatrix
//
function TODEElementBase.GetMatrix: TMatrix;
begin
  Result := FLocalMatrix;
end;

// RebuildMatrix
//
procedure TODEElementBase.RebuildMatrix;
begin
  VectorCrossProduct(FUp.AsVector, FDirection.AsVector, FLocalMatrix.X);
  SetVector(FLocalMatrix.Y, FUp.AsVector);
  SetVector(FLocalMatrix.Z, FDirection.AsVector);
  SetVector(FLocalMatrix.W, FPosition.AsVector);
end;

// RebuildVectors
//
procedure TODEElementBase.RebuildVectors;
begin
  FUp.SetVector(FLocalMatrix.Y.X, FLocalMatrix.Y.Y, FLocalMatrix.Y.Z);
  FDirection.SetVector(FLocalMatrix.Z.X, FLocalMatrix.Z.Y, FLocalMatrix.Z.Z);
  FPosition.SetPoint(FLocalMatrix.W.X, FLocalMatrix.W.Y, FLocalMatrix.W.Z);
end;

// SetDensity
//
procedure TODEElementBase.SetDensity(const Value: TdReal);
begin
  FDensity := Value;
end;

// SetMatrix
//
procedure TODEElementBase.SetMatrix(const Value: TMatrix);
begin
  FLocalMatrix := Value;
  RebuildVectors;
  ODERebuild;
end;

// ODERebuild
//
procedure TODEElementBase.ODERebuild;
begin
  if Initialized then
  begin
    if FDynamic then
    begin
      CalculateMass;
      AlignGeomElementToMatrix(FLocalMatrix);
    end
    else
      AlignGeomElementToMatrix(AbsoluteMatrix);
  end;
  if Assigned(Owner) then
    TODEElements(Owner).NotifyChange(Self);
end;

// SetPosition
//
procedure TODEElementBase.SetPosition(const Value: TVKCoordinates);
begin
  FPosition.Assign(Value);
end;

// SetDirection
//
procedure TODEElementBase.SetDirection(const Value: TVKCoordinates);
begin
  FDirection.Assign(Value);
end;

// SetUp
//
procedure TODEElementBase.SetUp(const Value: TVKCoordinates);
begin
  FUp.Assign(Value);
end;


// ---------------
// --------------- TODEElementBox ---------------
// ---------------

// BuildList
//
procedure TODEElementBox.Render(var rci: TVKRenderContextInfo);
begin
  glPushMatrix;

  glMultMatrixf(@FLocalMatrix);

  glBegin(GL_LINE_LOOP);
  glVertex3f(-FBoxWidth / 2, -FBoxHeight / 2, -FBoxDepth / 2);
  glVertex3f(-FBoxWidth / 2, FBoxHeight / 2, -FBoxDepth / 2);
  glVertex3f(-FBoxWidth / 2, FBoxHeight / 2, FBoxDepth / 2);
  glVertex3f(-FBoxWidth / 2, -FBoxHeight / 2, FBoxDepth / 2);
  glEnd;

  glBegin(GL_LINE_LOOP);
  glVertex3f(FBoxWidth / 2, FBoxHeight / 2, FBoxDepth / 2);
  glVertex3f(FBoxWidth / 2, -FBoxHeight / 2, FBoxDepth / 2);
  glVertex3f(FBoxWidth / 2, -FBoxHeight / 2, -FBoxDepth / 2);
  glVertex3f(FBoxWidth / 2, FBoxHeight / 2, -FBoxDepth / 2);
  glEnd;

  glBegin(GL_LINES);
  glVertex3f(-FBoxWidth / 2, FBoxHeight / 2, -FBoxDepth / 2);
  glVertex3f(FBoxWidth / 2, FBoxHeight / 2, -FBoxDepth / 2);
  glVertex3f(-FBoxWidth / 2, -FBoxHeight / 2, FBoxDepth / 2);
  glVertex3f(FBoxWidth / 2, -FBoxHeight / 2, FBoxDepth / 2);
  glVertex3f(-FBoxWidth / 2, -FBoxHeight / 2, -FBoxDepth / 2);
  glVertex3f(FBoxWidth / 2, -FBoxHeight / 2, -FBoxDepth / 2);
  glVertex3f(-FBoxWidth / 2, FBoxHeight / 2, FBoxDepth / 2);
  glVertex3f(FBoxWidth / 2, FBoxHeight / 2, FBoxDepth / 2);
  glEnd;

  glPopMatrix;
end;

// Create
//
constructor TODEElementBox.Create(AOwner: TVKXCollection);
begin
  inherited;
  BoxWidth := 1;
  BoxHeight := 1;
  BoxDepth := 1;
end;

// Initialize
//
procedure TODEElementBox.Initialize;
begin
  if FInitialized then
    Exit;
  if not IsODEInitialized then
    Exit;

  FGeomElement := dCreateBox(nil, FBoxWidth, FBoxHeight, FBoxDepth);
  inherited;
end;

// WriteToFiler
//
procedure TODEElementBox.WriteToFiler(writer: TWriter);
begin
  inherited;
  with writer do
  begin
    WriteInteger(0); // Archive version
    WriteFloat(BoxWidth);
    WriteFloat(BoxHeight);
    WriteFloat(BoxDepth);
  end;
end;

// ReadFromFiler
//
procedure TODEElementBox.ReadFromFiler(reader: TReader);
begin
  inherited;
  with reader do
  begin
    Assert(ReadInteger = 0); // Archive version
    BoxWidth := ReadFloat;
    BoxHeight := ReadFloat;
    BoxDepth := ReadFloat;
  end;
end;

// FriendlyName
//
class function TODEElementBox.FriendlyName: String;
begin
  Result := 'Box';
end;

// FriendlyDescription
//
class function TODEElementBox.FriendlyDescription: String;
begin
  Result := 'The ODE box element implementation';
end;

// ItemCategory
//
class function TODEElementBox.ItemCategory: String;
begin
  Result := 'Primitives';
end;

// CalculateMass
//
function TODEElementBox.CalculateMass: TdMass;
begin
  dMassSetBox(FMass, FDensity, BoxWidth, BoxHeight, BoxDepth);
  Result := inherited CalculateMass;
end;

// GetBoxWidth
//
function TODEElementBox.GetBoxWidth: TdReal;
var
  vec: TdVector3;
begin
  if Assigned(FGeomTransform) then
  begin
    dGeomBoxGetLengths(Geom, vec);
    FBoxWidth := vec[0];
  end;
  Result := FBoxWidth;
end;

// GetBoxHeight
//
function TODEElementBox.GetBoxHeight: TdReal;
var
  vec: TdVector3;
begin
  if Assigned(FGeomTransform) then
  begin
    dGeomBoxGetLengths(Geom, vec);
    FBoxHeight := vec[1];
  end;
  Result := FBoxHeight;
end;

// GetBoxDepth
//
function TODEElementBox.GetBoxDepth: TdReal;
var
  vec: TdVector3;
begin
  if Assigned(FGeomTransform) then
  begin
    dGeomBoxGetLengths(Geom, vec);
    FBoxDepth := vec[2];
  end;
  Result := FBoxDepth;
end;

// ODERebuild
//
procedure TODEElementBox.ODERebuild;
begin
  if Assigned(Geom) then
    dGeomBoxSetLengths(Geom, FBoxWidth, FBoxHeight, FBoxDepth);
  inherited;
end;

// SetBoxWidth
//
procedure TODEElementBox.SetBoxWidth(const Value: TdReal);
begin
  FBoxWidth := Value;
  ODERebuild;
end;

// SetBoxHeight
//
procedure TODEElementBox.SetBoxHeight(const Value: TdReal);
begin
  FBoxHeight := Value;
  ODERebuild;
end;

// SetBoxDepth
//
procedure TODEElementBox.SetBoxDepth(const Value: TdReal);
begin
  FBoxDepth := Value;
  ODERebuild;
end;


// ---------------
// --------------- TODEElementSphere ---------------
// ---------------

// Render
//
procedure TODEElementSphere.Render(var rci: TVKRenderContextInfo);
var
  AngTop, AngBottom, AngStart, AngStop, StepV, StepH: double;
  SinP, CosP, SinP2, CosP2, SinT, CosT, Phi, Phi2, Theta: double;
  FTop, FBottom, FStart, FStop: Single;
  i, J, FSlices, FStacks: Integer;
begin
  glPushMatrix;

  glMultMatrixf(@FLocalMatrix);
  glScalef(Radius, Radius, Radius);

  FTop := 90;
  FBottom := -90;
  FStart := 0;
  FStop := 360;
  FSlices := 16;
  FStacks := 16;

  AngTop := DegToRadian(FTop);
  AngBottom := DegToRadian(FBottom);
  AngStart := DegToRadian(FStart);
  AngStop := DegToRadian(FStop);
  StepH := (AngStop - AngStart) / FSlices;
  StepV := (AngTop - AngBottom) / FStacks;

  Phi := AngTop;
  Phi2 := Phi - StepV;
  for J := 0 to FStacks - 1 do
  begin
    Theta := AngStart;
    SinCosine(Phi, SinP, CosP);
    SinCosine(Phi2, SinP2, CosP2);

    glBegin(GL_LINE_LOOP);
    for i := 0 to FSlices do
    begin
      SinCosine(Theta, SinT, CosT);
      glVertex3f(CosP * SinT, SinP, CosP * CosT);
      Theta := Theta + StepH;
    end;
    glEnd;
    Phi := Phi2;
    Phi2 := Phi2 - StepV;
  end;

  Phi := AngTop;
  Phi2 := Phi - StepV;
  for J := 0 to FStacks - 1 do
  begin
    Theta := AngStart;
    SinCosine(Phi, SinP, CosP);
    SinCosine(Phi2, SinP2, CosP2);

    glBegin(GL_LINE_LOOP);
    for i := 0 to FSlices do
    begin
      SinCosine(Theta, SinT, CosT);
      glVertex3f(SinP, CosP * SinT, CosP * CosT);
      Theta := Theta + StepH;
    end;
    glEnd;
    Phi := Phi2;
    Phi2 := Phi2 - StepV;
  end;

  glPopMatrix;
end;

// Create
//
constructor TODEElementSphere.Create(AOwner: TVKXCollection);
begin
  inherited;
  FRadius := 0.5;
end;

// Initialize
//
procedure TODEElementSphere.Initialize;
begin
  if FInitialized then
    Exit;
  if not IsODEInitialized then
    Exit;

  FGeomElement := dCreateSphere(nil, FRadius);
  inherited;
end;

// WriteToFiler
//
procedure TODEElementSphere.WriteToFiler(writer: TWriter);
begin
  inherited;
  with writer do
  begin
    WriteInteger(0); // Archive version
    WriteFloat(Radius);
  end;
end;

// ReadFromFiler
//
procedure TODEElementSphere.ReadFromFiler(reader: TReader);
begin
  inherited;
  with reader do
  begin
    Assert(ReadInteger = 0); // Archive version
    Radius := ReadFloat;
  end;
end;

// FriendlyName
//
class function TODEElementSphere.FriendlyName: String;
begin
  Result := 'Sphere';
end;

// FriendlyDescription
//
class function TODEElementSphere.FriendlyDescription: String;
begin
  Result := 'The ODE sphere element implementation';
end;

// ItemCategory
//
class function TODEElementSphere.ItemCategory: String;
begin
  Result := 'Primitives';
end;

// CalculateMass
//
function TODEElementSphere.CalculateMass: TdMass;
begin
  dMassSetSphere(FMass, FDensity, Radius);
  Result := inherited CalculateMass;
end;

// GetRadius
//
function TODEElementSphere.GetRadius: TdReal;
begin
  if Assigned(FGeomElement) then
    FRadius := dGeomSphereGetRadius(FGeomElement);
  Result := FRadius;
end;

// ODERebuild
//
procedure TODEElementSphere.ODERebuild;
begin
  if Assigned(Geom) then
  begin
    dGeomSphereSetRadius(Geom, FRadius);
  end;
  inherited;
end;

// SetRadius
//
procedure TODEElementSphere.SetRadius(const Value: TdReal);
begin
  FRadius := Value;
  ODERebuild;
end;


// ---------------
// --------------- TODEElementCapsule ---------------
// ---------------

// Render
//
procedure TODEElementCapsule.Render(var rci: TVKRenderContextInfo);
var
  i, J, Stacks, Slices: Integer;
begin
  glPushMatrix;

  glMultMatrixf(@FLocalMatrix);

  Stacks := 8;
  Slices := 16;

  // Middle horizontal circles
  for J := 0 to Stacks - 1 do
  begin
    glBegin(GL_LINE_LOOP);
    for i := 0 to Slices - 1 do
      glVertex3f(FRadius * sin(2 * i * PI / Slices),
        FRadius * cos(2 * i * PI / Slices), -FLength / 2 + FLength * J /
        (Stacks - 1));
    glEnd;
  end;

  // Middle vertical lines
  glBegin(GL_LINES);
  for i := 0 to (Slices div 2) - 1 do
  begin
    glVertex3f(FRadius * sin(2 * i * PI / Slices),
      FRadius * cos(2 * i * PI / Slices), -FLength / 2);
    glVertex3f(FRadius * sin(2 * i * PI / Slices),
      FRadius * cos(2 * i * PI / Slices), FLength / 2);
    glVertex3f(-FRadius * sin(2 * i * PI / Slices),
      -FRadius * cos(2 * i * PI / Slices), -FLength / 2);
    glVertex3f(-FRadius * sin(2 * i * PI / Slices),
      -FRadius * cos(2 * i * PI / Slices), FLength / 2);
  end;
  glEnd;

  // Cap XZ half-circles
  glPushMatrix;
  for J := 0 to (Slices div 2) - 1 do
  begin
    // Top
    glBegin(GL_LINE_STRIP);
    for i := 0 to Slices do
      glVertex3f(FRadius * cos(i * PI / Slices), 0,
        FRadius * sin(i * PI / Slices) + FLength / 2);
    glEnd;

    // Bottom
    glBegin(GL_LINE_STRIP);
    for i := 0 to Slices do
      glVertex3f(FRadius * cos(i * PI / Slices), 0,
        -(FRadius * sin(i * PI / Slices) + FLength / 2));
    glEnd;
    glRotatef(360 / Slices, 0, 0, 1);
  end;
  glPopMatrix;
  glPopMatrix;
end;

// Create
//
constructor TODEElementCapsule.Create(AOwner: TVKXCollection);
begin
  inherited;
  FRadius := 0.5;
  FLength := 1;
end;

// Initialize
//
procedure TODEElementCapsule.Initialize;
begin
  if FInitialized then
    Exit;
  if not IsODEInitialized then
    Exit;

  FGeomElement := dCreateCapsule(nil, FRadius, FLength);
  inherited;
end;

// WriteToFiler
//
procedure TODEElementCapsule.WriteToFiler(writer: TWriter);
begin
  inherited;
  with writer do
  begin
    WriteInteger(0); // Archive version
    WriteFloat(Radius);
    WriteFloat(Length);
  end;
end;

// ReadFromFiler
//
procedure TODEElementCapsule.ReadFromFiler(reader: TReader);
begin
  inherited;
  with reader do
  begin
    Assert(ReadInteger = 0); // Archive version
    Radius := ReadFloat;
    Length := ReadFloat;
  end;
end;

// FriendlyName
//
class function TODEElementCapsule.FriendlyName: String;
begin
  Result := 'Capsule';
end;

// FriendlyDescription
//
class function TODEElementCapsule.FriendlyDescription: String;
begin
  Result := 'The ODE capped cylinder element implementation';
end;

// ItemCategory
//
class function TODEElementCapsule.ItemCategory: String;
begin
  Result := 'Primitives';
end;

// CalculateMass
//
function TODEElementCapsule.CalculateMass: TdMass;
begin
  dMassSetCapsule(FMass, FDensity, 3, FRadius, FLength);
  Result := inherited CalculateMass;
end;

// GetRadius
//
function TODEElementCapsule.GetRadius: TdReal;
var
  rad, len: TdReal;
begin
  if Assigned(FGeomElement) then
  begin
    dGeomCapsuleGetParams(Geom, rad, len);
    FRadius := rad;
  end;
  Result := FRadius;
end;

// GetLength
//
function TODEElementCapsule.GetLength: TdReal;
var
  rad, len: TdReal;
begin
  if Assigned(FGeomElement) then
  begin
    dGeomCapsuleGetParams(Geom, rad, len);
    FLength := len;
  end;
  Result := FLength;
end;

// ODERebuild
//
procedure TODEElementCapsule.ODERebuild;
begin
  if Assigned(Geom) then
    dGeomCapsuleSetParams(Geom, FRadius, FLength);
  inherited;
end;

// SetRadius
//
procedure TODEElementCapsule.SetRadius(const Value: TdReal);
begin
  FRadius := Value;
  ODERebuild;
end;

// SetLength
//
procedure TODEElementCapsule.SetLength(const Value: TdReal);
begin
  FLength := Value;
  ODERebuild;
end;


// ---------------
// --------------- TODEElementCylinder ---------------
// ---------------

// Render
//
procedure TODEElementCylinder.Render(var rci: TVKRenderContextInfo);
var
  i, J, Stacks, Slices: Integer;
begin
  glPushMatrix;

  glMultMatrixf(@FLocalMatrix);

  Stacks := 8;
  Slices := 16;

  // Middle horizontal circles
  for J := 0 to Stacks - 1 do
  begin
    glBegin(GL_LINE_LOOP);
    for i := 0 to Slices - 1 do
      glVertex3f(FRadius * sin(2 * i * PI / Slices), -FLength / 2 + FLength * J
        / (Stacks - 1), FRadius * cos(2 * i * PI / Slices));
    glEnd;
  end;

  // Middle vertical lines
  glBegin(GL_LINES);
  for i := 0 to (Slices div 2) - 1 do
  begin
    glVertex3f(FRadius * sin(2 * i * PI / Slices), -FLength / 2,
      FRadius * cos(2 * i * PI / Slices));
    glVertex3f(FRadius * sin(2 * i * PI / Slices), FLength / 2,
      FRadius * cos(2 * i * PI / Slices));
    glVertex3f(-FRadius * sin(2 * i * PI / Slices), -FLength / 2,
      -FRadius * cos(2 * i * PI / Slices));
    glVertex3f(-FRadius * sin(2 * i * PI / Slices), FLength / 2,
      -FRadius * cos(2 * i * PI / Slices));
  end;
  glEnd;

  // Caps
  glPushMatrix;
  for J := 0 to (Slices div 2) - 1 do
  begin
    glBegin(GL_LINES);
    glVertex3f(-FRadius, FLength / 2, 0);
    glVertex3f(FRadius, FLength / 2, 0);
    glVertex3f(-FRadius, -FLength / 2, 0);
    glVertex3f(FRadius, -FLength / 2, 0);
    glEnd;
    glRotatef(360 / Slices, 0, 1, 0);
  end;
  glPopMatrix;

  glPopMatrix;
end;

// Create
//
constructor TODEElementCylinder.Create(AOwner: TVKXCollection);
begin
  inherited;
  FRadius := 0.5;
  FLength := 1;
end;

// Initialize
//
procedure TODEElementCylinder.Initialize;
begin
  if FInitialized then
    Exit;
  if not IsODEInitialized then
    Exit;

  FGeomElement := dCreateCylinder(nil, FRadius, FLength);
  inherited;
end;

// WriteToFiler
//
procedure TODEElementCylinder.WriteToFiler(writer: TWriter);
begin
  inherited;
  with writer do
  begin
    WriteInteger(0); // Archive version
    WriteFloat(Radius);
    WriteFloat(Length);
  end;
end;

// ReadFromFiler
//
procedure TODEElementCylinder.ReadFromFiler(reader: TReader);
begin
  inherited;
  with reader do
  begin
    Assert(ReadInteger = 0); // Archive version
    Radius := ReadFloat;
    Length := ReadFloat;
  end;
end;

// FriendlyName
//
class function TODEElementCylinder.FriendlyName: String;
begin
  Result := 'Cylinder';
end;

// FriendlyDescription
//
class function TODEElementCylinder.FriendlyDescription: String;
begin
  Result := 'The ODE cylinder element implementation';
end;

// ItemCategory
//
class function TODEElementCylinder.ItemCategory: String;
begin
  Result := 'Primitives';
end;

// CalculateMass
//
function TODEElementCylinder.CalculateMass: TdMass;
begin
  dMassSetCylinder(FMass, FDensity, 3, FRadius, FLength);
  Result := inherited CalculateMass;
end;

// GetRadius
//
function TODEElementCylinder.GetRadius: TdReal;
var
  rad, len: TdReal;
begin
  if Assigned(FGeomElement) then
  begin
    dGeomCylinderGetParams(Geom, rad, len);
    FRadius := rad;
  end;
  Result := FRadius;
end;

// GetLength
//
function TODEElementCylinder.GetLength: TdReal;
var
  rad, len: TdReal;
begin
  if Assigned(FGeomElement) then
  begin
    dGeomCylinderGetParams(Geom, rad, len);
    FLength := len;
  end;
  Result := FLength;
end;

// ODERebuild
//
procedure TODEElementCylinder.ODERebuild;
begin
  if Assigned(Geom) then
    dGeomCylinderSetParams(Geom, FRadius, FLength);
  inherited;
end;

// SetRadius
//
procedure TODEElementCylinder.SetRadius(const Value: TdReal);
begin
  FRadius := Value;
  ODERebuild;
end;

// SetLength
//
procedure TODEElementCylinder.SetLength(const Value: TdReal);
begin
  FLength := Value;
  ODERebuild;
end;



// ---------------
// --------------- TODEElementTriMesh ---------------
// ---------------

// Create
//
constructor TODEElementTriMesh.Create(AOwner: TVKXCollection);
begin
  inherited;
  FVertices := TAffineVectorList.Create;
  FIndices := TIntegerList.Create;
end;

// Destroy
//
destructor TODEElementTriMesh.Destroy;
begin
  FVertices.Free;
  FIndices.Free;
  inherited;
end;

// Initialize
//
procedure TODEElementTriMesh.Initialize;
begin
  if not IsODEInitialized then
    Exit;
  if FInitialized or not((FVertices.Count > 0) and (FIndices.Count > 0)) then
    Exit;

  FTriMeshData := dGeomTriMeshDataCreate;
  dGeomTriMeshDataBuildSingle(FTriMeshData, @FVertices.List[0],
    3 * SizeOf(Single), FVertices.Count, @FIndices.List[0], FIndices.Count,
    3 * SizeOf(Integer));
  FGeomElement := dCreateTriMesh(nil, FTriMeshData, nil, nil, nil);

  inherited;
end;

// Finalize
//
procedure TODEElementTriMesh.Finalize;
begin
  if not FInitialized then
    Exit;
  if Assigned(FTriMeshData) then
    dGeomTriMeshDataDestroy(FTriMeshData);
  inherited;
end;

// WriteToFiler
//
procedure TODEElementTriMesh.WriteToFiler(writer: TWriter);
begin
  inherited;
  with writer do
  begin
    WriteInteger(0); // Archive version
  end;
end;

// ReadFromFiler
//
procedure TODEElementTriMesh.ReadFromFiler(reader: TReader);
begin
  inherited;
  with reader do
  begin
    Assert(ReadInteger = 0); // Archive version
  end;
end;

// FriendlyName
//
class function TODEElementTriMesh.FriendlyName: String;
begin
  Result := 'Tri-Mesh';
end;

// FriendlyDescription
//
class function TODEElementTriMesh.FriendlyDescription: String;
begin
  Result := 'The ODE tri-mesh element implementation';
end;

// ItemCategory
//
class function TODEElementTriMesh.ItemCategory: String;
begin
  Result := 'Meshes';
end;

// CalculateMass
//
function TODEElementTriMesh.CalculateMass: TdMass;
var
  R: Single;
  min, max: TAffineVector;
begin
  if Vertices.Count > 0 then
  begin
    Vertices.GetExtents(min, max);
    R := MaxFloat(VectorLength(min), VectorLength(max));
  end
  else
    R := 1;
  dMassSetSphere(FMass, FDensity, R);
  Result := inherited CalculateMass;
end;

// SetVertices
//
procedure TODEElementTriMesh.SetVertices(const Value: TAffineVectorList);
begin
  FVertices.Assign(Value);
  RefreshTriMeshData;
end;

// SetIndices
//
procedure TODEElementTriMesh.SetIndices(const Value: TIntegerList);
begin
  FIndices.Assign(Value);
  RefreshTriMeshData;
end;

// RefreshTriMeshData
//
procedure TODEElementTriMesh.RefreshTriMeshData;
begin
  if FInitialized then
    Finalize;
  Initialize;
end;


// ---------------
// --------------- TODEElementPlane ---------------
// ---------------

// Initialize
//
procedure TODEElementPlane.Initialize;
begin
  if FInitialized then
    Exit;
  if not IsODEInitialized then
    Exit;

  FGeomElement := dCreatePlane(nil, 0, 0, 1, 0);
  inherited;
end;

// WriteToFiler
//
procedure TODEElementPlane.WriteToFiler(writer: TWriter);
begin
  // ArchiveVersion 1, added inherited call
  writer.WriteInteger(1);
  inherited;
end;

// ReadFromFiler
//
procedure TODEElementPlane.ReadFromFiler(reader: TReader);
var
  archiveVersion: Integer;
begin
  archiveVersion := reader.ReadInteger;
  Assert(archiveVersion in [0 .. 1]);
  if archiveVersion >= 1 then
    inherited;
end;

// FriendlyName
//
class function TODEElementPlane.FriendlyName: String;
begin
  Result := 'Plane';
end;

// FriendlyDescription
//
class function TODEElementPlane.FriendlyDescription: String;
begin
  Result := 'The ODE plane element implementation';
end;

// ItemCategory
//
class function TODEElementPlane.ItemCategory: String;
begin
  Result := 'Primitives';
end;

// CanAddTo
//
class function TODEElementPlane.CanAddTo(collection: TVKXCollection): Boolean;
begin
  Result := False;
  if Assigned(TODEElements(collection).Owner) then
    if TODEElements(collection).Owner is TVKODEStatic then
      Result := True;
end;

// AlignGeomElementToMatrix
//
procedure TODEElementPlane.AlignGeomElementToMatrix(Mat: TMatrix);
var
  d: Single;
begin
  if not Assigned(FGeomElement) then
    Exit;
  d := VectorDotProduct(Mat.Z, Mat.W);
  dGeomPlaneSetParams(FGeomElement, Mat.Z.X, Mat.Z.Y, Mat.Z.Z, d);
end;


// ---------------
// --------------- TODEJoints ---------------
// ---------------

// ItemsClass
//
class function TODEJoints.ItemsClass: TVKXCollectionItemClass;
begin
  Result := TODEJointBase;
end;

// Initialize
//
procedure TODEJoints.Initialize;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    Joint[i].Initialize;
end;

// Finalize
//
procedure TODEJoints.Finalize;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    Joint[i].Finalize;
end;

// GetJoint
//
function TODEJoints.GetJoint(index: Integer): TODEJointBase;
begin
  Result := TODEJointBase(Items[index]);
end;


// ---------------
// --------------- TVKODEJointList ---------------
// ---------------

// Create
//
constructor TVKODEJointList.Create(AOwner: TComponent);
begin
  inherited;
  FJoints := TODEJoints.Create(Self);
end;

// Destroy
//
destructor TVKODEJointList.Destroy;
begin
  FJoints.Free;
  inherited;
end;

// DefineProperties
//
procedure TVKODEJointList.DefineProperties(Filer: TFiler);
begin
  inherited;
  Filer.DefineBinaryProperty('ODEJointsData', ReadJoints, WriteJoints,
    (Assigned(FJoints) and (FJoints.Count > 0)));
end;

// WriteJoints
//
procedure TVKODEJointList.WriteJoints(stream: TStream);
var
  writer: TWriter;
begin
  writer := TWriter.Create(stream, 16384);
  try
    Joints.WriteToFiler(writer);
  finally
    writer.Free;
  end;
end;

// ReadJoints
//
procedure TVKODEJointList.ReadJoints(stream: TStream);
var
  reader: TReader;
begin
  reader := TReader.Create(stream, 16384);
  try
    Joints.ReadFromFiler(reader);
  finally
    reader.Free;
  end;
end;

// Loaded
//
procedure TVKODEJointList.Loaded;
var
  i: Integer;
begin
  inherited;
  for i := 0 to FJoints.Count - 1 do
    FJoints[i].Loaded;
end;

// Notification
//
procedure TVKODEJointList.Notification(AComponent: TComponent;
  Operation: TOperation);
var
  i: Integer;
begin
  inherited;
  if (Operation = opRemove) and (AComponent is TVKBaseSceneObject) then
    for i := 0 to Joints.Count - 1 do
    begin
      if TVKBaseSceneObject(AComponent) = Joints[i].Object1 then
        Joints[i].Object1 := nil;
      if TVKBaseSceneObject(AComponent) = Joints[i].Object2 then
        Joints[i].Object2 := nil;
    end;
end;


// ---------------
// --------------- TODEJointBase ---------------
// ---------------

// Create
//
constructor TODEJointBase.Create(AOwner: TVKXCollection);
begin
  inherited;
  FJointID := nil;
  FEnabled := True;
  FInitialized := False;
end;

// Destroy
destructor TODEJointBase.Destroy;
begin
  Finalize;
  inherited;
end;

// Initialize
//
procedure TODEJointBase.Initialize;
begin
  if not IsODEInitialized then
    Exit;

  if Assigned(FObject1) then
    RegisterJointWithObject(FObject1);
  if Assigned(FObject2) then
    RegisterJointWithObject(FObject2);
  Attach;

  FInitialized := True;
end;

// Finalize
//
procedure TODEJointBase.Finalize;
begin
  if not Initialized then
    Exit;

  if Assigned(FObject1) then
    UnregisterJointWithObject(FObject1);
  if Assigned(FObject2) then
    UnregisterJointWithObject(FObject2);
  if FJointID <> nil then
    dJointDestroy(FJointID);

  FInitialized := False;
end;

// WriteToFiler
//
procedure TODEJointBase.WriteToFiler(writer: TWriter);
begin
  inherited;
  with writer do
  begin
    WriteInteger(0); // Archive version
    if Assigned(FManager) then
      WriteString(FManager.GetNamePath)
    else
      WriteString('');
    if Assigned(FObject1) then
      WriteString(FObject1.GetNamePath)
    else
      WriteString('');
    if Assigned(FObject2) then
      WriteString(FObject2.GetNamePath)
    else
      WriteString('');
    WriteBoolean(FEnabled);
  end;
end;

// ReadFromFiler
//
procedure TODEJointBase.ReadFromFiler(reader: TReader);
begin
  inherited;
  with reader do
  begin
    Assert(ReadInteger = 0); // Archive version
    FManagerName := ReadString;
    FObject1Name := ReadString;
    FObject2Name := ReadString;
    FEnabled := ReadBoolean;
  end;
end;

// Loaded
//
procedure TODEJointBase.Loaded;
begin
  DoLoaded;
end;

// RegisterJointWithObject
//
procedure TODEJointBase.RegisterJointWithObject(Obj: TVKBaseSceneObject);
var
  temp: TVKODEDynamic;
begin
  if Assigned(Obj) then
  begin
    temp := TVKODEDynamic(Obj.Behaviours.GetByClass(TVKODEDynamic));
    if Assigned(temp) then
      temp.RegisterJoint(Self);
  end;
end;

// UnregisterJointWithObject
//
procedure TODEJointBase.UnregisterJointWithObject(Obj: TVKBaseSceneObject);
var
  temp: TVKODEDynamic;
begin
  if Assigned(Obj) then
  begin
    temp := TVKODEDynamic(Obj.Behaviours.GetByClass(TVKODEDynamic));
    if Assigned(temp) then
      temp.UnregisterJoint(Self);
  end;
end;

// IsODEInitialized
//
function TODEJointBase.IsODEInitialized: Boolean;
begin
  Result := False;
  if not Assigned(Manager) then
    Exit;
  Result := Assigned(Manager.World);
end;

// Attach
//
procedure TODEJointBase.Attach;
var
  Body1, Body2: PdxBody;
begin
  if (FJointID = nil) or not FInitialized then
    Exit;

  if Enabled then
  begin
    Body1 := GetBodyFromVKSceneObject(FObject1);
    Body2 := GetBodyFromVKSceneObject(FObject2);
  end
  else
  begin
    Body1 := nil;
    Body2 := nil;
  end;

  if (joBothObjectsMustBeAssigned in JointOptions) then
    if not(Assigned(Body1) and Assigned(Body2)) then
      Exit;

  dJointAttach(FJointID, Body1, Body2);
  if Assigned(Body1) or Assigned(Body2) then
    StructureChanged;
end;

// SetManager
//
procedure TODEJointBase.SetManager(const Value: TVKODEManager);
begin
  if FManager <> Value then
  begin
    if Assigned(FManager) then
      if not(csDesigning in FManager.ComponentState) then
        Finalize;
    FManager := Value;
    if Assigned(FManager) then
      if not(csDesigning in FManager.ComponentState) then
        Initialize;
  end;
end;

// SetObject1
//
procedure TODEJointBase.SetObject1(const Value: TVKBaseSceneObject);
begin
  if FObject1 <> Value then
  begin
    if Assigned(FObject1) then
      UnregisterJointWithObject(FObject1);
    FObject1 := Value;
    if Assigned(FObject1) then
      if IsGLODEObject(FObject1) then
        RegisterJointWithObject(FObject1)
      else
        FObject1 := nil;
    Attach;
  end;
end;

// SetObject2
//
procedure TODEJointBase.SetObject2(const Value: TVKBaseSceneObject);
begin
  if FObject2 <> Value then
  begin
    if Assigned(FObject2) then
      UnregisterJointWithObject(FObject2);
    FObject2 := Value;
    if Assigned(FObject2) then
      if IsGLODEObject(FObject2) then
        RegisterJointWithObject(FObject2)
      else
        FObject2 := nil;
    Attach;
  end;
end;

// SetEnabled
//
procedure TODEJointBase.SetEnabled(const Value: Boolean);
begin
  if FEnabled <> Value then
  begin
    FEnabled := Value;
    if IsODEInitialized then
      Attach;
  end;
end;

// StructureChanged
//
procedure TODEJointBase.StructureChanged;
begin
  // nothing yet
end;

// DoLoaded (public proc for Loaded)
//
procedure TODEJointBase.DoLoaded;
var
  mng: TComponent;
  Obj: TVKBaseSceneObject;
begin
  inherited;
  if FManagerName <> '' then
  begin
    mng := FindManager(TVKODEManager, FManagerName);
    if Assigned(mng) then
      Manager := TVKODEManager(mng);
    FManagerName := '';
  end;
  if FObject1Name <> '' then
  begin
    Obj := GetVKSceneObject(FObject1Name);
    if Assigned(Obj) then
      Object1 := Obj;
    FObject1Name := '';
  end;
  if FObject2Name <> '' then
  begin
    Obj := GetVKSceneObject(FObject2Name);
    if Assigned(Obj) then
      Object2 := Obj;
    FObject2Name := '';
  end;
  Attach;
end;

// IsAttached
//
function TODEJointBase.IsAttached: Boolean;
var
  Body1, Body2: PdxBody;
begin
  Result := False;
  if JointID <> nil then
  begin
    Body1 := dJointGetBody(JointID, 0);
    Body2 := dJointGetBody(JointID, 1);
    if joBothObjectsMustBeAssigned in JointOptions then
      Result := Assigned(Body1) and Assigned(Body2)
    else
      Result := Assigned(Body1) or Assigned(Body2);
  end;
end;

// SetJointOptions
//
procedure TODEJointBase.SetJointOptions(const Value: TJointOptions);
begin
  if Value <> FJointOptions then
  begin
    FJointOptions := Value;
    Attach;
  end;
end;


// ---------------
// --------------- TODEJointParams ---------------
// ---------------

// Create
//
constructor TODEJointParams.Create(AOwner: TPersistent);
begin
  inherited Create;
  FOwner := AOwner;
end;

// GetOwner
//
function TODEJointParams.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

// Assign
//
procedure TODEJointParams.Assign(Source: TPersistent);
begin
  inherited;
  if not Assigned(Source) then
    Exit;
  if Source is TODEJointParams then
  begin
    LoStop := TODEJointParams(Source).LoStop;
    HiStop := TODEJointParams(Source).HiStop;
    Vel := TODEJointParams(Source).Vel;
    FMax := TODEJointParams(Source).FMax;
    FudgeFactor := TODEJointParams(Source).FudgeFactor;
    Bounce := TODEJointParams(Source).Bounce;
    CFM := TODEJointParams(Source).CFM;
    StopERP := TODEJointParams(Source).StopERP;
    StopCFM := TODEJointParams(Source).StopCFM;
    SuspensionERP := TODEJointParams(Source).SuspensionERP;
    SuspensionCFM := TODEJointParams(Source).SuspensionCFM;
  end;
end;

// WriteToFiler
//
procedure TODEJointParams.WriteToFiler(writer: TWriter);
begin
  with writer do
  begin
    WriteInteger(0); // Archive version
    WriteFloat(LoStop);
    WriteFloat(HiStop);
    WriteFloat(Vel);
    WriteFloat(FMax);
    WriteFloat(FudgeFactor);
    WriteFloat(Bounce);
    WriteFloat(CFM);
    WriteFloat(StopERP);
    WriteFloat(StopCFM);
    WriteFloat(SuspensionERP);
    WriteFloat(SuspensionCFM);
  end;
end;

// ReadFromFiler
//
procedure TODEJointParams.ReadFromFiler(reader: TReader);
var
  archiveVersion: Integer;
begin
  with reader do
  begin
    archiveVersion := ReadInteger;
    Assert(archiveVersion = 0);

    LoStop := ReadFloat;
    HiStop := ReadFloat;
    Vel := ReadFloat;
    FMax := ReadFloat;
    FudgeFactor := ReadFloat;
    Bounce := ReadFloat;
    CFM := ReadFloat;
    StopERP := ReadFloat;
    StopCFM := ReadFloat;
    SuspensionERP := ReadFloat;
    SuspensionCFM := ReadFloat;
  end;
end;

// GetLoStop
//
function TODEJointParams.GetLoStop: TdReal;
begin
  if Assigned(GetCallback) then
    GetCallback(dParamLoStop1, FLoStop);
  Result := FLoStop;
end;

// GetHiStop
//
function TODEJointParams.GetHiStop: TdReal;
begin
  if Assigned(GetCallback) then
    GetCallback(dParamHiStop1, FHiStop);
  Result := FHiStop;
end;

// GetVel
//
function TODEJointParams.GetVel: TdReal;
begin
  if Assigned(GetCallback) then
    GetCallback(dParamVel1, FVel);
  Result := FVel;
end;

// GetFMax
//
function TODEJointParams.GetFMax: TdReal;
begin
  if Assigned(GetCallback) then
    GetCallback(dParamFMax1, FFMax);
  Result := FFMax;
end;

// GetFudgeFactor
//
function TODEJointParams.GetFudgeFactor: TdReal;
begin
  if Assigned(GetCallback) then
    GetCallback(dParamFudgeFactor1, FFudgeFactor);
  Result := FFudgeFactor;
end;

// GetBounce
//
function TODEJointParams.GetBounce: TdReal;
begin
  if Assigned(GetCallback) then
    GetCallback(dParamBounce1, FBounce);
  Result := FBounce;
end;

// GetCFM
//
function TODEJointParams.GetCFM: TdReal;
begin
  if Assigned(GetCallback) then
    GetCallback(dParamCFM1, FCFM);
  Result := FCFM;
end;

// GetStopERP
//
function TODEJointParams.GetStopERP: TdReal;
begin
  if Assigned(GetCallback) then
    GetCallback(dParamStopERP1, FStopERP);
  Result := FStopERP;
end;

// GetStopCFM
//
function TODEJointParams.GetStopCFM: TdReal;
begin
  if Assigned(GetCallback) then
    GetCallback(dParamStopCFM1, FStopCFM);
  Result := FStopCFM;
end;

// GetSuspensionERP
//
function TODEJointParams.GetSuspensionERP: TdReal;
begin
  if Assigned(GetCallback) then
    GetCallback(dParamSuspensionERP, FSuspensionERP);
  Result := FSuspensionERP;
end;

// GetSuspensionCFM
//
function TODEJointParams.GetSuspensionCFM: TdReal;
begin
  if Assigned(GetCallback) then
    GetCallback(dParamSuspensionCFM, FSuspensionCFM);
  Result := FSuspensionCFM;
end;

// SetLoStop
//
procedure TODEJointParams.SetLoStop(const Value: TdReal);
begin
  if Value <> FLoStop then
  begin
    FLoStop := Value;
    if Assigned(SetCallback) then
      FFlagLoStop := not SetCallback(dParamLoStop1, FLoStop)
    else
      FFlagLoStop := True;
  end;
end;

// SetHiStop
//
procedure TODEJointParams.SetHiStop(const Value: TdReal);
begin
  if Value <> FHiStop then
  begin
    FHiStop := Value;
    if Assigned(SetCallback) then
      FFlagHiStop := not SetCallback(dParamHiStop1, FHiStop)
    else
      FFlagHiStop := True;
  end;
end;

// SetVel
//
procedure TODEJointParams.SetVel(const Value: TdReal);
begin
  if Value <> FVel then
  begin
    FVel := Value;
    if Assigned(SetCallback) then
      FFlagVel := not SetCallback(dParamVel1, FVel)
    else
      FFlagVel := True;
  end;
end;

// SetFMax
//
procedure TODEJointParams.SetFMax(const Value: TdReal);
begin
  if Value <> FFMax then
  begin
    FFMax := Value;
    if Assigned(SetCallback) then
      FFlagFMax := not SetCallback(dParamFMax1, FFMax)
    else
      FFlagFMax := True;
  end;
end;

// SetFudgeFactor
//
procedure TODEJointParams.SetFudgeFactor(const Value: TdReal);
begin
  if Value <> FFudgeFactor then
  begin
    FFudgeFactor := Value;
    if Assigned(SetCallback) then
      FFlagFudgeFactor := not SetCallback(dParamFudgeFactor1, FFudgeFactor)
    else
      FFlagFudgeFactor := True;
  end;
end;

// SetBounce
//
procedure TODEJointParams.SetBounce(const Value: TdReal);
begin
  if Value <> FBounce then
  begin
    FBounce := Value;
    if Assigned(SetCallback) then
      FFlagBounce := not SetCallback(dParamBounce1, FBounce)
    else
      FFlagBounce := True;
  end;
end;

// SetCFM
//
procedure TODEJointParams.SetCFM(const Value: TdReal);
begin
  if Value <> FCFM then
  begin
    FCFM := Value;
    if Assigned(SetCallback) then
      FFlagCFM := not SetCallback(dParamCFM1, FCFM)
    else
      FFlagCFM := True;
  end;
end;

// SetStopERP
//
procedure TODEJointParams.SetStopERP(const Value: TdReal);
begin
  if Value <> FStopERP then
  begin
    FStopERP := Value;
    if Assigned(SetCallback) then
      FFlagStopERP := not SetCallback(dParamStopERP1, FStopERP)
    else
      FFlagStopERP := True;
  end;
end;

// SetStopCFM
//
procedure TODEJointParams.SetStopCFM(const Value: TdReal);
begin
  if Value <> FStopCFM then
  begin
    FStopCFM := Value;
    if Assigned(SetCallback) then
      FFlagStopCFM := not SetCallback(dParamStopCFM1, FStopCFM)
    else
      FFlagStopCFM := True;
  end;
end;

// SetSuspensionERP
//
procedure TODEJointParams.SetSuspensionERP(const Value: TdReal);
begin
  if Value <> FSuspensionERP then
  begin
    FSuspensionERP := Value;
    if Assigned(SetCallback) then
      FFlagSuspensionERP := not SetCallback(dParamSuspensionERP, FSuspensionERP)
    else
      FFlagSuspensionERP := True;
  end;
end;

// SetSuspensionCFM
//
procedure TODEJointParams.SetSuspensionCFM(const Value: TdReal);
begin
  if Value <> FSuspensionCFM then
  begin
    FSuspensionCFM := Value;
    if Assigned(SetCallback) then
      FFlagSuspensionCFM := not SetCallback(dParamSuspensionCFM, FSuspensionCFM)
    else
      FFlagSuspensionCFM := True;
  end;
end;

// ApplyFlagged
//
procedure TODEJointParams.ApplyFlagged;
begin
  if not Assigned(SetCallback) then
    Exit;
  if FFlagLoStop then
    SetCallback(dParamLoStop1, FLoStop);
  if FFlagHiStop then
    SetCallback(dParamHiStop1, FHiStop);
  if FFlagVel then
    SetCallback(dParamVel1, FVel);
  if FFlagFMax then
    SetCallback(dParamFMax1, FFMax);
  if FFlagFudgeFactor then
    SetCallback(dParamFudgeFactor1, FFudgeFactor);
  if FFlagBounce then
    SetCallback(dParamBounce1, FBounce);
  if FFlagCFM then
    SetCallback(dParamCFM1, FCFM);
  if FFlagStopERP then
    SetCallback(dParamStopERP1, FStopERP);
  if FFlagStopCFM then
    SetCallback(dParamStopCFM1, FStopCFM);
  if FFlagSuspensionERP then
    SetCallback(dParamSuspensionERP, FSuspensionERP);
  if FFlagSuspensionCFM then
    SetCallback(dParamSuspensionCFM, FSuspensionCFM);
end;


// ---------------
// --------------- TGLODEJointHinge ---------------
// ---------------

// Create
//
constructor TGLODEJointHinge.Create(AOwner: TVKXCollection);
begin
  inherited;
  FAnchor := TVKCoordinates.CreateInitialized(Self, NullHmgPoint, csPoint);
  FAnchor.OnNotifyChange := AnchorChange;
  FAxis := TVKCoordinates.CreateInitialized(Self, ZHmgVector, csVector);
  FAxis.OnNotifyChange := AxisChange;
  FAxisParams := TODEJointParams.Create(Self);
  FAxisParams.SetCallback := SetAxisParam;
  FAxisParams.GetCallback := GetAxisParam;

end;

// Destroy
destructor TGLODEJointHinge.Destroy;
begin
  FAnchor.Free;
  FAxis.Free;
  FAxisParams.Free;
  inherited;
end;

// Initialize
//
procedure TGLODEJointHinge.Initialize;
begin
  if (not IsODEInitialized) or (FInitialized) then
    Exit;
  FJointID := dJointCreateHinge(FManager.World, nil);
  inherited;
end;

// WriteToFiler
//
procedure TGLODEJointHinge.WriteToFiler(writer: TWriter);
begin
  inherited;
  with writer do
  begin
    WriteInteger(0); // Archive version
    FAnchor.WriteToFiler(writer);
    FAxis.WriteToFiler(writer);
    FAxisParams.WriteToFiler(writer);
  end;
end;

// ReadFromFiler
//
procedure TGLODEJointHinge.ReadFromFiler(reader: TReader);
begin
  inherited;
  with reader do
  begin
    Assert(ReadInteger = 0); // Archive version
    FAnchor.ReadFromFiler(reader);
    FAxis.ReadFromFiler(reader);
    FAxisParams.ReadFromFiler(reader);
  end;
end;

// StructureChanged
//
procedure TGLODEJointHinge.StructureChanged;
begin
  AnchorChange(nil);
  AxisChange(nil);
  FAxisParams.ApplyFlagged;
end;

// AnchorChange
//
procedure TGLODEJointHinge.AnchorChange(Sender: TObject);
begin
  if IsAttached then
    dJointSetHingeAnchor(FJointID, FAnchor.X, FAnchor.Y, FAnchor.Z);
end;

// AxisChange
//
procedure TGLODEJointHinge.AxisChange(Sender: TObject);
var
  vec: TVector;
begin
  vec := FAxis.DirectVector;
  NormalizeVector(vec);
  FAxis.DirectVector := vec;
  if IsAttached then
    dJointSetHingeAxis(FJointID, FAxis.X, FAxis.Y, FAxis.Z);
end;

// FriendlyName
//
class function TGLODEJointHinge.FriendlyName: String;
begin
  Result := 'Hinge';
end;

// FriendlyDescription
//
class function TGLODEJointHinge.FriendlyDescription: String;
begin
  Result := 'ODE Hinge joint';
end;

// SetAnchor
//
procedure TGLODEJointHinge.SetAnchor(const Value: TVKCoordinates);
begin
  FAnchor.Assign(Value);
end;

// SetAxis
//
procedure TGLODEJointHinge.SetAxis(const Value: TVKCoordinates);
begin
  FAxis.Assign(Value);
end;

// SetAxisParams
//
procedure TGLODEJointHinge.SetAxisParams(const Value: TODEJointParams);
begin
  AxisParams.Assign(Value);
end;

// SetAxisParam
//
function TGLODEJointHinge.SetAxisParam(Param: Integer;
  const Value: TdReal): Boolean;
begin
  if IsAttached then
  begin
    dJointSetHingeParam(JointID, Param, Value);
    Result := True;
  end
  else
    Result := False;
end;

// GetAxisParam
//
function TGLODEJointHinge.GetAxisParam(Param: Integer;
  var Value: TdReal): Boolean;
begin
  if IsAttached then
  begin
    Value := dJointGetHingeParam(JointID, Param);
    Result := True;
  end
  else
    Result := False;
end;


// ---------------
// --------------- TODEJointBall ---------------
// ---------------

// Create
//
constructor TODEJointBall.Create(AOwner: TVKXCollection);
begin
  inherited;
  FAnchor := TVKCoordinates.CreateInitialized(Self, NullHmgPoint, csPoint);
  FAnchor.OnNotifyChange := AnchorChange;
end;

// Destroy
destructor TODEJointBall.Destroy;
begin
  FAnchor.Free;
  inherited;
end;

// Initialize
//
procedure TODEJointBall.Initialize;
begin
  if (not IsODEInitialized) or (FInitialized) then
    Exit;
  FJointID := dJointCreateBall(FManager.World, nil);
  inherited;
end;

// WriteToFiler
//
procedure TODEJointBall.WriteToFiler(writer: TWriter);
begin
  inherited;
  with writer do
  begin
    WriteInteger(0); // Archive version
    FAnchor.WriteToFiler(writer);
  end;
end;

// ReadFromFiler
//
procedure TODEJointBall.ReadFromFiler(reader: TReader);
begin
  inherited;
  with reader do
  begin
    Assert(ReadInteger = 0); // Archive version
    FAnchor.ReadFromFiler(reader);
  end;
end;

// StructureChanged
//
procedure TODEJointBall.StructureChanged;
begin
  AnchorChange(nil);
end;

// AnchorChange
//
procedure TODEJointBall.AnchorChange(Sender: TObject);
begin
  if IsAttached then
    dJointSetBallAnchor(FJointID, FAnchor.X, FAnchor.Y, FAnchor.Z);
end;

// FriendlyName
//
class function TODEJointBall.FriendlyName: String;
begin
  Result := 'Ball';
end;

// FriendlyDescription
//
class function TODEJointBall.FriendlyDescription: String;
begin
  Result := 'ODE Ball joint implementation';
end;

// SetAnchor
//
procedure TODEJointBall.SetAnchor(const Value: TVKCoordinates);
begin
  FAnchor.Assign(Value);
end;


// ---------------
// --------------- TODEJointSlider ---------------
// ---------------

// Create
//
constructor TODEJointSlider.Create(AOwner: TVKXCollection);
begin
  inherited;
  FAxis := TVKCoordinates.CreateInitialized(Self, ZHmgVector, csVector);
  FAxis.OnNotifyChange := AxisChange;
  FAxisParams := TODEJointParams.Create(Self);
  FAxisParams.SetCallback := SetAxisParam;
  FAxisParams.GetCallback := GetAxisParam;
end;

// Destroy
destructor TODEJointSlider.Destroy;
begin
  FAxis.Free;
  FAxisParams.Free;
  inherited;
end;

// Initialize
//
procedure TODEJointSlider.Initialize;
begin
  if (not IsODEInitialized) or (FInitialized) then
    Exit;
  FJointID := dJointCreateSlider(FManager.World, nil);
  inherited;
end;

// WriteToFiler
//
procedure TODEJointSlider.WriteToFiler(writer: TWriter);
begin
  inherited;
  with writer do
  begin
    WriteInteger(0); // Archive version
    FAxis.WriteToFiler(writer);
    FAxisParams.WriteToFiler(writer);
  end;
end;

// ReadFromFiler
//
procedure TODEJointSlider.ReadFromFiler(reader: TReader);
begin
  inherited;
  with reader do
  begin
    Assert(ReadInteger = 0); // Archive version
    FAxis.ReadFromFiler(reader);
    FAxisParams.ReadFromFiler(reader);
  end;
end;

// StructureChanged
//
procedure TODEJointSlider.StructureChanged;
begin
  AxisChange(nil);
  AxisParams.ApplyFlagged;
end;

// AxisChange
//
procedure TODEJointSlider.AxisChange(Sender: TObject);
var
  vec: TVector;
begin
  vec := FAxis.DirectVector;
  NormalizeVector(vec);
  FAxis.DirectVector := vec;
  if IsAttached then
    dJointSetSliderAxis(FJointID, FAxis.X, FAxis.Y, FAxis.Z);
end;

// FriendlyName
//
class function TODEJointSlider.FriendlyName: String;
begin
  Result := 'Slider';
end;

// FriendlyDescription
//
class function TODEJointSlider.FriendlyDescription: String;
begin
  Result := 'ODE Slider joint implementation';
end;

// SetAxis
//
procedure TODEJointSlider.SetAxis(const Value: TVKCoordinates);
begin
  FAxis.Assign(Value);
end;

// SetAxisParams
//
procedure TODEJointSlider.SetAxisParams(const Value: TODEJointParams);
begin
  AxisParams.Assign(Value);
end;

// SetAxisParam
//
function TODEJointSlider.SetAxisParam(Param: Integer;
  const Value: TdReal): Boolean;
begin
  if IsAttached then
  begin
    dJointSetSliderParam(JointID, Param, Value);
    Result := True;
  end
  else
    Result := False;
end;

// GetAxisParam
//
function TODEJointSlider.GetAxisParam(Param: Integer;
  var Value: TdReal): Boolean;
begin
  if IsAttached then
  begin
    Value := dJointGetSliderParam(JointID, Param);
    Result := True;
  end
  else
    Result := False;
end;


// ---------------
// --------------- TODEJointFixed ---------------
// ---------------

// Initialize
//
procedure TODEJointFixed.Initialize;
begin
  if (not IsODEInitialized) or (FInitialized) then
    Exit;
  FJointID := dJointCreateFixed(FManager.World, nil);
  inherited;
end;

// WriteToFiler
//
procedure TODEJointFixed.WriteToFiler(writer: TWriter);
begin
  inherited;
  with writer do
  begin
    WriteInteger(0); // Archive version
  end;
end;

// ReadFromFiler
//
procedure TODEJointFixed.ReadFromFiler(reader: TReader);
begin
  inherited;
  with reader do
  begin
    Assert(ReadInteger = 0); // Archive version
  end;
end;

// FriendlyName
//
class function TODEJointFixed.FriendlyName: String;
begin
  Result := 'Fixed';
end;

// FriendlyDescription
//
class function TODEJointFixed.FriendlyDescription: String;
begin
  Result := 'ODE Fixed joint implementation';
end;


// ---------------
// --------------- TGLODEJointHinge2 ---------------
// ---------------

// Create
//
constructor TGLODEJointHinge2.Create(AOwner: TVKXCollection);
begin
  inherited;
  FAnchor := TVKCoordinates.CreateInitialized(Self, NullHmgPoint, csPoint);
  FAnchor.OnNotifyChange := AnchorChange;
  FAxis1 := TVKCoordinates.CreateInitialized(Self, ZHmgVector, csVector);
  FAxis1.OnNotifyChange := Axis1Change;
  FAxis2 := TVKCoordinates.CreateInitialized(Self, ZHmgVector, csVector);
  FAxis2.OnNotifyChange := Axis2Change;
  FAxis1Params := TODEJointParams.Create(Self);
  FAxis1Params.SetCallback := SetAxis1Param;
  FAxis1Params.GetCallback := GetAxis1Param;
  FAxis2Params := TODEJointParams.Create(Self);
  FAxis2Params.SetCallback := SetAxis2Param;
  FAxis2Params.GetCallback := GetAxis2Param;

  JointOptions := [joBothObjectsMustBeAssigned];
end;

// Destroy
destructor TGLODEJointHinge2.Destroy;
begin
  FAnchor.Free;
  FAxis1.Free;
  FAxis2.Free;
  FAxis1Params.Free;
  FAxis2Params.Free;
  inherited;
end;

// Initialize
//
procedure TGLODEJointHinge2.Initialize;
begin
  if (not IsODEInitialized) or (FInitialized) then
    Exit;
  FJointID := dJointCreateHinge2(FManager.World, nil);
  inherited;
end;

// WriteToFiler
//
procedure TGLODEJointHinge2.WriteToFiler(writer: TWriter);
begin
  inherited;
  with writer do
  begin
    WriteInteger(0); // Archive version
    FAnchor.WriteToFiler(writer);
    FAxis1.WriteToFiler(writer);
    FAxis2.WriteToFiler(writer);
    FAxis1Params.WriteToFiler(writer);
    FAxis2Params.WriteToFiler(writer);
  end;
end;

// ReadFromFiler
//
procedure TGLODEJointHinge2.ReadFromFiler(reader: TReader);
begin
  inherited;
  with reader do
  begin
    Assert(ReadInteger = 0); // Archive version
    FAnchor.ReadFromFiler(reader);
    FAxis1.ReadFromFiler(reader);
    FAxis2.ReadFromFiler(reader);
    FAxis1Params.ReadFromFiler(reader);
    FAxis2Params.ReadFromFiler(reader);
  end;
end;

// StructureChanged
//
procedure TGLODEJointHinge2.StructureChanged;
begin
  AnchorChange(nil);
  Axis1Change(nil);
  Axis2Change(nil);
  Axis1Params.ApplyFlagged;
  Axis2Params.ApplyFlagged;
end;

// AnchorChange
//
procedure TGLODEJointHinge2.AnchorChange(Sender: TObject);
begin
  if IsAttached then
    dJointSetHinge2Anchor(FJointID, FAnchor.X, FAnchor.Y, FAnchor.Z);
end;

// Axis1Change
//
procedure TGLODEJointHinge2.Axis1Change(Sender: TObject);
var
  vec: TVector;
begin
  vec := FAxis1.DirectVector;
  NormalizeVector(vec);
  FAxis1.DirectVector := vec;
  if IsAttached then
    dJointSetHinge2Axis1(FJointID, FAxis1.X, FAxis1.Y, FAxis1.Z);
end;

// Axis2Change
//
procedure TGLODEJointHinge2.Axis2Change(Sender: TObject);
var
  vec: TVector;
begin
  vec := FAxis2.DirectVector;
  NormalizeVector(vec);
  FAxis2.DirectVector := vec;
  if IsAttached then
    dJointSetHinge2Axis2(FJointID, FAxis2.X, FAxis2.Y, FAxis2.Z);
end;

// FriendlyName
//
class function TGLODEJointHinge2.FriendlyName: String;
begin
  Result := 'Hinge2';
end;

// FriendlyDescription
//
class function TGLODEJointHinge2.FriendlyDescription: String;
begin
  Result := 'ODE Double Axis Hinge joint implementation';
end;

// SetAnchor
//
procedure TGLODEJointHinge2.SetAnchor(const Value: TVKCoordinates);
begin
  FAnchor.Assign(Value);
end;

// SetAxis1
//
procedure TGLODEJointHinge2.SetAxis1(const Value: TVKCoordinates);
begin
  FAxis1.Assign(Value);
end;

// SetAxis2
//
procedure TGLODEJointHinge2.SetAxis2(const Value: TVKCoordinates);
begin
  FAxis2.Assign(Value);
end;

// SetAxis1Params
//
procedure TGLODEJointHinge2.SetAxis1Params(const Value: TODEJointParams);
begin
  Axis1Params.Assign(Value);
end;

// SetAxis2Params
//
procedure TGLODEJointHinge2.SetAxis2Params(const Value: TODEJointParams);
begin
  Axis2Params.Assign(Value);
end;

// SetAxis1Param
//
function TGLODEJointHinge2.SetAxis1Param(Param: Integer;
  const Value: TdReal): Boolean;
begin
  if IsAttached then
  begin
    dJointSetHinge2Param(JointID, Param, Value);
    Result := True;
  end
  else
    Result := False;
end;

// SetAxis2Param
//
function TGLODEJointHinge2.SetAxis2Param(Param: Integer;
  const Value: TdReal): Boolean;
begin
  if IsAttached then
  begin
    dJointSetHinge2Param(JointID, dParamLoStop2 + Param, Value);
    Result := True;
  end
  else
    Result := False;
end;

// GetAxis1Param
//
function TGLODEJointHinge2.GetAxis1Param(Param: Integer;
  var Value: TdReal): Boolean;
begin
  if IsAttached then
  begin
    Value := dJointGetHinge2Param(JointID, Param);
    Result := True;
  end
  else
    Result := False;
end;

// GetAxis2Param
//
function TGLODEJointHinge2.GetAxis2Param(Param: Integer;
  var Value: TdReal): Boolean;
begin
  if IsAttached then
  begin
    Value := dJointGetHinge2Param(JointID, dParamLoStop2 + Param);
    Result := True;
  end
  else
    Result := False;
end;


// ---------------
// --------------- TODEJointUniversal ---------------
// ---------------

// Create
//
constructor TODEJointUniversal.Create(AOwner: TVKXCollection);
begin
  inherited;
  FAnchor := TVKCoordinates.CreateInitialized(Self, NullHmgPoint, csPoint);
  FAnchor.OnNotifyChange := AnchorChange;
  FAxis1 := TVKCoordinates.CreateInitialized(Self, ZHmgVector, csVector);
  FAxis1.OnNotifyChange := Axis1Change;
  FAxis2 := TVKCoordinates.CreateInitialized(Self, XHmgVector, csVector);
  FAxis2.OnNotifyChange := Axis2Change;
  FAxis1Params := TODEJointParams.Create(Self);
  FAxis1Params.SetCallback := SetAxis1Param;
  FAxis1Params.GetCallback := GetAxis1Param;
  FAxis2Params := TODEJointParams.Create(Self);
  FAxis2Params.SetCallback := SetAxis2Param;
  FAxis2Params.GetCallback := GetAxis2Param;

  JointOptions := [joBothObjectsMustBeAssigned];
end;

// Destroy
destructor TODEJointUniversal.Destroy;
begin
  FAnchor.Free;
  FAxis1.Free;
  FAxis2.Free;
  FAxis1Params.Free;
  FAxis2Params.Free;
  inherited;
end;

// Initialize
//
procedure TODEJointUniversal.Initialize;
begin
  if (not IsODEInitialized) or (FInitialized) then
    Exit;
  FJointID := dJointCreateUniversal(FManager.World, nil);
  inherited;
end;

// WriteToFiler
//
procedure TODEJointUniversal.WriteToFiler(writer: TWriter);
begin
  inherited;
  with writer do
  begin
    WriteInteger(0); // Archive version
    FAnchor.WriteToFiler(writer);
    FAxis1.WriteToFiler(writer);
    FAxis2.WriteToFiler(writer);
    FAxis1Params.WriteToFiler(writer);
    FAxis2Params.WriteToFiler(writer);
  end;
end;

// ReadFromFiler
//
procedure TODEJointUniversal.ReadFromFiler(reader: TReader);
begin
  inherited;
  with reader do
  begin
    Assert(ReadInteger = 0); // Archive version
    FAnchor.ReadFromFiler(reader);
    FAxis1.ReadFromFiler(reader);
    FAxis2.ReadFromFiler(reader);
    FAxis1Params.ReadFromFiler(reader);
    FAxis2Params.ReadFromFiler(reader);
  end;
end;

// StructureChanged
//
procedure TODEJointUniversal.StructureChanged;
begin
  AnchorChange(nil);
  Axis1Change(nil);
  Axis2Change(nil);
  Axis1Params.ApplyFlagged;
  Axis2Params.ApplyFlagged;
end;

// AnchorChange
//
procedure TODEJointUniversal.AnchorChange(Sender: TObject);
begin
  if IsAttached then
    dJointSetUniversalAnchor(FJointID, FAnchor.X, FAnchor.Y, FAnchor.Z);
end;

// Axis1Change
//
procedure TODEJointUniversal.Axis1Change(Sender: TObject);
var
  vec: TVector;
begin
  vec := FAxis1.DirectVector;
  NormalizeVector(vec);
  FAxis1.DirectVector := vec;
  if IsAttached then
    dJointSetUniversalAxis1(FJointID, FAxis1.X, FAxis1.Y, FAxis1.Z);
end;

// Axis2Change
//
procedure TODEJointUniversal.Axis2Change(Sender: TObject);
var
  vec: TVector;
begin
  vec := FAxis2.DirectVector;
  NormalizeVector(vec);
  FAxis2.DirectVector := vec;
  if IsAttached then
    dJointSetUniversalAxis2(FJointID, FAxis2.X, FAxis2.Y, FAxis2.Z);
end;

// FriendlyName
//
class function TODEJointUniversal.FriendlyName: String;
begin
  Result := 'Universal';
end;

// FriendlyDescription
//
class function TODEJointUniversal.FriendlyDescription: String;
begin
  Result := 'ODE Universal joint implementation';
end;

// SetAnchor
//
procedure TODEJointUniversal.SetAnchor(const Value: TVKCoordinates);
begin
  FAnchor.Assign(Value);
end;

// SetAxis1
//
procedure TODEJointUniversal.SetAxis1(const Value: TVKCoordinates);
begin
  FAxis1.Assign(Value);
end;

// SetAxis2
//
procedure TODEJointUniversal.SetAxis2(const Value: TVKCoordinates);
begin
  FAxis2.Assign(Value);
end;

// SetAxis1Params
//
procedure TODEJointUniversal.SetAxis1Params(const Value: TODEJointParams);
begin
  Axis1Params.Assign(Value);
end;

// SetAxis2Params
//
procedure TODEJointUniversal.SetAxis2Params(const Value: TODEJointParams);
begin
  Axis2Params.Assign(Value);
end;

// SetAxis1Param
//
function TODEJointUniversal.SetAxis1Param(Param: Integer;
  const Value: TdReal): Boolean;
begin
  if IsAttached then
  begin
    dJointSetUniversalParam(JointID, Param, Value);
    Result := True;
  end
  else
    Result := False;
end;

// SetAxis2Param
//
function TODEJointUniversal.SetAxis2Param(Param: Integer;
  const Value: TdReal): Boolean;
begin
  if IsAttached then
  begin
    dJointSetUniversalParam(JointID, dParamLoStop2 + Param, Value);
    Result := True;
  end
  else
    Result := False;
end;

// GetAxis1Param
//
function TODEJointUniversal.GetAxis1Param(Param: Integer;
  var Value: TdReal): Boolean;
begin
  if IsAttached then
  begin
    Value := dJointGetUniversalParam(JointID, Param);
    Result := True;
  end
  else
    Result := False;
end;

// GetAxis2Param
//
function TODEJointUniversal.GetAxis2Param(Param: Integer;
  var Value: TdReal): Boolean;
begin
  if IsAttached then
  begin
    Value := dJointGetUniversalParam(JointID, dParamLoStop2 + Param);
    Result := True;
  end
  else
    Result := False;
end;


// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

initialization

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

vGLODEObjectRegister := TList.Create;

RegisterXCollectionItemClass(TVKODEDynamic);
RegisterXCollectionItemClass(TVKODEStatic);

RegisterXCollectionItemClass(TODEElementBox);
RegisterXCollectionItemClass(TODEElementSphere);
RegisterXCollectionItemClass(TODEElementCapsule);
RegisterXCollectionItemClass(TODEElementCylinder);
RegisterXCollectionItemClass(TODEElementTriMesh);
RegisterXCollectionItemClass(TODEElementPlane);

RegisterXCollectionItemClass(TGLODEJointHinge);
RegisterXCollectionItemClass(TODEJointBall);
RegisterXCollectionItemClass(TODEJointSlider);
RegisterXCollectionItemClass(TODEJointFixed);
RegisterXCollectionItemClass(TGLODEJointHinge2);
RegisterXCollectionItemClass(TODEJointUniversal);

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
finalization

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

vGLODEObjectRegister.Free;

UnregisterXCollectionItemClass(TVKODEDynamic);
UnregisterXCollectionItemClass(TVKODEStatic);

UnregisterXCollectionItemClass(TODEElementBox);
UnregisterXCollectionItemClass(TODEElementSphere);
UnregisterXCollectionItemClass(TODEElementCapsule);
UnregisterXCollectionItemClass(TODEElementCylinder);
UnregisterXCollectionItemClass(TODEElementTriMesh);
UnregisterXCollectionItemClass(TODEElementPlane);

UnregisterXCollectionItemClass(TGLODEJointHinge);
UnregisterXCollectionItemClass(TODEJointBall);
UnregisterXCollectionItemClass(TODEJointSlider);
UnregisterXCollectionItemClass(TODEJointFixed);
UnregisterXCollectionItemClass(TGLODEJointHinge2);
UnregisterXCollectionItemClass(TODEJointUniversal);

// CloseODE;

end.
