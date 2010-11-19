//
// This unit is part of the GLScene Project, http://glscene.org
//
{ : GLNGDManager<p>

  A Newton Game Dynamics Manager for GLScene.<p>

  Where can I find ... ?<ul>
  <li>GLScene                                   (http://glscene.org)
  <li>Newton Game Dynamics Engine               (http://newtondynamics.com)
  <li>NewtonImport, a Delph1i header translation (http://newtondynamics.com/forum/viewtopic.php?f=9&t=5273#p35865)
  </ul>

  Notes:
  This code is still being developed so any part of it may change at anytime.
  To install use the GLS_NGD?.dpk in the GLScene/Delphi? folder.<p>

  <b>History : </b><font size=-1><ul>

  <li>19/11/10 - FP - Added UseGravity property for TGLNGDDynamic
  <li>05/11/10 - FP - Removed check freeform in TGLNGDStatic.GeTree
  Removed FCollisionArray from TGLNGDBehaviour
  Modified misspelling usevelovity to usevelocity [thx bobrob69]
  Moved Creation of compound collision for freeform from GetCollisionFromBaseSceneObject to SetCollision for TGLNGDDynamic [thx bobrob69]
  <li>25/10/10 - FP - Fixed Material badly loaded when created in design time
  <li>25/10/10 - FP - Commented 'Release each collision form the array' in TGLNGDBehaviour.SetCollision.
  Changed angular friction in  TGLNGDDynamic.Pick method to be able to pick body with small mass.
  Added Beta Serialize and Deserialise for TGLNGDBehaviour.
  Commented 'rebuild in runtime' in TGLNGDStatic.Render, because this is conflicting with news serialize methods
  <li>23/10/10 - Yar - Replace OpenGL1x to OpenGLAdapter
  <li>08/10/10 - FP - Added show contact for dynamic in render.
  Uncommented ShowContact property in manager.
  <li>07/10/10 - FP - Joints connected to TGLNGDBehaviour are now freed in TGLNGDBehaviour.Destroy
  <li>30/09/10 - FP - Removed beta functions of player and car in TGLNGDDynamic.
  Added AddImpulse function in TGLNGDDynamic.
  <li>29/09/10 - FP - Moved FManager assignation for MaterialPair from loaded to create
  <li>21/09/10 - FP - Added timestep in TContactProcessEvent.
  Removed Manager property of MaterialPair.
  MaterialPair.loaded use the owner.owner component as manager now.
  MaterialPair FilerVersion up to 1
  <li>20/09/10 - FP - Call Finalize/Initialize in Setid
  <li>20/09/10 - YP - Moved MaterialAutoCreateGroupID call into Material.Initialize
  <li>19/09/10 - YP - Added MaterialAutoCreateGroupID to fix loaded order
  <li>18/09/10 - YP - Added Get and GetOrCreate NGD behaviors routine
  <li>15/07/10 - FP - Creation by Franck Papouin
  </ul></font>
}

unit GLNGDManager;

interface

uses
  NewtonImport, NewtonImport_JointLibrary // Newton
  , VectorGeometry // PVector TVector TMatrix PMatrix NullHmgVector...
  , VectorLists // TaffineVectorList for SetTree
  // , VectorTypes  //TVector3f...
  , Classes // TComponent Tlist TWriter TReader
  , PersistentClasses, XCollection // TPersistent TXCollection
  , SysUtils // IntToStr  for material in render
  , GLScene, GLManager, GLCrossPlatform, GLCoordinates //
  , GLObjects, GLGeomObjects, GLVectorFileObjects // cube cone freeform...
  , OpenGLTokens, GLRenderContextInfo, GLContext, GLTextureFormat
  // Base OpenGL
  , GLColor, GLBitmapFont, GLState // For show debug
  , GLFile3DS;

type
  { Enums }
  TNGDSolverModels = (smExact = 0, smLinear1, smLinear2, smLinear3);
  TNGDFrictionModels = (fmExact = 0, fmAdaptive);
  TNGDPickedModes = (pmAttach = 0, pmMove, pmDetach);

  TGLNGDBehaviour = class;
  TNGDMaterialPair = class;
  TNGDMaterials = class;
  TNGDJointBase = class;

  { Events }
  TMaterialHitEvent = procedure(obj0, obj1: TGLBaseSceneObject;
    id0, id1: Integer) of object;
  TContactProcessEvent = procedure(NGDMaterialPair: TNGDMaterialPair;
    contact: PNewtonJoint; timestep: Float) of object;

  { Class }
  TGLNGDManager = class(TComponent)
  private
    { Private Declarations }
    FNGDBehaviours: TPersistentObjectList;
    FRenderPoint: TGLRenderPoint;
    FBitmapFont: TGLCustomBitmapFont;
    FMaterials: TNGDMaterials;
    FMaxMaterialID: Integer;
    FVisible, FVisibleAtRunTime: Boolean; // Show Debug
    FNewtonWorld: PNewtonWorld;
    FVersion: Integer;
    FSolverModel: TNGDSolverModels; // Default=Exact
    FFrictionModel: TNGDFrictionModels; // Default=Exact
    FMinimumFrameRate: Integer; // Default=60
    FWorldSizeMin: TGLCoordinates; // Default=-100, -100, -100
    FWorldSizeMax: TGLCoordinates; // Default=100, 100, 100
    FThreadCount: Integer; // Default=1
    FGravity: TGLCoordinates; // Default=(0,-9.81,0)
    FWaterDensity: Single; // Default=0;
    FWaterPlane: TGLCoordinates4; // direction and distance to origin (0,1,0,0)

    // Debugs
    FShowGeometry: Boolean; // Default=False
    FGeomColorDyn, FGeomColorStat: TGLColor; // Green, Red
    FShowAABB: Boolean; // Default=False
    FAABBColor: TGLColor; // Yellow
    FAABBColorSleep: TGLColor; // Orange
    FShowMaterialESP: Boolean; // Default=False
    FMaterialESPColor: TGLColor; // Purple
    FShowContact: Boolean; // Default=False
    FContactColor: TGLColor; // White
    FShowJoint: Boolean; // Default=False
    FJointColor: TGLColor; // Blue
    FShowSpring: Boolean; // Default=False
    FSpringColor: TGLColor; // Aqua

    // Events
    FMaterialHitEvent: TMaterialHitEvent;
  protected
    { Protected Declarations }
    procedure Loaded; override;
    procedure SetRenderPoint(const value: TGLRenderPoint);
    procedure SetBitmapFont(const value: TGLCustomBitmapFont);
    procedure RenderEvent(Sender: TObject; var rci: TRenderContextInfo);
    procedure RenderPointFreed(Sender: TObject);
    procedure SetVisible(const value: Boolean);
    procedure SetVisibleAtRunTime(const value: Boolean);
    procedure SetShowGeometry(const value: Boolean);
    procedure SetShowAABB(const value: Boolean);
    procedure SetShowMaterialESP(const value: Boolean);
    procedure SetShowContact(const value: Boolean);
    procedure SetShowJoint(const value: Boolean);
    procedure SetShowSpring(const value: Boolean);
    function GetNGDBehaviour(index: Integer): TGLNGDBehaviour;
    property NGDBehaviours[index: Integer]
      : TGLNGDBehaviour read GetNGDBehaviour;
    procedure RegisterNGDBehaviour(NGDBehaviour: TGLNGDBehaviour);
    procedure UnregisterNGDBehaviour(NGDBehaviour: TGLNGDBehaviour);
    procedure SetSolverModel(val: TNGDSolverModels);
    procedure SetFrictionModel(val: TNGDFrictionModels);
    procedure SetMinimumFrameRate(val: Integer);
    procedure SetThreadCount(val: Integer);
    function GetBodyCount: Integer;
    function GetConstraintCount: Integer;
    procedure SetWaterDensity(val: Single);
    procedure CallBodyIterator;

    // Materials
    procedure WriteMaterials(stream: TStream);
    procedure ReadMaterials(stream: TStream);
    procedure DefineProperties(Filer: TFiler); override;
    procedure MaterialAutoCreateGroupID(MaterialID: Integer);

    // Events
    procedure NotifyWorldSizeChange(Sender: TObject);
    procedure NotifyGravityChange(Sender: TObject);
    procedure NotifyWaterPlaneChange(Sender: TObject);
  public
    { Public Declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure NotifyChange(Sender: TObject);
    procedure Step(deltatime: Single);
    function MaterialCreateGroupID: Integer;
    // Property NewtonWorld: PNewtonWorld Read FNewtonWorld;

  published
    { Published Declarations }
    property RenderPoint: TGLRenderPoint read FRenderPoint write SetRenderPoint;
    property BitmapFont: TGLCustomBitmapFont read FBitmapFont write
      SetBitmapFont;
    property Visible: Boolean read FVisible write SetVisible default False;
    property VisibleAtRunTime: Boolean read FVisibleAtRunTime write
      SetVisibleAtRunTime default False;
    property Materials: TNGDMaterials read FMaterials;
    property WaterDensity: Single read FWaterDensity write SetWaterDensity;
    property SolverModel: TNGDSolverModels read FSolverModel write
      SetSolverModel default smExact;
    property FrictionModel: TNGDFrictionModels read FFrictionModel write
      SetFrictionModel default fmExact;
    property MinimumFrameRate: Integer read FMinimumFrameRate write
      SetMinimumFrameRate default 60;
    property ThreadCount
      : Integer read FThreadCount write SetThreadCount default 1;
    property Version: Integer read FVersion;
    property BodyCount: Integer read GetBodyCount;
    property ConstraintCount: Integer read GetConstraintCount;
    property Gravity: TGLCoordinates read FGravity write FGravity;
    property WorldSizeMin
      : TGLCoordinates read FWorldSizeMin write FWorldSizeMin;
    property WorldSizeMax
      : TGLCoordinates read FWorldSizeMax write FWorldSizeMax;
    property WaterPlane: TGLCoordinates4 read FWaterPlane write FWaterPlane;

    // Debugs
    property ShowGeometry
      : Boolean read FShowGeometry write SetShowGeometry default
      False;
    property GeomColorDyn: TGLColor read FGeomColorDyn write FGeomColorDyn;
    property GeomColorStat: TGLColor read FGeomColorStat write FGeomColorStat;
    property ShowAABB: Boolean read FShowAABB write SetShowAABB default False;
    property AABBColor: TGLColor read FAABBColor write FAABBColor;
    property AABBColorSleep
      : TGLColor read FAABBColorSleep write FAABBColorSleep;
    property ShowMaterialESP: Boolean read FShowMaterialESP write
      SetShowMaterialESP default False;
    property MaterialESPColor
      : TGLColor read FMaterialESPColor write FMaterialESPColor;
    property ShowContact
      : Boolean read FShowContact write SetShowContact default False;
    property ContactColor: TGLColor read FContactColor write FContactColor;
    property ShowJoint
      : Boolean read FShowJoint write SetShowJoint default False;
    property JointColor: TGLColor read FJointColor write FJointColor;
    property ShowSpring
      : Boolean read FShowSpring write SetShowSpring default False;
    property SpringColor: TGLColor read FSpringColor write FSpringColor;

    // Events
    property OnMaterialHit: TMaterialHitEvent read FMaterialHitEvent write
      FMaterialHitEvent;
  end;

  { : Basis structures for GLScene behaviour style implementations. }
  TGLNGDBehaviour = class(TGLBehaviour)
  private
    { Private Declartions }
    FManager: TGLNGDManager;
    FManagerName: string;
    FInitialized: Boolean;
    FOwnerBaseSceneObject: TGLBaseSceneObject;
    FJointRegister: TList;
    FNewtonBody: PNewtonBody;
    FCollision: PNewtonCollision;
    FMaterialID: Integer; // Default=0
    FNewtonBodyMatrix: TMatrix; // Position and Orientation
    FContinuousCollisionMode: Boolean; // Default=False
    FBoundingSphereRadius: Single; // Checked every frame in design time
  protected
    { Protected Declarations }
    procedure Initialize; virtual;
    procedure Finalize; virtual;
    procedure WriteToFiler(writer: TWriter); override;
    procedure ReadFromFiler(reader: TReader); override;
    procedure Loaded; override;
    procedure SetManager(value: TGLNGDManager);
    procedure RegisterJoint(Joint: TNGDJointBase);
    procedure UnregisterJoint(Joint: TNGDJointBase);
    procedure SetNewtonBodyMatrix(val: TMatrix);
    procedure SetContinuousCollisionMode(val: Boolean);
    procedure SetMaterialID(val: Integer);
    function GetNewtonBodyMatrix: TMatrix;
    procedure SetCollision; virtual;
    procedure Render(var rci: TRenderContextInfo); virtual;
    function GetCollisionFromBaseSceneObject(SceneObject: TGLBaseSceneObject)
      : PNewtonCollision;

  public
    { Public Declarations }
    constructor Create(AOwner: TXCollection); override;
    destructor Destroy; override;
    procedure NotifyChange(Sender: TObject);
    procedure Reinitialize;
    property Initialized: Boolean read FInitialized;
    class function UniqueItem: Boolean; override;
    property NewtonBodyMatrix: TMatrix read GetNewtonBodyMatrix write
      SetNewtonBodyMatrix;
    procedure Serialize(filename: string);
    procedure DeSerialize(filename: string);

  published
    { Published Declarations }
    property Manager: TGLNGDManager read FManager write SetManager;
    property ContinuousCollisionMode
      : Boolean read FContinuousCollisionMode write
      SetContinuousCollisionMode default False;
    property MaterialID: Integer read FMaterialID write SetMaterialID default 0;
  end;

  TGLNGDDynamic = class(TGLNGDBehaviour)
  private
    { Private Declarations }
    FAABBmin: TGLCoordinates;
    FAABBmax: TGLCoordinates;
    FForce: TGLCoordinates;
    FTorque: TGLCoordinates;
    FOmega: TGLCoordinates;
    FVelocity: TGLCoordinates;
    FCenterOfMass: TGLCoordinates;
    FAutoSleep: Boolean; // Default=True
    FLinearDamping: Single; // default=0.1
    FAngularDamping: TGLCoordinates; // Default=0.1
    FDensity: Single; // Default=1
    FNewtonUserJoint: PNewtonUserJoint; // For PickJoint Kinematic
    FNewtonJoint: PNewtonJoint; // For UpVector
    FUpVector: Boolean; // Default=False
    FUpVectorDirection: TGLCoordinates; // Default [0,1,0]
    FUseVelocity: Boolean; // Default=False
    FUseOmega: Boolean; // Default=False
    FUseGravity: Boolean; // Default=True

    // Read Only
    FVolume: Single;
    FMass: Single;
    FAppliedForce: TGLCoordinates;
    FAppliedTorque: TGLCoordinates;
    FAppliedOmega: TGLCoordinates;
    FAppliedVelocity: TGLCoordinates;

  protected
    { Protected Declarations }
    procedure SetAutoSleep(val: Boolean);
    procedure SetLinearDamping(val: Single);
    procedure SetDensity(val: Single); virtual;
    procedure Initialize; override;
    procedure Finalize; override;
    procedure WriteToFiler(writer: TWriter); override;
    procedure ReadFromFiler(reader: TReader); override;
    procedure Loaded; override;
    procedure Render(var rci: TRenderContextInfo); override;
    procedure SetCollision; override;
    procedure SetUpVector(val: Boolean);

    // Events
    procedure NotifyCenterOfMassChange(Sender: TObject);
    procedure NotifyAngularDampingChange(Sender: TObject);
    procedure NotifyUpVectorDirectionChange(Sender: TObject);

  public
    { Public Declarations }
    constructor Create(AOwner: TXCollection); override;
    destructor Destroy; override;
    procedure Pick(pickpoint: TVector; Mode: TNGDPickedModes);
    { function player(pindir: TMatrix; stepfactor, cushion: Single)
      : PNewtonUserJoint;
      function car(maxTireCount: Integer; const cordenateSytemInLocalSpace: TMatrix)
      : PNewtonUserJoint; }
    procedure AddImpulse(veloc, pointposit: TVector);

    class function FriendlyName: string; override;

  published
    { Published Declarations }
    property Force: TGLCoordinates read FForce write FForce;
    property Torque: TGLCoordinates read FTorque write FTorque;
    property Omega: TGLCoordinates read FOmega write FOmega;
    property Velocity: TGLCoordinates read FVelocity write FVelocity;
    property CenterOfMass
      : TGLCoordinates read FCenterOfMass write FCenterOfMass;
    property AutoSleep: Boolean read FAutoSleep write SetAutoSleep default True;
    property LinearDamping: Single read FLinearDamping write SetLinearDamping;
    property AngularDamping
      : TGLCoordinates read FAngularDamping write FAngularDamping;
    property Density: Single read FDensity write SetDensity;
    property UpVector: Boolean read FUpVector write SetUpVector default False;
    property UpVectorDirection
      : TGLCoordinates read FUpVectorDirection write FUpVectorDirection;
    property UseVelocity
      : Boolean read FUseVelocity write FUseVelocity default False;
    property UseOmega: Boolean read FUseOmega write FUseOmega default False;
    property UseGravity
      : Boolean read FUseGravity write FUseGravity default True;

    // Read Only
    property AppliedOmega: TGLCoordinates read FAppliedOmega;
    property AppliedVelocity: TGLCoordinates read FAppliedVelocity;
    property AppliedForce: TGLCoordinates read FAppliedForce;
    property AppliedTorque: TGLCoordinates read FAppliedTorque;
    property Volume: Single read FVolume;
    property Mass: Single read FMass;
  end;

  TGLNGDStatic = class(TGLNGDBehaviour)
  private
    { Private Declarations }
    // FHeightFieldWordArray: array of UInt16;

  protected
    { Protected Declarations }
    procedure SetCollision; override;
    procedure Render(var rci: TRenderContextInfo); override;
    function GetTree(optimize: Boolean; scaleXYZ: Single): PNewtonCollision;

  public
    { Public Declarations }
    class function FriendlyName: string; override;
    procedure SetHeightField(heightArray: array of UInt16; x: Integer;
      y: Integer; xScale: Single; yScale: Single);

  published
    { Published Declarations }
  end;

  { : An XCollection decendant for NGD Materials. }
  TNGDMaterials = class(TXCollection)
  protected
    { Protected Declarations }
    function GetMaterialPair(index: Integer): TNGDMaterialPair;

  public
    { Public Declarations }
    class function ItemsClass: TXCollectionItemClass; override;
    procedure Initialize;
    procedure Finalize;
    property MaterialPair[index: Integer]
      : TNGDMaterialPair read GetMaterialPair; default;
  end;

  TNGDMaterialPair = class(TXCollectionItem)
  private
    { Private Declarations }
    FManager: TGLNGDManager;
    FSoftness: Single; // 0.1
    FElasticity: Single; // 0.4
    FCollidable: Boolean; // 1
    FStaticFriction: Single; // 0.9
    FKineticFriction: Single; // 0.5
    Fid0, Fid1: Integer;
    FInitialized: Boolean;

    // Event
    FContactProcessEvent: TContactProcessEvent;

  protected
    { Protected Declarations }
    procedure SetSoftness(val: Single);
    procedure SetElasticity(val: Single);
    procedure SetCollidable(val: Boolean);
    procedure SetStaticFriction(val: Single);
    procedure SetKineticFriction(val: Single);
    procedure Setid0(const value: Integer);
    procedure Setid1(const value: Integer);

    procedure WriteToFiler(writer: TWriter); override;
    procedure ReadFromFiler(reader: TReader); override;
    procedure Loaded; override;
    procedure Initialize; virtual;
    procedure Finalize; virtual;

  public
    { Public Declarations }
    constructor Create(AOwner: TXCollection); override;
    destructor Destroy; override;
    class function FriendlyName: string; override;
    class function FriendlyDescription: string; override;
    property Initialized: Boolean read FInitialized;
    property OnContactProcess
      : TContactProcessEvent read FContactProcessEvent
      write FContactProcessEvent;
  published
    { Published Declarations }

    property Softness: Single read FSoftness write SetSoftness;
    property Elasticity: Single read FElasticity write SetElasticity;
    property Collidable: Boolean read FCollidable write SetCollidable;
    property StaticFriction
      : Single read FStaticFriction write SetStaticFriction;
    property KineticFriction
      : Single read FKineticFriction write SetKineticFriction;
    property id0: Integer read Fid0 write Setid0 default 0;
    property id1: Integer read Fid1 write Setid1 default 0;
  end;

  { : An XCollection decendant for NGD Joints. }
  TNGDJoints = class(TXCollection)
  protected
    { Protected Declarations }
    function GetJoint(index: Integer): TNGDJointBase;

  public
    { Public Declarations }
    class function ItemsClass: TXCollectionItemClass; override;
    procedure Initialize;
    procedure Finalize;
    property Joint[index: Integer]: TNGDJointBase read GetJoint; default;
  end;

  { : Component front-end for storing NGD Joints. }
  TGLNGDJointList = class(TComponent)
  private
    { Private Declarations }
    FJoints: TNGDJoints;

  protected
    { Protected Declarations }
    procedure WriteJoints(stream: TStream);
    procedure ReadJoints(stream: TStream);
    procedure DefineProperties(Filer: TFiler); override;
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation);
      override;

  public
    { Public Declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

  published
    { Published Declarations }
    property Joints: TNGDJoints read FJoints;
  end;

  TNGDJointBase = class(TXCollectionItem)
  private
    { Private Declarations }
    FObject1, FObject2: TGLBaseSceneObject;
    FManager: TGLNGDManager;
    FInitialized: Boolean;
    FObject1Name, FObject2Name, FManagerName: string;

  protected
    { Protected Declarations }
    procedure WriteToFiler(writer: TWriter); override;
    procedure ReadFromFiler(reader: TReader); override;
    procedure Loaded; override;
    procedure Render; virtual;
    procedure RegisterJointWithObject(Obj: TGLBaseSceneObject);
    procedure UnregisterJointWithObject(Obj: TGLBaseSceneObject);
    procedure SetManager(const value: TGLNGDManager);
    procedure SetObject1(const value: TGLBaseSceneObject);
    procedure SetObject2(const value: TGLBaseSceneObject);
    procedure Initialize; virtual;
    procedure Finalize; virtual;
    property Initialized: Boolean read FInitialized;

  public
    { Public Declarations }
    constructor Create(AOwner: TXCollection); override;
    destructor Destroy; override;
    procedure StructureChanged(Sender: TObject); virtual;

  published
    { Published Declarations }
    property Manager: TGLNGDManager read FManager write SetManager;
    property Object1: TGLBaseSceneObject read FObject1 write SetObject1;
    property Object2: TGLBaseSceneObject read FObject2 write SetObject2;
  end;

  { : NGD ball joint implementation. }
  TNGDJointBall = class(TNGDJointBase)
  private
    { Private Declarations }
    FNewtonJoint: PNewtonJoint;
    FPivotPoint: TGLCoordinates;
    FStiffness: Single; // Default=0.9
    FCollisionState: Boolean; // Default=False

  protected
    { Protected Declarations }
    procedure WriteToFiler(writer: TWriter); override;
    procedure ReadFromFiler(reader: TReader); override;
    procedure Render; override;
    procedure SetStiffness(val: Single); virtual;
    procedure SetCollisionState(val: Boolean); virtual;

  public
    { Public Declarations }
    constructor Create(AOwner: TXCollection); override;
    destructor Destroy; override;
    procedure StructureChanged(Sender: TObject); override;
    class function FriendlyName: string; override;
    class function FriendlyDescription: string; override;

  published
    { Published Declarations }
    property Stiffness: Single read FStiffness write SetStiffness;
    property CollisionState
      : Boolean read FCollisionState write SetCollisionState default True;
    property PivotPoint: TGLCoordinates read FPivotPoint write FPivotPoint;
  end;

  { : NGD hinge joint implementation. }
  TNGDJointHinge = class(TNGDJointBall)
  private
    { Private Declarations }
    FPinDir: TGLCoordinates;

  protected
    { Protected Declarations }
    procedure WriteToFiler(writer: TWriter); override;
    procedure ReadFromFiler(reader: TReader); override;
    procedure Render; override;

  public
    { Public Declarations }
    constructor Create(AOwner: TXCollection); override;
    destructor Destroy; override;
    procedure StructureChanged(Sender: TObject); override;
    class function FriendlyName: string; override;
    class function FriendlyDescription: string; override;

  published
    { Published Declarations }
    property pindir: TGLCoordinates read FPinDir write FPinDir;
  end;

  { : NGD slider joint implementation. }
  TNGDJointSlider = class(TNGDJointHinge)
  private
    { Private Declarations }

  protected
    { Protected Declarations }

  public
    { Public Declarations }
    procedure StructureChanged(Sender: TObject); override;
    class function FriendlyName: string; override;
    class function FriendlyDescription: string; override;
  end;

  { : NGD Corkscrew joint implementation. }
  TNGDJointCorkscrew = class(TNGDJointHinge)
  private
    { Private Declarations }

  protected
    { Protected Declarations }

  public
    { Public Declarations }
    procedure StructureChanged(Sender: TObject); override;
    class function FriendlyName: string; override;
    class function FriendlyDescription: string; override;
  end;

  { : NGD hinge joint implementation. }
  TNGDJointUniversal = class(TNGDJointHinge)
  private
    { Private Declarations }
    FPinDir2: TGLCoordinates;

  protected
    { Protected Declarations }
    procedure WriteToFiler(writer: TWriter); override;
    procedure ReadFromFiler(reader: TReader); override;
    procedure Render; override;

  public
    { Public Declarations }
    constructor Create(AOwner: TXCollection); override;
    destructor Destroy; override;
    procedure StructureChanged(Sender: TObject); override;
    class function FriendlyName: string; override;
    class function FriendlyDescription: string; override;

  published
    { Published Declarations }
    property PinDir2: TGLCoordinates read FPinDir2 write FPinDir2;
  end;

  { : NGD Custom joint base implementation. }
  TNGDCustomJointBase = class(TNGDJointBase)
  private
    { Private Declarations }
    FNewtonUserJoint: PNewtonUserJoint;
    FPivotPoint: TGLCoordinates;
    FPinAndPivotMatrix: TMatrix;
    FStiffness: Single; // Default=0.9
    FCollisionState: Boolean; // Default=False

  protected
    { Protected Declarations }
    FMinLimit, FMaxLimit: Single;
    procedure WriteToFiler(writer: TWriter); override;
    procedure ReadFromFiler(reader: TReader); override;
    procedure Render; override;
    procedure SetMaxLimit(val: Single); virtual;
    procedure SetMinLimit(val: Single); virtual;
    procedure SetStiffness(val: Single); virtual;
    procedure SetCollisionState(val: Boolean); virtual;

  public
    { Public Declarations }
    constructor Create(AOwner: TXCollection); override;
    destructor Destroy; override;
    procedure StructureChanged(Sender: TObject); override;

  published
    { Published Declarations }
    property Stiffness: Single read FStiffness write SetStiffness;
    property CollisionState
      : Boolean read FCollisionState write SetCollisionState default True;
    property PivotPoint: TGLCoordinates read FPivotPoint write FPivotPoint;
  end;

  { : NGD Custom joint ball implementation. }
  TNGDCustomJointBall = class(TNGDCustomJointBase)
  private
    { Private Declarations }
    FConeAngle: Single;

  protected
    { Protected Declarations }
    procedure WriteToFiler(writer: TWriter); override;
    procedure ReadFromFiler(reader: TReader); override;
    procedure SetConeAngle(val: Single);
    procedure SetMaxLimit(val: Single); override;
    procedure SetMinLimit(val: Single); override;

  public
    { Public Declarations }
    constructor Create(AOwner: TXCollection); override;
    procedure StructureChanged(Sender: TObject); override;
    class function FriendlyName: string; override;
    class function FriendlyDescription: string; override;

  published
    { Published Declarations }
    property ConeAngle: Single read FConeAngle write SetConeAngle;
    property MinTwistAngle: Single read FMinLimit write SetMinLimit;
    property MaxTwistAngle: Single read FMaxLimit write SetMaxLimit;
  end;

  { : NGD Custom joint hinge implementation. }
  TNGDCustomJointBaseDir = class(TNGDCustomJointBase)
  private
    { Private Declarations }
    FPinDir: TGLCoordinates;

  protected
    { Protected Declarations }
    procedure WriteToFiler(writer: TWriter); override;
    procedure ReadFromFiler(reader: TReader); override;
    procedure Render; override;

  public
    { Public Declarations }
    constructor Create(AOwner: TXCollection); override;
    destructor Destroy; override;
    procedure StructureChanged(Sender: TObject); override;

  published
    { Published Declarations }
    property pindir: TGLCoordinates read FPinDir write FPinDir;
  end;

  { : NGD Custom joint hinge implementation. }
  TNGDCustomJointHinge = class(TNGDCustomJointBaseDir)
  private
    { Private Declarations }

  protected
    { Protected Declarations }
    procedure SetMaxLimit(val: Single); override;
    procedure SetMinLimit(val: Single); override;

  public
    { Public Declarations }
    constructor Create(AOwner: TXCollection); override;
    procedure StructureChanged(Sender: TObject); override;
    class function FriendlyName: string; override;
    class function FriendlyDescription: string; override;

  published
    { Published Declarations }
    property MinAngle: Single read FMinLimit write SetMinLimit;
    property MaxAngle: Single read FMaxLimit write SetMaxLimit;
  end;

  { : NGD Custom joint slider implementation. }
  TNGDCustomJointSlider = class(TNGDCustomJointBaseDir)
  private
    { Private Declarations }

  protected
    { Protected Declarations }
    procedure SetMaxLimit(val: Single); override;
    procedure SetMinLimit(val: Single); override;

  public
    { Public Declarations }
    constructor Create(AOwner: TXCollection); override;
    procedure StructureChanged(Sender: TObject); override;
    class function FriendlyName: string; override;
    class function FriendlyDescription: string; override;

  published
    { Published Declarations }
    property MinDist: Single read FMinLimit write SetMinLimit;
    property MaxDist: Single read FMaxLimit write SetMaxLimit;
  end;

  { Pure Newton Callback }
procedure NewtonBodyIterator(const body: PNewtonBody); cdecl;

function NewtonGetBuoyancyPlane(const collisionID: Integer; context: Pointer;
  const globalSpaceMatrix: PFloat; globalSpacePlane: PVector): Integer; cdecl;

procedure NewtonApplyForceAndTorque(const body: PNewtonBody; timestep: Float;
  threadIndex: Integer); cdecl;

procedure NewtonSetTransform(const body: PNewtonBody;
  const matrix: NewtonImport.PFloat; threadIndex: Integer); cdecl;

procedure NewtonBodyDestructor(const body: PNewtonBody); cdecl;

procedure NewtonCollisionIterator(const body: PNewtonBody;
  VertexCount: Integer; const FaceArray: PFloat; FaceId: Integer); cdecl;

procedure NewtonBodyLeaveWorld(const body: PNewtonBody; threadIndex: Integer);
  cdecl;

function NewtonOnAABBOverlap(const material: PNewtonMaterial;
  const body0: PNewtonBody; const body1: PNewtonBody;
  threadIndex: Integer): Integer; cdecl;

procedure NewtonContactsProcess(const contact: PNewtonJoint; timestep: Float;
  threadIndex: Integer); cdecl;

procedure NewtonSerialize(serializeHandle: Pointer; const buffer: Pointer;
  size: size_t); cdecl;

procedure NewtonDeserialize(serializeHandle: Pointer; buffer: Pointer;
  size: size_t); cdecl;

// GLNGDObject register methods (used for joint object persistence)
procedure RegisterGLSceneObject(anObject: TGLBaseSceneObject);
procedure UnregisterGLSceneObject(anObject: TGLBaseSceneObject);
function GetGLSceneObject(anObjectName: string): TGLBaseSceneObject;
function GetBodyFromGLSceneObject(anObject: TGLBaseSceneObject): PNewtonBody;

function GetNGDStatic(Obj: TGLBaseSceneObject): TGLNGDStatic;
function GetOrCreateNGDStatic(Obj: TGLBaseSceneObject): TGLNGDStatic;
function GetNGDDynamic(Obj: TGLBaseSceneObject): TGLNGDDynamic;
function GetOrCreateNGDDynamic(Obj: TGLBaseSceneObject): TGLNGDDynamic;

var
  vGLNGDObjectRegister: TList;

implementation

{ CallBack }

// API Callback wich iterate list of NewtonBody to apply special stuff
// Prefering iterate BehaviorsList from Manager
// Do nothing for the moment
// Called by manager with CallBodyIterator.
procedure NewtonBodyIterator(const body: PNewtonBody);
begin
  // NewtonBodySetFreezeState(body, 1);
end;

// API callback called by each body in 'NewtonApplyForceAndTorque' to set the
// waterplane equation. For the moment the plane is the same for everybody.
// This function could be used to create physics waves in the futur.
function NewtonGetBuoyancyPlane(const collisionID: Integer; context: Pointer;
  const globalSpaceMatrix: PFloat; globalSpacePlane: PVector): Integer;
var
  NGDManager: TGLNGDManager;
begin
  // the normal for the plane is just a unit vector.
  // the distance along this normal, to the origin. y0-ywater=-ywater
  // When the parameter buoyancyPlane is set to NULL, the body is considered
  // to completely immersed in the fluid. This can be used to simulate boats
  // and lighter than air vehicles etc..
  NGDManager := context;
  globalSpacePlane^ := (NGDManager.FWaterPlane.AsVector);
  Result := 1; // Boolean, 1 to apply Buoyancy, 0 to ignore
end;

// Called after Manager.Step, Runtime only
procedure NewtonApplyForceAndTorque(const body: PNewtonBody; timestep: Float;
  threadIndex: Integer);
var
  NGDDynamicBody: TGLNGDDynamic;
  Gravity: TVector;
  DensityCorrection, FluidDensity: Single;
  // forceout: TVector;
begin
  NGDDynamicBody := NewtonBodyGetUserData(body);

  // Read Only: We get the force and torque resulting from
  // every interaction on this body
  NewtonBodyGetForce(body, @(NGDDynamicBody.FAppliedForce.AsVector));
  NewtonBodyGetTorque(body, @(NGDDynamicBody.FAppliedTorque.AsVector));

  NewtonBodyGetVelocity(body, @(NGDDynamicBody.FAppliedVelocity.AsVector));
  NewtonBodyGetOmega(body, @(NGDDynamicBody.FAppliedOmega.AsVector));

  NewtonBodySetForce(body, @(NGDDynamicBody.Force.AsVector));
  NewtonBodySetTorque(body, @(NGDDynamicBody.Torque.AsVector));

  // Add Gravity from World
  Gravity := VectorScale(NGDDynamicBody.Manager.Gravity.AsVector,
    NGDDynamicBody.FMass);
  if NGDDynamicBody.FUseGravity then
    NewtonBodyAddForce(body, @(Gravity));

  // Add Buoyancy      RQ: Volume*Density=Mass
  if NGDDynamicBody.Manager.WaterDensity > 0 then
  begin
    DensityCorrection := 1;

    // Buoyancy for sphere does not work very well in Newton,
    // So we put a correction wich is a constante.
    // We have the same problem with the Capsule-Collision but the correction
    // depend of the height and radius.
    // Create ConvexHull {with capsule in glfeedback to a freeform}
    // to resolve this bug.
    if (NGDDynamicBody.OwnerBaseSceneObject is TGLSphere) then
      DensityCorrection := 1.43;

    FluidDensity := NGDDynamicBody.Manager.WaterDensity * DensityCorrection /
      NGDDynamicBody.FMass;
    NewtonBodyAddBuoyancyForce(body, FluidDensity, 0.5, 0.5, @(Gravity),
      @GLNGDManager.NewtonGetBuoyancyPlane, NGDDynamicBody.Manager);
  end;

  if NGDDynamicBody.FUseVelocity then
    NewtonBodySetVelocity(body, @(NGDDynamicBody.FVelocity.AsVector));
  { NewtonBodyCalculateInverseDynamicsForce(body, timestep,
    @(NGDDynamicBody.FVelocity.AsVector), @forceout);
    NewtonBodySetForce(body, @forceout); }

  if NGDDynamicBody.FUseOmega then
    NewtonBodySetOmega(body, @(NGDDynamicBody.FOmega.AsVector));

end;

// Runtime Only
procedure NewtonSetTransform(const body: PNewtonBody;
  const matrix: NewtonImport.PFloat; threadIndex: Integer);
var
  NGDDynamicBody: TGLNGDDynamic;
  ObjScale: TVector;
begin
  NGDDynamicBody := NewtonBodyGetUserData(body);

  // The Newton API does not support scale [scale modifie value in matrix],
  // so this line reset scale of the glsceneObject to (1,1,1)
  // to avoid crashing the application
  ObjScale := NGDDynamicBody.OwnerBaseSceneObject.Scale.AsVector;
  if (ObjScale[0] > 1.1) or (ObjScale[1] > 1.1) or (ObjScale[2] > 1.1) or
    (ObjScale[0] < 0.9) or (ObjScale[1] < 0.9) or (ObjScale[2] < 0.9) then
  begin
    NGDDynamicBody.OwnerBaseSceneObject.Scale.SetVector(1, 1, 1);
    NGDDynamicBody.SetNewtonBodyMatrix
      (NGDDynamicBody.OwnerBaseSceneObject.AbsoluteMatrix);
  end
  else
    // Make the Position and orientation of the glscene-Object relative to the
    // NewtonBody position and orientation.
    NGDDynamicBody.OwnerBaseSceneObject.AbsoluteMatrix := pMatrix(matrix)^;

end;

// This API Callback is set by default, but do nothing for the moment.
procedure NewtonBodyDestructor(const body: PNewtonBody);
begin
  //
end;

// The Manager use this CallBack from RenderEvent procedure
// in Runtime and design time
procedure NewtonCollisionIterator(const body: PNewtonBody;
  VertexCount: Integer; const FaceArray: PFloat; FaceId: Integer);
var
  I: Integer;
  v0, v1: array [0 .. 2] of Single;
  vA: array of Single;
begin
  // This algorithme draw Collision Shape for Debuggin.
  // Taken to Sascha Willems in SDLNewton-Demo at
  // http://www.saschawillems.de/?page_id=82
  if VertexCount = 0 then
    exit;
  SetLength(vA, VertexCount * 3);
  Move(FaceArray^, vA[0], VertexCount * 3 * SizeOf(Single));
  v0[0] := vA[(VertexCount - 1) * 3];
  v0[1] := vA[(VertexCount - 1) * 3 + 1];
  v0[2] := vA[(VertexCount - 1) * 3 + 2];
  for I := 0 to VertexCount - 1 do
  begin
    v1[0] := vA[I * 3];
    v1[1] := vA[I * 3 + 1];
    v1[2] := vA[I * 3 + 2];
    GL.Vertex3f(v0[0], v0[1], v0[2]);
    GL.Vertex3f(v1[0], v1[1], v1[2]);
    v0 := v1;
  end;
end;

// API Callback, When NewtonBody Leave the NewtonWorld
// [size of NewtonWorld Defined in manager]
// Do nothing for the moment
procedure NewtonBodyLeaveWorld(const body: PNewtonBody; threadIndex: Integer);
begin
  // When The body is leaving the world we change its freezestate to make
  // debuggin color more clear in mind.
  // But there is no way to remake the body active after this callback.
  // Another way would be to destroy body and behaviors and glscene-Object.
  // Or simply reset Body Position and orientation by using
  // NewtonBodySetMatrix API function before it leave the world.
  NewtonBodySetFreezeState(body, 1);
end;

// API Callback Runtime Only. Raise two even if the application want to
// apply special effect like conveyor...
procedure NewtonContactsProcess(const contact: PNewtonJoint; timestep: Float;
  threadIndex: Integer);
var
  NGDMaterialPair: TNGDMaterialPair;
  obj0, obj1: TGLBaseSceneObject;
  NGDBehaviour: TGLNGDBehaviour;
begin

  NGDMaterialPair := NewtonMaterialGetMaterialPairUserData
    (NewtonContactGetMaterial(NewtonContactJointGetFirstContact(contact)));

  // Raise material's event when two Bodies Collide
  with (NGDMaterialPair) do
  begin
    if Assigned(FContactProcessEvent) then
      FContactProcessEvent(NGDMaterialPair, contact, timestep)
  end;

  // Raise manager's event when two Bodies Collide
  NGDBehaviour := NewtonBodyGetUserData(NewtonJointGetBody0(contact));
  obj0 := NGDBehaviour.OwnerBaseSceneObject;
  NGDBehaviour := NewtonBodyGetUserData(NewtonJointGetBody1(contact));
  obj1 := NGDBehaviour.OwnerBaseSceneObject;
  with (NGDMaterialPair.FManager) do
  begin
    if Assigned(FMaterialHitEvent) then
      FMaterialHitEvent(obj0, obj1, NGDMaterialPair.Fid0, NGDMaterialPair.Fid1);
  end;
end;

// This API Callback is called juste before NewtonContactsProcess.
// This function can be used to get or set information of body and behaviors.
function NewtonOnAABBOverlap(const material: PNewtonMaterial;
  const body0: PNewtonBody; const body1: PNewtonBody;
  threadIndex: Integer): Integer;
begin
  // Boolean, if 1, continue
  // if 0, the two body won't collide and will go trough each other.
  Result := 1
end;

// Serializes are called by NGDBehaviour to save and load collision in file
// It's better to save/load big collisions [over 50000 polygones] to reduce
// loading time
procedure NewtonSerialize(serializeHandle: Pointer; const buffer: Pointer;
  size: size_t);
begin
  TFileStream(serializeHandle).write(buffer^, size);
end;

procedure NewtonDeserialize(serializeHandle: Pointer; buffer: Pointer;
  size: size_t);
begin
  TFileStream(serializeHandle).read(buffer^, size);
end;

{ Register }
// RegisterGLSceneObject
// Called when NGDBehaviors is created
procedure RegisterGLSceneObject(anObject: TGLBaseSceneObject);
begin
  if vGLNGDObjectRegister.IndexOf(anObject) = -1 then
    vGLNGDObjectRegister.Add(anObject);
end;

// UnregisterGLSceneObject
// Called when NGDBehaviors is destroyed
procedure UnregisterGLSceneObject(anObject: TGLBaseSceneObject);
begin
  vGLNGDObjectRegister.Remove(anObject);
end;

// GetGLSceneObject
// Use the list of registered glscene-Object
function GetGLSceneObject(anObjectName: string): TGLBaseSceneObject;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to vGLNGDObjectRegister.Count - 1 do
    if TGLBaseSceneObject(vGLNGDObjectRegister[I])
      .GetNamePath = anObjectName then
    begin
      Result := vGLNGDObjectRegister[I];
      exit;
    end;
end;

// GetBodyFromGLSceneObject
// Useful for NGDjoint who need FNewtonBody, a private pointer.
function GetBodyFromGLSceneObject(anObject: TGLBaseSceneObject): PNewtonBody;
var
  temp: TGLNGDBehaviour;
begin
  Result := nil;
  if Assigned(anObject) then
  begin
    temp := TGLNGDBehaviour(anObject.Behaviours.GetByClass(TGLNGDBehaviour));
    if temp <> nil then
      Result := temp.FNewtonBody;
  end;
end;

// GetNGDStatic
//
function GetNGDStatic(Obj: TGLBaseSceneObject): TGLNGDStatic;
begin
  Result := TGLNGDStatic(Obj.Behaviours.GetByClass(TGLNGDStatic));
end;

// GetOrCreateNGDStatic
//
function GetOrCreateNGDStatic(Obj: TGLBaseSceneObject): TGLNGDStatic;
begin
  Result := TGLNGDStatic(Obj.GetOrCreateBehaviour(TGLNGDStatic));
end;

// GetNGDDynamic
//
function GetNGDDynamic(Obj: TGLBaseSceneObject): TGLNGDDynamic;
begin
  Result := TGLNGDDynamic(Obj.Behaviours.GetByClass(TGLNGDDynamic));
end;

// GetOrCreateNGDDynamic
//
function GetOrCreateNGDDynamic(Obj: TGLBaseSceneObject): TGLNGDDynamic;
begin
  Result := TGLNGDDynamic(Obj.GetOrCreateBehaviour(TGLNGDDynamic));
end;


// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

{ TGLNGDManager }

constructor TGLNGDManager.Create(AOwner: TComponent);
var
  minworld, maxworld: TVector;
begin
  inherited;
  FRenderPoint := nil;
  FBitmapFont := nil;
  FNGDBehaviours := TPersistentObjectList.Create;
  FVisible := True;
  FVisibleAtRunTime := False;
  FMaterials := TNGDMaterials.Create(self);
  FMaxMaterialID := 0;
  FSolverModel := smExact;
  FFrictionModel := fmExact;
  FMinimumFrameRate := 60;
  FWorldSizeMin := TGLCoordinates.CreateInitialized(self,
    VectorMake(-100, -100, -100, 0), csPoint);
  FWorldSizeMax := TGLCoordinates.CreateInitialized(self,
    VectorMake(100, 100, 100, 0), csPoint);

  // Using Events because we need to call API Function when
  // theses TGLCoordinates change.
  FWorldSizeMin.OnNotifyChange := NotifyWorldSizeChange;
  FWorldSizeMax.OnNotifyChange := NotifyWorldSizeChange;

  FThreadCount := 1;
  FGravity := TGLCoordinates3.CreateInitialized(self,
    VectorMake(0, -9.81, 0, 0), csVector);
  FGravity.OnNotifyChange := NotifyGravityChange;
  FWaterDensity := 0;
  FWaterPlane := TGLCoordinates4.CreateInitialized(self,
    VectorMake(0, 1, 0, 0), csUnknown);
  FWaterPlane.OnNotifyChange := NotifyWaterPlaneChange;
  FNewtonWorld := NewtonCreate(nil, nil);
  FVersion := NewtonWorldGetVersion(FNewtonWorld);

  // This is to prevent body out the world at startTime
  minworld := VectorMake(-1E50, -1E50, -1E50);
  maxworld := VectorMake(1E50, 1E50, 1E50);
  NewtonSetWorldSize(FNewtonWorld, @minworld, @maxworld);

  NewtonSetBodyLeaveWorldEvent(FNewtonWorld, @NewtonBodyLeaveWorld);
  NewtonWorldSetUserData(FNewtonWorld, self);
  FMaterialHitEvent := nil;

  // For Debuggin
  FShowGeometry := False;
  FGeomColorDyn := TGLColor.CreateInitialized(self, clrGreen, NotifyChange);
  FGeomColorStat := TGLColor.CreateInitialized(self, clrRed, NotifyChange);
  FShowAABB := False;
  FAABBColor := TGLColor.CreateInitialized(self, clrYellow, NotifyChange);
  FAABBColorSleep := TGLColor.CreateInitialized(self, clrOrange, NotifyChange);
  FShowMaterialESP := False;
  FMaterialESPColor := TGLColor.CreateInitialized(self, clrPurple,
    NotifyChange);
  FShowContact := False;
  FContactColor := TGLColor.CreateInitialized(self, clrWhite, NotifyChange);
  FShowJoint := False;
  FJointColor := TGLColor.CreateInitialized(self, clrBlue, NotifyChange);
  FShowSpring := False;
  FSpringColor := TGLColor.CreateInitialized(self, clrAqua, NotifyChange);

  RegisterManager(self);

end;

destructor TGLNGDManager.Destroy;
begin
  RenderPoint := nil;
  BitmapFont := nil;

  // Unregister everything
  while FNGDBehaviours.Count > 0 do
    NGDBehaviours[0].Manager := nil;

  // Clean up everything
  FMaterials.Free;
  FNGDBehaviours.Free;
  FWorldSizeMin.Free;
  FWorldSizeMax.Free;
  FGravity.Free;
  FWaterPlane.Free;

  FGeomColorDyn.Free;
  FGeomColorStat.Free;
  FAABBColor.Free;
  FAABBColorSleep.Free;
  FMaterialESPColor.Free;
  FContactColor.Free;
  FJointColor.Free;
  FSpringColor.Free;

  // We don't need to destroy NewtonJoint because joints attached to bodies
  // are deleted when bodies are deleted.
  NewtonDestroyAllBodies(FNewtonWorld);
  NewtonSetBodyLeaveWorldEvent(FNewtonWorld, nil);
  NewtonMaterialDestroyAllGroupID(FNewtonWorld);
  NewtonDestroy(FNewtonWorld);
  FNewtonWorld := nil;

  DeregisterManager(self);
  inherited;
end;

procedure TGLNGDManager.Loaded;
var
  I: Integer;
begin
  inherited;
  NotifyWorldSizeChange(self);
  // NotifyGravityChange(self);
  // NotifyWaterPlaneChange(self);

  for I := 0 to FMaterials.Count - 1 do
  begin
    FMaterials[I].Loaded;
  end;
end;

procedure TGLNGDManager.MaterialAutoCreateGroupID(MaterialID: Integer);
var
  I: Integer;
begin
  // Create GroupID
  for I := FMaxMaterialID to MaterialID - 1 do
    MaterialCreateGroupID;

  FMaxMaterialID := MaxInteger(FMaxMaterialID, MaterialID);
end;

procedure TGLNGDManager.ReadMaterials(stream: TStream);
var
  reader: TReader;
begin
  reader := TReader.Create(stream, 16384);
  try
    Materials.ReadFromFiler(reader);
  finally
    reader.Free;
  end;
end;

procedure TGLNGDManager.WriteMaterials(stream: TStream);
var
  writer: TWriter;
begin
  writer := TWriter.Create(stream, 16384);
  try
    Materials.WriteToFiler(writer);
  finally
    writer.Free;
  end;
end;

procedure TGLNGDManager.DefineProperties(Filer: TFiler);
begin
  inherited;
  Filer.DefineBinaryProperty('NGDMaterialsData', ReadMaterials, WriteMaterials,
    (Assigned(FMaterials) and (FMaterials.Count > 0)));
end;

function TGLNGDManager.GetBodyCount: Integer;
begin
  if Assigned(FNewtonWorld) then
    Result := NewtonWorldGetBodyCount(FNewtonWorld)
  else
    Result := FNGDBehaviours.Count;
end;

function TGLNGDManager.GetConstraintCount: Integer;
begin
  if Assigned(FNewtonWorld) then
    Result := NewtonWorldGetConstraintCount(FNewtonWorld)
    // Constraint is the number of joint
  else
    Result := 0;
end;

function TGLNGDManager.GetNGDBehaviour(index: Integer): TGLNGDBehaviour;
begin
  Result := TGLNGDBehaviour(FNGDBehaviours[index]);
end;

procedure TGLNGDManager.CallBodyIterator;
begin
  if Assigned(FNewtonWorld) then
    NewtonWorldForEachBodyInAABBDo(FNewtonWorld, @(FWorldSizeMin.AsVector),
      @(FWorldSizeMax.AsVector), @NewtonBodyIterator, nil);
end;

function TGLNGDManager.MaterialCreateGroupID: Integer;
begin
  if Assigned(FNewtonWorld) then
    Result := NewtonMaterialCreateGroupID(FNewtonWorld)
  else
    Result := 0;
end;

procedure TGLNGDManager.NotifyChange(Sender: TObject);
begin
  if Assigned(RenderPoint) then
    RenderPoint.StructureChanged;
end;

procedure TGLNGDManager.RegisterNGDBehaviour(NGDBehaviour: TGLNGDBehaviour);
begin
  FNGDBehaviours.Add(NGDBehaviour);
  NotifyChange(self);
end;

procedure TGLNGDManager.UnregisterNGDBehaviour(NGDBehaviour: TGLNGDBehaviour);
begin
  FNGDBehaviours.Remove(NGDBehaviour);
  NotifyChange(self);
end;

procedure TGLNGDManager.RenderEvent(Sender: TObject;
  var rci: TRenderContextInfo);
var
  I: Integer;
begin
  if not Visible then
    exit;
  if not(csDesigning in ComponentState) then
    if not VisibleAtRunTime then
      exit;

  rci.GLStates.ActiveTextureEnabled[ttTexture2D] := False;
  rci.GLStates.Disable(stLighting);
  rci.GLStates.Enable(stLineStipple);

  // Equivalent to ForEachBodyINAABB
  for I := 0 to FNGDBehaviours.Count - 1 do
  begin
    if NewtonBodyGetSleepState(NGDBehaviours[I].FNewtonBody) = 1 then
      GL.Color4fv(GeomColorStat.AsAddress) // red
    else
      GL.Color4fv(GeomColorDyn.AsAddress); // green

    if (csDesigning in ComponentState) then
    begin
      if (NGDBehaviours[I] is TGLNGDStatic) then
        GL.Color4fv(GeomColorStat.AsAddress); // red

      if (NGDBehaviours[I] is TGLNGDDynamic) then
        if (NGDBehaviours[I] as TGLNGDDynamic).FDensity = 0 then
          GL.Color4fv(GeomColorStat.AsAddress); // red
    end;

    NGDBehaviours[I].Render(rci);
  end;
end;

procedure TGLNGDManager.RenderPointFreed(Sender: TObject);
begin
  FRenderPoint := nil;
end;

procedure TGLNGDManager.SetBitmapFont(const value: TGLCustomBitmapFont);
begin
  if FBitmapFont <> value then
  begin
    FBitmapFont := value;
    NotifyChange(self);
  end;
end;

procedure TGLNGDManager.SetFrictionModel(val: TNGDFrictionModels);
begin
  if Assigned(FNewtonWorld) then
    NewtonSetFrictionModel(FNewtonWorld, Ord(val));
  FFrictionModel := val;
end;

procedure TGLNGDManager.NotifyGravityChange(Sender: TObject);
begin
  // Nothing here for the moment
end;

procedure TGLNGDManager.SetMinimumFrameRate(val: Integer);
begin
  if (val >= 60) and (val <= 1000) then
  begin
    NewtonSetMinimumFrameRate(FNewtonWorld, val);
    FMinimumFrameRate := val;
  end;
end;

procedure TGLNGDManager.SetRenderPoint(const value: TGLRenderPoint);
begin
  if FRenderPoint <> value then
  begin
    if Assigned(FRenderPoint) then
      FRenderPoint.UnRegisterCallBack(RenderEvent);
    FRenderPoint := value;
    if Assigned(FRenderPoint) then
      FRenderPoint.RegisterCallBack(RenderEvent, RenderPointFreed);
    NotifyChange(self);
  end;
end;

procedure TGLNGDManager.SetShowAABB(const value: Boolean);
begin
  if value <> FShowAABB then
  begin
    FShowAABB := value;
    NotifyChange(self);
  end;
end;

procedure TGLNGDManager.SetShowContact(const value: Boolean);
begin
  if value <> FShowContact then
  begin
    FShowContact := value;
    NotifyChange(self);
  end;
end;

procedure TGLNGDManager.SetShowGeometry(const value: Boolean);
begin
  if value <> FShowGeometry then
  begin
    FShowGeometry := value;
    NotifyChange(self);
  end;
end;

procedure TGLNGDManager.SetShowJoint(const value: Boolean);
begin
  if value <> FShowJoint then
  begin
    FShowJoint := value;
    NotifyChange(self);
  end;
end;

procedure TGLNGDManager.SetShowMaterialESP(const value: Boolean);
begin
  if value <> FShowMaterialESP then
  begin
    FShowMaterialESP := value;
    NotifyChange(self);
  end;
end;

procedure TGLNGDManager.SetShowSpring(const value: Boolean);
begin
  if value <> FShowSpring then
  begin
    FShowSpring := value;
    NotifyChange(self);
  end;
end;

procedure TGLNGDManager.SetSolverModel(val: TNGDSolverModels);
begin
  if Assigned(FNewtonWorld) then
    NewtonSetSolverModel(FNewtonWorld, Ord(val));
  FSolverModel := val;
end;

procedure TGLNGDManager.SetThreadCount(val: Integer);
begin
  if val > 0 then
  begin
    if Assigned(FNewtonWorld) then
      NewtonSetThreadsCount(FNewtonWorld, val);
    FThreadCount := val;
  end;
end;

procedure TGLNGDManager.SetVisible(const value: Boolean);
begin
  if value <> FVisible then
  begin
    FVisible := value;
    NotifyChange(self);
  end;
end;

procedure TGLNGDManager.SetVisibleAtRunTime(const value: Boolean);
begin
  if value <> FVisibleAtRunTime then
  begin
    FVisibleAtRunTime := value;
    NotifyChange(self);
  end;
end;

procedure TGLNGDManager.SetWaterDensity(val: Single);
begin
  if val < 0 then
    FWaterDensity := 0
  else
    FWaterDensity := val;
end;

procedure TGLNGDManager.NotifyWaterPlaneChange(Sender: TObject);
var
  W: Single;
begin
  FWaterPlane.OnNotifyChange := nil;
  W := FWaterPlane.W;
  FWaterPlane.Normalize;
  FWaterPlane.W := W;
  FWaterPlane.OnNotifyChange := NotifyWaterPlaneChange;
end;

procedure TGLNGDManager.NotifyWorldSizeChange(Sender: TObject);
begin
  if Assigned(FNewtonWorld) then
    NewtonSetWorldSize(FNewtonWorld, @FWorldSizeMin.AsVector,
      @FWorldSizeMax.AsVector);
end;

procedure TGLNGDManager.Step(deltatime: Single);
begin
  if Assigned(FNewtonWorld) then
    NewtonUpdate(FNewtonWorld, deltatime);
end;

{ TGLNGDBehaviour }

constructor TGLNGDBehaviour.Create(AOwner: TXCollection);
begin
  inherited;
  FInitialized := False;
  FOwnerBaseSceneObject := OwnerBaseSceneObject;
  if Assigned(FOwnerBaseSceneObject) then
    RegisterGLSceneObject(OwnerBaseSceneObject);
  FJointRegister := TList.Create;
  FContinuousCollisionMode := False;
  FMaterialID := 0;
  FBoundingSphereRadius := OwnerBaseSceneObject.BoundingSphereRadius;
  FNewtonBody := nil;
  FCollision := nil;
end;

destructor TGLNGDBehaviour.Destroy;
var
  I: Integer;
begin

  for I := FJointRegister.Count - 1 downto 0 do
    TNGDJointBase(FJointRegister[I]).Free;

  if Assigned(FManager) then
    Manager := nil;
  if Assigned(FOwnerBaseSceneObject) then
    UnregisterGLSceneObject(FOwnerBaseSceneObject);
  FJointRegister.Free;
  inherited;
end;

procedure TGLNGDBehaviour.Finalize;
var
  I: Integer;
begin
  FInitialized := False;

  for I := FJointRegister.Count - 1 downto 0 do
    TNGDJointBase(FJointRegister[I]).Finalize;

  if Assigned(FManager) then
  begin
    NewtonBodySetDestructorCallback(FNewtonBody, nil);
    NewtonDestroyBody(FManager.FNewtonWorld, FNewtonBody);
    FNewtonBody := nil;
    FCollision := nil;
  end;
end;

function TGLNGDBehaviour.GetCollisionFromBaseSceneObject
  (SceneObject: TGLBaseSceneObject): PNewtonCollision;
var
  FCollisionOffsetMatrix: TMatrix; // For cone capsule and cylinder

begin
  FCollisionOffsetMatrix := IdentityHmgMatrix;

  if (SceneObject is TGLCube) then
  begin
    with (SceneObject as TGLCube) do
      Result := NewtonCreateBox(FManager.FNewtonWorld, CubeWidth, CubeHeight,
        CubeDepth, 0, @FCollisionOffsetMatrix);
  end

  else if (SceneObject is TGLSphere) then
  begin
    with (SceneObject as TGLSphere) do
      Result := NewtonCreateSphere(FManager.FNewtonWorld, radius, radius,
        radius, 0, @FCollisionOffsetMatrix);
  end

  else if (SceneObject is TGLCone) then
  begin
    FCollisionOffsetMatrix := MatrixMultiply(FCollisionOffsetMatrix,
      CreateRotationMatrixZ(DegToRad(90)));
    with (SceneObject as TGLCone) do
      Result := NewtonCreateCone(FManager.FNewtonWorld, BottomRadius, Height,
        0, @FCollisionOffsetMatrix);
  end

  else if (SceneObject is TGLCapsule) then
  begin
    FCollisionOffsetMatrix := MatrixMultiply(FCollisionOffsetMatrix,
      CreateRotationMatrixY(DegToRad(90)));
    with (SceneObject as TGLCapsule) do
      // Use Cylinder shape for buoyancy
      Result := NewtonCreateCapsule(FManager.FNewtonWorld, radius,
        Height + 2 * radius, 0, @FCollisionOffsetMatrix);
  end

  else if (SceneObject is TGLCylinder) then
  begin
    FCollisionOffsetMatrix := MatrixMultiply(FCollisionOffsetMatrix,
      CreateRotationMatrixZ(DegToRad(90)));
    with (SceneObject as TGLCylinder) do
      Result := NewtonCreateCylinder(FManager.FNewtonWorld, BottomRadius,
        Height, 0, @FCollisionOffsetMatrix);
  end

  else if (SceneObject is TGLFreeForm) and
    ((SceneObject as TGLFreeForm).MeshObjects.Count > 0) then
  begin
    Result := nil;
  end

  else
  begin
    Result := NewtonCreateNull(FManager.FNewtonWorld);
  end;
end;

function TGLNGDBehaviour.GetNewtonBodyMatrix: TMatrix;
begin
  if Assigned(FManager) then
    NewtonBodyGetmatrix(FNewtonBody, @FNewtonBodyMatrix);
  Result := FNewtonBodyMatrix;
end;

procedure TGLNGDBehaviour.Initialize;
var
  I: Integer;
begin
  FInitialized := True;

  for I := 0 to FJointRegister.Count - 1 do
    TNGDJointBase(FJointRegister[I]).StructureChanged(self);

  if Assigned(FManager) then
  begin
    // Create NewtonBody with null collision
    FCollision := NewtonCreateNull(FManager.FNewtonWorld);
    FNewtonBody := NewtonCreateBody(FManager.FNewtonWorld, FCollision);

    // Release NewtonCollision
    NewtonReleaseCollision(FManager.FNewtonWorld, FCollision);

    // Set Appropriate NewtonCollision
    SetCollision();

    // Set Link between glscene and newton
    NewtonBodySetUserdata(FNewtonBody, self);

    // Set position and orientation
    SetNewtonBodyMatrix(OwnerBaseSceneObject.AbsoluteMatrix);

    // Save BoundingSphereRadius
    FBoundingSphereRadius := OwnerBaseSceneObject.BoundingSphereRadius;

    // Set Callback
    NewtonBodySetDestructorCallback(FNewtonBody, @NewtonBodyDestructor);
  end;
end;

procedure TGLNGDBehaviour.Loaded;
var
  mng: TComponent;
begin
  inherited;
  if FManagerName <> '' then
  begin
    mng := FindManager(TGLNGDManager, FManagerName);
    if Assigned(mng) then
      Manager := TGLNGDManager(mng);
    FManagerName := '';
  end;

  if Assigned(FManager) then
  begin
    SetContinuousCollisionMode(FContinuousCollisionMode);
    SetMaterialID(FMaterialID);
  end;
end;

procedure TGLNGDBehaviour.NotifyChange(Sender: TObject);
begin
  if Assigned(FManager) then
    Manager.NotifyChange(self);
end;

procedure TGLNGDBehaviour.Reinitialize;
begin
  if Initialized then
  begin
    // Set Appropriate NewtonCollision
    SetCollision();
    // Set position and orientation
    SetNewtonBodyMatrix(OwnerBaseSceneObject.AbsoluteMatrix);
    // Save BoundingSphereRadius
    FBoundingSphereRadius := OwnerBaseSceneObject.BoundingSphereRadius;
  end;
  Loaded;
end;

procedure TGLNGDBehaviour.Render(var rci: TRenderContextInfo);
var
  NGDJointBase: TNGDJointBase;
  I: Integer;
  bar: TVector;
  f: Single;
  ESPText: string;
begin
  if (csDesigning in Manager.ComponentState) then
  begin
    // Move/Rotate NewtonObject if matrix are not equal in design time.
    if not MatrixEquals(NewtonBodyMatrix, OwnerBaseSceneObject.AbsoluteMatrix)
      then
      SetNewtonBodyMatrix(OwnerBaseSceneObject.AbsoluteMatrix);

    // Rebuild Collision if Radius has been modified
    if FBoundingSphereRadius <> OwnerBaseSceneObject.BoundingSphereRadius then
    begin

      // Exit if freeform, no need rebuild in design time
      if OwnerBaseSceneObject is TGLFreeForm then
        exit;

      Reinitialize;

    end;
  end;

  if Manager.ShowGeometry then
  begin
    rci.GLStates.LineWidth := 1;
    rci.GLStates.LineStippleFactor := 1;
    rci.GLStates.LineStipplePattern := $FFFF;
    // full opengl lines
    GL.Begin_(GL_LINES);
    NewtonBodyMatrix;
    NewtonCollisionForEachPolygonDo(FCollision, @(FNewtonBodyMatrix),
      @NewtonCollisionIterator, nil);
    GL.End_;
  end;

  if Manager.ShowJoint then
  begin
    rci.GLStates.LineWidth := 5;
    rci.GLStates.LineStippleFactor := 1;
    rci.GLStates.LineStipplePattern := $00FF;
    // dot style opengl lines
    GL.Begin_(GL_LINES);
    GL.Color4fv(FManager.JointColor.AsAddress); // Blue
    for I := 0 to FJointRegister.Count - 1 do
    begin
      NGDJointBase := FJointRegister.Items[I];
      NGDJointBase.Render;
    end;
    GL.End_;
  end;

  if Manager.ShowMaterialESP and Assigned(FManager.BitmapFont) then
  begin
    // Render a string in 3d world at body position
    bar := OwnerBaseSceneObject.BarycenterAbsolutePosition;

    GL.PushMatrix;

    f := rci.renderDPI / 96;
    GL.LoadMatrixf(@TGLSceneBuffer(rci.buffer).BaseProjectionMatrix);
    bar := (rci.buffer as TGLSceneBuffer).WorldToScreen(bar);
    GL.Scalef(2 / rci.viewPortSize.cx, 2 / rci.viewPortSize.cy, 1);
    GL.Translatef(bar[0] * f - rci.viewPortSize.cx / 2,
      +bar[1] * f - rci.viewPortSize.cy / 2, -bar[2]);

    GL.MatrixMode(GL_PROJECTION);
    GL.PushMatrix;
    GL.LoadIdentity;

    ESPText := 'ID:=' + IntToStr(FMaterialID);

    rci.GLStates.Disable(stDepthTest);
    if bar[2] < 1 then
      Manager.BitmapFont.RenderString(rci, ESPText, taCenter, tlCenter,
        Manager.MaterialESPColor.Color);
    rci.GLStates.Enable(stDepthTest);

    GL.PopMatrix;
    GL.MatrixMode(GL_MODELVIEW);
    GL.PopMatrix;
  end;
end;

// In this procedure, we assign collision to body
// [Because when initialised, the collision for body is type NULL]
procedure TGLNGDBehaviour.SetCollision;
var
  CollisionInfoRecord: TNewtonCollisionInfoRecord;
  // k: Integer;
begin
  if Assigned(FCollision) then
  begin
    NewtonBodySetCollision(FNewtonBody, FCollision);

    // The API Ask for releasing Collision to avoid memory leak
    NewtonCollisionGetInfo(FCollision, @CollisionInfoRecord);
    if CollisionInfoRecord.m_referenceCount > 2 then
      NewtonReleaseCollision(FManager.FNewtonWorld, FCollision);

    // Release each collision form the array
    {
      Need to be moved after creating a compound
      for k := 0 to Length(FCollisionArray) - 1 do
      begin
      if FCollisionArray[k] <> nil then
      begin
      NewtonCollisionGetInfo(FCollisionArray[k], @CollisionInfoRecord);
      if CollisionInfoRecord.m_referenceCount > 2 then
      NewtonReleaseCollision(FManager.FNewtonWorld, FCollisionArray[k]);
      end;
      end;
      }

  end;
end;

procedure TGLNGDBehaviour.SetContinuousCollisionMode(val: Boolean);
begin
  if Assigned(FManager) then
    NewtonBodySetContinuousCollisionMode(FNewtonBody, Ord(val));
  FContinuousCollisionMode := val;
end;

procedure TGLNGDBehaviour.SetManager(value: TGLNGDManager);
begin
  if FManager <> value then
  begin
    if Assigned(FManager) then
    begin
      if Initialized then
        Finalize;
      FManager.UnregisterNGDBehaviour(self);
    end;
    FManager := value;
    if Assigned(FManager) then
    begin
      // Commented to allow debug in design time
      // if not(csDesigning in TComponent(Owner.Owner).ComponentState) then
      Initialize;
      FManager.RegisterNGDBehaviour(self);
    end;
  end;
end;

procedure TGLNGDBehaviour.SetMaterialID(val: Integer);
begin
  FMaterialID := val;
  if Assigned(FManager) then
  begin
    FManager.MaterialAutoCreateGroupID(FMaterialID);
    NewtonBodySetMaterialGroupID(FNewtonBody, FMaterialID);
    FManager.NotifyChange(self);
  end;

end;

procedure TGLNGDBehaviour.SetNewtonBodyMatrix(val: TMatrix);
begin
  if Assigned(FManager) then
    NewtonBodySetmatrix(FNewtonBody, @val);
  FNewtonBodyMatrix := val;
end;

procedure TGLNGDBehaviour.RegisterJoint(Joint: TNGDJointBase);
begin
  if FJointRegister.IndexOf(Joint) = -1 then
    FJointRegister.Add(Joint);
end;

class function TGLNGDBehaviour.UniqueItem: Boolean;
begin
  Result := True;
end;

procedure TGLNGDBehaviour.UnregisterJoint(Joint: TNGDJointBase);
begin
  if FJointRegister.IndexOf(Joint) > -1 then
    FJointRegister.Remove(Joint);
end;

procedure TGLNGDBehaviour.ReadFromFiler(reader: TReader);
begin
  inherited;
  with reader do
  begin
    Assert(ReadInteger = 0);
    // Archive version
    FManagerName := ReadString;
    FContinuousCollisionMode := ReadBoolean;
    FMaterialID := ReadInteger;
  end;
end;

procedure TGLNGDBehaviour.WriteToFiler(writer: TWriter);
begin
  inherited;
  with writer do
  begin
    WriteInteger(0);
    // Archive version
    if Assigned(FManager) then
      WriteString(FManager.GetNamePath)
    else
      WriteString('');
    WriteBoolean(FContinuousCollisionMode);
    WriteInteger(FMaterialID);
  end;
end;

procedure TGLNGDBehaviour.Serialize(filename: string);
var
  MyFile: TFileStream;
begin
  MyFile := TFileStream.Create(filename, fmCreate or fmOpenReadWrite);

  NewtonCollisionSerialize(FManager.FNewtonWorld, FCollision, @NewtonSerialize,
    Pointer(MyFile));

  MyFile.Free;
end;

procedure TGLNGDBehaviour.DeSerialize(filename: string);
var
  MyFile: TFileStream;
  CollisionInfoRecord: TNewtonCollisionInfoRecord;
begin
  MyFile := TFileStream.Create(filename, fmOpenRead);

  FCollision := NewtonCreateCollisionFromSerialization(FManager.FNewtonWorld,
    @NewtonDeserialize, Pointer(MyFile));

  // SetCollision;
  NewtonBodySetCollision(FNewtonBody, FCollision);

  // Release collision
  NewtonCollisionGetInfo(FCollision, @CollisionInfoRecord);
  if CollisionInfoRecord.m_referenceCount > 2 then
    NewtonReleaseCollision(FManager.FNewtonWorld, FCollision);

  MyFile.Free;
end;

{ TGLNGDDynamic }

{ function TGLNGDDynamic.car(maxTireCount: Integer;
  const cordenateSytemInLocalSpace: TMatrix): PNewtonUserJoint;
  begin
  Result := DGRaycastVehicleCreate(maxTireCount, @cordenateSytemInLocalSpace,
  FNewtonBody);
  end; }

procedure TGLNGDDynamic.AddImpulse(veloc, pointposit: TVector);
begin
  NewtonBodyAddImpulse(FNewtonBody, @veloc, @pointposit);
end;

constructor TGLNGDDynamic.Create(AOwner: TXCollection);
begin
  inherited;
  FAutoSleep := True;
  FLinearDamping := 0.1;
  FAngularDamping := TGLCoordinates.CreateInitialized(self,
    VectorMake(0.1, 0.1, 0.1, 0), csPoint);
  FAngularDamping.OnNotifyChange := NotifyAngularDampingChange;
  FDensity := 1;
  FVolume := 1;
  FForce := TGLCoordinates.CreateInitialized(self, NullHmgVector, csVector);
  FTorque := TGLCoordinates.CreateInitialized(self, NullHmgVector, csVector);
  FVelocity := TGLCoordinates.CreateInitialized(self, NullHmgVector, csVector);
  FOmega := TGLCoordinates.CreateInitialized(self, NullHmgVector, csVector);
  FCenterOfMass := TGLCoordinates.CreateInitialized(self, NullHmgVector,
    csPoint);
  FCenterOfMass.OnNotifyChange := NotifyCenterOfMassChange;
  FAABBmin := TGLCoordinates.CreateInitialized(self, NullHmgVector, csPoint);
  FAABBmax := TGLCoordinates.CreateInitialized(self, NullHmgVector, csPoint);
  FAppliedOmega := TGLCoordinates.CreateInitialized(self, NullHmgVector,
    csVector);
  FAppliedVelocity := TGLCoordinates.CreateInitialized(self, NullHmgVector,
    csVector);
  FAppliedForce := TGLCoordinates.CreateInitialized(self, NullHmgVector,
    csVector);
  FAppliedTorque := TGLCoordinates.CreateInitialized(self, NullHmgVector,
    csVector);
  FUpVectorDirection := TGLCoordinates.CreateInitialized(self, YHmgVector,
    csVector);
  FUpVectorDirection.OnNotifyChange := NotifyUpVectorDirectionChange;
  FUseVelocity := False;
  FUseOmega := False;
  FUseGravity := True;

  FNewtonUserJoint := nil;
  FNewtonJoint := nil;
end;

destructor TGLNGDDynamic.Destroy;
begin
  // Clean up everything
  FForce.Free;
  FTorque.Free;
  FCenterOfMass.Free;
  FAABBmin.Free;
  FAABBmax.Free;
  FOmega.Free;
  FVelocity.Free;
  FAppliedForce.Free;
  FAppliedTorque.Free;
  FAppliedVelocity.Free;
  FAppliedOmega.Free;
  FUpVectorDirection.Free;
  FNewtonUserJoint := nil;
  FNewtonJoint := nil;
  inherited;
end;

procedure TGLNGDDynamic.Finalize;
begin
  if Assigned(FManager) then
  begin
    // Removing Callback
    NewtonBodySetForceAndTorqueCallback(FNewtonBody, nil);
    NewtonBodySetTransformCallback(FNewtonBody, nil);
  end;
  inherited;
end;

class function TGLNGDDynamic.FriendlyName: string;
begin
  Result := 'NGD Dynamic';
end;

procedure TGLNGDDynamic.Initialize;
begin
  inherited;
  if Assigned(FManager) then
  begin
    // Set Density, Mass and inertie matrix
    SetDensity(FDensity);

    // Set Callback
    NewtonBodySetForceAndTorqueCallback(FNewtonBody,
      @NewtonApplyForceAndTorque);
    NewtonBodySetTransformCallback(FNewtonBody, @NewtonSetTransform);
  end;
end;

procedure TGLNGDDynamic.Render(var rci: TRenderContextInfo);
var
  Barycenter, pickpoint: TVector;
  PickedMatrix: TMatrix;

  procedure DrawAABB(min, max: TGLCoordinates3);
  begin

    {
      //    H________G
      //   /.       /|
      //  / .      / |
      // D__._____C  |
      // |  .     |  |
      // | E.-----|--F
      // | .      | /
      // |.       |/
      // A________B
      }
    // Back
    GL.Vertex3f(min.x, min.y, min.z); // E
    GL.Vertex3f(max.x, min.y, min.z); // F

    GL.Vertex3f(max.x, min.y, min.z); // F
    GL.Vertex3f(max.x, max.y, min.z); // G

    GL.Vertex3f(max.x, max.y, min.z); // G
    GL.Vertex3f(min.x, max.y, min.z); // H

    GL.Vertex3f(min.x, max.y, min.z); // H
    GL.Vertex3f(min.x, min.y, min.z); // E

    // Front
    GL.Vertex3f(min.x, min.y, max.z); // A
    GL.Vertex3f(max.x, min.y, max.z); // B

    GL.Vertex3f(max.x, min.y, max.z); // B
    GL.Vertex3f(max.x, max.y, max.z); // C

    GL.Vertex3f(max.x, max.y, max.z); // C
    GL.Vertex3f(min.x, max.y, max.z); // D

    GL.Vertex3f(min.x, max.y, max.z); // D
    GL.Vertex3f(min.x, min.y, max.z); // A

    // Edges
    GL.Vertex3f(min.x, min.y, max.z); // A
    GL.Vertex3f(min.x, min.y, min.z); // E

    GL.Vertex3f(max.x, min.y, max.z); // B
    GL.Vertex3f(max.x, min.y, min.z); // F

    GL.Vertex3f(max.x, max.y, max.z); // C
    GL.Vertex3f(max.x, max.y, min.z); // G

    GL.Vertex3f(min.x, max.y, max.z); // D
    GL.Vertex3f(min.x, max.y, min.z); // H
  end;

  procedure DrawContact;
  var
    cnt: PNewtonJoint;
    ThisContact: PNewtonJoint;
    material: PNewtonMaterial;
    pos, nor: TVector;
  begin
    cnt := NewtonBodyGetFirstContactJoint(FNewtonBody);
    while cnt <> nil do
    begin
      ThisContact := NewtonContactJointGetFirstContact(cnt);
      while ThisContact <> nil do
      begin
        material := NewtonContactGetMaterial(ThisContact);
        NewtonMaterialGetContactPositionAndNormal(material, @pos, @nor);

        GL.Vertex3fv(@pos);
        nor := VectorAdd(pos, nor);
        GL.Vertex3fv(@nor);

        ThisContact := NewtonContactJointGetNextContact(cnt, ThisContact);
      end;
      cnt := NewtonBodyGetNextContactJoint(FNewtonBody, cnt);
    end;
  end;

begin
  inherited;

  // Exit if freeform, no need debug in design time
  if (csDesigning in Manager.ComponentState) and
    (OwnerBaseSceneObject is TGLFreeForm) then
    exit;

  if Manager.ShowAABB then
  begin
    rci.GLStates.LineWidth := 1;
    rci.GLStates.LineStippleFactor := 1;
    rci.GLStates.LineStipplePattern := $FFFF;
    // full openGL. lines
    GL.Begin_(GL_LINES);

    if (FDensity = 0) or (NewtonBodyGetSleepState(FNewtonBody) = 1) then
      GL.Color4fv(FManager.AABBColorSleep.AsAddress)
    else
      GL.Color4fv(FManager.AABBColor.AsAddress);
    // Get AABB
    NewtonBodyGetAABB(FNewtonBody, @(FAABBmin.AsVector), @(FAABBmax.AsVector));
    DrawAABB(FAABBmin, FAABBmax);
    GL.End_;
  end;

  if Manager.ShowSpring then
  begin
    if Assigned(FNewtonUserJoint) then
    begin
      // DrawLine
      rci.GLStates.LineWidth := 5;
      rci.GLStates.LineStippleFactor := 1;
      rci.GLStates.LineStipplePattern := $FF00; // dot style opengl lines
      GL.Begin_(GL_LINES);
      GL.Color4fv(FManager.SpringColor.AsAddress); // Aqua
      Barycenter := OwnerBaseSceneObject.BarycenterAbsolutePosition;
      CustomKinematicControllerGetTargetMatrix(FNewtonUserJoint, @PickedMatrix);
      pickpoint[0] := PickedMatrix[3, 0];
      pickpoint[1] := PickedMatrix[3, 1];
      pickpoint[2] := PickedMatrix[3, 2];
      GL.Vertex3fv(@pickpoint);
      GL.Vertex3fv(@Barycenter);
      GL.End_;

      // DrawPoint
      rci.GLStates.PointSize := 10;
      rci.GLStates.Enable(stPointSmooth);
      GL.Begin_(GL_POINTS);
      GL.Vertex3fv(@pickpoint);
      GL.Vertex3fv(@Barycenter);
      GL.End_;
      GL.Disable(GL_POINT_SMOOTH);
    end;
  end;

  if Manager.ShowContact then
  begin
    rci.GLStates.LineWidth := 1;
    rci.GLStates.LineStippleFactor := 1;
    rci.GLStates.LineStipplePattern := $FFFF; // full opengl lines

    GL.Begin_(GL_LINES);
    GL.Color4fv(FManager.ContactColor.AsAddress); // Aqua
    DrawContact;
    GL.End_;
  end;

end;

procedure TGLNGDDynamic.SetAutoSleep(val: Boolean);
begin
  if Assigned(FManager) then
    NewtonBodySetAutoSleep(FNewtonBody, Ord(val));
  FAutoSleep := val;
end;

procedure TGLNGDDynamic.SetCollision;
var
  VertexList: array of TAffineVector; // For FreeformMesh
  I, j: Integer; // For FreeformMesh
  CollisionArray: array of PNewtonCollision;

begin
  // Return nullcollision if unknow, return nil if freeform
  FCollision := GetCollisionFromBaseSceneObject(OwnerBaseSceneObject);

  // Create compound if freeform
  if not Assigned(FCollision) then
    with (OwnerBaseSceneObject as TGLFreeForm) do
    begin
      SetLength(CollisionArray, MeshObjects.Count);
      for j := 0 to MeshObjects.Count - 1 do
        with MeshObjects[j] do
          // If the mesh has less than 4 vertex its a plane
          if Vertices.Count < 3 then
            CollisionArray[j] := NewtonCreateNull(FManager.FNewtonWorld)
          else
          begin
            SetLength(VertexList, Vertices.Count);
            for I := 0 to Length(VertexList) - 1 do
            begin
              VertexList[I][0] := Vertices[I][0];
              VertexList[I][1] := Vertices[I][1];
              VertexList[I][2] := Vertices[I][2];
            end;

            CollisionArray[j] := NewtonCreateConvexHull(FManager.FNewtonWorld,
              Length(VertexList), @VertexList[0], SizeOf(TAffineVector), 0.001,
              0, nil);

            // if NewtonCreateConvexHull not sucessfull create nul collision
            if CollisionArray[j] = nil then
              CollisionArray[j] := NewtonCreateNull(FManager.FNewtonWorld);

          end;

      FCollision := NewtonCreateCompoundCollision(FManager.FNewtonWorld,
        Length(CollisionArray), @CollisionArray[0], 0);

    end;

  inherited;
end;

procedure TGLNGDDynamic.SetDensity(val: Single);
var
  inertia: TVector;
  origin: TVector;
begin
  if val >= 0 then
  begin
    FDensity := val;

    FVolume := NewtonConvexCollisionCalculateVolume(FCollision);
    NewtonConvexCollisionCalculateInertialMatrix(FCollision, @inertia, @origin);

    FMass := FVolume * FDensity;

    NewtonBodySetMassMatrix(FNewtonBody, FMass, FMass * inertia[0],
      FMass * inertia[1], FMass * inertia[2]);

    CenterOfMass.AsVector := origin;
  end;
end;

procedure TGLNGDDynamic.SetLinearDamping(val: Single);
begin
  if (val >= 0) and (val <= 1) then
    FLinearDamping := val;
  if Assigned(FManager) then
    NewtonBodySetLinearDamping(FNewtonBody, FLinearDamping);
end;

procedure TGLNGDDynamic.SetUpVector(val: Boolean);
begin
  FUpVector := val;
  if Assigned(FManager) then
  begin
    if FNewtonJoint <> nil then
    begin
      NewtonDestroyJoint(FManager.FNewtonWorld, FNewtonJoint);
      FNewtonJoint := nil
    end;

    if FUpVector then
      FNewtonJoint := NewtonConstraintCreateUpVector(FManager.FNewtonWorld,
        @FUpVectorDirection.AsVector, FNewtonBody)
  end;
end;

// WriteToFiler
//
procedure TGLNGDDynamic.WriteToFiler(writer: TWriter);
begin
  inherited;
  with writer do
  begin
    WriteInteger(1);
    // Archive version
    WriteBoolean(FAutoSleep);
    WriteSingle(FLinearDamping);
    WriteSingle(FDensity);
    WriteBoolean(FUpVector);
    WriteBoolean(FUseVelocity);
    WriteBoolean(FUseOmega);
    WriteBoolean(FUseGravity);
  end;
  FForce.WriteToFiler(writer);
  FTorque.WriteToFiler(writer);
  FVelocity.WriteToFiler(writer);
  FOmega.WriteToFiler(writer);
  FCenterOfMass.WriteToFiler(writer);
  FAngularDamping.WriteToFiler(writer);
  FUpVectorDirection.WriteToFiler(writer);
end;

// ReadFromFiler
//
procedure TGLNGDDynamic.ReadFromFiler(reader: TReader);
var
  Version: Integer;
begin
  inherited;
  with reader do
  begin
    Version := ReadInteger; // read data version
    Assert(Version <= 1); // Archive version

    FAutoSleep := ReadBoolean;
    FLinearDamping := ReadSingle;
    FDensity := ReadSingle;
    FUpVector := ReadBoolean;
    FUseVelocity := ReadBoolean;
    FUseOmega := ReadBoolean;

    if Version >= 1 then
      FUseGravity := ReadBoolean;

  end;
  FForce.ReadFromFiler(reader);
  FTorque.ReadFromFiler(reader);
  FVelocity.ReadFromFiler(reader);
  FOmega.ReadFromFiler(reader);
  FCenterOfMass.ReadFromFiler(reader);
  FAngularDamping.ReadFromFiler(reader);
  FUpVectorDirection.ReadFromFiler(reader);
end;

procedure TGLNGDDynamic.Loaded;
begin
  inherited;
  if Assigned(FManager) then
  begin
    SetAutoSleep(FAutoSleep);
    SetLinearDamping(FLinearDamping);
    SetDensity(FDensity);
    NotifyCenterOfMassChange(self);
    NotifyAngularDampingChange(self);
    SetUpVector(FUpVector);
    NotifyUpVectorDirectionChange(self);
  end;
end;

procedure TGLNGDDynamic.NotifyAngularDampingChange(Sender: TObject);
begin
  FAngularDamping.OnNotifyChange := nil;
  if (FAngularDamping.x >= 0) and (FAngularDamping.x <= 1) and
    (FAngularDamping.y >= 0) and (FAngularDamping.y <= 1) and
    (FAngularDamping.z >= 0) and (FAngularDamping.z <= 1) then
    if Assigned(FManager) then
      NewtonBodySetAngularDamping(FNewtonBody, @(FAngularDamping.AsVector));
  FAngularDamping.OnNotifyChange := NotifyAngularDampingChange;
end;

procedure TGLNGDDynamic.NotifyCenterOfMassChange(Sender: TObject);
begin
  FCenterOfMass.OnNotifyChange := nil;
  if Assigned(FManager) then
    NewtonBodySetCentreOfMass(FNewtonBody, @(FCenterOfMass.AsVector));
  FCenterOfMass.OnNotifyChange := NotifyCenterOfMassChange;
end;

procedure TGLNGDDynamic.NotifyUpVectorDirectionChange(Sender: TObject);
begin
  FUpVectorDirection.OnNotifyChange := nil;
  FUpVectorDirection.Normalize;
  FUpVectorDirection.OnNotifyChange := NotifyUpVectorDirectionChange;

  if Assigned(FManager) then
    if FNewtonJoint <> nil then
      NewtonUpVectorSetPin(FNewtonJoint, @FUpVectorDirection.AsVector);
end;

procedure TGLNGDDynamic.Pick(pickpoint: TVector; Mode: TNGDPickedModes);
begin
  // Create the joint
  if Mode = pmAttach then
  begin
    if Assigned(FNewtonBody) then
      FNewtonUserJoint := CreateCustomKinematicController(FNewtonBody,
        @pickpoint);
    if Assigned(FNewtonUserJoint) then
    begin
      CustomKinematicControllerSetPickMode(FNewtonUserJoint, 0);
      CustomKinematicControllerSetMaxLinearFriction(FNewtonUserJoint, 750);
      CustomKinematicControllerSetMaxAngularFriction(FNewtonUserJoint, 250);
      CustomKinematicControllerSetTargetPosit(FNewtonUserJoint, @pickpoint);
    end;
  end;

  // Change the TargetPoint
  if Mode = pmMove then
  begin
    if Assigned(FNewtonUserJoint) then
    begin
      CustomKinematicControllerSetTargetPosit(FNewtonUserJoint, @pickpoint);
    end;
  end;

  // Delete the joint
  if Mode = pmDetach then
  begin
    if Assigned(FNewtonUserJoint) then
    begin
      CustomDestroyJoint(FNewtonUserJoint);
      FNewtonUserJoint := nil;
    end;
  end;
end;

{ function TGLNGDDynamic.player(pindir: TMatrix;
  stepfactor, cushion: Single): PNewtonUserJoint;
  begin
  Result := CreateCustomPlayerController(@pindir, FNewtonBody, stepfactor,
  cushion);
  end; }

{ TGLNGDStatic }

procedure TGLNGDStatic.Render(var rci: TRenderContextInfo);
begin
  inherited;
  // Move/Rotate NewtonObject if matrix are not equal in run time.
  if not MatrixEquals(NewtonBodyMatrix, OwnerBaseSceneObject.AbsoluteMatrix)
    then
    SetNewtonBodyMatrix(OwnerBaseSceneObject.AbsoluteMatrix);

  // Rebuild Collision if Radius has been modified
  if FBoundingSphereRadius <> OwnerBaseSceneObject.BoundingSphereRadius then
    // Wont be called if visible at runtime is inactive
    // Reinitialize;
    ;
end;

procedure TGLNGDStatic.SetCollision;
begin
  // Return nullcollision if unknow, return nil if freeform
  FCollision := GetCollisionFromBaseSceneObject(OwnerBaseSceneObject);

  // Create tree if freeform
  if not Assigned(FCollision) then
    FCollision := GetTree(True, OwnerBaseSceneObject.Scale.x);

  inherited;
end;

// To create heightField
procedure TGLNGDStatic.SetHeightField(heightArray: array of UInt16; x: Integer;
  y: Integer; xScale: Single; yScale: Single);
{ Var
  attributeMap: Array Of UInt8;
  i: integer; }
begin

  { SetLength(FHeightFieldWordArray, Length(heightArray));
    SetLength(attributeMap, Length(heightArray));

    For i := 0 To Length(heightArray) - 1 Do
    Begin
    FHeightFieldWordArray[i] := heightArray[i];
    attributeMap[i] := 0;
    End;

    FCollision := NewtonCreateHeightFieldCollision(FManager.FNewtonWorld, x, y,
    0, Punsigned_short(FHeightFieldWordArray), P2Char(attributeMap), xScale,
    yScale, 0);

    NewtonBodySetCollision(FNewtonBody, FCollision);
    NewtonReleaseCollision(FManager.FNewtonWorld, FCollision); }
end;

function TGLNGDStatic.GetTree(optimize: Boolean;
  scaleXYZ: Single): PNewtonCollision;
var
  MeshIndex, TriangleIndex: Integer;
  TriangleList: TAffineVectorList;
  v: array [0 .. 2] of TAffineVector;
  NewtonCollision: PNewtonCollision;
begin
  with (OwnerBaseSceneObject as TGLFreeForm) do
  begin
    NewtonCollision := NewtonCreateTreeCollision(FManager.FNewtonWorld, 0);
    NewtonTreeCollisionBeginBuild(NewtonCollision);

    for MeshIndex := 0 to MeshObjects.Count - 1 do
    begin
      TriangleList := MeshObjects[MeshIndex].ExtractTriangles;
      for TriangleIndex := 0 to TriangleList.Count - 1 do
      begin
        if TriangleIndex mod 3 = 0 then
        begin
          v[0] := TriangleList.Items[TriangleIndex];
          ScaleVector(v[0], scaleXYZ);
          v[1] := TriangleList.Items[TriangleIndex + 1];
          ScaleVector(v[1], scaleXYZ);
          v[2] := TriangleList.Items[TriangleIndex + 2];
          ScaleVector(v[2], scaleXYZ);
          NewtonTreeCollisionAddFace(NewtonCollision, 3, @(v),
            SizeOf(TAffineVector), 1);
        end;
      end;
      TriangleList.Free;
    end;
    NewtonTreeCollisionEndBuild(NewtonCollision, Ord(optimize));
    Result := NewtonCollision;
  end;
end;

class function TGLNGDStatic.FriendlyName: string;
begin
  Result := 'NGD Static';
end;

{ TNGDMaterialPair }

constructor TNGDMaterialPair.Create(AOwner: TXCollection);
begin
  inherited;
  FSoftness := 0.1;
  FElasticity := 0.4;
  FCollidable := True;
  FStaticFriction := 0.9;
  FKineticFriction := 0.5;
  Fid0 := 0;
  Fid1 := 0;
  FContactProcessEvent := nil;
  FManager := TGLNGDManager(owner.owner);
end;

destructor TNGDMaterialPair.Destroy;
begin
  inherited;
end;

procedure TNGDMaterialPair.Finalize;
begin
  FInitialized := False;
  if Assigned(FManager) then
    NewtonMaterialSetCollisionCallback(FManager.FNewtonWorld, Fid0, Fid1, nil,
      nil, nil);
end;

class function TNGDMaterialPair.FriendlyDescription: string;
begin
  Result := 'MaterialPair';
end;

class function TNGDMaterialPair.FriendlyName: string;
begin
  Result := 'NGD MaterialPair implementation';
end;

procedure TNGDMaterialPair.Initialize;
begin
  if Assigned(FManager) then
  begin
    FManager.MaterialAutoCreateGroupID(Fid0);
    FManager.MaterialAutoCreateGroupID(Fid1);
    NewtonMaterialSetCollisionCallback(FManager.FNewtonWorld, Fid0, Fid1, self,
      @NewtonOnAABBOverlap, @NewtonContactsProcess);

    FInitialized := True;
  end;
end;

procedure TNGDMaterialPair.Loaded;
begin
  inherited;
  Setid0(Fid0);
  Setid1(Fid1);
  SetSoftness(FSoftness);
  SetCollidable(FCollidable);
  SetElasticity(FElasticity);
  SetStaticFriction(FStaticFriction);
  SetKineticFriction(FKineticFriction);
end;

procedure TNGDMaterialPair.ReadFromFiler(reader: TReader);
{ var
  ContactProcessEventOwner, ContactProcessEventName: string; }
var
  Version: Integer;
begin
  inherited;
  with reader do
  begin
    Version := ReadInteger; // read data version
    Assert(Version <= 1); // Archive version

    if Version = 0 then
      ReadString;

    FSoftness := ReadSingle;
    FElasticity := ReadSingle;
    FCollidable := ReadBoolean;
    FStaticFriction := ReadSingle;
    FKineticFriction := ReadSingle;
    Fid0 := ReadInteger;
    Fid1 := ReadInteger;
    // ContactProcessEventOwner := ReadString;
    // ContactProcessEventName := ReadString;
  end;
  // FContactProcessEvent := TContactProcessEvent
  // (GetMethodFromString(ContactProcessEventOwner, ContactProcessEventName));
end;

procedure TNGDMaterialPair.WriteToFiler(writer: TWriter);
{ var
  ContactProcessEventOwner, ContactProcessEventName: string; }
begin
  inherited;
  with writer do
  begin
    WriteInteger(1); // Archive version
    WriteSingle(FSoftness);
    WriteSingle(FElasticity);
    WriteBoolean(FCollidable);
    WriteSingle(FStaticFriction);
    WriteSingle(FKineticFriction);
    WriteInteger(Fid0);
    WriteInteger(Fid1);

    { GetStringFromMethod(TMethod(FContactProcessEvent),
      ContactProcessEventOwner, ContactProcessEventName);
      WriteString(ContactProcessEventOwner);
      WriteString(ContactProcessEventName); }
  end;
end;

procedure TNGDMaterialPair.SetCollidable(val: Boolean);
begin
  FCollidable := val;
  if Initialized then
    NewtonMaterialSetDefaultCollidable(FManager.FNewtonWorld, Fid0, Fid1,
      Ord(FCollidable));
end;

procedure TNGDMaterialPair.SetElasticity(val: Single);
begin
  if (val >= 0) then
    FElasticity := val;
  if Initialized then
    NewtonMaterialSetDefaultElasticity(FManager.FNewtonWorld, Fid0, Fid1,
      FElasticity);
end;

procedure TNGDMaterialPair.Setid0(const value: Integer);
begin
  if Initialized then
    Finalize;
  Fid0 := value;
  if not Initialized then
    Initialize;
end;

procedure TNGDMaterialPair.Setid1(const value: Integer);
begin
  if Initialized then
    Finalize;
  Fid1 := value;
  if not Initialized then
    Initialize;
end;

procedure TNGDMaterialPair.SetSoftness(val: Single);
begin
  if (val >= 0) and (val <= 1) then
    FSoftness := val;
  if Initialized then
    NewtonMaterialSetDefaultSoftness(FManager.FNewtonWorld, Fid0, Fid1,
      FSoftness);
end;

procedure TNGDMaterialPair.SetStaticFriction(val: Single);
begin
  if (val <= 1) and (val >= FKineticFriction) then
    FStaticFriction := val;
  if Initialized then
    NewtonMaterialSetDefaultFriction(FManager.FNewtonWorld, Fid0, Fid1,
      FStaticFriction, FKineticFriction);
end;

procedure TNGDMaterialPair.SetKineticFriction(val: Single);
begin
  if (val <= FStaticFriction) and (val >= 0) then
    FKineticFriction := val;
  if Initialized then
    NewtonMaterialSetDefaultFriction(FManager.FNewtonWorld, Fid0, Fid1,
      FStaticFriction, FKineticFriction);
end;

{ TNGDMaterials }

procedure TNGDMaterials.Finalize;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    MaterialPair[I].Finalize;
end;

function TNGDMaterials.GetMaterialPair(index: Integer): TNGDMaterialPair;
begin
  Result := TNGDMaterialPair(Items[index]);
end;

procedure TNGDMaterials.Initialize;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    MaterialPair[I].Initialize;
end;

class function TNGDMaterials.ItemsClass: TXCollectionItemClass;
begin
  Result := TNGDMaterialPair;
end;

{ TNGDJoints }

procedure TNGDJoints.Finalize;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    Joint[I].Finalize;
end;

function TNGDJoints.GetJoint(index: Integer): TNGDJointBase;
begin
  Result := TNGDJointBase(Items[index]);
end;

procedure TNGDJoints.Initialize;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    Joint[I].Initialize;
end;

class function TNGDJoints.ItemsClass: TXCollectionItemClass;
begin
  Result := TNGDJointBase;
end;

{ TGLNGDJointList }

constructor TGLNGDJointList.Create(AOwner: TComponent);
begin
  inherited;
  FJoints := TNGDJoints.Create(self);
end;

procedure TGLNGDJointList.DefineProperties(Filer: TFiler);
begin
  inherited;
  Filer.DefineBinaryProperty('NGDJointsData', ReadJoints, WriteJoints,
    (Assigned(FJoints) and (FJoints.Count > 0)));
end;

destructor TGLNGDJointList.Destroy;
begin
  FJoints.Free;
  inherited;
end;

procedure TGLNGDJointList.Loaded;
var
  I: Integer;
begin
  inherited;
  for I := 0 to FJoints.Count - 1 do
    FJoints[I].Loaded;
end;

procedure TGLNGDJointList.Notification(AComponent: TComponent;
  Operation: TOperation);
var
  I: Integer;
begin
  inherited;
  if (Operation = opRemove) and (AComponent is TGLBaseSceneObject) then
    for I := 0 to Joints.Count - 1 do
    begin
      if TGLBaseSceneObject(AComponent) = Joints[I].Object1 then
        Joints[I].Object1 := nil;
      if TGLBaseSceneObject(AComponent) = Joints[I].Object2 then
        Joints[I].Object2 := nil;
    end;
end;

procedure TGLNGDJointList.ReadJoints(stream: TStream);
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

procedure TGLNGDJointList.WriteJoints(stream: TStream);
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

{ TNGDJointBase }

constructor TNGDJointBase.Create(AOwner: TXCollection);
begin
  inherited;
  FInitialized := False;
end;

destructor TNGDJointBase.Destroy;
begin
  Finalize;
  inherited;
end;

procedure TNGDJointBase.Finalize;
begin
  if not Initialized then
    exit;

  if Assigned(FObject1) then
    UnregisterJointWithObject(FObject1);
  if Assigned(FObject2) then
    UnregisterJointWithObject(FObject2);

  FInitialized := False;
end;

procedure TNGDJointBase.Initialize;
begin
  if not Assigned(FManager) then
    exit;
  if Assigned(FObject1) then
    RegisterJointWithObject(FObject1);
  if Assigned(FObject2) then
    RegisterJointWithObject(FObject2);
  FInitialized := True;
end;

procedure TNGDJointBase.Loaded;
var
  mng: TComponent;
  Obj: TGLBaseSceneObject;
begin
  inherited;
  if FManagerName <> '' then
  begin
    mng := FindManager(TGLNGDManager, FManagerName);
    if Assigned(mng) then
      Manager := TGLNGDManager(mng);
    FManagerName := '';
  end;
  if FObject1Name <> '' then
  begin
    Obj := GetGLSceneObject(FObject1Name);
    if Assigned(Obj) then
      Object1 := Obj;
    FObject1Name := '';
  end;
  if FObject2Name <> '' then
  begin
    Obj := GetGLSceneObject(FObject2Name);
    if Assigned(Obj) then
      Object2 := Obj;
    FObject2Name := '';
  end;
  StructureChanged(self);
end;

procedure TNGDJointBase.ReadFromFiler(reader: TReader);
begin
  inherited;
  with reader do
  begin
    Assert(ReadInteger = 0);
    // Archive version
    FManagerName := ReadString;
    FObject1Name := ReadString;
    FObject2Name := ReadString;
  end;
end;

procedure TNGDJointBase.WriteToFiler(writer: TWriter);
begin
  inherited;
  with writer do
  begin
    WriteInteger(0);
    // Archive version
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
  end;
end;

procedure TNGDJointBase.RegisterJointWithObject(Obj: TGLBaseSceneObject);
var
  temp: TGLNGDBehaviour;
begin
  if Assigned(Obj) then
  begin
    temp := TGLNGDBehaviour(Obj.Behaviours.GetByClass(TGLNGDBehaviour));
    if Assigned(temp) then
      temp.RegisterJoint(self);
  end;
end;

procedure TNGDJointBase.Render;
var
  bar1, bar2: TVector;
begin
  if Assigned(Object1) and Assigned(Object2) then
  begin
    bar1 := Object1.BarycenterAbsolutePosition;
    bar2 := Object2.BarycenterAbsolutePosition;
    GL.Vertex3fv(@bar1);
    GL.Vertex3fv(@bar2);
  end;
end;

procedure TNGDJointBase.UnregisterJointWithObject(Obj: TGLBaseSceneObject);
var
  temp: TGLNGDBehaviour;
begin
  if Assigned(Obj) then
  begin
    temp := TGLNGDBehaviour(Obj.Behaviours.GetByClass(TGLNGDBehaviour));
    if Assigned(temp) then
      temp.UnregisterJoint(self);
  end;
end;

procedure TNGDJointBase.SetManager(const value: TGLNGDManager);
begin
  if FManager <> value then
  begin
    if Assigned(FManager) then
      if Initialized then // if not(csDesigning in FManager.ComponentState) then
        Finalize;
    FManager := value;
    if Assigned(FManager) then
      // if not(csDesigning in FManager.ComponentState) then
      Initialize;
  end;
end;

procedure TNGDJointBase.SetObject1(const value: TGLBaseSceneObject);
begin
  if FObject1 <> value then
  begin
    if Assigned(FObject1) then
      UnregisterJointWithObject(FObject1);
    FObject1 := value;
    if Assigned(FObject1) then
      RegisterJointWithObject(FObject1)
    else
      FObject1 := nil;
  end;
end;

procedure TNGDJointBase.SetObject2(const value: TGLBaseSceneObject);
begin
  if FObject2 <> value then
  begin
    if Assigned(FObject2) then
      UnregisterJointWithObject(FObject2);
    FObject2 := value;
    if Assigned(FObject2) then
      RegisterJointWithObject(FObject2)
    else
      FObject2 := nil;
  end;
end;

procedure TNGDJointBase.StructureChanged(Sender: TObject);
begin
  if Assigned(FManager) then
    FManager.NotifyChange(self);
end;

{ TNGDJointBall }

constructor TNGDJointBall.Create(AOwner: TXCollection);
begin
  inherited;

  FNewtonJoint := nil;
  FStiffness := 0.9;
  FCollisionState := False;

  FPivotPoint := TGLCoordinates.CreateInitialized(self, NullHMGPoint, csPoint);
  FPivotPoint.OnNotifyChange := StructureChanged;
end;

destructor TNGDJointBall.Destroy;
begin
  if FNewtonJoint <> nil then
    if Assigned(FManager) then
      NewtonDestroyJoint(FManager.FNewtonWorld, FNewtonJoint);
  FNewtonJoint := nil;
  FPivotPoint.Free;
  inherited;
end;

class function TNGDJointBall.FriendlyDescription: string;
begin
  Result := 'NGD Ball joint implementation';
end;

class function TNGDJointBall.FriendlyName: string;
begin
  Result := 'Ball';
end;

procedure TNGDJointBall.ReadFromFiler(reader: TReader);
begin
  inherited;
  with reader do
  begin
    Assert(ReadInteger = 0);
    // Archive version
    FStiffness := ReadSingle;
    FCollisionState := ReadBoolean;
  end;
  FPivotPoint.ReadFromFiler(reader);
end;

procedure TNGDJointBall.WriteToFiler(writer: TWriter);
begin
  inherited;
  with writer do
  begin
    WriteInteger(0);
    // Archive version
    WriteSingle(FStiffness);
    WriteBoolean(FCollisionState);
  end;
  FPivotPoint.WriteToFiler(writer);
end;

procedure TNGDJointBall.Render;
var
  bar1, bar2: TVector;
begin
  if Assigned(Object1) and Assigned(Object2) then
  begin
    bar1 := Object1.BarycenterAbsolutePosition;
    bar2 := Object2.BarycenterAbsolutePosition;
    GL.Vertex3fv(@bar1);
    GL.Vertex3fv(FPivotPoint.AsAddress);
    GL.Vertex3fv(FPivotPoint.AsAddress);
    GL.Vertex3fv(@bar2);
  end;
end;

procedure TNGDJointBall.StructureChanged(Sender: TObject);
begin
  inherited;
  if Assigned(FManager) then
  begin
    if FNewtonJoint <> nil then
    begin
      NewtonDestroyJoint(FManager.FNewtonWorld, FNewtonJoint);
      FNewtonJoint := nil;
    end;

    if self.Classtype = TNGDJointBall then
      if Assigned(Object1) and Assigned(Object2) then
      begin
        FNewtonJoint := NewtonConstraintCreateBall(FManager.FNewtonWorld,
          @(FPivotPoint.AsVector), GetBodyFromGLSceneObject(Object1),
          GetBodyFromGLSceneObject(Object2));
        CollisionState := FCollisionState;
        Stiffness := FStiffness;
      end;
  end;
end;

procedure TNGDJointBall.SetStiffness(val: Single);
begin
  if (val >= 0) and (val <= 1) then
    FStiffness := val;
  if FNewtonJoint <> nil then
    NewtonJointSetStiffness(FNewtonJoint, FStiffness);
end;

procedure TNGDJointBall.SetCollisionState(val: Boolean);
begin
  FCollisionState := val;
  if FNewtonJoint <> nil then
    NewtonJointSetCollisionState(FNewtonJoint, Ord(FCollisionState));
end;

{ TNGDJointHinge }

constructor TNGDJointHinge.Create(AOwner: TXCollection);
begin
  inherited;
  FPinDir := TGLCoordinates.CreateInitialized(self, ZHmgVector, csVector);
  FPinDir.OnNotifyChange := StructureChanged;
end;

destructor TNGDJointHinge.Destroy;
begin
  FPinDir.Free;
  inherited;
end;

class function TNGDJointHinge.FriendlyDescription: string;
begin
  Result := 'NGD Hinge joint';
end;

class function TNGDJointHinge.FriendlyName: string;
begin
  Result := 'Hinge';
end;

procedure TNGDJointHinge.ReadFromFiler(reader: TReader);
begin
  inherited;
  Assert(reader.ReadInteger = 0);
  // Archive version
  FPinDir.ReadFromFiler(reader);
end;

procedure TNGDJointHinge.WriteToFiler(writer: TWriter);
begin
  inherited;
  writer.WriteInteger(0);
  // Archive version
  FPinDir.WriteToFiler(writer);
end;

procedure TNGDJointHinge.Render;
var
  axe: TVector;
begin
  inherited;
  if Assigned(Object1) and Assigned(Object2) then
  begin
    axe[0] := FPivotPoint.x - 10 * FPinDir.x;
    axe[1] := FPivotPoint.y - 10 * FPinDir.y;
    axe[2] := FPivotPoint.z - 10 * FPinDir.z;
    GL.Vertex3fv(@axe);
    axe[0] := FPivotPoint.x + 10 * FPinDir.x;
    axe[1] := FPivotPoint.y + 10 * FPinDir.y;
    axe[2] := FPivotPoint.z + 10 * FPinDir.z;
    GL.Vertex3fv(@axe);
  end;
end;

procedure TNGDJointHinge.StructureChanged(Sender: TObject);
begin
  inherited;
  if Assigned(FManager) then
    if self.Classtype = TNGDJointHinge then
    begin
      FPinDir.OnNotifyChange := nil;
      FPinDir.Normalize;
      FPinDir.OnNotifyChange := StructureChanged;

      if Assigned(Object1) and Assigned(Object2) then
      begin
        FNewtonJoint := NewtonConstraintCreateHinge(FManager.FNewtonWorld,
          @(FPivotPoint.AsVector), @(FPinDir.AsVector),
          GetBodyFromGLSceneObject(Object1), GetBodyFromGLSceneObject(Object2));
        CollisionState := FCollisionState;
        Stiffness := FStiffness;
      end;
    end;
end;

{ TNGDJointSlider }

class function TNGDJointSlider.FriendlyDescription: string;
begin
  Result := 'NGD Slider joint';
end;

class function TNGDJointSlider.FriendlyName: string;
begin
  Result := 'Slider';
end;

procedure TNGDJointSlider.StructureChanged(Sender: TObject);
begin
  inherited;
  if Assigned(FManager) then
    if self.Classtype = TNGDJointSlider then
    begin
      FPinDir.OnNotifyChange := nil;
      FPinDir.Normalize;
      FPinDir.OnNotifyChange := StructureChanged;

      if Assigned(Object1) and Assigned(Object2) then
      begin
        FNewtonJoint := NewtonConstraintCreateSlider(FManager.FNewtonWorld,
          @(FPivotPoint.AsVector), @(FPinDir.AsVector),
          GetBodyFromGLSceneObject(Object1), GetBodyFromGLSceneObject(Object2));
        CollisionState := FCollisionState;
        Stiffness := FStiffness;
      end;
    end;
end;

{ TNGDJointCorkscrew }

class function TNGDJointCorkscrew.FriendlyDescription: string;
begin
  Result := 'NGD Corkscrew joint';
end;

class function TNGDJointCorkscrew.FriendlyName: string;
begin
  Result := 'Corkscrew';
end;

procedure TNGDJointCorkscrew.StructureChanged(Sender: TObject);
begin
  inherited;
  if Assigned(FManager) then
    if self.Classtype = TNGDJointCorkscrew then
    begin
      FPinDir.OnNotifyChange := nil;
      FPinDir.Normalize;
      FPinDir.OnNotifyChange := StructureChanged;

      if Assigned(Object1) and Assigned(Object2) then
      begin
        FNewtonJoint := NewtonConstraintCreateCorkscrew(FManager.FNewtonWorld,
          @(FPivotPoint.AsVector), @(FPinDir.AsVector),
          GetBodyFromGLSceneObject(Object1), GetBodyFromGLSceneObject(Object2));
        CollisionState := FCollisionState;
        Stiffness := FStiffness;
      end;
    end;
end;

{ TNGDJointUniversal }

constructor TNGDJointUniversal.Create(AOwner: TXCollection);
begin
  inherited;
  FPinDir2 := TGLCoordinates.CreateInitialized(self, ZHmgVector, csVector);
  FPinDir2.OnNotifyChange := StructureChanged;
end;

destructor TNGDJointUniversal.Destroy;
begin
  FPinDir2.Free;
  inherited;
end;

class function TNGDJointUniversal.FriendlyDescription: string;
begin
  Result := 'NGD Universal joint';
end;

class function TNGDJointUniversal.FriendlyName: string;
begin
  Result := 'Universal';
end;

procedure TNGDJointUniversal.ReadFromFiler(reader: TReader);
begin
  inherited;
  Assert(reader.ReadInteger = 0);
  // Archive version
  FPinDir2.ReadFromFiler(reader);
end;

procedure TNGDJointUniversal.WriteToFiler(writer: TWriter);
begin
  inherited;
  writer.WriteInteger(0);
  // Archive version
  FPinDir2.WriteToFiler(writer);
end;

procedure TNGDJointUniversal.Render;
var
  axe: TVector;
begin
  inherited;
  if Assigned(Object1) and Assigned(Object2) then
  begin
    axe[0] := FPivotPoint.x - 10 * FPinDir2.x;
    axe[1] := FPivotPoint.y - 10 * FPinDir2.y;
    axe[2] := FPivotPoint.z - 10 * FPinDir2.z;
    GL.Vertex3fv(@axe);
    axe[0] := FPivotPoint.x + 10 * FPinDir2.x;
    axe[1] := FPivotPoint.y + 10 * FPinDir2.y;
    axe[2] := FPivotPoint.z + 10 * FPinDir2.z;
    GL.Vertex3fv(@axe);
  end;
end;

procedure TNGDJointUniversal.StructureChanged(Sender: TObject);
begin
  inherited;
  if Assigned(FManager) then
    if self.Classtype = TNGDJointUniversal then
    begin
      FPinDir.OnNotifyChange := nil;
      FPinDir2.OnNotifyChange := nil;
      FPinDir.Normalize;
      FPinDir2.Normalize;
      FPinDir.OnNotifyChange := StructureChanged;
      FPinDir2.OnNotifyChange := StructureChanged;

      if Assigned(Object1) and Assigned(Object2) then
      begin
        FNewtonJoint := NewtonConstraintCreateUniversal(FManager.FNewtonWorld,
          @(FPivotPoint.AsVector), @(FPinDir.AsVector), @(FPinDir2.AsVector),
          GetBodyFromGLSceneObject(Object1), GetBodyFromGLSceneObject(Object2));
        CollisionState := FCollisionState;
        Stiffness := FStiffness;
      end;
    end;
end;

{ TNGDCustomJointBase }

constructor TNGDCustomJointBase.Create(AOwner: TXCollection);
begin
  inherited;
  FNewtonUserJoint := nil;
  FStiffness := 0.9;
  FCollisionState := False;
  FMinLimit := -50;
  FMaxLimit := 50;
  FPivotPoint := TGLCoordinates.CreateInitialized(self, NullHMGPoint, csPoint);
  FPivotPoint.OnNotifyChange := StructureChanged;
end;

destructor TNGDCustomJointBase.Destroy;
begin
  if FNewtonUserJoint <> nil then
    CustomDestroyJoint(FNewtonUserJoint);
  FNewtonUserJoint := nil;
  FPivotPoint.Free;
  inherited;
end;

procedure TNGDCustomJointBase.ReadFromFiler(reader: TReader);
begin
  inherited;
  with reader do
  begin
    Assert(ReadInteger = 0);
    // Archive version
    FStiffness := ReadSingle;
    FCollisionState := ReadBoolean;
    FMinLimit := ReadSingle;
    FMaxLimit := ReadSingle;
  end;
  FPivotPoint.ReadFromFiler(reader);
end;

procedure TNGDCustomJointBase.Render;
var
  bar1, bar2: TVector;
begin
  if Assigned(Object1) and Assigned(Object2) then
  begin
    bar1 := Object1.BarycenterAbsolutePosition;
    bar2 := Object2.BarycenterAbsolutePosition;
    GL.Vertex3fv(@bar1);
    GL.Vertex3fv(FPivotPoint.AsAddress);
    GL.Vertex3fv(FPivotPoint.AsAddress);
    GL.Vertex3fv(@bar2);
  end;
end;

procedure TNGDCustomJointBase.SetCollisionState(val: Boolean);
begin
  FCollisionState := val;
  if FNewtonUserJoint <> nil then
    CustomSetBodiesCollisionState(FNewtonUserJoint, Ord(FCollisionState));
end;

procedure TNGDCustomJointBase.SetMaxLimit(val: Single);
begin
  // Virtual
end;

procedure TNGDCustomJointBase.SetMinLimit(val: Single);
begin
  // Virtual
end;

procedure TNGDCustomJointBase.SetStiffness(val: Single);
begin
  if (val >= 0) and (val <= 1) then
    FStiffness := val;
  if FNewtonUserJoint <> nil then
    NewtonJointSetStiffness(CustomGetNewtonJoint(FNewtonUserJoint), FStiffness);
end;

procedure TNGDCustomJointBase.StructureChanged(Sender: TObject);
begin
  inherited;
  if Assigned(FManager) then
    if FNewtonUserJoint <> nil then
    begin
      CustomDestroyJoint(FNewtonUserJoint);
      FNewtonUserJoint := nil;
    end;
  FPinAndPivotMatrix := IdentityHmgMatrix;
  FPinAndPivotMatrix[3, 0] := FPivotPoint.x;
  FPinAndPivotMatrix[3, 1] := FPivotPoint.y;
  FPinAndPivotMatrix[3, 2] := FPivotPoint.z;
end;

procedure TNGDCustomJointBase.WriteToFiler(writer: TWriter);
begin
  inherited;
  with writer do
  begin
    WriteInteger(0);
    // Archive version
    WriteSingle(FStiffness);
    WriteBoolean(FCollisionState);
    WriteSingle(FMinLimit);
    WriteSingle(FMaxLimit);
  end;
  FPivotPoint.WriteToFiler(writer);
end;

{ TNGDCustomJointBall }

constructor TNGDCustomJointBall.Create(AOwner: TXCollection);
begin
  inherited;
  FConeAngle := 90;
  FMinLimit := -90;
  FMaxLimit := 90;
end;

class function TNGDCustomJointBall.FriendlyDescription: string;
begin
  Result := 'NGD Custom Ball joint implementation';
end;

class function TNGDCustomJointBall.FriendlyName: string;
begin
  Result := 'CustomBall';
end;

procedure TNGDCustomJointBall.ReadFromFiler(reader: TReader);
begin
  inherited;
  with reader do
  begin
    Assert(ReadInteger = 0);
    // Archive version
    FConeAngle := ReadSingle;
  end;
end;

procedure TNGDCustomJointBall.SetConeAngle(val: Single);
begin
  if (val >= 0) and (val < 180) then
    FConeAngle := val;
  if FNewtonUserJoint <> nil then
    BallAndSocketSetConeAngle(FNewtonUserJoint, DegToRad(FConeAngle));
end;

procedure TNGDCustomJointBall.SetMaxLimit(val: Single);
begin
  if (val >= FMinLimit) then
    FMaxLimit := val;
  if FNewtonUserJoint <> nil then
    BallAndSocketSetTwistAngle(FNewtonUserJoint, DegToRad(FMinLimit),
      DegToRad(FMaxLimit));
end;

procedure TNGDCustomJointBall.SetMinLimit(val: Single);
begin
  if (val <= FMaxLimit) then
    FMinLimit := val;
  if FNewtonUserJoint <> nil then
    BallAndSocketSetTwistAngle(FNewtonUserJoint, DegToRad(FMinLimit),
      DegToRad(FMaxLimit));
end;

procedure TNGDCustomJointBall.StructureChanged(Sender: TObject);
begin
  inherited;
  if Assigned(FManager) then
    if self.Classtype = TNGDCustomJointBall then
      if Assigned(Object1) and Assigned(Object2) then
      begin
        FNewtonUserJoint := CreateCustomBallAndSocket(@FPinAndPivotMatrix,
          GetBodyFromGLSceneObject(Object1), GetBodyFromGLSceneObject(Object2));
        BallAndSocketSetTwistAngle(FNewtonUserJoint, DegToRad(FMinLimit),
          DegToRad(FMaxLimit));
        BallAndSocketSetConeAngle(FNewtonUserJoint, DegToRad(FConeAngle));
        CollisionState := FCollisionState;
        Stiffness := FStiffness;
      end;
end;

procedure TNGDCustomJointBall.WriteToFiler(writer: TWriter);
begin
  inherited;
  with writer do
  begin
    WriteInteger(0);
    // Archive version
    WriteSingle(FConeAngle);
  end;
end;

{ TNGDCustomJointBaseDir }

constructor TNGDCustomJointBaseDir.Create(AOwner: TXCollection);
begin
  inherited;
  FPinDir := TGLCoordinates.CreateInitialized(self, ZHmgVector, csVector);
  FPinDir.OnNotifyChange := StructureChanged;
end;

destructor TNGDCustomJointBaseDir.Destroy;
begin
  FPinDir.Free;
  inherited;
end;

procedure TNGDCustomJointBaseDir.ReadFromFiler(reader: TReader);
begin
  inherited;
  with reader do
  begin
    Assert(ReadInteger = 0);
    // Archive version
  end;
  FPinDir.ReadFromFiler(reader);
end;

procedure TNGDCustomJointBaseDir.Render;
var
  axe: TVector;
begin
  inherited;
  if Assigned(Object1) and Assigned(Object2) then
  begin
    axe[0] := FPivotPoint.x - 10 * FPinDir.x;
    axe[1] := FPivotPoint.y - 10 * FPinDir.y;
    axe[2] := FPivotPoint.z - 10 * FPinDir.z;
    GL.Vertex3fv(@axe);
    axe[0] := FPivotPoint.x + 10 * FPinDir.x;
    axe[1] := FPivotPoint.y + 10 * FPinDir.y;
    axe[2] := FPivotPoint.z + 10 * FPinDir.z;
    GL.Vertex3fv(@axe);
  end;
end;

procedure TNGDCustomJointBaseDir.StructureChanged(Sender: TObject);
var
  bso: TGLBaseSceneObject;
  Line: TVector;
begin
  inherited;
  FPinDir.OnNotifyChange := nil;
  FPinDir.Normalize;
  FPinDir.OnNotifyChange := StructureChanged;

  if Assigned(FManager) then
  begin
    bso := TGLBaseSceneObject.Create(FManager);
    bso.AbsolutePosition := FPivotPoint.AsVector;
    bso.AbsoluteDirection := FPinDir.AsVector;
    FPinAndPivotMatrix := bso.AbsoluteMatrix;
    bso.Free;

    { Newton wait from FPinAndPivotMatrix a structure like that:
      First row: the pin direction
      Second and third rows are set to create an orthogonal matrix
      Fourth: The pivot position

      In glscene, the GLBaseSceneObjects direction is the third row,
      because the first row is the right vector (second row is up vector). }
    Line[0] := FPinAndPivotMatrix[2, 0];
    Line[1] := FPinAndPivotMatrix[2, 1];
    Line[2] := FPinAndPivotMatrix[2, 2];
    Line[3] := FPinAndPivotMatrix[2, 3];

    FPinAndPivotMatrix[2, 0] := FPinAndPivotMatrix[0, 0];
    FPinAndPivotMatrix[2, 1] := FPinAndPivotMatrix[0, 1];
    FPinAndPivotMatrix[2, 2] := FPinAndPivotMatrix[0, 2];
    FPinAndPivotMatrix[2, 3] := FPinAndPivotMatrix[0, 3];

    FPinAndPivotMatrix[0, 0] := Line[0];
    FPinAndPivotMatrix[0, 1] := Line[1];
    FPinAndPivotMatrix[0, 2] := Line[2];
    FPinAndPivotMatrix[0, 3] := Line[3];

  end;

end;

procedure TNGDCustomJointBaseDir.WriteToFiler(writer: TWriter);
begin
  inherited;
  with writer do
  begin
    WriteInteger(0);
    // Archive version
  end;
  FPinDir.WriteToFiler(writer);
end;

{ TNGDCustomJointHinge }

constructor TNGDCustomJointHinge.Create(AOwner: TXCollection);
begin
  inherited;
  FMinLimit := -90;
  FMaxLimit := 90;
end;

class function TNGDCustomJointHinge.FriendlyDescription: string;
begin
  Result := 'NGD Custom Hinge joint implementation';
end;

class function TNGDCustomJointHinge.FriendlyName: string;
begin
  Result := 'CustomHinge';
end;

procedure TNGDCustomJointHinge.SetMaxLimit(val: Single);
begin
  if (val >= FMinLimit) then
    FMaxLimit := val;
  if FNewtonUserJoint <> nil then
    HingeSetLimis(FNewtonUserJoint, DegToRad(FMinLimit), DegToRad(FMaxLimit));
end;

procedure TNGDCustomJointHinge.SetMinLimit(val: Single);
begin
  if (val <= FMaxLimit) then
    FMinLimit := val;
  if FNewtonUserJoint <> nil then
    HingeSetLimis(FNewtonUserJoint, DegToRad(FMinLimit), DegToRad(FMaxLimit));
end;

procedure TNGDCustomJointHinge.StructureChanged(Sender: TObject);
begin
  inherited;
  if Assigned(FManager) then
    if self.Classtype = TNGDCustomJointHinge then
      if Assigned(Object1) and Assigned(Object2) then
      begin
        FNewtonUserJoint := CreateCustomHinge(@FPinAndPivotMatrix,
          GetBodyFromGLSceneObject(Object1), GetBodyFromGLSceneObject(Object2));
        HingeEnableLimits(FNewtonUserJoint, 1);
        HingeSetLimis(FNewtonUserJoint, DegToRad(FMinLimit),
          DegToRad(FMaxLimit));
        CollisionState := FCollisionState;
        Stiffness := FStiffness;
      end;
end;

{ TNGDCustomJointSlider }

constructor TNGDCustomJointSlider.Create(AOwner: TXCollection);
begin
  inherited;
  FMinLimit := -10;
  FMaxLimit := 10;
end;

class function TNGDCustomJointSlider.FriendlyDescription: string;
begin
  Result := 'NGD Custom Slider joint implementation';
end;

class function TNGDCustomJointSlider.FriendlyName: string;
begin
  Result := 'CustomSlider';
end;

procedure TNGDCustomJointSlider.SetMaxLimit(val: Single);
begin
  if (val >= FMinLimit) then
    FMaxLimit := val;
  if FNewtonUserJoint <> nil then
    SliderSetLimis(FNewtonUserJoint, FMinLimit, FMaxLimit);
end;

procedure TNGDCustomJointSlider.SetMinLimit(val: Single);
begin
  if (val <= FMaxLimit) then
    FMinLimit := val;
  if FNewtonUserJoint <> nil then
    SliderSetLimis(FNewtonUserJoint, FMinLimit, FMaxLimit);
end;

procedure TNGDCustomJointSlider.StructureChanged(Sender: TObject);
begin
  inherited;
  if Assigned(FManager) then
    if self.Classtype = TNGDCustomJointSlider then
      if Assigned(Object1) and Assigned(Object2) then
      begin
        FNewtonUserJoint := CreateCustomSlider(@FPinAndPivotMatrix,
          GetBodyFromGLSceneObject(Object1), GetBodyFromGLSceneObject(Object2));
        SliderEnableLimits(FNewtonUserJoint, 1);
        SliderSetLimis(FNewtonUserJoint, FMinLimit, FMaxLimit);
        CollisionState := FCollisionState;
        Stiffness := FStiffness;
      end;
end;

initialization

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

vGLNGDObjectRegister := TList.Create;

RegisterXCollectionItemClass(TGLNGDDynamic);
RegisterXCollectionItemClass(TNGDMaterialPair);
RegisterXCollectionItemClass(TGLNGDStatic);

RegisterXCollectionItemClass(TNGDJointBall);
RegisterXCollectionItemClass(TNGDJointHinge);
RegisterXCollectionItemClass(TNGDJointSlider);
RegisterXCollectionItemClass(TNGDJointCorkscrew);
RegisterXCollectionItemClass(TNGDJointUniversal);

RegisterXCollectionItemClass(TNGDCustomJointBall);
RegisterXCollectionItemClass(TNGDCustomJointHinge);
RegisterXCollectionItemClass(TNGDCustomJointSlider);

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

finalization

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

vGLNGDObjectRegister.Free;

UnregisterXCollectionItemClass(TGLNGDDynamic);
UnregisterXCollectionItemClass(TNGDMaterialPair);
UnregisterXCollectionItemClass(TGLNGDStatic);

UnregisterXCollectionItemClass(TNGDJointBall);
UnregisterXCollectionItemClass(TNGDJointHinge);
UnregisterXCollectionItemClass(TNGDJointSlider);
UnregisterXCollectionItemClass(TNGDJointCorkscrew);
UnregisterXCollectionItemClass(TNGDJointUniversal);

UnregisterXCollectionItemClass(TNGDCustomJointBall);
UnregisterXCollectionItemClass(TNGDCustomJointHinge);
UnregisterXCollectionItemClass(TNGDCustomJointSlider);

// CloseNGD;

end.
