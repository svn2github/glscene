//
// This unit is part of the GLScene Project, http://glscene.org
//
{ : GLNGDManager<p>

  A Newton Game Dynamics Manager for GLScene.<p>

  Where can I find ... ?<ul>
  <li>GLScene                                   (http://glscene.org)
  <li>Newton Game Dynamics Engine               (http://newtondynamics.com)
  <li>NewtonImport, a Delphi header translation (http://newtondynamics.com/forum/viewtopic.php?f=9&t=5273#p35865)
  </ul>

  Notes:
  This code is still being developed so any part of it may change at anytime.
  To install use the GLS_NGD?.dpk in the GLScene/Delphi? folder.<p>

  <b>History : </b><font size=-1><ul>
  <li>15/07/10 - FP - Creation by Franck Papouin
  </ul></font>
}

Unit GLNGDManager;

Interface

Uses
  NewtonImport, NewtonImport_JointLibrary // Newton
  , VectorGeometry // PVector TVector TMatrix PMatrix NullHmgVector...
  , VectorLists // TaffineVectorList for SetTree
  // , VectorTypes  //TVector3f...
  , Classes // TComponent Tlist TWriter TReader
  , PersistentClasses, XCollection // TPersistent TXCollection
  , SysUtils // IntToStr  for material in render
  , GLScene, GLManager, GLCrossPlatform, GLCoordinates //
  , GLObjects, GLGeomObjects, GLVectorFileObjects // cube cone freeform...
  , OpenGL1x, OpenGLTokens, GLRenderContextInfo // Base OpenGL
  , GLColor, GLBitmapFont, GLState // For show debug
  , GLFile3DS;

Type
  { Enums }
  TNGDSolverModels = (smExact = 0, smLinear1, smLinear2, smLinear3);
  TNGDFrictionModels = (fmExact = 0, fmAdaptive);
  TNGDPickedModes = (pmAttach = 0, pmMove, pmDetach);

  TGLNGDBehaviour = Class;
  TNGDMaterialPair = Class;
  TNGDMaterials = Class;
  TNGDJointBase = Class;

  { Events }
  TMaterialHitEvent = Procedure(obj0, obj1: TGLBaseSceneObject;
    id0, id1: integer) Of Object;
  TContactProcessEvent = Procedure(NGDMaterialPair: TNGDMaterialPair;
    contact: PNewtonJoint) Of Object;

  { Class }
  TGLNGDManager = Class(TComponent)
  Private
    { Private Declarations }
    FNGDBehaviours: TPersistentObjectList;
    FRenderPoint: TGLRenderPoint;
    FBitmapFont: TGLCustomBitmapFont;
    FMaterials: TNGDMaterials;
    FVisible, FVisibleAtRunTime: Boolean; // Show Debug
    FNewtonWorld: PNewtonWorld;
    FVersion: integer;
    FSolverModel: TNGDSolverModels; // Default=Exact
    FFrictionModel: TNGDFrictionModels; // Default=Exact
    FMinimumFrameRate: integer; // Default=60
    FWorldSizeMin: TGLCoordinates; // Default=-100, -100, -100
    FWorldSizeMax: TGLCoordinates; // Default=100, 100, 100
    FThreadCount: integer; // Default=1
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

  Protected
    { Protected Declarations }
    Procedure Loaded; Override;
    Procedure SetRenderPoint(Const value: TGLRenderPoint);
    Procedure SetBitmapFont(Const value: TGLCustomBitmapFont);
    Procedure RenderEvent(Sender: TObject; Var rci: TRenderContextInfo);
    Procedure RenderPointFreed(Sender: TObject);
    Procedure SetVisible(Const value: Boolean);
    Procedure SetVisibleAtRunTime(Const value: Boolean);
    Procedure SetShowGeometry(Const value: Boolean);
    Procedure SetShowAABB(Const value: Boolean);
    Procedure SetShowMaterialESP(Const value: Boolean);
    Procedure SetShowContact(Const value: Boolean);
    Procedure SetShowJoint(Const value: Boolean);
    Procedure SetShowSpring(Const value: Boolean);
    Function GetNGDBehaviour(Index: integer): TGLNGDBehaviour;
    Property NGDBehaviours[Index: integer]
      : TGLNGDBehaviour Read GetNGDBehaviour;
    Procedure RegisterNGDBehaviour(NGDBehaviour: TGLNGDBehaviour);
    Procedure UnregisterNGDBehaviour(NGDBehaviour: TGLNGDBehaviour);
    Procedure SetSolverModel(val: TNGDSolverModels);
    Procedure SetFrictionModel(val: TNGDFrictionModels);
    Procedure SetMinimumFrameRate(val: integer);
    Procedure SetThreadCount(val: integer);
    Function GetBodyCount: integer;
    Function GetConstraintCount: integer;
    Procedure SetWaterDensity(val: Single);
    Procedure CallBodyIterator;

    // Materials
    Procedure WriteMaterials(stream: TStream);
    Procedure ReadMaterials(stream: TStream);
    Procedure DefineProperties(Filer: TFiler); Override;

    // Events
    Procedure NotifyWorldSizeChange(Sender: TObject);
    Procedure NotifyGravityChange(Sender: TObject);
    Procedure NotifyWaterPlaneChange(Sender: TObject);

  Public
    { Public Declarations }
    Constructor Create(AOwner: TComponent); Override;
    Destructor Destroy; Override;
    Procedure NotifyChange(Sender: TObject);
    Procedure Step(deltatime: Single);
    Function MaterialCreateGroupID: integer;
    // Property NewtonWorld: PNewtonWorld Read FNewtonWorld;

  Published
    { Published Declarations }
    Property RenderPoint: TGLRenderPoint Read FRenderPoint Write SetRenderPoint;
    Property BitmapFont: TGLCustomBitmapFont Read FBitmapFont Write
      SetBitmapFont;
    Property Visible: Boolean Read FVisible Write SetVisible Default False;
    Property VisibleAtRunTime: Boolean Read FVisibleAtRunTime Write
      SetVisibleAtRunTime Default False;
    Property Materials: TNGDMaterials Read FMaterials;
    Property WaterDensity: Single Read FWaterDensity Write SetWaterDensity;
    Property SolverModel: TNGDSolverModels Read FSolverModel Write
      SetSolverModel Default smExact;
    Property FrictionModel: TNGDFrictionModels Read FFrictionModel Write
      SetFrictionModel Default fmExact;
    Property MinimumFrameRate: integer Read FMinimumFrameRate Write
      SetMinimumFrameRate Default 60;
    Property ThreadCount
      : integer Read FThreadCount Write SetThreadCount Default 1;
    Property Version: integer Read FVersion;
    Property BodyCount: integer Read GetBodyCount;
    Property ConstraintCount: integer Read GetConstraintCount;
    Property Gravity: TGLCoordinates Read FGravity Write FGravity;
    Property WorldSizeMin
      : TGLCoordinates Read FWorldSizeMin Write FWorldSizeMin;
    Property WorldSizeMax
      : TGLCoordinates Read FWorldSizeMax Write FWorldSizeMax;
    Property WaterPlane: TGLCoordinates4 Read FWaterPlane Write FWaterPlane;

    // Debugs
    Property ShowGeometry
      : Boolean Read FShowGeometry Write SetShowGeometry Default
      False;
    Property GeomColorDyn: TGLColor Read FGeomColorDyn Write FGeomColorDyn;
    Property GeomColorStat: TGLColor Read FGeomColorStat Write FGeomColorStat;
    Property ShowAABB: Boolean Read FShowAABB Write SetShowAABB Default False;
    Property AABBColor: TGLColor Read FAABBColor Write FAABBColor;
    Property AABBColorSleep
      : TGLColor Read FAABBColorSleep Write FAABBColorSleep;
    Property ShowMaterialESP: Boolean Read FShowMaterialESP Write
      SetShowMaterialESP Default False;
    Property MaterialESPColor
      : TGLColor Read FMaterialESPColor Write FMaterialESPColor;
    // Property ShowContact
    // : Boolean Read FShowContact Write SetShowContact Default False;
    Property ContactColor: TGLColor Read FContactColor Write FContactColor;
    Property ShowJoint
      : Boolean Read FShowJoint Write SetShowJoint Default False;
    Property JointColor: TGLColor Read FJointColor Write FJointColor;
    Property ShowSpring
      : Boolean Read FShowSpring Write SetShowSpring Default False;
    Property SpringColor: TGLColor Read FSpringColor Write FSpringColor;

    // Events
    Property OnMaterialHit: TMaterialHitEvent Read FMaterialHitEvent Write
      FMaterialHitEvent;
  End;

  { : Basis structures for GLScene behaviour style implementations. }
  TGLNGDBehaviour = Class(TGLBehaviour)
  Private
    { Private Declartions }
    FManager: TGLNGDManager;
    FManagerName: String;
    FInitialized: Boolean;
    FOwnerBaseSceneObject: TGLBaseSceneObject;
    FJointRegister: TList;
    FNewtonBody: PNewtonBody;
    FCollision: PNewtonCollision;
    // For Compound [dynamic] and SceneCollision [static]
    FCollisionArray: Array Of PNewtonCollision;
    FMaterialID: integer; // Default=0
    FNewtonBodyMatrix: TMatrix; // Position and Orientation
    FContinuousCollisionMode: Boolean; // Default=False
    FBoundingSphereRadius: Single; // Checked every frame in design time
  Protected
    { Protected Declarations }
    Procedure Initialize; Virtual;
    Procedure Finalize; Virtual;
    Procedure WriteToFiler(writer: TWriter); Override;
    Procedure ReadFromFiler(reader: TReader); Override;
    Procedure Loaded; Override;
    Procedure SetManager(value: TGLNGDManager);
    Procedure RegisterJoint(Joint: TNGDJointBase);
    Procedure UnregisterJoint(Joint: TNGDJointBase);
    Procedure SetNewtonBodyMatrix(val: TMatrix);
    Procedure SetContinuousCollisionMode(val: Boolean);
    Procedure SetMaterialID(val: integer);
    Function GetNewtonBodyMatrix: TMatrix;
    Procedure SetCollision; Virtual;
    Procedure Render(Var rci: TRenderContextInfo); Virtual;
    Function GetCollisionFromBaseSceneObject(SceneObject: TGLBaseSceneObject)
      : PNewtonCollision;

  Public
    { Public Declarations }
    Constructor Create(AOwner: TXCollection); Override;
    Destructor Destroy; Override;
    Procedure NotifyChange(Sender: TObject);
    Procedure Reinitialize;
    Property Initialized: Boolean Read FInitialized;
    Class Function UniqueItem: Boolean; Override;
    Property NewtonBodyMatrix: TMatrix Read GetNewtonBodyMatrix Write
      SetNewtonBodyMatrix;

  Published
    { Published Declarations }
    Property Manager: TGLNGDManager Read FManager Write SetManager;
    Property ContinuousCollisionMode
      : Boolean Read FContinuousCollisionMode Write
      SetContinuousCollisionMode Default False;
    Property MaterialID: integer Read FMaterialID Write SetMaterialID Default 0;
  End;

  TGLNGDDynamic = Class(TGLNGDBehaviour)
  Private
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
    FUseVelovity: Boolean;
    FUseOmega: Boolean;

    // Read Only
    FVolume: Single;
    FMass: Single;
    FAppliedForce: TGLCoordinates;
    FAppliedTorque: TGLCoordinates;
    FAppliedOmega: TGLCoordinates;
    FAppliedVelocity: TGLCoordinates;

  Protected
    { Protected Declarations }
    Procedure SetAutoSleep(val: Boolean);
    Procedure SetLinearDamping(val: Single);
    Procedure SetDensity(val: Single); Virtual;
    Procedure Initialize; Override;
    Procedure Finalize; Override;
    Procedure WriteToFiler(writer: TWriter); Override;
    Procedure ReadFromFiler(reader: TReader); Override;
    Procedure Loaded; Override;
    Procedure Render(Var rci: TRenderContextInfo); Override;
    Procedure SetCollision; Override;
    Procedure SetUpVector(val: Boolean);

    // Events
    Procedure NotifyCenterOfMassChange(Sender: TObject);
    Procedure NotifyAngularDampingChange(Sender: TObject);
    Procedure NotifyUpVectorDirectionChange(Sender: TObject);

  Public
    { Public Declarations }
    Constructor Create(AOwner: TXCollection); Override;
    Destructor Destroy; Override;
    Procedure Pick(pickpoint: TVector; Mode: TNGDPickedModes);

    Class Function FriendlyName: String; Override;

  Published
    { Published Declarations }
    Property Force: TGLCoordinates Read FForce Write FForce;
    Property Torque: TGLCoordinates Read FTorque Write FTorque;
    Property Omega: TGLCoordinates Read FOmega Write FOmega;
    Property Velocity: TGLCoordinates Read FVelocity Write FVelocity;
    Property CenterOfMass
      : TGLCoordinates Read FCenterOfMass Write FCenterOfMass;
    Property AutoSleep: Boolean Read FAutoSleep Write SetAutoSleep Default True;
    Property LinearDamping: Single Read FLinearDamping Write SetLinearDamping;
    Property AngularDamping
      : TGLCoordinates Read FAngularDamping Write FAngularDamping;
    Property Density: Single Read FDensity Write SetDensity;
    Property UpVector: Boolean Read FUpVector Write SetUpVector Default False;
    Property UpVectorDirection
      : TGLCoordinates Read FUpVectorDirection Write FUpVectorDirection;
    Property UseVelovity
      : Boolean Read FUseVelovity Write FUseVelovity Default False;
    Property UseOmega: Boolean Read FUseOmega Write FUseOmega Default False;

    // Read Only
    Property AppliedOmega: TGLCoordinates Read FAppliedOmega;
    Property AppliedVelocity: TGLCoordinates Read FAppliedVelocity;
    Property AppliedForce: TGLCoordinates Read FAppliedForce;
    Property AppliedTorque: TGLCoordinates Read FAppliedTorque;
    Property Volume: Single Read FVolume;
    Property Mass: Single Read FMass;
  End;

  TGLNGDStatic = Class(TGLNGDBehaviour)
  Private
    { Private Declarations }
    FHeightFieldWordArray: Array Of UInt16;

  Protected
    { Protected Declarations }
    Procedure SetCollision; Override;
    Procedure Render(Var rci: TRenderContextInfo); Override;
    Function GetTree(optimize: Boolean; scaleXYZ: Single): PNewtonCollision;

  Public
    { Public Declarations }
    Class Function FriendlyName: String; Override;
    Procedure SetHeightField(heightArray: Array Of UInt16; x: integer;
      y: integer; xScale: Single; yScale: Single);

  Published
    { Published Declarations }
  End;

  { : An XCollection decendant for NGD Materials. }
  TNGDMaterials = Class(TXCollection)
  Protected
    { Protected Declarations }
    Function GetMaterialPair(Index: integer): TNGDMaterialPair;

  Public
    { Public Declarations }
    Class Function ItemsClass: TXCollectionItemClass; Override;
    Procedure Initialize;
    Procedure Finalize;
    Property MaterialPair[Index: integer]
      : TNGDMaterialPair Read GetMaterialPair; Default;
  End;

  TNGDMaterialPair = Class(TXCollectionItem)
  Private
    { Private Declarations }
    FManager: TGLNGDManager;
    FSoftness: Single; // 0.1
    FElasticity: Single; // 0.4
    FCollidable: Boolean; // 1
    FStaticFriction: Single; // 0.9
    FKineticFriction: Single; // 0.5
    Fid0, Fid1: integer;
    FManagerName: String;
    FInitialized: Boolean;

    // Event
    FContactProcessEvent: TContactProcessEvent;

  Protected
    { Protected Declarations }
    Procedure SetSoftness(val: Single);
    Procedure SetElasticity(val: Single);
    Procedure SetCollidable(val: Boolean);
    Procedure SetStaticFriction(val: Single);
    Procedure SetKineticFriction(val: Single);
    Procedure WriteToFiler(writer: TWriter); Override;
    Procedure ReadFromFiler(reader: TReader); Override;
    Procedure Loaded; Override;
    Procedure SetManager(Const value: TGLNGDManager);
    Procedure Initialize; Virtual;
    Procedure Finalize; Virtual;

  Public
    { Public Declarations }
    Constructor Create(AOwner: TXCollection); Override;
    Destructor Destroy; Override;
    Class Function FriendlyName: String; Override;
    Class Function FriendlyDescription: String; Override;
    Property Initialized: Boolean Read FInitialized;
    Property OnContactProcess
      : TContactProcessEvent Read FContactProcessEvent
      Write FContactProcessEvent;
  Published
    { Published Declarations }
    Property Softness: Single Read FSoftness Write SetSoftness;
    Property Elasticity: Single Read FElasticity Write SetElasticity;
    Property Collidable: Boolean Read FCollidable Write SetCollidable;
    Property StaticFriction
      : Single Read FStaticFriction Write SetStaticFriction;
    Property KineticFriction
      : Single Read FKineticFriction Write SetKineticFriction;
    Property id0: integer Read Fid0 Write Fid0 Default 0;
    Property id1: integer Read Fid1 Write Fid1 Default 0;

    Property Manager: TGLNGDManager Read FManager Write SetManager;
  End;

  { : An XCollection decendant for NGD Joints. }
  TNGDJoints = Class(TXCollection)
  Protected
    { Protected Declarations }
    Function GetJoint(Index: integer): TNGDJointBase;

  Public
    { Public Declarations }
    Class Function ItemsClass: TXCollectionItemClass; Override;
    Procedure Initialize;
    Procedure Finalize;
    Property Joint[Index: integer]: TNGDJointBase Read GetJoint; Default;
  End;

  { : Component front-end for storing NGD Joints. }
  TGLNGDJointList = Class(TComponent)
  Private
    { Private Declarations }
    FJoints: TNGDJoints;

  Protected
    { Protected Declarations }
    Procedure WriteJoints(stream: TStream);
    Procedure ReadJoints(stream: TStream);
    Procedure DefineProperties(Filer: TFiler); Override;
    Procedure Loaded; Override;
    Procedure Notification(AComponent: TComponent; Operation: TOperation);
      Override;

  Public
    { Public Declarations }
    Constructor Create(AOwner: TComponent); Override;
    Destructor Destroy; Override;

  Published
    { Published Declarations }
    Property Joints: TNGDJoints Read FJoints;
  End;

  TNGDJointBase = Class(TXCollectionItem)
  Private
    { Private Declarations }
    FObject1, FObject2: TGLBaseSceneObject;
    FManager: TGLNGDManager;
    FInitialized: Boolean;
    FObject1Name, FObject2Name, FManagerName: String;

  Protected
    { Protected Declarations }
    Procedure WriteToFiler(writer: TWriter); Override;
    Procedure ReadFromFiler(reader: TReader); Override;
    Procedure Loaded; Override;
    Procedure Render; Virtual;
    Procedure RegisterJointWithObject(Obj: TGLBaseSceneObject);
    Procedure UnregisterJointWithObject(Obj: TGLBaseSceneObject);
    Procedure SetManager(Const value: TGLNGDManager);
    Procedure SetObject1(Const value: TGLBaseSceneObject);
    Procedure SetObject2(Const value: TGLBaseSceneObject);
    Procedure Initialize; Virtual;
    Procedure Finalize; Virtual;
    Property Initialized: Boolean Read FInitialized;

  Public
    { Public Declarations }
    Constructor Create(AOwner: TXCollection); Override;
    Destructor Destroy; Override;
    Procedure StructureChanged(Sender: TObject); Virtual;

  Published
    { Published Declarations }
    Property Manager: TGLNGDManager Read FManager Write SetManager;
    Property Object1: TGLBaseSceneObject Read FObject1 Write SetObject1;
    Property Object2: TGLBaseSceneObject Read FObject2 Write SetObject2;
  End;

  { : NGD ball joint implementation. }
  TNGDJointBall = Class(TNGDJointBase)
  Private
    { Private Declarations }
    FNewtonJoint: PNewtonJoint;
    FPivotPoint: TGLCoordinates;
    FStiffness: Single; // Default=0.9
    FCollisionState: Boolean; // Default=False

  Protected
    { Protected Declarations }
    Procedure WriteToFiler(writer: TWriter); Override;
    Procedure ReadFromFiler(reader: TReader); Override;
    Procedure Render; Override;
    Procedure SetStiffness(val: Single); Virtual;
    Procedure SetCollisionState(val: Boolean); Virtual;

  Public
    { Public Declarations }
    Constructor Create(AOwner: TXCollection); Override;
    Destructor Destroy; Override;
    Procedure StructureChanged(Sender: TObject); Override;
    Class Function FriendlyName: String; Override;
    Class Function FriendlyDescription: String; Override;

  Published
    { Published Declarations }
    Property Stiffness: Single Read FStiffness Write SetStiffness;
    Property CollisionState
      : Boolean Read FCollisionState Write SetCollisionState Default True;
    Property PivotPoint: TGLCoordinates Read FPivotPoint Write FPivotPoint;
  End;

  { : NGD hinge joint implementation. }
  TNGDJointHinge = Class(TNGDJointBall)
  Private
    { Private Declarations }
    FPinDir: TGLCoordinates;

  Protected
    { Protected Declarations }
    Procedure WriteToFiler(writer: TWriter); Override;
    Procedure ReadFromFiler(reader: TReader); Override;
    Procedure Render; Override;

  Public
    { Public Declarations }
    Constructor Create(AOwner: TXCollection); Override;
    Destructor Destroy; Override;
    Procedure StructureChanged(Sender: TObject); Override;
    Class Function FriendlyName: String; Override;
    Class Function FriendlyDescription: String; Override;

  Published
    { Published Declarations }
    Property PinDir: TGLCoordinates Read FPinDir Write FPinDir;
  End;

  { : NGD slider joint implementation. }
  TNGDJointSlider = Class(TNGDJointHinge)
  Private
    { Private Declarations }

  Protected
    { Protected Declarations }

  Public
    { Public Declarations }
    Procedure StructureChanged(Sender: TObject); Override;
    Class Function FriendlyName: String; Override;
    Class Function FriendlyDescription: String; Override;
  End;

  { : NGD Corkscrew joint implementation. }
  TNGDJointCorkscrew = Class(TNGDJointHinge)
  Private
    { Private Declarations }

  Protected
    { Protected Declarations }

  Public
    { Public Declarations }
    Procedure StructureChanged(Sender: TObject); Override;
    Class Function FriendlyName: String; Override;
    Class Function FriendlyDescription: String; Override;
  End;

  { : NGD hinge joint implementation. }
  TNGDJointUniversal = Class(TNGDJointHinge)
  Private
    { Private Declarations }
    FPinDir2: TGLCoordinates;

  Protected
    { Protected Declarations }
    Procedure WriteToFiler(writer: TWriter); Override;
    Procedure ReadFromFiler(reader: TReader); Override;
    Procedure Render; Override;

  Public
    { Public Declarations }
    Constructor Create(AOwner: TXCollection); Override;
    Destructor Destroy; Override;
    Procedure StructureChanged(Sender: TObject); Override;
    Class Function FriendlyName: String; Override;
    Class Function FriendlyDescription: String; Override;

  Published
    { Published Declarations }
    Property PinDir2: TGLCoordinates Read FPinDir2 Write FPinDir2;
  End;

  { : NGD Custom joint base implementation. }
  TNGDCustomJointBase = Class(TNGDJointBase)
  Private
    { Private Declarations }
    FNewtonUserJoint: PNewtonUserJoint;
    FPivotPoint: TGLCoordinates;
    FPinAndPivotMatrix: TMatrix;
    FStiffness: Single; // Default=0.9
    FCollisionState: Boolean; // Default=False

  Protected
    { Protected Declarations }
    FMinLimit, FMaxLimit: Single;
    Procedure WriteToFiler(writer: TWriter); Override;
    Procedure ReadFromFiler(reader: TReader); Override;
    Procedure Render; Override;
    Procedure SetMaxLimit(val: Single); Virtual;
    Procedure SetMinLimit(val: Single); Virtual;
    Procedure SetStiffness(val: Single); Virtual;
    Procedure SetCollisionState(val: Boolean); Virtual;

  Public
    { Public Declarations }
    Constructor Create(AOwner: TXCollection); Override;
    Destructor Destroy; Override;
    Procedure StructureChanged(Sender: TObject); Override;

  Published
    { Published Declarations }
    Property Stiffness: Single Read FStiffness Write SetStiffness;
    Property CollisionState
      : Boolean Read FCollisionState Write SetCollisionState Default True;
    Property PivotPoint: TGLCoordinates Read FPivotPoint Write FPivotPoint;
  End;

  { : NGD Custom joint ball implementation. }
  TNGDCustomJointBall = Class(TNGDCustomJointBase)
  Private
    { Private Declarations }
    FConeAngle: Single;

  Protected
    { Protected Declarations }
    Procedure WriteToFiler(writer: TWriter); Override;
    Procedure ReadFromFiler(reader: TReader); Override;
    Procedure SetConeAngle(val: Single);
    Procedure SetMaxLimit(val: Single); Override;
    Procedure SetMinLimit(val: Single); Override;

  Public
    { Public Declarations }
    Constructor Create(AOwner: TXCollection); Override;
    Procedure StructureChanged(Sender: TObject); Override;
    Class Function FriendlyName: String; Override;
    Class Function FriendlyDescription: String; Override;

  Published
    { Published Declarations }
    Property ConeAngle: Single Read FConeAngle Write SetConeAngle;
    Property MinTwistAngle: Single Read FMinLimit Write SetMinLimit;
    Property MaxTwistAngle: Single Read FMaxLimit Write SetMaxLimit;
  End;

  { : NGD Custom joint hinge implementation. }
  TNGDCustomJointBaseDir = Class(TNGDCustomJointBase)
  Private
    { Private Declarations }
    FPinDir: TGLCoordinates;

  Protected
    { Protected Declarations }
    Procedure WriteToFiler(writer: TWriter); Override;
    Procedure ReadFromFiler(reader: TReader); Override;
    Procedure Render; Override;

  Public
    { Public Declarations }
    Constructor Create(AOwner: TXCollection); Override;
    Destructor Destroy; Override;
    Procedure StructureChanged(Sender: TObject); Override;

  Published
    { Published Declarations }
    Property PinDir: TGLCoordinates Read FPinDir Write FPinDir;
  End;

  { : NGD Custom joint hinge implementation. }
  TNGDCustomJointHinge = Class(TNGDCustomJointBaseDir)
  Private
    { Private Declarations }

  Protected
    { Protected Declarations }
    Procedure SetMaxLimit(val: Single); Override;
    Procedure SetMinLimit(val: Single); Override;

  Public
    { Public Declarations }
    Constructor Create(AOwner: TXCollection); Override;
    Procedure StructureChanged(Sender: TObject); Override;
    Class Function FriendlyName: String; Override;
    Class Function FriendlyDescription: String; Override;

  Published
    { Published Declarations }
    Property MinAngle: Single Read FMinLimit Write SetMinLimit;
    Property MaxAngle: Single Read FMaxLimit Write SetMaxLimit;
  End;

  { : NGD Custom joint slider implementation. }
  TNGDCustomJointSlider = Class(TNGDCustomJointBaseDir)
  Private
    { Private Declarations }

  Protected
    { Protected Declarations }
    Procedure SetMaxLimit(val: Single); Override;
    Procedure SetMinLimit(val: Single); Override;

  Public
    { Public Declarations }
    Constructor Create(AOwner: TXCollection); Override;
    Procedure StructureChanged(Sender: TObject); Override;
    Class Function FriendlyName: String; Override;
    Class Function FriendlyDescription: String; Override;

  Published
    { Published Declarations }
    Property MinDist: Single Read FMinLimit Write SetMinLimit;
    Property MaxDist: Single Read FMaxLimit Write SetMaxLimit;
  End;

  { Pure Newton Callback }
Procedure NewtonBodyIterator(Const body: PNewtonBody); Cdecl;

Function NewtonGetBuoyancyPlane(Const collisionID: integer; context: Pointer;
  Const globalSpaceMatrix: PFloat; globalSpacePlane: PVector): integer; Cdecl;

Procedure NewtonApplyForceAndTorque(Const body: PNewtonBody; timestep: Float;
  threadIndex: integer); Cdecl;

Procedure NewtonSetTransform(Const body: PNewtonBody;
  Const matrix: NewtonImport.PFloat; threadIndex: integer); Cdecl;

Procedure NewtonBodyDestructor(Const body: PNewtonBody); Cdecl;

Procedure NewtonCollisionIterator(Const body: PNewtonBody;
  VertexCount: integer; Const FaceArray: PFloat; FaceId: integer); Cdecl;

Procedure NewtonBodyLeaveWorld(Const body: PNewtonBody; threadIndex: integer);
  Cdecl;

Function NewtonOnAABBOverlap(Const material: PNewtonMaterial;
  Const body0: PNewtonBody; Const body1: PNewtonBody;
  threadIndex: integer): integer; Cdecl;

Procedure NewtonContactsProcess(Const contact: PNewtonJoint; timestep: Float;
  threadIndex: integer); Cdecl;

// GLNGDObject register methods (used for joint object persistence)
Procedure RegisterGLSceneObject(anObject: TGLBaseSceneObject);
Procedure UnregisterGLSceneObject(anObject: TGLBaseSceneObject);
Function GetGLSceneObject(anObjectName: String): TGLBaseSceneObject;
Function GetBodyFromGLSceneObject(anObject: TGLBaseSceneObject): PNewtonBody;

Var
  vGLNGDObjectRegister: TList;

Implementation

{ CallBack }

// API Callback wich iterate list of NewtonBody to apply special stuff
// Prefering iterate BehaviorsList from Manager
// Do nothing for the moment
// Called by manager with CallBodyIterator.
Procedure NewtonBodyIterator(Const body: PNewtonBody);
Begin
  // NewtonBodySetFreezeState(body, 1);
End;

// API callback called by each body in 'NewtonApplyForceAndTorque' to set the
// waterplane equation. For the moment the plane is the same for everybody.
// This function could be used to create physics waves in the futur.
Function NewtonGetBuoyancyPlane(Const collisionID: integer; context: Pointer;
  Const globalSpaceMatrix: PFloat; globalSpacePlane: PVector): integer;
Var
  NGDManager: TGLNGDManager;
Begin
  // the normal for the plane is just a unit vector.
  // the distance along this normal, to the origin. y0-ywater=-ywater
  // When the parameter buoyancyPlane is set to NULL, the body is considered
  // to completely immersed in the fluid. This can be used to simulate boats
  // and lighter than air vehicles etc..
  NGDManager := context;
  globalSpacePlane^ := (NGDManager.FWaterPlane.AsVector);
  Result := 1; // Boolean, 1 to apply Buoyancy, 0 to ignore
End;

// Called after Manager.Step, Runtime only
Procedure NewtonApplyForceAndTorque(Const body: PNewtonBody; timestep: Float;
  threadIndex: integer);
Var
  NGDDynamicBody: TGLNGDDynamic;
  Gravity: TVector;
  DensityCorrection, FluidDensity: Single;
  // forceout: TVector;
Begin
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
  NewtonBodyAddForce(body, @(Gravity));

  // Add Buoyancy      RQ: Volume*Density=Mass
  If NGDDynamicBody.Manager.WaterDensity > 0 Then
  Begin
    DensityCorrection := 1;

    // Buoyancy for sphere does not work very well in Newton,
    // So we put a correction wich is a constante.
    // We have the same problem with the Capsule-Collision but the correction
    // depend of the height and radius.
    // Create ConvexHull {with capsule in glfeedback to a freeform}
    // to resolve this bug.
    If (NGDDynamicBody.OwnerBaseSceneObject Is TGLSphere) Then
      DensityCorrection := 1.43;

    FluidDensity := NGDDynamicBody.Manager.WaterDensity * DensityCorrection /
      NGDDynamicBody.FMass;
    NewtonBodyAddBuoyancyForce(body, FluidDensity, 0.5, 0.5, @(Gravity),
      @GLNGDManager.NewtonGetBuoyancyPlane, NGDDynamicBody.Manager);
  End;

  If NGDDynamicBody.FUseVelovity Then
    NewtonBodySetVelocity(body, @(NGDDynamicBody.FVelocity.AsVector));
  { NewtonBodyCalculateInverseDynamicsForce(body, timestep,
    @(NGDDynamicBody.FVelocity.AsVector), @forceout);
    NewtonBodySetForce(body, @forceout); }

  If NGDDynamicBody.FUseOmega Then
    NewtonBodySetOmega(body, @(NGDDynamicBody.FOmega.AsVector));

End;

// Runtime Only
Procedure NewtonSetTransform(Const body: PNewtonBody;
  Const matrix: NewtonImport.PFloat; threadIndex: integer);
Var
  NGDDynamicBody: TGLNGDDynamic;
  ObjScale: TVector;
Begin
  NGDDynamicBody := NewtonBodyGetUserData(body);

  // The Newton API does not support scale [scale modifie value in matrix],
  // so this line reset scale of the glsceneObject to (1,1,1)
  // to avoid crashing the application
  ObjScale := NGDDynamicBody.OwnerBaseSceneObject.Scale.AsVector;
  If (ObjScale[0] > 1.1) Or (ObjScale[1] > 1.1) Or (ObjScale[2] > 1.1) Or
    (ObjScale[0] < 0.9) Or (ObjScale[1] < 0.9) Or (ObjScale[2] < 0.9) Then
  Begin
    NGDDynamicBody.OwnerBaseSceneObject.Scale.SetVector(1, 1, 1);
    NGDDynamicBody.SetNewtonBodyMatrix
      (NGDDynamicBody.OwnerBaseSceneObject.AbsoluteMatrix);
  End
  Else
    // Make the Position and orientation of the glscene-Object relative to the
    // NewtonBody position and orientation.
    NGDDynamicBody.OwnerBaseSceneObject.AbsoluteMatrix := pMatrix(matrix)^;

End;

// This API Callback is set by default, but do nothing for the moment.
Procedure NewtonBodyDestructor(Const body: PNewtonBody);
Begin
  //
End;

// The Manager use this CallBack from RenderEvent procedure
// in Runtime and design time
Procedure NewtonCollisionIterator(Const body: PNewtonBody;
  VertexCount: integer; Const FaceArray: PFloat; FaceId: integer);
Var
  i: integer;
  v0, v1: Array [0 .. 2] Of Single;
  vA: Array Of Single;
Begin
  // This algorithme draw Collision Shape for Debuggin.
  // Taken to Sascha Willems in SDLNewton-Demo at
  // http://www.saschawillems.de/?page_id=82
  If VertexCount = 0 Then
    exit;
  SetLength(vA, VertexCount * 3);
  Move(FaceArray^, vA[0], VertexCount * 3 * SizeOf(Single));
  v0[0] := vA[(VertexCount - 1) * 3];
  v0[1] := vA[(VertexCount - 1) * 3 + 1];
  v0[2] := vA[(VertexCount - 1) * 3 + 2];
  For i := 0 To VertexCount - 1 Do
  Begin
    v1[0] := vA[i * 3];
    v1[1] := vA[i * 3 + 1];
    v1[2] := vA[i * 3 + 2];
    glVertex3f(v0[0], v0[1], v0[2]);
    glVertex3f(v1[0], v1[1], v1[2]);
    v0 := v1;
  End;
End;

// API Callback, When NewtonBody Leave the NewtonWorld
// [size of NewtonWorld Defined in manager]
// Do nothing for the moment
Procedure NewtonBodyLeaveWorld(Const body: PNewtonBody; threadIndex: integer);
Begin
  // When The body is leaving the world we change its freezestate to make
  // debuggin color more clear in mind.
  // But there is no way to remake the body active after this callback.
  // Another way would be to destroy body and behaviors and glscene-Object.
  // Or simply reset Body Position and orientation by using
  // NewtonBodySetMatrix API function before it leave the world.
  NewtonBodySetFreezeState(body, 1);
End;

// API Callback Runtime Only. Raise two even if the application want to
// apply special effect like conveyor...
Procedure NewtonContactsProcess(Const contact: PNewtonJoint; timestep: Float;
  threadIndex: integer);
Var
  NGDMaterialPair: TNGDMaterialPair;
  obj0, obj1: TGLBaseSceneObject;
  NGDBehaviour: TGLNGDBehaviour;
Begin

  NGDMaterialPair := NewtonMaterialGetMaterialPairUserData
    (NewtonContactGetMaterial(NewtonContactJointGetFirstContact(contact)));

  // Raise material's event when two Bodies Collide
  With (NGDMaterialPair) Do
  Begin
    If Assigned(FContactProcessEvent) Then
      FContactProcessEvent(NGDMaterialPair, contact);
  End;

  // Raise manager's event when two Bodies Collide
  NGDBehaviour := NewtonBodyGetUserData(NewtonJointGetBody0(contact));
  obj0 := NGDBehaviour.OwnerBaseSceneObject;
  NGDBehaviour := NewtonBodyGetUserData(NewtonJointGetBody1(contact));
  obj1 := NGDBehaviour.OwnerBaseSceneObject;
  With (NGDMaterialPair.Manager) Do
  Begin
    If Assigned(FMaterialHitEvent) Then
      FMaterialHitEvent(obj0, obj1, NGDMaterialPair.Fid0, NGDMaterialPair.Fid1);
  End;
End;

// This API Callback is called juste before NewtonContactsProcess.
// This function can be used to get or set information of body and behaviors.
Function NewtonOnAABBOverlap(Const material: PNewtonMaterial;
  Const body0: PNewtonBody; Const body1: PNewtonBody;
  threadIndex: integer): integer;
Begin
  // Boolean, if 1, continue
  // if 0, the two body won't collide and will go trough each other.
  Result := 1
End;

{ Register }
// RegisterGLSceneObject
// Called when NGDBehaviors is created
Procedure RegisterGLSceneObject(anObject: TGLBaseSceneObject);
Begin
  If vGLNGDObjectRegister.IndexOf(anObject) = -1 Then
    vGLNGDObjectRegister.Add(anObject);
End;

// UnregisterGLSceneObject
// Called when NGDBehaviors is destroyed
Procedure UnregisterGLSceneObject(anObject: TGLBaseSceneObject);
Begin
  vGLNGDObjectRegister.Remove(anObject);
End;

// GetGLSceneObject
// Use the list of registered glscene-Object
Function GetGLSceneObject(anObjectName: String): TGLBaseSceneObject;
Var
  i: integer;
Begin
  Result := Nil;
  For i := 0 To vGLNGDObjectRegister.Count - 1 Do
    If TGLBaseSceneObject(vGLNGDObjectRegister[i])
      .GetNamePath = anObjectName Then
    Begin
      Result := vGLNGDObjectRegister[i];
      exit;
    End;
End;

// GetBodyFromGLSceneObject
// Useful for NGDjoint who need FNewtonBody, a private pointer.
Function GetBodyFromGLSceneObject(anObject: TGLBaseSceneObject): PNewtonBody;
Var
  temp: TGLNGDBehaviour;
Begin
  Result := Nil;
  If Assigned(anObject) Then
  Begin
    temp := TGLNGDBehaviour(anObject.Behaviours.GetByClass(TGLNGDBehaviour));
    If temp <> Nil Then
      Result := temp.FNewtonBody;
  End;
End;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

{ TGLNGDManager }

Constructor TGLNGDManager.Create(AOwner: TComponent);
Var
  minworld, maxworld: TVector;
Begin
  Inherited;
  FRenderPoint := Nil;
  FBitmapFont := Nil;
  FNGDBehaviours := TPersistentObjectList.Create;
  FVisible := True;
  FVisibleAtRunTime := False;
  FMaterials := TNGDMaterials.Create(self);
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
  FNewtonWorld := NewtonCreate(Nil, Nil);
  FVersion := NewtonWorldGetVersion(FNewtonWorld);

  // This is to prevent body out the world at startTime
  minworld := VectorMake(-1E50, -1E50, -1E50);
  maxworld := VectorMake(1E50, 1E50, 1E50);
  NewtonSetWorldSize(FNewtonWorld, @minworld, @maxworld);

  NewtonSetBodyLeaveWorldEvent(FNewtonWorld, @NewtonBodyLeaveWorld);
  NewtonWorldSetUserData(FNewtonWorld, self);
  FMaterialHitEvent := Nil;

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

End;

Destructor TGLNGDManager.Destroy;
Begin
  RenderPoint := Nil;
  BitmapFont := Nil;

  // Unregister everything
  While FNGDBehaviours.Count > 0 Do
    NGDBehaviours[0].Manager := Nil;

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
  NewtonSetBodyLeaveWorldEvent(FNewtonWorld, Nil);
  NewtonMaterialDestroyAllGroupID(FNewtonWorld);
  NewtonDestroy(FNewtonWorld);
  FNewtonWorld := Nil;

  DeregisterManager(self);
  Inherited;
End;

Procedure TGLNGDManager.Loaded;
Var
  maxMaterialID: integer;
  i: integer;
Begin
  Inherited;
  NotifyWorldSizeChange(self);
  // NotifyGravityChange(self);
  // NotifyWaterPlaneChange(self);

  maxMaterialID := 0;

  // Get the max MaterialGroupID
  For i := 0 To FMaterials.Count - 1 Do
  Begin
    maxMaterialID := MaxInteger(maxMaterialID, FMaterials[i].id0);
    maxMaterialID := MaxInteger(maxMaterialID, FMaterials[i].id1);
  End;

  // Get the max MaterialID
  For i := 0 To FNGDBehaviours.Count - 1 Do
    maxMaterialID := MaxInteger(maxMaterialID, NGDBehaviours[i].MaterialID);

  // Create GroupID
  For i := 0 To maxMaterialID - 1 Do
    MaterialCreateGroupID;

  For i := 0 To FMaterials.Count - 1 Do
  Begin
    FMaterials[i].Loaded;
  End;
End;

Procedure TGLNGDManager.ReadMaterials(stream: TStream);
Var
  reader: TReader;
Begin
  reader := TReader.Create(stream, 16384);
  Try
    Materials.ReadFromFiler(reader);
  Finally
    reader.Free;
  End;
End;

Procedure TGLNGDManager.WriteMaterials(stream: TStream);
Var
  writer: TWriter;
Begin
  writer := TWriter.Create(stream, 16384);
  Try
    Materials.WriteToFiler(writer);
  Finally
    writer.Free;
  End;
End;

Procedure TGLNGDManager.DefineProperties(Filer: TFiler);
Begin
  Inherited;
  Filer.DefineBinaryProperty('NGDMaterialsData', ReadMaterials, WriteMaterials,
    (Assigned(FMaterials) And (FMaterials.Count > 0)));
End;

Function TGLNGDManager.GetBodyCount: integer;
Begin
  If Assigned(FNewtonWorld) Then
    Result := NewtonWorldGetBodyCount(FNewtonWorld)
  Else
    Result := FNGDBehaviours.Count;
End;

Function TGLNGDManager.GetConstraintCount: integer;
Begin
  If Assigned(FNewtonWorld) Then
    Result := NewtonWorldGetConstraintCount(FNewtonWorld)
    // Constraint is the number of joint
  Else
    Result := 0;
End;

Function TGLNGDManager.GetNGDBehaviour(Index: integer): TGLNGDBehaviour;
Begin
  Result := TGLNGDBehaviour(FNGDBehaviours[Index]);
End;

Procedure TGLNGDManager.CallBodyIterator;
Begin
  If Assigned(FNewtonWorld) Then
    NewtonWorldForEachBodyInAABBDo(FNewtonWorld, @(FWorldSizeMin.AsVector),
      @(FWorldSizeMax.AsVector), @NewtonBodyIterator, Nil);
End;

Function TGLNGDManager.MaterialCreateGroupID: integer;
Begin
  If Assigned(FNewtonWorld) Then
    Result := NewtonMaterialCreateGroupID(FNewtonWorld)
  Else
    Result := 0;
End;

Procedure TGLNGDManager.NotifyChange(Sender: TObject);
Begin
  If Assigned(RenderPoint) Then
    RenderPoint.StructureChanged;
End;

Procedure TGLNGDManager.RegisterNGDBehaviour(NGDBehaviour: TGLNGDBehaviour);
Begin
  FNGDBehaviours.Add(NGDBehaviour);
  NotifyChange(self);
End;

Procedure TGLNGDManager.UnregisterNGDBehaviour(NGDBehaviour: TGLNGDBehaviour);
Begin
  FNGDBehaviours.Remove(NGDBehaviour);
  NotifyChange(self);
End;

Procedure TGLNGDManager.RenderEvent(Sender: TObject;
  Var rci: TRenderContextInfo);
Var
  i: integer;
Begin
  If Not Visible Then
    exit;
  If Not(csDesigning In ComponentState) Then
    If Not VisibleAtRunTime Then
      exit;

  glPushAttrib(GL_LINE_WIDTH);
  glPushAttrib(GL_LINE_STIPPLE);
  glDisable(GL_TEXTURE_2D);
  glDisable(GL_LIGHTING);
  glEnable(GL_LINE_STIPPLE);

  // Equivalent to ForEachBodyINAABB
  For i := 0 To FNGDBehaviours.Count - 1 Do
  Begin
    If NewtonBodyGetSleepState(NGDBehaviours[i].FNewtonBody) = 1 Then
      glColor4fv(GeomColorStat.AsAddress) // red
    Else
      glColor4fv(GeomColorDyn.AsAddress); // green

    If (csDesigning In ComponentState) Then
    Begin
      If (NGDBehaviours[i] Is TGLNGDStatic) Then
        glColor4fv(GeomColorStat.AsAddress); // red

      If (NGDBehaviours[i] Is TGLNGDDynamic) Then
        If (NGDBehaviours[i] As TGLNGDDynamic).FDensity = 0 Then
          glColor4fv(GeomColorStat.AsAddress); // red
    End;

    NGDBehaviours[i].Render(rci);
  End;

  If FShowContact Then
  Begin
    glLineWidth(1);
    glLineStipple(1, $FFFF); // full opengl lines

    glBegin(GL_LINES);
    //
    glEnd;
  End;

  glDisable(GL_LINE_STIPPLE);
  glEnable(GL_TEXTURE_2D);
  glEnable(GL_LIGHTING);
  glPopAttrib;
  glPopAttrib;
End;

Procedure TGLNGDManager.RenderPointFreed(Sender: TObject);
Begin
  FRenderPoint := Nil;
End;

Procedure TGLNGDManager.SetBitmapFont(Const value: TGLCustomBitmapFont);
Begin
  If FBitmapFont <> value Then
  Begin
    FBitmapFont := value;
    NotifyChange(self);
  End;
End;

Procedure TGLNGDManager.SetFrictionModel(val: TNGDFrictionModels);
Begin
  If Assigned(FNewtonWorld) Then
    NewtonSetFrictionModel(FNewtonWorld, Ord(val));
  FFrictionModel := val;
End;

Procedure TGLNGDManager.NotifyGravityChange(Sender: TObject);
Begin
  // Nothing here for the moment
End;

Procedure TGLNGDManager.SetMinimumFrameRate(val: integer);
Begin
  If (val >= 60) AND (val <= 1000) Then
  Begin
    NewtonSetMinimumFrameRate(FNewtonWorld, val);
    FMinimumFrameRate := val;
  End;
End;

Procedure TGLNGDManager.SetRenderPoint(Const value: TGLRenderPoint);
Begin
  If FRenderPoint <> value Then
  Begin
    If Assigned(FRenderPoint) Then
      FRenderPoint.UnRegisterCallBack(RenderEvent);
    FRenderPoint := value;
    If Assigned(FRenderPoint) Then
      FRenderPoint.RegisterCallBack(RenderEvent, RenderPointFreed);
    NotifyChange(self);
  End;
End;

Procedure TGLNGDManager.SetShowAABB(Const value: Boolean);
Begin
  If value <> FShowAABB Then
  Begin
    FShowAABB := value;
    NotifyChange(self);
  End;
End;

Procedure TGLNGDManager.SetShowContact(Const value: Boolean);
Begin
  If value <> FShowContact Then
  Begin
    FShowContact := value;
    NotifyChange(self);
  End;
End;

Procedure TGLNGDManager.SetShowGeometry(Const value: Boolean);
Begin
  If value <> FShowGeometry Then
  Begin
    FShowGeometry := value;
    NotifyChange(self);
  End;
End;

Procedure TGLNGDManager.SetShowJoint(Const value: Boolean);
Begin
  If value <> FShowJoint Then
  Begin
    FShowJoint := value;
    NotifyChange(self);
  End;
End;

Procedure TGLNGDManager.SetShowMaterialESP(Const value: Boolean);
Begin
  If value <> FShowMaterialESP Then
  Begin
    FShowMaterialESP := value;
    NotifyChange(self);
  End;
End;

Procedure TGLNGDManager.SetShowSpring(Const value: Boolean);
Begin
  If value <> FShowSpring Then
  Begin
    FShowSpring := value;
    NotifyChange(self);
  End;
End;

Procedure TGLNGDManager.SetSolverModel(val: TNGDSolverModels);
Begin
  If Assigned(FNewtonWorld) Then
    NewtonSetSolverModel(FNewtonWorld, Ord(val));
  FSolverModel := val;
End;

Procedure TGLNGDManager.SetThreadCount(val: integer);
Begin
  If val > 0 Then
  Begin
    If Assigned(FNewtonWorld) Then
      NewtonSetThreadsCount(FNewtonWorld, val);
    FThreadCount := val;
  End;
End;

Procedure TGLNGDManager.SetVisible(Const value: Boolean);
Begin
  If value <> FVisible Then
  Begin
    FVisible := value;
    NotifyChange(self);
  End;
End;

Procedure TGLNGDManager.SetVisibleAtRunTime(Const value: Boolean);
Begin
  If value <> FVisibleAtRunTime Then
  Begin
    FVisibleAtRunTime := value;
    NotifyChange(self);
  End;
End;

Procedure TGLNGDManager.SetWaterDensity(val: Single);
Begin
  If val < 0 Then
    FWaterDensity := 0
  Else
    FWaterDensity := val;
End;

Procedure TGLNGDManager.NotifyWaterPlaneChange(Sender: TObject);
Var
  W: Single;
Begin
  FWaterPlane.OnNotifyChange := Nil;
  W := FWaterPlane.W;
  FWaterPlane.Normalize;
  FWaterPlane.W := W;
  FWaterPlane.OnNotifyChange := NotifyWaterPlaneChange;
End;

Procedure TGLNGDManager.NotifyWorldSizeChange(Sender: TObject);
Begin
  If Assigned(FNewtonWorld) Then
    NewtonSetWorldSize(FNewtonWorld, @FWorldSizeMin.AsVector,
      @FWorldSizeMax.AsVector);
End;

Procedure TGLNGDManager.Step(deltatime: Single);
Begin
  If Assigned(FNewtonWorld) Then
    NewtonUpdate(FNewtonWorld, deltatime);
End;

{ TGLNGDBehaviour }

Constructor TGLNGDBehaviour.Create(AOwner: TXCollection);
Begin
  Inherited;
  FInitialized := False;
  FOwnerBaseSceneObject := OwnerBaseSceneObject;
  If Assigned(FOwnerBaseSceneObject) Then
    RegisterGLSceneObject(OwnerBaseSceneObject);
  FJointRegister := TList.Create;
  FContinuousCollisionMode := False;
  FMaterialID := 0;
  FBoundingSphereRadius := OwnerBaseSceneObject.BoundingSphereRadius;
  FNewtonBody := Nil;
  FCollision := Nil;
  FCollisionArray := Nil;
End;

Destructor TGLNGDBehaviour.Destroy;
Begin
  If Assigned(FManager) Then
    Manager := Nil;
  If Assigned(FOwnerBaseSceneObject) Then
    UnregisterGLSceneObject(FOwnerBaseSceneObject);
  FJointRegister.Free;
  Inherited;
End;

Procedure TGLNGDBehaviour.Finalize;
Var
  i: integer;
Begin
  FInitialized := False;

  For i := 0 To FJointRegister.Count - 1 Do
    TNGDJointBase(FJointRegister[i]).Finalize;

  If Assigned(FManager) Then
  Begin
    NewtonBodySetDestructorCallback(FNewtonBody, Nil);
    NewtonDestroyBody(FManager.FNewtonWorld, FNewtonBody);
    FNewtonBody := Nil;
    FCollision := Nil;
    FCollisionArray := Nil;
  End;
End;

Function TGLNGDBehaviour.GetCollisionFromBaseSceneObject
  (SceneObject: TGLBaseSceneObject): PNewtonCollision;
Var
  VertexList: Array Of TAffineVector; // For ConvexHull
  i, k: integer; // For FreeformMesh
  FCollisionOffsetMatrix: TMatrix;

  Function CorrectOffsetMatrix(SceneObject: TGLBaseSceneObject): TMatrix;
  Var
    OwnerRotation: TMatrix;
    OwnerAngles: TVector;
  Begin
    If OwnerBaseSceneObject <> SceneObject Then
    Begin
      FCollisionOffsetMatrix := MatrixMultiply(FCollisionOffsetMatrix,
        SceneObject.AbsoluteMatrix);

      FCollisionOffsetMatrix[3, 0] := FCollisionOffsetMatrix[3,
        0] - OwnerBaseSceneObject.Position.x;
      FCollisionOffsetMatrix[3, 1] := FCollisionOffsetMatrix[3,
        1] - OwnerBaseSceneObject.Position.y;
      FCollisionOffsetMatrix[3, 2] := FCollisionOffsetMatrix[3,
        2] - OwnerBaseSceneObject.Position.z;

      OwnerRotation := OwnerBaseSceneObject.AbsoluteMatrix;
      NewtonGetEulerAngle(@(OwnerRotation), @OwnerAngles);
      NewtonSetEulerAngle(@OwnerAngles, @(OwnerRotation));
      FCollisionOffsetMatrix := MatrixMultiply(FCollisionOffsetMatrix,
        MatrixInvert(OwnerRotation));
    End;
    Result := FCollisionOffsetMatrix;
  End;

Begin
  FCollisionOffsetMatrix := IdentityHmgMatrix;

  If (SceneObject Is TGLCube) Then
  Begin
    FCollisionOffsetMatrix := CorrectOffsetMatrix(SceneObject);
    With (SceneObject As TGLCube) Do
      Result := NewtonCreateBox(FManager.FNewtonWorld, CubeWidth, CubeHeight,
        CubeDepth, 0, @FCollisionOffsetMatrix);
  End

  Else If (SceneObject Is TGLSphere) Then
  Begin
    FCollisionOffsetMatrix := CorrectOffsetMatrix(SceneObject);
    With (SceneObject As TGLSphere) Do
      Result := NewtonCreateSphere(FManager.FNewtonWorld, Radius, Radius,
        Radius, 0, @FCollisionOffsetMatrix);
  End

  Else If (SceneObject Is TGLCone) Then
  Begin
    FCollisionOffsetMatrix := MatrixMultiply(FCollisionOffsetMatrix,
      CreateRotationMatrixZ(DegToRad(90)));
    FCollisionOffsetMatrix := CorrectOffsetMatrix(SceneObject);
    With (SceneObject As TGLCone) Do
      Result := NewtonCreateCone(FManager.FNewtonWorld, BottomRadius, Height,
        0, @FCollisionOffsetMatrix);
  End

  Else If (SceneObject Is TGLCapsule) Then
  Begin
    FCollisionOffsetMatrix := MatrixMultiply(FCollisionOffsetMatrix,
      CreateRotationMatrixY(DegToRad(90)));
    FCollisionOffsetMatrix := CorrectOffsetMatrix(SceneObject);
    With (SceneObject As TGLCapsule) Do
      // Use Cylinder shape for buoyancy
      Result := NewtonCreateCapsule(FManager.FNewtonWorld, Radius,
        Height + 2 * Radius, 0, @FCollisionOffsetMatrix);
  End

  Else If (SceneObject Is TGLCylinder) Then
  Begin
    FCollisionOffsetMatrix := MatrixMultiply(FCollisionOffsetMatrix,
      CreateRotationMatrixZ(DegToRad(90)));
    FCollisionOffsetMatrix := CorrectOffsetMatrix(SceneObject);
    With (SceneObject As TGLCylinder) Do
      Result := NewtonCreateCylinder(FManager.FNewtonWorld, BottomRadius,
        Height, 0, @FCollisionOffsetMatrix);
  End

  Else If (SceneObject Is TGLFreeForm) And
    ((SceneObject As TGLFreeForm).MeshObjects.Count > 0) Then
  Begin
    FCollisionOffsetMatrix := CorrectOffsetMatrix(SceneObject);
    SetLength(FCollisionArray, (SceneObject As TGLFreeForm).MeshObjects.Count);

    For k := 0 To Length(FCollisionArray) - 1 Do
    Begin
      With (SceneObject As TGLFreeForm).MeshObjects[k] Do
        If Vertices.Count > 2 Then
        Begin

          SetLength(VertexList, Vertices.Count);
          For i := 0 To Length(VertexList) - 1 Do
          Begin
            VertexList[i][0] := Vertices[i][0];
            VertexList[i][1] := Vertices[i][1];
            VertexList[i][2] := Vertices[i][2];
          End;

          FCollisionArray[k] := NewtonCreateConvexHull(FManager.FNewtonWorld,
            Length(VertexList), @VertexList[0], SizeOf(TAffineVector), 0, 0,
            @FCollisionOffsetMatrix);

          // If the mesh is a plane
          If FCollisionArray[k] = Nil Then
            FCollisionArray[k] := NewtonCreateNull(FManager.FNewtonWorld);

        End
        Else
          // If the mesh has less than three vertex
          FCollisionArray[k] := NewtonCreateNull(FManager.FNewtonWorld);

    End;
    Result := Nil;
    // If we have only one mesh, send the NewtonCollision
    If (SceneObject As TGLFreeForm).MeshObjects.Count = 1 Then
      Result := FCollisionArray[0];
  End

  Else
  Begin
    Result := NewtonCreateNull(FManager.FNewtonWorld);
  End;
End;

Function TGLNGDBehaviour.GetNewtonBodyMatrix: TMatrix;
Begin
  If Assigned(FManager) Then
    NewtonBodyGetmatrix(FNewtonBody, @FNewtonBodyMatrix);
  Result := FNewtonBodyMatrix;
End;

Procedure TGLNGDBehaviour.Initialize;
Var
  i: integer;
Begin
  FInitialized := True;

  For i := 0 To FJointRegister.Count - 1 Do
    TNGDJointBase(FJointRegister[i]).StructureChanged(self);

  If Assigned(FManager) Then
  Begin
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
  End;
End;

Procedure TGLNGDBehaviour.Loaded;
Var
  mng: TComponent;
Begin
  Inherited;
  If FManagerName <> '' Then
  Begin
    mng := FindManager(TGLNGDManager, FManagerName);
    If Assigned(mng) Then
      Manager := TGLNGDManager(mng);
    FManagerName := '';
  End;

  If Assigned(FManager) Then
  Begin
    SetContinuousCollisionMode(FContinuousCollisionMode);
    SetMaterialID(FMaterialID);
  End;
End;

Procedure TGLNGDBehaviour.NotifyChange(Sender: TObject);
Begin
  If Assigned(FManager) Then
    Manager.NotifyChange(self);
End;

Procedure TGLNGDBehaviour.Reinitialize;
Begin
  If Initialized Then
  Begin
    // Set Appropriate NewtonCollision
    SetCollision();
    // Set position and orientation
    SetNewtonBodyMatrix(OwnerBaseSceneObject.AbsoluteMatrix);
    // Save BoundingSphereRadius
    FBoundingSphereRadius := OwnerBaseSceneObject.BoundingSphereRadius;
  End;
  Loaded;
End;

Procedure TGLNGDBehaviour.Render(Var rci: TRenderContextInfo);
Var
  NGDJointBase: TNGDJointBase;
  i: integer;
  bar: TVector;
  f: Single;
  ESPText: String;
Begin
  If (csDesigning In Manager.ComponentState) Then
  Begin
    // Move/Rotate NewtonObject if matrix are not equal in design time.
    If Not MatrixEquals(NewtonBodyMatrix, OwnerBaseSceneObject.AbsoluteMatrix)
      Then
      SetNewtonBodyMatrix(OwnerBaseSceneObject.AbsoluteMatrix);

    // Rebuild Collision if Radius has been modified
    If FBoundingSphereRadius <> OwnerBaseSceneObject.BoundingSphereRadius Then
    Begin

      // Exit if freeform, no need rebuild in design time
      If OwnerBaseSceneObject Is TGLFreeForm Then
        exit;

      Reinitialize;

    End;
  End;

  If Manager.ShowGeometry Then
  Begin
    glLineWidth(1);
    glLineStipple(1, $FFFF);
    // full opengl lines
    glBegin(GL_LINES);
    NewtonBodyMatrix;
    NewtonCollisionForEachPolygonDo(FCollision, @(FNewtonBodyMatrix),
      @NewtonCollisionIterator, Nil);
    glEnd;
  End;

  If Manager.ShowJoint Then
  Begin
    glLineWidth(5);
    glLineStipple(1, $00FF);
    // dot style opengl lines
    glBegin(GL_LINES);
    glColor4fv(FManager.JointColor.AsAddress); // Blue
    For i := 0 To FJointRegister.Count - 1 Do
    Begin
      NGDJointBase := FJointRegister.Items[i];
      NGDJointBase.Render;
    End;
    glEnd;
  End;

  If Manager.ShowMaterialESP And Assigned(FManager.BitmapFont) Then
  Begin
    // Render a string in 3d world at body position
    bar := OwnerBaseSceneObject.BarycenterAbsolutePosition;

    glPushMatrix;

    f := rci.renderDPI / 96;
    glLoadMatrixf(@TGLSceneBuffer(rci.buffer).BaseProjectionMatrix);
    bar := (rci.buffer As TGLSceneBuffer).WorldToScreen(bar);
    glScalef(2 / rci.viewPortSize.cx, 2 / rci.viewPortSize.cy, 1);
    glTranslatef(bar[0] * f - rci.viewPortSize.cx / 2,
      +bar[1] * f - rci.viewPortSize.cy / 2, -bar[2]);

    glMatrixMode(GL_PROJECTION);
    glPushMatrix;
    glLoadIdentity;

    ESPText := 'ID:=' + IntToStr(FMaterialID);

    rci.GLStates.Disable(stDepthTest);
    If bar[2] < 1 Then
      Manager.BitmapFont.RenderString(rci, ESPText, taCenter, tlCenter,
        Manager.MaterialESPColor.Color);
    rci.GLStates.Enable(stDepthTest);

    glPopMatrix;
    glMatrixMode(GL_MODELVIEW);
    glPopMatrix;
  End;
End;

// In this procedure, we assign collision to body
// [Because when initialised, the collision for body is type NULL]
Procedure TGLNGDBehaviour.SetCollision;
Var
  CollisionInfoRecord: TNewtonCollisionInfoRecord;
  k: integer;
Begin
  If Assigned(FCollision) Then
  Begin
    NewtonBodySetCollision(FNewtonBody, FCollision);

    // The API Ask for releasing Collision to avoid memory leak
    NewtonCollisionGetInfo(FCollision, @CollisionInfoRecord);
    If CollisionInfoRecord.m_referenceCount > 2 Then
      NewtonReleaseCollision(FManager.FNewtonWorld, FCollision);

    // Release each collision form the array
    For k := 0 To Length(FCollisionArray) - 1 Do
    Begin
      If FCollisionArray[k] <> Nil Then
      Begin
        NewtonCollisionGetInfo(FCollisionArray[k], @CollisionInfoRecord);
        If CollisionInfoRecord.m_referenceCount > 2 Then
          NewtonReleaseCollision(FManager.FNewtonWorld, FCollisionArray[k]);
      End;
    End;

  End;
End;

Procedure TGLNGDBehaviour.SetContinuousCollisionMode(val: Boolean);
Begin
  If Assigned(FManager) Then
    NewtonBodySetContinuousCollisionMode(FNewtonBody, Ord(val));
  FContinuousCollisionMode := val;
End;

Procedure TGLNGDBehaviour.SetManager(value: TGLNGDManager);
Begin
  If FManager <> value Then
  Begin
    If Assigned(FManager) Then
    Begin
      If Initialized Then
        Finalize;
      FManager.UnregisterNGDBehaviour(self);
    End;
    FManager := value;
    If Assigned(FManager) Then
    Begin
      // Commented to allow debug in design time
      // if not(csDesigning in TComponent(Owner.Owner).ComponentState) then
      Initialize;
      FManager.RegisterNGDBehaviour(self);
    End;
  End;
End;

Procedure TGLNGDBehaviour.SetMaterialID(val: integer);
Begin
  FMaterialID := val;
  If Assigned(FManager) Then
  Begin
    NewtonBodySetMaterialGroupID(FNewtonBody, FMaterialID);
    FManager.NotifyChange(self);
  End;

End;

Procedure TGLNGDBehaviour.SetNewtonBodyMatrix(val: TMatrix);
Begin
  If Assigned(FManager) Then
    NewtonBodySetmatrix(FNewtonBody, @val);
  FNewtonBodyMatrix := val;
End;

Procedure TGLNGDBehaviour.RegisterJoint(Joint: TNGDJointBase);
Begin
  If FJointRegister.IndexOf(Joint) = -1 Then
    FJointRegister.Add(Joint);
End;

Class Function TGLNGDBehaviour.UniqueItem: Boolean;
Begin
  Result := True;
End;

Procedure TGLNGDBehaviour.UnregisterJoint(Joint: TNGDJointBase);
Begin
  If FJointRegister.IndexOf(Joint) > -1 Then
    FJointRegister.Remove(Joint);
End;

Procedure TGLNGDBehaviour.ReadFromFiler(reader: TReader);
Begin
  Inherited;
  With reader Do
  Begin
    Assert(ReadInteger = 0);
    // Archive version
    FManagerName := ReadString;
    FContinuousCollisionMode := ReadBoolean;
    FMaterialID := ReadInteger;
  End;
End;

Procedure TGLNGDBehaviour.WriteToFiler(writer: TWriter);
Begin
  Inherited;
  With writer Do
  Begin
    WriteInteger(0);
    // Archive version
    If Assigned(FManager) Then
      WriteString(FManager.GetNamePath)
    Else
      WriteString('');
    WriteBoolean(FContinuousCollisionMode);
    WriteInteger(FMaterialID);
  End;
End;

{ TGLNGDDynamic }

Constructor TGLNGDDynamic.Create(AOwner: TXCollection);
Begin
  Inherited;
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
  FUseVelovity := False;
  FUseOmega := False;

  FNewtonUserJoint := Nil;
  FNewtonJoint := Nil;
End;

Destructor TGLNGDDynamic.Destroy;
Begin
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
  FNewtonUserJoint := Nil;
  FNewtonJoint := Nil;
  Inherited;
End;

Procedure TGLNGDDynamic.Finalize;
Begin
  If Assigned(FManager) Then
  Begin
    // Removing Callback
    NewtonBodySetForceAndTorqueCallback(FNewtonBody, Nil);
    NewtonBodySetTransformCallback(FNewtonBody, Nil);
  End;
  Inherited;
End;

Class Function TGLNGDDynamic.FriendlyName: String;
Begin
  Result := 'NGD Dynamic';
End;

Procedure TGLNGDDynamic.Initialize;
Begin
  Inherited;
  If Assigned(FManager) Then
  Begin
    // Set Density, Mass and inertie matrix
    SetDensity(FDensity);

    // Set Callback
    NewtonBodySetForceAndTorqueCallback(FNewtonBody,
      @NewtonApplyForceAndTorque);
    NewtonBodySetTransformCallback(FNewtonBody, @NewtonSetTransform);
  End;
End;

Procedure TGLNGDDynamic.Render(Var rci: TRenderContextInfo);
Var
  Barycenter, pickpoint: TVector;
  PickedMatrix: TMatrix;

  Procedure DrawAABB(min, max: TGLCoordinates3);
  Begin

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
    glVertex3f(min.x, min.y, min.z); // E
    glVertex3f(max.x, min.y, min.z); // F

    glVertex3f(max.x, min.y, min.z); // F
    glVertex3f(max.x, max.y, min.z); // G

    glVertex3f(max.x, max.y, min.z); // G
    glVertex3f(min.x, max.y, min.z); // H

    glVertex3f(min.x, max.y, min.z); // H
    glVertex3f(min.x, min.y, min.z); // E

    // Front
    glVertex3f(min.x, min.y, max.z); // A
    glVertex3f(max.x, min.y, max.z); // B

    glVertex3f(max.x, min.y, max.z); // B
    glVertex3f(max.x, max.y, max.z); // C

    glVertex3f(max.x, max.y, max.z); // C
    glVertex3f(min.x, max.y, max.z); // D

    glVertex3f(min.x, max.y, max.z); // D
    glVertex3f(min.x, min.y, max.z); // A

    // Edges
    glVertex3f(min.x, min.y, max.z); // A
    glVertex3f(min.x, min.y, min.z); // E

    glVertex3f(max.x, min.y, max.z); // B
    glVertex3f(max.x, min.y, min.z); // F

    glVertex3f(max.x, max.y, max.z); // C
    glVertex3f(max.x, max.y, min.z); // G

    glVertex3f(min.x, max.y, max.z); // D
    glVertex3f(min.x, max.y, min.z); // H
  End;

Begin
  Inherited;

  // Exit if freeform, no need debug in design time
  If (csDesigning In Manager.ComponentState) And
    (OwnerBaseSceneObject Is TGLFreeForm) Then
    exit;

  If Manager.ShowAABB Then
  Begin
    glLineWidth(1);
    glLineStipple(1, $FFFF);
    // full opengl lines
    glBegin(GL_LINES);

    If (FDensity = 0) Or (NewtonBodyGetSleepState(FNewtonBody) = 1) Then
      glColor4fv(FManager.AABBColorSleep.AsAddress)
    Else
      glColor4fv(FManager.AABBColor.AsAddress);
    // Get AABB
    NewtonBodyGetAABB(FNewtonBody, @(FAABBmin.AsVector), @(FAABBmax.AsVector));
    DrawAABB(FAABBmin, FAABBmax);
    glEnd;
  End;

  If Manager.ShowSpring Then
  Begin
    If Assigned(FNewtonUserJoint) Then
    Begin
      // DrawLine
      glLineWidth(5);
      glLineStipple(1, $FF00); // dot style opengl lines
      glBegin(GL_LINES);
      glColor4fv(FManager.SpringColor.AsAddress); // Aqua
      Barycenter := OwnerBaseSceneObject.BarycenterAbsolutePosition;
      CustomKinematicControllerGetTargetMatrix(FNewtonUserJoint, @PickedMatrix);
      pickpoint[0] := PickedMatrix[3, 0];
      pickpoint[1] := PickedMatrix[3, 1];
      pickpoint[2] := PickedMatrix[3, 2];
      glVertex3fv(@pickpoint);
      glVertex3fv(@Barycenter);
      glEnd;

      // DrawPoint
      glPointSize(10);
      glEnable(GL_POINT_SMOOTH);
      glBegin(GL_POINTS);
      glVertex3fv(@pickpoint);
      glVertex3fv(@Barycenter);
      glEnd;
      glDisable(GL_POINT_SMOOTH);
    End;
  End;
End;

Procedure TGLNGDDynamic.SetAutoSleep(val: Boolean);
Begin
  If Assigned(FManager) Then
    NewtonBodySetAutoSleep(FNewtonBody, Ord(val));
  FAutoSleep := val;
End;

Procedure TGLNGDDynamic.SetCollision;
Begin
  // Return nullcollision if unknow, return nil if freeform
  FCollision := GetCollisionFromBaseSceneObject(OwnerBaseSceneObject);

  // Create compound if freeform
  If Not Assigned(FCollision) Then
    FCollision := NewtonCreateCompoundCollision(FManager.FNewtonWorld,
      Length(FCollisionArray), @FCollisionArray[0], 0);
  Inherited;
End;

Procedure TGLNGDDynamic.SetDensity(val: Single);
Var
  inertia: TVector;
  origin: TVector;
Begin
  If val >= 0 Then
  Begin
    FDensity := val;

    FVolume := NewtonConvexCollisionCalculateVolume(FCollision);
    NewtonConvexCollisionCalculateInertialMatrix(FCollision, @inertia, @origin);

    FMass := FVolume * FDensity;

    NewtonBodySetMassMatrix(FNewtonBody, FMass, FMass * inertia[0],
      FMass * inertia[1], FMass * inertia[2]);

    CenterOfMass.AsVector := origin;
  End;
End;

Procedure TGLNGDDynamic.SetLinearDamping(val: Single);
Begin
  If (val >= 0) AND (val <= 1) Then
    FLinearDamping := val;
  If Assigned(FManager) Then
    NewtonBodySetLinearDamping(FNewtonBody, FLinearDamping);
End;

Procedure TGLNGDDynamic.SetUpVector(val: Boolean);
Begin
  FUpVector := val;
  If Assigned(FManager) Then
  Begin
    If FNewtonJoint <> Nil Then
    Begin
      NewtonDestroyJoint(FManager.FNewtonWorld, FNewtonJoint);
      FNewtonJoint := Nil
    End;

    If FUpVector Then
      FNewtonJoint := NewtonConstraintCreateUpVector(FManager.FNewtonWorld,
        @FUpVectorDirection.AsVector, FNewtonBody)
  End;
End;

// WriteToFiler
//
Procedure TGLNGDDynamic.WriteToFiler(writer: TWriter);
Begin
  Inherited;
  With writer Do
  Begin
    WriteInteger(0);
    // Archive version
    WriteBoolean(FAutoSleep);
    WriteSingle(FLinearDamping);
    WriteSingle(FDensity);
    WriteBoolean(FUpVector);
    WriteBoolean(FUseVelovity);
    WriteBoolean(FUseOmega);
  End;
  FForce.WriteToFiler(writer);
  FTorque.WriteToFiler(writer);
  FVelocity.WriteToFiler(writer);
  FOmega.WriteToFiler(writer);
  FCenterOfMass.WriteToFiler(writer);
  FAngularDamping.WriteToFiler(writer);
  FUpVectorDirection.WriteToFiler(writer);
End;

// ReadFromFiler
//
Procedure TGLNGDDynamic.ReadFromFiler(reader: TReader);
Begin
  Inherited;
  With reader Do
  Begin
    Assert(ReadInteger = 0); // Archive version
    FAutoSleep := ReadBoolean;
    FLinearDamping := ReadSingle;
    FDensity := ReadSingle;
    FUpVector := ReadBoolean;
    FUseVelovity := ReadBoolean;
    FUseOmega := ReadBoolean;
  End;
  FForce.ReadFromFiler(reader);
  FTorque.ReadFromFiler(reader);
  FVelocity.ReadFromFiler(reader);
  FOmega.ReadFromFiler(reader);
  FCenterOfMass.ReadFromFiler(reader);
  FAngularDamping.ReadFromFiler(reader);
  FUpVectorDirection.ReadFromFiler(reader);
End;

Procedure TGLNGDDynamic.Loaded;
Begin
  Inherited;
  If Assigned(FManager) Then
  Begin
    SetAutoSleep(FAutoSleep);
    SetLinearDamping(FLinearDamping);
    SetDensity(FDensity);
    NotifyCenterOfMassChange(self);
    NotifyAngularDampingChange(self);
    SetUpVector(FUpVector);
    NotifyUpVectorDirectionChange(self);
  End;
End;

Procedure TGLNGDDynamic.NotifyAngularDampingChange(Sender: TObject);
Begin
  FAngularDamping.OnNotifyChange := Nil;
  If (FAngularDamping.x >= 0) AND (FAngularDamping.x <= 1) AND
    (FAngularDamping.y >= 0) AND (FAngularDamping.y <= 1) AND
    (FAngularDamping.z >= 0) AND (FAngularDamping.z <= 1) Then
    If Assigned(FManager) Then
      NewtonBodySetAngularDamping(FNewtonBody, @(FAngularDamping.AsVector));
  FAngularDamping.OnNotifyChange := NotifyAngularDampingChange;
End;

Procedure TGLNGDDynamic.NotifyCenterOfMassChange(Sender: TObject);
Begin
  FCenterOfMass.OnNotifyChange := Nil;
  If Assigned(FManager) Then
    NewtonBodySetCentreOfMass(FNewtonBody, @(FCenterOfMass.AsVector));
  FCenterOfMass.OnNotifyChange := NotifyCenterOfMassChange;
End;

Procedure TGLNGDDynamic.NotifyUpVectorDirectionChange(Sender: TObject);
Begin
  FUpVectorDirection.OnNotifyChange := Nil;
  FUpVectorDirection.Normalize;
  FUpVectorDirection.OnNotifyChange := NotifyUpVectorDirectionChange;

  If Assigned(FManager) Then
    If FNewtonJoint <> Nil Then
      NewtonUpVectorSetPin(FNewtonJoint, @FUpVectorDirection.AsVector);
End;

Procedure TGLNGDDynamic.Pick(pickpoint: TVector; Mode: TNGDPickedModes);
Begin
  // Create the joint
  If Mode = pmAttach Then
  Begin
    If Assigned(FNewtonBody) Then
      FNewtonUserJoint := CreateCustomKinematicController(FNewtonBody,
        @pickpoint);
    If Assigned(FNewtonUserJoint) Then
    Begin
      CustomKinematicControllerSetPickMode(FNewtonUserJoint, 0);
      CustomKinematicControllerSetMaxLinearFriction(FNewtonUserJoint, 750);
      CustomKinematicControllerSetMaxAngularFriction(FNewtonUserJoint,
        750 / FMass);
      CustomKinematicControllerSetTargetPosit(FNewtonUserJoint, @pickpoint);
    End;
  End;

  // Change the TargetPoint
  If Mode = pmMove Then
  Begin
    If Assigned(FNewtonUserJoint) Then
    Begin
      CustomKinematicControllerSetTargetPosit(FNewtonUserJoint, @pickpoint);
    End;
  End;

  // Delete the joint
  If Mode = pmDetach Then
  Begin
    If Assigned(FNewtonUserJoint) Then
    Begin
      CustomDestroyJoint(FNewtonUserJoint);
      FNewtonUserJoint := Nil;
    End;
  End;
End;

{ TGLNGDStatic }

Procedure TGLNGDStatic.Render(Var rci: TRenderContextInfo);
Begin
  Inherited;
  // Move/Rotate NewtonObject if matrix are not equal in design time.
  If Not MatrixEquals(NewtonBodyMatrix, OwnerBaseSceneObject.AbsoluteMatrix)
    Then
    SetNewtonBodyMatrix(OwnerBaseSceneObject.AbsoluteMatrix);

  // Rebuild Collision if Radius has been modified
  If FBoundingSphereRadius <> OwnerBaseSceneObject.BoundingSphereRadius Then
    { Wont be called if visible at runtime is inactive }
    Reinitialize;

End;

Procedure TGLNGDStatic.SetCollision;
Begin
  // Return nullcollision if unknow, return nil if freeform
  FCollision := GetCollisionFromBaseSceneObject(OwnerBaseSceneObject);

  // Create tree if freeform
  If Not Assigned(FCollision) Then
    With OwnerBaseSceneObject Do
    Begin
      { If ((x - y) <= 0.01) AND ((x - z) <= 0.01) Then
        FCollision := GetTree(False, x) // Scaled TreeCollision
        Else }
      FCollision := GetTree(True, scale.x);
    End;

  Inherited;
  {
    // FCollision := NewtonCreateSceneCollision(Manager.NewtonWorld, 0);
    // for k := 0 to Length(FCollisionArray) - 1 do
    // NewtonSceneCollisionCreateProxy(FCollision, FCollisionArray[k]);
    // NewtonSceneCollisionCreateProxy(FCollision,
    // NewtonCreateSphere(Manager.NewtonWorld,1,2,3,0,nil));
    // NewtonSceneCollisionOptimize(FCollision); }
End;

// To create heightField
Procedure TGLNGDStatic.SetHeightField(heightArray: Array Of UInt16; x: integer;
  y: integer; xScale: Single; yScale: Single);
{Var
  attributeMap: Array Of UInt8;
  i: integer; }
Begin

  {SetLength(FHeightFieldWordArray, Length(heightArray));
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
  NewtonReleaseCollision(FManager.FNewtonWorld, FCollision);}
End;

Function TGLNGDStatic.GetTree(optimize: Boolean;
  scaleXYZ: Single): PNewtonCollision;
Var
  MeshIndex, TriangleIndex: integer;
  TriangleList: TAffineVectorList;
  v: Array [0 .. 2] Of TAffineVector;
  NewtonCollision: PNewtonCollision;
Begin
  If OwnerBaseSceneObject Is TGLFreeForm Then
  Begin
    With (OwnerBaseSceneObject As TGLFreeForm) Do
    Begin
      NewtonCollision := NewtonCreateTreeCollision(FManager.FNewtonWorld, 0);
      NewtonTreeCollisionBeginBuild(NewtonCollision);

      For MeshIndex := 0 To MeshObjects.Count - 1 Do
      Begin
        TriangleList := MeshObjects[MeshIndex].ExtractTriangles;
        For TriangleIndex := 0 To TriangleList.Count - 1 Do
        Begin
          If TriangleIndex Mod 3 = 0 Then
          Begin
            v[0] := TriangleList.Items[TriangleIndex];
            ScaleVector(v[0], scaleXYZ);
            v[1] := TriangleList.Items[TriangleIndex + 1];
            ScaleVector(v[1], scaleXYZ);
            v[2] := TriangleList.Items[TriangleIndex + 2];
            ScaleVector(v[2], scaleXYZ);
            NewtonTreeCollisionAddFace(NewtonCollision, 3, @(v),
              SizeOf(TAffineVector), 1);
          End;
        End;
        TriangleList.Free;
      End;
      NewtonTreeCollisionEndBuild(NewtonCollision, Ord(optimize));
      Result := NewtonCollision;
    End;
  End
  Else
    Result := NewtonCreateNull(FManager.FNewtonWorld);
End;

Class Function TGLNGDStatic.FriendlyName: String;
Begin
  Result := 'NGD Static';
End;

{ TNGDMaterialPair }

Constructor TNGDMaterialPair.Create(AOwner: TXCollection);
Begin
  Inherited;
  FSoftness := 0.1;
  FElasticity := 0.4;
  FCollidable := True;
  FStaticFriction := 0.9;
  FKineticFriction := 0.5;
  Fid0 := 0;
  Fid1 := 0;
  FContactProcessEvent := Nil;
End;

Destructor TNGDMaterialPair.Destroy;
Begin
  Inherited;
End;

Procedure TNGDMaterialPair.Finalize;
Begin
  FInitialized := False;
  If Assigned(FManager) Then
    NewtonMaterialSetCollisionCallback(FManager.FNewtonWorld, Fid0, Fid1, Nil,
      Nil, Nil);
End;

Class Function TNGDMaterialPair.FriendlyDescription: String;
Begin
  Result := 'MaterialPair';
End;

Class Function TNGDMaterialPair.FriendlyName: String;
Begin
  Result := 'NGD MaterialPair implementation';
End;

Procedure TNGDMaterialPair.Initialize;
Begin
  FInitialized := True;
  If Assigned(FManager) Then
    NewtonMaterialSetCollisionCallback(FManager.FNewtonWorld, Fid0, Fid1, self,
      @NewtonOnAABBOverlap, @NewtonContactsProcess);
End;

Procedure TNGDMaterialPair.Loaded;
Var
  mng: TComponent;
Begin
  Inherited;
  If FManagerName <> '' Then
  Begin
    mng := FindManager(TGLNGDManager, FManagerName);
    If Assigned(mng) Then
      Manager := TGLNGDManager(mng);
    FManagerName := '';
  End;
  SetSoftness(FSoftness);
  SetCollidable(FCollidable);
  SetElasticity(FElasticity);
  SetStaticFriction(FStaticFriction);
  SetKineticFriction(FKineticFriction);
End;

Procedure TNGDMaterialPair.ReadFromFiler(reader: TReader);
{ var
  ContactProcessEventOwner, ContactProcessEventName: string; }
Begin
  Inherited;
  With reader Do
  Begin
    Assert(ReadInteger = 0);
    // Archive version
    FManagerName := ReadString;
    FSoftness := ReadSingle;
    FElasticity := ReadSingle;
    FCollidable := ReadBoolean;
    FStaticFriction := ReadSingle;
    FKineticFriction := ReadSingle;
    Fid0 := ReadInteger;
    Fid1 := ReadInteger;
    // ContactProcessEventOwner := ReadString;
    // ContactProcessEventName := ReadString;
  End;
  // FContactProcessEvent := TContactProcessEvent
  // (GetMethodFromString(ContactProcessEventOwner, ContactProcessEventName));
End;

Procedure TNGDMaterialPair.WriteToFiler(writer: TWriter);
{ var
  ContactProcessEventOwner, ContactProcessEventName: string; }
Begin
  Inherited;
  With writer Do
  Begin
    WriteInteger(0);
    // Archive version
    If Assigned(FManager) Then
      WriteString(FManager.GetNamePath)
    Else
      WriteString('');
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
  End;
End;

Procedure TNGDMaterialPair.SetCollidable(val: Boolean);
Begin
  FCollidable := val;
  If Initialized Then
    NewtonMaterialSetDefaultCollidable(FManager.FNewtonWorld, Fid0, Fid1,
      Ord(FCollidable));
End;

Procedure TNGDMaterialPair.SetElasticity(val: Single);
Begin
  If (val >= 0) Then
    FElasticity := val;
  If Initialized Then
    NewtonMaterialSetDefaultElasticity(FManager.FNewtonWorld, Fid0, Fid1,
      FElasticity);
End;

Procedure TNGDMaterialPair.SetSoftness(val: Single);
Begin
  If (val >= 0) AND (val <= 1) Then
    FSoftness := val;
  If Initialized Then
    NewtonMaterialSetDefaultSoftness(FManager.FNewtonWorld, Fid0, Fid1,
      FSoftness);
End;

Procedure TNGDMaterialPair.SetStaticFriction(val: Single);
Begin
  If (val <= 1) And (val >= FKineticFriction) Then
    FStaticFriction := val;
  If Initialized Then
    NewtonMaterialSetDefaultFriction(FManager.FNewtonWorld, Fid0, Fid1,
      FStaticFriction, FKineticFriction);
End;

Procedure TNGDMaterialPair.SetKineticFriction(val: Single);
Begin
  If (val <= FStaticFriction) And (val >= 0) Then
    FKineticFriction := val;
  If Initialized Then
    NewtonMaterialSetDefaultFriction(FManager.FNewtonWorld, Fid0, Fid1,
      FStaticFriction, FKineticFriction);
End;

Procedure TNGDMaterialPair.SetManager(Const value: TGLNGDManager);
Begin
  If FManager <> value Then
  Begin
    If Assigned(FManager) Then
      If Not(csDesigning In Manager.ComponentState) Then
        Finalize;
    FManager := value;
    If Assigned(FManager) Then
      If Not(csDesigning In Manager.ComponentState) Then
        Initialize;
  End;
End;

{ TNGDMaterials }

Procedure TNGDMaterials.Finalize;
Var
  i: integer;
Begin
  For i := 0 To Count - 1 Do
    MaterialPair[i].Finalize;
End;

Function TNGDMaterials.GetMaterialPair(Index: integer): TNGDMaterialPair;
Begin
  Result := TNGDMaterialPair(Items[Index]);
End;

Procedure TNGDMaterials.Initialize;
Var
  i: integer;
Begin
  For i := 0 To Count - 1 Do
    MaterialPair[i].Initialize;
End;

Class Function TNGDMaterials.ItemsClass: TXCollectionItemClass;
Begin
  Result := TNGDMaterialPair;
End;

{ TNGDJoints }

Procedure TNGDJoints.Finalize;
Var
  i: integer;
Begin
  For i := 0 To Count - 1 Do
    Joint[i].Finalize;
End;

Function TNGDJoints.GetJoint(Index: integer): TNGDJointBase;
Begin
  Result := TNGDJointBase(Items[Index]);
End;

Procedure TNGDJoints.Initialize;
Var
  i: integer;
Begin
  For i := 0 To Count - 1 Do
    Joint[i].Initialize;
End;

Class Function TNGDJoints.ItemsClass: TXCollectionItemClass;
Begin
  Result := TNGDJointBase;
End;

{ TGLNGDJointList }

Constructor TGLNGDJointList.Create(AOwner: TComponent);
Begin
  Inherited;
  FJoints := TNGDJoints.Create(self);
End;

Procedure TGLNGDJointList.DefineProperties(Filer: TFiler);
Begin
  Inherited;
  Filer.DefineBinaryProperty('NGDJointsData', ReadJoints, WriteJoints,
    (Assigned(FJoints) And (FJoints.Count > 0)));
End;

Destructor TGLNGDJointList.Destroy;
Begin
  FJoints.Free;
  Inherited;
End;

Procedure TGLNGDJointList.Loaded;
Var
  i: integer;
Begin
  Inherited;
  For i := 0 To FJoints.Count - 1 Do
    FJoints[i].Loaded;
End;

Procedure TGLNGDJointList.Notification(AComponent: TComponent;
  Operation: TOperation);
Var
  i: integer;
Begin
  Inherited;
  If (Operation = opRemove) And (AComponent Is TGLBaseSceneObject) Then
    For i := 0 To Joints.Count - 1 Do
    Begin
      If TGLBaseSceneObject(AComponent) = Joints[i].Object1 Then
        Joints[i].Object1 := Nil;
      If TGLBaseSceneObject(AComponent) = Joints[i].Object2 Then
        Joints[i].Object2 := Nil;
    End;
End;

Procedure TGLNGDJointList.ReadJoints(stream: TStream);
Var
  reader: TReader;
Begin
  reader := TReader.Create(stream, 16384);
  Try
    Joints.ReadFromFiler(reader);
  Finally
    reader.Free;
  End;
End;

Procedure TGLNGDJointList.WriteJoints(stream: TStream);
Var
  writer: TWriter;
Begin
  writer := TWriter.Create(stream, 16384);
  Try
    Joints.WriteToFiler(writer);
  Finally
    writer.Free;
  End;
End;

{ TNGDJointBase }

Constructor TNGDJointBase.Create(AOwner: TXCollection);
Begin
  Inherited;
  FInitialized := False;
End;

Destructor TNGDJointBase.Destroy;
Begin
  Finalize;
  Inherited;
End;

Procedure TNGDJointBase.Finalize;
Begin
  If Not Initialized Then
    exit;

  If Assigned(FObject1) Then
    UnregisterJointWithObject(FObject1);
  If Assigned(FObject2) Then
    UnregisterJointWithObject(FObject2);

  FInitialized := False;
End;

Procedure TNGDJointBase.Initialize;
Begin
  If Not Assigned(FManager) Then
    exit;
  If Assigned(FObject1) Then
    RegisterJointWithObject(FObject1);
  If Assigned(FObject2) Then
    RegisterJointWithObject(FObject2);
  FInitialized := True;
End;

Procedure TNGDJointBase.Loaded;
Var
  mng: TComponent;
  Obj: TGLBaseSceneObject;
Begin
  Inherited;
  If FManagerName <> '' Then
  Begin
    mng := FindManager(TGLNGDManager, FManagerName);
    If Assigned(mng) Then
      Manager := TGLNGDManager(mng);
    FManagerName := '';
  End;
  If FObject1Name <> '' Then
  Begin
    Obj := GetGLSceneObject(FObject1Name);
    If Assigned(Obj) Then
      Object1 := Obj;
    FObject1Name := '';
  End;
  If FObject2Name <> '' Then
  Begin
    Obj := GetGLSceneObject(FObject2Name);
    If Assigned(Obj) Then
      Object2 := Obj;
    FObject2Name := '';
  End;
  StructureChanged(self);
End;

Procedure TNGDJointBase.ReadFromFiler(reader: TReader);
Begin
  Inherited;
  With reader Do
  Begin
    Assert(ReadInteger = 0);
    // Archive version
    FManagerName := ReadString;
    FObject1Name := ReadString;
    FObject2Name := ReadString;
  End;
End;

Procedure TNGDJointBase.WriteToFiler(writer: TWriter);
Begin
  Inherited;
  With writer Do
  Begin
    WriteInteger(0);
    // Archive version
    If Assigned(FManager) Then
      WriteString(FManager.GetNamePath)
    Else
      WriteString('');
    If Assigned(FObject1) Then
      WriteString(FObject1.GetNamePath)
    Else
      WriteString('');
    If Assigned(FObject2) Then
      WriteString(FObject2.GetNamePath)
    Else
      WriteString('');
  End;
End;

Procedure TNGDJointBase.RegisterJointWithObject(Obj: TGLBaseSceneObject);
Var
  temp: TGLNGDBehaviour;
Begin
  If Assigned(Obj) Then
  Begin
    temp := TGLNGDBehaviour(Obj.Behaviours.GetByClass(TGLNGDBehaviour));
    If Assigned(temp) Then
      temp.RegisterJoint(self);
  End;
End;

Procedure TNGDJointBase.Render;
Var
  bar1, bar2: TVector;
Begin
  If Assigned(Object1) AND Assigned(Object2) Then
  Begin
    bar1 := Object1.BarycenterAbsolutePosition;
    bar2 := Object2.BarycenterAbsolutePosition;
    glVertex3fv(@bar1);
    glVertex3fv(@bar2);
  End;
End;

Procedure TNGDJointBase.UnregisterJointWithObject(Obj: TGLBaseSceneObject);
Var
  temp: TGLNGDBehaviour;
Begin
  If Assigned(Obj) Then
  Begin
    temp := TGLNGDBehaviour(Obj.Behaviours.GetByClass(TGLNGDBehaviour));
    If Assigned(temp) Then
      temp.UnregisterJoint(self);
  End;
End;

Procedure TNGDJointBase.SetManager(Const value: TGLNGDManager);
Begin
  If FManager <> value Then
  Begin
    If Assigned(FManager) Then
      If Initialized Then // if not(csDesigning in FManager.ComponentState) then
        Finalize;
    FManager := value;
    If Assigned(FManager) Then
      // if not(csDesigning in FManager.ComponentState) then
      Initialize;
  End;
End;

Procedure TNGDJointBase.SetObject1(Const value: TGLBaseSceneObject);
Begin
  If FObject1 <> value Then
  Begin
    If Assigned(FObject1) Then
      UnregisterJointWithObject(FObject1);
    FObject1 := value;
    If Assigned(FObject1) Then
      RegisterJointWithObject(FObject1)
    Else
      FObject1 := Nil;
  End;
End;

Procedure TNGDJointBase.SetObject2(Const value: TGLBaseSceneObject);
Begin
  If FObject2 <> value Then
  Begin
    If Assigned(FObject2) Then
      UnregisterJointWithObject(FObject2);
    FObject2 := value;
    If Assigned(FObject2) Then
      RegisterJointWithObject(FObject2)
    Else
      FObject2 := Nil;
  End;
End;

Procedure TNGDJointBase.StructureChanged(Sender: TObject);
Begin
  If Assigned(FManager) Then
    FManager.NotifyChange(self);
End;

{ TNGDJointBall }

Constructor TNGDJointBall.Create(AOwner: TXCollection);
Begin
  Inherited;

  FNewtonJoint := Nil;
  FStiffness := 0.9;
  FCollisionState := False;

  FPivotPoint := TGLCoordinates.CreateInitialized(self, NullHMGPoint, csPoint);
  FPivotPoint.OnNotifyChange := StructureChanged;
End;

Destructor TNGDJointBall.Destroy;
Begin
  If FNewtonJoint <> Nil Then
    If Assigned(FManager) Then
      NewtonDestroyJoint(FManager.FNewtonWorld, FNewtonJoint);
  FNewtonJoint := Nil;
  FPivotPoint.Free;
  Inherited;
End;

Class Function TNGDJointBall.FriendlyDescription: String;
Begin
  Result := 'NGD Ball joint implementation';
End;

Class Function TNGDJointBall.FriendlyName: String;
Begin
  Result := 'Ball';
End;

Procedure TNGDJointBall.ReadFromFiler(reader: TReader);
Begin
  Inherited;
  With reader Do
  Begin
    Assert(ReadInteger = 0);
    // Archive version
    FStiffness := ReadSingle;
    FCollisionState := ReadBoolean;
  End;
  FPivotPoint.ReadFromFiler(reader);
End;

Procedure TNGDJointBall.WriteToFiler(writer: TWriter);
Begin
  Inherited;
  With writer Do
  Begin
    WriteInteger(0);
    // Archive version
    WriteSingle(FStiffness);
    WriteBoolean(FCollisionState);
  End;
  FPivotPoint.WriteToFiler(writer);
End;

Procedure TNGDJointBall.Render;
Var
  bar1, bar2: TVector;
Begin
  If Assigned(Object1) AND Assigned(Object2) Then
  Begin
    bar1 := Object1.BarycenterAbsolutePosition;
    bar2 := Object2.BarycenterAbsolutePosition;
    glVertex3fv(@bar1);
    glVertex3fv(FPivotPoint.AsAddress);
    glVertex3fv(FPivotPoint.AsAddress);
    glVertex3fv(@bar2);
  End;
End;

Procedure TNGDJointBall.StructureChanged(Sender: TObject);
Begin
  Inherited;
  If Assigned(FManager) Then
  Begin
    If FNewtonJoint <> Nil Then
    Begin
      NewtonDestroyJoint(FManager.FNewtonWorld, FNewtonJoint);
      FNewtonJoint := Nil;
    End;

    If self.Name = 'Ball' Then
      If Assigned(Object1) And Assigned(Object2) Then
      Begin
        FNewtonJoint := NewtonConstraintCreateBall(FManager.FNewtonWorld,
          @(FPivotPoint.AsVector), GetBodyFromGLSceneObject(Object1),
          GetBodyFromGLSceneObject(Object2));
        CollisionState := FCollisionState;
        Stiffness := FStiffness;
      End;
  End;
End;

Procedure TNGDJointBall.SetStiffness(val: Single);
Begin
  If (val >= 0) And (val <= 1) Then
    FStiffness := val;
  If FNewtonJoint <> Nil Then
    NewtonJointSetStiffness(FNewtonJoint, FStiffness);
End;

Procedure TNGDJointBall.SetCollisionState(val: Boolean);
Begin
  FCollisionState := val;
  If FNewtonJoint <> Nil Then
    NewtonJointSetCollisionState(FNewtonJoint, Ord(FCollisionState));
End;

{ TNGDJointHinge }

Constructor TNGDJointHinge.Create(AOwner: TXCollection);
Begin
  Inherited;
  FPinDir := TGLCoordinates.CreateInitialized(self, ZHmgVector, csVector);
  FPinDir.OnNotifyChange := StructureChanged;
End;

Destructor TNGDJointHinge.Destroy;
Begin
  FPinDir.Free;
  Inherited;
End;

Class Function TNGDJointHinge.FriendlyDescription: String;
Begin
  Result := 'NGD Hinge joint';
End;

Class Function TNGDJointHinge.FriendlyName: String;
Begin
  Result := 'Hinge';
End;

Procedure TNGDJointHinge.ReadFromFiler(reader: TReader);
Begin
  Inherited;
  Assert(reader.ReadInteger = 0);
  // Archive version
  FPinDir.ReadFromFiler(reader);
End;

Procedure TNGDJointHinge.WriteToFiler(writer: TWriter);
Begin
  Inherited;
  writer.WriteInteger(0);
  // Archive version
  FPinDir.WriteToFiler(writer);
End;

Procedure TNGDJointHinge.Render;
Var
  axe: TVector;
Begin
  Inherited;
  If Assigned(Object1) AND Assigned(Object2) Then
  Begin
    axe[0] := FPivotPoint.x - 10 * FPinDir.x;
    axe[1] := FPivotPoint.y - 10 * FPinDir.y;
    axe[2] := FPivotPoint.z - 10 * FPinDir.z;
    glVertex3fv(@axe);
    axe[0] := FPivotPoint.x + 10 * FPinDir.x;
    axe[1] := FPivotPoint.y + 10 * FPinDir.y;
    axe[2] := FPivotPoint.z + 10 * FPinDir.z;
    glVertex3fv(@axe);
  End;
End;

Procedure TNGDJointHinge.StructureChanged(Sender: TObject);
Begin
  Inherited;
  If Assigned(FManager) Then
    If self.Name = 'Hinge' Then
    Begin
      FPinDir.OnNotifyChange := Nil;
      FPinDir.Normalize;
      FPinDir.OnNotifyChange := StructureChanged;

      If Assigned(Object1) And Assigned(Object2) Then
      Begin
        FNewtonJoint := NewtonConstraintCreateHinge(FManager.FNewtonWorld,
          @(FPivotPoint.AsVector), @(FPinDir.AsVector),
          GetBodyFromGLSceneObject(Object1), GetBodyFromGLSceneObject(Object2));
        CollisionState := FCollisionState;
        Stiffness := FStiffness;
      End;
    End;
End;

{ TNGDJointSlider }

Class Function TNGDJointSlider.FriendlyDescription: String;
Begin
  Result := 'NGD Slider joint';
End;

Class Function TNGDJointSlider.FriendlyName: String;
Begin
  Result := 'Slider';
End;

Procedure TNGDJointSlider.StructureChanged(Sender: TObject);
Begin
  Inherited;
  If Assigned(FManager) Then
    If self.Name = 'Slider' Then
    Begin
      FPinDir.OnNotifyChange := Nil;
      FPinDir.Normalize;
      FPinDir.OnNotifyChange := StructureChanged;

      If Assigned(Object1) And Assigned(Object2) Then
      Begin
        FNewtonJoint := NewtonConstraintCreateSlider(FManager.FNewtonWorld,
          @(FPivotPoint.AsVector), @(FPinDir.AsVector),
          GetBodyFromGLSceneObject(Object1), GetBodyFromGLSceneObject(Object2));
        CollisionState := FCollisionState;
        Stiffness := FStiffness;
      End;
    End;
End;

{ TNGDJointCorkscrew }

Class Function TNGDJointCorkscrew.FriendlyDescription: String;
Begin
  Result := 'NGD Corkscrew joint';
End;

Class Function TNGDJointCorkscrew.FriendlyName: String;
Begin
  Result := 'Corkscrew';
End;

Procedure TNGDJointCorkscrew.StructureChanged(Sender: TObject);
Begin
  Inherited;
  If Assigned(FManager) Then
    If self.Name = 'Corkscrew' Then
    Begin
      FPinDir.OnNotifyChange := Nil;
      FPinDir.Normalize;
      FPinDir.OnNotifyChange := StructureChanged;

      If Assigned(Object1) And Assigned(Object2) Then
      Begin
        FNewtonJoint := NewtonConstraintCreateCorkscrew(FManager.FNewtonWorld,
          @(FPivotPoint.AsVector), @(FPinDir.AsVector),
          GetBodyFromGLSceneObject(Object1), GetBodyFromGLSceneObject(Object2));
        CollisionState := FCollisionState;
        Stiffness := FStiffness;
      End;
    End;
End;

{ TNGDJointUniversal }

Constructor TNGDJointUniversal.Create(AOwner: TXCollection);
Begin
  Inherited;
  FPinDir2 := TGLCoordinates.CreateInitialized(self, ZHmgVector, csVector);
  FPinDir2.OnNotifyChange := StructureChanged;
End;

Destructor TNGDJointUniversal.Destroy;
Begin
  FPinDir2.Free;
  Inherited;
End;

Class Function TNGDJointUniversal.FriendlyDescription: String;
Begin
  Result := 'NGD Universal joint';
End;

Class Function TNGDJointUniversal.FriendlyName: String;
Begin
  Result := 'Universal';
End;

Procedure TNGDJointUniversal.ReadFromFiler(reader: TReader);
Begin
  Inherited;
  Assert(reader.ReadInteger = 0);
  // Archive version
  FPinDir2.ReadFromFiler(reader);
End;

Procedure TNGDJointUniversal.WriteToFiler(writer: TWriter);
Begin
  Inherited;
  writer.WriteInteger(0);
  // Archive version
  FPinDir2.WriteToFiler(writer);
End;

Procedure TNGDJointUniversal.Render;
Var
  axe: TVector;
Begin
  Inherited;
  If Assigned(Object1) AND Assigned(Object2) Then
  Begin
    axe[0] := FPivotPoint.x - 10 * FPinDir2.x;
    axe[1] := FPivotPoint.y - 10 * FPinDir2.y;
    axe[2] := FPivotPoint.z - 10 * FPinDir2.z;
    glVertex3fv(@axe);
    axe[0] := FPivotPoint.x + 10 * FPinDir2.x;
    axe[1] := FPivotPoint.y + 10 * FPinDir2.y;
    axe[2] := FPivotPoint.z + 10 * FPinDir2.z;
    glVertex3fv(@axe);
  End;
End;

Procedure TNGDJointUniversal.StructureChanged(Sender: TObject);
Begin
  Inherited;
  If Assigned(FManager) Then
    If self.Name = 'Universal' Then
    Begin
      FPinDir.OnNotifyChange := Nil;
      FPinDir2.OnNotifyChange := Nil;
      FPinDir.Normalize;
      FPinDir2.Normalize;
      FPinDir.OnNotifyChange := StructureChanged;
      FPinDir2.OnNotifyChange := StructureChanged;

      If Assigned(Object1) And Assigned(Object2) Then
      Begin
        FNewtonJoint := NewtonConstraintCreateUniversal(FManager.FNewtonWorld,
          @(FPivotPoint.AsVector), @(FPinDir.AsVector), @(FPinDir2.AsVector),
          GetBodyFromGLSceneObject(Object1), GetBodyFromGLSceneObject(Object2));
        CollisionState := FCollisionState;
        Stiffness := FStiffness;
      End;
    End;
End;

{ TNGDCustomJointBase }

Constructor TNGDCustomJointBase.Create(AOwner: TXCollection);
Begin
  Inherited;
  FNewtonUserJoint := Nil;
  FStiffness := 0.9;
  FCollisionState := False;
  FMinLimit := -50;
  FMaxLimit := 50;
  FPivotPoint := TGLCoordinates.CreateInitialized(self, NullHMGPoint, csPoint);
  FPivotPoint.OnNotifyChange := StructureChanged;
End;

Destructor TNGDCustomJointBase.Destroy;
Begin
  If FNewtonUserJoint <> Nil Then
    CustomDestroyJoint(FNewtonUserJoint);
  FNewtonUserJoint := Nil;
  FPivotPoint.Free;
  Inherited;
End;

Procedure TNGDCustomJointBase.ReadFromFiler(reader: TReader);
Begin
  Inherited;
  With reader Do
  Begin
    Assert(ReadInteger = 0);
    // Archive version
    FStiffness := ReadSingle;
    FCollisionState := ReadBoolean;
    FMinLimit := ReadSingle;
    FMaxLimit := ReadSingle;
  End;
  FPivotPoint.ReadFromFiler(reader);
End;

Procedure TNGDCustomJointBase.Render;
Var
  bar1, bar2: TVector;
Begin
  If Assigned(Object1) AND Assigned(Object2) Then
  Begin
    bar1 := Object1.BarycenterAbsolutePosition;
    bar2 := Object2.BarycenterAbsolutePosition;
    glVertex3fv(@bar1);
    glVertex3fv(FPivotPoint.AsAddress);
    glVertex3fv(FPivotPoint.AsAddress);
    glVertex3fv(@bar2);
  End;
End;

Procedure TNGDCustomJointBase.SetCollisionState(val: Boolean);
Begin
  FCollisionState := val;
  If FNewtonUserJoint <> Nil Then
    CustomSetBodiesCollisionState(FNewtonUserJoint, Ord(FCollisionState));
End;

Procedure TNGDCustomJointBase.SetMaxLimit(val: Single);
Begin
  // Virtual
End;

Procedure TNGDCustomJointBase.SetMinLimit(val: Single);
Begin
  // Virtual
End;

Procedure TNGDCustomJointBase.SetStiffness(val: Single);
Begin
  If (val >= 0) And (val <= 1) Then
    FStiffness := val;
  If FNewtonUserJoint <> Nil Then
    NewtonJointSetStiffness(CustomGetNewtonJoint(FNewtonUserJoint), FStiffness);
End;

Procedure TNGDCustomJointBase.StructureChanged(Sender: TObject);
Begin
  Inherited;
  If Assigned(FManager) Then
    If FNewtonUserJoint <> Nil Then
    Begin
      CustomDestroyJoint(FNewtonUserJoint);
      FNewtonUserJoint := Nil;
    End;
  FPinAndPivotMatrix := IdentityHmgMatrix;
  FPinAndPivotMatrix[3, 0] := FPivotPoint.x;
  FPinAndPivotMatrix[3, 1] := FPivotPoint.y;
  FPinAndPivotMatrix[3, 2] := FPivotPoint.z;
End;

Procedure TNGDCustomJointBase.WriteToFiler(writer: TWriter);
Begin
  Inherited;
  With writer Do
  Begin
    WriteInteger(0);
    // Archive version
    WriteSingle(FStiffness);
    WriteBoolean(FCollisionState);
    WriteSingle(FMinLimit);
    WriteSingle(FMaxLimit);
  End;
  FPivotPoint.WriteToFiler(writer);
End;

{ TNGDCustomJointBall }

Constructor TNGDCustomJointBall.Create(AOwner: TXCollection);
Begin
  Inherited;
  FConeAngle := 90;
  FMinLimit := -90;
  FMaxLimit := 90;
End;

Class Function TNGDCustomJointBall.FriendlyDescription: String;
Begin
  Result := 'NGD Custom Ball joint implementation';
End;

Class Function TNGDCustomJointBall.FriendlyName: String;
Begin
  Result := 'CustomBall';
End;

Procedure TNGDCustomJointBall.ReadFromFiler(reader: TReader);
Begin
  Inherited;
  With reader Do
  Begin
    Assert(ReadInteger = 0);
    // Archive version
    FConeAngle := ReadSingle;
  End;
End;

Procedure TNGDCustomJointBall.SetConeAngle(val: Single);
Begin
  If (val >= 0) AND (val < 180) Then
    FConeAngle := val;
  If FNewtonUserJoint <> Nil Then
    BallAndSocketSetConeAngle(FNewtonUserJoint, DegToRad(FConeAngle));
End;

Procedure TNGDCustomJointBall.SetMaxLimit(val: Single);
Begin
  If (val >= FMinLimit) Then
    FMaxLimit := val;
  If FNewtonUserJoint <> Nil Then
    BallAndSocketSetTwistAngle(FNewtonUserJoint, DegToRad(FMinLimit),
      DegToRad(FMaxLimit));
End;

Procedure TNGDCustomJointBall.SetMinLimit(val: Single);
Begin
  If (val <= FMaxLimit) Then
    FMinLimit := val;
  If FNewtonUserJoint <> Nil Then
    BallAndSocketSetTwistAngle(FNewtonUserJoint, DegToRad(FMinLimit),
      DegToRad(FMaxLimit));
End;

Procedure TNGDCustomJointBall.StructureChanged(Sender: TObject);
Begin
  Inherited;
  If Assigned(FManager) Then
    If self.Name = 'CustomBall' Then
      If Assigned(Object1) And Assigned(Object2) Then
      Begin
        FNewtonUserJoint := CreateCustomBallAndSocket(@FPinAndPivotMatrix,
          GetBodyFromGLSceneObject(Object1), GetBodyFromGLSceneObject(Object2));
        BallAndSocketSetTwistAngle(FNewtonUserJoint, DegToRad(FMinLimit),
          DegToRad(FMaxLimit));
        BallAndSocketSetConeAngle(FNewtonUserJoint, DegToRad(FConeAngle));
        CollisionState := FCollisionState;
        Stiffness := FStiffness;
      End;
End;

Procedure TNGDCustomJointBall.WriteToFiler(writer: TWriter);
Begin
  Inherited;
  With writer Do
  Begin
    WriteInteger(0);
    // Archive version
    WriteSingle(FConeAngle);
  End;
End;

{ TNGDCustomJointBaseDir }

Constructor TNGDCustomJointBaseDir.Create(AOwner: TXCollection);
Begin
  Inherited;
  FPinDir := TGLCoordinates.CreateInitialized(self, ZHmgVector, csVector);
  FPinDir.OnNotifyChange := StructureChanged;
End;

Destructor TNGDCustomJointBaseDir.Destroy;
Begin
  FPinDir.Free;
  Inherited;
End;

Procedure TNGDCustomJointBaseDir.ReadFromFiler(reader: TReader);
Begin
  Inherited;
  With reader Do
  Begin
    Assert(ReadInteger = 0);
    // Archive version
  End;
  FPinDir.ReadFromFiler(reader);
End;

Procedure TNGDCustomJointBaseDir.Render;
Var
  axe: TVector;
Begin
  Inherited;
  If Assigned(Object1) AND Assigned(Object2) Then
  Begin
    axe[0] := FPivotPoint.x - 10 * FPinDir.x;
    axe[1] := FPivotPoint.y - 10 * FPinDir.y;
    axe[2] := FPivotPoint.z - 10 * FPinDir.z;
    glVertex3fv(@axe);
    axe[0] := FPivotPoint.x + 10 * FPinDir.x;
    axe[1] := FPivotPoint.y + 10 * FPinDir.y;
    axe[2] := FPivotPoint.z + 10 * FPinDir.z;
    glVertex3fv(@axe);
  End;
End;

Procedure TNGDCustomJointBaseDir.StructureChanged(Sender: TObject);
Var
  bso: TGLBaseSceneObject;
  Line: TVector;
Begin
  Inherited;
  FPinDir.OnNotifyChange := Nil;
  FPinDir.Normalize;
  FPinDir.OnNotifyChange := StructureChanged;

  If Assigned(FManager) Then
  Begin
    bso := TGLBaseSceneObject.Create(FManager);
    bso.AbsolutePosition := FPivotPoint.AsVector;
    bso.AbsoluteDirection := FPinDir.AsVector;
    FPinAndPivotMatrix := bso.AbsoluteMatrix;
    bso.Free;

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

  End;

End;

Procedure TNGDCustomJointBaseDir.WriteToFiler(writer: TWriter);
Begin
  Inherited;
  With writer Do
  Begin
    WriteInteger(0);
    // Archive version
  End;
  FPinDir.WriteToFiler(writer);
End;

{ TNGDCustomJointHinge }

Constructor TNGDCustomJointHinge.Create(AOwner: TXCollection);
Begin
  Inherited;
  FMinLimit := -90;
  FMaxLimit := 90;
End;

Class Function TNGDCustomJointHinge.FriendlyDescription: String;
Begin
  Result := 'NGD Custom Hinge joint implementation';
End;

Class Function TNGDCustomJointHinge.FriendlyName: String;
Begin
  Result := 'CustomHinge';
End;

Procedure TNGDCustomJointHinge.SetMaxLimit(val: Single);
Begin
  If (val >= FMinLimit) Then
    FMaxLimit := val;
  If FNewtonUserJoint <> Nil Then
    HingeSetLimis(FNewtonUserJoint, DegToRad(FMinLimit), DegToRad(FMaxLimit));
End;

Procedure TNGDCustomJointHinge.SetMinLimit(val: Single);
Begin
  If (val <= FMaxLimit) Then
    FMinLimit := val;
  If FNewtonUserJoint <> Nil Then
    HingeSetLimis(FNewtonUserJoint, DegToRad(FMinLimit), DegToRad(FMaxLimit));
End;

Procedure TNGDCustomJointHinge.StructureChanged(Sender: TObject);
Begin
  Inherited;
  If Assigned(FManager) Then
    If self.Name = 'CustomHinge' Then
      If Assigned(Object1) And Assigned(Object2) Then
      Begin
        FNewtonUserJoint := CreateCustomHinge(@FPinAndPivotMatrix,
          GetBodyFromGLSceneObject(Object1), GetBodyFromGLSceneObject(Object2));
        HingeEnableLimits(FNewtonUserJoint, 1);
        HingeSetLimis(FNewtonUserJoint, DegToRad(FMinLimit),
          DegToRad(FMaxLimit));
        CollisionState := FCollisionState;
        Stiffness := FStiffness;
      End;
End;

{ TNGDCustomJointSlider }

Constructor TNGDCustomJointSlider.Create(AOwner: TXCollection);
Begin
  Inherited;
  FMinLimit := -10;
  FMaxLimit := 10;
End;

Class Function TNGDCustomJointSlider.FriendlyDescription: String;
Begin
  Result := 'NGD Custom Slider joint implementation';
End;

Class Function TNGDCustomJointSlider.FriendlyName: String;
Begin
  Result := 'CustomSlider';
End;

Procedure TNGDCustomJointSlider.SetMaxLimit(val: Single);
Begin
  If (val >= FMinLimit) Then
    FMaxLimit := val;
  If FNewtonUserJoint <> Nil Then
    SliderSetLimis(FNewtonUserJoint, FMinLimit, FMaxLimit);
End;

Procedure TNGDCustomJointSlider.SetMinLimit(val: Single);
Begin
  If (val <= FMaxLimit) Then
    FMinLimit := val;
  If FNewtonUserJoint <> Nil Then
    SliderSetLimis(FNewtonUserJoint, FMinLimit, FMaxLimit);
End;

Procedure TNGDCustomJointSlider.StructureChanged(Sender: TObject);
Begin
  Inherited;
  If Assigned(FManager) Then
    If self.Name = 'CustomSlider' Then
      If Assigned(Object1) And Assigned(Object2) Then
      Begin
        FNewtonUserJoint := CreateCustomSlider(@FPinAndPivotMatrix,
          GetBodyFromGLSceneObject(Object1), GetBodyFromGLSceneObject(Object2));
        SliderEnableLimits(FNewtonUserJoint, 1);
        SliderSetLimis(FNewtonUserJoint, FMinLimit, FMaxLimit);
        CollisionState := FCollisionState;
        Stiffness := FStiffness;
      End;
End;

Initialization

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

Finalization

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

End.
