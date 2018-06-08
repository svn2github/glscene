//
// VXScene Component Library, based on GLScene http://glscene.sourceforge.net 
//
{
   Base classes for scene-wide blended particles FX. 

   These provide a mechanism to render heterogenous particles systems with per
   particle depth-sorting (allowing correct rendering of interwoven separate
   fire and smoke particle systems for instance). 
    
}
unit VXS.ParticleFX;

interface

{$I VXScene.inc}

uses
  System.Classes,
  System.SysUtils,
  System.Types,
  System.Math,

  VXS.OpenGL,
  VXS.Scene,
  VXS.CrossPlatform,
  VXS.State,
  VXS.VectorTypes,
  VXS.PersistentClasses,
  VXS.VectorGeometry,
  VXS.XCollection,
  VXS.Material,
  VXS.Cadencer,
  VXS.VectorLists,
  VXS.Graphics,
  VXS.Context,
  VXS.Color, 
  VXS.BaseClasses,
  VXS.Coordinates, 
  VXS.RenderContextInfo, 
  VXS.Manager,
  VXS.PipelineTransformation,
  VXS.TextureFormat;

const
  cPFXNbRegions = 128; // number of distance regions
  cPFXGranularity = 128; // granularity of particles per region

type

  TVXParticleList = class;
  TVXParticleFXManager = class;
  TVXParticleFXEffect = class;

  { Base class for particles. 
     The class implements properties for position, velocity and time, whatever
     you need in excess of that will have to be placed in subclasses (this
     class should remain as compact as possible). }
  TVXParticle = class(TPersistentObject)
  private
    FID, FTag: Integer;
    FManager: TVXParticleFXManager; // NOT persistent
    FPosition: TAffineVector;
    FVelocity: TAffineVector;
    FRotation: Single;
    FCreationTime: Double;
    FEffectScale: Single;
    function GetPosition(const Index: Integer): Single;
    procedure WritePosition(const Index: Integer; const aValue: Single);
    function GetVelocity(const Index: Integer): Single;
    procedure WriteVelocity(const Index: Integer; const aValue: Single);
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure WriteToFiler(writer: TVirtualWriter); override;
    procedure ReadFromFiler(reader: TVirtualReader); override;
    property Manager: TVXParticleFXManager read FManager write FManager;
    { Particle's ID, given at birth. 
       ID is a value unique per manager. }
    property ID: Integer read FID;
    { Particle's absolute position. 
       Note that this property is read-accessed directly at rendering time
       in the innards of the depth-sorting code. }
    property Position: TAffineVector read FPosition write FPosition;
    { Particle's velocity.
       This velocity is indicative and is NOT automatically applied
       to the position during progression events by this class (subclasses
       may implement that). }
    property Velocity: TAffineVector read FVelocity write FVelocity;
    { Time at which particle was created }
    property CreationTime: Double read FCreationTime write FCreationTime;

    property PosX : Single index 0 read GetPosition write WritePosition;
    property PosY : Single index 1 read GetPosition write WritePosition;
    property PosZ : Single index 2 read GetPosition write WritePosition;
    property VelX : Single index 0 read GetVelocity write WriteVelocity;
    property VelY : Single index 1 read GetVelocity write WriteVelocity;
    property VelZ : Single index 2 read GetVelocity write WriteVelocity;
    property Tag: Integer read FTag write FTag;
  end;

  TVXParticleClass = class of TVXParticle;
  TVXParticleArray = array[0..MaxInt shr 4] of TVXParticle;
  PGLParticleArray = ^TVXParticleArray;

  { List of particles.
     This list is managed with particles and performance in mind, make sure to
     check methods doc. }
  TVXParticleList = class(TPersistentObject)
  private
    FOwner: TVXParticleFXManager; // NOT persistent
    FItemList: TPersistentObjectList;
    FDirectList: PGLParticleArray; // NOT persistent
  protected
    function GetItems(index: Integer): TVXParticle;
    procedure SetItems(index: Integer; val: TVXParticle);
    procedure AfterItemCreated(Sender: TObject);
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure WriteToFiler(writer: TVirtualWriter); override;
    procedure ReadFromFiler(reader: TVirtualReader); override;
    { Refers owner manager }
    property Owner: TVXParticleFXManager read FOwner write FOwner;
    property Items[index: Integer]: TVXParticle read GetItems write SetItems; default;
    function ItemCount: Integer;
    { Adds a particle to the list.
       Particle owneship is defined blindly, if the particle was previously
       in another list, it won't be automatically removed from that list. }
    function AddItem(aItem: TVXParticle): Integer;
    { Removes and frees a particular item for the list.
       If the item is not part of the list, nothing is done.
       If found in the list, the item's "slot" is set to nil and item is
       freed (after setting its ownership to nil). The nils can be removed
       with a call to Pack. }
    procedure RemoveAndFreeItem(aItem: TVXParticle);
    function IndexOfItem(aItem: TVXParticle): Integer;
    { Packs the list by removing all "nil" items.
       Note: this functions is orders of magnitude faster than the TList
       version. }
    procedure Pack;
    property List: PGLParticleArray read FDirectList;
  end;

  TVXParticleFXRenderer = class;
  TPFXCreateParticleEvent = procedure(Sender: TObject; aParticle: TVXParticle) of object;

  { Base class for particle FX managers.
     Managers take care of life and death of particles for a particular
     particles FX system. You can have multiple scene-wide particle
     FX managers in a scene, handled by the same ParticleFxRenderer.
     Before subclassing, make sure you understood how the Initialize/Finalize
     Rendering, Begin/End Particles and RenderParticles methods (and also
     understood that rendering of manager's particles may be interwoven). }
  TVXParticleFXManager = class(TVXCadencedComponent)
  private
    FBlendingMode: TBlendingMode;
    FRenderer: TVXParticleFXRenderer;
    FParticles: TVXParticleList;
    FNextID: Integer;
    FOnCreateParticle: TPFXCreateParticleEvent;
    FAutoFreeWhenEmpty: Boolean;
    FUsers: TList; //list of objects that use this manager
  protected
    procedure SetRenderer(const val: TVXParticleFXRenderer);
    procedure SetParticles(const aParticles: TVXParticleList);
    { Texturing mode for the particles.
       Subclasses should return GL_TEXTURE_1D, 2D or 3D depending on their
       needs, and zero if they don't use texturing. This method is used
       to reduce the number of texturing activations/deactivations. }
    function TexturingMode: Cardinal; virtual; abstract;
    { Invoked when the particles of the manager will be rendered.
       This method is fired with the "base" OpenVX states and matrices
       that will be used throughout the whole rendering, per-frame
       initialization should take place here. 
       OpenVX states/matrices should not be altered in any way here. }
    procedure InitializeRendering(var rci: TVXRenderContextInfo); virtual; abstract;
    { Triggered just before rendering a set of particles. 
       The current OpenVX state should be assumed to be the "base" one as
       was found during InitializeRendering. Manager-specific states should
       be established here. 
       Multiple BeginParticles can occur during a render (but all will be
       between InitializeRendering and Finalizerendering, and at least one
       particle will be rendered before EndParticles is invoked). }
    procedure BeginParticles(var rci: TVXRenderContextInfo); virtual; abstract;
    { Request to render a particular particle. 
       Due to the nature of the rendering, no particular order should be
       assumed. If possible, no OpenVX state changes should be made in this
       method, but should be placed in Begin/EndParticles. }
    procedure RenderParticle(var rci: TVXRenderContextInfo; aParticle: TVXParticle); virtual; abstract;
    { Triggered after a set of particles as been rendered. 
       If OpenVX state were altered directly (ie. not through the states
       caches of GLMisc), it should be restored back to the "base" state. }
    procedure EndParticles(var rci: TVXRenderContextInfo); virtual; abstract;
    { Invoked when rendering of particles for this manager is done. }
    procedure FinalizeRendering(var rci: TVXRenderContextInfo); virtual; abstract;
    { ID for the next created particle. }
    property NextID: Integer read FNextID write FNextID;
    { Blending mode for the particles. 
       Protected and unused in the base class. }
    property BlendingMode: TBlendingMode read FBlendingMode write FBlendingMode;
    { Apply BlendingMode relatively to the renderer's blending mode. }
    procedure ApplyBlendingMode(var rci: TVXRenderContextInfo);
    { Unapply BlendingMode relatively by restoring the renderer's blending mode. }
    procedure UnapplyBlendingMode(var rci: TVXRenderContextInfo);
    procedure registerUser(obj: TVXParticleFXEffect);
    procedure unregisterUser(obj: TVXParticleFXEffect);
  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
    procedure NotifyChange(Sender: TObject); override;
    procedure DoProgress(const progressTime: TVXProgressTimes); override;
    // Class of particles created by this manager. }
    class function ParticlesClass: TVXParticleClass; virtual;
    { Creates a new particle controlled by the manager. }
    function CreateParticle: TVXParticle; virtual;
    { Create several particles at once. }
    procedure CreateParticles(nbParticles: Integer);
    { A TVXParticleList property. }
    property Particles: TVXParticleList read FParticles write SetParticles;
    { Return the number of particles. 
       Note that subclasses may decide to return a particle count inferior
       to Particles.ItemCount, and the value returned by this method will
       be the one honoured at render time. }
    function ParticleCount: Integer; virtual;
    { If True the manager will free itself when its particle count reaches zero. 
       Check happens in the progression event, use with caution and only
       if you know what you're doing! }
    property AutoFreeWhenEmpty: Boolean read FAutoFreeWhenEmpty write FAutoFreeWhenEmpty;
  published
    { References the renderer. 
      The renderer takes care of ordering the particles of the manager
      (and other managers linked to it) and rendering them all depth-sorted. }
    property Renderer: TVXParticleFXRenderer read FRenderer write SetRenderer;
    { Event triggered after standard particle creation and initialization. }
    property OnCreateParticle: TPFXCreateParticleEvent read FOnCreateParticle write FOnCreateParticle;
    property Cadencer;
  end;

  { Base class for linking scene objects to a particle FX manager.  }
  TVXParticleFXEffect = class(TVXObjectPostEffect)
  private
    FManager: TVXParticleFXManager;
    FManagerName: string;
    FEffectScale: single;
    procedure SetEffectScale(const Value: single); // NOT persistent, temporarily used for persistence
  protected
    procedure SetManager(val: TVXParticleFXManager);
    procedure WriteToFiler(writer: TWriter); override;
    procedure ReadFromFiler(reader: TReader); override;
    procedure Loaded; override;
    procedure managerNotification(aManager: TVXParticleFXManager; Operation: TOperation);
  public
    constructor Create(aOwner: TXCollection); override;
    destructor Destroy; override;
  published
    { Reference to the Particle FX manager }
    property Manager: TVXParticleFXManager read FManager write SetManager;
    property EffectScale: single read FEffectScale write SetEffectScale;
  end;

  // PFX region rendering structures
  TParticleReference = packed record
    particle: TVXParticle;
    distance: Integer; // stores an IEEE single!
  end;
  PParticleReference = ^TParticleReference;
  TParticleReferenceArray = packed array[0..MaxInt shr 8-1] of TParticleReference;
  PParticleReferenceArray = ^TParticleReferenceArray;
  PFXPointerList = ^TFXPointerList;
  TFXPointerList = array[0..MaxInt shr 8-1] of Pointer;
  TPFXRegion = record
    count, capacity: Integer;
    particleRef: PParticleReferenceArray;
    particleOrder: PFXPointerList;
  end;
  PPFXRegion = ^TPFXRegion;

  TPFXSortAccuracy = (saLow, saOneTenth, saOneThird, saOneHalf, saHigh);

  { Rendering interface for scene-wide particle FX. 
     A renderer can take care of rendering any number of particle systems,
     its main task being to depth-sort the particles so that they are blended
     appropriately. 
     This object will usually be placed at the end of the scene hierarchy,
     just before the HUD overlays, its position, rotation etc. is of no
     importance and has no effect on the rendering of the particles. }
  TVXParticleFXRenderer = class(TVXBaseSceneObject)
  private
    FManagerList: TList;
    FLastSortTime: Double;
    FLastParticleCount: Integer;
    FZWrite, FZTest, FZCull: Boolean;
    FZSortAccuracy: TPFXSortAccuracy;
    FZMaxDistance: Single;
    FBlendingMode: TBlendingMode;
    FRegions: array[0..cPFXNbRegions - 1] of TPFXRegion;
  protected
    function StoreZMaxDistance: Boolean;
    { Register a manager }
    procedure RegisterManager(aManager: TVXParticleFXManager);
    { UnRegister a manager }
    procedure UnRegisterManager(aManager: TVXParticleFXManager);
    procedure UnRegisterAll;
  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
   { Quick Explanation of what is below:
   The purpose is to depth-sort a large number (thousandths) of particles and
   render them back to front. The rendering part is not particularly complex,
   it just invokes the various PFX managers involved and request particle
   renderings.
   The sort uses a first-pass region partition (the depth range is split into
   regions, and particles are assigned directly to the region they belong to),
   then each region is sorted with a QuickSort.
   The QuickSort itself is the regular classic variant, but the comparison is
   made on singles as if they were integers, this is allowed by the IEEE format
   in a very efficient manner if all values are superior to 1, which is ensured
   by the distance calculation and a fixed offset of 1 }
    procedure BuildList(var rci: TVXRenderContextInfo); override;
    { Time (in msec) spent sorting the particles for last render. }
    property LastSortTime: Double read FLastSortTime;
    { Amount of particles during the last render. }
    property LastParticleCount: Integer read FLastParticleCount;
  published
    {Specifies if particles should write to ZBuffer.
     If the PFXRenderer is the last object to be rendered in the scene,
     it is not necessary to write to the ZBuffer since the particles
     are depth-sorted. Writing to the ZBuffer has a performance penalty. }
    property ZWrite: Boolean read FZWrite write FZWrite default False;
    { Specifies if particles should write to test ZBuffer.  }
    property ZTest: Boolean read FZTest write FZTest default True;
    { If true the renderer will cull particles that are behind the camera. }
    property ZCull: Boolean read FZCull write FZCull default True;
    { If true particles will be accurately sorted back to front. 
       When false, only a rough ordering is used, which can result in
       visual glitches but may be faster. }
    property ZSortAccuracy: TPFXSortAccuracy read FZSortAccuracy write FZSortAccuracy default saHigh;
    { Maximum distance for rendering PFX particles. 
       If zero, camera's DepthOfView is used. }
    property ZMaxDistance: Single read FZMaxDistance write FZMaxDistance stored StoreZMaxDistance;
    { Default blending mode for particles. 
       "Additive" blending is the usual mode (increases brightness and
       saturates), "transparency" may be used for smoke or systems that
       opacify view, "opaque" is more rarely used. 
       Note: specific PFX managers may override/ignore this setting. }
    property BlendingMode: TBlendingMode read FBlendingMode write FBlendingMode default bmAdditive;
    property Visible;
  end;

  TVXSourcePFXVelocityMode = (svmAbsolute, svmRelative);
  TVXSourcePFXPositionMode = (spmAbsoluteOffset, spmRelative);
  TVXSourcePFXDispersionMode = (sdmFast, sdmIsotropic);

  { Simple Particles Source.  }
  TVXSourcePFXEffect = class(TVXParticleFXEffect)
  private
    FInitialVelocity: TVXCoordinates;
    FInitialPosition: TVXCoordinates;
    FPositionDispersionRange: TVXCoordinates;
    FVelocityDispersion: Single;
    FPositionDispersion: Single;
    FParticleInterval: Single;
    FVelocityMode: TVXSourcePFXVelocityMode;
    FPositionMode: TVXSourcePFXPositionMode;
    FDispersionMode: TVXSourcePFXDispersionMode;
    FEnabled: Boolean;
    FDisabledIfOwnerInvisible: Boolean;
    FTimeRemainder: Double;
    FRotationDispersion: Single;
  protected
    procedure SetInitialVelocity(const val: TVXCoordinates);
    procedure SetInitialPosition(const val: TVXCoordinates);
    procedure SetPositionDispersionRange(const val: TVXCoordinates);
    procedure SetParticleInterval(const val: Single);
    procedure WriteToFiler(writer: TWriter); override;
    procedure ReadFromFiler(reader: TReader); override;
    function ParticleAbsoluteInitialPos: TAffineVector;
  public
    constructor Create(aOwner: TXCollection); override;
    destructor Destroy; override;
    class function FriendlyName: string; override;
    class function FriendlyDescription: string; override;
    procedure DoProgress(const progressTime: TVXProgressTimes); override;
    // Instantaneously creates nb particles
    procedure Burst(time: Double; nb: Integer);
    procedure RingExplosion(time: Double;
      minInitialSpeed, maxInitialSpeed: Single;
      nbParticles: Integer);
  published
    property InitialVelocity: TVXCoordinates read FInitialVelocity write SetInitialVelocity;
    property VelocityDispersion: Single read FVelocityDispersion write FVelocityDispersion;
    property InitialPosition: TVXCoordinates read FInitialPosition write SetInitialPosition;
    property PositionDispersion: Single read FPositionDispersion write FPositionDispersion;
    property PositionDispersionRange: TVXCoordinates read FPositionDispersionRange write SetPositionDispersionRange;
    property ParticleInterval: Single read FParticleInterval write SetParticleInterval;
    property VelocityMode: TVXSourcePFXVelocityMode read FVelocityMode write FVelocityMode default svmAbsolute;
    property PositionMode: TVXSourcePFXPositionMode read FPositionMode write FPositionMode default spmAbsoluteOffset;
    property DispersionMode: TVXSourcePFXDispersionMode read FDispersionMode write FDispersionMode default sdmFast;
    property RotationDispersion: Single read FRotationDispersion write FRotationDispersion;
    property Enabled: boolean read FEnabled write FEnabled;
    property DisabledIfOwnerInvisible: boolean read FDisabledIfOwnerInvisible write FDisabledIfOwnerInvisible;
  end;

  { An abstract PFX manager for simple dynamic particles. 
     Adds properties and progress implementation for handling moving particles
     (simple velocity and const acceleration integration). }
  TVXDynamicPFXManager = class(TVXParticleFXManager)
  private
    FAcceleration: TVXCoordinates;
    FFriction: Single;
    FCurrentTime: Double;
    //FRotationCenter: TAffineVector;
  protected
    procedure SetAcceleration(const val: TVXCoordinates);
    { Returns the maximum age for a particle.
       Particles older than that will be killed by DoProgress. }
    function MaxParticleAge: Single; virtual; abstract;
    property CurrentTime: Double read FCurrentTime;
  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
    procedure DoProgress(const progressTime: TVXProgressTimes); override;
  published
      { Oriented acceleration applied to the particles. }
    property Acceleration: TVXCoordinates read FAcceleration write SetAcceleration;
    { Friction applied to the particles. 
       Friction is applied as a speed scaling factor over 1 second, ie.
       a friction of 0.5 will half speed over 1 second, a friction of 3
       will triple speed over 1 second, and a friction of 1 (default
       value) will have no effect. }
    property Friction: Single read FFriction write FFriction;
  end;

  TPFXLifeColor = class(TCollectionItem)
  private
    FColorInner: TVXColor;
    FColorOuter: TVXColor;
    FLifeTime, FInvLifeTime: Single;
    FIntervalRatio: Single;
    FSizeScale: Single;
    FDoScale: Boolean;
    FDoRotate: boolean;
    FRotateAngle: Single;
  protected
    function GetDisplayName: string; override;
    procedure SetColorInner(const val: TVXColor);
    procedure SetColorOuter(const val: TVXColor);
    procedure SetLifeTime(const val: Single);
    procedure SetSizeScale(const val: Single);
    procedure SetRotateAngle(const Value: Single); // indirectly persistent
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    { Stores 1/LifeTime }
    property InvLifeTime: Single read FInvLifeTime;
    { Stores 1/(LifeTime[Next]-LifeTime[Self]) }
    property InvIntervalRatio: Single read FIntervalRatio;
  published
    property ColorInner: TVXColor read FColorInner write SetColorInner;
    property ColorOuter: TVXColor read FColorOuter write SetColorOuter;
    property LifeTime: Single read FLifeTime write SetLifeTime;
    property SizeScale: Single read FSizeScale write SetSizeScale;
    property RotateAngle: Single read FRotateAngle write SetRotateAngle;
  end;

  TPFXLifeColors = class(TOwnedCollection)
  protected
    procedure SetItems(index: Integer; const val: TPFXLifeColor);
    function GetItems(index: Integer): TPFXLifeColor;
  public
    constructor Create(AOwner: TPersistent);
    function Add: TPFXLifeColor;
    function FindItemID(ID: Integer): TPFXLifeColor;
    property Items[index: Integer]: TPFXLifeColor read GetItems write SetItems; default;
    function MaxLifeTime: Double;
    function RotationsDefined: Boolean;
    function ScalingDefined: Boolean;
    procedure PrepareIntervalRatios;
  end;

  { Base PFX manager for particles with life colors. 
     Particles have a core and edge color, for subclassing. }
  TVXLifeColoredPFXManager = class(TVXDynamicPFXManager)
  private
    FLifeColors: TPFXLifeColors;
    FLifeColorsLookup: TList;
    FLifeRotations: Boolean;
    FLifeScaling: Boolean;
    FColorInner: TVXColor;
    FColorOuter: TVXColor;
    FParticleSize: Single;
  protected
    procedure SetLifeColors(const val: TPFXLifeColors);
    procedure SetColorInner(const val: TVXColor);
    procedure SetColorOuter(const val: TVXColor);
    procedure InitializeRendering(var rci: TVXRenderContextInfo); override;
    procedure FinalizeRendering(var rci: TVXRenderContextInfo); override;
    function MaxParticleAge: Single; override;
    procedure ComputeColors(var lifeTime: Single; var inner, outer: TColorVector);
    procedure ComputeInnerColor(var lifeTime: Single; var inner: TColorVector);
    procedure ComputeOuterColor(var lifeTime: Single; var outer: TColorVector);
    function ComputeSizeScale(var lifeTime: Single; var sizeScale: Single): Boolean;
    function ComputeRotateAngle(var lifeTime, rotateAngle: Single): Boolean;
    procedure RotateVertexBuf(buf: TAffineVectorList; lifeTime: Single;
      const axis: TAffineVector; offsetAngle: Single);
  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
    property ParticleSize: Single read FParticleSize write FParticleSize;
    property ColorInner: TVXColor read FColorInner write SetColorInner;
    property ColorOuter: TVXColor read FColorOuter write SetColorOuter;
    property LifeColors: TPFXLifeColors read FLifeColors write SetLifeColors;
  published
    property BlendingMode default bmAdditive;
  end;

  TPFXDirectRenderEvent = procedure(Sender: TObject; aParticle: TVXParticle;
    var rci: TVXRenderContextInfo) of object;
  TPFXProgressEvent = procedure(Sender: TObject; const progressTime: TVXProgressTimes;
    var defaultProgress: Boolean) of object;
  TPFXParticleProgress = procedure(Sender: TObject; const progressTime: TVXProgressTimes;
    aParticle: TVXParticle; var killParticle: Boolean) of object;
  TPFXGetParticleCountEvent = function(Sender: TObject): Integer of object;

  { A particles FX manager offering events for customization/experimentation. 
     This manager essentially surfaces the PFX methods as events, and is best
     suited when you have specific particles that don't fall into any existing
     category, or when you want to experiment with particles and later plan to
     wrap things up in a full-blown manager. 
     If the events aren't handled, nothing will be rendered. }
  TVXCustomPFXManager = class(TVXLifeColoredPFXManager)
  private
    FOnInitializeRendering: TDirectRenderEvent;
    FOnBeginParticles: TDirectRenderEvent;
    FOnRenderParticle: TPFXDirectRenderEvent;
    FOnEndParticles: TDirectRenderEvent;
    FOnFinalizeRendering: TDirectRenderEvent;
    FOnProgress: TPFXProgressEvent;
    FOnParticleProgress: TPFXParticleProgress;
    FOnGetParticleCountEvent: TPFXGetParticleCountEvent;
  protected
    function TexturingMode: Cardinal; override;
    procedure InitializeRendering(var rci: TVXRenderContextInfo); override;
    procedure BeginParticles(var rci: TVXRenderContextInfo); override;
    procedure RenderParticle(var rci: TVXRenderContextInfo; aParticle: TVXParticle); override;
    procedure EndParticles(var rci: TVXRenderContextInfo); override;
    procedure FinalizeRendering(var rci: TVXRenderContextInfo); override;
  public
    procedure DoProgress(const progressTime: TVXProgressTimes); override;
    function ParticleCount: Integer; override;
  published
    property OnInitializeRendering: TDirectRenderEvent read FOnInitializeRendering write FOnInitializeRendering;
    property OnBeginParticles: TDirectRenderEvent read FOnBeginParticles write FOnBeginParticles;
    property OnRenderParticle: TPFXDirectRenderEvent read FOnRenderParticle write FOnRenderParticle;
    property OnEndParticles: TDirectRenderEvent read FOnEndParticles write FOnEndParticles;
    property OnFinalizeRendering: TDirectRenderEvent read FOnFinalizeRendering write FOnFinalizeRendering;
    property OnProgress: TPFXProgressEvent read FOnProgress write FOnProgress;
    property OnParticleProgress: TPFXParticleProgress read FOnParticleProgress write FOnParticleProgress;
    property OnGetParticleCountEvent: TPFXGetParticleCountEvent read FOnGetParticleCountEvent write FOnGetParticleCountEvent;
    property ParticleSize;
    property ColorInner;
    property ColorOuter;
    property LifeColors;
  end;

  { Polygonal particles FX manager. 
     The particles of this manager are made of N-face regular polygon with
     a core and edge color. No texturing is available. 
     If you render large particles and don't have T&L acceleration, consider
     using TVXPointLightPFXManager. }
  TVXPolygonPFXManager = class(TVXLifeColoredPFXManager)
  private
    FNbSides: Integer;
    Fvx, Fvy: TAffineVector; // NOT persistent
    FVertices: TAffineVectorList; // NOT persistent
    FVertBuf: TAffineVectorList; // NOT persistent
  protected
    procedure SetNbSides(const val: Integer);
    function TexturingMode: Cardinal; override;
    procedure InitializeRendering(var rci: TVXRenderContextInfo); override;
    procedure BeginParticles(var rci: TVXRenderContextInfo); override;
    procedure RenderParticle(var rci: TVXRenderContextInfo; aParticle: TVXParticle); override;
    procedure EndParticles(var rci: TVXRenderContextInfo); override;
    procedure FinalizeRendering(var rci: TVXRenderContextInfo); override;
  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
  published
    property NbSides: Integer read FNbSides write SetNbSides default 6;
    property ParticleSize;
    property ColorInner;
    property ColorOuter;
    property LifeColors;
  end;

  { Sprite color modes. 
      scmFade: vertex coloring is used to fade inner-outer
      scmInner: vertex coloring uses inner color only
      scmOuter: vertex coloring uses outer color only
      scmNone: vertex coloring is NOT used (colors are ignored). }
  TSpriteColorMode = (scmFade, scmInner, scmOuter, scmNone);

  { Sprites per sprite texture for the SpritePFX. }
  TSpritesPerTexture = (sptOne, sptFour);

  { Base class for sprite-based particles FX managers. 
     The particles are made of optionally centered single-textured quads. }
  TVXBaseSpritePFXManager = class(TVXLifeColoredPFXManager)
  private
    FTexHandle: TVXTextureHandle;
    Fvx, Fvy, Fvz: TAffineVector; // NOT persistent
    FVertices: TAffineVectorList; // NOT persistent
    FVertBuf: TAffineVectorList; // NOT persistent
    FAspectRatio: Single;
    FRotation: Single;
    FShareSprites: TVXBaseSpritePFXManager;
    FSpritesPerTexture: TSpritesPerTexture;
    FColorMode: TSpriteColorMode;
  protected
    { Subclasses should draw their stuff in this bmp32. }
    procedure PrepareImage(bmp32: TVXBitmap32; var texFormat: Integer); virtual; abstract;
    procedure BindTexture(var rci: TVXRenderContextInfo);
    procedure SetSpritesPerTexture(const val: TSpritesPerTexture); virtual;
    procedure SetColorMode(const val: TSpriteColorMode);
    procedure SetAspectRatio(const val: Single);
    function StoreAspectRatio: Boolean;
    procedure SetRotation(const val: Single);
    procedure SetShareSprites(const val: TVXBaseSpritePFXManager);
    function TexturingMode: Cardinal; override;
    procedure InitializeRendering(var rci: TVXRenderContextInfo); override;
    procedure BeginParticles(var rci: TVXRenderContextInfo); override;
    procedure RenderParticle(var rci: TVXRenderContextInfo; aParticle: TVXParticle); override;
    procedure EndParticles(var rci: TVXRenderContextInfo); override;
    procedure FinalizeRendering(var rci: TVXRenderContextInfo); override;
    property SpritesPerTexture: TSpritesPerTexture read FSpritesPerTexture write SetSpritesPerTexture;
  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
    property ColorMode: TSpriteColorMode read FColorMode write SetColorMode;
  published
      { Ratio between width and height. 
         An AspectRatio of 1 (default) will result in square sprite particles,
         values higher than one will result in horizontally stretched sprites,
         values below one will stretch vertically (assuming no rotation is applied). }
    property AspectRatio: Single read FAspectRatio write SetAspectRatio stored StoreAspectRatio;
    { Particle sprites rotation (in degrees). 
       All particles of the PFX manager share this rotation. }
    property Rotation: Single read FRotation write SetRotation;
    { If specified the manager will reuse the other manager's sprites. 
       Sharing sprites between PFX managers can help at the rendering stage
       if particles of the managers are mixed by helping reduce the number
       of texture switches. Note that only the texture is shared, not the
       colors, sizes or other dynamic parameters.  }
    property ShareSprites: TVXBaseSpritePFXManager read FShareSprites write FShareSprites;
  end;

  TPFXPrepareTextureImageEvent = procedure(Sender: TObject; destBmp32: TVXBitmap32; var texFormat: Integer) of object;

  { A sprite-based particles FX managers using user-specified code to prepare the texture.  }
  TVXCustomSpritePFXManager = class(TVXBaseSpritePFXManager)
  private
    FOnPrepareTextureImage: TPFXPrepareTextureImageEvent;
  protected
    procedure PrepareImage(bmp32: TVXBitmap32; var texFormat: Integer); override;
  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
  published
      { Place your texture rendering code in this event.  }
    property OnPrepareTextureImage: TPFXPrepareTextureImageEvent read FOnPrepareTextureImage write FOnPrepareTextureImage;
    property ColorMode default scmInner;
    property SpritesPerTexture default sptOne;
    property ParticleSize;
    property ColorInner;
    property ColorOuter;
    property LifeColors;
  end;

  { A sprite-based particles FX managers using point light maps. 
     The texture map is a round, distance-based transparency map (center "opaque"),
     you can adjust the quality (size) of the underlying texture map with the
     TexMapSize property. 
     This PFX manager renders particles similar to what you can get with
     TVXPolygonPFXManager but stresses fillrate more than T&L rate (and will
     usually be slower than the PolygonPFX when nbSides is low or T&L acceleration
     available). Consider this implementation as a sample for your own PFX managers
     that may use particles with more complex textures. }
  TVXPointLightPFXManager = class(TVXBaseSpritePFXManager)
  private
    FTexMapSize: Integer;
  protected
    procedure PrepareImage(bmp32: TVXBitmap32; var texFormat: Integer); override;
    procedure SetTexMapSize(const val: Integer);
  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
  published
      { Underlying texture map size, as a power of two. 
         Min value is 3 (size=8), max value is 9 (size=512). }
    property TexMapSize: Integer read FTexMapSize write SetTexMapSize default 5;
    property ColorMode default scmInner;
    property ParticleSize;
    property ColorInner;
    property ColorOuter;
    property LifeColors;
  end;

  { Returns or creates the TVXBInertia within the given object's behaviours.  }
function GetOrCreateSourcePFX(obj: TVXBaseSceneObject; const name: string = ''): TVXSourcePFXEffect;

// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------

function GetOrCreateSourcePFX(obj: TVXBaseSceneObject; const name: string = ''): TVXSourcePFXEffect;
var
  i: Integer;
begin
  with obj.Effects do
  begin
    if name = '' then
    begin
      i := IndexOfClass(TVXSourcePFXEffect);
      if i >= 0 then
        Result := TVXSourcePFXEffect(Items[i])
      else
        Result := TVXSourcePFXEffect.Create(obj.Effects);
    end
    else
    begin
      i := IndexOfName(name);
      if i >= 0 then
        Result := (Items[i] as TVXSourcePFXEffect)
      else
      begin
        Result := TVXSourcePFXEffect.Create(obj.Effects);
        Result.Name := name;
      end;
    end;
  end;
end;

// RndVector

procedure RndVector(const dispersion: TVXSourcePFXDispersionMode;
  var v: TAffineVector; var f: Single;
  dispersionRange: TVXCoordinates);
var
  f2, fsq: Single;
  p: TVector;
begin
  f2 := 2 * f;
  if Assigned(dispersionRange) then
    p := VectorScale(dispersionRange.DirectVector, f2)
  else
    p := VectorScale(XYZHmgVector, f2);
  case dispersion of
    sdmFast:
      begin
        v.X := (Random - 0.5) * p.X;
        v.Y := (Random - 0.5) * p.Y;
        v.Z := (Random - 0.5) * p.Z;
      end;
  else
    fsq := Sqr(0.5);
    repeat
      v.X := (Random - 0.5);
      v.Y := (Random - 0.5);
      v.Z := (Random - 0.5);
    until VectorNorm(v) <= fsq;
    v.X := v.X * p.X;
    v.Y := v.Y * p.Y;
    v.Z := v.Z * p.Z;
  end;
end;

// ------------------
// ------------------ TVXParticle ------------------
// ------------------

constructor TVXParticle.Create;
begin
  FEffectScale := 1;
  inherited Create;
end;

destructor TVXParticle.Destroy;
begin
  inherited Destroy;
end;

function TVXParticle.GetPosition(const Index: Integer): Single;
begin
  Result := FPosition.V[Index];
end;

procedure TVXParticle.WritePosition(const Index: Integer; const aValue: Single);
begin
  if (aValue <> FPosition.V[Index]) then
    FPosition.V[Index] := aValue;
end;

function TVXParticle.GetVelocity(const Index: Integer): Single;
begin
  Result := FVelocity.X;
end;

procedure TVXParticle.WriteVelocity(const Index: Integer; const aValue: Single);
begin
  if (aValue <> FVelocity.V[Index]) then
    FVelocity.V[Index] := aValue;
end;

procedure TVXParticle.WriteToFiler(writer: TVirtualWriter);
begin
  inherited WriteToFiler(writer);
  with writer do
  begin
    WriteInteger(0); // Archive Version 0
    WriteInteger(FID);
    Write(FPosition, SizeOf(FPosition));
    Write(FVelocity, SizeOf(FVelocity));
    WriteFloat(FCreationTime);
  end;
end;


procedure TVXParticle.ReadFromFiler(reader: TVirtualReader);
var
  archiveVersion: integer;
begin
  inherited ReadFromFiler(reader);
  archiveVersion := reader.ReadInteger;
  if archiveVersion = 0 then
    with reader do
    begin
      FID := ReadInteger;
      Read(FPosition, SizeOf(FPosition));
      Read(FVelocity, SizeOf(FVelocity));
      FCreationTime := ReadFloat;
    end
  else
    RaiseFilerException(archiveVersion);
end;

// ------------------
// ------------------ TVXParticleList ------------------
// ------------------

constructor TVXParticleList.Create;
begin
  inherited Create;
  FItemList := TPersistentObjectList.Create;
  FitemList.GrowthDelta := 64;
  FDirectList := nil;
end;

destructor TVXParticleList.Destroy;
begin
  FItemList.CleanFree;
  inherited Destroy;
end;

procedure TVXParticleList.WriteToFiler(writer: TVirtualWriter);
begin
  inherited WriteToFiler(writer);
  with writer do
  begin
    WriteInteger(0); // Archive Version 0
    FItemList.WriteToFiler(writer);
  end;
end;

procedure TVXParticleList.ReadFromFiler(reader: TVirtualReader);
var
  archiveVersion: integer;
begin
  inherited ReadFromFiler(reader);
  archiveVersion := reader.ReadInteger;
  if archiveVersion = 0 then
    with reader do
    begin
      FItemList.ReadFromFilerWithEvent(reader, AfterItemCreated);
      FDirectList := PGLParticleArray(FItemList.List);
    end
  else
    RaiseFilerException(archiveVersion);
end;

function TVXParticleList.GetItems(index: Integer): TVXParticle;
begin
  Result := TVXParticle(FItemList[index]);
end;

procedure TVXParticleList.SetItems(index: Integer; val: TVXParticle);
begin
  FItemList[index] := val;
end;

procedure TVXParticleList.AfterItemCreated(Sender: TObject);
begin
  (Sender as TVXParticle).Manager := Self.Owner;
end;

function TVXParticleList.ItemCount: Integer;
begin
  Result := FItemList.Count;
end;

function TVXParticleList.AddItem(aItem: TVXParticle): Integer;
begin
  aItem.Manager := Self.Owner;
  Result := FItemList.Add(aItem);
  FDirectList := PGLParticleArray(FItemList.List);
end;

procedure TVXParticleList.RemoveAndFreeItem(aItem: TVXParticle);
var
  i: Integer;
begin
  i := FItemList.IndexOf(aItem);
  if i >= 0 then
  begin
    if aItem.Manager = Self.Owner then
      aItem.Manager := nil;
    aItem.Free;
    FItemList.List^[i] := nil;
  end;
end;

function TVXParticleList.IndexOfItem(aItem: TVXParticle): Integer;
begin
  Result := FItemList.IndexOf(aItem);
end;

procedure TVXParticleList.Pack;
begin
  FItemList.Pack;
  FDirectList := PGLParticleArray(FItemList.List);
end;

// ------------------
// ------------------ TVXParticleFXManager ------------------
// ------------------

constructor TVXParticleFXManager.Create(aOwner: TComponent);
begin
  inherited;
  FUsers := TList.create;
  FParticles := TVXParticleList.Create;
  FParticles.Owner := Self;
  FBlendingMode := bmAdditive;
  RegisterManager(Self);
end;

destructor TVXParticleFXManager.Destroy;
var
  i: integer;
begin
  inherited Destroy;
  for i := FUsers.Count - 1 downto 0 do
    TVXParticleFXEffect(FUsers[i]).managerNotification(self, opRemove);
  DeRegisterManager(Self);
  Renderer := nil;
  FParticles.Free;
  FUsers.Free;
end;

procedure TVXParticleFXManager.NotifyChange(Sender: TObject);
begin
  if Assigned(FRenderer) then
    Renderer.StructureChanged;
end;

procedure TVXParticleFXManager.DoProgress(const progressTime: TVXProgressTimes);
begin
  inherited;
  if FAutoFreeWhenEmpty and (FParticles.ItemCount = 0) then
    Free;
end;

class function TVXParticleFXManager.ParticlesClass: TVXParticleClass;
begin
  Result := TVXParticle;
end;

function TVXParticleFXManager.CreateParticle: TVXParticle;
begin
  Result := ParticlesClass.Create;
  Result.FID := FNextID;
  if Assigned(cadencer) then
    Result.FCreationTime := Cadencer.CurrentTime;
  Inc(FNextID);
  FParticles.AddItem(Result);
  if Assigned(FOnCreateParticle) then
    FOnCreateParticle(Self, Result);
end;

procedure TVXParticleFXManager.CreateParticles(nbParticles: Integer);
var
  i: Integer;
begin
  FParticles.FItemList.RequiredCapacity(FParticles.ItemCount + nbParticles);
  for i := 1 to nbParticles do
    CreateParticle;
end;

procedure TVXParticleFXManager.SetRenderer(const val: TVXParticleFXRenderer);
begin
  if FRenderer <> val then
  begin
    if Assigned(FRenderer) then
      FRenderer.UnRegisterManager(Self);
    FRenderer := val;
    if Assigned(FRenderer) then
      FRenderer.RegisterManager(Self);
  end;
end;

procedure TVXParticleFXManager.SetParticles(const aParticles: TVXParticleList);
begin
  FParticles.Assign(aParticles);
end;

function TVXParticleFXManager.ParticleCount: Integer;
begin
  Result := FParticles.FItemList.Count;
end;

procedure TVXParticleFXManager.ApplyBlendingMode;
begin
  if Renderer.BlendingMode <> BlendingMode then
  begin
    // case disjunction to minimize OpenVX State changes
    if Renderer.BlendingMode in [bmAdditive, bmTransparency] then
    begin
      case BlendingMode of
        bmAdditive:
          rci.VXStates.SetBlendFunc(bfSrcAlpha, bfOne);
        bmTransparency:
          rci.VXStates.SetBlendFunc(bfSrcAlpha, bfOneMinusSrcAlpha);
      else // bmOpaque
        rci.VXStates.Disable(stBlend);
      end;
    end
    else
    begin
      case BlendingMode of
        bmAdditive:
          begin
            rci.VXStates.SetBlendFunc(bfSrcAlpha, bfOne);
            rci.VXStates.Enable(stBlend);
          end;
        bmTransparency:
          begin
            rci.VXStates.SetBlendFunc(bfSrcAlpha, bfOneMinusSrcAlpha);
            rci.VXStates.Enable(stBlend);
          end;
      else
        // bmOpaque, do nothing
      end;
    end;
  end;
end;

procedure TVXParticleFXManager.UnapplyBlendingMode;
begin
  if Renderer.BlendingMode <> BlendingMode then
  begin
    // case disjunction to minimize OpenVX State changes
    if BlendingMode in [bmAdditive, bmTransparency] then
    begin
      case Renderer.BlendingMode of
        bmAdditive:
          rci.VXStates.SetBlendFunc(bfSrcAlpha, bfOne);
        bmTransparency:
          rci.VXStates.SetBlendFunc(bfSrcAlpha, bfOneMinusSrcAlpha);
      else // bmOpaque
        rci.VXStates.Disable(stBlend);
      end;
    end
    else
    begin
      case Renderer.BlendingMode of
        bmAdditive:
          begin
            rci.VXStates.SetBlendFunc(bfSrcAlpha, bfOne);
            rci.VXStates.Enable(stBlend);
          end;
        bmTransparency:
          begin
            rci.VXStates.SetBlendFunc(bfSrcAlpha, bfOneMinusSrcAlpha);
            rci.VXStates.Enable(stBlend);
          end;
      else
        // bmOpaque, do nothing
      end;
    end;
  end;
end;

procedure TVXParticleFXManager.RegisterUser(obj: TVXParticleFXEffect);
begin
  if FUsers.IndexOf(obj) = -1 then
    FUsers.Add(obj);
end;

procedure TVXParticleFXManager.UnregisterUser(obj: TVXParticleFXEffect);
begin
  FUsers.Remove(obj);
end;

// ------------------
// ------------------ TVXParticleFXEffect ------------------
// ------------------

constructor TVXParticleFXEffect.Create(aOwner: TXCollection);
begin
  FEffectScale := 1;
  inherited;
end;

destructor TVXParticleFXEffect.Destroy;
begin
  Manager := nil;
  inherited Destroy;
end;

procedure TVXParticleFXEffect.WriteToFiler(writer: TWriter);
var
  st: string;
begin
  with writer do
  begin
    // ArchiveVersion 1, added EffectScale
    // ArchiveVersion 2, added inherited call
    WriteInteger(2);
    inherited;
    if Manager <> nil then
      st := Manager.GetNamePath
    else
      st := '';
    WriteString(st);
    WriteFloat(FEffectScale);
  end;
end;

procedure TVXParticleFXEffect.ReadFromFiler(reader: TReader);
var
  archiveVersion: integer;
begin
  with reader do
  begin
    archiveVersion := ReadInteger;
    Assert(archiveVersion in [0..2]);
    if archiveVersion >= 2 then
      inherited;
    if archiveVersion >= 0 then
    begin
      FManagerName := ReadString;
      Manager := nil;
    end;
    if archiveVersion >= 1 then
    begin
      FEffectScale := ReadFloat;
    end;
  end;
end;

procedure TVXParticleFXEffect.Loaded;
var
  mng: TComponent;
begin
  inherited;
  if FManagerName <> '' then
  begin
    mng := FindManager(TVXParticleFXManager, FManagerName);
    if Assigned(mng) then
      Manager := TVXParticleFXManager(mng);
    FManagerName := '';
  end;
end;

procedure TVXParticleFXEffect.SetManager(val: TVXParticleFXManager);
begin
  if assigned(FManager) then
    FManager.unregisterUser(self);
  FManager := val;
  if assigned(FManager) then
    FManager.registerUser(self);
end;

procedure TVXParticleFXEffect.SetEffectScale(const Value: single);
begin
  FEffectScale := Value;
end;

procedure TVXParticleFXEffect.managerNotification(
  aManager: TVXParticleFXManager; Operation: TOperation);
begin
  if (Operation = opRemove) and (aManager = manager) then
    manager := nil;
end;

// ------------------
// ------------------ TVXParticleFXRenderer ------------------
// ------------------

constructor TVXParticleFXRenderer.Create(aOwner: TComponent);
begin
  inherited;
  ObjectStyle := ObjectStyle + [osNoVisibilityCulling, osDirectDraw];
  FZTest := True;
  FZCull := True;
  FZSortAccuracy := saHigh;
  FManagerList := TList.Create;
  FBlendingMode := bmAdditive;
end;

destructor TVXParticleFXRenderer.Destroy;
var
  i: Integer;
begin
  for i := 0 to cPFXNbRegions - 1 do
  begin
    FreeMem(FRegions[i].particleRef);
    FreeMem(FRegions[i].particleOrder);
  end;

  UnRegisterAll;
  FManagerList.Free;
  inherited Destroy;
end;

procedure TVXParticleFXRenderer.RegisterManager(aManager: TVXParticleFXManager);
begin
  FManagerList.Add(aManager);
end;

procedure TVXParticleFXRenderer.UnRegisterManager(aManager: TVXParticleFXManager);
begin
  FManagerList.Remove(aManager);
end;

procedure TVXParticleFXRenderer.UnRegisterAll;
begin
  while FManagerList.Count > 0 do
    TVXParticleFXManager(FManagerList[FManagerList.Count - 1]).Renderer := nil;
end;

// (beware, large and complex stuff below... this is the heart of the ParticleFX)
procedure TVXParticleFXRenderer.BuildList(var rci: TVXRenderContextInfo);
 { Quick Explanation of what is below:
   The purpose is to depth-sort a large number (thousandths) of particles and
   render them back to front. The rendering part is not particularly complex,
   it just invokes the various PFX managers involved and request particle
   renderings.
   The sort uses a first-pass region partition (the depth range is split into
   regions, and particles are assigned directly to the region they belong to),
   then each region is sorted with a QuickSort.
   The QuickSort itself is the regular classic variant, but the comparison is
   made on singles as if they were integers, this is allowed by the IEEE format
   in a very efficient manner if all values are superior to 1, which is ensured
   by the distance calculation and a fixed offset of 1.}
var
  dist, distDelta, invRegionSize: Single;
  managerIdx, particleIdx, regionIdx: Integer;

  procedure QuickSortRegion(startIndex, endIndex: Integer; region: PPFXRegion);
  var
    I, J: Integer;
    P: Integer;
    poptr: PPointerArray;
    buf: Pointer;
  begin
    if endIndex - startIndex > 1 then
    begin
      poptr := @region^.particleOrder^[0];
      repeat
        I := startIndex;
        J := endIndex;
        P := PParticleReference(poptr^[(I + J) shr 1])^.distance;
        repeat
          while PParticleReference(poptr^[I])^.distance < P do
            Inc(I);
          while PParticleReference(poptr^[J])^.distance > P do
            Dec(J);
          if I <= J then
          begin
            buf := poptr^[I];
            poptr^[I] := poptr^[J];
            poptr^[J] := buf;
            Inc(I);
            Dec(J);
          end;
        until I > J;
        if startIndex < J then
          QuickSortRegion(startIndex, J, region);
        startIndex := I;
      until I >= endIndex;
    end
    else if endIndex - startIndex > 0 then
    begin
      poptr := @region^.particleOrder^[0];
      if PParticleReference(poptr^[endIndex])^.distance < PParticleReference(poptr^[startIndex])^.distance then
      begin
        buf := poptr^[startIndex];
        poptr^[startIndex] := poptr^[endIndex];
        poptr^[endIndex] := buf;
      end;
    end;
  end;

  procedure DistToRegionIdx; register;
  {$IFOPT O-}
  begin
    regionIdx := Trunc((dist - distDelta) * invRegionSize);
  {$ELSE}
    // !! WARNING !! This may cause incorrect behaviour if optimization is turned
    // off for the project.
  asm
      FLD     dist
      FSUB    distDelta
      FMUL    invRegionSize
      FISTP   regionIdx
   {$ENDIF}
  end;

var
  minDist, maxDist, sortMaxRegion: Integer;
  curManager: TVXParticleFXManager;
  curList: PGLParticleArray;
  curParticle: TVXParticle;
  curRegion: PPFXRegion;
  curParticleOrder: PPointerArray;
  cameraPos, cameraVector: TAffineVector;
  timer: Int64;
  currentTexturingMode: Cardinal;
begin
  if csDesigning in ComponentState then
    Exit;
  timer := StartPrecisionTimer;
  // precalcs
  PSingle(@minDist)^ := rci.rcci.nearClippingDistance + 1;
  if ZMaxDistance <= 0 then
  begin
    PSingle(@maxDist)^ := rci.rcci.farClippingDistance + 1;
    invRegionSize := (cPFXNbRegions - 2) / (rci.rcci.farClippingDistance - rci.rcci.nearClippingDistance);
  end
  else
  begin
    PSingle(@maxDist)^ := rci.rcci.nearClippingDistance + ZMaxDistance + 1;
    invRegionSize := (cPFXNbRegions - 2) / ZMaxDistance;
  end;
  distDelta := rci.rcci.nearClippingDistance + 1 + 0.49999 / invRegionSize;

  SetVector(cameraPos, rci.cameraPosition);
  SetVector(cameraVector, rci.cameraDirection);
  try
    // Collect particles
    // only depth-clipping performed as of now.
    FLastParticleCount := 0;
    for managerIdx := 0 to FManagerList.Count - 1 do
    begin
      curManager := TVXParticleFXManager(FManagerList[managerIdx]);
      curList := curManager.FParticles.List;
      Inc(FLastParticleCount, curManager.ParticleCount);
      for particleIdx := 0 to curManager.ParticleCount - 1 do
      begin
        curParticle := curList^[particleIdx];
        dist := PointProject(curParticle.FPosition, cameraPos, cameraVector) + 1;
        if not FZCull then
        begin
          if PInteger(@dist)^ < minDist then
            PInteger(@dist)^ := minDist;
        end;
        if (PInteger(@dist)^ >= minDist) and (PInteger(@dist)^ <= maxDist) then
        begin
          DistToRegionIdx;
          curRegion := @FRegions[regionIdx];
          // add particle to region
          if curRegion^.count = curRegion^.capacity then
          begin
            Inc(curRegion^.capacity, cPFXGranularity);
            ReallocMem(curRegion^.particleRef, curRegion^.capacity * SizeOf(TParticleReference));
            ReallocMem(curRegion^.particleOrder, curRegion^.capacity * SizeOf(Pointer));
          end;
          with curRegion^.particleRef^[curRegion^.count] do
          begin
            particle := curParticle;
            distance := PInteger(@dist)^;
          end;
          Inc(curRegion^.count);
        end;
      end;
    end;
    // Sort regions
    case ZSortAccuracy of
      saLow: sortMaxRegion := 0;
      saOneTenth: sortMaxRegion := cPFXNbRegions div 10;
      saOneThird: sortMaxRegion := cPFXNbRegions div 3;
      saOneHalf: sortMaxRegion := cPFXNbRegions div 2;
    else
      sortMaxRegion := cPFXNbRegions;
    end;
    for regionIdx := 0 to cPFXNbRegions - 1 do
    begin
      curRegion := @FRegions[regionIdx];
      if curRegion^.count > 1 then
      begin
        // Prepare order table
        with curRegion^ do
          for particleIdx := 0 to count - 1 do
            particleOrder^[particleIdx] := @particleRef[particleIdx];
        // QuickSort
        if (regionIdx < sortMaxRegion) and (FBlendingMode <> bmAdditive) then
          QuickSortRegion(0, curRegion^.count - 1, curRegion);
      end
      else if curRegion^.Count = 1 then
      begin
        // Prepare order table
        curRegion^.particleOrder^[0] := @curRegion^.particleRef[0];
      end;
    end;
    FLastSortTime := StopPrecisionTimer(timer) * 1000;

    rci.PipelineTransformation.Push;
    rci.PipelineTransformation.SetModelMatrix(IdentityHmgMatrix);

    rci.VXStates.Disable(stCullFace);
    rci.VXStates.ActiveTextureEnabled[ttTexture2D] := True;
    currentTexturingMode := 0;
    rci.VXStates.Disable(stLighting);
    rci.VXStates.PolygonMode := pmFill;

    case FBlendingMode of
      bmAdditive:
        begin
          rci.VXStates.SetBlendFunc(bfSrcAlpha, bfOne);
          rci.VXStates.Enable(stBlend);
        end;
      bmTransparency:
        begin
          rci.VXStates.SetBlendFunc(bfSrcAlpha, bfOneMinusSrcAlpha);
          rci.VXStates.Enable(stBlend);
        end;
    else
      // bmOpaque, do nothing
    end;
    rci.VXStates.DepthFunc := cfLEqual;
    if not FZWrite then
    begin
      rci.VXStates.DepthWriteMask := False;
    end;
    if not FZTest then
      rci.VXStates.Disable(stDepthTest);

    try
      // Initialize managers
      for managerIdx := 0 to FManagerList.Count - 1 do
        TVXParticleFXManager(FManagerList.Items[managerIdx]).InitializeRendering(rci);
      // Start Rendering... at last ;)
      try
        curManager := nil;
        for regionIdx := cPFXNbRegions - 1 downto 0 do
        begin
          curRegion := @FRegions[regionIdx];
          if curRegion^.count > 0 then
          begin
            curParticleOrder := @curRegion^.particleOrder^[0];
            for particleIdx := curRegion^.count - 1 downto 0 do
            begin
              curParticle := PParticleReference(curParticleOrder^[particleIdx])^.particle;
              if curParticle.Manager <> curManager then
              begin
                if Assigned(curManager) then
                  curManager.EndParticles(rci);
                curManager := curParticle.Manager;
                if curManager.TexturingMode <> currentTexturingMode then
                begin
                  if currentTexturingMode <> 0 then
                    glDisable(currentTexturingMode);
                  currentTexturingMode := curManager.TexturingMode;
                  if currentTexturingMode <> 0 then
                    glEnable(currentTexturingMode);
                end;
                curManager.BeginParticles(rci);
              end;
              curManager.RenderParticle(rci, curParticle);
            end;
          end;
        end;
        if Assigned(curManager) then
          curManager.EndParticles(rci);
      finally
        // Finalize managers
        for managerIdx := 0 to FManagerList.Count - 1 do
          TVXParticleFXManager(FManagerList.Items[managerIdx]).FinalizeRendering(rci);
      end;
    finally
      rci.PipelineTransformation.Pop;
    end;
    rci.VXStates.ActiveTextureEnabled[ttTexture2D] := False;
    rci.VXStates.DepthWriteMask := True;
  finally
    // cleanup
    for regionIdx := cPFXNbRegions - 1 downto 0 do
      FRegions[regionIdx].count := 0;
  end;
end;

function TVXParticleFXRenderer.StoreZMaxDistance: Boolean;
begin
  Result := (FZMaxDistance <> 0);
end;

// ------------------
// ------------------ TVXSourcePFXEffect ------------------
// ------------------

constructor TVXSourcePFXEffect.Create(aOwner: TXCollection);
begin
  inherited;
  FInitialVelocity := TVXCoordinates.CreateInitialized(Self, NullHmgVector, csVector);
  FInitialPosition := TVXCoordinates.CreateInitialized(Self, NullHmgVector, csPoint);
  FPositionDispersionRange := TVXCoordinates.CreateInitialized(Self, XYZHmgVector, csPoint);
  FVelocityDispersion := 0;
  FPositionDispersion := 0;
  FParticleInterval := 0.1;
  FVelocityMode := svmAbsolute;
  FPositionMode := spmAbsoluteOffset;
  FDispersionMode := sdmFast;
  FEnabled := true;
  FDisabledIfOwnerInvisible := False;
end;

destructor TVXSourcePFXEffect.Destroy;
begin
  FPositionDispersionRange.Free;
  FInitialVelocity.Free;
  FInitialPosition.Free;
  inherited Destroy;
end;

class function TVXSourcePFXEffect.FriendlyName: string;
begin
  Result := 'PFX Source';
end;

class function TVXSourcePFXEffect.FriendlyDescription: string;
begin
  Result := 'Simple Particles FX Source';
end;

procedure TVXSourcePFXEffect.WriteToFiler(writer: TWriter);
begin
  inherited;
  with writer do
  begin
    WriteInteger(6); // ArchiveVersion 6, added FPositionMode
    // ArchiveVersion 5, added FDisabledIfOwnerInvisible:
    // ArchiveVersion 4, added FRotationDispersion
    // ArchiveVersion 3, added FEnabled
    // ArchiveVersion 2, added FPositionDispersionRange
    // ArchiveVersion 1, added FDispersionMode
    FInitialVelocity.WriteToFiler(writer);
    FInitialPosition.WriteToFiler(writer);
    FPositionDispersionRange.WriteToFiler(writer);
    WriteFloat(FVelocityDispersion);
    WriteFloat(FPositionDispersion);
    WriteFloat(FParticleInterval);
    WriteInteger(Integer(FVelocityMode));
    WriteInteger(Integer(FDispersionMode));
    WriteBoolean(FEnabled);
    WriteFloat(FRotationDispersion);
    WriteBoolean(FDisabledIfOwnerInvisible);
    WriteInteger(Integer(FPositionMode));
  end;
end;

procedure TVXSourcePFXEffect.ReadFromFiler(reader: TReader);
var
  archiveVersion: Integer;
begin
  inherited;
  with reader do
  begin
    archiveVersion := ReadInteger;
    Assert(archiveVersion in [0..6]);
    FInitialVelocity.ReadFromFiler(reader);
    FInitialPosition.ReadFromFiler(reader);
    if archiveVersion >= 2 then
      FPositionDispersionRange.ReadFromFiler(reader);
    FVelocityDispersion := ReadFloat;
    FPositionDispersion := ReadFloat;
    FParticleInterval := ReadFloat;
    FVelocityMode := TVXSourcePFXVelocityMode(ReadInteger);
    if archiveVersion >= 1 then
      FDispersionMode := TVXSourcePFXDispersionMode(ReadInteger);
    if archiveVersion >= 3 then
      FEnabled := ReadBoolean;
    if archiveVersion >= 4 then
      FRotationDispersion := ReadFloat;
    if archiveVersion >= 5 then
      FDisabledIfOwnerInvisible := ReadBoolean;
    if archiveVersion >= 6 then
      FPositionMode := TVXSourcePFXPositionMode(ReadInteger);
  end;
end;

procedure TVXSourcePFXEffect.SetInitialVelocity(const val: TVXCoordinates);
begin
  FInitialVelocity.Assign(val);
end;

procedure TVXSourcePFXEffect.SetInitialPosition(const val: TVXCoordinates);
begin
  FInitialPosition.Assign(val);
end;

procedure TVXSourcePFXEffect.SetPositionDispersionRange(const val: TVXCoordinates);
begin
  FPositionDispersionRange.Assign(val);
end;

procedure TVXSourcePFXEffect.SetParticleInterval(const val: Single);
begin
  if FParticleInterval <> val then
  begin
    FParticleInterval := val;
    if FParticleInterval < 0 then
      FParticleInterval := 0;
    if FTimeRemainder > FParticleInterval then
      FTimeRemainder := FParticleInterval;
  end;
end;

procedure TVXSourcePFXEffect.DoProgress(const progressTime: TVXProgressTimes);
var
  n: Integer;
begin
  if Enabled and Assigned(Manager) and (ParticleInterval > 0) then
  begin
    if OwnerBaseSceneObject.Visible or (not DisabledIfOwnerInvisible) then
    begin
      FTimeRemainder := FTimeRemainder + progressTime.deltaTime;
      if FTimeRemainder > FParticleInterval then
      begin
        n := Trunc((FTimeRemainder - FParticleInterval) / FParticleInterval);
        Burst(progressTime.newTime, n);
        FTimeRemainder := FTimeRemainder - n * FParticleInterval;
      end;
    end;
  end;
end;

function TVXSourcePFXEffect.ParticleAbsoluteInitialPos: TAffineVector;
begin
  if PositionMode = spmRelative then
  begin
    Result := OwnerBaseSceneObject.LocalToAbsolute(InitialPosition.AsAffineVector);
  end
  else
  begin
    SetVector(Result, OwnerBaseSceneObject.AbsolutePosition);
    AddVector(Result, InitialPosition.AsAffineVector);
  end;
end;

procedure TVXSourcePFXEffect.Burst(time: Double; nb: Integer);

var
  particle: TVXParticle;
  av, pos: TAffineVector;
  OwnerObjRelPos: TAffineVector;
begin
  if Manager = nil then
    Exit;

  OwnerObjRelPos := OwnerBaseSceneObject.LocalToAbsolute(NullVector);
  pos := ParticleAbsoluteInitialPos;

  //   if FManager is TVXDynamicPFXManager then
  //     TVXDynamicPFXManager(FManager).FRotationCenter := pos;

  while nb > 0 do
  begin
    particle := Manager.CreateParticle;
    particle.FEffectScale := FEffectScale;
    RndVector(DispersionMode, av, FPositionDispersion, FPositionDispersionRange);
    if VelocityMode = svmRelative then
      av := VectorSubtract(OwnerBaseSceneObject.LocalToAbsolute(av), OwnerObjRelPos);

    ScaleVector(av, FEffectScale);
    VectorAdd(pos, av, @particle.Position);

    RndVector(DispersionMode, av, FVelocityDispersion, nil);
    VectorAdd(InitialVelocity.AsAffineVector, av, @particle.Velocity);

    particle.Velocity := VectorScale(particle.Velocity, FEffectScale);
    if VelocityMode = svmRelative then
      particle.FVelocity := VectorSubtract(OwnerBaseSceneObject.LocalToAbsolute(particle.FVelocity), OwnerObjRelPos);

    particle.CreationTime := time;
    if FRotationDispersion <> 0 then
      particle.FRotation := Random * FRotationDispersion
    else
      particle.FRotation := 0;
    Dec(nb);
  end;
end;

procedure TVXSourcePFXEffect.RingExplosion(time: Double;
  minInitialSpeed, maxInitialSpeed: Single;
  nbParticles: Integer);
var
  particle: TVXParticle;
  av, pos, tmp: TAffineVector;
  ringVectorX, ringVectorY: TAffineVector;
  fx, fy, d: Single;
begin
  if (Manager = nil) or (nbParticles <= 0) then
    Exit;
  pos := ParticleAbsoluteInitialPos;
  SetVector(ringVectorY, OwnerBaseSceneObject.AbsoluteUp);
  SetVector(ringVectorX, OwnerBaseSceneObject.AbsoluteDirection);
  ringVectorY := VectorCrossProduct(ringVectorX, ringVectorY);
  while (nbParticles > 0) do
  begin
    // okay, ain't exactly an "isotropic" ring...
    fx := Random - 0.5;
    fy := Random - 0.5;
    d := RLength(fx, fy);
    tmp := VectorCombine(ringVectorX, ringVectorY, fx * d, fy * d);
    ScaleVector(tmp, minInitialSpeed + Random * (maxInitialSpeed - minInitialSpeed));
    AddVector(tmp, InitialVelocity.AsVector);
    particle := Manager.CreateParticle;
    with particle do
    begin
      RndVector(DispersionMode, av, FPositionDispersion, FPositionDispersionRange);
      VectorAdd(pos, av, @Position);
      RndVector(DispersionMode, av, FVelocityDispersion, nil);
      VectorAdd(tmp, av, @Velocity);
      if VelocityMode = svmRelative then
        Velocity := OwnerBaseSceneObject.LocalToAbsolute(Velocity);
      particle.CreationTime := time;
    end;
    Dec(nbParticles);
  end;
end;

// ------------------
// ------------------ TPFXLifeColor ------------------
// ------------------

constructor TPFXLifeColor.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FColorInner := TVXColor.CreateInitialized(Self, NullHmgVector);
  FColorOuter := TVXColor.CreateInitialized(Self, NullHmgVector);
  FLifeTime := 1;
  FInvLifeTime := 1;
  FSizeScale := 1;
  FRotateAngle := 0;
end;

destructor TPFXLifeColor.Destroy;
begin
  FColorOuter.Free;
  FColorInner.Free;
  inherited Destroy;
end;

procedure TPFXLifeColor.Assign(Source: TPersistent);
begin
  if Source is TPFXLifeColor then
  begin
    FColorInner.Assign(TPFXLifeColor(Source).ColorInner);
    FColorOuter.Assign(TPFXLifeColor(Source).ColorOuter);
    FLifeTime := TPFXLifeColor(Source).LifeTime;
    FRotateAngle := TPFXLifeColor(Source).RotateAngle;
  end
  else
    inherited;
end;

function TPFXLifeColor.GetDisplayName: string;
begin
  Result := Format('LifeTime %f - Inner [%.2f, %.2f, %.2f, %.2f] - Outer [%.2f, %.2f, %.2f, %.2f]',
    [LifeTime,
    ColorInner.Red, ColorInner.Green, ColorInner.Blue, ColorInner.Alpha,
      ColorOuter.Red, ColorOuter.Green, ColorOuter.Blue, ColorOuter.Alpha]);
end;

procedure TPFXLifeColor.SetColorInner(const val: TVXColor);
begin
  FColorInner.Assign(val);
end;

procedure TPFXLifeColor.SetColorOuter(const val: TVXColor);
begin
  FColorOuter.Assign(val);
end;

procedure TPFXLifeColor.SetLifeTime(const val: Single);
begin
  if FLifeTime <> val then
  begin
    FLifeTime := val;
    if FLifeTime <= 0 then
      FLifeTime := 1e-6;
    FInvLifeTime := 1 / FLifeTime;
  end;
end;

procedure TPFXLifeColor.SetSizeScale(const val: Single);
begin
  if FSizeScale <> val then
  begin
    FSizeScale := val;
    FDoScale := (FSizeScale <> 1);
  end;
end;

procedure TPFXLifeColor.SetRotateAngle(const Value: Single);
begin
  if FRotateAngle <> Value then
  begin
    FRotateAngle := Value;
    FDoRotate := (FRotateAngle <> 0);
  end;
end;

// ------------------
// ------------------ TPFXLifeColors ------------------
// ------------------

constructor TPFXLifeColors.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner, TPFXLifeColor);
end;

procedure TPFXLifeColors.SetItems(index: Integer; const val: TPFXLifeColor);
begin
  inherited Items[index] := val;
end;

function TPFXLifeColors.GetItems(index: Integer): TPFXLifeColor;
begin
  Result := TPFXLifeColor(inherited Items[index]);
end;

function TPFXLifeColors.Add: TPFXLifeColor;
begin
  Result := (inherited Add) as TPFXLifeColor;
end;

function TPFXLifeColors.FindItemID(ID: Integer): TPFXLifeColor;
begin
  Result := (inherited FindItemID(ID)) as TPFXLifeColor;
end;

function TPFXLifeColors.MaxLifeTime: Double;
begin
  if Count > 0 then
    Result := Items[Count - 1].LifeTime
  else
    Result := 1e30;
end;

function TPFXLifeColors.RotationsDefined: Boolean;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
  begin
    if Items[i].RotateAngle <> 0 then
    begin
      Result := True;
      Exit;
    end;
  end;
  Result := False;
end;

function TPFXLifeColors.ScalingDefined: Boolean;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
  begin
    if Items[i].SizeScale <> 1 then
    begin
      Result := True;
      Exit;
    end;
  end;
  Result := False;
end;

procedure TPFXLifeColors.PrepareIntervalRatios;
var
  i: Integer;
begin
  for i := 0 to Count - 2 do
    Items[i].FIntervalRatio := 1 / (Items[i + 1].LifeTime - Items[i].LifeTime);
end;

// ------------------
// ------------------ TVXDynamicPFXManager ------------------
// ------------------

constructor TVXDynamicPFXManager.Create(aOwner: TComponent);
begin
  inherited;
  FAcceleration := TVXCoordinates.CreateInitialized(Self, NullHmgVector, csVector);
  FFriction := 1;
end;

destructor TVXDynamicPFXManager.Destroy;
begin
  FAcceleration.Free;
  inherited Destroy;
end;

procedure TVXDynamicPFXManager.DoProgress(const progressTime: TVXProgressTimes);
var
  i: Integer;
  curParticle: TVXParticle;
  maxAge: Double;
  {pos, pos1, axis,}accelVector: TAffineVector;
  {ff,}dt: Single;
  list: PGLParticleArray;
  doFriction, doPack: Boolean;
  frictionScale: Single;
  //pos4: TVector;
begin
  maxAge := MaxParticleAge;
  accelVector := Acceleration.AsAffineVector;
  dt := progressTime.deltaTime;
  doFriction := (FFriction <> 1);
  if doFriction then
  begin
    frictionScale := Power(FFriction, dt)
  end
  else
    frictionScale := 1;
  FCurrentTime := progressTime.newTime;

  doPack := False;
  list := Particles.List;
  for i := 0 to Particles.ItemCount - 1 do
  begin
    curParticle := list^[i];
    if (progressTime.newTime - curParticle.CreationTime) < maxAge then
    begin
      // particle alive, just update velocity and position
      with curParticle do
      begin
        CombineVector(FPosition, FVelocity, dt);

        // DanB - this doesn't seem to fit here, rotation is already
        // calculated when rendering
        {if (FRotation <> 0) and (Renderer <> nil) then begin
          pos := FPosition;
          pos1 := FPosition;
          ff := 1;
          CombineVector(pos1, FVelocity, ff);

          SetVector(axis, Renderer.Scene.CurrentGLCamera.AbsolutePosition);
          SetVector(axis, VectorSubtract(axis, FRotationCenter));
          NormalizeVector(axis);
          MakeVector(pos4, pos1);
          pos4[0] := pos4[0] - FRotationCenter[0];
          pos4[1] := pos4[1] - FRotationCenter[1];
          pos4[2] := pos4[2] - FRotationCenter[2];
          RotateVector(pos4, axis, FRotation * dt);
          pos4[0] := pos4[0] + FRotationCenter[0];
          pos4[1] := pos4[1] + FRotationCenter[1];
          pos4[2] := pos4[2] + FRotationCenter[2];
          MakeVector(pos1, pos4[0], pos4[1], pos4[2]);

          FVelocity := VectorSubtract(pos1, pos);
          CombineVector(FPosition, FVelocity, dt);
        end;}

        CombineVector(FVelocity, accelVector, dt);
        if doFriction then
          ScaleVector(FVelocity, frictionScale);
      end;
    end
    else
    begin
      // kill particle
      curParticle.Free;
      list^[i] := nil;
      doPack := True;
    end;
  end;
  if doPack then
    Particles.Pack;
end;

procedure TVXDynamicPFXManager.SetAcceleration(const val: TVXCoordinates);
begin
  FAcceleration.Assign(val);
end;

// ------------------
// ------------------ TVXLifeColoredPFXManager ------------------
// ------------------

constructor TVXLifeColoredPFXManager.Create(aOwner: TComponent);
begin
  inherited;
  FLifeColors := TPFXLifeColors.Create(Self);
  FColorInner := TVXColor.CreateInitialized(Self, clrYellow);
  FColorOuter := TVXColor.CreateInitialized(Self, NullHmgVector);
  with FLifeColors.Add do
  begin
    LifeTime := 3;
  end;
  FParticleSize := 1;
end;

destructor TVXLifeColoredPFXManager.Destroy;
begin
  FLifeColors.Free;
  FColorInner.Free;
  FColorOuter.Free;
  inherited Destroy;
end;

procedure TVXLifeColoredPFXManager.SetColorInner(const val: TVXColor);
begin
  FColorInner.Assign(val);
end;

procedure TVXLifeColoredPFXManager.SetColorOuter(const val: TVXColor);
begin
  FColorOuter.Assign(val);
end;

procedure TVXLifeColoredPFXManager.SetLifeColors(const val: TPFXLifeColors);
begin
  FLifeColors.Assign(Self);
end;

procedure TVXLifeColoredPFXManager.InitializeRendering(var rci: TVXRenderContextInfo);
var
  i, n: Integer;
begin
  n := LifeColors.Count;
  FLifeColorsLookup := TList.Create;
  FLifeColorsLookup.Capacity := n;
  for i := 0 to n - 1 do
    FLifeColorsLookup.Add(LifeColors[i]);
  FLifeRotations := LifeColors.RotationsDefined;
  FLifeScaling := LifeColors.ScalingDefined;
  LifeColors.PrepareIntervalRatios;
end;

procedure TVXLifeColoredPFXManager.FinalizeRendering(var rci: TVXRenderContextInfo);
begin
  FLifeColorsLookup.Free;
end;

function TVXLifeColoredPFXManager.MaxParticleAge: Single;
begin
  Result := LifeColors.MaxLifeTime;
end;

procedure TVXLifeColoredPFXManager.ComputeColors(var lifeTime: Single; var inner, outer: TColorVector);
var
  i, k, n: Integer;
  f: Single;
  lck, lck1: TPFXLifeColor;
begin
  with LifeColors do
  begin
    n := Count - 1;
    if n < 0 then
    begin
      inner := ColorInner.Color;
      outer := ColorOuter.Color;
    end
    else
    begin
      if n > 0 then
      begin
        k := -1;
        for i := 0 to n do
          if TPFXLifeColor(FLifeColorsLookup.Items[i]).LifeTime < lifeTime then
            k := i;
        if k < n then
          Inc(k);
      end
      else
        k := 0;
      case k of
        0:
          begin
            lck := TPFXLifeColor(FLifeColorsLookup.Items[k]);
            f := lifeTime * lck.InvLifeTime;
            VectorLerp(ColorInner.Color, lck.ColorInner.Color, f, inner);
            VectorLerp(ColorOuter.Color, lck.ColorOuter.Color, f, outer);
          end;
      else
        lck := TPFXLifeColor(FLifeColorsLookup.Items[k]);
        lck1 := TPFXLifeColor(FLifeColorsLookup.Items[k - 1]);
        f := (lifeTime - lck1.LifeTime) * lck1.InvIntervalRatio;
        VectorLerp(lck1.ColorInner.Color, lck.ColorInner.Color, f, inner);
        VectorLerp(lck1.ColorOuter.Color, lck.ColorOuter.Color, f, outer);
      end;
    end;
  end;
end;

procedure TVXLifeColoredPFXManager.ComputeInnerColor(var lifeTime: Single; var inner: TColorVector);
var
  i, k, n: Integer;
  f: Single;
  lck, lck1: TPFXLifeColor;
  lifeColorsLookupList: PFXPointerList;
begin
  with LifeColors do
  begin
    n := Count - 1;
    if n < 0 then
      inner := ColorInner.Color
    else
    begin
      lifeColorsLookupList := @FLifeColorsLookup.List[0];
      if n > 0 then
      begin
        k := -1;
        for i := 0 to n do
          if TPFXLifeColor(lifeColorsLookupList^[i]).LifeTime < lifeTime then
            k := i;
        if k < n then
          Inc(k);
      end
      else
        k := 0;
      if k = 0 then
      begin
        lck := TPFXLifeColor(lifeColorsLookupList^[k]);
        f := lifeTime * lck.InvLifeTime;
        VectorLerp(ColorInner.Color, lck.ColorInner.Color, f, inner);
      end
      else
      begin
        lck := TPFXLifeColor(lifeColorsLookupList^[k]);
        lck1 := TPFXLifeColor(lifeColorsLookupList^[k - 1]);
        f := (lifeTime - lck1.LifeTime) * lck1.InvIntervalRatio;
        VectorLerp(lck1.ColorInner.Color, lck.ColorInner.Color, f, inner);
      end;
    end;
  end;
end;

procedure TVXLifeColoredPFXManager.ComputeOuterColor(var lifeTime: Single; var outer: TColorVector);
var
  i, k, n: Integer;
  f: Single;
  lck, lck1: TPFXLifeColor;
begin
  with LifeColors do
  begin
    n := Count - 1;
    if n < 0 then
      outer := ColorOuter.Color
    else
    begin
      if n > 0 then
      begin
        k := -1;
        for i := 0 to n do
          if TPFXLifeColor(FLifeColorsLookup.Items[i]).LifeTime < lifeTime then
            k := i;
        if k < n then
          Inc(k);
      end
      else
        k := 0;
      case k of
        0:
          begin
            lck := TPFXLifeColor(FLifeColorsLookup.Items[k]);
            f := lifeTime * lck.InvLifeTime;
            VectorLerp(ColorOuter.Color, lck.ColorOuter.Color, f, outer);
          end;
      else
        lck := TPFXLifeColor(FLifeColorsLookup.Items[k]);
        lck1 := TPFXLifeColor(FLifeColorsLookup.Items[k - 1]);
        f := (lifeTime - lck1.LifeTime) * lck1.InvIntervalRatio;
        VectorLerp(lck1.ColorOuter.Color, lck.ColorOuter.Color, f, outer);
      end;
    end;
  end;
end;

function TVXLifeColoredPFXManager.ComputeSizeScale(var lifeTime: Single; var sizeScale: Single): Boolean;
var
  i, k, n: Integer;
  f: Single;
  lck, lck1: TPFXLifeColor;
begin
  with LifeColors do
  begin
    n := Count - 1;
    if n < 0 then
      Result := False
    else
    begin
      if n > 0 then
      begin
        k := -1;
        for i := 0 to n do
          if TPFXLifeColor(FLifeColorsLookup.Items[i]).LifeTime < lifeTime then
            k := i;
        if k < n then
          Inc(k);
      end
      else
        k := 0;
      case k of
        0:
          begin
            lck := TPFXLifeColor(FLifeColorsLookup.Items[k]);
            Result := lck.FDoScale;
            if Result then
            begin
              f := lifeTime * lck.InvLifeTime;
              sizeScale := Lerp(1, lck.SizeScale, f);
            end;
          end;
      else
        lck := TPFXLifeColor(FLifeColorsLookup.Items[k]);
        lck1 := TPFXLifeColor(FLifeColorsLookup.Items[k - 1]);
        Result := lck.FDoScale or lck1.FDoScale;
        if Result then
        begin
          f := (lifeTime - lck1.LifeTime) * lck1.InvIntervalRatio;
          sizeScale := Lerp(lck1.SizeScale, lck.SizeScale, f);
        end;
      end;
    end;
  end;
end;

function TVXLifeColoredPFXManager.ComputeRotateAngle(var lifeTime: Single; var rotateAngle: Single): Boolean;
var
  i, k, n: Integer;
  f: Single;
  lck, lck1: TPFXLifeColor;
begin
  with LifeColors do
  begin
    n := Count - 1;
    if n < 0 then
      Result := False
    else
    begin
      if n > 0 then
      begin
        k := -1;
        for i := 0 to n do
          if Items[i].LifeTime < lifeTime then
            k := i;
        if k < n then
          Inc(k);
      end
      else
        k := 0;
      case k of
        0:
          begin
            lck := LifeColors[k];
            Result := lck.FDoRotate;
            if Result then
            begin
              f := lifeTime * lck.InvLifeTime;
              rotateAngle := Lerp(1, lck.rotateAngle, f);
            end;
          end;
      else
        lck := LifeColors[k];
        lck1 := LifeColors[k - 1];
        Result := lck.FDoRotate or lck1.FDoRotate;
        if Result then
        begin
          f := (lifeTime - lck1.LifeTime) * lck1.InvIntervalRatio;
          rotateAngle := Lerp(lck1.rotateAngle, lck.rotateAngle, f);
        end;
      end;
    end;
  end;
end;

procedure TVXLifeColoredPFXManager.RotateVertexBuf(buf: TAffineVectorList;
  lifeTime: Single; const axis: TAffineVector; offsetAngle: Single);
var
  rotateAngle: Single;
  rotMatrix: TMatrix;
  diff: Single;
  lifeRotationApplied: Boolean;
begin
  rotateAngle := 0;
  lifeRotationApplied := ComputeRotateAngle(lifeTime, rotateAngle);
  rotateAngle := rotateAngle + offsetAngle;
  if lifeRotationApplied or (rotateAngle <> 0) then
  begin
    diff := DegToRad(rotateAngle);
    rotMatrix := CreateRotationMatrix(axis, diff);
    buf.TransformAsVectors(rotMatrix);
  end;
end;

// ------------------
// ------------------ TVXCustomPFXManager ------------------
// ------------------

procedure TVXCustomPFXManager.DoProgress(const progressTime: TVXProgressTimes);
var
  i: Integer;
  list: PGLParticleArray;
  curParticle: TVXParticle;
  defaultProgress, killParticle, doPack: Boolean;
begin
  if Assigned(FOnProgress) then
  begin
    defaultProgress := False;
    FOnProgress(Self, progressTime, defaultProgress);
    if defaultProgress then
      inherited;
  end
  else
    inherited;
  if Assigned(FOnParticleProgress) then
  begin
    doPack := False;
    list := Particles.List;
    for i := 0 to Particles.ItemCount - 1 do
    begin
      killParticle := True;
      curParticle := list^[i];
      FOnParticleProgress(Self, progressTime, curParticle, killParticle);
      if killParticle then
      begin
        curParticle.Free;
        list^[i] := nil;
        doPack := True;
      end;
    end;
    if doPack then
      Particles.Pack;
  end;
end;

function TVXCustomPFXManager.TexturingMode: Cardinal;
begin
  Result := 0;
end;

procedure TVXCustomPFXManager.InitializeRendering(var rci: TVXRenderContextInfo);
begin
  inherited;
  if Assigned(FOnInitializeRendering) then
    FOnInitializeRendering(Self, rci);
end;

procedure TVXCustomPFXManager.BeginParticles(var rci: TVXRenderContextInfo);
begin
  if Assigned(FOnBeginParticles) then
    FOnBeginParticles(Self, rci);
end;

procedure TVXCustomPFXManager.RenderParticle(var rci: TVXRenderContextInfo; aParticle: TVXParticle);
begin
  if Assigned(FOnRenderParticle) then
    FOnRenderParticle(Self, aParticle, rci);
end;

procedure TVXCustomPFXManager.EndParticles(var rci: TVXRenderContextInfo);
begin
  if Assigned(FOnEndParticles) then
    FOnEndParticles(Self, rci);
end;

procedure TVXCustomPFXManager.FinalizeRendering(var rci: TVXRenderContextInfo);
begin
  if Assigned(FOnFinalizeRendering) then
    FOnFinalizeRendering(Self, rci);
  inherited;
end;

function TVXCustomPFXManager.ParticleCount: Integer;
begin
  if Assigned(FOnGetParticleCountEvent) then
    Result := FOnGetParticleCountEvent(Self)
  else
    Result := FParticles.FItemList.Count;
end;

// ------------------
// ------------------ TVXPolygonPFXManager ------------------
// ------------------

constructor TVXPolygonPFXManager.Create(aOwner: TComponent);
begin
  inherited;
  FNbSides := 6;
end;

destructor TVXPolygonPFXManager.Destroy;
begin
  inherited Destroy;
end;

procedure TVXPolygonPFXManager.SetNbSides(const val: Integer);
begin
  if val <> FNbSides then
  begin
    FNbSides := val;
    if FNbSides < 3 then
      FNbSides := 3;
    NotifyChange(Self);
  end;
end;

function TVXPolygonPFXManager.TexturingMode: Cardinal;
begin
  Result := 0;
end;

procedure TVXPolygonPFXManager.InitializeRendering(var rci: TVXRenderContextInfo);
var
  i: Integer;
  matrix: TMatrix;
  s, c: Single;
begin
  inherited;
  glGetFloatv(GL_MODELVIEW_MATRIX, @matrix);
  for i := 0 to 2 do
  begin
    Fvx.V[i] := matrix.V[i].X * FParticleSize;
    Fvy.V[i] := matrix.V[i].Y * FParticleSize;
  end;
  FVertices := TAffineVectorList.Create;
  FVertices.Capacity := FNbSides;
  for i := 0 to FNbSides - 1 do
  begin
    SinCos(i * c2PI / FNbSides, s, c);
    FVertices.Add(VectorCombine(FVx, Fvy, c, s));
  end;
  FVertBuf := TAffineVectorList.Create;
  FVertBuf.Count := FVertices.Count;
end;

procedure TVXPolygonPFXManager.BeginParticles(var rci: TVXRenderContextInfo);
begin
  ApplyBlendingMode(rci);
end;

procedure TVXPolygonPFXManager.RenderParticle(var rci: TVXRenderContextInfo; aParticle: TVXParticle);
var
  i: Integer;
  lifeTime, sizeScale: Single;
  inner, outer: TColorVector;
  pos: TAffineVector;
  vertexList: PAffineVectorArray;
begin
  lifeTime := FCurrentTime - aParticle.CreationTime;
  ComputeColors(lifeTime, inner, outer);

  pos := aParticle.Position;

  vertexList := FVertBuf.List;

  // copy vertices
  for I := 0 to FVertBuf.Count - 1 do
    vertexList[i] := FVertices[i];

  // rotate vertices (if needed)
  if FLifeRotations or (aParticle.FRotation <> 0) then
    RotateVertexBuf(FVertBuf, lifeTime, AffineVectorMake(rci.cameraDirection), aParticle.FRotation);

  // scale vertices (if needed) then translate to particle position
  if FLifeScaling or (aParticle.FEffectScale <> 1) then
  begin
    if FLifeScaling and ComputeSizeScale(lifeTime, sizeScale) then
      sizeScale := sizeScale * aParticle.FEffectScale
    else
      sizeScale := aParticle.FEffectScale;

    for i := 0 to FVertBuf.Count - 1 do
      vertexList^[i] := VectorCombine(vertexList^[i], pos, sizeScale, 1);
  end
  else
    FVertBuf.Translate(pos);

  glBegin(GL_TRIANGLE_FAN);
  glColor4fv(@inner);
  glVertex3fv(@pos);
  glColor4fv(@outer);
  for i := 0 to FVertBuf.Count - 1 do
    glVertex3fv(@vertexList[i]);

  glVertex3fv(@vertexList[0]);
  glEnd;
end;

procedure TVXPolygonPFXManager.EndParticles(var rci: TVXRenderContextInfo);
begin
  UnapplyBlendingMode(rci);
end;

procedure TVXPolygonPFXManager.FinalizeRendering(var rci: TVXRenderContextInfo);
begin
  FVertBuf.Free;
  FVertices.Free;
  inherited;
end;

// ------------------
// ------------------ TVXBaseSpritePFXManager ------------------
// ------------------

constructor TVXBaseSpritePFXManager.Create(aOwner: TComponent);
begin
  inherited;
  FTexHandle := TVXTextureHandle.Create;
  FSpritesPerTexture := sptOne;
  FAspectRatio := 1;
end;

destructor TVXBaseSpritePFXManager.Destroy;
begin
  FTexHandle.Free;
  FShareSprites := nil;
  inherited Destroy;
end;

procedure TVXBaseSpritePFXManager.SetSpritesPerTexture(const val: TSpritesPerTexture);
begin
  if val <> FSpritesPerTexture then
  begin
    FSpritesPerTexture := val;
    FTexHandle.DestroyHandle;
    NotifyChange(Self);
  end;
end;

procedure TVXBaseSpritePFXManager.SetColorMode(const val: TSpriteColorMode);
begin
  if val <> FColorMode then
  begin
    FColorMode := val;
    NotifyChange(Self);
  end;
end;

procedure TVXBaseSpritePFXManager.SetAspectRatio(const val: Single);
begin
  if FAspectRatio <> val then
  begin
    FAspectRatio := ClampValue(val, 1e-3, 1e3);
    NotifyChange(Self);
  end;
end;

function TVXBaseSpritePFXManager.StoreAspectRatio: Boolean;
begin
  Result := (FAspectRatio <> 1);
end;

procedure TVXBaseSpritePFXManager.SetRotation(const val: Single);
begin
  if FRotation <> val then
  begin
    FRotation := val;
    NotifyChange(Self);
  end;
end;

procedure TVXBaseSpritePFXManager.SetShareSprites(const val: TVXBaseSpritePFXManager);
begin
  if FShareSprites <> val then
  begin
    if Assigned(FShareSprites) then
      FShareSprites.RemoveFreeNotification(Self);
    FShareSprites := val;
    if Assigned(FShareSprites) then
      FShareSprites.FreeNotification(Self);
  end;
end;

procedure TVXBaseSpritePFXManager.BindTexture(var rci: TVXRenderContextInfo);
var
  bmp32: TVXBitmap32;
  tw, th, td, tf: Integer;
begin
  if Assigned(FShareSprites) then
    FShareSprites.BindTexture(rci)
  else
  begin
    if FTexHandle.Handle = 0 then
    begin
      FTexHandle.AllocateHandle;
      FTexHandle.Target := ttTexture2D;
      rci.VXStates.TextureBinding[0, ttTexture2D] := FTexHandle.Handle;
      glHint(GL_PERSPECTIVE_CORRECTION_HINT, GL_NICEST);
      rci.VXStates.UnpackAlignment := 4;
      rci.VXStates.UnpackRowLength := 0;
      rci.VXStates.UnpackSkipRows := 0;
      rci.VXStates.UnpackSkipPixels := 0;

      glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR_MIPMAP_LINEAR);
      glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);

      bmp32 := TVXBitmap32.Create;
      try
        tf := GL_RGBA;
        PrepareImage(bmp32, tf);
        bmp32.RegisterAsOpenVXTexture(
          FTexHandle,
          True,
          tf, tw, th, td);
      finally
        bmp32.Free;
      end;
    end
    else
    begin

      rci.VXStates.TextureBinding[0, ttTexture2D] := FTexHandle.Handle;
    end;
  end;
end;

function TVXBaseSpritePFXManager.TexturingMode: Cardinal;
begin
  Result := GL_TEXTURE_2D;
end;

procedure TVXBaseSpritePFXManager.InitializeRendering(var rci: TVXRenderContextInfo);
var
  i: Integer;
  matrix: TMatrix;
  s, c, w, h: Single;
begin
  inherited;
  glGetFloatv(GL_MODELVIEW_MATRIX, @matrix);

  w := FParticleSize * Sqrt(FAspectRatio);
  h := Sqr(FParticleSize) / w;

  for i := 0 to 2 do
  begin
    Fvx.V[i] := matrix.V[i].X * w;
    Fvy.V[i] := matrix.V[i].Y * h;
    Fvz.V[i] := matrix.V[i].Z;
  end;

  FVertices := TAffineVectorList.Create;
  for i := 0 to 3 do
  begin
    SinCos(i * cPIdiv2 + cPIdiv4, s, c);
    FVertices.Add(VectorCombine(Fvx, Fvy, c, s));
  end;
  if FRotation <> 0 then
  begin
    matrix := CreateRotationMatrix(Fvz, -FRotation);
    FVertices.TransformAsPoints(matrix);
  end;

  FVertBuf := TAffineVectorList.Create;
  FVertBuf.Count := FVertices.Count;
end;

procedure TVXBaseSpritePFXManager.BeginParticles(var rci: TVXRenderContextInfo);
begin
  BindTexture(rci);
  if ColorMode = scmNone then
    glTexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_REPLACE)
  else
    glTexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_MODULATE);
  ApplyBlendingMode(rci);
  if ColorMode <> scmFade then
    glBegin(GL_QUADS);
end;

procedure TVXBaseSpritePFXManager.RenderParticle(var rci: TVXRenderContextInfo; aParticle: TVXParticle);
type
  TTexCoordsSet = array[0..3] of TTexPoint;
  PTexCoordsSet = ^TTexCoordsSet;
const
  cBaseTexCoordsSet: TTexCoordsSet = ((S: 1; T: 1), (S: 0; T: 1), (S: 0; T: 0), (S: 1; T: 0));
  cTexCoordsSets: array[0..3] of TTexCoordsSet =
   (((S: 1.0; T: 1.0), (S: 0.5; T: 1.0), (S: 0.5; T: 0.5), (S: 1.0; T: 0.5)),
    ((S: 0.5; T: 1.0), (S: 0.0; T: 1.0), (S: 0.0; T: 0.5), (S: 0.5; T: 0.5)),
    ((S: 1.0; T: 0.5), (S: 0.5; T: 0.5), (S: 0.5; T: 0.0), (S: 1.0; T: 0.0)),
    ((S: 0.5; T: 0.5), (S: 0.0; T: 0.5), (S: 0.0; T: 0.0), (S: 0.5; T: 0.0)));
var
  lifeTime, sizeScale: Single;
  inner, outer: TColorVector;
  pos: TAffineVector;
  vertexList: PAffineVectorArray;
  i: Integer;
  tcs: PTexCoordsSet;
  spt: TSpritesPerTexture;

  procedure IssueVertices;
  begin
    glTexCoord2fv(@tcs[0]);
    glVertex3fv(@vertexList[0]);
    glTexCoord2fv(@tcs[1]);
    glVertex3fv(@vertexList[1]);
    glTexCoord2fv(@tcs[2]);
    glVertex3fv(@vertexList[2]);
    glTexCoord2fv(@tcs[3]);
    glVertex3fv(@vertexList[3]);
  end;

begin
  lifeTime := FCurrentTime - aParticle.CreationTime;

  if Assigned(ShareSprites) then
    spt := ShareSprites.SpritesPerTexture
  else
    spt := SpritesPerTexture;
  case spt of
    sptFour: tcs := @cTexCoordsSets[(aParticle.ID and 3)];
  else
    tcs := @cBaseTexCoordsSet;
  end;

  pos := aParticle.Position;
  vertexList := FVertBuf.List;
  sizeScale := 1;

  // copy vertices
  for i := 0 to FVertBuf.Count - 1 do
    vertexList^[i] := FVertices[i];

  // rotate vertices (if needed)
  if FLifeRotations or (aParticle.FRotation <> 0) then
    RotateVertexBuf(FVertBuf, lifeTime, AffineVectorMake(rci.cameraDirection), aParticle.FRotation);

  // scale vertices (if needed) then translate to particle position
  if FLifeScaling or (aParticle.FEffectScale <> 1) then
  begin
    if FLifeScaling and ComputeSizeScale(lifeTime, sizeScale) then
      sizeScale := sizeScale * aParticle.FEffectScale
    else
      sizeScale := aParticle.FEffectScale;

    for i := 0 to FVertBuf.Count - 1 do
      vertexList^[i] := VectorCombine(vertexList^[i], pos, sizeScale, 1);
  end
  else
    FVertBuf.Translate(pos);

  case ColorMode of
    scmFade:
      begin
        ComputeColors(lifeTime, inner, outer);
        glBegin(GL_TRIANGLE_FAN);
        glColor4fv(@inner);
        glTexCoord2f((tcs^[0].S + tcs^[2].S) * 0.5, (tcs^[0].T + tcs^[2].T) * 0.5);
        glVertex3fv(@pos);
        glColor4fv(@outer);
        IssueVertices;
        glTexCoord2fv(@tcs[0]);
        glVertex3fv(@vertexList[0]);
        glEnd;
      end;
    scmInner:
      begin
        ComputeInnerColor(lifeTime, inner);
        glColor4fv(@inner);
        IssueVertices;
      end;
    scmOuter:
      begin
        ComputeOuterColor(lifeTime, outer);
        glColor4fv(@outer);
        IssueVertices;
      end;
    scmNone:
      begin
        IssueVertices;
      end;
  else
    Assert(False);
  end;
end;

procedure TVXBaseSpritePFXManager.EndParticles(var rci: TVXRenderContextInfo);
begin
  if ColorMode <> scmFade then
    glEnd;
  UnApplyBlendingMode(rci);
end;

procedure TVXBaseSpritePFXManager.FinalizeRendering(var rci: TVXRenderContextInfo);
begin
  FVertBuf.Free;
  FVertices.Free;
  inherited;
end;

// ------------------
// ------------------ TVXCustomSpritePFXManager ------------------
// ------------------

constructor TVXCustomSpritePFXManager.Create(aOwner: TComponent);
begin
  inherited;
  FColorMode := scmInner;
  FSpritesPerTexture := sptOne;
end;

destructor TVXCustomSpritePFXManager.Destroy;
begin
  inherited Destroy;
end;

procedure TVXCustomSpritePFXManager.PrepareImage(bmp32: TVXBitmap32; var texFormat: Integer);
begin
  if Assigned(FOnPrepareTextureImage) then
    FOnPrepareTextureImage(Self, bmp32, texFormat);
end;

// ------------------
// ------------------ TVXPointLightPFXManager ------------------
// ------------------

constructor TVXPointLightPFXManager.Create(aOwner: TComponent);
begin
  inherited;
  FTexMapSize := 5;
  FColorMode := scmInner;
end;

destructor TVXPointLightPFXManager.Destroy;
begin
  inherited Destroy;
end;

procedure TVXPointLightPFXManager.SetTexMapSize(const val: Integer);
begin
  if val <> FTexMapSize then
  begin
    FTexMapSize := val;
    if FTexMapSize < 3 then
      FTexMapSize := 3;
    if FTexMapSize > 9 then
      FTexMapSize := 9;
    NotifyChange(Self);
  end;
end;

procedure TVXPointLightPFXManager.PrepareImage(bmp32: TVXBitmap32; var texFormat: Integer);
var
  s: Integer;
  x, y, d, h2: Integer;
  ih2, f, fy: Single;
  scanLine1, scanLine2: PPixel32Array;
begin
  s := (1 shl TexMapSize);
  bmp32.Width := s;
  bmp32.Height := s;
  bmp32.Blank := false;
  texFormat := GL_LUMINANCE_ALPHA;

  h2 := s div 2;
  ih2 := 1 / h2;
  for y := 0 to h2 - 1 do
  begin
    fy := Sqr((y + 0.5 - h2) * ih2);
    scanLine1 := bmp32.ScanLine[y];
    scanLine2 := bmp32.ScanLine[s - 1 - y];
    for x := 0 to h2 - 1 do
    begin
      f := Sqr((x + 0.5 - h2) * ih2) + fy;
      if f < 1 then
      begin
        d := Trunc((1 - Sqrt(f)) * 256);
        d := d + (d shl 8) + (d shl 16) + (d shl 24);
      end
      else
        d := 0;
      PInteger(@scanLine1[x])^ := d;
      PInteger(@scanLine2[x])^ := d;
      PInteger(@scanLine1[s - 1 - x])^ := d;
      PInteger(@scanLine2[s - 1 - x])^ := d;
    end;
  end;
end;

// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------

     // class registrations
  RegisterClasses([TVXParticle, TVXParticleList,
    TVXParticleFXEffect, TVXParticleFXRenderer,
      TVXCustomPFXManager,
      TVXPolygonPFXManager,
      TVXCustomSpritePFXManager,
      TVXPointLightPFXManager]);
  RegisterXCollectionItemClass(TVXSourcePFXEffect);

finalization

  UnregisterXCollectionItemClass(TVXSourcePFXEffect);

end.

