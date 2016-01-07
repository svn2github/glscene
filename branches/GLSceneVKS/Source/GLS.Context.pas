//
// GLScene on Vulkan, http://glscene.sourceforge.net 
//
{
   Prototypes and base implementation of TVKContext. 
      
}
unit GLS.Context;

interface

{$I GLScene.inc}

uses
  {$IFDEF MSWINDOWS}
///  Winapi.Windows,
  {$ENDIF}
  System.Classes, System.SysUtils, System.Types, System.SyncObjs,
  FMX.Consts,
  FMX.Forms,
  FMX.Controls,
  FMX.Types,

{$IFDEF VKS_SERVICE_CONTEXT}
  GLS.Generics,
{$ENDIF}
  GLS.OpenGLTokens,
  GLS.CrossPlatform,
  GLS.OpenGLAdapter,
  GLS.VectorGeometry,
  GLS.Strings,
  GLS.VectorTypes,
  GLS.State,
  GLS.PipelineTransformation,
  GLS.TextureFormat,
  GLS.Log;

// Buffer ID's for Multiple-Render-Targets (using GL_ATI_draw_buffers)
const
  MRT_BUFFERS: array[0..3] of GLenum = (GL_FRONT_LEFT, GL_AUX0, GL_AUX1, GL_AUX2);

type

  // TVKRCOptions
  //
  TVKRCOption = (rcoDoubleBuffered, rcoStereo, rcoDebug, rcoOGL_ES);
  TVKRCOptions = set of TVKRCOption;

  TVKContextLayer = (clUnderlay2, clUnderlay1, clMainPlane, clOverlay1, clOverlay2);

  TFinishTaskEvent = class(TEvent)
  public
    constructor Create; reintroduce;
  end;

  TTaskProcedure = procedure of object; stdcall;
  TServiceContextTask = record
    Task: TTaskProcedure;
    Event: TFinishTaskEvent;
  end;

{$IFDEF VKS_SERVICE_CONTEXT}
  TServiceContextTaskList = {$IFDEF VKS_GENERIC_PREFIX} specialize {$ENDIF}
    GThreadList < TServiceContextTask > ;
{$ENDIF VKS_SERVICE_CONTEXT}

  TVKContext = class;
  TVKContextManager = class;

  TAbstractMultitextureCoordinator = class(TObject)
  protected
    FOwner: TVKContext;
  public
    constructor Create(AOwner: TVKContext); virtual;
  end;

  TAbstractMultitextureCoordinatorClass = class of TAbstractMultitextureCoordinator;

  // TVKContextAcceleration
  //
  TVKContextAcceleration = (chaUnknown, chaHardware, chaSoftware);

  // TVKAntiAliasing
  //
  TVKAntiAliasing = (// Multisample Antialiasing
    aaDefault, aaNone, aa2x, aa2xHQ, aa4x, aa4xHQ,
    aa6x, aa8x, aa16x,
    // Coverage Sampling Antialiasing
    csa8x, csa8xHQ, csa16x, csa16xHQ);

  // TVSyncMode
  //
  TVSyncMode = (vsmSync, vsmNoSync);

  // TVKContext
  //
  { Wrapper around an OpenGL rendering context. 
     The aim of this class is to offer platform-independant
     initialization, activation and management of OpenGL
     rendering context. The class also offers notifications
     event and error/problems detection. 
     This is a virtual abstract a class, and platform-specific
     subclasses must be used. 
     All rendering context share the same lists. }
  TVKContext = class
  private
    { Private Declarations }
    FColorBits, FAlphaBits: Integer;
    FDepthBits: Integer;
    FStencilBits: Integer;
    FAccumBits: Integer;
    FAuxBuffers: Integer;
    FAntiAliasing: TVKAntiAliasing;
    FOptions: TVKRCOptions;
    FOnDestroyContext: TNotifyEvent;
    FManager: TVKContextManager;
    FActivationCount: Integer;
    FOwnedHandlesCount: Integer;
    FIsPraparationNeed: Boolean;
    procedure SetColorBits(const aColorBits: Integer);
    procedure SetAlphaBits(const aAlphaBits: Integer);
    procedure SetDepthBits(const val: Integer);
    procedure SetStencilBits(const aStencilBits: Integer);
    procedure SetAccumBits(const aAccumBits: Integer);
    procedure SetAuxBuffers(const aAuxBuffers: Integer);
    procedure SetOptions(const aOptions: TVKRCOptions);
    procedure SetAntiAliasing(const val: TVKAntiAliasing);
    procedure SetAcceleration(const val: TVKContextAcceleration);
    function GetActive: Boolean;
    procedure SetActive(const aActive: Boolean);
    procedure SetLayer(const Value: TVKContextLayer);
  protected
    { Protected Declarations }
    FGL: TGLExtensionsAndEntryPoints;
    FXGL: TAbstractMultitextureCoordinator;
    FGLStates: TVKStateCache;
    FTransformation: TVKTransformation;
    FAcceleration: TVKContextAcceleration;
    FLayer: TVKContextLayer;
{$IFNDEF VKS_MULTITHREAD}
    FSharedContexts: TList;
{$ELSE}
    FSharedContexts: TThreadList;
    FLock: TCriticalSection;
{$ENDIF}
    procedure PropagateSharedContext;

    procedure DoCreateContext(ADeviceHandle: THandle); virtual; abstract; //VCL -> HDC
    procedure DoCreateMemoryContext(OutputDevice: THandle; Width, Height: //VCL ->HWND
         Integer; BufferCount: integer = 1); virtual; abstract;
    function DoShareLists(aContext: TVKContext): Boolean; virtual; abstract;
    procedure DoDestroyContext; virtual; abstract;
    procedure DoActivate; virtual; abstract;
    procedure DoDeactivate; virtual; abstract;
    class function ServiceContext: TVKContext;
    procedure MakeGLCurrent;
    function GetXGL: TAbstractMultitextureCoordinator;
  public
    { Public Declarations }
    constructor Create; virtual;
    destructor Destroy; override;

    { An application-side cache of global per-context OpenGL states
       and parameters }
    property GLStates: TVKStateCache read FGLStates;

    property PipelineTransformation: TVKTransformation read FTransformation;

    //: Context manager reference
    property Manager: TVKContextManager read FManager;

    { Color bits for the rendering context }
    property ColorBits: Integer read FColorBits write SetColorBits;
    { Alpha bits for the rendering context }
    property AlphaBits: Integer read FAlphaBits write SetAlphaBits;
    { Depth bits for the rendering context }
    property DepthBits: Integer read FDepthBits write SetDepthBits;
    { Stencil bits for the rendering context }
    property StencilBits: Integer read FStencilBits write SetStencilBits;
    { Accumulation buffer bits for the rendering context }
    property AccumBits: Integer read FAccumBits write SetAccumBits;
    { Auxiliary buffers bits for the rendering context }
    property AuxBuffers: Integer read FAuxBuffers write SetAuxBuffers;
    { AntiAliasing option. 
       Ignored if not hardware supported, currently based on ARB_multisample. }
    property AntiAliasing: TVKAntiAliasing read FAntiAliasing write
      SetAntiAliasing;
    { Specifies the layer plane that the rendering context is bound to. }
    property Layer: TVKContextLayer read FLayer write SetLayer;
    { Rendering context options. }
    property Options: TVKRCOptions read FOptions write SetOptions;
    { Allows reading and defining the activity for the context. 
       The methods of this property are just wrappers around calls
       to Activate and Deactivate. }
    property Active: Boolean read GetActive write SetActive;
    { Indicates if the context is hardware-accelerated. }
    property Acceleration: TVKContextAcceleration read FAcceleration write SetAcceleration;
    { Triggered whenever the context is destroyed. 
       This events happens *before* the context has been
       actually destroyed, OpenGL resource cleanup can
       still occur here. }
    property OnDestroyContext: TNotifyEvent read FOnDestroyContext write
      FOnDestroyContext;

    { Creates the context. 
       This method must be invoked before the context can be used. }
    procedure CreateContext(ADeviceHandle: THandle); overload; //VCL -> HDC
    { Creates an in-memory context. 
       The function should fail if no hardware-accelerated memory context
       can be created (the CreateContext method can handle software OpenGL
       contexts). }
    procedure CreateMemoryContext(OutputDevice: THandle; Width, Height: //HWND
      Integer; BufferCount: integer = 1);
    { Setup display list sharing between two rendering contexts. 
       Both contexts must have the same pixel format. }
    procedure ShareLists(AContext: TVKContext);
    { Destroy the context. 
       Will fail if no context has been created. 
       The method will first invoke the OnDestroyContext
       event, then attempts to deactivate the context
       (if it is active) before destroying it. }
    procedure DestroyContext;
    { Activates the context. 
       A context can be activated multiple times (and must be
       deactivated the same number of times), but this function
       will fail if another context is already active. }
    procedure Activate;
    { Deactivates the context. 
       Will fail if the context is not active or another
       context has been activated. }
    procedure Deactivate;
    { Call OnPrepare for all handles.  }
    procedure PrepareHandlesData;
    { Returns true if the context is valid. 
       A context is valid from the time it has been successfully
       created to the time of its destruction. }
    function IsValid: Boolean; virtual; abstract;
    { Request to swap front and back buffers if they were defined. }
    procedure SwapBuffers; virtual; abstract;

    { Returns the first compatible context that isn't self in the shares. }
    function FindCompatibleContext: TVKContext;
    procedure DestroyAllHandles;

    function RenderOutputDevice: Pointer; virtual; abstract;
    { Access to OpenGL command and extension. }
    property GL: TGLExtensionsAndEntryPoints read FGL;
    property MultitextureCoordinator: TAbstractMultitextureCoordinator read GetXGL;
    property IsPraparationNeed: Boolean read FIsPraparationNeed;
  end;

  TVKContextClass = class of TVKContext;

  // TVKScreenControlingContext
  //
  { A TVKContext with screen control property and methods. 
     This variety of contexts is for drivers that access windows and OpenGL
     through an intermediate opaque cross-platform API. 
     TVKSceneViewer won't use them, TVKMemoryViewer may be able to use them,
     but most of the time they will be accessed through a specific viewer
     class/subclass. }
  TVKScreenControlingContext = class(TVKContext)
  private
    { Private Declarations }
    FWidth, FHeight: Integer;
    FFullScreen: Boolean;

  protected
    { Protected Declarations }

  public
    { Public Declarations }
    property Width: Integer read FWidth write FWidth;
    property Height: Integer read FHeight write FHeight;
    property FullScreen: Boolean read FFullScreen write FFullScreen;
  end;

  PGLRCHandle = ^TVKRCHandle;
  TVKRCHandle = record
    FRenderingContext: TVKContext;
    FHandle: TGLuint;
    FChanged: Boolean;
  end;

  TOnPrepareHandleData = procedure(AContext: TVKContext) of object;

  // TVKContextHandle
  //
  { Wrapper around an OpenGL context handle. 
     This wrapper also takes care of context registrations and data releases
     related to context releases an cleanups. This is an abstract class,
     use the TVKListHandle and TVKTextureHandle subclasses. }
  TVKContextHandle = class
  private
    { Private Declarations }
    FHandles: TList;
    FLastHandle : PGLRCHandle;
    FOnPrepare: TOnPrepareHandleData;
    function GetHandle: TGLuint;
    function GetContext: TVKContext;
    function SearchRC(AContext: TVKContext): PGLRCHandle;
    function RCItem(AIndex: integer): PGLRCHandle; {$IFDEF VKS_INLINE}inline;{$ENDIF}
    procedure CheckCurrentRC;
  protected
    { Protected Declarations }
    //: Invoked by when there is no compatible context left for relocation
    procedure ContextDestroying;

    //: Specifies if the handle can be transfered across shared contexts
    class function Transferable: Boolean; virtual;
    class function IsValid(const ID: GLuint): Boolean; virtual;

    function DoAllocateHandle: Cardinal; virtual; abstract;
    procedure DoDestroyHandle(var AHandle: TGLuint); virtual; abstract;

  public
    { Public Declarations }
    constructor Create; virtual;
    constructor CreateAndAllocate(failIfAllocationFailed: Boolean = True);
    destructor Destroy; override;

    { Return OpenGL identifier in current context. }
    property Handle: TGLuint read GetHandle;
    { Return current rendering context if handle is allocated in it
       or first context where handle is allocated. }
    property RenderingContext: TVKContext read GetContext;
    { Return True is data need update in current context. }
    function IsDataNeedUpdate: Boolean;
    { Return True if data updated in all contexts. }
    function IsDataComplitelyUpdated: Boolean;
    { Notify the data was updated in current context. }
    procedure NotifyDataUpdated;
    { Notify the data was changed through all context. }
    procedure NotifyChangesOfData;

    //: Checks if required extensions / OpenGL version are met
    class function IsSupported: Boolean; virtual;
    function IsAllocatedForContext(AContext: TVKContext = nil): Boolean;
    function IsShared: Boolean;

    function  AllocateHandle: TGLuint;
    procedure DestroyHandle;

    property OnPrapare: TOnPrepareHandleData read FOnPrepare write FOnPrepare;
  end;

  TVKVirtualHandle = class;
  TVKVirtualHandleEvent = procedure(Sender: TVKVirtualHandle; var handle:
    TGLuint) of object;

  // TVKVirtualHandle
  //
  { A context handle with event-based handle allocation and destruction. }
  TVKVirtualHandle = class(TVKContextHandle)
  private
    { Private Declarations }
    FOnAllocate, FOnDestroy: TVKVirtualHandleEvent;
    FTag: Integer;
  protected
    { Protected Declarations }
    function DoAllocateHandle: Cardinal; override;
    procedure DoDestroyHandle(var AHandle: TGLuint); override;
    class function Transferable: Boolean; override;
  public
    { Public Declarations }
    property OnAllocate: TVKVirtualHandleEvent read FOnAllocate write
      FOnAllocate;
    property OnDestroy: TVKVirtualHandleEvent read FOnDestroy write FOnDestroy;

    property Tag: Integer read FTag write FTag;
  end;

  // TVKVirtualHandleTransf
  //
  { Transferable virtual handle. }
  TVKVirtualHandleTransf = class(TVKVirtualHandle)
  protected
    class function Transferable: Boolean; override;
  end;

  // TVKListHandle
  //
  { Manages a handle to a display list. }
  TVKListHandle = class(TVKContextHandle)
  private
    { Private Declarations }

  protected
    { Protected Declarations }
    function DoAllocateHandle: Cardinal; override;
    procedure DoDestroyHandle(var AHandle: TGLuint); override;
    class function IsValid(const ID: GLuint): Boolean; override;
  public
    { Public Declarations }
    procedure NewList(mode: Cardinal);
    procedure EndList;
    procedure CallList;
  end;

  // TVKTextureHandle
  //
  { Manages a handle to a texture. }
  TVKTextureHandle = class(TVKContextHandle)
  private
    FTarget: TVKTextureTarget;
    procedure SetTarget(ATarget: TVKTextureTarget);
  protected
    { Protected Declarations }
    function DoAllocateHandle: Cardinal; override;
    procedure DoDestroyHandle(var AHandle: TGLuint); override;
    class function IsValid(const ID: GLuint): Boolean; override;
  public
    { Public Declarations }
    property Target: TVKTextureTarget read FTarget write SetTarget;
  end;

  // TVKSamplerHandle
  //
  { Manages a handle to a sampler. }
  TVKSamplerHandle = class(TVKContextHandle)
  protected
    { Protected Declarations }
    function DoAllocateHandle: Cardinal; override;
    procedure DoDestroyHandle(var AHandle: TGLuint); override;
    class function IsValid(const ID: GLuint): Boolean; override;
  public
    { Public Declarations }
    class function IsSupported: Boolean; override;
  end;

  // TVKQueryHandle
  //
  { Manages a handle to a query. 
     Do not use this class directly, use one of its subclasses instead. }
  TVKQueryHandle = class(TVKContextHandle)
  private
    { Private Declarations }
    FActive: Boolean;
  protected
    { Protected Declarations }
    class function Transferable: Boolean; override;
    function DoAllocateHandle: Cardinal; override;
    procedure DoDestroyHandle(var AHandle: TGLuint); override;
    function GetTarget: TGLuint; virtual; abstract;
    function GetQueryType: TQueryType; virtual; abstract;
    class function IsValid(const ID: GLuint): Boolean; override;
  public
    { Public Declarations }
    procedure BeginQuery;
    procedure EndQuery;

    // Check if result is available from the query.  Result may not be available
    // immediately after ending the query
    function IsResultAvailable: boolean;
    // Number of bits used to store the query result. eg. 32/64 bit
    function CounterBits: integer;
    // Retrieve query result, may cause a stall if the result is not available yet
    function QueryResultInt: TGLInt;
    function QueryResultUInt: TGLUInt;
    function QueryResultInt64: TGLint64EXT;
    function QueryResultUInt64: TGLuint64EXT;
    function QueryResultBool: TGLboolean;

    property Target: TGLuint read GetTarget;
    property QueryType: TQueryType read GetQueryType;

    { True if within a Begin/EndQuery. }
    property Active: Boolean read FActive;
  end;

  // TVKOcclusionQueryHandle
  //
  { Manages a handle to an occlusion query. 
     Requires OpenGL 1.5+ 
     Does *NOT* check for extension availability, this is assumed to have been
     checked by the user. }
  TVKOcclusionQueryHandle = class(TVKQueryHandle)
  protected
    function GetTarget: TGLuint; override;
    function GetQueryType: TQueryType; override;
  public
    class function IsSupported: Boolean; override;
    // Number of samples (pixels) drawn during the query, some pixels may
    // be drawn to several times in the same query
    function PixelCount: Integer;
  end;

  TVKBooleanOcclusionQueryHandle = class(TVKQueryHandle)
  protected
    function GetTarget: TGLuint; override;
    function GetQueryType: TQueryType; override;
  public
    class function IsSupported: Boolean; override;
  end;

  // TVKTimerQueryHandle
  //
  { Manages a handle to a timer query. 
     Requires GL_EXT_timer_query extension. 
     Does *NOT* check for extension availability, this is assumed to have been
     checked by the user. }
  TVKTimerQueryHandle = class(TVKQueryHandle)
  protected
    function GetTarget: TGLuint; override;
    function GetQueryType: TQueryType; override;
  public
    class function IsSupported: Boolean; override;
    // Time, in nanoseconds (1 ns = 10^-9 s) between starting + ending the query.
    // with 32 bit integer can measure up to approximately 4 seconds, use
    // QueryResultUInt64 if you may need longer
    function Time: Integer;
  end;

  // TVKPrimitiveQueryHandle
  //
  { Manages a handle to a primitive query. 
     Requires OpenGL 3.0+ 
     Does *NOT* check for extension availability, this is assumed to have been
     checked by the user. }
  TVKPrimitiveQueryHandle = class(TVKQueryHandle)
  protected
    function GetTarget: TGLuint; override;
    function GetQueryType: TQueryType; override;
  public
    class function IsSupported: Boolean; override;
    // Number of primitives (eg. Points, Triangles etc.) drawn whilst the
    // query was active
    function PrimitivesGenerated: Integer;
  end;

  // TVKBufferObjectHandle
  //
  { Manages a handle to a Buffer Object. 
     Does *NOT* check for extension availability, this is assumed to have been
     checked by the user.  }
  TVKBufferObjectHandle = class(TVKContextHandle)
  private
    { Private Declarations }
    FSize: Integer;
  protected
    { Protected Declarations }
    function DoAllocateHandle: Cardinal; override;
    procedure DoDestroyHandle(var AHandle: TGLuint); override;

    function GetTarget: TGLuint; virtual; abstract;
    class function IsValid(const ID: GLuint): Boolean; override;
  public
    { Public Declarations }
    { Creates the buffer object buffer and initializes it. }
    constructor CreateFromData(p: Pointer; size: Integer; bufferUsage: TGLuint);

    procedure Bind; virtual; abstract;
    { Note that it is not necessary to UnBind before Binding another buffer. }
    procedure UnBind; virtual; abstract;

    { Bind a buffer object to an indexed target, used by transform feedback
       buffer objects and uniform buffer objects. (OpenGL 3.0+) }
    procedure BindRange(index: TGLuint; offset: TGLintptr; size: TGLsizeiptr);
      virtual;
    { Equivalent to calling BindRange with offset = 0, and size = the size of buffer.}
    procedure BindBase(index: TGLuint); virtual;
    procedure UnBindBase(index: TGLuint); virtual;

    { Specifies buffer content. 
       Common bufferUsage values are GL_STATIC_DRAW_ARB for data that will
       change rarely, but be used often, GL_STREAM_DRAW_ARB for data specified
       once but used only a few times, and GL_DYNAMIC_DRAW_ARB for data
       that is re-specified very often. 
       Valid only if the buffer has been bound. }
    procedure BufferData(p: Pointer; size: Integer; bufferUsage: TGLuint);
    //: Invokes Bind then BufferData
    procedure BindBufferData(p: Pointer; size: Integer; bufferUsage: TGLuint);
    { Updates part of an already existing buffer. 
       offset and size indicate which part of the data in the buffer is
       to bo modified and p where the data should be taken from. }
    procedure BufferSubData(offset, size: Integer; p: Pointer);
    { Map buffer content to memory. 
       Values for access are GL_READ_ONLY_ARB, GL_WRITE_ONLY_ARB and
       GL_READ_WRITE_ARB. 
       Valid only if the buffer has been bound, must be followed by
       an UnmapBuffer, only one buffer may be mapped at a time. }
    function MapBuffer(access: TGLuint): Pointer;
    function MapBufferRange(offset: TGLint; len: TGLsizei; access: TGLbitfield):
      Pointer;
    procedure Flush(offset: TGLint; len: TGLsizei);
    { Unmap buffer content from memory. 
       Must follow a MapBuffer, and happen before the buffer is unbound. }
    function UnmapBuffer: Boolean;

    class function IsSupported: Boolean; override;

    property Target: TGLuint read GetTarget;
    property BufferSize: Integer read FSize;
  end;

  // TVKVBOHandle
  //
  { Manages a handle to an Vertex Buffer Object. 
     Does *NOT* check for extension availability, this is assumed to have been
     checked by the user. 
     Do not use this class directly, use one of its subclasses instead. }
  TVKVBOHandle = class(TVKBufferObjectHandle)
  private
    { Private Declarations }

    function GetVBOTarget: TGLuint;
  public

    property VBOTarget: TGLuint read GetVBOTarget;
  end;

  // TVKVBOArrayBufferHandle
  //
  { Manages a handle to VBO Array Buffer. 
     Typically used to store vertices, normals, texcoords, etc. }
  TVKVBOArrayBufferHandle = class(TVKVBOHandle)
  protected
    function GetTarget: TGLuint; override;
  public
    procedure Bind; override;
    procedure UnBind; override;
  end;

  // TVKVBOElementArrayHandle
  //
  { Manages a handle to VBO Element Array Buffer. 
     Typically used to store vertex indices. }
  TVKVBOElementArrayHandle = class(TVKVBOHandle)
  protected
    function GetTarget: TGLuint; override;
  public
    procedure Bind; override;
    procedure UnBind; override;
  end;

  // TVKPackPBOHandle
  //
  { Manages a handle to PBO Pixel Pack Buffer. 
     When bound, commands such as ReadPixels write
     their data into a buffer object. }
  TVKPackPBOHandle = class(TVKBufferObjectHandle)
  protected
    function GetTarget: TGLuint; override;
  public
    procedure Bind; override;
    procedure UnBind; override;
    class function IsSupported: Boolean; override;
  end;

  // TVKUnpackPBOHandle
  //
  { Manages a handle to PBO Pixel Unpack Buffer. 
     When bound, commands such as DrawPixels read
     their data from a buffer object. }
  TVKUnpackPBOHandle = class(TVKBufferObjectHandle)
  protected
    function GetTarget: TGLuint; override;
  public
    procedure Bind; override;
    procedure UnBind; override;
    class function IsSupported: Boolean; override;
  end;

  // TVKTransformFeedbackBufferHandle
  //
  { Manages a handle to a Transform Feedback Buffer Object. 
     Transform feedback buffers can be used to capture vertex data from the
     vertex or geometry shader stage to perform further processing without
     going on to the fragment shader stage. }
  TVKTransformFeedbackBufferHandle = class(TVKBufferObjectHandle)
    //    FTransformFeedbackBufferBuffer: array[0..15] of TGLuint; // (0, 0, 0, ...)
    //    FTransformFeedbackBufferStart: array[0..15] of TGLuint64; // (0, 0, 0, ...)
    //    FTransformFeedbackBufferSize: array[0..15] of TGLuint64; // (0, 0, 0, ...)
  protected
    function GetTarget: TGLuint; override;
  public
    procedure Bind; override;
    procedure UnBind; override;
    procedure BeginTransformFeedback(primitiveMode: TGLenum);
    procedure EndTransformFeedback();
    procedure BindRange(index: TGLuint; offset: TGLintptr; size: TGLsizeiptr); override;
    procedure BindBase(index: TGLuint); override;
    procedure UnBindBase(index: TGLuint); override;

    class function IsSupported: Boolean; override;
  end;

  // TVKTextureBufferHandle
  //
  { Manages a handle to a Buffer Texture. (TBO) }
  TVKTextureBufferHandle = class(TVKBufferObjectHandle)
  protected
    function GetTarget: TGLuint; override;
  public
    procedure Bind; override;
    procedure UnBind; override;
    class function IsSupported: Boolean; override;
  end;

  // TVKUniformBufferHandle
  //
  { Manages a handle to a Uniform Buffer Object (UBO).
     Uniform buffer objects store "uniform blocks"; groups of uniforms
     that can be passed as a group into a GLSL program. }
  TVKUniformBufferHandle = class(TVKBufferObjectHandle)
    //    FUniformBufferBuffer: array[0..15] of TGLuint; // (0, 0, 0, ...)
    //    FUniformBufferStart: array[0..15] of TGLuint64; // (0, 0, 0, ...)
    //    FUniformBufferSize: array[0..15] of TGLuint64; // (0, 0, 0, ...)
  protected
    function GetTarget: TGLuint; override;
  public
    procedure Bind; override;
    procedure UnBind; override;
    procedure BindRange(index: TGLuint; offset: TGLintptr; size: TGLsizeiptr); override;
    procedure BindBase(index: TGLuint); override;
    procedure UnBindBase(index: TGLuint); override;
    class function IsSupported: Boolean; override;
  end;

  // TVKVertexArrayHandle
  //
  { Manages a handle to a Vertex Array Object (VAO).
     Vertex array objects are used to rapidly switch between large sets
     of array state. }
  TVKVertexArrayHandle = class(TVKContextHandle)
  protected
    class function Transferable: Boolean; override;
    function DoAllocateHandle: Cardinal; override;
    procedure DoDestroyHandle(var AHandle: TGLuint); override;
    class function IsValid(const ID: GLuint): Boolean; override;
  public
    procedure Bind;
    procedure UnBind;
    class function IsSupported: Boolean; override;
  end;

  TVKFramebufferStatus = (fsComplete, fsIncompleteAttachment,
    fsIncompleteMissingAttachment,
    fsIncompleteDuplicateAttachment, fsIncompleteDimensions,
    fsIncompleteFormats,
    fsIncompleteDrawBuffer, fsIncompleteReadBuffer, fsUnsupported,
    fsIncompleteMultisample,
    fsStatusError);

  // TVKFramebufferHandle
  //
  { Manages a handle to a Framebuffer Object (FBO).
     Framebuffer objects provide a way of drawing to rendering
     destinations other than the buffers provided to the GL by the
     window-system.  One or more "framebuffer-attachable images" can be attached
     to a Framebuffer for uses such as: offscreen rendering, "render to texture" +
     "multiple render targets" (MRT).
     There are several types of framebuffer-attachable images:
     - The image of a renderbuffer object, which is always 2D.
     - A single level of a 1D texture, which is treated as a 2D image with a height of one.
     - A single level of a 2D or rectangle texture.
     - A single face of a cube map texture level, which is treated as a 2D image.
     - A single layer of a 1D or 2D array texture or 3D texture, which is treated as a 2D image.
     Additionally, an entire level of a 3D texture, cube map texture,
     or 1D or 2D array texture can be attached to an attachment point.
     Such attachments are treated as an array of 2D images, arranged in
     layers, and the corresponding attachment point is considered to be layered. }
  TVKFramebufferHandle = class(TVKContextHandle)
  protected
    class function Transferable: Boolean; override;
    function DoAllocateHandle: Cardinal; override;
    procedure DoDestroyHandle(var AHandle: TGLuint); override;
    class function IsValid(const ID: GLuint): Boolean; override;
  public
    // Bind framebuffer for both drawing + reading
    procedure Bind;
    // Bind framebuffer for drawing
    procedure BindForDrawing;
    // Bind framebuffer for reading
    procedure BindForReading;
    { Note that it is not necessary to unbind before binding another framebuffer. }
    procedure UnBind;
    procedure UnBindForDrawing;
    procedure UnBindForReading;
    // target = GL_DRAW_FRAMEBUFFER, GL_READ_FRAMEBUFFER, GL_FRAMEBUFFER (attach to both READ + DRAW)
    // attachment = COLOR_ATTACHMENTi, DEPTH_ATTACHMENT, STENCIL_ATTACHMENT, DEPTH_STENCIL_ATTACHMENT
    procedure Attach1DTexture(target: TGLenum; attachment: TGLenum; textarget:
      TGLenum; texture: TGLuint; level: TGLint);
    procedure Attach2DTexture(target: TGLenum; attachment: TGLenum; textarget:
      TGLenum; texture: TGLuint; level: TGLint);
    procedure Attach3DTexture(target: TGLenum; attachment: TGLenum; textarget:
      TGLenum; texture: TGLuint; level: TGLint; layer: TGLint);
    procedure AttachLayer(target: TGLenum; attachment: TGLenum; texture:
      TGLuint; level: TGLint; layer: TGLint);
    procedure AttachRenderBuffer(target: TGLenum; attachment: TGLenum;
      renderbuffertarget: TGLenum; renderbuffer: TGLuint);
    // OpenGL 3.2+ only.
    // If texture is the name of a three-dimensional texture, cube map texture, one-or
    // two-dimensional array texture, or two-dimensional multisample array texture, the
    // texture level attached to the framebuffer attachment point is an array of images,
    // and the framebuffer attachment is considered layered.
    procedure AttachTexture(target: TGLenum; attachment: TGLenum; texture:
      TGLuint; level: TGLint);
    // OpenGL 3.2+ only
    procedure AttachTextureLayer(target: TGLenum; attachment: TGLenum; texture:
      TGLuint; level: TGLint; layer: TGLint);

    // copy rect from bound read framebuffer to bound draw framebuffer
    procedure Blit(srcX0: TGLint; srcY0: TGLint; srcX1: TGLint; srcY1: TGLint;
      dstX0: TGLint; dstY0: TGLint; dstX1: TGLint; dstY1: TGLint;
      mask: TGLbitfield; filter: TGLenum);
    // target = GL_DRAW_FRAMEBUFFER, GL_READ_FRAMEBUFFER, GL_FRAMEBUFFER (equivalent to GL_DRAW_FRAMEBUFFER)
    // If default framebuffer (0) is bound:
    // attachment = GL_FRONT_LEFT, GL_FRONT_RIGHT, GL_BACK_LEFT, or GL_BACK_RIGHT, GL_DEPTH, GL_STENCIL
    // if a framebuffer object is bound:
    // attachment = GL_COLOR_ATTACHMENTi, GL_DEPTH_ATTACHMENT, GL_STENCIL_ATTACHMENT, GL_DEPTH_STENCIL_ATTACHMENT
    // param = GL_FRAMEBUFFER_ATTACHMENT_(OBJECT_TYPE, OBJECT_NAME,
    //       RED_SIZE, GREEN_SIZE, BLUE_SIZE, ALPHA_SIZE, DEPTH_SIZE, STENCIL_SIZE,
    //       COMPONENT_TYPE, COLOR_ENCODING, TEXTURE_LEVEL, LAYERED, TEXTURE_CUBE_MAP_FACE, TEXTURE_LAYER
    function GetAttachmentParameter(target: TGLenum; attachment: TGLenum; pname:
      TGLenum): TGLint;
    // Returns the type of object bound to attachment point:
    // GL_NONE, GL_FRAMEBUFFER_DEFAULT, GL_TEXTURE, or GL_RENDERBUFFER
    function GetAttachmentObjectType(target: TGLenum; attachment: TGLenum):
      TGLint;
    // Returns the name (ID) of the texture or renderbuffer attached to attachment point
    function GetAttachmentObjectName(target: TGLenum; attachment: TGLenum):
      TGLint;

    function GetStatus: TVKFramebufferStatus;
    function GetStringStatus(out clarification: string): TVKFramebufferStatus;

    class function IsSupported: Boolean; override;
  end;

  // TVKRenderbufferHandle
  //
  { Manages a handle to a Renderbuffer Object.
     A Renderbuffer is a "framebuffer-attachable image" for generalized offscreen
     rendering and it also provides a means to support rendering to GL logical
     buffer types which have no corresponding texture format (stencil, accum, etc). }
  TVKRenderbufferHandle = class(TVKContextHandle)
  protected
    function DoAllocateHandle: Cardinal; override;
    procedure DoDestroyHandle(var AHandle: TGLuint); override;
    class function IsValid(const ID: GLuint): Boolean; override;
  public
    procedure Bind;
    procedure UnBind;
    procedure SetStorage(internalformat: TGLenum; width, height: TGLsizei);
    procedure SetStorageMultisample(internalformat: TGLenum; samples: TGLsizei;
      width, height: TGLsizei);
    class function IsSupported: Boolean; override;
  end;

  TVKARBProgramHandle = class(TVKContextHandle)
  private
    { Private Declarations }
    FReady: Boolean;
    FInfoLog: string;
  protected
    { Protected Declarations }
    function DoAllocateHandle: Cardinal; override;
    procedure DoDestroyHandle(var AHandle: TGLuint); override;
    class function IsValid(const ID: GLuint): Boolean; override;
    class function GetTarget: TGLenum; virtual; abstract;
  public
    { Public Declarations }
    procedure LoadARBProgram(AText: string);
    procedure Enable;
    procedure Disable;
    procedure Bind;
    property Ready: Boolean read FReady;
    property InfoLog: string read FInfoLog;
  end;

  TVKARBVertexProgramHandle = class(TVKARBProgramHandle)
  protected
    { Protected Declarations }
    class function GetTarget: TGLenum; override;
  public
    { Public Declarations }
    class function IsSupported: Boolean; override;
  end;

  TVKARBFragmentProgramHandle = class(TVKARBProgramHandle)
  protected
    { Protected Declarations }
    class function GetTarget: TGLenum; override;
  public
    { Public Declarations }
    class function IsSupported: Boolean; override;
  end;

  TVKARBGeometryProgramHandle = class(TVKARBProgramHandle)
  protected
    { Protected Declarations }
    class function GetTarget: TGLenum; override;
  public
    { Public Declarations }
    class function IsSupported: Boolean; override;
  end;

  // TVKSLHandle
  //
  { Base class for GLSL handles (programs and shaders). 
     Do not use this class directly, use one of its subclasses instead. }
  TVKSLHandle = class(TVKContextHandle)
  private
    { Private Declarations }

  protected
    { Protected Declarations }
    procedure DoDestroyHandle(var AHandle: TGLuint); override;

  public
    { Public Declarations }
    function InfoLog: string;
    class function IsSupported: Boolean; override;
  end;

  // TVKShaderHandle
  //
  { Manages a handle to a Shader Object. 
     Does *NOT* check for extension availability, this is assumed to have been
     checked by the user. 
     Do not use this class directly, use one of its subclasses instead. }
  TVKShaderHandle = class(TVKSLHandle)
  private
    { Private Declarations }
    FShaderType: Cardinal;

  protected
    { Protected Declarations }
    function DoAllocateHandle: Cardinal; override;
    class function IsValid(const ID: GLuint): Boolean; override;
  public
    { Public Declarations }
    procedure ShaderSource(const source: AnsiString); overload;
    //: Returns True if compilation sucessful
    function CompileShader: Boolean;

    property ShaderType: Cardinal read FShaderType;
  end;

  TVKShaderHandleClass = class of TVKShaderHandle;

  // TVKVertexShaderHandle
  //
  { Manages a handle to a Vertex Shader Object. }
  TVKVertexShaderHandle = class(TVKShaderHandle)
  public
    { Public Declarations }
    constructor Create; override;
    class function IsSupported: Boolean; override;
  end;

  // TVKGeometryShaderHandle
  //
  { Manages a handle to a Geometry Shader Object. }
  TVKGeometryShaderHandle = class(TVKShaderHandle)
  public
    { Public Declarations }
    constructor Create; override;
    class function IsSupported: Boolean; override;
  end;

  // TVKFragmentShaderHandle
  //
  { Manages a handle to a Fragment Shader Object. }
  TVKFragmentShaderHandle = class(TVKShaderHandle)
  public
    { Public Declarations }
    constructor Create; override;
    class function IsSupported: Boolean; override;
  end;

  // TVKTessControlShaderHandle
  //
  { Manages a handle to a Tessellation Control Shader Object. }
  TVKTessControlShaderHandle = class(TVKShaderHandle)
  public
    { Public Declarations }
    constructor Create; override;
    class function IsSupported: Boolean; override;
  end;

  // TVKTessEvaluationShaderHandle
  //
  { Manages a handle to a Tessellation Evaluation Shader Object. }
  TVKTessEvaluationShaderHandle = class(TVKShaderHandle)
  public
    { Public Declarations }
    constructor Create; override;
    class function IsSupported: Boolean; override;
  end;

  // TVKProgramHandle
  //
  { Manages a GLSL Program Object. 
     Does *NOT* check for extension availability, this is assumed to have been
     checked by the user.  }
  TVKProgramHandle = class(TVKSLHandle)
  public
    class function IsValid(const ID: GLuint): Boolean; override;
  private
    { Private Declarations }
    FName: string;
    function GetUniform1i(const index: string): Integer;
    procedure SetUniform1i(const index: string; val: Integer);
    function GetUniform2i(const index: string): TVector2i;
    procedure SetUniform2i(const index: string; const Value: TVector2i);
    function GetUniform3i(const index: string): TVector3i;
    procedure SetUniform3i(const index: string; const Value: TVector3i);
    function GetUniform4i(const index: string): TVector4i;
    procedure SetUniform4i(const index: string; const Value: TVector4i);

    function GetUniform1f(const index: string): Single;
    procedure SetUniform1f(const index: string; val: Single);
    function GetUniform2f(const index: string): TVector2f;
    procedure SetUniform2f(const index: string; const val: TVector2f);
    function GetUniform3f(const index: string): TAffineVector;
    procedure SetUniform3f(const index: string; const val: TAffineVector);
    function GetUniform4f(const index: string): TVector;
    procedure SetUniform4f(const index: string; const val: TVector);

    function GetUniformMatrix2fv(const index: string): TMatrix2f;
    procedure SetUniformMatrix2fv(const index: string; const val: TMatrix2f);
    function GetUniformMatrix3fv(const index: string): TMatrix3f;
    procedure SetUniformMatrix3fv(const index: string; const val: TMatrix3f);
    function GetUniformMatrix4fv(const index: string): TMatrix;
    procedure SetUniformMatrix4fv(const index: string; const val: TMatrix);

    function GetUniformTextureHandle(const index: string;
      const TextureIndex: Integer; const TextureTarget: TVKTextureTarget):
      Cardinal;
    procedure SetUniformTextureHandle(const index: string;
      const TextureIndex: Integer; const TextureTarget: TVKTextureTarget;
      const Value: Cardinal);
    procedure SetUniformBuffer(const index: string;
      Value: TVKUniformBufferHandle);
  protected
    { Protected Declarations }
    function DoAllocateHandle: cardinal; override;

  public
    { Public Declarations }
    property Name: string read FName write FName;

    constructor Create; override;

    { Compile and attach a new shader. 
       Raises an EGLShader exception in case of failure. }
    procedure AddShader(shaderType: TVKShaderHandleClass; const shaderSource:
      string;
      treatWarningsAsErrors: Boolean = False);

    procedure AttachObject(shader: TVKShaderHandle);
    procedure DetachAllObject;
    procedure BindAttribLocation(index: Integer; const aName: string);
    procedure BindFragDataLocation(index: Integer; const aName: string);
    function LinkProgram: Boolean;
    function ValidateProgram: Boolean;
    function GetAttribLocation(const aName: string): Integer;
    function GetUniformLocation(const aName: string): Integer;
    function GetUniformOffset(const aName: string): PGLInt;
    function GetUniformBlockIndex(const aName: string): Integer;

    function GetVaryingLocation(const aName: string): Integer;
    // Currently, NVidia-specific.
    procedure AddActiveVarying(const aName: string);
    // Currently, NVidia-specific.

    function GetUniformBufferSize(const aName: string): Integer;

    procedure UseProgramObject;
    procedure EndUseProgramObject;

    procedure SetUniformi(const index: string; const val: integer); overload;
    procedure SetUniformi(const index: string; const val: TVector2i); overload;
    procedure SetUniformi(const index: string; const val: TVector3i); overload;
    procedure SetUniformi(const index: string; const val: TVector4i); overload;

    procedure SetUniformf(const index: string; const val: single); overload;
    procedure SetUniformf(const index: string; const val: TVector2f); overload;
    procedure SetUniformf(const index: string; const val: TVector3f); overload;
    procedure SetUniformf(const index: string; const val: TVector4f); overload;

    { Shader parameters. }
    property Uniform1i[const index: string]: Integer read GetUniform1i write
    SetUniform1i;
    property Uniform2i[const index: string]: TVector2i read GetUniform2i write
    SetUniform2i;
    property Uniform3i[const index: string]: TVector3i read GetUniform3i write
    SetUniform3i;
    property Uniform4i[const index: string]: TVector4i read GetUniform4i write
    SetUniform4i;

    property Uniform1f[const index: string]: Single read GetUniform1f write
    SetUniform1f;
    property Uniform2f[const index: string]: TVector2f read GetUniform2f write
    SetUniform2f;
    property Uniform3f[const index: string]: TAffineVector read GetUniform3f
    write SetUniform3f;
    property Uniform4f[const index: string]: TVector read GetUniform4f write
    SetUniform4f;

    property UniformMatrix2fv[const index: string]: TMatrix2f read
    GetUniformMatrix2fv write SetUniformMatrix2fv;
    property UniformMatrix3fv[const index: string]: TMatrix3f read
    GetUniformMatrix3fv write SetUniformMatrix3fv;
    property UniformMatrix4fv[const index: string]: TMatrix read
    GetUniformMatrix4fv write SetUniformMatrix4fv;

    property UniformTextureHandle[const index: string; const TextureIndex:
    Integer; const TextureTarget: TVKTextureTarget]: Cardinal read
    GetUniformTextureHandle write SetUniformTextureHandle;
    property UniformBuffer[const index: string]: TVKUniformBufferHandle write
    SetUniformBuffer;
  end;

  // TVKContextNotification
  //
  TVKContextNotification = record
    obj: TObject;
    event: TNotifyEvent;
  end;

  // TVKContextManager
  //
  { Stores and manages all the TVKContext objects.  }
  TVKContextManager = class
  private
    { Private Declarations }
    FList: TThreadList;
    FTerminated: Boolean;
    FNotifications: array of TVKContextNotification;
    FCreatedRCCount: Integer;

{$IFNDEF VKS_MULTITHREAD}
    FHandles: TList;
{$ELSE}
    FHandles: TThreadList;
{$ENDIF VKS_MULTITHREAD}

{$IFDEF VKS_SERVICE_CONTEXT}
    FThread: TThread;
    FServiceStarter: TEvent;
    FThreadTask: TServiceContextTaskList;
{$ENDIF}
    FServiceContext: TVKContext;
  protected
    { Protected Declarations }
    procedure Lock;
    procedure UnLock;

    procedure RegisterContext(aContext: TVKContext);
    procedure UnRegisterContext(aContext: TVKContext);

    procedure ContextCreatedBy(aContext: TVKContext);
    procedure DestroyingContextBy(aContext: TVKContext);

{$IFDEF VKS_SERVICE_CONTEXT}
    { Create a special service and resource-keeper context. }
    procedure CreateServiceContext;
    procedure QueueTaskDepleted;
    property ServiceStarter: TEvent read FServiceStarter;
{$ENDIF}
    property ServiceContext: TVKContext read FServiceContext;
  public
    { Public Declarations }
    constructor Create;
    destructor Destroy; override;

    { Returns an appropriate, ready-to use context. 
       The returned context should be freed by caller. }
    function CreateContext(AClass: TVKContextClass = nil): TVKContext;

    { Returns the number of TVKContext object. 
       This is *not* the number of OpenGL rendering contexts! }
    function ContextCount: Integer;
    { Registers a new object to notify when the last context is destroyed. 
       When the last rendering context is destroyed, the 'anEvent' will
       be invoked with 'anObject' as parameter. 
       Note that the registration is kept until the notification is triggered
       or a RemoveNotification on 'anObject' is issued. }
    procedure LastContextDestroyNotification(anObject: TObject; anEvent:
      TNotifyEvent);
    { Unregisters an object from the notification lists.  }
    procedure RemoveNotification(anObject: TObject);

    //: Marks the context manager for termination
    procedure Terminate;

    { Request all contexts to destroy all their handles. }
    procedure DestroyAllHandles;

    { Notify all contexts about necessity of handles preparation. }
    procedure NotifyPreparationNeed;
  end;

  EGLContext = class(Exception);

  EPBuffer = class(Exception);

  EGLShader = class(EGLContext);

  { Drivers should register themselves via this function. }
procedure RegisterGLContextClass(aGLContextClass: TVKContextClass);
{ The TVKContext that is the currently active context, if any. 
   Returns nil if no context is active. }
function CurrentGLContext: TVKContext;
function SafeCurrentGLContext: TVKContext;
function GL: TGLExtensionsAndEntryPoints;
function IsMainThread: Boolean;
function IsServiceContextAvaible: Boolean;
function GetServiceWindow: TForm;
{$IFDEF VKS_SERVICE_CONTEXT}
procedure AddTaskForServiceContext(ATask: TTaskProcedure; FinishEvent: TFinishTaskEvent = nil);
{$ENDIF}

resourcestring
  cIncompatibleContexts = 'Incompatible contexts';
  cDeleteContextFailed = 'Delete context failed';
  cContextActivationFailed = 'Context activation failed: %X, %s';
  cContextDeactivationFailed = 'Context deactivation failed';
  cUnableToCreateLegacyContext = 'Unable to create legacy context';
  cNoActiveRC = 'No active rendering context';
  glsFailedToShare = 'DoCreateContext - Failed to share contexts';

var
  GLContextManager: TVKContextManager;
  vIgnoreOpenGLErrors: Boolean = False;
  vContextActivationFailureOccurred: Boolean = false;
  vMultitextureCoordinatorClass: TAbstractMultitextureCoordinatorClass;

  // ------------------------------------------------------------------
  // ------------------------------------------------------------------
  // ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

resourcestring
  cCannotAlterAnActiveContext = 'Cannot alter an active context';
  cInvalidContextRegistration = 'Invalid context registration';
  cInvalidNotificationRemoval = 'Invalid notification removal';
  cContextAlreadyCreated = 'Context already created';
  cContextNotCreated = 'Context not created';
  cUnbalancedContexActivations = 'Unbalanced context activations';

{$IFDEF VKS_SERVICE_CONTEXT}
type
  // TServiceContextThread
  //
  TServiceContextThread = class(TThread)
  private
    FDC: THandle; // VCL -> HDC;
    FWindow: TForm;
    FLastTaskStartTime: Double;
    FReported: Boolean;
  protected
    procedure Execute; override;
    procedure DoCreateServiceContext; stdcall;
  public
    constructor Create;
    destructor Destroy; override;
  end;
{$ENDIF}

var
  vContextClasses: TList;
  GLwithoutContext: TGLExtensionsAndEntryPoints;
  vServiceWindow: TForm;
{$IFDEF VKS_SERVICE_CONTEXT}
  OldInitProc: Pointer;
{$ENDIF}

{$IFNDEF VKS_MULTITHREAD}
var
{$ELSE}
threadvar
{$ENDIF}
  vGL: TGLExtensionsAndEntryPoints;
  vCurrentGLContext: TVKContext;
  vMainThread: Boolean;

  // CurrentGLContext
  //

function CurrentGLContext: TVKContext;
begin
  Result := vCurrentGLContext;
end;

function SafeCurrentGLContext: TVKContext;
begin
  Result := CurrentGLContext;
  if not Assigned(Result) then
  begin
   {$IFDEF VKS_LOGGING}
    GLSLogger.LogError(cNoActiveRC);
   {$ENDIF}
    Abort;
  end;
end;

function GL: TGLExtensionsAndEntryPoints;
begin
  Result := vGL;
end;

function IsMainThread: Boolean;
begin
  Result := vMainThread;
end;

function IsServiceContextAvaible: Boolean;
begin
  Result := GLContextManager.ServiceContext <> nil;
end;

function GetServiceWindow: TForm;
begin
  Result := vServiceWindow;
end;


// RegisterGLContextClass
//

procedure RegisterGLContextClass(aGLContextClass: TVKContextClass);
begin
  if not Assigned(vContextClasses) then
    vContextClasses := TList.Create;
  vContextClasses.Add(aGLContextClass);
end;

constructor TAbstractMultitextureCoordinator.Create(AOwner: TVKContext);
begin
  FOwner := AOwner;
end;

// ------------------
// ------------------ TVKContext ------------------
// ------------------

// Create
//

constructor TVKContext.Create;
begin
  inherited Create;
{$IFDEF VKS_MULTITHREAD}
  FLock := TCriticalSection.Create;
{$ENDIF}
  FColorBits := 32;
  FStencilBits := 0;
  FAccumBits := 0;
  FAuxBuffers := 0;
  FLayer := clMainPlane;
  FOptions := [];
{$IFNDEF VKS_MULTITHREAD}
  FSharedContexts := TList.Create;
{$ELSE}
  FSharedContexts := TThreadList.Create;
{$ENDIF}
  FSharedContexts.Add(Self);
  FAcceleration := chaUnknown;
  FGLStates := TVKStateCache.Create;
  FGL := TGLExtensionsAndEntryPoints.Create;
  FTransformation := TVKTransformation.Create;
  FTransformation.LoadMatricesEnabled := True;
  GLContextManager.RegisterContext(Self);
  FIsPraparationNeed := True;
end;

// Destroy
//

destructor TVKContext.Destroy;
begin
  if IsValid then
    DestroyContext;
  GLContextManager.UnRegisterContext(Self);
  FGLStates.Free;
  FGL.Free;
  FXGL.Free;
  FTransformation.Free;
  FSharedContexts.Free;
{$IFDEF VKS_MULTITHREAD}
  FLock.Free;
{$ENDIF}
  inherited Destroy;
end;

// SetColorBits
//

procedure TVKContext.SetColorBits(const aColorBits: Integer);
begin
  if Active then
    raise EGLContext.Create(cCannotAlterAnActiveContext)
  else
    FColorBits := aColorBits;
end;

// SetAlphaBits
//

procedure TVKContext.SetAlphaBits(const aAlphaBits: Integer);
begin
  if Active then
    raise EGLContext.Create(cCannotAlterAnActiveContext)
  else
    FAlphaBits := aAlphaBits;
end;

// SetDepthBits
//

procedure TVKContext.SetDepthBits(const val: Integer);
begin
  if Active then
    raise EGLContext.Create(cCannotAlterAnActiveContext)
  else
    FDepthBits := val;
end;

procedure TVKContext.SetLayer(const Value: TVKContextLayer);
begin
  if Active then
    raise EGLContext.Create(cCannotAlterAnActiveContext)
  else
    FLayer := Value;
end;

// SetStencilBits
//

procedure TVKContext.SetStencilBits(const aStencilBits: Integer);
begin
  if Active then
    raise EGLContext.Create(cCannotAlterAnActiveContext)
  else
    FStencilBits := aStencilBits;
end;

// SetAccumBits
//

procedure TVKContext.SetAccumBits(const aAccumBits: Integer);
begin
  if Active then
    raise EGLContext.Create(cCannotAlterAnActiveContext)
  else
    FAccumBits := aAccumBits;
end;

// SetAuxBuffers
//

procedure TVKContext.SetAuxBuffers(const aAuxBuffers: Integer);
begin
  if Active then
    raise EGLContext.Create(cCannotAlterAnActiveContext)
  else
    FAuxBuffers := aAuxBuffers;
end;

// SetOptions
//

procedure TVKContext.SetOptions(const aOptions: TVKRCOptions);
begin
  if Active then
    raise EGLContext.Create(cCannotAlterAnActiveContext)
  else
    FOptions := aOptions;
end;

// SetAntiAliasing
//

procedure TVKContext.SetAntiAliasing(const val: TVKAntiAliasing);
begin
  if Active then
    raise EGLContext.Create(cCannotAlterAnActiveContext)
  else
    FAntiAliasing := val;
end;

// SetAcceleration
//

procedure TVKContext.SetAcceleration(const val: TVKContextAcceleration);
begin
  if Active then
    raise EGLContext.Create(cCannotAlterAnActiveContext)
  else
    FAcceleration := val;
end;

// GetActive
//

function TVKContext.GetActive: Boolean;
begin
  Result := (FActivationCount > 0);
end;

// SetActive
//

procedure TVKContext.SetActive(const aActive: Boolean);
begin
  // activation/deactivation can be nested...
  while aActive <> Active do
  begin
    if aActive then
      Activate
    else
      Deactivate;
  end;
end;

// CreateContext
//

procedure TVKContext.CreateContext(ADeviceHandle: THandle);
begin
  if IsValid then
    raise EGLContext.Create(cContextAlreadyCreated);
  DoCreateContext(ADeviceHandle);
  Manager.ContextCreatedBy(Self);
end;

// CreateMemoryContext
//

procedure TVKContext.CreateMemoryContext(OutputDevice: THandle;
  Width, Height: Integer; BufferCount: integer);
begin
  if IsValid then
    raise EGLContext.Create(cContextAlreadyCreated);
  DoCreateMemoryContext(OutputDevice, Width, Height, BufferCount);
  Manager.ContextCreatedBy(Self);
end;

// PrepareHandlesData
//

procedure TVKContext.PrepareHandlesData;
var
  I: Integer;
  LHandle: TVKContextHandle;
begin
  if vCurrentGLContext = Self then
  begin
{$IFNDEF VKS_MULTITHREAD}
    for i := Manager.FHandles.Count - 1 downto 0 do
    begin
      LHandle := TVKContextHandle(Manager.FHandles[i]);
      if Assigned(LHandle.FOnPrepare) then
        LHandle.FOnPrepare(Self);
    end;
{$ELSE}
    with Manager.FHandles.LockList do
      try
        for i := Count - 1 downto 0 do
        begin
          LHandle := TVKContextHandle(Items[i]);
          if Assigned(LHandle.FOnPrepare) then
            LHandle.FOnPrepare(Self);
        end;
      finally
        Manager.FHandles.UnlockList;
      end;
{$ENDIF}
    FIsPraparationNeed := False;
  end;
end;

// PropagateSharedContext
//

procedure TVKContext.PropagateSharedContext;
var
  i, j: Integer;
  otherContext: TVKContext;
  otherList: TList;
begin
{$IFNDEF VKS_MULTITHREAD}
  with FSharedContexts do
  begin
    for i := 1 to Count - 1 do
    begin
      otherContext := TVKContext(Items[i]);
      otherList := otherContext.FSharedContexts;
      for J := 0 to otherList.Count - 1 do
        if IndexOf(otherList[J]) < 0 then
          Add(otherList[J]);
    end;
    for i := 1 to Count - 1 do
    begin
      otherContext := TVKContext(Items[i]);
      otherList := otherContext.FSharedContexts;
      if otherList.IndexOf(Self) < 0 then
        otherList.Add(Self);
    end;
  end;
{$ELSE}
  with FSharedContexts.LockList do
    try
      for i := 1 to Count - 1 do
      begin
        otherContext := TVKContext(Items[i]);
        otherList := otherContext.FSharedContexts.LockList;
        for J := 0 to otherList.Count - 1 do
          if IndexOf(otherList[J]) < 0 then
            Add(otherList[J]);
        otherContext.FSharedContexts.UnlockList;
      end;
      for i := 1 to Count - 1 do
      begin
        otherContext := TVKContext(Items[i]);
        otherList := otherContext.FSharedContexts.LockList;
        if otherList.IndexOf(Self) < 0 then
          otherList.Add(Self);
        otherContext.FSharedContexts.UnlockList;
      end;
    finally
      FSharedContexts.UnlockList;
    end;
{$ENDIF}
end;

// ShareLists
//

procedure TVKContext.ShareLists(AContext: TVKContext);
begin
{$IFNDEF VKS_MULTITHREAD}
  if FSharedContexts.IndexOf(AContext) < 0 then
  begin
    if DoShareLists(AContext) then
    begin
      FSharedContexts.Add(AContext);
      PropagateSharedContext;
    end;
  end;
{$ELSE}
  with FSharedContexts.LockList do
    try
      if IndexOf(aContext) < 0 then
      begin
        if DoShareLists(AContext) then
        begin
          Add(aContext);
          PropagateSharedContext;
        end;
      end;
    finally
      FSharedContexts.UnlockList;
    end;
{$ENDIF}
end;

// DestroyAllHandles
//

procedure TVKContext.DestroyAllHandles;
var
  i: Integer;
begin
  Activate;
  try
{$IFNDEF VKS_MULTITHREAD}
    for i := Manager.FHandles.Count - 1 downto 0 do
      TVKContextHandle(Manager.FHandles[i]).ContextDestroying;
{$ELSE}
    with Manager.FHandles.LockList do
      try
        for i := Count - 1 downto 0 do
          TVKContextHandle(Items[i]).ContextDestroying;
      finally
        Manager.FHandles.UnlockList;
      end;
{$ENDIF}
  finally
    Deactivate;
  end;
end;

// DestroyContext
//

procedure TVKContext.DestroyContext;
var
  I: Integer;
  oldContext, otherContext: TVKContext;
  contextHandle: TVKContextHandle;
  aList: TList;
begin

  if vCurrentGLContext <> Self then
  begin
    oldContext := vCurrentGLContext;
    if Assigned(oldContext) then
      oldContext.Deactivate;
  end
  else
    oldContext := nil;

  Activate;
  try
{$IFNDEF VKS_MULTITHREAD}
    for i := Manager.FHandles.Count - 1 downto 0 do
    begin
      contextHandle := TVKContextHandle(Manager.FHandles[i]);
      contextHandle.ContextDestroying;
    end;
{$ELSE}
    aList := Manager.FHandles.LockList;
    try
      for i := aList.Count - 1 downto 0 do
      begin
        contextHandle := TVKContextHandle(aList[i]);
        contextHandle.ContextDestroying;
      end;
    finally
      Manager.FHandles.UnlockList;
    end;
{$ENDIF}
    Manager.DestroyingContextBy(Self);

{$IFDEF VKS_MULTITHREAD}
    aList := FSharedContexts.LockList;
{$ELSE}
    aList := FSharedContexts;
{$ENDIF}
    for I := 1 to aList.Count - 1 do
    begin
      otherContext := TVKContext(aList[I]);
      otherContext.FSharedContexts.Remove(Self);
    end;
    FSharedContexts.Clear;
    FSharedContexts.Add(Self);
{$IFDEF VKS_MULTITHREAD}
    FSharedContexts.UnlockList;
{$ENDIF}
    Active := False;
    DoDestroyContext;
  finally
    if Assigned(oldContext) then
      oldContext.Activate;
  end;
  FAcceleration := chaUnknown;
  FGL.Close;
end;

// Activate
//

procedure TVKContext.Activate;
begin
{$IFDEF VKS_MULTITHREAD}
  FLock.Enter;
{$ENDIF}
  if FActivationCount = 0 then
  begin
    if not IsValid then
      raise EGLContext.Create(cContextNotCreated);

    vContextActivationFailureOccurred := False;
    try
      DoActivate;
    except
      vContextActivationFailureOccurred := True;
    end;
    vGL := FGL;
    vCurrentGLContext := Self;
  end
  else
    Assert(vCurrentGLContext = Self, 'vCurrentGLContext <> Self');
  Inc(FActivationCount);
end;

// Deactivate
//

procedure TVKContext.Deactivate;
begin
  Assert(vCurrentGLContext = Self);
  Dec(FActivationCount);
  if FActivationCount = 0 then
  begin
    if not IsValid then
      raise EGLContext.Create(cContextNotCreated);
    if not vContextActivationFailureOccurred then
      DoDeactivate;
    vCurrentGLContext := nil;
    vGL := GLwithoutContext;
  end
  else if FActivationCount < 0 then
    raise EGLContext.Create(cUnbalancedContexActivations);
{$IFDEF VKS_MULTITHREAD}
  FLock.Leave;
{$ENDIF}
end;

// FindCompatibleContext
//

function TVKContext.FindCompatibleContext: TVKContext;
var
  i: Integer;
begin
  Result := nil;
{$IFNDEF VKS_MULTITHREAD}
  for i := 0 to FSharedContexts.Count - 1 do
    if TVKContext(FSharedContexts[i]) <> Self then
    begin
      Result := TVKContext(FSharedContexts[i]);
      Break;
    end;
{$ELSE}
  with FSharedContexts.LockList do
    try
      for i := 0 to Count - 1 do
        if TVKContext(Items[i]) <> Self then
        begin
          Result := TVKContext(Items[i]);
          Break;
        end;
    finally
      FSharedContexts.UnlockList;
    end;
{$ENDIF}
end;

class function TVKContext.ServiceContext: TVKContext;
begin
  Result := GLContextManager.FServiceContext;
end;

procedure TVKContext.MakeGLCurrent;
begin
  vGL := FGL;
end;

function TVKContext.GetXGL: TAbstractMultitextureCoordinator;
begin
  if FXGL = nil then
    FXGL := vMultitextureCoordinatorClass.Create(Self);
  Result := FXGL;
end;

// ------------------
// ------------------ TVKContextHandle ------------------
// ------------------

// Create
//

constructor TVKContextHandle.Create;
begin
  inherited Create;
  FHandles := TList.Create;
  //first is a dummy record
  new(FLastHandle);
  FillChar(FLastHandle^, sizeof(FLastHandle^), 0);
  FHandles.Add(FLastHandle);
  GLContextManager.FHandles.Add(Self);
end;

// CreateAndAllocate
//

constructor TVKContextHandle.CreateAndAllocate(failIfAllocationFailed: Boolean =
  True);
begin
  Create;
  AllocateHandle;
  if failIfAllocationFailed and (Handle = 0) then
    raise EGLContext.Create('Auto-allocation failed');
end;

// Destroy
//

destructor TVKContextHandle.Destroy;
var
  i : integer;
begin
  DestroyHandle;
  for i := 0 to FHandles.Count-1 do
    Dispose(RCItem(i));
  FHandles.Free;
  if Assigned(GLContextManager) then
    GLContextManager.FHandles.Remove(Self);
  inherited Destroy;
end;

// AllocateHandle
//

function TVKContextHandle.AllocateHandle: TGLuint;
var
  I: Integer;
  bSucces: Boolean;
  aList: TList;
  p : PGLRCHandle;

begin
  // if handle aready allocated in current context
  Result := GetHandle;
  if Result <> 0 then
    exit;

  if vCurrentGLContext = nil then
  begin
///    GLSLogger.LogError('Failed to allocate OpenGL identifier - no active rendering context!');
    exit;
  end;

  //add entry
  New(FLastHandle);
  FillChar(FLastHandle^, sizeof(FLastHandle^), 0);
  FHandles.Add(FLastHandle);
  FLastHandle.FRenderingContext := vCurrentGLContext;

  bSucces := False;
  if Transferable then
  begin
{$IFNDEF VKS_MULTITHREAD}
    aList := vCurrentGLContext.FSharedContexts;
{$ELSE}
    aList := vCurrentGLContext.FSharedContexts.LockList;
    try
{$ENDIF}
      for I := aList.Count - 1 downto 0 do
      begin
        P := SearchRC(aList[I]);
        if (P.FHandle > 0) then
        begin
          // Copy shared handle
          //FLastHandle.FRenderingContext := vCurrentGLContext;
          FLastHandle.FHandle           := P.FHandle;
          FLastHandle.FChanged          := P.FChanged;
          Inc(vCurrentGLContext.FOwnedHandlesCount);
          bSucces := True;
          break;
        end;
      end;
{$IFNDEF VKS_MULTITHREAD}
{$ELSE}
    finally
      vCurrentGLContext.FSharedContexts.UnlockList;
    end;
{$ENDIF}
  end;

  if not bSucces then
  begin
    // Allocate handle in current context
    FLastHandle.FHandle := DoAllocateHandle;
    bSucces := FLastHandle.FHandle <> 0;
    FLastHandle.FChanged := bSucces;
    if bSucces then
      Inc(vCurrentGLContext.FOwnedHandlesCount);
  end;

  Result := FLastHandle.FHandle;
  if not bSucces then
///    GLSLogger.LogError(cNoActiveRC)
  else if Assigned(FOnPrepare) then
    GLContextManager.NotifyPreparationNeed;
end;

function TVKContextHandle.IsAllocatedForContext(AContext: TVKContext = nil): Boolean;
begin
  Result := SearchRC(AContext).FHandle > 0;
end;

function TVKContextHandle.SearchRC(AContext: TVKContext): PGLRCHandle;
var
  i : integer;
begin
  if AContext = nil then
    AContext := vCurrentGLContext;

  if AContext = FLastHandle.FRenderingContext then
  begin
    Result := FLastHandle;
    exit;
  end;

  for i := 1 to FHandles.Count-1 do
    if RCItem(i).FRenderingContext = AContext then
    begin
      Result := RCItem(i);
      exit;
    end;

  //first handle is always a dummy
  Result := FHandles[0];
end;

procedure TVKContextHandle.CheckCurrentRC;
begin
  if vCurrentGLContext <> FLastHandle.FRenderingContext then
    FLastHandle := SearchRC(vCurrentGLContext);
end;

function TVKContextHandle.GetHandle: TGLuint;
begin
//  CheckCurrentRC;
//inline doesn't always work... so optimize it here
  if vCurrentGLContext <> FLastHandle.FRenderingContext then
    FLastHandle := SearchRC(vCurrentGLContext);

  Result := FLastHandle.FHandle;
end;

// DestroyHandle
//

procedure TVKContextHandle.DestroyHandle;
var
  oldContext: TVKContext;
  P : PGLRCHandle;
  I: Integer;
begin
  oldContext := vCurrentGLContext;
  if Assigned(oldContext) then
    oldContext.Deactivate;
  try
    for I := FHandles.Count-1 downto 1 do
    begin
      P := FHandles[I];
      if P.FHandle > 0 then
      begin
        P.FRenderingContext.Activate;
        if IsValid(P.FHandle) then
          DoDestroyHandle(P.FHandle);
        Dec(P.FRenderingContext.FOwnedHandlesCount);
        P.FRenderingContext.Deactivate;
        P.FRenderingContext := nil;
        P.FHandle := 0;
        P.FChanged := True;
      end;
      Dispose(P);
    end;
    FHandles.Count := 1; //delete all in 1 step
    FLastHandle := FHandles[0];
  finally
    if Assigned(vCurrentGLContext) then
      vCurrentGLContext.Deactivate;
    if Assigned(oldContext) then
      oldContext.Activate;
  end;
end;

// ContextDestroying
//

procedure TVKContextHandle.ContextDestroying;
var
  I: Integer;
  P: PGLRCHandle;
  aList: TList;
  bShared: Boolean;
begin
  if Assigned(vCurrentGLContext) then
  begin
    bShared := False;
    if Transferable then
    begin
    {$IFNDEF VKS_MULTITHREAD}
      aList := vCurrentGLContext.FSharedContexts;
    {$ELSE}
      aList := vCurrentGLContext.FSharedContexts.LockList;
      try
    {$ENDIF VKS_MULTITHREAD}
        for I := FHandles.Count-1 downto 1 do
        begin
          P := RCItem(I);
          if (P.FRenderingContext <> vCurrentGLContext)
            and (P.FHandle <> 0)
            and (aList.IndexOf(P.FRenderingContext) > -1) then
            begin
              bShared := True;
              break;
            end;
        end;
    {$IFDEF VKS_MULTITHREAD}
      finally
        vCurrentGLContext.FSharedContexts.UnLockList;
      end;
    {$ENDIF VKS_MULTITHREAD}
    end;

    for I := FHandles.Count-1 downto 1 do
    begin
      P := RCItem(I);
      if (P.FRenderingContext = vCurrentGLContext) and (P.FHandle <> 0) then
      begin
        if not bShared then
          if IsValid(P.FHandle) then
            DoDestroyHandle(P.FHandle);
        Dec(P.FRenderingContext.FOwnedHandlesCount);
        P.FHandle := 0;
        P.FRenderingContext := nil;
        P.FChanged := True;
        Dispose(P);
        FHandles.Delete(I);
        if FLastHandle = P then
          FLastHandle := FHandles[0];
        exit;
      end;
    end;
  end;
end;

function TVKContextHandle.GetContext: TVKContext;
var
  I: Integer;
  P: PGLRCHandle;
begin
  Result := nil;
  // Return first context where handle is allocated
  for I := FHandles.Count-1 downto 1 do
  begin
    P := RCItem(I);
    if (P.FRenderingContext <> nil) and (P.FHandle <> 0) then
    begin
      Result := P.FRenderingContext;
      // If handle allocated in active context - return it
      if (Result = vCurrentGLContext) then
        exit;
    end;
  end;
end;

function TVKContextHandle.IsDataNeedUpdate: Boolean;
begin
  if GetHandle = 0 then
    CheckCurrentRC;
  Result := (FLastHandle.FHandle = 0) or FLastHandle.FChanged;
end;

function TVKContextHandle.IsDataComplitelyUpdated: Boolean;
var
  I: Integer;
begin
  Result := false;
  for I := FHandles.Count-1 downto 1 do
  begin
    with RCItem(i)^ do
      if (FRenderingContext <> nil) and (FHandle <> 0) and FChanged then exit;
  end;
  Result := true;
end;

procedure TVKContextHandle.NotifyDataUpdated;
var
  I: Integer;
  aList: TList;
begin
  if Assigned(vCurrentGLContext) then
  begin
    if not Transferable then
    begin
      CheckCurrentRC();
      if FLastHandle.FHandle <> 0 then
      begin
        FLastHandle.FChanged := False;
        exit;
      end;
    end
    else
    begin
  {$IFNDEF VKS_MULTITHREAD}
      aList := vCurrentGLContext.FSharedContexts;
  {$ELSE}
      aList := vCurrentGLContext.FSharedContexts.LockList;
      try
  {$ENDIF}
        for I := 0 to aList.Count - 1 do
        begin
          with SearchRC(aList[I])^ do
            if (FHandle <> 0) then
              FChanged := False;
        end;
  {$IFDEF VKS_MULTITHREAD}
      finally
        vCurrentGLContext.FSharedContexts.UnlockList;
      end;
  {$ENDIF}
    end;
  end
  else
///    GLSLogger.LogError(cNoActiveRC);
end;

function TVKContextHandle.RCItem(AIndex: integer): PGLRCHandle;
begin
  Result := FHandles[AIndex];
end;

procedure TVKContextHandle.NotifyChangesOfData;
var
  I: Integer;
begin
  for I := FHandles.Count-1 downto 1 do
    RCItem(I).FChanged := True;
  if Assigned(FOnPrepare) then
    GLContextManager.NotifyPreparationNeed;
end;

function TVKContextHandle.IsShared: Boolean;
var
  I: Integer;
  vContext: TVKContext;
  aList: TList;
begin
  Result := False;
  // untransferable handles can't be shared
  if not Transferable then
    exit;
  Result := True;
{$IFNDEF VKS_MULTITHREAD}
  aList := vCurrentGLContext.FSharedContexts;
{$ELSE}
  aList := vCurrentGLContext.FSharedContexts.LockList;
  try
{$ENDIF}
    for I := 0 to aList.Count - 1 do
    begin
      vContext := aList[I];
      if (vContext <> vCurrentGLContext) and
        // at least one context is friendly
        (SearchRC(vContext).FHandle <> 0) then
        exit;
    end;
{$IFDEF VKS_MULTITHREAD}
  finally
    vCurrentGLContext.FSharedContexts.UnlockList;
  end;
{$ENDIF}
  Result := false;
end;

// Transferable
//

class function TVKContextHandle.Transferable: Boolean;
begin
  Result := True;
end;

class function TVKContextHandle.IsValid(const ID: GLuint): Boolean;
begin
  Result := True;
end;
// IsSupported
//

class function TVKContextHandle.IsSupported: Boolean;
begin
  Result := True;
end;

// ------------------
// ------------------ TVKVirtualHandle ------------------
// ------------------

// DoAllocateHandle
//

function TVKVirtualHandle.DoAllocateHandle: Cardinal;
begin
  Result := 0;
  if Assigned(FOnAllocate) then
    FOnAllocate(Self, Result);
end;

// DoDestroyHandle
//

procedure TVKVirtualHandle.DoDestroyHandle(var AHandle: TGLuint);
begin
  if not vContextActivationFailureOccurred then
  with GL do
  begin
    // reset error status
    ClearError;
    // delete
    if Assigned(FOnDestroy) then
      FOnDestroy(Self, AHandle);
    // check for error
    CheckError;
  end;
end;

class function TVKVirtualHandle.Transferable: Boolean;
begin
  Result := False;
end;

{ TVKVirtualHandleTransf }

class function TVKVirtualHandleTransf.Transferable: Boolean;
begin
  Result := True;
end;

// ------------------
// ------------------ TVKListHandle ------------------
// ------------------

// DoAllocateHandle
//

function TVKListHandle.DoAllocateHandle: Cardinal;
begin
  Result := GL.GenLists(1);
end;

// DoDestroyHandle
//

procedure TVKListHandle.DoDestroyHandle(var AHandle: TGLuint);
begin
  if not vContextActivationFailureOccurred then
  with GL do
  begin
    // reset error status
    ClearError;
    // delete
    DeleteLists(AHandle, 1);
    // check for error
    CheckError;
  end;
end;

// IsValid
//

class function TVKListHandle.IsValid(const ID: GLuint): Boolean;
begin
  Result := GL.IsList(ID);
end;

// NewList
//

procedure TVKListHandle.NewList(mode: Cardinal);
begin
  vCurrentGLContext.GLStates.NewList(GetHandle, mode);
end;

// EndList
//

procedure TVKListHandle.EndList;
begin
  vCurrentGLContext.GLStates.EndList;
end;

// CallList
//

procedure TVKListHandle.CallList;
begin
  vCurrentGLContext.GLStates.CallList(GetHandle);
end;

// ------------------
// ------------------ TVKTextureHandle ------------------
// ------------------

// DoAllocateHandle
//

function TVKTextureHandle.DoAllocateHandle: Cardinal;
begin
  Result := 0;
  GL.GenTextures(1, @Result);
  FTarget := ttNoShape;
end;

// DoDestroyHandle
//

procedure TVKTextureHandle.DoDestroyHandle(var AHandle: TGLuint);
var
  a: TGLint;
  t: TVKTextureTarget;
begin
  if not vContextActivationFailureOccurred then
  with GL do
  begin
    // reset error status
    GetError;
    { Unbind identifier from all image selectors. }
    if ARB_multitexture then
    begin
      with GetContext.GLStates do
      begin
        for a := 0 to MaxTextureImageUnits - 1 do
          for t := Low(TVKTextureTarget) to High(TVKTextureTarget) do
            if TextureBinding[a, t] = AHandle then
              TextureBinding[a, t] := 0;
      end
    end
    else
      with GetContext.GLStates do
        for t := Low(TVKTextureTarget) to High(TVKTextureTarget) do
          if TextureBinding[0, t] = AHandle then
            TextureBinding[0, t] := 0;

    DeleteTextures(1, @AHandle);
    // check for error
    CheckError;
  end;
end;

// IsValid
//

class function TVKTextureHandle.IsValid(const ID: GLuint): Boolean;
begin
  Result := GL.IsTexture(ID);
end;

procedure TVKTextureHandle.SetTarget(ATarget: TVKTextureTarget);
begin
  if FTarget = ttNoShape then
    FTarget := ATarget;
end;

// ------------------
// ------------------ TVKSamplerHandle ------------------
// ------------------

// DoAllocateHandle
//

function TVKSamplerHandle.DoAllocateHandle: Cardinal;
begin
  Result := 0;
  GL.GenSamplers(1, @Result);
end;

// DoDestroyHandle
//

procedure TVKSamplerHandle.DoDestroyHandle(var AHandle: TGLuint);
begin
  if not vContextActivationFailureOccurred then
  with GL do
  begin
    // reset error status
    GetError;
    // delete
    DeleteSamplers(1, @AHandle);
    // check for error
    CheckError;
  end;
end;

// TVKSamplerHandle
//

class function TVKSamplerHandle.IsSupported: Boolean;
begin
  Result := GL.ARB_sampler_objects;
end;

// IsValid
//

class function TVKSamplerHandle.IsValid(const ID: GLuint): Boolean;
begin
  Result := GL.IsSampler(ID);
end;

// ------------------
// ------------------ TVKQueryHandle ------------------
// ------------------

// BeginQuery
//

procedure TVKQueryHandle.BeginQuery;
begin
  if vCurrentGLContext.GLStates.CurrentQuery[QueryType] = 0 then
    vCurrentGLContext.GLStates.BeginQuery(QueryType, GetHandle);
  Factive := True;
end;

// CounterBits
//

function TVKQueryHandle.CounterBits: integer;
begin
  GL.GetQueryiv(Target, GL_QUERY_COUNTER_BITS, @Result);
end;

// DoAllocateHandle
//

function TVKQueryHandle.DoAllocateHandle: Cardinal;
begin
  Result := 0;
  GL.GenQueries(1, @Result);
end;

// DoDestroyHandle
//

procedure TVKQueryHandle.DoDestroyHandle(var AHandle: TGLuint);
begin
  if not vContextActivationFailureOccurred then
  with GL do
  begin
    // reset error status
    GetError;
    // delete
    DeleteQueries(1, @AHandle);
    // check for error
    CheckError;
  end;
end;

// IsValid
//

class function TVKQueryHandle.IsValid(const ID: GLuint): Boolean;
begin
  Result := GL.IsQuery(ID);
end;

// EndQuery
//

procedure TVKQueryHandle.EndQuery;
begin
  Assert(FActive = true, 'Cannot end a query before it begins');
  Factive := False;
  Assert(Handle <> 0);
  //glEndQuery(Target);
  vCurrentGLContext.GLStates.EndQuery(QueryType);
end;

// IsResultAvailable
//

function TVKQueryHandle.IsResultAvailable: boolean;
begin
  GL.GetQueryObjectiv(Handle, GL_QUERY_RESULT_AVAILABLE, @Result);
end;

// QueryResultInt
//

function TVKQueryHandle.QueryResultInt: TGLInt;
begin
  GL.GetQueryObjectiv(Handle, GL_QUERY_RESULT, @Result);
end;

// QueryResultInt64
//

function TVKQueryHandle.QueryResultInt64: TGLint64EXT;
begin
  GL.GetQueryObjecti64v(Handle, GL_QUERY_RESULT, @Result);
end;

// QueryResultUInt
//

function TVKQueryHandle.QueryResultUInt: TGLuint;
begin
  GL.GetQueryObjectuiv(Handle, GL_QUERY_RESULT, @Result);
end;

// QueryResultUInt64
//

function TVKQueryHandle.QueryResultUInt64: TGLuint64EXT;
begin
  GL.GetQueryObjectui64v(Handle, GL_QUERY_RESULT, @Result);
end;

function TVKQueryHandle.QueryResultBool: TGLboolean;
var
  I: TGLuint;
begin
  GL.GetQueryObjectuiv(Handle, GL_QUERY_RESULT, @I);
  Result := I > 0;
end;

// Transferable
//

class function TVKQueryHandle.Transferable: Boolean;
begin
  Result := False;
end;

// ------------------
// ------------------ TVKOcclusionQueryHandle ------------------
// ------------------

// GetQueryType
//

function TVKOcclusionQueryHandle.GetQueryType: TQueryType;
begin
  Result := qrySamplesPassed;
end;

// GetTarget
//

function TVKOcclusionQueryHandle.GetTarget: TGLuint;
begin
  Result := GL_SAMPLES_PASSED;
end;

// IsSupported
//

class function TVKOcclusionQueryHandle.IsSupported: Boolean;
begin
  Result := GL.VERSION_1_5;
end;

// PixelCount
//

function TVKOcclusionQueryHandle.PixelCount: Integer;
begin
  Result := QueryResultUInt;
end;

// ------------------
// ------------------ TVKBooleanOcclusionQueryHandle ------------------
// ------------------

// GetQueryType
//

function TVKBooleanOcclusionQueryHandle.GetQueryType: TQueryType;
begin
  Result := qryAnySamplesPassed;
end;

// GetTarget
//

function TVKBooleanOcclusionQueryHandle.GetTarget: TGLuint;
begin
  Result := GL_ANY_SAMPLES_PASSED;
end;

// IsSupported
//

class function TVKBooleanOcclusionQueryHandle.IsSupported: Boolean;
begin
  Result := GL.ARB_occlusion_query2;
end;

// ------------------
// ------------------ TVKTimerQueryHandle ------------------
// ------------------

// GetTarget
//

function TVKTimerQueryHandle.GetQueryType: TQueryType;
begin
  Result := qryTimeElapsed;
end;

function TVKTimerQueryHandle.GetTarget: TGLuint;
begin
  Result := GL_TIME_ELAPSED;
end;

// IsSupported
//

class function TVKTimerQueryHandle.IsSupported: Boolean;
begin
  Result := GL.EXT_timer_query or GL.ARB_timer_query;
end;

// Time
//

function TVKTimerQueryHandle.Time: Integer;
begin
  Result := QueryResultUInt;
end;

// ------------------
// ------------------ TVKPrimitiveQueryHandle ------------------
// ------------------

// GetQueryType
//

function TVKPrimitiveQueryHandle.GetQueryType: TQueryType;
begin
  Result := qryPrimitivesGenerated;
end;

// GetTarget
//

function TVKPrimitiveQueryHandle.GetTarget: TGLuint;
begin
  Result := GL_PRIMITIVES_GENERATED;
end;

// IsSupported
//

class function TVKPrimitiveQueryHandle.IsSupported: Boolean;
begin
  Result := GL.VERSION_3_0;
end;

// PrimitivesGenerated
//

function TVKPrimitiveQueryHandle.PrimitivesGenerated: Integer;
begin
  Result := QueryResultUInt;
end;

// ------------------
// ------------------ TVKBufferObjectHandle ------------------
// ------------------

// CreateFromData
//

constructor TVKBufferObjectHandle.CreateFromData(p: Pointer; size: Integer;
  bufferUsage: TGLuint);
begin
  Create;
  AllocateHandle;
  Bind;
  BufferData(p, size, bufferUsage);
  UnBind;
end;

// DoAllocateHandle
//

function TVKBufferObjectHandle.DoAllocateHandle: Cardinal;
begin
  Result := 0;
  GL.GenBuffers(1, @Result);
end;

// DoDestroyHandle
//

procedure TVKBufferObjectHandle.DoDestroyHandle(var AHandle: TGLuint);
begin
  if not vContextActivationFailureOccurred then
  with GL do
  begin
    // reset error status
    GetError;
    UnBind;
    // delete
    DeleteBuffers(1, @AHandle);
    // check for error
    CheckError;
  end;
end;

// IsValid
//

class function TVKBufferObjectHandle.IsValid(const ID: GLuint): Boolean;
begin
  Result := GL.IsBuffer(ID);
end;

// IsSupported
//

class function TVKBufferObjectHandle.IsSupported: Boolean;
begin
  Result := GL.ARB_vertex_buffer_object;
end;

// BindRange
//

procedure TVKBufferObjectHandle.BindRange(index: TGLuint; offset: TGLintptr;
  size: TGLsizeiptr);
begin
  Assert(False, 'BindRange only XBO and UBO');
end;

// BindBase
//

procedure TVKBufferObjectHandle.BindBase(index: TGLuint);
begin
  Assert(False, 'BindRange only XBO and UBO');
end;

// UnBindBase
//

procedure TVKBufferObjectHandle.UnBindBase(index: TGLuint);
begin
  Assert(False, 'BindRange only XBO and UBO');
end;

// BufferData
//

procedure TVKBufferObjectHandle.BufferData(p: Pointer; size: Integer;
  bufferUsage: TGLuint);
begin
  FSize := size;
  GL.BufferData(Target, size, p, bufferUsage);
end;

// BindBufferData
//

procedure TVKBufferObjectHandle.BindBufferData(p: Pointer; size: Integer;
  bufferUsage: TGLuint);
begin
  Bind;
  FSize := size;
  GL.BufferData(Target, size, p, bufferUsage);
end;

// BufferSubData
//

procedure TVKBufferObjectHandle.BufferSubData(offset, size: Integer; p:
  Pointer);
begin
  Assert(offset + size <= FSize);
  GL.BufferSubData(Target, offset, size, p);
end;

// MapBuffer
//

function TVKBufferObjectHandle.MapBuffer(access: TGLuint): Pointer;
begin
  Result := GL.MapBuffer(Target, access);
end;

// MapBufferRange
//

function TVKBufferObjectHandle.MapBufferRange(offset: TGLint; len: TGLsizei;
  access: TGLbitfield): Pointer;
begin
  Result := GL.MapBufferRange(Target, offset, len, access);
end;

// Flush
//

procedure TVKBufferObjectHandle.Flush(offset: TGLint; len: TGLsizei);
begin
  GL.FlushMappedBufferRange(Target, offset, len);
end;

// UnmapBuffer
//

function TVKBufferObjectHandle.UnmapBuffer: Boolean;
begin
  Result := GL.UnmapBuffer(Target);
end;

// ------------------
// ------------------ TVKVBOHandle ------------------
// ------------------

// GetVBOTarget
//

function TVKVBOHandle.GetVBOTarget: TGLuint;
begin
  Result := Target;
end;

// ------------------
// ------------------ TVKVBOArrayBufferHandle ------------------
// ------------------

procedure TVKVBOArrayBufferHandle.Bind;
begin
  vCurrentGLContext.GLStates.ArrayBufferBinding := Handle;
end;

procedure TVKVBOArrayBufferHandle.UnBind;
begin
  vCurrentGLContext.GLStates.ArrayBufferBinding := 0;
end;

// GetTarget
//

function TVKVBOArrayBufferHandle.GetTarget: TGLuint;
begin
  Result := GL_ARRAY_BUFFER;
end;

// ------------------
// ------------------ TVKVBOElementArrayHandle ------------------
// ------------------

procedure TVKVBOElementArrayHandle.Bind;
begin
  vCurrentGLContext.GLStates.ElementBufferBinding := Handle;
end;

procedure TVKVBOElementArrayHandle.UnBind;
begin
  vCurrentGLContext.GLStates.ElementBufferBinding := 0;
end;

// GetTarget
//

function TVKVBOElementArrayHandle.GetTarget: TGLuint;
begin
  Result := GL_ELEMENT_ARRAY_BUFFER;
end;

// ------------------
// ------------------ TVKPackPBOHandle ------------------
// ------------------

procedure TVKPackPBOHandle.Bind;
begin
  vCurrentGLContext.GLStates.PixelPackBufferBinding := Handle;
end;

procedure TVKPackPBOHandle.UnBind;
begin
  vCurrentGLContext.GLStates.PixelPackBufferBinding := 0;
end;

// GetTarget
//

function TVKPackPBOHandle.GetTarget: TGLuint;
begin
  Result := GL_PIXEL_PACK_BUFFER;
end;

// IsSupported
//

class function TVKPackPBOHandle.IsSupported: Boolean;
begin
  Result := GL.ARB_pixel_buffer_object;
end;

// ------------------
// ------------------ TVKUnpackPBOHandle ------------------
// ------------------

procedure TVKUnpackPBOHandle.Bind;
begin
  vCurrentGLContext.GLStates.PixelUnpackBufferBinding := Handle;
end;

procedure TVKUnpackPBOHandle.UnBind;
begin
  vCurrentGLContext.GLStates.PixelUnpackBufferBinding := 0;
end;

// GetTarget
//

function TVKUnpackPBOHandle.GetTarget: TGLuint;
begin
  Result := GL_PIXEL_UNPACK_BUFFER;
end;

// IsSupported
//

class function TVKUnpackPBOHandle.IsSupported: Boolean;
begin
  Result := GL.ARB_pixel_buffer_object;
end;

// ------------------
// ------------------ TVKTransformFeedbackBufferHandle ------------------
// ------------------

// GetTarget
//

procedure TVKTransformFeedbackBufferHandle.Bind;
begin
  vCurrentGLContext.GLStates.TransformFeedbackBufferBinding := Handle;
end;

procedure TVKTransformFeedbackBufferHandle.UnBind;
begin
  vCurrentGLContext.GLStates.TransformFeedbackBufferBinding := 0;
end;

function TVKTransformFeedbackBufferHandle.GetTarget: TGLuint;
begin
  Result := GL_TRANSFORM_FEEDBACK_BUFFER;
end;

// BeginTransformFeedback
//

procedure TVKTransformFeedbackBufferHandle.BeginTransformFeedback(primitiveMode:
  TGLenum);
begin
  GL.BeginTransformFeedback(primitiveMode);
end;

// EndTransformFeedback
//

procedure TVKTransformFeedbackBufferHandle.EndTransformFeedback();
begin
  GL.EndTransformFeedback();
end;

procedure TVKTransformFeedbackBufferHandle.BindRange(index: TGLuint; offset: TGLintptr;
  size: TGLsizeiptr);
begin
  vCurrentGLContext.GLStates.SetBufferIndexedBinding(Handle, bbtTransformFeedBack,
    index, offset, size);
end;

procedure TVKTransformFeedbackBufferHandle.BindBase(index: TGLuint);
begin
  vCurrentGLContext.GLStates.SetBufferIndexedBinding(Handle, bbtTransformFeedBack,
    index, BufferSize);
end;

procedure TVKTransformFeedbackBufferHandle.UnBindBase(index: TGLuint);
begin
  vCurrentGLContext.GLStates.SetBufferIndexedBinding(0, bbtTransformFeedBack,
    index, 0);
end;

// IsSupported
//

class function TVKTransformFeedbackBufferHandle.IsSupported: Boolean;
begin
  Result := GL.EXT_transform_feedback or GL.VERSION_3_0;
end;

// ------------------
// ------------------ TVKTextureBufferHandle ------------------
// ------------------

procedure TVKTextureBufferHandle.Bind;
begin
  vCurrentGLContext.GLStates.TextureBufferBinding := Handle;
end;

procedure TVKTextureBufferHandle.UnBind;
begin
  vCurrentGLContext.GLStates.TextureBufferBinding := 0;
end;

// GetTarget
//

function TVKTextureBufferHandle.GetTarget: TGLuint;
begin
  Result := GL_TEXTURE_BUFFER;
end;

// IsSupported
//

class function TVKTextureBufferHandle.IsSupported: Boolean;
begin
  Result := GL.EXT_texture_buffer_object or GL.ARB_texture_buffer_object or
    GL.VERSION_3_1;
end;

// ------------------
// ------------------ TVKUniformBufferHandle ------------------
// ------------------

procedure TVKUniformBufferHandle.Bind;
begin
  vCurrentGLContext.GLStates.UniformBufferBinding := Handle;
end;

procedure TVKUniformBufferHandle.UnBind;
begin
  vCurrentGLContext.GLStates.UniformBufferBinding := 0;
end;

procedure TVKUniformBufferHandle.BindRange(index: TGLuint; offset: TGLintptr;
  size: TGLsizeiptr);
begin
  vCurrentGLContext.GLStates.SetBufferIndexedBinding(Handle, bbtUniform,
    index, offset, size);
end;

procedure TVKUniformBufferHandle.BindBase(index: TGLuint);
begin
  vCurrentGLContext.GLStates.SetBufferIndexedBinding(Handle, bbtUniform,
    index, BufferSize);
end;

procedure TVKUniformBufferHandle.UnBindBase(index: TGLuint);
begin
  vCurrentGLContext.GLStates.SetBufferIndexedBinding(0, bbtUniform,
    index, 0);
end;

// GetTarget
//

function TVKUniformBufferHandle.GetTarget: TGLuint;
begin
  Result := GL_UNIFORM_BUFFER;
end;

// IsSupported
//

class function TVKUniformBufferHandle.IsSupported: Boolean;
begin
  Result := GL.ARB_uniform_buffer_object;
end;

// ------------------
// ------------------ TVKVertexArrayHandle ------------------
// ------------------

// DoAllocateHandle
//

function TVKVertexArrayHandle.DoAllocateHandle: Cardinal;
begin
  Result := 0;
  GL.GenVertexArrays(1, @Result);
end;

// DoDestroyHandle
//

procedure TVKVertexArrayHandle.DoDestroyHandle(var AHandle: TGLuint);
begin
  if not vContextActivationFailureOccurred then
  with GL do
  begin
    // reset error status
    GetError;
    // delete
    DeleteVertexArrays(1, @AHandle);
    // check for error
    CheckError;
  end;
end;

// IsValid
//

class function TVKVertexArrayHandle.IsValid(const ID: GLuint): Boolean;
begin
  Result := GL.IsVertexArray(ID);
end;

// Bind
//

procedure TVKVertexArrayHandle.Bind;
begin
  Assert(vCurrentGLContext <> nil);
  vCurrentGLContext.GLStates.VertexArrayBinding := Handle;
end;

// UnBind
//

procedure TVKVertexArrayHandle.UnBind;
begin
  Assert(vCurrentGLContext <> nil);
  vCurrentGLContext.GLStates.VertexArrayBinding := 0;
end;

// IsSupported
//

class function TVKVertexArrayHandle.IsSupported: Boolean;
begin
  Result := GL.ARB_vertex_array_object;
end;

// Transferable
//

class function TVKVertexArrayHandle.Transferable: Boolean;
begin
  Result := False;
end;

// ------------------
// ------------------ TVKFramebufferHandle ------------------
// ------------------

// DoAllocateHandle
//

function TVKFramebufferHandle.DoAllocateHandle: Cardinal;
begin
  Result := 0;
  GL.GenFramebuffers(1, @Result)
end;

// DoDestroyHandle
//

procedure TVKFramebufferHandle.DoDestroyHandle(var AHandle: TGLuint);
begin
  if not vContextActivationFailureOccurred then
  with GL do
  begin
    // reset error status
    GetError;
    // delete
    DeleteFramebuffers(1, @AHandle);
    // check for error
    CheckError;
  end;
end;

// IsValid
//

class function TVKFramebufferHandle.IsValid(const ID: GLuint): Boolean;
begin
  Result := GL.IsFramebuffer(ID);
end;

// Bind
//

procedure TVKFramebufferHandle.Bind;
begin
  Assert(vCurrentGLContext <> nil);
  vCurrentGLContext.GLStates.SetFrameBuffer(Handle);
end;

// BindForDrawing
//

procedure TVKFramebufferHandle.BindForDrawing;
begin
  Assert(vCurrentGLContext <> nil);
  vCurrentGLContext.GLStates.DrawFrameBuffer := Handle;
end;

// BindForReading
//

procedure TVKFramebufferHandle.BindForReading;
begin
  Assert(vCurrentGLContext <> nil);
  vCurrentGLContext.GLStates.ReadFrameBuffer := Handle;
end;

// UnBind
//

procedure TVKFramebufferHandle.UnBind;
begin
  Assert(vCurrentGLContext <> nil);
  vCurrentGLContext.GLStates.SetFrameBuffer(0);
end;

// UnBindForDrawing
//

procedure TVKFramebufferHandle.UnBindForDrawing;
begin
  Assert(vCurrentGLContext <> nil);
  vCurrentGLContext.GLStates.DrawFrameBuffer := 0;
end;

// UnBindForReading
//

procedure TVKFramebufferHandle.UnBindForReading;
begin
  Assert(vCurrentGLContext <> nil);
  vCurrentGLContext.GLStates.ReadFrameBuffer := 0;
end;

// Attach1DTexture
//

procedure TVKFramebufferHandle.Attach1DTexture(target: TGLenum; attachment:
  TGLenum; textarget: TGLenum; texture: TGLuint; level: TGLint);
begin
  GL.FramebufferTexture1D(target, attachment, textarget, texture, level);
end;

// Attach2DTexture
//

procedure TVKFramebufferHandle.Attach2DTexture(target: TGLenum; attachment:
  TGLenum; textarget: TGLenum; texture: TGLuint; level: TGLint);
begin
  GL.FramebufferTexture2D(target, attachment, textarget, texture, level);
end;

// Attach3DTexture
//

procedure TVKFramebufferHandle.Attach3DTexture(target: TGLenum; attachment:
  TGLenum; textarget: TGLenum; texture: TGLuint; level: TGLint; layer: TGLint);
begin
  GL.FramebufferTexture3D(target, attachment, textarget, texture, level, layer);
end;

// AttachLayer
//

procedure TVKFramebufferHandle.AttachLayer(target: TGLenum; attachment: TGLenum;
  texture: TGLuint; level: TGLint; layer: TGLint);
begin
  GL.FramebufferTextureLayer(target, attachment, texture, level, layer);
end;

// AttachRenderBuffer
//

procedure TVKFramebufferHandle.AttachRenderBuffer(target: TGLenum; attachment:
  TGLenum; renderbuffertarget: TGLenum; renderbuffer: TGLuint);
begin
  GL.FramebufferRenderbuffer(target, attachment, renderbuffertarget,
    renderbuffer);
end;

// AttachTexture
//

procedure TVKFramebufferHandle.AttachTexture(target: TGLenum; attachment:
  TGLenum; texture: TGLuint; level: TGLint);
begin
  GL.FramebufferTexture(target, attachment, texture, level);
end;

// AttachTextureLayer
//

procedure TVKFramebufferHandle.AttachTextureLayer(target: TGLenum; attachment:
  TGLenum; texture: TGLuint; level: TGLint; layer: TGLint);
begin
  GL.FramebufferTextureLayer(target, attachment, texture, level, layer);
end;

// Blit
//

procedure TVKFramebufferHandle.Blit(srcX0: TGLint; srcY0: TGLint; srcX1: TGLint;
  srcY1: TGLint;
  dstX0: TGLint; dstY0: TGLint; dstX1: TGLint; dstY1: TGLint;
  mask: TGLbitfield; filter: TGLenum);
begin
  GL.BlitFramebuffer(srcX0, srcY0, srcX1, srcY1, dstX0, dstY0, dstX1, dstY1,
    mask, filter);
end;

// GetAttachmentParameter
//

function TVKFramebufferHandle.GetAttachmentParameter(target: TGLenum;
  attachment: TGLenum; pname: TGLenum): TGLint;
begin
  GL.GetFramebufferAttachmentParameteriv(target, attachment, pname, @Result)
end;

// GetAttachmentObjectType
//

function TVKFramebufferHandle.GetAttachmentObjectType(target: TGLenum;
  attachment: TGLenum): TGLint;
begin
  GL.GetFramebufferAttachmentParameteriv(target, attachment,
    GL_FRAMEBUFFER_ATTACHMENT_OBJECT_TYPE, @Result);
end;

// GetAttachmentObjectName
//

function TVKFramebufferHandle.GetAttachmentObjectName(target: TGLenum;
  attachment: TGLenum): TGLint;
begin
  GL.GetFramebufferAttachmentParameteriv(target, attachment,
    GL_FRAMEBUFFER_ATTACHMENT_OBJECT_NAME, @Result);
end;

// CheckStatus
//

function TVKFramebufferHandle.GetStatus: TVKFramebufferStatus;
var
  Status: cardinal;
begin
  Status := GL.CheckFramebufferStatus(GL_FRAMEBUFFER);

  case Status of
    GL_FRAMEBUFFER_COMPLETE_EXT: Result := fsComplete;
    GL_FRAMEBUFFER_INCOMPLETE_ATTACHMENT_EXT: Result := fsIncompleteAttachment;
    GL_FRAMEBUFFER_INCOMPLETE_MISSING_ATTACHMENT_EXT: Result :=
      fsIncompleteMissingAttachment;
    GL_FRAMEBUFFER_INCOMPLETE_DUPLICATE_ATTACHMENT_EXT: Result :=
      fsIncompleteDuplicateAttachment;
    GL_FRAMEBUFFER_INCOMPLETE_DIMENSIONS_EXT: Result := fsIncompleteDimensions;
    GL_FRAMEBUFFER_INCOMPLETE_FORMATS_EXT: Result := fsIncompleteFormats;
    GL_FRAMEBUFFER_INCOMPLETE_DRAW_BUFFER_EXT: Result := fsIncompleteDrawBuffer;
    GL_FRAMEBUFFER_INCOMPLETE_READ_BUFFER_EXT: Result := fsIncompleteReadBuffer;
    GL_FRAMEBUFFER_UNSUPPORTED_EXT: Result := fsUnsupported;
    GL_FRAMEBUFFER_INCOMPLETE_MULTISAMPLE: Result := fsIncompleteMultisample;
  else
    Result := fsStatusError;
  end;
end;

function TVKFramebufferHandle.GetStringStatus(out clarification: string):
  TVKFramebufferStatus;
const
  cFBOStatus: array[TVKFramebufferStatus] of string = (
    'Complete',
    'Incomplete attachment',
    'Incomplete missing attachment',
    'Incomplete duplicate attachment',
    'Incomplete dimensions',
    'Incomplete formats',
    'Incomplete draw buffer',
    'Incomplete read buffer',
    'Unsupported',
    'Incomplite multisample',
    'Status Error');
begin
  Result := GetStatus;
  clarification := cFBOStatus[Result];
end;

// IsSupported
//

class function TVKFramebufferHandle.IsSupported: Boolean;
begin
  Result := GL.EXT_framebuffer_object or GL.ARB_framebuffer_object;
end;

// Transferable
//

class function TVKFramebufferHandle.Transferable: Boolean;
begin
  Result := False;
end;

// ------------------
// ------------------ TVKRenderbufferObject ------------------
// ------------------

// DoAllocateHandle
//

function TVKRenderbufferHandle.DoAllocateHandle: Cardinal;
begin
  Result := 0;
  GL.GenRenderbuffers(1, @Result);
end;

// DoDestroyHandle
//

procedure TVKRenderbufferHandle.DoDestroyHandle(var AHandle: TGLuint);
begin
  if not vContextActivationFailureOccurred then
  with GL do
  begin
    // reset error status
    GetError;
    // delete
    DeleteRenderbuffers(1, @AHandle);
    // check for error
    CheckError;
  end;
end;

// IsValid
//

class function TVKRenderbufferHandle.IsValid(const ID: GLuint): Boolean;
begin
  Result := GL.IsRenderbuffer(ID);
end;

// Bind
//

procedure TVKRenderbufferHandle.Bind;
begin
  vCurrentGLContext.GLStates.RenderBuffer := GetHandle;
end;

// UnBind
//

procedure TVKRenderbufferHandle.UnBind;
begin
  if vCurrentGLContext <> nil then
    vCurrentGLContext.GLStates.RenderBuffer := 0;
end;

// SetStorage
//

procedure TVKRenderbufferHandle.SetStorage(internalformat: TGLenum; width,
  height: TGLsizei);
begin
  GL.RenderbufferStorage(GL_RENDERBUFFER, internalformat, width, height);
end;

// SetStorageMultisample
//

procedure TVKRenderbufferHandle.SetStorageMultisample(internalformat: TGLenum;
  samples: TGLsizei; width, height: TGLsizei);
begin
  GL.RenderbufferStorageMultisample(GL_RENDERBUFFER, samples, internalformat,
    width, height);
end;

// IsSupported
//

class function TVKRenderbufferHandle.IsSupported: Boolean;
begin
  Result := GL.EXT_framebuffer_object or GL.ARB_framebuffer_object;
end;

// ------------------
// ------------------ TVKARBProgramHandle ------------------
// ------------------

// DoAllocateHandle
//

function TVKARBProgramHandle.DoAllocateHandle: Cardinal;
begin
  Result := 0;
  GL.GenPrograms(1, @Result);
  FReady := False;
end;

// DoDestroyHandle
//

procedure TVKARBProgramHandle.DoDestroyHandle(var AHandle: TGLuint);
begin
  if not vContextActivationFailureOccurred then
  with GL do
  begin
    // reset error status
    GetError;
    // delete
    DeletePrograms(1, @AHandle);
    // check for error
    CheckError;
  end;
end;

// IsValid
//

class function TVKARBProgramHandle.IsValid(const ID: GLuint): Boolean;
begin
  Result := GL.IsProgram(ID);
end;

procedure TVKARBProgramHandle.LoadARBProgram(AText: string);
const
  cProgType: array[0..2] of string =
    ('ARB vertex', 'ARB fragment', 'NV geometry');
var
  errPos, P: Integer;
begin
  Bind;
  GL.ProgramString(GetTarget, GL_PROGRAM_FORMAT_ASCII_ARB,
    Length(AText), PGLChar(TGLString(AText)));
  GL.GetIntegerv(GL_PROGRAM_ERROR_POSITION_ARB, @errPos);
  if errPos > -1 then
  begin
    FInfoLog := string(GL.GetString(GL_PROGRAM_ERROR_STRING_ARB));
    case GetTarget of
      GL_VERTEX_PROGRAM_ARB: P := 0;
      GL_FRAGMENT_PROGRAM_ARB: P := 1;
    else
      P := 2;
    end;
///    GLSLogger.LogError(Format('%s Program Error - [Pos: %d][Error %s]', [cProgType[P], errPos, FInfoLog]));
    FReady := False;
  end
  else
  begin
    FReady := True;
    FInfoLog := '';
  end;
end;

procedure TVKARBProgramHandle.Enable;
begin
  if FReady then
    GL.Enable(GetTarget)
  else
    Abort;
end;

procedure TVKARBProgramHandle.Disable;
begin
  GL.Disable(GetTarget);
end;

procedure TVKARBProgramHandle.Bind;
begin
  GL.BindProgram(GetTarget, Handle);
end;

class function TVKARBVertexProgramHandle.GetTarget: TGLenum;
begin
  Result := GL_VERTEX_PROGRAM_ARB;
end;

class function TVKARBVertexProgramHandle.IsSupported: Boolean;
begin
  Result := GL.ARB_vertex_program;
end;

class function TVKARBFragmentProgramHandle.GetTarget: TGLenum;
begin
  Result := GL_FRAGMENT_PROGRAM_ARB;
end;

class function TVKARBFragmentProgramHandle.IsSupported: Boolean;
begin
  Result := GL.ARB_vertex_program;
end;

class function TVKARBGeometryProgramHandle.GetTarget: TGLenum;
begin
  Result := GL_GEOMETRY_PROGRAM_NV;
end;

class function TVKARBGeometryProgramHandle.IsSupported: Boolean;
begin
  Result := GL.NV_geometry_program4;
end;

// ------------------
// ------------------ TVKSLHandle ------------------
// ------------------

procedure TVKSLHandle.DoDestroyHandle(var AHandle: TGLuint);
begin
  if not vContextActivationFailureOccurred then
  with GL do
  begin
    // reset error status
    ClearError;
    // delete
    DeleteObject(AHandle);
    // check for error
    CheckError;
  end;
end;

// InfoLog
//

function TVKSLHandle.InfoLog: string;
var
  maxLength: Integer;
  log: TGLString;
begin
  maxLength := 0;
  GL.GetObjectParameteriv(GetHandle, GL_OBJECT_INFO_LOG_LENGTH_ARB, @maxLength);
  SetLength(log, maxLength);
  if maxLength > 0 then
  begin
    GL.GetInfoLog(GetHandle, maxLength, @maxLength, @log[1]);
    SetLength(log, maxLength);
  end;
  Result := string(log);
end;

// IsSupported
//

class function TVKSLHandle.IsSupported: Boolean;
begin
  Result := GL.ARB_shader_objects;
end;

// ------------------
// ------------------ TVKShaderHandle ------------------
// ------------------

// DoAllocateHandle
//

function TVKShaderHandle.DoAllocateHandle: Cardinal;
begin
  Result := GL.CreateShader(FShaderType)
end;

// IsValid
//

class function TVKShaderHandle.IsValid(const ID: GLuint): Boolean;
begin
  Result := GL.IsShader(ID);
end;

// ShaderSource
//

procedure TVKShaderHandle.ShaderSource(const source: AnsiString);
var
  p: PGLChar;
begin
  p := PGLChar(TGLString(source));
  GL.ShaderSource(GetHandle, 1, @p, nil);
end;

// CompileShader
//

function TVKShaderHandle.CompileShader: Boolean;
var
  compiled: Integer;
  glH: TGLuint;
begin
  glH := GetHandle;
  GL.CompileShader(glH);
  compiled := 0;
  GL.GetShaderiv(glH, GL_COMPILE_STATUS, @compiled);
  Result := (compiled <> 0);
end;

// ------------------
// ------------------ TVKVertexShaderHandle ------------------
// ------------------

// Create
//

constructor TVKVertexShaderHandle.Create;
begin
  FShaderType := GL_VERTEX_SHADER_ARB;
  inherited;
end;

// IsSupported
//

class function TVKVertexShaderHandle.IsSupported: Boolean;
begin
  Result := GL.ARB_vertex_shader;
end;

// ------------------
// ------------------ TVKGeometryShaderHandle ------------------
// ------------------

// Create
//

constructor TVKGeometryShaderHandle.Create;
begin
  FShaderType := GL_GEOMETRY_SHADER_EXT;
  inherited;
end;

// IsSupported
//

class function TVKGeometryShaderHandle.IsSupported: Boolean;
begin
  Result := GL.EXT_geometry_shader4;
end;

// ------------------
// ------------------ TVKFragmentShaderHandle ------------------
// ------------------

// Create
//

constructor TVKFragmentShaderHandle.Create;
begin
  FShaderType := GL_FRAGMENT_SHADER_ARB;
  inherited;
end;

// IsSupported
//

class function TVKFragmentShaderHandle.IsSupported: Boolean;
begin
  Result := GL.ARB_fragment_shader;
end;

// ------------------
// ------------------ TVKTessControlShaderHandle ------------------
// ------------------

// Create
//

constructor TVKTessControlShaderHandle.Create;
begin
  FShaderType := GL_TESS_CONTROL_SHADER;
  inherited;
end;

// IsSupported
//

class function TVKTessControlShaderHandle.IsSupported: Boolean;
begin
  Result := GL.ARB_tessellation_shader;
end;

// ------------------
// ------------------ TVKTessEvaluationShaderHandle ------------------
// ------------------

// Create
//

constructor TVKTessEvaluationShaderHandle.Create;
begin
  FShaderType := GL_TESS_EVALUATION_SHADER;
  inherited;
end;

// IsSupported
//

class function TVKTessEvaluationShaderHandle.IsSupported: Boolean;
begin
  Result := GL.ARB_tessellation_shader;
end;

// ------------------
// ------------------ TVKProgramHandle ------------------
// ------------------

// DoAllocateHandle
//

function TVKProgramHandle.DoAllocateHandle: cardinal;
begin
  Result := GL.CreateProgram();
end;

// IsValid
//

class function TVKProgramHandle.IsValid(const ID: GLuint): Boolean;
begin
  Result := GL.IsProgram(ID);
end;

// AddShader
//

procedure TVKProgramHandle.AddShader(shaderType: TVKShaderHandleClass; const
  shaderSource: string;
  treatWarningsAsErrors: Boolean = False);
var
  shader: TVKShaderHandle;
begin
  shader := shaderType.CreateAndAllocate;
  try
    if shader.Handle = 0 then
      raise EGLShader.Create('Couldn''t allocate ' + shaderType.ClassName);
    shader.ShaderSource(AnsiString(shaderSource));
    if (not shader.CompileShader)
      or (treatWarningsAsErrors and (Pos('warning', LowerCase(shader.InfoLog)) >
      0)) then
      raise EGLShader.Create(FName + ' (' + shader.ClassName + '): '#13#10 +
        shader.InfoLog);
    AttachObject(shader);
  finally
    shader.Free;
  end;
  GL.CheckError;
end;

// AttachObject
//

procedure TVKProgramHandle.AttachObject(shader: TVKShaderHandle);
begin
  GL.AttachShader(GetHandle, shader.Handle);
end;

// DetachAllObject
//

procedure TVKProgramHandle.DetachAllObject;
var
  glH: TGLuint;
  I: Integer;
  count: GLSizei;
  buffer: array[0..255] of TGLuint;
begin
  glH := GetHandle;
  if glH > 0 then
  begin
    GL.GetAttachedShaders(glH, Length(buffer), @count, @buffer[0]);
    count := MinInteger(count, Length(buffer));
    for I := 0 to count - 1 do
      GL.DetachShader(glH, buffer[I]);
    NotifyChangesOfData;
  end;
end;

// BindAttribLocation
//

procedure TVKProgramHandle.BindAttribLocation(index: Integer; const aName:
  string);
begin
  GL.BindAttribLocation(GetHandle, index, PGLChar(TGLString(aName)));
end;

// BindFragDataLocation
//

procedure TVKProgramHandle.BindFragDataLocation(index: Integer; const aName:
  string);
begin
  GL.BindFragDataLocation(GetHandle, index, PGLChar(TGLString(name)));
end;

// LinkProgram
//

function TVKProgramHandle.LinkProgram: Boolean;
var
  status: Integer;
  glH: TGLuint;
begin
  glH := GetHandle;
  GL.LinkProgram(glH);
  status := 0;
  GL.GetProgramiv(glH, GL_LINK_STATUS, @status);
  Result := (status <> 0);
end;

// ValidateProgram
//

function TVKProgramHandle.ValidateProgram: Boolean;
var
  validated: Integer;
  h: TGLuint;
begin
  h := GetHandle;
  GL.ValidateProgram(h);
  validated := 0;
  GL.GetProgramiv(h, GL_VALIDATE_STATUS, @validated);
  Result := (validated <> 0);
end;

// GetAttribLocation
//

function TVKProgramHandle.GetAttribLocation(const aName: string): Integer;
begin
  Result := GL.GetAttribLocation(GetHandle, PGLChar(TGLString(aName)));
  Assert(Result >= 0, Format(vksUnknownParam, ['attrib', aName, Name]));
end;

// GetUniformLocation
//

function TVKProgramHandle.GetUniformLocation(const aName: string): Integer;
begin
  Result := GL.GetUniformLocation(GetHandle, PGLChar(TGLString(aName)));
  Assert(Result >= 0, Format(vksUnknownParam, ['uniform', aName, Name]));
end;

// GetVaryingLocation
//

function TVKProgramHandle.GetVaryingLocation(const aName: string): Integer;
begin
  Result := GL.GetVaryingLocation(GetHandle, PGLChar(TGLString(aName)));
  Assert(Result >= 0, Format(vksUnknownParam, ['varying', aName, Name]));
end;

// AddActiveVarying
//

procedure TVKProgramHandle.AddActiveVarying(const aName: string);
begin
  GL.ActiveVarying(GetHandle, PGLChar(TGLString(aName)));
end;

// GetAttribLocation
//

procedure TVKProgramHandle.UseProgramObject;
begin
  Assert(vCurrentGLContext <> nil);
  vCurrentGLContext.GLStates.CurrentProgram := Handle;
end;

// GetAttribLocation
//

procedure TVKProgramHandle.EndUseProgramObject;
begin
  Assert(vCurrentGLContext <> nil);
  vCurrentGLContext.GLStates.CurrentProgram := 0;
end;

// GetUniform1i
//

function TVKProgramHandle.GetUniform1i(const index: string): Integer;
begin
  GL.GetUniformiv(GetHandle, GetUniformLocation(index), @Result);
end;

// GetUniform2i
//

function TVKProgramHandle.GetUniform2i(const index: string): TVector2i;
begin
  GL.GetUniformiv(GetHandle, GetUniformLocation(index), @Result);
end;

// GetUniform3i
//

function TVKProgramHandle.GetUniform3i(const index: string): TVector3i;
begin
  GL.GetUniformiv(GetHandle, GetUniformLocation(index), @Result);
end;

// GetUniform4i
//

function TVKProgramHandle.GetUniform4i(const index: string): TVector4i;
begin
  GL.GetUniformiv(GetHandle, GetUniformLocation(index), @Result);
end;

// SetUniform1f
//

procedure TVKProgramHandle.SetUniform1f(const index: string; val: Single);
begin
  GL.Uniform1f(GetUniformLocation(index), val);
end;

// GetUniform1f
//

function TVKProgramHandle.GetUniform1f(const index: string): Single;
begin
  GL.GetUniformfv(GetHandle, GetUniformLocation(index), @Result);
end;

// SetUniform1i
//

procedure TVKProgramHandle.SetUniform1i(const index: string; val: Integer);
begin
  GL.Uniform1i(GetUniformLocation(index), val);
end;

// SetUniform2i
//

procedure TVKProgramHandle.SetUniform2i(const index: string;
  const Value: TVector2i);
begin
  GL.Uniform2i(GetUniformLocation(index), Value.V[0], Value.V[1]);
end;

// SetUniform3i
//

procedure TVKProgramHandle.SetUniform3i(const index: string;
  const Value: TVector3i);
begin
  GL.Uniform3i(GetUniformLocation(index), Value.V[0], Value.V[1], Value.V[2]);
end;

// SetUniform4i
//

procedure TVKProgramHandle.SetUniform4i(const index: string;
  const Value: TVector4i);
begin
  GL.Uniform4i(GetUniformLocation(index), Value.V[0], Value.V[1], Value.V[2],
    Value.V[3]);
end;

// GetUniform2f
//

function TVKProgramHandle.GetUniform2f(const index: string): TVector2f;
begin
  GL.GetUniformfv(GetHandle, GetUniformLocation(index), @Result);
end;

// SetUniform2f
//

procedure TVKProgramHandle.SetUniform2f(const index: string; const val:
  TVector2f);
begin
  GL.Uniform2f(GetUniformLocation(index), val.V[0], val.V[1]);
end;

// GetUniform3f
//

function TVKProgramHandle.GetUniform3f(const index: string): TAffineVector;
begin
  GL.GetUniformfv(GetHandle, GetUniformLocation(index), @Result);
end;

// SetUniform3f
//

procedure TVKProgramHandle.SetUniform3f(const index: string; const val:
  TAffineVector);
begin
  GL.Uniform3f(GetUniformLocation(index), val.V[0], val.V[1], val.V[2]);
end;

// GetUniform4f
//

function TVKProgramHandle.GetUniform4f(const index: string): TVector;
begin
  GL.GetUniformfv(GetHandle, GetUniformLocation(index), @Result);
end;

// SetUniform4f
//

procedure TVKProgramHandle.SetUniform4f(const index: string; const val:
  TVector);
begin
  GL.Uniform4f(GetUniformLocation(index), val.V[0], val.V[1], val.V[2], val.V[3]);
end;

// GetUniformMatrix2fv
//

function TVKProgramHandle.GetUniformMatrix2fv(const index: string): TMatrix2f;
begin
  GL.GetUniformfv(GetHandle, GetUniformLocation(index), @Result);
end;

// SetUniformMatrix2fv
//

procedure TVKProgramHandle.SetUniformMatrix2fv(const index: string; const val:
  TMatrix2f);
begin
  GL.UniformMatrix2fv(GetUniformLocation(index), 1, False, @val);
end;

// GetUniformMatrix3fv
//

function TVKProgramHandle.GetUniformMatrix3fv(const index: string): TMatrix3f;
begin
  GL.GetUniformfv(GetHandle, GetUniformLocation(index), @Result);
end;

// SetUniformMatrix3fv
//

procedure TVKProgramHandle.SetUniformMatrix3fv(const index: string; const val:
  TMatrix3f);
begin
  GL.UniformMatrix3fv(GetUniformLocation(index), 1, False, @val);
end;

// GetUniformMatrix4fv
//

function TVKProgramHandle.GetUniformMatrix4fv(const index: string): TMatrix;
begin
  GL.GetUniformfv(GetHandle, GetUniformLocation(index), @Result);
end;

// SetUniformMatrix4fv
//

procedure TVKProgramHandle.SetUniformMatrix4fv(const index: string; const val:
  TMatrix);
begin
  GL.UniformMatrix4fv(GetUniformLocation(index), 1, False, @val);
end;

// SetUniformf
//

procedure TVKProgramHandle.SetUniformf(const index: string;
  const val: single);
begin
  SetUniform1f(index, val);
end;

// SetUniformf
//

procedure TVKProgramHandle.SetUniformf(const index: string; const val:
  TVector2f);
begin
  SetUniform2f(index, val);
end;

// SetUniformf
//

procedure TVKProgramHandle.SetUniformf(const index: string;
  const val: TVector3f);
begin
  SetUniform3f(index, val);
end;

// SetUniformf
//

procedure TVKProgramHandle.SetUniformf(const index: string;
  const val: TVector4f);
begin
  SetUniform4f(index, val);
end;

// SetUniformf
//

procedure TVKProgramHandle.SetUniformi(const index: string;
  const val: integer);
begin
  SetUniform1f(index, val);
end;

// SetUniformf
//

procedure TVKProgramHandle.SetUniformi(const index: string; const val:
  TVector2i);
begin
  SetUniform2i(index, val);
end;

// SetUniformf
//

procedure TVKProgramHandle.SetUniformi(const index: string;
  const val: TVector3i);
begin
  SetUniform3i(index, val);
end;

// SetUniformf
//

procedure TVKProgramHandle.SetUniformi(const index: string;
  const val: TVector4i);
begin
  SetUniform4i(index, val);
end;

// GetUniformTextureHandle
//

function TVKProgramHandle.GetUniformTextureHandle(const index: string;
  const TextureIndex: Integer; const TextureTarget: TVKTextureTarget): Cardinal;
begin
  Result := GetUniform1i(index);
end;

// SetUniformTextureHandle
//

procedure TVKProgramHandle.SetUniformTextureHandle(const index: string;
  const TextureIndex: Integer; const TextureTarget: TVKTextureTarget;
  const Value: Cardinal);
begin
  vCurrentGLContext.GLStates.TextureBinding[0, TextureTarget] := Value;
  SetUniform1i(index, TextureIndex);
end;

// SetUniformBuffer
//

procedure TVKProgramHandle.SetUniformBuffer(const index: string;
  Value: TVKUniformBufferHandle);
begin
  GL.UniformBuffer(Handle, GetUniformLocation(index), Value.Handle);
end;

// GetUniformBufferSize
//

function TVKProgramHandle.GetUniformBufferSize(const aName: string): Integer;
begin
  Result := GL.GetUniformBufferSize(Handle, GetUniformLocation(aName));
end;

// GetUniformOffset
//

function TVKProgramHandle.GetUniformOffset(const aName: string): PGLInt;
begin
  Result := GL.GetUniformOffset(Handle, GetUniformLocation(aName));
end;

// GetUniformBlockIndex
//

function TVKProgramHandle.GetUniformBlockIndex(const aName: string): Integer;
begin
  Result := GL.GetUniformBlockIndex(Handle, PGLChar(TGLString(aName)));
  Assert(Result >= 0, Format(vksUnknownParam, ['uniform block', aName, Name]));
end;

// Create
//

constructor TVKProgramHandle.Create;
begin
  inherited Create;
  FName := 'DefaultShaderName';
end;

// ------------------
// ------------------ TVKContextManager ------------------
// ------------------

{$IFDEF VKS_SERVICE_CONTEXT}
procedure OnApplicationInitialize;
begin
  InitProc := OldInitProc;
  Application.Initialize;
  GLContextManager.CreateServiceContext;
end;
{$ENDIF}

// Create
//

constructor TVKContextManager.Create;
begin
  inherited Create;
{$IFNDEF VKS_MULTITHREAD}
  FHandles := TList.Create;
{$ELSE}
  FHandles := TThreadList.Create;
{$ENDIF VKS_MULTITHREAD}
  FList := TThreadList.Create;
end;

// Destroy
//

destructor TVKContextManager.Destroy;
begin
  FHandles.Free;
  FList.Free;
  inherited Destroy;
end;

// CreateContext
//

function TVKContextManager.CreateContext(AClass: TVKContextClass): TVKContext;
begin
  if Assigned(AClass) then
  begin
    Result := AClass.Create;
    Result.FManager := Self;
  end
  else if Assigned(vContextClasses) and (vContextClasses.Count > 0) then
  begin
    Result := TVKContextClass(vContextClasses.Last).Create;
    Result.FManager := Self;
  end
  else
    Result := nil;
end;

{$IFDEF VKS_SERVICE_CONTEXT}

procedure TVKContextManager.CreateServiceContext;
begin
  FServiceContext := CreateContext;
  FThreadTask := TServiceContextTaskList.Create;
  FServiceStarter := TFinishTaskEvent.Create;
  FThread := TServiceContextThread.Create;
  AddTaskForServiceContext(TServiceContextThread(FThread).DoCreateServiceContext);
end;

procedure TVKContextManager.QueueTaskDepleted;
var
  TaskRec: TServiceContextTask;
  I: Integer;
  nowTime: Double;
begin
  with FThreadTask.LockList do
    try
      for I := 0 to Count - 1 do
      begin
        TaskRec := Items[I];
        if Assigned(TaskRec.Task) then
        begin
          FThreadTask.UnlockList;
          // Task queue not empty
          FServiceStarter.SetEvent;
          exit;
        end;
      end;
    finally
      FThreadTask.UnlockList;
    end;

  FServiceStarter.ResetEvent;
  FThreadTask.Clear;
  nowTime := GLSTime;
  with TServiceContextThread(FThread) do
  if (nowTime - FLastTaskStartTime > 30000)
    and not FReported then
  begin
    FReported := True;
    GLS.Log.GLSLogger.LogInfo('Service context queue task depleted');
  end;
end;

{$ENDIF VKS_SERVICE_CONTEXT}


// Lock
//

procedure TVKContextManager.Lock;
begin
  FList.LockList;
end;

procedure TVKContextManager.NotifyPreparationNeed;
var
  I: Integer;
  LList: TList;
begin
  LList := FList.LockList;
  try
    for I := LList.Count - 1 downto 0 do
      TVKContext(LList[I]).FIsPraparationNeed := True;
  finally
    FList.UnlockList;
  end;
end;

// UnLock
//

procedure TVKContextManager.UnLock;
begin
  FList.UnlockList;
end;

// ContextCount
//

function TVKContextManager.ContextCount: Integer;
begin
  // try..finally just a waste of CPU here, if Count fails, the list is amok,
  // and so is the lock...
  Result := FList.LockList.Count;
  FList.UnLockList;
end;

// RegisterContext
//

procedure TVKContextManager.RegisterContext(aContext: TVKContext);
begin
  with FList.LockList do
    try
      if IndexOf(aContext) >= 0 then
        raise EGLContext.Create(cInvalidContextRegistration)
      else
        Add(aContext);
    finally
      FList.UnlockList;
    end;
end;

// UnRegisterContext
//

procedure TVKContextManager.UnRegisterContext(aContext: TVKContext);
begin
  with FList.LockList do
    try
      if IndexOf(aContext) < 0 then
        raise EGLContext.Create(cInvalidContextRegistration)
      else
        Remove(aContext);
    finally
      FList.UnlockList;
    end;
end;

// ContextCreatedBy
//

procedure TVKContextManager.ContextCreatedBy(aContext: TVKContext);
begin
  Lock;
  try
    Inc(FCreatedRCCount);
  finally
    UnLock;
  end;
end;

// DestroyingContextBy
//

procedure TVKContextManager.DestroyingContextBy(aContext: TVKContext);
var
  cn: TVKContextNotification;
begin
  Lock;
  try
    Dec(FCreatedRCCount);
    if FCreatedRCCount = 0 then
    begin
      // yes, slow and bulky, but allows for the triggered event to
      // cascade-remove notifications safely
      while Length(FNotifications) > 0 do
      begin
        cn := FNotifications[High(FNotifications)];
        SetLength(FNotifications, Length(FNotifications) - 1);
        cn.event(cn.obj);
      end;
    end;
  finally
    UnLock;
  end;
end;

// LastContextDestroyNotification
//

procedure TVKContextManager.LastContextDestroyNotification(
  anObject: TObject; anEvent: TNotifyEvent);
begin
  Lock;
  try
    SetLength(FNotifications, Length(FNotifications) + 1);
    with FNotifications[High(FNotifications)] do
    begin
      obj := anObject;
      event := anEvent;
    end;
  finally
    UnLock;
  end;
end;

// RemoveNotification
//

procedure TVKContextManager.RemoveNotification(anObject: TObject);
var
  i: Integer;
  found: Boolean;
begin
  Lock;
  try
    found := False;
    i := Low(FNotifications);
    while i <= High(FNotifications) do
    begin
      if FNotifications[i].obj = anObject then
      begin
        found := True;
        while i <= High(FNotifications) do
        begin
          FNotifications[i] := FNotifications[i + 1];
          Inc(i);
        end;
        SetLength(FNotifications, Length(FNotifications) - 1);
        Break;
      end;
      Inc(i);
    end;
    if not found then
      raise EGLContext.Create(cInvalidNotificationRemoval);
  finally
    UnLock;
  end;
end;

// Terminate
//

procedure TVKContextManager.Terminate;
begin
  FTerminated := True;
{$IFDEF VKS_SERVICE_CONTEXT}
  // Sevice context may not be created becouse Application.Initialize not happened
  if Assigned(FServiceContext) then
  begin
    CheckSynchronize;
    FThread.Terminate;
    FServiceStarter.SetEvent;
    FThread.WaitFor;
    FThread.Destroy;
    GLSLogger.LogDebug('Service thread destroyed');
    FServiceStarter.Destroy;
    FThreadTask.Destroy;
  end;
{$ENDIF}
  if ContextCount = 0 then
  begin
    GLContextManager := nil;
    Free;
  end;
end;

// DestroyAllHandles
//

procedure TVKContextManager.DestroyAllHandles;
var
  i: Integer;
begin
  with FList.LockList do
    try
      for i := Count - 1 downto 0 do
        TVKContext(Items[i]).DestroyAllHandles;
    finally
      FList.UnLockList;
    end;
end;

{$IFDEF VKS_SERVICE_CONTEXT}

{$REGION 'TServiceContextThread'}

constructor TServiceContextThread.Create;
begin
  FWindow := TForm.CreateNew(nil);
  FWindow.Hide;
  FWindow.Position := TFormPosition.ScreenCenter;
  FWindow.Width := 1;
  FWindow.Height := 1;
  FWindow.BorderStyle := TFmxFormBorderStyle.None;
  FWindow.FormStyle := TFormStyle.StayOnTop;
  FWindow.Fill.Color := 0;
  vServiceWindow := FWindow;
{$IFDEF MSWINDOWS}
  { TODO -oPW : E2010 Incompatible types: 'NativeUInt' and 'TWindowHandle' }
  (*FDC := GetDeviceContext(FWindow.Handle);*)
  {$ENDIF}
{$IFDEF LINUX}
  FDC := FWindow.Handle;
{$ENDIF}
  inherited Create(False);
end;

destructor TServiceContextThread.Destroy;
begin
  FWindow.Handle.Free; //VCL -> ReleaseDC(FWindow.Handle, FDC);
  FWindow.Free;
  inherited;
end;

procedure TServiceContextThread.DoCreateServiceContext; stdcall;

  procedure Fail;
  begin
    GLSLogger.LogError(Format('%s: can''t initialize rendering context', [ClassName]));
    FWindow.Destroy;
    vServiceWindow := nil;
  end;

begin
  try
    GLContextManager.ServiceContext.Acceleration := chaHardware;
    GLContextManager.ServiceContext.CreateMemoryContext(FDC, 1, 1, 1);
  except
    on EGLContext do
    begin
      Fail;
      exit;
    end;
    on EPBuffer do
    begin
      GLSLogger.LogWarning(Format('%s: can''t initialize memory rendering context. Try initialize common context.', [ClassName]));
      try
        { TODO -oPW : E2250 There is no overloaded version of 'CreateContext' that can be called with these arguments }
        (*GLContextManager.ServiceContext.CreateContext(FDC);*)
      except
        Fail;
        exit;
      end;
    end;
  end;
  GLSLogger.LogNotice('Service context successfuly initialized');
  GLContextManager.ServiceContext.Activate;
  FWindow.Hide;
  vServiceWindow := nil;
end;

procedure TServiceContextThread.Execute;
var
  TaskRec: TServiceContextTask;

  procedure NextTask;
  const
    NullTask: TServiceContextTask = (Task: nil; Event: nil);
  var
    I: Integer;
  begin
    TaskRec.Task := nil;
    with GLContextManager.FThreadTask.LockList do
      try
        for I := 0 to Count - 1 do
        begin
          TaskRec := Items[I];
          if Assigned(TaskRec.Task) then
          begin
            Items[I] := NullTask;
            break;
          end;
        end;
      finally
        GLContextManager.FThreadTask.UnlockList;
      end;
  end;

begin
  with GLContextManager do
  begin
    vMainThread := False;
    GLSLogger.LogNotice('Service thread started');
    Sleep(100);
    try
      while not Terminated do
      begin
        NextTask;
        if Assigned(TaskRec.Task) then
        begin
          with GLContextManager.ServiceContext do
          begin
            if IsValid then
              Activate;
            try
              TaskRec.Task;
            except
              GLSLogger.LogError('Service thread task raised exception');
            end;
            if IsValid then
              Deactivate;
            if Assigned(TaskRec.Event) then
              TaskRec.Event.SetEvent;
          end;
         end
        else
          Synchronize(GLContextManager.QueueTaskDepleted);
        ServiceStarter.WaitFor(30000);
      end;
    finally
      ServiceContext.Destroy;
      FServiceContext := nil;
      GLSLogger.LogNotice('Service thread finished');
    end;
  end;
end;

procedure AddTaskForServiceContext(ATask: TTaskProcedure; FinishEvent: TFinishTaskEvent = nil);
var
  TaskRec: TServiceContextTask;
  rEvent: TFinishTaskEvent;
begin
  if vMainThread then
  begin
    rEvent := nil;
    if Assigned(GLContextManager.ServiceContext) and Assigned(ATask) then
    begin
      CheckSynchronize;
      with GLContextManager.FThreadTask.LockList do
        try
          TaskRec.Task := ATask;
          if FinishEvent = nil then
          begin // Synchronous call
            rEvent := TFinishTaskEvent.Create;
            TaskRec.Event := rEvent;
          end
          else  // Asynchronous call
            TaskRec.Event := FinishEvent;
          Add(TaskRec);
          with TServiceContextThread(GLContextManager.FThread) do
          begin
            FLastTaskStartTime := GLSTime;
            FReported := False;
          end;
        finally
          GLContextManager.FThreadTask.UnlockList;
        end;
      GLContextManager.ServiceStarter.SetEvent;
    end;
    // Wait task finishing
    if Assigned(rEvent) then
    begin
      rEvent.WaitFor(INFINITE);
      rEvent.Destroy;
      CheckSynchronize;
    end;
  end
  else
  begin // Direct task execution in service thread
    try
      ATask;
    except
      GLSLogger.LogError('Service thread task raised exception');
    end;
    if Assigned(FinishEvent) then
      FinishEvent.SetEvent;
  end;
end;

{$ENDIF VKS_SERVICE_CONTEXT}

constructor TFinishTaskEvent.Create;
begin
  inherited Create(nil, True, False, '');
end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

initialization
  // ------------------------------------------------------------------
  // ------------------------------------------------------------------
  // ------------------------------------------------------------------

  vMainThread := True;
{$IFDEF VKS_SERVICE_CONTEXT}
  OldInitProc := InitProc;
  InitProc := @OnApplicationInitialize;
{$ENDIF VKS_SERVICE_CONTEXT}
  GLContextManager := TVKContextManager.Create;
  GLwithoutContext := TGLExtensionsAndEntryPoints.Create;
  GLwithoutContext.Close;
  vLocalGL := @GL;

finalization

  GLContextManager.Terminate;
  vContextClasses.Free;
  vContextClasses := nil;
  GLwithoutContext.Free;
  GLwithoutContext := nil;

end.



