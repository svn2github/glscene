//
// VKScene Component Library, based on GLScene http://glscene.sourceforge.net
//
{
   Prototypes and base implementation of TVKContext.
   The history is logged in a former GLS version of the unit.
}
unit VKS.Context;

interface

{$I VKScene.inc}

uses
  Winapi.Windows,
  Winapi.OpenGL,
  Winapi.OpenGLext,
  System.Classes,
  System.SysUtils,
  System.Types,
  System.SyncObjs,
  System.StrUtils,
  FMX.Consts,
  FMX.Forms,
  FMX.Controls,
  FMX.Types,
  FMX.Dialogs,
  
  OpenGLAdapter,
  VKS.Generics,
  VKS.CrossPlatform,
  VKS.VectorGeometry,
  VKS.Strings,
  VKS.VectorTypes,
  VKS.State,
  VKS.PipelineTransformation,
  VKS.TextureFormat;

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

  TServiceContextTaskList = {$IFDEF VKS_GENERIC_PREFIX} specialize {$ENDIF}
    GThreadList < TServiceContextTask > ;

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
  { Wrapper around an Vulkan rendering context.
     The aim of this class is to offer platform-independant
     initialization, activation and management of Vulkan
     rendering context. The class also offers notifications
     event and error/problems detection.
     This is a virtual abstract a class, and platform-specific
     subclasses must be used.
     All rendering context share the same lists. }
  TVKContext = class
  private
    
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
    
    FVK: TVKExtensionsAndEntryPoints;
    FXVK: TAbstractMultitextureCoordinator;
    FVKStates: TVKStateCache;
    FTransformation: TVKTransformation;
    FAcceleration: TVKContextAcceleration;
    FLayer: TVKContextLayer;
    FSharedContexts: TThreadList;
    FLock: TCriticalSection;
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
    
    constructor Create; virtual;
    destructor Destroy; override;

    { An application-side cache of global per-context OpenGL states
       and parameters }
    property VKStates: TVKStateCache read FVKStates;

    property PipelineTransformation: TVKTransformation read FTransformation;

    // Context manager reference
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
       actually destroyed, Vulkan resource cleanup can
       still occur here. }
    property OnDestroyContext: TNotifyEvent read FOnDestroyContext write
      FOnDestroyContext;

    { Creates the context.
       This method must be invoked before the context can be used. }
    procedure CreateContext(ADeviceHandle: THandle); overload; //VCL -> HDC
    { Creates an in-memory context. 
       The function should fail if no hardware-accelerated memory context
       can be created (the CreateContext method can handle software Vulkan
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
    ///property GL: TVKExtensionsAndEntryPoints read FGL; depricated
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
    
    FWidth, FHeight: Integer;
    FFullScreen: Boolean;

  protected
    

  public
    
    property Width: Integer read FWidth write FWidth;
    property Height: Integer read FHeight write FHeight;
    property FullScreen: Boolean read FFullScreen write FFullScreen;
  end;

  PGLRCHandle = ^TVKRCHandle;
  TVKRCHandle = record
    FRenderingContext: TVKContext;
    FHandle: GLuint;
    FChanged: Boolean;
  end;

  TOnPrepareHandleData = procedure(AContext: TVKContext) of object;

  // TVKContextHandle
  //
  { Wrapper around an Vulkan context handle. 
     This wrapper also takes care of context registrations and data releases
     related to context releases an cleanups. This is an abstract class,
     use the TVKListHandle and TVKTextureHandle subclasses. }
  TVKContextHandle = class
  private
    
    FHandles: TList;
    FLastHandle : PGLRCHandle;
    FOnPrepare: TOnPrepareHandleData;
    function GetHandle: GLuint;
    function GetContext: TVKContext;
    function SearchRC(AContext: TVKContext): PGLRCHandle;
    function RCItem(AIndex: integer): PGLRCHandle; {$IFDEF VKS_INLINE}inline;{$ENDIF}
    procedure CheckCurrentRC;
  protected
    
    // Invoked by when there is no compatible context left for relocation
    procedure ContextDestroying;

    // Specifies if the handle can be transfered across shared contexts
    class function Transferable: Boolean; virtual;
    class function IsValid(const ID: GLuint): GLboolean; virtual;

    function DoAllocateHandle: GLuint; virtual; abstract;
    procedure DoDestroyHandle(var AHandle: GLuint); virtual; abstract;

  public
    
    constructor Create; virtual;
    constructor CreateAndAllocate(failIfAllocationFailed: Boolean = True);
    destructor Destroy; override;

    { Return OpenGL identifier in current context. }
    property Handle: GLuint read GetHandle;
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

    // Checks if required extensions / OpenGL version are met
    class function IsSupported: Boolean; virtual;
    function IsAllocatedForContext(AContext: TVKContext = nil): Boolean;
    function IsShared: Boolean;

    function  AllocateHandle: GLuint;
    procedure DestroyHandle;

    property OnPrapare: TOnPrepareHandleData read FOnPrepare write FOnPrepare;
  end;

  TVKVirtualHandle = class;
  TVKVirtualHandleEvent = procedure(Sender: TVKVirtualHandle; var Handle: GLuint) of object;

  // TVKVirtualHandle
  //
  { A context handle with event-based handle allocation and destruction. }
  TVKVirtualHandle = class(TVKContextHandle)
  private
    
    FOnAllocate,
    FOnDestroy: TVKVirtualHandleEvent;
    FTag: Integer;
  protected
    
    function DoAllocateHandle: GLuint; override;
    procedure DoDestroyHandle(var AHandle: GLuint); override;
    class function Transferable: Boolean; override;
  public
    
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
    

  protected
    
    function DoAllocateHandle: GLuint; override;
    procedure DoDestroyHandle(var AHandle: GLuint); override;
    class function IsValid(const ID: GLuint): GLboolean; override;
  public
    
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
    
    function DoAllocateHandle: GLuint; override;
    procedure DoDestroyHandle(var AHandle: GLuint); override;
    class function IsValid(const ID: GLuint): GLboolean; override;
  public
    
    property Target: TVKTextureTarget read FTarget write SetTarget;
  end;

  // TVKSamplerHandle
  //
  { Manages a handle to a sampler. }
  TVKSamplerHandle = class(TVKContextHandle)
  protected
    
    function DoAllocateHandle: GLuint; override;
    procedure DoDestroyHandle(var AHandle: GLuint); override;
    class function IsValid(const ID: GLuint): GLboolean; override;
  public
    
    class function IsSupported: Boolean; override;
  end;

  // TVKQueryHandle
  //
  { Manages a handle to a query. 
     Do not use this class directly, use one of its subclasses instead. }
  TVKQueryHandle = class(TVKContextHandle)
  private
    
    FActive: Boolean;
  protected
    
    class function Transferable: Boolean; override;
    function DoAllocateHandle: GLuint; override;
    procedure DoDestroyHandle(var AHandle: GLuint); override;
    function GetTarget: GLuint; virtual; abstract;
    function GetQueryType: TQueryType; virtual; abstract;
    class function IsValid(const ID: GLuint): GLboolean; override;
  public
    
    procedure BeginQuery;
    procedure EndQuery;

    // Check if result is available from the query.  Result may not be available
    // immediately after ending the query
    function IsResultAvailable: boolean;
    // Number of bits used to store the query result. eg. 32/64 bit
    function CounterBits: integer;
    // Retrieve query result, may cause a stall if the result is not available yet
    function QueryResultInt: GLint;
    function QueryResultUInt: GLuint;
    function QueryResultInt64: GLint64EXT;
    function QueryResultUInt64: GLuint64EXT;
    function QueryResultBool: GLboolean;

    property Target: GLuint read GetTarget;
    property QueryType: TQueryType read GetQueryType;

    { True if within a Begin/EndQuery. }
    property Active: Boolean read FActive;
  end;

  // TVKOcclusionQueryHandle
  //
  { Manages a handle to an occlusion query. 
     Requires Vulkan 1.5+ 
     Does *NOT* check for extension availability, this is assumed to have been
     checked by the user. }
  TVKOcclusionQueryHandle = class(TVKQueryHandle)
  protected
    function GetTarget: GLuint; override;
    function GetQueryType: TQueryType; override;
  public
    class function IsSupported: Boolean; override;
    // Number of samples (pixels) drawn during the query, some pixels may
    // be drawn to several times in the same query
    function PixelCount: Integer;
  end;

  TVKBooleanOcclusionQueryHandle = class(TVKQueryHandle)
  protected
    function GetTarget: GLuint; override;
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
    function GetTarget: GLuint; override;
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
     Requires Vulkan 3.0+ 
     Does *NOT* check for extension availability, this is assumed to have been
     checked by the user. }
  TVKPrimitiveQueryHandle = class(TVKQueryHandle)
  protected
    function GetTarget: GLuint; override;
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
    
    FSize: Integer;
  protected
    
    function DoAllocateHandle: GLuint; override;
    procedure DoDestroyHandle(var AHandle: GLuint); override;

    function GetTarget: GLuint; virtual; abstract;
    class function IsValid(const ID: GLuint): GLboolean; override;
  public
    
    { Creates the buffer object buffer and initializes it. }
    constructor CreateFromData(p: Pointer; size: Integer; bufferUsage: GLuint);

    procedure Bind; virtual; abstract;
    { Note that it is not necessary to UnBind before Binding another buffer. }
    procedure UnBind; virtual; abstract;

    { Bind a buffer object to an indexed target, used by transform feedback
       buffer objects and uniform buffer objects. (Vulkan 3.0+) }
    procedure BindRange(index: GLuint; offset: GLintptr; size: GLsizeiptr);
      virtual;
    { Equivalent to calling BindRange with offset = 0, and size = the size of buffer.}
    procedure BindBase(index: GLuint); virtual;
    procedure UnBindBase(index: GLuint); virtual;

    { Specifies buffer content. 
       Common bufferUsage values are GL_STATIC_DRAW_ARB for data that will
       change rarely, but be used often, GL_STREAM_DRAW_ARB for data specified
       once but used only a few times, and GL_DYNAMIC_DRAW_ARB for data
       that is re-specified very often. 
       Valid only if the buffer has been bound. }
    procedure BufferData(p: Pointer; size: Integer; bufferUsage: GLuint);
    // Invokes Bind then BufferData
    procedure BindBufferData(p: Pointer; size: Integer; bufferUsage: GLuint);
    { Updates part of an already existing buffer. 
       offset and size indicate which part of the data in the buffer is
       to bo modified and p where the data should be taken from. }
    procedure BufferSubData(offset, size: Integer; p: Pointer);
    { Map buffer content to memory. 
       Values for access are GL_READ_ONLY_ARB, GL_WRITE_ONLY_ARB and
       GL_READ_WRITE_ARB. 
       Valid only if the buffer has been bound, must be followed by
       an UnmapBuffer, only one buffer may be mapped at a time. }
    function MapBuffer(access: GLuint): Pointer;
    function MapBufferRange(offset: GLint; len: GLsizei; access: GLbitfield):
      Pointer;
    procedure Flush(offset: GLint; len: GLsizei);
    { Unmap buffer content from memory. 
       Must follow a MapBuffer, and happen before the buffer is unbound. }
    function UnmapBuffer: GLboolean;

    class function IsSupported: Boolean; override;

    property Target: GLuint read GetTarget;
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
    

    function GetVBOTarget: GLuint;
  public

    property VBOTarget: GLuint read GetVBOTarget;
  end;

  // TVKVBOArrayBufferHandle
  //
  { Manages a handle to VBO Array Buffer. 
     Typically used to store vertices, normals, texcoords, etc. }
  TVKVBOArrayBufferHandle = class(TVKVBOHandle)
  protected
    function GetTarget: GLuint; override;
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
    function GetTarget: GLuint; override;
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
    function GetTarget: GLuint; override;
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
    function GetTarget: GLuint; override;
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
    //    FTransformFeedbackBufferBuffer: array[0..15] of GLuint; // (0, 0, 0, ...)
    //    FTransformFeedbackBufferStart: array[0..15] of GLuint64; // (0, 0, 0, ...)
    //    FTransformFeedbackBufferSize: array[0..15] of GLuint64; // (0, 0, 0, ...)
  protected
    function GetTarget: GLuint; override;
  public
    procedure Bind; override;
    procedure UnBind; override;
    procedure BeginTransformFeedback(primitiveMode: GLEnum);
    procedure EndTransformFeedback();
    procedure BindRange(index: GLuint; offset: GLintptr; size: GLsizeiptr); override;
    procedure BindBase(index: GLuint); override;
    procedure UnBindBase(index: GLuint); override;

    class function IsSupported: Boolean; override;
  end;

  // TVKTextureBufferHandle
  //
  { Manages a handle to a Buffer Texture. (TBO) }
  TVKTextureBufferHandle = class(TVKBufferObjectHandle)
  protected
    function GetTarget: GLuint; override;
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
    //    FUniformBufferBuffer: array[0..15] of GLuint; // (0, 0, 0, ...)
    //    FUniformBufferStart: array[0..15] of GLuint64; // (0, 0, 0, ...)
    //    FUniformBufferSize: array[0..15] of GLuint64; // (0, 0, 0, ...)
  protected
    function GetTarget: GLuint; override;
  public
    procedure Bind; override;
    procedure UnBind; override;
    procedure BindRange(index: GLuint; offset: GLintptr; size: GLsizeiptr); override;
    procedure BindBase(index: GLuint); override;
    procedure UnBindBase(index: GLuint); override;
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
    function DoAllocateHandle: GLuint; override;
    procedure DoDestroyHandle(var AHandle: GLuint); override;
    class function IsValid(const ID: GLuint): GLboolean; override;
  public
    procedure Bind;
    procedure UnBind;
    class function IsSupported: Boolean; override;
  end;

  TVKFramebufferStatus = (
    fsComplete,
    fsIncompleteAttachment,
    fsIncompleteMissingAttachment,
    fsIncompleteDuplicateAttachment,
    fsIncompleteDimensions,
    fsIncompleteFormats,
    fsIncompleteDrawBuffer,
    fsIncompleteReadBuffer,
    fsUnsupported,
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
    function DoAllocateHandle: GLuint; override;
    procedure DoDestroyHandle(var AHandle: GLuint); override;
    class function IsValid(const ID: GLuint): GLboolean; override;
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
    procedure Attach1DTexture(target: GLEnum; attachment: GLEnum; textarget:
      GLEnum; texture: GLuint; level: GLint);
    procedure Attach2DTexture(target: GLEnum; attachment: GLEnum; textarget:
      GLEnum; texture: GLuint; level: GLint);
    procedure Attach3DTexture(target: GLEnum; attachment: GLEnum; textarget:
      GLEnum; texture: GLuint; level: GLint; layer: GLint);
    procedure AttachLayer(target: GLEnum; attachment: GLEnum; texture:
      GLuint; level: GLint; layer: GLint);
    procedure AttachRenderBuffer(target: GLEnum; attachment: GLEnum;
      renderbuffertarget: GLEnum; renderbuffer: GLuint);
    // Vulkan 3.2+ only.
    // If texture is the name of a three-dimensional texture, cube map texture, one-or
    // two-dimensional array texture, or two-dimensional multisample array texture, the
    // texture level attached to the framebuffer attachment point is an array of images,
    // and the framebuffer attachment is considered layered.
    procedure AttachTexture(target: GLEnum; attachment: GLEnum; texture:
      GLuint; level: GLint);
    // Vulkan 3.2+ only
    procedure AttachTextureLayer(target: GLEnum; attachment: GLEnum; texture:
      GLuint; level: GLint; layer: GLint);

    // copy rect from bound read framebuffer to bound draw framebuffer
    procedure Blit(srcX0: GLint; srcY0: GLint; srcX1: GLint; srcY1: GLint;
      dstX0: GLint; dstY0: GLint; dstX1: GLint; dstY1: GLint;
      mask: GLbitfield; filter: GLEnum);
    // target = GL_DRAW_FRAMEBUFFER, GL_READ_FRAMEBUFFER, GL_FRAMEBUFFER (equivalent to GL_DRAW_FRAMEBUFFER)
    // If default framebuffer (0) is bound:
    // attachment = GL_FRONT_LEFT, GL_FRONT_RIGHT, GL_BACK_LEFT, or GL_BACK_RIGHT, GL_DEPTH, GL_STENCIL
    // if a framebuffer object is bound:
    // attachment = GL_COLOR_ATTACHMENTi, GL_DEPTH_ATTACHMENT, GL_STENCIL_ATTACHMENT, GL_DEPTH_STENCIL_ATTACHMENT
    // param = GL_FRAMEBUFFER_ATTACHMENT_(OBJECT_TYPE, OBJECT_NAME,
    //       RED_SIZE, GREEN_SIZE, BLUE_SIZE, ALPHA_SIZE, DEPTH_SIZE, STENCIL_SIZE,
    //       COMPONENT_TYPE, COLOR_ENCODING, TEXTURE_LEVEL, LAYERED, TEXTURE_CUBE_MAP_FACE, TEXTURE_LAYER
    function GetAttachmentParameter(target: GLEnum; attachment: GLEnum; pname:
      GLEnum): GLint;
    // Returns the type of object bound to attachment point:
    // GL_NONE, GL_FRAMEBUFFER_DEFAULT, GL_TEXTURE, or GL_RENDERBUFFER
    function GetAttachmentObjectType(target: GLEnum; attachment: GLEnum):
      GLint;
    // Returns the name (ID) of the texture or renderbuffer attached to attachment point
    function GetAttachmentObjectName(target: GLEnum; attachment: GLEnum):
      GLint;

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
    function DoAllocateHandle: GLuint; override;
    procedure DoDestroyHandle(var AHandle: GLuint); override;
    class function IsValid(const ID: GLuint): GLboolean; override;
  public
    procedure Bind;
    procedure UnBind;
    procedure SetStorage(internalformat: GLEnum; width, height: GLsizei);
    procedure SetStorageMultisample(internalformat: GLEnum; samples: GLsizei;
      width, height: GLsizei);
    class function IsSupported: Boolean; override;
  end;

  TVKProgramHandleEXT = class(TVKContextHandle)
  private
    
    FReady: Boolean;
    FInfoLog: string;
  protected
    
    function DoAllocateHandle: GLuint; override;
    procedure DoDestroyHandle(var AHandle: GLuint); override;
    class function IsValid(const ID: GLuint): GLboolean; override;
    class function GetTarget: GLEnum; virtual; abstract;
  public
    
    procedure LoadARBProgram(AText: string);
    procedure Enable;
    procedure Disable;
    procedure Bind;
    property Ready: Boolean read FReady;
    property InfoLog: string read FInfoLog;
  end;

  TVKVertexProgramHandle = class(TVKProgramHandleEXT)
  protected
    
    class function GetTarget: GLEnum; override;
  public
    
    class function IsSupported: Boolean; override;
  end;

  TVKFragmentProgramHandle = class(TVKProgramHandleEXT)
  protected
    
    class function GetTarget: GLEnum; override;
  public
    
    class function IsSupported: Boolean; override;
  end;

  TVKGeometryProgramHandle = class(TVKProgramHandleEXT)
  protected
    
    class function GetTarget: GLEnum; override;
  public
    
    class function IsSupported: Boolean; override;
  end;

  // TVKSLHandle
  //
  { Base class for GLSL handles (programs and shaders).
     Do not use this class directly, use one of its subclasses instead. }
  TVKSLHandle = class(TVKContextHandle)
  private
    

  protected
    
    procedure DoDestroyHandle(var AHandle: GLuint); override;

  public
    
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
    
    FShaderType: Cardinal;

  protected
    
    function DoAllocateHandle: GLuint; override;
    class function IsValid(const ID: GLuint): GLboolean; override;
  public
    
    procedure ShaderSource(const source: AnsiString); overload;
    // Returns True if compilation sucessful
    function CompileShader: Boolean;

    property ShaderType: Cardinal read FShaderType;
  end;

  TVKShaderHandleClass = class of TVKShaderHandle;

  // TVKVertexShaderHandle
  //
  { Manages a handle to a Vertex Shader Object. }
  TVKVertexShaderHandle = class(TVKShaderHandle)
  public
    
    constructor Create; override;
    class function IsSupported: Boolean; override;
  end;

  // TVKGeometryShaderHandle
  //
  { Manages a handle to a Geometry Shader Object. }
  TVKGeometryShaderHandle = class(TVKShaderHandle)
  public
    
    constructor Create; override;
    class function IsSupported: Boolean; override;
  end;

  // TVKFragmentShaderHandle
  //
  { Manages a handle to a Fragment Shader Object. }
  TVKFragmentShaderHandle = class(TVKShaderHandle)
  public
    
    constructor Create; override;
    class function IsSupported: Boolean; override;
  end;

  // TVKTessControlShaderHandle
  //
  { Manages a handle to a Tessellation Control Shader Object. }
  TVKTessControlShaderHandle = class(TVKShaderHandle)
  public
    
    constructor Create; override;
    class function IsSupported: Boolean; override;
  end;

  // TVKTessEvaluationShaderHandle
  //
  { Manages a handle to a Tessellation Evaluation Shader Object. }
  TVKTessEvaluationShaderHandle = class(TVKShaderHandle)
  public
    
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
    class function IsValid(const ID: GLuint): GLboolean; override;
  private
    
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
    
    function DoAllocateHandle: GLuint; override;

  public
    
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
    function GetAttribLocation(const aName: string): GLInt;
    function GetUniformLocation(const aName: string): GLInt;
    function GetUniformOffset(const aName: string): GLintptr;
    function GetUniformBlockIndex(const aName: string): GLInt;

    function GetVaryingLocation(const aName: string): GLInt;
    // Currently, NVidia-specific.
    procedure AddActiveVarying(const aName: string);
    // Currently, NVidia-specific.

    function GetUniformBufferSize(const aName: string): GLInt;

    procedure UseProgramObject;
    procedure EndUseProgramObject;

    procedure SetUniformi(const index: string; const val: GLInt); overload;
    procedure SetUniformi(const index: string; const val: TVector2i); overload;
    procedure SetUniformi(const index: string; const val: TVector3i); overload;
    procedure SetUniformi(const index: string; const val: TVector4i); overload;

    procedure SetUniformf(const index: string; const val: single); overload;
    procedure SetUniformf(const index: string; const val: TVector2f); overload;
    procedure SetUniformf(const index: string; const val: TVector3f); overload;
    procedure SetUniformf(const index: string; const val: TVector4f); overload;

    { Shader parameters. }
    property Uniform1i[const index: string]: GLInt read GetUniform1i write
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
    Integer; const TextureTarget: TVKTextureTarget]: GLuint read
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
    
    FList: TThreadList;
    FTerminated: Boolean;
    FNotifications: array of TVKContextNotification;
    FCreatedRCCount: Integer;
    FHandles: TThreadList;
    FThread: TThread;
    FServiceStarter: TEvent;
    FThreadTask: TServiceContextTaskList;
    FServiceContext: TVKContext;
  protected
    
    procedure Lock;
    procedure UnLock;

    procedure RegisterContext(aContext: TVKContext);
    procedure UnRegisterContext(aContext: TVKContext);

    procedure ContextCreatedBy(aContext: TVKContext);
    procedure DestroyingContextBy(aContext: TVKContext);

      public
         
         constructor Create;
         destructor Destroy; override;

    { Returns an appropriate, ready-to use context. 
       The returned context should be freed by caller. }
    function CreateContext(AClass: TVKContextClass = nil): TVKContext;

    { Returns the number of TVKContext object. 
       This is *not* the number of Vulkan rendering contexts! }
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

    // Marks the context manager for termination
    procedure Terminate;

    { Request all contexts to destroy all their handles. }
    procedure DestroyAllHandles;

    { Notify all contexts about necessity of handles preparation. }
    procedure NotifyPreparationNeed;
  end;

  EVKContext = class(Exception);

  EPBuffer = class(Exception);

  EGLShader = class(EVKContext);

  { Drivers should register themselves via this function. }
procedure RegisterVKContextClass(aVKContextClass: TVKContextClass);
{ The TVKContext that is the currently active context, if any. 
   Returns nil if no context is active. }
function CurrentVKContext: TVKContext;
function SafeCurrentVKContext: TVKContext;
function IsMainThread: Boolean;
function IsServiceContextAvaible: Boolean;
function GetServiceWindow: TForm;

var
  VKContextManager: TVKContextManager;
  vIgnoreVulkanErrors: Boolean = False;
  vContextActivationFailureOccurred: Boolean = false;
  vMultitextureCoordinatorClass: TAbstractMultitextureCoordinatorClass;

  // ------------------------------------------------------------------
  // ------------------------------------------------------------------
  // ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

var
  vContextClasses: TList;
  GLwithoutContext: TVKExtensionsAndEntryPoints;
  vServiceWindow: TForm;

  vVK: TVKExtensionsAndEntryPoints;
  vCurrentVKContext: TVKContext;
  vMainThread: Boolean;

  // CurrentVKContext
  //

function CurrentVKContext: TVKContext;
begin
  Result := vCurrentVKContext;
end;

function SafeCurrentVKContext: TVKContext;
begin
  Result := CurrentVKContext;
  if not Assigned(Result) then
  begin
   {$IFDEF VKS_LOGGING}
    ShowMessages(cNoActiveRC);
   {$ENDIF}
    Abort;
  end;
end;

function IsMainThread: Boolean;
begin
  Result := vMainThread;
end;

function IsServiceContextAvaible: Boolean;
begin
  Result := VKContextManager.FHandles <> nil;
end;

function GetServiceWindow: TForm;
begin
  Result := vServiceWindow;
end;


// RegisterVKContextClass
//

procedure RegisterVKContextClass(aVKContextClass: TVKContextClass);
begin
  if not Assigned(vContextClasses) then
    vContextClasses := TList.Create;
  vContextClasses.Add(aVKContextClass);
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
  FLock := TCriticalSection.Create;
  FColorBits := 32;
  FStencilBits := 0;
  FAccumBits := 0;
  FAuxBuffers := 0;
  FLayer := clMainPlane;
  FOptions := [];
  FSharedContexts := TThreadList.Create;
  FSharedContexts.Add(Self);
  FAcceleration := chaUnknown;
  FVKStates := TVKStateCache.Create;
  FVK := TVKExtensionsAndEntryPoints.Create;
  FTransformation := TVKTransformation.Create;
  FTransformation.LoadMatricesEnabled := True;
  VKContextManager.RegisterContext(Self);
  FIsPraparationNeed := True;
end;

// Destroy
//

destructor TVKContext.Destroy;
begin
  if IsValid then
    DestroyContext;
  VKContextManager.UnRegisterContext(Self);
  FVKStates.Free;
  FVK.Free;
  FXVK.Free;
  FTransformation.Free;
  FSharedContexts.Free;
  FLock.Free;
  inherited Destroy;
end;

// SetColorBits
//

procedure TVKContext.SetColorBits(const aColorBits: Integer);
begin
  if Active then
    raise EVKContext.Create(strCannotAlterAnActiveContext)
  else
    FColorBits := aColorBits;
end;

// SetAlphaBits
//

procedure TVKContext.SetAlphaBits(const aAlphaBits: Integer);
begin
  if Active then
    raise EVKContext.Create(strCannotAlterAnActiveContext)
  else
    FAlphaBits := aAlphaBits;
end;

// SetDepthBits
//

procedure TVKContext.SetDepthBits(const val: Integer);
begin
  if Active then
    raise EVKContext.Create(strCannotAlterAnActiveContext)
  else
    FDepthBits := val;
end;

procedure TVKContext.SetLayer(const Value: TVKContextLayer);
begin
  if Active then
    raise EVKContext.Create(strCannotAlterAnActiveContext)
  else
    FLayer := Value;
end;

// SetStencilBits
//

procedure TVKContext.SetStencilBits(const aStencilBits: Integer);
begin
  if Active then
    raise EVKContext.Create(strCannotAlterAnActiveContext)
  else
    FStencilBits := aStencilBits;
end;

// SetAccumBits
//

procedure TVKContext.SetAccumBits(const aAccumBits: Integer);
begin
  if Active then
    raise EVKContext.Create(strCannotAlterAnActiveContext)
  else
    FAccumBits := aAccumBits;
end;

// SetAuxBuffers
//

procedure TVKContext.SetAuxBuffers(const aAuxBuffers: Integer);
begin
  if Active then
    raise EVKContext.Create(strCannotAlterAnActiveContext)
  else
    FAuxBuffers := aAuxBuffers;
end;

// SetOptions
//

procedure TVKContext.SetOptions(const aOptions: TVKRCOptions);
begin
  if Active then
    raise EVKContext.Create(strCannotAlterAnActiveContext)
  else
    FOptions := aOptions;
end;

// SetAntiAliasing
//

procedure TVKContext.SetAntiAliasing(const val: TVKAntiAliasing);
begin
  if Active then
    raise EVKContext.Create(strCannotAlterAnActiveContext)
  else
    FAntiAliasing := val;
end;

// SetAcceleration
//

procedure TVKContext.SetAcceleration(const val: TVKContextAcceleration);
begin
  if Active then
    raise EVKContext.Create(strCannotAlterAnActiveContext)
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
    raise EVKContext.Create(strContextAlreadyCreated);
  DoCreateContext(ADeviceHandle);
  Manager.ContextCreatedBy(Self);
end;

// CreateMemoryContext
//

procedure TVKContext.CreateMemoryContext(OutputDevice: THandle;
  Width, Height: Integer; BufferCount: integer);
begin
  if IsValid then
    raise EVKContext.Create(strContextAlreadyCreated);
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
  if vCurrentVKContext = Self then
  begin
{$IFNDEF VKS_MULTITHREAD}
    for i := Manager.FHandles.LockList.Count - 1 downto 0 do
    begin
      LHandle := TVKContextHandle(Manager.FHandles.LockList[i]);
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
end;

// ShareLists
//

procedure TVKContext.ShareLists(AContext: TVKContext);
begin
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
end;

// DestroyAllHandles
//

procedure TVKContext.DestroyAllHandles;
var
  i: Integer;
begin
  Activate;
  try
    with Manager.FHandles.LockList do
      try
        for i := Count - 1 downto 0 do
          TVKContextHandle(Items[i]).ContextDestroying;
      finally
        Manager.FHandles.UnlockList;
      end;
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

  if vCurrentVKContext <> Self then
  begin
    oldContext := vCurrentVKContext;
    if Assigned(oldContext) then
      oldContext.Deactivate;
  end
  else
    oldContext := nil;

  Activate;
  try
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
    Manager.DestroyingContextBy(Self);

    aList := FSharedContexts.LockList;
    for I := 1 to aList.Count - 1 do
    begin
      otherContext := TVKContext(aList[I]);
      otherContext.FSharedContexts.Remove(Self);
    end;
    FSharedContexts.Clear;
    FSharedContexts.Add(Self);
    FSharedContexts.UnlockList;
    Active := False;
    DoDestroyContext;
  finally
    if Assigned(oldContext) then
      oldContext.Activate;
  end;
  FAcceleration := chaUnknown;
  ///FGL.Close;
end;

// Activate
//

procedure TVKContext.Activate;
begin
  FLock.Enter;
  if FActivationCount = 0 then
  begin
    if not IsValid then
      raise EVKContext.Create(strContextNotCreated);

    vContextActivationFailureOccurred := False;
    try
      DoActivate;
    except
      vContextActivationFailureOccurred := True;
    end;
    ///vGL := FGL;
    vCurrentVKContext := Self;
  end
  else
    Assert(vCurrentVKContext = Self, 'vCurrentVKContext <> Self');
  Inc(FActivationCount);
end;

// Deactivate
//

procedure TVKContext.Deactivate;
begin
  Assert(vCurrentVKContext = Self);
  Dec(FActivationCount);
  if FActivationCount = 0 then
  begin
    if not IsValid then
      raise EVKContext.Create(strContextNotCreated);
    if not vContextActivationFailureOccurred then
      DoDeactivate;
    vCurrentVKContext := nil;
    ///vGL := GLwithoutContext;
  end
  else if FActivationCount < 0 then
    raise EVKContext.Create(strUnbalancedContexActivations);
  FLock.Leave;
end;

// FindCompatibleContext
//

function TVKContext.FindCompatibleContext: TVKContext;
var
  i: Integer;
begin
  Result := nil;
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
end;

class function TVKContext.ServiceContext: TVKContext;
begin
  Result := VKContextManager.FServiceContext;
end;

procedure TVKContext.MakeGLCurrent;
begin
  vVK := FVK;
end;

function TVKContext.GetXGL: TAbstractMultitextureCoordinator;
begin
  if FXVK = nil then
    FXVK := vMultitextureCoordinatorClass.Create(Self);
  Result := FXVK;
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
  VKContextManager.FHandles.Add(Self);
end;

// CreateAndAllocate
//

constructor TVKContextHandle.CreateAndAllocate(failIfAllocationFailed: Boolean =
  True);
begin
  Create;
  AllocateHandle;
  if failIfAllocationFailed and (Handle = 0) then
    raise EVKContext.Create('Auto-allocation failed');
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
  if Assigned(VKContextManager) then
    VKContextManager.FHandles.Remove(Self);
  inherited Destroy;
end;

// AllocateHandle
//

function TVKContextHandle.AllocateHandle: GLuint;
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

  if vCurrentVKContext = nil then
  begin
    ShowMessage('Failed to allocate Vulkan identifier - no active rendering context!');
    exit;
  end;

  //add entry
  New(FLastHandle);
  FillChar(FLastHandle^, sizeof(FLastHandle^), 0);
  FHandles.Add(FLastHandle);
  FLastHandle.FRenderingContext := vCurrentVKContext;

  bSucces := False;
  if Transferable then
  begin
    aList := vCurrentVKContext.FSharedContexts.LockList;
    try
      for I := aList.Count - 1 downto 0 do
      begin
        P := SearchRC(aList[I]);
        if (P.FHandle > 0) then
        begin
          // Copy shared handle
          //FLastHandle.FRenderingContext := vCurrentVKContext;
          FLastHandle.FHandle           := P.FHandle;
          FLastHandle.FChanged          := P.FChanged;
          Inc(vCurrentVKContext.FOwnedHandlesCount);
          bSucces := True;
          break;
        end;
      end;
    finally
      vCurrentVKContext.FSharedContexts.UnlockList;
    end;
  end;
  if not bSucces then
  begin
    // Allocate handle in current context
    FLastHandle.FHandle := DoAllocateHandle;
    bSucces := FLastHandle.FHandle <> 0;
    FLastHandle.FChanged := bSucces;
    if bSucces then
      Inc(vCurrentVKContext.FOwnedHandlesCount);
  end;

  Result := FLastHandle.FHandle;
  if not bSucces then
///    ShowMessages(cNoActiveRC)
  else if Assigned(FOnPrepare) then
    VKContextManager.NotifyPreparationNeed;
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
    AContext := vCurrentVKContext;

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
  if vCurrentVKContext <> FLastHandle.FRenderingContext then
    FLastHandle := SearchRC(vCurrentVKContext);
end;

function TVKContextHandle.GetHandle: GLuint;
begin
//  CheckCurrentRC;
//inline doesn't always work... so optimize it here
  if vCurrentVKContext <> FLastHandle.FRenderingContext then
    FLastHandle := SearchRC(vCurrentVKContext);

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
  oldContext := vCurrentVKContext;
  if Assigned(oldContext) then
    oldContext.Deactivate;
  try
    for I := FHandles.Count-1 downto 1 do
    begin
      P := FHandles[I];
      if P.FHandle > 0 then
      begin
        P.FRenderingContext.Activate;
        if IsValid(P.FHandle) > 0 then
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
    if Assigned(vCurrentVKContext) then
      vCurrentVKContext.Deactivate;
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
  if Assigned(vCurrentVKContext) then
  begin
    bShared := False;
    if Transferable then
    begin
      aList := vCurrentVKContext.FSharedContexts.LockList;
      try
        for I := FHandles.Count-1 downto 1 do
        begin
          P := RCItem(I);
          if (P.FRenderingContext <> vCurrentVKContext)
            and (P.FHandle <> 0)
            and (aList.IndexOf(P.FRenderingContext) > -1) then
            begin
              bShared := True;
              break;
            end;
        end;
      finally
        vCurrentVKContext.FSharedContexts.UnLockList;
      end;
    end;

    for I := FHandles.Count-1 downto 1 do
    begin
      P := RCItem(I);
      if (P.FRenderingContext = vCurrentVKContext) and (P.FHandle <> 0) then
      begin
        if not bShared then
          if IsValid(P.FHandle) > 0 then
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
      if (Result = vCurrentVKContext) then
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
  if Assigned(vCurrentVKContext) then
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
      aList := vCurrentVKContext.FSharedContexts.LockList;
      try
        for I := 0 to aList.Count - 1 do
        begin
          with SearchRC(aList[I])^ do
            if (FHandle <> 0) then
              FChanged := False;
        end;
      finally
        vCurrentVKContext.FSharedContexts.UnlockList;
      end;
    end;
  end
  else
///    ShowMessages(cNoActiveRC);
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
    VKContextManager.NotifyPreparationNeed;
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
  aList := vCurrentVKContext.FSharedContexts.LockList;
  try
    for I := 0 to aList.Count - 1 do
    begin
      vContext := aList[I];
      if (vContext <> vCurrentVKContext) and
        // at least one context is friendly
        (SearchRC(vContext).FHandle <> 0) then
        exit;
    end;
  finally
    vCurrentVKContext.FSharedContexts.UnlockList;
  end;
  Result := false;
end;

// Transferable
//

class function TVKContextHandle.Transferable: Boolean;
begin
  Result := True;
end;

class function TVKContextHandle.IsValid(const ID: GLuint): GLboolean;
begin
  Result := 1;
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

function TVKVirtualHandle.DoAllocateHandle: GLuint;
begin
  Result := 0;
  if Assigned(FOnAllocate) then
    FOnAllocate(Self, Result);
end;

// DoDestroyHandle
//

procedure TVKVirtualHandle.DoDestroyHandle(var AHandle: GLuint);
begin
  if not vContextActivationFailureOccurred then
  begin
    // reset error status
    glGetError;
    // delete
    if Assigned(FOnDestroy) then
      FOnDestroy(Self, AHandle);
    // check for error
    {CheckError; } // from Vulkan adapter
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

function TVKListHandle.DoAllocateHandle: GLuint;
begin
  Result := glGenLists(1);
end;

// DoDestroyHandle
//

procedure TVKListHandle.DoDestroyHandle(var AHandle: GLuint);
begin
  if not vContextActivationFailureOccurred then
//  with GL do
  begin
    // reset error status
    glGetError;
    // delete
    glDeleteLists(AHandle, 1);
    // check for error
    {CheckError;}
  end;
end;

// IsValid
//

class function TVKListHandle.IsValid(const ID: GLuint): GLboolean;
begin
  Result := glIsList(ID);
end;

// NewList
//

procedure TVKListHandle.NewList(mode: Cardinal);
begin
  vCurrentVKContext.VKStates.NewList(GetHandle, mode);
end;

// EndList
//

procedure TVKListHandle.EndList;
begin
  vCurrentVKContext.VKStates.EndList;
end;

// CallList
//

procedure TVKListHandle.CallList;
begin
  vCurrentVKContext.VKStates.CallList(GetHandle);
end;

// ------------------
// ------------------ TVKTextureHandle ------------------
// ------------------

// DoAllocateHandle
//

function TVKTextureHandle.DoAllocateHandle: Cardinal;
begin
  Result := 0;
  glGenTextures(1, @Result);
  FTarget := ttNoShape;
end;

// DoDestroyHandle
//

procedure TVKTextureHandle.DoDestroyHandle(var AHandle: GLuint);
var
  a: GLint;
  t: TVKTextureTarget;
begin
  if not vContextActivationFailureOccurred then
///  with GL do
  begin
    // reset error status
    glGetError;
    { Unbind identifier from all image selectors. }
    with GetContext.VKStates do
    begin
      for a := 0 to MaxTextureImageUnits - 1 do
        for t := Low(TVKTextureTarget) to High(TVKTextureTarget) do
          if TextureBinding[a, t] = AHandle then
            TextureBinding[a, t] := 0;
    end;
    glDeleteTextures(1, @AHandle);
    // check for error
    glGetError;
  end;
end;

// IsValid
//

class function TVKTextureHandle.IsValid(const ID: GLuint): GLboolean;
begin
  Result := glIsTexture(ID);
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
  glGenSamplers(1, @Result);
end;

// DoDestroyHandle
//

procedure TVKSamplerHandle.DoDestroyHandle(var AHandle: GLuint);
begin
  if not vContextActivationFailureOccurred then
///  with GL do
  begin
    // reset error status
    glGetError;
    // delete
    glDeleteSamplers(1, @AHandle);
    // check for error
    {CheckError;}
  end;
end;

// TVKSamplerHandle
//

class function TVKSamplerHandle.IsSupported: Boolean;
begin
  Result := GL_ARB_sampler_objects;
end;

// IsValid
//

class function TVKSamplerHandle.IsValid(const ID: GLuint): GLboolean;
begin
  Result := glIsSampler(ID);
end;

// ------------------
// ------------------ TVKQueryHandle ------------------
// ------------------

// BeginQuery
//

procedure TVKQueryHandle.BeginQuery;
begin
  if vCurrentVKContext.VKStates.CurrentQuery[QueryType] = 0 then
    vCurrentVKContext.VKStates.BeginQuery(QueryType, GetHandle);
  Factive := True;
end;

// CounterBits
//

function TVKQueryHandle.CounterBits: integer;
begin
  glGetQueryiv(Target, GL_QUERY_COUNTER_BITS, @Result);
end;

// DoAllocateHandle
//

function TVKQueryHandle.DoAllocateHandle: Cardinal;
begin
  Result := 0;
  glGenQueries(1, @Result);
end;

// DoDestroyHandle
//

procedure TVKQueryHandle.DoDestroyHandle(var AHandle: GLuint);
begin
  if not vContextActivationFailureOccurred then
  begin
    // reset error status
    glGetError;
    // delete
    glDeleteQueries(1, @AHandle);
    // check for error
    {CheckError;}
  end;
end;

// IsValid
//

class function TVKQueryHandle.IsValid(const ID: GLuint): GLboolean;
begin
  Result := glIsQuery(ID);
end;

// EndQuery
//

procedure TVKQueryHandle.EndQuery;
begin
  Assert(FActive = true, 'Cannot end a query before it begins');
  Factive := False;
  Assert(Handle <> 0);
  //glEndQuery(Target);
  vCurrentVKContext.VKStates.EndQuery(QueryType);
end;

// IsResultAvailable
//

function TVKQueryHandle.IsResultAvailable: boolean;
begin
  glGetQueryObjectiv(Handle, GL_QUERY_RESULT_AVAILABLE, @Result);
end;

// QueryResultInt
//

function TVKQueryHandle.QueryResultInt: GLint;
begin
  glGetQueryObjectiv(Handle, GL_QUERY_RESULT, @Result);
end;

// QueryResultInt64
//

function TVKQueryHandle.QueryResultInt64: GLint64EXT;
begin
  glGetQueryObjecti64v(Handle, GL_QUERY_RESULT, @Result);
end;

// QueryResultUInt
//

function TVKQueryHandle.QueryResultUInt: GLuint;
begin
  glGetQueryObjectuiv(Handle, GL_QUERY_RESULT, @Result);
end;

// QueryResultUInt64
//

function TVKQueryHandle.QueryResultUInt64: GLuint64EXT;
begin
  glGetQueryObjectui64v(Handle, GL_QUERY_RESULT, @Result);
end;

function TVKQueryHandle.QueryResultBool: GLboolean;
var
  I: GLuint;
begin
  glGetQueryObjectuiv(Handle, GL_QUERY_RESULT, @I);
  Result := 1;
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

function TVKOcclusionQueryHandle.GetTarget: GLuint;
begin
  Result := GL_SAMPLES_PASSED;
end;

// IsSupported
//

class function TVKOcclusionQueryHandle.IsSupported: Boolean;
begin
  Result := GL_VERSION_1_5;
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

function TVKBooleanOcclusionQueryHandle.GetTarget: GLuint;
begin
  Result := GL_ANY_SAMPLES_PASSED;
end;

// IsSupported
//

class function TVKBooleanOcclusionQueryHandle.IsSupported: Boolean;
begin
  Result := GL_ARB_occlusion_query2;
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

function TVKTimerQueryHandle.GetTarget: GLuint;
begin
  Result := GL_TIME_ELAPSED;
end;

// IsSupported
//

class function TVKTimerQueryHandle.IsSupported: Boolean;
begin
  Result := GL_EXT_timer_query or GL_ARB_timer_query;
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

function TVKPrimitiveQueryHandle.GetTarget: GLuint;
begin
  Result := GL_PRIMITIVES_GENERATED;
end;

// IsSupported
//

class function TVKPrimitiveQueryHandle.IsSupported: Boolean;
begin
  Result := GL_VERSION_3_0;
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
  bufferUsage: GLuint);
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
  glGenBuffers(1, @Result);
end;

// DoDestroyHandle
//

procedure TVKBufferObjectHandle.DoDestroyHandle(var AHandle: GLuint);
begin
  if not vContextActivationFailureOccurred then
  begin
    // reset error status
    glGetError;
    UnBind;
    // delete
    glDeleteBuffers(1, @AHandle);
    // check for error
    {CheckError;}
  end;
end;

// IsValid
//

class function TVKBufferObjectHandle.IsValid(const ID: GLuint): GLboolean;
begin
  Result := glIsBuffer(ID);
end;

// IsSupported
//

class function TVKBufferObjectHandle.IsSupported: Boolean;
begin
  Result := GL_ARB_vertex_buffer_object;
end;

// BindRange
//

procedure TVKBufferObjectHandle.BindRange(index: GLuint; offset: GLintptr;
  size: GLsizeiptr);
begin
  Assert(False, 'BindRange only XBO and UBO');
end;

// BindBase
//

procedure TVKBufferObjectHandle.BindBase(index: GLuint);
begin
  Assert(False, 'BindRange only XBO and UBO');
end;

// UnBindBase
//

procedure TVKBufferObjectHandle.UnBindBase(index: GLuint);
begin
  Assert(False, 'BindRange only XBO and UBO');
end;

// BufferData
//

procedure TVKBufferObjectHandle.BufferData(p: Pointer; size: Integer;
  bufferUsage: GLuint);
begin
  FSize := size;
  glBufferData(Target, size, p, bufferUsage);
end;

// BindBufferData
//

procedure TVKBufferObjectHandle.BindBufferData(p: Pointer; size: Integer;
  bufferUsage: GLuint);
begin
  Bind;
  FSize := size;
  glBufferData(Target, size, p, bufferUsage);
end;

// BufferSubData
//

procedure TVKBufferObjectHandle.BufferSubData(offset, size: Integer; p:
  Pointer);
begin
  Assert(offset + size <= FSize);
  glBufferSubData(Target, offset, size, p);
end;

// MapBuffer
//

function TVKBufferObjectHandle.MapBuffer(access: GLuint): Pointer;
begin
  Result := glMapBuffer(Target, access);
end;

// MapBufferRange
//

function TVKBufferObjectHandle.MapBufferRange(offset: GLint; len: GLsizei;
  access: GLbitfield): Pointer;
begin
  Result := glMapBufferRange(Target, offset, len, access);
end;

// Flush
//

procedure TVKBufferObjectHandle.Flush(offset: GLint; len: GLsizei);
begin
  glFlushMappedBufferRange(Target, offset, len);
end;

// UnmapBuffer
//

function TVKBufferObjectHandle.UnmapBuffer: GLboolean;
begin
  Result := glUnmapBuffer(Target);
end;

// ------------------
// ------------------ TVKVBOHandle ------------------
// ------------------

// GetVBOTarget
//

function TVKVBOHandle.GetVBOTarget: GLuint;
begin
  Result := Target;
end;

// ------------------
// ------------------ TVKVBOArrayBufferHandle ------------------
// ------------------

procedure TVKVBOArrayBufferHandle.Bind;
begin
  vCurrentVKContext.VKStates.ArrayBufferBinding := Handle;
end;

procedure TVKVBOArrayBufferHandle.UnBind;
begin
  vCurrentVKContext.VKStates.ArrayBufferBinding := 0;
end;

// GetTarget
//

function TVKVBOArrayBufferHandle.GetTarget: GLuint;
begin
  Result := GL_ARRAY_BUFFER;
end;

// ------------------
// ------------------ TVKVBOElementArrayHandle ------------------
// ------------------

procedure TVKVBOElementArrayHandle.Bind;
begin
  vCurrentVKContext.VKStates.ElementBufferBinding := Handle;
end;

procedure TVKVBOElementArrayHandle.UnBind;
begin
  vCurrentVKContext.VKStates.ElementBufferBinding := 0;
end;

// GetTarget
//

function TVKVBOElementArrayHandle.GetTarget: GLuint;
begin
  Result := GL_ELEMENT_ARRAY_BUFFER;
end;

// ------------------
// ------------------ TVKPackPBOHandle ------------------
// ------------------

procedure TVKPackPBOHandle.Bind;
begin
  vCurrentVKContext.VKStates.PixelPackBufferBinding := Handle;
end;

procedure TVKPackPBOHandle.UnBind;
begin
  vCurrentVKContext.VKStates.PixelPackBufferBinding := 0;
end;

// GetTarget
//

function TVKPackPBOHandle.GetTarget: GLuint;
begin
  Result := GL_PIXEL_PACK_BUFFER;
end;

// IsSupported
//

class function TVKPackPBOHandle.IsSupported: Boolean;
begin
  Result := GL_ARB_pixel_buffer_object;
end;

// ------------------
// ------------------ TVKUnpackPBOHandle ------------------
// ------------------

procedure TVKUnpackPBOHandle.Bind;
begin
  vCurrentVKContext.VKStates.PixelUnpackBufferBinding := Handle;
end;

procedure TVKUnpackPBOHandle.UnBind;
begin
  vCurrentVKContext.VKStates.PixelUnpackBufferBinding := 0;
end;

// GetTarget
//

function TVKUnpackPBOHandle.GetTarget: GLuint;
begin
  Result := GL_PIXEL_UNPACK_BUFFER;
end;

// IsSupported
//

class function TVKUnpackPBOHandle.IsSupported: Boolean;
begin
  Result := GL_ARB_pixel_buffer_object;
end;

// ------------------
// ------------------ TVKTransformFeedbackBufferHandle ------------------
// ------------------

// GetTarget
//

procedure TVKTransformFeedbackBufferHandle.Bind;
begin
  vCurrentVKContext.VKStates.TransformFeedbackBufferBinding := Handle;
end;

procedure TVKTransformFeedbackBufferHandle.UnBind;
begin
  vCurrentVKContext.VKStates.TransformFeedbackBufferBinding := 0;
end;

function TVKTransformFeedbackBufferHandle.GetTarget: GLuint;
begin
  Result := GL_TRANSFORM_FEEDBACK_BUFFER;
end;

// BeginTransformFeedback
//

procedure TVKTransformFeedbackBufferHandle.BeginTransformFeedback(primitiveMode:
  GLEnum);
begin
  glBeginTransformFeedback(primitiveMode);
end;

// EndTransformFeedback
//

procedure TVKTransformFeedbackBufferHandle.EndTransformFeedback();
begin
  glEndTransformFeedback();
end;

procedure TVKTransformFeedbackBufferHandle.BindRange(index: GLuint; offset: GLintptr;
  size: GLsizeiptr);
begin
  vCurrentVKContext.VKStates.SetBufferIndexedBinding(Handle, bbtTransformFeedBack,
    index, offset, size);
end;

procedure TVKTransformFeedbackBufferHandle.BindBase(index: GLuint);
begin
  vCurrentVKContext.VKStates.SetBufferIndexedBinding(Handle, bbtTransformFeedBack,
    index, BufferSize);
end;

procedure TVKTransformFeedbackBufferHandle.UnBindBase(index: GLuint);
begin
  vCurrentVKContext.VKStates.SetBufferIndexedBinding(0, bbtTransformFeedBack,
    index, 0);
end;

// IsSupported
//

class function TVKTransformFeedbackBufferHandle.IsSupported: Boolean;
begin
  Result := GL_EXT_transform_feedback or GL_VERSION_3_0;
end;

// ------------------
// ------------------ TVKTextureBufferHandle ------------------
// ------------------

procedure TVKTextureBufferHandle.Bind;
begin
  vCurrentVKContext.VKStates.TextureBufferBinding := Handle;
end;

procedure TVKTextureBufferHandle.UnBind;
begin
  vCurrentVKContext.VKStates.TextureBufferBinding := 0;
end;

// GetTarget
//

function TVKTextureBufferHandle.GetTarget: GLuint;
begin
  Result := GL_TEXTURE_BUFFER;
end;

// IsSupported
//

class function TVKTextureBufferHandle.IsSupported: Boolean;
begin
  Result := GL_VERSION_3_1;
end;

// ------------------
// ------------------ TVKUniformBufferHandle ------------------
// ------------------

procedure TVKUniformBufferHandle.Bind;
begin
  vCurrentVKContext.VKStates.UniformBufferBinding := Handle;
end;

procedure TVKUniformBufferHandle.UnBind;
begin
  vCurrentVKContext.VKStates.UniformBufferBinding := 0;
end;

procedure TVKUniformBufferHandle.BindRange(index: GLuint; offset: GLintptr;
  size: GLsizeiptr);
begin
  vCurrentVKContext.VKStates.SetBufferIndexedBinding(Handle, bbtUniform,
    index, offset, size);
end;

procedure TVKUniformBufferHandle.BindBase(index: GLuint);
begin
  vCurrentVKContext.VKStates.SetBufferIndexedBinding(Handle, bbtUniform,
    index, BufferSize);
end;

procedure TVKUniformBufferHandle.UnBindBase(index: GLuint);
begin
  vCurrentVKContext.VKStates.SetBufferIndexedBinding(0, bbtUniform,
    index, 0);
end;

// GetTarget
//

function TVKUniformBufferHandle.GetTarget: GLuint;
begin
  Result := GL_UNIFORM_BUFFER;
end;

// IsSupported
//

class function TVKUniformBufferHandle.IsSupported: Boolean;
begin
  Result := GL_ARB_uniform_buffer_object;
end;

// ------------------
// ------------------ TVKVertexArrayHandle ------------------
// ------------------

// DoAllocateHandle
//

function TVKVertexArrayHandle.DoAllocateHandle: Cardinal;
begin
  Result := 0;
  glGenVertexArrays(1, @Result);
end;

// DoDestroyHandle
//

procedure TVKVertexArrayHandle.DoDestroyHandle(var AHandle: GLuint);
begin
  if not vContextActivationFailureOccurred then
  begin
    // reset error status
    glGetError;
    // delete
    glDeleteVertexArrays(1, @AHandle);
    // check for error
    {CheckError;}
  end;
end;

// IsValid
//

class function TVKVertexArrayHandle.IsValid(const ID: GLuint): GLboolean;
begin
  Result := glIsVertexArray(ID);
end;

// Bind
//

procedure TVKVertexArrayHandle.Bind;
begin
  Assert(vCurrentVKContext <> nil);
  vCurrentVKContext.VKStates.VertexArrayBinding := Handle;
end;

// UnBind
//

procedure TVKVertexArrayHandle.UnBind;
begin
  Assert(vCurrentVKContext <> nil);
  vCurrentVKContext.VKStates.VertexArrayBinding := 0;
end;

// IsSupported
//

class function TVKVertexArrayHandle.IsSupported: Boolean;
begin
  Result := GL_ARB_vertex_array_object;
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
  glGenFramebuffers(1, @Result)
end;

// DoDestroyHandle
//

procedure TVKFramebufferHandle.DoDestroyHandle(var AHandle: GLuint);
begin
  if not vContextActivationFailureOccurred then
  begin
    // reset error status
    glGetError;
    // delete
    glDeleteFramebuffers(1, @AHandle);
    // check for error
    {CheckError;}
  end;
end;

// IsValid
//

class function TVKFramebufferHandle.IsValid(const ID: GLuint): GLboolean;
begin
  Result := glIsFramebuffer(ID);
end;

// Bind
//

procedure TVKFramebufferHandle.Bind;
begin
  Assert(vCurrentVKContext <> nil);
  vCurrentVKContext.VKStates.SetFrameBuffer(Handle);
end;

// BindForDrawing
//

procedure TVKFramebufferHandle.BindForDrawing;
begin
  Assert(vCurrentVKContext <> nil);
  vCurrentVKContext.VKStates.DrawFrameBuffer := Handle;
end;

// BindForReading
//

procedure TVKFramebufferHandle.BindForReading;
begin
  Assert(vCurrentVKContext <> nil);
  vCurrentVKContext.VKStates.ReadFrameBuffer := Handle;
end;

// UnBind
//

procedure TVKFramebufferHandle.UnBind;
begin
  Assert(vCurrentVKContext <> nil);
  vCurrentVKContext.VKStates.SetFrameBuffer(0);
end;

// UnBindForDrawing
//

procedure TVKFramebufferHandle.UnBindForDrawing;
begin
  Assert(vCurrentVKContext <> nil);
  vCurrentVKContext.VKStates.DrawFrameBuffer := 0;
end;

// UnBindForReading
//

procedure TVKFramebufferHandle.UnBindForReading;
begin
  Assert(vCurrentVKContext <> nil);
  vCurrentVKContext.VKStates.ReadFrameBuffer := 0;
end;

// Attach1DTexture
//

procedure TVKFramebufferHandle.Attach1DTexture(target: GLEnum; attachment:
  GLEnum; textarget: GLEnum; texture: GLuint; level: GLint);
begin
  glFramebufferTexture1D(target, attachment, textarget, texture, level);
end;

// Attach2DTexture
//

procedure TVKFramebufferHandle.Attach2DTexture(target: GLEnum; attachment:
  GLEnum; textarget: GLEnum; texture: GLuint; level: GLint);
begin
  glFramebufferTexture2D(target, attachment, textarget, texture, level);
end;

// Attach3DTexture
//

procedure TVKFramebufferHandle.Attach3DTexture(target: GLEnum; attachment:
  GLEnum; textarget: GLEnum; texture: GLuint; level: GLint; layer: GLint);
begin
  glFramebufferTexture3D(target, attachment, textarget, texture, level, layer);
end;

// AttachLayer
//

procedure TVKFramebufferHandle.AttachLayer(target: GLEnum; attachment: GLEnum;
  texture: GLuint; level: GLint; layer: GLint);
begin
  glFramebufferTextureLayer(target, attachment, texture, level, layer);
end;

// AttachRenderBuffer
//

procedure TVKFramebufferHandle.AttachRenderBuffer(target: GLEnum; attachment:
  GLEnum; renderbuffertarget: GLEnum; renderbuffer: GLuint);
begin
  glFramebufferRenderbuffer(target, attachment, renderbuffertarget,
    renderbuffer);
end;

// AttachTexture
//

procedure TVKFramebufferHandle.AttachTexture(target: GLEnum; attachment:
  GLEnum; texture: GLuint; level: GLint);
begin
  glFramebufferTexture(target, attachment, texture, level);
end;

// AttachTextureLayer
//

procedure TVKFramebufferHandle.AttachTextureLayer(target: GLEnum; attachment:
  GLEnum; texture: GLuint; level: GLint; layer: GLint);
begin
  glFramebufferTextureLayer(target, attachment, texture, level, layer);
end;

// Blit
//

procedure TVKFramebufferHandle.Blit(srcX0: GLint; srcY0: GLint; srcX1: GLint;
  srcY1: GLint;
  dstX0: GLint; dstY0: GLint; dstX1: GLint; dstY1: GLint;
  mask: GLbitfield; filter: GLEnum);
begin
  glBlitFramebuffer(srcX0, srcY0, srcX1, srcY1, dstX0, dstY0, dstX1, dstY1,
    mask, filter);
end;

// GetAttachmentParameter
//

function TVKFramebufferHandle.GetAttachmentParameter(target: GLEnum;
  attachment: GLEnum; pname: GLEnum): GLint;
begin
  glGetFramebufferAttachmentParameteriv(target, attachment, pname, @Result)
end;

// GetAttachmentObjectType
//

function TVKFramebufferHandle.GetAttachmentObjectType(target: GLEnum;
  attachment: GLEnum): GLint;
begin
  glGetFramebufferAttachmentParameteriv(target, attachment,
    GL_FRAMEBUFFER_ATTACHMENT_OBJECT_TYPE, @Result);
end;

// GetAttachmentObjectName
//

function TVKFramebufferHandle.GetAttachmentObjectName(target: GLEnum;
  attachment: GLEnum): GLint;
begin
  glGetFramebufferAttachmentParameteriv(target, attachment,
    GL_FRAMEBUFFER_ATTACHMENT_OBJECT_NAME, @Result);
end;

// CheckStatus
//

function TVKFramebufferHandle.GetStatus: TVKFramebufferStatus;
var
  Status: cardinal;
begin
  Status := glCheckFramebufferStatus(GL_FRAMEBUFFER);

  case Status of
    GL_FRAMEBUFFER_COMPLETE_EXT: Result := fsComplete;
    GL_FRAMEBUFFER_INCOMPLETE_ATTACHMENT_EXT: Result := fsIncompleteAttachment;
    GL_FRAMEBUFFER_INCOMPLETE_MISSING_ATTACHMENT_EXT: Result :=
      fsIncompleteMissingAttachment;
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
    'IncompleteDuplicateAttachment',
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
  Result := GL_EXT_framebuffer_object or GL_ARB_framebuffer_object;
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
  glGenRenderbuffers(1, @Result);
end;

// DoDestroyHandle
//

procedure TVKRenderbufferHandle.DoDestroyHandle(var AHandle: GLuint);
begin
  if not vContextActivationFailureOccurred then
  begin
    // reset error status
    glGetError;
    // delete
    glDeleteRenderbuffers(1, @AHandle);
    // check for error
    {CheckError;}
  end;
end;

// IsValid
//

class function TVKRenderbufferHandle.IsValid(const ID: GLuint): GLboolean;
begin
  Result := glIsRenderbuffer(ID);
end;

// Bind
//

procedure TVKRenderbufferHandle.Bind;
begin
  vCurrentVKContext.VKStates.RenderBuffer := GetHandle;
end;

// UnBind
//

procedure TVKRenderbufferHandle.UnBind;
begin
  if vCurrentVKContext <> nil then
    vCurrentVKContext.VKStates.RenderBuffer := 0;
end;

// SetStorage
//

procedure TVKRenderbufferHandle.SetStorage(internalformat: GLEnum; width,
  height: GLsizei);
begin
  glRenderbufferStorage(GL_RENDERBUFFER, internalformat, width, height);
end;

// SetStorageMultisample
//

procedure TVKRenderbufferHandle.SetStorageMultisample(internalformat: GLEnum;
  samples: GLsizei; width, height: GLsizei);
begin
  glRenderbufferStorageMultisample(GL_RENDERBUFFER, samples, internalformat,
    width, height);
end;

// IsSupported
//

class function TVKRenderbufferHandle.IsSupported: Boolean;
begin
  Result := GL_EXT_framebuffer_object or GL_ARB_framebuffer_object;
end;

// ------------------
// ------------------ TVKProgramHandleEXT ------------------
// ------------------

// DoAllocateHandle
//

function TVKProgramHandleEXT.DoAllocateHandle: Cardinal;
begin
  Result := 0;
  glGenProgramsARB(1, @Result);
  FReady := False;
end;

// DoDestroyHandle
//

procedure TVKProgramHandleEXT.DoDestroyHandle(var AHandle: GLuint);
begin
  if not vContextActivationFailureOccurred then
  begin
    // reset error status
    glGetError;
    // delete
    glDeleteProgramsARB(1, @AHandle);
    // check for error
    {CheckError;}
  end;
end;

// IsValid
//

class function TVKProgramHandleEXT.IsValid(const ID: GLuint): GLboolean;
begin
  Result := glIsProgram(ID);
end;

procedure TVKProgramHandleEXT.LoadARBProgram(AText: string);
const
  cProgType: array[0..2] of string =
    ('ARB vertex', 'ARB fragment', 'NV geometry');
var
  errPos, P: Integer;
begin
  Bind;
  glProgramStringARB(GetTarget, GL_PROGRAM_FORMAT_ASCII_ARB,
    Length(AText), PGLChar(AText));
  glGetIntegerv(GL_PROGRAM_ERROR_POSITION_ARB, @errPos);
  if errPos > -1 then
  begin
    FInfoLog := string(glGetString(GL_PROGRAM_ERROR_STRING_ARB));
    case GetTarget of
      GL_VERTEX_PROGRAM_ARB: P := 0;
      GL_FRAGMENT_PROGRAM_ARB: P := 1;
    else
      P := 2;
    end;
///    ShowMessages(Format('%s Program Error - [Pos: %d][Error %s]', [cProgType[P], errPos, FInfoLog]));
    FReady := False;
  end
  else
  begin
    FReady := True;
    FInfoLog := '';
  end;
end;

procedure TVKProgramHandleEXT.Enable;
begin
  if FReady then
    glEnable(GetTarget)
  else
    Abort;
end;

procedure TVKProgramHandleEXT.Disable;
begin
  glDisable(GetTarget);
end;

procedure TVKProgramHandleEXT.Bind;
begin
  glBindProgramARB(GetTarget, Handle);
end;

class function TVKVertexProgramHandle.GetTarget: GLEnum;
begin
  Result := GL_VERTEX_PROGRAM_ARB;
end;

class function TVKVertexProgramHandle.IsSupported: Boolean;
begin
  Result := GL_ARB_vertex_program;
end;

class function TVKFragmentProgramHandle.GetTarget: GLEnum;
begin
  Result := GL_FRAGMENT_PROGRAM_ARB;
end;

class function TVKFragmentProgramHandle.IsSupported: Boolean;
begin
  Result := GL_ARB_vertex_program;
end;

class function TVKGeometryProgramHandle.GetTarget: GLEnum;
begin
  Result := GL_GEOMETRY_PROGRAM_NV;
end;

class function TVKGeometryProgramHandle.IsSupported: Boolean;
begin
  Result := GL_NV_geometry_program4;
end;

// ------------------
// ------------------ TVKSLHandle ------------------
// ------------------

procedure TVKSLHandle.DoDestroyHandle(var AHandle: GLuint);
begin
  if not vContextActivationFailureOccurred then
  begin
    // reset error status
    glGetError;
    // delete
    glDeleteObjectARB(@AHandle);
    // check for error
    {CheckError;}
  end;
end;

// InfoLog
//

function TVKSLHandle.InfoLog: string;
var
  maxLength: Integer;
  log: String;
  AHandle : GLuint;
begin
  maxLength := 0;
  AHandle := GetHandle;
  glGetObjectParameterivARB(@AHandle, GL_OBJECT_INFO_LOG_LENGTH_ARB, @maxLength);
  SetLength(log, maxLength);
  if maxLength > 0 then
  begin
    glGetInfoLogARB(@AHandle, maxLength, @maxLength, @log[1]);
    SetLength(log, maxLength);
  end;
  Result := string(log);
end;

// IsSupported
//

class function TVKSLHandle.IsSupported: Boolean;
begin
  Result := GL_ARB_shader_objects;
end;

// ------------------
// ------------------ TVKShaderHandle ------------------
// ------------------

// DoAllocateHandle
//

function TVKShaderHandle.DoAllocateHandle: GLuint;
begin
  Result := glCreateShader(FShaderType)
end;

// IsValid
//

class function TVKShaderHandle.IsValid(const ID: GLuint): GLboolean;
begin
  Result := glIsShader(ID);
end;

// ShaderSource
//

procedure TVKShaderHandle.ShaderSource(const source: AnsiString);
var
  p: PGLChar;
begin
  p := PGLChar(source);
  glShaderSource(GetHandle, 1, @p, nil);
end;

// CompileShader
//

function TVKShaderHandle.CompileShader: Boolean;
var
  compiled: Integer;
  glH: GLuint;
begin
  glH := GetHandle;
  glCompileShader(glH);
  compiled := 0;
  glGetShaderiv(glH, GL_COMPILE_STATUS, @compiled);
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
  Result := GL_ARB_vertex_shader;
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
  Result := GL_EXT_geometry_shader4;
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
  Result := GL_ARB_fragment_shader;
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
  Result := GL_ARB_tessellation_shader;
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
  Result := GL_ARB_tessellation_shader;
end;

// ------------------
// ------------------ TVKProgramHandle ------------------
// ------------------

// DoAllocateHandle
//

function TVKProgramHandle.DoAllocateHandle: GLuint;
begin
  Result := glCreateProgram();
end;

// IsValid
//

class function TVKProgramHandle.IsValid(const ID: GLuint): GLboolean;
begin
  Result := glIsProgram(ID);
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
  glGetError;
end;

// AttachObject
//

procedure TVKProgramHandle.AttachObject(shader: TVKShaderHandle);
begin
  glAttachShader(GetHandle, shader.Handle);
end;

// DetachAllObject
//

procedure TVKProgramHandle.DetachAllObject;
var
  glH: GLuint;
  I: Integer;
  count: GLSizei;
  buffer: array[0..255] of GLuint;
begin
  glH := GetHandle;
  if glH > 0 then
  begin
    glGetAttachedShaders(glH, Length(buffer), @count, @buffer[0]);
    count := MinInteger(count, Length(buffer));
    for I := 0 to count - 1 do
      glDetachShader(glH, buffer[I]);
    NotifyChangesOfData;
  end;
end;

// BindAttribLocation
//

procedure TVKProgramHandle.BindAttribLocation(index: Integer; const aName: string);
begin
  glBindAttribLocation(GetHandle, index, PGLChar(aName));
end;

// BindFragDataLocation
//

procedure TVKProgramHandle.BindFragDataLocation(index: Integer; const aName:
  string);
begin
  glBindFragDataLocation(GetHandle, index, PGLChar(name));
end;

// LinkProgram
//

function TVKProgramHandle.LinkProgram: Boolean;
var
  status: Integer;
  glH: GLuint;
begin
  glH := GetHandle;
  glLinkProgram(glH);
  status := 0;
  glGetProgramiv(glH, GL_LINK_STATUS, @status);
  Result := (status <> 0);
end;

// ValidateProgram
//

function TVKProgramHandle.ValidateProgram: Boolean;
var
  validated: Integer;
  h: GLuint;
begin
  h := GetHandle;
  glValidateProgram(h);
  validated := 0;
  glGetProgramiv(h, GL_VALIDATE_STATUS, @validated);
  Result := (validated <> 0);
end;

// GetAttribLocation
//

function TVKProgramHandle.GetAttribLocation(const aName: string): Integer;
begin
  Result := glGetAttribLocation(GetHandle, PGLChar(aName));
  Assert(Result >= 0, Format(strUnknownParam, ['attrib', aName, Name]));
end;

// GetUniformLocation
//

function TVKProgramHandle.GetUniformLocation(const aName: string): Integer;
begin
  Result := glGetUniformLocation(GetHandle, PGLChar(aName));
  Assert(Result >= 0, Format(strUnknownParam, ['uniform', aName, Name]));
end;

// GetVaryingLocation
//

function TVKProgramHandle.GetVaryingLocation(const aName: string): Integer;
begin
  Result := glGetVaryingLocationNV(GetHandle, PGLChar(aName));
  Assert(Result >= 0, Format(strUnknownParam, ['varying', aName, Name]));
end;

// AddActiveVarying
//

procedure TVKProgramHandle.AddActiveVarying(const aName: string);
begin
  glActiveVaryingNV(GetHandle, PGLChar(aName));
end;

// GetAttribLocation
//

procedure TVKProgramHandle.UseProgramObject;
begin
  Assert(vCurrentVKContext <> nil);
  vCurrentVKContext.VKStates.CurrentProgram := Handle;
end;

// GetAttribLocation
//

procedure TVKProgramHandle.EndUseProgramObject;
begin
  Assert(vCurrentVKContext <> nil);
  vCurrentVKContext.VKStates.CurrentProgram := 0;
end;

// GetUniform1i
//

function TVKProgramHandle.GetUniform1i(const index: string): Integer;
begin
  glGetUniformiv(GetHandle, GetUniformLocation(index), @Result);
end;

// GetUniform2i
//

function TVKProgramHandle.GetUniform2i(const index: string): TVector2i;
begin
  glGetUniformiv(GetHandle, GetUniformLocation(index), @Result);
end;

// GetUniform3i
//

function TVKProgramHandle.GetUniform3i(const index: string): TVector3i;
begin
  glGetUniformiv(GetHandle, GetUniformLocation(index), @Result);
end;

// GetUniform4i
//

function TVKProgramHandle.GetUniform4i(const index: string): TVector4i;
begin
  glGetUniformiv(GetHandle, GetUniformLocation(index), @Result);
end;

// SetUniform1f
//

procedure TVKProgramHandle.SetUniform1f(const index: string; val: Single);
begin
  glUniform1f(GetUniformLocation(index), val);
end;

// GetUniform1f
//

function TVKProgramHandle.GetUniform1f(const index: string): Single;
begin
  glGetUniformfv(GetHandle, GetUniformLocation(index), @Result);
end;

// SetUniform1i
//

procedure TVKProgramHandle.SetUniform1i(const index: string; val: Integer);
begin
  glUniform1i(GetUniformLocation(index), val);
end;

// SetUniform2i
//

procedure TVKProgramHandle.SetUniform2i(const index: string;
  const Value: TVector2i);
begin
  glUniform2i(GetUniformLocation(index), Value.X, Value.Y);
end;

// SetUniform3i
//

procedure TVKProgramHandle.SetUniform3i(const index: string;
  const Value: TVector3i);
begin
  glUniform3i(GetUniformLocation(index), Value.X, Value.Y, Value.Z);
end;

// SetUniform4i
//

procedure TVKProgramHandle.SetUniform4i(const index: string;
  const Value: TVector4i);
begin
  glUniform4i(GetUniformLocation(index), Value.X, Value.Y, Value.Z, Value.W);
end;

// GetUniform2f
//

function TVKProgramHandle.GetUniform2f(const index: string): TVector2f;
begin
  glGetUniformfv(GetHandle, GetUniformLocation(index), @Result);
end;

// SetUniform2f
//

procedure TVKProgramHandle.SetUniform2f(const index: string; const val:
  TVector2f);
begin
  glUniform2f(GetUniformLocation(index), val.X, val.Y);
end;

// GetUniform3f
//

function TVKProgramHandle.GetUniform3f(const index: string): TAffineVector;
begin
  glGetUniformfv(GetHandle, GetUniformLocation(index), @Result);
end;

// SetUniform3f
//

procedure TVKProgramHandle.SetUniform3f(const index: string; const val:
  TAffineVector);
begin
  glUniform3f(GetUniformLocation(index), val.X, val.Y, val.Z);
end;

// GetUniform4f
//

function TVKProgramHandle.GetUniform4f(const index: string): TVector;
begin
  glGetUniformfv(GetHandle, GetUniformLocation(index), @Result);
end;

// SetUniform4f
//

procedure TVKProgramHandle.SetUniform4f(const index: string; const val:
  TVector);
begin
  glUniform4f(GetUniformLocation(index), val.X, val.Y, val.Z, val.W);
end;

// GetUniformMatrix2fv
//

function TVKProgramHandle.GetUniformMatrix2fv(const index: string): TMatrix2f;
begin
  glGetUniformfv(GetHandle, GetUniformLocation(index), @Result);
end;

// SetUniformMatrix2fv
//

procedure TVKProgramHandle.SetUniformMatrix2fv(const index: string; const val:
  TMatrix2f);
begin
  glUniformMatrix2fv(GetUniformLocation(index), 1, 0{False}, @val);
end;

// GetUniformMatrix3fv
//

function TVKProgramHandle.GetUniformMatrix3fv(const index: string): TMatrix3f;
begin
  glGetUniformfv(GetHandle, GetUniformLocation(index), @Result);
end;

// SetUniformMatrix3fv
//

procedure TVKProgramHandle.SetUniformMatrix3fv(const index: string; const val:
  TMatrix3f);
begin
  glUniformMatrix3fv(GetUniformLocation(index), 1, 0{False}, @val);
end;

// GetUniformMatrix4fv
//

function TVKProgramHandle.GetUniformMatrix4fv(const index: string): TMatrix;
begin
  glGetUniformfv(GetHandle, GetUniformLocation(index), @Result);
end;

// SetUniformMatrix4fv
//

procedure TVKProgramHandle.SetUniformMatrix4fv(const index: string; const val:
  TMatrix);
begin
  glUniformMatrix4fv(GetUniformLocation(index), 1, 0{False}, @val);
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
  const Value: GLuint);
begin
  vCurrentVKContext.VKStates.TextureBinding[0, TextureTarget] := Value;
  SetUniform1i(index, TextureIndex);
end;

// SetUniformBuffer
//

procedure TVKProgramHandle.SetUniformBuffer(const index: string;
  Value: TVKUniformBufferHandle);
begin
  glUniformBufferEXT(Handle, GetUniformLocation(index), Value.Handle);
end;

// GetUniformBufferSize
//

function TVKProgramHandle.GetUniformBufferSize(const aName: string): GLInt;
begin
  Result := glGetUniformBufferSizeEXT(Handle, GetUniformLocation(aName));
end;

// GetUniformOffset
//

function TVKProgramHandle.GetUniformOffset(const aName: string): GLintptr;
begin
  Result := glGetUniformOffsetEXT(Handle, GetUniformLocation(aName));
end;

// GetUniformBlockIndex
//

function TVKProgramHandle.GetUniformBlockIndex(const aName: string): Integer;
begin
  Result := glGetUniformBlockIndex(Handle, PGLChar(aName));
  Assert(Result >= 0, Format(strUnknownParam, ['uniform block', aName, Name]));
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


// Create
//

constructor TVKContextManager.Create;
begin
  inherited Create;
  FHandles := TThreadList.Create;

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
    ShowMessage('Service context queue task depleted');
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
        raise EVKContext.Create(strInvalidContextRegistration)
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
        raise EVKContext.Create(strInvalidContextRegistration)
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
      raise EVKContext.Create(strInvalidNotificationRemoval);
  finally
    UnLock;
  end;
end;

// Terminate
//

procedure TVKContextManager.Terminate;
begin
  FTerminated := True;
  if ContextCount = 0 then
  begin
    VKContextManager := nil;
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
    ShowMessage(Format('%s: can''t initialize rendering context', [ClassName]));
    FWindow.Destroy;
    vServiceWindow := nil;
  end;

begin
  try
    VKContextManager.ServiceContext.Acceleration := chaHardware;
    VKContextManager.ServiceContext.CreateMemoryContext(FDC, 1, 1, 1);
  except
    on EVKContext do
    begin
      Fail;
      exit;
    end;
    on EPBuffer do
    begin
      ShowMessage(Format('%s: can''t initialize memory rendering context. Try initialize common context.', [ClassName]));
      try
        { TODO -oPW : E2250 There is no overloaded version of 'CreateContext' that can be called with these arguments }
        (*VKContextManager.ServiceContext.CreateContext(FDC);*)
      except
        Fail;
        exit;
      end;
    end;
  end;
  ShowMessage('Service context successfuly initialized');
  VKContextManager.ServiceContext.Activate;
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
    with VKContextManager.FThreadTask.LockList do
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
        VKContextManager.FThreadTask.UnlockList;
      end;
  end;

begin
  with VKContextManager do
  begin
    vMainThread := False;
    ShowMessage('Service thread started');
    Sleep(100);
    try
      while not Terminated do
      begin
        NextTask;
        if Assigned(TaskRec.Task) then
        begin
          with VKContextManager.ServiceContext do
          begin
            if IsValid then
              Activate;
            try
              TaskRec.Task;
            except
              ShowMessage('Service thread task raised exception');
            end;
            if IsValid then
              Deactivate;
            if Assigned(TaskRec.Event) then
              TaskRec.Event.SetEvent;
          end;
         end
        else
          Synchronize(VKContextManager.QueueTaskDepleted);
        ServiceStarter.WaitFor(30000);
      end;
    finally
      ServiceContext.Destroy;
      FServiceContext := nil;
      ShowMessage('Service thread finished');
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
    if Assigned(VKContextManager.ServiceContext) and Assigned(ATask) then
    begin
      CheckSynchronize;
      with VKContextManager.FThreadTask.LockList do
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
          with TServiceContextThread(VKContextManager.FThread) do
          begin
            FLastTaskStartTime := GLSTime;
            FReported := False;
          end;
        finally
          VKContextManager.FThreadTask.UnlockList;
        end;
      VKContextManager.ServiceStarter.SetEvent;
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
      ShowMessage('Service thread task raised exception');
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
  VKContextManager := TVKContextManager.Create;

finalization

  VKContextManager.Terminate;
  vContextClasses.Free;
  vContextClasses := nil;

end.



