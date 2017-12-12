//
// VXScene Component Library, based on GLScene http://glscene.sourceforge.net
//
{
  Prototypes and base implementation of TVXContext.
  The history is logged in a former version of the unit.
}
unit VXS.Context;

interface

{$I VXScene.inc}

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

  VXS.OpenGL1x,
  VXS.Generics,
  VXS.CrossPlatform,
  VXS.VectorGeometry,
  VXS.Strings,
  VXS.VectorTypes,
  VXS.State,
  VXS.PipelineTransformation,
  VXS.TextureFormat;

// Buffer ID's for Multiple-Render-Targets (using GL_ATI_draw_buffers)
const
  MRT_BUFFERS: array [0 .. 3] of Cardinal = (GL_FRONT_LEFT, GL_AUX0, GL_AUX1, GL_AUX2);

type
  TVXRCOption = (rcoDoubleBuffered, rcoStereo, rcoDebug, rcoOGL_ES);
  TVXRCOptions = set of TVXRCOption;

  TVXContextLayer = (clUnderlay2, clUnderlay1, clMainPlane, clOverlay1, clOverlay2);

  TVXFinishTaskEvent = class(TEvent)
  public
    constructor Create; reintroduce;
  end;

  TVXTaskProcedure = procedure of object; stdcall;

  TVXServiceContextTask = record
    Task: TVXTaskProcedure;
    Event: TVXFinishTaskEvent;
  end;

  TVXContext = class;
  TVXContextManager = class;

  TVXAbstractMultitextureCoordinator = class(TObject)
  protected
    FOwner: TVXContext;
  public
    constructor Create(AOwner: TVXContext); virtual;
  end;

  TVXAbstractMultitextureCoordinatorClass = class of TVXAbstractMultitextureCoordinator;

  TVXContextAcceleration = (chaUnknown, chaHardware, chaSoftware);

  TVXAntiAliasing = (
    // Multisample Antialiasing
    aaDefault, aaNone, aa2x, aa2xHQ, aa4x, aa4xHQ, aa6x, aa8x, aa16x,
    // Coverage Sampling Antialiasing
    csa8x, csa8xHQ, csa16x, csa16xHQ);

  TVXSyncMode = (vsmSync, vsmNoSync);

  (* Wrapper around an OpenGL rendering context.
    The aim of this class is to offer platform-independant
    initialization, activation and management of OpenGL
    rendering context. The class also offers notifications
    event and error/problems detection.
    This is a virtual abstract a class, and platform-specific
    subclasses must be used. All rendering context share the same lists. *)
  TVXContext = class
  private
    FColorBits, FAlphaBits: Integer;
    FDepthBits: Integer;
    FStencilBits: Integer;
    FAccumBits: Integer;
    FAuxBuffers: Integer;
    FAntiAliasing: TVXAntiAliasing;
    FOptions: TVXRCOptions;
    FOnDestroyContext: TNotifyEvent;
    FManager: TVXContextManager;
    FActivationCount: Integer;
    FOwnedHandlesCount: Integer;
    FIsPraparationNeed: Boolean;
    procedure SetColorBits(const aColorBits: Integer); inline;
    procedure SetAlphaBits(const aAlphaBits: Integer); inline;
    procedure SetDepthBits(const val: Integer); inline;
    procedure SetStencilBits(const aStencilBits: Integer); inline;
    procedure SetAccumBits(const aAccumBits: Integer); inline;
    procedure SetAuxBuffers(const aAuxBuffers: Integer); inline;
    procedure SetOptions(const aOptions: TVXRCOptions); inline;
    procedure SetAntiAliasing(const val: TVXAntiAliasing); inline;
    procedure SetAcceleration(const val: TVXContextAcceleration); inline;
    function GetActive: Boolean; inline;
    procedure SetActive(const aActive: Boolean); inline;
    procedure SetLayer(const Value: TVXContextLayer); inline;
  protected
    FXVX: TVXAbstractMultitextureCoordinator;
    FVXStates: TVXStateCache;
    FTransformation: TVXTransformation;
    FAcceleration: TVXContextAcceleration;
    FLayer: TVXContextLayer;
{$IFDEF USE_MULTITHREAD}
    FSharedContexts: TThreadList;
    FLock: TCriticalSection;
{$ELSE}
    FSharedContexts: TList;
    FOwnedHandles : TList;
{$ENDIF}
    procedure PropagateSharedContext;
    procedure DoCreateContext(ADeviceHandle: THandle); virtual; abstract; // VCL -> HDC
    procedure DoCreateMemoryContext(OutputDevice: THandle; Width, Height: // VCL ->HWND
      Integer; BufferCount: Integer = 1); virtual; abstract;
    function DoShareLists(aContext: TVXContext): Boolean; virtual; abstract;
    procedure DoDestroyContext; virtual; abstract;
    procedure DoActivate; virtual; abstract;
    procedure DoDeactivate; virtual; abstract;
    class function ServiceContext: TVXContext;
    procedure MakeGLCurrent;
    function GetXGL: TVXAbstractMultitextureCoordinator;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    { An application-side cache of global per-context OpenGL states
      and parameters }
    property VXStates: TVXStateCache read FVXStates;
    property PipelineTransformation: TVXTransformation read FTransformation;
    // Context manager reference
    property Manager: TVXContextManager read FManager;
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
    { AntiAliasing option.  Ignored if not hardware supported, currently based on ARB_multisample. }
    property AntiAliasing: TVXAntiAliasing read FAntiAliasing write SetAntiAliasing;
    { Specifies the layer plane that the rendering context is bound to. }
    property Layer: TVXContextLayer read FLayer write SetLayer;
    { Rendering context options. }
    property Options: TVXRCOptions read FOptions write SetOptions;
    { Allows reading and defining the activity for the context.
      The methods of this property are just wrappers around calls to Activate and Deactivate. }
    property Active: Boolean read GetActive write SetActive;
    { Indicates if the context is hardware-accelerated. }
    property Acceleration: TVXContextAcceleration read FAcceleration write SetAcceleration;
    { Triggered whenever the context is destroyed.
      This events happens *before* the context has been
      actually destroyed, OpenGL resource cleanup can still occur here. }
    property OnDestroyContext: TNotifyEvent read FOnDestroyContext write FOnDestroyContext;
    { Creates the context.
      This method must be invoked before the context can be used. }
    procedure CreateContext(ADeviceHandle: THandle); overload; // VCL -> HDC
    { Creates an in-memory context.
      The function should fail if no hardware-accelerated memory context
      can be created (the CreateContext method can handle software OpenVX
      contexts). }
    procedure CreateMemoryContext(OutputDevice: THandle; Width, Height: // VCL -> HWND
      Integer; BufferCount: Integer = 1);
    { Setup display list sharing between two rendering contexts.
      Both contexts must have the same pixel format. }
    procedure ShareLists(aContext: TVXContext);
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
    { Call OnPrepare for all handles. }
    procedure PrepareHandlesData;
    { Returns true if the context is valid.
      A context is valid from the time it has been successfully
      created to the time of its destruction. }
    function IsValid: Boolean; virtual; abstract;
    { Request to swap front and back buffers if they were defined. }
    procedure SwapBuffers; virtual; abstract;
    { Returns the first compatible context that isn't self in the shares. }
    function FindCompatibleContext: TVXContext;
    procedure DestroyAllHandles;
    function RenderOutputDevice: Pointer; virtual; abstract;
    { Access to OpenGL command and extension. }
    /// property GL: TGLExtensionsAndEntryPoints read FGL; depricated from OpenGLAdapter
    property MultitextureCoordinator: TVXAbstractMultitextureCoordinator read GetXGL;
    property IsPraparationNeed: Boolean read FIsPraparationNeed;
  end;

  TVXContextClass = class of TVXContext;

  { Context with screen control property and methods.
    This variety of contexts is for drivers that access windows and OpenGL
    through an intermediate opaque cross-platform API.
    TVXSceneViewer won't use them, TVXMemoryViewer may be able to use them,
    but most of the time they will be accessed through a specific viewer
    class/subclass. }
  TVXScreenControlingContext = class(TVXContext)
  strict private
    FWidth, FHeight: Integer;
    FFullScreen: Boolean;
  protected
  public
    property Width: Integer read FWidth write FWidth;
    property Height: Integer read FHeight write FHeight;
    property FullScreen: Boolean read FFullScreen write FFullScreen;
  end;

  PVXRCHandle = ^TVXRCHandle;

  TVXRCHandle = record
    FRenderingContext: TVXContext;
    FHandle: Cardinal;
    FChanged: Boolean;
  end;

  TOnPrepareHandleData = procedure(aContext: TVXContext) of object;

  { Wrapper around an OpenGL context handle.
    This wrapper also takes care of context registrations and data releases
    related to context releases an cleanups. This is an abstract class,
    use the TVXListHandle and TVXTextureHandle subclasses. }
  TVXContextHandle = class
  private
    FHandles: TList;
    FLastHandle: PVXRCHandle;
    FOnPrepare: TOnPrepareHandleData;
    function GetHandle: Cardinal; inline;
    function GetContext: TVXContext;
    function SearchRC(aContext: TVXContext): PVXRCHandle;
    function RCItem(AIndex: Integer): PVXRCHandle; inline;
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
    property RenderingContext: TVXContext read GetContext;
    { Return True is data need update in current context. }
    function IsDataNeedUpdate: Boolean; inline;
    { Return True if data updated in all contexts. }
    function IsDataComplitelyUpdated: Boolean;
    { Notify the data was updated in current context. }
    procedure NotifyDataUpdated;
    { Notify the data was changed through all context. }
    procedure NotifyChangesOfData;
    // Checks if required extensions / OpenGL version are met
    class function IsSupported: Boolean; virtual;
    function IsAllocatedForContext(aContext: TVXContext = nil): Boolean;
    function IsShared: Boolean;
    function AllocateHandle: Cardinal;
    procedure DestroyHandle;
    property OnPrapare: TOnPrepareHandleData read FOnPrepare write FOnPrepare;
  end;

  TVXVirtualHandle = class;
  TVXVirtualHandleEvent = procedure(Sender: TVXVirtualHandle; var Handle: GLuint) of object;

  { A context handle with event-based handle allocation and destruction. }
  TVXVirtualHandle = class(TVXContextHandle)
  private
    FOnAllocate, FOnDestroy: TVXVirtualHandleEvent;
    FTag: Integer;
  protected
    function DoAllocateHandle: Cardinal; override;
    procedure DoDestroyHandle(var AHandle: Cardinal); override;
    class function Transferable: Boolean; override;
  public
    property OnAllocate: TVXVirtualHandleEvent read FOnAllocate write FOnAllocate;
    property OnDestroy: TVXVirtualHandleEvent read FOnDestroy write FOnDestroy;
    property Tag: Integer read FTag write FTag;
  end;

  { Transferable virtual handle. }
  TVXVirtualHandleTransf = class(TVXVirtualHandle)
  protected
    class function Transferable: Boolean; override;
  end;

  { Manages a handle to a display list. }
  TVXListHandle = class(TVXContextHandle)
  protected
    function DoAllocateHandle: GLuint; override;
    procedure DoDestroyHandle(var AHandle: GLuint); override;
    class function IsValid(const ID: GLuint): GLboolean; override;
  public
    procedure NewList(mode: Cardinal);
    procedure EndList;
    procedure CallList;
  end;

  { Manages a handle to a texture. }
  TVXTextureHandle = class(TVXContextHandle)
  private
    FTarget: TVXTextureTarget;
    procedure SetTarget(ATarget: TVXTextureTarget);
  protected
    function DoAllocateHandle: GLuint; override;
    procedure DoDestroyHandle(var AHandle: GLuint); override;
    class function IsValid(const ID: GLuint): GLboolean; override;
  public
    property Target: TVXTextureTarget read FTarget write SetTarget;
  end;

  { Manages a handle to a sampler. }
  TVXSamplerHandle = class(TVXContextHandle)
  protected
    function DoAllocateHandle: GLuint; override;
    procedure DoDestroyHandle(var AHandle: GLuint); override;
    class function IsValid(const ID: GLuint): GLboolean; override;
  public
    class function IsSupported: Boolean; override;
  end;

  { Manages a handle to a query.
    Do not use this class directly, use one of its subclasses instead. }
  TVXQueryHandle = class(TVXContextHandle)
  private
    FActive: Boolean;
  protected
    class function Transferable: Boolean; override;
    function DoAllocateHandle: GLuint; override;
    procedure DoDestroyHandle(var AHandle: GLuint); override;
    function GetTarget: GLuint; virtual; abstract;
    function GetQueryType: TVXQueryType; virtual; abstract;
    class function IsValid(const ID: GLuint): GLboolean; override;
  public
    procedure BeginQuery;
    procedure EndQuery;
    // Check if result is available from the query.  Result may not be available
    // immediately after ending the query
    function IsResultAvailable: Boolean;
    // Number of bits used to store the query result. eg. 32/64 bit
    function CounterBits: Integer;
    // Retrieve query result, may cause a stall if the result is not available yet
    function QueryResultInt: GLint;
    function QueryResultUInt: GLuint;
    function QueryResultInt64: GLint64EXT;
    function QueryResultUInt64: GLuint64EXT;
    function QueryResultBool: GLboolean;
    property Target: GLuint read GetTarget;
    property QueryType: TVXQueryType read GetQueryType;
    { True if within a Begin/EndQuery. }
    property Active: Boolean read FActive;
  end;

  { Manages a handle to an occlusion query.
    Requires OpenVX 1.5+
    Does *NOT* check for extension availability, this is assumed to have been
    checked by the user. }
  TVXOcclusionQueryHandle = class(TVXQueryHandle)
  protected
    function GetTarget: GLuint; override;
    function GetQueryType: TVXQueryType; override;
  public
    class function IsSupported: Boolean; override;
    // Number of samples (pixels) drawn during the query, some pixels may
    // be drawn to several times in the same query
    function PixelCount: Integer;
  end;

  TVXBooleanOcclusionQueryHandle = class(TVXQueryHandle)
  protected
    function GetTarget: GLuint; override;
    function GetQueryType: TVXQueryType; override;
  public
    class function IsSupported: Boolean; override;
  end;

  { Manages a handle to a timer query.
    Requires GL_EXT_timer_query extension.
    Does *NOT* check for extension availability, this is assumed to have been
    checked by the user. }
  TVXTimerQueryHandle = class(TVXQueryHandle)
  protected
    function GetTarget: GLuint; override;
    function GetQueryType: TVXQueryType; override;
  public
    class function IsSupported: Boolean; override;
    // Time, in nanoseconds (1 ns = 10^-9 s) between starting + ending the query.
    // with 32 bit integer can measure up to approximately 4 seconds, use
    // QueryResultUInt64 if you may need longer
    function Time: Integer;
  end;

  { Manages a handle to a primitive query.
    Requires OpenVX 3.0+
    Does *NOT* check for extension availability, this is assumed to have been
    checked by the user. }
  TVXPrimitiveQueryHandle = class(TVXQueryHandle)
  protected
    function GetTarget: GLuint; override;
    function GetQueryType: TVXQueryType; override;
  public
    class function IsSupported: Boolean; override;
    // Number of primitives (eg. Points, Triangles etc.) drawn whilst the
    // query was active
    function PrimitivesGenerated: Integer;
  end;

  { Manages a handle to a Buffer Object.
    Does *NOT* check for extension availability, this is assumed to have been
    checked by the user. }
  TVXBufferObjectHandle = class(TVXContextHandle)
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
      buffer objects and uniform buffer objects. (OpenVX 3.0+) }
    procedure BindRange(index: GLuint; offset: GLintptr; size: GLsizeiptr); virtual;
    { Equivalent to calling BindRange with offset = 0, and size = the size of buffer. }
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
    function MapBufferRange(offset: GLint; len: GLsizei; access: GLbitfield): Pointer;
    procedure Flush(offset: GLint; len: GLsizei);
    { Unmap buffer content from memory.
      Must follow a MapBuffer, and happen before the buffer is unbound. }
    function UnmapBuffer: GLboolean;
    class function IsSupported: Boolean; override;
    property Target: GLuint read GetTarget;
    property BufferSize: Integer read FSize;
  end;

  { Manages a handle to an Vertex Buffer Object.
    Does *NOT* check for extension availability, this is assumed to have been
    checked by the user.
    Do not use this class directly, use one of its subclasses instead. }
  TVXVBOHandle = class(TVXBufferObjectHandle)
  private
    function GetVBOTarget: GLuint;
  public
    property VBOTarget: GLuint read GetVBOTarget;
  end;

  { Manages a handle to VBO Array Buffer.
    Typically used to store vertices, normals, texcoords, etc. }
  TVXVBOArrayBufferHandle = class(TVXVBOHandle)
  protected
    function GetTarget: GLuint; override;
  public
    procedure Bind; override;
    procedure UnBind; override;
  end;

  { Manages a handle to VBO Element Array Buffer.
    Typically used to store vertex indices. }
  TVXVBOElementArrayHandle = class(TVXVBOHandle)
  protected
    function GetTarget: GLuint; override;
  public
    procedure Bind; override;
    procedure UnBind; override;
  end;

  { Manages a handle to PBO Pixel Pack Buffer.
    When bound, commands such as ReadPixels write
    their data into a buffer object. }
  TVXPackPBOHandle = class(TVXBufferObjectHandle)
  protected
    function GetTarget: GLuint; override;
  public
    procedure Bind; override;
    procedure UnBind; override;
    class function IsSupported: Boolean; override;
  end;

  { Manages a handle to PBO Pixel Unpack Buffer.
    When bound, commands such as DrawPixels read their data from a buffer object }
  TVXUnpackPBOHandle = class(TVXBufferObjectHandle)
  protected
    function GetTarget: GLuint; override;
  public
    procedure Bind; override;
    procedure UnBind; override;
    class function IsSupported: Boolean; override;
  end;

  { Manages a handle to a Transform Feedback Buffer Object.
    Transform feedback buffers can be used to capture vertex data from the
    vertex or geometry shader stage to perform further processing without
    going on to the fragment shader stage. }
  TVXTransformFeedbackBufferHandle = class(TVXBufferObjectHandle)
    // FTransformFeedbackBufferBuffer: array[0..15] of GLuint; // (0, 0, 0, ...)
    // FTransformFeedbackBufferStart: array[0..15] of GLuint64; // (0, 0, 0, ...)
    // FTransformFeedbackBufferSize: array[0..15] of GLuint64; // (0, 0, 0, ...)
  protected
    function GetTarget: GLuint; override;
  public
    procedure Bind; override;
    procedure UnBind; override;
    procedure BeginTransformFeedback(primitiveMode: GLenum);
    procedure EndTransformFeedback();
    procedure BindRange(index: GLuint; offset: GLintptr; size: GLsizeiptr); override;
    procedure BindBase(index: GLuint); override;
    procedure UnBindBase(index: GLuint); override;
    class function IsSupported: Boolean; override;
  end;

  { Manages a handle to a Buffer Texture. (TBO) }
  TVXTextureBufferHandle = class(TVXBufferObjectHandle)
  protected
    function GetTarget: GLuint; override;
  public
    procedure Bind; override;
    procedure UnBind; override;
    class function IsSupported: Boolean; override;
  end;

  { Manages a handle to a Uniform Buffer Object (UBO).
    Uniform buffer objects store "uniform blocks"; groups of uniforms
    that can be passed as a group into a GLSL program. }
  TVXUniformBufferHandle = class(TVXBufferObjectHandle)
    // FUniformBufferBuffer: array[0..15] of GLuint; // (0, 0, 0, ...)
    // FUniformBufferStart: array[0..15] of GLuint64; // (0, 0, 0, ...)
    // FUniformBufferSize: array[0..15] of GLuint64; // (0, 0, 0, ...)
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

  (* Manages a handle to a Vertex Array Object (VAO).
    Vertex array objects are used to rapidly switch between large sets
    of array state. *)
  TVXVertexArrayHandle = class(TVXContextHandle)
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

  TVXFramebufferStatus = (fsComplete, fsIncompleteAttachment, fsIncompleteMissingAttachment, fsIncompleteDuplicateAttachment,
    fsIncompleteDimensions, fsIncompleteFormats, fsIncompleteDrawBuffer, fsIncompleteReadBuffer, fsUnsupported,
    fsIncompleteMultisample, fsStatusError);

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
  TVXFramebufferHandle = class(TVXContextHandle)
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
    procedure Attach1DTexture(Target: GLenum; attachment: GLenum; textarget: GLenum; texture: GLuint; level: GLint);
    procedure Attach2DTexture(Target: GLenum; attachment: GLenum; textarget: GLenum; texture: GLuint; level: GLint);
    procedure Attach3DTexture(Target: GLenum; attachment: GLenum; textarget: GLenum; texture: GLuint; level: GLint; Layer: GLint);
    procedure AttachLayer(Target: GLenum; attachment: GLenum; texture: GLuint; level: GLint; Layer: GLint);
    procedure AttachRenderBuffer(Target: GLenum; attachment: GLenum; renderbuffertarget: GLenum; renderbuffer: GLuint);
    { If texture is the name of a three-dimensional texture, cube map texture, one-or
      two-dimensional array texture, or two-dimensional multisample array texture, the
      texture level attached to the framebuffer attachment point is an array of images,
      and the framebuffer attachment is considered layered. }
    procedure AttachTexture(Target: GLenum; attachment: GLenum; texture: GLuint; level: GLint);
    procedure AttachTextureLayer(Target: GLenum; attachment: GLenum; texture: GLuint; level: GLint; Layer: GLint);
    // copy rect from bound read framebuffer to bound draw framebuffer
    procedure Blit(srcX0: GLint; srcY0: GLint; srcX1: GLint; srcY1: GLint; dstX0: GLint; dstY0: GLint; dstX1: GLint;
      dstY1: GLint; mask: GLbitfield; filter: GLenum);
    // target = GL_DRAW_FRAMEBUFFER, GL_READ_FRAMEBUFFER, GL_FRAMEBUFFER (equivalent to GL_DRAW_FRAMEBUFFER)
    // If default framebuffer (0) is bound:
    // attachment = GL_FRONT_LEFT, GL_FRONT_RIGHT, GL_BACK_LEFT, or GL_BACK_RIGHT, GL_DEPTH, GL_STENCIL
    // if a framebuffer object is bound:
    // attachment = GL_COLOR_ATTACHMENTi, GL_DEPTH_ATTACHMENT, GL_STENCIL_ATTACHMENT, GL_DEPTH_STENCIL_ATTACHMENT
    // param = GL_FRAMEBUFFER_ATTACHMENT_(OBJECT_TYPE, OBJECT_NAME,
    // RED_SIZE, GREEN_SIZE, BLUE_SIZE, ALPHA_SIZE, DEPTH_SIZE, STENCIL_SIZE,
    // COMPONENT_TYPE, COLOR_ENCODING, TEXTURE_LEVEL, LAYERED, TEXTURE_CUBE_MAP_FACE, TEXTURE_LAYER
    function GetAttachmentParameter(Target: GLenum; attachment: GLenum; pname: GLenum): GLint;
    // Returns the type of object bound to attachment point:
    // GL_NONE, GL_FRAMEBUFFER_DEFAULT, GL_TEXTURE, or GL_RENDERBUFFER
    function GetAttachmentObjectType(Target: GLenum; attachment: GLenum): GLint;
    { Returns the name (ID) of the texture or renderbuffer attached to attachment point }
    function GetAttachmentObjectName(Target: GLenum; attachment: GLenum): GLint;
    function GetStatus: TVXFramebufferStatus;
    function GetStringStatus(out clarification: string): TVXFramebufferStatus;
    class function IsSupported: Boolean; override;
  end;

  { Manages a handle to a Renderbuffer Object.
    A Renderbuffer is a "framebuffer-attachable image" for generalized offscreen
    rendering and it also provides a means to support rendering to GL logical
    buffer types which have no corresponding texture format (stencil, accum, etc). }
  TVXRenderbufferHandle = class(TVXContextHandle)
  protected
    function DoAllocateHandle: Cardinal; override;
    procedure DoDestroyHandle(var AHandle: Cardinal); override;
    class function IsValid(const ID: GLuint): GLboolean; override;
  public
    procedure Bind;
    procedure UnBind;
    procedure SetStorage(internalformat: GLenum; Width, Height: GLsizei);
    procedure SetStorageMultisample(internalformat: GLenum; samples: GLsizei; Width, Height: GLsizei);
    class function IsSupported: Boolean; override;
  end;

  TVXProgramHandleEXT = class(TVXContextHandle)
  private
    FReady: Boolean;
    FInfoLog: string;
  protected
    function DoAllocateHandle: GLuint; override;
    procedure DoDestroyHandle(var AHandle: GLuint); override;
    class function IsValid(const ID: GLuint): GLboolean; override;
    class function GetTarget: GLenum; virtual; abstract;
  public
    procedure LoadARBProgram(AText: string);
    procedure Enable;
    procedure Disable;
    procedure Bind;
    property Ready: Boolean read FReady;
    property InfoLog: string read FInfoLog;
  end;

  TVXVertexProgramHandle = class(TVXProgramHandleEXT)
  protected
    class function GetTarget: GLenum; override;
  public
    class function IsSupported: Boolean; override;
  end;

  TVXFragmentProgramHandle = class(TVXProgramHandleEXT)
  protected
    class function GetTarget: GLenum; override;
  public
    class function IsSupported: Boolean; override;
  end;

  TVXGeometryProgramHandle = class(TVXProgramHandleEXT)
  protected
    class function GetTarget: GLenum; override;
  public
    class function IsSupported: Boolean; override;
  end;

  { Base class for GLSL handles (programs and shaders).
    Do not use this class directly, use one of its subclasses instead. }
  TVXSLHandle = class(TVXContextHandle)
  private
  protected
    procedure DoDestroyHandle(var AHandle: GLuint); override;
  public
    function InfoLog: string;
    class function IsSupported: Boolean; override;
  end;

  { Manages a handle to a Shader Object.
    Does *NOT* check for extension availability, this is assumed to have been
    checked by the user.
    Do not use this class directly, use one of its subclasses instead. }
  TVXShaderHandle = class(TVXSLHandle)
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

  TVXShaderHandleClass = class of TVXShaderHandle;

  { Manages a handle to a Vertex Shader Object. }
  TVXVertexShaderHandle = class(TVXShaderHandle)
  public
    constructor Create; override;
    class function IsSupported: Boolean; override;
  end;

  { Manages a handle to a Geometry Shader Object. }
  TVXGeometryShaderHandle = class(TVXShaderHandle)
  public
    constructor Create; override;
    class function IsSupported: Boolean; override;
  end;

  { Manages a handle to a Fragment Shader Object. }
  TVXFragmentShaderHandle = class(TVXShaderHandle)
  public
    constructor Create; override;
    class function IsSupported: Boolean; override;
  end;

  { Manages a handle to a Tessellation Control Shader Object. }
  TVXTessControlShaderHandle = class(TVXShaderHandle)
  public
    constructor Create; override;
    class function IsSupported: Boolean; override;
  end;

  { Manages a handle to a Tessellation Evaluation Shader Object. }
  TVXTessEvaluationShaderHandle = class(TVXShaderHandle)
  public
    constructor Create; override;
    class function IsSupported: Boolean; override;
  end;

  { Manages a GLSL Program Object.
    Does *NOT* check for extension availability, this is assumed to have been
    checked by the user. }
  TVXProgramHandle = class(TVXSLHandle)
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
    function GetUniformTextureHandle(const index: string; const TextureIndex: Integer; const TextureTarget: TVXTextureTarget): Cardinal;
    procedure SetUniformTextureHandle(const index: string; const TextureIndex: Integer; const TextureTarget: TVXTextureTarget; const Value: Cardinal);
    procedure SetUniformBuffer(const index: string; Value: TVXUniformBufferHandle);
  protected
    function DoAllocateHandle: GLuint; override;
  public
    property Name: string read FName write FName;
    constructor Create; override;
    { Compile and attach a new shader.
      Raises an EGLShader exception in case of failure. }
    procedure AddShader(ShaderType: TVXShaderHandleClass; const ShaderSource: string; treatWarningsAsErrors: Boolean = False);
    procedure AttachObject(shader: TVXShaderHandle);
    procedure DetachAllObject;
    procedure BindAttribLocation(index: Integer; const aName: string);
    procedure BindFragDataLocation(index: Integer; const aName: string);
    function LinkProgram: Boolean;
    function ValidateProgram: Boolean;
    function GetAttribLocation(const aName: string): GLint;
    function GetUniformLocation(const aName: string): GLint;
    function GetUniformOffset(const aName: string): PGLint;
    function GetUniformBlockIndex(const aName: string): GLint;
    function GetVaryingLocation(const aName: string): GLint;
    // Currently, NVidia-specific.
    procedure AddActiveVarying(const aName: string);
    // Currently, NVidia-specific.
    function GetUniformBufferSize(const aName: string): GLint;
    procedure UseProgramObject;
    procedure EndUseProgramObject;
    procedure SetUniformi(const index: string; const val: GLint); overload;
    procedure SetUniformi(const index: string; const val: TVector2i); overload;
    procedure SetUniformi(const index: string; const val: TVector3i); overload;
    procedure SetUniformi(const index: string; const val: TVector4i); overload;
    procedure SetUniformf(const index: string; const val: Single); overload;
    procedure SetUniformf(const index: string; const val: TVector2f); overload;
    procedure SetUniformf(const index: string; const val: TVector3f); overload;
    procedure SetUniformf(const index: string; const val: TVector4f); overload;
    { Shader parameters. }
    property Uniform1i[const index: string]: GLint read GetUniform1i write SetUniform1i;
    property Uniform2i[const index: string]: TVector2i read GetUniform2i write SetUniform2i;
    property Uniform3i[const index: string]: TVector3i read GetUniform3i write SetUniform3i;
    property Uniform4i[const index: string]: TVector4i read GetUniform4i write SetUniform4i;
    property Uniform1f[const index: string]: Single read GetUniform1f write SetUniform1f;
    property Uniform2f[const index: string]: TVector2f read GetUniform2f write SetUniform2f;
    property Uniform3f[const index: string]: TAffineVector read GetUniform3f write SetUniform3f;
    property Uniform4f[const index: string]: TVector read GetUniform4f write SetUniform4f;
    property UniformMatrix2fv[const index: string]: TMatrix2f read GetUniformMatrix2fv write SetUniformMatrix2fv;
    property UniformMatrix3fv[const index: string]: TMatrix3f read GetUniformMatrix3fv write SetUniformMatrix3fv;
    property UniformMatrix4fv[const index: string]: TMatrix read GetUniformMatrix4fv write SetUniformMatrix4fv;
    property UniformTextureHandle[const index: string; const TextureIndex: Integer; const TextureTarget: TVXTextureTarget]
      : GLuint read GetUniformTextureHandle write SetUniformTextureHandle;
    property UniformBuffer[const index: string]: TVXUniformBufferHandle write SetUniformBuffer;
  end;

  TVXContextNotification = record
    obj: TObject;
    Event: TNotifyEvent;
  end;

  { Stores and manages all the TVXContext objects. }
  TVXContextManager = class
  private
    FList: TThreadList;
    FTerminated: Boolean;
    FNotifications: array of TVXContextNotification;
    FCreatedRCCount: Integer;
    FHandles: TThreadList;
    FThread: TThread;
    FServiceStarter: TEvent;
    FServiceContext: TVXContext;
  protected
    procedure Lock;
    procedure UnLock;
    procedure RegisterContext(aContext: TVXContext);
    procedure UnRegisterContext(aContext: TVXContext);
    procedure ContextCreatedBy(aContext: TVXContext);
    procedure DestroyingContextBy(aContext: TVXContext);
    property ServiceContext: TVXContext read FServiceContext;
  public
    constructor Create;
    destructor Destroy; override;
    { Returns an appropriate, ready-to use context.
      The returned context should be freed by caller. }
    function CreateContext(AClass: TVXContextClass = nil): TVXContext;
    { Returns the number of TVXContext object.
      This is *not* the number of OpenVX rendering contexts! }
    function ContextCount: Integer;
    { Registers a new object to notify when the last context is destroyed.
      When the last rendering context is destroyed, the 'anEvent' will
      be invoked with 'anObject' as parameter.
      Note that the registration is kept until the notification is triggered
      or a RemoveNotification on 'anObject' is issued. }
    procedure LastContextDestroyNotification(anObject: TObject; anEvent: TNotifyEvent);
    { Unregisters an object from the notification lists. }
    procedure RemoveNotification(anObject: TObject);
    // Marks the context manager for termination
    procedure Terminate;
    { Request all contexts to destroy all their handles. }
    procedure DestroyAllHandles;
    { Notify all contexts about necessity of handles preparation. }
    procedure NotifyPreparationNeed;
  end;

  EVXContext = class(Exception);
  EPBuffer = class(Exception);
  EVXShader = class(EVXContext);

{ Drivers should register themselves via this function. }
procedure RegisterVXContextClass(aVXContextClass: TVXContextClass);
{ The TVXContext that is the currently active context, if any.
  Returns nil if no context is active. }
function CurrentVXContext: TVXContext;
function SafeCurrentVXContext: TVXContext;
function IsMainThread: Boolean;
function IsServiceContextAvaible: Boolean;
function GetServiceWindow: TForm;

var
  VXContextManager: TVXContextManager;
  vIgnoreOpenVXErrors: Boolean = False;
  vContextActivationFailureOccurred: Boolean = False;
  vMultitextureCoordinatorClass: TVXAbstractMultitextureCoordinatorClass;
  vCurrentContext: TVXContext;


// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------

var
  vContextClasses: TList;
  vServiceWindow: TForm;
  vMainThread: Boolean;

function CurrentVXContext: TVXContext; inline;
begin
  Result := vCurrentContext;
end;

function SafeCurrentVXContext: TVXContext; inline;
begin
  Result := CurrentVXContext;
  if not Assigned(Result) then
  begin
{$IFDEF USE_LOGGING}
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
  Result := VXContextManager.FHandles <> nil;
end;

function GetServiceWindow: TForm;
begin
  Result := vServiceWindow;
end;

procedure RegisterVXContextClass(aVXContextClass: TVXContextClass);
begin
  if not Assigned(vContextClasses) then
    vContextClasses := TList.Create;
  vContextClasses.Add(aVXContextClass);
end;

constructor TVXAbstractMultitextureCoordinator.Create(AOwner: TVXContext);
begin
  FOwner := AOwner;
end;

// ------------------
// ------------------ TVXContext ------------------
// ------------------

constructor TVXContext.Create;
begin
  inherited Create;
{$IFDEF USE_MULTITHREAD}
  FLock := TCriticalSection.Create;
{$ENDIF}
  FColorBits := 32;
  FStencilBits := 0;
  FAccumBits := 0;
  FAuxBuffers := 0;
  FLayer := clMainPlane;
  FOptions := [];
{$IFDEF USE_MULTITHREAD}
  FSharedContexts := TThreadList.Create;
{$ELSE}
  FSharedContexts := TList.Create;
  FOwnedHandles := TList.Create;
{$ENDIF}
  FSharedContexts.Add(Self);
  FAcceleration := chaUnknown;
  FVXStates := TVXStateCache.Create;
  FTransformation := TVXTransformation.Create;
  FTransformation.LoadMatricesEnabled := True;
  VXContextManager.RegisterContext(Self);
  FIsPraparationNeed := True;
end;

destructor TVXContext.Destroy;
begin
  if IsValid then
    DestroyContext;
  VXContextManager.UnRegisterContext(Self);
  FOwnedHandles.Free;
  
  FSharedContexts.Free;
  FVXStates.Free;
  FXVX.Free;
  FTransformation.Free;
  FSharedContexts.Free;
{$IFDEF USE_MULTITHREAD}
  FLock.Free;
{$ENDIF}
  inherited Destroy;
end;

procedure TVXContext.SetColorBits(const aColorBits: Integer);
begin
  if Active then
    raise EVXContext.Create(strCannotAlterAnActiveContext)
  else
    FColorBits := aColorBits;
end;

procedure TVXContext.SetAlphaBits(const aAlphaBits: Integer);
begin
  if Active then
    raise EVXContext.Create(strCannotAlterAnActiveContext)
  else
    FAlphaBits := aAlphaBits;
end;

procedure TVXContext.SetDepthBits(const val: Integer);
begin
  if Active then
    raise EVXContext.Create(strCannotAlterAnActiveContext)
  else
    FDepthBits := val;
end;

procedure TVXContext.SetLayer(const Value: TVXContextLayer);
begin
  if Active then
    raise EVXContext.Create(strCannotAlterAnActiveContext)
  else
    FLayer := Value;
end;

procedure TVXContext.SetStencilBits(const aStencilBits: Integer);
begin
  if Active then
    raise EVXContext.Create(strCannotAlterAnActiveContext)
  else
    FStencilBits := aStencilBits;
end;

procedure TVXContext.SetAccumBits(const aAccumBits: Integer);
begin
  if Active then
    raise EVXContext.Create(strCannotAlterAnActiveContext)
  else
    FAccumBits := aAccumBits;
end;

procedure TVXContext.SetAuxBuffers(const aAuxBuffers: Integer);
begin
  if Active then
    raise EVXContext.Create(strCannotAlterAnActiveContext)
  else
    FAuxBuffers := aAuxBuffers;
end;

procedure TVXContext.SetOptions(const aOptions: TVXRCOptions);
begin
  if Active then
    raise EVXContext.Create(strCannotAlterAnActiveContext)
  else
    FOptions := aOptions;
end;

procedure TVXContext.SetAntiAliasing(const val: TVXAntiAliasing);
begin
  if Active then
    raise EVXContext.Create(strCannotAlterAnActiveContext)
  else
    FAntiAliasing := val;
end;

procedure TVXContext.SetAcceleration(const val: TVXContextAcceleration);
begin
  if Active then
    raise EVXContext.Create(strCannotAlterAnActiveContext)
  else
    FAcceleration := val;
end;

function TVXContext.GetActive: Boolean;
begin
  Result := (FActivationCount > 0);
end;

procedure TVXContext.SetActive(const aActive: Boolean);
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

procedure TVXContext.CreateContext(ADeviceHandle: THandle);
begin
  if IsValid then
    raise EVXContext.Create(strContextAlreadyCreated);
  DoCreateContext(ADeviceHandle);
  Manager.ContextCreatedBy(Self);
end;

procedure TVXContext.CreateMemoryContext(OutputDevice: THandle; Width, Height: Integer; BufferCount: Integer);
begin
  if IsValid then
    raise EVXContext.Create(strContextAlreadyCreated);
  DoCreateMemoryContext(OutputDevice, Width, Height, BufferCount);
  Manager.ContextCreatedBy(Self);
end;

procedure TVXContext.PrepareHandlesData;
var
  I: Integer;
  LHandle: TVXContextHandle;
begin
  if vCurrentContext = Self then
  begin
{$IFNDEF USE_MULTITHREAD}
    for I := Manager.FHandles.LockList.Count - 1 downto 0 do
    begin
      LHandle := TVXContextHandle(Manager.FHandles.LockList[I]);
      if Assigned(LHandle.FOnPrepare) then
        LHandle.FOnPrepare(Self);
    end;
{$ELSE}
    with Manager.FHandles.LockList do
      try
        for I := Count - 1 downto 0 do
        begin
          LHandle := TVXContextHandle(Items[I]);
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

procedure TVXContext.PropagateSharedContext;
var
  I, j: Integer;
  otherContext: TVXContext;
  otherList: TList;
begin
{$IFNDEF USE_MULTITHREAD}
  with FSharedContexts do
  begin
    for I := 1 to Count - 1 do
    begin
      otherContext := TVXContext(Items[I]);
      otherList := otherContext.FSharedContexts;
      for j := 0 to otherList.Count - 1 do
        if IndexOf(otherList[j]) < 0 then
          Add(otherList[j]);
    end;
    for I := 1 to Count - 1 do
    begin
      otherContext := TVXContext(Items[I]);
      otherList := otherContext.FSharedContexts;
      if otherList.IndexOf(Self) < 0 then
        otherList.Add(Self);
    end;
  end;
{$ELSE}
  with FSharedContexts.LockList do
    try
      for I := 1 to Count - 1 do
      begin
        otherContext := TVXContext(Items[I]);
        otherList := otherContext.FSharedContexts.LockList;
        for j := 0 to otherList.Count - 1 do
          if IndexOf(otherList[j]) < 0 then
            Add(otherList[j]);
        otherContext.FSharedContexts.UnlockList;
      end;
      for I := 1 to Count - 1 do
      begin
        otherContext := TVXContext(Items[I]);
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

procedure TVXContext.ShareLists(aContext: TVXContext);
begin
{$IFNDEF USE_MULTITHREAD}
  if FSharedContexts.IndexOf(aContext) < 0 then
  begin
    if DoShareLists(aContext) then
    begin
      FSharedContexts.Add(aContext);
      PropagateSharedContext;
    end;
  end;
{$ELSE}
  with FSharedContexts.LockList do
    try
      if IndexOf(aContext) < 0 then
      begin
        if DoShareLists(aContext) then
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

procedure TVXContext.DestroyAllHandles;
var
  I: Integer;
begin
  Activate;
  try
{$IFNDEF USE_MULTITHREAD}
      for i:=FOwnedHandles.Count-1 downto 0 do
         TVXContextHandle(FOwnedHandles[i]).DestroyHandle;
{$ELSE}
    with Manager.FHandles.LockList do
      try
        for I := Count - 1 downto 0 do
          TVXContextHandle(Items[I]).ContextDestroying;
      finally
        Manager.FHandles.UnlockList;
      end;
{$ENDIF}
  finally
    Deactivate;
  end;
end;

procedure TVXContext.DestroyContext;
var
  I: Integer;
  oldContext, otherContext: TVXContext;
  contextHandle: TVXContextHandle;
  aList: TList;
begin

  if vCurrentContext <> Self then
  begin
    oldContext := vCurrentContext;
    if Assigned(oldContext) then
      oldContext.Deactivate;
  end
  else
    oldContext := nil;
  Activate;
  try
{$IFNDEF USE_MULTITHREAD}
    for I := Manager.FHandles.LockList.Count - 1 downto 0 do
    begin
      contextHandle := TVXContextHandle(Manager.FHandles.LockList[I]);
      contextHandle.ContextDestroying;
    end;
{$ELSE}
    aList := Manager.FHandles.LockList;
    try
      for I := aList.Count - 1 downto 0 do
      begin
        contextHandle := TVXContextHandle(aList[I]);
        contextHandle.ContextDestroying;
      end;
    finally
      Manager.FHandles.UnlockList;
    end;
{$ENDIF}
    Manager.DestroyingContextBy(Self);

{$IFDEF USE_MULTITHREAD}
    aList := FSharedContexts.LockList;
{$ELSE}
    aList := FSharedContexts;
{$ENDIF}
    for I := 1 to aList.Count - 1 do
    begin
      otherContext := TVXContext(aList[I]);
      otherContext.FSharedContexts.Remove(Self);
    end;
    FSharedContexts.Clear;
    FSharedContexts.Add(Self);
{$IFDEF USE_MULTITHREAD}
    FSharedContexts.UnlockList;
{$ENDIF}
    Active := False;
    DoDestroyContext;
  finally
    if Assigned(oldContext) then
      oldContext.Activate;
  end;
  FAcceleration := chaUnknown;
end;

procedure TVXContext.Activate;
begin
{$IFDEF USE_MULTITHREAD}
  FLock.Enter;
{$ENDIF}
  if FActivationCount = 0 then
  begin
    if not IsValid then
      raise EVXContext.Create(strContextNotCreated);

    vContextActivationFailureOccurred := False;
    try
      DoActivate;
    except
      vContextActivationFailureOccurred := True;
    end;
    vCurrentContext := Self;
  end
  else
    Assert(vCurrentContext = Self, 'vCurrentContext <> Self');
  Inc(FActivationCount);
end;

procedure TVXContext.Deactivate;
begin
  Assert(vCurrentContext = Self);
  Dec(FActivationCount);
  if FActivationCount = 0 then
  begin
    if not IsValid then
      raise EVXContext.Create(strContextNotCreated);
    if not vContextActivationFailureOccurred then
      DoDeactivate;
    vCurrentContext := nil;
  end
  else if FActivationCount < 0 then
    raise EVXContext.Create(strUnbalancedContexActivations);
{$IFDEF USE_MULTITHREAD}
  FLock.Leave;
{$ENDIF}
end;

function TVXContext.FindCompatibleContext: TVXContext;
var
  I: Integer;
begin
  Result := nil;
{$IFNDEF USE_MULTITHREAD}
  for I := 0 to FSharedContexts.Count - 1 do
    if TVXContext(FSharedContexts[I]) <> Self then
    begin
      Result := TVXContext(FSharedContexts[I]);
      Break;
    end;
{$ELSE}
  with FSharedContexts.LockList do
    try
      for I := 0 to Count - 1 do
        if TVXContext(Items[I]) <> Self then
        begin
          Result := TVXContext(Items[I]);
          Break;
        end;
    finally
      FSharedContexts.UnlockList;
    end;
{$ENDIF}
end;

class function TVXContext.ServiceContext: TVXContext;
begin
  Result := VXContextManager.FServiceContext;
end;

procedure TVXContext.MakeGLCurrent;
begin
//
end;

function TVXContext.GetXGL: TVXAbstractMultitextureCoordinator;
begin
  if FXVX = nil then
    FXVX := vMultitextureCoordinatorClass.Create(Self);
  Result := FXVX;
end;

// ------------------
// ------------------ TVXContextHandle ------------------
// ------------------

constructor TVXContextHandle.Create;
begin
  inherited Create;
  FHandles := TList.Create;
  // first is a dummy record
  new(FLastHandle);
  FillChar(FLastHandle^, sizeof(FLastHandle^), 0);
  FHandles.Add(FLastHandle);
  VXContextManager.FHandles.Add(Self);
end;

constructor TVXContextHandle.CreateAndAllocate(failIfAllocationFailed: Boolean = True);
begin
  Create;
  AllocateHandle;
  if failIfAllocationFailed and (Handle = 0) then
    raise EVXContext.Create('Auto-allocation failed');
end;

destructor TVXContextHandle.Destroy;
var
  I: Integer;
begin
  DestroyHandle;
  for I := 0 to FHandles.Count - 1 do
    Dispose(RCItem(I));
  FHandles.Free;
  if Assigned(VXContextManager) then
    VXContextManager.FHandles.Remove(Self);
  inherited Destroy;
end;

function TVXContextHandle.AllocateHandle: GLuint;
var
  I: Integer;
  bSucces: Boolean;
  aList: TList;
  p: PVXRCHandle;

begin
  // if handle aready allocated in current context
  Result := GetHandle;
  if Result <> 0 then
    exit;

  if vCurrentContext = nil then
  begin
    ShowMessage('Failed to allocate OpenGL identifier - no active rendering context!');
    exit;
  end;

  // add entry
  new(FLastHandle);
  FillChar(FLastHandle^, sizeof(FLastHandle^), 0);
  FHandles.Add(FLastHandle);
  FLastHandle.FRenderingContext := vCurrentContext;

  bSucces := False;
  if Transferable then
  begin
{$IFNDEF USE_MULTITHREAD}
    aList := vCurrentContext.FSharedContexts;
{$ELSE}
    aList := vCurrentGLContext.FSharedContexts.LockList;
    try
{$ENDIF}
      for I := aList.Count - 1 downto 0 do
      begin
        p := SearchRC(aList[I]);
        if (p.FHandle > 0) then
        begin
          // Copy shared handle
          // FLastHandle.FRenderingContext := vCurrentContext;
          FLastHandle.FHandle := p.FHandle;
          FLastHandle.FChanged := p.FChanged;
          Inc(vCurrentContext.FOwnedHandlesCount);
          bSucces := True;
          Break;
        end;
      end;
{$IFNDEF USE_MULTITHREAD}
{$ELSE}
    finally
      vCurrentContext.FSharedContexts.UnlockList;
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
      Inc(vCurrentContext.FOwnedHandlesCount);
  end;

  Result := FLastHandle.FHandle;
  if not bSucces then
    /// ShowMessages(cNoActiveRC)
  else if Assigned(FOnPrepare) then
    VXContextManager.NotifyPreparationNeed;
end;

function TVXContextHandle.IsAllocatedForContext(aContext: TVXContext = nil): Boolean;
begin
  Result := SearchRC(aContext).FHandle > 0;
end;

function TVXContextHandle.SearchRC(aContext: TVXContext): PVXRCHandle;
var
  I: Integer;
begin
  if aContext = nil then
    aContext := vCurrentContext;

  if aContext = FLastHandle.FRenderingContext then
  begin
    Result := FLastHandle;
    exit;
  end;

  for I := 1 to FHandles.Count - 1 do
    if RCItem(I).FRenderingContext = aContext then
    begin
      Result := RCItem(I);
      exit;
    end;

  // first handle is always a dummy
  Result := FHandles[0];
end;

procedure TVXContextHandle.CheckCurrentRC;
begin
  if vCurrentContext <> FLastHandle.FRenderingContext then
    FLastHandle := SearchRC(vCurrentContext);
end;

function TVXContextHandle.GetHandle: GLuint;
begin
  CheckCurrentRC;
  // inline doesn't always work... so optimize it here
  if vCurrentContext <> FLastHandle.FRenderingContext then
    FLastHandle := SearchRC(vCurrentContext);

  Result := FLastHandle.FHandle;
end;

procedure TVXContextHandle.DestroyHandle;
var
  oldContext: TVXContext;
  p: PVXRCHandle;
  I: Integer;
begin
  oldContext := vCurrentContext;
  if Assigned(oldContext) then
    oldContext.Deactivate;
  try
    for I := FHandles.Count - 1 downto 1 do
    begin
      p := FHandles[I];
      if p.FHandle > 0 then
      begin
        p.FRenderingContext.Activate;
        if IsValid(p.FHandle) then
          DoDestroyHandle(p.FHandle);
        Dec(p.FRenderingContext.FOwnedHandlesCount);
        p.FRenderingContext.Deactivate;
        p.FRenderingContext := nil;
        p.FHandle := 0;
        p.FChanged := True;
      end;
      Dispose(p);
    end;
    FHandles.Count := 1; // delete all in 1 step
    FLastHandle := FHandles[0];
  finally
    if Assigned(vCurrentContext) then
      vCurrentContext.Deactivate;
    if Assigned(oldContext) then
      oldContext.Activate;
  end;
end;

procedure TVXContextHandle.ContextDestroying;
var
  I: Integer;
  p: PVXRCHandle;
  aList: TList;
  bShared: Boolean;
begin
  if Assigned(vCurrentContext) then
  begin
    bShared := False;
    if Transferable then
    begin
{$IFNDEF USE_MULTITHREAD}
      aList := vCurrentContext.FSharedContexts;
{$ELSE}
      aList := vCurrentContext.FSharedContexts.LockList;
      try
{$ENDIF USE_MULTITHREAD}
        for I := FHandles.Count - 1 downto 1 do
        begin
          p := RCItem(I);
          if (p.FRenderingContext <> vCurrentContext) and (p.FHandle <> 0) and (aList.IndexOf(p.FRenderingContext) > -1) then
          begin
            bShared := True;
            Break;
          end;
        end;
{$IFDEF USE_MULTITHREAD}
      finally
        vCurrentContext.FSharedContexts.UnlockList;
      end;
{$ENDIF USE_MULTITHREAD}
    end;

    for I := FHandles.Count - 1 downto 1 do
    begin
      p := RCItem(I);
      if (p.FRenderingContext = vCurrentContext) and (p.FHandle <> 0) then
      begin
        if not bShared then
          if IsValid(p.FHandle) then
            DoDestroyHandle(p.FHandle);
        Dec(p.FRenderingContext.FOwnedHandlesCount);
        p.FHandle := 0;
        p.FRenderingContext := nil;
        p.FChanged := True;
        Dispose(p);
        FHandles.Delete(I);
        if FLastHandle = p then
          FLastHandle := FHandles[0];
        exit;
      end;
    end;
  end;
end;

function TVXContextHandle.GetContext: TVXContext;
var
  I: Integer;
  p: PVXRCHandle;
begin
  Result := nil;
  // Return first context where handle is allocated
  for I := FHandles.Count - 1 downto 1 do
  begin
    p := RCItem(I);
    if (p.FRenderingContext <> nil) and (p.FHandle <> 0) then
    begin
      Result := p.FRenderingContext;
      // If handle allocated in active context - return it
      if (Result = vCurrentContext) then
        exit;
    end;
  end;
end;

function TVXContextHandle.IsDataNeedUpdate: Boolean;
begin
  if GetHandle = 0 then
    CheckCurrentRC;
  Result := (FLastHandle.FHandle = 0) or FLastHandle.FChanged;
end;

function TVXContextHandle.IsDataComplitelyUpdated: Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := FHandles.Count - 1 downto 1 do
  begin
    with RCItem(I)^ do
      if (FRenderingContext <> nil) and (FHandle <> 0) and FChanged then
        exit;
  end;
  Result := True;
end;

procedure TVXContextHandle.NotifyDataUpdated;
var
  I: Integer;
  aList: TList;
begin
  if Assigned(vCurrentContext) then
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
{$IFNDEF USE_MULTITHREAD}
      aList := vCurrentContext.FSharedContexts;
{$ELSE}
      aList := vCurrentContext.FSharedContexts.LockList;
      try
{$ENDIF}
        for I := 0 to aList.Count - 1 do
        begin
          with SearchRC(aList[I])^ do
            if (FHandle <> 0) then
              FChanged := False;
        end;
{$IFDEF USE_MULTITHREAD}
      finally
        vCurrentContext.FSharedContexts.UnlockList;
      end;
{$ENDIF}
    end;
  end
  else
    /// ShowMessages(cNoActiveRC);
end;

function TVXContextHandle.RCItem(AIndex: Integer): PVXRCHandle;
begin
  Result := FHandles[AIndex];
end;

procedure TVXContextHandle.NotifyChangesOfData;
var
  I: Integer;
begin
  for I := FHandles.Count - 1 downto 1 do
    RCItem(I).FChanged := True;
  if Assigned(FOnPrepare) then
    VXContextManager.NotifyPreparationNeed;
end;

function TVXContextHandle.IsShared: Boolean;
var
  I: Integer;
  vContext: TVXContext;
  aList: TList;
begin
  Result := False;
  // untransferable handles can't be shared
  if not Transferable then
    exit;
  Result := True;
{$IFNDEF USE_MULTITHREAD}
  aList := vCurrentContext.FSharedContexts;
{$ELSE}
  aList := vCurrentContext.FSharedContexts.LockList;
  try
{$ENDIF}
    for I := 0 to aList.Count - 1 do
    begin
      vContext := aList[I];
      if (vContext <> vCurrentContext) and
      // at least one context is friendly
        (SearchRC(vContext).FHandle <> 0) then
        exit;
    end;
{$IFDEF USE_MULTITHREAD}
  finally
    vCurrentContext.FSharedContexts.UnlockList;
  end;
{$ENDIF}
  Result := False;
end;

class function TVXContextHandle.Transferable: Boolean;
begin
  Result := True;
end;

class function TVXContextHandle.IsValid(const ID: GLuint): GLboolean;
begin
  Result := True;
end;

class function TVXContextHandle.IsSupported: Boolean;
begin
  Result := True;
end;

// ------------------
// ------------------ TVXVirtualHandle ------------------
// ------------------

function TVXVirtualHandle.DoAllocateHandle: GLuint;
begin
  Result := 0;
  if Assigned(FOnAllocate) then
    FOnAllocate(Self, Result);
end;

procedure TVXVirtualHandle.DoDestroyHandle(var AHandle: GLuint);
begin
  if not vContextActivationFailureOccurred then
  begin
    // reset error status
    glGetError;
    // delete
    if Assigned(FOnDestroy) then
      FOnDestroy(Self, AHandle);
    // check for error
    CheckOpenGLError;
  end;
end;

class function TVXVirtualHandle.Transferable: Boolean;
begin
  Result := False;
end;

// ------------------
{ TVXVirtualHandleTransf }
// ------------------

class function TVXVirtualHandleTransf.Transferable: Boolean;
begin
  Result := True;
end;

// ------------------
// ------------------ TVXListHandle ------------------
// ------------------

function TVXListHandle.DoAllocateHandle: GLuint;
begin
  Result := glGenLists(1);
end;

procedure TVXListHandle.DoDestroyHandle(var AHandle: GLuint);
begin
  if not vContextActivationFailureOccurred then
  begin
    // reset error status
    glGetError;
    // delete
    glDeleteLists(AHandle, 1);
    // check for error
    CheckOpenGLError;
  end;
end;

class function TVXListHandle.IsValid(const ID: GLuint): GLboolean;
begin
  Result := glIsList(ID);
end;

procedure TVXListHandle.NewList(mode: Cardinal);
begin
  vCurrentContext.VXStates.NewList(GetHandle, mode);
end;

procedure TVXListHandle.EndList;
begin
  vCurrentContext.VXStates.EndList;
end;

procedure TVXListHandle.CallList;
begin
  vCurrentContext.VXStates.CallList(GetHandle);
end;

// ------------------
// ------------------ TVXTextureHandle ------------------
// ------------------

function TVXTextureHandle.DoAllocateHandle: Cardinal;
begin
  Result := 0;
  glGenTextures(1, @Result);
  FTarget := ttNoShape;
end;

procedure TVXTextureHandle.DoDestroyHandle(var AHandle: GLuint);
var
  a: GLint;
  t: TVXTextureTarget;
begin
  if not vContextActivationFailureOccurred then
  // reset error status
  CheckOpenGLError;
  { Unbind identifier from all image selectors. }
  with GetContext.VXStates do
  begin
    for a := 0 to MaxTextureImageUnits - 1 do
      for t := Low(TVXTextureTarget) to High(TVXTextureTarget) do
        if TextureBinding[a, t] = AHandle then
          TextureBinding[a, t] := 0;
  end;
  glDeleteTextures(1, @AHandle);
  // check for error
  CheckOpenGLError;
end;

class function TVXTextureHandle.IsValid(const ID: GLuint): GLboolean;
begin
  Result := glIsTexture(ID);
end;

procedure TVXTextureHandle.SetTarget(ATarget: TVXTextureTarget);
begin
  if FTarget = ttNoShape then
    FTarget := ATarget;
end;

// ------------------
// ------------------ TVXSamplerHandle ------------------
// ------------------

function TVXSamplerHandle.DoAllocateHandle: Cardinal;
begin
  Result := 0;
  glGenSamplers(1, @Result);
end;

procedure TVXSamplerHandle.DoDestroyHandle(var AHandle: GLuint);
begin
  if not vContextActivationFailureOccurred then
  // reset error status
  glGetError;
  // delete
  glDeleteSamplers(1, @AHandle);
  // check for error
  CheckOpenGLError;
end;

class function TVXSamplerHandle.IsSupported: Boolean;
begin
  Result := GL_ARB_sampler_objects;
end;

class function TVXSamplerHandle.IsValid(const ID: GLuint): GLboolean;
begin
  Result := glIsSampler(ID);
end;

// ------------------
// ------------------ TVXQueryHandle ------------------
// ------------------

procedure TVXQueryHandle.BeginQuery;
begin
  if vCurrentContext.VXStates.CurrentQuery[QueryType] = 0 then
    vCurrentContext.VXStates.BeginQuery(QueryType, GetHandle);
  FActive := True;
end;

function TVXQueryHandle.CounterBits: Integer;
begin
  glGetQueryiv(Target, GL_QUERY_COUNTER_BITS, @Result);
end;

function TVXQueryHandle.DoAllocateHandle: Cardinal;
begin
  Result := 0;
  glGenQueries(1, @Result);
end;

procedure TVXQueryHandle.DoDestroyHandle(var AHandle: GLuint);
begin
  if not vContextActivationFailureOccurred then
  begin
    // reset error status
    CheckOpenGLError;
    // delete
    glDeleteQueries(1, @AHandle);
    // check for error
    CheckOpenGLError;
  end;
end;

class function TVXQueryHandle.IsValid(const ID: GLuint): GLboolean;
begin
  Result := glIsQuery(ID);
end;

procedure TVXQueryHandle.EndQuery;
begin
  Assert(FActive = True, 'Cannot end a query before it begins');
  FActive := False;
  Assert(Handle <> 0);
  // glEndQuery(Target);
  vCurrentContext.VXStates.EndQuery(QueryType);
end;

function TVXQueryHandle.IsResultAvailable: Boolean;
begin
  glGetQueryObjectiv(Handle, GL_QUERY_RESULT_AVAILABLE, @Result);
end;

function TVXQueryHandle.QueryResultInt: GLint;
begin
  glGetQueryObjectiv(Handle, GL_QUERY_RESULT, @Result);
end;

function TVXQueryHandle.QueryResultInt64: GLint64EXT;
begin
  glGetQueryObjecti64v(Handle, GL_QUERY_RESULT, @Result);
end;

function TVXQueryHandle.QueryResultUInt: GLuint;
begin
  glGetQueryObjectuiv(Handle, GL_QUERY_RESULT, @Result);
end;

function TVXQueryHandle.QueryResultUInt64: GLuint64EXT;
begin
  glGetQueryObjectui64v(Handle, GL_QUERY_RESULT, @Result);
end;

function TVXQueryHandle.QueryResultBool: GLboolean;
var
  I: GLuint;
begin
  glGetQueryObjectuiv(Handle, GL_QUERY_RESULT, @I);
  Result := True;
end;

class function TVXQueryHandle.Transferable: Boolean;
begin
  Result := False;
end;

// ------------------
// ------------------ TVXOcclusionQueryHandle ------------------
// ------------------

function TVXOcclusionQueryHandle.GetQueryType: TVXQueryType;
begin
  Result := qrySamplesPassed;
end;

function TVXOcclusionQueryHandle.GetTarget: GLuint;
begin
  Result := GL_SAMPLES_PASSED;
end;

class function TVXOcclusionQueryHandle.IsSupported: Boolean;
begin
  Result := GL_VERSION_1_5;
end;

function TVXOcclusionQueryHandle.PixelCount: Integer;
begin
  Result := QueryResultUInt;
end;

// ------------------
// ------------------ TVXBooleanOcclusionQueryHandle ------------------
// ------------------

function TVXBooleanOcclusionQueryHandle.GetQueryType: TVXQueryType;
begin
  Result := qryAnySamplesPassed;
end;

function TVXBooleanOcclusionQueryHandle.GetTarget: GLuint;
begin
  Result := GL_ANY_SAMPLES_PASSED;
end;

class function TVXBooleanOcclusionQueryHandle.IsSupported: Boolean;
begin
  Result := GL_ARB_occlusion_query2;
end;

// ------------------
// ------------------ TVXTimerQueryHandle ------------------
// ------------------

function TVXTimerQueryHandle.GetQueryType: TVXQueryType;
begin
  Result := qryTimeElapsed;
end;

function TVXTimerQueryHandle.GetTarget: GLuint;
begin
  Result := GL_TIME_ELAPSED;
end;

class function TVXTimerQueryHandle.IsSupported: Boolean;
begin
  Result := GL_EXT_timer_query or GL_ARB_timer_query;
end;

function TVXTimerQueryHandle.Time: Integer;
begin
  Result := QueryResultUInt;
end;

// ------------------
// ------------------ TVXPrimitiveQueryHandle ------------------
// ------------------

function TVXPrimitiveQueryHandle.GetQueryType: TVXQueryType;
begin
  Result := qryPrimitivesGenerated;
end;

function TVXPrimitiveQueryHandle.GetTarget: GLuint;
begin
  Result := GL_PRIMITIVES_GENERATED;
end;

class function TVXPrimitiveQueryHandle.IsSupported: Boolean;
begin
  Result := GL_VERSION_3_0;
end;

function TVXPrimitiveQueryHandle.PrimitivesGenerated: Integer;
begin
  Result := QueryResultUInt;
end;

// ------------------
// ------------------ TVXBufferObjectHandle ------------------
// ------------------

constructor TVXBufferObjectHandle.CreateFromData(p: Pointer; size: Integer; bufferUsage: GLuint);
begin
  Create;
  AllocateHandle;
  Bind;
  BufferData(p, size, bufferUsage);
  UnBind;
end;

function TVXBufferObjectHandle.DoAllocateHandle: Cardinal;
begin
  Result := 0;
  glGenBuffers(1, @Result);
end;

procedure TVXBufferObjectHandle.DoDestroyHandle(var AHandle: GLuint);
begin
  if not vContextActivationFailureOccurred then
  begin
    // reset error status
    glGetError;
    UnBind;
    // delete
    glDeleteBuffers(1, @AHandle);
    // check for error
    CheckOpenGLError;
  end;
end;

class function TVXBufferObjectHandle.IsValid(const ID: GLuint): GLboolean;
begin
  Result := glIsBuffer(ID);
end;

class function TVXBufferObjectHandle.IsSupported: Boolean;
begin
  Result := GL_ARB_vertex_buffer_object;
end;

procedure TVXBufferObjectHandle.BindRange(index: GLuint; offset: GLintptr; size: GLsizeiptr);
begin
  Assert(False, 'BindRange only XBO and UBO');
end;

procedure TVXBufferObjectHandle.BindBase(index: GLuint);
begin
  Assert(False, 'BindRange only XBO and UBO');
end;

procedure TVXBufferObjectHandle.UnBindBase(index: GLuint);
begin
  Assert(False, 'BindRange only XBO and UBO');
end;

procedure TVXBufferObjectHandle.BufferData(p: Pointer; size: Integer; bufferUsage: GLuint);
begin
  FSize := size;
  glBufferData(Target, size, p, bufferUsage);
end;

procedure TVXBufferObjectHandle.BindBufferData(p: Pointer; size: Integer; bufferUsage: GLuint);
begin
  Bind;
  FSize := size;
  glBufferData(Target, size, p, bufferUsage);
end;

procedure TVXBufferObjectHandle.BufferSubData(offset, size: Integer; p: Pointer);
begin
  Assert(offset + size <= FSize);
  glBufferSubData(Target, offset, size, p);
end;

function TVXBufferObjectHandle.MapBuffer(access: GLuint): Pointer;
begin
  Result := glMapBuffer(Target, access);
end;

function TVXBufferObjectHandle.MapBufferRange(offset: GLint; len: GLsizei; access: GLbitfield): Pointer;
begin
  Result := glMapBufferRange(Target, offset, len, access);
end;

procedure TVXBufferObjectHandle.Flush(offset: GLint; len: GLsizei);
begin
  glFlushMappedBufferRange(Target, offset, len);
end;

function TVXBufferObjectHandle.UnmapBuffer: GLboolean;
begin
  Result := glUnmapBuffer(Target);
end;

// ------------------
// ------------------ TVXVBOHandle ------------------
// ------------------

function TVXVBOHandle.GetVBOTarget: GLuint;
begin
  Result := Target;
end;

// ------------------
// ------------------ TVXVBOArrayBufferHandle ------------------
// ------------------

procedure TVXVBOArrayBufferHandle.Bind;
begin
  vCurrentContext.VXStates.ArrayBufferBinding := Handle;
end;

procedure TVXVBOArrayBufferHandle.UnBind;
begin
  vCurrentContext.VXStates.ArrayBufferBinding := 0;
end;

function TVXVBOArrayBufferHandle.GetTarget: GLuint;
begin
  Result := GL_ARRAY_BUFFER;
end;

// ------------------
// ------------------ TVXVBOElementArrayHandle ------------------
// ------------------

procedure TVXVBOElementArrayHandle.Bind;
begin
  vCurrentContext.VXStates.ElementBufferBinding := Handle;
end;

procedure TVXVBOElementArrayHandle.UnBind;
begin
  vCurrentContext.VXStates.ElementBufferBinding := 0;
end;

function TVXVBOElementArrayHandle.GetTarget: GLuint;
begin
  Result := GL_ELEMENT_ARRAY_BUFFER;
end;

// ------------------
// ------------------ TVXPackPBOHandle ------------------
// ------------------

procedure TVXPackPBOHandle.Bind;
begin
  vCurrentContext.VXStates.PixelPackBufferBinding := Handle;
end;

procedure TVXPackPBOHandle.UnBind;
begin
  vCurrentContext.VXStates.PixelPackBufferBinding := 0;
end;

function TVXPackPBOHandle.GetTarget: GLuint;
begin
  Result := GL_PIXEL_PACK_BUFFER;
end;

class function TVXPackPBOHandle.IsSupported: Boolean;
begin
  Result := GL_ARB_pixel_buffer_object;
end;

// ------------------
// ------------------ TVXUnpackPBOHandle ------------------
// ------------------

procedure TVXUnpackPBOHandle.Bind;
begin
  vCurrentContext.VXStates.PixelUnpackBufferBinding := Handle;
end;

procedure TVXUnpackPBOHandle.UnBind;
begin
  vCurrentContext.VXStates.PixelUnpackBufferBinding := 0;
end;

function TVXUnpackPBOHandle.GetTarget: GLuint;
begin
  Result := GL_PIXEL_UNPACK_BUFFER;
end;

class function TVXUnpackPBOHandle.IsSupported: Boolean;
begin
  Result := GL_ARB_pixel_buffer_object;
end;

// ------------------
// ------------------ TVXTransformFeedbackBufferHandle ------------------
// ------------------

procedure TVXTransformFeedbackBufferHandle.Bind;
begin
  vCurrentContext.VXStates.TransformFeedbackBufferBinding := Handle;
end;

procedure TVXTransformFeedbackBufferHandle.UnBind;
begin
  vCurrentContext.VXStates.TransformFeedbackBufferBinding := 0;
end;

function TVXTransformFeedbackBufferHandle.GetTarget: GLuint;
begin
  Result := GL_TRANSFORM_FEEDBACK_BUFFER;
end;

procedure TVXTransformFeedbackBufferHandle.BeginTransformFeedback(primitiveMode: GLenum);
begin
  glBeginTransformFeedback(primitiveMode);
end;

procedure TVXTransformFeedbackBufferHandle.EndTransformFeedback();
begin
  glEndTransformFeedback();
end;

procedure TVXTransformFeedbackBufferHandle.BindRange(index: GLuint; offset: GLintptr; size: GLsizeiptr);
begin
  vCurrentContext.VXStates.SetBufferIndexedBinding(Handle, bbtTransformFeedBack, index, offset, size);
end;

procedure TVXTransformFeedbackBufferHandle.BindBase(index: GLuint);
begin
  vCurrentContext.VXStates.SetBufferIndexedBinding(Handle, bbtTransformFeedBack, index, BufferSize);
end;

procedure TVXTransformFeedbackBufferHandle.UnBindBase(index: GLuint);
begin
  vCurrentContext.VXStates.SetBufferIndexedBinding(0, bbtTransformFeedBack, index, 0);
end;

class function TVXTransformFeedbackBufferHandle.IsSupported: Boolean;
begin
  Result := GL_EXT_transform_feedback or GL_VERSION_3_0;
end;

// ------------------
// ------------------ TVXTextureBufferHandle ------------------
// ------------------

procedure TVXTextureBufferHandle.Bind;
begin
  vCurrentContext.VXStates.TextureBufferBinding := Handle;
end;

procedure TVXTextureBufferHandle.UnBind;
begin
  vCurrentContext.VXStates.TextureBufferBinding := 0;
end;

function TVXTextureBufferHandle.GetTarget: GLuint;
begin
  Result := GL_TEXTURE_BUFFER;
end;

class function TVXTextureBufferHandle.IsSupported: Boolean;
begin
  Result := GL_VERSION_3_1;
end;

// ------------------
// ------------------ TVXUniformBufferHandle ------------------
// ------------------

procedure TVXUniformBufferHandle.Bind;
begin
  vCurrentContext.VXStates.UniformBufferBinding := Handle;
end;

procedure TVXUniformBufferHandle.UnBind;
begin
  vCurrentContext.VXStates.UniformBufferBinding := 0;
end;

procedure TVXUniformBufferHandle.BindRange(index: GLuint; offset: GLintptr; size: GLsizeiptr);
begin
  vCurrentContext.VXStates.SetBufferIndexedBinding(Handle, bbtUniform, index, offset, size);
end;

procedure TVXUniformBufferHandle.BindBase(index: GLuint);
begin
  vCurrentContext.VXStates.SetBufferIndexedBinding(Handle, bbtUniform, index, BufferSize);
end;

procedure TVXUniformBufferHandle.UnBindBase(index: GLuint);
begin
  vCurrentContext.VXStates.SetBufferIndexedBinding(0, bbtUniform, index, 0);
end;

function TVXUniformBufferHandle.GetTarget: GLuint;
begin
  Result := GL_UNIFORM_BUFFER;
end;

class function TVXUniformBufferHandle.IsSupported: Boolean;
begin
  Result := GL_ARB_uniform_buffer_object;
end;

// ------------------
// ------------------ TVXVertexArrayHandle ------------------
// ------------------

function TVXVertexArrayHandle.DoAllocateHandle: Cardinal;
begin
  Result := 0;
  glGenVertexArrays(1, @Result);
end;

procedure TVXVertexArrayHandle.DoDestroyHandle(var AHandle: GLuint);
begin
  if not vContextActivationFailureOccurred then
  begin
    // reset error status
    glGetError;
    // delete
    glDeleteVertexArrays(1, @AHandle);
    // check for error
    CheckOpenGLError;
  end;
end;

class function TVXVertexArrayHandle.IsValid(const ID: GLuint): GLboolean;
begin
  Result := glIsVertexArray(ID);
end;

procedure TVXVertexArrayHandle.Bind;
begin
  Assert(vCurrentContext <> nil);
  vCurrentContext.VXStates.VertexArrayBinding := Handle;
end;

procedure TVXVertexArrayHandle.UnBind;
begin
  Assert(vCurrentContext <> nil);
  vCurrentContext.VXStates.VertexArrayBinding := 0;
end;

class function TVXVertexArrayHandle.IsSupported: Boolean;
begin
  Result := GL_ARB_vertex_array_object;
end;

class function TVXVertexArrayHandle.Transferable: Boolean;
begin
  Result := False;
end;

// ------------------
// ------------------ TVXFramebufferHandle ------------------
// ------------------

function TVXFramebufferHandle.DoAllocateHandle: Cardinal;
begin
  Result := 0;
  glGenFramebuffers(1, @Result)
end;

procedure TVXFramebufferHandle.DoDestroyHandle(var AHandle: GLuint);
begin
  if not vContextActivationFailureOccurred then
  begin
    // reset error status
    glGetError;
    // delete
    glDeleteFramebuffers(1, @AHandle);
    // check for error
    { CheckError; }
  end;
end;

class function TVXFramebufferHandle.IsValid(const ID: GLuint): GLboolean;
begin
  Result := glIsFramebuffer(ID);
end;

procedure TVXFramebufferHandle.Bind;
begin
  Assert(vCurrentContext <> nil);
  vCurrentContext.VXStates.SetFrameBuffer(Handle);
end;

procedure TVXFramebufferHandle.BindForDrawing;
begin
  Assert(vCurrentContext <> nil);
  vCurrentContext.VXStates.DrawFrameBuffer := Handle;
end;

procedure TVXFramebufferHandle.BindForReading;
begin
  Assert(vCurrentContext <> nil);
  vCurrentContext.VXStates.ReadFrameBuffer := Handle;
end;

procedure TVXFramebufferHandle.UnBind;
begin
  Assert(vCurrentContext <> nil);
  vCurrentContext.VXStates.SetFrameBuffer(0);
end;

procedure TVXFramebufferHandle.UnBindForDrawing;
begin
  Assert(vCurrentContext <> nil);
  vCurrentContext.VXStates.DrawFrameBuffer := 0;
end;

procedure TVXFramebufferHandle.UnBindForReading;
begin
  Assert(vCurrentContext <> nil);
  vCurrentContext.VXStates.ReadFrameBuffer := 0;
end;

procedure TVXFramebufferHandle.Attach1DTexture(Target: GLenum; attachment: GLenum; textarget: GLenum; texture: GLuint;
  level: GLint);
begin
  glFramebufferTexture1D(Target, attachment, textarget, texture, level);
end;

procedure TVXFramebufferHandle.Attach2DTexture(Target: GLenum; attachment: GLenum; textarget: GLenum; texture: GLuint;
  level: GLint);
begin
  glFramebufferTexture2D(Target, attachment, textarget, texture, level);
end;

procedure TVXFramebufferHandle.Attach3DTexture(Target: GLenum; attachment: GLenum; textarget: GLenum; texture: GLuint;
  level: GLint; Layer: GLint);
begin
  glFramebufferTexture3D(Target, attachment, textarget, texture, level, Layer);
end;

procedure TVXFramebufferHandle.AttachLayer(Target: GLenum; attachment: GLenum; texture: GLuint; level: GLint; Layer: GLint);
begin
  glFramebufferTextureLayer(Target, attachment, texture, level, Layer);
end;

procedure TVXFramebufferHandle.AttachRenderBuffer(Target: GLenum; attachment: GLenum; renderbuffertarget: GLenum;
  renderbuffer: GLuint);
begin
  glFramebufferRenderbuffer(Target, attachment, renderbuffertarget, renderbuffer);
end;

procedure TVXFramebufferHandle.AttachTexture(Target: GLenum; attachment: GLenum; texture: GLuint; level: GLint);
begin
  glFramebufferTexture(Target, attachment, texture, level);
end;

procedure TVXFramebufferHandle.AttachTextureLayer(Target: GLenum; attachment: GLenum; texture: GLuint; level: GLint;
  Layer: GLint);
begin
  glFramebufferTextureLayer(Target, attachment, texture, level, Layer);
end;

procedure TVXFramebufferHandle.Blit(srcX0: GLint; srcY0: GLint; srcX1: GLint; srcY1: GLint; dstX0: GLint; dstY0: GLint;
  dstX1: GLint; dstY1: GLint; mask: GLbitfield; filter: GLenum);
begin
  glBlitFramebuffer(srcX0, srcY0, srcX1, srcY1, dstX0, dstY0, dstX1, dstY1, mask, filter);
end;

function TVXFramebufferHandle.GetAttachmentParameter(Target: GLenum; attachment: GLenum; pname: GLenum): GLint;
begin
  glGetFramebufferAttachmentParameteriv(Target, attachment, pname, @Result)
end;

function TVXFramebufferHandle.GetAttachmentObjectType(Target: GLenum; attachment: GLenum): GLint;
begin
  glGetFramebufferAttachmentParameteriv(Target, attachment, GL_FRAMEBUFFER_ATTACHMENT_OBJECT_TYPE, @Result);
end;

function TVXFramebufferHandle.GetAttachmentObjectName(Target: GLenum; attachment: GLenum): GLint;
begin
  glGetFramebufferAttachmentParameteriv(Target, attachment, GL_FRAMEBUFFER_ATTACHMENT_OBJECT_NAME, @Result);
end;

function TVXFramebufferHandle.GetStatus: TVXFramebufferStatus;
var
  Status: Cardinal;
begin
  Status := glCheckFramebufferStatus(GL_FRAMEBUFFER);

  case Status of
    GL_FRAMEBUFFER_COMPLETE_EXT:
      Result := fsComplete;
    GL_FRAMEBUFFER_INCOMPLETE_ATTACHMENT_EXT:
      Result := fsIncompleteAttachment;
    GL_FRAMEBUFFER_INCOMPLETE_MISSING_ATTACHMENT_EXT:
      Result := fsIncompleteMissingAttachment;
    GL_FRAMEBUFFER_INCOMPLETE_DIMENSIONS_EXT:
      Result := fsIncompleteDimensions;
    GL_FRAMEBUFFER_INCOMPLETE_FORMATS_EXT:
      Result := fsIncompleteFormats;
    GL_FRAMEBUFFER_INCOMPLETE_DRAW_BUFFER_EXT:
      Result := fsIncompleteDrawBuffer;
    GL_FRAMEBUFFER_INCOMPLETE_READ_BUFFER_EXT:
      Result := fsIncompleteReadBuffer;
    GL_FRAMEBUFFER_UNSUPPORTED_EXT:
      Result := fsUnsupported;
    GL_FRAMEBUFFER_INCOMPLETE_MULTISAMPLE:
      Result := fsIncompleteMultisample;
  else
    Result := fsStatusError;
  end;
end;

function TVXFramebufferHandle.GetStringStatus(out clarification: string): TVXFramebufferStatus;
const
  cFBOStatus: array [TVXFramebufferStatus] of string = ('Complete', 'Incomplete attachment', 'Incomplete missing attachment',
    'IncompleteDuplicateAttachment', 'Incomplete dimensions', 'Incomplete formats', 'Incomplete draw buffer',
    'Incomplete read buffer', 'Unsupported', 'Incomplite multisample', 'Status Error');
begin
  Result := GetStatus;
  clarification := cFBOStatus[Result];
end;

class function TVXFramebufferHandle.IsSupported: Boolean;
begin
  Result := GL_EXT_framebuffer_object or GL_ARB_framebuffer_object;
end;

class function TVXFramebufferHandle.Transferable: Boolean;
begin
  Result := False;
end;

// ------------------
// ------------------ TVXRenderbufferObject ------------------
// ------------------

function TVXRenderbufferHandle.DoAllocateHandle: Cardinal;
begin
  Result := 0;
  glGenRenderbuffers(1, @Result);
end;

procedure TVXRenderbufferHandle.DoDestroyHandle(var AHandle: GLuint);
begin
  if not vContextActivationFailureOccurred then
  begin
    // reset error status
    glGetError;
    // delete
    glDeleteRenderbuffers(1, @AHandle);
    // check for error
    CheckOpenGLError;
  end;
end;

class function TVXRenderbufferHandle.IsValid(const ID: GLuint): GLboolean;
begin
  Result := glIsRenderbuffer(ID);
end;

procedure TVXRenderbufferHandle.Bind;
begin
  vCurrentContext.VXStates.renderbuffer := GetHandle;
end;

procedure TVXRenderbufferHandle.UnBind;
begin
  if vCurrentContext <> nil then
    vCurrentContext.VXStates.renderbuffer := 0;
end;

procedure TVXRenderbufferHandle.SetStorage(internalformat: GLenum; Width, Height: GLsizei);
begin
  glRenderbufferStorage(GL_RENDERBUFFER, internalformat, Width, Height);
end;

procedure TVXRenderbufferHandle.SetStorageMultisample(internalformat: GLenum; samples: GLsizei; Width, Height: GLsizei);
begin
  glRenderbufferStorageMultisample(GL_RENDERBUFFER, samples, internalformat, Width, Height);
end;

class function TVXRenderbufferHandle.IsSupported: Boolean;
begin
  Result := GL_EXT_framebuffer_object or GL_ARB_framebuffer_object;
end;

// ------------------
// ------------------ TVXProgramHandleEXT ------------------
// ------------------

function TVXProgramHandleEXT.DoAllocateHandle: Cardinal;
begin
  Result := 0;
  glGenProgramsARB(1, @Result);
  FReady := False;
end;

procedure TVXProgramHandleEXT.DoDestroyHandle(var AHandle: GLuint);
begin
  if not vContextActivationFailureOccurred then
  begin
    // reset error status
    glGetError;
    // delete
    glDeleteProgramsARB(1, @AHandle);
    // check for error
    CheckOpenGLError;
  end;
end;

class function TVXProgramHandleEXT.IsValid(const ID: GLuint): GLboolean;
begin
  Result := glIsProgram(ID);
end;

procedure TVXProgramHandleEXT.LoadARBProgram(AText: string);
const
  cProgType: array [0 .. 2] of string = ('ARB vertex', 'ARB fragment', 'NV geometry');
var
  errPos, p: Integer;
begin
  Bind;
  glProgramStringARB(GetTarget, GL_PROGRAM_FORMAT_ASCII_ARB, Length(AText), PGLChar(AText));
  glGetIntegerv(GL_PROGRAM_ERROR_POSITION_ARB, @errPos);
  if errPos > -1 then
  begin
    FInfoLog := string(glGetString(GL_PROGRAM_ERROR_STRING_ARB));
    case GetTarget of
      GL_VERTEX_PROGRAM_ARB:
        p := 0;
      GL_FRAGMENT_PROGRAM_ARB:
        p := 1;
    else
      p := 2;
    end;
    /// ShowMessages(Format('%s Program Error - [Pos: %d][Error %s]', [cProgType[P], errPos, FInfoLog]));
    FReady := False;
  end
  else
  begin
    FReady := True;
    FInfoLog := '';
  end;
end;

procedure TVXProgramHandleEXT.Enable;
begin
  if FReady then
    glEnable(GetTarget)
  else
    Abort;
end;

procedure TVXProgramHandleEXT.Disable;
begin
  glDisable(GetTarget);
end;

procedure TVXProgramHandleEXT.Bind;
begin
  glBindProgramARB(GetTarget, Handle);
end;

class function TVXVertexProgramHandle.GetTarget: GLenum;
begin
  Result := GL_VERTEX_PROGRAM_ARB;
end;

class function TVXVertexProgramHandle.IsSupported: Boolean;
begin
  Result := GL_ARB_vertex_program;
end;

class function TVXFragmentProgramHandle.GetTarget: GLenum;
begin
  Result := GL_FRAGMENT_PROGRAM_ARB;
end;

class function TVXFragmentProgramHandle.IsSupported: Boolean;
begin
  Result := GL_ARB_vertex_program;
end;

class function TVXGeometryProgramHandle.GetTarget: GLenum;
begin
  Result := GL_GEOMETRY_PROGRAM_NV;
end;

class function TVXGeometryProgramHandle.IsSupported: Boolean;
begin
  Result := GL_NV_geometry_program4;
end;

// ------------------
// ------------------ TVXSLHandle ------------------
// ------------------

procedure TVXSLHandle.DoDestroyHandle(var AHandle: GLuint);
begin
  if not vContextActivationFailureOccurred then
  begin
    // reset error status
    glGetError;
    // delete
    glDeleteObjectARB(AHandle);
    // check for error
    { CheckError; }
  end;
end;

function TVXSLHandle.InfoLog: string;
var
  maxLength: Integer;
  log: String;
  AHandle: GLuint;
begin
  maxLength := 0;
  AHandle := GetHandle;
  glGetObjectParameterivARB(AHandle, GL_OBJECT_INFO_LOG_LENGTH_ARB, @maxLength);
  SetLength(log, maxLength);
  if maxLength > 0 then
  begin
    glGetInfoLogARB(AHandle, maxLength, @maxLength, @log[1]);
    SetLength(log, maxLength);
  end;
  Result := string(log);
end;

class function TVXSLHandle.IsSupported: Boolean;
begin
  Result := GL_ARB_shader_objects;
end;

// ------------------
// ------------------ TVXShaderHandle ------------------
// ------------------

function TVXShaderHandle.DoAllocateHandle: GLuint;
begin
  Result := glCreateShader(FShaderType)
end;

class function TVXShaderHandle.IsValid(const ID: GLuint): GLboolean;
begin
  Result := glIsShader(ID);
end;

procedure TVXShaderHandle.ShaderSource(const source: AnsiString);
var
  p: PGLChar;
begin
  p := PGLChar(source);
  glShaderSource(GetHandle, 1, @p, nil);
end;

function TVXShaderHandle.CompileShader: Boolean;
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
// ------------------ TVXVertexShaderHandle ------------------
// ------------------

constructor TVXVertexShaderHandle.Create;
begin
  FShaderType := GL_VERTEX_SHADER_ARB;
  inherited;
end;

class function TVXVertexShaderHandle.IsSupported: Boolean;
begin
  Result := GL_ARB_vertex_shader;
end;

// ------------------
// ------------------ TVXGeometryShaderHandle ------------------
// ------------------

constructor TVXGeometryShaderHandle.Create;
begin
  FShaderType := GL_GEOMETRY_SHADER_EXT;
  inherited;
end;

class function TVXGeometryShaderHandle.IsSupported: Boolean;
begin
  Result := GL_EXT_geometry_shader4;
end;

// ------------------
// ------------------ TVXFragmentShaderHandle ------------------
// ------------------

constructor TVXFragmentShaderHandle.Create;
begin
  FShaderType := GL_FRAGMENT_SHADER_ARB;
  inherited;
end;

class function TVXFragmentShaderHandle.IsSupported: Boolean;
begin
  Result := GL_ARB_fragment_shader;
end;

// ------------------
// ------------------ TVXTessControlShaderHandle ------------------
// ------------------

constructor TVXTessControlShaderHandle.Create;
begin
  FShaderType := GL_TESS_CONTROL_SHADER;
  inherited;
end;

class function TVXTessControlShaderHandle.IsSupported: Boolean;
begin
  Result := GL_ARB_tessellation_shader;
end;

// ------------------
// ------------------ TVXTessEvaluationShaderHandle ------------------
// ------------------

constructor TVXTessEvaluationShaderHandle.Create;
begin
  FShaderType := GL_TESS_EVALUATION_SHADER;
  inherited;
end;

class function TVXTessEvaluationShaderHandle.IsSupported: Boolean;
begin
  Result := GL_ARB_tessellation_shader;
end;

// ------------------
// ------------------ TVXProgramHandle ------------------
// ------------------

function TVXProgramHandle.DoAllocateHandle: GLuint;
begin
  Result := glCreateProgram();
end;

class function TVXProgramHandle.IsValid(const ID: GLuint): GLboolean;
begin
  Result := glIsProgram(ID);
end;

procedure TVXProgramHandle.AddShader(ShaderType: TVXShaderHandleClass; const ShaderSource: string;
  treatWarningsAsErrors: Boolean = False);
var
  shader: TVXShaderHandle;
begin
  shader := ShaderType.CreateAndAllocate;
  try
    if shader.Handle = 0 then
      raise EVXShader.Create('Couldn''t allocate ' + ShaderType.ClassName);
    shader.ShaderSource(AnsiString(ShaderSource));
    if (not shader.CompileShader) or (treatWarningsAsErrors and (Pos('warning', LowerCase(shader.InfoLog)) > 0)) then
      raise EVXShader.Create(FName + ' (' + shader.ClassName + '): '#13#10 + shader.InfoLog);
    AttachObject(shader);
  finally
    shader.Free;
  end;
  glGetError;
end;

procedure TVXProgramHandle.AttachObject(shader: TVXShaderHandle);
begin
  glAttachShader(GetHandle, shader.Handle);
end;

procedure TVXProgramHandle.DetachAllObject;
var
  glH: Cardinal;
  I: Integer;
  Count: TGLsizei;
  buffer: array [0 .. 255] of Cardinal;
begin
  glH := GetHandle;
  if glH > 0 then
  begin
    glGetAttachedShaders(glH, Length(buffer), @Count, @buffer[0]);
    Count := MinInteger(Count, Length(buffer));
    for I := 0 to Count - 1 do
      glDetachShader(glH, buffer[I]);
    NotifyChangesOfData;
  end;
end;

procedure TVXProgramHandle.BindAttribLocation(index: Integer; const aName: string);
begin
  glBindAttribLocation(GetHandle, index, PGLChar(aName));
end;

procedure TVXProgramHandle.BindFragDataLocation(index: Integer; const aName: string);
begin
  glBindFragDataLocation(GetHandle, index, PGLChar(name));
end;

function TVXProgramHandle.LinkProgram: Boolean;
var
  Status: Integer;
  glH: GLuint;
begin
  glH := GetHandle;
  glLinkProgram(glH);
  Status := 0;
  glGetProgramiv(glH, GL_LINK_STATUS, @Status);
  Result := (Status <> 0);
end;

function TVXProgramHandle.ValidateProgram: Boolean;
var
  validated: Integer;
  h: Cardinal;
begin
  h := GetHandle;
  glValidateProgram(h);
  validated := 0;
  glGetProgramiv(h, GL_VALIDATE_STATUS, @validated);
  Result := (validated <> 0);
end;

function TVXProgramHandle.GetAttribLocation(const aName: string): Integer;
begin
  Result := glGetAttribLocation(GetHandle, PGLChar(aName));
  Assert(Result >= 0, Format(strUnknownParam, ['attrib', aName, Name]));
end;

function TVXProgramHandle.GetUniformLocation(const aName: string): Integer;
begin
  Result := glGetUniformLocation(GetHandle, PGLChar(aName));
  Assert(Result >= 0, Format(strUnknownParam, ['uniform', aName, Name]));
end;

function TVXProgramHandle.GetVaryingLocation(const aName: string): Integer;
begin
  Result := glGetVaryingLocationNV(GetHandle, PGLChar(aName));
  Assert(Result >= 0, Format(strUnknownParam, ['varying', aName, Name]));
end;

procedure TVXProgramHandle.AddActiveVarying(const aName: string);
begin
  glActiveVaryingNV(GetHandle, PGLChar(aName));
end;

procedure TVXProgramHandle.UseProgramObject;
begin
  Assert(vCurrentContext <> nil);
  vCurrentContext.VXStates.CurrentProgram := Handle;
end;

procedure TVXProgramHandle.EndUseProgramObject;
begin
  Assert(vCurrentContext <> nil);
  vCurrentContext.VXStates.CurrentProgram := 0;
end;

function TVXProgramHandle.GetUniform1i(const index: string): Integer;
begin
  glGetUniformiv(GetHandle, GetUniformLocation(index), @Result);
end;

function TVXProgramHandle.GetUniform2i(const index: string): TVector2i;
begin
  glGetUniformiv(GetHandle, GetUniformLocation(index), @Result);
end;

function TVXProgramHandle.GetUniform3i(const index: string): TVector3i;
begin
  glGetUniformiv(GetHandle, GetUniformLocation(index), @Result);
end;

function TVXProgramHandle.GetUniform4i(const index: string): TVector4i;
begin
  glGetUniformiv(GetHandle, GetUniformLocation(index), @Result);
end;

procedure TVXProgramHandle.SetUniform1f(const index: string; val: Single);
begin
  glUniform1f(GetUniformLocation(index), val);
end;

function TVXProgramHandle.GetUniform1f(const index: string): Single;
begin
  glGetUniformfv(GetHandle, GetUniformLocation(index), @Result);
end;

procedure TVXProgramHandle.SetUniform1i(const index: string; val: Integer);
begin
  glUniform1i(GetUniformLocation(index), val);
end;

procedure TVXProgramHandle.SetUniform2i(const index: string; const Value: TVector2i);
begin
  glUniform2i(GetUniformLocation(index), Value.X, Value.Y);
end;

procedure TVXProgramHandle.SetUniform3i(const index: string; const Value: TVector3i);
begin
  glUniform3i(GetUniformLocation(index), Value.X, Value.Y, Value.Z);
end;

procedure TVXProgramHandle.SetUniform4i(const index: string; const Value: TVector4i);
begin
  glUniform4i(GetUniformLocation(index), Value.X, Value.Y, Value.Z, Value.W);
end;

function TVXProgramHandle.GetUniform2f(const index: string): TVector2f;
begin
  glGetUniformfv(GetHandle, GetUniformLocation(index), @Result);
end;

procedure TVXProgramHandle.SetUniform2f(const index: string; const val: TVector2f);
begin
  glUniform2f(GetUniformLocation(index), val.X, val.Y);
end;

function TVXProgramHandle.GetUniform3f(const index: string): TAffineVector;
begin
  glGetUniformfv(GetHandle, GetUniformLocation(index), @Result);
end;

procedure TVXProgramHandle.SetUniform3f(const index: string; const val: TAffineVector);
begin
  glUniform3f(GetUniformLocation(index), val.X, val.Y, val.Z);
end;

function TVXProgramHandle.GetUniform4f(const index: string): TVector;
begin
  glGetUniformfv(GetHandle, GetUniformLocation(index), @Result);
end;

procedure TVXProgramHandle.SetUniform4f(const index: string; const val: TVector);
begin
  glUniform4f(GetUniformLocation(index), val.X, val.Y, val.Z, val.W);
end;

function TVXProgramHandle.GetUniformMatrix2fv(const index: string): TMatrix2f;
begin
  glGetUniformfv(GetHandle, GetUniformLocation(index), @Result);
end;

procedure TVXProgramHandle.SetUniformMatrix2fv(const index: string; const val: TMatrix2f);
begin
  glUniformMatrix2fv(GetUniformLocation(index), 1,  False , @val);
end;

function TVXProgramHandle.GetUniformMatrix3fv(const index: string): TMatrix3f;
begin
  glGetUniformfv(GetHandle, GetUniformLocation(index), @Result);
end;

procedure TVXProgramHandle.SetUniformMatrix3fv(const index: string; const val: TMatrix3f);
begin
  glUniformMatrix3fv(GetUniformLocation(index), 1,  False, @val);
end;

function TVXProgramHandle.GetUniformMatrix4fv(const index: string): TMatrix;
begin
  glGetUniformfv(GetHandle, GetUniformLocation(index), @Result);
end;

procedure TVXProgramHandle.SetUniformMatrix4fv(const index: string; const val: TMatrix);
begin
  glUniformMatrix4fv(GetUniformLocation(index), 1,  False, @val);
end;

procedure TVXProgramHandle.SetUniformf(const index: string; const val: Single);
begin
  SetUniform1f(index, val);
end;

procedure TVXProgramHandle.SetUniformf(const index: string; const val: TVector2f);
begin
  SetUniform2f(index, val);
end;

procedure TVXProgramHandle.SetUniformf(const index: string; const val: TVector3f);
begin
  SetUniform3f(index, val);
end;

procedure TVXProgramHandle.SetUniformf(const index: string; const val: TVector4f);
begin
  SetUniform4f(index, val);
end;

procedure TVXProgramHandle.SetUniformi(const index: string; const val: Integer);
begin
  SetUniform1f(index, val);
end;

procedure TVXProgramHandle.SetUniformi(const index: string; const val: TVector2i);
begin
  SetUniform2i(index, val);
end;

procedure TVXProgramHandle.SetUniformi(const index: string; const val: TVector3i);
begin
  SetUniform3i(index, val);
end;

procedure TVXProgramHandle.SetUniformi(const index: string; const val: TVector4i);
begin
  SetUniform4i(index, val);
end;

function TVXProgramHandle.GetUniformTextureHandle(const index: string; const TextureIndex: Integer;
  const TextureTarget: TVXTextureTarget): Cardinal;
begin
  Result := GetUniform1i(index);
end;

procedure TVXProgramHandle.SetUniformTextureHandle(const index: string; const TextureIndex: Integer;
  const TextureTarget: TVXTextureTarget; const Value: GLuint);
begin
  vCurrentContext.VXStates.TextureBinding[0, TextureTarget] := Value;
  SetUniform1i(index, TextureIndex);
end;

procedure TVXProgramHandle.SetUniformBuffer(const index: string; Value: TVXUniformBufferHandle);
begin
  glUniformBufferEXT(Handle, GetUniformLocation(index), Value.Handle);
end;

function TVXProgramHandle.GetUniformBufferSize(const aName: string): GLint;
begin
  Result := glGetUniformBufferSizeEXT(Handle, GetUniformLocation(aName));
end;

function TVXProgramHandle.GetUniformOffset(const aName: string): PGLint;
begin
  Result := glGetUniformOffsetEXT(Handle, GetUniformLocation(aName));
end;

function TVXProgramHandle.GetUniformBlockIndex(const aName: string): Integer;
begin
  Result := glGetUniformBlockIndex(Handle, PGLChar(aName));
  Assert(Result >= 0, Format(strUnknownParam, ['uniform block', aName, Name]));
end;

constructor TVXProgramHandle.Create;
begin
  inherited Create;
  FName := 'DefaultShaderName';
end;

// ------------------
// ------------------ TVXContextManager ------------------
// ------------------

constructor TVXContextManager.Create;
begin
  inherited Create;
  FHandles := TThreadList.Create;
  FList := TThreadList.Create;
end;

destructor TVXContextManager.Destroy;
begin
  FHandles.Free;
  FList.Free;
  inherited Destroy;
end;

function TVXContextManager.CreateContext(AClass: TVXContextClass): TVXContext;
begin
  if Assigned(AClass) then
  begin
    Result := AClass.Create;
    Result.FManager := Self;
  end
  else if Assigned(vContextClasses) and (vContextClasses.Count > 0) then
  begin
    Result := TVXContextClass(vContextClasses.Last).Create;
    Result.FManager := Self;
  end
  else
    Result := nil;
end;

{$IFDEF USE_SERVICE_CONTEXT}
procedure TVXContextManager.CreateContext(AClass: TVXContextClass): TVXContext;;
begin
  FServiceContext := CreateContext;
  FThreadTask := TVXServiceContextTaskList.Create;
  FServiceStarter := TFinishTaskEvent.Create;
  FThread := TServiceContextThread.Create;
  AddTaskForServiceContext(TServiceContextThread(FThread).DoCreateServiceContext);
end;

procedure TVXContextManager.QueueTaskDepleted;
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
    if (nowTime - FLastTaskStartTime > 30000) and not FReported then
    begin
      FReported := True;
      ShowMessage('Service context queue task depleted');
    end;
end;

{$ENDIF}

procedure TVXContextManager.Lock;
begin
  FList.LockList;
end;

procedure TVXContextManager.NotifyPreparationNeed;
var
  I: Integer;
  LList: TList;
begin
  LList := FList.LockList;
  try
    for I := LList.Count - 1 downto 0 do
      TVXContext(LList[I]).FIsPraparationNeed := True;
  finally
    FList.UnlockList;
  end;
end;

procedure TVXContextManager.UnLock;
begin
  FList.UnlockList;
end;

function TVXContextManager.ContextCount: Integer;
begin
  // try..finally just a waste of CPU here, if Count fails, the list is amok,
  // and so is the lock...
  Result := FList.LockList.Count;
  FList.UnlockList;
end;

procedure TVXContextManager.RegisterContext(aContext: TVXContext);
begin
  with FList.LockList do
    try
      if IndexOf(aContext) >= 0 then
        raise EVXContext.Create(strInvalidContextRegistration)
      else
        Add(aContext);
    finally
      FList.UnlockList;
    end;
end;

procedure TVXContextManager.UnRegisterContext(aContext: TVXContext);
begin
  with FList.LockList do
    try
      if IndexOf(aContext) < 0 then
        raise EVXContext.Create(strInvalidContextRegistration)
      else
        Remove(aContext);
    finally
      FList.UnlockList;
    end;
end;

procedure TVXContextManager.ContextCreatedBy(aContext: TVXContext);
begin
  Lock;
  try
    Inc(FCreatedRCCount);
  finally
    UnLock;
  end;
end;

procedure TVXContextManager.DestroyingContextBy(aContext: TVXContext);
var
  cn: TVXContextNotification;
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
        cn.Event(cn.obj);
      end;
    end;
  finally
    UnLock;
  end;
end;

procedure TVXContextManager.LastContextDestroyNotification(anObject: TObject; anEvent: TNotifyEvent);
begin
  Lock;
  try
    SetLength(FNotifications, Length(FNotifications) + 1);
    with FNotifications[High(FNotifications)] do
    begin
      obj := anObject;
      Event := anEvent;
    end;
  finally
    UnLock;
  end;
end;

procedure TVXContextManager.RemoveNotification(anObject: TObject);
var
  I: Integer;
  found: Boolean;
begin
  Lock;
  try
    found := False;
    I := Low(FNotifications);
    while I <= High(FNotifications) do
    begin
      if FNotifications[I].obj = anObject then
      begin
        found := True;
        while I <= High(FNotifications) do
        begin
          FNotifications[I] := FNotifications[I + 1];
          Inc(I);
        end;
        SetLength(FNotifications, Length(FNotifications) - 1);
        Break;
      end;
      Inc(I);
    end;
    if not found then
      raise EVXContext.Create(strInvalidNotificationRemoval);
  finally
    UnLock;
  end;
end;

procedure TVXContextManager.Terminate;
begin
  FTerminated := True;
  if ContextCount = 0 then
  begin
    VXContextManager := nil;
    Free;
  end;
end;

procedure TVXContextManager.DestroyAllHandles;
var
  I: Integer;
begin
  with FList.LockList do
    try
      for I := Count - 1 downto 0 do
        TVXContext(Items[I]).DestroyAllHandles;
    finally
      FList.UnlockList;
    end;
end;

constructor TVXFinishTaskEvent.Create;
begin
  inherited Create(nil, True, False, '');
end;

// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------

vMainThread := True;
VXContextManager := TVXContextManager.Create;

finalization

VXContextManager.Terminate;
vContextClasses.Free;
vContextClasses := nil;

end.
