//
// This unit is part of the DGLEngine Project, http://glscene.org
//
{ : DGLContextHandles<p>

  Prototypes and base implementation of TDGLContextHandles.<p>
  Interface For Managing OpenGL Buffers and Handles
  <ul>
  <li>Texture Buffer
  <li>Sampler Buffer
  <li>Render Buffer
  <li>FBO  : Frame Buffer Object
  <li>VBO  : Vertex Buffer Object
  <li>VAO  : Vertex Array Object
  <li>PBO  : Pixel Buffer Object
  <li>UBO  : Uniform Buffer Object
  <li>SSBO : Shader Sampler Buffer Object
  <li>Vertex Program
  <li>Fragment Program
  <li>Tesselate Program
  <li>Geometry Program
  <li>Shader Program
  </ul><p>

  <b>History: </b><font size=-1><ul>
  <li>21/12/15 - JD -  Simplyfied and Imported From GLScene
  </ul></font>
}
unit DGLContextHandles;

interface

{$I DGLEngine.inc}

uses
  // System
  Winapi.Windows,
  System.Classes, System.SysUtils, System.Types, System.SyncObjs,
  VCL.Forms, VCL.Controls, VCL.Consts,
  // DGLE
  DGLSLog, DGLTypes,
  DGLResStrings, dglOpenGL,
  DGLCrossPlatform, DGLContext, DGLState,
  DGLVectorTypes, DGLVectorLists, DGLVectorMaths,
  DGLTextureFormat;

Type

  EGLShader = class(Exception);

  TDGLVirtualHandle      = class;
  TDGLVirtualHandleEvent = procedure(Sender: TDGLVirtualHandle; var handle: TGLuint) of object;

  // TDGLVirtualHandle
  //
  { : A context handle with event-based handle allocation and destruction. }
  TDGLVirtualHandle = class(TDGLContextHandle)
  private
    { Private Declarations }
    FOnAllocate, FOnDestroy: TDGLVirtualHandleEvent;
    FTag:                    Integer;
  protected
    { Protected Declarations }
    function DoAllocateHandle: Cardinal; override;
    procedure DoDestroyHandle(var AHandle: TGLuint); override;
    class function Transferable: Boolean; override;
  public
    { Public Declarations }
    property OnAllocate: TDGLVirtualHandleEvent read FOnAllocate write FOnAllocate;
    property OnDestroy:  TDGLVirtualHandleEvent read FOnDestroy write FOnDestroy;

    property Tag: Integer read FTag write FTag;
  end;

  // TDGLVirtualHandleTransferable
  //
  { : Transferable virtual handle. }
  TDGLVirtualHandleTransferable = class(TDGLVirtualHandle)
  protected
    class function Transferable: Boolean; override;
  end;

//   // TDGLRenderHandle --> Replace the old TDGLListHandle
//
//  { : Base class to Manage  handles for rendering objects.<br>
//    Do not use this class directly, use one of its subclasses instead. }
//   TDGLCustomRenderHandle = class(TDGLContextHandle)
//   private
//   { Private Declarations }
//
//   protected
//   { Protected Declarations }
//   function DoAllocateHandle: Cardinal; override;
//   procedure DoDestroyHandle(var AHandle: TGLuint); override;
//   class function IsValid(const ID: GLuint): Boolean; override;
//   public
//   { Public Declarations }
//   procedure NewRender(mode: Cardinal);
//   procedure EndRender;
//   procedure Render;
//   end;

  // TDGLTextureHandle
  //
  { : Manages a handle to a texture.  }
  TDGLTextureHandle = class(TDGLContextHandle)
  private
    FTarget: TDGLTextureTarget;
    procedure SetTarget(ATarget: TDGLTextureTarget);
  protected
    { Protected Declarations }
    function DoAllocateHandle: Cardinal; override;
    procedure DoDestroyHandle(var AHandle: TGLuint); override;
    class function IsValid(const ID: GLuint): Boolean; override;
  public
    { Public Declarations }
    property Target: TDGLTextureTarget read FTarget write SetTarget;
  end;

  // TDGLSamplerHandle
  //
  { : Manages a handle to a sampler. }
  TDGLSamplerHandle = class(TDGLContextHandle)
  protected
    { Protected Declarations }
    function DoAllocateHandle: Cardinal; override;
    procedure DoDestroyHandle(var AHandle: TGLuint); override;
    class function IsValid(const ID: GLuint): Boolean; override;
  public
    { Public Declarations }
    class function IsSupported: Boolean; override;
  end;

  // TDGLQueryHandle
  //
  { : Manages a handle to a query.<br>
    Do not use this class directly, use one of its subclasses instead. }
  TDGLQueryHandle = class(TDGLContextHandle)
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
    function IsResultAvailable: Boolean;
    // Number of bits used to store the query result. eg. 32/64 bit
    function CounterBits: Integer;
    // Retrieve query result, may cause a stall if the result is not available yet
    function QueryResultInt: TGLint;
    function QueryResultUInt: TGLuint;
    function QueryResultInt64: TGLint64EXT;
    function QueryResultUInt64: TGLuint64EXT;
    function QueryResultBool: TGLBoolean;

    property Target: TGLuint read GetTarget;
    property QueryType: TQueryType read GetQueryType;

    { : True if within a Begin/EndQuery. }
    property Active: Boolean read FActive;
  end;

  // TDGLOcclusionQueryHandle
  //
  { : Manages a handle to an occlusion query.<br>
    Requires OpenGL 1.5+<br>
    Does *NOT* check for extension availability, this is assumed to have been
    checked by the user. }
  TDGLOcclusionQueryHandle = class(TDGLQueryHandle)
  protected
    function GetTarget: TGLuint; override;
    function GetQueryType: TQueryType; override;
  public
    class function IsSupported: Boolean; override;
    // Number of samples (pixels) drawn during the query, some pixels may
    // be drawn to several times in the same query
    function PixelCount: Integer;
  end;

  TGLBooleanOcclusionQueryHandle = class(TDGLQueryHandle)
  protected
    function GetTarget: TGLuint; override;
    function GetQueryType: TQueryType; override;
  public
    class function IsSupported: Boolean; override;
  end;

  // TDGLTimerQueryHandle
  //
  { : Manages a handle to a timer query.<br>
    Requires GL_EXT_timer_query extension.<br>
    Does *NOT* check for extension availability, this is assumed to have been
    checked by the user. }
  TDGLTimerQueryHandle = class(TDGLQueryHandle)
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

  // TDGLPrimitiveQueryHandle
  //
  { : Manages a handle to a primitive query.<br>
    Requires OpenGL 3.0+<br>
    Does *NOT* check for extension availability, this is assumed to have been
    checked by the user. }
  TDGLPrimitiveQueryHandle = class(TDGLQueryHandle)
  protected
    function GetTarget: TGLuint; override;
    function GetQueryType: TQueryType; override;
  public
    class function IsSupported: Boolean; override;
    // Number of primitives (eg. Points, Triangles etc.) drawn whilst the
    // query was active
    function PrimitivesGenerated: Integer;
  end;

  // TDGLBufferObjectHandle
  //
  { : Manages a handle to a Buffer Object.<br>
    Does *NOT* check for extension availability, this is assumed to have been
    checked by the user.<br> }
  TDGLBufferObjectHandle = class(TDGLContextHandle)
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
    { : Creates the buffer object buffer and initializes it. }
    constructor CreateFromData(p: Pointer; size: Integer; bufferUsage: TGLuint);

    procedure Bind; virtual; abstract;
    { : Note that it is not necessary to UnBind before Binding another buffer. }
    procedure UnBind; virtual; abstract;

    { : Bind a buffer object to an indexed target, used by transform feedback
      buffer objects and uniform buffer objects. (OpenGL 3.0+) }
    procedure BindRange(index: TGLuint; offset: TGLintptr; size: TGLsizeiptr); virtual;
    { : Equivalent to calling BindRange with offset = 0, and size = the size of buffer. }
    procedure BindBase(index: TGLuint); virtual;
    procedure UnBindBase(index: TGLuint); virtual;

    { : Specifies buffer content.<p>
      Common bufferUsage values are GL_STATIC_DRAW_ARB for data that will
      change rarely, but be used often, GL_STREAM_DRAW_ARB for data specified
      once but used only a few times, and GL_DYNAMIC_DRAW_ARB for data
      that is re-specified very often.<p>
      Valid only if the buffer has been bound. }
    procedure BufferData(p: Pointer; size: Integer; bufferUsage: TGLuint);
    // : Invokes Bind then BufferData
    procedure BindBufferData(p: Pointer; size: Integer; bufferUsage: TGLuint);
    { : Updates part of an already existing buffer.<p>
      offset and size indicate which part of the data in the buffer is
      to bo modified and p where the data should be taken from. }
    procedure BufferSubData(offset, size: Integer; p: Pointer);
    { : Map buffer content to memory.<p>
      Values for access are GL_READ_ONLY_ARB, GL_WRITE_ONLY_ARB and
      GL_READ_WRITE_ARB.<p>
      Valid only if the buffer has been bound, must be followed by
      an UnmapBuffer, only one buffer may be mapped at a time. }
    function MapBuffer(access: TGLuint): Pointer;
    function MapBufferRange(offset: TGLint; len: TGLsizei; access: TGLbitfield): Pointer;
    procedure Flush(offset: TGLint; len: TGLsizei);
    { : Unmap buffer content from memory.<p>
      Must follow a MapBuffer, and happen before the buffer is unbound. }
    function UnmapBuffer: Boolean;

    class function IsSupported: Boolean; override;

    property Target: TGLuint read GetTarget;
    property BufferSize: Integer read FSize;
  end;


  // TDGLVBOHandle
  //
  { : Manages a handle to an Vertex Buffer Object.<br>
    Does *NOT* check for extension availability, this is assumed to have been
    checked by the user.<br>
    Do not use this class directly, use one of its subclasses instead. }
  TDGLVBOHandle = class(TDGLBufferObjectHandle)
  private
    { Private Declarations }

    function GetVBOTarget: TGLuint;
  public

    property VBOTarget: TGLuint read GetVBOTarget;
  end;

  // TDGLVBOArrayBufferHandle
  //
  { : Manages a handle to VBO Array Buffer.<p>
    Typically used to store vertices, normals, texcoords, etc. }
  TDGLVBOArrayBufferHandle = class(TDGLVBOHandle)
  protected
    function GetTarget: TGLuint; override;
  public
    procedure Bind; override;
    procedure UnBind; override;
  end;

  // TDGLVBOElementArrayHandle
  //
  { : Manages a handle to VBO Element Array Buffer.<p>
    Typically used to store vertex indices. }
  TDGLVBOElementArrayHandle = class(TDGLVBOHandle)
  protected
    function GetTarget: TGLuint; override;
  public
    procedure Bind; override;
    procedure UnBind; override;
  end;

  // TDGLPackPBOHandle
  //
  { : Manages a handle to PBO Pixel Pack Buffer.<p>
    When bound, commands such as ReadPixels write
    their data into a buffer object. }
  TDGLPackPBOHandle = class(TDGLBufferObjectHandle)
  protected
    function GetTarget: TGLuint; override;
  public
    procedure Bind; override;
    procedure UnBind; override;
    class function IsSupported: Boolean; override;
  end;

  // TDGLUnpackPBOHandle
  //
  { : Manages a handle to PBO Pixel Unpack Buffer.<p>
    When bound, commands such as DrawPixels read
    their data from a buffer object. }
  TDGLUnpackPBOHandle = class(TDGLBufferObjectHandle)
  protected
    function GetTarget: TGLuint; override;
  public
    procedure Bind; override;
    procedure UnBind; override;
    class function IsSupported: Boolean; override;
  end;

  // TDGLTransformFeedbackBufferHandle
  //
  { : Manages a handle to a Transform Feedback Buffer Object.<p>
    Transform feedback buffers can be used to capture vertex data from the
    vertex or geometry shader stage to perform further processing without
    going on to the fragment shader stage. }
  TDGLTransformFeedbackBufferHandle = class(TDGLBufferObjectHandle)
    // FTransformFeedbackBufferBuffer: array[0..15] of TGLuint; // (0, 0, 0, ...)
    // FTransformFeedbackBufferStart: array[0..15] of TGLuint64; // (0, 0, 0, ...)
    // FTransformFeedbackBufferSize: array[0..15] of TGLuint64; // (0, 0, 0, ...)
  protected
    function GetTarget: TGLuint; override;
  public
    procedure Bind; override;
    procedure UnBind; override;
    procedure BeginTransformFeedback(primitiveMode: TGLEnum);
    procedure EndTransformFeedback();
    procedure BindRange(index: TGLuint; offset: TGLintptr; size: TGLsizeiptr); override;
    procedure BindBase(index: TGLuint); override;
    procedure UnBindBase(index: TGLuint); override;

    class function IsSupported: Boolean; override;
  end;

  // TDGLTextureBufferHandle
  //
  { : Manages a handle to a Buffer Texture. (TBO) }
  TDGLTextureBufferHandle = class(TDGLBufferObjectHandle)
  protected
    function GetTarget: TGLuint; override;
  public
    procedure Bind; override;
    procedure UnBind; override;
    class function IsSupported: Boolean; override;
  end;

  // TDGLUniformBufferHandle
  //
  { : Manages a handle to a Uniform Buffer Object (UBO).
    Uniform buffer objects store "uniform blocks"; groups of uniforms
    that can be passed as a group into a GLSL program. }
  TDGLUniformBufferHandle = class(TDGLBufferObjectHandle)
    // FUniformBufferBuffer: array[0..15] of TGLuint; // (0, 0, 0, ...)
    // FUniformBufferStart: array[0..15] of TGLuint64; // (0, 0, 0, ...)
    // FUniformBufferSize: array[0..15] of TGLuint64; // (0, 0, 0, ...)
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

  // TDGLVertexArrayHandle
  //
  { : Manages a handle to a Vertex Array Object (VAO).
    Vertex array objects are used to rapidly switch between large sets
    of array state. }
  TDGLVertexArrayHandle = class(TDGLContextHandle)
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


  // TDGLFramebufferHandle
  //
  { : Manages a handle to a Framebuffer Object (FBO).
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
  TDGLFramebufferHandle = class(TDGLContextHandle)
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
    { : Note that it is not necessary to unbind before binding another framebuffer. }
    procedure UnBind;
    procedure UnBindForDrawing;
    procedure UnBindForReading;
    // target = GL_DRAW_FRAMEBUFFER, GL_READ_FRAMEBUFFER, GL_FRAMEBUFFER (attach to both READ + DRAW)
    // attachment = COLOR_ATTACHMENTi, DEPTH_ATTACHMENT, STENCIL_ATTACHMENT, DEPTH_STENCIL_ATTACHMENT
    procedure Attach1DTexture(Target: TGLEnum; attachment: TGLEnum; textarget: TGLEnum; texture: TGLuint; level: TGLint);
    procedure Attach2DTexture(Target: TGLEnum; attachment: TGLEnum; textarget: TGLEnum; texture: TGLuint; level: TGLint);
    procedure Attach3DTexture(Target: TGLEnum; attachment: TGLEnum; textarget: TGLEnum; texture: TGLuint; level: TGLint; layer: TGLint);
    procedure AttachLayer(Target: TGLEnum; attachment: TGLEnum; texture: TGLuint; level: TGLint; layer: TGLint);
    procedure AttachRenderBuffer(Target: TGLEnum; attachment: TGLEnum; renderbuffertarget: TGLEnum; renderbuffer: TGLuint);
    // OpenGL 3.2+ only.
    // If texture is the name of a three-dimensional texture, cube map texture, one-or
    // two-dimensional array texture, or two-dimensional multisample array texture, the
    // texture level attached to the framebuffer attachment point is an array of images,
    // and the framebuffer attachment is considered layered.
    procedure AttachTexture(Target: TGLEnum; attachment: TGLEnum; texture: TGLuint; level: TGLint);
    // OpenGL 3.2+ only
    procedure AttachTextureLayer(Target: TGLEnum; attachment: TGLEnum; texture: TGLuint; level: TGLint; layer: TGLint);

    // copy rect from bound read framebuffer to bound draw framebuffer
    procedure Blit(srcX0: TGLint; srcY0: TGLint; srcX1: TGLint; srcY1: TGLint; dstX0: TGLint; dstY0: TGLint; dstX1: TGLint; dstY1: TGLint; mask: TGLbitfield; filter: TGLEnum);
    // target = GL_DRAW_FRAMEBUFFER, GL_READ_FRAMEBUFFER, GL_FRAMEBUFFER (equivalent to GL_DRAW_FRAMEBUFFER)
    // If default framebuffer (0) is bound:
    // attachment = GL_FRONT_LEFT, GL_FRONT_RIGHT, GL_BACK_LEFT, or GL_BACK_RIGHT, GL_DEPTH, GL_STENCIL
    // if a framebuffer object is bound:
    // attachment = GL_COLOR_ATTACHMENTi, GL_DEPTH_ATTACHMENT, GL_STENCIL_ATTACHMENT, GL_DEPTH_STENCIL_ATTACHMENT
    // param = GL_FRAMEBUFFER_ATTACHMENT_(OBJECT_TYPE, OBJECT_NAME,
    // RED_SIZE, GREEN_SIZE, BLUE_SIZE, ALPHA_SIZE, DEPTH_SIZE, STENCIL_SIZE,
    // COMPONENT_TYPE, COLOR_ENCODING, TEXTURE_LEVEL, LAYERED, TEXTURE_CUBE_MAP_FACE, TEXTURE_LAYER
    function GetAttachmentParameter(Target: TGLEnum; attachment: TGLEnum; pname: TGLEnum): TGLint;
    // Returns the type of object bound to attachment point:
    // GL_NONE, GL_FRAMEBUFFER_DEFAULT, GL_TEXTURE, or GL_RENDERBUFFER
    function GetAttachmentObjectType(Target: TGLEnum; attachment: TGLEnum): TGLint;
    // Returns the name (ID) of the texture or renderbuffer attached to attachment point
    function GetAttachmentObjectName(Target: TGLEnum; attachment: TGLEnum): TGLint;

    function GetStatus: TDGLFramebufferStatus;
    function GetStringStatus(out clarification: string): TDGLFramebufferStatus;

    class function IsSupported: Boolean; override;
  end;

  // TDGLRenderbufferHandle
  //
  { : Manages a handle to a Renderbuffer Object (FBO).
    A Renderbuffer is a "framebuffer-attachable image" for generalized offscreen
    rendering and it also provides a means to support rendering to GL logical
    buffer types which have no corresponding texture format (stencil, accum, etc). }
  TDGLRenderbufferHandle = class(TDGLContextHandle)
  protected
    function DoAllocateHandle: Cardinal; override;
    procedure DoDestroyHandle(var AHandle: TGLuint); override;
    class function IsValid(const ID: GLuint): Boolean; override;
  public
    procedure Bind;
    procedure UnBind;
    procedure SetStorage(internalformat: TGLEnum; width, height: TGLsizei);
    procedure SetStorageMultisample(internalformat: TGLEnum; samples: TGLsizei; width, height: TGLsizei);
    class function IsSupported: Boolean; override;
  end;

  TDGLARBProgramHandle = class(TDGLContextHandle)
  private
    { Private Declarations }
    FReady:   Boolean;
    FInfoLog: string;
  protected
    { Protected Declarations }
    function DoAllocateHandle: Cardinal; override;
    procedure DoDestroyHandle(var AHandle: TGLuint); override;
    class function IsValid(const ID: GLuint): Boolean; override;
    class function GetTarget: TGLEnum; virtual; abstract;
  public
    { Public Declarations }
    procedure LoadARBProgram(AText: string);
    procedure Enable;
    procedure Disable;
    procedure Bind;
    property Ready: Boolean read FReady;
    property InfoLog: string read FInfoLog;
  end;

  TDGLARBVertexProgramHandle = class(TDGLARBProgramHandle)
  protected
    { Protected Declarations }
    class function GetTarget: TGLEnum; override;
  public
    { Public Declarations }
    class function IsSupported: Boolean; override;
  end;

  TDGLARBFragmentProgramHandle = class(TDGLARBProgramHandle)
  protected
    { Protected Declarations }
    class function GetTarget: TGLEnum; override;
  public
    { Public Declarations }
    class function IsSupported: Boolean; override;
  end;

  TDGLARBGeometryProgramHandle = class(TDGLARBProgramHandle)
  protected
    { Protected Declarations }
    class function GetTarget: TGLEnum; override;
  public
    { Public Declarations }
    class function IsSupported: Boolean; override;
  end;

  // TDGLSLHandle
  //
  { : Base class for GLSL handles (programs and shaders).<p>
    Do not use this class directly, use one of its subclasses instead. }
  TDGLSLHandle = class(TDGLContextHandle)
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

  // TDGLShaderHandle
  //
  { : Manages a handle to a Shader Object.<br>
    Does *NOT* check for extension availability, this is assumed to have been
    checked by the user.<br>
    Do not use this class directly, use one of its subclasses instead. }
  TDGLShaderHandle = class(TDGLSLHandle)
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
    // : Returns True if compilation sucessful
    function CompileShader: Boolean;

    property ShaderType: Cardinal read FShaderType;
  end;

  TDGLShaderHandleClass = class of TDGLShaderHandle;

  // TDGLVertexShaderHandle
  //
  { : Manages a handle to a Vertex Shader Object. }
  TDGLVertexShaderHandle = class(TDGLShaderHandle)
  public
    { Public Declarations }
    constructor Create; override;
    class function IsSupported: Boolean; override;
  end;

  // TDGLGeometryShaderHandle
  //
  { : Manages a handle to a Geometry Shader Object. }
  TDGLGeometryShaderHandle = class(TDGLShaderHandle)
  public
    { Public Declarations }
    constructor Create; override;
    class function IsSupported: Boolean; override;
  end;

  // TDGLFragmentShaderHandle
  //
  { : Manages a handle to a Fragment Shader Object. }
  TDGLFragmentShaderHandle = class(TDGLShaderHandle)
  public
    { Public Declarations }
    constructor Create; override;
    class function IsSupported: Boolean; override;
  end;

  // TDGLTessControlShaderHandle
  //
  { : Manages a handle to a Tessellation Control Shader Object. }
  TDGLTessControlShaderHandle = class(TDGLShaderHandle)
  public
    { Public Declarations }
    constructor Create; override;
    class function IsSupported: Boolean; override;
  end;

  // TDGLTessEvaluationShaderHandle
  //
  { : Manages a handle to a Tessellation Evaluation Shader Object. }
  TDGLTessEvaluationShaderHandle = class(TDGLShaderHandle)
  public
    { Public Declarations }
    constructor Create; override;
    class function IsSupported: Boolean; override;
  end;

  // TDGLProgramHandle
  //
  { : Manages a GLSL Program Object.<br>
    Does *NOT* check for extension availability, this is assumed to have been
    checked by the user.<br> }
  TDGLProgramHandle = class(TDGLSLHandle)
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

    function GetUniformTextureHandle(const index: string; const TextureIndex: Integer; const TextureTarget: TDGLTextureTarget): Cardinal;
    procedure SetUniformTextureHandle(const index: string; const TextureIndex: Integer; const TextureTarget: TDGLTextureTarget; const Value: Cardinal);
    procedure SetUniformBuffer(const index: string; Value: TDGLUniformBufferHandle);
  protected
    { Protected Declarations }
    function DoAllocateHandle: Cardinal; override;

  public
    { Public Declarations }
    property Name: string read FName write FName;

    constructor Create; override;

    { : Compile and attach a new shader.<p>
      Raises an EGLShader exception in case of failure. }
    procedure AddShader(ShaderType: TDGLShaderHandleClass; const ShaderSource: string; treatWarningsAsErrors: Boolean = False);

    procedure AttachObject(shader: TDGLShaderHandle);
    procedure DetachAllObject;

    function GetAttribLocation(const aName: string): Integer;
    procedure BindAttribLocation(index: Integer; const aName: string);
    procedure BindFragDataLocation(index: Integer; const aName: string);

    // Generic
    // procedure SetAttribLocation(const Location: Integer; const Buffer: Pointer);

    function LinkProgram: Boolean;
    function ValidateProgram: Boolean;

    function GetUniformLocation(const aName: string): Integer;
    function GetUniformOffset(const aName: string):GLintptr;// PGLInt;
    function GetUniformBlockIndex(const aName: string): Integer;

//    function GetVaryingLocation(const aName: string): Integer;
    // Currently, NVidia-specific.
//    procedure AddActiveVarying(const aName: string);
    // Currently, NVidia-specific.

    function GetUniformBufferSize(const aName: string): Integer;

    procedure UseProgramObject;
    procedure EndUseProgramObject;

    procedure SetUniformi(const index: string; const val: Integer); overload;
    procedure SetUniformi(const index: string; const val: TVector2i); overload;
    procedure SetUniformi(const index: string; const val: TVector3i); overload;
    procedure SetUniformi(const index: string; const val: TVector4i); overload;

    procedure SetUniformf(const index: string; const val: Single); overload;
    procedure SetUniformf(const index: string; const val: TVector2f); overload;
    procedure SetUniformf(const index: string; const val: TVector3f); overload;
    procedure SetUniformf(const index: string; const val: TVector4f); overload;

    { : Shader parameters. }
    property Uniform1i[const index: string]: Integer read GetUniform1i write SetUniform1i;
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

    property UniformTextureHandle[const index: string; const TextureIndex: Integer; const TextureTarget: TDGLTextureTarget]: Cardinal read GetUniformTextureHandle write SetUniformTextureHandle;
    property UniformBuffer[const index: string]: TDGLUniformBufferHandle write SetUniformBuffer;
  end;

implementation

// ------------------
{ TDGLVirtualHandle }
{$IFDEF GLS_REGION}{$REGION 'TDGLVirtualHandle'}{$ENDIF}

function TDGLVirtualHandle.DoAllocateHandle: Cardinal;
begin
  Result := 0;
  if Assigned(FOnAllocate) then
    FOnAllocate(Self, Result);
end;

procedure TDGLVirtualHandle.DoDestroyHandle(var AHandle: TGLuint);
begin
  if not vContextActivationFailureOccurred then
  begin
    // reset error status
    ClearOpenGLError;
    // delete
    if Assigned(FOnDestroy) then
      FOnDestroy(Self, AHandle);
    // check for error
    CheckOpenGLError;
  end;
end;

class function TDGLVirtualHandle.Transferable: Boolean;
begin
  Result := False;
end;

class function TDGLVirtualHandleTransferable.Transferable: Boolean;
begin
  Result := True;
end;

{$IFDEF GLS_REGION}{$ENDREGION}{$ENDIF}

// ------------------
{ TDGLListHandle }
{$IFDEF GLS_REGION}{$REGION 'TDGLListHandle'}{$ENDIF}

// function TDGLListHandle.DoAllocateHandle: Cardinal;
// begin
// Result := glGenLists(1);
// end;

// procedure TDGLListHandle.DoDestroyHandle(var AHandle: TGLuint);
// begin
// if not vContextActivationFailureOccurred then
// with GL do
// begin
// // reset error status
// ClearError;
// // delete
// DeleteLists(AHandle, 1);
// // check for error
// CheckError;
// end;
// end;

// class function TDGLListHandle.IsValid(const ID: GLuint): Boolean;
// begin
// Result := glIsList(ID);
// end;

// procedure TDGLListHandle.NewList(mode: Cardinal);
// begin
// CurrentDGLContext.GLStates.NewList(GetHandle, mode);
// end;

// procedure TDGLListHandle.EndList;
// begin
// CurrentDGLContext.GLStates.EndList;
// end;

// procedure TDGLListHandle.CallList;
// begin
// CurrentDGLContext.GLStates.CallList(GetHandle);
// end;

{$IFDEF GLS_REGION}{$ENDREGION}{$ENDIF}

// ------------------
{ TDGLTextureHandle }
{$IFDEF GLS_REGION}{$REGION 'TDGLTextureHandle'}{$ENDIF}

function TDGLTextureHandle.DoAllocateHandle: Cardinal;
begin
  Result := 0;
  glGenTextures(1, @Result);
  FTarget := ttNoShape;
end;

procedure TDGLTextureHandle.DoDestroyHandle(var AHandle: TGLuint);
var
  a: TGLint;
  t: TDGLTextureTarget;
begin
  if not vContextActivationFailureOccurred then

      // reset error status
      glGetError;
      { : Unbind identifier from all image selectors. }
      if dglCheckExtension('ARB_multitexture') then
      begin
        with GetContext.GLStates do
        begin
          for a   := 0 to MaxTextureImageUnits - 1 do
            for t := Low(TDGLTextureTarget) to High(TDGLTextureTarget) do
              if TextureBinding[a, t] = AHandle then TextureBinding[a, t] := 0;
        end
      end
      else
        with GetContext.GLStates do
          for t := Low(TDGLTextureTarget) to High(TDGLTextureTarget) do
            if TextureBinding[0, t] = AHandle then TextureBinding[0, t] := 0;

      glDeleteTextures(1, @AHandle);
      // check for error
      CheckOpenGLError;

end;

class function TDGLTextureHandle.IsValid(const ID: GLuint): Boolean;
begin
  Result := glIsTextureEXT(ID);
end;

procedure TDGLTextureHandle.SetTarget(ATarget: TDGLTextureTarget);
begin
  if FTarget = ttNoShape then
    FTarget := ATarget;
end;

{$IFDEF GLS_REGION}{$ENDREGION}{$ENDIF}

// ------------------
{ TDGLSamplerHandle }
{$IFDEF GLS_REGION}{$REGION 'TDGLSamplerHandle'}{$ENDIF}

function TDGLSamplerHandle.DoAllocateHandle: Cardinal;
begin
  Result := 0;
  glGenSamplers(1, @Result);
end;

procedure TDGLSamplerHandle.DoDestroyHandle(var AHandle: TGLuint);
begin
  if not vContextActivationFailureOccurred then
    begin
      // reset error status
      glGetError;
      // delete
      glDeleteSamplers(1, @AHandle);
      // check for error
      CheckOpenGLError;
    end;
end;

class function TDGLSamplerHandle.IsSupported: Boolean;
begin
  Result := dglCheckExtension('ARB_sampler_objects');
end;

class function TDGLSamplerHandle.IsValid(const ID: GLuint): Boolean;
begin
  Result := glIsSampler(ID);
end;

{$IFDEF GLS_REGION}{$ENDREGION}{$ENDIF}

// ------------------
{ TDGLQueryHandle }
{$IFDEF GLS_REGION}{$REGION 'TDGLQueryHandle'}{$ENDIF}

procedure TDGLQueryHandle.BeginQuery;
begin
  if CurrentDGLContext.GLStates.CurrentQuery[QueryType] = 0 then
    CurrentDGLContext.GLStates.BeginQuery(QueryType, GetHandle);
  FActive := True;
end;

function TDGLQueryHandle.CounterBits: Integer;
begin
  glGetQueryiv(Target, GL_QUERY_COUNTER_BITS, @Result);
end;

function TDGLQueryHandle.DoAllocateHandle: Cardinal;
begin
  Result := 0;
  glGenQueries(1, @Result);
end;

procedure TDGLQueryHandle.DoDestroyHandle(var AHandle: TGLuint);
begin
  if not vContextActivationFailureOccurred then
    begin
      // reset error status
      glGetError;
      // delete
      glDeleteQueries(1, @AHandle);
      // check for error
      CheckOpenGLError;
    end;
end;

class function TDGLQueryHandle.IsValid(const ID: GLuint): Boolean;
begin
  Result := glIsQuery(ID);
end;

procedure TDGLQueryHandle.EndQuery;
begin
  Assert(FActive = True, 'Cannot end a query before it begins');
  FActive := False;
  Assert(handle <> 0);
  // glEndQuery(Target);
  CurrentDGLContext.GLStates.EndQuery(QueryType);
end;

function TDGLQueryHandle.IsResultAvailable: Boolean;
begin
  glGetQueryObjectiv(handle, GL_QUERY_RESULT_AVAILABLE, @Result);
end;

function TDGLQueryHandle.QueryResultInt: TGLint;
begin
  glGetQueryObjectiv(handle, GL_QUERY_RESULT, @Result);
end;

function TDGLQueryHandle.QueryResultInt64: TGLint64EXT;
begin
  glGetQueryObjecti64v(handle, GL_QUERY_RESULT, @Result);
end;

function TDGLQueryHandle.QueryResultUInt: TGLuint;
begin
  glGetQueryObjectuiv(handle, GL_QUERY_RESULT, @Result);
end;

function TDGLQueryHandle.QueryResultUInt64: TGLuint64EXT;
begin
  glGetQueryObjectui64v(handle, GL_QUERY_RESULT, @Result);
end;

function TDGLQueryHandle.QueryResultBool: TGLBoolean;
var
  I: TGLuint;
begin
  glGetQueryObjectuiv(handle, GL_QUERY_RESULT, @I);
  Result := I > 0;
end;

class function TDGLQueryHandle.Transferable: Boolean;
begin
  Result := False;
end;

{$IFDEF GLS_REGION}{$ENDREGION}{$ENDIF}

// ------------------
{ TDGLOcclusionQueryHandle }
{$IFDEF GLS_REGION}{$REGION 'TDGLOcclusionQueryHandle'}{$ENDIF}

function TDGLOcclusionQueryHandle.GetQueryType: TQueryType;
begin
  Result := qrySamplesPassed;
end;

function TDGLOcclusionQueryHandle.GetTarget: TGLuint;
begin
  Result := GL_SAMPLES_PASSED;
end;

class function TDGLOcclusionQueryHandle.IsSupported: Boolean;
begin
  Result := GL_VERSION_1_5;
end;

function TDGLOcclusionQueryHandle.PixelCount: Integer;
begin
  Result := QueryResultUInt;
end;

{$IFDEF GLS_REGION}{$ENDREGION}{$ENDIF}

// ------------------
{ TGLBooleanOcclusionQueryHandle }
{$IFDEF GLS_REGION}{$REGION 'TGLBooleanOcclusionQueryHandle'}{$ENDIF}

function TGLBooleanOcclusionQueryHandle.GetQueryType: TQueryType;
begin
  Result := qryAnySamplesPassed;
end;

function TGLBooleanOcclusionQueryHandle.GetTarget: TGLuint;
begin
  Result := GL_ANY_SAMPLES_PASSED;
end;

class function TGLBooleanOcclusionQueryHandle.IsSupported: Boolean;
begin
  Result := dglCheckExtension('ARB_occlusion_query2');
end;

{$IFDEF GLS_REGION}{$ENDREGION}{$ENDIF}

// ------------------
{ TDGLTimerQueryHandle }
{$IFDEF GLS_REGION}{$REGION 'TDGLTimerQueryHandle'}{$ENDIF}

function TDGLTimerQueryHandle.GetQueryType: TQueryType;
begin
  Result := qryTimeElapsed;
end;

function TDGLTimerQueryHandle.GetTarget: TGLuint;
begin
  Result := GL_TIME_ELAPSED;
end;

class function TDGLTimerQueryHandle.IsSupported: Boolean;
begin
  Result := dglCheckExtension('EXT_timer_query') or dglCheckExtension('ARB_timer_query');
end;

function TDGLTimerQueryHandle.Time: Integer;
begin
  Result := QueryResultUInt;
end;

{$IFDEF GLS_REGION}{$ENDREGION}{$ENDIF}

// ------------------
{ TDGLPrimitiveQueryHandle }
{$IFDEF GLS_REGION}{$REGION 'TDGLPrimitiveQueryHandle'}{$ENDIF}

function TDGLPrimitiveQueryHandle.GetQueryType: TQueryType;
begin
  Result := qryPrimitivesGenerated;
end;

function TDGLPrimitiveQueryHandle.GetTarget: TGLuint;
begin
  Result := GL_PRIMITIVES_GENERATED;
end;

class function TDGLPrimitiveQueryHandle.IsSupported: Boolean;
begin
  Result := GL_VERSION_3_0;
end;

function TDGLPrimitiveQueryHandle.PrimitivesGenerated: Integer;
begin
  Result := QueryResultUInt;
end;

{$IFDEF GLS_REGION}{$ENDREGION}{$ENDIF}

// ------------------
{ TDGLBufferObjectHandle }
{$IFDEF GLS_REGION}{$REGION 'TDGLBufferObjectHandle'}{$ENDIF}

constructor TDGLBufferObjectHandle.CreateFromData(p: Pointer; size: Integer; bufferUsage: TGLuint);
begin
  Create;
  AllocateHandle;
  Bind;
  BufferData(p, size, bufferUsage);
  UnBind;
end;

function TDGLBufferObjectHandle.DoAllocateHandle: Cardinal;
begin
  Result := 0;
  glGenBuffers(1, @Result);
end;

procedure TDGLBufferObjectHandle.DoDestroyHandle(var AHandle: TGLuint);
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

class function TDGLBufferObjectHandle.IsValid(const ID: GLuint): Boolean;
begin
  Result := glIsBuffer(ID);
end;

class function TDGLBufferObjectHandle.IsSupported: Boolean;
begin
  Result := dglCheckExtension('ARB_vertex_buffer_object');
end;

procedure TDGLBufferObjectHandle.BindRange(index: TGLuint; offset: TGLintptr; size: TGLsizeiptr);
begin
  Assert(False, 'BindRange only XBO and UBO');
end;

procedure TDGLBufferObjectHandle.BindBase(index: TGLuint);
begin
  Assert(False, 'BindRange only XBO and UBO');
end;

procedure TDGLBufferObjectHandle.UnBindBase(index: TGLuint);
begin
  Assert(False, 'BindRange only XBO and UBO');
end;

procedure TDGLBufferObjectHandle.BufferData(p: Pointer; size: Integer; bufferUsage: TGLuint);
begin
  FSize := size;
  glBufferData(Target, size, p, bufferUsage);
end;

procedure TDGLBufferObjectHandle.BindBufferData(p: Pointer; size: Integer; bufferUsage: TGLuint);
begin
  Bind;
  FSize := size;
  glBufferData(Target, size, p, bufferUsage);
end;

procedure TDGLBufferObjectHandle.BufferSubData(offset, size: Integer; p: Pointer);
begin
  Assert(offset + size <= FSize);
  glBufferSubData(Target, offset, size, p);
end;

function TDGLBufferObjectHandle.MapBuffer(access: TGLuint): Pointer;
begin
  Result := glMapBuffer(Target, access);
end;

function TDGLBufferObjectHandle.MapBufferRange(offset: TGLint; len: TGLsizei; access: TGLbitfield): Pointer;
begin
  Result := glMapBufferRange(Target, offset, len, access);
end;

procedure TDGLBufferObjectHandle.Flush(offset: TGLint; len: TGLsizei);
begin
  glFlushMappedBufferRange(Target, offset, len);
end;

function TDGLBufferObjectHandle.UnmapBuffer: Boolean;
begin
  Result := glUnmapBuffer(Target);
end;

{$IFDEF GLS_REGION}{$ENDREGION}{$ENDIF}

// ------------------
{ TDGLVBOHandle }
{$IFDEF GLS_REGION}{$REGION 'TDGLVBOHandle'}{$ENDIF}

function TDGLVBOHandle.GetVBOTarget: TGLuint;
begin
  Result := Target;
end;

{$IFDEF GLS_REGION}{$ENDREGION}{$ENDIF}

// ------------------
{ TDGLVBOArrayBufferHandle }
{$IFDEF GLS_REGION}{$REGION 'TDGLVBOArrayBufferHandle'}{$ENDIF}

procedure TDGLVBOArrayBufferHandle.Bind;
begin
  CurrentDGLContext.GLStates.ArrayBufferBinding := handle;
end;

procedure TDGLVBOArrayBufferHandle.UnBind;
begin
  CurrentDGLContext.GLStates.ArrayBufferBinding := 0;
end;

function TDGLVBOArrayBufferHandle.GetTarget: TGLuint;
begin
  Result := GL_ARRAY_BUFFER;
end;

{$IFDEF GLS_REGION}{$ENDREGION}{$ENDIF}

// ------------------
{ TDGLVBOElementArrayHandle }
{$IFDEF GLS_REGION}{$REGION 'TDGLVBOElementArrayHandle'}{$ENDIF}

procedure TDGLVBOElementArrayHandle.Bind;
begin
  CurrentDGLContext.GLStates.ElementBufferBinding := handle;
end;

procedure TDGLVBOElementArrayHandle.UnBind;
begin
  CurrentDGLContext.GLStates.ElementBufferBinding := 0;
end;

function TDGLVBOElementArrayHandle.GetTarget: TGLuint;
begin
  Result := GL_ELEMENT_ARRAY_BUFFER;
end;

{$IFDEF GLS_REGION}{$ENDREGION}{$ENDIF}

// ------------------
{ TDGLPackPBOHandle }
{$IFDEF GLS_REGION}{$REGION 'TDGLPackPBOHandle'}{$ENDIF}
procedure TDGLPackPBOHandle.Bind;
begin
  CurrentDGLContext.GLStates.PixelPackBufferBinding := handle;
end;

procedure TDGLPackPBOHandle.UnBind;
begin
  CurrentDGLContext.GLStates.PixelPackBufferBinding := 0;
end;

function TDGLPackPBOHandle.GetTarget: TGLuint;
begin
  Result := GL_PIXEL_PACK_BUFFER;
end;

class function TDGLPackPBOHandle.IsSupported: Boolean;
begin
  Result := dglCheckExtension('ARB_pixel_buffer_object');
end;

{$IFDEF GLS_REGION}{$ENDREGION}{$ENDIF}

// ------------------
{ TDGLUnpackPBOHandle }
{$IFDEF GLS_REGION}{$REGION 'TDGLUnpackPBOHandle'}{$ENDIF}
procedure TDGLUnpackPBOHandle.Bind;
begin
  CurrentDGLContext.GLStates.PixelUnpackBufferBinding := handle;
end;

procedure TDGLUnpackPBOHandle.UnBind;
begin
  CurrentDGLContext.GLStates.PixelUnpackBufferBinding := 0;
end;

function TDGLUnpackPBOHandle.GetTarget: TGLuint;
begin
  Result := GL_PIXEL_UNPACK_BUFFER;
end;

class function TDGLUnpackPBOHandle.IsSupported: Boolean;
begin
  Result := dglCheckExtension('ARB_pixel_buffer_object');
end;

{$IFDEF GLS_REGION}{$ENDREGION}{$ENDIF}

// ------------------
{ TDGLTransformFeedbackBufferHandle }
{$IFDEF GLS_REGION}{$REGION 'TDGLTransformFeedbackBufferHandle'}{$ENDIF}

procedure TDGLTransformFeedbackBufferHandle.Bind;
begin
  CurrentDGLContext.GLStates.TransformFeedbackBufferBinding := handle;
end;

procedure TDGLTransformFeedbackBufferHandle.UnBind;
begin
  CurrentDGLContext.GLStates.TransformFeedbackBufferBinding := 0;
end;

function TDGLTransformFeedbackBufferHandle.GetTarget: TGLuint;
begin
  Result := GL_TRANSFORM_FEEDBACK_BUFFER;
end;

procedure TDGLTransformFeedbackBufferHandle.BeginTransformFeedback(primitiveMode: TGLEnum);
begin
  glBeginTransformFeedback(primitiveMode);
end;

procedure TDGLTransformFeedbackBufferHandle.EndTransformFeedback();
begin
  glEndTransformFeedback();
end;

procedure TDGLTransformFeedbackBufferHandle.BindRange(index: TGLuint; offset: TGLintptr; size: TGLsizeiptr);
begin
  CurrentDGLContext.GLStates.SetBufferIndexedBinding(handle, bbtTransformFeedBack, index, offset, size);
end;

procedure TDGLTransformFeedbackBufferHandle.BindBase(index: TGLuint);
begin
  CurrentDGLContext.GLStates.SetBufferIndexedBinding(handle, bbtTransformFeedBack, index, BufferSize);
end;

procedure TDGLTransformFeedbackBufferHandle.UnBindBase(index: TGLuint);
begin
  CurrentDGLContext.GLStates.SetBufferIndexedBinding(0, bbtTransformFeedBack, index, 0);
end;

class function TDGLTransformFeedbackBufferHandle.IsSupported: Boolean;
begin
  Result := dglCheckExtension('EXT_transform_feedback') or GL_VERSION_3_0;
end;

{$IFDEF GLS_REGION}{$ENDREGION}{$ENDIF}

// ------------------
{ TDGLTextureBufferHandle }
{$IFDEF GLS_REGION}{$REGION 'TDGLTextureBufferHandle'}{$ENDIF}
procedure TDGLTextureBufferHandle.Bind;
begin
  CurrentDGLContext.GLStates.TextureBufferBinding := handle;
end;

procedure TDGLTextureBufferHandle.UnBind;
begin
  CurrentDGLContext.GLStates.TextureBufferBinding := 0;
end;

function TDGLTextureBufferHandle.GetTarget: TGLuint;
begin
  Result := GL_TEXTURE_BUFFER;
end;

class function TDGLTextureBufferHandle.IsSupported: Boolean;
begin
  Result := dglCheckExtension('EXT_texture_buffer_object') or dglCheckExtension('ARB_texture_buffer_object') or GL_VERSION_3_1;
end;

{$IFDEF GLS_REGION}{$ENDREGION}{$ENDIF}

// ------------------
{ TDGLUniformBufferHandle }
{$IFDEF GLS_REGION}{$REGION 'TDGLUniformBufferHandle'}{$ENDIF}
procedure TDGLUniformBufferHandle.Bind;
begin
  CurrentDGLContext.GLStates.UniformBufferBinding := handle;
end;

procedure TDGLUniformBufferHandle.UnBind;
begin
  CurrentDGLContext.GLStates.UniformBufferBinding := 0;
end;

procedure TDGLUniformBufferHandle.BindRange(index: TGLuint; offset: TGLintptr; size: TGLsizeiptr);
begin
  CurrentDGLContext.GLStates.SetBufferIndexedBinding(handle, bbtUniform, index, offset, size);
end;

procedure TDGLUniformBufferHandle.BindBase(index: TGLuint);
begin
  CurrentDGLContext.GLStates.SetBufferIndexedBinding(handle, bbtUniform, index, BufferSize);
end;

procedure TDGLUniformBufferHandle.UnBindBase(index: TGLuint);
begin
  CurrentDGLContext.GLStates.SetBufferIndexedBinding(0, bbtUniform, index, 0);
end;

function TDGLUniformBufferHandle.GetTarget: TGLuint;
begin
  Result := GL_UNIFORM_BUFFER;
end;

class function TDGLUniformBufferHandle.IsSupported: Boolean;
begin
  Result := dglCheckExtension('ARB_uniform_buffer_object');
end;

{$IFDEF GLS_REGION}{$ENDREGION}{$ENDIF}

// ------------------
{ TDGLVertexArrayHandle }
{$IFDEF GLS_REGION}{$REGION 'TDGLVertexArrayHandle'}{$ENDIF}

function TDGLVertexArrayHandle.DoAllocateHandle: Cardinal;
begin
  Result := 0;
  glGenVertexArrays(1, @Result);
end;

procedure TDGLVertexArrayHandle.DoDestroyHandle(var AHandle: TGLuint);
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

class function TDGLVertexArrayHandle.IsValid(const ID: GLuint): Boolean;
begin
  Result := glIsVertexArray(ID);
end;

procedure TDGLVertexArrayHandle.Bind;
begin
  Assert(CurrentDGLContext <> nil);
  CurrentDGLContext.GLStates.VertexArrayBinding := handle;
end;

procedure TDGLVertexArrayHandle.UnBind;
begin
  Assert(CurrentDGLContext <> nil);
  CurrentDGLContext.GLStates.VertexArrayBinding := 0;
end;

class function TDGLVertexArrayHandle.IsSupported: Boolean;
begin
  Result := dglCheckExtension('ARB_vertex_array_object');
end;

class function TDGLVertexArrayHandle.Transferable: Boolean;
begin
  Result := False;
end;

{$IFDEF GLS_REGION}{$ENDREGION}{$ENDIF}

// ------------------
{ TDGLFramebufferHandle }
{$IFDEF GLS_REGION}{$REGION 'TDGLFramebufferHandle'}{$ENDIF}

function TDGLFramebufferHandle.DoAllocateHandle: Cardinal;
begin
  Result := 0;
  glGenFramebuffers(1, @Result)
end;

procedure TDGLFramebufferHandle.DoDestroyHandle(var AHandle: TGLuint);
begin
  if not vContextActivationFailureOccurred then
    begin
      // reset error status
      glGetError;
      // delete
      glDeleteFramebuffers(1, @AHandle);
      // check for error
      CheckOpenGLError;
    end;
end;

class function TDGLFramebufferHandle.IsValid(const ID: GLuint): Boolean;
begin
  Result := glIsFramebuffer(ID);
end;

procedure TDGLFramebufferHandle.Bind;
begin
  Assert(CurrentDGLContext <> nil);
  CurrentDGLContext.GLStates.SetFrameBuffer(handle);
end;

procedure TDGLFramebufferHandle.BindForDrawing;
begin
  Assert(CurrentDGLContext <> nil);
  CurrentDGLContext.GLStates.DrawFrameBuffer := handle;
end;

procedure TDGLFramebufferHandle.BindForReading;
begin
  Assert(CurrentDGLContext <> nil);
  CurrentDGLContext.GLStates.ReadFrameBuffer := handle;
end;

procedure TDGLFramebufferHandle.UnBind;
begin
  Assert(CurrentDGLContext <> nil);
  CurrentDGLContext.GLStates.SetFrameBuffer(0);
end;

procedure TDGLFramebufferHandle.UnBindForDrawing;
begin
  Assert(CurrentDGLContext <> nil);
  CurrentDGLContext.GLStates.DrawFrameBuffer := 0;
end;

procedure TDGLFramebufferHandle.UnBindForReading;
begin
  Assert(CurrentDGLContext <> nil);
  CurrentDGLContext.GLStates.ReadFrameBuffer := 0;
end;

procedure TDGLFramebufferHandle.Attach1DTexture(Target: TGLEnum; attachment: TGLEnum; textarget: TGLEnum; texture: TGLuint; level: TGLint);
begin
  glFramebufferTexture1D(Target, attachment, textarget, texture, level);
end;

procedure TDGLFramebufferHandle.Attach2DTexture(Target: TGLEnum; attachment: TGLEnum; textarget: TGLEnum; texture: TGLuint; level: TGLint);
begin
  glFramebufferTexture2D(Target, attachment, textarget, texture, level);
end;

procedure TDGLFramebufferHandle.Attach3DTexture(Target: TGLEnum; attachment: TGLEnum; textarget: TGLEnum; texture: TGLuint; level: TGLint; layer: TGLint);
begin
  glFramebufferTexture3D(Target, attachment, textarget, texture, level, layer);
end;

procedure TDGLFramebufferHandle.AttachLayer(Target: TGLEnum; attachment: TGLEnum; texture: TGLuint; level: TGLint; layer: TGLint);
begin
  glFramebufferTextureLayer(Target, attachment, texture, level, layer);
end;

procedure TDGLFramebufferHandle.AttachRenderBuffer(Target: TGLEnum; attachment: TGLEnum; renderbuffertarget: TGLEnum; renderbuffer: TGLuint);
begin
  glFramebufferRenderbuffer(Target, attachment, renderbuffertarget, renderbuffer);
end;

procedure TDGLFramebufferHandle.AttachTexture(Target: TGLEnum; attachment: TGLEnum; texture: TGLuint; level: TGLint);
begin
  glFramebufferTexture(Target, attachment, texture, level);
end;

procedure TDGLFramebufferHandle.AttachTextureLayer(Target: TGLEnum; attachment: TGLEnum; texture: TGLuint; level: TGLint; layer: TGLint);
begin
  glFramebufferTextureLayer(Target, attachment, texture, level, layer);
end;

procedure TDGLFramebufferHandle.Blit(srcX0: TGLint; srcY0: TGLint; srcX1: TGLint; srcY1: TGLint; dstX0: TGLint; dstY0: TGLint; dstX1: TGLint; dstY1: TGLint; mask: TGLbitfield; filter: TGLEnum);
begin
  glBlitFramebuffer(srcX0, srcY0, srcX1, srcY1, dstX0, dstY0, dstX1, dstY1, mask, filter);
end;

function TDGLFramebufferHandle.GetAttachmentParameter(Target: TGLEnum; attachment: TGLEnum; pname: TGLEnum): TGLint;
begin
  glGetFramebufferAttachmentParameteriv(Target, attachment, pname, @Result)
end;

function TDGLFramebufferHandle.GetAttachmentObjectType(Target: TGLEnum; attachment: TGLEnum): TGLint;
begin
  glGetFramebufferAttachmentParameteriv(Target, attachment, GL_FRAMEBUFFER_ATTACHMENT_OBJECT_TYPE, @Result);
end;

function TDGLFramebufferHandle.GetAttachmentObjectName(Target: TGLEnum; attachment: TGLEnum): TGLint;
begin
  glGetFramebufferAttachmentParameteriv(Target, attachment, GL_FRAMEBUFFER_ATTACHMENT_OBJECT_NAME, @Result);
end;

function TDGLFramebufferHandle.GetStatus: TDGLFramebufferStatus;
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
    GL_FRAMEBUFFER_INCOMPLETE_DUPLICATE_ATTACHMENT_EXT:
      Result := fsIncompleteDuplicateAttachment;
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

function TDGLFramebufferHandle.GetStringStatus(out clarification: string): TDGLFramebufferStatus;
const
  cFBOStatus: array [TDGLFramebufferStatus] of string = ('Complete', 'Incomplete attachment', 'Incomplete missing attachment', 'Incomplete duplicate attachment', 'Incomplete dimensions', 'Incomplete formats', 'Incomplete draw buffer',
    'Incomplete read buffer', 'Unsupported', 'Incomplite multisample', 'Status Error');
begin
  Result        := GetStatus;
  clarification := cFBOStatus[Result];
end;

class function TDGLFramebufferHandle.IsSupported: Boolean;
begin
  Result := dglCheckExtension('EXT_framebuffer_object') or dglCheckExtension('ARB_framebuffer_object');
end;

class function TDGLFramebufferHandle.Transferable: Boolean;
begin
  Result := False;
end;

{$IFDEF GLS_REGION}{$ENDREGION}{$ENDIF}

// ------------------
{ TDGLRenderbufferObject }
{$IFDEF GLS_REGION}{$REGION 'TDGLRenderbufferObject'}{$ENDIF}

function TDGLRenderbufferHandle.DoAllocateHandle: Cardinal;
begin
  Result := 0;
  glGenRenderbuffers(1, @Result);
end;

procedure TDGLRenderbufferHandle.DoDestroyHandle(var AHandle: TGLuint);
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

class function TDGLRenderbufferHandle.IsValid(const ID: GLuint): Boolean;
begin
  Result := glIsRenderbuffer(ID);
end;

procedure TDGLRenderbufferHandle.Bind;
begin
  CurrentDGLContext.GLStates.renderbuffer := GetHandle;
end;

procedure TDGLRenderbufferHandle.UnBind;
begin
  if CurrentDGLContext <> nil then
    CurrentDGLContext.GLStates.renderbuffer := 0;
end;

procedure TDGLRenderbufferHandle.SetStorage(internalformat: TGLEnum; width, height: TGLsizei);
begin
  glRenderbufferStorage(GL_RENDERBUFFER, internalformat, width, height);
end;

procedure TDGLRenderbufferHandle.SetStorageMultisample(internalformat: TGLEnum; samples: TGLsizei; width, height: TGLsizei);
begin
  glRenderbufferStorageMultisample(GL_RENDERBUFFER, samples, internalformat, width, height);
end;

class function TDGLRenderbufferHandle.IsSupported: Boolean;
begin
  Result := dglCheckExtension('EXT_framebuffer_object') or dglCheckExtension('ARB_framebuffer_object');
end;

{$IFDEF GLS_REGION}{$ENDREGION}{$ENDIF}

// ------------------
{ TDGLARBProgramHandle }
{$IFDEF GLS_REGION}{$REGION 'TDGLARBProgramHandle'}{$ENDIF}

function TDGLARBProgramHandle.DoAllocateHandle: Cardinal;
begin
  glGenProgramsARB(1, @Result);
//  Result :=  glCreateProgram();
  FReady := False;
end;

procedure TDGLARBProgramHandle.DoDestroyHandle(var AHandle: TGLuint);
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

class function TDGLARBProgramHandle.IsValid(const ID: GLuint): Boolean;
begin
  Result := glIsProgram(ID);
end;

procedure TDGLARBProgramHandle.LoadARBProgram(AText: string);
const
  cProgType: array [0 .. 2] of string = ('ARB vertex', 'ARB fragment', 'NV geometry');
var
  errPos, p: Integer;
begin
  Bind;
  glProgramStringARB(GetTarget, GL_PROGRAM_FORMAT_ASCII_ARB, Length(AText), PGLChar(TGLString(AText)));
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
    DGLSLogger.LogError(Format('%s Program Error - [Pos: %d][Error %s]', [cProgType[p], errPos, FInfoLog]));
    FReady := False;
  end
  else
  begin
    FReady   := True;
    FInfoLog := '';
  end;
end;

procedure TDGLARBProgramHandle.Enable;
begin
  if FReady then
    glEnable(GetTarget)
  else
    Abort;
end;

procedure TDGLARBProgramHandle.Disable;
begin
  glDisable(GetTarget);
end;

procedure TDGLARBProgramHandle.Bind;
begin
//  glBindProgram(GetTarget, handle);
end;

class function TDGLARBVertexProgramHandle.GetTarget: TGLEnum;
begin
  Result := GL_VERTEX_PROGRAM_ARB;
end;

class function TDGLARBVertexProgramHandle.IsSupported: Boolean;
begin
  Result := dglCheckExtension('ARB_vertex_program');
end;

class function TDGLARBFragmentProgramHandle.GetTarget: TGLEnum;
begin
  Result := GL_FRAGMENT_PROGRAM_ARB;
end;

class function TDGLARBFragmentProgramHandle.IsSupported: Boolean;
begin
  Result := dglCheckExtension('ARB_vertex_program');
end;

class function TDGLARBGeometryProgramHandle.GetTarget: TGLEnum;
begin
  Result := GL_GEOMETRY_PROGRAM_NV;
end;

class function TDGLARBGeometryProgramHandle.IsSupported: Boolean;
begin
  Result := dglCheckExtension('NV_geometry_program4');
end;

{$IFDEF GLS_REGION}{$ENDREGION}{$ENDIF}

// ------------------
{ TDGLSLHandle }
{$IFDEF GLS_REGION}{$REGION 'TDGLSLHandle'}{$ENDIF}

procedure TDGLSLHandle.DoDestroyHandle(var AHandle: TGLuint);
begin
  if not vContextActivationFailureOccurred then
    begin
      // reset error status
      ClearOpenGLError;
      // delete
      glDeleteObjectARB(AHandle);
      // check for error
      CheckOpenGLError;
    end;
end;

function TDGLSLHandle.InfoLog: string;
var
  maxLength: Integer;
  log:       TGLString;
begin
  maxLength := 0;
  glGetObjectParameterivARB(GetHandle, GL_OBJECT_INFO_LOG_LENGTH_ARB, @maxLength);
  SetLength(log, maxLength);
  if maxLength > 0 then
  begin
    glGetInfoLogARB(GetHandle, maxLength, maxLength, @log[1]);
    SetLength(log, maxLength);
  end;
  Result := string(log);
end;

class function TDGLSLHandle.IsSupported: Boolean;
begin
  Result := dglCheckExtension('ARB_shader_objects');
end;

{$IFDEF GLS_REGION}{$ENDREGION}{$ENDIF}

// ------------------
{ TDGLShaderHandle }
{$IFDEF GLS_REGION}{$REGION 'TDGLShaderHandle'}{$ENDIF}

function TDGLShaderHandle.DoAllocateHandle: Cardinal;
begin
  Result := glCreateShader(FShaderType)
end;

class function TDGLShaderHandle.IsValid(const ID: GLuint): Boolean;
begin
  Result := glIsShader(ID);
end;

procedure TDGLShaderHandle.ShaderSource(const source: AnsiString);
var
  p: PGLChar;
begin
  p := PGLChar(TGLString(source));
  glShaderSource(GetHandle, 1, @p, nil);
end;

function TDGLShaderHandle.CompileShader: Boolean;
var
  compiled: Integer;
  glH:      TGLuint;
begin
  glH := GetHandle;
  glCompileShader(glH);
  compiled := 0;
  glGetShaderiv(glH, GL_COMPILE_STATUS, @compiled);
  Result := (compiled <> 0);
end;

{$IFDEF GLS_REGION}{$ENDREGION}{$ENDIF}

// ------------------
{ TDGLVertexShaderHandle }
{$IFDEF GLS_REGION}{$REGION 'TDGLVertexShaderHandle'}{$ENDIF}

constructor TDGLVertexShaderHandle.Create;
begin
  FShaderType := GL_VERTEX_SHADER_ARB;
  inherited;
end;

class function TDGLVertexShaderHandle.IsSupported: Boolean;
begin
  Result := dglCheckExtension('ARB_vertex_shader');
end;

{$IFDEF GLS_REGION}{$ENDREGION}{$ENDIF}

// ------------------
{ TDGLGeometryShaderHandle }
{$IFDEF GLS_REGION}{$REGION 'TDGLGeometryShaderHandle'}{$ENDIF}

constructor TDGLGeometryShaderHandle.Create;
begin
  FShaderType := GL_GEOMETRY_SHADER_EXT;
  inherited;
end;

class function TDGLGeometryShaderHandle.IsSupported: Boolean;
begin
  Result := dglCheckExtension('EXT_geometry_shader4');
end;

{$IFDEF GLS_REGION}{$ENDREGION}{$ENDIF}

// ------------------
{ TDGLFragmentShaderHandle }
{$IFDEF GLS_REGION}{$REGION 'TDGLFragmentShaderHandle'}{$ENDIF}

constructor TDGLFragmentShaderHandle.Create;
begin
  FShaderType := GL_FRAGMENT_SHADER_ARB;
  inherited;
end;

class function TDGLFragmentShaderHandle.IsSupported: Boolean;
begin
  Result := dglCheckExtension('ARB_fragment_shader');
end;

{$IFDEF GLS_REGION}{$ENDREGION}{$ENDIF}

// ------------------
{ TDGLTessControlShaderHandle }
{$IFDEF GLS_REGION}{$REGION 'TDGLTessControlShaderHandle'}{$ENDIF}

constructor TDGLTessControlShaderHandle.Create;
begin
  FShaderType := GL_TESS_CONTROL_SHADER;
  inherited;
end;

class function TDGLTessControlShaderHandle.IsSupported: Boolean;
begin
  Result := dglCheckExtension('ARB_tessellation_shader');
end;

{$IFDEF GLS_REGION}{$ENDREGION}{$ENDIF}

// ------------------
{ TDGLTessEvaluationShaderHandle }
{$IFDEF GLS_REGION}{$REGION 'TDGLTessEvaluationShaderHandle'}{$ENDIF}

constructor TDGLTessEvaluationShaderHandle.Create;
begin
  FShaderType := GL_TESS_EVALUATION_SHADER;
  inherited;
end;

class function TDGLTessEvaluationShaderHandle.IsSupported: Boolean;
begin
  Result := dglCheckExtension('ARB_tessellation_shader');
end;

{$IFDEF GLS_REGION}{$ENDREGION}{$ENDIF}

// ------------------
{ TDGLProgramHandle }
{$IFDEF GLS_REGION}{$REGION 'TDGLProgramHandle'}{$ENDIF}

function TDGLProgramHandle.DoAllocateHandle: Cardinal;
begin
  Result := glCreateProgram();
end;

class function TDGLProgramHandle.IsValid(const ID: GLuint): Boolean;
begin
  Result := glIsProgram(ID);
end;

procedure TDGLProgramHandle.AddShader(ShaderType: TDGLShaderHandleClass; const ShaderSource: string; treatWarningsAsErrors: Boolean = False);
var
  shader: TDGLShaderHandle;
begin
  shader := ShaderType.CreateAndAllocate;
  try
    if shader.handle = 0 then
      raise EGLShader.Create('Couldn''t allocate ' + ShaderType.ClassName);
    shader.ShaderSource(AnsiString(ShaderSource));
    if (not shader.CompileShader) or (treatWarningsAsErrors and (Pos('warning', LowerCase(shader.InfoLog)) > 0)) then
      raise EGLShader.Create(FName + ' (' + shader.ClassName + '): '#13#10 + shader.InfoLog);
    AttachObject(shader);
  finally
    shader.Free;
  end;
  CheckOpenGLError;
end;

procedure TDGLProgramHandle.AttachObject(shader: TDGLShaderHandle);
begin
  glAttachShader(GetHandle, shader.handle);
end;

procedure TDGLProgramHandle.DetachAllObject;
var
  glH:    TGLuint;
  I:      Integer;
  count:  GLSizei;
  buffer: array [0 .. 255] of TGLuint;
begin
  glH := GetHandle;
  if glH > 0 then
  begin
    glGetAttachedShaders(glH, Length(buffer), count, @buffer[0]);
    count := MinInteger(count, Length(buffer));
    for I := 0 to count - 1 do
      glDetachShader(glH, buffer[I]);
    NotifyChangesOfData;
  end;
end;

procedure TDGLProgramHandle.BindAttribLocation(index: Integer; const aName: string);
begin
  glBindAttribLocation(GetHandle, index, PGLChar(TGLString(aName)));
end;

procedure TDGLProgramHandle.BindFragDataLocation(index: Integer; const aName: string);
begin
  glBindFragDataLocation(GetHandle, index, PGLChar(TGLString(name)));
end;

function TDGLProgramHandle.LinkProgram: Boolean;
var
  Status: Integer;
  glH:    TGLuint;
begin
  glH := GetHandle;
  glLinkProgram(glH);
  Status := 0;
  glGetProgramiv(glH, GL_LINK_STATUS, @Status);
  Result := (Status <> 0);
end;

function TDGLProgramHandle.ValidateProgram: Boolean;
var
  validated: Integer;
  h:         TGLuint;
begin
  h := GetHandle;
  glValidateProgram(h);
  validated := 0;
  glGetProgramiv(h, GL_VALIDATE_STATUS, @validated);
  Result := (validated <> 0);
end;

function TDGLProgramHandle.GetAttribLocation(const aName: string): Integer;
begin
  Result := glGetAttribLocation(GetHandle, PGLChar(TGLString(aName)));
  Assert(Result >= 0, Format(glsUnknownParam, ['attrib', aName, Name]));
end;


// procedure TDGLProgramHandle.SetAttribLocation(const Location: Integer; const Buffer: Pointer);
// begin
// // http://www.openglorg/sdk/docs/man/xhtml/glBindBuffer.xml
// glBindBuffer(GL_ARRAY_BUFFER, Buffer);
// // http://www.openglorg/sdk/docs/man/xhtml/glVertexAttribPointer.xml
// glVertexAttribPointer(Location, 2, GL_FLOAT, false, 0, 0);
// // http://www.openglorg/sdk/docs/man/xhtml/glEnableVertexAttribArray.xml
// glEnableVertexAttribArray(Location);
// end;
// GetUniformLocation
//

function TDGLProgramHandle.GetUniformLocation(const aName: string): Integer;
begin
  Result := glGetUniformLocation(GetHandle, PGLChar(TGLString(aName)));
//  Assert(Result >= 0, Format(glsUnknownParam, ['uniform', aName, Name]));
end;

//function TDGLProgramHandle.GetVaryingLocation(const aName: string): Integer;
//begin
//  Result := glGetVaryingLocation(GetHandle, PGLChar(TGLString(aName)));
//  Assert(Result >= 0, Format(glsUnknownParam, ['varying', aName, Name]));
//end;

//procedure TDGLProgramHandle.AddActiveVarying(const aName: string);
//begin
//  glActiveVarying(GetHandle, PGLChar(TGLString(aName)));
//end;

procedure TDGLProgramHandle.UseProgramObject;
begin
  Assert(CurrentDGLContext <> nil);
  CurrentDGLContext.GLStates.CurrentProgram := handle;
end;

procedure TDGLProgramHandle.EndUseProgramObject;
begin
  Assert(CurrentDGLContext <> nil);
  CurrentDGLContext.GLStates.CurrentProgram := 0;
end;

function TDGLProgramHandle.GetUniform1i(const index: string): Integer;
begin
  glGetUniformiv(GetHandle, GetUniformLocation(index), @Result);
end;

function TDGLProgramHandle.GetUniform2i(const index: string): TVector2i;
begin
  glGetUniformiv(GetHandle, GetUniformLocation(index), @Result);
end;

function TDGLProgramHandle.GetUniform3i(const index: string): TVector3i;
begin
  glGetUniformiv(GetHandle, GetUniformLocation(index), @Result);
end;

function TDGLProgramHandle.GetUniform4i(const index: string): TVector4i;
begin
  glGetUniformiv(GetHandle, GetUniformLocation(index), @Result);
end;

procedure TDGLProgramHandle.SetUniform1f(const index: string; val: Single);
begin
  glUniform1f(GetUniformLocation(index), val);
end;

function TDGLProgramHandle.GetUniform1f(const index: string): Single;
begin
  glGetUniformfv(GetHandle, GetUniformLocation(index), @Result);
end;

procedure TDGLProgramHandle.SetUniform1i(const index: string; val: Integer);
begin
  glUniform1i(GetUniformLocation(index), val);
end;

procedure TDGLProgramHandle.SetUniform2i(const index: string; const Value: TVector2i);
begin
  glUniform2i(GetUniformLocation(index), Value.V[0], Value.V[1]);
end;

procedure TDGLProgramHandle.SetUniform3i(const index: string; const Value: TVector3i);
begin
  glUniform3i(GetUniformLocation(index), Value.V[0], Value.V[1], Value.V[2]);
end;

procedure TDGLProgramHandle.SetUniform4i(const index: string; const Value: TVector4i);
begin
  glUniform4i(GetUniformLocation(index), Value.V[0], Value.V[1], Value.V[2], Value.V[3]);
end;

function TDGLProgramHandle.GetUniform2f(const index: string): TVector2f;
begin
  glGetUniformfv(GetHandle, GetUniformLocation(index), @Result);
end;

procedure TDGLProgramHandle.SetUniform2f(const index: string; const val: TVector2f);
begin
  glUniform2f(GetUniformLocation(index), val.V[0], val.V[1]);
end;

function TDGLProgramHandle.GetUniform3f(const index: string): TAffineVector;
begin
  glGetUniformfv(GetHandle, GetUniformLocation(index), @Result);
end;

procedure TDGLProgramHandle.SetUniform3f(const index: string; const val: TAffineVector);
begin
  glUniform3f(GetUniformLocation(index), val.V[0], val.V[1], val.V[2]);
end;

function TDGLProgramHandle.GetUniform4f(const index: string): TVector;
begin
  glGetUniformfv(GetHandle, GetUniformLocation(index), @Result);
end;

procedure TDGLProgramHandle.SetUniform4f(const index: string; const val: TVector);
begin
  glUniform4f(GetUniformLocation(index), val.V[0], val.V[1], val.V[2], val.V[3]);
end;

function TDGLProgramHandle.GetUniformMatrix2fv(const index: string): TMatrix2f;
begin
  glGetUniformfv(GetHandle, GetUniformLocation(index), @Result);
end;

procedure TDGLProgramHandle.SetUniformMatrix2fv(const index: string; const val: TMatrix2f);
begin
  glUniformMatrix2fv(GetUniformLocation(index), 1, False, @val);
end;

function TDGLProgramHandle.GetUniformMatrix3fv(const index: string): TMatrix3f;
begin
  glGetUniformfv(GetHandle, GetUniformLocation(index), @Result);
end;

procedure TDGLProgramHandle.SetUniformMatrix3fv(const index: string; const val: TMatrix3f);
begin
  glUniformMatrix3fv(GetUniformLocation(index), 1, False, @val);
end;

function TDGLProgramHandle.GetUniformMatrix4fv(const index: string): TMatrix;
begin
  glGetUniformfv(GetHandle, GetUniformLocation(index), @Result);

end;

procedure TDGLProgramHandle.SetUniformMatrix4fv(const index: string; const val: TMatrix);
begin
  glUniformMatrix4fv(GetUniformLocation(index), 1, GL_FALSE, @val);

end;

procedure TDGLProgramHandle.SetUniformf(const index: string; const val: Single);
begin
  SetUniform1f(index, val);
end;

procedure TDGLProgramHandle.SetUniformf(const index: string; const val: TVector2f);
begin
  SetUniform2f(index, val);
end;

procedure TDGLProgramHandle.SetUniformf(const index: string; const val: TVector3f);
begin
  SetUniform3f(index, val);
end;

procedure TDGLProgramHandle.SetUniformf(const index: string; const val: TVector4f);
begin
  SetUniform4f(index, val);
end;

procedure TDGLProgramHandle.SetUniformi(const index: string; const val: Integer);
begin
  SetUniform1f(index, val);
end;

procedure TDGLProgramHandle.SetUniformi(const index: string; const val: TVector2i);
begin
  SetUniform2i(index, val);
end;

procedure TDGLProgramHandle.SetUniformi(const index: string; const val: TVector3i);
begin
  SetUniform3i(index, val);
end;

procedure TDGLProgramHandle.SetUniformi(const index: string; const val: TVector4i);
begin
  SetUniform4i(index, val);
end;

function TDGLProgramHandle.GetUniformTextureHandle(const index: string; const TextureIndex: Integer; const TextureTarget: TDGLTextureTarget): Cardinal;
begin
  Result := GetUniform1i(index);
end;

procedure TDGLProgramHandle.SetUniformTextureHandle(const index: string; const TextureIndex: Integer; const TextureTarget: TDGLTextureTarget; const Value: Cardinal);
begin
  CurrentDGLContext.GLStates.TextureBinding[0, TextureTarget] := Value;
  SetUniform1i(index, TextureIndex);
end;

procedure TDGLProgramHandle.SetUniformBuffer(const index: string; Value: TDGLUniformBufferHandle);
begin
  glUniformBufferEXT(handle, GetUniformLocation(index), Value.handle);
end;

function TDGLProgramHandle.GetUniformBufferSize(const aName: string): Integer;
begin
  Result := glGetUniformBufferSizeEXT(handle, GetUniformLocation(aName));
end;

function TDGLProgramHandle.GetUniformOffset(const aName: string): GLintptr; //PGLInt;
begin
  Result := glGetUniformOffsetEXT(handle, GetUniformLocation(aName));
end;

function TDGLProgramHandle.GetUniformBlockIndex(const aName: string): Integer;
begin
  Result := glGetUniformBlockIndex(handle, PGLChar(TGLString(aName)));
  Assert(Result >= 0, Format(glsUnknownParam, ['uniform block', aName, Name]));
end;

constructor TDGLProgramHandle.Create;
begin
  inherited Create;
  FName := 'DefaultShaderName';
end;
{$IFDEF GLS_REGION}{$ENDREGION}{$ENDIF}

end.
