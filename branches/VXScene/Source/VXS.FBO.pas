//
// VXScene Component Library, based on GLScene http://glscene.sourceforge.net 
//
{
   Implements FBO support 
   Original author of the unit is Riz.
   Modified by C4 and YarUnderoaker (hope, I didn't miss anybody).
 
}

unit VXS.FBO;

interface

{$I VXScene.inc}

uses
  Winapi.OpenGL,
  Winapi.OpenGLext,
  System.SysUtils,
  FMX.Dialogs,
  
  VXS.OpenGL,
  VXS.Scene,
  VXS.Context,
  VXS.State,
  VXS.Texture,
  VXS.Color,
  VXS.RenderContextInfo,
  VXS.MultisampleImage,
  VXS.Graphics,
  VXS.TextureFormat,
  VXS.VectorTypes;


const
  MaxColorAttachments = 32;

type
  TVXRenderbuffer = class
  private
    FRenderbufferHandle: TVXRenderbufferHandle;
    FWidth: Integer;
    FHeight: Integer;
    FStorageValid: Boolean;
    function GetHandle: GLuint;
    procedure SetHeight(const Value: Integer);
    procedure SetWidth(const Value: Integer);
  protected

    function GetInternalFormat: cardinal; virtual; abstract;

    procedure InvalidateStorage;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Bind;
    procedure Unbind;
    { Handle to the OpenGL render buffer object. 
      If the handle hasn't already been allocated, it will be allocated
      by this call (ie. do not use if no OpenGL context is active!) }
    property Handle: GLuint read GetHandle;
    property Width: Integer read FWidth write SetWidth;
    property Height: Integer read FHeight write SetHeight;
  end;

  TVXDepthRBO = class(TVXRenderbuffer)
  private
    FDepthPrecision: TVXDepthPrecision;
    procedure SetDepthPrecision(const Value: TVXDepthPrecision);
  protected
    function GetInternalFormat: cardinal; override;
  public
    constructor Create;

    property DepthPrecision: TVXDepthPrecision read FDepthPrecision write
      SetDepthPrecision;
  end;

  TVXStencilPrecision = (spDefault, sp1bit, sp4bits, sp8bits, sp16bits);

  TVXStencilRBO = class(TVXRenderbuffer)
  private
    FStencilPrecision: TVXStencilPrecision;
    procedure SetStencilPrecision(const Value: TVXStencilPrecision);
  protected
    function GetInternalFormat: cardinal; override;
  public
    constructor Create;

    property StencilPrecision: TVXStencilPrecision read FStencilPrecision write
      SetStencilPrecision;
  end;

  TVXFrameBuffer = class
  private
    FFrameBufferHandle: TVXFramebufferHandle;
    FTarget: GLEnum;
    FWidth: Integer;
    FHeight: Integer;
    FLayer: Integer;
    FLevel: Integer;
    FTextureMipmap: cardinal;
    FAttachedTexture: array[0..MaxColorAttachments - 1] of TVXTexture;
    FDepthTexture: TVXTexture;
    FDRBO: TVXDepthRBO;
    FSRBO: TVXStencilRBO;

    function GetStatus: TVXFramebufferStatus;
    procedure SetHeight(const Value: Integer);
    procedure SetWidth(const Value: Integer);
    procedure SetLayer(const Value: Integer);
    procedure SetLevel(const Value: Integer);
  protected
    procedure AttachTexture(
      const attachment: GLEnum;
      const textarget: GLEnum;
      const texture: GLuint;
      const level: GLint;
      const layer: GLint); overload;
    procedure ReattachTextures;
  public
    constructor Create;
    destructor Destroy; override;

    // attaches a depth rbo to the fbo
    // the depth buffer must have the same dimentions as the fbo
    procedure AttachDepthBuffer(DepthBuffer: TVXDepthRBO); overload;
    // detaches depth attachment from the fbo
    procedure DetachDepthBuffer;

    // attaches a stencil rbo to the fbo
    // the stencil buffer must have the same dimentions as the fbo
    procedure AttachStencilBuffer(StencilBuffer: TVXStencilRBO); overload;
    // detaches stencil attachment from the fbo
    procedure DetachStencilBuffer;

    // attaches a depth texture to the fbo
    // the depth texture must have the same dimentions as the fbo
    procedure AttachDepthTexture(Texture: TVXTexture); overload;
    procedure DetachDepthTexture;

    procedure AttachTexture(n: Cardinal; Texture: TVXTexture); overload;
    procedure DetachTexture(n: Cardinal);

    function GetStringStatus(out clarification: string): TVXFramebufferStatus;
    property Status: TVXFramebufferStatus read GetStatus;
    procedure Bind;
    procedure Unbind;

    procedure PreRender;
    procedure Render(var rci: TVXRenderContextInfo; baseObject:
      TVXBaseSceneObject);
    procedure PostRender(const PostGenerateMipmap: Boolean);

    property Handle: TVXFramebufferHandle read FFrameBufferHandle;
    property Width: Integer read FWidth write SetWidth;
    property Height: Integer read FHeight write SetHeight;
    property Layer: Integer read FLayer write SetLayer;
    property Level: Integer read FLevel write SetLevel;
  end;

implementation

{ TVXRenderbuffer }

constructor TVXRenderbuffer.Create;
begin
  inherited Create;
  FRenderbufferHandle := TVXRenderbufferHandle.Create;
  FWidth := 256;
  FHeight := 256;
end;

destructor TVXRenderbuffer.Destroy;
begin
  FRenderbufferHandle.DestroyHandle;
  FRenderbufferHandle.Free;
  inherited Destroy;
end;

function TVXRenderbuffer.GetHandle: GLuint;
begin
  if FRenderbufferHandle.Handle = 0 then
    FRenderbufferHandle.AllocateHandle;
  Result := FRenderbufferHandle.Handle;
end;

procedure TVXRenderbuffer.InvalidateStorage;
begin
  FStorageValid := False;
end;

procedure TVXRenderbuffer.SetHeight(const Value: Integer);
begin
  if FHeight <> Value then
  begin
    FHeight := Value;
    InvalidateStorage;
  end;
end;

procedure TVXRenderbuffer.SetWidth(const Value: Integer);
begin
  if FWidth <> Value then
  begin
    FWidth := Value;
    InvalidateStorage;
  end;
end;

procedure TVXRenderbuffer.Bind;
var
  internalFormat: cardinal;
begin
  FRenderbufferHandle.AllocateHandle;
  FRenderbufferHandle.Bind;
  if not FStorageValid then
  begin
    internalFormat := GetInternalFormat;
    FRenderbufferHandle.SetStorage(internalFormat, FWidth, FHeight);
  end;
end;

procedure TVXRenderbuffer.Unbind;
begin
  FRenderbufferHandle.UnBind;
end;

{ TVXDepthRBO }

constructor TVXDepthRBO.Create;
begin
  inherited Create;
  FDepthPrecision := dpDefault;
end;

function TVXDepthRBO.GetInternalFormat: cardinal;
begin
  case DepthPrecision of
    dp24bits: Result := GL_DEPTH_COMPONENT24;
    dp16bits: Result := GL_DEPTH_COMPONENT16;
    dp32bits: Result := GL_DEPTH_COMPONENT32;
  else
    // dpDefault
    Result := GL_DEPTH_COMPONENT24_ARB;
  end;
end;

procedure TVXDepthRBO.SetDepthPrecision(const Value: TVXDepthPrecision);
begin
  if FDepthPrecision <> Value then
  begin
    FDepthPrecision := Value;
    InvalidateStorage;
  end;
end;

{ TVXStencilRBO }

constructor TVXStencilRBO.Create;
begin
  inherited Create;
  FStencilPrecision := spDefault;
end;

function TVXStencilRBO.GetInternalFormat: cardinal;
begin
  case StencilPrecision of
    spDefault: Result := GL_STENCIL_INDEX;
    sp1bit: Result := GL_STENCIL_INDEX1_EXT;
    sp4bits: Result := GL_STENCIL_INDEX4_EXT;
    sp8bits: Result := GL_STENCIL_INDEX8_EXT;
    sp16bits: Result := GL_STENCIL_INDEX16_EXT;
  else
    // spDefault
    Result := GL_STENCIL_INDEX;
  end;
end;

procedure TVXStencilRBO.SetStencilPrecision(const Value: TVXStencilPrecision);
begin
  if FStencilPrecision <> Value then
  begin
    FStencilPrecision := Value;
    InvalidateStorage;
  end;
end;

{ TVXFrameBuffer }

constructor TVXFrameBuffer.Create;
begin
  inherited;
  FFrameBufferHandle := TVXFrameBufferHandle.Create;
  FWidth := 256;
  FHeight := 256;
  FLayer := 0;
  FLevel := 0;
  FTextureMipmap := 0;
  FTarget := GL_FRAMEBUFFER;
end;

destructor TVXFrameBuffer.Destroy;
begin
  FFrameBufferHandle.DestroyHandle;
  FFrameBufferHandle.Free;
  inherited Destroy;
end;

procedure TVXFrameBuffer.AttachTexture(n: Cardinal; Texture: TVXTexture);
var
  textarget: TVXTextureTarget;
begin
  Assert(n < MaxColorAttachments);
  Texture.Handle;
  FAttachedTexture[n] := Texture;
  textarget := Texture.Image.NativeTextureTarget;
  // Store mipmaping requires
  if not ((Texture.MinFilter in [miNearest, miLinear])
    or (textarget = ttTextureRect)) then
    FTextureMipmap := FTextureMipmap or (1 shl n);

  if Texture.Image is TVXMultiSampleImage then
    FTextureMipmap := 0;

  AttachTexture(
    GL_COLOR_ATTACHMENT0_EXT + n,
    DecodeTextureTarget(textarget),
    Texture.Handle,
    FLevel, FLayer);
end;

procedure TVXFrameBuffer.AttachDepthBuffer(DepthBuffer: TVXDepthRBO);

  procedure AttachDepthRB;
  begin
    // forces initialization
    DepthBuffer.Bind;
    DepthBuffer.Unbind;
    glFramebufferRenderbuffer(FTarget, GL_DEPTH_ATTACHMENT_EXT,
      GL_RENDERBUFFER_EXT, DepthBuffer.Handle);
  end;

var
  dp: TVXDepthPrecision;
begin
  if Assigned(FDRBO) then
    DetachDepthBuffer;
  FDRBO := DepthBuffer;

  Bind;
  AttachDepthRB;

  // if default format didn't work, try something else
  // crude, but might work
  if (Status = fsUnsupported) and (DepthBuffer.DepthPrecision = dpDefault) then
  begin
    // try the other formats
    // best quality first
    for dp := high(dp) downto low(dp) do
    begin
      if dp = dpDefault then
        Continue;

      DepthBuffer.DepthPrecision := dp;

      AttachDepthRB;

      if not (Status = fsUnsupported) then
        Break;
    end;
  end;
  Status;
  Unbind;
end;

procedure TVXFrameBuffer.AttachDepthTexture(Texture: TVXTexture);
begin
  FDepthTexture := Texture;

  if FDepthTexture.Image is TVXMultisampleImage then
  begin
    if not IsDepthFormat(FDepthTexture.TextureFormatEx) then
    begin
      // Force texture properties to depth compatibility
      FDepthTexture.TextureFormatEx := tfDEPTH_COMPONENT24;
      TVXMultisampleImage(FDepthTexture.Image).Width := Width;
      TVXMultisampleImage(FDepthTexture.Image).Height := Height;
    end;
    FTextureMipmap := 0;
  end
  else
  begin
    if not IsDepthFormat(FDepthTexture.TextureFormatEx) then
    begin
      // Force texture properties to depth compatibility
      FDepthTexture.ImageClassName := TVXBlankImage.ClassName;
      FDepthTexture.TextureFormatEx := tfDEPTH_COMPONENT24;
      TVXBlankImage(FDepthTexture.Image).Width := Width;
      TVXBlankImage(FDepthTexture.Image).Height := Height;
    end;
    if FDepthTexture.TextureFormatEx = tfDEPTH24_STENCIL8 then
    begin
      TVXBlankImage(FDepthTexture.Image).GetBitmap32.SetColorFormatDataType(GL_DEPTH_STENCIL, GL_UNSIGNED_INT_24_8);
      TVXBlankImage(FDepthTexture.Image).ColorFormat := GL_DEPTH_STENCIL;
    end
    else
    begin
      TVXBlankImage(FDepthTexture.Image).GetBitmap32.SetColorFormatDataType(GL_DEPTH_COMPONENT, GL_UNSIGNED_BYTE);
      TVXBlankImage(FDepthTexture.Image).ColorFormat := GL_DEPTH_COMPONENT;
    end;
    // Depth texture mipmaping
    if not ((FDepthTexture.MinFilter in [miNearest, miLinear])) then
      FTextureMipmap := FTextureMipmap or Cardinal(1 shl MaxColorAttachments);
  end;

  AttachTexture(
    GL_DEPTH_ATTACHMENT,
    DecodeTextureTarget(FDepthTexture.Image.NativeTextureTarget),
    FDepthTexture.Handle,
    FLevel,
    FLayer);

  if FDepthTexture.TextureFormatEx = tfDEPTH24_STENCIL8 then
    AttachTexture(
      GL_STENCIL_ATTACHMENT,
      DecodeTextureTarget(FDepthTexture.Image.NativeTextureTarget),
      FDepthTexture.Handle,
      FLevel,
      FLayer);
end;

procedure TVXFrameBuffer.DetachDepthTexture;
begin
  if Assigned(FDepthTexture) then
  begin
    FTextureMipmap := FTextureMipmap and (not (1 shl MaxColorAttachments));
    AttachTexture(
      GL_DEPTH_ATTACHMENT,
      DecodeTextureTarget(FDepthTexture.Image.NativeTextureTarget),
      0, 0, 0);
    FDepthTexture := nil;
  end;
end;

procedure TVXFrameBuffer.AttachStencilBuffer(StencilBuffer: TVXStencilRBO);

  procedure AttachStencilRB;
  begin
    // forces initialization
    StencilBuffer.Bind;
    StencilBuffer.Unbind;
    glFramebufferRenderbuffer(FTarget, GL_STENCIL_ATTACHMENT,
      GL_RENDERBUFFER_EXT, StencilBuffer.Handle);
  end;

var
  sp: TVXStencilPrecision;
begin
  if Assigned(FSRBO) then
    DetachStencilBuffer;
  FSRBO := StencilBuffer;

  Bind;
  AttachStencilRB;

  // if default format didn't work, try something else
  // crude, but might work
  if (Status = fsUnsupported)
    and (StencilBuffer.StencilPrecision = spDefault) then
  begin
    // try the other formats
    // best quality first
    for sp := high(sp) downto low(sp) do
    begin
      if sp = spDefault then
        Continue;

      StencilBuffer.StencilPrecision := sp;

      AttachStencilRB;

      if not (Status = fsUnsupported) then
        Break;
    end;
  end;
  Status;
  Unbind;
end;

procedure TVXFrameBuffer.AttachTexture(
  const attachment: GLEnum;
  const textarget: GLEnum;
  const texture: GLuint;
  const level: GLint;
  const layer: GLint);
var
  storeDFB: GLuint;
  RC: TVXContext;
begin
  RC := SafeCurrentVXContext;
  storeDFB := RC.VXStates.DrawFrameBuffer;
  if storeDFB <> FFrameBufferHandle.Handle then
    Bind;

  with FFrameBufferHandle do
    case textarget of
      GL_TEXTURE_1D:
        Attach1DTexture(FTarget, attachment, textarget, texture, level);

      GL_TEXTURE_2D:
        Attach2DTexture(FTarget, attachment, textarget, texture, level);

      GL_TEXTURE_RECTANGLE: // Rectangle texture can't be leveled
        Attach2DTexture(FTarget, attachment, textarget, texture, 0);

      GL_TEXTURE_3D:
        Attach3DTexture(FTarget, attachment, textarget, texture, level, layer);

      GL_TEXTURE_CUBE_MAP:
        Attach2DTexture(FTarget, attachment, GL_TEXTURE_CUBE_MAP_POSITIVE_X + layer, texture, level);

      GL_TEXTURE_CUBE_MAP_POSITIVE_X,
        GL_TEXTURE_CUBE_MAP_NEGATIVE_X,
        GL_TEXTURE_CUBE_MAP_POSITIVE_Y,
        GL_TEXTURE_CUBE_MAP_NEGATIVE_Y,
        GL_TEXTURE_CUBE_MAP_POSITIVE_Z,
        GL_TEXTURE_CUBE_MAP_NEGATIVE_Z:
          Attach2DTexture(FTarget, attachment, textarget, texture, level);

      GL_TEXTURE_CUBE_MAP_ARRAY,
        GL_TEXTURE_1D_ARRAY,
        GL_TEXTURE_2D_ARRAY:
        AttachLayer(FTarget, attachment, texture, level, layer);

      GL_TEXTURE_2D_MULTISAMPLE: // Multisample texture can't be leveled
        Attach2DTexture(FTarget, attachment, textarget, texture, 0);

      GL_TEXTURE_2D_MULTISAMPLE_ARRAY:
        AttachLayer(FTarget, attachment, texture, 0, layer);
    end;

  if storeDFB <> FFrameBufferHandle.Handle then
    RC.VXStates.SetFrameBuffer(storeDFB);
end;

procedure TVXFrameBuffer.Bind;
begin
  if Handle.IsDataNeedUpdate then
    ReattachTextures
  else
    Handle.Bind;
end;

procedure TVXFrameBuffer.Unbind;
begin
  FFrameBufferHandle.UnBind;
end;

procedure TVXFrameBuffer.DetachTexture(n: Cardinal);
begin
  // textarget ignored when binding 0
  if Assigned(FAttachedTexture[n]) then
  begin
    Bind;
    AttachTexture(
      GL_COLOR_ATTACHMENT0 + n,
      GL_TEXTURE_2D, // target does not matter
      0, 0, 0);

    FTextureMipmap := FTextureMipmap and (not (1 shl n));
    FAttachedTexture[n] := nil;
    Unbind;
  end;
end;

procedure TVXFrameBuffer.DetachDepthBuffer;
begin
  Bind;
  glFramebufferRenderbuffer(FTarget, GL_DEPTH_ATTACHMENT,
    GL_RENDERBUFFER, 0);
  Unbind;
  FDRBO := nil;
end;

procedure TVXFrameBuffer.DetachStencilBuffer;
begin
  Bind;
  glFramebufferRenderbuffer(FTarget, GL_STENCIL_ATTACHMENT,
    GL_RENDERBUFFER, 0);
  Unbind;
  FSRBO := nil;
end;

function TVXFrameBuffer.GetStatus: TVXFramebufferStatus;
var
  status: cardinal;
begin
  status := glCheckFramebufferStatus(FTarget);

  case status of
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

function TVXFrameBuffer.GetStringStatus(out clarification: string):
  TVXFramebufferStatus;
const
  cFBOStatus: array[TVXFramebufferStatus] of string = (
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

procedure TVXFrameBuffer.PostRender(const PostGenerateMipmap: Boolean);
var
  n: Integer;
  textarget: TVXTextureTarget;
begin
  if (FTextureMipmap > 0) and PostGenerateMipmap then
  begin
    for n := 0 to MaxColorAttachments - 1 do
      if Assigned(FAttachedTexture[n]) then
      begin
        if FTextureMipmap and (1 shl n) = 0 then
          Continue;
        textarget := FAttachedTexture[n].Image.NativeTextureTarget;
        with FFrameBufferHandle.RenderingContext.VxStates do
          TextureBinding[ActiveTexture, textarget] :=
            FAttachedTexture[n].Handle;
        glGenerateMipmap(DecodeTextureTarget(textarget));
      end;
  end;
end;

procedure TVXFrameBuffer.PreRender;
begin

end;

procedure TVXFrameBuffer.Render(var rci: TVXRenderContextInfo; baseObject:
  TVXBaseSceneObject);
var
  backColor: TColorVector;
  buffer: TVXSceneBuffer;
begin
  Bind;
  Assert(Status = fsComplete, 'Framebuffer not complete');

  buffer := TVXSceneBuffer(rci.buffer);

  backColor := ConvertWinColor(buffer.BackgroundColor);
  glClearColor(backColor.X, backColor.Y, backColor.Z,
    buffer.BackgroundAlpha);
  rci.VXStates.SetColorMask(cAllColorComponents);
  rci.VXStates.DepthWriteMask := True;
  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);

  baseObject.Render(rci);
  Unbind;
end;

procedure TVXFrameBuffer.SetHeight(const Value: Integer);
begin
  if FHeight <> Value then
  begin
    FHeight := Value;
  end;
end;

procedure TVXFrameBuffer.SetWidth(const Value: Integer);
begin
  if FWidth <> Value then
  begin
    FWidth := Value;
  end;
end;

procedure TVXFrameBuffer.ReattachTextures;
var
  n: Integer;
  bEmpty: Boolean;
  s: String;
begin
  Handle.AllocateHandle;
  Handle.Bind;
  // Reattach layered textures
  bEmpty := True;

  for n := 0 to MaxColorAttachments - 1 do
    if Assigned(FAttachedTexture[n]) then
    begin
      AttachTexture(
        GL_COLOR_ATTACHMENT0_EXT + n,
        DecodeTextureTarget(FAttachedTexture[n].Image.NativeTextureTarget),
        FAttachedTexture[n].Handle,
        FLevel,
        FLayer);
      bEmpty := False;
    end;

  if Assigned(FDepthTexture) then
  begin
    AttachTexture(
      GL_DEPTH_ATTACHMENT,
      DecodeTextureTarget(FDepthTexture.Image.NativeTextureTarget),
      FDepthTexture.Handle,
      FLevel,
      FLayer);
    bEmpty := False;
  end;

  if Assigned(FDRBO) then
  begin
    FDRBO.Bind;
    FDRBO.Unbind;
    glFramebufferRenderbuffer(FTarget, GL_DEPTH_ATTACHMENT_EXT,
      GL_RENDERBUFFER_EXT, FDRBO.Handle);
    bEmpty := False;
  end;

  if Assigned(FSRBO) then
  begin
    FSRBO.Bind;
    FSRBO.Unbind;
    glFramebufferRenderbuffer(FTarget, GL_STENCIL_ATTACHMENT,
      GL_RENDERBUFFER_EXT, FSRBO.Handle);
    bEmpty := False;
  end;

  if not bEmpty and (GetStringStatus(s) <> fsComplete) then
    ShowMessage(Format('Framebuffer error: %s. Deactivated', [s]));

  Handle.NotifyDataUpdated;
end;

procedure TVXFrameBuffer.SetLayer(const Value: Integer);
var
  RC: TVXContext;
begin
  if FLayer <> Value then
  begin
    FLayer := Value;
    RC := CurrentVXContext;
    if Assigned(RC) then
    begin
      if RC.VXStates.DrawFrameBuffer = FFrameBufferHandle.Handle then
        ReattachTextures;
    end;
  end;
end;

procedure TVXFrameBuffer.SetLevel(const Value: Integer);
var
  RC: TVXContext;
begin
  if FLevel <> Value then
  begin
    FLevel := Value;
    RC := CurrentVXContext;
    if Assigned(RC) then
    begin
      if RC.VXStates.DrawFrameBuffer = FFrameBufferHandle.Handle then
        ReattachTextures;
    end;
  end;
end;

end.
