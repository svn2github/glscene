//
// VXScene Component Library, based on GLScene http://glscene.sourceforge.net
//
{
  Implements FBO support for GLScene.
  Original author of the unit is Riz.
  Modified by C4 and YarUnderoaker (hope, I didn't miss anybody).

}
unit VXS.FBORenderer;

interface

{$I VXScene.inc}

uses
  Winapi.OpenGL,
  Winapi.OpenGLext,
  System.Classes,
  System.SysUtils,
  FMX.Dialogs,
  
  VXS.OpenGLAdapter,
  VXS.VectorGeometry,
  VXS.Scene,
  VXS.Texture,
  VXS.Context,
  VXS.FBO,
  VXS.Color,
  VXS.Material,
  VXS.RenderContextInfo,
  VXS.State,
  VXS.TextureFormat,
  VXS.VectorTypes,
  VXS.MultisampleImage;

type
  TVXEnabledRenderBuffer = (erbDepth, erbStencil);
  TVXEnabledRenderBuffers = set of TVXEnabledRenderBuffer;

  TVXFBOTargetVisibility = (tvDefault, tvFBOOnly);

  TVXFBOClearOption = (coColorBufferClear, coDepthBufferClear,
    coStencilBufferClear, coUseBufferBackground);
  TVXFBOClearOptions = set of TVXFBOClearOption;

  TVXTextureArray = array of TVXTexture;

  TSetTextureTargetsEvent = procedure(Sender: TObject;
    var colorTexs: TVXTextureArray) of object;

  TVXFBORenderer = class(TVXBaseSceneObject, IGLMaterialLibrarySupported)
  private
    FFbo: TVXFrameBuffer;
    FDepthRBO: TVXDepthRBO;
    FStencilRBO: TVXStencilRBO;
    FColorAttachment: Integer;
    FRendering: Boolean;

    FHasColor: Boolean;
    FHasDepth: Boolean;
    FHasStencil: Boolean;

    FMaterialLibrary: TVXMaterialLibrary;
    FColorTextureName: TVXLibMaterialName;
    FDepthTextureName: TVXLibMaterialName;
    FWidth: Integer;
    FHeight: Integer;
    FForceTextureDimensions: Boolean;
    FStencilPrecision: TVXStencilPrecision;
    FRootObject: TVXBaseSceneObject;
    FRootVisible: Boolean;
    FCamera: TVXCamera;
    FEnabledRenderBuffers: TVXEnabledRenderBuffers;
    FTargetVisibility: TVXFBOTargetVisibility;
    FBeforeRender: TDirectRenderEvent;
    FPostInitialize: TNotifyEvent;
    FAfterRender: TDirectRenderEvent;
    FPreInitialize: TNotifyEvent;
    FBackgroundColor: TVXColor;
    FClearOptions: TVXFBOClearOptions;
    FAspect: Single;
    FSceneScaleFactor: Single;
    FUseLibraryAsMultiTarget: Boolean;
    FPostGenerateMipmap: Boolean;
    FMaxSize: Integer;
    FMaxAttachment: Integer;
    FStoreCamera: array [0 .. 2] of TVector;
    FOnSetTextureTargets: TSetTextureTargetsEvent;
    // implementing IGLMaterialLibrarySupported
    function GetMaterialLibrary: TVXAbstractMaterialLibrary;
    procedure SetMaterialLibrary(const Value: TVXAbstractMaterialLibrary);
    procedure SetDepthTextureName(const Value: TVXLibMaterialName);
    procedure SetColorTextureName(const Value: TVXLibMaterialName);
    procedure SetForceTextureDimentions(const Value: Boolean);
    procedure SetHeight(Value: Integer);
    procedure SetWidth(Value: Integer);
    procedure SetLayer(const Value: Integer);
    function GetLayer: Integer;
    procedure SetLevel(const Value: Integer);
    function GetLevel: Integer;
    procedure SetStencilPrecision(const Value: TVXStencilPrecision);
    procedure SetRootObject(const Value: TVXBaseSceneObject);
    function GetViewport: TRectangle;
    procedure SetCamera(const Value: TVXCamera);
    procedure SetEnabledRenderBuffers(const Value: TVXEnabledRenderBuffers);
    procedure SetTargetVisibility(const Value: TVXFBOTargetVisibility);
    procedure SetBackgroundColor(const Value: TVXColor);
    function StoreSceneScaleFactor: Boolean;
    function StoreAspect: Boolean;
    procedure SetUseLibraryAsMultiTarget(Value: Boolean);
    procedure SetPostGenerateMipmap(const Value: Boolean);
  protected
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
    procedure Initialize;

    procedure ForceDimensions(Texture: TVXTexture);

    procedure RenderToFBO(var ARci: TVXRenderContextInfo);

    procedure ApplyCamera(var ARci: TVXRenderContextInfo);
    procedure UnApplyCamera(var ARci: TVXRenderContextInfo);

    procedure DoBeforeRender(var ARci: TVXRenderContextInfo);
    procedure DoAfterRender(var ARci: TVXRenderContextInfo);
    procedure DoPreInitialize;
    procedure DoPostInitialize;

    property HasColor: Boolean read FHasColor;
    property HasDepth: Boolean read FHasDepth;
    property HasStencil: Boolean read FHasStencil;

    property Viewport: TRectangle read GetViewport;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure DoRender(var ARci: TVXRenderContextInfo; ARenderSelf: Boolean;
      ARenderChildren: Boolean); override;

    { Layer (also cube map face) is activated only on
      the volume textures, texture array and cube map.
      You can select the layer during the drawing to. }
    property Layer: Integer read GetLayer write SetLayer;
    { Mipmap Level where will be rendering }
    property Level: Integer read GetLevel write SetLevel;

  published
    property Active: Boolean read GetVisible write SetVisible default True;
    property PickableTarget: Boolean read GetPickable write SetPickable
      default False;
    { force texture dimensions when initializing
      only works with TVXBlankImage and GLfloatDataImage, otherwise does nothing }
    property ForceTextureDimensions: Boolean read FForceTextureDimensions
      write SetForceTextureDimentions default True;

    property Width: Integer read FWidth write SetWidth default 256;
    property Height: Integer read FHeight write SetHeight default 256;

    property Aspect: Single read FAspect write FAspect stored StoreAspect;

    property ColorTextureName: TVXLibMaterialName read FColorTextureName
      write SetColorTextureName;

    property DepthTextureName: TVXLibMaterialName read FDepthTextureName
      write SetDepthTextureName;
    property MaterialLibrary: TVXAbstractMaterialLibrary read GetMaterialLibrary
      write SetMaterialLibrary;

    property BackgroundColor: TVXColor read FBackgroundColor
      write SetBackgroundColor;
    property ClearOptions: TVXFBOClearOptions read FClearOptions
      write FClearOptions;

    { camera used for rendering to the FBO
      if not assigned, use the active view's camera }
    property Camera: TVXCamera read FCamera write SetCamera;

    { adjust the scene scale of the camera so that the rendering
      becomes independent of the width of the fbo renderer
      0 = disabled }
    property SceneScaleFactor: Single read FSceneScaleFactor
      write FSceneScaleFactor stored StoreSceneScaleFactor;

    { root object used when rendering to the FBO
      if not assigned, uses itself as root and renders the child objects to the FBO }
    property RootObject: TVXBaseSceneObject read FRootObject
      write SetRootObject;

    { determines if target is rendered to FBO only or rendered normally
      in FBO only mode, if RootObject is assigned, the RootObject's Visible flag is modified
      in default mode, if RootObject is not assigned, children are rendered normally after being
      rendered to the FBO }
    property TargetVisibility: TVXFBOTargetVisibility read FTargetVisibility
      write SetTargetVisibility default tvDefault;

    { Enables the use of a render buffer if a texture is not assigned }
    property EnabledRenderBuffers: TVXEnabledRenderBuffers
      read FEnabledRenderBuffers write SetEnabledRenderBuffers;

    { use stencil buffer }
    property StencilPrecision: TVXStencilPrecision read FStencilPrecision
      write SetStencilPrecision default spDefault;

    { called before rendering to the FBO }
    property BeforeRender: TDirectRenderEvent read FBeforeRender
      write FBeforeRender;
    { called after the rendering to the FBO }
    property AfterRender: TDirectRenderEvent read FAfterRender
      write FAfterRender;
    { Called before the FBO is initialized
      the FBO is bound before calling this event }
    property PreInitialize: TNotifyEvent read FPreInitialize
      write FPreInitialize;
    { Called after the FBO is initialized, but before any rendering
      the FBO is bound before calling this event }
    property PostInitialize: TNotifyEvent read FPostInitialize
      write FPostInitialize;

    property UseLibraryAsMultiTarget: Boolean read FUseLibraryAsMultiTarget
      write SetUseLibraryAsMultiTarget default False;

    { Control mipmap generation after rendering
      texture must have MinFilter with mipmaping }
    property PostGenerateMipmap: Boolean read FPostGenerateMipmap
      write SetPostGenerateMipmap default True;

    { Allows multiTargeting to different texture sources instead of all coming
      from one single MatLib with UseLibraryAsMultiTarget. OnSetTextureTargets
      overrides the other method of setting target textures via the MaterialLibrary,
      ColorTextureName and DepthTextureName propertes }
    property OnSetTextureTargets: TSetTextureTargetsEvent
      read FOnSetTextureTargets write FOnSetTextureTargets;
  end;

implementation

{ TVXFBORenderer }

procedure TVXFBORenderer.ApplyCamera(var ARci: TVXRenderContextInfo);
var
  sc: Single;
begin
  with ARci.PipelineTransformation do
  begin
    Push;
    if Assigned(Camera) then
    begin
      FStoreCamera[0] := ARci.cameraPosition;
      FStoreCamera[1] := ARci.cameraDirection;
      FStoreCamera[2] := ARci.cameraUp;
      IdentityAll;
      sc := FCamera.SceneScale;
      if FSceneScaleFactor > 0 then
        FCamera.SceneScale := Width / FSceneScaleFactor;
      FCamera.ApplyPerspective(Viewport, Width, Height, 96);
      // 96 is default dpi
      FCamera.SceneScale := sc;

      ViewMatrix := CreateScaleMatrix(Vector3fMake(1.0 / FAspect, 1.0, 1.0));
      FCamera.Apply;
    end
    else
    begin
      ViewMatrix := MatrixMultiply(ViewMatrix,
        CreateScaleMatrix(Vector3fMake(1.0 / FAspect, 1.0, 1.0)));
    end;
  end;
end;

procedure TVXFBORenderer.UnApplyCamera(var ARci: TVXRenderContextInfo);
begin
  ARci.cameraPosition := FStoreCamera[0];
  ARci.cameraDirection := FStoreCamera[1];
  ARci.cameraUp := FStoreCamera[2];
  ARci.PipelineTransformation.Pop;
end;

constructor TVXFBORenderer.Create(AOwner: TComponent);
begin
  inherited;
  ObjectStyle := [osDirectDraw, osNoVisibilityCulling];
  FFbo := TVXFrameBuffer.Create;
  FBackgroundColor := TVXColor.Create(Self);
  FUseLibraryAsMultiTarget := False;
  FForceTextureDimensions := True;
  FWidth := 256;
  FHeight := 256;
  FEnabledRenderBuffers := [erbDepth];
  FClearOptions := [coColorBufferClear, coDepthBufferClear,
    coStencilBufferClear, coUseBufferBackground];
  PickableTarget := False;
  FAspect := 1.0;
  FSceneScaleFactor := 0.0;
  FPostGenerateMipmap := True;
  StructureChanged;
end;

destructor TVXFBORenderer.Destroy;
begin
  FFbo.Free;
  FDepthRBO.Free;
  FStencilRBO.Free;
  FBackgroundColor.Free;
  inherited;
end;

procedure TVXFBORenderer.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if (AComponent = FRootObject) and (Operation = opRemove) then
    FRootObject := nil;
end;

procedure TVXFBORenderer.DoAfterRender(var ARci: TVXRenderContextInfo);
begin
  if Assigned(FAfterRender) then
    FAfterRender(Self, ARci);
end;

procedure TVXFBORenderer.DoBeforeRender(var ARci: TVXRenderContextInfo);
begin
  if Assigned(FBeforeRender) then
    FBeforeRender(Self, ARci);
end;

procedure TVXFBORenderer.DoPostInitialize;
begin
  if Assigned(FPostInitialize) then
    FPostInitialize(Self);
end;

procedure TVXFBORenderer.DoPreInitialize;
begin
  if Assigned(FPreInitialize) then
    FPreInitialize(Self);
end;

procedure TVXFBORenderer.DoRender(var ARci: TVXRenderContextInfo;
  ARenderSelf, ARenderChildren: Boolean);
begin
  if not(csDesigning in ComponentState) then
    RenderToFBO(ARci);

  if (not Assigned(FRootObject)) and (TargetVisibility = tvDefault) and ARenderChildren
  then
  begin
    RenderChildren(0, Count - 1, ARci);
  end;
end;

procedure TVXFBORenderer.ForceDimensions(Texture: TVXTexture);
var
  bi: TVXBlankImage;
  mi: TVXMultisampleImage;
begin
  if Texture.Image is TVXBlankImage then
  begin
    bi := TVXBlankImage(Texture.Image);
    bi.Width := Width;
    bi.Height := Height;
  end
  else if Texture.Image is TVXMultisampleImage then
  begin
    mi := TVXMultisampleImage(Texture.Image);
    mi.Width := Width;
    mi.Height := Height;
  end;
end;

function TVXFBORenderer.GetViewport: TRectangle;
begin
  Result.Left := 0;
  Result.Top := 0;
  Result.Width := Width;
  Result.Height := Height;
end;

procedure TVXFBORenderer.Initialize;

  procedure AddOneMultiTarget(colorTex: TVXTexture);
  begin
    if ForceTextureDimensions then
      ForceDimensions(colorTex);
    if FColorAttachment >= FMaxAttachment then
    begin
      ShowMessage('Number of color attachments out of GL_MAX_COLOR_ATTACHMENTS');
      Visible := False;
      Abort;
    end;
    FFbo.AttachTexture(FColorAttachment, colorTex);
    Inc(FColorAttachment);
  end;

const
  cDrawBuffers: array [0 .. 15] of GLenum = (GL_COLOR_ATTACHMENT0,
    GL_COLOR_ATTACHMENT1, GL_COLOR_ATTACHMENT2, GL_COLOR_ATTACHMENT3,
    GL_COLOR_ATTACHMENT4, GL_COLOR_ATTACHMENT5, GL_COLOR_ATTACHMENT6,
    GL_COLOR_ATTACHMENT7, GL_COLOR_ATTACHMENT8, GL_COLOR_ATTACHMENT9,
    GL_COLOR_ATTACHMENT10, GL_COLOR_ATTACHMENT11, GL_COLOR_ATTACHMENT12,
    GL_COLOR_ATTACHMENT13, GL_COLOR_ATTACHMENT14, GL_COLOR_ATTACHMENT15);
var
  colorTex: TVXTexture;
  depthTex: TVXTexture;
  I: Integer;
  MulTexture: TVXTextureArray;
begin
  for I := 0 to MaxColorAttachments - 1 do
    FFbo.DetachTexture(I);

  if FMaxSize = 0 then
    glGetIntegerv(GL_MAX_RENDERBUFFER_SIZE, @FMaxSize);
  if Width > FMaxSize then
  begin
    FWidth := FMaxSize;
    ShowMessage(Format('%s.Width out of GL_MAX_RENDERBUFFER_SIZE', [Name]));
  end;
  if Height > FMaxSize then
  begin
    FHeight := FMaxSize;
    ShowMessage(Format('%s.Height out of GL_MAX_RENDERBUFFER_SIZE', [Name]));
  end;

  FFbo.Width := Width;
  FFbo.Height := Height;

  FFbo.Bind;
  DoPreInitialize;
  FFbo.Unbind;

  if Assigned(FMaterialLibrary) then
  begin
    colorTex := FMaterialLibrary.TextureByName(ColorTextureName);
    depthTex := FMaterialLibrary.TextureByName(DepthTextureName);
  end
  else
  begin
    colorTex := nil;
    depthTex := nil;
  end;

  FHasColor := False;
  FHasDepth := False;
  FHasStencil := False;
  FColorAttachment := 0;

  if FUseLibraryAsMultiTarget or Assigned(FOnSetTextureTargets) then
  begin
    if not(GL_ARB_draw_buffers or GL_ATI_draw_buffers) then
    begin
      ShowMessage('Hardware do not support MRT');
      Active := False;
      exit;
    end;
    if FMaxAttachment = 0 then
      glGetIntegerv(GL_MAX_COLOR_ATTACHMENTS, @FMaxAttachment);

    if Assigned(FOnSetTextureTargets) then
    begin
      FOnSetTextureTargets(Self, MulTexture);
      for I := 0 to High(MulTexture) do
      begin
        colorTex := MulTexture[I];
        // Skip depth texture
        if colorTex = depthTex then
          Continue;
        AddOneMultiTarget(colorTex);
      end;
    end
    else
      // Multicolor attachments
      for I := 0 to FMaterialLibrary.Materials.Count - 1 do
      begin
        colorTex := FMaterialLibrary.Materials[I].Material.Texture;
        // Skip depth texture
        if colorTex = depthTex then
          Continue;
        AddOneMultiTarget(colorTex);
      end;
    FHasColor := FColorAttachment > 0;
  end
  else
  begin
    // One color attachment
    if Assigned(colorTex) then
    begin
      if ForceTextureDimensions then
        ForceDimensions(colorTex);
      FFbo.AttachTexture(0, colorTex);
      Inc(FColorAttachment);
      FHasColor := True;
    end;
  end;

  if Assigned(depthTex) then
  begin
    if ForceTextureDimensions then
      ForceDimensions(depthTex);
    FFbo.AttachDepthTexture(depthTex);
    FDepthRBO.Free;
    FDepthRBO := nil;
    FHasDepth := True;
    FHasStencil := depthTex.TextureFormatEx = tfDEPTH24_STENCIL8;
  end
  else if erbDepth in EnabledRenderBuffers then
  begin
    if not Assigned(FDepthRBO) then
      FDepthRBO := TVXDepthRBO.Create;

    FDepthRBO.Width := Width;
    FDepthRBO.Height := Height;

    FFbo.AttachDepthBuffer(FDepthRBO);
    FHasDepth := True;
  end
  else
  begin
    FFbo.DetachDepthBuffer;
    if Assigned(FDepthRBO) then
    begin
      FDepthRBO.Free;
      FDepthRBO := nil;
    end;
  end;

  if erbStencil in EnabledRenderBuffers then
  begin
    if not Assigned(FStencilRBO) then
      FStencilRBO := TVXStencilRBO.Create;

    FStencilRBO.StencilPrecision := FStencilPrecision;
    FStencilRBO.Width := Width;
    FStencilRBO.Height := Height;

    FFbo.AttachStencilBuffer(FStencilRBO);
    FHasStencil := True;
  end
  else
  begin
    if not FHasStencil then
      FFbo.DetachStencilBuffer;
    if Assigned(FStencilRBO) then
    begin
      FStencilRBO.Free;
      FStencilRBO := nil;
    end;
  end;
  FFbo.Bind;

  if FColorAttachment = 0 then
  begin
    glDrawBuffer(GL_NONE);
    glReadBuffer(GL_NONE);
  end
  else
    glDrawBuffers(FColorAttachment, @cDrawBuffers);

  DoPostInitialize;
  FFbo.Unbind;

  CheckOpenGLError;
  ClearStructureChanged;
end;

procedure TVXFBORenderer.RenderToFBO(var ARci: TVXRenderContextInfo);

  function GetClearBits: cardinal;
  begin
    Result := 0;
    if HasColor and (coColorBufferClear in FClearOptions) then
      Result := Result or GL_COLOR_BUFFER_BIT;
    if HasDepth and (coDepthBufferClear in FClearOptions) then
      Result := Result or GL_DEPTH_BUFFER_BIT;
    if HasStencil and (coStencilBufferClear in FClearOptions) then
      Result := Result or GL_STENCIL_BUFFER_BIT;
  end;

type
  TVXStoredStates = record
    ColorClearValue: TColorVector;
    ColorWriteMask: TColorMask;
    Tests: TVXStates;
  end;

  function StoreStates: TVXStoredStates;
  begin
    Result.ColorClearValue := ARci.VKStates.ColorClearValue;
    Result.ColorWriteMask := ARci.VKStates.ColorWriteMask[0];
    Result.Tests := [stDepthTest, stStencilTest] * ARci.VKStates.States;
  end;

  procedure RestoreStates(const aStates: TVXStoredStates);
  begin
    ARci.VKStates.ColorClearValue := aStates.ColorClearValue;
    ARci.VKStates.SetColorMask(aStates.ColorWriteMask);
    if stDepthTest in aStates.Tests then
      ARci.VKStates.Enable(stDepthTest)
    else
      ARci.VKStates.Disable(stDepthTest);

    if stStencilTest in aStates.Tests then
      ARci.VKStates.Enable(stStencilTest)
    else
      ARci.VKStates.Disable(stStencilTest);
  end;

var
  backColor: TColorVector;
  buffer: TVXSceneBuffer;
  savedStates: TVXStoredStates;
  w, h: Integer;
  s: string;
begin
  if (ARci.drawState = dsPicking) and not PickableTarget then
    exit;

  if TVXFramebufferHandle.IsSupported = True then
  begin
    ShowMessage('Framebuffer not supported - deactivated');
    Active := False;
    exit;
  end;

  // prevent recursion
  if FRendering then
    exit;

  FRendering := True;
  if (ocStructure in Changes) or Assigned(FOnSetTextureTargets) then
  begin
    Initialize;
    if not Active then
      exit;
  end;

  ApplyCamera(ARci);

  try
    savedStates := StoreStates;

    FFbo.Bind;
    if FFbo.GetStringStatus(s) <> fsComplete then
    begin
      ShowMessage(Format('Framebuffer error: %s. Deactivated', [s]));
      Active := False;
      exit;
    end;

    DoBeforeRender(ARci);
    if Assigned(Camera) then
      Camera.Scene.SetupLights(ARci.VKStates.MaxLights);

    w := Width;
    h := Height;
    if FFbo.Level > 0 then
    begin
      w := w shr FFbo.Level;
      h := h shr FFbo.Level;
      if w = 0 then
        w := 1;
      if h = 0 then
        h := 1;
    end;
    ARci.VKStates.Viewport := Vector4iMake(0, 0, w, h);
    buffer := ARci.buffer as TVXSceneBuffer;

    if HasColor then
      ARci.VKStates.SetColorMask(cAllColorComponents)
    else
      ARci.VKStates.SetColorMask([]);

    ARci.VKStates.DepthWriteMask := GLboolean(HasDepth);

    if HasStencil then
      ARci.VKStates.Enable(stStencilTest)
    else
      ARci.VKStates.Disable(stStencilTest);

    if coUseBufferBackground in FClearOptions then
    begin
      backColor := ConvertWinColor(buffer.BackgroundColor);
      backColor.W := buffer.BackgroundAlpha;
      ARci.VKStates.ColorClearValue := backColor;
    end
    else
    begin
      ARci.VKStates.ColorClearValue := FBackgroundColor.Color;
    end;

    glClear(GetClearBits);

    FFbo.PreRender;
    // render to fbo
    if Assigned(RootObject) then
    begin
      // if object should only be rendered to the fbo
      // ensure it's visible before rendering to fbo
      if TargetVisibility = tvFBOOnly then
        RootObject.Visible := True;
      RootObject.Render(ARci);
      // then make it invisible afterwards
      if TargetVisibility = tvFBOOnly then
        RootObject.Visible := False;
    end
    else if (Count > 0) then
      RenderChildren(0, Count - 1, ARci);
    FFbo.PostRender(FPostGenerateMipmap);

    RestoreStates(savedStates);
    ARci.VKStates.Viewport := Vector4iMake(0, 0, ARci.viewPortSize.cx,
      ARci.viewPortSize.cy);
  finally
    FFbo.Unbind;
    FRendering := False;
    DoAfterRender(ARci);
    UnApplyCamera(ARci);
    if Assigned(Camera) then
      Camera.Scene.SetupLights(ARci.VKStates.MaxLights);
  end;
end;

procedure TVXFBORenderer.SetBackgroundColor(const Value: TVXColor);
begin
  FBackgroundColor.Assign(Value);
end;

procedure TVXFBORenderer.SetCamera(const Value: TVXCamera);
begin
  if FCamera <> Value then
  begin
    FCamera := Value;
    StructureChanged;
  end;
end;

procedure TVXFBORenderer.SetColorTextureName(const Value: TVXLibMaterialName);
begin
  if FColorTextureName <> Value then
  begin
    FColorTextureName := Value;
    StructureChanged;
  end;
end;

procedure TVXFBORenderer.SetDepthTextureName(const Value: TVXLibMaterialName);
begin
  if FDepthTextureName <> Value then
  begin
    FDepthTextureName := Value;
    StructureChanged;
  end;
end;

procedure TVXFBORenderer.SetEnabledRenderBuffers(const Value
  : TVXEnabledRenderBuffers);
begin
  if FEnabledRenderBuffers <> Value then
  begin
    FEnabledRenderBuffers := Value;
    StructureChanged;
  end;
end;

procedure TVXFBORenderer.SetForceTextureDimentions(const Value: Boolean);
begin
  if FForceTextureDimensions <> Value then
  begin
    FForceTextureDimensions := Value;
    StructureChanged;
  end;
end;

// GetMaterialLibrary
//

function TVXFBORenderer.GetMaterialLibrary: TVXAbstractMaterialLibrary;
begin
  Result := FMaterialLibrary;
end;

procedure TVXFBORenderer.SetMaterialLibrary(const Value
  : TVXAbstractMaterialLibrary);
begin
  if FMaterialLibrary <> Value then
  begin
    if Value is TVXMaterialLibrary then
    begin
      FMaterialLibrary := TVXMaterialLibrary(Value);
      StructureChanged;
    end;
  end;
end;

// SetUseLibraryAsMultiTarget
//

procedure TVXFBORenderer.SetUseLibraryAsMultiTarget(Value: Boolean);
begin
  if FUseLibraryAsMultiTarget <> Value then
  begin
    FUseLibraryAsMultiTarget := Value;
    StructureChanged;
  end;
end;

procedure TVXFBORenderer.SetPostGenerateMipmap(const Value: Boolean);
begin
  if FPostGenerateMipmap <> Value then
    FPostGenerateMipmap := Value;
end;

procedure TVXFBORenderer.SetRootObject(const Value: TVXBaseSceneObject);
begin
  if FRootObject <> Value then
  begin
    if Assigned(FRootObject) then
      FRootObject.RemoveFreeNotification(Self);
    FRootObject := Value;
    if Assigned(FRootObject) then
      FRootObject.FreeNotification(Self);
    StructureChanged;
  end;
end;

procedure TVXFBORenderer.SetStencilPrecision(const Value: TVXStencilPrecision);
begin
  if FStencilPrecision <> Value then
  begin
    FStencilPrecision := Value;
    StructureChanged;
  end;
end;

procedure TVXFBORenderer.SetTargetVisibility(const Value
  : TVXFBOTargetVisibility);
begin
  if FTargetVisibility <> Value then
  begin
    if Assigned(RootObject) then
    begin
      if (TargetVisibility = tvFBOOnly) then
      begin
        // we went from fbo only, restore root's old visibility
        RootObject.Visible := FRootVisible;
      end
      else
      begin
        // we're going to fbo only, save root visibility for later
        FRootVisible := RootObject.Visible;
      end;
    end;

    FTargetVisibility := Value;
    StructureChanged;
  end;
end;

// StoreSceneScaleFactor
//

function TVXFBORenderer.StoreSceneScaleFactor: Boolean;
begin
  Result := (FSceneScaleFactor <> 0.0);
end;

// StoreAspect
//

function TVXFBORenderer.StoreAspect: Boolean;
begin
  Result := (FAspect <> 1.0);
end;

procedure TVXFBORenderer.SetWidth(Value: Integer);
begin
  if FWidth <> Value then
  begin
    FWidth := Value;
    StructureChanged;
  end;
end;

procedure TVXFBORenderer.SetHeight(Value: Integer);
begin
  if FHeight <> Value then
  begin
    FHeight := Value;
    StructureChanged;
  end;
end;

procedure TVXFBORenderer.SetLayer(const Value: Integer);
begin
  if Value <> FFbo.Layer then
  begin
    if FRendering or (ocStructure in Changes) then
      FFbo.Layer := Value
    else
    begin
      FFbo.Bind;
      FFbo.Layer := Value;
      FFbo.Unbind;
    end;
  end;
end;

function TVXFBORenderer.GetLayer: Integer;
begin
  Result := FFbo.Layer;
end;

procedure TVXFBORenderer.SetLevel(const Value: Integer);
var
  w, h: Integer;
begin
  if Value <> FFbo.Level then
  begin
    if FRendering or (ocStructure in Changes) then
    begin
      FFbo.Level := Value;
      w := Width;
      h := Height;
      if FFbo.Level > 0 then
      begin
        w := w shr FFbo.Level;
        h := h shr FFbo.Level;
        if w = 0 then
          w := 1;
        if h = 0 then
          h := 1;
        CurrentVKContext.VKStates.Viewport := Vector4iMake(0, 0, w, h);
      end;
    end
    else
    begin
      FFbo.Bind;
      FFbo.Level := Value;
      FFbo.Unbind;
    end;
  end;
end;

function TVXFBORenderer.GetLevel: Integer;
begin
  Result := FFbo.Level;
end;

initialization

RegisterClasses([TVXFBORenderer]);

end.
