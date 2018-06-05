//
// VXScene Component Library, based on GLScene http://glscene.sourceforge.net
//
{
  Lens flare object.

}
unit VXS.LensFlare;

interface

{$I VXScene.inc}

uses
  Winapi.OpenGL,
  Winapi.OpenGLext,
  System.Classes,
  System.SysUtils,
  System.Math,

  VXS.OpenGL,
  VXS.PersistentClasses,
  VXS.Scene,
  VXS.VectorGeometry,
  VXS.Objects,
  VXS.PipelineTransformation,
  VXS.Context,
  VXS.Color,
  VXS.BaseClasses,
  VXS.RenderContextInfo,
  VXS.State,
  VXS.VectorTypes,
  VXS.Utils,
  VXS.TextureFormat;

type

  TFlareElement = (feGlow, feRing, feStreaks, feRays, feSecondaries);
  TFlareElements = set of TFlareElement;

  { The actual gradients between two colors are, of course, calculated by OpenGL.
    The start and end colors of a gradient are stored to represent the color of
    lens flare elements. }
  TVXFlareGradient = class(TVXUpdateAbleObject)
  private
    FFromColor: TVXColor;
    FToColor: TVXColor;
  protected
    procedure SetFromColor(const val: TVXColor);
    procedure SetToColor(const val: TVXColor);
  public
    constructor Create(AOwner: TPersistent); override;
    constructor CreateInitialized(AOwner: TPersistent; const fromColor, toColor: TColorVector);
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property fromColor: TVXColor read FFromColor write SetFromColor;
    property toColor: TVXColor read FToColor write SetToColor;
  end;

const
  cDefaultFlareElements = [feGlow, feRing, feStreaks, feRays, feSecondaries];

type

  TVXLensFlare = class(TVXBaseSceneObject)
  private
    FSize: Integer;
    FDeltaTime: Single;
    FCurrSize: Single;
    FSeed: Integer;
    FSqueeze: Single;
    FNumStreaks: Integer;
    FStreakWidth, FStreakAngle: Single;
    FNumSecs: Integer;
    FResolution: Integer;
    FAutoZTest: Boolean;
    FElements: TFlareElements;
    FSin20Res, FCos20Res: array of Single;
    FSinRes, FCosRes: array of Single;
    FTexRays: TVXTextureHandle;
    FFlareIsNotOccluded: Boolean;
    FOcclusionQuery: TVXOcclusionQueryHandle;
    FGlowGradient: TVXFlareGradient;
    FRingGradient: TVXFlareGradient;
    FStreaksGradient: TVXFlareGradient;
    FRaysGradient: TVXFlareGradient;
    FSecondariesGradient: TVXFlareGradient;
    FDynamic: Boolean;
    FPreRenderPoint: TVXRenderPoint;
  protected
    procedure SetGlowGradient(const val: TVXFlareGradient);
    procedure SetRingGradient(const val: TVXFlareGradient);
    procedure SetStreaksGradient(const val: TVXFlareGradient);
    procedure SetRaysGradient(const val: TVXFlareGradient);
    procedure SetSecondariesGradient(const val: TVXFlareGradient);
    procedure SetSize(aValue: Integer);
    procedure SetSeed(aValue: Integer);
    procedure SetSqueeze(aValue: Single);
    function StoreSqueeze: Boolean;
    procedure SetNumStreaks(aValue: Integer);
    procedure SetStreakWidth(aValue: Single);
    function StoreStreakWidth: Boolean;
    procedure SetStreakAngle(aValue: Single);
    procedure SetNumSecs(aValue: Integer);
    procedure SetResolution(aValue: Integer);
    procedure SetAutoZTest(aValue: Boolean);
    procedure SetElements(aValue: TFlareElements);
    procedure SetDynamic(aValue: Boolean);
    procedure SetPreRenderPoint(const val: TVXRenderPoint);
    procedure PreRenderEvent(Sender: TObject; var rci: TVXRenderContextInfo);
    procedure PreRenderPointFreed(Sender: TObject);
    // These are quite unusual in that they don't use an RCI, since
    // PreRender is done before proper rendering starts, but we do know
    // which RC is being used, so we can use this state cache
    procedure SetupRenderingOptions(StateCache: TVXStateCache);
    procedure RenderRays(StateCache: TVXStateCache; const size: Single);
    procedure RenderStreaks(StateCache: TVXStateCache);
    procedure RenderRing;
    procedure RenderSecondaries(const posVector: TAffineVector);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure BuildList(var rci: TVXRenderContextInfo); override;
    procedure DoProgress(const progressTime: TVXProgressTimes); override;
    { Prepares pre-rendered texture to speed up actual rendering.
      Will use the currently active context as scratch space, and will
      automatically do nothing if things have already been prepared,
      thus you can invoke it systematically in a Viewer.BeforeRender
      event f.i. }
    procedure PreRender(activeBuffer: TVXSceneBuffer);
    { Access to the Flare's current size.
      Flares decay or grow back over several frames, depending on their
      occlusion status, and this property allows to track or manually
      alter this instantaneous size. }
    property FlareInstantaneousSize: Single read FCurrSize write FCurrSize;
  published
    property GlowGradient: TVXFlareGradient read FGlowGradient write SetGlowGradient;
    property RingGradient: TVXFlareGradient read FRingGradient;
    property StreaksGradient: TVXFlareGradient read FStreaksGradient;
    property RaysGradient: TVXFlareGradient read FRaysGradient;
    property SecondariesGradient: TVXFlareGradient read FSecondariesGradient;
    // MaxRadius of the flare.
    property size: Integer read FSize write SetSize default 50;
    // Random seed
    property Seed: Integer read FSeed write SetSeed;
    // To create elliptic flares.
    property Squeeze: Single read FSqueeze write SetSqueeze stored StoreSqueeze;
    // Number of streaks.
    property NumStreaks: Integer read FNumStreaks write SetNumStreaks default 4;
    // Width of the streaks.
    property StreakWidth: Single read FStreakWidth write SetStreakWidth stored StoreStreakWidth;
    // Angle of the streaks (in degrees)
    property StreakAngle: Single read FStreakAngle write SetStreakAngle;
    // Number of secondary flares.
    property NumSecs: Integer read FNumSecs write SetNumSecs default 8;
    // Number of segments used when rendering circles.
    property Resolution: Integer read FResolution write SetResolution default 64;
    { Automatically computes FlareIsNotOccluded depending on ZBuffer test.
      Not that the automated test may use test result from the previous
      frame into the next (to avoid a rendering stall). }
    property AutoZTest: Boolean read FAutoZTest write SetAutoZTest default True;
    { Is the LensFlare not occluded?.
      If false the flare will fade away, if true, it will fade in and stay.
      This value is automatically updated if AutoZTest is set. }
    property FlareIsNotOccluded: Boolean read FFlareIsNotOccluded write FFlareIsNotOccluded;
    // Which elements should be rendered?
    property Elements: TFlareElements read FElements write SetElements default cDefaultFlareElements;
    { Is the flare size adjusted dynamically?
      If true, the flare size will be grown and reduced over a few frames
      when it switches between occluded and non-occluded states. This
      requires animation to be active, but results in a smoother appearance.
      When false, flare will either be at full size or hidden.
      The flare is always considered non-dynamic at design-time. }
    property Dynamic: Boolean read FDynamic write FDynamic default True;

    { PreRender point for pre-rendered flare textures.
      See PreRender method for more details. }
    property PreRenderPoint: TVXRenderPoint read FPreRenderPoint write SetPreRenderPoint;
    property ObjectsSorting;
    property Position;
    property Visible;
    property OnProgress;
    property Behaviours;
    property Effects;
  end;

// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------

// ------------------
// ------------------ TVXFlareGradient ------------------
// ------------------

constructor TVXFlareGradient.Create(AOwner: TPersistent);
begin
  inherited;
  FFromColor := TVXColor.Create(Self);
  FToColor := TVXColor.Create(Self);
end;

constructor TVXFlareGradient.CreateInitialized(AOwner: TPersistent; const fromColor, toColor: TColorVector);
begin
  Create(AOwner);
  FFromColor.Initialize(fromColor);
  FToColor.Initialize(toColor);
end;

destructor TVXFlareGradient.Destroy;
begin
  FToColor.Free;
  FFromColor.Free;
  inherited;
end;

procedure TVXFlareGradient.Assign(Source: TPersistent);
begin
  if Source is TVXFlareGradient then
  begin
    fromColor := TVXFlareGradient(Source).fromColor;
    toColor := TVXFlareGradient(Source).toColor;
  end;
  inherited;
end;

procedure TVXFlareGradient.SetFromColor(const val: TVXColor);
begin
  FFromColor.Assign(val);
end;

procedure TVXFlareGradient.SetToColor(const val: TVXColor);
begin
  FToColor.Assign(val);
end;

// ------------------
// ------------------ TVXLensFlare ------------------
// ------------------

constructor TVXLensFlare.Create(AOwner: TComponent);
begin
  inherited;
  // Set default parameters:
  ObjectStyle := ObjectStyle + [osDirectDraw, osNoVisibilityCulling];
  FSize := 50;
  FSeed := 1465;
  FSqueeze := 1;
  FNumStreaks := 4;
  FStreakWidth := 2;
  FNumSecs := 8;
  FAutoZTest := True;
  FlareIsNotOccluded := True;
  FDynamic := True;
  SetResolution(64);
  // Render all elements by default.
  FElements := [feGlow, feRing, feStreaks, feRays, feSecondaries];
  // Setup default gradients:
  FGlowGradient := TVXFlareGradient.CreateInitialized(Self, VectorMake(1, 1, 0.8, 0.3), VectorMake(1, 0.2, 0, 0));
  FRingGradient := TVXFlareGradient.CreateInitialized(Self, VectorMake(0.5, 0.2, 0, 0.1), VectorMake(0.5, 0.4, 0, 0.1));
  FStreaksGradient := TVXFlareGradient.CreateInitialized(Self, VectorMake(1, 1, 1, 0.2), VectorMake(0.2, 0, 1, 0));
  FRaysGradient := TVXFlareGradient.CreateInitialized(Self, VectorMake(1, 0.8, 0.5, 0.05), VectorMake(0.5, 0.2, 0, 0));
  FSecondariesGradient := TVXFlareGradient.CreateInitialized(Self, VectorMake(0, 0.2, 1, 0), VectorMake(0, 0.8, 0.2, 0.15));

  FTexRays := TVXTextureHandle.Create;
end;

destructor TVXLensFlare.Destroy;
begin
  PreRenderPoint := nil;
  FGlowGradient.Free;
  FRingGradient.Free;
  FStreaksGradient.Free;
  FRaysGradient.Free;
  FSecondariesGradient.Free;
  FOcclusionQuery.Free;
  FTexRays.Free;
  inherited;
end;

procedure TVXLensFlare.Notification(AComponent: TComponent; Operation: TOperation);
begin
  if (Operation = opRemove) and (AComponent = FPreRenderPoint) then
    PreRenderPoint := nil;
  inherited;
end;

procedure TVXLensFlare.SetupRenderingOptions(StateCache: TVXStateCache);
begin
  with StateCache do
  begin
    Disable(stLighting);
    Disable(stDepthTest);
    Disable(stFog);
    Disable(stColorMaterial);
    Disable(stCullFace);
    DepthWriteMask := False;
    Enable(stBlend);
    SetBlendFunc(bfSrcAlpha, bfOne);
    Disable(stAlphaTest);
    PolygonMode := pmFill;
  end;
end;

procedure TVXLensFlare.RenderRays(StateCache: TVXStateCache; const size: Single);
var
  i: Integer;
  rnd: Single;
begin
{$IFDEF USE_OPENGL_DEBUG}
  if GL.GREMEDY_string_marker then
    GL.StringMarkerGREMEDY(14, 'LensFlare.Rays');
{$ENDIF}
  with StateCache do
  begin
    LineWidth := 1;
    Disable(stLineSmooth);
    Disable(stLineStipple);
  end;

  glBegin(GL_LINES);
  for i := 0 to Resolution * 20 - 1 do
  begin
    if (i and 1) <> 0 then
      rnd := 1.5 * Random * size
    else
      rnd := Random * size;
    glColor4fv(RaysGradient.fromColor.AsAddress);
    glVertex2f(0, 0);
    glColor4fv(RaysGradient.toColor.AsAddress);
    glVertex2f(rnd * FCos20Res[i], rnd * FSin20Res[i] * Squeeze);
  end;
  glEnd;
end;

procedure TVXLensFlare.RenderStreaks(StateCache: TVXStateCache);
var
  i: Integer;
  a, f, s, c: Single;
begin
{$IFDEF USE_OPENGL_DEBUG}
  if GL.GREMEDY_string_marker then
    GL.StringMarkerGREMEDY(17, 'LensFlare.Streaks');
{$ENDIF}
  StateCache.Enable(stLineSmooth);
  StateCache.LineWidth := StreakWidth;
  a := c2PI / NumStreaks;
  f := 1.5 * FCurrSize;
  glBegin(GL_LINES);
  for i := 0 to NumStreaks - 1 do
  begin
    SinCosine(StreakAngle * cPIdiv180 + a * i, f, s, c);
    glColor4fv(StreaksGradient.fromColor.AsAddress);
    glVertex3fv(@NullVector);
    glColor4fv(StreaksGradient.toColor.AsAddress);
    glVertex2f(c, Squeeze * s);
  end;
  glEnd;
  StateCache.Disable(stLineSmooth);
end;

procedure TVXLensFlare.RenderRing;
var
  i: Integer;
  rW, s0, c0, s, c: Single;
begin
{$IFDEF USE_OPENGL_DEBUG}
  if GL.GREMEDY_string_marker then
    GL.StringMarkerGREMEDY(14, 'LensFlare.Ring');
{$ENDIF}
  rW := FCurrSize * (1 / 15); // Ring width
  glBegin(GL_QUADS);
  s0 := 0;
  c0 := 0.6;
  for i := 0 to Resolution - 1 do
  begin
    s := s0;
    c := c0;
    s0 := FSinRes[i] * 0.6 * Squeeze;
    c0 := FCosRes[i] * 0.6;

    glColor4fv(GlowGradient.toColor.AsAddress);
    glVertex2f((FCurrSize - rW) * c, (FCurrSize - rW) * s);
    glColor4fv(RingGradient.fromColor.AsAddress);
    glVertex2f(FCurrSize * c, Squeeze * FCurrSize * s);

    glVertex2f(FCurrSize * c0, FCurrSize * s0);
    glColor4fv(GlowGradient.toColor.AsAddress);
    glVertex2f((FCurrSize - rW) * c0, (FCurrSize - rW) * s0);

    glColor4fv(RingGradient.fromColor.AsAddress);
    glVertex2f(FCurrSize * c, FCurrSize * s);
    glVertex2f(FCurrSize * c0, FCurrSize * s0);

    glColor4fv(GlowGradient.toColor.AsAddress);
    glVertex2f((FCurrSize + rW) * c0, (FCurrSize + rW) * s0);
    glVertex2f((FCurrSize + rW) * c, (FCurrSize + rW) * s);
  end;
  glEnd;
end;

procedure TVXLensFlare.RenderSecondaries(const posVector: TAffineVector);
var
  i, j: Integer;
  rnd: Single;
  v: TAffineVector;
  grad: TVXFlareGradient;
begin
{$IFDEF USE_OPENGL_DEBUG}
  if GL.GREMEDY_string_marker then
    GL.StringMarkerGREMEDY(21, 'LensFlare.Secondaries');
{$ENDIF}
  // Other secondaries (plain gradiented circles, like the glow):
  for j := 1 to NumSecs do
  begin
    rnd := 2 * Random - 1;
    // If rnd < 0 then the secondary glow will end up on the other side
    // of the origin. In this case, we can push it really far away from
    // the flare. If  the secondary is on the flare's side, we pull it
    // slightly towards the origin to avoid it winding up in the middle
    // of the flare.
    if rnd < 0 then
      v := VectorScale(posVector, rnd)
    else
      v := VectorScale(posVector, 0.8 * rnd);
    if j mod 3 = 0 then
      grad := GlowGradient
    else
      grad := SecondariesGradient;
    rnd := (Random + 0.1) * FCurrSize * 0.25;

    glBegin(GL_TRIANGLE_FAN);
    glColor4fv(grad.fromColor.AsAddress);
    glVertex2f(v.X, v.Y);
    glColor4fv(grad.toColor.AsAddress);
    for i := 0 to Resolution - 1 do
      glVertex2f(FCosRes[i] * rnd + v.X, FSinRes[i] * rnd + v.Y);
    glEnd;
  end;
end;

procedure TVXLensFlare.BuildList(var rci: TVXRenderContextInfo);
var
  i: Integer;
  depth, dist: Single;
  posVector, v, rv: TAffineVector;
  screenPos: TAffineVector;
  flareInViewPort, dynamicSize: Boolean;
  oldSeed: LongInt;
  projMatrix: TMatrix;
  CurrentBuffer: TVXSceneBuffer;
begin
  if (rci.drawState = dsPicking) then
  begin
    if Count <> 0 then
      Self.RenderChildren(0, Count - 1, rci);
    Exit;
  end;
  CurrentBuffer := TVXSceneBuffer(rci.buffer);

  SetVector(v, AbsolutePosition);
  // are we looking towards the flare?
  rv := VectorSubtract(v, PAffineVector(@rci.cameraPosition)^);
  if VectorDotProduct(rci.cameraDirection, rv) > 0 then
  begin
    // find out where it is on the screen.
    screenPos := CurrentBuffer.WorldToScreen(v);
    flareInViewPort := (screenPos.X < rci.viewPortSize.cx) and (screenPos.X >= 0) and (screenPos.Y < rci.viewPortSize.cy) and
      (screenPos.Y >= 0);
  end
  else
    flareInViewPort := False;

  dynamicSize := FDynamic and not(csDesigning in ComponentState);
  if dynamicSize then
  begin
    // make the glow appear/disappear progressively
    if flareInViewPort and FlareIsNotOccluded then
    begin
      FCurrSize := FCurrSize + FDeltaTime * 10 * size;
      if FCurrSize > size then
        FCurrSize := size;
    end
    else
    begin
      FCurrSize := FCurrSize - FDeltaTime * 10 * size;
      if FCurrSize < 0 then
        FCurrSize := 0;
    end;
  end
  else
  begin
    if flareInViewPort and FlareIsNotOccluded then
      FCurrSize := size
    else
      FCurrSize := 0;
  end;

  // Prepare matrices
  glPushMatrix;
  glLoadMatrixf(@CurrentBuffer.BaseProjectionMatrix);

  glMatrixMode(GL_PROJECTION);
  glPushMatrix;
  projMatrix := IdentityHmgMatrix;
  projMatrix.X.X := 2 / rci.viewPortSize.cx;
  projMatrix.Y.Y := 2 / rci.viewPortSize.cy;
  glLoadMatrixf(@projMatrix);

  MakeVector(posVector, screenPos.X - rci.viewPortSize.cx * 0.5, screenPos.Y - rci.viewPortSize.cy * 0.5, 0);

  if AutoZTest then
  begin
    if dynamicSize and (TVXOcclusionQueryHandle.IsSupported = True) then  //GL_OCCLUSION_TEST_HP
    begin
      // hardware-based occlusion test is possible
      FlareIsNotOccluded := True;

      rci.VXStates.SetColorMask([]);
      rci.VXStates.Disable(stAlphaTest);
      rci.VXStates.DepthWriteMask := GLboolean(False);
      rci.VXStates.Enable(stDepthTest);
      rci.VXStates.DepthFunc := cfLEqual;

      if TVXOcclusionQueryHandle.IsSupported > False then
      begin
        // preferred method, doesn't stall rendering too badly
        if not Assigned(FOcclusionQuery) then
          FOcclusionQuery := TVXOcclusionQueryHandle.Create;
        FOcclusionQuery.AllocateHandle;
        if FOcclusionQuery.IsDataNeedUpdate then
          FOcclusionQuery.NotifyDataUpdated
        else
          FlareIsNotOccluded := (FOcclusionQuery.PixelCount <> 0);
        FOcclusionQuery.BeginQuery;
      end
      else
      begin
        // occlusion_test, stalls rendering a bit
        glEnable(GL_OCCLUSION_TEST_HP);
      end;

      glBegin(GL_QUADS);
      glVertex3f(posVector.X + 2, posVector.Y, 1);
      glVertex3f(posVector.X, posVector.Y + 2, 1);
      glVertex3f(posVector.X - 2, posVector.Y, 1);
      glVertex3f(posVector.X, posVector.Y - 2, 1);
      glEnd;

      if TVXOcclusionQueryHandle.IsSupported > False then
        FOcclusionQuery.EndQuery
      else
      begin
        glDisable(GL_OCCLUSION_TEST_HP);
        glGetBooleanv(GL_OCCLUSION_TEST_RESULT_HP, @FFlareIsNotOccluded)
      end;

      rci.VXStates.DepthFunc := cfLEqual;
      rci.VXStates.SetColorMask(cAllColorComponents);
    end
    else
    begin
      // Compares the distance to the lensflare, to the z-buffer depth.
      // This prevents the flare from being occluded by objects BEHIND the light.
      depth := CurrentBuffer.PixelToDistance(Round(screenPos.X), Round(rci.viewPortSize.cy - screenPos.Y));
      dist := VectorDistance(rci.cameraPosition, Self.AbsolutePosition);
      FlareIsNotOccluded := ((dist - depth) < 1);
    end;
  end;

  if FCurrSize >= 0 then
  begin

    // Random seed must be backed up, could be used for other purposes
    // (otherwise we essentially reset the random generator at each frame)
    oldSeed := RandSeed;
    RandSeed := Seed;

    SetupRenderingOptions(rci.VXStates);

    if [feGlow, feStreaks, feRays, feRing] * Elements <> [] then
    begin
      glTranslatef(posVector.X, posVector.Y, posVector.Z);

      // Glow (a circle with transparent edges):
      if feGlow in Elements then
      begin
        glBegin(GL_TRIANGLE_FAN);
        glColor4fv(GlowGradient.fromColor.AsAddress);
        glVertex2f(0, 0);
        glColor4fv(GlowGradient.toColor.AsAddress);
        for i := 0 to Resolution - 1 do
          glVertex2f(FCurrSize * FCosRes[i], Squeeze * FCurrSize * FSinRes[i]);
        glEnd;
      end;

      if feStreaks in Elements then
        RenderStreaks(rci.VXStates);

      // Rays (random-length lines from the origin):
      if feRays in Elements then
      begin
        if FTexRays.Handle <> 0 then
        begin
{$IFDEF USE_OPENGL_DEBUG}
          if GL.GREMEDY_string_marker then
            GL.StringMarkerGREMEDY(19, 'LensFlare.RaysQuad');
{$ENDIF}
          rci.VXStates.TextureBinding[0, ttTexture2D] := FTexRays.Handle;
          rci.VXStates.ActiveTextureEnabled[ttTexture2D] := True;
          glTexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_REPLACE);

          glBegin(GL_QUADS);
          glTexCoord2f(0, 0);
          glVertex2f(-FCurrSize, -FCurrSize);
          glTexCoord2f(1, 0);
          glVertex2f(FCurrSize, -FCurrSize);
          glTexCoord2f(1, 1);
          glVertex2f(FCurrSize, FCurrSize);
          glTexCoord2f(0, 1);
          glVertex2f(-FCurrSize, FCurrSize);
          glEnd;

          rci.VXStates.ActiveTextureEnabled[ttTexture2D] := False;
        end
        else
          RenderRays(rci.VXStates, FCurrSize);
      end;

      if feRing in Elements then
        RenderRing;

      glLoadMatrixf(@projMatrix);
    end;

    if feSecondaries in Elements then
      RenderSecondaries(posVector);

    RandSeed := oldSeed;
  end;

  glPopMatrix;
  glMatrixMode(GL_MODELVIEW);
  glPopMatrix;

  if Count > 0 then
    Self.RenderChildren(0, Count - 1, rci);
end;

procedure TVXLensFlare.DoProgress(const progressTime: TVXProgressTimes);
begin
  inherited;
  FDeltaTime := progressTime.deltaTime;
end;

procedure TVXLensFlare.PreRender(activeBuffer: TVXSceneBuffer);
var
  texSize, maxSize: Integer;
  StateCache: TVXStateCache;
begin
  if FTexRays.Handle <> 0 then
    Exit;
  with activeBuffer.RenderingContext do
  begin
    StateCache := VXStates;
    PipelineTransformation.Push;
    PipelineTransformation.SetProjectionMatrix(CreateOrthoMatrix(0, activeBuffer.Width, 0, activeBuffer.Height, -1, 1));
    PipelineTransformation.SetViewMatrix(IdentityHmgMatrix);
  end;
  SetupRenderingOptions(StateCache);

  texSize := RoundUpToPowerOf2(size);
  if texSize < size * 1.5 then
    texSize := texSize * 2;
  glGetIntegerv(GL_MAX_TEXTURE_SIZE, @maxSize);
  if texSize > maxSize then
    texSize := maxSize;

  StateCache.Disable(stBlend);
  glColor4f(0, 0, 0, 0);
  glBegin(GL_QUADS);
  glVertex2f(0, 0);
  glVertex2f(texSize + 4, 0);
  glVertex2f(texSize + 4, texSize + 4);
  glVertex2f(0, texSize + 4);
  glEnd;
  StateCache.Enable(stBlend);

  glTranslatef(texSize * 0.5 + 2, texSize * 0.5 + 2, 0);
  RenderRays(StateCache, texSize * 0.5);

  FTexRays.AllocateHandle;
  StateCache.TextureBinding[0, ttTexture2D] := FTexRays.Handle;
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);

  glCopyTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA, 2, 2, texSize, texSize, 0);

  activeBuffer.RenderingContext.PipelineTransformation.Pop;

///  CheckOpenGLError;
end;

procedure TVXLensFlare.SetGlowGradient(const val: TVXFlareGradient);
begin
  FGlowGradient.Assign(val);
  StructureChanged;
end;

procedure TVXLensFlare.SetRingGradient(const val: TVXFlareGradient);
begin
  FRingGradient.Assign(val);
  StructureChanged;
end;

procedure TVXLensFlare.SetStreaksGradient(const val: TVXFlareGradient);
begin
  FStreaksGradient.Assign(val);
  StructureChanged;
end;

procedure TVXLensFlare.SetRaysGradient(const val: TVXFlareGradient);
begin
  FRaysGradient.Assign(val);
  StructureChanged;
end;

procedure TVXLensFlare.SetSecondariesGradient(const val: TVXFlareGradient);
begin
  FSecondariesGradient.Assign(val);
  StructureChanged;
end;

procedure TVXLensFlare.SetSize(aValue: Integer);
begin
  FSize := aValue;
  StructureChanged;
end;

procedure TVXLensFlare.SetSeed(aValue: Integer);
begin
  FSeed := aValue;
  StructureChanged;
end;

procedure TVXLensFlare.SetSqueeze(aValue: Single);
begin
  FSqueeze := aValue;
  StructureChanged;
end;

function TVXLensFlare.StoreSqueeze: Boolean;
begin
  Result := (FSqueeze <> 1);
end;

procedure TVXLensFlare.SetNumStreaks(aValue: Integer);
begin
  FNumStreaks := aValue;
  StructureChanged;
end;

procedure TVXLensFlare.SetStreakWidth(aValue: Single);
begin
  FStreakWidth := aValue;
  StructureChanged;
end;

function TVXLensFlare.StoreStreakWidth: Boolean;
begin
  Result := (FStreakWidth <> 2);
end;

procedure TVXLensFlare.SetStreakAngle(aValue: Single);
begin
  FStreakAngle := aValue;
  StructureChanged;
end;

procedure TVXLensFlare.SetNumSecs(aValue: Integer);
begin
  FNumSecs := aValue;
  StructureChanged;
end;

procedure TVXLensFlare.SetResolution(aValue: Integer);
begin
  if FResolution <> aValue then
  begin
    FResolution := aValue;
    StructureChanged;
    SetLength(FSin20Res, 20 * FResolution);
    SetLength(FCos20Res, 20 * FResolution);
    PrepareSinCosCache(FSin20Res, FCos20Res, 0, 360);
    SetLength(FSinRes, FResolution);
    SetLength(FCosRes, FResolution);
    PrepareSinCosCache(FSinRes, FCosRes, 0, 360);
  end;
end;

procedure TVXLensFlare.SetAutoZTest(aValue: Boolean);
begin
  if FAutoZTest <> aValue then
  begin
    FAutoZTest := aValue;
    StructureChanged;
  end;
end;

procedure TVXLensFlare.SetElements(aValue: TFlareElements);
begin
  if FElements <> aValue then
  begin
    FElements := aValue;
    StructureChanged;
  end;
end;

procedure TVXLensFlare.SetDynamic(aValue: Boolean);
begin
  if aValue <> FDynamic then
  begin
    FDynamic := aValue;
    NotifyChange(Self);
  end;
end;

procedure TVXLensFlare.SetPreRenderPoint(const val: TVXRenderPoint);
begin
  if val <> FPreRenderPoint then
  begin
    if Assigned(FPreRenderPoint) then
      FPreRenderPoint.UnRegisterCallBack(Self.PreRenderEvent);
    FPreRenderPoint := val;
    if Assigned(FPreRenderPoint) then
      FPreRenderPoint.RegisterCallBack(Self.PreRenderEvent, Self.PreRenderPointFreed);
  end;
end;

procedure TVXLensFlare.PreRenderEvent(Sender: TObject; var rci: TVXRenderContextInfo);
begin
  PreRender(rci.buffer as TVXSceneBuffer);
end;

procedure TVXLensFlare.PreRenderPointFreed(Sender: TObject);
begin
  FPreRenderPoint := nil;
end;

// ------------------------------------------------------------------
initialization

// ------------------------------------------------------------------

RegisterClasses([TVXLensFlare]);

end.
