//
// This unit is part of the GLScene Project, http://glscene.org
//
{: GLBLensFlare<p>

   Lens flare object.<p>

 <b>History : </b><font size=-1><ul>
      <li>03/03/10 - Yar - Adapted GLLensFlare to OpenGL3x
 </ul></font><p>

}
unit GL3xLensFlare;

interface

{$I GLScene.inc}

uses
  Classes, GLScene, VectorGeometry, GLObjects, OpenGL1x, GLState,
  GLContext, GLColor, BaseClasses, GLRenderContextInfo, GLLensFlare,
  GLFBO, GL3xShadersManager, GLVBOManagers, GL3xMaterial, GLTexture, GLGraphics;

type

  // TGL3xLensFlare
  //
  TGL3xLensFlare = class(TGLBaseSceneObject)
  private
    { Private Declarations }
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
    FRaysTexture: TGLTexture;
    FFlareIsNotOccluded: Boolean;
    FOcclusionQuery: TGLOcclusionQueryHandle;
    FGlowGradient: TGLFlareGradient;
    FRingGradient: TGLFlareGradient;
    FStreaksGradient: TGLFlareGradient;
    FRaysGradient: TGLFlareGradient;
    FSecondariesGradient: TGLFlareGradient;
    FDynamic: Boolean;
    FBuiltPropertiesGlow: TGLBuiltProperties;
    FBuiltPropertiesRays: TGLBuiltProperties;
    FBuiltPropertiesStreaks: TGLBuiltProperties;
    FBuiltPropertiesRing: TGLBuiltProperties;
    FBuiltPropertiesSecondaries: TGLBuiltProperties;
  protected
    { Protected Declarations }
    procedure SetGlowGradient(const val: TGLFlareGradient);
    procedure SetRingGradient(const val: TGLFlareGradient);
    procedure SetStreaksGradient(const val: TGLFlareGradient);
    procedure SetRaysGradient(const val: TGLFlareGradient);
    procedure SetSecondariesGradient(const val: TGLFlareGradient);
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

    procedure SetupRenderingOptions(var rci: TRenderContextInfo);

    {: Prepares rays texture.<p> }
    procedure PrepareRayTexture(var rci: TRenderContextInfo);
  public
    { Public Declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure DoRender(var ARci: TRenderContextInfo;
      ARenderSelf, ARenderChildren: Boolean); override;
    procedure BuildList(var rci: TRenderContextInfo); override;
    procedure DoProgress(const progressTime: TProgressTimes); override;

    {: Access to the Flare's current size.<p>
       Flares decay or grow back over several frames, depending on their
       occlusion status, and this property allows to track or manually
       alter this instantaneous size. }
    property FlareInstantaneousSize: Single read FCurrSize write FCurrSize;

  published
    { Public Declarations }
    property GlowGradient: TGLFlareGradient read FGlowGradient write
      SetGlowGradient;
    property RingGradient: TGLFlareGradient read FRingGradient;
    property StreaksGradient: TGLFlareGradient read FStreaksGradient;
    property RaysGradient: TGLFlareGradient read FRaysGradient;
    property SecondariesGradient: TGLFlareGradient read FSecondariesGradient;

    //: MaxRadius of the flare.
    property Size: Integer read FSize write SetSize default 64;
    //: Random seed
    property Seed: Integer read FSeed write SetSeed;
    //: To create elliptic flares.
    property Squeeze: Single read FSqueeze write SetSqueeze stored StoreSqueeze;
    //: Number of streaks.
    property NumStreaks: Integer read FNumStreaks write SetNumStreaks default 4;
    //: Width of the streaks.
    property StreakWidth: Single read FStreakWidth write SetStreakWidth stored
      StoreStreakWidth;
    //: Angle of the streaks (in degrees)
    property StreakAngle: Single read FStreakAngle write SetStreakAngle;
    //: Number of secondary flares.
    property NumSecs: Integer read FNumSecs write SetNumSecs default 8;
    //: Number of segments used when rendering circles.
    property Resolution: Integer read FResolution write SetResolution default
      64;
    {: Automatically computes FlareIsNotOccluded depending on ZBuffer test.<p>
       Not that the automated test may use test result from the previous
       frame into the next (to avoid a rendering stall). }
    property AutoZTest: Boolean read FAutoZTest write SetAutoZTest default True;
    {: Is the LensFlare not occluded?.<p>
       If false the flare will fade away, if true, it will fade in and stay.
       This value is automatically updated if AutoZTest is set. }
    property FlareIsNotOccluded: Boolean read FFlareIsNotOccluded write
      FFlareIsNotOccluded;
    //: Which elements should be rendered?
    property Elements: TFlareElements read FElements write SetElements default
      cDefaultFlareElements;
    {: Is the flare size adjusted dynamically?<p>
       If true, the flare size will be grown and reduced over a few frames
       when it switches between occluded and non-occluded states. This
       requires animation to be active, but results in a smoother appearance.<br>
       When false, flare will either be at full size or hidden.<p>
       The flare is always considered non-dynamic at design-time. }
    property Dynamic: Boolean read FDynamic write FDynamic default True;

    property ObjectsSorting;
    property Position;
    property Visible;
    property OnProgress;
    property Behaviours;
    property Effects;
  end;

  // ------------------------------------------------------------------
  // ------------------------------------------------------------------
  // ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

uses GLUtils, VectorTypes, VectorGeometryEXT, GLTextureFormat;

{$IFDEF GLS_COMPILER_2005_UP}{$REGION 'Shaders'}{$ENDIF}
const
  Gradient_vp120: AnsiString =
    '#version 120' + #10#13 +
    'attribute vec3 Position;' + #10#13 +
    'attribute vec4 VertexColor;' + #10#13 +
    'uniform mat4 ProjectionMatrix;' + #10#13 +
    'varying vec4 color;' + #10#13 +
    'void main(void)' + #10#13 +
    '{' + #10#13 +
    ' color = VertexColor;' + #10#13 +
    ' gl_Position = ProjectionMatrix * vec4(Position, 1.0);' + #10#13 +
    '}';
  Gradient_fp120: AnsiString =
    '#version 120' + #10#13 +
    'varying vec4 color;' + #10#13 +
    'void main(void)' + #10#13 +
    '{' + #10#13 +
    ' gl_FragColor = color;' + #10#13 +
    '}';
  RayTex_vp120: AnsiString =
    '#version 120' + #10#13 +
    'attribute vec3 Position;' + #10#13 +
    'attribute vec2 TexCoord0;' + #10#13 +
    'uniform mat4 ProjectionMatrix;' + #10#13 +
    'varying vec2 texcoord;' + #10#13 +
    'void main(void)' + #10#13 +
    '{' + #10#13 +
    ' texcoord = TexCoord0;' + #10#13 +
    ' gl_Position = ProjectionMatrix * vec4(Position, 1.0);' + #10#13 +
    '}';
  RayTex_fp120: AnsiString =
    '#version 120' + #10#13 +
    'varying vec2 texcoord;' + #10#13 +
    'uniform sampler2D TexUnit0;' + #10#13 +
    'void main(void)' + #10#13 +
    '{' + #10#13 +
    ' gl_FragColor = texture2D(TexUnit0, texcoord);' + #10#13 +
    '}';

  Gradient_vp150: AnsiString =
    '#version 150' + #10#13 +
    'in vec3 Position;' + #10#13 +
    'in vec4 VertexColor;' + #10#13 +
    'uniform mat4 ProjectionMatrix;' + #10#13 +
    'out vec4 color;' + #10#13 +
    'void main(void)' + #10#13 +
    '{' + #10#13 +
    ' color = VertexColor;' + #10#13 +
    ' gl_Position = ProjectionMatrix * vec4(Position, 1.0);' + #10#13 +
    '}';
  Gradient_fp150: AnsiString =
    '#version 150' + #10#13 +
    'precision highp float;'+#10#13+
    'in vec4 color;' + #10#13 +
    'out vec4 FragColor;' + #10#13 +
    'void main(void)' + #10#13 +
    '{' + #10#13 +
    ' FragColor = color;' + #10#13 +
    '}';
  RayTex_vp150: AnsiString =
    '#version 150' + #10#13 +
    'in vec3 Position;' + #10#13 +
    'in vec2 TexCoord0;' + #10#13 +
    'uniform mat4 ProjectionMatrix;' + #10#13 +
    'out vec2 texcoord;' + #10#13 +
    'void main(void)' + #10#13 +
    '{' + #10#13 +
    ' texcoord = TexCoord0;' + #10#13 +
    ' gl_Position = ProjectionMatrix * vec4(Position, 1.0);' + #10#13 +
    '}';
  RayTex_fp150: AnsiString =
    '#version 150' + #10#13 +
    'precision highp float;'+#10#13+
    'in vec2 texcoord;' + #10#13 +
    'out vec4 FragColor;' + #10#13 +
    'uniform sampler2D TexUnit0;' + #10#13 +
    'void main(void)' + #10#13 +
    '{' + #10#13 +
    ' FragColor = texture(TexUnit0, texcoord);' + #10#13 +
    '}';
{$IFDEF GLS_COMPILER_2005_UP}{$ENDREGION}{$ENDIF}

var
  GradientProgram: TGLProgramHandle = nil;
  RaysProgram: TGLProgramHandle = nil;
  ProgramsWorks: Boolean = true;

  // ------------------
  // ------------------ TGL3xLensFlare ------------------
  // ------------------

  // Create
  //

constructor TGL3xLensFlare.Create(AOwner: TComponent);
begin
  inherited;
  // Set default parameters:
  ObjectStyle := ObjectStyle +
    [osDirectDraw, osNoVisibilityCulling, osBuiltStage];
  FSize := 64;
  FSeed := 1465;
  FSqueeze := 1;
  FNumStreaks := 4;
  FStreakWidth := 1;
  FNumSecs := 8;
  FAutoZTest := True;
  FlareIsNotOccluded := True;
  FDynamic := True;

  SetResolution(64);

  // Render all elements by default.
  FElements := [feGlow, feRing, feStreaks, feRays, feSecondaries];
  // Setup default gradients:
  FGlowGradient := TGLFlareGradient.CreateInitialized(Self,
    VectorMake(1, 1, 0.8, 0.3), VectorMake(1, 0.2, 0, 0));
  FRingGradient := TGLFlareGradient.CreateInitialized(Self,
    VectorMake(0.5, 0.2, 0, 0.1), VectorMake(0.5, 0.4, 0, 0.1));
  FStreaksGradient := TGLFlareGradient.CreateInitialized(Self,
    VectorMake(1, 1, 1, 0.2), VectorMake(0.2, 0, 1, 0));
  FRaysGradient := TGLFlareGradient.CreateInitialized(Self,
    VectorMake(1, 0.8, 0.5, 0.05), VectorMake(0.5, 0.2, 0, 0));
  FSecondariesGradient := TGLFlareGradient.CreateInitialized(Self,
    VectorMake(0, 0.2, 1, 0), VectorMake(0, 0.8, 0.2, 0.15));

  FRaysTexture := TGLTexture.Create(Self);
  FRaysTexture.TextureWrap := twSeparate;
  FRaysTexture.TextureWrapS := twClampToEdge;
  FRaysTexture.TextureWrapT := twClampToEdge;
  FRaysTexture.MinFilter := miLinear;
  FRaysTexture.TextureFormatEx := tfRGB8;
  FRaysTexture.ImageClassName := TGLBlankImage.ClassName;
  TGLBlankImage(FRaysTexture.Image).Width := 2*FSize;
  TGLBlankImage(FRaysTexture.Image).Height := 2*FSize;
  FBuiltPropertiesGlow := TGLBuiltProperties.Create(Self);
  FBuiltPropertiesRays := TGLBuiltProperties.Create(Self);
  FBuiltPropertiesStreaks := TGLBuiltProperties.Create(Self);
  FBuiltPropertiesRing := TGLBuiltProperties.Create(Self);
  FBuiltPropertiesSecondaries := TGLBuiltProperties.Create(Self);
end;

// Destroy
//

destructor TGL3xLensFlare.Destroy;
begin
  FGlowGradient.Free;
  FRingGradient.Free;
  FStreaksGradient.Free;
  FRaysGradient.Free;
  FSecondariesGradient.Free;
  FOcclusionQuery.Free;
  FRaysTexture.Free;
  FBuiltPropertiesGlow.Free;
  FBuiltPropertiesRays.Free;
  FBuiltPropertiesStreaks.Free;
  FBuiltPropertiesRing.Free;
  FBuiltPropertiesSecondaries.Free;
  inherited;
end;

// SetupRenderingOptions
//

procedure TGL3xLensFlare.SetupRenderingOptions(var rci: TRenderContextInfo);
begin
  with rci.GLStates do
  begin
    SetGLColorWriting(True);
    Disable(stDepthTest);
    Disable(stCullFace);
    Enable(stBlend);
    DepthWriteMask := False;
    SetBlendFunc(bfSrcAlpha, bfOne);
  end;
end;

procedure TGL3xLensFlare.DoRender(var ARci: TRenderContextInfo;
  ARenderSelf, ARenderChildren: Boolean);
var
  oldSeed: LongInt;
  i: Integer;
  depth, dist: Single;
  posVector, v, rv: TAffineVector;
  screenPos: TAffineVector;
  flareInViewPort, usedOcclusionQuery, dynamicSize: Boolean;
  projMatrix, M: TMatrix;
  rnd, lW: Single;
begin
  if (ARci.drawState = dsPicking) then
  begin
    if Count <> 0 then
      Self.RenderChildren(0, Count - 1, ARci);
    exit;
  end;
  // Render self
  if GL_VERSION_2_1 and not (csDesigning in ComponentState) then
  begin
    if GradientProgram.Handle = 0 then
    begin
      GradientProgram.AllocateHandle;
      with GradientProgram do
      begin
        if GL_VERSION_3_2 then
        begin
          AddShader(TGLVertexShaderHandle, string(Gradient_vp150), true);
          AddShader(TGLFragmentShaderHandle, string(Gradient_fp150), true);
        end
        else begin
          AddShader(TGLVertexShaderHandle, string(Gradient_vp120), true);
          AddShader(TGLFragmentShaderHandle, string(Gradient_fp120), true);
        end;
        try
          LinkProgram;
          ValidateProgram;
        except
          ProgramsWorks := false;
        end;
      end;
    end;

    if RaysProgram.Handle = 0 then
    begin
      RaysProgram.AllocateHandle;
      with RaysProgram do
      begin
        if GL_VERSION_3_2 then
        begin
          AddShader(TGLVertexShaderHandle, string(RayTex_vp150), true);
          AddShader(TGLFragmentShaderHandle, string(RayTex_fp150), true);
        end
        else begin
          AddShader(TGLVertexShaderHandle, string(RayTex_vp120), true);
          AddShader(TGLFragmentShaderHandle, string(RayTex_fp120), true);
        end;
        try
          LinkProgram;
          ValidateProgram;
        except
          ProgramsWorks := false;
        end;
      end;
    end;

    if ProgramsWorks then
    begin
      // Random seed must be backed up, could be used for other purposes
          // (otherwise we essentially reset the random generator at each frame)
      oldSeed := RandSeed;
      RandSeed := Seed;

      if osBuiltStage in ObjectStyle then
      begin
        try
          Self.BuildList(ARci);
        except
          StaticVBOManager.Discard;
          Self.Visible := false;
        end;
      end
      else
      begin
        SetVector(v, AbsolutePosition);
        // are we looking towards the flare?
        rv := VectorSubtract(v, PAffineVector(@ARci.cameraPosition)^);
        if VectorDotProduct(ARci.cameraDirection, rv) > 0 then
        begin
          // find out where it is on the screen.
          screenPos := TGLSceneBuffer(ARci.buffer).WorldToScreen(v);
          flareInViewPort := (screenPos[0] < ARci.viewPortSize.cx)
            and (screenPos[0] >= 0)
            and (screenPos[1] < ARci.viewPortSize.cy) and (screenPos[1] >= 0);
        end
        else
          flareInViewPort := False;

        dynamicSize := FDynamic and not (csDesigning in ComponentState);
        if dynamicSize then
        begin
          // make the glow appear/disappear progressively
          if flareInViewPort and FlareIsNotOccluded then
          begin
            FCurrSize := FCurrSize + FDeltaTime * 10 * Size;
            if FCurrSize > Size then
              FCurrSize := Size;
          end
          else
          begin
            FCurrSize := FCurrSize - FDeltaTime * 10 * Size;
            if FCurrSize < 0 then
              FCurrSize := 0;
          end;
        end
        else
        begin
          if flareInViewPort and FlareIsNotOccluded then
            FCurrSize := Size
          else
            FCurrSize := 0;
        end;

        // Prepare matrices
        projMatrix := IdentityHmgMatrix;
        projMatrix[0][0] := 2 / ARci.viewPortSize.cx;
        projMatrix[1][1] := 2 / ARci.viewPortSize.cy;

        GradientProgram.UseProgramObject;
        if not FRaysTexture.IsHandleAllocated then
          PrepareRayTexture(ARci);

        MakeVector(posVector,
          screenPos[0] - ARci.viewPortSize.cx * 0.5,
          screenPos[1] - ARci.viewPortSize.cy * 0.5,
          0);

        if AutoZTest then
        begin
          if dynamicSize and (GL_HP_occlusion_test or
            TGLOcclusionQueryHandle.IsSupported) then
          begin
            // hardware-based occlusion test is possible
            FlareIsNotOccluded := True;

            ARci.GLStates.SetGLColorWriting(False);
            ARci.GLStates.DepthWriteMask := False;
            ARci.GLStates.Enable(stDepthTest);

            usedOcclusionQuery := TGLOcclusionQueryHandle.IsSupported;
            if usedOcclusionQuery then
            begin
              // preferred method, doesn't stall rendering too badly
              if not Assigned(FOcclusionQuery) then
                FOcclusionQuery := TGLOcclusionQueryHandle.Create;
              if FOcclusionQuery.Handle = 0 then
              begin
                FOcclusionQuery.AllocateHandle;
                FOcclusionQuery.BeginQuery;
              end
              else
              begin
                if FOcclusionQuery.RenderingContext = CurrentGLContext then
                begin
                  FlareIsNotOccluded := (FOcclusionQuery.PixelCount <> 0);
                  FOcclusionQuery.BeginQuery;
                end
                else
                  usedOcclusionQuery := False;
              end;
            end;
            if not usedOcclusionQuery then
            begin
              // occlusion_test, stalls rendering a bit
              glEnable(GL_OCCLUSION_TEST_HP);
            end;
            ARci.GLStates.DepthFunc := cfLequal;
            GradientProgram.UniformMatrix4fv['ProjectionMatrix'] := projMatrix;
            with DynamicVBOManager do
            begin
              BeginObject(FBuiltPropertiesRays);
              Attribute3f(attrPosition, 0, 0, 0);
              Attribute4f(attrVertexColor, 1, 1, 1, 1);
              BeginPrimitives(GLVBOM_TRIANGLES);
              Attribute3f(attrPosition,posVector[0] + 2, posVector[1], 1);
              EmitVertex;
              Attribute3f(attrPosition,posVector[0], posVector[1] + 2, 1);
              EmitVertex;
              Attribute3f(attrPosition,posVector[0] - 2, posVector[1], 1);
              EmitVertex;
              Attribute3f(attrPosition,posVector[0] - 2, posVector[1], 1);
              EmitVertex;
              Attribute3f(attrPosition,posVector[0], posVector[1] - 2, 1);
              EmitVertex;
              Attribute3f(attrPosition,posVector[0] + 2, posVector[1], 1);
              EmitVertex;
              EndPrimitives;
              EndObject(ARci);
            end;
            ARci.GLStates.DepthFunc := cfLess;

            if usedOcclusionQuery then
              FOcclusionQuery.EndQuery
            else
            begin
              glDisable(GL_OCCLUSION_TEST_HP);
              glGetBooleanv(GL_OCCLUSION_TEST_RESULT_HP, @FFlareIsNotOccluded)
            end;

            ARci.GLStates.SetGLColorWriting(True);
            ARci.GLStates.DepthWriteMask := True;
          end
          else
          begin
            //Compares the distance to the lensflare, to the z-buffer depth.
            //This prevents the flare from being occluded by objects BEHIND the light.
            depth :=
              TGLSceneBuffer(ARci.buffer).PixelToDistance(Round(ScreenPos[0]),
              Round(ARci.viewPortSize.cy - ScreenPos[1]));
            dist := VectorDistance(ARci.cameraPosition, self.AbsolutePosition);
            FlareIsNotOccluded := ((dist - depth) < 1);
          end;
        end;

        if FCurrSize > 0 then
        begin
          SetupRenderingOptions(ARci);
          M := CreateScaleMatrix(AffineVectorMake(FCurrSize, FCurrSize, 1));
          M := MatrixMultiply(M, CreateTranslationMatrix(posVector));
          M := MatrixMultiply(M, projMatrix);
          GradientProgram.UniformMatrix4fv['ProjectionMatrix'] := M;

          // Glow (a circle with transparent edges):
          if feGlow in Elements then
            StaticVBOManager.RenderClient(FBuiltPropertiesGlow, ARci);

          // Streaks
          if feStreaks in Elements then
          begin
            ARci.GLStates.Enable(stLineSmooth);
            if GL_VERSION_3_2 then
              lw := StreakWidth
            else
              lw := StreakWidth*10;
            ARci.GLStates.LineWidth := lw;
            StaticVBOManager.RenderClient(FBuiltPropertiesStreaks, ARci);
          end;

          // Rays (random-length lines from the origin):
          if feRays in Elements then
          begin
            RaysProgram.UseProgramObject;
            RaysProgram.UniformMatrix4fv['ProjectionMatrix'] := M;
            ARci.GLStates.Enable(stTexture2D);
            with FRaysTexture do
              RaysProgram.UniformTextureHandle[uniformTexUnit0.Name, 0,
                Image.NativeTextureTarget] := Handle;
            StaticVBOManager.RenderClient(FBuiltPropertiesRays, ARci);
          end;

          if feRing in Elements then
          begin
            GradientProgram.UseProgramObject;
            StaticVBOManager.RenderClient(FBuiltPropertiesRing, ARci);
          end;
          // Other secondaries (plain gradiented circles, like the glow):
          if feSecondaries in Elements then
          begin
            GradientProgram.UseProgramObject;
            for i := 1 to NumSecs do
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
              rnd := (Random + 0.1) * FCurrSize * 0.25;
              M := CreateScaleMatrix(AffineVectorMake(rnd, rnd, 1));
              M := MatrixMultiply(M, CreateTranslationMatrix(v));
              M := MatrixMultiply(M, projMatrix);
              GradientProgram.UniformMatrix4fv['ProjectionMatrix'] := M;
              if i mod 3 = 0 then
                StaticVBOManager.RenderClient(FBuiltPropertiesGlow, ARci)
              else
                StaticVBOManager.RenderClient(FBuiltPropertiesSecondaries, ARci);
            end;
          end;
          ARci.GLStates.SetGLCurrentProgram(0);
        end;
      end;
      RandSeed := oldSeed;
    end;
  end;

  // Render children
  if Count > 0 then
    Self.RenderChildren(0, Count - 1, ARci);
end;

// BuildList
//

procedure TGL3xLensFlare.BuildList(var rci: TRenderContextInfo);
var
  i: Integer;
  a, f, s, c: Single;
  rW, s0, c0, angle: Single;
begin
  GradientProgram.UseProgramObject;
  with StaticVBOManager do
  begin
    // Build glow
    BeginObject(FBuiltPropertiesGlow);
    Attribute3f(attrPosition, 0, 0, 0);
    Attribute4f(attrVertexColor, 0, 0, 0, 0);
    BeginPrimitives(GLVBOM_TRIANGLE_FAN);
    Attribute4f(attrVertexColor, GlowGradient.FromColor.Color);
    EmitVertex;
    Attribute4f(attrVertexColor, GlowGradient.ToColor.Color);
    angle := 0;
    for i := 0 to Resolution do
    begin
      SinCos(angle, s, c);
      Attribute3f(attrPosition, c, Squeeze * s, 0);
      EmitVertex;
      angle := angle + 2*Pi/Resolution;
    end;
    EndPrimitives;
    EndObject(rci);
    // Build streaks
    a := c2PI / NumStreaks;
    f := 1.5;
    BeginObject(FBuiltPropertiesStreaks);
    Attribute3f(attrPosition, 0, 0, 0);
    Attribute4f(attrVertexColor, 0, 0, 0, 0);
    BeginPrimitives(GLVBOM_LINES);
    for i := 0 to NumStreaks - 1 do
    begin
      SinCos(StreakAngle * cPIdiv180 + a * i, f, s, c);
      Attribute4f(attrVertexColor, StreaksGradient.FromColor.Color);
      Attribute3f(attrPosition, 0, 0, 0);
      EmitVertex;
      Attribute4f(attrVertexColor, StreaksGradient.ToColor.Color);
      Attribute3f(attrPosition, c, Squeeze * s, 0);
      EmitVertex;
    end;
    EndPrimitives;
    EndObject(rci);
    // Build ring
    rW := 1 / 15; // Ring width
    BeginObject(FBuiltPropertiesRing);
    Attribute3f(attrPosition, 0, 0, 0);
    Attribute4f(attrVertexColor, 0, 0, 0, 0);
    BeginPrimitives(GLVBOM_TRIANGLES);
    s0 := 0;
    c0 := 0.6;
    angle := 0;
    for i := 0 to Resolution do
    begin
      s := s0;
      c := c0;
      SinCos(angle, s0, c0);
      s0 := s0 * 0.6 * Squeeze;
      c0 := c0 * 0.6;
      Attribute4f(attrVertexColor, GlowGradient.ToColor.Color);
      Attribute3f(attrPosition, (1 - rW) * c, (1 - rW) * s, 0);
      EmitVertex;
      Attribute4f(attrVertexColor, RingGradient.FromColor.Color);
      Attribute3f(attrPosition, c, Squeeze * s, 0);
      EmitVertex;
      Attribute3f(attrPosition, c0, s0, 0);
      EmitVertex;
      Attribute3f(attrPosition, c0, s0, 0);
      EmitVertex;
      Attribute4f(attrVertexColor, GlowGradient.ToColor.Color);
      Attribute3f(attrPosition, (1 - rW) * c0, (1 - rW) * s0, 0);
      EmitVertex;
      Attribute4f(attrVertexColor, GlowGradient.ToColor.Color);
      Attribute3f(attrPosition, (1 - rW) * c, (1 - rW) * s, 0);
      EmitVertex;
      Attribute4f(attrVertexColor, RingGradient.FromColor.Color);
      Attribute3f(attrPosition, c, s, 0);
      EmitVertex;
      Attribute3f(attrPosition, c0, s0, 0);
      EmitVertex;
      Attribute4f(attrVertexColor, GlowGradient.ToColor.Color);
      Attribute3f(attrPosition, (1 + rW) * c0, (1 + rW) * s0, 0);
      EmitVertex;

      Attribute3f(attrPosition, (1 + rW) * c0, (1 + rW) * s0, 0);
      EmitVertex;
      Attribute3f(attrPosition, (1 + rW) * c, (1 + rW) * s, 0);
      EmitVertex;
      Attribute4f(attrVertexColor, RingGradient.FromColor.Color);
      Attribute3f(attrPosition, c, s, 0);
      EmitVertex;

      angle := angle + 2*Pi / Resolution;
    end;
    EndPrimitives;
    EndObject(rci);
    // Build secondaries
    BeginObject(FBuiltPropertiesSecondaries);
    Attribute3f(attrPosition, 0, 0, 0);
    Attribute4f(attrVertexColor, 0, 0, 0, 0);
    BeginPrimitives(GLVBOM_TRIANGLE_FAN);
    Attribute4f(attrVertexColor, SecondariesGradient.FromColor.Color);
    EmitVertex;
    Attribute4f(attrVertexColor, SecondariesGradient.ToColor.Color);
    angle := 0;
    for i := 0 to Resolution do
    begin
      SinCos(angle, s, c);
      Attribute3f(attrPosition, c, Squeeze * s, 0);
      EmitVertex;
      angle := angle + 2*Pi/Resolution;
    end;
    EndPrimitives;
    EndObject(rci);
    RaysProgram.UseProgramObject;
    // Build rays
    BeginObject(FBuiltPropertiesRays);
    Attribute3f(attrPosition, 0, 0, 0);
    Attribute2f(attrTexCoord0, 0, 0);
    BeginPrimitives(GLVBOM_TRIANGLES);
    Attribute3f(attrPosition, -1, -1, 0);
    EmitVertex;
    Attribute2f(attrTexCoord0, 1, 0);
    Attribute3f(attrPosition, 1, -1, 0);
    EmitVertex;
    Attribute2f(attrTexCoord0, 1, 1);
    Attribute3f(attrPosition, 1, 1, 0);
    EmitVertex;
    EmitVertex;
    Attribute2f(attrTexCoord0, 0, 1);
    Attribute3f(attrPosition, -1, 1, 0);
    EmitVertex;
    Attribute2f(attrTexCoord0, 0, 0);
    Attribute3f(attrPosition, -1, -1, 0);
    EmitVertex;
    EndPrimitives;
    EndObject(rci);
  end;
  ObjectStyle := ObjectStyle - [osBuiltStage];
end;

// DoProgress
//

procedure TGL3xLensFlare.DoProgress(const progressTime: TProgressTimes);
begin
  inherited;
  FDeltaTime := progressTime.deltaTime;
end;

// PreRender
//

procedure TGL3xLensFlare.PrepareRayTexture(var rci: TRenderContextInfo);
var
  FrameBuffer: TGLFrameBuffer;
  M: TMatrix;
  i: Integer;
  rnd, alpha, s, c: Single;
begin
  FrameBuffer := TGLFrameBuffer.Create;
  FrameBuffer.Bind;
  FrameBuffer.AttachTexture(0, FRaysTexture);
  Assert(FrameBuffer.Status = fsComplete, 'Framebuffer not complete');
  M := CreateTranslationMatrix(
    AffineVectorMake(FSize, FSize, 0));
  M := MatrixMultiply(M, CreateProjectionMatrix(0, 2*FSize, 0, 2*FSize));
  GradientProgram.UniformMatrix4fv['ProjectionMatrix'] := M;
  glViewport(0, 0, 2*FSize, 2*FSize);
  with rci.GLStates do
  begin
    Enable(stBlend);
    Disable(stDepthTest);
    DepthWriteMask := False;
    SetBlendFunc(bfSrcAlpha, bfOne);
    LineWidth := 1;
    Disable(stLineSmooth);
  end;
  glClearColor(0, 0, 0, 0);
  glClear(GL_COLOR_BUFFER_BIT);
  with DynamicVBOManager do
  begin
    BeginObject(FBuiltPropertiesRays);
    Attribute3f(attrPosition, 0, 0, 0);
    Attribute4f(attrVertexColor, 0, 0, 0, 0);
    BeginPrimitives(GLVBOM_LINES);
    alpha := 0;
    for i := 0 to Resolution * 20 - 1 do
    begin
      if (i and 1) <> 0 then
        rnd := 1.5 * Random * FSize
      else
        rnd := Random * FSize;
      Attribute4f(attrVertexColor, RaysGradient.FromColor.Color);
      Attribute3f(attrPosition, 0, 0, 0);
      EmitVertex;
      Attribute4f(attrVertexColor, RaysGradient.ToColor.Color);
      SinCos(alpha, s, c);
      Attribute3f(attrPosition,rnd * c, rnd * s * Squeeze, 0);
      EmitVertex;
      alpha := alpha + 2*Pi/(20*Resolution);
    end;
    EndPrimitives;
    EndObject(rci);
  end;
  FrameBuffer.Unbind;
  FrameBuffer.Free;
  with rci.viewPortSize do
    glViewport(0, 0, cx, cy);
  CheckOpenGLError;
end;

// SetGlowGradient
//

procedure TGL3xLensFlare.SetGlowGradient(const val: TGLFlareGradient);
begin
  FGlowGradient.Assign(val);
  StructureChanged;
end;

// SetRingGradient
//

procedure TGL3xLensFlare.SetRingGradient(const val: TGLFlareGradient);
begin
  FRingGradient.Assign(val);
  StructureChanged;
end;

// SetStreaksGradient
//

procedure TGL3xLensFlare.SetStreaksGradient(const val: TGLFlareGradient);
begin
  FStreaksGradient.Assign(val);
  StructureChanged;
end;

// SetRaysGradient
//

procedure TGL3xLensFlare.SetRaysGradient(const val: TGLFlareGradient);
begin
  FRaysGradient.Assign(val);
  StructureChanged;
end;

// SetSecondariesGradient
//

procedure TGL3xLensFlare.SetSecondariesGradient(const val: TGLFlareGradient);
begin
  FSecondariesGradient.Assign(val);
  StructureChanged;
end;

// SetSize
//

procedure TGL3xLensFlare.SetSize(aValue: Integer);
begin
  if aValue < 16 then
    aValue := 16
  else if aValue > 512 then
    aValue := 512;
  if aValue <> FSize then
  begin
    FSize := aValue;
    FRaysTexture.DestroyHandles;
    TGLBlankImage(FRaysTexture.Image).Width := 2*FSize;
    TGLBlankImage(FRaysTexture.Image).Height := 2*FSize;
    StructureChanged;
  end;
end;

// SetSeed
//

procedure TGL3xLensFlare.SetSeed(aValue: Integer);
begin
  FSeed := aValue;
  StructureChanged;
end;

// SetSqueeze
//

procedure TGL3xLensFlare.SetSqueeze(aValue: Single);
begin
  FSqueeze := aValue;
  StructureChanged;
end;

// StoreSqueeze
//

function TGL3xLensFlare.StoreSqueeze: Boolean;
begin
  Result := (FSqueeze <> 1);
end;

// SetNumStreaks
//

procedure TGL3xLensFlare.SetNumStreaks(aValue: Integer);
begin
  FNumStreaks := aValue;
  StructureChanged;
end;

// SetStreakWidth
//

procedure TGL3xLensFlare.SetStreakWidth(aValue: Single);
begin
  if aValue > 1 then
    aValue := 1;
  if aValue <= 0 then
    aValue := EPSILON2;
  FStreakWidth := aValue;
  StructureChanged;
end;

// StoreStreakWidth
//

function TGL3xLensFlare.StoreStreakWidth: Boolean;
begin
  Result := (FStreakWidth <> 1);
end;

// SetStreakAngle
//

procedure TGL3xLensFlare.SetStreakAngle(aValue: Single);
begin
  FStreakAngle := aValue;
  StructureChanged;
end;

// SetNumSecs
//

procedure TGL3xLensFlare.SetNumSecs(aValue: Integer);
begin
  FNumSecs := aValue;
  StructureChanged;
end;

// SetResolution
//

procedure TGL3xLensFlare.SetResolution(aValue: Integer);
begin
  if FResolution <> aValue then
  begin
    FResolution := aValue;
    StructureChanged;
  end;
end;

// SetAutoZTest
//

procedure TGL3xLensFlare.SetAutoZTest(aValue: Boolean);
begin
  if FAutoZTest <> aValue then
  begin
    FAutoZTest := aValue;
    StructureChanged;
  end;
end;

// SetElements
//

procedure TGL3xLensFlare.SetElements(aValue: TFlareElements);
begin
  if FElements <> aValue then
  begin
    FElements := aValue;
    StructureChanged;
  end;
end;

// SetDynamic
//

procedure TGL3xLensFlare.SetDynamic(aValue: Boolean);
begin
  if aValue <> FDynamic then
  begin
    FDynamic := aValue;
    NotifyChange(Self);
  end;
end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization
  // ------------------------------------------------------------------
  // ------------------------------------------------------------------
  // ------------------------------------------------------------------

  RegisterClasses([TGL3xLensFlare]);
  GradientProgram := TGLProgramHandle.Create;
  RaysProgram := TGLProgramHandle.Create;

finalization

  GradientProgram.Destroy;
  GradientProgram := nil;
  RaysProgram.Destroy;
  RaysProgram := nil;

end.

