//
// This unit is part of the GLScene Project   
//
{: VKS.State<p>

   Tools for managing an application-side cache of OpenGL state.<p>

 <b>History : </b><font size=-1><ul>
      <li>10/11/12 - PW - Added CPP compatibility: inserted GetDepthRangeFar/Near on access to properties
      <li>12/05/11 - Yar - Bugfixed issue with glColor cache miss (it must be direct state due to it indeterminacy in many cases)
      <li>07/05/11 - Yar - Bugfixed stColorMaterial action inside display list
      <li>16/03/11 - Yar - Fixes after emergence of VKS.MaterialEx
      <li>16/12/10 - Yar - Added uniform and transform feedback buffers indexed binding cache
      <li>14/12/10 - DaStr - Added to TVKStateCache:
                               Color property
                               SetGLMaterialColorsNoLighting()
                               SetGLMaterialDiffuseColor()
                             Bugfixed: TVKStateCache.SetGLMaterialAlphaChannel()
      <li>04/11/10 - DaStr - Restored Delphi5 and Delphi6 compatibility
      <li>03/11/10 - Yar - Added LightSpotDirection, LightSpotExponent
      <li>27/10/10 - Yar - Bugfixed default OpenGL state for LightDiffuse[N>0]
      <li>09/10/10 - Yar - Added properties SamplerBinding, MaxTextureImageUnit, MaxTextureAnisotropy
                           SetGLTextureMatrix work for ActiveTexture (in four count)
      <li>23/08/10 - Yar - Done replacing OpenGL1x functions to OpenGLAdapter
      <li>03/08/10 - DaStr - Restored deprecated SetGLFrontFaceCW() function
      <li>16/06/10 - YP  - Out of range fix, increasing FListStates by 2 must be done in a while loop
      <li>16/06/10 - YP  - Out of range fix, MAX_HARDWARE_LIGHT can be up to 16
      <li>31/05/10 - Yar - Replaced OpenGL1x functions to OpenGLAdapter, not complete yet.
      <li>01/05/10 - Yar - Added cashing to Vertex Array Object
      <li>22/04/10 - Yar - VKS.State revision
      <li>11/04/10 - Yar - Added NewList, EndList, InsideList
      <li>08/05/10 - DanB - Added TVKStateCache.SetColorMask
      <li>05/03/10 - DanB - Added initial functions/properties for caching all
                            OpenGL 3.2 state, not complete yet.
      <li>22/02/10 - DanB - added SetGLCurrentProgram
      <li>22/02/10 - Yar - Added more control of states
      <li>13/05/07 - fig - Added stTexture3D (GL_TEXTURE_3D)
      <li>19/12/06 - DaStr - GetGLCurrentTexture, ResetGLTexture added to TVKStateCache
      <li>04/10/04 - NC - Added stTextureRect (GL_TEXTURE_RECTANGLE_NV)
      <li>07/01/04 - EG - Introduced TVKStateCache
      <li>05/09/03 - EG - Creation from GLMisc split
   </ul></font>
}

// TODO: Proper client-side pushing + popping of state, in OpenGL 3+ contexts,
//       rather than using glPushAttrib + glPopAttrib.
// TODO: Proper support for textures, taking into account that they probably
//       won't be linked to texture units in some future version of OpenGL.
// TODO: Once more of GLScene is cache-aware, enable some of the checks before
//       changing OpenGL state (where we will gain a speed increase).
// DONE: Cache some relevant legacy state
// TODO: improve binding objects to binding points
// TODO: decide how to implement the new Enable* options (without going above
//       32 elements in sets if possible, which would be slower in 32bit Delphi)
// DONE: remove stTexture1D, 2D, etc from TVKState if possible, since they are
//       per texture-unit + also deprecated in OpenGL 3+

unit VKS.State;

interface

{$I VKScene.inc}
{.$DEFINE GLS_CACHE_MISS_CHECK}

uses
  System.Classes, System.SysUtils,
   
  VKS.CrossPlatform,
  VKS.VectorTypes,
  VKS.VectorGeometry,
  VKS.OpenGLTokens,
  VKS.TextureFormat;

const
  GLS_VERTEX_ATTR_NUM = 16;

type

  TVKStateType = (sttCurrent, sttPoint, sttLine, sttPolygon, sttPolygonStipple,
    sttPixelMode, sttLighting, sttFog, sttDepthBuffer, sttAccumBuffer,
    sttStencilBuffer, sttViewport, sttTransform, sttEnable, sttColorBuffer,
    sttHint, sttEval, sttList, sttTexture, sttScissor,
    sttMultisample);
  TVKStateTypes = set of TVKStateType;

const
  cAllAttribBits = [low(TVKStateType)..High(TVKStateType)];

type

  TVKMeshPrimitive = (
    mpNOPRIMITIVE,
    mpTRIANGLES,
    mpTRIANGLE_STRIP,
    mpTRIANGLE_FAN,
    mpPOINTS,
    mpLINES,
    mpLINE_LOOP,
    mpLINE_STRIP,
    mpLINES_ADJACENCY,
    mpLINE_STRIP_ADJACENCY,
    mpTRIANGLES_ADJACENCY,
    mpTRIANGLE_STRIP_ADJACENCY,
    mpPATCHES
    );

  TVKMeshPrimitives = set of TVKMeshPrimitive;

const
  cAllMeshPrimitive = [
    mpTRIANGLES,
    mpTRIANGLE_STRIP,
    mpTRIANGLE_FAN,
    mpPOINTS,
    mpLINES,
    mpLINE_LOOP,
    mpLINE_STRIP,
    mpLINES_ADJACENCY,
    mpLINE_STRIP_ADJACENCY,
    mpTRIANGLES_ADJACENCY,
    mpTRIANGLE_STRIP_ADJACENCY,
    mpPATCHES];

type

  // TVKState
  //
//: Reflects all relevant (binary) states of OpenGL subsystem
  TVKState = (stAlphaTest, stAutoNormal,
    stBlend, stColorMaterial, stCullFace, stDepthTest, stDither,
    stFog, stLighting, stLineSmooth, stLineStipple,
    stIndexLogicOp, stColorLogicOp, stNormalize, stPointSmooth, stPointSprite,
    stPolygonSmooth, stPolygonStipple, stScissorTest, stStencilTest,
    stPolygonOffsetPoint, stPolygonOffsetLine, stPolygonOffsetFill,
    stDepthClamp);

  TVKStates = set of TVKState;

  TComparisonFunction = (cfNever, cfAlways, cfLess, cfLEqual, cfEqual,
    cfGreater, cfNotEqual, cfGEqual);
  TStencilFunction = TComparisonFunction;
  TDepthFunction = TComparisonFunction;

  TBlendFunction = (bfZero, bfOne,
    bfSrcColor, bfOneMinusSrcColor, bfDstColor, bfOneMinusDstColor,
    bfSrcAlpha, bfOneMinusSrcAlpha, bfDstAlpha, bfOneMinusDstAlpha,
    bfConstantColor, bfOneMinusConstantColor,
    bfConstantAlpha, bfOneMinusConstantAlpha,
    bfSrcAlphaSat);

  TDstBlendFunction = bfZero..bfOneMinusConstantAlpha;

  TBlendEquation = (beAdd, beSubtract, beReverseSubtract, beMin, beMax);

  TStencilOp = (soKeep, soZero, soReplace, soIncr, soDecr, soInvert, soIncrWrap,
    soDecrWrap);

  TLogicOp = (loClear, loAnd, loAndReverse, loCopy, loAndInverted, loNoOp,
    loXOr, loOr, loNor, loEquiv, loInvert, loOrReverse, loCopyInverted,
    loOrInverted, loNAnd, loSet);

  TQueryType = (
    qrySamplesPassed,
    qryPrimitivesGenerated,
    qryTransformFeedbackPrimitivesWritten,
    qryTimeElapsed,
    qryAnySamplesPassed);

  // TFaceWinding
  //
//: Describe what kind of winding has a front face
  TFaceWinding = (fwCounterClockWise, fwClockWise);

  TPolygonMode = (pmFill, pmLines, pmPoints);

  TCullFaceMode = (cmFront, cmBack, cmFrontAndBack);
  //  TSingleCullFaceMode = cmFront..cmBack;

  TColorComponent = (ccRed, ccGreen, ccBlue, ccAlpha);
  TColorMask = set of TColorComponent;

const
  cAllColorComponents = [ccRed, ccGreen, ccBlue, ccAlpha];
  MAX_HARDWARE_LIGHT = 16;
  MAX_SHADER_LIGHT = 8;
  MAX_HARDWARE_TEXTURE_UNIT = 48;
  MAX_HARDWARE_UNIFORM_BUFFER_BINDING = 75;

type

  THintType = (hintDontCare, hintFastest, hintNicest);

  TLightSourceState = packed record
    Position: array[0..MAX_HARDWARE_LIGHT-1] of TVector;
    Ambient: array[0..MAX_HARDWARE_LIGHT-1] of TVector;
    Diffuse: array[0..MAX_HARDWARE_LIGHT-1] of TVector;
    Specular: array[0..MAX_HARDWARE_LIGHT-1] of TVector;
    SpotDirection: array[0..MAX_HARDWARE_LIGHT-1] of TVector;
    SpotCosCutoffExponent: array[0..MAX_HARDWARE_LIGHT-1] of TVector;
    Attenuation: array[0..MAX_HARDWARE_LIGHT-1] of TVector;
  end;

  TShaderLightSourceState = packed record
    Position: array[0..MAX_SHADER_LIGHT-1] of TVector;
    Ambient: array[0..MAX_SHADER_LIGHT-1] of TVector;
    Diffuse: array[0..MAX_SHADER_LIGHT-1] of TVector;
    Specular: array[0..MAX_SHADER_LIGHT-1] of TVector;
    SpotDirection: array[0..MAX_SHADER_LIGHT-1] of TVector;
    SpotCosCutoffExponent: array[0..MAX_SHADER_LIGHT-1] of TVector;
    Attenuation: array[0..MAX_SHADER_LIGHT-1] of TVector;
  end;

  TOnLightsChanged = procedure(Sender: TObject);

  TVKBufferBindingTarget = (bbtUniform, bbtTransformFeedBack);

  TUBOStates = record
    FUniformBufferBinding: TVKuint;
    FOffset: TVKintptr;
    FSize: TVKsizeiptr;
  end;

  TVKMaterialLevel = (mlAuto, mlFixedFunction, mlMultitexturing, mlSM3, mlSM4, mlSM5);

  // TVKStateCache
  //
  {: Manages an application-side cache of OpenGL states and parameters.<p>
     Purpose of this class is to eliminate redundant state and parameter
     changes, and there will typically be no more than one state cache per
     OpenGL context. }
  TVKStateCache = class
  private
    { Private Declarations }
    // Legacy state
    FFrontBackColors: array[0..1, 0..3] of TVector;
    FFrontBackShininess: array[0..1] of Integer;
    FAlphaFunc: TComparisonFunction;
    FAlphaRef: TVKclampf;
    FPolygonBackMode: TPolygonMode; // Front + back have same polygon mode

    // Lighting state
    FMaxLights: GLuint;
    FLightEnabling: array[0..MAX_HARDWARE_LIGHT - 1] of Boolean;
    FLightIndices: array[0..MAX_HARDWARE_LIGHT - 1] of TVKint;
    FLightNumber: Integer;
    FLightStates: TLightSourceState;
    FSpotCutoff: array[0..MAX_HARDWARE_LIGHT-1] of Single;
    FShaderLightStates: TShaderLightSourceState;
    FShaderLightStatesChanged: Boolean;

    FColorWriting: Boolean; // TODO: change to per draw buffer (FColorWriteMask)
    FStates: TVKStates;
    FListStates: array of TVKStateTypes;
    FCurrentList: TVKuint;
    FTextureMatrixIsIdentity: array[0..3] of Boolean;
    FForwardContext: Boolean;
    FFFPLight: Boolean;

    // Vertex Array Data state
    FVertexArrayBinding: TVKuint;
    FArrayBufferBinding: TVKuint;
    FElementBufferBinding: TVKuint;
    FTextureBufferBinding: TVKuint;
    FEnablePrimitiveRestart: TVKboolean;
    FPrimitiveRestartIndex: TVKuint;

    // Transformation state
    FViewPort: TVector4i;
    FDepthRange: array[0..1] of TVKclampd;
    FEnableClipDistance: array[0..7] of TVKboolean;
    FEnableDepthClamp: TVKboolean;

    // Coloring state
    FClampReadColor: TVKenum; // GL_FIXED_ONLY
    FProvokingVertex: TVKenum; // GL_LAST_VERTEX_CONVENTION

    // Rasterization state
    FPointSize: TVKfloat;
    FPointFadeThresholdSize: TVKfloat;
    FPointSpriteCoordOrigin: TVKenum; // GL_UPPER_LEFT
    FLineWidth: Single;
    FLineStippleFactor: TVKint;
    FLineStipplePattern: TVKushort;

    FEnableLineSmooth: TVKboolean;
    FEnableCullFace: TVKboolean;
    FCullFaceMode: TCullFaceMode;
    FFrontFace: TFaceWinding;
    FEnablePolygonSmooth: TVKboolean;
    FPolygonMode: TPolygonMode;
    FPolygonOffsetFactor: TVKfloat;
    FPolygonOffsetUnits: TVKfloat;
    FEnablePolygonOffsetPoint: TVKboolean;
    FEnablePolygonOffsetLine: TVKboolean;
    FEnablePolygonOffsetFill: TVKboolean;

    // Multisample state
    FEnableMultisample: TVKboolean;
    FEnableSampleAlphaToCoverage: TVKboolean;
    FEnableSampleAlphaToOne: TVKboolean;
    FEnableSampleCoverage: TVKboolean;
    FSampleCoverageValue: TVKfloat;
    FSampleCoverageInvert: TVKboolean;
    FEnableSampleMask: TVKboolean;
    FSampleMaskValue: array[0..15] of TVKbitfield;

    // Texture state
    FMaxTextureSize: TVKuint;
    FMax3DTextureSize: TVKuint;
    FMaxCubeTextureSize: TVKuint;
    FMaxArrayTextureSize: TVKuint;
    FMaxTextureImageUnits: TVKuint;
    FMaxTextureAnisotropy: TVKuint;
    FMaxSamples: TVKuint;
    FTextureBinding: array[0..MAX_HARDWARE_TEXTURE_UNIT - 1, TVKTextureTarget] of TVKuint;
    FTextureBindingTime: array[0..MAX_HARDWARE_TEXTURE_UNIT - 1, TVKTextureTarget] of Double;
    FSamplerBinding: array[0..MAX_HARDWARE_TEXTURE_UNIT - 1] of TVKuint;

    // Active texture state
    FActiveTexture: TVKint; // 0 .. Max_texture_units
    FActiveTextureEnabling: array[0..MAX_HARDWARE_TEXTURE_UNIT - 1, TVKTextureTarget] of Boolean;

    // Pixel operation state
    FEnableScissorTest: TVKboolean;
    FScissorBox: TVector4i;

    FEnableStencilTest: TVKboolean;

    FStencilFunc: TStencilFunction;
    FStencilValueMask: TVKuint;
    FStencilRef: TVKint;
    FStencilFail: TStencilOp;
    FStencilPassDepthFail: TStencilOp;
    FStencilPassDepthPass: TStencilOp;

    FStencilBackFunc: TStencilFunction;
    FStencilBackValueMask: TVKuint;
    FStencilBackRef: TVKuint;
    FStencilBackFail: TStencilOp;
    FStencilBackPassDepthPass: TStencilOp;
    FStencilBackPassDepthFail: TStencilOp;

    FEnableDepthTest: TVKboolean;
    FDepthFunc: TDepthFunction;

    FEnableBlend: array[0..15] of TVKboolean;

    FBlendSrcRGB: TBlendFunction;
    FBlendSrcAlpha: TBlendFunction;
    FBlendDstRGB: TDstBlendFunction;
    FBlendDstAlpha: TDstBlendFunction;

    FBlendEquationRGB: TBlendEquation;
    FBlendEquationAlpha: TBlendEquation;
    FBlendColor: TVector;

    FEnableFramebufferSRGB: TVKboolean;
    FEnableDither: TVKboolean;
    FEnableColorLogicOp: TVKboolean;

    FLogicOpMode: TLogicOp;

    // Framebuffer control state
    FColorWriteMask: array[0..15] of TColorMask;
    FDepthWriteMask: TVKBoolean;
    FStencilWriteMask: TVKuint;
    FStencilBackWriteMask: TVKuint;
    FColorClearValue: TVector;
    FDepthClearValue: TVKfloat;
    FStencilClearValue: TVKuint;

    // Framebuffer state
    FDrawFrameBuffer: TVKuint;
    FReadFrameBuffer: TVKuint;

    // Renderbuffer state
    FRenderBuffer: TVKuint;

    // Pixels state
    FUnpackSwapBytes: TVKboolean;
    FUnpackLSBFirst: TVKboolean;
    FUnpackImageHeight: TVKuint;
    FUnpackSkipImages: TVKuint;
    FUnpackRowLength: TVKuint;
    FUnpackSkipRows: TVKuint;
    FUnpackSkipPixels: TVKuint;
    FUnpackAlignment: TVKuint;
    FPackSwapBytes: TVKboolean;
    FPackLSBFirst: TVKboolean;
    FPackImageHeight: TVKuint;
    FPackSkipImages: TVKuint;
    FPackRowLength: TVKuint;
    FPackSkipRows: TVKuint;
    FPackSkipPixels: TVKuint;
    FPackAlignment: TVKuint;

    FPixelPackBufferBinding: TVKuint;
    FPixelUnpackBufferBinding: TVKuint;

    // Program state
    FCurrentProgram: TVKuint;
    FMaxTextureUnits: TVKuint;
    FUniformBufferBinding: TVKuint;
    FUBOStates: array[TVKBufferBindingTarget, 0..MAX_HARDWARE_UNIFORM_BUFFER_BINDING-1] of TUBOStates;

    // Vector + Geometry Shader state
    FCurrentVertexAttrib: array[0..15] of TVector;
    FEnableProgramPointSize: TVKboolean;

    // Transform Feedback state
    FTransformFeedbackBufferBinding: TVKuint;

    // Hints state
    FTextureCompressionHint: THintType;
    FPolygonSmoothHint: THintType;
    FFragmentShaderDerivitiveHint: THintType;
    FLineSmoothHint: THintType;
    FMultisampleFilterHint: THintType;

    // Misc state
    FCurrentQuery: array[TQueryType] of TVKuint;
    FCopyReadBufferBinding: TVKuint;
    FCopyWriteBufferBinding: TVKuint;
    FEnableTextureCubeMapSeamless: TVKboolean;
    FInsideList: Boolean;

    FOnLightsChanged: TOnLightsChanged;
  protected
    { Protected Declarations }
    // Vertex Array Data state
    procedure SetVertexArrayBinding(const Value: TVKuint);
    function GetArrayBufferBinding: TVKuint;
    procedure SetArrayBufferBinding(const Value: TVKuint);
    function GetElementBufferBinding: TVKuint;
    procedure SetElementBufferBinding(const Value: TVKuint);
    function GetEnablePrimitiveRestart: TVKboolean;
    function GetPrimitiveRestartIndex: TVKuint;
    procedure SetEnablePrimitiveRestart(const enabled: TVKboolean);
    procedure SetPrimitiveRestartIndex(const index: TVKuint);
    procedure SetTextureBufferBinding(const Value: TVKuint);
    // Transformation state
    procedure SetViewPort(const Value: TVector4i);
    function GetEnableClipDistance(ClipDistance: Cardinal): TVKboolean;
    procedure SetEnableClipDistance(Index: Cardinal; const Value: TVKboolean);
    function GetDepthRangeFar:TVKclampd;
    procedure SetDepthRangeFar(const Value: TVKclampd);
    function GetDepthRangeNear:TVKclampd;
    procedure SetDepthRangeNear(const Value: TVKclampd);
    procedure SetEnableDepthClamp(const enabled: TVKboolean);
    // Coloring state
    procedure SetClampReadColor(const Value: TVKenum);
    procedure SetProvokingVertex(const Value: TVKenum);
    // Rasterization state
    procedure SetPointSize(const Value: TVKfloat);
    procedure SetPointFadeThresholdSize(const Value: TVKfloat);
    procedure SetPointSpriteCoordOrigin(const Value: TVKenum);
    procedure SetLineWidth(const Value: TVKfloat);
    procedure SetLineStippleFactor(const Value: TVKint);
    procedure SetLineStipplePattern(const Value: TVKushort);

    procedure SetEnableLineSmooth(const Value: TVKboolean);
    procedure SetEnableCullFace(const Value: TVKboolean);
    procedure SetCullFaceMode(const Value: TCullFaceMode);
    procedure SetFrontFace(const Value: TFaceWinding);
    procedure SetEnablePolygonSmooth(const Value: TVKboolean);
    procedure SetPolygonMode(const Value: TPolygonMode);
    procedure SetPolygonOffsetFactor(const Value: TVKfloat);
    procedure SetPolygonOffsetUnits(const Value: TVKfloat);
    procedure SetEnablePolygonOffsetPoint(const Value: TVKboolean);
    procedure SetEnablePolygonOffsetLine(const Value: TVKboolean);
    procedure SetEnablePolygonOffsetFill(const Value: TVKboolean);
    // Multisample state
    procedure SetEnableMultisample(const Value: TVKboolean);
    procedure SetEnableSampleAlphaToCoverage(const Value: TVKboolean);
    procedure SetEnableSampleAlphaToOne(const Value: TVKboolean);
    procedure SetEnableSampleCoverage(const Value: TVKboolean);
    procedure SetSampleCoverageValue(const Value: TVKfloat);
    procedure SetSampleCoverageInvert(const Value: TVKboolean);
    procedure SetEnableSampleMask(const Value: TVKboolean);
    function GetSampleMaskValue(Index: Integer): TVKbitfield;
    procedure SetSampleMaskValue(Index: Integer; const Value: TVKbitfield);
    // Texture state
    function GetMaxTextureSize: TVKuint;
    function GetMax3DTextureSize: TVKuint;
    function GetMaxCubeTextureSize: TVKuint;
    function GetMaxArrayTextureSize: TVKuint;
    function GetMaxTextureImageUnits: TVKuint;
    function GetMaxTextureAnisotropy: TVKuint;
    function GetMaxSamples: TVKuint;
    function GetTextureBinding(Index: Integer; target: TVKTextureTarget):
      TVKuint;
    function GetTextureBindingTime(Index: Integer; target: TVKTextureTarget):
      Double;
    procedure SetTextureBinding(Index: Integer; target: TVKTextureTarget;
      const Value: TVKuint);
    function GetActiveTextureEnabled(Target: TVKTextureTarget): Boolean;
    procedure SetActiveTextureEnabled(Target: TVKTextureTarget; const Value:
      Boolean);
    function GetSamplerBinding(Index: TVKuint): TVKuint;
    procedure SetSamplerBinding(Index: TVKuint; const Value: TVKuint);
    // Active texture
    procedure SetActiveTexture(const Value: TVKint);
    // Pixel operations
    procedure SetEnableScissorTest(const Value: TVKboolean);
    procedure SetScissorBox(const Value: TVector4i);
    procedure SetEnableStencilTest(const Value: TVKboolean);
    procedure SetEnableDepthTest(const Value: TVKboolean);
    procedure SetDepthFunc(const Value: TDepthFunction);
    function GetEnableBlend(Index: Integer): TVKboolean;
    procedure SetEnableBlend(Index: Integer; const Value: TVKboolean);
    procedure SetBlendColor(const Value: TVector);
    procedure SetEnableFramebufferSRGB(const Value: TVKboolean);
    procedure SetEnableDither(const Value: TVKboolean);
    procedure SetEnableColorLogicOp(const Value: TVKboolean);
    procedure SetLogicOpMode(const Value: TLogicOp);
    // Framebuffer control
    function GetColorWriteMask(Index: Integer): TColorMask;
    procedure SetColorWriteMask(Index: Integer; const Value: TColorMask);
    procedure SetDepthWriteMask(const Value: TVKboolean);
    procedure SetStencilWriteMask(const Value: TVKuint);
    procedure SetStencilBackWriteMask(const Value: TVKuint);
    procedure SetColorClearValue(const Value: TVector);
    procedure SetDepthClearValue(const Value: TVKfloat);
    procedure SetStencilClearValue(const Value: TVKuint);
    // Framebuffer
    procedure SetDrawFrameBuffer(const Value: TVKuint);
    procedure SetReadFrameBuffer(const Value: TVKuint);
    // Renderbuffer
    procedure SetRenderBuffer(const Value: TVKuint);
    // Pixels
    procedure SetUnpackSwapBytes(const Value: TVKboolean);
    procedure SetUnpackLSBFirst(const Value: TVKboolean);
    procedure SetUnpackImageHeight(const Value: TVKuint);
    procedure SetUnpackSkipImages(const Value: TVKuint);
    procedure SetUnpackRowLength(const Value: TVKuint);
    procedure SetUnpackSkipRows(const Value: TVKuint);
    procedure SetUnpackSkipPixels(const Value: TVKuint);
    procedure SetUnpackAlignment(const Value: TVKuint);
    procedure SetPackSwapBytes(const Value: TVKboolean);
    procedure SetPackLSBFirst(const Value: TVKboolean);
    procedure SetPackImageHeight(const Value: TVKuint);
    procedure SetPackSkipImages(const Value: TVKuint);
    procedure SetPackRowLength(const Value: TVKuint);
    procedure SetPackSkipRows(const Value: TVKuint);
    procedure SetPackSkipPixels(const Value: TVKuint);
    procedure SetPackAlignment(const Value: TVKuint);
    procedure SetPixelPackBufferBinding(const Value: TVKuint);
    procedure SetPixelUnpackBufferBinding(const Value: TVKuint);
    // Program
    procedure SetCurrentProgram(const Value: TVKuint);
    procedure SetUniformBufferBinding(const Value: TVKuint);
    function GetMaxTextureUnits: TVKuint;
    // Vector + Geometry Shader state
    function GetCurrentVertexAttrib(Index: Integer): TVector;
    procedure SetCurrentVertexAttrib(Index: Integer; const Value: TVector);
    procedure SetEnableProgramPointSize(const Value: TVKboolean);
    // Transform Feedback state
    procedure SetTransformFeedbackBufferBinding(const Value: TVKuint);
    // Hints
    procedure SetLineSmoothHint(const Value: THintType);
    procedure SetPolygonSmoothHint(const Value: THintType);
    procedure SetTextureCompressionHint(const Value: THintType);
    procedure SetFragmentShaderDerivitiveHint(const Value: THintType);
    procedure SetMultisampleFilterHint(const Value: THintType);
    // Misc
    function GetCurrentQuery(Index: TQueryType): TVKuint;
    //    procedure SetCurrentQuery(Index: TQueryType; const Value: TVKuint);
    procedure SetCopyReadBufferBinding(const Value: TVKuint);
    procedure SetCopyWriteBufferBinding(const Value: TVKuint);
    procedure SetEnableTextureCubeMapSeamless(const Value: TVKboolean);
    // Ligting
    procedure SetFFPLight(Value: Boolean);
    function GetMaxLights: Integer;
    function GetLightEnabling(I: Integer): Boolean;
    procedure SetLightEnabling(I: Integer; Value: Boolean);
    function GetLightPosition(I: Integer): TVector;
    procedure SetLightPosition(I: Integer; const Value: TVector);
    function GetLightSpotDirection(I: Integer): TAffineVector;
    procedure SetLightSpotDirection(I: Integer; const Value: TAffineVector);
    function GetLightAmbient(I: Integer): TVector;
    procedure SetLightAmbient(I: Integer; const Value: TVector);
    function GetLightDiffuse(I: Integer): TVector;
    procedure SetLightDiffuse(I: Integer; const Value: TVector);
    function GetLightSpecular(I: Integer): TVector;
    procedure SetLightSpecular(I: Integer; const Value: TVector);
    function GetSpotCutoff(I: Integer): Single;
    procedure SetSpotCutoff(I: Integer; const Value: Single);
    function GetSpotExponent(I: Integer): Single;
    procedure SetSpotExponent(I: Integer; const Value: Single);
    function GetConstantAtten(I: Integer): Single;
    procedure SetConstantAtten(I: Integer; const Value: Single);
    function GetLinearAtten(I: Integer): Single;
    procedure SetLinearAtten(I: Integer; const Value: Single);
    function GetQuadAtten(I: Integer): Single;
    procedure SetQuadAtten(I: Integer; const Value: Single);
    procedure SetForwardContext(Value: Boolean);

    function GetMaterialAmbient(const aFace: TCullFaceMode): TVector;
    function GetMaterialDiffuse(const aFace: TCullFaceMode): TVector;
    function GetMaterialSpecular(const aFace: TCullFaceMode): TVector;
    function GetMaterialEmission(const aFace: TCullFaceMode): TVector;
    function GetMaterialShininess(const aFace: TCullFaceMode): Integer;
  public
    { Public Declarations }
    constructor Create; virtual;
    destructor Destroy; override;

    procedure PushAttrib(stateTypes: TVKStateTypes);
    procedure PopAttrib();

    procedure Enable(const aState: TVKState);
    procedure Disable(const aState: TVKState);
    procedure PerformEnable(const aState: TVKState);
    procedure PerformDisable(const aState: TVKState);

    procedure SetGLState(const aState : TVKState); deprecated;
    procedure UnSetGLState(const aState : TVKState); deprecated;
    procedure ResetGLPolygonMode; deprecated;
    procedure ResetGLMaterialColors; deprecated;
    procedure ResetGLTexture(const TextureUnit: Integer); deprecated;
    procedure ResetGLCurrentTexture; deprecated;
    procedure ResetGLFrontFace; deprecated;
    procedure SetGLFrontFaceCW; deprecated;
    procedure ResetAll; deprecated;

    {: Adjusts material colors for a face. }
    procedure SetGLMaterialColors(const aFace: TCullFaceMode;
      const emission, ambient, diffuse, specular: TVector;
      const shininess: Integer);

    property MaterialAmbient[const aFace: TCullFaceMode]: TVector
      read GetMaterialAmbient;
    property MaterialDiffuse[const aFace: TCullFaceMode]: TVector
      read GetMaterialDiffuse;
    property MaterialSpecular[const aFace: TCullFaceMode]: TVector
      read GetMaterialSpecular;
    property MaterialEmission[const aFace: TCullFaceMode]: TVector
      read GetMaterialEmission;
    property MaterialShininess[const aFace: TCullFaceMode]: Integer
      read GetMaterialShininess;

    {: Adjusts material alpha channel for a face. }
    procedure SetGLMaterialAlphaChannel(const aFace: TVKEnum; const alpha: TVKFloat);

    {: Adjusts material diffuse color for a face. }
    procedure SetGLMaterialDiffuseColor(const aFace: TVKEnum; const diffuse: TVector);

    {: Lighting states }
    property FixedFunctionPipeLight: Boolean read FFFPLight write SetFFPLight;
    property MaxLights: Integer read GetMaxLights;
    property LightEnabling[Index: Integer]: Boolean read GetLightEnabling write
    SetLightEnabling;
    property LightPosition[Index: Integer]: TVector read GetLightPosition write
    SetLightPosition;
    property LightSpotDirection[Index: Integer]: TAffineVector read GetLightSpotDirection write
    SetLightSpotDirection;
    property LightAmbient[Index: Integer]: TVector read GetLightAmbient write
    SetLightAmbient;
    property LightDiffuse[Index: Integer]: TVector read GetLightDiffuse write
    SetLightDiffuse;
    property LightSpecular[Index: Integer]: TVector read GetLightSpecular write
    SetLightSpecular;
    property LightSpotCutoff[Index: Integer]: Single read GetSpotCutoff write
    SetSpotCutoff;
    property LightSpotExponent[Index: Integer]: Single read GetSpotExponent write
    SetSpotExponent;
    property LightConstantAtten[Index: Integer]: Single read GetConstantAtten
    write SetConstantAtten;
    property LightLinearAtten[Index: Integer]: Single read GetLinearAtten write
    SetLinearAtten;
    property LightQuadraticAtten[Index: Integer]: Single read GetQuadAtten write
    SetQuadAtten;
    function GetLightIndicesAsAddress: PGLInt;
    function GetLightStateAsAddress: Pointer;
    property LightNumber: Integer read FLightNumber;
    property OnLightsChanged: TOnLightsChanged read FOnLightsChanged write FOnLightsChanged;

    {: Blending states }
    procedure SetGLAlphaFunction(func: TComparisonFunction; ref: TVKclampf);

    // Vertex Array Data state
    {: The currently bound array buffer (calling glVertexAttribPointer
       locks this buffer to the currently bound VBO). }
    property VertexArrayBinding: TVKuint read FVertexArrayBinding write
      SetVertexArrayBinding;
    {: The currently bound vertex buffer object (VAO). }
    property ArrayBufferBinding: TVKuint read GetArrayBufferBinding write
      SetArrayBufferBinding;
    {: The currently bound element buffer object (EBO). }
    property ElementBufferBinding: TVKuint read GetElementBufferBinding write
      SetElementBufferBinding;
    {: Determines whether primitive restart is turned on or off. }
    property EnablePrimitiveRestart: TVKboolean read GetEnablePrimitiveRestart
      write SetEnablePrimitiveRestart;
    {: The index Value that causes a primitive restart. }
    property PrimitiveRestartIndex: TVKuint read GetPrimitiveRestartIndex write
      SetPrimitiveRestartIndex;
    {: The currently bound texture buffer object (TBO). }
    property TextureBufferBinding: TVKuint read FTextureBufferBinding write
      SetTextureBufferBinding;

    // Transformation state
    {: The viewport. }
    property ViewPort: TVector4i read FViewPort write SetViewPort;
    {: Modifies the near + far clipping planes. }
    procedure SetDepthRange(const ZNear, ZFar: TVKclampd);
    {: The near clipping plane distance. }
    property DepthRangeNear: TVKclampd read GetDepthRangeNear write
      SetDepthRangeNear;
    {: The far clipping plane distance. }
    property DepthRangeFar: TVKclampd read GetDepthRangeFar write
      SetDepthRangeFar;
    {: Enables/Disables each of the clip distances, used in shaders. }
    property EnableClipDistance[Index: Cardinal]: TVKboolean read
    GetEnableClipDistance write SetEnableClipDistance;
    {: Enables/Disables depth clamping. }
    property EnableDepthClamp: TVKboolean read FEnableDepthClamp write
      SetEnableDepthClamp;

    // Coloring state
    {: Controls read color clamping. }
    property ClampReadColor: TVKenum read FClampReadColor write
      SetClampReadColor;
    {: The provoking vertex used in flat shading.  All the vertices of each
       primitive will the same value determined by this property. }
    property ProvokingVertex: TVKenum read FProvokingVertex write
      SetProvokingVertex;

    // Rasterization state
    {: The default point size, used when EnableProgramPointSize = false. }
    property PointSize: TVKfloat read FPointSize write SetPointSize;
    {: If multisampling is enabled, this can control when points are faded out.}
    property PointFadeThresholdSize: TVKfloat read FPointFadeThresholdSize write
      SetPointFadeThresholdSize;
    {: The texture coordinate origin of point sprites. }
    property PointSpriteCoordOrigin: TVKenum read FPointSpriteCoordOrigin write
      SetPointSpriteCoordOrigin;
    {: The line width. }
    property LineWidth: TVKfloat read FLineWidth write SetLineWidth;
    {: The line stipple. }
    property LineStippleFactor: TVKint read FLineStippleFactor write
      SetLineStippleFactor;
    {: The line stipple. }
    property LineStipplePattern: TVKushort read FLineStipplePattern write
      SetLineStipplePattern;
    {: Enable/Disable line smoothing. }
    property EnableLineSmooth: TVKboolean read FEnableLineSmooth write
      SetEnableLineSmooth;
    {: Enable/Disable face culling. }
    property EnableCullFace: TVKboolean read FEnableCullFace write
      SetEnableCullFace;
    {: Selects which faces to cull: front, back or front+back.}
    property CullFaceMode: TCullFaceMode read FCullFaceMode write
      SetCullFaceMode;
    {: The winding direction that indicates a front facing primitive. }
    property FrontFace: {TVKenum} TFaceWinding read FFrontFace write
    SetFrontFace;
    // Enables/Disables polygon smoothing.
    property EnablePolygonSmooth: TVKboolean read FEnablePolygonSmooth write
      SetEnablePolygonSmooth;
    {: Whether polygons appear filled, lines or points. }
    property PolygonMode: TPolygonMode read FPolygonMode write SetPolygonMode;
    {: Scales the maximum depth of the polygon. }
    property PolygonOffsetFactor: TVKfloat read FPolygonOffsetFactor write
      SetPolygonOffsetFactor;
    {: Scales an implementation-dependent constant that relates to the usable
       resolution of the depth buffer. }
    property PolygonOffsetUnits: TVKfloat read FPolygonOffsetUnits write
      SetPolygonOffsetUnits;
    {: Set polygon offset. }
    procedure SetPolygonOffset(const factor, units: TVKfloat);
    {: Enable/Disable polygon offset for polygons in point mode. }
    property EnablePolygonOffsetPoint: TVKboolean read FEnablePolygonOffsetPoint
      write SetEnablePolygonOffsetPoint;
    {: Enable/Disable polygon offset for polygons in line mode. }
    property EnablePolygonOffsetLine: TVKboolean read FEnablePolygonOffsetLine
      write SetEnablePolygonOffsetLine;
    {: Enable/Disable polygon offset for polygons in fill mode. }
    property EnablePolygonOffsetFill: TVKboolean read FEnablePolygonOffsetFill
      write SetEnablePolygonOffsetFill;

    // Multisample state
    {: Enable/Disable multisampling. }
    property EnableMultisample: TVKboolean read FEnableMultisample write
      SetEnableMultisample;
    {: Enable/Disable sample alpha to coverage. }
    property EnableSampleAlphaToCoverage: TVKboolean read
      FEnableSampleAlphaToCoverage write SetEnableSampleAlphaToCoverage;
    {: Enable/Disable sample alpha to one. }
    property EnableSampleAlphaToOne: TVKboolean read FEnableSampleAlphaToOne
      write SetEnableSampleAlphaToOne;
    {: Enable/Disable sample coverage. }
    property EnableSampleCoverage: TVKboolean read FEnableSampleCoverage write
      SetEnableSampleCoverage;
    {: Sample coverage Value. }
    property SampleCoverageValue: TVKfloat read FSampleCoverageValue write
      SetSampleCoverageValue;
    {: Inverts sample coverage Value. }
    property SampleCoverageInvert: TVKboolean read FSampleCoverageInvert write
      SetSampleCoverageInvert;
    {: Set sample coverage. }
    procedure SetSampleCoverage(const Value: TVKfloat; invert: TVKboolean);
    {: Enable/Disable sample mask. }
    property EnableSampleMask: TVKboolean read FEnableSampleMask write
      SetEnableSampleMask;
    {: Sample mask values. }
    property SampleMaskValue[Index: Integer]: TVKbitfield read GetSampleMaskValue
    write SetSampleMaskValue;

    // Textures
    {: Textures bound to each texture unit + binding point. }
    property TextureBinding[Index: Integer; target: TVKTextureTarget]: TVKuint
      read GetTextureBinding write SetTextureBinding;
    property TextureBindingTime[Index: Integer; target: TVKTextureTarget]: Double
      read GetTextureBindingTime;
    property ActiveTextureEnabled[Target: TVKTextureTarget]: Boolean read
    GetActiveTextureEnabled write SetActiveTextureEnabled;
    property SamplerBinding[Index: TVKuint]: TVKuint read GetSamplerBinding
      write SetSamplerBinding;
    property MaxTextureSize: TVKuint read GetMaxTextureSize;
    property Max3DTextureSize: TVKuint read GetMax3DTextureSize;
    property MaxCubeTextureSize: TVKuint read GetMaxCubeTextureSize;
    property MaxArrayTextureSize: TVKuint read GetMaxArrayTextureSize;
    property MaxTextureImageUnits: TVKuint read GetMaxTextureImageUnits;
    property MaxTextureAnisotropy: TVKuint read GetMaxTextureAnisotropy;
    property MaxSamples: TVKuint read GetMaxSamples;
    // TODO: GL_TEXTURE_BUFFER_DATA_STORE_BINDING ?

    // Active texture
    {: The active texture unit.  Valid values are 0 .. Max texture units. }
    property ActiveTexture: TVKint read FActiveTexture write SetActiveTexture;

    // Pixel operations
    {: Enables/Disables scissor test. }
    property EnableScissorTest: TVKboolean read FEnableScissorTest write
      SetEnableScissorTest;
    {: The bounding box used in scissor test. }
    property ScissorBox: TVector4i read FScissorBox write SetScissorBox;
    {: Enables/Disables stencil test. }
    property EnableStencilTest: TVKboolean read FEnableStencilTest write
      SetEnableStencilTest;
    {: The stencil function.  Determines the comparison function to be used
       when comparing the reference + stored stencil values.  }
    property StencilFunc: TStencilFunction read FStencilFunc;
    // write SetStencilFunc;
  {: The stencil value mask.  Masks both the reference + stored stencil
     values. }
    property StencilValueMask: TVKuint read FStencilValueMask;
    // write SetStencilValueMask;
  {: The stencil reference value.  Clamped to 0..255 with an 8 bit stencil. }
    property StencilRef: TVKint read FStencilRef; // write SetStencilRef;
    {: The operation to perform when stencil test fails. }
    property StencilFail: TStencilOp read FStencilFail; // write SetStencilFail;
    {: The operation to perform when stencil test passes + depth test fails. }
    property StencilPassDepthFail: TStencilOp read FStencilPassDepthFail;
    // write SetStencilPassDepthFail;
  {: The operation to perform when stencil test passes + depth test passes. }
    property StencilPassDepthPass: TStencilOp read FStencilPassDepthPass;
    // write SetStencilPassDepthPass;

  {: The stencil back function.  Determines the comparison function to be
     used when comparing the reference + stored stencil values on back
     facing primitives. }
    property StencilBackFunc: TStencilFunction read FStencilBackFunc;
    // write SetStencilBackFunc;
  {: The stencil back value mask.  Masks both the reference + stored stencil
     values. }
    property StencilBackValueMask: TVKuint read FStencilBackValueMask;
    // write SetStencilBackValueMask;
  {: The stencil back reference value.  Clamped to 0..255 with an 8 bit
     stencil. }
    property StencilBackRef: TVKuint read FStencilBackRef;
    // write SetStencilBackRef;
  {: The operation to perform when stencil test fails on back facing
     primitives. }
    property StencilBackFail: TStencilOp read FStencilBackFail;
    // write SetStencilBackFail;
  {: The operation to perform when stencil test passes + depth test fails on
     back facing primitives. }
    property StencilBackPassDepthFail: TStencilOp read
      FStencilBackPassDepthFail;
    // write SetStencilBackPassDepthFail;
  {: The operation to perform when stencil test passes + depth test passes on
     back facing primitives. }
    property StencilBackPassDepthPass: TStencilOp read
      FStencilBackPassDepthPass;
    // write SetStencilBackPassDepthPass;
  {: Used to set stencil Function, Reference + Mask values, for both front +
     back facing primitives. }
    procedure SetStencilFunc(const func: TStencilFunction; const ref: TVKint;
      const mask: TVKuint);
    {: Used to set stencil Function, Reference + Mask values for either the
       front or back facing primitives (or both, which is the same as calling
       SetStencilFunc). }
    procedure SetStencilFuncSeparate(const face: TCullFaceMode;
      const func: TStencilFunction; const ref: TVKint; const mask: TVKuint);
    {: Used to set the StencilFail, StencilPassDepthFail + StencilPassDepthPass
       in one go. }
    procedure SetStencilOp(const fail, zfail, zpass: TStencilOp);
    {: Used to set the StencilFail, StencilPassDepthFail + StencilPassDepthPass
       in one go, for either front or back facing primitives. }
    procedure SetStencilOpSeparate(const face: TCullFaceMode; const sfail,
      dpfail, dppass: TStencilOp);

    {: Enables/disables depth testing. }
    property EnableDepthTest: TVKboolean read FEnableDepthTest write
      SetEnableDepthTest;
    {: The depth function.  Used to determine whether to keep a fragment or
       discard it, depending on the current value stored in the depth buffer. }
    property DepthFunc: TDepthFunction read FDepthFunc write SetDepthFunc;
    {: Enables/disables blending for each draw buffer. }
    property EnableBlend[Index: Integer]: TVKboolean read GetEnableBlend write
    SetEnableBlend;
    {: The weighting factor used in blending equation, for source RGB. }
    property BlendSrcRGB: TBlendFunction read FBlendSrcRGB;
    // write SetBlendSrcRGB;
  {: The weighting factor used in blending equation, for source alpha. }
    property BlendSrcAlpha: TBlendFunction read FBlendSrcAlpha;
    // write SetBlendSrcAlpha;
  {: The weighting factor used in blending equation, for destination RGB. }
    property BlendDstRGB: TDstBlendFunction read FBlendDstRGB;
    // write SetBlendDstRGB;
  {: The weighting factor used in blending equation, for destination alpha. }
    property BlendDstAlpha: TDstBlendFunction read FBlendDstAlpha;
    // write SetBlendDstAlpha;
  {: Sets the weighting factors to be used by the blending equation, for
     both color + alpha. }
    procedure SetBlendFunc(const Src: TBlendFunction;
      const Dst: TDstBlendFunction);
    {: Sets the weighting factors to be used by the blending equation, with
       separate values used for color + alpha components. }
    procedure SetBlendFuncSeparate(const SrcRGB: TBlendFunction;
      const DstRGB: TDstBlendFunction; const SrcAlpha: TBlendFunction;
      const DstAlpha: TDstBlendFunction);
    {: The blending equation.  Determines how the incoming source fragment's
       RGB are combined with the destination RGB. }
    property BlendEquationRGB: TBlendEquation read FBlendEquationRGB;
    // write SetBlendEquationRGB;
  {: The blending equation.  Determines how the incoming source fragment's
     alpha values are combined with the destination alpha values. }
    property BlendEquationAlpha: TBlendEquation read FBlendEquationAlpha;
    // write SetBlendEquationAlpha;
  {: Sets the blend equation for RGB + alpha to the same value. }
    procedure SetBlendEquation(const mode: TBlendEquation);
    {: Sets the blend equations for RGB + alpha separately. }
    procedure SetBlendEquationSeparate(const modeRGB, modeAlpha:
      TBlendEquation);
    {: A constant blend color, that can be used in the blend equation. }
    property BlendColor: TVector read FBlendColor write SetBlendColor;
    {: Enables/disables framebuffer SRGB. }
    property EnableFramebufferSRGB: TVKboolean read FEnableFramebufferSRGB write
      SetEnableFramebufferSRGB;
    {: Enables/disables dithering. }
    property EnableDither: TVKboolean read FEnableDither write SetEnableDither;
    {: Enables/disables color logic op. }
    property EnableColorLogicOp: TVKboolean read FEnableColorLogicOp write
      SetEnableColorLogicOp;
    {: Logic op mode. }
    property LogicOpMode: TLogicOp read FLogicOpMode write SetLogicOpMode;

    // Framebuffer control
    {: The color write mask, for each draw buffer. }
    property ColorWriteMask[Index: Integer]: TColorMask read GetColorWriteMask
    write SetColorWriteMask;
    {: Set the color write mask for all draw buffers. }
    procedure SetColorMask(mask: TColorMask);
    {: The depth write mask. }
    property DepthWriteMask: TVKBoolean read FDepthWriteMask write
      SetDepthWriteMask;
    {: The stencil write mask. }
    property StencilWriteMask: TVKuint read FStencilWriteMask write
      SetStencilWriteMask;
    {: The stencil back write mask. }
    property StencilBackWriteMask: TVKuint read FStencilBackWriteMask write
      SetStencilBackWriteMask;
    {: The color clear value. }
    property ColorClearValue: TVector read FColorClearValue write
      SetColorClearValue;
    {: The depth clear value. }
    property DepthClearValue: TVKfloat read FDepthClearValue write
      SetDepthClearValue;
    {: The stencil clear value. }
    property StencilClearValue: TVKuint read FStencilClearValue write
      SetStencilClearValue;

    // Framebuffer
    {: Framebuffer to be used for draw operations, 0 = default framebuffer. }
    property DrawFrameBuffer: TVKuint read FDrawFrameBuffer write
      SetDrawFrameBuffer;
    {: Framebuffer to be used for read operations, 0 = default framebuffer. }
    property ReadFrameBuffer: TVKuint read FReadFrameBuffer write
      SetReadFrameBuffer;
    {: set both draw + read framebuffer. }
    procedure SetFrameBuffer(const Value: TVKuint);
    //property FrameBuffer: TVKuint read FDrawFrameBuffer write SetFrameBuffer;

    // Renderbuffer
    {: Currently bound render buffer. }
    property RenderBuffer: TVKuint read FRenderBuffer write SetRenderBuffer;

    // Pixels
    {: Controls whether byte swapping occurs during pixel unpacking. }
    property UnpackSwapBytes: TVKboolean read FUnpackSwapBytes write
      SetUnpackSwapBytes;
    {: Whether unpacked data is required with LSB (least significant bit) first. }
    property UnpackLSBFirst: TVKboolean read FUnpackLSBFirst write
      SetUnpackLSBFirst;
    {: Unpack image height. }
    property UnpackImageHeight: TVKuint read FUnpackImageHeight write
      SetUnpackImageHeight;
    {: Unpack skip images. }
    property UnpackSkipImages: TVKuint read FUnpackSkipImages write
      SetUnpackSkipImages;
    {: Unpack row length. }
    property UnpackRowLength: TVKuint read FUnpackRowLength write
      SetUnpackRowLength;
    {: Unpack skip rows. }
    property UnpackSkipRows: TVKuint read FUnpackSkipRows write
      SetUnpackSkipRows;
    {: Unpack skip pixels. }
    property UnpackSkipPixels: TVKuint read FUnpackSkipPixels write
      SetUnpackSkipPixels;
    {: Unpack alignment. }
    property UnpackAlignment: TVKuint read FUnpackAlignment write
      SetUnpackAlignment;
    {: Controls whether byte swapping occurs during pixel packing. }
    property PackSwapBytes: TVKboolean read FPackSwapBytes write
      SetPackSwapBytes;
    {: Whether packed data is required with LSB (least significant bit) first. }
    property PackLSBFirst: TVKboolean read FPackLSBFirst write SetPackLSBFirst;
    {: Pack image height. }
    property PackImageHeight: TVKuint read FPackImageHeight write
      SetPackImageHeight;
    {: Pack skip images. }
    property PackSkipImages: TVKuint read FPackSkipImages write
      SetPackSkipImages;
    {: Pack row length. }
    property PackRowLength: TVKuint read FPackRowLength write SetPackRowLength;
    {: Pack skip rows. }
    property PackSkipRows: TVKuint read FPackSkipRows write SetPackSkipRows;
    {: Pack skip pixels. }
    property PackSkipPixels: TVKuint read FPackSkipPixels write
      SetPackSkipPixels;
    {: Pack alignment. }
    property PackAlignment: TVKuint read FPackAlignment write SetPackAlignment;
    {: Buffer bound for pixel packing (eg. ReadPixels). }
    property PixelPackBufferBinding: TVKuint read FPixelPackBufferBinding
      write SetPixelPackBufferBinding;
    {: Buffer bound for pixel unpacking (eg. Tex*Image). }
    property PixelUnpackBufferBinding: TVKuint read FPixelUnpackBufferBinding
      write SetPixelUnpackBufferBinding;

    // Program
    {: Currently bound program. }
    property CurrentProgram: TVKuint read FCurrentProgram write
      SetCurrentProgram;
    property MaxTextureUnits: TVKuint read GetMaxTextureUnits;
    {: Currently bound uniform buffer. }
    property UniformBufferBinding: TVKuint read FUniformBufferBinding
      write SetUniformBufferBinding;

    procedure SetBufferIndexedBinding(const Value: TVKuint; ATarget: TVKBufferBindingTarget; AIndex: TVKuint; ABufferSize: TVKsizeiptr); overload;
    procedure SetBufferIndexedBinding(const Value: TVKuint; ATarget: TVKBufferBindingTarget; AIndex: TVKuint; AOffset: TVKintptr; ARangeSize: TVKsizeiptr); overload;

    // Vector + Geometry Shader state
    {: Default values to be used when a vertex array is not used for that
       attribute. }
    property CurrentVertexAttrib[Index: Integer]: TVector
    read GetCurrentVertexAttrib write SetCurrentVertexAttrib;
    {: Enables/disables program point size. }
    property EnableProgramPointSize: TVKboolean read FEnableProgramPointSize
      write SetEnableProgramPointSize;

    // Transform Feedback state
    {: Currently bound transform feedbac buffer. }
    property TransformFeedbackBufferBinding: TVKuint
      read FTransformFeedbackBufferBinding write
      SetTransformFeedbackBufferBinding;

    // Hints
    {: Line smooth hint. }
    property LineSmoothHint: THintType read FLineSmoothHint write
      SetLineSmoothHint;
    {: Polygon smooth hint. }
    property PolygonSmoothHint: THintType read FPolygonSmoothHint write
      SetPolygonSmoothHint;
    {: Texture compression hint. }
    property TextureCompressionHint: THintType
      read FTextureCompressionHint write SetTextureCompressionHint;
    {: Fragment shader derivitive hint. }
    property FragmentShaderDerivitiveHint: THintType
      read FFragmentShaderDerivitiveHint write SetFragmentShaderDerivitiveHint;
    property MultisampleFilterHint: THintType read FMultisampleFilterHint
      write SetMultisampleFilterHint;

    // Misc
    {: Current queries. }
    property CurrentQuery[Index: TQueryType]: TVKuint read GetCurrentQuery;
    {: Begins a query of "Target" type.  "Value" must be a valid query object. }
    procedure BeginQuery(const Target: TQueryType; const Value: TVKuint);
    {: Ends current query of type "Target". }
    procedure EndQuery(const Target: TQueryType);
    {: The buffer currently bound to the copy read buffer binding point, this
       is an extra binding point provided so that you don't need to overwrite
       other binding points to copy between buffers. }
    property CopyReadBufferBinding: TVKuint read FCopyReadBufferBinding
      write SetCopyReadBufferBinding;
    {: The buffer currently bound to the copy write buffer binding point, this
       is an extra binding point provided so that you don't need to overwrite
       other binding points to copy between buffers. }
    property CopyWriteBufferBinding: TVKuint read FCopyWriteBufferBinding
      write SetCopyWriteBufferBinding;
    {: Enables/Disables seamless texture cube maps. }
    property EnableTextureCubeMapSeamless: TVKboolean read
      FEnableTextureCubeMapSeamless write SetEnableTextureCubeMapSeamless;
    {: Indicates the current presence within the list. }
    property InsideList: Boolean read FInsideList;
    {: Begin new display list. }
    procedure NewList(list: TVKuint; mode: TVKEnum);
    {: End display list. }
    procedure EndList;
    {: Call display list. }
    procedure CallList(list: TVKuint);

    {: Defines the OpenGL texture matrix.<p>
       Assumed texture mode is GL_MODELVIEW. }
    procedure SetGLTextureMatrix(const matrix: TMatrix);
    procedure ResetGLTextureMatrix;
    procedure ResetAllGLTextureMatrix;

    // note: needs to change to per draw-buffer
    procedure SetGLColorWriting(flag: Boolean);

    {: Inverts front face winding (CCW/CW). }
    procedure InvertGLFrontFace;

    // read only properties
    property States: TVKStates read FStates;

    {: True for ignore deprecated and removed features in OpenGL 3x }
    property ForwardContext: Boolean read FForwardContext
      write SetForwardContext;

  end;

type
  TStateRecord = record
    GLConst: TVKEnum;
    GLDeprecated: Boolean;
  end;

const
{$WARN SYMBOL_DEPRECATED OFF}
  cGLStateTypeToGLEnum: array[TVKStateType] of TVKenum = (
    GL_CURRENT_BIT, GL_POINT_BIT, GL_LINE_BIT, GL_POLYGON_BIT,
    GL_POLYGON_STIPPLE_BIT, GL_PIXEL_MODE_BIT, GL_LIGHTING_BIT, GL_FOG_BIT,
    GL_DEPTH_BUFFER_BIT, GL_ACCUM_BUFFER_BIT, GL_STENCIL_BUFFER_BIT,
    GL_VIEWPORT_BIT, GL_TRANSFORM_BIT, GL_ENABLE_BIT, GL_COLOR_BUFFER_BIT,
    GL_HINT_BIT, GL_EVAL_BIT, GL_LIST_BIT, GL_TEXTURE_BIT, GL_SCISSOR_BIT,
    GL_MULTISAMPLE_BIT);

{$WARN SYMBOL_DEPRECATED ON}
  cGLStateToGLEnum: array[TVKState] of TStateRecord =
    ((GLConst: GL_ALPHA_TEST; GLDeprecated: True),
    (GLConst: GL_AUTO_NORMAL; GLDeprecated: True),
    (GLConst: GL_BLEND; GLDeprecated: False),
    (GLConst: GL_COLOR_MATERIAL; GLDeprecated: True),
    (GLConst: GL_CULL_FACE; GLDeprecated: False),
    (GLConst: GL_DEPTH_TEST; GLDeprecated: False),
    (GLConst: GL_DITHER; GLDeprecated: False),
    (GLConst: GL_FOG; GLDeprecated: True),
    (GLConst: GL_LIGHTING; GLDeprecated: True),
    (GLConst: GL_LINE_SMOOTH; GLDeprecated: True),
    (GLConst: GL_LINE_STIPPLE; GLDeprecated: True),
    (GLConst: GL_INDEX_LOGIC_OP; GLDeprecated: True),
    (GLConst: GL_COLOR_LOGIC_OP; GLDeprecated: False),
    (GLConst: GL_NORMALIZE; GLDeprecated: True),
    (GLConst: GL_POINT_SMOOTH; GLDeprecated: True),
    (GLConst: GL_POINT_SPRITE; GLDeprecated: True),
    (GLConst: GL_POLYGON_SMOOTH; GLDeprecated: True),
    (GLConst: GL_POLYGON_STIPPLE; GLDeprecated: True),
    (GLConst: GL_SCISSOR_TEST; GLDeprecated: False),
    (GLConst: GL_STENCIL_TEST; GLDeprecated: False),
    (GLConst: GL_POLYGON_OFFSET_POINT; GLDeprecated: False),
    (GLConst: GL_POLYGON_OFFSET_LINE; GLDeprecated: False),
    (GLConst: GL_POLYGON_OFFSET_FILL; GLDeprecated: False),
    (GLConst: GL_DEPTH_CLAMP; GLDeprecated: False)
    );

  cGLTexTypeToGLEnum: array[TVKTextureTarget] of TVKenum =
    (0, GL_TEXTURE_1D, GL_TEXTURE_2D, GL_TEXTURE_3D, GL_TEXTURE_1D_ARRAY,
    GL_TEXTURE_2D_ARRAY, GL_TEXTURE_RECTANGLE, GL_TEXTURE_BUFFER,
    GL_TEXTURE_CUBE_MAP, GL_TEXTURE_2D_MULTISAMPLE,
    GL_TEXTURE_2D_MULTISAMPLE_ARRAY, GL_TEXTURE_CUBE_MAP_ARRAY);

  cGLQueryTypeToGLEnum: array[TQueryType] of TVKenum =
    (GL_SAMPLES_PASSED, GL_PRIMITIVES_GENERATED,
    GL_TRANSFORM_FEEDBACK_PRIMITIVES_WRITTEN,
    GL_TIME_ELAPSED, GL_ANY_SAMPLES_PASSED);

  cGLStencilOpToGLEnum: array[TStencilOp] of TVKenum =
    (GL_KEEP, GL_ZERO, GL_REPLACE, GL_INCR, GL_DECR, GL_INVERT, GL_INCR_WRAP,
    GL_DECR_WRAP);

  cGLLogicOpToGLEnum: array[TLogicOp] of TVKEnum =
    (GL_CLEAR, GL_AND, GL_AND_REVERSE, GL_COPY, GL_AND_INVERTED, GL_NOOP,
    GL_XOR, GL_OR, GL_NOR, GL_EQUIV, GL_INVERT, GL_OR_REVERSE,
    GL_COPY_INVERTED, GL_OR_INVERTED, GL_NAND, GL_SET);

  cGLComparisonFunctionToGLEnum: array[TComparisonFunction] of TVKenum =
    (GL_NEVER, GL_ALWAYS, GL_LESS, GL_LEQUAL, GL_EQUAL, GL_GREATER,
    GL_NOTEQUAL, GL_GEQUAL);

  cGLBlendFunctionToGLEnum: array[TBlendFunction] of TVKenum =
    (GL_ZERO, GL_ONE, GL_SRC_COLOR, GL_ONE_MINUS_SRC_COLOR, GL_DST_COLOR,
    GL_ONE_MINUS_DST_COLOR, GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA,
    GL_DST_ALPHA, GL_ONE_MINUS_DST_ALPHA, GL_CONSTANT_COLOR,
    GL_ONE_MINUS_CONSTANT_COLOR, GL_CONSTANT_ALPHA,
    GL_ONE_MINUS_CONSTANT_ALPHA, GL_SRC_ALPHA_SATURATE {valid for src only});

  cGLBlendEquationToGLEnum: array[TBlendEquation] of TVKEnum =
    (GL_FUNC_ADD, GL_FUNC_SUBTRACT, GL_FUNC_REVERSE_SUBTRACT, GL_MIN,
    GL_MAX);

  cGLFaceWindingToGLEnum: array[TFaceWinding] of TVKenum =
    (GL_CCW, GL_CW);

  cGLPolygonModeToGLEnum: array[TPolygonMode] of TVKEnum =
    (GL_FILL, GL_LINE, GL_POINT);

  cGLCullFaceModeToGLEnum: array[TCullFaceMode] of TVKEnum =
    (GL_FRONT, GL_BACK, GL_FRONT_AND_BACK);

  cGLHintToGLEnum: array[THintType] of TVKEnum =
    (GL_DONT_CARE, GL_FASTEST, GL_NICEST);

  cGLBufferBindingTarget: array[TVKBufferBindingTarget] of TVKEnum =
    (GL_UNIFORM_BUFFER, GL_TRANSFORM_FEEDBACK_BUFFER);
  //------------------------------------------------------
  //------------------------------------------------------
  //------------------------------------------------------
implementation
//------------------------------------------------------
//------------------------------------------------------
//------------------------------------------------------

uses
  VKS.Context, VKS.Color;

{$IFDEF GLS_CACHE_MISS_CHECK}
resourcestring
  glsStateCashMissing = 'States cash missing: ';
{$ENDIF}

  // ------------------
  // ------------------ TVKStateCache ------------------
  // ------------------

procedure TVKStateCache.BeginQuery(const Target: TQueryType; const Value:
  TVKuint);
begin
  Assert(FCurrentQuery[Target] = 0, 'Can only have one query (of each type)' +
    ' running at a time');
  // Assert(glIsQuery(Value), 'Not a valid query');
 //  if Value<>FCurrentQuery[Target] then
  begin
    FCurrentQuery[Target] := Value;
    GL.BeginQuery(cGLQueryTypeToGLEnum[Target], Value);
  end;
end;

// Create
//
constructor TVKStateCache.Create;
var
  I: Integer;
begin
  inherited;
  SetLength(FListStates, 128);
  FCurrentList := 0;

  // Material colors
  FFrontBackColors[0][0] := clrBlack;
  FFrontBackColors[0][1] := clrGray20;
  FFrontBackColors[0][2] := clrGray80;
  FFrontBackColors[0][3] := clrBlack;
  FFrontBackShininess[0] := 0;

  FFrontBackColors[1][0] := clrBlack;
  FFrontBackColors[1][1] := clrGray20;
  FFrontBackColors[1][2] := clrGray80;
  FFrontBackColors[1][3] := clrBlack;
  FFrontBackShininess[1] := 0;

  FAlphaFunc := cfAlways;

  // Lighting
  FFFPLight := True;
  FMaxLights := 0;
  FLightNumber := 0;

  for I := High(FLightEnabling) downto 0 do
  begin
    FLightEnabling[I] := False;
    FLightIndices[I] := 0;
    FLightStates.Position[I] := NullHmgVector;
    FLightStates.Ambient[I] := clrBlack;
    FLightStates.Diffuse[I] := clrBlack;
    FLightStates.Specular[I] := clrBlack;
    FLightStates.SpotDirection[I] := VectorMake(0.0, 0.0, -1.0, 0.0);
    FSpotCutoff[I] := 180.0;
    FlightStates.SpotCosCutoffExponent[I].V[0] := -1;
    FLightStates.SpotCosCutoffExponent[I].V[1] := 0;
    FLightStates.Attenuation[I] := NullHmgVector;
  end;
  FLightStates.Diffuse[0] := clrWhite;
  FLightStates.Specular[0] := clrWhite;

  for I := High(FTextureMatrixIsIdentity) downto 0 do
    FTextureMatrixIsIdentity[I] := False;
  FForwardContext := False;

  // Vertex Array Data state
  FVertexArrayBinding := 0;
  FTextureBufferBinding := 0;

  // Transformation state
  // FViewPort := Rect(0,0,0,0);  // (0, 0, Width, Height)
  FDepthRange[0] := 0.0;
  FDepthRange[1] := 1.0;

  FillChar(FEnableClipDistance, sizeof(FEnableClipDistance), $00);
  FEnableDepthClamp := false;

  // Coloring state
  FClampReadColor := GL_FIXED_ONLY;
  FProvokingVertex := GL_LAST_VERTEX_CONVENTION;

  // Rasterization state
  FPointSize := 1.0;
  FPointFadeThresholdSize := 1.0;
  FPointSpriteCoordOrigin := GL_UPPER_LEFT;
  FLineWidth := 1.0;
  FLineStippleFactor := 1;
  FLineStipplePattern := $FFFF;
  FEnableLineSmooth := false;
  FEnableCullFace := false;
  FCullFaceMode := cmBack;
  FFrontFace := fwCounterClockWise;
  FEnablePolygonSmooth := false;
  FPolygonMode := pmFill;
  FPolygonOffsetFactor := 0.0;
  FPolygonOffsetUnits := 0.0;
  FEnablePolygonOffsetPoint := false;
  FEnablePolygonOffsetLine := false;
  FEnablePolygonOffsetFill := false;

  // Multisample state
  FEnableMultisample := true;
  FEnableSampleAlphaToCoverage := false;
  FEnableSampleAlphaToOne := false;
  FEnableSampleCoverage := false;
  FSampleCoverageValue := 1.0;
  FSampleCoverageInvert := false;
  FEnableSampleMask := false;
  FillChar(FSampleMaskValue, sizeof(FSampleMaskValue), $FF);

  // Texture state
  FillChar(FTextureBinding, sizeof(FTextureBinding), $00);
  FillChar(FActiveTextureEnabling, sizeof(FActiveTextureEnabling), $00);

  // Active texture state
  FActiveTexture := 0;

  // Pixel operation state
  FEnableScissorTest := false;
  //    FScissorBox := Rect(0, 0, Width, Height);
  FEnableStencilTest := false;
  FStencilFunc := cfAlways;
  FStencilValueMask := $FFFFFFFF;
  FStencilRef := 0;
  FStencilFail := soKeep;
  FStencilPassDepthFail := soKeep;
  FStencilPassDepthPass := soKeep;

  FStencilBackFunc := cfAlways;
  FStencilBackValueMask := $FFFFFFFF;
  FStencilBackRef := 0;
  FStencilBackFail := soKeep;
  FStencilBackPassDepthPass := soKeep;
  FStencilBackPassDepthFail := soKeep;

  FEnableDepthTest := false;
  FDepthFunc := cfLess;

  FillChar(FEnableBlend, sizeof(FEnableBlend), $0);

  FBlendSrcRGB := bfOne;
  FBlendSrcAlpha := bfOne;
  FBlendDstRGB := bfZero;
  FBlendDstAlpha := bfZero;

  FBlendEquationRGB := beAdd;
  FBlendEquationAlpha := beAdd;
  FBlendColor := NullHmgVector;

  FEnableFramebufferSRGB := false;
  FEnableDither := true;
  FEnableColorLogicOp := false;

  FLogicOpMode := loCopy;

  // Framebuffer control state
//    for I := 0 to Length(FColorWriteMask) - 1 do
//      FColorWriteMask[i] := [ccRed, ccGreen, ccBlue, ccAlpha];
  FillChar(FColorWriteMask, sizeof(FColorWriteMask), $F);
  FDepthWriteMask := True;
  FStencilWriteMask := $FFFFFFFF;
  FStencilBackWriteMask := $FFFFFFFF;
  FColorClearValue := NullHmgVector;
  FDepthClearValue := 1.0;
  FStencilClearValue := 0;

  // Framebuffer state
  FDrawFrameBuffer := 0;
  FReadFrameBuffer := 0;

  // Renderbuffer state
  FRenderBuffer := 0;

  // Pixels state
  FUnpackSwapBytes := false;
  FUnpackLSBFirst := false;
  FUnpackImageHeight := 0;
  FUnpackSkipImages := 0;
  FUnpackRowLength := 0;
  FUnpackSkipRows := 0;
  FUnpackSkipPixels := 0;
  FUnpackAlignment := 4;
  FPackSwapBytes := False;
  FPackLSBFirst := False;
  FPackImageHeight := 0;
  FPackSkipImages := 0;
  FPackRowLength := 0;
  FPackSkipRows := 0;
  FPackSkipPixels := 0;
  FPackAlignment := 4;

  FPixelPackBufferBinding := 0;
  FPixelUnpackBufferBinding := 0;

  // Program state
  FCurrentProgram := 0;
  FUniformBufferBinding := 0;
  FillChar(FUBOStates[bbtUniform][0], SizeOf(FUBOStates), $00);

  // Vector + Geometry Shader state
  for I := 0 to Length(FCurrentVertexAttrib) - 1 do
    FCurrentVertexAttrib[I] := NullHmgPoint;
  FEnableProgramPointSize := false;

  // Transform Feedback state
  FTransformFeedbackBufferBinding := 0;

  // Hints state
  FTextureCompressionHint := hintDontCare;
  FPolygonSmoothHint := hintDontCare;
  FFragmentShaderDerivitiveHint := hintDontCare;
  FLineSmoothHint := hintDontCare;

  // Misc state
  FillChar(FCurrentQuery, sizeof(FCurrentQuery), $00);
  FCopyReadBufferBinding := 0;
  FCopyWriteBufferBinding := 0;
  FEnableTextureCubeMapSeamless := false;
  FInsideList := False;
end;

// Destroy
//
destructor TVKStateCache.Destroy;
begin
  inherited;
end;

procedure TVKStateCache.EndQuery(const Target: TQueryType);
begin
  Assert(FCurrentQuery[Target] <> 0, 'No query running');
  FCurrentQuery[Target] := 0;
  GL.EndQuery(cGLQueryTypeToGLEnum[Target]);
end;

// Enable
//
procedure TVKStateCache.Enable(const aState: TVKState);
begin
  if cGLStateToGLEnum[aState].GLDeprecated and FForwardContext then
    exit;
  if not (aState in FStates) or FInsideList then
  begin
    if FInsideList then
      Include(FListStates[FCurrentList], sttEnable)
    else
      Include(FStates, aState);
{$IFDEF GLS_CACHE_MISS_CHECK}
    if GL.IsEnabled(cGLStateToGLEnum[aState].GLConst) then
      GLSLogger.LogError(glsStateCashMissing + 'Enable');
{$ENDIF}
    GL.Enable(cGLStateToGLEnum[aState].GLConst);
  end;
end;

// Disable
//
procedure TVKStateCache.Disable(const aState: TVKState);
begin
  if cGLStateToGLEnum[aState].GLDeprecated and FForwardContext then
    exit;
  if (aState in FStates) or FInsideList then
  begin
    if FInsideList then
      Include(FListStates[FCurrentList], sttEnable)
    else
      Exclude(FStates, aState);
{$IFDEF GLS_CACHE_MISS_CHECK}
    if not GL.IsEnabled(cGLStateToGLEnum[aState].GLConst) then
      GLSLogger.LogError(glsStateCashMissing + 'Disable');
{$ENDIF}
    GL.Disable(cGLStateToGLEnum[aState].GLConst);
    if aState = stColorMaterial then
      if FInsideList then
        Include(FListStates[FCurrentList], sttLighting)
      else
        with GL do
        begin
          Materialfv(GL_FRONT, GL_EMISSION, @FFrontBackColors[0][0]);
          Materialfv(GL_FRONT, GL_AMBIENT, @FFrontBackColors[0][1]);
          Materialfv(GL_FRONT, GL_DIFFUSE, @FFrontBackColors[0][2]);
          Materialfv(GL_FRONT, GL_SPECULAR, @FFrontBackColors[0][3]);
          Materiali(GL_FRONT, GL_SHININESS, FFrontBackShininess[0]);

          Materialfv(GL_BACK, GL_EMISSION, @FFrontBackColors[1][0]);
          Materialfv(GL_BACK, GL_AMBIENT, @FFrontBackColors[1][1]);
          Materialfv(GL_BACK, GL_DIFFUSE, @FFrontBackColors[1][2]);
          Materialfv(GL_BACK, GL_SPECULAR, @FFrontBackColors[1][3]);
          Materiali(GL_BACK, GL_SHININESS, FFrontBackShininess[1]);
        end;
  end;
end;

// PerformEnable
//

procedure TVKStateCache.PerformEnable(const aState: TVKState);
begin
  if cGLStateToGLEnum[aState].GLDeprecated and FForwardContext then
    exit;
  Include(FStates, aState);
  GL.Enable(cGLStateToGLEnum[aState].GLConst);
end;

// PerformDisable
//
procedure TVKStateCache.PerformDisable(const aState: TVKState);
begin
  if cGLStateToGLEnum[aState].GLDeprecated and FForwardContext then
    exit;
  Exclude(FStates, aState);
  GL.Disable(cGLStateToGLEnum[aState].GLConst);
end;

procedure TVKStateCache.PopAttrib;
begin
  // TODO: replace with proper client side push/pop
  GL.PopAttrib();
end;

procedure TVKStateCache.PushAttrib(stateTypes: TVKStateTypes);
var
  tempFlag: TVKuint;
  I: Integer;
begin
  // TODO: replace with proper client side push/pop
  tempFlag := 0;
  for I := Integer(Low(TVKStateType)) to Integer(high(TVKStateType)) do
  begin
    if TVKStateType(I) in stateTypes then
    begin
      tempFlag := tempFlag or cGLStateTypeToGLEnum[TVKStateType(I)];
    end;
  end;
  GL.PushAttrib(tempFlag);
end;

// SetGLMaterialColors
//

procedure TVKStateCache.SetGLMaterialColors(const aFace: TCullFaceMode;
  const emission, ambient, diffuse, specular: TVector;
  const shininess: Integer);
var
  i: Integer;
  currentFace: TVKenum;
begin
  if FForwardContext then
    exit;
  Assert((aFace = cmFront) or (aFace = cmBack),
    'Only cmFront or cmBack supported');
  i := Integer(aFace);
  currentFace := cGLCullFaceModeToGLEnum[aFace];

  if (FFrontBackShininess[i] <> shininess)
    or FInsideList then
  begin
    GL.Materiali(currentFace, GL_SHININESS, shininess);
    if not FInsideList then
      FFrontBackShininess[i] := shininess;
  end;
  if not AffineVectorEquals(FFrontBackColors[i][0], emission)
    or FInsideList then
  begin
    GL.Materialfv(currentFace, GL_EMISSION, @emission);
    if not FInsideList then
      SetVector(FFrontBackColors[i][0], emission);
  end;
  if not AffineVectorEquals(FFrontBackColors[i][1], ambient)
    or FInsideList then
  begin
    GL.Materialfv(currentFace, GL_AMBIENT, @ambient);
    if not FInsideList then
      SetVector(FFrontBackColors[i][1], ambient);
  end;
  if not VectorEquals(FFrontBackColors[i][2], diffuse)
    or FInsideList then
  begin
    GL.Materialfv(currentFace, GL_DIFFUSE, @diffuse);
    if not FInsideList then
      SetVector(FFrontBackColors[i][2], diffuse);
  end;
  if not AffineVectorEquals(FFrontBackColors[i][3], specular)
    or FInsideList then
  begin
    GL.Materialfv(currentFace, GL_SPECULAR, @specular);
    if not FInsideList then
      SetVector(FFrontBackColors[i][3], specular);
  end;
  if FInsideList then
    Include(FListStates[FCurrentList], sttLighting);
end;

// SetGLMaterialAlphaChannel
//

procedure TVKStateCache.SetGLMaterialAlphaChannel(const aFace: TVKEnum; const
  alpha: TVKFloat);
var
  i: Integer;
  color: TVector4f;
begin
  if FForwardContext then Exit;

  if not(stLighting in FStates) then
  begin
    // We need a temp variable, because FColor is cauched.
    GL.GetFloatv(GL_CURRENT_COLOR, @color);
    color.V[3] := alpha;
    GL.Color4fv(@color);
  end
  else
  begin
    i := aFace - GL_FRONT;
    if (FFrontBackColors[i][2].V[3] <> alpha) or FInsideList then
    begin
      if FInsideList then
      begin
        Include(FListStates[FCurrentList], sttLighting);
        GL.Materialfv(aFace, GL_DIFFUSE, @FFrontBackColors[i][2]);

      end
      else
      begin
        FFrontBackColors[i][2].V[3] := alpha;
        GL.Materialfv(aFace, GL_DIFFUSE, @FFrontBackColors[i][2]);
      end;
    end;
  end;
end;

procedure TVKStateCache.SetGLMaterialDiffuseColor(const aFace: TVKEnum; const diffuse: TVector);
var
  i: Integer;
begin
  if FForwardContext then Exit;

  if not(stLighting in FStates) then
  begin
    GL.Color4fv(@diffuse);
  end
  else
  begin
    //
    i := aFace - GL_FRONT;
    if (not VectorEquals(FFrontBackColors[i][2], diffuse)) or FInsideList then
    begin
      if FInsideList then
      begin
        Include(FListStates[FCurrentList], sttLighting);
        GL.Materialfv(aFace, GL_DIFFUSE, @FFrontBackColors[i][2]);
      end
      else
      begin
        FFrontBackColors[i][2] := diffuse;
        GL.Materialfv(aFace, GL_DIFFUSE, @diffuse);
      end;
    end;
  end;
end;

procedure TVKStateCache.SetActiveTexture(const Value: TVKint);
begin
  if GL.ARB_multitexture then
    if (Value <> FActiveTexture) or FInsideList then
    begin
      if FInsideList then
        Include(FListStates[FCurrentList], sttTexture)
      else
        FActiveTexture := Value;
      GL.ActiveTexture(GL_TEXTURE0 + Value);
    end;
end;

procedure TVKStateCache.SetVertexArrayBinding(const Value: TVKuint);
begin
  if Value <> FVertexArrayBinding then
  begin
    FVertexArrayBinding := Value;
    GL.BindVertexArray(Value);
  end;
end;

function TVKStateCache.GetArrayBufferBinding: TVKuint;
begin
  Result := FArrayBufferBinding;
end;

procedure TVKStateCache.SetArrayBufferBinding(const Value: TVKuint);
begin
  if (Value <> FArrayBufferBinding) or (FVertexArrayBinding <> 0) then
  begin
    FArrayBufferBinding := Value;
    GL.BindBuffer(GL_ARRAY_BUFFER, Value);
  end;
end;

function TVKStateCache.GetElementBufferBinding: TVKuint;
begin
  Result := FElementBufferBinding
end;

procedure TVKStateCache.SetElementBufferBinding(const Value: TVKuint);
begin
  if (Value <> FElementBufferBinding) or (FVertexArrayBinding <> 0) then
  begin
    FElementBufferBinding := Value;
    GL.BindBuffer(GL_ELEMENT_ARRAY_BUFFER, Value);
  end;
end;

function TVKStateCache.GetEnablePrimitiveRestart: TVKboolean;
begin
  Result := FEnablePrimitiveRestart;
end;

procedure TVKStateCache.SetEnablePrimitiveRestart(const enabled: TVKboolean);
begin
  if enabled <> FEnablePrimitiveRestart then
  begin
    FEnablePrimitiveRestart := enabled;
    if FForwardContext then
    begin
      if enabled then
        GL.Enable(GL_PRIMITIVE_RESTART)
      else
        GL.Disable(GL_PRIMITIVE_RESTART);
    end
    else if GL.NV_primitive_restart then
    begin
      if enabled then
        GL.EnableClientState(GL_PRIMITIVE_RESTART_NV)
      else
        GL.DisableClientState(GL_PRIMITIVE_RESTART_NV);
    end;
  end;
end;

function TVKStateCache.GetPrimitiveRestartIndex: TVKuint;
begin
  Result := FPrimitiveRestartIndex;
end;

procedure TVKStateCache.SetPrimitiveRestartIndex(const index: TVKuint);
begin
  if index <> FPrimitiveRestartIndex then
  begin
    if GL.NV_primitive_restart or FForwardContext then
    begin
      FPrimitiveRestartIndex := index;
      GL.PrimitiveRestartIndex(index)
    end;
  end;
end;

procedure TVKStateCache.SetEnableProgramPointSize(const Value: TVKboolean);
begin
  if Value <> FEnableProgramPointSize then
  begin
    FEnableProgramPointSize := Value;
    if Value then
      GL.Enable(GL_PROGRAM_POINT_SIZE)
    else
      GL.Disable(GL_PROGRAM_POINT_SIZE);
  end;
end;

procedure TVKStateCache.SetBlendColor(const Value: TVector);
begin
  if not VectorEquals(Value, FBlendColor) or FInsideList then
  begin
    if FInsideList then
      Include(FListStates[FCurrentList], sttColorBuffer)
    else
      FBlendColor := Value;
    GL.BlendColor(Value.V[0], Value.V[1], Value.V[2], Value.V[3]);
  end;
end;

procedure TVKStateCache.SetBlendEquationSeparate(const modeRGB, modeAlpha:
  TBlendEquation);
begin
  if (modeRGB <> FBlendEquationRGB) or (modeAlpha <> FBlendEquationAlpha)
    or FInsideList then
  begin
    FBlendEquationRGB := modeRGB;
    FBlendEquationAlpha := modeAlpha;
    GL.BlendEquationSeparate(cGLBlendEquationToGLEnum[modeRGB],
      cGLBlendEquationToGLEnum[modeAlpha]);
  end;
  if FInsideList then
    Include(FListStates[FCurrentList], sttColorBuffer);
end;

procedure TVKStateCache.SetBlendEquation(const mode: TBlendEquation);
begin
  if (mode <> FBlendEquationRGB) or (mode <> FBlendEquationAlpha)
    or FInsideList then
  begin
    if FInsideList then
      Include(FListStates[FCurrentList], sttColorBuffer)
    else
    begin
      FBlendEquationRGB := mode;
      FBlendEquationAlpha := mode;
    end;
    GL.BlendEquation(cGLBlendEquationToGLEnum[mode]);
  end;
end;

procedure TVKStateCache.SetBlendFunc(const Src: TBlendFunction;
  const Dst: TDstBlendFunction);
begin
  if (Src <> FBlendSrcRGB) or (Dst <> FBlendDstRGB) or FInsideList then
  begin
    if FInsideList then
      Include(FListStates[FCurrentList], sttColorBuffer)
    else
    begin
      FBlendSrcRGB := Src;
      FBlendDstRGB := Dst;
      FBlendSrcAlpha := Src;
      FBlendSrcAlpha := Dst;
    end;
    GL.BlendFunc(cGLBlendFunctionToGLEnum[Src], cGLBlendFunctionToGLEnum[Dst]);
  end;
end;

procedure TVKStateCache.SetBlendFuncSeparate(const SrcRGB: TBlendFunction;
  const DstRGB: TDstBlendFunction; const SrcAlpha: TBlendFunction;
  const DstAlpha: TDstBlendFunction);
begin
  if (SrcRGB <> FBlendSrcRGB) or (DstRGB <> FBlendDstRGB) or
    (SrcAlpha <> FBlendSrcAlpha) or (DstAlpha <> FBlendDstAlpha)
    or FInsideList then
  begin
    if FInsideList then
      Include(FListStates[FCurrentList], sttColorBuffer)
    else
    begin
      FBlendSrcRGB := SrcRGB;
      FBlendDstRGB := DstRGB;
      FBlendSrcAlpha := SrcAlpha;
      FBlendDstAlpha := DstAlpha;
    end;
    GL.BlendFuncSeparate(
      cGLBlendFunctionToGLEnum[SrcRGB],
      cGLBlendFunctionToGLEnum[DstRGB],
      cGLBlendFunctionToGLEnum[SrcAlpha],
      cGLBlendFunctionToGLEnum[DstAlpha]);
  end;
end;

procedure TVKStateCache.SetClampReadColor(const Value: TVKenum);
begin
  if (Value <> FClampReadColor) or FInsideList then
  begin
    if FInsideList then
      Include(FListStates[FCurrentList], sttColorBuffer)
    else
      FClampReadColor := Value;
    GL.ClampColor(GL_CLAMP_READ_COLOR, Value);
  end;
end;

procedure TVKStateCache.SetColorWriteMask(Index: Integer;
  const Value: TColorMask);
begin
  if FColorWriteMask[Index] <> Value then
  begin
    FColorWriteMask[Index] := Value;
    GL.ColorMaski(Index, ccRed in Value, ccGreen in Value, ccBlue in Value,
      ccAlpha in Value);
  end;
end;

procedure TVKStateCache.SetCopyReadBufferBinding(const Value: TVKuint);
begin
  if Value <> FCopyReadBufferBinding then
  begin
    FCopyReadBufferBinding := Value;
    GL.BindBuffer(GL_COPY_READ_BUFFER, Value);
  end;
end;

procedure TVKStateCache.SetCopyWriteBufferBinding(const Value: TVKuint);
begin
  if Value <> FCopyWriteBufferBinding then
  begin
    FCopyWriteBufferBinding := Value;
    GL.BindBuffer(GL_COPY_WRITE_BUFFER, Value);
  end;
end;

procedure TVKStateCache.SetCullFaceMode(const Value: TCullFaceMode);
begin
  if (Value <> FCullFaceMode) or FInsideList then
  begin
    if FInsideList then
      Include(FListStates[FCurrentList], sttPolygon)
    else
      FCullFaceMode := Value;
    GL.CullFace(cGLCullFaceModeToGLEnum[Value]);
  end;

end;

procedure TVKStateCache.SetCurrentProgram(const Value: TVKuint);
begin
  if Value <> FCurrentProgram then
  begin
    FCurrentProgram := Value;
    GL.UseProgram(Value);
  end;
end;

procedure TVKStateCache.SetTextureBufferBinding(const Value: TVKuint);
begin
  if Value <> FTextureBufferBinding then
  begin
    FTextureBufferBinding := Value;
    GL.BindBuffer(GL_TEXTURE_BUFFER, Value);
  end;
end;

procedure TVKStateCache.SetCurrentVertexAttrib(Index: Integer;
  const Value: TVector);
begin
  if not VectorEquals(Value, FCurrentVertexAttrib[Index]) then
  begin
    FCurrentVertexAttrib[Index] := Value;
    GL.VertexAttrib4fv(Index, @Value.V[0]);
  end;
end;

procedure TVKStateCache.SetDepthClearValue(const Value: TVKfloat);
begin
  if (Value <> FDepthClearValue) or FInsideList then
  begin
    if FInsideList then
      Include(FListStates[FCurrentList], sttDepthBuffer)
    else
      FDepthClearValue := Value;
    GL.ClearDepth(Value);
  end;

end;

procedure TVKStateCache.SetDepthFunc(const Value: TDepthFunction);
begin
  if (Value <> FDepthFunc) or FInsideList then
  begin
    if FInsideList then
      Include(FListStates[FCurrentList], sttDepthBuffer)
    else
      FDepthFunc := Value;
    GL.DepthFunc(cGLComparisonFunctionToGLEnum[Value]);
  end;

end;

procedure TVKStateCache.SetDepthRange(const ZNear, ZFar: TVKclampd);
begin
  if (ZNear <> FDepthRange[0]) or (ZFar <> FDepthRange[1])
    or FInsideList then
  begin
    if FInsideList then
      Include(FListStates[FCurrentList], sttViewport)
    else
    begin
      FDepthRange[0] := ZNear;
      FDepthRange[1] := ZFar;
    end;
    GL.DepthRange(ZNear, ZFar);
  end;
end;

procedure TVKStateCache.SetDepthRangeFar(const Value: TVKclampd);
begin
  if (Value <> FDepthRange[1]) or FInsideList then
  begin
    if FInsideList then
      Include(FListStates[FCurrentList], sttViewport)
    else
      FDepthRange[1] := Value;
    GL.DepthRange(FDepthRange[0], Value);
  end;
end;

procedure TVKStateCache.SetDepthRangeNear(const Value: TVKclampd);
begin
  if (Value <> FDepthRange[0]) or FInsideList then
  begin
    if FInsideList then
      Include(FListStates[FCurrentList], sttViewport)
    else
      FDepthRange[0] := Value;
    GL.DepthRange(Value, FDepthRange[1]);
  end;
end;

procedure TVKStateCache.SetDepthWriteMask(const Value: TVKboolean);
begin
  if (Value <> FDepthWriteMask) or FInsideList then
  begin
    if FInsideList then
      Include(FListStates[FCurrentList], sttDepthBuffer)
    else
      FDepthWriteMask := Value;
    GL.DepthMask(Value);
  end;
end;

procedure TVKStateCache.SetDrawFrameBuffer(const Value: TVKuint);
begin
  if Value <> FDrawFrameBuffer then
  begin
    FDrawFrameBuffer := Value;
    GL.BindFramebuffer(GL_DRAW_FRAMEBUFFER, Value);
  end;
end;

procedure TVKStateCache.SetEnableBlend(Index: Integer;
  const Value: TVKboolean);
begin
  if FEnableBlend[Index] <> Value then
  begin
    FEnableBlend[Index] := Value;
    if Value then
      GL.Enablei(GL_BLEND, Index)
    else
      GL.Disablei(GL_BLEND, Index);
  end;
end;

procedure TVKStateCache.SetEnableClipDistance(Index: Cardinal;
  const Value: TVKboolean);
begin
  if FEnableClipDistance[Index] <> Value then
  begin
    FEnableClipDistance[Index] := Value;
    if Value then
      GL.Enable(GL_CLIP_DISTANCE0 + Index)
    else
      GL.Disable(GL_CLIP_DISTANCE0 + Index);
  end;
end;

procedure TVKStateCache.SetEnableColorLogicOp(const Value: TVKboolean);
begin
  if Value <> FEnableColorLogicOp then
  begin
    FEnableColorLogicOp := Value;
    if Value then
      GL.Enable(GL_COLOR_LOGIC_OP)
    else
      GL.Disable(GL_COLOR_LOGIC_OP);
  end;
end;

procedure TVKStateCache.SetEnableCullFace(const Value: TVKboolean);
begin

end;

procedure TVKStateCache.SetEnableDepthClamp(const enabled: TVKboolean);
begin

end;

procedure TVKStateCache.SetEnableDepthTest(const Value: TVKboolean);
begin

end;

procedure TVKStateCache.SetEnableDither(const Value: TVKboolean);
begin

end;

procedure TVKStateCache.SetEnableFramebufferSRGB(const Value: TVKboolean);
begin

end;

procedure TVKStateCache.SetEnableLineSmooth(const Value: TVKboolean);
begin

end;

procedure TVKStateCache.SetEnableMultisample(const Value: TVKboolean);
begin

end;

procedure TVKStateCache.SetEnablePolygonOffsetFill(const Value: TVKboolean);
begin

end;

procedure TVKStateCache.SetEnablePolygonOffsetLine(const Value: TVKboolean);
begin

end;

procedure TVKStateCache.SetEnablePolygonOffsetPoint(const Value: TVKboolean);
begin

end;

procedure TVKStateCache.SetEnablePolygonSmooth(const Value: TVKboolean);
begin

end;

procedure TVKStateCache.SetEnableSampleAlphaToCoverage(const Value: TVKboolean);
begin
  if Value <> FEnableSampleAlphaToCoverage then
  begin
    FEnableSampleAlphaToCoverage := Value;
    if Value then
      GL.Enable(GL_SAMPLE_ALPHA_TO_COVERAGE)
    else
      GL.Disable(GL_SAMPLE_ALPHA_TO_COVERAGE);
  end;
end;

procedure TVKStateCache.SetEnableSampleCoverage(const Value: TVKboolean);
begin
  if Value <> FEnableSampleCoverage then
  begin
    FEnableSampleCoverage := Value;
    if Value then
      GL.Enable(GL_SAMPLE_COVERAGE)
    else
      GL.Disable(GL_SAMPLE_COVERAGE);
  end;
end;

procedure TVKStateCache.SetEnableSampleMask(const Value: TVKboolean);
begin
  if Value <> FEnableSampleMask then
  begin
    FEnableSampleMask := Value;
    if Value then
      GL.Enable(GL_SAMPLE_MASK)
    else
      GL.Disable(GL_SAMPLE_MASK);
  end;
end;

procedure TVKStateCache.SetEnableSampleAlphaToOne(const Value: TVKboolean);
begin
  if Value <> FEnableSampleAlphaToOne then
  begin
    FEnableSampleAlphaToOne := Value;
    if Value then
      GL.Enable(GL_SAMPLE_ALPHA_TO_ONE)
    else
      GL.Disable(GL_SAMPLE_ALPHA_TO_ONE);
  end;
end;

procedure TVKStateCache.SetEnableScissorTest(const Value: TVKboolean);
begin

end;

procedure TVKStateCache.SetEnableStencilTest(const Value: TVKboolean);
begin

end;

procedure TVKStateCache.SetFragmentShaderDerivitiveHint(const Value: THintType);
begin
  if Value <> FFragmentShaderDerivitiveHint then
  begin
    if FInsideList then
      Include(FListStates[FCurrentList], sttHint)
    else
      FFragmentShaderDerivitiveHint := Value;
    GL.Hint(GL_FRAGMENT_SHADER_DERIVATIVE_HINT, cGLHintToGLEnum[Value]);
  end;
end;

procedure TVKStateCache.SetMultisampleFilterHint(const Value: THintType);
begin
  if GL.NV_multisample_filter_hint then
    if Value <> FMultisampleFilterHint then
    begin
      if FInsideList then
        Include(FListStates[FCurrentList], sttHint)
      else
        FMultisampleFilterHint := Value;
      GL.Hint(GL_MULTISAMPLE_FILTER_HINT_NV, cGLHintToGLEnum[Value]);
    end;
end;

procedure TVKStateCache.SetFrameBuffer(const Value: TVKuint);
begin
  if (Value <> FDrawFrameBuffer) or (Value <> FReadFrameBuffer)
    or FInsideList then
  begin
    FDrawFrameBuffer := Value;
    FReadFrameBuffer := Value;
    GL.BindFramebuffer(GL_FRAMEBUFFER, Value);
  end;
end;

procedure TVKStateCache.SetFrontFace(const Value: TFaceWinding);
begin
  if (Value <> FFrontFace) or FInsideList then
  begin
    if FInsideList then
      Include(FListStates[FCurrentList], sttPolygon)
    else
      FFrontFace := Value;
    GL.FrontFace(cGLFaceWindingToGLEnum[Value]);
  end;
end;

procedure TVKStateCache.SetGLAlphaFunction(func: TComparisonFunction;
  ref: TVKclampf);
{$IFDEF GLS_CACHE_MISS_CHECK}
var I: TVKuint; E: Single;
{$ENDIF}
begin
  if FForwardContext then
    exit;
{$IFDEF GLS_CACHE_MISS_CHECK}
  GL.GetIntegerv(GL_ALPHA_TEST_FUNC, @I);
  if cGLComparisonFunctionToGLEnum[FAlphaFunc] <> I then
    GLSLogger.LogError(glsStateCashMissing + 'AlphaTest function');
  GL.GetFloatv(GL_ALPHA_TEST_REF, @E);
  if FAlphaRef <> E then
    GLSLogger.LogError(glsStateCashMissing + 'AlphaTest reference');
{$ENDIF}
  if (FAlphaFunc <> func) or (FAlphaRef <> ref)
    or FInsideList then
  begin
    if FInsideList then
      Include(FListStates[FCurrentList], sttColorBuffer)
    else
    begin
      FAlphaFunc := func;
      FAlphaRef := ref;
    end;
    GL.AlphaFunc(cGLComparisonFunctionToGLEnum[func], ref);
  end;
end;

function TVKStateCache.GetColorWriteMask(Index: Integer): TColorMask;
begin
  Result := FColorWriteMask[Index];
end;

function TVKStateCache.GetCurrentQuery(Index: TQueryType): TVKuint;
begin
  Result := FCurrentQuery[Index];
end;

function TVKStateCache.GetCurrentVertexAttrib(Index: Integer): TVector;
begin
  Result := FCurrentVertexAttrib[Index];
end;

function TVKStateCache.GetDepthRangeFar: TVKclampd;
begin
  Result := FDepthRange[1];
end;

function TVKStateCache.GetDepthRangeNear: TVKclampd;
begin
  Result := FDepthRange[0];
end;

function TVKStateCache.GetEnableBlend(Index: Integer): TVKboolean;
begin
  Result := FEnableBlend[Index];
end;

function TVKStateCache.GetEnableClipDistance(
  ClipDistance: Cardinal): TVKboolean;
begin
  Result := FEnableClipDistance[ClipDistance];
end;

function TVKStateCache.GetSampleMaskValue(Index: Integer): TVKbitfield;
begin
  Result := FSampleMaskValue[Index];
end;

function TVKStateCache.GetMaxTextureSize: TVKuint;
begin
  if FMaxTextureSize = 0 then
    GL.GetIntegerv(GL_MAX_TEXTURE_SIZE, @FMaxTextureSize);
  Result := FMaxTextureSize;
end;

function TVKStateCache.GetMaterialAmbient(const aFace: TCullFaceMode): TVector;
begin
  Result := FFrontBackColors[ord(aFace)][1];
end;

function TVKStateCache.GetMaterialDiffuse(const aFace: TCullFaceMode): TVector;
begin
  Result := FFrontBackColors[ord(aFace)][2];
end;

function TVKStateCache.GetMaterialEmission(const aFace: TCullFaceMode): TVector;
begin
  Result := FFrontBackColors[ord(aFace)][0];
end;

function TVKStateCache.GetMaterialShininess(const aFace: TCullFaceMode): Integer;
begin
  Result := FFrontBackShininess[ord(aFace)];
end;

function TVKStateCache.GetMaterialSpecular(const aFace: TCullFaceMode): TVector;
begin
  Result := FFrontBackColors[ord(aFace)][3];
end;

function TVKStateCache.GetMax3DTextureSize: TVKuint;
begin
  if FMax3DTextureSize = 0 then
    GL.GetIntegerv(GL_MAX_3D_TEXTURE_SIZE, @FMax3DTextureSize);
  Result := FMax3DTextureSize;
end;

function TVKStateCache.GetMaxCubeTextureSize: TVKuint;
begin
  if FMaxCubeTextureSize = 0 then
    GL.GetIntegerv(GL_MAX_CUBE_MAP_TEXTURE_SIZE, @FMaxCubeTextureSize);
  Result := FMaxCubeTextureSize;
end;

function TVKStateCache.GetMaxArrayTextureSize: TVKuint;
begin
  if FMaxArrayTextureSize = 0 then
    GL.GetIntegerv(GL_MAX_ARRAY_TEXTURE_LAYERS, @FMaxArrayTextureSize);
  Result := FMaxArrayTextureSize;
end;


function TVKStateCache.GetMaxTextureImageUnits: TVKuint;
begin
  if FMaxTextureImageUnits = 0 then
    GL.GetIntegerv(GL_MAX_TEXTURE_IMAGE_UNITS, @FMaxTextureImageUnits);
  Result := FMaxTextureImageUnits;
end;

function TVKStateCache.GetMaxTextureAnisotropy: TVKuint;
begin
  if (FMaxTextureAnisotropy = 0) and GL.EXT_texture_filter_anisotropic then
    GL.GetIntegerv(GL_MAX_TEXTURE_MAX_ANISOTROPY_EXT, @FMaxTextureAnisotropy);
  Result := FMaxTextureAnisotropy;
end;

function TVKStateCache.GetMaxSamples: TVKuint;
begin
  if (FMaxSamples = 0) and GL.EXT_multisample then
    GL.GetIntegerv(GL_MAX_SAMPLES, @FMaxSamples);
  Result := FMaxSamples;
end;

function TVKStateCache.GetTextureBinding(Index: Integer;
  target: TVKTextureTarget): TVKuint;
begin
  Result := FTextureBinding[Index, target];
end;

function TVKStateCache.GetTextureBindingTime(Index: Integer; target: TVKTextureTarget):
  Double;
begin
  Result := FTextureBindingTime[Index, target];
end;

function TVKStateCache.GetSamplerBinding(Index: TVKuint): TVKuint;
begin
  Result := FSamplerBinding[Index];
end;

procedure TVKStateCache.SetSamplerBinding(Index: TVKuint; const Value: TVKuint);
begin
  if Index > High(FSamplerBinding) then
    exit;
  if (Value <> FSamplerBinding[Index]) or FInsideList then
  begin
    if FInsideList then
      Include(FListStates[FCurrentList], sttTexture)
    else
      FSamplerBinding[Index] := Value;
    GL.BindSampler(Index, Value);
  end;
end;

// SetGLTextureMatrix
//

procedure TVKStateCache.SetGLTextureMatrix(const matrix: TMatrix);
begin
  if FForwardContext then
    exit;
  if FInsideList then
    Include(FListStates[FCurrentList], sttTransform)
  else
    FTextureMatrixIsIdentity[ActiveTexture] := False;
  GL.MatrixMode(GL_TEXTURE);
  GL.LoadMatrixf(PGLFloat(@matrix.V[0].V[0]));
  GL.MatrixMode(GL_MODELVIEW);
end;

// ResetGLTextureMatrix
//

procedure TVKStateCache.ResetGLTextureMatrix;
begin
  if FForwardContext then
    exit;
  GL.MatrixMode(GL_TEXTURE);
  GL.LoadIdentity;
  FTextureMatrixIsIdentity[ActiveTexture] := True;
  GL.MatrixMode(GL_MODELVIEW);
end;

// ResetAllGLTextureMatrix
//

procedure TVKStateCache.ResetAllGLTextureMatrix;
var
  I: Integer;
  lastActiveTexture: TVKuint;
begin
  if FForwardContext then
    exit;
  lastActiveTexture := ActiveTexture;
  GL.MatrixMode(GL_TEXTURE);
  for I := High(FTextureMatrixIsIdentity) downto 0 do
    if not FTextureMatrixIsIdentity[I] then
    begin
      ActiveTexture := I;
      GL.LoadIdentity;
      FTextureMatrixIsIdentity[I] := True;
    end;
  GL.MatrixMode(GL_MODELVIEW);
  ActiveTexture := lastActiveTexture;
end;

procedure TVKStateCache.SetLineSmoothHint(const Value: THintType);
begin
  if (Value <> FLineSmoothHint) or FInsideList then
  begin
    if FInsideList then
      Include(FListStates[FCurrentList], sttHint)
    else
      FLineSmoothHint := Value;
    GL.Hint(GL_LINE_SMOOTH_HINT, cGLHintToGLEnum[Value]);
  end;
end;

procedure TVKStateCache.SetLineWidth(const Value: TVKfloat);
begin
  // note: wide lines no longer deprecated (see OpenGL spec)
  if (Value <> FLineWidth) or FInsideList then
  begin
    if FInsideList then
      Include(FListStates[FCurrentList], sttLine)
    else
      FLineWidth := Value;
    GL.LineWidth(Value);
  end;
end;

procedure TVKStateCache.SetLineStippleFactor(const Value: TVKint);
begin
  if (Value <> FLineStippleFactor) or FInsideList then
  begin
    if FInsideList then
      Include(FListStates[FCurrentList], sttLine)
    else
      FLineStippleFactor := Value;
    GL.LineStipple(Value, FLineStipplePattern);
  end;
end;

procedure TVKStateCache.SetLineStipplePattern(const Value: TVKushort);
begin
  if (Value <> FLineStipplePattern) or FInsideList then
  begin
    if FInsideList then
      Include(FListStates[FCurrentList], sttLine)
    else
      FLineStipplePattern := Value;
    GL.LineStipple(FLineStippleFactor, Value);
  end;
end;

procedure TVKStateCache.SetLogicOpMode(const Value: TLogicOp);
begin
  if (Value <> FLogicOpMode) or FInsideList then
  begin
    if FInsideList then
      Include(FListStates[FCurrentList], sttColorBuffer)
    else
      FLogicOpMode := Value;
    GL.LogicOp(cGLLogicOpToGLEnum[Value]);
  end;
end;

procedure TVKStateCache.SetPackAlignment(const Value: TVKuint);
begin
  if Value <> FPackAlignment then
  begin
    FPackAlignment := Value;
    GL.PixelStoref(GL_PACK_ALIGNMENT, Value);
  end;
end;

procedure TVKStateCache.SetPackImageHeight(const Value: TVKuint);
begin
  if Value <> FPackImageHeight then
  begin
    FPackImageHeight := Value;
    GL.PixelStoref(GL_PACK_IMAGE_HEIGHT, Value);
  end;
end;

procedure TVKStateCache.SetPackLSBFirst(const Value: TVKboolean);
begin
  if Value <> FPackLSBFirst then
  begin
    FPackLSBFirst := Value;
    GL.PixelStorei(GL_PACK_LSB_FIRST, byte(Value));
  end;
end;

procedure TVKStateCache.SetPackRowLength(const Value: TVKuint);
begin
  if Value <> FPackRowLength then
  begin
    FPackRowLength := Value;
    GL.PixelStoref(GL_PACK_ROW_LENGTH, Value);
  end;
end;

procedure TVKStateCache.SetPackSkipImages(const Value: TVKuint);
begin
  if Value <> FPackSkipImages then
  begin
    FPackSkipImages := Value;
    GL.PixelStoref(GL_PACK_SKIP_IMAGES, Value);
  end;
end;

procedure TVKStateCache.SetPackSkipPixels(const Value: TVKuint);
begin
  if Value <> FPackSkipPixels then
  begin
    FPackSkipPixels := Value;
    GL.PixelStoref(GL_PACK_SKIP_PIXELS, Value);
  end;
end;

procedure TVKStateCache.SetPackSkipRows(const Value: TVKuint);
begin
  if Value <> FPackSkipRows then
  begin
    FPackSkipRows := Value;
    GL.PixelStoref(GL_PACK_SKIP_ROWS, Value);
  end;
end;

procedure TVKStateCache.SetPackSwapBytes(const Value: TVKboolean);
begin
  if Value <> FPackSwapBytes then
  begin
    FPackSwapBytes := Value;
    GL.PixelStorei(GL_PACK_SWAP_BYTES, byte(Value));
  end;
end;

procedure TVKStateCache.SetPixelPackBufferBinding(const Value: TVKuint);
begin
  if Value <> FPixelPackBufferBinding then
  begin
    FPixelPackBufferBinding := Value;
    GL.BindBuffer(GL_PIXEL_PACK_BUFFER, Value);
  end;
end;

procedure TVKStateCache.SetPixelUnpackBufferBinding(const Value: TVKuint);
begin
  if Value <> FPixelUnpackBufferBinding then
  begin
    FPixelUnpackBufferBinding := Value;
    GL.BindBuffer(GL_PIXEL_UNPACK_BUFFER, Value);
  end;
end;

procedure TVKStateCache.SetPointFadeThresholdSize(const Value: TVKfloat);
begin
  if (Value <> FPointFadeThresholdSize) or FInsideList then
  begin
    if FInsideList then
      Include(FListStates[FCurrentList], sttPoint)
    else
      FPointFadeThresholdSize := Value;
    GL.PointParameterf(GL_POINT_FADE_THRESHOLD_SIZE, Value);
  end;
end;

procedure TVKStateCache.SetPointSize(const Value: TVKfloat);
begin
  if (Value <> FPointSize) or FInsideList then
  begin
    if FInsideList then
      Include(FListStates[FCurrentList], sttPoint)
    else
      FPointSize := Value;
    GL.PointSize(Value);
  end;
end;

procedure TVKStateCache.SetPointSpriteCoordOrigin(const Value: TVKenum);
begin
  if (Value <> FPointSpriteCoordOrigin) or FInsideList then
  begin
    if FInsideList then
      Include(FListStates[FCurrentList], sttPoint)
    else
      FPointSpriteCoordOrigin := Value;
    GL.PointParameterf(GL_POINT_SPRITE_COORD_ORIGIN, Value);
  end;
end;

procedure TVKStateCache.SetPolygonMode(const Value: TPolygonMode);
begin
  if (Value <> FPolygonMode) or FInsideList then
  begin
    if FInsideList then
      Include(FListStates[FCurrentList], sttPolygon)
    else
    begin
      FPolygonMode := Value;
      FPolygonBackMode := Value;
    end;
    GL.PolygonMode(GL_FRONT_AND_BACK, cGLPolygonModeToGLEnum[Value]);
  end;
end;

procedure TVKStateCache.SetPolygonOffset(const factor, units: TVKfloat);
begin
  if (factor <> FPolygonOffsetFactor) or (units <> FPolygonOffsetUnits)
    or FInsideList then
  begin
    if FInsideList then
      Include(FListStates[FCurrentList], sttPolygon)
    else
    begin
      FPolygonOffsetFactor := factor;
      FPolygonOffsetUnits := units;
    end;
    GL.PolygonOffset(factor, units);
  end;
end;

procedure TVKStateCache.SetPolygonOffsetFactor(const Value: TVKfloat);
begin
  if (Value <> FPolygonOffsetFactor) or FInsideList then
  begin
    if FInsideList then
      Include(FListStates[FCurrentList], sttPolygon)
    else
      FPolygonOffsetFactor := Value;
    GL.PolygonOffset(Value, FPolygonOffsetUnits);
  end;
end;

procedure TVKStateCache.SetPolygonOffsetUnits(const Value: TVKfloat);
begin
  if (Value <> FPolygonOffsetUnits) or FInsideList then
  begin
    if FInsideList then
      Include(FListStates[FCurrentList], sttPolygon)
    else
      FPolygonOffsetUnits := Value;
    GL.PolygonOffset(FPolygonOffsetFactor, Value);
  end;
end;

procedure TVKStateCache.SetPolygonSmoothHint(const Value: THintType);
begin
  if (Value <> FPolygonSmoothHint) or FInsideList then
  begin
    if FInsideList then
      Include(FListStates[FCurrentList], sttHint)
    else
      FPolygonSmoothHint := Value;
    GL.Hint(GL_POLYGON_SMOOTH_HINT, cGLHintToGLEnum[Value]);
  end;
end;

procedure TVKStateCache.SetProvokingVertex(const Value: TVKenum);
begin
  if Value <> FProvokingVertex then
  begin
    FProvokingVertex := Value;
    GL.ProvokingVertex(Value);
  end;
end;

procedure TVKStateCache.SetReadFrameBuffer(const Value: TVKuint);
begin
  if Value <> FReadFrameBuffer then
  begin
    FReadFrameBuffer := Value;
    GL.BindFramebuffer(GL_READ_FRAMEBUFFER, Value);
  end;
end;

procedure TVKStateCache.SetRenderBuffer(const Value: TVKuint);
begin
  if Value <> FRenderBuffer then
  begin
    FRenderBuffer := Value;
    GL.BindRenderbuffer(GL_RENDERBUFFER, Value);
  end;
end;

procedure TVKStateCache.SetSampleCoverage(const Value: TVKfloat;
  invert: TVKboolean);
begin
  if (Value <> FSampleCoverageValue) or (invert <> FSampleCoverageInvert)
    or FInsideList then
  begin
    if FInsideList then
      Include(FListStates[FCurrentList], sttMultisample)
    else
    begin
      FSampleCoverageValue := Value;
      FSampleCoverageInvert := invert;
    end;
    GL.SampleCoverage(Value, invert);
  end;
end;

procedure TVKStateCache.SetSampleCoverageInvert(const Value: TVKboolean);
begin
  if (Value <> FSampleCoverageInvert) or FInsideList then
  begin
    if FInsideList then
      Include(FListStates[FCurrentList], sttMultisample)
    else
      FSampleCoverageInvert := Value;
    GL.SampleCoverage(FSampleCoverageValue, Value);
  end;
end;

procedure TVKStateCache.SetSampleCoverageValue(const Value: TVKfloat);
begin
  if (Value <> FSampleCoverageValue) or FInsideList then
  begin
    if FInsideList then
      Include(FListStates[FCurrentList], sttMultisample)
    else
      FSampleCoverageValue := Value;
    GL.SampleCoverage(Value, FSampleCoverageInvert);
  end;
end;

procedure TVKStateCache.SetSampleMaskValue(Index: Integer;
  const Value: TVKbitfield);
begin
  if (FSampleMaskValue[Index] <> Value) or FInsideList then
  begin
    if FInsideList then
      Include(FListStates[FCurrentList], sttMultisample)
    else
      FSampleMaskValue[Index] := Value;
    GL.SampleMaski(Index, Value);
  end;
end;

procedure TVKStateCache.SetScissorBox(const Value: TVector4i);
begin
  if not VectorEquals(FScissorBox, Value) or FInsideList then
  begin
    if FInsideList then
      Include(FListStates[FCurrentList], sttScissor)
    else
      FScissorBox := Value;
    GL.Scissor(Value.V[0], Value.V[1], Value.V[2], Value.V[3]);
  end;
end;

procedure TVKStateCache.SetStencilBackWriteMask(const Value: TVKuint);
begin
  if (Value <> FStencilBackWriteMask) or FInsideList then
  begin
    if FInsideList then
      Include(FListStates[FCurrentList], sttStencilBuffer)
    else
      FStencilBackWriteMask := Value;
    // DONE: ignore if unsupported
    if GL.VERSION_2_0 then
      GL.StencilMaskSeparate(GL_BACK, Value);
  end;
end;

procedure TVKStateCache.SetStencilClearValue(const Value: TVKuint);
{$IFDEF GLS_CACHE_MISS_CHECK}
var I: TVKuint;
{$ENDIF}
begin
{$IFDEF GLS_CACHE_MISS_CHECK}
  GL.GetIntegerv(GL_STENCIL_CLEAR_VALUE, @I);
  if FStencilClearValue <> I then
    GLSLogger.LogError(glsStateCashMissing + 'Stencil clear value');
{$ENDIF}
  if (Value <> FStencilClearValue) or FInsideList then
  begin
    if FInsideList then
      Include(FListStates[FCurrentList], sttStencilBuffer)
    else
      FStencilClearValue := Value;
    GL.ClearStencil(Value);
  end;
end;

procedure TVKStateCache.SetColorClearValue(const Value: TVector);
begin
  if not VectorEquals(Value, FColorClearValue) or FInsideList then
  begin
    if FInsideList then
      Include(FListStates[FCurrentList], sttColorBuffer)
    else
      FColorClearValue := Value;
    GL.ClearColor(Value.V[0], Value.V[1], Value.V[2], Value.V[3]);
  end;
end;

procedure TVKStateCache.SetColorMask(mask: TColorMask);
var
  i: integer;
begin
  // it might be faster to keep track of whether all draw buffers are same
  // value or not, since using this is probably more common than setting
  // the color write mask for individual draw buffers
  if FInsideList then
    Include(FListStates[FCurrentList], sttColorBuffer)
  else
    for I := low(FColorWriteMask) to high(FColorWriteMask) do
    begin
      FColorWriteMask[I] := mask;
    end;
  GL.ColorMask(ccRed in mask, ccGreen in mask, ccBlue in mask, ccAlpha in mask);
end;

procedure TVKStateCache.SetStencilFuncSeparate(const face: TCullFaceMode;
  const func: TStencilFunction; const ref: TVKint; const mask: TVKuint);
{$IFDEF GLS_CACHE_MISS_CHECK}
var UI: TVKuint; I: TVKint;
{$ENDIF}
begin
//  if (func<>FStencilFunc) or (ref<>FStencilRef) or (mask<>FStencilValueMask)
//    or FInsideList then
{$IFDEF GLS_CACHE_MISS_CHECK}
  GL.GetIntegerv(GL_STENCIL_FUNC, @UI);
  if cGLComparisonFunctionToGLEnum[FStencilFunc] <> UI then
    GLSLogger.LogError(glsStateCashMissing + 'Stencil function');
  GL.GetIntegerv(GL_STENCIL_REF, @I);
  if FStencilRef <> I then
    GLSLogger.LogError(glsStateCashMissing + 'Stencil reference');
    GLSLogger.LogError(glsStateCashMissing + 'Stencil function');
  GL.GetIntegerv(GL_STENCIL_VALUE_MASK, @UI);
  if FStencilValueMask <> UI then
    GLSLogger.LogError(glsStateCashMissing + 'Stencil value mask');
{$ENDIF}
  begin
    if FInsideList then
      Include(FListStates[FCurrentList], sttStencilBuffer)
    else
      case face of
        cmFront:
          begin
            FStencilFunc := func;
            FStencilRef := ref;
            FStencilValueMask := mask;
          end;
        cmBack:
          begin
            FStencilBackFunc := func;
            FStencilBackRef := ref;
            FStencilBackValueMask := mask;
          end;
        cmFrontAndBack:
          begin
            FStencilFunc := func;
            FStencilRef := ref;
            FStencilValueMask := mask;
            FStencilBackFunc := func;
            FStencilBackRef := ref;
            FStencilBackValueMask := mask;
          end;
      end;

    GL.StencilFuncSeparate(cGLCullFaceModeToGLEnum[face],
      cGLComparisonFunctionToGLEnum[func], ref, mask);
  end;
end;

procedure TVKStateCache.SetStencilFunc(const func: TStencilFunction; const ref:
  TVKint; const mask: TVKuint);
begin
  if (func <> FStencilFunc) or (ref <> FStencilRef) or (mask <>
    FStencilValueMask) or FInsideList then
  begin
    if FInsideList then
      Include(FListStates[FCurrentList], sttStencilBuffer)
    else
    begin
      FStencilFunc := func;
      FStencilRef := ref;
      FStencilValueMask := mask;
    end;
    GL.StencilFunc(cGLComparisonFunctionToGLEnum[func], ref, mask);
  end;
end;

procedure TVKStateCache.SetStencilOp(const fail, zfail, zpass: TStencilOp);
{$IFDEF GLS_CACHE_MISS_CHECK}
var I: TVKuint;
{$ENDIF}
begin
{$IFDEF GLS_CACHE_MISS_CHECK}
  GL.GetIntegerv(GL_STENCIL_FAIL, @I);
  if cGLStencilOpToGLEnum[FStencilFail] <> I then
    GLSLogger.LogError(glsStateCashMissing + 'Stencil fail');
  GL.GetIntegerv(GL_STENCIL_PASS_DEPTH_FAIL, @I);
  if cGLStencilOpToGLEnum[FStencilPassDepthFail] <> I then
    GLSLogger.LogError(glsStateCashMissing + 'Stencil zfail');
  GL.GetIntegerv(GL_STENCIL_PASS_DEPTH_PASS, @I);
  if cGLStencilOpToGLEnum[FStencilPassDepthPass] <> I then
    GLSLogger.LogError(glsStateCashMissing + 'Stencil zpass');
{$ENDIF}
  if (fail <> FStencilFail) or (zfail <> FStencilPassDepthFail)
    or (zpass <> FStencilPassDepthPass) or FInsideList then
  begin
    if FInsideList then
      Include(FListStates[FCurrentList], sttStencilBuffer)
    else
    begin
      FStencilFail := fail;
      FStencilPassDepthFail := zfail;
      FStencilPassDepthPass := zpass;
    end;
    GL.StencilOp(cGLStencilOpToGLEnum[fail],
      cGLStencilOpToGLEnum[zfail],
      cGLStencilOpToGLEnum[zpass]);
  end;
end;

procedure TVKStateCache.SetStencilOpSeparate(const face: TCullFaceMode;
  const sfail, dpfail, dppass: TStencilOp);
begin
  if FInsideList then
    Include(FListStates[FCurrentList], sttStencilBuffer)
  else
    case face of
      cmFront:
        begin
          FStencilFail := sfail;
          FStencilPassDepthFail := dpfail;
          FStencilPassDepthPass := dppass;
        end;
      cmBack:
        begin
          FStencilBackFail := sfail;
          FStencilBackPassDepthFail := dpfail;
          FStencilBackPassDepthPass := dppass;
        end;
      cmFrontAndBack:
        begin
          FStencilFail := sfail;
          FStencilPassDepthFail := dpfail;
          FStencilPassDepthPass := dppass;
          FStencilBackFail := sfail;
          FStencilBackPassDepthFail := dpfail;
          FStencilBackPassDepthPass := dppass;
        end;
    end;

  GL.StencilOpSeparate(cGLCullFaceModeToGLEnum[face],
    cGLStencilOpToGLEnum[sfail],
    cGLStencilOpToGLEnum[dpfail],
    cGLStencilOpToGLEnum[dppass]);
end;

procedure TVKStateCache.SetStencilWriteMask(const Value: TVKuint);
{$IFDEF GLS_CACHE_MISS_CHECK}
var I: TVKuint;
{$ENDIF}
begin
{$IFDEF GLS_CACHE_MISS_CHECK}
  GL.GetIntegerv(GL_STENCIL_WRITEMASK, @I);
  if FStencilWriteMask <> I then
    GLSLogger.LogError(glsStateCashMissing + 'Stencil write mask');
{$ENDIF}
  if (Value <> FStencilWriteMask) or FInsideList then
  begin
    if FInsideList then
      Include(FListStates[FCurrentList], sttStencilBuffer)
    else
      FStencilWriteMask := Value;
    GL.StencilMaskSeparate(GL_FRONT, Value);
  end;
end;

procedure TVKStateCache.SetTextureBinding(Index: Integer; target:
  TVKTextureTarget;
  const Value: TVKuint);
var
  lastActiveTexture: TVKuint;
begin
  if target = ttNoShape then
    exit;
  if (Value <> FTextureBinding[Index, target]) or FInsideList then
  begin
    if FInsideList then
      Include(FListStates[FCurrentList], sttTexture)
    else
      FTextureBinding[Index, target] := Value;
    lastActiveTexture := ActiveTexture;
    ActiveTexture := Index;
    GL.BindTexture(cGLTexTypeToGLEnum[target], Value);
    ActiveTexture := lastActiveTexture;
  end;
  FTextureBindingTime[Index, target] := GLSTime;
end;

function TVKStateCache.GetActiveTextureEnabled(Target: TVKTextureTarget):
  Boolean;
begin
  Result := FActiveTextureEnabling[FActiveTexture][Target];
end;

procedure TVKStateCache.SetActiveTextureEnabled(Target: TVKTextureTarget;
  const Value: Boolean);
var
  glTarget: TVKEnum;
begin
  glTarget := DecodeGLTextureTarget(Target);
  if FForwardContext or not IsTargetSupported(glTarget) then
    exit;
  if (Value <> FActiveTextureEnabling[FActiveTexture][Target])
    or FInsideList then
  begin
    if FInsideList then
      Include(FListStates[FCurrentList], sttEnable)
    else
      FActiveTextureEnabling[FActiveTexture][Target] := Value;
    if Value then
      GL.Enable(glTarget)
    else
      GL.Disable(glTarget);
  end;
end;

procedure TVKStateCache.SetTextureCompressionHint(const Value: THintType);
begin
  if (Value <> FTextureCompressionHint) or FInsideList then
  begin
    if FInsideList then
      Include(FListStates[FCurrentList], sttHint)
    else
      FTextureCompressionHint := Value;
    GL.Hint(GL_TEXTURE_COMPRESSION_HINT, cGLHintToGLEnum[Value]);
  end;
end;

procedure TVKStateCache.SetTransformFeedbackBufferBinding(const Value: TVKuint);
begin
  if (Value <> FTransformFeedbackBufferBinding) or FInsideList then
  begin
    FTransformFeedbackBufferBinding := Value;
    GL.BindBuffer(GL_TRANSFORM_FEEDBACK_BUFFER, Value);
  end;
end;

procedure TVKStateCache.SetEnableTextureCubeMapSeamless(const Value:
  TVKboolean);
begin
  if Value <> FEnableTextureCubeMapSeamless then
  begin
    FEnableTextureCubeMapSeamless := Value;
    if Value = true then
      GL.Enable(GL_TEXTURE_CUBE_MAP_SEAMLESS)
    else
      GL.Disable(GL_TEXTURE_CUBE_MAP_SEAMLESS);
  end;
end;

procedure TVKStateCache.NewList(list: TVKuint; mode: TVKEnum);
var
  I: TVKuint;
begin
  Assert(mode = GL_COMPILE,
    'Compile & executing not supported by TVKStateCache');
  FCurrentList := list - 1;
  while High(FListStates) < Integer(FCurrentList) do
    SetLength(FListStates, 2 * Length(FListStates));

  FListStates[FCurrentList] := [];
  FInsideList := True;
  // Reset VBO binding and client attribute
  with GL do
  begin
    if ARB_vertex_buffer_object then
    begin
      ArrayBufferBinding := 0;
      ElementBufferBinding := 0;
      for I := 0 to 15 do
        DisableVertexAttribArray(I);
    end;
    NewList(list, mode);
  end;
end;

procedure TVKStateCache.EndList;
begin
  GL.EndList;
  FInsideList := False;
end;

procedure TVKStateCache.CallList(list: TVKuint);
begin
  while High(FListStates) < Integer(list) do
    SetLength(FListStates, 2 * Length(FListStates));

  if FListStates[list - 1] <> [] then
  begin
    PushAttrib(FListStates[list - 1]);
    GL.CallList(list);
    PopAttrib;
  end
  else
    GL.CallList(list);
end;

procedure TVKStateCache.SetUniformBufferBinding(const Value: TVKuint);
begin
  Assert(not FInsideList);
  if Value <> FUniformBufferBinding then
  begin
    FUniformBufferBinding := Value;
    GL.BindBuffer(GL_UNIFORM_BUFFER, Value);
  end;
end;

procedure TVKStateCache.SetBufferIndexedBinding(const Value: TVKuint;
  ATarget: TVKBufferBindingTarget; AIndex: TVKuint; ABufferSize: TVKsizeiptr);
begin
  Assert(not FInsideList);
  if (FUBOStates[ATarget, AIndex].FUniformBufferBinding <> Value)
    or (FUBOStates[ATarget, AIndex].FOffset > 0)
    or (FUBOStates[ATarget, AIndex].FSize <> ABufferSize) then
  begin
    case ATarget of
      bbtUniform: FUniformBufferBinding := Value;
      bbtTransformFeedBack: FTransformFeedbackBufferBinding := Value;
    end;
    FUBOStates[ATarget, AIndex].FUniformBufferBinding := Value;
    FUBOStates[ATarget, AIndex].FOffset := 0;
    FUBOStates[ATarget, AIndex].FSize := ABufferSize;
    GL.BindBufferBase(cGLBufferBindingTarget[ATarget], AIndex, Value);
  end
  else
    case ATarget of
      bbtUniform: SetUniformBufferBinding(Value);
      bbtTransformFeedBack: SetTransformFeedbackBufferBinding(Value);
    end;
end;

procedure TVKStateCache.SetBufferIndexedBinding(const Value: TVKuint;
  ATarget: TVKBufferBindingTarget; AIndex: TVKuint;
    AOffset: TVKintptr; ARangeSize: TVKsizeiptr);
begin
  Assert(not FInsideList);
  if (FUBOStates[ATarget, AIndex].FUniformBufferBinding <> Value)
    or (FUBOStates[ATarget, AIndex].FOffset <> AOffset)
    or (FUBOStates[ATarget, AIndex].FSize <> ARangeSize) then
  begin
    case ATarget of
      bbtUniform: FUniformBufferBinding := Value;
      bbtTransformFeedBack: FTransformFeedbackBufferBinding := Value;
    end;
    FUBOStates[ATarget, AIndex].FUniformBufferBinding := Value;
    FUBOStates[ATarget, AIndex].FOffset := AOffset;
    FUBOStates[ATarget, AIndex].FSize := ARangeSize;
    GL.BindBufferRange(cGLBufferBindingTarget[ATarget], AIndex, Value, AOffset, ARangeSize);
  end;
end;

function TVKStateCache.GetMaxTextureUnits: TVKuint;
begin
  if FMaxTextureUnits = 0 then
    GL.GetIntegerv(GL_MAX_TEXTURE_IMAGE_UNITS_ARB, @FMaxTextureUnits);
  Result := FMaxTextureUnits;
end;

procedure TVKStateCache.SetUnpackAlignment(const Value: TVKuint);
begin
  if Value <> FUnpackAlignment then
  begin
    FUnpackAlignment := Value;
    GL.PixelStoref(GL_UNPACK_ALIGNMENT, Value);
  end;
end;

procedure TVKStateCache.SetUnpackImageHeight(const Value: TVKuint);
begin
  if Value <> FUnpackImageHeight then
  begin
    FUnpackImageHeight := Value;
    GL.PixelStoref(GL_UNPACK_IMAGE_HEIGHT, Value);
  end;
end;

procedure TVKStateCache.SetUnpackLSBFirst(const Value: TVKboolean);
begin
  if Value <> FUnpackLSBFirst then
  begin
    FUnpackLSBFirst := Value;
    GL.PixelStorei(GL_UNPACK_LSB_FIRST, byte(Value));
  end;
end;

procedure TVKStateCache.SetUnpackRowLength(const Value: TVKuint);
begin
  if Value <> FUnpackRowLength then
  begin
    FUnpackRowLength := Value;
    GL.PixelStoref(GL_UNPACK_ROW_LENGTH, Value);
  end;
end;

procedure TVKStateCache.SetUnpackSkipImages(const Value: TVKuint);
begin
  if Value <> FUnpackSkipImages then
  begin
    FUnpackSkipImages := Value;
    GL.PixelStoref(GL_UNPACK_SKIP_IMAGES, Value);
  end;
end;

procedure TVKStateCache.SetUnpackSkipPixels(const Value: TVKuint);
begin
  if Value <> FUnpackSkipPixels then
  begin
    FUnpackSkipPixels := Value;
    GL.PixelStoref(GL_UNPACK_SKIP_PIXELS, Value);
  end;
end;

procedure TVKStateCache.SetUnpackSkipRows(const Value: TVKuint);
begin
  if Value <> FUnpackSkipRows then
  begin
    FUnpackSkipRows := Value;
    GL.PixelStoref(GL_UNPACK_SKIP_ROWS, Value);
  end;
end;

procedure TVKStateCache.SetUnpackSwapBytes(const Value: TVKboolean);
begin
  if Value <> FUnpackSwapBytes then
  begin
    FUnpackSwapBytes := Value;
    GL.PixelStorei(GL_UNPACK_SWAP_BYTES, byte(Value));
  end;
end;

procedure TVKStateCache.SetViewPort(const Value: TVector4i);
begin
  if not VectorEquals(Value, FViewPort) or FInsideList then
  begin
    if FInsideList then
      Include(FListStates[FCurrentList], sttViewport)
    else
      FViewPort := Value;
    GL.Viewport(Value.V[0], Value.V[1], Value.V[2], Value.V[3]);
  end;
end;

procedure TVKStateCache.SetFFPLight(Value: Boolean);
begin
  FFFPLight := Value and not FForwardContext;
end;

function TVKStateCache.GetMaxLights: Integer;
begin
  if FMaxLights = 0 then
  if FForwardContext then
    FMaxLights := MAX_HARDWARE_LIGHT
  else
    GL.GetIntegerv(GL_MAX_LIGHTS, @FMaxLights);
  Result := FMaxLights;
end;

function TVKStateCache.GetLightEnabling(I: Integer): Boolean;
begin
  Result := FLightEnabling[I];
end;

procedure TVKStateCache.SetLightEnabling(I: Integer; Value: Boolean);
var
  J, K: Integer;
begin
  if (FLightEnabling[I] <> Value) or FInsideList then
  begin
    if FInsideList then
      Include(FListStates[FCurrentList], sttLighting)
    else
      FLightEnabling[I] := Value;

    if FFFPLight then
    begin
      if Value then
        GL.Enable(GL_LIGHT0 + I)
      else
        GL.Disable(GL_LIGHT0 + I);
    end;

    K := 0;
    for J := 0 to MAX_HARDWARE_LIGHT - 1 do
    if FLightEnabling[J] then
    begin
      FLightIndices[K] := J;
      Inc(K);
    end;
    FLightNumber := K;

    FShaderLightStatesChanged := True;
    if Assigned(FOnLightsChanged) then
      FOnLightsChanged(Self);
  end;
end;

function TVKStateCache.GetLightIndicesAsAddress: PGLInt;
begin
  Result := @FLightIndices[0];
end;

function TVKStateCache.GetLightStateAsAddress: Pointer;
var
  I, J, C: Integer;
begin
  C := MinInteger(FLightNumber, MAX_SHADER_LIGHT);
  if FShaderLightStatesChanged then
  begin
    if C > 0 then
    begin
      if GL.VERSION_3_0 then
      begin
        Move(FLightStates.Position,
          FShaderLightStates.Position,
          SizeOf(FShaderLightStates.Position));
        Move(FLightStates.Ambient,
         FShaderLightStates.Ambient,
         SizeOf(FShaderLightStates.Ambient));
        Move(FLightStates.Diffuse,
          FShaderLightStates.Diffuse,
          SizeOf(FShaderLightStates.Diffuse));
        Move(FLightStates.Specular,
          FShaderLightStates.Specular,
          SizeOf(FShaderLightStates.Specular));
        Move(FLightStates.SpotDirection,
          FShaderLightStates.SpotDirection,
          SizeOf(FShaderLightStates.SpotDirection));
        Move(FLightStates.SpotCosCutoffExponent,
          FShaderLightStates.SpotCosCutoffExponent,
          SizeOf(FShaderLightStates.SpotCosCutoffExponent));
        Move(FLightStates.Attenuation,
          FShaderLightStates.Attenuation,
          SizeOf(FShaderLightStates.Attenuation));
      end
      else
      begin
        for I := C - 1 downto 0 do
        begin
          J := FLightIndices[I];
          FShaderLightStates.Position[I] := FLightStates.Position[J];
          FShaderLightStates.Ambient[I] := FLightStates.Ambient[J];
          FShaderLightStates.Diffuse[I] := FLightStates.Diffuse[J];
          FShaderLightStates.Specular[I] := FLightStates.Specular[J];
          FShaderLightStates.SpotDirection[I] := FLightStates.SpotDirection[J];
          FShaderLightStates.SpotCosCutoffExponent[I] := FLightStates.SpotCosCutoffExponent[J];
          FShaderLightStates.Attenuation[I] := FLightStates.Attenuation[J];
        end;
      end;
    end
    else
      FillChar(FShaderLightStatesChanged, SizeOf(FShaderLightStatesChanged), $00);
    FShaderLightStatesChanged := False;
  end;

  Result := @FShaderLightStates;
end;

function TVKStateCache.GetLightPosition(I: Integer): TVector;
begin
  Result := FLightStates.Position[I];
end;

procedure TVKStateCache.SetLightPosition(I: Integer; const Value: TVector);
begin
  if not VectorEquals(Value, FLightStates.Position[I]) then
  begin
    FLightStates.Position[I] := Value;
    FShaderLightStatesChanged := True;
    if Assigned(FOnLightsChanged) then
      FOnLightsChanged(Self);
  end;
end;

function TVKStateCache.GetLightSpotDirection(I: Integer): TAffineVector;
begin
  Result := AffineVectorMake(FLightStates.SpotDirection[I]);
end;

procedure TVKStateCache.SetLightSpotDirection(I: Integer; const Value: TAffineVector);
begin
  if not VectorEquals(Value, AffineVectorMake(FLightStates.SpotDirection[I])) then
  begin
    FLightStates.SpotDirection[I] := VectorMake(Value);
    FShaderLightStatesChanged := True;
    if Assigned(FOnLightsChanged) then
      FOnLightsChanged(Self);
  end;
end;

function TVKStateCache.GetLightAmbient(I: Integer): TVector;
begin
  Result := FLightStates.Ambient[I];
end;

procedure TVKStateCache.SetLightAmbient(I: Integer; const Value: TVector);
begin
  if not VectorEquals(Value, FLightStates.Ambient[I]) or FInsideList then
  begin
    if FInsideList then
      Include(FListStates[FCurrentList], sttLighting)
    else
      FLightStates.Ambient[I] := Value;

    if FFFPLight then
      GL.Lightfv(GL_LIGHT0 + I, GL_AMBIENT, @Value);

    FShaderLightStatesChanged := True;
    if Assigned(FOnLightsChanged) then
      FOnLightsChanged(Self);
  end;
end;

function TVKStateCache.GetLightDiffuse(I: Integer): TVector;
begin
  Result := FLightStates.Diffuse[I];
end;

procedure TVKStateCache.SetLightDiffuse(I: Integer; const Value: TVector);
begin
  if not VectorEquals(Value, FLightStates.Diffuse[I]) or FInsideList then
  begin
    if FInsideList then
      Include(FListStates[FCurrentList], sttLighting)
    else
      FLightStates.Diffuse[I] := Value;

    if FFFPLight then
      GL.Lightfv(GL_LIGHT0 + I, GL_DIFFUSE, @Value);

    FShaderLightStatesChanged := True;
    if Assigned(FOnLightsChanged) then
      FOnLightsChanged(Self);
  end;
end;

function TVKStateCache.GetLightSpecular(I: Integer): TVector;
begin
  Result := FLightStates.Specular[I];
end;

procedure TVKStateCache.SetLightSpecular(I: Integer; const Value: TVector);
begin
  if not VectorEquals(Value, FLightStates.Specular[I]) or FInsideList then
  begin
    if FInsideList then
      Include(FListStates[FCurrentList], sttLighting)
    else
      FLightStates.Specular[I] := Value;

    if FFFPLight then
      GL.Lightfv(GL_LIGHT0 + I, GL_SPECULAR, @Value);

    FShaderLightStatesChanged := True;
    if Assigned(FOnLightsChanged) then
      FOnLightsChanged(Self);
  end;
end;

function TVKStateCache.GetSpotCutoff(I: Integer): Single;
begin
  Result := FSpotCutoff[I];
end;

procedure TVKStateCache.SetSpotCutoff(I: Integer; const Value: Single);
begin
  if (Value <> FSpotCutoff[I]) or FInsideList then
  begin
    if FInsideList then
      Include(FListStates[FCurrentList], sttLighting)
    else
    begin
      FSpotCutoff[I] := Value;
      FLightStates.SpotCosCutoffExponent[I].V[0] := cos(DegToRadian(Value));
    end;
	
    if FFFPLight then
      GL.Lightfv(GL_LIGHT0 + I, GL_SPOT_CUTOFF, @Value);
                  
    FShaderLightStatesChanged := True;
    if Assigned(FOnLightsChanged) then
      FOnLightsChanged(Self);
  end;
end;

function TVKStateCache.GetSpotExponent(I: Integer): Single;
begin
  Result := FLightStates.SpotCosCutoffExponent[I].V[1];
end;

procedure TVKStateCache.SetSpotExponent(I: Integer; const Value: Single);
begin
  if (Value <> FLightStates.SpotCosCutoffExponent[I].V[1] )
    or FInsideList then
  begin
    if FInsideList then
      Include(FListStates[FCurrentList], sttLighting)
    else
      FLightStates.SpotCosCutoffExponent[I].V[1]  := Value;

    if FFFPLight then
      GL.Lightfv(GL_LIGHT0 + I, GL_SPOT_EXPONENT, @Value);

    FShaderLightStatesChanged := True;
    if Assigned(FOnLightsChanged) then
      FOnLightsChanged(Self);
  end;
end;

function TVKStateCache.GetConstantAtten(I: Integer): Single;
begin
  Result := FLightStates.Attenuation[I].V[0] ;
end;

procedure TVKStateCache.SetConstantAtten(I: Integer; const Value: Single);
begin
  if (Value <> FLightStates.Attenuation[I].V[0] ) or FInsideList then
  begin
    if FInsideList then
      Include(FListStates[FCurrentList], sttLighting)
    else
      FLightStates.Attenuation[I].V[0]  := Value;

    if FFFPLight then
      GL.Lightfv(GL_LIGHT0 + I, GL_CONSTANT_ATTENUATION, @Value);

    FShaderLightStatesChanged := True;
    if Assigned(FOnLightsChanged) then
      FOnLightsChanged(Self);
  end;
end;

function TVKStateCache.GetLinearAtten(I: Integer): Single;
begin
  Result := FLightStates.Attenuation[I].V[1] ;
end;

procedure TVKStateCache.SetLinearAtten(I: Integer; const Value: Single);
begin
  if (Value <> FLightStates.Attenuation[I].V[1] ) or FInsideList then
  begin
    if FInsideList then
      Include(FListStates[FCurrentList], sttLighting)
    else
      FLightStates.Attenuation[I].V[1]  := Value;

    if FFFPLight then
      GL.Lightfv(GL_LIGHT0 + I, GL_LINEAR_ATTENUATION, @Value);

    FShaderLightStatesChanged := True;
    if Assigned(FOnLightsChanged) then
      FOnLightsChanged(Self);
  end;
end;

function TVKStateCache.GetQuadAtten(I: Integer): Single;
begin
  Result := FLightStates.Attenuation[I].V[2] ;
end;

procedure TVKStateCache.SetQuadAtten(I: Integer; const Value: Single);
begin
  if (Value <> FLightStates.Attenuation[I].V[2] ) or FInsideList then
  begin
    if FInsideList then
      Include(FListStates[FCurrentList], sttLighting)
    else
      FLightStates.Attenuation[I].V[2]  := Value;

    if FFFPLight then
      GL.Lightfv(GL_LIGHT0 + I, GL_QUADRATIC_ATTENUATION, @Value);

    FShaderLightStatesChanged := True;
    if Assigned(FOnLightsChanged) then
      FOnLightsChanged(Self);
  end;
end;

procedure TVKStateCache.SetForwardContext(Value: Boolean);
begin
  if Value <> FForwardContext then
  begin
    FForwardContext := Value;
    if Value then
    begin
      SetFFPlight(False);
    end;
  end;
end;


// SetGLColorIgnoring
//

procedure TVKStateCache.SetGLColorWriting(flag: Boolean);
begin
  if (FColorWriting <> flag) or FInsideList then
  begin
    if FInsideList then
      Include(FListStates[FCurrentList], sttColorBuffer)
    else
      FColorWriting := flag;
    GL.ColorMask(flag, flag, flag, flag);
  end;
end;

// InvertGLFrontFace
//

procedure TVKStateCache.InvertGLFrontFace;
begin
  if FFrontFace = fwCounterClockWise then
    FrontFace := fwClockWise
  else
    FrontFace := fwCounterClockWise;
end;

// SetGLState
//
procedure TVKStateCache.SetGLState(const aState : TVKState);
begin
	Enable(aState);
end;

// UnSetGLState
//
procedure TVKStateCache.UnSetGLState(const aState : TVKState);
begin
	Disable(aState);
end;

// ResetGLPolygonMode
//

procedure TVKStateCache.ResetGLPolygonMode;
begin
  GL.PolygonMode(GL_FRONT_AND_BACK, GL_FILL);
  FPolygonMode := pmFill;
  FPolygonBackMode := pmFill;
end;

// ResetGLMaterialColors
//

procedure TVKStateCache.ResetGLMaterialColors;
begin
  GL.Materialfv(GL_FRONT_AND_BACK, GL_AMBIENT, @clrGray20);
  GL.Materialfv(GL_FRONT_AND_BACK, GL_DIFFUSE, @clrGray80);
  GL.Materialfv(GL_FRONT_AND_BACK, GL_SPECULAR, @clrBlack);
  GL.Materialfv(GL_FRONT_AND_BACK, GL_EMISSION, @clrBlack);
  GL.Materiali(GL_FRONT_AND_BACK, GL_SHININESS, 0);
  FillChar(FFrontBackColors, SizeOf(FFrontBackColors), 127);
  FFrontBackShininess[0] := 0;
  FFrontBackShininess[1] := 0;
end;

// ResetGLTexture
//

procedure TVKStateCache.ResetGLTexture(const TextureUnit: Integer);
var
  t: TVKTextureTarget;
  glTarget: TVKEnum;
begin
  GL.ActiveTexture(GL_TEXTURE0 + TextureUnit);
  for t := Low(TVKTextureTarget) to High(TVKTextureTarget) do
  begin
    glTarget := DecodeGLTextureTarget(t);
    if IsTargetSupported(glTarget) then
    begin
      GL.BindTexture(glTarget, 0);
      FTextureBinding[TextureUnit, t] := 0;
    end;
  end;
  GL.ActiveTexture(GL_TEXTURE0);
  FActiveTexture := 0;
end;

// ResetGLCurrentTexture
//

procedure TVKStateCache.ResetGLCurrentTexture;
var
  a: TVKint;
  t: TVKTextureTarget;
  glTarget: TVKEnum;
begin
  if GL.ARB_multitexture then
  begin
    for a := MaxTextureImageUnits - 1 to 0 do
    begin
      GL.ActiveTexture(GL_TEXTURE0 + a);
      for t := Low(TVKTextureTarget) to High(TVKTextureTarget) do
      begin
        glTarget := DecodeGLTextureTarget(t);
        if IsTargetSupported(glTarget) then
        begin
          GL.BindTexture(glTarget, 0);
          FTextureBinding[a, t] := 0;
        end;
      end;
    end;
  end
  else
    for t := Low(TVKTextureTarget) to High(TVKTextureTarget) do
    begin
      glTarget := DecodeGLTextureTarget(t);
      if IsTargetSupported(glTarget) then
      begin
        GL.BindTexture(glTarget, 0);
        FTextureBinding[0, t] := 0;
      end;
    end;
end;

// ResetGLFrontFace
//

procedure TVKStateCache.ResetGLFrontFace;
begin
  GL.FrontFace(GL_CCW);
  FFrontFace := fwCounterClockWise;
end;


procedure TVKStateCache.SetGLFrontFaceCW;
begin
  if FFrontFace = fwCounterClockWise then
  begin
    GL.FrontFace(GL_CW);
    FFrontFace := fwClockWise;
  end;
end;

// ResetAll
//

procedure TVKStateCache.ResetAll;
begin
 {$WARN SYMBOL_DEPRECATED OFF}
  ResetGLPolygonMode;
  ResetGLMaterialColors;
  ResetGLCurrentTexture;
  ResetGLFrontFace;
 {$WARN SYMBOL_DEPRECATED ON}
end;

end.

