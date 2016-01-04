//
// This unit is part of the DGLEngine Project, http://glscene.org
//
{ : GLState<p>

  Tools for managing an application-side cache of OpenGL state.<p>

  <b>History : </b><font size=-1><ul>
  <li>21/12/15 - JD - Imported and Improved from GLScene
  </ul></font><p>
}

// TODO: Proper client-side pushing + popping of state, in OpenGL 3+ contexts,
// rather than using glPushAttrib + glPopAttrib.
// TODO: Proper support for textures, taking into account that they probably
// won't be linked to texture units in some future version of OpenGL.
// TODO: Once more of GLScene is cache-aware, enable some of the checks before
// changing OpenGL state (where we will gain a speed increase).
// TODO: improve binding objects to binding points
// TODO: decide how to implement the new Enable* options (without going above
// 32 elements in sets if possible, which would be slower in 32bit Delphi)

unit DGLState;

interface

{$I DGLEngine.inc}
{ .$DEFINE GLS_CACHE_MISS_CHECK }

uses
  System.Classes, System.SysUtils,
  // GLS
  DGLCrossPlatform,
  DGLTypes,
  dglOpenGL,
  DGLVectorTypes,
  DGLVectorMaths,
  DGLTextureFormat,
  DGLSLog;

const
  GLS_VERTEX_ATTR_NUM = 16;

Type
  TLightSourceState = packed record
    Position: array [0 .. 15] of TVector;
    Ambient: array [0 .. 15] of TVector;
    Diffuse: array [0 .. 15] of TVector;
    Specular: array [0 .. 15] of TVector;
    SpotDirection: array [0 .. 15] of TVector;
    SpotCosCutoffExponent: array [0 .. 15] of TVector;
    Attenuation: array [0 .. 15] of TVector;
  end;

  TShaderLightSourceState = packed record
    Position: array [0 .. 15] of TVector;
    Ambient: array [0 .. 15] of TVector;
    Diffuse: array [0 .. 15] of TVector;
    Specular: array [0 .. 15] of TVector;
    SpotDirection: array [0 .. 15] of TVector;
    SpotCosCutoffExponent: array [0 .. 15] of TVector;
    Attenuation: array [0 .. 15] of TVector;
  end;

  TOnLightsChanged = procedure(Sender: TObject);

  // TDGLStateCache
  //
  { : Manages an application-side cache of OpenGL states and parameters.<p>
    Purpose of this class is to eliminate redundant state and parameter
    changes, and there will typically be no more than one state cache per
    OpenGL context. }
  TDGLStateCache = class
  private
    { Private Declarations }

    // Legacy state
    FFrontBackColors:    array [0 .. 1, 0 .. 3] of TVector;
    FFrontBackShininess: array [0 .. 1] of Integer;
    // FAlphaFunc: TComparisonFunction;
    // FAlphaRef: TDGLclampf;
    // FPolygonBackMode: TPolygonMode; // Front + back have same polygon mode

    // Lighting state
    FMaxLights:                GLuint;
    FLightEnabling:            array [0 .. 15] of Boolean;
    FLightIndices:             array [0 .. 15] of TGLint;
    FLightNumber:              Integer;
    FLightStates:              TLightSourceState;
    FSpotCutoff:               array [0 .. 15] of Single;
    FShaderLightStates:        TShaderLightSourceState;
    FShaderLightStatesChanged: Boolean;

    FColorWriting: Boolean; // TODO: change to per draw buffer (FColorWriteMask)

    FStates:      TDGLStates;
    FListStates:  array of TDGLStateTypes;
    FCurrentList: TGLuint;
    // FTextureMatrixIsIdentity: array[0..3] of Boolean;
    // FForwardContext: Boolean;

    FFFPLight:           Boolean;
    FcurrentShaderLevel: TDGLShaderMaterialLevel;

    // Texture state
    FMaxTextureSize:       TGLuint;
    FMax3DTextureSize:     TGLuint;
    FMaxCubeTextureSize:   TGLuint;
    FMaxArrayTextureSize:  TGLuint;
    FMaxTextureImageUnits: TGLuint;
    FMaxTextureAnisotropy: TGLuint;
    FMaxSamples:           TGLuint;
    FTextureBinding:       array [0 .. 47, TDGLTextureTarget] of TGLuint;
    FTextureBindingTime:   array [0 .. 47, TDGLTextureTarget] of Double;
    FSamplerBinding:       array [0 .. 47] of TGLuint;

    // Active texture state
    FActiveTexture:         TGLint; // 0 .. Max_texture_units
    FActiveTextureEnabling: array [0 .. 47, TDGLTextureTarget] of Boolean;

    // Vertex Array Data state
    FVertexArrayBinding:   TGLuint;
    FArrayBufferBinding:   TGLuint;
    FElementBufferBinding: TGLuint;
    FTextureBufferBinding: TGLuint;
    // FEnablePrimitiveRestart: TGLBoolean;
    // FPrimitiveRestartIndex: TGLuint;

    // Transformation state
    FViewPort:           TVector4i;
    FDepthRange:         array [0 .. 1] of TGLclampd;
    FEnableClipDistance: array [0 .. 7] of TGLBoolean;
    FEnableDepthClamp:   TGLBoolean;

    // Coloring state
    FClampReadColor:  TGLEnum; // GL_FIXED_ONLY
    FProvokingVertex: TGLEnum; // GL_LAST_VERTEX_CONVENTION

    // Rasterization state
    FPointSize:              TGLfloat;
    FPointFadeThresholdSize: TGLfloat;
    FPointSpriteCoordOrigin: TGLEnum; // GL_UPPER_LEFT
    FLineWidth:              Single;
    FLineStippleFactor:      TGLint;
    FLineStipplePattern:     TGLushort;

    FEnableLineSmooth:         TGLBoolean;
    FEnableCullFace:           TGLBoolean;
    FCullFaceMode:             TCullFaceMode;
    FFrontFace:                TFaceWinding;
    FEnablePolygonSmooth:      TGLBoolean;
    FPolygonMode:              TPolygonMode;
    FPolygonOffsetFactor:      TGLfloat;
    FPolygonOffsetUnits:       TGLfloat;
    FEnablePolygonOffsetPoint: TGLBoolean;
    FEnablePolygonOffsetLine:  TGLBoolean;
    FEnablePolygonOffsetFill:  TGLBoolean;

    // Multisample state
    FEnableMultisample:           TGLBoolean;
    FEnableSampleAlphaToCoverage: TGLBoolean;
    FEnableSampleAlphaToOne:      TGLBoolean;
    FEnableSampleCoverage:        TGLBoolean;
    FSampleCoverageValue:         TGLfloat;
    FSampleCoverageInvert:        TGLBoolean;
    FEnableSampleMask:            TGLBoolean;
    FSampleMaskValue:             array [0 .. 15] of TGLbitfield;

    // Pixel operation state
    FEnableScissorTest: TGLBoolean;
    FScissorBox:        TVector;

    FEnableStencilTest: TGLBoolean;

    FStencilFunc:          TStencilFunction;
    FStencilValueMask:     TGLuint;
    FStencilRef:           TGLint;
    FStencilFail:          TStencilOp;
    FStencilPassDepthFail: TStencilOp;
    FStencilPassDepthPass: TStencilOp;

    FStencilBackFunc:          TStencilFunction;
    FStencilBackValueMask:     TGLuint;
    FStencilBackRef:           TGLuint;
    FStencilBackFail:          TStencilOp;
    FStencilBackPassDepthPass: TStencilOp;
    FStencilBackPassDepthFail: TStencilOp;

    FEnableDepthTest: TGLBoolean;
    FDepthFunc:       TDepthFunction;

    FEnableBlend: array [0 .. 15] of TGLBoolean;

    FBlendSrcRGB:   TBlendFunction;
    FBlendSrcAlpha: TBlendFunction;
    FBlendDstRGB:   TDstBlendFunction;
    FBlendDstAlpha: TDstBlendFunction;

    FBlendEquationRGB:   TBlendEquation;
    FBlendEquationAlpha: TBlendEquation;
    FBlendColor:         TVector;

    FEnableFramebufferSRGB: TGLBoolean;
    FEnableDither:          TGLBoolean;
    FEnableColorLogicOp:    TGLBoolean;

    FLogicOpMode: TLogicOp;

    // Framebuffer control state
    FColorWriteMask:       array [0 .. 15] of TColorMask;
    FDepthWriteMask:       TGLBoolean;
    FStencilWriteMask:     TGLuint;
    FStencilBackWriteMask: TGLuint;
    FColorClearValue:      TVector;
    FDepthClearValue:      TGLfloat;
    FStencilClearValue:    TGLuint;

    // Framebuffer state
    FDrawFrameBuffer: TGLuint;
    FReadFrameBuffer: TGLuint;

    // Renderbuffer state
    FRenderBuffer: TGLuint;

    // Pixels state
    FUnpackSwapBytes:   TGLBoolean;
    FUnpackLSBFirst:    TGLBoolean;
    FUnpackImageHeight: TGLuint;
    FUnpackSkipImages:  TGLuint;
    FUnpackRowLength:   TGLuint;
    FUnpackSkipRows:    TGLuint;
    FUnpackSkipPixels:  TGLuint;
    FUnpackAlignment:   TGLuint;
    FPackSwapBytes:     TGLBoolean;
    FPackLSBFirst:      TGLBoolean;
    FPackImageHeight:   TGLuint;
    FPackSkipImages:    TGLuint;
    FPackRowLength:     TGLuint;
    FPackSkipRows:      TGLuint;
    FPackSkipPixels:    TGLuint;
    FPackAlignment:     TGLuint;

    FPixelPackBufferBinding:   TGLuint;
    FPixelUnpackBufferBinding: TGLuint;

    // Program state
    FCurrentProgram:       TGLuint;
    FMaxTextureUnits:      TGLuint;
    FUniformBufferBinding: TGLuint;
    FUBOStates:            array [TDGLBufferBindingTarget, 0 .. 83] of TUBOStates;

    // Vector + Geometry Shader state
    FCurrentVertexAttrib:    array [0 .. 15] of TVector;
    FEnableProgramPointSize: TGLBoolean;

    // Transform Feedback state
    FTransformFeedbackBufferBinding: TGLuint;

    // Hints state
    FTextureCompressionHint:       THintType;
    FPolygonSmoothHint:            THintType;
    FFragmentShaderDerivitiveHint: THintType;
    FLineSmoothHint:               THintType;
    FMultisampleFilterHint:        THintType;

    // Misc state
    FCurrentQuery:                 array [TQueryType] of TGLuint;
    FCopyReadBufferBinding:        TGLuint;
    FCopyWriteBufferBinding:       TGLuint;
    FEnableTextureCubeMapSeamless: TGLBoolean;
    FInsideList:                   Boolean;

    FOnLightsChanged: TOnLightsChanged;

    // STANDARD OBJECT'S SHADER'S UNIFORMS attributes
    FattrPosition, FattrNormal, FattrVertexColor, FattrTexCoord0,
    FattrTexCoord1, FattrTexCoord2, FattrTexCoord3, FattrTangent, FattrBinormal, FattrIndex: TDGLSLAttribute;

    // FuniformModelMatrix,    ---> cf PIPELINETRANSFORMATION
    // FuniformViewProjectionMatrix,
    FuniformModelMatrix,FuniformViewProjectionMatrix,FuniformDiffuse,
    FuniformDiffuseColor, FuniformAmbientColor, FuniformSpecularColor, FuniformEnvColor,
    FuniformTexUnit0, FuniformTexUnit1, FuniformTexUnit2, FuniformTexUnit3, FuniformTexUnit4,
    FuniformTexUnit5, FuniformTexUnit6, FuniformTexUnit7,
    FuniformInstanceID: TDGLSLUniform;

  protected
    { Protected Declarations }
    // Vertex Array Data state
    procedure SetVertexArrayBinding(const Value: TGLuint);
    function GetArrayBufferBinding: TGLuint;
    procedure SetArrayBufferBinding(const Value: TGLuint);
    function GetElementBufferBinding: TGLuint;
    procedure SetElementBufferBinding(const Value: TGLuint);
    // function GetEnablePrimitiveRestart: TGLBoolean;
    // function GetPrimitiveRestartIndex: TGLuint;
    // procedure SetEnablePrimitiveRestart(const enabled: TGLBoolean);
    // procedure SetPrimitiveRestartIndex(const index: TGLuint);
    procedure SetTextureBufferBinding(const Value: TGLuint);
    // Transformation state
    procedure SetViewPort(const Value: TVector4i);
    function GetEnableClipDistance(ClipDistance: Cardinal): TGLBoolean;
    procedure SetEnableClipDistance(Index: Cardinal; const Value: TGLBoolean);
    function GetDepthRangeFar: TGLclampd;
    procedure SetDepthRangeFar(const Value: TGLclampd);
    function GetDepthRangeNear: TGLclampd;
    procedure SetDepthRangeNear(const Value: TGLclampd);
    procedure SetEnableDepthClamp(const enabled: TGLBoolean);
    // Coloring state
    procedure SetClampReadColor(const Value: TGLEnum);
    procedure SetProvokingVertex(const Value: TGLEnum);
    // Rasterization state
    procedure SetPointSize(const Value: TGLfloat);
    procedure SetPointFadeThresholdSize(const Value: TGLfloat);
    procedure SetPointSpriteCoordOrigin(const Value: TGLEnum);
    procedure SetLineWidth(const Value: TGLfloat);
    // procedure SetLineStippleFactor(const Value: TGLint);
    // procedure SetLineStipplePattern(const Value: TDGLushort);

    procedure SetEnableLineSmooth(const Value: TGLBoolean);
    procedure SetEnableCullFace(const Value: TGLBoolean);
    procedure SetCullFaceMode(const Value: TCullFaceMode);
    procedure SetFrontFace(const Value: TFaceWinding);
    procedure SetEnablePolygonSmooth(const Value: TGLBoolean);
    procedure SetPolygonMode(const Value: TPolygonMode);
    procedure SetPolygonOffsetFactor(const Value: TGLfloat);
    procedure SetPolygonOffsetUnits(const Value: TGLfloat);
    procedure SetEnablePolygonOffsetPoint(const Value: TGLBoolean);
    procedure SetEnablePolygonOffsetLine(const Value: TGLBoolean);
    procedure SetEnablePolygonOffsetFill(const Value: TGLBoolean);
    // Multisample state
    procedure SetEnableMultisample(const Value: TGLBoolean);
    procedure SetEnableSampleAlphaToCoverage(const Value: TGLBoolean);
    procedure SetEnableSampleAlphaToOne(const Value: TGLBoolean);
    procedure SetEnableSampleCoverage(const Value: TGLBoolean);
    procedure SetSampleCoverageValue(const Value: TGLfloat);
    procedure SetSampleCoverageInvert(const Value: TGLBoolean);
    procedure SetEnableSampleMask(const Value: TGLBoolean);
    function GetSampleMaskValue(Index: Integer): TGLbitfield;
    procedure SetSampleMaskValue(Index: Integer; const Value: TGLbitfield);

    // Texture state
    function GetMaxTextureSize: TGLuint;
    function GetMax3DTextureSize: TGLuint;
    function GetMaxCubeTextureSize: TGLuint;
    function GetMaxArrayTextureSize: TGLuint;
    function GetMaxTextureImageUnits: TGLuint;
    function GetMaxTextureAnisotropy: TGLuint;
    function GetMaxSamples: TGLuint;
    function GetTextureBinding(Index: Integer; target: TDGLTextureTarget): TGLuint;
    function GetTextureBindingTime(Index: Integer; target: TDGLTextureTarget): Double;
    procedure SetTextureBinding(Index: Integer; target: TDGLTextureTarget; const Value: TGLuint);
    function GetActiveTextureEnabled(target: TDGLTextureTarget): Boolean;
    procedure SetActiveTextureEnabled(target: TDGLTextureTarget; const Value: Boolean);
    function GetSamplerBinding(Index: TGLuint): TGLuint;
    procedure SetSamplerBinding(Index: TGLuint; const Value: TGLuint);
    // Active texture
    procedure SetActiveTexture(const Value: TGLint);

    // Pixel operations
    procedure SetEnableScissorTest(const Value: TGLBoolean);
    procedure SetScissorBox(const Value: TVector);
    procedure SetEnableStencilTest(const Value: TGLBoolean);
    procedure SetEnableDepthTest(const Value: TGLBoolean);
    procedure SetDepthFunc(const Value: TDepthFunction);
    function GetEnableBlend(Index: Integer): TGLBoolean;
    procedure SetEnableBlend(Index: Integer; const Value: TGLBoolean);
    procedure SetBlendColor(const Value: TVector);
    procedure SetEnableFramebufferSRGB(const Value: TGLBoolean);
    procedure SetEnableDither(const Value: TGLBoolean);
    procedure SetEnableColorLogicOp(const Value: TGLBoolean);
    procedure SetLogicOpMode(const Value: TLogicOp);
    // Framebuffer control
    function GetColorWriteMask(Index: Integer): TColorMask;
    procedure SetColorWriteMask(Index: Integer; const Value: TColorMask);
    procedure SetDepthWriteMask(const Value: TGLBoolean);
    procedure SetStencilWriteMask(const Value: TGLuint);
    procedure SetStencilBackWriteMask(const Value: TGLuint);
    procedure SetColorClearValue(const Value: TVector);
    procedure SetDepthClearValue(const Value: TGLfloat);
    procedure SetStencilClearValue(const Value: TGLuint);
    // Framebuffer
    procedure SetDrawFrameBuffer(const Value: TGLuint);
    procedure SetReadFrameBuffer(const Value: TGLuint);
    // Renderbuffer
    procedure SetRenderBuffer(const Value: TGLuint);
    // Pixels
    procedure SetUnpackSwapBytes(const Value: TGLBoolean);
    procedure SetUnpackLSBFirst(const Value: TGLBoolean);
    procedure SetUnpackImageHeight(const Value: TGLuint);
    procedure SetUnpackSkipImages(const Value: TGLuint);
    procedure SetUnpackRowLength(const Value: TGLuint);
    procedure SetUnpackSkipRows(const Value: TGLuint);
    procedure SetUnpackSkipPixels(const Value: TGLuint);
    procedure SetUnpackAlignment(const Value: TGLuint);
    procedure SetPackSwapBytes(const Value: TGLBoolean);
    procedure SetPackLSBFirst(const Value: TGLBoolean);
    procedure SetPackImageHeight(const Value: TGLuint);
    procedure SetPackSkipImages(const Value: TGLuint);
    procedure SetPackRowLength(const Value: TGLuint);
    procedure SetPackSkipRows(const Value: TGLuint);
    procedure SetPackSkipPixels(const Value: TGLuint);
    procedure SetPackAlignment(const Value: TGLuint);
    procedure SetPixelPackBufferBinding(const Value: TGLuint);
    procedure SetPixelUnpackBufferBinding(const Value: TGLuint);
    // Program
    procedure SetCurrentProgram(const Value: TGLuint);
    procedure SetUniformBufferBinding(const Value: TGLuint);
    function GetMaxTextureUnits: TGLuint;
    // Vector + Geometry Shader state
    function GetCurrentVertexAttrib(Index: Integer): TVector;
    procedure SetCurrentVertexAttrib(Index: Integer; const Value: TVector);
    procedure SetEnableProgramPointSize(const Value: TGLBoolean);
    // Transform Feedback state
    procedure SetTransformFeedbackBufferBinding(const Value: TGLuint);
    // Hints
    procedure SetLineSmoothHint(const Value: THintType);
    procedure SetPolygonSmoothHint(const Value: THintType);
    procedure SetTextureCompressionHint(const Value: THintType);
    procedure SetFragmentShaderDerivitiveHint(const Value: THintType);
    procedure SetMultisampleFilterHint(const Value: THintType);
    // Misc
    function GetCurrentQuery(Index: TQueryType): TGLuint;
    // procedure SetCurrentQuery(Index: TQueryType; const Value: TGLuint);
    procedure SetCopyReadBufferBinding(const Value: TGLuint);
    procedure SetCopyWriteBufferBinding(const Value: TGLuint);
    procedure SetEnableTextureCubeMapSeamless(const Value: TGLBoolean);

    // Ligting  ------> DEPRECATED
    procedure SetFFPLight(Value: Boolean);
    // function GetMaxLights: Integer;
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
    // procedure SetForwardContext(Value: Boolean);

  public
    { Public Declarations }
    constructor Create; virtual;
    destructor Destroy; override;

    procedure PushAttrib(stateTypes: TDGLStateTypes);
    procedure PopAttrib();

    procedure Enable(const aState: TDGLState);
    procedure Disable(const aState: TDGLState);
    procedure PerformEnable(const aState: TDGLState);
    procedure PerformDisable(const aState: TDGLState);

    // procedure SetGLState(const aState : TDGLState);
    // procedure UnSetGLState(const aState : TDGLState);
    procedure ResetGLPolygonMode;
    procedure ResetGLMaterialColors;
    procedure ResetGLTexture(const TextureUnit: Integer);
    procedure ResetGLCurrentTexture;
    procedure ResetGLFrontFace;
    procedure SetGLFrontFaceCW;
    procedure ResetAll;

    { : Adjusts material colors for a face. }
    procedure SetGLMaterialColors(const aFace: TCullFaceMode; const emission, Ambient, Diffuse, Specular: TVector; const shininess: Integer);
    function MaterialEmission(const aFace: TCullFaceMode): TVector;
    function MaterialAmbient(const aFace: TCullFaceMode): TVector;
    function MaterialDiffuse(const aFace: TCullFaceMode): TVector;
    function MaterialSpecular(const aFace: TCullFaceMode): TVector;
    function MaterialShininess(const aFace: TCullFaceMode): Integer;

    { : Lighting states }
    property FixedFunctionPipeLight: Boolean read FFFPLight write SetFFPLight;
    // property MaxLights: Integer read GetMaxLights;
    property LightEnabling[Index: Integer]: Boolean read GetLightEnabling write SetLightEnabling;
    property LightPosition[Index: Integer]: TVector read GetLightPosition write SetLightPosition;
    property LightSpotDirection[Index: Integer]: TAffineVector read GetLightSpotDirection write SetLightSpotDirection;
    property LightAmbient[Index: Integer]: TVector read GetLightAmbient write SetLightAmbient;
    property LightDiffuse[Index: Integer]: TVector read GetLightDiffuse write SetLightDiffuse;
    property LightSpecular[Index: Integer]: TVector read GetLightSpecular write SetLightSpecular;
    property LightSpotCutoff[Index: Integer]: Single read GetSpotCutoff write SetSpotCutoff;
    property LightSpotExponent[Index: Integer]: Single read GetSpotExponent write SetSpotExponent;
    property LightConstantAtten[Index: Integer]: Single read GetConstantAtten write SetConstantAtten;
    property LightLinearAtten[Index: Integer]: Single read GetLinearAtten write SetLinearAtten;
    property LightQuadraticAtten[Index: Integer]: Single read GetQuadAtten write SetQuadAtten;
    function GetLightIndicesAsAddress: PGLInt;
    function GetLightStateAsAddress: Pointer;
    property LightNumber: Integer read FLightNumber;
    property OnLightsChanged: TOnLightsChanged read FOnLightsChanged write FOnLightsChanged;

    { : Blending states }
    // procedure SeTDGLAlphaFunction(func: TComparisonFunction; ref: TDGLclampf);

    // Vertex Array Data state
    { : The currently bound array buffer (calling glVertexAttribPointer
      locks this buffer to the currently bound VBO). }
    property VertexArrayBinding: TGLuint read FVertexArrayBinding write SetVertexArrayBinding;
    { : The currently bound vertex buffer object (VAO). }
    property ArrayBufferBinding: TGLuint read GetArrayBufferBinding write SetArrayBufferBinding;
    { : The currently bound element buffer object (EBO). }
    property ElementBufferBinding: TGLuint read GetElementBufferBinding write SetElementBufferBinding;
    { : Determines whether primitive restart is turned on or off. }
    // property EnablePrimitiveRestart: TGLBoolean read GetEnablePrimitiveRestart write SetEnablePrimitiveRestart;
    // { @HTML ( The index Value that causes a primitive restart. }
    // property PrimitiveRestartIndex: TGLuint read GetPrimitiveRestartIndex write  SetPrimitiveRestartIndex;
    { : The currently bound texture buffer object (TBO). }
    property TextureBufferBinding: TGLuint read FTextureBufferBinding write SetTextureBufferBinding;

    // Transformation state
    { : The viewport. }
    property ViewPort: TVector4i read FViewPort write SetViewPort;
    { : Modifies the near + far clipping planes. }
    procedure SetDepthRange(const ZNear, ZFar: TGLclampd);
    { : The near clipping plane distance. }
    property DepthRangeNear: TGLclampd read GetDepthRangeNear write SetDepthRangeNear;
    { : The far clipping plane distance. }
    property DepthRangeFar: TGLclampd read GetDepthRangeFar write SetDepthRangeFar;
    { : Enables/Disables each of the clip distances, used in shaders. }
    property EnableClipDistance[Index: Cardinal]: TGLBoolean read GetEnableClipDistance write SetEnableClipDistance;
    { : Enables/Disables depth clamping. }
    property EnableDepthClamp: TGLBoolean read FEnableDepthClamp write SetEnableDepthClamp;

    // Coloring state
    { : Controls read color clamping. }
    property ClampReadColor: TGLEnum read FClampReadColor write SetClampReadColor;
    { : The provoking vertex used in flat shading.  All the vertices of each
      primitive will the same value determined by this property. }
    property ProvokingVertex: TGLEnum read FProvokingVertex write SetProvokingVertex;

    // Rasterization state
    { : The default point size, used when EnableProgramPointSize = false. }
    property PointSize: TGLfloat read FPointSize write SetPointSize;
    { : If multisampling is enabled, this can control when points are faded out. }
    property PointFadeThresholdSize: TGLfloat read FPointFadeThresholdSize write SetPointFadeThresholdSize;
    { : The texture coordinate origin of point sprites. }
    property PointSpriteCoordOrigin: TGLEnum read FPointSpriteCoordOrigin write SetPointSpriteCoordOrigin;
    { : The line width. }
    property LineWidth: TGLfloat read FLineWidth write SetLineWidth;

    { : Enable/Disable line smoothing. }
    property EnableLineSmooth: TGLBoolean read FEnableLineSmooth write SetEnableLineSmooth;
    { : Enable/Disable face culling. }
    property EnableCullFace: TGLBoolean read FEnableCullFace write SetEnableCullFace;
    { : Selects which faces to cull: front, back or front+back. }
    property CullFaceMode: TCullFaceMode read FCullFaceMode write SetCullFaceMode;
    { : The winding direction that indicates a front facing primitive. }
    property FrontFace: { TGLEnum } TFaceWinding read FFrontFace write SetFrontFace;
    // Enables/Disables polygon smoothing.
    property EnablePolygonSmooth: TGLBoolean read FEnablePolygonSmooth write SetEnablePolygonSmooth;
    { : Whether polygons appear filled, lines or points. }
    property PolygonMode: TPolygonMode read FPolygonMode write SetPolygonMode;
    { : Scales the maximum depth of the polygon. }
    property PolygonOffsetFactor: TGLfloat read FPolygonOffsetFactor write SetPolygonOffsetFactor;
    { : Scales an implementation-dependent constant that relates to the usable
      resolution of the depth buffer. }
    property PolygonOffsetUnits: TGLfloat read FPolygonOffsetUnits write SetPolygonOffsetUnits;
    { : Set polygon offset. }
    procedure SetPolygonOffset(const factor, units: TGLfloat);
    { : Enable/Disable polygon offset for polygons in point mode. }
    property EnablePolygonOffsetPoint: TGLBoolean read FEnablePolygonOffsetPoint write SetEnablePolygonOffsetPoint;
    { : Enable/Disable polygon offset for polygons in line mode. }
    property EnablePolygonOffsetLine: TGLBoolean read FEnablePolygonOffsetLine write SetEnablePolygonOffsetLine;
    { : Enable/Disable polygon offset for polygons in fill mode. }
    property EnablePolygonOffsetFill: TGLBoolean read FEnablePolygonOffsetFill write SetEnablePolygonOffsetFill;

    // Multisample state
    { : Enable/Disable multisampling. }
    property EnableMultisample: TGLBoolean read FEnableMultisample write SetEnableMultisample;
    { : Enable/Disable sample alpha to coverage. }
    property EnableSampleAlphaToCoverage: TGLBoolean read FEnableSampleAlphaToCoverage write SetEnableSampleAlphaToCoverage;
    { : Enable/Disable sample alpha to one. }
    property EnableSampleAlphaToOne: TGLBoolean read FEnableSampleAlphaToOne write SetEnableSampleAlphaToOne;
    { : Enable/Disable sample coverage. }
    property EnableSampleCoverage: TGLBoolean read FEnableSampleCoverage write SetEnableSampleCoverage;
    { : Sample coverage Value. }
    property SampleCoverageValue: TGLfloat read FSampleCoverageValue write SetSampleCoverageValue;
    { : Inverts sample coverage Value. }
    property SampleCoverageInvert: TGLBoolean read FSampleCoverageInvert write SetSampleCoverageInvert;
    { : Set sample coverage. }
    procedure SetSampleCoverage(const Value: TGLfloat; invert: TGLBoolean); { : Enable/Disable sample mask. }
    property EnableSampleMask: TGLBoolean read FEnableSampleMask write SetEnableSampleMask;
    { : Sample mask values. }
    property SampleMaskValue[Index: Integer]: TGLbitfield read GetSampleMaskValue write SetSampleMaskValue;

    // Pixel operations
    { : Enables/Disables scissor test. }
    property EnableScissorTest: TGLBoolean read FEnableScissorTest write SetEnableScissorTest;
    { : The bounding box used in scissor test. }
    property ScissorBox: TVector read FScissorBox write SetScissorBox;
    { : Enables/Disables stencil test. }
    property EnableStencilTest: TGLBoolean read FEnableStencilTest write SetEnableStencilTest;
    { : The stencil function.  Determines the comparison function to be used
      when comparing the reference + stored stencil values. }
    property StencilFunc: TStencilFunction read FStencilFunc;
    // write SetStencilFunc;
    { : The stencil value mask.  Masks both the reference + stored stencil
      values. }
    property StencilValueMask: TGLuint read FStencilValueMask;
    // write SetStencilValueMask;
    { : The stencil reference value.  Clamped to 0..255 with an 8 bit stencil. }
    property StencilRef: TGLint read FStencilRef; // write SetStencilRef;
    { : The operation to perform when stencil test fails. }
    property StencilFail: TStencilOp read FStencilFail; // write SetStencilFail;
    { : The operation to perform when stencil test passes + depth test fails. }
    property StencilPassDepthFail: TStencilOp read FStencilPassDepthFail;
    // write SetStencilPassDepthFail;
    { : The operation to perform when stencil test passes + depth test passes. }
    property StencilPassDepthPass: TStencilOp read FStencilPassDepthPass;
    // write SetStencilPassDepthPass;

    { : The stencil back function.  Determines the comparison function to be
      used when comparing the reference + stored stencil values on back
      facing primitives. }
    property StencilBackFunc: TStencilFunction read FStencilBackFunc;
    // write SetStencilBackFunc;
    { : The stencil back value mask.  Masks both the reference + stored stencil
      values. }
    property StencilBackValueMask: TGLuint read FStencilBackValueMask;
    // write SetStencilBackValueMask;
    { : The stencil back reference value.  Clamped to 0..255 with an 8 bit
      stencil. }
    property StencilBackRef: TGLuint read FStencilBackRef;
    // write SetStencilBackRef;
    { : The operation to perform when stencil test fails on back facing
      primitives. }
    property StencilBackFail: TStencilOp read FStencilBackFail;
    // write SetStencilBackFail;
    { : The operation to perform when stencil test passes + depth test fails on
      back facing primitives. }
    property StencilBackPassDepthFail: TStencilOp read FStencilBackPassDepthFail;
    // write SetStencilBackPassDepthFail;
    { : The operation to perform when stencil test passes + depth test passes on
      back facing primitives. }
    property StencilBackPassDepthPass: TStencilOp read FStencilBackPassDepthPass;
    // write SetStencilBackPassDepthPass;
    { : Used to set stencil Function, Reference + Mask values, for both front +
      back facing primitives. }
    procedure SetStencilFunc(const func: TStencilFunction; const ref: TGLint; const mask: TGLuint);
    { : Used to set stencil Function, Reference + Mask values for either the
      front or back facing primitives (or both, which is the same as calling
      SetStencilFunc). }
    procedure SetStencilFuncSeparate(const face: TCullFaceMode; const func: TStencilFunction; const ref: TGLint; const mask: TGLuint);
    { : Used to set the StencilFail, StencilPassDepthFail + StencilPassDepthPass
      in one go. }
    procedure SetStencilOp(const fail, zfail, zpass: TStencilOp);
    { : Used to set the StencilFail, StencilPassDepthFail + StencilPassDepthPass
      in one go, for either front or back facing primitives. }
    procedure SetStencilOpSeparate(const face: TCullFaceMode; const sfail, dpfail, dppass: TStencilOp);

    { : Enables/disables depth testing. }
    property EnableDepthTest: TGLBoolean read FEnableDepthTest write SetEnableDepthTest;
    { : The depth function.  Used to determine whether to keep a fragment or
      discard it, depending on the current value stored in the depth buffer. }
    property DepthFunc: TDepthFunction read FDepthFunc write SetDepthFunc;
    { : Enables/disables blending for each draw buffer. }
    property EnableBlend[Index: Integer]: TGLBoolean read GetEnableBlend write SetEnableBlend;
    { : The weighting factor used in blending equation, for source RGB. }
    property BlendSrcRGB: TBlendFunction read FBlendSrcRGB;
    // write SetBlendSrcRGB;
    { : The weighting factor used in blending equation, for source alpha. }
    property BlendSrcAlpha: TBlendFunction read FBlendSrcAlpha;
    // write SetBlendSrcAlpha;
    { : The weighting factor used in blending equation, for destination RGB. }
    property BlendDstRGB: TDstBlendFunction read FBlendDstRGB;
    // write SetBlendDstRGB;
    { : The weighting factor used in blending equation, for destination alpha. }
    property BlendDstAlpha: TDstBlendFunction read FBlendDstAlpha;
    // write SetBlendDstAlpha;
    { : Sets the weighting factors to be used by the blending equation, for
      both color + alpha. }
    procedure SetBlendFunc(const Src: TBlendFunction; const Dst: TDstBlendFunction);
    { : Sets the weighting factors to be used by the blending equation, with
      separate values used for color + alpha components. }
    procedure SetBlendFuncSeparate(const SrcRGB: TBlendFunction; const DstRGB: TDstBlendFunction; const SrcAlpha: TBlendFunction; const DstAlpha: TDstBlendFunction);
    { : The blending equation.  Determines how the incoming source fragment's
      RGB are combined with the destination RGB. }
    property BlendEquationRGB: TBlendEquation read FBlendEquationRGB;
    // write SetBlendEquationRGB;
    { : The blending equation.  Determines how the incoming source fragment's
      alpha values are combined with the destination alpha values. }
    property BlendEquationAlpha: TBlendEquation read FBlendEquationAlpha;
    // write SetBlendEquationAlpha;
    { : Sets the blend equation for RGB + alpha to the same value. }
    procedure SetBlendEquation(const mode: TBlendEquation);
    { : Sets the blend equations for RGB + alpha separately. }
    procedure SetBlendEquationSeparate(const modeRGB, modeAlpha: TBlendEquation);
    { : A constant blend color, that can be used in the blend equation. }
    property BlendColor: TVector read FBlendColor write SetBlendColor;
    { : Enables/disables framebuffer SRGB. }
    property EnableFramebufferSRGB: TGLBoolean read FEnableFramebufferSRGB write SetEnableFramebufferSRGB;
    { : Enables/disables dithering. }
    property EnableDither: TGLBoolean read FEnableDither write SetEnableDither;
    { : Enables/disables color logic op. }
    property EnableColorLogicOp: TGLBoolean read FEnableColorLogicOp write SetEnableColorLogicOp;
    { : Logic op mode. }
    property LogicOpMode: TLogicOp read FLogicOpMode write SetLogicOpMode;

    // Framebuffer control
    { : The color write mask, for each draw buffer. }
    property ColorWriteMask[Index: Integer]: TColorMask read GetColorWriteMask write SetColorWriteMask;
    { : Set the color write mask for all draw buffers. }
    procedure SetColorMask(mask: TColorMask);
    { : The depth write mask. }
    property DepthWriteMask: TGLBoolean read FDepthWriteMask write SetDepthWriteMask;
    { : The stencil write mask. }
    property StencilWriteMask: TGLuint read FStencilWriteMask write SetStencilWriteMask;
    { : The stencil back write mask. }
    property StencilBackWriteMask: TGLuint read FStencilBackWriteMask write SetStencilBackWriteMask;
    { : The color clear value. }
    property ColorClearValue: TVector read FColorClearValue write SetColorClearValue;
    { : The depth clear value. }
    property DepthClearValue: TGLfloat read FDepthClearValue write SetDepthClearValue;
    { : The stencil clear value. }
    property StencilClearValue: TGLuint read FStencilClearValue write SetStencilClearValue;

    // Textures
    { : Textures bound to each texture unit + binding point. }
    property TextureBinding[Index: Integer; target: TDGLTextureTarget]: TGLuint read GetTextureBinding write SetTextureBinding;
    property TextureBindingTime[Index: Integer; target: TDGLTextureTarget]: Double read GetTextureBindingTime;
    property ActiveTextureEnabled[target: TDGLTextureTarget]: Boolean read GetActiveTextureEnabled write SetActiveTextureEnabled;
    property SamplerBinding[Index: TGLuint]: TGLuint read GetSamplerBinding write SetSamplerBinding;
    property MaxTextureSize: TGLuint read GetMaxTextureSize;
    property Max3DTextureSize: TGLuint read GetMax3DTextureSize;
    property MaxCubeTextureSize: TGLuint read GetMaxCubeTextureSize;
    property MaxArrayTextureSize: TGLuint read GetMaxArrayTextureSize;
    property MaxTextureImageUnits: TGLuint read GetMaxTextureImageUnits;
    property MaxTextureAnisotropy: TGLuint read GetMaxTextureAnisotropy;
    property MaxSamples: TGLuint read GetMaxSamples;
    // TODO: GL_TEXTURE_BUFFER_DATA_STORE_BINDING ?

    // Active texture
    { : The active texture unit.  Valid values are 0 .. Max texture units. }
    property ActiveTexture: TGLint read FActiveTexture write SetActiveTexture;

    // Framebuffer
    { : Framebuffer to be used for draw operations, 0 = default framebuffer. }
    property DrawFrameBuffer: TGLuint read FDrawFrameBuffer write SetDrawFrameBuffer;
    { : Framebuffer to be used for read operations, 0 = default framebuffer. }
    property ReadFrameBuffer: TGLuint read FReadFrameBuffer write SetReadFrameBuffer;
    { : set both draw + read framebuffer. }
    procedure SetFrameBuffer(const Value: TGLuint);
    // property FrameBuffer: TGLuint read FDrawFrameBuffer write SetFrameBuffer;

    // Renderbuffer
    { : Currently bound render buffer. }
    property RenderBuffer: TGLuint read FRenderBuffer write SetRenderBuffer;

    // Pixels
    { : Controls whether byte swapping occurs during pixel unpacking. }
    property UnpackSwapBytes: TGLBoolean read FUnpackSwapBytes write SetUnpackSwapBytes;
    { : Whether unpacked data is required with LSB (least significant bit) first. }
    property UnpackLSBFirst: TGLBoolean read FUnpackLSBFirst write SetUnpackLSBFirst;
    { : Unpack image height. }
    property UnpackImageHeight: TGLuint read FUnpackImageHeight write SetUnpackImageHeight;
    { : Unpack skip images. }
    property UnpackSkipImages: TGLuint read FUnpackSkipImages write SetUnpackSkipImages;
    { : Unpack row length. }
    property UnpackRowLength: TGLuint read FUnpackRowLength write SetUnpackRowLength;
    { : Unpack skip rows. }
    property UnpackSkipRows: TGLuint read FUnpackSkipRows write SetUnpackSkipRows;
    { : Unpack skip pixels. }
    property UnpackSkipPixels: TGLuint read FUnpackSkipPixels write SetUnpackSkipPixels;
    { : Unpack alignment. }
    property UnpackAlignment: TGLuint read FUnpackAlignment write SetUnpackAlignment;
    { : Controls whether byte swapping occurs during pixel packing. }
    property PackSwapBytes: TGLBoolean read FPackSwapBytes write SetPackSwapBytes;
    { : Whether packed data is required with LSB (least significant bit) first. }
    property PackLSBFirst: TGLBoolean read FPackLSBFirst write SetPackLSBFirst;
    { : Pack image height. }
    property PackImageHeight: TGLuint read FPackImageHeight write SetPackImageHeight;
    { : Pack skip images. }
    property PackSkipImages: TGLuint read FPackSkipImages write SetPackSkipImages;
    { : Pack row length. }
    property PackRowLength: TGLuint read FPackRowLength write SetPackRowLength;
    { : Pack skip rows. }
    property PackSkipRows: TGLuint read FPackSkipRows write SetPackSkipRows;
    { : Pack skip pixels. }
    property PackSkipPixels: TGLuint read FPackSkipPixels write SetPackSkipPixels;
    { : Pack alignment. }
    property PackAlignment: TGLuint read FPackAlignment write SetPackAlignment;
    { : Buffer bound for pixel packing (eg. ReadPixels). }
    property PixelPackBufferBinding: TGLuint read FPixelPackBufferBinding write SetPixelPackBufferBinding;
    { : Buffer bound for pixel unpacking (eg. Tex*Image). }
    property PixelUnpackBufferBinding: TGLuint read FPixelUnpackBufferBinding write SetPixelUnpackBufferBinding;

    // Program
    { : Currently bound program. }
    property CurrentProgram: TGLuint read FCurrentProgram write SetCurrentProgram;
    property MaxTextureUnits: TGLuint read GetMaxTextureUnits;
    { : Currently bound uniform buffer. }
    property UniformBufferBinding: TGLuint read FUniformBufferBinding write SetUniformBufferBinding;

    procedure SetBufferIndexedBinding(const Value: TGLuint; ATarget: TDGLBufferBindingTarget; AIndex: TGLuint; ABufferSize: GLsizeiptr); overload;
    procedure SetBufferIndexedBinding(const Value: TGLuint; ATarget: TDGLBufferBindingTarget; AIndex: TGLuint; AOffset: GLintptr; ARangeSize: GLsizeiptr); overload;

    // Vector + Geometry Shader state
    { : Default values to be used when a vertex array is not used for that
      attribute. }
    property CurrentVertexAttrib[Index: Integer]: TVector read GetCurrentVertexAttrib write SetCurrentVertexAttrib;
    { : Enables/disables program point size. }
    property EnableProgramPointSize: TGLBoolean read FEnableProgramPointSize write SetEnableProgramPointSize;

    // Transform Feedback state
    { : Currently bound transform feedbac buffer. }
    property TransformFeedbackBufferBinding: TGLuint read FTransformFeedbackBufferBinding write SetTransformFeedbackBufferBinding;

    // Hints
    { : Line smooth hint. }
    property LineSmoothHint: THintType read FLineSmoothHint write SetLineSmoothHint;
    { : Polygon smooth hint. }
    property PolygonSmoothHint: THintType read FPolygonSmoothHint write SetPolygonSmoothHint;
    { : Texture compression hint. }
    property TextureCompressionHint: THintType read FTextureCompressionHint write SetTextureCompressionHint;
    { : Fragment shader derivitive hint. }
    property FragmentShaderDerivitiveHint: THintType read FFragmentShaderDerivitiveHint write SetFragmentShaderDerivitiveHint;
    property MultisampleFilterHint: THintType read FMultisampleFilterHint write SetMultisampleFilterHint;

    // Misc
    { : Current queries. }
    property CurrentQuery[Index: TQueryType]: TGLuint read GetCurrentQuery;
    { : Begins a query of "Target" type.  "Value" must be a valid query object. }
    procedure BeginQuery(const target: TQueryType; const Value: TGLuint);
    { : Ends current query of type "Target". }
    procedure EndQuery(const target: TQueryType);
    { : The buffer currently bound to the copy read buffer binding point, this
      is an extra binding point provided so that you don't need to overwrite
      other binding points to copy between buffers. }
    property CopyReadBufferBinding: TGLuint read FCopyReadBufferBinding write SetCopyReadBufferBinding;
    { : The buffer currently bound to the copy write buffer binding point, this
      is an extra binding point provided so that you don't need to overwrite
      other binding points to copy between buffers. }
    property CopyWriteBufferBinding: TGLuint read FCopyWriteBufferBinding write SetCopyWriteBufferBinding;
    { : Enables/Disables seamless texture cube maps. }
    property EnableTextureCubeMapSeamless: TGLBoolean read FEnableTextureCubeMapSeamless write SetEnableTextureCubeMapSeamless;
    { : Indicates the current presence within the list. }
    property InsideList: Boolean read FInsideList;
    // { @HTML ( Begin new display list. }
    // procedure NewList(list: TGLuint; mode: TGLEnum);
    // { @HTML ( End display list. }
    // procedure EndList;

    // {$IFDEF DGL_DEPRECATED}
    // { @HTML ( Defines the OpenGL texture matrix.<p>
    // Assumed texture mode is GL_MODELVIEW. }
    // { @HTML ( Call display list. }
    /// /    procedure CallList(list: TGLuint);
    // procedure SeTextureMatrix(const matrix: TMatrix);
    // procedure ReseTextureMatrix;
    // procedure ResetAllTextureMatrix;
    // {$ENDIF}
    // note: needs to change to per draw-buffer
    procedure SeTDGLColorWriting(flag: Boolean);

    { : Inverts front face winding (CCW/CW). }
    procedure InverTDGLFrontFace;

    // read only properties
    property States: TDGLStates read FStates;

    { : True for ignore deprecated and removed features in OpenGL 3x }
    // property ForwardContext: Boolean read FForwardContext write SetForwardContext default false;

    // STANDARD OBJECT'S SHADER'S UNIFORMS attributes
    property attrPosition: TDGLSLAttribute read FattrPosition;
    property attrNormal: TDGLSLAttribute read FattrNormal;
    property attrVertexColor: TDGLSLAttribute read FattrVertexColor;
    property attrTexCoord0: TDGLSLAttribute read FattrTexCoord0;
    property attrTexCoord1: TDGLSLAttribute read FattrTexCoord1;
    property attrTexCoord2: TDGLSLAttribute read FattrTexCoord2;
    property attrTexCoord3: TDGLSLAttribute read FattrTexCoord3;
    property attrTangent: TDGLSLAttribute read FattrTangent;
    property attrBinormal: TDGLSLAttribute read FattrBinormal;
    property attrIndex: TDGLSLAttribute read FattrIndex;

    // FuniformModelMatrix,    ---> cf PIPELINETRANSFORMATION
    // FuniformViewProjectionMatrix,
    property uniformModelMatrix: TDGLSLUniform read FuniformModelMatrix;
    property uniformViewProjectionMatrix: TDGLSLUniform read FuniformViewProjectionMatrix;
    property uniformDiffuse: TDGLSLUniform read FuniformDiffuse;
    property uniformDiffuseColor: TDGLSLUniform read FuniformDiffuseColor;
    property uniformAmbientColor: TDGLSLUniform read FuniformAmbientColor;
    property uniformSpecularColor: TDGLSLUniform read FuniformSpecularColor;
    property uniformEnvColor: TDGLSLUniform read FuniformEnvColor;
    property uniformTexUnit0: TDGLSLUniform read FuniformTexUnit0;
    property uniformTexUnit1: TDGLSLUniform read FuniformTexUnit1;
    property uniformTexUnit2: TDGLSLUniform read FuniformTexUnit2;
    property uniformTexUnit3: TDGLSLUniform read FuniformTexUnit3;
    property uniformTexUnit4: TDGLSLUniform read FuniformTexUnit4;
    property uniformTexUnit5: TDGLSLUniform read FuniformTexUnit5;
    property uniformTexUnit6: TDGLSLUniform read FuniformTexUnit6;
    property uniformTexUnit7: TDGLSLUniform read FuniformTexUnit7;
    property uniformInstanceID: TDGLSLUniform read FuniformInstanceID;

    property currentShaderLevel: TDGLShaderMaterialLevel read FcurrentShaderLevel write FcurrentShaderLevel;
  end;

type
  TStateRecord = record
    GLConst: TGLEnum;
    GLDeprecated: Boolean;
  end;

const
  // {$WARN SYMBOL_DEPRECATED OFF}
  // cGLStateTypeToGLEnum: array[TDGLStateType] of TGLEnum = (
  // GL_CURRENT_BIT, GL_POINT_BIT, GL_LINE_BIT, GL_POLYGON_BIT,
  // GL_POLYGON_STIPPLE_BIT, GL_PIXEL_MODE_BIT, GL_LIGHTING_BIT, GL_FOG_BIT,
  // GL_DEPTH_BUFFER_BIT, GL_ACCUM_BUFFER_BIT, GL_STENCIL_BUFFER_BIT,
  // GL_VIEWPORT_BIT, GL_TRANSFORM_BIT, GL_ENABLE_BIT, GL_COLOR_BUFFER_BIT,
  // GL_HINT_BIT, GL_EVAL_BIT, GL_LIST_BIT, GL_TEXTURE_BIT, GL_SCISSOR_BIT,
  // GL_MULTISAMPLE_BIT);

  {$WARN SYMBOL_DEPRECATED ON}
  cGLStateToGLEnum: array [TDGLState] of TStateRecord = ((GLConst: GL_BLEND; GLDeprecated: False), (GLConst: GL_CULL_FACE; GLDeprecated: False), (GLConst: GL_DEPTH_TEST; GLDeprecated: False), (GLConst: GL_DITHER; GLDeprecated: False),
    (GLConst: GL_LINE_SMOOTH; GLDeprecated: False), (GLConst: GL_COLOR_LOGIC_OP; GLDeprecated: False), (GLConst: GL_POLYGON_SMOOTH; GLDeprecated: False), (GLConst: GL_SCISSOR_TEST; GLDeprecated: False), (GLConst: GL_STENCIL_TEST;
    GLDeprecated: False), (GLConst: GL_POLYGON_OFFSET_POINT; GLDeprecated: False), (GLConst: GL_POLYGON_OFFSET_LINE; GLDeprecated: False), (GLConst: GL_POLYGON_OFFSET_FILL; GLDeprecated: False), (GLConst: GL_DEPTH_CLAMP; GLDeprecated: False));

  cGLTexTypeToGLEnum: array [TDGLTextureTarget] of TGLEnum = (0, GL_TEXTURE_1D, GL_TEXTURE_2D, GL_TEXTURE_3D, GL_TEXTURE_1D_ARRAY, GL_TEXTURE_2D_ARRAY, GL_TEXTURE_RECTANGLE, GL_TEXTURE_BUFFER, GL_TEXTURE_CUBE_MAP, GL_TEXTURE_2D_MULTISAMPLE,
    GL_TEXTURE_2D_MULTISAMPLE_ARRAY, GL_TEXTURE_CUBE_MAP_ARRAY);

  cGLQueryTypeToGLEnum: array [TQueryType] of TGLEnum = (GL_SAMPLES_PASSED, GL_PRIMITIVES_GENERATED, GL_TRANSFORM_FEEDBACK_PRIMITIVES_WRITTEN, GL_TIME_ELAPSED, GL_ANY_SAMPLES_PASSED);

  cGLStencilOpToGLEnum: array [TStencilOp] of TGLEnum = (GL_KEEP, GL_ZERO, GL_REPLACE, GL_INCR, GL_DECR, GL_INVERT, GL_INCR_WRAP, GL_DECR_WRAP);

  cGLLogicOpToGLEnum: array [TLogicOp] of TGLEnum = (GL_CLEAR, GL_AND, GL_AND_REVERSE, GL_COPY, GL_AND_INVERTED, GL_NOOP, GL_XOR, GL_OR, GL_NOR, GL_EQUIV, GL_INVERT, GL_OR_REVERSE, GL_COPY_INVERTED, GL_OR_INVERTED, GL_NAND, GL_SET);

  cGLComparisonFunctionToGLEnum: array [TComparisonFunction] of TGLEnum = (GL_NEVER, GL_ALWAYS, GL_LESS, GL_LEQUAL, GL_EQUAL, GL_GREATER, GL_NOTEQUAL, GL_GEQUAL);

  cGLBlendFunctionToGLEnum: array [TBlendFunction] of TGLEnum = (GL_ZERO, GL_ONE, GL_SRC_COLOR, GL_ONE_MINUS_SRC_COLOR, GL_DST_COLOR, GL_ONE_MINUS_DST_COLOR, GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA, GL_DST_ALPHA, GL_ONE_MINUS_DST_ALPHA,
    // GL_CONSTANT_COLOR,
    GL_ONE_MINUS_CONSTANT_COLOR, GL_CONSTANT_ALPHA, GL_ONE_MINUS_CONSTANT_ALPHA, GL_SRC_ALPHA_SATURATE { valid for src only } );

  cGLBlendEquationToGLEnum: array [TBlendEquation] of TGLEnum = (GL_FUNC_ADD, GL_FUNC_SUBTRACT, GL_FUNC_REVERSE_SUBTRACT, GL_MIN, GL_MAX);

  cGLFaceWindingToGLEnum: array [TFaceWinding] of TGLEnum = (GL_CCW, GL_CW);

  cGLPolygonModeToGLEnum: array [TPolygonMode] of TGLEnum = (GL_FILL, GL_LINE, GL_POINT);

  cGLCullFaceModeToGLEnum: array [TCullFaceMode] of TGLEnum = (GL_FRONT, GL_BACK, GL_FRONT_AND_BACK);

  cGLHintToGLEnum: array [THintType] of TGLEnum = (GL_DONT_CARE, GL_FASTEST, GL_NICEST);

  cGLBufferBindingTarget: array [TDGLBufferBindingTarget] of TGLEnum = (GL_UNIFORM_BUFFER, GL_TRANSFORM_FEEDBACK_BUFFER);

Var
  MAX_HARDWARE_LIGHT, MAX_SHADER_LIGHT, MAX_HARDWARE_TEXTURE_UNIT, MAX_HARDWARE_UNIFORM_BUFFER_BINDING: Integer; // must be retreive from OpenGL

  // ------------------------------------------------------
  // ------------------------------------------------------
  // ------------------------------------------------------
implementation

// ------------------------------------------------------
// ------------------------------------------------------
// ------------------------------------------------------

uses
  DGLContext, DGLColor;

// {$IFDEF GLS_CACHE_MISS_CHECK}
// resourcestring
// glsStateCashMissing = 'States cache missing: ';
// {$ENDIF}

var
  AttributeRegistry: array[Byte] of TDGLSLAttribute;
  UniformRegistry: array[Byte] of TDGLSLUniform;

procedure ClearAttributeRegistry;
var
  a: Integer;
begin
  for a := 0 to High(AttributeRegistry) do
    AttributeRegistry[a].ID := 0;
end;

function RegisterGLSLAttribute(var Attr: TDGLSLAttribute; const AName: string): Boolean;
var
  a: Integer;
begin
  Result := false;
  for a := 0 to High(AttributeRegistry) do
  begin
    if AttributeRegistry[a].ID > 0 then
      if AttributeRegistry[a].Name = AName then
      begin
        Result := true;
        exit;
      end;
  end;
  for a := 0 to High(AttributeRegistry) do
  begin
    if AttributeRegistry[a].ID = 0 then
    begin
      AttributeRegistry[a].ID := a + 1;
      AttributeRegistry[a].Name := AName;
      AttributeRegistry[a].DataType := GLSLTypeUndefined;
      AttributeRegistry[a].WarningAbsenceLoged := False;
      Attr := AttributeRegistry[a];
      Result := true;
      exit;
    end;
  end;
end;

function GetGLSLAttribute(const AName: string; out Attr: TDGLSLAttribute): Boolean;
var
  a: Integer;
begin
  for a := 0 to High(AttributeRegistry) do
  begin
    if AttributeRegistry[a].ID > 0 then
      if AttributeRegistry[a].Name = AName then
      begin
        Attr := AttributeRegistry[a];
        Result := true;
        exit;
      end;
  end;
  Result := false;
end;

procedure ClearUniformRegistry;
var
  u: Integer;
begin
  for u := 0 to High(UniformRegistry) do
    UniformRegistry[u].ID := 0;
end;

function RegisterGLSLUniform(var Uniform: TDGLSLUniform; const AName: string):Boolean;
var
  u: Integer;
begin
  Result := false;
  for u := 0 to High(UniformRegistry) do
  begin
    if UniformRegistry[u].ID > 0 then
      if UniformRegistry[u].Name = AName then
      begin
        Uniform := UniformRegistry[u];
        Result := true;
        exit;
      end;
  end;
  for u := 0 to High(UniformRegistry) do
  begin
    if UniformRegistry[u].ID = 0 then
    begin
      UniformRegistry[u].ID := u + 1;
      UniformRegistry[u].Name := AName;
      UniformRegistry[u].DataType := GLSLTypeUndefined;
      UniformRegistry[u].WarningAbsenceLoged := False;
      Uniform := UniformRegistry[u];
      Result := true;
      exit;
    end;
  end;
end;

function GetGLSLUniform(const AName: string; out Uniform: TDGLSLUniform):Boolean;
var
  u: Integer;
begin
  for u := 0 to High(UniformRegistry) do
  begin
    if UniformRegistry[u].ID > 0 then
      if UniformRegistry[u].Name = AName then
      begin
        Uniform := UniformRegistry[u];
        Result := true;
        exit;
      end;
  end;
  Result := false;
end;

// ------------------
{ TDGLStateCache }
{$IFDEF GLS_REGION}{$REGION 'TDGLStateCache'}{$ENDIF}

procedure TDGLStateCache.BeginQuery(const target: TQueryType; const Value: TGLuint);
begin
  Assert(FCurrentQuery[target] = 0, 'Can only have one query (of each type) running at a time');
  // Assert(glIsQuery(Value), 'Not a valid query');
  // if Value<>FCurrentQuery[Target] then
  begin
    FCurrentQuery[target] := Value;
    glBeginQuery(cGLQueryTypeToGLEnum[target], Value);
  end;
end;

// Create
//
constructor TDGLStateCache.Create;
var
  I: Integer;
begin
  inherited;
  SetLength(FListStates, 128);
  FCurrentList := 0;

  {$IFDEF DGL_DEPRECATED}
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
  {$ENDIF}
  // Lighting
  FFFPLight    := True;
  FMaxLights   := 0;
  FLightNumber := 0;

  for I := High(FLightEnabling) downto 0 do
  begin
    FLightEnabling[I]                          := False;
    FLightIndices[I]                           := 0;
    FLightStates.Position[I]                   := NullHmgVector;
    FLightStates.Ambient[I]                    := clrBlack;
    FLightStates.Diffuse[I]                    := clrBlack;
    FLightStates.Specular[I]                   := clrBlack;
    FLightStates.SpotDirection[I]              := VectorMake(0.0, 0.0, -1.0, 0.0);
    FSpotCutoff[I]                             := 180.0;
    FLightStates.SpotCosCutoffExponent[I].V[0] := -1;
    FLightStates.SpotCosCutoffExponent[I].V[1] := 0;
    FLightStates.Attenuation[I]                := NullHmgVector;
  end;
  FLightStates.Diffuse[0]  := clrWhite;
  FLightStates.Specular[0] := clrWhite;

  // for I := High(FTextureMatrixIsIdentity) downto 0 do
  // FTextureMatrixIsIdentity[I] := False;
  // FForwardContext := False;

  // Vertex Array Data state
  FVertexArrayBinding   := 0;
  FTextureBufferBinding := 0;

  // Transformation state
  FViewPort      := Vector4iMake(0, 0, 0, 0);
  FDepthRange[0] := 0.0;
  FDepthRange[1] := 1.0;

  FillChar(FEnableClipDistance, sizeof(FEnableClipDistance), $00);
  FEnableDepthClamp := False;

  // Coloring state
  FClampReadColor  := GL_FIXED_ONLY;
  FProvokingVertex := GL_LAST_VERTEX_CONVENTION;

  // Rasterization state
  FPointSize                := 1.0;
  FPointFadeThresholdSize   := 1.0;
  FPointSpriteCoordOrigin   := GL_UPPER_LEFT;
  FLineWidth                := 1.0;
  FLineStippleFactor        := 1;
  FLineStipplePattern       := $FFFF;
  FEnableLineSmooth         := False;
  FEnableCullFace           := False;
  FCullFaceMode             := cmBack;
  FFrontFace                := fwCounterClockWise;
  FEnablePolygonSmooth      := False;
  FPolygonMode              := pmFill;
  FPolygonOffsetFactor      := 0.0;
  FPolygonOffsetUnits       := 0.0;
  FEnablePolygonOffsetPoint := False;
  FEnablePolygonOffsetLine  := False;
  FEnablePolygonOffsetFill  := False;

  // Multisample state
  FEnableMultisample           := True;
  FEnableSampleAlphaToCoverage := False;
  FEnableSampleAlphaToOne      := False;
  FEnableSampleCoverage        := False;
  FSampleCoverageValue         := 1.0;
  FSampleCoverageInvert        := False;
  FEnableSampleMask            := False;
  FillChar(FSampleMaskValue, sizeof(FSampleMaskValue), $FF);

  // Texture state
  FillChar(FTextureBinding, sizeof(FTextureBinding), $00);
  FillChar(FActiveTextureEnabling, sizeof(FActiveTextureEnabling), $00);

  // Active texture state
  FActiveTexture := 0;

  // Pixel operation state
  FEnableScissorTest := False;
  // FScissorBox := Rect(0, 0, Width, Height);
  FEnableStencilTest    := False;
  FStencilFunc          := cfAlways;
  FStencilValueMask     := $FFFFFFFF;
  FStencilRef           := 0;
  FStencilFail          := soKeep;
  FStencilPassDepthFail := soKeep;
  FStencilPassDepthPass := soKeep;

  FStencilBackFunc          := cfAlways;
  FStencilBackValueMask     := $FFFFFFFF;
  FStencilBackRef           := 0;
  FStencilBackFail          := soKeep;
  FStencilBackPassDepthPass := soKeep;
  FStencilBackPassDepthFail := soKeep;

  FEnableDepthTest := False;
  FDepthFunc       := cfLess;

  FillChar(FEnableBlend, sizeof(FEnableBlend), $0);

  FBlendSrcRGB   := bfOne;
  FBlendSrcAlpha := bfOne;
  FBlendDstRGB   := bfZero;
  FBlendDstAlpha := bfZero;

  FBlendEquationRGB   := beAdd;
  FBlendEquationAlpha := beAdd;
  FBlendColor         := NullHmgVector;

  FEnableFramebufferSRGB := False;
  FEnableDither          := True;
  FEnableColorLogicOp    := False;

  FLogicOpMode := loCopy;

  // Framebuffer control state
  // for I := 0 to Length(FColorWriteMask) - 1 do
  // FColorWriteMask[i] := [ccRed, ccGreen, ccBlue, ccAlpha];
  FillChar(FColorWriteMask, sizeof(FColorWriteMask), $F);
  FDepthWriteMask       := True;
  FStencilWriteMask     := $FFFFFFFF;
  FStencilBackWriteMask := $FFFFFFFF;
  FColorClearValue      := NullHmgVector;
  FDepthClearValue      := 1.0;
  FStencilClearValue    := 0;

  // Framebuffer state
  FDrawFrameBuffer := 0;
  FReadFrameBuffer := 0;

  // Renderbuffer state
  FRenderBuffer := 0;

  // Pixels state
  FUnpackSwapBytes   := False;
  FUnpackLSBFirst    := False;
  FUnpackImageHeight := 0;
  FUnpackSkipImages  := 0;
  FUnpackRowLength   := 0;
  FUnpackSkipRows    := 0;
  FUnpackSkipPixels  := 0;
  FUnpackAlignment   := 4;
  FPackSwapBytes     := False;
  FPackLSBFirst      := False;
  FPackImageHeight   := 0;
  FPackSkipImages    := 0;
  FPackRowLength     := 0;
  FPackSkipRows      := 0;
  FPackSkipPixels    := 0;
  FPackAlignment     := 4;

  FPixelPackBufferBinding   := 0;
  FPixelUnpackBufferBinding := 0;

  // Program state
  FCurrentProgram       := 0;
  FUniformBufferBinding := 0;
  FillChar(FUBOStates[bbtUniform][0], sizeof(FUBOStates), $00);

  // Vector + Geometry Shader state
  for I                     := 0 to Length(FCurrentVertexAttrib) - 1 do
    FCurrentVertexAttrib[I] := NullHmgPoint;
  FEnableProgramPointSize   := False;

  // Transform Feedback state
  FTransformFeedbackBufferBinding := 0;

  // Hints state
  FTextureCompressionHint       := hintDontCare;
  FPolygonSmoothHint            := hintDontCare;
  FFragmentShaderDerivitiveHint := hintDontCare;
  FLineSmoothHint               := hintDontCare;

  // Misc state
  FillChar(FCurrentQuery, sizeof(FCurrentQuery), $00);
  FCopyReadBufferBinding        := 0;
  FCopyWriteBufferBinding       := 0;
  FEnableTextureCubeMapSeamless := False;
  FInsideList                   := False;

  {: Registration of the most common attributes. }
  ClearAttributeRegistry;
  RegisterGLSLAttribute(FattrPosition, 'Position');
  RegisterGLSLAttribute(FattrNormal, 'Normal');
  RegisterGLSLAttribute(FattrVertexColor, 'VertexColor');
  RegisterGLSLAttribute(FattrTexCoord0, 'TexCoord0');
  RegisterGLSLAttribute(FattrTexCoord1, 'TexCoord1');
  RegisterGLSLAttribute(FattrTexCoord2, 'TexCoord2');
  RegisterGLSLAttribute(FattrTexCoord3, 'TexCoord3');
  RegisterGLSLAttribute(FattrTangent, 'Tangent');
  RegisterGLSLAttribute(FattrBinormal, 'Binormal');
  RegisterGLSLAttribute(FattrIndex, 'Index');

  {: Registration of the most common uniforms. }
  ClearUniformRegistry;
  RegisterGLSLUniform(FuniformModelMatrix, 'ModelMatrix');
  RegisterGLSLUniform(FuniformViewProjectionMatrix, 'ViewProjectionMatrix');
  RegisterGLSLUniform(FuniformDiffuse, 'Diffuse');
  RegisterGLSLUniform(FuniformTexUnit0, 'TexUnit0');
  RegisterGLSLUniform(FuniformTexUnit1, 'TexUnit1');
  RegisterGLSLUniform(FuniformTexUnit2, 'TexUnit2');
  RegisterGLSLUniform(FuniformTexUnit3, 'TexUnit3');
  RegisterGLSLUniform(FuniformTexUnit4, 'TexUnit4');
  RegisterGLSLUniform(FuniformTexUnit5, 'TexUnit5');
  RegisterGLSLUniform(FuniformTexUnit6, 'TexUnit6');
  RegisterGLSLUniform(FuniformTexUnit7, 'TexUnit7');
  RegisterGLSLUniform(FuniformInstanceID, 'InstanceID');

end;

// Destroy
//
destructor TDGLStateCache.Destroy;
begin
  inherited;
end;

procedure TDGLStateCache.EndQuery(const target: TQueryType);
begin
  Assert(FCurrentQuery[target] <> 0, 'No query running');
  FCurrentQuery[target] := 0;
  glEndQuery(cGLQueryTypeToGLEnum[target]);
end;

// Enable
//
procedure TDGLStateCache.Enable(const aState: TDGLState);
begin
  // if cGLStateToGLEnum[aState].GLDeprecated and FForwardContext then exit;
  if not(aState in FStates) or FInsideList then
  begin
    if FInsideList then
      Include(FListStates[FCurrentList], sttEnable)
    else
      Include(FStates, aState);
    // {$IFDEF GLS_CACHE_MISS_CHECK}
    // if glIsEnabled(cGLStateToGLEnum[aState].GLConst) then
    // DGLSLogger.LogError(glsStateCashMissing + 'Enable');
    // {$ENDIF}
    glEnable(cGLStateToGLEnum[aState].GLConst);
  end;
end;

// Disable
//
procedure TDGLStateCache.Disable(const aState: TDGLState);
begin
  // if cGLStateToGLEnum[aState].GLDeprecated and FForwardContext then exit;
  if (aState in FStates) or FInsideList then
  begin
    if FInsideList then
      Include(FListStates[FCurrentList], sttEnable)
    else
      Exclude(FStates, aState);
    // {$IFDEF GLS_CACHE_MISS_CHECK}
    // if not glIsEnabled(cGLStateToGLEnum[aState].GLConst) then
    // DGLSLogger.LogError(glsStateCashMissing + 'Disable');
    // {$ENDIF}
    glDisable(cGLStateToGLEnum[aState].GLConst);
    // if aState = stColorMaterial then
    // if FInsideList then
    // Include(FListStates[FCurrentList], sttLighting);
  end;
end;

// PerformEnable
//

procedure TDGLStateCache.PerformEnable(const aState: TDGLState);
begin
  // if cGLStateToGLEnum[aState].GLDeprecated and FForwardContext then exit;
  Include(FStates, aState);
  glEnable(cGLStateToGLEnum[aState].GLConst);
end;

// PerformDisable
//
procedure TDGLStateCache.PerformDisable(const aState: TDGLState);
begin
  // if cGLStateToGLEnum[aState].GLDeprecated and FForwardContext then exit;
  Exclude(FStates, aState);
  glDisable(cGLStateToGLEnum[aState].GLConst);
end;

procedure TDGLStateCache.PopAttrib;
begin
  // TODO: replace with proper client side push/pop
  // glPopAttrib();
end;

procedure TDGLStateCache.PushAttrib(stateTypes: TDGLStateTypes);
// var
// tempFlag: TGLuint;
// I: Integer;
begin
  // TODO: replace with proper client side push/pop
  // tempFlag := 0;
  // for I := Integer(Low(TDGLStateType)) to Integer(high(TDGLStateType)) do
  // begin
  // if TDGLStateType(I) in stateTypes then
  // begin
  // tempFlag := tempFlag or cGLStateTypeToGLEnum[TDGLStateType(I)];
  // end;
  // end;
  // glPushAttrib(tempFlag);
end;

procedure TDGLStateCache.SetVertexArrayBinding(const Value: TGLuint);
begin
  if Value <> FVertexArrayBinding then
  begin
    FVertexArrayBinding := Value;
    glBindVertexArray(Value);
  end;
end;

function TDGLStateCache.GetArrayBufferBinding: TGLuint;
begin
  Result := FArrayBufferBinding;
end;

procedure TDGLStateCache.SetArrayBufferBinding(const Value: TGLuint);
begin
  if (Value <> FArrayBufferBinding) or (FVertexArrayBinding <> 0) then
  begin
    FArrayBufferBinding := Value;
    glBindBuffer(GL_ARRAY_BUFFER, Value);
  end;
end;

function TDGLStateCache.GetElementBufferBinding: TGLuint;
begin
  Result := FElementBufferBinding
end;

procedure TDGLStateCache.SetElementBufferBinding(const Value: TGLuint);
begin
  if (Value <> FElementBufferBinding) or (FVertexArrayBinding <> 0) then
  begin
    FElementBufferBinding := Value;
    glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, Value);
  end;
end;

procedure TDGLStateCache.SetEnableProgramPointSize(const Value: TGLBoolean);
begin
  if Value <> FEnableProgramPointSize then
  begin
    FEnableProgramPointSize := Value;
    if Value then
      glEnable(GL_PROGRAM_POINT_SIZE)
    else
      glDisable(GL_PROGRAM_POINT_SIZE);
  end;
end;

procedure TDGLStateCache.SetBlendColor(const Value: TVector);
begin
  if not VectorEquals(Value, FBlendColor) or FInsideList then
  begin
    if FInsideList then
      Include(FListStates[FCurrentList], sttColorBuffer)
    else
      FBlendColor := Value;
    glBlendColor(Value.V[0], Value.V[1], Value.V[2], Value.V[3]);
  end;
end;

procedure TDGLStateCache.SetBlendEquationSeparate(const modeRGB, modeAlpha: TBlendEquation);
begin
  if (modeRGB <> FBlendEquationRGB) or (modeAlpha <> FBlendEquationAlpha) or FInsideList then
  begin
    FBlendEquationRGB   := modeRGB;
    FBlendEquationAlpha := modeAlpha;
    glBlendEquationSeparate(cGLBlendEquationToGLEnum[modeRGB], cGLBlendEquationToGLEnum[modeAlpha]);
  end;
  if FInsideList then
    Include(FListStates[FCurrentList], sttColorBuffer);
end;

procedure TDGLStateCache.SetBlendEquation(const mode: TBlendEquation);
begin
  if (mode <> FBlendEquationRGB) or (mode <> FBlendEquationAlpha) or FInsideList then
  begin
    if FInsideList then
      Include(FListStates[FCurrentList], sttColorBuffer)
    else
    begin
      FBlendEquationRGB   := mode;
      FBlendEquationAlpha := mode;
    end;
    glBlendEquation(cGLBlendEquationToGLEnum[mode]);
  end;
end;

procedure TDGLStateCache.SetBlendFunc(const Src: TBlendFunction; const Dst: TDstBlendFunction);
begin
  if (Src <> FBlendSrcRGB) or (Dst <> FBlendDstRGB) or FInsideList then
  begin
    if FInsideList then
      Include(FListStates[FCurrentList], sttColorBuffer)
    else
    begin
      FBlendSrcRGB   := Src;
      FBlendDstRGB   := Dst;
      FBlendSrcAlpha := Src;
      FBlendSrcAlpha := Dst;
    end;
    glBlendFunc(cGLBlendFunctionToGLEnum[Src], cGLBlendFunctionToGLEnum[Dst]);
  end;
end;

procedure TDGLStateCache.SetBlendFuncSeparate(const SrcRGB: TBlendFunction; const DstRGB: TDstBlendFunction; const SrcAlpha: TBlendFunction; const DstAlpha: TDstBlendFunction);
begin
  if (SrcRGB <> FBlendSrcRGB) or (DstRGB <> FBlendDstRGB) or (SrcAlpha <> FBlendSrcAlpha) or (DstAlpha <> FBlendDstAlpha) or FInsideList then
  begin
    if FInsideList then
      Include(FListStates[FCurrentList], sttColorBuffer)
    else
    begin
      FBlendSrcRGB   := SrcRGB;
      FBlendDstRGB   := DstRGB;
      FBlendSrcAlpha := SrcAlpha;
      FBlendDstAlpha := DstAlpha;
    end;
    glBlendFuncSeparate(cGLBlendFunctionToGLEnum[SrcRGB], cGLBlendFunctionToGLEnum[DstRGB], cGLBlendFunctionToGLEnum[SrcAlpha], cGLBlendFunctionToGLEnum[DstAlpha]);
  end;
end;

procedure TDGLStateCache.SetClampReadColor(const Value: TGLEnum);
begin
  if (Value <> FClampReadColor) or FInsideList then
  begin
    if FInsideList then
      Include(FListStates[FCurrentList], sttColorBuffer)
    else
      FClampReadColor := Value;
    glClampColor(GL_CLAMP_READ_COLOR, Value);
  end;
end;

procedure TDGLStateCache.SetColorWriteMask(Index: Integer; const Value: TColorMask);
begin
  if FColorWriteMask[Index] <> Value then
  begin
    FColorWriteMask[Index] := Value;
    glColorMaski(Index, ccRed in Value, ccGreen in Value, ccBlue in Value, ccAlpha in Value);
  end;
end;

procedure TDGLStateCache.SetCopyReadBufferBinding(const Value: TGLuint);
begin
  if Value <> FCopyReadBufferBinding then
  begin
    FCopyReadBufferBinding := Value;
    glBindBuffer(GL_COPY_READ_BUFFER, Value);
  end;
end;

procedure TDGLStateCache.SetCopyWriteBufferBinding(const Value: TGLuint);
begin
  if Value <> FCopyWriteBufferBinding then
  begin
    FCopyWriteBufferBinding := Value;
    glBindBuffer(GL_COPY_WRITE_BUFFER, Value);
  end;
end;

procedure TDGLStateCache.SetCullFaceMode(const Value: TCullFaceMode);
begin
  if (Value <> FCullFaceMode) or FInsideList then
  begin
    if FInsideList then
      Include(FListStates[FCurrentList], sttPolygon)
    else
      FCullFaceMode := Value;
    glCullFace(cGLCullFaceModeToGLEnum[Value]);
  end;

end;

procedure TDGLStateCache.SetCurrentProgram(const Value: TGLuint);
begin
  if Value <> FCurrentProgram then
  begin
    FCurrentProgram := Value;
    glUseProgram(Value);
  end;
end;

procedure TDGLStateCache.SetActiveTexture(const Value: TGLint);
begin
  if GL_ARB_multitexture then
    if (Value <> FActiveTexture) or FInsideList then
    begin
      if FInsideList then
        Include(FListStates[FCurrentList], sttTexture)
      else
        FActiveTexture := Value;
      glActiveTexture(GL_TEXTURE0 + Value);
    end;
end;

procedure TDGLStateCache.SetTextureBufferBinding(const Value: TGLuint);
begin
  if Value <> FTextureBufferBinding then
  begin
    FTextureBufferBinding := Value;
    glBindBuffer(GL_TEXTURE_BUFFER, Value);
  end;
end;

procedure TDGLStateCache.SetCurrentVertexAttrib(Index: Integer; const Value: TVector);
begin
  if not VectorEquals(Value, FCurrentVertexAttrib[Index]) then
  begin
    FCurrentVertexAttrib[Index] := Value;
    glVertexAttrib4fv(Index, @Value.V[0]);
  end;
end;

procedure TDGLStateCache.SetDepthClearValue(const Value: TGLfloat);
begin
  if (Value <> FDepthClearValue) or FInsideList then
  begin
    if FInsideList then
      Include(FListStates[FCurrentList], sttDepthBuffer)
    else
      FDepthClearValue := Value;
    glClearDepth(Value);
  end;

end;

procedure TDGLStateCache.SetDepthFunc(const Value: TDepthFunction);
begin
  if (Value <> FDepthFunc) or FInsideList then
  begin
    if FInsideList then
      Include(FListStates[FCurrentList], sttDepthBuffer)
    else
      FDepthFunc := Value;
    glDepthFunc(cGLComparisonFunctionToGLEnum[Value]);
  end;

end;

procedure TDGLStateCache.SetDepthRange(const ZNear, ZFar: TGLclampd);
begin
  if (ZNear <> FDepthRange[0]) or (ZFar <> FDepthRange[1]) or FInsideList then
  begin
    if FInsideList then
      Include(FListStates[FCurrentList], sttViewport)
    else
    begin
      FDepthRange[0] := ZNear;
      FDepthRange[1] := ZFar;
    end;
    glDepthRange(ZNear, ZFar);
  end;
end;

procedure TDGLStateCache.SetDepthRangeFar(const Value: TGLclampd);
begin
  if (Value <> FDepthRange[1]) or FInsideList then
  begin
    if FInsideList then
      Include(FListStates[FCurrentList], sttViewport)
    else
      FDepthRange[1] := Value;
    glDepthRange(FDepthRange[0], Value);
  end;
end;

procedure TDGLStateCache.SetDepthRangeNear(const Value: TGLclampd);
begin
  if (Value <> FDepthRange[0]) or FInsideList then
  begin
    if FInsideList then
      Include(FListStates[FCurrentList], sttViewport)
    else
      FDepthRange[0] := Value;
    glDepthRange(Value, FDepthRange[1]);
  end;
end;

procedure TDGLStateCache.SetDepthWriteMask(const Value: TGLBoolean);
begin
  if (Value <> FDepthWriteMask) or FInsideList then
  begin
    if FInsideList then
      Include(FListStates[FCurrentList], sttDepthBuffer)
    else
      FDepthWriteMask := Value;
    glDepthMask(Value);
  end;
end;

procedure TDGLStateCache.SetDrawFrameBuffer(const Value: TGLuint);
begin
  if Value <> FDrawFrameBuffer then
  begin
    FDrawFrameBuffer := Value;
    glBindFramebuffer(GL_DRAW_FRAMEBUFFER, Value);
  end;
end;

procedure TDGLStateCache.SetEnableBlend(Index: Integer; const Value: TGLBoolean);
begin
  if FEnableBlend[Index] <> Value then
  begin
    FEnableBlend[Index] := Value;
    if Value then
      glEnablei(GL_BLEND, Index)
    else
      glDisablei(GL_BLEND, Index);
  end;
end;

procedure TDGLStateCache.SetEnableClipDistance(Index: Cardinal; const Value: TGLBoolean);
begin
  if FEnableClipDistance[Index] <> Value then
  begin
    FEnableClipDistance[Index] := Value;
    if Value then
      glEnable(GL_CLIP_DISTANCE0 + Index)
    else
      glDisable(GL_CLIP_DISTANCE0 + Index);
  end;
end;

procedure TDGLStateCache.SetEnableColorLogicOp(const Value: TGLBoolean);
begin
  if Value <> FEnableColorLogicOp then
  begin
    FEnableColorLogicOp := Value;
    if Value then
      glEnable(GL_COLOR_LOGIC_OP)
    else
      glDisable(GL_COLOR_LOGIC_OP);
  end;
end;

procedure TDGLStateCache.SetEnableCullFace(const Value: TGLBoolean);
begin

end;

procedure TDGLStateCache.SetEnableDepthClamp(const enabled: TGLBoolean);
begin

end;

procedure TDGLStateCache.SetEnableDepthTest(const Value: TGLBoolean);
begin

end;

procedure TDGLStateCache.SetEnableDither(const Value: TGLBoolean);
begin

end;

procedure TDGLStateCache.SetEnableFramebufferSRGB(const Value: TGLBoolean);
begin

end;

procedure TDGLStateCache.SetEnableLineSmooth(const Value: TGLBoolean);
begin

end;

procedure TDGLStateCache.SetEnableMultisample(const Value: TGLBoolean);
begin

end;

procedure TDGLStateCache.SetEnablePolygonOffsetFill(const Value: TGLBoolean);
begin

end;

procedure TDGLStateCache.SetEnablePolygonOffsetLine(const Value: TGLBoolean);
begin

end;

procedure TDGLStateCache.SetEnablePolygonOffsetPoint(const Value: TGLBoolean);
begin

end;

procedure TDGLStateCache.SetEnablePolygonSmooth(const Value: TGLBoolean);
begin

end;

procedure TDGLStateCache.SetEnableSampleAlphaToCoverage(const Value: TGLBoolean);
begin
  if Value <> FEnableSampleAlphaToCoverage then
  begin
    FEnableSampleAlphaToCoverage := Value;
    if Value then
      glEnable(GL_SAMPLE_ALPHA_TO_COVERAGE)
    else
      glDisable(GL_SAMPLE_ALPHA_TO_COVERAGE);
  end;
end;

procedure TDGLStateCache.SetEnableSampleCoverage(const Value: TGLBoolean);
begin
  if Value <> FEnableSampleCoverage then
  begin
    FEnableSampleCoverage := Value;
    if Value then
      glEnable(GL_SAMPLE_COVERAGE)
    else
      glDisable(GL_SAMPLE_COVERAGE);
  end;
end;

procedure TDGLStateCache.SetEnableSampleMask(const Value: TGLBoolean);
begin
  if Value <> FEnableSampleMask then
  begin
    FEnableSampleMask := Value;
    if Value then
      glEnable(GL_SAMPLE_MASK)
    else
      glDisable(GL_SAMPLE_MASK);
  end;
end;

procedure TDGLStateCache.SetEnableSampleAlphaToOne(const Value: TGLBoolean);
begin
  if Value <> FEnableSampleAlphaToOne then
  begin
    FEnableSampleAlphaToOne := Value;
    if Value then
      glEnable(GL_SAMPLE_ALPHA_TO_ONE)
    else
      glDisable(GL_SAMPLE_ALPHA_TO_ONE);
  end;
end;

procedure TDGLStateCache.SetEnableScissorTest(const Value: TGLBoolean);
begin

end;

procedure TDGLStateCache.SetEnableStencilTest(const Value: TGLBoolean);
begin

end;

procedure TDGLStateCache.SetFragmentShaderDerivitiveHint(const Value: THintType);
begin
  if Value <> FFragmentShaderDerivitiveHint then
  begin
    if FInsideList then
      Include(FListStates[FCurrentList], sttHint)
    else
      FFragmentShaderDerivitiveHint := Value;
    glHint(GL_FRAGMENT_SHADER_DERIVATIVE_HINT, cGLHintToGLEnum[Value]);
  end;
end;

procedure TDGLStateCache.SetFrameBuffer(const Value: TGLuint);
begin
  if (Value <> FDrawFrameBuffer) or (Value <> FReadFrameBuffer) or FInsideList then
  begin
    FDrawFrameBuffer := Value;
    FReadFrameBuffer := Value;
    glBindFramebuffer(GL_FRAMEBUFFER, Value);
  end;
end;

procedure TDGLStateCache.SetFrontFace(const Value: TFaceWinding);
begin
  if (Value <> FFrontFace) or FInsideList then
  begin
    if FInsideList then
      Include(FListStates[FCurrentList], sttPolygon)
    else
      FFrontFace := Value;
    glFrontFace(cGLFaceWindingToGLEnum[Value]);
  end;
end;

function TDGLStateCache.GetColorWriteMask(Index: Integer): TColorMask;
begin
  Result := FColorWriteMask[Index];
end;

function TDGLStateCache.GetCurrentQuery(Index: TQueryType): TGLuint;
begin
  Result := FCurrentQuery[Index];
end;

function TDGLStateCache.GetCurrentVertexAttrib(Index: Integer): TVector;
begin
  Result := FCurrentVertexAttrib[Index];
end;

function TDGLStateCache.GetDepthRangeFar: TGLclampd;
begin
  Result := FDepthRange[1];
end;

function TDGLStateCache.GetDepthRangeNear: TGLclampd;
begin
  Result := FDepthRange[0];
end;

function TDGLStateCache.GetEnableBlend(Index: Integer): TGLBoolean;
begin
  Result := FEnableBlend[Index];
end;

function TDGLStateCache.GetEnableClipDistance(ClipDistance: Cardinal): TGLBoolean;
begin
  Result := FEnableClipDistance[ClipDistance];
end;

function TDGLStateCache.GetSampleMaskValue(Index: Integer): TGLbitfield;
begin
  Result := FSampleMaskValue[Index];
end;

function TDGLStateCache.GetMaxTextureSize: TGLuint;
begin
  if FMaxTextureSize = 0 then
    glGetIntegerv(GL_MAX_TEXTURE_SIZE, @FMaxTextureSize);
  Result := FMaxTextureSize;
end;

function TDGLStateCache.GetMax3DTextureSize: TGLuint;
begin
  if FMax3DTextureSize = 0 then
    glGetIntegerv(GL_MAX_3D_TEXTURE_SIZE, @FMax3DTextureSize);
  Result := FMax3DTextureSize;
end;

function TDGLStateCache.GetMaxCubeTextureSize: TGLuint;
begin
  if FMaxCubeTextureSize = 0 then
    glGetIntegerv(GL_MAX_CUBE_MAP_TEXTURE_SIZE, @FMaxCubeTextureSize);
  Result := FMaxCubeTextureSize;
end;

function TDGLStateCache.GetMaxArrayTextureSize: TGLuint;
begin
  if FMaxArrayTextureSize = 0 then
    glGetIntegerv(GL_MAX_ARRAY_TEXTURE_LAYERS, @FMaxArrayTextureSize);
  Result := FMaxArrayTextureSize;
end;

function TDGLStateCache.GetMaxTextureImageUnits: TGLuint;
begin
  if FMaxTextureImageUnits = 0 then
    glGetIntegerv(GL_MAX_TEXTURE_IMAGE_UNITS, @FMaxTextureImageUnits);
  Result := FMaxTextureImageUnits;
end;

function TDGLStateCache.GetMaxTextureAnisotropy: TGLuint;
begin
  if (FMaxTextureAnisotropy = 0) and GL_EXT_texture_filter_anisotropic then
    glGetIntegerv(GL_MAX_TEXTURE_MAX_ANISOTROPY_EXT, @FMaxTextureAnisotropy);
  Result := FMaxTextureAnisotropy;
end;

function TDGLStateCache.GetMaxSamples: TGLuint;
begin
  if (FMaxSamples = 0) and GL_ARB_multisample then
    glGetIntegerv(GL_MAX_SAMPLES, @FMaxSamples);
  Result := FMaxSamples;
end;

function TDGLStateCache.GetTextureBinding(Index: Integer; target: TDGLTextureTarget): TGLuint;
begin
  Result := FTextureBinding[Index, target];
end;

function TDGLStateCache.GetTextureBindingTime(Index: Integer; target: TDGLTextureTarget): Double;
begin
  Result := FTextureBindingTime[Index, target];
end;

function TDGLStateCache.GetSamplerBinding(Index: TGLuint): TGLuint;
begin
  Result := FSamplerBinding[Index];
end;

procedure TDGLStateCache.SetSamplerBinding(Index: TGLuint; const Value: TGLuint);
begin
  if Index > High(FSamplerBinding) then
    exit;
  if (Value <> FSamplerBinding[Index]) or FInsideList then
  begin
    if FInsideList then
      Include(FListStates[FCurrentList], sttTexture)
    else
      FSamplerBinding[Index] := Value;
    glBindSampler(Index, Value);
  end;
end;

/// / SeTDGLTextureMatrix
/// /
// {$ifdef DGL_DEPRECATED}
// procedure TDGLStateCache.SeTextureMatrix(const matrix: TMatrix);
// begin
// if FForwardContext then
// exit;
// if FInsideList then
// Include(FListStates[FCurrentList], sttTransform)
// else
// FTextureMatrixIsIdentity[ActiveTexture] := False;
// glMatrixMode(GL_TEXTURE);
// glLoadMatrixf(PGLFloat(@matrix.V[0].V[0]));
// glMatrixMode(GL_MODELVIEW);
// end;
//
/// / ReseTDGLTextureMatrix
/// /
//
// procedure TDGLStateCache.ReseTextureMatrix;
// begin
// if FForwardContext then
// exit;
// glMatrixMode(GL_TEXTURE);
// glLoadIdentity;
// FTextureMatrixIsIdentity[ActiveTexture] := True;
// glMatrixMode(GL_MODELVIEW);
// end;
//
//
/// / ResetAllGLTextureMatrix
/// /
//
// procedure TDGLStateCache.ResetTextureMatrix;
// var
// I: Integer;
// lastActiveTexture: TGLuint;
// begin
// if FForwardContext then
// exit;
// lastActiveTexture := ActiveTexture;
// glMatrixMode(GL_TEXTURE);
// for I := High(FTextureMatrixIsIdentity) downto 0 do
// if not FTextureMatrixIsIdentity[I] then
// begin
// ActiveTexture := I;
// glLoadIdentity;
// FTextureMatrixIsIdentity[I] := True;
// end;
// glMatrixMode(GL_MODELVIEW);
// ActiveTexture := lastActiveTexture;
// end;
// {$ENDIF}

procedure TDGLStateCache.SetLineSmoothHint(const Value: THintType);
begin
  if (Value <> FLineSmoothHint) or FInsideList then
  begin
    if FInsideList then
      Include(FListStates[FCurrentList], sttHint)
    else
      FLineSmoothHint := Value;
    glHint(GL_LINE_SMOOTH_HINT, cGLHintToGLEnum[Value]);
  end;
end;

procedure TDGLStateCache.SetLineWidth(const Value: TGLfloat);
begin
  // note: wide lines no longer deprecated (see OpenGL spec)
  if (Value <> FLineWidth) or FInsideList then
  begin
    if FInsideList then
      Include(FListStates[FCurrentList], sttLine)
    else
      FLineWidth := Value;
    glLineWidth(Value);
  end;
end;

procedure TDGLStateCache.SetLogicOpMode(const Value: TLogicOp);
begin
  if (Value <> FLogicOpMode) or FInsideList then
  begin
    if FInsideList then
      Include(FListStates[FCurrentList], sttColorBuffer)
    else
      FLogicOpMode := Value;
    glLogicOp(cGLLogicOpToGLEnum[Value]);
  end;
end;

procedure TDGLStateCache.SetPackAlignment(const Value: TGLuint);
begin
  if Value <> FPackAlignment then
  begin
    FPackAlignment := Value;
    glPixelStoref(GL_PACK_ALIGNMENT, Value);
  end;
end;

procedure TDGLStateCache.SetPackImageHeight(const Value: TGLuint);
begin
  if Value <> FPackImageHeight then
  begin
    FPackImageHeight := Value;
    glPixelStoref(GL_PACK_IMAGE_HEIGHT, Value);
  end;
end;

procedure TDGLStateCache.SetPackLSBFirst(const Value: TGLBoolean);
begin
  if Value <> FPackLSBFirst then
  begin
    FPackLSBFirst := Value;
    glPixelStorei(GL_PACK_LSB_FIRST, byte(Value));
  end;
end;

procedure TDGLStateCache.SetPackRowLength(const Value: TGLuint);
begin
  if Value <> FPackRowLength then
  begin
    FPackRowLength := Value;
    glPixelStoref(GL_PACK_ROW_LENGTH, Value);
  end;
end;

procedure TDGLStateCache.SetPackSkipImages(const Value: TGLuint);
begin
  if Value <> FPackSkipImages then
  begin
    FPackSkipImages := Value;
    glPixelStoref(GL_PACK_SKIP_IMAGES, Value);
  end;
end;

procedure TDGLStateCache.SetPackSkipPixels(const Value: TGLuint);
begin
  if Value <> FPackSkipPixels then
  begin
    FPackSkipPixels := Value;
    glPixelStoref(GL_PACK_SKIP_PIXELS, Value);
  end;
end;

procedure TDGLStateCache.SetPackSkipRows(const Value: TGLuint);
begin
  if Value <> FPackSkipRows then
  begin
    FPackSkipRows := Value;
    glPixelStoref(GL_PACK_SKIP_ROWS, Value);
  end;
end;

procedure TDGLStateCache.SetPackSwapBytes(const Value: TGLBoolean);
begin
  if Value <> FPackSwapBytes then
  begin
    FPackSwapBytes := Value;
    glPixelStorei(GL_PACK_SWAP_BYTES, byte(Value));
  end;
end;

procedure TDGLStateCache.SetPixelPackBufferBinding(const Value: TGLuint);
begin
  if Value <> FPixelPackBufferBinding then
  begin
    FPixelPackBufferBinding := Value;
    glBindBuffer(GL_PIXEL_PACK_BUFFER, Value);
  end;
end;

procedure TDGLStateCache.SetPixelUnpackBufferBinding(const Value: TGLuint);
begin
  if Value <> FPixelUnpackBufferBinding then
  begin
    FPixelUnpackBufferBinding := Value;
    glBindBuffer(GL_PIXEL_UNPACK_BUFFER, Value);
  end;
end;

procedure TDGLStateCache.SetPointFadeThresholdSize(const Value: TGLfloat);
begin
  if (Value <> FPointFadeThresholdSize) or FInsideList then
  begin
    if FInsideList then
      Include(FListStates[FCurrentList], sttPoint)
    else
      FPointFadeThresholdSize := Value;
    glPointParameterf(GL_POINT_FADE_THRESHOLD_SIZE, Value);
  end;
end;

procedure TDGLStateCache.SetPointSize(const Value: TGLfloat);
begin
  if (Value <> FPointSize) or FInsideList then
  begin
    if FInsideList then
      Include(FListStates[FCurrentList], sttPoint)
    else
      FPointSize := Value;
    glPointSize(Value);
  end;
end;

procedure TDGLStateCache.SetPointSpriteCoordOrigin(const Value: TGLEnum);
begin
  if (Value <> FPointSpriteCoordOrigin) or FInsideList then
  begin
    if FInsideList then
      Include(FListStates[FCurrentList], sttPoint)
    else
      FPointSpriteCoordOrigin := Value;
    glPointParameterf(GL_POINT_SPRITE_COORD_ORIGIN, Value);
  end;
end;

procedure TDGLStateCache.SetPolygonMode(const Value: TPolygonMode);
begin
  if (Value <> FPolygonMode) or FInsideList then
  begin
    if FInsideList then
      Include(FListStates[FCurrentList], sttPolygon)
    else
    begin
      FPolygonMode := Value;
      // FPolygonBackMode := Value;
    end;
    glPolygonMode(GL_FRONT_AND_BACK, cGLPolygonModeToGLEnum[Value]);
  end;
end;

procedure TDGLStateCache.SetPolygonOffset(const factor, units: TGLfloat);
begin
  if (factor <> FPolygonOffsetFactor) or (units <> FPolygonOffsetUnits) or FInsideList then
  begin
    if FInsideList then
      Include(FListStates[FCurrentList], sttPolygon)
    else
    begin
      FPolygonOffsetFactor := factor;
      FPolygonOffsetUnits  := units;
    end;
    glPolygonOffset(factor, units);
  end;
end;

procedure TDGLStateCache.SetPolygonOffsetFactor(const Value: TGLfloat);
begin
  if (Value <> FPolygonOffsetFactor) or FInsideList then
  begin
    if FInsideList then
      Include(FListStates[FCurrentList], sttPolygon)
    else
      FPolygonOffsetFactor := Value;
    glPolygonOffset(Value, FPolygonOffsetUnits);
  end;
end;

procedure TDGLStateCache.SetPolygonOffsetUnits(const Value: TGLfloat);
begin
  if (Value <> FPolygonOffsetUnits) or FInsideList then
  begin
    if FInsideList then
      Include(FListStates[FCurrentList], sttPolygon)
    else
      FPolygonOffsetUnits := Value;
    glPolygonOffset(FPolygonOffsetFactor, Value);
  end;
end;

procedure TDGLStateCache.SetPolygonSmoothHint(const Value: THintType);
begin
  if (Value <> FPolygonSmoothHint) or FInsideList then
  begin
    if FInsideList then
      Include(FListStates[FCurrentList], sttHint)
    else
      FPolygonSmoothHint := Value;
    glHint(GL_POLYGON_SMOOTH_HINT, cGLHintToGLEnum[Value]);
  end;
end;

procedure TDGLStateCache.SetMultisampleFilterHint(const Value: THintType);
begin
  if dglCheckExtension('GL_MULTISAMPLE_FILTER_HINT_NV') then
    if Value <> FMultisampleFilterHint then
    begin
      if FInsideList then
        Include(FListStates[FCurrentList], sttHint)
      else
        FMultisampleFilterHint := Value;
      glHint(GL_MULTISAMPLE_FILTER_HINT_NV, cGLHintToGLEnum[Value]);
    end;
end;

procedure TDGLStateCache.SetProvokingVertex(const Value: TGLEnum);
begin
  if Value <> FProvokingVertex then
  begin
    FProvokingVertex := Value;
    glProvokingVertex(Value);
  end;
end;

procedure TDGLStateCache.SetReadFrameBuffer(const Value: TGLuint);
begin
  if Value <> FReadFrameBuffer then
  begin
    FReadFrameBuffer := Value;
    glBindFramebuffer(GL_READ_FRAMEBUFFER, Value);
  end;
end;

procedure TDGLStateCache.SetRenderBuffer(const Value: TGLuint);
begin
  if Value <> FRenderBuffer then
  begin
    FRenderBuffer := Value;
    glBindRenderbuffer(GL_RENDERBUFFER, Value);
  end;
end;

procedure TDGLStateCache.SetSampleCoverage(const Value: TGLfloat; invert: TGLBoolean);
begin
  if (Value <> FSampleCoverageValue) or (invert <> FSampleCoverageInvert) or FInsideList then
  begin
    if FInsideList then
      Include(FListStates[FCurrentList], sttMultisample)
    else
    begin
      FSampleCoverageValue  := Value;
      FSampleCoverageInvert := invert;
    end;
    glSampleCoverage(Value, invert);
  end;
end;

procedure TDGLStateCache.SetSampleCoverageInvert(const Value: TGLBoolean);
begin
  if (Value <> FSampleCoverageInvert) or FInsideList then
  begin
    if FInsideList then
      Include(FListStates[FCurrentList], sttMultisample)
    else
      FSampleCoverageInvert := Value;
    glSampleCoverage(FSampleCoverageValue, Value);
  end;
end;

procedure TDGLStateCache.SetSampleCoverageValue(const Value: TGLfloat);
begin
  if (Value <> FSampleCoverageValue) or FInsideList then
  begin
    if FInsideList then
      Include(FListStates[FCurrentList], sttMultisample)
    else
      FSampleCoverageValue := Value;
    glSampleCoverage(Value, FSampleCoverageInvert);
  end;
end;

procedure TDGLStateCache.SetSampleMaskValue(Index: Integer; const Value: TGLbitfield);
begin
  if (FSampleMaskValue[Index] <> Value) or FInsideList then
  begin
    if FInsideList then
      Include(FListStates[FCurrentList], sttMultisample)
    else
      FSampleMaskValue[Index] := Value;
    glSampleMaski(Index, Value);
  end;
end;

procedure TDGLStateCache.SetScissorBox(const Value: TVector);
begin
  if not VectorEquals(FScissorBox, Value) or FInsideList then
  begin
    if FInsideList then
      Include(FListStates[FCurrentList], sttScissor)
    else
      FScissorBox := Value;
    glScissor(round(Value.V[0]), round(Value.V[1]), round(Value.V[2]), round(Value.V[3]));
  end;
end;

procedure TDGLStateCache.SetStencilBackWriteMask(const Value: TGLuint);
begin
  if (Value <> FStencilBackWriteMask) or FInsideList then
  begin
    if FInsideList then
      Include(FListStates[FCurrentList], sttStencilBuffer)
    else
      FStencilBackWriteMask := Value;
  end;
end;

procedure TDGLStateCache.SetStencilClearValue(const Value: TGLuint);
// {$IFDEF GLS_CACHE_MISS_CHECK}
// var I: TGLuint;
// {$ENDIF}
begin
  // {$IFDEF GLS_CACHE_MISS_CHECK}
  // glGetIntegerv(GL_STENCIL_CLEAR_VALUE, @I);
  // if FStencilClearValue <> I then
  // DGLSLogger.LogError(glsStateCashMissing + 'Stencil clear value');
  // {$ENDIF}
  if (Value <> FStencilClearValue) or FInsideList then
  begin
    if FInsideList then
      Include(FListStates[FCurrentList], sttStencilBuffer)
    else
      FStencilClearValue := Value;
    glClearStencil(Value);
  end;
end;

procedure TDGLStateCache.SetColorClearValue(const Value: TVector);
begin
  if not VectorEquals(Value, FColorClearValue) or FInsideList then
  begin
    if FInsideList then
      Include(FListStates[FCurrentList], sttColorBuffer)
    else
      FColorClearValue := Value;
    glClearColor(Value.V[0], Value.V[1], Value.V[2], Value.V[3]);
  end;
end;

procedure TDGLStateCache.SetColorMask(mask: TColorMask);
var
  I: Integer;
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
  glColorMask(ccRed in mask, ccGreen in mask, ccBlue in mask, ccAlpha in mask);
end;

procedure TDGLStateCache.SetStencilFuncSeparate(const face: TCullFaceMode; const func: TStencilFunction; const ref: TGLint; const mask: TGLuint);
// {$IFDEF GLS_CACHE_MISS_CHECK}
// var UI: TGLuint; I: TGLint;
// {$ENDIF}
begin
  // if (func<>FStencilFunc) or (ref<>FStencilRef) or (mask<>FStencilValueMask)
  // or FInsideList then
  // {$IFDEF GLS_CACHE_MISS_CHECK}
  // glGetIntegerv(GL_STENCIL_FUNC, @UI);
  // if cGLComparisonFunctionToGLEnum[FStencilFunc] <> UI then
  // DGLSLogger.LogError(glsStateCashMissing + 'Stencil function');
  // glGetIntegerv(GL_STENCIL_REF, @I);
  // if FStencilRef <> I then
  // DGLSLogger.LogError(glsStateCashMissing + 'Stencil reference');
  // DGLSLogger.LogError(glsStateCashMissing + 'Stencil function');
  // glGetIntegerv(GL_STENCIL_VALUE_MASK, @UI);
  // if FStencilValueMask <> UI then
  // DGLSLogger.LogError(glsStateCashMissing + 'Stencil value mask');
  // {$ENDIF}
  begin
    if FInsideList then
      Include(FListStates[FCurrentList], sttStencilBuffer)
    else
      case face of
        cmFront:
          begin
            FStencilFunc      := func;
            FStencilRef       := ref;
            FStencilValueMask := mask;
          end;
        cmBack:
          begin
            FStencilBackFunc      := func;
            FStencilBackRef       := ref;
            FStencilBackValueMask := mask;
          end;
        cmFrontAndBack:
          begin
            FStencilFunc          := func;
            FStencilRef           := ref;
            FStencilValueMask     := mask;
            FStencilBackFunc      := func;
            FStencilBackRef       := ref;
            FStencilBackValueMask := mask;
          end;
      end;

    glStencilFuncSeparate(cGLCullFaceModeToGLEnum[face], cGLComparisonFunctionToGLEnum[func], ref, mask);
  end;
end;

procedure TDGLStateCache.SetStencilFunc(const func: TStencilFunction; const ref: TGLint; const mask: TGLuint);
begin
  if (func <> FStencilFunc) or (ref <> FStencilRef) or (mask <> FStencilValueMask) or FInsideList then
  begin
    if FInsideList then
      Include(FListStates[FCurrentList], sttStencilBuffer)
    else
    begin
      FStencilFunc      := func;
      FStencilRef       := ref;
      FStencilValueMask := mask;
    end;
    glStencilFunc(cGLComparisonFunctionToGLEnum[func], ref, mask);
  end;
end;

procedure TDGLStateCache.SetStencilOp(const fail, zfail, zpass: TStencilOp);
// {$IFDEF GLS_CACHE_MISS_CHECK}
// var I: TGLuint;
// {$ENDIF}
begin
  // {$IFDEF GLS_CACHE_MISS_CHECK}
  // glGetIntegerv(GL_STENCIL_FAIL, @I);
  // if cGLStencilOpToGLEnum[FStencilFail] <> I then
  // DGLSLogger.LogError(glsStateCashMissing + 'Stencil fail');
  // glGetIntegerv(GL_STENCIL_PASS_DEPTH_FAIL, @I);
  // if cGLStencilOpToGLEnum[FStencilPassDepthFail] <> I then
  // DGLSLogger.LogError(glsStateCashMissing + 'Stencil zfail');
  // glGetIntegerv(GL_STENCIL_PASS_DEPTH_PASS, @I);
  // if cGLStencilOpToGLEnum[FStencilPassDepthPass] <> I then
  // DGLSLogger.LogError(glsStateCashMissing + 'Stencil zpass');
  // {$ENDIF}
  if (fail <> FStencilFail) or (zfail <> FStencilPassDepthFail) or (zpass <> FStencilPassDepthPass) or FInsideList then
  begin
    if FInsideList then
      Include(FListStates[FCurrentList], sttStencilBuffer)
    else
    begin
      FStencilFail          := fail;
      FStencilPassDepthFail := zfail;
      FStencilPassDepthPass := zpass;
    end;
    glStencilOp(cGLStencilOpToGLEnum[fail], cGLStencilOpToGLEnum[zfail], cGLStencilOpToGLEnum[zpass]);
  end;
end;

procedure TDGLStateCache.SetStencilOpSeparate(const face: TCullFaceMode; const sfail, dpfail, dppass: TStencilOp);
begin
  if FInsideList then
    Include(FListStates[FCurrentList], sttStencilBuffer)
  else
    case face of
      cmFront:
        begin
          FStencilFail          := sfail;
          FStencilPassDepthFail := dpfail;
          FStencilPassDepthPass := dppass;
        end;
      cmBack:
        begin
          FStencilBackFail          := sfail;
          FStencilBackPassDepthFail := dpfail;
          FStencilBackPassDepthPass := dppass;
        end;
      cmFrontAndBack:
        begin
          FStencilFail              := sfail;
          FStencilPassDepthFail     := dpfail;
          FStencilPassDepthPass     := dppass;
          FStencilBackFail          := sfail;
          FStencilBackPassDepthFail := dpfail;
          FStencilBackPassDepthPass := dppass;
        end;
    end;

  glStencilOpSeparate(cGLCullFaceModeToGLEnum[face], cGLStencilOpToGLEnum[sfail], cGLStencilOpToGLEnum[dpfail], cGLStencilOpToGLEnum[dppass]);
end;

procedure TDGLStateCache.SetStencilWriteMask(const Value: TGLuint);
// {$IFDEF GLS_CACHE_MISS_CHECK}
// var I: TGLuint;
// {$ENDIF}
begin
  // {$IFDEF GLS_CACHE_MISS_CHECK}
  // glGetIntegerv(GL_STENCIL_WRITEMASK, @I);
  // if FStencilWriteMask <> I then
  // DGLSLogger.LogError(glsStateCashMissing + 'Stencil write mask');
  // {$ENDIF}
  if (Value <> FStencilWriteMask) or FInsideList then
  begin
    if FInsideList then
      Include(FListStates[FCurrentList], sttStencilBuffer)
    else
      FStencilWriteMask := Value;
    glStencilMaskSeparate(GL_FRONT, Value);
  end;
end;

procedure TDGLStateCache.SetTextureBinding(Index: Integer; target: TDGLTextureTarget; const Value: TGLuint);
var
  lastActiveTexture: TGLuint;
begin
  if target = ttNoShape then
    exit;
  if (Value <> FTextureBinding[Index, target]) or FInsideList then
  begin
    if FInsideList then
      Include(FListStates[FCurrentList], sttTexture)
    else
      FTextureBinding[Index, target] := Value;
    lastActiveTexture                := ActiveTexture;
    ActiveTexture                    := Index;
    glBindTexture(cGLTexTypeToGLEnum[target], Value);
    ActiveTexture := lastActiveTexture;
  end;
  FTextureBindingTime[Index, target] := GLSTime;
end;

function TDGLStateCache.GetActiveTextureEnabled(target: TDGLTextureTarget): Boolean;
begin
  Result := FActiveTextureEnabling[FActiveTexture][target];
end;

procedure TDGLStateCache.SetActiveTextureEnabled(target: TDGLTextureTarget; const Value: Boolean);
var
  glTarget: TGLEnum;
begin
  glTarget := DecodeGLTextureTarget(target);
  // if FForwardContext or not IsTargetSupported(glTarget) then exit;
  if (Value <> FActiveTextureEnabling[FActiveTexture][target]) or FInsideList then
  begin
    if FInsideList then
      Include(FListStates[FCurrentList], sttEnable)
    else
      FActiveTextureEnabling[FActiveTexture][target] := Value;
    if Value then
      glEnable(glTarget)
    else
      glDisable(glTarget);
  end;
end;

procedure TDGLStateCache.SetTextureCompressionHint(const Value: THintType);
begin
  if (Value <> FTextureCompressionHint) or FInsideList then
  begin
    if FInsideList then
      Include(FListStates[FCurrentList], sttHint)
    else
      FTextureCompressionHint := Value;
    glHint(GL_TEXTURE_COMPRESSION_HINT, cGLHintToGLEnum[Value]);
  end;
end;

procedure TDGLStateCache.SetTransformFeedbackBufferBinding(const Value: TGLuint);
begin
  if (Value <> FTransformFeedbackBufferBinding) or FInsideList then
  begin
    FTransformFeedbackBufferBinding := Value;
    glBindBuffer(GL_TRANSFORM_FEEDBACK_BUFFER, Value);
  end;
end;

procedure TDGLStateCache.SetEnableTextureCubeMapSeamless(const Value: TGLBoolean);
begin
  if Value <> FEnableTextureCubeMapSeamless then
  begin
    FEnableTextureCubeMapSeamless := Value;
    if Value = True then
      glEnable(GL_TEXTURE_CUBE_MAP_SEAMLESS)
    else
      glDisable(GL_TEXTURE_CUBE_MAP_SEAMLESS);
  end;
end;
{$IFDEF DGL_DEPRECATED}

// THIS DEPRACATED PART HAS BEEN REPLACED BY THE RENDER MANAGER
procedure TDGLStateCache.NewList(list: TGLuint; mode: TGLEnum);
var
  I: TGLuint;
begin
  // Assert(mode = GL_COMPILE,'Compile & executing not supported by TDGLStateCache');
  FCurrentList := list - 1;
  while High(FListStates) < Integer(FCurrentList) do
    SetLength(FListStates, 2 * Length(FListStates));

  FListStates[FCurrentList] := [];
  FInsideList               := True;
  // Reset VBO binding and client attribute
  if GL_ARB_vertex_buffer_object then
  begin
    ArrayBufferBinding   := 0;
    ElementBufferBinding := 0;
    // @TODO Use the counter FromVBOManager
    for I := 0 to 15 do
      glDisableVertexAttribArray(I);
  end;
  // @TODO Reset VAO binding and client attribute
  NewList(list, mode);
end;

procedure TDGLStateCache.EndList;
begin
  // @TODO VAO/VBO UnBinding
  // glEndList;
  FInsideList := False;
end;

// Must Be Replaced by GLDrawArray
procedure TDGLStateCache.CallList(list: TGLuint);
begin
  while High(FListStates) < Integer(list) do
    SetLength(FListStates, 2 * Length(FListStates));

  if FListStates[list - 1] <> [] then
  begin
    PushAttrib(FListStates[list - 1]);
    glCallList(list);
    PopAttrib;
  end
  else
    glCallList(list);
end;
{$ENDIF}

procedure TDGLStateCache.SetUniformBufferBinding(const Value: TGLuint);
begin
  Assert(not FInsideList);
  if Value <> FUniformBufferBinding then
  begin
    FUniformBufferBinding := Value;
    glBindBuffer(GL_UNIFORM_BUFFER, Value);
  end;
end;

procedure TDGLStateCache.SetBufferIndexedBinding(const Value: TGLuint; ATarget: TDGLBufferBindingTarget; AIndex: TGLuint; ABufferSize: GLsizeiptr);
begin
  Assert(not FInsideList);
  if (FUBOStates[ATarget, AIndex].FUniformBufferBinding <> Value) or (FUBOStates[ATarget, AIndex].FOffset > 0) or (FUBOStates[ATarget, AIndex].FSize <> ABufferSize) then
  begin
    case ATarget of
      bbtUniform:
        FUniformBufferBinding := Value;
      bbtTransformFeedBack:
        FTransformFeedbackBufferBinding := Value;
    end;
    FUBOStates[ATarget, AIndex].FUniformBufferBinding := Value;
    FUBOStates[ATarget, AIndex].FOffset               := 0;
    FUBOStates[ATarget, AIndex].FSize                 := ABufferSize;
    glBindBufferBase(cGLBufferBindingTarget[ATarget], AIndex, Value);
  end
  else
    case ATarget of
      bbtUniform:
        SetUniformBufferBinding(Value);
      bbtTransformFeedBack:
        SetTransformFeedbackBufferBinding(Value);
    end;
end;

procedure TDGLStateCache.SetBufferIndexedBinding(const Value: TGLuint; ATarget: TDGLBufferBindingTarget; AIndex: TGLuint; AOffset: GLintptr; ARangeSize: GLsizeiptr);
begin
  Assert(not FInsideList);
  if (FUBOStates[ATarget, AIndex].FUniformBufferBinding <> Value) or (FUBOStates[ATarget, AIndex].FOffset <> AOffset) or (FUBOStates[ATarget, AIndex].FSize <> ARangeSize) then
  begin
    case ATarget of
      bbtUniform:
        FUniformBufferBinding := Value;
      bbtTransformFeedBack:
        FTransformFeedbackBufferBinding := Value;
    end;
    FUBOStates[ATarget, AIndex].FUniformBufferBinding := Value;
    FUBOStates[ATarget, AIndex].FOffset               := AOffset;
    FUBOStates[ATarget, AIndex].FSize                 := ARangeSize;
    glBindBufferRange(cGLBufferBindingTarget[ATarget], AIndex, Value, AOffset, ARangeSize);
  end;
end;

function TDGLStateCache.GetMaxTextureUnits: TGLuint;
begin
  if FMaxTextureUnits = 0 then
    glGetIntegerv(GL_MAX_TEXTURE_IMAGE_UNITS_ARB, @FMaxTextureUnits);
  Result := FMaxTextureUnits;
end;

procedure TDGLStateCache.SetUnpackAlignment(const Value: TGLuint);
begin
  if Value <> FUnpackAlignment then
  begin
    FUnpackAlignment := Value;
    glPixelStoref(GL_UNPACK_ALIGNMENT, Value);
  end;
end;

procedure TDGLStateCache.SetUnpackImageHeight(const Value: TGLuint);
begin
  if Value <> FUnpackImageHeight then
  begin
    FUnpackImageHeight := Value;
    glPixelStoref(GL_UNPACK_IMAGE_HEIGHT, Value);
  end;
end;

procedure TDGLStateCache.SetUnpackLSBFirst(const Value: TGLBoolean);
begin
  if Value <> FUnpackLSBFirst then
  begin
    FUnpackLSBFirst := Value;
    glPixelStorei(GL_UNPACK_LSB_FIRST, byte(Value));
  end;
end;

procedure TDGLStateCache.SetUnpackRowLength(const Value: TGLuint);
begin
  if Value <> FUnpackRowLength then
  begin
    FUnpackRowLength := Value;
    glPixelStoref(GL_UNPACK_ROW_LENGTH, Value);
  end;
end;

procedure TDGLStateCache.SetUnpackSkipImages(const Value: TGLuint);
begin
  if Value <> FUnpackSkipImages then
  begin
    FUnpackSkipImages := Value;
    glPixelStoref(GL_UNPACK_SKIP_IMAGES, Value);
  end;
end;

procedure TDGLStateCache.SetUnpackSkipPixels(const Value: TGLuint);
begin
  if Value <> FUnpackSkipPixels then
  begin
    FUnpackSkipPixels := Value;
    glPixelStoref(GL_UNPACK_SKIP_PIXELS, Value);
  end;
end;

procedure TDGLStateCache.SetUnpackSkipRows(const Value: TGLuint);
begin
  if Value <> FUnpackSkipRows then
  begin
    FUnpackSkipRows := Value;
    glPixelStoref(GL_UNPACK_SKIP_ROWS, Value);
  end;
end;

procedure TDGLStateCache.SetUnpackSwapBytes(const Value: TGLBoolean);
begin
  if Value <> FUnpackSwapBytes then
  begin
    FUnpackSwapBytes := Value;
    glPixelStorei(GL_UNPACK_SWAP_BYTES, byte(Value));
  end;
end;

procedure TDGLStateCache.SetViewPort(const Value: TVector4i);
begin
  if (not(VectorEquals(Value, FViewPort)) or FInsideList) then
  begin
    if FInsideList then
      Include(FListStates[FCurrentList], sttViewport)
    else
      FViewPort := Value;
    glViewport(Value.V[0], Value.V[1], Value.V[2], Value.V[3]);
  end;
end;

procedure TDGLStateCache.SetFFPLight(Value: Boolean);
begin
  // FFFPLight := Value and not FForwardContext;
  FFFPLight := true;
end;

// function TDGLStateCache.GetMaxLights: Integer;
// begin
// if FMaxLights = 0 then
// if FForwardContext then
// FMaxLights := MAX_HARDWARE_LIGHT
// else
// glGetIntegerv(GL_MAX_LIGHTS, @FMaxLights);
// Result := FMaxLights;
// end;

function TDGLStateCache.GetLightEnabling(I: Integer): Boolean;
begin
  Result := FLightEnabling[I];
end;

procedure TDGLStateCache.SetLightEnabling(I: Integer; Value: Boolean);
var
  J, K: Integer;
begin
  if (FLightEnabling[I] <> Value) or FInsideList then
  begin
    // if FInsideList then
    // Include(FListStates[FCurrentList], sttLighting)
    // else
    FLightEnabling[I] := Value;

    // if FFFPLight then
    // begin
    // if Value then
    // glEnable(GL_LIGHT0 + I)
    // else
    // glDisable(GL_LIGHT0 + I);
    // end;

    K     := 0;
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

function TDGLStateCache.GetLightIndicesAsAddress: PGLInt;
begin
  Result := @FLightIndices[0];
end;

function TDGLStateCache.GetLightStateAsAddress: Pointer;
var
  C: Integer;
begin
  C := MinInteger(FLightNumber, MAX_SHADER_LIGHT);
  if FShaderLightStatesChanged then
  begin
    if C > 0 then
    begin
      // if glVERSION_3_3 then
      // begin
      Move(FLightStates.Position, FShaderLightStates.Position, sizeof(FShaderLightStates.Position));
      Move(FLightStates.Ambient, FShaderLightStates.Ambient, sizeof(FShaderLightStates.Ambient));
      Move(FLightStates.Diffuse, FShaderLightStates.Diffuse, sizeof(FShaderLightStates.Diffuse));
      Move(FLightStates.Specular, FShaderLightStates.Specular, sizeof(FShaderLightStates.Specular));
      Move(FLightStates.SpotDirection, FShaderLightStates.SpotDirection, sizeof(FShaderLightStates.SpotDirection));
      Move(FLightStates.SpotCosCutoffExponent, FShaderLightStates.SpotCosCutoffExponent, sizeof(FShaderLightStates.SpotCosCutoffExponent));
      Move(FLightStates.Attenuation, FShaderLightStates.Attenuation, sizeof(FShaderLightStates.Attenuation));
      // end
      // else
      // begin
      // for I := C - 1 downto 0 do
      // begin
      // J := FLightIndices[I];
      // FShaderLightStates.Position[I] := FLightStates.Position[J];
      // FShaderLightStates.Ambient[I] := FLightStates.Ambient[J];
      // FShaderLightStates.Diffuse[I] := FLightStates.Diffuse[J];
      // FShaderLightStates.Specular[I] := FLightStates.Specular[J];
      // FShaderLightStates.SpotDirection[I] := FLightStates.SpotDirection[J];
      // FShaderLightStates.SpotCosCutoffExponent[I] := FLightStates.SpotCosCutoffExponent[J];
      // FShaderLightStates.Attenuation[I] := FLightStates.Attenuation[J];
      // end;
      // end;
    end
    else
      FillChar(FShaderLightStatesChanged, sizeof(FShaderLightStatesChanged), $00);
    FShaderLightStatesChanged := False;
  end;

  Result := @FShaderLightStates;
end;

function TDGLStateCache.GetLightPosition(I: Integer): TVector;
begin
  Result := FLightStates.Position[I];
end;

procedure TDGLStateCache.SetLightPosition(I: Integer; const Value: TVector);
begin
  if not VectorEquals(Value, FLightStates.Position[I]) then
  begin
    FLightStates.Position[I]  := Value;
    FShaderLightStatesChanged := True;
    if Assigned(FOnLightsChanged) then
      FOnLightsChanged(Self);
  end;
end;

function TDGLStateCache.GetLightSpotDirection(I: Integer): TAffineVector;
begin
  Result := AffineVectorMake(FLightStates.SpotDirection[I]);
end;

procedure TDGLStateCache.SetLightSpotDirection(I: Integer; const Value: TAffineVector);
begin
  if not VectorEquals(Value, AffineVectorMake(FLightStates.SpotDirection[I])) then
  begin
    FLightStates.SpotDirection[I] := VectorMake(Value);
    FShaderLightStatesChanged     := True;
    if Assigned(FOnLightsChanged) then
      FOnLightsChanged(Self);
  end;
end;

function TDGLStateCache.GetLightAmbient(I: Integer): TVector;
begin
  Result := FLightStates.Ambient[I];
end;

procedure TDGLStateCache.SetLightAmbient(I: Integer; const Value: TVector);
begin
  if not VectorEquals(Value, FLightStates.Ambient[I]) or FInsideList then
  begin
    // if FInsideList then
    // Include(FListStates[FCurrentList], sttLighting)
    // else
    FLightStates.Ambient[I] := Value;

    // if FFFPLight then
    // glLightfv(GL_LIGHT0 + I, GL_AMBIENT, @Value);

    FShaderLightStatesChanged := True;
    if Assigned(FOnLightsChanged) then
      FOnLightsChanged(Self);
  end;
end;

function TDGLStateCache.GetLightDiffuse(I: Integer): TVector;
begin
  Result := FLightStates.Diffuse[I];
end;

procedure TDGLStateCache.SetLightDiffuse(I: Integer; const Value: TVector);
begin
  if not VectorEquals(Value, FLightStates.Diffuse[I]) or FInsideList then
  begin
    // if FInsideList then
    // Include(FListStates[FCurrentList], sttLighting)
    // else
    FLightStates.Diffuse[I] := Value;

    // if FFFPLight then
    // glLightfv(GL_LIGHT0 + I, GL_DIFFUSE, @Value);

    FShaderLightStatesChanged := True;
    if Assigned(FOnLightsChanged) then
      FOnLightsChanged(Self);
  end;
end;

function TDGLStateCache.GetLightSpecular(I: Integer): TVector;
begin
  Result := FLightStates.Specular[I];
end;

procedure TDGLStateCache.SetLightSpecular(I: Integer; const Value: TVector);
begin
  if not VectorEquals(Value, FLightStates.Specular[I]) or FInsideList then
  begin
    // if FInsideList then
    // Include(FListStates[FCurrentList], sttLighting)
    // else
    FLightStates.Specular[I] := Value;

    // if FFFPLight then
    // glLightfv(GL_LIGHT0 + I, GL_SPECULAR, @Value);

    FShaderLightStatesChanged := True;
    if Assigned(FOnLightsChanged) then
      FOnLightsChanged(Self);
  end;
end;

function TDGLStateCache.GetSpotCutoff(I: Integer): Single;
begin
  Result := FSpotCutoff[I];
end;

procedure TDGLStateCache.SetSpotCutoff(I: Integer; const Value: Single);
begin
  if (Value <> FSpotCutoff[I]) or FInsideList then
  begin
    // if FInsideList then
    // Include(FListStates[FCurrentList], sttLighting)
    // else
    // begin
    FSpotCutoff[I]                             := Value;
    FLightStates.SpotCosCutoffExponent[I].V[0] := cos(DegToRadian(Value));
    // end;

    // if FFFPLight then
    // glLightfv(GL_LIGHT0 + I, GL_SPOT_CUTOFF, @Value);

    FShaderLightStatesChanged := True;
    if Assigned(FOnLightsChanged) then
      FOnLightsChanged(Self);
  end;
end;

function TDGLStateCache.GetSpotExponent(I: Integer): Single;
begin
  Result := FLightStates.SpotCosCutoffExponent[I].V[1];
end;

procedure TDGLStateCache.SetSpotExponent(I: Integer; const Value: Single);
begin
  if (Value <> FLightStates.SpotCosCutoffExponent[I].V[1]) or FInsideList then
  begin
    // if FInsideList then
    // Include(FListStates[FCurrentList], sttLighting)
    // else
    FLightStates.SpotCosCutoffExponent[I].V[1] := Value;

    // if FFFPLight then
    // glLightfv(GL_LIGHT0 + I, GL_SPOT_EXPONENT, @Value);

    FShaderLightStatesChanged := True;
    if Assigned(FOnLightsChanged) then
      FOnLightsChanged(Self);
  end;
end;

function TDGLStateCache.GetConstantAtten(I: Integer): Single;
begin
  Result := FLightStates.Attenuation[I].V[0];
end;

procedure TDGLStateCache.SetConstantAtten(I: Integer; const Value: Single);
begin
  if (Value <> FLightStates.Attenuation[I].V[0]) or FInsideList then
  begin
    // if FInsideList then
    // Include(FListStates[FCurrentList], sttLighting)
    // else
    FLightStates.Attenuation[I].V[0] := Value;

    // if FFFPLight then
    // glLightfv(GL_LIGHT0 + I, GL_CONSTANT_ATTENUATION, @Value);

    FShaderLightStatesChanged := True;
    if Assigned(FOnLightsChanged) then
      FOnLightsChanged(Self);
  end;
end;

function TDGLStateCache.GetLinearAtten(I: Integer): Single;
begin
  Result := FLightStates.Attenuation[I].V[1];
end;

procedure TDGLStateCache.SetLinearAtten(I: Integer; const Value: Single);
begin
  if (Value <> FLightStates.Attenuation[I].V[1]) or FInsideList then
  begin
    // if FInsideList then
    // Include(FListStates[FCurrentList], sttLighting)
    // else
    FLightStates.Attenuation[I].V[1] := Value;

    // if FFFPLight then
    // glLightfv(GL_LIGHT0 + I, GL_LINEAR_ATTENUATION, @Value);

    FShaderLightStatesChanged := True;
    if Assigned(FOnLightsChanged) then
      FOnLightsChanged(Self);
  end;
end;

function TDGLStateCache.GetQuadAtten(I: Integer): Single;
begin
  Result := FLightStates.Attenuation[I].V[2];
end;

procedure TDGLStateCache.SetQuadAtten(I: Integer; const Value: Single);
begin
  if (Value <> FLightStates.Attenuation[I].V[2]) or FInsideList then
  begin
    // if FInsideList then
    // Include(FListStates[FCurrentList], sttLighting)
    // else
    FLightStates.Attenuation[I].V[2] := Value;

    // if FFFPLight then
    // glLightfv(GL_LIGHT0 + I, GL_QUADRATIC_ATTENUATION, @Value);

    FShaderLightStatesChanged := True;
    if Assigned(FOnLightsChanged) then
      FOnLightsChanged(Self);
  end;
end;

// procedure TDGLStateCache.SetForwardContext(Value: Boolean);
// begin
// if Value <> FForwardContext then
// begin
// FForwardContext := Value;
// if Value then
// begin
// SetFFPlight(False);
// end;
// end;
// end;


// SeTDGLColorIgnoring
//

procedure TDGLStateCache.SeTDGLColorWriting(flag: Boolean);
begin
  if (FColorWriting <> flag) or FInsideList then
  begin
    if FInsideList then
      Include(FListStates[FCurrentList], sttColorBuffer)
    else
      FColorWriting := flag;
    glColorMask(flag, flag, flag, flag);
  end;
end;

// InverTDGLFrontFace
//

procedure TDGLStateCache.InverTDGLFrontFace;
begin
  if FFrontFace = fwCounterClockWise then
    FrontFace := fwClockWise
  else
    FrontFace := fwCounterClockWise;
end;

// SeTDGLState
//
// procedure TDGLStateCache.SetGLState(const aState : TDGLState);
// begin
// Enable(aState);
// end;
//
// UnSeTDGLState
//
// procedure TDGLStateCache.UnSetGLState(const aState : TDGLState);
// begin
// Disable(aState);
// end;

// ReseTDGLPolygonMode
//

procedure TDGLStateCache.ResetGLPolygonMode;
begin
  glPolygonMode(GL_FRONT_AND_BACK, GL_FILL);
  FPolygonMode := pmFill;
  // FPolygonBackMode := pmFill;
end;

// ReseTDGLMaterialColors
//

procedure TDGLStateCache.ResetGLMaterialColors;
begin
  // glMaterialfv(GL_FRONT_AND_BACK, GL_AMBIENT, @clrGray20);
  // glMaterialfv(GL_FRONT_AND_BACK, GL_DIFFUSE, @clrGray80);
  // glMaterialfv(GL_FRONT_AND_BACK, GL_SPECULAR, @clrBlack);
  // glMaterialfv(GL_FRONT_AND_BACK, GL_EMISSION, @clrBlack);
  // glMateriali(GL_FRONT_AND_BACK, GL_SHININESS, 0);
  FillChar(FFrontBackColors, sizeof(FFrontBackColors), 127);
  FFrontBackShininess[0] := 0;
  FFrontBackShininess[1] := 0;
end;

// ReseTDGLTexture
//

procedure TDGLStateCache.ResetGLTexture(const TextureUnit: Integer);
var
  t:        TDGLTextureTarget;
  glTarget: TGLEnum;
begin
  glActiveTexture(GL_TEXTURE0 + TextureUnit);
  for t := Low(TDGLTextureTarget) to High(TDGLTextureTarget) do
  begin
    glTarget := DecodeGLTextureTarget(t);
    if IsTargetSupported(glTarget) then
    begin
      glBindTexture(glTarget, 0);
      FTextureBinding[TextureUnit, t] := 0;
    end;
  end;
  glActiveTexture(GL_TEXTURE0);
  FActiveTexture := 0;
end;

// ReseTDGLCurrentTexture
//

procedure TDGLStateCache.ResetGLCurrentTexture;
var
  a:        TGLint;
  t:        TDGLTextureTarget;
  glTarget: TGLEnum;
begin
  if GL_ARB_multitexture then
  begin
    for a := MaxTextureImageUnits - 1 to 0 do
    begin
      glActiveTexture(GL_TEXTURE0 + a);
      for t := Low(TDGLTextureTarget) to High(TDGLTextureTarget) do
      begin
        glTarget := DecodeGLTextureTarget(t);
        if IsTargetSupported(glTarget) then
        begin
          glBindTexture(glTarget, 0);
          FTextureBinding[a, t] := 0;
        end;
      end;
    end;
  end
  else
    for t := Low(TDGLTextureTarget) to High(TDGLTextureTarget) do
    begin
      glTarget := DecodeGLTextureTarget(t);
      if IsTargetSupported(glTarget) then
      begin
        glBindTexture(glTarget, 0);
        FTextureBinding[0, t] := 0;
      end;
    end;
end;

// ReseTDGLFrontFace
//

procedure TDGLStateCache.ResetGLFrontFace;
begin
  glFrontFace(GL_CCW);
  FFrontFace := fwCounterClockWise;
end;

procedure TDGLStateCache.SetGLFrontFaceCW;
begin
  if FFrontFace = fwCounterClockWise then
  begin
    glFrontFace(GL_CW);
    FFrontFace := fwClockWise;
  end;
end;

// ResetAll
//

procedure TDGLStateCache.ResetAll;
begin
  { .$WARN SYMBOL_DEPRECATED OFF }
  // ReseTDGLPolygonMode;
  // ReseTDGLMaterialColors;
  ResetGLCurrentTexture;
  ResetGLFrontFace;
  { .$WARN SYMBOL_DEPRECATED ON }
end;


// SetGLMaterialColors
//

procedure TDGLStateCache.SetGLMaterialColors(const aFace: TCullFaceMode; const emission, Ambient, Diffuse, Specular: TVector; const shininess: Integer);
var
  I: Integer;
  // currentFace: TGLenum;
begin
  Assert((aFace = cmFront) or (aFace = cmBack), 'Only cmFront or cmBack supported');
  I := Integer(aFace);
  // currentFace := cGLCullFaceModeToGLEnum[aFace];

  if (FFrontBackShininess[I] <> shininess) or FInsideList then
  begin
    if not FInsideList then
      FFrontBackShininess[I] := shininess;
  end;
  if not AffineVectorEquals(FFrontBackColors[I][0], emission) or FInsideList then
  begin
    if not FInsideList then
      SetVector(FFrontBackColors[I][0], emission);
  end;
  if not AffineVectorEquals(FFrontBackColors[I][1], Ambient) or FInsideList then
  begin
    if not FInsideList then
      SetVector(FFrontBackColors[I][1], Ambient);
  end;
  if not VectorEquals(FFrontBackColors[I][2], Diffuse) or FInsideList then
  begin
    if not FInsideList then
      SetVector(FFrontBackColors[I][2], Diffuse);
  end;
  if not AffineVectorEquals(FFrontBackColors[I][3], Specular) or FInsideList then
  begin
    if not FInsideList then
      SetVector(FFrontBackColors[I][3], Specular);
  end;
  // if FInsideList then Include(FListStates[FCurrentList], sttLighting);
end;

function TDGLStateCache.MaterialEmission(const aFace: TCullFaceMode): TVector;
var
  I: Integer;
begin
  Assert((aFace = cmFront) or (aFace = cmBack), 'Only cmFront or cmBack supported');
  I      := Integer(aFace);
  Result := FFrontBackColors[I][0];
end;

function TDGLStateCache.MaterialAmbient(const aFace: TCullFaceMode): TVector;
var
  I: Integer;
begin
  Assert((aFace = cmFront) or (aFace = cmBack), 'Only cmFront or cmBack supported');
  I      := Integer(aFace);
  Result := FFrontBackColors[I][1];
end;

function TDGLStateCache.MaterialDiffuse(const aFace: TCullFaceMode): TVector;
var
  I: Integer;
begin
  Assert((aFace = cmFront) or (aFace = cmBack), 'Only cmFront or cmBack supported');
  I      := Integer(aFace);
  Result := FFrontBackColors[I][2];
end;

function TDGLStateCache.MaterialSpecular(const aFace: TCullFaceMode): TVector;
var
  I: Integer;
begin
  Assert((aFace = cmFront) or (aFace = cmBack), 'Only cmFront or cmBack supported');
  I      := Integer(aFace);
  Result := FFrontBackColors[I][3];
end;

function TDGLStateCache.MaterialShininess(const aFace: TCullFaceMode): Integer;
var
  I: Integer;
begin
  Assert((aFace = cmFront) or (aFace = cmBack), 'Only cmFront or cmBack supported');
  I      := Integer(aFace);
  Result := FFrontBackShininess[I];
end;

{$IFDEF GLS_REGIONS}{$ENDREGION}{$ENDIF}

initialization

MAX_HARDWARE_LIGHT                  := 16;
MAX_SHADER_LIGHT                    := 16;
MAX_HARDWARE_UNIFORM_BUFFER_BINDING := 84;
MAX_HARDWARE_TEXTURE_UNIT           := 48;
// glGetIntegerv(GL_MAX_TEXTURE_IMAGE_UNITS,@MAX_HARDWARE_TEXTURE_UNIT);
// glGetIntegerv(GL_MAX_UNIFORM_BUFFER_BINDINGS,@MAX_HARDWARE_UNIFORM_BUFFER_BINDING);

finalization

end.
