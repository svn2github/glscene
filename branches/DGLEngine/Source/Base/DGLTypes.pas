//
// This unit is part of the DGLEngine Project, http://glscene.org
//
{: DGLTypes<p>

   Reference all needed types, vars and consts.
   This file is here for removing many units dependancies.<p>

 <b>History : </b><font size=-1><ul>
      <li>21/12/15 - JD - Created
   </ul></font><p>
}
unit DGLTypes;

interface

uses
  dglOpenGL, DGLVectorTypes, DGLVectorMaths;

Const
  GLS_VERTEX_ATTR_NUM = 16;

type

  TDGLStateType = (sttCurrent, sttPoint, sttLine, sttPolygon,
    sttPixelMode,  sttDepthBuffer, sttAccumBuffer,
    sttStencilBuffer, sttViewport, sttTransform, sttEnable, sttColorBuffer,
    sttHint, sttEval, sttList, sttTexture, sttScissor,
    sttMultisample);
  TDGLStateTypes = set of TDGLStateType;


  TDGLMeshPrimitive = (
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

  TDGLMeshPrimitives = set of TDGLMeshPrimitive;


  // TDGLState
  //
  //: Reflects all relevant (binary) states of OpenGL subsystem
  TDGLState = (stBlend, stCullFace, stDepthTest, stDither,
    stLineSmooth, stColorLogicOp, stPolygonSmooth,  stScissorTest, stStencilTest,
    stPolygonOffsetPoint, stPolygonOffsetLine, stPolygonOffsetFill,stDepthClamp);


  TDGLStates = set of TDGLState;


  TDGLBufferBindingTarget = (bbtUniform, bbtTransformFeedBack);

  TUBOStates = record
    FUniformBufferBinding: TGLuint;
    FOffset: TGLintptr;
    FSize: TGLsizeiptr;
  end;

  TDGLShaderMaterialLevel = (mlAuto, mlBaseMaterial, mlMultitexturing, mlShader);

  TComparisonFunction = (cfNever, cfAlways, cfLess, cfLEqual, cfEqual,
    cfGreater, cfNotEqual, cfGEqual);
  TStencilFunction = TComparisonFunction;
  TDepthFunction = TComparisonFunction;

  TBlendFunction = (bfZero, bfOne,
    bfSrcColor, bfOneMinusSrcColor, bfDstColor, bfOneMinusDstColor,
    bfSrcAlpha, bfOneMinusSrcAlpha, bfDstAlpha, bfOneMinusDstAlpha,
    bfOneMinusConstantColor,
    bfConstantAlpha, bfOneMinusConstantAlpha,
    bfSrcAlphaSat);

  TDstBlendFunction = bfZero..bfOneMinusConstantAlpha;

  TBlendEquation = (beAdd, beSubtract, beReverseSubtract, beMin, beMax);

  TStencilOp = (soKeep, soZero, soReplace, soIncr, soDecr, soInvert, soIncrWrap, soDecrWrap);
  TDGLStencilPrecision = (spDefault, sp1bit, sp4bits, sp8bits, sp16bits);

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


  THintType = (hintDontCare, hintFastest, hintNicest);

  TDGLTextureMode = (tmDecal, tmModulate, tmBlend, tmReplace, tmAdd);
  TDGLTextureWrap = (twBoth, twNone, twVertical, twHorizontal, twSeparate);

  TDGLMinFilter =
  (
    miNearest,
    miLinear,
    miNearestMipmapNearest,
    miLinearMipmapNearest,
    miNearestMipmapLinear,
    miLinearMipmapLinear
  );

  // TDGLInternalCompression
  //
  {: Texture compression option.<p>
     If OpenGL supports it, this will activate a compressed texture format:<ul>
     <li>tcDefault : uses global default compression option
     <li>tcNone : do not use compression
     <li>tcStandard : use standard compression, average quality, average rate
     <li>tcHighQuality : choose a high-quality, low-speed compression
     <li>tcHighSpeed : choose a high-speed, low-quality compression
     </ul>. }
  TDGLInternalCompression = (tcDefault, tcNone, tcStandard, tcHighQuality,
    tcHighSpeed);

  TDGLMagFilter = (maNearest, maLinear);

  // Specifies how depth values should be treated
  // during filtering and texture application
  TDGLDepthTextureMode = (dtmLuminance, dtmIntensity, dtmAlpha);

  // Specifies the depth comparison function.
  TDGLDepthCompareFunc = TDepthFunction;


  {: Texture format for OpenGL (rendering) use.<p>
  Internally, GLScene handles all "base" images as 32 Bits RGBA, but you can
  specify a generic format to reduce OpenGL texture memory use:<ul>}
  TDGLTextureFormat = (
    tfDefault,
    tfRGB, // = tfRGB8
    tfRGBA, // = tfRGBA8
    tfRGB16, // = tfRGB5
    tfRGBA16, // = tfRGBA4
    tfAlpha, // = tfALPHA8
    tfLuminance, // = tfLUMINANCE8
    tfLuminanceAlpha, // = tfLUMINANCE8_ALPHA8
    tfIntensity, // = tfINTENSITY8
    tfNormalMap, // = tfRGB8
    tfRGBAFloat16, // = tfRGBA_FLOAT16_ATI
    tfRGBAFloat32, // = tfRGBA_FLOAT32_ATI
    tfExtended);

  {: Defines how and if Alpha channel is defined for a texture image.<ul>
   <li>tiaDefault : uses the alpha channel in the image if any
   <li>tiaAlphaFromIntensity : the alpha channel value is deduced from other
    RGB components intensity (the brighter, the more opaque)
   <li>tiaSuperBlackTransparent : pixels with a RGB color of (0, 0, 0) are
    completely transparent, others are completely opaque
   <li>tiaLuminance : the luminance value is calculated for each pixel
    and used for RGB and Alpha values
   <li>tiaLuminanceSqrt : same as tiaLuminance but with an Sqrt(Luminance)
       <li>tiaOpaque : alpha channel is uniformously set to 1.0
       <li>tiaTopLeftPointColorTransparent : points of the same color as the
          top left point of the bitmap are transparent, others are opaque.
       </ul>
    }
  TDGLTextureImageAlpha =
  (
    tiaDefault,
    tiaAlphaFromIntensity,
    tiaSuperBlackTransparent,
    tiaLuminance,
    tiaLuminanceSqrt,
    tiaOpaque,
    tiaTopLeftPointColorTransparent,
    tiaInverseLuminance,
    tiaInverseLuminanceSqrt,
    tiaBottomRightPointColorTransparent
  );

  TDGLDiffuseLightMode =(dlmLambert,dlmHalfLambert, dlmPhong, dlmMinnAert, dlmOrenNayar);
  TDGLSpecularLightMode=(slmNone,slmDefault,slmPhong,slmBlinn,slmIsotropicWard,slmAnisotropic);


  TDGLAntiAliasing = (
    // Multisample Antialiasing
    aaDefault, aaNone, aa2x, aa2xHQ, aa4x, aa4xHQ,
    aa6x, aa8x, aa16x,
    // Coverage Sampling Antialiasing
    csa8x, csa8xHQ, csa16x, csa16xHQ);

  TVSyncMode = (vsmSync, vsmNoSync);

  TDrawState = (dsRendering, dsPicking, dsPrinting);

  TDGLSize = record
    cx: Longint;
    cy: Longint;
  end;

  // TDGLObjectsSorting
  //
  {: Determines if objects are sorted, and how.<p>
     Sorting is done level by level (and not for all entities), values are :<ul>
     <li>osInherited : use inherited sorting mode, defaults to osRenderFarthestFirst
     <li>osNone : do not sort objects.
 <li>osRenderFarthestFirst : render objects whose Position is the farthest from
  the camera first.
     <li>osRenderBlendedLast : opaque objects are not sorted and rendered
        first, blended ones are rendered afterwards and depth sorted.
 <li>osRenderNearestFirst : render objects whose Position is the nearest to
  the camera first.
      </ul> }
  TDGLObjectsSorting = (osInherited, osNone,osRenderFarthestFirst, osRenderBlendedLast,osRenderNearestFirst);

  // TDGLVisibilityCulling
  //
  {: Determines the visibility culling mode.
     Culling is done level by level, allowed values are:<ul>
     <li>vcInherited : use inherited culling value, if selected for the root
        level, defaults to vcNone
     <li>vcNone : no visibility culling is performed
     <li>vcObjectBased : culling is done on a per-object basis, each object may
        or may not be culled base on its own AxisAlignedDimensions,
        culling has no impact on the visibility of its children
     <li>vcHierarchical : culling is performed hierarchically, using hierarchical
        bounding boxes, if a parent is culled, all of its children, whatever their
        culling options are invisible.
     <li><br>Depending on the structure of your scene the most efficient culling
     method will be either vcObjectBased or vcHierarchical. Also note that if
     you use many objects with "static" geometry and have a T&amp;L graphics
     board, it may be faster not to cull at all (ie. leave this to the hardware). }
  TDGLVisibilityCulling = (vcInherited, vcNone, vcObjectBased, vcHierarchical);

  TMapTexCoordMode = (mtcmUndefined, mtcmNull, mtcmMain, mtcmDual, mtcmSecond,mtcmArbitrary);

  // TDGLCoordinatesStyle
  //
  { : Identify the data type stocked in a TDGLCustomCoordinates.<p>
    <ul><li>csPoint2D : 2D point (Z=0, W=0)
    <ul><li>csPoint : point (W=1)
    <li>csVector : a vector (W=0)
    <li>csUnknown : null
    </ul> }
  TDGLCoordinatesStyle = (CsPoint2D, CsPoint, CsVector, CsUnknown);

  // TDGLShaderStyle
  //
  {: Define GLShader style application relatively to a material.<ul>
     <li>ssHighLevel: shader is applied before material application, and unapplied
           after material unapplication
     <li>ssLowLevel: shader is applied after material application, and unapplied
           before material unapplication
     <li>ssReplace: shader is applied in place of the material (and material
           is completely ignored)
     </ul> }
  TDGLShaderStyle = (ssHighLevel, ssLowLevel, ssReplace);

  // TDGLShaderFailedInitAction
  //
  {: Defines what to do if for some reason shader failed to initialize.<ul>
     <li>fiaSilentdisable:          just disable it
     <li>fiaRaiseHandledException:  raise an exception, and handle it right away
                                    (usefull, when debigging within Delphi)
     <li>fiaRaiseStardardException: raises the exception with a string from this
                                      function GetStardardNotSupportedMessage
     <li>fiaReRaiseException:       Re-raises the exception
     <li>fiaGenerateEvent:          Handles the exception, but generates an event
                                    that user can respond to. For example, he can
                                    try to compile a substitude shader, or replace
                                    it by a material.
                                    Note: HandleFailedInitialization does *not*
                                    create this event, it is left to user shaders
                                    which may chose to override this procedure.
                                    Commented out, because not sure if this
                                    option should exist, let other generations of
                                    developers decide ;)
     </ul> }
  TDGLShaderFailedInitAction = (
    fiaSilentDisable, fiaRaiseStandardException,
    fiaRaiseHandledException, fiaReRaiseException
    {,fiaGenerateEvent});

  // TDGLCameraInvarianceMode
  //
  TDGLCameraInvarianceMode = (cimNone, cimPosition, cimOrientation);

  // TDGLCameraStyle
  //

  TDGLCameraStyle = (csPerspective, csOrthogonal, csOrtho2D, csCustom, csInfinitePerspective, csPerspectiveKeepFOV);

  TDGLCameraKeepFOVMode = (ckmHorizontalFOV, ckmVerticalFOV);


  TDGLSceneViewerMode = (svmDisabled, svmDefault, svmNavigation, svmGizmo);

  // flags for design notification
  TSceneOperation = (soAdd, soRemove, soMove, soRename, soSelect, soBeginUpdate, soEndUpdate);

  // IDs for limit determination
  TLimitType = (limClipPlanes, limEvalOrder, limLights, limListNesting, limModelViewStack, limNameStack, limPixelMapTable, limProjectionStack, limTextureSize, limTextureStack, limViewportDims, limAccumAlphaBits, limAccumBlueBits, limAccumGreenBits,
    limAccumRedBits, limAlphaBits, limAuxBuffers, limBlueBits, limGreenBits, limRedBits, limIndexBits, limStereo, limDoubleBuffer, limSubpixelBits, limDepthBits, limStencilBits, limNbTextureUnits);


  TNormalDirection = (ndInside, ndOutside);

  // TDGLObjectStyle
  //
  { : Possible styles/options for a GLScene object.<p>
    Allowed styles are:<ul>
    <li>osDirectDraw : object shall not make use of compiled call lists, but issue
    direct calls each time a render should be performed.
    <li>osIgnoreDepthBuffer : object is rendered with depth test disabled,
    this is true for its children too.
    <li>osNoVisibilityCulling : whatever the VisibilityCulling setting,
    it will be ignored and the object rendered
    </ul> }
  TDGLObjectStyle  = (osDirectDraw, osIgnoreDepthBuffer, osNoVisibilityCulling, osBuiltStage);
  TDGLObjectStyles = set of TDGLObjectStyle;


  // TLightStyle
  //
  { : Defines the various styles for lightsources.<p>
    <ul>
    <li>lsSpot : a spot light, oriented and with a cutoff zone (note that if
    cutoff is 180, the spot is rendered as an omni source)
    <li>lsOmni : an omnidirectionnal source, punctual and sending light in
    all directions uniformously
    <li>lsParallel : a parallel light, oriented as the light source is (this
    type of light can help speed up rendering)
    </ul> }
  TLightStyle = (lsSpot, lsOmni, lsParallel, lsParallelSpot);

  // TFogMode
  //
  TFogMode = (fmLinear, fmExp, fmExp2);

  // TFogDistance
  //
  { : Fog distance calculation mode.<p>
    <ul>
    <li>fdDefault: let OpenGL use its default formula
    <li>fdEyeRadial: uses radial "true" distance (best quality)
    <li>fdEyePlane: uses the distance to the projection plane
    (same as Z-Buffer, faster)
    </ul>Requires support of GL_NV_fog_distance extension, otherwise,
    it is ignored. }
  TFogDistance = (fdDefault, fdEyeRadial, fdEyePlane);

  // TDGLDepthPrecision
  //
  TDGLDepthPrecision = (dpDefault, dp16bits, dp24bits, dp32bits);

  // TDGLColorDepth
  //
  TDGLColorDepth = (cdDefault, cd8bits, cd16bits, cd24bits, cdFloat64bits, cdFloat128bits); // float_type

  // TDGLShadeModel
  //
  TDGLShadeModel = (smDefault, smSmooth, smFlat);

  TDGLShaderType = (shtVertex, shtControl, shtEvaluation, shtGeometry, shtFragment);
  TDGLSLDataType = (
    GLSLTypeUndefined,
    GLSLType1F,
    GLSLType2F,
    GLSLType3F,
    GLSLType4F,
    GLSLType1I,
    GLSLType2I,
    GLSLType3I,
    GLSLType4I,
    GLSLType1UI,
    GLSLType2UI,
    GLSLType3UI,
    GLSLType4UI,
    GLSLType4UB, // ??????
    GLSLTypeMat2F,
    GLSLTypeMat3F,
    GLSLTypeMat4F,
    GLSLTypeVoid);

  TDGLSLSamplerType = (
    GLSLSamplerUndefined,
    GLSLSampler1D,
    GLSLSampler2D,
    GLSLSampler3D,
    GLSLSamplerCube,
    GLSLSampler1DShadow,
    GLSLSampler2DShadow,
    GLSLSampler1DArray,
    GLSLSampler2DArray,
    GLSLSampler1DArrayShadow,
    GLSLSampler2DArrayShadow,
    GLSLSamplerCubeShadow,
    GLSLIntSampler1D,
    GLSLIntSampler2D,
    GLSLIntSampler3D,
    GLSLIntSamplerCube,
    GLSLIntSampler1DArray,
    GLSLIntSampler2DArray,
    GLSLUIntSampler1D,
    GLSLUIntSampler2D,
    GLSLUIntSampler3D,
    GLSLUIntSamplerCube,
    GLSLUIntSampler1DArray,
    GLSLUIntSampler2DArray,
    GLSLSamplerRect,
    GLSLSamplerRectShadow,
    GLSLSamplerBuffer,
    GLSLIntSamplerRect,
    GLSLIntSamplerBuffer,
    GLSLUIntSamplerRect,
    GLSLUIntSamplerBuffer,
    GLSLSamplerMS,
    GLSLIntSamplerMS,
    GLSLUIntSamplerMS,
    GLSLSamplerMSArray,
    GLSLIntSamplerMSArray,
    GLSLUIntSamplerMSArray
    );

  TDGLgsInTypes = (
    gsInPoints,
    gsInLines,
    gsInAdjLines,
    gsInTriangles,
    gsInAdjTriangles
  );

  TDGLgsOutTypes = (
    gsOutPoints,
    gsOutLineStrip,
    sOutTriangleStrip
  );

  TDGLSLAttribute = record
  private
    FID: Integer;
    FName: string;
    Location: GLInt;
  public
    Tag: Integer;
    DataType: TDGLSLDataType;
    WarningAbsenceLoged: Boolean;
    property ID: Integer read FID;
    property Name: string read FName;
  end;

  TDGLSLUniform = record
  private
    Location: GLInt;
    FID: Integer;
    FName: string;
  public
    Tag: Integer;
    DataType: TDGLSLDataType;
    WarningAbsenceLoged: Boolean;
    property ID: Integer read FID;
    property Name: string read FName;
  end;
  PGLSLUniform = ^TDGLSLUniform;
  PGLSLAttribute = ^TDGLSLAttribute;
  TDGLSLAttributeArray = array[0..GLS_VERTEX_ATTR_NUM - 1] of TDGLSLAttribute;

  // Texture addressing rules
  TDGLSeparateTextureWrap = (twRepeat, twClampToEdge, twClampToBorder, twMirrorRepeat, twMirrorClampToEdge, twMirrorClampToBorder);

  // Specifies the texture comparison mode for currently bound depth textures.
  // That is, a texture whose internal format is tfDEPTH_COMPONENT*
  TDGLTextureCompareMode = (tcmNone, tcmCompareRtoTexture);

  // Filtering quality
  TDGLTextureFilteringQuality = (tfIsotropic, tfAnisotropic);

  // TDGLTextureTarget
  //
  TDGLTextureTarget =
  (
    ttNoShape, ttTexture1D, ttTexture2D, ttTexture3D, ttTexture1DArray,
    ttTexture2DArray, ttTextureRect, ttTextureBuffer, ttTextureCube,
    ttTexture2DMultisample, ttTexture2DMultisampleArray, ttTextureCubeArray
  );

  // TGLTextureMappingMode DEPRECATED
  //
  TDGLTextureMappingMode = (tmmUser, tmmObjectLinear, tmmEyeLinear, tmmSphere,
    tmmCubeMapReflection, tmmCubeMapNormal,
    tmmCubeMapLight0, tmmCubeMapCamera);

  TMipmapGenerationMode = (mgmNoMip, mgmLeaveExisting, mgmOnFly, mgmBoxFilter, mgmTriangleFilter, mgmHermiteFilter, mgmBellFilter, mgmSplineFilter, mgmLanczos3Filter, mgmMitchellFilter);

  //
  // DaStr: if you write smth like af_GL_NEVER = GL_NEVER in the definition,
  // it won't show up in the Dephi 7 design-time editor. So I had to add
  // vTGlAlphaFuncValues and vTGLBlendFuncFactorValues arrays.
  //
  TDGlAlphaFunc = TComparisonFunction;

  // TMaterialOptions
  //
  { : Control special rendering options for a material.<p>
    moIgnoreFog : fog is deactivated when the material is rendered }
  TMaterialOption  = (moIgnoreFog, moNoLighting);

  TDGLTextureSwizzle = (tswRed, tswGreen, tswBlue, tswAlpha, tswZero, tswOne);
  type
    TSwizzleVector = record
        case Integer of
      0 : (V: array[0..3] of TDGLTextureSwizzle);
      1 : (R, G, B, A : TDGLTextureSwizzle);
  end;

  // TBlendingMode
  //
  { : Simplified blending options.<p>
    bmOpaque : disable blending<br>
    bmTransparency : uses standard alpha blending<br>
    bmAdditive : activates additive blending (with saturation)<br>
    bmAlphaTest50 : uses opaque blending, with alpha-testing at 50% (full
    transparency if alpha is below 0.5, full opacity otherwise)<br>
    bmAlphaTest100 : uses opaque blending, with alpha-testing at 100%<br>
    bmModulate : uses modulation blending<br>
    bmCustom : uses TGLBlendingParameters options
  }
  TBlendingMode = (bmOpaque, bmTransparency, bmAdditive, bmAlphaTest50, bmAlphaTest100, bmModulate, bmCustom);
  { : Adds two more blending modes to standard ones.
    Not sure how to name them or if they should be included in TBlending mode,
    so I created a new type here. }
  TDGLBlendingModeEx = (bmxOpaque, bmxTransparency, bmxAdditive, bmxAlphaTest50, bmxAlphaTest100, bmxModulate, bmxDestColorOne, bmxDestAlphaOne);

  // TFaceCulling
  //
  TFaceCulling = (fcBufferDefault, fcCull, fcNoCull);

  TGLVBOMState = (GLVBOM_DEFAULT, GLVBOM_OBJECT, GLVBOM_PRIMITIVE);

  TGLVBOMEnum = (GLVBOM_NOPRIMITIVE, GLVBOM_TRIANGLES, GLVBOM_TRIANGLE_STRIP, GLVBOM_TRIANGLE_FAN,
    // GLVBOM_QUADS, // OpenGL3x deprecated !!
    // GLVBOM_QUAD_STRIP, // OpenGL3x deprecated !!
    GLVBOM_POINTS, GLVBOM_LINES, GLVBOM_LINE_LOOP, GLVBOM_LINE_STRIP,
    // GLVBOM_POLYGON, // OpenGL3x deprecated !!
    GLVBOM_LINES_ADJACENCY, GLVBOM_LINE_STRIP_ADJACENCY, GLVBOM_TRIANGLES_ADJACENCY, GLVBOM_TRIANGLE_STRIP_ADJACENCY,

    GLVBOM_NODATA, GLVBOM_CUSTOM_DATA, GLVBOM_1F, GLVBOM_2F, GLVBOM_3F, GLVBOM_4F, GLVBOM_1I, GLVBOM_2I, GLVBOM_3I, GLVBOM_4I, GLVBOM_4UB);

  TGLBufferUsage = (buStatic, buDynamic, buStream);

  TDGLFramebufferStatus = (fsComplete, fsIncompleteAttachment, fsIncompleteMissingAttachment, fsIncompleteDuplicateAttachment, fsIncompleteDimensions, fsIncompleteFormats, fsIncompleteDrawBuffer, fsIncompleteReadBuffer, fsUnsupported,
    fsIncompleteMultisample, fsStatusError);



const
  cAllAttribBits = [low(TDGLStateType)..High(TDGLStateType)];

  cAllColorComponents = [ccRed, ccGreen, ccBlue, ccAlpha];


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


  cTextureMagFilter: array [maNearest .. maLinear] of TGLEnum             = (GL_NEAREST, GL_LINEAR);
  cTextureMinFilter: array [miNearest .. miLinearMipMapLinear] of TGLEnum = (GL_NEAREST, GL_LINEAR, GL_NEAREST_MIPMAP_NEAREST, GL_LINEAR_MIPMAP_NEAREST, GL_NEAREST_MIPMAP_LINEAR, GL_LINEAR_MIPMAP_LINEAR);
  cTextureWrapMode: array [twRepeat .. twMirrorClampToBorder] of TGLEnum  = (GL_REPEAT, GL_CLAMP_TO_EDGE, GL_CLAMP_TO_BORDER, GL_MIRRORED_REPEAT, GL_MIRROR_CLAMP_TO_EDGE_ATI, GL_MIRROR_CLAMP_TO_BORDER_EXT);
  cTextureCompareMode: array [tcmNone .. tcmCompareRtoTexture] of TGLEnum = (GL_NONE, GL_COMPARE_R_TO_TEXTURE_ARB);
  cSamplerToTexture: array [TDGLSLSamplerType] of TDGLTextureTarget = (ttNoShape, ttTexture1D, ttTexture2D, ttTexture3D, ttTextureCube, ttTexture1D, ttTexture2D, ttTexture1DArray, ttTexture2DArray, ttTexture1DArray, ttTexture1DArray, ttTextureCube,
    ttTexture1D, ttTexture2D, ttTexture3D, ttTextureCube, ttTexture1DArray, ttTexture2DArray, ttTexture1D, ttTexture2D, ttTexture3D, ttTextureCube, ttTexture1DArray, ttTexture2DArray, ttTextureRect, ttTextureRect, ttTextureBuffer, ttTextureRect,
    ttTextureBuffer, ttTextureRect, ttTextureBuffer, ttTexture2DMultisample, ttTexture2DMultisample, ttTexture2DMultisample, ttTexture2DMultisampleArray, ttTexture2DMultisampleArray, ttTexture2DMultisample);

  cTextureSwizzle: array [TDGLTextureSwizzle] of TGLEnum = (GL_RED, GL_GREEN, GL_BLUE, GL_ALPHA, GL_ZERO, GL_ONE);


//  cTextureMode: array [TDGLTextureMode] of TGLEnum = (GL_DECAL, GL_MODULATE, GL_BLEND, GL_REPLACE, GL_ADD);


  cShaderTypeName: array [TDGLShaderType] of string = ('vertex', 'control', 'evaluation', 'geomtery', 'fragment');

  cDefaultNormalMapScale = 0.125;

  CmtPX = 0;
  CmtNX = 1;
  CmtPY = 2;
  CmtNY = 3;
  CmtPZ = 4;
  CmtNZ = 5;

const
  cGLSLTypeString: array[TDGLSLDataType] of AnsiString = (
    'undefined',
    'float',
    'vec2',
    'vec3',
    'vec4',
    'int',
    'ivec2',
    'ivec3',
    'ivec4',
    'uint',
    'uivec2',
    'uivec3',
    'uivec4',
    'bool',
    'mat2',
    'mat3',
    'mat4',
    'void');

  cGLSLSamplerString: array[TDGLSLSamplerType] of AnsiString = (
    'undefined',
    'sampler1D',
    'sampler2D',
    'sampler3D',
    'samplerCube',
    'sampler1DShadow',
    'sampler2DShadow',
    'sampler1DArray',
    'sampler2DArray',
    'sampler1DArrayShadow',
    'sampler2DArrayShadow',
    'samplerCubeShadow',
    'isampler1D',
    'isampler2D',
    'isampler3D',
    'isamplerCube',
    'isampler1DArray',
    'isampler2DArray',
    'usampler1D',
    'usampler2D',
    'usampler3D',
    'usamplerCube',
    'usampler1DArray',
    'usampler2DArray',
    'samplerRect',
    'samplerRectShadow',
    'samplerBuffer',
    'isamplerRect',
    'isamplerBuffer',
    'usamplerRect',
    'usamplerBuffer',
    'samplerMS',
    'isamplerMS',
    'usamplerMS',
    'samplerMSArray',
    'isamplerMSArray',
    'usamplerMSArray');

const
  cGLgsInTypes : array[TDGLgsInTypes] of GLenum =
    (GL_POINTS, GL_LINES, GL_LINES_ADJACENCY_EXT, GL_TRIANGLES,
     GL_TRIANGLES_ADJACENCY_EXT);
  cGLgsOutTypes: array[TDGLgsOutTypes] of GLenum =
    (GL_POINTS, GL_LINE_STRIP, GL_TRIANGLE_STRIP);


const
  cDefaultPointSize: Single = 1.0;

// Buffer ID's for Multiple-Render-Targets (using GL_ATI_draw_buffers)
const
  MRT_BUFFERS:GLEnum = GL_FRONT_LEFT;


Var
  cDefaultSwizzleVector: TSwizzleVector;
  vDefaultViewMatrix:          TMatrix;
  vDefaultProjectionMatrix:    TMatrix;
  vDefaultLightSourcePosition: TVector;

implementation

initialization
  cDefaultSwizzleVector.r:=tswRed;
  cDefaultSwizzleVector.g:=tswGreen;
  cDefaultSwizzleVector.b:=tswBlue;
  cDefaultSwizzleVector.a:=tswAlpha;


  vDefaultViewMatrix.V[0].X:= 1;
  vDefaultViewMatrix.V[0].Y:= 0;
  vDefaultViewMatrix.V[0].Z:= 0;
  vDefaultViewMatrix.V[0].W:= 0;
  vDefaultViewMatrix.V[1].X:= 0;
  vDefaultViewMatrix.V[1].Y:= 1;
  vDefaultViewMatrix.V[1].Z:= 0;
  vDefaultViewMatrix.V[1].W:= 0;
  vDefaultViewMatrix.V[2].X:= 0;
  vDefaultViewMatrix.V[2].Y:= 0;
  vDefaultViewMatrix.V[2].Z:= 1;
  vDefaultViewMatrix.V[2].W:= 0;
  vDefaultViewMatrix.V[3].X:= 0;
  vDefaultViewMatrix.V[3].Y:= 0;
  vDefaultViewMatrix.V[3].Z:= 0;
  vDefaultViewMatrix.V[3].W:= 1;

  vDefaultProjectionMatrix := vDefaultViewMatrix;

  vDefaultLightSourcePosition.X:=10;
  vDefaultLightSourcePosition.X:=10;
  vDefaultLightSourcePosition.X:=10;
  vDefaultLightSourcePosition.X:=1;


end.
