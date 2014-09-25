//
// This unit is part of the GLScene Project, http://glscene.org
//
{ : GLTexture<p>

  Handles all the color and texture stuff.<p>

  <b>History : </b><font size=-1><ul>
  <li>14/03/07 - DaStr - TGLPicFileImage now provides correct Width and Height
                 (BugtrackerID = 1680742)
  <li>09/03/07 - DaStr - Added TGLMaterial.GetActualPrimaryMaterial, GetLibMaterial
                 Bugfixed TGLColor.Initialize and TGLColor.Destroy
                 (thanks Burkhard Carstens) (BugtrackerID = 1678650)
  <li>04/03/07 - DaStr - Added TGLTextureTarget, [Encode/Decode]GLTextureTarget
  <li>23/02/07 - DaStr - Added TGLShaderClass, TGLShaderFailedInitAction,
                 EGLShaderException
                 Added TGLShader.HandleFailedInitialization, ShaderSupported,
                 GetStardardNotSupportedMessage, FailedInitAction
                 Added default value for TGLShader.ShaderStyle
                 Fixed TGLShader.InitializeShader
                 Fixed TGLTextureExItem.Create (TGLCoordinatesStyle stuff)
  <li>16/02/07 - DaStr - Global $Q- removed
                 Added TGLLibMaterials.GetTextureIndex, GetMaterialIndex,
                 GetNameOfTexture, GetNameOfLibMaterial
                 Added TGLMaterialLibrary.TextureByName,
                 GetNameOfTexture, GetNameOfLibMaterial
  <li>01/02/07 - LIN - Added TGLLibMaterial.IsUsed : true if texture has registered users
  <li>23/01/07 - LIN - Added TGLTextureImage.AssignToBitmap : Converts the TextureImage to a TBitmap
  <li>23/01/07 - LIN - Added TGLTextureImage.AsBitmap : Returns the TextureImage as a TBitmap
  <li>22/01/07 - DaStr - IGLMaterialLibrarySupported abstracted
                 TGLLibMaterial.TextureOffset/TextureScale.FStyle bugfxed (thanks Ian Mac)
  <li>20/12/06 - DaStr - TGLColorManager.Enumcolors overloaded
                 TGLShader.Apply bugfixed, TGLShader.Assign added
  <li>19/10/06 - LC - Fixed TGLLibMaterial.UnApply so it doesn't unapply a 2nd
                 texture that was never applied. Bugtracker ID=1234085
  <li>19/10/06 - LC - Fixed TGLLibMaterial.Assign. Bugtracker ID=1549843 (thanks Zapology)
  <li>15/09/06 - NC - TGLShader.handle as Integer -> Cardinal
  <li>12/09/06 - NC - Added GetFloatTexImage and SetFloatTexImage
  <li>06/03/05 - EG - FTextureEx now autocreated (like FTexture)
  <li>30/11/04 - EG - No longer stores TextureEx if empty
  <li>06/10/04 - NC - Corrected filtering param. setting for float texture,
                 Now keep using GL_TEXTURE_RECTANGLE_NV for TGLFloatDataImage
  <li>05/10/04 - SG - Added Material.TextureEx (texture extension)
  <li>04/10/04 - NC - Added TGLFloatDataImage
  <li>02/08/04 - LR, YHC - BCB corrections: use record instead array
                 Replace direct access of some properties by a getter and a setter
                 Add dummy method for the abstract problem
                 Changed type of TGLCubeMapTarget to integer
  <li>03/07/04 - LR - Move InitWinColors to GLCrossPlatform
                 Replace TGraphics, TBitmap by TGLGraphics, TGLBitmap
  <li>29/06/04 - SG - Added bmModulate blending mode
  <li>08/04/04 - EG - Added AddMaterialsFromXxxx logic
  <li>04/09/03 - EG - Added TGLShader.Enabled
  <li>02/09/03 - EG - Added TGLColor.HSVA
  <li>28/07/03 - aidave - Added TGLColor.RandomColor
  <li>24/07/03 - EG - Introduced TGLTextureImageEditor mechanism
  <li>04/07/03 - EG - Material.Texture now autocreating,
                 added per-texture brightness and gamma correction
  <li>13/06/03 - EG - cubemap images can now be saved/restored as a whole
  <li>05/06/03 - EG - Assign fixes (Andrzej Kaluza)
  <li>23/05/03 - EG - More generic libmaterial registration
  <li>08/12/02 - EG - Added tiaInverseLuminance
  <li>13/11/02 - EG - Added tmmCubeMapLight0
  <li>18/10/02 - EG - CubeMap texture matrix now setup for 2nd texture unit too
  <li>24/07/02 - EG - Added TGLLibMaterials.DeleteUnusedMaterials
  <li>13/07/02 - EG - Improved materials when lighting is off
  <li>10/07/02 - EG - Added basic protection against cyclic material refs
  <li>08/07/02 - EG - Multipass support
  <li>18/06/02 - EG - Added TGLShader
  <li>26/01/02 - EG - Makes use of new xglBegin/EndUpdate mechanism
  <li>24/01/02 - EG - Added vUseDefaultSets mechanism,
                      TGLPictureImage no longer systematically creates a TPicture
  <li>21/01/02 - EG - Fixed OnTextureNeeded calls (Leonel)
  <li>20/01/02 - EG - Fixed texture memory use report error
  <li>10/01/02 - EG - Added Material.FaceCulling, default texture filters
                      are now Linear/MipMap
  <li>07/01/02 - EG - Added renderDPI to rci
  <li>16/12/01 - EG - Added support for cube maps (texture and mappings)
  <li>30/11/01 - EG - Texture-compression related errors now ignored (unsupported formats)
  <li>14/09/01 - EG - Use of vFileStreamClass
  <li>06/09/01 - EG - No longers depends on 'Windows'
  <li>04/09/01 - EG - Texture binding cache
  <li>31/08/01 - EG - tiaDefault wasn't honoured (Rene Lindsay)
  <li>25/08/01 - EG - Added TGLBlankImage
  <li>16/08/01 - EG - drawState now part of TRenderContextInfo
  <li>15/08/01 - EG - TexGen support (object_linear, eye_linear and sphere_map)
  <li>13/08/01 - EG - Fixed OnTextureNeeded handling (paths for mat lib)
  <li>12/08/01 - EG - Completely rewritten handles management
  <li>27/07/01 - EG - TGLLibMaterials now a TOwnedCollection
  <li>19/07/01 - EG - Added "Enabled" to TGLTexture
  <li>28/06/01 - EG - Added AddTextureMaterial TGraphic variant
  <li>14/03/01 - EG - Streaming fixes by Uwe Raabe
  <li>08/03/01 - EG - TGLPicFileImage.GetBitmap32 now resets filename if not found
  <li>01/03/01 - EG - Fixed TGLMaterial.DestroyHandle,
                      Added Texture2 notifications and material cacheing
  <li>26/02/01 - EG - Added support for GL_EXT_texture_filter_anisotropic
  <li>23/02/01 - EG - Fixed texture matrix messup (second was using first)
  <li>21/02/01 - EG - Minor fix for TextureImageRequiredMemory,
                      TexGen calls now based on XOpenGL
  <li>14/02/01 - EG - Added support for texture format & texture compression
  <li>31/01/01 - EG - Added Multitexture support
  <li>28/01/01 - EG - Added MaterialOptions
  <li>15/01/01 - EG - Enhanced TGLPicFileImage.LoadFromFile
  <li>13/01/01 - EG - New helper functions for TGLMaterialLibrary
  <li>08/01/01 - EG - Not-so-clean fix for TGLTexture.Destroy... better fix
                      will require awareness of rendering contexts...
  <li>06/12/00 - EG - Added PrepareBuildList mechanism
  <li>16/10/00 - EG - Fix in TGLPictureImage.Assign
  <li>25/09/00 - EG - New texture management implemented
  <li>13/08/00 - EG - Added AddTextureMaterial
  <li>06/08/00 - EG - File not found error now happens only once per texture,
                      also added some more doc and texture transforms support
                      to TGLLibMaterial
  <li>27/07/00 - EG - TGLPictureImage.Assign now accepts TGraphic & TPicture,
                      Added max texture size clamping
  <li>15/07/00 - EG - Upgrade for new list/handle destruction scheme
  <li>05/07/00 - EG - Added tiaTopLeftPointColorTransparent
  <li>28/06/00 - EG - Added asserts for missing texture files
  <li>01/06/00 - EG - Added ReloadTexture (support for texture library),
                      Fixed persistence of material names in a texture library
  <li>28/05/00 - EG - TGLColor now has NotifyChange support for TGLBaseSceneObject
  <li>23/04/00 - EG - Fixed bugs with TGLPicFileImage & TGLPersistentImage,
                      Added tiaOpaque
  <li>17/04/00 - EG - Added Assign to DummyCube and Sprite
  <li>16/04/00 - EG - Added TGLPicFileImage.Assign
  <li>26/03/00 - EG - Finally fixed nasty bug in TGLMaterial.Free
  <li>22/03/00 - EG - Added BeginUpdate/EndUpdate to TGLPictureImage,
                      Made use of [Un]SetGLState in TGLMaterial
                     (gain = 7-10% on T&L intensive rendering),
                      TGLTexBaseClass is no more (RIP)
  <li>21/03/00 - EG - TGLMaterial props are now longer stored when it is
                      linked to a material library entry,
                      Added TGLPictureImage (split from TGLPersistentImage),
                      TGLPicFileImage has been updated and reactivated,
                      ColorManager is now autocreated and non longer force-linked.
  <li>19/03/00 - EG - Added SaveToXxxx & LoadFromXxxx to TGLMaterialLibrary
  <li>18/03/00 - EG - Added GetGLTextureImageClassesAsStrings,
                      Added FindGLTextureImageClassByFriendlyName,
                      FChanges states now ignored in TGLTexture.GetHandle,
                      Added SaveToFile/LoadFromFile to TextureImage
  <li>17/03/00 - EG - Added tiaLuminance
  <li>14/03/00 - EG - Added RegisterGLTextureImageClass stuff,
                      Added ImageAlpha
  <li>13/03/00 - EG - Changed TGLTextureImage image persistence again,
                      Added "Edit" method for texture image classes,
                      TMagFilter/TMinFilter -> TGLMagFilter/TGLMinFilter
  <li>03/03/00 - EG - Removed TImagePath,
                      Started major rework of the whole TGLTextureImage stuff,
                      Fixed and optimized TGLTexture.PrepareImage
  <li>12/02/00 - EG - Added Material Library
  <li>10/02/00 - EG - Fixed crash when texture is empty
  <li>08/02/00 - EG - Added AsWinColor & DeclareCurrentAsDefault to TGLColor,
                      fixed notification on material property setXxx methods,
                      Objects now begin with 'TGL'
  <li>07/02/00 - EG - "Update"s renamed to "NotifyChange"s
  <li>06/02/00 - EG - RoundUpToPowerOf2, RoundDownToPowerOf2 and
                      IsPowerOf2 moved to GLMisc, added TGLPersistentImage.Assign,
                      fixed TGLMaterial.Assign,
                      disable inheritance stuff in TGLFaceProperties.Apply (needs fixing),
                      Diffuse & ambient color now default to openGL values
  <li>05/02/00 - EG - Javadocisation, fixes and enhancements :<br>
                      TGLColor.Update, ConvertWinColor, TPicImage,
                      TGLMaterial.Apply
  </ul></font>
}
unit GLTexture;

interface

{$I GLScene.inc}

uses
  Classes, OpenGL1x, VectorGeometry, SysUtils, GLMisc, GLGraphics, GLContext,
  GLCrossPlatform, PersistentClasses, GLUtils, GLState;

type
  PColorVector = ^TColorVector;
  TColorVector = TVector;

const

  // color definitions

  // Window's colors (must be filled at program
  // startup, since they depend on the desktop scheme)

{$J+ - allow change of the following typed constants}
  ClrScrollBar: TColorVector = (X: 0; Y: 0; Z: 0; W: 1);
  ClrBackground: TColorVector = (X: 0; Y: 0; Z: 0; W: 1);
  ClrActiveCaption: TColorVector = (X: 0; Y: 0; Z: 0; W: 1);
  ClrInactiveCaption: TColorVector = (X: 0; Y: 0; Z: 0; W: 1);
  ClrMenu: TColorVector = (X: 0; Y: 0; Z: 0; W: 1);
  ClrWindow: TColorVector = (X: 0; Y: 0; Z: 0; W: 1);
  ClrWindowFrame: TColorVector = (X: 0; Y: 0; Z: 0; W: 1);
  ClrMenuText: TColorVector = (X: 0; Y: 0; Z: 0; W: 1);
  ClrWindowText: TColorVector = (X: 0; Y: 0; Z: 0; W: 1);
  ClrCaptionText: TColorVector = (X: 0; Y: 0; Z: 0; W: 1);
  ClrActiveBorder: TColorVector = (X: 0; Y: 0; Z: 0; W: 1);
  ClrInactiveBorder: TColorVector = (X: 0; Y: 0; Z: 0; W: 1);
  ClrAppWorkSpace: TColorVector = (X: 0; Y: 0; Z: 0; W: 1);
  ClrHighlight: TColorVector = (X: 0; Y: 0; Z: 0; W: 1);
  ClrHighlightText: TColorVector = (X: 0; Y: 0; Z: 0; W: 1);
  ClrBtnFace: TColorVector = (X: 0; Y: 0; Z: 0; W: 1);
  ClrBtnShadow: TColorVector = (X: 0; Y: 0; Z: 0; W: 1);
  ClrGrayText: TColorVector = (X: 0; Y: 0; Z: 0; W: 1);
  ClrBtnText: TColorVector = (X: 0; Y: 0; Z: 0; W: 1);
  ClrInactiveCaptionText: TColorVector = (X: 0; Y: 0; Z: 0; W: 1);
  ClrBtnHighlight: TColorVector = (X: 0; Y: 0; Z: 0; W: 1);
  Clr3DDkShadow: TColorVector = (X: 0; Y: 0; Z: 0; W: 1);
  Clr3DLight: TColorVector = (X: 0; Y: 0; Z: 0; W: 1);
  ClrInfoText: TColorVector = (X: 0; Y: 0; Z: 0; W: 1);
  ClrInfoBk: TColorVector = (X: 0; Y: 0; Z: 0; W: 1);

{$J- - disable change of other typed constants}
  // 'static' color definitions
  // sort of grays
  ClrTransparent: TColorVector = (X: 0; Y: 0; Z: 0; W: 0);
  ClrBlack: TColorVector = (X: 0; Y: 0; Z: 0; W: 1);
  ClrGray05: TColorVector = (X: 0.05; Y: 0.05; Z: 0.05; W: 1);
  ClrGray10: TColorVector = (X: 0.10; Y: 0.10; Z: 0.10; W: 1);
  ClrGray15: TColorVector = (X: 0.15; Y: 0.15; Z: 0.15; W: 1);
  ClrGray20: TColorVector = (X: 0.20; Y: 0.20; Z: 0.20; W: 1);
  ClrGray25: TColorVector = (X: 0.25; Y: 0.25; Z: 0.25; W: 1);
  ClrGray30: TColorVector = (X: 0.30; Y: 0.30; Z: 0.30; W: 1);
  ClrGray35: TColorVector = (X: 0.35; Y: 0.35; Z: 0.35; W: 1);
  ClrGray40: TColorVector = (X: 0.40; Y: 0.40; Z: 0.40; W: 1);
  ClrGray45: TColorVector = (X: 0.45; Y: 0.45; Z: 0.45; W: 1);
  ClrGray50: TColorVector = (X: 0.50; Y: 0.50; Z: 0.50; W: 1);
  ClrGray55: TColorVector = (X: 0.55; Y: 0.55; Z: 0.55; W: 1);
  ClrGray60: TColorVector = (X: 0.60; Y: 0.60; Z: 0.60; W: 1);
  ClrGray65: TColorVector = (X: 0.65; Y: 0.65; Z: 0.65; W: 1);
  ClrGray70: TColorVector = (X: 0.70; Y: 0.70; Z: 0.70; W: 1);
  ClrGray75: TColorVector = (X: 0.75; Y: 0.75; Z: 0.75; W: 1);
  ClrGray80: TColorVector = (X: 0.80; Y: 0.80; Z: 0.80; W: 1);
  ClrGray85: TColorVector = (X: 0.85; Y: 0.85; Z: 0.85; W: 1);
  ClrGray90: TColorVector = (X: 0.90; Y: 0.90; Z: 0.90; W: 1);
  ClrGray95: TColorVector = (X: 0.95; Y: 0.95; Z: 0.95; W: 1);
  ClrWhite: TColorVector = (X: 1; Y: 1; Z: 1; W: 1);

  // other grays
  ClrDimGray: TColorVector = (X: 0.329412; Y: 0.329412; Z: 0.329412; W: 1);
  ClrGray: TColorVector = (X: 0.752941; Y: 0.752941; Z: 0.752941; W: 1);
  ClrLightGray: TColorVector = (X: 0.658824; Y: 0.658824; Z: 0.658824; W: 1);

  // colors en masse
  ClrAquamarine: TColorVector = (X: 0.439216; Y: 0.858824; Z: 0.576471; W: 1);
  ClrBlueViolet: TColorVector = (X: 0.62352; Y: 0.372549; Z: 0.623529; W: 1);
  ClrBrown: TColorVector = (X: 0.647059; Y: 0.164706; Z: 0.164706; W: 1);
  ClrCadetBlue: TColorVector = (X: 0.372549; Y: 0.623529; Z: 0.623529; W: 1);
  ClrCoral: TColorVector = (X: 1; Y: 0.498039; Z: 0.0; W: 1); ClrCornflowerBlue: TColorVector = (X: 0.258824; Y: 0.258824; Z: 0.435294; W: 1);
  ClrDarkGreen: TColorVector = (X: 0.184314; Y: 0.309804; Z: 0.184314; W: 1);
  ClrDarkOliveGreen: TColorVector = (X: 0.309804; Y: 0.309804;  Z: 0.184314; W: 1);
  ClrDarkOrchid: TColorVector = (X: 0.6; Y: 0.196078; Z: 0.8; W: 1);
  ClrDarkSlateBlue: TColorVector = (X: 0.419608; Y: 0.137255; Z: 0.556863; W: 1);
  ClrDarkSlateGray: TColorVector = (X: 0.184314; Y: 0.309804; Z: 0.309804; W: 1);
  ClrDarkSlateGrey: TColorVector = (X: 0.184314; Y: 0.309804; Z: 0.309804; W: 1);
  ClrDarkTurquoise: TColorVector = (X: 0.439216; Y: 0.576471;  Z: 0.858824; W: 1);
  ClrFirebrick: TColorVector = (X: 0.556863; Y: 0.137255; Z: 0.137255; W: 1);
  ClrForestGreen: TColorVector = (X: 0.137255; Y: 0.556863; Z: 0.137255; W: 1);
  ClrGold: TColorVector = (X: 0.8; Y: 0.498039; Z: 0.196078; W: 1);
  ClrGoldenrod: TColorVector = (X: 0.858824; Y: 0.858824; Z: 0.439216; W: 1);
  ClrGreenYellow: TColorVector = (X: 0.576471; Y: 0.858824; Z: 0.439216; W: 1);
  ClrIndian: TColorVector = (X: 0.309804; Y: 0.184314; Z: 0.184314; W: 1);
  ClrKhaki: TColorVector = (X: 0.623529; Y: 0.623529; Z: 0.372549; W: 1);
  ClrLightBlue: TColorVector = (X: 0.74902; Y: 0.847059; Z: 0.847059; W: 1);
  ClrLightSteelBlue: TColorVector = (X: 0.560784; Y: 0.560784; Z: 0.737255; W: 1);
  ClrLimeGreen: TColorVector = (X: 0.196078; Y: 0.8; Z: 0.196078; W: 1);
  ClrMaroon: TColorVector = (X: 0.556863; Y: 0.137255; Z: 0.419608; W: 1);
  ClrMediumAquamarine: TColorVector = (X: 0.196078; Y: 0.8; Z: 0.6; W: 1);
  ClrMediumBlue: TColorVector = (X: 0.196078; Y: 0.196078; Z: 0.8; W: 1);
  ClrMediumForestGreen: TColorVector = (X: 0.419608; Y: 0.556863; Z: 0.137255; W: 1);
  ClrMediumGoldenrod: TColorVector = (X: 0.917647; Y: 0.917647; Z: 0.678431; W: 1);
  ClrMediumOrchid: TColorVector = (X: 0.576471; Y: 0.439216; Z: 0.858824; W: 1);
  ClrMediumSeaGreen: TColorVector = (X: 0.258824; Y: 0.435294; Z: 0.258824; W: 1);
  ClrMediumSlateBlue: TColorVector = (X: 0.498039; Y: 0; Z: 1; W: 1);
  ClrMediumSpringGreen: TColorVector = (X: 0.498039; Y: 1; Z: 0; W: 1);
  ClrMediumTurquoise: TColorVector = (X: 0.439216; Y: 0.858824;
    Z: 0.858824; W: 1);
  ClrMediumViolet: TColorVector = (X: 0.858824; Y: 0.439216; Z: 0.576471; W: 1);
  ClrMidnightBlue: TColorVector = (X: 0.184314; Y: 0.184314; Z: 0.309804; W: 1);
  ClrNavy: TColorVector = (X: 0.137255; Y: 0.137255; Z: 0.556863; W: 1);
  ClrNavyBlue: TColorVector = (X: 0.137255; Y: 0.137255; Z: 0.556863; W: 1);
  ClrOrange: TColorVector = (X: 1; Y: 0.5; Z: 0.0; W: 1);
  ClrOrangeRed: TColorVector = (X: 1; Y: 0.25; Z: 0; W: 1);
  ClrOrchid: TColorVector = (X: 0.858824; Y: 0.439216; Z: 0.858824; W: 1);
  ClrPaleGreen: TColorVector = (X: 0.560784; Y: 0.737255; Z: 0.560784; W: 1);
  ClrPink: TColorVector = (X: 0.737255; Y: 0.560784; Z: 0.560784; W: 1);
  ClrPlum: TColorVector = (X: 0.917647; Y: 0.678431; Z: 0.917647; W: 1);
  ClrSalmon: TColorVector = (X: 0.435294; Y: 0.258824; Z: 0.258824; W: 1);
  ClrSeaGreen: TColorVector = (X: 0.137255; Y: 0.556863; Z: 0.419608; W: 1);
  ClrSienna: TColorVector = (X: 0.556863; Y: 0.419608; Z: 0.137255; W: 1);
  ClrSkyBlue: TColorVector = (X: 0.196078; Y: 0.6; Z: 0.8; W: 1);
  ClrSlateBlue: TColorVector = (X: 0; Y: 0.498039; Z: 1; W: 1);
  ClrSpringGreen: TColorVector = (X: 0; Y: 1; Z: 0.498039; W: 1);
  ClrSteelBlue: TColorVector = (X: 0.137255; Y: 0.419608; Z: 0.556863; W: 1);
  ClrTan: TColorVector = (X: 0.858824; Y: 0.576471; Z: 0.439216; W: 1);
  ClrThistle: TColorVector = (X: 0.847059; Y: 0.74902; Z: 0.847059; W: 1);
  ClrTurquoise: TColorVector = (X: 0.678431; Y: 0.917647; Z: 0.917647; W: 1);
  ClrViolet: TColorVector = (X: 0.309804; Y: 0.184314; Z: 0.309804; W: 1);
  ClrVioletRed: TColorVector = (X: 0.8; Y: 0.196078; Z: 0.6; W: 1);
  ClrWheat: TColorVector = (X: 0.847059; Y: 0.847059; Z: 0.74902; W: 1);
  ClrYellowGreen: TColorVector = (X: 0.6; Y: 0.8; Z: 0.196078; W: 1);
  ClrSummerSky: TColorVector = (X: 0.22; Y: 0.69; Z: 0.87; W: 1);
  ClrRichBlue: TColorVector = (X: 0.35; Y: 0.35; Z: 0.67; W: 1);
  ClrBrass: TColorVector = (X: 0.71; Y: 0.65; Z: 0.26; W: 1);
  ClrCopper: TColorVector = (X: 0.72; Y: 0.45; Z: 0.20; W: 1);
  ClrBronze: TColorVector = (X: 0.55; Y: 0.47; Z: 0.14; W: 1);
  ClrBronze2: TColorVector = (X: 0.65; Y: 0.49; Z: 0.24; W: 1);
  ClrSilver: TColorVector = (X: 0.90; Y: 0.91; Z: 0.98; W: 1);
  ClrBrightGold: TColorVector = (X: 0.85; Y: 0.85; Z: 0.10; W: 1);
  ClrOldGold: TColorVector = (X: 0.81; Y: 0.71; Z: 0.23; W: 1);
  ClrFeldspar: TColorVector = (X: 0.82; Y: 0.57; Z: 0.46; W: 1);
  ClrQuartz: TColorVector = (X: 0.85; Y: 0.85; Z: 0.95; W: 1);
  ClrNeonPink: TColorVector = (X: 1.00; Y: 0.43; Z: 0.78; W: 1);
  ClrDarkPurple: TColorVector = (X: 0.53; Y: 0.12; Z: 0.47; W: 1);
  ClrNeonBlue: TColorVector = (X: 0.30; Y: 0.30; Z: 1.00; W: 1);
  ClrCoolCopper: TColorVector = (X: 0.85; Y: 0.53; Z: 0.10; W: 1);
  ClrMandarinOrange: TColorVector = (X: 0.89; Y: 0.47; Z: 0.20; W: 1);
  ClrLightWood: TColorVector = (X: 0.91; Y: 0.76; Z: 0.65; W: 1);
  ClrMediumWood: TColorVector = (X: 0.65; Y: 0.50; Z: 0.39; W: 1);
  ClrDarkWood: TColorVector = (X: 0.52; Y: 0.37; Z: 0.26; W: 1);
  ClrSpicyPink: TColorVector = (X: 1.00; Y: 0.11; Z: 0.68; W: 1);
  ClrSemiSweetChoc: TColorVector = (X: 0.42; Y: 0.26; Z: 0.15; W: 1);
  ClrBakersChoc: TColorVector = (X: 0.36; Y: 0.20; Z: 0.09; W: 1);
  ClrFlesh: TColorVector = (X: 0.96; Y: 0.80; Z: 0.69; W: 1);
  ClrNewTan: TColorVector = (X: 0.92; Y: 0.78; Z: 0.62; W: 1);
  ClrNewMidnightBlue: TColorVector = (X: 0.00; Y: 0.00; Z: 0.61; W: 1);
  ClrVeryDarkBrown: TColorVector = (X: 0.35; Y: 0.16; Z: 0.14; W: 1);
  ClrDarkBrown: TColorVector = (X: 0.36; Y: 0.25; Z: 0.20; W: 1);
  ClrDarkTan: TColorVector = (X: 0.59; Y: 0.41; Z: 0.31; W: 1);
  ClrGreenCopper: TColorVector = (X: 0.32; Y: 0.49; Z: 0.46; W: 1);
  ClrDkGreenCopper: TColorVector = (X: 0.29; Y: 0.46; Z: 0.43; W: 1);
  ClrDustyRose: TColorVector = (X: 0.52; Y: 0.39; Z: 0.39; W: 1);
  ClrHuntersGreen: TColorVector = (X: 0.13; Y: 0.37; Z: 0.31; W: 1);
  ClrScarlet: TColorVector = (X: 0.55; Y: 0.09; Z: 0.09; W: 1);
  ClrMediumPurple: TColorVector = (X: 0.73; Y: 0.16; Z: 0.96; W: 1);
  ClrLightPurple: TColorVector = (X: 0.87; Y: 0.58; Z: 0.98; W: 1);
  ClrVeryLightPurple: TColorVector = (X: 0.94; Y: 0.81; Z: 0.99; W: 1);
  ClrGreen: TColorVector = (X: 0; Y: 0.5; Z: 0; W: 1);
  ClrOlive: TColorVector = (X: 0.5; Y: 0.5; Z: 1; W: 1);
  ClrPurple: TColorVector = (X: 1; Y: 0; Z: 1; W: 1);
  ClrTeal: TColorVector = (X: 0; Y: 0.5; Z: 0.5; W: 1);
  ClrRed: TColorVector = (X: 1; Y: 0; Z: 0; W: 1);
  ClrLime: TColorVector = (X: 0; Y: 1; Z: 0; W: 1);
  ClrYellow: TColorVector = (X: 1; Y: 1; Z: 0; W: 1);
  ClrBlue: TColorVector = (X: 0; Y: 0; Z: 1; W: 1);
  ClrFuchsia: TColorVector = (X: 1; Y: 0; Z: 1; W: 1);
  ClrAqua: TColorVector = (X: 0; Y: 1; Z: 1; W: 1);

  CDefaultNormalMapScale = 0.125;

{$IFDEF GLS_CPPB}
  CmtPX = 0;
  CmtNX = 1;
  CmtPY = 2;
  CmtNY = 3;
  CmtPZ = 4;
  CmtNZ = 5;
{$ENDIF}

type

  PRGBColor = ^TRGBColor;
  TRGBColor = TAffineByteVector;

  TGLTextureMode = (TmDecal, TmModulate, TmBlend, TmReplace);
  TGLTextureWrap = (TwBoth, TwNone, TwVertical, TwHorizontal);
  TGLTextureTarget = (TtTexture1d, TtTexture2d, TtTexture3d, TtTextureRect,
    TtTextureCube);

  TGLFaceProperties = class;
  TGLTexture = class;
  TGLMaterial = class;
  TGLMaterialLibrary = class;

  // an interface for proper TGLLibMaterialNameProperty support
  IGLMaterialLibrarySupported = interface(IInterface)
    ['{8E442AF9-D212-4A5E-8A88-92F798BABFD1}']
    function GetMaterialLibrary: TGLMaterialLibrary;
  end;

  TDrawState = (DsRendering, DsPicking, DsPrinting);

  TGLSize = record
    Cx: Longint;
    Cy: Longint;
  end;

  // TRenderContextInfo
  //
  { : Stores contextual info useful during rendering methods. }
  TRenderContextInfo = record
    Scene: TObject;
    Buffer: TObject;
    CameraPosition: TVector;
    CameraDirection, CameraUp: TVector;
    ModelViewMatrix: PMatrix;
    ViewPortSize: TGLSize;
    RenderDPI: Integer;
    MaterialLibrary: TGLMaterialLibrary;
    LightmapLibrary: TGLMaterialLibrary;
    FogDisabledCounter: Integer;
    LightingDisabledCounter: Integer;
    DrawState: TDrawState;
    ObjectsSorting: TGLObjectsSorting;
    VisibilityCulling: TGLVisibilityCulling;
    GLStates: TGLStateCache;
    Rcci: TRenderContextClippingInfo;
    SceneAmbientColor: TColorVector;
    BufferFaceCull: Boolean;
    ProxySubObject: Boolean;
    IgnoreMaterials: Boolean;
    IgnoreBlendingRequests: Boolean;
    Amalgamating: Boolean;
  end;

  PRenderContextInfo = ^TRenderContextInfo;

  // TGLColor
  //
  { : Wraps an OpenGL color. }
  TGLColor = class(TGLUpdateAbleObject)
  private
    { Private Properties }
    FColor: TColorVector;
    FPDefaultColor: PColorVector;
    procedure SetColorVector(const AColor: TColorVector); overload;
    procedure SetColorComponent(Index: Integer; Value: TGLFloat);
    function GetColorComponent(const Index: Integer): TGLFloat;
    procedure SetAsWinColor(const Val: TColor);
    function GetAsWinColor: TColor;

  protected
    { Protected Properties }
    procedure DefineProperties(Filer: TFiler); override;
    procedure ReadData(Stream: TStream);
    procedure WriteData(Stream: TStream);

    function GetHSVA: TVector;
    procedure SetHSVA(const Hsva: TVector);

  public
    { Public Properties }
    constructor Create(AOwner: TPersistent); override;
    constructor CreateInitialized(AOwner: TPersistent;
      const Color: TColorVector; ChangeEvent: TNotifyEvent = nil);
    destructor Destroy; override;
    procedure NotifyChange(Sender: TObject); override;
    procedure Assign(Source: TPersistent); override;
    procedure Initialize(const Color: TColorVector);
    function AsAddress: PGLFloat;

    procedure RandomColor;
    procedure SetColor(Red, Green, Blue: Single; Alpha: Single = 1); overload;

    property Color: TColorVector read FColor write SetColorVector;
    property AsWinColor: TColor read GetAsWinColor write SetAsWinColor;
    property HSVA: TVector read GetHSVA write SetHSVA;

    property DefaultColor: TColorVector read FColor;

{$IFNDEF FPC}
  published
    { Published Properties }
    property Red: TGLFloat index 0 read GetColorComponent
      write SetColorComponent stored False;
    property Green: TGLFloat index 1 read GetColorComponent
      write SetColorComponent stored False;
    property Blue: TGLFloat index 2 read GetColorComponent
      write SetColorComponent stored False;
    property Alpha: TGLFloat index 3 read GetColorComponent
      write SetColorComponent stored False;
{$ELSE}
    property Red: TGLFloat index 0 read GetColorComponent
      write SetColorComponent;
    property Green: TGLFloat index 1 read GetColorComponent
      write SetColorComponent;
    property Blue: TGLFloat index 2 read GetColorComponent
      write SetColorComponent;
    property Alpha: TGLFloat index 3 read GetColorComponent
      write SetColorComponent;
{$ENDIF}
  end;

  // TTextureNeededEvent
  //
  TTextureNeededEvent = procedure(Sender: TObject; var TextureFileName: String)
    of object;

  TGLTextureChange = (TcImage, TcParams);
  TGLTextureChanges = set of TGLTextureChange;

  { : Defines how and if Alpha channel is defined for a texture image.<ul>
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
  TGLTextureImageAlpha = (TiaDefault, TiaAlphaFromIntensity,
    TiaSuperBlackTransparent, TiaLuminance, TiaLuminanceSqrt, TiaOpaque,
    TiaTopLeftPointColorTransparent, TiaInverseLuminance,
    TiaInverseLuminanceSqrt);

  // TGLTextureImage
  //
  { : Base class for texture image data.<p>
    Basicly, subclasses are to be considered as different ways of getting
    a HBitmap (interfacing the actual source).<br>
    SubClasses should be registered using RegisterGLTextureImageClass to allow
    proper persistence and editability in the IDE experts. }
  TGLTextureImage = class(TGLUpdateAbleObject)
  private
    FOwnerTexture: TGLTexture;
    FOnTextureNeeded: TTextureNeededEvent;

  protected
    function GetHeight: Integer; virtual; {$IFDEF GLS_DELPHI} abstract; {$ENDIF}
    function GetWidth: Integer; virtual; {$IFDEF GLS_DELPHI} abstract; {$ENDIF}
    property OnTextureNeeded: TTextureNeededEvent read FOnTextureNeeded
      write FOnTextureNeeded;

  public
    { Public Properties }
    constructor Create(AOwner: TPersistent); override;
    destructor Destroy; override;

    property OwnerTexture: TGLTexture read FOwnerTexture write FOwnerTexture;
    procedure NotifyChange(Sender: TObject); override;

    { : Request to edit the textureImage.<p>
      Returns True if changes have been made.<br>
      This method may be invoked from the IDE or at run-time. }
    function Edit: Boolean;
    { : Save textureImage to file.<p>
      This may not save a picture, but for instance, parameters, if the
      textureImage is a procedural texture. }
    procedure SaveToFile(const FileName: String); dynamic;{$IFDEF GLS_DELPHI}abstract;{$ENDIF}
    { : Load textureImage from a file.<p>
      This may not load a picture, but for instance, parameters, if the
      textureImage is a procedural texture.<br>
      Subclasses should invoke inherited which will take care of the
      "OnTextureNeeded" stuff. }
    procedure LoadFromFile(const FileName: String); dynamic;
    { : Returns a user-friendly denomination for the class.<p>
      This denomination is used for picking a texture image class
      in the IDE expert. }
    class function FriendlyName: String; virtual; {$IFDEF GLS_DELPHI} abstract;{$ENDIF}
    { : Returns a user-friendly description for the class.<p>
      This denomination is used for helping the user when picking a
      texture image class in the IDE expert. If it's not overriden,
      takes its value from FriendlyName. }
    class function FriendlyDescription: String; virtual;
    { : Native opengl texture target.<p>
      Usually GL_TEXTURE_2D (default) or GL_TEXTURE_CUBE_MAP_ARB. }
    class function NativeTextureTarget: TGLUInt; virtual;

    { : Request reload/refresh of data upon next use. }
    procedure Invalidate; dynamic;

    { : Returns image's bitmap handle.<p>
      The specified target can be TEXTURE_2D or one of the cube maps targets.<br>
      If the actual image is not a windows bitmap (BMP), descendants should
      take care of properly converting to bitmap. }
    function GetBitmap32(Target: TGLUInt): TGLBitmap32; virtual;{$IFDEF GLS_DELPHI} abstract; {$ENDIF}
    { : Request for unloading bitmapData, to free some memory.<p>
      This one is invoked when GLScene no longer needs the Bitmap data
      it got through a call to GetHBitmap.<br>
      Subclasses may ignore this call if the HBitmap was obtained at
      no particular memory cost. }
    procedure ReleaseBitmap32; virtual;
    // {: AsBitmap : Returns the TextureImage as a TBitmap }
    function AsBitmap: TGLBitmap;
    procedure AssignToBitmap(ABitmap: TGLBitmap);

    property Width: Integer read GetWidth;
    property Height: Integer read GetHeight;
  end;

  TGLTextureImageClass = class of TGLTextureImage;

  // TGLTextureImageEditor
  //
  TGLTextureImageEditor = class(TObject)
  public
    { Public Properties }
    { : Request to edit a textureImage.<p>
      Returns True if changes have been made.<br>
      This method may be invoked from the IDE or at run-time. }
    class function Edit(ATexImage: TGLTextureImage): Boolean; virtual;{$IFDEF GLS_DELPHI} abstract; {$ENDIF}
  end;

  TGLTextureImageEditorClass = class of TGLTextureImageEditor;

  // TGLBlankImage
  //
  { : A texture image with no specified content, only a size.<p>
    This texture image type is of use if the context of your texture is
    calculated at run-time (with a TGLMemoryViewer for instance). }
  TGLBlankImage = class(TGLTextureImage)
  private
    { Private Declarations }
    FBitmap: TGLBitmap32;
    FWidth, FHeight: Integer;

  protected
    { Protected Declarations }
    procedure SetWidth(Val: Integer);
    function GetWidth: Integer; override;
    procedure SetHeight(Val: Integer);
    function GetHeight: Integer; override;

  public
    { Public Declarations }
    constructor Create(AOwner: TPersistent); override;
    destructor Destroy; override;

    procedure Assign(Source: TPersistent); override;

    function GetBitmap32(Target: TGLUInt): TGLBitmap32; override;
    procedure ReleaseBitmap32; override;

    procedure SaveToFile(const FileName: String); override;
    procedure LoadFromFile(const FileName: String); override;
    class function FriendlyName: String; override;
    class function FriendlyDescription: String; override;

  published
    { Published Declarations }
    { : Width of the blank image (for memory allocation). }
    property Width: Integer read GetWidth write SetWidth default 256;
    { : Width of the blank image (for memory allocation). }
    property Height: Integer read GetHeight write SetHeight default 256;
  end;

  // TGLPictureImage
  //
  { : Base class for image data classes internally based on a TPicture. }
  TGLPictureImage = class(TGLTextureImage)
  private
    { Private Declarations }
    FBitmap: TGLBitmap32;
    FGLPicture: TGLPicture;
    FUpdateCounter: Integer;

  protected
    { Protected Declarations }
    function GetHeight: Integer; override;
    function GetWidth: Integer; override;

    function GetPicture: TGLPicture;
    procedure SetPicture(const APicture: TGLPicture);
    procedure PictureChanged(Sender: TObject);

  public
    { Public Declarations }
    constructor Create(AOwner: TPersistent); override;
    destructor Destroy; override;

    procedure Assign(Source: TPersistent); override;

    { : Use this function if you are going to modify the Picture directly.<p>
      Each invokation MUST be balanced by a call to EndUpdate. }
    procedure BeginUpdate;
    { : Ends a direct picture modification session.<p>
      Follows a BeginUpdate. }
    procedure EndUpdate;
    function GetBitmap32(Target: TGLUInt): TGLBitmap32; override;
    procedure ReleaseBitmap32; override;

    { : Holds the image content. }
    property Picture: TGLPicture read GetPicture write SetPicture;
  end;

  // TGLPersistentImage
  //
  { : Stores any image compatible with Delphi's TPicture mechanism.<p>
    The picture's data is actually stored into the DFM, the original
    picture name or path is not remembered. It is similar in behaviour
    to Delphi's TImage.<p>
    Note that if original image is for instance JPEG format, only the JPEG
    data will be stored in the DFM (compact) }
  TGLPersistentImage = class(TGLPictureImage)
  public
    { Public Declarations }
    constructor Create(AOwner: TPersistent); override;
    destructor Destroy; override;

    procedure SaveToFile(const FileName: String); override;
    procedure LoadFromFile(const FileName: String); override;
    class function FriendlyName: String; override;
    class function FriendlyDescription: String; override;

  published
    { Published Declarations }
    property Picture;
  end;

  // TGLPicFileImage
  //
  { : Uses a picture whose data is found in a file (only filename is stored).<p>
    The image is unloaded after upload to OpenGL. }
  TGLPicFileImage = class(TGLPictureImage)
  private
    FPictureFileName: String;
    FAlreadyWarnedAboutMissingFile: Boolean;
    FWidth: Integer;
    FHeight: Integer;
  protected
    procedure SetPictureFileName(const Val: String);
    function GetHeight: Integer; override;
    function GetWidth: Integer; override;
  public
    { Public Declarations }
    constructor Create(AOwner: TPersistent); override;
    destructor Destroy; override;

    procedure Assign(Source: TPersistent); override;

    // : Only picture file name is saved
    procedure SaveToFile(const FileName: String); override;
    { : Load picture file name or use fileName as picture filename.<p>
      The autodetection is based on the filelength and presence of zeros. }
    procedure LoadFromFile(const FileName: String); override;
    class function FriendlyName: String; override;
    class function FriendlyDescription: String; override;

    function GetBitmap32(Target: TGLUInt): TGLBitmap32; override;
    procedure Invalidate; override;

  published
    { : Filename of the picture to use. }
    property PictureFileName: String read FPictureFileName
      write SetPictureFileName;
  end;

  // TGLCubeMapTarget
  //
{$IFDEF GLS_DELPHI}
  TGLCubeMapTarget = (CmtPX, CmtNX, CmtPY, CmtNY, CmtPZ, CmtNZ);
{$ENDIF}
{$IFDEF GLS_CPPB}
  TGLCubeMapTarget = Integer;
{$ENDIF}

  // TGLCubeMapImage
  //
  { : A texture image used for specifying and stroing a cube map.<p>
    Not unlike TGLPictureImage, but storing 6 of them instead of just one.<br>
    Saving & loading as a whole currently not supported. }
  TGLCubeMapImage = class(TGLTextureImage)
  private
    { Private Declarations }
    FBitmap: TGLBitmap32;
    FUpdateCounter: Integer;
    FPicture: array [CmtPX .. CmtNZ] of TGLPicture;
  protected
    { Protected Declarations }
    function GetWidth: Integer; override;
    function GetHeight: Integer; override;
    procedure SetPicture(Index: TGLCubeMapTarget; const Val: TGLPicture);
    function GetPicture(Index: TGLCubeMapTarget): TGLPicture;

    procedure PictureChanged(Sender: TObject);

  public
    { Public Declarations }
    constructor Create(AOwner: TPersistent); override;
    destructor Destroy; override;

    procedure Assign(Source: TPersistent); override;

    function GetBitmap32(Target: TGLUInt): TGLBitmap32; override;
    procedure ReleaseBitmap32; override;

    { : Use this function if you are going to modify the Picture directly.<p>
      Each invokation MUST be balanced by a call to EndUpdate. }
    procedure BeginUpdate;
    procedure EndUpdate;

    procedure SaveToFile(const FileName: String); override;
    procedure LoadFromFile(const FileName: String); override;
    class function FriendlyName: String; override;
    class function NativeTextureTarget: TGLUInt; override;

    { : Indexed access to the cube map's sub pictures. }
    property Picture[index: TGLCubeMapTarget]: TGLPicture read GetPicture
      write SetPicture;

  published
    { Public Declarations }
{$IFNDEF FPC}
    property PicturePX: TGLPicture index CmtPX read GetPicture write SetPicture;
    property PictureNX: TGLPicture index CmtNX read GetPicture write SetPicture;
    property PicturePY: TGLPicture index CmtPY read GetPicture write SetPicture;
    property PictureNY: TGLPicture index CmtNY read GetPicture write SetPicture;
    property PicturePZ: TGLPicture index CmtPZ read GetPicture write SetPicture;
    property PictureNZ: TGLPicture index CmtNZ read GetPicture write SetPicture;
{$ENDIF}
  end;

  // TGLFloatDataImage
  //
  { : A texture image of float data type.<p>
    Currently only support dynamic nvidia 32bit/16bit RGBA. }
  TGLFloatDataImage = class(TGLTextureImage)
  private
    { Private Declarations }
    FBitmap: TGLBitmap32;
    FWidth, FHeight: Integer;

  protected
    { Protected Declarations }
    procedure SetWidth(Val: Integer);
    function GetWidth: Integer; override;
    procedure SetHeight(Val: Integer);
    function GetHeight: Integer; override;

    // property Picture;

  public
    { Public Declarations }
    constructor Create(AOwner: TPersistent); override;
    destructor Destroy; override;

    procedure Assign(Source: TPersistent); override;

    function GetBitmap32(Target: TGLUInt): TGLBitmap32; override;
    procedure ReleaseBitmap32; override;

    procedure SaveToFile(const FileName: String); override;
    procedure LoadFromFile(const FileName: String); override;
    class function FriendlyName: String; override;
    class function FriendlyDescription: String; override;

    class function NativeTextureTarget: TGLUInt; override;
  published
    { Published Declarations }
    { : Width of the blank image (for memory allocation). }
    property Width: Integer read GetWidth write SetWidth default 256;
    { : Width of the blank image (for memory allocation). }
    property Height: Integer read GetHeight write SetHeight default 256;
  end;

  TGLLibMaterial = Class;

  // TGLShaderStyle
  //
  { : Define GLShader style application relatively to a material.<ul>
    <li>ssHighLevel: shader is applied before material application, and unapplied
    after material unapplication
    <li>ssLowLevel: shader is applied after material application, and unapplied
    before material unapplication
    <li>ssReplace: shader is applied in place of the material (and material
    is completely ignored)
    </ul> }
  TGLShaderStyle = (SsHighLevel, SsLowLevel, SsReplace);

  // TGLShaderFailedInitAction
  //
  { : Defines what to do if for some reason shader failed to initialize.<ul>
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
  TGLShaderFailedInitAction = (FiaSilentDisable, FiaRaiseStandardException,
    FiaRaiseHandledException, FiaReRaiseException
    { ,fiaGenerateEvent } );

  // TGLShader
  //
  { : Generic, abstract shader class.<p>
    Shaders are modeled here as an abstract material-altering entity with
    transaction-like behaviour. The base class provides basic context and user
    tracking, as well as setup/application facilities.<br>
    Subclasses are expected to provide implementation for DoInitialize,
    DoApply, DoUnApply and DoFinalize. }
  TGLShader = class(TGLUpdateAbleComponent)
  private
    { Private Declarations }
    FEnabled: Boolean;
    FLibMatUsers: TList;
    FVirtualHandle: TGLVirtualHandle;
    FShaderStyle: TGLShaderStyle;
    FUpdateCount: Integer;
    FShaderActive, FShaderInitialized: Boolean;
    FFailedInitAction: TGLShaderFailedInitAction;

  protected
    { Protected Declarations }
    { : Invoked once, before the first call to DoApply.<p>
      The call happens with the OpenGL context being active. }
    procedure DoInitialize; dynamic;
    { : Request to apply the shader.<p>
      Always followed by a DoUnApply when the shader is no longer needed. }
    procedure DoApply(var Rci: TRenderContextInfo; Sender: TObject); virtual;{$IFDEF GLS_DELPHI} abstract; {$ENDIF}
    { : Request to un-apply the shader.<p>
      Subclasses can assume the shader has been applied previously.<br>
      Return True to request a multipass. }
    function DoUnApply(var Rci: TRenderContextInfo): Boolean; virtual;{$IFDEF GLS_DELPHI} abstract; {$ENDIF}
    { : Invoked once, before the destruction of context or release of shader.<p>
      The call happens with the OpenGL context being active. }
    procedure DoFinalize; dynamic;

    function GetShaderInitialized: Boolean;
    procedure InitializeShader;
    procedure FinalizeShader;
    procedure OnVirtualHandleAllocate(Sender: TGLVirtualHandle;
      var Handle: Cardinal);
    procedure OnVirtualHandleDestroy(Sender: TGLVirtualHandle;
      var Handle: Cardinal);
    procedure SetEnabled(Val: Boolean);

    property ShaderInitialized: Boolean read GetShaderInitialized;
    property ShaderActive: Boolean read FShaderActive;

    procedure RegisterUser(LibMat: TGLLibMaterial);
    procedure UnRegisterUser(LibMat: TGLLibMaterial);

    { : Used by the DoInitialize procedure of descendant classes to raise errors. }
    procedure HandleFailedInitialization(const LastErrorMessage
      : string = ''); virtual;

    { : May be this should be a function inside HandleFailedInitialization... }
    class function GetStardardNotSupportedMessage: string; virtual;

  public
    { Public Declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    { : Subclasses should invoke this function when shader properties are altered.
      This procedure can also be used to reset/recompile the shader. }
    procedure NotifyChange(Sender: TObject); override;
    procedure BeginUpdate;
    procedure EndUpdate;

    { : Apply shader to OpenGL state machine. }
    procedure Apply(var Rci: TRenderContextInfo; Sender: TObject);
    { : UnApply shader.<p>
      When returning True, the caller is expected to perform a multipass
      rendering by re-rendering then invoking UnApply again, until a
      "False" is returned. }
    function UnApply(var Rci: TRenderContextInfo): Boolean;

    { : Shader application style (default is ssLowLevel). }
    property ShaderStyle: TGLShaderStyle read FShaderStyle write FShaderStyle
      default SsLowLevel;

    procedure Assign(Source: TPersistent); override;

    { : Defines if shader is supported by hardware/drivers.
      Default - always supported. Descendants are encouraged to override
      this function. }
    function ShaderSupported: Boolean; virtual;

    { : Defines what to do if for some reason shader failed to initialize.
      Note, that in some cases it cannon be determined by just checking the
      required OpenGL extentions. You need to try to compile and link the
      shader - only at that stage you might catch an error }
    property FailedInitAction: TGLShaderFailedInitAction read FFailedInitAction
      write FFailedInitAction default FiaRaiseStandardException;

  published
    { Published Declarations }
    { : Turns on/off shader application.<p>
      Note that this only turns on/off the shader application, if the
      ShaderStyle is ssReplace, the material won't be applied even if
      the shader is disabled. }
    property Enabled: Boolean read FEnabled write SetEnabled default True;
  end;

  TGLShaderClass = class of TGLShader;

  // TGLTextureFormat
  //
  { : Texture format for OpenGL (rendering) use.<p>
    Internally, GLScene handles all "base" images as 32 Bits RGBA, but you can
    specify a generic format to reduce OpenGL texture memory use:<ul>
    <li>tfDefault : uses global default format
    <li>tfRGB : 24 bits RGB, 8 bits per component
    <li>tfRGBA : 32 bits RGBA, 8 bits per component
    <li>tfRGB16 : 16 bits RGB (5, 5, 5)
    <li>tfRGBA16 : 16 bits RGBA (4, 4, 4, 4)
    <li>tfAlpha : 8 Bits Alpha-channel only
    <li>tfLuminance : 8 bits Luminance only
    <li>tfLuminanceAlpha : 16 bits Luminance and Alpha channel (8, 8)
    <li>tfIntensity : 8 bits Intensity only
    <li>tfNormalMap : 24 bits RGB normal map, which is computed from the
    original texture (assumed to be an hightmap)
    </ul><br>The actual representation may differ, f.i. old 16bits boards
    may convert everything to 16bit formats, GeForce boards don't have
    a 24 bits format internally and will convert to 32 bits, etc. }
  TGLTextureFormat = (TfDefault, TfRGB, TfRGBA, TfRGB16, TfRGBA16, TfAlpha,
    TfLuminance, TfLuminanceAlpha, TfIntensity, TfNormalMap, TfRGBAFloat16,
    TfRGBAFloat32); // float_type

  // TGLTextureCompression
  //
  { : Texture compression option.<p>
    If OpenGL supports it, this will activate a compressed texture format:<ul>
    <li>tcDefault : uses global default compression option
    <li>tcNone : do not use compression
    <li>tcStandard : use standard compression, average quality, average rate
    <li>tcHighQuality : choose a high-quality, low-speed compression
    <li>tcHighSpeed : choose a high-speed, low-quality compression
    </ul>. }
  TGLTextureCompression = (TcDefault, TcNone, TcStandard, TcHighQuality,
    TcHighSpeed);

  // TGLTextureFilteringQuality
  //
  TGLTextureFilteringQuality = (TfIsotropic, TfAnisotropic);

  // TGLTextureMappingMode
  //
  TGLTextureMappingMode = (TmmUser, TmmObjectLinear, TmmEyeLinear, TmmSphere,
    TmmCubeMapReflection, TmmCubeMapNormal, TmmCubeMapLight0, TmmCubeMapCamera);

  // TGLTexture
  //
  { : Defines basic texturing properties.<p>
    You can control texture wrapping, smoothing/filtering and of course define
    the texture map (note that texturing is disabled by default).<p>
    A built-in mechanism (through ImageAlpha) allows auto-generation of an
    Alpha channel for all bitmaps (see TGLTextureImageAlpha). }
  TGLTexture = class(TGLUpdateAbleObject)
  private
    { Private Declarations }
    FTextureHandle: TGLTextureHandle;
    FTextureMode: TGLTextureMode;
    FTextureWrap: TGLTextureWrap;
    FTextureFormat: TGLTextureFormat;
    FMinFilter: TGLMinFilter;
    FMagFilter: TGLMagFilter;
    FChanges: TGLTextureChanges;
    FDisabled: Boolean;
    FImage: TGLTextureImage;
    FImageAlpha: TGLTextureImageAlpha;
    FImageBrightness: Single;
    FImageGamma: Single;
    FMappingMode: TGLTextureMappingMode;
    FMapSCoordinates: TGLCoordinates4;
    FMapTCoordinates: TGLCoordinates4;
    FOnTextureNeeded: TTextureNeededEvent;
    FCompression: TGLTextureCompression;
    FRequiredMemorySize: Integer;
    FFilteringQuality: TGLTextureFilteringQuality;
    FTexWidth, FTexHeight: Integer;
    FEnvColor: TGLColor;
    FNormalMapScale: Single;

  protected
    { Protected Declarations }
    procedure NotifyImageChange;
    procedure NotifyParamsChange;

    procedure SetImage(AValue: TGLTextureImage);
    procedure SetImageAlpha(const Val: TGLTextureImageAlpha);
    procedure SetImageBrightness(const Val: Single);
    function StoreBrightness: Boolean;
    procedure SetImageGamma(const Val: Single);
    function StoreGamma: Boolean;
    procedure SetMagFilter(AValue: TGLMagFilter);
    procedure SetMinFilter(AValue: TGLMinFilter);
    procedure SetTextureMode(AValue: TGLTextureMode);
    procedure SetTextureWrap(AValue: TGLTextureWrap);
    procedure SetTextureFormat(const Val: TGLTextureFormat);
    procedure SetCompression(const Val: TGLTextureCompression);
    procedure SetFilteringQuality(const Val: TGLTextureFilteringQuality);
    procedure SetMappingMode(const Val: TGLTextureMappingMode);
    function GetMappingSCoordinates: TGLCoordinates4;
    procedure SetMappingSCoordinates(const Val: TGLCoordinates4);
    function GetMappingTCoordinates: TGLCoordinates4;
    procedure SetMappingTCoordinates(const Val: TGLCoordinates4);
    procedure SetDisabled(AValue: Boolean);
    procedure SetEnabled(const Val: Boolean);
    function GetEnabled: Boolean;
    procedure SetEnvColor(const Val: TGLColor);
    procedure SetNormalMapScale(const Val: Single);
    function StoreNormalMapScale: Boolean;

    function StoreImageClassName: Boolean;

    function GetHandle: TGLuint; virtual;
    // : Load texture to OpenGL subsystem
    procedure PrepareImage(Target: TGLUInt); virtual;
    // : Setup OpenGL texture parameters
    procedure PrepareParams(Target: TGLUInt); virtual;

    property OnTextureNeeded: TTextureNeededEvent read FOnTextureNeeded
      write FOnTextureNeeded;
    procedure DoOnTextureNeeded(Sender: TObject; var TextureFileName: String);

  public
    { Public Declarations }
    constructor Create(AOwner: TPersistent); override;
    destructor Destroy; override;

    procedure PrepareBuildList;
    procedure ApplyMappingMode;
    procedure UnApplyMappingMode;
    procedure Apply(var Rci: TRenderContextInfo);
    procedure UnApply(var Rci: TRenderContextInfo);
    { : Applies to TEXTURE1_ARB }
    procedure ApplyAsTexture2(var Rci: TRenderContextInfo;
      LibMaterial: TGLLibMaterial);
    procedure UnApplyAsTexture2(var Rci: TRenderContextInfo;
      LibMaterial: TGLLibMaterial);
    { : N=1 for TEXTURE0_ARB, N=2 for TEXTURE1_ARB, etc. }
    procedure ApplyAsTextureN(N: Integer; var Rci: TRenderContextInfo;
      LibMaterial: TGLLibMaterial);
    procedure UnApplyAsTextureN(N: Integer; var Rci: TRenderContextInfo;
      LibMaterial: TGLLibMaterial);

    procedure Assign(Source: TPersistent); override;
    procedure NotifyChange(Sender: TObject); override;

    procedure DestroyHandles;

    procedure SetImageClassName(const Val: String);
    function GetImageClassName: String;

    { : Returns the OpenGL memory used by the texture.<p>
      The compressed size is returned if, and only if texture compression
      if active and possible, and the texture has been allocated (Handle
      is defined), otherwise the estimated size (from TextureFormat
      specification) is returned. }
    function TextureImageRequiredMemory: Integer;
    { : Allocates the texture handle if not already allocated.<p>
      The texture is binded and parameters are setup, but no image data
      is initialized by this call - for expert use only. }
    function AllocateHandle: TGLuint;
    function IsHandleAllocated: Boolean;
    { : Returns OpenGL texture format corresponding to current options. }
    function OpenGLTextureFormat: Integer;
    { : Returns if of float data type }
    function IsFloatType: Boolean;

    { : Copy texture image from texture memory to main memory.<p>
      Useful for retriving texture data generated with GPU.
      RenderingContext is needed because we need an activated rendering context
      to call OpenGL functions for accessing texture data.
    }
    procedure GetFloatTexImage(RenderingContext: TGLContext; Data: Pointer);
    procedure SetFloatTexImage(RenderingContext: TGLContext; Data: Pointer);

    { : Is the texture enabled?.<p>
      Always equals to 'not Disabled'. }
    property Enabled: Boolean read GetEnabled write SetEnabled;
    { : Handle to the OpenGL texture object.<p>
      If the handle hasn't already been allocated, it will be allocated
      by this call (ie. do not use if no OpenGL context is active!) }
    property Handle: TGLuint read GetHandle;

    { : Actual width used for last texture specification binding. }
    property TexWidth: Integer read FTexWidth;
    { : Actual width used for last texture specification binding. }
    property TexHeight: Integer read FTexHeight;

  published
    { Published Declarations }

    { : Image ClassName for enabling True polymorphism.<p>
      This is ugly, but since the default streaming mechanism does a
      really bad job at storing polymorphic owned-object properties,
      and neither TFiler nor TPicture allow proper use of the built-in
      streaming, that's the only way I found to allow a user-extensible
      mechanism. }
    property ImageClassName: String read GetImageClassName
      write SetImageClassName stored StoreImageClassName;
    { : Image data for the texture.<p> }
    property Image: TGLTextureImage read FImage write SetImage;

    { : Automatic Image Alpha setting.<p>
      Allows to control how and if the image's Alpha channel (transparency)
      is computed. }
    property ImageAlpha: TGLTextureImageAlpha read FImageAlpha
      write SetImageAlpha default TiaDefault;
    { : Texture brightness correction.<p>
      This correction is applied upon loading a TGLTextureImage, it's a
      simple saturating scaling applied to the RGB components of
      the 32 bits image, before it is passed to OpenGL, and before
      gamma correction (if any). }
    property ImageBrightness: Single read FImageBrightness
      write SetImageBrightness stored StoreBrightness;
    { : Texture gamma correction.<p>
      The gamma correction is applied upon loading a TGLTextureImage,
      applied to the RGB components of the 32 bits image, before it is
      passed to OpenGL, after brightness correction (if any). }
    property ImageGamma: Single read FImageGamma write SetImageGamma
      stored StoreGamma;

    { : Texture magnification filter. }
    property MagFilter: TGLMagFilter read FMagFilter write SetMagFilter
      default MaLinear;
    { : Texture minification filter. }
    property MinFilter: TGLMinFilter read FMinFilter write SetMinFilter
      default MiLinearMipMapLinear;
    { : Texture application mode. }
    property TextureMode: TGLTextureMode read FTextureMode write SetTextureMode
      default TmDecal;
    { : Wrapping mode for the texture. }
    property TextureWrap: TGLTextureWrap read FTextureWrap write SetTextureWrap
      default TwBoth;

    { : Texture format for use by the renderer.<p>
      See TGLTextureFormat for details. }
    property TextureFormat: TGLTextureFormat read FTextureFormat
      write SetTextureFormat default TfDefault;
    { : Texture compression control.<p>
      If True the compressed TextureFormat variant (the OpenGL ICD must
      support GL_ARB_texture_compression, or this option is ignored). }
    property Compression: TGLTextureCompression read FCompression
      write SetCompression default TcDefault;
    { : Specifies texture filtering quality.<p>
      You can choose between bilinear and trilinear filetring (anisotropic).<p>
      The OpenGL ICD must support GL_EXT_texture_filter_anisotropic or
      this property is ignored. }
    property FilteringQuality: TGLTextureFilteringQuality read FFilteringQuality
      write SetFilteringQuality default TfIsotropic;

    { : Texture coordinates mapping mode.<p>
      This property controls automatic texture coordinates generation. }
    property MappingMode: TGLTextureMappingMode read FMappingMode
      write SetMappingMode default TmmUser;
    { : Texture mapping coordinates mode for S axis.<p>
      This property stores the coordinates for automatic texture
      coordinates generation. }
    property MappingSCoordinates: TGLCoordinates4 read GetMappingSCoordinates
      write SetMappingSCoordinates;
    { : Texture mapping coordinates mode for T axis.<p>
      This property stores the coordinates for automatic texture
      coordinates generation. }
    property MappingTCoordinates: TGLCoordinates4 read GetMappingTCoordinates
      write SetMappingTCoordinates;

    { : Texture Environment color. }
    property EnvColor: TGLColor read FEnvColor write SetEnvColor;

    { : If true, the texture is disabled (not used). }
    property Disabled: Boolean read FDisabled write SetDisabled default True;

    { : Normal Map scaling.<p>
      Only applies when TextureFormat is tfNormalMap, this property defines
      the scaling that is applied during normal map generation (ie. controls
      the intensity of the bumps). }
    property NormalMapScale: Single read FNormalMapScale write SetNormalMapScale
      stored StoreNormalMapScale;
  end;

  // TGLTextureExItem
  //
  TGLTextureExItem = class(TCollectionItem)
  private
    { Private Decalarations }
    FTexture: TGLTexture;
    FTextureIndex: Integer;
    FTextureOffset, FTextureScale: TGLCoordinates;
    FTextureMatrixIsIdentity: Boolean;
    FTextureMatrix: TMatrix;
    FApplied: Boolean;

  protected
    { Protected Decalarations }
    function GetDisplayName: String; override;
    function GetOwner: TPersistent; override;
    procedure SetTexture(const Value: TGLTexture);
    procedure SetTextureIndex(const Value: Integer);
    procedure SetTextureOffset(const Value: TGLCoordinates);
    procedure SetTextureScale(const Value: TGLCoordinates);
    procedure NotifyTexMapChange(Sender: TObject);

    procedure CalculateTextureMatrix;

    procedure OnNotifyChange(Sender: TObject);

  public
    { Public Decalarations }
    constructor Create(ACollection: TCollection); override;
    destructor Destroy; override;

    procedure Assign(Source: TPersistent); override;
    procedure NotifyChange(Sender: TObject);

    procedure Apply(var Rci: TRenderContextInfo);
    procedure UnApply(var Rci: TRenderContextInfo);

  published
    { Published Decalarations }
    property Texture: TGLTexture read FTexture write SetTexture;
    property TextureIndex: Integer read FTextureIndex write SetTextureIndex;
    property TextureOffset: TGLCoordinates read FTextureOffset
      write SetTextureOffset;
    property TextureScale: TGLCoordinates read FTextureScale
      write SetTextureScale;

  end;

  // TGLTextureEx
  //
  TGLTextureEx = class(TCollection)
  private
    FMaterial: TGLMaterial;

  protected
    { Protected Decalarations }
    procedure SetItems(Index: Integer; const Value: TGLTextureExItem);
    function GetItems(Index: Integer): TGLTextureExItem;
    function GetOwner: TPersistent; override;
    procedure Loaded;

  public
    { Public Decalarations }
    constructor Create(AOwner: TGLMaterial);

    procedure NotifyChange(Sender: TObject);
    procedure Apply(var Rci: TRenderContextInfo);
    procedure UnApply(var Rci: TRenderContextInfo);
    function IsTextureEnabled(Index: Integer): Boolean;

    function Add: TGLTextureExItem;

    property Items[index: Integer]: TGLTextureExItem read GetItems
      write SetItems; default;
  end;

  TShininess = 0 .. 128;
  TPolygonMode = (PmFill, PmLines, PmPoints);

  // TGLFaceProperties
  //
  { : Stores basic face lighting properties.<p>
    The lighting is described with the standard ambient/diffuse/emission/specular
    properties that behave like those of most rendering tools.<br>
    You also have control over shininess (governs specular lighting) and
    polygon mode (lines / fill). }
  TGLFaceProperties = class(TGLUpdateAbleObject)
  private
    { Private Declarations }
    FAmbient, FDiffuse, FSpecular, FEmission: TGLColor;
    FPolygonMode: TPolygonMode;
    FShininess: TShininess;

  protected
    { Protected Declarations }
    procedure SetAmbient(AValue: TGLColor);
    procedure SetDiffuse(AValue: TGLColor);
    procedure SetEmission(AValue: TGLColor);
    procedure SetSpecular(AValue: TGLColor);
    procedure SetPolygonMode(AValue: TPolygonMode);
    procedure SetShininess(AValue: TShininess);

  public
    { Public Declarations }
    constructor Create(AOwner: TPersistent); override;
    destructor Destroy; override;

    procedure Apply(var Rci: TRenderContextInfo; AFace: TGLEnum);
    procedure ApplyNoLighting(var Rci: TRenderContextInfo; AFace: TGLEnum);
    procedure Assign(Source: TPersistent); override;

  published
    { Published Declarations }
    property Ambient: TGLColor read FAmbient write SetAmbient;
    property Diffuse: TGLColor read FDiffuse write SetDiffuse;
    property Emission: TGLColor read FEmission write SetEmission;
    property Shininess: TShininess read FShininess write SetShininess default 0;
    property PolygonMode: TPolygonMode read FPolygonMode write SetPolygonMode
      default PmFill;
    property Specular: TGLColor read FSpecular write SetSpecular;
  end;

  TGLLibMaterialName = String;

  // TBlendingMode
  //
  { : Simplified blending options.<p>
    bmOpaque : disable blending<br>
    bmTransparency : uses standard alpha blending<br>
    bmAdditive : activates additive blending (with saturation)<br>
    bmAlphaTest50 : uses opaque blending, with alpha-testing at 50% (full
    transparency if alpha is below 0.5, full opacity otherwise)<br>
    bmAlphaTest100 : uses opaque blending, with alpha-testing at 100%<br>
    bmModulate : uses modulation blending }
  TBlendingMode = (BmOpaque, BmTransparency, BmAdditive, BmAlphaTest50,
    BmAlphaTest100, BmModulate);

  // TFaceCulling
  //
  TFaceCulling = (FcBufferDefault, FcCull, FcNoCull);

  // TMaterialOptions
  //
  { : Control special rendering options for a material.<p>
    moIgnoreFog : fog is deactivated when the material is rendered }
  TMaterialOption = (MoIgnoreFog, MoNoLighting);
  TMaterialOptions = set of TMaterialOption;

  // TGLMaterial
  //
  { : Describes a rendering material.<p>
    A material is basicly a set of face properties (front and back) that take
    care of standard material rendering parameters (diffuse, ambient, emission
    and specular) and texture mapping.<br>
    An instance of this class is available for almost all objects in GLScene
    to allow quick definition of material properties. It can link to a
    TGLLibMaterial (taken for a material library).<p>
    The TGLLibMaterial has more adavanced properties (like texture transforms)
    and provides a standard way of sharing definitions and texture maps. }
  TGLMaterial = class(TGLUpdateAbleObject, IGLMaterialLibrarySupported)
  private
    { Private Declarations }
    FFrontProperties, FGLBackProperties: TGLFaceProperties;
    FBlendingMode: TBlendingMode;
    FTexture: TGLTexture;
    FTextureEx: TGLTextureEx;
    FMaterialLibrary: TGLMaterialLibrary;
    FLibMaterialName: TGLLibMaterialName;
    FMaterialOptions: TMaterialOptions;
    FFaceCulling: TFaceCulling;
    CurrentLibMaterial: TGLLibMaterial;
    // implementing IGLMaterialLibrarySupported
    function GetMaterialLibrary: TGLMaterialLibrary;
    // implementing IInterface
    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
  protected
    { Protected Declarations }
    function GetBackProperties: TGLFaceProperties;
    procedure SetBackProperties(Values: TGLFaceProperties);
    procedure SetFrontProperties(Values: TGLFaceProperties);
    procedure SetBlendingMode(const Val: TBlendingMode);
    procedure SetMaterialOptions(const Val: TMaterialOptions);
    function GetTexture: TGLTexture;
    procedure SetTexture(ATexture: TGLTexture);
    procedure SetMaterialLibrary(const Val: TGLMaterialLibrary);
    procedure SetLibMaterialName(const Val: TGLLibMaterialName);
    procedure SetFaceCulling(const Val: TFaceCulling);
    function GetTextureEx: TGLTextureEx;
    procedure SetTextureEx(const Value: TGLTextureEx);
    function StoreTextureEx: Boolean;

    procedure NotifyLibMaterialDestruction;
    // : Back, Front, Texture and blending not stored if linked to a LibMaterial
    function StoreMaterialProps: Boolean;

  public
    { Public Declarations }
    constructor Create(AOwner: TPersistent); override;
    destructor Destroy; override;

    procedure PrepareBuildList;
    procedure Apply(var Rci: TRenderContextInfo);
    { : Restore non-standard material states that were altered;<p>
      A return value of True is a multipass request. }
    function UnApply(var Rci: TRenderContextInfo): Boolean;
    procedure Assign(Source: TPersistent); override;
    procedure NotifyChange(Sender: TObject); override;
    procedure NotifyTexMapChange(Sender: TObject);
    procedure DestroyHandles;

    procedure Loaded;

    { : Returns True if the material is blended.<p>
      Will return the libmaterial's blending if it is linked to a material
      library. }
    function Blended: Boolean;

    // : True if the material has a secondary texture
    function HasSecondaryTexture: Boolean;

    // : True if the material comes from the library instead of the texture property
    function MaterialIsLinkedToLib: Boolean;

    // : Gets the primary texture either from material library or the texture property
    function GetActualPrimaryTexture: TGLTexture;

    // : Gets the primary Material either from material library or the texture property
    function GetActualPrimaryMaterial: TGLMaterial;

    // : Return the LibMaterial (see LibMaterialName)
    function GetLibMaterial: TGLLibMaterial;

    procedure QuickAssignMaterial(const MaterialLibrary: TGLMaterialLibrary;
      const Material: TGLLibMaterial);
  published
    { Published Declarations }
    property BackProperties: TGLFaceProperties read GetBackProperties
      write SetBackProperties stored StoreMaterialProps;
    property FrontProperties: TGLFaceProperties read FFrontProperties
      write SetFrontProperties stored StoreMaterialProps;
    property BlendingMode: TBlendingMode read FBlendingMode
      write SetBlendingMode stored StoreMaterialProps default BmOpaque;
    property MaterialOptions: TMaterialOptions read FMaterialOptions
      write SetMaterialOptions default [];
    property Texture: TGLTexture read GetTexture write SetTexture
      stored StoreMaterialProps;
    property FaceCulling: TFaceCulling read FFaceCulling write SetFaceCulling
      default FcBufferDefault;

    property MaterialLibrary: TGLMaterialLibrary read FMaterialLibrary
      write SetMaterialLibrary;
    property LibMaterialName: TGLLibMaterialName read FLibMaterialName
      write SetLibMaterialName;
    property TextureEx: TGLTextureEx read GetTextureEx write SetTextureEx
      stored StoreTextureEx;
  end;

  // TGLLibMaterial
  //
  { : Material in a material library.<p>
    Introduces Texture transformations (offset and scale). Those transformations
    are available only for lib materials to minimize the memory cost of basic
    materials (which are used in almost all objects). }
  TGLLibMaterial = class(TCollectionItem, IGLMaterialLibrarySupported)
  private
    { Private Declarations }
    UserList: TList;
    FName: TGLLibMaterialName;
    FNameHashKey: Integer;
    FMaterial: TGLMaterial;
    FTextureOffset, FTextureScale: TGLCoordinates;
    FTextureMatrixIsIdentity: Boolean;
    FTextureMatrix: TMatrix;
    FTexture2Name: TGLLibMaterialName;
    FShader: TGLShader;
    Notifying: Boolean; // used for recursivity protection
    LibMatTexture2: TGLLibMaterial; // internal cache
    FTag: Integer;
    // implementing IGLMaterialLibrarySupported
    function GetMaterialLibrary: TGLMaterialLibrary;
    // implementing IInterface
    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
  protected
    { Protected Declarations }
    function GetDisplayName: String; override;
    procedure Loaded;

    class function ComputeNameHashKey(const Name: String): Integer;

    procedure SetName(const Val: TGLLibMaterialName);
    procedure SetMaterial(const Val: TGLMaterial);
    procedure SetTextureOffset(const Val: TGLCoordinates);
    procedure SetTextureScale(const Val: TGLCoordinates);
    procedure SetTexture2Name(const Val: TGLLibMaterialName);
    procedure SetShader(const Val: TGLShader);

    procedure CalculateTextureMatrix;
    procedure DestroyHandles;
    procedure OnNotifyChange(Sender: TObject);
    procedure DoOnTextureNeeded(Sender: TObject; var TextureFileName: String);

  public
    { Public Declarations }
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;

    procedure Assign(Source: TPersistent); override;

    procedure PrepareBuildList;
    procedure Apply(var Rci: TRenderContextInfo);
    // : Restore non-standard material states that were altered
    function UnApply(var Rci: TRenderContextInfo): Boolean;

    procedure RegisterUser(Obj: TGLUpdateAbleObject); overload;
    procedure UnregisterUser(Obj: TGLUpdateAbleObject); overload;
    procedure RegisterUser(Comp: TGLUpdateAbleComponent); overload;
    procedure UnregisterUser(Comp: TGLUpdateAbleComponent); overload;
    procedure RegisterUser(LibMaterial: TGLLibMaterial); overload;
    procedure UnregisterUser(LibMaterial: TGLLibMaterial); overload;
    procedure NotifyUsers;
    procedure NotifyUsersOfTexMapChange;
    function IsUsed: Boolean; // returns true if the texture has registed users
    property NameHashKey: Integer read FNameHashKey;

  published
    { Published Declarations }
    property Name: TGLLibMaterialName read FName write SetName;
    property Material: TGLMaterial read FMaterial write SetMaterial;
    property Tag: Integer read FTag write FTag;

    { : Texture offset in texture coordinates.<p>
      The offset is applied <i>after</i> scaling. }
    property TextureOffset: TGLCoordinates read FTextureOffset
      write SetTextureOffset;
    { : Texture coordinates scaling.<p>
      Scaling is applied <i>before</i> applying the offset, and is applied
      to the texture coordinates, meaning that a scale factor of (2, 2, 2)
      will make your texture look twice <i>smaller</i>. }
    property TextureScale: TGLCoordinates read FTextureScale
      write SetTextureScale;

    { : Reference to the second texture.<p>
      The referred LibMaterial *must* be in the same material library.<p>
      Second textures are supported only through ARB multitexturing (ignored
      if not supported). }
    property Texture2Name: TGLLibMaterialName read FTexture2Name
      write SetTexture2Name;

    { : Optionnal shader for the material. }
    property Shader: TGLShader read FShader write SetShader;
  end;

  // TGLLibMaterials
  //
  { : A collection of materials, mainly used in material libraries. }
  TGLLibMaterials = class(TOwnedCollection)
  private
    { Protected Declarations }

  protected
    { Protected Declarations }
    procedure Loaded;

    procedure SetItems(Index: Integer; const Val: TGLLibMaterial);
    function GetItems(Index: Integer): TGLLibMaterial;
    procedure DestroyHandles;

  public
    { Public Declarations }
    constructor Create(AOwner: TComponent);

    function Owner: TPersistent;

    function IndexOf(const Item: TGLLibMaterial): Integer;
    function Add: TGLLibMaterial;
    function FindItemID(ID: Integer): TGLLibMaterial;
    property Items[index: Integer]: TGLLibMaterial read GetItems
      write SetItems; default;
    function MakeUniqueName(const NameRoot: TGLLibMaterialName)
      : TGLLibMaterialName;
    function GetLibMaterialByName(const Name: TGLLibMaterialName)
      : TGLLibMaterial;

    { : Returns index of this Texture if it exists. }
    function GetTextureIndex(const Texture: TGLTexture): Integer;

    { : Returns index of this Material if it exists. }
    function GetMaterialIndex(const Material: TGLMaterial): Integer;

    { : Returns name of this Texture if it exists. }
    function GetNameOfTexture(const Texture: TGLTexture): TGLLibMaterialName;

    { : Returns name of this Material if it exists. }
    function GetNameOfLibMaterial(const Material: TGLLibMaterial)
      : TGLLibMaterialName;

    procedure PrepareBuildList;
    procedure SetNamesToTStrings(AStrings: TStrings);
    { : Deletes all the unused materials in the collection.<p>
      A material is considered unused if no other material or updateable object references it.
      WARNING: For this to work, objects that use the textuere, have to REGISTER to the texture. }
    procedure DeleteUnusedMaterials;
  end;

  // TGLMaterialLibrary
  //
  { : Stores a set of materials, to be used and shared by scene objects.<p>
    Use a material libraries for storing commonly used materials, it provides
    an efficient way to share texture and material data among many objects,
    thus reducing memory needs and rendering time.<p>
    Materials in a material library also feature advanced control properties
    like texture coordinates transforms. }
  TGLMaterialLibrary = class(TGLCadenceAbleComponent)
  private
    { Private Declarations }
    FDoNotClearMaterialsOnLoad: Boolean;
    FMaterials: TGLLibMaterials;
    FTexturePaths: String;
    FOnTextureNeeded: TTextureNeededEvent;
    FTexturePathList: TStringList;
    FLastAppliedMaterial: TGLLibMaterial;

  protected
    { Protected Declarations }
    procedure Loaded; override;
    procedure SetMaterials(const Val: TGLLibMaterials);
    function StoreMaterials: Boolean;
    procedure SetTexturePaths(const Val: String);

  public
    { Public Declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure DestroyHandles;

    procedure WriteToFiler(Writer: TVirtualWriter);
    procedure ReadFromFiler(Reader: TVirtualReader);
    procedure SaveToStream(AStream: TStream); dynamic;
    procedure LoadFromStream(AStream: TStream); dynamic;
    procedure AddMaterialsFromStream(AStream: TStream);

    { : Save library content to a file.<p>
      Recommended extension : .GLML<br>
      Currently saves only texture, ambient, diffuse, emission
      and specular colors. }
    procedure SaveToFile(const FileName: String);
    procedure LoadFromFile(const FileName: String);
    procedure AddMaterialsFromFile(const FileName: String);

    { : Add a "standard" texture material.<p>
      "standard" means linear texturing mode with mipmaps and texture
      modulation mode with default-strength color components.<br>
      If persistent is True, the image will be loaded persistently in memory
      (via a TGLPersistentImage), if false, it will be unloaded after upload
      to OpenGL (via TGLPicFileImage). }
    function AddTextureMaterial(const MaterialName, FileName: String;
      Persistent: Boolean = True): TGLLibMaterial; overload;
    { : Add a "standard" texture material.<p>
      TGLGraphic based variant. }
    function AddTextureMaterial(const MaterialName: String; Graphic: TGLGraphic)
      : TGLLibMaterial; overload;

    { : Returns libMaterial of given name if any exists. }
    function LibMaterialByName(const Name: TGLLibMaterialName): TGLLibMaterial;

    { : Returns Texture of given material's name if any exists. }
    function TextureByName(const LibMatName: TGLLibMaterialName): TGLTexture;

    { : Returns name of texture if any exists. }
    function GetNameOfTexture(const Texture: TGLTexture): TGLLibMaterialName;

    { : Returns name of Material if any exists. }
    function GetNameOfLibMaterial(const LibMat: TGLLibMaterial)
      : TGLLibMaterialName;

    { : Applies the material of given name.<p>
      Returns False if the material could not be found. ake sure this
      call is balanced with a corresponding UnApplyMaterial (or an
      assertion will be triggered in the destructor).<br>
      If a material is already applied, and has not yet been unapplied,
      an assertion will be triggered. }
    function ApplyMaterial(const MaterialName: String;
      var Rci: TRenderContextInfo): Boolean;
    { : Un-applies the last applied material.<p>
      Use this function in conjunction with ApplyMaterial.<br>
      If no material was applied, an assertion will be triggered. }
    function UnApplyMaterial(var Rci: TRenderContextInfo): Boolean;

  published
    { Published Declarations }
    { : The materials collection. }
    property Materials: TGLLibMaterials read FMaterials write SetMaterials
      stored StoreMaterials;
    { : Paths to lookup when attempting to load a texture.<p>
      You can specify multiple paths when loading a texture, the separator
      being the semi-colon ';' character. Directories are looked up from
      first to last, the first file name match is used.<br>
      The current directory is always implicit and checked last.<p>
      Note that you can also use the OnTextureNeeded event to provide a
      filename. }
    property TexturePaths: String read FTexturePaths write SetTexturePaths;
    { : This event is fired whenever a texture needs to be loaded from disk.<p>
      The event is triggered before even attempting to load the texture,
      and before TexturePaths is used. }
    property OnTextureNeeded: TTextureNeededEvent read FOnTextureNeeded
      write FOnTextureNeeded;

  end;

  PColorEntry = ^TColorEntry;

  TColorEntry = record
    Name: String[31];
    Color: TColorVector;
  end;

  // TGLColorManager
  //
  TGLColorManager = class(TList)
  public
    destructor Destroy; override;

    procedure AddColor(const AName: String; const AColor: TColorVector);
    procedure EnumColors(Proc: TGetStrProc); overload;
    procedure EnumColors(AValues: TStrings); overload;

    function FindColor(const AName: String): TColorVector;
    { : Convert a clrXxxx or a '<red green blue alpha> to a color vector }
    function GetColor(const AName: String): TColorVector;
    function GetColorName(const AColor: TColorVector): String;
    procedure RegisterDefaultColors;
    procedure RemoveColor(const AName: String);
  end;

  ETexture = class(Exception);
  EGLShaderException = class(Exception);

function ColorManager: TGLColorManager;

// : Converts a color vector (containing float values)
function ConvertColorVector(const AColor: TColorVector): TColor; overload;
{ : Converts a color vector (containing float values) and alter intensity.<p>
  intensity is in [0..1] }
function ConvertColorVector(const AColor: TColorVector; Intensity: Single)
  : TColor; overload;
// : Converts RGB components into a color vector with correct range
function ConvertRGBColor(const AColor: array of Byte): TColorVector;
// : Converts a delphi color into its RGB fragments and correct range
function ConvertWinColor(AColor: TColor; Alpha: Single = 1): TColorVector;

procedure RegisterColor(const AName: String; const AColor: TColorVector);
procedure UnRegisterColor(const AName: String);

// : Register a TGLTextureImageClass (used for persistence and IDE purposes)
procedure RegisterGLTextureImageClass(TextureImageClass: TGLTextureImageClass);
// : Finds a registerer TGLTextureImageClass using its classname
function FindGLTextureImageClass(const ClassName: String): TGLTextureImageClass;
// : Finds a registerer TGLTextureImageClass using its FriendlyName
function FindGLTextureImageClassByFriendlyName(const FriendlyName: String)
  : TGLTextureImageClass;
// : Defines a TStrings with the list of registered TGLTextureImageClass.
procedure SetGLTextureImageClassesToStrings(AStrings: TStrings);
{ : Creates a TStrings with the list of registered TGLTextureImageClass.<p>
  To be freed by caller. }
function GetGLTextureImageClassesAsStrings: TStrings;

function DecodeGLTextureTarget(const TextureTarget: TGLTextureTarget): Cardinal;
function EncodeGLTextureTarget(const TextureTarget: Cardinal): TGLTextureTarget;

// Global texturing defaults
//
var
  VDefaultTextureFormat: TGLTextureFormat = TfRGBA;
  VDefaultTextureCompression: TGLTextureCompression = TcNone;

  // : Invokes the editor for the given TGLTextureImage
function EditGLTextureImage(ATexImage: TGLTextureImage): Boolean;
procedure RegisterGLTextureImageEditor(ATexImageClass: TGLTextureImageClass;
  TexImageEditor: TGLTextureImageEditorClass);
procedure UnRegisterGLTextureImageEditor(TexImageEditor
  : TGLTextureImageEditorClass);

procedure RegisterTGraphicClassFileExtension(const Extension: String;
  const AClass: TGraphicClass);
function CreateGraphicFromFile(const FileName: String): TGLGraphic;

// ------------------------------------------------------------------------------
// ------------------------------------------------------------------------------
// ------------------------------------------------------------------------------
implementation

// ------------------------------------------------------------------------------
// ------------------------------------------------------------------------------
// ------------------------------------------------------------------------------

uses GLScene, GLStrings, XOpenGL, ApplicationFileIO, PictureRegisteredFormats;

var
  VGLTextureImageClasses: TList;
  VColorManager: TGLColorManager;
  VTIEClass, VTIEEditor: TList;

const
  CTextureMode: array [TmDecal .. TmReplace] of TGLEnum = (GL_DECAL,
    GL_MODULATE, GL_BLEND, GL_REPLACE);

var
  VTGraphicFileExtension: array of String;
  VTGraphicClass: array of TGraphicClass;

  // Dummy method for CPP
{$IFDEF GLS_CPPB}

function TGLTextureImage.GetHeight: Integer;
begin
  Result := 0;
end;

function TGLTextureImage.GetWidth: Integer;
begin
  Result := 0;
end;

procedure TGLTextureImage.SaveToFile(const FileName: String);
begin
end;

class function TGLTextureImage.FriendlyName: String;
begin
  Result := '';
end;

function TGLTextureImage.GetBitmap32(Target: TGLUInt): TGLBitmap32;
begin
  Result := nil;
end;

class function TGLTextureImageEditor.Edit(ATexImage: TGLTextureImage): Boolean;
begin
  Result := True;
end;

procedure TGLShader.DoApply(var Rci: TRenderContextInfo; Sender: TObject);
begin
end;

function TGLShader.DoUnApply(var Rci: TRenderContextInfo): Boolean;
begin
  Result := True;
end;

{$ENDIF}

// DecodeGLTextureTarget
//
function DecodeGLTextureTarget(const TextureTarget: TGLTextureTarget): Cardinal;
begin
  case TextureTarget of
    TtTexture1d:
      Result := GL_TEXTURE_1D;
    TtTexture2d:
      Result := GL_TEXTURE_2D;
    TtTexture3d:
      Result := GL_TEXTURE_3D;
    TtTextureRect:
      Result := GL_TEXTURE_RECTANGLE_ARB;
    TtTextureCube:
      Result := GL_TEXTURE_CUBE_MAP_ARB;
  else
    begin
      Result := 0;
      Assert(False, GlsUnknownType);
    end;
  end;
end;

// EncodeGLTextureTarget
//
function EncodeGLTextureTarget(const TextureTarget: Cardinal): TGLTextureTarget;
begin
  case TextureTarget of
    GL_TEXTURE_1D:
      Result := TtTexture1d;
    GL_TEXTURE_2D:
      Result := TtTexture2d;
    GL_TEXTURE_3D:
      Result := TtTexture3d;
    GL_TEXTURE_RECTANGLE_ARB:
      Result := TtTextureRect;
    GL_TEXTURE_CUBE_MAP_ARB:
      Result := TtTextureCube;
  else
    begin
      Result := TtTexture2d;
      Assert(False, GlsUnknownType);
    end;
  end;
end;

// RegisterTGraphicClassFileExtension
//
procedure RegisterTGraphicClassFileExtension(const Extension: String;
  const AClass: TGraphicClass);
var
  N: Integer;
begin
  N := Length(VTGraphicFileExtension);
  SetLength(VTGraphicFileExtension, N + 1);
  SetLength(VTGraphicClass, N + 1);
  VTGraphicFileExtension[N] := LowerCase(Extension);
  VTGraphicClass[N] := AClass;
end;

// CreateGraphicFromFile
//
function CreateGraphicFromFile(const FileName: String): TGLGraphic;
var
  I: Integer;
  Ext: String;
  Fs: TStream;
  GraphicClass: TGraphicClass;
begin
  Result := nil;
  if FileStreamExists(FileName) then
  begin
    GraphicClass := nil;
    Ext := LowerCase(ExtractFileExt(FileName));
    for I := 0 to High(VTGraphicFileExtension) do
    begin
      if VTGraphicFileExtension[I] = Ext then
      begin
        GraphicClass := TGraphicClass(VTGraphicClass[I]);
        Break;
      end;
    end;
    if GraphicClass = nil then
      GraphicClass := GraphicClassForExtension(Ext);
    if GraphicClass <> nil then
    begin
      Result := GraphicClass.Create;
      try
        Fs := CreateFileStream(FileName, FmOpenRead);
        try
          Result.LoadFromStream(Fs);
        finally
          Fs.Free;
        end;
      except
        FreeAndNil(Result);
        raise;
      end;
    end;
  end;
end;

// EditGLTextureImage
//
function EditGLTextureImage(ATexImage: TGLTextureImage): Boolean;
var
  I: Integer;
  Editor: TGLTextureImageEditorClass;
begin
  if Assigned(VTIEClass) then
  begin
    I := VTIEClass.IndexOf(Pointer(ATexImage.ClassType));
    if I >= 0 then
    begin
      Editor := TGLTextureImageEditorClass(VTIEEditor[I]);
      Result := Editor.Edit(ATexImage);
      Exit;
    end;
  end;
  InformationDlg(ATexImage.ClassName + ': editing not supported.');
  Result := False;
end;

// RegisterGLTextureImageEditor
//
procedure RegisterGLTextureImageEditor(ATexImageClass: TGLTextureImageClass;
  TexImageEditor: TGLTextureImageEditorClass);
begin
  if not Assigned(VTIEClass) then
  begin
    VTIEClass := TList.Create;
    VTIEEditor := TList.Create;
  end;
  VTIEClass.Add(Pointer(ATexImageClass));
  VTIEEditor.Add(TexImageEditor);
end;

// UnRegisterGLTextureImageEditor
//
procedure UnRegisterGLTextureImageEditor(TexImageEditor
  : TGLTextureImageEditorClass);
var
  I: Integer;
begin
  if Assigned(VTIEClass) then
  begin
    I := VTIEEditor.IndexOf(TexImageEditor);
    if I >= 0 then
    begin
      VTIEClass.Delete(I);
      VTIEEditor.Delete(I);
    end;
  end;
end;

// ColorManager
//
function ColorManager: TGLColorManager;
begin
  if not Assigned(VColorManager) then
  begin
    VColorManager := TGLColorManager.Create;
    VColorManager.RegisterDefaultColors;
  end;
  Result := VColorManager;
end;

// RegisterGLTextureImageClass
//
procedure RegisterGLTextureImageClass(TextureImageClass: TGLTextureImageClass);
begin
  if not Assigned(VGLTextureImageClasses) then
    VGLTextureImageClasses := TList.Create;
  VGLTextureImageClasses.Add(TextureImageClass);
end;

// FindGLTextureImageClass
//
function FindGLTextureImageClass(const ClassName: String): TGLTextureImageClass;
var
  I: Integer;
  Tic: TGLTextureImageClass;
begin
  Result := nil;
  if Assigned(VGLTextureImageClasses) then
    for I := 0 to VGLTextureImageClasses.Count - 1 do
    begin
      Tic := TGLTextureImageClass(VGLTextureImageClasses[I]);
      if Tic.ClassName = ClassName then
      begin
        Result := Tic;
        Break;
      end;
    end;

end;

// FindGLTextureImageClassByFriendlyName
//
function FindGLTextureImageClassByFriendlyName(const FriendlyName: String)
  : TGLTextureImageClass;
var
  I: Integer;
  Tic: TGLTextureImageClass;
begin
  Result := nil;
  if Assigned(VGLTextureImageClasses) then
    for I := 0 to VGLTextureImageClasses.Count - 1 do
    begin
      Tic := TGLTextureImageClass(VGLTextureImageClasses[I]);
      if Tic.FriendlyName = FriendlyName then
      begin
        Result := Tic;
        Break;
      end;
    end;
end;

// SetGLTextureImageClassesToStrings
//
procedure SetGLTextureImageClassesToStrings(AStrings: TStrings);
var
  I: Integer;
  Tic: TGLTextureImageClass;
begin
  with AStrings do
  begin
    BeginUpdate;
    Clear;
    if Assigned(VGLTextureImageClasses) then
      for I := 0 to VGLTextureImageClasses.Count - 1 do
      begin
        Tic := TGLTextureImageClass(VGLTextureImageClasses[I]);
        AddObject(Tic.FriendlyName, Pointer(Tic));
      end;
    EndUpdate;
  end;
end;

// GetGLTextureImageClassesAsStrings
//
function GetGLTextureImageClassesAsStrings: TStrings;
begin
  Result := TStringList.Create;
  SetGLTextureImageClassesToStrings(Result);
end;

// IncludeTrailingBackslash
//
function IncludeTrailingBackslash(const S: string): string;
begin
  if IsDelimiter('\', S, Length(S) - 1) then
    Result := S + '\'
  else
    Result := S;
end;

// ------------------
// ------------------ TGLColor ------------------
// ------------------

// Create
//
constructor TGLColor.Create(AOwner: TPersistent);
begin
  inherited;
  Initialize(ClrBlack);
end;

// CreateInitialized
//
constructor TGLColor.CreateInitialized(AOwner: TPersistent;
  const Color: TColorVector; ChangeEvent: TNotifyEvent = nil);
begin
  Create(AOwner);
  Initialize(Color);
  OnNotifyChange := ChangeEvent;
end;

// Destroy
//
destructor TGLColor.Destroy;
begin
  if Assigned(FPDefaultColor) then
    Dispose(FPDefaultColor);
  inherited;
end;

// Initialize
//
procedure TGLColor.Initialize(const Color: TColorVector);
begin
  SetVector(FColor, Color);
  if VUseDefaultSets then
  begin
    if not Assigned(FPDefaultColor) then
      New(FPDefaultColor);
    SetVector(FPDefaultColor^, Color);
  end;
end;

// SetColorVector
//
procedure TGLColor.SetColorVector(const AColor: TColorVector);
begin
  SetVector(FColor, AColor);
  NotifyChange(Self);
end;

// GetColorComponent
//
function TGLColor.GetColorComponent(const Index: Integer): TGLFloat;
begin
  Result := FColor.Coord[Index];
end;

// SetColorComponent
//
procedure TGLColor.SetColorComponent(Index: Integer; Value: TGLFloat);
begin
  if FColor.Coord[index] <> Value then
  begin
    FColor.Coord[index] := Value;
    NotifyChange(Self);
  end;
end;

// SetAsWinColor
//
procedure TGLColor.SetAsWinColor(const Val: TColor);
begin
  FColor := ConvertWinColor(Val);
  NotifyChange(Self);
end;

// GetAsWinColor
//
function TGLColor.GetAsWinColor: TColor;
begin
  Result := ConvertColorVector(FColor);
end;

// Assign
//
procedure TGLColor.Assign(Source: TPersistent);
begin
  if Assigned(Source) and (Source is TGLColor) then
  begin
    FColor := TGLColor(Source).FColor;
    NotifyChange(Self);
  end
  else
    inherited;
end;

// DefineProperties
//
procedure TGLColor.DefineProperties(Filer: TFiler);
begin
  inherited;
  Filer.DefineBinaryProperty('Color', ReadData, WriteData,
    not(Assigned(FPDefaultColor) and VectorEquals(FColor, FPDefaultColor^)));
end;

// ReadData
//
procedure TGLColor.ReadData(Stream: TStream);
begin
  Stream.Read(FColor, SizeOf(FColor));
end;

// WriteData
//
procedure TGLColor.WriteData(Stream: TStream);
begin
  Stream.Write(FColor, SizeOf(FColor));
end;

// NotifyChange
//
procedure TGLColor.NotifyChange(Sender: TObject);
begin
  if Assigned(Owner) then
  begin
    if Owner is TGLBaseSceneObject then
      TGLBaseSceneObject(Owner).StructureChanged;
    inherited;
  end;
end;

// AsAddress
//
function TGLColor.AsAddress: PGLFloat;
begin
  Result := @FColor;
end;

// RandomColor
//
procedure TGLColor.RandomColor;
begin
  Red := Random;
  Green := Random;
  Blue := Random;
end;

// SetColor
//
procedure TGLColor.SetColor(Red, Green, Blue: Single; Alpha: Single = 1);
begin
  FColor.Coord[0] := Red;
  FColor.Coord[1] := Green;
  FColor.Coord[2] := Blue;
  FColor.Coord[3] := Alpha;
  NotifyChange(Self);
end;

// GetHSVA
//
function TGLColor.GetHSVA: TVector;
var
  Delta, Min: Single;
const
  H = 0;
  S = 1;
  V = 2;
begin
  Min := MinFloat(PFloatVector(@FColor), 3);
  Result.Coord[V] := MaxFloat(PFloatVector(@FColor), 3);
  Delta := Result.Coord[V] - Min;

  // saturation is zero if R, G & B are zero
  // hue undefined (zero) if saturation is zero or color is gray (delta=zero)
  if (Result.Coord[V] = 0) or (Delta = 0) then
  begin
    Result.Coord[S] := 0;
    Result.Coord[H] := 0;
  end
  else
  begin
    Result.Coord[S] := Delta / Result.Coord[V];
    if Red = Result.Coord[V] then
      // between yellow and magenta
      Result.Coord[H] := 60 * (Green - Blue) / Delta
    else if Green = Result.Coord[V] then
      // between cyan and yellow
      Result.Coord[H] := 120 + 60 * (Blue - Red) / Delta
    else // between magenta and cyan
      Result.Coord[H] := 240 + 60 * (Red - Green) / Delta;
    if Result.Coord[H] < 0 then // normalize H
      Result.Coord[H] := Result.Coord[H] + 360;
  end;
  Result.Coord[3] := Alpha;
end;

// SetHSVA
//
procedure TGLColor.SetHSVA(const Hsva: TVector);
var
  F, HTemp, P, Q, T: Single;
const
  H = 0;
  S = 1;
  V = 2;
begin
  if Hsva.Coord[S] = 0 then
  begin
    // gray (ignore hue)
    FColor.Coord[0] := Hsva.Coord[V];
    FColor.Coord[1] := Hsva.Coord[V];
    FColor.Coord[2] := Hsva.Coord[V];
  end
  else
  begin
    HTemp := Hsva.Coord[H] * (1 / 60);
    F := Frac(HTemp);

    P := Hsva.Coord[V] * (1 - Hsva.Coord[S]);
    Q := Hsva.Coord[V] * (1 - (Hsva.Coord[S] * F));
    T := Hsva.Coord[V] * (1 - (Hsva.Coord[S] * (1 - F)));

    case Trunc(HTemp) mod 6 of
      0:
        begin
          FColor.Coord[0] := Hsva.Coord[V];
          FColor.Coord[1] := T;
          FColor.Coord[2] := P;
        end;
      1:
        begin
          FColor.Coord[0] := Q;
          FColor.Coord[1] := Hsva.Coord[V];
          FColor.Coord[2] := P;
        end;
      2:
        begin
          FColor.Coord[0] := P;
          FColor.Coord[1] := Hsva.Coord[V];
          FColor.Coord[2] := T;
        end;
      3:
        begin
          FColor.Coord[0] := P;
          FColor.Coord[1] := Q;
          FColor.Coord[2] := Hsva.Coord[V];
        end;
      4:
        begin
          FColor.Coord[0] := T;
          FColor.Coord[1] := P;
          FColor.Coord[2] := Hsva.Coord[V];
        end;
      5:
        begin
          FColor.Coord[0] := Hsva.Coord[V];
          FColor.Coord[1] := P;
          FColor.Coord[2] := Q;
        end;
    end
  end;
  FColor.Coord[3] := Hsva.Coord[3];
  NotifyChange(Self);
end;

// ------------------
// ------------------ TGLFaceProperties ------------------
// ------------------

// Create
//
constructor TGLFaceProperties.Create(AOwner: TPersistent);
begin
  inherited;
  // OpenGL default colors
  FAmbient := TGLColor.CreateInitialized(Self, ClrGray20);
  FDiffuse := TGLColor.CreateInitialized(Self, ClrGray80);
  FEmission := TGLColor.Create(Self);
  FSpecular := TGLColor.Create(Self);
  FShininess := 0;
end;

// Destroy
//
destructor TGLFaceProperties.Destroy;
begin
  FAmbient.Free;
  FDiffuse.Free;
  FEmission.Free;
  FSpecular.Free;
  inherited Destroy;
end;

// Apply
//
procedure TGLFaceProperties.Apply(var Rci: TRenderContextInfo; AFace: TGLEnum);
const
  CPolygonMode: array [PmFill .. PmPoints] of TGLEnum = (GL_FILL, GL_LINE,
    GL_POINT);
begin
  Rci.GLStates.SetGLMaterialColors(AFace, Emission.FColor, Ambient.FColor,
    Diffuse.FColor, Specular.FColor, FShininess);
  Rci.GLStates.SetGLPolygonMode(AFace, CPolygonMode[FPolygonMode]);
end;

// ApplyNoLighting
//
procedure TGLFaceProperties.ApplyNoLighting(var Rci: TRenderContextInfo;
  AFace: TGLEnum);
const
  CPolygonMode: array [PmFill .. PmPoints] of TGLEnum = (GL_FILL, GL_LINE,
    GL_POINT);
begin
  GlColor4fv(@Diffuse.FColor);
  Rci.GLStates.SetGLPolygonMode(AFace, CPolygonMode[FPolygonMode]);
end;

// Assign
//
procedure TGLFaceProperties.Assign(Source: TPersistent);
begin
  if Assigned(Source) and (Source is TGLFaceProperties) then
  begin
    FAmbient.FColor := TGLFaceProperties(Source).FAmbient.FColor;
    FDiffuse.FColor := TGLFaceProperties(Source).FDiffuse.FColor;
    FSpecular.FColor := TGLFaceProperties(Source).FSpecular.FColor;
    FShininess := TGLFaceProperties(Source).FShininess;
    FPolygonMode := TGLFaceProperties(Source).FPolygonMode;
    FEmission.FColor := TGLFaceProperties(Source).FEmission.FColor;
    NotifyChange(Self);
  end;
end;

// SetAmbient
//
procedure TGLFaceProperties.SetAmbient(AValue: TGLColor);
begin
  FAmbient.FColor := AValue.FColor;
  NotifyChange(Self);
end;

// SetDiffuse
//
procedure TGLFaceProperties.SetDiffuse(AValue: TGLColor);
begin
  FDiffuse.FColor := AValue.FColor;
  NotifyChange(Self);
end;

// SetEmission
//
procedure TGLFaceProperties.SetEmission(AValue: TGLColor);
begin
  FEmission.FColor := AValue.FColor;
  NotifyChange(Self);
end;

// SetSpecular
//
procedure TGLFaceProperties.SetSpecular(AValue: TGLColor);
begin
  FSpecular.FColor := AValue.FColor;
  NotifyChange(Self);
end;

// SetPolygonMode
//
procedure TGLFaceProperties.SetPolygonMode(AValue: TPolygonMode);
begin
  if AValue <> FPolygonMode then
  begin
    FPolygonMode := AValue;
    NotifyChange(Self);
  end;
end;

// SetShininess
//
procedure TGLFaceProperties.SetShininess(AValue: TShininess);
begin
  if FShininess <> AValue then
  begin
    FShininess := AValue;
    NotifyChange(Self);
  end;
end;

// ------------------
// ------------------ TGLTextureImage ------------------
// ------------------

// Create
//
constructor TGLTextureImage.Create(AOwner: TPersistent);
begin
  inherited;
  FOwnerTexture := (AOwner as TGLTexture);
end;

// Destroy
//
destructor TGLTextureImage.Destroy;
begin
  inherited Destroy;
end;

// FriendlyDescription
//
class function TGLTextureImage.FriendlyDescription: String;
begin
  Result := FriendlyName;
end;

// NativeTextureTarget
//
class function TGLTextureImage.NativeTextureTarget: TGLUInt;
begin
  Result := GL_TEXTURE_2D;
end;

// Invalidate
//
procedure TGLTextureImage.Invalidate;
begin
  ReleaseBitmap32;
  Include(FOwnerTexture.FChanges, TcImage);
  NotifyChange(Self);
end;

// ReleaseBitmap32
//
procedure TGLTextureImage.ReleaseBitmap32;
begin
  // nothing here.
end;

// AsBitmap : Returns the TextureImage as a TBitmap
// WARNING: This Creates a new bitmap. Remember to free it, to prevent leaks.
// If possible, rather use AssignToBitmap.
//
function TGLTextureImage.AsBitmap: TGLBitmap;
begin
  Result := Self.GetBitmap32(GL_TEXTURE_2D).Create32BitsBitmap;
end;

// AssignToBitmap
//
procedure TGLTextureImage.AssignToBitmap(ABitmap: TGLBitmap);
begin
  Self.GetBitmap32(GL_TEXTURE_2D).AssignToBitmap(ABitmap);
end;

// NotifyChange
//
procedure TGLTextureImage.NotifyChange;
begin
  Include(FOwnerTexture.FChanges, TcImage);
  FOwnerTexture.NotifyChange(Self);
end;

// Edit
//
function TGLTextureImage.Edit: Boolean;
begin
  Result := EditGLTextureImage(Self);
end;

// LoadFromFile
//
procedure TGLTextureImage.LoadFromFile(const FileName: String);
var
  Buf: String;
begin
  if Assigned(FOnTextureNeeded) then
  begin
    Buf := FileName;
    FOnTextureNeeded(Self, Buf);
  end;
end;

// ------------------
// ------------------ TGLBlankImage ------------------
// ------------------

// Create
//
constructor TGLBlankImage.Create(AOwner: TPersistent);
begin
  inherited;
  FWidth := 256;
  FHeight := 256;
end;

// Destroy
//
destructor TGLBlankImage.Destroy;
begin
  ReleaseBitmap32;
  inherited Destroy;
end;

// Assign
//
procedure TGLBlankImage.Assign(Source: TPersistent);
begin
  if Assigned(Source) then
  begin
    if (Source is TGLBlankImage) then
    begin
      FWidth := TGLBlankImage(Source).FWidth;
      FHeight := TGLBlankImage(Source).FHeight;
      Invalidate;
    end
    else
      inherited;
  end
  else
    inherited;
end;

// SetWidth
//
procedure TGLBlankImage.SetWidth(Val: Integer);
begin
  if Val <> FWidth then
  begin
    FWidth := Val;
    if FWidth < 1 then
      FWidth := 1;
    Invalidate;
  end;
end;

// GetWidth
//
function TGLBlankImage.GetWidth: Integer;
begin
  Result := FWidth;
end;

// SetHeight
//
procedure TGLBlankImage.SetHeight(Val: Integer);
begin
  if Val <> FHeight then
  begin
    FHeight := Val;
    if FHeight < 1 then
      FHeight := 1;
    Invalidate;
  end;
end;

// GetHeight
//
function TGLBlankImage.GetHeight: Integer;
begin
  Result := FHeight;
end;

// GetBitmap32
//
function TGLBlankImage.GetBitmap32(Target: TGLUInt): TGLBitmap32;
begin
  if not Assigned(FBitmap) then
  begin
    FBitmap := TGLBitmap32.Create;
    FBitmap.Width := FWidth;
    FBitmap.Height := FHeight;
  end;
  Result := FBitmap;
end;

// ReleaseBitmap32
//
procedure TGLBlankImage.ReleaseBitmap32;
begin
  if Assigned(FBitmap) then
  begin
    FBitmap.Free;
    FBitmap := nil;
  end;
end;

// SaveToFile
//
procedure TGLBlankImage.SaveToFile(const FileName: String);
begin
  SaveStringToFile(FileName, '[BlankImage]'#13#10'Width=' + IntToStr(Width) +
    #13#10'Height=' + IntToStr(Height));
end;

// LoadFromFile
//
procedure TGLBlankImage.LoadFromFile(const FileName: String);
var
  Sl: TStringList;
  Buf: String;
begin
  Buf := FileName;
  if Assigned(FOnTextureNeeded) then
    FOnTextureNeeded(Self, Buf);
  if FileExists(Buf) then
  begin
    Sl := TStringList.Create;
    try
      Sl.LoadFromFile(Buf);
      FWidth := StrToInt(Sl.Values['Width']);
      FHeight := StrToInt(Sl.Values['Height']);
    finally
      Sl.Free;
    end;
  end
  else
  begin
    Assert(False, Format(GlsFailedOpenFile, [FileName]));
  end;
end;

// FriendlyName
//
class function TGLBlankImage.FriendlyName: String;
begin
  Result := 'Blank Image';
end;

// FriendlyDescription
//
class function TGLBlankImage.FriendlyDescription: String;
begin
  Result := 'Blank Image (Width x Height)';
end;

// ------------------
// ------------------ TGLPictureImage ------------------
// ------------------

// Create
//
constructor TGLPictureImage.Create(AOwner: TPersistent);
begin
  inherited;
end;

// Destroy
//
destructor TGLPictureImage.Destroy;
begin
  ReleaseBitmap32;
  FGLPicture.Free;
  inherited Destroy;
end;

// Assign
//
procedure TGLPictureImage.Assign(Source: TPersistent);
var
  Bmp: TGLBitmap;
begin
  if Assigned(Source) then
  begin
    if (Source is TGLPersistentImage) then
      Picture.Assign(TGLPersistentImage(Source).Picture)
    else if (Source is TGLGraphic) then
      Picture.Assign(Source)
    else if (Source is TGLPicture) then
      Picture.Assign(Source)
    else if (Source is TGLBitmap32) then
    begin
      Bmp := TGLBitmap32(Source).Create32BitsBitmap;
      Picture.Graphic := Bmp;
      Bmp.Free;
    end
    else
      inherited;
  end
  else
    inherited;
end;

// BeginUpdate
//
procedure TGLPictureImage.BeginUpdate;
begin
  Inc(FUpdateCounter);
  Picture.OnChange := nil;
end;

// EndUpdate
//
procedure TGLPictureImage.EndUpdate;
begin
  Assert(FUpdateCounter > 0, ClassName + ': Unbalanced Begin/EndUpdate');
  Dec(FUpdateCounter);
  Picture.OnChange := PictureChanged;
  if FUpdateCounter = 0 then
    PictureChanged(Picture);
end;

// GetHeight
//
function TGLPictureImage.GetHeight: Integer;
begin
  Result := Picture.Height;
end;

// GetWidth
//
function TGLPictureImage.GetWidth: Integer;
begin
  Result := Picture.Width;
end;

// GetBitmap32
//
function TGLPictureImage.GetBitmap32(Target: TGLUInt): TGLBitmap32;
begin
  if not Assigned(FBitmap) then
  begin
    FBitmap := TGLBitmap32.Create;
    // we need to deactivate OnChange, due to a "glitch" in some TGraphics,
    // for instance, TJPegImage triggers an OnChange when it is drawn...
    if Assigned(Picture.OnChange) then
    begin
      Picture.OnChange := nil;
      try
        FBitmap.Assign(Picture.Graphic);
      finally
        Picture.OnChange := PictureChanged;
      end;
    end
    else
      FBitmap.Assign(Picture.Graphic);
  end;
  Result := FBitmap;
end;

// ReleaseBitmap32
//
procedure TGLPictureImage.ReleaseBitmap32;
begin
  if Assigned(FBitmap) then
  begin
    FBitmap.Free;
    FBitmap := nil;
  end;
end;

// PictureChanged
//
procedure TGLPictureImage.PictureChanged(Sender: TObject);
begin
  Invalidate;
end;

// GetPicture
//
function TGLPictureImage.GetPicture: TGLPicture;
begin
  if not Assigned(FGLPicture) then
  begin
    FGLPicture := TGLPicture.Create;
    FGLPicture.OnChange := PictureChanged;
  end;
  Result := FGLPicture;
end;

// SetPicture
//
procedure TGLPictureImage.SetPicture(const APicture: TGLPicture);
begin
  Picture.Assign(APicture);
end;

// ------------------
// ------------------ TGLPersistentImage ------------------
// ------------------

// Create
//
constructor TGLPersistentImage.Create(AOwner: TPersistent);
begin
  inherited;
end;

// Destroy
//
destructor TGLPersistentImage.Destroy;
begin
  inherited Destroy;
end;

// SaveToFile
//
procedure TGLPersistentImage.SaveToFile(const FileName: String);
begin
  Picture.SaveToFile(FileName);
end;

// LoadFromFile
//
procedure TGLPersistentImage.LoadFromFile(const FileName: String);
var
  Buf: String;
  Gr: TGLGraphic;
begin
  Buf := FileName;
  if Assigned(FOnTextureNeeded) then
    FOnTextureNeeded(Self, Buf);
  if ApplicationFileIODefined then
  begin
    Gr := CreateGraphicFromFile(Buf);
    if Assigned(Gr) then
    begin
      Picture.Graphic := Gr;
      Gr.Free;
      Exit;
    end;
  end
  else if FileExists(Buf) then
  begin
    Picture.LoadFromFile(Buf);
    Exit;
  end;
  Picture.Graphic := nil;
  raise ETexture.CreateFmt(GlsFailedOpenFile, [FileName]);
end;

// FriendlyName
//
class function TGLPersistentImage.FriendlyName: String;
begin
  Result := 'Persistent Image';
end;

// FriendlyDescription
//
class function TGLPersistentImage.FriendlyDescription: String;
begin
  Result := 'Image data is stored in its original format with other form resources,'
    + 'ie. in the DFM at design-time, and embedded in the EXE at run-time.';
end;

// ------------------
// ------------------ TGLPicFileImage ------------------
// ------------------

// Create
//
constructor TGLPicFileImage.Create(AOwner: TPersistent);
begin
  inherited;
end;

// Destroy
//
destructor TGLPicFileImage.Destroy;
begin
  inherited;
end;

// Assign
//
procedure TGLPicFileImage.Assign(Source: TPersistent);
begin
  if Source is TGLPicFileImage then
  begin
    FPictureFileName := TGLPicFileImage(Source).FPictureFileName
  end
  else
    inherited;
end;

// SetPictureFileName
//
procedure TGLPicFileImage.SetPictureFileName(const Val: String);
begin
  if Val <> FPictureFileName then
  begin
    FPictureFileName := Val;
    FAlreadyWarnedAboutMissingFile := False;
    Invalidate;
  end;
end;

// Invalidate
//
procedure TGLPicFileImage.Invalidate;
begin
  Picture.OnChange := nil;
  try
    Picture.Assign(nil);
    FBitmap := nil;
  finally
    Picture.OnChange := PictureChanged;
  end;
  inherited;
end;

// GetHeight
//
function TGLPicFileImage.GetHeight: Integer;
begin
  Result := FHeight;
end;

// GetWidth
//
function TGLPicFileImage.GetWidth: Integer;
begin
  Result := FWidth;
end;

// GetBitmap32
//
function TGLPicFileImage.GetBitmap32(Target: TGLUInt): TGLBitmap32;
var
  Buf: String;
  Gr: TGLGraphic;
begin
  if (GetWidth <= 0) and (PictureFileName <> '') then
  begin
    Picture.OnChange := nil;
    try
      Buf := PictureFileName;
      if Assigned(FOnTextureNeeded) then
        FOnTextureNeeded(Self, Buf);
      if FileStreamExists(Buf) then
      begin
        Gr := CreateGraphicFromFile(Buf);
        Picture.Graphic := Gr;
        Gr.Free;
      end
      else
      begin
        Picture.Graphic := nil;
        if not FAlreadyWarnedAboutMissingFile then
        begin
          FAlreadyWarnedAboutMissingFile := True;
          Assert(False, Format(GlsFailedOpenFile, [PictureFileName]));
        end;
      end;
      Result := inherited GetBitmap32(Target);
      FWidth := Result.Width;
      FHeight := Result.Height;
      Picture.Graphic := nil;
    finally
      Picture.OnChange := PictureChanged;
    end;
  end
  else
    Result := inherited GetBitmap32(Target);
end;

// SaveToFile
//
procedure TGLPicFileImage.SaveToFile(const FileName: String);
begin
  SaveStringToFile(FileName, PictureFileName);
end;

// LoadFromFile
//
procedure TGLPicFileImage.LoadFromFile(const FileName: String);
var
  Buf: String;
begin
  inherited;
  // attempt to autodetect if we are pointed to a file containing
  // a filename or directly to an image
  if SizeOfFile(FileName) < 512 then
  begin
    Buf := LoadStringFromFile(FileName);
    if Pos(#0, Buf) > 0 then
      PictureFileName := FileName
    else
      PictureFileName := Buf;
  end
  else
    PictureFileName := FileName;
end;

// FriendlyName
//
class function TGLPicFileImage.FriendlyName: String;
begin
  Result := 'PicFile Image';
end;

// FriendlyDescription
//
class function TGLPicFileImage.FriendlyDescription: String;
begin
  Result := 'Image data is retrieved from a file.';
end;

// ------------------
// ------------------ TGLCubeMapImage ------------------
// ------------------

// Create
//
constructor TGLCubeMapImage.Create(AOwner: TPersistent);
var
  I: TGLCubeMapTarget;
begin
  inherited;
  for I := Low(FPicture) to High(FPicture) do
  begin
    FPicture[I] := TGLPicture.Create;
    FPicture[I].OnChange := PictureChanged;
  end;
end;

// Destroy
//
destructor TGLCubeMapImage.Destroy;
var
  I: TGLCubeMapTarget;
begin
  ReleaseBitmap32;
  for I := Low(FPicture) to High(FPicture) do
    FPicture[I].Free;
  inherited Destroy;
end;

// Assign
//
procedure TGLCubeMapImage.Assign(Source: TPersistent);
var
  I: TGLCubeMapTarget;
begin
  if Assigned(Source) then
  begin
    if (Source is TGLCubeMapImage) then
    begin
      for I := Low(FPicture) to High(FPicture) do
        FPicture[I].Assign(TGLCubeMapImage(Source).FPicture[I]);
      Invalidate;
    end
    else
      inherited;
  end
  else
    inherited;
end;

// GetWidth
//
function TGLCubeMapImage.GetWidth: Integer;
begin
  Result := FPicture[CmtPX].Width;
end;

// GetHeight
//
function TGLCubeMapImage.GetHeight: Integer;
begin
  Result := FPicture[CmtPX].Height;
end;

// GetBitmap32
//
function TGLCubeMapImage.GetBitmap32(Target: TGLUInt): TGLBitmap32;
var
  I: TGLCubeMapTarget;
begin
  I := CmtPX;
  case Target of
    GL_TEXTURE_CUBE_MAP_POSITIVE_X_ARB:
      I := CmtPX;
    GL_TEXTURE_CUBE_MAP_NEGATIVE_X_ARB:
      I := CmtNX;
    GL_TEXTURE_CUBE_MAP_POSITIVE_Y_ARB:
      I := CmtPY;
    GL_TEXTURE_CUBE_MAP_NEGATIVE_Y_ARB:
      I := CmtNY;
    GL_TEXTURE_CUBE_MAP_POSITIVE_Z_ARB:
      I := CmtPZ;
    GL_TEXTURE_CUBE_MAP_NEGATIVE_Z_ARB:
      I := CmtNZ;
  end;
  if Assigned(FBitmap) then
    FBitmap.Free;
  FBitmap := TGLBitmap32.Create;
  FPicture[I].OnChange := nil;
  try
    FBitmap.VerticalReverseOnAssignFromBitmap := True;
    FBitmap.Assign(FPicture[I].Graphic);
  finally
    FPicture[I].OnChange := PictureChanged;
  end;
  Result := FBitmap;
end;

// ReleaseBitmap32
//
procedure TGLCubeMapImage.ReleaseBitmap32;
begin
  if Assigned(FBitmap) then
  begin
    FBitmap.Free;
    FBitmap := nil;
  end;
end;

// BeginUpdate
//
procedure TGLCubeMapImage.BeginUpdate;
var
  I: TGLCubeMapTarget;
begin
  Inc(FUpdateCounter);
  for I := Low(FPicture) to High(FPicture) do
    FPicture[I].OnChange := nil;
end;

// EndUpdate
//
procedure TGLCubeMapImage.EndUpdate;
var
  I: TGLCubeMapTarget;
begin
  Assert(FUpdateCounter > 0, ClassName + ': Unbalanced Begin/EndUpdate');
  Dec(FUpdateCounter);
  for I := Low(FPicture) to High(FPicture) do
    FPicture[I].OnChange := PictureChanged;
  if FUpdateCounter = 0 then
    PictureChanged(FPicture[CmtPX]);
end;

// SaveToFile
//
procedure TGLCubeMapImage.SaveToFile(const FileName: String);
var
  Fs: TFileStream;
  Bmp: TGLBitmap;
  I: TGLCubeMapTarget;
  Version: Word;
begin
  Fs := TFileStream.Create(FileName, FmCreate);
  Bmp := TGLBitmap.Create;
  try
    Version := $0100;
    Fs.Write(Version, 2);
    for I := Low(FPicture) to High(FPicture) do
    begin
      Bmp.Assign(FPicture[I].Graphic);
      Bmp.SaveToStream(Fs);
    end;
  finally
    Bmp.Free;
    Fs.Free;
  end;
end;

// LoadFromFile
//
procedure TGLCubeMapImage.LoadFromFile(const FileName: String);
var
  Fs: TFileStream;
  Bmp: TGLBitmap;
  I: TGLCubeMapTarget;
  Version: Word;
begin
  Fs := TFileStream.Create(FileName, FmOpenRead + FmShareDenyWrite);
  Bmp := TGLBitmap.Create;
  try
    Fs.Read(Version, 2);
    Assert(Version = $0100);
    for I := Low(FPicture) to High(FPicture) do
    begin
      Bmp.LoadFromStream(Fs);
      FPicture[I].Graphic := Bmp;
    end;
  finally
    Bmp.Free;
    Fs.Free;
  end;
end;

// FriendlyName
//
class function TGLCubeMapImage.FriendlyName: String;
begin
  Result := 'CubeMap Image';
end;

// NativeTextureTarget
//
class function TGLCubeMapImage.NativeTextureTarget: TGLUInt;
begin
  Result := GL_TEXTURE_CUBE_MAP_ARB;
end;

// PictureChanged
//
procedure TGLCubeMapImage.PictureChanged(Sender: TObject);
begin
  Invalidate;
end;

// SetPicture
//
procedure TGLCubeMapImage.SetPicture(Index: TGLCubeMapTarget;
  const Val: TGLPicture);
begin
  FPicture[index].Assign(Val);
end;

// GetPicture
//
function TGLCubeMapImage.GetPicture(Index: TGLCubeMapTarget): TGLPicture;
begin
  Result := FPicture[index];
end;

// ------------------
// ------------------ TGLShader ------------------
// ------------------

// Create
//
constructor TGLShader.Create(AOwner: TComponent);
begin
  FLibMatUsers := TList.Create;
  FVirtualHandle := TGLVirtualHandle.Create;
  FShaderStyle := SsLowLevel;
  FEnabled := True;
  FFailedInitAction := FiaRaiseStandardException;
  inherited;
end;

// Destroy
//
destructor TGLShader.Destroy;
var
  I: Integer;
  List: TList;
begin
  FVirtualHandle.DestroyHandle;
  FinalizeShader;
  inherited;
  List := FLibMatUsers;
  FLibMatUsers := nil;
  for I := List.Count - 1 downto 0 do
    TGLLibMaterial(List[I]).Shader := nil;
  List.Free;
  FVirtualHandle.Free;
end;

// NotifyChange
//
procedure TGLShader.NotifyChange(Sender: TObject);
var
  I: Integer;
begin
  if FUpdateCount = 0 then
  begin
    for I := FLibMatUsers.Count - 1 downto 0 do
      TGLLibMaterial(FLibMatUsers[I]).NotifyUsers;
    FinalizeShader;
  end;
end;

// BeginUpdate
//
procedure TGLShader.BeginUpdate;
begin
  Inc(FUpdateCount);
end;

// EndUpdate
//
procedure TGLShader.EndUpdate;
begin
  Dec(FUpdateCount);
  if FUpdateCount = 0 then
    NotifyChange(Self);
end;

// DoInitialize
//
procedure TGLShader.DoInitialize;
begin
  // nothing here
end;

// DoFinalize
//
procedure TGLShader.DoFinalize;
begin
  // nothing here
end;

// GetShaderInitialized
//
function TGLShader.GetShaderInitialized: Boolean;
begin
  Result := (FVirtualHandle.Handle <> 0);
end;

// InitializeShader
//
procedure TGLShader.InitializeShader;
begin
  if FVirtualHandle.Handle = 0 then
  begin
    FVirtualHandle.OnAllocate := OnVirtualHandleAllocate;
    FVirtualHandle.OnDestroy := OnVirtualHandleDestroy;
    FVirtualHandle.AllocateHandle;
    FShaderInitialized := True;
    DoInitialize;
  end;
end;

// FinalizeShader
//
procedure TGLShader.FinalizeShader;
var
  ActivateContext: Boolean;
begin
  if FVirtualHandle.Handle <> 0 then
  begin
    if FShaderInitialized then
    begin
      ActivateContext := (not FVirtualHandle.RenderingContext.Active);
      if ActivateContext then
        FVirtualHandle.RenderingContext.Activate;
      try
        FShaderInitialized := False;
        DoFinalize;
      finally
        if ActivateContext then
          FVirtualHandle.RenderingContext.Deactivate;
      end;
      FVirtualHandle.DestroyHandle;
    end;
  end;
end;

// Apply
//
procedure TGLShader.Apply(var Rci: TRenderContextInfo; Sender: TObject);
begin
  Assert(not FShaderActive, 'Unbalanced shader application.');

  // Need to check it twice, because shader may refuse to initialize
  // and choose to disable itself during initialization.
  if FEnabled then
    if FVirtualHandle.Handle = 0 then
      InitializeShader;

  if FEnabled then
    DoApply(Rci, Sender);

  FShaderActive := True;
end;

// UnApply
//
function TGLShader.UnApply(var Rci: TRenderContextInfo): Boolean;
begin
  Assert(FShaderActive, 'Unbalanced shader application.');
  if Enabled then
  begin
    Result := DoUnApply(Rci);
    if not Result then
      FShaderActive := False;
  end
  else
  begin
    FShaderActive := False;
    Result := False;
  end;
end;

// OnVirtualHandleDestroy
//
procedure TGLShader.OnVirtualHandleDestroy(Sender: TGLVirtualHandle;
  var Handle: Cardinal);
begin
  FinalizeShader;
  Handle := 0;
end;

// OnVirtualHandleAllocate
//
procedure TGLShader.OnVirtualHandleAllocate(Sender: TGLVirtualHandle;
  var Handle: Cardinal);
begin
  Handle := 1;
end;

// SetEnabled
//
procedure TGLShader.SetEnabled(Val: Boolean);
begin
  Assert(not FShaderActive, 'Shader is active.');
  if Val <> FEnabled then
  begin
    FEnabled := Val;
    NotifyChange(Self);
  end;
end;

// RegisterUser
//
procedure TGLShader.RegisterUser(LibMat: TGLLibMaterial);
var
  I: Integer;
begin
  I := FLibMatUsers.IndexOf(LibMat);
  if I < 0 then
    FLibMatUsers.Add(LibMat);
end;

// UnRegisterUser
//
procedure TGLShader.UnRegisterUser(LibMat: TGLLibMaterial);
begin
  if Assigned(FLibMatUsers) then
    FLibMatUsers.Remove(LibMat);
end;

// Assign
//
procedure TGLShader.Assign(Source: TPersistent);
begin
  if Source is TGLShader then
  begin
    FShaderStyle := TGLShader(Source).FShaderStyle;
    FFailedInitAction := TGLShader(Source).FFailedInitAction;
    Enabled := TGLShader(Source).FEnabled;
  end
  else
    inherited Assign(Source); // to the pit of doom ;)
end;

// Assign
//
function TGLShader.ShaderSupported: Boolean;
begin
  Result := True;
end;

// HandleFailedInitialization
//
procedure TGLShader.HandleFailedInitialization(const LastErrorMessage
  : string = '');
begin
  case FailedInitAction of
    FiaSilentdisable:
      ; // Do nothing ;)
    FiaRaiseHandledException:
      try
        raise EGLShaderException.Create(GetStardardNotSupportedMessage);
      except
      end;
    FiaRaiseStandardException:
      raise EGLShaderException.Create(GetStardardNotSupportedMessage);
    FiaReRaiseException:
      begin
        if LastErrorMessage <> '' then
          raise EGLShaderException.Create(LastErrorMessage)
        else
          raise EGLShaderException.Create(GetStardardNotSupportedMessage)
      end;
    // fiaGenerateEvent:; // Do nothing. Event creation is left up to user shaders
    // // which may choose to override this procedure.
  else
    Assert(False, GlsUnknownType);
  end;
end;

// GetStardardNotSupportedMessage
//
class function TGLShader.GetStardardNotSupportedMessage: string;
begin
  Result := 'Your hardware/driver doesn''t support shader ' + ClassName + '!';
end;

// ------------------
// ------------------ TGLTexture ------------------
// ------------------

// Create
//
constructor TGLTexture.Create(AOwner: TPersistent);
begin
  inherited;
  FDisabled := True;
  FChanges := [TcImage, TcParams];
  FImage := TGLPersistentImage.Create(Self);
  FImage.FOnTextureNeeded := DoOnTextureNeeded;
  FImageAlpha := TiaDefault;
  FImageBrightness := 1.0;
  FImageGamma := 1.0;
  FMagFilter := MaLinear;
  FMinFilter := MiLinearMipMapLinear;
  FFilteringQuality := TfIsotropic;
  FRequiredMemorySize := -1;
  FTextureHandle := TGLTextureHandle.Create;
  FMappingMode := TmmUser;
  FEnvColor := TGLColor.CreateInitialized(Self, ClrTransparent);
  FNormalMapScale := CDefaultNormalMapScale;
end;

// Destroy
//
destructor TGLTexture.Destroy;
begin
  FEnvColor.Free;
  FMapSCoordinates.Free;
  FMapTCoordinates.Free;
  DestroyHandles;
  FTextureHandle.Free;
  FImage.Free;
  inherited Destroy;
end;

// Assign
//
procedure TGLTexture.Assign(Source: TPersistent);
begin
  if Assigned(Source) then
  begin
    if (Source is TGLTexture) then
    begin
      if Source <> Self then
      begin
        FImageAlpha := TGLTexture(Source).FImageAlpha;
        FTextureMode := TGLTexture(Source).FTextureMode;
        FTextureWrap := TGLTexture(Source).FTextureWrap;
        FTextureFormat := TGLTexture(Source).FTextureFormat;
        FCompression := TGLTexture(Source).FCompression;
        FMinFilter := TGLTexture(Source).FMinFilter;
        FMagFilter := TGLTexture(Source).FMagFilter;
        FMappingMode := TGLTexture(Source).FMappingMode;
        MappingSCoordinates.Assign(TGLTexture(Source).MappingSCoordinates);
        MappingTCoordinates.Assign(TGLTexture(Source).MappingTCoordinates);
        FDisabled := TGLTexture(Source).FDisabled;
        SetImage(TGLTexture(Source).FImage);
        FChanges := [TcParams, TcImage];
      end;
    end
    else if (Source is TGLGraphic) then
    begin
      Image.Assign(Source);
    end
    else if (Source is TGLPicture) then
    begin
      Image.Assign(TGLPicture(Source).Graphic);
    end
    else
      inherited Assign(Source);
  end
  else
  begin
    FDisabled := True;
    SetImage(nil);
    FChanges := [TcParams, TcImage];
  end;
end;

// NotifyChange
//
procedure TGLTexture.NotifyChange(Sender: TObject);
begin
  if Assigned(Owner) then
  begin
    if Owner is TGLTextureExItem then
      TGLTextureExItem(Owner).NotifyChange(Sender);
  end;

  inherited;
end;

// NotifyImageChange
//
procedure TGLTexture.NotifyImageChange;
begin
  Include(FChanges, TcImage);
  NotifyChange(Self);
end;

// NotifyParamsChange
//
procedure TGLTexture.NotifyParamsChange;
begin
  Include(FChanges, TcParams);
  NotifyChange(Self);
end;

// SetImage
//
procedure TGLTexture.SetImage(AValue: TGLTextureImage);
begin
  if Assigned(AValue) then
  begin
    if FImage.ClassType <> AValue.ClassType then
    begin
      FImage.Free;
      FImage := TGLTextureImageClass(AValue.ClassType).Create(Self);
      FImage.OnTextureNeeded := DoOnTextureNeeded;
    end;
    FImage.Assign(AValue);
  end
  else
  begin
    FImage.Free;
    FImage := TGLPersistentImage.Create(Self);
    FImage.FOnTextureNeeded := DoOnTextureNeeded;
  end;
end;

// SetImageClassName
//
procedure TGLTexture.SetImageClassName(const Val: String);
begin
  if Val <> '' then
    if FImage.ClassName <> Val then
    begin
      FImage.Free;
      FImage := TGLTextureImageClass(FindGLTextureImageClass(Val)).Create(Self);
      FImage.OnTextureNeeded := DoOnTextureNeeded;
      NotifyImageChange;
    end;
end;

// GetImageClassName
//
function TGLTexture.GetImageClassName: String;
begin
  Result := FImage.ClassName;
end;

// TextureImageRequiredMemory
//
function TGLTexture.TextureImageRequiredMemory: Integer;
const
  CTextureFormatToPixelSize: array [TfRGB .. high(TGLTextureFormat)
    ] of Integer = ( // float_type
    3, 4, 2, 2, 1, 8, 16, 1, 2, 1, 3);
var
  Tf: TGLTextureFormat;
begin
  if FRequiredMemorySize > 0 then
    Result := FRequiredMemorySize
  else
  begin
    if TextureFormat = TfDefault then
      if VDefaultTextureFormat = TfDefault then
        Tf := TfRGBA
      else
        Tf := VDefaultTextureFormat
    else
      Tf := TextureFormat;
    Result := CTextureFormatToPixelSize[Tf] * Image.Width * Image.Height;
  end;
end;

// SetImageAlpha
//
procedure TGLTexture.SetImageAlpha(const Val: TGLTextureImageAlpha);
begin
  if FImageAlpha <> Val then
  begin
    FImageAlpha := Val;
    NotifyImageChange;
  end;
end;

// SetImageBrightness
//
procedure TGLTexture.SetImageBrightness(const Val: Single);
begin
  if FImageBrightness <> Val then
  begin
    FImageBrightness := Val;
    NotifyImageChange;
  end;
end;

// StoreBrightness
//
function TGLTexture.StoreBrightness: Boolean;
begin
  Result := (FImageBrightness <> 1.0);
end;

// SetImageGamma
//
procedure TGLTexture.SetImageGamma(const Val: Single);
begin
  if FImageGamma <> Val then
  begin
    FImageGamma := Val;
    NotifyImageChange;
  end;
end;

// StoreGamma
//
function TGLTexture.StoreGamma: Boolean;
begin
  Result := (FImageGamma <> 1.0);
end;

// SetMagFilter
//
procedure TGLTexture.SetMagFilter(AValue: TGLMagFilter);
begin
  if AValue <> FMagFilter then
  begin
    FMagFilter := AValue;
    NotifyParamsChange;
  end;
end;

// SetMinFilter
//
procedure TGLTexture.SetMinFilter(AValue: TGLMinFilter);
begin
  if AValue <> FMinFilter then
  begin
    FMinFilter := AValue;
    NotifyParamsChange;
  end;
end;

// SetTextureMode
//
procedure TGLTexture.SetTextureMode(AValue: TGLTextureMode);
begin
  if AValue <> FTextureMode then
  begin
    FTextureMode := AValue;
    NotifyParamsChange;
  end;
end;

// SetDisabled
//
procedure TGLTexture.SetDisabled(AValue: Boolean);
begin
  if AValue <> FDisabled then
  begin
    FDisabled := AValue;
    if Assigned(Owner) or ((Owner is TGLMaterial) or (Owner is TGLTextureExItem))
    then
    begin
      if Owner is TGLMaterial then
        TGLMaterial(Owner).NotifyTexMapChange(Self);
      if Owner is TGLTextureExItem then
        TGLTextureExItem(Owner).NotifyTexMapChange(Self);
    end
    else
      NotifyChange(Self);
  end;
end;

// SetEnabled
//
procedure TGLTexture.SetEnabled(const Val: Boolean);
begin
  Disabled := not Val;
end;

// GetEnabled
//
function TGLTexture.GetEnabled: Boolean;
begin
  Result := not Disabled;
end;

// SetEnvColor
//
procedure TGLTexture.SetEnvColor(const Val: TGLColor);
begin
  FEnvColor.Assign(Val);
  NotifyParamsChange;
end;

// SetNormalMapScale
//
procedure TGLTexture.SetNormalMapScale(const Val: Single);
begin
  if Val <> FNormalMapScale then
  begin
    FNormalMapScale := Val;
    if TextureFormat = TfNormalMap then
      NotifyImageChange;
  end;
end;

// StoreNormalMapScale
//
function TGLTexture.StoreNormalMapScale: Boolean;
begin
  Result := (FNormalMapScale <> CDefaultNormalMapScale);
end;

// SetTextureWrap
//
procedure TGLTexture.SetTextureWrap(AValue: TGLTextureWrap);
begin
  if AValue <> FTextureWrap then
  begin
    FTextureWrap := AValue;
    NotifyParamsChange;
  end;
end;

// SetTextureFormat
//
procedure TGLTexture.SetTextureFormat(const Val: TGLTextureFormat);
begin
  if Val <> FTextureFormat then
  begin
    FTextureFormat := Val;
    NotifyParamsChange;
  end;
end;

// SetCompression
//
procedure TGLTexture.SetCompression(const Val: TGLTextureCompression);
begin
  if Val <> FCompression then
  begin
    FCompression := Val;
    NotifyParamsChange;
  end;
end;

// SetFilteringQuality
//
procedure TGLTexture.SetFilteringQuality(const Val: TGLTextureFilteringQuality);
begin
  if Val <> FFilteringQuality then
  begin
    FFilteringQuality := Val;
    NotifyParamsChange;
  end;
end;

// SetMappingMode
//
procedure TGLTexture.SetMappingMode(const Val: TGLTextureMappingMode);
var
  TexMapChange: Boolean;
begin
  if Val <> FMappingMode then
  begin
    TexMapChange := ((Val = TmmUser) and (FMappingMode <> TmmUser)) or
      ((Val = TmmUser) and (FMappingMode <> TmmUser));
    FMappingMode := Val;
    if TexMapChange then
    begin
      // when switching between texGen modes and user mode, the geometry
      // must be rebuilt in whole (to specify/remove texCoord data!)
      if Assigned(Owner) and (Owner is TGLMaterial) then
        TGLMaterial(Owner).NotifyTexMapChange(Self);
    end
    else
      NotifyChange(Self);
  end;
end;

// SetMappingSCoordinates
//
procedure TGLTexture.SetMappingSCoordinates(const Val: TGLCoordinates4);
begin
  MappingSCoordinates.Assign(Val);
end;

// GetMappingSCoordinates
//
function TGLTexture.GetMappingSCoordinates: TGLCoordinates4;
begin
  if not Assigned(FMapSCoordinates) then
    FMapSCoordinates := TGLCoordinates4.CreateInitialized(Self, XHmgVector,
      CsVector);
  Result := FMapSCoordinates;
end;

// SetMappingTCoordinates
//
procedure TGLTexture.SetMappingTCoordinates(const Val: TGLCoordinates4);
begin
  MappingTCoordinates.Assign(Val);
end;

// GetMappingTCoordinates
//
function TGLTexture.GetMappingTCoordinates: TGLCoordinates4;
begin
  if not Assigned(FMapTCoordinates) then
    FMapTCoordinates := TGLCoordinates4.CreateInitialized(Self, XHmgVector,
      CsVector);
  Result := FMapTCoordinates;
end;

// StoreImageClassName
//
function TGLTexture.StoreImageClassName: Boolean;
begin
  Result := (FImage.ClassName <> TGLPersistentImage.ClassName);
end;

// PrepareBuildList
//
procedure TGLTexture.PrepareBuildList;
begin
  GetHandle;
end;

// ApplyMappingMode
//
procedure TGLTexture.ApplyMappingMode;
begin
  case MappingMode of
    TmmUser:
      ; // nothing to do, but checked first (common case)
    TmmObjectLinear:
      begin
        GlTexGeni(GL_S, GL_TEXTURE_GEN_MODE, GL_OBJECT_LINEAR);
        GlTexGeni(GL_T, GL_TEXTURE_GEN_MODE, GL_OBJECT_LINEAR);
        GlTexGenfv(GL_S, GL_OBJECT_PLANE, @MappingSCoordinates.DirectVector);
        GlTexGenfv(GL_T, GL_OBJECT_PLANE, @MappingTCoordinates.DirectVector);
        GlEnable(GL_TEXTURE_GEN_S);
        GlEnable(GL_TEXTURE_GEN_T);
      end;
    TmmEyeLinear:
      begin
        GlTexGeni(GL_S, GL_TEXTURE_GEN_MODE, GL_EYE_LINEAR);
        GlTexGeni(GL_T, GL_TEXTURE_GEN_MODE, GL_EYE_LINEAR);
        GlTexGenfv(GL_S, GL_EYE_PLANE, @MappingSCoordinates.DirectVector);
        GlTexGenfv(GL_T, GL_EYE_PLANE, @MappingTCoordinates.DirectVector);
        GlEnable(GL_TEXTURE_GEN_S);
        GlEnable(GL_TEXTURE_GEN_T);
      end;
    TmmSphere:
      begin
        GlTexGeni(GL_S, GL_TEXTURE_GEN_MODE, GL_SPHERE_MAP);
        GlTexGeni(GL_T, GL_TEXTURE_GEN_MODE, GL_SPHERE_MAP);
        GlEnable(GL_TEXTURE_GEN_S);
        GlEnable(GL_TEXTURE_GEN_T);
      end;
    TmmCubeMapReflection, TmmCubeMapCamera:
      if GL_ARB_texture_cube_map then
      begin
        GlTexGeni(GL_S, GL_TEXTURE_GEN_MODE, GL_REFLECTION_MAP_ARB);
        GlTexGeni(GL_T, GL_TEXTURE_GEN_MODE, GL_REFLECTION_MAP_ARB);
        GlTexGeni(GL_R, GL_TEXTURE_GEN_MODE, GL_REFLECTION_MAP_ARB);
        GlEnable(GL_TEXTURE_GEN_S);
        GlEnable(GL_TEXTURE_GEN_T);
        GlEnable(GL_TEXTURE_GEN_R);
      end;
    TmmCubeMapNormal, TmmCubeMapLight0:
      if GL_ARB_texture_cube_map then
      begin
        GlTexGeni(GL_S, GL_TEXTURE_GEN_MODE, GL_NORMAL_MAP_ARB);
        GlTexGeni(GL_T, GL_TEXTURE_GEN_MODE, GL_NORMAL_MAP_ARB);
        GlTexGeni(GL_R, GL_TEXTURE_GEN_MODE, GL_NORMAL_MAP_ARB);
        GlEnable(GL_TEXTURE_GEN_S);
        GlEnable(GL_TEXTURE_GEN_T);
        GlEnable(GL_TEXTURE_GEN_R);
      end;
  else
    Assert(False);
  end;
end;

// ApplyMappingMode
//
procedure TGLTexture.UnApplyMappingMode;
begin
  if MappingMode <> TmmUser then
  begin
    GlDisable(GL_TEXTURE_GEN_S);
    GlDisable(GL_TEXTURE_GEN_T);
    GlDisable(GL_TEXTURE_GEN_R);
  end;
end;

// Apply
//
procedure TGLTexture.Apply(var Rci: TRenderContextInfo);
var
  M: TMatrix;
begin
  if not Disabled then
  begin
    if Image.NativeTextureTarget = GL_TEXTURE_2D then
    begin
      Rci.GLStates.SetGLState(StTexture2D);
      Rci.GLStates.SetGLCurrentTexture(0, GL_TEXTURE_2D, Handle);
    end
    else
      // NV float needs GL_TEXTURE_RECTANGLE_NV; doesn't affect ATI_float
      if Image.NativeTextureTarget = GL_TEXTURE_RECTANGLE_NV then
      begin
        Rci.GLStates.SetGLState(StTextureRect);
        Rci.GLStates.SetGLCurrentTexture(0, GL_TEXTURE_RECTANGLE_NV, Handle);
      end
      else if GL_ARB_texture_cube_map then
      begin
        Rci.GLStates.SetGLState(StTextureCubeMap);
        Rci.GLStates.SetGLCurrentTexture(0, GL_TEXTURE_CUBE_MAP_ARB, Handle);
        // compute model view matrix for proper viewing
        GlMatrixMode(GL_TEXTURE);
        case MappingMode of
          TmmCubeMapReflection, TmmCubeMapNormal:
            begin
              M := Rci.ModelViewMatrix^;
              NormalizeMatrix(M);
              // Transposition = Matrix inversion (matrix is now orthonormal)
              if GL_ARB_transpose_matrix then
                GlLoadTransposeMatrixfARB(@M)
              else
              begin
                TransposeMatrix(M);
                GlLoadMatrixf(@M);
              end;
            end;
          TmmCubeMapLight0:
            begin
              with TGLScene(Rci.Scene).Lights do
                if Count > 0 then
                begin
                  M := TGLLightSource(Items[0]).AbsoluteMatrix;
                  NormalizeMatrix(M);
                  if GL_ARB_transpose_matrix then
                    GlLoadTransposeMatrixfARB(@M)
                  else
                  begin
                    TransposeMatrix(M);
                    GlLoadMatrixf(@M);
                  end;

                  M := Rci.ModelViewMatrix^;
                  NormalizeMatrix(M);
                  TransposeMatrix(M);
                  GlMultMatrixf(@M);
                end;
            end;
          TmmCubeMapCamera:
            begin
              M.Coord[0] := VectorCrossProduct(Rci.CameraUp,
                Rci.CameraDirection);
              M.Coord[1] := VectorNegate(Rci.CameraDirection);
              M.Coord[2] := Rci.CameraUp;
              M.Coord[3] := WHmgPoint;
              if GL_ARB_transpose_matrix then
                GlLoadTransposeMatrixfARB(@M)
              else
              begin
                TransposeMatrix(M);
                GlLoadMatrixf(@M);
              end;

              M := Rci.ModelViewMatrix^;
              NormalizeMatrix(M);
              TransposeMatrix(M);
              GlMultMatrixf(@M);
            end;
        end;

        GlMatrixMode(GL_MODELVIEW);
      end; // if GL_ARB_texture_cube_map
    GlTexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, CTextureMode[FTextureMode]);
    GlTexEnvfv(GL_TEXTURE_ENV, GL_TEXTURE_ENV_COLOR, FEnvColor.AsAddress);
    ApplyMappingMode;
    XglMapTexCoordToMain;
  end
  else
  begin // if disabled
    Rci.GLStates.UnSetGLState(StTexture2D);
    Rci.GLStates.UnSetGLState(StTextureRECT);
    XglMapTexCoordToMain;
  end;
end;

// UnApply
//
procedure TGLTexture.UnApply(var Rci: TRenderContextInfo);
begin
  if not Disabled then
  begin
    if StTextureCubeMap in Rci.GLStates.States then
    begin
      Rci.GLStates.UnSetGLState(StTextureCubeMap);
      GlMatrixMode(GL_TEXTURE);
      GlLoadIdentity;
      GlMatrixMode(GL_MODELVIEW);
    end
    else if StTexture2D in Rci.GLStates.States then
      Rci.GLStates.UnSetGLState(StTexture2D)
    else
      Rci.GLStates.UnSetGLState(StTextureRECT);

    UnApplyMappingMode;
  end;
end;

// ApplyAsTexture2
//
procedure TGLTexture.ApplyAsTexture2(var Rci: TRenderContextInfo;
  LibMaterial: TGLLibMaterial);
begin
  ApplyAsTextureN(2, Rci, LibMaterial);
end;

// UnApplyAsTexture2
//
procedure TGLTexture.UnApplyAsTexture2(var Rci: TRenderContextInfo;
  LibMaterial: TGLLibMaterial);
begin
  UnApplyAsTextureN(2, Rci, LibMaterial);
end;

// ApplyAsTextureN
//
procedure TGLTexture.ApplyAsTextureN(N: Integer; var Rci: TRenderContextInfo;
  LibMaterial: TGLLibMaterial);
var
  M: TMatrix;
begin
  if not Disabled then
  begin
    GlActiveTextureARB(GL_TEXTURE0_ARB + N - 1);

    if Image.NativeTextureTarget = GL_TEXTURE_2D then
    begin
      GlEnable(Image.NativeTextureTarget);
      Rci.GLStates.SetGLCurrentTexture(N - 1,
        Image.NativeTextureTarget, Handle);

      if Assigned(LibMaterial) and (not LibMaterial.FTextureMatrixIsIdentity)
      then
      begin
        GlMatrixMode(GL_TEXTURE);
        GlLoadMatrixf(PGLFloat(@LibMaterial.FTextureMatrix.Coord[0].Coord[0]));
        GlMatrixMode(GL_MODELVIEW);
      end;

    end
    else if GL_ARB_texture_cube_map then
    begin
      GlEnable(Image.NativeTextureTarget);
      Rci.GLStates.SetGLCurrentTexture(N - 1,
        Image.NativeTextureTarget, Handle);

      // compute model view matrix for proper viewing
      GlMatrixMode(GL_TEXTURE);
      M := Rci.ModelViewMatrix^;
      NormalizeMatrix(M);
      // Transposition = Matrix inversion (matrix is now orthonormal)
      if GL_ARB_transpose_matrix then
        GlLoadTransposeMatrixfARB(@M)
      else
      begin
        TransposeMatrix(M);
        GlLoadMatrixf(@M);
      end;
      GlMatrixMode(GL_MODELVIEW);
    end;

    GlTexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, CTextureMode[FTextureMode]);
    GlTexEnvfv(GL_TEXTURE_ENV, GL_TEXTURE_ENV_COLOR, FEnvColor.AsAddress);

    ApplyMappingMode;
    GlActiveTextureARB(GL_TEXTURE0_ARB);
  end;
end;

// UnApplyAsTextureN
//
procedure TGLTexture.UnApplyAsTextureN(N: Integer; var Rci: TRenderContextInfo;
  LibMaterial: TGLLibMaterial);
begin
  GlActiveTextureARB(GL_TEXTURE0_ARB + N - 1);
  UnApplyMappingMode;
  if (Image.NativeTextureTarget <> GL_TEXTURE_2D) or
    (Assigned(LibMaterial) and (not LibMaterial.FTextureMatrixIsIdentity)) then
  begin
    GlMatrixMode(GL_TEXTURE);
    GlLoadIdentity;
    GlMatrixMode(GL_MODELVIEW);
  end;
  GlDisable(Image.NativeTextureTarget);
  GlActiveTextureARB(GL_TEXTURE0_ARB);
end;

// AllocateHandle
//
function TGLTexture.AllocateHandle: TGLuint;
var
  Target: TGLUInt;
begin
  if FTextureHandle.Handle = 0 then
  begin
    FTextureHandle.AllocateHandle;
    Assert(FTextureHandle.Handle <> 0);
  end;
  // bind texture
  Target := Image.NativeTextureTarget;
  if (Target <> GL_TEXTURE_CUBE_MAP_ARB) or GL_ARB_texture_cube_map then
  begin
    GlBindTexture(Target, FTextureHandle.Handle);
    PrepareParams(Target);
  end;
  Result := FTextureHandle.Handle;
  FChanges := [];
end;

// IsHandleAllocated
//
function TGLTexture.IsHandleAllocated: Boolean;
begin
  Result := (FTextureHandle.Handle <> 0);
end;

// GetHandle
//
function TGLTexture.GetHandle: TGLuint;
var
  I, Target: TGLUInt;
  CubeMapSize: Integer;
  Cmt: TGLCubeMapTarget;
  CubeMapOk: Boolean;
  CubeMapImage: TGLCubeMapImage;
begin
  if (FTextureHandle.Handle = 0) or (FChanges <> []) then
  begin
    AllocateHandle;
    // Load images
    Target := Image.NativeTextureTarget;
    if (Target <> GL_TEXTURE_CUBE_MAP_ARB) or GL_ARB_texture_cube_map then
    begin
      if Target = GL_TEXTURE_CUBE_MAP_ARB then
      begin
        // first check if everything is coherent, otherwise, bail out
        CubeMapImage := (Image as TGLCubeMapImage);
        CubeMapSize := CubeMapImage.Picture[CmtPX].Width;
        CubeMapOk := (CubeMapSize > 0);
        if CubeMapOk then
        begin
          for Cmt := CmtPX to CmtNZ do
            with CubeMapImage.Picture[Cmt] do
            begin
              CubeMapOk := (Width = CubeMapSize) and (Height = CubeMapSize);
              if not CubeMapOk then
                Break;
            end;
        end;
        if CubeMapOk then
        begin
          for I := GL_TEXTURE_CUBE_MAP_POSITIVE_X_ARB to
            GL_TEXTURE_CUBE_MAP_NEGATIVE_Z_ARB do
            PrepareImage(I);
        end;
      end
      else
        PrepareImage(Target);
    end;
  end;
  Result := FTextureHandle.Handle;
end;

// DestroyHandles
//
procedure TGLTexture.DestroyHandles;
begin
  FTextureHandle.DestroyHandle;
  FChanges := [TcParams, TcImage];
  FRequiredMemorySize := -1;
end;

function TGLTexture.IsFloatType: Boolean;
begin
  Result := TextureFormat in [TfRGBAFloat16, TfRGBAFloat32];
end;

// OpenGLTextureFormat
//
function TGLTexture.OpenGLTextureFormat: Integer;
const
  CTextureFormatToOpenGL: array [TfRGB .. high(TGLTextureFormat)
    ] of Integer = (GL_RGB8, GL_RGBA8, GL_RGB5, GL_RGBA4, GL_ALPHA8,
    GL_LUMINANCE8, GL_LUMINANCE8_ALPHA8, GL_INTENSITY8, GL_RGB8,
    GL_RGBA_FLOAT16_ATI, GL_RGBA_FLOAT32_ATI);
  // float_type default is ATI types
  CCompressedTextureFormatToOpenGL: array [TfRGB .. high(TGLTextureFormat)
    ] of Integer = (GL_COMPRESSED_RGB_ARB, GL_COMPRESSED_RGBA_ARB,
    GL_COMPRESSED_RGB_ARB, GL_COMPRESSED_RGBA_ARB, GL_COMPRESSED_ALPHA_ARB,
    GL_COMPRESSED_LUMINANCE_ARB, GL_COMPRESSED_LUMINANCE_ALPHA_ARB,
    GL_COMPRESSED_INTENSITY_ARB, GL_COMPRESSED_RGB_ARB, 0, 0);
  // compression not supported for float_type
var
  TexForm: TGLTextureFormat;
  TexComp: TGLTextureCompression;
begin
  if TextureFormat = TfDefault then
    if VDefaultTextureFormat = TfDefault then
      TexForm := TfRGBA
    else
      TexForm := VDefaultTextureFormat
  else
    TexForm := TextureFormat;

  if GL_ARB_texture_compression then
  begin
    if Compression = TcDefault then
      if VDefaultTextureCompression = TcDefault then
        TexComp := TcNone
      else
        TexComp := VDefaultTextureCompression
    else
      TexComp := Compression;
  end
  else
    TexComp := TcNone;

  if IsFloatType then
    TexComp := TcNone; // no compression support for float_type

  if TexComp <> TcNone then
  begin
    case TexComp of
      TcStandard:
        GlHint(GL_TEXTURE_COMPRESSION_HINT_ARB, GL_DONT_CARE);
      TcHighQuality:
        GlHint(GL_TEXTURE_COMPRESSION_HINT_ARB, GL_NICEST);
      TcHighSpeed:
        GlHint(GL_TEXTURE_COMPRESSION_HINT_ARB, GL_FASTEST);
    else
      Assert(False);
    end;
    Result := CCompressedTextureFormatToOpenGL[TexForm];
  end
  else
    Result := CTextureFormatToOpenGL[TexForm];

  if IsFloatType and not GL_ATI_texture_float then
  begin // override if NV
    case TexForm of
      TfRGBAFloat16:
        Result := GL_FLOAT_RGBA16_NV;
      TfRGBAFloat32:
        Result := GL_FLOAT_RGBA32_NV;
    end;
  end;
end;

// PrepareImage
//
procedure TGLTexture.PrepareImage(Target: TGLUInt);
var
  Bitmap32: TGLBitmap32;
  TargetFormat: Integer;
begin
  Bitmap32 := Image.GetBitmap32(Target);

  if (Bitmap32 = nil) or Bitmap32.IsEmpty then
    Exit;
  // select targetFormat from texture format & compression options
  TargetFormat := OpenGLTextureFormat;
  if TextureFormat = TfNormalMap then
  begin
    Bitmap32.GrayScaleToNormalMap(NormalMapScale,
      TextureWrap in [TwBoth, TwHorizontal], TextureWrap in [TwBoth,
      TwVertical]);
  end;
  // prepare AlphaChannel
  case ImageAlpha of
    TiaDefault:
      ; // nothing to do
    TiaAlphaFromIntensity:
      Bitmap32.SetAlphaFromIntensity;
    TiaSuperBlackTransparent:
      Bitmap32.SetAlphaTransparentForColor($000000);
    TiaLuminance:
      Bitmap32.SetAlphaFromIntensity;
    TiaLuminanceSqrt:
      begin
        Bitmap32.SetAlphaFromIntensity;
        Bitmap32.SqrtAlpha;
      end;
    TiaOpaque:
      Bitmap32.SetAlphaToValue(255);
    TiaTopLeftPointColorTransparent:
      Bitmap32.SetAlphaTransparentForColor(Bitmap32.Data[0]);
    TiaInverseLuminance:
      begin
        Bitmap32.SetAlphaFromIntensity;
        Bitmap32.InvertAlpha;
      end;
    TiaInverseLuminanceSqrt:
      begin
        Bitmap32.SetAlphaFromIntensity;
        Bitmap32.SqrtAlpha;
        Bitmap32.InvertAlpha;
      end;
  else
    Assert(False);
  end;
  // apply brightness correction
  if FImageBrightness <> 1.0 then
    Bitmap32.BrightnessCorrection(FImageBrightness);
  // apply gamma correction
  if FImageGamma <> 1.0 then
    Bitmap32.GammaCorrection(FImageGamma);

  CheckOpenGLError;
  Bitmap32.RegisterAsOpenGLTexture(Target, MinFilter, TargetFormat, FTexWidth,
    FTexHeight);
  GlGetTexLevelParameteriv(Target, 0, GL_TEXTURE_COMPRESSED_IMAGE_SIZE_ARB,
    @FRequiredMemorySize);
  if GlGetError <> GL_NO_ERROR then
    FRequiredMemorySize := -1;
  ClearGLError; // ignore texture-size errors
  Image.ReleaseBitmap32;
end;

// PrepareParams
//
procedure TGLTexture.PrepareParams(Target: TGLUInt);
const
  CTextureSWrap: array [TwBoth .. TwHorizontal] of TGLEnum = (GL_REPEAT,
    GL_CLAMP_TO_EDGE, GL_CLAMP_TO_EDGE, GL_REPEAT);
  CTextureTWrap: array [TwBoth .. TwHorizontal] of TGLEnum = (GL_REPEAT,
    GL_CLAMP_TO_EDGE, GL_REPEAT, GL_CLAMP_TO_EDGE);
  { cTextureSWrapARB : array [twBoth..twHorizontal] of TGLEnum =
    ( GL_REPEAT, GL_CLAMP_TO_BORDER_ARB, GL_CLAMP_TO_BORDER_ARB, GL_REPEAT );
    cTextureTWrapARB : array [twBoth..twHorizontal] of TGLEnum =
    ( GL_REPEAT, GL_CLAMP_TO_BORDER_ARB, GL_REPEAT, GL_CLAMP_TO_BORDER_ARB ); }
  CTextureSWrapOld: array [TwBoth .. TwHorizontal] of TGLEnum = (GL_REPEAT,
    GL_CLAMP, GL_CLAMP, GL_REPEAT);
  CTextureTWrapOld: array [TwBoth .. TwHorizontal] of TGLEnum = (GL_REPEAT,
    GL_CLAMP, GL_REPEAT, GL_CLAMP);
  CTextureMagFilter: array [MaNearest .. MaLinear] of TGLEnum = (GL_NEAREST,
    GL_LINEAR);
  CTextureMinFilter: array [MiNearest .. MiLinearMipmapLinear] of TGLEnum =
    (GL_NEAREST, GL_LINEAR, GL_NEAREST_MIPMAP_NEAREST, GL_LINEAR_MIPMAP_NEAREST,
    GL_NEAREST_MIPMAP_LINEAR, GL_LINEAR_MIPMAP_LINEAR);
  CFilteringQuality: array [TfIsotropic .. TfAnisotropic] of Integer = (1, 2);
begin
  GlHint(GL_PERSPECTIVE_CORRECTION_HINT, GL_NICEST);
  GlPixelStorei(GL_UNPACK_ALIGNMENT, 4);
  GlPixelStorei(GL_UNPACK_ROW_LENGTH, 0);
  GlPixelStorei(GL_UNPACK_SKIP_ROWS, 0);
  GlPixelStorei(GL_UNPACK_SKIP_PIXELS, 0);

  { if GL_ARB_texture_border_clamp then begin
    glTexParameteri(target, GL_TEXTURE_WRAP_S, cTextureSWrapARB[FTextureWrap]);
    glTexParameteri(target, GL_TEXTURE_WRAP_T, cTextureTWrapARB[FTextureWrap]);
    end else }

  if IsFloatType then
  begin // float_type
    // Note: HW accerl. only with GL_CLAMP_TO_EDGE for Nvidia GPUs
    GlTexParameteri(Target, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
    GlTexParameteri(Target, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
    // Linear filtering works with nv40, 16-bit float only  (via GL_ATI_texture_float)
    if GL_ATI_texture_float and (TextureFormat = TfRGBAFloat16) then
    begin
      GlTexParameteri(Target, GL_TEXTURE_MIN_FILTER,
        CTextureMinFilter[FMinFilter]);
      GlTexParameteri(Target, GL_TEXTURE_MAG_FILTER,
        CTextureMagFilter[FMagFilter]);
    end
    else
    begin
      GlTexParameteri(Target, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
      GlTexParameteri(Target, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
    end;
  end
  else
  begin
    if GL_VERSION_1_2 or GL_EXT_texture_edge_clamp then
    begin
      GlTexParameteri(Target, GL_TEXTURE_WRAP_S, CTextureSWrap[FTextureWrap]);
      GlTexParameteri(Target, GL_TEXTURE_WRAP_T, CTextureTWrap[FTextureWrap]);
    end
    else
    begin
      GlTexParameteri(Target, GL_TEXTURE_WRAP_S,
        CTextureSWrapOld[FTextureWrap]);
      GlTexParameteri(Target, GL_TEXTURE_WRAP_T,
        CTextureTWrapOld[FTextureWrap]);
    end;

    GlTexParameteri(Target, GL_TEXTURE_MIN_FILTER,
      CTextureMinFilter[FMinFilter]);
    GlTexParameteri(Target, GL_TEXTURE_MAG_FILTER,
      CTextureMagFilter[FMagFilter]);

    if GL_EXT_texture_filter_anisotropic then
      GlTexParameteri(Target, GL_TEXTURE_MAX_ANISOTROPY_EXT,
        CFilteringQuality[FFilteringQuality]);
  end;
end;

// DoOnTextureNeeded
//
procedure TGLTexture.DoOnTextureNeeded(Sender: TObject;
  var TextureFileName: String);
begin
  if Assigned(FOnTextureNeeded) then
    FOnTextureNeeded(Sender, TextureFileName);
end;


// ---------------
// --------------- TGLTextureExItem ---------------
// ---------------

// Create
//
constructor TGLTextureExItem.Create(ACollection: TCollection);
begin
  inherited;

  FTexture := TGLTexture.Create(Self);
  FTextureOffset := TGLCoordinates.CreateInitialized(Self,
    NullHMGVector, CsPoint);
  FTextureOffset.OnNotifyChange := OnNotifyChange;
  FTextureScale := TGLCoordinates.CreateInitialized(Self, XYZHmgVector,
    CsPoint);
  FTextureScale.OnNotifyChange := OnNotifyChange;

  FTextureIndex := ID;
  FTextureMatrix := IdentityHMGMatrix;
end;

// Destroy
//
destructor TGLTextureExItem.Destroy;
begin
  FTexture.Free;
  FTextureOffset.Free;
  FTextureScale.Free;

  inherited;
end;

// Assign
//
procedure TGLTextureExItem.Assign(Source: TPersistent);
begin
  if Source is TGLTextureExItem then
  begin
    Texture := TGLTextureExItem(Source).Texture;
    TextureIndex := TGLTextureExItem(Source).TextureIndex;
    TextureOffset := TGLTextureExItem(Source).TextureOffset;
    TextureScale := TGLTextureExItem(Source).TextureScale;
    NotifyChange(Self);
  end
  else
    inherited;
end;

// NotifyChange
//
procedure TGLTextureExItem.NotifyChange(Sender: TObject);
begin
  if Assigned(Collection) then
    TGLTextureEx(Collection).NotifyChange(Sender);
end;

// Apply
//
procedure TGLTextureExItem.Apply(var Rci: TRenderContextInfo);
begin
  FApplied := False;
  if FTexture.Enabled then
  begin
    GlActiveTextureARB(GL_TEXTURE0_ARB + FTextureIndex);
    GlMatrixMode(GL_TEXTURE);
    GlPushMatrix;
    if FTextureMatrixIsIdentity then
      GlLoadIdentity
    else
      GlLoadMatrixf(@FTextureMatrix.Coord[0].Coord[0]);
    GlMatrixMode(GL_MODELVIEW);
    GlActiveTextureARB(GL_TEXTURE0_ARB);
    if FTextureIndex = 0 then
      FTexture.Apply(Rci)
    else if FTextureIndex = 1 then
      FTexture.ApplyAsTexture2(Rci, nil)
    else if FTextureIndex >= 2 then
      FTexture.ApplyAsTextureN(FTextureIndex + 1, Rci, nil);
    FApplied := True;
  end;
end;

// UnApply
//
procedure TGLTextureExItem.UnApply(var Rci: TRenderContextInfo);
begin
  if FApplied then
  begin
    if FTextureIndex = 0 then
      FTexture.UnApply(Rci)
    else if FTextureIndex = 1 then
      FTexture.UnApplyAsTexture2(Rci, nil)
    else if FTextureIndex >= 2 then
      FTexture.UnApplyAsTextureN(FTextureIndex + 1, Rci, nil);
    GlActiveTextureARB(GL_TEXTURE0_ARB + FTextureIndex);
    GlMatrixMode(GL_TEXTURE);
    GlPopMatrix;
    GlMatrixMode(GL_MODELVIEW);
    GlActiveTextureARB(GL_TEXTURE0_ARB);
    FApplied := False;
  end;
end;

// GetDisplayName
//
function TGLTextureExItem.GetDisplayName: String;
begin
  Result := Format('Tex [%d]', [FTextureIndex]);
end;

// GetOwner
//
function TGLTextureExItem.GetOwner: TPersistent;
begin
  Result := Collection;
end;

// NotifyTexMapChange
//
procedure TGLTextureExItem.NotifyTexMapChange(Sender: TObject);
begin
  if Assigned(TGLTextureEx(Collection).FMaterial) then
    TGLTextureEx(Collection).FMaterial.NotifyTexMapChange(Sender);
end;

// SetTexture
//
procedure TGLTextureExItem.SetTexture(const Value: TGLTexture);
begin
  FTexture.Assign(Value);
  NotifyChange(Self);
end;

// SetTextureIndex
//
procedure TGLTextureExItem.SetTextureIndex(const Value: Integer);
var
  Temp: Integer;
begin
  Temp := Value;
  if Temp < 0 then
    Temp := 0;
  if Temp <> FTextureIndex then
  begin
    FTextureIndex := Temp;
    NotifyChange(Self);
  end;
end;

// SetTextureOffset
//
procedure TGLTextureExItem.SetTextureOffset(const Value: TGLCoordinates);
begin
  FTextureOffset.Assign(Value);
  NotifyChange(Self);
end;

// SetTextureScale
//
procedure TGLTextureExItem.SetTextureScale(const Value: TGLCoordinates);
begin
  FTextureScale.Assign(Value);
  NotifyChange(Self);
end;

// CalculateTextureMatrix
//
procedure TGLTextureExItem.CalculateTextureMatrix;
begin
  if TextureOffset.Equals(NullHmgVector) and TextureScale.Equals(XYZHmgVector)
  then
    FTextureMatrixIsIdentity := True
  else
  begin
    FTextureMatrixIsIdentity := False;
    FTextureMatrix := CreateScaleAndTranslationMatrix(TextureScale.AsVector,
      TextureOffset.AsVector);
  end;
  NotifyChange(Self);
end;

// OnNotifyChange
//
procedure TGLTextureExItem.OnNotifyChange(Sender: TObject);
begin
  CalculateTextureMatrix;
end;


// ---------------
// --------------- TGLTextureEx ---------------
// ---------------

// Create
//
constructor TGLTextureEx.Create(AOwner: TGLMaterial);
begin
  inherited Create(TGLTextureExItem);

  FMaterial := AOwner;
end;

// NotifyChange
//
procedure TGLTextureEx.NotifyChange(Sender: TObject);
begin
  if Assigned(FMaterial) then
    FMaterial.NotifyChange(Sender);
end;

// Apply
//
procedure TGLTextureEx.Apply(var Rci: TRenderContextInfo);
var
  I, TexUnits: Integer;
  Units: Cardinal;
begin
  if not GL_ARB_multitexture then
    Exit;

  Units := 0;
  GlGetIntegerv(GL_MAX_TEXTURE_UNITS_ARB, @TexUnits);
  for I := 0 to Count - 1 do
  begin
    if Items[I].TextureIndex < TexUnits then
    begin
      Items[I].Apply(Rci);
      if Items[I].FApplied then
        if (Items[I].TextureIndex > 0) and
          (Items[I].Texture.MappingMode = TmmUser) then
          Units := Units or (1 shl Items[I].TextureIndex);
    end;
  end;
  if Units > 0 then
    XglMapTexCoordToArbitraryAdd(Units);
end;

// UnApply
//
procedure TGLTextureEx.UnApply(var Rci: TRenderContextInfo);
var
  I: Integer;
begin
  if not GL_ARB_multitexture then
    Exit;
  for I := 0 to Count - 1 do
    Items[I].UnApply(Rci);
end;

// Add
//
function TGLTextureEx.Add: TGLTextureExItem;
begin
  Result := TGLTextureExItem(inherited Add);
end;

// Loaded
//
procedure TGLTextureEx.Loaded;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    Items[I].CalculateTextureMatrix;
end;

// GetOwner
//
function TGLTextureEx.GetOwner: TPersistent;
begin
  Result := FMaterial;
end;

// SetItems
//
procedure TGLTextureEx.SetItems(Index: Integer; const Value: TGLTextureExItem);
begin
  inherited SetItem(index, Value);
end;

// GetItems
//
function TGLTextureEx.GetItems(Index: Integer): TGLTextureExItem;
begin
  Result := TGLTextureExItem(inherited GetItem(index));
end;

// IsTextureEnabled
//
function TGLTextureEx.IsTextureEnabled(Index: Integer): Boolean;
var
  I: Integer;
begin
  Result := False;
  if Self = nil then
    Exit;
  for I := 0 to Count - 1 do
    if Items[I].TextureIndex = Index then
      Result := Result or Items[I].Texture.Enabled;
end;


// ----------------- TGLMaterial --------------------------------------------------

// Create
//
constructor TGLMaterial.Create(AOwner: TPersistent);
begin
  inherited;
  FFrontProperties := TGLFaceProperties.Create(Self);
  FTexture := nil; // AutoCreate
  FFaceCulling := FcBufferDefault;
end;

// Destroy
//
destructor TGLMaterial.Destroy;
begin
  if Assigned(CurrentLibMaterial) then
    CurrentLibMaterial.UnregisterUser(Self);
  FGLBackProperties.Free;
  FFrontProperties.Free;
  FTexture.Free;
  FTextureEx.Free;
  inherited Destroy;
end;

// GetMaterialLibrary
//
function TGLMaterial.GetMaterialLibrary: TGLMaterialLibrary;
begin
  Result := FMaterialLibrary;
end;

// QueryInterface
//
function TGLMaterial.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  if GetInterface(IID, Obj) then
    Result := S_OK
  else
    Result := E_NOINTERFACE;
end;

// _AddRef
//
function TGLMaterial._AddRef: Integer;
begin
  Result := -1; // ignore
end;

// _Release
//
function TGLMaterial._Release: Integer;
begin
  Result := -1; // ignore
end;

// SetBackProperties
//
procedure TGLMaterial.SetBackProperties(Values: TGLFaceProperties);
begin
  BackProperties.Assign(Values);
  NotifyChange(Self);
end;

// GetBackProperties
//
function TGLMaterial.GetBackProperties: TGLFaceProperties;
begin
  if not Assigned(FGLBackProperties) then
    FGLBackProperties := TGLFaceProperties.Create(Self);
  Result := FGLBackProperties;
end;

// SetFrontProperties
//
procedure TGLMaterial.SetFrontProperties(Values: TGLFaceProperties);
begin
  FFrontProperties.Assign(Values);
  NotifyChange(Self);
end;

// SetBlendingMode
//
procedure TGLMaterial.SetBlendingMode(const Val: TBlendingMode);
begin
  if Val <> FBlendingMode then
  begin
    FBlendingMode := Val;
    NotifyChange(Self);
  end;
end;

// SetMaterialOptions
//
procedure TGLMaterial.SetMaterialOptions(const Val: TMaterialOptions);
begin
  if Val <> FMaterialOptions then
  begin
    FMaterialOptions := Val;
    NotifyChange(Self);
  end;
end;

// GetTexture
//
function TGLMaterial.GetTexture: TGLTexture;
begin
  if not Assigned(FTexture) then
    FTexture := TGLTexture.Create(Self);
  Result := FTexture;
end;

// SetTexture
//
procedure TGLMaterial.SetTexture(ATexture: TGLTexture);
begin
  if Assigned(ATexture) then
    Texture.Assign(ATexture)
  else
    FreeAndNil(FTexture);
end;

// SetFaceCulling
//
procedure TGLMaterial.SetFaceCulling(const Val: TFaceCulling);
begin
  if Val <> FFaceCulling then
  begin
    FFaceCulling := Val;
    NotifyChange(Self);
  end;
end;

// SetMaterialLibrary
//
procedure TGLMaterial.SetMaterialLibrary(const Val: TGLMaterialLibrary);
begin
  FMaterialLibrary := Val;
  SetLibMaterialName(LibMaterialName);
end;

// SetLibMaterialName
//
procedure TGLMaterial.SetLibMaterialName(const Val: TGLLibMaterialName);

  function MaterialLoopFrom(CurMat: TGLLibMaterial): Boolean;
  var
    LoopCount: Integer;
  begin
    LoopCount := 0;
    while Assigned(CurMat) and (LoopCount < 16) do
    begin
      with CurMat.Material do
      begin
        if MaterialLibrary <> nil then
          CurMat := MaterialLibrary.Materials.GetLibMaterialByName
            (LibMaterialName)
        else
          CurMat := nil;
      end;
      Inc(LoopCount)
    end;
    Result := (LoopCount >= 16);
  end;

var
  NewLibMaterial: TGLLibMaterial;
begin
  // locate new libmaterial
  if Assigned(FMaterialLibrary) then
    NewLibMaterial := MaterialLibrary.Materials.GetLibMaterialByName(Val)
  else
    NewLibMaterial := nil;
  // make sure new won't trigger an infinite loop
  Assert(not MaterialLoopFrom(NewLibMaterial),
    'Error: Cyclic material reference detected!');
  FLibMaterialName := Val;
  // unregister if required
  if NewLibMaterial <> CurrentLibMaterial then
  begin
    // unregister from old
    if Assigned(CurrentLibMaterial) then
      CurrentLibMaterial.UnregisterUser(Self);
    CurrentLibMaterial := NewLibMaterial;
    // register with new
    if Assigned(CurrentLibMaterial) then
      CurrentLibMaterial.RegisterUser(Self);
    NotifyTexMapChange(Self);
  end;
end;

// GetTextureEx
//
function TGLMaterial.GetTextureEx: TGLTextureEx;
begin
  if not Assigned(FTextureEx) then
    FTextureEx := TGLTextureEx.Create(Self);
  Result := FTextureEx;
end;

// SetTextureEx
//
procedure TGLMaterial.SetTextureEx(const Value: TGLTextureEx);
begin
  if Assigned(Value) or Assigned(FTextureEx) then
    TextureEx.Assign(Value);
end;

// StoreTextureEx
//
function TGLMaterial.StoreTextureEx: Boolean;
begin
  Result := (Assigned(FTextureEx) and (TextureEx.Count > 0));
end;

// NotifyLibMaterialDestruction
//
procedure TGLMaterial.NotifyLibMaterialDestruction;
begin
  FMaterialLibrary := nil;
  FLibMaterialName := '';
  CurrentLibMaterial := nil;
end;

// Loaded
//
procedure TGLMaterial.Loaded;
begin
  inherited;
  if Assigned(FTextureEx) then
    TextureEx.Loaded;
end;

// StoreMaterialProps
//
function TGLMaterial.StoreMaterialProps: Boolean;
begin
  Result := not Assigned(CurrentLibMaterial);
end;

// PrepareBuildList
//
procedure TGLMaterial.PrepareBuildList;
begin
  if Assigned(FTexture) and (not FTexture.Disabled) then
    FTexture.PrepareBuildList;
end;

// Apply
//
procedure TGLMaterial.Apply(var Rci: TRenderContextInfo);
begin
  if Assigned(CurrentLibMaterial) then
    CurrentLibMaterial.Apply(Rci)
  else
  begin
    // Lighting switch
    if MoNoLighting in MaterialOptions then
    begin
      if StLighting in Rci.GLStates.States then
      begin
        Rci.GLStates.UnSetGLState(StLighting);
        Inc(Rci.LightingDisabledCounter);
      end;
    end;
    if StLighting in Rci.GLStates.States then
      FFrontProperties.Apply(Rci, GL_FRONT)
    else
      FFrontProperties.ApplyNoLighting(Rci, GL_FRONT);
    // Apply FaceCulling and BackProperties (if needs be)
    if (StCullFace in Rci.GLStates.States) then
    begin
      // currently culling
      case FFaceCulling of
        FcBufferDefault:
          if not Rci.BufferFaceCull then
          begin
            Rci.GLStates.UnSetGLState(StCullFace);
            BackProperties.Apply(Rci, GL_BACK);
          end;
        FcCull:
          ; // nothing to do
        FcNoCull:
          begin
            Rci.GLStates.UnSetGLState(StCullFace);
            BackProperties.Apply(Rci, GL_BACK);
          end;
      else
        Assert(False);
      end;
    end
    else
    begin
      // currently NOT culling
      case FFaceCulling of
        FcBufferDefault:
          begin
            if Rci.BufferFaceCull then
              Rci.GLStates.SetGLState(StCullFace)
            else
              BackProperties.Apply(Rci, GL_BACK);
          end;
        FcCull:
          Rci.GLStates.SetGLState(StCullFace);
        FcNoCull:
          BackProperties.Apply(Rci, GL_BACK);
      else
        Assert(False);
      end;
    end;
    // Apply Blending mode
    if not Rci.IgnoreBlendingRequests then
      case FBlendingMode of
        BmOpaque:
          begin
            Rci.GLStates.UnSetGLState(StBlend);
            Rci.GLStates.UnSetGLState(StAlphaTest);
          end;
        BmTransparency:
          begin
            Rci.GLStates.SetGLState(StBlend);
            Rci.GLStates.SetGLState(StAlphaTest);
            GlBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
          end;
        BmAdditive:
          begin
            Rci.GLStates.SetGLState(StBlend);
            Rci.GLStates.SetGLState(StAlphaTest);
            GlBlendFunc(GL_SRC_ALPHA, GL_ONE);
          end;
        BmAlphaTest50:
          begin
            Rci.GLStates.UnSetGLState(StBlend);
            Rci.GLStates.SetGLState(StAlphaTest);
            GlAlphaFunc(GL_GEQUAL, 0.5);
          end;
        BmAlphaTest100:
          begin
            Rci.GLStates.UnSetGLState(StBlend);
            Rci.GLStates.SetGLState(StAlphaTest);
            GlAlphaFunc(GL_GEQUAL, 1.0);
          end;
        BmModulate:
          begin
            Rci.GLStates.SetGLState(StBlend);
            Rci.GLStates.SetGLState(StAlphaTest);
            GlBlendFunc(GL_DST_COLOR, GL_ZERO);
          end;
      else
        Assert(False);
      end;
    // Fog switch
    if MoIgnoreFog in MaterialOptions then
    begin
      if StFog in Rci.GLStates.States then
      begin
        Rci.GLStates.UnSetGLState(StFog);
        Inc(Rci.FogDisabledCounter);
      end;
    end;
    if not Assigned(FTextureEx) then
    begin
      if Assigned(FTexture) then
        FTexture.Apply(Rci)
    end
    else
    begin
      if Assigned(FTexture) and not FTextureEx.IsTextureEnabled(0) then
        FTexture.Apply(Rci)
      else if FTextureEx.Count > 0 then
        FTextureEx.Apply(Rci);
    end;
  end;
end;

// UnApply
//
function TGLMaterial.UnApply(var Rci: TRenderContextInfo): Boolean;
begin
  if Assigned(CurrentLibMaterial) then
    Result := CurrentLibMaterial.UnApply(Rci)
  else
  begin
    if BlendingMode in [BmAlphaTest50, BmAlphaTest100] then
      GlAlphaFunc(GL_GREATER, 0);
    if MoNoLighting in MaterialOptions then
    begin
      if Rci.LightingDisabledCounter > 0 then
      begin
        Dec(Rci.LightingDisabledCounter);
        if Rci.LightingDisabledCounter = 0 then
          Rci.GLStates.SetGLState(StLighting);
      end;
    end;
    if MoIgnoreFog in MaterialOptions then
    begin
      if Rci.FogDisabledCounter > 0 then
      begin
        Dec(Rci.FogDisabledCounter);
        if Rci.FogDisabledCounter = 0 then
          Rci.GLStates.SetGLState(StFog);
      end;
    end;
    if Assigned(FTexture) and (not FTexture.Disabled) and
      (not FTextureEx.IsTextureEnabled(0)) then
      FTexture.UnApply(Rci)
    else if Assigned(FTextureEx) then
      FTextureEx.UnApply(Rci);
    Result := False;
  end;
end;

// Assign
//
procedure TGLMaterial.Assign(Source: TPersistent);
begin
  if Assigned(Source) and (Source is TGLMaterial) then
  begin
    if Assigned(TGLMaterial(Source).FGLBackProperties) then
      BackProperties.Assign(TGLMaterial(Source).BackProperties)
    else
      FreeAndNil(FGLBackProperties);
    FFrontProperties.Assign(TGLMaterial(Source).FFrontProperties);
    FBlendingMode := TGLMaterial(Source).FBlendingMode;
    FMaterialOptions := TGLMaterial(Source).FMaterialOptions;
    if Assigned(TGLMaterial(Source).FTexture) then
      Texture.Assign(TGLMaterial(Source).FTexture)
    else
      FreeAndNil(FTexture);
    FFaceCulling := TGLMaterial(Source).FFaceCulling;
    FMaterialLibrary := TGLMaterial(Source).MaterialLibrary;
    SetLibMaterialName(TGLMaterial(Source).LibMaterialName);
    TextureEx.Assign(TGLMaterial(Source).TextureEx);
    NotifyChange(Self);
  end
  else
    inherited;
end;

// NotifyChange
//
procedure TGLMaterial.NotifyChange(Sender: TObject);
begin
  if Assigned(Owner) then
    if Owner is TGLBaseSceneObject then
      TGLBaseSceneObject(Owner).NotifyChange(Self)
    else if Owner is TGLLibMaterial then
      TGLLibMaterial(Owner).NotifyUsers;
end;

// NotifyTexMapChange
//
procedure TGLMaterial.NotifyTexMapChange(Sender: TObject);
begin
  if Assigned(Owner) then
    if Owner is TGLBaseSceneObject then
      TGLBaseSceneObject(Owner).StructureChanged
    else if Owner is TGLLibMaterial then
      TGLLibMaterial(Owner).NotifyUsersOfTexMapChange;
end;

// DestroyHandles
//
procedure TGLMaterial.DestroyHandles;
begin
  if Assigned(FTexture) then
    FTexture.DestroyHandles;
end;

// Blended
//
function TGLMaterial.Blended: Boolean;
begin
  if Assigned(CurrentLibMaterial) then
    Result := CurrentLibMaterial.Material.Blended
  else
    Result := not(BlendingMode in [BmOpaque, BmAlphaTest50, BmAlphaTest100]);
end;

// HasSecondaryTexture
//
function TGLMaterial.HasSecondaryTexture: Boolean;
begin
  Result := Assigned(CurrentLibMaterial) and
    Assigned(CurrentLibMaterial.LibMatTexture2);
end;

// MaterialIsLinkedToLib
//
function TGLMaterial.MaterialIsLinkedToLib: Boolean;
begin
  Result := Assigned(CurrentLibMaterial);
end;

// GetActualPrimaryTexture
//
function TGLMaterial.GetActualPrimaryTexture: TGLTexture;
begin
  if Assigned(CurrentLibMaterial) then
    Result := CurrentLibMaterial.Material.Texture
  else
    Result := Texture;
end;

// GetActualPrimaryTexture
//
function TGLMaterial.GetActualPrimaryMaterial: TGLMaterial;
begin
  if Assigned(CurrentLibMaterial) then
    Result := CurrentLibMaterial.Material
  else
    Result := Self;
end;

// QuickAssignMaterial
//
function TGLMaterial.GetLibMaterial: TGLLibMaterial;
begin
  Result := CurrentLibMaterial;
end;

// QuickAssignMaterial
//
procedure TGLMaterial.QuickAssignMaterial(const MaterialLibrary
  : TGLMaterialLibrary; const Material: TGLLibMaterial);
begin
  FMaterialLibrary := MaterialLibrary;
  FLibMaterialName := Material.FName;

  if Material <> CurrentLibMaterial then
  begin
    // unregister from old
    if Assigned(CurrentLibMaterial) then
      CurrentLibMaterial.UnregisterUser(Self);
    CurrentLibMaterial := Material;
    // register with new
    if Assigned(CurrentLibMaterial) then
      CurrentLibMaterial.RegisterUser(Self);

    NotifyTexMapChange(Self);
  end;
end;

// ------------------
// ------------------ TGLLibMaterial ------------------
// ------------------

// Create
//
constructor TGLLibMaterial.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  UserList := TList.Create;
  FName := TGLLibMaterials(Collection).MakeUniqueName('LibMaterial');
  FNameHashKey := ComputeNameHashKey(FName);
  FMaterial := TGLMaterial.Create(Self);
  FMaterial.Texture.OnTextureNeeded := DoOnTextureNeeded;
  FTextureOffset := TGLCoordinates.CreateInitialized(Self,
    NullHmgVector, CsPoint);
  FTextureOffset.OnNotifyChange := OnNotifyChange;
  FTextureScale := TGLCoordinates.CreateInitialized(Self, XYZHmgVector,
    CsPoint);
  FTextureScale.OnNotifyChange := OnNotifyChange;
  FTextureMatrixIsIdentity := True;
end;

// Destroy
//
destructor TGLLibMaterial.Destroy;
var
  I: Integer;
  MatObj: TObject;
begin
  Shader := nil; // drop dependency
  Texture2Name := ''; // drop dependency
  for I := 0 to UserList.Count - 1 do
  begin
    MatObj := TObject(UserList[I]);
    if MatObj is TGLMaterial then
      TGLMaterial(MatObj).NotifyLibMaterialDestruction
    else if MatObj is TGLLibMaterial then
    begin
      TGLLibMaterial(MatObj).LibMatTexture2 := nil;
      TGLLibMaterial(MatObj).FTexture2Name := '';
    end;
  end;
  UserList.Free;
  FMaterial.Free;
  FTextureOffset.Free;
  FTextureScale.Free;
  inherited Destroy;
end;

// GetMaterialLibrary
//
function TGLLibMaterial.GetMaterialLibrary: TGLMaterialLibrary;
begin
  if Collection = nil then
    Result := nil
  else
    Result := TGLMaterialLibrary(TGLLibMaterials(Collection).Owner);
end;

// QueryInterface
//
function TGLLibMaterial.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  if GetInterface(IID, Obj) then
    Result := S_OK
  else
    Result := E_NOINTERFACE;
end;

// _AddRef
//
function TGLLibMaterial._AddRef: Integer;
begin
  Result := -1; // ignore
end;

// _Release
//
function TGLLibMaterial._Release: Integer;
begin
  Result := -1; // ignore
end;

// Assign
//
procedure TGLLibMaterial.Assign(Source: TPersistent);
begin
  if Source is TGLLibMaterial then
  begin
    FName := TGLLibMaterials(Collection)
      .MakeUniqueName(TGLLibMaterial(Source).Name);
    FNameHashKey := ComputeNameHashKey(FName);
    FMaterial.Assign(TGLLibMaterial(Source).Material);
    FTextureOffset.Assign(TGLLibMaterial(Source).TextureOffset);
    FTextureScale.Assign(TGLLibMaterial(Source).TextureScale);
    FTexture2Name := TGLLibMaterial(Source).Texture2Name;
    FShader := TGLLibMaterial(Source).Shader;
    CalculateTextureMatrix;
  end
  else
    inherited;
end;

// PrepareBuildList
//
procedure TGLLibMaterial.PrepareBuildList;
begin
  if Assigned(Self) then
    Material.PrepareBuildList;
end;

// Apply
//
procedure TGLLibMaterial.Apply(var Rci: TRenderContextInfo);
var
  Multitextured: Boolean;
begin
  XglBeginUpdate;
  if Assigned(FShader) then
  begin
    case Shader.ShaderStyle of
      SsHighLevel:
        Shader.Apply(Rci, Self);
      SsReplace:
        begin
          Shader.Apply(Rci, Self);
          Exit;
        end;
    end;
  end;
  if (Texture2Name <> '') and GL_ARB_multitexture and
    (not VSecondTextureUnitForbidden) then
  begin
    if not Assigned(LibMatTexture2) then
    begin
      LibMatTexture2 := TGLLibMaterials(Collection).GetLibMaterialByName
        (Texture2Name);
      if Assigned(LibMatTexture2) then
        LibMatTexture2.RegisterUser(Self)
      else
        FTexture2Name := '';
    end;
    Multitextured := Assigned(LibMatTexture2) and
      (not LibMatTexture2.Material.Texture.Disabled);
  end
  else
    Multitextured := False;
  if not Multitextured then
  begin
    // no multitexturing ("standard" mode)
    if not Material.Texture.Disabled then
      if not FTextureMatrixIsIdentity then
        Rci.GLStates.SetGLTextureMatrix(FTextureMatrix);
    Material.Apply(Rci);
  end
  else
  begin
    // multitexturing is ON
    if not FTextureMatrixIsIdentity then
      Rci.GLStates.SetGLTextureMatrix(FTextureMatrix);
    Material.Apply(Rci);

    LibMatTexture2.Material.Texture.ApplyAsTexture2(Rci, LibMatTexture2);
    if (not Material.Texture.Disabled) and
      (Material.Texture.MappingMode = TmmUser) then
      if LibMatTexture2.Material.Texture.MappingMode = TmmUser then
        XglMapTexCoordToDual
      else
        XglMapTexCoordToMain
    else if LibMatTexture2.Material.Texture.MappingMode = TmmUser then
      XglMapTexCoordToSecond
    else
      XglMapTexCoordToMain;

    // EG: TextureEx stuff below doesn't seem to work for libmaterials,
    // and interferes with Texture2, not sure what should be done to get
    // it operational with shaders and a 2nd material active, so I just
    // restored classic behaviour
    { if Assigned(Material.FTextureEx) and not Material.TextureEx.IsTextureEnabled(1) then begin
      libMatTexture2.Material.Texture.ApplyAsTexture2(rci, libMatTexture2);
      // calculate and apply appropriate xgl mode
      if (not Material.Texture.Disabled) and (Material.Texture.MappingMode=tmmUser) then
      xglMapTexCoordToArbitraryAdd(1);
      if libMatTexture2.Material.Texture.MappingMode=tmmUser then
      xglMapTexCoordToArbitraryAdd(2); }

    // OLD TEXCOORD MAPPING
    // if (not Material.Texture.Disabled) and (Material.Texture.MappingMode=tmmUser) then
    // if libMatTexture2.Material.Texture.MappingMode=tmmUser then
    // xglMapTexCoordToDual
    // else xglMapTexCoordToMain
    // else if libMatTexture2.Material.Texture.MappingMode=tmmUser then
    // xglMapTexCoordToSecond
    // else xglMapTexCoordToMain;
    // end;
  end;
  if Assigned(FShader) then
  begin
    case Shader.ShaderStyle of
      SsLowLevel:
        Shader.Apply(Rci, Self);
    end;
  end;
  XglEndUpdate;
end;

// UnApply
//
function TGLLibMaterial.UnApply(var Rci: TRenderContextInfo): Boolean;
begin
  Result := False;
  if Assigned(FShader) then
  begin
    case Shader.ShaderStyle of
      SsLowLevel:
        Result := Shader.UnApply(Rci);
      SsReplace:
        begin
          Result := Shader.UnApply(Rci);
          Exit;
        end;
    end;
  end;
  if not Result then
  begin
    // if multipassing, this will occur upon last pass only
    { if Assigned(Material.FTextureEx) then begin
      if not Material.TextureEx.IsTextureEnabled(1) then begin }
    if Assigned(LibMatTexture2) and GL_ARB_multitexture and
      (not VSecondTextureUnitForbidden) then
    begin
      LibMatTexture2.Material.Texture.UnApplyAsTexture2(Rci, LibMatTexture2);
      XglMapTexCoordToMain;
    end;
    { end;
      end; }
    Material.UnApply(Rci);
    if not Material.Texture.Disabled then
      if not FTextureMatrixIsIdentity then
        Rci.GLStates.ResetGLTextureMatrix;
    if Assigned(FShader) then
    begin
      case Shader.ShaderStyle of
        SsHighLevel:
          Result := Shader.UnApply(Rci);
      end;
    end;
  end;
end;

// RegisterUser
//
procedure TGLLibMaterial.RegisterUser(Obj: TGLUpdateAbleObject);
begin
  Assert(UserList.IndexOf(Obj) < 0);
  UserList.Add(Obj);
end;

// UnregisterUser
//
procedure TGLLibMaterial.UnRegisterUser(Obj: TGLUpdateAbleObject);
begin
  UserList.Remove(Obj);
end;

// RegisterUser
//
procedure TGLLibMaterial.RegisterUser(Comp: TGLUpdateAbleComponent);
begin
  Assert(UserList.IndexOf(Comp) < 0);
  UserList.Add(Comp);
end;

// UnregisterUser
//
procedure TGLLibMaterial.UnRegisterUser(Comp: TGLUpdateAbleComponent);
begin
  UserList.Remove(Comp);
end;

// RegisterUser
//
procedure TGLLibMaterial.RegisterUser(LibMaterial: TGLLibMaterial);
begin
  Assert(UserList.IndexOf(LibMaterial) < 0);
  UserList.Add(LibMaterial);
end;

// UnregisterUser
//
procedure TGLLibMaterial.UnRegisterUser(LibMaterial: TGLLibMaterial);
begin
  UserList.Remove(LibMaterial);
end;

// NotifyUsers
//
procedure TGLLibMaterial.NotifyUsers;
var
  I: Integer;
  Obj: TObject;
begin
  if Notifying then
    Exit;
  Notifying := True;
  try
    for I := 0 to UserList.Count - 1 do
    begin
      Obj := TObject(UserList[I]);
      if Obj is TGLUpdateAbleObject then
        TGLUpdateAbleObject(UserList[I]).NotifyChange(Self)
      else if Obj is TGLUpdateAbleComponent then
        TGLUpdateAbleComponent(UserList[I]).NotifyChange(Self)
      else
      begin
        Assert(Obj is TGLLibMaterial);
        TGLLibMaterial(UserList[I]).NotifyUsers;
      end;
    end;
  finally
    Notifying := False;
  end;
end;

// NotifyUsersOfTexMapChange
//
procedure TGLLibMaterial.NotifyUsersOfTexMapChange;
var
  I: Integer;
  Obj: TObject;
begin
  if Notifying then
    Exit;
  Notifying := True;
  try
    for I := 0 to UserList.Count - 1 do
    begin
      Obj := TObject(UserList[I]);
      if Obj is TGLMaterial then
        TGLMaterial(UserList[I]).NotifyTexMapChange(Self)
      else if Obj is TGLLibMaterial then
        TGLLibMaterial(UserList[I]).NotifyUsersOfTexMapChange
      else if Obj is TGLUpdateAbleObject then
        TGLUpdateAbleObject(UserList[I]).NotifyChange(Self)
      else if Obj is TGLUpdateAbleComponent then
        TGLUpdateAbleComponent(UserList[I]).NotifyChange(Self);
    end;
  finally
    Notifying := False;
  end;
end;

// IsUsed
//
function TGLLibMaterial.IsUsed: Boolean;
begin
  Result := Assigned(Self) and (Self.Userlist.Count > 0);
end;

// GetDisplayName
//
function TGLLibMaterial.GetDisplayName: String;
begin
  Result := Name;
end;

// Loaded
//
procedure TGLLibMaterial.Loaded;
begin
  CalculateTextureMatrix;
  Material.Loaded;
end;

// ComputeNameHashKey
//
class function TGLLibMaterial.ComputeNameHashKey(const Name: String): Integer;
var
  I, N: Integer;
begin
  N := Length(name);
  Result := N;
  for I := 1 to N do
    Result := (Result shl 1) + Byte(name[I]);
end;

// SetName
//
procedure TGLLibMaterial.SetName(const Val: TGLLibMaterialName);
begin
  if Val <> FName then
  begin
    if not(CsLoading in TComponent(TGLLibMaterials(Collection).GetOwner)
      .ComponentState) then
    begin
      if TGLLibMaterials(Collection).GetLibMaterialByName(Val) <> Self then
        FName := TGLLibMaterials(Collection).MakeUniqueName(Val)
      else
        FName := Val;
    end
    else
      FName := Val;
    FNameHashKey := ComputeNameHashKey(FName);
  end;
end;

// SetMaterial
//
procedure TGLLibMaterial.SetMaterial(const Val: TGLMaterial);
begin
  FMaterial.Assign(Val);
end;

// SetTextureOffset
//
procedure TGLLibMaterial.SetTextureOffset(const Val: TGLCoordinates);
begin
  FTextureOffset.AsVector := Val.AsVector;
  CalculateTextureMatrix;
end;

// SetTextureScale
//
procedure TGLLibMaterial.SetTextureScale(const Val: TGLCoordinates);
begin
  FTextureScale.AsVector := Val.AsVector;
  CalculateTextureMatrix;
end;

// SetTexture2
//
procedure TGLLibMaterial.SetTexture2Name(const Val: TGLLibMaterialName);
begin
  if Val <> Texture2Name then
  begin
    if Assigned(LibMatTexture2) then
    begin
      LibMatTexture2.UnregisterUser(Self);
      LibMatTexture2 := nil;
    end;
    FTexture2Name := Val;
    NotifyUsers;
  end;
end;

// SetShader
//
procedure TGLLibMaterial.SetShader(const Val: TGLShader);
begin
  if Val <> FShader then
  begin
    if Assigned(FShader) then
      FShader.UnRegisterUser(Self);
    FShader := Val;
    if Assigned(FShader) then
      FShader.RegisterUser(Self);
    NotifyUsers;
  end;
end;

// CalculateTextureMatrix
//
procedure TGLLibMaterial.CalculateTextureMatrix;
begin
  if TextureOffset.Equals(NullHmgVector) and TextureScale.Equals(XYZHmgVector)
  then
    FTextureMatrixIsIdentity := True
  else
  begin
    FTextureMatrixIsIdentity := False;
    FTextureMatrix := CreateScaleAndTranslationMatrix(TextureScale.AsVector,
      TextureOffset.AsVector);
  end;
  NotifyUsers;
end;

// DestroyHandles
//
procedure TGLLibMaterial.DestroyHandles;
var
  LibMat: TGLLibMaterial;
begin
  FMaterial.DestroyHandles;
  if FTexture2Name <> '' then
  begin
    LibMat := TGLLibMaterials(Collection).GetLibMaterialByName(Texture2Name);
    if Assigned(LibMat) then
      LibMat.DestroyHandles;
  end;
end;

// OnNotifyChange
//
procedure TGLLibMaterial.OnNotifyChange(Sender: TObject);
begin
  CalculateTextureMatrix;
end;

// DoOnTextureNeeded
//
procedure TGLLibMaterial.DoOnTextureNeeded(Sender: TObject;
  var TextureFileName: String);
var
  MLib: TGLMaterialLibrary;
  I: Integer;
  TryName: String;
begin
  MLib := TGLMaterialLibrary((Collection as TGLLibMaterials).GetOwner);
  if MLib is TGLMaterialLibrary then
    with MLib do
      if Assigned(FOnTextureNeeded) then
        FOnTextureNeeded(MLib, TextureFileName);
  // if a ':' is present, or if it starts with a '\', consider it as an absolute path
  if (Pos(':', TextureFileName) > 0) or (Copy(TextureFileName, 1, 1) = '\') then
    Exit;
  // ok, not an absolute path, try given paths
  with MLib do
  begin
    if FTexturePathList <> nil then
      for I := 0 to FTexturePathList.Count - 1 do
      begin
        TryName := IncludeTrailingBackslash(FTexturePathList[I]) +
          TextureFileName;
        if (Assigned(VAFIOCreateFileStream) and FileStreamExists(TryName)) or
          FileExists(TryName) then
        begin
          TextureFileName := TryName;
          Break;
        end;
      end;
  end;
end;

// ------------------
// ------------------ TGLLibMaterials ------------------
// ------------------

// Create
//
constructor TGLLibMaterials.Create(AOwner: TComponent);
begin
  inherited Create(AOwner, TGLLibMaterial);
end;

// Loaded
//
procedure TGLLibMaterials.Loaded;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    Items[I].Loaded;
end;

// SetItems
//
procedure TGLLibMaterials.SetItems(Index: Integer; const Val: TGLLibMaterial);
begin
  inherited Items[index] := Val;
end;

// GetItems
//
function TGLLibMaterials.GetItems(Index: Integer): TGLLibMaterial;
begin
  Result := TGLLibMaterial(inherited Items[index]);
end;

// DestroyHandles
//
procedure TGLLibMaterials.DestroyHandles;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    Items[I].DestroyHandles;
end;

// Owner
//
function TGLLibMaterials.Owner: TPersistent;
begin
  Result := GetOwner;
end;

// Add
//
function TGLLibMaterials.Add: TGLLibMaterial;
begin
  Result := (inherited Add) as TGLLibMaterial;
end;

// FindItemID
//
function TGLLibMaterials.FindItemID(ID: Integer): TGLLibMaterial;
begin
  Result := (inherited FindItemID(ID)) as TGLLibMaterial;
end;

// MakeUniqueName
//
function TGLLibMaterials.MakeUniqueName(const NameRoot: TGLLibMaterialName)
  : TGLLibMaterialName;
var
  I: Integer;
begin
  Result := NameRoot;
  I := 1;
  while GetLibMaterialByName(Result) <> nil do
  begin
    Result := NameRoot + IntToStr(I);
    Inc(I);
  end;
end;

// GetLibMaterialByName
//
function TGLLibMaterials.GetLibMaterialByName(const Name: TGLLibMaterialName)
  : TGLLibMaterial;
var
  I, Hk: Integer;
  Lm: TGLLibMaterial;
begin
  Hk := TGLLibMaterial.ComputeNameHashKey(name);
  for I := 0 to Count - 1 do
  begin
    Lm := TGLLibMaterial(inherited Items[I]);
    if (Lm.NameHashKey = Hk) and (Lm.Name = name) then
    begin
      Result := Lm;
      Exit;
    end;
  end;
  Result := nil;
end;

// GetTextureIndex
//
function TGLLibMaterials.GetTextureIndex(const Texture: TGLTexture): Integer;
var
  I: Integer;
begin
  if Count <> 0 then
    for I := 0 to Count - 1 do
      if GetItems(I).Material.Texture = Texture then
      begin
        Result := I;
        Exit;
      end;
  Result := -1;
end;

// GetMaterialIndex
//
function TGLLibMaterials.GetMaterialIndex(const Material: TGLMaterial): Integer;
var
  I: Integer;
begin
  if Count <> 0 then
    for I := 0 to Count - 1 do
      if GetItems(I).Material = Material then
      begin
        Result := I;
        Exit;
      end;
  Result := -1;
end;

// GetMaterialIndex
//
function TGLLibMaterials.GetNameOfTexture(const Texture: TGLTexture)
  : TGLLibMaterialName;
var
  MatIndex: Integer;
begin
  MatIndex := GetTextureIndex(Texture);
  if MatIndex <> -1 then
    Result := GetItems(MatIndex).Name
  else
    Result := '';
end;

// GetNameOfMaterial
//
function TGLLibMaterials.GetNameOfLibMaterial(const Material: TGLLibMaterial)
  : TGLLibMaterialName;
var
  MatIndex: Integer;
begin
  MatIndex := IndexOf(Material);
  if MatIndex <> -1 then
    Result := GetItems(MatIndex).Name
  else
    Result := '';
end;

// IndexOf
//
function TGLLibMaterials.IndexOf(const Item: TGLLibMaterial): Integer;
var
  I: Integer;
begin
  Result := -1;
  if Count <> 0 then
    for I := 0 to Count - 1 do
      if GetItems(I) = Item then
      begin
        Result := I;
        Exit;
      end;
end;

// PrepareBuildList
//
procedure TGLLibMaterials.PrepareBuildList;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    TGLLibMaterial(inherited Items[I]).PrepareBuildList;
end;

// SetNamesToTStrings
//
procedure TGLLibMaterials.SetNamesToTStrings(AStrings: TStrings);
var
  I: Integer;
  Lm: TGLLibMaterial;
begin
  with AStrings do
  begin
    BeginUpdate;
    Clear;
    for I := 0 to Self.Count - 1 do
    begin
      Lm := TGLLibMaterial(inherited Items[I]);
      AddObject(Lm.Name, Lm);
    end;
    EndUpdate;
  end;
end;

// DeleteUnusedMaterials
//
procedure TGLLibMaterials.DeleteUnusedMaterials;
var
  I: Integer;
  GotNone: Boolean;
begin
  BeginUpdate;
  repeat
    GotNone := True;
    for I := Count - 1 downto 0 do
    begin
      if TGLLibMaterial(inherited Items[I]).UserList.Count = 0 then
      begin
        TGLLibMaterial(inherited Items[I]).Free;
        GotNone := False;
      end;
    end;
  until GotNone;
  EndUpdate;
end;

// ------------------
// ------------------ TGLMaterialLibrary ------------------
// ------------------

// Create
//
constructor TGLMaterialLibrary.Create(AOwner: TComponent);
begin
  inherited;
  FMaterials := TGLLibMaterials.Create(Self);
end;

// Destroy
//
destructor TGLMaterialLibrary.Destroy;
begin
  Assert(FLastAppliedMaterial = nil, 'Unbalanced material application');
  FTexturePathList.Free;
  FMaterials.Free;
  FMaterials := nil;
  inherited;
end;

// DestroyHandles
//
procedure TGLMaterialLibrary.DestroyHandles;
begin
  if Assigned(FMaterials) then
    FMaterials.DestroyHandles;
end;

// Loaded
//
procedure TGLMaterialLibrary.Loaded;
begin
  FMaterials.Loaded;
  inherited;
end;

// SetMaterials
//
procedure TGLMaterialLibrary.SetMaterials(const Val: TGLLibMaterials);
begin
  FMaterials.Assign(Val);
end;

// StoreMaterials
//
function TGLMaterialLibrary.StoreMaterials: Boolean;
begin
  Result := (FMaterials.Count > 0);
end;

// SetTexturePaths
//
procedure TGLMaterialLibrary.SetTexturePaths(const Val: String);
var
  I, Lp: Integer;

  procedure AddCurrent;
  var
    Buf: String;
  begin
    Buf := Trim(Copy(Val, Lp + 1, I - Lp - 1));
    if Length(Buf) > 0 then
    begin
      // make sure '\' is the terminator
      if Buf[Length(Buf)] <> '\' then
        Buf := Buf + '\';
      FTexturePathList.Add(Buf);
    end;
  end;

begin
  FTexturePathList.Free;
  FTexturePathList := nil;
  FTexturePaths := Val;
  if Val <> '' then
  begin
    FTexturePathList := TStringList.Create;
    Lp := 0;
    for I := 1 to Length(Val) do
    begin
      if Val[I] = ';' then
      begin
        AddCurrent;
        Lp := I;
      end;
    end;
    I := Length(Val) + 1;
    AddCurrent;
  end;
end;

// WriteToFiler
//
procedure TGLMaterialLibrary.WriteToFiler(Writer: TVirtualWriter);
var
  I, J: Integer;
  LibMat: TGLLibMaterial;
  Tex: TGLTexture;
  Img: TGLTextureImage;
  Pim: TGLPersistentImage;
  Ss: TStringStream;
  Bmp: TGLBitmap;
  TexExItem: TGLTextureExItem;
begin
  with Writer do
  begin
    WriteInteger(2); // archive version 0, texture persistence only
    // archive version 1, libmat properties
    // archive version 2, Material.TextureEx properties
    WriteInteger(Materials.Count);
    for I := 0 to Materials.Count - 1 do
    begin
      // version 0
      LibMat := Materials[I];
      WriteString(LibMat.Name);
      Tex := LibMat.Material.Texture;
      Img := Tex.Image;
      Pim := TGLPersistentImage(Img);
      if Tex.Enabled and (Img is TGLPersistentImage) and
        (Pim.Picture.Graphic <> nil) then
      begin
        WriteBoolean(True);
        Ss := TStringStream.Create('');
        try
          Bmp := TGLBitmap.Create;
          try
            Bmp.Assign(Pim.Picture.Graphic);
            Bmp.SaveToStream(Ss);
          finally
            Bmp.Free;
          end;
          WriteString(Ss.DataString);
        finally
          Ss.Free;
        end;
      end
      else
        WriteBoolean(False);
      with LibMat.Material.FrontProperties do
      begin
        Write(Ambient.AsAddress^, SizeOf(Single) * 3);
        Write(Diffuse.AsAddress^, SizeOf(Single) * 4);
        Write(Emission.AsAddress^, SizeOf(Single) * 3);
        Write(Specular.AsAddress^, SizeOf(Single) * 3);
      end;

      // version 1
      with LibMat.Material.FrontProperties do
      begin
        Write(FShininess, 1);
        WriteInteger(Integer(PolygonMode));
      end;
      with LibMat.Material.BackProperties do
      begin
        Write(Ambient.AsAddress^, SizeOf(Single) * 3);
        Write(Diffuse.AsAddress^, SizeOf(Single) * 4);
        Write(Emission.AsAddress^, SizeOf(Single) * 3);
        Write(Specular.AsAddress^, SizeOf(Single) * 3);
        Write(Byte(FShininess), 1);
        WriteInteger(Integer(PolygonMode));
      end;
      WriteInteger(Integer(LibMat.Material.BlendingMode));
      WriteInteger(SizeOf(TMaterialOptions));
      Write(LibMat.Material.MaterialOptions, SizeOf(TMaterialOptions));
      Write(LibMat.TextureOffset.AsAddress^, SizeOf(Single) * 3);
      Write(LibMat.TextureScale.AsAddress^, SizeOf(Single) * 3);
      WriteString(LibMat.Texture2Name);

      // version 2
      WriteInteger(LibMat.Material.TextureEx.Count);
      for J := 0 to LibMat.Material.TextureEx.Count - 1 do
      begin
        TexExItem := LibMat.Material.TextureEx[J];
        Img := TexExItem.Texture.Image;
        Pim := TGLPersistentImage(Img);
        if TexExItem.Texture.Enabled and (Img is TGLPersistentImage) and
          (Pim.Picture.Graphic <> nil) then
        begin
          WriteBoolean(True);
          Ss := TStringStream.Create('');
          try
            Bmp := TGLBitmap.Create;
            try
              Bmp.Assign(Pim.Picture.Graphic);
              Bmp.SaveToStream(Ss);
            finally
              Bmp.Free;
            end;
            WriteString(Ss.DataString);
          finally
            Ss.Free;
          end;
        end
        else
          WriteBoolean(False);
        WriteInteger(TexExItem.TextureIndex);
        Write(TexExItem.TextureOffset.AsAddress^, SizeOf(Single) * 3);
        Write(TexExItem.TextureScale.AsAddress^, SizeOf(Single) * 3);
      end;
    end;
  end;
end;

// ReadFromFiler
//
procedure TGLMaterialLibrary.ReadFromFiler(Reader: TVirtualReader);
var
  ArchiveVersion: Integer;
  LibMat: TGLLibMaterial;
  I, N, Size, Tex, TexCount: Integer;
  Name: String;
  Ss: TStringStream;
  Bmp: TGLBitmap;
  TexExItem: TGLTextureExItem;
begin
  ArchiveVersion := Reader.ReadInteger;
  if (ArchiveVersion >= 0) and (ArchiveVersion <= 2) then
    with Reader do
    begin
      if not FDoNotClearMaterialsOnLoad then
        Materials.Clear;
      N := ReadInteger;
      for I := 0 to N - 1 do
      begin
        // version 0
        name := ReadString;
        if FDoNotClearMaterialsOnLoad then
          LibMat := LibMaterialByName(name)
        else
          LibMat := nil;
        if ReadBoolean then
        begin
          Ss := TStringStream.Create(ReadString);
          try
            Bmp := TGLBitmap.Create;
            try
              Bmp.LoadFromStream(Ss);
              if LibMat = nil then
                LibMat := AddTextureMaterial(name, Bmp)
              else
                LibMat.Material.Texture.Image.Assign(Bmp);
            finally
              Bmp.Free;
            end;
          finally
            Ss.Free;
          end;
        end
        else
        begin
          if LibMat = nil then
          begin
            LibMat := Materials.Add;
            LibMat.Name := name;
          end;
        end;
        with LibMat.Material.FrontProperties do
        begin
          Read(Ambient.AsAddress^, SizeOf(Single) * 3);
          Read(Diffuse.AsAddress^, SizeOf(Single) * 4);
          Read(Emission.AsAddress^, SizeOf(Single) * 3);
          Read(Specular.AsAddress^, SizeOf(Single) * 3);
        end;

        // version 1
        if ArchiveVersion >= 1 then
        begin
          with LibMat.Material.FrontProperties do
          begin
            Read(FShininess, 1);
            PolygonMode := TPolygonMode(ReadInteger);
          end;
          with LibMat.Material.BackProperties do
          begin
            Read(Ambient.AsAddress^, SizeOf(Single) * 3);
            Read(Diffuse.AsAddress^, SizeOf(Single) * 4);
            Read(Emission.AsAddress^, SizeOf(Single) * 3);
            Read(Specular.AsAddress^, SizeOf(Single) * 3);
            Read(FShininess, 1);
            PolygonMode := TPolygonMode(ReadInteger);
          end;
          LibMat.Material.BlendingMode := TBlendingMode(ReadInteger);
          Size := ReadInteger;
          Read(LibMat.Material.FMaterialOptions, Size);
          Read(LibMat.TextureOffset.AsAddress^, SizeOf(Single) * 3);
          Read(LibMat.TextureScale.AsAddress^, SizeOf(Single) * 3);
          LibMat.Texture2Name := ReadString;
        end;

        // version 2
        if ArchiveVersion >= 2 then
        begin
          TexCount := ReadInteger;
          for Tex := 0 to TexCount - 1 do
          begin
            TexExItem := LibMat.Material.TextureEx.Add;
            if ReadBoolean then
            begin
              Ss := TStringStream.Create(ReadString);
              Bmp := TGLBitmap.Create;
              try
                Bmp.LoadFromStream(Ss);
                TexExItem.Texture.Image.Assign(Bmp);
                TexExItem.Texture.Enabled := True;
              finally
                Bmp.Free;
                Ss.Free;
              end;
            end;
            TexExItem.TextureIndex := ReadInteger;
            Read(TexExItem.TextureOffset.AsAddress^, SizeOf(Single) * 3);
            Read(TexExItem.TextureScale.AsAddress^, SizeOf(Single) * 3);
          end;
        end;
      end;
    end
  else
    RaiseFilerException(Self.ClassType, ArchiveVersion);
end;

// SaveToStream
//
procedure TGLMaterialLibrary.SaveToStream(AStream: TStream);
var
  Wr: TBinaryWriter;
begin
  Wr := TBinaryWriter.Create(AStream);
  try
    Self.WriteToFiler(Wr);
  finally
    Wr.Free;
  end;
end;

// LoadFromStream
//
procedure TGLMaterialLibrary.LoadFromStream(AStream: TStream);
var
  Rd: TBinaryReader;
begin
  Rd := TBinaryReader.Create(AStream);
  try
    Self.ReadFromFiler(Rd);
  finally
    Rd.Free;
  end;
end;

// AddMaterialsFromStream
//
procedure TGLMaterialLibrary.AddMaterialsFromStream(AStream: TStream);
begin
  FDoNotClearMaterialsOnLoad := True;
  try
    LoadFromStream(AStream);
  finally
    FDoNotClearMaterialsOnLoad := False;
  end;
end;

// SaveToFile
//
procedure TGLMaterialLibrary.SaveToFile(const FileName: String);
var
  Fs: TStream;
begin
  Fs := CreateFileStream(FileName, FmCreate);
  try
    SaveToStream(Fs);
  finally
    Fs.Free;
  end;
end;

// LoadFromFile
//
procedure TGLMaterialLibrary.LoadFromFile(const FileName: String);
var
  Fs: TStream;
begin
  Fs := CreateFileStream(FileName, FmOpenRead + FmShareDenyNone);
  try
    LoadFromStream(Fs);
  finally
    Fs.Free;
  end;
end;

// AddMaterialsFromFile
//
procedure TGLMaterialLibrary.AddMaterialsFromFile(const FileName: String);
var
  Fs: TStream;
begin
  Fs := CreateFileStream(FileName, FmOpenRead + FmShareDenyNone);
  try
    AddMaterialsFromStream(Fs);
  finally
    Fs.Free;
  end;
end;

// AddTextureMaterial
//
function TGLMaterialLibrary.AddTextureMaterial(const MaterialName,
  FileName: String; Persistent: Boolean = True): TGLLibMaterial;
begin
  Result := Materials.Add;
  with Result do
  begin
    Name := MaterialName;
    with Material.Texture do
    begin
      MinFilter := MiLinearMipmapLinear;
      MagFilter := MaLinear;
      TextureMode := TmModulate;
      Disabled := False;
      if Persistent then
      begin
        ImageClassName := TGLPersistentImage.ClassName;
        if FileName <> '' then
          Image.LoadFromFile(FileName);
      end
      else
      begin
        ImageClassName := TGLPicFileImage.ClassName;
        TGLPicFileImage(Image).PictureFileName := FileName;
      end;
    end;
  end;
end;

// AddTextureMaterial
//
function TGLMaterialLibrary.AddTextureMaterial(const MaterialName: String;
  Graphic: TGLGraphic): TGLLibMaterial;
begin
  Result := Materials.Add;
  with Result do
  begin
    Name := MaterialName;
    with Material.Texture do
    begin
      MinFilter := MiLinearMipmapLinear;
      MagFilter := MaLinear;
      TextureMode := TmModulate;
      Disabled := False;
      Image.Assign(Graphic);
    end;
  end;
end;

// LibMaterialByName
//
function TGLMaterialLibrary.LibMaterialByName(const Name: TGLLibMaterialName)
  : TGLLibMaterial;
begin
  if Assigned(Self) then
    Result := Materials.GetLibMaterialByName(name)
  else
    Result := nil;
end;

// TextureByName
//
function TGLMaterialLibrary.TextureByName(const LibMatName: TGLLibMaterialName)
  : TGLTexture;
var
  LibMat: TGLLibMaterial;
begin
  if Self = nil then
    raise ETexture.Create(GlsMatLibNotDefined)
  else if LibMatName = '' then
    Result := nil
  else
  begin
    LibMat := LibMaterialByName(LibMatName);
    if LibMat = nil then
      raise ETexture.CreateFmt(GlsMaterialNotFoundInMatlibEx, [LibMatName])
    else
      Result := LibMat.Material.Texture;
  end;
end;

// GetNameOfTexture
//
function TGLMaterialLibrary.GetNameOfTexture(const Texture: TGLTexture)
  : TGLLibMaterialName;
begin
  if (Self = nil) or (Texture = nil) then
    Result := ''
  else
    Result := FMaterials.GetNameOfTexture(Texture);
end;

// GetNameOfMaterial
//
function TGLMaterialLibrary.GetNameOfLibMaterial(const LibMat: TGLLibMaterial)
  : TGLLibMaterialName;
begin
  if (Self = nil) or (LibMat = nil) then
    Result := ''
  else
    Result := FMaterials.GetNameOfLibMaterial(LibMat);
end;

// ApplyMaterial
//
function TGLMaterialLibrary.ApplyMaterial(const MaterialName: String;
  var Rci: TRenderContextInfo): Boolean;
begin
  FLastAppliedMaterial := Materials.GetLibMaterialByName(MaterialName);
  Result := Assigned(FLastAppliedMaterial);
  if Result then
    FLastAppliedMaterial.Apply(Rci);
end;

// UnApplyMaterial
//
function TGLMaterialLibrary.UnApplyMaterial
  (var Rci: TRenderContextInfo): Boolean;
begin
  if Assigned(FLastAppliedMaterial) then
  begin
    Result := FLastAppliedMaterial.UnApply(Rci);
    if not Result then
      FLastAppliedMaterial := nil;
  end
  else
    Result := False;
end;

// ------------------
// ------------------ TGLColorManager ------------------
// ------------------

// Find Color
//
function TGLColorManager.FindColor(const AName: String): TColorVector;
var
  I: Integer;
begin
  Result := ClrBlack;
  for I := 0 to Count - 1 do
    if CompareText(TColorEntry(Items[I]^).Name, AName) = 0 then
    begin
      SetVector(Result, TColorEntry(Items[I]^).Color);
      Break;
    end;
end;

// GetColor
//
function TGLColorManager.GetColor(const AName: String): TColorVector;
var
  WorkCopy: String;
  Delimiter: Integer;
begin
  if AName = '' then
    Result := ClrBlack
  else
  begin
    WorkCopy := Trim(AName);
    if AName[1] in ['(', '[', '<'] then
      WorkCopy := Copy(WorkCopy, 2, Length(AName) - 2);
    if CompareText(Copy(WorkCopy, 1, 3), 'clr') = 0 then
      SetVector(Result, FindColor(WorkCopy))
    else
      try
        // initialize result
        Result := ClrBlack;
        WorkCopy := Trim(WorkCopy);
        Delimiter := Pos(' ', WorkCopy);
        if (Length(WorkCopy) > 0) and (Delimiter > 0) then
        begin
          Result.Coord[0] := StrToFloat(Copy(WorkCopy, 1, Delimiter - 1));
          System.Delete(WorkCopy, 1, Delimiter);
          WorkCopy := TrimLeft(WorkCopy);
          Delimiter := Pos(' ', WorkCopy);
          if (Length(WorkCopy) > 0) and (Delimiter > 0) then
          begin
            Result.Coord[1] := StrToFloat(Copy(WorkCopy, 1, Delimiter - 1));
            System.Delete(WorkCopy, 1, Delimiter);
            WorkCopy := TrimLeft(WorkCopy);
            Delimiter := Pos(' ', WorkCopy);
            if (Length(WorkCopy) > 0) and (Delimiter > 0) then
            begin
              Result.Coord[2] := StrToFloat(Copy(WorkCopy, 1, Delimiter - 1));
              System.Delete(WorkCopy, 1, Delimiter);
              WorkCopy := TrimLeft(WorkCopy);
              Result.Coord[3] := StrToFloat(WorkCopy);
            end
            else
              Result.Coord[2] := StrToFloat(WorkCopy);
          end
          else
            Result.Coord[1] := StrToFloat(WorkCopy);
        end
        else
          Result.Coord[0] := StrToFloat(WorkCopy);
      except
        InformationDlg('Wrong vector format. Use: ''<red green blue alpha>''!');
        Abort;
      end;
  end;
end;

// ------------------------------------------------------------------------------

function TGLColorManager.GetColorName(const AColor: TColorVector): String;

const
  MinDiff = 1E-6;

var
  I: Integer;

begin
  for I := 0 to Count - 1 do
    with TColorEntry(Items[I]^) do
      if (Abs(Color.Coord[0] - AColor.Coord[0]) < MinDiff) and
        (Abs(Color.Coord[1] - AColor.Coord[1]) < MinDiff) and
        (Abs(Color.Coord[2] - AColor.Coord[2]) < MinDiff) and
        (Abs(Color.Coord[3] - AColor.Coord[3]) < MinDiff) then
        Break;
  if I < Count then
    Result := TColorEntry(Items[I]^).Name
  else
    Result := Format('<%.3f %.3f %.3f %.3f>', [AColor.Coord[0], AColor.Coord[1],
      AColor.Coord[2], AColor.Coord[3]]);
end;

// Destroy
//
destructor TGLColorManager.Destroy;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    FreeMem(Items[I], SizeOf(TColorEntry));
  inherited Destroy;
end;

// AddColor
//
procedure TGLColorManager.AddColor(const AName: String;
  const AColor: TColorVector);
var
  NewEntry: PColorEntry;
begin
  New(NewEntry);
  if NewEntry = nil then
    raise Exception.Create('Could not allocate memory for color registration!');
  with NewEntry^ do
  begin
    Name := AName;
    SetVector(Color, AColor);
  end;
  Add(NewEntry);
end;

// EnumColors
//
procedure TGLColorManager.EnumColors(Proc: TGetStrProc);
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    Proc(TColorEntry(Items[I]^).Name);
end;

// EnumColors
//
procedure TGLColorManager.EnumColors(AValues: TStrings);
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    AValues.Add(TColorEntry(Items[I]^).Name);
end;

// RegisterDefaultColors
//
procedure TGLColorManager.RegisterDefaultColors;
begin
  Capacity := 150;
  AddColor('clrTransparent', ClrTransparent);
  AddColor('clrBlack', ClrBlack);
  AddColor('clrGray05', ClrGray05);
  AddColor('clrGray10', ClrGray10);
  AddColor('clrGray15', ClrGray15);
  AddColor('clrGray20', ClrGray20);
  AddColor('clrGray25', ClrGray25);
  AddColor('clrGray30', ClrGray30);
  AddColor('clrGray35', ClrGray35);
  AddColor('clrGray40', ClrGray40);
  AddColor('clrGray45', ClrGray45);
  AddColor('clrGray50', ClrGray50);
  AddColor('clrGray55', ClrGray55);
  AddColor('clrGray60', ClrGray60);
  AddColor('clrGray65', ClrGray65);
  AddColor('clrGray70', ClrGray70);
  AddColor('clrGray75', ClrGray75);
  AddColor('clrGray80', ClrGray80);
  AddColor('clrGray85', ClrGray85);
  AddColor('clrGray90', ClrGray90);
  AddColor('clrGray95', ClrGray95);
  AddColor('clrWhite', ClrWhite);
  AddColor('clrDimGray', ClrDimGray);
  AddColor('clrGray', ClrGray);
  AddColor('clrLightGray', ClrLightGray);
  AddColor('clrAquamarine', ClrAquamarine);
  AddColor('clrBakersChoc', ClrBakersChoc);
  AddColor('clrBlueViolet', ClrBlueViolet);
  AddColor('clrBrass', ClrBrass);
  AddColor('clrBrightGold', ClrBrightGold);
  AddColor('clrBronze', ClrBronze);
  AddColor('clrBronze2', ClrBronze2);
  AddColor('clrBrown', ClrBrown);
  AddColor('clrCadetBlue', ClrCadetBlue);
  AddColor('clrCoolCopper', ClrCoolCopper);
  AddColor('clrCopper', ClrCopper);
  AddColor('clrCoral', ClrCoral);
  AddColor('clrCornflowerBlue', ClrCornflowerBlue);
  AddColor('clrDarkBrown', ClrDarkBrown);
  AddColor('clrDarkGreen', ClrDarkGreen);
  AddColor('clrDarkOliveGreen', ClrDarkOliveGreen);
  AddColor('clrDarkOrchid', ClrDarkOrchid);
  AddColor('clrDarkPurple', ClrDarkPurple);
  AddColor('clrDarkSlateBlue', ClrDarkSlateBlue);
  AddColor('clrDarkSlateGray', ClrDarkSlateGray);
  AddColor('clrDarkSlateGrey', ClrDarkSlateGrey);
  AddColor('clrDarkTan', ClrDarkTan);
  AddColor('clrDarkTurquoise', ClrDarkTurquoise);
  AddColor('clrDarkWood', ClrDarkWood);
  AddColor('clrDkGreenCopper', ClrDkGreenCopper);
  AddColor('clrDustyRose', ClrDustyRose);
  AddColor('clrFeldspar', ClrFeldspar);
  AddColor('clrFirebrick', ClrFirebrick);
  AddColor('clrFlesh', ClrFlesh);
  AddColor('clrForestGreen', ClrForestGreen);
  AddColor('clrGold', ClrGold);
  AddColor('clrGoldenrod', ClrGoldenrod);
  AddColor('clrGreenCopper', ClrGreenCopper);
  AddColor('clrGreenYellow', ClrGreenYellow);
  AddColor('clrHuntersGreen', ClrHuntersGreen);
  AddColor('clrIndian', ClrIndian);
  AddColor('clrKhaki', ClrKhaki);
  AddColor('clrLightBlue', ClrLightBlue);
  AddColor('clrLightPurple', ClrLightPurple);
  AddColor('clrLightSteelBlue', ClrLightSteelBlue);
  AddColor('clrLightWood', ClrLightWood);
  AddColor('clrLimeGreen', ClrLimeGreen);
  AddColor('clrMandarinOrange', ClrMandarinOrange);
  AddColor('clrMaroon', ClrMaroon);
  AddColor('clrMediumAquamarine', ClrMediumAquamarine);
  AddColor('clrMediumBlue', ClrMediumBlue);
  AddColor('clrMediumForestGreen', ClrMediumForestGreen);
  AddColor('clrMediumGoldenrod', ClrMediumGoldenrod);
  AddColor('clrMediumOrchid', ClrMediumOrchid);
  AddColor('clrMediumPurple', ClrMediumPurple);
  AddColor('clrMediumSeaGreen', ClrMediumSeaGreen);
  AddColor('clrMediumSlateBlue', ClrMediumSlateBlue);
  AddColor('clrMediumSpringGreen', ClrMediumSpringGreen);
  AddColor('clrMediumTurquoise', ClrMediumTurquoise);
  AddColor('clrMediumViolet', ClrMediumViolet);
  AddColor('clrMediumWood', ClrMediumWood);
  AddColor('clrMidnightBlue', ClrMidnightBlue);
  AddColor('clrNavy', ClrNavy);
  AddColor('clrNavyBlue', ClrNavyBlue);
  AddColor('clrNeonBlue', ClrNeonBlue);
  AddColor('clrNeonPink', ClrNeonPink);
  AddColor('clrNewMidnightBlue', ClrNewMidnightBlue);
  AddColor('clrNewTan', ClrNewTan);
  AddColor('clrOldGold', ClrOldGold);
  AddColor('clrOrange', ClrOrange);
  AddColor('clrOrangeRed', ClrOrangeRed);
  AddColor('clrOrchid', ClrOrchid);
  AddColor('clrPaleGreen', ClrPaleGreen);
  AddColor('clrPink', ClrPink);
  AddColor('clrPlum', ClrPlum);
  AddColor('clrQuartz', ClrQuartz);
  AddColor('clrRichBlue', ClrRichBlue);
  AddColor('clrSalmon', ClrSalmon);
  AddColor('clrScarlet', ClrScarlet);
  AddColor('clrSeaGreen', ClrSeaGreen);
  AddColor('clrSemiSweetChoc', ClrSemiSweetChoc);
  AddColor('clrSienna', ClrSienna);
  AddColor('clrSilver', ClrSilver);
  AddColor('clrSkyBlue', ClrSkyBlue);
  AddColor('clrSlateBlue', ClrSlateBlue);
  AddColor('clrSpicyPink', ClrSpicyPink);
  AddColor('clrSpringGreen', ClrSpringGreen);
  AddColor('clrSteelBlue', ClrSteelBlue);
  AddColor('clrSummerSky', ClrSummerSky);
  AddColor('clrTan', ClrTan);
  AddColor('clrThistle', ClrThistle);
  AddColor('clrTurquoise', ClrTurquoise);
  AddColor('clrViolet', ClrViolet);
  AddColor('clrVioletRed', ClrVioletRed);
  AddColor('clrVeryDarkBrown', ClrVeryDarkBrown);
  AddColor('clrVeryLightPurple', ClrVeryLightPurple);
  AddColor('clrWheat', ClrWheat);
  AddColor('clrYellowGreen', ClrYellowGreen);
  AddColor('clrGreen', ClrGreen);
  AddColor('clrOlive', ClrOlive);
  AddColor('clrPurple', ClrPurple);
  AddColor('clrTeal', ClrTeal);
  AddColor('clrRed', ClrRed);
  AddColor('clrLime', ClrLime);
  AddColor('clrYellow', ClrYellow);
  AddColor('clrBlue', ClrBlue);
  AddColor('clrFuchsia', ClrFuchsia);
  AddColor('clrAqua', ClrAqua);

  AddColor('clrScrollBar', ClrScrollBar);
  AddColor('clrBackground', ClrBackground);
  AddColor('clrActiveCaption', ClrActiveCaption);
  AddColor('clrInactiveCaption', ClrInactiveCaption);
  AddColor('clrMenu', ClrMenu);
  AddColor('clrWindow', ClrWindow);
  AddColor('clrWindowFrame', ClrWindowFrame);
  AddColor('clrMenuText', ClrMenuText);
  AddColor('clrWindowText', ClrWindowText);
  AddColor('clrCaptionText', ClrCaptionText);
  AddColor('clrActiveBorder', ClrActiveBorder);
  AddColor('clrInactiveBorder', ClrInactiveBorder);
  AddColor('clrAppWorkSpace', ClrAppWorkSpace);
  AddColor('clrHighlight', ClrHighlight);
  AddColor('clrHighlightText', ClrHighlightText);
  AddColor('clrBtnFace', ClrBtnFace);
  AddColor('clrBtnShadow', ClrBtnShadow);
  AddColor('clrGrayText', ClrGrayText);
  AddColor('clrBtnText', ClrBtnText);
  AddColor('clrInactiveCaptionText', ClrInactiveCaptionText);
  AddColor('clrBtnHighlight', ClrBtnHighlight);
  AddColor('clr3DDkShadow', Clr3DDkShadow);
  AddColor('clr3DLight', Clr3DLight);
  AddColor('clrInfoText', ClrInfoText);
  AddColor('clrInfoBk', ClrInfoBk);
end;

// RemoveColor
//
procedure TGLColorManager.RemoveColor(const AName: String);
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
  begin
    if CompareText(TColorEntry(Items[I]^).Name, AName) = 0 then
    begin
      Delete(I);
      Break;
    end;
  end;
end;

// ConvertWinColor
//
function ConvertWinColor(AColor: TColor; Alpha: Single = 1): TColorVector;
var
  WinColor: Integer;
begin
  // Delphi color to Windows color
  WinColor := GLCrossPlatform.ColorToRGB(AColor);
  // convert 0..255 range into 0..1 range
  Result.Coord[0] := (WinColor and $FF) * (1 / 255);
  Result.Coord[1] := ((WinColor shr 8) and $FF) * (1 / 255);
  Result.Coord[2] := ((WinColor shr 16) and $FF) * (1 / 255);
  Result.Coord[3] := Alpha;
end;

// ConvertColorVector
//
function ConvertColorVector(const AColor: TColorVector): TColor;
begin
  Result := RGB(Round(255 * AColor.Coord[0]), Round(255 * AColor.Coord[1]),
    Round(255 * AColor.Coord[2]));
end;

// ConvertColorVector
//
function ConvertColorVector(const AColor: TColorVector;
  Intensity: Single): TColor;
begin
  Intensity := 255 * Intensity;
  Result := RGB(Round(Intensity * AColor.Coord[0]),
    Round(Intensity * AColor.Coord[1]), Round(Intensity * AColor.Coord[2]));
end;

// ConvertRGBColor
//
function ConvertRGBColor(const AColor: array of Byte): TColorVector;
var
  N: Integer;
begin
  // convert 0..255 range into 0..1 range
  N := High(AColor);
  Result.Coord[0] := AColor[0] * (1 / 255);
  if N > 0 then
    Result.Coord[1] := AColor[1] * (1 / 255)
  else
    Result.Coord[1] := 0;
  if N > 1 then
    Result.Coord[2] := AColor[2] * (1 / 255)
  else
    Result.Coord[2] := 0;
  if N > 2 then
    Result.Coord[3] := AColor[3] * (1 / 255)
  else
    Result.Coord[3] := 1;
end;

// RegisterColor
//
procedure RegisterColor(const AName: String; const AColor: TColorVector);
begin
  ColorManager.AddColor(AName, AColor);
end;

// UnregisterColor
//
procedure UnregisterColor(const AName: String);
begin
  ColorManager.RemoveColor(AName);
end;


// ------------------
// ------------------ TGLFloatDataImage ------------------
// ------------------

// Create
//
constructor TGLFloatDataImage.Create(AOwner: TPersistent);
begin
  inherited;
  FWidth := 256;
  FHeight := 256;
end;

// Destroy
//
destructor TGLFloatDataImage.Destroy;
begin
  ReleaseBitmap32;
  inherited Destroy;
end;

// Assign
//
procedure TGLFloatDataImage.Assign(Source: TPersistent);
begin
  if Assigned(Source) then
  begin
    if (Source is TGLFloatDataImage) then
    begin
      FWidth := TGLFloatDataImage(Source).FWidth;
      FHeight := TGLFloatDataImage(Source).FHeight;
      Invalidate;
    end
    else
      inherited;
  end
  else
    inherited;
end;

// SetWidth
//
procedure TGLFloatDataImage.SetWidth(Val: Integer);
begin
  if Val <> FWidth then
  begin
    FWidth := Val;
    if FWidth < 1 then
      FWidth := 1;
    Invalidate;
  end;
end;

// GetWidth
//
function TGLFloatDataImage.GetWidth: Integer;
begin
  Result := FWidth;
end;

// SetHeight
//
procedure TGLFloatDataImage.SetHeight(Val: Integer);
begin
  if Val <> FHeight then
  begin
    FHeight := Val;
    if FHeight < 1 then
      FHeight := 1;
    Invalidate;
  end;
end;

// GetHeight
//
function TGLFloatDataImage.GetHeight: Integer;
begin
  Result := FHeight;
end;

// GetBitmap32
//
function TGLFloatDataImage.GetBitmap32(Target: TGLUInt): TGLBitmap32;
begin
  if not Assigned(FBitmap) then
  begin
    FBitmap := TGLBitmap32.Create;
    FBitmap.Blank := True;
    FBitmap.Width := FWidth;
    FBitmap.Height := FHeight;
  end;
  Result := FBitmap;
end;

// ReleaseBitmap32
//
procedure TGLFloatDataImage.ReleaseBitmap32;
begin
  if Assigned(FBitmap) then
  begin
    FBitmap.Free;
    FBitmap := nil;
  end;
end;

// SaveToFile
//
procedure TGLFloatDataImage.SaveToFile(const FileName: String);
begin
  SaveStringToFile(FileName, '[FloatDataImage]'#13#10'Width=' + IntToStr(Width)
    + #13#10'Height=' + IntToStr(Height));
end;

// LoadFromFile
//
procedure TGLFloatDataImage.LoadFromFile(const FileName: String);
var
  Sl: TStringList;
  Buf: String;
begin
  Buf := FileName;
  if Assigned(FOnTextureNeeded) then
    FOnTextureNeeded(Self, Buf);
  if FileExists(Buf) then
  begin
    Sl := TStringList.Create;
    try
      Sl.LoadFromFile(Buf);
      FWidth := StrToInt(Sl.Values['Width']);
      FHeight := StrToInt(Sl.Values['Height']);
    finally
      Sl.Free;
    end;
  end
  else
  begin
    Assert(False, Format(GlsFailedOpenFile, [FileName]));
  end;
end;

// FriendlyName
//
class function TGLFloatDataImage.FriendlyName: String;
begin
  Result := 'FloatData Image';
end;

// FriendlyDescription
//
class function TGLFloatDataImage.FriendlyDescription: String;
begin
  Result := 'Image to be dynamically generated by OpenGL';
end;

class function TGLFloatDataImage.NativeTextureTarget: TGLUInt;
{ Note: It's faster to use GL_TEXTURE_2D instead of GL_TEXTURE_RECTANGLE_NV for
  nvidia cards (tested on SM3.0) }
begin
  Result := GL_TEXTURE_2D
end;

procedure TGLTexture.GetFloatTexImage(RenderingContext: TGLContext;
  Data: Pointer);
begin
  RenderingContext.Activate;
  GlBindTexture(GL_TEXTURE_2D, FTextureHandle.Handle);
  GlGetTexImage(GL_TEXTURE_2D, 0, GL_RGBA, GL_FLOAT, Data);
  RenderingContext.Deactivate;
end;

procedure TGLTexture.SetFloatTexImage(RenderingContext: TGLContext;
  Data: Pointer);
begin
  RenderingContext.Activate;
  GlBindTexture(GL_TEXTURE_2D, FTextureHandle.Handle);
  GlTexImage2d(GL_TEXTURE_2D, 0, GL_RGBA_FLOAT16_ATI, TexWidth, TexHeight, 0,
    GL_RGBA, GL_FLOAT, Data);
  RenderingContext.Deactivate;
end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

InitWinColors;
RegisterGLTextureImageClass(TGLBlankImage);
RegisterGLTextureImageClass(TGLPersistentImage);
RegisterGLTextureImageClass(TGLPicFileImage);
RegisterGLTextureImageClass(TGLCubeMapImage);
RegisterGLTextureImageClass(TGLFloatDataImage);
RegisterClasses([TGLMaterialLibrary]);
RegisterTGraphicClassFileExtension('.bmp', TGLBitmap);

finalization

VColorManager.Free;
VGLTextureImageClasses.Free;
VGLTextureImageClasses := nil;
FreeAndNil(VTIEClass);
FreeAndNil(VTIEEditor);

end.
