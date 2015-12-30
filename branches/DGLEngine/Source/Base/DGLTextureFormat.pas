//
// This unit is part of the DGLEngine Project, http://DGLEngine.org
//
{: DGLTextureFormat<p>

     Describe OpenGL Texture Formats<p>

 <b>Historique : </b><font size=-1><ul>
      <li>22/12/15 - JD -  Imported and improved From GLScene
 </ul></font>
}
unit DGLTextureFormat;

interface

uses
  DGLResStrings,
  DGLTypes,
  dglOpenGL;

type

  // TDGLInternalFormat
  //
  TDGLInternalFormat = (
    tfDEPTH_COMPONENT16,
    tfDEPTH_COMPONENT24,
    tfDEPTH_COMPONENT32,
    tfDEPTH24_STENCIL8,
    tfDEPTH_COMPONENT32F,
    tfDEPTH32F_STENCIL8,

    tfR3_G3_B2,
	  tfRGB2,
    tfRGB4,
    tfRGB5,
    tfRGB8,
    tfRGB10,
    tfRGB12,
	  tfRGB16,
	  tfRGBA16,
    tfRGBA2,
    tfRGBA4,
    tfRGB5_A1,
    tfRGBA8,
    tfRGB10_A2,
    tfRGBA12,
    tfRGB9_E5,
    tfR11F_G11F_B10F,
    tfR16G16B16,
    tfR16G16B16A16,
//    tfBGR,
//    tfBGRA,

    tfALPHA4,
    tfALPHA8,
    tfALPHA12,
    tfALPHA16,
    tfLUMINANCE4,
    tfLUMINANCE8,
    tfLUMINANCE12,
    tfLUMINANCE16,
    tfLUMINANCE4_ALPHA4,
    tfLUMINANCE6_ALPHA2,
    tfLUMINANCE8_ALPHA8,
    tfLUMINANCE12_ALPHA4,
    tfLUMINANCE12_ALPHA12,
    tfLUMINANCE16_ALPHA16,
    tfINTENSITY4,
    tfINTENSITY8,
    tfINTENSITY12,
    tfINTENSITY16,

//  tfSRGB,
//	tfSRGB_ALPHA,
    tfSRGB8,
	  tfSRGB8_ALPHA8,
//    tfSLUMINANCE8,
//    tfSLUMINANCE8_ALPHA8,

	  tfRG8,
	  tfR8,
    tfRG16,
    tfR16,

    tfRGB8UI,
	  tfRGBA8UI,
    tfALPHA8UI,
    tfINTENSITY8UI,
    tfLUMINANCE8UI,
    tfLUMINANCE_ALPHA8UI,
	  tfRG8UI,
    tfR8UI,

    tfRGB16UI,
	  tfRGBA16UI,
    tfALPHA16UI,
    tfINTENSITY16UI,
    tfLUMINANCE16UI,
    tfLUMINANCE_ALPHA16UI,
	  tfRG16UI,
    tfR16UI,

    tfRGB32UI,
	  tfRGBA32UI,
    tfALPHA32UI,
    tfINTENSITY32UI,
    tfLUMINANCE32UI,
    tfLUMINANCE_ALPHA32UI,
    tfRG32UI,
    tfR32UI,

    tfRGB8I,
    tfRGBA8I,
    tfALPHA8I,
    tfINTENSITY8I,
    tfLUMINANCE8I,
    tfLUMINANCE_ALPHA8I,
	  tfRG8I,
    tfR8I,

  	tfRGB16I,
	  tfRGBA16I,
    tfALPHA16I,
    tfINTENSITY16I,
    tfLUMINANCE16I,
    tfLUMINANCE_ALPHA16I,
    tfRG16I,
    tfR16I,

    tfRGB32I,
    tfRGBA32I,
    tfALPHA32I,
    tfINTENSITY32I,
    tfLUMINANCE32I,
    tfLUMINANCE_ALPHA32I,
    tfRG32I,
    tfR32I,

    tfRGB16F,
    tfRGBA16F,
    tfALPHA16F,
    tfINTENSITY16F,
    tfLUMINANCE16F,
    tfLUMINANCE_ALPHA16F,
    tfRG16F,
    tfR16F,

    tfRGB32F,
    tfRGBA32F,
    tfALPHA32F,
    tfINTENSITY32F,
    tfLUMINANCE32F,
    tfLUMINANCE_ALPHA32F,
  	tfRG32F,
    tfR32F,

    tfR8_SNORM,
    tfRG8_SNORM,
    tfRGB8_SNORM,
    tfRGBA8_SNORM,
    tfR16_SNORM,
    tfRG16_SNORM,
    tfRGB16_SNORM,
    tfRGBA16_SNORM,
//  tfALPHA_SNORM,
//  tfLUMINANCE_SNORM,
//  tfLUMINANCE_ALPHA_SNORM,
//  tfINTENSITY_SNORM,
//  tfALPHA8_SNORM,
//  tfLUMINANCE8_SNORM,
//  tfLUMINANCE8_ALPHA8_SNORM,
//  tfINTENSITY8_SNORM,
//  tfALPHA16_SNORM,
//  tfLUMINANCE16_SNORM,
//  tfLUMINANCE16_ALPHA16_SNORM,
//  tfINTENSITY16_SNORM,

//   tfCOMPRESSED_RGB,
//   tfCOMPRESSED_RGBA,

//  tfCOMPRESSED_SRGB,
//  tfCOMPRESSED_SRGB_ALPHA,

    tfCOMPRESSED_RGB_S3TC_DXT1,
    tfCOMPRESSED_RGBA_S3TC_DXT1,
    tfCOMPRESSED_RGBA_S3TC_DXT3,
    tfCOMPRESSED_RGBA_S3TC_DXT5,

    tfCOMPRESSED_SRGB_S3TC_DXT1,
    tfCOMPRESSED_SRGB_ALPHA_S3TC_DXT1,
    tfCOMPRESSED_SRGB_ALPHA_S3TC_DXT3,
    tfCOMPRESSED_SRGB_ALPHA_S3TC_DXT5,

   //tfCOMPRESSED_LUMINANCE_ALPHA_3DC,

    tfCOMPRESSED_LUMINANCE_LATC1,
    tfCOMPRESSED_SIGNED_LUMINANCE_LATC1,
    tfCOMPRESSED_LUMINANCE_ALPHA_LATC2,
    tfCOMPRESSED_SIGNED_LUMINANCE_ALPHA_LATC2,

    tfCOMPRESSED_RED_RGTC1,
    tfCOMPRESSED_SIGNED_RED_RGTC1,
    tfCOMPRESSED_RG_RGTC2,
    tfCOMPRESSED_SIGNED_RG_RGTC2

//    tfCOMPRESSED_RGBA_BPTC_UNORM,
//    tfCOMPRESSED_SRGB_ALPHA_BPTC_UNORM,
//    tfCOMPRESSED_RGB_BPTC_SIGNED_FLOAT,
//    tfCOMPRESSED_RGB_BPTC_UNSIGNED_FLOAT,

// OpenGL 4.3
//  tfCOMPRESSED_RGB8_ETC2,
//  tfCOMPRESSED_SRGB8_ETC2,
//  tfCOMPRESSED_RGB8_PUNCHTHROUGH_ALPHA1_ETC2,
//  tfCOMPRESSED_SRGB8_PUNCHTHROUGH_ALPHA1_ETC2,
//  tfCOMPRESSED_RGBA8_ETC2_EAC,
//  tfCOMPRESSED_SRGB8_ALPHA8_ETC2_EAC,
//  tfCOMPRESSED_R11_EAC,
//  tfCOMPRESSED_SIGNED_R11_EAC,
//  tfCOMPRESSED_RG11_EAC,
//  tfCOMPRESSED_SIGNED_RG11_EAC,

// OpenGL 4.4
// GL_KHR_texture_compression_astc_hdr
//    tfCOMPRESSED_RGBA_ASTC_4x4_KHR,
//    tfCOMPRESSED_RGBA_ASTC_5x4_KHR,
//    tfCOMPRESSED_RGBA_ASTC_5x5_KHR,
//    tfCOMPRESSED_RGBA_ASTC_6x5_KHR,
//    tfCOMPRESSED_RGBA_ASTC_6x6_KHR,
//    tfCOMPRESSED_RGBA_ASTC_8x5_KHR,
//    tfCOMPRESSED_RGBA_ASTC_8x6_KHR,
//    tfCOMPRESSED_RGBA_ASTC_8x8_KHR,
//    tfCOMPRESSED_RGBA_ASTC_105_KHR,
//    tfCOMPRESSED_RGBA_ASTC_106_KHR,
//    tfCOMPRESSED_RGBA_ASTC_108_KHR,
//    tfCOMPRESSED_RGBA_ASTC_110_KHR,
//    tfCOMPRESSED_RGBA_ASTC_12x10_KHR,
//    tfCOMPRESSED_RGBA_ASTC_12x12_KHR,
//    tfCOMPRESSED_SRGB8_ALPHA8_ASTC_4x4_KHR,
//    tfCOMPRESSED_SRGB8_ALPHA8_ASTC_5x4_KHR,
//    tfCOMPRESSED_SRGB8_ALPHA8_ASTC_5x5_KHR,
//    tfCOMPRESSED_SRGB8_ALPHA8_ASTC_6x5_KHR,
//    tfCOMPRESSED_SRGB8_ALPHA8_ASTC_6x6_KHR,
//    tfCOMPRESSED_SRGB8_ALPHA8_ASTC_8x5_KHR,
//    tfCOMPRESSED_SRGB8_ALPHA8_ASTC_8x6_KHR,
//    tfCOMPRESSED_SRGB8_ALPHA8_ASTC_8x8_KHR,
//    tfCOMPRESSED_SRGB8_ALPHA8_ASTC_10x5_KHR,
//    tfCOMPRESSED_SRGB8_ALPHA8_ASTC_10x6_KHR,
//    tfCOMPRESSED_SRGB8_ALPHA8_ASTC_10x8_KHR,
//    tfCOMPRESSED_SRGB8_ALPHA8_ASTC_10x10_KHR,
//    tfCOMPRESSED_SRGB8_ALPHA8_ASTC_12x10_KHR,
//    tfCOMPRESSED_SRGB8_ALPHA8_ASTC_12x12_KHR
    );



  // Global texturing defaults
  //
var
  vDefaultTextureFormat: TDGLInternalFormat = tfRGBA8;
  vDefaultTextureCompression: TDGLInternalCompression = tcNone;



{: Give a openGL texture format from GLScene texture format. }
function InternalFormatToOpenGLFormat(intFormat: TDGLInternalFormat): TGLEnum;
{: Give a GLScene texture format from openGL texture format. }
function OpenGLFormatToInternalFormat(glFormat: TGLEnum): TDGLInternalFormat;
{: Give a pixel size in bytes from texture format or data format. }
function GetTextureElementSize(intFormat: TDGLInternalFormat): Integer; overload;
function GetTextureElementSize(colorFormat: TGLEnum; dataType: TGLEnum): Integer; overload;
{: Give compatible openGL image format and data type. }
procedure FindCompatibleDataFormat(intFormat: TDGLInternalFormat; out dFormat: GLenum; out dType: GLenum);
{: Give a compressed openGL texture format from GLScene texture format
  if format is have not compression than return same openGL format. }
function CompressedInternalFormatToOpenGL(intFormat: TDGLInternalFormat): Integer;
{: True if texture target supported. }
function IsTargetSupported(glTarget: TGLEnum): Boolean; overload;
function IsTargetSupported(target: TDGLTextureTarget): Boolean; overload;
{: True if texture format is supported by hardware or software. }
function IsFormatSupported(intFormat: TDGLInternalFormat): Boolean;
{: True if texture format is float. }
function IsFloatFormat(intFormat: TDGLInternalFormat): Boolean; overload;
function IsFloatFormat(glFormat: TGLEnum): Boolean; overload;
{: True if depth texture. }
function IsDepthFormat(intFormat: TDGLInternalFormat): boolean; overload;
function IsDepthFormat(glFormat: TGLEnum): Boolean; overload;
{: True if texture compressed. }
function IsCompressedFormat(intFormat: TDGLInternalFormat): Boolean; overload;
function IsCompressedFormat(glFormat: TGLEnum): Boolean; overload;
{: Give generic compressed OpenGL texture format. }
function GetGenericCompressedFormat(const intFormat: TDGLInternalFormat; const colorFormat: TGLEnum; out internalFormat: TGLEnum): Boolean;
{: Give uncompressed texture format and OpenGL color format. }
function GetUncompressedFormat(const intFormat: TDGLInternalFormat; out internalFormat: TDGLInternalFormat; out colorFormat: TGLEnum): Boolean;

function DecodeGLTextureTarget(const TextureTarget: TDGLTextureTarget): TGLEnum;
function EncodeGLTextureTarget(const glTarget: TGLEnum): TDGLTextureTarget;
function IsTargetSupportMipmap(const TextureTarget: TDGLTextureTarget): Boolean; overload;
function IsTargetSupportMipmap(const glTarget: TGLEnum): Boolean; overload;

//---------------------------------------------------------------------------
implementation
//---------------------------------------------------------------------------



type

  TFormatDesc = record
    IntFmt: TGLEnum;
    ClrFmt: TGLEnum;
    DataFmt: TGLEnum;
    RBit: Byte;
    GBit: Byte;
    BBit: Byte;
    ABit: Byte;
    LBit: Byte;
    DBit: Byte;
    Sign: Boolean;
    Flt: Boolean;
    Fix: Boolean;
    Comp: Boolean;
  end;

const
  //: InternalFormat, ColorFormat, DataType
  cTextureFormatToOpenGL: array[low(TDGLInternalFormat)..high(TDGLInternalFormat)] of TFormatDesc =
  (
(IntFmt: GL_DEPTH_COMPONENT16; ClrFmt: GL_DEPTH_COMPONENT; DataFmt: GL_UNSIGNED_BYTE; RBit: 0; GBit: 0; BBit: 0; ABit: 0; LBit: 0; DBit: 16; Sign: False; Flt: False; Fix: False; Comp: False),
(IntFmt: GL_DEPTH_COMPONENT24; ClrFmt: GL_DEPTH_COMPONENT; DataFmt: GL_UNSIGNED_BYTE; RBit: 0; GBit: 0; BBit: 0; ABit: 0; LBit: 0; DBit: 24; Sign: False; Flt: False; Fix: False; Comp: False),
(IntFmt: GL_DEPTH_COMPONENT32; ClrFmt: GL_DEPTH_COMPONENT; DataFmt: GL_UNSIGNED_BYTE; RBit: 0; GBit: 0; BBit: 0; ABit: 0; LBit: 0; DBit: 32; Sign: False; Flt: False; Fix: False; Comp: False),
(IntFmt: GL_DEPTH24_STENCIL8; ClrFmt: GL_DEPTH_STENCIL; DataFmt: GL_UNSIGNED_BYTE; RBit: 0; GBit: 0; BBit: 0; ABit: 0; LBit: 0; DBit: 24; Sign: False; Flt: False; Fix: False; Comp: False),
(IntFmt: GL_DEPTH_COMPONENT32F; ClrFmt: GL_DEPTH_COMPONENT; DataFmt: GL_FLOAT; RBit: 0; GBit: 0; BBit: 0; ABit: 0; LBit: 0; DBit: 32; Sign: False; Flt: True; Fix: False; Comp: False),
(IntFmt: GL_DEPTH32F_STENCIL8; ClrFmt: GL_DEPTH_STENCIL; DataFmt: GL_FLOAT; RBit: 0; GBit: 0; BBit: 0; ABit: 0; LBit: 0; DBit: 32; Sign: False; Flt: True; Fix: False; Comp: False),

(IntFmt: GL_R3_G3_B2; ClrFmt: GL_RGB; DataFmt: GL_UNSIGNED_BYTE_3_3_2; RBit: 3; GBit: 3; BBit: 2; ABit: 0; LBit: 0; DBit: 0; Sign: False; Flt: False; Fix: False; Comp: False),
(IntFmt: GL_RGB2_EXT; ClrFmt: GL_RGB; DataFmt: GL_UNSIGNED_BYTE; RBit: 2; GBit: 2; BBit: 2; ABit: 2; LBit: 0; DBit: 0; Sign: False; Flt: False; Fix: False; Comp: False),
(IntFmt: GL_RGB4; ClrFmt: GL_RGB; DataFmt: GL_UNSIGNED_BYTE; RBit: 4; GBit: 4; BBit: 4; ABit: 0; LBit: 0; DBit: 0; Sign: False; Flt: False; Fix: False; Comp: False),
(IntFmt: GL_RGB5; ClrFmt: GL_RGB; DataFmt: GL_UNSIGNED_SHORT_5_6_5; RBit: 5; GBit: 6; BBit: 5; ABit: 0; LBit: 0; DBit: 0; Sign: False; Flt: False; Fix: False; Comp: False),
(IntFmt: GL_RGB8; ClrFmt: GL_RGB; DataFmt: GL_UNSIGNED_BYTE; RBit: 8; GBit: 8; BBit: 8; ABit: 0; LBit: 0; DBit: 0; Sign: False; Flt: False; Fix: False; Comp: False),
(IntFmt: GL_RGB10; ClrFmt: GL_RGBA; DataFmt: GL_UNSIGNED_INT_10_10_10_2; RBit: 10; GBit: 10; BBit: 10; ABit: 0; LBit: 0; DBit: 0; Sign: False; Flt: False; Fix: False; Comp: False),
(IntFmt: GL_RGB12; ClrFmt: GL_RGB; DataFmt: GL_UNSIGNED_BYTE; RBit: 12; GBit: 12; BBit: 12; ABit: 0; LBit: 0; DBit: 0; Sign: False; Flt: False; Fix: False; Comp: False),
(IntFmt: GL_RGB16; ClrFmt: GL_RGB; DataFmt: GL_UNSIGNED_SHORT; RBit: 16; GBit: 16; BBit: 16; ABit: 0; LBit: 0; DBit: 0; Sign: False; Flt: False; Fix: False; Comp: False),
(IntFmt: GL_RGBA16; ClrFmt: GL_RGBA; DataFmt: GL_UNSIGNED_SHORT; RBit: 16; GBit: 16; BBit: 16; ABit: 16; LBit: 0; DBit: 0; Sign: False; Flt: False; Fix: False; Comp: False),
(IntFmt: GL_RGBA2; ClrFmt: GL_RGBA; DataFmt: GL_UNSIGNED_BYTE; RBit: 2; GBit: 2; BBit: 2; ABit: 2; LBit: 0; DBit: 0; Sign: False; Flt: False; Fix: False; Comp: False),
(IntFmt: GL_RGBA4; ClrFmt: GL_RGBA; DataFmt: GL_UNSIGNED_SHORT_4_4_4_4; RBit: 4; GBit: 4; BBit: 4; ABit: 4; LBit: 0; DBit: 0; Sign: False; Flt: False; Fix: False; Comp: False),
(IntFmt: GL_RGB5_A1; ClrFmt: GL_RGBA; DataFmt: GL_UNSIGNED_SHORT_5_5_5_1; RBit: 5; GBit: 5; BBit: 5; ABit: 1; LBit: 0; DBit: 0; Sign: False; Flt: False; Fix: False; Comp: False),
(IntFmt: GL_RGBA8; ClrFmt: GL_RGBA; DataFmt: GL_UNSIGNED_BYTE; RBit: 8; GBit: 8; BBit: 8; ABit: 8; LBit: 0; DBit: 0; Sign: False; Flt: False; Fix: False; Comp: False),
(IntFmt: GL_RGB10_A2; ClrFmt: GL_RGBA; DataFmt: GL_UNSIGNED_INT_10_10_10_2; RBit: 10; GBit: 10; BBit: 10; ABit: 2; LBit: 0; DBit: 0; Sign: False; Flt: False; Fix: False; Comp: False),
(IntFmt: GL_RGBA12; ClrFmt: GL_RGBA; DataFmt: GL_UNSIGNED_BYTE; RBit: 12; GBit: 12; BBit: 12; ABit: 12; LBit: 0; DBit: 0; Sign: False; Flt: False; Fix: False; Comp: False),
(IntFmt: GL_RGB9_E5; ClrFmt: GL_RGBA; DataFmt: GL_FLOAT; RBit: 8; GBit: 8; BBit: 8; ABit: 0; LBit: 0; DBit: 0; Sign: False; Flt: False; Fix: False; Comp: False),
(IntFmt: GL_R11F_G11F_B10F; ClrFmt: GL_RGB; DataFmt: GL_FLOAT; RBit: 11; GBit: 11; BBit: 10; ABit: 0; LBit: 0; DBit: 0; Sign: False; Flt: True; Fix: False; Comp: False),
(IntFmt: GL_RGB16; ClrFmt: GL_RGB; DataFmt: GL_UNSIGNED_SHORT; RBit: 16; GBit: 16; BBit: 16; ABit: 0; LBit: 0; DBit: 0; Sign: False; Flt: False; Fix: False; Comp: False),
(IntFmt: GL_RGBA16; ClrFmt: GL_RGBA; DataFmt: GL_UNSIGNED_SHORT; RBit: 16; GBit: 16; BBit: 16; ABit: 16; LBit: 0; DBit: 0; Sign: False; Flt: False; Fix: False; Comp: False),
//
//

(IntFmt: GL_ALPHA4_EXT; ClrFmt: GL_ALPHA; DataFmt: GL_UNSIGNED_BYTE; RBit: 0; GBit: 0; BBit: 0; ABit: 4; LBit: 0; DBit: 0; Sign: False; Flt: False; Fix: False; Comp: False),
(IntFmt: GL_ALPHA8_EXT; ClrFmt: GL_ALPHA; DataFmt: GL_UNSIGNED_BYTE; RBit: 0; GBit: 0; BBit: 0; ABit: 8; LBit: 0; DBit: 0; Sign: False; Flt: False; Fix: False; Comp: False),
(IntFmt: GL_ALPHA12_EXT; ClrFmt: GL_ALPHA; DataFmt: GL_UNSIGNED_SHORT; RBit: 0; GBit: 0; BBit: 0; ABit: 12; LBit: 0; DBit: 0; Sign: False; Flt: False; Fix: False; Comp: False),
(IntFmt: GL_ALPHA16_EXT; ClrFmt: GL_ALPHA; DataFmt: GL_UNSIGNED_SHORT; RBit: 0; GBit: 0; BBit: 0; ABit: 16; LBit: 0; DBit: 0; Sign: False; Flt: False; Fix: False; Comp: False),
(IntFmt: GL_LUMINANCE4_EXT; ClrFmt: GL_LUMINANCE_INTEGER_EXT; DataFmt: GL_UNSIGNED_BYTE; RBit: 0; GBit: 0; BBit: 0; ABit: 0; LBit: 4; DBit: 0; Sign: False; Flt: False; Fix: False; Comp: False),
(IntFmt: GL_LUMINANCE8_EXT; ClrFmt: GL_LUMINANCE_INTEGER_EXT; DataFmt: GL_UNSIGNED_BYTE; RBit: 0; GBit: 0; BBit: 0; ABit: 0; LBit: 8; DBit: 0; Sign: False; Flt: False; Fix: False; Comp: False),
(IntFmt: GL_LUMINANCE12_EXT; ClrFmt: GL_LUMINANCE_INTEGER_EXT; DataFmt: GL_UNSIGNED_SHORT; RBit: 0; GBit: 0; BBit: 0; ABit: 0; LBit: 12; DBit: 0; Sign: False; Flt: False; Fix: False; Comp: False),
(IntFmt: GL_LUMINANCE16_EXT; ClrFmt: GL_LUMINANCE_INTEGER_EXT; DataFmt: GL_UNSIGNED_SHORT; RBit: 0; GBit: 0; BBit: 0; ABit: 0; LBit: 16; DBit: 0; Sign: False; Flt: False; Fix: False; Comp: False),
(IntFmt: GL_LUMINANCE4_ALPHA4_EXT; ClrFmt: GL_LUMINANCE_ALPHA_INTEGER_EXT; DataFmt: GL_UNSIGNED_BYTE; RBit: 0; GBit: 0; BBit: 0; ABit: 4; LBit: 4; DBit: 0; Sign: False; Flt: False; Fix: False; Comp: False),
(IntFmt: GL_LUMINANCE6_ALPHA2_EXT; ClrFmt: GL_LUMINANCE_ALPHA_INTEGER_EXT; DataFmt: GL_UNSIGNED_BYTE; RBit: 0; GBit: 0; BBit: 0; ABit: 6; LBit: 2; DBit: 0; Sign: False; Flt: False; Fix: False; Comp: False),
(IntFmt: GL_LUMINANCE8_ALPHA8_EXT; ClrFmt: GL_LUMINANCE_ALPHA_INTEGER_EXT; DataFmt: GL_UNSIGNED_BYTE; RBit: 0; GBit: 0; BBit: 0; ABit: 8; LBit: 8; DBit: 0; Sign: False; Flt: False; Fix: False; Comp: False),
(IntFmt: GL_LUMINANCE12_ALPHA4_EXT; ClrFmt: GL_LUMINANCE_ALPHA_INTEGER_EXT; DataFmt: GL_UNSIGNED_SHORT; RBit: 0; GBit: 0; BBit: 0; ABit: 4; LBit: 12; DBit: 0; Sign: False; Flt: False; Fix: False; Comp: False),
(IntFmt: GL_LUMINANCE12_ALPHA12_EXT; ClrFmt: GL_LUMINANCE_ALPHA_INTEGER_EXT; DataFmt: GL_UNSIGNED_SHORT; RBit: 0; GBit: 0; BBit: 0; ABit: 12; LBit: 12; DBit: 0; Sign: False; Flt: False; Fix: False; Comp: False),
(IntFmt: GL_LUMINANCE16_ALPHA16_EXT; ClrFmt: GL_LUMINANCE_ALPHA_INTEGER_EXT; DataFmt: GL_UNSIGNED_SHORT; RBit: 0; GBit: 0; BBit: 0; ABit: 16; LBit: 16; DBit: 0; Sign: False; Flt: False; Fix: False; Comp: False),
(IntFmt: GL_INTENSITY4_EXT; ClrFmt: GL_LUMINANCE_INTEGER_EXT; DataFmt: GL_UNSIGNED_BYTE; RBit: 0; GBit: 0; BBit: 0; ABit: 0; LBit: 4; DBit: 0; Sign: False; Flt: False; Fix: False; Comp: False),
(IntFmt: GL_INTENSITY8_EXT; ClrFmt: GL_LUMINANCE_INTEGER_EXT; DataFmt: GL_UNSIGNED_BYTE; RBit: 0; GBit: 0; BBit: 0; ABit: 0; LBit: 8; DBit: 0; Sign: False; Flt: False; Fix: False; Comp: False),
(IntFmt: GL_INTENSITY12_EXT; ClrFmt: GL_LUMINANCE_INTEGER_EXT; DataFmt: GL_UNSIGNED_SHORT; RBit: 0; GBit: 0; BBit: 0; ABit: 0; LBit: 12; DBit: 0; Sign: False; Flt: False; Fix: False; Comp: False),
(IntFmt: GL_INTENSITY16_EXT; ClrFmt: GL_LUMINANCE_INTEGER_EXT; DataFmt: GL_UNSIGNED_SHORT; RBit: 0; GBit: 0; BBit: 0; ABit: 0; LBit: 16; DBit: 0; Sign: False; Flt: False; Fix: False; Comp: False),

//
//
(IntFmt: GL_SRGB8; ClrFmt: GL_RGB; DataFmt: GL_UNSIGNED_BYTE; RBit: 8; GBit: 8; BBit: 8; ABit: 0; LBit: 0; DBit: 0; Sign: False; Flt: False; Fix: False; Comp: False),
(IntFmt: GL_SRGB8_ALPHA8; ClrFmt: GL_RGBA; DataFmt: GL_UNSIGNED_BYTE; RBit: 8; GBit: 8; BBit: 8; ABit: 8; LBit: 0; DBit: 0; Sign: False; Flt: False; Fix: False; Comp: False),
//
//

(IntFmt: GL_RG8; ClrFmt: GL_RG; DataFmt: GL_BYTE; RBit: 8; GBit: 8; BBit: 0; ABit: 0; LBit: 0; DBit: 0; Sign: False; Flt: False; Fix: False; Comp: False),
(IntFmt: GL_R8; ClrFmt: GL_RED; DataFmt: GL_BYTE; RBit: 8; GBit: 0; BBit: 0; ABit: 0; LBit: 0; DBit: 0; Sign: False; Flt: False; Fix: False; Comp: False),
(IntFmt: GL_RG16; ClrFmt: GL_RG; DataFmt: GL_SHORT; RBit: 16; GBit: 16; BBit: 0; ABit: 0; LBit: 0; DBit: 0; Sign: False; Flt: False; Fix: False; Comp: False),
(IntFmt: GL_R16; ClrFmt: GL_RED; DataFmt: GL_SHORT; RBit: 16; GBit: 0; BBit: 0; ABit: 0; LBit: 0; DBit: 0; Sign: False; Flt: False; Fix: False; Comp: False),

(IntFmt: GL_RGB8UI; ClrFmt: GL_RGB_INTEGER; DataFmt: GL_UNSIGNED_BYTE; RBit: 8; GBit: 8; BBit: 8; ABit: 0; LBit: 0; DBit: 0; Sign: False; Flt: False; Fix: True; Comp: False),
(IntFmt: GL_RGBA8UI; ClrFmt: GL_RGBA_INTEGER; DataFmt: GL_UNSIGNED_BYTE; RBit: 8; GBit: 8; BBit: 8; ABit: 8; LBit: 0; DBit: 0; Sign: False; Flt: False; Fix: True; Comp: False),
(IntFmt: GL_ALPHA8UI_EXT; ClrFmt: GL_ALPHA_INTEGER_EXT; DataFmt: GL_UNSIGNED_BYTE; RBit: 0; GBit: 0; BBit: 0; ABit: 8; LBit: 0; DBit: 0; Sign: False; Flt: False; Fix: True; Comp: False),
(IntFmt: GL_INTENSITY8UI_EXT; ClrFmt: GL_LUMINANCE_INTEGER_EXT; DataFmt: GL_UNSIGNED_BYTE; RBit: 0; GBit: 0; BBit: 0; ABit: 0; LBit: 8; DBit: 0; Sign: False; Flt: False; Fix: True; Comp: False),
(IntFmt: GL_LUMINANCE8UI_EXT; ClrFmt: GL_LUMINANCE_INTEGER_EXT; DataFmt: GL_UNSIGNED_BYTE; RBit: 0; GBit: 0; BBit: 0; ABit: 0; LBit: 8; DBit: 0; Sign: False; Flt: False; Fix: True; Comp: False),
(IntFmt: GL_LUMINANCE_ALPHA8UI_EXT; ClrFmt: GL_LUMINANCE_ALPHA_INTEGER_EXT; DataFmt: GL_UNSIGNED_BYTE; RBit: 0; GBit: 0; BBit: 0; ABit: 8; LBit: 8; DBit: 0; Sign: False; Flt: True; Fix: False; Comp: False),
(IntFmt: GL_RG8UI; ClrFmt: GL_RG; DataFmt: GL_UNSIGNED_BYTE; RBit: 8; GBit: 8; BBit: 0; ABit: 0; LBit: 0; DBit: 0; Sign: False; Flt: False; Fix: True; Comp: False),
(IntFmt: GL_R8UI; ClrFmt: GL_RED_INTEGER; DataFmt: GL_UNSIGNED_BYTE; RBit: 8; GBit: 0; BBit: 0; ABit: 0; LBit: 0; DBit: 0; Sign: False; Flt: True; Fix: False; Comp: False),

(IntFmt: GL_RGB16UI; ClrFmt: GL_RGB_INTEGER; DataFmt: GL_UNSIGNED_SHORT; RBit: 16; GBit: 16; BBit: 16; ABit: 0; LBit: 0; DBit: 0; Sign: False; Flt: False; Fix: True; Comp: False),
(IntFmt: GL_RGBA16UI; ClrFmt: GL_RGBA_INTEGER; DataFmt: GL_UNSIGNED_SHORT; RBit: 16; GBit: 16; BBit: 16; ABit: 16; LBit: 0; DBit: 0; Sign: False; Flt: False; Fix: True; Comp: False),
(IntFmt: GL_ALPHA16UI_EXT; ClrFmt: GL_ALPHA_INTEGER_EXT; DataFmt: GL_UNSIGNED_SHORT; RBit: 0; GBit: 0; BBit: 0; ABit: 16; LBit: 0; DBit: 0; Sign: False; Flt: False; Fix: True; Comp: False),
(IntFmt: GL_INTENSITY16UI_EXT; ClrFmt: GL_LUMINANCE_INTEGER_EXT; DataFmt: GL_UNSIGNED_SHORT; RBit: 0; GBit: 0; BBit: 0; ABit: 0; LBit: 16; DBit: 0; Sign: False; Flt: False; Fix: True; Comp: False),
(IntFmt: GL_LUMINANCE16UI_EXT; ClrFmt: GL_LUMINANCE_INTEGER_EXT; DataFmt: GL_UNSIGNED_SHORT; RBit: 0; GBit: 0; BBit: 0; ABit: 0; LBit: 16; DBit: 0; Sign: False; Flt: False; Fix: True; Comp: False),
(IntFmt: GL_LUMINANCE_ALPHA16UI_EXT; ClrFmt: GL_LUMINANCE_ALPHA_INTEGER_EXT; DataFmt: GL_UNSIGNED_SHORT; RBit: 0; GBit: 0; BBit: 0; ABit: 16; LBit: 16; DBit: 0; Sign: False; Flt: False; Fix: True; Comp: False),
(IntFmt: GL_RG16UI; ClrFmt: GL_RG; DataFmt: GL_UNSIGNED_SHORT; RBit: 16; GBit: 16; BBit: 0; ABit: 0; LBit: 0; DBit: 0; Sign: False; Flt: False; Fix: True; Comp: False),
(IntFmt: GL_R16UI; ClrFmt: GL_RED_INTEGER; DataFmt: GL_UNSIGNED_SHORT; RBit: 16; GBit: 0; BBit: 0; ABit: 0; LBit: 0; DBit: 0; Sign: False; Flt: False; Fix: True; Comp: False),

(IntFmt: GL_RGB32UI; ClrFmt: GL_RGB_INTEGER; DataFmt: GL_UNSIGNED_INT; RBit: 32; GBit: 32; BBit: 32; ABit: 0; LBit: 0; DBit: 0; Sign: False; Flt: False; Fix: True; Comp: False),
(IntFmt: GL_RGBA32UI; ClrFmt: GL_RGBA_INTEGER; DataFmt: GL_UNSIGNED_INT; RBit: 32; GBit: 32; BBit: 32; ABit: 32; LBit: 0; DBit: 0; Sign: False; Flt: False; Fix: True; Comp: False),
(IntFmt: GL_ALPHA32UI_EXT; ClrFmt: GL_ALPHA_INTEGER_EXT; DataFmt: GL_UNSIGNED_INT; RBit: 0; GBit: 0; BBit: 0; ABit: 32; LBit: 0; DBit: 0; Sign: False; Flt: False; Fix: True; Comp: False),
(IntFmt: GL_INTENSITY32UI_EXT; ClrFmt: GL_LUMINANCE_INTEGER_EXT; DataFmt: GL_UNSIGNED_INT; RBit: 0; GBit: 0; BBit: 0; ABit: 0; LBit: 32; DBit: 0; Sign: False; Flt: False; Fix: True; Comp: False),
(IntFmt: GL_LUMINANCE32UI_EXT; ClrFmt: GL_LUMINANCE_INTEGER_EXT; DataFmt: GL_UNSIGNED_INT; RBit: 0; GBit: 0; BBit: 0; ABit: 0; LBit: 32; DBit: 0; Sign: False; Flt: False; Fix: True; Comp: False),
(IntFmt: GL_LUMINANCE_ALPHA32UI_EXT; ClrFmt: GL_LUMINANCE_ALPHA_INTEGER_EXT; DataFmt: GL_UNSIGNED_INT; RBit: 0; GBit: 0; BBit: 0; ABit: 32; LBit: 32; DBit: 0; Sign: False; Flt: True; Fix: False; Comp: False),
(IntFmt: GL_RG32UI; ClrFmt: GL_RG; DataFmt: GL_UNSIGNED_INT; RBit: 8; GBit: 8; BBit: 0; ABit: 0; LBit: 0; DBit: 0; Sign: False; Flt: False; Fix: True; Comp: False),
(IntFmt: GL_R32UI; ClrFmt: GL_RED_INTEGER; DataFmt: GL_UNSIGNED_INT; RBit: 8; GBit: 0; BBit: 0; ABit: 0; LBit: 0; DBit: 0; Sign: False; Flt: False; Fix: True; Comp: False),

(IntFmt: GL_RGBA8I; ClrFmt: GL_RGBA_INTEGER; DataFmt: GL_BYTE; RBit: 8; GBit: 8; BBit: 8; ABit: 8; LBit: 0; DBit: 0; Sign: True; Flt: False; Fix: True; Comp: False),
(IntFmt: GL_RGB8I; ClrFmt: GL_RGB_INTEGER; DataFmt: GL_BYTE; RBit: 8; GBit: 8; BBit: 8; ABit: 0; LBit: 0; DBit: 0; Sign: True; Flt: False; Fix: True; Comp: False),
(IntFmt: GL_ALPHA8I_EXT; ClrFmt: GL_ALPHA_INTEGER_EXT; DataFmt: GL_BYTE; RBit: 0; GBit: 0; BBit: 0; ABit: 8; LBit: 0; DBit: 0; Sign: True; Flt: False; Fix: True; Comp: False),
(IntFmt: GL_INTENSITY8I_EXT; ClrFmt: GL_LUMINANCE_INTEGER_EXT; DataFmt: GL_BYTE; RBit: 0; GBit: 0; BBit: 0; ABit: 0; LBit: 8; DBit: 0; Sign: True; Flt: False; Fix: True; Comp: False),
(IntFmt: GL_LUMINANCE8I_EXT; ClrFmt: GL_LUMINANCE_INTEGER_EXT; DataFmt: GL_BYTE; RBit: 0; GBit: 0; BBit: 0; ABit: 0; LBit: 8; DBit: 0; Sign: True; Flt: False; Fix: True; Comp: False),
(IntFmt: GL_LUMINANCE_ALPHA8I_EXT; ClrFmt: GL_LUMINANCE_ALPHA_INTEGER_EXT; DataFmt: GL_BYTE; RBit: 0; GBit: 0; BBit: 0; ABit: 8; LBit: 8; DBit: 0; Sign: True; Flt: False; Fix: True; Comp: False),
(IntFmt: GL_RG8I; ClrFmt: GL_RG; DataFmt: GL_BYTE; RBit: 8; GBit: 8; BBit: 0; ABit: 0; LBit: 0; DBit: 0; Sign: True; Flt: False; Fix: True; Comp: False),
(IntFmt: GL_R8I; ClrFmt: GL_RED_INTEGER; DataFmt: GL_BYTE; RBit: 8; GBit: 0; BBit: 0; ABit: 0; LBit: 0; DBit: 0; Sign: True; Flt: False; Fix: True; Comp: False),

(IntFmt: GL_RGB16I; ClrFmt: GL_RGB_INTEGER; DataFmt: GL_SHORT; RBit: 16; GBit: 16; BBit: 16; ABit: 0; LBit: 0; DBit: 0; Sign: True; Flt: False; Fix: True; Comp: False),
(IntFmt: GL_RGBA16I; ClrFmt: GL_RGBA_INTEGER; DataFmt: GL_SHORT; RBit: 16; GBit: 16; BBit: 16; ABit: 16; LBit: 0; DBit: 0; Sign: True; Flt: False; Fix: True; Comp: False),
(IntFmt: GL_ALPHA16I_EXT; ClrFmt: GL_ALPHA_INTEGER_EXT; DataFmt: GL_SHORT; RBit: 0; GBit: 0; BBit: 0; ABit: 16; LBit: 0; DBit: 0; Sign: True; Flt: False; Fix: True; Comp: False),
(IntFmt: GL_INTENSITY16I_EXT; ClrFmt: GL_LUMINANCE_INTEGER_EXT; DataFmt: GL_SHORT; RBit: 0; GBit: 0; BBit: 0; ABit: 0; LBit: 16; DBit: 0; Sign: True; Flt: False; Fix: True; Comp: False),
(IntFmt: GL_LUMINANCE16I_EXT; ClrFmt: GL_LUMINANCE_INTEGER_EXT; DataFmt: GL_SHORT; RBit: 0; GBit: 0; BBit: 0; ABit: 0; LBit: 16; DBit: 0; Sign: True; Flt: False; Fix: True; Comp: False),
(IntFmt: GL_LUMINANCE_ALPHA16I_EXT; ClrFmt: GL_LUMINANCE_ALPHA_INTEGER_EXT; DataFmt: GL_SHORT; RBit: 0; GBit: 0; BBit: 0; ABit: 16; LBit: 16; DBit: 0; Sign: True; Flt: True; Fix: False; Comp: False),
(IntFmt: GL_RG16I; ClrFmt: GL_RG; DataFmt: GL_SHORT; RBit: 16; GBit: 16; BBit: 0; ABit: 0; LBit: 0; DBit: 0; Sign: True; Flt: False; Fix: True; Comp: False),
(IntFmt: GL_R16I; ClrFmt: GL_RED_INTEGER; DataFmt: GL_SHORT; RBit: 16; GBit: 0; BBit: 0; ABit: 0; LBit: 0; DBit: 0; Sign: True; Flt: False; Fix: True; Comp: False),

(IntFmt: GL_RGB32I; ClrFmt: GL_RGB_INTEGER; DataFmt: GL_INT; RBit: 32; GBit: 32; BBit: 32; ABit: 0; LBit: 0; DBit: 0; Sign: True; Flt: False; Fix: True; Comp: False),
(IntFmt: GL_RGBA32I; ClrFmt: GL_RGBA_INTEGER; DataFmt: GL_INT; RBit: 32; GBit: 32; BBit: 32; ABit: 32; LBit: 0; DBit: 0; Sign: True; Flt: False; Fix: True; Comp: False),
(IntFmt: GL_ALPHA32I_EXT; ClrFmt: GL_ALPHA_INTEGER_EXT; DataFmt: GL_INT; RBit: 0; GBit: 0; BBit: 0; ABit: 32; LBit: 0; DBit: 0; Sign: True; Flt: False; Fix: True; Comp: False),
(IntFmt: GL_INTENSITY32I_EXT; ClrFmt: GL_LUMINANCE_INTEGER_EXT; DataFmt: GL_INT; RBit: 0; GBit: 0; BBit: 0; ABit: 0; LBit: 32; DBit: 0; Sign: True; Flt: False; Fix: True; Comp: False),
(IntFmt: GL_LUMINANCE32I_EXT; ClrFmt: GL_LUMINANCE_INTEGER_EXT; DataFmt: GL_INT; RBit: 0; GBit: 0; BBit: 0; ABit: 0; LBit: 32; DBit: 0; Sign: True; Flt: False; Fix: True; Comp: False),
(IntFmt: GL_LUMINANCE_ALPHA32I_EXT; ClrFmt: GL_LUMINANCE_ALPHA_INTEGER_EXT; DataFmt: GL_INT; RBit: 0; GBit: 0; BBit: 0; ABit: 32; LBit: 32; DBit: 0; Sign: True; Flt: False; Fix: True; Comp: False),
(IntFmt: GL_RG32I; ClrFmt: GL_RG; DataFmt: GL_INT; RBit: 32; GBit: 32; BBit: 0; ABit: 0; LBit: 0; DBit: 0; Sign: True; Flt: False; Fix: True; Comp: False),
(IntFmt: GL_R32I; ClrFmt: GL_RED_INTEGER; DataFmt: GL_INT; RBit: 16; GBit: 0; BBit: 0; ABit: 0; LBit: 0; DBit: 0; Sign: True; Flt: False; Fix: True; Comp: False),

(IntFmt: GL_RGB16F_ARB; ClrFmt: GL_RGB; DataFmt: GL_HALF_FLOAT; RBit: 16; GBit: 16; BBit: 16; ABit: 0; LBit: 0; DBit: 0; Sign: False; Flt: True; Fix: False; Comp: False),
(IntFmt: GL_RGBA16F_ARB; ClrFmt: GL_RGBA; DataFmt: GL_HALF_FLOAT; RBit: 16; GBit: 16; BBit: 16; ABit: 16; LBit: 0; DBit: 0; Sign: False; Flt: True; Fix: False; Comp: False),
(IntFmt: GL_ALPHA16F_ARB; ClrFmt: GL_ALPHA; DataFmt: GL_HALF_FLOAT; RBit: 0; GBit: 0; BBit: 0; ABit: 16; LBit: 0; DBit: 0; Sign: False; Flt: True; Fix: False; Comp: False),
(IntFmt: GL_INTENSITY16F_ARB; ClrFmt: GL_LUMINANCE16F_ARB; DataFmt: GL_HALF_FLOAT; RBit: 0; GBit: 0; BBit: 0; ABit: 0; LBit: 16; DBit: 0; Sign: False; Flt: True; Fix: False; Comp: False),
(IntFmt: GL_LUMINANCE16F_ARB; ClrFmt: GL_LUMINANCE16F_ARB; DataFmt: GL_HALF_FLOAT; RBit: 0; GBit: 0; BBit: 0; ABit: 0; LBit: 16; DBit: 0; Sign: False; Flt: True; Fix: False; Comp: False),
(IntFmt: GL_LUMINANCE_ALPHA16F_ARB; ClrFmt: GL_LUMINANCE16F_ARB; DataFmt: GL_HALF_FLOAT; RBit: 0; GBit: 0; BBit: 0; ABit: 16; LBit: 16; DBit: 0; Sign: False; Flt: True; Fix: False; Comp: False),
(IntFmt: GL_RG16F; ClrFmt: GL_RG; DataFmt: GL_HALF_FLOAT; RBit: 16; GBit: 16; BBit: 0; ABit: 0; LBit: 0; DBit: 0; Sign: False; Flt: True; Fix: False; Comp: False),
(IntFmt: GL_R16F; ClrFmt: GL_RED; DataFmt: GL_HALF_FLOAT; RBit: 16; GBit: 0; BBit: 0; ABit: 0; LBit: 0; DBit: 0; Sign: False; Flt: True; Fix: False; Comp: False),

(IntFmt: GL_RGB32F_ARB; ClrFmt: GL_RGB; DataFmt: GL_FLOAT; RBit: 32; GBit: 32; BBit: 32; ABit: 0; LBit: 0; DBit: 0; Sign: False; Flt: True; Fix: False; Comp: False),
(IntFmt: GL_RGBA32F_ARB; ClrFmt: GL_RGBA; DataFmt: GL_FLOAT; RBit: 32; GBit: 32; BBit: 32; ABit: 32; LBit: 0; DBit: 0; Sign: False; Flt: True; Fix: False; Comp: False),
(IntFmt: GL_ALPHA32F_ARB; ClrFmt: GL_ALPHA; DataFmt: GL_FLOAT; RBit: 0; GBit: 0; BBit: 0; ABit: 32; LBit: 0; DBit: 0; Sign: False; Flt: True; Fix: False; Comp: False),
(IntFmt: GL_INTENSITY32F_ARB; ClrFmt: GL_INTENSITY32F_ARB; DataFmt: GL_FLOAT; RBit: 0; GBit: 0; BBit: 0; ABit: 0; LBit: 32; DBit: 0; Sign: False; Flt: True; Fix: False; Comp: False),
(IntFmt: GL_LUMINANCE32F_ARB; ClrFmt: GL_LUMINANCE32F_ARB; DataFmt: GL_FLOAT; RBit: 0; GBit: 0; BBit: 0; ABit: 0; LBit: 32; DBit: 0; Sign: False; Flt: True; Fix: False; Comp: False),
(IntFmt: GL_LUMINANCE_ALPHA32F_ARB; ClrFmt: GL_LUMINANCE_ALPHA32F_ARB; DataFmt: GL_FLOAT; RBit: 0; GBit: 0; BBit: 0; ABit: 32; LBit: 32; DBit: 0; Sign: False; Flt: True; Fix: False; Comp: False),
(IntFmt: GL_RG32F; ClrFmt: GL_RG; DataFmt: GL_FLOAT; RBit: 32; GBit: 32; BBit: 0; ABit: 0; LBit: 0; DBit: 0; Sign: False; Flt: True; Fix: False; Comp: False),
(IntFmt: GL_R32F; ClrFmt: GL_LUMINANCE32F_ARB; DataFmt: GL_FLOAT; RBit: 32; GBit: 0; BBit: 0; ABit: 0; LBit: 0; DBit: 0; Sign: False; Flt: True; Fix: False; Comp: False),

(IntFmt: GL_R8_SNORM; ClrFmt:GL_RED_INTEGER; DataFmt: GL_BYTE; RBit: 8; GBit: 0; BBit: 0; ABit: 0; LBit: 0; DBit: 0; Sign: False; Flt: False; Fix: False; Comp: False),
(IntFmt: GL_RG8_SNORM; ClrFmt: GL_RG; DataFmt: GL_BYTE; RBit: 8; GBit: 8; BBit: 0; ABit: 0; LBit: 0; DBit: 0; Sign: False; Flt: False; Fix: False; Comp: False),
(IntFmt: GL_RGB8_SNORM; ClrFmt: GL_RGB; DataFmt: GL_BYTE; RBit: 8; GBit: 8; BBit: 8; ABit: 0; LBit: 0; DBit: 0; Sign: False; Flt: False; Fix: False; Comp: False),
(IntFmt: GL_RGBA8_SNORM; ClrFmt: GL_RGBA; DataFmt: GL_BYTE; RBit: 8; GBit: 8; BBit: 8; ABit: 8; LBit: 0; DBit: 0; Sign: False; Flt: False; Fix: False; Comp: False),
(IntFmt: GL_R16_SNORM; ClrFmt: GL_RED_INTEGER; DataFmt: GL_SHORT; RBit: 16; GBit: 0; BBit: 0; ABit: 0; LBit: 0; DBit: 0; Sign: False; Flt: False; Fix: False; Comp: False),
(IntFmt: GL_RG16_SNORM; ClrFmt: GL_RG; DataFmt: GL_SHORT; RBit: 16; GBit: 16; BBit: 0; ABit: 0; LBit: 0; DBit: 0; Sign: False; Flt: False; Fix: False; Comp: False),
(IntFmt: GL_RGB16_SNORM; ClrFmt: GL_RGB; DataFmt: GL_SHORT; RBit: 16; GBit: 16; BBit: 16; ABit: 0; LBit: 0; DBit: 0; Sign: False; Flt: False; Fix: False; Comp: False),
(IntFmt: GL_RGBA16_SNORM; ClrFmt: GL_RGBA; DataFmt: GL_SHORT; RBit: 16; GBit: 16; BBit: 16; ABit: 16; LBit: 0; DBit: 0; Sign: False; Flt: False; Fix: False; Comp: False),
//
//
//
//
//
//
//
//
//
//
//
//


//
//
//
//

(IntFmt: GL_COMPRESSED_RGB_S3TC_DXT1_EXT; ClrFmt: GL_COMPRESSED_RGB_S3TC_DXT1_EXT; DataFmt: GL_COMPRESSED_RGB_S3TC_DXT1_EXT; RBit: 8; GBit: 8; BBit: 8; ABit: 0; LBit: 0; DBit: 0; Sign: False; Flt: False; Fix: False; Comp: True),
(IntFmt: GL_COMPRESSED_RGBA_S3TC_DXT1_EXT; ClrFmt: GL_COMPRESSED_RGBA_S3TC_DXT1_EXT; DataFmt: GL_COMPRESSED_RGBA_S3TC_DXT1_EXT; RBit: 8; GBit: 8; BBit: 8; ABit: 1; LBit: 0; DBit: 0; Sign: False; Flt: False; Fix: False; Comp: True),
(IntFmt: GL_COMPRESSED_RGBA_S3TC_DXT3_EXT; ClrFmt: GL_COMPRESSED_RGBA_S3TC_DXT3_EXT; DataFmt: GL_COMPRESSED_RGBA_S3TC_DXT3_EXT; RBit: 8; GBit: 8; BBit: 8; ABit: 8; LBit: 0; DBit: 0; Sign: False; Flt: False; Fix: False; Comp: True),
(IntFmt: GL_COMPRESSED_RGBA_S3TC_DXT5_EXT; ClrFmt: GL_COMPRESSED_RGBA_S3TC_DXT5_EXT; DataFmt: GL_COMPRESSED_RGBA_S3TC_DXT5_EXT; RBit: 8; GBit: 8; BBit: 0; ABit: 8; LBit: 0; DBit: 0; Sign: False; Flt: False; Fix: False; Comp: True),

(IntFmt: GL_COMPRESSED_SRGB_S3TC_DXT1_EXT; ClrFmt: GL_COMPRESSED_SRGB_S3TC_DXT1_EXT; DataFmt: GL_COMPRESSED_SRGB_S3TC_DXT1_EXT; RBit: 8; GBit: 8; BBit: 8; ABit: 0; LBit: 0; DBit: 0; Sign: False; Flt: False; Fix: False; Comp: True),
(IntFmt: GL_COMPRESSED_SRGB_ALPHA_S3TC_DXT1_EXT; ClrFmt: GL_COMPRESSED_SRGB_ALPHA_S3TC_DXT1_EXT; DataFmt: GL_COMPRESSED_SRGB_ALPHA_S3TC_DXT1_EXT; RBit: 8; GBit: 8; BBit: 8; ABit: 1; LBit: 0; DBit: 0; Sign: False; Flt: False; Fix: False; Comp: True),
(IntFmt: GL_COMPRESSED_SRGB_ALPHA_S3TC_DXT3_EXT; ClrFmt: GL_COMPRESSED_SRGB_ALPHA_S3TC_DXT3_EXT; DataFmt: GL_COMPRESSED_SRGB_ALPHA_S3TC_DXT3_EXT; RBit: 8; GBit: 8; BBit: 8; ABit: 8; LBit: 0; DBit: 0; Sign: False; Flt: False; Fix: False; Comp: True),
(IntFmt: GL_COMPRESSED_SRGB_ALPHA_S3TC_DXT5_EXT; ClrFmt: GL_COMPRESSED_SRGB_ALPHA_S3TC_DXT5_EXT; DataFmt: GL_COMPRESSED_SRGB_ALPHA_S3TC_DXT5_EXT; RBit: 8; GBit: 8; BBit: 8; ABit: 8; LBit: 0; DBit: 0; Sign: False; Flt: False; Fix: False; Comp: True),

//(IntFmt: GL_COMPRESSED_LUMINANCE_ALPHA_3DC_ATI; ClrFmt: GL_COMPRESSED_LUMINANCE_ALPHA_3DC_ATI; DataFmt: GL_COMPRESSED_LUMINANCE_ALPHA_3DC_ATI; RBit: 0; GBit: 0; BBit: 0; ABit: 8; LBit: 8; DBit: 0; Sign: False; Flt: False; Fix: False; Comp: True),

(IntFmt: GL_COMPRESSED_LUMINANCE_LATC1_EXT; ClrFmt: GL_COMPRESSED_LUMINANCE_LATC1_EXT; DataFmt: GL_COMPRESSED_LUMINANCE_LATC1_EXT; RBit: 0; GBit: 0; BBit: 0; ABit: 0; LBit: 8; DBit: 0; Sign: False; Flt: False; Fix: False; Comp: True),
(IntFmt: GL_COMPRESSED_SIGNED_LUMINANCE_LATC1_EXT; ClrFmt: GL_COMPRESSED_SIGNED_LUMINANCE_LATC1_EXT; DataFmt: GL_COMPRESSED_SIGNED_LUMINANCE_LATC1_EXT; RBit: 0; GBit: 0; BBit: 0; ABit: 0; LBit: 8; DBit: 0; Sign: True; Flt: False; Fix: False; Comp: True),
(IntFmt: GL_COMPRESSED_LUMINANCE_ALPHA_LATC2_EXT; ClrFmt: GL_COMPRESSED_LUMINANCE_ALPHA_LATC2_EXT; DataFmt: GL_COMPRESSED_LUMINANCE_ALPHA_LATC2_EXT; RBit: 0; GBit: 0; BBit: 0; ABit: 8; LBit: 8; DBit: 0; Sign: False; Flt: False; Fix: False; Comp: True),
(IntFmt: GL_COMPRESSED_SIGNED_LUMINANCE_ALPHA_LATC2_EXT; ClrFmt: GL_COMPRESSED_SIGNED_LUMINANCE_ALPHA_LATC2_EXT; DataFmt: GL_COMPRESSED_SIGNED_LUMINANCE_ALPHA_LATC2_EXT; RBit: 0; GBit: 0; BBit: 0; ABit: 8; LBit: 8; DBit: 0; Sign: True; Flt: False; Fix: False; Comp: True),

(IntFmt: GL_COMPRESSED_RED_RGTC1; ClrFmt: GL_COMPRESSED_RED_RGTC1; DataFmt: GL_COMPRESSED_RED_RGTC1; RBit: 8; GBit: 0; BBit: 0; ABit: 0; LBit: 0; DBit: 0; Sign: False; Flt: False; Fix: False; Comp: True),
(IntFmt: GL_COMPRESSED_SIGNED_RED_RGTC1; ClrFmt: GL_COMPRESSED_SIGNED_RED_RGTC1; DataFmt: GL_COMPRESSED_SIGNED_RED_RGTC1; RBit: 8; GBit: 0; BBit: 0; ABit: 0; LBit: 0; DBit: 0; Sign: True; Flt: False; Fix: False; Comp: True),
(IntFmt: GL_COMPRESSED_RG_RGTC2; ClrFmt: GL_COMPRESSED_RG_RGTC2; DataFmt: GL_COMPRESSED_RG_RGTC2; RBit: 8; GBit: 8; BBit: 0; ABit: 0; LBit: 0; DBit: 0; Sign: False; Flt: False; Fix: False; Comp: True),
(IntFmt: GL_COMPRESSED_SIGNED_RG_RGTC2; ClrFmt: GL_COMPRESSED_SIGNED_RG_RGTC2; DataFmt: GL_COMPRESSED_SIGNED_RG_RGTC2; RBit: 8; GBit: 8; BBit: 0; ABit: 0; LBit: 0; DBit: 0; Sign: True; Flt: False; Fix: False; Comp: True)

//
//
//
//


//
//
//
//
//
//
//
//
//
//



//
//
//
//
//
//
//
//
//
//
//
//
//
//
//
//
//
//
//
//
//
//
//
//
//
//
//
//
   );

function InternalFormatToOpenGLFormat(intFormat: TDGLInternalFormat): TGLEnum;
begin
  Result := cTextureFormatToOpenGL[intFormat].IntFmt;
end;

function OpenGLFormatToInternalFormat(glFormat: TGLEnum): TDGLInternalFormat;
var
  i: TDGLInternalFormat;
begin
  Result := tfRGBA8;
  for i := Low(cTextureFormatToOpenGL) to High(cTextureFormatToOpenGL) do
    if glFormat = cTextureFormatToOpenGL[i].IntFmt then
    begin
      Result := i;
      Exit;
    end;
  Assert(false);
end;

function GetTextureElementSize(intFormat: TDGLInternalFormat): Integer;
begin
  Result := GetTextureElementSize(
    cTextureFormatToOpenGL[intFormat].ClrFmt,
    cTextureFormatToOpenGL[intFormat].DataFmt);
end;

function GetTextureElementSize(colorFormat: TGLEnum; dataType: TGLEnum): Integer;
var
  components: Byte;
begin
  case colorFormat of
    GL_RGB, GL_BGR: components := 3;
    GL_RGBA, GL_BGRA: components := 4;
    GL_ALPHA: components := 1;
    //GL_LUMINANCE: components := 1;
    //GL_LUMINANCE_ALPHA: components := 2;
    //GL_INTENSITY: components := 1;
    GL_RED: components := 1;
    GL_GREEN: components := 1;
    GL_BLUE: components := 1;
    GL_RG: components := 2;

    GL_RGB_INTEGER: components := 3;
    GL_RGBA_INTEGER: components := 4;
    //GL_ALPHA_INTEGER: components := 1;
    GL_LUMINANCE_INTEGER_EXT: components := 1;
    GL_LUMINANCE_ALPHA_INTEGER_EXT: components := 2;
    GL_RED_INTEGER: components := 1;
    GL_RG_INTEGER: components := 2;
  else
    components := 1;
  end;

  case dataType of
   // GL_BITMAP,
      GL_UNSIGNED_BYTE,
      GL_BYTE: Result := components;
    GL_UNSIGNED_BYTE_3_3_2,
      GL_UNSIGNED_BYTE_2_3_3_REV: Result := 1;
    GL_UNSIGNED_SHORT,
      GL_SHORT: Result := components * 2;
    GL_UNSIGNED_SHORT_4_4_4_4,
      GL_UNSIGNED_SHORT_4_4_4_4_REV,
      GL_UNSIGNED_SHORT_5_6_5,
      GL_UNSIGNED_SHORT_5_6_5_REV,
      GL_UNSIGNED_SHORT_5_5_5_1,
      GL_UNSIGNED_SHORT_1_5_5_5_REV: Result := 2;

    GL_UNSIGNED_INT,
      GL_INT: Result := components * 4;
    GL_UNSIGNED_INT_8_8_8_8,
      GL_UNSIGNED_INT_8_8_8_8_REV,
      GL_UNSIGNED_INT_10_10_10_2,
      GL_UNSIGNED_INT_2_10_10_10_REV: Result := 4;

    GL_FLOAT: Result := components * 4;
    GL_HALF_FLOAT: Result := components * 2;

    GL_COMPRESSED_RGB_S3TC_DXT1_EXT: Result := 8;
    GL_COMPRESSED_RGBA_S3TC_DXT1_EXT: Result := 8;
    GL_COMPRESSED_RGBA_S3TC_DXT3_EXT: Result := 16;
    GL_COMPRESSED_RGBA_S3TC_DXT5_EXT: Result := 16;
    GL_COMPRESSED_SRGB_S3TC_DXT1_EXT: Result := 8;
    GL_COMPRESSED_SRGB_ALPHA_S3TC_DXT1_EXT: Result := 8;
    GL_COMPRESSED_SRGB_ALPHA_S3TC_DXT3_EXT: Result := 16;
    GL_COMPRESSED_SRGB_ALPHA_S3TC_DXT5_EXT: Result := 16;
    GL_COMPRESSED_LUMINANCE_LATC1_EXT: Result := 8;
    GL_COMPRESSED_SIGNED_LUMINANCE_LATC1_EXT: Result := 8;
    GL_COMPRESSED_LUMINANCE_ALPHA_LATC2_EXT: Result := 16;
    GL_COMPRESSED_SIGNED_LUMINANCE_ALPHA_LATC2_EXT: Result := 16;
    //GL_COMPRESSED_LUMINANCE_ALPHA_3DC_ATI: Result := 16;
    GL_COMPRESSED_RED_RGTC1: Result := 8;
    GL_COMPRESSED_SIGNED_RED_RGTC1: Result := 8;
    GL_COMPRESSED_RG_RGTC2: Result := 16;
    GL_COMPRESSED_SIGNED_RG_RGTC2: Result := 16;
  else
    Result := 1;
  end;
end;

function CompressedInternalFormatToOpenGL(intFormat: TDGLInternalFormat):
  Integer;
begin
  Result := GL_COMPRESSED_RGBA;
  case intFormat of
    tfRGB8: Result := GL_COMPRESSED_RGB;
    tfRGBA8: Result := GL_COMPRESSED_RGBA;
    tfRGB5: Result := GL_COMPRESSED_RGB;
    tfRGBA4: Result := GL_COMPRESSED_RGBA;
    //tfALPHA8: Result := GL_COMPRESSED_ALPHA_ARB;
    //tfLUMINANCE8: Result := GL_COMPRESSED_LUMINANCE_ARB;
    //tfLUMINANCE8_ALPHA8: Result := GL_COMPRESSED_LUMINANCE_ALPHA_ARB;
    //tfINTENSITY8: Result := GL_COMPRESSED_INTENSITY_ARB;
  else
    Assert(false);
  end;
end;

procedure FindCompatibleDataFormat(intFormat: TDGLInternalFormat; out dFormat:
  TGLEnum; out dType: GLenum);
begin
  dFormat := cTextureFormatToOpenGL[intFormat].ClrFmt;
  dType := cTextureFormatToOpenGL[intFormat].DataFmt;
end;

function IsTargetSupported(target: TDGLTextureTarget): Boolean;
begin
  Result := IsTargetSupported(DecodeGLTextureTarget(target));
end;

function IsTargetSupported(glTarget: TGLEnum): Boolean;
begin
  case glTarget of
    GL_TEXTURE_1D: Result := GL_VERSION_1_1 or dglCheckExtension('EXT_texture_object');
    GL_TEXTURE_2D: Result := GL_VERSION_1_1 or dglCheckExtension('EXT_texture_object');
    GL_TEXTURE_3D: Result := dglCheckExtension('GL_EXT_texture3D');
    GL_TEXTURE_RECTANGLE: Result := dglCheckExtension('ARB_texture_rectangle');
    GL_TEXTURE_CUBE_MAP,
      GL_TEXTURE_CUBE_MAP_POSITIVE_X,
      GL_TEXTURE_CUBE_MAP_NEGATIVE_X,
      GL_TEXTURE_CUBE_MAP_POSITIVE_Y,
      GL_TEXTURE_CUBE_MAP_NEGATIVE_Y,
      GL_TEXTURE_CUBE_MAP_POSITIVE_Z,
      GL_TEXTURE_CUBE_MAP_NEGATIVE_Z: Result := dglCheckExtension('ARB_texture_cube_map');
    GL_TEXTURE_1D_ARRAY: Result := dglCheckExtension('EXT_texture_array');
    GL_TEXTURE_2D_ARRAY: Result := dglCheckExtension('EXT_texture_array');
    GL_TEXTURE_CUBE_MAP_ARRAY: Result := dglCheckExtension('ARB_texture_cube_map_array');
    GL_TEXTURE_BUFFER: Result := dglCheckExtension('ARB_texture_buffer_object');
    GL_TEXTURE_2D_MULTISAMPLE,
      GL_TEXTURE_2D_MULTISAMPLE_ARRAY: Result := dglCheckExtension('ARB_texture_multisample');
  else
    begin
      Result := false;
      Assert(False, glsErrorEx + glsUnknownType);
    end;
  end;
end;

function IsFormatSupported(intFormat: TDGLInternalFormat): Boolean;
begin
  Result := false;

 // if ((intFormat >= tfALPHA4) and (intFormat <= tfALPHA16)) or  ((intFormat >= tfLUMINANCE4) and (intFormat <= tfR16G16B16A16)) then
//  begin
//    Result := GL.VERSION_1_1;
//    EXIT;
//  end;

  if ((intFormat >= tfDEPTH_COMPONENT16) and (intFormat <= tfDEPTH_COMPONENT32)) then
  begin
    Result := dglCheckExtension('ARB_depth_texture');
    EXIT;
  end;

  if ((intFormat >= tfCOMPRESSED_RGB_S3TC_DXT1) and (intFormat <= tfCOMPRESSED_RGBA_S3TC_DXT5)) then
  begin
    Result := dglCheckExtension('EXT_texture_compression_s3tc');
    EXIT;
  end;

//  if ((intFormat >= tfSIGNED_LUMINANCE8) and (intFormat <= tfDSDT8_MAG8_INTENSITY8)) then
//  begin
//    Result := GL.NV_texture_shader;
//    EXIT;
//  end;
//
//  if ((intFormat = tfHILO8) or (intFormat = tfSIGNED_HILO8)) then
//  begin
//    Result := GL.NV_texture_shader3;
//    EXIT;
//  end;
//
//  if ((intFormat >= tfFLOAT_R16) and (intFormat <= tfFLOAT_RGBA32)) then
//  begin
//    Result := GL.NV_float_buffer;
//    EXIT;
//  end;
//
//  if ((intFormat >= tfRGBA_FLOAT32) and (intFormat <= tfLUMINANCE_ALPHA_FLOAT16)) then
//  begin
//    Result := GL.ARB_texture_float or GL.ATI_texture_float;
//    EXIT;
//  end;
//
//  if intFormat = tfDEPTH24_STENCIL8 then
//  begin
//    Result := GL.EXT_packed_depth_stencil;
//    EXIT;
//  end;
//
//  if ((intFormat = tfDEPTH_COMPONENT32F) or (intFormat = tfDEPTH32F_STENCIL8)) then
//  begin
//    Result := GL.NV_depth_buffer_float;
//    EXIT;
//  end;

  if ((intFormat >= tfSRGB8) and (intFormat <= tfCOMPRESSED_SRGB_ALPHA_S3TC_DXT5)) then
  begin
    Result := dglCheckExtension('EXT_texture_sRGB');
    EXIT;
  end;

  if intFormat = tfRGB9_E5 then
  begin
    Result := dglCheckExtension('EXT_texture_shared_exponent');
    EXIT;
  end;

  if intFormat = tfR11F_G11F_B10F then
  begin
    Result := dglCheckExtension('EXT_packed_float');
    EXIT;
  end;

  if ((intFormat >= tfCOMPRESSED_LUMINANCE_LATC1) and (intFormat <= tfCOMPRESSED_SIGNED_LUMINANCE_ALPHA_LATC2)) then
  begin
    Result := dglCheckExtension('EXT_texture_compression_latc');
    EXIT;
  end;

//  if intFormat = tfCOMPRESSED_LUMINANCE_ALPHA_3DC then
//  begin
//    Result := GL.ATI_texture_compression_3dc;
//    EXIT;
//  end;

  if ((intFormat >= tfRGBA32UI) and (intFormat <= tfLUMINANCE_ALPHA8I)) then
  begin
    Result := dglCheckExtension('EXT_texture_integer');
    EXIT;
  end;

  if ((intFormat >= tfRG32UI) and (intFormat <= tfR32F)) then
    Result := dglCheckExtension('ARB_texture_rg');

  if ((intFormat >= tfCOMPRESSED_RED_RGTC1) and (intFormat <= tfCOMPRESSED_SIGNED_RG_RGTC2)) then
  begin
    Result := dglCheckExtension('ARB_texture_compression_rgtc');
    EXIT;
  end;

  if ((intFormat >= tfR8_SNORM) and (intFormat <= tfRGBA16_SNORM)) then
  begin
    Result := GL_VERSION_3_1;
    EXIT;
  end
end;

function IsFloatFormat(intFormat: TDGLInternalFormat): boolean;
begin
  Result := cTextureFormatToOpenGL[intFormat].Flt;
end;

function IsFloatFormat(glFormat: TGLEnum): boolean;
begin
  Result := IsFloatFormat(OpenGLFormatToInternalFormat(glFormat));
end;

function IsDepthFormat(intFormat: TDGLInternalFormat): boolean;
begin
  Result := cTextureFormatToOpenGL[intFormat].DBit > 0;
end;

function IsDepthFormat(glFormat: TGLEnum): boolean;
begin
  Result := cTextureFormatToOpenGL[OpenGLFormatToInternalFormat(glFormat)].DBit > 0;
end;

function IsCompressedFormat(intFormat: TDGLInternalFormat): boolean;
begin
  Result := cTextureFormatToOpenGL[intFormat].Comp;
end;

function IsCompressedFormat(glFormat: TGLEnum): boolean;
begin
  Result := cTextureFormatToOpenGL[OpenGLFormatToInternalFormat(glFormat)].Comp;
end;

function GetGenericCompressedFormat(const intFormat: TDGLInternalFormat;
  const colorFormat: TGLEnum; out internalFormat: TGLEnum): Boolean;

begin
  Result := false;
  if IsCompressedFormat(intFormat) then Exit;
  if not IsFormatSupported(intFormat) then Exit;
  internalFormat := 0;

  if ((intFormat >= tfSRGB8) and (intFormat <= tfCOMPRESSED_SRGB_ALPHA_S3TC_DXT5)) then
    case colorFormat of
      GL_RGB: internalFormat := GL_COMPRESSED_SRGB;
      GL_RGBA: internalFormat := GL_COMPRESSED_SRGB_ALPHA;
//      GL_LUMINANCE16F_ARB: internalFormat := GL_COMPRESSED_SLUMINANCE_ARB;
//      GL_LUMINANCE_ALPHA16F_ARB: internalFormat := GL_COMPRESSED_SLUMINANCE_ALPHA;
//      GL_LUMINANCE32F_ARB: internalFormat := GL_COMPRESSED_SLUMINANCE_ARB;
//      GL_LUMINANCE_ALPHA32F_ARB: internalFormat := GL_COMPRESSED_SLUMINANCE_ALPHA;
    end
  else
    case colorFormat of
      GL_RGB, GL_BGR: internalFormat := GL_COMPRESSED_RGB;
      GL_RGBA, GL_BGRA: internalFormat := GL_COMPRESSED_RGBA;
      GL_ALPHA: internalFormat := GL_COMPRESSED_ALPHA_ARB;
      //GL_LUMINANCE: internalFormat := GL_COMPRESSED_LUMINANCE;
      //GL_LUMINANCE_ALPHA: internalFormat := GL_COMPRESSED_LUMINANCE_ALPHA;
      //GL_INTENSITY: internalFormat := GL_COMPRESSED_INTENSITY;

//      GL_LUMINANCE16F_ARB: internalFormat := GL_COMPRESSED_SLUMINANCE_ARB;
//      GL_LUMINANCE_ALPHA16F_ARB: internalFormat := GL_COMPRESSED_SLUMINANCE_ALPHA;
//      GL_LUMINANCE32F_ARB: internalFormat := GL_COMPRESSED_SLUMINANCE_ARB;
//      GL_LUMINANCE_ALPHA32F_ARB: internalFormat := GL_COMPRESSED_SLUMINANCE_ALPHA;
      GL_INTENSITY16F_ARB: internalFormat := GL_COMPRESSED_INTENSITY_ARB;
      GL_INTENSITY32F_ARB: internalFormat := GL_COMPRESSED_INTENSITY_ARB;

      GL_RED: internalFormat := GL_COMPRESSED_RED;
      GL_RG: internalFormat := GL_COMPRESSED_RG;
    end;

  if internalFormat = 0 then
    Exit;
  Result := true;
end;

function GetUncompressedFormat(const intFormat: TDGLInternalFormat;
  out internalFormat: TDGLInternalFormat; out colorFormat: TGLEnum): Boolean;
begin
  Result := false;
  if not IsCompressedFormat(intFormat) then
    Exit;
  if not IsFormatSupported(intFormat) then
    Exit;
  colorFormat := 0;
  case intFormat of
    tfCOMPRESSED_RGB_S3TC_DXT1:
      begin
        colorFormat := GL_RGB;
        internalFormat := tfRGB8;
      end;
    tfCOMPRESSED_RGBA_S3TC_DXT1:
      begin
        colorFormat := GL_RGBA;
        internalFormat := tfRGBA8;
      end;
    tfCOMPRESSED_RGBA_S3TC_DXT3:
      begin
        colorFormat := GL_RGBA;
        internalFormat := tfRGBA8;
      end;
    tfCOMPRESSED_RGBA_S3TC_DXT5:
      begin
        colorFormat := GL_RGBA;
        internalFormat := tfRGBA8;
      end;
    tfCOMPRESSED_SRGB_S3TC_DXT1:
      begin
        colorFormat := GL_RGBA;
        internalFormat := tfSRGB8;
      end;
    tfCOMPRESSED_SRGB_ALPHA_S3TC_DXT1:
      begin
        colorFormat := GL_RGBA;
        internalFormat := tfSRGB8_ALPHA8;
      end;
    tfCOMPRESSED_SRGB_ALPHA_S3TC_DXT3:
      begin
        colorFormat := GL_RGBA;
        internalFormat := tfSRGB8_ALPHA8;
      end;
    tfCOMPRESSED_SRGB_ALPHA_S3TC_DXT5:
      begin
        colorFormat := GL_RGBA;
        internalFormat := tfSRGB8_ALPHA8;
      end;
    tfCOMPRESSED_LUMINANCE_LATC1:
      begin
        //colorFormat := GL_LUMINANCE;
        //internalFormat := tfLUMINANCE8;
      end;
    tfCOMPRESSED_SIGNED_LUMINANCE_LATC1:
      begin
        //colorFormat := GL_LUMINANCE_ARB;
        //internalFormat := tfSIGNED_LUMINANCE8;
      end;
    tfCOMPRESSED_LUMINANCE_ALPHA_LATC2:
      begin
        //colorFormat := GL_LUMINANCE_ALPHA;
        //internalFormat := tfLUMINANCE8_ALPHA8;
      end;
    tfCOMPRESSED_SIGNED_LUMINANCE_ALPHA_LATC2:
      begin
        //colorFormat := GL_LUMINANCE_ALPHA;
        //internalFormat := tfSIGNED_LUMINANCE8_ALPHA8;
      end;
   // tfCOMPRESSED_LUMINANCE_ALPHA_3DC:
   //   begin
        //colorFormat := GL_LUMINANCE_ALPHA;
        //internalFormat := tfLUMINANCE8_ALPHA8;
    //  end;
    tfCOMPRESSED_RED_RGTC1:
      begin
        colorFormat := GL_RED;
        internalFormat := tfR8;
      end;
    tfCOMPRESSED_SIGNED_RED_RGTC1:
      begin
        colorFormat := GL_RED;
        internalFormat := tfR8;
      end;
    tfCOMPRESSED_RG_RGTC2:
      begin
        colorFormat := GL_RG;
        internalFormat := tfRG8;
      end;
    tfCOMPRESSED_SIGNED_RG_RGTC2:
      begin
        colorFormat := GL_RG;
        internalFormat := tfRG8;
      end;
  end;
  Result := colorFormat <> 0;
end;

function DecodeGLTextureTarget(const TextureTarget: TDGLTextureTarget): Cardinal;
const
  cTargetToEnum: array[TDGLTextureTarget] of TGLEnum =
  (
    0,
    GL_TEXTURE_1D,
    GL_TEXTURE_2D,
    GL_TEXTURE_3D,
    GL_TEXTURE_1D_ARRAY,
    GL_TEXTURE_2D_ARRAY,
    GL_TEXTURE_RECTANGLE,
    GL_TEXTURE_BUFFER,
    GL_TEXTURE_CUBE_MAP,
    GL_TEXTURE_2D_MULTISAMPLE,
    GL_TEXTURE_2D_MULTISAMPLE_ARRAY,
    GL_TEXTURE_CUBE_MAP_ARRAY
  );
begin
  Assert(TextureTarget <> ttNoShape);
  Result := cTargetToEnum[TextureTarget];
end;

function EncodeGLTextureTarget(const glTarget: TGLEnum): TDGLTextureTarget;
begin
  case glTarget of
    GL_TEXTURE_1D: Result := ttTexture1d;
    GL_TEXTURE_2D: Result := ttTexture2d;
    GL_TEXTURE_3D: Result := ttTexture3d;
    GL_TEXTURE_RECTANGLE: Result := ttTextureRect;
    GL_TEXTURE_CUBE_MAP: Result := ttTextureCube;
    GL_TEXTURE_1D_ARRAY: Result := ttTexture1dArray;
    GL_TEXTURE_2D_ARRAY: Result := ttTexture2dArray;
    GL_TEXTURE_CUBE_MAP_ARRAY: Result := ttTextureCubeArray;
    GL_TEXTURE_2D_MULTISAMPLE: Result := ttTexture2DMultisample;
    GL_TEXTURE_2D_MULTISAMPLE_ARRAY: Result := ttTexture2DMultisampleArray;
  else
    begin
      Result := ttTexture2d;
      Assert(False, glsErrorEx + glsUnknownType);
    end;
  end;
end;

function IsTargetSupportMipmap(const TextureTarget: TDGLTextureTarget): Boolean;
begin
  Result := (TextureTarget <> ttTextureRect)
    and (TextureTarget <> ttTexture2DMultisample)
    and (TextureTarget <> ttTexture2DMultisampleArray);
end;

function IsTargetSupportMipmap(const glTarget: TGLEnum): Boolean;
begin
  Result := (glTarget <> GL_TEXTURE_RECTANGLE)
    and (glTarget <> GL_TEXTURE_2D_MULTISAMPLE)
    and (glTarget <> GL_TEXTURE_2D_MULTISAMPLE_ARRAY);
end;

end.


