// CodeGear C++Builder
// Copyright (c) 1995, 2012 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'ImageUtils.pas' rev: 24.00 (Win32)

#ifndef ImageutilsHPP
#define ImageutilsHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <System.SysUtils.hpp>	// Pascal unit
#include <System.Classes.hpp>	// Pascal unit
#include <GLCrossPlatform.hpp>	// Pascal unit
#include <OpenGLTokens.hpp>	// Pascal unit
#include <GLTextureFormat.hpp>	// Pascal unit
#include <VectorGeometry.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Imageutils
{
//-- type declarations -------------------------------------------------------
struct DECLSPEC_DRECORD TIntermediateFormat
{
public:
	float R;
	float G;
	float B;
	float A;
};


typedef System::DynamicArray<void *> TPointerArray;

typedef TIntermediateFormat *PRGBA32F;

typedef System::StaticArray<TIntermediateFormat, 67108864> TIntermediateFormatArray;

typedef TIntermediateFormatArray *PIntermediateFormatArray;

typedef System::StaticArray<System::StaticArray<System::Byte, 4>, 4> TU48BitBlock;

typedef System::StaticArray<System::StaticArray<short, 4>, 4> T48BitBlock;

class DELPHICLASS EGLImageUtils;
#pragma pack(push,4)
class PASCALIMPLEMENTATION EGLImageUtils : public System::Sysutils::Exception
{
	typedef System::Sysutils::Exception inherited;
	
public:
	/* Exception.Create */ inline __fastcall EGLImageUtils(const System::UnicodeString Msg) : System::Sysutils::Exception(Msg) { }
	/* Exception.CreateFmt */ inline __fastcall EGLImageUtils(const System::UnicodeString Msg, System::TVarRec const *Args, const int Args_Size) : System::Sysutils::Exception(Msg, Args, Args_Size) { }
	/* Exception.CreateRes */ inline __fastcall EGLImageUtils(NativeUInt Ident)/* overload */ : System::Sysutils::Exception(Ident) { }
	/* Exception.CreateRes */ inline __fastcall EGLImageUtils(System::PResStringRec ResStringRec)/* overload */ : System::Sysutils::Exception(ResStringRec) { }
	/* Exception.CreateResFmt */ inline __fastcall EGLImageUtils(NativeUInt Ident, System::TVarRec const *Args, const int Args_Size)/* overload */ : System::Sysutils::Exception(Ident, Args, Args_Size) { }
	/* Exception.CreateResFmt */ inline __fastcall EGLImageUtils(System::PResStringRec ResStringRec, System::TVarRec const *Args, const int Args_Size)/* overload */ : System::Sysutils::Exception(ResStringRec, Args, Args_Size) { }
	/* Exception.CreateHelp */ inline __fastcall EGLImageUtils(const System::UnicodeString Msg, int AHelpContext) : System::Sysutils::Exception(Msg, AHelpContext) { }
	/* Exception.CreateFmtHelp */ inline __fastcall EGLImageUtils(const System::UnicodeString Msg, System::TVarRec const *Args, const int Args_Size, int AHelpContext) : System::Sysutils::Exception(Msg, Args, Args_Size, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall EGLImageUtils(NativeUInt Ident, int AHelpContext)/* overload */ : System::Sysutils::Exception(Ident, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall EGLImageUtils(System::PResStringRec ResStringRec, int AHelpContext)/* overload */ : System::Sysutils::Exception(ResStringRec, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall EGLImageUtils(System::PResStringRec ResStringRec, System::TVarRec const *Args, const int Args_Size, int AHelpContext)/* overload */ : System::Sysutils::Exception(ResStringRec, Args, Args_Size, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall EGLImageUtils(NativeUInt Ident, System::TVarRec const *Args, const int Args_Size, int AHelpContext)/* overload */ : System::Sysutils::Exception(Ident, Args, Args_Size, AHelpContext) { }
	/* Exception.Destroy */ inline __fastcall virtual ~EGLImageUtils(void) { }
	
};

#pragma pack(pop)

typedef float __fastcall (*TImageFilterFunction)(float Value);

typedef void __fastcall (*TImageAlphaProc)(TIntermediateFormat &AColor);

//-- var, const, procedure ---------------------------------------------------
extern PACKAGE int vImageScaleFilterWidth;
extern PACKAGE float __fastcall ImageBoxFilter(float Value);
extern PACKAGE float __fastcall ImageTriangleFilter(float Value);
extern PACKAGE float __fastcall ImageHermiteFilter(float Value);
extern PACKAGE float __fastcall ImageBellFilter(float Value);
extern PACKAGE float __fastcall ImageSplineFilter(float Value);
extern PACKAGE float __fastcall ImageLanczos3Filter(float Value);
extern PACKAGE float __fastcall ImageMitchellFilter(float Value);
extern PACKAGE void __fastcall ImageAlphaFromIntensity(TIntermediateFormat &AColor);
extern PACKAGE void __fastcall ImageAlphaSuperBlackTransparent(TIntermediateFormat &AColor);
extern PACKAGE void __fastcall ImageAlphaLuminance(TIntermediateFormat &AColor);
extern PACKAGE void __fastcall ImageAlphaLuminanceSqrt(TIntermediateFormat &AColor);
extern PACKAGE void __fastcall ImageAlphaOpaque(TIntermediateFormat &AColor);
extern PACKAGE void __fastcall ImageAlphaTopLeftPointColorTransparent(TIntermediateFormat &AColor);
extern PACKAGE void __fastcall ImageAlphaInverseLuminance(TIntermediateFormat &AColor);
extern PACKAGE void __fastcall ImageAlphaInverseLuminanceSqrt(TIntermediateFormat &AColor);
extern PACKAGE void __fastcall ImageAlphaBottomRightPointColorTransparent(TIntermediateFormat &AColor);
extern PACKAGE void __fastcall ConvertImage(const void * ASrc, const void * ADst, unsigned ASrcColorFormat, unsigned ADstColorFormat, unsigned ASrcDataType, unsigned ADstDataType, int AWidth, int AHeight);
extern PACKAGE void __fastcall RescaleImage(const void * ASrc, const void * ADst, unsigned AColorFormat, unsigned ADataType, TImageFilterFunction AFilter, int ASrcWidth, int ASrcHeight, int ADstWidth, int ADstHeight);
extern PACKAGE void __fastcall Build2DMipmap(const void * ASrc, const TPointerArray ADst, unsigned AColorFormat, unsigned ADataType, TImageFilterFunction AFilter, int ASrcWidth, int ASrcHeight);
extern PACKAGE void __fastcall AlphaGammaBrightCorrection(const void * ASrc, unsigned AColorFormat, unsigned ADataType, int ASrcWidth, int ASrcHeight, TImageAlphaProc anAlphaProc, float ABrightness, float AGamma);
}	/* namespace Imageutils */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_IMAGEUTILS)
using namespace Imageutils;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// ImageutilsHPP
