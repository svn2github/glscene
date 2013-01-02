// CodeGear C++Builder
// Copyright (c) 1995, 2012 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLUtils.pas' rev: 24.00 (Win32)

#ifndef GlutilsHPP
#define GlutilsHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <System.Classes.hpp>	// Pascal unit
#include <System.SysUtils.hpp>	// Pascal unit
#include <Vcl.Graphics.hpp>	// Pascal unit
#include <Vcl.Controls.hpp>	// Pascal unit
#include <VectorGeometry.hpp>	// Pascal unit
#include <GLCrossPlatform.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Glutils
{
//-- type declarations -------------------------------------------------------
class DELPHICLASS EGLUtilsException;
#pragma pack(push,4)
class PASCALIMPLEMENTATION EGLUtilsException : public System::Sysutils::Exception
{
	typedef System::Sysutils::Exception inherited;
	
public:
	/* Exception.Create */ inline __fastcall EGLUtilsException(const System::UnicodeString Msg) : System::Sysutils::Exception(Msg) { }
	/* Exception.CreateFmt */ inline __fastcall EGLUtilsException(const System::UnicodeString Msg, System::TVarRec const *Args, const int Args_Size) : System::Sysutils::Exception(Msg, Args, Args_Size) { }
	/* Exception.CreateRes */ inline __fastcall EGLUtilsException(NativeUInt Ident)/* overload */ : System::Sysutils::Exception(Ident) { }
	/* Exception.CreateRes */ inline __fastcall EGLUtilsException(System::PResStringRec ResStringRec)/* overload */ : System::Sysutils::Exception(ResStringRec) { }
	/* Exception.CreateResFmt */ inline __fastcall EGLUtilsException(NativeUInt Ident, System::TVarRec const *Args, const int Args_Size)/* overload */ : System::Sysutils::Exception(Ident, Args, Args_Size) { }
	/* Exception.CreateResFmt */ inline __fastcall EGLUtilsException(System::PResStringRec ResStringRec, System::TVarRec const *Args, const int Args_Size)/* overload */ : System::Sysutils::Exception(ResStringRec, Args, Args_Size) { }
	/* Exception.CreateHelp */ inline __fastcall EGLUtilsException(const System::UnicodeString Msg, int AHelpContext) : System::Sysutils::Exception(Msg, AHelpContext) { }
	/* Exception.CreateFmtHelp */ inline __fastcall EGLUtilsException(const System::UnicodeString Msg, System::TVarRec const *Args, const int Args_Size, int AHelpContext) : System::Sysutils::Exception(Msg, Args, Args_Size, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall EGLUtilsException(NativeUInt Ident, int AHelpContext)/* overload */ : System::Sysutils::Exception(Ident, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall EGLUtilsException(System::PResStringRec ResStringRec, int AHelpContext)/* overload */ : System::Sysutils::Exception(ResStringRec, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall EGLUtilsException(System::PResStringRec ResStringRec, System::TVarRec const *Args, const int Args_Size, int AHelpContext)/* overload */ : System::Sysutils::Exception(ResStringRec, Args, Args_Size, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall EGLUtilsException(NativeUInt Ident, System::TVarRec const *Args, const int Args_Size, int AHelpContext)/* overload */ : System::Sysutils::Exception(Ident, Args, Args_Size, AHelpContext) { }
	/* Exception.Destroy */ inline __fastcall virtual ~EGLUtilsException(void) { }
	
};

#pragma pack(pop)

typedef System::StaticArray<System::Byte, 256> TSqrt255Array;

typedef TSqrt255Array *PSqrt255Array;

//-- var, const, procedure ---------------------------------------------------
extern PACKAGE void __fastcall WordToIntegerArray(System::Sysutils::PWordArray Source, Vectorgeometry::PIntegerVector Dest, unsigned Count);
extern PACKAGE int __fastcall RoundUpToPowerOf2(int value);
extern PACKAGE int __fastcall RoundDownToPowerOf2(int value);
extern PACKAGE bool __fastcall IsPowerOf2(int value);
extern PACKAGE System::AnsiString __fastcall ReadCRLFString(System::Classes::TStream* aStream);
extern PACKAGE void __fastcall WriteCRLFString(System::Classes::TStream* aStream, const System::AnsiString aString);
extern PACKAGE bool __fastcall TryStrToFloat(const System::UnicodeString strValue, System::Extended &val);
extern PACKAGE System::Extended __fastcall StrToFloatDef(const System::UnicodeString strValue, System::Extended defValue = 0.000000E+00);
extern PACKAGE System::Uitypes::TColor __fastcall StringToColorAdvancedSafe(const System::UnicodeString Str, const System::Uitypes::TColor Default);
extern PACKAGE System::Uitypes::TColor __fastcall StringToColorAdvanced(const System::UnicodeString Str);
extern PACKAGE bool __fastcall TryStringToColorAdvanced(const System::UnicodeString Str, System::Uitypes::TColor &OutColor);
extern PACKAGE int __fastcall ParseInteger(System::WideChar * &p);
extern PACKAGE System::Extended __fastcall ParseFloat(System::WideChar * &p);
extern PACKAGE void __fastcall SaveAnsiStringToFile(const System::UnicodeString fileName, const System::AnsiString data);
extern PACKAGE System::AnsiString __fastcall LoadAnsiStringFromFile(const System::UnicodeString fileName);
extern PACKAGE void __fastcall SaveComponentToFile(System::Classes::TComponent* const Component, const System::UnicodeString FileName, const bool AsText = true);
extern PACKAGE void __fastcall LoadComponentFromFile(System::Classes::TComponent* const Component, const System::UnicodeString FileName, const bool AsText = true);
extern PACKAGE __int64 __fastcall SizeOfFile(const System::UnicodeString fileName);
extern PACKAGE PSqrt255Array __fastcall GetSqrt255Array(void);
extern PACKAGE void __fastcall InformationDlg(const System::UnicodeString msg);
extern PACKAGE bool __fastcall QuestionDlg(const System::UnicodeString msg);
extern PACKAGE System::UnicodeString __fastcall InputDlg(const System::UnicodeString aCaption, const System::UnicodeString aPrompt, const System::UnicodeString aDefault);
extern PACKAGE bool __fastcall SavePictureDialog(System::UnicodeString &aFileName, const System::UnicodeString aTitle = System::UnicodeString());
extern PACKAGE bool __fastcall OpenPictureDialog(System::UnicodeString &aFileName, const System::UnicodeString aTitle = System::UnicodeString());
}	/* namespace Glutils */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLUTILS)
using namespace Glutils;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// GlutilsHPP
