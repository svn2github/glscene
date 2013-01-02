// CodeGear C++Builder
// Copyright (c) 1995, 2012 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLCrossPlatform.pas' rev: 24.00 (Win32)

#ifndef GlcrossplatformHPP
#define GlcrossplatformHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <Winapi.Windows.hpp>	// Pascal unit
#include <System.Classes.hpp>	// Pascal unit
#include <System.SysUtils.hpp>	// Pascal unit
#include <Vcl.Consts.hpp>	// Pascal unit
#include <Vcl.Graphics.hpp>	// Pascal unit
#include <Vcl.Controls.hpp>	// Pascal unit
#include <Vcl.Forms.hpp>	// Pascal unit
#include <Vcl.Dialogs.hpp>	// Pascal unit
#include <System.StrUtils.hpp>	// Pascal unit
#include <System.Types.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Glcrossplatform
{
//-- type declarations -------------------------------------------------------
typedef int ptrInt;

typedef unsigned PtrUInt;

typedef System::Types::TPoint TGLPoint;

typedef System::Types::TPoint *PGLPoint;

typedef System::Types::TRect TGLRect;

typedef System::Types::TRect *PGLRect;

typedef System::Uitypes::TColor TDelphiColor;

typedef Vcl::Graphics::TPicture TGLPicture;

typedef Vcl::Graphics::TGraphic TGLGraphic;

typedef Vcl::Graphics::TBitmap TGLBitmap;

typedef System::TMetaClass* TGraphicClass;

enum TGLTextLayout : unsigned char { tlTop, tlCenter, tlBottom };

enum TGLMouseButton : unsigned char { mbLeft, mbRight, mbMiddle };

typedef void __fastcall (__closure *TGLMouseEvent)(System::TObject* Sender, TGLMouseButton Button, System::Classes::TShiftState Shift, int X, int Y);

typedef Vcl::Controls::TMouseMoveEvent TGLMouseMoveEvent;

typedef Vcl::Controls::TKeyEvent TGLKeyEvent;

typedef Vcl::Controls::TKeyPressEvent TGLKeyPressEvent;

struct DECLSPEC_DRECORD TPlatformInfo
{
public:
	unsigned Major;
	unsigned Minor;
	unsigned Revision;
	System::UnicodeString Version;
	unsigned PlatformId;
	System::UnicodeString ID;
	System::UnicodeString CodeName;
	System::UnicodeString Description;
	System::UnicodeString ProductBuildVersion;
};


enum TPlatformVersion : unsigned char { pvUnknown, pvWin95, pvWin98, pvWinME, pvWinNT3, pvWinNT4, pvWin2000, pvWinXP, pvWin2003, pvWinVista, pvWinSeven, pvWin2008, pvWinNew, pvLinuxArc, pvLinuxDebian, pvLinuxopenSUSE, pvLinuxFedora, pvLinuxGentoo, pvLinuxMandriva, pvLinuxRedHat, pvLinuxTurboLinux, pvLinuxUbuntu, pvLinuxXandros, pvLinuxOracle, pvAppleMacOSX };

typedef System::Sysutils::EOSError EGLOSError;

class DELPHICLASS TGLComponent;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLComponent : public System::Classes::TComponent
{
	typedef System::Classes::TComponent inherited;
	
public:
	/* TComponent.Create */ inline __fastcall virtual TGLComponent(System::Classes::TComponent* AOwner) : System::Classes::TComponent(AOwner) { }
	/* TComponent.Destroy */ inline __fastcall virtual ~TGLComponent(void) { }
	
};

#pragma pack(pop)

typedef System::UnicodeString __fastcall (*TProjectTargetNameFunc)(void);

typedef System::Word THalfFloat;

typedef THalfFloat *PHalfFloat;

//-- var, const, procedure ---------------------------------------------------
static const System::Int8 FPC_VERSION = System::Int8(0x0);
static const System::Int8 FPC_RELEASE = System::Int8(0x0);
static const System::Int8 FPC_PATCH = System::Int8(0x0);
static const System::Int8 LCL_RELEASE = System::Int8(0x0);
static const Vcl::Graphics::TPixelFormat glpf8Bit = (Vcl::Graphics::TPixelFormat)(3);
static const Vcl::Graphics::TPixelFormat glpf24bit = (Vcl::Graphics::TPixelFormat)(6);
static const Vcl::Graphics::TPixelFormat glpf32Bit = (Vcl::Graphics::TPixelFormat)(7);
static const Vcl::Graphics::TPixelFormat glpfDevice = (Vcl::Graphics::TPixelFormat)(0);
static const System::Int8 glKey_TAB = System::Int8(0x9);
static const System::Int8 glKey_SPACE = System::Int8(0x20);
static const System::Int8 glKey_RETURN = System::Int8(0xd);
static const System::Int8 glKey_DELETE = System::Int8(0x2e);
static const System::Int8 glKey_LEFT = System::Int8(0x25);
static const System::Int8 glKey_RIGHT = System::Int8(0x27);
static const System::Int8 glKey_HOME = System::Int8(0x24);
static const System::Int8 glKey_END = System::Int8(0x23);
static const System::Int8 glKey_CANCEL = System::Int8(0x3);
static const System::Int8 glKey_UP = System::Int8(0x26);
static const System::Int8 glKey_DOWN = System::Int8(0x28);
static const System::Int8 glKey_PRIOR = System::Int8(0x21);
static const System::Int8 glKey_NEXT = System::Int8(0x22);
static const System::Int8 glKey_CONTROL = System::Int8(0x11);
extern PACKAGE System::UnicodeString glsAllFilter;
static const System::Word GLS_FONT_CHARS_COUNT = System::Word(0x7e8);
extern PACKAGE bool IsDesignTime;
extern PACKAGE TProjectTargetNameFunc vProjectTargetName;
extern PACKAGE bool __fastcall IsSubComponent(System::Classes::TComponent* const AComponent);
extern PACKAGE void __fastcall MakeSubComponent(System::Classes::TComponent* const AComponent, const bool Value);
extern PACKAGE bool __fastcall AnsiStartsText(const System::UnicodeString ASubText, const System::UnicodeString AText);
extern PACKAGE int __fastcall GLOKMessageBox(const System::UnicodeString Text, const System::UnicodeString Caption);
extern PACKAGE void __fastcall GLLoadBitmapFromInstance(int Instance, Vcl::Graphics::TBitmap* ABitmap, System::UnicodeString AName);
extern PACKAGE __int64 __fastcall GLGetTickCount(void);
extern PACKAGE void __fastcall ShowHTMLUrl(System::UnicodeString Url);
extern PACKAGE System::Types::TPoint __fastcall GLPoint(const int x, const int y);
extern PACKAGE void __fastcall InflateGLRect(System::Types::TRect &aRect, int dx, int dy);
extern PACKAGE void __fastcall IntersectGLRect(System::Types::TRect &aRect, const System::Types::TRect &rect2);
extern PACKAGE void __fastcall RaiseLastOSError(void);
extern PACKAGE int __fastcall GetDeviceLogicalPixelsX(HDC device);
extern PACKAGE int __fastcall GetCurrentColorDepth(void);
extern PACKAGE int __fastcall PixelFormatToColorBits(Vcl::Graphics::TPixelFormat aPixelFormat);
extern PACKAGE void * __fastcall BitmapScanLine(Vcl::Graphics::TBitmap* aBitmap, int aRow);
extern PACKAGE void __fastcall FixPathDelimiter(System::UnicodeString &S);
extern PACKAGE System::UnicodeString __fastcall RelativePath(const System::UnicodeString S);
extern PACKAGE void __fastcall QueryPerformanceCounter(__int64 &val);
extern PACKAGE bool __fastcall QueryPerformanceFrequency(__int64 &val);
extern PACKAGE __int64 __fastcall StartPrecisionTimer(void);
extern PACKAGE double __fastcall PrecisionTimerLap(const __int64 precisionTimer);
extern PACKAGE double __fastcall StopPrecisionTimer(const __int64 precisionTimer);
extern PACKAGE double __fastcall GLSTime(void);
extern PACKAGE __int64 __fastcall RDTSC(void);
extern PACKAGE System::UnicodeString __fastcall FindUnitName(System::TObject* anObject)/* overload */;
extern PACKAGE System::UnicodeString __fastcall FindUnitName(System::TClass aClass)/* overload */;
extern PACKAGE void __fastcall SetExeDirectory(void);
extern PACKAGE System::WideChar __fastcall GetDecimalSeparator(void);
extern PACKAGE void __fastcall SetDecimalSeparator(System::WideChar AValue);
extern PACKAGE float __fastcall HalfToFloat(THalfFloat Half);
extern PACKAGE THalfFloat __fastcall FloatToHalf(float Float);
extern PACKAGE System::UnicodeString __fastcall GetValueFromStringsIndex(System::Classes::TStrings* const AStrings, const int AIndex);
extern PACKAGE TPlatformInfo __fastcall GetPlatformInfo(void);
extern PACKAGE TPlatformVersion __fastcall GetPlatformVersion(void);
extern PACKAGE System::UnicodeString __fastcall GetPlatformVersionStr(void);
extern PACKAGE bool __fastcall IsDirectoryWriteable(const System::UnicodeString AName);
extern PACKAGE System::WideChar __fastcall CharToWideChar(const char AChar);
extern PACKAGE bool __fastcall PtInRect(const System::Types::TRect &Rect, const System::Types::TPoint &P);
}	/* namespace Glcrossplatform */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLCROSSPLATFORM)
using namespace Glcrossplatform;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// GlcrossplatformHPP
