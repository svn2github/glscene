// CodeGear C++Builder
// Copyright (c) 1995, 2012 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLScreen.pas' rev: 24.00 (Win32)

#ifndef GlscreenHPP
#define GlscreenHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <Winapi.Windows.hpp>	// Pascal unit
#include <System.Classes.hpp>	// Pascal unit
#include <VectorGeometry.hpp>	// Pascal unit
#include <GLCrossPlatform.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Glscreen
{
//-- type declarations -------------------------------------------------------
typedef System::Byte TResolution;

enum TWindowAttribute : unsigned char { woDesktop, woStayOnTop, woTransparent };

typedef System::Set<TWindowAttribute, TWindowAttribute::woDesktop, TWindowAttribute::woTransparent>  TWindowAttributes;

enum TWindowFitting : unsigned char { wfDefault, wfFitWindowToScreen, wfFitScreenToWindow };

class DELPHICLASS TDisplayOptions;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TDisplayOptions : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
private:
	bool FFullScreen;
	TResolution FScreenResolution;
	TWindowAttributes FWindowAttributes;
	TWindowFitting FWindowFitting;
	
public:
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	
__published:
	__property bool FullScreen = {read=FFullScreen, write=FFullScreen, default=0};
	__property TResolution ScreenResolution = {read=FScreenResolution, write=FScreenResolution, default=0};
	__property TWindowAttributes WindowAttributes = {read=FWindowAttributes, write=FWindowAttributes, default=0};
	__property TWindowFitting WindowFitting = {read=FWindowFitting, write=FWindowFitting, default=0};
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TDisplayOptions(void) { }
	
public:
	/* TObject.Create */ inline __fastcall TDisplayOptions(void) : System::Classes::TPersistent() { }
	
};

#pragma pack(pop)

#pragma pack(push,1)
struct DECLSPEC_DRECORD TVideoMode
{
public:
	System::Word Width;
	System::Word Height;
	System::Byte ColorDepth;
	System::Byte MaxFrequency;
	System::UnicodeString Description;
};
#pragma pack(pop)


typedef TVideoMode *PVideoMode;

typedef System::DynamicArray<TVideoMode> Glscreen__2;

//-- var, const, procedure ---------------------------------------------------
static const System::Byte MaxVideoModes = System::Byte(0xc8);
static const System::Int8 lcl_release = System::Int8(0x0);
extern PACKAGE int vNumberVideoModes;
extern PACKAGE int vCurrentVideoMode;
extern PACKAGE Glscreen__2 vVideoModes;
extern PACKAGE TResolution __fastcall GetIndexFromResolution(int XRes, int YRes, int BPP);
extern PACKAGE void __fastcall ReadVideoModes(void);
extern PACKAGE bool __fastcall SetFullscreenMode(TResolution modeIndex, int displayFrequency = 0x0);
extern PACKAGE void __fastcall ReadScreenImage(HDC Dest, int DestLeft, int DestTop, const Vectorgeometry::TRectangle &SrcRect);
extern PACKAGE void __fastcall RestoreDefaultMode(void);
extern PACKAGE void __fastcall GLShowCursor(bool AShow);
extern PACKAGE void __fastcall GLSetCursorPos(int AScreenX, int AScreenY);
extern PACKAGE void __fastcall GLGetCursorPos(System::Types::TPoint &point);
extern PACKAGE int __fastcall GLGetScreenWidth(void);
extern PACKAGE int __fastcall GLGetScreenHeight(void);
}	/* namespace Glscreen */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLSCREEN)
using namespace Glscreen;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// GlscreenHPP
