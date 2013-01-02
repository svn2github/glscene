// CodeGear C++Builder
// Copyright (c) 1995, 2012 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'RGBE.pas' rev: 24.00 (Win32)

#ifndef RgbeHPP
#define RgbeHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <System.Classes.hpp>	// Pascal unit
#include <VectorTypes.hpp>	// Pascal unit
#include <VectorGeometry.hpp>	// Pascal unit
#include <GLCrossPlatform.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Rgbe
{
//-- type declarations -------------------------------------------------------
//-- var, const, procedure ---------------------------------------------------
extern PACKAGE void __fastcall Float2rgbe(Vectortypes::TVector4b &RGBE, const float Red, const float Green, const float Blue);
extern PACKAGE void __fastcall Rgbe2float(float &Red, float &Green, float &Blue, const Vectortypes::TVector4b RGBE);
extern PACKAGE void __fastcall LoadRLEpixels(System::Classes::TStream* Stream, System::PSingle Dst, int Scanline_width, int Num_scanlines);
extern PACKAGE void __fastcall LoadRGBEpixels(System::Classes::TStream* Stream, System::PSingle Dst, int Numpixels);
}	/* namespace Rgbe */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_RGBE)
using namespace Rgbe;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// RgbeHPP
