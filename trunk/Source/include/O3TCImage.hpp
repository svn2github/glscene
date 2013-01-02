// CodeGear C++Builder
// Copyright (c) 1995, 2012 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'O3TCImage.pas' rev: 24.00 (Win32)

#ifndef O3tcimageHPP
#define O3tcimageHPP

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
#include <GLCrossPlatform.hpp>	// Pascal unit
#include <VectorGeometry.hpp>	// Pascal unit
#include <GLGraphics.hpp>	// Pascal unit
#include <OpenGLTokens.hpp>	// Pascal unit
#include <Vcl.Graphics.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace O3tcimage
{
//-- type declarations -------------------------------------------------------
class DELPHICLASS TO3TCImage;
class PASCALIMPLEMENTATION TO3TCImage : public Vcl::Graphics::TBitmap
{
	typedef Vcl::Graphics::TBitmap inherited;
	
public:
	virtual void __fastcall LoadFromStream(System::Classes::TStream* stream);
	virtual void __fastcall SaveToStream(System::Classes::TStream* stream);
public:
	/* TBitmap.Create */ inline __fastcall virtual TO3TCImage(void) : Vcl::Graphics::TBitmap() { }
	/* TBitmap.Destroy */ inline __fastcall virtual ~TO3TCImage(void) { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace O3tcimage */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_O3TCIMAGE)
using namespace O3tcimage;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// O3tcimageHPP
