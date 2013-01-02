// CodeGear C++Builder
// Copyright (c) 1995, 2012 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLFileBMP.pas' rev: 24.00 (Win32)

#ifndef GlfilebmpHPP
#define GlfilebmpHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <System.Classes.hpp>	// Pascal unit
#include <System.SysUtils.hpp>	// Pascal unit
#include <GLCrossPlatform.hpp>	// Pascal unit
#include <OpenGLTokens.hpp>	// Pascal unit
#include <GLContext.hpp>	// Pascal unit
#include <GLGraphics.hpp>	// Pascal unit
#include <GLTextureFormat.hpp>	// Pascal unit
#include <ApplicationFileIO.hpp>	// Pascal unit
#include <BaseClasses.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Glfilebmp
{
//-- type declarations -------------------------------------------------------
class DELPHICLASS TGLBMPImage;
class PASCALIMPLEMENTATION TGLBMPImage : public Glgraphics::TGLBaseImage
{
	typedef Glgraphics::TGLBaseImage inherited;
	
private:
	bool FTopDown;
	unsigned RedMask;
	unsigned GreenMask;
	unsigned BlueMask;
	System::Int8 RedShift;
	System::Int8 GreenShift;
	System::Int8 BlueShift;
	System::Sysutils::TByteArray *FLineBuffer;
	int FReadSize;
	int FDeltaX;
	int FDeltaY;
	System::Int8 __fastcall CountBits(System::Byte Value);
	System::Int8 __fastcall ShiftCount(unsigned Mask);
	Glgraphics::TGLPixel32 __fastcall ExpandColor(unsigned Value);
	void __fastcall ExpandRLE4ScanLine(int Row, System::Classes::TStream* Stream);
	void __fastcall ExpandRLE8ScanLine(int Row, System::Classes::TStream* Stream);
	int __fastcall Monochrome(int N);
	int __fastcall Quadrochrome(int N);
	int __fastcall Octochrome(int N);
	
public:
	DYNAMIC void __fastcall LoadFromFile(const System::UnicodeString filename);
	DYNAMIC void __fastcall SaveToFile(const System::UnicodeString filename);
	DYNAMIC void __fastcall LoadFromStream(System::Classes::TStream* stream);
	DYNAMIC void __fastcall SaveToStream(System::Classes::TStream* stream);
	__classmethod virtual Applicationfileio::TDataFileCapabilities __fastcall Capabilities();
	HIDESBASE void __fastcall AssignFromTexture(Glcontext::TGLContext* textureContext, const unsigned textureHandle, Gltextureformat::TGLTextureTarget textureTarget, const bool CurrentFormat, const Gltextureformat::TGLInternalFormat intFormat);
public:
	/* TGLBaseImage.Create */ inline __fastcall virtual TGLBMPImage(void) : Glgraphics::TGLBaseImage() { }
	/* TGLBaseImage.Destroy */ inline __fastcall virtual ~TGLBMPImage(void) { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Glfilebmp */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLFILEBMP)
using namespace Glfilebmp;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// GlfilebmpHPP
