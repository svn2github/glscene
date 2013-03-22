// CodeGear C++Builder
// Copyright (c) 1995, 2012 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLFileJPEG.pas' rev: 24.00 (Win32)

#ifndef GlfilejpegHPP
#define GlfilejpegHPP

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

namespace Glfilejpeg
{
//-- type declarations -------------------------------------------------------
class DELPHICLASS TGLJPEGImage;
class PASCALIMPLEMENTATION TGLJPEGImage : public Glgraphics::TGLBaseImage
{
	typedef Glgraphics::TGLBaseImage inherited;
	
private:
	bool FAbortLoading;
	unsigned FDivScale;
	bool FDither;
	bool FSmoothing;
	bool FProgressiveEncoding;
	void __fastcall SetSmoothing(const bool AValue);
	
public:
	__fastcall virtual TGLJPEGImage(void);
	__classmethod virtual Applicationfileio::TDataFileCapabilities __fastcall Capabilities();
	DYNAMIC void __fastcall LoadFromFile(const System::UnicodeString filename);
	DYNAMIC void __fastcall SaveToFile(const System::UnicodeString filename);
	DYNAMIC void __fastcall LoadFromStream(System::Classes::TStream* stream);
	DYNAMIC void __fastcall SaveToStream(System::Classes::TStream* stream);
	HIDESBASE void __fastcall AssignFromTexture(Glcontext::TGLContext* textureContext, const unsigned textureHandle, Gltextureformat::TGLTextureTarget textureTarget, const bool CurrentFormat, const Gltextureformat::TGLInternalFormat intFormat);
	__property unsigned DivScale = {read=FDivScale, write=FDivScale, nodefault};
	__property bool Dither = {read=FDither, write=FDither, nodefault};
	__property bool Smoothing = {read=FSmoothing, write=SetSmoothing, nodefault};
	__property bool ProgressiveEncoding = {read=FProgressiveEncoding, nodefault};
public:
	/* TGLBaseImage.Destroy */ inline __fastcall virtual ~TGLJPEGImage(void) { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Glfilejpeg */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLFILEJPEG)
using namespace Glfilejpeg;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// GlfilejpegHPP
