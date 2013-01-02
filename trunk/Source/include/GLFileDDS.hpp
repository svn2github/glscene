// CodeGear C++Builder
// Copyright (c) 1995, 2012 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLFileDDS.pas' rev: 24.00 (Win32)

#ifndef GlfileddsHPP
#define GlfileddsHPP

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
#include <RGBE.hpp>	// Pascal unit
#include <ApplicationFileIO.hpp>	// Pascal unit
#include <BaseClasses.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Glfiledds
{
//-- type declarations -------------------------------------------------------
enum TGLDDSDetailLevels : unsigned char { ddsHighDet, ddsMediumDet, ddsLowDet };

class DELPHICLASS TGLDDSImage;
class PASCALIMPLEMENTATION TGLDDSImage : public Glgraphics::TGLBaseImage
{
	typedef Glgraphics::TGLBaseImage inherited;
	
private:
	void __fastcall flipSurface(System::PByte chgData, int w, int h, int d);
	
public:
	__classmethod virtual Applicationfileio::TDataFileCapabilities __fastcall Capabilities();
	DYNAMIC void __fastcall LoadFromFile(const System::UnicodeString filename);
	DYNAMIC void __fastcall SaveToFile(const System::UnicodeString filename);
	DYNAMIC void __fastcall LoadFromStream(System::Classes::TStream* stream);
	DYNAMIC void __fastcall SaveToStream(System::Classes::TStream* stream);
	HIDESBASE void __fastcall AssignFromTexture(Glcontext::TGLContext* textureContext, const unsigned textureHandle, Gltextureformat::TGLTextureTarget textureTarget, const bool CurrentFormat, const Gltextureformat::TGLInternalFormat intFormat);
public:
	/* TGLBaseImage.Create */ inline __fastcall virtual TGLDDSImage(void) : Glgraphics::TGLBaseImage() { }
	/* TGLBaseImage.Destroy */ inline __fastcall virtual ~TGLDDSImage(void) { }
	
};


//-- var, const, procedure ---------------------------------------------------
extern PACKAGE TGLDDSDetailLevels vDDSDetailLevel;
}	/* namespace Glfiledds */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLFILEDDS)
using namespace Glfiledds;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// GlfileddsHPP
