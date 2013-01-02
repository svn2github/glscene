// CodeGear C++Builder
// Copyright (c) 1995, 2012 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLCompositeImage.pas' rev: 24.00 (Win32)

#ifndef GlcompositeimageHPP
#define GlcompositeimageHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <System.Classes.hpp>	// Pascal unit
#include <OpenGLTokens.hpp>	// Pascal unit
#include <GLGraphics.hpp>	// Pascal unit
#include <GLTexture.hpp>	// Pascal unit
#include <GLTextureFormat.hpp>	// Pascal unit
#include <BaseClasses.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Glcompositeimage
{
//-- type declarations -------------------------------------------------------
class DELPHICLASS TGLCompositeImage;
class PASCALIMPLEMENTATION TGLCompositeImage : public Gltexture::TGLTextureImage
{
	typedef Gltexture::TGLTextureImage inherited;
	
private:
	Glgraphics::TGLImage* FBitmap;
	int FWidth;
	int FHeight;
	int FDepth;
	
protected:
	void __fastcall SetWidth(int val);
	void __fastcall SetHeight(int val);
	void __fastcall SetDepth(int val);
	virtual int __fastcall GetWidth(void);
	virtual int __fastcall GetHeight(void);
	virtual int __fastcall GetDepth(void);
	virtual Gltextureformat::TGLTextureTarget __fastcall GetTextureTarget(void);
	
public:
	__fastcall virtual TGLCompositeImage(System::Classes::TPersistent* AOwner);
	__fastcall virtual ~TGLCompositeImage(void);
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	virtual Glgraphics::TGLImage* __fastcall GetBitmap32(void);
	virtual void __fastcall ReleaseBitmap32(void);
	DYNAMIC void __fastcall SaveToFile(const System::UnicodeString fileName);
	DYNAMIC void __fastcall LoadFromFile(const System::UnicodeString fileName);
	void __fastcall LoadFromStream(System::Classes::TStream* const AStream);
	__classmethod virtual System::UnicodeString __fastcall FriendlyName();
	__classmethod virtual System::UnicodeString __fastcall FriendlyDescription();
	__property NativeTextureTarget;
	
__published:
	__property int Width = {read=GetWidth, write=SetWidth, nodefault};
	__property int Height = {read=GetHeight, write=SetHeight, nodefault};
	__property int Depth = {read=GetDepth, write=SetDepth, nodefault};
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Glcompositeimage */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLCOMPOSITEIMAGE)
using namespace Glcompositeimage;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// GlcompositeimageHPP
