// CodeGear C++Builder
// Copyright (c) 1995, 2012 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLMultisampleImage.pas' rev: 24.00 (Win32)

#ifndef GlmultisampleimageHPP
#define GlmultisampleimageHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <System.Classes.hpp>	// Pascal unit
#include <OpenGLTokens.hpp>	// Pascal unit
#include <GLContext.hpp>	// Pascal unit
#include <GLTexture.hpp>	// Pascal unit
#include <GLGraphics.hpp>	// Pascal unit
#include <GLTextureFormat.hpp>	// Pascal unit
#include <BaseClasses.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Glmultisampleimage
{
//-- type declarations -------------------------------------------------------
class DELPHICLASS TGLMultisampleImage;
class PASCALIMPLEMENTATION TGLMultisampleImage : public Gltexture::TGLTextureImage
{
	typedef Gltexture::TGLTextureImage inherited;
	
private:
	Glgraphics::TGLImage* FBitmap;
	int FSamplesCount;
	int FWidth;
	int FHeight;
	int FDepth;
	bool FFixedSamplesLocation;
	void __fastcall SetWidth(int val);
	void __fastcall SetHeight(int val);
	void __fastcall SetDepth(int val);
	void __fastcall SetSamplesCount(int val);
	void __fastcall SetFixedSamplesLocation(bool val);
	
protected:
	virtual int __fastcall GetWidth(void);
	virtual int __fastcall GetHeight(void);
	virtual int __fastcall GetDepth(void);
	virtual Gltextureformat::TGLTextureTarget __fastcall GetTextureTarget(void);
	
public:
	__fastcall virtual TGLMultisampleImage(System::Classes::TPersistent* AOwner);
	__fastcall virtual ~TGLMultisampleImage(void);
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	__classmethod virtual bool __fastcall IsSelfLoading();
	virtual void __fastcall LoadTexture(Gltextureformat::TGLInternalFormat AInternalFormat);
	virtual Glgraphics::TGLImage* __fastcall GetBitmap32(void);
	virtual void __fastcall ReleaseBitmap32(void);
	DYNAMIC void __fastcall SaveToFile(const System::UnicodeString fileName);
	DYNAMIC void __fastcall LoadFromFile(const System::UnicodeString fileName);
	__classmethod virtual System::UnicodeString __fastcall FriendlyName();
	__classmethod virtual System::UnicodeString __fastcall FriendlyDescription();
	__property NativeTextureTarget;
	
__published:
	__property int Width = {read=GetWidth, write=SetWidth, default=256};
	__property int Height = {read=GetHeight, write=SetHeight, default=256};
	__property int Depth = {read=GetDepth, write=SetDepth, default=0};
	__property int SamplesCount = {read=FSamplesCount, write=SetSamplesCount, default=0};
	__property bool FixedSamplesLocation = {read=FFixedSamplesLocation, write=SetFixedSamplesLocation, nodefault};
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Glmultisampleimage */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLMULTISAMPLEIMAGE)
using namespace Glmultisampleimage;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// GlmultisampleimageHPP
