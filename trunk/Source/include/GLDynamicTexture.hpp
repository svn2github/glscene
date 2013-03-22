// CodeGear C++Builder
// Copyright (c) 1995, 2012 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLDynamicTexture.pas' rev: 24.00 (Win32)

#ifndef GldynamictextureHPP
#define GldynamictextureHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <System.Classes.hpp>	// Pascal unit
#include <System.SysUtils.hpp>	// Pascal unit
#include <OpenGLTokens.hpp>	// Pascal unit
#include <GLContext.hpp>	// Pascal unit
#include <GLTexture.hpp>	// Pascal unit
#include <GLTextureFormat.hpp>	// Pascal unit
#include <GLGraphics.hpp>	// Pascal unit
#include <GLCrossPlatform.hpp>	// Pascal unit
#include <BaseClasses.hpp>	// Pascal unit
#include <System.Types.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Gldynamictexture
{
//-- type declarations -------------------------------------------------------
class DELPHICLASS TGLDynamicTextureImage;
class PASCALIMPLEMENTATION TGLDynamicTextureImage : public Gltexture::TGLBlankImage
{
	typedef Gltexture::TGLBlankImage inherited;
	
private:
	int FUpdating;
	int FTexSize;
	void *FBuffer;
	Glcontext::TGLBufferObjectHandle* FPBO;
	void *FData;
	System::Types::TRect FDirtyRect;
	bool FUseBGR;
	bool FUsePBO;
	void __fastcall SetDirtyRectangle(const System::Types::TRect &Value);
	void __fastcall SetUsePBO(const bool Value);
	
protected:
	int __fastcall GetTexSize(void);
	int __fastcall GetBitsPerPixel(void);
	int __fastcall GetDataFormat(void);
	int __fastcall GetTextureFormat(void);
	void __fastcall FreePBO(void);
	void __fastcall FreeBuffer(void);
	__property int BitsPerPixel = {read=GetBitsPerPixel, nodefault};
	__property int DataFormat = {read=GetDataFormat, nodefault};
	__property int TextureFormat = {read=GetTextureFormat, nodefault};
	
public:
	__fastcall virtual TGLDynamicTextureImage(System::Classes::TPersistent* AOwner);
	__classmethod virtual System::UnicodeString __fastcall FriendlyName();
	__classmethod virtual System::UnicodeString __fastcall FriendlyDescription();
	virtual void __fastcall NotifyChange(System::TObject* Sender);
	HIDESBASE void __fastcall BeginUpdate(void);
	HIDESBASE void __fastcall EndUpdate(void);
	__property void * Data = {read=FData};
	__property System::Types::TRect DirtyRectangle = {read=FDirtyRect, write=SetDirtyRectangle};
	__property bool UseBGR = {read=FUseBGR, write=FUseBGR, nodefault};
	__property bool UsePBO = {read=FUsePBO, write=SetUsePBO, nodefault};
public:
	/* TGLBlankImage.Destroy */ inline __fastcall virtual ~TGLDynamicTextureImage(void) { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Gldynamictexture */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLDYNAMICTEXTURE)
using namespace Gldynamictexture;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// GldynamictextureHPP
