// CodeGear C++Builder
// Copyright (c) 1995, 2012 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLFBO.pas' rev: 24.00 (Win32)

#ifndef GlfboHPP
#define GlfboHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <System.SysUtils.hpp>	// Pascal unit
#include <OpenGLTokens.hpp>	// Pascal unit
#include <GLScene.hpp>	// Pascal unit
#include <GLContext.hpp>	// Pascal unit
#include <GLState.hpp>	// Pascal unit
#include <GLTexture.hpp>	// Pascal unit
#include <GLColor.hpp>	// Pascal unit
#include <GLRenderContextInfo.hpp>	// Pascal unit
#include <GLMultisampleImage.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Glfbo
{
//-- type declarations -------------------------------------------------------
class DELPHICLASS TGLRenderbuffer;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLRenderbuffer : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	Glcontext::TGLRenderbufferHandle* FRenderbufferHandle;
	int FWidth;
	int FHeight;
	bool FStorageValid;
	unsigned __fastcall GetHandle(void);
	void __fastcall SetHeight(const int Value);
	void __fastcall SetWidth(const int Value);
	
protected:
	virtual unsigned __fastcall GetInternalFormat(void) = 0 ;
	void __fastcall InvalidateStorage(void);
	
public:
	__fastcall TGLRenderbuffer(void);
	__fastcall virtual ~TGLRenderbuffer(void);
	void __fastcall Bind(void);
	void __fastcall Unbind(void);
	__property unsigned Handle = {read=GetHandle, nodefault};
	__property int Width = {read=FWidth, write=SetWidth, nodefault};
	__property int Height = {read=FHeight, write=SetHeight, nodefault};
};

#pragma pack(pop)

class DELPHICLASS TGLDepthRBO;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLDepthRBO : public TGLRenderbuffer
{
	typedef TGLRenderbuffer inherited;
	
private:
	Glscene::TGLDepthPrecision FDepthPrecision;
	void __fastcall SetDepthPrecision(const Glscene::TGLDepthPrecision Value);
	
protected:
	virtual unsigned __fastcall GetInternalFormat(void);
	
public:
	__fastcall TGLDepthRBO(void);
	__property Glscene::TGLDepthPrecision DepthPrecision = {read=FDepthPrecision, write=SetDepthPrecision, nodefault};
public:
	/* TGLRenderbuffer.Destroy */ inline __fastcall virtual ~TGLDepthRBO(void) { }
	
};

#pragma pack(pop)

enum TGLStencilPrecision : unsigned char { spDefault, sp1bit, sp4bits, sp8bits, sp16bits };

class DELPHICLASS TGLStencilRBO;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLStencilRBO : public TGLRenderbuffer
{
	typedef TGLRenderbuffer inherited;
	
private:
	TGLStencilPrecision FStencilPrecision;
	void __fastcall SetStencilPrecision(const TGLStencilPrecision Value);
	
protected:
	virtual unsigned __fastcall GetInternalFormat(void);
	
public:
	__fastcall TGLStencilRBO(void);
	__property TGLStencilPrecision StencilPrecision = {read=FStencilPrecision, write=SetStencilPrecision, nodefault};
public:
	/* TGLRenderbuffer.Destroy */ inline __fastcall virtual ~TGLStencilRBO(void) { }
	
};

#pragma pack(pop)

class DELPHICLASS TGLFrameBuffer;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLFrameBuffer : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	Glcontext::TGLFramebufferHandle* FFrameBufferHandle;
	unsigned FTarget;
	int FWidth;
	int FHeight;
	int FLayer;
	int FLevel;
	unsigned FTextureMipmap;
	System::StaticArray<Gltexture::TGLTexture*, 32> FAttachedTexture;
	Gltexture::TGLTexture* FDepthTexture;
	TGLDepthRBO* FDRBO;
	TGLStencilRBO* FSRBO;
	Glcontext::TGLFramebufferStatus __fastcall GetStatus(void);
	void __fastcall SetHeight(const int Value);
	void __fastcall SetWidth(const int Value);
	void __fastcall SetLayer(const int Value);
	void __fastcall SetLevel(const int Value);
	
protected:
	void __fastcall AttachTexture(const unsigned attachment, const unsigned textarget, const unsigned texture, const int level, const int layer)/* overload */;
	void __fastcall ReattachTextures(void);
	
public:
	__fastcall TGLFrameBuffer(void);
	__fastcall virtual ~TGLFrameBuffer(void);
	void __fastcall AttachDepthBuffer(TGLDepthRBO* DepthBuffer)/* overload */;
	void __fastcall DetachDepthBuffer(void);
	void __fastcall AttachStencilBuffer(TGLStencilRBO* StencilBuffer)/* overload */;
	void __fastcall DetachStencilBuffer(void);
	void __fastcall AttachDepthTexture(Gltexture::TGLTexture* Texture)/* overload */;
	void __fastcall DetachDepthTexture(void);
	void __fastcall AttachTexture(unsigned n, Gltexture::TGLTexture* Texture)/* overload */;
	void __fastcall DetachTexture(unsigned n);
	Glcontext::TGLFramebufferStatus __fastcall GetStringStatus(/* out */ System::UnicodeString &clarification);
	__property Glcontext::TGLFramebufferStatus Status = {read=GetStatus, nodefault};
	void __fastcall Bind(void);
	void __fastcall Unbind(void);
	void __fastcall PreRender(void);
	void __fastcall Render(Glrendercontextinfo::TRenderContextInfo &rci, Glscene::TGLBaseSceneObject* baseObject);
	void __fastcall PostRender(const bool PostGenerateMipmap);
	__property Glcontext::TGLFramebufferHandle* Handle = {read=FFrameBufferHandle};
	__property int Width = {read=FWidth, write=SetWidth, nodefault};
	__property int Height = {read=FHeight, write=SetHeight, nodefault};
	__property int Layer = {read=FLayer, write=SetLayer, nodefault};
	__property int Level = {read=FLevel, write=SetLevel, nodefault};
};

#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------
static const System::Int8 MaxColorAttachments = System::Int8(0x20);
}	/* namespace Glfbo */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLFBO)
using namespace Glfbo;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// GlfboHPP
