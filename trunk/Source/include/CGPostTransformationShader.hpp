// CodeGear C++Builder
// Copyright (c) 1995, 2012 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'CGPostTransformationShader.pas' rev: 24.00 (Win32)

#ifndef CgposttransformationshaderHPP
#define CgposttransformationshaderHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <System.Classes.hpp>	// Pascal unit
#include <System.SysUtils.hpp>	// Pascal unit
#include <GLTexture.hpp>	// Pascal unit
#include <GLCadencer.hpp>	// Pascal unit
#include <GLContext.hpp>	// Pascal unit
#include <OpenGLTokens.hpp>	// Pascal unit
#include <GLScene.hpp>	// Pascal unit
#include <GLCustomShader.hpp>	// Pascal unit
#include <GLRenderContextInfo.hpp>	// Pascal unit
#include <GLTextureFormat.hpp>	// Pascal unit
#include <cg.hpp>	// Pascal unit
#include <cgGL.hpp>	// Pascal unit
#include <GLCgShader.hpp>	// Pascal unit
#include <GLMaterial.hpp>	// Pascal unit
#include <BaseClasses.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Cgposttransformationshader
{
//-- type declarations -------------------------------------------------------
class DELPHICLASS TGLCustomCGPostTransformationShader;
class PASCALIMPLEMENTATION TGLCustomCGPostTransformationShader : public Glcgshader::TCustomCgShader
{
	typedef Glcgshader::TCustomCgShader inherited;
	
private:
	float FTransformationPower;
	Gltexture::TGLTexture* FTransformationTexture;
	
protected:
	virtual void __fastcall DoApply(Glrendercontextinfo::TRenderContextInfo &rci, System::TObject* Sender);
	void __fastcall DoUseTempTexture(Glcontext::TGLTextureHandle* const TempTexture, Gltextureformat::TGLTextureTarget TextureTarget);
	Gltextureformat::TGLTextureTarget __fastcall GetTextureTarget(void);
	
public:
	__fastcall virtual TGLCustomCGPostTransformationShader(System::Classes::TComponent* AOwner);
	__property float TransformationPower = {read=FTransformationPower, write=FTransformationPower};
	__property Gltexture::TGLTexture* TransformationTexture = {read=FTransformationTexture, write=FTransformationTexture};
public:
	/* TCustomCgShader.Destroy */ inline __fastcall virtual ~TGLCustomCGPostTransformationShader(void) { }
	
private:
	void *__IGLPostShader;	/* Glcustomshader::IGLPostShader */
	
public:
	#if defined(MANAGED_INTERFACE_OPERATORS)
	// {68A62362-AF0A-4CE8-A9E1-714FE02AFA4A}
	operator Glcustomshader::_di_IGLPostShader()
	{
		Glcustomshader::_di_IGLPostShader intf;
		GetInterface(intf);
		return intf;
	}
	#else
	operator Glcustomshader::IGLPostShader*(void) { return (Glcustomshader::IGLPostShader*)&__IGLPostShader; }
	#endif
	
};


class DELPHICLASS TGLCGPostTransformationShader;
class PASCALIMPLEMENTATION TGLCGPostTransformationShader : public TGLCustomCGPostTransformationShader
{
	typedef TGLCustomCGPostTransformationShader inherited;
	
__published:
	__property TransformationPower = {default=0};
	__property TransformationTexture;
public:
	/* TGLCustomCGPostTransformationShader.Create */ inline __fastcall virtual TGLCGPostTransformationShader(System::Classes::TComponent* AOwner) : TGLCustomCGPostTransformationShader(AOwner) { }
	
public:
	/* TCustomCgShader.Destroy */ inline __fastcall virtual ~TGLCGPostTransformationShader(void) { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Cgposttransformationshader */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_CGPOSTTRANSFORMATIONSHADER)
using namespace Cgposttransformationshader;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// CgposttransformationshaderHPP
