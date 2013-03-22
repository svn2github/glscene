// CodeGear C++Builder
// Copyright (c) 1995, 2012 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLSLPostBlurShader.pas' rev: 24.00 (Win32)

#ifndef GlslpostblurshaderHPP
#define GlslpostblurshaderHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <System.Classes.hpp>	// Pascal unit
#include <GLTexture.hpp>	// Pascal unit
#include <GLScene.hpp>	// Pascal unit
#include <VectorGeometry.hpp>	// Pascal unit
#include <GLContext.hpp>	// Pascal unit
#include <GLSLShader.hpp>	// Pascal unit
#include <GLCustomShader.hpp>	// Pascal unit
#include <GLRenderContextInfo.hpp>	// Pascal unit
#include <GLTextureFormat.hpp>	// Pascal unit
#include <GLMaterial.hpp>	// Pascal unit
#include <BaseClasses.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Glslpostblurshader
{
//-- type declarations -------------------------------------------------------
class DELPHICLASS TGLCustomGLSLPostBlurShader;
class PASCALIMPLEMENTATION TGLCustomGLSLPostBlurShader : public Glslshader::TGLCustomGLSLShader
{
	typedef Glslshader::TGLCustomGLSLShader inherited;
	
private:
	float FThreshold;
	void __fastcall DoUseTempTexture(Glcontext::TGLTextureHandle* const TempTexture, Gltextureformat::TGLTextureTarget TextureTarget);
	Gltextureformat::TGLTextureTarget __fastcall GetTextureTarget(void);
	bool __fastcall StoreThreshold(void);
	
protected:
	virtual void __fastcall DoApply(Glrendercontextinfo::TRenderContextInfo &rci, System::TObject* Sender);
	virtual bool __fastcall DoUnApply(Glrendercontextinfo::TRenderContextInfo &rci);
	
public:
	__fastcall virtual TGLCustomGLSLPostBlurShader(System::Classes::TComponent* AOwner);
	__property float Threshold = {read=FThreshold, write=FThreshold, stored=StoreThreshold};
public:
	/* TGLCustomGLSLShader.Destroy */ inline __fastcall virtual ~TGLCustomGLSLPostBlurShader(void) { }
	
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


class DELPHICLASS TGLSLPostBlurShader;
class PASCALIMPLEMENTATION TGLSLPostBlurShader : public TGLCustomGLSLPostBlurShader
{
	typedef TGLCustomGLSLPostBlurShader inherited;
	
__published:
	__property Threshold = {default=0};
public:
	/* TGLCustomGLSLPostBlurShader.Create */ inline __fastcall virtual TGLSLPostBlurShader(System::Classes::TComponent* AOwner) : TGLCustomGLSLPostBlurShader(AOwner) { }
	
public:
	/* TGLCustomGLSLShader.Destroy */ inline __fastcall virtual ~TGLSLPostBlurShader(void) { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Glslpostblurshader */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLSLPOSTBLURSHADER)
using namespace Glslpostblurshader;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// GlslpostblurshaderHPP
