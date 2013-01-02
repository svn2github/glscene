// CodeGear C++Builder
// Copyright (c) 1995, 2012 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLCelShader.pas' rev: 24.00 (Win32)

#ifndef GlcelshaderHPP
#define GlcelshaderHPP

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
#include <GLContext.hpp>	// Pascal unit
#include <GLGraphics.hpp>	// Pascal unit
#include <GLUtils.hpp>	// Pascal unit
#include <VectorGeometry.hpp>	// Pascal unit
#include <OpenGLTokens.hpp>	// Pascal unit
#include <GLColor.hpp>	// Pascal unit
#include <GLRenderContextInfo.hpp>	// Pascal unit
#include <GLMaterial.hpp>	// Pascal unit
#include <GLState.hpp>	// Pascal unit
#include <GLTextureFormat.hpp>	// Pascal unit
#include <BaseClasses.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Glcelshader
{
//-- type declarations -------------------------------------------------------
enum TGLCelShaderOption : unsigned char { csoOutlines, csoTextured, csoNoBuildShadeTexture };

typedef System::Set<TGLCelShaderOption, TGLCelShaderOption::csoOutlines, TGLCelShaderOption::csoNoBuildShadeTexture>  TGLCelShaderOptions;

typedef void __fastcall (__closure *TGLCelShaderGetIntensity)(System::TObject* Sender, System::Byte &intensity);

class DELPHICLASS TGLCelShader;
class PASCALIMPLEMENTATION TGLCelShader : public Glmaterial::TGLShader
{
	typedef Glmaterial::TGLShader inherited;
	
private:
	float FOutlineWidth;
	TGLCelShaderOptions FCelShaderOptions;
	Glcontext::TGLARBVertexProgramHandle* FVPHandle;
	Gltexture::TGLTexture* FShadeTexture;
	TGLCelShaderGetIntensity FOnGetIntensity;
	bool FOutlinePass;
	bool FUnApplyShadeTexture;
	Glcolor::TGLColor* FOutlineColor;
	
protected:
	void __fastcall SetCelShaderOptions(const TGLCelShaderOptions val);
	void __fastcall SetOutlineWidth(const float val);
	void __fastcall SetOutlineColor(Glcolor::TGLColor* const val);
	void __fastcall BuildShadeTexture(void);
	virtual void __fastcall Loaded(void);
	System::UnicodeString __fastcall GenerateVertexProgram(void);
	
public:
	__fastcall virtual TGLCelShader(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TGLCelShader(void);
	virtual void __fastcall DoApply(Glrendercontextinfo::TRenderContextInfo &rci, System::TObject* Sender);
	virtual bool __fastcall DoUnApply(Glrendercontextinfo::TRenderContextInfo &rci);
	__property Gltexture::TGLTexture* ShadeTexture = {read=FShadeTexture};
	
__published:
	__property TGLCelShaderOptions CelShaderOptions = {read=FCelShaderOptions, write=SetCelShaderOptions, nodefault};
	__property Glcolor::TGLColor* OutlineColor = {read=FOutlineColor, write=SetOutlineColor};
	__property float OutlineWidth = {read=FOutlineWidth, write=SetOutlineWidth};
	__property TGLCelShaderGetIntensity OnGetIntensity = {read=FOnGetIntensity, write=FOnGetIntensity};
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Glcelshader */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLCELSHADER)
using namespace Glcelshader;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// GlcelshaderHPP
