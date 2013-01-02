// CodeGear C++Builder
// Copyright (c) 1995, 2012 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLMultiMaterialShader.pas' rev: 24.00 (Win32)

#ifndef GlmultimaterialshaderHPP
#define GlmultimaterialshaderHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <System.Classes.hpp>	// Pascal unit
#include <GLMaterial.hpp>	// Pascal unit
#include <GLRenderContextInfo.hpp>	// Pascal unit
#include <GLState.hpp>	// Pascal unit
#include <BaseClasses.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Glmultimaterialshader
{
//-- type declarations -------------------------------------------------------
class DELPHICLASS TGLMultiMaterialShader;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLMultiMaterialShader : public Glmaterial::TGLShader
{
	typedef Glmaterial::TGLShader inherited;
	
private:
	int FPass;
	Glmaterial::TGLMaterialLibrary* FMaterialLibrary;
	bool FVisibleAtDesignTime;
	bool FShaderActiveAtDesignTime;
	Glmaterial::TGLShaderStyle FShaderStyle;
	void __fastcall SetVisibleAtDesignTime(const bool Value);
	void __fastcall SetShaderStyle(const Glmaterial::TGLShaderStyle Value);
	
protected:
	void __fastcall SetMaterialLibrary(Glmaterial::TGLMaterialLibrary* const val);
	virtual void __fastcall DoApply(Glrendercontextinfo::TRenderContextInfo &rci, System::TObject* Sender);
	virtual bool __fastcall DoUnApply(Glrendercontextinfo::TRenderContextInfo &rci);
	
public:
	__fastcall virtual TGLMultiMaterialShader(System::Classes::TComponent* aOwner);
	
__published:
	__property Glmaterial::TGLMaterialLibrary* MaterialLibrary = {read=FMaterialLibrary, write=SetMaterialLibrary};
	__property bool VisibleAtDesignTime = {read=FVisibleAtDesignTime, write=SetVisibleAtDesignTime, nodefault};
	__property Glmaterial::TGLShaderStyle ShaderStyle = {read=FShaderStyle, write=SetShaderStyle, nodefault};
public:
	/* TGLShader.Destroy */ inline __fastcall virtual ~TGLMultiMaterialShader(void) { }
	
};

#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------
}	/* namespace Glmultimaterialshader */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLMULTIMATERIALSHADER)
using namespace Glmultimaterialshader;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// GlmultimaterialshaderHPP
