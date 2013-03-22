// CodeGear C++Builder
// Copyright (c) 1995, 2012 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLAsmShader.pas' rev: 24.00 (Win32)

#ifndef GlasmshaderHPP
#define GlasmshaderHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <System.Classes.hpp>	// Pascal unit
#include <System.SysUtils.hpp>	// Pascal unit
#include <VectorGeometry.hpp>	// Pascal unit
#include <VectorTypes.hpp>	// Pascal unit
#include <GLTexture.hpp>	// Pascal unit
#include <OpenGLTokens.hpp>	// Pascal unit
#include <GLContext.hpp>	// Pascal unit
#include <GLCustomShader.hpp>	// Pascal unit
#include <GLRenderContextInfo.hpp>	// Pascal unit
#include <GLMaterial.hpp>	// Pascal unit
#include <BaseClasses.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Glasmshader
{
//-- type declarations -------------------------------------------------------
class DELPHICLASS TGLCustomAsmShader;
typedef void __fastcall (__closure *TGLAsmShaderEvent)(TGLCustomAsmShader* Shader);

typedef void __fastcall (__closure *TGLAsmShaderUnUplyEvent)(TGLCustomAsmShader* Shader, bool &ThereAreMorePasses);

class DELPHICLASS TGLAsmShaderParameter;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLAsmShaderParameter : public Glcustomshader::TGLCustomShaderParameter
{
	typedef Glcustomshader::TGLCustomShaderParameter inherited;
	
public:
	/* TObject.Create */ inline __fastcall TGLAsmShaderParameter(void) : Glcustomshader::TGLCustomShaderParameter() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TGLAsmShaderParameter(void) { }
	
};

#pragma pack(pop)

class PASCALIMPLEMENTATION TGLCustomAsmShader : public Glcustomshader::TGLCustomShader
{
	typedef Glcustomshader::TGLCustomShader inherited;
	
private:
	Glcontext::TGLARBVertexProgramHandle* FVPHandle;
	Glcontext::TGLARBFragmentProgramHandle* FFPHandle;
	Glcontext::TGLARBGeometryProgramHandle* FGPHandle;
	TGLAsmShaderEvent FOnInitialize;
	TGLAsmShaderEvent FOnApply;
	TGLAsmShaderUnUplyEvent FOnUnApply;
	
protected:
	void __fastcall ApplyShaderPrograms(void);
	void __fastcall UnApplyShaderPrograms(void);
	virtual void __fastcall DestroyARBPrograms(void);
	__property TGLAsmShaderEvent OnApply = {read=FOnApply, write=FOnApply};
	__property TGLAsmShaderUnUplyEvent OnUnApply = {read=FOnUnApply, write=FOnUnApply};
	__property TGLAsmShaderEvent OnInitialize = {read=FOnInitialize, write=FOnInitialize};
	DYNAMIC void __fastcall DoInitialize(Glrendercontextinfo::TRenderContextInfo &rci, System::TObject* Sender);
	virtual void __fastcall DoApply(Glrendercontextinfo::TRenderContextInfo &rci, System::TObject* Sender);
	virtual bool __fastcall DoUnApply(Glrendercontextinfo::TRenderContextInfo &rci);
	DYNAMIC void __fastcall DoFinalize(void);
	
public:
	__fastcall virtual TGLCustomAsmShader(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TGLCustomAsmShader(void);
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	virtual bool __fastcall ShaderSupported(void);
};


class DELPHICLASS TGLAsmShader;
class PASCALIMPLEMENTATION TGLAsmShader : public TGLCustomAsmShader
{
	typedef TGLCustomAsmShader inherited;
	
__published:
	__property FragmentProgram;
	__property VertexProgram;
	__property GeometryProgram;
	__property OnApply;
	__property OnUnApply;
	__property OnInitialize;
	__property ShaderStyle = {default=1};
	__property FailedInitAction = {default=1};
public:
	/* TGLCustomAsmShader.Create */ inline __fastcall virtual TGLAsmShader(System::Classes::TComponent* AOwner) : TGLCustomAsmShader(AOwner) { }
	/* TGLCustomAsmShader.Destroy */ inline __fastcall virtual ~TGLAsmShader(void) { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Glasmshader */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLASMSHADER)
using namespace Glasmshader;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// GlasmshaderHPP
