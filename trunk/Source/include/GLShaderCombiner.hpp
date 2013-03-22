// CodeGear C++Builder
// Copyright (c) 1995, 2012 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLShaderCombiner.pas' rev: 24.00 (Win32)

#ifndef GlshadercombinerHPP
#define GlshadercombinerHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <System.Classes.hpp>	// Pascal unit
#include <GLMaterial.hpp>	// Pascal unit
#include <GLScene.hpp>	// Pascal unit
#include <VectorGeometry.hpp>	// Pascal unit
#include <GLStrings.hpp>	// Pascal unit
#include <GLRenderContextInfo.hpp>	// Pascal unit
#include <BaseClasses.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Glshadercombiner
{
//-- type declarations -------------------------------------------------------
enum TGLShaderCombinerType : unsigned char { sctOneSPTwoAP, sctTwoSPOneAP, sctOneMPTwoSP, sctTwoMPOneSP };

class DELPHICLASS TGLCustomShaderCombiner;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLCustomShaderCombiner : public Glmaterial::TGLShader
{
	typedef Glmaterial::TGLShader inherited;
	
private:
	int FCurrentPass;
	TGLShaderCombinerType FCombinerType;
	Glmaterial::TGLShader* FShaderOne;
	Glmaterial::TGLShader* FShaderTwo;
	void __fastcall SetShaderOne(Glmaterial::TGLShader* const Value);
	void __fastcall SetShaderTwo(Glmaterial::TGLShader* const Value);
	
protected:
	virtual void __fastcall DoApply(Glrendercontextinfo::TRenderContextInfo &rci, System::TObject* Sender);
	virtual bool __fastcall DoUnApply(Glrendercontextinfo::TRenderContextInfo &rci);
	virtual void __fastcall Notification(System::Classes::TComponent* AComponent, System::Classes::TOperation Operation);
	__property TGLShaderCombinerType CombinerType = {read=FCombinerType, write=FCombinerType, default=0};
	__property Glmaterial::TGLShader* ShaderOne = {read=FShaderOne, write=SetShaderOne};
	__property Glmaterial::TGLShader* ShaderTwo = {read=FShaderTwo, write=SetShaderTwo};
	__property int CurrentPass = {read=FCurrentPass, stored=false, nodefault};
	
public:
	__fastcall virtual TGLCustomShaderCombiner(System::Classes::TComponent* AOwner);
	virtual bool __fastcall ShaderSupported(void);
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
public:
	/* TGLShader.Destroy */ inline __fastcall virtual ~TGLCustomShaderCombiner(void) { }
	
};

#pragma pack(pop)

class DELPHICLASS TGLShaderCombiner;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLShaderCombiner : public TGLCustomShaderCombiner
{
	typedef TGLCustomShaderCombiner inherited;
	
__published:
	__property CombinerType = {default=0};
	__property ShaderOne;
	__property ShaderTwo;
	__property ShaderStyle = {default=1};
public:
	/* TGLCustomShaderCombiner.Create */ inline __fastcall virtual TGLShaderCombiner(System::Classes::TComponent* AOwner) : TGLCustomShaderCombiner(AOwner) { }
	
public:
	/* TGLShader.Destroy */ inline __fastcall virtual ~TGLShaderCombiner(void) { }
	
};

#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------
}	/* namespace Glshadercombiner */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLSHADERCOMBINER)
using namespace Glshadercombiner;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// GlshadercombinerHPP
