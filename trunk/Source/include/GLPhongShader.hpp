// CodeGear C++Builder
// Copyright (c) 1995, 2012 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLPhongShader.pas' rev: 24.00 (Win32)

#ifndef GlphongshaderHPP
#define GlphongshaderHPP

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
#include <VectorGeometry.hpp>	// Pascal unit
#include <VectorLists.hpp>	// Pascal unit
#include <OpenGLTokens.hpp>	// Pascal unit
#include <GLContext.hpp>	// Pascal unit
#include <GLAsmShader.hpp>	// Pascal unit
#include <GLRenderContextInfo.hpp>	// Pascal unit
#include <GLCustomShader.hpp>	// Pascal unit
#include <GLState.hpp>	// Pascal unit
#include <GLMaterial.hpp>	// Pascal unit
#include <BaseClasses.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Glphongshader
{
//-- type declarations -------------------------------------------------------
class DELPHICLASS TGLPhongShader;
class PASCALIMPLEMENTATION TGLPhongShader : public Glasmshader::TGLCustomAsmShader
{
	typedef Glasmshader::TGLCustomAsmShader inherited;
	
private:
	Vectorlists::TIntegerList* FLightIDs;
	bool FDesignTimeEnabled;
	bool FAmbientPass;
	void __fastcall SetDesignTimeEnabled(const bool Value);
	
protected:
	virtual void __fastcall DoLightPass(unsigned lightID);
	virtual void __fastcall DoAmbientPass(Glrendercontextinfo::TRenderContextInfo &rci);
	virtual void __fastcall UnApplyLights(Glrendercontextinfo::TRenderContextInfo &rci);
	virtual void __fastcall DoApply(Glrendercontextinfo::TRenderContextInfo &rci, System::TObject* Sender);
	virtual bool __fastcall DoUnApply(Glrendercontextinfo::TRenderContextInfo &rci);
	DYNAMIC void __fastcall DoInitialize(Glrendercontextinfo::TRenderContextInfo &rci, System::TObject* Sender);
	
public:
	__fastcall virtual TGLPhongShader(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TGLPhongShader(void);
	virtual bool __fastcall ShaderSupported(void);
	
__published:
	__property bool DesignTimeEnabled = {read=FDesignTimeEnabled, write=SetDesignTimeEnabled, default=0};
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Glphongshader */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLPHONGSHADER)
using namespace Glphongshader;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// GlphongshaderHPP
