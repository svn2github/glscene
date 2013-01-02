// CodeGear C++Builder
// Copyright (c) 1995, 2012 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLBumpShader.pas' rev: 24.00 (Win32)

#ifndef GlbumpshaderHPP
#define GlbumpshaderHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <System.Classes.hpp>	// Pascal unit
#include <System.SysUtils.hpp>	// Pascal unit
#include <GLMaterial.hpp>	// Pascal unit
#include <GLGraphics.hpp>	// Pascal unit
#include <GLUtils.hpp>	// Pascal unit
#include <VectorGeometry.hpp>	// Pascal unit
#include <OpenGLTokens.hpp>	// Pascal unit
#include <GLContext.hpp>	// Pascal unit
#include <VectorLists.hpp>	// Pascal unit
#include <GLColor.hpp>	// Pascal unit
#include <GLRenderContextInfo.hpp>	// Pascal unit
#include <GLState.hpp>	// Pascal unit
#include <GLTextureFormat.hpp>	// Pascal unit
#include <BaseClasses.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Glbumpshader
{
//-- type declarations -------------------------------------------------------
enum TBumpMethod : unsigned char { bmDot3TexCombiner, bmBasicARBFP };

enum TBumpSpace : unsigned char { bsObject, bsTangentExternal, bsTangentQuaternion };

enum TBumpOption : unsigned char { boDiffuseTexture2, boSpecularTexture3, boUseSecondaryTexCoords, boLightAttenuation, boParallaxMapping };

typedef System::Set<TBumpOption, TBumpOption::boDiffuseTexture2, TBumpOption::boParallaxMapping>  TBumpOptions;

enum TSpecularMode : unsigned char { smOff, smBlinn, smPhong };

class DELPHICLASS TGLBumpShader;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLBumpShader : public Glmaterial::TGLShader
{
	typedef Glmaterial::TGLShader inherited;
	
private:
	Glcontext::TGLARBVertexProgramHandle* FVertexProgramHandle;
	Glcontext::TGLARBFragmentProgramHandle* FFragmentProgramHandle;
	Vectorlists::TIntegerList* FLightIDs;
	int FLightsEnabled;
	TBumpMethod FBumpMethod;
	TBumpSpace FBumpSpace;
	TBumpOptions FBumpOptions;
	TSpecularMode FSpecularMode;
	bool FDesignTimeEnabled;
	bool FAmbientPass;
	bool FDiffusePass;
	System::Classes::TStringList* FVertexProgram;
	System::Classes::TStringList* FFragmentProgram;
	float FParallaxOffset;
	System::UnicodeString __fastcall GenerateVertexProgram(void);
	System::UnicodeString __fastcall GenerateFragmentProgram(void);
	void __fastcall DoLightPass(Glrendercontextinfo::TRenderContextInfo &rci, unsigned lightID);
	
protected:
	void __fastcall SetBumpMethod(const TBumpMethod Value);
	void __fastcall SetBumpSpace(const TBumpSpace Value);
	void __fastcall SetBumpOptions(const TBumpOptions Value);
	void __fastcall SetSpecularMode(const TSpecularMode Value);
	void __fastcall SetDesignTimeEnabled(const bool Value);
	void __fastcall SetParallaxOffset(const float Value);
	virtual void __fastcall Loaded(void);
	void __fastcall DeleteVertexPrograms(void);
	void __fastcall DeleteFragmentPrograms(void);
	
public:
	__fastcall virtual TGLBumpShader(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TGLBumpShader(void);
	virtual void __fastcall DoApply(Glrendercontextinfo::TRenderContextInfo &rci, System::TObject* Sender);
	virtual bool __fastcall DoUnApply(Glrendercontextinfo::TRenderContextInfo &rci);
	
__published:
	__property TBumpMethod BumpMethod = {read=FBumpMethod, write=SetBumpMethod, nodefault};
	__property TBumpSpace BumpSpace = {read=FBumpSpace, write=SetBumpSpace, nodefault};
	__property TBumpOptions BumpOptions = {read=FBumpOptions, write=SetBumpOptions, nodefault};
	__property TSpecularMode SpecularMode = {read=FSpecularMode, write=SetSpecularMode, nodefault};
	__property bool DesignTimeEnabled = {read=FDesignTimeEnabled, write=SetDesignTimeEnabled, nodefault};
	__property float ParallaxOffset = {read=FParallaxOffset, write=SetParallaxOffset};
};

#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------
}	/* namespace Glbumpshader */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLBUMPSHADER)
using namespace Glbumpshader;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// GlbumpshaderHPP
