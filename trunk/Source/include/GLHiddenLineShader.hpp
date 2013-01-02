// CodeGear C++Builder
// Copyright (c) 1995, 2012 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLHiddenLineShader.pas' rev: 24.00 (Win32)

#ifndef GlhiddenlineshaderHPP
#define GlhiddenlineshaderHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <System.Classes.hpp>	// Pascal unit
#include <GLMaterial.hpp>	// Pascal unit
#include <OpenGLTokens.hpp>	// Pascal unit
#include <GLCrossPlatform.hpp>	// Pascal unit
#include <GLScene.hpp>	// Pascal unit
#include <GLColor.hpp>	// Pascal unit
#include <BaseClasses.hpp>	// Pascal unit
#include <GLRenderContextInfo.hpp>	// Pascal unit
#include <GLState.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Glhiddenlineshader
{
//-- type declarations -------------------------------------------------------
class DELPHICLASS TGLLineSettings;
class PASCALIMPLEMENTATION TGLLineSettings : public Baseclasses::TGLUpdateAbleObject
{
	typedef Baseclasses::TGLUpdateAbleObject inherited;
	
private:
	Glcolor::TGLColor* FColor;
	float FWidth;
	System::Word FPattern;
	bool FForceMaterial;
	void __fastcall SetPattern(const System::Word value);
	void __fastcall SetColor(Glcolor::TGLColor* const v);
	void __fastcall SetWidth(const float Value);
	void __fastcall SetForceMaterial(bool v);
	
public:
	__fastcall virtual TGLLineSettings(System::Classes::TPersistent* AOwner);
	__fastcall virtual ~TGLLineSettings(void);
	void __fastcall Apply(Glrendercontextinfo::TRenderContextInfo &rci);
	void __fastcall UnApply(Glrendercontextinfo::TRenderContextInfo &rci);
	
__published:
	__property float Width = {read=FWidth, write=SetWidth};
	__property Glcolor::TGLColor* Color = {read=FColor, write=SetColor};
	__property System::Word Pattern = {read=FPattern, write=SetPattern, default=65535};
	__property bool ForceMaterial = {read=FForceMaterial, write=SetForceMaterial, default=0};
};


class DELPHICLASS TGLHiddenLineShader;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLHiddenLineShader : public Glmaterial::TGLShader
{
	typedef Glmaterial::TGLShader inherited;
	
private:
	int FPassCount;
	bool FLineSmooth;
	bool FSolid;
	Glcolor::TGLColor* FBackGroundColor;
	TGLLineSettings* FFrontLine;
	TGLLineSettings* FBackLine;
	bool FLighting;
	Glscene::TGLShadeModel FShadeModel;
	void __fastcall SetlineSmooth(bool v);
	void __fastcall SetSolid(bool v);
	void __fastcall SetBackgroundColor(Glcolor::TGLColor* AColor);
	void __fastcall SetLighting(bool v);
	void __fastcall SetShadeModel(const Glscene::TGLShadeModel val);
	
protected:
	virtual void __fastcall DoApply(Glrendercontextinfo::TRenderContextInfo &rci, System::TObject* Sender);
	virtual bool __fastcall DoUnApply(Glrendercontextinfo::TRenderContextInfo &rci);
	
public:
	__fastcall virtual TGLHiddenLineShader(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TGLHiddenLineShader(void);
	
__published:
	__property TGLLineSettings* FrontLine = {read=FFrontLine, write=FFrontLine};
	__property TGLLineSettings* BackLine = {read=FBackLine, write=FBackLine};
	__property bool LineSmooth = {read=FLineSmooth, write=SetlineSmooth, default=0};
	__property bool Solid = {read=FSolid, write=SetSolid, default=0};
	__property Glcolor::TGLColor* BackgroundColor = {read=FBackGroundColor, write=SetBackgroundColor};
	__property bool SurfaceLit = {read=FLighting, write=SetLighting, default=1};
	__property Glscene::TGLShadeModel ShadeModel = {read=FShadeModel, write=SetShadeModel, default=0};
};

#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------
}	/* namespace Glhiddenlineshader */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLHIDDENLINESHADER)
using namespace Glhiddenlineshader;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// GlhiddenlineshaderHPP
