// CodeGear C++Builder
// Copyright (c) 1995, 2012 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLCgBombShader.pas' rev: 24.00 (Win32)

#ifndef GlcgbombshaderHPP
#define GlcgbombshaderHPP

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
#include <GLStrings.hpp>	// Pascal unit
#include <GLMaterial.hpp>	// Pascal unit
#include <GLRenderContextInfo.hpp>	// Pascal unit
#include <GLTextureFormat.hpp>	// Pascal unit
#include <cgGL.hpp>	// Pascal unit
#include <GLCgShader.hpp>	// Pascal unit
#include <BaseClasses.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Glcgbombshader
{
//-- type declarations -------------------------------------------------------
class DELPHICLASS EGLCgBombShaderException;
#pragma pack(push,4)
class PASCALIMPLEMENTATION EGLCgBombShaderException : public Glcgshader::EGLCGShaderException
{
	typedef Glcgshader::EGLCGShaderException inherited;
	
public:
	/* Exception.Create */ inline __fastcall EGLCgBombShaderException(const System::UnicodeString Msg) : Glcgshader::EGLCGShaderException(Msg) { }
	/* Exception.CreateFmt */ inline __fastcall EGLCgBombShaderException(const System::UnicodeString Msg, System::TVarRec const *Args, const int Args_Size) : Glcgshader::EGLCGShaderException(Msg, Args, Args_Size) { }
	/* Exception.CreateRes */ inline __fastcall EGLCgBombShaderException(NativeUInt Ident)/* overload */ : Glcgshader::EGLCGShaderException(Ident) { }
	/* Exception.CreateRes */ inline __fastcall EGLCgBombShaderException(System::PResStringRec ResStringRec)/* overload */ : Glcgshader::EGLCGShaderException(ResStringRec) { }
	/* Exception.CreateResFmt */ inline __fastcall EGLCgBombShaderException(NativeUInt Ident, System::TVarRec const *Args, const int Args_Size)/* overload */ : Glcgshader::EGLCGShaderException(Ident, Args, Args_Size) { }
	/* Exception.CreateResFmt */ inline __fastcall EGLCgBombShaderException(System::PResStringRec ResStringRec, System::TVarRec const *Args, const int Args_Size)/* overload */ : Glcgshader::EGLCGShaderException(ResStringRec, Args, Args_Size) { }
	/* Exception.CreateHelp */ inline __fastcall EGLCgBombShaderException(const System::UnicodeString Msg, int AHelpContext) : Glcgshader::EGLCGShaderException(Msg, AHelpContext) { }
	/* Exception.CreateFmtHelp */ inline __fastcall EGLCgBombShaderException(const System::UnicodeString Msg, System::TVarRec const *Args, const int Args_Size, int AHelpContext) : Glcgshader::EGLCGShaderException(Msg, Args, Args_Size, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall EGLCgBombShaderException(NativeUInt Ident, int AHelpContext)/* overload */ : Glcgshader::EGLCGShaderException(Ident, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall EGLCgBombShaderException(System::PResStringRec ResStringRec, int AHelpContext)/* overload */ : Glcgshader::EGLCGShaderException(ResStringRec, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall EGLCgBombShaderException(System::PResStringRec ResStringRec, System::TVarRec const *Args, const int Args_Size, int AHelpContext)/* overload */ : Glcgshader::EGLCGShaderException(ResStringRec, Args, Args_Size, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall EGLCgBombShaderException(NativeUInt Ident, System::TVarRec const *Args, const int Args_Size, int AHelpContext)/* overload */ : Glcgshader::EGLCGShaderException(Ident, Args, Args_Size, AHelpContext) { }
	/* Exception.Destroy */ inline __fastcall virtual ~EGLCgBombShaderException(void) { }
	
};

#pragma pack(pop)

enum TGLCgBombShaderTextureSource : unsigned char { stsPrimaryTexture, stsSecondadyTexture, stsThirdTexture, stsUserSelectedTexture };

class DELPHICLASS TGLCustomCGBombShader;
class PASCALIMPLEMENTATION TGLCustomCGBombShader : public Glcgshader::TCadencableCustomCgShader
{
	typedef Glcgshader::TCadencableCustomCgShader inherited;
	
private:
	Glmaterial::TGLAbstractMaterialLibrary* FMaterialLibrary;
	Gltexture::TGLTexture* FGradientTexture;
	Gltexture::TGLTexture* FMainTexture;
	System::UnicodeString FMainTextureName;
	System::UnicodeString FGradientTextureName;
	float FSharpness;
	float FColorRange;
	float FSpeed;
	float FDisplacement;
	float FAlpha;
	float FTurbDensity;
	float FColorSharpness;
	float FGradientTextureShare;
	float FMainTextureShare;
	TGLCgBombShaderTextureSource FMainTextureSource;
	void __fastcall SetGradientTexture(Gltexture::TGLTexture* const Value);
	void __fastcall SetMainTexture(Gltexture::TGLTexture* const Value);
	System::UnicodeString __fastcall GetMainTextureName(void);
	void __fastcall SetMainTextureName(const System::UnicodeString Value);
	System::UnicodeString __fastcall GetGradientTextureName(void);
	void __fastcall SetGradientTextureName(const System::UnicodeString Value);
	bool __fastcall StoreColorRange(void);
	bool __fastcall StoreColorSharpness(void);
	bool __fastcall StoreDisplacement(void);
	bool __fastcall StoreGradientTextureShare(void);
	bool __fastcall StoreSharpness(void);
	bool __fastcall StoreSpeed(void);
	bool __fastcall StoreTurbDensity(void);
	bool __fastcall StoreMainTextureShare(void);
	Glmaterial::TGLAbstractMaterialLibrary* __fastcall GetMaterialLibrary(void);
	
protected:
	DYNAMIC void __fastcall DoInitialize(Glrendercontextinfo::TRenderContextInfo &rci, System::TObject* Sender);
	virtual void __fastcall DoApply(Glrendercontextinfo::TRenderContextInfo &rci, System::TObject* Sender);
	HIDESBASE virtual void __fastcall OnApplyVP(Glcgshader::TCgProgram* CgProgram, System::TObject* Sender);
	HIDESBASE virtual void __fastcall OnApplyFP(Glcgshader::TCgProgram* CgProgram, System::TObject* Sender);
	HIDESBASE virtual void __fastcall OnUnApplyFP(Glcgshader::TCgProgram* CgProgram);
	virtual void __fastcall SetMaterialLibrary(Glmaterial::TGLAbstractMaterialLibrary* const Value);
	virtual void __fastcall Notification(System::Classes::TComponent* AComponent, System::Classes::TOperation Operation);
	
public:
	__fastcall virtual TGLCustomCGBombShader(System::Classes::TComponent* AOwner);
	__property Gltexture::TGLTexture* MainTexture = {read=FMainTexture, write=SetMainTexture};
	__property System::UnicodeString MainTextureName = {read=GetMainTextureName, write=SetMainTextureName};
	__property Gltexture::TGLTexture* GradientTexture = {read=FGradientTexture, write=SetGradientTexture};
	__property System::UnicodeString GradientTextureName = {read=GetGradientTextureName, write=SetGradientTextureName};
	__property float GradientTextureShare = {read=FGradientTextureShare, write=FGradientTextureShare, stored=StoreGradientTextureShare};
	__property float MainTextureShare = {read=FMainTextureShare, write=FMainTextureShare, stored=StoreMainTextureShare};
	__property float Alpha = {read=FAlpha, write=FAlpha};
	__property float Displacement = {read=FDisplacement, write=FDisplacement, stored=StoreDisplacement};
	__property float Sharpness = {read=FSharpness, write=FSharpness, stored=StoreSharpness};
	__property float ColorSharpness = {read=FColorSharpness, write=FColorSharpness, stored=StoreColorSharpness};
	__property float Speed = {read=FSpeed, write=FSpeed, stored=StoreSpeed};
	__property float TurbDensity = {read=FTurbDensity, write=FTurbDensity, stored=StoreTurbDensity};
	__property float ColorRange = {read=FColorRange, write=FColorRange, stored=StoreColorRange};
	__property TGLCgBombShaderTextureSource MainTextureSource = {read=FMainTextureSource, write=FMainTextureSource, nodefault};
	__property Glmaterial::TGLAbstractMaterialLibrary* MaterialLibrary = {read=FMaterialLibrary, write=SetMaterialLibrary};
public:
	/* TCustomCgShader.Destroy */ inline __fastcall virtual ~TGLCustomCGBombShader(void) { }
	
private:
	void *__IGLMaterialLibrarySupported;	/* Glmaterial::IGLMaterialLibrarySupported */
	
public:
	#if defined(MANAGED_INTERFACE_OPERATORS)
	// {8E442AF9-D212-4A5E-8A88-92F798BABFD1}
	operator Glmaterial::_di_IGLMaterialLibrarySupported()
	{
		Glmaterial::_di_IGLMaterialLibrarySupported intf;
		GetInterface(intf);
		return intf;
	}
	#else
	operator Glmaterial::IGLMaterialLibrarySupported*(void) { return (Glmaterial::IGLMaterialLibrarySupported*)&__IGLMaterialLibrarySupported; }
	#endif
	
};


class DELPHICLASS TGLCgBombShader;
class PASCALIMPLEMENTATION TGLCgBombShader : public TGLCustomCGBombShader
{
	typedef TGLCustomCGBombShader inherited;
	
protected:
	DYNAMIC void __fastcall DoInitialize(Glrendercontextinfo::TRenderContextInfo &rci, System::TObject* Sender);
	virtual void __fastcall DoApply(Glrendercontextinfo::TRenderContextInfo &rci, System::TObject* Sender);
	virtual void __fastcall OnApplyVP(Glcgshader::TCgProgram* CgProgram, System::TObject* Sender);
	virtual void __fastcall OnApplyFP(Glcgshader::TCgProgram* CgProgram, System::TObject* Sender);
	virtual void __fastcall OnUnApplyFP(Glcgshader::TCgProgram* CgProgram);
	
__published:
	__property MainTextureShare = {default=0};
	__property MainTextureName = {default=0};
	__property GradientTextureShare = {default=0};
	__property GradientTextureName = {default=0};
	__property Alpha = {default=0};
	__property Cadencer;
	__property Displacement = {default=0};
	__property Sharpness = {default=0};
	__property ColorSharpness = {default=0};
	__property Speed = {default=0};
	__property TurbDensity = {default=0};
	__property ColorRange = {default=0};
	__property MaterialLibrary;
	__property DesignEnable = {default=0};
public:
	/* TGLCustomCGBombShader.Create */ inline __fastcall virtual TGLCgBombShader(System::Classes::TComponent* AOwner) : TGLCustomCGBombShader(AOwner) { }
	
public:
	/* TCustomCgShader.Destroy */ inline __fastcall virtual ~TGLCgBombShader(void) { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Glcgbombshader */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLCGBOMBSHADER)
using namespace Glcgbombshader;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// GlcgbombshaderHPP
