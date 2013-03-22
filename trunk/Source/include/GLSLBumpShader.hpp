// CodeGear C++Builder
// Copyright (c) 1995, 2012 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLSLBumpShader.pas' rev: 24.00 (Win32)

#ifndef GlslbumpshaderHPP
#define GlslbumpshaderHPP

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
#include <GLScene.hpp>	// Pascal unit
#include <VectorGeometry.hpp>	// Pascal unit
#include <VectorTypes.hpp>	// Pascal unit
#include <GLCadencer.hpp>	// Pascal unit
#include <GLStrings.hpp>	// Pascal unit
#include <OpenGLTokens.hpp>	// Pascal unit
#include <GLSLShader.hpp>	// Pascal unit
#include <GLCustomShader.hpp>	// Pascal unit
#include <GLColor.hpp>	// Pascal unit
#include <GLRenderContextInfo.hpp>	// Pascal unit
#include <GLMaterial.hpp>	// Pascal unit
#include <BaseClasses.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Glslbumpshader
{
//-- type declarations -------------------------------------------------------
class DELPHICLASS EGLSLBumpShaderException;
#pragma pack(push,4)
class PASCALIMPLEMENTATION EGLSLBumpShaderException : public Glslshader::EGLSLShaderException
{
	typedef Glslshader::EGLSLShaderException inherited;
	
public:
	/* Exception.Create */ inline __fastcall EGLSLBumpShaderException(const System::UnicodeString Msg) : Glslshader::EGLSLShaderException(Msg) { }
	/* Exception.CreateFmt */ inline __fastcall EGLSLBumpShaderException(const System::UnicodeString Msg, System::TVarRec const *Args, const int Args_Size) : Glslshader::EGLSLShaderException(Msg, Args, Args_Size) { }
	/* Exception.CreateRes */ inline __fastcall EGLSLBumpShaderException(NativeUInt Ident)/* overload */ : Glslshader::EGLSLShaderException(Ident) { }
	/* Exception.CreateRes */ inline __fastcall EGLSLBumpShaderException(System::PResStringRec ResStringRec)/* overload */ : Glslshader::EGLSLShaderException(ResStringRec) { }
	/* Exception.CreateResFmt */ inline __fastcall EGLSLBumpShaderException(NativeUInt Ident, System::TVarRec const *Args, const int Args_Size)/* overload */ : Glslshader::EGLSLShaderException(Ident, Args, Args_Size) { }
	/* Exception.CreateResFmt */ inline __fastcall EGLSLBumpShaderException(System::PResStringRec ResStringRec, System::TVarRec const *Args, const int Args_Size)/* overload */ : Glslshader::EGLSLShaderException(ResStringRec, Args, Args_Size) { }
	/* Exception.CreateHelp */ inline __fastcall EGLSLBumpShaderException(const System::UnicodeString Msg, int AHelpContext) : Glslshader::EGLSLShaderException(Msg, AHelpContext) { }
	/* Exception.CreateFmtHelp */ inline __fastcall EGLSLBumpShaderException(const System::UnicodeString Msg, System::TVarRec const *Args, const int Args_Size, int AHelpContext) : Glslshader::EGLSLShaderException(Msg, Args, Args_Size, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall EGLSLBumpShaderException(NativeUInt Ident, int AHelpContext)/* overload */ : Glslshader::EGLSLShaderException(Ident, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall EGLSLBumpShaderException(System::PResStringRec ResStringRec, int AHelpContext)/* overload */ : Glslshader::EGLSLShaderException(ResStringRec, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall EGLSLBumpShaderException(System::PResStringRec ResStringRec, System::TVarRec const *Args, const int Args_Size, int AHelpContext)/* overload */ : Glslshader::EGLSLShaderException(ResStringRec, Args, Args_Size, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall EGLSLBumpShaderException(NativeUInt Ident, System::TVarRec const *Args, const int Args_Size, int AHelpContext)/* overload */ : Glslshader::EGLSLShaderException(Ident, Args, Args_Size, AHelpContext) { }
	/* Exception.Destroy */ inline __fastcall virtual ~EGLSLBumpShaderException(void) { }
	
};

#pragma pack(pop)

class DELPHICLASS TGLBaseCustomGLSLBumpShader;
class PASCALIMPLEMENTATION TGLBaseCustomGLSLBumpShader : public Glslshader::TGLCustomGLSLShader
{
	typedef Glslshader::TGLCustomGLSLShader inherited;
	
private:
	float FBumpHeight;
	int FBumpSmoothness;
	float FSpecularPower;
	float FSpecularSpread;
	float FLightPower;
	Glmaterial::TGLMaterialLibrary* FMaterialLibrary;
	Gltexture::TGLTexture* FNormalTexture;
	Gltexture::TGLTexture* FSpecularTexture;
	System::UnicodeString FNormalTextureName;
	System::UnicodeString FSpecularTextureName;
	System::UnicodeString __fastcall GetNormalTextureName(void);
	System::UnicodeString __fastcall GetSpecularTextureName(void);
	void __fastcall SetNormalTextureName(const System::UnicodeString Value);
	void __fastcall SetSpecularTextureName(const System::UnicodeString Value);
	void __fastcall SetSpecularTexture(Gltexture::TGLTexture* const Value);
	void __fastcall SetNormalTexture(Gltexture::TGLTexture* const Value);
	Glmaterial::TGLAbstractMaterialLibrary* __fastcall GetMaterialLibrary(void);
	
protected:
	virtual void __fastcall DoApply(Glrendercontextinfo::TRenderContextInfo &rci, System::TObject* Sender);
	virtual bool __fastcall DoUnApply(Glrendercontextinfo::TRenderContextInfo &rci);
	virtual void __fastcall SetMaterialLibrary(Glmaterial::TGLMaterialLibrary* const Value);
	virtual void __fastcall Notification(System::Classes::TComponent* AComponent, System::Classes::TOperation Operation);
	
public:
	__fastcall virtual TGLBaseCustomGLSLBumpShader(System::Classes::TComponent* AOwner);
	__property float BumpHeight = {read=FBumpHeight, write=FBumpHeight};
	__property int BumpSmoothness = {read=FBumpSmoothness, write=FBumpSmoothness, nodefault};
	__property float SpecularPower = {read=FSpecularPower, write=FSpecularPower};
	__property float SpecularSpread = {read=FSpecularSpread, write=FSpecularSpread};
	__property float LightPower = {read=FLightPower, write=FLightPower};
	__property Gltexture::TGLTexture* NormalTexture = {read=FNormalTexture, write=SetNormalTexture};
	__property Gltexture::TGLTexture* SpecularTexture = {read=FSpecularTexture, write=SetSpecularTexture};
	__property System::UnicodeString NormalTextureName = {read=GetNormalTextureName, write=SetNormalTextureName};
	__property System::UnicodeString SpecularTextureName = {read=GetSpecularTextureName, write=SetSpecularTextureName};
	__property Glmaterial::TGLMaterialLibrary* MaterialLibrary = {read=FMaterialLibrary, write=SetMaterialLibrary};
public:
	/* TGLCustomGLSLShader.Destroy */ inline __fastcall virtual ~TGLBaseCustomGLSLBumpShader(void) { }
	
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


class DELPHICLASS TGLBaseCustomGLSLBumpShaderMT;
class PASCALIMPLEMENTATION TGLBaseCustomGLSLBumpShaderMT : public TGLBaseCustomGLSLBumpShader
{
	typedef TGLBaseCustomGLSLBumpShader inherited;
	
private:
	Gltexture::TGLTexture* FMainTexture;
	System::UnicodeString FMainTextureName;
	System::UnicodeString __fastcall GetMainTextureName(void);
	void __fastcall SetMainTextureName(const System::UnicodeString Value);
	
protected:
	virtual void __fastcall SetMaterialLibrary(Glmaterial::TGLMaterialLibrary* const Value);
	virtual void __fastcall Notification(System::Classes::TComponent* AComponent, System::Classes::TOperation Operation);
	
public:
	__property Gltexture::TGLTexture* MainTexture = {read=FMainTexture, write=FMainTexture};
	__property System::UnicodeString MainTextureName = {read=GetMainTextureName, write=SetMainTextureName};
public:
	/* TGLBaseCustomGLSLBumpShader.Create */ inline __fastcall virtual TGLBaseCustomGLSLBumpShaderMT(System::Classes::TComponent* AOwner) : TGLBaseCustomGLSLBumpShader(AOwner) { }
	
public:
	/* TGLCustomGLSLShader.Destroy */ inline __fastcall virtual ~TGLBaseCustomGLSLBumpShaderMT(void) { }
	
};


class DELPHICLASS TGLCustomGLSLBumpShaderAM;
class PASCALIMPLEMENTATION TGLCustomGLSLBumpShaderAM : public TGLBaseCustomGLSLBumpShaderMT
{
	typedef TGLBaseCustomGLSLBumpShaderMT inherited;
	
private:
	Glcolor::TGLColor* FAmbientColor;
	Glcolor::TGLColor* FDiffuseColor;
	Glcolor::TGLColor* FSpecularColor;
	float __fastcall GetAlpha(void);
	void __fastcall SetAlpha(const float Value);
	
protected:
	virtual void __fastcall DoApply(Glrendercontextinfo::TRenderContextInfo &rci, System::TObject* Sender);
	DYNAMIC void __fastcall DoInitialize(Glrendercontextinfo::TRenderContextInfo &rci, System::TObject* Sender);
	
public:
	__fastcall virtual TGLCustomGLSLBumpShaderAM(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TGLCustomGLSLBumpShaderAM(void);
	__property Glcolor::TGLColor* AmbientColor = {read=FAmbientColor};
	__property Glcolor::TGLColor* DiffuseColor = {read=FDiffuseColor};
	__property Glcolor::TGLColor* SpecularColor = {read=FSpecularColor};
	__property float Alpha = {read=GetAlpha, write=SetAlpha};
};


class DELPHICLASS TGLCustomGLSLBumpShaderMT;
class PASCALIMPLEMENTATION TGLCustomGLSLBumpShaderMT : public TGLBaseCustomGLSLBumpShaderMT
{
	typedef TGLBaseCustomGLSLBumpShaderMT inherited;
	
protected:
	virtual void __fastcall DoApply(Glrendercontextinfo::TRenderContextInfo &rci, System::TObject* Sender);
	DYNAMIC void __fastcall DoInitialize(Glrendercontextinfo::TRenderContextInfo &rci, System::TObject* Sender);
public:
	/* TGLBaseCustomGLSLBumpShader.Create */ inline __fastcall virtual TGLCustomGLSLBumpShaderMT(System::Classes::TComponent* AOwner) : TGLBaseCustomGLSLBumpShaderMT(AOwner) { }
	
public:
	/* TGLCustomGLSLShader.Destroy */ inline __fastcall virtual ~TGLCustomGLSLBumpShaderMT(void) { }
	
};


class DELPHICLASS TGLCustomGLSLBumpShader;
class PASCALIMPLEMENTATION TGLCustomGLSLBumpShader : public TGLBaseCustomGLSLBumpShader
{
	typedef TGLBaseCustomGLSLBumpShader inherited;
	
private:
	void __fastcall SetShaderTextures(Gltexture::TGLTexture* const *Textures, const int Textures_Size);
	void __fastcall GetShaderTextures(Gltexture::TGLTexture* *Textures, const int Textures_Size);
	void __fastcall SetShaderColorParams(const Vectortypes::TVector4f &AAmbientColor, const Vectortypes::TVector4f &ADiffuseColor, const Vectortypes::TVector4f &ASpecularcolor);
	void __fastcall GetShaderColorParams(Vectortypes::TVector4f &AAmbientColor, Vectortypes::TVector4f &ADiffuseColor, Vectortypes::TVector4f &ASpecularcolor);
	void __fastcall SetShaderMiscParameters(Glcadencer::TGLCadencer* const ACadencer, Glmaterial::TGLMaterialLibrary* const AMatLib, const Glcustomshader::TGLLightSourceSet ALightSources);
	void __fastcall GetShaderMiscParameters(Glcadencer::TGLCadencer* &ACadencer, Glmaterial::TGLMaterialLibrary* &AMatLib, Glcustomshader::TGLLightSourceSet &ALightSources);
	float __fastcall GetShaderAlpha(void);
	void __fastcall SetShaderAlpha(const float Value);
	System::UnicodeString __fastcall GetShaderDescription(void);
	
protected:
	virtual void __fastcall DoApply(Glrendercontextinfo::TRenderContextInfo &rci, System::TObject* Sender);
	DYNAMIC void __fastcall DoInitialize(Glrendercontextinfo::TRenderContextInfo &rci, System::TObject* Sender);
public:
	/* TGLBaseCustomGLSLBumpShader.Create */ inline __fastcall virtual TGLCustomGLSLBumpShader(System::Classes::TComponent* AOwner) : TGLBaseCustomGLSLBumpShader(AOwner) { }
	
public:
	/* TGLCustomGLSLShader.Destroy */ inline __fastcall virtual ~TGLCustomGLSLBumpShader(void) { }
	
private:
	void *__IGLShaderDescription;	/* Glcustomshader::IGLShaderDescription */
	
public:
	#if defined(MANAGED_INTERFACE_OPERATORS)
	// {04089C64-60C2-43F5-AC9C-38ED46264812}
	operator Glcustomshader::_di_IGLShaderDescription()
	{
		Glcustomshader::_di_IGLShaderDescription intf;
		GetInterface(intf);
		return intf;
	}
	#else
	operator Glcustomshader::IGLShaderDescription*(void) { return (Glcustomshader::IGLShaderDescription*)&__IGLShaderDescription; }
	#endif
	
};


class DELPHICLASS TGLCustomGLSLMLBumpShader;
class PASCALIMPLEMENTATION TGLCustomGLSLMLBumpShader : public TGLBaseCustomGLSLBumpShader
{
	typedef TGLBaseCustomGLSLBumpShader inherited;
	
private:
	Glcustomshader::TGLLightSourceSet FLightSources;
	float FLightCompensation;
	void __fastcall SetLightSources(const Glcustomshader::TGLLightSourceSet Value);
	void __fastcall SetLightCompensation(const float Value);
	void __fastcall SetShaderTextures(Gltexture::TGLTexture* const *Textures, const int Textures_Size);
	void __fastcall GetShaderTextures(Gltexture::TGLTexture* *Textures, const int Textures_Size);
	void __fastcall SetShaderColorParams(const Vectortypes::TVector4f &AAmbientColor, const Vectortypes::TVector4f &ADiffuseColor, const Vectortypes::TVector4f &ASpecularcolor);
	void __fastcall GetShaderColorParams(Vectortypes::TVector4f &AAmbientColor, Vectortypes::TVector4f &ADiffuseColor, Vectortypes::TVector4f &ASpecularcolor);
	void __fastcall SetShaderMiscParameters(Glcadencer::TGLCadencer* const ACadencer, Glmaterial::TGLMaterialLibrary* const AMatLib, const Glcustomshader::TGLLightSourceSet ALightSources);
	void __fastcall GetShaderMiscParameters(Glcadencer::TGLCadencer* &ACadencer, Glmaterial::TGLMaterialLibrary* &AMatLib, Glcustomshader::TGLLightSourceSet &ALightSources);
	float __fastcall GetShaderAlpha(void);
	void __fastcall SetShaderAlpha(const float Value);
	System::UnicodeString __fastcall GetShaderDescription(void);
	
protected:
	virtual void __fastcall DoApply(Glrendercontextinfo::TRenderContextInfo &rci, System::TObject* Sender);
	DYNAMIC void __fastcall DoInitialize(Glrendercontextinfo::TRenderContextInfo &rci, System::TObject* Sender);
	
public:
	__fastcall virtual TGLCustomGLSLMLBumpShader(System::Classes::TComponent* AOwner);
	__property Glcustomshader::TGLLightSourceSet LightSources = {read=FLightSources, write=SetLightSources, default=2};
	__property float LightCompensation = {read=FLightCompensation, write=SetLightCompensation};
public:
	/* TGLCustomGLSLShader.Destroy */ inline __fastcall virtual ~TGLCustomGLSLMLBumpShader(void) { }
	
private:
	void *__IGLShaderDescription;	/* Glcustomshader::IGLShaderDescription */
	
public:
	#if defined(MANAGED_INTERFACE_OPERATORS)
	// {04089C64-60C2-43F5-AC9C-38ED46264812}
	operator Glcustomshader::_di_IGLShaderDescription()
	{
		Glcustomshader::_di_IGLShaderDescription intf;
		GetInterface(intf);
		return intf;
	}
	#else
	operator Glcustomshader::IGLShaderDescription*(void) { return (Glcustomshader::IGLShaderDescription*)&__IGLShaderDescription; }
	#endif
	
};


class DELPHICLASS TGLCustomGLSLMLBumpShaderMT;
class PASCALIMPLEMENTATION TGLCustomGLSLMLBumpShaderMT : public TGLBaseCustomGLSLBumpShaderMT
{
	typedef TGLBaseCustomGLSLBumpShaderMT inherited;
	
private:
	Glcustomshader::TGLLightSourceSet FLightSources;
	float FLightCompensation;
	void __fastcall SetLightSources(const Glcustomshader::TGLLightSourceSet Value);
	void __fastcall SetLightCompensation(const float Value);
	
protected:
	virtual void __fastcall DoApply(Glrendercontextinfo::TRenderContextInfo &rci, System::TObject* Sender);
	DYNAMIC void __fastcall DoInitialize(Glrendercontextinfo::TRenderContextInfo &rci, System::TObject* Sender);
	
public:
	__fastcall virtual TGLCustomGLSLMLBumpShaderMT(System::Classes::TComponent* AOwner);
	__property Glcustomshader::TGLLightSourceSet LightSources = {read=FLightSources, write=SetLightSources, default=2};
	__property float LightCompensation = {read=FLightCompensation, write=SetLightCompensation};
public:
	/* TGLCustomGLSLShader.Destroy */ inline __fastcall virtual ~TGLCustomGLSLMLBumpShaderMT(void) { }
	
};


class DELPHICLASS TGLSLBumpShaderMT;
class PASCALIMPLEMENTATION TGLSLBumpShaderMT : public TGLCustomGLSLBumpShaderMT
{
	typedef TGLCustomGLSLBumpShaderMT inherited;
	
__published:
	__property MainTextureName = {default=0};
	__property NormalTextureName = {default=0};
	__property SpecularTextureName = {default=0};
	__property MaterialLibrary;
	__property BumpHeight = {default=0};
	__property BumpSmoothness;
	__property SpecularPower = {default=0};
	__property SpecularSpread = {default=0};
	__property LightPower = {default=0};
public:
	/* TGLBaseCustomGLSLBumpShader.Create */ inline __fastcall virtual TGLSLBumpShaderMT(System::Classes::TComponent* AOwner) : TGLCustomGLSLBumpShaderMT(AOwner) { }
	
public:
	/* TGLCustomGLSLShader.Destroy */ inline __fastcall virtual ~TGLSLBumpShaderMT(void) { }
	
};


class DELPHICLASS TGLSLBumpShader;
class PASCALIMPLEMENTATION TGLSLBumpShader : public TGLCustomGLSLBumpShader
{
	typedef TGLCustomGLSLBumpShader inherited;
	
__published:
	__property NormalTextureName = {default=0};
	__property SpecularTextureName = {default=0};
	__property MaterialLibrary;
	__property BumpHeight = {default=0};
	__property BumpSmoothness;
	__property SpecularPower = {default=0};
	__property SpecularSpread = {default=0};
	__property LightPower = {default=0};
public:
	/* TGLBaseCustomGLSLBumpShader.Create */ inline __fastcall virtual TGLSLBumpShader(System::Classes::TComponent* AOwner) : TGLCustomGLSLBumpShader(AOwner) { }
	
public:
	/* TGLCustomGLSLShader.Destroy */ inline __fastcall virtual ~TGLSLBumpShader(void) { }
	
};


class DELPHICLASS TGLSLBumpShaderAM;
class PASCALIMPLEMENTATION TGLSLBumpShaderAM : public TGLCustomGLSLBumpShaderAM
{
	typedef TGLCustomGLSLBumpShaderAM inherited;
	
__published:
	__property AmbientColor;
	__property DiffuseColor;
	__property SpecularColor;
	__property Alpha = {stored=false, default=0};
	__property MainTextureName = {default=0};
	__property NormalTextureName = {default=0};
	__property SpecularTextureName = {default=0};
	__property MaterialLibrary;
	__property BumpHeight = {default=0};
	__property BumpSmoothness;
	__property SpecularPower = {default=0};
	__property SpecularSpread = {default=0};
	__property LightPower = {default=0};
public:
	/* TGLCustomGLSLBumpShaderAM.Create */ inline __fastcall virtual TGLSLBumpShaderAM(System::Classes::TComponent* AOwner) : TGLCustomGLSLBumpShaderAM(AOwner) { }
	/* TGLCustomGLSLBumpShaderAM.Destroy */ inline __fastcall virtual ~TGLSLBumpShaderAM(void) { }
	
};


class DELPHICLASS TGLSLMLBumpShader;
class PASCALIMPLEMENTATION TGLSLMLBumpShader : public TGLCustomGLSLMLBumpShader
{
	typedef TGLCustomGLSLMLBumpShader inherited;
	
__published:
	__property NormalTextureName = {default=0};
	__property SpecularTextureName = {default=0};
	__property MaterialLibrary;
	__property BumpHeight = {default=0};
	__property BumpSmoothness;
	__property SpecularPower = {default=0};
	__property SpecularSpread = {default=0};
	__property LightPower = {default=0};
	__property LightSources = {default=2};
	__property LightCompensation = {default=0};
public:
	/* TGLCustomGLSLMLBumpShader.Create */ inline __fastcall virtual TGLSLMLBumpShader(System::Classes::TComponent* AOwner) : TGLCustomGLSLMLBumpShader(AOwner) { }
	
public:
	/* TGLCustomGLSLShader.Destroy */ inline __fastcall virtual ~TGLSLMLBumpShader(void) { }
	
};


class DELPHICLASS TGLSLMLBumpShaderMT;
class PASCALIMPLEMENTATION TGLSLMLBumpShaderMT : public TGLCustomGLSLMLBumpShaderMT
{
	typedef TGLCustomGLSLMLBumpShaderMT inherited;
	
__published:
	__property MainTextureName = {default=0};
	__property NormalTextureName = {default=0};
	__property SpecularTextureName = {default=0};
	__property MaterialLibrary;
	__property BumpHeight = {default=0};
	__property BumpSmoothness;
	__property SpecularPower = {default=0};
	__property SpecularSpread = {default=0};
	__property LightPower = {default=0};
	__property LightSources = {default=2};
	__property LightCompensation = {default=0};
public:
	/* TGLCustomGLSLMLBumpShaderMT.Create */ inline __fastcall virtual TGLSLMLBumpShaderMT(System::Classes::TComponent* AOwner) : TGLCustomGLSLMLBumpShaderMT(AOwner) { }
	
public:
	/* TGLCustomGLSLShader.Destroy */ inline __fastcall virtual ~TGLSLMLBumpShaderMT(void) { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Glslbumpshader */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLSLBUMPSHADER)
using namespace Glslbumpshader;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// GlslbumpshaderHPP
