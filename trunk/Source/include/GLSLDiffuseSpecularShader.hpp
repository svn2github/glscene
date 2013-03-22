// CodeGear C++Builder
// Copyright (c) 1995, 2012 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLSLDiffuseSpecularShader.pas' rev: 24.00 (Win32)

#ifndef GlsldiffusespecularshaderHPP
#define GlsldiffusespecularshaderHPP

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
#include <OpenGLTokens.hpp>	// Pascal unit
#include <GLStrings.hpp>	// Pascal unit
#include <GLCustomShader.hpp>	// Pascal unit
#include <GLSLShader.hpp>	// Pascal unit
#include <GLColor.hpp>	// Pascal unit
#include <GLRenderContextInfo.hpp>	// Pascal unit
#include <GLMaterial.hpp>	// Pascal unit
#include <BaseClasses.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Glsldiffusespecularshader
{
//-- type declarations -------------------------------------------------------
class DELPHICLASS EGLSLDiffuseSpecularShaderException;
#pragma pack(push,4)
class PASCALIMPLEMENTATION EGLSLDiffuseSpecularShaderException : public Glslshader::EGLSLShaderException
{
	typedef Glslshader::EGLSLShaderException inherited;
	
public:
	/* Exception.Create */ inline __fastcall EGLSLDiffuseSpecularShaderException(const System::UnicodeString Msg) : Glslshader::EGLSLShaderException(Msg) { }
	/* Exception.CreateFmt */ inline __fastcall EGLSLDiffuseSpecularShaderException(const System::UnicodeString Msg, System::TVarRec const *Args, const int Args_Size) : Glslshader::EGLSLShaderException(Msg, Args, Args_Size) { }
	/* Exception.CreateRes */ inline __fastcall EGLSLDiffuseSpecularShaderException(NativeUInt Ident)/* overload */ : Glslshader::EGLSLShaderException(Ident) { }
	/* Exception.CreateRes */ inline __fastcall EGLSLDiffuseSpecularShaderException(System::PResStringRec ResStringRec)/* overload */ : Glslshader::EGLSLShaderException(ResStringRec) { }
	/* Exception.CreateResFmt */ inline __fastcall EGLSLDiffuseSpecularShaderException(NativeUInt Ident, System::TVarRec const *Args, const int Args_Size)/* overload */ : Glslshader::EGLSLShaderException(Ident, Args, Args_Size) { }
	/* Exception.CreateResFmt */ inline __fastcall EGLSLDiffuseSpecularShaderException(System::PResStringRec ResStringRec, System::TVarRec const *Args, const int Args_Size)/* overload */ : Glslshader::EGLSLShaderException(ResStringRec, Args, Args_Size) { }
	/* Exception.CreateHelp */ inline __fastcall EGLSLDiffuseSpecularShaderException(const System::UnicodeString Msg, int AHelpContext) : Glslshader::EGLSLShaderException(Msg, AHelpContext) { }
	/* Exception.CreateFmtHelp */ inline __fastcall EGLSLDiffuseSpecularShaderException(const System::UnicodeString Msg, System::TVarRec const *Args, const int Args_Size, int AHelpContext) : Glslshader::EGLSLShaderException(Msg, Args, Args_Size, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall EGLSLDiffuseSpecularShaderException(NativeUInt Ident, int AHelpContext)/* overload */ : Glslshader::EGLSLShaderException(Ident, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall EGLSLDiffuseSpecularShaderException(System::PResStringRec ResStringRec, int AHelpContext)/* overload */ : Glslshader::EGLSLShaderException(ResStringRec, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall EGLSLDiffuseSpecularShaderException(System::PResStringRec ResStringRec, System::TVarRec const *Args, const int Args_Size, int AHelpContext)/* overload */ : Glslshader::EGLSLShaderException(ResStringRec, Args, Args_Size, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall EGLSLDiffuseSpecularShaderException(NativeUInt Ident, System::TVarRec const *Args, const int Args_Size, int AHelpContext)/* overload */ : Glslshader::EGLSLShaderException(Ident, Args, Args_Size, AHelpContext) { }
	/* Exception.Destroy */ inline __fastcall virtual ~EGLSLDiffuseSpecularShaderException(void) { }
	
};

#pragma pack(pop)

class DELPHICLASS TGLBaseCustomGLSLDiffuseSpecular;
class PASCALIMPLEMENTATION TGLBaseCustomGLSLDiffuseSpecular : public Glslshader::TGLCustomGLSLShader
{
	typedef Glslshader::TGLCustomGLSLShader inherited;
	
private:
	float FLightPower;
	bool FRealisticSpecular;
	Glcustomshader::TGLShaderFogSupport FFogSupport;
	void __fastcall SetRealisticSpecular(const bool Value);
	void __fastcall SetFogSupport(const Glcustomshader::TGLShaderFogSupport Value);
	
protected:
	virtual void __fastcall DoApply(Glrendercontextinfo::TRenderContextInfo &rci, System::TObject* Sender);
	virtual bool __fastcall DoUnApply(Glrendercontextinfo::TRenderContextInfo &rci);
	
public:
	__fastcall virtual TGLBaseCustomGLSLDiffuseSpecular(System::Classes::TComponent* AOwner);
	__property float LightPower = {read=FLightPower, write=FLightPower};
	__property bool RealisticSpecular = {read=FRealisticSpecular, write=SetRealisticSpecular, nodefault};
	__property Glcustomshader::TGLShaderFogSupport FogSupport = {read=FFogSupport, write=SetFogSupport, default=2};
public:
	/* TGLCustomGLSLShader.Destroy */ inline __fastcall virtual ~TGLBaseCustomGLSLDiffuseSpecular(void) { }
	
};


class DELPHICLASS TGLBaseGLSLDiffuseSpecularShaderMT;
class PASCALIMPLEMENTATION TGLBaseGLSLDiffuseSpecularShaderMT : public TGLBaseCustomGLSLDiffuseSpecular
{
	typedef TGLBaseCustomGLSLDiffuseSpecular inherited;
	
private:
	Glmaterial::TGLMaterialLibrary* FMaterialLibrary;
	Gltexture::TGLTexture* FMainTexture;
	System::UnicodeString FMainTextureName;
	System::UnicodeString __fastcall GetMainTextureName(void);
	void __fastcall SetMainTextureName(const System::UnicodeString Value);
	Glmaterial::TGLAbstractMaterialLibrary* __fastcall GetMaterialLibrary(void);
	
protected:
	virtual void __fastcall SetMaterialLibrary(Glmaterial::TGLMaterialLibrary* const Value);
	virtual void __fastcall Notification(System::Classes::TComponent* AComponent, System::Classes::TOperation Operation);
	virtual void __fastcall DoApply(Glrendercontextinfo::TRenderContextInfo &rci, System::TObject* Sender);
	
public:
	__property Gltexture::TGLTexture* MainTexture = {read=FMainTexture, write=FMainTexture};
	__property System::UnicodeString MainTextureName = {read=GetMainTextureName, write=SetMainTextureName};
	__property Glmaterial::TGLMaterialLibrary* MaterialLibrary = {read=FMaterialLibrary, write=SetMaterialLibrary};
public:
	/* TGLBaseCustomGLSLDiffuseSpecular.Create */ inline __fastcall virtual TGLBaseGLSLDiffuseSpecularShaderMT(System::Classes::TComponent* AOwner) : TGLBaseCustomGLSLDiffuseSpecular(AOwner) { }
	
public:
	/* TGLCustomGLSLShader.Destroy */ inline __fastcall virtual ~TGLBaseGLSLDiffuseSpecularShaderMT(void) { }
	
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


class DELPHICLASS TGLCustomGLSLDiffuseSpecularShader;
class PASCALIMPLEMENTATION TGLCustomGLSLDiffuseSpecularShader : public TGLBaseCustomGLSLDiffuseSpecular
{
	typedef TGLBaseCustomGLSLDiffuseSpecular inherited;
	
protected:
	DYNAMIC void __fastcall DoInitialize(Glrendercontextinfo::TRenderContextInfo &rci, System::TObject* Sender);
	virtual void __fastcall DoApply(Glrendercontextinfo::TRenderContextInfo &rci, System::TObject* Sender);
public:
	/* TGLBaseCustomGLSLDiffuseSpecular.Create */ inline __fastcall virtual TGLCustomGLSLDiffuseSpecularShader(System::Classes::TComponent* AOwner) : TGLBaseCustomGLSLDiffuseSpecular(AOwner) { }
	
public:
	/* TGLCustomGLSLShader.Destroy */ inline __fastcall virtual ~TGLCustomGLSLDiffuseSpecularShader(void) { }
	
};


class DELPHICLASS TGLCustomGLSLDiffuseSpecularShaderMT;
class PASCALIMPLEMENTATION TGLCustomGLSLDiffuseSpecularShaderMT : public TGLBaseGLSLDiffuseSpecularShaderMT
{
	typedef TGLBaseGLSLDiffuseSpecularShaderMT inherited;
	
protected:
	DYNAMIC void __fastcall DoInitialize(Glrendercontextinfo::TRenderContextInfo &rci, System::TObject* Sender);
public:
	/* TGLBaseCustomGLSLDiffuseSpecular.Create */ inline __fastcall virtual TGLCustomGLSLDiffuseSpecularShaderMT(System::Classes::TComponent* AOwner) : TGLBaseGLSLDiffuseSpecularShaderMT(AOwner) { }
	
public:
	/* TGLCustomGLSLShader.Destroy */ inline __fastcall virtual ~TGLCustomGLSLDiffuseSpecularShaderMT(void) { }
	
};


struct DECLSPEC_DRECORD TLightRecord
{
public:
	bool Enabled;
	Glscene::TLightStyle Style;
};


class DELPHICLASS TGLCustomGLSLMLDiffuseSpecularShader;
class PASCALIMPLEMENTATION TGLCustomGLSLMLDiffuseSpecularShader : public TGLBaseCustomGLSLDiffuseSpecular
{
	typedef TGLBaseCustomGLSLDiffuseSpecular inherited;
	
private:
	System::StaticArray<TLightRecord, 8> FLightTrace;
	
protected:
	DYNAMIC void __fastcall DoInitialize(Glrendercontextinfo::TRenderContextInfo &rci, System::TObject* Sender);
	virtual void __fastcall DoApply(Glrendercontextinfo::TRenderContextInfo &rci, System::TObject* Sender);
	
public:
	__fastcall virtual TGLCustomGLSLMLDiffuseSpecularShader(System::Classes::TComponent* AOwner);
public:
	/* TGLCustomGLSLShader.Destroy */ inline __fastcall virtual ~TGLCustomGLSLMLDiffuseSpecularShader(void) { }
	
};


class DELPHICLASS TGLCustomGLSLMLDiffuseSpecularShaderMT;
class PASCALIMPLEMENTATION TGLCustomGLSLMLDiffuseSpecularShaderMT : public TGLBaseGLSLDiffuseSpecularShaderMT
{
	typedef TGLBaseGLSLDiffuseSpecularShaderMT inherited;
	
private:
	System::StaticArray<TLightRecord, 8> FLightTrace;
	
protected:
	DYNAMIC void __fastcall DoInitialize(Glrendercontextinfo::TRenderContextInfo &rci, System::TObject* Sender);
	virtual void __fastcall DoApply(Glrendercontextinfo::TRenderContextInfo &rci, System::TObject* Sender);
	
public:
	__fastcall virtual TGLCustomGLSLMLDiffuseSpecularShaderMT(System::Classes::TComponent* AOwner);
public:
	/* TGLCustomGLSLShader.Destroy */ inline __fastcall virtual ~TGLCustomGLSLMLDiffuseSpecularShaderMT(void) { }
	
};


class DELPHICLASS TGLSLDiffuseSpecularShaderMT;
class PASCALIMPLEMENTATION TGLSLDiffuseSpecularShaderMT : public TGLCustomGLSLDiffuseSpecularShaderMT
{
	typedef TGLCustomGLSLDiffuseSpecularShaderMT inherited;
	
__published:
	__property MainTextureName = {default=0};
	__property LightPower = {default=0};
	__property FogSupport = {default=2};
public:
	/* TGLBaseCustomGLSLDiffuseSpecular.Create */ inline __fastcall virtual TGLSLDiffuseSpecularShaderMT(System::Classes::TComponent* AOwner) : TGLCustomGLSLDiffuseSpecularShaderMT(AOwner) { }
	
public:
	/* TGLCustomGLSLShader.Destroy */ inline __fastcall virtual ~TGLSLDiffuseSpecularShaderMT(void) { }
	
};


class DELPHICLASS TGLSLDiffuseSpecularShader;
class PASCALIMPLEMENTATION TGLSLDiffuseSpecularShader : public TGLCustomGLSLDiffuseSpecularShader
{
	typedef TGLCustomGLSLDiffuseSpecularShader inherited;
	
__published:
	__property LightPower = {default=0};
	__property FogSupport = {default=2};
public:
	/* TGLBaseCustomGLSLDiffuseSpecular.Create */ inline __fastcall virtual TGLSLDiffuseSpecularShader(System::Classes::TComponent* AOwner) : TGLCustomGLSLDiffuseSpecularShader(AOwner) { }
	
public:
	/* TGLCustomGLSLShader.Destroy */ inline __fastcall virtual ~TGLSLDiffuseSpecularShader(void) { }
	
};


class DELPHICLASS TGLSLMLDiffuseSpecularShaderMT;
class PASCALIMPLEMENTATION TGLSLMLDiffuseSpecularShaderMT : public TGLCustomGLSLMLDiffuseSpecularShaderMT
{
	typedef TGLCustomGLSLMLDiffuseSpecularShaderMT inherited;
	
__published:
	__property MainTextureName = {default=0};
	__property LightPower = {default=0};
	__property FogSupport = {default=2};
public:
	/* TGLCustomGLSLMLDiffuseSpecularShaderMT.Create */ inline __fastcall virtual TGLSLMLDiffuseSpecularShaderMT(System::Classes::TComponent* AOwner) : TGLCustomGLSLMLDiffuseSpecularShaderMT(AOwner) { }
	
public:
	/* TGLCustomGLSLShader.Destroy */ inline __fastcall virtual ~TGLSLMLDiffuseSpecularShaderMT(void) { }
	
};


class DELPHICLASS TGLSLMLDiffuseSpecularShader;
class PASCALIMPLEMENTATION TGLSLMLDiffuseSpecularShader : public TGLCustomGLSLMLDiffuseSpecularShader
{
	typedef TGLCustomGLSLMLDiffuseSpecularShader inherited;
	
__published:
	__property LightPower = {default=0};
	__property FogSupport = {default=2};
public:
	/* TGLCustomGLSLMLDiffuseSpecularShader.Create */ inline __fastcall virtual TGLSLMLDiffuseSpecularShader(System::Classes::TComponent* AOwner) : TGLCustomGLSLMLDiffuseSpecularShader(AOwner) { }
	
public:
	/* TGLCustomGLSLShader.Destroy */ inline __fastcall virtual ~TGLSLMLDiffuseSpecularShader(void) { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Glsldiffusespecularshader */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLSLDIFFUSESPECULARSHADER)
using namespace Glsldiffusespecularshader;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// GlsldiffusespecularshaderHPP
