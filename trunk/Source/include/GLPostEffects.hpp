// CodeGear C++Builder
// Copyright (c) 1995, 2012 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLPostEffects.pas' rev: 24.00 (Win32)

#ifndef GlposteffectsHPP
#define GlposteffectsHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <System.Classes.hpp>	// Pascal unit
#include <System.SysUtils.hpp>	// Pascal unit
#include <GLScene.hpp>	// Pascal unit
#include <GLTexture.hpp>	// Pascal unit
#include <OpenGLTokens.hpp>	// Pascal unit
#include <GLGraphics.hpp>	// Pascal unit
#include <GLStrings.hpp>	// Pascal unit
#include <GLCustomShader.hpp>	// Pascal unit
#include <GLContext.hpp>	// Pascal unit
#include <VectorGeometry.hpp>	// Pascal unit
#include <GLRenderContextInfo.hpp>	// Pascal unit
#include <GLMaterial.hpp>	// Pascal unit
#include <GLTextureFormat.hpp>	// Pascal unit
#include <BaseClasses.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Glposteffects
{
//-- type declarations -------------------------------------------------------
class DELPHICLASS EGLPostShaderHolderException;
#pragma pack(push,4)
class PASCALIMPLEMENTATION EGLPostShaderHolderException : public System::Sysutils::Exception
{
	typedef System::Sysutils::Exception inherited;
	
public:
	/* Exception.Create */ inline __fastcall EGLPostShaderHolderException(const System::UnicodeString Msg) : System::Sysutils::Exception(Msg) { }
	/* Exception.CreateFmt */ inline __fastcall EGLPostShaderHolderException(const System::UnicodeString Msg, System::TVarRec const *Args, const int Args_Size) : System::Sysutils::Exception(Msg, Args, Args_Size) { }
	/* Exception.CreateRes */ inline __fastcall EGLPostShaderHolderException(NativeUInt Ident)/* overload */ : System::Sysutils::Exception(Ident) { }
	/* Exception.CreateRes */ inline __fastcall EGLPostShaderHolderException(System::PResStringRec ResStringRec)/* overload */ : System::Sysutils::Exception(ResStringRec) { }
	/* Exception.CreateResFmt */ inline __fastcall EGLPostShaderHolderException(NativeUInt Ident, System::TVarRec const *Args, const int Args_Size)/* overload */ : System::Sysutils::Exception(Ident, Args, Args_Size) { }
	/* Exception.CreateResFmt */ inline __fastcall EGLPostShaderHolderException(System::PResStringRec ResStringRec, System::TVarRec const *Args, const int Args_Size)/* overload */ : System::Sysutils::Exception(ResStringRec, Args, Args_Size) { }
	/* Exception.CreateHelp */ inline __fastcall EGLPostShaderHolderException(const System::UnicodeString Msg, int AHelpContext) : System::Sysutils::Exception(Msg, AHelpContext) { }
	/* Exception.CreateFmtHelp */ inline __fastcall EGLPostShaderHolderException(const System::UnicodeString Msg, System::TVarRec const *Args, const int Args_Size, int AHelpContext) : System::Sysutils::Exception(Msg, Args, Args_Size, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall EGLPostShaderHolderException(NativeUInt Ident, int AHelpContext)/* overload */ : System::Sysutils::Exception(Ident, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall EGLPostShaderHolderException(System::PResStringRec ResStringRec, int AHelpContext)/* overload */ : System::Sysutils::Exception(ResStringRec, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall EGLPostShaderHolderException(System::PResStringRec ResStringRec, System::TVarRec const *Args, const int Args_Size, int AHelpContext)/* overload */ : System::Sysutils::Exception(ResStringRec, Args, Args_Size, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall EGLPostShaderHolderException(NativeUInt Ident, System::TVarRec const *Args, const int Args_Size, int AHelpContext)/* overload */ : System::Sysutils::Exception(Ident, Args, Args_Size, AHelpContext) { }
	/* Exception.Destroy */ inline __fastcall virtual ~EGLPostShaderHolderException(void) { }
	
};

#pragma pack(pop)

class DELPHICLASS TGLPostShaderCollectionItem;
class DELPHICLASS TGLPostShaderHolder;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLPostShaderCollectionItem : public System::Classes::TCollectionItem
{
	typedef System::Classes::TCollectionItem inherited;
	
private:
	Glmaterial::TGLShader* FShader;
	Glcustomshader::_di_IGLPostShader FPostShaderInterface;
	void __fastcall SetShader(Glmaterial::TGLShader* const Value);
	
protected:
	TGLPostShaderHolder* __fastcall GetRealOwner(void);
	virtual System::UnicodeString __fastcall GetDisplayName(void);
	
public:
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	
__published:
	__property Glmaterial::TGLShader* Shader = {read=FShader, write=SetShader};
public:
	/* TCollectionItem.Create */ inline __fastcall virtual TGLPostShaderCollectionItem(System::Classes::TCollection* Collection) : System::Classes::TCollectionItem(Collection) { }
	/* TCollectionItem.Destroy */ inline __fastcall virtual ~TGLPostShaderCollectionItem(void) { }
	
};

#pragma pack(pop)

class DELPHICLASS TGLPostShaderCollection;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLPostShaderCollection : public System::Classes::TOwnedCollection
{
	typedef System::Classes::TOwnedCollection inherited;
	
public:
	TGLPostShaderCollectionItem* operator[](const int Index) { return Items[Index]; }
	
private:
	TGLPostShaderCollectionItem* __fastcall GetItems(const int Index);
	void __fastcall SetItems(const int Index, TGLPostShaderCollectionItem* const Value);
	
public:
	void __fastcall Remove(Glmaterial::TGLShader* const Item);
	HIDESBASE TGLPostShaderCollectionItem* __fastcall Add(void);
	__property TGLPostShaderCollectionItem* Items[const int Index] = {read=GetItems, write=SetItems/*, default*/};
public:
	/* TOwnedCollection.Create */ inline __fastcall TGLPostShaderCollection(System::Classes::TPersistent* AOwner, System::Classes::TCollectionItemClass ItemClass) : System::Classes::TOwnedCollection(AOwner, ItemClass) { }
	
public:
	/* TCollection.Destroy */ inline __fastcall virtual ~TGLPostShaderCollection(void) { }
	
};

#pragma pack(pop)

class PASCALIMPLEMENTATION TGLPostShaderHolder : public Glscene::TGLBaseSceneObject
{
	typedef Glscene::TGLBaseSceneObject inherited;
	
private:
	TGLPostShaderCollection* FShaders;
	Glcontext::TGLTextureHandle* FTempTexture;
	Glrendercontextinfo::TGLSize FPreviousViewportSize;
	Gltextureformat::TGLTextureTarget FTempTextureTarget;
	void __fastcall SetShaders(TGLPostShaderCollection* const Value);
	
protected:
	virtual void __fastcall Notification(System::Classes::TComponent* AComponent, System::Classes::TOperation Operation);
	
public:
	__fastcall virtual TGLPostShaderHolder(System::Classes::TComponent* Owner);
	__fastcall virtual ~TGLPostShaderHolder(void);
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	virtual void __fastcall DoRender(Glrendercontextinfo::TRenderContextInfo &rci, bool renderSelf, bool renderChildren);
	
__published:
	__property Gltextureformat::TGLTextureTarget TempTextureTarget = {read=FTempTextureTarget, write=FTempTextureTarget, default=2};
	__property TGLPostShaderCollection* Shaders = {read=FShaders, write=SetShaders};
	__property Visible = {default=1};
	__property OnProgress;
public:
	/* TGLBaseSceneObject.CreateAsChild */ inline __fastcall TGLPostShaderHolder(Glscene::TGLBaseSceneObject* aParentOwner) : Glscene::TGLBaseSceneObject(aParentOwner) { }
	
};


struct DECLSPEC_DRECORD TGLPostEffectColor
{
public:
	System::Byte R;
	System::Byte G;
	System::Byte B;
	System::Byte A;
};


typedef System::DynamicArray<TGLPostEffectColor> TGLPostEffectBuffer;

typedef void __fastcall (__closure *TGLOnCustomPostEffectEvent)(System::TObject* Sender, Glrendercontextinfo::TRenderContextInfo &rci, TGLPostEffectBuffer &Buffer);

enum TGLPostEffectPreset : unsigned char { pepNone, pepGray, pepNegative, pepDistort, pepNoise, pepNightVision, pepBlur, pepCustom };

class DELPHICLASS TGLPostEffect;
class PASCALIMPLEMENTATION TGLPostEffect : public Glscene::TGLBaseSceneObject
{
	typedef Glscene::TGLBaseSceneObject inherited;
	
private:
	TGLOnCustomPostEffectEvent FOnCustomEffect;
	TGLPostEffectPreset FPreset;
	TGLPostEffectBuffer FRenderBuffer;
	
protected:
	virtual void __fastcall MakeGrayEffect(void);
	virtual void __fastcall MakeNegativeEffect(void);
	virtual void __fastcall MakeDistortEffect(void);
	virtual void __fastcall MakeNoiseEffect(void);
	virtual void __fastcall MakeNightVisionEffect(void);
	virtual void __fastcall MakeBlurEffect(Glrendercontextinfo::TRenderContextInfo &rci);
	virtual void __fastcall DoOnCustomEffect(Glrendercontextinfo::TRenderContextInfo &rci, TGLPostEffectBuffer &Buffer);
	
public:
	virtual void __fastcall DoRender(Glrendercontextinfo::TRenderContextInfo &rci, bool renderSelf, bool renderChildren);
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	
__published:
	__property TGLPostEffectPreset Preset = {read=FPreset, write=FPreset, default=0};
	__property TGLOnCustomPostEffectEvent OnCustomEffect = {read=FOnCustomEffect, write=FOnCustomEffect};
	__property Visible = {default=1};
	__property OnProgress;
public:
	/* TGLBaseSceneObject.Create */ inline __fastcall virtual TGLPostEffect(System::Classes::TComponent* AOwner) : Glscene::TGLBaseSceneObject(AOwner) { }
	/* TGLBaseSceneObject.CreateAsChild */ inline __fastcall TGLPostEffect(Glscene::TGLBaseSceneObject* aParentOwner) : Glscene::TGLBaseSceneObject(aParentOwner) { }
	/* TGLBaseSceneObject.Destroy */ inline __fastcall virtual ~TGLPostEffect(void) { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Glposteffects */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLPOSTEFFECTS)
using namespace Glposteffects;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// GlposteffectsHPP
