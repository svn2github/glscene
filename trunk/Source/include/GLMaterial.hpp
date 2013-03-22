// CodeGear C++Builder
// Copyright (c) 1995, 2012 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLMaterial.pas' rev: 24.00 (Win32)

#ifndef GlmaterialHPP
#define GlmaterialHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <System.Classes.hpp>	// Pascal unit
#include <System.SysUtils.hpp>	// Pascal unit
#include <GLRenderContextInfo.hpp>	// Pascal unit
#include <BaseClasses.hpp>	// Pascal unit
#include <OpenGLTokens.hpp>	// Pascal unit
#include <GLContext.hpp>	// Pascal unit
#include <GLTexture.hpp>	// Pascal unit
#include <GLColor.hpp>	// Pascal unit
#include <GLCoordinates.hpp>	// Pascal unit
#include <VectorGeometry.hpp>	// Pascal unit
#include <PersistentClasses.hpp>	// Pascal unit
#include <GLCrossPlatform.hpp>	// Pascal unit
#include <GLState.hpp>	// Pascal unit
#include <GLTextureFormat.hpp>	// Pascal unit
#include <VectorTypes.hpp>	// Pascal unit
#include <Vcl.Graphics.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Glmaterial
{
//-- type declarations -------------------------------------------------------
__interface IGLMaterialLibrarySupported;
typedef System::DelphiInterface<IGLMaterialLibrarySupported> _di_IGLMaterialLibrarySupported;
class DELPHICLASS TGLAbstractMaterialLibrary;
__interface  INTERFACE_UUID("{8E442AF9-D212-4A5E-8A88-92F798BABFD1}") IGLMaterialLibrarySupported  : public System::IInterface 
{
	
public:
	virtual TGLAbstractMaterialLibrary* __fastcall GetMaterialLibrary(void) = 0 ;
};

enum TGLShaderStyle : unsigned char { ssHighLevel, ssLowLevel, ssReplace };

enum TGLShaderFailedInitAction : unsigned char { fiaSilentDisable, fiaRaiseStandardException, fiaRaiseHandledException, fiaReRaiseException };

class DELPHICLASS TGLShader;
class DELPHICLASS TGLLibMaterial;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLShader : public Baseclasses::TGLUpdateAbleComponent
{
	typedef Baseclasses::TGLUpdateAbleComponent inherited;
	
private:
	bool FEnabled;
	System::Classes::TList* FLibMatUsers;
	Glcontext::TGLVirtualHandle* FVirtualHandle;
	TGLShaderStyle FShaderStyle;
	int FUpdateCount;
	bool FShaderActive;
	TGLShaderFailedInitAction FFailedInitAction;
	
protected:
	DYNAMIC void __fastcall DoInitialize(Glrendercontextinfo::TRenderContextInfo &rci, System::TObject* Sender);
	virtual void __fastcall DoApply(Glrendercontextinfo::TRenderContextInfo &rci, System::TObject* Sender);
	virtual bool __fastcall DoUnApply(Glrendercontextinfo::TRenderContextInfo &rci);
	DYNAMIC void __fastcall DoFinalize(void);
	bool __fastcall GetShaderInitialized(void);
	void __fastcall InitializeShader(Glrendercontextinfo::TRenderContextInfo &rci, System::TObject* Sender);
	void __fastcall FinalizeShader(void);
	void __fastcall OnVirtualHandleAllocate(Glcontext::TGLVirtualHandle* sender, unsigned &handle);
	void __fastcall OnVirtualHandleDestroy(Glcontext::TGLVirtualHandle* sender, unsigned &handle);
	void __fastcall SetEnabled(bool val);
	__property bool ShaderInitialized = {read=GetShaderInitialized, nodefault};
	__property bool ShaderActive = {read=FShaderActive, nodefault};
	void __fastcall RegisterUser(TGLLibMaterial* libMat);
	void __fastcall UnRegisterUser(TGLLibMaterial* libMat);
	virtual void __fastcall HandleFailedInitialization(const System::UnicodeString LastErrorMessage = System::UnicodeString());
	virtual System::UnicodeString __fastcall GetStardardNotSupportedMessage(void);
	
public:
	__fastcall virtual TGLShader(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TGLShader(void);
	virtual void __fastcall NotifyChange(System::TObject* Sender);
	void __fastcall BeginUpdate(void);
	void __fastcall EndUpdate(void);
	void __fastcall Apply(Glrendercontextinfo::TRenderContextInfo &rci, System::TObject* Sender);
	bool __fastcall UnApply(Glrendercontextinfo::TRenderContextInfo &rci);
	__property TGLShaderStyle ShaderStyle = {read=FShaderStyle, write=FShaderStyle, default=1};
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	virtual bool __fastcall ShaderSupported(void);
	__property TGLShaderFailedInitAction FailedInitAction = {read=FFailedInitAction, write=FFailedInitAction, default=1};
	
__published:
	__property bool Enabled = {read=FEnabled, write=SetEnabled, default=1};
};

#pragma pack(pop)

typedef System::TMetaClass* TGLShaderClass;

typedef System::Byte TShininess;

class DELPHICLASS TGLFaceProperties;
class PASCALIMPLEMENTATION TGLFaceProperties : public Baseclasses::TGLUpdateAbleObject
{
	typedef Baseclasses::TGLUpdateAbleObject inherited;
	
private:
	Glcolor::TGLColor* FAmbient;
	Glcolor::TGLColor* FDiffuse;
	Glcolor::TGLColor* FSpecular;
	Glcolor::TGLColor* FEmission;
	TShininess FShininess;
	
protected:
	void __fastcall SetAmbient(Glcolor::TGLColor* AValue);
	void __fastcall SetDiffuse(Glcolor::TGLColor* AValue);
	void __fastcall SetEmission(Glcolor::TGLColor* AValue);
	void __fastcall SetSpecular(Glcolor::TGLColor* AValue);
	void __fastcall SetShininess(TShininess AValue);
	
public:
	__fastcall virtual TGLFaceProperties(System::Classes::TPersistent* AOwner);
	__fastcall virtual ~TGLFaceProperties(void);
	void __fastcall Apply(Glrendercontextinfo::TRenderContextInfo &rci, Glstate::TCullFaceMode aFace);
	void __fastcall ApplyNoLighting(Glrendercontextinfo::TRenderContextInfo &rci, Glstate::TCullFaceMode aFace);
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	
__published:
	__property Glcolor::TGLColor* Ambient = {read=FAmbient, write=SetAmbient};
	__property Glcolor::TGLColor* Diffuse = {read=FDiffuse, write=SetDiffuse};
	__property Glcolor::TGLColor* Emission = {read=FEmission, write=SetEmission};
	__property TShininess Shininess = {read=FShininess, write=SetShininess, default=0};
	__property Glcolor::TGLColor* Specular = {read=FSpecular, write=SetSpecular};
};


class DELPHICLASS TGLDepthProperties;
class PASCALIMPLEMENTATION TGLDepthProperties : public Baseclasses::TGLUpdateAbleObject
{
	typedef Baseclasses::TGLUpdateAbleObject inherited;
	
private:
	bool FDepthTest;
	bool FDepthWrite;
	float FZNear;
	float FZFar;
	Glstate::TComparisonFunction FCompareFunc;
	bool FDepthClamp;
	
protected:
	void __fastcall SetZNear(float Value);
	void __fastcall SetZFar(float Value);
	void __fastcall SetCompareFunc(Glstate::TComparisonFunction Value);
	void __fastcall SetDepthTest(bool Value);
	void __fastcall SetDepthWrite(bool Value);
	void __fastcall SetDepthClamp(bool Value);
	bool __fastcall StoreZNear(void);
	bool __fastcall StoreZFar(void);
	
public:
	__fastcall virtual TGLDepthProperties(System::Classes::TPersistent* AOwner);
	void __fastcall Apply(Glrendercontextinfo::TRenderContextInfo &rci);
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	
__published:
	__property float ZNear = {read=FZNear, write=SetZNear, stored=StoreZNear};
	__property float ZFar = {read=FZFar, write=SetZFar, stored=StoreZFar};
	__property Glstate::TComparisonFunction DepthCompareFunction = {read=FCompareFunc, write=SetCompareFunc, default=3};
	__property bool DepthTest = {read=FDepthTest, write=SetDepthTest, default=1};
	__property bool DepthWrite = {read=FDepthWrite, write=SetDepthWrite, default=1};
	__property bool DepthClamp = {read=FDepthClamp, write=SetDepthClamp, default=0};
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TGLDepthProperties(void) { }
	
};


typedef System::UnicodeString TGLLibMaterialName;

typedef Glstate::TComparisonFunction TGlAlphaFunc;

class DELPHICLASS TGLBlendingParameters;
class PASCALIMPLEMENTATION TGLBlendingParameters : public Baseclasses::TGLUpdateAbleObject
{
	typedef Baseclasses::TGLUpdateAbleObject inherited;
	
private:
	bool FUseAlphaFunc;
	bool FUseBlendFunc;
	bool FSeparateBlendFunc;
	Glstate::TComparisonFunction FAlphaFuncType;
	float FAlphaFuncRef;
	Glstate::TBlendFunction FBlendFuncSFactor;
	Glstate::TBlendFunction FBlendFuncDFactor;
	Glstate::TBlendFunction FAlphaBlendFuncSFactor;
	Glstate::TBlendFunction FAlphaBlendFuncDFactor;
	void __fastcall SetUseAlphaFunc(const bool Value);
	void __fastcall SetUseBlendFunc(const bool Value);
	void __fastcall SetSeparateBlendFunc(const bool Value);
	void __fastcall SetAlphaFuncRef(const float Value);
	void __fastcall SetAlphaFuncType(const Glstate::TComparisonFunction Value);
	void __fastcall SetBlendFuncDFactor(const Glstate::TBlendFunction Value);
	void __fastcall SetBlendFuncSFactor(const Glstate::TBlendFunction Value);
	void __fastcall SetAlphaBlendFuncDFactor(const Glstate::TBlendFunction Value);
	void __fastcall SetAlphaBlendFuncSFactor(const Glstate::TBlendFunction Value);
	bool __fastcall StoreAlphaFuncRef(void);
	
public:
	__fastcall virtual TGLBlendingParameters(System::Classes::TPersistent* AOwner);
	void __fastcall Apply(Glrendercontextinfo::TRenderContextInfo &rci);
	
__published:
	__property bool UseAlphaFunc = {read=FUseAlphaFunc, write=SetUseAlphaFunc, default=0};
	__property Glstate::TComparisonFunction AlphaFunctType = {read=FAlphaFuncType, write=SetAlphaFuncType, default=5};
	__property float AlphaFuncRef = {read=FAlphaFuncRef, write=SetAlphaFuncRef, stored=StoreAlphaFuncRef};
	__property bool UseBlendFunc = {read=FUseBlendFunc, write=SetUseBlendFunc, default=1};
	__property bool SeparateBlendFunc = {read=FSeparateBlendFunc, write=SetSeparateBlendFunc, default=0};
	__property Glstate::TBlendFunction BlendFuncSFactor = {read=FBlendFuncSFactor, write=SetBlendFuncSFactor, default=6};
	__property Glstate::TBlendFunction BlendFuncDFactor = {read=FBlendFuncDFactor, write=SetBlendFuncDFactor, default=7};
	__property Glstate::TBlendFunction AlphaBlendFuncSFactor = {read=FAlphaBlendFuncSFactor, write=SetAlphaBlendFuncSFactor, default=6};
	__property Glstate::TBlendFunction AlphaBlendFuncDFactor = {read=FAlphaBlendFuncDFactor, write=SetAlphaBlendFuncDFactor, default=7};
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TGLBlendingParameters(void) { }
	
};


enum TBlendingMode : unsigned char { bmOpaque, bmTransparency, bmAdditive, bmAlphaTest50, bmAlphaTest100, bmModulate, bmCustom };

enum TFaceCulling : unsigned char { fcBufferDefault, fcCull, fcNoCull };

enum TMaterialOption : unsigned char { moIgnoreFog, moNoLighting };

typedef System::Set<TMaterialOption, TMaterialOption::moIgnoreFog, TMaterialOption::moNoLighting>  TMaterialOptions;

class DELPHICLASS TGLMaterial;
class DELPHICLASS TGLAbstractLibMaterial;
class DELPHICLASS TGLMaterialLibrary;
class PASCALIMPLEMENTATION TGLMaterial : public Baseclasses::TGLUpdateAbleObject
{
	typedef Baseclasses::TGLUpdateAbleObject inherited;
	
private:
	TGLFaceProperties* FFrontProperties;
	TGLFaceProperties* FBackProperties;
	TGLDepthProperties* FDepthProperties;
	TBlendingMode FBlendingMode;
	TGLBlendingParameters* FBlendingParams;
	Gltexture::TGLTexture* FTexture;
	Gltexture::TGLTextureEx* FTextureEx;
	TGLAbstractMaterialLibrary* FMaterialLibrary;
	System::UnicodeString FLibMaterialName;
	TMaterialOptions FMaterialOptions;
	TFaceCulling FFaceCulling;
	Glstate::TPolygonMode FPolygonMode;
	TGLAbstractLibMaterial* currentLibMaterial;
	TGLAbstractMaterialLibrary* __fastcall GetMaterialLibrary(void);
	
protected:
	TGLFaceProperties* __fastcall GetBackProperties(void);
	void __fastcall SetBackProperties(TGLFaceProperties* Values);
	void __fastcall SetFrontProperties(TGLFaceProperties* Values);
	void __fastcall SetDepthProperties(TGLDepthProperties* Values);
	void __fastcall SetBlendingMode(const TBlendingMode val);
	void __fastcall SetMaterialOptions(const TMaterialOptions val);
	Gltexture::TGLTexture* __fastcall GetTexture(void);
	void __fastcall SetTexture(Gltexture::TGLTexture* ATexture);
	void __fastcall SetMaterialLibrary(TGLAbstractMaterialLibrary* const val);
	void __fastcall SetLibMaterialName(const System::UnicodeString val);
	void __fastcall SetFaceCulling(const TFaceCulling val);
	void __fastcall SetPolygonMode(Glstate::TPolygonMode AValue);
	Gltexture::TGLTextureEx* __fastcall GetTextureEx(void);
	void __fastcall SetTextureEx(Gltexture::TGLTextureEx* const value);
	bool __fastcall StoreTextureEx(void);
	void __fastcall SetBlendingParams(TGLBlendingParameters* const Value);
	void __fastcall NotifyLibMaterialDestruction(void);
	bool __fastcall StoreMaterialProps(void);
	
public:
	__fastcall virtual TGLMaterial(System::Classes::TPersistent* AOwner);
	__fastcall virtual ~TGLMaterial(void);
	void __fastcall PrepareBuildList(void);
	void __fastcall Apply(Glrendercontextinfo::TRenderContextInfo &rci);
	bool __fastcall UnApply(Glrendercontextinfo::TRenderContextInfo &rci);
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	virtual void __fastcall NotifyChange(System::TObject* Sender);
	void __fastcall NotifyTexMapChange(System::TObject* Sender);
	void __fastcall DestroyHandles(void);
	void __fastcall Loaded(void);
	bool __fastcall Blended(void);
	bool __fastcall HasSecondaryTexture(void);
	bool __fastcall MaterialIsLinkedToLib(void);
	Gltexture::TGLTexture* __fastcall GetActualPrimaryTexture(void);
	TGLMaterial* __fastcall GetActualPrimaryMaterial(void);
	TGLLibMaterial* __fastcall GetLibMaterial(void);
	void __fastcall QuickAssignMaterial(TGLMaterialLibrary* const MaterialLibrary, TGLLibMaterial* const Material);
	
__published:
	__property TGLFaceProperties* BackProperties = {read=GetBackProperties, write=SetBackProperties, stored=StoreMaterialProps};
	__property TGLFaceProperties* FrontProperties = {read=FFrontProperties, write=SetFrontProperties, stored=StoreMaterialProps};
	__property TGLDepthProperties* DepthProperties = {read=FDepthProperties, write=SetDepthProperties, stored=StoreMaterialProps};
	__property TBlendingMode BlendingMode = {read=FBlendingMode, write=SetBlendingMode, stored=StoreMaterialProps, default=0};
	__property TGLBlendingParameters* BlendingParams = {read=FBlendingParams, write=SetBlendingParams};
	__property TMaterialOptions MaterialOptions = {read=FMaterialOptions, write=SetMaterialOptions, default=0};
	__property Gltexture::TGLTexture* Texture = {read=GetTexture, write=SetTexture, stored=StoreMaterialProps};
	__property TFaceCulling FaceCulling = {read=FFaceCulling, write=SetFaceCulling, default=0};
	__property TGLAbstractMaterialLibrary* MaterialLibrary = {read=FMaterialLibrary, write=SetMaterialLibrary};
	__property System::UnicodeString LibMaterialName = {read=FLibMaterialName, write=SetLibMaterialName};
	__property Gltexture::TGLTextureEx* TextureEx = {read=GetTextureEx, write=SetTextureEx, stored=StoreTextureEx};
	__property Glstate::TPolygonMode PolygonMode = {read=FPolygonMode, write=SetPolygonMode, default=0};
private:
	void *__IGLTextureNotifyAble;	/* Gltexture::IGLTextureNotifyAble */
	void *__IGLMaterialLibrarySupported;	/* IGLMaterialLibrarySupported */
	
public:
	#if defined(MANAGED_INTERFACE_OPERATORS)
	// {0D9DC0B0-ECE4-4513-A8A1-5AE7022C9426}
	operator Gltexture::_di_IGLTextureNotifyAble()
	{
		Gltexture::_di_IGLTextureNotifyAble intf;
		GetInterface(intf);
		return intf;
	}
	#else
	operator Gltexture::IGLTextureNotifyAble*(void) { return (Gltexture::IGLTextureNotifyAble*)&__IGLTextureNotifyAble; }
	#endif
	#if defined(MANAGED_INTERFACE_OPERATORS)
	// {00079A6C-D46E-4126-86EE-F9E2951B4593}
	operator Baseclasses::_di_IGLNotifyAble()
	{
		Baseclasses::_di_IGLNotifyAble intf;
		GetInterface(intf);
		return intf;
	}
	#else
	operator Baseclasses::IGLNotifyAble*(void) { return (Baseclasses::IGLNotifyAble*)&__IGLTextureNotifyAble; }
	#endif
	#if defined(MANAGED_INTERFACE_OPERATORS)
	// {8E442AF9-D212-4A5E-8A88-92F798BABFD1}
	operator _di_IGLMaterialLibrarySupported()
	{
		_di_IGLMaterialLibrarySupported intf;
		GetInterface(intf);
		return intf;
	}
	#else
	operator IGLMaterialLibrarySupported*(void) { return (IGLMaterialLibrarySupported*)&__IGLMaterialLibrarySupported; }
	#endif
	
};


#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLAbstractLibMaterial : public System::Classes::TCollectionItem
{
	typedef System::Classes::TCollectionItem inherited;
	
protected:
	System::Classes::TList* FUserList;
	System::UnicodeString FName;
	int FNameHashKey;
	int FTag;
	bool FNotifying;
	TGLAbstractMaterialLibrary* __fastcall GetMaterialLibrary(void);
	HRESULT __stdcall QueryInterface(const GUID &IID, /* out */ void *Obj);
	int __stdcall _AddRef(void);
	int __stdcall _Release(void);
	virtual System::UnicodeString __fastcall GetDisplayName(void);
	__classmethod int __fastcall ComputeNameHashKey(const System::UnicodeString name);
	void __fastcall SetName(const System::UnicodeString val);
	virtual void __fastcall Loaded(void);
	
public:
	__fastcall virtual TGLAbstractLibMaterial(System::Classes::TCollection* ACollection);
	__fastcall virtual ~TGLAbstractLibMaterial(void);
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	virtual void __fastcall Apply(Glrendercontextinfo::TRenderContextInfo &ARci);
	virtual bool __fastcall UnApply(Glrendercontextinfo::TRenderContextInfo &ARci);
	void __fastcall RegisterUser(Baseclasses::TGLUpdateAbleObject* obj)/* overload */;
	void __fastcall UnregisterUser(Baseclasses::TGLUpdateAbleObject* obj)/* overload */;
	void __fastcall RegisterUser(Baseclasses::TGLUpdateAbleComponent* comp)/* overload */;
	void __fastcall UnregisterUser(Baseclasses::TGLUpdateAbleComponent* comp)/* overload */;
	void __fastcall RegisterUser(TGLLibMaterial* libMaterial)/* overload */;
	void __fastcall UnregisterUser(TGLLibMaterial* libMaterial)/* overload */;
	void __fastcall NotifyUsers(void);
	bool __fastcall IsUsed(void);
	__property int NameHashKey = {read=FNameHashKey, nodefault};
	virtual void __fastcall NotifyChange(System::TObject* Sender);
	virtual bool __fastcall Blended(void);
	__property TGLAbstractMaterialLibrary* MaterialLibrary = {read=GetMaterialLibrary};
	
__published:
	__property System::UnicodeString Name = {read=FName, write=SetName};
	__property int Tag = {read=FTag, write=FTag, nodefault};
private:
	void *__IGLNotifyAble;	/* Baseclasses::IGLNotifyAble */
	void *__IGLMaterialLibrarySupported;	/* IGLMaterialLibrarySupported */
	
public:
	#if defined(MANAGED_INTERFACE_OPERATORS)
	// {00079A6C-D46E-4126-86EE-F9E2951B4593}
	operator Baseclasses::_di_IGLNotifyAble()
	{
		Baseclasses::_di_IGLNotifyAble intf;
		GetInterface(intf);
		return intf;
	}
	#else
	operator Baseclasses::IGLNotifyAble*(void) { return (Baseclasses::IGLNotifyAble*)&__IGLNotifyAble; }
	#endif
	#if defined(MANAGED_INTERFACE_OPERATORS)
	// {8E442AF9-D212-4A5E-8A88-92F798BABFD1}
	operator _di_IGLMaterialLibrarySupported()
	{
		_di_IGLMaterialLibrarySupported intf;
		GetInterface(intf);
		return intf;
	}
	#else
	operator IGLMaterialLibrarySupported*(void) { return (IGLMaterialLibrarySupported*)&__IGLMaterialLibrarySupported; }
	#endif
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLLibMaterial : public TGLAbstractLibMaterial
{
	typedef TGLAbstractLibMaterial inherited;
	
private:
	TGLMaterial* FMaterial;
	Glcoordinates::TGLCoordinates3* FTextureOffset;
	Glcoordinates::TGLCoordinates3* FTextureScale;
	float FTextureRotate;
	bool FTextureMatrixIsIdentity;
	bool FTextureOverride;
	Vectortypes::TMatrix4f FTextureMatrix;
	System::UnicodeString FTexture2Name;
	TGLShader* FShader;
	TGLLibMaterial* libMatTexture2;
	
protected:
	virtual void __fastcall Loaded(void);
	void __fastcall SetMaterial(TGLMaterial* const val);
	void __fastcall SetTextureOffset(Glcoordinates::TGLCoordinates3* const val);
	void __fastcall SetTextureScale(Glcoordinates::TGLCoordinates3* const val);
	void __fastcall SetTextureMatrix(const Vectortypes::TMatrix4f &Value);
	void __fastcall SetTexture2Name(const System::UnicodeString val);
	void __fastcall SetShader(TGLShader* const val);
	void __fastcall SetTextureRotate(float Value);
	bool __fastcall StoreTextureRotate(void);
	void __fastcall CalculateTextureMatrix(void);
	void __fastcall DestroyHandles(void);
	void __fastcall DoOnTextureNeeded(System::TObject* Sender, System::UnicodeString &textureFileName);
	void __fastcall OnNotifyChange(System::TObject* Sender);
	
public:
	__fastcall virtual TGLLibMaterial(System::Classes::TCollection* ACollection);
	__fastcall virtual ~TGLLibMaterial(void);
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	void __fastcall PrepareBuildList(void);
	virtual void __fastcall Apply(Glrendercontextinfo::TRenderContextInfo &ARci);
	virtual bool __fastcall UnApply(Glrendercontextinfo::TRenderContextInfo &ARci);
	void __fastcall NotifyUsersOfTexMapChange(void);
	__property Vectortypes::TMatrix4f TextureMatrix = {read=FTextureMatrix, write=SetTextureMatrix};
	__property bool TextureMatrixIsIdentity = {read=FTextureMatrixIsIdentity, nodefault};
	void __fastcall NotifyTexMapChange(System::TObject* Sender);
	virtual bool __fastcall Blended(void);
	
__published:
	__property TGLMaterial* Material = {read=FMaterial, write=SetMaterial};
	__property Glcoordinates::TGLCoordinates3* TextureOffset = {read=FTextureOffset, write=SetTextureOffset};
	__property Glcoordinates::TGLCoordinates3* TextureScale = {read=FTextureScale, write=SetTextureScale};
	__property float TextureRotate = {read=FTextureRotate, write=SetTextureRotate, stored=StoreTextureRotate};
	__property System::UnicodeString Texture2Name = {read=FTexture2Name, write=SetTexture2Name};
	__property TGLShader* Shader = {read=FShader, write=SetShader};
private:
	void *__IGLTextureNotifyAble;	/* Gltexture::IGLTextureNotifyAble */
	
public:
	#if defined(MANAGED_INTERFACE_OPERATORS)
	// {0D9DC0B0-ECE4-4513-A8A1-5AE7022C9426}
	operator Gltexture::_di_IGLTextureNotifyAble()
	{
		Gltexture::_di_IGLTextureNotifyAble intf;
		GetInterface(intf);
		return intf;
	}
	#else
	operator Gltexture::IGLTextureNotifyAble*(void) { return (Gltexture::IGLTextureNotifyAble*)&__IGLTextureNotifyAble; }
	#endif
	
};

#pragma pack(pop)

class DELPHICLASS TGLAbstractLibMaterials;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLAbstractLibMaterials : public System::Classes::TOwnedCollection
{
	typedef System::Classes::TOwnedCollection inherited;
	
protected:
	void __fastcall Loaded(void);
	TGLAbstractLibMaterial* __fastcall GetMaterial(const System::UnicodeString AName);
	
public:
	virtual System::UnicodeString __fastcall MakeUniqueName(const System::UnicodeString nameRoot);
public:
	/* TOwnedCollection.Create */ inline __fastcall TGLAbstractLibMaterials(System::Classes::TPersistent* AOwner, System::Classes::TCollectionItemClass ItemClass) : System::Classes::TOwnedCollection(AOwner, ItemClass) { }
	
public:
	/* TCollection.Destroy */ inline __fastcall virtual ~TGLAbstractLibMaterials(void) { }
	
};

#pragma pack(pop)

class DELPHICLASS TGLLibMaterials;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLLibMaterials : public TGLAbstractLibMaterials
{
	typedef TGLAbstractLibMaterials inherited;
	
public:
	TGLLibMaterial* operator[](int index) { return Items[index]; }
	
protected:
	void __fastcall SetItems(int index, TGLLibMaterial* const val);
	TGLLibMaterial* __fastcall GetItems(int index);
	void __fastcall DestroyHandles(void);
	
public:
	__fastcall TGLLibMaterials(System::Classes::TComponent* AOwner);
	HIDESBASE System::Classes::TPersistent* __fastcall Owner(void);
	int __fastcall IndexOf(TGLLibMaterial* const Item);
	HIDESBASE TGLLibMaterial* __fastcall Add(void);
	HIDESBASE TGLLibMaterial* __fastcall FindItemID(int ID);
	__property TGLLibMaterial* Items[int index] = {read=GetItems, write=SetItems/*, default*/};
	TGLLibMaterial* __fastcall GetLibMaterialByName(const System::UnicodeString AName);
	int __fastcall GetTextureIndex(Gltexture::TGLTexture* const Texture);
	int __fastcall GetMaterialIndex(TGLMaterial* const Material);
	System::UnicodeString __fastcall GetNameOfTexture(Gltexture::TGLTexture* const Texture);
	System::UnicodeString __fastcall GetNameOfLibMaterial(TGLLibMaterial* const Material);
	void __fastcall PrepareBuildList(void);
	void __fastcall DeleteUnusedMaterials(void);
public:
	/* TCollection.Destroy */ inline __fastcall virtual ~TGLLibMaterials(void) { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLAbstractMaterialLibrary : public Baseclasses::TGLCadenceAbleComponent
{
	typedef Baseclasses::TGLCadenceAbleComponent inherited;
	
protected:
	TGLAbstractLibMaterials* FMaterials;
	TGLAbstractLibMaterial* FLastAppliedMaterial;
	System::UnicodeString FTexturePaths;
	System::Classes::TStringList* FTexturePathList;
	void __fastcall SetTexturePaths(const System::UnicodeString val);
	__property System::UnicodeString TexturePaths = {read=FTexturePaths, write=SetTexturePaths};
	virtual void __fastcall Loaded(void);
	
public:
	void __fastcall SetNamesToTStrings(System::Classes::TStrings* AStrings);
	virtual bool __fastcall ApplyMaterial(const System::UnicodeString AName, Glrendercontextinfo::TRenderContextInfo &ARci);
	virtual bool __fastcall UnApplyMaterial(Glrendercontextinfo::TRenderContextInfo &ARci);
public:
	/* TComponent.Create */ inline __fastcall virtual TGLAbstractMaterialLibrary(System::Classes::TComponent* AOwner) : Baseclasses::TGLCadenceAbleComponent(AOwner) { }
	/* TComponent.Destroy */ inline __fastcall virtual ~TGLAbstractMaterialLibrary(void) { }
	
};

#pragma pack(pop)

class PASCALIMPLEMENTATION TGLMaterialLibrary : public TGLAbstractMaterialLibrary
{
	typedef TGLAbstractMaterialLibrary inherited;
	
private:
	bool FDoNotClearMaterialsOnLoad;
	Gltexture::TTextureNeededEvent FOnTextureNeeded;
	
protected:
	TGLLibMaterials* __fastcall GetMaterials(void);
	void __fastcall SetMaterials(TGLLibMaterials* const val);
	bool __fastcall StoreMaterials(void);
	
public:
	__fastcall virtual TGLMaterialLibrary(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TGLMaterialLibrary(void);
	void __fastcall DestroyHandles(void);
	void __fastcall WriteToFiler(Persistentclasses::TVirtualWriter* writer);
	void __fastcall ReadFromFiler(Persistentclasses::TVirtualReader* reader);
	DYNAMIC void __fastcall SaveToStream(System::Classes::TStream* aStream);
	DYNAMIC void __fastcall LoadFromStream(System::Classes::TStream* aStream);
	void __fastcall AddMaterialsFromStream(System::Classes::TStream* aStream);
	void __fastcall SaveToFile(const System::UnicodeString fileName);
	void __fastcall LoadFromFile(const System::UnicodeString fileName);
	void __fastcall AddMaterialsFromFile(const System::UnicodeString fileName);
	TGLLibMaterial* __fastcall AddTextureMaterial(const System::UnicodeString materialName, const System::UnicodeString fileName, bool persistent = true)/* overload */;
	TGLLibMaterial* __fastcall AddTextureMaterial(const System::UnicodeString materialName, Vcl::Graphics::TGraphic* graphic)/* overload */;
	TGLLibMaterial* __fastcall LibMaterialByName(const System::UnicodeString AName);
	Gltexture::TGLTexture* __fastcall TextureByName(const System::UnicodeString LibMatName);
	System::UnicodeString __fastcall GetNameOfTexture(Gltexture::TGLTexture* const Texture);
	System::UnicodeString __fastcall GetNameOfLibMaterial(TGLLibMaterial* const LibMat);
	
__published:
	__property TGLLibMaterials* Materials = {read=GetMaterials, write=SetMaterials, stored=StoreMaterials};
	__property Gltexture::TTextureNeededEvent OnTextureNeeded = {read=FOnTextureNeeded, write=FOnTextureNeeded};
	__property TexturePaths = {default=0};
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Glmaterial */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLMATERIAL)
using namespace Glmaterial;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// GlmaterialHPP
