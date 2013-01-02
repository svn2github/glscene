// CodeGear C++Builder
// Copyright (c) 1995, 2012 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLMaterialEx.pas' rev: 24.00 (Win32)

#ifndef GlmaterialexHPP
#define GlmaterialexHPP

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
#include <GLContext.hpp>	// Pascal unit
#include <VectorTypes.hpp>	// Pascal unit
#include <GLMaterial.hpp>	// Pascal unit
#include <GLTexture.hpp>	// Pascal unit
#include <GLColor.hpp>	// Pascal unit
#include <GLCoordinates.hpp>	// Pascal unit
#include <VectorGeometry.hpp>	// Pascal unit
#include <GLGraphics.hpp>	// Pascal unit
#include <PersistentClasses.hpp>	// Pascal unit
#include <GLCrossPlatform.hpp>	// Pascal unit
#include <GLState.hpp>	// Pascal unit
#include <GLTextureFormat.hpp>	// Pascal unit
#include <XCollection.hpp>	// Pascal unit
#include <GLTextureCombiners.hpp>	// Pascal unit
#include <OpenGLTokens.hpp>	// Pascal unit
#include <GLSLParameter.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Glmaterialex
{
//-- type declarations -------------------------------------------------------
typedef System::UnicodeString TGLMaterialComponentName;

class DELPHICLASS TGLASMVertexProgram;
typedef void __fastcall (__closure *TOnAsmProgSetting)(TGLASMVertexProgram* Sender, Glrendercontextinfo::TRenderContextInfo &ARci);

class DELPHICLASS TGLBaseShaderModel;
typedef void __fastcall (__closure *TOnUniformInitialize)(TGLBaseShaderModel* Sender);

typedef void __fastcall (__closure *TOnUniformSetting)(TGLBaseShaderModel* Sender, Glrendercontextinfo::TRenderContextInfo &ARci);

class DELPHICLASS TGLBaseMaterialCollectionItem;
class DELPHICLASS TGLMaterialLibraryEx;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLBaseMaterialCollectionItem : public Xcollection::TXCollectionItem
{
	typedef Xcollection::TXCollectionItem inherited;
	
private:
	int FNameHashKey;
	Persistentclasses::TPersistentObjectList* FUserList;
	bool FDefferedInit;
	bool FNotifying;
	bool FIsValid;
	Persistentclasses::TPersistentObjectList* __fastcall GetUserList(void);
	TGLMaterialLibraryEx* __fastcall GetMaterialLibraryEx(void);
	
protected:
	virtual void __fastcall SetName(const System::UnicodeString AValue);
	virtual void __fastcall NotifyChange(System::TObject* Sender);
	__property Persistentclasses::TPersistentObjectList* UserList = {read=GetUserList};
	virtual void __fastcall DoOnPrepare(Glcontext::TGLContext* Sender) = 0 ;
	
public:
	__fastcall virtual ~TGLBaseMaterialCollectionItem(void);
	void __fastcall RegisterUser(Baseclasses::TGLUpdateAbleObject* AUser);
	void __fastcall UnregisterUser(Baseclasses::TGLUpdateAbleObject* AUser);
	int __fastcall GetUserCount(void);
	Glmaterial::TGLAbstractMaterialLibrary* __fastcall GetMaterialLibrary(void);
	__property TGLMaterialLibraryEx* MaterialLibrary = {read=GetMaterialLibraryEx};
	__property bool IsValid = {read=FIsValid, nodefault};
	
__published:
	__property System::UnicodeString Name = {read=GetName, write=SetName};
	__property bool DefferedInit = {read=FDefferedInit, write=FDefferedInit, default=0};
public:
	/* TXCollectionItem.Create */ inline __fastcall virtual TGLBaseMaterialCollectionItem(Xcollection::TXCollection* aOwner) : Xcollection::TXCollectionItem(aOwner) { }
	
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

#pragma pack(pop)

typedef System::TMetaClass* CGLBaseMaterialCollectionItem;

class DELPHICLASS TGLLibMaterialProperty;
class DELPHICLASS TGLLibMaterialEx;
class PASCALIMPLEMENTATION TGLLibMaterialProperty : public Baseclasses::TGLUpdateAbleObject
{
	typedef Baseclasses::TGLUpdateAbleObject inherited;
	
protected:
	bool FEnabled;
	System::UnicodeString FNextPassName;
	TGLLibMaterialEx* __fastcall GetMaterial(void);
	TGLMaterialLibraryEx* __fastcall GetMaterialLibraryEx(void);
	virtual void __fastcall SetEnabled(bool AValue);
	void __fastcall SetNextPass(const System::UnicodeString AValue);
	virtual void __fastcall Loaded(void);
	__property System::UnicodeString NextPass = {read=FNextPassName, write=SetNextPass};
	
public:
	virtual void __fastcall NotifyChange(System::TObject* Sender);
	Glmaterial::TGLAbstractMaterialLibrary* __fastcall GetMaterialLibrary(void);
	__property TGLMaterialLibraryEx* MaterialLibrary = {read=GetMaterialLibraryEx};
	
__published:
	__property bool Enabled = {read=FEnabled, write=SetEnabled, nodefault};
public:
	/* TGLUpdateAbleObject.Create */ inline __fastcall virtual TGLLibMaterialProperty(System::Classes::TPersistent* AOwner) : Baseclasses::TGLUpdateAbleObject(AOwner) { }
	
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TGLLibMaterialProperty(void) { }
	
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


class DELPHICLASS TGLTextureSampler;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLTextureSampler : public TGLBaseMaterialCollectionItem
{
	typedef TGLBaseMaterialCollectionItem inherited;
	
protected:
	virtual void __fastcall WriteToFiler(System::Classes::TWriter* AWriter);
	virtual void __fastcall ReadFromFiler(System::Classes::TReader* AReader);
	
private:
	Glcontext::TGLSamplerHandle* FHandle;
	Gltexture::TGLMinFilter FMinFilter;
	Gltexture::TGLMagFilter FMagFilter;
	Gltextureformat::TGLTextureFilteringQuality FFilteringQuality;
	int FLODBias;
	float FLODBiasFract;
	System::StaticArray<Gltextureformat::TGLSeparateTextureWrap, 3> FWrap;
	Glcolor::TGLColor* FBorderColor;
	Gltextureformat::TGLTextureCompareMode FCompareMode;
	Glstate::TComparisonFunction FCompareFunc;
	bool FDecodeSRGB;
	void __fastcall SetMagFilter(Gltexture::TGLMagFilter AValue);
	void __fastcall SetMinFilter(Gltexture::TGLMinFilter AValue);
	void __fastcall SetLODBias(int AValue);
	void __fastcall SetFilteringQuality(Gltextureformat::TGLTextureFilteringQuality AValue);
	Gltextureformat::TGLSeparateTextureWrap __fastcall GetWrap(int Index);
	void __fastcall SetWrap(int Index, Gltextureformat::TGLSeparateTextureWrap AValue);
	void __fastcall SetBorderColor(Glcolor::TGLColor* const AValue);
	void __fastcall SetCompareMode(Gltextureformat::TGLTextureCompareMode AValue);
	void __fastcall SetCompareFunc(Glstate::TComparisonFunction AValue);
	void __fastcall SetDecodeSRGB(bool AValue);
	
public:
	__fastcall virtual TGLTextureSampler(Xcollection::TXCollection* AOwner);
	__fastcall virtual ~TGLTextureSampler(void);
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	virtual void __fastcall NotifyChange(System::TObject* Sender);
	virtual void __fastcall DoOnPrepare(Glcontext::TGLContext* Sender);
	void __fastcall Apply(Glrendercontextinfo::TRenderContextInfo &ARci);
	void __fastcall UnApply(Glrendercontextinfo::TRenderContextInfo &ARci);
	__classmethod virtual System::UnicodeString __fastcall FriendlyName();
	__property Glcontext::TGLSamplerHandle* Handle = {read=FHandle};
	
__published:
	__property Gltexture::TGLMagFilter MagFilter = {read=FMagFilter, write=SetMagFilter, default=1};
	__property Gltexture::TGLMinFilter MinFilter = {read=FMinFilter, write=SetMinFilter, default=5};
	__property Gltextureformat::TGLTextureFilteringQuality FilteringQuality = {read=FFilteringQuality, write=SetFilteringQuality, default=1};
	__property int LodBias = {read=FLODBias, write=SetLODBias, default=0};
	__property Gltextureformat::TGLSeparateTextureWrap WrapX = {read=GetWrap, write=SetWrap, index=0, default=0};
	__property Gltextureformat::TGLSeparateTextureWrap WrapY = {read=GetWrap, write=SetWrap, index=1, default=0};
	__property Gltextureformat::TGLSeparateTextureWrap WrapZ = {read=GetWrap, write=SetWrap, index=2, default=0};
	__property Glcolor::TGLColor* BorderColor = {read=FBorderColor, write=SetBorderColor};
	__property Gltextureformat::TGLTextureCompareMode CompareMode = {read=FCompareMode, write=SetCompareMode, default=0};
	__property Glstate::TComparisonFunction CompareFunc = {read=FCompareFunc, write=SetCompareFunc, default=3};
	__property bool sRGB_Encode = {read=FDecodeSRGB, write=SetDecodeSRGB, default=1};
};

#pragma pack(pop)

class DELPHICLASS TGLAbstractTexture;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLAbstractTexture : public TGLBaseMaterialCollectionItem
{
	typedef TGLBaseMaterialCollectionItem inherited;
	
protected:
	Glcontext::TGLTextureHandle* FHandle;
	Gltextureformat::TGLInternalFormat FInternalFormat;
	int FWidth;
	int FHeight;
	int FDepth;
	Gltextureformat::TSwizzleVector FSwizzles;
	TGLTextureSampler* FApplicableSampler;
	TGLTextureSampler* FLastSampler;
	Gltextureformat::TGLTextureTarget __fastcall GetTextureTarget(void);
	virtual void __fastcall Apply(Glrendercontextinfo::TRenderContextInfo &ARci) = 0 ;
	virtual void __fastcall UnApply(Glrendercontextinfo::TRenderContextInfo &ARci) = 0 ;
	
public:
	__property Glcontext::TGLTextureHandle* Handle = {read=FHandle};
	
__published:
	__property Gltextureformat::TGLTextureTarget Shape = {read=GetTextureTarget, nodefault};
public:
	/* TGLBaseMaterialCollectionItem.Destroy */ inline __fastcall virtual ~TGLAbstractTexture(void) { }
	
public:
	/* TXCollectionItem.Create */ inline __fastcall virtual TGLAbstractTexture(Xcollection::TXCollection* aOwner) : TGLBaseMaterialCollectionItem(aOwner) { }
	
};

#pragma pack(pop)

enum TMipmapGenerationMode : unsigned char { mgmNoMip, mgmLeaveExisting, mgmOnFly, mgmBoxFilter, mgmTriangleFilter, mgmHermiteFilter, mgmBellFilter, mgmSplineFilter, mgmLanczos3Filter, mgmMitchellFilter };

class DELPHICLASS TGLTextureImageEx;
class PASCALIMPLEMENTATION TGLTextureImageEx : public TGLAbstractTexture
{
	typedef TGLAbstractTexture inherited;
	
protected:
	virtual void __fastcall WriteToFiler(System::Classes::TWriter* AWriter);
	virtual void __fastcall ReadFromFiler(System::Classes::TReader* AReader);
	
private:
	Gltextureformat::TGLInternalCompression FCompression;
	Glgraphics::TGLBaseImage* FImage;
	Gltexture::TGLTextureImageAlpha FImageAlpha;
	float FImageBrightness;
	float FImageGamma;
	float FHeightToNormalScale;
	System::UnicodeString FSourceFile;
	int FApplyCounter;
	bool FInternallyStored;
	TMipmapGenerationMode FMipGenMode;
	bool FUseStreaming;
	int FBaseLevel;
	int FMaxLevel;
	double FLastTime;
	void __fastcall SetInternalFormat(const Gltextureformat::TGLInternalFormat AValue);
	void __fastcall SetImageAlpha(const Gltexture::TGLTextureImageAlpha AValue);
	void __fastcall SetImageBrightness(const float AValue);
	bool __fastcall StoreBrightness(void);
	void __fastcall SetImageGamma(const float AValue);
	bool __fastcall StoreGamma(void);
	void __fastcall SetNormalMapScale(const float AValue);
	bool __fastcall StoreNormalMapScale(void);
	void __fastcall SetCompression(const Gltextureformat::TGLInternalCompression AValue);
	void __fastcall SetSourceFile(System::UnicodeString AValue);
	void __fastcall SetInternallyStored(const bool AValue);
	void __fastcall SetMipGenMode(const TMipmapGenerationMode AValue);
	void __fastcall SetUseStreaming(const bool AValue);
	void __fastcall PrepareImage(void);
	void __fastcall FullTransfer(void);
	void __fastcall StreamTransfer(void);
	void __fastcall CalcLODRange(/* out */ int &AFirstLOD, /* out */ int &ALastLOD);
	
public:
	__fastcall virtual TGLTextureImageEx(Xcollection::TXCollection* AOwner);
	__fastcall virtual ~TGLTextureImageEx(void);
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	virtual void __fastcall NotifyChange(System::TObject* Sender);
	virtual void __fastcall DoOnPrepare(Glcontext::TGLContext* Sender);
	virtual void __fastcall Apply(Glrendercontextinfo::TRenderContextInfo &ARci);
	virtual void __fastcall UnApply(Glrendercontextinfo::TRenderContextInfo &ARci);
	__classmethod virtual System::UnicodeString __fastcall FriendlyName();
	
__published:
	__property int InternalWidth = {read=FWidth, nodefault};
	__property int InternalHeight = {read=FHeight, nodefault};
	__property int InternalDepth = {read=FDepth, nodefault};
	__property Gltextureformat::TGLInternalFormat InternalFormat = {read=FInternalFormat, write=SetInternalFormat, default=31};
	__property Gltexture::TGLTextureImageAlpha ImageAlpha = {read=FImageAlpha, write=SetImageAlpha, default=0};
	__property float ImageBrightness = {read=FImageBrightness, write=SetImageBrightness, stored=StoreBrightness};
	__property float ImageGamma = {read=FImageGamma, write=SetImageGamma, stored=StoreGamma};
	__property Gltextureformat::TGLInternalCompression Compression = {read=FCompression, write=SetCompression, default=0};
	__property float HeightToNormalScale = {read=FHeightToNormalScale, write=SetNormalMapScale, stored=StoreNormalMapScale};
	__property System::UnicodeString SourceFile = {read=FSourceFile, write=SetSourceFile};
	__property bool InternallyStored = {read=FInternallyStored, write=SetInternallyStored, default=0};
	__property TMipmapGenerationMode MipGenMode = {read=FMipGenMode, write=SetMipGenMode, default=2};
	__property bool UseStreaming = {read=FUseStreaming, write=SetUseStreaming, default=0};
};


class DELPHICLASS TGLFrameBufferAttachment;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLFrameBufferAttachment : public TGLAbstractTexture
{
	typedef TGLAbstractTexture inherited;
	
protected:
	virtual void __fastcall WriteToFiler(System::Classes::TWriter* AWriter);
	virtual void __fastcall ReadFromFiler(System::Classes::TReader* AReader);
	
private:
	Glcontext::TGLRenderbufferHandle* FRenderBufferHandle;
	bool FLayered;
	bool FCubeMap;
	int FSamples;
	bool FOnlyWrite;
	bool FFixedSamplesLocation;
	void __fastcall SetWidth(int AValue);
	void __fastcall SetHeight(int AValue);
	void __fastcall SetDepth(int AValue);
	void __fastcall SetInternalFormat(const Gltextureformat::TGLInternalFormat AValue);
	void __fastcall SetOnlyWrite(bool AValue);
	void __fastcall SetLayered(bool AValue);
	void __fastcall SetCubeMap(bool AValue);
	void __fastcall SetSamples(int AValue);
	void __fastcall SetFixedSamplesLocation(bool AValue);
	
public:
	__fastcall virtual TGLFrameBufferAttachment(Xcollection::TXCollection* AOwner);
	__fastcall virtual ~TGLFrameBufferAttachment(void);
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	virtual void __fastcall NotifyChange(System::TObject* Sender);
	virtual void __fastcall DoOnPrepare(Glcontext::TGLContext* Sender);
	virtual void __fastcall Apply(Glrendercontextinfo::TRenderContextInfo &ARci);
	virtual void __fastcall UnApply(Glrendercontextinfo::TRenderContextInfo &ARci);
	__classmethod virtual System::UnicodeString __fastcall FriendlyName();
	
__published:
	__property int InternalWidth = {read=FWidth, write=SetWidth, default=256};
	__property int InternalHeight = {read=FHeight, write=SetHeight, default=256};
	__property int InternalDepth = {read=FDepth, write=SetDepth, default=0};
	__property Gltextureformat::TGLInternalFormat InternalFormat = {read=FInternalFormat, write=SetInternalFormat, default=31};
	__property bool OnlyWrite = {read=FOnlyWrite, write=SetOnlyWrite, default=0};
	__property bool Layered = {read=FLayered, write=SetLayered, default=0};
	__property bool CubeMap = {read=FCubeMap, write=SetCubeMap, default=0};
	__property int Samples = {read=FSamples, write=SetSamples, default=-1};
	__property bool FixedSamplesLocation = {read=FFixedSamplesLocation, write=SetFixedSamplesLocation, default=0};
};

#pragma pack(pop)

class DELPHICLASS TGLTextureSwizzling;
class PASCALIMPLEMENTATION TGLTextureSwizzling : public Baseclasses::TGLUpdateAbleObject
{
	typedef Baseclasses::TGLUpdateAbleObject inherited;
	
private:
	Gltextureformat::TSwizzleVector FSwizzles;
	Gltextureformat::TGLTextureSwizzle __fastcall GetSwizzle(int AIndex);
	void __fastcall SetSwizzle(int AIndex, Gltextureformat::TGLTextureSwizzle AValue);
	bool __fastcall StoreSwizzle(int AIndex);
	
public:
	__fastcall virtual TGLTextureSwizzling(System::Classes::TPersistent* AOwner);
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	void __fastcall WriteToFiler(System::Classes::TWriter* AWriter);
	void __fastcall ReadFromFiler(System::Classes::TReader* AReader);
	
__published:
	__property Gltextureformat::TGLTextureSwizzle RedFrom = {read=GetSwizzle, write=SetSwizzle, stored=StoreSwizzle, index=0, nodefault};
	__property Gltextureformat::TGLTextureSwizzle GreenFrom = {read=GetSwizzle, write=SetSwizzle, stored=StoreSwizzle, index=1, nodefault};
	__property Gltextureformat::TGLTextureSwizzle BlueFrom = {read=GetSwizzle, write=SetSwizzle, stored=StoreSwizzle, index=2, nodefault};
	__property Gltextureformat::TGLTextureSwizzle AlphaFrom = {read=GetSwizzle, write=SetSwizzle, stored=StoreSwizzle, index=3, nodefault};
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TGLTextureSwizzling(void) { }
	
};


class DELPHICLASS TGLTextureProperties;
class PASCALIMPLEMENTATION TGLTextureProperties : public TGLLibMaterialProperty
{
	typedef TGLLibMaterialProperty inherited;
	
private:
	System::UnicodeString FLibTextureName;
	System::UnicodeString FLibSamplerName;
	TGLAbstractTexture* FLibTexture;
	TGLTextureSampler* FLibSampler;
	Glcoordinates::TGLCoordinates3* FTextureOffset;
	Glcoordinates::TGLCoordinates3* FTextureScale;
	float FTextureRotate;
	bool FTextureMatrixIsIdentity;
	bool FTextureOverride;
	Vectortypes::TMatrix4f FTextureMatrix;
	Gltexture::TGLTextureMappingMode FMappingMode;
	Glcolor::TGLColor* FEnvColor;
	Glcoordinates::TGLCoordinates4* FMapSCoordinates;
	Glcoordinates::TGLCoordinates4* FMapTCoordinates;
	Glcoordinates::TGLCoordinates4* FMapRCoordinates;
	Glcoordinates::TGLCoordinates4* FMapQCoordinates;
	TGLTextureSwizzling* FSwizzling;
	System::UnicodeString __fastcall GetLibTextureName(void);
	System::UnicodeString __fastcall GetLibSamplerName(void);
	void __fastcall SetLibTextureName(const System::UnicodeString AValue);
	void __fastcall SetLibSamplerName(const System::UnicodeString AValue);
	Glcoordinates::TGLCoordinates3* __fastcall GetTextureOffset(void);
	void __fastcall SetTextureOffset(Glcoordinates::TGLCoordinates3* const AValue);
	bool __fastcall StoreTextureOffset(void);
	Glcoordinates::TGLCoordinates3* __fastcall GetTextureScale(void);
	void __fastcall SetTextureScale(Glcoordinates::TGLCoordinates3* const AValue);
	bool __fastcall StoreTextureScale(void);
	void __fastcall SetTextureMatrix(const Vectortypes::TMatrix4f &AValue);
	void __fastcall SetTextureRotate(float AValue);
	bool __fastcall StoreTextureRotate(void);
	void __fastcall SetMappingMode(const Gltexture::TGLTextureMappingMode AValue);
	Glcoordinates::TGLCoordinates4* __fastcall GetMappingSCoordinates(void);
	void __fastcall SetMappingSCoordinates(Glcoordinates::TGLCoordinates4* const AValue);
	bool __fastcall StoreMappingSCoordinates(void);
	Glcoordinates::TGLCoordinates4* __fastcall GetMappingTCoordinates(void);
	void __fastcall SetMappingTCoordinates(Glcoordinates::TGLCoordinates4* const AValue);
	bool __fastcall StoreMappingTCoordinates(void);
	Glcoordinates::TGLCoordinates4* __fastcall GetMappingRCoordinates(void);
	void __fastcall SetMappingRCoordinates(Glcoordinates::TGLCoordinates4* const AValue);
	bool __fastcall StoreMappingRCoordinates(void);
	Glcoordinates::TGLCoordinates4* __fastcall GetMappingQCoordinates(void);
	void __fastcall SetMappingQCoordinates(Glcoordinates::TGLCoordinates4* const AValue);
	bool __fastcall StoreMappingQCoordinates(void);
	void __fastcall SetSwizzling(TGLTextureSwizzling* const AValue);
	bool __fastcall StoreSwizzling(void);
	void __fastcall SetEnvColor(Glcolor::TGLColor* const AValue);
	void __fastcall CalculateTextureMatrix(void);
	void __fastcall ApplyMappingMode(void);
	void __fastcall UnApplyMappingMode(void);
	
protected:
	virtual void __fastcall Loaded(void);
	
public:
	__fastcall virtual TGLTextureProperties(System::Classes::TPersistent* AOwner);
	__fastcall virtual ~TGLTextureProperties(void);
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	virtual void __fastcall NotifyChange(System::TObject* Sender);
	virtual void __fastcall Notification(System::TObject* Sender, System::Classes::TOperation Operation);
	bool __fastcall IsValid(void);
	void __fastcall Apply(Glrendercontextinfo::TRenderContextInfo &ARci);
	void __fastcall UnApply(Glrendercontextinfo::TRenderContextInfo &ARci);
	__property Vectortypes::TMatrix4f TextureMatrix = {read=FTextureMatrix, write=SetTextureMatrix};
	
__published:
	__property System::UnicodeString LibTextureName = {read=GetLibTextureName, write=SetLibTextureName};
	__property System::UnicodeString LibSamplerName = {read=GetLibSamplerName, write=SetLibSamplerName};
	__property Glcoordinates::TGLCoordinates3* TextureOffset = {read=GetTextureOffset, write=SetTextureOffset, stored=StoreTextureOffset};
	__property Glcoordinates::TGLCoordinates3* TextureScale = {read=GetTextureScale, write=SetTextureScale, stored=StoreTextureScale};
	__property float TextureRotate = {read=FTextureRotate, write=SetTextureRotate, stored=StoreTextureRotate};
	__property Glcolor::TGLColor* EnvColor = {read=FEnvColor, write=SetEnvColor};
	__property Gltexture::TGLTextureMappingMode MappingMode = {read=FMappingMode, write=SetMappingMode, default=0};
	__property Glcoordinates::TGLCoordinates4* MappingSCoordinates = {read=GetMappingSCoordinates, write=SetMappingSCoordinates, stored=StoreMappingSCoordinates};
	__property Glcoordinates::TGLCoordinates4* MappingTCoordinates = {read=GetMappingTCoordinates, write=SetMappingTCoordinates, stored=StoreMappingTCoordinates};
	__property Glcoordinates::TGLCoordinates4* MappingRCoordinates = {read=GetMappingRCoordinates, write=SetMappingRCoordinates, stored=StoreMappingRCoordinates};
	__property Glcoordinates::TGLCoordinates4* MappingQCoordinates = {read=GetMappingQCoordinates, write=SetMappingQCoordinates, stored=StoreMappingQCoordinates};
	__property TGLTextureSwizzling* Swizzling = {read=FSwizzling, write=SetSwizzling, stored=StoreSwizzling};
};


class DELPHICLASS TGLFixedFunctionProperties;
class PASCALIMPLEMENTATION TGLFixedFunctionProperties : public TGLLibMaterialProperty
{
	typedef TGLLibMaterialProperty inherited;
	
private:
	Glmaterial::TGLFaceProperties* FFrontProperties;
	Glmaterial::TGLFaceProperties* FBackProperties;
	Glmaterial::TGLDepthProperties* FDepthProperties;
	Glmaterial::TBlendingMode FBlendingMode;
	Glmaterial::TGLBlendingParameters* FBlendingParams;
	TGLTextureProperties* FTexProp;
	Glmaterial::TMaterialOptions FMaterialOptions;
	Glmaterial::TFaceCulling FFaceCulling;
	Glstate::TPolygonMode FPolygonMode;
	Gltexture::TGLTextureMode FTextureMode;
	Glmaterial::TGLFaceProperties* __fastcall GetBackProperties(void);
	void __fastcall SetBackProperties(Glmaterial::TGLFaceProperties* AValues);
	void __fastcall SetFrontProperties(Glmaterial::TGLFaceProperties* AValues);
	void __fastcall SetDepthProperties(Glmaterial::TGLDepthProperties* AValues);
	void __fastcall SetBlendingMode(const Glmaterial::TBlendingMode AValue);
	void __fastcall SetMaterialOptions(const Glmaterial::TMaterialOptions AValue);
	void __fastcall SetFaceCulling(const Glmaterial::TFaceCulling AValue);
	void __fastcall SetPolygonMode(Glstate::TPolygonMode AValue);
	void __fastcall SetBlendingParams(Glmaterial::TGLBlendingParameters* const AValue);
	void __fastcall SetTexProp(TGLTextureProperties* AValue);
	void __fastcall SetTextureMode(Gltexture::TGLTextureMode AValue);
	
public:
	__fastcall virtual TGLFixedFunctionProperties(System::Classes::TPersistent* AOwner);
	__fastcall virtual ~TGLFixedFunctionProperties(void);
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	void __fastcall Apply(Glrendercontextinfo::TRenderContextInfo &ARci);
	void __fastcall UnApply(Glrendercontextinfo::TRenderContextInfo &ARci);
	bool __fastcall Blended(void);
	
__published:
	__property Glmaterial::TMaterialOptions MaterialOptions = {read=FMaterialOptions, write=SetMaterialOptions, default=0};
	__property Glmaterial::TGLFaceProperties* BackProperties = {read=GetBackProperties, write=SetBackProperties};
	__property Glmaterial::TGLFaceProperties* FrontProperties = {read=FFrontProperties, write=SetFrontProperties};
	__property Glmaterial::TGLDepthProperties* DepthProperties = {read=FDepthProperties, write=SetDepthProperties};
	__property Glmaterial::TBlendingMode BlendingMode = {read=FBlendingMode, write=SetBlendingMode, default=0};
	__property Glmaterial::TGLBlendingParameters* BlendingParams = {read=FBlendingParams, write=SetBlendingParams};
	__property Glmaterial::TFaceCulling FaceCulling = {read=FFaceCulling, write=SetFaceCulling, default=0};
	__property Glstate::TPolygonMode PolygonMode = {read=FPolygonMode, write=SetPolygonMode, default=0};
	__property TGLTextureProperties* Texture = {read=FTexProp, write=SetTexProp};
	__property Gltexture::TGLTextureMode TextureMode = {read=FTextureMode, write=SetTextureMode, default=0};
	__property NextPass = {default=0};
};


class DELPHICLASS TGLTextureCombiner;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLTextureCombiner : public TGLBaseMaterialCollectionItem
{
	typedef TGLBaseMaterialCollectionItem inherited;
	
protected:
	virtual void __fastcall WriteToFiler(System::Classes::TWriter* AWriter);
	virtual void __fastcall ReadFromFiler(System::Classes::TReader* AReader);
	
private:
	Glcontext::TGLVirtualHandle* FHandle;
	System::Classes::TStringList* FScript;
	Gltexturecombiners::TCombinerCache FCommandCache;
	void __fastcall SetScript(System::Classes::TStringList* AValue);
	void __fastcall DoAllocate(Glcontext::TGLVirtualHandle* Sender, unsigned &handle);
	void __fastcall DoDeallocate(Glcontext::TGLVirtualHandle* Sender, unsigned &handle);
	
public:
	__fastcall virtual TGLTextureCombiner(Xcollection::TXCollection* AOwner);
	__fastcall virtual ~TGLTextureCombiner(void);
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	virtual void __fastcall NotifyChange(System::TObject* Sender);
	virtual void __fastcall DoOnPrepare(Glcontext::TGLContext* Sender);
	__classmethod virtual System::UnicodeString __fastcall FriendlyName();
	
__published:
	__property System::Classes::TStringList* Script = {read=FScript, write=SetScript};
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLASMVertexProgram : public TGLBaseMaterialCollectionItem
{
	typedef TGLBaseMaterialCollectionItem inherited;
	
protected:
	virtual void __fastcall WriteToFiler(System::Classes::TWriter* AWriter);
	virtual void __fastcall ReadFromFiler(System::Classes::TReader* AReader);
	
private:
	Glcontext::TGLARBVertexProgramHandle* FHandle;
	System::Classes::TStringList* FSource;
	System::UnicodeString FSourceFile;
	System::UnicodeString FInfoLog;
	void __fastcall SetSource(System::Classes::TStringList* AValue);
	void __fastcall SetSourceFile(System::UnicodeString AValue);
	Glcontext::TGLARBVertexProgramHandle* __fastcall GetHandle(void);
	
public:
	__fastcall virtual TGLASMVertexProgram(Xcollection::TXCollection* AOwner);
	__fastcall virtual ~TGLASMVertexProgram(void);
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	virtual void __fastcall DoOnPrepare(Glcontext::TGLContext* Sender);
	__classmethod virtual System::UnicodeString __fastcall FriendlyName();
	virtual void __fastcall NotifyChange(System::TObject* Sender);
	__property Glcontext::TGLARBVertexProgramHandle* Handle = {read=GetHandle};
	
__published:
	__property System::Classes::TStringList* Source = {read=FSource, write=SetSource};
	__property System::UnicodeString SourceFile = {read=FSourceFile, write=SetSourceFile};
	__property System::UnicodeString InfoLog = {read=FInfoLog};
};

#pragma pack(pop)

enum TLightDir2TexEnvColor : unsigned char { l2eNone, l2eEnvColor0, l2eEnvColor1, l2eEnvColor2, l2eEnvColor3 };

class DELPHICLASS TGLMultitexturingProperties;
class PASCALIMPLEMENTATION TGLMultitexturingProperties : public TGLLibMaterialProperty
{
	typedef TGLLibMaterialProperty inherited;
	
private:
	TGLTextureCombiner* FLibCombiner;
	TGLASMVertexProgram* FLibAsmProg;
	System::UnicodeString FLibCombinerName;
	System::UnicodeString FLibAsmProgName;
	System::StaticArray<TGLTextureProperties*, 4> FTexProps;
	Gltexture::TGLTextureMode FTextureMode;
	TLightDir2TexEnvColor FLightDir;
	int FLightSourceIndex;
	System::UnicodeString __fastcall GetLibCombinerName(void);
	System::UnicodeString __fastcall GetLibAsmProgName(void);
	void __fastcall SetLibCombinerName(const System::UnicodeString AValue);
	void __fastcall SetLibAsmProgName(const System::UnicodeString AValue);
	TGLTextureProperties* __fastcall GetTexProps(int AIndex);
	void __fastcall SetTexProps(int AIndex, TGLTextureProperties* AValue);
	void __fastcall SetTextureMode(Gltexture::TGLTextureMode AValue);
	void __fastcall SetLightSourceIndex(int AValue);
	
protected:
	virtual void __fastcall Loaded(void);
	
public:
	__fastcall virtual TGLMultitexturingProperties(System::Classes::TPersistent* AOwner);
	__fastcall virtual ~TGLMultitexturingProperties(void);
	virtual void __fastcall Notification(System::TObject* Sender, System::Classes::TOperation Operation);
	bool __fastcall IsValid(void);
	void __fastcall Apply(Glrendercontextinfo::TRenderContextInfo &ARci);
	void __fastcall UnApply(Glrendercontextinfo::TRenderContextInfo &ARci);
	
__published:
	__property System::UnicodeString LibCombinerName = {read=GetLibCombinerName, write=SetLibCombinerName};
	__property System::UnicodeString LibAsmProgName = {read=GetLibAsmProgName, write=SetLibAsmProgName};
	__property TGLTextureProperties* Texture0 = {read=GetTexProps, write=SetTexProps, index=0};
	__property TGLTextureProperties* Texture1 = {read=GetTexProps, write=SetTexProps, index=1};
	__property TGLTextureProperties* Texture2 = {read=GetTexProps, write=SetTexProps, index=2};
	__property TGLTextureProperties* Texture3 = {read=GetTexProps, write=SetTexProps, index=3};
	__property Gltexture::TGLTextureMode TextureMode = {read=FTextureMode, write=SetTextureMode, default=0};
	__property TLightDir2TexEnvColor LightDirTo = {read=FLightDir, write=FLightDir, default=0};
	__property int LightSourceIndex = {read=FLightSourceIndex, write=SetLightSourceIndex, default=0};
	__property NextPass = {default=0};
};


enum TGLShaderType : unsigned char { shtVertex, shtControl, shtEvaluation, shtGeometry, shtFragment };

class DELPHICLASS TGLShaderEx;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLShaderEx : public TGLBaseMaterialCollectionItem
{
	typedef TGLBaseMaterialCollectionItem inherited;
	
protected:
	virtual void __fastcall WriteToFiler(System::Classes::TWriter* AWriter);
	virtual void __fastcall ReadFromFiler(System::Classes::TReader* AReader);
	
private:
	System::StaticArray<Glcontext::TGLShaderHandle*, 5> FHandle;
	System::Classes::TStringList* FSource;
	System::UnicodeString FSourceFile;
	TGLShaderType FShaderType;
	System::UnicodeString FInfoLog;
	Glslparameter::TGLgsInTypes FGeometryInput;
	Glslparameter::TGLgsOutTypes FGeometryOutput;
	int FGeometryVerticesOut;
	void __fastcall SetSource(System::Classes::TStringList* AValue);
	void __fastcall SetSourceFile(System::UnicodeString AValue);
	void __fastcall SetShaderType(TGLShaderType AValue);
	void __fastcall SetGeometryInput(Glslparameter::TGLgsInTypes AValue);
	void __fastcall SetGeometryOutput(Glslparameter::TGLgsOutTypes AValue);
	void __fastcall SetGeometryVerticesOut(int AValue);
	Glcontext::TGLShaderHandle* __fastcall GetHandle(void);
	
public:
	__fastcall virtual TGLShaderEx(Xcollection::TXCollection* AOwner);
	__fastcall virtual ~TGLShaderEx(void);
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	virtual void __fastcall DoOnPrepare(Glcontext::TGLContext* Sender);
	__classmethod virtual System::UnicodeString __fastcall FriendlyName();
	virtual void __fastcall NotifyChange(System::TObject* Sender);
	__property Glcontext::TGLShaderHandle* Handle = {read=GetHandle};
	
__published:
	__property System::Classes::TStringList* Source = {read=FSource, write=SetSource};
	__property System::UnicodeString SourceFile = {read=FSourceFile, write=SetSourceFile};
	__property TGLShaderType ShaderType = {read=FShaderType, write=SetShaderType, default=0};
	__property System::UnicodeString InfoLog = {read=FInfoLog};
	__property Glslparameter::TGLgsInTypes GeometryInput = {read=FGeometryInput, write=SetGeometryInput, default=0};
	__property Glslparameter::TGLgsOutTypes GeometryOutput = {read=FGeometryOutput, write=SetGeometryOutput, default=0};
	__property int GeometryVerticesOut = {read=FGeometryVerticesOut, write=SetGeometryVerticesOut, default=1};
};

#pragma pack(pop)

class DELPHICLASS TGLAbstractShaderUniform;
class PASCALIMPLEMENTATION TGLAbstractShaderUniform : public Baseclasses::TGLUpdateAbleObject
{
	typedef Baseclasses::TGLUpdateAbleObject inherited;
	
protected:
	System::UnicodeString FName;
	int FNameHashCode;
	Glslparameter::TGLSLDataType FType;
	Glslparameter::TGLSLSamplerType FSamplerType;
	System::UnicodeString __fastcall GetName(void);
	Glslparameter::TGLSLDataType __fastcall GetGLSLType(void);
	Glslparameter::TGLSLSamplerType __fastcall GetGLSLSamplerType(void);
	virtual System::UnicodeString __fastcall GetAutoSetMethod(void);
	virtual System::UnicodeString __fastcall GetTextureName(void);
	virtual System::UnicodeString __fastcall GetSamplerName(void);
	virtual Gltextureformat::TSwizzleVector __fastcall GetTextureSwizzle(void);
	virtual void __fastcall SetTextureName(const System::UnicodeString AValue);
	virtual void __fastcall SetSamplerName(const System::UnicodeString AValue);
	virtual void __fastcall SetAutoSetMethod(const System::UnicodeString AValue);
	virtual void __fastcall SetTextureSwizzle(const Gltextureformat::TSwizzleVector AValue);
	virtual float __fastcall GetFloat(void);
	virtual Vectortypes::TVector2f __fastcall GetVec2(void);
	virtual Vectortypes::TVector3f __fastcall GetVec3(void);
	virtual Vectortypes::TVector4f __fastcall GetVec4(void);
	virtual int __fastcall GetInt(void);
	virtual Vectortypes::TVector2i __fastcall GetIVec2(void);
	virtual Vectortypes::TVector3i __fastcall GetIVec3(void);
	virtual Vectortypes::TVector4i __fastcall GetIVec4(void);
	virtual unsigned __fastcall GetUInt(void);
	virtual Vectortypes::TVector2ui __fastcall GetUVec2(void);
	virtual Vectortypes::TVector3ui __fastcall GetUVec3(void);
	virtual Vectortypes::TVector4ui __fastcall GetUVec4(void);
	virtual void __fastcall SetFloat(const float Value);
	virtual void __fastcall SetVec2(const Vectortypes::TVector2f &Value);
	virtual void __fastcall SetVec3(const Vectortypes::TVector3f &Value);
	virtual void __fastcall SetVec4(const Vectortypes::TVector4f &Value);
	virtual void __fastcall SetInt(const int Value);
	virtual void __fastcall SetIVec2(const Vectortypes::TVector2i &Value);
	virtual void __fastcall SetIVec3(const Vectortypes::TVector3i &Value);
	virtual void __fastcall SetIVec4(const Vectortypes::TVector4i &Value);
	virtual void __fastcall SetUInt(const unsigned Value);
	virtual void __fastcall SetUVec2(const Vectortypes::TVector2ui &Value);
	virtual void __fastcall SetUVec3(const Vectortypes::TVector3ui &Value);
	virtual void __fastcall SetUVec4(const Vectortypes::TVector4ui &Value);
	virtual Vectortypes::TMatrix2f __fastcall GetMat2(void);
	virtual Vectortypes::TMatrix3f __fastcall GetMat3(void);
	virtual Vectortypes::TMatrix4f __fastcall GetMat4(void);
	virtual void __fastcall SetMat2(const Vectortypes::TMatrix2f &Value);
	virtual void __fastcall SetMat3(const Vectortypes::TMatrix3f &Value);
	virtual void __fastcall SetMat4(const Vectortypes::TMatrix4f &Value);
	virtual void __fastcall SetFloatArray(const System::PSingle Values, int Count);
	virtual void __fastcall SetIntArray(const Opengltokens::PGLint Values, int Count);
	virtual void __fastcall SetUIntArray(const Opengltokens::PGLuint Values, int Count);
	virtual void __fastcall WriteToFiler(System::Classes::TWriter* AWriter);
	virtual void __fastcall ReadFromFiler(System::Classes::TReader* AReader);
	virtual void __fastcall Apply(Glrendercontextinfo::TRenderContextInfo &ARci);
public:
	/* TGLUpdateAbleObject.Create */ inline __fastcall virtual TGLAbstractShaderUniform(System::Classes::TPersistent* AOwner) : Baseclasses::TGLUpdateAbleObject(AOwner) { }
	
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TGLAbstractShaderUniform(void) { }
	
private:
	void *__IShaderParameter;	/* Glslparameter::IShaderParameter */
	
public:
	operator Glslparameter::IShaderParameter*(void) { return (Glslparameter::IShaderParameter*)&__IShaderParameter; }
	
};


typedef System::TMetaClass* CGLAbstractShaderUniform;

class DELPHICLASS TGLShaderUniform;
class PASCALIMPLEMENTATION TGLShaderUniform : public TGLAbstractShaderUniform
{
	typedef TGLAbstractShaderUniform inherited;
	
protected:
	int FLocation;
	unsigned FStoreProgram;
	Glslparameter::TUniformAutoSetMethod FAutoSet;
	unsigned __fastcall GetProgram(void);
	void __fastcall PushProgram(void);
	void __fastcall PopProgram(void);
	virtual float __fastcall GetFloat(void);
	virtual Vectortypes::TVector2f __fastcall GetVec2(void);
	virtual Vectortypes::TVector3f __fastcall GetVec3(void);
	virtual Vectortypes::TVector4f __fastcall GetVec4(void);
	virtual int __fastcall GetInt(void);
	virtual Vectortypes::TVector2i __fastcall GetIVec2(void);
	virtual Vectortypes::TVector3i __fastcall GetIVec3(void);
	virtual Vectortypes::TVector4i __fastcall GetIVec4(void);
	virtual unsigned __fastcall GetUInt(void);
	virtual Vectortypes::TVector2ui __fastcall GetUVec2(void);
	virtual Vectortypes::TVector3ui __fastcall GetUVec3(void);
	virtual Vectortypes::TVector4ui __fastcall GetUVec4(void);
	virtual void __fastcall SetFloat(const float Value);
	virtual void __fastcall SetVec2(const Vectortypes::TVector2f &Value);
	virtual void __fastcall SetVec3(const Vectortypes::TVector3f &Value);
	virtual void __fastcall SetVec4(const Vectortypes::TVector4f &Value);
	virtual void __fastcall SetInt(const int Value);
	virtual void __fastcall SetIVec2(const Vectortypes::TVector2i &Value);
	virtual void __fastcall SetIVec3(const Vectortypes::TVector3i &Value);
	virtual void __fastcall SetIVec4(const Vectortypes::TVector4i &Value);
	virtual void __fastcall SetUInt(const unsigned Value);
	virtual void __fastcall SetUVec2(const Vectortypes::TVector2ui &Value);
	virtual void __fastcall SetUVec3(const Vectortypes::TVector3ui &Value);
	virtual void __fastcall SetUVec4(const Vectortypes::TVector4ui &Value);
	virtual Vectortypes::TMatrix2f __fastcall GetMat2(void);
	virtual Vectortypes::TMatrix3f __fastcall GetMat3(void);
	virtual Vectortypes::TMatrix4f __fastcall GetMat4(void);
	virtual void __fastcall SetMat2(const Vectortypes::TMatrix2f &Value);
	virtual void __fastcall SetMat3(const Vectortypes::TMatrix3f &Value);
	virtual void __fastcall SetMat4(const Vectortypes::TMatrix4f &Value);
	virtual System::UnicodeString __fastcall GetAutoSetMethod(void);
	virtual void __fastcall SetAutoSetMethod(const System::UnicodeString AValue);
	virtual void __fastcall WriteToFiler(System::Classes::TWriter* AWriter);
	virtual void __fastcall ReadFromFiler(System::Classes::TReader* AReader);
	
public:
	virtual void __fastcall SetFloatArray(const System::PSingle Values, int Count);
	virtual void __fastcall SetIntArray(const Opengltokens::PGLint Values, int Count);
	virtual void __fastcall SetUIntArray(const Opengltokens::PGLuint Values, int Count);
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	virtual void __fastcall Apply(Glrendercontextinfo::TRenderContextInfo &ARci);
	__property System::UnicodeString Name = {read=GetName};
	__property int Location = {read=FLocation, nodefault};
	__property Glslparameter::TGLSLDataType GLSLType = {read=GetGLSLType, nodefault};
public:
	/* TGLUpdateAbleObject.Create */ inline __fastcall virtual TGLShaderUniform(System::Classes::TPersistent* AOwner) : TGLAbstractShaderUniform(AOwner) { }
	
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TGLShaderUniform(void) { }
	
private:
	void *__IShaderParameter;	/* Glslparameter::IShaderParameter */
	
public:
	operator Glslparameter::IShaderParameter*(void) { return (Glslparameter::IShaderParameter*)&__IShaderParameter; }
	
};


class DELPHICLASS TGLShaderUniformDSA;
class PASCALIMPLEMENTATION TGLShaderUniformDSA : public TGLShaderUniform
{
	typedef TGLShaderUniform inherited;
	
protected:
	virtual void __fastcall SetFloat(const float Value);
	virtual void __fastcall SetVec2(const Vectortypes::TVector2f &Value);
	virtual void __fastcall SetVec3(const Vectortypes::TVector3f &Value);
	virtual void __fastcall SetVec4(const Vectortypes::TVector4f &Value);
	virtual void __fastcall SetInt(const int Value);
	virtual void __fastcall SetIVec2(const Vectortypes::TVector2i &Value);
	virtual void __fastcall SetIVec3(const Vectortypes::TVector3i &Value);
	virtual void __fastcall SetIVec4(const Vectortypes::TVector4i &Value);
	virtual void __fastcall SetUInt(const unsigned Value);
	virtual void __fastcall SetUVec2(const Vectortypes::TVector2ui &Value);
	virtual void __fastcall SetUVec3(const Vectortypes::TVector3ui &Value);
	virtual void __fastcall SetUVec4(const Vectortypes::TVector4ui &Value);
	virtual void __fastcall SetMat2(const Vectortypes::TMatrix2f &Value);
	virtual void __fastcall SetMat3(const Vectortypes::TMatrix3f &Value);
	virtual void __fastcall SetMat4(const Vectortypes::TMatrix4f &Value);
	
public:
	virtual void __fastcall SetFloatArray(const System::PSingle Values, int Count);
	virtual void __fastcall SetIntArray(const Opengltokens::PGLint Values, int Count);
	virtual void __fastcall SetUIntArray(const Opengltokens::PGLuint Values, int Count);
public:
	/* TGLUpdateAbleObject.Create */ inline __fastcall virtual TGLShaderUniformDSA(System::Classes::TPersistent* AOwner) : TGLShaderUniform(AOwner) { }
	
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TGLShaderUniformDSA(void) { }
	
};


class DELPHICLASS TGLShaderUniformTexture;
class PASCALIMPLEMENTATION TGLShaderUniformTexture : public TGLShaderUniform
{
	typedef TGLShaderUniform inherited;
	
private:
	TGLAbstractTexture* FLibTexture;
	TGLTextureSampler* FLibSampler;
	Gltextureformat::TGLTextureTarget FTarget;
	Gltextureformat::TSwizzleVector FSwizzling;
	
protected:
	System::UnicodeString FLibTexureName;
	System::UnicodeString FLibSamplerName;
	virtual System::UnicodeString __fastcall GetTextureName(void);
	virtual System::UnicodeString __fastcall GetSamplerName(void);
	virtual Gltextureformat::TSwizzleVector __fastcall GetTextureSwizzle(void);
	virtual void __fastcall SetTextureName(const System::UnicodeString AValue);
	virtual void __fastcall SetSamplerName(const System::UnicodeString AValue);
	virtual void __fastcall SetTextureSwizzle(const Gltextureformat::TSwizzleVector AValue);
	virtual void __fastcall WriteToFiler(System::Classes::TWriter* AWriter);
	virtual void __fastcall ReadFromFiler(System::Classes::TReader* AReader);
	void __fastcall Loaded(void);
	
public:
	__fastcall virtual TGLShaderUniformTexture(System::Classes::TPersistent* AOwner);
	__fastcall virtual ~TGLShaderUniformTexture(void);
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	virtual void __fastcall Notification(System::TObject* Sender, System::Classes::TOperation Operation);
	virtual void __fastcall Apply(Glrendercontextinfo::TRenderContextInfo &ARci);
	__property System::UnicodeString LibTextureName = {read=GetTextureName, write=SetTextureName};
	__property System::UnicodeString LibSamplerName = {read=GetSamplerName, write=SetSamplerName};
	__property Glslparameter::TGLSLSamplerType GLSLSampler = {read=GetGLSLSamplerType, nodefault};
	__property Gltextureformat::TSwizzleVector Swizzling = {read=GetTextureSwizzle, write=SetTextureSwizzle};
};


class PASCALIMPLEMENTATION TGLBaseShaderModel : public TGLLibMaterialProperty
{
	typedef TGLLibMaterialProperty inherited;
	
private:
	typedef System::StaticArray<System::UnicodeString, 5> _TGLBaseShaderModel__1;
	
	
protected:
	Glcontext::TGLProgramHandle* FHandle;
	_TGLBaseShaderModel__1 FLibShaderName;
	System::StaticArray<TGLShaderEx*, 5> FShaders;
	bool FIsValid;
	System::UnicodeString FInfoLog;
	Persistentclasses::TPersistentObjectList* FUniforms;
	bool FAutoFill;
	System::UnicodeString __fastcall GetLibShaderName(TGLShaderType AType);
	void __fastcall SetLibShaderName(TGLShaderType AType, const System::UnicodeString AValue);
	Glslparameter::_di_IShaderParameter __fastcall GetUniform(const System::UnicodeString AName);
	__classmethod void __fastcall ReleaseUniforms(Persistentclasses::TPersistentObjectList* AList);
	__property System::UnicodeString LibVertexShaderName = {read=GetLibShaderName, write=SetLibShaderName, index=0};
	__property System::UnicodeString LibFragmentShaderName = {read=GetLibShaderName, write=SetLibShaderName, index=4};
	__property System::UnicodeString LibGeometryShaderName = {read=GetLibShaderName, write=SetLibShaderName, index=3};
	__property System::UnicodeString LibTessEvalShaderName = {read=GetLibShaderName, write=SetLibShaderName, index=2};
	__property System::UnicodeString LibTessControlShaderName = {read=GetLibShaderName, write=SetLibShaderName, index=1};
	virtual void __fastcall DefineProperties(System::Classes::TFiler* Filer);
	void __fastcall ReadUniforms(System::Classes::TStream* AStream);
	void __fastcall WriteUniforms(System::Classes::TStream* AStream);
	virtual void __fastcall Loaded(void);
	virtual __classmethod bool __fastcall IsSupported() = 0 ;
	
public:
	__fastcall virtual TGLBaseShaderModel(System::Classes::TPersistent* AOwner);
	__fastcall virtual ~TGLBaseShaderModel(void);
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	virtual void __fastcall NotifyChange(System::TObject* Sender);
	virtual void __fastcall Notification(System::TObject* Sender, System::Classes::TOperation Operation);
	void __fastcall DoOnPrepare(Glcontext::TGLContext* Sender);
	virtual void __fastcall Apply(Glrendercontextinfo::TRenderContextInfo &ARci);
	virtual void __fastcall UnApply(Glrendercontextinfo::TRenderContextInfo &ARci);
	void __fastcall GetUniformNames(System::Classes::TGetStrProc Proc);
	__property Glcontext::TGLProgramHandle* Handle = {read=FHandle};
	__property bool IsValid = {read=FIsValid, nodefault};
	__property Glslparameter::_di_IShaderParameter Uniforms[const System::UnicodeString AName] = {read=GetUniform};
	
__published:
	__property System::UnicodeString InfoLog = {read=FInfoLog};
	__property bool AutoFillOfUniforms = {read=FAutoFill, write=FAutoFill, stored=false, nodefault};
	__property NextPass = {default=0};
};


class DELPHICLASS TGLShaderModel3;
class PASCALIMPLEMENTATION TGLShaderModel3 : public TGLBaseShaderModel
{
	typedef TGLBaseShaderModel inherited;
	
public:
	__classmethod virtual bool __fastcall IsSupported();
	
__published:
	__property LibVertexShaderName = {index=0, default=0};
	__property LibFragmentShaderName = {index=4, default=0};
public:
	/* TGLBaseShaderModel.Create */ inline __fastcall virtual TGLShaderModel3(System::Classes::TPersistent* AOwner) : TGLBaseShaderModel(AOwner) { }
	/* TGLBaseShaderModel.Destroy */ inline __fastcall virtual ~TGLShaderModel3(void) { }
	
};


class DELPHICLASS TGLShaderModel4;
class PASCALIMPLEMENTATION TGLShaderModel4 : public TGLBaseShaderModel
{
	typedef TGLBaseShaderModel inherited;
	
public:
	__classmethod virtual bool __fastcall IsSupported();
	
__published:
	__property LibVertexShaderName = {index=0, default=0};
	__property LibGeometryShaderName = {index=3, default=0};
	__property LibFragmentShaderName = {index=4, default=0};
public:
	/* TGLBaseShaderModel.Create */ inline __fastcall virtual TGLShaderModel4(System::Classes::TPersistent* AOwner) : TGLBaseShaderModel(AOwner) { }
	/* TGLBaseShaderModel.Destroy */ inline __fastcall virtual ~TGLShaderModel4(void) { }
	
};


class DELPHICLASS TGLShaderModel5;
class PASCALIMPLEMENTATION TGLShaderModel5 : public TGLBaseShaderModel
{
	typedef TGLBaseShaderModel inherited;
	
public:
	virtual void __fastcall Apply(Glrendercontextinfo::TRenderContextInfo &ARci);
	virtual void __fastcall UnApply(Glrendercontextinfo::TRenderContextInfo &ARci);
	__classmethod virtual bool __fastcall IsSupported();
	
__published:
	__property LibTessControlShaderName = {index=1, default=0};
	__property LibTessEvalShaderName = {index=2, default=0};
	__property LibVertexShaderName = {index=0, default=0};
	__property LibGeometryShaderName = {index=3, default=0};
	__property LibFragmentShaderName = {index=4, default=0};
public:
	/* TGLBaseShaderModel.Create */ inline __fastcall virtual TGLShaderModel5(System::Classes::TPersistent* AOwner) : TGLBaseShaderModel(AOwner) { }
	/* TGLBaseShaderModel.Destroy */ inline __fastcall virtual ~TGLShaderModel5(void) { }
	
};


class PASCALIMPLEMENTATION TGLLibMaterialEx : public Glmaterial::TGLAbstractLibMaterial
{
	typedef Glmaterial::TGLAbstractLibMaterial inherited;
	
private:
	Glcontext::TGLVirtualHandle* FHandle;
	Glstate::TGLMaterialLevel FApplicableLevel;
	Glstate::TGLMaterialLevel FSelectedLevel;
	TGLFixedFunctionProperties* FFixedFunc;
	TGLMultitexturingProperties* FMultitexturing;
	TGLShaderModel3* FSM3;
	TGLShaderModel4* FSM4;
	TGLShaderModel5* FSM5;
	TOnAsmProgSetting FOnAsmProgSetting;
	TOnUniformInitialize FOnSM3UniformInit;
	TOnUniformSetting FOnSM3UniformSetting;
	TOnUniformInitialize FOnSM4UniformInit;
	TOnUniformSetting FOnSM4UniformSetting;
	TOnUniformInitialize FOnSM5UniformInit;
	TOnUniformSetting FOnSM5UniformSetting;
	TGLLibMaterialEx* FNextPass;
	bool FStoreAmalgamating;
	void __fastcall SetLevel(Glstate::TGLMaterialLevel AValue);
	void __fastcall SetFixedFunc(TGLFixedFunctionProperties* AValue);
	void __fastcall SetMultitexturing(TGLMultitexturingProperties* AValue);
	void __fastcall SetSM3(TGLShaderModel3* AValue);
	void __fastcall SetSM4(TGLShaderModel4* AValue);
	void __fastcall SetSM5(TGLShaderModel5* AValue);
	void __fastcall DoAllocate(Glcontext::TGLVirtualHandle* Sender, unsigned &handle);
	void __fastcall DoDeallocate(Glcontext::TGLVirtualHandle* Sender, unsigned &handle);
	
protected:
	virtual void __fastcall Loaded(void);
	void __fastcall RemoveDefferedInit(void);
	void __fastcall DoOnPrepare(Glcontext::TGLContext* Sender);
	
public:
	__fastcall virtual TGLLibMaterialEx(System::Classes::TCollection* ACollection);
	__fastcall virtual ~TGLLibMaterialEx(void);
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	virtual void __fastcall NotifyChange(System::TObject* Sender);
	virtual void __fastcall Apply(Glrendercontextinfo::TRenderContextInfo &ARci);
	virtual bool __fastcall UnApply(Glrendercontextinfo::TRenderContextInfo &ARci);
	virtual bool __fastcall Blended(void);
	
__published:
	__property Glstate::TGLMaterialLevel ApplicableLevel = {read=FApplicableLevel, write=SetLevel, default=0};
	__property Glstate::TGLMaterialLevel SelectedLevel = {read=FSelectedLevel, nodefault};
	__property TGLFixedFunctionProperties* FixedFunction = {read=FFixedFunc, write=SetFixedFunc};
	__property TGLMultitexturingProperties* Multitexturing = {read=FMultitexturing, write=SetMultitexturing};
	__property TGLShaderModel3* ShaderModel3 = {read=FSM3, write=SetSM3};
	__property TGLShaderModel4* ShaderModel4 = {read=FSM4, write=SetSM4};
	__property TGLShaderModel5* ShaderModel5 = {read=FSM5, write=SetSM5};
	__property TOnAsmProgSetting OnAsmProgSetting = {read=FOnAsmProgSetting, write=FOnAsmProgSetting};
	__property TOnUniformInitialize OnSM3UniformInitialize = {read=FOnSM3UniformInit, write=FOnSM3UniformInit};
	__property TOnUniformSetting OnSM3UniformSetting = {read=FOnSM3UniformSetting, write=FOnSM3UniformSetting};
	__property TOnUniformInitialize OnSM4UniformInitialize = {read=FOnSM4UniformInit, write=FOnSM4UniformInit};
	__property TOnUniformSetting OnSM4UniformSetting = {read=FOnSM4UniformSetting, write=FOnSM4UniformSetting};
	__property TOnUniformInitialize OnSM5UniformInitialize = {read=FOnSM5UniformInit, write=FOnSM5UniformInit};
	__property TOnUniformSetting OnSM5UniformSetting = {read=FOnSM5UniformSetting, write=FOnSM5UniformSetting};
};


class DELPHICLASS TGLLibMaterialsEx;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLLibMaterialsEx : public Glmaterial::TGLAbstractLibMaterials
{
	typedef Glmaterial::TGLAbstractLibMaterials inherited;
	
public:
	TGLLibMaterialEx* operator[](int index) { return Items[index]; }
	
protected:
	void __fastcall SetItems(int AIndex, TGLLibMaterialEx* const AValue);
	TGLLibMaterialEx* __fastcall GetItems(int AIndex);
	
public:
	__fastcall TGLLibMaterialsEx(System::Classes::TComponent* AOwner);
	TGLMaterialLibraryEx* __fastcall MaterialLibrary(void);
	int __fastcall IndexOf(TGLLibMaterialEx* const Item);
	HIDESBASE TGLLibMaterialEx* __fastcall Add(void);
	HIDESBASE TGLLibMaterialEx* __fastcall FindItemID(int ID);
	__property TGLLibMaterialEx* Items[int index] = {read=GetItems, write=SetItems/*, default*/};
	TGLLibMaterialEx* __fastcall GetLibMaterialByName(const System::UnicodeString AName);
public:
	/* TCollection.Destroy */ inline __fastcall virtual ~TGLLibMaterialsEx(void) { }
	
};

#pragma pack(pop)

class DELPHICLASS TGLMatLibComponents;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLMatLibComponents : public Xcollection::TXCollection
{
	typedef Xcollection::TXCollection inherited;
	
public:
	TGLBaseMaterialCollectionItem* operator[](int index) { return Items[index]; }
	
protected:
	HIDESBASE TGLBaseMaterialCollectionItem* __fastcall GetItems(int index);
	
public:
	DYNAMIC System::UnicodeString __fastcall GetNamePath(void);
	__classmethod virtual Xcollection::TXCollectionItemClass __fastcall ItemsClass();
	__property TGLBaseMaterialCollectionItem* Items[int index] = {read=GetItems/*, default*/};
	TGLBaseMaterialCollectionItem* __fastcall GetItemByName(const System::UnicodeString AName);
	TGLAbstractTexture* __fastcall GetTextureByName(const System::UnicodeString AName);
	TGLFrameBufferAttachment* __fastcall GetAttachmentByName(const System::UnicodeString AName);
	TGLTextureSampler* __fastcall GetSamplerByName(const System::UnicodeString AName);
	TGLTextureCombiner* __fastcall GetCombinerByName(const System::UnicodeString AName);
	TGLShaderEx* __fastcall GetShaderByName(const System::UnicodeString AName);
	TGLASMVertexProgram* __fastcall GetAsmProgByName(const System::UnicodeString AName);
	System::UnicodeString __fastcall MakeUniqueName(const System::UnicodeString AName);
public:
	/* TXCollection.Create */ inline __fastcall virtual TGLMatLibComponents(System::Classes::TPersistent* aOwner) : Xcollection::TXCollection(aOwner) { }
	/* TXCollection.Destroy */ inline __fastcall virtual ~TGLMatLibComponents(void) { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLMaterialLibraryEx : public Glmaterial::TGLAbstractMaterialLibrary
{
	typedef Glmaterial::TGLAbstractMaterialLibrary inherited;
	
private:
	TGLMatLibComponents* FComponents;
	
protected:
	virtual void __fastcall Loaded(void);
	TGLLibMaterialsEx* __fastcall GetMaterials(void);
	void __fastcall SetMaterials(TGLLibMaterialsEx* AValue);
	bool __fastcall StoreMaterials(void);
	void __fastcall SetComponents(TGLMatLibComponents* AValue);
	virtual void __fastcall DefineProperties(System::Classes::TFiler* Filer);
	void __fastcall WriteComponents(System::Classes::TStream* AStream);
	void __fastcall ReadComponents(System::Classes::TStream* AStream);
	
public:
	__fastcall virtual TGLMaterialLibraryEx(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TGLMaterialLibraryEx(void);
	void __fastcall GetNames(System::Classes::TGetStrProc Proc, CGLBaseMaterialCollectionItem AClass)/* overload */;
	TGLTextureImageEx* __fastcall AddTexture(const System::UnicodeString AName);
	TGLFrameBufferAttachment* __fastcall AddAttachment(const System::UnicodeString AName);
	TGLTextureSampler* __fastcall AddSampler(const System::UnicodeString AName);
	TGLTextureCombiner* __fastcall AddCombiner(const System::UnicodeString AName);
	TGLShaderEx* __fastcall AddShader(const System::UnicodeString AName);
	TGLASMVertexProgram* __fastcall AddAsmProg(const System::UnicodeString AName);
	void __fastcall SetLevelForAll(const Glstate::TGLMaterialLevel ALevel);
	
__published:
	__property TGLLibMaterialsEx* Materials = {read=GetMaterials, write=SetMaterials, stored=StoreMaterials};
	__property TGLMatLibComponents* Components = {read=FComponents, write=SetComponents};
	__property TexturePaths = {default=0};
};

#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------
extern PACKAGE void __fastcall RegisterGLMaterialExNameChangeEvent(System::Classes::TNotifyEvent AEvent);
extern PACKAGE void __fastcall DeRegisterGLMaterialExNameChangeEvent(System::Classes::TNotifyEvent AEvent);
}	/* namespace Glmaterialex */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLMATERIALEX)
using namespace Glmaterialex;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// GlmaterialexHPP
