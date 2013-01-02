// CodeGear C++Builder
// Copyright (c) 1995, 2012 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLTexture.pas' rev: 24.00 (Win32)

#ifndef GltextureHPP
#define GltextureHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <System.Classes.hpp>	// Pascal unit
#include <System.SysUtils.hpp>	// Pascal unit
#include <GLCrossPlatform.hpp>	// Pascal unit
#include <BaseClasses.hpp>	// Pascal unit
#include <OpenGLTokens.hpp>	// Pascal unit
#include <VectorGeometry.hpp>	// Pascal unit
#include <GLGraphics.hpp>	// Pascal unit
#include <GLContext.hpp>	// Pascal unit
#include <GLState.hpp>	// Pascal unit
#include <GLColor.hpp>	// Pascal unit
#include <GLCoordinates.hpp>	// Pascal unit
#include <GLRenderContextInfo.hpp>	// Pascal unit
#include <GLTextureFormat.hpp>	// Pascal unit
#include <Vcl.Graphics.hpp>	// Pascal unit
#include <VectorTypes.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Gltexture
{
//-- type declarations -------------------------------------------------------
enum TGLMinFilter : unsigned char { miNearest, miLinear, miNearestMipmapNearest, miLinearMipmapNearest, miNearestMipmapLinear, miLinearMipmapLinear };

enum TGLMagFilter : unsigned char { maNearest, maLinear };

enum TGLTextureMode : unsigned char { tmDecal, tmModulate, tmBlend, tmReplace, tmAdd };

enum TGLTextureWrap : unsigned char { twBoth, twNone, twVertical, twHorizontal, twSeparate };

enum TGLDepthTextureMode : unsigned char { dtmLuminance, dtmIntensity, dtmAlpha };

typedef Glstate::TComparisonFunction TGLDepthCompareFunc;

enum TGLTextureFormat : unsigned char { tfDefault, tfRGB, tfRGBA, tfRGB16, tfRGBA16, tfAlpha, tfLuminance, tfLuminanceAlpha, tfIntensity, tfNormalMap, tfRGBAFloat16, tfRGBAFloat32, tfExtended };

typedef Gltextureformat::TGLInternalCompression TGLTextureCompression;

__interface IGLTextureNotifyAble;
typedef System::DelphiInterface<IGLTextureNotifyAble> _di_IGLTextureNotifyAble;
__interface  INTERFACE_UUID("{0D9DC0B0-ECE4-4513-A8A1-5AE7022C9426}") IGLTextureNotifyAble  : public Baseclasses::IGLNotifyAble 
{
	
public:
	virtual void __fastcall NotifyTexMapChange(System::TObject* Sender) = 0 ;
};

typedef void __fastcall (__closure *TTextureNeededEvent)(System::TObject* Sender, System::UnicodeString &textureFileName);

enum TGLTextureChange : unsigned char { tcImage, tcParams };

typedef System::Set<TGLTextureChange, TGLTextureChange::tcImage, TGLTextureChange::tcParams>  TGLTextureChanges;

enum TGLTextureImageAlpha : unsigned char { tiaDefault, tiaAlphaFromIntensity, tiaSuperBlackTransparent, tiaLuminance, tiaLuminanceSqrt, tiaOpaque, tiaTopLeftPointColorTransparent, tiaInverseLuminance, tiaInverseLuminanceSqrt, tiaBottomRightPointColorTransparent };

class DELPHICLASS TGLTextureImage;
class DELPHICLASS TGLTexture;
class PASCALIMPLEMENTATION TGLTextureImage : public Baseclasses::TGLUpdateAbleObject
{
	typedef Baseclasses::TGLUpdateAbleObject inherited;
	
private:
	System::UnicodeString __fastcall GetResourceName(void);
	
protected:
	TGLTexture* FOwnerTexture;
	TTextureNeededEvent FOnTextureNeeded;
	System::UnicodeString FResourceFile;
	__classmethod virtual bool __fastcall IsSelfLoading();
	virtual void __fastcall LoadTexture(Gltextureformat::TGLInternalFormat AInternalFormat);
	virtual Gltextureformat::TGLTextureTarget __fastcall GetTextureTarget(void) = 0 ;
	virtual int __fastcall GetHeight(void);
	virtual int __fastcall GetWidth(void);
	virtual int __fastcall GetDepth(void);
	__property TTextureNeededEvent OnTextureNeeded = {read=FOnTextureNeeded, write=FOnTextureNeeded};
	
public:
	__fastcall virtual TGLTextureImage(System::Classes::TPersistent* AOwner);
	__fastcall virtual ~TGLTextureImage(void);
	__property TGLTexture* OwnerTexture = {read=FOwnerTexture, write=FOwnerTexture};
	virtual void __fastcall NotifyChange(System::TObject* Sender);
	DYNAMIC void __fastcall SaveToFile(const System::UnicodeString fileName);
	DYNAMIC void __fastcall LoadFromFile(const System::UnicodeString fileName);
	__classmethod virtual System::UnicodeString __fastcall FriendlyName();
	__classmethod virtual System::UnicodeString __fastcall FriendlyDescription();
	DYNAMIC void __fastcall Invalidate(void);
	virtual Glgraphics::TGLImage* __fastcall GetBitmap32(void);
	virtual void __fastcall ReleaseBitmap32(void);
	Vcl::Graphics::TBitmap* __fastcall AsBitmap(void);
	void __fastcall AssignToBitmap(Vcl::Graphics::TBitmap* aBitmap);
	__property int Width = {read=GetWidth, nodefault};
	__property int Height = {read=GetHeight, nodefault};
	__property int Depth = {read=GetDepth, nodefault};
	__property Gltextureformat::TGLTextureTarget NativeTextureTarget = {read=GetTextureTarget, nodefault};
	__property System::UnicodeString ResourceName = {read=GetResourceName};
};


typedef System::TMetaClass* TGLTextureImageClass;

class DELPHICLASS TGLBlankImage;
class PASCALIMPLEMENTATION TGLBlankImage : public TGLTextureImage
{
	typedef TGLTextureImage inherited;
	
private:
	void __fastcall SetWidth(int val);
	void __fastcall SetHeight(int val);
	void __fastcall SetDepth(int val);
	void __fastcall SetCubeMap(const bool val);
	void __fastcall SetArray(const bool val);
	
protected:
	Glgraphics::TGLImage* fBitmap;
	int fWidth;
	int fHeight;
	int fDepth;
	unsigned fColorFormat;
	bool fCubeMap;
	bool fArray;
	virtual int __fastcall GetWidth(void);
	virtual int __fastcall GetHeight(void);
	virtual int __fastcall GetDepth(void);
	virtual Gltextureformat::TGLTextureTarget __fastcall GetTextureTarget(void);
	
public:
	__fastcall virtual TGLBlankImage(System::Classes::TPersistent* AOwner);
	__fastcall virtual ~TGLBlankImage(void);
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	virtual Glgraphics::TGLImage* __fastcall GetBitmap32(void);
	virtual void __fastcall ReleaseBitmap32(void);
	DYNAMIC void __fastcall SaveToFile(const System::UnicodeString fileName);
	DYNAMIC void __fastcall LoadFromFile(const System::UnicodeString fileName);
	__classmethod virtual System::UnicodeString __fastcall FriendlyName();
	__classmethod virtual System::UnicodeString __fastcall FriendlyDescription();
	
__published:
	__property int Width = {read=GetWidth, write=SetWidth, default=256};
	__property int Height = {read=GetHeight, write=SetHeight, default=256};
	__property int Depth = {read=GetDepth, write=SetDepth, default=0};
	__property bool CubeMap = {read=fCubeMap, write=SetCubeMap, default=0};
	__property bool TextureArray = {read=fArray, write=SetArray, default=0};
	__property unsigned ColorFormat = {read=fColorFormat, write=fColorFormat, nodefault};
};


class DELPHICLASS TGLPictureImage;
class PASCALIMPLEMENTATION TGLPictureImage : public TGLTextureImage
{
	typedef TGLTextureImage inherited;
	
private:
	Glgraphics::TGLImage* FBitmap;
	Vcl::Graphics::TPicture* FGLPicture;
	int FUpdateCounter;
	
protected:
	virtual int __fastcall GetHeight(void);
	virtual int __fastcall GetWidth(void);
	virtual int __fastcall GetDepth(void);
	virtual Gltextureformat::TGLTextureTarget __fastcall GetTextureTarget(void);
	Vcl::Graphics::TPicture* __fastcall GetPicture(void);
	void __fastcall SetPicture(Vcl::Graphics::TPicture* const aPicture);
	void __fastcall PictureChanged(System::TObject* Sender);
	
public:
	__fastcall virtual TGLPictureImage(System::Classes::TPersistent* AOwner);
	__fastcall virtual ~TGLPictureImage(void);
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	HIDESBASE void __fastcall BeginUpdate(void);
	HIDESBASE void __fastcall EndUpdate(void);
	virtual Glgraphics::TGLImage* __fastcall GetBitmap32(void);
	virtual void __fastcall ReleaseBitmap32(void);
	__property Vcl::Graphics::TPicture* Picture = {read=GetPicture, write=SetPicture};
};


class DELPHICLASS TGLPersistentImage;
class PASCALIMPLEMENTATION TGLPersistentImage : public TGLPictureImage
{
	typedef TGLPictureImage inherited;
	
public:
	__fastcall virtual TGLPersistentImage(System::Classes::TPersistent* AOwner);
	__fastcall virtual ~TGLPersistentImage(void);
	DYNAMIC void __fastcall SaveToFile(const System::UnicodeString fileName);
	DYNAMIC void __fastcall LoadFromFile(const System::UnicodeString fileName);
	__classmethod virtual System::UnicodeString __fastcall FriendlyName();
	__classmethod virtual System::UnicodeString __fastcall FriendlyDescription();
	__property NativeTextureTarget;
	
__published:
	__property Picture;
};


class DELPHICLASS TGLPicFileImage;
class PASCALIMPLEMENTATION TGLPicFileImage : public TGLPictureImage
{
	typedef TGLPictureImage inherited;
	
private:
	System::UnicodeString FPictureFileName;
	bool FAlreadyWarnedAboutMissingFile;
	int FWidth;
	int FHeight;
	
protected:
	void __fastcall SetPictureFileName(const System::UnicodeString val);
	virtual int __fastcall GetWidth(void);
	virtual int __fastcall GetHeight(void);
	virtual int __fastcall GetDepth(void);
	
public:
	__fastcall virtual TGLPicFileImage(System::Classes::TPersistent* AOwner);
	__fastcall virtual ~TGLPicFileImage(void);
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	DYNAMIC void __fastcall SaveToFile(const System::UnicodeString fileName);
	DYNAMIC void __fastcall LoadFromFile(const System::UnicodeString fileName);
	__classmethod virtual System::UnicodeString __fastcall FriendlyName();
	__classmethod virtual System::UnicodeString __fastcall FriendlyDescription();
	__property NativeTextureTarget;
	virtual Glgraphics::TGLImage* __fastcall GetBitmap32(void);
	DYNAMIC void __fastcall Invalidate(void);
	
__published:
	__property System::UnicodeString PictureFileName = {read=FPictureFileName, write=SetPictureFileName};
};


typedef int TGLCubeMapTarget;

class DELPHICLASS TGLCubeMapImage;
class PASCALIMPLEMENTATION TGLCubeMapImage : public TGLTextureImage
{
	typedef TGLTextureImage inherited;
	
private:
	Glgraphics::TGLImage* FImage;
	int FUpdateCounter;
	System::StaticArray<Vcl::Graphics::TPicture*, 6> FPicture;
	
protected:
	virtual int __fastcall GetWidth(void);
	virtual int __fastcall GetHeight(void);
	virtual int __fastcall GetDepth(void);
	void __fastcall SetPicture(int index, Vcl::Graphics::TPicture* const val);
	Vcl::Graphics::TPicture* __fastcall GetPicture(int index);
	virtual Gltextureformat::TGLTextureTarget __fastcall GetTextureTarget(void);
	void __fastcall PictureChanged(System::TObject* Sender);
	
public:
	__fastcall virtual TGLCubeMapImage(System::Classes::TPersistent* AOwner);
	__fastcall virtual ~TGLCubeMapImage(void);
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	virtual Glgraphics::TGLImage* __fastcall GetBitmap32(void);
	virtual void __fastcall ReleaseBitmap32(void);
	HIDESBASE void __fastcall BeginUpdate(void);
	HIDESBASE void __fastcall EndUpdate(void);
	DYNAMIC void __fastcall SaveToFile(const System::UnicodeString fileName);
	DYNAMIC void __fastcall LoadFromFile(const System::UnicodeString fileName);
	__classmethod virtual System::UnicodeString __fastcall FriendlyName();
	__classmethod virtual System::UnicodeString __fastcall FriendlyDescription();
	__property NativeTextureTarget;
	__property Vcl::Graphics::TPicture* Picture[int index] = {read=GetPicture, write=SetPicture};
	
__published:
	__property Vcl::Graphics::TPicture* PicturePX = {read=GetPicture, write=SetPicture, index=0};
	__property Vcl::Graphics::TPicture* PictureNX = {read=GetPicture, write=SetPicture, index=1};
	__property Vcl::Graphics::TPicture* PicturePY = {read=GetPicture, write=SetPicture, index=2};
	__property Vcl::Graphics::TPicture* PictureNY = {read=GetPicture, write=SetPicture, index=3};
	__property Vcl::Graphics::TPicture* PicturePZ = {read=GetPicture, write=SetPicture, index=4};
	__property Vcl::Graphics::TPicture* PictureNZ = {read=GetPicture, write=SetPicture, index=5};
};


enum TGLTextureMappingMode : unsigned char { tmmUser, tmmObjectLinear, tmmEyeLinear, tmmSphere, tmmCubeMapReflection, tmmCubeMapNormal, tmmCubeMapLight0, tmmCubeMapCamera };

class PASCALIMPLEMENTATION TGLTexture : public Baseclasses::TGLUpdateAbleObject
{
	typedef Baseclasses::TGLUpdateAbleObject inherited;
	
private:
	Glcontext::TGLTextureHandle* FTextureHandle;
	Glcontext::TGLVirtualHandle* FSamplerHandle;
	Gltextureformat::TGLInternalFormat FTextureFormat;
	TGLTextureMode FTextureMode;
	TGLTextureWrap FTextureWrap;
	TGLMinFilter FMinFilter;
	TGLMagFilter FMagFilter;
	bool FDisabled;
	TGLTextureImage* FImage;
	TGLTextureImageAlpha FImageAlpha;
	float FImageBrightness;
	float FImageGamma;
	TGLTextureMappingMode FMappingMode;
	Glcoordinates::TGLCoordinates4* FMapSCoordinates;
	Glcoordinates::TGLCoordinates4* FMapTCoordinates;
	Glcoordinates::TGLCoordinates4* FMapRCoordinates;
	Glcoordinates::TGLCoordinates4* FMapQCoordinates;
	TTextureNeededEvent FOnTextureNeeded;
	Gltextureformat::TGLInternalCompression FCompression;
	int FRequiredMemorySize;
	Gltextureformat::TGLTextureFilteringQuality FFilteringQuality;
	int FTexWidth;
	int FTexHeight;
	int FTexDepth;
	Glcolor::TGLColor* FEnvColor;
	Glcolor::TGLColor* FBorderColor;
	float FNormalMapScale;
	Gltextureformat::TGLSeparateTextureWrap FTextureWrapS;
	Gltextureformat::TGLSeparateTextureWrap FTextureWrapT;
	Gltextureformat::TGLSeparateTextureWrap FTextureWrapR;
	Gltextureformat::TGLTextureCompareMode fTextureCompareMode;
	Glstate::TComparisonFunction fTextureCompareFunc;
	TGLDepthTextureMode fDepthTextureMode;
	bool FKeepImageAfterTransfer;
	
protected:
	void __fastcall SetImage(TGLTextureImage* AValue);
	void __fastcall SetImageAlpha(const TGLTextureImageAlpha val);
	void __fastcall SetImageBrightness(const float val);
	bool __fastcall StoreBrightness(void);
	void __fastcall SetImageGamma(const float val);
	bool __fastcall StoreGamma(void);
	void __fastcall SetMagFilter(TGLMagFilter AValue);
	void __fastcall SetMinFilter(TGLMinFilter AValue);
	void __fastcall SetTextureMode(TGLTextureMode AValue);
	void __fastcall SetTextureWrap(TGLTextureWrap AValue);
	void __fastcall SetTextureWrapS(Gltextureformat::TGLSeparateTextureWrap AValue);
	void __fastcall SetTextureWrapT(Gltextureformat::TGLSeparateTextureWrap AValue);
	void __fastcall SetTextureWrapR(Gltextureformat::TGLSeparateTextureWrap AValue);
	TGLTextureFormat __fastcall GetTextureFormat(void);
	void __fastcall SetTextureFormat(const TGLTextureFormat val);
	void __fastcall SetTextureFormatEx(const Gltextureformat::TGLInternalFormat val);
	bool __fastcall StoreTextureFormatEx(void);
	void __fastcall SetCompression(const Gltextureformat::TGLInternalCompression val);
	void __fastcall SetFilteringQuality(const Gltextureformat::TGLTextureFilteringQuality val);
	void __fastcall SetMappingMode(const TGLTextureMappingMode val);
	Glcoordinates::TGLCoordinates4* __fastcall GetMappingSCoordinates(void);
	void __fastcall SetMappingSCoordinates(Glcoordinates::TGLCoordinates4* const val);
	bool __fastcall StoreMappingSCoordinates(void);
	Glcoordinates::TGLCoordinates4* __fastcall GetMappingTCoordinates(void);
	void __fastcall SetMappingTCoordinates(Glcoordinates::TGLCoordinates4* const val);
	bool __fastcall StoreMappingTCoordinates(void);
	Glcoordinates::TGLCoordinates4* __fastcall GetMappingRCoordinates(void);
	void __fastcall SetMappingRCoordinates(Glcoordinates::TGLCoordinates4* const val);
	bool __fastcall StoreMappingRCoordinates(void);
	Glcoordinates::TGLCoordinates4* __fastcall GetMappingQCoordinates(void);
	void __fastcall SetMappingQCoordinates(Glcoordinates::TGLCoordinates4* const val);
	bool __fastcall StoreMappingQCoordinates(void);
	void __fastcall SetDisabled(bool AValue);
	void __fastcall SetEnabled(const bool val);
	bool __fastcall GetEnabled(void);
	void __fastcall SetEnvColor(Glcolor::TGLColor* const val);
	void __fastcall SetBorderColor(Glcolor::TGLColor* const val);
	void __fastcall SetNormalMapScale(const float val);
	void __fastcall SetTextureCompareMode(const Gltextureformat::TGLTextureCompareMode val);
	void __fastcall SetTextureCompareFunc(const Glstate::TComparisonFunction val);
	void __fastcall SetDepthTextureMode(const TGLDepthTextureMode val);
	bool __fastcall StoreNormalMapScale(void);
	bool __fastcall StoreImageClassName(void);
	virtual unsigned __fastcall GetHandle(void);
	virtual void __fastcall PrepareImage(unsigned target);
	virtual void __fastcall PrepareParams(unsigned target);
	void __fastcall DoOnTextureNeeded(System::TObject* Sender, System::UnicodeString &textureFileName);
	void __fastcall OnSamplerAllocate(Glcontext::TGLVirtualHandle* Sender, unsigned &Handle);
	void __fastcall OnSamplerDestroy(Glcontext::TGLVirtualHandle* Sender, unsigned &Handle);
	void __fastcall SetTextureErrorImage(void);
	
public:
	__fastcall virtual TGLTexture(System::Classes::TPersistent* AOwner);
	__fastcall virtual ~TGLTexture(void);
	__property TTextureNeededEvent OnTextureNeeded = {read=FOnTextureNeeded, write=FOnTextureNeeded};
	void __fastcall PrepareBuildList(void);
	void __fastcall ApplyMappingMode(void);
	void __fastcall UnApplyMappingMode(void);
	void __fastcall Apply(Glrendercontextinfo::TRenderContextInfo &rci);
	void __fastcall UnApply(Glrendercontextinfo::TRenderContextInfo &rci);
	void __fastcall ApplyAsTexture2(Glrendercontextinfo::TRenderContextInfo &rci, Vectorgeometry::PMatrix textureMatrix = (Vectorgeometry::PMatrix)(0x0));
	void __fastcall UnApplyAsTexture2(Glrendercontextinfo::TRenderContextInfo &rci, bool reloadIdentityTextureMatrix);
	void __fastcall ApplyAsTextureN(int n, Glrendercontextinfo::TRenderContextInfo &rci, Vectorgeometry::PMatrix textureMatrix = (Vectorgeometry::PMatrix)(0x0));
	void __fastcall UnApplyAsTextureN(int n, Glrendercontextinfo::TRenderContextInfo &rci, bool reloadIdentityTextureMatrix);
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	virtual void __fastcall NotifyChange(System::TObject* Sender);
	void __fastcall NotifyImageChange(void);
	void __fastcall NotifyParamsChange(void);
	void __fastcall DestroyHandles(void);
	void __fastcall SetImageClassName(const System::UnicodeString val);
	System::UnicodeString __fastcall GetImageClassName(void);
	int __fastcall TextureImageRequiredMemory(void);
	unsigned __fastcall AllocateHandle(void);
	bool __fastcall IsHandleAllocated(void);
	int __fastcall OpenGLTextureFormat(void);
	bool __fastcall IsFloatType(void);
	__property bool Enabled = {read=GetEnabled, write=SetEnabled, nodefault};
	__property unsigned Handle = {read=GetHandle, nodefault};
	__property Glcontext::TGLTextureHandle* TextureHandle = {read=FTextureHandle};
	__property int TexWidth = {read=FTexWidth, nodefault};
	__property int TexHeight = {read=FTexHeight, nodefault};
	__property int TexDepth = {read=FTexDepth, nodefault};
	
__published:
	__property System::UnicodeString ImageClassName = {read=GetImageClassName, write=SetImageClassName, stored=StoreImageClassName};
	__property TGLTextureImage* Image = {read=FImage, write=SetImage};
	__property TGLTextureImageAlpha ImageAlpha = {read=FImageAlpha, write=SetImageAlpha, default=0};
	__property float ImageBrightness = {read=FImageBrightness, write=SetImageBrightness, stored=StoreBrightness};
	__property float ImageGamma = {read=FImageGamma, write=SetImageGamma, stored=StoreGamma};
	__property TGLMagFilter MagFilter = {read=FMagFilter, write=SetMagFilter, default=1};
	__property TGLMinFilter MinFilter = {read=FMinFilter, write=SetMinFilter, default=5};
	__property TGLTextureMode TextureMode = {read=FTextureMode, write=SetTextureMode, default=0};
	__property TGLTextureWrap TextureWrap = {read=FTextureWrap, write=SetTextureWrap, default=0};
	__property Gltextureformat::TGLSeparateTextureWrap TextureWrapS = {read=FTextureWrapS, write=SetTextureWrapS, default=0};
	__property Gltextureformat::TGLSeparateTextureWrap TextureWrapT = {read=FTextureWrapT, write=SetTextureWrapT, default=0};
	__property Gltextureformat::TGLSeparateTextureWrap TextureWrapR = {read=FTextureWrapR, write=SetTextureWrapR, default=0};
	__property TGLTextureFormat TextureFormat = {read=GetTextureFormat, write=SetTextureFormat, default=0};
	__property Gltextureformat::TGLInternalFormat TextureFormatEx = {read=FTextureFormat, write=SetTextureFormatEx, stored=StoreTextureFormatEx, nodefault};
	__property Gltextureformat::TGLInternalCompression Compression = {read=FCompression, write=SetCompression, default=0};
	__property Gltextureformat::TGLTextureFilteringQuality FilteringQuality = {read=FFilteringQuality, write=SetFilteringQuality, default=0};
	__property TGLTextureMappingMode MappingMode = {read=FMappingMode, write=SetMappingMode, default=0};
	__property Glcoordinates::TGLCoordinates4* MappingSCoordinates = {read=GetMappingSCoordinates, write=SetMappingSCoordinates, stored=StoreMappingSCoordinates};
	__property Glcoordinates::TGLCoordinates4* MappingTCoordinates = {read=GetMappingTCoordinates, write=SetMappingTCoordinates, stored=StoreMappingTCoordinates};
	__property Glcoordinates::TGLCoordinates4* MappingRCoordinates = {read=GetMappingRCoordinates, write=SetMappingRCoordinates, stored=StoreMappingRCoordinates};
	__property Glcoordinates::TGLCoordinates4* MappingQCoordinates = {read=GetMappingQCoordinates, write=SetMappingQCoordinates, stored=StoreMappingQCoordinates};
	__property Glcolor::TGLColor* EnvColor = {read=FEnvColor, write=SetEnvColor};
	__property Glcolor::TGLColor* BorderColor = {read=FBorderColor, write=SetBorderColor};
	__property bool Disabled = {read=FDisabled, write=SetDisabled, default=1};
	__property float NormalMapScale = {read=FNormalMapScale, write=SetNormalMapScale, stored=StoreNormalMapScale};
	__property Gltextureformat::TGLTextureCompareMode TextureCompareMode = {read=fTextureCompareMode, write=SetTextureCompareMode, default=0};
	__property Glstate::TComparisonFunction TextureCompareFunc = {read=fTextureCompareFunc, write=SetTextureCompareFunc, default=3};
	__property TGLDepthTextureMode DepthTextureMode = {read=fDepthTextureMode, write=SetDepthTextureMode, default=0};
	__property bool KeepImageAfterTransfer = {read=FKeepImageAfterTransfer, write=FKeepImageAfterTransfer, default=0};
};


class DELPHICLASS TGLTextureExItem;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLTextureExItem : public System::Classes::TCollectionItem
{
	typedef System::Classes::TCollectionItem inherited;
	
private:
	TGLTexture* FTexture;
	int FTextureIndex;
	Glcoordinates::TGLCoordinates3* FTextureOffset;
	Glcoordinates::TGLCoordinates3* FTextureScale;
	bool FTextureMatrixIsIdentity;
	Vectortypes::TMatrix4f FTextureMatrix;
	bool FApplied;
	HRESULT __stdcall QueryInterface(const GUID &IID, /* out */ void *Obj);
	int __stdcall _AddRef(void);
	int __stdcall _Release(void);
	
protected:
	virtual System::UnicodeString __fastcall GetDisplayName(void);
	DYNAMIC System::Classes::TPersistent* __fastcall GetOwner(void);
	void __fastcall SetTexture(TGLTexture* const Value);
	void __fastcall SetTextureIndex(const int Value);
	void __fastcall SetTextureOffset(Glcoordinates::TGLCoordinates3* const Value);
	void __fastcall SetTextureScale(Glcoordinates::TGLCoordinates3* const Value);
	void __fastcall NotifyTexMapChange(System::TObject* Sender);
	void __fastcall CalculateTextureMatrix(void);
	void __fastcall OnNotifyChange(System::TObject* Sender);
	
public:
	__fastcall virtual TGLTextureExItem(System::Classes::TCollection* ACollection);
	__fastcall virtual ~TGLTextureExItem(void);
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	void __fastcall NotifyChange(System::TObject* Sender);
	void __fastcall Apply(Glrendercontextinfo::TRenderContextInfo &rci);
	void __fastcall UnApply(Glrendercontextinfo::TRenderContextInfo &rci);
	
__published:
	__property TGLTexture* Texture = {read=FTexture, write=SetTexture};
	__property int TextureIndex = {read=FTextureIndex, write=SetTextureIndex, nodefault};
	__property Glcoordinates::TGLCoordinates3* TextureOffset = {read=FTextureOffset, write=SetTextureOffset};
	__property Glcoordinates::TGLCoordinates3* TextureScale = {read=FTextureScale, write=SetTextureScale};
private:
	void *__IGLTextureNotifyAble;	/* IGLTextureNotifyAble */
	
public:
	#if defined(MANAGED_INTERFACE_OPERATORS)
	// {0D9DC0B0-ECE4-4513-A8A1-5AE7022C9426}
	operator _di_IGLTextureNotifyAble()
	{
		_di_IGLTextureNotifyAble intf;
		GetInterface(intf);
		return intf;
	}
	#else
	operator IGLTextureNotifyAble*(void) { return (IGLTextureNotifyAble*)&__IGLTextureNotifyAble; }
	#endif
	
};

#pragma pack(pop)

class DELPHICLASS TGLTextureEx;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLTextureEx : public System::Classes::TCollection
{
	typedef System::Classes::TCollection inherited;
	
public:
	TGLTextureExItem* operator[](int index) { return Items[index]; }
	
private:
	Baseclasses::TGLUpdateAbleObject* FOwner;
	
protected:
	void __fastcall SetItems(int index, TGLTextureExItem* const Value);
	TGLTextureExItem* __fastcall GetItems(int index);
	DYNAMIC System::Classes::TPersistent* __fastcall GetOwner(void);
	
public:
	__fastcall TGLTextureEx(Baseclasses::TGLUpdateAbleObject* AOwner);
	void __fastcall NotifyChange(System::TObject* Sender);
	void __fastcall Apply(Glrendercontextinfo::TRenderContextInfo &rci);
	void __fastcall UnApply(Glrendercontextinfo::TRenderContextInfo &rci);
	bool __fastcall IsTextureEnabled(int Index);
	HIDESBASE TGLTextureExItem* __fastcall Add(void);
	__property TGLTextureExItem* Items[int index] = {read=GetItems, write=SetItems/*, default*/};
	void __fastcall Loaded(void);
public:
	/* TCollection.Destroy */ inline __fastcall virtual ~TGLTextureEx(void) { }
	
};

#pragma pack(pop)

class DELPHICLASS ETexture;
#pragma pack(push,4)
class PASCALIMPLEMENTATION ETexture : public System::Sysutils::Exception
{
	typedef System::Sysutils::Exception inherited;
	
public:
	/* Exception.Create */ inline __fastcall ETexture(const System::UnicodeString Msg) : System::Sysutils::Exception(Msg) { }
	/* Exception.CreateFmt */ inline __fastcall ETexture(const System::UnicodeString Msg, System::TVarRec const *Args, const int Args_Size) : System::Sysutils::Exception(Msg, Args, Args_Size) { }
	/* Exception.CreateRes */ inline __fastcall ETexture(NativeUInt Ident)/* overload */ : System::Sysutils::Exception(Ident) { }
	/* Exception.CreateRes */ inline __fastcall ETexture(System::PResStringRec ResStringRec)/* overload */ : System::Sysutils::Exception(ResStringRec) { }
	/* Exception.CreateResFmt */ inline __fastcall ETexture(NativeUInt Ident, System::TVarRec const *Args, const int Args_Size)/* overload */ : System::Sysutils::Exception(Ident, Args, Args_Size) { }
	/* Exception.CreateResFmt */ inline __fastcall ETexture(System::PResStringRec ResStringRec, System::TVarRec const *Args, const int Args_Size)/* overload */ : System::Sysutils::Exception(ResStringRec, Args, Args_Size) { }
	/* Exception.CreateHelp */ inline __fastcall ETexture(const System::UnicodeString Msg, int AHelpContext) : System::Sysutils::Exception(Msg, AHelpContext) { }
	/* Exception.CreateFmtHelp */ inline __fastcall ETexture(const System::UnicodeString Msg, System::TVarRec const *Args, const int Args_Size, int AHelpContext) : System::Sysutils::Exception(Msg, Args, Args_Size, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall ETexture(NativeUInt Ident, int AHelpContext)/* overload */ : System::Sysutils::Exception(Ident, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall ETexture(System::PResStringRec ResStringRec, int AHelpContext)/* overload */ : System::Sysutils::Exception(ResStringRec, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall ETexture(System::PResStringRec ResStringRec, System::TVarRec const *Args, const int Args_Size, int AHelpContext)/* overload */ : System::Sysutils::Exception(ResStringRec, Args, Args_Size, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall ETexture(NativeUInt Ident, System::TVarRec const *Args, const int Args_Size, int AHelpContext)/* overload */ : System::Sysutils::Exception(Ident, Args, Args_Size, AHelpContext) { }
	/* Exception.Destroy */ inline __fastcall virtual ~ETexture(void) { }
	
};

#pragma pack(pop)

class DELPHICLASS EGLShaderException;
#pragma pack(push,4)
class PASCALIMPLEMENTATION EGLShaderException : public System::Sysutils::Exception
{
	typedef System::Sysutils::Exception inherited;
	
public:
	/* Exception.Create */ inline __fastcall EGLShaderException(const System::UnicodeString Msg) : System::Sysutils::Exception(Msg) { }
	/* Exception.CreateFmt */ inline __fastcall EGLShaderException(const System::UnicodeString Msg, System::TVarRec const *Args, const int Args_Size) : System::Sysutils::Exception(Msg, Args, Args_Size) { }
	/* Exception.CreateRes */ inline __fastcall EGLShaderException(NativeUInt Ident)/* overload */ : System::Sysutils::Exception(Ident) { }
	/* Exception.CreateRes */ inline __fastcall EGLShaderException(System::PResStringRec ResStringRec)/* overload */ : System::Sysutils::Exception(ResStringRec) { }
	/* Exception.CreateResFmt */ inline __fastcall EGLShaderException(NativeUInt Ident, System::TVarRec const *Args, const int Args_Size)/* overload */ : System::Sysutils::Exception(Ident, Args, Args_Size) { }
	/* Exception.CreateResFmt */ inline __fastcall EGLShaderException(System::PResStringRec ResStringRec, System::TVarRec const *Args, const int Args_Size)/* overload */ : System::Sysutils::Exception(ResStringRec, Args, Args_Size) { }
	/* Exception.CreateHelp */ inline __fastcall EGLShaderException(const System::UnicodeString Msg, int AHelpContext) : System::Sysutils::Exception(Msg, AHelpContext) { }
	/* Exception.CreateFmtHelp */ inline __fastcall EGLShaderException(const System::UnicodeString Msg, System::TVarRec const *Args, const int Args_Size, int AHelpContext) : System::Sysutils::Exception(Msg, Args, Args_Size, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall EGLShaderException(NativeUInt Ident, int AHelpContext)/* overload */ : System::Sysutils::Exception(Ident, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall EGLShaderException(System::PResStringRec ResStringRec, int AHelpContext)/* overload */ : System::Sysutils::Exception(ResStringRec, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall EGLShaderException(System::PResStringRec ResStringRec, System::TVarRec const *Args, const int Args_Size, int AHelpContext)/* overload */ : System::Sysutils::Exception(ResStringRec, Args, Args_Size, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall EGLShaderException(NativeUInt Ident, System::TVarRec const *Args, const int Args_Size, int AHelpContext)/* overload */ : System::Sysutils::Exception(Ident, Args, Args_Size, AHelpContext) { }
	/* Exception.Destroy */ inline __fastcall virtual ~EGLShaderException(void) { }
	
};

#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------
#define cDefaultNormalMapScale  (1.250000E-01)
extern PACKAGE void __fastcall RegisterTGraphicClassFileExtension(const System::UnicodeString extension, const Glcrossplatform::TGraphicClass aClass);
extern PACKAGE Vcl::Graphics::TGraphic* __fastcall CreateGraphicFromFile(const System::UnicodeString fileName);
extern PACKAGE void __fastcall RegisterGLTextureImageClass(TGLTextureImageClass textureImageClass);
extern PACKAGE TGLTextureImageClass __fastcall FindGLTextureImageClass(const System::UnicodeString className);
extern PACKAGE TGLTextureImageClass __fastcall FindGLTextureImageClassByFriendlyName(const System::UnicodeString friendlyName);
extern PACKAGE void __fastcall SetGLTextureImageClassesToStrings(System::Classes::TStrings* aStrings);
extern PACKAGE System::Classes::TStrings* __fastcall GetGLTextureImageClassesAsStrings(void);
}	/* namespace Gltexture */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLTEXTURE)
using namespace Gltexture;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// GltextureHPP
