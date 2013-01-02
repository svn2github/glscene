// CodeGear C++Builder
// Copyright (c) 1995, 2012 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLImposter.pas' rev: 24.00 (Win32)

#ifndef GlimposterHPP
#define GlimposterHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <System.Classes.hpp>	// Pascal unit
#include <GLScene.hpp>	// Pascal unit
#include <GLContext.hpp>	// Pascal unit
#include <VectorTypes.hpp>	// Pascal unit
#include <VectorGeometry.hpp>	// Pascal unit
#include <PersistentClasses.hpp>	// Pascal unit
#include <GLCrossPlatform.hpp>	// Pascal unit
#include <GLGraphics.hpp>	// Pascal unit
#include <GLColor.hpp>	// Pascal unit
#include <GLRenderContextInfo.hpp>	// Pascal unit
#include <GLCoordinates.hpp>	// Pascal unit
#include <BaseClasses.hpp>	// Pascal unit
#include <GLState.hpp>	// Pascal unit
#include <GLTextureFormat.hpp>	// Pascal unit
#include <System.Types.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Glimposter
{
//-- type declarations -------------------------------------------------------
enum TImposterOption : unsigned char { impoBlended, impoAlphaTest, impoNearestFiltering, impoPerspectiveCorrection };

typedef System::Set<TImposterOption, TImposterOption::impoBlended, TImposterOption::impoPerspectiveCorrection>  TImposterOptions;

class DELPHICLASS TImposter;
class DELPHICLASS TGLImposterBuilder;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TImposter : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	int FRequestCount;
	TGLImposterBuilder* FBuilder;
	Glcontext::TGLTextureHandle* FTexture;
	Glscene::TGLBaseSceneObject* FImpostoredObject;
	float FAspectRatio;
	bool FModulated;
	
protected:
	Vectortypes::TVector4f FVx;
	Vectortypes::TVector4f FVy;
	Vectortypes::TVector4f FStaticOffset;
	System::StaticArray<Vectortypes::TVector4f, 4> FQuad;
	float FStaticScale;
	DYNAMIC void __fastcall PrepareTexture(Glrendercontextinfo::TRenderContextInfo &rci);
	void __fastcall RenderQuad(const Vectortypes::TVector4f &texExtents, const Vectortypes::TVector4f &objPos, float size);
	
public:
	__fastcall virtual TImposter(TGLImposterBuilder* aBuilder);
	__fastcall virtual ~TImposter(void);
	virtual void __fastcall BeginRender(Glrendercontextinfo::TRenderContextInfo &rci);
	virtual void __fastcall Render(Glrendercontextinfo::TRenderContextInfo &rci, const Vectortypes::TVector4f &objPos, const Vectortypes::TVector4f &localCameraPos, float size);
	virtual void __fastcall EndRender(Glrendercontextinfo::TRenderContextInfo &rci);
	void __fastcall RenderOnce(Glrendercontextinfo::TRenderContextInfo &rci, const Vectortypes::TVector4f &objPos, const Vectortypes::TVector4f &localCameraPos, float size);
	__property float AspectRatio = {read=FAspectRatio, write=FAspectRatio};
	__property TGLImposterBuilder* Builder = {read=FBuilder};
	__property Glcontext::TGLTextureHandle* Texture = {read=FTexture};
	__property Glscene::TGLBaseSceneObject* ImpostoredObject = {read=FImpostoredObject, write=FImpostoredObject};
	__property bool Modulated = {read=FModulated, write=FModulated, nodefault};
};

#pragma pack(pop)

typedef Glgraphics::TGLImage* __fastcall (__closure *TLoadingImposterEvent)(System::TObject* Sender, Glscene::TGLBaseSceneObject* impostoredObject, TImposter* destImposter);

typedef void __fastcall (__closure *TImposterLoadedEvent)(System::TObject* Sender, Glscene::TGLBaseSceneObject* impostoredObject, TImposter* destImposter);

enum TImposterReference : unsigned char { irCenter, irTop, irBottom };

class PASCALIMPLEMENTATION TGLImposterBuilder : public Baseclasses::TGLUpdateAbleComponent
{
	typedef Baseclasses::TGLUpdateAbleComponent inherited;
	
private:
	Glcolor::TGLColor* FBackColor;
	Glcoordinates::TGLCoordinates3* FBuildOffset;
	Persistentclasses::TPersistentObjectList* FImposterRegister;
	Glscene::TGLRenderPoint* FRenderPoint;
	TImposterOptions FImposterOptions;
	float FAlphaTreshold;
	TImposterReference FImposterReference;
	TLoadingImposterEvent FOnLoadingImposter;
	TImposterLoadedEvent FOnImposterLoaded;
	
protected:
	void __fastcall SetRenderPoint(Glscene::TGLRenderPoint* AValue);
	void __fastcall RenderPointFreed(System::TObject* Sender);
	void __fastcall SetBackColor(Glcolor::TGLColor* AValue);
	void __fastcall SetBuildOffset(Glcoordinates::TGLCoordinates3* AValue);
	void __fastcall SetImposterReference(TImposterReference AValue);
	void __fastcall InitializeImpostorTexture(const System::Types::TPoint &textureSize);
	__property Persistentclasses::TPersistentObjectList* ImposterRegister = {read=FImposterRegister};
	void __fastcall UnregisterImposter(TImposter* imposter);
	virtual TImposter* __fastcall CreateNewImposter(void);
	virtual void __fastcall PrepareImposters(System::TObject* Sender, Glrendercontextinfo::TRenderContextInfo &rci);
	virtual void __fastcall DoPrepareImposter(Glrendercontextinfo::TRenderContextInfo &rci, Glscene::TGLBaseSceneObject* impostoredObject, TImposter* destImposter) = 0 ;
	virtual void __fastcall DoUserSpecifiedImposter(Glrendercontextinfo::TRenderContextInfo &rci, TImposter* destImposter, Glgraphics::TGLImage* bmp32);
	
public:
	__fastcall virtual TGLImposterBuilder(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TGLImposterBuilder(void);
	virtual void __fastcall Notification(System::Classes::TComponent* AComponent, System::Classes::TOperation Operation);
	virtual void __fastcall NotifyChange(System::TObject* Sender);
	TImposter* __fastcall ImposterFor(Glscene::TGLBaseSceneObject* impostoredObject);
	void __fastcall RequestImposterFor(Glscene::TGLBaseSceneObject* impostoredObject);
	void __fastcall UnRequestImposterFor(Glscene::TGLBaseSceneObject* impostoredObject);
	
__published:
	__property Glscene::TGLRenderPoint* RenderPoint = {read=FRenderPoint, write=SetRenderPoint};
	__property Glcolor::TGLColor* BackColor = {read=FBackColor, write=SetBackColor};
	__property Glcoordinates::TGLCoordinates3* BuildOffset = {read=FBuildOffset, write=SetBuildOffset};
	__property TImposterOptions ImposterOptions = {read=FImposterOptions, write=FImposterOptions, default=3};
	__property TImposterReference ImposterReference = {read=FImposterReference, write=SetImposterReference, default=0};
	__property float AlphaTreshold = {read=FAlphaTreshold, write=FAlphaTreshold};
	__property TLoadingImposterEvent OnLoadingImposter = {read=FOnLoadingImposter, write=FOnLoadingImposter};
	__property TImposterLoadedEvent OnImposterLoaded = {read=FOnImposterLoaded, write=FOnImposterLoaded};
};


class DELPHICLASS TGLStaticImposterBuilderCorona;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLStaticImposterBuilderCorona : public System::Classes::TCollectionItem
{
	typedef System::Classes::TCollectionItem inherited;
	
private:
	int FSamples;
	float FElevation;
	int FSampleBaseIndex;
	
protected:
	virtual System::UnicodeString __fastcall GetDisplayName(void);
	void __fastcall SetSamples(int AValue);
	void __fastcall SetElevation(float AValue);
	
public:
	__fastcall virtual TGLStaticImposterBuilderCorona(System::Classes::TCollection* ACollection);
	__fastcall virtual ~TGLStaticImposterBuilderCorona(void);
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	
__published:
	__property int Samples = {read=FSamples, write=SetSamples, default=8};
	__property float Elevation = {read=FElevation, write=SetElevation};
};

#pragma pack(pop)

struct DECLSPEC_DRECORD TCoronaTangentLookup
{
public:
	float minTan;
	float maxTan;
	TGLStaticImposterBuilderCorona* corona;
};


class DELPHICLASS TGLStaticImposterBuilderCoronas;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLStaticImposterBuilderCoronas : public System::Classes::TOwnedCollection
{
	typedef System::Classes::TOwnedCollection inherited;
	
private:
	typedef System::DynamicArray<TCoronaTangentLookup> _TGLStaticImposterBuilderCoronas__1;
	
	
public:
	TGLStaticImposterBuilderCorona* operator[](int AIndex) { return Items[AIndex]; }
	
private:
	_TGLStaticImposterBuilderCoronas__1 FCoronaTangentLookup;
	
protected:
	void __fastcall SetItems(int AIndex, TGLStaticImposterBuilderCorona* const AValue);
	TGLStaticImposterBuilderCorona* __fastcall GetItems(int AIndex);
	virtual void __fastcall Update(System::Classes::TCollectionItem* Item);
	void __fastcall PrepareSampleBaseIndices(void);
	void __fastcall PrepareCoronaTangentLookup(void);
	TGLStaticImposterBuilderCorona* __fastcall CoronaForElevationTangent(float aTangent);
	
public:
	__fastcall TGLStaticImposterBuilderCoronas(System::Classes::TPersistent* AOwner);
	HIDESBASE TGLStaticImposterBuilderCorona* __fastcall Add(void)/* overload */;
	HIDESBASE TGLStaticImposterBuilderCorona* __fastcall Add(const float elevation, int samples)/* overload */;
	__property TGLStaticImposterBuilderCorona* Items[int AIndex] = {read=GetItems, write=SetItems/*, default*/};
	int __fastcall SampleCount(void);
	virtual void __fastcall NotifyChange(void);
	virtual void __fastcall EndUpdate(void);
public:
	/* TCollection.Destroy */ inline __fastcall virtual ~TGLStaticImposterBuilderCoronas(void) { }
	
};

#pragma pack(pop)

class DELPHICLASS TStaticImposter;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TStaticImposter : public TImposter
{
	typedef TImposter inherited;
	
public:
	virtual void __fastcall Render(Glrendercontextinfo::TRenderContextInfo &rci, const Vectortypes::TVector4f &objPos, const Vectortypes::TVector4f &localCameraPos, float size);
public:
	/* TImposter.Create */ inline __fastcall virtual TStaticImposter(TGLImposterBuilder* aBuilder) : TImposter(aBuilder) { }
	/* TImposter.Destroy */ inline __fastcall virtual ~TStaticImposter(void) { }
	
};

#pragma pack(pop)

enum TSIBLigthing : unsigned char { siblNoLighting, siblStaticLighting, siblLocalLighting };

class DELPHICLASS TGLStaticImposterBuilder;
class PASCALIMPLEMENTATION TGLStaticImposterBuilder : public TGLImposterBuilder
{
	typedef TGLImposterBuilder inherited;
	
private:
	TGLStaticImposterBuilderCoronas* FCoronas;
	int FSampleSize;
	System::Types::TPoint FTextureSize;
	System::Types::TPoint FSamplesPerAxis;
	Vectortypes::TVector2f FInvSamplesPerAxis;
	float FSamplingRatioBias;
	float FInvSamplingRatioBias;
	TSIBLigthing FLighting;
	float FSamplesAlphaScale;
	
protected:
	void __fastcall SetCoronas(TGLStaticImposterBuilderCoronas* AValue);
	void __fastcall SetSampleSize(int AValue);
	void __fastcall SetSamplingRatioBias(float AValue);
	bool __fastcall StoreSamplingRatioBias(void);
	void __fastcall SetLighting(TSIBLigthing AValue);
	void __fastcall SetSamplesAlphaScale(float AValue);
	bool __fastcall StoreSamplesAlphaScale(void);
	System::UnicodeString __fastcall GetTextureSizeInfo(void);
	void __fastcall SetTextureSizeInfo(const System::UnicodeString texSize);
	System::Types::TPoint __fastcall ComputeOptimalTextureSize(void);
	virtual TImposter* __fastcall CreateNewImposter(void);
	virtual void __fastcall DoPrepareImposter(Glrendercontextinfo::TRenderContextInfo &rci, Glscene::TGLBaseSceneObject* impostoredObject, TImposter* destImposter);
	virtual void __fastcall DoUserSpecifiedImposter(Glrendercontextinfo::TRenderContextInfo &rci, TImposter* destImposter, Glgraphics::TGLImage* bmp32);
	void __fastcall ComputeStaticParams(TImposter* destImposter);
	
public:
	__fastcall virtual TGLStaticImposterBuilder(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TGLStaticImposterBuilder(void);
	void __fastcall Render(Glrendercontextinfo::TRenderContextInfo &rci, Glscene::TGLBaseSceneObject* impostoredObject, TImposter* destImposter);
	float __fastcall TextureFillRatio(void);
	__property System::Types::TPoint TextureSize = {read=FTextureSize};
	__property System::Types::TPoint SamplesPerAxis = {read=FSamplesPerAxis};
	
__published:
	__property TGLStaticImposterBuilderCoronas* Coronas = {read=FCoronas, write=SetCoronas};
	__property int SampleSize = {read=FSampleSize, write=SetSampleSize, default=32};
	__property float SamplingRatioBias = {read=FSamplingRatioBias, write=SetSamplingRatioBias, stored=StoreSamplingRatioBias};
	__property float SamplesAlphaScale = {read=FSamplesAlphaScale, write=SetSamplesAlphaScale, stored=StoreSamplesAlphaScale};
	__property TSIBLigthing Lighting = {read=FLighting, write=FLighting, default=1};
	__property System::UnicodeString TextureSizeInfo = {read=GetTextureSizeInfo, write=SetTextureSizeInfo, stored=false};
};


class DELPHICLASS TGLDynamicImposterBuilder;
class PASCALIMPLEMENTATION TGLDynamicImposterBuilder : public TGLImposterBuilder
{
	typedef TGLImposterBuilder inherited;
	
private:
	int FMinTexSize;
	int FMaxTexSize;
	float FMinDistance;
	float FTolerance;
	bool FUseMatrixError;
	
protected:
	void __fastcall SetMinDistance(const float AValue);
	
public:
	__fastcall virtual TGLDynamicImposterBuilder(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TGLDynamicImposterBuilder(void);
	
__published:
	__property int MinTexSize = {read=FMinTexSize, write=FMinTexSize, nodefault};
	__property int MaxTexSize = {read=FMaxTexSize, write=FMaxTexSize, nodefault};
	__property float MinDistance = {read=FMinDistance, write=SetMinDistance};
	__property float Tolerance = {read=FTolerance, write=FTolerance};
	__property bool UseMatrixError = {read=FUseMatrixError, write=FUseMatrixError, nodefault};
};


class DELPHICLASS TGLImposter;
class PASCALIMPLEMENTATION TGLImposter : public Glscene::TGLImmaterialSceneObject
{
	typedef Glscene::TGLImmaterialSceneObject inherited;
	
private:
	TGLImposterBuilder* FBuilder;
	Glscene::TGLBaseSceneObject* FImpostoredObject;
	
protected:
	void __fastcall SetBuilder(TGLImposterBuilder* const AValue);
	void __fastcall SetImpostoredObject(Glscene::TGLBaseSceneObject* const AValue);
	
public:
	__fastcall virtual TGLImposter(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TGLImposter(void);
	virtual void __fastcall Notification(System::Classes::TComponent* AComponent, System::Classes::TOperation Operation);
	virtual void __fastcall DoRender(Glrendercontextinfo::TRenderContextInfo &ARci, bool ARenderSelf, bool ARenderChildren);
	
__published:
	__property TGLImposterBuilder* Builder = {read=FBuilder, write=SetBuilder};
	__property Glscene::TGLBaseSceneObject* ImpostoredObject = {read=FImpostoredObject, write=SetImpostoredObject};
public:
	/* TGLBaseSceneObject.CreateAsChild */ inline __fastcall TGLImposter(Glscene::TGLBaseSceneObject* aParentOwner) : Glscene::TGLImmaterialSceneObject(aParentOwner) { }
	
};


//-- var, const, procedure ---------------------------------------------------
#define cDefaultImposterOptions (System::Set<TImposterOption, TImposterOption::impoBlended, TImposterOption::impoPerspectiveCorrection> () << TImposterOption::impoBlended << TImposterOption::impoAlphaTest )
}	/* namespace Glimposter */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLIMPOSTER)
using namespace Glimposter;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// GlimposterHPP
