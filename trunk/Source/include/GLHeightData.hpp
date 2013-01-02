// CodeGear C++Builder
// Copyright (c) 1995, 2012 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLHeightData.pas' rev: 24.00 (Win32)

#ifndef GlheightdataHPP
#define GlheightdataHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <System.SysUtils.hpp>	// Pascal unit
#include <System.Classes.hpp>	// Pascal unit
#include <VectorGeometry.hpp>	// Pascal unit
#include <GLCrossPlatform.hpp>	// Pascal unit
#include <GLMaterial.hpp>	// Pascal unit
#include <BaseClasses.hpp>	// Pascal unit
#include <System.Types.hpp>	// Pascal unit
#include <VectorTypes.hpp>	// Pascal unit
#include <Vcl.Graphics.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Glheightdata
{
//-- type declarations -------------------------------------------------------
typedef System::StaticArray<System::Byte, 1073741824> TByteArray;

typedef System::StaticArray<Vectorgeometry::PByteVector, 268435456> TByteRaster;

typedef TByteRaster *PByteRaster;

typedef System::StaticArray<short, 536870912> TSmallintArray;

typedef TSmallintArray *PSmallIntArray;

typedef System::StaticArray<PSmallIntArray, 268435456> TSmallIntRaster;

typedef TSmallIntRaster *PSmallIntRaster;

typedef System::StaticArray<Vectorgeometry::PFloatVector, 268435456> TSingleRaster;

typedef TSingleRaster *PSingleRaster;

typedef System::TMetaClass* THeightDataClass;

enum THeightDataType : unsigned char { hdtByte, hdtSmallInt, hdtSingle, hdtDefault };

class DELPHICLASS THeightDataSource;
class DELPHICLASS THeightData;
#pragma pack(push,4)
class PASCALIMPLEMENTATION THeightDataSource : public System::Classes::TComponent
{
	typedef System::Classes::TComponent inherited;
	
private:
	System::Classes::TThreadList* FData;
	System::StaticArray<System::Classes::TList*, 256> FDataHash;
	System::Classes::TThread* FThread;
	int FMaxThreads;
	int FMaxPoolSize;
	THeightDataClass FHeightDataClass;
	float FDefaultHeight;
	
protected:
	void __fastcall SetMaxThreads(const int val);
	int __fastcall HashKey(int xLeft, int yTop);
	__property THeightDataClass HeightDataClass = {read=FHeightDataClass, write=FHeightDataClass};
	THeightData* __fastcall FindMatchInList(int xLeft, int yTop, int size, THeightDataType dataType);
	
public:
	__fastcall virtual THeightDataSource(System::Classes::TComponent* AOwner);
	__fastcall virtual ~THeightDataSource(void);
	__property System::Classes::TThreadList* Data = {read=FData};
	void __fastcall Clear(void);
	void __fastcall CleanUp(void);
	virtual THeightData* __fastcall GetData(int xLeft, int yTop, int size, THeightDataType dataType);
	virtual THeightData* __fastcall PreLoad(int xLeft, int yTop, int size, THeightDataType dataType);
	void __fastcall PreloadReplacement(THeightData* aHeightData);
	virtual void __fastcall Release(THeightData* aHeightData);
	virtual void __fastcall MarkDirty(const System::Types::TRect &area)/* overload */;
	void __fastcall MarkDirty(int xLeft, int yTop, int xRight, int yBottom)/* overload */;
	void __fastcall MarkDirty(void)/* overload */;
	__property int MaxThreads = {read=FMaxThreads, write=SetMaxThreads, nodefault};
	__property int MaxPoolSize = {read=FMaxPoolSize, write=FMaxPoolSize, nodefault};
	__property float DefaultHeight = {read=FDefaultHeight, write=FDefaultHeight};
	virtual float __fastcall InterpolatedHeight(float x, float y, int tileSize);
	virtual int __fastcall Width(void) = 0 ;
	virtual int __fastcall Height(void) = 0 ;
	virtual void __fastcall ThreadIsIdle(void);
	virtual void __fastcall BeforePreparingData(THeightData* heightData);
	virtual void __fastcall StartPreparingData(THeightData* heightData);
	virtual void __fastcall AfterPreparingData(THeightData* heightData);
	void __fastcall TextureCoordinates(THeightData* heightData, bool Stretch = false);
};

#pragma pack(pop)

enum THDTextureCoordinatesMode : unsigned char { tcmWorld, tcmLocal };

enum THeightDataState : unsigned char { hdsQueued, hdsPreparing, hdsReady, hdsNone };

typedef void __fastcall (__closure *TOnHeightDataDirtyEvent)(THeightData* sender);

struct DECLSPEC_DRECORD THeightDataUser
{
public:
	System::TObject* user;
	TOnHeightDataDirtyEvent event;
};


class DELPHICLASS THeightDataThread;
class PASCALIMPLEMENTATION THeightData : public Baseclasses::TGLUpdateAbleObject
{
	typedef Baseclasses::TGLUpdateAbleObject inherited;
	
private:
	typedef System::DynamicArray<THeightDataUser> _THeightData__1;
	
	
private:
	_THeightData__1 FUsers;
	THeightDataSource* FOwner;
	THeightDataState FDataState;
	int FSize;
	int FXLeft;
	int FYTop;
	int FUseCounter;
	THeightDataType FDataType;
	int FDataSize;
	Vectorgeometry::TByteVector *FByteData;
	TByteRaster *FByteRaster;
	TSmallintArray *FSmallIntData;
	TSmallIntRaster *FSmallIntRaster;
	Vectorgeometry::TFloatVector *FSingleData;
	TSingleRaster *FSingleRaster;
	THDTextureCoordinatesMode FTextureCoordinatesMode;
	Vectorgeometry::TTexPoint FTCOffset;
	Vectorgeometry::TTexPoint FTCScale;
	System::UnicodeString FMaterialName;
	Glmaterial::TGLLibMaterial* FLibMaterial;
	System::TObject* FObjectTag;
	int FTag;
	int FTag2;
	System::Classes::TNotifyEvent FOnDestroy;
	bool FDirty;
	float FHeightMin;
	float FHeightMax;
	void __fastcall BuildByteRaster(void);
	void __fastcall BuildSmallIntRaster(void);
	void __fastcall BuildSingleRaster(void);
	void __fastcall ConvertByteToSmallInt(void);
	void __fastcall ConvertByteToSingle(void);
	void __fastcall ConvertSmallIntToByte(void);
	void __fastcall ConvertSmallIntToSingle(void);
	void __fastcall ConvertSingleToByte(void);
	void __fastcall ConvertSingleToSmallInt(void);
	
protected:
	THeightDataThread* FThread;
	void __fastcall SetDataType(const THeightDataType val);
	void __fastcall SetMaterialName(const System::UnicodeString MaterialName);
	void __fastcall SetLibMaterial(Glmaterial::TGLLibMaterial* LibMaterial);
	float __fastcall GetHeightMin(void);
	float __fastcall GetHeightMax(void);
	
public:
	THeightData* OldVersion;
	THeightData* NewVersion;
	bool DontUse;
	__fastcall virtual THeightData(THeightDataSource* AOwner, int aXLeft, int aYTop, int aSize, THeightDataType aDataType);
	__fastcall virtual ~THeightData(void);
	__property THeightDataSource* Owner = {read=FOwner};
	__property System::Classes::TNotifyEvent OnDestroy = {read=FOnDestroy, write=FOnDestroy};
	__property int UseCounter = {read=FUseCounter, nodefault};
	void __fastcall RegisterUse(void);
	DYNAMIC void __fastcall Allocate(const THeightDataType val);
	void __fastcall Release(void);
	void __fastcall MarkDirty(void);
	__property int xLeft = {read=FXLeft, nodefault};
	__property int yTop = {read=FYTop, nodefault};
	__property THeightDataType dataType = {read=FDataType, write=SetDataType, nodefault};
	__property THeightDataState DataState = {read=FDataState, write=FDataState, nodefault};
	__property int size = {read=FSize, nodefault};
	__property bool Dirty = {read=FDirty, write=FDirty, nodefault};
	__property int DataSize = {read=FDataSize, nodefault};
	__property Vectorgeometry::PByteVector ByteData = {read=FByteData};
	__property PByteRaster ByteRaster = {read=FByteRaster};
	__property PSmallIntArray SmallIntData = {read=FSmallIntData};
	__property PSmallIntRaster SmallIntRaster = {read=FSmallIntRaster};
	__property Vectorgeometry::PFloatVector SingleData = {read=FSingleData};
	__property PSingleRaster SingleRaster = {read=FSingleRaster};
	__property System::UnicodeString MaterialName = {read=FMaterialName, write=SetMaterialName};
	__property Glmaterial::TGLLibMaterial* LibMaterial = {read=FLibMaterial, write=SetLibMaterial};
	__property THDTextureCoordinatesMode TextureCoordinatesMode = {read=FTextureCoordinatesMode, write=FTextureCoordinatesMode, nodefault};
	__property Vectorgeometry::TTexPoint TextureCoordinatesOffset = {read=FTCOffset, write=FTCOffset};
	__property Vectorgeometry::TTexPoint TextureCoordinatesScale = {read=FTCScale, write=FTCScale};
	System::Byte __fastcall ByteHeight(int x, int y);
	short __fastcall SmallIntHeight(int x, int y);
	float __fastcall SingleHeight(int x, int y);
	float __fastcall InterpolatedHeight(float x, float y);
	__property float HeightMin = {read=GetHeightMin, write=FHeightMin};
	__property float HeightMax = {read=GetHeightMax, write=FHeightMax};
	float __fastcall Height(int x, int y);
	virtual Vectortypes::TVector3f __fastcall Normal(int x, int y, const Vectortypes::TVector3f &scale);
	virtual Vectortypes::TVector3f __fastcall NormalAtNode(int x, int y, const Vectortypes::TVector3f &scale);
	bool __fastcall OverlapsArea(const System::Types::TRect &area);
	__property System::TObject* ObjectTag = {read=FObjectTag, write=FObjectTag};
	__property int Tag = {read=FTag, write=FTag, nodefault};
	__property int Tag2 = {read=FTag2, write=FTag2, nodefault};
	__property THeightDataThread* Thread = {read=FThread, write=FThread};
};


class PASCALIMPLEMENTATION THeightDataThread : public System::Classes::TThread
{
	typedef System::Classes::TThread inherited;
	
protected:
	THeightData* FHeightData;
	
public:
	__fastcall virtual ~THeightDataThread(void);
	__property THeightData* heightData = {read=FHeightData, write=FHeightData};
public:
	/* TThread.Create */ inline __fastcall THeightDataThread(void)/* overload */ : System::Classes::TThread() { }
	/* TThread.Create */ inline __fastcall THeightDataThread(bool CreateSuspended)/* overload */ : System::Classes::TThread(CreateSuspended) { }
	
};


class DELPHICLASS TGLBitmapHDS;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLBitmapHDS : public THeightDataSource
{
	typedef THeightDataSource inherited;
	
private:
	typedef System::DynamicArray<Vectorgeometry::PByteVector> _TGLBitmapHDS__1;
	
	
private:
	_TGLBitmapHDS__1 FScanLineCache;
	Vcl::Graphics::TBitmap* FBitmap;
	Vcl::Graphics::TPicture* FPicture;
	bool FInfiniteWrap;
	bool FInverted;
	
protected:
	void __fastcall SetPicture(Vcl::Graphics::TPicture* const val);
	void __fastcall OnPictureChanged(System::TObject* sender);
	void __fastcall SetInfiniteWrap(bool val);
	void __fastcall SetInverted(bool val);
	void __fastcall CreateMonochromeBitmap(int size);
	void __fastcall FreeMonochromeBitmap(void);
	Vectorgeometry::PByteVector __fastcall GetScanLine(int y);
	
public:
	__fastcall virtual TGLBitmapHDS(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TGLBitmapHDS(void);
	virtual void __fastcall StartPreparingData(THeightData* heightData);
	virtual void __fastcall MarkDirty(const System::Types::TRect &area)/* overload */;
	virtual int __fastcall Width(void);
	virtual int __fastcall Height(void);
	
__published:
	__property Vcl::Graphics::TPicture* Picture = {read=FPicture, write=SetPicture};
	__property bool InfiniteWrap = {read=FInfiniteWrap, write=SetInfiniteWrap, default=1};
	__property bool Inverted = {read=FInverted, write=SetInverted, default=1};
	__property MaxPoolSize;
/* Hoisted overloads: */
	
public:
	inline void __fastcall  MarkDirty(int xLeft, int yTop, int xRight, int yBottom){ THeightDataSource::MarkDirty(xLeft, yTop, xRight, yBottom); }
	inline void __fastcall  MarkDirty(void){ THeightDataSource::MarkDirty(); }
	
};

#pragma pack(pop)

typedef void __fastcall (__closure *TStartPreparingDataEvent)(THeightData* heightData);

typedef void __fastcall (__closure *TMarkDirtyEvent)(const System::Types::TRect &area);

class DELPHICLASS TGLCustomHDS;
class PASCALIMPLEMENTATION TGLCustomHDS : public THeightDataSource
{
	typedef THeightDataSource inherited;
	
private:
	TStartPreparingDataEvent FOnStartPreparingData;
	TMarkDirtyEvent FOnMarkDirty;
	
public:
	__fastcall virtual TGLCustomHDS(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TGLCustomHDS(void);
	virtual void __fastcall StartPreparingData(THeightData* heightData);
	virtual void __fastcall MarkDirty(const System::Types::TRect &area)/* overload */;
	
__published:
	__property MaxPoolSize;
	__property TStartPreparingDataEvent OnStartPreparingData = {read=FOnStartPreparingData, write=FOnStartPreparingData};
	__property TMarkDirtyEvent OnMarkDirtyEvent = {read=FOnMarkDirty, write=FOnMarkDirty};
/* Hoisted overloads: */
	
public:
	inline void __fastcall  MarkDirty(int xLeft, int yTop, int xRight, int yBottom){ THeightDataSource::MarkDirty(xLeft, yTop, xRight, yBottom); }
	inline void __fastcall  MarkDirty(void){ THeightDataSource::MarkDirty(); }
	
};


class DELPHICLASS TGLTerrainBaseHDS;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLTerrainBaseHDS : public THeightDataSource
{
	typedef THeightDataSource inherited;
	
public:
	__fastcall virtual TGLTerrainBaseHDS(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TGLTerrainBaseHDS(void);
	virtual void __fastcall StartPreparingData(THeightData* heightData);
	
__published:
	__property MaxPoolSize;
};

#pragma pack(pop)

class DELPHICLASS THeightDataSourceFilter;
typedef void __fastcall (__closure *TSourceDataFetchedEvent)(THeightDataSourceFilter* sender, THeightData* heightData);

class PASCALIMPLEMENTATION THeightDataSourceFilter : public THeightDataSource
{
	typedef THeightDataSource inherited;
	
private:
	THeightDataSource* FHDS;
	TSourceDataFetchedEvent FOnSourceDataFetched;
	bool FActive;
	
protected:
	virtual void __fastcall PreparingData(THeightData* heightData) = 0 ;
	void __fastcall SetHDS(THeightDataSource* val);
	
public:
	__fastcall virtual THeightDataSourceFilter(System::Classes::TComponent* AOwner);
	__fastcall virtual ~THeightDataSourceFilter(void);
	virtual void __fastcall Release(THeightData* aHeightData);
	virtual void __fastcall StartPreparingData(THeightData* heightData);
	virtual void __fastcall Notification(System::Classes::TComponent* AComponent, System::Classes::TOperation Operation);
	virtual int __fastcall Width(void);
	virtual int __fastcall Height(void);
	__property TSourceDataFetchedEvent OnSourceDataFetched = {read=FOnSourceDataFetched, write=FOnSourceDataFetched};
	
__published:
	__property MaxPoolSize;
	__property THeightDataSource* HeightDataSource = {read=FHDS, write=SetHDS};
	__property bool Active = {read=FActive, write=FActive, nodefault};
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Glheightdata */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLHEIGHTDATA)
using namespace Glheightdata;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// GlheightdataHPP
