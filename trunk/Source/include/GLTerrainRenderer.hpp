// CodeGear C++Builder
// Copyright (c) 1995, 2012 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLTerrainRenderer.pas' rev: 24.00 (Win32)

#ifndef GlterrainrendererHPP
#define GlterrainrendererHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <System.Classes.hpp>	// Pascal unit
#include <GLScene.hpp>	// Pascal unit
#include <GLHeightData.hpp>	// Pascal unit
#include <GLMaterial.hpp>	// Pascal unit
#include <VectorGeometry.hpp>	// Pascal unit
#include <GLContext.hpp>	// Pascal unit
#include <GLROAMPatch.hpp>	// Pascal unit
#include <VectorLists.hpp>	// Pascal unit
#include <GLRenderContextInfo.hpp>	// Pascal unit
#include <VectorTypes.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Glterrainrenderer
{
//-- type declarations -------------------------------------------------------
typedef void __fastcall (__closure *TGetTerrainBoundsEvent)(float &l, float &t, float &r, float &b);

typedef void __fastcall (__closure *TPatchPostRenderEvent)(Glrendercontextinfo::TRenderContextInfo &rci, System::Classes::TList* const patches);

typedef void __fastcall (__closure *THeightDataPostRenderEvent)(Glrendercontextinfo::TRenderContextInfo &rci, System::Classes::TList* const heightDatas);

typedef void __fastcall (__closure *TMaxCLODTrianglesReachedEvent)(Glrendercontextinfo::TRenderContextInfo &rci);

enum TTerrainHighResStyle : unsigned char { hrsFullGeometry, hrsTesselated };

enum TTerrainOcclusionTesselate : unsigned char { totTesselateAlways, totTesselateIfVisible };

enum TTileManagementFlag : unsigned char { tmClearUsedFlags, tmMarkUsedTiles, tmReleaseUnusedTiles, tmAllocateNewTiles, tmWaitForPreparing };

typedef System::Set<TTileManagementFlag, TTileManagementFlag::tmClearUsedFlags, TTileManagementFlag::tmWaitForPreparing>  TTileManagementFlags;

class DELPHICLASS TGLTerrainRenderer;
class PASCALIMPLEMENTATION TGLTerrainRenderer : public Glscene::TGLSceneObject
{
	typedef Glscene::TGLSceneObject inherited;
	
private:
	Glheightdata::THeightDataSource* FHeightDataSource;
	int FTileSize;
	float FQualityDistance;
	float FinvTileSize;
	int FLastTriangleCount;
	float FTilesPerTexture;
	int FMaxCLODTriangles;
	int FCLODPrecision;
	Vectorlists::TAffineVectorList* FBufferVertices;
	Vectorlists::TTexPointList* FBufferTexPoints;
	Vectorlists::TIntegerList* FBufferVertexIndices;
	Glmaterial::TGLMaterialLibrary* FMaterialLibrary;
	TGetTerrainBoundsEvent FOnGetTerrainBounds;
	TPatchPostRenderEvent FOnPatchPostRender;
	THeightDataPostRenderEvent FOnHeightDataPostRender;
	TMaxCLODTrianglesReachedEvent FOnMaxCLODTrianglesReached;
	TTerrainHighResStyle FQualityStyle;
	int FOcclusionFrameSkip;
	TTerrainOcclusionTesselate FOcclusionTesselate;
	
protected:
	System::StaticArray<System::Classes::TList*, 256> FTilesHash;
	void __fastcall MarkAllTilesAsUnused(void);
	void __fastcall ReleaseAllUnusedTiles(void);
	void __fastcall MarkHashedTileAsUsed(const Vectortypes::TVector3f &tilePos);
	Glheightdata::THeightData* __fastcall HashedTile(const Vectortypes::TVector3f &tilePos, bool canAllocate = true)/* overload */;
	Glheightdata::THeightData* __fastcall HashedTile(const int xLeft, const int yTop, bool canAllocate = true)/* overload */;
	void __fastcall SetHeightDataSource(Glheightdata::THeightDataSource* const val);
	void __fastcall SetTileSize(const int val);
	void __fastcall SetTilesPerTexture(const float val);
	void __fastcall SetCLODPrecision(const int val);
	void __fastcall SetMaterialLibrary(Glmaterial::TGLMaterialLibrary* const val);
	void __fastcall SetQualityStyle(const TTerrainHighResStyle val);
	void __fastcall SetOcclusionFrameSkip(int val);
	virtual void __fastcall Notification(System::Classes::TComponent* AComponent, System::Classes::TOperation Operation);
	DYNAMIC void __fastcall DestroyHandle(void);
	DYNAMIC void __fastcall ReleaseAllTiles(void);
	virtual void __fastcall OnTileDestroyed(System::TObject* Sender);
	Glroampatch::TGLROAMPatch* __fastcall GetPreparedPatch(const Vectortypes::TVector3f &tilePos, const Vectortypes::TVector3f &eyePos, float texFactor, System::Classes::TList* hdList);
	
public:
	TTileManagementFlags TileManagement;
	__fastcall virtual TGLTerrainRenderer(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TGLTerrainRenderer(void);
	virtual void __fastcall BuildList(Glrendercontextinfo::TRenderContextInfo &rci);
	virtual bool __fastcall RayCastIntersect(const Vectortypes::TVector4f &rayStart, const Vectortypes::TVector4f &rayVector, Vectorgeometry::PVector intersectPoint = (Vectorgeometry::PVector)(0x0), Vectorgeometry::PVector intersectNormal = (Vectorgeometry::PVector)(0x0));
	virtual float __fastcall InterpolatedHeight(const Vectortypes::TVector4f &p)/* overload */;
	float __fastcall InterpolatedHeight(const Vectortypes::TVector3f &p)/* overload */;
	__property int LastTriangleCount = {read=FLastTriangleCount, nodefault};
	int __fastcall HashedTileCount(void);
	
__published:
	__property Glheightdata::THeightDataSource* HeightDataSource = {read=FHeightDataSource, write=SetHeightDataSource};
	__property int TileSize = {read=FTileSize, write=SetTileSize, default=16};
	__property float TilesPerTexture = {read=FTilesPerTexture, write=SetTilesPerTexture};
	__property Glmaterial::TGLMaterialLibrary* MaterialLibrary = {read=FMaterialLibrary, write=SetMaterialLibrary};
	__property float QualityDistance = {read=FQualityDistance, write=FQualityDistance};
	__property TTerrainHighResStyle QualityStyle = {read=FQualityStyle, write=SetQualityStyle, default=0};
	__property int MaxCLODTriangles = {read=FMaxCLODTriangles, write=FMaxCLODTriangles, default=65536};
	__property int CLODPrecision = {read=FCLODPrecision, write=SetCLODPrecision, default=100};
	__property int OcclusionFrameSkip = {read=FOcclusionFrameSkip, write=SetOcclusionFrameSkip, default=0};
	__property TTerrainOcclusionTesselate OcclusionTesselate = {read=FOcclusionTesselate, write=FOcclusionTesselate, default=1};
	__property TGetTerrainBoundsEvent OnGetTerrainBounds = {read=FOnGetTerrainBounds, write=FOnGetTerrainBounds};
	__property TPatchPostRenderEvent OnPatchPostRender = {read=FOnPatchPostRender, write=FOnPatchPostRender};
	__property THeightDataPostRenderEvent OnHeightDataPostRender = {read=FOnHeightDataPostRender, write=FOnHeightDataPostRender};
	__property TMaxCLODTrianglesReachedEvent OnMaxCLODTrianglesReached = {read=FOnMaxCLODTrianglesReached, write=FOnMaxCLODTrianglesReached};
public:
	/* TGLBaseSceneObject.CreateAsChild */ inline __fastcall TGLTerrainRenderer(Glscene::TGLBaseSceneObject* aParentOwner) : Glscene::TGLSceneObject(aParentOwner) { }
	
};


//-- var, const, procedure ---------------------------------------------------
static const System::Byte cTilesHashSize = System::Byte(0xff);
}	/* namespace Glterrainrenderer */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLTERRAINRENDERER)
using namespace Glterrainrenderer;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// GlterrainrendererHPP
