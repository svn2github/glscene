// CodeGear C++Builder
// Copyright (c) 1995, 2012 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLBSP.pas' rev: 24.00 (Win32)

#ifndef GlbspHPP
#define GlbspHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <System.Classes.hpp>	// Pascal unit
#include <GLVectorFileObjects.hpp>	// Pascal unit
#include <GLMaterial.hpp>	// Pascal unit
#include <GLCrossPlatform.hpp>	// Pascal unit
#include <VectorGeometry.hpp>	// Pascal unit
#include <VectorLists.hpp>	// Pascal unit
#include <GLColor.hpp>	// Pascal unit
#include <GLRenderContextInfo.hpp>	// Pascal unit
#include <VectorTypes.hpp>	// Pascal unit
#include <PersistentClasses.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Glbsp
{
//-- type declarations -------------------------------------------------------
struct DECLSPEC_DRECORD TBSPCullingSphere
{
public:
	Vectortypes::TVector4f position;
	float radius;
};


struct DECLSPEC_DRECORD TBSPRenderContextInfo
{
private:
	typedef System::DynamicArray<TBSPCullingSphere> _TBSPRenderContextInfo__1;
	
	
public:
	Vectortypes::TVector4f cameraLocal;
	Glrendercontextinfo::TRenderContextInfo *rci;
	System::Classes::TList* faceGroups;
	_TBSPRenderContextInfo__1 cullingSpheres;
};


enum TBSPRenderSort : unsigned char { rsNone, rsBackToFront, rsFrontToBack };

class DELPHICLASS TBSPClusterVisibility;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TBSPClusterVisibility : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	Vectorgeometry::TByteVector *FData;
	int FSize;
	int FBytesPerCluster;
	int FCount;
	
protected:
	void __fastcall SetCount(int NumClusters);
	bool __fastcall GetVisibility(int Source, int Destination);
	void __fastcall SetVisibility(int Source, int Destination, const bool Value);
	
public:
	__fastcall TBSPClusterVisibility(void);
	__fastcall virtual ~TBSPClusterVisibility(void);
	void __fastcall SetData(System::PByte Source, int NumClusters);
	__property int Count = {read=FCount, write=SetCount, nodefault};
	__property bool Visibility[int src][int dst] = {read=GetVisibility, write=SetVisibility};
};

#pragma pack(pop)

class DELPHICLASS TBSPMeshObject;
class DELPHICLASS TFGBSPNode;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TBSPMeshObject : public Glvectorfileobjects::TMeshObject
{
	typedef Glvectorfileobjects::TMeshObject inherited;
	
private:
	TBSPRenderSort FRenderSort;
	TBSPClusterVisibility* FClusterVisibility;
	bool FUseClusterVisibility;
	
public:
	__fastcall TBSPMeshObject(Glvectorfileobjects::TMeshObjectList* AOwner);
	__fastcall virtual ~TBSPMeshObject(void);
	virtual void __fastcall BuildList(Glrendercontextinfo::TRenderContextInfo &mrci);
	void __fastcall CleanupUnusedNodes(void);
	float __fastcall AverageDepth(void);
	TFGBSPNode* __fastcall FindNodeByPoint(const Vectortypes::TVector4f &aPoint);
	__property TBSPRenderSort RenderSort = {read=FRenderSort, write=FRenderSort, nodefault};
	__property TBSPClusterVisibility* ClusterVisibility = {read=FClusterVisibility};
	__property bool UseClusterVisibility = {read=FUseClusterVisibility, write=FUseClusterVisibility, nodefault};
public:
	/* TMeshObject.Create */ inline __fastcall virtual TBSPMeshObject(void) : Glvectorfileobjects::TMeshObject() { }
	
public:
	/* TPersistentObject.CreateFromFiler */ inline __fastcall TBSPMeshObject(Persistentclasses::TVirtualReader* reader) : Glvectorfileobjects::TMeshObject(reader) { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TFGBSPNode : public Glvectorfileobjects::TFGVertexIndexList
{
	typedef Glvectorfileobjects::TFGVertexIndexList inherited;
	
private:
	Vectortypes::TVector4f FSplitPlane;
	int FPositiveSubNodeIndex;
	int FNegativeSubNodeIndex;
	int FCluster;
	
protected:
	int __fastcall AddLerp(int iA, int iB, float fB, float fA);
	int __fastcall AddLerpIfDistinct(int iA, int iB, int iMid);
	
public:
	__fastcall virtual TFGBSPNode(Glvectorfileobjects::TFaceGroups* AOwner);
	__fastcall virtual ~TFGBSPNode(void);
	void __fastcall IsCulled(const TBSPRenderContextInfo &bsprci, bool &positive, bool &negative);
	void __fastcall CollectNoSort(TBSPRenderContextInfo &bsprci);
	void __fastcall CollectFrontToBack(TBSPRenderContextInfo &bsprci);
	void __fastcall CollectBackToFront(TBSPRenderContextInfo &bsprci);
	Vectortypes::TVector4f __fastcall FindSplitPlane(float triangleSplitCost = 1.000000E+00, float triangleImbalanceCost = 5.000000E-01);
	void __fastcall EvaluateSplitPlane(const Vectortypes::TVector4f &splitPlane, int &nbTriangleSplit, int &nbPositiveTriangles, int &nbNegativeTriangles);
	void __fastcall PerformSplit(const Vectortypes::TVector4f &splitPlane, const int maxTrianglesPerLeaf = 0x7fffffff);
	void __fastcall FixTJunctions(Vectorlists::TIntegerList* const tJunctionsCandidates);
	__property Vectortypes::TVector4f splitPlane = {read=FSplitPlane, write=FSplitPlane};
	__property int PositiveSubNodeIndex = {read=FPositiveSubNodeIndex, write=FPositiveSubNodeIndex, nodefault};
	__property int NegativeSubNodeIndex = {read=FNegativeSubNodeIndex, write=FNegativeSubNodeIndex, nodefault};
	__property int Cluster = {read=FCluster, write=FCluster, nodefault};
public:
	/* TFGVertexIndexList.Create */ inline __fastcall virtual TFGBSPNode(void) : Glvectorfileobjects::TFGVertexIndexList() { }
	
public:
	/* TPersistentObject.CreateFromFiler */ inline __fastcall TFGBSPNode(Persistentclasses::TVirtualReader* reader) : Glvectorfileobjects::TFGVertexIndexList(reader) { }
	
};

#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------
}	/* namespace Glbsp */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLBSP)
using namespace Glbsp;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// GlbspHPP
