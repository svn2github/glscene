// CodeGear C++Builder
// Copyright (c) 1995, 2012 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'SpatialPartitioning.pas' rev: 24.00 (Win32)

#ifndef SpatialpartitioningHPP
#define SpatialpartitioningHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <System.Classes.hpp>	// Pascal unit
#include <VectorTypes.hpp>	// Pascal unit
#include <VectorGeometry.hpp>	// Pascal unit
#include <System.SysUtils.hpp>	// Pascal unit
#include <GeometryBB.hpp>	// Pascal unit
#include <PersistentClasses.hpp>	// Pascal unit
#include <System.Math.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Spatialpartitioning
{
//-- type declarations -------------------------------------------------------
struct DECLSPEC_DRECORD TSPCone
{
public:
	Vectortypes::TVector3f Base;
	Vectortypes::TVector3f Axis;
	float Angle;
	float Length;
};


struct DECLSPEC_DRECORD TExtendedFrustum
{
public:
	Vectorgeometry::TFrustum Frustum;
	Geometrybb::TBSphere BSphere;
};


class DELPHICLASS TSpacePartitionLeaf;
class DELPHICLASS TBaseSpacePartition;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TSpacePartitionLeaf : public Persistentclasses::TPersistentObject
{
	typedef Persistentclasses::TPersistentObject inherited;
	
private:
	TBaseSpacePartition* FSpacePartition;
	void __fastcall SetSpacePartition(TBaseSpacePartition* const Value);
	
public:
	void *FPartitionTag;
	Geometrybb::TAABB FCachedAABB;
	Geometrybb::TBSphere FCachedBSphere;
	virtual void __fastcall Changed(void);
	virtual void __fastcall UpdateCachedAABBAndBSphere(void);
	__property TBaseSpacePartition* SpacePartition = {read=FSpacePartition, write=SetSpacePartition};
	__property void * PartitionTag = {read=FPartitionTag};
	__fastcall TSpacePartitionLeaf(TBaseSpacePartition* SpacePartition);
	__fastcall virtual ~TSpacePartitionLeaf(void);
public:
	/* TPersistentObject.Create */ inline __fastcall virtual TSpacePartitionLeaf(void) : Persistentclasses::TPersistentObject() { }
	/* TPersistentObject.CreateFromFiler */ inline __fastcall TSpacePartitionLeaf(Persistentclasses::TVirtualReader* reader) : Persistentclasses::TPersistentObject(reader) { }
	
};

#pragma pack(pop)

class DELPHICLASS TSpacePartitionLeafList;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TSpacePartitionLeafList : public Persistentclasses::TPersistentObjectList
{
	typedef Persistentclasses::TPersistentObjectList inherited;
	
public:
	TSpacePartitionLeaf* operator[](int I) { return Items[I]; }
	
private:
	TSpacePartitionLeaf* __fastcall GetItems(int I);
	void __fastcall SetItems(int I, TSpacePartitionLeaf* const Value);
	
public:
	__property TSpacePartitionLeaf* Items[int I] = {read=GetItems, write=SetItems/*, default*/};
	__fastcall virtual TSpacePartitionLeafList(void);
public:
	/* TPersistentObjectList.Destroy */ inline __fastcall virtual ~TSpacePartitionLeafList(void) { }
	
public:
	/* TPersistentObject.CreateFromFiler */ inline __fastcall TSpacePartitionLeafList(Persistentclasses::TVirtualReader* reader) : Persistentclasses::TPersistentObjectList(reader) { }
	
};

#pragma pack(pop)

enum TCullingMode : unsigned char { CmFineCulling, CmGrossCulling };

#pragma pack(push,4)
class PASCALIMPLEMENTATION TBaseSpacePartition : public Persistentclasses::TPersistentObject
{
	typedef Persistentclasses::TPersistentObject inherited;
	
private:
	TCullingMode FCullingMode;
	virtual int __fastcall QueryCone(const TSPCone &ACone);
	
protected:
	TSpacePartitionLeafList* FQueryResult;
	int FQueryInterObjectTests;
	virtual void __fastcall FlushQueryResult(void);
	
public:
	__property TSpacePartitionLeafList* QueryResult = {read=FQueryResult};
	virtual void __fastcall Clear(void);
	virtual void __fastcall AddLeaf(TSpacePartitionLeaf* ALeaf);
	virtual void __fastcall RemoveLeaf(TSpacePartitionLeaf* ALeaf);
	virtual void __fastcall LeafChanged(TSpacePartitionLeaf* ALeaf);
	virtual int __fastcall QueryAABB(const Geometrybb::TAABB &AAABB);
	virtual int __fastcall QueryBSphere(const Geometrybb::TBSphere &ABSphere);
	virtual int __fastcall QueryLeaf(TSpacePartitionLeaf* const ALeaf);
	virtual int __fastcall QueryPlane(const Vectortypes::TVector3f &Location, const Vectortypes::TVector3f &Normal);
	virtual int __fastcall QueryFrustum(const Vectorgeometry::TFrustum &Frustum);
	virtual int __fastcall QueryFrustumEx(const TExtendedFrustum &ExtendedFrustum);
	__property int QueryInterObjectTests = {read=FQueryInterObjectTests, nodefault};
	virtual void __fastcall ProcessUpdated(void);
	__property TCullingMode CullingMode = {read=FCullingMode, write=FCullingMode, nodefault};
	__fastcall virtual TBaseSpacePartition(void);
	__fastcall virtual ~TBaseSpacePartition(void);
public:
	/* TPersistentObject.CreateFromFiler */ inline __fastcall TBaseSpacePartition(Persistentclasses::TVirtualReader* reader) : Persistentclasses::TPersistentObject(reader) { }
	
};

#pragma pack(pop)

class DELPHICLASS TLeavedSpacePartition;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TLeavedSpacePartition : public TBaseSpacePartition
{
	typedef TBaseSpacePartition inherited;
	
private:
	TSpacePartitionLeafList* FLeaves;
	virtual int __fastcall QueryCone(const TSPCone &ACone);
	
public:
	virtual void __fastcall Clear(void);
	virtual void __fastcall AddLeaf(TSpacePartitionLeaf* ALeaf);
	virtual void __fastcall RemoveLeaf(TSpacePartitionLeaf* ALeaf);
	virtual int __fastcall QueryAABB(const Geometrybb::TAABB &AAABB);
	virtual int __fastcall QueryBSphere(const Geometrybb::TBSphere &ABSphere);
	virtual int __fastcall QueryPlane(const Vectortypes::TVector3f &FLocation, const Vectortypes::TVector3f &FNormal);
	__fastcall virtual TLeavedSpacePartition(void);
	__fastcall virtual ~TLeavedSpacePartition(void);
	
__published:
	__property TSpacePartitionLeafList* Leaves = {read=FLeaves};
public:
	/* TPersistentObject.CreateFromFiler */ inline __fastcall TLeavedSpacePartition(Persistentclasses::TVirtualReader* reader) : TBaseSpacePartition(reader) { }
	
};

#pragma pack(pop)

class DELPHICLASS TSectorNode;
typedef System::StaticArray<TSectorNode*, 8> TSectorNodeArray;

class DELPHICLASS TSectoredSpacePartition;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TSectorNode : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	TSpacePartitionLeafList* FLeaves;
	Geometrybb::TAABB FAABB;
	TSectoredSpacePartition* FSectoredSpacePartition;
	int FRecursiveLeafCount;
	TSectorNode* FParent;
	int FNodeDepth;
	int FChildCount;
	TSectorNodeArray FChildren;
	Geometrybb::TBSphere FBSphere;
	bool __fastcall GetNoChildren(void);
	void __fastcall SetAABB(const Geometrybb::TAABB &Value);
	Vectortypes::TVector3f __fastcall GetCenter(void);
	
protected:
	int __fastcall CalcRecursiveLeafCount(void);
	TSectorNode* __fastcall PlaceLeafInChild(TSpacePartitionLeaf* ALeaf);
	System::UnicodeString __fastcall VerifyRecursiveLeafCount(void);
	virtual void __fastcall ChildrenChanged(void);
	
public:
	void __fastcall Clear(void);
	__property Geometrybb::TAABB AABB = {read=FAABB, write=SetAABB};
	__property Geometrybb::TBSphere BSphere = {read=FBSphere};
	__property Vectortypes::TVector3f Center = {read=GetCenter};
	__property bool NoChildren = {read=GetNoChildren, nodefault};
	__property TSectorNodeArray Children = {read=FChildren};
	__property int ChildCount = {read=FChildCount, nodefault};
	virtual TSectorNode* __fastcall GetChildForAABB(const Geometrybb::TAABB &AABB);
	__property TSpacePartitionLeafList* Leaves = {read=FLeaves};
	__property TSectoredSpacePartition* SectoredSpacePartition = {read=FSectoredSpacePartition};
	__property TSectorNode* Parent = {read=FParent};
	__property int RecursiveLeafCount = {read=FRecursiveLeafCount, nodefault};
	__property int NodeDepth = {read=FNodeDepth, nodefault};
	virtual bool __fastcall AABBFitsInNode(const Geometrybb::TAABB &AAABB);
	virtual bool __fastcall AABBIntersectsNode(const Geometrybb::TAABB &AAABB);
	virtual bool __fastcall BSphereFitsInNode(const Geometrybb::TBSphere &BSphere);
	virtual bool __fastcall BSphereIntersectsNode(const Geometrybb::TBSphere &BSphere);
	virtual Geometrybb::TSpaceContains __fastcall AABBContainsSector(const Geometrybb::TAABB &AABB);
	virtual Geometrybb::TSpaceContains __fastcall BSphereContainsSector(const Geometrybb::TBSphere &BSphere);
	virtual Geometrybb::TSpaceContains __fastcall ContainsBSphere(const Geometrybb::TBSphere &ABSphere);
	virtual Geometrybb::TSpaceContains __fastcall ContainsAABB(const Geometrybb::TAABB &AAABB);
	TSectorNode* __fastcall AddLeaf(TSpacePartitionLeaf* ALeaf);
	bool __fastcall RemoveLeaf(TSpacePartitionLeaf* ALeaf, bool OwnerByThis);
	void __fastcall QueryAABB(const Geometrybb::TAABB &AAABB, TSpacePartitionLeafList* const QueryResult);
	void __fastcall QueryBSphere(const Geometrybb::TBSphere &ABSphere, TSpacePartitionLeafList* const QueryResult);
	void __fastcall QueryPlane(const Vectortypes::TVector3f &Location, const Vectortypes::TVector3f &Normal, TSpacePartitionLeafList* const QueryResult);
	void __fastcall QueryFrustum(const Vectorgeometry::TFrustum &Frustum, TSpacePartitionLeafList* const QueryResult);
	void __fastcall QueryFrustumEx(const TExtendedFrustum &ExtendedFrustum, TSpacePartitionLeafList* const QueryResult);
	void __fastcall AddAllLeavesRecursive(TSpacePartitionLeafList* const QueryResult);
	void __fastcall ExpandNode(void);
	virtual void __fastcall CreateChildren(void);
	void __fastcall CollapseNode(void);
	int __fastcall GetNodeCount(void);
	__fastcall TSectorNode(TSectoredSpacePartition* ASectoredSpacePartition, TSectorNode* AParent);
	__fastcall virtual ~TSectorNode(void);
};

#pragma pack(pop)

enum TGrowMethod : unsigned char { GmNever, GmBestFit, GmIncreaseToFitAll };

#pragma pack(push,4)
class PASCALIMPLEMENTATION TSectoredSpacePartition : public TLeavedSpacePartition
{
	typedef TLeavedSpacePartition inherited;
	
private:
	TSectorNode* FRootNode;
	int FLeafThreshold;
	int FMaxTreeDepth;
	float FGrowGravy;
	TGrowMethod FGrowMethod;
	void __fastcall SetLeafThreshold(const int Value);
	void __fastcall SetMaxTreeDepth(const int Value);
	
protected:
	int FQueryNodeTests;
	virtual void __fastcall FlushQueryResult(void);
	
public:
	virtual void __fastcall AddLeaf(TSpacePartitionLeaf* ALeaf);
	virtual void __fastcall RemoveLeaf(TSpacePartitionLeaf* ALeaf);
	virtual void __fastcall LeafChanged(TSpacePartitionLeaf* ALeaf);
	virtual int __fastcall QueryAABB(const Geometrybb::TAABB &AAABB);
	virtual int __fastcall QueryBSphere(const Geometrybb::TBSphere &ABSphere);
	virtual int __fastcall QueryLeaf(TSpacePartitionLeaf* const ALeaf);
	virtual int __fastcall QueryPlane(const Vectortypes::TVector3f &Location, const Vectortypes::TVector3f &Normal);
	virtual int __fastcall QueryFrustum(const Vectorgeometry::TFrustum &Frustum);
	virtual int __fastcall QueryFrustumEx(const TExtendedFrustum &ExtendedFrustum);
	__property int QueryNodeTests = {read=FQueryNodeTests, nodefault};
	int __fastcall GetNodeCount(void);
	void __fastcall UpdateStructureSize(float Gravy);
	void __fastcall RebuildTree(const Geometrybb::TAABB &NewAABB);
	Geometrybb::TAABB __fastcall GetAABB(void);
	virtual TSectorNode* __fastcall CreateNewNode(TSectorNode* AParent);
	virtual void __fastcall Clear(void);
	__fastcall virtual TSectoredSpacePartition(void);
	__fastcall virtual ~TSectoredSpacePartition(void);
	
__published:
	__property TSectorNode* RootNode = {read=FRootNode};
	__property int MaxTreeDepth = {read=FMaxTreeDepth, write=SetMaxTreeDepth, nodefault};
	__property int LeafThreshold = {read=FLeafThreshold, write=SetLeafThreshold, nodefault};
	__property TGrowMethod GrowMethod = {read=FGrowMethod, write=FGrowMethod, nodefault};
	__property float GrowGravy = {read=FGrowGravy, write=FGrowGravy};
public:
	/* TPersistentObject.CreateFromFiler */ inline __fastcall TSectoredSpacePartition(Persistentclasses::TVirtualReader* reader) : TLeavedSpacePartition(reader) { }
	
};

#pragma pack(pop)

class DELPHICLASS TSPOctreeNode;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TSPOctreeNode : public TSectorNode
{
	typedef TSectorNode inherited;
	
public:
	virtual void __fastcall CreateChildren(void);
	virtual bool __fastcall AABBFitsInNode(const Geometrybb::TAABB &AAABB);
	virtual bool __fastcall AABBIntersectsNode(const Geometrybb::TAABB &AAABB);
	virtual bool __fastcall BSphereFitsInNode(const Geometrybb::TBSphere &BSphere);
	virtual bool __fastcall BSphereIntersectsNode(const Geometrybb::TBSphere &BSphere);
public:
	/* TSectorNode.Create */ inline __fastcall TSPOctreeNode(TSectoredSpacePartition* ASectoredSpacePartition, TSectorNode* AParent) : TSectorNode(ASectoredSpacePartition, AParent) { }
	/* TSectorNode.Destroy */ inline __fastcall virtual ~TSPOctreeNode(void) { }
	
};

#pragma pack(pop)

class DELPHICLASS TOctreeSpacePartition;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TOctreeSpacePartition : public TSectoredSpacePartition
{
	typedef TSectoredSpacePartition inherited;
	
public:
	void __fastcall SetSize(const Vectortypes::TVector3f &Min, const Vectortypes::TVector3f &Max);
	virtual TSectorNode* __fastcall CreateNewNode(TSectorNode* AParent);
public:
	/* TSectoredSpacePartition.Create */ inline __fastcall virtual TOctreeSpacePartition(void) : TSectoredSpacePartition() { }
	/* TSectoredSpacePartition.Destroy */ inline __fastcall virtual ~TOctreeSpacePartition(void) { }
	
public:
	/* TPersistentObject.CreateFromFiler */ inline __fastcall TOctreeSpacePartition(Persistentclasses::TVirtualReader* reader) : TSectoredSpacePartition(reader) { }
	
};

#pragma pack(pop)

class DELPHICLASS TSPQuadtreeNode;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TSPQuadtreeNode : public TSPOctreeNode
{
	typedef TSPOctreeNode inherited;
	
protected:
	virtual void __fastcall ChildrenChanged(void);
	
public:
	virtual void __fastcall CreateChildren(void);
	virtual bool __fastcall AABBFitsInNode(const Geometrybb::TAABB &AAABB);
	virtual bool __fastcall AABBIntersectsNode(const Geometrybb::TAABB &AAABB);
	virtual bool __fastcall BSphereFitsInNode(const Geometrybb::TBSphere &BSphere);
	virtual bool __fastcall BSphereIntersectsNode(const Geometrybb::TBSphere &BSphere);
	virtual TSectorNode* __fastcall GetChildForAABB(const Geometrybb::TAABB &AABB);
public:
	/* TSectorNode.Create */ inline __fastcall TSPQuadtreeNode(TSectoredSpacePartition* ASectoredSpacePartition, TSectorNode* AParent) : TSPOctreeNode(ASectoredSpacePartition, AParent) { }
	/* TSectorNode.Destroy */ inline __fastcall virtual ~TSPQuadtreeNode(void) { }
	
};

#pragma pack(pop)

class DELPHICLASS TQuadtreeSpacePartition;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TQuadtreeSpacePartition : public TSectoredSpacePartition
{
	typedef TSectoredSpacePartition inherited;
	
public:
	void __fastcall SetSize(const Vectortypes::TVector3f &Min, const Vectortypes::TVector3f &Max);
	virtual TSectorNode* __fastcall CreateNewNode(TSectorNode* AParent);
public:
	/* TSectoredSpacePartition.Create */ inline __fastcall virtual TQuadtreeSpacePartition(void) : TSectoredSpacePartition() { }
	/* TSectoredSpacePartition.Destroy */ inline __fastcall virtual ~TQuadtreeSpacePartition(void) { }
	
public:
	/* TPersistentObject.CreateFromFiler */ inline __fastcall TQuadtreeSpacePartition(Persistentclasses::TVirtualReader* reader) : TSectoredSpacePartition(reader) { }
	
};

#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------
static const System::Int8 COctree_LEAF_TRHESHOLD = System::Int8(0x1e);
static const System::Int8 COctree_MAX_TREE_DEPTH = System::Int8(0x8);
#define COctree_GROW_GRAVY  (1.000000E-01)
extern PACKAGE Geometrybb::TSpaceContains __fastcall ConeContainsBSphere(const TSPCone &Cone, const Geometrybb::TBSphere &BSphere);
extern PACKAGE bool __fastcall ExtendedFrustumIntersectsBSphere(const TExtendedFrustum &AExtendedFrustum, const Geometrybb::TBSphere &ABSphere);
extern PACKAGE TExtendedFrustum __fastcall ExtendedFrustumMake(const Vectorgeometry::TFrustum &AFrustum, const float ANearDist, const float AFarDist, const float AFieldOfViewRadians, const Vectortypes::TVector3f &ACameraPosition, const Vectortypes::TVector3f &ALookVector);
}	/* namespace Spatialpartitioning */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_SPATIALPARTITIONING)
using namespace Spatialpartitioning;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// SpatialpartitioningHPP
