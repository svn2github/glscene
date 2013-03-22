// CodeGear C++Builder
// Copyright (c) 1995, 2012 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'Octree.pas' rev: 24.00 (Win32)

#ifndef OctreeHPP
#define OctreeHPP

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
#include <VectorLists.hpp>	// Pascal unit
#include <GeometryBB.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Octree
{
//-- type declarations -------------------------------------------------------
typedef void __fastcall (__closure *TProcInt)(int I);

typedef void __fastcall (__closure *TProcAffineAffineAffine)(Vectortypes::TVector3f &V1, Vectortypes::TVector3f &V2, Vectortypes::TVector3f &V3);

struct DECLSPEC_DRECORD TOctreeTriangleInfo
{
public:
	int Index;
	System::StaticArray<Vectortypes::TVector3f, 3> Vertex;
};


typedef TOctreeTriangleInfo *POctreeTriangleInfo;

struct TOctreeNode;
typedef TOctreeNode *POctreeNode;

struct DECLSPEC_DRECORD TOctreeNode
{
private:
	typedef System::DynamicArray<int> _TOctreeNode__1;
	
	
public:
	Vectortypes::TVector3f MinExtent;
	Vectortypes::TVector3f MaxExtent;
	_TOctreeNode__1 TriArray;
	System::StaticArray<POctreeNode, 8> ChildArray;
};


class DELPHICLASS TOctree;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TOctree : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	typedef System::DynamicArray<POctreeNode> _TOctree__1;
	
	
private:
	int Intersections;
	
protected:
	float __fastcall GetMidPoint(float Min, float Max);
	bool __fastcall PointInNode(const Vectortypes::TVector3f &Min, const Vectortypes::TVector3f &Max, const Vectortypes::TVector3f &APoint);
	bool __fastcall TriIntersectNode(const Vectortypes::TVector3f &MinExtent, const Vectortypes::TVector3f &MaxExtent, const Vectortypes::TVector3f &V1, const Vectortypes::TVector3f &V2, const Vectortypes::TVector3f &V3);
	bool __fastcall SphereInNode(const Vectortypes::TVector3f &MinExtent, const Vectortypes::TVector3f &MaxExtent, const Vectortypes::TVector4f &C, float Radius);
	void __fastcall WalkTriToLeafx(POctreeNode Onode, const Vectortypes::TVector3f &V1, const Vectortypes::TVector3f &V2, const Vectortypes::TVector3f &V3);
	void __fastcall WalkPointToLeafx(POctreeNode ONode, const Vectortypes::TVector3f &P);
	void __fastcall WalkSphereToLeafx(POctreeNode Onode, const Vectortypes::TVector4f &P, float Radius);
	void __fastcall WalkRayToLeafx(POctreeNode Onode, const Vectortypes::TVector4f &P, const Vectortypes::TVector4f &V);
	Vectortypes::TVector3f __fastcall GetExtent(System::Byte const *Flags, const int Flags_Size, POctreeNode ParentNode);
	void __fastcall Refine(POctreeNode ParentNode, int Level);
	void __fastcall WalkPointToLeaf(POctreeNode ONode, const Vectortypes::TVector3f &P);
	void __fastcall WalkTriToLeaf(POctreeNode Onode, const Vectortypes::TVector3f &V1, const Vectortypes::TVector3f &V2, const Vectortypes::TVector3f &V3);
	void __fastcall WalkRayToLeaf(POctreeNode Onode, const Vectortypes::TVector4f &P, const Vectortypes::TVector4f &V);
	void __fastcall ConvertR4(POctreeNode ONode, const Vectortypes::TVector3f &Scale);
	void __fastcall CreateTree(int Depth);
	void __fastcall CutMesh(void);
	
public:
	Vectortypes::TVector3f WorldMinExtent;
	Vectortypes::TVector3f WorldMaxExtent;
	TOctreeNode *RootNode;
	int MaxOlevel;
	int NodeCount;
	int TriCountMesh;
	int TriCountOctree;
	int MeshCount;
	_TOctree__1 ResultArray;
	Vectorlists::TAffineVectorList* TriangleFiler;
	void __fastcall WalkSphereToLeaf(POctreeNode Onode, const Vectortypes::TVector4f &P, float Radius);
	void __fastcall InitializeTree(const Vectortypes::TVector3f &AWorldMinExtent, const Vectortypes::TVector3f &AWorldMaxExtent, Vectorlists::TAffineVectorList* const ATriangles, const int ATreeDepth);
	void __fastcall DisposeTree(void);
	__fastcall virtual ~TOctree(void);
	bool __fastcall RayCastIntersect(const Vectortypes::TVector4f &RayStart, const Vectortypes::TVector4f &RayVector, Vectorgeometry::PVector IntersectPoint = (Vectorgeometry::PVector)(0x0), Vectorgeometry::PVector IntersectNormal = (Vectorgeometry::PVector)(0x0), POctreeTriangleInfo TriangleInfo = (POctreeTriangleInfo)(0x0));
	bool __fastcall SphereSweepIntersect(const Vectortypes::TVector4f &RayStart, const Vectortypes::TVector4f &RayVector, const float Velocity, const float Radius, Vectorgeometry::PVector IntersectPoint = (Vectorgeometry::PVector)(0x0), Vectorgeometry::PVector IntersectNormal = (Vectorgeometry::PVector)(0x0));
	bool __fastcall TriangleIntersect(const Vectortypes::TVector3f &V1, const Vectortypes::TVector3f &V2, const Vectortypes::TVector3f &V3);
	Vectorlists::TAffineVectorList* __fastcall GetTrianglesFromNodesIntersectingAABB(const Geometrybb::TAABB &ObjAABB);
	Vectorlists::TAffineVectorList* __fastcall GetTrianglesFromNodesIntersectingCube(const Geometrybb::TAABB &ObjAABB, const Vectortypes::TMatrix4f &ObjToSelf, const Vectortypes::TMatrix4f &SelfToObj);
	bool __fastcall AABBIntersect(const Geometrybb::TAABB &AABB, const Vectortypes::TMatrix4f &M1to2, const Vectortypes::TMatrix4f &M2to1, Vectorlists::TAffineVectorList* Triangles = (Vectorlists::TAffineVectorList*)(0x0));
public:
	/* TObject.Create */ inline __fastcall TOctree(void) : System::TObject() { }
	
};

#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------
}	/* namespace Octree */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_OCTREE)
using namespace Octree;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// OctreeHPP
