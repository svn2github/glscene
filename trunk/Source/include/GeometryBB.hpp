// CodeGear C++Builder
// Copyright (c) 1995, 2012 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GeometryBB.pas' rev: 24.00 (Win32)

#ifndef GeometrybbHPP
#define GeometrybbHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <VectorGeometry.hpp>	// Pascal unit
#include <VectorLists.hpp>	// Pascal unit
#include <VectorTypes.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Geometrybb
{
//-- type declarations -------------------------------------------------------
struct THmgBoundingBox;
typedef THmgBoundingBox *PHmgBoundingBox;

struct DECLSPEC_DRECORD THmgBoundingBox
{
public:
	System::StaticArray<Vectortypes::TVector4f, 8> BBox;
};


struct DECLSPEC_DRECORD TAABB
{
public:
	Vectortypes::TVector3f Min;
	Vectortypes::TVector3f Max;
};


typedef TAABB *PAABB;

struct DECLSPEC_DRECORD TBSphere
{
public:
	Vectortypes::TVector3f Center;
	float Radius;
};


struct DECLSPEC_DRECORD TClipRect
{
public:
	float Left;
	float Top;
	float Right;
	float Bottom;
};


enum TSpaceContains : unsigned char { ScNoOverlap, ScContainsFully, ScContainsPartially };

typedef System::StaticArray<Vectortypes::TVector3f, 8> TAABBCorners;

typedef System::StaticArray<int, 4> TPlanIndices;

typedef System::StaticArray<System::StaticArray<int, 4>, 6> TPlanBB;

typedef System::StaticArray<int, 6> TDirPlan;

//-- var, const, procedure ---------------------------------------------------
extern PACKAGE THmgBoundingBox NullBoundingBox;
extern PACKAGE TPlanIndices CBBFront;
extern PACKAGE TPlanIndices CBBBack;
extern PACKAGE TPlanIndices CBBLeft;
extern PACKAGE TPlanIndices CBBRight;
extern PACKAGE TPlanIndices CBBTop;
extern PACKAGE TPlanIndices CBBBottom;
extern PACKAGE TPlanBB CBBPlans;
extern PACKAGE TDirPlan CDirPlan;
extern PACKAGE bool __fastcall BoundingBoxesAreEqual(const THmgBoundingBox &ABoundingBox1, const THmgBoundingBox &ABoundingBox2)/* overload */;
extern PACKAGE bool __fastcall BoundingBoxesAreEqual(const PHmgBoundingBox ABoundingBox1, const PHmgBoundingBox ABoundingBox2)/* overload */;
extern PACKAGE THmgBoundingBox __fastcall AddBB(THmgBoundingBox &C1, const THmgBoundingBox &C2);
extern PACKAGE void __fastcall AddAABB(TAABB &Aabb, const TAABB &Aabb1);
extern PACKAGE void __fastcall SetBB(THmgBoundingBox &C, const Vectortypes::TVector4f &V);
extern PACKAGE void __fastcall SetAABB(TAABB &Bb, const Vectortypes::TVector4f &V);
extern PACKAGE void __fastcall BBTransform(THmgBoundingBox &C, const Vectortypes::TMatrix4f &M);
extern PACKAGE void __fastcall AABBTransform(TAABB &Bb, const Vectortypes::TMatrix4f &M);
extern PACKAGE void __fastcall AABBScale(TAABB &Bb, const Vectortypes::TVector3f &V);
extern PACKAGE float __fastcall BBMinX(const THmgBoundingBox &C);
extern PACKAGE float __fastcall BBMaxX(const THmgBoundingBox &C);
extern PACKAGE float __fastcall BBMinY(const THmgBoundingBox &C);
extern PACKAGE float __fastcall BBMaxY(const THmgBoundingBox &C);
extern PACKAGE float __fastcall BBMinZ(const THmgBoundingBox &C);
extern PACKAGE float __fastcall BBMaxZ(const THmgBoundingBox &C);
extern PACKAGE void __fastcall AABBInclude(TAABB &Bb, const Vectortypes::TVector3f &P);
extern PACKAGE void __fastcall AABBFromSweep(TAABB &SweepAABB, const Vectortypes::TVector4f &Start, const Vectortypes::TVector4f &Dest, const float Radius);
extern PACKAGE TAABB __fastcall AABBIntersection(const TAABB &Aabb1, const TAABB &Aabb2);
extern PACKAGE TAABB __fastcall BBToAABB(const THmgBoundingBox &ABB);
extern PACKAGE THmgBoundingBox __fastcall AABBToBB(const TAABB &AnAABB)/* overload */;
extern PACKAGE THmgBoundingBox __fastcall AABBToBB(const TAABB &AnAABB, const Vectortypes::TMatrix4f &M)/* overload */;
extern PACKAGE void __fastcall OffsetAABB(TAABB &Aabb, const Vectortypes::TVector3f &Delta)/* overload */;
extern PACKAGE void __fastcall OffsetAABB(TAABB &Aabb, const Vectortypes::TVector4f &Delta)/* overload */;
extern PACKAGE void __fastcall OffsetBB(THmgBoundingBox &Bb, const Vectortypes::TVector3f &Delta)/* overload */;
extern PACKAGE void __fastcall OffsetBB(THmgBoundingBox &Bb, const Vectortypes::TVector4f &Delta)/* overload */;
extern PACKAGE void __fastcall OffsetBBPoint(THmgBoundingBox &Bb, const Vectortypes::TVector4f &Delta)/* overload */;
extern PACKAGE bool __fastcall IntersectAABBs(const TAABB &Aabb1, const TAABB &Aabb2, const Vectortypes::TMatrix4f &M1To2, const Vectortypes::TMatrix4f &M2To1)/* overload */;
extern PACKAGE bool __fastcall IntersectAABBsAbsoluteXY(const TAABB &Aabb1, const TAABB &Aabb2);
extern PACKAGE bool __fastcall IntersectAABBsAbsoluteXZ(const TAABB &Aabb1, const TAABB &Aabb2);
extern PACKAGE bool __fastcall IntersectAABBsAbsolute(const TAABB &Aabb1, const TAABB &Aabb2);
extern PACKAGE bool __fastcall AABBFitsInAABBAbsolute(const TAABB &Aabb1, const TAABB &Aabb2);
extern PACKAGE bool __fastcall PointInAABB(const Vectortypes::TVector3f &P, const TAABB &Aabb)/* overload */;
extern PACKAGE bool __fastcall PointInAABB(const Vectortypes::TVector4f &P, const TAABB &Aabb)/* overload */;
extern PACKAGE bool __fastcall PlaneIntersectAABB(const Vectortypes::TVector3f &Normal, float D, const TAABB &Aabb);
extern PACKAGE Vectorlists::TAffineVectorList* __fastcall PlaneAABBIntersection(const Vectortypes::TVector4f &plane, const TAABB &AABB);
extern PACKAGE bool __fastcall TriangleIntersectAABB(const TAABB &Aabb, const Vectortypes::TVector3f &V1, const Vectortypes::TVector3f &V2, const Vectortypes::TVector3f &V3);
extern PACKAGE void __fastcall ExtractAABBCorners(const TAABB &AABB, Vectortypes::TVector3f *AABBCorners);
extern PACKAGE void __fastcall AABBToBSphere(const TAABB &AABB, TBSphere &BSphere);
extern PACKAGE void __fastcall BSphereToAABB(const TBSphere &BSphere, TAABB &AABB)/* overload */;
extern PACKAGE TAABB __fastcall BSphereToAABB(const Vectortypes::TVector3f &Center, float Radius)/* overload */;
extern PACKAGE TAABB __fastcall BSphereToAABB(const Vectortypes::TVector4f &Center, float Radius)/* overload */;
extern PACKAGE TSpaceContains __fastcall AABBContainsAABB(const TAABB &MainAABB, const TAABB &TestAABB);
extern PACKAGE TSpaceContains __fastcall AABBContainsBSphere(const TAABB &MainAABB, const TBSphere &TestBSphere);
extern PACKAGE TSpaceContains __fastcall PlaneContainsBSphere(const Vectortypes::TVector3f &Location, const Vectortypes::TVector3f &Normal, const TBSphere &TestBSphere);
extern PACKAGE TSpaceContains __fastcall FrustumContainsBSphere(const Vectorgeometry::TFrustum &Frustum, const TBSphere &TestBSphere);
extern PACKAGE TSpaceContains __fastcall FrustumContainsAABB(const Vectorgeometry::TFrustum &Frustum, const TAABB &TestAABB);
extern PACKAGE TSpaceContains __fastcall BSphereContainsAABB(const TBSphere &MainBSphere, const TAABB &TestAABB);
extern PACKAGE TSpaceContains __fastcall BSphereContainsBSphere(const TBSphere &MainBSphere, const TBSphere &TestBSphere);
extern PACKAGE bool __fastcall BSphereIntersectsBSphere(const TBSphere &MainBSphere, const TBSphere &TestBSphere);
extern PACKAGE Vectortypes::TVector3f __fastcall ClipToAABB(const Vectortypes::TVector3f &V, const TAABB &AABB);
extern PACKAGE void __fastcall IncludeInClipRect(TClipRect &ClipRect, float X, float Y);
extern PACKAGE TClipRect __fastcall AABBToClipRect(const TAABB &Aabb, const Vectortypes::TMatrix4f &ModelViewProjection, int ViewportSizeX, int ViewportSizeY);
extern PACKAGE bool __fastcall RayCastAABBIntersect(const Vectortypes::TVector4f &RayOrigin, const Vectortypes::TVector4f &RayDirection, const TAABB &Aabb, /* out */ float &TNear, /* out */ float &TFar)/* overload */;
extern PACKAGE bool __fastcall RayCastAABBIntersect(const Vectortypes::TVector4f &RayOrigin, const Vectortypes::TVector4f &RayDirection, const TAABB &Aabb, Vectorgeometry::PVector IntersectPoint = (Vectorgeometry::PVector)(0x0))/* overload */;
}	/* namespace Geometrybb */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GEOMETRYBB)
using namespace Geometrybb;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// GeometrybbHPP
