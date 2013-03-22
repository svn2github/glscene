// CodeGear C++Builder
// Copyright (c) 1995, 2012 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLEllipseCollision.pas' rev: 24.00 (Win32)

#ifndef GlellipsecollisionHPP
#define GlellipsecollisionHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <VectorGeometry.hpp>	// Pascal unit
#include <Octree.hpp>	// Pascal unit
#include <VectorLists.hpp>	// Pascal unit
#include <VectorTypes.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Glellipsecollision
{
//-- type declarations -------------------------------------------------------
class DELPHICLASS TECPlane;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TECPlane : public System::TObject
{
	typedef System::TObject inherited;
	
public:
	System::StaticArray<float, 4> Equation;
	Vectortypes::TVector3f Origin;
	Vectortypes::TVector3f Normal;
	void __fastcall MakePlane(const Vectortypes::TVector3f &nOrigin, const Vectortypes::TVector3f &nNormal)/* overload */;
	void __fastcall MakePlane(const Vectortypes::TVector3f &p1, const Vectortypes::TVector3f &p2, const Vectortypes::TVector3f &p3)/* overload */;
	bool __fastcall isFrontFacingTo(const Vectortypes::TVector3f &Direction);
	float __fastcall signedDistanceTo(const Vectortypes::TVector3f &Point);
public:
	/* TObject.Create */ inline __fastcall TECPlane(void) : System::TObject() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TECPlane(void) { }
	
};

#pragma pack(pop)

struct DECLSPEC_DRECORD TECObjectInfo
{
public:
	Vectortypes::TMatrix4f AbsoluteMatrix;
	bool Solid;
	bool IsDynamic;
	int ObjectID;
};


struct DECLSPEC_DRECORD TECTriangle
{
public:
	Vectortypes::TVector3f p1;
	Vectortypes::TVector3f p2;
	Vectortypes::TVector3f p3;
};


struct DECLSPEC_DRECORD TECTriMesh
{
private:
	typedef System::DynamicArray<TECTriangle> _TECTriMesh__1;
	
	
public:
	_TECTriMesh__1 Triangles;
	TECObjectInfo ObjectInfo;
};


typedef System::DynamicArray<TECTriMesh> TECTriMeshList;

struct DECLSPEC_DRECORD TECFreeForm
{
private:
	typedef System::DynamicArray<Octree::POctreeNode> _TECFreeForm__1;
	
	
public:
	_TECFreeForm__1 OctreeNodes;
	Vectorlists::TAffineVectorList* *triangleFiler;
	bool InvertedNormals;
	TECObjectInfo ObjectInfo;
};


typedef System::DynamicArray<TECFreeForm> TECFreeFormList;

enum TECColliderShape : unsigned char { csEllipsoid, csPoint };

struct DECLSPEC_DRECORD TECCollider
{
public:
	Vectortypes::TVector3f Position;
	Vectortypes::TVector3f Radius;
	TECColliderShape Shape;
	TECObjectInfo ObjectInfo;
};


typedef System::DynamicArray<TECCollider> TECColliderList;

struct DECLSPEC_DRECORD TECContact
{
public:
	Vectortypes::TVector3f Position;
	Vectortypes::TVector3f SurfaceNormal;
	float Distance;
	TECObjectInfo ObjectInfo;
};


typedef System::DynamicArray<TECContact> TECContactList;

struct DECLSPEC_DRECORD TECCollisionPacket
{
public:
	Vectortypes::TVector3f velocity;
	Vectortypes::TVector3f normalizedVelocity;
	Vectortypes::TVector3f basePoint;
	bool foundCollision;
	float nearestDistance;
	int NearestObject;
	Vectortypes::TVector3f intersectionPoint;
	Vectortypes::TVector3f intersectionNormal;
};


struct DECLSPEC_DRECORD TECMovePack
{
public:
	TECTriMeshList TriMeshes;
	TECFreeFormList Freeforms;
	TECColliderList Colliders;
	Vectortypes::TVector3f Position;
	Vectortypes::TVector3f Velocity;
	Vectortypes::TVector3f Gravity;
	Vectortypes::TVector3f Radius;
	TECObjectInfo ObjectInfo;
	float CollisionRange;
	double UnitScale;
	System::Byte MaxRecursionDepth;
	TECCollisionPacket CP;
	System::Byte collisionRecursionDepth;
	Vectortypes::TVector3f ResultPos;
	int NearestObject;
	bool VelocityCollided;
	bool GravityCollided;
	Vectortypes::TVector3f GroundNormal;
	TECContactList Contacts;
};


typedef System::DynamicArray<TECTriangle> Glellipsecollision__2;

//-- var, const, procedure ---------------------------------------------------
extern PACKAGE float cECCloseDistance;
extern PACKAGE Glellipsecollision__2 debug_tri;
extern PACKAGE Vectortypes::TVector3f __fastcall VectorDivide(const Vectortypes::TVector3f &v, const Vectortypes::TVector3f &divider);
extern PACKAGE void __fastcall CollideAndSlide(TECMovePack &MP);
extern PACKAGE void __fastcall CollideWithWorld(TECMovePack &MP, const Vectortypes::TVector3f &pos, const Vectortypes::TVector3f &vel, bool &HasCollided);
}	/* namespace Glellipsecollision */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLELLIPSECOLLISION)
using namespace Glellipsecollision;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// GlellipsecollisionHPP
