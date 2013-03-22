// CodeGear C++Builder
// Copyright (c) 1995, 2012 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'NewtonImport.pas' rev: 24.00 (Win32)

#ifndef NewtonimportHPP
#define NewtonimportHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <System.Classes.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Newtonimport
{
//-- type declarations -------------------------------------------------------
typedef float Float;

typedef System::Extended Long_double;

typedef int Int;

typedef System::Int8 __Int8;

typedef short __Int16;

typedef int __Int32;

typedef __int64 __Int64;

typedef System::Int8 NChar;

typedef System::Byte Unsigned_char;

typedef short Short;

typedef System::Word Unsigned_short;

typedef int Long;

typedef unsigned Unsigned_long;

typedef unsigned Unsigned_int;

typedef unsigned size_t;

typedef System::StaticArray<System::WideChar, 256> CharArray;

typedef int *PInt;

typedef System::Int8 *P__int8;

typedef short *P__int16;

typedef int *P__int32;

typedef __int64 *P__int64;

typedef System::Int8 *P2Char;

typedef System::Byte *PUnsigned_char;

typedef short *PShort;

typedef System::Word *PUnsigned_short;

typedef int *PLong;

typedef unsigned *PUnsigned_long;

typedef unsigned *PUnsigned_int;

typedef unsigned *Psize_t;

typedef float *PFloat;

typedef System::Extended *PLong_double;

typedef CharArray *PCharArray;

typedef bool Bool;

typedef void * Pvoid;

typedef bool *PBool;

typedef void * *PNewtonMesh;

typedef void * *PNewtonBody;

typedef void * *PNewtonWorld;

typedef void * *PNewtonJoint;

typedef void * *PNewtonContact;

typedef void * *PNewtonMaterial;

typedef void * *PNewtonCollision;

typedef void * *PNewtonSceneProxy;

typedef void * *PNewtonBreakableComponentMesh;

typedef void * *PNewtonMeshVertex;

typedef void * *PNewtonMeshEdge;

typedef void * *PNewtonMeshFace;

typedef void * *PNewtonMeshPoint;

typedef void * *PNewtonUserJoint;

#pragma pack(push,1)
struct DECLSPEC_DRECORD TNewtonBoxParam
{
public:
	float m_x;
	float m_y;
	float m_z;
};
#pragma pack(pop)


#pragma pack(push,1)
struct DECLSPEC_DRECORD TNewtonSphereParam
{
public:
	float m_r0;
	float m_r1;
	float m_r2;
};
#pragma pack(pop)


#pragma pack(push,1)
struct DECLSPEC_DRECORD TNewtonCylinderParam
{
public:
	float m_r0;
	float m_r1;
	float m_height;
};
#pragma pack(pop)


#pragma pack(push,1)
struct DECLSPEC_DRECORD TNewtonCapsuleParam
{
public:
	float m_r0;
	float m_r1;
	float m_height;
};
#pragma pack(pop)


#pragma pack(push,1)
struct DECLSPEC_DRECORD TNewtonConeParam
{
public:
	float m_r;
	float m_height;
};
#pragma pack(pop)


#pragma pack(push,1)
struct DECLSPEC_DRECORD TNewtonChamferCylinderParam
{
public:
	float m_r;
	float m_height;
};
#pragma pack(pop)


#pragma pack(push,1)
struct DECLSPEC_DRECORD TNewtonConvexHullParam
{
public:
	int m_vertexCount;
	int m_vertexStrideInBytes;
	int m_faceCount;
	float *m_vertex;
};
#pragma pack(pop)


#pragma pack(push,1)
struct DECLSPEC_DRECORD TNewtonConvexHullModifierParam
{
public:
	void * *m_chidren;
};
#pragma pack(pop)


#pragma pack(push,1)
struct DECLSPEC_DRECORD TNewtonCompoundCollisionParam
{
public:
	int m_chidrenCount;
	void *m_chidren;
};
#pragma pack(pop)


#pragma pack(push,1)
struct DECLSPEC_DRECORD TNewtonCollisionTreeParam
{
public:
	int m_vertexCount;
	int m_indexCount;
};
#pragma pack(pop)


#pragma pack(push,1)
struct DECLSPEC_DRECORD TNewtonHeightFieldCollisionParam
{
public:
	int m_width;
	int m_height;
	int m_gridsDiagonals;
	float m_horizonalScale;
	float m_verticalScale;
	void *m_elevation;
	System::WideChar *m_atributes;
};
#pragma pack(pop)


#pragma pack(push,1)
struct DECLSPEC_DRECORD TNewtonSceneCollisionParam
{
public:
	int m_childrenProxyCount;
};
#pragma pack(pop)


#pragma pack(push,1)
struct DECLSPEC_DRECORD TNewtonCollisionNullParam
{
};
#pragma pack(pop)


#pragma pack(push,1)
struct DECLSPEC_DRECORD TNewtonCollisionInfoRecord
{
public:
	System::StaticArray<System::StaticArray<float, 4>, 4> m_offsetMatrix;
	int m_collisionType;
	int m_referenceCount;
	int m_collisionUserID;
	union
	{
		struct 
		{
			TNewtonSceneCollisionParam shapedatascenecollision;
		};
		struct 
		{
			System::StaticArray<float, 64> m_paramArray;
		};
		struct 
		{
			TNewtonHeightFieldCollisionParam shapedataheightfield;
		};
		struct 
		{
			
		};
		struct 
		{
			TNewtonCollisionTreeParam shapedatatree;
		};
		struct 
		{
			TNewtonChamferCylinderParam shapedatachamfercylinder;
		};
		struct 
		{
			TNewtonConvexHullModifierParam shapedataxonvexhull;
		};
		struct 
		{
			TNewtonConvexHullParam shapedataconvexhull;
		};
		struct 
		{
			TNewtonCompoundCollisionParam shapedatacompound;
		};
		struct 
		{
			TNewtonCylinderParam shapedatacylinder;
		};
		struct 
		{
			TNewtonCapsuleParam shapedatacapsule;
		};
		struct 
		{
			TNewtonSphereParam shapedatasphere;
		};
		struct 
		{
			TNewtonConeParam shapedata;
		};
		struct 
		{
			TNewtonBoxParam shapedatabox;
		};
		
	};
};
#pragma pack(pop)


typedef TNewtonCollisionInfoRecord *PNewtonCollisionInfoRecord;

struct NewtonJointRecord;
typedef NewtonJointRecord *PNewtonJointRecord;

struct DECLSPEC_DRECORD NewtonJointRecord
{
public:
	System::StaticArray<System::StaticArray<float, 4>, 4> m_attachmenMatrix_0;
	System::StaticArray<System::StaticArray<float, 4>, 4> m_attachmenMatrix_1;
	System::StaticArray<float, 3> m_minLinearDof;
	System::StaticArray<float, 3> m_maxLinearDof;
	System::StaticArray<float, 3> m_minAngularDof;
	System::StaticArray<float, 3> m_maxAngularDof;
	void * *m_attachBody_0;
	void * *m_attachBody_1;
	System::StaticArray<float, 16> m_extraParameters;
	int m_bodiesCollisionOn;
	System::StaticArray<System::Int8, 32> m_descriptionType;
};


struct NewtonUserMeshCollisionCollideDesc;
typedef NewtonUserMeshCollisionCollideDesc *PNewtonUserMeshCollisionCollideDesc;

struct DECLSPEC_DRECORD NewtonUserMeshCollisionCollideDesc
{
public:
	System::StaticArray<float, 4> m_boxP0;
	System::StaticArray<float, 4> m_boxP1;
	int m_threadNumber;
	int m_faceCount;
	int m_vertexStrideInBytes;
	void *m_userData;
	float *m_vertex;
	int *m_userAttribute;
	int *m_faceIndexCount;
	int *m_faceVertexIndex;
	void * *m_objBody;
	void * *m_polySoupBody;
};


struct NewtonWorldConvexCastReturnInfo;
typedef NewtonWorldConvexCastReturnInfo *PNewtonWorldConvexCastReturnInfo;

struct DECLSPEC_DRECORD NewtonWorldConvexCastReturnInfo
{
public:
	System::StaticArray<float, 4> m_point;
	System::StaticArray<float, 4> m_normal;
	System::StaticArray<float, 4> m_normalOnHitPoint;
	float m_penetration;
	int m_contactID;
	void * *m_hitBody;
};


struct NewtonUserMeshCollisionRayHitDesc;
typedef NewtonUserMeshCollisionRayHitDesc *PNewtonUserMeshCollisionRayHitDesc;

struct DECLSPEC_DRECORD NewtonUserMeshCollisionRayHitDesc
{
public:
	System::StaticArray<float, 4> m_p0;
	System::StaticArray<float, 4> m_p1;
	System::StaticArray<float, 4> m_normalOut;
	int m_userIdOut;
	void *m_userData;
};


struct NewtonHingeSliderUpdateDesc;
typedef NewtonHingeSliderUpdateDesc *PNewtonHingeSliderUpdateDesc;

struct DECLSPEC_DRECORD NewtonHingeSliderUpdateDesc
{
public:
	float m_accel;
	float m_minFriction;
	float m_maxFriction;
	float m_timestep;
};


typedef void * __cdecl (*NewtonAllocMemory)(int sizeInBytes);

typedef NewtonAllocMemory *PNewtonAllocMemory;

typedef void __cdecl (*NewtonFreeMemory)(void * ptr, int sizeInBytes);

typedef NewtonFreeMemory *PNewtonFreeMemory;

typedef void __cdecl (*NewtonDestroyWorld)(const PNewtonWorld NewtonWorld);

typedef NewtonDestroyWorld *PNewtonDestroyWorld;

typedef unsigned __cdecl (*NewtonGetTicksCountCallback)(void);

typedef NewtonGetTicksCountCallback *PNewtonGetTicksCountCallback;

typedef void __cdecl (*NewtonSerialize)(void * serializeHandle, const void * buffer, unsigned size);

typedef NewtonSerialize *PNewtonSerialize;

typedef void __cdecl (*NewtonDeserialize)(void * serializeHandle, void * buffer, unsigned size);

typedef NewtonDeserialize *PNewtonDeserialize;

typedef void __cdecl (*NewtonUserMeshCollisionDestroyCallback)(void * descData);

typedef NewtonUserMeshCollisionDestroyCallback *PNewtonUserMeshCollisionDestroyCallback;

typedef void __cdecl (*NewtonUserMeshCollisionCollideCallback)(PNewtonUserMeshCollisionCollideDesc NewtonUserMeshCollisionCollideDesc);

typedef NewtonUserMeshCollisionCollideCallback *PNewtonUserMeshCollisionCollideCallback;

typedef int __cdecl (*NewtonUserMeshCollisionRayHitCallback)(PNewtonUserMeshCollisionRayHitDesc NewtonUserMeshCollisionRayHitDesc);

typedef NewtonUserMeshCollisionRayHitCallback *PNewtonUserMeshCollisionRayHitCallback;

typedef void __cdecl (*NewtonUserMeshCollisionGetCollisionInfo)(void * userData, PNewtonCollisionInfoRecord infoRecord);

typedef NewtonUserMeshCollisionGetCollisionInfo *PNewtonUserMeshCollisionGetCollisionInfo;

typedef int __cdecl (*NewtonUserMeshCollisionGetFacesInAABB)(void * userData, const PFloat p0, const PFloat p1, const PFloat vertexArray, PInt vertexCount, PInt vertexStrideInBytes, const PInt indexList, int maxIndexCount, const PInt userDataList);

typedef NewtonUserMeshCollisionGetFacesInAABB *PNewtonUserMeshCollisionGetFacesInAABB;

typedef float __cdecl (*NewtonCollisionTreeRayCastCallback)(const PNewtonBody Body, const PNewtonCollision TreeCollision, float interception, PFloat normal, int faceId, void * usedData);

typedef NewtonCollisionTreeRayCastCallback *PNewtonCollisionTreeRayCastCallback;

typedef float __cdecl (*NewtonHeightFieldRayCastCallback)(const PNewtonBody Body, const PNewtonCollision HeightFieldCollision, float Interception, int Row, int Col, PFloat Normal, int FaceID, void * UsedData);

typedef NewtonHeightFieldRayCastCallback *PNewtonHeightFieldRayCastCallback;

typedef void __cdecl (*NewtonTreeCollisionCallback)(const PNewtonBody bodyWithTreeCollision, const PNewtonBody body, int faceID, const PFloat vertex, int vertexstrideInBytes);

typedef NewtonTreeCollisionCallback *PNewtonTreeCollisionCallback;

typedef void __cdecl (*NewtonBodyDestructor)(const PNewtonBody body);

typedef NewtonBodyDestructor *PNewtonBodyDestructor;

typedef void __cdecl (*NewtonApplyForceAndTorque)(const PNewtonBody body, float timestep, int threadIndex);

typedef NewtonApplyForceAndTorque *PNewtonApplyForceAndTorque;

typedef void __cdecl (*NewtonSetTransform)(const PNewtonBody body, const PFloat matrix, int threadIndex);

typedef NewtonSetTransform *PNewtonSetTransform;

typedef int __cdecl (*NewtonIslandUpdate)(const PNewtonWorld World, void * islandHandle, int bodyCount);

typedef NewtonIslandUpdate *PNewtonIslandUpdate;

typedef void __cdecl (*NewtonBodyLeaveWorld)(const PNewtonBody body, int threadIndex);

typedef NewtonBodyLeaveWorld *PNewtonBodyLeaveWorld;

typedef void __cdecl (*NewtonDestroyBodyByExeciveForce)(const PNewtonBody body, const PNewtonJoint contact);

typedef NewtonDestroyBodyByExeciveForce *PNewtonDestroyBodyByExeciveForce;

typedef void __cdecl (*NewtonCollisionDestructor)(const PNewtonWorld World, const PNewtonCollision collision);

typedef NewtonCollisionDestructor *PNewtonCollisionDestructor;

typedef int __cdecl (*NewtonCollisionCompoundBreakableCallback)(const PNewtonMesh Mesh, void * userData, PFloat planeMatrixOut);

typedef NewtonCollisionCompoundBreakableCallback *PNewtonCollisionCompoundBreakableCallback;

typedef int __cdecl (*NewtonGetBuoyancyPlane)(const int collisionID, void * context, const PFloat globalSpaceMatrix, PFloat globalSpacePlane);

typedef NewtonGetBuoyancyPlane *PNewtonGetBuoyancyPlane;

typedef unsigned __cdecl (*NewtonWorldRayPrefilterCallback)(const PNewtonBody body, const PNewtonCollision collision, void * userData);

typedef NewtonWorldRayPrefilterCallback *PNewtonWorldRayPrefilterCallback;

typedef float __cdecl (*NewtonWorldRayFilterCallback)(const PNewtonBody body, const PFloat hitNormal, int collisionID, void * userData, float intersetParam);

typedef NewtonWorldRayFilterCallback *PNewtonWorldRayFilterCallback;

typedef int __cdecl (*NewtonOnAABBOverlap)(const PNewtonMaterial material, const PNewtonBody body0, const PNewtonBody body1, int threadIndex);

typedef NewtonOnAABBOverlap *PNewtonOnAABBOverlap;

typedef void __cdecl (*NewtonContactsProcess)(const PNewtonJoint contact, float timestep, int threadIndex);

typedef NewtonContactsProcess *PNewtonContactsProcess;

typedef void __cdecl (*NewtonBodyIterator)(const PNewtonBody body, void * userData);

typedef NewtonBodyIterator *PNewtonBodyIterator;

typedef void __cdecl (*NewtonJointIterator)(const PNewtonJoint joint, void * userData);

typedef NewtonJointIterator *PNewtonJointIterator;

typedef void __cdecl (*NewtonCollisionIterator)(void * userData, int vertexCount, const PFloat FaceArray, int faceId);

typedef NewtonCollisionIterator *PNewtonCollisionIterator;

typedef void __cdecl (*NewtonBallCallBack)(const PNewtonJoint ball, float timestep);

typedef NewtonBallCallBack *PNewtonBallCallBack;

typedef unsigned __cdecl (*NewtonHingeCallBack)(const PNewtonJoint hinge, PNewtonHingeSliderUpdateDesc desc);

typedef NewtonHingeCallBack *PNewtonHingeCallBack;

typedef unsigned __cdecl (*NewtonSliderCallBack)(const PNewtonJoint slider, PNewtonHingeSliderUpdateDesc desc);

typedef NewtonSliderCallBack *PNewtonSliderCallBack;

typedef unsigned __cdecl (*NewtonUniversalCallBack)(const PNewtonJoint universal, PNewtonHingeSliderUpdateDesc desc);

typedef NewtonUniversalCallBack *PNewtonUniversalCallBack;

typedef unsigned __cdecl (*NewtonCorkscrewCallBack)(const PNewtonJoint corkscrew, PNewtonHingeSliderUpdateDesc desc);

typedef NewtonCorkscrewCallBack *PNewtonCorkscrewCallBack;

typedef void __cdecl (*NewtonUserBilateralCallBack)(const PNewtonJoint userJoint, float timestep, int threadIndex);

typedef NewtonUserBilateralCallBack *PNewtonUserBilateralCallBack;

typedef void __cdecl (*NewtonUserBilateralGetInfoCallBack)(const PNewtonJoint userJoint, PNewtonJointRecord info);

typedef NewtonUserBilateralGetInfoCallBack *PNewtonUserBilateralGetInfoCallBack;

typedef void __cdecl (*NewtonConstraintDestructor)(const PNewtonJoint me);

typedef NewtonConstraintDestructor *PNewtonConstraintDestructor;

typedef System::DynamicArray<PNewtonCollision> TCollisionPrimitiveArray;

//-- var, const, procedure ---------------------------------------------------
#define NewtonDLL L"Newton.dll"
static const System::Int8 NEWTON_MAJOR_VERSION = System::Int8(0x2);
static const System::Int8 NEWTON_MINOR_VERSION = System::Int8(0x23);
static const System::Int8 NEWTON_PROFILER_WORLD_UPDATE = System::Int8(0x0);
static const System::Int8 NEWTON_PROFILER_COLLISION_UPDATE = System::Int8(0x1);
static const System::Int8 NEWTON_PROFILER_COLLISION_UPDATE_BROAD_PHASE = System::Int8(0x2);
static const System::Int8 NEWTON_PROFILER_COLLISION_UPDATE_NARROW_PHASE = System::Int8(0x3);
static const System::Int8 NEWTON_PROFILER_DYNAMICS_UPDATE = System::Int8(0x4);
static const System::Int8 NEWTON_PROFILER_DYNAMICS_CONSTRAINT_GRAPH = System::Int8(0x5);
static const System::Int8 NEWTON_PROFILER_FORCE_CALLBACK_UPDATE = System::Int8(0x6);
static const System::Int8 NEWTON_PROFILER_DYNAMICS_SOLVE_CONSTRAINT_GRAPH = System::Int8(0x7);
static const System::Int8 SERIALIZE_ID_BOX = System::Int8(0x0);
static const System::Int8 SERIALIZE_ID_CONE = System::Int8(0x1);
static const System::Int8 SERIALIZE_ID_SPHERE = System::Int8(0x2);
static const System::Int8 SERIALIZE_ID_CAPSULE = System::Int8(0x3);
static const System::Int8 SERIALIZE_ID_CYLINDER = System::Int8(0x4);
static const System::Int8 SERIALIZE_ID_COMPOUND = System::Int8(0x5);
static const System::Int8 SERIALIZE_ID_CONVEXHULL = System::Int8(0x6);
static const System::Int8 SERIALIZE_ID_CONVEXMODIFIER = System::Int8(0x7);
static const System::Int8 SERIALIZE_ID_CHAMFERCYLINDER = System::Int8(0x8);
static const System::Int8 SERIALIZE_ID_TREE = System::Int8(0x9);
static const System::Int8 SERIALIZE_ID_NULL = System::Int8(0xa);
static const System::Int8 SERIALIZE_ID_HEIGHTFIELD = System::Int8(0xb);
static const System::Int8 SERIALIZE_ID_USERMESH = System::Int8(0xc);
static const System::Int8 SERIALIZE_ID_SCENE = System::Int8(0xd);
static const System::Int8 SERIALIZE_ID_COMPOUND_BREAKABLE = System::Int8(0xe);
extern "C" int __cdecl NewtonWorldGetVersion(const PNewtonWorld newtonWorld);
extern "C" int __cdecl NewtonWorldFloatSize(const PNewtonWorld newtonWorld);
extern "C" PNewtonWorld __cdecl NewtonCreate(NewtonAllocMemory malloc, NewtonFreeMemory mfree);
extern "C" void __cdecl NewtonDestroy(const PNewtonWorld newtonWorld);
extern "C" void __cdecl NewtonDestroyAllBodies(const PNewtonWorld newtonWorld);
extern "C" int __cdecl NewtonGetMemoryUsed(void);
extern "C" void __cdecl NewtonSetMemorySystem(NewtonAllocMemory malloc, NewtonFreeMemory mfree);
extern "C" void __cdecl NewtonUpdate(const PNewtonWorld newtonWorld, float timestep);
extern "C" void __cdecl NewtonInvalidateCache(const PNewtonWorld newtonWorld);
extern "C" void __cdecl NewtonCollisionUpdate(const PNewtonWorld newtonWorld);
extern "C" void __cdecl NewtonSetSolverModel(const PNewtonWorld NewtonWorld, int Model);
extern "C" void __cdecl NewtonSetPlatformArchitecture(const PNewtonWorld newtonWorld, int mode);
extern "C" int __cdecl NewtonGetPlatformArchitecture(const PNewtonWorld newtonWorld, PCharArray description);
extern "C" void __cdecl NewtonSetMultiThreadSolverOnSingleIsland(const PNewtonWorld newtonWorld, int mode);
extern "C" int __cdecl NewtonGetMultiThreadSolverOnSingleIsland(const PNewtonWorld newtonWorld);
extern "C" void __cdecl NewtonSetPerformanceClock(const PNewtonWorld newtonWorld, PNewtonGetTicksCountCallback NewtonGetTicksCountCallback);
extern "C" unsigned __cdecl NewtonReadPerformanceTicks(const PNewtonWorld newtonWorld, unsigned performanceEntry);
extern "C" unsigned __cdecl NewtonReadThreadPerformanceTicks(const PNewtonWorld NewtonWorld, unsigned ThreadIndex);
extern "C" void __cdecl NewtonWorldCriticalSectionLock(const PNewtonWorld newtonWorld);
extern "C" void __cdecl NewtonWorldCriticalSectionUnlock(const PNewtonWorld newtonWorld);
extern "C" void __cdecl NewtonSetThreadsCount(const PNewtonWorld newtonWorld, int threads);
extern "C" int __cdecl NewtonGetThreadsCount(const PNewtonWorld newtonWorld);
extern "C" int __cdecl NewtonGetMaxThreadsCount(const PNewtonWorld NewtonWorld);
extern "C" void __cdecl NewtonSetFrictionModel(const PNewtonWorld NewtonWorld, int Model);
extern "C" void __cdecl NewtonSetMinimumFrameRate(const PNewtonWorld newtonWorld, float frameRate);
extern "C" void __cdecl NewtonSetBodyLeaveWorldEvent(const PNewtonWorld newtonWorld, PNewtonBodyLeaveWorld callback);
extern "C" void __cdecl NewtonSetWorldSize(const PNewtonWorld newtonWorld, const PFloat minPoint, const PFloat maxPoint);
extern "C" void __cdecl NewtonSetIslandUpdateEvent(const PNewtonWorld newtonWorld, PNewtonIslandUpdate NewtonIslandUpdate);
extern "C" void __cdecl NewtonSetCollisionDestructor(const PNewtonWorld newtonWorld, PNewtonCollisionDestructor callback);
extern "C" void __cdecl NewtonSetDestroyBodyByExeciveForce(const PNewtonWorld newtonWorld, PNewtonDestroyBodyByExeciveForce callback);
extern "C" void __cdecl NewtonWorldForEachJointDo(const PNewtonWorld newtonWorld, PNewtonJointIterator callback, void * userData);
extern "C" void __cdecl NewtonWorldForEachBodyInAABBDo(const PNewtonWorld newtonWorld, const PFloat p0, const PFloat p1, PNewtonBodyIterator callback, void * userData);
extern "C" void __cdecl NewtonWorldSetUserData(const PNewtonWorld newtonWorld, void * userData);
extern "C" void * __cdecl NewtonWorldGetUserData(const PNewtonWorld newtonWorld);
extern "C" void __cdecl NewtonWorldSetDestructorCallBack(const PNewtonWorld newtonWorld, PNewtonDestroyWorld NewtonDestroyWorld);
extern "C" PNewtonDestroyWorld __cdecl NewtonWorldGetDestructorCallBack(const PNewtonWorld newtonWorld);
extern "C" void __cdecl NewtonWorldRayCast(const PNewtonWorld newtonWorld, const PFloat p0, const PFloat p1, PNewtonWorldRayFilterCallback filter, void * userData, NewtonWorldRayPrefilterCallback prefilter);
extern "C" int __cdecl NewtonWorldConvexCast(const PNewtonWorld newtonWorld, const PFloat matrix, const PFloat target, const PNewtonCollision shape, PFloat hitParam, void * userData, NewtonWorldRayPrefilterCallback prefilter, PNewtonWorldConvexCastReturnInfo info, int maxContactsCount, int threadIndex);
extern "C" int __cdecl NewtonWorldGetBodyCount(const PNewtonWorld newtonWorld);
extern "C" int __cdecl NewtonWorldGetConstraintCount(const PNewtonWorld newtonWorld);
extern "C" PNewtonBody __cdecl NewtonIslandGetBody(const void * island, int bodyIndex);
extern "C" void __cdecl NewtonIslandGetBodyAABB(const void * island, int bodyIndex, PFloat p0, PFloat p1);
extern "C" int __cdecl NewtonMaterialCreateGroupID(const PNewtonWorld newtonWorld);
extern "C" int __cdecl NewtonMaterialGetDefaultGroupID(const PNewtonWorld newtonWorld);
extern "C" void __cdecl NewtonMaterialDestroyAllGroupID(const PNewtonWorld newtonWorld);
extern "C" void * __cdecl NewtonMaterialGetUserData(const PNewtonWorld NewtonWorld, int id0, int id1);
extern "C" void __cdecl NewtonMaterialSetSurfaceThickness(const PNewtonWorld newtonWorld, int id0, int id1, float thickness);
extern "C" void __cdecl NewtonMaterialSetContinuousCollisionMode(const PNewtonWorld newtonWorld, int id0, int id1, int state);
extern "C" void __cdecl NewtonMaterialSetCollisionCallback(const PNewtonWorld newtonWorld, int id0, int id1, void * userData, PNewtonOnAABBOverlap AABBOverlap, PNewtonContactsProcess process);
extern "C" void __cdecl NewtonMaterialSetDefaultSoftness(const PNewtonWorld newtonWorld, int id0, int id1, float value);
extern "C" void __cdecl NewtonMaterialSetDefaultElasticity(const PNewtonWorld newtonWorld, int id0, int id1, float elasticCoef);
extern "C" void __cdecl NewtonMaterialSetDefaultCollidable(const PNewtonWorld newtonWorld, int id0, int id1, int state);
extern "C" void __cdecl NewtonMaterialSetDefaultFriction(const PNewtonWorld newtonWorld, int id0, int id1, float staticFriction, float kineticFriction);
extern "C" PNewtonMaterial __cdecl NewtonWorldGetFirstMaterial(const PNewtonWorld NewtonWorld);
extern "C" PNewtonMaterial __cdecl NewtonWorldGetNextMaterial(const PNewtonWorld NewtonWorld, const PNewtonMaterial material);
extern "C" PNewtonBody __cdecl NewtonWorldGetFirstBody(const PNewtonWorld NewtonWorld);
extern "C" PNewtonBody __cdecl NewtonWorldGetNextBody(const PNewtonWorld NewtonWorld, const PNewtonBody curBody);
extern "C" void * __cdecl NewtonMaterialGetMaterialPairUserData(const PNewtonMaterial material);
extern "C" unsigned __cdecl NewtonMaterialGetContactFaceAttribute(const PNewtonMaterial material);
extern "C" unsigned __cdecl NewtonMaterialGetBodyCollisionID(const PNewtonMaterial material, PNewtonBody body);
extern "C" float __cdecl NewtonMaterialGetContactNormalSpeed(const PNewtonMaterial material);
extern "C" void __cdecl NewtonMaterialGetContactForce(const PNewtonMaterial Material, const PNewtonBody Body, PFloat Force);
extern "C" void __cdecl NewtonMaterialGetContactPositionAndNormal(const PNewtonMaterial Material, const PNewtonBody Body, const PFloat Posit, const PFloat Normal);
extern "C" void __cdecl NewtonMaterialGetContactTangentDirections(const PNewtonMaterial Material, const PNewtonBody Body, const PFloat Dir0, const PFloat Dir1);
extern "C" float __cdecl NewtonMaterialGetContactTangentSpeed(const PNewtonMaterial material, int index);
extern "C" void __cdecl NewtonMaterialSetContactSoftness(const PNewtonMaterial material, float softness);
extern "C" void __cdecl NewtonMaterialSetContactElasticity(const PNewtonMaterial material, float restitution);
extern "C" void __cdecl NewtonMaterialSetContactFrictionState(const PNewtonMaterial material, int state, int index);
extern "C" void __cdecl NewtonMaterialSetContactFrictionCoef(const PNewtonMaterial material, float staticFrictionCoef, float kineticFrictionCoef, int index);
extern "C" void __cdecl NewtonMaterialSetContactNormalAcceleration(const PNewtonMaterial material, float accel);
extern "C" void __cdecl NewtonMaterialSetContactNormalDirection(const PNewtonMaterial material, PFloat directionVector);
extern "C" void __cdecl NewtonMaterialSetContactTangentAcceleration(const PNewtonMaterial material, float accel, int index);
extern "C" void __cdecl NewtonMaterialContactRotateTangentDirections(const PNewtonMaterial material, const PFloat directionVector);
extern "C" PNewtonCollision __cdecl NewtonCreateNull(const PNewtonWorld newtonWorld);
extern "C" PNewtonCollision __cdecl NewtonCreateSphere(const PNewtonWorld newtonWorld, float radiusX, float radiusY, float radiusZ, int shapeID, const PFloat offsetMatrix);
extern "C" PNewtonCollision __cdecl NewtonCreateBox(const PNewtonWorld newtonWorld, float dx, float dy, float dz, int shapeID, const PFloat offsetMatrix);
extern "C" PNewtonCollision __cdecl NewtonCreateCone(const PNewtonWorld newtonWorld, float radius, float height, int shapeID, const PFloat offsetMatrix);
extern "C" PNewtonCollision __cdecl NewtonCreateCapsule(const PNewtonWorld newtonWorld, float radius, float height, int shapeID, const PFloat offsetMatrix);
extern "C" PNewtonCollision __cdecl NewtonCreateCylinder(const PNewtonWorld newtonWorld, float radius, float height, int shapeID, const PFloat offsetMatrix);
extern "C" PNewtonCollision __cdecl NewtonCreateChamferCylinder(const PNewtonWorld newtonWorld, float raduis, float height, int shapeID, const PFloat offsetMatrix);
extern "C" PNewtonCollision __cdecl NewtonCreateConvexHull(const PNewtonWorld newtonWorld, int count, const PFloat vertexCloud, int strideInBytes, float tolerance, int shapeID, const PFloat offsetMatrix);
extern "C" PNewtonCollision __cdecl NewtonCreateConvexHullFromMesh(const PNewtonWorld newtonWorld, PNewtonMesh mesh, float tolerance, int shapeID);
extern "C" PNewtonCollision __cdecl NewtonCreateConvexHullModifier(const PNewtonWorld newtonWorld, const PNewtonCollision convexHullCollision, int shapeID);
extern "C" void __cdecl NewtonConvexHullModifierGetMatrix(const PNewtonCollision convexHullCollision, PFloat matrix);
extern "C" void __cdecl NewtonConvexHullModifierSetMatrix(const PNewtonCollision convexHullCollision, const PFloat matrix);
extern "C" int __cdecl NewtonCollisionIsTriggerVolume(const PNewtonCollision convexCollision);
extern "C" void __cdecl NewtonCollisionSetAsTriggerVolume(const PNewtonCollision convexCollision, int trigger);
extern "C" void __cdecl NewtonCollisionSetMaxBreakImpactImpulse(const PNewtonCollision convexHullCollision, float maxImpactImpulse);
extern "C" float __cdecl NewtonCollisionGetMaxBreakImpactImpulse(const PNewtonCollision convexHullCollision);
extern "C" void __cdecl NewtonCollisionSetUserID(const PNewtonCollision convexCollision, unsigned id);
extern "C" unsigned __cdecl NewtonCollisionGetUserID(const PNewtonCollision convexCollision);
extern "C" int __cdecl NewtonConvexHullGetFaceIndices(const PNewtonCollision convexHullCollision, int face, PInt faceIndices);
extern "C" float __cdecl NewtonConvexCollisionCalculateVolume(const PNewtonCollision convexCollision);
extern "C" void __cdecl NewtonConvexCollisionCalculateInertialMatrix(const PNewtonCollision convexCollision, PFloat inertia, PFloat origin);
extern "C" void __cdecl NewtonCollisionMakeUnique(const PNewtonWorld newtonWorld, const PNewtonCollision collision);
extern "C" void __cdecl NewtonReleaseCollision(const PNewtonWorld newtonWorld, const PNewtonCollision collision);
extern "C" int __cdecl NewtonAddCollisionReference(const PNewtonCollision Collision);
extern "C" PNewtonCollision __cdecl NewtonCreateCompoundCollision(const PNewtonWorld newtonWorld, int count, const TCollisionPrimitiveArray collisionPrimitiveArray, int shapeID);
extern "C" PNewtonCollision __cdecl NewtonCreateCompoundCollisionFromMesh(const PNewtonWorld newtonWorld, const PNewtonMesh mesh, int maxSubShapesCount, int shapeID, int subShapeID);
extern "C" PNewtonCollision __cdecl NewtonCreateUserMeshCollision(const PNewtonWorld newtonWorld, const PFloat minBox, const PFloat maxBox, void * userData, NewtonUserMeshCollisionCollideCallback collideCallback, NewtonUserMeshCollisionRayHitCallback rayHitCallback, NewtonUserMeshCollisionDestroyCallback destroyCallback, NewtonUserMeshCollisionGetCollisionInfo getInfoCallback, NewtonUserMeshCollisionGetFacesInAABB facesInAABBCallback, int shapeID);
extern "C" PNewtonCollision __cdecl NewtonCreateTreeCollisionFromMesh(const PNewtonWorld NewtonWorld, const PNewtonMesh Mesh, int ShapeID);
extern "C" PNewtonCollision __cdecl NewtonCreateSceneCollision(const PNewtonWorld newtonWorld, int shapeID);
extern "C" PNewtonSceneProxy __cdecl NewtonSceneCollisionCreateProxy(PNewtonCollision scene, PNewtonCollision collision);
extern "C" void __cdecl NewtonSceneCollisionDestroyProxy(PNewtonCollision scene, PNewtonSceneProxy Proxy);
extern "C" void __cdecl NewtonSceneProxySetMatrix(PNewtonSceneProxy Proxy, const PFloat Matrix);
extern "C" void __cdecl NewtonSceneProxyGetMatrix(PNewtonSceneProxy Proxy, PFloat Matrix);
extern "C" void __cdecl NewtonSceneSetProxyUserData(const PNewtonSceneProxy Proxy, void * UserData);
extern "C" void * __cdecl NewtonSceneGetProxyUserData(const PNewtonSceneProxy Proxy);
extern "C" PNewtonSceneProxy __cdecl NewtonSceneGetFirstProxy(const PNewtonCollision Scene);
extern "C" PNewtonSceneProxy __cdecl NewtonSceneGetNextProxy(const PNewtonCollision Scene, const PNewtonSceneProxy Proxy);
extern "C" void __cdecl NewtonSceneCollisionOptimize(PNewtonCollision scene);
extern "C" PNewtonCollision __cdecl NewtonCreateCompoundBreakable(const PNewtonWorld NewtonWorld, int meshCount, const PNewtonMesh SolidsArray, const PInt ShapeIDArray, PFloat Densities, PInt internalFaceMaterial, int ShapeID, int debrisID, float DebrisSeparationGap);
extern "C" void __cdecl NewtonCompoundBreakableResetAnchoredPieces(const PNewtonCollision compoundBreakable);
extern "C" void __cdecl NewtonCompoundBreakableSetAnchoredPieces(const PNewtonCollision compoundBreakable, int fixshapesCount, PFloat matrixPallete, PNewtonCollision fixedShapesArray);
extern "C" int __cdecl NewtonCompoundBreakableGetVertexCount(const PNewtonCollision compoundBreakable);
extern "C" void __cdecl NewtonCompoundBreakableGetVertexStreams(const PNewtonCollision compoundBreakable, int vertexStrideInByte, PFloat Vertex, int normalStrideInByte, PFloat normal, int uvStrideInByte, PFloat uv);
extern "C" PNewtonBreakableComponentMesh __cdecl NewtonBreakableGetMainMesh(const PNewtonCollision compoundBreakable);
extern "C" PNewtonBreakableComponentMesh __cdecl NewtonBreakableGetFirstComponent(const PNewtonCollision compoundBreakable);
extern "C" PNewtonBreakableComponentMesh __cdecl NewtonBreakableGetNextComponent(const PNewtonBreakableComponentMesh component);
extern "C" void __cdecl NewtonBreakableBeginDelete(const PNewtonCollision compoundBreakable);
extern "C" PNewtonBody __cdecl NewtonBreakableCreateDebrieBody(const PNewtonCollision compoundBreakable, const PNewtonBreakableComponentMesh component);
extern "C" void __cdecl NewtonBreakableDeleteComponent(const PNewtonCollision compoundBreakable, const PNewtonBreakableComponentMesh component);
extern "C" void __cdecl NewtonBreakableEndDelete(const PNewtonCollision compoundBreakable);
extern "C" int __cdecl NewtonBreakableGetComponentsInRadius(const PNewtonCollision compoundBreakable, const PFloat position, float radius, PNewtonBreakableComponentMesh Segments, int maxCount);
extern "C" void * __cdecl NewtonBreakableGetFirstSegment(const PNewtonBreakableComponentMesh BreakableComponent);
extern "C" void * __cdecl NewtonBreakableGetNextSegment(const void * Segment);
extern "C" int __cdecl NewtonBreakableSegmentGetMaterial(const void * Segment);
extern "C" int __cdecl NewtonBreakableSegmentGetIndexCount(const void * Segment);
extern "C" int __cdecl NewtonBreakableSegmentGetIndexStream(PNewtonCollision CompoundBreakable, const PNewtonBreakableComponentMesh MeshOwner, const void * Segment, PInt Index);
extern "C" int __cdecl NewtonBreakableSegmentGetIndexStreamShort(PNewtonCollision CompoundBreakable, const PNewtonBreakableComponentMesh MeshOwner, const void * Segment, PShort Index);
extern "C" PNewtonCollision __cdecl NewtonCreateCollisionFromSerialization(const PNewtonWorld newtonWorld, PNewtonDeserialize deserializeFunction, void * serializeHandle);
extern "C" void __cdecl NewtonCollisionSerialize(const PNewtonWorld newtonWorld, const PNewtonCollision collision, PNewtonSerialize serializeFunction, void * serializeHandle);
extern "C" void __cdecl NewtonCollisionGetInfo(const PNewtonCollision collision, PNewtonCollisionInfoRecord collisionInfo);
extern "C" PNewtonCollision __cdecl NewtonCreateHeightFieldCollision(const PNewtonWorld newtonWorld, int width, int height, int gridDiagonals, PUnsigned_short elevationMap, P2Char attributeMap, float horizontalScale, float verticalScale, int shapeID);
extern "C" void __cdecl NewtonHeightFieldSetUserRayCastCallback(const PNewtonCollision TreeCollision, PNewtonHeightFieldRayCastCallback RayHitCallBack);
extern "C" PNewtonCollision __cdecl NewtonCreateTreeCollision(const PNewtonWorld newtonWorld, int shapeID);
extern "C" void __cdecl NewtonTreeCollisionSetUserRayCastCallback(const PNewtonCollision treeCollision, PNewtonCollisionTreeRayCastCallback rayHitCallback);
extern "C" void __cdecl NewtonTreeCollisionBeginBuild(const PNewtonCollision treeCollision);
extern "C" void __cdecl NewtonTreeCollisionAddFace(const PNewtonCollision treeCollision, int vertexCount, const PFloat vertexPtr, int strideInBytes, int faceAttribute);
extern "C" void __cdecl NewtonTreeCollisionEndBuild(const PNewtonCollision treeCollision, int optimize);
extern "C" int __cdecl NewtonTreeCollisionGetFaceAtribute(const PNewtonCollision treeCollision, const PInt faceIndexArray);
extern "C" void __cdecl NewtonTreeCollisionSetFaceAtribute(const PNewtonCollision treeCollision, const PInt faceIndexArray, int attribute);
extern "C" int __cdecl NewtonTreeCollisionGetVertexListIndexListInAABB(const PNewtonCollision treeCollision, const PFloat p0, const PFloat p1, const PFloat vertexArray, PInt vertexCount, PInt vertexStrideInBytes, const PInt indexList, int maxIndexCount, const PInt faceAttribute);
extern "C" void __cdecl NewtonStaticCollisionSetDebugCallback(const PNewtonCollision staticCollision, PNewtonTreeCollisionCallback userCallback);
extern "C" int __cdecl NewtonCollisionPointDistance(const PNewtonWorld newtonWorld, const PFloat point, const PNewtonCollision collision, const PFloat matrix, PFloat contact, PFloat normal, int threadIndex);
extern "C" int __cdecl NewtonCollisionClosestPoint(const PNewtonWorld newtonWorld, const PNewtonCollision collsionA, const PFloat matrixA, const PNewtonCollision collisionB, const PFloat matrixB, PFloat contactA, PFloat contactB, PFloat normalAB, int threadIndex);
extern "C" int __cdecl NewtonCollisionCollide(const PNewtonWorld newtonWorld, int maxSize, const PNewtonCollision collsionA, const PFloat matrixA, const PNewtonCollision collisionB, const PFloat matrixB, PFloat contacts, PFloat normals, PFloat penetration, int threadIndex);
extern "C" int __cdecl NewtonCollisionCollideContinue(const PNewtonWorld newtonWorld, int maxSize, const float timestep, const PNewtonCollision collsionA, const PFloat matrixA, const PFloat velocA, const float omegaA, const PNewtonCollision collsionB, const PFloat matrixB, const PFloat velocB, const float omegaB, PFloat timeOfImpact, PFloat contacts, PFloat normals, PFloat penetration, int threadIndex);
extern "C" void __cdecl NewtonCollisionSupportVertex(const PNewtonCollision collision, const PFloat dir, PFloat vertex);
extern "C" float __cdecl NewtonCollisionRayCast(const PNewtonCollision collision, const PFloat p0, const PFloat p1, PFloat normals, PInt attribute);
extern "C" void __cdecl NewtonCollisionCalculateAABB(const PNewtonCollision collision, const PFloat matrix, PFloat p0, PFloat p1);
extern "C" void __cdecl NewtonCollisionForEachPolygonDo(const PNewtonCollision collision, const PFloat matrix, NewtonCollisionIterator callback, void * UserData);
extern "C" void __cdecl NewtonGetEulerAngle(const PFloat matrix, PFloat eulersAngles);
extern "C" void __cdecl NewtonSetEulerAngle(const PFloat eulersAngles, PFloat matrix);
extern "C" float __cdecl NewtonCalculateSpringDamperAcceleration(float dt, float ks, float x, float kd, float s);
extern "C" PNewtonBody __cdecl NewtonCreateBody(const PNewtonWorld newtonWorld, const PNewtonCollision collision, const PFloat Matrix);
extern "C" void __cdecl NewtonDestroyBody(const PNewtonWorld newtonWorld, const PNewtonBody body);
extern "C" void __cdecl NewtonBodyAddForce(const PNewtonBody body, const PFloat force);
extern "C" void __cdecl NewtonBodyAddTorque(const PNewtonBody body, const PFloat torque);
extern "C" void __cdecl NewtonBodyCalculateInverseDynamicsForce(const PNewtonBody body, float timestep, const PFloat desiredVeloc, PFloat forceOut);
extern "C" void __cdecl NewtonBodySetMatrix(const PNewtonBody body, const PFloat matrix);
extern "C" void __cdecl NewtonBodySetMatrixRecursive(const PNewtonBody body, const PFloat matrix);
extern "C" void __cdecl NewtonBodySetMassMatrix(const PNewtonBody body, float mass, float Ixx, float Iyy, float Izz);
extern "C" void __cdecl NewtonBodySetMaterialGroupID(const PNewtonBody body, int id);
extern "C" void __cdecl NewtonBodySetContinuousCollisionMode(const PNewtonBody body, int state);
extern "C" void __cdecl NewtonBodySetJointRecursiveCollision(const PNewtonBody body, unsigned state);
extern "C" void __cdecl NewtonBodySetOmega(const PNewtonBody body, const PFloat omega);
extern "C" void __cdecl NewtonBodySetVelocity(const PNewtonBody body, const PFloat velocity);
extern "C" void __cdecl NewtonBodySetForce(const PNewtonBody body, const PFloat force);
extern "C" void __cdecl NewtonBodySetTorque(const PNewtonBody body, const PFloat torque);
extern "C" void __cdecl NewtonBodySetCentreOfMass(const PNewtonBody body, const PFloat com);
extern "C" void __cdecl NewtonBodySetLinearDamping(const PNewtonBody body, float linearDamp);
extern "C" void __cdecl NewtonBodySetAngularDamping(const PNewtonBody body, const PFloat angularDamp);
extern "C" void __cdecl NewtonBodySetUserData(const PNewtonBody body, void * userData);
extern "C" void __cdecl NewtonBodySetCollision(const PNewtonBody body, const PNewtonCollision collision);
extern "C" int __cdecl NewtonBodyGetSleepState(const PNewtonBody body);
extern "C" int __cdecl NewtonBodyGetAutoSleep(const PNewtonBody body);
extern "C" void __cdecl NewtonBodySetAutoSleep(const PNewtonBody body, int state);
extern "C" int __cdecl NewtonBodyGetFreezeState(const PNewtonBody body);
extern "C" void __cdecl NewtonBodySetFreezeState(const PNewtonBody body, int state);
extern "C" void __cdecl NewtonBodySetDestructorCallback(const PNewtonBody body, NewtonBodyDestructor callback);
extern "C" void __cdecl NewtonBodySetTransformCallback(const PNewtonBody body, NewtonSetTransform callback);
extern "C" NewtonSetTransform __cdecl NewtonBodyGetTransformCallback(const PNewtonBody body);
extern "C" void __cdecl NewtonBodySetForceAndTorqueCallback(const PNewtonBody body, NewtonApplyForceAndTorque callback);
extern "C" NewtonApplyForceAndTorque __cdecl NewtonBodyGetForceAndTorqueCallback(const PNewtonBody body);
extern "C" void * __cdecl NewtonBodyGetUserData(const PNewtonBody body);
extern "C" PNewtonWorld __cdecl NewtonBodyGetWorld(const PNewtonBody body);
extern "C" PNewtonCollision __cdecl NewtonBodyGetCollision(const PNewtonBody body);
extern "C" int __cdecl NewtonBodyGetMaterialGroupID(const PNewtonBody body);
extern "C" int __cdecl NewtonBodyGetContinuousCollisionMode(const PNewtonBody body);
extern "C" int __cdecl NewtonBodyGetJointRecursiveCollision(const PNewtonBody body);
extern "C" void __cdecl NewtonBodyGetMatrix(const PNewtonBody body, PFloat matrix);
extern "C" void __cdecl NewtonBodyGetRotation(const PNewtonBody body, PFloat rotation);
extern "C" void __cdecl NewtonBodyGetMassMatrix(const PNewtonBody body, PFloat mass, PFloat Ixx, PFloat Iyy, PFloat Izz);
extern "C" void __cdecl NewtonBodyGetInvMass(const PNewtonBody body, PFloat invMass, PFloat invIxx, PFloat invIyy, PFloat invIzz);
extern "C" void __cdecl NewtonBodyGetOmega(const PNewtonBody body, PFloat vector);
extern "C" void __cdecl NewtonBodyGetVelocity(const PNewtonBody body, PFloat vector);
extern "C" void __cdecl NewtonBodyGetForce(const PNewtonBody body, PFloat vector);
extern "C" void __cdecl NewtonBodyGetTorque(const PNewtonBody body, PFloat vector);
extern "C" void __cdecl NewtonBodyGetForceAcc(const PNewtonBody body, PFloat vector);
extern "C" void __cdecl NewtonBodyGetTorqueAcc(const PNewtonBody body, PFloat vector);
extern "C" void __cdecl NewtonBodyGetCentreOfMass(const PNewtonBody body, PFloat com);
extern "C" float __cdecl NewtonBodyGetLinearDamping(const PNewtonBody body);
extern "C" void __cdecl NewtonBodyGetAngularDamping(const PNewtonBody body, PFloat vector);
extern "C" void __cdecl NewtonBodyGetAABB(const PNewtonBody body, PFloat p0, PFloat p1);
extern "C" void __cdecl NewtonBodyGetFreezeTreshold(const PNewtonBody body, PFloat freezeSpeed2, PFloat freezeOmega2);
extern "C" PNewtonJoint __cdecl NewtonBodyGetFirstJoint(const PNewtonBody body);
extern "C" PNewtonJoint __cdecl NewtonBodyGetNextJoint(const PNewtonBody body, const PNewtonJoint joint);
extern "C" PNewtonJoint __cdecl NewtonBodyGetFirstContactJoint(const PNewtonBody body);
extern "C" PNewtonJoint __cdecl NewtonBodyGetNextContactJoint(const PNewtonBody body, const PNewtonJoint contactJoint);
extern "C" void * __cdecl NewtonContactJointGetFirstContact(const PNewtonJoint contactJoint);
extern "C" void * __cdecl NewtonContactJointGetNextContact(const PNewtonJoint contactJoint, void * contact);
extern "C" int __cdecl NewtonContactJointGetContactCount(const PNewtonJoint contactJoint);
extern "C" void __cdecl NewtonContactJointRemoveContact(const PNewtonJoint contactJoint, void * contact);
extern "C" PNewtonMaterial __cdecl NewtonContactGetMaterial(const void * contact);
extern "C" void __cdecl NewtonBodyAddBuoyancyForce(const PNewtonBody body, float fluidDensity, float fluidLinearViscosity, float fluidAngularViscosity, const PFloat gravityVector, NewtonGetBuoyancyPlane buoyancyPlane, void * context);
extern "C" void __cdecl NewtonBodyAddImpulse(const PNewtonBody body, const PFloat pointDeltaVeloc, const PFloat pointPosit);
extern "C" void __cdecl NewtonBodyApplyImpulseArray(const PNewtonBody Body, int ImpuleCount, int StrideInByte, const PFloat impulseArray, const PFloat pointArray);
extern "C" void * __cdecl NewtonJointGetUserData(const PNewtonJoint joint);
extern "C" void __cdecl NewtonJointSetUserData(const PNewtonJoint joint, void * userData);
extern "C" PNewtonBody __cdecl NewtonJointGetBody0(const PNewtonJoint joint);
extern "C" PNewtonBody __cdecl NewtonJointGetBody1(const PNewtonJoint joint);
extern "C" void __cdecl NewtonJointGetInfo(const PNewtonJoint joint, PNewtonJointRecord info);
extern "C" int __cdecl NewtonJointGetCollisionState(const PNewtonJoint joint);
extern "C" void __cdecl NewtonJointSetCollisionState(const PNewtonJoint joint, int state);
extern "C" float __cdecl NewtonJointGetStiffness(const PNewtonJoint joint);
extern "C" void __cdecl NewtonJointSetStiffness(const PNewtonJoint joint, float state);
extern "C" void __cdecl NewtonDestroyJoint(const PNewtonWorld newtonWorld, const PNewtonJoint joint);
extern "C" void __cdecl NewtonJointSetDestructor(const PNewtonJoint joint, NewtonConstraintDestructor _destructor);
extern "C" PNewtonJoint __cdecl NewtonConstraintCreateBall(const PNewtonWorld newtonWorld, const PFloat pivotPoint, const PNewtonBody childBody, const PNewtonBody parentBody);
extern "C" void __cdecl NewtonBallSetUserCallback(const PNewtonJoint ball, NewtonBallCallBack callback);
extern "C" void __cdecl NewtonBallGetJointAngle(const PNewtonJoint ball, PFloat angle);
extern "C" void __cdecl NewtonBallGetJointOmega(const PNewtonJoint ball, PFloat omega);
extern "C" void __cdecl NewtonBallGetJointForce(const PNewtonJoint ball, PFloat force);
extern "C" void __cdecl NewtonBallSetConeLimits(const PNewtonJoint ball, const PFloat pin, float maxConeAngle, float maxTwistAngle);
extern "C" PNewtonJoint __cdecl NewtonConstraintCreateHinge(const PNewtonWorld newtonWorld, const PFloat pivotPoint, const PFloat pinDir, const PNewtonBody childBody, const PNewtonBody parentBody);
extern "C" void __cdecl NewtonHingeSetUserCallback(const PNewtonJoint hinge, NewtonHingeCallBack callback);
extern "C" float __cdecl NewtonHingeGetJointAngle(const PNewtonJoint hinge);
extern "C" float __cdecl NewtonHingeGetJointOmega(const PNewtonJoint hinge);
extern "C" void __cdecl NewtonHingeGetJointForce(const PNewtonJoint hinge, PFloat force);
extern "C" float __cdecl NewtonHingeCalculateStopAlpha(const PNewtonJoint hinge, const PNewtonHingeSliderUpdateDesc desc, float angle);
extern "C" PNewtonJoint __cdecl NewtonConstraintCreateSlider(const PNewtonWorld newtonWorld, const PFloat pivotPoint, const PFloat pinDir, const PNewtonBody childBody, const PNewtonBody parentBody);
extern "C" void __cdecl NewtonSliderSetUserCallback(const PNewtonJoint slider, NewtonSliderCallBack callback);
extern "C" float __cdecl NewtonSliderGetJointPosit(const PNewtonJoint slider);
extern "C" float __cdecl NewtonSliderGetJointVeloc(const PNewtonJoint slider);
extern "C" void __cdecl NewtonSliderGetJointForce(const PNewtonJoint slider, PFloat force);
extern "C" float __cdecl NewtonSliderCalculateStopAccel(const PNewtonJoint slider, const PNewtonHingeSliderUpdateDesc desc, float position);
extern "C" PNewtonJoint __cdecl NewtonConstraintCreateCorkscrew(const PNewtonWorld newtonWorld, const PFloat pivotPoint, const PFloat pinDir, const PNewtonBody childBody, const PNewtonBody parentBody);
extern "C" void __cdecl NewtonCorkscrewSetUserCallback(const PNewtonJoint corkscrew, NewtonCorkscrewCallBack callback);
extern "C" float __cdecl NewtonCorkscrewGetJointPosit(const PNewtonJoint corkscrew);
extern "C" float __cdecl NewtonCorkscrewGetJointAngle(const PNewtonJoint corkscrew);
extern "C" float __cdecl NewtonCorkscrewGetJointVeloc(const PNewtonJoint corkscrew);
extern "C" float __cdecl NewtonCorkscrewGetJointOmega(const PNewtonJoint corkscrew);
extern "C" void __cdecl NewtonCorkscrewGetJointForce(const PNewtonJoint corkscrew, PFloat force);
extern "C" float __cdecl NewtonCorkscrewCalculateStopAlpha(const PNewtonJoint corkscrew, const PNewtonHingeSliderUpdateDesc desc, float angle);
extern "C" float __cdecl NewtonCorkscrewCalculateStopAccel(const PNewtonJoint corkscrew, const PNewtonHingeSliderUpdateDesc desc, float position);
extern "C" PNewtonJoint __cdecl NewtonConstraintCreateUniversal(const PNewtonWorld newtonWorld, const PFloat pivotPoint, const PFloat pinDir0, const PFloat pinDir1, const PNewtonBody childBody, const PNewtonBody parentBody);
extern "C" void __cdecl NewtonUniversalSetUserCallback(const PNewtonJoint universal, NewtonUniversalCallBack callback);
extern "C" float __cdecl NewtonUniversalGetJointAngle0(const PNewtonJoint universal);
extern "C" float __cdecl NewtonUniversalGetJointAngle1(const PNewtonJoint universal);
extern "C" float __cdecl NewtonUniversalGetJointOmega0(const PNewtonJoint universal);
extern "C" float __cdecl NewtonUniversalGetJointOmega1(const PNewtonJoint universal);
extern "C" void __cdecl NewtonUniversalGetJointForce(const PNewtonJoint universal, PFloat force);
extern "C" float __cdecl NewtonUniversalCalculateStopAlpha0(const PNewtonJoint universal, const PNewtonHingeSliderUpdateDesc desc, float angle);
extern "C" float __cdecl NewtonUniversalCalculateStopAlpha1(const PNewtonJoint universal, const PNewtonHingeSliderUpdateDesc desc, float angle);
extern "C" PNewtonJoint __cdecl NewtonConstraintCreateUpVector(const PNewtonWorld newtonWorld, const PFloat pinDir, const PNewtonBody body);
extern "C" void __cdecl NewtonUpVectorGetPin(const PNewtonJoint upVector, PFloat pin);
extern "C" void __cdecl NewtonUpVectorSetPin(const PNewtonJoint upVector, const PFloat pin);
extern "C" PNewtonJoint __cdecl NewtonConstraintCreateUserJoint(const PNewtonWorld NewtonWorld, int MaxDOF, PNewtonUserBilateralCallBack Callback, PNewtonUserBilateralGetInfoCallBack GetInfo, const PNewtonBody ChildBody, const PNewtonBody parentBody);
extern "C" void __cdecl NewtonUserJointSetFeedbackCollectorCallback(const PNewtonJoint Joint, PNewtonUserBilateralCallBack GetFeedback);
extern "C" void __cdecl NewtonUserJointAddLinearRow(const PNewtonJoint Joint, const PFloat pivot0, const PFloat pivot1, const PFloat Dir);
extern "C" void __cdecl NewtonUserJointAddAngularRow(const PNewtonJoint Joint, float RelativeAngle, const PFloat Dir);
extern "C" void __cdecl NewtonUserJointAddGeneralRow(const PNewtonJoint Joint, const PFloat Jacobian0, const PFloat Jacobian1);
extern "C" void __cdecl NewtonUserJointSetRowMinimumFriction(const PNewtonJoint Joint, float Friction);
extern "C" void __cdecl NewtonUserJointSetRowMaximumFriction(const PNewtonJoint Joint, float Friction);
extern "C" void __cdecl NewtonUserJointSetRowAcceleration(const PNewtonJoint Joint, float Acceleration);
extern "C" void __cdecl NewtonUserJointSetRowSpringDamperAcceleration(const PNewtonJoint joint, float springK, float springD);
extern "C" void __cdecl NewtonUserJointSetRowStiffness(const PNewtonJoint Joint, float Stiffness);
extern "C" float __cdecl NewtonUserJointGetRowForce(const PNewtonJoint Joint, int Row);
extern "C" PNewtonMesh __cdecl NewtonMeshCreate(const PNewtonWorld World);
extern "C" PNewtonMesh __cdecl NewtonMeshCreateFromMesh(const PNewtonMesh Mesh);
extern "C" PNewtonMesh __cdecl NewtonMeshCreateFromCollision(const PNewtonCollision collision);
extern "C" PNewtonMesh __cdecl NewtonMeshConvexHull(const PNewtonWorld NewtonWorld, int count, const PFloat vertexCloud, int strideInBytes, float tolerance);
extern "C" PNewtonMesh __cdecl NewtonMeshCreatePlane(const PNewtonWorld World, const PFloat locationMatrix, float width, float breadth, int material, const PFloat textureMatrix0, const void *textureMatrix1);
extern "C" void __cdecl NewtonMeshDestroy(const PNewtonMesh mesh);
extern "C" void __cdecl NewtonMeshCalculateOOBB(const PNewtonMesh mesh, const PFloat matrix, PFloat x, PFloat y, PFloat z);
extern "C" void __cdecl NewtonMesApplyTransform(const PNewtonMesh Mesh, const PFloat Matrix);
extern "C" void __cdecl NewtonMeshCalculateVertexNormals(const PNewtonMesh mesh, float angleInRadians);
extern "C" void __cdecl NewtonMeshApplySphericalMapping(const PNewtonMesh mesh, int material);
extern "C" void __cdecl NewtonMeshApplyBoxMapping(const PNewtonMesh mesh, int front, int side, int top);
extern "C" void __cdecl NewtonMeshApplyCylindricalMapping(const PNewtonMesh mesh, int cylinderMaterial, int capMaterial);
extern "C" int __cdecl NewtonMeshIsOpenMesh(const PNewtonMesh mesh);
extern "C" void __cdecl NewtonMeshFixTJoints(const PNewtonMesh mesh);
extern "C" void __cdecl NewtonMeshPolygonize(const PNewtonMesh mesh);
extern "C" void __cdecl NewtonMeshTriangulate(const PNewtonMesh mesh);
extern "C" PNewtonMesh __cdecl NewtonMeshUnion(const PNewtonMesh mesh, PNewtonMesh clipper, PFloat clipperMatrix);
extern "C" PNewtonMesh __cdecl NewtonMeshDifference(const PNewtonMesh mesh, PNewtonMesh clipper, PFloat clipperMatrix);
extern "C" PNewtonMesh __cdecl NewtonMeshIntersection(const PNewtonMesh mesh, PNewtonMesh clipper, PFloat clipperMatrix);
extern "C" void __cdecl NewtonMeshClip(const PNewtonMesh mesh, const PNewtonMesh clipper, const PFloat clippermatrix, const PNewtonMesh topMesh, const PNewtonMesh bottomMesh);
extern "C" void __cdecl NewtonMeshPlaneClip(const PNewtonMesh Mesh, const PFloat planeMatrix, const PFloat PlaneTextureMatrix, int PlaneMaterial, const PNewtonMesh topMesh, const PNewtonMesh bottomMesh);
extern "C" PNewtonMesh __cdecl NewtonMeshConvexDecomposition(const PNewtonMesh Mesh, int MaxCount);
extern "C" PNewtonMesh __cdecl NewtonMeshVoronoiDecomposition(const PNewtonMesh Mesh, int PointCount, int PointStrideInBytes, const PFloat PointCloud, int InternalMaterial, const PFloat TextureMatrix);
extern "C" void __cdecl NewtonRemoveUnusedVertices(const PNewtonMesh mesh, PInt vertexRemapTable);
extern "C" void __cdecl NewtonMeshBeginFace(const PNewtonMesh mesh);
extern "C" void __cdecl NewtonMeshAddFace(const PNewtonMesh mesh, int vertexCount, const PFloat vertex, int strideInBytes, int materialIndex);
extern "C" void __cdecl NewtonMeshEndFace(const PNewtonMesh mesh);
extern "C" void __cdecl NewtonMeshBuildFromVertexListIndexList(const PNewtonMesh Mesh, int FaceCount, const PInt faceIndexCount, const PInt faceMaterialIndex, const PFloat vertex, int vertexStrideInBytes, const PInt vertexIndex, const PFloat normal, int normalStrideInBytes, const PInt normalIndex, const PFloat uv0, int uv0StrideInBytes, const PInt uv0Index, const PFloat uv1, int uv1StrideInBytes, const PInt uv1Index);
extern "C" void __cdecl NewtonMeshGetVertexStreams(const PNewtonMesh Mesh, int vertexStrideInByte, PFloat Vertex, int normalStrideInByte, PFloat Normal, int uvStrideInByte0, PFloat uv0, int uvStrideInByte1, PFloat uv1);
extern "C" void __cdecl NewtonMeshGetIndirectVertexStreams(const PNewtonMesh mesh, int vertexStrideInByte, PFloat vertex, PInt vertexIndices, PInt vertexCount, int normalStrideInByte, PFloat normal, PInt normalIndices, PInt normalCount, int uvStrideInByte0, PFloat uv0, PInt uvIndices0, PInt uvCount0, int uvStrideInByte1, PFloat uv1, PInt uvIndices1, PInt uvCount1);
extern "C" void * __cdecl NewtonMeshBeginHandle(const PNewtonMesh mesh);
extern "C" void __cdecl NewtonMeshEndHandle(const PNewtonMesh mesh, void * Handle);
extern "C" int __cdecl NewtonMeshFirstMaterial(const PNewtonMesh mesh, void * Handle);
extern "C" int __cdecl NewtonMeshNextMaterial(const PNewtonMesh mesh, void * Handle, int materialID);
extern "C" int __cdecl NewtonMeshMaterialGetMaterial(const PNewtonMesh mesh, void * Handle, int materialID);
extern "C" int __cdecl NewtonMeshMaterialGetIndexCount(const PNewtonMesh mesh, void * Handle, int materialID);
extern "C" void __cdecl NewtonMeshMaterialGetIndexStream(const PNewtonMesh mesh, void * Handle, int materialID, PInt index);
extern "C" void __cdecl NewtonMeshMaterialGetIndexStreamShort(const PNewtonMesh mesh, void * Handle, int materialID, PShort index);
extern "C" PNewtonMesh __cdecl NewtonMeshCreateFirstSingleSegment(const PNewtonMesh mesh);
extern "C" PNewtonMesh __cdecl NewtonMeshCreateNextSingleSegment(const PNewtonMesh mesh, PNewtonMesh Segment);
extern "C" PNewtonMesh __cdecl NewtonMeshCreateFirstLayer(const PNewtonMesh Mesh);
extern "C" PNewtonMesh __cdecl NewtonMeshCreateNextLayer(const PNewtonMesh Mesh, const PNewtonMesh Segment);
extern "C" int __cdecl NewtonMeshGetTotalFaceCount(const PNewtonMesh mesh);
extern "C" int __cdecl NewtonMeshGetTotalIndexCount(const PNewtonMesh mesh);
extern "C" void __cdecl NewtonMeshGetFaces(const PNewtonMesh mesh, const PInt faceIndexCount, PInt faceMaterial, PInt faceIndices);
extern "C" int __cdecl NewtonMeshGetPointCount(const PNewtonMesh Mesh);
extern "C" int __cdecl NewtonMeshGetPointStrideInByte(const PNewtonMesh Mesh);
extern "C" PFloat __cdecl NewtonMeshGetPointArray(const PNewtonMesh Mesh);
extern "C" PFloat __cdecl NewtonMeshGetNormalArray(const PNewtonMesh Mesh);
extern "C" PFloat __cdecl NewtonMeshGetUV0Array(const PNewtonMesh Mesh);
extern "C" PFloat __cdecl NewtonMeshGetUV1Array(const PNewtonMesh Mesh);
extern "C" int __cdecl NewtonMeshGetVertexCount(const PNewtonMesh Mesh);
extern "C" int __cdecl NewtonMeshGetVertexStrideInByte(const PNewtonMesh Mesh);
extern "C" PFloat __cdecl NewtonMeshGetVertexArray(const PNewtonMesh Mesh);
extern "C" void * __cdecl NewtonMeshGetFirstVertex(const PNewtonMesh Mesh);
extern "C" void * __cdecl NewtonMeshGetNextVertex(const PNewtonMesh Mesh, const void * Vertex);
extern "C" int __cdecl NewtonMeshGetVertexIndex(const PNewtonMesh Mesh, const void * Vertex);
extern "C" void * __cdecl NewtonMeshGetFirstPoint(const PNewtonMesh Mesh);
extern "C" void * __cdecl NewtonMeshGetNextPoint(const PNewtonMesh Mesh, const void * point);
extern "C" int __cdecl NewtonMeshGetPointIndex(const PNewtonMesh Mesh, const void * point);
extern "C" int __cdecl NewtonMeshGetVertexIndexFromPoint(const PNewtonMesh Mesh, const void * point);
extern "C" void * __cdecl NewtonMeshGetFirstEdge(const PNewtonMesh Mesh);
extern "C" void * __cdecl NewtonMeshGetNextEdge(const PNewtonMesh Mesh, const void * Edge);
extern "C" void __cdecl NewtonMeshGetEdgeIndices(const PNewtonMesh Mesh, const void * Edge, PInt v0, PInt v1);
extern "C" void * __cdecl NewtonMeshGetFirstFace(const PNewtonMesh Mesh);
extern "C" void * __cdecl NewtonMeshGetNextFace(const PNewtonMesh Mesh, const void * Face);
extern "C" int __cdecl NewtonMeshIsFaceOpen(const PNewtonMesh Mesh, const void * Face);
extern "C" int __cdecl NewtonMeshGetFaceMaterial(const PNewtonMesh Mesh, const void * Face);
extern "C" int __cdecl NewtonMeshGetFaceIndexCount(const PNewtonMesh Mesh, const void * Face);
extern "C" void __cdecl NewtonMeshGetFaceIndices(const PNewtonMesh Mesh, const void * Face, PInt Indices);
extern "C" void __cdecl NewtonMeshGetFacePointIndices(const PNewtonMesh Mesh, const void * Face, PInt Indices);
}	/* namespace Newtonimport */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_NEWTONIMPORT)
using namespace Newtonimport;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// NewtonimportHPP
