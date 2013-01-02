// CodeGear C++Builder
// Copyright (c) 1995, 2012 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLNGDManager.pas' rev: 24.00 (Win32)

#ifndef GlngdmanagerHPP
#define GlngdmanagerHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <NewtonImport.hpp>	// Pascal unit
#include <NewtonImport_JointLibrary.hpp>	// Pascal unit
#include <VectorGeometry.hpp>	// Pascal unit
#include <VectorLists.hpp>	// Pascal unit
#include <System.Classes.hpp>	// Pascal unit
#include <XCollection.hpp>	// Pascal unit
#include <System.SysUtils.hpp>	// Pascal unit
#include <BaseClasses.hpp>	// Pascal unit
#include <GLScene.hpp>	// Pascal unit
#include <GLManager.hpp>	// Pascal unit
#include <GLCrossPlatform.hpp>	// Pascal unit
#include <GLCoordinates.hpp>	// Pascal unit
#include <GLObjects.hpp>	// Pascal unit
#include <GLGeomObjects.hpp>	// Pascal unit
#include <GLVectorFileObjects.hpp>	// Pascal unit
#include <System.Math.hpp>	// Pascal unit
#include <GLColor.hpp>	// Pascal unit
#include <GeometryBB.hpp>	// Pascal unit
#include <VectorTypes.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Glngdmanager
{
//-- type declarations -------------------------------------------------------
typedef float NGDFloat;

typedef float *PNGDFloat;

struct DECLSPEC_DRECORD THeightField
{
private:
	typedef System::DynamicArray<System::Word> _THeightField__1;
	
	
public:
	_THeightField__1 heightArray;
	int width;
	int depth;
	bool gridDiagonals;
	float widthDepthScale;
	float heightScale;
};


enum TNGDSolverModels : unsigned char { smExact, smLinear1, smLinear2, smLinear3, smLinear4, smLinear5, smLinear6, smLinear7, smLinear8, smLinear9 };

enum TNGDFrictionModels : unsigned char { fmExact, fmAdaptive };

enum TNGDPickedActions : unsigned char { paAttach, paMove, paDetach };

enum TNGDManagerDebug : unsigned char { mdShowGeometry, mdShowAABB, mdShowCenterOfMass, mdShowContact, mdShowJoint, mdShowForce, mdShowAppliedForce, mdShowAppliedVelocity };

typedef System::Set<TNGDManagerDebug, TNGDManagerDebug::mdShowGeometry, TNGDManagerDebug::mdShowAppliedVelocity>  TNGDManagerDebugs;

enum TNGDNewtonCollisions : unsigned char { nc_Primitive, nc_Convex, nc_BBox, nc_BSphere, nc_Tree, nc_Mesh, nc_Null, nc_HeightField, nc_NGDFile };

enum TNGDNewtonJoints : unsigned char { nj_BallAndSocket, nj_Hinge, nj_Slider, nj_Corkscrew, nj_Universal, nj_CustomBallAndSocket, nj_CustomHinge, nj_CustomSlider, nj_UpVector, nj_KinematicController };

class DELPHICLASS TGLNGDBehaviourList;
class DELPHICLASS TGLNGDBehaviour;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLNGDBehaviourList : public System::Classes::TList
{
	typedef System::Classes::TList inherited;
	
public:
	TGLNGDBehaviour* operator[](int index) { return ItemsBehav[index]; }
	
protected:
	TGLNGDBehaviour* __fastcall GetBehav(int index);
	void __fastcall PutBehav(int index, TGLNGDBehaviour* Item);
	
public:
	__property TGLNGDBehaviour* ItemsBehav[int index] = {read=GetBehav, write=PutBehav/*, default*/};
public:
	/* TList.Destroy */ inline __fastcall virtual ~TGLNGDBehaviourList(void) { }
	
public:
	/* TObject.Create */ inline __fastcall TGLNGDBehaviourList(void) : System::Classes::TList() { }
	
};

#pragma pack(pop)

typedef void __fastcall (__closure *TCollisionIteratorEvent)(const void * userData, int vertexCount, const PNGDFloat cfaceArray, int faceId);

typedef void __fastcall (__closure *TApplyForceAndTorqueEvent)(const Newtonimport::PNewtonBody cbody, float timestep, int threadIndex);

typedef void __fastcall (__closure *TSetTransformEvent)(const Newtonimport::PNewtonBody cbody, const PNGDFloat cmatrix, int threadIndex);

typedef void __fastcall (__closure *TSerializeEvent)(void * serializeHandle, const void * cbuffer, unsigned size);

typedef void __fastcall (__closure *TDeSerializeEvent)(void * serializeHandle, void * buffer, unsigned size);

typedef bool __fastcall (__closure *TAABBOverlapEvent)(const Newtonimport::PNewtonMaterial cmaterial, const Newtonimport::PNewtonBody cbody0, const Newtonimport::PNewtonBody cbody1, int threadIndex);

typedef void __fastcall (__closure *TContactProcessEvent)(const Newtonimport::PNewtonJoint ccontact, float timestep, int threadIndex);

class DELPHICLASS TNGDDebugOption;
class DELPHICLASS TGLNGDManager;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TNGDDebugOption : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
private:
	TGLNGDManager* FManager;
	Glcolor::TGLColor* FGeomColorDyn;
	Glcolor::TGLColor* FGeomColorStat;
	Glcolor::TGLColor* FAABBColor;
	Glcolor::TGLColor* FAABBColorSleep;
	Glcolor::TGLColor* FCenterOfMassColor;
	Glcolor::TGLColor* FContactColor;
	Glcolor::TGLColor* FJointAxisColor;
	Glcolor::TGLColor* FJointPivotColor;
	Glcolor::TGLColor* FForceColor;
	Glcolor::TGLColor* FAppliedForceColor;
	Glcolor::TGLColor* FAppliedVelocityColor;
	Glcolor::TGLColor* FCustomColor;
	float FDotAxisSize;
	TNGDManagerDebugs FNGDManagerDebugs;
	void __fastcall SetNGDManagerDebugs(const TNGDManagerDebugs Value);
	void __fastcall SetDotAxisSize(const float Value);
	bool __fastcall StoredDotAxis(void);
	
public:
	__fastcall TNGDDebugOption(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TNGDDebugOption(void);
	
__published:
	__property Glcolor::TGLColor* GeomColorDyn = {read=FGeomColorDyn, write=FGeomColorDyn};
	__property Glcolor::TGLColor* GeomColorStat = {read=FGeomColorStat, write=FGeomColorStat};
	__property Glcolor::TGLColor* AABBColor = {read=FAABBColor, write=FAABBColor};
	__property Glcolor::TGLColor* AABBColorSleep = {read=FAABBColorSleep, write=FAABBColorSleep};
	__property Glcolor::TGLColor* CenterOfMassColor = {read=FCenterOfMassColor, write=FCenterOfMassColor};
	__property Glcolor::TGLColor* ContactColor = {read=FContactColor, write=FContactColor};
	__property Glcolor::TGLColor* JointAxisColor = {read=FJointAxisColor, write=FJointAxisColor};
	__property Glcolor::TGLColor* JointPivotColor = {read=FJointPivotColor, write=FJointPivotColor};
	__property Glcolor::TGLColor* ForceColor = {read=FForceColor, write=FForceColor};
	__property Glcolor::TGLColor* AppliedForceColor = {read=FAppliedForceColor, write=FAppliedForceColor};
	__property Glcolor::TGLColor* AppliedVelocityColor = {read=FAppliedVelocityColor, write=FAppliedVelocityColor};
	__property Glcolor::TGLColor* CustomColor = {read=FCustomColor, write=FCustomColor};
	__property TNGDManagerDebugs NGDManagerDebugs = {read=FNGDManagerDebugs, write=SetNGDManagerDebugs, default=0};
	__property float DotAxisSize = {read=FDotAxisSize, write=SetDotAxisSize, stored=StoredDotAxis};
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLNGDManager : public System::Classes::TComponent
{
	typedef System::Classes::TComponent inherited;
	
private:
	bool FVisible;
	bool FVisibleAtRunTime;
	int FDllVersion;
	TNGDSolverModels FSolverModel;
	TNGDFrictionModels FFrictionModel;
	int FMinimumFrameRate;
	Glcoordinates::TGLCoordinates3* FWorldSizeMin;
	Glcoordinates::TGLCoordinates3* FWorldSizeMax;
	int FThreadCount;
	Glcoordinates::TGLCoordinates3* FGravity;
	System::Classes::TCollection* FNewtonSurfaceItem;
	System::Classes::TOwnedCollection* FNewtonSurfacePair;
	System::Classes::TOwnedCollection* FNewtonJointGroup;
	TNGDDebugOption* FNGDDebugOption;
	Globjects::TGLLines* FGLLines;
	
private:
	void * *FNewtonWorld;
	TGLNGDBehaviourList* FNGDBehaviours;
	Glcolor::TGLColor* FCurrentColor;
	
protected:
	virtual void __fastcall Loaded(void);
	void __fastcall SetVisible(const bool Value);
	void __fastcall SetVisibleAtRunTime(const bool Value);
	void __fastcall SetSolverModel(const TNGDSolverModels Value);
	void __fastcall SetFrictionModel(const TNGDFrictionModels Value);
	void __fastcall SetMinimumFrameRate(const int Value);
	void __fastcall SetThreadCount(const int Value);
	void __fastcall SetGLLines(Globjects::TGLLines* const Value);
	int __fastcall GetBodyCount(void);
	int __fastcall GetConstraintCount(void);
	void __fastcall AddNode(Glcoordinates::TGLCustomCoordinates* const coords)/* overload */;
	void __fastcall AddNode(const float X, const float Y, const float Z)/* overload */;
	void __fastcall AddNode(const Vectortypes::TVector4f &Value)/* overload */;
	void __fastcall AddNode(const Vectortypes::TVector3f &Value)/* overload */;
	void __fastcall RebuildAllMaterial(void);
	void __fastcall RebuildAllJoint(System::TObject* Sender);
	void __fastcall NotifyWorldSizeChange(System::TObject* Sender);
	void __fastcall NotifyChange(System::TObject* Sender);
	
public:
	__fastcall virtual TGLNGDManager(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TGLNGDManager(void);
	void __fastcall Step(float deltatime);
	
__published:
	__property bool Visible = {read=FVisible, write=SetVisible, default=1};
	__property bool VisibleAtRunTime = {read=FVisibleAtRunTime, write=SetVisibleAtRunTime, default=0};
	__property TNGDSolverModels SolverModel = {read=FSolverModel, write=SetSolverModel, default=0};
	__property TNGDFrictionModels FrictionModel = {read=FFrictionModel, write=SetFrictionModel, default=0};
	__property int MinimumFrameRate = {read=FMinimumFrameRate, write=SetMinimumFrameRate, default=60};
	__property int ThreadCount = {read=FThreadCount, write=SetThreadCount, default=1};
	__property int DllVersion = {read=FDllVersion, nodefault};
	__property int NewtonBodyCount = {read=GetBodyCount, nodefault};
	__property int NewtonConstraintCount = {read=GetConstraintCount, nodefault};
	__property Glcoordinates::TGLCoordinates3* Gravity = {read=FGravity, write=FGravity};
	__property Glcoordinates::TGLCoordinates3* WorldSizeMin = {read=FWorldSizeMin, write=FWorldSizeMin};
	__property Glcoordinates::TGLCoordinates3* WorldSizeMax = {read=FWorldSizeMax, write=FWorldSizeMax};
	__property System::Classes::TCollection* NewtonSurfaceItem = {read=FNewtonSurfaceItem, write=FNewtonSurfaceItem};
	__property System::Classes::TOwnedCollection* NewtonSurfacePair = {read=FNewtonSurfacePair, write=FNewtonSurfacePair};
	__property TNGDDebugOption* DebugOption = {read=FNGDDebugOption, write=FNGDDebugOption};
	__property Globjects::TGLLines* Line = {read=FGLLines, write=SetGLLines};
	__property System::Classes::TOwnedCollection* NewtonJoint = {read=FNewtonJointGroup, write=FNewtonJointGroup};
};

#pragma pack(pop)

class DELPHICLASS TNGDSurfaceItem;
class PASCALIMPLEMENTATION TGLNGDBehaviour : public Glscene::TGLBehaviour
{
	typedef Glscene::TGLBehaviour inherited;
	
private:
	TGLNGDManager* FManager;
	System::UnicodeString FManagerName;
	bool FInitialized;
	void * *FNewtonBody;
	void * *FCollision;
	Vectortypes::TMatrix4f FNewtonBodyMatrix;
	bool FContinuousCollisionMode;
	TNGDNewtonCollisions FNGDNewtonCollisions;
	TCollisionIteratorEvent FCollisionIteratorEvent;
	Glscene::TGLBaseSceneObject* FOwnerBaseSceneObject;
	bool FTreeCollisionOptimize;
	float FConvexCollisionTolerance;
	System::UnicodeString FFileCollision;
	TNGDSurfaceItem* FNGDSurfaceItem;
	THeightField FHeightFieldOptions;
	
protected:
	virtual void __fastcall Initialize(void);
	virtual void __fastcall Finalize(void);
	virtual void __fastcall WriteToFiler(System::Classes::TWriter* writer);
	virtual void __fastcall ReadFromFiler(System::Classes::TReader* reader);
	DYNAMIC void __fastcall Loaded(void);
	void __fastcall SetManager(TGLNGDManager* Value);
	void __fastcall SetNewtonBodyMatrix(const Vectortypes::TMatrix4f &Value);
	void __fastcall SetContinuousCollisionMode(const bool Value);
	Vectortypes::TMatrix4f __fastcall GetNewtonBodyMatrix(void);
	Geometrybb::TAABB __fastcall GetNewtonBodyAABB(void);
	virtual void __fastcall UpdCollision(void);
	virtual void __fastcall Render(void);
	void __fastcall SetNGDNewtonCollisions(const TNGDNewtonCollisions Value);
	void __fastcall SetNGDSurfaceItem(TNGDSurfaceItem* const Value);
	void __fastcall SetHeightFieldOptions(const THeightField &Value);
	Newtonimport::PNewtonCollision __fastcall GetPrimitiveCollision(void);
	Newtonimport::PNewtonCollision __fastcall GetConvexCollision(void);
	Newtonimport::PNewtonCollision __fastcall GetBBoxCollision(void);
	Newtonimport::PNewtonCollision __fastcall GetBSphereCollision(void);
	Newtonimport::PNewtonCollision __fastcall GetTreeCollision(void);
	Newtonimport::PNewtonCollision __fastcall GetMeshCollision(void);
	Newtonimport::PNewtonCollision __fastcall GetNullCollision(void);
	Newtonimport::PNewtonCollision __fastcall GetHeightFieldCollision(void);
	Newtonimport::PNewtonCollision __fastcall GetNGDFileCollision(void);
	bool __fastcall StoredTolerance(void);
	void __fastcall OnCollisionIteratorEvent(const void * userData, int vertexCount, const PNGDFloat cfaceArray, int faceId);
	static void __cdecl NewtonCollisionIterator(const void * userData, int vertexCount, const PNGDFloat faceArray, int faceId);
	static void __cdecl NewtonSerialize(void * serializeHandle, const void * buffer, unsigned size);
	static void __cdecl NewtonDeserialize(void * serializeHandle, void * buffer, unsigned size);
	
public:
	__fastcall virtual TGLNGDBehaviour(Xcollection::TXCollection* AOwner);
	__fastcall virtual ~TGLNGDBehaviour(void);
	void __fastcall Reinitialize(void);
	__property bool Initialized = {read=FInitialized, nodefault};
	__classmethod virtual bool __fastcall UniqueItem();
	__property Vectortypes::TMatrix4f NewtonBodyMatrix = {read=GetNewtonBodyMatrix, write=SetNewtonBodyMatrix};
	__property Geometrybb::TAABB NewtonBodyAABB = {read=GetNewtonBodyAABB};
	void __fastcall Serialize(System::UnicodeString filename);
	void __fastcall DeSerialize(System::UnicodeString filename);
	__property THeightField HeightFieldOptions = {read=FHeightFieldOptions, write=SetHeightFieldOptions};
	
__published:
	__property TGLNGDManager* Manager = {read=FManager, write=SetManager};
	__property bool ContinuousCollisionMode = {read=FContinuousCollisionMode, write=SetContinuousCollisionMode, default=0};
	__property TNGDNewtonCollisions NGDNewtonCollisions = {read=FNGDNewtonCollisions, write=SetNGDNewtonCollisions, default=0};
	__property bool TreeCollisionOptimize = {read=FTreeCollisionOptimize, write=FTreeCollisionOptimize, default=1};
	__property float ConvexCollisionTolerance = {read=FConvexCollisionTolerance, write=FConvexCollisionTolerance, stored=StoredTolerance};
	__property System::UnicodeString FileCollision = {read=FFileCollision, write=FFileCollision};
	__property TNGDSurfaceItem* NGDSurfaceItem = {read=FNGDSurfaceItem, write=SetNGDSurfaceItem};
};


class DELPHICLASS TGLNGDDynamic;
class PASCALIMPLEMENTATION TGLNGDDynamic : public TGLNGDBehaviour
{
	typedef TGLNGDBehaviour inherited;
	
private:
	Glcoordinates::TGLCoordinates3* FAABBmin;
	Glcoordinates::TGLCoordinates3* FAABBmax;
	Glcoordinates::TGLCoordinates3* FForce;
	Glcoordinates::TGLCoordinates3* FTorque;
	Glcoordinates::TGLCoordinates3* FCenterOfMass;
	bool FAutoSleep;
	float FLinearDamping;
	Glcoordinates::TGLCoordinates3* FAngularDamping;
	float FDensity;
	bool FUseGravity;
	float FNullCollisionVolume;
	TApplyForceAndTorqueEvent FApplyForceAndTorqueEvent;
	TSetTransformEvent FSetTransformEvent;
	TApplyForceAndTorqueEvent FCustomForceAndTorqueEvent;
	float FVolume;
	float FMass;
	Glcoordinates::TGLCoordinates3* FAppliedForce;
	Glcoordinates::TGLCoordinates3* FAppliedTorque;
	bool __fastcall StoredDensity(void);
	bool __fastcall StoredLinearDamping(void);
	bool __fastcall StoredNullCollisionVolume(void);
	
protected:
	Vectortypes::TVector3f __fastcall GetVelocity(void);
	void __fastcall SetVelocity(const Vectortypes::TVector3f &Velocity);
	Vectortypes::TVector3f __fastcall GetOmega(void);
	void __fastcall SetOmega(const Vectortypes::TVector3f &Omega);
	void __fastcall SetAutoSleep(const bool Value);
	void __fastcall SetLinearDamping(const float Value);
	virtual void __fastcall SetDensity(const float Value);
	virtual void __fastcall Initialize(void);
	virtual void __fastcall Finalize(void);
	virtual void __fastcall WriteToFiler(System::Classes::TWriter* writer);
	virtual void __fastcall ReadFromFiler(System::Classes::TReader* reader);
	DYNAMIC void __fastcall Loaded(void);
	virtual void __fastcall Render(void);
	void __fastcall NotifyCenterOfMassChange(System::TObject* Sender);
	void __fastcall NotifyAngularDampingChange(System::TObject* Sender);
	void __fastcall OnApplyForceAndTorqueEvent(const Newtonimport::PNewtonBody cbody, float timestep, int threadIndex);
	void __fastcall OnSetTransformEvent(const Newtonimport::PNewtonBody cbody, const PNGDFloat cmatrix, int threadIndex);
	static void __cdecl NewtonApplyForceAndTorque(const Newtonimport::PNewtonBody body, float timestep, int threadIndex);
	static void __cdecl NewtonSetTransform(const Newtonimport::PNewtonBody body, const PNGDFloat matrix, int threadIndex);
	
public:
	__fastcall virtual TGLNGDDynamic(Xcollection::TXCollection* AOwner);
	__fastcall virtual ~TGLNGDDynamic(void);
	void __fastcall AddImpulse(const Vectortypes::TVector4f &veloc, const Vectortypes::TVector4f &pointposit);
	__classmethod virtual System::UnicodeString __fastcall FriendlyName();
	__property TApplyForceAndTorqueEvent CustomForceAndTorqueEvent = {read=FCustomForceAndTorqueEvent, write=FCustomForceAndTorqueEvent};
	__property Vectortypes::TVector3f Velocity = {read=GetVelocity, write=SetVelocity};
	__property Vectortypes::TVector3f Omega = {read=GetOmega, write=SetOmega};
	
__published:
	__property Glcoordinates::TGLCoordinates3* Force = {read=FForce, write=FForce};
	__property Glcoordinates::TGLCoordinates3* Torque = {read=FTorque, write=FTorque};
	__property Glcoordinates::TGLCoordinates3* CenterOfMass = {read=FCenterOfMass, write=FCenterOfMass};
	__property bool AutoSleep = {read=FAutoSleep, write=SetAutoSleep, default=1};
	__property float LinearDamping = {read=FLinearDamping, write=SetLinearDamping, stored=StoredLinearDamping};
	__property Glcoordinates::TGLCoordinates3* AngularDamping = {read=FAngularDamping, write=FAngularDamping};
	__property float Density = {read=FDensity, write=SetDensity, stored=StoredDensity};
	__property bool UseGravity = {read=FUseGravity, write=FUseGravity, default=1};
	__property float NullCollisionVolume = {read=FNullCollisionVolume, write=FNullCollisionVolume, stored=StoredNullCollisionVolume};
	__property Glcoordinates::TGLCoordinates3* AppliedForce = {read=FAppliedForce};
	__property Glcoordinates::TGLCoordinates3* AppliedTorque = {read=FAppliedTorque};
	__property float Volume = {read=FVolume};
	__property float Mass = {read=FMass};
};


class DELPHICLASS TGLNGDStatic;
class PASCALIMPLEMENTATION TGLNGDStatic : public TGLNGDBehaviour
{
	typedef TGLNGDBehaviour inherited;
	
protected:
	virtual void __fastcall Render(void);
	
public:
	__classmethod virtual System::UnicodeString __fastcall FriendlyName();
public:
	/* TGLNGDBehaviour.Create */ inline __fastcall virtual TGLNGDStatic(Xcollection::TXCollection* AOwner) : TGLNGDBehaviour(AOwner) { }
	/* TGLNGDBehaviour.Destroy */ inline __fastcall virtual ~TGLNGDStatic(void) { }
	
};


#pragma pack(push,4)
class PASCALIMPLEMENTATION TNGDSurfaceItem : public System::Classes::TCollectionItem
{
	typedef System::Classes::TCollectionItem inherited;
	
private:
	System::UnicodeString FDisplayName;
	
protected:
	virtual System::UnicodeString __fastcall GetDisplayName(void);
	virtual void __fastcall SetDisplayName(const System::UnicodeString Value);
	
__published:
	__property DisplayName = {default=0};
	__property ID;
public:
	/* TCollectionItem.Create */ inline __fastcall virtual TNGDSurfaceItem(System::Classes::TCollection* Collection) : System::Classes::TCollectionItem(Collection) { }
	/* TCollectionItem.Destroy */ inline __fastcall virtual ~TNGDSurfaceItem(void) { }
	
};

#pragma pack(pop)

class DELPHICLASS TNGDSurfacePair;
class PASCALIMPLEMENTATION TNGDSurfacePair : public System::Classes::TCollectionItem
{
	typedef System::Classes::TCollectionItem inherited;
	
private:
	TGLNGDManager* FManager;
	TNGDSurfaceItem* FNGDSurfaceItem1;
	TNGDSurfaceItem* FNGDSurfaceItem2;
	TAABBOverlapEvent FAABBOverlapEvent;
	TContactProcessEvent FContactProcessEvent;
	float FSoftness;
	float FElasticity;
	bool FCollidable;
	float FStaticFriction;
	float FKineticFriction;
	bool FContinuousCollisionMode;
	bool FThickness;
	void __fastcall SetCollidable(const bool Value);
	void __fastcall SetElasticity(const float Value);
	void __fastcall SetKineticFriction(const float Value);
	void __fastcall SetSoftness(const float Value);
	void __fastcall SetStaticFriction(const float Value);
	void __fastcall SetContinuousCollisionMode(const bool Value);
	void __fastcall SetThickness(const bool Value);
	bool __fastcall StoredElasticity(void);
	bool __fastcall StoredKineticFriction(void);
	bool __fastcall StoredSoftness(void);
	bool __fastcall StoredStaticFriction(void);
	
private:
	static int __cdecl NewtonAABBOverlap(const Newtonimport::PNewtonMaterial material, const Newtonimport::PNewtonBody body0, const Newtonimport::PNewtonBody body1, int threadIndex);
	static void __cdecl NewtonContactsProcess(const Newtonimport::PNewtonJoint contact, float timestep, int threadIndex);
	bool __fastcall OnNewtonAABBOverlapEvent(const Newtonimport::PNewtonMaterial cmaterial, const Newtonimport::PNewtonBody cbody0, const Newtonimport::PNewtonBody cbody1, int threadIndex);
	void __fastcall OnNewtonContactsProcessEvent(const Newtonimport::PNewtonJoint ccontact, float timestep, int threadIndex);
	
public:
	__fastcall virtual TNGDSurfacePair(System::Classes::TCollection* Collection);
	void __fastcall SetMaterialItems(TNGDSurfaceItem* const item1, TNGDSurfaceItem* const item2);
	__property TNGDSurfaceItem* NGDSurfaceItem1 = {read=FNGDSurfaceItem1};
	__property TNGDSurfaceItem* NGDSurfaceItem2 = {read=FNGDSurfaceItem2};
	
__published:
	__property float Softness = {read=FSoftness, write=SetSoftness, stored=StoredSoftness};
	__property float Elasticity = {read=FElasticity, write=SetElasticity, stored=StoredElasticity};
	__property bool Collidable = {read=FCollidable, write=SetCollidable, default=1};
	__property float StaticFriction = {read=FStaticFriction, write=SetStaticFriction, stored=StoredStaticFriction};
	__property float KineticFriction = {read=FKineticFriction, write=SetKineticFriction, stored=StoredKineticFriction};
	__property bool ContinuousCollisionMode = {read=FContinuousCollisionMode, write=SetContinuousCollisionMode, default=0};
	__property bool Thickness = {read=FThickness, write=SetThickness, default=0};
	__property TContactProcessEvent ContactProcessEvent = {read=FContactProcessEvent, write=FContactProcessEvent};
	__property TAABBOverlapEvent AABBOverlapEvent = {read=FAABBOverlapEvent, write=FAABBOverlapEvent};
public:
	/* TCollectionItem.Destroy */ inline __fastcall virtual ~TNGDSurfacePair(void) { }
	
};


class DELPHICLASS TNGDJointPivot;
class DELPHICLASS TNGDJoint;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TNGDJointPivot : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
private:
	TGLNGDManager* FManager;
	Glcoordinates::TGLCoordinates3* FPivotPoint;
	TNGDJoint* FOuter;
	
public:
	__fastcall virtual TNGDJointPivot(System::Classes::TComponent* AOwner, TNGDJoint* aOuter);
	__fastcall virtual ~TNGDJointPivot(void);
	
__published:
	__property Glcoordinates::TGLCoordinates3* PivotPoint = {read=FPivotPoint, write=FPivotPoint};
};

#pragma pack(pop)

class DELPHICLASS TNGDJointPin;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TNGDJointPin : public TNGDJointPivot
{
	typedef TNGDJointPivot inherited;
	
private:
	Glcoordinates::TGLCoordinates3* FPinDirection;
	
public:
	__fastcall virtual TNGDJointPin(System::Classes::TComponent* AOwner, TNGDJoint* aOuter);
	__fastcall virtual ~TNGDJointPin(void);
	
__published:
	__property Glcoordinates::TGLCoordinates3* PinDirection = {read=FPinDirection, write=FPinDirection};
};

#pragma pack(pop)

class DELPHICLASS TNGDJointPin2;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TNGDJointPin2 : public TNGDJointPin
{
	typedef TNGDJointPin inherited;
	
private:
	Glcoordinates::TGLCoordinates3* FPinDirection2;
	
public:
	__fastcall virtual TNGDJointPin2(System::Classes::TComponent* AOwner, TNGDJoint* aOuter);
	__fastcall virtual ~TNGDJointPin2(void);
	
__published:
	__property Glcoordinates::TGLCoordinates3* PinDirection2 = {read=FPinDirection2, write=FPinDirection2};
};

#pragma pack(pop)

class DELPHICLASS TNGDJointBallAndSocket;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TNGDJointBallAndSocket : public TNGDJointPivot
{
	typedef TNGDJointPivot inherited;
	
private:
	float FConeAngle;
	float FMinTwistAngle;
	float FMaxTwistAngle;
	void __fastcall SetConeAngle(const float Value);
	void __fastcall SetMaxTwistAngle(const float Value);
	void __fastcall SetMinTwistAngle(const float Value);
	bool __fastcall StoredMaxTwistAngle(void);
	bool __fastcall StoredMinTwistAngle(void);
	bool __fastcall StoredConeAngle(void);
	
public:
	__fastcall virtual TNGDJointBallAndSocket(System::Classes::TComponent* AOwner, TNGDJoint* aOuter);
	
__published:
	__property float ConeAngle = {read=FConeAngle, write=SetConeAngle, stored=StoredConeAngle};
	__property float MinTwistAngle = {read=FMinTwistAngle, write=SetMinTwistAngle, stored=StoredMinTwistAngle};
	__property float MaxTwistAngle = {read=FMaxTwistAngle, write=SetMaxTwistAngle, stored=StoredMaxTwistAngle};
public:
	/* TNGDJointPivot.Destroy */ inline __fastcall virtual ~TNGDJointBallAndSocket(void) { }
	
};

#pragma pack(pop)

class DELPHICLASS TNGDJointHinge;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TNGDJointHinge : public TNGDJointPin
{
	typedef TNGDJointPin inherited;
	
private:
	float FMinAngle;
	float FMaxAngle;
	void __fastcall SetMaxAngle(const float Value);
	void __fastcall SetMinAngle(const float Value);
	bool __fastcall StoredMaxAngle(void);
	bool __fastcall StoredMinAngle(void);
	
public:
	__fastcall virtual TNGDJointHinge(System::Classes::TComponent* AOwner, TNGDJoint* aOuter);
	
__published:
	__property float MinAngle = {read=FMinAngle, write=SetMinAngle, stored=StoredMinAngle};
	__property float MaxAngle = {read=FMaxAngle, write=SetMaxAngle, stored=StoredMaxAngle};
public:
	/* TNGDJointPin.Destroy */ inline __fastcall virtual ~TNGDJointHinge(void) { }
	
};

#pragma pack(pop)

class DELPHICLASS TNGDJointSlider;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TNGDJointSlider : public TNGDJointPin
{
	typedef TNGDJointPin inherited;
	
private:
	float FMinDistance;
	float FMaxDistance;
	void __fastcall SetMaxDistance(const float Value);
	void __fastcall SetMinDistance(const float Value);
	bool __fastcall StoredMaxDistance(void);
	bool __fastcall StoredMinDistance(void);
	
public:
	__fastcall virtual TNGDJointSlider(System::Classes::TComponent* AOwner, TNGDJoint* aOuter);
	
__published:
	__property float MinDistance = {read=FMinDistance, write=SetMinDistance, stored=StoredMinDistance};
	__property float MaxDistance = {read=FMaxDistance, write=SetMaxDistance, stored=StoredMaxDistance};
public:
	/* TNGDJointPin.Destroy */ inline __fastcall virtual ~TNGDJointSlider(void) { }
	
};

#pragma pack(pop)

class DELPHICLASS TNGDJointKinematicController;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TNGDJointKinematicController : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
private:
	bool FPickModeLinear;
	float FLinearFriction;
	float FAngularFriction;
	bool __fastcall StoredAngularFriction(void);
	bool __fastcall StoredLinearFriction(void);
	
public:
	__fastcall TNGDJointKinematicController(void);
	
__published:
	__property bool PickModeLinear = {read=FPickModeLinear, write=FPickModeLinear, default=0};
	__property float LinearFriction = {read=FLinearFriction, write=FLinearFriction, stored=StoredLinearFriction};
	__property float AngularFriction = {read=FAngularFriction, write=FAngularFriction, stored=StoredAngularFriction};
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TNGDJointKinematicController(void) { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TNGDJoint : public System::Classes::TCollectionItem
{
	typedef System::Classes::TCollectionItem inherited;
	
private:
	TGLNGDManager* FManager;
	Glscene::TGLBaseSceneObject* FParentObject;
	TNGDNewtonJoints FJointType;
	float FStiffness;
	Glscene::TGLBaseSceneObject* FChildObject;
	bool FCollisionState;
	void * *FNewtonJoint;
	void * *FNewtonUserJoint;
	Glcoordinates::TGLCoordinates3* FUPVectorDirection;
	TNGDJointPivot* FBallAndSocketOptions;
	TNGDJointPin* FHingeOptions;
	TNGDJointPin* FSliderOptions;
	TNGDJointPin* FCorkscrewOptions;
	TNGDJointPin2* FUniversalOptions;
	TNGDJointBallAndSocket* FCustomBallAndSocketOptions;
	TNGDJointHinge* FCustomHingeOptions;
	TNGDJointSlider* FCustomSliderOptions;
	TNGDJointKinematicController* FKinematicOptions;
	void __fastcall SetJointType(const TNGDNewtonJoints Value);
	void __fastcall SetChildObject(Glscene::TGLBaseSceneObject* const Value);
	void __fastcall SetCollisionState(const bool Value);
	void __fastcall SetParentObject(Glscene::TGLBaseSceneObject* const Value);
	void __fastcall SetStiffness(const float Value);
	void __fastcall Render(void);
	bool __fastcall StoredStiffness(void);
	void __fastcall DestroyNewtonData(void);
	
public:
	__fastcall virtual TNGDJoint(System::Classes::TCollection* Collection);
	__fastcall virtual ~TNGDJoint(void);
	void __fastcall KinematicControllerPick(const Vectortypes::TVector4f &pickpoint, TNGDPickedActions PickedActions);
	
__published:
	__property TNGDJointPivot* BallAndSocketOptions = {read=FBallAndSocketOptions, write=FBallAndSocketOptions};
	__property TNGDJointPin* HingeOptions = {read=FHingeOptions, write=FHingeOptions};
	__property TNGDJointPin* SliderOptions = {read=FSliderOptions, write=FSliderOptions};
	__property TNGDJointPin* CorkscrewOptions = {read=FCorkscrewOptions, write=FCorkscrewOptions};
	__property TNGDJointPin2* UniversalOptions = {read=FUniversalOptions, write=FUniversalOptions};
	__property TNGDJointBallAndSocket* CustomBallAndSocketOptions = {read=FCustomBallAndSocketOptions, write=FCustomBallAndSocketOptions};
	__property TNGDJointHinge* CustomHingeOptions = {read=FCustomHingeOptions, write=FCustomHingeOptions};
	__property TNGDJointSlider* CustomSliderOptions = {read=FCustomSliderOptions, write=FCustomSliderOptions};
	__property TNGDJointKinematicController* KinematicControllerOptions = {read=FKinematicOptions, write=FKinematicOptions};
	__property TNGDNewtonJoints JointType = {read=FJointType, write=SetJointType, nodefault};
	__property Glscene::TGLBaseSceneObject* ParentObject = {read=FParentObject, write=SetParentObject};
	__property Glscene::TGLBaseSceneObject* ChildObject = {read=FChildObject, write=SetChildObject};
	__property bool CollisionState = {read=FCollisionState, write=SetCollisionState, default=0};
	__property float Stiffness = {read=FStiffness, write=SetStiffness, stored=StoredStiffness};
	__property Glcoordinates::TGLCoordinates3* UPVectorDirection = {read=FUPVectorDirection, write=FUPVectorDirection};
};

#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------
extern PACKAGE TGLNGDStatic* __fastcall GetNGDStatic(Glscene::TGLBaseSceneObject* Obj);
extern PACKAGE TGLNGDStatic* __fastcall GetOrCreateNGDStatic(Glscene::TGLBaseSceneObject* Obj);
extern PACKAGE TGLNGDDynamic* __fastcall GetNGDDynamic(Glscene::TGLBaseSceneObject* Obj);
extern PACKAGE TGLNGDDynamic* __fastcall GetOrCreateNGDDynamic(Glscene::TGLBaseSceneObject* Obj);
extern PACKAGE Newtonimport::PNewtonBody __fastcall GetBodyFromGLSceneObject(Glscene::TGLBaseSceneObject* Obj);
}	/* namespace Glngdmanager */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLNGDMANAGER)
using namespace Glngdmanager;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// GlngdmanagerHPP
