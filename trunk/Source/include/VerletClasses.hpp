// CodeGear C++Builder
// Copyright (c) 1995, 2012 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'VerletClasses.pas' rev: 24.00 (Win32)

#ifndef VerletclassesHPP
#define VerletclassesHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <System.Classes.hpp>	// Pascal unit
#include <GLCrossPlatform.hpp>	// Pascal unit
#include <VectorGeometry.hpp>	// Pascal unit
#include <System.SysUtils.hpp>	// Pascal unit
#include <VectorLists.hpp>	// Pascal unit
#include <SpatialPartitioning.hpp>	// Pascal unit
#include <GeometryBB.hpp>	// Pascal unit
#include <PersistentClasses.hpp>	// Pascal unit
#include <VectorTypes.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Verletclasses
{
//-- type declarations -------------------------------------------------------
#pragma pack(push,1)
struct DECLSPEC_DRECORD TVerletProgressTimes
{
public:
	double deltaTime;
	double newTime;
	float sqrDeltaTime;
	float invSqrDeltaTime;
};
#pragma pack(pop)


class DELPHICLASS TVerletNode;
class DELPHICLASS TVerletWorld;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TVerletNode : public Spatialpartitioning::TSpacePartitionLeaf
{
	typedef Spatialpartitioning::TSpacePartitionLeaf inherited;
	
private:
	Vectortypes::TVector3f FForce;
	TVerletWorld* FOwner;
	float FWeight;
	float FInvWeight;
	float FRadius;
	bool FNailedDown;
	float FFriction;
	int FChangedOnStep;
	Vectortypes::TVector3f __fastcall GetSpeed(void);
	
protected:
	Vectortypes::TVector3f FLocation;
	Vectortypes::TVector3f FOldLocation;
	virtual void __fastcall SetLocation(const Vectortypes::TVector3f &Value);
	void __fastcall SetWeight(const float value);
	virtual void __fastcall AfterProgress(void);
	
public:
	__fastcall virtual TVerletNode(TVerletWorld* const aOwner);
	__fastcall virtual ~TVerletNode(void);
	void __fastcall ApplyFriction(const float friction, const float penetrationDepth, const Vectortypes::TVector3f &surfaceNormal);
	void __fastcall OldApplyFriction(const float friction, const float penetrationDepth);
	virtual void __fastcall Verlet(const TVerletProgressTimes &vpt);
	DYNAMIC void __fastcall Initialize(void);
	float __fastcall DistanceToNode(TVerletNode* const node);
	Vectortypes::TVector3f __fastcall GetMovement(void);
	virtual void __fastcall UpdateCachedAABBAndBSphere(void);
	__property TVerletWorld* Owner = {read=FOwner};
	__property Vectortypes::TVector3f Location = {read=FLocation, write=SetLocation};
	__property Vectortypes::TVector3f OldLocation = {read=FOldLocation, write=FOldLocation};
	__property float Radius = {read=FRadius, write=FRadius};
	__property Vectortypes::TVector3f Force = {read=FForce, write=FForce};
	__property bool NailedDown = {read=FNailedDown, write=FNailedDown, nodefault};
	__property float Weight = {read=FWeight, write=SetWeight};
	__property float InvWeight = {read=FInvWeight};
	__property Vectortypes::TVector3f Speed = {read=GetSpeed};
	__property float Friction = {read=FFriction, write=FFriction};
	__property int ChangedOnStep = {read=FChangedOnStep, nodefault};
public:
	/* TPersistentObject.Create */ inline __fastcall virtual TVerletNode(void) : Spatialpartitioning::TSpacePartitionLeaf() { }
	/* TPersistentObject.CreateFromFiler */ inline __fastcall TVerletNode(Persistentclasses::TVirtualReader* reader) : Spatialpartitioning::TSpacePartitionLeaf(reader) { }
	
};

#pragma pack(pop)

typedef System::TMetaClass* TVerletNodeClass;

class DELPHICLASS TVerletNodeList;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TVerletNodeList : public System::Classes::TList
{
	typedef System::Classes::TList inherited;
	
public:
	TVerletNode* operator[](int i) { return Items[i]; }
	
private:
	TVerletNode* __fastcall GetItems(int i);
	void __fastcall SetItems(int i, TVerletNode* const value);
	
public:
	__property TVerletNode* Items[int i] = {read=GetItems, write=SetItems/*, default*/};
public:
	/* TList.Destroy */ inline __fastcall virtual ~TVerletNodeList(void) { }
	
public:
	/* TObject.Create */ inline __fastcall TVerletNodeList(void) : System::Classes::TList() { }
	
};

#pragma pack(pop)

class DELPHICLASS TVerletConstraint;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TVerletConstraint : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	TVerletWorld* FOwner;
	bool FEnabled;
	int FTag;
	
public:
	__fastcall virtual TVerletConstraint(TVerletWorld* const aOwner);
	__fastcall virtual ~TVerletConstraint(void);
	virtual void __fastcall SatisfyConstraint(const int iteration, const int maxIterations) = 0 ;
	virtual void __fastcall RemoveNode(TVerletNode* const aNode) = 0 ;
	virtual void __fastcall BeforeIterations(void);
	__property TVerletWorld* Owner = {read=FOwner};
	__property bool Enabled = {read=FEnabled, write=FEnabled, nodefault};
	__property int Tag = {read=FTag, write=FTag, nodefault};
};

#pragma pack(pop)

class DELPHICLASS TVerletDualConstraint;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TVerletDualConstraint : public TVerletConstraint
{
	typedef TVerletConstraint inherited;
	
private:
	TVerletNode* FNodeA;
	TVerletNode* FNodeB;
	
public:
	virtual void __fastcall RemoveNode(TVerletNode* const aNode);
	__property TVerletNode* NodeA = {read=FNodeA, write=FNodeA};
	__property TVerletNode* NodeB = {read=FNodeB, write=FNodeB};
public:
	/* TVerletConstraint.Create */ inline __fastcall virtual TVerletDualConstraint(TVerletWorld* const aOwner) : TVerletConstraint(aOwner) { }
	/* TVerletConstraint.Destroy */ inline __fastcall virtual ~TVerletDualConstraint(void) { }
	
};

#pragma pack(pop)

class DELPHICLASS TVerletGroupConstraint;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TVerletGroupConstraint : public TVerletConstraint
{
	typedef TVerletConstraint inherited;
	
private:
	TVerletNodeList* FNodes;
	
public:
	__fastcall virtual TVerletGroupConstraint(TVerletWorld* const aOwner);
	__fastcall virtual ~TVerletGroupConstraint(void);
	virtual void __fastcall RemoveNode(TVerletNode* const aNode);
	__property TVerletNodeList* Nodes = {read=FNodes};
};

#pragma pack(pop)

class DELPHICLASS TVerletEdge;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TVerletEdge : public Spatialpartitioning::TSpacePartitionLeaf
{
	typedef Spatialpartitioning::TSpacePartitionLeaf inherited;
	
private:
	TVerletNode* FNodeA;
	TVerletNode* FNodeB;
	
public:
	virtual void __fastcall UpdateCachedAABBAndBSphere(void);
	__fastcall TVerletEdge(TVerletNode* const aNodeA, TVerletNode* const aNodeB);
	__property TVerletNode* NodeA = {read=FNodeA, write=FNodeA};
	__property TVerletNode* NodeB = {read=FNodeB, write=FNodeB};
public:
	/* TSpacePartitionLeaf.CreateOwned */ inline __fastcall TVerletEdge(Spatialpartitioning::TBaseSpacePartition* SpacePartition) : Spatialpartitioning::TSpacePartitionLeaf(SpacePartition) { }
	/* TSpacePartitionLeaf.Destroy */ inline __fastcall virtual ~TVerletEdge(void) { }
	
public:
	/* TPersistentObject.Create */ inline __fastcall virtual TVerletEdge(void) : Spatialpartitioning::TSpacePartitionLeaf() { }
	/* TPersistentObject.CreateFromFiler */ inline __fastcall TVerletEdge(Persistentclasses::TVirtualReader* reader) : Spatialpartitioning::TSpacePartitionLeaf(reader) { }
	
};

#pragma pack(pop)

class DELPHICLASS TVerletEdgeList;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TVerletEdgeList : public System::Classes::TList
{
	typedef System::Classes::TList inherited;
	
public:
	TVerletEdge* operator[](int i) { return Items[i]; }
	
private:
	TVerletEdge* __fastcall GetItems(int i);
	void __fastcall SetItems(int i, TVerletEdge* const Value);
	
public:
	__property TVerletEdge* Items[int i] = {read=GetItems, write=SetItems/*, default*/};
public:
	/* TList.Destroy */ inline __fastcall virtual ~TVerletEdgeList(void) { }
	
public:
	/* TObject.Create */ inline __fastcall TVerletEdgeList(void) : System::Classes::TList() { }
	
};

#pragma pack(pop)

class DELPHICLASS TVerletGlobalConstraint;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TVerletGlobalConstraint : public TVerletConstraint
{
	typedef TVerletConstraint inherited;
	
private:
	Vectortypes::TVector3f FKickbackForce;
	Vectortypes::TVector3f FKickbackTorque;
	Vectortypes::TVector3f FLocation;
	virtual void __fastcall SetLocation(const Vectortypes::TVector3f &Value);
	
public:
	__fastcall virtual TVerletGlobalConstraint(TVerletWorld* const aOwner);
	__fastcall virtual ~TVerletGlobalConstraint(void);
	virtual void __fastcall RemoveNode(TVerletNode* const aNode);
	virtual void __fastcall BeforeIterations(void);
	virtual void __fastcall SatisfyConstraint(const int iteration, const int maxIterations);
	virtual void __fastcall SatisfyConstraintForNode(TVerletNode* const aNode, const int iteration, const int maxIterations) = 0 ;
	virtual void __fastcall SatisfyConstraintForEdge(TVerletEdge* const aEdge, const int iteration, const int maxIterations);
	__property Vectortypes::TVector3f Location = {read=FLocation, write=SetLocation};
	__property Vectortypes::TVector3f KickbackForce = {read=FKickbackForce, write=FKickbackForce};
	__property Vectortypes::TVector3f KickbackTorque = {read=FKickbackTorque, write=FKickbackTorque};
	void __fastcall AddKickbackForceAt(const Vectortypes::TVector3f &Pos, const Vectortypes::TVector3f &Force);
	Vectortypes::TVector3f __fastcall TranslateKickbackTorque(const Vectortypes::TVector3f &TorqueCenter);
};

#pragma pack(pop)

class DELPHICLASS TVerletGlobalFrictionConstraint;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TVerletGlobalFrictionConstraint : public TVerletGlobalConstraint
{
	typedef TVerletGlobalConstraint inherited;
	
private:
	float FFrictionRatio;
	
public:
	__fastcall virtual TVerletGlobalFrictionConstraint(TVerletWorld* const aOwner);
	__property float FrictionRatio = {read=FFrictionRatio, write=FFrictionRatio};
public:
	/* TVerletGlobalConstraint.Destroy */ inline __fastcall virtual ~TVerletGlobalFrictionConstraint(void) { }
	
};

#pragma pack(pop)

class DELPHICLASS TVerletGlobalFrictionConstraintSP;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TVerletGlobalFrictionConstraintSP : public TVerletGlobalFrictionConstraint
{
	typedef TVerletGlobalFrictionConstraint inherited;
	
public:
	virtual void __fastcall SatisfyConstraint(const int iteration, const int maxIterations);
	virtual void __fastcall PerformSpaceQuery(void) = 0 ;
public:
	/* TVerletGlobalFrictionConstraint.Create */ inline __fastcall virtual TVerletGlobalFrictionConstraintSP(TVerletWorld* const aOwner) : TVerletGlobalFrictionConstraint(aOwner) { }
	
public:
	/* TVerletGlobalConstraint.Destroy */ inline __fastcall virtual ~TVerletGlobalFrictionConstraintSP(void) { }
	
};

#pragma pack(pop)

class DELPHICLASS TVerletGlobalFrictionConstraintSphere;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TVerletGlobalFrictionConstraintSphere : public TVerletGlobalFrictionConstraintSP
{
	typedef TVerletGlobalFrictionConstraintSP inherited;
	
private:
	Geometrybb::TBSphere FCachedBSphere;
	virtual void __fastcall SetLocation(const Vectortypes::TVector3f &Value);
	
public:
	void __fastcall UpdateCachedBSphere(void);
	virtual void __fastcall PerformSpaceQuery(void);
	virtual Geometrybb::TBSphere __fastcall GetBSphere(void) = 0 ;
	__property Geometrybb::TBSphere CachedBSphere = {read=FCachedBSphere};
public:
	/* TVerletGlobalFrictionConstraint.Create */ inline __fastcall virtual TVerletGlobalFrictionConstraintSphere(TVerletWorld* const aOwner) : TVerletGlobalFrictionConstraintSP(aOwner) { }
	
public:
	/* TVerletGlobalConstraint.Destroy */ inline __fastcall virtual ~TVerletGlobalFrictionConstraintSphere(void) { }
	
};

#pragma pack(pop)

class DELPHICLASS TVerletGlobalFrictionConstraintBox;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TVerletGlobalFrictionConstraintBox : public TVerletGlobalFrictionConstraintSP
{
	typedef TVerletGlobalFrictionConstraintSP inherited;
	
private:
	Geometrybb::TAABB FCachedAABB;
	virtual void __fastcall SetLocation(const Vectortypes::TVector3f &Value);
	
public:
	void __fastcall UpdateCachedAABB(void);
	virtual void __fastcall PerformSpaceQuery(void);
	virtual Geometrybb::TAABB __fastcall GetAABB(void) = 0 ;
	__property Geometrybb::TAABB CachedAABB = {read=FCachedAABB};
public:
	/* TVerletGlobalFrictionConstraint.Create */ inline __fastcall virtual TVerletGlobalFrictionConstraintBox(TVerletWorld* const aOwner) : TVerletGlobalFrictionConstraintSP(aOwner) { }
	
public:
	/* TVerletGlobalConstraint.Destroy */ inline __fastcall virtual ~TVerletGlobalFrictionConstraintBox(void) { }
	
};

#pragma pack(pop)

class DELPHICLASS TVerletConstraintList;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TVerletConstraintList : public System::Classes::TList
{
	typedef System::Classes::TList inherited;
	
public:
	TVerletConstraint* operator[](int i) { return Items[i]; }
	
private:
	TVerletConstraint* __fastcall GetItems(int i);
	void __fastcall SetItems(int i, TVerletConstraint* const Value);
	
public:
	__property TVerletConstraint* Items[int i] = {read=GetItems, write=SetItems/*, default*/};
public:
	/* TList.Destroy */ inline __fastcall virtual ~TVerletConstraintList(void) { }
	
public:
	/* TObject.Create */ inline __fastcall TVerletConstraintList(void) : System::Classes::TList() { }
	
};

#pragma pack(pop)

class DELPHICLASS TVerletForce;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TVerletForce : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	TVerletWorld* FOwner;
	
public:
	__fastcall virtual TVerletForce(TVerletWorld* const aOwner);
	__fastcall virtual ~TVerletForce(void);
	virtual void __fastcall AddForce(const TVerletProgressTimes &vpt) = 0 ;
	virtual void __fastcall RemoveNode(TVerletNode* const aNode) = 0 ;
	__property TVerletWorld* Owner = {read=FOwner};
};

#pragma pack(pop)

class DELPHICLASS TVerletDualForce;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TVerletDualForce : public TVerletForce
{
	typedef TVerletForce inherited;
	
private:
	TVerletNode* FNodeA;
	TVerletNode* FNodeB;
	
public:
	virtual void __fastcall RemoveNode(TVerletNode* const aNode);
	__property TVerletNode* NodeA = {read=FNodeA, write=FNodeA};
	__property TVerletNode* NodeB = {read=FNodeB, write=FNodeB};
public:
	/* TVerletForce.Create */ inline __fastcall virtual TVerletDualForce(TVerletWorld* const aOwner) : TVerletForce(aOwner) { }
	/* TVerletForce.Destroy */ inline __fastcall virtual ~TVerletDualForce(void) { }
	
};

#pragma pack(pop)

class DELPHICLASS TVerletGroupForce;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TVerletGroupForce : public TVerletForce
{
	typedef TVerletForce inherited;
	
private:
	TVerletNodeList* FNodes;
	
public:
	__fastcall virtual TVerletGroupForce(TVerletWorld* const aOwner);
	__fastcall virtual ~TVerletGroupForce(void);
	virtual void __fastcall RemoveNode(TVerletNode* const aNode);
	__property TVerletNodeList* Nodes = {read=FNodes};
};

#pragma pack(pop)

class DELPHICLASS TVerletGlobalForce;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TVerletGlobalForce : public TVerletForce
{
	typedef TVerletForce inherited;
	
public:
	virtual void __fastcall RemoveNode(TVerletNode* const aNode);
	virtual void __fastcall AddForce(const TVerletProgressTimes &vpt);
	virtual void __fastcall AddForceToNode(TVerletNode* const aNode) = 0 ;
public:
	/* TVerletForce.Create */ inline __fastcall virtual TVerletGlobalForce(TVerletWorld* const aOwner) : TVerletForce(aOwner) { }
	/* TVerletForce.Destroy */ inline __fastcall virtual ~TVerletGlobalForce(void) { }
	
};

#pragma pack(pop)

class DELPHICLASS TVerletForceList;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TVerletForceList : public System::Classes::TList
{
	typedef System::Classes::TList inherited;
	
public:
	TVerletForce* operator[](int i) { return Items[i]; }
	
private:
	TVerletForce* __fastcall GetItems(int i);
	void __fastcall SetItems(int i, TVerletForce* const Value);
	
public:
	__property TVerletForce* Items[int i] = {read=GetItems, write=SetItems/*, default*/};
public:
	/* TList.Destroy */ inline __fastcall virtual ~TVerletForceList(void) { }
	
public:
	/* TObject.Create */ inline __fastcall TVerletForceList(void) : System::Classes::TList() { }
	
};

#pragma pack(pop)

enum TUpdateSpacePartion : unsigned char { uspEveryIteration, uspEveryFrame, uspNever };

enum TCollisionConstraintTypes : unsigned char { cctEdge, cctNode };

typedef System::Set<TCollisionConstraintTypes, TCollisionConstraintTypes::cctEdge, TCollisionConstraintTypes::cctNode>  TCollisionConstraintTypesSet;

class DELPHICLASS TVCStick;
class DELPHICLASS TVFSpring;
class DELPHICLASS TVCSlider;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TVerletWorld : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	int FIterations;
	TVerletNodeList* FNodes;
	TVerletConstraintList* FConstraints;
	TVerletForceList* FForces;
	float FMaxDeltaTime;
	float FSimTime;
	float FDrag;
	float FCurrentDeltaTime;
	float FInvCurrentDeltaTime;
	TVerletEdgeList* FSolidEdges;
	Spatialpartitioning::TBaseSpacePartition* FSpacePartition;
	int FCurrentStepCount;
	TUpdateSpacePartion FUpdateSpacePartion;
	TCollisionConstraintTypesSet FCollisionConstraintTypes;
	TVerletConstraintList* FConstraintsWithBeforeIterations;
	TVerletNodeClass FVerletNodeClass;
	bool FInertia;
	int FInertaPauseSteps;
	
protected:
	virtual void __fastcall AccumulateForces(const TVerletProgressTimes &vpt);
	virtual void __fastcall Verlet(const TVerletProgressTimes &vpt);
	virtual void __fastcall SatisfyConstraints(const TVerletProgressTimes &vpt);
	void __fastcall DoUpdateSpacePartition(void);
	
public:
	__fastcall virtual TVerletWorld(void);
	__fastcall virtual ~TVerletWorld(void);
	int __fastcall AddNode(TVerletNode* const aNode);
	void __fastcall RemoveNode(TVerletNode* const aNode);
	int __fastcall AddConstraint(TVerletConstraint* const aConstraint);
	void __fastcall RemoveConstraint(TVerletConstraint* const aConstraint);
	int __fastcall AddForce(TVerletForce* const aForce);
	void __fastcall RemoveForce(TVerletForce* const aForce);
	void __fastcall AddSolidEdge(TVerletNode* const aNodeA, TVerletNode* const aNodeB);
	void __fastcall PauseInertia(const int IterationSteps);
	TVerletNode* __fastcall CreateOwnedNode(const Vectortypes::TVector3f &location, const float aRadius = 0.000000E+00, const float aWeight = 1.000000E+00);
	TVCStick* __fastcall CreateStick(TVerletNode* const aNodeA, TVerletNode* const aNodeB, const float Slack = 0.000000E+00);
	TVFSpring* __fastcall CreateSpring(TVerletNode* const aNodeA, TVerletNode* const aNodeB, const float aStrength, const float aDamping, const float aSlack = 0.000000E+00);
	TVCSlider* __fastcall CreateSlider(TVerletNode* const aNodeA, TVerletNode* const aNodeB, const Vectortypes::TVector3f &aSlideDirection);
	DYNAMIC void __fastcall Initialize(void);
	void __fastcall CreateOctree(const Vectortypes::TVector3f &OctreeMin, const Vectortypes::TVector3f &OctreeMax, const int LeafThreshold, const int MaxTreeDepth);
	virtual int __fastcall Progress(const double deltaTime, const double newTime);
	TVerletNode* __fastcall FirstNode(void);
	TVerletNode* __fastcall LastNode(void);
	__property float Drag = {read=FDrag, write=FDrag};
	__property int Iterations = {read=FIterations, write=FIterations, nodefault};
	__property TVerletNodeList* Nodes = {read=FNodes};
	__property TVerletConstraintList* Constraints = {read=FConstraints};
	__property TVerletConstraintList* ConstraintsWithBeforeIterations = {read=FConstraintsWithBeforeIterations};
	__property float SimTime = {read=FSimTime, write=FSimTime};
	__property float MaxDeltaTime = {read=FMaxDeltaTime, write=FMaxDeltaTime};
	__property float CurrentDeltaTime = {read=FCurrentDeltaTime};
	__property TVerletEdgeList* SolidEdges = {read=FSolidEdges, write=FSolidEdges};
	__property int CurrentStepCount = {read=FCurrentStepCount, nodefault};
	__property Spatialpartitioning::TBaseSpacePartition* SpacePartition = {read=FSpacePartition};
	__property TUpdateSpacePartion UpdateSpacePartion = {read=FUpdateSpacePartion, write=FUpdateSpacePartion, nodefault};
	__property TCollisionConstraintTypesSet CollisionConstraintTypes = {read=FCollisionConstraintTypes, write=FCollisionConstraintTypes, nodefault};
	__property TVerletNodeClass VerletNodeClass = {read=FVerletNodeClass, write=FVerletNodeClass};
	__property bool Inertia = {read=FInertia, write=FInertia, nodefault};
};

#pragma pack(pop)

class DELPHICLASS TVFGravity;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TVFGravity : public TVerletGlobalForce
{
	typedef TVerletGlobalForce inherited;
	
private:
	Vectortypes::TVector3f FGravity;
	
public:
	__fastcall virtual TVFGravity(TVerletWorld* const aOwner);
	virtual void __fastcall AddForceToNode(TVerletNode* const aNode);
	__property Vectortypes::TVector3f Gravity = {read=FGravity, write=FGravity};
public:
	/* TVerletForce.Destroy */ inline __fastcall virtual ~TVFGravity(void) { }
	
};

#pragma pack(pop)

class DELPHICLASS TVFAirResistance;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TVFAirResistance : public TVerletGlobalForce
{
	typedef TVerletGlobalForce inherited;
	
private:
	float FDragCoeff;
	Vectortypes::TVector3f FWindDirection;
	float FWindMagnitude;
	float FWindChaos;
	void __fastcall SetWindDirection(const Vectortypes::TVector3f &Value);
	
public:
	__fastcall virtual TVFAirResistance(TVerletWorld* const aOwner);
	virtual void __fastcall AddForceToNode(TVerletNode* const aNode);
	__property float DragCoeff = {read=FDragCoeff, write=FDragCoeff};
	__property Vectortypes::TVector3f WindDirection = {read=FWindDirection, write=SetWindDirection};
	__property float WindMagnitude = {read=FWindMagnitude, write=FWindMagnitude};
	__property float WindChaos = {read=FWindChaos, write=FWindChaos};
public:
	/* TVerletForce.Destroy */ inline __fastcall virtual ~TVFAirResistance(void) { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TVFSpring : public TVerletDualForce
{
	typedef TVerletDualForce inherited;
	
private:
	float FRestLength;
	float FStrength;
	float FDamping;
	float FSlack;
	float FForceFactor;
	
protected:
	void __fastcall SetSlack(const float value);
	
public:
	virtual void __fastcall AddForce(const TVerletProgressTimes &vpt);
	void __fastcall SetRestLengthToCurrent(void);
	__property float Strength = {read=FStrength, write=FStrength};
	__property float Damping = {read=FDamping, write=FDamping};
	__property float Slack = {read=FSlack, write=SetSlack};
public:
	/* TVerletForce.Create */ inline __fastcall virtual TVFSpring(TVerletWorld* const aOwner) : TVerletDualForce(aOwner) { }
	/* TVerletForce.Destroy */ inline __fastcall virtual ~TVFSpring(void) { }
	
};

#pragma pack(pop)

class DELPHICLASS TVCFloor;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TVCFloor : public TVerletGlobalFrictionConstraintSP
{
	typedef TVerletGlobalFrictionConstraintSP inherited;
	
private:
	float FBounceRatio;
	float FFloorLevel;
	Vectortypes::TVector3f FNormal;
	
protected:
	void __fastcall SetNormal(const Vectortypes::TVector3f &value);
	
public:
	__fastcall virtual TVCFloor(TVerletWorld* const aOwner);
	virtual void __fastcall PerformSpaceQuery(void);
	virtual void __fastcall SatisfyConstraintForNode(TVerletNode* const aNode, const int iteration, const int maxIterations);
	__property float BounceRatio = {read=FBounceRatio, write=FBounceRatio};
	__property float FloorLevel = {read=FFloorLevel, write=FFloorLevel};
	__property Vectortypes::TVector3f Normal = {read=FNormal, write=SetNormal};
public:
	/* TVerletGlobalConstraint.Destroy */ inline __fastcall virtual ~TVCFloor(void) { }
	
};

#pragma pack(pop)

class DELPHICLASS TVCHeightField;
typedef float __fastcall (__closure *TVCHeightFieldOnNeedHeight)(TVCHeightField* hfConstraint, TVerletNode* node);

class PASCALIMPLEMENTATION TVCHeightField : public TVCFloor
{
	typedef TVCFloor inherited;
	
private:
	TVCHeightFieldOnNeedHeight FOnNeedHeight;
	
public:
	virtual void __fastcall SatisfyConstraintForNode(TVerletNode* const aNode, const int iteration, const int maxIterations);
	__property TVCHeightFieldOnNeedHeight OnNeedHeight = {read=FOnNeedHeight, write=FOnNeedHeight};
public:
	/* TVCFloor.Create */ inline __fastcall virtual TVCHeightField(TVerletWorld* const aOwner) : TVCFloor(aOwner) { }
	
public:
	/* TVerletGlobalConstraint.Destroy */ inline __fastcall virtual ~TVCHeightField(void) { }
	
};


#pragma pack(push,4)
class PASCALIMPLEMENTATION TVCStick : public TVerletDualConstraint
{
	typedef TVerletDualConstraint inherited;
	
private:
	float FSlack;
	float FRestLength;
	
public:
	virtual void __fastcall SatisfyConstraint(const int iteration, const int maxIterations);
	void __fastcall SetRestLengthToCurrent(void);
	__property float Slack = {read=FSlack, write=FSlack};
	__property float RestLength = {read=FRestLength, write=FRestLength};
public:
	/* TVerletConstraint.Create */ inline __fastcall virtual TVCStick(TVerletWorld* const aOwner) : TVerletDualConstraint(aOwner) { }
	/* TVerletConstraint.Destroy */ inline __fastcall virtual ~TVCStick(void) { }
	
};

#pragma pack(pop)

class DELPHICLASS TVCRigidBody;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TVCRigidBody : public TVerletGroupConstraint
{
	typedef TVerletGroupConstraint inherited;
	
private:
	typedef System::DynamicArray<Vectortypes::TVector3f> _TVCRigidBody__1;
	
	typedef System::DynamicArray<Vectortypes::TVector3f> _TVCRigidBody__2;
	
	
private:
	_TVCRigidBody__1 FNodeParams;
	_TVCRigidBody__2 FNodeCoords;
	Vectortypes::TMatrix3f FNatMatrix;
	Vectortypes::TMatrix3f FInvNatMatrix;
	
protected:
	void __fastcall ComputeBarycenter(Vectortypes::TVector3f &barycenter);
	void __fastcall ComputeNaturals(const Vectortypes::TVector3f &barycenter, Vectortypes::TVector3f &natX, Vectortypes::TVector3f &natY, Vectortypes::TVector3f &natZ);
	
public:
	void __fastcall ComputeRigidityParameters(void);
	virtual void __fastcall SatisfyConstraint(const int iteration, const int maxIterations);
public:
	/* TVerletGroupConstraint.Create */ inline __fastcall virtual TVCRigidBody(TVerletWorld* const aOwner) : TVerletGroupConstraint(aOwner) { }
	/* TVerletGroupConstraint.Destroy */ inline __fastcall virtual ~TVCRigidBody(void) { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TVCSlider : public TVerletDualConstraint
{
	typedef TVerletDualConstraint inherited;
	
private:
	Vectortypes::TVector3f FSlideDirection;
	bool FConstrained;
	
protected:
	void __fastcall SetSlideDirection(const Vectortypes::TVector3f &value);
	
public:
	virtual void __fastcall SatisfyConstraint(const int iteration, const int maxIterations);
	__property Vectortypes::TVector3f SlideDirection = {read=FSlideDirection, write=SetSlideDirection};
	__property bool Constrained = {read=FConstrained, write=FConstrained, nodefault};
public:
	/* TVerletConstraint.Create */ inline __fastcall virtual TVCSlider(TVerletWorld* const aOwner) : TVerletDualConstraint(aOwner) { }
	/* TVerletConstraint.Destroy */ inline __fastcall virtual ~TVCSlider(void) { }
	
};

#pragma pack(pop)

class DELPHICLASS TVCSphere;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TVCSphere : public TVerletGlobalFrictionConstraintSphere
{
	typedef TVerletGlobalFrictionConstraintSphere inherited;
	
private:
	float FRadius;
	
public:
	virtual Geometrybb::TBSphere __fastcall GetBSphere(void);
	virtual void __fastcall SatisfyConstraintForNode(TVerletNode* const aNode, const int iteration, const int maxIterations);
	virtual void __fastcall SatisfyConstraintForEdge(TVerletEdge* const aEdge, const int iteration, const int maxIterations);
	__property float Radius = {read=FRadius, write=FRadius};
public:
	/* TVerletGlobalFrictionConstraint.Create */ inline __fastcall virtual TVCSphere(TVerletWorld* const aOwner) : TVerletGlobalFrictionConstraintSphere(aOwner) { }
	
public:
	/* TVerletGlobalConstraint.Destroy */ inline __fastcall virtual ~TVCSphere(void) { }
	
};

#pragma pack(pop)

class DELPHICLASS TVCCylinder;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TVCCylinder : public TVerletGlobalFrictionConstraint
{
	typedef TVerletGlobalFrictionConstraint inherited;
	
private:
	Vectortypes::TVector3f FAxis;
	float FRadius;
	float FRadius2;
	
protected:
	void __fastcall SetRadius(const float val);
	
public:
	virtual void __fastcall SatisfyConstraintForNode(TVerletNode* const aNode, const int iteration, const int maxIterations);
	__property Vectortypes::TVector3f Axis = {read=FAxis, write=FAxis};
	__property float Radius = {read=FRadius, write=SetRadius};
public:
	/* TVerletGlobalFrictionConstraint.Create */ inline __fastcall virtual TVCCylinder(TVerletWorld* const aOwner) : TVerletGlobalFrictionConstraint(aOwner) { }
	
public:
	/* TVerletGlobalConstraint.Destroy */ inline __fastcall virtual ~TVCCylinder(void) { }
	
};

#pragma pack(pop)

class DELPHICLASS TVCCube;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TVCCube : public TVerletGlobalFrictionConstraintBox
{
	typedef TVerletGlobalFrictionConstraintBox inherited;
	
private:
	Vectortypes::TVector3f FHalfSides;
	Vectortypes::TVector3f FSides;
	Vectortypes::TVector3f FDirection;
	void __fastcall SetSides(const Vectortypes::TVector3f &Value);
	
public:
	virtual Geometrybb::TAABB __fastcall GetAABB(void);
	virtual void __fastcall SatisfyConstraintForNode(TVerletNode* const aNode, const int iteration, const int maxIterations);
	virtual void __fastcall SatisfyConstraintForEdge(TVerletEdge* const aEdge, const int iteration, const int maxIterations);
	__property Vectortypes::TVector3f Direction = {read=FDirection, write=FDirection};
	__property Vectortypes::TVector3f Sides = {read=FSides, write=SetSides};
public:
	/* TVerletGlobalFrictionConstraint.Create */ inline __fastcall virtual TVCCube(TVerletWorld* const aOwner) : TVerletGlobalFrictionConstraintBox(aOwner) { }
	
public:
	/* TVerletGlobalConstraint.Destroy */ inline __fastcall virtual ~TVCCube(void) { }
	
};

#pragma pack(pop)

class DELPHICLASS TVCCapsule;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TVCCapsule : public TVerletGlobalFrictionConstraintSphere
{
	typedef TVerletGlobalFrictionConstraintSphere inherited;
	
private:
	Vectortypes::TVector3f FAxis;
	float FRadius;
	float FRadius2;
	float FLength;
	float FLengthDiv2;
	
protected:
	void __fastcall SetAxis(const Vectortypes::TVector3f &val);
	void __fastcall SetRadius(const float val);
	void __fastcall SetLength(const float val);
	
public:
	virtual Geometrybb::TBSphere __fastcall GetBSphere(void);
	virtual void __fastcall SatisfyConstraintForNode(TVerletNode* const aNode, const int iteration, const int maxIterations);
	virtual void __fastcall SatisfyConstraintForEdge(TVerletEdge* const aEdge, const int iteration, const int maxIterations);
	__property Vectortypes::TVector3f Axis = {read=FAxis, write=SetAxis};
	__property float Radius = {read=FRadius, write=SetRadius};
	__property float Length = {read=FLength, write=SetLength};
public:
	/* TVerletGlobalFrictionConstraint.Create */ inline __fastcall virtual TVCCapsule(TVerletWorld* const aOwner) : TVerletGlobalFrictionConstraintSphere(aOwner) { }
	
public:
	/* TVerletGlobalConstraint.Destroy */ inline __fastcall virtual ~TVCCapsule(void) { }
	
};

#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------
#define G_DRAG  (1.000000E-04)
#define cDEFAULT_CONSTRAINT_FRICTION  (6.000000E-01)
}	/* namespace Verletclasses */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_VERLETCLASSES)
using namespace Verletclasses;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// VerletclassesHPP
