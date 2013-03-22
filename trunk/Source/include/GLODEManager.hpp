// CodeGear C++Builder
// Copyright (c) 1995, 2012 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLODEManager.pas' rev: 24.00 (Win32)

#ifndef GlodemanagerHPP
#define GlodemanagerHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <System.Classes.hpp>	// Pascal unit
#include <ODEGL.hpp>	// Pascal unit
#include <ODEImport.hpp>	// Pascal unit
#include <GLScene.hpp>	// Pascal unit
#include <VectorGeometry.hpp>	// Pascal unit
#include <GLTexture.hpp>	// Pascal unit
#include <OpenGLTokens.hpp>	// Pascal unit
#include <XOpenGL.hpp>	// Pascal unit
#include <System.SysUtils.hpp>	// Pascal unit
#include <GLObjects.hpp>	// Pascal unit
#include <XCollection.hpp>	// Pascal unit
#include <PersistentClasses.hpp>	// Pascal unit
#include <VectorLists.hpp>	// Pascal unit
#include <GLColor.hpp>	// Pascal unit
#include <GLCoordinates.hpp>	// Pascal unit
#include <GLRenderContextInfo.hpp>	// Pascal unit
#include <GLManager.hpp>	// Pascal unit
#include <GLState.hpp>	// Pascal unit
#include <VectorTypes.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Glodemanager
{
//-- type declarations -------------------------------------------------------
typedef void __fastcall (__closure *TODECustomCollisionEvent)(Odeimport::PdxGeom Geom1, Odeimport::PdxGeom Geom2);

typedef void __fastcall (__closure *TODECollisionEvent)(System::TObject* Sender, System::TObject* Object1, System::TObject* Object2, Odeimport::TdContact &Contact, bool &HandleCollision);

typedef void __fastcall (__closure *TODEObjectCollisionEvent)(System::TObject* Sender, System::TObject* Object2, Odeimport::TdContact &Contact, bool &HandleCollision);

enum TODECollisionSurfaceMode : unsigned char { csmMu2, csmFDir1, csmBounce, csmSoftERP, csmSoftCFM, csmMotion1, csmMotion2, csmSlip1, csmSlip2 };

typedef System::Set<TODECollisionSurfaceMode, TODECollisionSurfaceMode::csmMu2, TODECollisionSurfaceMode::csmSlip2>  TSurfaceModes;

enum TODESolverMethod : unsigned char { osmDefault, osmStepFast, osmQuickStep };

class DELPHICLASS TGLODEManager;
class DELPHICLASS TGLODEBehaviour;
class PASCALIMPLEMENTATION TGLODEManager : public System::Classes::TComponent
{
	typedef System::Classes::TComponent inherited;
	
private:
	typedef System::DynamicArray<Odeimport::TdContact> _TGLODEManager__1;
	
	typedef System::DynamicArray<Odeimport::TdContactGeom> _TGLODEManager__2;
	
	
private:
	Odeimport::TdxWorld *FWorld;
	Odeimport::TdxSpace *FSpace;
	Odeimport::TdxJointGroup *FContactGroup;
	Glcoordinates::TGLCoordinates3* FGravity;
	TODECollisionEvent FOnCollision;
	TODECustomCollisionEvent FOnCustomCollision;
	int FNumContactJoints;
	int FMaxContacts;
	Persistentclasses::TPersistentObjectList* FODEBehaviours;
	System::Classes::TList* FRFContactList;
	int FIterations;
	TODESolverMethod FSolver;
	_TGLODEManager__1 FContacts;
	_TGLODEManager__2 FContactGeoms;
	Glscene::TGLRenderPoint* FRenderPoint;
	bool FVisible;
	bool FVisibleAtRunTime;
	Glcolor::TGLColor* FGeomColorDynD;
	Glcolor::TGLColor* FGeomColorDynE;
	Glcolor::TGLColor* FGeomColorStat;
	
protected:
	virtual void __fastcall Loaded(void);
	void __fastcall CalcContact(System::TObject* Object1, System::TObject* Object2, Odeimport::TdContact &Contact);
	void __fastcall Collision(Odeimport::PdxGeom g1, Odeimport::PdxGeom g2);
	void __fastcall GravityChange(System::TObject* Sender);
	void __fastcall SetMaxContacts(const int Value);
	void __fastcall SetGravity(Glcoordinates::TGLCoordinates3* value);
	void __fastcall SetIterations(const int val);
	TGLODEBehaviour* __fastcall GetODEBehaviour(int index);
	void __fastcall RegisterODEBehaviour(TGLODEBehaviour* ODEBehaviour);
	void __fastcall UnregisterODEBehaviour(TGLODEBehaviour* ODEBehaviour);
	void __fastcall SetRenderPoint(Glscene::TGLRenderPoint* const value);
	void __fastcall RenderEvent(System::TObject* Sender, Glrendercontextinfo::TRenderContextInfo &rci);
	void __fastcall RenderPointFreed(System::TObject* Sender);
	void __fastcall SetVisible(const bool Value);
	void __fastcall SetVisibleAtRunTime(const bool Value);
	void __fastcall SetGeomColorDynE(Glcolor::TGLColor* const Value);
	void __fastcall GeomColorChangeDynE(System::TObject* Sender);
	void __fastcall SetGeomColorDynD(Glcolor::TGLColor* const Value);
	void __fastcall GeomColorChangeDynD(System::TObject* Sender);
	void __fastcall SetGeomColorStat(Glcolor::TGLColor* const Value);
	void __fastcall GeomColorChangeStat(System::TObject* Sender);
	__property TGLODEBehaviour* ODEBehaviours[int index] = {read=GetODEBehaviour};
	
public:
	__fastcall virtual TGLODEManager(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TGLODEManager(void);
	void __fastcall Step(double deltaTime);
	void __fastcall NotifyChange(System::TObject* Sender);
	__property Odeimport::PdxWorld World = {read=FWorld};
	__property Odeimport::PdxSpace Space = {read=FSpace};
	__property Odeimport::PdxJointGroup ContactGroup = {read=FContactGroup};
	__property int NumContactJoints = {read=FNumContactJoints, nodefault};
	
__published:
	__property Glcoordinates::TGLCoordinates3* Gravity = {read=FGravity, write=SetGravity};
	__property TODECollisionEvent OnCollision = {read=FOnCollision, write=FOnCollision};
	__property TODECustomCollisionEvent OnCustomCollision = {read=FOnCustomCollision, write=FOnCustomCollision};
	__property TODESolverMethod Solver = {read=FSolver, write=FSolver, nodefault};
	__property int Iterations = {read=FIterations, write=SetIterations, nodefault};
	__property int MaxContacts = {read=FMaxContacts, write=SetMaxContacts, nodefault};
	__property Glscene::TGLRenderPoint* RenderPoint = {read=FRenderPoint, write=SetRenderPoint};
	__property bool Visible = {read=FVisible, write=SetVisible, nodefault};
	__property bool VisibleAtRunTime = {read=FVisibleAtRunTime, write=SetVisibleAtRunTime, nodefault};
	__property Glcolor::TGLColor* GeomColorDynD = {read=FGeomColorDynD, write=SetGeomColorDynD};
	__property Glcolor::TGLColor* GeomColorDynE = {read=FGeomColorDynE, write=SetGeomColorDynE};
	__property Glcolor::TGLColor* GeomColorStat = {read=FGeomColorStat, write=SetGeomColorStat};
};


class DELPHICLASS TODECollisionSurface;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TODECollisionSurface : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
private:
	System::Classes::TPersistent* FOwner;
	Odeimport::TdSurfaceParameters FSurfaceParams;
	float FRFCoeff;
	bool FRFEnabled;
	
protected:
	void __fastcall WriteToFiler(System::Classes::TWriter* writer);
	void __fastcall ReadFromFiler(System::Classes::TReader* reader);
	TSurfaceModes __fastcall GetSurfaceMode(void);
	float __fastcall GetMu(void);
	float __fastcall GetMu2(void);
	float __fastcall GetBounce(void);
	float __fastcall GetBounce_Vel(void);
	float __fastcall GetSoftERP(void);
	float __fastcall GetSoftCFM(void);
	float __fastcall GetMotion1(void);
	float __fastcall GetMotion2(void);
	float __fastcall GetSlip1(void);
	float __fastcall GetSlip2(void);
	void __fastcall SetSurfaceMode(TSurfaceModes value);
	void __fastcall SetMu(float value);
	void __fastcall SetMu2(float value);
	void __fastcall SetBounce(float value);
	void __fastcall SetBounce_Vel(float value);
	void __fastcall SetSoftERP(float value);
	void __fastcall SetSoftCFM(float value);
	void __fastcall SetMotion1(float value);
	void __fastcall SetMotion2(float value);
	void __fastcall SetSlip1(float value);
	void __fastcall SetSlip2(float value);
	
public:
	__fastcall TODECollisionSurface(System::Classes::TPersistent* AOwner);
	DYNAMIC System::Classes::TPersistent* __fastcall GetOwner(void);
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	
__published:
	__property float RollingFrictionCoeff = {read=FRFCoeff, write=FRFCoeff};
	__property bool RollingFrictionEnabled = {read=FRFEnabled, write=FRFEnabled, nodefault};
	__property TSurfaceModes SurfaceMode = {read=GetSurfaceMode, write=SetSurfaceMode, nodefault};
	__property float Mu = {read=GetMu, write=SetMu};
	__property float Mu2 = {read=GetMu2, write=SetMu2};
	__property float Bounce = {read=GetBounce, write=SetBounce};
	__property float Bounce_Vel = {read=GetBounce_Vel, write=SetBounce_Vel};
	__property float SoftERP = {read=GetSoftERP, write=SetSoftERP};
	__property float SoftCFM = {read=GetSoftCFM, write=SetSoftCFM};
	__property float Motion1 = {read=GetMotion1, write=SetMotion1};
	__property float Motion2 = {read=GetMotion2, write=SetMotion2};
	__property float Slip1 = {read=GetSlip1, write=SetSlip1};
	__property float Slip2 = {read=GetSlip2, write=SetSlip2};
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TODECollisionSurface(void) { }
	
};

#pragma pack(pop)

typedef System::TMetaClass* TODEElementClass;

class PASCALIMPLEMENTATION TGLODEBehaviour : public Glscene::TGLBehaviour
{
	typedef Glscene::TGLBehaviour inherited;
	
private:
	TGLODEManager* FManager;
	System::UnicodeString FManagerName;
	TODECollisionSurface* FSurface;
	TODEObjectCollisionEvent FOnCollision;
	bool FInitialized;
	Glscene::TGLBaseSceneObject* FOwnerBaseSceneObject;
	
protected:
	virtual void __fastcall Initialize(void);
	virtual void __fastcall Finalize(void);
	virtual void __fastcall WriteToFiler(System::Classes::TWriter* writer);
	virtual void __fastcall ReadFromFiler(System::Classes::TReader* reader);
	DYNAMIC void __fastcall Loaded(void);
	void __fastcall SetManager(TGLODEManager* Value);
	void __fastcall SetSurface(TODECollisionSurface* value);
	Vectortypes::TMatrix4f __fastcall GetAbsoluteMatrix(void);
	
public:
	__fastcall virtual TGLODEBehaviour(Xcollection::TXCollection* AOwner);
	__fastcall virtual ~TGLODEBehaviour(void);
	void __fastcall NotifyChange(System::TObject* Sender);
	virtual void __fastcall Render(Glrendercontextinfo::TRenderContextInfo &rci);
	void __fastcall Reinitialize(void);
	__property bool Initialized = {read=FInitialized, nodefault};
	__property Vectortypes::TMatrix4f AbsoluteMatrix = {read=GetAbsoluteMatrix};
	
__published:
	__property TGLODEManager* Manager = {read=FManager, write=SetManager};
	__property TODECollisionSurface* Surface = {read=FSurface, write=SetSurface};
	__property TODEObjectCollisionEvent OnCollision = {read=FOnCollision, write=FOnCollision};
};


class DELPHICLASS TGLODEDynamic;
class DELPHICLASS TODEElements;
class DELPHICLASS TODEJointBase;
class DELPHICLASS TODEElementBase;
class PASCALIMPLEMENTATION TGLODEDynamic : public TGLODEBehaviour
{
	typedef TGLODEBehaviour inherited;
	
private:
	Odeimport::TdxBody *FBody;
	Odeimport::TdMass FMass;
	TODEElements* FElements;
	bool FEnabled;
	System::Classes::TList* FJointRegister;
	
protected:
	virtual void __fastcall Initialize(void);
	virtual void __fastcall Finalize(void);
	virtual void __fastcall WriteToFiler(System::Classes::TWriter* writer);
	virtual void __fastcall ReadFromFiler(System::Classes::TReader* reader);
	void __fastcall SetMass(const Odeimport::TdMass &Value);
	Odeimport::TdMass __fastcall GetMass(void);
	void __fastcall AlignBodyToMatrix(const Vectortypes::TMatrix4f &Mat);
	void __fastcall SetEnabled(const bool Value);
	bool __fastcall GetEnabled(void);
	void __fastcall RegisterJoint(TODEJointBase* Joint);
	void __fastcall UnregisterJoint(TODEJointBase* Joint);
	
public:
	__fastcall virtual TGLODEDynamic(Xcollection::TXCollection* AOwner);
	__fastcall virtual ~TGLODEDynamic(void);
	virtual void __fastcall Render(Glrendercontextinfo::TRenderContextInfo &rci);
	__classmethod virtual System::UnicodeString __fastcall FriendlyName();
	__classmethod virtual bool __fastcall UniqueItem();
	DYNAMIC TODEElementBase* __fastcall AddNewElement(TODEElementClass AChild);
	void __fastcall AlignObject(void);
	Odeimport::TdMass __fastcall CalculateMass(void);
	void __fastcall CalibrateCenterOfMass(void);
	void __fastcall AddForce(const Vectortypes::TVector3f &Force);
	void __fastcall AddForceAtPos(const Vectortypes::TVector3f &Force, const Vectortypes::TVector3f &Pos);
	void __fastcall AddForceAtRelPos(const Vectortypes::TVector3f &Force, const Vectortypes::TVector3f &Pos);
	void __fastcall AddRelForce(const Vectortypes::TVector3f &Force);
	void __fastcall AddRelForceAtPos(const Vectortypes::TVector3f &Force, const Vectortypes::TVector3f &Pos);
	void __fastcall AddRelForceAtRelPos(const Vectortypes::TVector3f &Force, const Vectortypes::TVector3f &Pos);
	void __fastcall AddTorque(const Vectortypes::TVector3f &Torque);
	void __fastcall AddRelTorque(const Vectortypes::TVector3f &Torque);
	__property Odeimport::PdxBody Body = {read=FBody};
	__property Odeimport::TdMass Mass = {read=GetMass, write=SetMass};
	
__published:
	__property TODEElements* Elements = {read=FElements};
	__property bool Enabled = {read=GetEnabled, write=SetEnabled, nodefault};
};


class DELPHICLASS TGLODEStatic;
class PASCALIMPLEMENTATION TGLODEStatic : public TGLODEBehaviour
{
	typedef TGLODEBehaviour inherited;
	
private:
	TODEElements* FElements;
	
protected:
	virtual void __fastcall Initialize(void);
	virtual void __fastcall Finalize(void);
	virtual void __fastcall WriteToFiler(System::Classes::TWriter* writer);
	virtual void __fastcall ReadFromFiler(System::Classes::TReader* reader);
	void __fastcall AlignElements(void);
	
public:
	__fastcall virtual TGLODEStatic(Xcollection::TXCollection* AOwner);
	__fastcall virtual ~TGLODEStatic(void);
	virtual void __fastcall Render(Glrendercontextinfo::TRenderContextInfo &rci);
	__classmethod virtual System::UnicodeString __fastcall FriendlyName();
	__classmethod virtual bool __fastcall UniqueItem();
	DYNAMIC TODEElementBase* __fastcall AddNewElement(TODEElementClass AChild);
	
__published:
	__property TODEElements* Elements = {read=FElements};
};


#pragma pack(push,4)
class PASCALIMPLEMENTATION TODEElements : public Xcollection::TXCollection
{
	typedef Xcollection::TXCollection inherited;
	
private:
	TODEElementBase* __fastcall GetElement(int index);
	
public:
	__fastcall virtual ~TODEElements(void);
	__classmethod virtual Xcollection::TXCollectionItemClass __fastcall ItemsClass();
	void __fastcall Initialize(void);
	void __fastcall Finalize(void);
	void __fastcall NotifyChange(System::TObject* Sender);
	void __fastcall Render(Glrendercontextinfo::TRenderContextInfo &rci);
	__property TODEElementBase* Element[int index] = {read=GetElement};
public:
	/* TXCollection.Create */ inline __fastcall virtual TODEElements(System::Classes::TPersistent* aOwner) : Xcollection::TXCollection(aOwner) { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TODEElementBase : public Xcollection::TXCollectionItem
{
	typedef Xcollection::TXCollectionItem inherited;
	
private:
	Odeimport::TdMass FMass;
	float FDensity;
	Odeimport::TdxGeom *FGeomTransform;
	Odeimport::TdxGeom *FGeomElement;
	Glcoordinates::TGLCoordinates3* FPosition;
	Glcoordinates::TGLCoordinates3* FDirection;
	Glcoordinates::TGLCoordinates3* FUp;
	Vectortypes::TMatrix4f FLocalMatrix;
	bool FRealignODE;
	bool FInitialized;
	bool FDynamic;
	bool FIsCalculating;
	
protected:
	virtual void __fastcall Initialize(void);
	virtual void __fastcall Finalize(void);
	virtual Odeimport::TdMass __fastcall CalculateMass(void);
	virtual void __fastcall ODERebuild(void);
	void __fastcall NotifyChange(System::TObject* Sender);
	void __fastcall CoordinateChanged(System::TObject* Sender);
	virtual void __fastcall WriteToFiler(System::Classes::TWriter* writer);
	virtual void __fastcall ReadFromFiler(System::Classes::TReader* reader);
	bool __fastcall IsODEInitialized(void);
	virtual void __fastcall AlignGeomElementToMatrix(const Vectortypes::TMatrix4f &Mat);
	void __fastcall SetGeomElement(Odeimport::PdxGeom aGeom);
	void __fastcall RebuildMatrix(void);
	void __fastcall RebuildVectors(void);
	void __fastcall SetDensity(const float Value);
	void __fastcall SetMatrix(const Vectortypes::TMatrix4f &Value);
	Vectortypes::TMatrix4f __fastcall GetMatrix(void);
	void __fastcall SetPosition(Glcoordinates::TGLCoordinates3* const Value);
	void __fastcall SetDirection(Glcoordinates::TGLCoordinates3* const Value);
	void __fastcall SetUp(Glcoordinates::TGLCoordinates3* const Value);
	
public:
	__fastcall virtual TODEElementBase(Xcollection::TXCollection* AOwner);
	__fastcall virtual ~TODEElementBase(void);
	virtual void __fastcall Render(Glrendercontextinfo::TRenderContextInfo &rci);
	Vectortypes::TMatrix4f __fastcall AbsoluteMatrix(void);
	Vectortypes::TVector3f __fastcall AbsolutePosition(void);
	__property Vectortypes::TMatrix4f Matrix = {read=GetMatrix, write=SetMatrix};
	__property Odeimport::PdxGeom GeomTransform = {read=FGeomTransform};
	__property Odeimport::PdxGeom Geom = {read=FGeomElement};
	__property bool Initialized = {read=FInitialized, nodefault};
	
__published:
	__property float Density = {read=FDensity, write=SetDensity};
	__property Glcoordinates::TGLCoordinates3* Position = {read=FPosition, write=SetPosition};
	__property Glcoordinates::TGLCoordinates3* Direction = {read=FDirection, write=SetDirection};
	__property Glcoordinates::TGLCoordinates3* Up = {read=FUp, write=SetUp};
};

#pragma pack(pop)

class DELPHICLASS TODEElementBox;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TODEElementBox : public TODEElementBase
{
	typedef TODEElementBase inherited;
	
private:
	float FBoxWidth;
	float FBoxHeight;
	float FBoxDepth;
	
protected:
	virtual void __fastcall Initialize(void);
	virtual Odeimport::TdMass __fastcall CalculateMass(void);
	virtual void __fastcall ODERebuild(void);
	virtual void __fastcall WriteToFiler(System::Classes::TWriter* writer);
	virtual void __fastcall ReadFromFiler(System::Classes::TReader* reader);
	float __fastcall GetBoxWidth(void);
	float __fastcall GetBoxHeight(void);
	float __fastcall GetBoxDepth(void);
	void __fastcall SetBoxWidth(const float Value);
	void __fastcall SetBoxHeight(const float Value);
	void __fastcall SetBoxDepth(const float Value);
	
public:
	__fastcall virtual TODEElementBox(Xcollection::TXCollection* AOwner);
	virtual void __fastcall Render(Glrendercontextinfo::TRenderContextInfo &rci);
	__classmethod virtual System::UnicodeString __fastcall FriendlyName();
	__classmethod virtual System::UnicodeString __fastcall FriendlyDescription();
	__classmethod virtual System::UnicodeString __fastcall ItemCategory();
	
__published:
	__property float BoxWidth = {read=GetBoxWidth, write=SetBoxWidth};
	__property float BoxHeight = {read=GetBoxHeight, write=SetBoxHeight};
	__property float BoxDepth = {read=GetBoxDepth, write=SetBoxDepth};
public:
	/* TODEElementBase.Destroy */ inline __fastcall virtual ~TODEElementBox(void) { }
	
};

#pragma pack(pop)

class DELPHICLASS TODEElementSphere;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TODEElementSphere : public TODEElementBase
{
	typedef TODEElementBase inherited;
	
private:
	float FRadius;
	
protected:
	virtual void __fastcall Initialize(void);
	virtual Odeimport::TdMass __fastcall CalculateMass(void);
	virtual void __fastcall ODERebuild(void);
	virtual void __fastcall WriteToFiler(System::Classes::TWriter* writer);
	virtual void __fastcall ReadFromFiler(System::Classes::TReader* reader);
	float __fastcall GetRadius(void);
	void __fastcall SetRadius(const float Value);
	
public:
	__fastcall virtual TODEElementSphere(Xcollection::TXCollection* AOwner);
	virtual void __fastcall Render(Glrendercontextinfo::TRenderContextInfo &rci);
	__classmethod virtual System::UnicodeString __fastcall FriendlyName();
	__classmethod virtual System::UnicodeString __fastcall FriendlyDescription();
	__classmethod virtual System::UnicodeString __fastcall ItemCategory();
	
__published:
	__property float Radius = {read=GetRadius, write=SetRadius};
public:
	/* TODEElementBase.Destroy */ inline __fastcall virtual ~TODEElementSphere(void) { }
	
};

#pragma pack(pop)

class DELPHICLASS TODEElementCapsule;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TODEElementCapsule : public TODEElementBase
{
	typedef TODEElementBase inherited;
	
private:
	float FRadius;
	float FLength;
	
protected:
	virtual void __fastcall Initialize(void);
	virtual Odeimport::TdMass __fastcall CalculateMass(void);
	virtual void __fastcall ODERebuild(void);
	virtual void __fastcall WriteToFiler(System::Classes::TWriter* writer);
	virtual void __fastcall ReadFromFiler(System::Classes::TReader* reader);
	float __fastcall GetRadius(void);
	float __fastcall GetLength(void);
	void __fastcall SetRadius(const float Value);
	void __fastcall SetLength(const float Value);
	
public:
	__fastcall virtual TODEElementCapsule(Xcollection::TXCollection* AOwner);
	virtual void __fastcall Render(Glrendercontextinfo::TRenderContextInfo &rci);
	__classmethod virtual System::UnicodeString __fastcall FriendlyName();
	__classmethod virtual System::UnicodeString __fastcall FriendlyDescription();
	__classmethod virtual System::UnicodeString __fastcall ItemCategory();
	
__published:
	__property float Radius = {read=GetRadius, write=SetRadius};
	__property float Length = {read=GetLength, write=SetLength};
public:
	/* TODEElementBase.Destroy */ inline __fastcall virtual ~TODEElementCapsule(void) { }
	
};

#pragma pack(pop)

class DELPHICLASS TODEElementCylinder;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TODEElementCylinder : public TODEElementBase
{
	typedef TODEElementBase inherited;
	
private:
	float FRadius;
	float FLength;
	
protected:
	virtual void __fastcall Initialize(void);
	virtual Odeimport::TdMass __fastcall CalculateMass(void);
	virtual void __fastcall ODERebuild(void);
	virtual void __fastcall WriteToFiler(System::Classes::TWriter* writer);
	virtual void __fastcall ReadFromFiler(System::Classes::TReader* reader);
	float __fastcall GetRadius(void);
	float __fastcall GetLength(void);
	void __fastcall SetRadius(const float Value);
	void __fastcall SetLength(const float Value);
	
public:
	__fastcall virtual TODEElementCylinder(Xcollection::TXCollection* AOwner);
	virtual void __fastcall Render(Glrendercontextinfo::TRenderContextInfo &rci);
	__classmethod virtual System::UnicodeString __fastcall FriendlyName();
	__classmethod virtual System::UnicodeString __fastcall FriendlyDescription();
	__classmethod virtual System::UnicodeString __fastcall ItemCategory();
	
__published:
	__property float Radius = {read=GetRadius, write=SetRadius};
	__property float Length = {read=GetLength, write=SetLength};
public:
	/* TODEElementBase.Destroy */ inline __fastcall virtual ~TODEElementCylinder(void) { }
	
};

#pragma pack(pop)

class DELPHICLASS TODEElementTriMesh;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TODEElementTriMesh : public TODEElementBase
{
	typedef TODEElementBase inherited;
	
private:
	Odeimport::TdxTriMeshData *FTriMeshData;
	Vectorlists::TAffineVectorList* FVertices;
	Vectorlists::TIntegerList* FIndices;
	
protected:
	virtual void __fastcall Initialize(void);
	virtual void __fastcall Finalize(void);
	virtual Odeimport::TdMass __fastcall CalculateMass(void);
	virtual void __fastcall WriteToFiler(System::Classes::TWriter* writer);
	virtual void __fastcall ReadFromFiler(System::Classes::TReader* reader);
	void __fastcall SetVertices(Vectorlists::TAffineVectorList* const Value);
	void __fastcall SetIndices(Vectorlists::TIntegerList* const Value);
	
public:
	__fastcall virtual TODEElementTriMesh(Xcollection::TXCollection* AOwner);
	__fastcall virtual ~TODEElementTriMesh(void);
	__classmethod virtual System::UnicodeString __fastcall FriendlyName();
	__classmethod virtual System::UnicodeString __fastcall FriendlyDescription();
	__classmethod virtual System::UnicodeString __fastcall ItemCategory();
	void __fastcall RefreshTriMeshData(void);
	__property Vectorlists::TAffineVectorList* Vertices = {read=FVertices, write=SetVertices};
	__property Vectorlists::TIntegerList* Indices = {read=FIndices, write=SetIndices};
};

#pragma pack(pop)

class DELPHICLASS TODEElementPlane;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TODEElementPlane : public TODEElementBase
{
	typedef TODEElementBase inherited;
	
protected:
	virtual void __fastcall Initialize(void);
	virtual void __fastcall WriteToFiler(System::Classes::TWriter* writer);
	virtual void __fastcall ReadFromFiler(System::Classes::TReader* reader);
	virtual void __fastcall AlignGeomElementToMatrix(const Vectortypes::TMatrix4f &Mat);
	
public:
	__classmethod virtual System::UnicodeString __fastcall FriendlyName();
	__classmethod virtual System::UnicodeString __fastcall FriendlyDescription();
	__classmethod virtual System::UnicodeString __fastcall ItemCategory();
	__classmethod virtual bool __fastcall CanAddTo(Xcollection::TXCollection* collection);
public:
	/* TODEElementBase.Create */ inline __fastcall virtual TODEElementPlane(Xcollection::TXCollection* AOwner) : TODEElementBase(AOwner) { }
	/* TODEElementBase.Destroy */ inline __fastcall virtual ~TODEElementPlane(void) { }
	
};

#pragma pack(pop)

class DELPHICLASS TODEJoints;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TODEJoints : public Xcollection::TXCollection
{
	typedef Xcollection::TXCollection inherited;
	
public:
	TODEJointBase* operator[](int index) { return Joint[index]; }
	
protected:
	TODEJointBase* __fastcall GetJoint(int index);
	
public:
	__classmethod virtual Xcollection::TXCollectionItemClass __fastcall ItemsClass();
	void __fastcall Initialize(void);
	void __fastcall Finalize(void);
	__property TODEJointBase* Joint[int index] = {read=GetJoint/*, default*/};
public:
	/* TXCollection.Create */ inline __fastcall virtual TODEJoints(System::Classes::TPersistent* aOwner) : Xcollection::TXCollection(aOwner) { }
	/* TXCollection.Destroy */ inline __fastcall virtual ~TODEJoints(void) { }
	
};

#pragma pack(pop)

class DELPHICLASS TGLODEJointList;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLODEJointList : public System::Classes::TComponent
{
	typedef System::Classes::TComponent inherited;
	
private:
	TODEJoints* FJoints;
	
protected:
	void __fastcall WriteJoints(System::Classes::TStream* stream);
	void __fastcall ReadJoints(System::Classes::TStream* stream);
	virtual void __fastcall DefineProperties(System::Classes::TFiler* Filer);
	virtual void __fastcall Loaded(void);
	virtual void __fastcall Notification(System::Classes::TComponent* AComponent, System::Classes::TOperation Operation);
	
public:
	__fastcall virtual TGLODEJointList(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TGLODEJointList(void);
	
__published:
	__property TODEJoints* Joints = {read=FJoints};
};

#pragma pack(pop)

enum TJointOption : unsigned char { joBothObjectsMustBeAssigned };

typedef System::Set<TJointOption, TJointOption::joBothObjectsMustBeAssigned, TJointOption::joBothObjectsMustBeAssigned>  TJointOptions;

#pragma pack(push,4)
class PASCALIMPLEMENTATION TODEJointBase : public Xcollection::TXCollectionItem
{
	typedef Xcollection::TXCollectionItem inherited;
	
private:
	Odeimport::TdxJoint *FJointID;
	Glscene::TGLBaseSceneObject* FObject1;
	Glscene::TGLBaseSceneObject* FObject2;
	TGLODEManager* FManager;
	System::UnicodeString FObject1Name;
	System::UnicodeString FObject2Name;
	System::UnicodeString FManagerName;
	bool FInitialized;
	bool FEnabled;
	TJointOptions FJointOptions;
	
protected:
	virtual void __fastcall WriteToFiler(System::Classes::TWriter* writer);
	virtual void __fastcall ReadFromFiler(System::Classes::TReader* reader);
	DYNAMIC void __fastcall Loaded(void);
	bool __fastcall IsODEInitialized(void);
	void __fastcall RegisterJointWithObject(Glscene::TGLBaseSceneObject* Obj);
	void __fastcall UnregisterJointWithObject(Glscene::TGLBaseSceneObject* Obj);
	void __fastcall Attach(void);
	void __fastcall SetManager(TGLODEManager* const Value);
	void __fastcall SetObject1(Glscene::TGLBaseSceneObject* const Value);
	void __fastcall SetObject2(Glscene::TGLBaseSceneObject* const Value);
	void __fastcall SetEnabled(const bool Value);
	void __fastcall SetJointOptions(const TJointOptions Value);
	__property TJointOptions JointOptions = {read=FJointOptions, write=SetJointOptions, nodefault};
	
public:
	__fastcall virtual TODEJointBase(Xcollection::TXCollection* aOwner);
	__fastcall virtual ~TODEJointBase(void);
	virtual void __fastcall StructureChanged(void);
	virtual void __fastcall Initialize(void);
	virtual void __fastcall Finalize(void);
	bool __fastcall IsAttached(void);
	void __fastcall DoLoaded(void);
	__property Odeimport::PdxJoint JointID = {read=FJointID};
	__property bool Initialized = {read=FInitialized, nodefault};
	
__published:
	__property TGLODEManager* Manager = {read=FManager, write=SetManager};
	__property Glscene::TGLBaseSceneObject* Object1 = {read=FObject1, write=SetObject1};
	__property Glscene::TGLBaseSceneObject* Object2 = {read=FObject2, write=SetObject2};
	__property bool Enabled = {read=FEnabled, write=SetEnabled, nodefault};
};

#pragma pack(pop)

typedef bool __fastcall (__closure *TODESetParamCallback)(int Param, const float Value);

typedef bool __fastcall (__closure *TODEGetParamCallback)(int Param, float &Value);

class DELPHICLASS TODEJointParams;
class PASCALIMPLEMENTATION TODEJointParams : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
private:
	System::Classes::TPersistent* FOwner;
	TODESetParamCallback FSetCallback;
	TODEGetParamCallback FGetCallback;
	float FLoStop;
	float FHiStop;
	float FVel;
	float FFMax;
	float FFudgeFactor;
	float FBounce;
	float FCFM;
	float FStopERP;
	float FStopCFM;
	float FSuspensionERP;
	float FSuspensionCFM;
	bool FFlagLoStop;
	bool FFlagHiStop;
	bool FFlagVel;
	bool FFlagFMax;
	bool FFlagFudgeFactor;
	bool FFlagBounce;
	bool FFlagCFM;
	bool FFlagStopERP;
	bool FFlagStopCFM;
	bool FFlagSuspensionERP;
	bool FFlagSuspensionCFM;
	
protected:
	float __fastcall GetLoStop(void);
	float __fastcall GetHiStop(void);
	float __fastcall GetVel(void);
	float __fastcall GetFMax(void);
	float __fastcall GetFudgeFactor(void);
	float __fastcall GetBounce(void);
	float __fastcall GetCFM(void);
	float __fastcall GetStopERP(void);
	float __fastcall GetStopCFM(void);
	float __fastcall GetSuspensionERP(void);
	float __fastcall GetSuspensionCFM(void);
	void __fastcall SetLoStop(const float Value);
	void __fastcall SetHiStop(const float Value);
	void __fastcall SetVel(const float Value);
	void __fastcall SetFMax(const float Value);
	void __fastcall SetFudgeFactor(const float Value);
	void __fastcall SetBounce(const float Value);
	void __fastcall SetCFM(const float Value);
	void __fastcall SetStopERP(const float Value);
	void __fastcall SetStopCFM(const float Value);
	void __fastcall SetSuspensionERP(const float Value);
	void __fastcall SetSuspensionCFM(const float Value);
	void __fastcall WriteToFiler(System::Classes::TWriter* writer);
	void __fastcall ReadFromFiler(System::Classes::TReader* reader);
	
public:
	__fastcall TODEJointParams(System::Classes::TPersistent* AOwner);
	DYNAMIC System::Classes::TPersistent* __fastcall GetOwner(void);
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	void __fastcall ApplyFlagged(void);
	__property TODESetParamCallback SetCallback = {read=FSetCallback, write=FSetCallback};
	__property TODEGetParamCallback GetCallback = {read=FGetCallback, write=FGetCallback};
	
__published:
	__property float LoStop = {read=GetLoStop, write=SetLoStop};
	__property float HiStop = {read=GetHiStop, write=SetHiStop};
	__property float Vel = {read=GetVel, write=SetVel};
	__property float FMax = {read=GetFMax, write=SetFMax};
	__property float FudgeFactor = {read=GetFudgeFactor, write=SetFudgeFactor};
	__property float Bounce = {read=GetBounce, write=SetBounce};
	__property float CFM = {read=GetCFM, write=SetCFM};
	__property float StopERP = {read=GetStopERP, write=SetStopERP};
	__property float StopCFM = {read=GetStopCFM, write=SetStopCFM};
	__property float SuspensionERP = {read=GetSuspensionERP, write=SetSuspensionERP};
	__property float SuspensionCFM = {read=GetSuspensionCFM, write=SetSuspensionCFM};
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TODEJointParams(void) { }
	
};


class DELPHICLASS TODEJointHinge;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TODEJointHinge : public TODEJointBase
{
	typedef TODEJointBase inherited;
	
private:
	Glcoordinates::TGLCoordinates3* FAnchor;
	Glcoordinates::TGLCoordinates3* FAxis;
	TODEJointParams* FAxisParams;
	
protected:
	virtual void __fastcall WriteToFiler(System::Classes::TWriter* writer);
	virtual void __fastcall ReadFromFiler(System::Classes::TReader* reader);
	void __fastcall SetAnchor(Glcoordinates::TGLCoordinates3* const Value);
	void __fastcall SetAxis(Glcoordinates::TGLCoordinates3* const Value);
	void __fastcall AnchorChange(System::TObject* Sender);
	void __fastcall AxisChange(System::TObject* Sender);
	void __fastcall SetAxisParams(TODEJointParams* const Value);
	bool __fastcall SetAxisParam(int Param, const float Value);
	bool __fastcall GetAxisParam(int Param, float &Value);
	
public:
	__fastcall virtual TODEJointHinge(Xcollection::TXCollection* aOwner);
	__fastcall virtual ~TODEJointHinge(void);
	virtual void __fastcall StructureChanged(void);
	virtual void __fastcall Initialize(void);
	__classmethod virtual System::UnicodeString __fastcall FriendlyName();
	__classmethod virtual System::UnicodeString __fastcall FriendlyDescription();
	
__published:
	__property Glcoordinates::TGLCoordinates3* Anchor = {read=FAnchor, write=SetAnchor};
	__property Glcoordinates::TGLCoordinates3* Axis = {read=FAxis, write=SetAxis};
	__property TODEJointParams* AxisParams = {read=FAxisParams, write=SetAxisParams};
};

#pragma pack(pop)

class DELPHICLASS TODEJointBall;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TODEJointBall : public TODEJointBase
{
	typedef TODEJointBase inherited;
	
private:
	Glcoordinates::TGLCoordinates3* FAnchor;
	
protected:
	virtual void __fastcall WriteToFiler(System::Classes::TWriter* writer);
	virtual void __fastcall ReadFromFiler(System::Classes::TReader* reader);
	void __fastcall SetAnchor(Glcoordinates::TGLCoordinates3* const Value);
	void __fastcall AnchorChange(System::TObject* Sender);
	
public:
	__fastcall virtual TODEJointBall(Xcollection::TXCollection* aOwner);
	__fastcall virtual ~TODEJointBall(void);
	virtual void __fastcall StructureChanged(void);
	virtual void __fastcall Initialize(void);
	__classmethod virtual System::UnicodeString __fastcall FriendlyName();
	__classmethod virtual System::UnicodeString __fastcall FriendlyDescription();
	
__published:
	__property Glcoordinates::TGLCoordinates3* Anchor = {read=FAnchor, write=SetAnchor};
};

#pragma pack(pop)

class DELPHICLASS TODEJointSlider;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TODEJointSlider : public TODEJointBase
{
	typedef TODEJointBase inherited;
	
private:
	Glcoordinates::TGLCoordinates3* FAxis;
	TODEJointParams* FAxisParams;
	
protected:
	virtual void __fastcall WriteToFiler(System::Classes::TWriter* writer);
	virtual void __fastcall ReadFromFiler(System::Classes::TReader* reader);
	void __fastcall SetAxis(Glcoordinates::TGLCoordinates3* const Value);
	void __fastcall AxisChange(System::TObject* Sender);
	void __fastcall SetAxisParams(TODEJointParams* const Value);
	bool __fastcall SetAxisParam(int Param, const float Value);
	bool __fastcall GetAxisParam(int Param, float &Value);
	
public:
	__fastcall virtual TODEJointSlider(Xcollection::TXCollection* aOwner);
	__fastcall virtual ~TODEJointSlider(void);
	virtual void __fastcall StructureChanged(void);
	virtual void __fastcall Initialize(void);
	__classmethod virtual System::UnicodeString __fastcall FriendlyName();
	__classmethod virtual System::UnicodeString __fastcall FriendlyDescription();
	
__published:
	__property Glcoordinates::TGLCoordinates3* Axis = {read=FAxis, write=SetAxis};
	__property TODEJointParams* AxisParams = {read=FAxisParams, write=SetAxisParams};
};

#pragma pack(pop)

class DELPHICLASS TODEJointFixed;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TODEJointFixed : public TODEJointBase
{
	typedef TODEJointBase inherited;
	
protected:
	virtual void __fastcall WriteToFiler(System::Classes::TWriter* writer);
	virtual void __fastcall ReadFromFiler(System::Classes::TReader* reader);
	
public:
	__classmethod virtual System::UnicodeString __fastcall FriendlyName();
	__classmethod virtual System::UnicodeString __fastcall FriendlyDescription();
	virtual void __fastcall Initialize(void);
public:
	/* TODEJointBase.Create */ inline __fastcall virtual TODEJointFixed(Xcollection::TXCollection* aOwner) : TODEJointBase(aOwner) { }
	/* TODEJointBase.Destroy */ inline __fastcall virtual ~TODEJointFixed(void) { }
	
};

#pragma pack(pop)

class DELPHICLASS TODEJointHinge2;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TODEJointHinge2 : public TODEJointBase
{
	typedef TODEJointBase inherited;
	
private:
	Glcoordinates::TGLCoordinates3* FAnchor;
	Glcoordinates::TGLCoordinates3* FAxis1;
	Glcoordinates::TGLCoordinates3* FAxis2;
	TODEJointParams* FAxis1Params;
	TODEJointParams* FAxis2Params;
	
protected:
	virtual void __fastcall WriteToFiler(System::Classes::TWriter* writer);
	virtual void __fastcall ReadFromFiler(System::Classes::TReader* reader);
	void __fastcall SetAnchor(Glcoordinates::TGLCoordinates3* const Value);
	void __fastcall SetAxis1(Glcoordinates::TGLCoordinates3* const Value);
	void __fastcall SetAxis2(Glcoordinates::TGLCoordinates3* const Value);
	void __fastcall AnchorChange(System::TObject* Sender);
	void __fastcall Axis1Change(System::TObject* Sender);
	void __fastcall Axis2Change(System::TObject* Sender);
	void __fastcall SetAxis1Params(TODEJointParams* const Value);
	void __fastcall SetAxis2Params(TODEJointParams* const Value);
	bool __fastcall SetAxis1Param(int Param, const float Value);
	bool __fastcall SetAxis2Param(int Param, const float Value);
	bool __fastcall GetAxis1Param(int Param, float &Value);
	bool __fastcall GetAxis2Param(int Param, float &Value);
	
public:
	__fastcall virtual TODEJointHinge2(Xcollection::TXCollection* aOwner);
	__fastcall virtual ~TODEJointHinge2(void);
	virtual void __fastcall StructureChanged(void);
	virtual void __fastcall Initialize(void);
	__classmethod virtual System::UnicodeString __fastcall FriendlyName();
	__classmethod virtual System::UnicodeString __fastcall FriendlyDescription();
	
__published:
	__property Glcoordinates::TGLCoordinates3* Anchor = {read=FAnchor, write=SetAnchor};
	__property Glcoordinates::TGLCoordinates3* Axis1 = {read=FAxis1, write=SetAxis1};
	__property Glcoordinates::TGLCoordinates3* Axis2 = {read=FAxis2, write=SetAxis2};
	__property TODEJointParams* Axis1Params = {read=FAxis1Params, write=SetAxis1Params};
	__property TODEJointParams* Axis2Params = {read=FAxis2Params, write=SetAxis2Params};
};

#pragma pack(pop)

class DELPHICLASS TODEJointUniversal;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TODEJointUniversal : public TODEJointBase
{
	typedef TODEJointBase inherited;
	
private:
	Glcoordinates::TGLCoordinates3* FAnchor;
	Glcoordinates::TGLCoordinates3* FAxis1;
	Glcoordinates::TGLCoordinates3* FAxis2;
	TODEJointParams* FAxis1Params;
	TODEJointParams* FAxis2Params;
	
protected:
	virtual void __fastcall WriteToFiler(System::Classes::TWriter* writer);
	virtual void __fastcall ReadFromFiler(System::Classes::TReader* reader);
	void __fastcall SetAnchor(Glcoordinates::TGLCoordinates3* const Value);
	void __fastcall SetAxis1(Glcoordinates::TGLCoordinates3* const Value);
	void __fastcall SetAxis2(Glcoordinates::TGLCoordinates3* const Value);
	void __fastcall AnchorChange(System::TObject* Sender);
	void __fastcall Axis1Change(System::TObject* Sender);
	void __fastcall Axis2Change(System::TObject* Sender);
	void __fastcall SetAxis1Params(TODEJointParams* const Value);
	void __fastcall SetAxis2Params(TODEJointParams* const Value);
	bool __fastcall SetAxis1Param(int Param, const float Value);
	bool __fastcall SetAxis2Param(int Param, const float Value);
	bool __fastcall GetAxis1Param(int Param, float &Value);
	bool __fastcall GetAxis2Param(int Param, float &Value);
	
public:
	__fastcall virtual TODEJointUniversal(Xcollection::TXCollection* aOwner);
	__fastcall virtual ~TODEJointUniversal(void);
	virtual void __fastcall Initialize(void);
	virtual void __fastcall StructureChanged(void);
	__classmethod virtual System::UnicodeString __fastcall FriendlyName();
	__classmethod virtual System::UnicodeString __fastcall FriendlyDescription();
	
__published:
	__property Glcoordinates::TGLCoordinates3* Anchor = {read=FAnchor, write=SetAnchor};
	__property Glcoordinates::TGLCoordinates3* Axis1 = {read=FAxis1, write=SetAxis1};
	__property Glcoordinates::TGLCoordinates3* Axis2 = {read=FAxis2, write=SetAxis2};
	__property TODEJointParams* Axis1Params = {read=FAxis1Params, write=SetAxis1Params};
	__property TODEJointParams* Axis2Params = {read=FAxis2Params, write=SetAxis2Params};
};

#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------
extern PACKAGE System::Classes::TList* vGLODEObjectRegister;
extern PACKAGE void __cdecl nearCallBack(void * Data, Odeimport::PdxGeom o1, Odeimport::PdxGeom o2);
extern PACKAGE Odeimport::PdxBody __fastcall GetBodyFromObject(System::TObject* anObject);
extern PACKAGE Odeimport::PdxBody __fastcall GetBodyFromGLSceneObject(Glscene::TGLBaseSceneObject* anObject);
extern PACKAGE TODECollisionSurface* __fastcall GetSurfaceFromObject(System::TObject* anObject);
extern PACKAGE void __fastcall RegisterGLSceneObject(Glscene::TGLBaseSceneObject* anObject);
extern PACKAGE void __fastcall UnregisterGLSceneObject(Glscene::TGLBaseSceneObject* anObject);
extern PACKAGE Glscene::TGLBaseSceneObject* __fastcall GetGLSceneObject(System::UnicodeString anObjectName);
extern PACKAGE TGLODEStatic* __fastcall GetOdeStatic(Glscene::TGLBaseSceneObject* obj);
extern PACKAGE TGLODEStatic* __fastcall GetOrCreateOdeStatic(Glscene::TGLBaseSceneObject* obj);
extern PACKAGE TGLODEDynamic* __fastcall GetOdeDynamic(Glscene::TGLBaseSceneObject* obj);
extern PACKAGE TGLODEDynamic* __fastcall GetOrCreateOdeDynamic(Glscene::TGLBaseSceneObject* obj);
}	/* namespace Glodemanager */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLODEMANAGER)
using namespace Glodemanager;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// GlodemanagerHPP
