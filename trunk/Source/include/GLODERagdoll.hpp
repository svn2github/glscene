// CodeGear C++Builder
// Copyright (c) 1995, 2012 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLODERagdoll.pas' rev: 24.00 (Win32)

#ifndef GloderagdollHPP
#define GloderagdollHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <GLRagdoll.hpp>	// Pascal unit
#include <ODEImport.hpp>	// Pascal unit
#include <GLScene.hpp>	// Pascal unit
#include <GLObjects.hpp>	// Pascal unit
#include <VectorGeometry.hpp>	// Pascal unit
#include <ODEGL.hpp>	// Pascal unit
#include <GLTexture.hpp>	// Pascal unit
#include <GLVectorFileObjects.hpp>	// Pascal unit
#include <System.Classes.hpp>	// Pascal unit
#include <VectorTypes.hpp>	// Pascal unit
#include <PersistentClasses.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Gloderagdoll
{
//-- type declarations -------------------------------------------------------
class DELPHICLASS TODERagdollCube;
class DELPHICLASS TODERagdollBone;
class DELPHICLASS TODERagdoll;
class PASCALIMPLEMENTATION TODERagdollCube : public Globjects::TGLCube
{
	typedef Globjects::TGLCube inherited;
	
public:
	TODERagdollBone* Bone;
	TODERagdoll* Ragdoll;
public:
	/* TGLCube.Create */ inline __fastcall virtual TODERagdollCube(System::Classes::TComponent* AOwner) : Globjects::TGLCube(AOwner) { }
	
public:
	/* TGLCustomSceneObject.Destroy */ inline __fastcall virtual ~TODERagdollCube(void) { }
	
public:
	/* TGLBaseSceneObject.CreateAsChild */ inline __fastcall TODERagdollCube(Glscene::TGLBaseSceneObject* aParentOwner) : Globjects::TGLCube(aParentOwner) { }
	
};


class DELPHICLASS TODERagdollWorld;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TODERagdollWorld : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	Odeimport::TdxSpace *FSpace;
	Odeimport::TdxWorld *FWorld;
	Odeimport::TdxJointGroup *FContactGroup;
	TODERagdoll* FRagdoll;
	bool isWorldCreated;
	
public:
	__fastcall TODERagdollWorld(void);
	__fastcall TODERagdollWorld(Odeimport::PdxWorld World, Odeimport::PdxSpace Space, Odeimport::PdxJointGroup ContactGroup);
	__fastcall virtual ~TODERagdollWorld(void);
	void __fastcall WorldUpdate(void);
	__property Odeimport::PdxWorld World = {read=FWorld};
	__property Odeimport::PdxSpace Space = {read=FSpace};
	__property Odeimport::PdxJointGroup ContactGroup = {read=FContactGroup};
	__property TODERagdoll* Ragdoll = {read=FRagdoll};
};

#pragma pack(pop)

class DELPHICLASS TODERagdollDummyJoint;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TODERagdollDummyJoint : public Glragdoll::TRagdollJoint
{
	typedef Glragdoll::TRagdollJoint inherited;
	
public:
	/* TObject.Create */ inline __fastcall TODERagdollDummyJoint(void) : Glragdoll::TRagdollJoint() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TODERagdollDummyJoint(void) { }
	
};

#pragma pack(pop)

class DELPHICLASS TODERagdollHingeJoint;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TODERagdollHingeJoint : public Glragdoll::TRagdollJoint
{
	typedef Glragdoll::TRagdollJoint inherited;
	
private:
	float FParamHiStop;
	float FParamLoStop;
	Vectortypes::TVector3f FAxis;
	
public:
	__fastcall TODERagdollHingeJoint(const Vectortypes::TVector3f &Axis, float ParamLoStop, float ParamHiStop);
	__property Vectortypes::TVector3f Axis = {read=FAxis};
	__property float ParamLoStop = {read=FParamLoStop, write=FParamLoStop};
	__property float ParamHiStop = {read=FParamHiStop, write=FParamHiStop};
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TODERagdollHingeJoint(void) { }
	
};

#pragma pack(pop)

class DELPHICLASS TODERagdollUniversalJoint;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TODERagdollUniversalJoint : public TODERagdollHingeJoint
{
	typedef TODERagdollHingeJoint inherited;
	
private:
	float FParamHiStop2;
	float FParamLoStop2;
	Vectortypes::TVector3f FAxis2;
	
public:
	__fastcall TODERagdollUniversalJoint(const Vectortypes::TVector3f &Axis, float ParamLoStop, float ParamHiStop, const Vectortypes::TVector3f &Axis2, float ParamLoStop2, float ParamHiStop2);
	__property Vectortypes::TVector3f Axis2 = {read=FAxis2};
	__property float ParamLoStop2 = {read=FParamLoStop2, write=FParamLoStop2};
	__property float ParamHiStop2 = {read=FParamHiStop2, write=FParamHiStop2};
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TODERagdollUniversalJoint(void) { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TODERagdollBone : public Glragdoll::TRagdollBone
{
	typedef Glragdoll::TRagdollBone inherited;
	
private:
	TODERagdollBone* FOwner;
	TODERagdoll* FRagdoll;
	Odeimport::TdxBody *FBody;
	Odeimport::TdxGeom *FGeom;
	Odeimport::TdxJoint *FJointId;
	void __fastcall AlignBodyToMatrix(const Vectortypes::TMatrix4f &Mat);
	
protected:
	virtual void __fastcall Start(void);
	virtual void __fastcall Align(void);
	virtual void __fastcall Update(void);
	virtual void __fastcall Stop(void);
	
public:
	__fastcall TODERagdollBone(TODERagdollBone* aOwner);
	__fastcall TODERagdollBone(TODERagdoll* Ragdoll);
	__property Odeimport::PdxBody Body = {read=FBody};
	__property Odeimport::PdxGeom Geom = {read=FGeom};
public:
	/* TRagdollBone.Destroy */ inline __fastcall virtual ~TODERagdollBone(void) { }
	
public:
	/* TPersistentObject.CreateFromFiler */ inline __fastcall TODERagdollBone(Persistentclasses::TVirtualReader* reader) : Glragdoll::TRagdollBone(reader) { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TODERagdoll : public Glragdoll::TGLRagdoll
{
	typedef Glragdoll::TGLRagdoll inherited;
	
private:
	TODERagdollWorld* FODEWorld;
	Glscene::TGLBaseSceneObject* FGLSceneRoot;
	bool FShowBoundingBoxes;
	
public:
	__fastcall TODERagdoll(Glvectorfileobjects::TGLBaseMesh* AOwner);
	__property TODERagdollWorld* ODEWorld = {read=FODEWorld, write=FODEWorld};
	__property Glscene::TGLBaseSceneObject* GLSceneRoot = {read=FGLSceneRoot, write=FGLSceneRoot};
	__property bool ShowBoundingBoxes = {read=FShowBoundingBoxes, write=FShowBoundingBoxes, nodefault};
public:
	/* TGLRagdoll.Destroy */ inline __fastcall virtual ~TODERagdoll(void) { }
	
public:
	/* TPersistentObject.CreateFromFiler */ inline __fastcall TODERagdoll(Persistentclasses::TVirtualReader* reader) : Glragdoll::TGLRagdoll(reader) { }
	
};

#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------
static const System::Int8 cMaxContacts = System::Int8(0x4);
extern PACKAGE float vGLODERagdoll_cDensity;
extern PACKAGE float vGLODERagdoll_cMass;
}	/* namespace Gloderagdoll */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLODERAGDOLL)
using namespace Gloderagdoll;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// GloderagdollHPP
