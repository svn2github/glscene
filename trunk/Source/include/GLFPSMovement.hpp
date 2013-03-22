// CodeGear C++Builder
// Copyright (c) 1995, 2012 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLFPSMovement.pas' rev: 24.00 (Win32)

#ifndef GlfpsmovementHPP
#define GlfpsmovementHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <System.Classes.hpp>	// Pascal unit
#include <Vcl.Graphics.hpp>	// Pascal unit
#include <VectorGeometry.hpp>	// Pascal unit
#include <GLScene.hpp>	// Pascal unit
#include <GLVectorFileObjects.hpp>	// Pascal unit
#include <VectorLists.hpp>	// Pascal unit
#include <XCollection.hpp>	// Pascal unit
#include <GLGeomObjects.hpp>	// Pascal unit
#include <GLNavigator.hpp>	// Pascal unit
#include <GLRenderContextInfo.hpp>	// Pascal unit
#include <BaseClasses.hpp>	// Pascal unit
#include <GLManager.hpp>	// Pascal unit
#include <GLState.hpp>	// Pascal unit
#include <VectorTypes.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Glfpsmovement
{
//-- type declarations -------------------------------------------------------
struct DECLSPEC_DRECORD TContactPoint
{
public:
	Vectortypes::TVector4f intPoint;
	Vectortypes::TVector4f intNormal;
};


class DELPHICLASS TCollisionState;
class PASCALIMPLEMENTATION TCollisionState : public System::TObject
{
	typedef System::TObject inherited;
	
public:
	Vectortypes::TVector4f Position;
	TContactPoint Contact;
	__int64 Time;
public:
	/* TObject.Create */ inline __fastcall TCollisionState(void) : System::TObject() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TCollisionState(void) { }
	
};


class DELPHICLASS TCollisionStates;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TCollisionStates : public System::Classes::TList
{
	typedef System::Classes::TList inherited;
	
public:
	/* TList.Destroy */ inline __fastcall virtual ~TCollisionStates(void) { }
	
public:
	/* TObject.Create */ inline __fastcall TCollisionStates(void) : System::Classes::TList() { }
	
};

#pragma pack(pop)

class DELPHICLASS TGLMapCollectionItem;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLMapCollectionItem : public Xcollection::TXCollectionItem
{
	typedef Xcollection::TXCollectionItem inherited;
	
private:
	Glvectorfileobjects::TGLFreeForm* FMap;
	System::UnicodeString FMapName;
	int FCollisionGroup;
	void __fastcall setMap(Glvectorfileobjects::TGLFreeForm* value);
	
protected:
	virtual void __fastcall WriteToFiler(System::Classes::TWriter* writer);
	virtual void __fastcall ReadFromFiler(System::Classes::TReader* reader);
	DYNAMIC void __fastcall Loaded(void);
	
public:
	__fastcall virtual TGLMapCollectionItem(Xcollection::TXCollection* aOwner);
	__classmethod virtual System::UnicodeString __fastcall FriendlyName();
	
__published:
	__property Glvectorfileobjects::TGLFreeForm* Map = {read=FMap, write=setMap};
	__property int CollisionGroup = {read=FCollisionGroup, write=FCollisionGroup, nodefault};
public:
	/* TXCollectionItem.Destroy */ inline __fastcall virtual ~TGLMapCollectionItem(void) { }
	
};

#pragma pack(pop)

typedef System::TMetaClass* TGLMapCollectionItemClass;

class DELPHICLASS TGLMapCollection;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLMapCollection : public Xcollection::TXCollection
{
	typedef Xcollection::TXCollection inherited;
	
public:
	__classmethod virtual Xcollection::TXCollectionItemClass __fastcall ItemsClass();
	TGLMapCollectionItem* __fastcall addMap(Glvectorfileobjects::TGLFreeForm* Map, int CollisionGroup = 0x0);
	TGLMapCollectionItem* __fastcall findMap(Glvectorfileobjects::TGLFreeForm* mapFreeForm);
public:
	/* TXCollection.Create */ inline __fastcall virtual TGLMapCollection(System::Classes::TPersistent* aOwner) : Xcollection::TXCollection(aOwner) { }
	/* TXCollection.Destroy */ inline __fastcall virtual ~TGLMapCollection(void) { }
	
};

#pragma pack(pop)

class DELPHICLASS TGLFPSMovementManager;
class DELPHICLASS TGLBFPSMovement;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLFPSMovementManager : public System::Classes::TComponent
{
	typedef System::Classes::TComponent inherited;
	
private:
	Glnavigator::TGLNavigator* FNavigator;
	int FDisplayTime;
	float FMovementScale;
	TGLMapCollection* FMaps;
	Glscene::TGLScene* FScene;
	void __fastcall SetNavigator(Glnavigator::TGLNavigator* value);
	void __fastcall setScene(Glscene::TGLScene* value);
	void __fastcall DrawArrows(const Vectortypes::TVector4f &intPoint, const Vectortypes::TVector4f &intNormal, const Vectortypes::TVector4f &Ray, Glgeomobjects::TGLArrowLine* Arrow1, Glgeomobjects::TGLArrowLine* Arrow2);
	
protected:
	virtual void __fastcall Loaded(void);
	virtual void __fastcall DefineProperties(System::Classes::TFiler* Filer);
	void __fastcall WriteMaps(System::Classes::TStream* stream);
	void __fastcall ReadMaps(System::Classes::TStream* stream);
	virtual void __fastcall Notification(System::Classes::TComponent* AComponent, System::Classes::TOperation Operation);
	
public:
	__fastcall virtual TGLFPSMovementManager(System::Classes::TComponent* aOwner);
	__fastcall virtual ~TGLFPSMovementManager(void);
	bool __fastcall SphereSweepAndSlide(Glvectorfileobjects::TGLFreeForm* freeform, TGLBFPSMovement* behaviour, const Vectortypes::TVector4f &SphereStart, Vectortypes::TVector4f &Velocity, Vectortypes::TVector4f &newPosition, float sphereRadius)/* overload */;
	void __fastcall SphereSweepAndSlide(TGLBFPSMovement* behaviour, const Vectortypes::TVector4f &SphereStart, Vectortypes::TVector4f &Velocity, Vectortypes::TVector4f &newPosition, float sphereRadius)/* overload */;
	
__published:
	__property TGLMapCollection* Maps = {read=FMaps, write=FMaps};
	__property Glnavigator::TGLNavigator* Navigator = {read=FNavigator, write=SetNavigator};
	__property Glscene::TGLScene* Scene = {read=FScene, write=setScene};
	__property int DisplayTime = {read=FDisplayTime, write=FDisplayTime, nodefault};
	__property float MovementScale = {read=FMovementScale, write=FMovementScale};
};

#pragma pack(pop)

class PASCALIMPLEMENTATION TGLBFPSMovement : public Glscene::TGLBehaviour
{
	typedef Glscene::TGLBehaviour inherited;
	
private:
	TGLFPSMovementManager* FManager;
	TCollisionStates* CollisionStates;
	Glgeomobjects::TGLArrowLine* ArrowLine1;
	Glgeomobjects::TGLArrowLine* ArrowLine2;
	Glgeomobjects::TGLArrowLine* ArrowLine3;
	Glgeomobjects::TGLArrowLine* ArrowLine4;
	Glgeomobjects::TGLArrowLine* ArrowLine5;
	Glgeomobjects::TGLArrowLine* ArrowLine6;
	Glscene::TGLDirectOpenGL* dirGl;
	__int64 tickCount;
	Vectortypes::TVector4f oldPosition;
	bool FGravityEnabled;
	float FSphereRadius;
	bool FShowArrows;
	int FCollisionGroup;
	System::UnicodeString FManagerName;
	void __fastcall setShowArrows(bool value);
	void __fastcall RenderArrowLines(System::TObject* Sender, Glrendercontextinfo::TRenderContextInfo &rci);
	
protected:
	virtual void __fastcall WriteToFiler(System::Classes::TWriter* writer);
	virtual void __fastcall ReadFromFiler(System::Classes::TReader* reader);
	DYNAMIC void __fastcall Loaded(void);
	
public:
	Vectortypes::TVector4f Velocity;
	__fastcall virtual TGLBFPSMovement(Xcollection::TXCollection* aOwner);
	__fastcall virtual ~TGLBFPSMovement(void);
	virtual void __fastcall DoProgress(const Baseclasses::TProgressTimes &progressTime);
	__classmethod virtual System::UnicodeString __fastcall FriendlyName();
	void __fastcall TurnHorizontal(float Angle);
	void __fastcall TurnVertical(float Angle);
	void __fastcall MoveForward(float Distance);
	void __fastcall StrafeHorizontal(float Distance);
	void __fastcall StrafeVertical(float Distance);
	void __fastcall Straighten(void);
	
__published:
	__property TGLFPSMovementManager* Manager = {read=FManager, write=FManager};
	__property float sphereRadius = {read=FSphereRadius, write=FSphereRadius};
	__property bool ShowArrows = {read=FShowArrows, write=setShowArrows, nodefault};
	__property int CollisionGroup = {read=FCollisionGroup, write=FCollisionGroup, nodefault};
	__property bool GravityEnabled = {read=FGravityEnabled, write=FGravityEnabled, nodefault};
};


//-- var, const, procedure ---------------------------------------------------
extern PACKAGE TGLBFPSMovement* __fastcall GetFPSMovement(Glscene::TGLBehaviours* behaviours)/* overload */;
extern PACKAGE TGLBFPSMovement* __fastcall GetFPSMovement(Glscene::TGLBaseSceneObject* obj)/* overload */;
extern PACKAGE TGLBFPSMovement* __fastcall GetOrCreateFPSMovement(Glscene::TGLBehaviours* behaviours)/* overload */;
extern PACKAGE TGLBFPSMovement* __fastcall GetOrCreateFPSMovement(Glscene::TGLBaseSceneObject* obj)/* overload */;
}	/* namespace Glfpsmovement */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLFPSMOVEMENT)
using namespace Glfpsmovement;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// GlfpsmovementHPP
