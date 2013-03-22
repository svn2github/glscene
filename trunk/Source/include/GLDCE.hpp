// CodeGear C++Builder
// Copyright (c) 1995, 2012 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLDCE.pas' rev: 24.00 (Win32)

#ifndef GldceHPP
#define GldceHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <System.Classes.hpp>	// Pascal unit
#include <GLScene.hpp>	// Pascal unit
#include <XCollection.hpp>	// Pascal unit
#include <VectorGeometry.hpp>	// Pascal unit
#include <VectorLists.hpp>	// Pascal unit
#include <GLVectorFileObjects.hpp>	// Pascal unit
#include <GLCrossPlatform.hpp>	// Pascal unit
#include <GLDCEMisc.hpp>	// Pascal unit
#include <GLEllipseCollision.hpp>	// Pascal unit
#include <GLTerrainRenderer.hpp>	// Pascal unit
#include <GLCoordinates.hpp>	// Pascal unit
#include <BaseClasses.hpp>	// Pascal unit
#include <GLManager.hpp>	// Pascal unit
#include <VectorTypes.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Gldce
{
//-- type declarations -------------------------------------------------------
enum TDCEShape : unsigned char { csEllipsoid, csBox, csFreeform, csTerrain };

enum TDCECollisionSelection : unsigned char { ccsDCEStandard, ccsCollisionStandard, ccsHybrid };

struct DECLSPEC_DRECORD TDCECollision
{
public:
	Vectortypes::TVector3f Position;
	Vectortypes::TVector3f Normal;
	Vectortypes::TVector3f Bounce;
	bool Nearest;
	bool RootCollision;
	float Distance;
};


typedef void __fastcall (__closure *TDCECollisionEvent)(System::TObject* Sender, Glscene::TGLBaseSceneObject* object1, Glscene::TGLBaseSceneObject* object2, TDCECollision &CollisionInfo);

typedef void __fastcall (__closure *TDCEObjectCollisionEvent)(System::TObject* Sender, Glscene::TGLBaseSceneObject* ObjectCollided, TDCECollision &CollisionInfo);

class DELPHICLASS TGLDCEManager;
class DELPHICLASS TGLDCEStatic;
class DELPHICLASS TGLDCEDynamic;
class PASCALIMPLEMENTATION TGLDCEManager : public System::Classes::TComponent
{
	typedef System::Classes::TComponent inherited;
	
private:
	System::Classes::TList* FStatics;
	System::Classes::TList* FDynamics;
	float FGravity;
	Glcoordinates::TGLCoordinates3* FWorldDirection;
	float FWorldScale;
	float FMovimentScale;
	TDCECollisionSelection FStandardiseLayers;
	bool FManualStep;
	TDCECollisionEvent FOnCollision;
	void __fastcall SetWorldDirection(Glcoordinates::TGLCoordinates3* const Value);
	void __fastcall SetWorldScale(const float Value);
	int __fastcall GetDynamicCount(void);
	int __fastcall GetStaticCount(void);
	
protected:
	void __fastcall RegisterStatic(TGLDCEStatic* aClient);
	void __fastcall DeRegisterStatic(TGLDCEStatic* aClient);
	void __fastcall DeRegisterAllStatics(void);
	void __fastcall RegisterDynamic(TGLDCEDynamic* aClient);
	void __fastcall DeRegisterDynamic(TGLDCEDynamic* aClient);
	void __fastcall DeRegisterAllDynamics(void);
	
public:
	__fastcall virtual TGLDCEManager(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TGLDCEManager(void);
	float __fastcall MoveByDistance(TGLDCEDynamic* &Body, const Vectortypes::TVector3f &deltaS, const Vectortypes::TVector3f &deltaAbsS);
	void __fastcall Step(double deltaTime);
	__property int DynamicCount = {read=GetDynamicCount, nodefault};
	__property int StaticCount = {read=GetStaticCount, nodefault};
	
__published:
	__property float Gravity = {read=FGravity, write=FGravity};
	__property Glcoordinates::TGLCoordinates3* WorldDirection = {read=FWorldDirection, write=SetWorldDirection};
	__property float WorldScale = {read=FWorldScale, write=SetWorldScale};
	__property float MovimentScale = {read=FMovimentScale, write=FMovimentScale};
	__property TDCECollisionSelection StandardiseLayers = {read=FStandardiseLayers, write=FStandardiseLayers, nodefault};
	__property bool ManualStep = {read=FManualStep, write=FManualStep, nodefault};
	__property TDCECollisionEvent OnCollision = {read=FOnCollision, write=FOnCollision};
};


class PASCALIMPLEMENTATION TGLDCEStatic : public Glscene::TGLBehaviour
{
	typedef Glscene::TGLBehaviour inherited;
	
private:
	TGLDCEManager* FManager;
	System::UnicodeString FManagerName;
	bool FActive;
	TDCEShape FShape;
	int FLayer;
	bool FSolid;
	float FFriction;
	float FBounceFactor;
	Glcoordinates::TGLCoordinates3* FSize;
	TDCEObjectCollisionEvent FOnCollision;
	void __fastcall SetShape(const TDCEShape Value);
	void __fastcall SetFriction(const float Value);
	void __fastcall SetBounceFactor(const float Value);
	void __fastcall SetSize(Glcoordinates::TGLCoordinates3* const Value);
	
protected:
	void __fastcall SetManager(TGLDCEManager* const val);
	virtual void __fastcall WriteToFiler(System::Classes::TWriter* writer);
	virtual void __fastcall ReadFromFiler(System::Classes::TReader* reader);
	DYNAMIC void __fastcall Loaded(void);
	
public:
	__fastcall virtual TGLDCEStatic(Xcollection::TXCollection* aOwner);
	__fastcall virtual ~TGLDCEStatic(void);
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	__classmethod virtual System::UnicodeString __fastcall FriendlyName();
	__classmethod virtual System::UnicodeString __fastcall FriendlyDescription();
	__property TDCEObjectCollisionEvent OnCollision = {read=FOnCollision, write=FOnCollision};
	
__published:
	__property bool Active = {read=FActive, write=FActive, nodefault};
	__property TGLDCEManager* Manager = {read=FManager, write=SetManager};
	__property TDCEShape Shape = {read=FShape, write=SetShape, nodefault};
	__property int Layer = {read=FLayer, write=FLayer, nodefault};
	__property bool Solid = {read=FSolid, write=FSolid, nodefault};
	__property float Friction = {read=FFriction, write=SetFriction};
	__property float BounceFactor = {read=FBounceFactor, write=SetBounceFactor};
	__property Glcoordinates::TGLCoordinates3* Size = {read=FSize, write=SetSize};
};


enum TDCESlideOrBounce : unsigned char { csbSlide, csbBounce };

class PASCALIMPLEMENTATION TGLDCEDynamic : public Glscene::TGLBehaviour
{
	typedef Glscene::TGLBehaviour inherited;
	
private:
	TGLDCEManager* FManager;
	System::UnicodeString FManagerName;
	bool FActive;
	bool FUseGravity;
	int FLayer;
	bool FSolid;
	float FFriction;
	float FBounceFactor;
	Glcoordinates::TGLCoordinates3* FSize;
	System::Byte FMaxRecursionDepth;
	TDCESlideOrBounce FSlideOrBounce;
	Vectortypes::TVector3f FAccel;
	Vectortypes::TVector3f FSpeed;
	Vectortypes::TVector3f FAbsAccel;
	Vectortypes::TVector3f FAbsSpeed;
	Vectortypes::TVector3f FGravSpeed;
	float FTotalFriction;
	bool FInGround;
	Vectortypes::TVector3f FGroundNormal;
	float FJumpHeight;
	float FJumpForce;
	float FJumpSpeed;
	bool FJumping;
	TDCEObjectCollisionEvent FOnCollision;
	void __fastcall SetFriction(const float Value);
	void __fastcall SetBounceFactor(const float Value);
	void __fastcall SetSize(Glcoordinates::TGLCoordinates3* const Value);
	
protected:
	void __fastcall SetManager(TGLDCEManager* const val);
	virtual void __fastcall WriteToFiler(System::Classes::TWriter* writer);
	virtual void __fastcall ReadFromFiler(System::Classes::TReader* reader);
	DYNAMIC void __fastcall Loaded(void);
	
public:
	__fastcall virtual TGLDCEDynamic(Xcollection::TXCollection* aOwner);
	__fastcall virtual ~TGLDCEDynamic(void);
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	__classmethod virtual System::UnicodeString __fastcall FriendlyName();
	__classmethod virtual System::UnicodeString __fastcall FriendlyDescription();
	void __fastcall ApplyAccel(const Vectortypes::TVector3f &NewAccel)/* overload */;
	void __fastcall ApplyAccel(float x, float y, float z)/* overload */;
	void __fastcall ApplyAbsAccel(const Vectortypes::TVector3f &NewAccel)/* overload */;
	void __fastcall ApplyAbsAccel(float x, float y, float z)/* overload */;
	void __fastcall StopAccel(void);
	void __fastcall StopAbsAccel(void);
	void __fastcall Jump(float jHeight, float jSpeed);
	void __fastcall Move(const Vectortypes::TVector3f &deltaS, double deltaTime);
	void __fastcall MoveTo(const Vectortypes::TVector3f &Position, float Amount);
	void __fastcall DoMove(double deltaTime);
	virtual void __fastcall DoProgress(const Baseclasses::TProgressTimes &progressTime);
	__property Vectortypes::TVector3f Speed = {read=FSpeed, write=FSpeed};
	__property bool InGround = {read=FInGround, nodefault};
	__property System::Byte MaxRecursionDepth = {read=FMaxRecursionDepth, write=FMaxRecursionDepth, nodefault};
	__property TDCEObjectCollisionEvent OnCollision = {read=FOnCollision, write=FOnCollision};
	
__published:
	__property bool Active = {read=FActive, write=FActive, nodefault};
	__property TGLDCEManager* Manager = {read=FManager, write=SetManager};
	__property bool UseGravity = {read=FUseGravity, write=FUseGravity, nodefault};
	__property int Layer = {read=FLayer, write=FLayer, nodefault};
	__property bool Solid = {read=FSolid, write=FSolid, nodefault};
	__property float Friction = {read=FFriction, write=SetFriction};
	__property float BounceFactor = {read=FBounceFactor, write=SetBounceFactor};
	__property Glcoordinates::TGLCoordinates3* Size = {read=FSize, write=SetSize};
	__property TDCESlideOrBounce SlideOrBounce = {read=FSlideOrBounce, write=FSlideOrBounce, nodefault};
};


//-- var, const, procedure ---------------------------------------------------
extern PACKAGE TGLDCEStatic* __fastcall GetOrCreateDCEStatic(Glscene::TGLBehaviours* behaviours)/* overload */;
extern PACKAGE TGLDCEStatic* __fastcall GetOrCreateDCEStatic(Glscene::TGLBaseSceneObject* obj)/* overload */;
extern PACKAGE TGLDCEDynamic* __fastcall GetOrCreateDCEDynamic(Glscene::TGLBehaviours* behaviours)/* overload */;
extern PACKAGE TGLDCEDynamic* __fastcall GetOrCreateDCEDynamic(Glscene::TGLBaseSceneObject* obj)/* overload */;
}	/* namespace Gldce */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLDCE)
using namespace Gldce;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// GldceHPP
