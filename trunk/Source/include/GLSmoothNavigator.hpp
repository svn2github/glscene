// CodeGear C++Builder
// Copyright (c) 1995, 2012 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLSmoothNavigator.pas' rev: 24.00 (Win32)

#ifndef GlsmoothnavigatorHPP
#define GlsmoothnavigatorHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <System.Classes.hpp>	// Pascal unit
#include <GLNavigator.hpp>	// Pascal unit
#include <VectorGeometry.hpp>	// Pascal unit
#include <GLScene.hpp>	// Pascal unit
#include <GLCrossPlatform.hpp>	// Pascal unit
#include <GLCoordinates.hpp>	// Pascal unit
#include <GLScreen.hpp>	// Pascal unit
#include <XCollection.hpp>	// Pascal unit
#include <VectorTypes.hpp>	// Pascal unit
#include <System.Types.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Glsmoothnavigator
{
//-- type declarations -------------------------------------------------------
class DELPHICLASS TGLNavigatorAbstractParameters;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLNavigatorAbstractParameters : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
private:
	System::Classes::TPersistent* FOwner;
	float FInertia;
	float FSpeed;
	float FCutoff;
	bool __fastcall StoreCutoff(void);
	
protected:
	virtual bool __fastcall StoreInertia(void);
	virtual bool __fastcall StoreSpeed(void);
	DYNAMIC System::Classes::TPersistent* __fastcall GetOwner(void);
	
public:
	__fastcall virtual TGLNavigatorAbstractParameters(System::Classes::TPersistent* AOwner);
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	virtual void __fastcall ScaleParameters(const float Value);
	
__published:
	__property float Inertia = {read=FInertia, write=FInertia, stored=StoreInertia};
	__property float Speed = {read=FSpeed, write=FSpeed, stored=StoreSpeed};
	__property float Cutoff = {read=FCutoff, write=FCutoff, stored=StoreCutoff};
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TGLNavigatorAbstractParameters(void) { }
	
};

#pragma pack(pop)

class DELPHICLASS TGLNavigatorSmoothChangeItem;
class DELPHICLASS TGLSmoothNavigator;
class PASCALIMPLEMENTATION TGLNavigatorSmoothChangeItem : public Xcollection::TXCollectionItem
{
	typedef Xcollection::TXCollectionItem inherited;
	
private:
	float FInertia;
	float FSpeed;
	bool FEnabled;
	float FSpeedLimit;
	double FCutoff;
	bool __fastcall StoreInertia(void);
	bool __fastcall StoreSpeed(void);
	bool __fastcall StoreSpeedLimit(void);
	bool __fastcall StoreCutoff(void);
	
protected:
	TGLSmoothNavigator* __fastcall GetNavigator(void);
	
public:
	virtual bool __fastcall Proceed(double ADeltaTime) = 0 ;
	__fastcall virtual TGLNavigatorSmoothChangeItem(Xcollection::TXCollection* aOwner);
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	virtual void __fastcall ScaleParameters(const float Value);
	virtual void __fastcall ResetTargetValue(void) = 0 ;
	
__published:
	__property float Inertia = {read=FInertia, write=FInertia, stored=StoreInertia};
	__property float Speed = {read=FSpeed, write=FSpeed, stored=StoreSpeed};
	__property float SpeedLimit = {read=FSpeedLimit, write=FSpeedLimit, stored=StoreSpeedLimit};
	__property double Cutoff = {read=FCutoff, write=FCutoff, stored=StoreCutoff};
	__property bool Enabled = {read=FEnabled, write=FEnabled, default=1};
public:
	/* TXCollectionItem.Destroy */ inline __fastcall virtual ~TGLNavigatorSmoothChangeItem(void) { }
	
};


class DELPHICLASS TGLNavigatorSmoothChangeSingle;
typedef float __fastcall (__closure *TGLNavigatorSmoothChangeSingleGetEvent)(TGLNavigatorSmoothChangeSingle* const ASender);

typedef void __fastcall (__closure *TGLNavigatorSmoothChangeSingleSetEvent)(TGLNavigatorSmoothChangeSingle* const ASender, const float AValue);

class PASCALIMPLEMENTATION TGLNavigatorSmoothChangeSingle : public TGLNavigatorSmoothChangeItem
{
	typedef TGLNavigatorSmoothChangeItem inherited;
	
private:
	float FTargetValue;
	TGLNavigatorSmoothChangeSingleGetEvent FOnGetCurrentValue;
	TGLNavigatorSmoothChangeSingleSetEvent FOnSetCurrentValue;
	
public:
	__classmethod virtual System::UnicodeString __fastcall FriendlyName();
	virtual bool __fastcall Proceed(double ADeltaTime);
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	virtual void __fastcall ResetTargetValue(void);
	
__published:
	__property float TargetValue = {read=FTargetValue, write=FTargetValue};
	__property TGLNavigatorSmoothChangeSingleGetEvent OnGetCurrentValue = {read=FOnGetCurrentValue, write=FOnGetCurrentValue};
	__property TGLNavigatorSmoothChangeSingleSetEvent OnSetCurrentValue = {read=FOnSetCurrentValue, write=FOnSetCurrentValue};
public:
	/* TGLNavigatorSmoothChangeItem.Create */ inline __fastcall virtual TGLNavigatorSmoothChangeSingle(Xcollection::TXCollection* aOwner) : TGLNavigatorSmoothChangeItem(aOwner) { }
	
public:
	/* TXCollectionItem.Destroy */ inline __fastcall virtual ~TGLNavigatorSmoothChangeSingle(void) { }
	
};


class DELPHICLASS TGLNavigatorSmoothChangeVector;
typedef Vectortypes::TVector4f __fastcall (__closure *TGLNavigatorSmoothChangeVectorGetEvent)(TGLNavigatorSmoothChangeVector* const ASender);

typedef void __fastcall (__closure *TGLNavigatorSmoothChangeVectorSetEvent)(TGLNavigatorSmoothChangeVector* const ASender, const Vectortypes::TVector4f &AValue);

class PASCALIMPLEMENTATION TGLNavigatorSmoothChangeVector : public TGLNavigatorSmoothChangeItem
{
	typedef TGLNavigatorSmoothChangeItem inherited;
	
private:
	Glcoordinates::TGLCoordinates3* FTargetValue;
	TGLNavigatorSmoothChangeVectorGetEvent FOnGetCurrentValue;
	TGLNavigatorSmoothChangeVectorSetEvent FOnSetCurrentValue;
	void __fastcall SetTargetValue(Glcoordinates::TGLCoordinates3* const Value);
	
public:
	__classmethod virtual System::UnicodeString __fastcall FriendlyName();
	virtual bool __fastcall Proceed(double ADeltaTime);
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	__fastcall virtual TGLNavigatorSmoothChangeVector(Xcollection::TXCollection* aOwner);
	__fastcall virtual ~TGLNavigatorSmoothChangeVector(void);
	virtual void __fastcall ResetTargetValue(void);
	
__published:
	__property Glcoordinates::TGLCoordinates3* TargetValue = {read=FTargetValue, write=SetTargetValue};
	__property TGLNavigatorSmoothChangeVectorGetEvent OnGetCurrentValue = {read=FOnGetCurrentValue, write=FOnGetCurrentValue};
	__property TGLNavigatorSmoothChangeVectorSetEvent OnSetCurrentValue = {read=FOnSetCurrentValue, write=FOnSetCurrentValue};
};


typedef System::TMetaClass* TGLNavigatorSmoothChangeItemClass;

class DELPHICLASS TGLNavigatorSmoothChangeItems;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLNavigatorSmoothChangeItems : public Xcollection::TXCollection
{
	typedef Xcollection::TXCollection inherited;
	
public:
	TGLNavigatorSmoothChangeItem* operator[](const int Index) { return Items[Index]; }
	
private:
	HIDESBASE TGLNavigatorSmoothChangeItem* __fastcall GetItems(const int Index);
	void __fastcall SetItems(const int Index, TGLNavigatorSmoothChangeItem* const Value);
	
protected:
	void __fastcall DoProceed(double ADeltaTime);
	
public:
	HIDESBASE TGLNavigatorSmoothChangeItem* __fastcall Add(TGLNavigatorSmoothChangeItemClass AClass);
	virtual bool __fastcall CanAdd(Xcollection::TXCollectionItemClass AClass);
	__classmethod virtual Xcollection::TXCollectionItemClass __fastcall ItemsClass();
	__property TGLNavigatorSmoothChangeItem* Items[const int Index] = {read=GetItems, write=SetItems/*, default*/};
public:
	/* TXCollection.Create */ inline __fastcall virtual TGLNavigatorSmoothChangeItems(System::Classes::TPersistent* aOwner) : Xcollection::TXCollection(aOwner) { }
	/* TXCollection.Destroy */ inline __fastcall virtual ~TGLNavigatorSmoothChangeItems(void) { }
	
};

#pragma pack(pop)

class DELPHICLASS TGLNavigatorAdjustDistanceParameters;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLNavigatorAdjustDistanceParameters : public TGLNavigatorAbstractParameters
{
	typedef TGLNavigatorAbstractParameters inherited;
	
private:
	float FOldDistanceRatio;
	float FImpulseSpeed;
	bool __fastcall StoreImpulseSpeed(void);
	
public:
	__fastcall virtual TGLNavigatorAdjustDistanceParameters(System::Classes::TPersistent* AOwner);
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	virtual void __fastcall ScaleParameters(const float Value);
	virtual void __fastcall AddImpulse(const float Impulse);
	
__published:
	__property float ImpulseSpeed = {read=FImpulseSpeed, write=FImpulseSpeed, stored=StoreImpulseSpeed};
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TGLNavigatorAdjustDistanceParameters(void) { }
	
};

#pragma pack(pop)

class DELPHICLASS TGLNavigatorAdjustDistanceParametersEx;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLNavigatorAdjustDistanceParametersEx : public TGLNavigatorAbstractParameters
{
	typedef TGLNavigatorAbstractParameters inherited;
	
private:
	float FSpeedLimit;
	float FTargetDistance;
	bool __fastcall StoreSpeedLimit(void);
	bool __fastcall StoreTargetDistance(void);
	
protected:
	virtual bool __fastcall StoreSpeed(void);
	virtual bool __fastcall StoreInertia(void);
	
public:
	__fastcall virtual TGLNavigatorAdjustDistanceParametersEx(System::Classes::TPersistent* AOwner);
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	
__published:
	__property float TargetDistance = {read=FTargetDistance, write=FTargetDistance, stored=StoreTargetDistance};
	__property float SpeedLimit = {read=FSpeedLimit, write=FSpeedLimit, stored=StoreSpeedLimit};
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TGLNavigatorAdjustDistanceParametersEx(void) { }
	
};

#pragma pack(pop)

class DELPHICLASS TGLNavigatorInertiaParameters;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLNavigatorInertiaParameters : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
private:
	System::Classes::TPersistent* FOwner;
	float OldTurnHorizontalAngle;
	float OldTurnVerticalAngle;
	float OldMoveForwardDistance;
	float OldStrafeHorizontalDistance;
	float OldStrafeVerticalDistance;
	float FTurnInertia;
	float FTurnSpeed;
	float FTurnMaxAngle;
	float FMovementAcceleration;
	float FMovementInertia;
	float FMovementSpeed;
	bool __fastcall StoreTurnMaxAngle(void);
	bool __fastcall StoreMovementAcceleration(void);
	bool __fastcall StoreMovementInertia(void);
	bool __fastcall StoreMovementSpeed(void);
	bool __fastcall StoreTurnInertia(void);
	bool __fastcall StoreTurnSpeed(void);
	
protected:
	DYNAMIC System::Classes::TPersistent* __fastcall GetOwner(void);
	
public:
	__fastcall virtual TGLNavigatorInertiaParameters(System::Classes::TPersistent* AOwner);
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	virtual void __fastcall ScaleParameters(const float Value);
	
__published:
	__property float MovementAcceleration = {read=FMovementAcceleration, write=FMovementAcceleration, stored=StoreMovementAcceleration};
	__property float MovementInertia = {read=FMovementInertia, write=FMovementInertia, stored=StoreMovementInertia};
	__property float MovementSpeed = {read=FMovementSpeed, write=FMovementSpeed, stored=StoreMovementSpeed};
	__property float TurnMaxAngle = {read=FTurnMaxAngle, write=FTurnMaxAngle, stored=StoreTurnMaxAngle};
	__property float TurnInertia = {read=FTurnInertia, write=FTurnInertia, stored=StoreTurnInertia};
	__property float TurnSpeed = {read=FTurnSpeed, write=FTurnSpeed, stored=StoreTurnSpeed};
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TGLNavigatorInertiaParameters(void) { }
	
};

#pragma pack(pop)

class DELPHICLASS TGLNavigatorGeneralParameters;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLNavigatorGeneralParameters : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
private:
	System::Classes::TPersistent* FOwner;
	float FAutoScaleMin;
	float FAutoScaleMax;
	float FAutoScaleMult;
	bool __fastcall StoreAutoScaleMax(void);
	bool __fastcall StoreAutoScaleMin(void);
	bool __fastcall StoreAutoScaleMult(void);
	
protected:
	DYNAMIC System::Classes::TPersistent* __fastcall GetOwner(void);
	
public:
	__fastcall virtual TGLNavigatorGeneralParameters(System::Classes::TPersistent* AOwner);
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	
__published:
	__property float AutoScaleMin = {read=FAutoScaleMin, write=FAutoScaleMin, stored=StoreAutoScaleMin};
	__property float AutoScaleMax = {read=FAutoScaleMax, write=FAutoScaleMax, stored=StoreAutoScaleMax};
	__property float AutoScaleMult = {read=FAutoScaleMult, write=FAutoScaleMult, stored=StoreAutoScaleMult};
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TGLNavigatorGeneralParameters(void) { }
	
};

#pragma pack(pop)

class DELPHICLASS TGLNavigatorMoveAroundParameters;
class PASCALIMPLEMENTATION TGLNavigatorMoveAroundParameters : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
private:
	System::Classes::TPersistent* FOwner;
	Glscene::TGLBaseSceneObject* FTargetObject;
	float FOldPitchInertiaAngle;
	float FOldTurnInertiaAngle;
	float FPitchSpeed;
	float FTurnSpeed;
	float FInertia;
	float FMaxAngle;
	double FCutoff;
	bool __fastcall StoreInertia(void);
	bool __fastcall StoreMaxAngle(void);
	bool __fastcall StorePitchSpeed(void);
	bool __fastcall StoreTurnSpeed(void);
	void __fastcall SetTargetObject(Glscene::TGLBaseSceneObject* const Value);
	bool __fastcall StoreCutoff(void);
	
protected:
	DYNAMIC System::Classes::TPersistent* __fastcall GetOwner(void);
	
public:
	__fastcall virtual TGLNavigatorMoveAroundParameters(System::Classes::TPersistent* AOwner);
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	virtual void __fastcall ScaleParameters(const float Value);
	
__published:
	__property float Inertia = {read=FInertia, write=FInertia, stored=StoreInertia};
	__property float MaxAngle = {read=FMaxAngle, write=FMaxAngle, stored=StoreMaxAngle};
	__property float PitchSpeed = {read=FPitchSpeed, write=FPitchSpeed, stored=StorePitchSpeed};
	__property float TurnSpeed = {read=FTurnSpeed, write=FTurnSpeed, stored=StoreTurnSpeed};
	__property Glscene::TGLBaseSceneObject* TargetObject = {read=FTargetObject, write=SetTargetObject};
	__property double Cutoff = {read=FCutoff, write=FCutoff, stored=StoreCutoff};
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TGLNavigatorMoveAroundParameters(void) { }
	
};


class PASCALIMPLEMENTATION TGLSmoothNavigator : public Glnavigator::TGLNavigator
{
	typedef Glnavigator::TGLNavigator inherited;
	
private:
	double FMaxExpectedDeltaTime;
	TGLNavigatorInertiaParameters* FInertiaParams;
	TGLNavigatorGeneralParameters* FGeneralParams;
	TGLNavigatorMoveAroundParameters* FMoveAroundParams;
	TGLNavigatorAdjustDistanceParameters* FAdjustDistanceParams;
	TGLNavigatorAdjustDistanceParametersEx* FAdjustDistanceParamsEx;
	TGLNavigatorSmoothChangeItems* FCustomAnimatedItems;
	void __fastcall SetInertiaParams(TGLNavigatorInertiaParameters* const Value);
	bool __fastcall StoreMaxExpectedDeltaTime(void);
	void __fastcall SetGeneralParams(TGLNavigatorGeneralParameters* const Value);
	void __fastcall SetMoveAroundParams(TGLNavigatorMoveAroundParameters* const Value);
	void __fastcall SetAdjustDistanceParams(TGLNavigatorAdjustDistanceParameters* const Value);
	void __fastcall SetAdjustDistanceParamsEx(TGLNavigatorAdjustDistanceParametersEx* const Value);
	void __fastcall SetCustomAnimatedItems(TGLNavigatorSmoothChangeItems* const Value);
	
protected:
	virtual void __fastcall Notification(System::Classes::TComponent* AComponent, System::Classes::TOperation Operation);
	
public:
	__fastcall virtual TGLSmoothNavigator(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TGLSmoothNavigator(void);
	virtual void __fastcall SetObject(Glscene::TGLBaseSceneObject* Value);
	HIDESBASE virtual void __fastcall TurnHorizontal(float Angle, double ADeltaTime);
	HIDESBASE virtual void __fastcall TurnVertical(float Angle, double ADeltaTime);
	HIDESBASE virtual void __fastcall FlyForward(const bool Plus, const bool Minus, double ADeltaTime, const bool Accelerate = false);
	HIDESBASE virtual void __fastcall MoveForward(const bool Plus, const bool Minus, double ADeltaTime, const bool Accelerate = false);
	HIDESBASE virtual void __fastcall StrafeHorizontal(const bool Plus, const bool Minus, double ADeltaTime, const bool Accelerate = false);
	HIDESBASE virtual void __fastcall StrafeVertical(const bool Plus, const bool Minus, double ADeltaTime, const bool Accelerate = false);
	virtual bool __fastcall MoveAroundTarget(const float PitchDelta, const float TurnDelta, const double ADeltaTime);
	virtual bool __fastcall MoveObjectAround(Glscene::TGLBaseSceneObject* const AObject, float PitchDelta, float TurnDelta, double ADeltaTime);
	virtual bool __fastcall AdjustDistanceToPoint(const Vectortypes::TVector4f &APoint, const float DistanceRatio, double ADeltaTime);
	virtual bool __fastcall AdjustDistanceToTarget(const float DistanceRatio, const double ADeltaTime);
	virtual bool __fastcall AdjustDistanceToPointEx(const Vectortypes::TVector4f &APoint, double ADeltaTime);
	virtual bool __fastcall AdjustDistanceToTargetEx(const double ADeltaTime);
	virtual void __fastcall AnimateCustomItems(const double ADeltaTime);
	virtual void __fastcall ScaleParameters(const float Value);
	virtual void __fastcall AutoScaleParameters(const float FPS);
	virtual void __fastcall AutoScaleParametersUp(const float FPS);
	
__published:
	__property double MaxExpectedDeltaTime = {read=FMaxExpectedDeltaTime, write=FMaxExpectedDeltaTime, stored=StoreMaxExpectedDeltaTime};
	__property TGLNavigatorInertiaParameters* InertiaParams = {read=FInertiaParams, write=SetInertiaParams};
	__property TGLNavigatorGeneralParameters* GeneralParams = {read=FGeneralParams, write=SetGeneralParams};
	__property TGLNavigatorMoveAroundParameters* MoveAroundParams = {read=FMoveAroundParams, write=SetMoveAroundParams};
	__property TGLNavigatorAdjustDistanceParameters* AdjustDistanceParams = {read=FAdjustDistanceParams, write=SetAdjustDistanceParams};
	__property TGLNavigatorAdjustDistanceParametersEx* AdjustDistanceParamsEx = {read=FAdjustDistanceParamsEx, write=SetAdjustDistanceParamsEx};
	__property TGLNavigatorSmoothChangeItems* CustomAnimatedItems = {read=FCustomAnimatedItems, write=SetCustomAnimatedItems};
};


class DELPHICLASS TGLSmoothUserInterface;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLSmoothUserInterface : public System::Classes::TComponent
{
	typedef System::Classes::TComponent inherited;
	
private:
	bool FAutoUpdateMouse;
	bool FMouseLookActive;
	TGLSmoothNavigator* FSmoothNavigator;
	TGLSmoothNavigator* FSmoothVertNavigator;
	bool FInvertMouse;
	Glcoordinates::TGLCoordinates2* FOriginalMousePos;
	virtual void __fastcall SetSmoothNavigator(TGLSmoothNavigator* const Value);
	virtual void __fastcall SetOriginalMousePos(Glcoordinates::TGLCoordinates2* const Value);
	virtual void __fastcall SetSmoothVertNavigator(TGLSmoothNavigator* const Value);
	virtual void __fastcall SetMouseLookActive(const bool Value);
	
protected:
	virtual void __fastcall Notification(System::Classes::TComponent* AComponent, System::Classes::TOperation Operation);
	
public:
	__fastcall virtual TGLSmoothUserInterface(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TGLSmoothUserInterface(void);
	virtual void __fastcall TurnHorizontal(const float Angle, const double ADeltaTime);
	virtual void __fastcall TurnVertical(const float Angle, const double ADeltaTime);
	virtual void __fastcall MouseLookActiveToggle(void);
	bool __fastcall MouseLook(const double ADeltaTime)/* overload */;
	bool __fastcall MouseLook(const System::Types::TPoint &NewXY, const double ADeltaTime)/* overload */;
	bool __fastcall MouseLook(const int NewX, const int NewY, const double ADeltaTime)/* overload */;
	
__published:
	__property bool AutoUpdateMouse = {read=FAutoUpdateMouse, write=FAutoUpdateMouse, default=1};
	__property bool MouseLookActive = {read=FMouseLookActive, write=SetMouseLookActive, default=0};
	__property TGLSmoothNavigator* SmoothVertNavigator = {read=FSmoothVertNavigator, write=SetSmoothVertNavigator};
	__property TGLSmoothNavigator* SmoothNavigator = {read=FSmoothNavigator, write=SetSmoothNavigator};
	__property bool InvertMouse = {read=FInvertMouse, write=FInvertMouse, default=0};
	__property Glcoordinates::TGLCoordinates2* OriginalMousePos = {read=FOriginalMousePos, write=SetOriginalMousePos};
};

#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------
}	/* namespace Glsmoothnavigator */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLSMOOTHNAVIGATOR)
using namespace Glsmoothnavigator;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// GlsmoothnavigatorHPP
