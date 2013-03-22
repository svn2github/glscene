// CodeGear C++Builder
// Copyright (c) 1995, 2012 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLMultiProxy.pas' rev: 24.00 (Win32)

#ifndef GlmultiproxyHPP
#define GlmultiproxyHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <System.Classes.hpp>	// Pascal unit
#include <GLScene.hpp>	// Pascal unit
#include <VectorGeometry.hpp>	// Pascal unit
#include <GLSilhouette.hpp>	// Pascal unit
#include <GLRenderContextInfo.hpp>	// Pascal unit
#include <BaseClasses.hpp>	// Pascal unit
#include <VectorTypes.hpp>	// Pascal unit
#include <GLCoordinates.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Glmultiproxy
{
//-- type declarations -------------------------------------------------------
class DELPHICLASS TGLMultiProxyMaster;
class DELPHICLASS TGLMultiProxy;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLMultiProxyMaster : public System::Classes::TCollectionItem
{
	typedef System::Classes::TCollectionItem inherited;
	
private:
	Glscene::TGLBaseSceneObject* FMasterObject;
	float FDistanceMin;
	float FDistanceMin2;
	float FDistanceMax;
	float FDistanceMax2;
	bool FVisible;
	
protected:
	virtual System::UnicodeString __fastcall GetDisplayName(void);
	void __fastcall SetMasterObject(Glscene::TGLBaseSceneObject* const val);
	void __fastcall SetDistanceMin(const float val);
	void __fastcall SetDistanceMax(const float val);
	void __fastcall SetVisible(const bool val);
	
public:
	__fastcall virtual TGLMultiProxyMaster(System::Classes::TCollection* Collection);
	__fastcall virtual ~TGLMultiProxyMaster(void);
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	TGLMultiProxy* __fastcall OwnerObject(void);
	void __fastcall NotifyChange(void);
	
__published:
	__property Glscene::TGLBaseSceneObject* MasterObject = {read=FMasterObject, write=SetMasterObject};
	__property float DistanceMin = {read=FDistanceMin, write=SetDistanceMin};
	__property float DistanceMax = {read=FDistanceMax, write=SetDistanceMax};
	__property bool Visible = {read=FVisible, write=SetVisible, default=1};
};

#pragma pack(pop)

class DELPHICLASS TGLMultiProxyMasters;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLMultiProxyMasters : public System::Classes::TOwnedCollection
{
	typedef System::Classes::TOwnedCollection inherited;
	
public:
	TGLMultiProxyMaster* operator[](int index) { return Items[index]; }
	
protected:
	void __fastcall SetItems(int index, TGLMultiProxyMaster* const val);
	TGLMultiProxyMaster* __fastcall GetItems(int index);
	virtual void __fastcall Update(System::Classes::TCollectionItem* Item);
	
public:
	__fastcall TGLMultiProxyMasters(System::Classes::TPersistent* AOwner);
	HIDESBASE TGLMultiProxyMaster* __fastcall Add(void)/* overload */;
	HIDESBASE TGLMultiProxyMaster* __fastcall Add(Glscene::TGLBaseSceneObject* master, float distanceMin, float distanceMax)/* overload */;
	__property TGLMultiProxyMaster* Items[int index] = {read=GetItems, write=SetItems/*, default*/};
	void __fastcall Notification(System::Classes::TComponent* AComponent);
	void __fastcall NotifyChange(void);
	virtual void __fastcall EndUpdate(void);
public:
	/* TCollection.Destroy */ inline __fastcall virtual ~TGLMultiProxyMasters(void) { }
	
};

#pragma pack(pop)

class PASCALIMPLEMENTATION TGLMultiProxy : public Glscene::TGLSceneObject
{
	typedef Glscene::TGLSceneObject inherited;
	
private:
	TGLMultiProxyMasters* FMasterObjects;
	bool FRendering;
	
protected:
	void __fastcall SetMasterObjects(TGLMultiProxyMasters* const val);
	virtual void __fastcall Notification(System::Classes::TComponent* AComponent, System::Classes::TOperation Operation);
	Glscene::TGLBaseSceneObject* __fastcall PrimaryMaster(void);
	
public:
	__fastcall virtual TGLMultiProxy(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TGLMultiProxy(void);
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	virtual void __fastcall DoRender(Glrendercontextinfo::TRenderContextInfo &rci, bool renderSelf, bool renderChildren);
	virtual Vectortypes::TVector4f __fastcall AxisAlignedDimensionsUnscaled(void);
	virtual bool __fastcall RayCastIntersect(const Vectortypes::TVector4f &rayStart, const Vectortypes::TVector4f &rayVector, Vectorgeometry::PVector intersectPoint = (Vectorgeometry::PVector)(0x0), Vectorgeometry::PVector intersectNormal = (Vectorgeometry::PVector)(0x0));
	virtual Glsilhouette::TGLSilhouette* __fastcall GenerateSilhouette(const Glsilhouette::TGLSilhouetteParameters &silhouetteParameters);
	
__published:
	__property TGLMultiProxyMasters* MasterObjects = {read=FMasterObjects, write=SetMasterObjects};
	__property ObjectsSorting = {default=0};
	__property Direction;
	__property PitchAngle = {default=0};
	__property Position;
	__property RollAngle = {default=0};
	__property Scale;
	__property ShowAxes = {default=0};
	__property TurnAngle = {default=0};
	__property Up;
	__property Visible = {default=1};
	__property OnProgress;
	__property Behaviours;
public:
	/* TGLBaseSceneObject.CreateAsChild */ inline __fastcall TGLMultiProxy(Glscene::TGLBaseSceneObject* aParentOwner) : Glscene::TGLSceneObject(aParentOwner) { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Glmultiproxy */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLMULTIPROXY)
using namespace Glmultiproxy;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// GlmultiproxyHPP
