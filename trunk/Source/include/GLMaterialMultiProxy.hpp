// CodeGear C++Builder
// Copyright (c) 1995, 2012 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLMaterialMultiProxy.pas' rev: 24.00 (Win32)

#ifndef GlmaterialmultiproxyHPP
#define GlmaterialmultiproxyHPP

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
#include <GLTexture.hpp>	// Pascal unit
#include <GLMaterial.hpp>	// Pascal unit
#include <GLSilhouette.hpp>	// Pascal unit
#include <GLStrings.hpp>	// Pascal unit
#include <GLCrossPlatform.hpp>	// Pascal unit
#include <PersistentClasses.hpp>	// Pascal unit
#include <GLRenderContextInfo.hpp>	// Pascal unit
#include <BaseClasses.hpp>	// Pascal unit
#include <VectorTypes.hpp>	// Pascal unit
#include <GLCoordinates.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Glmaterialmultiproxy
{
//-- type declarations -------------------------------------------------------
class DELPHICLASS TGLMaterialMultiProxyMaster;
class DELPHICLASS TGLMaterialMultiProxy;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLMaterialMultiProxyMaster : public Persistentclasses::TGLInterfacedCollectionItem
{
	typedef Persistentclasses::TGLInterfacedCollectionItem inherited;
	
private:
	Glscene::TGLBaseSceneObject* FMasterObject;
	Glmaterial::TGLLibMaterial* FMasterLibMaterial;
	System::UnicodeString FTempLibMaterialName;
	float FDistanceMin2;
	float FDistanceMax2;
	void __fastcall SetMasterLibMaterialName(const System::UnicodeString Value);
	System::UnicodeString __fastcall GetMasterLibMaterialName(void);
	Glmaterial::TGLAbstractMaterialLibrary* __fastcall GetMaterialLibrary(void);
	
protected:
	virtual System::UnicodeString __fastcall GetDisplayName(void);
	void __fastcall SetMasterObject(Glscene::TGLBaseSceneObject* const Val);
	void __fastcall SetDistanceMin(const float Val);
	void __fastcall SetDistanceMax(const float Val);
	float __fastcall GetDistanceMin(void);
	float __fastcall GetDistanceMax(void);
	
public:
	__fastcall virtual TGLMaterialMultiProxyMaster(System::Classes::TCollection* Collection);
	__fastcall virtual ~TGLMaterialMultiProxyMaster(void);
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	TGLMaterialMultiProxy* __fastcall OwnerObject(void);
	void __fastcall NotifyChange(void);
	__property Glmaterial::TGLLibMaterial* MasterLibMaterial = {read=FMasterLibMaterial, write=FMasterLibMaterial, stored=false};
	
__published:
	__property Glscene::TGLBaseSceneObject* MasterObject = {read=FMasterObject, write=SetMasterObject};
	__property System::UnicodeString MasterLibMaterialName = {read=GetMasterLibMaterialName, write=SetMasterLibMaterialName};
	__property float DistanceMin = {read=GetDistanceMin, write=SetDistanceMin};
	__property float DistanceMax = {read=GetDistanceMax, write=SetDistanceMax};
private:
	void *__IGLMaterialLibrarySupported;	/* Glmaterial::IGLMaterialLibrarySupported */
	
public:
	#if defined(MANAGED_INTERFACE_OPERATORS)
	// {8E442AF9-D212-4A5E-8A88-92F798BABFD1}
	operator Glmaterial::_di_IGLMaterialLibrarySupported()
	{
		Glmaterial::_di_IGLMaterialLibrarySupported intf;
		GetInterface(intf);
		return intf;
	}
	#else
	operator Glmaterial::IGLMaterialLibrarySupported*(void) { return (Glmaterial::IGLMaterialLibrarySupported*)&__IGLMaterialLibrarySupported; }
	#endif
	
};

#pragma pack(pop)

class DELPHICLASS TGLMaterialMultiProxyMasters;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLMaterialMultiProxyMasters : public System::Classes::TOwnedCollection
{
	typedef System::Classes::TOwnedCollection inherited;
	
public:
	TGLMaterialMultiProxyMaster* operator[](int index) { return Items[index]; }
	
protected:
	void __fastcall SetItems(int index, TGLMaterialMultiProxyMaster* const Val);
	TGLMaterialMultiProxyMaster* __fastcall GetItems(int index);
	virtual void __fastcall Update(System::Classes::TCollectionItem* Item);
	virtual void __fastcall Notification(System::Classes::TComponent* AComponent);
	
public:
	__fastcall TGLMaterialMultiProxyMasters(System::Classes::TPersistent* AOwner);
	HIDESBASE TGLMaterialMultiProxyMaster* __fastcall Add(void)/* overload */;
	HIDESBASE TGLMaterialMultiProxyMaster* __fastcall Add(Glscene::TGLBaseSceneObject* Master, float DistanceMin, float DistanceMax)/* overload */;
	HIDESBASE TGLMaterialMultiProxyMaster* __fastcall Add(Glscene::TGLBaseSceneObject* Master, Glmaterial::TGLLibMaterial* MasterLibMaterial, float DistanceMin, float DistanceMax)/* overload */;
	__property TGLMaterialMultiProxyMaster* Items[int index] = {read=GetItems, write=SetItems/*, default*/};
	void __fastcall NotifyChange(void);
	virtual void __fastcall EndUpdate(void);
public:
	/* TCollection.Destroy */ inline __fastcall virtual ~TGLMaterialMultiProxyMasters(void) { }
	
};

#pragma pack(pop)

class PASCALIMPLEMENTATION TGLMaterialMultiProxy : public Glscene::TGLBaseSceneObject
{
	typedef Glscene::TGLBaseSceneObject inherited;
	
private:
	TGLMaterialMultiProxyMasters* FMasterObjects;
	bool FRendering;
	Glmaterial::TGLMaterialLibrary* FMaterialLibrary;
	void __fastcall SetMaterialLibrary(Glmaterial::TGLMaterialLibrary* const Value);
	
protected:
	void __fastcall SetMasterObjects(TGLMaterialMultiProxyMasters* const Val);
	virtual void __fastcall Notification(System::Classes::TComponent* AComponent, System::Classes::TOperation Operation);
	Glscene::TGLBaseSceneObject* __fastcall PrimaryMaster(void);
	
public:
	__fastcall virtual TGLMaterialMultiProxy(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TGLMaterialMultiProxy(void);
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	virtual void __fastcall DoRender(Glrendercontextinfo::TRenderContextInfo &rci, bool renderSelf, bool renderChildren);
	virtual Vectortypes::TVector4f __fastcall AxisAlignedDimensionsUnscaled(void);
	virtual bool __fastcall RayCastIntersect(const Vectortypes::TVector4f &rayStart, const Vectortypes::TVector4f &rayVector, Vectorgeometry::PVector intersectPoint = (Vectorgeometry::PVector)(0x0), Vectorgeometry::PVector intersectNormal = (Vectorgeometry::PVector)(0x0));
	virtual Glsilhouette::TGLSilhouette* __fastcall GenerateSilhouette(const Glsilhouette::TGLSilhouetteParameters &silhouetteParameters);
	
__published:
	__property TGLMaterialMultiProxyMasters* MasterObjects = {read=FMasterObjects, write=SetMasterObjects};
	__property Glmaterial::TGLMaterialLibrary* MaterialLibrary = {read=FMaterialLibrary, write=SetMaterialLibrary};
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
	__property Effects;
public:
	/* TGLBaseSceneObject.CreateAsChild */ inline __fastcall TGLMaterialMultiProxy(Glscene::TGLBaseSceneObject* aParentOwner) : Glscene::TGLBaseSceneObject(aParentOwner) { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Glmaterialmultiproxy */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLMATERIALMULTIPROXY)
using namespace Glmaterialmultiproxy;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// GlmaterialmultiproxyHPP
