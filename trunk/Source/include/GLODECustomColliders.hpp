// CodeGear C++Builder
// Copyright (c) 1995, 2012 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLODECustomColliders.pas' rev: 24.00 (Win32)

#ifndef GlodecustomcollidersHPP
#define GlodecustomcollidersHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <System.Classes.hpp>	// Pascal unit
#include <System.SysUtils.hpp>	// Pascal unit
#include <GLODEManager.hpp>	// Pascal unit
#include <ODEGL.hpp>	// Pascal unit
#include <ODEImport.hpp>	// Pascal unit
#include <VectorGeometry.hpp>	// Pascal unit
#include <VectorLists.hpp>	// Pascal unit
#include <GLScene.hpp>	// Pascal unit
#include <GLTerrainRenderer.hpp>	// Pascal unit
#include <GLGraph.hpp>	// Pascal unit
#include <XCollection.hpp>	// Pascal unit
#include <OpenGLTokens.hpp>	// Pascal unit
#include <GLContext.hpp>	// Pascal unit
#include <GLTexture.hpp>	// Pascal unit
#include <GLColor.hpp>	// Pascal unit
#include <GLRenderContextInfo.hpp>	// Pascal unit
#include <GLState.hpp>	// Pascal unit
#include <VectorTypes.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Glodecustomcolliders
{
//-- type declarations -------------------------------------------------------
class DELPHICLASS TContactPoint;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TContactPoint : public System::TObject
{
	typedef System::TObject inherited;
	
public:
	Vectortypes::TVector3f Position;
	Vectortypes::TVector3f Normal;
	float Depth;
public:
	/* TObject.Create */ inline __fastcall TContactPoint(void) : System::TObject() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TContactPoint(void) { }
	
};

#pragma pack(pop)

class DELPHICLASS TGLODECustomCollider;
class PASCALIMPLEMENTATION TGLODECustomCollider : public Glodemanager::TGLODEBehaviour
{
	typedef Glodemanager::TGLODEBehaviour inherited;
	
private:
	Odeimport::TdxGeom *FGeom;
	System::Classes::TList* FContactList;
	System::Classes::TList* FContactCache;
	Vectortypes::TMatrix4f FTransform;
	float FContactResolution;
	bool FRenderContacts;
	Vectorlists::TAffineVectorList* FContactRenderPoints;
	float FPointSize;
	Glcolor::TGLColor* FContactColor;
	
protected:
	virtual void __fastcall Initialize(void);
	virtual void __fastcall Finalize(void);
	virtual void __fastcall WriteToFiler(System::Classes::TWriter* writer);
	virtual void __fastcall ReadFromFiler(System::Classes::TReader* reader);
	virtual bool __fastcall Collide(const Vectortypes::TVector3f &aPos, float &Depth, Vectortypes::TVector3f &cPos, Vectortypes::TVector3f &cNorm) = 0 ;
	void __fastcall ClearContacts(void);
	void __fastcall AddContact(float x, float y, float z)/* overload */;
	void __fastcall AddContact(const Vectortypes::TVector3f &pos)/* overload */;
	int __fastcall ApplyContacts(Odeimport::PdxGeom o1, Odeimport::PdxGeom o2, int flags, Odeimport::PdContactGeom contact, int skip);
	void __fastcall SetTransform(const Vectortypes::TMatrix4f &ATransform);
	void __fastcall SetContactResolution(const float Value);
	void __fastcall SetRenderContacts(const bool Value);
	void __fastcall SetPointSize(const float Value);
	void __fastcall SetContactColor(Glcolor::TGLColor* const Value);
	
public:
	__fastcall virtual TGLODECustomCollider(Xcollection::TXCollection* AOwner);
	__fastcall virtual ~TGLODECustomCollider(void);
	virtual void __fastcall Render(Glrendercontextinfo::TRenderContextInfo &rci);
	__property Odeimport::PdxGeom Geom = {read=FGeom};
	
__published:
	__property float ContactResolution = {read=FContactResolution, write=SetContactResolution};
	__property bool RenderContacts = {read=FRenderContacts, write=SetRenderContacts, nodefault};
	__property float PointSize = {read=FPointSize, write=SetPointSize};
	__property Glcolor::TGLColor* ContactColor = {read=FContactColor, write=SetContactColor};
};


class DELPHICLASS TGLODEHeightField;
class PASCALIMPLEMENTATION TGLODEHeightField : public TGLODECustomCollider
{
	typedef TGLODECustomCollider inherited;
	
protected:
	virtual void __fastcall WriteToFiler(System::Classes::TWriter* writer);
	virtual void __fastcall ReadFromFiler(System::Classes::TReader* reader);
	virtual bool __fastcall Collide(const Vectortypes::TVector3f &aPos, float &Depth, Vectortypes::TVector3f &cPos, Vectortypes::TVector3f &cNorm);
	
public:
	__fastcall virtual TGLODEHeightField(Xcollection::TXCollection* AOwner);
	__classmethod virtual System::UnicodeString __fastcall FriendlyName();
	__classmethod virtual System::UnicodeString __fastcall FriendlyDescription();
	__classmethod virtual bool __fastcall UniqueItem();
	__classmethod virtual bool __fastcall CanAddTo(Xcollection::TXCollection* collection);
public:
	/* TGLODECustomCollider.Destroy */ inline __fastcall virtual ~TGLODEHeightField(void) { }
	
};


//-- var, const, procedure ---------------------------------------------------
extern PACKAGE TGLODEHeightField* __fastcall GetODEHeightField(Glscene::TGLBaseSceneObject* obj);
extern PACKAGE TGLODEHeightField* __fastcall GetOrCreateODEHeightField(Glscene::TGLBaseSceneObject* obj);
}	/* namespace Glodecustomcolliders */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLODECUSTOMCOLLIDERS)
using namespace Glodecustomcolliders;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// GlodecustomcollidersHPP
