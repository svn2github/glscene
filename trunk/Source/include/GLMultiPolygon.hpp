// CodeGear C++Builder
// Copyright (c) 1995, 2012 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLMultiPolygon.pas' rev: 24.00 (Win32)

#ifndef GlmultipolygonHPP
#define GlmultipolygonHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <System.Classes.hpp>	// Pascal unit
#include <OpenGLTokens.hpp>	// Pascal unit
#include <OpenGLAdapter.hpp>	// Pascal unit
#include <Spline.hpp>	// Pascal unit
#include <VectorGeometry.hpp>	// Pascal unit
#include <VectorLists.hpp>	// Pascal unit
#include <PersistentClasses.hpp>	// Pascal unit
#include <GLScene.hpp>	// Pascal unit
#include <GLObjects.hpp>	// Pascal unit
#include <GLGeomObjects.hpp>	// Pascal unit
#include <GLNodes.hpp>	// Pascal unit
#include <BaseClasses.hpp>	// Pascal unit
#include <GLCoordinates.hpp>	// Pascal unit
#include <GLRenderContextInfo.hpp>	// Pascal unit
#include <VectorTypes.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Glmultipolygon
{
//-- type declarations -------------------------------------------------------
class DELPHICLASS TGLContourNodes;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLContourNodes : public Glnodes::TGLNodes
{
	typedef Glnodes::TGLNodes inherited;
	
public:
	virtual void __fastcall NotifyChange(void);
public:
	/* TGLNodes.Create */ inline __fastcall TGLContourNodes(System::Classes::TPersistent* AOwner, System::Classes::TCollectionItemClass AItemClass) : Glnodes::TGLNodes(AOwner, AItemClass) { }
	
public:
	/* TCollection.Destroy */ inline __fastcall virtual ~TGLContourNodes(void) { }
	
};

#pragma pack(pop)

class DELPHICLASS TGLContour;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLContour : public System::Classes::TCollectionItem
{
	typedef System::Classes::TCollectionItem inherited;
	
private:
	TGLContourNodes* FNodes;
	int FDivision;
	Globjects::TLineSplineMode FSplineMode;
	System::UnicodeString FDescription;
	void __fastcall SetNodes(TGLContourNodes* const Value);
	void __fastcall SetDivision(int Value);
	void __fastcall SetSplineMode(const Globjects::TLineSplineMode Value);
	void __fastcall SetDescription(const System::UnicodeString Value);
	
protected:
	DYNAMIC void __fastcall CreateNodes(void);
	void __fastcall NodesChanged(System::TObject* Sender);
	virtual System::UnicodeString __fastcall GetDisplayName(void);
	
public:
	__fastcall virtual TGLContour(System::Classes::TCollection* Collection);
	__fastcall virtual ~TGLContour(void);
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	
__published:
	__property System::UnicodeString Description = {read=FDescription, write=SetDescription};
	__property TGLContourNodes* Nodes = {read=FNodes, write=SetNodes};
	__property int Division = {read=FDivision, write=SetDivision, default=10};
	__property Globjects::TLineSplineMode SplineMode = {read=FSplineMode, write=SetSplineMode, default=0};
};

#pragma pack(pop)

typedef System::TMetaClass* TGLContourClass;

class DELPHICLASS TGLContours;
class PASCALIMPLEMENTATION TGLContours : public Baseclasses::TNotifyCollection
{
	typedef Baseclasses::TNotifyCollection inherited;
	
public:
	TGLContour* operator[](int index) { return Items[index]; }
	
private:
	TGLContour* __fastcall GetItems(int index);
	void __fastcall SetItems(int index, TGLContour* const Value);
	
public:
	__fastcall TGLContours(System::Classes::TComponent* AOwner)/* overload */;
	HIDESBASE TGLContour* __fastcall Add(void);
	HIDESBASE TGLContour* __fastcall FindItemID(int ID);
	__property TGLContour* Items[int index] = {read=GetItems, write=SetItems/*, default*/};
	void __fastcall GetExtents(Vectortypes::TVector3f &min, Vectortypes::TVector3f &max);
public:
	/* TCollection.Destroy */ inline __fastcall virtual ~TGLContours(void) { }
	
};


class DELPHICLASS TPolygonList;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TPolygonList : public Persistentclasses::TPersistentObjectList
{
	typedef Persistentclasses::TPersistentObjectList inherited;
	
private:
	Vectorlists::TAffineVectorList* FAktList;
	Vectorlists::TAffineVectorList* __fastcall GetList(int I);
	
public:
	HIDESBASE void __fastcall Add(void);
	__property Vectorlists::TAffineVectorList* AktList = {read=FAktList};
	__property Vectorlists::TAffineVectorList* List[int I] = {read=GetList};
public:
	/* TPersistentObjectList.Create */ inline __fastcall virtual TPolygonList(void) : Persistentclasses::TPersistentObjectList() { }
	/* TPersistentObjectList.Destroy */ inline __fastcall virtual ~TPolygonList(void) { }
	
public:
	/* TPersistentObject.CreateFromFiler */ inline __fastcall TPolygonList(Persistentclasses::TVirtualReader* reader) : Persistentclasses::TPersistentObjectList(reader) { }
	
};

#pragma pack(pop)

class DELPHICLASS TMultiPolygonBase;
class PASCALIMPLEMENTATION TMultiPolygonBase : public Glscene::TGLSceneObject
{
	typedef Glscene::TGLSceneObject inherited;
	
private:
	TGLContours* FContours;
	TPolygonList* FOutline;
	Vectortypes::TVector3f FContoursNormal;
	Vectortypes::TVector4f FAxisAlignedDimensionsCache;
	void __fastcall SetContours(TGLContours* const Value);
	TGLContourNodes* __fastcall GetPath(int i);
	void __fastcall SetPath(int i, TGLContourNodes* const value);
	TPolygonList* __fastcall GetOutline(void);
	void __fastcall SetContoursNormal(const Vectortypes::TVector3f &Value);
	
protected:
	void __fastcall RenderTesselatedPolygon(bool textured, Vectorgeometry::PAffineVector normal, bool invertNormals);
	void __fastcall RetrieveOutline(TPolygonList* List);
	virtual void __fastcall ContourChanged(System::TObject* Sender);
	
public:
	__fastcall virtual TMultiPolygonBase(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TMultiPolygonBase(void);
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	void __fastcall AddNode(const int i, Glcoordinates::TGLCoordinates3* const coords)/* overload */;
	void __fastcall AddNode(const int i, const float X, const float Y, const float Z)/* overload */;
	void __fastcall AddNode(const int i, const Vectortypes::TVector4f &value)/* overload */;
	void __fastcall AddNode(const int i, const Vectortypes::TVector3f &value)/* overload */;
	__property TGLContourNodes* Path[int i] = {read=GetPath, write=SetPath};
	__property TPolygonList* Outline = {read=GetOutline};
	__property Vectortypes::TVector3f ContoursNormal = {read=FContoursNormal, write=SetContoursNormal};
	virtual Vectortypes::TVector4f __fastcall AxisAlignedDimensionsUnscaled(void);
	DYNAMIC void __fastcall StructureChanged(void);
	
__published:
	__property TGLContours* Contours = {read=FContours, write=SetContours};
public:
	/* TGLBaseSceneObject.CreateAsChild */ inline __fastcall TMultiPolygonBase(Glscene::TGLBaseSceneObject* aParentOwner) : Glscene::TGLSceneObject(aParentOwner) { }
	
};


class DELPHICLASS TGLMultiPolygon;
class PASCALIMPLEMENTATION TGLMultiPolygon : public TMultiPolygonBase
{
	typedef TMultiPolygonBase inherited;
	
private:
	Glgeomobjects::TPolygonParts FParts;
	
protected:
	void __fastcall SetParts(const Glgeomobjects::TPolygonParts value);
	
public:
	__fastcall virtual TGLMultiPolygon(System::Classes::TComponent* AOwner);
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	virtual void __fastcall BuildList(Glrendercontextinfo::TRenderContextInfo &rci);
	
__published:
	__property Glgeomobjects::TPolygonParts Parts = {read=FParts, write=SetParts, default=3};
public:
	/* TMultiPolygonBase.Destroy */ inline __fastcall virtual ~TGLMultiPolygon(void) { }
	
public:
	/* TGLBaseSceneObject.CreateAsChild */ inline __fastcall TGLMultiPolygon(Glscene::TGLBaseSceneObject* aParentOwner) : TMultiPolygonBase(aParentOwner) { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Glmultipolygon */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLMULTIPOLYGON)
using namespace Glmultipolygon;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// GlmultipolygonHPP
