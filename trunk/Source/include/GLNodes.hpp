// CodeGear C++Builder
// Copyright (c) 1995, 2012 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLNodes.pas' rev: 24.00 (Win32)

#ifndef GlnodesHPP
#define GlnodesHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <System.Classes.hpp>	// Pascal unit
#include <VectorGeometry.hpp>	// Pascal unit
#include <OpenGLTokens.hpp>	// Pascal unit
#include <OpenGLAdapter.hpp>	// Pascal unit
#include <GLContext.hpp>	// Pascal unit
#include <BaseClasses.hpp>	// Pascal unit
#include <GLCoordinates.hpp>	// Pascal unit
#include <Spline.hpp>	// Pascal unit
#include <VectorTypes.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Glnodes
{
//-- type declarations -------------------------------------------------------
class DELPHICLASS TGLNode;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLNode : public System::Classes::TCollectionItem
{
	typedef System::Classes::TCollectionItem inherited;
	
private:
	Vectortypes::TVector4f FCoords;
	System::TObject* FTagObject;
	void __fastcall SetAsVector(const Vectortypes::TVector4f &Value);
	void __fastcall SetAsAffineVector(const Vectortypes::TVector3f &Value);
	Vectortypes::TVector3f __fastcall GetAsAffineVector(void);
	void __fastcall SetCoordinate(int AIndex, float AValue);
	float __fastcall GetCoordinate(const int Index);
	
protected:
	bool __fastcall StoreCoordinate(int AIndex);
	virtual System::UnicodeString __fastcall GetDisplayName(void);
	
public:
	__fastcall virtual TGLNode(System::Classes::TCollection* ACollection);
	__fastcall virtual ~TGLNode(void);
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	System::PSingle __fastcall AsAddress(void);
	__property Vectortypes::TVector4f AsVector = {read=FCoords, write=SetAsVector};
	__property Vectortypes::TVector3f AsAffineVector = {read=GetAsAffineVector, write=SetAsAffineVector};
	__property float W = {read=GetCoordinate, write=SetCoordinate, stored=StoreCoordinate, index=3};
	__property System::TObject* TagObject = {read=FTagObject, write=FTagObject};
	
__published:
	__property float X = {read=GetCoordinate, write=SetCoordinate, stored=StoreCoordinate, index=0};
	__property float Y = {read=GetCoordinate, write=SetCoordinate, stored=StoreCoordinate, index=1};
	__property float Z = {read=GetCoordinate, write=SetCoordinate, stored=StoreCoordinate, index=2};
};

#pragma pack(pop)

class DELPHICLASS TGLNodes;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLNodes : public System::Classes::TOwnedCollection
{
	typedef System::Classes::TOwnedCollection inherited;
	
public:
	TGLNode* operator[](int index) { return Items[index]; }
	
protected:
	void __fastcall SetItems(int Index, TGLNode* const Val);
	TGLNode* __fastcall GetItems(int Index);
	virtual void __fastcall Update(System::Classes::TCollectionItem* Item);
	
public:
	__fastcall TGLNodes(System::Classes::TPersistent* AOwner, System::Classes::TCollectionItemClass AItemClass);
	TGLNodes* __fastcall CreateCopy(System::Classes::TPersistent* AOwner);
	HIDESBASE TGLNode* __fastcall Add(void);
	HIDESBASE TGLNode* __fastcall FindItemID(int ID);
	__property TGLNode* Items[int index] = {read=GetItems, write=SetItems/*, default*/};
	TGLNode* __fastcall First(void);
	TGLNode* __fastcall Last(void);
	virtual void __fastcall NotifyChange(void);
	virtual void __fastcall EndUpdate(void);
	void __fastcall AddNode(Glcoordinates::TGLCustomCoordinates* const Coords)/* overload */;
	void __fastcall AddNode(const float X, const float Y, const float Z)/* overload */;
	void __fastcall AddNode(const Vectortypes::TVector4f &Value)/* overload */;
	void __fastcall AddNode(const Vectortypes::TVector3f &Value)/* overload */;
	void __fastcall AddXYArc(float XRadius, float YRadius, float StartAngle, float StopAngle, int NbSegments, const Vectortypes::TVector3f &Center);
	Vectortypes::TVector3f __fastcall Barycenter(void);
	Vectortypes::TVector3f __fastcall Normal(void);
	Vectortypes::TVector3f __fastcall Vector(int I);
	void __fastcall GetExtents(Vectortypes::TVector3f &Min, Vectortypes::TVector3f &Max);
	void __fastcall Translate(const Vectortypes::TVector3f &Tv);
	void __fastcall Scale(const Vectortypes::TVector3f &Fv)/* overload */;
	void __fastcall Scale(float F)/* overload */;
	void __fastcall RotateAroundX(float Angle);
	void __fastcall RotateAroundY(float Angle);
	void __fastcall RotateAroundZ(float Angle);
	void __fastcall RenderTesselatedPolygon(bool ATextured, Vectorgeometry::PAffineVector ANormal = (Vectorgeometry::PAffineVector)(0x0), int ASplineDivisions = 0x1, bool AInvertNormals = false);
	Spline::TCubicSpline* __fastcall CreateNewCubicSpline(void);
public:
	/* TCollection.Destroy */ inline __fastcall virtual ~TGLNodes(void) { }
	
};

#pragma pack(pop)

typedef System::TMetaClass* TGLNodesClass;

//-- var, const, procedure ---------------------------------------------------
}	/* namespace Glnodes */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLNODES)
using namespace Glnodes;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// GlnodesHPP
