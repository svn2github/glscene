// CodeGear C++Builder
// Copyright (c) 1995, 2012 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLMesh.pas' rev: 24.00 (Win32)

#ifndef GlmeshHPP
#define GlmeshHPP

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
#include <OpenGLTokens.hpp>	// Pascal unit
#include <OpenGLAdapter.hpp>	// Pascal unit
#include <GLState.hpp>	// Pascal unit
#include <GLColor.hpp>	// Pascal unit
#include <BaseClasses.hpp>	// Pascal unit
#include <GLRenderContextInfo.hpp>	// Pascal unit
#include <VectorTypes.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Glmesh
{
//-- type declarations -------------------------------------------------------
enum TMeshMode : unsigned char { mmTriangleStrip, mmTriangleFan, mmTriangles, mmQuadStrip, mmQuads, mmPolygon };

enum TVertexMode : unsigned char { vmV, vmVN, vmVNC, vmVNCT, vmVNT, vmVT };

#pragma pack(push,1)
struct DECLSPEC_DRECORD TVertexData
{
public:
	Vectorgeometry::TTexPoint textCoord;
	Vectortypes::TVector4f color;
	Vectortypes::TVector3f normal;
	Vectortypes::TVector3f coord;
};
#pragma pack(pop)


typedef TVertexData *PVertexData;

typedef System::StaticArray<TVertexData, 33554432> TVertexDataArray;

typedef TVertexDataArray *PVertexDataArray;

class DELPHICLASS TVertexList;
class PASCALIMPLEMENTATION TVertexList : public Baseclasses::TGLUpdateAbleObject
{
	typedef Baseclasses::TGLUpdateAbleObject inherited;
	
public:
	TVertexData operator[](int index) { return Vertices[index]; }
	
private:
	TVertexDataArray *FValues;
	int FCount;
	int FCapacity;
	int FGrowth;
	TVertexDataArray *FLockedOldValues;
	
protected:
	void __fastcall SetCapacity(const int val);
	void __fastcall SetGrowth(const int val);
	void __fastcall Grow(void);
	void __fastcall SetVertices(int index, const TVertexData &val);
	TVertexData __fastcall GetVertices(int index);
	void __fastcall SetVertexCoord(int index, const Vectortypes::TVector3f &val);
	Vectortypes::TVector3f __fastcall GetVertexCoord(int index);
	void __fastcall SetVertexNormal(int index, const Vectortypes::TVector3f &val);
	Vectortypes::TVector3f __fastcall GetVertexNormal(int index);
	void __fastcall SetVertexTexCoord(int index, const Vectorgeometry::TTexPoint &val);
	Vectorgeometry::TTexPoint __fastcall GetVertexTexCoord(int index);
	void __fastcall SetVertexColor(int index, const Vectortypes::TVector4f &val);
	Vectortypes::TVector4f __fastcall GetVertexColor(int index);
	System::PSingle __fastcall GetFirstEntry(void);
	System::PSingle __fastcall GetFirstColor(void);
	System::PSingle __fastcall GetFirstNormal(void);
	System::PSingle __fastcall GetFirstVertex(void);
	System::PSingle __fastcall GetFirstTexPoint(void);
	bool __fastcall GetLocked(void);
	void __fastcall SetLocked(bool val);
	
public:
	__fastcall virtual TVertexList(System::Classes::TPersistent* AOwner);
	__fastcall virtual ~TVertexList(void);
	TVertexList* __fastcall CreateInterpolatedCoords(TVertexList* list2, float lerpFactor);
	void __fastcall AddVertex(const TVertexData &vertexData)/* overload */;
	void __fastcall AddVertex3(const TVertexData &vd1, const TVertexData &vd2, const TVertexData &vd3)/* overload */;
	void __fastcall AddVertex(const Vectortypes::TVector3f &aVertex, const Vectortypes::TVector3f &aNormal, const Vectortypes::TVector4f &aColor, const Vectorgeometry::TTexPoint &aTexPoint)/* overload */;
	void __fastcall AddVertex(const Vectortypes::TVector3f &vertex, const Vectortypes::TVector3f &normal, const Vectortypes::TVector4f &color)/* overload */;
	void __fastcall AddVertex(const Vectortypes::TVector3f &vertex, const Vectortypes::TVector3f &normal)/* overload */;
	void __fastcall DuplicateVertex(int index);
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	void __fastcall Clear(void);
	__property TVertexData Vertices[int index] = {read=GetVertices, write=SetVertices/*, default*/};
	__property Vectortypes::TVector3f VertexCoord[int index] = {read=GetVertexCoord, write=SetVertexCoord};
	__property Vectortypes::TVector3f VertexNormal[int index] = {read=GetVertexNormal, write=SetVertexNormal};
	__property Vectorgeometry::TTexPoint VertexTexCoord[int index] = {read=GetVertexTexCoord, write=SetVertexTexCoord};
	__property Vectortypes::TVector4f VertexColor[int index] = {read=GetVertexColor, write=SetVertexColor};
	__property int Count = {read=FCount, nodefault};
	__property int Capacity = {read=FCapacity, write=SetCapacity, nodefault};
	__property int Growth = {read=FGrowth, write=SetGrowth, nodefault};
	Vectortypes::TVector3f __fastcall SumVertexCoords(void);
	void __fastcall GetExtents(Vectortypes::TVector3f &min, Vectortypes::TVector3f &max);
	void __fastcall NormalizeNormals(void);
	void __fastcall Translate(const Vectortypes::TVector3f &v);
	void __fastcall DefineOpenGLArrays(void);
	__property System::PSingle FirstColor = {read=GetFirstColor};
	__property System::PSingle FirstEntry = {read=GetFirstEntry};
	__property System::PSingle FirstNormal = {read=GetFirstNormal};
	__property System::PSingle FirstVertex = {read=GetFirstVertex};
	__property System::PSingle FirstTexPoint = {read=GetFirstTexPoint};
	__property bool Locked = {read=GetLocked, write=SetLocked, nodefault};
	void __fastcall EnterLockSection(void);
	void __fastcall LeaveLockSection(void);
};


class DELPHICLASS TGLMesh;
class PASCALIMPLEMENTATION TGLMesh : public Glscene::TGLSceneObject
{
	typedef Glscene::TGLSceneObject inherited;
	
private:
	TVertexList* FVertices;
	TMeshMode FMode;
	TVertexMode FVertexMode;
	Vectortypes::TVector4f FAxisAlignedDimensionsCache;
	
protected:
	void __fastcall SetMode(TMeshMode AValue);
	void __fastcall SetVertices(TVertexList* AValue);
	void __fastcall SetVertexMode(TVertexMode AValue);
	void __fastcall VerticesChanged(System::TObject* Sender);
	
public:
	__fastcall virtual TGLMesh(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TGLMesh(void);
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	virtual void __fastcall BuildList(Glrendercontextinfo::TRenderContextInfo &rci);
	void __fastcall CalcNormals(Glstate::TFaceWinding Frontface);
	__property TVertexList* Vertices = {read=FVertices, write=SetVertices};
	virtual Vectortypes::TVector4f __fastcall AxisAlignedDimensionsUnscaled(void);
	DYNAMIC void __fastcall StructureChanged(void);
	
__published:
	__property TMeshMode Mode = {read=FMode, write=SetMode, nodefault};
	__property TVertexMode VertexMode = {read=FVertexMode, write=SetVertexMode, default=3};
public:
	/* TGLBaseSceneObject.CreateAsChild */ inline __fastcall TGLMesh(Glscene::TGLBaseSceneObject* aParentOwner) : Glscene::TGLSceneObject(aParentOwner) { }
	
};


//-- var, const, procedure ---------------------------------------------------
extern PACKAGE System::StaticArray<unsigned, 6> cMeshModeToGLEnum;
extern PACKAGE System::StaticArray<unsigned, 6> cVertexModeToGLEnum;
}	/* namespace Glmesh */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLMESH)
using namespace Glmesh;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// GlmeshHPP
