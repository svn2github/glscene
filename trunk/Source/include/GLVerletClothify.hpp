// CodeGear C++Builder
// Copyright (c) 1995, 2012 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLVerletClothify.pas' rev: 24.00 (Win32)

#ifndef GlverletclothifyHPP
#define GlverletclothifyHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <System.Classes.hpp>	// Pascal unit
#include <GLVectorFileObjects.hpp>	// Pascal unit
#include <VerletClasses.hpp>	// Pascal unit
#include <VectorTypes.hpp>	// Pascal unit
#include <VectorLists.hpp>	// Pascal unit
#include <VectorGeometry.hpp>	// Pascal unit
#include <GLTexture.hpp>	// Pascal unit
#include <OpenGLTokens.hpp>	// Pascal unit
#include <System.SysUtils.hpp>	// Pascal unit
#include <GLRenderContextInfo.hpp>	// Pascal unit
#include <GLState.hpp>	// Pascal unit
#include <SpatialPartitioning.hpp>	// Pascal unit
#include <PersistentClasses.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Glverletclothify
{
//-- type declarations -------------------------------------------------------
class DELPHICLASS TFace;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TFace : public System::TObject
{
	typedef System::TObject inherited;
	
public:
	System::StaticArray<int, 3> Vertices;
	Vectortypes::TVector3f Normal;
	Glvectorfileobjects::TMeshObject* MeshObject;
	bool Active;
	void __fastcall UpdateNormal(void);
	__fastcall TFace(Glvectorfileobjects::TMeshObject* aMeshObject);
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TFace(void) { }
	
};

#pragma pack(pop)

class DELPHICLASS TFaceList;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TFaceList : public System::Classes::TList
{
	typedef System::Classes::TList inherited;
	
public:
	TFace* operator[](int i) { return Items[i]; }
	
private:
	TFace* __fastcall GetItems(int i);
	void __fastcall SetItems(int i, TFace* const Value);
	
public:
	__property TFace* Items[int i] = {read=GetItems, write=SetItems/*, default*/};
public:
	/* TList.Destroy */ inline __fastcall virtual ~TFaceList(void) { }
	
public:
	/* TObject.Create */ inline __fastcall TFaceList(void) : System::Classes::TList() { }
	
};

#pragma pack(pop)

class DELPHICLASS TFaceExtractor;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TFaceExtractor : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	TFaceList* FFaceList;
	Glvectorfileobjects::TGLBaseMesh* FGLBaseMesh;
	Verletclasses::TVerletNodeList* FNodeList;
	float FWeldDistance;
	int FEdgeDoublesSkipped;
	void __fastcall SetWeldDistance(const float Value);
	
protected:
	virtual void __fastcall ProcessMeshObject(Glvectorfileobjects::TMeshObject* const MeshObject);
	
public:
	void __fastcall ExtractFacesFromVertexIndexList(Glvectorfileobjects::TFGVertexIndexList* const FaceGroup, Glvectorfileobjects::TMeshObject* const MeshObject);
	__property TFaceList* FaceList = {read=FFaceList};
	virtual void __fastcall Clear(void);
	virtual void __fastcall ProcessMesh(void);
	__property float WeldDistance = {read=FWeldDistance, write=SetWeldDistance};
	__property int EdgeDoublesSkipped = {read=FEdgeDoublesSkipped, nodefault};
	__property Glvectorfileobjects::TGLBaseMesh* GLBaseMesh = {read=FGLBaseMesh};
	__property Verletclasses::TVerletNodeList* NodeList = {read=FNodeList};
	virtual TFace* __fastcall AddFace(const int Vi0, const int Vi1, const int Vi2, Glvectorfileobjects::TMeshObject* const MeshObject);
	__fastcall virtual TFaceExtractor(Glvectorfileobjects::TGLBaseMesh* const aGLBaseMesh);
	__fastcall virtual ~TFaceExtractor(void);
};

#pragma pack(pop)

class DELPHICLASS TEdge;
class DELPHICLASS TEdgeDetector;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TEdge : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	bool FSolid;
	float FLength;
	Glvectorfileobjects::TMeshObject* FMeshObject;
	TEdgeDetector* FOwner;
	
public:
	System::StaticArray<int, 2> Vertices;
	System::StaticArray<TFace*, 2> Faces;
	void __fastcall Contract(void);
	__property TEdgeDetector* Owner = {read=FOwner};
	__property Glvectorfileobjects::TMeshObject* MeshObject = {read=FMeshObject, write=FMeshObject};
	__property float Length = {read=FLength, write=FLength};
	__property bool Solid = {read=FSolid, write=FSolid, nodefault};
	void __fastcall UpdateEdgeLength(void);
	__fastcall TEdge(TEdgeDetector* const AOwner, int AVi0, int AVi1, TFace* AFace0, TFace* AFace1, Glvectorfileobjects::TMeshObject* AMeshObject, bool ASolid);
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TEdge(void) { }
	
};

#pragma pack(pop)

class DELPHICLASS TEdgeList;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TEdgeList : public System::Classes::TList
{
	typedef System::Classes::TList inherited;
	
public:
	TEdge* operator[](int i) { return Items[i]; }
	
private:
	TEdge* __fastcall GetItems(int i);
	void __fastcall SetItems(int i, TEdge* const Value);
	
public:
	__property TEdge* Items[int i] = {read=GetItems, write=SetItems/*, default*/};
	void __fastcall SortByLength(void);
	int __fastcall InsertSorted(TEdge* AEdge);
public:
	/* TList.Destroy */ inline __fastcall virtual ~TEdgeList(void) { }
	
public:
	/* TObject.Create */ inline __fastcall TEdgeList(void) : System::Classes::TList() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TEdgeDetector : public TFaceExtractor
{
	typedef TFaceExtractor inherited;
	
private:
	TEdgeList* FEdgeList;
	int FCurrentNodeOffset;
	bool FNodesAdded;
	void __fastcall BuildOpposingEdges(void);
	
protected:
	bool FCalcEdgeLength;
	
public:
	__property TEdgeList* EdgeList = {read=FEdgeList};
	virtual void __fastcall Clear(void);
	virtual void __fastcall ProcessMesh(void);
	TEdge* __fastcall AddEdge(const int Vi0, const int Vi1, TFace* const Face, Glvectorfileobjects::TMeshObject* const AMeshObject);
	virtual TFace* __fastcall AddFace(const int Vi0, const int Vi1, const int Vi2, Glvectorfileobjects::TMeshObject* const MeshObject);
	virtual Verletclasses::TVerletNode* __fastcall AddNode(Verletclasses::TVerletWorld* const VerletWorld, Glvectorfileobjects::TMeshObject* const MeshObject, const int VertexIndex);
	void __fastcall AddNodes(Verletclasses::TVerletWorld* const VerletWorld);
	void __fastcall AddEdgesAsSticks(Verletclasses::TVerletWorld* const VerletWorld, const float Slack);
	void __fastcall AddEdgesAsSprings(Verletclasses::TVerletWorld* const VerletWorld, const float Strength, const float Damping, const float Slack);
	void __fastcall AddEdgesAsSolidEdges(Verletclasses::TVerletWorld* const VerletWorld);
	void __fastcall AddOuterEdgesAsSolidEdges(Verletclasses::TVerletWorld* const VerletWorld);
	void __fastcall RenderEdges(Glrendercontextinfo::TRenderContextInfo &rci);
	__property int CurrentNodeOffset = {read=FCurrentNodeOffset, nodefault};
	__property bool NodesAdded = {read=FNodesAdded, nodefault};
	void __fastcall ReplaceVertexIndex(const int ViRemove, const int ViReplaceWith);
	__fastcall virtual TEdgeDetector(Glvectorfileobjects::TGLBaseMesh* const aGLBaseMesh);
	__fastcall virtual ~TEdgeDetector(void);
};

#pragma pack(pop)

class DELPHICLASS TMeshObjectVerletNode;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TMeshObjectVerletNode : public Verletclasses::TVerletNode
{
	typedef Verletclasses::TVerletNode inherited;
	
private:
	Glvectorfileobjects::TMeshObject* MeshObject;
	Vectorlists::TIntegerList* VertexIndices;
	
public:
	virtual void __fastcall AfterProgress(void);
	__fastcall virtual TMeshObjectVerletNode(Verletclasses::TVerletWorld* const aOwner);
	__fastcall virtual ~TMeshObjectVerletNode(void);
public:
	/* TPersistentObject.Create */ inline __fastcall virtual TMeshObjectVerletNode(void) : Verletclasses::TVerletNode() { }
	/* TPersistentObject.CreateFromFiler */ inline __fastcall TMeshObjectVerletNode(Persistentclasses::TVirtualReader* reader) : Verletclasses::TVerletNode(reader) { }
	
};

#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------
}	/* namespace Glverletclothify */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLVERLETCLOTHIFY)
using namespace Glverletclothify;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// GlverletclothifyHPP
