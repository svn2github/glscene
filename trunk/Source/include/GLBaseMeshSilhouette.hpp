// CodeGear C++Builder
// Copyright (c) 1995, 2012 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLBaseMeshSilhouette.pas' rev: 24.00 (Win32)

#ifndef GlbasemeshsilhouetteHPP
#define GlbasemeshsilhouetteHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <System.Classes.hpp>	// Pascal unit
#include <VectorGeometry.hpp>	// Pascal unit
#include <VectorLists.hpp>	// Pascal unit
#include <GLVectorFileObjects.hpp>	// Pascal unit
#include <GLSilhouette.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Glbasemeshsilhouette
{
//-- type declarations -------------------------------------------------------
class DELPHICLASS TFaceGroupConnectivity;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TFaceGroupConnectivity : public Glsilhouette::TConnectivity
{
	typedef Glsilhouette::TConnectivity inherited;
	
private:
	Glvectorfileobjects::TMeshObject* FMeshObject;
	bool FOwnsVertices;
	void __fastcall SetMeshObject(Glvectorfileobjects::TMeshObject* const Value);
	
public:
	virtual void __fastcall Clear(void);
	void __fastcall RebuildEdgeList(void);
	__property Glvectorfileobjects::TMeshObject* MeshObject = {read=FMeshObject, write=SetMeshObject};
	__fastcall virtual TFaceGroupConnectivity(bool APrecomputeFaceNormal);
	__fastcall TFaceGroupConnectivity(Glvectorfileobjects::TMeshObject* aMeshObject, bool APrecomputeFaceNormal);
	__fastcall virtual ~TFaceGroupConnectivity(void);
};

#pragma pack(pop)

class DELPHICLASS TGLBaseMeshConnectivity;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLBaseMeshConnectivity : public Glsilhouette::TBaseConnectivity
{
	typedef Glsilhouette::TBaseConnectivity inherited;
	
private:
	Glvectorfileobjects::TGLBaseMesh* FGLBaseMesh;
	System::Classes::TList* FFaceGroupConnectivityList;
	TFaceGroupConnectivity* __fastcall GetFaceGroupConnectivity(int i);
	int __fastcall GetConnectivityCount(void);
	void __fastcall SetGLBaseMesh(Glvectorfileobjects::TGLBaseMesh* const Value);
	
protected:
	virtual int __fastcall GetEdgeCount(void);
	virtual int __fastcall GetFaceCount(void);
	
public:
	__property int ConnectivityCount = {read=GetConnectivityCount, nodefault};
	__property TFaceGroupConnectivity* FaceGroupConnectivity[int i] = {read=GetFaceGroupConnectivity};
	__property Glvectorfileobjects::TGLBaseMesh* GLBaseMesh = {read=FGLBaseMesh, write=SetGLBaseMesh};
	void __fastcall Clear(bool SaveFaceGroupConnectivity);
	void __fastcall RebuildEdgeList(void);
	virtual void __fastcall CreateSilhouette(const Glsilhouette::TGLSilhouetteParameters &silhouetteParameters, Glsilhouette::TGLSilhouette* &aSilhouette, bool AddToSilhouette);
	__fastcall virtual TGLBaseMeshConnectivity(bool APrecomputeFaceNormal);
	__fastcall TGLBaseMeshConnectivity(Glvectorfileobjects::TGLBaseMesh* aGLBaseMesh);
	__fastcall virtual ~TGLBaseMeshConnectivity(void);
};

#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------
}	/* namespace Glbasemeshsilhouette */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLBASEMESHSILHOUETTE)
using namespace Glbasemeshsilhouette;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// GlbasemeshsilhouetteHPP
