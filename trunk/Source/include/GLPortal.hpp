// CodeGear C++Builder
// Copyright (c) 1995, 2012 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLPortal.pas' rev: 24.00 (Win32)

#ifndef GlportalHPP
#define GlportalHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <System.Classes.hpp>	// Pascal unit
#include <GLVectorFileObjects.hpp>	// Pascal unit
#include <GLScene.hpp>	// Pascal unit
#include <GLMaterial.hpp>	// Pascal unit
#include <VectorGeometry.hpp>	// Pascal unit
#include <GLRenderContextInfo.hpp>	// Pascal unit
#include <PersistentClasses.hpp>	// Pascal unit
#include <VectorTypes.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Glportal
{
//-- type declarations -------------------------------------------------------
class DELPHICLASS TPortalMeshObjectList;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TPortalMeshObjectList : public Glvectorfileobjects::TMeshObjectList
{
	typedef Glvectorfileobjects::TMeshObjectList inherited;
	
public:
	__fastcall TPortalMeshObjectList(Glvectorfileobjects::TGLBaseMesh* AOwner);
	__fastcall virtual ~TPortalMeshObjectList(void);
	virtual void __fastcall BuildList(Glrendercontextinfo::TRenderContextInfo &mrci);
public:
	/* TPersistentObjectList.Create */ inline __fastcall virtual TPortalMeshObjectList(void) : Glvectorfileobjects::TMeshObjectList() { }
	
public:
	/* TPersistentObject.CreateFromFiler */ inline __fastcall TPortalMeshObjectList(Persistentclasses::TVirtualReader* reader) : Glvectorfileobjects::TMeshObjectList(reader) { }
	
};

#pragma pack(pop)

class DELPHICLASS TSectorMeshObject;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TSectorMeshObject : public Glvectorfileobjects::TMorphableMeshObject
{
	typedef Glvectorfileobjects::TMorphableMeshObject inherited;
	
private:
	bool FRenderDone;
	
public:
	__fastcall TSectorMeshObject(Glvectorfileobjects::TMeshObjectList* AOwner);
	__fastcall virtual ~TSectorMeshObject(void);
	virtual void __fastcall BuildList(Glrendercontextinfo::TRenderContextInfo &mrci);
	DYNAMIC void __fastcall Prepare(void);
	__property bool RenderDone = {read=FRenderDone, write=FRenderDone, nodefault};
public:
	/* TMorphableMeshObject.Create */ inline __fastcall virtual TSectorMeshObject(void) : Glvectorfileobjects::TMorphableMeshObject() { }
	
public:
	/* TPersistentObject.CreateFromFiler */ inline __fastcall TSectorMeshObject(Persistentclasses::TVirtualReader* reader) : Glvectorfileobjects::TMorphableMeshObject(reader) { }
	
};

#pragma pack(pop)

class DELPHICLASS TFGPolygon;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TFGPolygon : public Glvectorfileobjects::TFGVertexNormalTexIndexList
{
	typedef Glvectorfileobjects::TFGVertexNormalTexIndexList inherited;
	
public:
	__fastcall virtual TFGPolygon(Glvectorfileobjects::TFaceGroups* AOwner);
	__fastcall virtual ~TFGPolygon(void);
	DYNAMIC void __fastcall Prepare(void);
public:
	/* TFGVertexNormalTexIndexList.Create */ inline __fastcall virtual TFGPolygon(void) : Glvectorfileobjects::TFGVertexNormalTexIndexList() { }
	
public:
	/* TPersistentObject.CreateFromFiler */ inline __fastcall TFGPolygon(Persistentclasses::TVirtualReader* reader) : Glvectorfileobjects::TFGVertexNormalTexIndexList(reader) { }
	
};

#pragma pack(pop)

class DELPHICLASS TFGPortalPolygon;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TFGPortalPolygon : public TFGPolygon
{
	typedef TFGPolygon inherited;
	
private:
	int FDestinationSectorIndex;
	Vectortypes::TVector3f FCenter;
	Vectortypes::TVector3f FNormal;
	float FRadius;
	
public:
	__fastcall virtual TFGPortalPolygon(Glvectorfileobjects::TFaceGroups* AOwner);
	__fastcall virtual ~TFGPortalPolygon(void);
	virtual void __fastcall BuildList(Glrendercontextinfo::TRenderContextInfo &mrci);
	DYNAMIC void __fastcall Prepare(void);
	__property int DestinationSectorIndex = {read=FDestinationSectorIndex, write=FDestinationSectorIndex, nodefault};
public:
	/* TFGVertexNormalTexIndexList.Create */ inline __fastcall virtual TFGPortalPolygon(void) : TFGPolygon() { }
	
public:
	/* TPersistentObject.CreateFromFiler */ inline __fastcall TFGPortalPolygon(Persistentclasses::TVirtualReader* reader) : TFGPolygon(reader) { }
	
};

#pragma pack(pop)

class DELPHICLASS TGLPortal;
class PASCALIMPLEMENTATION TGLPortal : public Glvectorfileobjects::TGLBaseMesh
{
	typedef Glvectorfileobjects::TGLBaseMesh inherited;
	
public:
	__fastcall virtual TGLPortal(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TGLPortal(void);
	
__published:
	__property MaterialLibrary;
public:
	/* TGLBaseSceneObject.CreateAsChild */ inline __fastcall TGLPortal(Glscene::TGLBaseSceneObject* aParentOwner) : Glvectorfileobjects::TGLBaseMesh(aParentOwner) { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Glportal */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLPORTAL)
using namespace Glportal;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// GlportalHPP
