// CodeGear C++Builder
// Copyright (c) 1995, 2012 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLSpatialPartitioning.pas' rev: 24.00 (Win32)

#ifndef GlspatialpartitioningHPP
#define GlspatialpartitioningHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <GLViewer.hpp>	// Pascal unit
#include <SpatialPartitioning.hpp>	// Pascal unit
#include <GLScene.hpp>	// Pascal unit
#include <VectorGeometry.hpp>	// Pascal unit
#include <OpenGLTokens.hpp>	// Pascal unit
#include <GeometryBB.hpp>	// Pascal unit
#include <GLRenderContextInfo.hpp>	// Pascal unit
#include <GLState.hpp>	// Pascal unit
#include <PersistentClasses.hpp>	// Pascal unit
#include <System.Classes.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Glspatialpartitioning
{
//-- type declarations -------------------------------------------------------
class DELPHICLASS TSceneObj;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TSceneObj : public Spatialpartitioning::TSpacePartitionLeaf
{
	typedef Spatialpartitioning::TSpacePartitionLeaf inherited;
	
public:
	Glscene::TGLBaseSceneObject* Obj;
	virtual void __fastcall UpdateCachedAABBAndBSphere(void);
	__fastcall TSceneObj(Spatialpartitioning::TSectoredSpacePartition* Owner, Glscene::TGLBaseSceneObject* aObj);
	__fastcall virtual ~TSceneObj(void);
public:
	/* TSpacePartitionLeaf.CreateOwned */ inline __fastcall TSceneObj(Spatialpartitioning::TBaseSpacePartition* SpacePartition) : Spatialpartitioning::TSpacePartitionLeaf(SpacePartition) { }
	
public:
	/* TPersistentObject.Create */ inline __fastcall virtual TSceneObj(void) : Spatialpartitioning::TSpacePartitionLeaf() { }
	/* TPersistentObject.CreateFromFiler */ inline __fastcall TSceneObj(Persistentclasses::TVirtualReader* reader) : Spatialpartitioning::TSpacePartitionLeaf(reader) { }
	
};

#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------
extern PACKAGE void __fastcall RenderAABB(Glrendercontextinfo::TRenderContextInfo &rci, const Geometrybb::TAABB &AABB)/* overload */;
extern PACKAGE void __fastcall RenderAABB(Glrendercontextinfo::TRenderContextInfo &rci, const Geometrybb::TAABB &AABB, float w, float r, float g, float b)/* overload */;
extern PACKAGE void __fastcall RenderSpatialPartitioning(Glrendercontextinfo::TRenderContextInfo &rci, Spatialpartitioning::TSectoredSpacePartition* const Space);
extern PACKAGE Spatialpartitioning::TExtendedFrustum __fastcall ExtendedFrustumMakeFromSceneViewer(const Vectorgeometry::TFrustum &AFrustum, Glwin32viewer::TGLSceneViewer* const AGLSceneViewer)/* overload */;
extern PACKAGE Spatialpartitioning::TExtendedFrustum __fastcall ExtendedFrustumMakeFromSceneViewer(const Vectorgeometry::TFrustum &AFrustum, const int vWidth, const int vHeight, Glscene::TGLCamera* AGLCamera)/* overload */;
}	/* namespace Glspatialpartitioning */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLSPATIALPARTITIONING)
using namespace Glspatialpartitioning;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// GlspatialpartitioningHPP
