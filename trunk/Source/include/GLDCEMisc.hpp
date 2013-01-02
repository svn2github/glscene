// CodeGear C++Builder
// Copyright (c) 1995, 2012 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLDCEMisc.pas' rev: 24.00 (Win32)

#ifndef GldcemiscHPP
#define GldcemiscHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <GLVectorFileObjects.hpp>	// Pascal unit
#include <GLEllipseCollision.hpp>	// Pascal unit
#include <VectorGeometry.hpp>	// Pascal unit
#include <VectorLists.hpp>	// Pascal unit
#include <GLScene.hpp>	// Pascal unit
#include <GLTerrainRenderer.hpp>	// Pascal unit
#include <GLProxyObjects.hpp>	// Pascal unit
#include <GLMultiProxy.hpp>	// Pascal unit
#include <VectorTypes.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Gldcemisc
{
//-- type declarations -------------------------------------------------------
//-- var, const, procedure ---------------------------------------------------
extern PACKAGE System::StaticArray<Vectortypes::TVector3f, 36> DCEBox;
extern PACKAGE void __fastcall ECSetCollisionRange(Glellipsecollision::TECMovePack &MovePack);
extern PACKAGE void __fastcall ECResetColliders(Glellipsecollision::TECMovePack &MovePack);
extern PACKAGE void __fastcall ECAddFreeForm(Glellipsecollision::TECMovePack &MovePack, Glscene::TGLBaseSceneObject* FreeForm, bool Solid, int ObjectID);
extern PACKAGE void __fastcall ECAddBox(Glellipsecollision::TECMovePack &MovePack, Glscene::TGLBaseSceneObject* BoxObj, const Vectortypes::TVector3f &BoxSize, bool Solid, int ObjectID);
extern PACKAGE void __fastcall ECAddTerrain(Glellipsecollision::TECMovePack &MovePack, Glterrainrenderer::TGLTerrainRenderer* TerrainRenderer, float Resolution, bool Solid, int ObjectID);
extern PACKAGE void __fastcall ECAddEllipsoid(Glellipsecollision::TECMovePack &MovePack, const Vectortypes::TVector3f &ePos, const Vectortypes::TVector3f &eRadius, bool Solid, int ObjectID);
}	/* namespace Gldcemisc */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLDCEMISC)
using namespace Gldcemisc;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// GldcemiscHPP
