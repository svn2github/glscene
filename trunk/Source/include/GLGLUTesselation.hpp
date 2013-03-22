// CodeGear C++Builder
// Copyright (c) 1995, 2012 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLGLUTesselation.pas' rev: 24.00 (Win32)

#ifndef GlglutesselationHPP
#define GlglutesselationHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <GLVectorFileObjects.hpp>	// Pascal unit
#include <VectorLists.hpp>	// Pascal unit
#include <VectorGeometry.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Glglutesselation
{
//-- type declarations -------------------------------------------------------
//-- var, const, procedure ---------------------------------------------------
extern PACKAGE void __fastcall DoTesselate(Vectorlists::TAffineVectorList* Vertexes, Glvectorfileobjects::TGLBaseMesh* Mesh, Vectorgeometry::PAffineVector normal = (Vectorgeometry::PAffineVector)(0x0), bool invertNormals = false);
}	/* namespace Glglutesselation */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLGLUTESSELATION)
using namespace Glglutesselation;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// GlglutesselationHPP
