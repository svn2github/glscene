// CodeGear C++Builder
// Copyright (c) 1995, 2012 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLMeshBuilder.pas' rev: 24.00 (Win32)

#ifndef GlmeshbuilderHPP
#define GlmeshbuilderHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <System.SysUtils.hpp>	// Pascal unit
#include <System.Classes.hpp>	// Pascal unit
#include <GLScene.hpp>	// Pascal unit
#include <GLVectorFileObjects.hpp>	// Pascal unit
#include <VectorTypes.hpp>	// Pascal unit
#include <VectorGeometry.hpp>	// Pascal unit
#include <VectorLists.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Glmeshbuilder
{
//-- type declarations -------------------------------------------------------
//-- var, const, procedure ---------------------------------------------------
extern PACKAGE void __fastcall BuildCube(Glvectorfileobjects::TMeshObject* Mesh, const Vectortypes::TVector3f &Position, const Vectortypes::TVector3f &Scale);
extern PACKAGE void __fastcall BuildCylinder(Glvectorfileobjects::TMeshObject* Mesh, const Vectortypes::TVector3f &Position, const Vectortypes::TVector3f &Scale, int Slices);
extern PACKAGE void __fastcall BuildCylinder2(Glvectorfileobjects::TMeshObject* Mesh, const Vectortypes::TVector3f &Position, const Vectortypes::TVector3f &Scale, float TopRadius, float BottomRadius, float Height, int Slices);
}	/* namespace Glmeshbuilder */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLMESHBUILDER)
using namespace Glmeshbuilder;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// GlmeshbuilderHPP
