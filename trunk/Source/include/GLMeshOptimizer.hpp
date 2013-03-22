// CodeGear C++Builder
// Copyright (c) 1995, 2012 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLMeshOptimizer.pas' rev: 24.00 (Win32)

#ifndef GlmeshoptimizerHPP
#define GlmeshoptimizerHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <System.Classes.hpp>	// Pascal unit
#include <System.SysUtils.hpp>	// Pascal unit
#include <VectorGeometry.hpp>	// Pascal unit
#include <GLVectorFileObjects.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Glmeshoptimizer
{
//-- type declarations -------------------------------------------------------
enum TMeshOptimizerOption : unsigned char { mooStandardize, mooVertexCache, mooSortByMaterials, mooMergeObjects };

typedef System::Set<TMeshOptimizerOption, TMeshOptimizerOption::mooStandardize, TMeshOptimizerOption::mooMergeObjects>  TMeshOptimizerOptions;

//-- var, const, procedure ---------------------------------------------------
extern PACKAGE TMeshOptimizerOptions vDefaultMeshOptimizerOptions;
extern PACKAGE void __fastcall OptimizeMesh(Glvectorfileobjects::TMeshObjectList* aList)/* overload */;
extern PACKAGE void __fastcall OptimizeMesh(Glvectorfileobjects::TMeshObjectList* aList, TMeshOptimizerOptions options)/* overload */;
extern PACKAGE void __fastcall OptimizeMesh(Glvectorfileobjects::TMeshObject* aMeshObject)/* overload */;
extern PACKAGE void __fastcall OptimizeMesh(Glvectorfileobjects::TMeshObject* aMeshObject, TMeshOptimizerOptions options)/* overload */;
extern PACKAGE void __fastcall FacesSmooth(Glvectorfileobjects::TMeshObject* aMeshObj, float aWeldDistance = 1.000000E-07, float aThreshold = 3.500000E+01, bool InvertNormals = false);
}	/* namespace Glmeshoptimizer */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLMESHOPTIMIZER)
using namespace Glmeshoptimizer;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// GlmeshoptimizerHPP
