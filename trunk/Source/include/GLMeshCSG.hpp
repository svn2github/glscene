// CodeGear C++Builder
// Copyright (c) 1995, 2012 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLMeshCSG.pas' rev: 24.00 (Win32)

#ifndef GlmeshcsgHPP
#define GlmeshcsgHPP

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
#include <VectorGeometry.hpp>	// Pascal unit
#include <GLBSP.hpp>	// Pascal unit
#include <VectorLists.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Glmeshcsg
{
//-- type declarations -------------------------------------------------------
enum TCSGOperation : unsigned char { CSG_Union, CSG_Subtraction, CSG_Intersection };

//-- var, const, procedure ---------------------------------------------------
extern PACKAGE void __fastcall CSG_Operation(Glvectorfileobjects::TMeshObject* obj1, Glvectorfileobjects::TMeshObject* obj2, TCSGOperation Operation, Glvectorfileobjects::TMeshObject* Res, const System::UnicodeString MaterialName1, const System::UnicodeString MaterialName2);
}	/* namespace Glmeshcsg */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLMESHCSG)
using namespace Glmeshcsg;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// GlmeshcsgHPP
