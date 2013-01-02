// CodeGear C++Builder
// Copyright (c) 1995, 2012 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'Polynomials.pas' rev: 24.00 (Win32)

#ifndef PolynomialsHPP
#define PolynomialsHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <VectorGeometry.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Polynomials
{
//-- type declarations -------------------------------------------------------
typedef System::DynamicArray<double> TDoubleArray;

//-- var, const, procedure ---------------------------------------------------
extern PACKAGE double __fastcall EvalPolynom(const TDoubleArray poly, const double x);
extern PACKAGE TDoubleArray __fastcall DerivatedPolynom(const TDoubleArray poly);
extern PACKAGE double __fastcall FindRoot(const TDoubleArray poly, double min, double max, double epsilon);
extern PACKAGE bool __fastcall MinPositiveCoef(const TDoubleArray coefs, double &aMin);
extern PACKAGE double __fastcall cbrt(const double x);
extern PACKAGE TDoubleArray __fastcall SolveQuadric(const Vectorgeometry::PDoubleVector c);
extern PACKAGE TDoubleArray __fastcall SolveCubic(const Vectorgeometry::PDoubleVector c);
extern PACKAGE TDoubleArray __fastcall SolveQuartic(const Vectorgeometry::PDoubleVector c);
}	/* namespace Polynomials */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_POLYNOMIALS)
using namespace Polynomials;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// PolynomialsHPP
