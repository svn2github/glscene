// CodeGear C++Builder
// Copyright (c) 1995, 2012 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'CurvesAndSurfaces.pas' rev: 24.00 (Win32)

#ifndef CurvesandsurfacesHPP
#define CurvesandsurfacesHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <VectorGeometry.hpp>	// Pascal unit
#include <VectorLists.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Curvesandsurfaces
{
//-- type declarations -------------------------------------------------------
enum TBSplineContinuity : unsigned char { bscUniformNonPeriodic, bscUniformPeriodic };

//-- var, const, procedure ---------------------------------------------------
extern PACKAGE Vectortypes::TVector3f __fastcall BezierCurvePoint(float t, int n, Vectorgeometry::PAffineVectorArray cp);
extern PACKAGE Vectortypes::TVector3f __fastcall BezierSurfacePoint(float s, float t, int m, int n, Vectorgeometry::PAffineVectorArray cp);
extern PACKAGE void __fastcall GenerateBezierCurve(int Steps, Vectorlists::TAffineVectorList* ControlPoints, Vectorlists::TAffineVectorList* Vertices);
extern PACKAGE void __fastcall GenerateBezierSurface(int Steps, int Width, int Height, Vectorlists::TAffineVectorList* ControlPoints, Vectorlists::TAffineVectorList* Vertices);
extern PACKAGE Vectortypes::TVector3f __fastcall BSplinePoint(float t, int n, int k, Vectorgeometry::PFloatVector knots, Vectorgeometry::PAffineVectorArray cp);
extern PACKAGE Vectortypes::TVector3f __fastcall BSplineSurfacePoint(float s, float t, int m, int n, int k1, int k2, Vectorgeometry::PFloatVector uknots, Vectorgeometry::PFloatVector vknots, Vectorgeometry::PAffineVectorArray cp);
extern PACKAGE void __fastcall GenerateBSpline(int Steps, int Order, Vectorlists::TSingleList* KnotVector, Vectorlists::TAffineVectorList* ControlPoints, Vectorlists::TAffineVectorList* Vertices);
extern PACKAGE void __fastcall GenerateBSplineSurface(int Steps, int UOrder, int VOrder, int Width, int Height, Vectorlists::TSingleList* UKnotVector, Vectorlists::TSingleList* VKnotVector, Vectorlists::TAffineVectorList* ControlPoints, Vectorlists::TAffineVectorList* Vertices);
extern PACKAGE void __fastcall GenerateKnotVector(Vectorlists::TSingleList* KnotVector, int NumberOfPoints, int Order, TBSplineContinuity Continuity);
}	/* namespace Curvesandsurfaces */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_CURVESANDSURFACES)
using namespace Curvesandsurfaces;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// CurvesandsurfacesHPP
