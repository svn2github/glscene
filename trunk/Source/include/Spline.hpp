// CodeGear C++Builder
// Copyright (c) 1995, 2012 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'Spline.pas' rev: 24.00 (Win32)

#ifndef SplineHPP
#define SplineHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <VectorGeometry.hpp>	// Pascal unit
#include <VectorTypes.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Spline
{
//-- type declarations -------------------------------------------------------
typedef System::DynamicArray<System::StaticArray<float, 4> > TCubicSplineMatrix;

class DELPHICLASS TCubicSpline;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TCubicSpline : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	TCubicSplineMatrix matX;
	TCubicSplineMatrix matY;
	TCubicSplineMatrix matZ;
	TCubicSplineMatrix matW;
	int FNb;
	
public:
	__fastcall TCubicSpline(const Vectorgeometry::PFloatVector X, const Vectorgeometry::PFloatVector Y, const Vectorgeometry::PFloatVector Z, const Vectorgeometry::PFloatVector W, const int nb);
	__fastcall virtual ~TCubicSpline(void);
	float __fastcall SplineX(const float t);
	float __fastcall SplineY(const float t);
	float __fastcall SplineZ(const float t);
	float __fastcall SplineW(const float t);
	void __fastcall SplineXY(const float t, float &X, float &Y);
	void __fastcall SplineXYZ(const float t, float &X, float &Y, float &Z);
	void __fastcall SplineXYZW(const float t, float &X, float &Y, float &Z, float &W);
	Vectortypes::TVector3f __fastcall SplineAffineVector(const float t)/* overload */;
	void __fastcall SplineAffineVector(const float t, Vectortypes::TVector3f &vector)/* overload */;
	Vectortypes::TVector4f __fastcall SplineVector(const float t)/* overload */;
	void __fastcall SplineVector(const float t, Vectortypes::TVector4f &vector)/* overload */;
	float __fastcall SplineSlopeX(const float t);
	float __fastcall SplineSlopeY(const float t);
	float __fastcall SplineSlopeZ(const float t);
	float __fastcall SplineSlopeW(const float t);
	Vectortypes::TVector3f __fastcall SplineSlopeVector(const float t)/* overload */;
	bool __fastcall SplineIntersecYZ(float X, float &Y, float &Z);
	bool __fastcall SplineIntersecXZ(float Y, float &X, float &Z);
	bool __fastcall SplineIntersecXY(float Z, float &X, float &Y);
};

#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------
}	/* namespace Spline */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_SPLINE)
using namespace Spline;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// SplineHPP
