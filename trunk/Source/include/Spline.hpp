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
#include <System.SysUtils.hpp>	// Pascal unit
#include <System.Classes.hpp>	// Pascal unit
#include <VectorTypes.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Spline
{
//-- type declarations -------------------------------------------------------
enum TDistCalculationMode : unsigned char { dcmDefault, dcmLinear, dcmSqLinear };

enum TSplineInterpolateMode : unsigned char { simDefault, simNormalized };

enum TSplineTermCode : unsigned char { stcSecondDerivations, stcFirstDeriveations, stcFreeSpline, stcPeriodicSpline, stcFixedSpline };

enum TTermConditionIndex : unsigned char { tciStart, TciEnd };

enum TTermConditionCoord : unsigned char { tccX, tccY, tccZ, tccW };

class DELPHICLASS TGFSpline;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TGFSpline : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	typedef System::DynamicArray<double> _TGFSpline__1;
	
	
public:
	double operator[](double X) { return Interpolate[X]; }
	
private:
	_TGFSpline__1 fX;
	_TGFSpline__1 fY;
	_TGFSpline__1 fA;
	_TGFSpline__1 fB;
	_TGFSpline__1 fC;
	_TGFSpline__1 fD;
	_TGFSpline__1 fU;
	int FSize;
	int FCode;
	int Locks;
	bool change;
	void __fastcall TDMP(void);
	void __fastcall TRIDIG(void);
	double __fastcall GetInterpolate(double X);
	void __fastcall SetCode(const int Value);
	void __fastcall SetSize(const int Value);
	double __fastcall getA(int Index);
	double __fastcall getB(int Index);
	double __fastcall getC(int Index);
	double __fastcall getD(int Index);
	double __fastcall getU(int Index);
	double __fastcall getX(int Index);
	double __fastcall getY(int Index);
	void __fastcall setA(int Index, const double Value);
	void __fastcall setB(int Index, const double Value);
	void __fastcall setC(int Index, const double Value);
	void __fastcall setD(int Index, const double Value);
	void __fastcall setU(int Index, const double Value);
	void __fastcall setX(int Index, const double Value);
	void __fastcall setY(int Index, const double Value);
	double __fastcall GetSlope(double X);
	double __fastcall GetConvex(double X);
	
public:
	__fastcall TGFSpline(void);
	__fastcall virtual ~TGFSpline(void);
	void __fastcall BeginUpdate(void);
	void __fastcall EndUpdate(void);
	void __fastcall Recalc(void);
	void __fastcall Changed(void);
	__property int Size = {read=FSize, write=SetSize, nodefault};
	__property double X[int Index] = {read=getX, write=setX};
	__property double Y[int Index] = {read=getY, write=setY};
	__property double A[int Index] = {read=getA, write=setA};
	__property double B[int Index] = {read=getB, write=setB};
	__property double C[int Index] = {read=getC, write=setC};
	__property double D[int Index] = {read=getD, write=setD};
	__property double U[int Index] = {read=getU, write=setU};
	__property int Code = {read=FCode, write=SetCode, nodefault};
	__property double Interpolate[double X] = {read=GetInterpolate/*, default*/};
	__property double Slope[double X] = {read=GetSlope};
	__property double Convex[double X] = {read=GetConvex};
};

#pragma pack(pop)

class DELPHICLASS TAbstractSpline;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TAbstractSpline : public System::TObject
{
	typedef System::TObject inherited;
	
public:
	virtual float __fastcall SplineX(const float t) = 0 ;
	virtual float __fastcall SplineY(const float t) = 0 ;
	virtual float __fastcall SplineZ(const float t) = 0 ;
	virtual float __fastcall SplineW(const float t) = 0 ;
	virtual void __fastcall SplineXY(const float t, float &X, float &Y) = 0 ;
	virtual void __fastcall SplineXYZ(const float t, float &X, float &Y, float &Z) = 0 ;
	virtual void __fastcall SplineXYZW(const float t, float &X, float &Y, float &Z, float &W) = 0 ;
	virtual Vectortypes::TVector3f __fastcall SplineAffineVector(const float t) = 0 /* overload */;
	virtual void __fastcall SplineAffineVector(const float t, Vectortypes::TVector3f &vector) = 0 /* overload */;
	virtual Vectortypes::TVector4f __fastcall SplineVector(const float t) = 0 /* overload */;
	virtual void __fastcall SplineVector(const float t, Vectortypes::TVector4f &vector) = 0 /* overload */;
	virtual float __fastcall SplineSlopeX(const float t) = 0 ;
	virtual float __fastcall SplineSlopeY(const float t) = 0 ;
	virtual float __fastcall SplineSlopeZ(const float t) = 0 ;
	virtual float __fastcall SplineSlopeW(const float t) = 0 ;
	virtual Vectortypes::TVector3f __fastcall SplineSlopeVector(const float t) = 0 /* overload */;
	virtual float __fastcall SplineConvexX(const float t) = 0 ;
	virtual float __fastcall SplineConvexY(const float t) = 0 ;
	virtual float __fastcall SplineConvexZ(const float t) = 0 ;
	virtual float __fastcall SplineConvexW(const float t) = 0 ;
	virtual Vectortypes::TVector3f __fastcall SplineConvexVector(const float t) = 0 /* overload */;
	virtual bool __fastcall SplineIntersecYZ(float X, float &Y, float &Z) = 0 ;
	virtual bool __fastcall SplineIntersecXZ(float Y, float &X, float &Z) = 0 ;
	virtual bool __fastcall SplineIntersecXY(float Z, float &X, float &Y) = 0 ;
public:
	/* TObject.Create */ inline __fastcall TAbstractSpline(void) : System::TObject() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TAbstractSpline(void) { }
	
};

#pragma pack(pop)

class DELPHICLASS TCubicSpline;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TCubicSpline : public TAbstractSpline
{
	typedef TAbstractSpline inherited;
	
private:
	typedef System::DynamicArray<double> _TCubicSpline__1;
	
	
private:
	Vectorgeometry::TFloatVector *ffX;
	Vectorgeometry::TFloatVector *ffY;
	Vectorgeometry::TFloatVector *ffZ;
	Vectorgeometry::TFloatVector *ffW;
	_TCubicSpline__1 Tau;
	int fNb;
	TGFSpline* fSplineX;
	TGFSpline* fSplineY;
	TGFSpline* fSplineZ;
	TGFSpline* fSplineW;
	TDistCalculationMode FDistCalculationMode;
	TSplineTermCode FSplineTermCode;
	TSplineInterpolateMode FInterpolateMode;
	void __fastcall SetDistCalculationMode(const TDistCalculationMode Value);
	void __fastcall SetSplineTermCode(const TSplineTermCode Value);
	void __fastcall SetInterpolateMode(const TSplineInterpolateMode Value);
	Vectortypes::TVector4f __fastcall GetTermCondition(TTermConditionIndex Index);
	void __fastcall SetTermCondition(TTermConditionIndex Index, const Vectortypes::TVector4f &Value);
	Vectortypes::TVector4f __fastcall GetU(int Index);
	void __fastcall SetU(int Index, const Vectortypes::TVector4f &Value);
	float __fastcall GetX(int Index);
	float __fastcall GetY(int Index);
	float __fastcall GetZ(int Index);
	void __fastcall SetX(int Index, const float Value);
	void __fastcall SetY(int Index, const float Value);
	void __fastcall SetZ(int Index, const float Value);
	float __fastcall GetW(int Index);
	void __fastcall SetW(int Index, const float Value);
	
public:
	__property TSplineInterpolateMode InterpolateMode = {read=FInterpolateMode, write=SetInterpolateMode, nodefault};
	__property TDistCalculationMode DistCalculationMode = {read=FDistCalculationMode, write=SetDistCalculationMode, nodefault};
	__property TSplineTermCode SplineTermCode = {read=FSplineTermCode, write=SetSplineTermCode, nodefault};
	__property Vectortypes::TVector4f TermCondition[TTermConditionIndex Index] = {read=GetTermCondition, write=SetTermCondition};
	__property Vectortypes::TVector4f U[int Index] = {read=GetU, write=SetU};
	
protected:
	void __fastcall Reinit(void);
	void __fastcall AssignCoordinates(void);
	void __fastcall RecalcTau(void);
	float __fastcall GetTau(float T);
	
public:
	void __fastcall BeginUpdate(void);
	void __fastcall EndUpdate(void);
	__fastcall TCubicSpline(const Vectorgeometry::PFloatVector X, const Vectorgeometry::PFloatVector Y, const Vectorgeometry::PFloatVector Z, const Vectorgeometry::PFloatVector W, const int nb, TSplineTermCode SplineTermCode, TSplineInterpolateMode InterpolateMode, TDistCalculationMode DistCalc);
	void __fastcall SetXYZW(const Vectorgeometry::PFloatVector X, const Vectorgeometry::PFloatVector Y, const Vectorgeometry::PFloatVector Z, const Vectorgeometry::PFloatVector W, const int nb);
	__fastcall virtual ~TCubicSpline(void);
	virtual float __fastcall SplineX(const float t);
	virtual float __fastcall SplineY(const float t);
	virtual float __fastcall SplineZ(const float t);
	virtual float __fastcall SplineW(const float t);
	virtual void __fastcall SplineXY(const float t, float &X, float &Y);
	virtual void __fastcall SplineXYZ(const float t, float &X, float &Y, float &Z);
	virtual void __fastcall SplineXYZW(const float t, float &X, float &Y, float &Z, float &W);
	virtual Vectortypes::TVector3f __fastcall SplineAffineVector(const float t)/* overload */;
	virtual void __fastcall SplineAffineVector(const float t, Vectortypes::TVector3f &vector)/* overload */;
	virtual Vectortypes::TVector4f __fastcall SplineVector(const float t)/* overload */;
	virtual void __fastcall SplineVector(const float t, Vectortypes::TVector4f &vector)/* overload */;
	virtual float __fastcall SplineSlopeX(const float t);
	virtual float __fastcall SplineSlopeY(const float t);
	virtual float __fastcall SplineSlopeZ(const float t);
	virtual float __fastcall SplineSlopeW(const float t);
	virtual Vectortypes::TVector3f __fastcall SplineSlopeVector(const float t)/* overload */;
	virtual float __fastcall SplineConvexX(const float t)/* overload */;
	virtual float __fastcall SplineConvexY(const float t)/* overload */;
	virtual float __fastcall SplineConvexZ(const float t)/* overload */;
	virtual float __fastcall SplineConvexW(const float t)/* overload */;
	virtual Vectortypes::TVector3f __fastcall SplineConvexVector(const float t)/* overload */;
	virtual bool __fastcall SplineIntersecYZ(float X, float &Y, float &Z);
	virtual bool __fastcall SplineIntersecXZ(float Y, float &X, float &Z);
	virtual bool __fastcall SplineIntersecXY(float Z, float &X, float &Y);
	System::Extended __fastcall Curvature(const float t);
	__property float pX[int Index] = {read=GetX, write=SetX};
	__property float pY[int Index] = {read=GetY, write=SetY};
	__property float pZ[int Index] = {read=GetZ, write=SetZ};
	__property float pW[int Index] = {read=GetW, write=SetW};
};

#pragma pack(pop)

class DELPHICLASS TCubicSplineOld;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TCubicSplineOld : public TAbstractSpline
{
	typedef TAbstractSpline inherited;
	
private:
	void *matX;
	void *matY;
	void *matZ;
	void *matW;
	int FNb;
	
public:
	__fastcall TCubicSplineOld(const Vectorgeometry::PFloatVector X, const Vectorgeometry::PFloatVector Y, const Vectorgeometry::PFloatVector Z, const Vectorgeometry::PFloatVector W, const int nb);
	__fastcall virtual ~TCubicSplineOld(void);
	virtual float __fastcall SplineX(const float t);
	virtual float __fastcall SplineY(const float t);
	virtual float __fastcall SplineZ(const float t);
	virtual float __fastcall SplineW(const float t);
	virtual void __fastcall SplineXY(const float t, float &X, float &Y);
	virtual void __fastcall SplineXYZ(const float t, float &X, float &Y, float &Z);
	virtual void __fastcall SplineXYZW(const float t, float &X, float &Y, float &Z, float &W);
	virtual Vectortypes::TVector3f __fastcall SplineAffineVector(const float t)/* overload */;
	virtual void __fastcall SplineAffineVector(const float t, Vectortypes::TVector3f &vector)/* overload */;
	virtual Vectortypes::TVector4f __fastcall SplineVector(const float t)/* overload */;
	virtual void __fastcall SplineVector(const float t, Vectortypes::TVector4f &vector)/* overload */;
	virtual float __fastcall SplineSlopeX(const float t);
	virtual float __fastcall SplineSlopeY(const float t);
	virtual float __fastcall SplineSlopeZ(const float t);
	virtual float __fastcall SplineSlopeW(const float t);
	virtual Vectortypes::TVector3f __fastcall SplineSlopeVector(const float t)/* overload */;
	virtual bool __fastcall SplineIntersecYZ(float X, float &Y, float &Z);
	virtual bool __fastcall SplineIntersecXZ(float Y, float &X, float &Z);
	virtual bool __fastcall SplineIntersecXY(float Z, float &X, float &Y);
};

#pragma pack(pop)

class DELPHICLASS ESplineError;
#pragma pack(push,4)
class PASCALIMPLEMENTATION ESplineError : public System::Sysutils::Exception
{
	typedef System::Sysutils::Exception inherited;
	
public:
	/* Exception.Create */ inline __fastcall ESplineError(const System::UnicodeString Msg) : System::Sysutils::Exception(Msg) { }
	/* Exception.CreateFmt */ inline __fastcall ESplineError(const System::UnicodeString Msg, System::TVarRec const *Args, const int Args_Size) : System::Sysutils::Exception(Msg, Args, Args_Size) { }
	/* Exception.CreateRes */ inline __fastcall ESplineError(NativeUInt Ident)/* overload */ : System::Sysutils::Exception(Ident) { }
	/* Exception.CreateRes */ inline __fastcall ESplineError(System::PResStringRec ResStringRec)/* overload */ : System::Sysutils::Exception(ResStringRec) { }
	/* Exception.CreateResFmt */ inline __fastcall ESplineError(NativeUInt Ident, System::TVarRec const *Args, const int Args_Size)/* overload */ : System::Sysutils::Exception(Ident, Args, Args_Size) { }
	/* Exception.CreateResFmt */ inline __fastcall ESplineError(System::PResStringRec ResStringRec, System::TVarRec const *Args, const int Args_Size)/* overload */ : System::Sysutils::Exception(ResStringRec, Args, Args_Size) { }
	/* Exception.CreateHelp */ inline __fastcall ESplineError(const System::UnicodeString Msg, int AHelpContext) : System::Sysutils::Exception(Msg, AHelpContext) { }
	/* Exception.CreateFmtHelp */ inline __fastcall ESplineError(const System::UnicodeString Msg, System::TVarRec const *Args, const int Args_Size, int AHelpContext) : System::Sysutils::Exception(Msg, Args, Args_Size, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall ESplineError(NativeUInt Ident, int AHelpContext)/* overload */ : System::Sysutils::Exception(Ident, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall ESplineError(System::PResStringRec ResStringRec, int AHelpContext)/* overload */ : System::Sysutils::Exception(ResStringRec, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall ESplineError(System::PResStringRec ResStringRec, System::TVarRec const *Args, const int Args_Size, int AHelpContext)/* overload */ : System::Sysutils::Exception(ResStringRec, Args, Args_Size, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall ESplineError(NativeUInt Ident, System::TVarRec const *Args, const int Args_Size, int AHelpContext)/* overload */ : System::Sysutils::Exception(Ident, Args, Args_Size, AHelpContext) { }
	/* Exception.Destroy */ inline __fastcall virtual ~ESplineError(void) { }
	
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
