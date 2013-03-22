// CodeGear C++Builder
// Copyright (c) 1995, 2012 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'VectorGeometry.pas' rev: 24.00 (Win32)

#ifndef VectorgeometryHPP
#define VectorgeometryHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <GLCrossPlatform.hpp>	// Pascal unit
#include <VectorTypes.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Vectorgeometry
{
//-- type declarations -------------------------------------------------------
typedef System::PSingle PFloat;

struct TTexPoint;
typedef TTexPoint *PTexPoint;

#pragma pack(push,1)
struct DECLSPEC_DRECORD TTexPoint
{
public:
	float S;
	float T;
};
#pragma pack(pop)


typedef System::StaticArray<System::Byte, 134217728> TByteVector;

typedef TByteVector *PByteVector;

typedef PByteVector PByteArray;

typedef System::StaticArray<System::Word, 134217728> TWordVector;

typedef TWordVector *PWordVector;

typedef System::StaticArray<int, 134217728> TIntegerVector;

typedef TIntegerVector *PIntegerVector;

typedef PIntegerVector PIntegerArray;

typedef System::StaticArray<float, 134217728> TFloatVector;

typedef TFloatVector *PFloatVector;

typedef PFloatVector PFloatArray;

typedef PFloatVector PSingleArray;

typedef System::DynamicArray<float> TSingleArray;

typedef System::StaticArray<double, 134217728> TDoubleVector;

typedef TDoubleVector *PDoubleVector;

typedef PDoubleVector PDoubleArray;

typedef System::StaticArray<System::Extended, 134217728> TExtendedVector;

typedef TExtendedVector *PExtendedVector;

typedef PExtendedVector PExtendedArray;

typedef System::StaticArray<void *, 134217728> TPointerVector;

typedef TPointerVector *PPointerVector;

typedef PPointerVector PPointerArray;

typedef System::StaticArray<unsigned, 134217728> TCardinalVector;

typedef TCardinalVector *PCardinalVector;

typedef PCardinalVector PCardinalArray;

typedef System::StaticArray<unsigned, 134217728> TLongWordVector;

typedef TLongWordVector *PLongWordVector;

typedef PLongWordVector PLongWordArray;

typedef Vectortypes::TVector4b *PHomogeneousByteVector;

typedef Vectortypes::TVector4b THomogeneousByteVector;

typedef Vectortypes::TVector4w *PHomogeneousWordVector;

typedef Vectortypes::TVector4w THomogeneousWordVector;

typedef Vectortypes::TVector4i *PHomogeneousIntVector;

typedef Vectortypes::TVector4i THomogeneousIntVector;

typedef Vectortypes::TVector4f *PHomogeneousFltVector;

typedef Vectortypes::TVector4f THomogeneousFltVector;

typedef Vectortypes::TVector4d *PHomogeneousDblVector;

typedef Vectortypes::TVector4d THomogeneousDblVector;

typedef Vectortypes::TVector4e *PHomogeneousExtVector;

typedef Vectortypes::TVector4e THomogeneousExtVector;

typedef Vectortypes::TVector4p *PHomogeneousPtrVector;

typedef Vectortypes::TVector4p THomogeneousPtrVector;

typedef Vectortypes::TVector3b *PAffineByteVector;

typedef Vectortypes::TVector3b TAffineByteVector;

typedef Vectortypes::TVector3w *PAffineWordVector;

typedef Vectortypes::TVector3w TAffineWordVector;

typedef Vectortypes::TVector3i *PAffineIntVector;

typedef Vectortypes::TVector3i TAffineIntVector;

typedef Vectortypes::TVector3f *PAffineFltVector;

typedef Vectortypes::TVector3f TAffineFltVector;

typedef Vectortypes::TVector3d *PAffineDblVector;

typedef Vectortypes::TVector3d TAffineDblVector;

typedef Vectortypes::TVector3e *PAffineExtVector;

typedef Vectortypes::TVector3e TAffineExtVector;

typedef Vectortypes::TVector3p *PAffinePtrVector;

typedef Vectortypes::TVector3p TAffinePtrVector;

typedef Vectortypes::TVector2f *PVector2f;

typedef Vectortypes::TVector4f *PVector;

typedef Vectortypes::TVector4f TVector;

typedef Vectortypes::TVector4f *PHomogeneousVector;

typedef Vectortypes::TVector4f THomogeneousVector;

typedef Vectortypes::TVector3f *PAffineVector;

typedef Vectortypes::TVector3f TAffineVector;

typedef Vectortypes::TVector3f *PVertex;

typedef Vectortypes::TVector3f TVertex;

typedef System::StaticArray<Vectortypes::TVector3f, 134217728> TAffineVectorArray;

typedef TAffineVectorArray *PAffineVectorArray;

typedef System::StaticArray<Vectortypes::TVector4f, 67108864> TVectorArray;

typedef TVectorArray *PVectorArray;

typedef System::StaticArray<TTexPoint, 134217728> TTexPointArray;

typedef TTexPointArray *PTexPointArray;

typedef Vectortypes::TMatrix4b THomogeneousByteMatrix;

typedef System::StaticArray<Vectortypes::TVector4w, 4> THomogeneousWordMatrix;

typedef Vectortypes::TMatrix4i THomogeneousIntMatrix;

typedef Vectortypes::TMatrix4f THomogeneousFltMatrix;

typedef Vectortypes::TMatrix4d THomogeneousDblMatrix;

typedef System::StaticArray<Vectortypes::TVector4e, 4> THomogeneousExtMatrix;

typedef Vectortypes::TMatrix3b TAffineByteMatrix;

typedef System::StaticArray<Vectortypes::TVector3w, 3> TAffineWordMatrix;

typedef Vectortypes::TMatrix3i TAffineIntMatrix;

typedef Vectortypes::TMatrix3f TAffineFltMatrix;

typedef Vectortypes::TMatrix3d TAffineDblMatrix;

typedef System::StaticArray<Vectortypes::TVector3e, 3> TAffineExtMatrix;

typedef Vectortypes::TMatrix4f *PMatrix;

typedef Vectortypes::TMatrix4f TMatrix;

typedef System::StaticArray<Vectortypes::TMatrix4f, 16777216> TMatrixArray;

typedef TMatrixArray *PMatrixArray;

typedef Vectortypes::TMatrix4f *PHomogeneousMatrix;

typedef Vectortypes::TMatrix4f THomogeneousMatrix;

typedef Vectortypes::TMatrix3f *PAffineMatrix;

typedef Vectortypes::TMatrix3f TAffineMatrix;

typedef Vectortypes::TVector4f THmgPlane;

typedef Vectortypes::TVector4d TDoubleHmgPlane;

struct TQuaternion;
typedef TQuaternion *PQuaternion;

struct DECLSPEC_DRECORD TQuaternion
{
public:
	Vectortypes::TVector3f ImagPart;
	float RealPart;
};


typedef System::StaticArray<TQuaternion, 67108864> TQuaternionArray;

typedef TQuaternionArray *PQuaternionArray;

struct DECLSPEC_DRECORD TRectangle
{
public:
	int Left;
	int Top;
	int Width;
	int Height;
};


struct DECLSPEC_DRECORD TFrustum
{
public:
	Vectortypes::TVector4f pLeft;
	Vectortypes::TVector4f pTop;
	Vectortypes::TVector4f pRight;
	Vectortypes::TVector4f pBottom;
	Vectortypes::TVector4f pNear;
	Vectortypes::TVector4f pFar;
};


enum TTransType : unsigned char { ttScaleX, ttScaleY, ttScaleZ, ttShearXY, ttShearXZ, ttShearYZ, ttRotateX, ttRotateY, ttRotateZ, ttTranslateX, ttTranslateY, ttTranslateZ, ttPerspectiveX, ttPerspectiveY, ttPerspectiveZ, ttPerspectiveW };

typedef System::StaticArray<float, 16> TTransformations;

typedef System::StaticArray<short, 3> TPackedRotationMatrix;

enum TGLInterpolationType : unsigned char { itLinear, itPower, itSin, itSinAlt, itTan, itLn, itExp };

enum TEulerOrder : unsigned char { eulXYZ, eulXZY, eulYXZ, eulYZX, eulZXY, eulZYX };

//-- var, const, procedure ---------------------------------------------------
static const int cMaxArray = int(0x7ffffff);
static const System::Extended cColinearBias = 1.000000E-08;
extern PACKAGE TTexPoint XTexPoint;
extern PACKAGE TTexPoint YTexPoint;
extern PACKAGE TTexPoint XYTexPoint;
extern PACKAGE TTexPoint NullTexPoint;
extern PACKAGE TTexPoint MidTexPoint;
extern PACKAGE Vectortypes::TVector3f XVector;
extern PACKAGE Vectortypes::TVector3f YVector;
extern PACKAGE Vectortypes::TVector3f ZVector;
extern PACKAGE Vectortypes::TVector3f XYVector;
extern PACKAGE Vectortypes::TVector3f XZVector;
extern PACKAGE Vectortypes::TVector3f YZVector;
extern PACKAGE Vectortypes::TVector3f XYZVector;
extern PACKAGE Vectortypes::TVector3f NullVector;
extern PACKAGE Vectortypes::TVector3f MinusXVector;
extern PACKAGE Vectortypes::TVector3f MinusYVector;
extern PACKAGE Vectortypes::TVector3f MinusZVector;
extern PACKAGE Vectortypes::TVector4f XHmgVector;
extern PACKAGE Vectortypes::TVector4f YHmgVector;
extern PACKAGE Vectortypes::TVector4f ZHmgVector;
extern PACKAGE Vectortypes::TVector4f WHmgVector;
extern PACKAGE Vectortypes::TVector4f XYHmgVector;
extern PACKAGE Vectortypes::TVector4f YZHmgVector;
extern PACKAGE Vectortypes::TVector4f XZHmgVector;
extern PACKAGE Vectortypes::TVector4f XYZHmgVector;
extern PACKAGE Vectortypes::TVector4f XYZWHmgVector;
extern PACKAGE Vectortypes::TVector4f NullHmgVector;
extern PACKAGE Vectortypes::TVector4f XHmgPoint;
extern PACKAGE Vectortypes::TVector4f YHmgPoint;
extern PACKAGE Vectortypes::TVector4f ZHmgPoint;
extern PACKAGE Vectortypes::TVector4f WHmgPoint;
extern PACKAGE Vectortypes::TVector4f NullHmgPoint;
extern PACKAGE Vectortypes::TMatrix3f IdentityMatrix;
extern PACKAGE Vectortypes::TMatrix4f IdentityHmgMatrix;
extern PACKAGE Vectortypes::TMatrix4d IdentityHmgDblMatrix;
extern PACKAGE Vectortypes::TMatrix3f EmptyMatrix;
extern PACKAGE Vectortypes::TMatrix4f EmptyHmgMatrix;
extern PACKAGE TQuaternion IdentityQuaternion;
extern PACKAGE float EPSILON;
extern PACKAGE float EPSILON2;
extern PACKAGE float cPI;
extern PACKAGE float cPIdiv180;
extern PACKAGE float c180divPI;
extern PACKAGE float c2PI;
extern PACKAGE float cPIdiv2;
extern PACKAGE float cPIdiv4;
extern PACKAGE float c3PIdiv2;
extern PACKAGE float c3PIdiv4;
extern PACKAGE float cInv2PI;
extern PACKAGE float cInv360;
extern PACKAGE float c180;
extern PACKAGE float c360;
extern PACKAGE float cOneHalf;
extern PACKAGE float cLn10;
static const System::Extended MinSingle = 1.500000E-45;
static const System::Extended MaxSingle = 3.400000E+38;
static const System::Extended MinDouble = 5.000000E-324;
static const System::Extended MaxDouble = 1.700000E+308;
static const System::Extended MinExtended = 3.400000E-4932;
static const System::Extended MaxExtended = 1.100000E+4932;
static const System::Extended MinComp = -9.223372E+18;
static const System::Extended MaxComp = 9.223372E+18;
extern PACKAGE System::Byte vSIMD;
extern PACKAGE System::UnicodeString __fastcall GeometryOptimizationMode(void);
extern PACKAGE void __fastcall BeginFPUOnlySection(void);
extern PACKAGE void __fastcall EndFPUOnlySection(void);
extern PACKAGE TTexPoint __fastcall TexPointMake(const float s, const float t);
extern PACKAGE Vectortypes::TVector3f __fastcall AffineVectorMake(const float x, const float y, const float z)/* overload */;
extern PACKAGE Vectortypes::TVector3f __fastcall AffineVectorMake(const Vectortypes::TVector4f &v)/* overload */;
extern PACKAGE void __fastcall SetAffineVector(/* out */ Vectortypes::TVector3f &v, const float x, const float y, const float z)/* overload */;
extern PACKAGE void __fastcall SetVector(/* out */ Vectortypes::TVector3f &v, const float x, const float y, const float z)/* overload */;
extern PACKAGE void __fastcall SetVector(/* out */ Vectortypes::TVector3f &v, const Vectortypes::TVector4f &vSrc)/* overload */;
extern PACKAGE void __fastcall SetVector(/* out */ Vectortypes::TVector3f &v, const Vectortypes::TVector3f &vSrc)/* overload */;
extern PACKAGE void __fastcall SetVector(/* out */ Vectortypes::TVector3d &v, const Vectortypes::TVector3f &vSrc)/* overload */;
extern PACKAGE void __fastcall SetVector(/* out */ Vectortypes::TVector3d &v, const Vectortypes::TVector4f &vSrc)/* overload */;
extern PACKAGE Vectortypes::TVector4f __fastcall VectorMake(const Vectortypes::TVector3f &v, float w = 0.000000E+00)/* overload */;
extern PACKAGE Vectortypes::TVector4f __fastcall VectorMake(const float x, const float y, const float z, float w = 0.000000E+00)/* overload */;
extern PACKAGE Vectortypes::TVector4f __fastcall PointMake(const float x, const float y, const float z)/* overload */;
extern PACKAGE Vectortypes::TVector4f __fastcall PointMake(const Vectortypes::TVector3f &v)/* overload */;
extern PACKAGE Vectortypes::TVector4f __fastcall PointMake(const Vectortypes::TVector4f &v)/* overload */;
extern PACKAGE void __fastcall SetVector(/* out */ Vectortypes::TVector4f &v, const float x, const float y, const float z, float w = 0.000000E+00)/* overload */;
extern PACKAGE void __fastcall SetVector(/* out */ Vectortypes::TVector4f &v, const Vectortypes::TVector3f &av, float w = 0.000000E+00)/* overload */;
extern PACKAGE void __fastcall SetVector(/* out */ Vectortypes::TVector4f &v, const Vectortypes::TVector4f &vSrc)/* overload */;
extern PACKAGE void __fastcall MakePoint(/* out */ Vectortypes::TVector4f &v, const float x, const float y, const float z)/* overload */;
extern PACKAGE void __fastcall MakePoint(/* out */ Vectortypes::TVector4f &v, const Vectortypes::TVector3f &av)/* overload */;
extern PACKAGE void __fastcall MakePoint(/* out */ Vectortypes::TVector4f &v, const Vectortypes::TVector4f &av)/* overload */;
extern PACKAGE void __fastcall MakeVector(/* out */ Vectortypes::TVector3f &v, const float x, const float y, const float z)/* overload */;
extern PACKAGE void __fastcall MakeVector(/* out */ Vectortypes::TVector4f &v, const float x, const float y, const float z)/* overload */;
extern PACKAGE void __fastcall MakeVector(/* out */ Vectortypes::TVector4f &v, const Vectortypes::TVector3f &av)/* overload */;
extern PACKAGE void __fastcall MakeVector(/* out */ Vectortypes::TVector4f &v, const Vectortypes::TVector4f &av)/* overload */;
extern PACKAGE void __fastcall RstVector(Vectortypes::TVector3f &v)/* overload */;
extern PACKAGE void __fastcall RstVector(Vectortypes::TVector4f &v)/* overload */;
extern PACKAGE Vectortypes::TVector2f __fastcall VectorAdd(const Vectortypes::TVector2f &v1, const Vectortypes::TVector2f &v2)/* overload */;
extern PACKAGE Vectortypes::TVector3f __fastcall VectorAdd(const Vectortypes::TVector3f &v1, const Vectortypes::TVector3f &v2)/* overload */;
extern PACKAGE void __fastcall VectorAdd(const Vectortypes::TVector3f &v1, const Vectortypes::TVector3f &v2, Vectortypes::TVector3f &vr)/* overload */;
extern PACKAGE void __fastcall VectorAdd(const Vectortypes::TVector3f &v1, const Vectortypes::TVector3f &v2, PAffineVector vr)/* overload */;
extern PACKAGE Vectortypes::TVector4f __fastcall VectorAdd(const Vectortypes::TVector4f &v1, const Vectortypes::TVector4f &v2)/* overload */;
extern PACKAGE void __fastcall VectorAdd(const Vectortypes::TVector4f &v1, const Vectortypes::TVector4f &v2, Vectortypes::TVector4f &vr)/* overload */;
extern PACKAGE Vectortypes::TVector3f __fastcall VectorAdd(const Vectortypes::TVector3f &v, const float f)/* overload */;
extern PACKAGE Vectortypes::TVector4f __fastcall VectorAdd(const Vectortypes::TVector4f &v, const float f)/* overload */;
extern PACKAGE Vectortypes::TVector4f __fastcall PointAdd(Vectortypes::TVector4f &v1, const Vectortypes::TVector4f &v2)/* overload */;
extern PACKAGE void __fastcall AddVector(Vectortypes::TVector3f &v1, const Vectortypes::TVector3f &v2)/* overload */;
extern PACKAGE void __fastcall AddVector(Vectortypes::TVector3f &v1, const Vectortypes::TVector4f &v2)/* overload */;
extern PACKAGE void __fastcall AddVector(Vectortypes::TVector4f &v1, const Vectortypes::TVector4f &v2)/* overload */;
extern PACKAGE void __fastcall AddVector(Vectortypes::TVector3f &v, const float f)/* overload */;
extern PACKAGE void __fastcall AddVector(Vectortypes::TVector4f &v, const float f)/* overload */;
extern PACKAGE void __fastcall AddPoint(Vectortypes::TVector4f &v1, const Vectortypes::TVector4f &v2)/* overload */;
extern PACKAGE void __fastcall TexPointArrayAdd(const PTexPointArray src, const TTexPoint &delta, const int nb, PTexPointArray dest)/* overload */;
extern PACKAGE void __fastcall TexPointArrayScaleAndAdd(const PTexPointArray src, const TTexPoint &delta, const int nb, const TTexPoint &scale, PTexPointArray dest)/* overload */;
extern PACKAGE void __fastcall VectorArrayAdd(const PAffineVectorArray src, const Vectortypes::TVector3f &delta, const int nb, PAffineVectorArray dest)/* overload */;
extern PACKAGE Vectortypes::TVector3f __fastcall VectorSubtract(const Vectortypes::TVector3f &V1, const Vectortypes::TVector3f &V2)/* overload */;
extern PACKAGE Vectortypes::TVector2f __fastcall VectorSubtract(const Vectortypes::TVector2f &V1, const Vectortypes::TVector2f &V2)/* overload */;
extern PACKAGE void __fastcall VectorSubtract(const Vectortypes::TVector3f &v1, const Vectortypes::TVector3f &v2, Vectortypes::TVector3f &result)/* overload */;
extern PACKAGE void __fastcall VectorSubtract(const Vectortypes::TVector3f &v1, const Vectortypes::TVector3f &v2, Vectortypes::TVector4f &result)/* overload */;
extern PACKAGE void __fastcall VectorSubtract(const Vectortypes::TVector4f &v1, const Vectortypes::TVector3f &v2, Vectortypes::TVector4f &result)/* overload */;
extern PACKAGE Vectortypes::TVector4f __fastcall VectorSubtract(const Vectortypes::TVector4f &V1, const Vectortypes::TVector4f &V2)/* overload */;
extern PACKAGE void __fastcall VectorSubtract(const Vectortypes::TVector4f &v1, const Vectortypes::TVector4f &v2, Vectortypes::TVector4f &result)/* overload */;
extern PACKAGE void __fastcall VectorSubtract(const Vectortypes::TVector4f &v1, const Vectortypes::TVector4f &v2, Vectortypes::TVector3f &result)/* overload */;
extern PACKAGE Vectortypes::TVector3f __fastcall VectorSubtract(const Vectortypes::TVector3f &v1, float delta)/* overload */;
extern PACKAGE Vectortypes::TVector4f __fastcall VectorSubtract(const Vectortypes::TVector4f &v1, float delta)/* overload */;
extern PACKAGE void __fastcall SubtractVector(Vectortypes::TVector3f &V1, const Vectortypes::TVector3f &V2)/* overload */;
extern PACKAGE void __fastcall SubtractVector(Vectortypes::TVector2f &V1, const Vectortypes::TVector2f &V2)/* overload */;
extern PACKAGE void __fastcall SubtractVector(Vectortypes::TVector4f &V1, const Vectortypes::TVector4f &V2)/* overload */;
extern PACKAGE void __fastcall CombineVector(Vectortypes::TVector3f &vr, const Vectortypes::TVector3f &v, float &f)/* overload */;
extern PACKAGE void __fastcall CombineVector(Vectortypes::TVector3f &vr, const Vectortypes::TVector3f &v, System::PSingle pf)/* overload */;
extern PACKAGE TTexPoint __fastcall TexPointCombine(const TTexPoint &t1, const TTexPoint &t2, float f1, float f2);
extern PACKAGE Vectortypes::TVector3f __fastcall VectorCombine(const Vectortypes::TVector3f &V1, const Vectortypes::TVector3f &V2, const float F1, const float F2)/* overload */;
extern PACKAGE Vectortypes::TVector3f __fastcall VectorCombine3(const Vectortypes::TVector3f &V1, const Vectortypes::TVector3f &V2, const Vectortypes::TVector3f &V3, const float F1, const float F2, const float F3)/* overload */;
extern PACKAGE void __fastcall VectorCombine3(const Vectortypes::TVector3f &V1, const Vectortypes::TVector3f &V2, const Vectortypes::TVector3f &V3, const float F1, const float F2, const float F3, Vectortypes::TVector3f &vr)/* overload */;
extern PACKAGE void __fastcall CombineVector(Vectortypes::TVector4f &vr, const Vectortypes::TVector4f &v, float &f)/* overload */;
extern PACKAGE void __fastcall CombineVector(Vectortypes::TVector4f &vr, const Vectortypes::TVector3f &v, float &f)/* overload */;
extern PACKAGE Vectortypes::TVector4f __fastcall VectorCombine(const Vectortypes::TVector4f &V1, const Vectortypes::TVector4f &V2, const float F1, const float F2)/* overload */;
extern PACKAGE Vectortypes::TVector4f __fastcall VectorCombine(const Vectortypes::TVector4f &V1, const Vectortypes::TVector3f &V2, const float F1, const float F2)/* overload */;
extern PACKAGE void __fastcall VectorCombine(const Vectortypes::TVector4f &V1, const Vectortypes::TVector4f &V2, const float F1, const float F2, Vectortypes::TVector4f &vr)/* overload */;
extern PACKAGE void __fastcall VectorCombine(const Vectortypes::TVector4f &V1, const Vectortypes::TVector4f &V2, const float F2, Vectortypes::TVector4f &vr)/* overload */;
extern PACKAGE void __fastcall VectorCombine(const Vectortypes::TVector4f &V1, const Vectortypes::TVector3f &V2, const float F1, const float F2, Vectortypes::TVector4f &vr)/* overload */;
extern PACKAGE Vectortypes::TVector4f __fastcall VectorCombine3(const Vectortypes::TVector4f &V1, const Vectortypes::TVector4f &V2, const Vectortypes::TVector4f &V3, const float F1, const float F2, const float F3)/* overload */;
extern PACKAGE void __fastcall VectorCombine3(const Vectortypes::TVector4f &V1, const Vectortypes::TVector4f &V2, const Vectortypes::TVector4f &V3, const float F1, const float F2, const float F3, Vectortypes::TVector4f &vr)/* overload */;
extern PACKAGE float __fastcall VectorDotProduct(const Vectortypes::TVector2f &V1, const Vectortypes::TVector2f &V2)/* overload */;
extern PACKAGE float __fastcall VectorDotProduct(const Vectortypes::TVector3f &V1, const Vectortypes::TVector3f &V2)/* overload */;
extern PACKAGE float __fastcall VectorDotProduct(const Vectortypes::TVector4f &V1, const Vectortypes::TVector4f &V2)/* overload */;
extern PACKAGE float __fastcall VectorDotProduct(const Vectortypes::TVector4f &V1, const Vectortypes::TVector3f &V2)/* overload */;
extern PACKAGE float __fastcall PointProject(const Vectortypes::TVector3f &p, const Vectortypes::TVector3f &origin, const Vectortypes::TVector3f &direction)/* overload */;
extern PACKAGE float __fastcall PointProject(const Vectortypes::TVector4f &p, const Vectortypes::TVector4f &origin, const Vectortypes::TVector4f &direction)/* overload */;
extern PACKAGE Vectortypes::TVector3f __fastcall VectorCrossProduct(const Vectortypes::TVector3f &V1, const Vectortypes::TVector3f &V2)/* overload */;
extern PACKAGE Vectortypes::TVector4f __fastcall VectorCrossProduct(const Vectortypes::TVector4f &V1, const Vectortypes::TVector4f &V2)/* overload */;
extern PACKAGE void __fastcall VectorCrossProduct(const Vectortypes::TVector4f &v1, const Vectortypes::TVector4f &v2, Vectortypes::TVector4f &vr)/* overload */;
extern PACKAGE void __fastcall VectorCrossProduct(const Vectortypes::TVector3f &v1, const Vectortypes::TVector3f &v2, Vectortypes::TVector4f &vr)/* overload */;
extern PACKAGE void __fastcall VectorCrossProduct(const Vectortypes::TVector4f &v1, const Vectortypes::TVector4f &v2, Vectortypes::TVector3f &vr)/* overload */;
extern PACKAGE void __fastcall VectorCrossProduct(const Vectortypes::TVector3f &v1, const Vectortypes::TVector3f &v2, Vectortypes::TVector3f &vr)/* overload */;
extern PACKAGE float __fastcall Lerp(const float start, const float stop, const float t);
extern PACKAGE float __fastcall AngleLerp(float start, float stop, float t);
extern PACKAGE float __fastcall DistanceBetweenAngles(float angle1, float angle2);
extern PACKAGE TTexPoint __fastcall TexPointLerp(const TTexPoint &t1, const TTexPoint &t2, float t)/* overload */;
extern PACKAGE Vectortypes::TVector3f __fastcall VectorLerp(const Vectortypes::TVector3f &v1, const Vectortypes::TVector3f &v2, float t)/* overload */;
extern PACKAGE void __fastcall VectorLerp(const Vectortypes::TVector3f &v1, const Vectortypes::TVector3f &v2, float t, Vectortypes::TVector3f &vr)/* overload */;
extern PACKAGE Vectortypes::TVector4f __fastcall VectorLerp(const Vectortypes::TVector4f &v1, const Vectortypes::TVector4f &v2, float t)/* overload */;
extern PACKAGE void __fastcall VectorLerp(const Vectortypes::TVector4f &v1, const Vectortypes::TVector4f &v2, float t, Vectortypes::TVector4f &vr)/* overload */;
extern PACKAGE Vectortypes::TVector3f __fastcall VectorAngleLerp(const Vectortypes::TVector3f &v1, const Vectortypes::TVector3f &v2, float t)/* overload */;
extern PACKAGE Vectortypes::TVector3f __fastcall VectorAngleCombine(const Vectortypes::TVector3f &v1, const Vectortypes::TVector3f &v2, float f)/* overload */;
extern PACKAGE void __fastcall VectorArrayLerp(const PVectorArray src1, const PVectorArray src2, float t, int n, PVectorArray dest)/* overload */;
extern PACKAGE void __fastcall VectorArrayLerp(const PAffineVectorArray src1, const PAffineVectorArray src2, float t, int n, PAffineVectorArray dest)/* overload */;
extern PACKAGE void __fastcall VectorArrayLerp(const PTexPointArray src1, const PTexPointArray src2, float t, int n, PTexPointArray dest)/* overload */;
extern PACKAGE float __fastcall InterpolateCombined(const float Start, const float Stop, const float Delta, const float DistortionDegree, const TGLInterpolationType InterpolationType);
extern PACKAGE float __fastcall InterpolateCombinedFastPower(const float OriginalStart, const float OriginalStop, const float OriginalCurrent, const float TargetStart, const float TargetStop, const float DistortionDegree);
extern PACKAGE float __fastcall InterpolateCombinedSafe(const float OriginalStart, const float OriginalStop, const float OriginalCurrent, const float TargetStart, const float TargetStop, const float DistortionDegree, const TGLInterpolationType InterpolationType);
extern PACKAGE float __fastcall InterpolateCombinedFast(const float OriginalStart, const float OriginalStop, const float OriginalCurrent, const float TargetStart, const float TargetStop, const float DistortionDegree, const TGLInterpolationType InterpolationType);
extern PACKAGE float __fastcall InterpolateLn(const float Start, const float Stop, const float Delta, const float DistortionDegree);
extern PACKAGE float __fastcall InterpolateExp(const float Start, const float Stop, const float Delta, const float DistortionDegree);
extern PACKAGE float __fastcall InterpolateSinAlt(const float Start, const float Stop, const float Delta);
extern PACKAGE float __fastcall InterpolateSin(const float Start, const float Stop, const float Delta);
extern PACKAGE float __fastcall InterpolateTan(const float Start, const float Stop, const float Delta);
extern PACKAGE float __fastcall InterpolatePower(const float Start, const float Stop, const float Delta, const float DistortionDegree);
extern PACKAGE Vectortypes::TMatrix4f __fastcall MatrixLerp(const Vectortypes::TMatrix4f &m1, const Vectortypes::TMatrix4f &m2, const float Delta);
extern PACKAGE float __fastcall VectorLength(float const *v, const int v_Size)/* overload */;
extern PACKAGE float __fastcall VectorLength(const float x, const float y)/* overload */;
extern PACKAGE float __fastcall VectorLength(const float x, const float y, const float z)/* overload */;
extern PACKAGE float __fastcall VectorLength(const Vectortypes::TVector2f &v)/* overload */;
extern PACKAGE float __fastcall VectorLength(const Vectortypes::TVector3f &v)/* overload */;
extern PACKAGE float __fastcall VectorLength(const Vectortypes::TVector4f &v)/* overload */;
extern PACKAGE float __fastcall VectorNorm(const float x, const float y)/* overload */;
extern PACKAGE float __fastcall VectorNorm(const Vectortypes::TVector3f &v)/* overload */;
extern PACKAGE float __fastcall VectorNorm(const Vectortypes::TVector4f &v)/* overload */;
extern PACKAGE float __fastcall VectorNorm(float *V, const int V_Size)/* overload */;
extern PACKAGE void __fastcall NormalizeVector(Vectortypes::TVector2f &v)/* overload */;
extern PACKAGE void __fastcall NormalizeVector(Vectortypes::TVector3f &v)/* overload */;
extern PACKAGE Vectortypes::TVector2f __fastcall VectorNormalize(const Vectortypes::TVector2f &v)/* overload */;
extern PACKAGE Vectortypes::TVector3f __fastcall VectorNormalize(const Vectortypes::TVector3f &v)/* overload */;
extern PACKAGE void __fastcall NormalizeVectorArray(PAffineVectorArray list, int n)/* overload */;
extern PACKAGE void __fastcall NormalizeVector(Vectortypes::TVector4f &v)/* overload */;
extern PACKAGE Vectortypes::TVector4f __fastcall VectorNormalize(const Vectortypes::TVector4f &v)/* overload */;
extern PACKAGE float __fastcall VectorAngleCosine(const Vectortypes::TVector3f &V1, const Vectortypes::TVector3f &V2)/* overload */;
extern PACKAGE float __fastcall VectorAngleCosine(const Vectortypes::TVector4f &V1, const Vectortypes::TVector4f &V2)/* overload */;
extern PACKAGE Vectortypes::TVector3f __fastcall VectorNegate(const Vectortypes::TVector3f &Vector)/* overload */;
extern PACKAGE Vectortypes::TVector4f __fastcall VectorNegate(const Vectortypes::TVector4f &Vector)/* overload */;
extern PACKAGE void __fastcall NegateVector(Vectortypes::TVector3f &V)/* overload */;
extern PACKAGE void __fastcall NegateVector(Vectortypes::TVector4f &V)/* overload */;
extern PACKAGE void __fastcall NegateVector(float *V, const int V_Size)/* overload */;
extern PACKAGE void __fastcall ScaleVector(Vectortypes::TVector2f &v, float factor)/* overload */;
extern PACKAGE void __fastcall ScaleVector(Vectortypes::TVector3f &v, float factor)/* overload */;
extern PACKAGE void __fastcall ScaleVector(Vectortypes::TVector4f &v, float factor)/* overload */;
extern PACKAGE void __fastcall ScaleVector(Vectortypes::TVector3f &v, const Vectortypes::TVector3f &factor)/* overload */;
extern PACKAGE void __fastcall ScaleVector(Vectortypes::TVector4f &v, const Vectortypes::TVector4f &factor)/* overload */;
extern PACKAGE Vectortypes::TVector2f __fastcall VectorScale(const Vectortypes::TVector2f &v, float factor)/* overload */;
extern PACKAGE Vectortypes::TVector3f __fastcall VectorScale(const Vectortypes::TVector3f &v, float factor)/* overload */;
extern PACKAGE void __fastcall VectorScale(const Vectortypes::TVector3f &v, float factor, Vectortypes::TVector3f &vr)/* overload */;
extern PACKAGE Vectortypes::TVector4f __fastcall VectorScale(const Vectortypes::TVector4f &v, float factor)/* overload */;
extern PACKAGE void __fastcall VectorScale(const Vectortypes::TVector4f &v, float factor, Vectortypes::TVector4f &vr)/* overload */;
extern PACKAGE void __fastcall VectorScale(const Vectortypes::TVector4f &v, float factor, Vectortypes::TVector3f &vr)/* overload */;
extern PACKAGE Vectortypes::TVector3f __fastcall VectorScale(const Vectortypes::TVector3f &v, const Vectortypes::TVector3f &Factor)/* overload */;
extern PACKAGE Vectortypes::TVector4f __fastcall VectorScale(const Vectortypes::TVector4f &v, const Vectortypes::TVector4f &Factor)/* overload */;
extern PACKAGE void __fastcall DivideVector(Vectortypes::TVector4f &v, const Vectortypes::TVector4f &divider)/* overload */;
extern PACKAGE void __fastcall DivideVector(Vectortypes::TVector3f &v, const Vectortypes::TVector3f &divider)/* overload */;
extern PACKAGE Vectortypes::TVector4f __fastcall VectorDivide(const Vectortypes::TVector4f &v, const Vectortypes::TVector4f &divider)/* overload */;
extern PACKAGE Vectortypes::TVector3f __fastcall VectorDivide(const Vectortypes::TVector3f &v, const Vectortypes::TVector3f &divider)/* overload */;
extern PACKAGE bool __fastcall TexpointEquals(const TTexPoint &p1, const TTexPoint &p2);
extern PACKAGE bool __fastcall RectEquals(const System::Types::TRect &Rect1, const System::Types::TRect &Rect2);
extern PACKAGE bool __fastcall VectorEquals(const Vectortypes::TVector4f &V1, const Vectortypes::TVector4f &V2)/* overload */;
extern PACKAGE bool __fastcall VectorEquals(const Vectortypes::TVector3f &V1, const Vectortypes::TVector3f &V2)/* overload */;
extern PACKAGE bool __fastcall AffineVectorEquals(const Vectortypes::TVector4f &V1, const Vectortypes::TVector4f &V2)/* overload */;
extern PACKAGE bool __fastcall VectorIsNull(const Vectortypes::TVector4f &v)/* overload */;
extern PACKAGE bool __fastcall VectorIsNull(const Vectortypes::TVector3f &v)/* overload */;
extern PACKAGE float __fastcall VectorSpacing(const TTexPoint &v1, const TTexPoint &v2)/* overload */;
extern PACKAGE float __fastcall VectorSpacing(const Vectortypes::TVector3f &v1, const Vectortypes::TVector3f &v2)/* overload */;
extern PACKAGE float __fastcall VectorSpacing(const Vectortypes::TVector4f &v1, const Vectortypes::TVector4f &v2)/* overload */;
extern PACKAGE float __fastcall VectorDistance(const Vectortypes::TVector3f &v1, const Vectortypes::TVector3f &v2)/* overload */;
extern PACKAGE float __fastcall VectorDistance(const Vectortypes::TVector4f &v1, const Vectortypes::TVector4f &v2)/* overload */;
extern PACKAGE float __fastcall VectorDistance2(const Vectortypes::TVector3f &v1, const Vectortypes::TVector3f &v2)/* overload */;
extern PACKAGE float __fastcall VectorDistance2(const Vectortypes::TVector4f &v1, const Vectortypes::TVector4f &v2)/* overload */;
extern PACKAGE Vectortypes::TVector3f __fastcall VectorPerpendicular(const Vectortypes::TVector3f &V, const Vectortypes::TVector3f &N);
extern PACKAGE Vectortypes::TVector3f __fastcall VectorReflect(const Vectortypes::TVector3f &V, const Vectortypes::TVector3f &N);
extern PACKAGE void __fastcall RotateVector(Vectortypes::TVector4f &vector, const Vectortypes::TVector3f &axis, float angle)/* overload */;
extern PACKAGE void __fastcall RotateVector(Vectortypes::TVector4f &vector, const Vectortypes::TVector4f &axis, float angle)/* overload */;
extern PACKAGE void __fastcall RotateVectorAroundY(Vectortypes::TVector3f &v, float alpha);
extern PACKAGE Vectortypes::TVector3f __fastcall VectorRotateAroundX(const Vectortypes::TVector3f &v, float alpha)/* overload */;
extern PACKAGE Vectortypes::TVector3f __fastcall VectorRotateAroundY(const Vectortypes::TVector3f &v, float alpha)/* overload */;
extern PACKAGE void __fastcall VectorRotateAroundY(const Vectortypes::TVector3f &v, float alpha, Vectortypes::TVector3f &vr)/* overload */;
extern PACKAGE Vectortypes::TVector3f __fastcall VectorRotateAroundZ(const Vectortypes::TVector3f &v, float alpha)/* overload */;
extern PACKAGE void __fastcall AbsVector(Vectortypes::TVector4f &v)/* overload */;
extern PACKAGE void __fastcall AbsVector(Vectortypes::TVector3f &v)/* overload */;
extern PACKAGE Vectortypes::TVector4f __fastcall VectorAbs(const Vectortypes::TVector4f &v)/* overload */;
extern PACKAGE Vectortypes::TVector3f __fastcall VectorAbs(const Vectortypes::TVector3f &v)/* overload */;
extern PACKAGE bool __fastcall IsColinear(const Vectortypes::TVector2f &v1, const Vectortypes::TVector2f &v2)/* overload */;
extern PACKAGE bool __fastcall IsColinear(const Vectortypes::TVector3f &v1, const Vectortypes::TVector3f &v2)/* overload */;
extern PACKAGE bool __fastcall IsColinear(const Vectortypes::TVector4f &v1, const Vectortypes::TVector4f &v2)/* overload */;
extern PACKAGE void __fastcall SetMatrix(Vectortypes::TMatrix4d &dest, const Vectortypes::TMatrix4f &src)/* overload */;
extern PACKAGE void __fastcall SetMatrix(Vectortypes::TMatrix3f &dest, const Vectortypes::TMatrix4f &src)/* overload */;
extern PACKAGE void __fastcall SetMatrix(Vectortypes::TMatrix4f &dest, const Vectortypes::TMatrix3f &src)/* overload */;
extern PACKAGE void __fastcall SetMatrixRow(Vectortypes::TMatrix4f &dest, int rowNb, const Vectortypes::TVector4f &aRow)/* overload */;
extern PACKAGE Vectortypes::TMatrix4f __fastcall CreateScaleMatrix(const Vectortypes::TVector3f &v)/* overload */;
extern PACKAGE Vectortypes::TMatrix4f __fastcall CreateScaleMatrix(const Vectortypes::TVector4f &v)/* overload */;
extern PACKAGE Vectortypes::TMatrix4f __fastcall CreateTranslationMatrix(const Vectortypes::TVector3f &V)/* overload */;
extern PACKAGE Vectortypes::TMatrix4f __fastcall CreateTranslationMatrix(const Vectortypes::TVector4f &V)/* overload */;
extern PACKAGE Vectortypes::TMatrix4f __fastcall CreateScaleAndTranslationMatrix(const Vectortypes::TVector4f &scale, const Vectortypes::TVector4f &offset)/* overload */;
extern PACKAGE Vectortypes::TMatrix4f __fastcall CreateRotationMatrixX(const float sine, const float cosine)/* overload */;
extern PACKAGE Vectortypes::TMatrix4f __fastcall CreateRotationMatrixX(const float angle)/* overload */;
extern PACKAGE Vectortypes::TMatrix4f __fastcall CreateRotationMatrixY(const float sine, const float cosine)/* overload */;
extern PACKAGE Vectortypes::TMatrix4f __fastcall CreateRotationMatrixY(const float angle)/* overload */;
extern PACKAGE Vectortypes::TMatrix4f __fastcall CreateRotationMatrixZ(const float sine, const float cosine)/* overload */;
extern PACKAGE Vectortypes::TMatrix4f __fastcall CreateRotationMatrixZ(const float angle)/* overload */;
extern PACKAGE Vectortypes::TMatrix4f __fastcall CreateRotationMatrix(const Vectortypes::TVector3f &anAxis, float angle)/* overload */;
extern PACKAGE Vectortypes::TMatrix4f __fastcall CreateRotationMatrix(const Vectortypes::TVector4f &anAxis, float angle)/* overload */;
extern PACKAGE Vectortypes::TMatrix3f __fastcall CreateAffineRotationMatrix(const Vectortypes::TVector3f &anAxis, float angle);
extern PACKAGE Vectortypes::TMatrix3f __fastcall MatrixMultiply(const Vectortypes::TMatrix3f &M1, const Vectortypes::TMatrix3f &M2)/* overload */;
extern PACKAGE Vectortypes::TMatrix4f __fastcall MatrixMultiply(const Vectortypes::TMatrix4f &M1, const Vectortypes::TMatrix4f &M2)/* overload */;
extern PACKAGE void __fastcall MatrixMultiply(const Vectortypes::TMatrix4f &M1, const Vectortypes::TMatrix4f &M2, Vectortypes::TMatrix4f &MResult)/* overload */;
extern PACKAGE Vectortypes::TVector4f __fastcall VectorTransform(const Vectortypes::TVector4f &V, const Vectortypes::TMatrix4f &M)/* overload */;
extern PACKAGE Vectortypes::TVector4f __fastcall VectorTransform(const Vectortypes::TVector4f &V, const Vectortypes::TMatrix3f &M)/* overload */;
extern PACKAGE Vectortypes::TVector3f __fastcall VectorTransform(const Vectortypes::TVector3f &V, const Vectortypes::TMatrix4f &M)/* overload */;
extern PACKAGE Vectortypes::TVector3f __fastcall VectorTransform(const Vectortypes::TVector3f &V, const Vectortypes::TMatrix3f &M)/* overload */;
extern PACKAGE float __fastcall MatrixDeterminant(const Vectortypes::TMatrix3f &M)/* overload */;
extern PACKAGE float __fastcall MatrixDeterminant(const Vectortypes::TMatrix4f &M)/* overload */;
extern PACKAGE void __fastcall AdjointMatrix(Vectortypes::TMatrix4f &M)/* overload */;
extern PACKAGE void __fastcall AdjointMatrix(Vectortypes::TMatrix3f &M)/* overload */;
extern PACKAGE void __fastcall ScaleMatrix(Vectortypes::TMatrix3f &M, const float factor)/* overload */;
extern PACKAGE void __fastcall ScaleMatrix(Vectortypes::TMatrix4f &M, const float factor)/* overload */;
extern PACKAGE void __fastcall TranslateMatrix(Vectortypes::TMatrix4f &M, const Vectortypes::TVector3f &v)/* overload */;
extern PACKAGE void __fastcall TranslateMatrix(Vectortypes::TMatrix4f &M, const Vectortypes::TVector4f &v)/* overload */;
extern PACKAGE void __fastcall NormalizeMatrix(Vectortypes::TMatrix4f &M);
extern PACKAGE void __fastcall TransposeMatrix(Vectortypes::TMatrix3f &M)/* overload */;
extern PACKAGE void __fastcall TransposeMatrix(Vectortypes::TMatrix4f &M)/* overload */;
extern PACKAGE void __fastcall InvertMatrix(Vectortypes::TMatrix4f &M)/* overload */;
extern PACKAGE Vectortypes::TMatrix4f __fastcall MatrixInvert(const Vectortypes::TMatrix4f &M)/* overload */;
extern PACKAGE void __fastcall InvertMatrix(Vectortypes::TMatrix3f &M)/* overload */;
extern PACKAGE Vectortypes::TMatrix3f __fastcall MatrixInvert(const Vectortypes::TMatrix3f &M)/* overload */;
extern PACKAGE Vectortypes::TMatrix4f __fastcall AnglePreservingMatrixInvert(const Vectortypes::TMatrix4f &mat);
extern PACKAGE bool __fastcall MatrixDecompose(const Vectortypes::TMatrix4f &M, float *Tran);
extern PACKAGE Vectortypes::TMatrix4f __fastcall CreateLookAtMatrix(const Vectortypes::TVector4f &eye, const Vectortypes::TVector4f &center, const Vectortypes::TVector4f &normUp);
extern PACKAGE Vectortypes::TMatrix4f __fastcall CreateMatrixFromFrustum(float Left, float Right, float Bottom, float Top, float ZNear, float ZFar);
extern PACKAGE Vectortypes::TMatrix4f __fastcall CreatePerspectiveMatrix(float FOV, float Aspect, float ZNear, float ZFar);
extern PACKAGE Vectortypes::TMatrix4f __fastcall CreateOrthoMatrix(float Left, float Right, float Bottom, float Top, float ZNear, float ZFar);
extern PACKAGE Vectortypes::TMatrix4f __fastcall CreatePickMatrix(float x, float y, float deltax, float deltay, const Vectortypes::TVector4i &viewport);
extern PACKAGE bool __fastcall Project(const Vectortypes::TVector4f &objectVector, const Vectortypes::TMatrix4f &ViewProjMatrix, const Vectortypes::TVector4i &viewport, /* out */ Vectortypes::TVector4f &WindowVector);
extern PACKAGE bool __fastcall UnProject(const Vectortypes::TVector4f &WindowVector, const Vectortypes::TMatrix4f &ViewProjMatrix, const Vectortypes::TVector4i &viewport, /* out */ Vectortypes::TVector4f &objectVector);
extern PACKAGE Vectortypes::TVector3f __fastcall CalcPlaneNormal(const Vectortypes::TVector3f &p1, const Vectortypes::TVector3f &p2, const Vectortypes::TVector3f &p3)/* overload */;
extern PACKAGE void __fastcall CalcPlaneNormal(const Vectortypes::TVector3f &p1, const Vectortypes::TVector3f &p2, const Vectortypes::TVector3f &p3, Vectortypes::TVector3f &vr)/* overload */;
extern PACKAGE void __fastcall CalcPlaneNormal(const Vectortypes::TVector4f &p1, const Vectortypes::TVector4f &p2, const Vectortypes::TVector4f &p3, Vectortypes::TVector3f &vr)/* overload */;
extern PACKAGE Vectortypes::TVector4f __fastcall PlaneMake(const Vectortypes::TVector3f &point, const Vectortypes::TVector3f &normal)/* overload */;
extern PACKAGE Vectortypes::TVector4f __fastcall PlaneMake(const Vectortypes::TVector4f &point, const Vectortypes::TVector4f &normal)/* overload */;
extern PACKAGE Vectortypes::TVector4f __fastcall PlaneMake(const Vectortypes::TVector3f &p1, const Vectortypes::TVector3f &p2, const Vectortypes::TVector3f &p3)/* overload */;
extern PACKAGE Vectortypes::TVector4f __fastcall PlaneMake(const Vectortypes::TVector4f &p1, const Vectortypes::TVector4f &p2, const Vectortypes::TVector4f &p3)/* overload */;
extern PACKAGE void __fastcall SetPlane(Vectortypes::TVector4d &dest, const Vectortypes::TVector4f &src);
extern PACKAGE void __fastcall NormalizePlane(Vectortypes::TVector4f &plane);
extern PACKAGE float __fastcall PlaneEvaluatePoint(const Vectortypes::TVector4f &plane, const Vectortypes::TVector3f &point)/* overload */;
extern PACKAGE float __fastcall PlaneEvaluatePoint(const Vectortypes::TVector4f &plane, const Vectortypes::TVector4f &point)/* overload */;
extern PACKAGE bool __fastcall PointIsInHalfSpace(const Vectortypes::TVector4f &point, const Vectortypes::TVector4f &planePoint, const Vectortypes::TVector4f &planeNormal)/* overload */;
extern PACKAGE bool __fastcall PointIsInHalfSpace(const Vectortypes::TVector3f &point, const Vectortypes::TVector3f &planePoint, const Vectortypes::TVector3f &planeNormal)/* overload */;
extern PACKAGE bool __fastcall PointIsInHalfSpace(const Vectortypes::TVector3f &point, const Vectortypes::TVector4f &plane)/* overload */;
extern PACKAGE float __fastcall PointPlaneDistance(const Vectortypes::TVector4f &point, const Vectortypes::TVector4f &planePoint, const Vectortypes::TVector4f &planeNormal)/* overload */;
extern PACKAGE float __fastcall PointPlaneDistance(const Vectortypes::TVector3f &point, const Vectortypes::TVector3f &planePoint, const Vectortypes::TVector3f &planeNormal)/* overload */;
extern PACKAGE float __fastcall PointPlaneDistance(const Vectortypes::TVector3f &point, const Vectortypes::TVector4f &plane)/* overload */;
extern PACKAGE bool __fastcall PointPlaneOrthoProjection(const Vectortypes::TVector3f &point, const Vectortypes::TVector4f &plane, Vectortypes::TVector3f &inter, bool bothface = true);
extern PACKAGE bool __fastcall PointPlaneProjection(const Vectortypes::TVector3f &point, const Vectortypes::TVector3f &direction, const Vectortypes::TVector4f &plane, Vectortypes::TVector3f &inter, bool bothface = true);
extern PACKAGE bool __fastcall SegmentPlaneIntersection(const Vectortypes::TVector3f &ptA, const Vectortypes::TVector3f &ptB, const Vectortypes::TVector4f &plane, Vectortypes::TVector3f &inter);
extern PACKAGE bool __fastcall PointTriangleOrthoProjection(const Vectortypes::TVector3f &point, const Vectortypes::TVector3f &ptA, const Vectortypes::TVector3f &ptB, const Vectortypes::TVector3f &ptC, Vectortypes::TVector3f &inter, bool bothface = true);
extern PACKAGE bool __fastcall PointTriangleProjection(const Vectortypes::TVector3f &point, const Vectortypes::TVector3f &direction, const Vectortypes::TVector3f &ptA, const Vectortypes::TVector3f &ptB, const Vectortypes::TVector3f &ptC, Vectortypes::TVector3f &inter, bool bothface = true);
extern PACKAGE bool __fastcall IsLineIntersectTriangle(const Vectortypes::TVector3f &point, const Vectortypes::TVector3f &direction, const Vectortypes::TVector3f &ptA, const Vectortypes::TVector3f &ptB, const Vectortypes::TVector3f &ptC);
extern PACKAGE bool __fastcall PointQuadOrthoProjection(const Vectortypes::TVector3f &point, const Vectortypes::TVector3f &ptA, const Vectortypes::TVector3f &ptB, const Vectortypes::TVector3f &ptC, const Vectortypes::TVector3f &ptD, Vectortypes::TVector3f &inter, bool bothface = true);
extern PACKAGE bool __fastcall PointQuadProjection(const Vectortypes::TVector3f &point, const Vectortypes::TVector3f &direction, const Vectortypes::TVector3f &ptA, const Vectortypes::TVector3f &ptB, const Vectortypes::TVector3f &ptC, const Vectortypes::TVector3f &ptD, Vectortypes::TVector3f &inter, bool bothface = true);
extern PACKAGE bool __fastcall IsLineIntersectQuad(const Vectortypes::TVector3f &point, const Vectortypes::TVector3f &direction, const Vectortypes::TVector3f &ptA, const Vectortypes::TVector3f &ptB, const Vectortypes::TVector3f &ptC, const Vectortypes::TVector3f &ptD);
extern PACKAGE bool __fastcall PointDiskOrthoProjection(const Vectortypes::TVector3f &point, const Vectortypes::TVector3f &center, const Vectortypes::TVector3f &up, const float radius, Vectortypes::TVector3f &inter, bool bothface = true);
extern PACKAGE bool __fastcall PointDiskProjection(const Vectortypes::TVector3f &point, const Vectortypes::TVector3f &direction, const Vectortypes::TVector3f &center, const Vectortypes::TVector3f &up, const float radius, Vectortypes::TVector3f &inter, bool bothface = true);
extern PACKAGE Vectortypes::TVector3f __fastcall PointLineClosestPoint(const Vectortypes::TVector3f &point, const Vectortypes::TVector3f &linePoint, const Vectortypes::TVector3f &lineDirection);
extern PACKAGE float __fastcall PointLineDistance(const Vectortypes::TVector3f &point, const Vectortypes::TVector3f &linePoint, const Vectortypes::TVector3f &lineDirection);
extern PACKAGE Vectortypes::TVector4f __fastcall PointSegmentClosestPoint(const Vectortypes::TVector4f &point, const Vectortypes::TVector4f &segmentStart, const Vectortypes::TVector4f &segmentStop)/* overload */;
extern PACKAGE Vectortypes::TVector3f __fastcall PointSegmentClosestPoint(const Vectortypes::TVector3f &point, const Vectortypes::TVector3f &segmentStart, const Vectortypes::TVector3f &segmentStop)/* overload */;
extern PACKAGE float __fastcall PointSegmentDistance(const Vectortypes::TVector3f &point, const Vectortypes::TVector3f &segmentStart, const Vectortypes::TVector3f &segmentStop);
extern PACKAGE void __fastcall SegmentSegmentClosestPoint(const Vectortypes::TVector3f &S0Start, const Vectortypes::TVector3f &S0Stop, const Vectortypes::TVector3f &S1Start, const Vectortypes::TVector3f &S1Stop, Vectortypes::TVector3f &Segment0Closest, Vectortypes::TVector3f &Segment1Closest);
extern PACKAGE float __fastcall SegmentSegmentDistance(const Vectortypes::TVector3f &S0Start, const Vectortypes::TVector3f &S0Stop, const Vectortypes::TVector3f &S1Start, const Vectortypes::TVector3f &S1Stop);
extern PACKAGE float __fastcall LineLineDistance(const Vectortypes::TVector3f &linePt0, const Vectortypes::TVector3f &lineDir0, const Vectortypes::TVector3f &linePt1, const Vectortypes::TVector3f &lineDir1);
extern PACKAGE TQuaternion __fastcall QuaternionMake(float const *Imag, const int Imag_Size, float Real);
extern PACKAGE TQuaternion __fastcall QuaternionConjugate(const TQuaternion &Q);
extern PACKAGE float __fastcall QuaternionMagnitude(const TQuaternion &Q);
extern PACKAGE void __fastcall NormalizeQuaternion(TQuaternion &Q);
extern PACKAGE TQuaternion __fastcall QuaternionFromPoints(const Vectortypes::TVector3f &V1, const Vectortypes::TVector3f &V2);
extern PACKAGE TQuaternion __fastcall QuaternionFromMatrix(const Vectortypes::TMatrix4f &mat);
extern PACKAGE TQuaternion __fastcall QuaternionMultiply(const TQuaternion &qL, const TQuaternion &qR);
extern PACKAGE Vectortypes::TMatrix4f __fastcall QuaternionToMatrix(const TQuaternion &quat);
extern PACKAGE Vectortypes::TMatrix3f __fastcall QuaternionToAffineMatrix(const TQuaternion &quat);
extern PACKAGE TQuaternion __fastcall QuaternionFromAngleAxis(const float angle, const Vectortypes::TVector3f &axis);
extern PACKAGE TQuaternion __fastcall QuaternionFromRollPitchYaw(const float r, const float p, const float y);
extern PACKAGE TQuaternion __fastcall QuaternionFromEuler(const float x, const float y, const float z, TEulerOrder eulerOrder);
extern PACKAGE void __fastcall QuaternionToPoints(const TQuaternion &Q, Vectortypes::TVector3f &ArcFrom, Vectortypes::TVector3f &ArcTo);
extern PACKAGE System::Extended __fastcall LnXP1(System::Extended X);
extern PACKAGE System::Extended __fastcall Log10(System::Extended X);
extern PACKAGE System::Extended __fastcall Log2(System::Extended X)/* overload */;
extern PACKAGE float __fastcall Log2(float X)/* overload */;
extern PACKAGE System::Extended __fastcall LogN(System::Extended Base, System::Extended X);
extern PACKAGE System::Extended __fastcall IntPower(System::Extended Base, int Exponent);
extern PACKAGE float __fastcall Power(const float Base, const float Exponent)/* overload */;
extern PACKAGE float __fastcall Power(float Base, int Exponent)/* overload */;
extern PACKAGE float __fastcall Power(float Base, __int64 Exponent)/* overload */;
extern PACKAGE System::Extended __fastcall DegToRad(const System::Extended Degrees)/* overload */;
extern PACKAGE float __fastcall DegToRad(const float Degrees)/* overload */;
extern PACKAGE System::Extended __fastcall RadToDeg(const System::Extended Radians)/* overload */;
extern PACKAGE float __fastcall RadToDeg(const float Radians)/* overload */;
extern PACKAGE float __fastcall NormalizeAngle(float angle);
extern PACKAGE float __fastcall NormalizeDegAngle(float angle);
extern PACKAGE void __fastcall SinCos(const double Theta, /* out */ double &Sin, /* out */ double &Cos)/* overload */;
extern PACKAGE void __fastcall SinCos(const float Theta, /* out */ float &Sin, /* out */ float &Cos)/* overload */;
extern PACKAGE void __fastcall SinCos(const double theta, const double radius, /* out */ double &Sin, /* out */ double &Cos)/* overload */;
extern PACKAGE void __fastcall SinCos(const float theta, const float radius, /* out */ float &Sin, /* out */ float &Cos)/* overload */;
extern PACKAGE void __fastcall PrepareSinCosCache(float *s, const int s_Size, float *c, const int c_Size, float startAngle, float stopAngle);
extern PACKAGE System::Extended __fastcall ArcCos(const System::Extended X)/* overload */;
extern PACKAGE float __fastcall ArcCos(const float x)/* overload */;
extern PACKAGE System::Extended __fastcall ArcSin(const System::Extended X)/* overload */;
extern PACKAGE float __fastcall ArcSin(const float X)/* overload */;
extern PACKAGE System::Extended __fastcall ArcTan2(const System::Extended Y, const System::Extended X)/* overload */;
extern PACKAGE float __fastcall ArcTan2(const float Y, const float X)/* overload */;
extern PACKAGE float __fastcall FastArcTan2(float y, float x);
extern PACKAGE System::Extended __fastcall Tan(const System::Extended X)/* overload */;
extern PACKAGE float __fastcall Tan(const float X)/* overload */;
extern PACKAGE System::Extended __fastcall CoTan(const System::Extended X)/* overload */;
extern PACKAGE float __fastcall CoTan(const float X)/* overload */;
extern PACKAGE float __fastcall Sinh(const float x)/* overload */;
extern PACKAGE double __fastcall Sinh(const double x)/* overload */;
extern PACKAGE float __fastcall Cosh(const float x)/* overload */;
extern PACKAGE double __fastcall Cosh(const double x)/* overload */;
extern PACKAGE float __fastcall RSqrt(float v);
extern PACKAGE int __fastcall ISqrt(int i);
extern PACKAGE int __fastcall ILength(int x, int y)/* overload */;
extern PACKAGE int __fastcall ILength(int x, int y, int z)/* overload */;
extern PACKAGE float __fastcall RLength(float x, float y);
extern PACKAGE void __fastcall RegisterBasedExp(void);
extern PACKAGE void __fastcall RandomPointOnSphere(Vectortypes::TVector3f &p);
extern PACKAGE float __fastcall RoundInt(float v)/* overload */;
extern PACKAGE System::Extended __fastcall RoundInt(System::Extended v)/* overload */;
extern PACKAGE __int64 __fastcall Trunc64(System::Extended v)/* overload */;
extern PACKAGE int __fastcall Trunc(float v)/* overload */;
extern PACKAGE System::Extended __fastcall Int(System::Extended v)/* overload */;
extern PACKAGE float __fastcall Int(float v)/* overload */;
extern PACKAGE System::Extended __fastcall Frac(System::Extended v)/* overload */;
extern PACKAGE float __fastcall Frac(float v)/* overload */;
extern PACKAGE __int64 __fastcall Round64(float v)/* overload */;
extern PACKAGE __int64 __fastcall Round64(System::Extended v)/* overload */;
extern PACKAGE int __fastcall Round(float v)/* overload */;
extern PACKAGE __int64 __fastcall Ceil64(System::Extended v)/* overload */;
extern PACKAGE int __fastcall Ceil(float v)/* overload */;
extern PACKAGE __int64 __fastcall Floor64(System::Extended v)/* overload */;
extern PACKAGE int __fastcall Floor(float v)/* overload */;
extern PACKAGE int __fastcall Sign(float x);
extern PACKAGE int __fastcall SignStrict(float x);
extern PACKAGE int __fastcall ScaleAndRound(int i, float &s);
extern PACKAGE bool __fastcall IsInRange(const float x, const float a, const float b)/* overload */;
extern PACKAGE bool __fastcall IsInRange(const double x, const double a, const double b)/* overload */;
extern PACKAGE bool __fastcall IsInCube(const Vectortypes::TVector3f &p, const Vectortypes::TVector3f &d)/* overload */;
extern PACKAGE bool __fastcall IsInCube(const Vectortypes::TVector4f &p, const Vectortypes::TVector4f &d)/* overload */;
extern PACKAGE float __fastcall MinFloat(PFloatVector values, int nbItems)/* overload */;
extern PACKAGE double __fastcall MinFloat(PDoubleVector values, int nbItems)/* overload */;
extern PACKAGE System::Extended __fastcall MinFloat(PExtendedVector values, int nbItems)/* overload */;
extern PACKAGE float __fastcall MinFloat(float const *v, const int v_Size)/* overload */;
extern PACKAGE float __fastcall MinFloat(const float v1, const float v2)/* overload */;
extern PACKAGE double __fastcall MinFloat(const double v1, const double v2)/* overload */;
extern PACKAGE float __fastcall MinFloat(const float v1, const float v2, const float v3)/* overload */;
extern PACKAGE double __fastcall MinFloat(const double v1, const double v2, const double v3)/* overload */;
extern PACKAGE float __fastcall MaxFloat(PFloatVector values, int nbItems)/* overload */;
extern PACKAGE double __fastcall MaxFloat(PDoubleVector values, int nbItems)/* overload */;
extern PACKAGE System::Extended __fastcall MaxFloat(PExtendedVector values, int nbItems)/* overload */;
extern PACKAGE float __fastcall MaxFloat(float const *v, const int v_Size)/* overload */;
extern PACKAGE float __fastcall MaxFloat(const float v1, const float v2)/* overload */;
extern PACKAGE double __fastcall MaxFloat(const double v1, const double v2)/* overload */;
extern PACKAGE float __fastcall MaxFloat(const float v1, const float v2, const float v3)/* overload */;
extern PACKAGE double __fastcall MaxFloat(const double v1, const double v2, const double v3)/* overload */;
extern PACKAGE int __fastcall MinInteger(const int v1, const int v2)/* overload */;
extern PACKAGE unsigned __fastcall MinInteger(const unsigned v1, const unsigned v2)/* overload */;
extern PACKAGE int __fastcall MinInteger(const int v1, const int v2, const int v3)/* overload */;
extern PACKAGE unsigned __fastcall MinInteger(const unsigned v1, const unsigned v2, const unsigned v3)/* overload */;
extern PACKAGE int __fastcall MaxInteger(const int v1, const int v2)/* overload */;
extern PACKAGE unsigned __fastcall MaxInteger(const unsigned v1, const unsigned v2)/* overload */;
extern PACKAGE int __fastcall MaxInteger(const int v1, const int v2, const int v3)/* overload */;
extern PACKAGE unsigned __fastcall MaxInteger(const unsigned v1, const unsigned v2, const unsigned v3)/* overload */;
extern PACKAGE int __fastcall ClampInteger(const int value, const int min, const int max)/* overload */;
extern PACKAGE unsigned __fastcall ClampInteger(const unsigned value, const unsigned min, const unsigned max)/* overload */;
extern PACKAGE float __fastcall TriangleArea(const Vectortypes::TVector3f &p1, const Vectortypes::TVector3f &p2, const Vectortypes::TVector3f &p3)/* overload */;
extern PACKAGE float __fastcall PolygonArea(const PAffineVectorArray p, int nSides)/* overload */;
extern PACKAGE float __fastcall TriangleSignedArea(const Vectortypes::TVector3f &p1, const Vectortypes::TVector3f &p2, const Vectortypes::TVector3f &p3)/* overload */;
extern PACKAGE float __fastcall PolygonSignedArea(const PAffineVectorArray p, int nSides)/* overload */;
extern PACKAGE void __fastcall ScaleFloatArray(PFloatVector values, int nb, float &factor)/* overload */;
extern PACKAGE void __fastcall ScaleFloatArray(TSingleArray &values, float factor)/* overload */;
extern PACKAGE void __fastcall OffsetFloatArray(PFloatVector values, int nb, float &delta)/* overload */;
extern PACKAGE void __fastcall OffsetFloatArray(float *values, const int values_Size, float delta)/* overload */;
extern PACKAGE void __fastcall OffsetFloatArray(PFloatVector valuesDest, PFloatVector valuesDelta, int nb)/* overload */;
extern PACKAGE float __fastcall MaxXYZComponent(const Vectortypes::TVector4f &v)/* overload */;
extern PACKAGE float __fastcall MaxXYZComponent(const Vectortypes::TVector3f &v)/* overload */;
extern PACKAGE float __fastcall MinXYZComponent(const Vectortypes::TVector4f &v)/* overload */;
extern PACKAGE float __fastcall MinXYZComponent(const Vectortypes::TVector3f &v)/* overload */;
extern PACKAGE float __fastcall MaxAbsXYZComponent(const Vectortypes::TVector4f &v);
extern PACKAGE float __fastcall MinAbsXYZComponent(const Vectortypes::TVector4f &v);
extern PACKAGE void __fastcall MaxVector(Vectortypes::TVector4f &v, const Vectortypes::TVector4f &v1)/* overload */;
extern PACKAGE void __fastcall MaxVector(Vectortypes::TVector3f &v, const Vectortypes::TVector3f &v1)/* overload */;
extern PACKAGE void __fastcall MinVector(Vectortypes::TVector4f &v, const Vectortypes::TVector4f &v1)/* overload */;
extern PACKAGE void __fastcall MinVector(Vectortypes::TVector3f &v, const Vectortypes::TVector3f &v1)/* overload */;
extern PACKAGE void __fastcall SortArrayAscending(System::Extended *a, const int a_Size);
extern PACKAGE float __fastcall ClampValue(const float aValue, const float aMin, const float aMax)/* overload */;
extern PACKAGE float __fastcall ClampValue(const float aValue, const float aMin)/* overload */;
extern PACKAGE Vectortypes::TVector3d __fastcall MakeAffineDblVector(double *v, const int v_Size);
extern PACKAGE Vectortypes::TVector4d __fastcall MakeDblVector(double *v, const int v_Size);
extern PACKAGE bool __fastcall PointInPolygon(float *xp, const int xp_Size, float *yp, const int yp_Size, float x, float y);
extern PACKAGE bool __fastcall IsPointInPolygon(System::Types::TPoint *Polygon, const int Polygon_Size, const System::Types::TPoint &p);
extern PACKAGE void __fastcall DivMod(int Dividend, System::Word Divisor, System::Word &Result, System::Word &Remainder);
extern PACKAGE Vectortypes::TVector4f __fastcall ConvertRotation(const Vectortypes::TVector3f &Angles);
extern PACKAGE TQuaternion __fastcall QuaternionSlerp(const TQuaternion &QStart, const TQuaternion &QEnd, int Spin, float t)/* overload */;
extern PACKAGE TQuaternion __fastcall QuaternionSlerp(const TQuaternion &source, const TQuaternion &dest, const float t)/* overload */;
extern PACKAGE Vectortypes::TVector4f __fastcall VectorDblToFlt(const Vectortypes::TVector4d &v);
extern PACKAGE Vectortypes::TVector3f __fastcall VectorAffineDblToFlt(const Vectortypes::TVector3d &v);
extern PACKAGE Vectortypes::TVector3d __fastcall VectorAffineFltToDbl(const Vectortypes::TVector3f &v);
extern PACKAGE Vectortypes::TVector4d __fastcall VectorFltToDbl(const Vectortypes::TVector4f &v);
extern PACKAGE Vectortypes::TMatrix4f __fastcall Turn(const Vectortypes::TMatrix4f &Matrix, float angle)/* overload */;
extern PACKAGE Vectortypes::TMatrix4f __fastcall Turn(const Vectortypes::TMatrix4f &Matrix, const Vectortypes::TVector3f &MasterUp, float Angle)/* overload */;
extern PACKAGE Vectortypes::TMatrix4f __fastcall Pitch(const Vectortypes::TMatrix4f &Matrix, float Angle)/* overload */;
extern PACKAGE Vectortypes::TMatrix4f __fastcall Pitch(const Vectortypes::TMatrix4f &Matrix, const Vectortypes::TVector3f &MasterRight, float Angle)/* overload */;
extern PACKAGE Vectortypes::TMatrix4f __fastcall Roll(const Vectortypes::TMatrix4f &Matrix, float Angle)/* overload */;
extern PACKAGE Vectortypes::TMatrix4f __fastcall Roll(const Vectortypes::TMatrix4f &Matrix, const Vectortypes::TVector3f &MasterDirection, float Angle)/* overload */;
extern PACKAGE bool __fastcall RayCastPlaneIntersect(const Vectortypes::TVector4f &rayStart, const Vectortypes::TVector4f &rayVector, const Vectortypes::TVector4f &planePoint, const Vectortypes::TVector4f &planeNormal, PVector intersectPoint = (PVector)(0x0))/* overload */;
extern PACKAGE bool __fastcall RayCastPlaneXZIntersect(const Vectortypes::TVector4f &rayStart, const Vectortypes::TVector4f &rayVector, const float planeY, PVector intersectPoint = (PVector)(0x0))/* overload */;
extern PACKAGE bool __fastcall RayCastTriangleIntersect(const Vectortypes::TVector4f &rayStart, const Vectortypes::TVector4f &rayVector, const Vectortypes::TVector3f &p1, const Vectortypes::TVector3f &p2, const Vectortypes::TVector3f &p3, PVector intersectPoint = (PVector)(0x0), PVector intersectNormal = (PVector)(0x0))/* overload */;
extern PACKAGE float __fastcall RayCastMinDistToPoint(const Vectortypes::TVector4f &rayStart, const Vectortypes::TVector4f &rayVector, const Vectortypes::TVector4f &point);
extern PACKAGE bool __fastcall RayCastIntersectsSphere(const Vectortypes::TVector4f &rayStart, const Vectortypes::TVector4f &rayVector, const Vectortypes::TVector4f &sphereCenter, const float sphereRadius)/* overload */;
extern PACKAGE int __fastcall RayCastSphereIntersect(const Vectortypes::TVector4f &rayStart, const Vectortypes::TVector4f &rayVector, const Vectortypes::TVector4f &sphereCenter, const float sphereRadius, Vectortypes::TVector4f &i1, Vectortypes::TVector4f &i2)/* overload */;
extern PACKAGE bool __fastcall RayCastBoxIntersect(const Vectortypes::TVector3f &rayStart, const Vectortypes::TVector3f &rayVector, const Vectortypes::TVector3f &aMinExtent, const Vectortypes::TVector3f &aMaxExtent, PAffineVector intersectPoint = (PAffineVector)(0x0));
extern PACKAGE float __fastcall SphereVisibleRadius(float distance, float radius);
extern PACKAGE int __fastcall IntersectLinePlane(const Vectortypes::TVector4f &point, const Vectortypes::TVector4f &direction, const Vectortypes::TVector4f &plane, PVector intersectPoint = (PVector)(0x0))/* overload */;
extern PACKAGE bool __fastcall IntersectTriangleBox(const Vectortypes::TVector3f &p1, const Vectortypes::TVector3f &p2, const Vectortypes::TVector3f &p3, const Vectortypes::TVector3f &aMinExtent, const Vectortypes::TVector3f &aMaxExtent);
extern PACKAGE bool __fastcall IntersectSphereBox(const Vectortypes::TVector4f &SpherePos, const float SphereRadius, const Vectortypes::TMatrix4f &BoxMatrix, const Vectortypes::TVector3f &BoxScale, PAffineVector intersectPoint = (PAffineVector)(0x0), PAffineVector normal = (PAffineVector)(0x0), System::PSingle depth = (System::PSingle)(0x0));
extern PACKAGE TFrustum __fastcall ExtractFrustumFromModelViewProjection(const Vectortypes::TMatrix4f &modelViewProj);
extern PACKAGE bool __fastcall IsVolumeClipped(const Vectortypes::TVector3f &objPos, const float objRadius, const TFrustum &Frustum)/* overload */;
extern PACKAGE bool __fastcall IsVolumeClipped(const Vectortypes::TVector4f &objPos, const float objRadius, const TFrustum &Frustum)/* overload */;
extern PACKAGE bool __fastcall IsVolumeClipped(const Vectortypes::TVector3f &min, const Vectortypes::TVector3f &max, const TFrustum &Frustum)/* overload */;
extern PACKAGE Vectortypes::TMatrix4f __fastcall MakeParallelProjectionMatrix(const Vectortypes::TVector4f &plane, const Vectortypes::TVector4f &dir);
extern PACKAGE Vectortypes::TMatrix4f __fastcall MakeShadowMatrix(const Vectortypes::TVector4f &planePoint, const Vectortypes::TVector4f &planeNormal, const Vectortypes::TVector4f &lightPos);
extern PACKAGE Vectortypes::TMatrix4f __fastcall MakeReflectionMatrix(const Vectortypes::TVector3f &planePoint, const Vectortypes::TVector3f &planeNormal);
extern PACKAGE TPackedRotationMatrix __fastcall PackRotationMatrix(const Vectortypes::TMatrix4f &mat);
extern PACKAGE Vectortypes::TMatrix4f __fastcall UnPackRotationMatrix(short const *packedMatrix);
extern PACKAGE bool __fastcall BarycentricCoordinates(const Vectortypes::TVector3f &v1, const Vectortypes::TVector3f &v2, const Vectortypes::TVector3f &v3, const Vectortypes::TVector3f &p, float &u, float &v);
extern PACKAGE Vectortypes::TVector2f __fastcall Vector2fMake(const float X, const float Y)/* overload */;
extern PACKAGE Vectortypes::TVector2i __fastcall Vector2iMake(const int X, const int Y)/* overload */;
extern PACKAGE Vectortypes::TVector2s __fastcall Vector2sMake(const short X, const short Y)/* overload */;
extern PACKAGE Vectortypes::TVector2d __fastcall Vector2dMake(const double X, const double Y)/* overload */;
extern PACKAGE Vectortypes::TVector2b __fastcall Vector2bMake(const System::Byte X, const System::Byte Y)/* overload */;
extern PACKAGE Vectortypes::TVector2f __fastcall Vector2fMake(const Vectortypes::TVector3f &Vector)/* overload */;
extern PACKAGE Vectortypes::TVector2i __fastcall Vector2iMake(const Vectortypes::TVector3i &Vector)/* overload */;
extern PACKAGE Vectortypes::TVector2s __fastcall Vector2sMake(const Vectortypes::TVector3s &Vector)/* overload */;
extern PACKAGE Vectortypes::TVector2d __fastcall Vector2dMake(const Vectortypes::TVector3d &Vector)/* overload */;
extern PACKAGE Vectortypes::TVector2b __fastcall Vector2bMake(const Vectortypes::TVector3b Vector)/* overload */;
extern PACKAGE Vectortypes::TVector2f __fastcall Vector2fMake(const Vectortypes::TVector4f &Vector)/* overload */;
extern PACKAGE Vectortypes::TVector2i __fastcall Vector2iMake(const Vectortypes::TVector4i &Vector)/* overload */;
extern PACKAGE Vectortypes::TVector2s __fastcall Vector2sMake(const Vectortypes::TVector4s &Vector)/* overload */;
extern PACKAGE Vectortypes::TVector2d __fastcall Vector2dMake(const Vectortypes::TVector4d &Vector)/* overload */;
extern PACKAGE Vectortypes::TVector2b __fastcall Vector2bMake(const Vectortypes::TVector4b Vector)/* overload */;
extern PACKAGE Vectortypes::TVector3f __fastcall Vector3fMake(const float X, const float Y = 0.000000E+00, const float Z = 0.000000E+00)/* overload */;
extern PACKAGE Vectortypes::TVector3i __fastcall Vector3iMake(const int X, const int Y = 0x0, const int Z = 0x0)/* overload */;
extern PACKAGE Vectortypes::TVector3s __fastcall Vector3sMake(const short X, const short Y = (short)(0x0), const short Z = (short)(0x0))/* overload */;
extern PACKAGE Vectortypes::TVector3d __fastcall Vector3dMake(const double X, const double Y = 0.000000E+00, const double Z = 0.000000E+00)/* overload */;
extern PACKAGE Vectortypes::TVector3b __fastcall Vector3bMake(const System::Byte X, const System::Byte Y = (System::Byte)(0x0), const System::Byte Z = (System::Byte)(0x0))/* overload */;
extern PACKAGE Vectortypes::TVector3f __fastcall Vector3fMake(const Vectortypes::TVector2f &Vector, const float Z = 0.000000E+00)/* overload */;
extern PACKAGE Vectortypes::TVector3i __fastcall Vector3iMake(const Vectortypes::TVector2i &Vector, const int Z = 0x0)/* overload */;
extern PACKAGE Vectortypes::TVector3s __fastcall Vector3sMake(const Vectortypes::TVector2s Vector, const short Z = (short)(0x0))/* overload */;
extern PACKAGE Vectortypes::TVector3d __fastcall Vector3dMake(const Vectortypes::TVector2d &Vector, const double Z = 0.000000E+00)/* overload */;
extern PACKAGE Vectortypes::TVector3b __fastcall Vector3bMake(const Vectortypes::TVector2b Vector, const System::Byte Z = (System::Byte)(0x0))/* overload */;
extern PACKAGE Vectortypes::TVector3f __fastcall Vector3fMake(const Vectortypes::TVector4f &Vector)/* overload */;
extern PACKAGE Vectortypes::TVector3i __fastcall Vector3iMake(const Vectortypes::TVector4i &Vector)/* overload */;
extern PACKAGE Vectortypes::TVector3s __fastcall Vector3sMake(const Vectortypes::TVector4s &Vector)/* overload */;
extern PACKAGE Vectortypes::TVector3d __fastcall Vector3dMake(const Vectortypes::TVector4d &Vector)/* overload */;
extern PACKAGE Vectortypes::TVector3b __fastcall Vector3bMake(const Vectortypes::TVector4b Vector)/* overload */;
extern PACKAGE Vectortypes::TVector4f __fastcall Vector4fMake(const float X, const float Y = 0.000000E+00, const float Z = 0.000000E+00, const float W = 0.000000E+00)/* overload */;
extern PACKAGE Vectortypes::TVector4i __fastcall Vector4iMake(const int X, const int Y = 0x0, const int Z = 0x0, const int W = 0x0)/* overload */;
extern PACKAGE Vectortypes::TVector4s __fastcall Vector4sMake(const short X, const short Y = (short)(0x0), const short Z = (short)(0x0), const short W = (short)(0x0))/* overload */;
extern PACKAGE Vectortypes::TVector4d __fastcall Vector4dMake(const double X, const double Y = 0.000000E+00, const double Z = 0.000000E+00, const double W = 0.000000E+00)/* overload */;
extern PACKAGE Vectortypes::TVector4b __fastcall Vector4bMake(const System::Byte X, const System::Byte Y = (System::Byte)(0x0), const System::Byte Z = (System::Byte)(0x0), const System::Byte W = (System::Byte)(0x0))/* overload */;
extern PACKAGE Vectortypes::TVector4f __fastcall Vector4fMake(const Vectortypes::TVector3f &Vector, const float W = 0.000000E+00)/* overload */;
extern PACKAGE Vectortypes::TVector4i __fastcall Vector4iMake(const Vectortypes::TVector3i &Vector, const int W = 0x0)/* overload */;
extern PACKAGE Vectortypes::TVector4s __fastcall Vector4sMake(const Vectortypes::TVector3s &Vector, const short W = (short)(0x0))/* overload */;
extern PACKAGE Vectortypes::TVector4d __fastcall Vector4dMake(const Vectortypes::TVector3d &Vector, const double W = 0.000000E+00)/* overload */;
extern PACKAGE Vectortypes::TVector4b __fastcall Vector4bMake(const Vectortypes::TVector3b Vector, const System::Byte W = (System::Byte)(0x0))/* overload */;
extern PACKAGE Vectortypes::TVector4f __fastcall Vector4fMake(const Vectortypes::TVector2f &Vector, const float Z = 0.000000E+00, const float W = 0.000000E+00)/* overload */;
extern PACKAGE Vectortypes::TVector4i __fastcall Vector4iMake(const Vectortypes::TVector2i &Vector, const int Z = 0x0, const int W = 0x0)/* overload */;
extern PACKAGE Vectortypes::TVector4s __fastcall Vector4sMake(const Vectortypes::TVector2s Vector, const short Z = (short)(0x0), const short W = (short)(0x0))/* overload */;
extern PACKAGE Vectortypes::TVector4d __fastcall Vector4dMake(const Vectortypes::TVector2d &Vector, const double Z = 0.000000E+00, const double W = 0.000000E+00)/* overload */;
extern PACKAGE Vectortypes::TVector4b __fastcall Vector4bMake(const Vectortypes::TVector2b Vector, const System::Byte Z = (System::Byte)(0x0), const System::Byte W = (System::Byte)(0x0))/* overload */;
extern PACKAGE bool __fastcall VectorEquals(const Vectortypes::TVector2f &Vector1, const Vectortypes::TVector2f &Vector2)/* overload */;
extern PACKAGE bool __fastcall VectorEquals(const Vectortypes::TVector2i &Vector1, const Vectortypes::TVector2i &Vector2)/* overload */;
extern PACKAGE bool __fastcall VectorEquals(const Vectortypes::TVector2d &V1, const Vectortypes::TVector2d &V2)/* overload */;
extern PACKAGE bool __fastcall VectorEquals(const Vectortypes::TVector2s V1, const Vectortypes::TVector2s V2)/* overload */;
extern PACKAGE bool __fastcall VectorEquals(const Vectortypes::TVector2b V1, const Vectortypes::TVector2b V2)/* overload */;
extern PACKAGE bool __fastcall VectorEquals(const Vectortypes::TVector3i &V1, const Vectortypes::TVector3i &V2)/* overload */;
extern PACKAGE bool __fastcall VectorEquals(const Vectortypes::TVector3d &V1, const Vectortypes::TVector3d &V2)/* overload */;
extern PACKAGE bool __fastcall VectorEquals(const Vectortypes::TVector3s &V1, const Vectortypes::TVector3s &V2)/* overload */;
extern PACKAGE bool __fastcall VectorEquals(const Vectortypes::TVector3b V1, const Vectortypes::TVector3b V2)/* overload */;
extern PACKAGE bool __fastcall VectorEquals(const Vectortypes::TVector4i &V1, const Vectortypes::TVector4i &V2)/* overload */;
extern PACKAGE bool __fastcall VectorEquals(const Vectortypes::TVector4d &V1, const Vectortypes::TVector4d &V2)/* overload */;
extern PACKAGE bool __fastcall VectorEquals(const Vectortypes::TVector4s &V1, const Vectortypes::TVector4s &V2)/* overload */;
extern PACKAGE bool __fastcall VectorEquals(const Vectortypes::TVector4b V1, const Vectortypes::TVector4b V2)/* overload */;
extern PACKAGE bool __fastcall MatrixEquals(const Vectortypes::TMatrix3f &Matrix1, const Vectortypes::TMatrix3f &Matrix2)/* overload */;
extern PACKAGE bool __fastcall MatrixEquals(const Vectortypes::TMatrix3i &Matrix1, const Vectortypes::TMatrix3i &Matrix2)/* overload */;
extern PACKAGE bool __fastcall MatrixEquals(const Vectortypes::TMatrix3d &Matrix1, const Vectortypes::TMatrix3d &Matrix2)/* overload */;
extern PACKAGE bool __fastcall MatrixEquals(const Vectortypes::TMatrix3s &Matrix1, const Vectortypes::TMatrix3s &Matrix2)/* overload */;
extern PACKAGE bool __fastcall MatrixEquals(const Vectortypes::TMatrix3b &Matrix1, const Vectortypes::TMatrix3b &Matrix2)/* overload */;
extern PACKAGE bool __fastcall MatrixEquals(const Vectortypes::TMatrix4f &Matrix1, const Vectortypes::TMatrix4f &Matrix2)/* overload */;
extern PACKAGE bool __fastcall MatrixEquals(const Vectortypes::TMatrix4i &Matrix1, const Vectortypes::TMatrix4i &Matrix2)/* overload */;
extern PACKAGE bool __fastcall MatrixEquals(const Vectortypes::TMatrix4d &Matrix1, const Vectortypes::TMatrix4d &Matrix2)/* overload */;
extern PACKAGE bool __fastcall MatrixEquals(const Vectortypes::TMatrix4s &Matrix1, const Vectortypes::TMatrix4s &Matrix2)/* overload */;
extern PACKAGE bool __fastcall MatrixEquals(const Vectortypes::TMatrix4b &Matrix1, const Vectortypes::TMatrix4b &Matrix2)/* overload */;
extern PACKAGE bool __fastcall VectorMoreThen(const Vectortypes::TVector3f &SourceVector, const Vectortypes::TVector3f &ComparedVector)/* overload */;
extern PACKAGE bool __fastcall VectorMoreEqualThen(const Vectortypes::TVector3f &SourceVector, const Vectortypes::TVector3f &ComparedVector)/* overload */;
extern PACKAGE bool __fastcall VectorLessThen(const Vectortypes::TVector3f &SourceVector, const Vectortypes::TVector3f &ComparedVector)/* overload */;
extern PACKAGE bool __fastcall VectorLessEqualThen(const Vectortypes::TVector3f &SourceVector, const Vectortypes::TVector3f &ComparedVector)/* overload */;
extern PACKAGE bool __fastcall VectorMoreThen(const Vectortypes::TVector4f &SourceVector, const Vectortypes::TVector4f &ComparedVector)/* overload */;
extern PACKAGE bool __fastcall VectorMoreEqualThen(const Vectortypes::TVector4f &SourceVector, const Vectortypes::TVector4f &ComparedVector)/* overload */;
extern PACKAGE bool __fastcall VectorLessThen(const Vectortypes::TVector4f &SourceVector, const Vectortypes::TVector4f &ComparedVector)/* overload */;
extern PACKAGE bool __fastcall VectorLessEqualThen(const Vectortypes::TVector4f &SourceVector, const Vectortypes::TVector4f &ComparedVector)/* overload */;
extern PACKAGE bool __fastcall VectorMoreThen(const Vectortypes::TVector3i &SourceVector, const Vectortypes::TVector3i &ComparedVector)/* overload */;
extern PACKAGE bool __fastcall VectorMoreEqualThen(const Vectortypes::TVector3i &SourceVector, const Vectortypes::TVector3i &ComparedVector)/* overload */;
extern PACKAGE bool __fastcall VectorLessThen(const Vectortypes::TVector3i &SourceVector, const Vectortypes::TVector3i &ComparedVector)/* overload */;
extern PACKAGE bool __fastcall VectorLessEqualThen(const Vectortypes::TVector3i &SourceVector, const Vectortypes::TVector3i &ComparedVector)/* overload */;
extern PACKAGE bool __fastcall VectorMoreThen(const Vectortypes::TVector4i &SourceVector, const Vectortypes::TVector4i &ComparedVector)/* overload */;
extern PACKAGE bool __fastcall VectorMoreEqualThen(const Vectortypes::TVector4i &SourceVector, const Vectortypes::TVector4i &ComparedVector)/* overload */;
extern PACKAGE bool __fastcall VectorLessThen(const Vectortypes::TVector4i &SourceVector, const Vectortypes::TVector4i &ComparedVector)/* overload */;
extern PACKAGE bool __fastcall VectorLessEqualThen(const Vectortypes::TVector4i &SourceVector, const Vectortypes::TVector4i &ComparedVector)/* overload */;
extern PACKAGE bool __fastcall VectorMoreThen(const Vectortypes::TVector3s &SourceVector, const Vectortypes::TVector3s &ComparedVector)/* overload */;
extern PACKAGE bool __fastcall VectorMoreEqualThen(const Vectortypes::TVector3s &SourceVector, const Vectortypes::TVector3s &ComparedVector)/* overload */;
extern PACKAGE bool __fastcall VectorLessThen(const Vectortypes::TVector3s &SourceVector, const Vectortypes::TVector3s &ComparedVector)/* overload */;
extern PACKAGE bool __fastcall VectorLessEqualThen(const Vectortypes::TVector3s &SourceVector, const Vectortypes::TVector3s &ComparedVector)/* overload */;
extern PACKAGE bool __fastcall VectorMoreThen(const Vectortypes::TVector4s &SourceVector, const Vectortypes::TVector4s &ComparedVector)/* overload */;
extern PACKAGE bool __fastcall VectorMoreEqualThen(const Vectortypes::TVector4s &SourceVector, const Vectortypes::TVector4s &ComparedVector)/* overload */;
extern PACKAGE bool __fastcall VectorLessThen(const Vectortypes::TVector4s &SourceVector, const Vectortypes::TVector4s &ComparedVector)/* overload */;
extern PACKAGE bool __fastcall VectorLessEqualThen(const Vectortypes::TVector4s &SourceVector, const Vectortypes::TVector4s &ComparedVector)/* overload */;
extern PACKAGE bool __fastcall VectorMoreThen(const Vectortypes::TVector3f &SourceVector, const float ComparedNumber)/* overload */;
extern PACKAGE bool __fastcall VectorMoreEqualThen(const Vectortypes::TVector3f &SourceVector, const float ComparedNumber)/* overload */;
extern PACKAGE bool __fastcall VectorLessThen(const Vectortypes::TVector3f &SourceVector, const float ComparedNumber)/* overload */;
extern PACKAGE bool __fastcall VectorLessEqualThen(const Vectortypes::TVector3f &SourceVector, const float ComparedNumber)/* overload */;
extern PACKAGE bool __fastcall VectorMoreThen(const Vectortypes::TVector4f &SourceVector, const float ComparedNumber)/* overload */;
extern PACKAGE bool __fastcall VectorMoreEqualThen(const Vectortypes::TVector4f &SourceVector, const float ComparedNumber)/* overload */;
extern PACKAGE bool __fastcall VectorLessThen(const Vectortypes::TVector4f &SourceVector, const float ComparedNumber)/* overload */;
extern PACKAGE bool __fastcall VectorLessEqualThen(const Vectortypes::TVector4f &SourceVector, const float ComparedNumber)/* overload */;
extern PACKAGE bool __fastcall VectorMoreThen(const Vectortypes::TVector3i &SourceVector, const float ComparedNumber)/* overload */;
extern PACKAGE bool __fastcall VectorMoreEqualThen(const Vectortypes::TVector3i &SourceVector, const float ComparedNumber)/* overload */;
extern PACKAGE bool __fastcall VectorLessThen(const Vectortypes::TVector3i &SourceVector, const float ComparedNumber)/* overload */;
extern PACKAGE bool __fastcall VectorLessEqualThen(const Vectortypes::TVector3i &SourceVector, const float ComparedNumber)/* overload */;
extern PACKAGE bool __fastcall VectorMoreThen(const Vectortypes::TVector4i &SourceVector, const float ComparedNumber)/* overload */;
extern PACKAGE bool __fastcall VectorMoreEqualThen(const Vectortypes::TVector4i &SourceVector, const float ComparedNumber)/* overload */;
extern PACKAGE bool __fastcall VectorLessThen(const Vectortypes::TVector4i &SourceVector, const float ComparedNumber)/* overload */;
extern PACKAGE bool __fastcall VectorLessEqualThen(const Vectortypes::TVector4i &SourceVector, const float ComparedNumber)/* overload */;
extern PACKAGE bool __fastcall VectorMoreThen(const Vectortypes::TVector3s &SourceVector, const float ComparedNumber)/* overload */;
extern PACKAGE bool __fastcall VectorMoreEqualThen(const Vectortypes::TVector3s &SourceVector, const float ComparedNumber)/* overload */;
extern PACKAGE bool __fastcall VectorLessThen(const Vectortypes::TVector3s &SourceVector, const float ComparedNumber)/* overload */;
extern PACKAGE bool __fastcall VectorLessEqualThen(const Vectortypes::TVector3s &SourceVector, const float ComparedNumber)/* overload */;
extern PACKAGE bool __fastcall VectorMoreThen(const Vectortypes::TVector4s &SourceVector, const float ComparedNumber)/* overload */;
extern PACKAGE bool __fastcall VectorMoreEqualThen(const Vectortypes::TVector4s &SourceVector, const float ComparedNumber)/* overload */;
extern PACKAGE bool __fastcall VectorLessThen(const Vectortypes::TVector4s &SourceVector, const float ComparedNumber)/* overload */;
extern PACKAGE bool __fastcall VectorLessEqualThen(const Vectortypes::TVector4s &SourceVector, const float ComparedNumber)/* overload */;
extern PACKAGE bool __fastcall RectanglesIntersect(const Vectortypes::TVector2f &ACenterOfRect1, const Vectortypes::TVector2f &ACenterOfRect2, const Vectortypes::TVector2f &ASizeOfRect1, const Vectortypes::TVector2f &ASizeOfRect2);
extern PACKAGE bool __fastcall RectangleContains(const Vectortypes::TVector2f &ACenterOfBigRect1, const Vectortypes::TVector2f &ACenterOfSmallRect2, const Vectortypes::TVector2f &ASizeOfBigRect1, const Vectortypes::TVector2f &ASizeOfSmallRect2, const float AEps = 0.000000E+00);
extern PACKAGE Vectortypes::TVector2f __fastcall GetSafeTurnAngle(const Vectortypes::TVector4f &AOriginalPosition, const Vectortypes::TVector4f &AOriginalUpVector, const Vectortypes::TVector4f &ATargetPosition, const Vectortypes::TVector4f &AMoveAroundTargetCenter)/* overload */;
extern PACKAGE Vectortypes::TVector2f __fastcall GetSafeTurnAngle(const Vectortypes::TVector3f &AOriginalPosition, const Vectortypes::TVector3f &AOriginalUpVector, const Vectortypes::TVector3f &ATargetPosition, const Vectortypes::TVector3f &AMoveAroundTargetCenter)/* overload */;
extern PACKAGE Vectortypes::TVector4f __fastcall MoveObjectAround(const Vectortypes::TVector4f &AMovingObjectPosition, const Vectortypes::TVector4f &AMovingObjectUp, const Vectortypes::TVector4f &ATargetPosition, float pitchDelta, float turnDelta);
extern PACKAGE float __fastcall AngleBetweenVectors(const Vectortypes::TVector4f &A, const Vectortypes::TVector4f &B, const Vectortypes::TVector4f &ACenterPoint)/* overload */;
extern PACKAGE float __fastcall AngleBetweenVectors(const Vectortypes::TVector3f &A, const Vectortypes::TVector3f &B, const Vectortypes::TVector3f &ACenterPoint)/* overload */;
extern PACKAGE Vectortypes::TVector4f __fastcall ShiftObjectFromCenter(const Vectortypes::TVector4f &AOriginalPosition, const Vectortypes::TVector4f &ACenter, const float ADistance, const bool AFromCenterSpot)/* overload */;
extern PACKAGE Vectortypes::TVector3f __fastcall ShiftObjectFromCenter(const Vectortypes::TVector3f &AOriginalPosition, const Vectortypes::TVector3f &ACenter, const float ADistance, const bool AFromCenterSpot)/* overload */;
}	/* namespace Vectorgeometry */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_VECTORGEOMETRY)
using namespace Vectorgeometry;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// VectorgeometryHPP
