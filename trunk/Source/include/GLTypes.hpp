// CodeGear C++Builder
// Copyright (c) 1995, 2012 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLTypes.pas' rev: 24.00 (Win32)

#ifndef GltypesHPP
#define GltypesHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Gltypes
{
//-- type declarations -------------------------------------------------------
struct DECLSPEC_DRECORD TGLBox
{
public:
	float ALeft;
	float ATop;
	float ANear;
	float ARight;
	float ABottom;
	float AFar;
};


struct TGLPoint2D;
typedef TGLPoint2D *PGLPoint2D;

struct DECLSPEC_DRECORD TGLPoint2D
{
public:
	float X;
	float Y;
	__fastcall TGLPoint2D(float X, float Y);
	void __fastcall SetPosition(const float X, const float Y);
	TGLPoint2D __fastcall Add(const TGLPoint2D &APoint2D);
	float __fastcall Length(void);
	float __fastcall Distance(const TGLPoint2D &APoint2D);
	void __fastcall Offset(const float ADeltaX, const float ADeltaY);
	TGLPoint2D() {}
};


struct TGLPoint3D;
typedef TGLPoint3D *PGLPoint3D;

struct DECLSPEC_DRECORD TGLPoint3D
{
public:
	float X;
	float Y;
	float Z;
	__fastcall TGLPoint3D(float X, float Y, float Z);
	void __fastcall SetPosition(const float X, const float Y, const float Z);
	TGLPoint3D __fastcall Add(const TGLPoint3D &AGLPoint3D);
	float __fastcall Length(void);
	float __fastcall Distance(const TGLPoint3D &APoint3D);
	void __fastcall Offset(const float ADeltaX, const float ADeltaY, const float ADeltaZ);
	TGLPoint3D() {}
};


typedef System::DynamicArray<TGLPoint2D> TGLPoint2DArray;

typedef System::DynamicArray<TGLPoint3D> TGLPoint3DArray;

typedef TGLPoint2DArray TGLPolygon2D;

typedef TGLPoint3DArray TGLPolygon3D;

typedef System::DynamicArray<TGLPoint3DArray> TGLPolyhedron;

typedef System::StaticArray<float, 2> TGLVector2DType;

typedef System::StaticArray<float, 3> TGLVector3DType;

struct DECLSPEC_DRECORD TGLVector2D
{
private:
	TGLVector2D __fastcall Add(const TGLVector2D &AVector2D);
	float __fastcall Norm(void);
	
public:
	__fastcall TGLVector2D(const float AX, const float AY, const float AW);
	float __fastcall Length(void);
	TGLVector2D() {}
	#pragma pack(push,1)
	union
	{
		struct 
		{
			float X;
			float Y;
			float W;
		};
		struct 
		{
			TGLVector2DType V;
		};
		
	};
	#pragma pack(pop)
};


struct DECLSPEC_DRECORD TGLVector3D
{
private:
	TGLVector3D __fastcall Add(const TGLVector3D &AVector3D);
	float __fastcall Norm(void);
	
public:
	__fastcall TGLVector3D(const float AX, const float AY, const float AZ, const float AW);
	float __fastcall Length(void);
	TGLVector3D() {}
	#pragma pack(push,1)
	union
	{
		struct 
		{
			float X;
			float Y;
			float Z;
			float W;
		};
		struct 
		{
			TGLVector3DType V;
		};
		
	};
	#pragma pack(pop)
};


struct DECLSPEC_DRECORD TGLMatrix2D
{
	#pragma pack(push,1)
	union
	{
		struct 
		{
			float e11;
			float e12;
			float e13;
			float e21;
			float e22;
			float e23;
			float e31;
			float e32;
			float e33;
		};
		struct 
		{
			TGLMatrix2DType M;
		};
		
	};
	#pragma pack(pop)
};


struct DECLSPEC_DRECORD TGLMatrix3D
{
	#pragma pack(push,1)
	union
	{
		struct 
		{
			float e11;
			float e12;
			float e13;
			float e14;
			float e21;
			float e22;
			float e23;
			float e24;
			float e31;
			float e32;
			float e33;
			float e34;
			float e41;
			float e42;
			float e43;
			float e44;
		};
		struct 
		{
			TGLMatrix3DType M;
		};
		
	};
	#pragma pack(pop)
};


typedef System::DynamicArray<TGLMatrix2D> TGLMatrix2DArray;

typedef System::DynamicArray<TGLMatrix3D> TGLMatrix3DArray;

#pragma pack(push,1)
struct DECLSPEC_DRECORD TGLMesh2DVertex
{
public:
	float X;
	float Y;
	float NX;
	float NY;
	float tU;
	float tV;
};
#pragma pack(pop)


#pragma pack(push,1)
struct DECLSPEC_DRECORD TGLMesh3DVertex
{
public:
	float X;
	float Y;
	float Z;
	float NX;
	float NY;
	float NZ;
	float tU;
	float tV;
};
#pragma pack(pop)


typedef System::DynamicArray<TGLMesh2DVertex> TGLMesh2D;

typedef System::DynamicArray<TGLMesh3DVertex> TGLMesh3D;

struct DECLSPEC_DRECORD TGLQuaternion3D
{
public:
	TGLVector3D ImPart;
	float RePart;
};


//-- var, const, procedure ---------------------------------------------------
extern PACKAGE TGLPoint2D ClosedPolygon2D;
extern PACKAGE TGLPoint3D ClosedPolygon3D;
}	/* namespace Gltypes */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLTYPES)
using namespace Gltypes;
#endif

//-- user supplied -----------------------------------------------------------
namespace Gltypes
{
typedef TGLVector2D TGLMatrix2DArray[4];
}	/* namespace Gltypes */
namespace Gltypes
{
typedef TGLVector3D TGLMatrix3DType[4];
}	/* namespace Gltypes */

#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// GltypesHPP
