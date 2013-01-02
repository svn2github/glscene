// CodeGear C++Builder
// Copyright (c) 1995, 2012 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'TypesMS3D.pas' rev: 24.00 (Win32)

#ifndef Typesms3dHPP
#define Typesms3dHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <System.Classes.hpp>	// Pascal unit
#include <VectorTypes.hpp>	// Pascal unit
#include <GLColor.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Typesms3d
{
//-- type declarations -------------------------------------------------------
class DELPHICLASS TMS3DGroup;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TMS3DGroup : public System::TObject
{
	typedef System::TObject inherited;
	
public:
	System::Byte Flags;
	System::StaticArray<char, 32> Name;
	System::Word NumTriangles;
	System::Classes::TList* TriangleIndices;
	System::Int8 MaterialIndex;
	__fastcall TMS3DGroup(void);
	__fastcall virtual ~TMS3DGroup(void);
};

#pragma pack(pop)

#pragma pack(push,1)
struct DECLSPEC_DRECORD TMS3DHeader
{
public:
	System::StaticArray<char, 10> ID;
	int Version;
};
#pragma pack(pop)


#pragma pack(push,1)
struct DECLSPEC_DRECORD TMS3DVertex
{
public:
	System::Byte Flags;
	Vectortypes::TD3DVector Vertex;
	char BoneId;
	System::Byte ReferenceCount;
};
#pragma pack(pop)


typedef System::StaticArray<TMS3DVertex, 8192> TMS3DVertexArray;

typedef TMS3DVertexArray *PMS3DVertexArray;

#pragma pack(push,1)
struct DECLSPEC_DRECORD TMS3DTriangle
{
public:
	System::Word Flags;
	System::StaticArray<System::Word, 3> VertexIndices;
	System::StaticArray<Vectortypes::TD3DVector, 3> VertexNormals;
	System::StaticArray<float, 3> S;
	System::StaticArray<float, 3> T;
	System::Byte SmoothingGroup;
	System::Byte GroupIndex;
};
#pragma pack(pop)


typedef System::StaticArray<TMS3DTriangle, 16384> TMS3DTriangleArray;

typedef TMS3DTriangleArray *PMS3DTriangleArray;

#pragma pack(push,1)
struct DECLSPEC_DRECORD TMS3DMaterial
{
public:
	System::StaticArray<char, 32> Name;
	Vectortypes::TVector4f Ambient;
	Vectortypes::TVector4f Diffuse;
	Vectortypes::TVector4f Specular;
	Vectortypes::TVector4f Emissive;
	float Shininess;
	float Transparency;
	char Mode;
	System::StaticArray<char, 128> Texture;
	System::StaticArray<char, 128> Alphamap;
};
#pragma pack(pop)


#pragma pack(push,1)
struct DECLSPEC_DRECORD TMS3DKeyframeRotation
{
public:
	float Time;
	Vectortypes::TD3DVector Rotation;
};
#pragma pack(pop)


typedef System::StaticArray<TMS3DKeyframeRotation, 5000> TMS3DKeyframeRotationArray;

typedef TMS3DKeyframeRotationArray *PMS3DKeyframeRotationArray;

#pragma pack(push,1)
struct DECLSPEC_DRECORD TMS3DKeyframePosition
{
public:
	float Time;
	Vectortypes::TD3DVector Position;
};
#pragma pack(pop)


typedef System::StaticArray<TMS3DKeyframePosition, 5000> TMS3DKeyframePositionArray;

typedef TMS3DKeyframePositionArray *PMS3DKeyframePositionArray;

#pragma pack(push,1)
struct DECLSPEC_DRECORD TMS3DJointBase
{
public:
	System::Byte Flags;
	System::StaticArray<char, 32> Name;
	System::StaticArray<char, 32> ParentName;
	Vectortypes::TD3DVector Rotation;
	Vectortypes::TD3DVector Position;
	System::Word NumKeyFramesRot;
	System::Word NumKeyFramesTrans;
};
#pragma pack(pop)


#pragma pack(push,1)
struct DECLSPEC_DRECORD TMS3DJoint
{
public:
	TMS3DJointBase Base;
	TMS3DKeyframeRotationArray *KeyFramesRot;
	TMS3DKeyframePositionArray *KeyFramesTrans;
};
#pragma pack(pop)


typedef System::StaticArray<TMS3DJoint, 128> TMS3DJointArray;

typedef TMS3DJointArray *PMS3DJointArray;

#pragma pack(push,1)
struct DECLSPEC_DRECORD TMS3DComment
{
private:
	typedef System::DynamicArray<char> _TMS3DComment__1;
	
	
public:
	int index;
	int commentLength;
	_TMS3DComment__1 comment;
};
#pragma pack(pop)


typedef TMS3DComment *pMS3DComment;

class DELPHICLASS TMS3DCommentList;
#pragma pack(push,1)
class PASCALIMPLEMENTATION TMS3DCommentList : public System::Classes::TList
{
	typedef System::Classes::TList inherited;
	
public:
	__fastcall virtual ~TMS3DCommentList(void);
	pMS3DComment __fastcall NewComment(void);
public:
	/* TObject.Create */ inline __fastcall TMS3DCommentList(void) : System::Classes::TList() { }
	
};

#pragma pack(pop)

#pragma pack(push,1)
struct DECLSPEC_DRECORD TMS3D_vertex_ex_t
{
public:
	System::StaticArray<System::Byte, 3> boneIds;
	System::StaticArray<System::Byte, 3> weights;
	unsigned extra;
	unsigned unknown;
};
#pragma pack(pop)


typedef TMS3D_vertex_ex_t *pMS3D_vertex_ex_t;

class DELPHICLASS TVertexWeightList;
#pragma pack(push,1)
class PASCALIMPLEMENTATION TVertexWeightList : public System::Classes::TList
{
	typedef System::Classes::TList inherited;
	
private:
	int FsubVersion;
	pMS3D_vertex_ex_t __fastcall GetWeight(int idx);
	void __fastcall SetsubVersion(const int Value);
	
public:
	pMS3D_vertex_ex_t __fastcall newWeight(void);
	virtual void __fastcall Clear(void);
	__fastcall virtual ~TVertexWeightList(void);
	__property pMS3D_vertex_ex_t Weight[int idx] = {read=GetWeight};
	__property int subVersion = {read=FsubVersion, write=SetsubVersion, nodefault};
public:
	/* TObject.Create */ inline __fastcall TVertexWeightList(void) : System::Classes::TList() { }
	
};

#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------
static const System::Word MAX_MS3D_VERTICES = System::Word(0x2000);
static const System::Word MAX_MS3D_TRIANGLES = System::Word(0x4000);
static const System::Byte MAX_MS3D_GROUPS = System::Byte(0x80);
static const System::Byte MAX_MS3D_MATERIALS = System::Byte(0x80);
static const System::Byte MAX_MS3D_JOINTS = System::Byte(0x80);
static const System::Word MAX_MS3D_KEYFRAMES = System::Word(0x1388);
}	/* namespace Typesms3d */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_TYPESMS3D)
using namespace Typesms3d;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Typesms3dHPP
