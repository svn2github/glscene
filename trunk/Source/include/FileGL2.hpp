// CodeGear C++Builder
// Copyright (c) 1995, 2012 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'FileGL2.pas' rev: 24.00 (Win32)

#ifndef Filegl2HPP
#define Filegl2HPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <System.Classes.hpp>	// Pascal unit
#include <VectorTypes.hpp>	// Pascal unit
#include <System.SysUtils.hpp>	// Pascal unit
#include <VectorGeometry.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Filegl2
{
//-- type declarations -------------------------------------------------------
struct DECLSPEC_DRECORD TGLMHeader
{
public:
	System::StaticArray<System::WideChar, 4> fileID;
	int version;
	System::StaticArray<System::WideChar, 64> strFile;
	System::StaticArray<System::WideChar, 64> animName;
	int animIndex;
	int numBones;
	int numLODs;
	int ofsLODs;
	int numSurfaces;
	int ofsSurfHierarchy;
	int ofsEnd;
};


typedef System::DynamicArray<int> TGLMSurfaceHeirachyOffsets;

struct DECLSPEC_DRECORD TGLMSurfaceHeirachy
{
private:
	typedef System::DynamicArray<int> _TGLMSurfaceHeirachy__1;
	
	
public:
	System::StaticArray<System::WideChar, 64> name;
	unsigned flags;
	System::StaticArray<System::WideChar, 64> shader;
	int shaderIndex;
	int parentIndex;
	int numChildren;
	_TGLMSurfaceHeirachy__1 childIndices;
};


struct DECLSPEC_DRECORD TGLMSurfaceHeader
{
public:
	int ident;
	int thisSurfaceIndex;
	int ofsHeader;
	int numVerts;
	int ofsVerts;
	int numTriangles;
	int ofsTriangles;
	int numBoneReferences;
	int ofsBoneReferences;
	int ofsEnd;
};


struct DECLSPEC_DRECORD TGLMTriangle
{
public:
	System::StaticArray<int, 3> indices;
};


struct DECLSPEC_DRECORD TGLMVertex
{
public:
	Vectortypes::TVector3f normal;
	Vectortypes::TVector3f vertex;
	unsigned uiNumWeightsAndBoneIndices;
	System::StaticArray<System::Byte, 4> BoneWeightings;
};


struct DECLSPEC_DRECORD TGLMSurface
{
private:
	typedef System::DynamicArray<TGLMTriangle> _TGLMSurface__1;
	
	typedef System::DynamicArray<TGLMVertex> _TGLMSurface__2;
	
	typedef System::DynamicArray<Vectortypes::TVector2f> _TGLMSurface__3;
	
	typedef System::DynamicArray<int> _TGLMSurface__4;
	
	
public:
	TGLMSurfaceHeader SurfaceHeader;
	_TGLMSurface__1 Triangles;
	_TGLMSurface__2 Vertices;
	_TGLMSurface__3 TexCoords;
	_TGLMSurface__4 BoneReferences;
};


struct DECLSPEC_DRECORD TGLMLODInfo
{
public:
	int ofsEnd;
};


typedef System::DynamicArray<int> TGLMLODSurfaceOffsets;

struct DECLSPEC_DRECORD TGLMLODs
{
private:
	typedef System::DynamicArray<TGLMSurface> _TGLMLODs__1;
	
	
public:
	TGLMLODInfo LODInfo;
	TGLMLODSurfaceOffsets LODSurfaceOffsets;
	_TGLMLODs__1 Surfaces;
};


struct DECLSPEC_DRECORD TGLAHeader
{
public:
	System::StaticArray<System::WideChar, 4> fileID;
	int version;
	System::StaticArray<System::WideChar, 64> Name;
	float fScale;
	int numFrames;
	int ofsFrames;
	int numBones;
	int ofsCompBonePool;
	int ofsSkel;
	int ofsEnd;
};


typedef System::StaticArray<Vectortypes::TVector4f, 3> TGLABone;

typedef System::StaticArray<System::Word, 7> TGLACompQuatBone;

typedef System::DynamicArray<int> TGLASkeletonOffsets;

struct DECLSPEC_DRECORD TGLASkeleton
{
private:
	typedef System::DynamicArray<int> _TGLASkeleton__1;
	
	
public:
	System::StaticArray<System::WideChar, 64> name;
	unsigned flags;
	int parent;
	TGLABone BasePoseMat;
	TGLABone BasePoseMatInv;
	int numChildren;
	_TGLASkeleton__1 children;
};


class DELPHICLASS TFileGLM;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TFileGLM : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	typedef System::DynamicArray<TGLMSurfaceHeirachy> _TFileGLM__1;
	
	typedef System::DynamicArray<TGLMLODs> _TFileGLM__2;
	
	
public:
	TGLMHeader ModelHeader;
	TGLMSurfaceHeirachyOffsets SurfaceHeirachyOffsets;
	_TFileGLM__1 SurfaceHeirachy;
	_TFileGLM__2 LODs;
	void __fastcall LoadFromStream(System::Classes::TStream* aStream);
public:
	/* TObject.Create */ inline __fastcall TFileGLM(void) : System::TObject() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TFileGLM(void) { }
	
};

#pragma pack(pop)

class DELPHICLASS TFileGLA;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TFileGLA : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	typedef System::DynamicArray<TGLASkeleton> _TFileGLA__1;
	
	typedef System::DynamicArray<int> _TFileGLA__2;
	
	typedef System::DynamicArray<TGLACompQuatBone> _TFileGLA__3;
	
	
public:
	TGLAHeader AnimHeader;
	TGLASkeletonOffsets SkeletonOffsets;
	_TFileGLA__1 Skeleton;
	_TFileGLA__2 BoneIndices;
	_TFileGLA__3 CompBonePool;
	TGLACompQuatBone __fastcall GetCompressedMatrix(int Frame, int Bone);
	Vectortypes::TMatrix4f __fastcall GetUnCompressedMatrix(int Frame, int Bone);
	void __fastcall LoadFromStream(System::Classes::TStream* aStream);
public:
	/* TObject.Create */ inline __fastcall TFileGLA(void) : System::TObject() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TFileGLA(void) { }
	
};

#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------
extern PACKAGE int __fastcall G2_GetVertWeights(const TGLMVertex &vert);
extern PACKAGE int __fastcall G2_GetVertBoneIndex(const TGLMVertex &vert, int iWeightNum);
extern PACKAGE float __fastcall G2_GetVertBoneWeight(const TGLMVertex &vert, unsigned iWeightNum, float &fTotalWeight, const unsigned iNumWeights);
extern PACKAGE void __fastcall MC_UnCompressQuat(Vectortypes::TMatrix4f &mat, System::Word const *comp);
}	/* namespace Filegl2 */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_FILEGL2)
using namespace Filegl2;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Filegl2HPP
