// CodeGear C++Builder
// Copyright (c) 1995, 2012 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLVectorFileObjects.pas' rev: 24.00 (Win32)

#ifndef GlvectorfileobjectsHPP
#define GlvectorfileobjectsHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <System.Classes.hpp>	// Pascal unit
#include <GLScene.hpp>	// Pascal unit
#include <OpenGLTokens.hpp>	// Pascal unit
#include <VectorGeometry.hpp>	// Pascal unit
#include <System.SysUtils.hpp>	// Pascal unit
#include <GLTexture.hpp>	// Pascal unit
#include <GLMaterial.hpp>	// Pascal unit
#include <GLMesh.hpp>	// Pascal unit
#include <VectorLists.hpp>	// Pascal unit
#include <PersistentClasses.hpp>	// Pascal unit
#include <Octree.hpp>	// Pascal unit
#include <GeometryBB.hpp>	// Pascal unit
#include <ApplicationFileIO.hpp>	// Pascal unit
#include <GLSilhouette.hpp>	// Pascal unit
#include <GLContext.hpp>	// Pascal unit
#include <GLColor.hpp>	// Pascal unit
#include <GLRenderContextInfo.hpp>	// Pascal unit
#include <GLCoordinates.hpp>	// Pascal unit
#include <BaseClasses.hpp>	// Pascal unit
#include <GLTextureFormat.hpp>	// Pascal unit
#include <VectorTypes.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Glvectorfileobjects
{
//-- type declarations -------------------------------------------------------
enum TMeshAutoCentering : unsigned char { macCenterX, macCenterY, macCenterZ, macUseBarycenter, macRestorePosition };

typedef System::Set<TMeshAutoCentering, TMeshAutoCentering::macCenterX, TMeshAutoCentering::macRestorePosition>  TMeshAutoCenterings;

enum TMeshObjectMode : unsigned char { momTriangles, momTriangleStrip, momFaceGroups };

class DELPHICLASS TBaseMeshObject;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TBaseMeshObject : public Persistentclasses::TPersistentObject
{
	typedef Persistentclasses::TPersistentObject inherited;
	
private:
	System::UnicodeString FName;
	Vectorlists::TAffineVectorList* FVertices;
	Vectorlists::TAffineVectorList* FNormals;
	bool FVisible;
	
protected:
	void __fastcall SetVertices(Vectorlists::TAffineVectorList* const val);
	void __fastcall SetNormals(Vectorlists::TAffineVectorList* const val);
	DYNAMIC void __fastcall ContributeToBarycenter(Vectortypes::TVector3f &currentSum, int &nb);
	
public:
	__fastcall virtual TBaseMeshObject(void);
	__fastcall virtual ~TBaseMeshObject(void);
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	DYNAMIC void __fastcall WriteToFiler(Persistentclasses::TVirtualWriter* writer);
	DYNAMIC void __fastcall ReadFromFiler(Persistentclasses::TVirtualReader* reader);
	DYNAMIC void __fastcall Clear(void);
	DYNAMIC void __fastcall Translate(const Vectortypes::TVector3f &delta);
	void __fastcall BuildNormals(Vectorlists::TIntegerList* vertexIndices, TMeshObjectMode mode, Vectorlists::TIntegerList* normalIndices = (Vectorlists::TIntegerList*)(0x0));
	DYNAMIC Vectorlists::TAffineVectorList* __fastcall ExtractTriangles(Vectorlists::TAffineVectorList* texCoords = (Vectorlists::TAffineVectorList*)(0x0), Vectorlists::TAffineVectorList* normals = (Vectorlists::TAffineVectorList*)(0x0));
	__property System::UnicodeString Name = {read=FName, write=FName};
	__property bool Visible = {read=FVisible, write=FVisible, nodefault};
	__property Vectorlists::TAffineVectorList* Vertices = {read=FVertices, write=SetVertices};
	__property Vectorlists::TAffineVectorList* Normals = {read=FNormals, write=SetNormals};
public:
	/* TPersistentObject.CreateFromFiler */ inline __fastcall TBaseMeshObject(Persistentclasses::TVirtualReader* reader) : Persistentclasses::TPersistentObject(reader) { }
	
};

#pragma pack(pop)

enum TSkeletonFrameTransform : unsigned char { sftRotation, sftQuaternion };

class DELPHICLASS TSkeletonFrame;
class DELPHICLASS TSkeletonFrameList;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TSkeletonFrame : public Persistentclasses::TPersistentObject
{
	typedef Persistentclasses::TPersistentObject inherited;
	
private:
	TSkeletonFrameList* FOwner;
	System::UnicodeString FName;
	Vectorlists::TAffineVectorList* FPosition;
	Vectorlists::TAffineVectorList* FRotation;
	Vectorlists::TQuaternionList* FQuaternion;
	Vectorgeometry::TMatrixArray *FLocalMatrixList;
	TSkeletonFrameTransform FTransformMode;
	
protected:
	void __fastcall SetPosition(Vectorlists::TAffineVectorList* const val);
	void __fastcall SetRotation(Vectorlists::TAffineVectorList* const val);
	void __fastcall SetQuaternion(Vectorlists::TQuaternionList* const val);
	
public:
	__fastcall TSkeletonFrame(TSkeletonFrameList* aOwner);
	__fastcall virtual TSkeletonFrame(void);
	__fastcall virtual ~TSkeletonFrame(void);
	DYNAMIC void __fastcall WriteToFiler(Persistentclasses::TVirtualWriter* writer);
	DYNAMIC void __fastcall ReadFromFiler(Persistentclasses::TVirtualReader* reader);
	__property TSkeletonFrameList* Owner = {read=FOwner};
	__property System::UnicodeString Name = {read=FName, write=FName};
	__property Vectorlists::TAffineVectorList* Position = {read=FPosition, write=SetPosition};
	__property Vectorlists::TAffineVectorList* Rotation = {read=FRotation, write=SetRotation};
	__property Vectorlists::TQuaternionList* Quaternion = {read=FQuaternion, write=SetQuaternion};
	__property TSkeletonFrameTransform TransformMode = {read=FTransformMode, write=FTransformMode, nodefault};
	Vectorgeometry::PMatrixArray __fastcall LocalMatrixList(void);
	void __fastcall FlushLocalMatrixList(void);
	void __fastcall ConvertQuaternionsToRotations(bool KeepQuaternions = true);
	void __fastcall ConvertRotationsToQuaternions(bool KeepRotations = true);
public:
	/* TPersistentObject.CreateFromFiler */ inline __fastcall TSkeletonFrame(Persistentclasses::TVirtualReader* reader) : Persistentclasses::TPersistentObject(reader) { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TSkeletonFrameList : public Persistentclasses::TPersistentObjectList
{
	typedef Persistentclasses::TPersistentObjectList inherited;
	
public:
	TSkeletonFrame* operator[](int Index) { return Items[Index]; }
	
private:
	System::Classes::TPersistent* FOwner;
	
protected:
	TSkeletonFrame* __fastcall GetSkeletonFrame(int Index);
	
public:
	__fastcall TSkeletonFrameList(System::Classes::TPersistent* AOwner);
	__fastcall virtual ~TSkeletonFrameList(void);
	DYNAMIC void __fastcall ReadFromFiler(Persistentclasses::TVirtualReader* reader);
	void __fastcall ConvertQuaternionsToRotations(bool KeepQuaternions = true, bool SetTransformMode = true);
	void __fastcall ConvertRotationsToQuaternions(bool KeepRotations = true, bool SetTransformMode = true);
	__property System::Classes::TPersistent* Owner = {read=FOwner};
	DYNAMIC void __fastcall Clear(void);
	__property TSkeletonFrame* Items[int Index] = {read=GetSkeletonFrame/*, default*/};
public:
	/* TPersistentObjectList.Create */ inline __fastcall virtual TSkeletonFrameList(void) : Persistentclasses::TPersistentObjectList() { }
	
public:
	/* TPersistentObject.CreateFromFiler */ inline __fastcall TSkeletonFrameList(Persistentclasses::TVirtualReader* reader) : Persistentclasses::TPersistentObjectList(reader) { }
	
};

#pragma pack(pop)

class DELPHICLASS TSkeletonBoneList;
class DELPHICLASS TSkeleton;
class DELPHICLASS TSkeletonBone;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TSkeletonBoneList : public Persistentclasses::TPersistentObjectList
{
	typedef Persistentclasses::TPersistentObjectList inherited;
	
public:
	TSkeletonBone* operator[](int Index) { return Items[Index]; }
	
private:
	TSkeleton* FSkeleton;
	
protected:
	Vectortypes::TMatrix4f FGlobalMatrix;
	TSkeletonBone* __fastcall GetSkeletonBone(int Index);
	virtual void __fastcall AfterObjectCreatedByReader(System::TObject* Sender);
	
public:
	__fastcall TSkeletonBoneList(TSkeleton* aOwner);
	__fastcall virtual TSkeletonBoneList(void);
	__fastcall virtual ~TSkeletonBoneList(void);
	DYNAMIC void __fastcall WriteToFiler(Persistentclasses::TVirtualWriter* writer);
	DYNAMIC void __fastcall ReadFromFiler(Persistentclasses::TVirtualReader* reader);
	__property TSkeleton* Skeleton = {read=FSkeleton};
	__property TSkeletonBone* Items[int Index] = {read=GetSkeletonBone/*, default*/};
	virtual TSkeletonBone* __fastcall BoneByID(int anID);
	virtual TSkeletonBone* __fastcall BoneByName(const System::UnicodeString aName);
	int __fastcall BoneCount(void);
	virtual void __fastcall BuildList(Glrendercontextinfo::TRenderContextInfo &mrci) = 0 ;
	virtual void __fastcall PrepareGlobalMatrices(void);
public:
	/* TPersistentObject.CreateFromFiler */ inline __fastcall TSkeletonBoneList(Persistentclasses::TVirtualReader* reader) : Persistentclasses::TPersistentObjectList(reader) { }
	
};

#pragma pack(pop)

class DELPHICLASS TSkeletonRootBoneList;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TSkeletonRootBoneList : public TSkeletonBoneList
{
	typedef TSkeletonBoneList inherited;
	
public:
	DYNAMIC void __fastcall WriteToFiler(Persistentclasses::TVirtualWriter* writer);
	DYNAMIC void __fastcall ReadFromFiler(Persistentclasses::TVirtualReader* reader);
	virtual void __fastcall BuildList(Glrendercontextinfo::TRenderContextInfo &mrci);
	__property Vectortypes::TMatrix4f GlobalMatrix = {read=FGlobalMatrix, write=FGlobalMatrix};
public:
	/* TSkeletonBoneList.CreateOwned */ inline __fastcall TSkeletonRootBoneList(TSkeleton* aOwner) : TSkeletonBoneList(aOwner) { }
	/* TSkeletonBoneList.Create */ inline __fastcall virtual TSkeletonRootBoneList(void) : TSkeletonBoneList() { }
	/* TSkeletonBoneList.Destroy */ inline __fastcall virtual ~TSkeletonRootBoneList(void) { }
	
public:
	/* TPersistentObject.CreateFromFiler */ inline __fastcall TSkeletonRootBoneList(Persistentclasses::TVirtualReader* reader) : TSkeletonBoneList(reader) { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TSkeletonBone : public TSkeletonBoneList
{
	typedef TSkeletonBoneList inherited;
	
public:
	TSkeletonBone* operator[](int Index) { return Items[Index]; }
	
private:
	TSkeletonBoneList* FOwner;
	int FBoneID;
	System::UnicodeString FName;
	unsigned FColor;
	
protected:
	HIDESBASE TSkeletonBone* __fastcall GetSkeletonBone(int Index);
	void __fastcall SetColor(const unsigned val);
	
public:
	__fastcall TSkeletonBone(TSkeletonBoneList* aOwner);
	__fastcall virtual TSkeletonBone(void);
	__fastcall virtual ~TSkeletonBone(void);
	DYNAMIC void __fastcall WriteToFiler(Persistentclasses::TVirtualWriter* writer);
	DYNAMIC void __fastcall ReadFromFiler(Persistentclasses::TVirtualReader* reader);
	virtual void __fastcall BuildList(Glrendercontextinfo::TRenderContextInfo &mrci);
	__property TSkeletonBoneList* Owner = {read=FOwner};
	__property System::UnicodeString Name = {read=FName, write=FName};
	__property int BoneID = {read=FBoneID, write=FBoneID, nodefault};
	__property unsigned Color = {read=FColor, write=SetColor, nodefault};
	__property TSkeletonBone* Items[int Index] = {read=GetSkeletonBone/*, default*/};
	virtual TSkeletonBone* __fastcall BoneByID(int anID);
	virtual TSkeletonBone* __fastcall BoneByName(const System::UnicodeString aName);
	void __fastcall SetGlobalMatrix(const Vectortypes::TMatrix4f &Matrix);
	void __fastcall SetGlobalMatrixForRagDoll(const Vectortypes::TMatrix4f &RagDollMatrix);
	virtual void __fastcall PrepareGlobalMatrices(void);
	__property Vectortypes::TMatrix4f GlobalMatrix = {read=FGlobalMatrix};
	DYNAMIC void __fastcall Clean(void);
public:
	/* TPersistentObject.CreateFromFiler */ inline __fastcall TSkeletonBone(Persistentclasses::TVirtualReader* reader) : TSkeletonBoneList(reader) { }
	
};

#pragma pack(pop)

class DELPHICLASS TSkeletonCollider;
class DELPHICLASS TSkeletonColliderList;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TSkeletonCollider : public Persistentclasses::TPersistentObject
{
	typedef Persistentclasses::TPersistentObject inherited;
	
private:
	TSkeletonColliderList* FOwner;
	TSkeletonBone* FBone;
	int FBoneID;
	Vectortypes::TMatrix4f FLocalMatrix;
	Vectortypes::TMatrix4f FGlobalMatrix;
	bool FAutoUpdate;
	
protected:
	void __fastcall SetBone(TSkeletonBone* const val);
	void __fastcall SetLocalMatrix(const Vectortypes::TMatrix4f &val);
	
public:
	__fastcall virtual TSkeletonCollider(void);
	__fastcall TSkeletonCollider(TSkeletonColliderList* AOwner);
	DYNAMIC void __fastcall WriteToFiler(Persistentclasses::TVirtualWriter* writer);
	DYNAMIC void __fastcall ReadFromFiler(Persistentclasses::TVirtualReader* reader);
	virtual void __fastcall AlignCollider(void);
	__property TSkeletonColliderList* Owner = {read=FOwner};
	__property TSkeletonBone* Bone = {read=FBone, write=SetBone};
	__property Vectortypes::TMatrix4f LocalMatrix = {read=FLocalMatrix, write=SetLocalMatrix};
	__property Vectortypes::TMatrix4f GlobalMatrix = {read=FGlobalMatrix};
	__property bool AutoUpdate = {read=FAutoUpdate, write=FAutoUpdate, nodefault};
public:
	/* TPersistentObject.CreateFromFiler */ inline __fastcall TSkeletonCollider(Persistentclasses::TVirtualReader* reader) : Persistentclasses::TPersistentObject(reader) { }
	/* TPersistentObject.Destroy */ inline __fastcall virtual ~TSkeletonCollider(void) { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TSkeletonColliderList : public Persistentclasses::TPersistentObjectList
{
	typedef Persistentclasses::TPersistentObjectList inherited;
	
public:
	TSkeletonCollider* operator[](int Index) { return Items[Index]; }
	
private:
	System::Classes::TPersistent* FOwner;
	
protected:
	TSkeletonCollider* __fastcall GetSkeletonCollider(int index);
	
public:
	__fastcall TSkeletonColliderList(System::Classes::TPersistent* AOwner);
	__fastcall virtual ~TSkeletonColliderList(void);
	DYNAMIC void __fastcall ReadFromFiler(Persistentclasses::TVirtualReader* reader);
	DYNAMIC void __fastcall Clear(void);
	void __fastcall AlignColliders(void);
	__property System::Classes::TPersistent* Owner = {read=FOwner};
	__property TSkeletonCollider* Items[int Index] = {read=GetSkeletonCollider/*, default*/};
public:
	/* TPersistentObjectList.Create */ inline __fastcall virtual TSkeletonColliderList(void) : Persistentclasses::TPersistentObjectList() { }
	
public:
	/* TPersistentObject.CreateFromFiler */ inline __fastcall TSkeletonColliderList(Persistentclasses::TVirtualReader* reader) : Persistentclasses::TPersistentObjectList(reader) { }
	
};

#pragma pack(pop)

struct DECLSPEC_DRECORD TBlendedLerpInfo
{
public:
	int frameIndex1;
	int frameIndex2;
	float lerpFactor;
	float weight;
	Vectorlists::TAffineVectorList* externalPositions;
	Vectorlists::TAffineVectorList* externalRotations;
	Vectorlists::TQuaternionList* externalQuaternions;
};


class DELPHICLASS TGLBaseMesh;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TSkeleton : public Persistentclasses::TPersistentObject
{
	typedef Persistentclasses::TPersistentObject inherited;
	
private:
	TGLBaseMesh* FOwner;
	TSkeletonRootBoneList* FRootBones;
	TSkeletonFrameList* FFrames;
	TSkeletonFrame* FCurrentFrame;
	System::Classes::TList* FBonesByIDCache;
	TSkeletonColliderList* FColliders;
	bool FRagDollEnabled;
	bool FMorphInvisibleParts;
	
protected:
	void __fastcall SetRootBones(TSkeletonRootBoneList* const val);
	void __fastcall SetFrames(TSkeletonFrameList* const val);
	TSkeletonFrame* __fastcall GetCurrentFrame(void);
	void __fastcall SetCurrentFrame(TSkeletonFrame* val);
	void __fastcall SetColliders(TSkeletonColliderList* const val);
	
public:
	__fastcall TSkeleton(TGLBaseMesh* AOwner);
	__fastcall virtual TSkeleton(void);
	__fastcall virtual ~TSkeleton(void);
	DYNAMIC void __fastcall WriteToFiler(Persistentclasses::TVirtualWriter* writer);
	DYNAMIC void __fastcall ReadFromFiler(Persistentclasses::TVirtualReader* reader);
	__property TGLBaseMesh* Owner = {read=FOwner};
	__property TSkeletonRootBoneList* RootBones = {read=FRootBones, write=SetRootBones};
	__property TSkeletonFrameList* Frames = {read=FFrames, write=SetFrames};
	__property TSkeletonFrame* CurrentFrame = {read=GetCurrentFrame, write=SetCurrentFrame};
	__property TSkeletonColliderList* Colliders = {read=FColliders, write=SetColliders};
	void __fastcall FlushBoneByIDCache(void);
	TSkeletonBone* __fastcall BoneByID(int anID);
	TSkeletonBone* __fastcall BoneByName(const System::UnicodeString aName);
	int __fastcall BoneCount(void);
	void __fastcall MorphTo(int frameIndex)/* overload */;
	void __fastcall MorphTo(TSkeletonFrame* frame)/* overload */;
	void __fastcall Lerp(int frameIndex1, int frameIndex2, float lerpFactor);
	void __fastcall BlendedLerps(TBlendedLerpInfo const *lerpInfos, const int lerpInfos_Size);
	void __fastcall MakeSkeletalTranslationStatic(int startFrame, int endFrame);
	void __fastcall MakeSkeletalRotationDelta(int startFrame, int endFrame);
	void __fastcall MorphMesh(bool normalize);
	void __fastcall Synchronize(TSkeleton* reference);
	void __fastcall Clear(void);
	void __fastcall StartRagdoll(void);
	void __fastcall StopRagdoll(void);
	__property bool MorphInvisibleParts = {read=FMorphInvisibleParts, write=FMorphInvisibleParts, nodefault};
public:
	/* TPersistentObject.CreateFromFiler */ inline __fastcall TSkeleton(Persistentclasses::TVirtualReader* reader) : Persistentclasses::TPersistentObject(reader) { }
	
};

#pragma pack(pop)

enum TMeshObjectRenderingOption : unsigned char { moroGroupByMaterial };

typedef System::Set<TMeshObjectRenderingOption, TMeshObjectRenderingOption::moroGroupByMaterial, TMeshObjectRenderingOption::moroGroupByMaterial>  TMeshObjectRenderingOptions;

enum TVBOBuffer : unsigned char { vbVertices, vbNormals, vbColors, vbTexCoords, vbLightMapTexCoords, vbTexCoordsEx };

typedef System::Set<TVBOBuffer, TVBOBuffer::vbVertices, TVBOBuffer::vbTexCoordsEx>  TVBOBuffers;

class DELPHICLASS TMeshObject;
class DELPHICLASS TMeshObjectList;
class DELPHICLASS TFaceGroups;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TMeshObject : public TBaseMeshObject
{
	typedef TBaseMeshObject inherited;
	
private:
	typedef System::DynamicArray<Glcontext::TGLVBOHandle*> _TMeshObject__1;
	
	
private:
	TMeshObjectList* FOwner;
	unsigned FExtentCacheRevision;
	Vectorlists::TAffineVectorList* FTexCoords;
	Vectorlists::TAffineVectorList* FLightMapTexCoords;
	Vectorlists::TVectorList* FColors;
	TFaceGroups* FFaceGroups;
	TMeshObjectMode FMode;
	TMeshObjectRenderingOptions FRenderingOptions;
	bool FArraysDeclared;
	bool FLightMapArrayEnabled;
	int FLastLightMapIndex;
	System::Classes::TList* FTexCoordsEx;
	int FBinormalsTexCoordIndex;
	int FTangentsTexCoordIndex;
	unsigned FLastXOpenGLTexMapping;
	bool FUseVBO;
	Glcontext::TGLVBOHandle* FVerticesVBO;
	Glcontext::TGLVBOHandle* FNormalsVBO;
	Glcontext::TGLVBOHandle* FColorsVBO;
	_TMeshObject__1 FTexCoordsVBO;
	Glcontext::TGLVBOHandle* FLightmapTexCoordsVBO;
	TVBOBuffers FValidBuffers;
	Geometrybb::TAABB FExtentCache;
	void __fastcall SetUseVBO(const bool Value);
	void __fastcall SetValidBuffers(TVBOBuffers Value);
	
protected:
	void __fastcall SetTexCoords(Vectorlists::TAffineVectorList* const val);
	void __fastcall SetLightmapTexCoords(Vectorlists::TAffineVectorList* const val);
	void __fastcall SetColors(Vectorlists::TVectorList* const val);
	void __fastcall BufferArrays(void);
	void __fastcall DeclareArraysToOpenGL(Glrendercontextinfo::TRenderContextInfo &mrci, bool evenIfAlreadyDeclared = false);
	void __fastcall DisableOpenGLArrays(Glrendercontextinfo::TRenderContextInfo &mrci);
	void __fastcall EnableLightMapArray(Glrendercontextinfo::TRenderContextInfo &mrci);
	void __fastcall DisableLightMapArray(Glrendercontextinfo::TRenderContextInfo &mrci);
	void __fastcall SetTexCoordsEx(int index, Vectorlists::TVectorList* const val);
	Vectorlists::TVectorList* __fastcall GetTexCoordsEx(int index);
	void __fastcall SetBinormals(Vectorlists::TVectorList* const val);
	Vectorlists::TVectorList* __fastcall GetBinormals(void);
	void __fastcall SetBinormalsTexCoordIndex(const int val);
	void __fastcall SetTangents(Vectorlists::TVectorList* const val);
	Vectorlists::TVectorList* __fastcall GetTangents(void);
	void __fastcall SetTangentsTexCoordIndex(const int val);
	__property TVBOBuffers ValidBuffers = {read=FValidBuffers, write=SetValidBuffers, nodefault};
	
public:
	__fastcall TMeshObject(TMeshObjectList* AOwner);
	__fastcall virtual TMeshObject(void);
	__fastcall virtual ~TMeshObject(void);
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	DYNAMIC void __fastcall WriteToFiler(Persistentclasses::TVirtualWriter* writer);
	DYNAMIC void __fastcall ReadFromFiler(Persistentclasses::TVirtualReader* reader);
	DYNAMIC void __fastcall Clear(void);
	DYNAMIC Vectorlists::TAffineVectorList* __fastcall ExtractTriangles(Vectorlists::TAffineVectorList* texCoords = (Vectorlists::TAffineVectorList*)(0x0), Vectorlists::TAffineVectorList* normals = (Vectorlists::TAffineVectorList*)(0x0));
	DYNAMIC int __fastcall TriangleCount(void);
	void __fastcall PrepareMaterialLibraryCache(Glmaterial::TGLMaterialLibrary* matLib);
	void __fastcall DropMaterialLibraryCache(void);
	virtual void __fastcall PrepareBuildList(Glrendercontextinfo::TRenderContextInfo &mrci);
	virtual void __fastcall BuildList(Glrendercontextinfo::TRenderContextInfo &mrci);
	virtual void __fastcall GetExtents(/* out */ Vectortypes::TVector3f &min, /* out */ Vectortypes::TVector3f &max)/* overload */;
	virtual void __fastcall GetExtents(/* out */ Geometrybb::TAABB &aabb)/* overload */;
	Vectortypes::TVector4f __fastcall GetBarycenter(void);
	DYNAMIC void __fastcall Prepare(void);
	virtual bool __fastcall PointInObject(const Vectortypes::TVector3f &aPoint);
	void __fastcall GetTriangleData(int tri, Vectorlists::TAffineVectorList* list, Vectortypes::TVector3f &v0, Vectortypes::TVector3f &v1, Vectortypes::TVector3f &v2)/* overload */;
	void __fastcall GetTriangleData(int tri, Vectorlists::TVectorList* list, Vectortypes::TVector4f &v0, Vectortypes::TVector4f &v1, Vectortypes::TVector4f &v2)/* overload */;
	void __fastcall SetTriangleData(int tri, Vectorlists::TAffineVectorList* list, const Vectortypes::TVector3f &v0, const Vectortypes::TVector3f &v1, const Vectortypes::TVector3f &v2)/* overload */;
	void __fastcall SetTriangleData(int tri, Vectorlists::TVectorList* list, const Vectortypes::TVector4f &v0, const Vectortypes::TVector4f &v1, const Vectortypes::TVector4f &v2)/* overload */;
	void __fastcall BuildTangentSpace(bool buildBinormals = true, bool buildTangents = true);
	__property TMeshObjectList* Owner = {read=FOwner};
	__property TMeshObjectMode Mode = {read=FMode, write=FMode, nodefault};
	__property Vectorlists::TAffineVectorList* TexCoords = {read=FTexCoords, write=SetTexCoords};
	__property Vectorlists::TAffineVectorList* LightMapTexCoords = {read=FLightMapTexCoords, write=SetLightmapTexCoords};
	__property Vectorlists::TVectorList* Colors = {read=FColors, write=SetColors};
	__property TFaceGroups* FaceGroups = {read=FFaceGroups};
	__property TMeshObjectRenderingOptions RenderingOptions = {read=FRenderingOptions, write=FRenderingOptions, nodefault};
	__property bool UseVBO = {read=FUseVBO, write=SetUseVBO, nodefault};
	__property Vectorlists::TVectorList* TexCoordsEx[int index] = {read=GetTexCoordsEx, write=SetTexCoordsEx};
	__property Vectorlists::TVectorList* Binormals = {read=GetBinormals, write=SetBinormals};
	__property Vectorlists::TVectorList* Tangents = {read=GetTangents, write=SetTangents};
	__property int BinormalsTexCoordIndex = {read=FBinormalsTexCoordIndex, write=SetBinormalsTexCoordIndex, nodefault};
	__property int TangentsTexCoordIndex = {read=FTangentsTexCoordIndex, write=SetTangentsTexCoordIndex, nodefault};
public:
	/* TPersistentObject.CreateFromFiler */ inline __fastcall TMeshObject(Persistentclasses::TVirtualReader* reader) : TBaseMeshObject(reader) { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TMeshObjectList : public Persistentclasses::TPersistentObjectList
{
	typedef Persistentclasses::TPersistentObjectList inherited;
	
public:
	TMeshObject* operator[](int Index) { return Items[Index]; }
	
private:
	TGLBaseMesh* FOwner;
	bool __fastcall GetUseVBO(void);
	void __fastcall SetUseVBO(const bool Value);
	
protected:
	TMeshObject* __fastcall GetMeshObject(int Index);
	
public:
	__fastcall TMeshObjectList(TGLBaseMesh* aOwner);
	__fastcall virtual ~TMeshObjectList(void);
	DYNAMIC void __fastcall ReadFromFiler(Persistentclasses::TVirtualReader* reader);
	void __fastcall PrepareMaterialLibraryCache(Glmaterial::TGLMaterialLibrary* matLib);
	void __fastcall DropMaterialLibraryCache(void);
	virtual void __fastcall PrepareBuildList(Glrendercontextinfo::TRenderContextInfo &mrci);
	virtual void __fastcall BuildList(Glrendercontextinfo::TRenderContextInfo &mrci);
	void __fastcall MorphTo(int morphTargetIndex);
	void __fastcall Lerp(int morphTargetIndex1, int morphTargetIndex2, float lerpFactor);
	int __fastcall MorphTargetCount(void);
	void __fastcall GetExtents(/* out */ Vectortypes::TVector3f &min, /* out */ Vectortypes::TVector3f &max);
	void __fastcall Translate(const Vectortypes::TVector3f &delta);
	Vectorlists::TAffineVectorList* __fastcall ExtractTriangles(Vectorlists::TAffineVectorList* texCoords = (Vectorlists::TAffineVectorList*)(0x0), Vectorlists::TAffineVectorList* normals = (Vectorlists::TAffineVectorList*)(0x0));
	int __fastcall TriangleCount(void);
	void __fastcall BuildTangentSpace(bool buildBinormals = true, bool buildTangents = true);
	__property bool UseVBO = {read=GetUseVBO, write=SetUseVBO, nodefault};
	DYNAMIC void __fastcall Prepare(void);
	TMeshObject* __fastcall FindMeshByName(System::UnicodeString MeshName);
	__property TGLBaseMesh* Owner = {read=FOwner};
	DYNAMIC void __fastcall Clear(void);
	__property TMeshObject* Items[int Index] = {read=GetMeshObject/*, default*/};
public:
	/* TPersistentObjectList.Create */ inline __fastcall virtual TMeshObjectList(void) : Persistentclasses::TPersistentObjectList() { }
	
public:
	/* TPersistentObject.CreateFromFiler */ inline __fastcall TMeshObjectList(Persistentclasses::TVirtualReader* reader) : Persistentclasses::TPersistentObjectList(reader) { }
	
};

#pragma pack(pop)

typedef System::TMetaClass* TMeshObjectListClass;

class DELPHICLASS TMeshMorphTarget;
class DELPHICLASS TMeshMorphTargetList;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TMeshMorphTarget : public TBaseMeshObject
{
	typedef TBaseMeshObject inherited;
	
private:
	TMeshMorphTargetList* FOwner;
	
public:
	__fastcall TMeshMorphTarget(TMeshMorphTargetList* AOwner);
	__fastcall virtual ~TMeshMorphTarget(void);
	DYNAMIC void __fastcall WriteToFiler(Persistentclasses::TVirtualWriter* writer);
	DYNAMIC void __fastcall ReadFromFiler(Persistentclasses::TVirtualReader* reader);
	__property TMeshMorphTargetList* Owner = {read=FOwner};
public:
	/* TBaseMeshObject.Create */ inline __fastcall virtual TMeshMorphTarget(void) : TBaseMeshObject() { }
	
public:
	/* TPersistentObject.CreateFromFiler */ inline __fastcall TMeshMorphTarget(Persistentclasses::TVirtualReader* reader) : TBaseMeshObject(reader) { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TMeshMorphTargetList : public Persistentclasses::TPersistentObjectList
{
	typedef Persistentclasses::TPersistentObjectList inherited;
	
public:
	TMeshMorphTarget* operator[](int Index) { return Items[Index]; }
	
private:
	System::Classes::TPersistent* FOwner;
	
protected:
	TMeshMorphTarget* __fastcall GetMeshMorphTarget(int Index);
	
public:
	__fastcall TMeshMorphTargetList(System::Classes::TPersistent* AOwner);
	__fastcall virtual ~TMeshMorphTargetList(void);
	DYNAMIC void __fastcall ReadFromFiler(Persistentclasses::TVirtualReader* reader);
	void __fastcall Translate(const Vectortypes::TVector3f &delta);
	__property System::Classes::TPersistent* Owner = {read=FOwner};
	DYNAMIC void __fastcall Clear(void);
	__property TMeshMorphTarget* Items[int Index] = {read=GetMeshMorphTarget/*, default*/};
public:
	/* TPersistentObjectList.Create */ inline __fastcall virtual TMeshMorphTargetList(void) : Persistentclasses::TPersistentObjectList() { }
	
public:
	/* TPersistentObject.CreateFromFiler */ inline __fastcall TMeshMorphTargetList(Persistentclasses::TVirtualReader* reader) : Persistentclasses::TPersistentObjectList(reader) { }
	
};

#pragma pack(pop)

class DELPHICLASS TMorphableMeshObject;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TMorphableMeshObject : public TMeshObject
{
	typedef TMeshObject inherited;
	
private:
	TMeshMorphTargetList* FMorphTargets;
	
public:
	__fastcall virtual TMorphableMeshObject(void);
	__fastcall virtual ~TMorphableMeshObject(void);
	DYNAMIC void __fastcall WriteToFiler(Persistentclasses::TVirtualWriter* writer);
	DYNAMIC void __fastcall ReadFromFiler(Persistentclasses::TVirtualReader* reader);
	DYNAMIC void __fastcall Clear(void);
	DYNAMIC void __fastcall Translate(const Vectortypes::TVector3f &delta);
	virtual void __fastcall MorphTo(int morphTargetIndex);
	virtual void __fastcall Lerp(int morphTargetIndex1, int morphTargetIndex2, float lerpFactor);
	__property TMeshMorphTargetList* MorphTargets = {read=FMorphTargets};
public:
	/* TMeshObject.CreateOwned */ inline __fastcall TMorphableMeshObject(TMeshObjectList* AOwner) : TMeshObject(AOwner) { }
	
public:
	/* TPersistentObject.CreateFromFiler */ inline __fastcall TMorphableMeshObject(Persistentclasses::TVirtualReader* reader) : TMeshObject(reader) { }
	
};

#pragma pack(pop)

#pragma pack(push,1)
struct DECLSPEC_DRECORD TVertexBoneWeight
{
public:
	int BoneID;
	float Weight;
};
#pragma pack(pop)


typedef System::StaticArray<TVertexBoneWeight, 134217728> TVertexBoneWeightArray;

typedef TVertexBoneWeightArray *PVertexBoneWeightArray;

typedef System::StaticArray<PVertexBoneWeightArray, 268435456> TVerticesBoneWeights;

typedef TVerticesBoneWeights *PVerticesBoneWeights;

typedef System::DynamicArray<TVertexBoneWeight> TVertexBoneWeightDynArray;

class DELPHICLASS TSkeletonMeshObject;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TSkeletonMeshObject : public TMorphableMeshObject
{
	typedef TMorphableMeshObject inherited;
	
private:
	TVerticesBoneWeights *FVerticesBonesWeights;
	int FVerticeBoneWeightCount;
	int FVerticeBoneWeightCapacity;
	int FBonesPerVertex;
	int FLastVerticeBoneWeightCount;
	int FLastBonesPerVertex;
	System::Classes::TList* FBoneMatrixInvertedMeshes;
	System::Classes::TList* FBackupInvertedMeshes;
	void __fastcall BackupBoneMatrixInvertedMeshes(void);
	void __fastcall RestoreBoneMatrixInvertedMeshes(void);
	
protected:
	void __fastcall SetVerticeBoneWeightCount(const int val);
	void __fastcall SetVerticeBoneWeightCapacity(const int val);
	void __fastcall SetBonesPerVertex(const int val);
	void __fastcall ResizeVerticesBonesWeights(void);
	
public:
	__fastcall virtual TSkeletonMeshObject(void);
	__fastcall virtual ~TSkeletonMeshObject(void);
	DYNAMIC void __fastcall WriteToFiler(Persistentclasses::TVirtualWriter* writer);
	DYNAMIC void __fastcall ReadFromFiler(Persistentclasses::TVirtualReader* reader);
	DYNAMIC void __fastcall Clear(void);
	__property PVerticesBoneWeights VerticesBonesWeights = {read=FVerticesBonesWeights};
	__property int VerticeBoneWeightCount = {read=FVerticeBoneWeightCount, write=SetVerticeBoneWeightCount, nodefault};
	__property int VerticeBoneWeightCapacity = {read=FVerticeBoneWeightCapacity, write=SetVerticeBoneWeightCapacity, nodefault};
	__property int BonesPerVertex = {read=FBonesPerVertex, write=SetBonesPerVertex, nodefault};
	int __fastcall FindOrAdd(int boneID, const Vectortypes::TVector3f &vertex, const Vectortypes::TVector3f &normal)/* overload */;
	int __fastcall FindOrAdd(const TVertexBoneWeightDynArray boneIDs, const Vectortypes::TVector3f &vertex, const Vectortypes::TVector3f &normal)/* overload */;
	void __fastcall AddWeightedBone(int aBoneID, float aWeight);
	void __fastcall AddWeightedBones(const TVertexBoneWeightDynArray boneIDs);
	void __fastcall PrepareBoneMatrixInvertedMeshes(void);
	void __fastcall ApplyCurrentSkeletonFrame(bool normalize);
public:
	/* TMeshObject.CreateOwned */ inline __fastcall TSkeletonMeshObject(TMeshObjectList* AOwner) : TMorphableMeshObject(AOwner) { }
	
public:
	/* TPersistentObject.CreateFromFiler */ inline __fastcall TSkeletonMeshObject(Persistentclasses::TVirtualReader* reader) : TMorphableMeshObject(reader) { }
	
};

#pragma pack(pop)

class DELPHICLASS TFaceGroup;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TFaceGroup : public Persistentclasses::TPersistentObject
{
	typedef Persistentclasses::TPersistentObject inherited;
	
private:
	TFaceGroups* FOwner;
	System::UnicodeString FMaterialName;
	Glmaterial::TGLLibMaterial* FMaterialCache;
	int FLightMapIndex;
	int FRenderGroupID;
	
protected:
	void __fastcall AttachLightmap(Gltexture::TGLTexture* lightMap, Glrendercontextinfo::TRenderContextInfo &mrci);
	void __fastcall AttachOrDetachLightmap(Glrendercontextinfo::TRenderContextInfo &mrci);
	
public:
	__fastcall virtual TFaceGroup(TFaceGroups* AOwner);
	__fastcall virtual ~TFaceGroup(void);
	DYNAMIC void __fastcall WriteToFiler(Persistentclasses::TVirtualWriter* writer);
	DYNAMIC void __fastcall ReadFromFiler(Persistentclasses::TVirtualReader* reader);
	void __fastcall PrepareMaterialLibraryCache(Glmaterial::TGLMaterialLibrary* matLib);
	void __fastcall DropMaterialLibraryCache(void);
	virtual void __fastcall BuildList(Glrendercontextinfo::TRenderContextInfo &mrci) = 0 ;
	DYNAMIC void __fastcall AddToTriangles(Vectorlists::TAffineVectorList* aList, Vectorlists::TAffineVectorList* aTexCoords = (Vectorlists::TAffineVectorList*)(0x0), Vectorlists::TAffineVectorList* aNormals = (Vectorlists::TAffineVectorList*)(0x0));
	DYNAMIC int __fastcall TriangleCount(void) = 0 ;
	DYNAMIC void __fastcall Reverse(void);
	DYNAMIC void __fastcall Prepare(void);
	__property TFaceGroups* Owner = {read=FOwner, write=FOwner};
	__property System::UnicodeString MaterialName = {read=FMaterialName, write=FMaterialName};
	__property Glmaterial::TGLLibMaterial* MaterialCache = {read=FMaterialCache};
	__property int LightMapIndex = {read=FLightMapIndex, write=FLightMapIndex, nodefault};
public:
	/* TPersistentObject.Create */ inline __fastcall virtual TFaceGroup(void) : Persistentclasses::TPersistentObject() { }
	/* TPersistentObject.CreateFromFiler */ inline __fastcall TFaceGroup(Persistentclasses::TVirtualReader* reader) : Persistentclasses::TPersistentObject(reader) { }
	
};

#pragma pack(pop)

enum TFaceGroupMeshMode : unsigned char { fgmmTriangles, fgmmTriangleStrip, fgmmFlatTriangles, fgmmTriangleFan, fgmmQuads };

class DELPHICLASS TFGVertexIndexList;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TFGVertexIndexList : public TFaceGroup
{
	typedef TFaceGroup inherited;
	
private:
	Vectorlists::TIntegerList* FVertexIndices;
	Glcontext::TGLVBOElementArrayHandle* FIndexVBO;
	TFaceGroupMeshMode FMode;
	void __fastcall SetupVBO(void);
	void __fastcall InvalidateVBO(void);
	
protected:
	void __fastcall SetVertexIndices(Vectorlists::TIntegerList* const val);
	void __fastcall AddToList(Vectorlists::TAffineVectorList* source, Vectorlists::TAffineVectorList* destination, Vectorlists::TIntegerList* indices);
	
public:
	__fastcall virtual TFGVertexIndexList(void);
	__fastcall virtual ~TFGVertexIndexList(void);
	DYNAMIC void __fastcall WriteToFiler(Persistentclasses::TVirtualWriter* writer);
	DYNAMIC void __fastcall ReadFromFiler(Persistentclasses::TVirtualReader* reader);
	virtual void __fastcall BuildList(Glrendercontextinfo::TRenderContextInfo &mrci);
	DYNAMIC void __fastcall AddToTriangles(Vectorlists::TAffineVectorList* aList, Vectorlists::TAffineVectorList* aTexCoords = (Vectorlists::TAffineVectorList*)(0x0), Vectorlists::TAffineVectorList* aNormals = (Vectorlists::TAffineVectorList*)(0x0));
	DYNAMIC int __fastcall TriangleCount(void);
	DYNAMIC void __fastcall Reverse(void);
	void __fastcall Add(int idx);
	void __fastcall GetExtents(Vectortypes::TVector3f &min, Vectortypes::TVector3f &max);
	void __fastcall ConvertToList(void);
	Vectortypes::TVector3f __fastcall GetNormal(void);
	__property TFaceGroupMeshMode Mode = {read=FMode, write=FMode, nodefault};
	__property Vectorlists::TIntegerList* VertexIndices = {read=FVertexIndices, write=SetVertexIndices};
public:
	/* TFaceGroup.CreateOwned */ inline __fastcall virtual TFGVertexIndexList(TFaceGroups* AOwner) : TFaceGroup(AOwner) { }
	
public:
	/* TPersistentObject.CreateFromFiler */ inline __fastcall TFGVertexIndexList(Persistentclasses::TVirtualReader* reader) : TFaceGroup(reader) { }
	
};

#pragma pack(pop)

class DELPHICLASS TFGVertexNormalTexIndexList;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TFGVertexNormalTexIndexList : public TFGVertexIndexList
{
	typedef TFGVertexIndexList inherited;
	
private:
	Vectorlists::TIntegerList* FNormalIndices;
	Vectorlists::TIntegerList* FTexCoordIndices;
	
protected:
	void __fastcall SetNormalIndices(Vectorlists::TIntegerList* const val);
	void __fastcall SetTexCoordIndices(Vectorlists::TIntegerList* const val);
	
public:
	__fastcall virtual TFGVertexNormalTexIndexList(void);
	__fastcall virtual ~TFGVertexNormalTexIndexList(void);
	DYNAMIC void __fastcall WriteToFiler(Persistentclasses::TVirtualWriter* writer);
	DYNAMIC void __fastcall ReadFromFiler(Persistentclasses::TVirtualReader* reader);
	virtual void __fastcall BuildList(Glrendercontextinfo::TRenderContextInfo &mrci);
	DYNAMIC void __fastcall AddToTriangles(Vectorlists::TAffineVectorList* aList, Vectorlists::TAffineVectorList* aTexCoords = (Vectorlists::TAffineVectorList*)(0x0), Vectorlists::TAffineVectorList* aNormals = (Vectorlists::TAffineVectorList*)(0x0));
	HIDESBASE void __fastcall Add(int vertexIdx, int normalIdx, int texCoordIdx);
	__property Vectorlists::TIntegerList* NormalIndices = {read=FNormalIndices, write=SetNormalIndices};
	__property Vectorlists::TIntegerList* TexCoordIndices = {read=FTexCoordIndices, write=SetTexCoordIndices};
public:
	/* TFaceGroup.CreateOwned */ inline __fastcall virtual TFGVertexNormalTexIndexList(TFaceGroups* AOwner) : TFGVertexIndexList(AOwner) { }
	
public:
	/* TPersistentObject.CreateFromFiler */ inline __fastcall TFGVertexNormalTexIndexList(Persistentclasses::TVirtualReader* reader) : TFGVertexIndexList(reader) { }
	
};

#pragma pack(pop)

class DELPHICLASS TFGIndexTexCoordList;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TFGIndexTexCoordList : public TFGVertexIndexList
{
	typedef TFGVertexIndexList inherited;
	
private:
	Vectorlists::TAffineVectorList* FTexCoords;
	
protected:
	void __fastcall SetTexCoords(Vectorlists::TAffineVectorList* const val);
	
public:
	__fastcall virtual TFGIndexTexCoordList(void);
	__fastcall virtual ~TFGIndexTexCoordList(void);
	DYNAMIC void __fastcall WriteToFiler(Persistentclasses::TVirtualWriter* writer);
	DYNAMIC void __fastcall ReadFromFiler(Persistentclasses::TVirtualReader* reader);
	virtual void __fastcall BuildList(Glrendercontextinfo::TRenderContextInfo &mrci);
	DYNAMIC void __fastcall AddToTriangles(Vectorlists::TAffineVectorList* aList, Vectorlists::TAffineVectorList* aTexCoords = (Vectorlists::TAffineVectorList*)(0x0), Vectorlists::TAffineVectorList* aNormals = (Vectorlists::TAffineVectorList*)(0x0));
	HIDESBASE void __fastcall Add(int idx, const Vectortypes::TVector3f &texCoord)/* overload */;
	HIDESBASE void __fastcall Add(int idx, const float s, const float t)/* overload */;
	__property Vectorlists::TAffineVectorList* TexCoords = {read=FTexCoords, write=SetTexCoords};
public:
	/* TFaceGroup.CreateOwned */ inline __fastcall virtual TFGIndexTexCoordList(TFaceGroups* AOwner) : TFGVertexIndexList(AOwner) { }
	
public:
	/* TPersistentObject.CreateFromFiler */ inline __fastcall TFGIndexTexCoordList(Persistentclasses::TVirtualReader* reader) : TFGVertexIndexList(reader) { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TFaceGroups : public Persistentclasses::TPersistentObjectList
{
	typedef Persistentclasses::TPersistentObjectList inherited;
	
public:
	TFaceGroup* operator[](int Index) { return Items[Index]; }
	
private:
	TMeshObject* FOwner;
	
protected:
	TFaceGroup* __fastcall GetFaceGroup(int Index);
	
public:
	__fastcall TFaceGroups(TMeshObject* AOwner);
	__fastcall virtual ~TFaceGroups(void);
	DYNAMIC void __fastcall ReadFromFiler(Persistentclasses::TVirtualReader* reader);
	void __fastcall PrepareMaterialLibraryCache(Glmaterial::TGLMaterialLibrary* matLib);
	void __fastcall DropMaterialLibraryCache(void);
	__property TMeshObject* Owner = {read=FOwner};
	DYNAMIC void __fastcall Clear(void);
	__property TFaceGroup* Items[int Index] = {read=GetFaceGroup/*, default*/};
	void __fastcall AddToTriangles(Vectorlists::TAffineVectorList* aList, Vectorlists::TAffineVectorList* aTexCoords = (Vectorlists::TAffineVectorList*)(0x0), Vectorlists::TAffineVectorList* aNormals = (Vectorlists::TAffineVectorList*)(0x0));
	Glmaterial::TGLMaterialLibrary* __fastcall MaterialLibrary(void);
	void __fastcall SortByMaterial(void);
public:
	/* TPersistentObjectList.Create */ inline __fastcall virtual TFaceGroups(void) : Persistentclasses::TPersistentObjectList() { }
	
public:
	/* TPersistentObject.CreateFromFiler */ inline __fastcall TFaceGroups(Persistentclasses::TVirtualReader* reader) : Persistentclasses::TPersistentObjectList(reader) { }
	
};

#pragma pack(pop)

enum TMeshNormalsOrientation : unsigned char { mnoDefault, mnoInvert };

class DELPHICLASS TVectorFile;
class PASCALIMPLEMENTATION TVectorFile : public Applicationfileio::TDataFile
{
	typedef Applicationfileio::TDataFile inherited;
	
private:
	TMeshNormalsOrientation FNormalsOrientation;
	
protected:
	virtual void __fastcall SetNormalsOrientation(const TMeshNormalsOrientation val);
	
public:
	__fastcall virtual TVectorFile(System::Classes::TPersistent* AOwner);
	HIDESBASE TGLBaseMesh* __fastcall Owner(void);
	__property TMeshNormalsOrientation NormalsOrientation = {read=FNormalsOrientation, write=SetNormalsOrientation, nodefault};
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TVectorFile(void) { }
	
};


typedef System::TMetaClass* TVectorFileClass;

class DELPHICLASS TGLGLSMVectorFile;
class PASCALIMPLEMENTATION TGLGLSMVectorFile : public TVectorFile
{
	typedef TVectorFile inherited;
	
public:
	__classmethod virtual Applicationfileio::TDataFileCapabilities __fastcall Capabilities();
	DYNAMIC void __fastcall LoadFromStream(System::Classes::TStream* aStream);
	DYNAMIC void __fastcall SaveToStream(System::Classes::TStream* aStream);
public:
	/* TVectorFile.Create */ inline __fastcall virtual TGLGLSMVectorFile(System::Classes::TPersistent* AOwner) : TVectorFile(AOwner) { }
	
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TGLGLSMVectorFile(void) { }
	
};


class PASCALIMPLEMENTATION TGLBaseMesh : public Glscene::TGLSceneObject
{
	typedef Glscene::TGLSceneObject inherited;
	
private:
	TMeshNormalsOrientation FNormalsOrientation;
	Glmaterial::TGLMaterialLibrary* FMaterialLibrary;
	Glmaterial::TGLMaterialLibrary* FLightmapLibrary;
	Vectortypes::TVector4f FAxisAlignedDimensionsCache;
	bool FBaryCenterOffsetChanged;
	Vectortypes::TVector4f FBaryCenterOffset;
	bool FUseMeshMaterials;
	bool FOverlaySkeleton;
	bool FIgnoreMissingTextures;
	TMeshAutoCenterings FAutoCentering;
	Glcoordinates::TGLCoordinates3* FAutoScaling;
	bool FMaterialLibraryCachesPrepared;
	System::TObject* FConnectivity;
	System::UnicodeString FLastLoadedFilename;
	
protected:
	TMeshObjectList* FMeshObjects;
	TSkeleton* FSkeleton;
	void __fastcall SetUseMeshMaterials(const bool val);
	void __fastcall SetMaterialLibrary(Glmaterial::TGLMaterialLibrary* const val);
	void __fastcall SetLightmapLibrary(Glmaterial::TGLMaterialLibrary* const val);
	void __fastcall SetNormalsOrientation(const TMeshNormalsOrientation val);
	void __fastcall SetOverlaySkeleton(const bool val);
	void __fastcall SetAutoScaling(Glcoordinates::TGLCoordinates3* const Value);
	DYNAMIC void __fastcall DestroyHandle(void);
	DYNAMIC void __fastcall PrepareVectorFile(TVectorFile* aFile);
	DYNAMIC void __fastcall PrepareMesh(void);
	void __fastcall PrepareMaterialLibraryCache(void);
	void __fastcall DropMaterialLibraryCache(void);
	DYNAMIC void __fastcall PrepareBuildList(Glrendercontextinfo::TRenderContextInfo &mrci);
	
public:
	__fastcall virtual TGLBaseMesh(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TGLBaseMesh(void);
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	virtual void __fastcall Notification(System::Classes::TComponent* AComponent, System::Classes::TOperation Operation);
	virtual Vectortypes::TVector4f __fastcall AxisAlignedDimensionsUnscaled(void);
	Vectortypes::TVector4f __fastcall BarycenterOffset(void);
	Vectortypes::TVector4f __fastcall BarycenterPosition(void);
	virtual Vectortypes::TVector4f __fastcall BarycenterAbsolutePosition(void);
	virtual void __fastcall BuildList(Glrendercontextinfo::TRenderContextInfo &rci);
	virtual void __fastcall DoRender(Glrendercontextinfo::TRenderContextInfo &rci, bool renderSelf, bool renderChildren);
	DYNAMIC void __fastcall StructureChanged(void);
	void __fastcall StructureChangedNoPrepare(void);
	virtual bool __fastcall RayCastIntersect(const Vectortypes::TVector4f &rayStart, const Vectortypes::TVector4f &rayVector, Vectorgeometry::PVector intersectPoint = (Vectorgeometry::PVector)(0x0), Vectorgeometry::PVector intersectNormal = (Vectorgeometry::PVector)(0x0));
	virtual Glsilhouette::TGLSilhouette* __fastcall GenerateSilhouette(const Glsilhouette::TGLSilhouetteParameters &silhouetteParameters);
	void __fastcall BuildSilhouetteConnectivityData(void);
	__property TMeshObjectList* MeshObjects = {read=FMeshObjects};
	__property TSkeleton* Skeleton = {read=FSkeleton};
	void __fastcall GetExtents(/* out */ Vectortypes::TVector3f &min, /* out */ Vectortypes::TVector3f &max);
	Vectortypes::TVector3f __fastcall GetBarycenter(void);
	DYNAMIC void __fastcall PerformAutoCentering(void);
	DYNAMIC void __fastcall PerformAutoScaling(void);
	DYNAMIC void __fastcall LoadFromFile(const System::UnicodeString filename);
	DYNAMIC void __fastcall LoadFromStream(const System::UnicodeString filename, System::Classes::TStream* aStream);
	DYNAMIC void __fastcall SaveToFile(const System::UnicodeString fileName);
	DYNAMIC void __fastcall SaveToStream(const System::UnicodeString fileName, System::Classes::TStream* aStream);
	DYNAMIC void __fastcall AddDataFromFile(const System::UnicodeString filename);
	DYNAMIC void __fastcall AddDataFromStream(const System::UnicodeString filename, System::Classes::TStream* aStream);
	System::UnicodeString __fastcall LastLoadedFilename(void);
	__property TMeshAutoCenterings AutoCentering = {read=FAutoCentering, write=FAutoCentering, default=0};
	__property Glcoordinates::TGLCoordinates3* AutoScaling = {read=FAutoScaling, write=FAutoScaling};
	__property Glmaterial::TGLMaterialLibrary* MaterialLibrary = {read=FMaterialLibrary, write=SetMaterialLibrary};
	__property bool UseMeshMaterials = {read=FUseMeshMaterials, write=SetUseMeshMaterials, default=1};
	__property Glmaterial::TGLMaterialLibrary* LightmapLibrary = {read=FLightmapLibrary, write=SetLightmapLibrary};
	__property bool IgnoreMissingTextures = {read=FIgnoreMissingTextures, write=FIgnoreMissingTextures, default=0};
	__property TMeshNormalsOrientation NormalsOrientation = {read=FNormalsOrientation, write=SetNormalsOrientation, default=0};
	__property bool OverlaySkeleton = {read=FOverlaySkeleton, write=SetOverlaySkeleton, default=0};
public:
	/* TGLBaseSceneObject.CreateAsChild */ inline __fastcall TGLBaseMesh(Glscene::TGLBaseSceneObject* aParentOwner) : Glscene::TGLSceneObject(aParentOwner) { }
	
};


class DELPHICLASS TGLFreeForm;
class PASCALIMPLEMENTATION TGLFreeForm : public TGLBaseMesh
{
	typedef TGLBaseMesh inherited;
	
private:
	Octree::TOctree* FOctree;
	
protected:
	Octree::TOctree* __fastcall GetOctree(void);
	
public:
	__fastcall virtual TGLFreeForm(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TGLFreeForm(void);
	bool __fastcall OctreeRayCastIntersect(const Vectortypes::TVector4f &rayStart, const Vectortypes::TVector4f &rayVector, Vectorgeometry::PVector intersectPoint = (Vectorgeometry::PVector)(0x0), Vectorgeometry::PVector intersectNormal = (Vectorgeometry::PVector)(0x0));
	bool __fastcall OctreeSphereSweepIntersect(const Vectortypes::TVector4f &rayStart, const Vectortypes::TVector4f &rayVector, const float velocity, const float radius, Vectorgeometry::PVector intersectPoint = (Vectorgeometry::PVector)(0x0), Vectorgeometry::PVector intersectNormal = (Vectorgeometry::PVector)(0x0));
	bool __fastcall OctreeTriangleIntersect(const Vectortypes::TVector3f &v1, const Vectortypes::TVector3f &v2, const Vectortypes::TVector3f &v3);
	bool __fastcall OctreePointInMesh(const Vectortypes::TVector4f &Point);
	bool __fastcall OctreeAABBIntersect(const Geometrybb::TAABB &AABB, const Vectortypes::TMatrix4f &objMatrix, const Vectortypes::TMatrix4f &invObjMatrix, Vectorlists::TAffineVectorList* triangles = (Vectorlists::TAffineVectorList*)(0x0));
	__property Octree::TOctree* Octree = {read=GetOctree};
	void __fastcall BuildOctree(int TreeDepth = 0x3);
	
__published:
	__property AutoCentering = {default=0};
	__property AutoScaling;
	__property MaterialLibrary;
	__property LightmapLibrary;
	__property UseMeshMaterials = {default=1};
	__property NormalsOrientation = {default=0};
public:
	/* TGLBaseSceneObject.CreateAsChild */ inline __fastcall TGLFreeForm(Glscene::TGLBaseSceneObject* aParentOwner) : TGLBaseMesh(aParentOwner) { }
	
};


enum TGLActorOption : unsigned char { aoSkeletonNormalizeNormals };

typedef System::Set<TGLActorOption, TGLActorOption::aoSkeletonNormalizeNormals, TGLActorOption::aoSkeletonNormalizeNormals>  TGLActorOptions;

enum TActorAnimationReference : unsigned char { aarMorph, aarSkeleton, aarNone };

class DELPHICLASS TActorAnimation;
class DELPHICLASS TGLActor;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TActorAnimation : public System::Classes::TCollectionItem
{
	typedef System::Classes::TCollectionItem inherited;
	
private:
	System::UnicodeString FName;
	int FStartFrame;
	int FEndFrame;
	TActorAnimationReference FReference;
	
protected:
	virtual System::UnicodeString __fastcall GetDisplayName(void);
	int __fastcall FrameCount(void);
	void __fastcall SetStartFrame(const int val);
	void __fastcall SetEndFrame(const int val);
	void __fastcall SetReference(TActorAnimationReference val);
	void __fastcall SetAsString(const System::UnicodeString val);
	System::UnicodeString __fastcall GetAsString(void);
	
public:
	__fastcall virtual TActorAnimation(System::Classes::TCollection* Collection);
	__fastcall virtual ~TActorAnimation(void);
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	__property System::UnicodeString AsString = {read=GetAsString, write=SetAsString};
	TGLActor* __fastcall OwnerActor(void);
	void __fastcall MakeSkeletalTranslationStatic(void);
	void __fastcall MakeSkeletalRotationDelta(void);
	
__published:
	__property System::UnicodeString Name = {read=FName, write=FName};
	__property int StartFrame = {read=FStartFrame, write=SetStartFrame, nodefault};
	__property int EndFrame = {read=FEndFrame, write=SetEndFrame, nodefault};
	__property TActorAnimationReference Reference = {read=FReference, write=SetReference, default=0};
};

#pragma pack(pop)

typedef System::UnicodeString TActorAnimationName;

class DELPHICLASS TActorAnimations;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TActorAnimations : public System::Classes::TCollection
{
	typedef System::Classes::TCollection inherited;
	
public:
	TActorAnimation* operator[](int index) { return Items[index]; }
	
private:
	TGLActor* FOwner;
	
protected:
	DYNAMIC System::Classes::TPersistent* __fastcall GetOwner(void);
	void __fastcall SetItems(int index, TActorAnimation* const val);
	TActorAnimation* __fastcall GetItems(int index);
	
public:
	__fastcall TActorAnimations(TGLActor* AOwner);
	HIDESBASE TActorAnimation* __fastcall Add(void);
	HIDESBASE TActorAnimation* __fastcall FindItemID(int ID);
	TActorAnimation* __fastcall FindName(const System::UnicodeString aName);
	TActorAnimation* __fastcall FindFrame(int aFrame, TActorAnimationReference aReference);
	void __fastcall SetToStrings(System::Classes::TStrings* aStrings);
	void __fastcall SaveToStream(System::Classes::TStream* aStream);
	void __fastcall LoadFromStream(System::Classes::TStream* aStream);
	void __fastcall SaveToFile(const System::UnicodeString fileName);
	void __fastcall LoadFromFile(const System::UnicodeString fileName);
	__property TActorAnimation* Items[int index] = {read=GetItems, write=SetItems/*, default*/};
	TActorAnimation* __fastcall Last(void);
public:
	/* TCollection.Destroy */ inline __fastcall virtual ~TActorAnimations(void) { }
	
};

#pragma pack(pop)

class DELPHICLASS TGLBaseAnimationControler;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLBaseAnimationControler : public System::Classes::TComponent
{
	typedef System::Classes::TComponent inherited;
	
private:
	bool FEnabled;
	TGLActor* FActor;
	
protected:
	virtual void __fastcall Notification(System::Classes::TComponent* AComponent, System::Classes::TOperation Operation);
	void __fastcall SetEnabled(const bool val);
	void __fastcall SetActor(TGLActor* const val);
	virtual void __fastcall DoChange(void);
	virtual bool __fastcall Apply(TBlendedLerpInfo &lerpInfo);
	
public:
	__fastcall virtual TGLBaseAnimationControler(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TGLBaseAnimationControler(void);
	
__published:
	__property bool Enabled = {read=FEnabled, write=SetEnabled, default=1};
	__property TGLActor* Actor = {read=FActor, write=SetActor};
};

#pragma pack(pop)

class DELPHICLASS TGLAnimationControler;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLAnimationControler : public TGLBaseAnimationControler
{
	typedef TGLBaseAnimationControler inherited;
	
private:
	System::UnicodeString FAnimationName;
	float FRatio;
	
protected:
	void __fastcall SetAnimationName(const System::UnicodeString val);
	void __fastcall SetRatio(const float val);
	virtual void __fastcall DoChange(void);
	virtual bool __fastcall Apply(TBlendedLerpInfo &lerpInfo);
	
__published:
	__property System::UnicodeString AnimationName = {read=FAnimationName, write=SetAnimationName};
	__property float Ratio = {read=FRatio, write=SetRatio};
public:
	/* TGLBaseAnimationControler.Create */ inline __fastcall virtual TGLAnimationControler(System::Classes::TComponent* AOwner) : TGLBaseAnimationControler(AOwner) { }
	/* TGLBaseAnimationControler.Destroy */ inline __fastcall virtual ~TGLAnimationControler(void) { }
	
};

#pragma pack(pop)

enum TActorFrameInterpolation : unsigned char { afpNone, afpLinear };

enum TActorAnimationMode : unsigned char { aamNone, aamPlayOnce, aamLoop, aamBounceForward, aamBounceBackward, aamLoopBackward, aamExternal };

class PASCALIMPLEMENTATION TGLActor : public TGLBaseMesh
{
	typedef TGLBaseMesh inherited;
	
private:
	int FStartFrame;
	int FEndFrame;
	TActorAnimationReference FReference;
	int FCurrentFrame;
	float FCurrentFrameDelta;
	TActorFrameInterpolation FFrameInterpolation;
	int FInterval;
	TActorAnimationMode FAnimationMode;
	System::Classes::TNotifyEvent FOnFrameChanged;
	System::Classes::TNotifyEvent FOnEndFrameReached;
	System::Classes::TNotifyEvent FOnStartFrameReached;
	TActorAnimations* FAnimations;
	TActorAnimation* FTargetSmoothAnimation;
	System::Classes::TList* FControlers;
	TGLActorOptions FOptions;
	
protected:
	void __fastcall SetCurrentFrame(int val);
	void __fastcall SetStartFrame(int val);
	void __fastcall SetEndFrame(int val);
	HIDESBASE void __fastcall SetReference(TActorAnimationReference val);
	void __fastcall SetAnimations(TActorAnimations* const val);
	bool __fastcall StoreAnimations(void);
	void __fastcall SetOptions(const TGLActorOptions val);
	DYNAMIC void __fastcall PrepareMesh(void);
	DYNAMIC void __fastcall PrepareBuildList(Glrendercontextinfo::TRenderContextInfo &mrci);
	virtual void __fastcall DoAnimate(void);
	void __fastcall RegisterControler(TGLBaseAnimationControler* aControler);
	void __fastcall UnRegisterControler(TGLBaseAnimationControler* aControler);
	
public:
	__fastcall virtual TGLActor(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TGLActor(void);
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	virtual void __fastcall BuildList(Glrendercontextinfo::TRenderContextInfo &rci);
	virtual void __fastcall DoProgress(const Baseclasses::TProgressTimes &progressTime);
	DYNAMIC void __fastcall LoadFromStream(const System::UnicodeString filename, System::Classes::TStream* aStream);
	void __fastcall SwitchToAnimation(TActorAnimation* anAnimation, bool smooth = false)/* overload */;
	void __fastcall SwitchToAnimation(const System::UnicodeString animationName, bool smooth = false)/* overload */;
	void __fastcall SwitchToAnimation(int animationIndex, bool smooth = false)/* overload */;
	System::UnicodeString __fastcall CurrentAnimation(void);
	void __fastcall Synchronize(TGLActor* referenceActor);
	void __fastcall SetCurrentFrameDirect(const int Value);
	int __fastcall NextFrameIndex(void);
	void __fastcall NextFrame(int nbSteps = 0x1);
	void __fastcall PrevFrame(int nbSteps = 0x1);
	int __fastcall FrameCount(void);
	bool __fastcall isSwitchingAnimation(void);
	
__published:
	__property int StartFrame = {read=FStartFrame, write=SetStartFrame, default=0};
	__property int EndFrame = {read=FEndFrame, write=SetEndFrame, default=0};
	__property TActorAnimationReference Reference = {read=FReference, write=FReference, default=0};
	__property int CurrentFrame = {read=FCurrentFrame, write=SetCurrentFrame, default=0};
	__property float CurrentFrameDelta = {read=FCurrentFrameDelta, write=FCurrentFrameDelta};
	__property TActorFrameInterpolation FrameInterpolation = {read=FFrameInterpolation, write=FFrameInterpolation, default=1};
	__property TActorAnimationMode AnimationMode = {read=FAnimationMode, write=FAnimationMode, default=0};
	__property int Interval = {read=FInterval, write=FInterval, nodefault};
	__property TGLActorOptions Options = {read=FOptions, write=SetOptions, default=1};
	__property System::Classes::TNotifyEvent OnFrameChanged = {read=FOnFrameChanged, write=FOnFrameChanged};
	__property System::Classes::TNotifyEvent OnEndFrameReached = {read=FOnEndFrameReached, write=FOnEndFrameReached};
	__property System::Classes::TNotifyEvent OnStartFrameReached = {read=FOnStartFrameReached, write=FOnStartFrameReached};
	__property TActorAnimations* Animations = {read=FAnimations, write=SetAnimations, stored=StoreAnimations};
	__property AutoCentering = {default=0};
	__property MaterialLibrary;
	__property LightmapLibrary;
	__property UseMeshMaterials = {default=1};
	__property NormalsOrientation = {default=0};
	__property OverlaySkeleton = {default=0};
public:
	/* TGLBaseSceneObject.CreateAsChild */ inline __fastcall TGLActor(Glscene::TGLBaseSceneObject* aParentOwner) : TGLBaseMesh(aParentOwner) { }
	
};


class DELPHICLASS TVectorFileFormat;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TVectorFileFormat : public System::TObject
{
	typedef System::TObject inherited;
	
public:
	TVectorFileClass VectorFileClass;
	System::UnicodeString Extension;
	System::UnicodeString Description;
	int DescResID;
public:
	/* TObject.Create */ inline __fastcall TVectorFileFormat(void) : System::TObject() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TVectorFileFormat(void) { }
	
};

#pragma pack(pop)

class DELPHICLASS TVectorFileFormatsList;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TVectorFileFormatsList : public Persistentclasses::TPersistentObjectList
{
	typedef Persistentclasses::TPersistentObjectList inherited;
	
public:
	__fastcall virtual ~TVectorFileFormatsList(void);
	HIDESBASE void __fastcall Add(const System::UnicodeString Ext, const System::UnicodeString Desc, int DescID, TVectorFileClass AClass);
	TVectorFileClass __fastcall FindExt(System::UnicodeString ext);
	TVectorFileClass __fastcall FindFromFileName(const System::UnicodeString fileName);
	HIDESBASE void __fastcall Remove(TVectorFileClass AClass);
	void __fastcall BuildFilterStrings(TVectorFileClass vectorFileClass, /* out */ System::UnicodeString &descriptions, /* out */ System::UnicodeString &filters, bool formatsThatCanBeOpened = true, bool formatsThatCanBeSaved = false);
	System::UnicodeString __fastcall FindExtByIndex(int index, bool formatsThatCanBeOpened = true, bool formatsThatCanBeSaved = false);
public:
	/* TPersistentObjectList.Create */ inline __fastcall virtual TVectorFileFormatsList(void) : Persistentclasses::TPersistentObjectList() { }
	
public:
	/* TPersistentObject.CreateFromFiler */ inline __fastcall TVectorFileFormatsList(Persistentclasses::TVirtualReader* reader) : Persistentclasses::TPersistentObjectList(reader) { }
	
};

#pragma pack(pop)

class DELPHICLASS EInvalidVectorFile;
#pragma pack(push,4)
class PASCALIMPLEMENTATION EInvalidVectorFile : public System::Sysutils::Exception
{
	typedef System::Sysutils::Exception inherited;
	
public:
	/* Exception.Create */ inline __fastcall EInvalidVectorFile(const System::UnicodeString Msg) : System::Sysutils::Exception(Msg) { }
	/* Exception.CreateFmt */ inline __fastcall EInvalidVectorFile(const System::UnicodeString Msg, System::TVarRec const *Args, const int Args_Size) : System::Sysutils::Exception(Msg, Args, Args_Size) { }
	/* Exception.CreateRes */ inline __fastcall EInvalidVectorFile(NativeUInt Ident)/* overload */ : System::Sysutils::Exception(Ident) { }
	/* Exception.CreateRes */ inline __fastcall EInvalidVectorFile(System::PResStringRec ResStringRec)/* overload */ : System::Sysutils::Exception(ResStringRec) { }
	/* Exception.CreateResFmt */ inline __fastcall EInvalidVectorFile(NativeUInt Ident, System::TVarRec const *Args, const int Args_Size)/* overload */ : System::Sysutils::Exception(Ident, Args, Args_Size) { }
	/* Exception.CreateResFmt */ inline __fastcall EInvalidVectorFile(System::PResStringRec ResStringRec, System::TVarRec const *Args, const int Args_Size)/* overload */ : System::Sysutils::Exception(ResStringRec, Args, Args_Size) { }
	/* Exception.CreateHelp */ inline __fastcall EInvalidVectorFile(const System::UnicodeString Msg, int AHelpContext) : System::Sysutils::Exception(Msg, AHelpContext) { }
	/* Exception.CreateFmtHelp */ inline __fastcall EInvalidVectorFile(const System::UnicodeString Msg, System::TVarRec const *Args, const int Args_Size, int AHelpContext) : System::Sysutils::Exception(Msg, Args, Args_Size, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall EInvalidVectorFile(NativeUInt Ident, int AHelpContext)/* overload */ : System::Sysutils::Exception(Ident, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall EInvalidVectorFile(System::PResStringRec ResStringRec, int AHelpContext)/* overload */ : System::Sysutils::Exception(ResStringRec, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall EInvalidVectorFile(System::PResStringRec ResStringRec, System::TVarRec const *Args, const int Args_Size, int AHelpContext)/* overload */ : System::Sysutils::Exception(ResStringRec, Args, Args_Size, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall EInvalidVectorFile(NativeUInt Ident, System::TVarRec const *Args, const int Args_Size, int AHelpContext)/* overload */ : System::Sysutils::Exception(Ident, Args, Args_Size, AHelpContext) { }
	/* Exception.Destroy */ inline __fastcall virtual ~EInvalidVectorFile(void) { }
	
};

#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------
#define cDefaultGLActorOptions (System::Set<TGLActorOption, TGLActorOption::aoSkeletonNormalizeNormals, TGLActorOption::aoSkeletonNormalizeNormals> () << TGLActorOption::aoSkeletonNormalizeNormals )
extern PACKAGE bool vGLVectorFileObjectsAllocateMaterials;
extern PACKAGE bool vGLVectorFileObjectsEnableVBOByDefault;
extern PACKAGE TVectorFileFormatsList* __fastcall GetVectorFileFormats(void);
extern PACKAGE System::UnicodeString __fastcall VectorFileFormatsFilter(void);
extern PACKAGE System::UnicodeString __fastcall VectorFileFormatsSaveFilter(void);
extern PACKAGE void __fastcall RegisterVectorFileFormat(const System::UnicodeString aExtension, const System::UnicodeString aDescription, TVectorFileClass aClass);
extern PACKAGE void __fastcall UnregisterVectorFileClass(TVectorFileClass aClass);
extern PACKAGE System::UnicodeString __fastcall VectorFileFormatExtensionByIndex(int index);
}	/* namespace Glvectorfileobjects */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLVECTORFILEOBJECTS)
using namespace Glvectorfileobjects;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// GlvectorfileobjectsHPP
