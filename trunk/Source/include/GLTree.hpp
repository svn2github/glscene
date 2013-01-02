// CodeGear C++Builder
// Copyright (c) 1995, 2012 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLTree.pas' rev: 24.00 (Win32)

#ifndef GltreeHPP
#define GltreeHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <System.Classes.hpp>	// Pascal unit
#include <System.SysUtils.hpp>	// Pascal unit
#include <GLScene.hpp>	// Pascal unit
#include <GLMaterial.hpp>	// Pascal unit
#include <VectorGeometry.hpp>	// Pascal unit
#include <VectorLists.hpp>	// Pascal unit
#include <OpenGLTokens.hpp>	// Pascal unit
#include <GLVectorFileObjects.hpp>	// Pascal unit
#include <ApplicationFileIO.hpp>	// Pascal unit
#include <GLRenderContextInfo.hpp>	// Pascal unit
#include <VectorTypes.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Gltree
{
//-- type declarations -------------------------------------------------------
class DELPHICLASS TGLTreeLeaves;
class DELPHICLASS TGLTree;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLTreeLeaves : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	TGLTree* FOwner;
	int FCount;
	Vectorlists::TAffineVectorList* FVertices;
	Vectorlists::TAffineVectorList* FNormals;
	Vectorlists::TAffineVectorList* FTexCoords;
	
public:
	__fastcall TGLTreeLeaves(TGLTree* AOwner);
	__fastcall virtual ~TGLTreeLeaves(void);
	void __fastcall BuildList(Glrendercontextinfo::TRenderContextInfo &rci);
	void __fastcall AddNew(const Vectortypes::TMatrix4f &matrix);
	void __fastcall Clear(void);
	__property TGLTree* Owner = {read=FOwner};
	__property int Count = {read=FCount, nodefault};
	__property Vectorlists::TAffineVectorList* Vertices = {read=FVertices};
	__property Vectorlists::TAffineVectorList* Normals = {read=FNormals};
	__property Vectorlists::TAffineVectorList* TexCoords = {read=FTexCoords};
};

#pragma pack(pop)

class DELPHICLASS TGLTreeBranch;
class DELPHICLASS TGLTreeBranches;
class DELPHICLASS TGLTreeBranchNoise;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLTreeBranch : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	TGLTreeBranches* FOwner;
	TGLTreeBranch* FLeft;
	TGLTreeBranch* FCenter;
	TGLTreeBranch* FRight;
	TGLTreeBranch* FParent;
	int FBranchID;
	int FParentID;
	Vectortypes::TMatrix4f FMatrix;
	Vectorlists::TIntegerList* FLower;
	Vectorlists::TIntegerList* FUpper;
	bool FCentralLeader;
	void __fastcall BuildBranch(TGLTreeBranchNoise* branchNoise, const Vectortypes::TMatrix4f &Matrix, float TexCoordY, float Twist, int Level);
	
public:
	__fastcall TGLTreeBranch(TGLTreeBranches* AOwner, TGLTreeBranch* AParent);
	__fastcall virtual ~TGLTreeBranch(void);
	__property TGLTreeBranches* Owner = {read=FOwner};
	__property TGLTreeBranch* Left = {read=FLeft};
	__property TGLTreeBranch* Center = {read=FCenter};
	__property TGLTreeBranch* Right = {read=FRight};
	__property TGLTreeBranch* Parent = {read=FParent};
	__property Vectortypes::TMatrix4f Matrix = {read=FMatrix};
	__property Vectorlists::TIntegerList* Lower = {read=FLower};
	__property Vectorlists::TIntegerList* Upper = {read=FUpper};
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLTreeBranches : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	TGLTree* FOwner;
	Vectorlists::TSingleList* FSinList;
	Vectorlists::TSingleList* FCosList;
	Vectorlists::TAffineVectorList* FVertices;
	Vectorlists::TAffineVectorList* FNormals;
	Vectorlists::TAffineVectorList* FTexCoords;
	Vectorlists::TIntegerList* FIndices;
	TGLTreeBranch* FRoot;
	int FCount;
	System::Classes::TList* FBranchCache;
	Vectorlists::TIntegerList* FBranchIndices;
	void __fastcall BuildBranches(void);
	
public:
	__fastcall TGLTreeBranches(TGLTree* AOwner);
	__fastcall virtual ~TGLTreeBranches(void);
	void __fastcall BuildList(Glrendercontextinfo::TRenderContextInfo &rci);
	void __fastcall Clear(void);
	__property TGLTree* Owner = {read=FOwner};
	__property Vectorlists::TSingleList* SinList = {read=FSinList};
	__property Vectorlists::TSingleList* CosList = {read=FCosList};
	__property Vectorlists::TAffineVectorList* Vertices = {read=FVertices};
	__property Vectorlists::TAffineVectorList* Normals = {read=FNormals};
	__property Vectorlists::TAffineVectorList* TexCoords = {read=FTexCoords};
	__property int Count = {read=FCount, nodefault};
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLTreeBranchNoise : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	float FBranchNoise;
	TGLTreeBranchNoise* FLeft;
	TGLTreeBranchNoise* FRight;
	TGLTreeBranchNoise* FCenter;
	TGLTreeBranchNoise* __fastcall GetLeft(void);
	TGLTreeBranchNoise* __fastcall GetCenter(void);
	TGLTreeBranchNoise* __fastcall GetRight(void);
	
public:
	__fastcall TGLTreeBranchNoise(void);
	__fastcall virtual ~TGLTreeBranchNoise(void);
	__property TGLTreeBranchNoise* Left = {read=GetLeft};
	__property TGLTreeBranchNoise* Center = {read=GetCenter};
	__property TGLTreeBranchNoise* Right = {read=GetRight};
	__property float BranchNoise = {read=FBranchNoise};
};

#pragma pack(pop)

class PASCALIMPLEMENTATION TGLTree : public Glscene::TGLImmaterialSceneObject
{
	typedef Glscene::TGLImmaterialSceneObject inherited;
	
private:
	int FDepth;
	int FBranchFacets;
	float FLeafSize;
	float FBranchSize;
	float FBranchNoise;
	float FBranchAngleBias;
	float FBranchAngle;
	float FBranchTwist;
	float FBranchRadius;
	float FLeafThreshold;
	float FCentralLeaderBias;
	bool FCentralLeader;
	int FSeed;
	bool FAutoCenter;
	bool FAutoRebuild;
	float FCenterBranchConstant;
	TGLTreeLeaves* FLeaves;
	TGLTreeBranches* FBranches;
	TGLTreeBranchNoise* FNoise;
	Glmaterial::TGLMaterialLibrary* FMaterialLibrary;
	System::UnicodeString FLeafMaterialName;
	System::UnicodeString FLeafBackMaterialName;
	System::UnicodeString FBranchMaterialName;
	bool FRebuildTree;
	Vectortypes::TVector4f FAxisAlignedDimensionsCache;
	
protected:
	void __fastcall SetDepth(const int Value);
	void __fastcall SetBranchFacets(const int Value);
	void __fastcall SetLeafSize(const float Value);
	void __fastcall SetBranchSize(const float Value);
	void __fastcall SetBranchNoise(const float Value);
	void __fastcall SetBranchAngleBias(const float Value);
	void __fastcall SetBranchAngle(const float Value);
	void __fastcall SetBranchTwist(const float Value);
	void __fastcall SetBranchRadius(const float Value);
	void __fastcall SetLeafThreshold(const float Value);
	void __fastcall SetCentralLeaderBias(const float Value);
	void __fastcall SetCentralLeader(const bool Value);
	void __fastcall SetSeed(const int Value);
	void __fastcall SetAutoCenter(const bool Value);
	void __fastcall SetAutoRebuild(const bool Value);
	void __fastcall SetCenterBranchConstant(const float Value);
	void __fastcall SetMaterialLibrary(Glmaterial::TGLMaterialLibrary* const Value);
	void __fastcall SetLeafMaterialName(const System::UnicodeString Value);
	void __fastcall SetLeafBackMaterialName(const System::UnicodeString Value);
	void __fastcall SetBranchMaterialName(const System::UnicodeString Value);
	virtual void __fastcall Loaded(void);
	
public:
	__fastcall virtual TGLTree(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TGLTree(void);
	virtual void __fastcall Notification(System::Classes::TComponent* AComponent, System::Classes::TOperation Operation);
	virtual void __fastcall DoRender(Glrendercontextinfo::TRenderContextInfo &ARci, bool ARenderSelf, bool ARenderChildren);
	virtual void __fastcall BuildList(Glrendercontextinfo::TRenderContextInfo &rci);
	DYNAMIC void __fastcall StructureChanged(void);
	void __fastcall BuildMesh(Glvectorfileobjects::TGLBaseMesh* GLBaseMesh);
	void __fastcall RebuildTree(void);
	void __fastcall ForceTotalRebuild(void);
	void __fastcall Clear(void);
	void __fastcall GetExtents(Vectortypes::TVector3f &min, Vectortypes::TVector3f &max);
	virtual Vectortypes::TVector4f __fastcall AxisAlignedDimensionsUnscaled(void);
	void __fastcall LoadFromStream(System::Classes::TStream* aStream);
	void __fastcall SaveToStream(System::Classes::TStream* aStream);
	void __fastcall LoadFromFile(System::UnicodeString aFileName);
	void __fastcall SaveToFile(System::UnicodeString aFileName);
	__property TGLTreeLeaves* Leaves = {read=FLeaves};
	__property TGLTreeBranches* Branches = {read=FBranches};
	__property TGLTreeBranchNoise* Noise = {read=FNoise};
	
__published:
	__property int Depth = {read=FDepth, write=SetDepth, nodefault};
	__property int BranchFacets = {read=FBranchFacets, write=SetBranchFacets, nodefault};
	__property float LeafSize = {read=FLeafSize, write=SetLeafSize};
	__property float BranchSize = {read=FBranchSize, write=SetBranchSize};
	__property float BranchNoise = {read=FBranchNoise, write=SetBranchNoise};
	__property float BranchAngleBias = {read=FBranchAngleBias, write=SetBranchAngleBias};
	__property float BranchAngle = {read=FBranchAngle, write=SetBranchAngle};
	__property float BranchTwist = {read=FBranchTwist, write=SetBranchTwist};
	__property float BranchRadius = {read=FBranchRadius, write=SetBranchRadius};
	__property float LeafThreshold = {read=FLeafThreshold, write=SetLeafThreshold};
	__property float CentralLeaderBias = {read=FCentralLeaderBias, write=SetCentralLeaderBias};
	__property bool CentralLeader = {read=FCentralLeader, write=SetCentralLeader, nodefault};
	__property int Seed = {read=FSeed, write=SetSeed, nodefault};
	__property bool AutoCenter = {read=FAutoCenter, write=SetAutoCenter, nodefault};
	__property bool AutoRebuild = {read=FAutoRebuild, write=SetAutoRebuild, nodefault};
	__property float CenterBranchConstant = {read=FCenterBranchConstant, write=SetCenterBranchConstant};
	__property Glmaterial::TGLMaterialLibrary* MaterialLibrary = {read=FMaterialLibrary, write=SetMaterialLibrary};
	__property System::UnicodeString LeafMaterialName = {read=FLeafMaterialName, write=SetLeafMaterialName};
	__property System::UnicodeString LeafBackMaterialName = {read=FLeafBackMaterialName, write=SetLeafBackMaterialName};
	__property System::UnicodeString BranchMaterialName = {read=FBranchMaterialName, write=SetBranchMaterialName};
public:
	/* TGLBaseSceneObject.CreateAsChild */ inline __fastcall TGLTree(Glscene::TGLBaseSceneObject* aParentOwner) : Glscene::TGLImmaterialSceneObject(aParentOwner) { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Gltree */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLTREE)
using namespace Gltree;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// GltreeHPP
