// CodeGear C++Builder
// Copyright (c) 1995, 2012 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLRagdoll.pas' rev: 24.00 (Win32)

#ifndef GlragdollHPP
#define GlragdollHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <GLScene.hpp>	// Pascal unit
#include <PersistentClasses.hpp>	// Pascal unit
#include <VectorGeometry.hpp>	// Pascal unit
#include <GLVectorFileObjects.hpp>	// Pascal unit
#include <VectorLists.hpp>	// Pascal unit
#include <GLObjects.hpp>	// Pascal unit
#include <System.Classes.hpp>	// Pascal unit
#include <VectorTypes.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Glragdoll
{
//-- type declarations -------------------------------------------------------
class DELPHICLASS TRagdollJoint;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TRagdollJoint : public System::TObject
{
	typedef System::TObject inherited;
	
public:
	/* TObject.Create */ inline __fastcall TRagdollJoint(void) : System::TObject() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TRagdollJoint(void) { }
	
};

#pragma pack(pop)

class DELPHICLASS TRagdollBoneList;
class DELPHICLASS TGLRagdoll;
class DELPHICLASS TRagdollBone;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TRagdollBoneList : public Persistentclasses::TPersistentObjectList
{
	typedef Persistentclasses::TPersistentObjectList inherited;
	
public:
	TRagdollBone* operator[](int Index) { return Items[Index]; }
	
private:
	TGLRagdoll* FRagdoll;
	
protected:
	TRagdollBone* __fastcall GetRagdollBone(int Index);
	
public:
	__fastcall TRagdollBoneList(TGLRagdoll* Ragdoll);
	__fastcall virtual ~TRagdollBoneList(void);
	DYNAMIC void __fastcall WriteToFiler(Persistentclasses::TVirtualWriter* writer);
	DYNAMIC void __fastcall ReadFromFiler(Persistentclasses::TVirtualReader* reader);
	__property TGLRagdoll* Ragdoll = {read=FRagdoll};
	__property TRagdollBone* Items[int Index] = {read=GetRagdollBone/*, default*/};
public:
	/* TPersistentObject.CreateFromFiler */ inline __fastcall TRagdollBoneList(Persistentclasses::TVirtualReader* reader) : Persistentclasses::TPersistentObjectList(reader) { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TRagdollBone : public TRagdollBoneList
{
	typedef TRagdollBoneList inherited;
	
public:
	TRagdollBone* operator[](int Index) { return Items[Index]; }
	
private:
	TRagdollBoneList* FOwner;
	System::UnicodeString FName;
	int FBoneID;
	Vectortypes::TVector3f FBoundMax;
	Vectortypes::TVector3f FBoundMin;
	Vectortypes::TVector3f FBoundBoneDelta;
	Vectortypes::TVector3f FOrigin;
	Vectortypes::TVector3f FSize;
	Vectortypes::TMatrix4f FBoneMatrix;
	TRagdollJoint* FJoint;
	Vectortypes::TMatrix4f FOriginalMatrix;
	Vectortypes::TMatrix4f FReferenceMatrix;
	Vectortypes::TVector3f FAnchor;
	void __fastcall CreateBoundingBox(void);
	void __fastcall SetAnchor(const Vectortypes::TVector3f &Anchor);
	void __fastcall AlignToSkeleton(void);
	void __fastcall CreateBoundsChild(void);
	void __fastcall StartChild(void);
	void __fastcall AlignChild(void);
	void __fastcall UpdateChild(void);
	void __fastcall StopChild(void);
	
protected:
	HIDESBASE TRagdollBone* __fastcall GetRagdollBone(int Index);
	virtual void __fastcall Start(void) = 0 ;
	virtual void __fastcall Align(void) = 0 ;
	virtual void __fastcall Update(void) = 0 ;
	virtual void __fastcall Stop(void) = 0 ;
	
public:
	__fastcall TRagdollBone(TRagdollBoneList* aOwner);
	__fastcall TRagdollBone(TGLRagdoll* Ragdoll);
	__fastcall virtual ~TRagdollBone(void);
	DYNAMIC void __fastcall WriteToFiler(Persistentclasses::TVirtualWriter* writer);
	DYNAMIC void __fastcall ReadFromFiler(Persistentclasses::TVirtualReader* reader);
	__property TRagdollBoneList* Owner = {read=FOwner};
	__property System::UnicodeString Name = {read=FName, write=FName};
	__property int BoneID = {read=FBoneID, write=FBoneID, nodefault};
	__property Vectortypes::TVector3f Origin = {read=FOrigin};
	__property Vectortypes::TVector3f Size = {read=FSize};
	__property Vectortypes::TMatrix4f BoneMatrix = {read=FBoneMatrix};
	__property Vectortypes::TMatrix4f ReferenceMatrix = {read=FReferenceMatrix};
	__property Vectortypes::TVector3f Anchor = {read=FAnchor};
	__property TRagdollJoint* Joint = {read=FJoint, write=FJoint};
	__property TRagdollBone* Items[int Index] = {read=GetRagdollBone/*, default*/};
public:
	/* TPersistentObject.CreateFromFiler */ inline __fastcall TRagdollBone(Persistentclasses::TVirtualReader* reader) : TRagdollBoneList(reader) { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLRagdoll : public Persistentclasses::TPersistentObject
{
	typedef Persistentclasses::TPersistentObject inherited;
	
private:
	Glvectorfileobjects::TGLBaseMesh* FOwner;
	TRagdollBone* FRootBone;
	bool FEnabled;
	bool FBuilt;
	
public:
	__fastcall TGLRagdoll(Glvectorfileobjects::TGLBaseMesh* AOwner);
	__fastcall virtual ~TGLRagdoll(void);
	DYNAMIC void __fastcall WriteToFiler(Persistentclasses::TVirtualWriter* writer);
	DYNAMIC void __fastcall ReadFromFiler(Persistentclasses::TVirtualReader* reader);
	void __fastcall SetRootBone(TRagdollBone* RootBone);
	void __fastcall BuildRagdoll(void);
	void __fastcall Start(void);
	void __fastcall Update(void);
	void __fastcall Stop(void);
	__property Glvectorfileobjects::TGLBaseMesh* Owner = {read=FOwner};
	__property TRagdollBone* RootBone = {read=FRootBone};
	__property bool Enabled = {read=FEnabled, nodefault};
public:
	/* TPersistentObject.CreateFromFiler */ inline __fastcall TGLRagdoll(Persistentclasses::TVirtualReader* reader) : Persistentclasses::TPersistentObject(reader) { }
	
};

#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------
}	/* namespace Glragdoll */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLRAGDOLL)
using namespace Glragdoll;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// GlragdollHPP
