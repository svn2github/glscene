// CodeGear C++Builder
// Copyright (c) 1995, 2012 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLSelection.pas' rev: 24.00 (Win32)

#ifndef GlselectionHPP
#define GlselectionHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <System.SysUtils.hpp>	// Pascal unit
#include <System.Classes.hpp>	// Pascal unit
#include <OpenGLTokens.hpp>	// Pascal unit
#include <GLContext.hpp>	// Pascal unit
#include <VectorLists.hpp>	// Pascal unit
#include <VectorGeometry.hpp>	// Pascal unit
#include <BaseClasses.hpp>	// Pascal unit
#include <PersistentClasses.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Glselection
{
//-- type declarations -------------------------------------------------------
typedef System::DynamicArray<int> TPickSubObjects;

class DELPHICLASS TPickRecord;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TPickRecord : public System::TObject
{
	typedef System::TObject inherited;
	
public:
	Baseclasses::TGLUpdateAbleComponent* AObject;
	TPickSubObjects SubObjects;
	float ZMin;
	float ZMax;
public:
	/* TObject.Create */ inline __fastcall TPickRecord(void) : System::TObject() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TPickRecord(void) { }
	
};

#pragma pack(pop)

enum TPickSortType : unsigned char { psDefault, psName, psMinDepth, psMaxDepth };

class DELPHICLASS TGLPickList;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLPickList : public Persistentclasses::TPersistentObjectList
{
	typedef Persistentclasses::TPersistentObjectList inherited;
	
public:
	System::TObject* operator[](int Index) { return Hit[Index]; }
	
private:
	float __fastcall GetFar(int aValue);
	System::TObject* __fastcall GetHit(int aValue);
	float __fastcall GetNear(int aValue);
	TPickSubObjects __fastcall GetSubObjects(int aValue);
	
public:
	__fastcall TGLPickList(TPickSortType aSortType);
	void __fastcall AddHit(System::TObject* obj, const TPickSubObjects subObj, float zMin, float zMax);
	DYNAMIC void __fastcall Clear(void);
	int __fastcall FindObject(System::TObject* AObject);
	__property float FarDistance[int Index] = {read=GetFar};
	__property System::TObject* Hit[int Index] = {read=GetHit/*, default*/};
	__property float NearDistance[int Index] = {read=GetNear};
	__property TPickSubObjects SubObjects[int Index] = {read=GetSubObjects};
public:
	/* TPersistentObjectList.Destroy */ inline __fastcall virtual ~TGLPickList(void) { }
	
public:
	/* TPersistentObject.CreateFromFiler */ inline __fastcall TGLPickList(Persistentclasses::TVirtualReader* reader) : Persistentclasses::TPersistentObjectList(reader) { }
	
};

#pragma pack(pop)

class DELPHICLASS TGLBaseSelectTechnique;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLBaseSelectTechnique : public System::TObject
{
	typedef System::TObject inherited;
	
protected:
	System::StaticArray<System::TObject*, 4096> FObjectStack;
	System::StaticArray<unsigned, 256> FNameStack;
	unsigned FCurrentName;
	int FStackPosition;
	int FObjectCountGuess;
	int FHits;
	virtual System::TObject* __fastcall GetObject(void) = 0 ;
	virtual void __fastcall SetObject(System::TObject* Value) = 0 ;
	virtual int __fastcall GetHits(void) = 0 ;
	virtual void __fastcall SetHits(int Value) = 0 ;
	virtual void __fastcall SetObjectCountGuess(int Value) = 0 ;
	virtual System::TObject* __fastcall GetItems(int Value) = 0 ;
	
public:
	virtual __classmethod bool __fastcall IsSupported() = 0 ;
	virtual void __fastcall Start(void) = 0 ;
	virtual bool __fastcall Stop(void) = 0 ;
	virtual void __fastcall PushObject(System::TObject* AName) = 0 ;
	virtual void __fastcall PopObject(void) = 0 ;
	virtual void __fastcall LoadObject(System::TObject* AName) = 0 ;
	virtual void __fastcall FillPickingList(TGLPickList* &AList) = 0 ;
	__property System::TObject* CurrentObject = {read=GetObject, write=SetObject};
	__property int ObjectCountGuess = {read=FObjectCountGuess, write=SetObjectCountGuess, nodefault};
	__property int Hits = {read=GetHits, write=SetHits, nodefault};
public:
	/* TObject.Create */ inline __fastcall TGLBaseSelectTechnique(void) : System::TObject() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TGLBaseSelectTechnique(void) { }
	
};

#pragma pack(pop)

typedef System::TMetaClass* TGLBaseSelectTechniqueClass;

class DELPHICLASS TGLSelectRenderModeTechnique;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLSelectRenderModeTechnique : public TGLBaseSelectTechnique
{
	typedef TGLBaseSelectTechnique inherited;
	
private:
	typedef System::DynamicArray<unsigned> _TGLSelectRenderModeTechnique__1;
	
	
private:
	_TGLSelectRenderModeTechnique__1 FBuffer;
	
protected:
	virtual System::TObject* __fastcall GetObject(void);
	virtual void __fastcall SetObject(System::TObject* Value);
	virtual int __fastcall GetHits(void);
	virtual void __fastcall SetHits(int Value);
	virtual void __fastcall SetObjectCountGuess(int Value);
	
public:
	__classmethod virtual bool __fastcall IsSupported();
	virtual void __fastcall Start(void);
	virtual bool __fastcall Stop(void);
	virtual void __fastcall FillPickingList(TGLPickList* &AList);
	__property ObjectCountGuess;
	__property Hits;
	__property CurrentObject;
public:
	/* TObject.Create */ inline __fastcall TGLSelectRenderModeTechnique(void) : TGLBaseSelectTechnique() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TGLSelectRenderModeTechnique(void) { }
	
};

#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------
static const System::Word MAX_OBJECT_STACK_DEPTH = System::Word(0x1000);
extern PACKAGE TGLBaseSelectTechniqueClass __fastcall GetBestSelectorClass(void);
}	/* namespace Glselection */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLSELECTION)
using namespace Glselection;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// GlselectionHPP
