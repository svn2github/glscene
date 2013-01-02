// CodeGear C++Builder
// Copyright (c) 1995, 2012 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'VectorLists.pas' rev: 24.00 (Win32)

#ifndef VectorlistsHPP
#define VectorlistsHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <System.Classes.hpp>	// Pascal unit
#include <System.SysUtils.hpp>	// Pascal unit
#include <VectorTypes.hpp>	// Pascal unit
#include <VectorGeometry.hpp>	// Pascal unit
#include <PersistentClasses.hpp>	// Pascal unit
#include <GLCrossPlatform.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Vectorlists
{
//-- type declarations -------------------------------------------------------
enum TBaseListOption : unsigned char { bloExternalMemory, bloSetCountResetsMemory };

typedef System::Set<TBaseListOption, TBaseListOption::bloExternalMemory, TBaseListOption::bloSetCountResetsMemory>  TBaseListOptions;

class DELPHICLASS TBaseList;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TBaseList : public Persistentclasses::TPersistentObject
{
	typedef Persistentclasses::TPersistentObject inherited;
	
private:
	int FCount;
	int FCapacity;
	int FGrowthDelta;
	Vectorgeometry::TByteVector *FBufferItem;
	TBaseListOptions FOptions;
	unsigned FRevision;
	System::UnicodeString FTagString;
	
protected:
	Vectorgeometry::TByteVector *FBaseList;
	int FItemSize;
	void __fastcall SetCount(int Val);
	virtual void __fastcall SetCapacity(int NewCapacity);
	Vectorgeometry::PByteVector __fastcall BufferItem(void);
	bool __fastcall GetSetCountResetsMemory(void);
	void __fastcall SetSetCountResetsMemory(const bool Val);
	virtual void __fastcall ReadItemsData(System::Classes::TReader* AReader);
	virtual void __fastcall WriteItemsData(System::Classes::TWriter* AWriter);
	virtual void __fastcall DefineProperties(System::Classes::TFiler* AFiler);
	
public:
	__fastcall virtual TBaseList(void);
	__fastcall virtual ~TBaseList(void);
	virtual void __fastcall Assign(System::Classes::TPersistent* Src);
	DYNAMIC void __fastcall WriteToFiler(Persistentclasses::TVirtualWriter* writer);
	DYNAMIC void __fastcall ReadFromFiler(Persistentclasses::TVirtualReader* reader);
	void __fastcall AddNulls(unsigned nbVals);
	void __fastcall InsertNulls(int Index, unsigned nbVals);
	void __fastcall AdjustCapacityToAtLeast(const int size);
	int __fastcall DataSize(void);
	void __fastcall UseMemory(void * rangeStart, int rangeCapacity);
	void __fastcall Flush(void);
	void __fastcall Clear(void);
	void __fastcall Delete(int Index);
	void __fastcall DeleteItems(int Index, unsigned nbVals);
	void __fastcall Exchange(int index1, int index2);
	void __fastcall Move(int curIndex, int newIndex);
	void __fastcall Reverse(void);
	__property int Count = {read=FCount, write=SetCount, nodefault};
	__property int Capacity = {read=FCapacity, write=SetCapacity, nodefault};
	__property int GrowthDelta = {read=FGrowthDelta, write=FGrowthDelta, nodefault};
	__property bool SetCountResetsMemory = {read=GetSetCountResetsMemory, write=SetSetCountResetsMemory, nodefault};
	__property System::UnicodeString TagString = {read=FTagString, write=FTagString};
	__property unsigned Revision = {read=FRevision, write=FRevision, nodefault};
public:
	/* TPersistentObject.CreateFromFiler */ inline __fastcall TBaseList(Persistentclasses::TVirtualReader* reader) : Persistentclasses::TPersistentObject(reader) { }
	
};

#pragma pack(pop)

class DELPHICLASS TBaseVectorList;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TBaseVectorList : public TBaseList
{
	typedef TBaseList inherited;
	
protected:
	Vectorgeometry::PFloatVector __fastcall GetItemAddress(int Index);
	
public:
	DYNAMIC void __fastcall WriteToFiler(Persistentclasses::TVirtualWriter* writer);
	DYNAMIC void __fastcall ReadFromFiler(Persistentclasses::TVirtualReader* reader);
	DYNAMIC void __fastcall GetExtents(/* out */ Vectortypes::TVector3f &min, /* out */ Vectortypes::TVector3f &max);
	DYNAMIC Vectortypes::TVector3f __fastcall Sum(void);
	DYNAMIC void __fastcall Normalize(void);
	DYNAMIC float __fastcall MaxSpacing(TBaseVectorList* list2);
	DYNAMIC void __fastcall Translate(const Vectortypes::TVector3f &delta)/* overload */;
	DYNAMIC void __fastcall Translate(TBaseVectorList* const delta)/* overload */;
	DYNAMIC void __fastcall TranslateInv(TBaseVectorList* const delta)/* overload */;
	DYNAMIC void __fastcall Lerp(TBaseVectorList* const list1, TBaseVectorList* const list2, float lerpFactor) = 0 ;
	void __fastcall AngleLerp(TBaseVectorList* const list1, TBaseVectorList* const list2, float lerpFactor);
	void __fastcall AngleCombine(TBaseVectorList* const list1, float intensity);
	DYNAMIC void __fastcall Combine(TBaseVectorList* const list2, float factor);
	__property Vectorgeometry::PFloatVector ItemAddress[int Index] = {read=GetItemAddress};
public:
	/* TBaseList.Create */ inline __fastcall virtual TBaseVectorList(void) : TBaseList() { }
	/* TBaseList.Destroy */ inline __fastcall virtual ~TBaseVectorList(void) { }
	
public:
	/* TPersistentObject.CreateFromFiler */ inline __fastcall TBaseVectorList(Persistentclasses::TVirtualReader* reader) : TBaseList(reader) { }
	
};

#pragma pack(pop)

class DELPHICLASS TAffineVectorList;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TAffineVectorList : public TBaseVectorList
{
	typedef TBaseVectorList inherited;
	
public:
	Vectortypes::TVector3f operator[](int Index) { return Items[Index]; }
	
private:
	Vectorgeometry::TAffineVectorArray *FList;
	
protected:
	Vectortypes::TVector3f __fastcall Get(int Index);
	void __fastcall Put(int Index, const Vectortypes::TVector3f &item);
	virtual void __fastcall SetCapacity(int NewCapacity);
	
public:
	__fastcall virtual TAffineVectorList(void);
	virtual void __fastcall Assign(System::Classes::TPersistent* Src);
	int __fastcall Add(const Vectortypes::TVector3f &item)/* overload */;
	int __fastcall Add(const Vectortypes::TVector4f &item)/* overload */;
	void __fastcall Add(const Vectortypes::TVector3f &i1, const Vectortypes::TVector3f &i2)/* overload */;
	void __fastcall Add(const Vectortypes::TVector3f &i1, const Vectortypes::TVector3f &i2, const Vectortypes::TVector3f &i3)/* overload */;
	int __fastcall Add(const Vectortypes::TVector2f &item)/* overload */;
	int __fastcall Add(const Vectorgeometry::TTexPoint &item)/* overload */;
	int __fastcall Add(const float X, const float Y)/* overload */;
	int __fastcall Add(const float X, const float Y, const float Z)/* overload */;
	int __fastcall Add(const int X, const int Y, const int Z)/* overload */;
	int __fastcall AddNC(const int X, const int Y, const int Z)/* overload */;
	int __fastcall Add(const Vectorgeometry::PIntegerVector xy, const int Z)/* overload */;
	int __fastcall AddNC(const Vectorgeometry::PIntegerVector xy, const int Z)/* overload */;
	void __fastcall Add(TAffineVectorList* const list)/* overload */;
	void __fastcall Push(const Vectortypes::TVector3f &Val);
	Vectortypes::TVector3f __fastcall Pop(void);
	void __fastcall Insert(int Index, const Vectortypes::TVector3f &item);
	int __fastcall IndexOf(const Vectortypes::TVector3f &item);
	int __fastcall FindOrAdd(const Vectortypes::TVector3f &item);
	__property Vectortypes::TVector3f Items[int Index] = {read=Get, write=Put/*, default*/};
	__property Vectorgeometry::PAffineVectorArray List = {read=FList};
	DYNAMIC void __fastcall Translate(const Vectortypes::TVector3f &delta)/* overload */;
	HIDESBASE void __fastcall Translate(const Vectortypes::TVector3f &delta, int base, int nb)/* overload */;
	void __fastcall TranslateItem(int Index, const Vectortypes::TVector3f &delta);
	void __fastcall TranslateItems(int Index, const Vectortypes::TVector3f &delta, int nb);
	void __fastcall CombineItem(int Index, const Vectortypes::TVector3f &vector, const float f);
	void __fastcall TransformAsPoints(const Vectortypes::TMatrix4f &matrix);
	void __fastcall TransformAsVectors(const Vectortypes::TMatrix4f &matrix)/* overload */;
	void __fastcall TransformAsVectors(const Vectortypes::TMatrix3f &matrix)/* overload */;
	DYNAMIC void __fastcall Normalize(void);
	DYNAMIC void __fastcall Lerp(TBaseVectorList* const list1, TBaseVectorList* const list2, float lerpFactor);
	void __fastcall Scale(float factor)/* overload */;
	void __fastcall Scale(const Vectortypes::TVector3f &factors)/* overload */;
public:
	/* TBaseList.Destroy */ inline __fastcall virtual ~TAffineVectorList(void) { }
	
public:
	/* TPersistentObject.CreateFromFiler */ inline __fastcall TAffineVectorList(Persistentclasses::TVirtualReader* reader) : TBaseVectorList(reader) { }
	
/* Hoisted overloads: */
	
public:
	DYNAMIC inline void __fastcall  Translate(TBaseVectorList* const delta){ TBaseVectorList::Translate(delta); }
	
};

#pragma pack(pop)

class DELPHICLASS TVectorList;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TVectorList : public TBaseVectorList
{
	typedef TBaseVectorList inherited;
	
public:
	Vectortypes::TVector4f operator[](int Index) { return Items[Index]; }
	
private:
	Vectorgeometry::TVectorArray *FList;
	
protected:
	Vectortypes::TVector4f __fastcall Get(int Index);
	void __fastcall Put(int Index, const Vectortypes::TVector4f &item);
	virtual void __fastcall SetCapacity(int NewCapacity);
	
public:
	__fastcall virtual TVectorList(void);
	virtual void __fastcall Assign(System::Classes::TPersistent* Src);
	int __fastcall Add(const Vectortypes::TVector4f &item)/* overload */;
	int __fastcall Add(const Vectortypes::TVector3f &item, float w)/* overload */;
	int __fastcall Add(const float X, const float Y, const float Z, const float w)/* overload */;
	void __fastcall Add(const Vectortypes::TVector3f &i1, const Vectortypes::TVector3f &i2, const Vectortypes::TVector3f &i3, float w)/* overload */;
	int __fastcall AddVector(const Vectortypes::TVector3f &item)/* overload */;
	int __fastcall AddPoint(const Vectortypes::TVector3f &item)/* overload */;
	int __fastcall AddPoint(const float X, const float Y, const float Z = 0.000000E+00)/* overload */;
	void __fastcall Push(const Vectortypes::TVector4f &Val);
	Vectortypes::TVector4f __fastcall Pop(void);
	int __fastcall IndexOf(const Vectortypes::TVector4f &item);
	int __fastcall FindOrAdd(const Vectortypes::TVector4f &item);
	int __fastcall FindOrAddPoint(const Vectortypes::TVector3f &item);
	void __fastcall Insert(int Index, const Vectortypes::TVector4f &item);
	__property Vectortypes::TVector4f Items[int Index] = {read=Get, write=Put/*, default*/};
	__property Vectorgeometry::PVectorArray List = {read=FList};
	DYNAMIC void __fastcall Lerp(TBaseVectorList* const list1, TBaseVectorList* const list2, float lerpFactor);
public:
	/* TBaseList.Destroy */ inline __fastcall virtual ~TVectorList(void) { }
	
public:
	/* TPersistentObject.CreateFromFiler */ inline __fastcall TVectorList(Persistentclasses::TVirtualReader* reader) : TBaseVectorList(reader) { }
	
};

#pragma pack(pop)

class DELPHICLASS TTexPointList;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TTexPointList : public TBaseVectorList
{
	typedef TBaseVectorList inherited;
	
public:
	Vectorgeometry::TTexPoint operator[](int Index) { return Items[Index]; }
	
private:
	Vectorgeometry::TTexPointArray *FList;
	
protected:
	Vectorgeometry::TTexPoint __fastcall Get(int Index);
	void __fastcall Put(int Index, const Vectorgeometry::TTexPoint &item);
	virtual void __fastcall SetCapacity(int NewCapacity);
	
public:
	__fastcall virtual TTexPointList(void);
	virtual void __fastcall Assign(System::Classes::TPersistent* Src);
	int __fastcall IndexOf(const Vectorgeometry::TTexPoint &item);
	int __fastcall FindOrAdd(const Vectorgeometry::TTexPoint &item);
	int __fastcall Add(const Vectorgeometry::TTexPoint &item)/* overload */;
	int __fastcall Add(const Vectortypes::TVector2f &item)/* overload */;
	int __fastcall Add(const float texS, const float Text)/* overload */;
	int __fastcall Add(const int texS, const int Text)/* overload */;
	int __fastcall AddNC(const int texS, const int Text)/* overload */;
	int __fastcall Add(const Vectorgeometry::PIntegerVector texST)/* overload */;
	int __fastcall AddNC(const Vectorgeometry::PIntegerVector texST)/* overload */;
	void __fastcall Push(const Vectorgeometry::TTexPoint &Val);
	Vectorgeometry::TTexPoint __fastcall Pop(void);
	void __fastcall Insert(int Index, const Vectorgeometry::TTexPoint &item);
	__property Vectorgeometry::TTexPoint Items[int Index] = {read=Get, write=Put/*, default*/};
	__property Vectorgeometry::PTexPointArray List = {read=FList};
	HIDESBASE void __fastcall Translate(const Vectorgeometry::TTexPoint &delta);
	void __fastcall ScaleAndTranslate(const Vectorgeometry::TTexPoint &scale, const Vectorgeometry::TTexPoint &delta)/* overload */;
	void __fastcall ScaleAndTranslate(const Vectorgeometry::TTexPoint &scale, const Vectorgeometry::TTexPoint &delta, int base, int nb)/* overload */;
	DYNAMIC void __fastcall Lerp(TBaseVectorList* const list1, TBaseVectorList* const list2, float lerpFactor);
public:
	/* TBaseList.Destroy */ inline __fastcall virtual ~TTexPointList(void) { }
	
public:
	/* TPersistentObject.CreateFromFiler */ inline __fastcall TTexPointList(Persistentclasses::TVirtualReader* reader) : TBaseVectorList(reader) { }
	
};

#pragma pack(pop)

class DELPHICLASS TIntegerList;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TIntegerList : public TBaseList
{
	typedef TBaseList inherited;
	
public:
	int operator[](int Index) { return Items[Index]; }
	
private:
	Vectorgeometry::TIntegerVector *FList;
	
protected:
	int __fastcall Get(int Index);
	void __fastcall Put(int Index, const int item);
	virtual void __fastcall SetCapacity(int newCapacity);
	
public:
	__fastcall virtual TIntegerList(void);
	virtual void __fastcall Assign(System::Classes::TPersistent* src);
	int __fastcall Add(const int item)/* overload */;
	int __fastcall AddNC(const int item)/* overload */;
	void __fastcall Add(const int i1, const int i2)/* overload */;
	void __fastcall Add(const int i1, const int i2, const int i3)/* overload */;
	void __fastcall Add(TIntegerList* const AList)/* overload */;
	void __fastcall Push(const int Val);
	int __fastcall Pop(void);
	void __fastcall Insert(int Index, const int item);
	void __fastcall Remove(const int item);
	int __fastcall IndexOf(int item);
	__property int Items[int Index] = {read=Get, write=Put/*, default*/};
	__property Vectorgeometry::PIntegerVector List = {read=FList};
	void __fastcall AddSerie(int aBase, int aDelta, int aCount);
	void __fastcall AddIntegers(const System::PInteger First, int n)/* overload */;
	void __fastcall AddIntegers(TIntegerList* const aList)/* overload */;
	void __fastcall AddIntegers(int const *anArray, const int anArray_Size)/* overload */;
	int __fastcall MinInteger(void);
	int __fastcall MaxInteger(void);
	void __fastcall Sort(void);
	void __fastcall SortAndRemoveDuplicates(void);
	int __fastcall BinarySearch(const int Value)/* overload */;
	int __fastcall BinarySearch(const int Value, bool returnBestFit, bool &found)/* overload */;
	int __fastcall AddSorted(const int Value, const bool ignoreDuplicates = false);
	void __fastcall RemoveSorted(const int Value);
	void __fastcall Offset(int delta)/* overload */;
	void __fastcall Offset(int delta, const int base, const int nb)/* overload */;
public:
	/* TBaseList.Destroy */ inline __fastcall virtual ~TIntegerList(void) { }
	
public:
	/* TPersistentObject.CreateFromFiler */ inline __fastcall TIntegerList(Persistentclasses::TVirtualReader* reader) : TBaseList(reader) { }
	
};

#pragma pack(pop)

typedef System::StaticArray<float, 134217728> TSingleArrayList;

typedef TSingleArrayList *PSingleArrayList;

class DELPHICLASS TSingleList;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TSingleList : public TBaseList
{
	typedef TBaseList inherited;
	
public:
	float operator[](int Index) { return Items[Index]; }
	
private:
	TSingleArrayList *FList;
	
protected:
	float __fastcall Get(int Index);
	void __fastcall Put(int Index, const float item);
	virtual void __fastcall SetCapacity(int NewCapacity);
	
public:
	__fastcall virtual TSingleList(void);
	virtual void __fastcall Assign(System::Classes::TPersistent* Src);
	int __fastcall Add(const float item)/* overload */;
	void __fastcall Add(const float i1, const float i2)/* overload */;
	void __fastcall AddSingles(const System::PSingle First, int n)/* overload */;
	void __fastcall AddSingles(float const *anArray, const int anArray_Size)/* overload */;
	void __fastcall Push(const float Val);
	float __fastcall Pop(void);
	void __fastcall Insert(int Index, const float item);
	__property float Items[int Index] = {read=Get, write=Put/*, default*/};
	__property PSingleArrayList List = {read=FList};
	void __fastcall AddSerie(float aBase, float aDelta, int aCount);
	void __fastcall Offset(float delta)/* overload */;
	void __fastcall Offset(TSingleList* const delta)/* overload */;
	void __fastcall Scale(float factor);
	void __fastcall Sqr(void);
	void __fastcall Sqrt(void);
	float __fastcall Sum(void);
	float __fastcall Min(void);
	float __fastcall Max(void);
public:
	/* TBaseList.Destroy */ inline __fastcall virtual ~TSingleList(void) { }
	
public:
	/* TPersistentObject.CreateFromFiler */ inline __fastcall TSingleList(Persistentclasses::TVirtualReader* reader) : TBaseList(reader) { }
	
};

#pragma pack(pop)

typedef System::StaticArray<double, 134217728> TDoubleArrayList;

typedef TDoubleArrayList *PDoubleArrayList;

class DELPHICLASS TDoubleList;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TDoubleList : public TBaseList
{
	typedef TBaseList inherited;
	
public:
	double operator[](int Index) { return Items[Index]; }
	
private:
	TDoubleArrayList *FList;
	
protected:
	double __fastcall Get(int Index);
	void __fastcall Put(int Index, const double item);
	virtual void __fastcall SetCapacity(int NewCapacity);
	
public:
	__fastcall virtual TDoubleList(void);
	virtual void __fastcall Assign(System::Classes::TPersistent* Src);
	int __fastcall Add(const double item);
	void __fastcall Push(const double Val);
	double __fastcall Pop(void);
	void __fastcall Insert(int Index, const double item);
	__property double Items[int Index] = {read=Get, write=Put/*, default*/};
	__property PDoubleArrayList List = {read=FList};
	void __fastcall AddSerie(double aBase, double aDelta, int aCount);
	void __fastcall Offset(double delta)/* overload */;
	void __fastcall Offset(TDoubleList* const delta)/* overload */;
	void __fastcall Scale(double factor);
	void __fastcall Sqr(void);
	void __fastcall Sqrt(void);
	double __fastcall Sum(void);
	float __fastcall Min(void);
	float __fastcall Max(void);
public:
	/* TBaseList.Destroy */ inline __fastcall virtual ~TDoubleList(void) { }
	
public:
	/* TPersistentObject.CreateFromFiler */ inline __fastcall TDoubleList(Persistentclasses::TVirtualReader* reader) : TBaseList(reader) { }
	
};

#pragma pack(pop)

class DELPHICLASS TByteList;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TByteList : public TBaseList
{
	typedef TBaseList inherited;
	
public:
	System::Byte operator[](int Index) { return Items[Index]; }
	
private:
	Vectorgeometry::TByteVector *FList;
	
protected:
	System::Byte __fastcall Get(int Index);
	void __fastcall Put(int Index, const System::Byte item);
	virtual void __fastcall SetCapacity(int NewCapacity);
	
public:
	__fastcall virtual TByteList(void);
	virtual void __fastcall Assign(System::Classes::TPersistent* Src);
	int __fastcall Add(const System::Byte item);
	void __fastcall Insert(int Index, const System::Byte item);
	__property System::Byte Items[int Index] = {read=Get, write=Put/*, default*/};
	__property Vectorgeometry::PByteVector List = {read=FList};
public:
	/* TBaseList.Destroy */ inline __fastcall virtual ~TByteList(void) { }
	
public:
	/* TPersistentObject.CreateFromFiler */ inline __fastcall TByteList(Persistentclasses::TVirtualReader* reader) : TBaseList(reader) { }
	
};

#pragma pack(pop)

class DELPHICLASS TQuaternionList;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TQuaternionList : public TBaseVectorList
{
	typedef TBaseVectorList inherited;
	
public:
	Vectorgeometry::TQuaternion operator[](int Index) { return Items[Index]; }
	
private:
	Vectorgeometry::TQuaternionArray *FList;
	
protected:
	Vectorgeometry::TQuaternion __fastcall Get(int Index);
	void __fastcall Put(int Index, const Vectorgeometry::TQuaternion &item);
	virtual void __fastcall SetCapacity(int NewCapacity);
	
public:
	__fastcall virtual TQuaternionList(void);
	virtual void __fastcall Assign(System::Classes::TPersistent* Src);
	int __fastcall Add(const Vectorgeometry::TQuaternion &item)/* overload */;
	int __fastcall Add(const Vectortypes::TVector3f &item, float w)/* overload */;
	int __fastcall Add(const float X, const float Y, const float Z, const float W)/* overload */;
	void __fastcall Push(const Vectorgeometry::TQuaternion &Val);
	Vectorgeometry::TQuaternion __fastcall Pop(void);
	int __fastcall IndexOf(const Vectorgeometry::TQuaternion &item);
	int __fastcall FindOrAdd(const Vectorgeometry::TQuaternion &item);
	void __fastcall Insert(int Index, const Vectorgeometry::TQuaternion &item);
	__property Vectorgeometry::TQuaternion Items[int Index] = {read=Get, write=Put/*, default*/};
	__property Vectorgeometry::PQuaternionArray List = {read=FList};
	DYNAMIC void __fastcall Lerp(TBaseVectorList* const list1, TBaseVectorList* const list2, float lerpFactor);
	DYNAMIC void __fastcall Combine(TBaseVectorList* const list2, float factor);
public:
	/* TBaseList.Destroy */ inline __fastcall virtual ~TQuaternionList(void) { }
	
public:
	/* TPersistentObject.CreateFromFiler */ inline __fastcall TQuaternionList(Persistentclasses::TVirtualReader* reader) : TBaseVectorList(reader) { }
	
};

#pragma pack(pop)

#pragma pack(push,1)
struct DECLSPEC_DRECORD T4ByteData
{
private:
	struct DECLSPEC_DRECORD _T4ByteData__1
	{
public:
		System::StaticArray<System::Byte, 4> Value;
	};
	
	
	struct DECLSPEC_DRECORD _T4ByteData__2
	{
public:
		int Value;
	};
	
	
	struct DECLSPEC_DRECORD _T4ByteData__3
	{
public:
		unsigned Value;
	};
	
	
	struct DECLSPEC_DRECORD _T4ByteData__4
	{
public:
		float Value;
	};
	
	
	struct DECLSPEC_DRECORD _T4ByteData__5
	{
public:
		System::StaticArray<System::Word, 2> Value;
	};
	
	
	
	union
	{
		struct 
		{
			_T4ByteData__5 Word;
		};
		struct 
		{
			_T4ByteData__4 Float;
		};
		struct 
		{
			_T4ByteData__3 UInt;
		};
		struct 
		{
			_T4ByteData__2 Int;
		};
		struct 
		{
			_T4ByteData__1 Bytes;
		};
		
	};
};
#pragma pack(pop)


typedef System::StaticArray<T4ByteData, 134217728> T4ByteArrayList;

typedef T4ByteArrayList *P4ByteArrayList;

class DELPHICLASS T4ByteList;
#pragma pack(push,4)
class PASCALIMPLEMENTATION T4ByteList : public TBaseList
{
	typedef TBaseList inherited;
	
public:
	T4ByteData operator[](int Index) { return Items[Index]; }
	
private:
	T4ByteArrayList *FList;
	
protected:
	T4ByteData __fastcall Get(int Index);
	void __fastcall Put(int Index, const T4ByteData item);
	virtual void __fastcall SetCapacity(int NewCapacity);
	
public:
	__fastcall virtual T4ByteList(void);
	virtual void __fastcall Assign(System::Classes::TPersistent* Src);
	int __fastcall Add(const T4ByteData item)/* overload */;
	void __fastcall Add(const float i1)/* overload */;
	void __fastcall Add(const float i1, const float i2)/* overload */;
	void __fastcall Add(const float i1, const float i2, const float i3)/* overload */;
	void __fastcall Add(const float i1, const float i2, const float i3, const float i4)/* overload */;
	void __fastcall Add(const int i1)/* overload */;
	void __fastcall Add(const int i1, const int i2)/* overload */;
	void __fastcall Add(const int i1, const int i2, const int i3)/* overload */;
	void __fastcall Add(const int i1, const int i2, const int i3, const int i4)/* overload */;
	void __fastcall Add(const unsigned i1)/* overload */;
	void __fastcall Add(const unsigned i1, const unsigned i2)/* overload */;
	void __fastcall Add(const unsigned i1, const unsigned i2, const unsigned i3)/* overload */;
	void __fastcall Add(const unsigned i1, const unsigned i2, const unsigned i3, const unsigned i4)/* overload */;
	void __fastcall Add(T4ByteList* const AList)/* overload */;
	void __fastcall Push(const T4ByteData Val);
	T4ByteData __fastcall Pop(void);
	void __fastcall Insert(int Index, const T4ByteData item);
	__property T4ByteData Items[int Index] = {read=Get, write=Put/*, default*/};
	__property P4ByteArrayList List = {read=FList};
public:
	/* TBaseList.Destroy */ inline __fastcall virtual ~T4ByteList(void) { }
	
public:
	/* TPersistentObject.CreateFromFiler */ inline __fastcall T4ByteList(Persistentclasses::TVirtualReader* reader) : TBaseList(reader) { }
	
};

#pragma pack(pop)

class DELPHICLASS TLongWordList;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TLongWordList : public TBaseList
{
	typedef TBaseList inherited;
	
public:
	unsigned operator[](int Index) { return Items[Index]; }
	
private:
	Vectorgeometry::TLongWordVector *FList;
	
protected:
	unsigned __fastcall Get(int Index);
	void __fastcall Put(int Index, const unsigned item);
	virtual void __fastcall SetCapacity(int newCapacity);
	
public:
	__fastcall virtual TLongWordList(void);
	virtual void __fastcall Assign(System::Classes::TPersistent* src);
	int __fastcall Add(const unsigned item)/* overload */;
	int __fastcall AddNC(const unsigned item)/* overload */;
	void __fastcall Add(const unsigned i1, const unsigned i2)/* overload */;
	void __fastcall Add(const unsigned i1, const unsigned i2, const unsigned i3)/* overload */;
	void __fastcall Add(TLongWordList* const AList)/* overload */;
	void __fastcall Push(const unsigned Val);
	unsigned __fastcall Pop(void);
	void __fastcall Insert(int Index, const unsigned item);
	void __fastcall Remove(const unsigned item);
	unsigned __fastcall IndexOf(int item);
	__property unsigned Items[int Index] = {read=Get, write=Put/*, default*/};
	__property Vectorgeometry::PLongWordVector List = {read=FList};
	void __fastcall AddLongWords(const System::PLongWord First, int n)/* overload */;
	void __fastcall AddLongWords(TLongWordList* const aList)/* overload */;
	void __fastcall AddLongWords(unsigned const *anArray, const int anArray_Size)/* overload */;
public:
	/* TBaseList.Destroy */ inline __fastcall virtual ~TLongWordList(void) { }
	
public:
	/* TPersistentObject.CreateFromFiler */ inline __fastcall TLongWordList(Persistentclasses::TVirtualReader* reader) : TBaseList(reader) { }
	
};

#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------
extern PACKAGE void __fastcall QuickSortLists(int startIndex, int endIndex, TSingleList* refList, System::Classes::TList* objList)/* overload */;
extern PACKAGE void __fastcall QuickSortLists(int startIndex, int endIndex, TSingleList* refList, TBaseList* objList)/* overload */;
extern PACKAGE void __fastcall FastQuickSortLists(int startIndex, int endIndex, TSingleList* refList, Persistentclasses::TPersistentObjectList* objList);
}	/* namespace Vectorlists */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_VECTORLISTS)
using namespace Vectorlists;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// VectorlistsHPP
