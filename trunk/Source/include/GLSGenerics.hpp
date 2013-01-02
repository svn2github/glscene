// CodeGear C++Builder
// Copyright (c) 1995, 2012 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLSGenerics.pas' rev: 24.00 (Win32)

#ifndef GlsgenericsHPP
#define GlsgenericsHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <System.SysUtils.hpp>	// Pascal unit
#include <System.Classes.hpp>	// Pascal unit
#include <System.SyncObjs.hpp>	// Pascal unit
#include <GLCrossPlatform.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Glsgenerics
{
//-- type declarations -------------------------------------------------------
template<typename T> class DELPHICLASS GList__1;
// Template declaration generated by Delphi parameterized types is
// used only for accessing Delphi variables and fields.
// Don't instantiate with new type parameters in user code.
template<typename T> class PASCALIMPLEMENTATION GList__1 : public System::TObject
{
	typedef System::TObject inherited;
	
public:
	
	#define _decl_Glsgenerics_GList__1_TListChangeEvent(T, _DECLNAME) void __fastcall (__closure *_DECLNAME)(System::TObject* Sender, const T Item, System::Classes::TListNotification Action)
	// typedef void __fastcall (__closure *TListChangeEvent)(System::TObject* Sender, const T Item, System::Classes::TListNotification Action);
	
	
public:
	T operator[](int Index) { return Items[Index]; }
	
private:
	System::DynamicArray<T> FItems;
	int FCount;
	int FCapacity;
	System::Classes::TNotifyEvent FOnChange;
	
protected:
	void __fastcall SetCapacity(int Value);
	void __fastcall SetCount(int Value);
	T __fastcall GetItem(int Index);
	void __fastcall SetItem(int Index, const T Value);
	void * __fastcall GetItemAddress(int Index);
	void __fastcall Grow(void);
	virtual void __fastcall Notify(const T Item, System::Classes::TListNotification Action);
	
public:
	__fastcall virtual ~GList__1(void);
	void __fastcall Clear(void);
	int __fastcall Add(T AItem);
	void __fastcall Delete(int Index);
	void __fastcall Extract(T AItem);
	int __fastcall Remove(T AItem);
	int __fastcall IndexOf(T AItem);
	void __fastcall Insert(int Index, T AItem);
	void __fastcall Exchange(int Index1, int Index2);
	T __fastcall First(void);
	T __fastcall Last(void);
	__property T Items[int Index] = {read=GetItem, write=SetItem/*, default*/};
	__property void * ItemAddress[int Index] = {read=GetItemAddress};
	__property int Capacity = {read=FCapacity, write=SetCapacity, nodefault};
	__property int Count = {read=FCount, write=SetCount, nodefault};
	__property System::Classes::TNotifyEvent OnChange = {read=FOnChange, write=FOnChange};
public:
	/* TObject.Create */ inline __fastcall GList__1(void) : System::TObject() { }
	
};


template<typename T> class DELPHICLASS GThreadList__1;
#pragma pack(push,4)
// Template declaration generated by Delphi parameterized types is
// used only for accessing Delphi variables and fields.
// Don't instantiate with new type parameters in user code.
template<typename T> class PASCALIMPLEMENTATION GThreadList__1 : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	GList__1<T>* FList;
	System::Syncobjs::TCriticalSection* FLock;
	
public:
	__fastcall GThreadList__1(void);
	__fastcall virtual ~GThreadList__1(void);
	void __fastcall Add(T AItem);
	void __fastcall Clear(void);
	GList__1<T>* __fastcall LockList(void);
	void __fastcall Remove(T AItem);
	void __fastcall UnlockList(void);
};

#pragma pack(pop)

template<typename T> class DELPHICLASS GOrderedList__1;
#pragma pack(push,4)
// Template declaration generated by Delphi parameterized types is
// used only for accessing Delphi variables and fields.
// Don't instantiate with new type parameters in user code.
template<typename T> class PASCALIMPLEMENTATION GOrderedList__1 : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	GList__1<T>* FList;
	
protected:
	virtual void __fastcall PushItem(T AItem) = 0 ;
	virtual T __fastcall PopItem(void);
	virtual T __fastcall PeekItem(void);
	__property GList__1<T>* List = {read=FList};
	
public:
	__fastcall virtual GOrderedList__1(void);
	__fastcall virtual ~GOrderedList__1(void);
	int __fastcall Count(void);
	bool __fastcall AtLeast(int ACount);
	T __fastcall Push(const T AItem);
	T __fastcall Pop(void);
	T __fastcall Peek(void);
};

#pragma pack(pop)

template<typename T> class DELPHICLASS GStack__1;
#pragma pack(push,4)
// Template declaration generated by Delphi parameterized types is
// used only for accessing Delphi variables and fields.
// Don't instantiate with new type parameters in user code.
template<typename T> class PASCALIMPLEMENTATION GStack__1 : public GOrderedList__1<T>
{
	typedef GOrderedList__1<T> inherited;
	
protected:
	virtual void __fastcall PushItem(T AItem);
public:
	/* {GLSGenerics}GOrderedList<GLSGenerics_GStack<T>_T>.Create */ inline __fastcall virtual GStack__1(void) : GOrderedList__1<T>() { }
	/* {GLSGenerics}GOrderedList<GLSGenerics_GStack<T>_T>.Destroy */ inline __fastcall virtual ~GStack__1(void) { }
	
};

#pragma pack(pop)

template<typename T> class DELPHICLASS GQueue__1;
#pragma pack(push,4)
// Template declaration generated by Delphi parameterized types is
// used only for accessing Delphi variables and fields.
// Don't instantiate with new type parameters in user code.
template<typename T> class PASCALIMPLEMENTATION GQueue__1 : public GOrderedList__1<T>
{
	typedef GOrderedList__1<T> inherited;
	
protected:
	virtual void __fastcall PushItem(T AItem);
public:
	/* {GLSGenerics}GOrderedList<GLSGenerics_GQueue<T>_T>.Create */ inline __fastcall virtual GQueue__1(void) : GOrderedList__1<T>() { }
	/* {GLSGenerics}GOrderedList<GLSGenerics_GQueue<T>_T>.Destroy */ inline __fastcall virtual ~GQueue__1(void) { }
	
};

#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------
static const int MaxListSize = int(0x7ffffff);
}	/* namespace Glsgenerics */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLSGENERICS)
using namespace Glsgenerics;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// GlsgenericsHPP
