// CodeGear C++Builder
// Copyright (c) 1995, 2012 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'PersistentClasses.pas' rev: 24.00 (Win32)

#ifndef PersistentclassesHPP
#define PersistentclassesHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <System.Classes.hpp>	// Pascal unit
#include <System.SysUtils.hpp>	// Pascal unit
#include <GLCrossPlatform.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Persistentclasses
{
//-- type declarations -------------------------------------------------------
typedef System::TObject* *PObject;

class DELPHICLASS TVirtualReader;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TVirtualReader : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	System::Classes::TStream* FStream;
	
public:
	__fastcall virtual TVirtualReader(System::Classes::TStream* Stream);
	__property System::Classes::TStream* Stream = {read=FStream};
	void __fastcall ReadTypeError(void);
	virtual void __fastcall Read(void *Buf, int Count) = 0 ;
	virtual System::Classes::TValueType __fastcall NextValue(void) = 0 ;
	virtual int __fastcall ReadInteger(void) = 0 ;
	virtual bool __fastcall ReadBoolean(void) = 0 ;
	virtual System::UnicodeString __fastcall ReadString(void) = 0 ;
	virtual System::Extended __fastcall ReadFloat(void) = 0 ;
	virtual void __fastcall ReadListBegin(void) = 0 ;
	virtual void __fastcall ReadListEnd(void) = 0 ;
	virtual bool __fastcall EndOfList(void) = 0 ;
	void __fastcall ReadTStrings(System::Classes::TStrings* aStrings);
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TVirtualReader(void) { }
	
};

#pragma pack(pop)

class DELPHICLASS TVirtualWriter;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TVirtualWriter : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	System::Classes::TStream* FStream;
	
public:
	__fastcall virtual TVirtualWriter(System::Classes::TStream* Stream);
	__property System::Classes::TStream* Stream = {read=FStream};
	virtual void __fastcall Write(const void *Buf, int Count) = 0 ;
	virtual void __fastcall WriteInteger(int anInteger) = 0 ;
	virtual void __fastcall WriteBoolean(bool aBoolean) = 0 ;
	virtual void __fastcall WriteString(const System::UnicodeString aString) = 0 ;
	virtual void __fastcall WriteFloat(const System::Extended aFloat) = 0 ;
	virtual void __fastcall WriteListBegin(void) = 0 ;
	virtual void __fastcall WriteListEnd(void) = 0 ;
	void __fastcall WriteTStrings(System::Classes::TStrings* const aStrings, bool storeObjects = true);
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TVirtualWriter(void) { }
	
};

#pragma pack(pop)

typedef System::TMetaClass* TVirtualReaderClass;

typedef System::TMetaClass* TVirtualWriterClass;

__interface IPersistentObject;
typedef System::DelphiInterface<IPersistentObject> _di_IPersistentObject;
__interface  INTERFACE_UUID("{A9A0198A-F11B-4325-A92C-2F24DB41652B}") IPersistentObject  : public System::IInterface 
{
	
public:
	virtual void __fastcall WriteToFiler(TVirtualWriter* writer) = 0 ;
	virtual void __fastcall ReadFromFiler(TVirtualReader* reader) = 0 ;
};

class DELPHICLASS TPersistentObject;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TPersistentObject : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
protected:
	void __fastcall RaiseFilerException(const int archiveVersion);
	HRESULT __stdcall QueryInterface(const GUID &IID, /* out */ void *Obj);
	int __stdcall _AddRef(void);
	int __stdcall _Release(void);
	
public:
	__fastcall virtual TPersistentObject(void);
	__fastcall TPersistentObject(TVirtualReader* reader);
	__fastcall virtual ~TPersistentObject(void);
	virtual void __fastcall Assign(System::Classes::TPersistent* source);
	DYNAMIC TPersistentObject* __fastcall CreateClone(void);
	__classmethod virtual System::UnicodeString __fastcall FileSignature();
	__classmethod virtual TVirtualWriterClass __fastcall FileVirtualWriter();
	__classmethod virtual TVirtualReaderClass __fastcall FileVirtualReader();
	DYNAMIC void __fastcall WriteToFiler(TVirtualWriter* writer);
	DYNAMIC void __fastcall ReadFromFiler(TVirtualReader* reader);
	DYNAMIC void __fastcall SaveToStream(System::Classes::TStream* stream, TVirtualWriterClass writerClass = 0x0);
	DYNAMIC void __fastcall LoadFromStream(System::Classes::TStream* stream, TVirtualReaderClass readerClass = 0x0);
	DYNAMIC void __fastcall SaveToFile(const System::UnicodeString fileName, TVirtualWriterClass writerClass = 0x0);
	DYNAMIC void __fastcall LoadFromFile(const System::UnicodeString fileName, TVirtualReaderClass readerClass = 0x0);
	DYNAMIC System::UnicodeString __fastcall SaveToString(TVirtualWriterClass writerClass = 0x0);
	DYNAMIC void __fastcall LoadFromString(const System::UnicodeString data, TVirtualReaderClass readerClass = 0x0);
private:
	void *__IPersistentObject;	/* IPersistentObject */
	
public:
	#if defined(MANAGED_INTERFACE_OPERATORS)
	// {A9A0198A-F11B-4325-A92C-2F24DB41652B}
	operator _di_IPersistentObject()
	{
		_di_IPersistentObject intf;
		GetInterface(intf);
		return intf;
	}
	#else
	operator IPersistentObject*(void) { return (IPersistentObject*)&__IPersistentObject; }
	#endif
	
};

#pragma pack(pop)

typedef System::TMetaClass* TPersistentObjectClass;

typedef System::StaticArray<System::TObject*, 268435456> TPointerObjectList;

typedef TPointerObjectList *PPointerObjectList;

typedef int __fastcall (*TObjectListSortCompare)(System::TObject* item1, System::TObject* item2);

class DELPHICLASS TPersistentObjectList;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TPersistentObjectList : public TPersistentObject
{
	typedef TPersistentObject inherited;
	
public:
	System::TObject* operator[](int Index) { return Items[Index]; }
	
private:
	TPointerObjectList *FList;
	int FCount;
	int FCapacity;
	int FGrowthDelta;
	
protected:
	virtual void __fastcall Error(void);
	System::TObject* __fastcall Get(int Index);
	void __fastcall Put(int Index, System::TObject* Item);
	void __fastcall SetCapacity(int newCapacity);
	void __fastcall SetCount(int NewCount);
	System::TObject* __fastcall GetFirst(void);
	void __fastcall SetFirst(System::TObject* item);
	System::TObject* __fastcall GetLast(void);
	void __fastcall SetLast(System::TObject* item);
	virtual void __fastcall AfterObjectCreatedByReader(System::TObject* Sender);
	void __fastcall DoClean(void);
	
public:
	__fastcall virtual TPersistentObjectList(void);
	__fastcall virtual ~TPersistentObjectList(void);
	DYNAMIC void __fastcall WriteToFiler(TVirtualWriter* writer);
	DYNAMIC void __fastcall ReadFromFiler(TVirtualReader* reader);
	void __fastcall ReadFromFilerWithEvent(TVirtualReader* reader, System::Classes::TNotifyEvent afterSenderObjectCreated);
	int __fastcall Add(System::TObject* const item);
	void __fastcall AddNils(unsigned nbVals);
	void __fastcall Delete(int index);
	void __fastcall DeleteItems(int index, unsigned nbVals);
	void __fastcall Exchange(int Index1, int Index2);
	void __fastcall Insert(int Index, System::TObject* Item);
	void __fastcall InsertNils(int index, unsigned nbVals);
	void __fastcall Move(int CurIndex, int NewIndex);
	int __fastcall Remove(System::TObject* Item);
	void __fastcall DeleteAndFree(int index);
	void __fastcall DeleteAndFreeItems(int index, unsigned nbVals);
	int __fastcall RemoveAndFree(System::TObject* item);
	__property int GrowthDelta = {read=FGrowthDelta, write=FGrowthDelta, nodefault};
	TPersistentObjectList* __fastcall Expand(void);
	__property System::TObject* Items[int Index] = {read=Get, write=Put/*, default*/};
	__property int Count = {read=FCount, write=SetCount, nodefault};
	__property PPointerObjectList List = {read=FList};
	__property int Capacity = {read=FCapacity, write=SetCapacity, nodefault};
	void __fastcall RequiredCapacity(int aCapacity);
	void __fastcall Pack(void);
	DYNAMIC void __fastcall Clear(void);
	DYNAMIC void __fastcall Clean(void);
	void __fastcall CleanFree(void);
	int __fastcall IndexOf(System::TObject* Item);
	__property System::TObject* First = {read=GetFirst, write=SetFirst};
	__property System::TObject* Last = {read=GetLast, write=SetLast};
	void __fastcall Push(System::TObject* item);
	System::TObject* __fastcall Pop(void);
	void __fastcall PopAndFree(void);
	int __fastcall AddObjects(TPersistentObjectList* const objectList);
	void __fastcall RemoveObjects(TPersistentObjectList* const objectList);
	void __fastcall Sort(TObjectListSortCompare compareFunc);
public:
	/* TPersistentObject.CreateFromFiler */ inline __fastcall TPersistentObjectList(TVirtualReader* reader) : TPersistentObject(reader) { }
	
};

#pragma pack(pop)

class DELPHICLASS TBinaryReader;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TBinaryReader : public TVirtualReader
{
	typedef TVirtualReader inherited;
	
protected:
	System::Classes::TValueType __fastcall ReadValue(void);
	System::WideString __fastcall ReadWideString(System::Classes::TValueType vType);
	
public:
	virtual void __fastcall Read(void *Buf, int Count);
	virtual System::Classes::TValueType __fastcall NextValue(void);
	virtual int __fastcall ReadInteger(void);
	virtual bool __fastcall ReadBoolean(void);
	virtual System::UnicodeString __fastcall ReadString(void);
	virtual System::Extended __fastcall ReadFloat(void);
	virtual void __fastcall ReadListBegin(void);
	virtual void __fastcall ReadListEnd(void);
	virtual bool __fastcall EndOfList(void);
public:
	/* TVirtualReader.Create */ inline __fastcall virtual TBinaryReader(System::Classes::TStream* Stream) : TVirtualReader(Stream) { }
	
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TBinaryReader(void) { }
	
};

#pragma pack(pop)

class DELPHICLASS TBinaryWriter;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TBinaryWriter : public TVirtualWriter
{
	typedef TVirtualWriter inherited;
	
protected:
	virtual void __fastcall WriteAnsiString(const System::AnsiString aString);
	virtual void __fastcall WriteWideString(const System::WideString aString);
	
public:
	virtual void __fastcall Write(const void *Buf, int Count);
	virtual void __fastcall WriteInteger(int anInteger);
	virtual void __fastcall WriteBoolean(bool aBoolean);
	virtual void __fastcall WriteString(const System::UnicodeString aString);
	virtual void __fastcall WriteFloat(const System::Extended aFloat);
	virtual void __fastcall WriteListBegin(void);
	virtual void __fastcall WriteListEnd(void);
public:
	/* TVirtualWriter.Create */ inline __fastcall virtual TBinaryWriter(System::Classes::TStream* Stream) : TVirtualWriter(Stream) { }
	
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TBinaryWriter(void) { }
	
};

#pragma pack(pop)

class DELPHICLASS TTextReader;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TTextReader : public TVirtualReader
{
	typedef TVirtualReader inherited;
	
private:
	System::UnicodeString FValueType;
	System::UnicodeString FData;
	
protected:
	void __fastcall ReadLine(const System::UnicodeString requestedType = System::UnicodeString());
	
public:
	virtual void __fastcall Read(void *Buf, int Count);
	virtual System::Classes::TValueType __fastcall NextValue(void);
	virtual int __fastcall ReadInteger(void);
	virtual bool __fastcall ReadBoolean(void);
	virtual System::UnicodeString __fastcall ReadString(void);
	virtual System::Extended __fastcall ReadFloat(void);
	virtual void __fastcall ReadListBegin(void);
	virtual void __fastcall ReadListEnd(void);
	virtual bool __fastcall EndOfList(void);
public:
	/* TVirtualReader.Create */ inline __fastcall virtual TTextReader(System::Classes::TStream* Stream) : TVirtualReader(Stream) { }
	
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TTextReader(void) { }
	
};

#pragma pack(pop)

class DELPHICLASS TTextWriter;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TTextWriter : public TVirtualWriter
{
	typedef TVirtualWriter inherited;
	
private:
	int FIndentLevel;
	
protected:
	void __fastcall WriteLine(const System::UnicodeString valueType, const System::UnicodeString data);
	
public:
	__fastcall virtual TTextWriter(System::Classes::TStream* aStream);
	__fastcall virtual ~TTextWriter(void);
	virtual void __fastcall Write(const void *Buf, int Count);
	virtual void __fastcall WriteInteger(int anInteger);
	virtual void __fastcall WriteBoolean(bool aBoolean);
	virtual void __fastcall WriteString(const System::UnicodeString aString);
	virtual void __fastcall WriteFloat(const System::Extended aFloat);
	virtual void __fastcall WriteListBegin(void);
	virtual void __fastcall WriteListEnd(void);
};

#pragma pack(pop)

class DELPHICLASS TGLOwnedPersistent;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLOwnedPersistent : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
private:
	System::Classes::TPersistent* FOwner;
	
protected:
	DYNAMIC System::Classes::TPersistent* __fastcall GetOwner(void);
	
public:
	__fastcall virtual TGLOwnedPersistent(System::Classes::TPersistent* AOwner);
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TGLOwnedPersistent(void) { }
	
};

#pragma pack(pop)

class DELPHICLASS TGLInterfacedPersistent;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLInterfacedPersistent : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
protected:
	HRESULT __stdcall QueryInterface(const GUID &IID, /* out */ void *Obj);
	int __stdcall _AddRef(void);
	int __stdcall _Release(void);
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TGLInterfacedPersistent(void) { }
	
public:
	/* TObject.Create */ inline __fastcall TGLInterfacedPersistent(void) : System::Classes::TPersistent() { }
	
private:
	void *__IInterface;	/* System::IInterface */
	
public:
	#if defined(MANAGED_INTERFACE_OPERATORS)
	// {00000000-0000-0000-C000-000000000046}
	operator System::_di_IInterface()
	{
		System::_di_IInterface intf;
		GetInterface(intf);
		return intf;
	}
	#else
	operator System::IInterface*(void) { return (System::IInterface*)&__IInterface; }
	#endif
	
};

#pragma pack(pop)

class DELPHICLASS TGLInterfacedCollectionItem;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLInterfacedCollectionItem : public System::Classes::TCollectionItem
{
	typedef System::Classes::TCollectionItem inherited;
	
protected:
	virtual HRESULT __stdcall QueryInterface(const GUID &IID, /* out */ void *Obj);
	virtual int __stdcall _AddRef(void);
	virtual int __stdcall _Release(void);
public:
	/* TCollectionItem.Create */ inline __fastcall virtual TGLInterfacedCollectionItem(System::Classes::TCollection* Collection) : System::Classes::TCollectionItem(Collection) { }
	/* TCollectionItem.Destroy */ inline __fastcall virtual ~TGLInterfacedCollectionItem(void) { }
	
private:
	void *__IInterface;	/* System::IInterface */
	
public:
	#if defined(MANAGED_INTERFACE_OPERATORS)
	// {00000000-0000-0000-C000-000000000046}
	operator System::_di_IInterface()
	{
		System::_di_IInterface intf;
		GetInterface(intf);
		return intf;
	}
	#else
	operator System::IInterface*(void) { return (System::IInterface*)&__IInterface; }
	#endif
	
};

#pragma pack(pop)

class DELPHICLASS EInvalidFileSignature;
#pragma pack(push,4)
class PASCALIMPLEMENTATION EInvalidFileSignature : public System::Sysutils::Exception
{
	typedef System::Sysutils::Exception inherited;
	
public:
	/* Exception.Create */ inline __fastcall EInvalidFileSignature(const System::UnicodeString Msg) : System::Sysutils::Exception(Msg) { }
	/* Exception.CreateFmt */ inline __fastcall EInvalidFileSignature(const System::UnicodeString Msg, System::TVarRec const *Args, const int Args_Size) : System::Sysutils::Exception(Msg, Args, Args_Size) { }
	/* Exception.CreateRes */ inline __fastcall EInvalidFileSignature(NativeUInt Ident)/* overload */ : System::Sysutils::Exception(Ident) { }
	/* Exception.CreateRes */ inline __fastcall EInvalidFileSignature(System::PResStringRec ResStringRec)/* overload */ : System::Sysutils::Exception(ResStringRec) { }
	/* Exception.CreateResFmt */ inline __fastcall EInvalidFileSignature(NativeUInt Ident, System::TVarRec const *Args, const int Args_Size)/* overload */ : System::Sysutils::Exception(Ident, Args, Args_Size) { }
	/* Exception.CreateResFmt */ inline __fastcall EInvalidFileSignature(System::PResStringRec ResStringRec, System::TVarRec const *Args, const int Args_Size)/* overload */ : System::Sysutils::Exception(ResStringRec, Args, Args_Size) { }
	/* Exception.CreateHelp */ inline __fastcall EInvalidFileSignature(const System::UnicodeString Msg, int AHelpContext) : System::Sysutils::Exception(Msg, AHelpContext) { }
	/* Exception.CreateFmtHelp */ inline __fastcall EInvalidFileSignature(const System::UnicodeString Msg, System::TVarRec const *Args, const int Args_Size, int AHelpContext) : System::Sysutils::Exception(Msg, Args, Args_Size, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall EInvalidFileSignature(NativeUInt Ident, int AHelpContext)/* overload */ : System::Sysutils::Exception(Ident, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall EInvalidFileSignature(System::PResStringRec ResStringRec, int AHelpContext)/* overload */ : System::Sysutils::Exception(ResStringRec, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall EInvalidFileSignature(System::PResStringRec ResStringRec, System::TVarRec const *Args, const int Args_Size, int AHelpContext)/* overload */ : System::Sysutils::Exception(ResStringRec, Args, Args_Size, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall EInvalidFileSignature(NativeUInt Ident, System::TVarRec const *Args, const int Args_Size, int AHelpContext)/* overload */ : System::Sysutils::Exception(Ident, Args, Args_Size, AHelpContext) { }
	/* Exception.Destroy */ inline __fastcall virtual ~EInvalidFileSignature(void) { }
	
};

#pragma pack(pop)

class DELPHICLASS EFilerException;
#pragma pack(push,4)
class PASCALIMPLEMENTATION EFilerException : public System::Sysutils::Exception
{
	typedef System::Sysutils::Exception inherited;
	
public:
	/* Exception.Create */ inline __fastcall EFilerException(const System::UnicodeString Msg) : System::Sysutils::Exception(Msg) { }
	/* Exception.CreateFmt */ inline __fastcall EFilerException(const System::UnicodeString Msg, System::TVarRec const *Args, const int Args_Size) : System::Sysutils::Exception(Msg, Args, Args_Size) { }
	/* Exception.CreateRes */ inline __fastcall EFilerException(NativeUInt Ident)/* overload */ : System::Sysutils::Exception(Ident) { }
	/* Exception.CreateRes */ inline __fastcall EFilerException(System::PResStringRec ResStringRec)/* overload */ : System::Sysutils::Exception(ResStringRec) { }
	/* Exception.CreateResFmt */ inline __fastcall EFilerException(NativeUInt Ident, System::TVarRec const *Args, const int Args_Size)/* overload */ : System::Sysutils::Exception(Ident, Args, Args_Size) { }
	/* Exception.CreateResFmt */ inline __fastcall EFilerException(System::PResStringRec ResStringRec, System::TVarRec const *Args, const int Args_Size)/* overload */ : System::Sysutils::Exception(ResStringRec, Args, Args_Size) { }
	/* Exception.CreateHelp */ inline __fastcall EFilerException(const System::UnicodeString Msg, int AHelpContext) : System::Sysutils::Exception(Msg, AHelpContext) { }
	/* Exception.CreateFmtHelp */ inline __fastcall EFilerException(const System::UnicodeString Msg, System::TVarRec const *Args, const int Args_Size, int AHelpContext) : System::Sysutils::Exception(Msg, Args, Args_Size, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall EFilerException(NativeUInt Ident, int AHelpContext)/* overload */ : System::Sysutils::Exception(Ident, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall EFilerException(System::PResStringRec ResStringRec, int AHelpContext)/* overload */ : System::Sysutils::Exception(ResStringRec, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall EFilerException(System::PResStringRec ResStringRec, System::TVarRec const *Args, const int Args_Size, int AHelpContext)/* overload */ : System::Sysutils::Exception(ResStringRec, Args, Args_Size, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall EFilerException(NativeUInt Ident, System::TVarRec const *Args, const int Args_Size, int AHelpContext)/* overload */ : System::Sysutils::Exception(Ident, Args, Args_Size, AHelpContext) { }
	/* Exception.Destroy */ inline __fastcall virtual ~EFilerException(void) { }
	
};

#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------
extern PACKAGE void __fastcall RaiseFilerException(System::TClass aClass, int archiveVersion);
extern PACKAGE System::WideString __fastcall UTF8ToWideString(const System::AnsiString s);
}	/* namespace Persistentclasses */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_PERSISTENTCLASSES)
using namespace Persistentclasses;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// PersistentclassesHPP
