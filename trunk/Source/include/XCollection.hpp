// CodeGear C++Builder
// Copyright (c) 1995, 2012 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'XCollection.pas' rev: 24.00 (Win32)

#ifndef XcollectionHPP
#define XcollectionHPP

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
#include <PersistentClasses.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Xcollection
{
//-- type declarations -------------------------------------------------------
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

class DELPHICLASS TXCollectionItem;
class DELPHICLASS TXCollection;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TXCollectionItem : public Persistentclasses::TGLInterfacedPersistent
{
	typedef Persistentclasses::TGLInterfacedPersistent inherited;
	
private:
	TXCollection* FOwner;
	System::UnicodeString FName;
	
protected:
	virtual System::UnicodeString __fastcall GetName(void);
	virtual void __fastcall SetName(const System::UnicodeString val);
	DYNAMIC System::Classes::TPersistent* __fastcall GetOwner(void);
	virtual void __fastcall WriteToFiler(System::Classes::TWriter* writer);
	virtual void __fastcall ReadFromFiler(System::Classes::TReader* reader);
	DYNAMIC void __fastcall Loaded(void);
	void __fastcall RaiseFilerException(const int archiveVersion);
	
public:
	__fastcall virtual TXCollectionItem(TXCollection* aOwner);
	__fastcall virtual ~TXCollectionItem(void);
	DYNAMIC System::UnicodeString __fastcall GetNamePath(void);
	__property TXCollection* Owner = {read=FOwner};
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	void __fastcall MoveUp(void);
	void __fastcall MoveDown(void);
	int __fastcall Index(void);
	__classmethod virtual System::UnicodeString __fastcall FriendlyName();
	__classmethod virtual System::UnicodeString __fastcall FriendlyDescription();
	__classmethod virtual System::UnicodeString __fastcall ItemCategory();
	__classmethod virtual bool __fastcall UniqueItem();
	__classmethod virtual bool __fastcall CanAddTo(TXCollection* collection);
	
__published:
	__property System::UnicodeString Name = {read=FName, write=SetName};
};

#pragma pack(pop)

typedef System::TMetaClass* TXCollectionItemClass;

#pragma pack(push,4)
class PASCALIMPLEMENTATION TXCollection : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
public:
	TXCollectionItem* operator[](int index) { return Items[index]; }
	
private:
	System::Classes::TPersistent* FOwner;
	System::Classes::TList* FList;
	int FCount;
	int FArchiveVersion;
	
protected:
	TXCollectionItem* __fastcall GetItems(int index);
	DYNAMIC System::Classes::TPersistent* __fastcall GetOwner(void);
	void __fastcall ReadFromFiler(System::Classes::TReader* reader);
	void __fastcall WriteToFiler(System::Classes::TWriter* writer);
	
public:
	__fastcall virtual TXCollection(System::Classes::TPersistent* aOwner);
	__fastcall virtual ~TXCollection(void);
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	void __fastcall Loaded(void);
	__property System::Classes::TPersistent* Owner = {read=FOwner, write=FOwner};
	DYNAMIC System::UnicodeString __fastcall GetNamePath(void);
	__classmethod virtual TXCollectionItemClass __fastcall ItemsClass();
	__property TXCollectionItem* Items[int index] = {read=GetItems/*, default*/};
	__property int Count = {read=FCount, nodefault};
	int __fastcall Add(TXCollectionItem* anItem);
	TXCollectionItem* __fastcall GetOrCreate(TXCollectionItemClass anItem);
	void __fastcall Delete(int index);
	void __fastcall Remove(TXCollectionItem* anItem);
	void __fastcall Clear(void);
	int __fastcall IndexOf(TXCollectionItem* anItem);
	int __fastcall IndexOfClass(TXCollectionItemClass aClass);
	TXCollectionItem* __fastcall GetByClass(TXCollectionItemClass aClass);
	int __fastcall IndexOfName(const System::UnicodeString aName);
	virtual bool __fastcall CanAdd(TXCollectionItemClass aClass);
	__property int ArchiveVersion = {read=FArchiveVersion, nodefault};
};

#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------
extern PACKAGE System::ResourceString _cUnknownArchiveVersion;
#define Xcollection_cUnknownArchiveVersion System::LoadResourceString(&Xcollection::_cUnknownArchiveVersion)
extern PACKAGE void __fastcall RegisterXCollectionDestroyEvent(System::Classes::TNotifyEvent notifyEvent);
extern PACKAGE void __fastcall DeRegisterXCollectionDestroyEvent(System::Classes::TNotifyEvent notifyEvent);
extern PACKAGE void __fastcall RegisterXCollectionItemClass(TXCollectionItemClass aClass);
extern PACKAGE void __fastcall UnregisterXCollectionItemClass(TXCollectionItemClass aClass);
extern PACKAGE TXCollectionItemClass __fastcall FindXCollectionItemClass(const System::UnicodeString ClassName);
extern PACKAGE System::Classes::TList* __fastcall GetXCollectionItemClassesList(TXCollectionItemClass baseClass = 0x0);
extern PACKAGE void __fastcall GetXCollectionClassesList(System::Classes::TList* &ClassesList, TXCollectionItemClass baseClass = 0x0);
}	/* namespace Xcollection */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_XCOLLECTION)
using namespace Xcollection;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// XcollectionHPP
