// CodeGear C++Builder
// Copyright (c) 1995, 2012 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLSArchiveManager.pas' rev: 24.00 (Win32)

#ifndef GlsarchivemanagerHPP
#define GlsarchivemanagerHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <System.Classes.hpp>	// Pascal unit
#include <System.SysUtils.hpp>	// Pascal unit
#include <PersistentClasses.hpp>	// Pascal unit
#include <ApplicationFileIO.hpp>	// Pascal unit
#include <BaseClasses.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Glsarchivemanager
{
//-- type declarations -------------------------------------------------------
enum TCompressionLevel : unsigned char { clNone, clFastest, clDefault, clMax, clLevel1, clLevel2, clLevel3, clLevel4, clLevel5, clLevel6, clLevel7, clLevel8, clLevel9 };

class DELPHICLASS TBaseArchive;
class PASCALIMPLEMENTATION TBaseArchive : public Applicationfileio::TDataFile
{
	typedef Applicationfileio::TDataFile inherited;
	
protected:
	System::UnicodeString FFileName;
	System::Classes::TStrings* FContentList;
	TCompressionLevel FCompressionLevel;
	virtual void __fastcall SetCompressionLevel(TCompressionLevel aValue);
	
public:
	__fastcall virtual TBaseArchive(System::Classes::TPersistent* AOwner);
	__fastcall virtual ~TBaseArchive(void);
	__property System::Classes::TStrings* ContentList = {read=FContentList};
	__property TCompressionLevel CompressionLevel = {read=FCompressionLevel, write=SetCompressionLevel, default=0};
	virtual void __fastcall Clear(void) = 0 ;
	virtual bool __fastcall ContentExists(System::UnicodeString ContentName) = 0 ;
	virtual System::Classes::TStream* __fastcall GetContent(System::Classes::TStream* Stream, int index) = 0 /* overload */;
	virtual System::Classes::TStream* __fastcall GetContent(System::UnicodeString ContentName) = 0 /* overload */;
	virtual System::Classes::TStream* __fastcall GetContent(int index) = 0 /* overload */;
	virtual int __fastcall GetContentSize(int index) = 0 /* overload */;
	virtual int __fastcall GetContentSize(System::UnicodeString ContentName) = 0 /* overload */;
	virtual void __fastcall AddFromStream(System::UnicodeString ContentName, System::UnicodeString Path, System::Classes::TStream* FS) = 0 ;
	virtual void __fastcall AddFromFile(System::UnicodeString FileName, System::UnicodeString Path) = 0 ;
	virtual void __fastcall RemoveContent(int index) = 0 /* overload */;
	virtual void __fastcall RemoveContent(System::UnicodeString ContentName) = 0 /* overload */;
	virtual void __fastcall Extract(int index, System::UnicodeString NewName) = 0 /* overload */;
	virtual void __fastcall Extract(System::UnicodeString ContentName, System::UnicodeString NewName) = 0 /* overload */;
};


typedef System::TMetaClass* TBaseArchiveClass;

class DELPHICLASS TArchiveFileFormat;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TArchiveFileFormat : public System::TObject
{
	typedef System::TObject inherited;
	
public:
	TBaseArchiveClass BaseArchiveClass;
	System::UnicodeString Extension;
	System::UnicodeString Description;
	int DescResID;
public:
	/* TObject.Create */ inline __fastcall TArchiveFileFormat(void) : System::TObject() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TArchiveFileFormat(void) { }
	
};

#pragma pack(pop)

class DELPHICLASS TArchiveFileFormatsList;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TArchiveFileFormatsList : public Persistentclasses::TPersistentObjectList
{
	typedef Persistentclasses::TPersistentObjectList inherited;
	
public:
	__fastcall virtual ~TArchiveFileFormatsList(void);
	HIDESBASE void __fastcall Add(const System::UnicodeString Ext, const System::UnicodeString Desc, int DescID, TBaseArchiveClass AClass);
	TBaseArchiveClass __fastcall FindExt(System::UnicodeString ext);
	TBaseArchiveClass __fastcall FindFromFileName(const System::UnicodeString fileName);
	HIDESBASE void __fastcall Remove(TBaseArchiveClass AClass);
public:
	/* TPersistentObjectList.Create */ inline __fastcall virtual TArchiveFileFormatsList(void) : Persistentclasses::TPersistentObjectList() { }
	
public:
	/* TPersistentObject.CreateFromFiler */ inline __fastcall TArchiveFileFormatsList(Persistentclasses::TVirtualReader* reader) : Persistentclasses::TPersistentObjectList(reader) { }
	
};

#pragma pack(pop)

class DELPHICLASS TLibArchive;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TLibArchive : public System::Classes::TCollectionItem
{
	typedef System::Classes::TCollectionItem inherited;
	
private:
	TBaseArchive* vArchive;
	TBaseArchiveClass ArcClass;
	System::UnicodeString FFileName;
	System::UnicodeString FName;
	void __fastcall SetCompressionLevel(TCompressionLevel aValue);
	TCompressionLevel __fastcall GetCompressionLevel(void);
	System::Classes::TStrings* __fastcall GetContentList(void);
	void __fastcall SetName(const System::UnicodeString val);
	
protected:
	virtual System::UnicodeString __fastcall GetDisplayName(void);
	
public:
	__fastcall virtual TLibArchive(System::Classes::TCollection* ACollection);
	__fastcall virtual ~TLibArchive(void);
	__property TCompressionLevel CompressionLevel = {read=GetCompressionLevel, write=SetCompressionLevel, default=0};
	void __fastcall CreateArchive(System::UnicodeString FileName, bool OverwriteExistingFile = false);
	__property System::Classes::TStrings* ContentList = {read=GetContentList};
	void __fastcall LoadFromFile(System::UnicodeString aFileName)/* overload */;
	void __fastcall LoadFromFile(System::UnicodeString aFileName, System::UnicodeString aAchiverType)/* overload */;
	void __fastcall Clear(void);
	bool __fastcall ContentExists(System::UnicodeString aContentName);
	__property System::UnicodeString FileName = {read=FFileName};
	System::Classes::TStream* __fastcall GetContent(int aindex)/* overload */;
	System::Classes::TStream* __fastcall GetContent(System::UnicodeString aContentName)/* overload */;
	int __fastcall GetContentSize(int aindex)/* overload */;
	int __fastcall GetContentSize(System::UnicodeString aContentName)/* overload */;
	void __fastcall AddFromStream(System::UnicodeString aContentName, System::UnicodeString aPath, System::Classes::TStream* aF)/* overload */;
	void __fastcall AddFromStream(System::UnicodeString aContentName, System::Classes::TStream* aF)/* overload */;
	void __fastcall AddFromFile(System::UnicodeString aFileName, System::UnicodeString aPath)/* overload */;
	void __fastcall AddFromFile(System::UnicodeString aFileName)/* overload */;
	void __fastcall RemoveContent(int aindex)/* overload */;
	void __fastcall RemoveContent(System::UnicodeString aContentName)/* overload */;
	void __fastcall Extract(int aindex, System::UnicodeString aNewName)/* overload */;
	void __fastcall Extract(System::UnicodeString aContentName, System::UnicodeString aNewName)/* overload */;
	
__published:
	__property System::UnicodeString Name = {read=FName, write=SetName};
};

#pragma pack(pop)

class DELPHICLASS TLibArchives;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TLibArchives : public System::Classes::TOwnedCollection
{
	typedef System::Classes::TOwnedCollection inherited;
	
public:
	TLibArchive* operator[](int index) { return Items[index]; }
	
protected:
	void __fastcall SetItems(int index, TLibArchive* const val);
	TLibArchive* __fastcall GetItems(int index);
	
public:
	__fastcall TLibArchives(System::Classes::TComponent* AOwner);
	HIDESBASE System::Classes::TPersistent* __fastcall Owner(void);
	int __fastcall IndexOf(TLibArchive* const Item);
	HIDESBASE TLibArchive* __fastcall Add(void);
	HIDESBASE TLibArchive* __fastcall FindItemID(int ID);
	__property TLibArchive* Items[int index] = {read=GetItems, write=SetItems/*, default*/};
	TLibArchive* __fastcall GetArchiveByFileName(const System::UnicodeString AName);
	System::UnicodeString __fastcall GetFileNameOfArchive(TLibArchive* aValue);
	System::UnicodeString __fastcall MakeUniqueName(const System::UnicodeString nameRoot);
	TLibArchive* __fastcall GetLibArchiveByName(const System::UnicodeString AName);
	System::UnicodeString __fastcall GetNameOfLibArchive(TLibArchive* const Archive);
public:
	/* TCollection.Destroy */ inline __fastcall virtual ~TLibArchives(void) { }
	
};

#pragma pack(pop)

class DELPHICLASS TGLSArchiveManager;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLSArchiveManager : public System::Classes::TComponent
{
	typedef System::Classes::TComponent inherited;
	
private:
	TLibArchives* FArchives;
	void __fastcall SetArchives(TLibArchives* aValue);
	
public:
	__fastcall virtual TGLSArchiveManager(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TGLSArchiveManager(void);
	TLibArchive* __fastcall GetArchiveByFileName(const System::UnicodeString aName);
	System::UnicodeString __fastcall GetFileNameOfArchive(TLibArchive* const aArchive);
	System::Classes::TStream* __fastcall GetContent(System::UnicodeString aContentName);
	bool __fastcall ContentExists(System::UnicodeString aContentName);
	TLibArchive* __fastcall OpenArchive(System::UnicodeString aFileName)/* overload */;
	TLibArchive* __fastcall OpenArchive(System::UnicodeString aFileName, System::UnicodeString aAchiverType)/* overload */;
	void __fastcall CloseArchive(TLibArchive* aArchive);
	
__published:
	__property TLibArchives* Archives = {read=FArchives, write=SetArchives};
};

#pragma pack(pop)

class DELPHICLASS EInvalidArchiveFile;
#pragma pack(push,4)
class PASCALIMPLEMENTATION EInvalidArchiveFile : public System::Sysutils::Exception
{
	typedef System::Sysutils::Exception inherited;
	
public:
	/* Exception.Create */ inline __fastcall EInvalidArchiveFile(const System::UnicodeString Msg) : System::Sysutils::Exception(Msg) { }
	/* Exception.CreateFmt */ inline __fastcall EInvalidArchiveFile(const System::UnicodeString Msg, System::TVarRec const *Args, const int Args_Size) : System::Sysutils::Exception(Msg, Args, Args_Size) { }
	/* Exception.CreateRes */ inline __fastcall EInvalidArchiveFile(NativeUInt Ident)/* overload */ : System::Sysutils::Exception(Ident) { }
	/* Exception.CreateRes */ inline __fastcall EInvalidArchiveFile(System::PResStringRec ResStringRec)/* overload */ : System::Sysutils::Exception(ResStringRec) { }
	/* Exception.CreateResFmt */ inline __fastcall EInvalidArchiveFile(NativeUInt Ident, System::TVarRec const *Args, const int Args_Size)/* overload */ : System::Sysutils::Exception(Ident, Args, Args_Size) { }
	/* Exception.CreateResFmt */ inline __fastcall EInvalidArchiveFile(System::PResStringRec ResStringRec, System::TVarRec const *Args, const int Args_Size)/* overload */ : System::Sysutils::Exception(ResStringRec, Args, Args_Size) { }
	/* Exception.CreateHelp */ inline __fastcall EInvalidArchiveFile(const System::UnicodeString Msg, int AHelpContext) : System::Sysutils::Exception(Msg, AHelpContext) { }
	/* Exception.CreateFmtHelp */ inline __fastcall EInvalidArchiveFile(const System::UnicodeString Msg, System::TVarRec const *Args, const int Args_Size, int AHelpContext) : System::Sysutils::Exception(Msg, Args, Args_Size, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall EInvalidArchiveFile(NativeUInt Ident, int AHelpContext)/* overload */ : System::Sysutils::Exception(Ident, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall EInvalidArchiveFile(System::PResStringRec ResStringRec, int AHelpContext)/* overload */ : System::Sysutils::Exception(ResStringRec, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall EInvalidArchiveFile(System::PResStringRec ResStringRec, System::TVarRec const *Args, const int Args_Size, int AHelpContext)/* overload */ : System::Sysutils::Exception(ResStringRec, Args, Args_Size, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall EInvalidArchiveFile(NativeUInt Ident, System::TVarRec const *Args, const int Args_Size, int AHelpContext)/* overload */ : System::Sysutils::Exception(Ident, Args, Args_Size, AHelpContext) { }
	/* Exception.Destroy */ inline __fastcall virtual ~EInvalidArchiveFile(void) { }
	
};

#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------
extern PACKAGE TArchiveFileFormatsList* __fastcall GetArchiveFileFormats(void);
extern PACKAGE void __fastcall RegisterArchiveFormat(const System::UnicodeString AExtension, const System::UnicodeString ADescription, TBaseArchiveClass AClass);
extern PACKAGE void __fastcall UnregisterArchiveFormat(TBaseArchiveClass AClass);
extern PACKAGE TGLSArchiveManager* __fastcall GetArchiveManager(void);
extern PACKAGE System::Classes::TStream* __fastcall ArcCreateFileStream(const System::UnicodeString fileName, System::Word mode);
extern PACKAGE bool __fastcall ArcFileStreamExists(const System::UnicodeString fileName);
}	/* namespace Glsarchivemanager */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLSARCHIVEMANAGER)
using namespace Glsarchivemanager;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// GlsarchivemanagerHPP
