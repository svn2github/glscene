// CodeGear C++Builder
// Copyright (c) 1995, 2012 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLVfsPAK.pas' rev: 24.00 (Win32)

#ifndef GlvfspakHPP
#define GlvfspakHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <System.Classes.hpp>	// Pascal unit
#include <System.Contnrs.hpp>	// Pascal unit
#include <System.SysUtils.hpp>	// Pascal unit
#include <ApplicationFileIO.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Glvfspak
{
//-- type declarations -------------------------------------------------------
enum TZCompressedMode : unsigned char { Good, Fast, Auto, None };

struct DECLSPEC_DRECORD TPakHeader
{
public:
	System::StaticArray<char, 4> Signature;
	int DirOffset;
	int DirLength;
};


struct DECLSPEC_DRECORD TFileSection
{
public:
	System::StaticArray<char, 120> FileName;
	int FilePos;
	int FileLength;
};


class DELPHICLASS TGLVfsPAK;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLVfsPAK : public System::Classes::TComponent
{
	typedef System::Classes::TComponent inherited;
	
private:
	typedef System::DynamicArray<TPakHeader> _TGLVfsPAK__1;
	
	
private:
	System::Classes::TStringList* FPakFiles;
	TPakHeader FHeader;
	_TGLVfsPAK__1 FHeaderList;
	System::Classes::TFileStream* FStream;
	System::Contnrs::TObjectList* FStreamList;
	System::Classes::TStrings* FFiles;
	System::Contnrs::TObjectList* FFilesLists;
	System::UnicodeString FFileName;
	TZCompressedMode FCompressionLevel;
	bool FCompressed;
	int __fastcall GetFileCount(void);
	void __fastcall MakeFileList(void);
	int __fastcall GetStreamNumber(void);
	void __fastcall SetStreamNumber(int i);
	
public:
	__property System::Classes::TStringList* PakFiles = {read=FPakFiles};
	__property System::Classes::TStrings* Files = {read=FFiles};
	__property int ActivePakNum = {read=GetStreamNumber, write=SetStreamNumber, nodefault};
	__property int FileCount = {read=GetFileCount, nodefault};
	__property System::UnicodeString PakFileName = {read=FFileName};
	__property bool Compressed = {read=FCompressed, nodefault};
	__property TZCompressedMode CompressionLevel = {read=FCompressionLevel, nodefault};
	__fastcall virtual TGLVfsPAK(System::Classes::TComponent* AOwner)/* overload */;
	__fastcall TGLVfsPAK(System::Classes::TComponent* AOwner, const TZCompressedMode CbrMode)/* overload */;
	__fastcall virtual ~TGLVfsPAK(void);
	void __fastcall LoadFromFile(System::UnicodeString FileName, System::Word Mode);
	void __fastcall ClearPakFiles(void);
	bool __fastcall FileExists(System::UnicodeString FileName);
	System::Classes::TStream* __fastcall GetFile(int index)/* overload */;
	System::Classes::TStream* __fastcall GetFile(System::UnicodeString FileName)/* overload */;
	int __fastcall GetFileSize(int index)/* overload */;
	int __fastcall GetFileSize(System::UnicodeString FileName)/* overload */;
	void __fastcall AddFromStream(System::UnicodeString FileName, System::UnicodeString Path, System::Classes::TStream* F);
	void __fastcall AddFromFile(System::UnicodeString FileName, System::UnicodeString Path);
	void __fastcall AddEmptyFile(System::UnicodeString FileName, System::UnicodeString Path);
	void __fastcall RemoveFile(int index)/* overload */;
	void __fastcall RemoveFile(System::UnicodeString FileName)/* overload */;
	void __fastcall Extract(int index, System::UnicodeString NewName)/* overload */;
	void __fastcall Extract(System::UnicodeString FileName, System::UnicodeString NewName)/* overload */;
};

#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------
#define SIGN L"PACK"
#define SIGN_COMPRESSED L"PACZ"
extern PACKAGE TGLVfsPAK* ActiveVfsPAK;
extern PACKAGE System::Classes::TStream* __fastcall PAKCreateFileStream(const System::UnicodeString fileName, System::Word mode);
extern PACKAGE bool __fastcall PAKFileStreamExists(const System::UnicodeString fileName);
}	/* namespace Glvfspak */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLVFSPAK)
using namespace Glvfspak;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// GlvfspakHPP
