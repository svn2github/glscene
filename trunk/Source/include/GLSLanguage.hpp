// CodeGear C++Builder
// Copyright (c) 1995, 2012 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLSLanguage.pas' rev: 24.00 (Win32)

#ifndef GlslanguageHPP
#define GlslanguageHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <System.Classes.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Glslanguage
{
//-- type declarations -------------------------------------------------------
typedef System::AnsiString UTF8String;

struct DECLSPEC_DRECORD TLanguageEntry
{
public:
	System::AnsiString ID;
	System::AnsiString Text;
};


typedef System::DynamicArray<TLanguageEntry> TLanguageEntryArray;

class DELPHICLASS TLanguage;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TLanguage : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	System::AnsiString FCurrentLanguageFile;
	TLanguageEntryArray Entry;
	System::AnsiString __fastcall EncodeToUTF8(System::AnsiString aValue);
	
public:
	int __fastcall FindID(const System::AnsiString ID);
	System::AnsiString __fastcall Translate(const System::AnsiString ID);
	void __fastcall LoadLanguageFromFile(const System::AnsiString Language);
	__property System::AnsiString CurrentLanguageFile = {read=FCurrentLanguageFile};
public:
	/* TObject.Create */ inline __fastcall TLanguage(void) : System::TObject() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TLanguage(void) { }
	
};

#pragma pack(pop)

class DELPHICLASS TLanguageExt;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TLanguageExt : public TLanguage
{
	typedef TLanguage inherited;
	
private:
	TLanguageEntry __fastcall GetEntry(int Index);
	void __fastcall SetEntry(int Index, const TLanguageEntry &aValue);
	int __fastcall GetCount(void);
	
public:
	void __fastcall AddConst(const System::AnsiString ID, const System::AnsiString Text);
	void __fastcall AddConsts(System::Classes::TStrings* aValues);
	void __fastcall ChangeConst(const System::AnsiString ID, const System::AnsiString Text);
	__property TLanguageEntry Items[int Index] = {read=GetEntry, write=SetEntry};
	__property int Count = {read=GetCount, nodefault};
	void __fastcall SaveLanguageFromFile(const System::AnsiString Language)/* overload */;
	void __fastcall SaveLanguageFromFile(void)/* overload */;
public:
	/* TObject.Create */ inline __fastcall TLanguageExt(void) : TLanguage() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TLanguageExt(void) { }
	
};

#pragma pack(pop)

class DELPHICLASS TGLSLanguage;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLSLanguage : public System::Classes::TComponent
{
	typedef System::Classes::TComponent inherited;
	
private:
	TLanguageExt* FLanguage;
	System::Classes::TStrings* FLanguageList;
	void __fastcall SetLanguage(TLanguageExt* aValue);
	
public:
	__fastcall virtual TGLSLanguage(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TGLSLanguage(void);
	void __fastcall LoadLanguageFromFile(const System::AnsiString Language);
	void __fastcall SaveLanguageFromFile(const System::AnsiString Language)/* overload */;
	void __fastcall SaveLanguageFromFile(void)/* overload */;
	System::AnsiString __fastcall Translate(const System::AnsiString ID);
	__property TLanguageExt* Language = {read=FLanguage, write=SetLanguage};
};

#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------
}	/* namespace Glslanguage */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLSLANGUAGE)
using namespace Glslanguage;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// GlslanguageHPP
