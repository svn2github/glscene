// CodeGear C++Builder
// Copyright (c) 1995, 2012 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'ApplicationFileIO.pas' rev: 24.00 (Win32)

#ifndef ApplicationfileioHPP
#define ApplicationfileioHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <System.Classes.hpp>	// Pascal unit
#include <System.SysUtils.hpp>	// Pascal unit
#include <BaseClasses.hpp>	// Pascal unit
#include <Winapi.Windows.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Applicationfileio
{
//-- type declarations -------------------------------------------------------
enum TGLSApplicationResource : unsigned char { aresNone, aresSplash, aresTexture, aresMaterial, aresSampler, aresFont, aresMesh };

typedef System::Classes::TStream* __fastcall (*TAFIOCreateFileStream)(const System::UnicodeString fileName, System::Word mode);

typedef bool __fastcall (*TAFIOFileStreamExists)(const System::UnicodeString fileName);

typedef void __fastcall (__closure *TAFIOFileStreamEvent)(const System::UnicodeString fileName, System::Word mode, System::Classes::TStream* &stream);

typedef bool __fastcall (__closure *TAFIOFileStreamExistsEvent)(const System::UnicodeString fileName);

class DELPHICLASS TApplicationFileIO;
class PASCALIMPLEMENTATION TApplicationFileIO : public System::Classes::TComponent
{
	typedef System::Classes::TComponent inherited;
	
private:
	TAFIOFileStreamEvent FOnFileStream;
	TAFIOFileStreamExistsEvent FOnFileStreamExists;
	
public:
	__fastcall virtual TApplicationFileIO(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TApplicationFileIO(void);
	
__published:
	__property TAFIOFileStreamEvent OnFileStream = {read=FOnFileStream, write=FOnFileStream};
	__property TAFIOFileStreamExistsEvent OnFileStreamExists = {read=FOnFileStreamExists, write=FOnFileStreamExists};
};


enum TDataFileCapability : unsigned char { dfcRead, dfcWrite };

typedef System::Set<TDataFileCapability, TDataFileCapability::dfcRead, TDataFileCapability::dfcWrite>  TDataFileCapabilities;

class DELPHICLASS TDataFile;
class PASCALIMPLEMENTATION TDataFile : public Baseclasses::TGLUpdateAbleObject
{
	typedef Baseclasses::TGLUpdateAbleObject inherited;
	
private:
	System::UnicodeString FResourceName;
	void __fastcall SetResourceName(const System::UnicodeString AName);
	
public:
	__classmethod virtual TDataFileCapabilities __fastcall Capabilities();
	DYNAMIC TDataFile* __fastcall CreateCopy(System::Classes::TPersistent* AOwner);
	DYNAMIC void __fastcall LoadFromFile(const System::UnicodeString fileName);
	DYNAMIC void __fastcall SaveToFile(const System::UnicodeString fileName);
	DYNAMIC void __fastcall LoadFromStream(System::Classes::TStream* stream);
	DYNAMIC void __fastcall SaveToStream(System::Classes::TStream* stream);
	DYNAMIC void __fastcall Initialize(void);
	__property System::UnicodeString ResourceName = {read=FResourceName, write=SetResourceName};
public:
	/* TGLUpdateAbleObject.Create */ inline __fastcall virtual TDataFile(System::Classes::TPersistent* AOwner) : Baseclasses::TGLUpdateAbleObject(AOwner) { }
	
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TDataFile(void) { }
	
};


typedef System::TMetaClass* TDataFileClass;

typedef System::Classes::TResourceStream TGLSResourceStream;

//-- var, const, procedure ---------------------------------------------------
#define GLS_RC_DDS_Type (System::WideChar *)(0xa)
#define GLS_RC_JPG_Type (System::WideChar *)(0xa)
#define GLS_RC_XML_Type (System::WideChar *)(0xa)
#define GLS_RC_String_Type (System::WideChar *)(0xa)
extern PACKAGE TAFIOCreateFileStream vAFIOCreateFileStream;
extern PACKAGE TAFIOFileStreamExists vAFIOFileStreamExists;
extern PACKAGE bool __fastcall ApplicationFileIODefined(void);
extern PACKAGE System::Classes::TStream* __fastcall CreateFileStream(const System::UnicodeString fileName, System::Word mode = (System::Word)(0x40));
extern PACKAGE bool __fastcall FileStreamExists(const System::UnicodeString fileName);
extern PACKAGE System::Classes::TResourceStream* __fastcall CreateResourceStream(const System::UnicodeString ResName, System::WideChar * ResType);
extern PACKAGE TGLSApplicationResource __fastcall StrToGLSResType(const System::UnicodeString AStrRes);
}	/* namespace Applicationfileio */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_APPLICATIONFILEIO)
using namespace Applicationfileio;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// ApplicationfileioHPP
