// CodeGear C++Builder
// Copyright (c) 1995, 2012 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLSoundFileObjects.pas' rev: 24.00 (Win32)

#ifndef GlsoundfileobjectsHPP
#define GlsoundfileobjectsHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <System.Classes.hpp>	// Pascal unit
#include <Winapi.MMSystem.hpp>	// Pascal unit
#include <ApplicationFileIO.hpp>	// Pascal unit
#include <GLCrossPlatform.hpp>	// Pascal unit
#include <BaseClasses.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Glsoundfileobjects
{
//-- type declarations -------------------------------------------------------
class DELPHICLASS TGLSoundSampling;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLSoundSampling : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
private:
	System::Classes::TPersistent* FOwner;
	int FFrequency;
	int FNbChannels;
	int FBitsPerSample;
	
protected:
	DYNAMIC System::Classes::TPersistent* __fastcall GetOwner(void);
	
public:
	__fastcall TGLSoundSampling(System::Classes::TPersistent* AOwner);
	__fastcall virtual ~TGLSoundSampling(void);
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	int __fastcall BytesPerSec(void);
	int __fastcall BytesPerSample(void);
	tWAVEFORMATEX __fastcall WaveFormat(void);
	
__published:
	__property int Frequency = {read=FFrequency, write=FFrequency, default=22050};
	__property int NbChannels = {read=FNbChannels, write=FNbChannels, default=1};
	__property int BitsPerSample = {read=FBitsPerSample, write=FBitsPerSample, default=8};
};

#pragma pack(pop)

class DELPHICLASS TGLSoundFile;
class PASCALIMPLEMENTATION TGLSoundFile : public Applicationfileio::TDataFile
{
	typedef Applicationfileio::TDataFile inherited;
	
private:
	TGLSoundSampling* FSampling;
	
protected:
	void __fastcall SetSampling(TGLSoundSampling* const val);
	
public:
	__fastcall virtual TGLSoundFile(System::Classes::TPersistent* AOwner);
	__fastcall virtual ~TGLSoundFile(void);
	DYNAMIC void __fastcall PlayOnWaveOut(void);
	virtual void * __fastcall WAVData(void) = 0 ;
	virtual int __fastcall WAVDataSize(void) = 0 ;
	virtual void * __fastcall PCMData(void) = 0 ;
	virtual int __fastcall LengthInBytes(void) = 0 ;
	int __fastcall LengthInSamples(void);
	float __fastcall LengthInSec(void);
	__property TGLSoundSampling* Sampling = {read=FSampling, write=SetSampling};
};


typedef System::TMetaClass* TGLSoundFileClass;

struct DECLSPEC_DRECORD TGLSoundFileFormat
{
public:
	TGLSoundFileClass SoundFileClass;
	System::UnicodeString Extension;
	System::UnicodeString Description;
	int DescResID;
};


typedef TGLSoundFileFormat *PSoundFileFormat;

class DELPHICLASS TGLSoundFileFormatsList;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLSoundFileFormatsList : public System::Classes::TList
{
	typedef System::Classes::TList inherited;
	
public:
	__fastcall virtual ~TGLSoundFileFormatsList(void);
	HIDESBASE void __fastcall Add(const System::UnicodeString Ext, const System::UnicodeString Desc, int DescID, TGLSoundFileClass AClass);
	TGLSoundFileClass __fastcall FindExt(System::UnicodeString Ext);
	HIDESBASE void __fastcall Remove(TGLSoundFileClass AClass);
	void __fastcall BuildFilterStrings(TGLSoundFileClass SoundFileClass, /* out */ System::UnicodeString &Descriptions, /* out */ System::UnicodeString &Filters);
public:
	/* TObject.Create */ inline __fastcall TGLSoundFileFormatsList(void) : System::Classes::TList() { }
	
};

#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------
extern PACKAGE TGLSoundFileFormatsList* __fastcall GetGLSoundFileFormats(void);
extern PACKAGE void __fastcall RegisterSoundFileFormat(const System::UnicodeString AExtension, const System::UnicodeString ADescription, TGLSoundFileClass AClass);
extern PACKAGE void __fastcall UnregisterSoundFileClass(TGLSoundFileClass AClass);
}	/* namespace Glsoundfileobjects */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLSOUNDFILEOBJECTS)
using namespace Glsoundfileobjects;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// GlsoundfileobjectsHPP
