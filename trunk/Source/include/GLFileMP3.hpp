// CodeGear C++Builder
// Copyright (c) 1995, 2012 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLFileMP3.pas' rev: 24.00 (Win32)

#ifndef Glfilemp3HPP
#define Glfilemp3HPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <System.Classes.hpp>	// Pascal unit
#include <ApplicationFileIO.hpp>	// Pascal unit
#include <GLSoundFileObjects.hpp>	// Pascal unit
#include <BaseClasses.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Glfilemp3
{
//-- type declarations -------------------------------------------------------
class DELPHICLASS TGLMP3File;
class PASCALIMPLEMENTATION TGLMP3File : public Glsoundfileobjects::TGLSoundFile
{
	typedef Glsoundfileobjects::TGLSoundFile inherited;
	
private:
	typedef System::DynamicArray<System::Byte> _TGLMP3File__1;
	
	
private:
	_TGLMP3File__1 data;
	
public:
	DYNAMIC Applicationfileio::TDataFile* __fastcall CreateCopy(System::Classes::TPersistent* AOwner);
	__classmethod virtual Applicationfileio::TDataFileCapabilities __fastcall Capabilities();
	DYNAMIC void __fastcall LoadFromStream(System::Classes::TStream* Stream);
	DYNAMIC void __fastcall SaveToStream(System::Classes::TStream* Stream);
	DYNAMIC void __fastcall PlayOnWaveOut(void);
	virtual void * __fastcall WAVData(void);
	virtual int __fastcall WAVDataSize(void);
	virtual void * __fastcall PCMData(void);
	virtual int __fastcall LengthInBytes(void);
public:
	/* TGLSoundFile.Create */ inline __fastcall virtual TGLMP3File(System::Classes::TPersistent* AOwner) : Glsoundfileobjects::TGLSoundFile(AOwner) { }
	/* TGLSoundFile.Destroy */ inline __fastcall virtual ~TGLMP3File(void) { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Glfilemp3 */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLFILEMP3)
using namespace Glfilemp3;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Glfilemp3HPP
