// CodeGear C++Builder
// Copyright (c) 1995, 2012 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLFileMD2.pas' rev: 24.00 (Win32)

#ifndef Glfilemd2HPP
#define Glfilemd2HPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <System.Classes.hpp>	// Pascal unit
#include <System.SysUtils.hpp>	// Pascal unit
#include <GLVectorFileObjects.hpp>	// Pascal unit
#include <ApplicationFileIO.hpp>	// Pascal unit
#include <FileMD2.hpp>	// Pascal unit
#include <TypesMD2.hpp>	// Pascal unit
#include <BaseClasses.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Glfilemd2
{
//-- type declarations -------------------------------------------------------
class DELPHICLASS TGLMD2VectorFile;
class PASCALIMPLEMENTATION TGLMD2VectorFile : public Glvectorfileobjects::TVectorFile
{
	typedef Glvectorfileobjects::TVectorFile inherited;
	
public:
	__classmethod virtual Applicationfileio::TDataFileCapabilities __fastcall Capabilities();
	DYNAMIC void __fastcall LoadFromStream(System::Classes::TStream* aStream);
public:
	/* TVectorFile.Create */ inline __fastcall virtual TGLMD2VectorFile(System::Classes::TPersistent* AOwner) : Glvectorfileobjects::TVectorFile(AOwner) { }
	
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TGLMD2VectorFile(void) { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Glfilemd2 */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLFILEMD2)
using namespace Glfilemd2;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Glfilemd2HPP
