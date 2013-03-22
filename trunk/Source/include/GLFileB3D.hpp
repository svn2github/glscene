// CodeGear C++Builder
// Copyright (c) 1995, 2012 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLFileB3D.pas' rev: 24.00 (Win32)

#ifndef Glfileb3dHPP
#define Glfileb3dHPP

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
#include <FileB3D.hpp>	// Pascal unit
#include <TypesB3D.hpp>	// Pascal unit
#include <BaseClasses.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Glfileb3d
{
//-- type declarations -------------------------------------------------------
class DELPHICLASS TGLB3DVectorFile;
class PASCALIMPLEMENTATION TGLB3DVectorFile : public Glvectorfileobjects::TVectorFile
{
	typedef Glvectorfileobjects::TVectorFile inherited;
	
public:
	__classmethod virtual Applicationfileio::TDataFileCapabilities __fastcall Capabilities();
	DYNAMIC void __fastcall LoadFromStream(System::Classes::TStream* AStream);
public:
	/* TVectorFile.Create */ inline __fastcall virtual TGLB3DVectorFile(System::Classes::TPersistent* AOwner) : Glvectorfileobjects::TVectorFile(AOwner) { }
	
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TGLB3DVectorFile(void) { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Glfileb3d */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLFILEB3D)
using namespace Glfileb3d;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Glfileb3dHPP
