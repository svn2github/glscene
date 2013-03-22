// CodeGear C++Builder
// Copyright (c) 1995, 2012 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLFileGTS.pas' rev: 24.00 (Win32)

#ifndef GlfilegtsHPP
#define GlfilegtsHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <System.Classes.hpp>	// Pascal unit
#include <GLVectorFileObjects.hpp>	// Pascal unit
#include <ApplicationFileIO.hpp>	// Pascal unit
#include <BaseClasses.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Glfilegts
{
//-- type declarations -------------------------------------------------------
class DELPHICLASS TGLGTSVectorFile;
class PASCALIMPLEMENTATION TGLGTSVectorFile : public Glvectorfileobjects::TVectorFile
{
	typedef Glvectorfileobjects::TVectorFile inherited;
	
public:
	__classmethod virtual Applicationfileio::TDataFileCapabilities __fastcall Capabilities();
	DYNAMIC void __fastcall LoadFromStream(System::Classes::TStream* aStream);
public:
	/* TVectorFile.Create */ inline __fastcall virtual TGLGTSVectorFile(System::Classes::TPersistent* AOwner) : Glvectorfileobjects::TVectorFile(AOwner) { }
	
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TGLGTSVectorFile(void) { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Glfilegts */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLFILEGTS)
using namespace Glfilegts;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// GlfilegtsHPP
