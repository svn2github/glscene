// CodeGear C++Builder
// Copyright (c) 1995, 2012 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLFileLWO.pas' rev: 24.00 (Win32)

#ifndef GlfilelwoHPP
#define GlfilelwoHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <System.Classes.hpp>	// Pascal unit
#include <GLVectorFileObjects.hpp>	// Pascal unit
#include <LWObjects.hpp>	// Pascal unit
#include <ApplicationFileIO.hpp>	// Pascal unit
#include <BaseClasses.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Glfilelwo
{
//-- type declarations -------------------------------------------------------
class DELPHICLASS TGLLWOVectorFile;
class PASCALIMPLEMENTATION TGLLWOVectorFile : public Glvectorfileobjects::TVectorFile
{
	typedef Glvectorfileobjects::TVectorFile inherited;
	
private:
	Lwobjects::TLWObjectFile* FLWO;
	Lwobjects::TLWPnts* FPnts;
	void __fastcall AddLayr(Lwobjects::TLWLayr* Layr, Lwobjects::TLWObjectFile* LWO);
	void __fastcall AddSurf(Lwobjects::TLWSurf* Surf, Lwobjects::TLWObjectFile* LWO);
	void __fastcall AddPnts(Lwobjects::TLWPnts* Pnts, Glvectorfileobjects::TMeshObject* Mesh);
	void __fastcall AddPols(Lwobjects::TLWPols* Pols, Glvectorfileobjects::TMeshObject* Mesh);
	void __fastcall AddVMap(Lwobjects::TLWVMap* VMap, Glvectorfileobjects::TMeshObject* Mesh);
	
public:
	DYNAMIC void __fastcall LoadFromStream(System::Classes::TStream* aStream);
public:
	/* TVectorFile.Create */ inline __fastcall virtual TGLLWOVectorFile(System::Classes::TPersistent* AOwner) : Glvectorfileobjects::TVectorFile(AOwner) { }
	
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TGLLWOVectorFile(void) { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Glfilelwo */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLFILELWO)
using namespace Glfilelwo;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// GlfilelwoHPP
