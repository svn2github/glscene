// CodeGear C++Builder
// Copyright (c) 1995, 2012 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLFileQ3BSP.pas' rev: 24.00 (Win32)

#ifndef Glfileq3bspHPP
#define Glfileq3bspHPP

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

namespace Glfileq3bsp
{
//-- type declarations -------------------------------------------------------
class DELPHICLASS TGLQ3BSPVectorFile;
class PASCALIMPLEMENTATION TGLQ3BSPVectorFile : public Glvectorfileobjects::TVectorFile
{
	typedef Glvectorfileobjects::TVectorFile inherited;
	
public:
	__classmethod virtual Applicationfileio::TDataFileCapabilities __fastcall Capabilities();
	DYNAMIC void __fastcall LoadFromStream(System::Classes::TStream* aStream);
public:
	/* TVectorFile.Create */ inline __fastcall virtual TGLQ3BSPVectorFile(System::Classes::TPersistent* AOwner) : Glvectorfileobjects::TVectorFile(AOwner) { }
	
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TGLQ3BSPVectorFile(void) { }
	
};


//-- var, const, procedure ---------------------------------------------------
extern PACKAGE float vQ3BSPLightmapGammaCorrection;
extern PACKAGE float vQ3BSPLightmapBrightness;
extern PACKAGE bool vGLFileQ3BSPLoadMaterials;
}	/* namespace Glfileq3bsp */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLFILEQ3BSP)
using namespace Glfileq3bsp;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Glfileq3bspHPP
