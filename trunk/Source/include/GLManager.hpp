// CodeGear C++Builder
// Copyright (c) 1995, 2012 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLManager.pas' rev: 24.00 (Win32)

#ifndef GlmanagerHPP
#define GlmanagerHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <System.Classes.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Glmanager
{
//-- type declarations -------------------------------------------------------
//-- var, const, procedure ---------------------------------------------------
extern PACKAGE void __fastcall RegisterManager(System::Classes::TComponent* aManager);
extern PACKAGE void __fastcall DeRegisterManager(System::Classes::TComponent* aManager);
extern PACKAGE System::Classes::TComponent* __fastcall FindManager(System::Classes::TComponentClass classType, const System::UnicodeString managerName);
}	/* namespace Glmanager */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLMANAGER)
using namespace Glmanager;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// GlmanagerHPP
