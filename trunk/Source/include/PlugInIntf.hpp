// CodeGear C++Builder
// Copyright (c) 1995, 2012 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'PlugInIntf.pas' rev: 24.00 (Win32)

#ifndef PluginintfHPP
#define PluginintfHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Pluginintf
{
//-- type declarations -------------------------------------------------------
enum TPIServiceType : unsigned char { stRaw, stObject, stBitmap, stTexture, stImport, stExport };

typedef System::Set<TPIServiceType, TPIServiceType::stRaw, TPIServiceType::stExport>  TPIServices;

typedef void __stdcall (*TEnumCallBack)(char * Name);

typedef void __stdcall (*TEnumResourceNames)(TPIServiceType Service, TEnumCallBack Callback);

typedef TPIServices __stdcall (*TGetServices)(void);

typedef char * __stdcall (*TGetVendor)(void);

typedef char * __stdcall (*TGetDescription)(void);

typedef char * __stdcall (*TGetVersion)(void);

//-- var, const, procedure ---------------------------------------------------
}	/* namespace Pluginintf */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_PLUGININTF)
using namespace Pluginintf;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// PluginintfHPP
