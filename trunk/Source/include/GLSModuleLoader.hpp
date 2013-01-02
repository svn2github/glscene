// CodeGear C++Builder
// Copyright (c) 1995, 2012 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLSModuleLoader.pas' rev: 24.00 (Win32)

#ifndef GlsmoduleloaderHPP
#define GlsmoduleloaderHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <Winapi.Windows.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Glsmoduleloader
{
//-- type declarations -------------------------------------------------------
typedef NativeUInt TModuleHandle;

//-- var, const, procedure ---------------------------------------------------
static const NativeUInt INVALID_MODULEHANDLE_VALUE = NativeUInt(0x0);
extern PACKAGE bool __stdcall LoadModule(NativeUInt &Module, System::WideChar * FileName);
extern PACKAGE bool __stdcall LoadModuleEx(NativeUInt &Module, System::WideChar * FileName, unsigned Flags);
extern PACKAGE void __stdcall UnloadModule(NativeUInt &Module);
extern PACKAGE void * __stdcall GetModuleSymbol(NativeUInt Module, System::WideChar * SymbolName);
extern PACKAGE void * __stdcall GetModuleSymbolEx(NativeUInt Module, System::WideChar * SymbolName, bool &Accu);
extern PACKAGE bool __stdcall ReadModuleData(NativeUInt Module, System::WideChar * SymbolName, void *Buffer, unsigned Size);
extern PACKAGE bool __stdcall WriteModuleData(NativeUInt Module, System::WideChar * SymbolName, void *Buffer, unsigned Size);
}	/* namespace Glsmoduleloader */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLSMODULELOADER)
using namespace Glsmoduleloader;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// GlsmoduleloaderHPP
