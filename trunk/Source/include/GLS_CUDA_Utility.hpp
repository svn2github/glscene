// CodeGear C++Builder
// Copyright (c) 1995, 2012 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLS_CUDA_Utility.pas' rev: 24.00 (Win32)

#ifndef Gls_cuda_utilityHPP
#define Gls_cuda_utilityHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <Winapi.Windows.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Gls_cuda_utility
{
//-- type declarations -------------------------------------------------------
//-- var, const, procedure ---------------------------------------------------
#define CUTILDLL L"cutil32.dll"
extern PACKAGE char * __stdcall (*cutFindFilePath)(const char * filename, const char * executablePath);
extern PACKAGE bool __stdcall (*cutLoadPGMf)(const char * filename, System::PSingle &data, int &w, int &h);
extern PACKAGE bool __stdcall (*cutSavePGMf)(const char * filename, System::PSingle data, int w, int h);
extern PACKAGE bool __stdcall (*cutLoadPGMub)(const char * filename, System::PByte &data, int &w, int &h);
extern PACKAGE bool __stdcall (*cutLoadPPMub)(const char * filename, System::PByte &data, int &w, int &h);
extern PACKAGE bool __stdcall (*cutLoadPPM4ub)(const char * filename, System::PByte &data, int &w, int &h);
extern PACKAGE bool __stdcall (*cutLoadPGMi)(const char * filename, System::PInteger &data, int &w, int &h);
extern PACKAGE bool __stdcall (*cutLoadPGMs)(const char * filename, PWORD &data, int &w, int &h);
extern PACKAGE bool __stdcall (*cutSavePGMub)(const char * filename, System::PByte data, int w, int h);
extern PACKAGE bool __stdcall (*cutSavePPMub)(const char * filename, System::PByte data, int w, int h);
extern PACKAGE bool __stdcall (*cutSavePPM4ub)(const char * filename, System::PByte data, int w, int h);
extern PACKAGE bool __stdcall (*cutSavePGMi)(const char * filename, System::PInteger data, int w, int h);
extern PACKAGE bool __stdcall (*cutSavePGMs)(const char * filename, PWORD data, int w, int h);
extern PACKAGE bool __stdcall (*cutComparef)(const Winapi::Windows::PSingle reference, const Winapi::Windows::PSingle data, const unsigned len);
extern PACKAGE bool __stdcall (*cutComparei)(const System::PInteger reference, const System::PInteger data, const unsigned len);
extern PACKAGE bool __stdcall (*cutCompareuit)(const System::PInteger reference, const System::PInteger data, const unsigned len, const float epsilon, const float threshold);
extern PACKAGE bool __stdcall (*cutCompareub)(const System::PByte reference, const System::PByte data, const unsigned len);
extern PACKAGE bool __stdcall (*cutCompareubt)(const System::PByte reference, const System::PByte data, const unsigned len, const float epsilon, const float threshold);
extern PACKAGE bool __stdcall (*cutCompareube)(const System::PByte reference, const System::PByte data, const unsigned len, const float epsilon);
extern PACKAGE bool __stdcall (*cutComparefe)(const Winapi::Windows::PSingle reference, const Winapi::Windows::PSingle data, const unsigned len, const float epsilon);
extern PACKAGE bool __stdcall (*cutComparefet)(const Winapi::Windows::PSingle reference, const Winapi::Windows::PSingle data, const unsigned len, const float epsilon, const float threshold);
extern PACKAGE bool __stdcall (*cutCompareL2fe)(const Winapi::Windows::PSingle reference, const Winapi::Windows::PSingle data, const unsigned len, const float epsilon);
extern PACKAGE bool __stdcall (*cutCreateTimer)(unsigned &name);
extern PACKAGE bool __stdcall (*cutStartTimer)(const unsigned name);
extern PACKAGE bool __stdcall (*cutStopTimer)(const unsigned name);
extern PACKAGE bool __stdcall (*cutResetTimer)(const unsigned name);
extern PACKAGE bool __stdcall (*cutDeleteTimer)(const unsigned name);
extern PACKAGE float __stdcall (*cutGetTimerValue)(const unsigned name);
extern PACKAGE float __stdcall (*cutGetAverageTimerValue)(const unsigned name);
extern PACKAGE void __stdcall (*cutFree)(void * ptr);
extern PACKAGE bool __fastcall InitCUTIL(void);
extern PACKAGE void __fastcall CloseCUTIL(void);
extern PACKAGE bool __fastcall InitCUTILFromLibrary(const System::WideString LibName);
extern PACKAGE bool __fastcall IsCUTILInitialized(void);
}	/* namespace Gls_cuda_utility */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLS_CUDA_UTILITY)
using namespace Gls_cuda_utility;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Gls_cuda_utilityHPP
