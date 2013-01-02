// CodeGear C++Builder
// Copyright (c) 1995, 2012 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLSLog.pas' rev: 24.00 (Win32)

#ifndef GlslogHPP
#define GlslogHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <Winapi.Windows.hpp>	// Pascal unit
#include <System.Classes.hpp>	// Pascal unit
#include <System.SysUtils.hpp>	// Pascal unit
#include <GLCrossPlatform.hpp>	// Pascal unit
#include <System.SyncObjs.hpp>	// Pascal unit
#include <Winapi.ShellAPI.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Glslog
{
//-- type declarations -------------------------------------------------------
enum TLogLevel : unsigned char { lkDebug, lkInfo, lkNotice, lkWarning, lkError, lkFatalError };

typedef System::Set<TLogLevel, TLogLevel::lkDebug, TLogLevel::lkFatalError>  TLogLevels;

typedef System::StaticArray<System::UnicodeString, 6> Glslog__1;

enum TLogTimeFormat : unsigned char { lfNone, lfDate, lfTime, lfDateTime, lfElapsed };

typedef System::TMetaClass* CLogSession;

class DELPHICLASS TLogSession;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TLogSession : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	typedef System::StaticArray<System::UnicodeString, 6> _TLogSession__1;
	
	
private:
	System::TextFile LogFile;
	System::UnicodeString LogFileName;
	TLogLevels FLogLevels;
	bool FEnabled;
	System::Syncobjs::TCriticalSection* FCriticalSection;
	System::StaticArray<int, 6> LogKindCount;
	void __fastcall SetMode(TLogLevels NewMode);
	
protected:
	_TLogSession__1 ModeTitles;
	TLogTimeFormat TimeFormat;
	unsigned StartedMs;
	virtual void __fastcall AppendLog(System::UnicodeString Desc, TLogLevel Level = (TLogLevel)(0x1));
	
public:
	__fastcall virtual TLogSession(const System::UnicodeString FileName, TLogTimeFormat ATimeFormat, TLogLevels ALevels);
	__fastcall virtual ~TLogSession(void);
	void __fastcall Log(const System::UnicodeString Desc, TLogLevel Level = (TLogLevel)(0x1));
	void __fastcall LogDebug(const System::UnicodeString Desc);
	void __fastcall LogInfo(const System::UnicodeString Desc);
	void __fastcall LogNotice(const System::UnicodeString Desc);
	void __fastcall LogWarning(const System::UnicodeString Desc);
	void __fastcall LogError(const System::UnicodeString Desc);
	void __fastcall LogFatalError(const System::UnicodeString Desc);
	void __fastcall LogDebugFmt(const System::UnicodeString Desc, System::TVarRec const *Args, const int Args_Size);
	void __fastcall LogInfoFmt(const System::UnicodeString Desc, System::TVarRec const *Args, const int Args_Size);
	void __fastcall LogNoticeFmt(const System::UnicodeString Desc, System::TVarRec const *Args, const int Args_Size);
	void __fastcall LogWarningFmt(const System::UnicodeString Desc, System::TVarRec const *Args, const int Args_Size);
	void __fastcall LogErrorFmt(const System::UnicodeString Desc, System::TVarRec const *Args, const int Args_Size);
	void __fastcall LogFatalErrorFmt(const System::UnicodeString Desc, System::TVarRec const *Args, const int Args_Size);
	__property TLogLevels LogLevels = {read=FLogLevels, write=SetMode, nodefault};
	__property bool Enabled = {read=FEnabled, write=FEnabled, nodefault};
public:
	/* TObject.Create */ inline __fastcall TLogSession(void) : System::TObject() { }
	
};

#pragma pack(pop)

class DELPHICLASS TGLSLogger;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLSLogger : public System::Classes::TComponent
{
	typedef System::Classes::TComponent inherited;
	
private:
	bool FReplaceAssertion;
	TLogTimeFormat FTimeFormat;
	TLogLevels FLogLevels;
	TLogSession* FLog;
	void __fastcall SetReplaceAssertion(bool Value);
	TLogSession* __fastcall GetLog(void);
	
public:
	__fastcall virtual TGLSLogger(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TGLSLogger(void);
	void __fastcall DoPrimary(void);
	__property TLogSession* Log = {read=GetLog};
	
__published:
	__property bool ReplaceAssertion = {read=FReplaceAssertion, write=SetReplaceAssertion, default=0};
	__property TLogTimeFormat TimeFormat = {read=FTimeFormat, write=FTimeFormat, default=4};
	__property TLogLevels LogLevels = {read=FLogLevels, write=FLogLevels, default=63};
};

#pragma pack(pop)

typedef void __fastcall (*TIDELogProc)(const System::UnicodeString AMsg);

//-- var, const, procedure ---------------------------------------------------
extern PACKAGE System::StaticArray<int, 6> llMessageLimit;
extern PACKAGE Glslog__1 lkPrefix;
extern PACKAGE TLogLevels llMax;
extern PACKAGE TLogLevels llMedium;
extern PACKAGE TLogLevels llMin;
extern PACKAGE TLogSession* GLSLogger;
extern PACKAGE TIDELogProc vIDELogProc;
extern PACKAGE TLogSession* __fastcall UserLog(void);
extern PACKAGE bool __fastcall SkipBeforeSTR(System::TextFile &TextFile, System::UnicodeString SkipSTR);
extern PACKAGE System::UnicodeString __fastcall ReadLine(System::TextFile &TextFile);
}	/* namespace Glslog */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLSLOG)
using namespace Glslog;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// GlslogHPP
