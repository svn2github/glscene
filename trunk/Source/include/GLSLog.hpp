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
#include <Vcl.Dialogs.hpp>	// Pascal unit
#include <Winapi.Windows.hpp>	// Pascal unit
#include <System.StrUtils.hpp>	// Pascal unit
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

enum TLogMessageLimitAction : unsigned char { mlaContinue, mlaStopLogging, mlaHalt };

typedef System::StaticArray<System::UnicodeString, 6> Glslog__1;

enum TLogTimeFormat : unsigned char { lfNone, lfDate, lfTime, lfTimeExact, lfDateTime, lfElapsed };

enum TLogBufferingMode : unsigned char { lbmWriteEmidiatly, lbmWritePeriodically, lbmWriteInTheEnd };

typedef System::TMetaClass* CLogSession;

class DELPHICLASS TLogBufferFlushThread;
class DELPHICLASS TLogSession;
class PASCALIMPLEMENTATION TLogBufferFlushThread : public System::Classes::TThread
{
	typedef System::Classes::TThread inherited;
	
private:
	TLogSession* FParent;
	
protected:
	virtual void __fastcall Execute(void);
	
public:
	__fastcall TLogBufferFlushThread(TLogSession* const AParent);
public:
	/* TThread.Destroy */ inline __fastcall virtual ~TLogBufferFlushThread(void) { }
	
};


class DELPHICLASS TLogCheckSizeThread;
class PASCALIMPLEMENTATION TLogCheckSizeThread : public System::Classes::TThread
{
	typedef System::Classes::TThread inherited;
	
private:
	TLogSession* FParent;
	
protected:
	virtual void __fastcall Execute(void);
	
public:
	__fastcall TLogCheckSizeThread(TLogSession* const AParent);
public:
	/* TThread.Destroy */ inline __fastcall virtual ~TLogCheckSizeThread(void) { }
	
};


#pragma pack(push,4)
class PASCALIMPLEMENTATION TLogSession : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	typedef System::StaticArray<System::UnicodeString, 6> _TLogSession__1;
	
	
private:
	System::Classes::TStringList* FBuffer;
	bool FBuffered;
	TLogBufferFlushThread* FBufferProcessingThread;
	TLogCheckSizeThread* FCheckLogSizeThread;
	int FFlushBufferPeriod;
	System::TextFile FLogFile;
	System::UnicodeString FOriginalLogFileName;
	System::UnicodeString FCurrentLogFileName;
	System::Classes::TStringList* FUsedLogFileNames;
	TLogLevels FLogLevels;
	bool FEnabled;
	System::Syncobjs::TCriticalSection* FBufferCriticalSection;
	System::Syncobjs::TCriticalSection* FFileAccessCriticalSection;
	_TLogSession__1 FModeTitles;
	System::StaticArray<int, 6> FLogKindCount;
	bool FLogThreadId;
	TLogMessageLimitAction FMessageLimitAction;
	TLogTimeFormat FTimeFormat;
	unsigned FStartedMs;
	int FLogFileMaxSize;
	int FCheckFileSizePeriod;
	TLogLevels FDisplayLogOnExitIfItContains;
	void __fastcall SetBuffered(const bool Value);
	void __fastcall SetMode(const TLogLevels NewMode);
	void __fastcall ChangeBufferedState(void);
	void __fastcall SetEnabled(const bool Value);
	void __fastcall SetLogFileMaxSize(const int Value);
	
protected:
	void __fastcall PrintLogLevels(void);
	void __fastcall PrintLogStatistics(void);
	bool __fastcall AttachLogFile(const System::UnicodeString AFileName, const bool AResetFile = true);
	void __fastcall ClearOldLogs(void);
	void __fastcall SaveOldLogs(const System::UnicodeString ACurrentLogFileName);
	void __fastcall CreateNewLogFileIfNeeded(void);
	void __fastcall AppendLog(const System::UnicodeString AString, const TLogLevel ALevel, const bool ALogTime = true);
	virtual bool __fastcall DoWriteToLog(const System::UnicodeString AString);
	virtual bool __fastcall DoWriteBufferToLog(void);
	virtual bool __fastcall DoResetLog(void);
	
public:
	__fastcall virtual TLogSession(const System::UnicodeString AFileName, const TLogTimeFormat ATimeFormat, const TLogLevels ALevels, const bool ALogThreadId, const bool ABuffered, const int AMaxSize, const bool ASaveOldLogs);
	__fastcall virtual ~TLogSession(void);
	void __fastcall Log(const System::UnicodeString Desc, const TLogLevel Level = (TLogLevel)(0x1));
	void __fastcall LogAdv(System::TVarRec const *args, const int args_Size, const TLogLevel ALevel = (TLogLevel)(0x4));
	void __fastcall LogException(System::Sysutils::Exception* const E, const System::UnicodeString aFunctionName, System::TVarRec const *args, const int args_Size, const TLogLevel ALevel = (TLogLevel)(0x4));
	void __fastcall LogDebug(const System::UnicodeString Desc);
	void __fastcall LogInfo(const System::UnicodeString Desc);
	void __fastcall LogNotice(const System::UnicodeString Desc);
	void __fastcall LogWarning(const System::UnicodeString Desc);
	void __fastcall LogError(const System::UnicodeString Desc);
	void __fastcall LogFatalError(const System::UnicodeString Desc);
	void __fastcall LogEmtryLine(void);
	void __fastcall LogDebugFmt(const System::UnicodeString Desc, System::TVarRec const *Args, const int Args_Size);
	void __fastcall LogInfoFmt(const System::UnicodeString Desc, System::TVarRec const *Args, const int Args_Size);
	void __fastcall LogNoticeFmt(const System::UnicodeString Desc, System::TVarRec const *Args, const int Args_Size);
	void __fastcall LogWarningFmt(const System::UnicodeString Desc, System::TVarRec const *Args, const int Args_Size);
	void __fastcall LogErrorFmt(const System::UnicodeString Desc, System::TVarRec const *Args, const int Args_Size);
	void __fastcall LogFatalErrorFmt(const System::UnicodeString Desc, System::TVarRec const *Args, const int Args_Size);
	void __fastcall DisplayLog(void);
	void __fastcall FlushBuffer(void);
	__property TLogLevels LogLevels = {read=FLogLevels, write=SetMode, nodefault};
	__property bool Enabled = {read=FEnabled, write=SetEnabled, nodefault};
	__property bool Buffered = {read=FBuffered, write=SetBuffered, default=0};
	__property int FlushBufferPeriod = {read=FFlushBufferPeriod, write=FFlushBufferPeriod, nodefault};
	__property bool LogThreadId = {read=FLogThreadId, write=FLogThreadId, nodefault};
	__property TLogMessageLimitAction MessageLimitAction = {read=FMessageLimitAction, write=FMessageLimitAction, default=2};
	__property TLogLevels DisplayLogOnExitIfItContains = {read=FDisplayLogOnExitIfItContains, write=FDisplayLogOnExitIfItContains, nodefault};
	__property int LogFileMaxSize = {read=FLogFileMaxSize, write=SetLogFileMaxSize, nodefault};
	__property int CheckFileSizePeriod = {read=FCheckFileSizePeriod, write=FCheckFileSizePeriod, nodefault};
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
	__property TLogTimeFormat TimeFormat = {read=FTimeFormat, write=FTimeFormat, default=5};
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
extern PACKAGE TIDELogProc vIDELogProc;
extern PACKAGE TLogSession* __fastcall GLSLogger(void);
extern PACKAGE void __fastcall UseCustomGLSLogger(TLogSession* const ALogger);
extern PACKAGE System::UnicodeString __fastcall ConstArrayToString(System::TVarRec const *Elements, const int Elements_Size);
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
