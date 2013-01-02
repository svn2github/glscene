// CodeGear C++Builder
// Copyright (c) 1995, 2012 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'AsyncTimer.pas' rev: 24.00 (Win32)

#ifndef AsynctimerHPP
#define AsynctimerHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <System.Classes.hpp>	// Pascal unit
#include <System.SyncObjs.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Asynctimer
{
//-- type declarations -------------------------------------------------------
class DELPHICLASS TAsyncTimer;
class PASCALIMPLEMENTATION TAsyncTimer : public System::Classes::TComponent
{
	typedef System::Classes::TComponent inherited;
	
private:
	bool FEnabled;
	System::Classes::TNotifyEvent FOnTimer;
	System::Classes::TThread* FTimerThread;
	System::Syncobjs::TCriticalSection* FMutex;
	
protected:
	void __fastcall SetEnabled(bool Value);
	System::Word __fastcall GetInterval(void);
	void __fastcall SetInterval(System::Word Value);
	System::Classes::TThreadPriority __fastcall GetThreadPriority(void);
	void __fastcall SetThreadPriority(System::Classes::TThreadPriority Value);
	void __fastcall DoTimer(void);
	
public:
	__fastcall virtual TAsyncTimer(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TAsyncTimer(void);
	
__published:
	__property bool Enabled = {read=FEnabled, write=SetEnabled, default=0};
	__property System::Word Interval = {read=GetInterval, write=SetInterval, default=1000};
	__property System::Classes::TNotifyEvent OnTimer = {read=FOnTimer, write=FOnTimer};
	__property System::Classes::TThreadPriority ThreadPriority = {read=GetThreadPriority, write=SetThreadPriority, default=6};
};


//-- var, const, procedure ---------------------------------------------------
static const System::Word cDEFAULT_TIMER_INTERVAL = System::Word(0x3e8);
}	/* namespace Asynctimer */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_ASYNCTIMER)
using namespace Asynctimer;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// AsynctimerHPP
