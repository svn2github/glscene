// CodeGear C++Builder
// Copyright (c) 1995, 2012 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLTimeEventsMgr.pas' rev: 24.00 (Win32)

#ifndef GltimeeventsmgrHPP
#define GltimeeventsmgrHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <GLCadencer.hpp>	// Pascal unit
#include <System.SysUtils.hpp>	// Pascal unit
#include <System.Classes.hpp>	// Pascal unit
#include <BaseClasses.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Gltimeeventsmgr
{
//-- type declarations -------------------------------------------------------
class DELPHICLASS TGLTimeEventsMGR;
class DELPHICLASS TTimeEvents;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLTimeEventsMGR : public Baseclasses::TGLUpdateAbleComponent
{
	typedef Baseclasses::TGLUpdateAbleComponent inherited;
	
private:
	Glcadencer::TGLCadencer* FCadencer;
	bool FEnabled;
	bool FFreeEventOnEnd;
	TTimeEvents* FEvents;
	
protected:
	virtual void __fastcall Notification(System::Classes::TComponent* AComponent, System::Classes::TOperation Operation);
	void __fastcall SetCadencer(Glcadencer::TGLCadencer* const val);
	void __fastcall SetEvents(TTimeEvents* const val);
	
public:
	__fastcall virtual TGLTimeEventsMGR(System::Classes::TComponent* aOwner);
	__fastcall virtual ~TGLTimeEventsMGR(void);
	virtual void __fastcall DoProgress(const Baseclasses::TProgressTimes &progressTime);
	void __fastcall Reset(void);
	
__published:
	__property Glcadencer::TGLCadencer* Cadencer = {read=FCadencer, write=SetCadencer};
	__property bool Enabled = {read=FEnabled, write=FEnabled, default=1};
	__property bool FreeEventOnEnd = {read=FFreeEventOnEnd, write=FFreeEventOnEnd, default=0};
	__property TTimeEvents* Events = {read=FEvents, write=SetEvents};
};

#pragma pack(pop)

class DELPHICLASS TTimeEvent;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TTimeEvents : public System::Classes::TCollection
{
	typedef System::Classes::TCollection inherited;
	
public:
	TTimeEvent* operator[](int index) { return Items[index]; }
	
protected:
	System::Classes::TComponent* Owner;
	DYNAMIC System::Classes::TPersistent* __fastcall GetOwner(void);
	void __fastcall SetItems(int index, TTimeEvent* const val);
	TTimeEvent* __fastcall GetItems(int index);
	
public:
	__fastcall TTimeEvents(System::Classes::TComponent* AOwner);
	HIDESBASE TTimeEvent* __fastcall Add(void);
	HIDESBASE TTimeEvent* __fastcall FindItemID(int ID);
	TTimeEvent* __fastcall EventByName(System::UnicodeString name);
	__property TTimeEvent* Items[int index] = {read=GetItems, write=SetItems/*, default*/};
public:
	/* TCollection.Destroy */ inline __fastcall virtual ~TTimeEvents(void) { }
	
};

#pragma pack(pop)

enum TTimeEventType : unsigned char { etOneShot, etContinuous, etPeriodic };

typedef void __fastcall (__closure *TTimeEventProc)(TTimeEvent* event);

class PASCALIMPLEMENTATION TTimeEvent : public System::Classes::TCollectionItem
{
	typedef System::Classes::TCollectionItem inherited;
	
private:
	System::UnicodeString FName;
	double FStartTime;
	double FEndTime;
	double FElapsedTime;
	double FPeriod;
	TTimeEventType FEventType;
	TTimeEventProc FOnEvent;
	bool FEnabled;
	unsigned FTickCount;
	void __fastcall SetEnabled(const bool Value);
	
protected:
	virtual System::UnicodeString __fastcall GetDisplayName(void);
	void __fastcall SetName(System::UnicodeString val);
	void __fastcall DoEvent(const double curTime);
	
public:
	__fastcall virtual TTimeEvent(System::Classes::TCollection* Collection);
	__fastcall virtual ~TTimeEvent(void);
	__property unsigned TickCount = {read=FTickCount, nodefault};
	__property double ElapsedTime = {read=FElapsedTime};
	
__published:
	__property System::UnicodeString Name = {read=FName, write=SetName};
	__property double StartTime = {read=FStartTime, write=FStartTime};
	__property double EndTime = {read=FEndTime, write=FEndTime};
	__property double Period = {read=FPeriod, write=FPeriod};
	__property TTimeEventType EventType = {read=FEventType, write=FEventType, default=0};
	__property TTimeEventProc OnEvent = {read=FOnEvent, write=FOnEvent};
	__property bool Enabled = {read=FEnabled, write=SetEnabled, default=1};
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Gltimeeventsmgr */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLTIMEEVENTSMGR)
using namespace Gltimeeventsmgr;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// GltimeeventsmgrHPP
