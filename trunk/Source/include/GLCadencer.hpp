// CodeGear C++Builder
// Copyright (c) 1995, 2012 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLCadencer.pas' rev: 24.00 (Win32)

#ifndef GlcadencerHPP
#define GlcadencerHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <GLScene.hpp>	// Pascal unit
#include <System.Classes.hpp>	// Pascal unit
#include <GLCrossPlatform.hpp>	// Pascal unit
#include <BaseClasses.hpp>	// Pascal unit
#include <Vcl.Forms.hpp>	// Pascal unit
#include <Winapi.Windows.hpp>	// Pascal unit
#include <Winapi.Messages.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Glcadencer
{
//-- type declarations -------------------------------------------------------
enum TGLCadencerMode : unsigned char { cmManual, cmASAP, cmApplicationIdle };

enum TGLCadencerTimeReference : unsigned char { cmRTC, cmPerformanceCounter, cmExternal };

class DELPHICLASS TGLCadencer;
class PASCALIMPLEMENTATION TGLCadencer : public System::Classes::TComponent
{
	typedef System::Classes::TComponent inherited;
	
private:
	System::Classes::TList* FSubscribedCadenceableComponents;
	Glscene::TGLScene* FScene;
	double FTimeMultiplier;
	double lastTime;
	double downTime;
	double lastMultiplier;
	bool FEnabled;
	int FSleepLength;
	TGLCadencerMode FMode;
	TGLCadencerTimeReference FTimeReference;
	double FCurrentTime;
	double FOriginTime;
	double FMaxDeltaTime;
	double FMinDeltaTime;
	double FFixedDeltaTime;
	Baseclasses::TGLProgressEvent FOnProgress;
	Baseclasses::TGLProgressEvent FOnTotalProgress;
	int FProgressing;
	void __fastcall SetCurrentTime(const double Value);
	
protected:
	virtual void __fastcall Notification(System::Classes::TComponent* AComponent, System::Classes::TOperation Operation);
	bool __fastcall StoreTimeMultiplier(void);
	void __fastcall SetEnabled(const bool val);
	void __fastcall SetScene(Glscene::TGLScene* const val);
	void __fastcall SetMode(const TGLCadencerMode val);
	void __fastcall SetTimeReference(const TGLCadencerTimeReference val);
	void __fastcall SetTimeMultiplier(const double val);
	double __fastcall GetRawReferenceTime(void);
	void __fastcall RestartASAP(void);
	virtual void __fastcall Loaded(void);
	void __fastcall OnIdleEvent(System::TObject* Sender, bool &Done);
	
public:
	__fastcall virtual TGLCadencer(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TGLCadencer(void);
	void __fastcall Subscribe(Baseclasses::TGLCadenceAbleComponent* aComponent);
	void __fastcall UnSubscribe(Baseclasses::TGLCadenceAbleComponent* aComponent);
	void __fastcall Progress(void);
	double __fastcall GetCurrenttime(void);
	bool __fastcall IsBusy(void);
	void __fastcall Reset(void);
	__property double OriginTime = {read=FOriginTime, write=FOriginTime};
	__property double CurrentTime = {read=FCurrentTime, write=SetCurrentTime};
	
__published:
	__property Glscene::TGLScene* Scene = {read=FScene, write=SetScene};
	__property bool Enabled = {read=FEnabled, write=SetEnabled, default=1};
	__property TGLCadencerTimeReference TimeReference = {read=FTimeReference, write=SetTimeReference, default=1};
	__property double TimeMultiplier = {read=FTimeMultiplier, write=SetTimeMultiplier, stored=StoreTimeMultiplier};
	__property double MaxDeltaTime = {read=FMaxDeltaTime, write=FMaxDeltaTime};
	__property double MinDeltaTime = {read=FMinDeltaTime, write=FMinDeltaTime};
	__property double FixedDeltaTime = {read=FFixedDeltaTime, write=FFixedDeltaTime};
	__property TGLCadencerMode Mode = {read=FMode, write=SetMode, default=1};
	__property int SleepLength = {read=FSleepLength, write=FSleepLength, default=-1};
	__property Baseclasses::TGLProgressEvent OnProgress = {read=FOnProgress, write=FOnProgress};
	__property Baseclasses::TGLProgressEvent OnTotalProgress = {read=FOnTotalProgress, write=FOnTotalProgress};
};


class DELPHICLASS TGLCustomCadencedComponent;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLCustomCadencedComponent : public Baseclasses::TGLUpdateAbleComponent
{
	typedef Baseclasses::TGLUpdateAbleComponent inherited;
	
private:
	TGLCadencer* FCadencer;
	
protected:
	void __fastcall SetCadencer(TGLCadencer* const val);
	__property TGLCadencer* Cadencer = {read=FCadencer, write=SetCadencer};
	
public:
	__fastcall virtual ~TGLCustomCadencedComponent(void);
	virtual void __fastcall Notification(System::Classes::TComponent* AComponent, System::Classes::TOperation Operation);
public:
	/* TComponent.Create */ inline __fastcall virtual TGLCustomCadencedComponent(System::Classes::TComponent* AOwner) : Baseclasses::TGLUpdateAbleComponent(AOwner) { }
	
};

#pragma pack(pop)

class DELPHICLASS TGLCadencedComponent;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLCadencedComponent : public TGLCustomCadencedComponent
{
	typedef TGLCustomCadencedComponent inherited;
	
__published:
	__property Cadencer;
public:
	/* TGLCustomCadencedComponent.Destroy */ inline __fastcall virtual ~TGLCadencedComponent(void) { }
	
public:
	/* TComponent.Create */ inline __fastcall virtual TGLCadencedComponent(System::Classes::TComponent* AOwner) : TGLCustomCadencedComponent(AOwner) { }
	
};

#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------
}	/* namespace Glcadencer */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLCADENCER)
using namespace Glcadencer;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// GlcadencerHPP
