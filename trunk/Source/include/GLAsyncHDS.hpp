// CodeGear C++Builder
// Copyright (c) 1995, 2012 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLAsyncHDS.pas' rev: 24.00 (Win32)

#ifndef GlasynchdsHPP
#define GlasynchdsHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <System.Classes.hpp>	// Pascal unit
#include <GLHeightData.hpp>	// Pascal unit
#include <GLCrossPlatform.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Glasynchds
{
//-- type declarations -------------------------------------------------------
class DELPHICLASS TGLAsyncHDS;
typedef void __fastcall (__closure *TIdleEvent)(TGLAsyncHDS* Sender, bool TilesUpdated);

typedef void __fastcall (__closure *TNewTilePreparedEvent)(TGLAsyncHDS* Sender, Glheightdata::THeightData* heightData);

enum TUseDirtyTiles : unsigned char { dtNever, dtUntilReplaced, dtUntilAllReplaced };

class PASCALIMPLEMENTATION TGLAsyncHDS : public Glheightdata::THeightDataSourceFilter
{
	typedef Glheightdata::THeightDataSourceFilter inherited;
	
private:
	TIdleEvent FOnIdleEvent;
	TNewTilePreparedEvent FOnNewTilePrepared;
	TUseDirtyTiles FUseDirtyTiles;
	bool FTilesUpdated;
	
public:
	__fastcall virtual TGLAsyncHDS(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TGLAsyncHDS(void);
	virtual void __fastcall BeforePreparingData(Glheightdata::THeightData* heightData);
	virtual void __fastcall StartPreparingData(Glheightdata::THeightData* heightData);
	virtual void __fastcall ThreadIsIdle(void);
	void __fastcall NewTilePrepared(Glheightdata::THeightData* heightData);
	int __fastcall ThreadCount(void);
	void __fastcall WaitFor(int TimeOut = 0x7d0);
	bool __fastcall TilesUpdated(void);
	void __fastcall TilesUpdatedFlagReset(void);
	
__published:
	__property TIdleEvent OnIdle = {read=FOnIdleEvent, write=FOnIdleEvent};
	__property TNewTilePreparedEvent OnNewTilePrepared = {read=FOnNewTilePrepared, write=FOnNewTilePrepared};
	__property TUseDirtyTiles UseDirtyTiles = {read=FUseDirtyTiles, write=FUseDirtyTiles, nodefault};
	__property MaxThreads;
	__property Active;
};


class DELPHICLASS TGLAsyncHDThread;
class PASCALIMPLEMENTATION TGLAsyncHDThread : public Glheightdata::THeightDataThread
{
	typedef Glheightdata::THeightDataThread inherited;
	
public:
	TGLAsyncHDS* Owner;
	Glheightdata::THeightDataSource* HDS;
	virtual void __fastcall Execute(void);
	void __fastcall Sync(void);
public:
	/* THeightDataThread.Destroy */ inline __fastcall virtual ~TGLAsyncHDThread(void) { }
	
public:
	/* TThread.Create */ inline __fastcall TGLAsyncHDThread(void)/* overload */ : Glheightdata::THeightDataThread() { }
	/* TThread.Create */ inline __fastcall TGLAsyncHDThread(bool CreateSuspended)/* overload */ : Glheightdata::THeightDataThread(CreateSuspended) { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Glasynchds */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLASYNCHDS)
using namespace Glasynchds;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// GlasynchdsHPP
