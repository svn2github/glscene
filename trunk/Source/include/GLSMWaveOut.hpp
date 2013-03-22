// CodeGear C++Builder
// Copyright (c) 1995, 2012 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLSMWaveOut.pas' rev: 24.00 (Win32)

#ifndef GlsmwaveoutHPP
#define GlsmwaveoutHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <System.Classes.hpp>	// Pascal unit
#include <GLSound.hpp>	// Pascal unit
#include <Winapi.MMSystem.hpp>	// Pascal unit
#include <GLSoundFileObjects.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Glsmwaveout
{
//-- type declarations -------------------------------------------------------
class DELPHICLASS TGLSMWaveOut;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLSMWaveOut : public Glsound::TGLSoundManager
{
	typedef Glsound::TGLSoundManager inherited;
	
protected:
	DYNAMIC bool __fastcall DoActivate(void);
	DYNAMIC void __fastcall DoDeActivate(void);
	virtual void __fastcall KillSource(Glsound::TGLBaseSoundSource* aSource);
	
public:
	__fastcall virtual TGLSMWaveOut(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TGLSMWaveOut(void);
	virtual void __fastcall UpdateSources(void);
	
__published:
	__property MaxChannels = {default=4};
};

#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------
extern PACKAGE NativeInt __fastcall PlayOnWaveOut(void * pcmData, int lengthInBytes, const tWAVEFORMATEX &waveFormat)/* overload */;
extern PACKAGE void __fastcall PlayOnWaveOut(void * pcmData, int lengthInBytes, Glsoundfileobjects::TGLSoundSampling* sampling)/* overload */;
}	/* namespace Glsmwaveout */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLSMWAVEOUT)
using namespace Glsmwaveout;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// GlsmwaveoutHPP
