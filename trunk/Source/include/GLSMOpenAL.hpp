// CodeGear C++Builder
// Copyright (c) 1995, 2012 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLSMOpenAL.pas' rev: 24.00 (Win32)

#ifndef GlsmopenalHPP
#define GlsmopenalHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <System.Classes.hpp>	// Pascal unit
#include <GLSound.hpp>	// Pascal unit
#include <GLScene.hpp>	// Pascal unit
#include <System.SysUtils.hpp>	// Pascal unit
#include <GLSoundFileObjects.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Glsmopenal
{
//-- type declarations -------------------------------------------------------
class DELPHICLASS TGLSMOpenAL;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLSMOpenAL : public Glsound::TGLSoundManager
{
	typedef Glsound::TGLSoundManager inherited;
	
private:
	bool FActivated;
	
protected:
	DYNAMIC bool __fastcall DoActivate(void);
	DYNAMIC void __fastcall DoDeActivate(void);
	DYNAMIC void __fastcall NotifyMasterVolumeChange(void);
	DYNAMIC void __fastcall Notify3DFactorsChanged(void);
	DYNAMIC void __fastcall NotifyEnvironmentChanged(void);
	virtual void __fastcall KillSource(Glsound::TGLBaseSoundSource* aSource);
	virtual void __fastcall UpdateSource(Glsound::TGLBaseSoundSource* aSource);
	virtual void __fastcall MuteSource(Glsound::TGLBaseSoundSource* aSource, bool muted);
	virtual void __fastcall PauseSource(Glsound::TGLBaseSoundSource* aSource, bool paused);
	int __fastcall GetDefaultFrequency(Glsound::TGLBaseSoundSource* aSource);
	int __fastcall GetALFormat(Glsoundfileobjects::TGLSoundSampling* sampling);
	
public:
	__fastcall virtual TGLSMOpenAL(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TGLSMOpenAL(void);
	virtual void __fastcall UpdateSources(void);
	DYNAMIC bool __fastcall EAXSupported(void);
};

#pragma pack(pop)

typedef System::Sysutils::Exception EOpenALError;

//-- var, const, procedure ---------------------------------------------------
extern PACKAGE void __fastcall Register(void);
}	/* namespace Glsmopenal */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLSMOPENAL)
using namespace Glsmopenal;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// GlsmopenalHPP
