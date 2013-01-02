// CodeGear C++Builder
// Copyright (c) 1995, 2012 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLSMBASS.pas' rev: 24.00 (Win32)

#ifndef GlsmbassHPP
#define GlsmbassHPP

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

//-- user supplied -----------------------------------------------------------

namespace Glsmbass
{
//-- type declarations -------------------------------------------------------
enum TBASS3DAlgorithm : unsigned char { algDefault, algOff, algFull, algLight };

class DELPHICLASS TGLSMBASS;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLSMBASS : public Glsound::TGLSoundManager
{
	typedef Glsound::TGLSoundManager inherited;
	
private:
	bool FActivated;
	TBASS3DAlgorithm FAlgorithm3D;
	
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
	
public:
	__fastcall virtual TGLSMBASS(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TGLSMBASS(void);
	virtual void __fastcall UpdateSources(void);
	virtual float __fastcall CPUUsagePercent(void);
	DYNAMIC bool __fastcall EAXSupported(void);
	
__published:
	__property TBASS3DAlgorithm Algorithm3D = {read=FAlgorithm3D, write=FAlgorithm3D, default=0};
};

#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------
extern PACKAGE void __fastcall Register(void);
}	/* namespace Glsmbass */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLSMBASS)
using namespace Glsmbass;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// GlsmbassHPP
