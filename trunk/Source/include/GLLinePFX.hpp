// CodeGear C++Builder
// Copyright (c) 1995, 2012 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLLinePFX.pas' rev: 24.00 (Win32)

#ifndef GllinepfxHPP
#define GllinepfxHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <System.Classes.hpp>	// Pascal unit
#include <PersistentClasses.hpp>	// Pascal unit
#include <VectorGeometry.hpp>	// Pascal unit
#include <GLParticleFX.hpp>	// Pascal unit
#include <GLTexture.hpp>	// Pascal unit
#include <GLColor.hpp>	// Pascal unit
#include <GLRenderContextInfo.hpp>	// Pascal unit
#include <VectorTypes.hpp>	// Pascal unit
#include <GLCadencer.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Gllinepfx
{
//-- type declarations -------------------------------------------------------
class DELPHICLASS TGLLineParticle;
class PASCALIMPLEMENTATION TGLLineParticle : public Glparticlefx::TGLParticle
{
	typedef Glparticlefx::TGLParticle inherited;
	
private:
	Vectortypes::TVector3f FDirection;
	float FLength;
	
public:
	DYNAMIC void __fastcall WriteToFiler(Persistentclasses::TVirtualWriter* writer);
	DYNAMIC void __fastcall ReadFromFiler(Persistentclasses::TVirtualReader* reader);
	__property Vectortypes::TVector3f Direction = {read=FDirection, write=FDirection};
	__property float Length = {read=FLength, write=FLength};
public:
	/* TGLParticle.Create */ inline __fastcall virtual TGLLineParticle(void) : Glparticlefx::TGLParticle() { }
	/* TGLParticle.Destroy */ inline __fastcall virtual ~TGLLineParticle(void) { }
	
public:
	/* TPersistentObject.CreateFromFiler */ inline __fastcall TGLLineParticle(Persistentclasses::TVirtualReader* reader) : Glparticlefx::TGLParticle(reader) { }
	
};


class DELPHICLASS TGLLinePFXManager;
class PASCALIMPLEMENTATION TGLLinePFXManager : public Glparticlefx::TGLLifeColoredPFXManager
{
	typedef Glparticlefx::TGLLifeColoredPFXManager inherited;
	
private:
	Vectortypes::TVector3f Fvx;
	Vectortypes::TVector3f Fvy;
	Vectortypes::TVector3f FNvx;
	Vectortypes::TVector3f FNvy;
	float FDefaultLength;
	
protected:
	bool __fastcall StoreDefaultLength(void);
	virtual unsigned __fastcall TexturingMode(void);
	DYNAMIC void __fastcall InitializeRendering(Glrendercontextinfo::TRenderContextInfo &rci);
	virtual void __fastcall BeginParticles(Glrendercontextinfo::TRenderContextInfo &rci);
	virtual void __fastcall RenderParticle(Glrendercontextinfo::TRenderContextInfo &rci, Glparticlefx::TGLParticle* aParticle);
	virtual void __fastcall EndParticles(Glrendercontextinfo::TRenderContextInfo &rci);
	DYNAMIC void __fastcall FinalizeRendering(Glrendercontextinfo::TRenderContextInfo &rci);
	
public:
	__fastcall virtual TGLLinePFXManager(System::Classes::TComponent* aOwner);
	__fastcall virtual ~TGLLinePFXManager(void);
	__classmethod virtual Glparticlefx::TGLParticleClass __fastcall ParticlesClass();
	virtual Glparticlefx::TGLParticle* __fastcall CreateParticle(void);
	
__published:
	__property float DefaultLength = {read=FDefaultLength, write=FDefaultLength, stored=StoreDefaultLength};
	__property ParticleSize = {default=0};
	__property ColorInner;
	__property ColorOuter;
	__property LifeColors;
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Gllinepfx */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLLINEPFX)
using namespace Gllinepfx;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// GllinepfxHPP
