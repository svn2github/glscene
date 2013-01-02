// CodeGear C++Builder
// Copyright (c) 1995, 2012 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLParticles.pas' rev: 24.00 (Win32)

#ifndef GlparticlesHPP
#define GlparticlesHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <System.Classes.hpp>	// Pascal unit
#include <GLScene.hpp>	// Pascal unit
#include <VectorGeometry.hpp>	// Pascal unit
#include <OpenGLTokens.hpp>	// Pascal unit
#include <GLContext.hpp>	// Pascal unit
#include <GLColor.hpp>	// Pascal unit
#include <BaseClasses.hpp>	// Pascal unit
#include <GLRenderContextInfo.hpp>	// Pascal unit
#include <GLState.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Glparticles
{
//-- type declarations -------------------------------------------------------
typedef void __fastcall (__closure *TGLParticleEvent)(System::TObject* Sender, Glscene::TGLBaseSceneObject* particle);

class DELPHICLASS TGLParticles;
class PASCALIMPLEMENTATION TGLParticles : public Glscene::TGLImmaterialSceneObject
{
	typedef Glscene::TGLImmaterialSceneObject inherited;
	
private:
	float FCubeSize;
	Glcolor::TGLColor* FEdgeColor;
	bool FVisibleAtRunTime;
	System::Classes::TList* particlePool;
	int FParticlePoolSize;
	TGLParticleEvent FOnCreateParticle;
	TGLParticleEvent FOnActivateParticle;
	TGLParticleEvent FOnKillParticle;
	TGLParticleEvent FOnDestroyParticle;
	Glscene::TDirectRenderEvent FOnBeforeRenderParticles;
	Glscene::TDirectRenderEvent FOnAfterRenderParticles;
	
protected:
	void __fastcall SetCubeSize(const float val);
	void __fastcall SetEdgeColor(Glcolor::TGLColor* const val);
	void __fastcall SetVisibleAtRunTime(const bool val);
	void __fastcall SetParticlePoolSize(int val);
	void __fastcall ClearParticlePool(void);
	
public:
	__fastcall virtual TGLParticles(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TGLParticles(void);
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	virtual void __fastcall BuildList(Glrendercontextinfo::TRenderContextInfo &ARci);
	virtual void __fastcall DoRender(Glrendercontextinfo::TRenderContextInfo &ARci, bool ARenderSelf, bool ARenderChildren);
	virtual void __fastcall DoProgress(const Baseclasses::TProgressTimes &progressTime);
	Glscene::TGLBaseSceneObject* __fastcall CreateParticle(void);
	void __fastcall KillParticle(Glscene::TGLBaseSceneObject* aParticle);
	void __fastcall KillParticles(void);
	
__published:
	__property float CubeSize = {read=FCubeSize, write=SetCubeSize};
	__property Glcolor::TGLColor* EdgeColor = {read=FEdgeColor, write=SetEdgeColor};
	__property bool VisibleAtRunTime = {read=FVisibleAtRunTime, write=SetVisibleAtRunTime, default=0};
	__property int ParticlePoolSize = {read=FParticlePoolSize, write=SetParticlePoolSize, default=0};
	__property TGLParticleEvent OnCreateParticle = {read=FOnCreateParticle, write=FOnCreateParticle};
	__property TGLParticleEvent OnActivateParticle = {read=FOnActivateParticle, write=FOnActivateParticle};
	__property TGLParticleEvent OnKillParticle = {read=FOnKillParticle, write=FOnKillParticle};
	__property TGLParticleEvent OnDestroyParticle = {read=FOnDestroyParticle, write=FOnDestroyParticle};
	__property Glscene::TDirectRenderEvent OnBeforeRenderParticles = {read=FOnBeforeRenderParticles, write=FOnBeforeRenderParticles};
	__property Glscene::TDirectRenderEvent OnAfterRenderParticles = {read=FOnAfterRenderParticles, write=FOnAfterRenderParticles};
public:
	/* TGLBaseSceneObject.CreateAsChild */ inline __fastcall TGLParticles(Glscene::TGLBaseSceneObject* aParentOwner) : Glscene::TGLImmaterialSceneObject(aParentOwner) { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Glparticles */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLPARTICLES)
using namespace Glparticles;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// GlparticlesHPP
