// CodeGear C++Builder
// Copyright (c) 1995, 2012 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLFireFX.pas' rev: 24.00 (Win32)

#ifndef GlfirefxHPP
#define GlfirefxHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <System.Classes.hpp>	// Pascal unit
#include <GLScene.hpp>	// Pascal unit
#include <XCollection.hpp>	// Pascal unit
#include <VectorGeometry.hpp>	// Pascal unit
#include <GLCadencer.hpp>	// Pascal unit
#include <GLColor.hpp>	// Pascal unit
#include <BaseClasses.hpp>	// Pascal unit
#include <GLCoordinates.hpp>	// Pascal unit
#include <GLManager.hpp>	// Pascal unit
#include <GLRenderContextInfo.hpp>	// Pascal unit
#include <GLState.hpp>	// Pascal unit
#include <GLTextureFormat.hpp>	// Pascal unit
#include <VectorTypes.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Glfirefx
{
//-- type declarations -------------------------------------------------------
struct TFireParticle;
typedef TFireParticle *PFireParticle;

struct DECLSPEC_DRECORD TFireParticle
{
public:
	Vectortypes::TVector4f Position;
	Vectortypes::TVector4f Speed;
	float Alpha;
	float TimeToLive;
	float LifeLength;
};


typedef System::StaticArray<TFireParticle, 33554432> TFireParticleArray;

typedef TFireParticleArray *PFireParticleArray;

class DELPHICLASS TGLFireFXManager;
class DELPHICLASS TGLBFireFX;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLFireFXManager : public Baseclasses::TGLCadenceAbleComponent
{
	typedef Baseclasses::TGLCadenceAbleComponent inherited;
	
private:
	System::Classes::TList* FClients;
	TFireParticleArray *FFireParticles;
	Glcoordinates::TGLCoordinates3* FFireDir;
	Glcoordinates::TGLCoordinates3* FInitialDir;
	Glcadencer::TGLCadencer* FCadencer;
	int FMaxParticles;
	int FParticleLife;
	float FParticleSize;
	float FFireDensity;
	float FFireEvaporation;
	float FFireCrown;
	float FParticleInterval;
	float IntervalDelta;
	int NP;
	Glcolor::TGLColor* FInnerColor;
	Glcolor::TGLColor* FOuterColor;
	float FFireBurst;
	float FFireRadius;
	bool FDisabled;
	bool FPaused;
	bool FUseInterval;
	Glscene::TGLBaseSceneObject* FReference;
	bool FNoZWrite;
	
protected:
	void __fastcall RegisterClient(TGLBFireFX* aClient);
	void __fastcall DeRegisterClient(TGLBFireFX* aClient);
	void __fastcall DeRegisterAllClients(void);
	void __fastcall SetFireDir(Glcoordinates::TGLCoordinates3* const val);
	void __fastcall SetInitialDir(Glcoordinates::TGLCoordinates3* const val);
	void __fastcall SetCadencer(Glcadencer::TGLCadencer* const val);
	bool __fastcall StoreParticleSize(void);
	void __fastcall SetInnerColor(Glcolor::TGLColor* const val);
	void __fastcall SetOuterColor(Glcolor::TGLColor* const val);
	HIDESBASE void __fastcall SetReference(Glscene::TGLBaseSceneObject* const val);
	void __fastcall SetMaxParticles(const int val);
	virtual void __fastcall Notification(System::Classes::TComponent* AComponent, System::Classes::TOperation Operation);
	void __fastcall CalcFire(double deltaTime, float ParticleInterval, float ParticleLife, float FireAlpha);
	void __fastcall AffParticle3d(const Vectortypes::TVector4f &Color2, const Vectortypes::TMatrix4f &mat);
	
public:
	__fastcall virtual TGLFireFXManager(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TGLFireFXManager(void);
	void __fastcall FireInit(void);
	void __fastcall IsotropicExplosion(float minInitialSpeed, float maxInitialSpeed, float lifeBoostFactor, int nbParticles = 0xffffffff);
	void __fastcall RingExplosion(float minInitialSpeed, float maxInitialSpeed, float lifeBoostFactor, const Vectortypes::TVector3f &ringVectorX, const Vectortypes::TVector3f &ringVectorY, int nbParticles = 0xffffffff);
	__property int ParticleCount = {read=NP, nodefault};
	virtual void __fastcall DoProgress(const Baseclasses::TProgressTimes &progressTime);
	
__published:
	__property Glcoordinates::TGLCoordinates3* FireDir = {read=FFireDir, write=SetFireDir};
	__property Glcoordinates::TGLCoordinates3* InitialDir = {read=FInitialDir, write=SetInitialDir};
	__property Glcadencer::TGLCadencer* Cadencer = {read=FCadencer, write=SetCadencer};
	__property int MaxParticles = {read=FMaxParticles, write=SetMaxParticles, default=256};
	__property float ParticleSize = {read=FParticleSize, write=FParticleSize, stored=StoreParticleSize};
	__property Glcolor::TGLColor* InnerColor = {read=FInnerColor, write=SetInnerColor};
	__property Glcolor::TGLColor* OuterColor = {read=FOuterColor, write=SetOuterColor};
	__property float FireDensity = {read=FFireDensity, write=FFireDensity};
	__property float FireEvaporation = {read=FFireEvaporation, write=FFireEvaporation};
	__property float FireCrown = {read=FFireCrown, write=FFireCrown};
	__property int ParticleLife = {read=FParticleLife, write=FParticleLife, default=3};
	__property float FireBurst = {read=FFireBurst, write=FFireBurst};
	__property float FireRadius = {read=FFireRadius, write=FFireRadius};
	__property bool Disabled = {read=FDisabled, write=FDisabled, nodefault};
	__property bool Paused = {read=FPaused, write=FPaused, nodefault};
	__property float ParticleInterval = {read=FParticleInterval, write=FParticleInterval};
	__property bool UseInterval = {read=FUseInterval, write=FUseInterval, nodefault};
	__property bool NoZWrite = {read=FNoZWrite, write=FNoZWrite, default=1};
	__property Glscene::TGLBaseSceneObject* Reference = {read=FReference, write=SetReference};
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLBFireFX : public Glscene::TGLObjectPostEffect
{
	typedef Glscene::TGLObjectPostEffect inherited;
	
private:
	TGLFireFXManager* FManager;
	System::UnicodeString FManagerName;
	
protected:
	void __fastcall SetManager(TGLFireFXManager* const val);
	virtual void __fastcall WriteToFiler(System::Classes::TWriter* writer);
	virtual void __fastcall ReadFromFiler(System::Classes::TReader* reader);
	DYNAMIC void __fastcall Loaded(void);
	
public:
	__fastcall virtual TGLBFireFX(Xcollection::TXCollection* aOwner);
	__fastcall virtual ~TGLBFireFX(void);
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	__classmethod virtual System::UnicodeString __fastcall FriendlyName();
	__classmethod virtual System::UnicodeString __fastcall FriendlyDescription();
	virtual void __fastcall Render(Glrendercontextinfo::TRenderContextInfo &rci);
	
__published:
	__property TGLFireFXManager* Manager = {read=FManager, write=SetManager};
};

#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------
extern PACKAGE TGLBFireFX* __fastcall GetOrCreateFireFX(Glscene::TGLObjectEffects* effects)/* overload */;
extern PACKAGE TGLBFireFX* __fastcall GetOrCreateFireFX(Glscene::TGLBaseSceneObject* obj)/* overload */;
}	/* namespace Glfirefx */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLFIREFX)
using namespace Glfirefx;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// GlfirefxHPP
