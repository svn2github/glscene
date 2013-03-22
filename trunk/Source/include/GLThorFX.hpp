// CodeGear C++Builder
// Copyright (c) 1995, 2012 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLThorFX.pas' rev: 24.00 (Win32)

#ifndef GlthorfxHPP
#define GlthorfxHPP

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
#include <GLRenderContextInfo.hpp>	// Pascal unit
#include <GLManager.hpp>	// Pascal unit
#include <GLState.hpp>	// Pascal unit
#include <GLTextureFormat.hpp>	// Pascal unit
#include <VectorTypes.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Glthorfx
{
//-- type declarations -------------------------------------------------------
struct TThorpoint;
typedef TThorpoint *PThorpoint;

struct DECLSPEC_DRECORD TThorpoint
{
public:
	Vectortypes::TVector4f Position;
	float Size;
};


typedef System::StaticArray<TThorpoint, 33554432> TThorpointArray;

typedef TThorpointArray *PThorpointArray;

typedef void __fastcall (__closure *TCalcPointEvent)(System::TObject* Sender, int PointNo, float &x, float &y, float &z);

class DELPHICLASS TGLThorFXManager;
class DELPHICLASS TGLBThorFX;
class PASCALIMPLEMENTATION TGLThorFXManager : public Baseclasses::TGLCadenceAbleComponent
{
	typedef Baseclasses::TGLCadenceAbleComponent inherited;
	
private:
	System::Classes::TList* FClients;
	TThorpointArray *FThorpoints;
	Glcoordinates::TGLCoordinates3* FTarget;
	Glcadencer::TGLCadencer* FCadencer;
	int FMaxpoints;
	float FGlowSize;
	float FVibrate;
	float FWildness;
	int NP;
	Glcolor::TGLColor* FInnerColor;
	Glcolor::TGLColor* FOuterColor;
	Glcolor::TGLColor* FCoreColor;
	bool FDisabled;
	bool FCore;
	bool FGlow;
	TCalcPointEvent FOnCalcPoint;
	
protected:
	void __fastcall RegisterClient(TGLBThorFX* aClient);
	void __fastcall DeRegisterClient(TGLBThorFX* aClient);
	void __fastcall DeRegisterAllClients(void);
	void __fastcall SetTarget(Glcoordinates::TGLCoordinates3* const val);
	void __fastcall SetCadencer(Glcadencer::TGLCadencer* const val);
	void __fastcall SetMaxpoints(const int val);
	bool __fastcall StoreGlowSize(void);
	bool __fastcall StoreVibrate(void);
	void __fastcall SetInnerColor(Glcolor::TGLColor* const val);
	void __fastcall SetOuterColor(Glcolor::TGLColor* const val);
	void __fastcall SetCoreColor(Glcolor::TGLColor* const val);
	virtual void __fastcall Notification(System::Classes::TComponent* AComponent, System::Classes::TOperation Operation);
	void __fastcall ThorInit(void);
	void __fastcall CalcThor(void);
	void __fastcall CalcFrac(int left, int right, float lh, float rh, int xyz);
	
public:
	__fastcall virtual TGLThorFXManager(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TGLThorFXManager(void);
	virtual void __fastcall DoProgress(const Baseclasses::TProgressTimes &progressTime);
	
__published:
	__property Glcoordinates::TGLCoordinates3* Target = {read=FTarget, write=SetTarget};
	__property Glcadencer::TGLCadencer* Cadencer = {read=FCadencer, write=SetCadencer};
	__property int Maxpoints = {read=FMaxpoints, write=SetMaxpoints, default=256};
	__property float GlowSize = {read=FGlowSize, write=FGlowSize, stored=StoreGlowSize};
	__property float Vibrate = {read=FVibrate, write=FVibrate, stored=StoreVibrate};
	__property Glcolor::TGLColor* InnerColor = {read=FInnerColor, write=SetInnerColor};
	__property Glcolor::TGLColor* OuterColor = {read=FOuterColor, write=SetOuterColor};
	__property Glcolor::TGLColor* CoreColor = {read=FCoreColor, write=SetCoreColor};
	__property bool Disabled = {read=FDisabled, write=FDisabled, nodefault};
	__property bool Core = {read=FCore, write=FCore, nodefault};
	__property bool Glow = {read=FGlow, write=FGlow, nodefault};
	__property float Wildness = {read=FWildness, write=FWildness};
	__property TCalcPointEvent OnCalcPoint = {read=FOnCalcPoint, write=FOnCalcPoint};
};


#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLBThorFX : public Glscene::TGLObjectPostEffect
{
	typedef Glscene::TGLObjectPostEffect inherited;
	
private:
	TGLThorFXManager* FManager;
	System::UnicodeString FManagerName;
	Glcoordinates::TGLCoordinates3* FTarget;
	
protected:
	void __fastcall SetManager(TGLThorFXManager* const val);
	virtual void __fastcall WriteToFiler(System::Classes::TWriter* writer);
	virtual void __fastcall ReadFromFiler(System::Classes::TReader* reader);
	DYNAMIC void __fastcall Loaded(void);
	void __fastcall SetTarget(Glcoordinates::TGLCoordinates3* const val);
	
public:
	__fastcall virtual TGLBThorFX(Xcollection::TXCollection* AOwner);
	__fastcall virtual ~TGLBThorFX(void);
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	__classmethod virtual System::UnicodeString __fastcall FriendlyName();
	__classmethod virtual System::UnicodeString __fastcall FriendlyDescription();
	virtual void __fastcall Render(Glrendercontextinfo::TRenderContextInfo &rci);
	
__published:
	__property TGLThorFXManager* Manager = {read=FManager, write=SetManager};
};

#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------
extern PACKAGE TGLBThorFX* __fastcall GetOrCreateThorFX(Glscene::TGLBaseSceneObject* obj, const System::UnicodeString name = System::UnicodeString());
}	/* namespace Glthorfx */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLTHORFX)
using namespace Glthorfx;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// GlthorfxHPP
