// CodeGear C++Builder
// Copyright (c) 1995, 2012 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLTexLensFlare.pas' rev: 24.00 (Win32)

#ifndef GltexlensflareHPP
#define GltexlensflareHPP

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
#include <GLObjects.hpp>	// Pascal unit
#include <GLTexture.hpp>	// Pascal unit
#include <OpenGLTokens.hpp>	// Pascal unit
#include <GLContext.hpp>	// Pascal unit
#include <GLRenderContextInfo.hpp>	// Pascal unit
#include <BaseClasses.hpp>	// Pascal unit
#include <GLState.hpp>	// Pascal unit
#include <GLCoordinates.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Gltexlensflare
{
//-- type declarations -------------------------------------------------------
class DELPHICLASS TGLTextureLensFlare;
class PASCALIMPLEMENTATION TGLTextureLensFlare : public Glscene::TGLBaseSceneObject
{
	typedef Glscene::TGLBaseSceneObject inherited;
	
private:
	int FSize;
	float FCurrSize;
	int FNumSecs;
	bool FAutoZTest;
	double FDeltaTime;
	Gltexture::TGLTexture* FImgSecondaries;
	Gltexture::TGLTexture* FImgRays;
	Gltexture::TGLTexture* FImgRing;
	Gltexture::TGLTexture* FImgGlow;
	int FSeed;
	void __fastcall SetImgGlow(Gltexture::TGLTexture* const Value);
	void __fastcall SetImgRays(Gltexture::TGLTexture* const Value);
	void __fastcall SetImgRing(Gltexture::TGLTexture* const Value);
	void __fastcall SetImgSecondaries(Gltexture::TGLTexture* const Value);
	void __fastcall SetSeed(const int Value);
	
protected:
	void __fastcall SetSize(int aValue);
	void __fastcall SetNumSecs(int aValue);
	void __fastcall SetAutoZTest(bool aValue);
	
public:
	__fastcall virtual TGLTextureLensFlare(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TGLTextureLensFlare(void);
	virtual void __fastcall BuildList(Glrendercontextinfo::TRenderContextInfo &rci);
	virtual void __fastcall DoProgress(const Baseclasses::TProgressTimes &progressTime);
	
__published:
	__property int Size = {read=FSize, write=SetSize, default=50};
	__property int Seed = {read=FSeed, write=SetSeed, nodefault};
	__property int NumSecs = {read=FNumSecs, write=SetNumSecs, default=8};
	__property bool AutoZTest = {read=FAutoZTest, write=SetAutoZTest, default=1};
	__property Gltexture::TGLTexture* ImgGlow = {read=FImgGlow, write=SetImgGlow};
	__property Gltexture::TGLTexture* ImgRays = {read=FImgRays, write=SetImgRays};
	__property Gltexture::TGLTexture* ImgRing = {read=FImgRing, write=SetImgRing};
	__property Gltexture::TGLTexture* ImgSecondaries = {read=FImgSecondaries, write=SetImgSecondaries};
	__property ObjectsSorting = {default=0};
	__property Position;
	__property Visible = {default=1};
	__property OnProgress;
	__property Behaviours;
	__property Effects;
public:
	/* TGLBaseSceneObject.CreateAsChild */ inline __fastcall TGLTextureLensFlare(Glscene::TGLBaseSceneObject* aParentOwner) : Glscene::TGLBaseSceneObject(aParentOwner) { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Gltexlensflare */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLTEXLENSFLARE)
using namespace Gltexlensflare;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// GltexlensflareHPP
