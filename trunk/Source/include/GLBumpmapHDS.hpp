// CodeGear C++Builder
// Copyright (c) 1995, 2012 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLBumpmapHDS.pas' rev: 24.00 (Win32)

#ifndef GlbumpmaphdsHPP
#define GlbumpmaphdsHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <System.Classes.hpp>	// Pascal unit
#include <GLHeightData.hpp>	// Pascal unit
#include <GLGraphics.hpp>	// Pascal unit
#include <VectorGeometry.hpp>	// Pascal unit
#include <GLTexture.hpp>	// Pascal unit
#include <System.SyncObjs.hpp>	// Pascal unit
#include <GLMaterial.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Glbumpmaphds
{
//-- type declarations -------------------------------------------------------
class DELPHICLASS TGLBumpmapHDS;
typedef void __fastcall (__closure *TNewTilePreparedEvent)(TGLBumpmapHDS* Sender, Glheightdata::THeightData* heightData, Glmaterial::TGLLibMaterial* normalMapMaterial);

class PASCALIMPLEMENTATION TGLBumpmapHDS : public Glheightdata::THeightDataSourceFilter
{
	typedef Glheightdata::THeightDataSourceFilter inherited;
	
private:
	Glmaterial::TGLMaterialLibrary* FBumpmapLibrary;
	TNewTilePreparedEvent FOnNewTilePrepared;
	float FBumpScale;
	int FSubSampling;
	int FMaxTextures;
	System::Syncobjs::TCriticalSection* Uno;
	
protected:
	void __fastcall SetBumpmapLibrary(Glmaterial::TGLMaterialLibrary* const val);
	void __fastcall SetBumpScale(const float val);
	bool __fastcall StoreBumpScale(void);
	void __fastcall SetSubSampling(const int val);
	void __fastcall Trim(int MaxTextureCount);
	
public:
	__fastcall virtual TGLBumpmapHDS(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TGLBumpmapHDS(void);
	virtual void __fastcall Release(Glheightdata::THeightData* aHeightData);
	virtual void __fastcall Notification(System::Classes::TComponent* AComponent, System::Classes::TOperation Operation);
	void __fastcall GenerateNormalMap(Glheightdata::THeightData* heightData, Glgraphics::TGLImage* normalMap, float scale);
	void __fastcall TrimTextureCache(int MaxTextureCount);
	virtual void __fastcall PreparingData(Glheightdata::THeightData* heightData);
	
__published:
	__property Glmaterial::TGLMaterialLibrary* BumpmapLibrary = {read=FBumpmapLibrary, write=SetBumpmapLibrary};
	__property TNewTilePreparedEvent OnNewTilePrepared = {read=FOnNewTilePrepared, write=FOnNewTilePrepared};
	__property float BumpScale = {read=FBumpScale, write=SetBumpScale, stored=StoreBumpScale};
	__property int SubSampling = {read=FSubSampling, write=SetSubSampling, default=1};
	__property MaxPoolSize;
	__property int MaxTextures = {read=FMaxTextures, write=FMaxTextures, nodefault};
	__property OnSourceDataFetched;
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Glbumpmaphds */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLBUMPMAPHDS)
using namespace Glbumpmaphds;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// GlbumpmaphdsHPP
