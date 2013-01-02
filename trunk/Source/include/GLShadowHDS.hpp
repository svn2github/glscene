// CodeGear C++Builder
// Copyright (c) 1995, 2012 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLShadowHDS.pas' rev: 24.00 (Win32)

#ifndef GlshadowhdsHPP
#define GlshadowhdsHPP

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
#include <VectorTypes.hpp>	// Pascal unit
#include <GLCoordinates.hpp>	// Pascal unit
#include <GLMaterial.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Glshadowhds
{
//-- type declarations -------------------------------------------------------
class DELPHICLASS TGLShadowHDS;
typedef void __fastcall (__closure *TNewTilePreparedEvent)(TGLShadowHDS* Sender, Glheightdata::THeightData* heightData, Glmaterial::TGLLibMaterial* ShadowMapMaterial);

typedef void __fastcall (__closure *TThreadBmp32)(TGLShadowHDS* Sender, Glheightdata::THeightData* heightData, Glgraphics::TGLImage* bmp32);

class PASCALIMPLEMENTATION TGLShadowHDS : public Glheightdata::THeightDataSourceFilter
{
	typedef Glheightdata::THeightDataSourceFilter inherited;
	
private:
	int FTileSize;
	Glmaterial::TGLMaterialLibrary* FShadowmapLibrary;
	Glcoordinates::TGLCoordinates3* FLightVector;
	Glcoordinates::TGLCoordinates3* FScale;
	Vectortypes::TVector3f FScaleVec;
	TNewTilePreparedEvent FOnNewTilePrepared;
	TThreadBmp32 FOnThreadBmp32;
	int FMaxTextures;
	Vectortypes::TVector3f Step;
	int FScanDistance;
	unsigned FSoftRange;
	float FDiffuse;
	float FAmbient;
	Glheightdata::THeightDataSource* OwnerHDS;
	
protected:
	void __fastcall SetShadowmapLibrary(Glmaterial::TGLMaterialLibrary* const val);
	void __fastcall SetScale(Glcoordinates::TGLCoordinates3* AValue);
	void __fastcall SetLightVector(Glcoordinates::TGLCoordinates3* AValue);
	void __fastcall SetSoftRange(unsigned AValue);
	void __fastcall SetDiffuse(float AValue);
	void __fastcall SetAmbient(float AValue);
	void __fastcall Trim(int MaxTextureCount);
	Glmaterial::TGLLibMaterial* __fastcall FindUnusedMaterial(void);
	Vectortypes::TVector3f __fastcall CalcStep(void);
	Vectortypes::TVector3f __fastcall CalcScale(void);
	int __fastcall WrapDist(float Lx, float Ly);
	void __fastcall LocalToWorld(float Lx, float Ly, Glheightdata::THeightData* HD, float &Wx, float &Wy);
	void __fastcall WorldToLocal(float wx, float wy, Glheightdata::THeightData* &HD, float &lx, float &ly);
	
public:
	bool SkipGenerate;
	__fastcall virtual TGLShadowHDS(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TGLShadowHDS(void);
	void __fastcall TrimTextureCache(int MaxTextureCount = 0x0);
	virtual void __fastcall Notification(System::Classes::TComponent* AComponent, System::Classes::TOperation Operation);
	virtual void __fastcall BeforePreparingData(Glheightdata::THeightData* heightData);
	virtual void __fastcall PreparingData(Glheightdata::THeightData* heightData);
	virtual void __fastcall AfterPreparingData(Glheightdata::THeightData* heightData);
	void __fastcall GenerateShadowMap(Glheightdata::THeightData* heightData, Glgraphics::TGLImage* ShadowMap, float scale);
	float __fastcall RayCastShadowHeight(Glheightdata::THeightData* HD, float localX, float localY)/* overload */;
	void __fastcall RayCastLine(Glheightdata::THeightData* HeightData, float Lx, float Ly, Glgraphics::TGLImage* ShadowMap);
	System::Byte __fastcall Shade(Glheightdata::THeightData* HeightData, int x, int y, float ShadowHeight, float TerrainHeight);
	
__published:
	__property Glmaterial::TGLMaterialLibrary* ShadowmapLibrary = {read=FShadowmapLibrary, write=SetShadowmapLibrary};
	__property TThreadBmp32 OnThreadBmp32 = {read=FOnThreadBmp32, write=FOnThreadBmp32};
	__property TNewTilePreparedEvent OnNewTilePrepared = {read=FOnNewTilePrepared, write=FOnNewTilePrepared};
	__property Glcoordinates::TGLCoordinates3* LightVector = {read=FLightVector, write=SetLightVector};
	__property Glcoordinates::TGLCoordinates3* Scale = {read=FScale, write=FScale};
	__property int ScanDistance = {read=FScanDistance, write=FScanDistance, nodefault};
	__property unsigned SoftRange = {read=FSoftRange, write=SetSoftRange, nodefault};
	__property float Diffuse = {read=FDiffuse, write=SetDiffuse};
	__property float Ambient = {read=FAmbient, write=SetAmbient};
	__property int MaxTextures = {read=FMaxTextures, write=FMaxTextures, nodefault};
	__property OnSourceDataFetched;
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Glshadowhds */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLSHADOWHDS)
using namespace Glshadowhds;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// GlshadowhdsHPP
