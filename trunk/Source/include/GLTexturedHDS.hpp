// CodeGear C++Builder
// Copyright (c) 1995, 2012 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLTexturedHDS.pas' rev: 24.00 (Win32)

#ifndef GltexturedhdsHPP
#define GltexturedhdsHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <System.Classes.hpp>	// Pascal unit
#include <GLCrossPlatform.hpp>	// Pascal unit
#include <GLHeightData.hpp>	// Pascal unit
#include <GLMaterial.hpp>	// Pascal unit
#include <System.Types.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Gltexturedhds
{
//-- type declarations -------------------------------------------------------
class DELPHICLASS TGLTexturedHDS;
class PASCALIMPLEMENTATION TGLTexturedHDS : public Glheightdata::THeightDataSource
{
	typedef Glheightdata::THeightDataSource inherited;
	
private:
	Glheightdata::TStartPreparingDataEvent FOnStartPreparingData;
	Glheightdata::TMarkDirtyEvent FOnMarkDirty;
	Glheightdata::THeightDataSource* FHeightDataSource;
	Glmaterial::TGLMaterialLibrary* FMaterialLibrary;
	bool FWholeTilesOnly;
	int FTileSize;
	int FTilesPerTexture;
	
protected:
	void __fastcall SetHeightDataSource(Glheightdata::THeightDataSource* val);
	
public:
	__fastcall virtual TGLTexturedHDS(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TGLTexturedHDS(void);
	virtual void __fastcall StartPreparingData(Glheightdata::THeightData* heightData);
	virtual void __fastcall MarkDirty(const System::Types::TRect &area)/* overload */;
	
__published:
	__property MaxPoolSize;
	__property Glheightdata::TStartPreparingDataEvent OnStartPreparingData = {read=FOnStartPreparingData, write=FOnStartPreparingData};
	__property Glheightdata::TMarkDirtyEvent OnMarkDirtyEvent = {read=FOnMarkDirty, write=FOnMarkDirty};
	__property Glheightdata::THeightDataSource* HeightDataSource = {read=FHeightDataSource, write=SetHeightDataSource};
	__property Glmaterial::TGLMaterialLibrary* MaterialLibrary = {read=FMaterialLibrary, write=FMaterialLibrary};
	__property bool WholeTilesOnly = {read=FWholeTilesOnly, write=FWholeTilesOnly, nodefault};
	__property int TileSize = {read=FTileSize, write=FTileSize, nodefault};
	__property int TilesPerTexture = {read=FTilesPerTexture, write=FTilesPerTexture, nodefault};
/* Hoisted overloads: */
	
public:
	inline void __fastcall  MarkDirty(int XLeft, int YTop, int xRight, int yBottom){ Glheightdata::THeightDataSource::MarkDirty(XLeft, YTop, xRight, yBottom); }
	inline void __fastcall  MarkDirty(void){ Glheightdata::THeightDataSource::MarkDirty(); }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Gltexturedhds */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLTEXTUREDHDS)
using namespace Gltexturedhds;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// GltexturedhdsHPP
