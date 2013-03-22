// CodeGear C++Builder
// Copyright (c) 1995, 2012 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLTilePlane.pas' rev: 24.00 (Win32)

#ifndef GltileplaneHPP
#define GltileplaneHPP

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
#include <GLMaterial.hpp>	// Pascal unit
#include <GLObjects.hpp>	// Pascal unit
#include <GLCrossPlatform.hpp>	// Pascal unit
#include <PersistentClasses.hpp>	// Pascal unit
#include <VectorLists.hpp>	// Pascal unit
#include <GLRenderContextInfo.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Gltileplane
{
//-- type declarations -------------------------------------------------------
class DELPHICLASS TGLTiledAreaRow;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLTiledAreaRow : public Persistentclasses::TPersistentObject
{
	typedef Persistentclasses::TPersistentObject inherited;
	
public:
	int operator[](int col) { return Cell[col]; }
	
private:
	int FColMin;
	int FColMax;
	Vectorlists::TIntegerList* FData;
	
protected:
	void __fastcall SetColMin(const int val);
	void __fastcall SetColMax(const int val);
	int __fastcall GetCell(int col);
	void __fastcall SetCell(int col, int val);
	
public:
	__fastcall virtual TGLTiledAreaRow(void);
	__fastcall virtual ~TGLTiledAreaRow(void);
	DYNAMIC void __fastcall WriteToFiler(Persistentclasses::TVirtualWriter* writer);
	DYNAMIC void __fastcall ReadFromFiler(Persistentclasses::TVirtualReader* reader);
	__property int Cell[int col] = {read=GetCell, write=SetCell/*, default*/};
	__property int ColMin = {read=FColMin, write=SetColMin, nodefault};
	__property int ColMax = {read=FColMax, write=SetColMax, nodefault};
	__property Vectorlists::TIntegerList* Data = {read=FData};
	void __fastcall Pack(void);
	bool __fastcall Empty(void);
	void __fastcall RemapTiles(Vectorlists::TIntegerList* remapList);
public:
	/* TPersistentObject.CreateFromFiler */ inline __fastcall TGLTiledAreaRow(Persistentclasses::TVirtualReader* reader) : Persistentclasses::TPersistentObject(reader) { }
	
};

#pragma pack(pop)

class DELPHICLASS TGLTiledArea;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLTiledArea : public Persistentclasses::TPersistentObject
{
	typedef Persistentclasses::TPersistentObject inherited;
	
private:
	int FRowMin;
	int FRowMax;
	Persistentclasses::TPersistentObjectList* FRows;
	
protected:
	void __fastcall SetRowMin(const int val);
	void __fastcall SetRowMax(const int val);
	int __fastcall GetTile(int col, int row);
	void __fastcall SetTile(int col, int row, int val);
	TGLTiledAreaRow* __fastcall GetRow(int index);
	
public:
	__fastcall virtual TGLTiledArea(void);
	__fastcall virtual ~TGLTiledArea(void);
	DYNAMIC void __fastcall WriteToFiler(Persistentclasses::TVirtualWriter* writer);
	DYNAMIC void __fastcall ReadFromFiler(Persistentclasses::TVirtualReader* reader);
	__property int Tile[int col][int row] = {read=GetTile, write=SetTile/*, default*/};
	__property TGLTiledAreaRow* Row[int index] = {read=GetRow};
	__property int RowMin = {read=FRowMin, write=SetRowMin, nodefault};
	__property int RowMax = {read=FRowMax, write=SetRowMax, nodefault};
	void __fastcall Pack(void);
	void __fastcall Clear(void);
	bool __fastcall Empty(void);
	void __fastcall RemapTiles(Vectorlists::TIntegerList* remapList);
public:
	/* TPersistentObject.CreateFromFiler */ inline __fastcall TGLTiledArea(Persistentclasses::TVirtualReader* reader) : Persistentclasses::TPersistentObject(reader) { }
	
};

#pragma pack(pop)

class DELPHICLASS TGLTilePlane;
class PASCALIMPLEMENTATION TGLTilePlane : public Glscene::TGLImmaterialSceneObject
{
	typedef Glscene::TGLImmaterialSceneObject inherited;
	
private:
	bool FNoZWrite;
	TGLTiledArea* FTiles;
	Glmaterial::TGLMaterialLibrary* FMaterialLibrary;
	bool FSortByMaterials;
	
protected:
	void __fastcall SetNoZWrite(const bool val);
	void __fastcall SetTiles(TGLTiledArea* const val);
	void __fastcall SetMaterialLibrary(Glmaterial::TGLMaterialLibrary* const val);
	void __fastcall SetSortByMaterials(const bool val);
	virtual void __fastcall Notification(System::Classes::TComponent* AComponent, System::Classes::TOperation Operation);
	
public:
	__fastcall virtual TGLTilePlane(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TGLTilePlane(void);
	virtual void __fastcall DoRender(Glrendercontextinfo::TRenderContextInfo &ARci, bool ARenderSelf, bool ARenderChildren);
	virtual void __fastcall BuildList(Glrendercontextinfo::TRenderContextInfo &rci);
	__property TGLTiledArea* Tiles = {read=FTiles, write=SetTiles};
	__property bool SortByMaterials = {read=FSortByMaterials, write=SetSortByMaterials, nodefault};
	
__published:
	__property bool NoZWrite = {read=FNoZWrite, write=SetNoZWrite, nodefault};
	__property Glmaterial::TGLMaterialLibrary* MaterialLibrary = {read=FMaterialLibrary, write=SetMaterialLibrary};
public:
	/* TGLBaseSceneObject.CreateAsChild */ inline __fastcall TGLTilePlane(Glscene::TGLBaseSceneObject* aParentOwner) : Glscene::TGLImmaterialSceneObject(aParentOwner) { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Gltileplane */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLTILEPLANE)
using namespace Gltileplane;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// GltileplaneHPP
