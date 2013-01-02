// CodeGear C++Builder
// Copyright (c) 1995, 2012 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'HeightTileFile.pas' rev: 24.00 (Win32)

#ifndef HeighttilefileHPP
#define HeighttilefileHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <System.Classes.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Heighttilefile
{
//-- type declarations -------------------------------------------------------
typedef System::Byte *PByte;

typedef System::StaticArray<int, 268435456> TIntegerArray;

typedef TIntegerArray *PIntegerArray;

typedef int *PInteger;

typedef System::StaticArray<short, 536870912> TSmallIntArray;

typedef TSmallIntArray *PSmallIntArray;

typedef short *PSmallInt;

typedef System::StaticArray<System::Int8, 536870912> TShortIntArray;

typedef TShortIntArray *PShortIntArray;

typedef System::Int8 *PShortInt;

#pragma pack(push,1)
struct DECLSPEC_DRECORD THeightTileInfo
{
public:
	int left;
	int top;
	int width;
	int height;
	short min;
	short max;
	short average;
	__int64 fileOffset;
};
#pragma pack(pop)


typedef THeightTileInfo *PHeightTileInfo;

typedef PHeightTileInfo *PPHeightTileInfo;

#pragma pack(push,1)
struct DECLSPEC_DRECORD THeightTile
{
private:
	typedef System::DynamicArray<short> _THeightTile__1;
	
	
public:
	THeightTileInfo info;
	_THeightTile__1 data;
};
#pragma pack(pop)


typedef THeightTile *PHeightTile;

#pragma pack(push,1)
struct DECLSPEC_DRECORD THTFHeader
{
public:
	System::StaticArray<char, 6> FileVersion;
	__int64 TileIndexOffset;
	int SizeX;
	int SizeY;
	int TileSize;
	short DefaultZ;
};
#pragma pack(pop)


class DELPHICLASS THeightTileFile;
typedef System::DynamicArray<int> _THeightTileFile__3;

#pragma pack(push,4)
class PASCALIMPLEMENTATION THeightTileFile : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	typedef System::DynamicArray<THeightTileInfo> _THeightTileFile__1;
	
	typedef System::DynamicArray<unsigned> _THeightTileFile__2;
	
	typedef System::StaticArray<_THeightTileFile__3, 1024> _THeightTileFile__4;
	
	typedef System::DynamicArray<int> _THeightTileFile__5;
	
	typedef System::StaticArray<System::StaticArray<_THeightTileFile__5, 32>, 32> _THeightTileFile__6;
	
	typedef System::DynamicArray<System::Int8> _THeightTileFile__7;
	
	
private:
	System::Classes::TStream* FFile;
	THTFHeader FHeader;
	_THeightTileFile__1 FTileIndex;
	_THeightTileFile__2 FTileMark;
	unsigned FLastMark;
	_THeightTileFile__4 FHashTable;
	_THeightTileFile__6 FQuadTable;
	bool FCreating;
	THeightTile FHeightTile;
	_THeightTileFile__7 FInBuf;
	
protected:
	PHeightTileInfo __fastcall GetTiles(int index);
	int __fastcall QuadTableX(int x);
	int __fastcall QuadTableY(int y);
	void __fastcall PackTile(int aWidth, int aHeight, PSmallIntArray src);
	void __fastcall UnPackTile(PShortIntArray source);
	__property __int64 TileIndexOffset = {read=FHeader.TileIndexOffset, write=FHeader.TileIndexOffset};
	
public:
	__fastcall THeightTileFile(const System::UnicodeString fileName, int aSizeX, int aSizeY, int aTileSize);
	__fastcall THeightTileFile(const System::UnicodeString fileName);
	__fastcall virtual ~THeightTileFile(void);
	int __fastcall GetTileIndex(int aLeft, int aTop);
	PHeightTile __fastcall GetTile(int aLeft, int aTop, PPHeightTileInfo pTileInfo = (PPHeightTileInfo)(0x0));
	void __fastcall CompressTile(int aLeft, int aTop, int aWidth, int aHeight, PSmallIntArray aData);
	void __fastcall ExtractRow(int x, int y, int len, PSmallIntArray dest);
	PHeightTileInfo __fastcall XYTileInfo(int anX, int anY);
	short __fastcall XYHeight(int anX, int anY);
	void __fastcall TilesInRect(int aLeft, int aTop, int aRight, int aBottom, System::Classes::TList* destList);
	int __fastcall TileCount(void);
	__property PHeightTileInfo Tiles[int index] = {read=GetTiles};
	int __fastcall IndexOfTile(PHeightTileInfo aTile);
	int __fastcall TileCompressedSize(int tileIndex);
	__property int SizeX = {read=FHeader.SizeX, nodefault};
	__property int SizeY = {read=FHeader.SizeY, nodefault};
	__property int TileSize = {read=FHeader.TileSize, nodefault};
	__property short DefaultZ = {read=FHeader.DefaultZ, write=FHeader.DefaultZ, nodefault};
};

#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------
static const System::Word cHTFHashTableSize = System::Word(0x3ff);
static const System::Int8 cHTFQuadTableSize = System::Int8(0x1f);
}	/* namespace Heighttilefile */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_HEIGHTTILEFILE)
using namespace Heighttilefile;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// HeighttilefileHPP
