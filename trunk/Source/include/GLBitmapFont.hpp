// CodeGear C++Builder
// Copyright (c) 1995, 2012 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLBitmapFont.pas' rev: 24.00 (Win32)

#ifndef GlbitmapfontHPP
#define GlbitmapfontHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <System.Classes.hpp>	// Pascal unit
#include <Vcl.Graphics.hpp>	// Pascal unit
#include <GLScene.hpp>	// Pascal unit
#include <VectorGeometry.hpp>	// Pascal unit
#include <GLContext.hpp>	// Pascal unit
#include <GLCrossPlatform.hpp>	// Pascal unit
#include <GLTexture.hpp>	// Pascal unit
#include <GLState.hpp>	// Pascal unit
#include <GLUtils.hpp>	// Pascal unit
#include <GLGraphics.hpp>	// Pascal unit
#include <GLColor.hpp>	// Pascal unit
#include <BaseClasses.hpp>	// Pascal unit
#include <GLRenderContextInfo.hpp>	// Pascal unit
#include <GLTextureFormat.hpp>	// Pascal unit
#include <VectorTypes.hpp>	// Pascal unit
#include <System.UITypes.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Glbitmapfont
{
//-- type declarations -------------------------------------------------------
class DELPHICLASS TBitmapFontRange;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TBitmapFontRange : public System::Classes::TCollectionItem
{
	typedef System::Classes::TCollectionItem inherited;
	
private:
	System::WideString __fastcall GetStartASCII(void);
	System::WideString __fastcall GetStopASCII(void);
	
protected:
	System::WideChar FStartASCII;
	System::WideChar FStopASCII;
	int FStartGlyphIdx;
	int FStopGlyphIdx;
	int FCharCount;
	void __fastcall SetStartASCII(const System::WideString val);
	void __fastcall SetStopASCII(const System::WideString val);
	void __fastcall SetStartGlyphIdx(int val);
	virtual System::UnicodeString __fastcall GetDisplayName(void);
	
public:
	__fastcall virtual TBitmapFontRange(System::Classes::TCollection* Collection);
	__fastcall virtual ~TBitmapFontRange(void);
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	void __fastcall NotifyChange(void);
	
__published:
	__property System::WideString StartASCII = {read=GetStartASCII, write=SetStartASCII};
	__property System::WideString StopASCII = {read=GetStopASCII, write=SetStopASCII};
	__property int StartGlyphIdx = {read=FStartGlyphIdx, write=SetStartGlyphIdx, nodefault};
	__property int StopGlyphIdx = {read=FStopGlyphIdx, nodefault};
	__property int CharCount = {read=FCharCount, nodefault};
};

#pragma pack(pop)

class DELPHICLASS TBitmapFontRanges;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TBitmapFontRanges : public System::Classes::TCollection
{
	typedef System::Classes::TCollection inherited;
	
public:
	TBitmapFontRange* operator[](int index) { return Items[index]; }
	
private:
	int FCharCount;
	
protected:
	System::Classes::TComponent* FOwner;
	DYNAMIC System::Classes::TPersistent* __fastcall GetOwner(void);
	void __fastcall SetItems(int index, TBitmapFontRange* const val);
	TBitmapFontRange* __fastcall GetItems(int index);
	int __fastcall CalcCharacterCount(void);
	virtual void __fastcall Update(System::Classes::TCollectionItem* Item);
	
public:
	__fastcall TBitmapFontRanges(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TBitmapFontRanges(void);
	HIDESBASE TBitmapFontRange* __fastcall Add(void)/* overload */;
	HIDESBASE TBitmapFontRange* __fastcall Add(const System::WideChar startASCII, const System::WideChar stopASCII)/* overload */;
	HIDESBASE TBitmapFontRange* __fastcall Add(const char startASCII, const char stopASCII)/* overload */;
	HIDESBASE TBitmapFontRange* __fastcall FindItemID(int ID);
	__property TBitmapFontRange* Items[int index] = {read=GetItems, write=SetItems/*, default*/};
	int __fastcall CharacterToTileIndex(System::WideChar aChar);
	System::WideChar __fastcall TileIndexToChar(int aIndex);
	void __fastcall NotifyChange(void);
	__property int CharacterCount = {read=FCharCount, nodefault};
};

#pragma pack(pop)

struct TCharInfo;
typedef TCharInfo *PCharInfo;

struct DECLSPEC_DRECORD TCharInfo
{
public:
	System::Word l;
	System::Word t;
	System::Word w;
};


class DELPHICLASS TGLCustomBitmapFont;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLCustomBitmapFont : public Baseclasses::TGLUpdateAbleComponent
{
	typedef Baseclasses::TGLUpdateAbleComponent inherited;
	
private:
	typedef System::DynamicArray<TCharInfo> _TGLCustomBitmapFont__1;
	
	
private:
	TBitmapFontRanges* FRanges;
	Vcl::Graphics::TPicture* FGlyphs;
	int FCharWidth;
	int FCharHeight;
	int FGlyphsIntervalX;
	int FGlyphsIntervalY;
	int FHSpace;
	int FVSpace;
	int FHSpaceFix;
	System::Classes::TList* FUsers;
	Gltexture::TGLMinFilter FMinFilter;
	Gltexture::TGLMagFilter FMagFilter;
	int FTextureWidth;
	int FTextureHeight;
	int FTextRows;
	int FTextCols;
	Gltexture::TGLTextureImageAlpha FGlyphsAlpha;
	System::Classes::TList* FTextures;
	bool FTextureModified;
	Glcontext::TGLTextureHandle* FLastTexture;
	
protected:
	_TGLCustomBitmapFont__1 FChars;
	bool FCharsLoaded;
	void __fastcall ResetCharWidths(int w = 0xffffffff);
	void __fastcall SetCharWidths(int index, int value);
	void __fastcall SetRanges(TBitmapFontRanges* const val);
	void __fastcall SetGlyphs(Vcl::Graphics::TPicture* const val);
	void __fastcall SetCharWidth(const int val);
	void __fastcall SetCharHeight(const int val);
	void __fastcall SetGlyphsIntervalX(const int val);
	void __fastcall SetGlyphsIntervalY(const int val);
	void __fastcall OnGlyphsChanged(System::TObject* Sender);
	void __fastcall SetHSpace(const int val);
	void __fastcall SetVSpace(const int val);
	void __fastcall SetMagFilter(Gltexture::TGLMagFilter AValue);
	void __fastcall SetMinFilter(Gltexture::TGLMinFilter AValue);
	void __fastcall SetGlyphsAlpha(Gltexture::TGLTextureImageAlpha val);
	void __fastcall TextureChanged(void);
	DYNAMIC void __fastcall FreeTextureHandle(void);
	DYNAMIC int __fastcall TextureFormat(void);
	void __fastcall InvalidateUsers(void);
	int __fastcall CharactersPerRow(void);
	void __fastcall GetCharTexCoords(System::WideChar ch, Vectorgeometry::TTexPoint &topLeft, Vectorgeometry::TTexPoint &bottomRight);
	void __fastcall GetICharTexCoords(Glrendercontextinfo::TRenderContextInfo &ARci, int chi, /* out */ Vectorgeometry::TTexPoint &topLeft, /* out */ Vectorgeometry::TTexPoint &bottomRight);
	virtual void __fastcall PrepareImage(Glrendercontextinfo::TRenderContextInfo &ARci);
	void __fastcall PrepareParams(Glrendercontextinfo::TRenderContextInfo &ARci);
	__property Vcl::Graphics::TPicture* Glyphs = {read=FGlyphs, write=SetGlyphs};
	__property int GlyphsIntervalX = {read=FGlyphsIntervalX, write=SetGlyphsIntervalX, nodefault};
	__property int GlyphsIntervalY = {read=FGlyphsIntervalY, write=SetGlyphsIntervalY, nodefault};
	__property TBitmapFontRanges* Ranges = {read=FRanges, write=SetRanges};
	__property int CharWidth = {read=FCharWidth, write=SetCharWidth, default=16};
	__property int HSpace = {read=FHSpace, write=SetHSpace, default=1};
	__property int VSpace = {read=FVSpace, write=SetVSpace, default=1};
	__property int HSpaceFix = {read=FHSpaceFix, write=FHSpaceFix, nodefault};
	__property Gltexture::TGLMagFilter MagFilter = {read=FMagFilter, write=SetMagFilter, default=1};
	__property Gltexture::TGLMinFilter MinFilter = {read=FMinFilter, write=SetMinFilter, default=1};
	__property Gltexture::TGLTextureImageAlpha GlyphsAlpha = {read=FGlyphsAlpha, write=FGlyphsAlpha, default=0};
	
public:
	__fastcall virtual TGLCustomBitmapFont(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TGLCustomBitmapFont(void);
	virtual void __fastcall RegisterUser(Glscene::TGLBaseSceneObject* anObject);
	virtual void __fastcall UnRegisterUser(Glscene::TGLBaseSceneObject* anObject);
	virtual void __fastcall RenderString(Glrendercontextinfo::TRenderContextInfo &ARci, const System::UnicodeString aText, System::Classes::TAlignment aAlignment, Glcrossplatform::TGLTextLayout aLayout, const Vectortypes::TVector4f &aColor, Vectorgeometry::PVector aPosition = (Vectorgeometry::PVector)(0x0), bool aReverseY = false)/* overload */;
	void __fastcall TextOut(Glrendercontextinfo::TRenderContextInfo &rci, float x, float y, const System::UnicodeString text, const Vectortypes::TVector4f &color)/* overload */;
	void __fastcall TextOut(Glrendercontextinfo::TRenderContextInfo &rci, float x, float y, const System::UnicodeString text, const System::Uitypes::TColor color)/* overload */;
	int __fastcall TextWidth(const System::UnicodeString text);
	virtual int __fastcall CharacterToTileIndex(System::WideChar aChar);
	virtual System::WideChar __fastcall TileIndexToChar(int aIndex);
	virtual int __fastcall CharacterCount(void);
	int __fastcall GetCharWidth(System::WideChar ch);
	virtual int __fastcall CalcStringWidth(const System::UnicodeString aText)/* overload */;
	void __fastcall CheckTexture(Glrendercontextinfo::TRenderContextInfo &ARci);
	__property int CharHeight = {read=FCharHeight, write=SetCharHeight, default=16};
	__property int TextureWidth = {read=FTextureWidth, write=FTextureWidth, nodefault};
	__property int TextureHeight = {read=FTextureHeight, write=FTextureHeight, nodefault};
};

#pragma pack(pop)

class DELPHICLASS TGLBitmapFont;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLBitmapFont : public TGLCustomBitmapFont
{
	typedef TGLCustomBitmapFont inherited;
	
__published:
	__property Glyphs;
	__property GlyphsIntervalX;
	__property GlyphsIntervalY;
	__property Ranges;
	__property CharWidth = {default=16};
	__property CharHeight = {default=16};
	__property HSpace = {default=1};
	__property VSpace = {default=1};
	__property MagFilter = {default=1};
	__property MinFilter = {default=1};
	__property GlyphsAlpha = {default=0};
public:
	/* TGLCustomBitmapFont.Create */ inline __fastcall virtual TGLBitmapFont(System::Classes::TComponent* AOwner) : TGLCustomBitmapFont(AOwner) { }
	/* TGLCustomBitmapFont.Destroy */ inline __fastcall virtual ~TGLBitmapFont(void) { }
	
};

#pragma pack(pop)

enum TGLFlatTextOption : unsigned char { ftoTwoSided };

typedef System::Set<TGLFlatTextOption, TGLFlatTextOption::ftoTwoSided, TGLFlatTextOption::ftoTwoSided>  TGLFlatTextOptions;

class DELPHICLASS TGLFlatText;
class PASCALIMPLEMENTATION TGLFlatText : public Glscene::TGLImmaterialSceneObject
{
	typedef Glscene::TGLImmaterialSceneObject inherited;
	
private:
	TGLCustomBitmapFont* FBitmapFont;
	System::UnicodeString FText;
	System::Classes::TAlignment FAlignment;
	Glcrossplatform::TGLTextLayout FLayout;
	Glcolor::TGLColor* FModulateColor;
	TGLFlatTextOptions FOptions;
	
protected:
	void __fastcall SetBitmapFont(TGLCustomBitmapFont* const val);
	void __fastcall SetText(const System::UnicodeString val);
	void __fastcall SetAlignment(const System::Classes::TAlignment val);
	void __fastcall SetLayout(const Glcrossplatform::TGLTextLayout val);
	void __fastcall SetModulateColor(Glcolor::TGLColor* const val);
	void __fastcall SetOptions(const TGLFlatTextOptions val);
	virtual void __fastcall Notification(System::Classes::TComponent* AComponent, System::Classes::TOperation Operation);
	
public:
	__fastcall virtual TGLFlatText(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TGLFlatText(void);
	virtual void __fastcall DoRender(Glrendercontextinfo::TRenderContextInfo &rci, bool renderSelf, bool renderChildren);
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	
__published:
	__property TGLCustomBitmapFont* BitmapFont = {read=FBitmapFont, write=SetBitmapFont};
	__property System::UnicodeString Text = {read=FText, write=SetText};
	__property System::Classes::TAlignment Alignment = {read=FAlignment, write=SetAlignment, nodefault};
	__property Glcrossplatform::TGLTextLayout Layout = {read=FLayout, write=SetLayout, nodefault};
	__property Glcolor::TGLColor* ModulateColor = {read=FModulateColor, write=SetModulateColor};
	__property TGLFlatTextOptions Options = {read=FOptions, write=SetOptions, nodefault};
public:
	/* TGLBaseSceneObject.CreateAsChild */ inline __fastcall TGLFlatText(Glscene::TGLBaseSceneObject* aParentOwner) : Glscene::TGLImmaterialSceneObject(aParentOwner) { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Glbitmapfont */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLBITMAPFONT)
using namespace Glbitmapfont;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// GlbitmapfontHPP
