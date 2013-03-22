// CodeGear C++Builder
// Copyright (c) 1995, 2012 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLHUDObjects.pas' rev: 24.00 (Win32)

#ifndef GlhudobjectsHPP
#define GlhudobjectsHPP

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
#include <GLBitmapFont.hpp>	// Pascal unit
#include <GLCrossPlatform.hpp>	// Pascal unit
#include <GLColor.hpp>	// Pascal unit
#include <GLRenderContextInfo.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Glhudobjects
{
//-- type declarations -------------------------------------------------------
class DELPHICLASS TGLHUDSprite;
class PASCALIMPLEMENTATION TGLHUDSprite : public Globjects::TGLSprite
{
	typedef Globjects::TGLSprite inherited;
	
private:
	int FXTiles;
	int FYTiles;
	bool __fastcall StoreWidth(void);
	bool __fastcall StoreHeight(void);
	
protected:
	void __fastcall SetXTiles(const int val);
	void __fastcall SetYTiles(const int val);
	
public:
	__fastcall virtual TGLHUDSprite(System::Classes::TComponent* AOwner);
	virtual void __fastcall DoRender(Glrendercontextinfo::TRenderContextInfo &rci, bool renderSelf, bool renderChildren);
	
__published:
	__property int XTiles = {read=FXTiles, write=SetXTiles, default=1};
	__property int YTiles = {read=FYTiles, write=SetYTiles, default=1};
	__property Width = {stored=StoreWidth, default=0};
	__property Height = {stored=StoreHeight, default=0};
public:
	/* TGLCustomSceneObject.Destroy */ inline __fastcall virtual ~TGLHUDSprite(void) { }
	
public:
	/* TGLBaseSceneObject.CreateAsChild */ inline __fastcall TGLHUDSprite(Glscene::TGLBaseSceneObject* aParentOwner) : Globjects::TGLSprite(aParentOwner) { }
	
};


class DELPHICLASS TGLHUDText;
class PASCALIMPLEMENTATION TGLHUDText : public Glscene::TGLImmaterialSceneObject
{
	typedef Glscene::TGLImmaterialSceneObject inherited;
	
private:
	Glbitmapfont::TGLCustomBitmapFont* FBitmapFont;
	System::UnicodeString FText;
	float FRotation;
	System::Classes::TAlignment FAlignment;
	Glcrossplatform::TGLTextLayout FLayout;
	Glcolor::TGLColor* FModulateColor;
	
protected:
	void __fastcall SetBitmapFont(Glbitmapfont::TGLCustomBitmapFont* const val);
	void __fastcall SetText(const System::UnicodeString val);
	HIDESBASE void __fastcall SetRotation(const float val);
	void __fastcall SetAlignment(const System::Classes::TAlignment val);
	void __fastcall SetLayout(const Glcrossplatform::TGLTextLayout val);
	void __fastcall SetModulateColor(Glcolor::TGLColor* const val);
	virtual void __fastcall Notification(System::Classes::TComponent* AComponent, System::Classes::TOperation Operation);
	void __fastcall RenderTextAtPosition(const float X, const float Y, const float Z, Glrendercontextinfo::TRenderContextInfo &rci);
	
public:
	__fastcall virtual TGLHUDText(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TGLHUDText(void);
	virtual void __fastcall DoRender(Glrendercontextinfo::TRenderContextInfo &rci, bool renderSelf, bool renderChildren);
	
__published:
	__property Glbitmapfont::TGLCustomBitmapFont* BitmapFont = {read=FBitmapFont, write=SetBitmapFont};
	__property System::UnicodeString Text = {read=FText, write=SetText};
	__property float Rotation = {read=FRotation, write=SetRotation};
	__property System::Classes::TAlignment Alignment = {read=FAlignment, write=SetAlignment, default=0};
	__property Glcrossplatform::TGLTextLayout Layout = {read=FLayout, write=SetLayout, default=0};
	__property Glcolor::TGLColor* ModulateColor = {read=FModulateColor, write=SetModulateColor};
public:
	/* TGLBaseSceneObject.CreateAsChild */ inline __fastcall TGLHUDText(Glscene::TGLBaseSceneObject* aParentOwner) : Glscene::TGLImmaterialSceneObject(aParentOwner) { }
	
};


class DELPHICLASS TGLAbsoluteHUDText;
class PASCALIMPLEMENTATION TGLAbsoluteHUDText : public TGLHUDText
{
	typedef TGLHUDText inherited;
	
public:
	virtual void __fastcall DoRender(Glrendercontextinfo::TRenderContextInfo &rci, bool renderSelf, bool renderChildren);
public:
	/* TGLHUDText.Create */ inline __fastcall virtual TGLAbsoluteHUDText(System::Classes::TComponent* AOwner) : TGLHUDText(AOwner) { }
	/* TGLHUDText.Destroy */ inline __fastcall virtual ~TGLAbsoluteHUDText(void) { }
	
public:
	/* TGLBaseSceneObject.CreateAsChild */ inline __fastcall TGLAbsoluteHUDText(Glscene::TGLBaseSceneObject* aParentOwner) : TGLHUDText(aParentOwner) { }
	
};


class DELPHICLASS TGLResolutionIndependantHUDText;
class PASCALIMPLEMENTATION TGLResolutionIndependantHUDText : public TGLHUDText
{
	typedef TGLHUDText inherited;
	
public:
	virtual void __fastcall DoRender(Glrendercontextinfo::TRenderContextInfo &rci, bool renderSelf, bool renderChildren);
	__fastcall virtual TGLResolutionIndependantHUDText(System::Classes::TComponent* AOwner);
public:
	/* TGLHUDText.Destroy */ inline __fastcall virtual ~TGLResolutionIndependantHUDText(void) { }
	
public:
	/* TGLBaseSceneObject.CreateAsChild */ inline __fastcall TGLResolutionIndependantHUDText(Glscene::TGLBaseSceneObject* aParentOwner) : TGLHUDText(aParentOwner) { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Glhudobjects */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLHUDOBJECTS)
using namespace Glhudobjects;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// GlhudobjectsHPP
