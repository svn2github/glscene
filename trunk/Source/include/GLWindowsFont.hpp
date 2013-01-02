// CodeGear C++Builder
// Copyright (c) 1995, 2012 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLWindowsFont.pas' rev: 24.00 (Win32)

#ifndef GlwindowsfontHPP
#define GlwindowsfontHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <Winapi.Windows.hpp>	// Pascal unit
#include <GLBitmapFont.hpp>	// Pascal unit
#include <GLRenderContextInfo.hpp>	// Pascal unit
#include <System.Classes.hpp>	// Pascal unit
#include <GLScene.hpp>	// Pascal unit
#include <GLTexture.hpp>	// Pascal unit
#include <Vcl.Graphics.hpp>	// Pascal unit
#include <VectorLists.hpp>	// Pascal unit
#include <GLCrossPlatform.hpp>	// Pascal unit
#include <BaseClasses.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Glwindowsfont
{
//-- type declarations -------------------------------------------------------
class DELPHICLASS TGLWindowsBitmapFont;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLWindowsBitmapFont : public Glbitmapfont::TGLCustomBitmapFont
{
	typedef Glbitmapfont::TGLCustomBitmapFont inherited;
	
private:
	Vcl::Graphics::TFont* FFont;
	void __fastcall SetList(Vectorlists::TIntegerList* const AList);
	
protected:
	void __fastcall SetFont(Vcl::Graphics::TFont* value);
	virtual void __fastcall LoadWindowsFont(void);
	bool __fastcall StoreRanges(void);
	virtual void __fastcall PrepareImage(Glrendercontextinfo::TRenderContextInfo &ARci);
	DYNAMIC int __fastcall TextureFormat(void);
	void __fastcall StreamlineRanges(void);
	
public:
	__fastcall virtual TGLWindowsBitmapFont(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TGLWindowsBitmapFont(void);
	virtual void __fastcall NotifyChange(System::TObject* Sender);
	int __fastcall FontTextureWidth(void);
	int __fastcall FontTextureHeight(void);
	void __fastcall EnsureString(const System::UnicodeString s)/* overload */;
	void __fastcall EnsureChars(const System::WideChar AStart, const System::WideChar AEnd);
	__property Glyphs;
	
__published:
	__property Vcl::Graphics::TFont* Font = {read=FFont, write=SetFont};
	__property HSpace = {default=1};
	__property VSpace = {default=1};
	__property MagFilter = {default=1};
	__property MinFilter = {default=1};
	__property Ranges = {stored=StoreRanges};
};

#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------
}	/* namespace Glwindowsfont */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLWINDOWSFONT)
using namespace Glwindowsfont;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// GlwindowsfontHPP
