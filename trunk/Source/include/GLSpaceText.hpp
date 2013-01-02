// CodeGear C++Builder
// Copyright (c) 1995, 2012 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLSpaceText.pas' rev: 24.00 (Win32)

#ifndef GlspacetextHPP
#define GlspacetextHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <Winapi.Windows.hpp>	// Pascal unit
#include <Winapi.Messages.hpp>	// Pascal unit
#include <System.Classes.hpp>	// Pascal unit
#include <Vcl.Dialogs.hpp>	// Pascal unit
#include <Vcl.Graphics.hpp>	// Pascal unit
#include <Vcl.Controls.hpp>	// Pascal unit
#include <GLScene.hpp>	// Pascal unit
#include <OpenGLTokens.hpp>	// Pascal unit
#include <GLTexture.hpp>	// Pascal unit
#include <GLContext.hpp>	// Pascal unit
#include <VectorGeometry.hpp>	// Pascal unit
#include <GLStrings.hpp>	// Pascal unit
#include <GLRenderContextInfo.hpp>	// Pascal unit
#include <GLState.hpp>	// Pascal unit
#include <System.UITypes.hpp>	// Pascal unit
#include <VectorTypes.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Glspacetext
{
//-- type declarations -------------------------------------------------------
enum TSpaceTextCharRange : unsigned char { stcrDefault, stcrAlphaNum, stcrNumbers, stcrWide };

enum TGLTextHorzAdjust : unsigned char { haLeft, haCenter, haRight, haAligned, haCentrically, haFitIn };

enum TGLTextVertAdjust : unsigned char { vaTop, vaCenter, vaBottom, vaBaseLine };

class DELPHICLASS TGLTextAdjust;
class PASCALIMPLEMENTATION TGLTextAdjust : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
private:
	TGLTextHorzAdjust FHorz;
	TGLTextVertAdjust FVert;
	System::Classes::TNotifyEvent FOnChange;
	void __fastcall SetHorz(const TGLTextHorzAdjust Value);
	void __fastcall SetVert(const TGLTextVertAdjust Value);
	
public:
	__fastcall TGLTextAdjust(void);
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	__property System::Classes::TNotifyEvent OnChange = {read=FOnChange, write=FOnChange};
	
__published:
	__property TGLTextHorzAdjust Horz = {read=FHorz, write=SetHorz, default=0};
	__property TGLTextVertAdjust Vert = {read=FVert, write=SetVert, default=3};
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TGLTextAdjust(void) { }
	
};


struct TFontEntry;
typedef TFontEntry *PFontEntry;

struct DECLSPEC_DRECORD TFontEntry
{
private:
	typedef System::DynamicArray<_GLYPHMETRICSFLOAT> _TFontEntry__1;
	
	
public:
	System::UnicodeString Name;
	Glcontext::TGLVirtualHandleTransf* FVirtualHandle;
	System::Uitypes::TFontStyles Styles;
	float Extrusion;
	int RefCount;
	float allowedDeviation;
	int firstChar;
	int lastChar;
	_TFontEntry__1 glyphMetrics;
	System::Classes::TList* FClients;
};


class DELPHICLASS TGLSpaceText;
class PASCALIMPLEMENTATION TGLSpaceText : public Glscene::TGLSceneObject
{
	typedef Glscene::TGLSceneObject inherited;
	
private:
	Vcl::Graphics::TFont* FFont;
	float FExtrusion;
	float FAllowedDeviation;
	TSpaceTextCharRange FCharacterRange;
	TGLTextAdjust* FAdjust;
	float FAspectRatio;
	float FOblique;
	float FTextHeight;
	System::Classes::TStringList* FLines;
	void __fastcall SetCharacterRange(const TSpaceTextCharRange val);
	void __fastcall SetAllowedDeviation(const float val);
	void __fastcall SetExtrusion(float AValue);
	void __fastcall SetFont(Vcl::Graphics::TFont* AFont);
	System::WideString __fastcall GetText(void);
	void __fastcall SetLines(System::Classes::TStringList* const Value);
	void __fastcall SetText(const System::WideString AText);
	void __fastcall SetAdjust(TGLTextAdjust* const Value);
	void __fastcall SetAspectRatio(const float Value);
	void __fastcall SetOblique(const float Value);
	void __fastcall SetTextHeight(const float Value);
	
protected:
	TFontEntry *FTextFontEntry;
	bool FontChanged;
	DYNAMIC void __fastcall DestroyHandle(void);
	void __fastcall OnFontChange(System::TObject* sender);
	void __fastcall GetFirstAndLastChar(int &firstChar, int &lastChar);
	virtual void __fastcall DoOnLinesChange(System::TObject* sender);
	
public:
	__fastcall virtual TGLSpaceText(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TGLSpaceText(void);
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	virtual void __fastcall BuildList(Glrendercontextinfo::TRenderContextInfo &rci);
	virtual void __fastcall DoRender(Glrendercontextinfo::TRenderContextInfo &ARci, bool ARenderSelf, bool ARenderChildren);
	float __fastcall TextWidth(const System::WideString str = System::WideString());
	float __fastcall TextMaxHeight(const System::WideString str = System::WideString());
	float __fastcall TextMaxUnder(const System::WideString str = System::WideString());
	void __fastcall TextMetrics(const System::WideString str, /* out */ float &width, /* out */ float &maxHeight, /* out */ float &maxUnder);
	void __fastcall NotifyFontChanged(void);
	virtual void __fastcall NotifyChange(System::TObject* sender);
	virtual void __fastcall DefaultHandler(void *Message);
	virtual Vectortypes::TVector4f __fastcall AxisAlignedDimensionsUnscaled(void);
	virtual Vectortypes::TVector4f __fastcall BarycenterAbsolutePosition(void);
	
__published:
	__property float Extrusion = {read=FExtrusion, write=SetExtrusion};
	__property Vcl::Graphics::TFont* Font = {read=FFont, write=SetFont};
	__property System::WideString Text = {read=GetText, write=SetText, stored=false};
	__property System::Classes::TStringList* Lines = {read=FLines, write=SetLines};
	__property float allowedDeviation = {read=FAllowedDeviation, write=SetAllowedDeviation};
	__property TSpaceTextCharRange CharacterRange = {read=FCharacterRange, write=SetCharacterRange, default=0};
	__property float AspectRatio = {read=FAspectRatio, write=SetAspectRatio};
	__property float TextHeight = {read=FTextHeight, write=SetTextHeight};
	__property float Oblique = {read=FOblique, write=SetOblique};
	__property TGLTextAdjust* Adjust = {read=FAdjust, write=SetAdjust};
public:
	/* TGLBaseSceneObject.CreateAsChild */ inline __fastcall TGLSpaceText(Glscene::TGLBaseSceneObject* aParentOwner) : Glscene::TGLSceneObject(aParentOwner) { }
	
};


class DELPHICLASS TFontManager;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TFontManager : public System::Classes::TList
{
	typedef System::Classes::TList inherited;
	
private:
	int FCurrentBase;
	
protected:
	void __fastcall NotifyClients(System::Classes::TList* Clients);
	void __fastcall VirtualHandleAlloc(Glcontext::TGLVirtualHandle* sender, unsigned &handle);
	void __fastcall VirtualHandleDestroy(Glcontext::TGLVirtualHandle* sender, unsigned &handle);
	
public:
	__fastcall TFontManager(void);
	__fastcall virtual ~TFontManager(void);
	PFontEntry __fastcall FindFont(System::UnicodeString AName, System::Uitypes::TFontStyles FStyles, float FExtrusion, float FAllowedDeviation, int FFirstChar, int FLastChar);
	PFontEntry __fastcall GetFontBase(System::UnicodeString AName, System::Uitypes::TFontStyles FStyles, float FExtrusion, float allowedDeviation, int firstChar, int lastChar, System::TObject* client);
	void __fastcall Release(PFontEntry entry, System::TObject* client);
};

#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------
extern PACKAGE unsigned vFontManagerMsgID;
extern PACKAGE TFontManager* __fastcall FontManager(void);
extern PACKAGE void __fastcall ReleaseFontManager(void);
}	/* namespace Glspacetext */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLSPACETEXT)
using namespace Glspacetext;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// GlspacetextHPP
