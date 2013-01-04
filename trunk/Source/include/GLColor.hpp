// CodeGear C++Builder
// Copyright (c) 1995, 2012 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLColor.pas' rev: 24.00 (Win32)

#ifndef GlcolorHPP
#define GlcolorHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <System.Classes.hpp>	// Pascal unit
#include <Vcl.Graphics.hpp>	// Pascal unit
#include <VectorTypes.hpp>	// Pascal unit
#include <VectorGeometry.hpp>	// Pascal unit
#include <GLCrossPlatform.hpp>	// Pascal unit
#include <PersistentClasses.hpp>	// Pascal unit
#include <BaseClasses.hpp>	// Pascal unit
#include <System.UITypes.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Glcolor
{
//-- type declarations -------------------------------------------------------
typedef Vectortypes::TVector4f *PColorVector;

typedef Vectortypes::TVector4f TColorVector;

typedef Vectortypes::TVector3b *PRGBColor;

typedef Vectortypes::TVector3b TRGBColor;

class DELPHICLASS TGLColor;
class PASCALIMPLEMENTATION TGLColor : public Baseclasses::TGLUpdateAbleObject
{
	typedef Baseclasses::TGLUpdateAbleObject inherited;
	
private:
	Vectortypes::TVector4f FColor;
	Vectortypes::TVector4f *FPDefaultColor;
	void __fastcall SetColorVector(const Vectortypes::TVector4f &aColor)/* overload */;
	void __fastcall SetColorComponent(int index, float value);
	float __fastcall GetColorComponent(const int Index);
	void __fastcall SetAsWinColor(const System::Uitypes::TColor val);
	System::Uitypes::TColor __fastcall GetAsWinColor(void);
	void __fastcall SetDirectColorVector(const Vectortypes::TVector4f &AColor);
	
protected:
	virtual void __fastcall DefineProperties(System::Classes::TFiler* Filer);
	void __fastcall ReadData(System::Classes::TStream* Stream);
	void __fastcall WriteData(System::Classes::TStream* Stream);
	Vectortypes::TVector4f __fastcall GetHSVA(void);
	void __fastcall SetHSVA(const Vectortypes::TVector4f &hsva);
	
public:
	__fastcall virtual TGLColor(System::Classes::TPersistent* AOwner);
	__fastcall TGLColor(System::Classes::TPersistent* AOwner, const Vectortypes::TVector4f &color, System::Classes::TNotifyEvent changeEvent);
	__fastcall virtual ~TGLColor(void);
	virtual void __fastcall NotifyChange(System::TObject* Sender);
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	void __fastcall Initialize(const Vectortypes::TVector4f &color);
	System::PSingle __fastcall AsAddress(void);
	void __fastcall RandomColor(void);
	void __fastcall SetColor(float red, float green, float blue, float alpha = 1.000000E+00)/* overload */;
	__property Vectortypes::TVector4f Color = {read=FColor, write=SetColorVector};
	__property Vectortypes::TVector4f DirectColor = {read=FColor, write=SetDirectColorVector};
	__property System::Uitypes::TColor AsWinColor = {read=GetAsWinColor, write=SetAsWinColor, nodefault};
	__property Vectortypes::TVector4f HSVA = {read=GetHSVA, write=SetHSVA};
	__property Vectortypes::TVector4f DefaultColor = {read=FColor};
	
__published:
	__property float Red = {read=GetColorComponent, write=SetColorComponent, stored=false, index=0};
	__property float Green = {read=GetColorComponent, write=SetColorComponent, stored=false, index=1};
	__property float Blue = {read=GetColorComponent, write=SetColorComponent, stored=false, index=2};
	__property float Alpha = {read=GetColorComponent, write=SetColorComponent, stored=false, index=3};
};


struct TColorEntry;
typedef TColorEntry *PColorEntry;

struct DECLSPEC_DRECORD TColorEntry
{
public:
	System::SmallString<31>  Name;
	Vectortypes::TVector4f Color;
};


class DELPHICLASS TGLColorManager;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLColorManager : public System::Classes::TList
{
	typedef System::Classes::TList inherited;
	
public:
	__fastcall virtual ~TGLColorManager(void);
	void __fastcall AddColor(const System::UnicodeString aName, const Vectortypes::TVector4f &aColor);
	void __fastcall EnumColors(System::Classes::TGetStrProc Proc)/* overload */;
	void __fastcall EnumColors(System::Classes::TStrings* AValues)/* overload */;
	Vectortypes::TVector4f __fastcall FindColor(const System::UnicodeString aName);
	Vectortypes::TVector4f __fastcall GetColor(const System::UnicodeString aName);
	System::UnicodeString __fastcall GetColorName(const Vectortypes::TVector4f &aColor);
	void __fastcall RegisterDefaultColors(void);
	void __fastcall RemoveColor(const System::UnicodeString aName);
public:
	/* TObject.Create */ inline __fastcall TGLColorManager(void) : System::Classes::TList() { }
	
};

#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------
static const System::Uitypes::TColor clForeground = System::Uitypes::TColor(-1);
static const System::Uitypes::TColor clButton = System::Uitypes::TColor(-2);
static const System::Uitypes::TColor clLight = System::Uitypes::TColor(-3);
static const System::Uitypes::TColor clMidlight = System::Uitypes::TColor(-4);
static const System::Uitypes::TColor clDark = System::Uitypes::TColor(-5);
static const System::Uitypes::TColor clMid = System::Uitypes::TColor(-6);
static const System::Uitypes::TColor clText = System::Uitypes::TColor(-7);
static const System::Uitypes::TColor clBrightText = System::Uitypes::TColor(-8);
static const System::Uitypes::TColor clButtonText = System::Uitypes::TColor(-9);
static const System::Uitypes::TColor clBase = System::Uitypes::TColor(-10);
static const System::Uitypes::TColor clBackground = System::Uitypes::TColor(-11);
static const System::Uitypes::TColor clShadow = System::Uitypes::TColor(-12);
static const System::Uitypes::TColor clHighlight = System::Uitypes::TColor(-13);
static const System::Uitypes::TColor clHighlightedText = System::Uitypes::TColor(-14);
static const System::Int8 cloNormal = System::Int8(0x20);
static const System::Int8 cloDisabled = System::Int8(0x40);
static const System::Int8 cloActive = System::Int8(0x60);
static const System::Uitypes::TColor clNormalForeground = System::Uitypes::TColor(-33);
static const System::Uitypes::TColor clNormalButton = System::Uitypes::TColor(-34);
static const System::Uitypes::TColor clNormalLight = System::Uitypes::TColor(-35);
static const System::Uitypes::TColor clNormalMidlight = System::Uitypes::TColor(-36);
static const System::Uitypes::TColor clNormalDark = System::Uitypes::TColor(-37);
static const System::Uitypes::TColor clNormalMid = System::Uitypes::TColor(-38);
static const System::Uitypes::TColor clNormalText = System::Uitypes::TColor(-39);
static const System::Uitypes::TColor clNormalBrightText = System::Uitypes::TColor(-40);
static const System::Uitypes::TColor clNormalButtonText = System::Uitypes::TColor(-41);
static const System::Uitypes::TColor clNormalBase = System::Uitypes::TColor(-42);
static const System::Uitypes::TColor clNormalBackground = System::Uitypes::TColor(-43);
static const System::Uitypes::TColor clNormalShadow = System::Uitypes::TColor(-44);
static const System::Uitypes::TColor clNormalHighlight = System::Uitypes::TColor(-45);
static const System::Uitypes::TColor clNormalHighlightedText = System::Uitypes::TColor(-46);
static const System::Uitypes::TColor clDisabledForeground = System::Uitypes::TColor(-65);
static const System::Uitypes::TColor clDisabledButton = System::Uitypes::TColor(-66);
static const System::Uitypes::TColor clDisabledLight = System::Uitypes::TColor(-67);
static const System::Uitypes::TColor clDisabledMidlight = System::Uitypes::TColor(-68);
static const System::Uitypes::TColor clDisabledDark = System::Uitypes::TColor(-69);
static const System::Uitypes::TColor clDisabledMid = System::Uitypes::TColor(-70);
static const System::Uitypes::TColor clDisabledText = System::Uitypes::TColor(-71);
static const System::Uitypes::TColor clDisabledBrightText = System::Uitypes::TColor(-72);
static const System::Uitypes::TColor clDisabledButtonText = System::Uitypes::TColor(-73);
static const System::Uitypes::TColor clDisabledBase = System::Uitypes::TColor(-74);
static const System::Uitypes::TColor clDisabledBackground = System::Uitypes::TColor(-75);
static const System::Uitypes::TColor clDisabledShadow = System::Uitypes::TColor(-76);
static const System::Uitypes::TColor clDisabledHighlight = System::Uitypes::TColor(-77);
static const System::Uitypes::TColor clDisabledHighlightedText = System::Uitypes::TColor(-78);
static const System::Uitypes::TColor clActiveForeground = System::Uitypes::TColor(-97);
static const System::Uitypes::TColor clActiveButton = System::Uitypes::TColor(-98);
static const System::Uitypes::TColor clActiveLight = System::Uitypes::TColor(-99);
static const System::Uitypes::TColor clActiveMidlight = System::Uitypes::TColor(-100);
static const System::Uitypes::TColor clActiveDark = System::Uitypes::TColor(-101);
static const System::Uitypes::TColor clActiveMid = System::Uitypes::TColor(-102);
static const System::Uitypes::TColor clActiveText = System::Uitypes::TColor(-103);
static const System::Uitypes::TColor clActiveBrightText = System::Uitypes::TColor(-104);
static const System::Uitypes::TColor clActiveButtonText = System::Uitypes::TColor(-105);
static const System::Uitypes::TColor clActiveBase = System::Uitypes::TColor(-106);
static const System::Uitypes::TColor clActiveBackground = System::Uitypes::TColor(-107);
static const System::Uitypes::TColor clActiveShadow = System::Uitypes::TColor(-108);
static const System::Uitypes::TColor clActiveHighlight = System::Uitypes::TColor(-109);
static const System::Uitypes::TColor clActiveHighlightedText = System::Uitypes::TColor(-110);
static const System::Uitypes::TColor clFirstSpecialColor = System::Uitypes::TColor(-110);
static const int clMask = int(16777215);
static const int clDontMask = int(0);
extern PACKAGE Vectortypes::TVector4f clrScrollBar;
extern PACKAGE Vectortypes::TVector4f clrBackground;
extern PACKAGE Vectortypes::TVector4f clrActiveCaption;
extern PACKAGE Vectortypes::TVector4f clrInactiveCaption;
extern PACKAGE Vectortypes::TVector4f clrMenu;
extern PACKAGE Vectortypes::TVector4f clrWindow;
extern PACKAGE Vectortypes::TVector4f clrWindowFrame;
extern PACKAGE Vectortypes::TVector4f clrMenuText;
extern PACKAGE Vectortypes::TVector4f clrWindowText;
extern PACKAGE Vectortypes::TVector4f clrCaptionText;
extern PACKAGE Vectortypes::TVector4f clrActiveBorder;
extern PACKAGE Vectortypes::TVector4f clrInactiveBorder;
extern PACKAGE Vectortypes::TVector4f clrAppWorkSpace;
extern PACKAGE Vectortypes::TVector4f clrHighlight;
extern PACKAGE Vectortypes::TVector4f clrHighlightText;
extern PACKAGE Vectortypes::TVector4f clrBtnFace;
extern PACKAGE Vectortypes::TVector4f clrBtnShadow;
extern PACKAGE Vectortypes::TVector4f clrGrayText;
extern PACKAGE Vectortypes::TVector4f clrBtnText;
extern PACKAGE Vectortypes::TVector4f clrInactiveCaptionText;
extern PACKAGE Vectortypes::TVector4f clrBtnHighlight;
extern PACKAGE Vectortypes::TVector4f clr3DDkShadow;
extern PACKAGE Vectortypes::TVector4f clr3DLight;
extern PACKAGE Vectortypes::TVector4f clrInfoText;
extern PACKAGE Vectortypes::TVector4f clrInfoBk;
extern PACKAGE Vectortypes::TVector4f clrTransparent;
extern PACKAGE Vectortypes::TVector4f clrBlack;
extern PACKAGE Vectortypes::TVector4f clrGray05;
extern PACKAGE Vectortypes::TVector4f clrGray10;
extern PACKAGE Vectortypes::TVector4f clrGray15;
extern PACKAGE Vectortypes::TVector4f clrGray20;
extern PACKAGE Vectortypes::TVector4f clrGray25;
extern PACKAGE Vectortypes::TVector4f clrGray30;
extern PACKAGE Vectortypes::TVector4f clrGray35;
extern PACKAGE Vectortypes::TVector4f clrGray40;
extern PACKAGE Vectortypes::TVector4f clrGray45;
extern PACKAGE Vectortypes::TVector4f clrGray50;
extern PACKAGE Vectortypes::TVector4f clrGray55;
extern PACKAGE Vectortypes::TVector4f clrGray60;
extern PACKAGE Vectortypes::TVector4f clrGray65;
extern PACKAGE Vectortypes::TVector4f clrGray70;
extern PACKAGE Vectortypes::TVector4f clrGray75;
extern PACKAGE Vectortypes::TVector4f clrGray80;
extern PACKAGE Vectortypes::TVector4f clrGray85;
extern PACKAGE Vectortypes::TVector4f clrGray90;
extern PACKAGE Vectortypes::TVector4f clrGray95;
extern PACKAGE Vectortypes::TVector4f clrWhite;
extern PACKAGE Vectortypes::TVector4f clrDimGray;
extern PACKAGE Vectortypes::TVector4f clrGray;
extern PACKAGE Vectortypes::TVector4f clrLightGray;
extern PACKAGE Vectortypes::TVector4f clrAquamarine;
extern PACKAGE Vectortypes::TVector4f clrBlueViolet;
extern PACKAGE Vectortypes::TVector4f clrBrown;
extern PACKAGE Vectortypes::TVector4f clrCadetBlue;
extern PACKAGE Vectortypes::TVector4f clrCoral;
extern PACKAGE Vectortypes::TVector4f clrCornflowerBlue;
extern PACKAGE Vectortypes::TVector4f clrDarkGreen;
extern PACKAGE Vectortypes::TVector4f clrDarkOliveGreen;
extern PACKAGE Vectortypes::TVector4f clrDarkOrchid;
extern PACKAGE Vectortypes::TVector4f clrDarkSlateBlue;
extern PACKAGE Vectortypes::TVector4f clrDarkSlateGray;
extern PACKAGE Vectortypes::TVector4f clrDarkSlateGrey;
extern PACKAGE Vectortypes::TVector4f clrDarkTurquoise;
extern PACKAGE Vectortypes::TVector4f clrFirebrick;
extern PACKAGE Vectortypes::TVector4f clrForestGreen;
extern PACKAGE Vectortypes::TVector4f clrGold;
extern PACKAGE Vectortypes::TVector4f clrGoldenrod;
extern PACKAGE Vectortypes::TVector4f clrGreenYellow;
extern PACKAGE Vectortypes::TVector4f clrIndian;
extern PACKAGE Vectortypes::TVector4f clrKhaki;
extern PACKAGE Vectortypes::TVector4f clrLightBlue;
extern PACKAGE Vectortypes::TVector4f clrLightSteelBlue;
extern PACKAGE Vectortypes::TVector4f clrLimeGreen;
extern PACKAGE Vectortypes::TVector4f clrMaroon;
extern PACKAGE Vectortypes::TVector4f clrMediumAquamarine;
extern PACKAGE Vectortypes::TVector4f clrMediumBlue;
extern PACKAGE Vectortypes::TVector4f clrMediumForestGreen;
extern PACKAGE Vectortypes::TVector4f clrMediumGoldenrod;
extern PACKAGE Vectortypes::TVector4f clrMediumOrchid;
extern PACKAGE Vectortypes::TVector4f clrMediumSeaGreen;
extern PACKAGE Vectortypes::TVector4f clrMediumSlateBlue;
extern PACKAGE Vectortypes::TVector4f clrMediumSpringGreen;
extern PACKAGE Vectortypes::TVector4f clrMediumTurquoise;
extern PACKAGE Vectortypes::TVector4f clrMediumViolet;
extern PACKAGE Vectortypes::TVector4f clrMidnightBlue;
extern PACKAGE Vectortypes::TVector4f clrNavy;
extern PACKAGE Vectortypes::TVector4f clrNavyBlue;
extern PACKAGE Vectortypes::TVector4f clrOrange;
extern PACKAGE Vectortypes::TVector4f clrOrangeRed;
extern PACKAGE Vectortypes::TVector4f clrOrchid;
extern PACKAGE Vectortypes::TVector4f clrPaleGreen;
extern PACKAGE Vectortypes::TVector4f clrPink;
extern PACKAGE Vectortypes::TVector4f clrPlum;
extern PACKAGE Vectortypes::TVector4f clrSalmon;
extern PACKAGE Vectortypes::TVector4f clrSeaGreen;
extern PACKAGE Vectortypes::TVector4f clrSienna;
extern PACKAGE Vectortypes::TVector4f clrSkyBlue;
extern PACKAGE Vectortypes::TVector4f clrSlateBlue;
extern PACKAGE Vectortypes::TVector4f clrSpringGreen;
extern PACKAGE Vectortypes::TVector4f clrSteelBlue;
extern PACKAGE Vectortypes::TVector4f clrTan;
extern PACKAGE Vectortypes::TVector4f clrThistle;
extern PACKAGE Vectortypes::TVector4f clrTurquoise;
extern PACKAGE Vectortypes::TVector4f clrViolet;
extern PACKAGE Vectortypes::TVector4f clrVioletRed;
extern PACKAGE Vectortypes::TVector4f clrWheat;
extern PACKAGE Vectortypes::TVector4f clrYellowGreen;
extern PACKAGE Vectortypes::TVector4f clrSummerSky;
extern PACKAGE Vectortypes::TVector4f clrRichBlue;
extern PACKAGE Vectortypes::TVector4f clrBrass;
extern PACKAGE Vectortypes::TVector4f clrCopper;
extern PACKAGE Vectortypes::TVector4f clrBronze;
extern PACKAGE Vectortypes::TVector4f clrBronze2;
extern PACKAGE Vectortypes::TVector4f clrSilver;
extern PACKAGE Vectortypes::TVector4f clrBrightGold;
extern PACKAGE Vectortypes::TVector4f clrOldGold;
extern PACKAGE Vectortypes::TVector4f clrFeldspar;
extern PACKAGE Vectortypes::TVector4f clrQuartz;
extern PACKAGE Vectortypes::TVector4f clrNeonPink;
extern PACKAGE Vectortypes::TVector4f clrDarkPurple;
extern PACKAGE Vectortypes::TVector4f clrNeonBlue;
extern PACKAGE Vectortypes::TVector4f clrCoolCopper;
extern PACKAGE Vectortypes::TVector4f clrMandarinOrange;
extern PACKAGE Vectortypes::TVector4f clrLightWood;
extern PACKAGE Vectortypes::TVector4f clrMediumWood;
extern PACKAGE Vectortypes::TVector4f clrDarkWood;
extern PACKAGE Vectortypes::TVector4f clrSpicyPink;
extern PACKAGE Vectortypes::TVector4f clrSemiSweetChoc;
extern PACKAGE Vectortypes::TVector4f clrBakersChoc;
extern PACKAGE Vectortypes::TVector4f clrFlesh;
extern PACKAGE Vectortypes::TVector4f clrNewTan;
extern PACKAGE Vectortypes::TVector4f clrNewMidnightBlue;
extern PACKAGE Vectortypes::TVector4f clrVeryDarkBrown;
extern PACKAGE Vectortypes::TVector4f clrDarkBrown;
extern PACKAGE Vectortypes::TVector4f clrDarkTan;
extern PACKAGE Vectortypes::TVector4f clrGreenCopper;
extern PACKAGE Vectortypes::TVector4f clrDkGreenCopper;
extern PACKAGE Vectortypes::TVector4f clrDustyRose;
extern PACKAGE Vectortypes::TVector4f clrHuntersGreen;
extern PACKAGE Vectortypes::TVector4f clrScarlet;
extern PACKAGE Vectortypes::TVector4f clrMediumPurple;
extern PACKAGE Vectortypes::TVector4f clrLightPurple;
extern PACKAGE Vectortypes::TVector4f clrVeryLightPurple;
extern PACKAGE Vectortypes::TVector4f clrGreen;
extern PACKAGE Vectortypes::TVector4f clrOlive;
extern PACKAGE Vectortypes::TVector4f clrPurple;
extern PACKAGE Vectortypes::TVector4f clrTeal;
extern PACKAGE Vectortypes::TVector4f clrRed;
extern PACKAGE Vectortypes::TVector4f clrLime;
extern PACKAGE Vectortypes::TVector4f clrYellow;
extern PACKAGE Vectortypes::TVector4f clrBlue;
extern PACKAGE Vectortypes::TVector4f clrFuchsia;
extern PACKAGE Vectortypes::TVector4f clrAqua;
#define cDefaultNormalMapScale  (1.250000E-01)
extern PACKAGE bool vUseDefaultColorSets;
extern PACKAGE TGLColorManager* __fastcall ColorManager(void);
extern PACKAGE Vectortypes::TVector4f __fastcall ConvertWinColor(System::Uitypes::TColor aColor, float alpha = 1.000000E+00);
extern PACKAGE void __fastcall InitGLSceneColors(void);
extern PACKAGE System::Uitypes::TColor __fastcall ConvertColorVector(const Vectortypes::TVector4f &AColor)/* overload */;
extern PACKAGE System::Uitypes::TColor __fastcall ConvertColorVector(const Vectortypes::TVector4f &AColor, float intensity)/* overload */;
extern PACKAGE Vectortypes::TVector4f __fastcall ConvertRGBColor(System::Byte const *aColor, const int aColor_Size);
extern PACKAGE void __fastcall RegisterColor(const System::UnicodeString aName, const Vectortypes::TVector4f &aColor);
extern PACKAGE void __fastcall UnRegisterColor(const System::UnicodeString aName);
}	/* namespace Glcolor */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLCOLOR)
using namespace Glcolor;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// GlcolorHPP
