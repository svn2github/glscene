// CodeGear C++Builder
// Copyright (c) 1995, 2012 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLSkydome.pas' rev: 24.00 (Win32)

#ifndef GlskydomeHPP
#define GlskydomeHPP

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
#include <GLGraphics.hpp>	// Pascal unit
#include <GLCrossPlatform.hpp>	// Pascal unit
#include <VectorTypes.hpp>	// Pascal unit
#include <GLColor.hpp>	// Pascal unit
#include <GLRenderContextInfo.hpp>	// Pascal unit
#include <System.UITypes.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Glskydome
{
//-- type declarations -------------------------------------------------------
class DELPHICLASS TSkyDomeBand;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TSkyDomeBand : public System::Classes::TCollectionItem
{
	typedef System::Classes::TCollectionItem inherited;
	
private:
	float FStartAngle;
	float FStopAngle;
	Glcolor::TGLColor* FStartColor;
	Glcolor::TGLColor* FStopColor;
	int FSlices;
	int FStacks;
	
protected:
	virtual System::UnicodeString __fastcall GetDisplayName(void);
	void __fastcall SetStartAngle(const float val);
	void __fastcall SetStartColor(Glcolor::TGLColor* const val);
	void __fastcall SetStopAngle(const float val);
	void __fastcall SetStopColor(Glcolor::TGLColor* const val);
	void __fastcall SetSlices(const int val);
	void __fastcall SetStacks(const int val);
	void __fastcall OnColorChange(System::TObject* sender);
	
public:
	__fastcall virtual TSkyDomeBand(System::Classes::TCollection* Collection);
	__fastcall virtual ~TSkyDomeBand(void);
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	void __fastcall BuildList(Glrendercontextinfo::TRenderContextInfo &rci);
	
__published:
	__property float StartAngle = {read=FStartAngle, write=SetStartAngle};
	__property Glcolor::TGLColor* StartColor = {read=FStartColor, write=SetStartColor};
	__property float StopAngle = {read=FStopAngle, write=SetStopAngle};
	__property Glcolor::TGLColor* StopColor = {read=FStopColor, write=SetStopColor};
	__property int Slices = {read=FSlices, write=SetSlices, default=12};
	__property int Stacks = {read=FStacks, write=SetStacks, default=1};
};

#pragma pack(pop)

class DELPHICLASS TSkyDomeBands;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TSkyDomeBands : public System::Classes::TCollection
{
	typedef System::Classes::TCollection inherited;
	
public:
	TSkyDomeBand* operator[](int index) { return Items[index]; }
	
protected:
	System::Classes::TComponent* owner;
	DYNAMIC System::Classes::TPersistent* __fastcall GetOwner(void);
	void __fastcall SetItems(int index, TSkyDomeBand* const val);
	TSkyDomeBand* __fastcall GetItems(int index);
	
public:
	__fastcall TSkyDomeBands(System::Classes::TComponent* AOwner);
	HIDESBASE TSkyDomeBand* __fastcall Add(void);
	HIDESBASE TSkyDomeBand* __fastcall FindItemID(int ID);
	__property TSkyDomeBand* Items[int index] = {read=GetItems, write=SetItems/*, default*/};
	void __fastcall NotifyChange(void);
	void __fastcall BuildList(Glrendercontextinfo::TRenderContextInfo &rci);
public:
	/* TCollection.Destroy */ inline __fastcall virtual ~TSkyDomeBands(void) { }
	
};

#pragma pack(pop)

class DELPHICLASS TSkyDomeStar;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TSkyDomeStar : public System::Classes::TCollectionItem
{
	typedef System::Classes::TCollectionItem inherited;
	
private:
	float FRA;
	float FDec;
	float FMagnitude;
	System::Uitypes::TColor FColor;
	Vectortypes::TVector3f FCacheCoord;
	
protected:
	virtual System::UnicodeString __fastcall GetDisplayName(void);
	
public:
	__fastcall virtual TSkyDomeStar(System::Classes::TCollection* Collection);
	__fastcall virtual ~TSkyDomeStar(void);
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	
__published:
	__property float RA = {read=FRA, write=FRA};
	__property float Dec = {read=FDec, write=FDec};
	__property float Magnitude = {read=FMagnitude, write=FMagnitude};
	__property System::Uitypes::TColor Color = {read=FColor, write=FColor, nodefault};
};

#pragma pack(pop)

class DELPHICLASS TSkyDomeStars;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TSkyDomeStars : public System::Classes::TCollection
{
	typedef System::Classes::TCollection inherited;
	
public:
	TSkyDomeStar* operator[](int index) { return Items[index]; }
	
protected:
	System::Classes::TComponent* owner;
	DYNAMIC System::Classes::TPersistent* __fastcall GetOwner(void);
	void __fastcall SetItems(int index, TSkyDomeStar* const val);
	TSkyDomeStar* __fastcall GetItems(int index);
	void __fastcall PrecomputeCartesianCoordinates(void);
	
public:
	__fastcall TSkyDomeStars(System::Classes::TComponent* AOwner);
	HIDESBASE TSkyDomeStar* __fastcall Add(void);
	HIDESBASE TSkyDomeStar* __fastcall FindItemID(int ID);
	__property TSkyDomeStar* Items[int index] = {read=GetItems, write=SetItems/*, default*/};
	void __fastcall BuildList(Glrendercontextinfo::TRenderContextInfo &rci, bool twinkle);
	void __fastcall AddRandomStars(const int nb, const System::Uitypes::TColor color, const bool limitToTopDome = false)/* overload */;
	void __fastcall AddRandomStars(const int nb, const Vectortypes::TVector3b ColorMin, const Vectortypes::TVector3b ColorMax, const float Magnitude_min, const float Magnitude_max, const bool limitToTopDome = false)/* overload */;
	void __fastcall LoadStarsFile(const System::UnicodeString starsFileName);
public:
	/* TCollection.Destroy */ inline __fastcall virtual ~TSkyDomeStars(void) { }
	
};

#pragma pack(pop)

enum TSkyDomeOption : unsigned char { sdoTwinkle };

typedef System::Set<TSkyDomeOption, TSkyDomeOption::sdoTwinkle, TSkyDomeOption::sdoTwinkle>  TSkyDomeOptions;

class DELPHICLASS TGLSkyDome;
class PASCALIMPLEMENTATION TGLSkyDome : public Glscene::TGLCameraInvariantObject
{
	typedef Glscene::TGLCameraInvariantObject inherited;
	
private:
	TSkyDomeOptions FOptions;
	TSkyDomeBands* FBands;
	TSkyDomeStars* FStars;
	
protected:
	void __fastcall SetBands(TSkyDomeBands* const val);
	void __fastcall SetStars(TSkyDomeStars* const val);
	void __fastcall SetOptions(const TSkyDomeOptions val);
	
public:
	__fastcall virtual TGLSkyDome(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TGLSkyDome(void);
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	virtual void __fastcall BuildList(Glrendercontextinfo::TRenderContextInfo &rci);
	
__published:
	__property TSkyDomeBands* Bands = {read=FBands, write=SetBands};
	__property TSkyDomeStars* Stars = {read=FStars, write=SetStars};
	__property TSkyDomeOptions Options = {read=FOptions, write=SetOptions, default=0};
public:
	/* TGLBaseSceneObject.CreateAsChild */ inline __fastcall TGLSkyDome(Glscene::TGLBaseSceneObject* aParentOwner) : Glscene::TGLCameraInvariantObject(aParentOwner) { }
	
};


enum TEarthSkydomeOption : unsigned char { esoFadeStarsWithSun, esoRotateOnTwelveHours, esoDepthTest };

typedef System::Set<TEarthSkydomeOption, TEarthSkydomeOption::esoFadeStarsWithSun, TEarthSkydomeOption::esoDepthTest>  TEarthSkydomeOptions;

class DELPHICLASS TGLEarthSkyDome;
class PASCALIMPLEMENTATION TGLEarthSkyDome : public TGLSkyDome
{
	typedef TGLSkyDome inherited;
	
private:
	float FSunElevation;
	float FTurbidity;
	Vectortypes::TVector4f FCurSunColor;
	Vectortypes::TVector4f FCurSkyColor;
	Vectortypes::TVector4f FCurHazeColor;
	float FCurHazeTurbid;
	float FCurSunSkyTurbid;
	Glcolor::TGLColor* FSunZenithColor;
	Glcolor::TGLColor* FSunDawnColor;
	Glcolor::TGLColor* FHazeColor;
	Glcolor::TGLColor* FSkyColor;
	Glcolor::TGLColor* FNightColor;
	Glcolor::TGLColor* FDeepColor;
	int FSlices;
	int FStacks;
	TEarthSkydomeOptions FExtendedOptions;
	bool FMorning;
	
protected:
	virtual void __fastcall Loaded(void);
	void __fastcall SetSunElevation(const float val);
	void __fastcall SetTurbidity(const float val);
	void __fastcall SetSunZenithColor(Glcolor::TGLColor* const val);
	void __fastcall SetSunDawnColor(Glcolor::TGLColor* const val);
	void __fastcall SetHazeColor(Glcolor::TGLColor* const val);
	void __fastcall SetSkyColor(Glcolor::TGLColor* const val);
	void __fastcall SetNightColor(Glcolor::TGLColor* const val);
	void __fastcall SetDeepColor(Glcolor::TGLColor* const val);
	void __fastcall SetSlices(const int val);
	void __fastcall SetStacks(const int val);
	void __fastcall OnColorChanged(System::TObject* Sender);
	void __fastcall PreCalculate(void);
	void __fastcall RenderDome(void);
	Vectortypes::TVector4f __fastcall CalculateColor(const float theta, const float cosGamma);
	
public:
	__fastcall virtual TGLEarthSkyDome(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TGLEarthSkyDome(void);
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	virtual void __fastcall BuildList(Glrendercontextinfo::TRenderContextInfo &rci);
	void __fastcall SetSunAtTime(float HH, float MM);
	
__published:
	__property float SunElevation = {read=FSunElevation, write=SetSunElevation};
	__property float Turbidity = {read=FTurbidity, write=SetTurbidity};
	__property Glcolor::TGLColor* SunZenithColor = {read=FSunZenithColor, write=SetSunZenithColor};
	__property Glcolor::TGLColor* SunDawnColor = {read=FSunDawnColor, write=SetSunDawnColor};
	__property Glcolor::TGLColor* HazeColor = {read=FHazeColor, write=SetHazeColor};
	__property Glcolor::TGLColor* SkyColor = {read=FSkyColor, write=SetSkyColor};
	__property Glcolor::TGLColor* NightColor = {read=FNightColor, write=SetNightColor};
	__property Glcolor::TGLColor* DeepColor = {read=FDeepColor, write=SetDeepColor};
	__property TEarthSkydomeOptions ExtendedOptions = {read=FExtendedOptions, write=FExtendedOptions, nodefault};
	__property int Slices = {read=FSlices, write=SetSlices, default=24};
	__property int Stacks = {read=FStacks, write=SetStacks, default=48};
public:
	/* TGLBaseSceneObject.CreateAsChild */ inline __fastcall TGLEarthSkyDome(Glscene::TGLBaseSceneObject* aParentOwner) : TGLSkyDome(aParentOwner) { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Glskydome */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLSKYDOME)
using namespace Glskydome;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// GlskydomeHPP
