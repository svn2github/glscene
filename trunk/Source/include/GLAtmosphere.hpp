// CodeGear C++Builder
// Copyright (c) 1995, 2012 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLAtmosphere.pas' rev: 24.00 (Win32)

#ifndef GlatmosphereHPP
#define GlatmosphereHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <System.SysUtils.hpp>	// Pascal unit
#include <System.Classes.hpp>	// Pascal unit
#include <GLScene.hpp>	// Pascal unit
#include <GLObjects.hpp>	// Pascal unit
#include <GLCadencer.hpp>	// Pascal unit
#include <OpenGLTokens.hpp>	// Pascal unit
#include <VectorGeometry.hpp>	// Pascal unit
#include <GLContext.hpp>	// Pascal unit
#include <GLStrings.hpp>	// Pascal unit
#include <GLColor.hpp>	// Pascal unit
#include <GLRenderContextInfo.hpp>	// Pascal unit
#include <GLState.hpp>	// Pascal unit
#include <GLCrossPlatform.hpp>	// Pascal unit
#include <VectorTypes.hpp>	// Pascal unit
#include <GLCoordinates.hpp>	// Pascal unit
#include <BaseClasses.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Glatmosphere
{
//-- type declarations -------------------------------------------------------
class DELPHICLASS EGLAtmosphereException;
#pragma pack(push,4)
class PASCALIMPLEMENTATION EGLAtmosphereException : public System::Sysutils::Exception
{
	typedef System::Sysutils::Exception inherited;
	
public:
	/* Exception.Create */ inline __fastcall EGLAtmosphereException(const System::UnicodeString Msg) : System::Sysutils::Exception(Msg) { }
	/* Exception.CreateFmt */ inline __fastcall EGLAtmosphereException(const System::UnicodeString Msg, System::TVarRec const *Args, const int Args_Size) : System::Sysutils::Exception(Msg, Args, Args_Size) { }
	/* Exception.CreateRes */ inline __fastcall EGLAtmosphereException(NativeUInt Ident)/* overload */ : System::Sysutils::Exception(Ident) { }
	/* Exception.CreateRes */ inline __fastcall EGLAtmosphereException(System::PResStringRec ResStringRec)/* overload */ : System::Sysutils::Exception(ResStringRec) { }
	/* Exception.CreateResFmt */ inline __fastcall EGLAtmosphereException(NativeUInt Ident, System::TVarRec const *Args, const int Args_Size)/* overload */ : System::Sysutils::Exception(Ident, Args, Args_Size) { }
	/* Exception.CreateResFmt */ inline __fastcall EGLAtmosphereException(System::PResStringRec ResStringRec, System::TVarRec const *Args, const int Args_Size)/* overload */ : System::Sysutils::Exception(ResStringRec, Args, Args_Size) { }
	/* Exception.CreateHelp */ inline __fastcall EGLAtmosphereException(const System::UnicodeString Msg, int AHelpContext) : System::Sysutils::Exception(Msg, AHelpContext) { }
	/* Exception.CreateFmtHelp */ inline __fastcall EGLAtmosphereException(const System::UnicodeString Msg, System::TVarRec const *Args, const int Args_Size, int AHelpContext) : System::Sysutils::Exception(Msg, Args, Args_Size, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall EGLAtmosphereException(NativeUInt Ident, int AHelpContext)/* overload */ : System::Sysutils::Exception(Ident, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall EGLAtmosphereException(System::PResStringRec ResStringRec, int AHelpContext)/* overload */ : System::Sysutils::Exception(ResStringRec, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall EGLAtmosphereException(System::PResStringRec ResStringRec, System::TVarRec const *Args, const int Args_Size, int AHelpContext)/* overload */ : System::Sysutils::Exception(ResStringRec, Args, Args_Size, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall EGLAtmosphereException(NativeUInt Ident, System::TVarRec const *Args, const int Args_Size, int AHelpContext)/* overload */ : System::Sysutils::Exception(Ident, Args, Args_Size, AHelpContext) { }
	/* Exception.Destroy */ inline __fastcall virtual ~EGLAtmosphereException(void) { }
	
};

#pragma pack(pop)

enum TGLAtmosphereBlendingMode : unsigned char { abmOneMinusDstColor, abmOneMinusSrcAlpha };

class DELPHICLASS TGLCustomAtmosphere;
class PASCALIMPLEMENTATION TGLCustomAtmosphere : public Glscene::TGLBaseSceneObject
{
	typedef Glscene::TGLBaseSceneObject inherited;
	
private:
	typedef System::DynamicArray<float> _TGLCustomAtmosphere__1;
	
	
private:
	_TGLCustomAtmosphere__1 cosCache;
	_TGLCustomAtmosphere__1 sinCache;
	Vectorgeometry::TVectorArray *pVertex;
	Vectorgeometry::TVectorArray *pColor;
	int FSlices;
	TGLAtmosphereBlendingMode FBlendingMode;
	float FPlanetRadius;
	float FAtmosphereRadius;
	float FOpacity;
	Glcolor::TGLColor* FLowAtmColor;
	Glcolor::TGLColor* FHighAtmColor;
	Glscene::TGLBaseSceneObject* FSun;
	void __fastcall SetSun(Glscene::TGLBaseSceneObject* const Value);
	void __fastcall SetAtmosphereRadius(const float Value);
	void __fastcall SetPlanetRadius(const float Value);
	void __fastcall EnableGLBlendingMode(Glstate::TGLStateCache* StateCache);
	bool __fastcall StoreAtmosphereRadius(void);
	bool __fastcall StoreOpacity(void);
	bool __fastcall StorePlanetRadius(void);
	void __fastcall SetSlices(const int Value);
	void __fastcall SetLowAtmColor(Glcolor::TGLColor* const AValue);
	void __fastcall SetHighAtmColor(Glcolor::TGLColor* const AValue);
	bool __fastcall StoreLowAtmColor(void);
	bool __fastcall StoreHighAtmColor(void);
	
protected:
	virtual void __fastcall Notification(System::Classes::TComponent* AComponent, System::Classes::TOperation Operation);
	
public:
	__property Glscene::TGLBaseSceneObject* Sun = {read=FSun, write=SetSun};
	__property int Slices = {read=FSlices, write=SetSlices, default=60};
	__property float Opacity = {read=FOpacity, write=FOpacity, stored=StoreOpacity};
	__property float AtmosphereRadius = {read=FAtmosphereRadius, write=SetAtmosphereRadius, stored=StoreAtmosphereRadius};
	__property float PlanetRadius = {read=FPlanetRadius, write=SetPlanetRadius, stored=StorePlanetRadius};
	__property Glcolor::TGLColor* LowAtmColor = {read=FLowAtmColor, write=SetLowAtmColor, stored=StoreLowAtmColor};
	__property Glcolor::TGLColor* HighAtmColor = {read=FHighAtmColor, write=SetHighAtmColor, stored=StoreHighAtmColor};
	__property TGLAtmosphereBlendingMode BlendingMode = {read=FBlendingMode, write=FBlendingMode, default=1};
	void __fastcall SetOptimalAtmosphere(const float ARadius);
	void __fastcall SetOptimalAtmosphere2(const float ARadius);
	void __fastcall TogleBlendingMode(void);
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	__fastcall virtual TGLCustomAtmosphere(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TGLCustomAtmosphere(void);
	virtual void __fastcall DoRender(Glrendercontextinfo::TRenderContextInfo &rci, bool renderSelf, bool renderChildren);
	virtual Vectortypes::TVector4f __fastcall AxisAlignedDimensionsUnscaled(void);
public:
	/* TGLBaseSceneObject.CreateAsChild */ inline __fastcall TGLCustomAtmosphere(Glscene::TGLBaseSceneObject* aParentOwner) : Glscene::TGLBaseSceneObject(aParentOwner) { }
	
};


class DELPHICLASS TGLAtmosphere;
class PASCALIMPLEMENTATION TGLAtmosphere : public TGLCustomAtmosphere
{
	typedef TGLCustomAtmosphere inherited;
	
__published:
	__property Sun;
	__property Slices = {default=60};
	__property Opacity = {default=0};
	__property AtmosphereRadius = {default=0};
	__property PlanetRadius = {default=0};
	__property LowAtmColor;
	__property HighAtmColor;
	__property BlendingMode = {default=1};
	__property Position;
	__property ObjectsSorting = {default=0};
	__property ShowAxes = {default=0};
	__property Visible = {default=1};
	__property OnProgress;
	__property Behaviours;
	__property Effects;
public:
	/* TGLCustomAtmosphere.Create */ inline __fastcall virtual TGLAtmosphere(System::Classes::TComponent* AOwner) : TGLCustomAtmosphere(AOwner) { }
	/* TGLCustomAtmosphere.Destroy */ inline __fastcall virtual ~TGLAtmosphere(void) { }
	
public:
	/* TGLBaseSceneObject.CreateAsChild */ inline __fastcall TGLAtmosphere(Glscene::TGLBaseSceneObject* aParentOwner) : TGLCustomAtmosphere(aParentOwner) { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Glatmosphere */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLATMOSPHERE)
using namespace Glatmosphere;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// GlatmosphereHPP
