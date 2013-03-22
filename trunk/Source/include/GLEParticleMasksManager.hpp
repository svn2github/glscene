// CodeGear C++Builder
// Copyright (c) 1995, 2012 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLEParticleMasksManager.pas' rev: 24.00 (Win32)

#ifndef GleparticlemasksmanagerHPP
#define GleparticlemasksmanagerHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <System.SysUtils.hpp>	// Pascal unit
#include <System.Classes.hpp>	// Pascal unit
#include <Vcl.Graphics.hpp>	// Pascal unit
#include <GLTexture.hpp>	// Pascal unit
#include <GLMaterial.hpp>	// Pascal unit
#include <GLScene.hpp>	// Pascal unit
#include <VectorGeometry.hpp>	// Pascal unit
#include <VectorTypes.hpp>	// Pascal unit
#include <GLParticleFX.hpp>	// Pascal unit
#include <GLCrossPlatform.hpp>	// Pascal unit
#include <GLCoordinates.hpp>	// Pascal unit
#include <System.UITypes.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Gleparticlemasksmanager
{
//-- type declarations -------------------------------------------------------
enum TGLEProjectedParticleMask : unsigned char { pptXMask, pptYMask, pptZMask };

class DELPHICLASS TGLEParticleMask;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLEParticleMask : public System::Classes::TCollectionItem
{
	typedef System::Classes::TCollectionItem inherited;
	
private:
	System::UnicodeString FName;
	Glcoordinates::TGLCoordinates3* FScale;
	Glcoordinates::TGLCoordinates3* FPosition;
	System::UnicodeString FYMask;
	System::UnicodeString FZMask;
	System::UnicodeString FXMask;
	Glmaterial::TGLMaterialLibrary* FMaterialLibrary;
	System::Uitypes::TColor FBackgroundColor;
	System::Uitypes::TColor FMaskColor;
	int FMaxX;
	int FMaxY;
	int FMaxZ;
	int FMinX;
	int FMinY;
	int FMinZ;
	int IXW;
	int IXH;
	int IYW;
	int IYH;
	int IZW;
	int IZH;
	int LX;
	int LY;
	int LZ;
	int MX;
	int MY;
	bool BogusMask;
	bool BogusMaskX;
	bool BogusMaskY;
	bool BogusMaskZ;
	float FRollAngle;
	float FPitchAngle;
	float FTurnAngle;
	void __fastcall SetName(const System::UnicodeString Value);
	void __fastcall SetXMask(const System::UnicodeString Value);
	void __fastcall SetYMask(const System::UnicodeString Value);
	void __fastcall SetZMask(const System::UnicodeString Value);
	void __fastcall SetMaterialLibrary(Glmaterial::TGLMaterialLibrary* const Value);
	Vcl::Graphics::TBitmap* __fastcall XCan(void);
	Vcl::Graphics::TBitmap* __fastcall YCan(void);
	Vcl::Graphics::TBitmap* __fastcall ZCan(void);
	Glmaterial::TGLAbstractMaterialLibrary* __fastcall GetMaterialLibrary(void);
	HRESULT __stdcall QueryInterface(const GUID &IID, /* out */ void *Obj);
	int __stdcall _AddRef(void);
	int __stdcall _Release(void);
	
protected:
	virtual System::UnicodeString __fastcall GetDisplayName(void);
	
public:
	__fastcall virtual TGLEParticleMask(System::Classes::TCollection* Collection);
	__fastcall virtual ~TGLEParticleMask(void);
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	void __fastcall UpdateExtents(void);
	void __fastcall Roll(float Angle);
	void __fastcall Turn(float Angle);
	void __fastcall Pitch(float Angle);
	void __fastcall GenerateMaskFromProjection(TGLEProjectedParticleMask FromMask, TGLEProjectedParticleMask ToMask, int Depth);
	
__published:
	__property Glcoordinates::TGLCoordinates3* Scale = {read=FScale, write=FScale};
	__property Glcoordinates::TGLCoordinates3* Position = {read=FPosition, write=FPosition};
	__property System::UnicodeString Name = {read=FName, write=SetName};
	__property Glmaterial::TGLMaterialLibrary* MaterialLibrary = {read=FMaterialLibrary, write=SetMaterialLibrary};
	__property System::UnicodeString XMask = {read=FXMask, write=SetXMask};
	__property System::UnicodeString YMask = {read=FYMask, write=SetYMask};
	__property System::UnicodeString ZMask = {read=FZMask, write=SetZMask};
	__property System::Uitypes::TColor BackgroundColor = {read=FBackgroundColor, write=FBackgroundColor, nodefault};
	__property System::Uitypes::TColor MaskColor = {read=FMaskColor, write=FMaskColor, nodefault};
	__property float RollAngle = {read=FRollAngle, write=FRollAngle};
	__property float PitchAngle = {read=FPitchAngle, write=FPitchAngle};
	__property float TurnAngle = {read=FTurnAngle, write=FTurnAngle};
private:
	void *__IGLMaterialLibrarySupported;	/* Glmaterial::IGLMaterialLibrarySupported */
	
public:
	#if defined(MANAGED_INTERFACE_OPERATORS)
	// {8E442AF9-D212-4A5E-8A88-92F798BABFD1}
	operator Glmaterial::_di_IGLMaterialLibrarySupported()
	{
		Glmaterial::_di_IGLMaterialLibrarySupported intf;
		GetInterface(intf);
		return intf;
	}
	#else
	operator Glmaterial::IGLMaterialLibrarySupported*(void) { return (Glmaterial::IGLMaterialLibrarySupported*)&__IGLMaterialLibrarySupported; }
	#endif
	
};

#pragma pack(pop)

class DELPHICLASS TGLEParticleMasks;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLEParticleMasks : public System::Classes::TCollection
{
	typedef System::Classes::TCollection inherited;
	
public:
	TGLEParticleMask* operator[](int Index) { return Items[Index]; }
	
protected:
	System::Classes::TComponent* Owner;
	DYNAMIC System::Classes::TPersistent* __fastcall GetOwner(void);
	void __fastcall SetItems(int Index, TGLEParticleMask* const Val);
	TGLEParticleMask* __fastcall GetItems(int Index);
	
public:
	HIDESBASE TGLEParticleMask* __fastcall Add(void);
	__fastcall TGLEParticleMasks(System::Classes::TComponent* AOwner);
	__property TGLEParticleMask* Items[int Index] = {read=GetItems, write=SetItems/*, default*/};
public:
	/* TCollection.Destroy */ inline __fastcall virtual ~TGLEParticleMasks(void) { }
	
};

#pragma pack(pop)

class DELPHICLASS TGLEParticleMasksManager;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLEParticleMasksManager : public System::Classes::TComponent
{
	typedef System::Classes::TComponent inherited;
	
private:
	TGLEParticleMasks* FParticleMasks;
	
protected:
	void __fastcall ApplyOrthoGraphic(Vectortypes::TVector3f &Vec, TGLEParticleMask* Mask);
	void __fastcall ApplyRotation(Vectortypes::TVector3f &Vec, TGLEParticleMask* Mask);
	void __fastcall ApplyRotationTarget(Vectortypes::TVector3f &Vec, TGLEParticleMask* Mask, Glscene::TGLBaseSceneObject* TargetObject);
	void __fastcall ApplyScaleAndPosition(Vectortypes::TVector3f &Vec, TGLEParticleMask* Mask);
	void __fastcall ApplyScaleAndPositionTarget(Vectortypes::TVector3f &Vec, TGLEParticleMask* Mask, Glscene::TGLBaseSceneObject* TargetObject);
	void __fastcall FindParticlePosition(Vectortypes::TVector3f &Vec, TGLEParticleMask* Mask);
	
public:
	__fastcall virtual TGLEParticleMasksManager(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TGLEParticleMasksManager(void);
	Vectortypes::TVector3f __fastcall CreateParticlePositionFromMask(System::UnicodeString MaskName);
	Vectortypes::TVector3f __fastcall TargetParticlePositionFromMask(Glscene::TGLBaseSceneObject* TargetObject, System::UnicodeString MaskName);
	void __fastcall SetParticlePositionFromMask(Glparticlefx::TGLParticle* Particle, System::UnicodeString MaskName);
	void __fastcall SetParticlePositionFromMaskTarget(Glparticlefx::TGLParticle* Particle, System::UnicodeString MaskName, Glscene::TGLBaseSceneObject* TargetObject);
	TGLEParticleMask* __fastcall ParticleMaskByName(System::UnicodeString MaskName);
	
__published:
	__property TGLEParticleMasks* ParticleMasks = {read=FParticleMasks, write=FParticleMasks};
};

#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------
}	/* namespace Gleparticlemasksmanager */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLEPARTICLEMASKSMANAGER)
using namespace Gleparticlemasksmanager;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// GleparticlemasksmanagerHPP
