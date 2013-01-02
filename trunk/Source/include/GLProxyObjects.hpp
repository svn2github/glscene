// CodeGear C++Builder
// Copyright (c) 1995, 2012 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLProxyObjects.pas' rev: 24.00 (Win32)

#ifndef GlproxyobjectsHPP
#define GlproxyobjectsHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <System.Classes.hpp>	// Pascal unit
#include <System.SysUtils.hpp>	// Pascal unit
#include <GLScene.hpp>	// Pascal unit
#include <VectorGeometry.hpp>	// Pascal unit
#include <GLTexture.hpp>	// Pascal unit
#include <GLVectorFileObjects.hpp>	// Pascal unit
#include <GLStrings.hpp>	// Pascal unit
#include <GLRenderContextInfo.hpp>	// Pascal unit
#include <BaseClasses.hpp>	// Pascal unit
#include <GLMaterial.hpp>	// Pascal unit
#include <VectorTypes.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Glproxyobjects
{
//-- type declarations -------------------------------------------------------
class DELPHICLASS EGLProxyException;
#pragma pack(push,4)
class PASCALIMPLEMENTATION EGLProxyException : public System::Sysutils::Exception
{
	typedef System::Sysutils::Exception inherited;
	
public:
	/* Exception.Create */ inline __fastcall EGLProxyException(const System::UnicodeString Msg) : System::Sysutils::Exception(Msg) { }
	/* Exception.CreateFmt */ inline __fastcall EGLProxyException(const System::UnicodeString Msg, System::TVarRec const *Args, const int Args_Size) : System::Sysutils::Exception(Msg, Args, Args_Size) { }
	/* Exception.CreateRes */ inline __fastcall EGLProxyException(NativeUInt Ident)/* overload */ : System::Sysutils::Exception(Ident) { }
	/* Exception.CreateRes */ inline __fastcall EGLProxyException(System::PResStringRec ResStringRec)/* overload */ : System::Sysutils::Exception(ResStringRec) { }
	/* Exception.CreateResFmt */ inline __fastcall EGLProxyException(NativeUInt Ident, System::TVarRec const *Args, const int Args_Size)/* overload */ : System::Sysutils::Exception(Ident, Args, Args_Size) { }
	/* Exception.CreateResFmt */ inline __fastcall EGLProxyException(System::PResStringRec ResStringRec, System::TVarRec const *Args, const int Args_Size)/* overload */ : System::Sysutils::Exception(ResStringRec, Args, Args_Size) { }
	/* Exception.CreateHelp */ inline __fastcall EGLProxyException(const System::UnicodeString Msg, int AHelpContext) : System::Sysutils::Exception(Msg, AHelpContext) { }
	/* Exception.CreateFmtHelp */ inline __fastcall EGLProxyException(const System::UnicodeString Msg, System::TVarRec const *Args, const int Args_Size, int AHelpContext) : System::Sysutils::Exception(Msg, Args, Args_Size, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall EGLProxyException(NativeUInt Ident, int AHelpContext)/* overload */ : System::Sysutils::Exception(Ident, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall EGLProxyException(System::PResStringRec ResStringRec, int AHelpContext)/* overload */ : System::Sysutils::Exception(ResStringRec, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall EGLProxyException(System::PResStringRec ResStringRec, System::TVarRec const *Args, const int Args_Size, int AHelpContext)/* overload */ : System::Sysutils::Exception(ResStringRec, Args, Args_Size, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall EGLProxyException(NativeUInt Ident, System::TVarRec const *Args, const int Args_Size, int AHelpContext)/* overload */ : System::Sysutils::Exception(Ident, Args, Args_Size, AHelpContext) { }
	/* Exception.Destroy */ inline __fastcall virtual ~EGLProxyException(void) { }
	
};

#pragma pack(pop)

class DELPHICLASS TGLColorProxy;
class PASCALIMPLEMENTATION TGLColorProxy : public Glscene::TGLProxyObject
{
	typedef Glscene::TGLProxyObject inherited;
	
private:
	Glmaterial::TGLFaceProperties* FFrontColor;
	Glscene::TGLCustomSceneObject* __fastcall GetMasterMaterialObject(void);
	void __fastcall SetMasterMaterialObject(Glscene::TGLCustomSceneObject* const Value);
	void __fastcall SetFrontColor(Glmaterial::TGLFaceProperties* AValue);
	
public:
	__fastcall virtual TGLColorProxy(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TGLColorProxy(void);
	virtual void __fastcall DoRender(Glrendercontextinfo::TRenderContextInfo &ARci, bool ARenderSelf, bool ARenderChildren);
	
__published:
	__property Glmaterial::TGLFaceProperties* FrontColor = {read=FFrontColor, write=SetFrontColor};
	__property Glscene::TGLCustomSceneObject* MasterObject = {read=GetMasterMaterialObject, write=SetMasterMaterialObject};
public:
	/* TGLBaseSceneObject.CreateAsChild */ inline __fastcall TGLColorProxy(Glscene::TGLBaseSceneObject* aParentOwner) : Glscene::TGLProxyObject(aParentOwner) { }
	
};


class DELPHICLASS TGLMaterialProxy;
class PASCALIMPLEMENTATION TGLMaterialProxy : public Glscene::TGLProxyObject
{
	typedef Glscene::TGLProxyObject inherited;
	
private:
	System::UnicodeString FTempLibMaterialName;
	Glmaterial::TGLLibMaterial* FMasterLibMaterial;
	Glmaterial::TGLMaterialLibrary* FMaterialLibrary;
	void __fastcall SetMaterialLibrary(Glmaterial::TGLMaterialLibrary* const Value);
	System::UnicodeString __fastcall GetMasterLibMaterialName(void);
	void __fastcall SetMasterLibMaterialName(const System::UnicodeString Value);
	Glscene::TGLCustomSceneObject* __fastcall GetMasterMaterialObject(void);
	void __fastcall SetMasterMaterialObject(Glscene::TGLCustomSceneObject* const Value);
	Glmaterial::TGLAbstractMaterialLibrary* __fastcall GetMaterialLibrary(void);
	
public:
	__fastcall virtual TGLMaterialProxy(System::Classes::TComponent* AOwner);
	virtual void __fastcall Notification(System::Classes::TComponent* AComponent, System::Classes::TOperation Operation);
	__fastcall virtual ~TGLMaterialProxy(void);
	virtual void __fastcall DoRender(Glrendercontextinfo::TRenderContextInfo &ARci, bool ARenderSelf, bool ARenderChildren);
	__property Glmaterial::TGLLibMaterial* MasterLibMaterial = {read=FMasterLibMaterial, write=FMasterLibMaterial, stored=false};
	
__published:
	__property Glmaterial::TGLMaterialLibrary* MaterialLibrary = {read=FMaterialLibrary, write=SetMaterialLibrary};
	__property System::UnicodeString MasterLibMaterialName = {read=GetMasterLibMaterialName, write=SetMasterLibMaterialName};
	__property Glscene::TGLCustomSceneObject* MasterObject = {read=GetMasterMaterialObject, write=SetMasterMaterialObject};
public:
	/* TGLBaseSceneObject.CreateAsChild */ inline __fastcall TGLMaterialProxy(Glscene::TGLBaseSceneObject* aParentOwner) : Glscene::TGLProxyObject(aParentOwner) { }
	
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


class DELPHICLASS TGLFreeFormProxy;
class PASCALIMPLEMENTATION TGLFreeFormProxy : public Glscene::TGLProxyObject
{
	typedef Glscene::TGLProxyObject inherited;
	
private:
	Glvectorfileobjects::TGLFreeForm* __fastcall GetMasterFreeFormObject(void);
	void __fastcall SetMasterFreeFormObject(Glvectorfileobjects::TGLFreeForm* const Value);
	
public:
	bool __fastcall OctreeRayCastIntersect(const Vectortypes::TVector4f &rayStart, const Vectortypes::TVector4f &rayVector, Vectorgeometry::PVector intersectPoint = (Vectorgeometry::PVector)(0x0), Vectorgeometry::PVector intersectNormal = (Vectorgeometry::PVector)(0x0));
	bool __fastcall OctreeSphereSweepIntersect(const Vectortypes::TVector4f &rayStart, const Vectortypes::TVector4f &rayVector, const float velocity, const float radius, const float modelscale, Vectorgeometry::PVector intersectPoint = (Vectorgeometry::PVector)(0x0), Vectorgeometry::PVector intersectNormal = (Vectorgeometry::PVector)(0x0));
	
__published:
	__property Glvectorfileobjects::TGLFreeForm* MasterObject = {read=GetMasterFreeFormObject, write=SetMasterFreeFormObject};
public:
	/* TGLProxyObject.Create */ inline __fastcall virtual TGLFreeFormProxy(System::Classes::TComponent* AOwner) : Glscene::TGLProxyObject(AOwner) { }
	/* TGLProxyObject.Destroy */ inline __fastcall virtual ~TGLFreeFormProxy(void) { }
	
public:
	/* TGLBaseSceneObject.CreateAsChild */ inline __fastcall TGLFreeFormProxy(Glscene::TGLBaseSceneObject* aParentOwner) : Glscene::TGLProxyObject(aParentOwner) { }
	
};


class DELPHICLASS TBoneMatrixObj;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TBoneMatrixObj : public System::TObject
{
	typedef System::TObject inherited;
	
public:
	Vectortypes::TMatrix4f Matrix;
	System::UnicodeString BoneName;
	int BoneIndex;
public:
	/* TObject.Create */ inline __fastcall TBoneMatrixObj(void) : System::TObject() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TBoneMatrixObj(void) { }
	
};

#pragma pack(pop)

enum TGLActorProxyAnimationMode : unsigned char { pamInherited, pamNone, pamPlayOnce };

class DELPHICLASS TGLActorProxy;
class PASCALIMPLEMENTATION TGLActorProxy : public Glscene::TGLProxyObject
{
	typedef Glscene::TGLProxyObject inherited;
	
private:
	int FCurrentFrame;
	int FStartFrame;
	int FEndFrame;
	int FLastFrame;
	float FCurrentFrameDelta;
	Baseclasses::TProgressTimes FCurrentTime;
	System::UnicodeString FAnimation;
	System::UnicodeString FTempLibMaterialName;
	Glmaterial::TGLLibMaterial* FMasterLibMaterial;
	Glmaterial::TGLMaterialLibrary* FMaterialLibrary;
	System::Classes::TStringList* FBonesMatrices;
	bool FStoreBonesMatrix;
	System::Classes::TStrings* FStoredBoneNames;
	Baseclasses::TGLProgressEvent FOnBeforeRender;
	TGLActorProxyAnimationMode FAnimationMode;
	void __fastcall SetAnimation(const System::UnicodeString Value);
	void __fastcall SetMasterActorObject(Glvectorfileobjects::TGLActor* const Value);
	Glvectorfileobjects::TGLActor* __fastcall GetMasterActorObject(void);
	System::UnicodeString __fastcall GetLibMaterialName(void);
	void __fastcall SetLibMaterialName(const System::UnicodeString Value);
	void __fastcall SetMaterialLibrary(Glmaterial::TGLMaterialLibrary* const Value);
	Glmaterial::TGLAbstractMaterialLibrary* __fastcall GetMaterialLibrary(void);
	void __fastcall SetStoreBonesMatrix(const bool Value);
	void __fastcall SetStoredBoneNames(System::Classes::TStrings* const Value);
	void __fastcall SetOnBeforeRender(const Baseclasses::TGLProgressEvent Value);
	
protected:
	void __fastcall DoStoreBonesMatrices(void);
	
public:
	__fastcall virtual TGLActorProxy(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TGLActorProxy(void);
	virtual void __fastcall Notification(System::Classes::TComponent* AComponent, System::Classes::TOperation Operation);
	virtual void __fastcall DoRender(Glrendercontextinfo::TRenderContextInfo &ARci, bool ARenderSelf, bool ARenderChildren);
	virtual void __fastcall DoProgress(const Baseclasses::TProgressTimes &progressTime);
	__property int CurrentFrame = {read=FCurrentFrame, nodefault};
	__property int StartFrame = {read=FStartFrame, nodefault};
	__property int EndFrame = {read=FEndFrame, nodefault};
	__property float CurrentFrameDelta = {read=FCurrentFrameDelta};
	__property Baseclasses::TProgressTimes CurrentTime = {read=FCurrentTime};
	Vectortypes::TMatrix4f __fastcall BoneMatrix(int BoneIndex)/* overload */;
	Vectortypes::TMatrix4f __fastcall BoneMatrix(System::UnicodeString BoneName)/* overload */;
	void __fastcall BoneMatricesClear(void);
	virtual bool __fastcall RayCastIntersect(const Vectortypes::TVector4f &rayStart, const Vectortypes::TVector4f &rayVector, Vectorgeometry::PVector intersectPoint = (Vectorgeometry::PVector)(0x0), Vectorgeometry::PVector intersectNormal = (Vectorgeometry::PVector)(0x0));
	bool __fastcall RayCastIntersectEx(Glvectorfileobjects::TGLActor* RefActor, const Vectortypes::TVector4f &rayStart, const Vectortypes::TVector4f &rayVector, Vectorgeometry::PVector intersectPoint = (Vectorgeometry::PVector)(0x0), Vectorgeometry::PVector intersectNormal = (Vectorgeometry::PVector)(0x0))/* overload */;
	
__published:
	__property TGLActorProxyAnimationMode AnimationMode = {read=FAnimationMode, write=FAnimationMode, default=0};
	__property System::UnicodeString Animation = {read=FAnimation, write=SetAnimation};
	__property Glvectorfileobjects::TGLActor* MasterObject = {read=GetMasterActorObject, write=SetMasterActorObject};
	__property ProxyOptions = {default=3};
	__property Glmaterial::TGLMaterialLibrary* MaterialLibrary = {read=FMaterialLibrary, write=SetMaterialLibrary};
	__property System::UnicodeString LibMaterialName = {read=GetLibMaterialName, write=SetLibMaterialName};
	__property bool StoreBonesMatrix = {read=FStoreBonesMatrix, write=SetStoreBonesMatrix, nodefault};
	__property System::Classes::TStrings* StoredBoneNames = {read=FStoredBoneNames, write=SetStoredBoneNames};
	__property Baseclasses::TGLProgressEvent OnBeforeRender = {read=FOnBeforeRender, write=SetOnBeforeRender};
public:
	/* TGLBaseSceneObject.CreateAsChild */ inline __fastcall TGLActorProxy(Glscene::TGLBaseSceneObject* aParentOwner) : Glscene::TGLProxyObject(aParentOwner) { }
	
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


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Glproxyobjects */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLPROXYOBJECTS)
using namespace Glproxyobjects;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// GlproxyobjectsHPP
