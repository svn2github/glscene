// CodeGear C++Builder
// Copyright (c) 1995, 2012 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLFile3DS.pas' rev: 24.00 (Win32)

#ifndef Glfile3dsHPP
#define Glfile3dsHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <System.Classes.hpp>	// Pascal unit
#include <System.SysUtils.hpp>	// Pascal unit
#include <System.Math.hpp>	// Pascal unit
#include <GLScene.hpp>	// Pascal unit
#include <GLObjects.hpp>	// Pascal unit
#include <GLVectorFileObjects.hpp>	// Pascal unit
#include <GLTexture.hpp>	// Pascal unit
#include <ApplicationFileIO.hpp>	// Pascal unit
#include <VectorGeometry.hpp>	// Pascal unit
#include <File3DS.hpp>	// Pascal unit
#include <Types3DS.hpp>	// Pascal unit
#include <OpenGLTokens.hpp>	// Pascal unit
#include <GLContext.hpp>	// Pascal unit
#include <PersistentClasses.hpp>	// Pascal unit
#include <GLStrings.hpp>	// Pascal unit
#include <GLFile3DSSceneObjects.hpp>	// Pascal unit
#include <GLCrossPlatform.hpp>	// Pascal unit
#include <VectorTypes.hpp>	// Pascal unit
#include <VectorLists.hpp>	// Pascal unit
#include <GLRenderContextInfo.hpp>	// Pascal unit
#include <GLMaterial.hpp>	// Pascal unit
#include <BaseClasses.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Glfile3ds
{
//-- type declarations -------------------------------------------------------
class DELPHICLASS EGLFile3DS;
#pragma pack(push,4)
class PASCALIMPLEMENTATION EGLFile3DS : public System::Sysutils::Exception
{
	typedef System::Sysutils::Exception inherited;
	
public:
	/* Exception.Create */ inline __fastcall EGLFile3DS(const System::UnicodeString Msg) : System::Sysutils::Exception(Msg) { }
	/* Exception.CreateFmt */ inline __fastcall EGLFile3DS(const System::UnicodeString Msg, System::TVarRec const *Args, const int Args_Size) : System::Sysutils::Exception(Msg, Args, Args_Size) { }
	/* Exception.CreateRes */ inline __fastcall EGLFile3DS(NativeUInt Ident)/* overload */ : System::Sysutils::Exception(Ident) { }
	/* Exception.CreateRes */ inline __fastcall EGLFile3DS(System::PResStringRec ResStringRec)/* overload */ : System::Sysutils::Exception(ResStringRec) { }
	/* Exception.CreateResFmt */ inline __fastcall EGLFile3DS(NativeUInt Ident, System::TVarRec const *Args, const int Args_Size)/* overload */ : System::Sysutils::Exception(Ident, Args, Args_Size) { }
	/* Exception.CreateResFmt */ inline __fastcall EGLFile3DS(System::PResStringRec ResStringRec, System::TVarRec const *Args, const int Args_Size)/* overload */ : System::Sysutils::Exception(ResStringRec, Args, Args_Size) { }
	/* Exception.CreateHelp */ inline __fastcall EGLFile3DS(const System::UnicodeString Msg, int AHelpContext) : System::Sysutils::Exception(Msg, AHelpContext) { }
	/* Exception.CreateFmtHelp */ inline __fastcall EGLFile3DS(const System::UnicodeString Msg, System::TVarRec const *Args, const int Args_Size, int AHelpContext) : System::Sysutils::Exception(Msg, Args, Args_Size, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall EGLFile3DS(NativeUInt Ident, int AHelpContext)/* overload */ : System::Sysutils::Exception(Ident, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall EGLFile3DS(System::PResStringRec ResStringRec, int AHelpContext)/* overload */ : System::Sysutils::Exception(ResStringRec, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall EGLFile3DS(System::PResStringRec ResStringRec, System::TVarRec const *Args, const int Args_Size, int AHelpContext)/* overload */ : System::Sysutils::Exception(ResStringRec, Args, Args_Size, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall EGLFile3DS(NativeUInt Ident, System::TVarRec const *Args, const int Args_Size, int AHelpContext)/* overload */ : System::Sysutils::Exception(Ident, Args, Args_Size, AHelpContext) { }
	/* Exception.Destroy */ inline __fastcall virtual ~EGLFile3DS(void) { }
	
};

#pragma pack(pop)

#pragma pack(push,1)
struct DECLSPEC_DRECORD TGLFile3DSAnimationData
{
public:
	Vectortypes::TMatrix4f ModelMatrix;
	Vectortypes::TVector4f Color;
	Vectortypes::TVector3f TargetPos;
	float SpotLightCutOff;
	float HotSpot;
	float Roll;
};
#pragma pack(pop)


class DELPHICLASS TGLFile3DSAnimationKeys;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLFile3DSAnimationKeys : public Persistentclasses::TPersistentObject
{
	typedef Persistentclasses::TPersistentObject inherited;
	
private:
	typedef System::DynamicArray<Types3ds::TKeyHeader3DS> _TGLFile3DSAnimationKeys__1;
	
	
private:
	int FNumKeys;
	_TGLFile3DSAnimationKeys__1 FKeys;
	void __fastcall InterpolateFrame(int &I, double &w, const double AFrame);
	
protected:
	float __fastcall InterpolateValue(float const *AValues, const int AValues_Size, const double AFrame)/* overload */;
	Vectortypes::TVector3f __fastcall InterpolateValue(Vectortypes::TVector3f const *AValues, const int AValues_Size, const double AFrame)/* overload */;
	Vectortypes::TMatrix4f __fastcall InterpolateValue(Types3ds::TKFRotKey3DS const *AValues, const int AValues_Size, const double AFrame)/* overload */;
	
public:
	virtual void __fastcall LoadData(const int ANumKeys, const Types3ds::PKeyHeaderList Keys, const void * AData);
	virtual void __fastcall Apply(TGLFile3DSAnimationData &DataTransf, const double AFrame) = 0 ;
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	DYNAMIC void __fastcall WriteToFiler(Persistentclasses::TVirtualWriter* Writer);
	DYNAMIC void __fastcall ReadFromFiler(Persistentclasses::TVirtualReader* Reader);
public:
	/* TPersistentObject.Create */ inline __fastcall virtual TGLFile3DSAnimationKeys(void) : Persistentclasses::TPersistentObject() { }
	/* TPersistentObject.CreateFromFiler */ inline __fastcall TGLFile3DSAnimationKeys(Persistentclasses::TVirtualReader* reader) : Persistentclasses::TPersistentObject(reader) { }
	/* TPersistentObject.Destroy */ inline __fastcall virtual ~TGLFile3DSAnimationKeys(void) { }
	
};

#pragma pack(pop)

class DELPHICLASS TGLFile3DSScaleAnimationKeys;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLFile3DSScaleAnimationKeys : public TGLFile3DSAnimationKeys
{
	typedef TGLFile3DSAnimationKeys inherited;
	
private:
	typedef System::DynamicArray<Vectortypes::TVector3f> _TGLFile3DSScaleAnimationKeys__1;
	
	
private:
	_TGLFile3DSScaleAnimationKeys__1 FScale;
	
public:
	virtual void __fastcall LoadData(const int ANumKeys, const Types3ds::PKeyHeaderList Keys, const void * AData);
	virtual void __fastcall Apply(TGLFile3DSAnimationData &DataTransf, const double AFrame);
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	DYNAMIC void __fastcall WriteToFiler(Persistentclasses::TVirtualWriter* Writer);
	DYNAMIC void __fastcall ReadFromFiler(Persistentclasses::TVirtualReader* Reader);
public:
	/* TPersistentObject.Create */ inline __fastcall virtual TGLFile3DSScaleAnimationKeys(void) : TGLFile3DSAnimationKeys() { }
	/* TPersistentObject.CreateFromFiler */ inline __fastcall TGLFile3DSScaleAnimationKeys(Persistentclasses::TVirtualReader* reader) : TGLFile3DSAnimationKeys(reader) { }
	/* TPersistentObject.Destroy */ inline __fastcall virtual ~TGLFile3DSScaleAnimationKeys(void) { }
	
};

#pragma pack(pop)

class DELPHICLASS TGLFile3DSRotationAnimationKeys;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLFile3DSRotationAnimationKeys : public TGLFile3DSAnimationKeys
{
	typedef TGLFile3DSAnimationKeys inherited;
	
private:
	typedef System::DynamicArray<Types3ds::TKFRotKey3DS> _TGLFile3DSRotationAnimationKeys__1;
	
	
private:
	_TGLFile3DSRotationAnimationKeys__1 FRot;
	
public:
	virtual void __fastcall LoadData(const int ANumKeys, const Types3ds::PKeyHeaderList Keys, const void * AData);
	virtual void __fastcall Apply(TGLFile3DSAnimationData &DataTransf, const double AFrame);
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	DYNAMIC void __fastcall WriteToFiler(Persistentclasses::TVirtualWriter* Writer);
	DYNAMIC void __fastcall ReadFromFiler(Persistentclasses::TVirtualReader* Reader);
public:
	/* TPersistentObject.Create */ inline __fastcall virtual TGLFile3DSRotationAnimationKeys(void) : TGLFile3DSAnimationKeys() { }
	/* TPersistentObject.CreateFromFiler */ inline __fastcall TGLFile3DSRotationAnimationKeys(Persistentclasses::TVirtualReader* reader) : TGLFile3DSAnimationKeys(reader) { }
	/* TPersistentObject.Destroy */ inline __fastcall virtual ~TGLFile3DSRotationAnimationKeys(void) { }
	
};

#pragma pack(pop)

class DELPHICLASS TGLFile3DSPositionAnimationKeys;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLFile3DSPositionAnimationKeys : public TGLFile3DSAnimationKeys
{
	typedef TGLFile3DSAnimationKeys inherited;
	
private:
	typedef System::DynamicArray<Vectortypes::TVector3f> _TGLFile3DSPositionAnimationKeys__1;
	
	
private:
	_TGLFile3DSPositionAnimationKeys__1 FPos;
	
public:
	virtual void __fastcall LoadData(const int ANumKeys, const Types3ds::PKeyHeaderList Keys, const void * AData);
	virtual void __fastcall Apply(TGLFile3DSAnimationData &DataTransf, const double AFrame);
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	DYNAMIC void __fastcall WriteToFiler(Persistentclasses::TVirtualWriter* Writer);
	DYNAMIC void __fastcall ReadFromFiler(Persistentclasses::TVirtualReader* Reader);
public:
	/* TPersistentObject.Create */ inline __fastcall virtual TGLFile3DSPositionAnimationKeys(void) : TGLFile3DSAnimationKeys() { }
	/* TPersistentObject.CreateFromFiler */ inline __fastcall TGLFile3DSPositionAnimationKeys(Persistentclasses::TVirtualReader* reader) : TGLFile3DSAnimationKeys(reader) { }
	/* TPersistentObject.Destroy */ inline __fastcall virtual ~TGLFile3DSPositionAnimationKeys(void) { }
	
};

#pragma pack(pop)

class DELPHICLASS TGLFile3DSColorAnimationKeys;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLFile3DSColorAnimationKeys : public TGLFile3DSAnimationKeys
{
	typedef TGLFile3DSAnimationKeys inherited;
	
private:
	typedef System::DynamicArray<Vectortypes::TVector3f> _TGLFile3DSColorAnimationKeys__1;
	
	
private:
	_TGLFile3DSColorAnimationKeys__1 FCol;
	
public:
	virtual void __fastcall LoadData(const int ANumKeys, const Types3ds::PKeyHeaderList Keys, const void * AData);
	virtual void __fastcall Apply(TGLFile3DSAnimationData &DataTransf, const double AFrame);
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	DYNAMIC void __fastcall WriteToFiler(Persistentclasses::TVirtualWriter* Writer);
	DYNAMIC void __fastcall ReadFromFiler(Persistentclasses::TVirtualReader* Reader);
public:
	/* TPersistentObject.Create */ inline __fastcall virtual TGLFile3DSColorAnimationKeys(void) : TGLFile3DSAnimationKeys() { }
	/* TPersistentObject.CreateFromFiler */ inline __fastcall TGLFile3DSColorAnimationKeys(Persistentclasses::TVirtualReader* reader) : TGLFile3DSAnimationKeys(reader) { }
	/* TPersistentObject.Destroy */ inline __fastcall virtual ~TGLFile3DSColorAnimationKeys(void) { }
	
};

#pragma pack(pop)

class DELPHICLASS TTGLFile3DSPositionAnimationKeys;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TTGLFile3DSPositionAnimationKeys : public TGLFile3DSAnimationKeys
{
	typedef TGLFile3DSAnimationKeys inherited;
	
private:
	typedef System::DynamicArray<Vectortypes::TVector3f> _TTGLFile3DSPositionAnimationKeys__1;
	
	
private:
	_TTGLFile3DSPositionAnimationKeys__1 FTPos;
	
public:
	virtual void __fastcall LoadData(const int ANumKeys, const Types3ds::PKeyHeaderList Keys, const void * AData);
	virtual void __fastcall Apply(TGLFile3DSAnimationData &DataTransf, const double AFrame);
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	DYNAMIC void __fastcall WriteToFiler(Persistentclasses::TVirtualWriter* Writer);
	DYNAMIC void __fastcall ReadFromFiler(Persistentclasses::TVirtualReader* Reader);
public:
	/* TPersistentObject.Create */ inline __fastcall virtual TTGLFile3DSPositionAnimationKeys(void) : TGLFile3DSAnimationKeys() { }
	/* TPersistentObject.CreateFromFiler */ inline __fastcall TTGLFile3DSPositionAnimationKeys(Persistentclasses::TVirtualReader* reader) : TGLFile3DSAnimationKeys(reader) { }
	/* TPersistentObject.Destroy */ inline __fastcall virtual ~TTGLFile3DSPositionAnimationKeys(void) { }
	
};

#pragma pack(pop)

class DELPHICLASS TGLFile3DSSpotLightCutOffAnimationKeys;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLFile3DSSpotLightCutOffAnimationKeys : public TGLFile3DSAnimationKeys
{
	typedef TGLFile3DSAnimationKeys inherited;
	
private:
	typedef System::DynamicArray<float> _TGLFile3DSSpotLightCutOffAnimationKeys__1;
	
	
private:
	_TGLFile3DSSpotLightCutOffAnimationKeys__1 FFall;
	
public:
	virtual void __fastcall LoadData(const int ANumKeys, const Types3ds::PKeyHeaderList Keys, const void * AData);
	virtual void __fastcall Apply(TGLFile3DSAnimationData &DataTransf, const double AFrame);
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	DYNAMIC void __fastcall WriteToFiler(Persistentclasses::TVirtualWriter* Writer);
	DYNAMIC void __fastcall ReadFromFiler(Persistentclasses::TVirtualReader* Reader);
public:
	/* TPersistentObject.Create */ inline __fastcall virtual TGLFile3DSSpotLightCutOffAnimationKeys(void) : TGLFile3DSAnimationKeys() { }
	/* TPersistentObject.CreateFromFiler */ inline __fastcall TGLFile3DSSpotLightCutOffAnimationKeys(Persistentclasses::TVirtualReader* reader) : TGLFile3DSAnimationKeys(reader) { }
	/* TPersistentObject.Destroy */ inline __fastcall virtual ~TGLFile3DSSpotLightCutOffAnimationKeys(void) { }
	
};

#pragma pack(pop)

class DELPHICLASS TGLFile3DSLightHotSpotAnimationKeys;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLFile3DSLightHotSpotAnimationKeys : public TGLFile3DSAnimationKeys
{
	typedef TGLFile3DSAnimationKeys inherited;
	
private:
	typedef System::DynamicArray<float> _TGLFile3DSLightHotSpotAnimationKeys__1;
	
	
private:
	_TGLFile3DSLightHotSpotAnimationKeys__1 FHot;
	
public:
	virtual void __fastcall LoadData(const int ANumKeys, const Types3ds::PKeyHeaderList Keys, const void * AData);
	virtual void __fastcall Apply(TGLFile3DSAnimationData &DataTransf, const double AFrame);
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	DYNAMIC void __fastcall WriteToFiler(Persistentclasses::TVirtualWriter* Writer);
	DYNAMIC void __fastcall ReadFromFiler(Persistentclasses::TVirtualReader* Reader);
public:
	/* TPersistentObject.Create */ inline __fastcall virtual TGLFile3DSLightHotSpotAnimationKeys(void) : TGLFile3DSAnimationKeys() { }
	/* TPersistentObject.CreateFromFiler */ inline __fastcall TGLFile3DSLightHotSpotAnimationKeys(Persistentclasses::TVirtualReader* reader) : TGLFile3DSAnimationKeys(reader) { }
	/* TPersistentObject.Destroy */ inline __fastcall virtual ~TGLFile3DSLightHotSpotAnimationKeys(void) { }
	
};

#pragma pack(pop)

class DELPHICLASS TGLFile3DSRollAnimationKeys;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLFile3DSRollAnimationKeys : public TGLFile3DSAnimationKeys
{
	typedef TGLFile3DSAnimationKeys inherited;
	
private:
	typedef System::DynamicArray<float> _TGLFile3DSRollAnimationKeys__1;
	
	
private:
	_TGLFile3DSRollAnimationKeys__1 FRoll;
	
public:
	virtual void __fastcall LoadData(const int ANumKeys, const Types3ds::PKeyHeaderList Keys, const void * AData);
	virtual void __fastcall Apply(TGLFile3DSAnimationData &DataTransf, const double AFrame);
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	DYNAMIC void __fastcall WriteToFiler(Persistentclasses::TVirtualWriter* Writer);
	DYNAMIC void __fastcall ReadFromFiler(Persistentclasses::TVirtualReader* Reader);
public:
	/* TPersistentObject.Create */ inline __fastcall virtual TGLFile3DSRollAnimationKeys(void) : TGLFile3DSAnimationKeys() { }
	/* TPersistentObject.CreateFromFiler */ inline __fastcall TGLFile3DSRollAnimationKeys(Persistentclasses::TVirtualReader* reader) : TGLFile3DSAnimationKeys(reader) { }
	/* TPersistentObject.Destroy */ inline __fastcall virtual ~TGLFile3DSRollAnimationKeys(void) { }
	
};

#pragma pack(pop)

class DELPHICLASS TGLFile3DSAnimationKeyList;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLFile3DSAnimationKeyList : public Persistentclasses::TPersistentObject
{
	typedef Persistentclasses::TPersistentObject inherited;
	
private:
	typedef System::DynamicArray<TGLFile3DSAnimationKeys*> _TGLFile3DSAnimationKeyList__1;
	
	
private:
	_TGLFile3DSAnimationKeyList__1 FAnimKeysList;
	
protected:
	void __fastcall ApplyAnimKeys(TGLFile3DSAnimationData &DataTransf, const double AFrame);
	
public:
	void __fastcall AddKeys(TGLFile3DSAnimationKeys* const AItem);
	void __fastcall ClearKeys(void);
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	DYNAMIC void __fastcall WriteToFiler(Persistentclasses::TVirtualWriter* Writer);
	DYNAMIC void __fastcall ReadFromFiler(Persistentclasses::TVirtualReader* Reader);
	__fastcall virtual ~TGLFile3DSAnimationKeyList(void);
public:
	/* TPersistentObject.Create */ inline __fastcall virtual TGLFile3DSAnimationKeyList(void) : Persistentclasses::TPersistentObject() { }
	/* TPersistentObject.CreateFromFiler */ inline __fastcall TGLFile3DSAnimationKeyList(Persistentclasses::TVirtualReader* reader) : Persistentclasses::TPersistentObject(reader) { }
	
};

#pragma pack(pop)

enum TGLFile3DSAnimKeysClassType : unsigned char { ctScale, ctRot, ctPos, ctCol, ctTPos, ctFall, ctHot, ctRoll };

class DELPHICLASS TGLFile3DSDummyObject;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLFile3DSDummyObject : public Glvectorfileobjects::TMorphableMeshObject
{
	typedef Glvectorfileobjects::TMorphableMeshObject inherited;
	
private:
	TGLFile3DSAnimationKeyList* FAnimList;
	void *FAnimData;
	TGLFile3DSAnimationData FRefTranf;
	TGLFile3DSAnimationData FAnimTransf;
	TGLFile3DSDummyObject* FParent;
	Types3ds::String64 FParentName;
	bool FStatic;
	
public:
	virtual void __fastcall LoadAnimation(const void * AData);
	virtual void __fastcall SetFrame(const double AFrame);
	virtual void __fastcall MorphTo(int morphTargetIndex);
	virtual void __fastcall Lerp(int morphTargetIndex1, int morphTargetIndex2, float lerpFactor);
	virtual void __fastcall GetExtents(/* out */ Vectortypes::TVector3f &min, /* out */ Vectortypes::TVector3f &max)/* overload */;
	DYNAMIC Vectorlists::TAffineVectorList* __fastcall ExtractTriangles(Vectorlists::TAffineVectorList* texCoords = (Vectorlists::TAffineVectorList*)(0x0), Vectorlists::TAffineVectorList* normals = (Vectorlists::TAffineVectorList*)(0x0));
	DYNAMIC void __fastcall WriteToFiler(Persistentclasses::TVirtualWriter* Writer);
	DYNAMIC void __fastcall ReadFromFiler(Persistentclasses::TVirtualReader* Reader);
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	__fastcall virtual TGLFile3DSDummyObject(void);
	__fastcall virtual ~TGLFile3DSDummyObject(void);
	__property TGLFile3DSAnimationKeyList* AnimList = {read=FAnimList};
	__property TGLFile3DSDummyObject* Parent = {read=FParent, write=FParent};
	__property TGLFile3DSAnimationData RefrenceTransf = {read=FRefTranf, write=FRefTranf};
public:
	/* TMeshObject.CreateOwned */ inline __fastcall TGLFile3DSDummyObject(Glvectorfileobjects::TMeshObjectList* AOwner) : Glvectorfileobjects::TMorphableMeshObject(AOwner) { }
	
public:
	/* TPersistentObject.CreateFromFiler */ inline __fastcall TGLFile3DSDummyObject(Persistentclasses::TVirtualReader* reader) : Glvectorfileobjects::TMorphableMeshObject(reader) { }
	
/* Hoisted overloads: */
	
public:
	inline void __fastcall  GetExtents(/* out */ Geometrybb::TAABB &aabb){ Glvectorfileobjects::TMeshObject::GetExtents(aabb); }
	
};

#pragma pack(pop)

class DELPHICLASS TGLFile3DSMeshObject;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLFile3DSMeshObject : public TGLFile3DSDummyObject
{
	typedef TGLFile3DSDummyObject inherited;
	
public:
	virtual void __fastcall LoadAnimation(const void * AData);
	virtual void __fastcall BuildList(Glrendercontextinfo::TRenderContextInfo &ARci);
public:
	/* TGLFile3DSDummyObject.Create */ inline __fastcall virtual TGLFile3DSMeshObject(void) : TGLFile3DSDummyObject() { }
	/* TGLFile3DSDummyObject.Destroy */ inline __fastcall virtual ~TGLFile3DSMeshObject(void) { }
	
public:
	/* TMeshObject.CreateOwned */ inline __fastcall TGLFile3DSMeshObject(Glvectorfileobjects::TMeshObjectList* AOwner) : TGLFile3DSDummyObject(AOwner) { }
	
public:
	/* TPersistentObject.CreateFromFiler */ inline __fastcall TGLFile3DSMeshObject(Persistentclasses::TVirtualReader* reader) : TGLFile3DSDummyObject(reader) { }
	
};

#pragma pack(pop)

class DELPHICLASS TGLFile3DSOmniLightObject;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLFile3DSOmniLightObject : public TGLFile3DSDummyObject
{
	typedef TGLFile3DSDummyObject inherited;
	
private:
	Glfile3dssceneobjects::TGLFile3DSLight* FLightSrc;
	Types3ds::String64 FLightSrcName;
	
public:
	__fastcall virtual TGLFile3DSOmniLightObject(void);
	virtual void __fastcall LoadData(Glvectorfileobjects::TGLBaseMesh* const AOwner, const Types3ds::PLight3DS AData);
	virtual void __fastcall LoadAnimation(const void * AData);
	virtual void __fastcall SetFrame(const double AFrame);
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	DYNAMIC void __fastcall WriteToFiler(Persistentclasses::TVirtualWriter* Writer);
	DYNAMIC void __fastcall ReadFromFiler(Persistentclasses::TVirtualReader* Reader);
	__fastcall virtual ~TGLFile3DSOmniLightObject(void);
public:
	/* TMeshObject.CreateOwned */ inline __fastcall TGLFile3DSOmniLightObject(Glvectorfileobjects::TMeshObjectList* AOwner) : TGLFile3DSDummyObject(AOwner) { }
	
public:
	/* TPersistentObject.CreateFromFiler */ inline __fastcall TGLFile3DSOmniLightObject(Persistentclasses::TVirtualReader* reader) : TGLFile3DSDummyObject(reader) { }
	
};

#pragma pack(pop)

class DELPHICLASS TGLFile3DSSpotLightObject;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLFile3DSSpotLightObject : public TGLFile3DSOmniLightObject
{
	typedef TGLFile3DSOmniLightObject inherited;
	
public:
	virtual void __fastcall LoadData(Glvectorfileobjects::TGLBaseMesh* const AOwner, const Types3ds::PLight3DS AData);
	virtual void __fastcall LoadAnimation(const void * AData);
	virtual void __fastcall SetFrame(const double AFrame);
public:
	/* TGLFile3DSOmniLightObject.Create */ inline __fastcall virtual TGLFile3DSSpotLightObject(void) : TGLFile3DSOmniLightObject() { }
	/* TGLFile3DSOmniLightObject.Destroy */ inline __fastcall virtual ~TGLFile3DSSpotLightObject(void) { }
	
public:
	/* TMeshObject.CreateOwned */ inline __fastcall TGLFile3DSSpotLightObject(Glvectorfileobjects::TMeshObjectList* AOwner) : TGLFile3DSOmniLightObject(AOwner) { }
	
public:
	/* TPersistentObject.CreateFromFiler */ inline __fastcall TGLFile3DSSpotLightObject(Persistentclasses::TVirtualReader* reader) : TGLFile3DSOmniLightObject(reader) { }
	
};

#pragma pack(pop)

class DELPHICLASS TGLFile3DSCameraObject;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLFile3DSCameraObject : public TGLFile3DSDummyObject
{
	typedef TGLFile3DSDummyObject inherited;
	
private:
	Globjects::TGLDummyCube* FTargetObj;
	Glfile3dssceneobjects::TGLFile3DSCamera* FCameraSrc;
	Types3ds::String64 FCameraSrcName;
	
public:
	__fastcall virtual TGLFile3DSCameraObject(void);
	void __fastcall LoadData(Glvectorfileobjects::TGLBaseMesh* Owner, Types3ds::PCamera3DS AData);
	virtual void __fastcall LoadAnimation(const void * AData);
	virtual void __fastcall SetFrame(const double AFrame);
	DYNAMIC void __fastcall WriteToFiler(Persistentclasses::TVirtualWriter* Writer);
	DYNAMIC void __fastcall ReadFromFiler(Persistentclasses::TVirtualReader* Reader);
	__fastcall virtual ~TGLFile3DSCameraObject(void);
public:
	/* TMeshObject.CreateOwned */ inline __fastcall TGLFile3DSCameraObject(Glvectorfileobjects::TMeshObjectList* AOwner) : TGLFile3DSDummyObject(AOwner) { }
	
public:
	/* TPersistentObject.CreateFromFiler */ inline __fastcall TGLFile3DSCameraObject(Persistentclasses::TVirtualReader* reader) : TGLFile3DSDummyObject(reader) { }
	
};

#pragma pack(pop)

class DELPHICLASS TGL3DSVectorFile;
class PASCALIMPLEMENTATION TGL3DSVectorFile : public Glvectorfileobjects::TVectorFile
{
	typedef Glvectorfileobjects::TVectorFile inherited;
	
public:
	__classmethod virtual Applicationfileio::TDataFileCapabilities __fastcall Capabilities();
	DYNAMIC void __fastcall LoadFromStream(System::Classes::TStream* aStream);
public:
	/* TVectorFile.Create */ inline __fastcall virtual TGL3DSVectorFile(System::Classes::TPersistent* AOwner) : Glvectorfileobjects::TVectorFile(AOwner) { }
	
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TGL3DSVectorFile(void) { }
	
};


//-- var, const, procedure ---------------------------------------------------
extern PACKAGE bool vGLFile3DS_UseTextureEx;
extern PACKAGE bool vGLFile3DS_EnableAnimation;
extern PACKAGE bool vGLFile3DS_FixDefaultUpAxisY;
extern PACKAGE int vGLFile3DS_LoadedStaticFrame;
}	/* namespace Glfile3ds */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLFILE3DS)
using namespace Glfile3ds;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Glfile3dsHPP
