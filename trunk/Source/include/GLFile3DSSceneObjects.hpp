// CodeGear C++Builder
// Copyright (c) 1995, 2012 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLFile3DSSceneObjects.pas' rev: 24.00 (Win32)

#ifndef Glfile3dssceneobjectsHPP
#define Glfile3dssceneobjectsHPP

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
#include <VectorGeometry.hpp>	// Pascal unit
#include <OpenGLTokens.hpp>	// Pascal unit
#include <OpenGLAdapter.hpp>	// Pascal unit
#include <GLContext.hpp>	// Pascal unit
#include <GLScene.hpp>	// Pascal unit
#include <GLVectorFileObjects.hpp>	// Pascal unit
#include <VectorTypes.hpp>	// Pascal unit
#include <PersistentClasses.hpp>	// Pascal unit
#include <GLCrossPlatform.hpp>	// Pascal unit
#include <GLCoordinates.hpp>	// Pascal unit
#include <GLRenderContextInfo.hpp>	// Pascal unit
#include <GLState.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Glfile3dssceneobjects
{
//-- type declarations -------------------------------------------------------
class DELPHICLASS TGLFile3DSLight;
class PASCALIMPLEMENTATION TGLFile3DSLight : public Glscene::TGLLightSource
{
	typedef Glscene::TGLLightSource inherited;
	
private:
	Glcoordinates::TGLCoordinates3* FTargetPos;
	float FHotSpot;
	float FMultipler;
	
public:
	__fastcall virtual TGLFile3DSLight(System::Classes::TComponent* AOwner);
	virtual void __fastcall DoRender(Glrendercontextinfo::TRenderContextInfo &rci, bool renderSelf, bool renderChildren);
	virtual void __fastcall CoordinateChanged(Glcoordinates::TGLCustomCoordinates* Sender);
	__fastcall virtual ~TGLFile3DSLight(void);
	
__published:
	__property Glcoordinates::TGLCoordinates3* SpotTargetPos = {read=FTargetPos};
	__property float HotSpot = {read=FHotSpot, write=FHotSpot};
	__property float Multipler = {read=FMultipler, write=FMultipler};
public:
	/* TGLBaseSceneObject.CreateAsChild */ inline __fastcall TGLFile3DSLight(Glscene::TGLBaseSceneObject* aParentOwner) : Glscene::TGLLightSource(aParentOwner) { }
	
};


class DELPHICLASS TGLFile3DSCamera;
class PASCALIMPLEMENTATION TGLFile3DSCamera : public Glscene::TGLCamera
{
	typedef Glscene::TGLCamera inherited;
	
private:
	Glcoordinates::TGLCoordinates3* FTargetPos;
	System::StaticArray<Opengltokens::PGLUQuadric, 2> FQuadCyl;
	System::StaticArray<Opengltokens::PGLUQuadric, 2> FQuadDisk;
	
public:
	__fastcall virtual TGLFile3DSCamera(System::Classes::TComponent* AOwner);
	virtual void __fastcall DoRender(Glrendercontextinfo::TRenderContextInfo &rci, bool renderSelf, bool renderChildren);
	virtual void __fastcall CoordinateChanged(Glcoordinates::TGLCustomCoordinates* Sender);
	__fastcall virtual ~TGLFile3DSCamera(void);
	
__published:
	__property Glcoordinates::TGLCoordinates3* CameraTargetPos = {read=FTargetPos};
	__property RollAngle = {default=0};
public:
	/* TGLBaseSceneObject.CreateAsChild */ inline __fastcall TGLFile3DSCamera(Glscene::TGLBaseSceneObject* aParentOwner) : Glscene::TGLCamera(aParentOwner) { }
	
};


class DELPHICLASS TGLFile3DSActor;
class PASCALIMPLEMENTATION TGLFile3DSActor : public Glvectorfileobjects::TGLActor
{
	typedef Glvectorfileobjects::TGLActor inherited;
	
private:
	void __fastcall ReadMesh(System::Classes::TStream* Stream);
	void __fastcall WriteMesh(System::Classes::TStream* Stream);
	
protected:
	virtual void __fastcall DefineProperties(System::Classes::TFiler* Filer);
public:
	/* TGLActor.Create */ inline __fastcall virtual TGLFile3DSActor(System::Classes::TComponent* AOwner) : Glvectorfileobjects::TGLActor(AOwner) { }
	/* TGLActor.Destroy */ inline __fastcall virtual ~TGLFile3DSActor(void) { }
	
public:
	/* TGLBaseSceneObject.CreateAsChild */ inline __fastcall TGLFile3DSActor(Glscene::TGLBaseSceneObject* aParentOwner) : Glvectorfileobjects::TGLActor(aParentOwner) { }
	
};


class DELPHICLASS TGLFile3DSFreeForm;
class PASCALIMPLEMENTATION TGLFile3DSFreeForm : public Glvectorfileobjects::TGLFreeForm
{
	typedef Glvectorfileobjects::TGLFreeForm inherited;
	
private:
	Vectortypes::TMatrix4f FTransfMat;
	Vectortypes::TMatrix4f FScaleMat;
	Vectortypes::TMatrix4f ParentMatrix;
	Glcoordinates::TGLCoordinates4* FS_Rot3DS;
	Glcoordinates::TGLCoordinates4* FRot3DS;
	Glcoordinates::TGLCoordinates4* FScale3DS;
	void __fastcall ReadMesh(System::Classes::TStream* Stream);
	void __fastcall WriteMesh(System::Classes::TStream* Stream);
	
protected:
	virtual void __fastcall DefineProperties(System::Classes::TFiler* Filer);
	
public:
	Vectortypes::TMatrix4f FRefMat;
	__fastcall virtual TGLFile3DSFreeForm(System::Classes::TComponent* AOWner);
	__fastcall virtual ~TGLFile3DSFreeForm(void);
	virtual void __fastcall BuildList(Glrendercontextinfo::TRenderContextInfo &rci);
	virtual void __fastcall CoordinateChanged(Glcoordinates::TGLCustomCoordinates* Sender);
	virtual Vectortypes::TVector4f __fastcall AxisAlignedDimensionsUnscaled(void);
	virtual Vectortypes::TVector4f __fastcall BarycenterAbsolutePosition(void);
	
__published:
	__property Glcoordinates::TGLCoordinates4* S_Rot3DS = {read=FS_Rot3DS};
	__property Glcoordinates::TGLCoordinates4* Rot3DS = {read=FRot3DS};
	__property Glcoordinates::TGLCoordinates4* Scale3DS = {read=FScale3DS};
public:
	/* TGLBaseSceneObject.CreateAsChild */ inline __fastcall TGLFile3DSFreeForm(Glscene::TGLBaseSceneObject* aParentOwner) : Glvectorfileobjects::TGLFreeForm(aParentOwner) { }
	
};


//-- var, const, procedure ---------------------------------------------------
extern PACKAGE bool vGLFile3DSSceneObjects_RenderCameraAndLights;
}	/* namespace Glfile3dssceneobjects */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLFILE3DSSCENEOBJECTS)
using namespace Glfile3dssceneobjects;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Glfile3dssceneobjectsHPP
