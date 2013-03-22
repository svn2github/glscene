// CodeGear C++Builder
// Copyright (c) 1995, 2012 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLGraph.pas' rev: 24.00 (Win32)

#ifndef GlgraphHPP
#define GlgraphHPP

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
#include <GLMaterial.hpp>	// Pascal unit
#include <GLObjects.hpp>	// Pascal unit
#include <VectorLists.hpp>	// Pascal unit
#include <GLColor.hpp>	// Pascal unit
#include <BaseClasses.hpp>	// Pascal unit
#include <GLRenderContextInfo.hpp>	// Pascal unit
#include <VectorTypes.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Glgraph
{
//-- type declarations -------------------------------------------------------
class DELPHICLASS TGLSamplingScale;
class PASCALIMPLEMENTATION TGLSamplingScale : public Baseclasses::TGLUpdateAbleObject
{
	typedef Baseclasses::TGLUpdateAbleObject inherited;
	
private:
	float FMin;
	float FMax;
	float FOrigin;
	float FStep;
	
protected:
	void __fastcall SetMin(const float val);
	void __fastcall SetMax(const float val);
	void __fastcall SetOrigin(const float val);
	void __fastcall SetStep(const float val);
	
public:
	__fastcall virtual TGLSamplingScale(System::Classes::TPersistent* AOwner);
	__fastcall virtual ~TGLSamplingScale(void);
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	float __fastcall StepBase(void);
	int __fastcall MaxStepCount(void);
	bool __fastcall IsValid(void);
	void __fastcall SetBaseStepMaxToVars(float &Base, float &Step, float &Max, bool SamplingEnabled = true);
	
__published:
	__property float Min = {read=FMin, write=SetMin};
	__property float Max = {read=FMax, write=SetMax};
	__property float Origin = {read=FOrigin, write=SetOrigin};
	__property float Step = {read=FStep, write=SetStep};
};


typedef void __fastcall (__closure *THeightFieldGetHeightEvent)(const float x, const float y, float &z, Vectortypes::TVector4f &Color, Vectorgeometry::TTexPoint &TexPoint);

typedef void __fastcall (__closure *THeightFieldGetHeight2Event)(System::TObject* Sender, const float x, const float y, float &z, Vectortypes::TVector4f &Color, Vectorgeometry::TTexPoint &TexPoint);

enum THeightFieldOption : unsigned char { hfoTextureCoordinates, hfoTwoSided };

typedef System::Set<THeightFieldOption, THeightFieldOption::hfoTextureCoordinates, THeightFieldOption::hfoTwoSided>  THeightFieldOptions;

enum THeightFieldColorMode : unsigned char { hfcmNone, hfcmEmission, hfcmAmbient, hfcmDiffuse, hfcmAmbientAndDiffuse };

class DELPHICLASS TGLHeightField;
class PASCALIMPLEMENTATION TGLHeightField : public Glscene::TGLSceneObject
{
	typedef Glscene::TGLSceneObject inherited;
	
private:
	THeightFieldGetHeightEvent FOnGetHeight;
	THeightFieldGetHeight2Event FOnGetHeight2;
	TGLSamplingScale* FXSamplingScale;
	TGLSamplingScale* FYSamplingScale;
	THeightFieldOptions FOptions;
	int FTriangleCount;
	THeightFieldColorMode FColorMode;
	
protected:
	void __fastcall SetXSamplingScale(TGLSamplingScale* const val);
	void __fastcall SetYSamplingScale(TGLSamplingScale* const val);
	void __fastcall SetOptions(const THeightFieldOptions val);
	void __fastcall SetOnGetHeight(const THeightFieldGetHeightEvent val);
	void __fastcall SetOnGetHeight2(const THeightFieldGetHeight2Event val);
	void __fastcall SetColorMode(const THeightFieldColorMode val);
	void __fastcall DefaultHeightField(const float x, const float y, float &z, Vectortypes::TVector4f &Color, Vectorgeometry::TTexPoint &TexPoint);
	void __fastcall Height2Field(const float x, const float y, float &z, Vectortypes::TVector4f &Color, Vectorgeometry::TTexPoint &texPoint);
	
public:
	__fastcall virtual TGLHeightField(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TGLHeightField(void);
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	virtual void __fastcall BuildList(Glrendercontextinfo::TRenderContextInfo &rci);
	virtual void __fastcall NotifyChange(System::TObject* Sender);
	__property int TriangleCount = {read=FTriangleCount, nodefault};
	
__published:
	__property TGLSamplingScale* XSamplingScale = {read=FXSamplingScale, write=SetXSamplingScale};
	__property TGLSamplingScale* YSamplingScale = {read=FYSamplingScale, write=SetYSamplingScale};
	__property THeightFieldColorMode ColorMode = {read=FColorMode, write=SetColorMode, default=0};
	__property THeightFieldOptions Options = {read=FOptions, write=SetOptions, default=2};
	__property THeightFieldGetHeightEvent OnGetHeight = {read=FOnGetHeight, write=SetOnGetHeight};
	__property THeightFieldGetHeight2Event OnGetHeight2 = {read=FOnGetHeight2, write=SetOnGetHeight2};
public:
	/* TGLBaseSceneObject.CreateAsChild */ inline __fastcall TGLHeightField(Glscene::TGLBaseSceneObject* aParentOwner) : Glscene::TGLSceneObject(aParentOwner) { }
	
};


enum TXYZGridPart : unsigned char { gpX, gpY, gpZ };

typedef System::Set<TXYZGridPart, TXYZGridPart::gpX, TXYZGridPart::gpZ>  TXYZGridParts;

enum TXYZGridLinesStyle : unsigned char { glsLine, glsSegments };

class DELPHICLASS TGLXYZGrid;
class PASCALIMPLEMENTATION TGLXYZGrid : public Globjects::TGLLineBase
{
	typedef Globjects::TGLLineBase inherited;
	
private:
	TGLSamplingScale* FXSamplingScale;
	TGLSamplingScale* FYSamplingScale;
	TGLSamplingScale* FZSamplingScale;
	TXYZGridParts FParts;
	TXYZGridLinesStyle FLinesStyle;
	
protected:
	void __fastcall SetXSamplingScale(TGLSamplingScale* const val);
	void __fastcall SetYSamplingScale(TGLSamplingScale* const val);
	void __fastcall SetZSamplingScale(TGLSamplingScale* const val);
	void __fastcall SetParts(const TXYZGridParts val);
	void __fastcall SetLinesStyle(const TXYZGridLinesStyle val);
	void __fastcall SetLinesSmoothing(const bool val);
	
public:
	__fastcall virtual TGLXYZGrid(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TGLXYZGrid(void);
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	virtual void __fastcall BuildList(Glrendercontextinfo::TRenderContextInfo &rci);
	virtual void __fastcall NotifyChange(System::TObject* Sender);
	
__published:
	__property TGLSamplingScale* XSamplingScale = {read=FXSamplingScale, write=SetXSamplingScale};
	__property TGLSamplingScale* YSamplingScale = {read=FYSamplingScale, write=SetYSamplingScale};
	__property TGLSamplingScale* ZSamplingScale = {read=FZSamplingScale, write=SetZSamplingScale};
	__property TXYZGridParts Parts = {read=FParts, write=SetParts, default=3};
	__property TXYZGridLinesStyle LinesStyle = {read=FLinesStyle, write=SetLinesStyle, default=1};
	__property bool LinesSmoothing = {write=SetLinesSmoothing, stored=false, nodefault};
public:
	/* TGLBaseSceneObject.CreateAsChild */ inline __fastcall TGLXYZGrid(Glscene::TGLBaseSceneObject* aParentOwner) : Globjects::TGLLineBase(aParentOwner) { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Glgraph */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLGRAPH)
using namespace Glgraph;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// GlgraphHPP
