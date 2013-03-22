// CodeGear C++Builder
// Copyright (c) 1995, 2012 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLParametricSurfaces.pas' rev: 24.00 (Win32)

#ifndef GlparametricsurfacesHPP
#define GlparametricsurfacesHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <GLVectorFileObjects.hpp>	// Pascal unit
#include <CurvesAndSurfaces.hpp>	// Pascal unit
#include <VectorGeometry.hpp>	// Pascal unit
#include <VectorLists.hpp>	// Pascal unit
#include <PersistentClasses.hpp>	// Pascal unit
#include <GLTexture.hpp>	// Pascal unit
#include <OpenGLTokens.hpp>	// Pascal unit
#include <OpenGLAdapter.hpp>	// Pascal unit
#include <GLState.hpp>	// Pascal unit
#include <GLRenderContextInfo.hpp>	// Pascal unit
#include <System.Classes.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Glparametricsurfaces
{
//-- type declarations -------------------------------------------------------
enum TParametricSurfaceRenderer : unsigned char { psrGLScene, psrOpenGL };

enum TParametricSurfaceBasis : unsigned char { psbBezier, psbBSpline };

class DELPHICLASS TMOParametricSurface;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TMOParametricSurface : public Glvectorfileobjects::TMeshObject
{
	typedef Glvectorfileobjects::TMeshObject inherited;
	
private:
	Vectorlists::TAffineVectorList* FControlPoints;
	Vectorlists::TAffineVectorList* FWeightedControlPoints;
	Vectorlists::TSingleList* FKnotsU;
	Vectorlists::TSingleList* FKnotsV;
	Vectorlists::TSingleList* FWeights;
	int FOrderU;
	int FOrderV;
	int FCountU;
	int FCountV;
	int FResolution;
	bool FAutoKnots;
	Curvesandsurfaces::TBSplineContinuity FContinuity;
	TParametricSurfaceRenderer FRenderer;
	TParametricSurfaceBasis FBasis;
	void __fastcall SetControlPoints(Vectorlists::TAffineVectorList* Value);
	void __fastcall SetKnotsU(Vectorlists::TSingleList* Value);
	void __fastcall SetKnotsV(Vectorlists::TSingleList* Value);
	void __fastcall SetWeights(Vectorlists::TSingleList* Value);
	void __fastcall SetRenderer(TParametricSurfaceRenderer Value);
	void __fastcall SetBasis(TParametricSurfaceBasis Value);
	
public:
	__fastcall virtual TMOParametricSurface(void);
	__fastcall virtual ~TMOParametricSurface(void);
	DYNAMIC void __fastcall WriteToFiler(Persistentclasses::TVirtualWriter* writer);
	DYNAMIC void __fastcall ReadFromFiler(Persistentclasses::TVirtualReader* reader);
	virtual void __fastcall BuildList(Glrendercontextinfo::TRenderContextInfo &mrci);
	DYNAMIC void __fastcall Prepare(void);
	DYNAMIC void __fastcall Clear(void);
	void __fastcall GenerateMesh(void);
	__property Vectorlists::TAffineVectorList* ControlPoints = {read=FControlPoints, write=SetControlPoints};
	__property Vectorlists::TSingleList* KnotsU = {read=FKnotsU, write=SetKnotsU};
	__property Vectorlists::TSingleList* KnotsV = {read=FKnotsV, write=SetKnotsV};
	__property Vectorlists::TSingleList* Weights = {read=FWeights, write=SetWeights};
	__property int OrderU = {read=FOrderU, write=FOrderU, nodefault};
	__property int OrderV = {read=FOrderV, write=FOrderV, nodefault};
	__property int CountU = {read=FCountU, write=FCountU, nodefault};
	__property int CountV = {read=FCountV, write=FCountV, nodefault};
	__property int Resolution = {read=FResolution, write=FResolution, nodefault};
	__property bool AutoKnots = {read=FAutoKnots, write=FAutoKnots, nodefault};
	__property Curvesandsurfaces::TBSplineContinuity Continuity = {read=FContinuity, write=FContinuity, nodefault};
	__property TParametricSurfaceRenderer Renderer = {read=FRenderer, write=SetRenderer, nodefault};
	__property TParametricSurfaceBasis Basis = {read=FBasis, write=SetBasis, nodefault};
public:
	/* TMeshObject.CreateOwned */ inline __fastcall TMOParametricSurface(Glvectorfileobjects::TMeshObjectList* AOwner) : Glvectorfileobjects::TMeshObject(AOwner) { }
	
public:
	/* TPersistentObject.CreateFromFiler */ inline __fastcall TMOParametricSurface(Persistentclasses::TVirtualReader* reader) : Glvectorfileobjects::TMeshObject(reader) { }
	
};

#pragma pack(pop)

class DELPHICLASS TFGBezierSurface;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TFGBezierSurface : public Glvectorfileobjects::TFaceGroup
{
	typedef Glvectorfileobjects::TFaceGroup inherited;
	
private:
	int FCountU;
	int FCountV;
	Vectorlists::TIntegerList* FControlPointIndices;
	Vectorlists::TIntegerList* FTexCoordIndices;
	int FResolution;
	float FMinU;
	float FMaxU;
	float FMinV;
	float FMaxV;
	Vectorlists::TAffineVectorList* FTempControlPoints;
	Vectorlists::TAffineVectorList* FTempTexCoords;
	
protected:
	void __fastcall SetControlPointIndices(Vectorlists::TIntegerList* const Value);
	void __fastcall SetTexCoordIndices(Vectorlists::TIntegerList* const Value);
	
public:
	__fastcall virtual TFGBezierSurface(void);
	__fastcall virtual ~TFGBezierSurface(void);
	DYNAMIC void __fastcall WriteToFiler(Persistentclasses::TVirtualWriter* writer);
	DYNAMIC void __fastcall ReadFromFiler(Persistentclasses::TVirtualReader* reader);
	virtual void __fastcall BuildList(Glrendercontextinfo::TRenderContextInfo &mrci);
	DYNAMIC void __fastcall Prepare(void);
	__property int CountU = {read=FCountU, write=FCountU, nodefault};
	__property int CountV = {read=FCountV, write=FCountV, nodefault};
	__property int Resolution = {read=FResolution, write=FResolution, nodefault};
	__property float MinU = {read=FMinU, write=FMinU};
	__property float MaxU = {read=FMaxU, write=FMaxU};
	__property float MinV = {read=FMinV, write=FMinV};
	__property float MaxV = {read=FMaxV, write=FMaxV};
	__property Vectorlists::TIntegerList* ControlPointIndices = {read=FControlPointIndices, write=SetControlPointIndices};
	__property Vectorlists::TIntegerList* TexCoordIndices = {read=FTexCoordIndices, write=SetTexCoordIndices};
public:
	/* TFaceGroup.CreateOwned */ inline __fastcall virtual TFGBezierSurface(Glvectorfileobjects::TFaceGroups* AOwner) : Glvectorfileobjects::TFaceGroup(AOwner) { }
	
public:
	/* TPersistentObject.CreateFromFiler */ inline __fastcall TFGBezierSurface(Persistentclasses::TVirtualReader* reader) : Glvectorfileobjects::TFaceGroup(reader) { }
	
};

#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------
}	/* namespace Glparametricsurfaces */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLPARAMETRICSURFACES)
using namespace Glparametricsurfaces;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// GlparametricsurfacesHPP
