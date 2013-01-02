// CodeGear C++Builder
// Copyright (c) 1995, 2012 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLRenderContextInfo.pas' rev: 24.00 (Win32)

#ifndef GlrendercontextinfoHPP
#define GlrendercontextinfoHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <PersistentClasses.hpp>	// Pascal unit
#include <VectorGeometry.hpp>	// Pascal unit
#include <GLState.hpp>	// Pascal unit
#include <GLPipelineTransformation.hpp>	// Pascal unit
#include <GLColor.hpp>	// Pascal unit
#include <VectorTypes.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Glrendercontextinfo
{
//-- type declarations -------------------------------------------------------
enum TDrawState : unsigned char { dsRendering, dsPicking, dsPrinting };

struct DECLSPEC_DRECORD TGLSize
{
public:
	int cx;
	int cy;
};


enum TGLObjectsSorting : unsigned char { osInherited, osNone, osRenderFarthestFirst, osRenderBlendedLast, osRenderNearestFirst };

enum TGLVisibilityCulling : unsigned char { vcInherited, vcNone, vcObjectBased, vcHierarchical };

struct DECLSPEC_DRECORD TRenderContextClippingInfo
{
public:
	Vectortypes::TVector4f origin;
	Vectortypes::TVector4f clippingDirection;
	float viewPortRadius;
	float nearClippingDistance;
	float farClippingDistance;
	Vectorgeometry::TFrustum frustum;
};


struct DECLSPEC_DRECORD TRenderContextInfo
{
public:
	System::TObject* scene;
	System::TObject* buffer;
	Vectortypes::TVector4f cameraPosition;
	Vectortypes::TVector4f cameraDirection;
	Vectortypes::TVector4f cameraUp;
	TGLSize viewPortSize;
	int renderDPI;
	System::TObject* materialLibrary;
	System::TObject* lightmapLibrary;
	int fogDisabledCounter;
	TDrawState drawState;
	TGLObjectsSorting objectsSorting;
	TGLVisibilityCulling visibilityCulling;
	Glstate::TGLStateCache* GLStates;
	Glpipelinetransformation::TGLTransformation* PipelineTransformation;
	TRenderContextClippingInfo rcci;
	Vectortypes::TVector4f sceneAmbientColor;
	bool bufferFaceCull;
	bool bufferLighting;
	bool bufferFog;
	bool bufferDepthTest;
	bool proxySubObject;
	bool ignoreMaterials;
	bool ignoreBlendingRequests;
	bool ignoreDepthRequests;
	bool amalgamating;
	Persistentclasses::TPersistentObjectList* lights;
	Persistentclasses::TPersistentObjectList* afterRenderEffects;
	Glstate::TGLMaterialLevel currentMaterialLevel;
	Glstate::TGLMeshPrimitives primitiveMask;
	int orderCounter;
};


typedef TRenderContextInfo *PRenderContextInfo;

//-- var, const, procedure ---------------------------------------------------
}	/* namespace Glrendercontextinfo */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLRENDERCONTEXTINFO)
using namespace Glrendercontextinfo;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// GlrendercontextinfoHPP
