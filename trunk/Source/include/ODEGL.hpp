// CodeGear C++Builder
// Copyright (c) 1995, 2012 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'ODEGL.pas' rev: 24.00 (Win32)

#ifndef OdeglHPP
#define OdeglHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <OpenGLTokens.hpp>	// Pascal unit
#include <GLContext.hpp>	// Pascal unit
#include <VectorGeometry.hpp>	// Pascal unit
#include <ODEImport.hpp>	// Pascal unit
#include <GLScene.hpp>	// Pascal unit
#include <VectorTypes.hpp>	// Pascal unit
#include <VectorLists.hpp>	// Pascal unit
#include <GLObjects.hpp>	// Pascal unit
#include <GLVerletClothify.hpp>	// Pascal unit
#include <GLVectorFileObjects.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Odegl
{
//-- type declarations -------------------------------------------------------
//-- var, const, procedure ---------------------------------------------------
extern PACKAGE void __fastcall ODERToGLSceneMatrix(Vectortypes::TMatrix4f &m, System::StaticArray<float, 4> *R, float *pos)/* overload */;
extern PACKAGE void __fastcall ODERToGLSceneMatrix(Vectortypes::TMatrix4f &m, Odeimport::PdMatrix3 R, Odeimport::PdVector3 pos)/* overload */;
extern PACKAGE void __fastcall ODERToGLSceneMatrix(Vectortypes::TMatrix4f &m, float *R, float *pos)/* overload */;
extern PACKAGE void __fastcall DrawBox(float *Sides);
extern PACKAGE Odeimport::TdMatrix3 __fastcall GLSceneMatrixToODER(const Vectortypes::TMatrix4f &m);
extern PACKAGE void __fastcall dsDrawBox(Odeimport::PdVector3 pos, Odeimport::PdMatrix3 R, float *Sides)/* overload */;
extern PACKAGE void __fastcall dsDrawBox(float *pos, float *R, float *Sides)/* overload */;
extern PACKAGE void __fastcall setTransform(float *pos, float *R);
extern PACKAGE Vectortypes::TVector3f __fastcall ConvertdVector3ToVector3f(float *R)/* overload */;
extern PACKAGE Vectortypes::TVector3f __fastcall ConvertdVector3ToVector3f(Odeimport::PdVector3 R)/* overload */;
extern PACKAGE Vectortypes::TVector4f __fastcall ConvertdVector3ToVector4f(float *R)/* overload */;
extern PACKAGE Vectortypes::TVector4f __fastcall ConvertdVector3ToVector4f(Odeimport::PdVector3 R)/* overload */;
extern PACKAGE Vectortypes::TVector3f __fastcall ConvertdVector3ToAffineVector(Odeimport::PdVector3 R)/* overload */;
extern PACKAGE Vectortypes::TVector3f __fastcall ConvertdVector3ToAffineVector(float *R)/* overload */;
extern PACKAGE Odeimport::TdVector3 __fastcall ConvertVector3fTodVector3(const Vectortypes::TVector3f &R);
extern PACKAGE Odeimport::PdVector3 __fastcall ConvertVector3fToPdVector3(const Vectortypes::TVector3f &R);
extern PACKAGE Odeimport::TdVector3 __fastcall ConvertVector4fTodVector3(const Vectortypes::TVector4f &R);
extern PACKAGE Odeimport::PdVector3 __fastcall ConvertVector4fToPdVector3(const Vectortypes::TVector4f &R);
extern PACKAGE Vectortypes::TVector3f __fastcall GetBodyPositionAsAffineVector(Odeimport::PdxBody Body);
extern PACKAGE void __fastcall PositionSceneObjectForGeom(Odeimport::PdxGeom Geom);
extern PACKAGE Vectortypes::TMatrix4f __fastcall GLMatrixFromGeom(Odeimport::PdxGeom Geom);
extern PACKAGE Vectortypes::TVector4f __fastcall GLDirectionFromGeom(Odeimport::PdxGeom Geom);
extern PACKAGE void __fastcall PositionSceneObject(Glscene::TGLBaseSceneObject* GLBaseSceneObject, Odeimport::PdxGeom Geom);
extern PACKAGE void __fastcall CopyCubeSizeFromBox(Globjects::TGLCube* Cube, Odeimport::PdxGeom Geom);
extern PACKAGE void __fastcall CopyPosFromGeomToGL(Odeimport::PdxGeom Geom, Glscene::TGLBaseSceneObject* GLBaseSceneObject);
extern PACKAGE Odeimport::PdxGeom __fastcall CreateGeomFromCube(Globjects::TGLCube* Cube, Odeimport::PdxSpace Space);
extern PACKAGE Odeimport::PdxBody __fastcall CreateBodyFromCube(Odeimport::PdxGeom &Geom, Globjects::TGLCube* Cube, Odeimport::PdxWorld World, Odeimport::PdxSpace Space);
extern PACKAGE Odeimport::PdxGeom __fastcall CreateTriMeshFromBaseMesh(Glvectorfileobjects::TGLBaseMesh* GLBaseMesh, Odeimport::PdxSpace Space, Odeimport::PdVector3Array &Vertices, Odeimport::PdIntegerArray &Indices);
extern PACKAGE void __fastcall CopyBodyFromCube(Odeimport::PdxBody Body, Odeimport::PdxGeom &Geom, Globjects::TGLCube* Cube, Odeimport::PdxSpace Space);
extern PACKAGE float __fastcall dBodyToBodyDistance(Odeimport::PdxBody Body1, Odeimport::PdxBody Body2);
extern PACKAGE float __fastcall dVector3Length(float *R)/* overload */;
extern PACKAGE float __fastcall dVector3Length(Odeimport::PdVector3 R)/* overload */;
extern PACKAGE void __fastcall RenderGeomList(Odeimport::TGeomList* GeomList);
extern PACKAGE Odeimport::PdxGeom __fastcall CreateODEPlaneFromGLPlane(Globjects::TGLPlane* Plane, Odeimport::PdxSpace Space);
extern PACKAGE Vectortypes::TVector4f __fastcall RandomColorVector(void);
}	/* namespace Odegl */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_ODEGL)
using namespace Odegl;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// OdeglHPP
