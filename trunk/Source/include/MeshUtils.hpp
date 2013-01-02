// CodeGear C++Builder
// Copyright (c) 1995, 2012 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'MeshUtils.pas' rev: 24.00 (Win32)

#ifndef MeshutilsHPP
#define MeshutilsHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <System.Classes.hpp>	// Pascal unit
#include <PersistentClasses.hpp>	// Pascal unit
#include <VectorLists.hpp>	// Pascal unit
#include <VectorGeometry.hpp>	// Pascal unit
#include <VectorTypes.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Meshutils
{
//-- type declarations -------------------------------------------------------
typedef void __fastcall (*TSubdivideEdgeEvent)(const int idxA, const int idxB, const int newIdx);

//-- var, const, procedure ---------------------------------------------------
extern PACKAGE bool vImprovedFixingOpenTriangleEdge;
extern PACKAGE unsigned vEdgeInfoReserveSize;
extern PACKAGE void __fastcall ConvertStripToList(Vectorlists::TAffineVectorList* const strip, Vectorlists::TAffineVectorList* list)/* overload */;
extern PACKAGE void __fastcall ConvertStripToList(Vectorlists::TIntegerList* const strip, Vectorlists::TIntegerList* list)/* overload */;
extern PACKAGE void __fastcall ConvertStripToList(Vectorlists::TAffineVectorList* const strip, Vectorlists::TIntegerList* const indices, Vectorlists::TAffineVectorList* list)/* overload */;
extern PACKAGE void __fastcall ConvertIndexedListToList(Vectorlists::TAffineVectorList* const data, Vectorlists::TIntegerList* const indices, Vectorlists::TAffineVectorList* list);
extern PACKAGE Vectorlists::TIntegerList* __fastcall BuildVectorCountOptimizedIndices(Vectorlists::TAffineVectorList* const vertices, Vectorlists::TAffineVectorList* const normals = (Vectorlists::TAffineVectorList*)(0x0), Vectorlists::TAffineVectorList* const texCoords = (Vectorlists::TAffineVectorList*)(0x0));
extern PACKAGE void __fastcall RemapReferences(Vectorlists::TAffineVectorList* reference, Vectorlists::TIntegerList* const indices)/* overload */;
extern PACKAGE void __fastcall RemapReferences(Vectorlists::TIntegerList* reference, Vectorlists::TIntegerList* const indices)/* overload */;
extern PACKAGE void __fastcall RemapAndCleanupReferences(Vectorlists::TAffineVectorList* reference, Vectorlists::TIntegerList* indices);
extern PACKAGE Vectorlists::TIntegerList* __fastcall RemapIndicesToIndicesMap(Vectorlists::TIntegerList* remapIndices);
extern PACKAGE void __fastcall RemapTrianglesIndices(Vectorlists::TIntegerList* indices, Vectorlists::TIntegerList* indicesMap);
extern PACKAGE void __fastcall RemapIndices(Vectorlists::TIntegerList* indices, Vectorlists::TIntegerList* indicesMap);
extern PACKAGE void __fastcall UnifyTrianglesWinding(Vectorlists::TIntegerList* indices);
extern PACKAGE void __fastcall InvertTrianglesWinding(Vectorlists::TIntegerList* indices);
extern PACKAGE Vectorlists::TAffineVectorList* __fastcall BuildNormals(Vectorlists::TAffineVectorList* reference, Vectorlists::TIntegerList* indices);
extern PACKAGE Vectorlists::TIntegerList* __fastcall BuildNonOrientedEdgesList(Vectorlists::TIntegerList* triangleIndices, Vectorlists::TIntegerList* triangleEdges = (Vectorlists::TIntegerList*)(0x0), Vectorlists::TIntegerList* edgesTriangles = (Vectorlists::TIntegerList*)(0x0));
extern PACKAGE void __fastcall IncreaseCoherency(Vectorlists::TIntegerList* indices, int cacheSize);
extern PACKAGE void __fastcall WeldVertices(Vectorlists::TAffineVectorList* vertices, Vectorlists::TIntegerList* indicesMap, float weldRadius);
extern PACKAGE Persistentclasses::TPersistentObjectList* __fastcall StripifyMesh(Vectorlists::TIntegerList* indices, int maxVertexIndex, bool agglomerateLoneTriangles = false);
extern PACKAGE void __fastcall SubdivideTriangles(float smoothFactor, Vectorlists::TAffineVectorList* vertices, Vectorlists::TIntegerList* triangleIndices, Vectorlists::TAffineVectorList* normals = (Vectorlists::TAffineVectorList*)(0x0), TSubdivideEdgeEvent onSubdivideEdge = 0x0);
extern PACKAGE Vectorlists::TLongWordList* __fastcall MakeTriangleAdjacencyList(const Vectorgeometry::PLongWordVector AindicesList, unsigned Count, const Vectorgeometry::PAffineVectorArray AVerticesList);
extern PACKAGE Vectorlists::TLongWordList* __fastcall ConvertStripToList(const Vectorgeometry::PLongWordVector AindicesList, unsigned Count, unsigned RestartIndex)/* overload */;
extern PACKAGE Vectorlists::TLongWordList* __fastcall ConvertFansToList(const Vectorgeometry::PLongWordVector AindicesList, unsigned Count, unsigned RestartIndex);
}	/* namespace Meshutils */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_MESHUTILS)
using namespace Meshutils;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// MeshutilsHPP
