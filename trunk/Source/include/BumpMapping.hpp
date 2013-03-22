// CodeGear C++Builder
// Copyright (c) 1995, 2012 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'BumpMapping.pas' rev: 24.00 (Win32)

#ifndef BumpmappingHPP
#define BumpmappingHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <VectorGeometry.hpp>	// Pascal unit
#include <VectorLists.hpp>	// Pascal unit
#include <GLCrossPlatform.hpp>	// Pascal unit
#include <VectorTypes.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Bumpmapping
{
//-- type declarations -------------------------------------------------------
enum TNormalMapSpace : unsigned char { nmsObject, nmsTangent };

//-- var, const, procedure ---------------------------------------------------
extern PACKAGE void __fastcall CalcObjectSpaceLightVectors(const Vectortypes::TVector3f &Light, Vectorlists::TAffineVectorList* Vertices, Vectorlists::TVectorList* Colors);
extern PACKAGE void __fastcall SetupTangentSpace(Vectorlists::TAffineVectorList* Vertices, Vectorlists::TAffineVectorList* Normals, Vectorlists::TAffineVectorList* TexCoords, Vectorlists::TAffineVectorList* Tangents, Vectorlists::TAffineVectorList* BiNormals);
extern PACKAGE void __fastcall CalcTangentSpaceLightVectors(const Vectortypes::TVector3f &Light, Vectorlists::TAffineVectorList* Vertices, Vectorlists::TAffineVectorList* Normals, Vectorlists::TAffineVectorList* Tangents, Vectorlists::TAffineVectorList* BiNormals, Vectorlists::TVectorList* Colors);
extern PACKAGE Vcl::Graphics::TBitmap* __fastcall CreateObjectSpaceNormalMap(int Width, int Height, Vectorlists::TAffineVectorList* HiNormals, Vectorlists::TAffineVectorList* HiTexCoords);
extern PACKAGE Vcl::Graphics::TBitmap* __fastcall CreateTangentSpaceNormalMap(int Width, int Height, Vectorlists::TAffineVectorList* HiNormals, Vectorlists::TAffineVectorList* HiTexCoords, Vectorlists::TAffineVectorList* LoNormals, Vectorlists::TAffineVectorList* LoTexCoords, Vectorlists::TAffineVectorList* Tangents, Vectorlists::TAffineVectorList* BiNormals);
}	/* namespace Bumpmapping */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_BUMPMAPPING)
using namespace Bumpmapping;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// BumpmappingHPP
