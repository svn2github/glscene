// CodeGear C++Builder
// Copyright (c) 1995, 2012 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLFileLMTS.pas' rev: 24.00 (Win32)

#ifndef GlfilelmtsHPP
#define GlfilelmtsHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <Vcl.Graphics.hpp>	// Pascal unit
#include <System.Classes.hpp>	// Pascal unit
#include <System.SysUtils.hpp>	// Pascal unit
#include <GLVectorFileObjects.hpp>	// Pascal unit
#include <ApplicationFileIO.hpp>	// Pascal unit
#include <VectorLists.hpp>	// Pascal unit
#include <VectorGeometry.hpp>	// Pascal unit
#include <GLTexture.hpp>	// Pascal unit
#include <PersistentClasses.hpp>	// Pascal unit
#include <GLGraphics.hpp>	// Pascal unit
#include <GLMaterial.hpp>	// Pascal unit
#include <VectorTypes.hpp>	// Pascal unit
#include <BaseClasses.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Glfilelmts
{
//-- type declarations -------------------------------------------------------
struct TLMTS_Header;
typedef TLMTS_Header *PLMTS_Header;

struct DECLSPEC_DRECORD TLMTS_Header
{
public:
	unsigned ID;
	unsigned Ver;
	unsigned headerSize;
	System::Word nTexts;
	System::Word nSubsets;
	unsigned nTris;
	System::Word subSize;
	System::Word vtxSize;
};


struct TLMTS_TexData;
typedef TLMTS_TexData *PLMTS_TexData;

struct DECLSPEC_DRECORD TLMTS_TexData
{
public:
	System::StaticArray<char, 256> fName;
	System::Word Flags;
};


struct TLMTS_Subset;
typedef TLMTS_Subset *PLMTS_Subset;

struct DECLSPEC_DRECORD TLMTS_Subset
{
public:
	int Offset;
	int Count;
	System::Word TextID1;
	System::Word TextID2;
};


struct TLMTS_Vertex;
typedef TLMTS_Vertex *PLMTS_Vertex;

struct DECLSPEC_DRECORD TLMTS_Vertex
{
public:
	float x;
	float y;
	float z;
	float u1;
	float v1;
	float u2;
	float v2;
};


struct TLMTS;
typedef TLMTS *PLMTS;

struct DECLSPEC_DRECORD TLMTS
{
public:
	TLMTS_Header header;
	void *usrData;
	unsigned usrSize;
	void *texData;
	void *subsets;
	void *tris;
	bool ok;
};


struct DECLSPEC_DRECORD TMaterialInfo
{
public:
	Glmaterial::TShininess FShininess;
	Glmaterial::TShininess BShininess;
	Vectortypes::TVector4f FAmbient;
	Vectortypes::TVector4f FDiffuse;
	Vectortypes::TVector4f FEmission;
	Vectortypes::TVector4f FSpecular;
	Vectortypes::TVector4f BAmbient;
	Vectortypes::TVector4f BDiffuse;
	Vectortypes::TVector4f BEmission;
	Vectortypes::TVector4f BSpecular;
	Gltexture::TGLTextureImageAlpha ImageAlpha;
	Gltexture::TGLMagFilter magFilter;
	Gltexture::TGLMinFilter minFilter;
	Gltexture::TGLTextureMode TextureMode;
	Gltexture::TGLTextureWrap TextureWrap;
	Glmaterial::TBlendingMode Blendingmode;
	Glmaterial::TFaceCulling FaceCulling;
	int mathash;
};


class DELPHICLASS TGLLMTSVectorFile;
class PASCALIMPLEMENTATION TGLLMTSVectorFile : public Glvectorfileobjects::TVectorFile
{
	typedef Glvectorfileobjects::TVectorFile inherited;
	
public:
	__classmethod virtual Applicationfileio::TDataFileCapabilities __fastcall Capabilities();
	DYNAMIC void __fastcall LoadFromStream(System::Classes::TStream* aStream);
	DYNAMIC void __fastcall SaveToStream(System::Classes::TStream* aStream);
public:
	/* TVectorFile.Create */ inline __fastcall virtual TGLLMTSVectorFile(System::Classes::TPersistent* AOwner) : Glvectorfileobjects::TVectorFile(AOwner) { }
	
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TGLLMTSVectorFile(void) { }
	
};


//-- var, const, procedure ---------------------------------------------------
static const int C_LMTS_ID = int(0x53544d4c);
static const System::Int8 C_LMTS_VER = System::Int8(0x4);
static const int C_LMTS_SUBS = int(0x53425553);
static const int C_LMTS_TEXT = int(0x54584554);
static const int C_LMTS_TRIS = int(0x53495254);
static const System::Byte C_LMTS_TEXFNLEN = System::Byte(0xff);
}	/* namespace Glfilelmts */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLFILELMTS)
using namespace Glfilelmts;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// GlfilelmtsHPP
