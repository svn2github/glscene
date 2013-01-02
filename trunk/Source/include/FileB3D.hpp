// CodeGear C++Builder
// Copyright (c) 1995, 2012 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'FileB3D.pas' rev: 24.00 (Win32)

#ifndef Fileb3dHPP
#define Fileb3dHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <System.Classes.hpp>	// Pascal unit
#include <TypesB3D.hpp>	// Pascal unit
#include <VectorGeometry.hpp>	// Pascal unit
#include <VectorTypes.hpp>	// Pascal unit
#include <VectorLists.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Fileb3d
{
//-- type declarations -------------------------------------------------------
class DELPHICLASS TB3DMaterial;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TB3DMaterial : public System::TObject
{
	typedef System::TObject inherited;
	
public:
	Typesb3d::TBRUSChunk MaterialData;
	__fastcall TB3DMaterial(void);
	__fastcall virtual ~TB3DMaterial(void);
	System::UnicodeString __fastcall GetMaterialName(void);
};

#pragma pack(pop)

class DELPHICLASS TB3DTexture;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TB3DTexture : public System::TObject
{
	typedef System::TObject inherited;
	
public:
	Typesb3d::TTEXSChunk TextureData;
	__fastcall TB3DTexture(void);
	__fastcall virtual ~TB3DTexture(void);
	System::UnicodeString __fastcall GetTextureName(void);
};

#pragma pack(pop)

class DELPHICLASS TB3DNode;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TB3DNode : public System::TObject
{
	typedef System::TObject inherited;
	
public:
	Typesb3d::TNODEChunk *NodeData;
	__fastcall TB3DNode(void);
	__fastcall virtual ~TB3DNode(void);
	System::UnicodeString __fastcall GetNodeName(void);
	void __fastcall DestroyNodeData(Typesb3d::PNODEChunk Node);
};

#pragma pack(pop)

class DELPHICLASS TFileB3D;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TFileB3D : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	System::Classes::TStringList* fTextures;
	System::Classes::TStringList* fMaterials;
	TB3DNode* fNodes;
	void __fastcall FreeLists(void);
	Typesb3d::TB3DChunkType __fastcall GetChunkType(const Typesb3d::TB3DChunk &aChunk);
	int __fastcall SkipChunk(System::Classes::TStream* aStream, const Typesb3d::TB3DChunk &aChunk);
	int __fastcall ReadTextureChunk(System::Classes::TStream* aStream, const Typesb3d::TB3DChunk &aChunk);
	int __fastcall ReadMaterialChunk(System::Classes::TStream* aStream, const Typesb3d::TB3DChunk &aChunk);
	int __fastcall ReadNodeChunk(System::Classes::TStream* aStream, const Typesb3d::TB3DChunk &aChunk, Typesb3d::PNODEChunk Node, int level);
	int __fastcall ReadMeshChunk(System::Classes::TStream* aStream, const Typesb3d::TB3DChunk &aChunk, Typesb3d::PMESHChunk Mesh);
	int __fastcall ReadVerticesChunk(System::Classes::TStream* aStream, const Typesb3d::TB3DChunk &aChunk, Typesb3d::PVRTSChunk Vertices);
	int __fastcall ReadTrianglesChunk(System::Classes::TStream* aStream, const Typesb3d::TB3DChunk &aChunk, Typesb3d::PTRISChunk Triangle);
	
public:
	__fastcall virtual TFileB3D(void);
	__fastcall virtual ~TFileB3D(void);
	void __fastcall LoadFromStream(System::Classes::TStream* aStream);
	void __fastcall Check(void);
	__property System::Classes::TStringList* Textures = {read=fTextures};
	__property System::Classes::TStringList* Materials = {read=fMaterials};
	__property TB3DNode* Nodes = {read=fNodes};
};

#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------
}	/* namespace Fileb3d */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_FILEB3D)
using namespace Fileb3d;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Fileb3dHPP
