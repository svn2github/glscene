// CodeGear C++Builder
// Copyright (c) 1995, 2012 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'VRMLParser.pas' rev: 24.00 (Win32)

#ifndef VrmlparserHPP
#define VrmlparserHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <System.Classes.hpp>	// Pascal unit
#include <System.SysUtils.hpp>	// Pascal unit
#include <VectorTypes.hpp>	// Pascal unit
#include <VectorLists.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Vrmlparser
{
//-- type declarations -------------------------------------------------------
class DELPHICLASS TVRMLNode;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TVRMLNode : public System::TObject
{
	typedef System::TObject inherited;
	
public:
	TVRMLNode* operator[](int index) { return Nodes[index]; }
	
private:
	System::Classes::TList* FNodes;
	TVRMLNode* FParent;
	System::UnicodeString FName;
	System::UnicodeString FDefName;
	TVRMLNode* __fastcall GetNode(int index);
	
public:
	__fastcall virtual TVRMLNode(void);
	__fastcall TVRMLNode(TVRMLNode* AParent);
	__fastcall virtual ~TVRMLNode(void);
	int __fastcall Count(void);
	void __fastcall Clear(void);
	void __fastcall Add(TVRMLNode* node);
	void __fastcall Remove(TVRMLNode* node);
	void __fastcall Delete(int index);
	__property TVRMLNode* Nodes[int index] = {read=GetNode/*, default*/};
	__property TVRMLNode* Parent = {read=FParent};
	__property System::UnicodeString Name = {read=FName, write=FName};
	__property System::UnicodeString DefName = {read=FDefName, write=FDefName};
};

#pragma pack(pop)

class DELPHICLASS TVRMLSingleArray;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TVRMLSingleArray : public TVRMLNode
{
	typedef TVRMLNode inherited;
	
private:
	Vectorlists::TSingleList* FValues;
	
public:
	__fastcall virtual TVRMLSingleArray(void);
	__fastcall virtual ~TVRMLSingleArray(void);
	__property Vectorlists::TSingleList* Values = {read=FValues};
public:
	/* TVRMLNode.CreateOwned */ inline __fastcall TVRMLSingleArray(TVRMLNode* AParent) : TVRMLNode(AParent) { }
	
};

#pragma pack(pop)

class DELPHICLASS TVRMLIntegerArray;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TVRMLIntegerArray : public TVRMLNode
{
	typedef TVRMLNode inherited;
	
private:
	Vectorlists::TIntegerList* FValues;
	
public:
	__fastcall virtual TVRMLIntegerArray(void);
	__fastcall virtual ~TVRMLIntegerArray(void);
	__property Vectorlists::TIntegerList* Values = {read=FValues};
public:
	/* TVRMLNode.CreateOwned */ inline __fastcall TVRMLIntegerArray(TVRMLNode* AParent) : TVRMLNode(AParent) { }
	
};

#pragma pack(pop)

class DELPHICLASS TVRMLMaterial;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TVRMLMaterial : public TVRMLNode
{
	typedef TVRMLNode inherited;
	
private:
	Vectortypes::TVector3f FDiffuseColor;
	Vectortypes::TVector3f FAmbientColor;
	Vectortypes::TVector3f FSpecularColor;
	Vectortypes::TVector3f FEmissiveColor;
	float FTransparency;
	float FShininess;
	bool FHasDiffuse;
	bool FHasAmbient;
	bool FHasSpecular;
	bool FHasEmissive;
	bool FHasTransparency;
	bool FHasShininess;
	
public:
	__fastcall virtual TVRMLMaterial(void);
	__property Vectortypes::TVector3f DiffuseColor = {read=FDiffuseColor, write=FDiffuseColor};
	__property Vectortypes::TVector3f AmbientColor = {read=FAmbientColor, write=FAmbientColor};
	__property Vectortypes::TVector3f SpecularColor = {read=FSpecularColor, write=FSpecularColor};
	__property Vectortypes::TVector3f EmissiveColor = {read=FEmissiveColor, write=FEmissiveColor};
	__property float Transparency = {read=FTransparency, write=FTransparency};
	__property float Shininess = {read=FShininess, write=FShininess};
	__property bool HasDiffuse = {read=FHasDiffuse, write=FHasDiffuse, nodefault};
	__property bool HasAmbient = {read=FHasAmbient, write=FHasAmbient, nodefault};
	__property bool HasSpecular = {read=FHasSpecular, write=FHasSpecular, nodefault};
	__property bool HasEmissive = {read=FHasEmissive, write=FHasEmissive, nodefault};
	__property bool HasTransparency = {read=FHasTransparency, write=FHasTransparency, nodefault};
	__property bool HasShininess = {read=FHasShininess, write=FHasShininess, nodefault};
public:
	/* TVRMLNode.CreateOwned */ inline __fastcall TVRMLMaterial(TVRMLNode* AParent) : TVRMLNode(AParent) { }
	/* TVRMLNode.Destroy */ inline __fastcall virtual ~TVRMLMaterial(void) { }
	
};

#pragma pack(pop)

class DELPHICLASS TVRMLUse;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TVRMLUse : public TVRMLNode
{
	typedef TVRMLNode inherited;
	
private:
	System::UnicodeString FValue;
	
public:
	__property System::UnicodeString Value = {read=FValue, write=FValue};
public:
	/* TVRMLNode.Create */ inline __fastcall virtual TVRMLUse(void) : TVRMLNode() { }
	/* TVRMLNode.CreateOwned */ inline __fastcall TVRMLUse(TVRMLNode* AParent) : TVRMLNode(AParent) { }
	/* TVRMLNode.Destroy */ inline __fastcall virtual ~TVRMLUse(void) { }
	
};

#pragma pack(pop)

class DELPHICLASS TVRMLShapeHints;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TVRMLShapeHints : public TVRMLNode
{
	typedef TVRMLNode inherited;
	
private:
	float FCreaseAngle;
	
public:
	__property float CreaseAngle = {read=FCreaseAngle, write=FCreaseAngle};
public:
	/* TVRMLNode.Create */ inline __fastcall virtual TVRMLShapeHints(void) : TVRMLNode() { }
	/* TVRMLNode.CreateOwned */ inline __fastcall TVRMLShapeHints(TVRMLNode* AParent) : TVRMLNode(AParent) { }
	/* TVRMLNode.Destroy */ inline __fastcall virtual ~TVRMLShapeHints(void) { }
	
};

#pragma pack(pop)

class DELPHICLASS TVRMLTransform;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TVRMLTransform : public TVRMLNode
{
	typedef TVRMLNode inherited;
	
private:
	Vectortypes::TVector3f FCenter;
	Vectortypes::TVector4f FRotation;
	Vectortypes::TVector3f FScaleFactor;
	
public:
	__fastcall virtual TVRMLTransform(void);
	__property Vectortypes::TVector3f Center = {read=FCenter, write=FCenter};
	__property Vectortypes::TVector4f Rotation = {read=FRotation, write=FRotation};
	__property Vectortypes::TVector3f ScaleFactor = {read=FScaleFactor, write=FScaleFactor};
public:
	/* TVRMLNode.CreateOwned */ inline __fastcall TVRMLTransform(TVRMLNode* AParent) : TVRMLNode(AParent) { }
	/* TVRMLNode.Destroy */ inline __fastcall virtual ~TVRMLTransform(void) { }
	
};

#pragma pack(pop)

class DELPHICLASS TVRMLParser;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TVRMLParser : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	int FCursor;
	System::Classes::TStringList* FTokens;
	TVRMLNode* FRootNode;
	TVRMLNode* FCurrentNode;
	bool FAllowUnknownNodes;
	System::Classes::TList* FDefines;
	
protected:
	System::UnicodeString __fastcall ReadToken(void);
	float __fastcall ReadSingle(void);
	Vectortypes::TVector3f __fastcall ReadVector3f(void);
	Vectortypes::TVector4f __fastcall ReadVector4f(void);
	void __fastcall ReadUnknownArray(System::UnicodeString defname = System::UnicodeString());
	void __fastcall ReadUnknownHeirachy(System::UnicodeString defname = System::UnicodeString());
	void __fastcall ReadUnknown(System::UnicodeString unknown_token, System::UnicodeString defname = System::UnicodeString());
	void __fastcall ReadPointArray(System::UnicodeString defname = System::UnicodeString());
	void __fastcall ReadCoordIndexArray(System::UnicodeString defname = System::UnicodeString());
	void __fastcall ReadNormalIndexArray(System::UnicodeString defname = System::UnicodeString());
	void __fastcall ReadTextureCoordIndexArray(System::UnicodeString defname = System::UnicodeString());
	void __fastcall ReadCoordinate3(System::UnicodeString defname = System::UnicodeString());
	void __fastcall ReadNormal(System::UnicodeString defname = System::UnicodeString());
	void __fastcall ReadTextureCoordinate2(System::UnicodeString defname = System::UnicodeString());
	void __fastcall ReadMaterial(System::UnicodeString defname = System::UnicodeString());
	void __fastcall ReadIndexedFaceSet(System::UnicodeString defname = System::UnicodeString());
	void __fastcall ReadTransform(System::UnicodeString defname = System::UnicodeString());
	void __fastcall ReadShapeHints(System::UnicodeString defname = System::UnicodeString());
	void __fastcall ReadSeparator(System::UnicodeString defname = System::UnicodeString());
	void __fastcall ReadGroup(System::UnicodeString defname = System::UnicodeString());
	void __fastcall ReadDef(void);
	void __fastcall ReadUse(void);
	
public:
	__fastcall TVRMLParser(void);
	__fastcall virtual ~TVRMLParser(void);
	void __fastcall Parse(System::UnicodeString Text);
	__property TVRMLNode* RootNode = {read=FRootNode};
	__property bool AllowUnknownNodes = {read=FAllowUnknownNodes, write=FAllowUnknownNodes, nodefault};
};

#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------
}	/* namespace Vrmlparser */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_VRMLPARSER)
using namespace Vrmlparser;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// VrmlparserHPP
