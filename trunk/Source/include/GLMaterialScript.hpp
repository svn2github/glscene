// CodeGear C++Builder
// Copyright (c) 1995, 2012 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLMaterialScript.pas' rev: 24.00 (Win32)

#ifndef GlmaterialscriptHPP
#define GlmaterialscriptHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <System.SysUtils.hpp>	// Pascal unit
#include <System.Classes.hpp>	// Pascal unit
#include <Vcl.StdCtrls.hpp>	// Pascal unit
#include <GLTexture.hpp>	// Pascal unit
#include <GLTextureFormat.hpp>	// Pascal unit
#include <GLGraphics.hpp>	// Pascal unit
#include <GLUtils.hpp>	// Pascal unit
#include <GLColor.hpp>	// Pascal unit
#include <GLCoordinates.hpp>	// Pascal unit
#include <GLMaterial.hpp>	// Pascal unit
#include <GLState.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Glmaterialscript
{
//-- type declarations -------------------------------------------------------
class DELPHICLASS TGLShaderItem;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLShaderItem : public System::Classes::TCollectionItem
{
	typedef System::Classes::TCollectionItem inherited;
	
private:
	Glmaterial::TGLShader* FShader;
	System::UnicodeString FName;
	void __fastcall SetShader(Glmaterial::TGLShader* const Value);
	void __fastcall SetName(const System::UnicodeString Value);
	
protected:
	virtual System::UnicodeString __fastcall GetDisplayName(void);
	
public:
	__fastcall virtual TGLShaderItem(System::Classes::TCollection* Collection);
	__fastcall virtual ~TGLShaderItem(void);
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	
__published:
	__property Glmaterial::TGLShader* Shader = {read=FShader, write=SetShader};
	__property System::UnicodeString Name = {read=FName, write=SetName};
};

#pragma pack(pop)

class DELPHICLASS TGLShaderItems;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLShaderItems : public System::Classes::TOwnedCollection
{
	typedef System::Classes::TOwnedCollection inherited;
	
public:
	TGLShaderItem* operator[](int Index) { return Items[Index]; }
	
private:
	void __fastcall SetItems(int Index, TGLShaderItem* const Val);
	TGLShaderItem* __fastcall GetItems(int Index);
	
public:
	__fastcall TGLShaderItems(System::Classes::TPersistent* AOwner);
	__property TGLShaderItem* Items[int Index] = {read=GetItems, write=SetItems/*, default*/};
public:
	/* TCollection.Destroy */ inline __fastcall virtual ~TGLShaderItems(void) { }
	
};

#pragma pack(pop)

class DELPHICLASS TGLMaterialLibraryItem;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLMaterialLibraryItem : public System::Classes::TCollectionItem
{
	typedef System::Classes::TCollectionItem inherited;
	
private:
	Glmaterial::TGLMaterialLibrary* FMaterialLibrary;
	System::UnicodeString FName;
	void __fastcall SetMaterialLibrary(Glmaterial::TGLMaterialLibrary* const Value);
	void __fastcall SetName(const System::UnicodeString Value);
	
protected:
	virtual System::UnicodeString __fastcall GetDisplayName(void);
	
public:
	__fastcall virtual TGLMaterialLibraryItem(System::Classes::TCollection* Collection);
	__fastcall virtual ~TGLMaterialLibraryItem(void);
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	
__published:
	__property Glmaterial::TGLMaterialLibrary* MaterialLibrary = {read=FMaterialLibrary, write=SetMaterialLibrary};
	__property System::UnicodeString Name = {read=FName, write=SetName};
};

#pragma pack(pop)

class DELPHICLASS TGLMaterialLibraryItems;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLMaterialLibraryItems : public System::Classes::TOwnedCollection
{
	typedef System::Classes::TOwnedCollection inherited;
	
public:
	TGLMaterialLibraryItem* operator[](int Index) { return Items[Index]; }
	
private:
	void __fastcall SetItems(int Index, TGLMaterialLibraryItem* const Val);
	TGLMaterialLibraryItem* __fastcall GetItems(int Index);
	
public:
	__fastcall TGLMaterialLibraryItems(System::Classes::TPersistent* AOwner);
	__property TGLMaterialLibraryItem* Items[int Index] = {read=GetItems, write=SetItems/*, default*/};
public:
	/* TCollection.Destroy */ inline __fastcall virtual ~TGLMaterialLibraryItems(void) { }
	
};

#pragma pack(pop)

class DELPHICLASS TGLMaterialScripter;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLMaterialScripter : public System::Classes::TComponent
{
	typedef System::Classes::TComponent inherited;
	
private:
	TGLShaderItems* FShaderItems;
	TGLMaterialLibraryItems* FMaterialLibraryItems;
	bool FAppend;
	bool FOverwrite;
	System::Classes::TStrings* FScript;
	Vcl::Stdctrls::TMemo* FMemo;
	Glmaterial::TGLMaterialLibrary* FMaterialLibrary;
	int Count;
	int infini;
	bool done;
	Glmaterial::TGLLibMaterial* NewMat;
	Glcoordinates::TGLCoordinates3* tmpcoords;
	Glcolor::TGLColor* tmpcolor;
	Glcoordinates::TGLCoordinates4* tmpcoords4;
	System::UnicodeString tmpstr;
	void __fastcall SeTGLShaderItems(TGLShaderItems* const Value);
	void __fastcall SeTGLMaterialLibraryItems(TGLMaterialLibraryItems* const Value);
	void __fastcall SetAppend(const bool Value);
	void __fastcall SetOverwrite(const bool Value);
	void __fastcall SetScript(System::Classes::TStrings* const Value);
	void __fastcall SetMaterialLibrary(Glmaterial::TGLMaterialLibrary* const Value);
	void __fastcall SetMemo(Vcl::Stdctrls::TMemo* const Value);
	void __fastcall CheckError(void);
	bool __fastcall ClassExists(System::UnicodeString arguement);
	bool __fastcall CheckRepeatDone(void);
	System::UnicodeString __fastcall ExtractValue(void);
	void __fastcall ExtractCoords3(void);
	void __fastcall ExtractCoords4(void);
	void __fastcall ExtractColors(void);
	System::UnicodeString __fastcall DeleteSpaces(System::UnicodeString Value);
	bool __fastcall SubstrExists(System::UnicodeString substr);
	bool __fastcall ValueExists(System::UnicodeString Value);
	void __fastcall ZMaterial(void);
	void __fastcall XMaterial(void);
	void __fastcall XName(void);
	void __fastcall XShader(void);
	void __fastcall XTexture2Name(void);
	void __fastcall XTextureOffset(void);
	void __fastcall XTextureScale(void);
	void __fastcall XTexture(void);
	void __fastcall XCompression(void);
	void __fastcall XEnvColor(void);
	void __fastcall XFilteringQuality(void);
	void __fastcall XImageAlpha(void);
	void __fastcall XImageBrightness(void);
	void __fastcall XImageClass(void);
	void __fastcall XImageGamma(void);
	void __fastcall XMagFilter(void);
	void __fastcall XMappingMode(void);
	void __fastcall XMappingSCoordinates(void);
	void __fastcall XMappingTCoordinates(void);
	void __fastcall XMinFilter(void);
	void __fastcall XNormalMapScale(void);
	void __fastcall XTextureFormat(void);
	void __fastcall XTextureMode(void);
	void __fastcall XTextureWrap(void);
	void __fastcall XBlendingMode(void);
	void __fastcall XPolygonMode(void);
	void __fastcall XFacingCulling(void);
	void __fastcall XLibMaterialName(void);
	void __fastcall XMaterialOptions(void);
	void __fastcall XMaterialLibrary(void);
	void __fastcall XBackProperties(void);
	void __fastcall XBackAmbient(void);
	void __fastcall XBackDiffuse(void);
	void __fastcall XBackEmission(void);
	void __fastcall XBackShininess(void);
	void __fastcall XBackSpecular(void);
	void __fastcall XFrontProperties(void);
	void __fastcall XFrontAmbient(void);
	void __fastcall XFrontDiffuse(void);
	void __fastcall XFrontEmission(void);
	void __fastcall XFrontShininess(void);
	void __fastcall XFrontSpecular(void);
	void __fastcall XPersistantImage(void);
	void __fastcall XBlankImage(void);
	void __fastcall XPictureFileName(void);
	void __fastcall XPicturePX(void);
	void __fastcall XPictureNX(void);
	void __fastcall XPicturePY(void);
	void __fastcall XPictureNY(void);
	void __fastcall XPicturePZ(void);
	void __fastcall XPictureNZ(void);
	
protected:
	virtual void __fastcall Notification(System::Classes::TComponent* AComponent, System::Classes::TOperation Operation);
	
public:
	__property Vcl::Stdctrls::TMemo* DebugMemo = {read=FMemo, write=SetMemo};
	__fastcall virtual TGLMaterialScripter(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TGLMaterialScripter(void);
	void __fastcall CompileScript(void);
	
__published:
	__property System::Classes::TStrings* Script = {read=FScript, write=SetScript};
	__property Glmaterial::TGLMaterialLibrary* MaterialLibrary = {read=FMaterialLibrary, write=SetMaterialLibrary};
	__property TGLShaderItems* Shaders = {read=FShaderItems, write=SeTGLShaderItems};
	__property TGLMaterialLibraryItems* MaterialLibraries = {read=FMaterialLibraryItems, write=SeTGLMaterialLibraryItems};
	__property bool AppendToMaterialLibrary = {read=FAppend, write=SetAppend, nodefault};
	__property bool OverwriteToMaterialLibrary = {read=FOverwrite, write=SetOverwrite, nodefault};
};

#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------
}	/* namespace Glmaterialscript */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLMATERIALSCRIPT)
using namespace Glmaterialscript;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// GlmaterialscriptHPP
