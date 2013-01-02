// CodeGear C++Builder
// Copyright (c) 1995, 2012 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLTextureSharingShader.pas' rev: 24.00 (Win32)

#ifndef GltexturesharingshaderHPP
#define GltexturesharingshaderHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <System.Classes.hpp>	// Pascal unit
#include <System.SysUtils.hpp>	// Pascal unit
#include <GLScene.hpp>	// Pascal unit
#include <VectorGeometry.hpp>	// Pascal unit
#include <GLColor.hpp>	// Pascal unit
#include <GLMaterial.hpp>	// Pascal unit
#include <GLStrings.hpp>	// Pascal unit
#include <GLVectorFileObjects.hpp>	// Pascal unit
#include <XOpenGL.hpp>	// Pascal unit
#include <GLState.hpp>	// Pascal unit
#include <PersistentClasses.hpp>	// Pascal unit
#include <GLCrossPlatform.hpp>	// Pascal unit
#include <GLCoordinates.hpp>	// Pascal unit
#include <GLRenderContextInfo.hpp>	// Pascal unit
#include <VectorTypes.hpp>	// Pascal unit
#include <BaseClasses.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Gltexturesharingshader
{
//-- type declarations -------------------------------------------------------
class DELPHICLASS TGLTextureSharingShaderMaterial;
class DELPHICLASS TGLTextureSharingShader;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLTextureSharingShaderMaterial : public Persistentclasses::TGLInterfacedCollectionItem
{
	typedef Persistentclasses::TGLInterfacedCollectionItem inherited;
	
private:
	Vectortypes::TMatrix4f FTextureMatrix;
	bool FNeedToUpdateTextureMatrix;
	bool FTextureMatrixIsUnitary;
	Glmaterial::TGLLibMaterial* FLibMaterial;
	Glcoordinates::TGLCoordinates2* FTexOffset;
	Glcoordinates::TGLCoordinates2* FTexScale;
	Glmaterial::TBlendingMode FBlendingMode;
	Glcolor::TGLColor* FSpecular;
	Glcolor::TGLColor* FAmbient;
	Glcolor::TGLColor* FDiffuse;
	Glcolor::TGLColor* FEmission;
	Glmaterial::TShininess FShininess;
	Glmaterial::TGLMaterialLibrary* FMaterialLibrary;
	System::UnicodeString FLibMaterialName;
	void __fastcall SetAmbient(Glcolor::TGLColor* const Value);
	void __fastcall SetDiffuse(Glcolor::TGLColor* const Value);
	void __fastcall SetEmission(Glcolor::TGLColor* const Value);
	void __fastcall SetShininess(const Glmaterial::TShininess Value);
	void __fastcall SetSpecular(Glcolor::TGLColor* const Value);
	void __fastcall SetMaterialLibrary(Glmaterial::TGLMaterialLibrary* const Value);
	void __fastcall SetLibMaterialName(const System::UnicodeString Value);
	void __fastcall SetBlendingMode(const Glmaterial::TBlendingMode Value);
	void __fastcall SetLibMaterial(Glmaterial::TGLLibMaterial* const Value);
	void __fastcall SetTexOffset(Glcoordinates::TGLCoordinates2* const Value);
	void __fastcall SetTexScale(Glcoordinates::TGLCoordinates2* const Value);
	Vectortypes::TMatrix4f __fastcall GetTextureMatrix(void);
	bool __fastcall GetTextureMatrixIsUnitary(void);
	
protected:
	void __fastcall coordNotifychange(System::TObject* Sender);
	void __fastcall OtherNotifychange(System::TObject* Sender);
	virtual System::UnicodeString __fastcall GetDisplayName(void);
	TGLTextureSharingShader* __fastcall GetTextureSharingShader(void);
	virtual Glmaterial::TGLAbstractMaterialLibrary* __fastcall GetMaterialLibrary(void);
	
public:
	void __fastcall Apply(Glrendercontextinfo::TRenderContextInfo &rci);
	void __fastcall UnApply(Glrendercontextinfo::TRenderContextInfo &rci);
	__fastcall virtual TGLTextureSharingShaderMaterial(System::Classes::TCollection* Collection);
	__fastcall virtual ~TGLTextureSharingShaderMaterial(void);
	__property Glmaterial::TGLLibMaterial* LibMaterial = {read=FLibMaterial, write=SetLibMaterial};
	__property Vectortypes::TMatrix4f TextureMatrix = {read=GetTextureMatrix};
	__property bool TextureMatrixIsUnitary = {read=GetTextureMatrixIsUnitary, nodefault};
	
__published:
	__property Glcoordinates::TGLCoordinates2* TexOffset = {read=FTexOffset, write=SetTexOffset};
	__property Glcoordinates::TGLCoordinates2* TexScale = {read=FTexScale, write=SetTexScale};
	__property Glmaterial::TBlendingMode BlendingMode = {read=FBlendingMode, write=SetBlendingMode, nodefault};
	__property Glcolor::TGLColor* Emission = {read=FEmission, write=SetEmission};
	__property Glcolor::TGLColor* Ambient = {read=FAmbient, write=SetAmbient};
	__property Glcolor::TGLColor* Diffuse = {read=FDiffuse, write=SetDiffuse};
	__property Glcolor::TGLColor* Specular = {read=FSpecular, write=SetSpecular};
	__property Glmaterial::TShininess Shininess = {read=FShininess, write=SetShininess, nodefault};
	__property Glmaterial::TGLMaterialLibrary* MaterialLibrary = {read=FMaterialLibrary, write=SetMaterialLibrary};
	__property System::UnicodeString LibMaterialName = {read=FLibMaterialName, write=SetLibMaterialName};
private:
	void *__IGLMaterialLibrarySupported;	/* Glmaterial::IGLMaterialLibrarySupported */
	
public:
	#if defined(MANAGED_INTERFACE_OPERATORS)
	// {8E442AF9-D212-4A5E-8A88-92F798BABFD1}
	operator Glmaterial::_di_IGLMaterialLibrarySupported()
	{
		Glmaterial::_di_IGLMaterialLibrarySupported intf;
		GetInterface(intf);
		return intf;
	}
	#else
	operator Glmaterial::IGLMaterialLibrarySupported*(void) { return (Glmaterial::IGLMaterialLibrarySupported*)&__IGLMaterialLibrarySupported; }
	#endif
	
};

#pragma pack(pop)

class DELPHICLASS TGLTextureSharingShaderMaterials;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLTextureSharingShaderMaterials : public System::Classes::TOwnedCollection
{
	typedef System::Classes::TOwnedCollection inherited;
	
public:
	TGLTextureSharingShaderMaterial* operator[](const int AIndex) { return Items[AIndex]; }
	
protected:
	TGLTextureSharingShaderMaterial* __fastcall GetItems(const int AIndex);
	void __fastcall SetItems(const int AIndex, TGLTextureSharingShaderMaterial* const Value);
	TGLTextureSharingShader* __fastcall GetParent(void);
	
public:
	HIDESBASE TGLTextureSharingShaderMaterial* __fastcall Add(void);
	__fastcall TGLTextureSharingShaderMaterials(TGLTextureSharingShader* AOwner);
	__property TGLTextureSharingShaderMaterial* Items[const int AIndex] = {read=GetItems, write=SetItems/*, default*/};
public:
	/* TCollection.Destroy */ inline __fastcall virtual ~TGLTextureSharingShaderMaterials(void) { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLTextureSharingShader : public Glmaterial::TGLShader
{
	typedef Glmaterial::TGLShader inherited;
	
private:
	TGLTextureSharingShaderMaterials* FMaterials;
	int FCurrentPass;
	void __fastcall SetMaterials(TGLTextureSharingShaderMaterials* const Value);
	
protected:
	virtual void __fastcall DoApply(Glrendercontextinfo::TRenderContextInfo &rci, System::TObject* Sender);
	virtual bool __fastcall DoUnApply(Glrendercontextinfo::TRenderContextInfo &rci);
	virtual void __fastcall Notification(System::Classes::TComponent* AComponent, System::Classes::TOperation Operation);
	
public:
	__fastcall virtual TGLTextureSharingShader(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TGLTextureSharingShader(void);
	TGLTextureSharingShaderMaterial* __fastcall AddLibMaterial(Glmaterial::TGLLibMaterial* const ALibMaterial);
	TGLTextureSharingShaderMaterial* __fastcall FindLibMaterial(Glmaterial::TGLLibMaterial* const ALibMaterial);
	
__published:
	__property TGLTextureSharingShaderMaterials* Materials = {read=FMaterials, write=SetMaterials};
};

#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------
}	/* namespace Gltexturesharingshader */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLTEXTURESHARINGSHADER)
using namespace Gltexturesharingshader;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// GltexturesharingshaderHPP
