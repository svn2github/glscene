// CodeGear C++Builder
// Copyright (c) 1995, 2012 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLTexCombineShader.pas' rev: 24.00 (Win32)

#ifndef GltexcombineshaderHPP
#define GltexcombineshaderHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <System.Classes.hpp>	// Pascal unit
#include <GLTexture.hpp>	// Pascal unit
#include <GLMaterial.hpp>	// Pascal unit
#include <GLRenderContextInfo.hpp>	// Pascal unit
#include <GLTextureCombiners.hpp>	// Pascal unit
#include <BaseClasses.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Gltexcombineshader
{
//-- type declarations -------------------------------------------------------
class DELPHICLASS TGLTexCombineShader;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLTexCombineShader : public Glmaterial::TGLShader
{
	typedef Glmaterial::TGLShader inherited;
	
private:
	System::Classes::TStringList* FCombiners;
	Gltexturecombiners::TCombinerCache FCommandCache;
	bool FCombinerIsValid;
	bool FDesignTimeEnabled;
	Glmaterial::TGLMaterialLibrary* FMaterialLibrary;
	System::UnicodeString FLibMaterial3Name;
	Glmaterial::TGLLibMaterial* currentLibMaterial3;
	System::UnicodeString FLibMaterial4Name;
	Glmaterial::TGLLibMaterial* currentLibMaterial4;
	bool FApplied3;
	bool FApplied4;
	
protected:
	void __fastcall SetCombiners(System::Classes::TStringList* const val);
	void __fastcall SetDesignTimeEnabled(const bool val);
	void __fastcall SetMaterialLibrary(Glmaterial::TGLMaterialLibrary* const val);
	void __fastcall SetLibMaterial3Name(const System::UnicodeString val);
	void __fastcall SetLibMaterial4Name(const System::UnicodeString val);
	void __fastcall NotifyLibMaterial3Destruction(void);
	void __fastcall NotifyLibMaterial4Destruction(void);
	DYNAMIC void __fastcall DoInitialize(Glrendercontextinfo::TRenderContextInfo &rci, System::TObject* Sender);
	virtual void __fastcall DoApply(Glrendercontextinfo::TRenderContextInfo &rci, System::TObject* Sender);
	virtual bool __fastcall DoUnApply(Glrendercontextinfo::TRenderContextInfo &rci);
	DYNAMIC void __fastcall DoFinalize(void);
	
public:
	__fastcall virtual TGLTexCombineShader(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TGLTexCombineShader(void);
	virtual void __fastcall Notification(System::Classes::TComponent* AComponent, System::Classes::TOperation Operation);
	virtual void __fastcall NotifyChange(System::TObject* Sender);
	
__published:
	__property System::Classes::TStringList* Combiners = {read=FCombiners, write=SetCombiners};
	__property bool DesignTimeEnabled = {read=FDesignTimeEnabled, write=SetDesignTimeEnabled, nodefault};
	__property Glmaterial::TGLMaterialLibrary* MaterialLibrary = {read=FMaterialLibrary, write=SetMaterialLibrary};
	__property System::UnicodeString LibMaterial3Name = {read=FLibMaterial3Name, write=SetLibMaterial3Name};
	__property System::UnicodeString LibMaterial4Name = {read=FLibMaterial4Name, write=SetLibMaterial4Name};
};

#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------
}	/* namespace Gltexcombineshader */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLTEXCOMBINESHADER)
using namespace Gltexcombineshader;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// GltexcombineshaderHPP
