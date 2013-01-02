// CodeGear C++Builder
// Copyright (c) 1995, 2012 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLSceneFormDesign.pas' rev: 24.00 (Win32)

#ifndef GlsceneformdesignHPP
#define GlsceneformdesignHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <Vcl.Forms.hpp>	// Pascal unit
#include <Winapi.Windows.hpp>	// Pascal unit
#include <System.Classes.hpp>	// Pascal unit
#include <ToolsAPI.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Glsceneformdesign
{
//-- type declarations -------------------------------------------------------
class DELPHICLASS TGLBaseSceneFormWizard;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLBaseSceneFormWizard : public Toolsapi::TNotifierObject
{
	typedef Toolsapi::TNotifierObject inherited;
	
private:
	System::UnicodeString FUnitIdent;
	System::UnicodeString FClassName;
	System::UnicodeString FFileName;
	
protected:
	virtual System::UnicodeString __fastcall GetIDString(void);
	virtual System::UnicodeString __fastcall GetName(void);
	Toolsapi::TWizardState __fastcall GetState(void);
	virtual void __fastcall Execute(void);
	System::UnicodeString __fastcall GetAuthor(void);
	System::UnicodeString __fastcall GetComment(void);
	System::UnicodeString __fastcall GetPage(void);
	unsigned __fastcall GetGlyph(void);
	System::UnicodeString __fastcall GetCreatorType(void);
	bool __fastcall GetExisting(void);
	System::UnicodeString __fastcall GetFileSystem(void);
	Toolsapi::_di_IOTAModule __fastcall GetOwner(void);
	bool __fastcall GetUnnamed(void);
	System::UnicodeString __fastcall GetAncestorName(void);
	System::UnicodeString __fastcall GetImplFileName(void);
	System::UnicodeString __fastcall GetIntfFileName(void);
	System::UnicodeString __fastcall GetFormName(void);
	bool __fastcall GetMainForm(void);
	bool __fastcall GetShowForm(void);
	bool __fastcall GetShowSource(void);
	virtual Toolsapi::_di_IOTAFile __fastcall NewFormFile(const System::UnicodeString FormIdent, const System::UnicodeString AncestorIdent);
	virtual Toolsapi::_di_IOTAFile __fastcall NewImplSource(const System::UnicodeString ModuleIdent, const System::UnicodeString FormIdent, const System::UnicodeString AncestorIdent);
	Toolsapi::_di_IOTAFile __fastcall NewIntfSource(const System::UnicodeString ModuleIdent, const System::UnicodeString FormIdent, const System::UnicodeString AncestorIdent);
	void __fastcall FormCreated(const Toolsapi::_di_IOTAFormEditor FormEditor);
	
public:
	__fastcall TGLBaseSceneFormWizard(const System::UnicodeString AUnitIdent, const System::UnicodeString AClassName, const System::UnicodeString AFileName);
	System::UnicodeString __fastcall GetDesigner(void);
	Toolsapi::_di_IOTAGalleryCategory __fastcall GetGalleryCategory(void);
	System::UnicodeString __fastcall GetPersonality(void);
public:
	/* TObject.Create */ inline __fastcall TGLBaseSceneFormWizard(void) : Toolsapi::TNotifierObject() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TGLBaseSceneFormWizard(void) { }
	
private:
	void *__IOTARepositoryWizard80;	/* Toolsapi::IOTARepositoryWizard80 */
	void *__IOTAModuleCreator;	/* Toolsapi::IOTAModuleCreator */
	void *__IOTAFormWizard;	/* Toolsapi::IOTAFormWizard */
	
public:
	#if defined(MANAGED_INTERFACE_OPERATORS)
	// {D7714D41-BC4A-445E-B695-25A65C2F561E}
	operator Toolsapi::_di_IOTARepositoryWizard80()
	{
		Toolsapi::_di_IOTARepositoryWizard80 intf;
		GetInterface(intf);
		return intf;
	}
	#else
	operator Toolsapi::IOTARepositoryWizard80*(void) { return (Toolsapi::IOTARepositoryWizard80*)&__IOTARepositoryWizard80; }
	#endif
	#if defined(MANAGED_INTERFACE_OPERATORS)
	// {08FCCD88-3A21-4281-ADC9-62FC034CDD12}
	operator Toolsapi::_di_IOTARepositoryWizard60()
	{
		Toolsapi::_di_IOTARepositoryWizard60 intf;
		GetInterface(intf);
		return intf;
	}
	#else
	operator Toolsapi::IOTARepositoryWizard60*(void) { return (Toolsapi::IOTARepositoryWizard60*)&__IOTARepositoryWizard80; }
	#endif
	#if defined(MANAGED_INTERFACE_OPERATORS)
	// {B75C0CE1-EEA6-11D1-9504-00608CCBF153}
	operator Toolsapi::_di_IOTARepositoryWizard()
	{
		Toolsapi::_di_IOTARepositoryWizard intf;
		GetInterface(intf);
		return intf;
	}
	#else
	operator Toolsapi::IOTARepositoryWizard*(void) { return (Toolsapi::IOTARepositoryWizard*)&__IOTARepositoryWizard80; }
	#endif
	#if defined(MANAGED_INTERFACE_OPERATORS)
	// {6EDB9B9A-F57A-11D1-AB23-00C04FB16FB3}
	operator Toolsapi::_di_IOTAModuleCreator()
	{
		Toolsapi::_di_IOTAModuleCreator intf;
		GetInterface(intf);
		return intf;
	}
	#else
	operator Toolsapi::IOTAModuleCreator*(void) { return (Toolsapi::IOTAModuleCreator*)&__IOTAModuleCreator; }
	#endif
	#if defined(MANAGED_INTERFACE_OPERATORS)
	// {6EDB9B9E-F57A-11D1-AB23-00C04FB16FB3}
	operator Toolsapi::_di_IOTACreator()
	{
		Toolsapi::_di_IOTACreator intf;
		GetInterface(intf);
		return intf;
	}
	#else
	operator Toolsapi::IOTACreator*(void) { return (Toolsapi::IOTACreator*)&__IOTAModuleCreator; }
	#endif
	#if defined(MANAGED_INTERFACE_OPERATORS)
	// {36C8BF35-EFFE-11D1-AB1D-00C04FB16FB3}
	operator Toolsapi::_di_IOTAFormWizard()
	{
		Toolsapi::_di_IOTAFormWizard intf;
		GetInterface(intf);
		return intf;
	}
	#else
	operator Toolsapi::IOTAFormWizard*(void) { return (Toolsapi::IOTAFormWizard*)&__IOTAFormWizard; }
	#endif
	#if defined(MANAGED_INTERFACE_OPERATORS)
	// {B75C0CE0-EEA6-11D1-9504-00608CCBF153}
	operator Toolsapi::_di_IOTAWizard()
	{
		Toolsapi::_di_IOTAWizard intf;
		GetInterface(intf);
		return intf;
	}
	#else
	operator Toolsapi::IOTAWizard*(void) { return (Toolsapi::IOTAWizard*)&__IOTARepositoryWizard80; }
	#endif
	
};

#pragma pack(pop)

class DELPHICLASS TGLSimpleSceneFormWizard;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLSimpleSceneFormWizard : public TGLBaseSceneFormWizard
{
	typedef TGLBaseSceneFormWizard inherited;
	
protected:
	virtual System::UnicodeString __fastcall GetIDString(void);
	virtual System::UnicodeString __fastcall GetName(void);
	virtual Toolsapi::_di_IOTAFile __fastcall NewFormFile(const System::UnicodeString FormIdent, const System::UnicodeString AncestorIdent);
	virtual Toolsapi::_di_IOTAFile __fastcall NewImplSource(const System::UnicodeString ModuleIdent, const System::UnicodeString FormIdent, const System::UnicodeString AncestorIdent);
public:
	/* TGLBaseSceneFormWizard.CreateAndExecute */ inline __fastcall TGLSimpleSceneFormWizard(const System::UnicodeString AUnitIdent, const System::UnicodeString AClassName, const System::UnicodeString AFileName) : TGLBaseSceneFormWizard(AUnitIdent, AClassName, AFileName) { }
	
public:
	/* TObject.Create */ inline __fastcall TGLSimpleSceneFormWizard(void) : TGLBaseSceneFormWizard() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TGLSimpleSceneFormWizard(void) { }
	
};

#pragma pack(pop)

class DELPHICLASS TGLExtendedSceneFormWizard;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLExtendedSceneFormWizard : public TGLBaseSceneFormWizard
{
	typedef TGLBaseSceneFormWizard inherited;
	
protected:
	virtual System::UnicodeString __fastcall GetIDString(void);
	virtual System::UnicodeString __fastcall GetName(void);
	virtual Toolsapi::_di_IOTAFile __fastcall NewFormFile(const System::UnicodeString FormIdent, const System::UnicodeString AncestorIdent);
	virtual Toolsapi::_di_IOTAFile __fastcall NewImplSource(const System::UnicodeString ModuleIdent, const System::UnicodeString FormIdent, const System::UnicodeString AncestorIdent);
public:
	/* TGLBaseSceneFormWizard.CreateAndExecute */ inline __fastcall TGLExtendedSceneFormWizard(const System::UnicodeString AUnitIdent, const System::UnicodeString AClassName, const System::UnicodeString AFileName) : TGLBaseSceneFormWizard(AUnitIdent, AClassName, AFileName) { }
	
public:
	/* TObject.Create */ inline __fastcall TGLExtendedSceneFormWizard(void) : TGLBaseSceneFormWizard() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TGLExtendedSceneFormWizard(void) { }
	
};

#pragma pack(pop)

class DELPHICLASS TGLBaseSceneProjectCreator;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLBaseSceneProjectCreator : public System::TInterfacedObject
{
	typedef System::TInterfacedObject inherited;
	
private:
	System::UnicodeString FUnitIdent;
	System::UnicodeString FClassName;
	System::UnicodeString FFileName;
	
public:
	__fastcall TGLBaseSceneProjectCreator(const System::UnicodeString AClassName, const System::UnicodeString AUnitIdent, const System::UnicodeString AFileName);
	System::UnicodeString __fastcall GetCreatorType(void);
	bool __fastcall GetExisting(void);
	System::UnicodeString __fastcall GetFileSystem(void);
	Toolsapi::_di_IOTAModule __fastcall GetOwner(void);
	bool __fastcall GetUnnamed(void);
	System::UnicodeString __fastcall GetFileName(void);
	System::UnicodeString __fastcall GetOptionFileName(void);
	bool __fastcall GetShowSource(void);
	void __fastcall NewDefaultModule(void);
	Toolsapi::_di_IOTAFile __fastcall NewOptionSource(const System::UnicodeString ProjectName);
	void __fastcall NewProjectResource(const Toolsapi::_di_IOTAProject Project);
	Toolsapi::_di_IOTAFile __fastcall NewProjectSource(const System::UnicodeString ProjectName);
	virtual void __fastcall NewDefaultProjectModule(const Toolsapi::_di_IOTAProject Project);
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TGLBaseSceneProjectCreator(void) { }
	
private:
	void *__IOTAProjectCreator50;	/* Toolsapi::IOTAProjectCreator50 */
	
public:
	#if defined(MANAGED_INTERFACE_OPERATORS)
	// {64312F82-62F3-48E9-BAF6-B03DF450312A}
	operator Toolsapi::_di_IOTAProjectCreator50()
	{
		Toolsapi::_di_IOTAProjectCreator50 intf;
		GetInterface(intf);
		return intf;
	}
	#else
	operator Toolsapi::IOTAProjectCreator50*(void) { return (Toolsapi::IOTAProjectCreator50*)&__IOTAProjectCreator50; }
	#endif
	#if defined(MANAGED_INTERFACE_OPERATORS)
	// {6EDB9B9D-F57A-11D1-AB23-00C04FB16FB3}
	operator Toolsapi::_di_IOTAProjectCreator()
	{
		Toolsapi::_di_IOTAProjectCreator intf;
		GetInterface(intf);
		return intf;
	}
	#else
	operator Toolsapi::IOTAProjectCreator*(void) { return (Toolsapi::IOTAProjectCreator*)&__IOTAProjectCreator50; }
	#endif
	#if defined(MANAGED_INTERFACE_OPERATORS)
	// {6EDB9B9E-F57A-11D1-AB23-00C04FB16FB3}
	operator Toolsapi::_di_IOTACreator()
	{
		Toolsapi::_di_IOTACreator intf;
		GetInterface(intf);
		return intf;
	}
	#else
	operator Toolsapi::IOTACreator*(void) { return (Toolsapi::IOTACreator*)&__IOTAProjectCreator50; }
	#endif
	
};

#pragma pack(pop)

class DELPHICLASS TGLBaseSceneProjectWizard;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLBaseSceneProjectWizard : public Toolsapi::TNotifierObject
{
	typedef Toolsapi::TNotifierObject inherited;
	
public:
	virtual System::UnicodeString __fastcall GetIDString(void);
	virtual System::UnicodeString __fastcall GetName(void);
	Toolsapi::TWizardState __fastcall GetState(void);
	virtual void __fastcall Execute(void);
	System::UnicodeString __fastcall GetAuthor(void);
	System::UnicodeString __fastcall GetComment(void);
	System::UnicodeString __fastcall GetPage(void);
	unsigned __fastcall GetGlyph(void);
	System::UnicodeString __fastcall GetDesigner(void);
	Toolsapi::_di_IOTAGalleryCategory __fastcall GetGalleryCategory(void);
	System::UnicodeString __fastcall GetPersonality(void);
public:
	/* TObject.Create */ inline __fastcall TGLBaseSceneProjectWizard(void) : Toolsapi::TNotifierObject() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TGLBaseSceneProjectWizard(void) { }
	
private:
	void *__IOTAProjectWizard;	/* Toolsapi::IOTAProjectWizard */
	void *__IOTARepositoryWizard80;	/* Toolsapi::IOTARepositoryWizard80 */
	
public:
	#if defined(MANAGED_INTERFACE_OPERATORS)
	// {36C8BF36-EFFE-11D1-AB1D-00C04FB16FB3}
	operator Toolsapi::_di_IOTAProjectWizard()
	{
		Toolsapi::_di_IOTAProjectWizard intf;
		GetInterface(intf);
		return intf;
	}
	#else
	operator Toolsapi::IOTAProjectWizard*(void) { return (Toolsapi::IOTAProjectWizard*)&__IOTAProjectWizard; }
	#endif
	#if defined(MANAGED_INTERFACE_OPERATORS)
	// {D7714D41-BC4A-445E-B695-25A65C2F561E}
	operator Toolsapi::_di_IOTARepositoryWizard80()
	{
		Toolsapi::_di_IOTARepositoryWizard80 intf;
		GetInterface(intf);
		return intf;
	}
	#else
	operator Toolsapi::IOTARepositoryWizard80*(void) { return (Toolsapi::IOTARepositoryWizard80*)&__IOTARepositoryWizard80; }
	#endif
	#if defined(MANAGED_INTERFACE_OPERATORS)
	// {08FCCD88-3A21-4281-ADC9-62FC034CDD12}
	operator Toolsapi::_di_IOTARepositoryWizard60()
	{
		Toolsapi::_di_IOTARepositoryWizard60 intf;
		GetInterface(intf);
		return intf;
	}
	#else
	operator Toolsapi::IOTARepositoryWizard60*(void) { return (Toolsapi::IOTARepositoryWizard60*)&__IOTARepositoryWizard80; }
	#endif
	#if defined(MANAGED_INTERFACE_OPERATORS)
	// {B75C0CE1-EEA6-11D1-9504-00608CCBF153}
	operator Toolsapi::_di_IOTARepositoryWizard()
	{
		Toolsapi::_di_IOTARepositoryWizard intf;
		GetInterface(intf);
		return intf;
	}
	#else
	operator Toolsapi::IOTARepositoryWizard*(void) { return (Toolsapi::IOTARepositoryWizard*)&__IOTAProjectWizard; }
	#endif
	#if defined(MANAGED_INTERFACE_OPERATORS)
	// {B75C0CE0-EEA6-11D1-9504-00608CCBF153}
	operator Toolsapi::_di_IOTAWizard()
	{
		Toolsapi::_di_IOTAWizard intf;
		GetInterface(intf);
		return intf;
	}
	#else
	operator Toolsapi::IOTAWizard*(void) { return (Toolsapi::IOTAWizard*)&__IOTAProjectWizard; }
	#endif
	
};

#pragma pack(pop)

class DELPHICLASS TGLSimpleSceneProjectWizard;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLSimpleSceneProjectWizard : public TGLBaseSceneProjectWizard
{
	typedef TGLBaseSceneProjectWizard inherited;
	
public:
	virtual System::UnicodeString __fastcall GetIDString(void);
	virtual System::UnicodeString __fastcall GetName(void);
	virtual void __fastcall Execute(void);
public:
	/* TObject.Create */ inline __fastcall TGLSimpleSceneProjectWizard(void) : TGLBaseSceneProjectWizard() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TGLSimpleSceneProjectWizard(void) { }
	
};

#pragma pack(pop)

class DELPHICLASS TGLSimpleSceneProjectCreator;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLSimpleSceneProjectCreator : public TGLBaseSceneProjectCreator
{
	typedef TGLBaseSceneProjectCreator inherited;
	
public:
	virtual void __fastcall NewDefaultProjectModule(const Toolsapi::_di_IOTAProject Project);
public:
	/* TGLBaseSceneProjectCreator.Create */ inline __fastcall TGLSimpleSceneProjectCreator(const System::UnicodeString AClassName, const System::UnicodeString AUnitIdent, const System::UnicodeString AFileName) : TGLBaseSceneProjectCreator(AClassName, AUnitIdent, AFileName) { }
	
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TGLSimpleSceneProjectCreator(void) { }
	
};

#pragma pack(pop)

class DELPHICLASS TGLExtendedSceneProjectWizard;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLExtendedSceneProjectWizard : public TGLBaseSceneProjectWizard
{
	typedef TGLBaseSceneProjectWizard inherited;
	
public:
	virtual System::UnicodeString __fastcall GetIDString(void);
	virtual System::UnicodeString __fastcall GetName(void);
	virtual void __fastcall Execute(void);
public:
	/* TObject.Create */ inline __fastcall TGLExtendedSceneProjectWizard(void) : TGLBaseSceneProjectWizard() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TGLExtendedSceneProjectWizard(void) { }
	
};

#pragma pack(pop)

class DELPHICLASS TGLExtendedSceneProjectCreator;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLExtendedSceneProjectCreator : public TGLBaseSceneProjectCreator
{
	typedef TGLBaseSceneProjectCreator inherited;
	
public:
	virtual void __fastcall NewDefaultProjectModule(const Toolsapi::_di_IOTAProject Project);
public:
	/* TGLBaseSceneProjectCreator.Create */ inline __fastcall TGLExtendedSceneProjectCreator(const System::UnicodeString AClassName, const System::UnicodeString AUnitIdent, const System::UnicodeString AFileName) : TGLBaseSceneProjectCreator(AClassName, AUnitIdent, AFileName) { }
	
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TGLExtendedSceneProjectCreator(void) { }
	
};

#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------
extern PACKAGE System::ResourceString _rBaseProjectLocalizedName;
#define Glsceneformdesign_rBaseProjectLocalizedName System::LoadResourceString(&Glsceneformdesign::_rBaseProjectLocalizedName)
extern PACKAGE System::ResourceString _rBaseProjectLocalizedDescription;
#define Glsceneformdesign_rBaseProjectLocalizedDescription System::LoadResourceString(&Glsceneformdesign::_rBaseProjectLocalizedDescription)
extern PACKAGE System::ResourceString _rSimpleProjectLocalizedName;
#define Glsceneformdesign_rSimpleProjectLocalizedName System::LoadResourceString(&Glsceneformdesign::_rSimpleProjectLocalizedName)
extern PACKAGE System::ResourceString _rSimpleProjectLocalizedDescription;
#define Glsceneformdesign_rSimpleProjectLocalizedDescription System::LoadResourceString(&Glsceneformdesign::_rSimpleProjectLocalizedDescription)
extern PACKAGE System::ResourceString _rExtendedProjectLocalizedName;
#define Glsceneformdesign_rExtendedProjectLocalizedName System::LoadResourceString(&Glsceneformdesign::_rExtendedProjectLocalizedName)
extern PACKAGE System::ResourceString _rExtendedProjectLocalizedDescription;
#define Glsceneformdesign_rExtendedProjectLocalizedDescription System::LoadResourceString(&Glsceneformdesign::_rExtendedProjectLocalizedDescription)
extern PACKAGE System::ResourceString _rBaseFormLocalizedName;
#define Glsceneformdesign_rBaseFormLocalizedName System::LoadResourceString(&Glsceneformdesign::_rBaseFormLocalizedName)
extern PACKAGE System::ResourceString _rBaseFormLocalizedDescription;
#define Glsceneformdesign_rBaseFormLocalizedDescription System::LoadResourceString(&Glsceneformdesign::_rBaseFormLocalizedDescription)
extern PACKAGE System::ResourceString _rSimpleFormLocalizedName;
#define Glsceneformdesign_rSimpleFormLocalizedName System::LoadResourceString(&Glsceneformdesign::_rSimpleFormLocalizedName)
extern PACKAGE System::ResourceString _rSimpleFormLocalizedDescription;
#define Glsceneformdesign_rSimpleFormLocalizedDescription System::LoadResourceString(&Glsceneformdesign::_rSimpleFormLocalizedDescription)
extern PACKAGE System::ResourceString _rExtendedFormLocalizedName;
#define Glsceneformdesign_rExtendedFormLocalizedName System::LoadResourceString(&Glsceneformdesign::_rExtendedFormLocalizedName)
extern PACKAGE System::ResourceString _rExtendedFormLocalizedDescription;
#define Glsceneformdesign_rExtendedFormLocalizedDescription System::LoadResourceString(&Glsceneformdesign::_rExtendedFormLocalizedDescription)
extern PACKAGE void __fastcall Register(void);
}	/* namespace Glsceneformdesign */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLSCENEFORMDESIGN)
using namespace Glsceneformdesign;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// GlsceneformdesignHPP
