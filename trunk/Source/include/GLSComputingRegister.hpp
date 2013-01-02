// CodeGear C++Builder
// Copyright (c) 1995, 2012 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLSComputingRegister.pas' rev: 24.00 (Win32)

#ifndef GlscomputingregisterHPP
#define GlscomputingregisterHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <System.Classes.hpp>	// Pascal unit
#include <System.SysUtils.hpp>	// Pascal unit
#include <GLSceneRegister.hpp>	// Pascal unit
#include <DesignIntf.hpp>	// Pascal unit
#include <DesignEditors.hpp>	// Pascal unit
#include <StrEdit.hpp>	// Pascal unit
#include <ToolsAPI.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Glscomputingregister
{
//-- type declarations -------------------------------------------------------
class DELPHICLASS TGLSCUDAEditor;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLSCUDAEditor : public Designeditors::TComponentEditor
{
	typedef Designeditors::TComponentEditor inherited;
	
public:
	virtual void __fastcall Edit(void);
	virtual void __fastcall ExecuteVerb(int Index);
	virtual System::UnicodeString __fastcall GetVerb(int Index);
	virtual int __fastcall GetVerbCount(void);
public:
	/* TComponentEditor.Create */ inline __fastcall virtual TGLSCUDAEditor(System::Classes::TComponent* AComponent, Designintf::_di_IDesigner ADesigner) : Designeditors::TComponentEditor(AComponent, ADesigner) { }
	
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TGLSCUDAEditor(void) { }
	
};

#pragma pack(pop)

class DELPHICLASS TGLSCUDACompilerEditor;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLSCUDACompilerEditor : public Designeditors::TComponentEditor
{
	typedef Designeditors::TComponentEditor inherited;
	
public:
	virtual void __fastcall Edit(void);
	virtual void __fastcall ExecuteVerb(int Index);
	virtual System::UnicodeString __fastcall GetVerb(int Index);
	virtual int __fastcall GetVerbCount(void);
public:
	/* TComponentEditor.Create */ inline __fastcall virtual TGLSCUDACompilerEditor(System::Classes::TComponent* AComponent, Designintf::_di_IDesigner ADesigner) : Designeditors::TComponentEditor(AComponent, ADesigner) { }
	
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TGLSCUDACompilerEditor(void) { }
	
};

#pragma pack(pop)

class DELPHICLASS TGLSCUDACompilerSourceProperty;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLSCUDACompilerSourceProperty : public Designeditors::TStringProperty
{
	typedef Designeditors::TStringProperty inherited;
	
private:
	System::Classes::TStringList* FModuleList;
	void __fastcall RefreshModuleList(void);
	
public:
	__fastcall virtual TGLSCUDACompilerSourceProperty(const Designintf::_di_IDesigner ADesigner, int APropCount);
	__fastcall virtual ~TGLSCUDACompilerSourceProperty(void);
	virtual Designintf::TPropertyAttributes __fastcall GetAttributes(void);
	virtual void __fastcall GetValues(System::Classes::TGetStrProc Proc);
	virtual void __fastcall SetValue(const System::UnicodeString Value)/* overload */;
/* Hoisted overloads: */
	
public:
	inline void __fastcall  SetValue(const System::WideString Value){ Designeditors::TPropertyEditor::SetValue(Value); }
	
};

#pragma pack(pop)

class DELPHICLASS TGLSCUDADeviceProperty;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLSCUDADeviceProperty : public Designeditors::TStringProperty
{
	typedef Designeditors::TStringProperty inherited;
	
private:
	System::Classes::TStringList* FDeviceList;
	
public:
	__fastcall virtual TGLSCUDADeviceProperty(const Designintf::_di_IDesigner ADesigner, int APropCount);
	__fastcall virtual ~TGLSCUDADeviceProperty(void);
	virtual Designintf::TPropertyAttributes __fastcall GetAttributes(void);
	virtual void __fastcall GetValues(System::Classes::TGetStrProc Proc);
	virtual void __fastcall SetValue(const System::UnicodeString Value)/* overload */;
/* Hoisted overloads: */
	
public:
	inline void __fastcall  SetValue(const System::WideString Value){ Designeditors::TPropertyEditor::SetValue(Value); }
	
};

#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------
extern PACKAGE void __fastcall Register(void);
}	/* namespace Glscomputingregister */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLSCOMPUTINGREGISTER)
using namespace Glscomputingregister;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// GlscomputingregisterHPP
