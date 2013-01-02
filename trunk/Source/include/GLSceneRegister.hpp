// CodeGear C++Builder
// Copyright (c) 1995, 2012 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLSceneRegister.pas' rev: 24.00 (Win32)

#ifndef GlsceneregisterHPP
#define GlsceneregisterHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <Winapi.Windows.hpp>	// Pascal unit
#include <System.Classes.hpp>	// Pascal unit
#include <Vcl.Forms.hpp>	// Pascal unit
#include <Vcl.Controls.hpp>	// Pascal unit
#include <Vcl.StdCtrls.hpp>	// Pascal unit
#include <Vcl.Graphics.hpp>	// Pascal unit
#include <GLScene.hpp>	// Pascal unit
#include <GLContext.hpp>	// Pascal unit
#include <GLColor.hpp>	// Pascal unit
#include <GLCrossPlatform.hpp>	// Pascal unit
#include <GLObjectManager.hpp>	// Pascal unit
#include <ToolsAPI.hpp>	// Pascal unit
#include <DesignIntf.hpp>	// Pascal unit
#include <DesignEditors.hpp>	// Pascal unit
#include <VCLEditors.hpp>	// Pascal unit
#include <System.Types.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Glsceneregister
{
//-- type declarations -------------------------------------------------------
class DELPHICLASS TGLLibMaterialNameProperty;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLLibMaterialNameProperty : public Designeditors::TStringProperty
{
	typedef Designeditors::TStringProperty inherited;
	
public:
	virtual Designintf::TPropertyAttributes __fastcall GetAttributes(void);
	virtual void __fastcall Edit(void);
public:
	/* TPropertyEditor.Create */ inline __fastcall virtual TGLLibMaterialNameProperty(const Designintf::_di_IDesigner ADesigner, int APropCount) : Designeditors::TStringProperty(ADesigner, APropCount) { }
	/* TPropertyEditor.Destroy */ inline __fastcall virtual ~TGLLibMaterialNameProperty(void) { }
	
};

#pragma pack(pop)

class DELPHICLASS TGLSceneViewerEditor;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLSceneViewerEditor : public Designeditors::TComponentEditor
{
	typedef Designeditors::TComponentEditor inherited;
	
public:
	virtual void __fastcall ExecuteVerb(int Index);
	virtual System::UnicodeString __fastcall GetVerb(int Index);
	virtual int __fastcall GetVerbCount(void);
public:
	/* TComponentEditor.Create */ inline __fastcall virtual TGLSceneViewerEditor(System::Classes::TComponent* AComponent, Designintf::_di_IDesigner ADesigner) : Designeditors::TComponentEditor(AComponent, ADesigner) { }
	
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TGLSceneViewerEditor(void) { }
	
};

#pragma pack(pop)

class DELPHICLASS TGLSceneEditor;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLSceneEditor : public Designeditors::TComponentEditor
{
	typedef Designeditors::TComponentEditor inherited;
	
public:
	virtual void __fastcall Edit(void);
	virtual void __fastcall ExecuteVerb(int Index);
	virtual System::UnicodeString __fastcall GetVerb(int Index);
	virtual int __fastcall GetVerbCount(void);
public:
	/* TComponentEditor.Create */ inline __fastcall virtual TGLSceneEditor(System::Classes::TComponent* AComponent, Designintf::_di_IDesigner ADesigner) : Designeditors::TComponentEditor(AComponent, ADesigner) { }
	
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TGLSceneEditor(void) { }
	
};

#pragma pack(pop)

class DELPHICLASS TResolutionProperty;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TResolutionProperty : public Designeditors::TPropertyEditor
{
	typedef Designeditors::TPropertyEditor inherited;
	
public:
	virtual Designintf::TPropertyAttributes __fastcall GetAttributes(void);
	virtual System::UnicodeString __fastcall GetValue(void);
	virtual void __fastcall GetValues(System::Classes::TGetStrProc Proc);
	virtual void __fastcall SetValue(const System::UnicodeString Value)/* overload */;
public:
	/* TPropertyEditor.Create */ inline __fastcall virtual TResolutionProperty(const Designintf::_di_IDesigner ADesigner, int APropCount) : Designeditors::TPropertyEditor(ADesigner, APropCount) { }
	/* TPropertyEditor.Destroy */ inline __fastcall virtual ~TResolutionProperty(void) { }
	
/* Hoisted overloads: */
	
public:
	inline void __fastcall  SetValue(const System::WideString Value){ Designeditors::TPropertyEditor::SetValue(Value); }
	
};

#pragma pack(pop)

class DELPHICLASS TGLTextureProperty;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLTextureProperty : public Designeditors::TClassProperty
{
	typedef Designeditors::TClassProperty inherited;
	
public:
	virtual Designintf::TPropertyAttributes __fastcall GetAttributes(void);
public:
	/* TPropertyEditor.Create */ inline __fastcall virtual TGLTextureProperty(const Designintf::_di_IDesigner ADesigner, int APropCount) : Designeditors::TClassProperty(ADesigner, APropCount) { }
	/* TPropertyEditor.Destroy */ inline __fastcall virtual ~TGLTextureProperty(void) { }
	
};

#pragma pack(pop)

class DELPHICLASS TGLTextureImageProperty;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLTextureImageProperty : public Designeditors::TClassProperty
{
	typedef Designeditors::TClassProperty inherited;
	
public:
	virtual Designintf::TPropertyAttributes __fastcall GetAttributes(void);
	virtual void __fastcall Edit(void);
public:
	/* TPropertyEditor.Create */ inline __fastcall virtual TGLTextureImageProperty(const Designintf::_di_IDesigner ADesigner, int APropCount) : Designeditors::TClassProperty(ADesigner, APropCount) { }
	/* TPropertyEditor.Destroy */ inline __fastcall virtual ~TGLTextureImageProperty(void) { }
	
};

#pragma pack(pop)

class DELPHICLASS TGLImageClassProperty;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLImageClassProperty : public Designeditors::TClassProperty
{
	typedef Designeditors::TClassProperty inherited;
	
public:
	virtual Designintf::TPropertyAttributes __fastcall GetAttributes(void);
	virtual void __fastcall GetValues(System::Classes::TGetStrProc proc);
	virtual System::UnicodeString __fastcall GetValue(void);
	virtual void __fastcall SetValue(const System::UnicodeString value)/* overload */;
public:
	/* TPropertyEditor.Create */ inline __fastcall virtual TGLImageClassProperty(const Designintf::_di_IDesigner ADesigner, int APropCount) : Designeditors::TClassProperty(ADesigner, APropCount) { }
	/* TPropertyEditor.Destroy */ inline __fastcall virtual ~TGLImageClassProperty(void) { }
	
/* Hoisted overloads: */
	
public:
	inline void __fastcall  SetValue(const System::WideString Value){ Designeditors::TPropertyEditor::SetValue(Value); }
	
};

#pragma pack(pop)

class DELPHICLASS TGLColorProperty;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLColorProperty : public Designeditors::TClassProperty
{
	typedef Designeditors::TClassProperty inherited;
	
protected:
	System::Uitypes::TColor __fastcall ColorToBorderColor(const Vectortypes::TVector4f &aColor, bool selected);
	
public:
	virtual Designintf::TPropertyAttributes __fastcall GetAttributes(void);
	virtual void __fastcall GetValues(System::Classes::TGetStrProc Proc);
	virtual void __fastcall Edit(void);
	void __fastcall ListMeasureHeight(const System::UnicodeString Value, Vcl::Graphics::TCanvas* ACanvas, int &AHeight);
	void __fastcall ListMeasureWidth(const System::UnicodeString Value, Vcl::Graphics::TCanvas* ACanvas, int &AWidth);
	void __fastcall ListDrawValue(const System::UnicodeString Value, Vcl::Graphics::TCanvas* ACanvas, const System::Types::TRect &ARect, bool ASelected);
	void __fastcall PropDrawName(Vcl::Graphics::TCanvas* ACanvas, const System::Types::TRect &ARect, bool ASelected);
	void __fastcall PropDrawValue(Vcl::Graphics::TCanvas* ACanvas, const System::Types::TRect &ARect, bool ASelected);
	virtual System::UnicodeString __fastcall GetValue(void);
	virtual void __fastcall SetValue(const System::UnicodeString Value)/* overload */;
public:
	/* TPropertyEditor.Create */ inline __fastcall virtual TGLColorProperty(const Designintf::_di_IDesigner ADesigner, int APropCount) : Designeditors::TClassProperty(ADesigner, APropCount) { }
	/* TPropertyEditor.Destroy */ inline __fastcall virtual ~TGLColorProperty(void) { }
	
/* Hoisted overloads: */
	
public:
	inline void __fastcall  SetValue(const System::WideString Value){ Designeditors::TPropertyEditor::SetValue(Value); }
	
private:
	void *__ICustomPropertyListDrawing;	/* Vcleditors::ICustomPropertyListDrawing */
	void *__ICustomPropertyDrawing;	/* Vcleditors::ICustomPropertyDrawing */
	
public:
	#if defined(MANAGED_INTERFACE_OPERATORS)
	// {BE2B8CF7-DDCA-4D4B-BE26-2396B969F8E0}
	operator Vcleditors::_di_ICustomPropertyListDrawing()
	{
		Vcleditors::_di_ICustomPropertyListDrawing intf;
		GetInterface(intf);
		return intf;
	}
	#else
	operator Vcleditors::ICustomPropertyListDrawing*(void) { return (Vcleditors::ICustomPropertyListDrawing*)&__ICustomPropertyListDrawing; }
	#endif
	#if defined(MANAGED_INTERFACE_OPERATORS)
	// {E1A50419-1288-4B26-9EFA-6608A35F0824}
	operator Vcleditors::_di_ICustomPropertyDrawing()
	{
		Vcleditors::_di_ICustomPropertyDrawing intf;
		GetInterface(intf);
		return intf;
	}
	#else
	operator Vcleditors::ICustomPropertyDrawing*(void) { return (Vcleditors::ICustomPropertyDrawing*)&__ICustomPropertyDrawing; }
	#endif
	
};

#pragma pack(pop)

class DELPHICLASS TSoundFileProperty;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TSoundFileProperty : public Designeditors::TClassProperty
{
	typedef Designeditors::TClassProperty inherited;
	
public:
	virtual Designintf::TPropertyAttributes __fastcall GetAttributes(void);
	virtual System::UnicodeString __fastcall GetValue(void);
	virtual void __fastcall Edit(void);
public:
	/* TPropertyEditor.Create */ inline __fastcall virtual TSoundFileProperty(const Designintf::_di_IDesigner ADesigner, int APropCount) : Designeditors::TClassProperty(ADesigner, APropCount) { }
	/* TPropertyEditor.Destroy */ inline __fastcall virtual ~TSoundFileProperty(void) { }
	
};

#pragma pack(pop)

class DELPHICLASS TSoundNameProperty;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TSoundNameProperty : public Designeditors::TStringProperty
{
	typedef Designeditors::TStringProperty inherited;
	
public:
	virtual Designintf::TPropertyAttributes __fastcall GetAttributes(void);
	virtual void __fastcall GetValues(System::Classes::TGetStrProc Proc);
public:
	/* TPropertyEditor.Create */ inline __fastcall virtual TSoundNameProperty(const Designintf::_di_IDesigner ADesigner, int APropCount) : Designeditors::TStringProperty(ADesigner, APropCount) { }
	/* TPropertyEditor.Destroy */ inline __fastcall virtual ~TSoundNameProperty(void) { }
	
};

#pragma pack(pop)

class DELPHICLASS TGLCoordinatesProperty;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLCoordinatesProperty : public Designeditors::TClassProperty
{
	typedef Designeditors::TClassProperty inherited;
	
public:
	virtual Designintf::TPropertyAttributes __fastcall GetAttributes(void);
	virtual void __fastcall Edit(void);
public:
	/* TPropertyEditor.Create */ inline __fastcall virtual TGLCoordinatesProperty(const Designintf::_di_IDesigner ADesigner, int APropCount) : Designeditors::TClassProperty(ADesigner, APropCount) { }
	/* TPropertyEditor.Destroy */ inline __fastcall virtual ~TGLCoordinatesProperty(void) { }
	
};

#pragma pack(pop)

class DELPHICLASS TGLMaterialProperty;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLMaterialProperty : public Designeditors::TClassProperty
{
	typedef Designeditors::TClassProperty inherited;
	
public:
	virtual Designintf::TPropertyAttributes __fastcall GetAttributes(void);
	virtual void __fastcall Edit(void);
public:
	/* TPropertyEditor.Create */ inline __fastcall virtual TGLMaterialProperty(const Designintf::_di_IDesigner ADesigner, int APropCount) : Designeditors::TClassProperty(ADesigner, APropCount) { }
	/* TPropertyEditor.Destroy */ inline __fastcall virtual ~TGLMaterialProperty(void) { }
	
};

#pragma pack(pop)

class DELPHICLASS TGLGUILayoutEditor;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLGUILayoutEditor : public Designeditors::TComponentEditor
{
	typedef Designeditors::TComponentEditor inherited;
	
public:
	virtual void __fastcall Edit(void);
	virtual void __fastcall ExecuteVerb(int Index);
	virtual System::UnicodeString __fastcall GetVerb(int Index);
	virtual int __fastcall GetVerbCount(void);
public:
	/* TComponentEditor.Create */ inline __fastcall virtual TGLGUILayoutEditor(System::Classes::TComponent* AComponent, Designintf::_di_IDesigner ADesigner) : Designeditors::TComponentEditor(AComponent, ADesigner) { }
	
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TGLGUILayoutEditor(void) { }
	
};

#pragma pack(pop)

class DELPHICLASS TReuseableDefaultEditor;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TReuseableDefaultEditor : public Designeditors::TComponentEditor
{
	typedef Designeditors::TComponentEditor inherited;
	
protected:
	Designintf::_di_IProperty FFirst;
	Designintf::_di_IProperty FBest;
	bool FContinue;
	void __fastcall CheckEdit(const Designintf::_di_IProperty Prop);
	virtual void __fastcall EditProperty(const Designintf::_di_IProperty Prop, bool &Continue);
	
public:
	virtual void __fastcall Edit(void);
public:
	/* TComponentEditor.Create */ inline __fastcall virtual TReuseableDefaultEditor(System::Classes::TComponent* AComponent, Designintf::_di_IDesigner ADesigner) : Designeditors::TComponentEditor(AComponent, ADesigner) { }
	
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TReuseableDefaultEditor(void) { }
	
private:
	void *__IDefaultEditor;	/* Designintf::IDefaultEditor */
	
public:
	#if defined(MANAGED_INTERFACE_OPERATORS)
	// {5484FAE1-5C60-11D1-9FB6-0020AF3D82DA}
	operator Designintf::_di_IDefaultEditor()
	{
		Designintf::_di_IDefaultEditor intf;
		GetInterface(intf);
		return intf;
	}
	#else
	operator Designintf::IDefaultEditor*(void) { return (Designintf::IDefaultEditor*)&__IDefaultEditor; }
	#endif
	
};

#pragma pack(pop)

class DELPHICLASS TGLMaterialLibraryEditor;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLMaterialLibraryEditor : public TReuseableDefaultEditor
{
	typedef TReuseableDefaultEditor inherited;
	
protected:
	virtual void __fastcall EditProperty(const Designintf::_di_IProperty Prop, bool &Continue);
	
public:
	virtual void __fastcall ExecuteVerb(int Index);
	virtual System::UnicodeString __fastcall GetVerb(int Index);
	virtual int __fastcall GetVerbCount(void);
public:
	/* TComponentEditor.Create */ inline __fastcall virtual TGLMaterialLibraryEditor(System::Classes::TComponent* AComponent, Designintf::_di_IDesigner ADesigner) : TReuseableDefaultEditor(AComponent, ADesigner) { }
	
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TGLMaterialLibraryEditor(void) { }
	
private:
	void *__IDefaultEditor;	/* Designintf::IDefaultEditor */
	
public:
	#if defined(MANAGED_INTERFACE_OPERATORS)
	// {5484FAE1-5C60-11D1-9FB6-0020AF3D82DA}
	operator Designintf::_di_IDefaultEditor()
	{
		Designintf::_di_IDefaultEditor intf;
		GetInterface(intf);
		return intf;
	}
	#else
	operator Designintf::IDefaultEditor*(void) { return (Designintf::IDefaultEditor*)&__IDefaultEditor; }
	#endif
	
};

#pragma pack(pop)

class DELPHICLASS TGLAnimationNameProperty;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLAnimationNameProperty : public Designeditors::TStringProperty
{
	typedef Designeditors::TStringProperty inherited;
	
public:
	virtual Designintf::TPropertyAttributes __fastcall GetAttributes(void);
	virtual void __fastcall GetValues(System::Classes::TGetStrProc proc);
public:
	/* TPropertyEditor.Create */ inline __fastcall virtual TGLAnimationNameProperty(const Designintf::_di_IDesigner ADesigner, int APropCount) : Designeditors::TStringProperty(ADesigner, APropCount) { }
	/* TPropertyEditor.Destroy */ inline __fastcall virtual ~TGLAnimationNameProperty(void) { }
	
};

#pragma pack(pop)

class DELPHICLASS TGLSoundLibrarySelectionEditor;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLSoundLibrarySelectionEditor : public Designeditors::TSelectionEditor
{
	typedef Designeditors::TSelectionEditor inherited;
	
public:
	virtual void __fastcall RequiresUnits(System::Classes::TGetStrProc Proc);
public:
	/* TSelectionEditor.Create */ inline __fastcall virtual TGLSoundLibrarySelectionEditor(const Designintf::_di_IDesigner ADesigner) : Designeditors::TSelectionEditor(ADesigner) { }
	
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TGLSoundLibrarySelectionEditor(void) { }
	
};

#pragma pack(pop)

class DELPHICLASS TGLBaseSceneObjectSelectionEditor;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLBaseSceneObjectSelectionEditor : public Designeditors::TSelectionEditor
{
	typedef Designeditors::TSelectionEditor inherited;
	
public:
	virtual void __fastcall RequiresUnits(System::Classes::TGetStrProc Proc);
public:
	/* TSelectionEditor.Create */ inline __fastcall virtual TGLBaseSceneObjectSelectionEditor(const Designintf::_di_IDesigner ADesigner) : Designeditors::TSelectionEditor(ADesigner) { }
	
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TGLBaseSceneObjectSelectionEditor(void) { }
	
};

#pragma pack(pop)

class DELPHICLASS TGLSArchiveManagerEditor;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLSArchiveManagerEditor : public TReuseableDefaultEditor
{
	typedef TReuseableDefaultEditor inherited;
	
protected:
	virtual void __fastcall EditProperty(const Designintf::_di_IProperty Prop, bool &Continue);
	
public:
	virtual void __fastcall ExecuteVerb(int Index);
	virtual System::UnicodeString __fastcall GetVerb(int Index);
	virtual int __fastcall GetVerbCount(void);
public:
	/* TComponentEditor.Create */ inline __fastcall virtual TGLSArchiveManagerEditor(System::Classes::TComponent* AComponent, Designintf::_di_IDesigner ADesigner) : TReuseableDefaultEditor(AComponent, ADesigner) { }
	
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TGLSArchiveManagerEditor(void) { }
	
private:
	void *__IDefaultEditor;	/* Designintf::IDefaultEditor */
	
public:
	#if defined(MANAGED_INTERFACE_OPERATORS)
	// {5484FAE1-5C60-11D1-9FB6-0020AF3D82DA}
	operator Designintf::_di_IDefaultEditor()
	{
		Designintf::_di_IDefaultEditor intf;
		GetInterface(intf);
		return intf;
	}
	#else
	operator Designintf::IDefaultEditor*(void) { return (Designintf::IDefaultEditor*)&__IDefaultEditor; }
	#endif
	
};

#pragma pack(pop)

class DELPHICLASS TGLMaterialComponentNameProperty;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLMaterialComponentNameProperty : public Designeditors::TStringProperty
{
	typedef Designeditors::TStringProperty inherited;
	
public:
	virtual Designintf::TPropertyAttributes __fastcall GetAttributes(void);
	virtual void __fastcall Edit(void);
public:
	/* TPropertyEditor.Create */ inline __fastcall virtual TGLMaterialComponentNameProperty(const Designintf::_di_IDesigner ADesigner, int APropCount) : Designeditors::TStringProperty(ADesigner, APropCount) { }
	/* TPropertyEditor.Destroy */ inline __fastcall virtual ~TGLMaterialComponentNameProperty(void) { }
	
};

#pragma pack(pop)

class DELPHICLASS TGLLibTextureNameProperty;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLLibTextureNameProperty : public TGLMaterialComponentNameProperty
{
	typedef TGLMaterialComponentNameProperty inherited;
	
public:
	virtual void __fastcall GetValues(System::Classes::TGetStrProc Proc);
public:
	/* TPropertyEditor.Create */ inline __fastcall virtual TGLLibTextureNameProperty(const Designintf::_di_IDesigner ADesigner, int APropCount) : TGLMaterialComponentNameProperty(ADesigner, APropCount) { }
	/* TPropertyEditor.Destroy */ inline __fastcall virtual ~TGLLibTextureNameProperty(void) { }
	
};

#pragma pack(pop)

class DELPHICLASS TGLLibSamplerNameProperty;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLLibSamplerNameProperty : public TGLMaterialComponentNameProperty
{
	typedef TGLMaterialComponentNameProperty inherited;
	
public:
	virtual void __fastcall GetValues(System::Classes::TGetStrProc Proc);
public:
	/* TPropertyEditor.Create */ inline __fastcall virtual TGLLibSamplerNameProperty(const Designintf::_di_IDesigner ADesigner, int APropCount) : TGLMaterialComponentNameProperty(ADesigner, APropCount) { }
	/* TPropertyEditor.Destroy */ inline __fastcall virtual ~TGLLibSamplerNameProperty(void) { }
	
};

#pragma pack(pop)

class DELPHICLASS TGLLibCombinerNameProperty;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLLibCombinerNameProperty : public TGLMaterialComponentNameProperty
{
	typedef TGLMaterialComponentNameProperty inherited;
	
public:
	virtual void __fastcall GetValues(System::Classes::TGetStrProc Proc);
public:
	/* TPropertyEditor.Create */ inline __fastcall virtual TGLLibCombinerNameProperty(const Designintf::_di_IDesigner ADesigner, int APropCount) : TGLMaterialComponentNameProperty(ADesigner, APropCount) { }
	/* TPropertyEditor.Destroy */ inline __fastcall virtual ~TGLLibCombinerNameProperty(void) { }
	
};

#pragma pack(pop)

class DELPHICLASS TGLLibShaderNameProperty;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLLibShaderNameProperty : public TGLMaterialComponentNameProperty
{
	typedef TGLMaterialComponentNameProperty inherited;
	
public:
	virtual void __fastcall GetValues(System::Classes::TGetStrProc Proc);
public:
	/* TPropertyEditor.Create */ inline __fastcall virtual TGLLibShaderNameProperty(const Designintf::_di_IDesigner ADesigner, int APropCount) : TGLMaterialComponentNameProperty(ADesigner, APropCount) { }
	/* TPropertyEditor.Destroy */ inline __fastcall virtual ~TGLLibShaderNameProperty(void) { }
	
};

#pragma pack(pop)

class DELPHICLASS TGLLibAttachmentNameProperty;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLLibAttachmentNameProperty : public TGLMaterialComponentNameProperty
{
	typedef TGLMaterialComponentNameProperty inherited;
	
public:
	virtual void __fastcall GetValues(System::Classes::TGetStrProc Proc);
public:
	/* TPropertyEditor.Create */ inline __fastcall virtual TGLLibAttachmentNameProperty(const Designintf::_di_IDesigner ADesigner, int APropCount) : TGLMaterialComponentNameProperty(ADesigner, APropCount) { }
	/* TPropertyEditor.Destroy */ inline __fastcall virtual ~TGLLibAttachmentNameProperty(void) { }
	
};

#pragma pack(pop)

class DELPHICLASS TGLLibAsmProgNameProperty;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLLibAsmProgNameProperty : public TGLMaterialComponentNameProperty
{
	typedef TGLMaterialComponentNameProperty inherited;
	
public:
	virtual void __fastcall GetValues(System::Classes::TGetStrProc Proc);
public:
	/* TPropertyEditor.Create */ inline __fastcall virtual TGLLibAsmProgNameProperty(const Designintf::_di_IDesigner ADesigner, int APropCount) : TGLMaterialComponentNameProperty(ADesigner, APropCount) { }
	/* TPropertyEditor.Destroy */ inline __fastcall virtual ~TGLLibAsmProgNameProperty(void) { }
	
};

#pragma pack(pop)

class DELPHICLASS TPictureFileProperty;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TPictureFileProperty : public Designeditors::TStringProperty
{
	typedef Designeditors::TStringProperty inherited;
	
public:
	virtual Designintf::TPropertyAttributes __fastcall GetAttributes(void);
	virtual void __fastcall Edit(void);
public:
	/* TPropertyEditor.Create */ inline __fastcall virtual TPictureFileProperty(const Designintf::_di_IDesigner ADesigner, int APropCount) : Designeditors::TStringProperty(ADesigner, APropCount) { }
	/* TPropertyEditor.Destroy */ inline __fastcall virtual ~TPictureFileProperty(void) { }
	
};

#pragma pack(pop)

class DELPHICLASS TShaderFileProperty;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TShaderFileProperty : public Designeditors::TStringProperty
{
	typedef Designeditors::TStringProperty inherited;
	
public:
	virtual Designintf::TPropertyAttributes __fastcall GetAttributes(void);
	virtual void __fastcall Edit(void);
public:
	/* TPropertyEditor.Create */ inline __fastcall virtual TShaderFileProperty(const Designintf::_di_IDesigner ADesigner, int APropCount) : Designeditors::TStringProperty(ADesigner, APropCount) { }
	/* TPropertyEditor.Destroy */ inline __fastcall virtual ~TShaderFileProperty(void) { }
	
};

#pragma pack(pop)

class DELPHICLASS TAsmProgFileProperty;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TAsmProgFileProperty : public Designeditors::TStringProperty
{
	typedef Designeditors::TStringProperty inherited;
	
public:
	virtual Designintf::TPropertyAttributes __fastcall GetAttributes(void);
	virtual void __fastcall Edit(void);
public:
	/* TPropertyEditor.Create */ inline __fastcall virtual TAsmProgFileProperty(const Designintf::_di_IDesigner ADesigner, int APropCount) : Designeditors::TStringProperty(ADesigner, APropCount) { }
	/* TPropertyEditor.Destroy */ inline __fastcall virtual ~TAsmProgFileProperty(void) { }
	
};

#pragma pack(pop)

class DELPHICLASS TUniformAutoSetProperty;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TUniformAutoSetProperty : public Designeditors::TPropertyEditor
{
	typedef Designeditors::TPropertyEditor inherited;
	
private:
	void __fastcall PassUniform(const System::UnicodeString S);
	
public:
	virtual Designintf::TPropertyAttributes __fastcall GetAttributes(void);
	virtual void __fastcall Edit(void);
public:
	/* TPropertyEditor.Create */ inline __fastcall virtual TUniformAutoSetProperty(const Designintf::_di_IDesigner ADesigner, int APropCount) : Designeditors::TPropertyEditor(ADesigner, APropCount) { }
	/* TPropertyEditor.Destroy */ inline __fastcall virtual ~TUniformAutoSetProperty(void) { }
	
};

#pragma pack(pop)

class DELPHICLASS TGLShaderEditorProperty;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLShaderEditorProperty : public Designeditors::TClassProperty
{
	typedef Designeditors::TClassProperty inherited;
	
protected:
	System::Classes::TStrings* __fastcall GetStrings(void);
	void __fastcall SetStrings(System::Classes::TStrings* const Value);
	void __fastcall OnShaderCheck(System::TObject* Sender);
	
public:
	virtual Designintf::TPropertyAttributes __fastcall GetAttributes(void);
	virtual void __fastcall Edit(void);
public:
	/* TPropertyEditor.Create */ inline __fastcall virtual TGLShaderEditorProperty(const Designintf::_di_IDesigner ADesigner, int APropCount) : Designeditors::TClassProperty(ADesigner, APropCount) { }
	/* TPropertyEditor.Destroy */ inline __fastcall virtual ~TGLShaderEditorProperty(void) { }
	
};

#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------
extern PACKAGE System::ResourceString _sOpenGLCategoryName;
#define Glsceneregister_sOpenGLCategoryName System::LoadResourceString(&Glsceneregister::_sOpenGLCategoryName)
extern PACKAGE Globjectmanager::TObjectManager* __fastcall ObjectManager(void);
extern PACKAGE void __fastcall Register(void);
}	/* namespace Glsceneregister */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLSCENEREGISTER)
using namespace Glsceneregister;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// GlsceneregisterHPP
