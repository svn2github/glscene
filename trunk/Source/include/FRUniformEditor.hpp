// CodeGear C++Builder
// Copyright (c) 1995, 2012 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'FRUniformEditor.pas' rev: 24.00 (Win32)

#ifndef FruniformeditorHPP
#define FruniformeditorHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <System.SysUtils.hpp>	// Pascal unit
#include <System.Variants.hpp>	// Pascal unit
#include <System.Classes.hpp>	// Pascal unit
#include <Vcl.Graphics.hpp>	// Pascal unit
#include <Vcl.Controls.hpp>	// Pascal unit
#include <Vcl.Forms.hpp>	// Pascal unit
#include <Vcl.Dialogs.hpp>	// Pascal unit
#include <Vcl.StdCtrls.hpp>	// Pascal unit
#include <Vcl.ExtCtrls.hpp>	// Pascal unit
#include <Vcl.Buttons.hpp>	// Pascal unit
#include <GLSLParameter.hpp>	// Pascal unit
#include <GLTextureFormat.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Fruniformeditor
{
//-- type declarations -------------------------------------------------------
class DELPHICLASS TShaderUniformEditor;
class PASCALIMPLEMENTATION TShaderUniformEditor : public Vcl::Forms::TForm
{
	typedef Vcl::Forms::TForm inherited;
	
private:
	typedef System::DynamicArray<Glslparameter::_di_IShaderParameter> _TShaderUniformEditor__1;
	
	
__published:
	Vcl::Stdctrls::TListBox* LBUniforms;
	Vcl::Stdctrls::TLabel* Labe1;
	Vcl::Stdctrls::TComboBox* AutoSetBox;
	Vcl::Stdctrls::TComboBox* SamplerBox;
	Vcl::Extctrls::TPanel* Panel1;
	Vcl::Extctrls::TRadioGroup* RedGroup;
	Vcl::Extctrls::TRadioGroup* GreenGroup;
	Vcl::Extctrls::TRadioGroup* BlueGroup;
	Vcl::Extctrls::TRadioGroup* AlphaGroup;
	Vcl::Stdctrls::TLabel* Label1;
	Vcl::Stdctrls::TLabel* Label2;
	Vcl::Stdctrls::TLabel* Label3;
	Vcl::Stdctrls::TLabel* Label4;
	Vcl::Stdctrls::TComboBox* TextureBox;
	Vcl::Stdctrls::TButton* Button1;
	void __fastcall FormDestroy(System::TObject* Sender);
	void __fastcall LBUniformsClick(System::TObject* Sender);
	void __fastcall ColorGroupClick(System::TObject* Sender);
	void __fastcall AutoSetBoxChange(System::TObject* Sender);
	void __fastcall TextureBoxChange(System::TObject* Sender);
	void __fastcall SamplerBoxChange(System::TObject* Sender);
	void __fastcall LBUniformsKeyPress(System::TObject* Sender, System::WideChar &Key);
	
private:
	_TShaderUniformEditor__1 FUniformList;
	
public:
	void __fastcall Clear(void);
	void __fastcall AddTextureName(const System::UnicodeString S);
	void __fastcall AddSamplerName(const System::UnicodeString S);
	void __fastcall AddUniform(Glslparameter::_di_IShaderParameter AValue);
	void __fastcall Execute(void);
public:
	/* TCustomForm.Create */ inline __fastcall virtual TShaderUniformEditor(System::Classes::TComponent* AOwner) : Vcl::Forms::TForm(AOwner) { }
	/* TCustomForm.CreateNew */ inline __fastcall virtual TShaderUniformEditor(System::Classes::TComponent* AOwner, int Dummy) : Vcl::Forms::TForm(AOwner, Dummy) { }
	/* TCustomForm.Destroy */ inline __fastcall virtual ~TShaderUniformEditor(void) { }
	
public:
	/* TWinControl.CreateParented */ inline __fastcall TShaderUniformEditor(HWND ParentWindow) : Vcl::Forms::TForm(ParentWindow) { }
	
};


//-- var, const, procedure ---------------------------------------------------
extern PACKAGE TShaderUniformEditor* __fastcall ShaderUniformEditor(void);
extern PACKAGE void __fastcall ReleaseShaderUniformEditor(void);
}	/* namespace Fruniformeditor */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_FRUNIFORMEDITOR)
using namespace Fruniformeditor;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// FruniformeditorHPP
