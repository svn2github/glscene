// CodeGear C++Builder
// Copyright (c) 1995, 2012 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'PlugInManagerPropEditor.pas' rev: 24.00 (Win32)

#ifndef PluginmanagerpropeditorHPP
#define PluginmanagerpropeditorHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <Vcl.Forms.hpp>	// Pascal unit
#include <Vcl.Dialogs.hpp>	// Pascal unit
#include <Vcl.StdCtrls.hpp>	// Pascal unit
#include <Vcl.Controls.hpp>	// Pascal unit
#include <Vcl.Buttons.hpp>	// Pascal unit
#include <System.Classes.hpp>	// Pascal unit
#include <PlugInManager.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Pluginmanagerpropeditor
{
//-- type declarations -------------------------------------------------------
class DELPHICLASS TPlugInManagerPropForm;
class PASCALIMPLEMENTATION TPlugInManagerPropForm : public Vcl::Forms::TForm
{
	typedef Vcl::Forms::TForm inherited;
	
__published:
	Vcl::Dialogs::TOpenDialog* OpenDialog;
	Vcl::Stdctrls::TListBox* ListBox;
	Vcl::Stdctrls::TLabel* Label1;
	Vcl::Buttons::TSpeedButton* OKButton;
	Vcl::Buttons::TSpeedButton* LoadButton;
	Vcl::Buttons::TSpeedButton* UnloadButton;
	Vcl::Stdctrls::TGroupBox* GroupBox;
	Vcl::Stdctrls::TMemo* DescriptionMemo;
	Vcl::Stdctrls::TLabel* Label2;
	Vcl::Stdctrls::TLabel* Label3;
	Vcl::Stdctrls::TLabel* DateLabel;
	Vcl::Stdctrls::TLabel* SizeLabel;
	Vcl::Stdctrls::TLabel* Label4;
	Vcl::Stdctrls::TLabel* Label5;
	Vcl::Stdctrls::TComboBox* ServiceBox;
	Vcl::Stdctrls::TComboBox* NameBox;
	void __fastcall OKButtonClick(System::TObject* Sender);
	void __fastcall LoadButtonClick(System::TObject* Sender);
	void __fastcall ListBoxClick(System::TObject* Sender);
	void __fastcall UnloadButtonClick(System::TObject* Sender);
	void __fastcall ServiceBoxChange(System::TObject* Sender);
	
private:
	Pluginmanager::TPlugInManager* FManager;
	
public:
	__classmethod void __fastcall EditPlugIns(Pluginmanager::TPlugInManager* AManager);
public:
	/* TCustomForm.Create */ inline __fastcall virtual TPlugInManagerPropForm(System::Classes::TComponent* AOwner) : Vcl::Forms::TForm(AOwner) { }
	/* TCustomForm.CreateNew */ inline __fastcall virtual TPlugInManagerPropForm(System::Classes::TComponent* AOwner, int Dummy) : Vcl::Forms::TForm(AOwner, Dummy) { }
	/* TCustomForm.Destroy */ inline __fastcall virtual ~TPlugInManagerPropForm(void) { }
	
public:
	/* TWinControl.CreateParented */ inline __fastcall TPlugInManagerPropForm(HWND ParentWindow) : Vcl::Forms::TForm(ParentWindow) { }
	
};


//-- var, const, procedure ---------------------------------------------------
extern PACKAGE TPlugInManagerPropForm* PlugInManagerPropForm;
}	/* namespace Pluginmanagerpropeditor */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_PLUGINMANAGERPROPEDITOR)
using namespace Pluginmanagerpropeditor;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// PluginmanagerpropeditorHPP
