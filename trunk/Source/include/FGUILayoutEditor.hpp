// CodeGear C++Builder
// Copyright (c) 1995, 2012 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'FGUILayoutEditor.pas' rev: 24.00 (Win32)

#ifndef FguilayouteditorHPP
#define FguilayouteditorHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <Winapi.Windows.hpp>	// Pascal unit
#include <System.SysUtils.hpp>	// Pascal unit
#include <System.Variants.hpp>	// Pascal unit
#include <System.Classes.hpp>	// Pascal unit
#include <Vcl.Graphics.hpp>	// Pascal unit
#include <Vcl.Controls.hpp>	// Pascal unit
#include <Vcl.Forms.hpp>	// Pascal unit
#include <Vcl.Dialogs.hpp>	// Pascal unit
#include <Vcl.Buttons.hpp>	// Pascal unit
#include <Vcl.ExtDlgs.hpp>	// Pascal unit
#include <Vcl.StdCtrls.hpp>	// Pascal unit
#include <Vcl.ExtCtrls.hpp>	// Pascal unit
#include <Vcl.Samples.Spin.hpp>	// Pascal unit
#include <Vcl.Grids.hpp>	// Pascal unit
#include <GLCrossPlatform.hpp>	// Pascal unit
#include <BaseClasses.hpp>	// Pascal unit
#include <GLGui.hpp>	// Pascal unit
#include <Winapi.ShellAPI.hpp>	// Pascal unit
#include <System.UITypes.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Fguilayouteditor
{
//-- type declarations -------------------------------------------------------
class DELPHICLASS Tlayouts_form;
class PASCALIMPLEMENTATION Tlayouts_form : public Vcl::Forms::TForm
{
	typedef Vcl::Forms::TForm inherited;
	
__published:
	Vcl::Extctrls::TPanel* Panel1;
	Vcl::Extctrls::TPanel* Panel2;
	Vcl::Stdctrls::TListBox* items_list;
	Vcl::Stdctrls::TLabel* x_label;
	Vcl::Stdctrls::TLabel* y_label;
	Vcl::Buttons::TBitBtn* open_image_button;
	Vcl::Buttons::TBitBtn* open_button;
	Vcl::Buttons::TBitBtn* save_button;
	Vcl::Dialogs::TOpenDialog* OpenDialog1;
	Vcl::Dialogs::TSaveDialog* SaveDialog1;
	Vcl::Buttons::TBitBtn* delete_item_button;
	Vcl::Buttons::TBitBtn* add_button;
	Vcl::Stdctrls::TLabel* Label1;
	Vcl::Stdctrls::TLabel* Label2;
	Vcl::Samples::Spin::TSpinEdit* left_edit;
	Vcl::Samples::Spin::TSpinEdit* top_edit;
	Vcl::Stdctrls::TLabel* Label3;
	Vcl::Stdctrls::TLabel* Label4;
	Vcl::Samples::Spin::TSpinEdit* height_edit;
	Vcl::Samples::Spin::TSpinEdit* width_edit;
	Vcl::Stdctrls::TLabel* Label5;
	Vcl::Stdctrls::TEdit* name_edit;
	Vcl::Grids::TStringGrid* elements_grid;
	Vcl::Extctrls::TPanel* Panel3;
	Vcl::Buttons::TBitBtn* BitBtn4;
	Vcl::Buttons::TBitBtn* BitBtn5;
	Vcl::Forms::TScrollBox* ScrollBox1;
	Vcl::Extctrls::TImage* Image2;
	Vcl::Extctrls::TPaintBox* PaintBox1;
	Vcl::Extctrls::TImage* Image1;
	Vcl::Buttons::TBitBtn* BitBtn6;
	Vcl::Buttons::TBitBtn* BitBtn1;
	Glgui::TGLGuiLayout* GLGuiLayout1;
	void __fastcall open_image_buttonClick(System::TObject* Sender);
	void __fastcall open_buttonClick(System::TObject* Sender);
	void __fastcall save_buttonClick(System::TObject* Sender);
	void __fastcall FormCreate(System::TObject* Sender);
	void __fastcall Image1MouseMove(System::TObject* Sender, System::Classes::TShiftState Shift, int X, int Y);
	void __fastcall Image1MouseDown(System::TObject* Sender, System::Uitypes::TMouseButton Button, System::Classes::TShiftState Shift, int X, int Y);
	void __fastcall Image1MouseUp(System::TObject* Sender, System::Uitypes::TMouseButton Button, System::Classes::TShiftState Shift, int X, int Y);
	void __fastcall add_buttonClick(System::TObject* Sender);
	void __fastcall delete_item_buttonClick(System::TObject* Sender);
	void __fastcall items_listClick(System::TObject* Sender);
	void __fastcall name_editExit(System::TObject* Sender);
	void __fastcall name_editKeyPress(System::TObject* Sender, System::WideChar &Key);
	void __fastcall elements_gridClick(System::TObject* Sender);
	void __fastcall left_editChange(System::TObject* Sender);
	void __fastcall top_editChange(System::TObject* Sender);
	void __fastcall width_editChange(System::TObject* Sender);
	void __fastcall height_editChange(System::TObject* Sender);
	void __fastcall BitBtn4Click(System::TObject* Sender);
	void __fastcall BitBtn6Click(System::TObject* Sender);
	void __fastcall elements_gridDblClick(System::TObject* Sender);
	
private:
	void __fastcall SyncImages(void);
	void __fastcall DrawCurrentElement(void);
	void __fastcall RefreshComponentBox(void);
	bool __fastcall GetEnabledSpins(void);
	void __fastcall SetEnabledSpins(bool Value);
	
public:
	void __fastcall Execute(Glgui::TGLGuiLayout* AGUILayout);
	__property bool EnabledSpins = {read=GetEnabledSpins, write=SetEnabledSpins, nodefault};
public:
	/* TCustomForm.Create */ inline __fastcall virtual Tlayouts_form(System::Classes::TComponent* AOwner) : Vcl::Forms::TForm(AOwner) { }
	/* TCustomForm.CreateNew */ inline __fastcall virtual Tlayouts_form(System::Classes::TComponent* AOwner, int Dummy) : Vcl::Forms::TForm(AOwner, Dummy) { }
	/* TCustomForm.Destroy */ inline __fastcall virtual ~Tlayouts_form(void) { }
	
public:
	/* TWinControl.CreateParented */ inline __fastcall Tlayouts_form(HWND ParentWindow) : Vcl::Forms::TForm(ParentWindow) { }
	
};


//-- var, const, procedure ---------------------------------------------------
extern PACKAGE Tlayouts_form* __fastcall GUILayoutEditorForm(void);
extern PACKAGE void __fastcall ReleaseGUILayoutEditor(void);
}	/* namespace Fguilayouteditor */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_FGUILAYOUTEDITOR)
using namespace Fguilayouteditor;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// FguilayouteditorHPP
