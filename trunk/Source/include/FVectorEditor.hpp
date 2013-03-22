// CodeGear C++Builder
// Copyright (c) 1995, 2012 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'FVectorEditor.pas' rev: 24.00 (Win32)

#ifndef FvectoreditorHPP
#define FvectoreditorHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <Vcl.Forms.hpp>	// Pascal unit
#include <Vcl.ComCtrls.hpp>	// Pascal unit
#include <Vcl.StdCtrls.hpp>	// Pascal unit
#include <Vcl.ToolWin.hpp>	// Pascal unit
#include <Vcl.ExtCtrls.hpp>	// Pascal unit
#include <Vcl.Buttons.hpp>	// Pascal unit
#include <Vcl.Graphics.hpp>	// Pascal unit
#include <Vcl.Controls.hpp>	// Pascal unit
#include <System.Classes.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Fvectoreditor
{
//-- type declarations -------------------------------------------------------
class DELPHICLASS TVectorEditorForm;
class PASCALIMPLEMENTATION TVectorEditorForm : public Vcl::Forms::TForm
{
	typedef Vcl::Forms::TForm inherited;
	
__published:
	Vcl::Stdctrls::TEdit* EDx;
	Vcl::Stdctrls::TLabel* Label1;
	Vcl::Stdctrls::TLabel* Label2;
	Vcl::Stdctrls::TLabel* Label3;
	Vcl::Stdctrls::TEdit* EDy;
	Vcl::Stdctrls::TEdit* EDz;
	Vcl::Buttons::TBitBtn* BBok;
	Vcl::Buttons::TBitBtn* BBcancel;
	Vcl::Extctrls::TImage* IMx;
	Vcl::Extctrls::TImage* IMy;
	Vcl::Extctrls::TImage* IMz;
	Vcl::Buttons::TSpeedButton* SpeedButton1;
	Vcl::Buttons::TSpeedButton* SBmX;
	Vcl::Buttons::TSpeedButton* SpeedButton3;
	Vcl::Buttons::TSpeedButton* SBmY;
	Vcl::Buttons::TSpeedButton* SpeedButton5;
	Vcl::Buttons::TSpeedButton* SBmZ;
	Vcl::Buttons::TSpeedButton* SpeedButton7;
	Vcl::Buttons::TSpeedButton* SBUnit;
	Vcl::Buttons::TSpeedButton* SpeedButton9;
	Vcl::Extctrls::TBevel* Bevel1;
	Vcl::Buttons::TSpeedButton* SBInvert;
	void __fastcall TBxClick(System::TObject* Sender);
	void __fastcall TByClick(System::TObject* Sender);
	void __fastcall TBzClick(System::TObject* Sender);
	void __fastcall TBnullClick(System::TObject* Sender);
	void __fastcall EDxChange(System::TObject* Sender);
	void __fastcall EDyChange(System::TObject* Sender);
	void __fastcall EDzChange(System::TObject* Sender);
	void __fastcall SBmXClick(System::TObject* Sender);
	void __fastcall SBmYClick(System::TObject* Sender);
	void __fastcall SBmZClick(System::TObject* Sender);
	void __fastcall SBUnitClick(System::TObject* Sender);
	void __fastcall SpeedButton9Click(System::TObject* Sender);
	void __fastcall SBInvertClick(System::TObject* Sender);
	
private:
	float vx;
	float vy;
	float vz;
	void __fastcall TestInput(Vcl::Stdctrls::TEdit* edit, Vcl::Extctrls::TImage* imError, float &dest);
	
public:
	bool __fastcall Execute(float &x, float &y, float &z);
public:
	/* TCustomForm.Create */ inline __fastcall virtual TVectorEditorForm(System::Classes::TComponent* AOwner) : Vcl::Forms::TForm(AOwner) { }
	/* TCustomForm.CreateNew */ inline __fastcall virtual TVectorEditorForm(System::Classes::TComponent* AOwner, int Dummy) : Vcl::Forms::TForm(AOwner, Dummy) { }
	/* TCustomForm.Destroy */ inline __fastcall virtual ~TVectorEditorForm(void) { }
	
public:
	/* TWinControl.CreateParented */ inline __fastcall TVectorEditorForm(HWND ParentWindow) : Vcl::Forms::TForm(ParentWindow) { }
	
};


//-- var, const, procedure ---------------------------------------------------
extern PACKAGE TVectorEditorForm* __fastcall VectorEditorForm(void);
extern PACKAGE void __fastcall ReleaseVectorEditorForm(void);
}	/* namespace Fvectoreditor */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_FVECTOREDITOR)
using namespace Fvectoreditor;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// FvectoreditorHPP
