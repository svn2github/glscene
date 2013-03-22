// CodeGear C++Builder
// Copyright (c) 1995, 2012 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'FRColorEditor.pas' rev: 24.00 (Win32)

#ifndef FrcoloreditorHPP
#define FrcoloreditorHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <Winapi.Windows.hpp>	// Pascal unit
#include <Vcl.Forms.hpp>	// Pascal unit
#include <Vcl.StdCtrls.hpp>	// Pascal unit
#include <Vcl.ComCtrls.hpp>	// Pascal unit
#include <Vcl.ExtCtrls.hpp>	// Pascal unit
#include <Vcl.Dialogs.hpp>	// Pascal unit
#include <Vcl.Controls.hpp>	// Pascal unit
#include <Vcl.Graphics.hpp>	// Pascal unit
#include <System.Classes.hpp>	// Pascal unit
#include <VectorGeometry.hpp>	// Pascal unit
#include <FRTrackBarEdit.hpp>	// Pascal unit
#include <System.SysUtils.hpp>	// Pascal unit
#include <GLColor.hpp>	// Pascal unit
#include <System.UITypes.hpp>	// Pascal unit
#include <VectorTypes.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Frcoloreditor
{
//-- type declarations -------------------------------------------------------
class DELPHICLASS TRColorEditor;
class PASCALIMPLEMENTATION TRColorEditor : public Vcl::Forms::TFrame
{
	typedef Vcl::Forms::TFrame inherited;
	
private:
	enum _TRColorEditor__1 : unsigned char { None, Red, Green, Blue, Alpha };
	
	
__published:
	Vcl::Stdctrls::TLabel* Label1;
	Vcl::Stdctrls::TLabel* Label2;
	Vcl::Stdctrls::TLabel* Label3;
	Vcl::Stdctrls::TLabel* Label4;
	Vcl::Extctrls::TPanel* PAPreview;
	Vcl::Dialogs::TColorDialog* ColorDialog;
	Vcl::Extctrls::TPanel* Panel1;
	Vcl::Extctrls::TPaintBox* ColorEditorPaintBox;
	Vcl::Stdctrls::TEdit* RedEdit;
	Vcl::Stdctrls::TEdit* GreenEdit;
	Vcl::Stdctrls::TEdit* BlueEdit;
	Vcl::Stdctrls::TEdit* AlphaEdit;
	void __fastcall TBEChange(System::TObject* Sender);
	void __fastcall PAPreviewDblClick(System::TObject* Sender);
	void __fastcall ColorEditorPaintBoxPaint(System::TObject* Sender);
	void __fastcall FrameResize(System::TObject* Sender);
	void __fastcall ColorEditorPaintBoxMouseDown(System::TObject* Sender, System::Uitypes::TMouseButton Button, System::Classes::TShiftState Shift, int X, int Y);
	void __fastcall ColorEditorPaintBoxMouseMove(System::TObject* Sender, System::Classes::TShiftState Shift, int X, int Y);
	void __fastcall ColorEditorPaintBoxMouseUp(System::TObject* Sender, System::Uitypes::TMouseButton Button, System::Classes::TShiftState Shift, int X, int Y);
	void __fastcall RedEditChange(System::TObject* Sender);
	void __fastcall GreenEditChange(System::TObject* Sender);
	void __fastcall BlueEditChange(System::TObject* Sender);
	void __fastcall AlphaEditChange(System::TObject* Sender);
	
private:
	System::Classes::TNotifyEvent FOnChange;
	bool updating;
	Vcl::Graphics::TBitmap* WorkBitmap;
	int RedValue;
	int GreenValue;
	int BlueValue;
	int AlphaVAlue;
	_TRColorEditor__1 DraggingValue;
	HIDESBASE void __fastcall SetColor(const Vectortypes::TVector4f &val);
	Vectortypes::TVector4f __fastcall GetColor(void);
	void __fastcall DrawContents(void);
	void __fastcall DragColorSliderToPosition(int XPos);
	void __fastcall ContentsChanged(void);
	
public:
	__fastcall virtual TRColorEditor(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TRColorEditor(void);
	__property Vectortypes::TVector4f Color = {read=GetColor, write=SetColor};
	
__published:
	__property System::Classes::TNotifyEvent OnChange = {read=FOnChange, write=FOnChange};
public:
	/* TWinControl.CreateParented */ inline __fastcall TRColorEditor(HWND ParentWindow) : Vcl::Forms::TFrame(ParentWindow) { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Frcoloreditor */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_FRCOLOREDITOR)
using namespace Frcoloreditor;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// FrcoloreditorHPP
