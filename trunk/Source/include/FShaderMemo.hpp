// CodeGear C++Builder
// Copyright (c) 1995, 2012 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'FShaderMemo.pas' rev: 24.00 (Win32)

#ifndef FshadermemoHPP
#define FshadermemoHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <Winapi.Windows.hpp>	// Pascal unit
#include <Winapi.Messages.hpp>	// Pascal unit
#include <System.SysUtils.hpp>	// Pascal unit
#include <System.Variants.hpp>	// Pascal unit
#include <System.Classes.hpp>	// Pascal unit
#include <Vcl.Controls.hpp>	// Pascal unit
#include <Vcl.Forms.hpp>	// Pascal unit
#include <Vcl.ComCtrls.hpp>	// Pascal unit
#include <Vcl.ImgList.hpp>	// Pascal unit
#include <Vcl.Dialogs.hpp>	// Pascal unit
#include <Vcl.Menus.hpp>	// Pascal unit
#include <Vcl.ActnList.hpp>	// Pascal unit
#include <Vcl.ToolWin.hpp>	// Pascal unit
#include <Vcl.ExtCtrls.hpp>	// Pascal unit
#include <Vcl.StdCtrls.hpp>	// Pascal unit
#include <Vcl.Graphics.hpp>	// Pascal unit
#include <GLSMemo.hpp>	// Pascal unit
#include <System.Types.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Fshadermemo
{
//-- type declarations -------------------------------------------------------
class DELPHICLASS TGLShaderEditor;
class PASCALIMPLEMENTATION TGLShaderEditor : public Vcl::Forms::TForm
{
	typedef Vcl::Forms::TForm inherited;
	
__published:
	Vcl::Controls::TImageList* ImageList;
	Vcl::Comctrls::TToolBar* ToolBar;
	Vcl::Comctrls::TToolButton* TBOpen;
	Vcl::Comctrls::TToolButton* TBSave;
	Vcl::Comctrls::TToolButton* TBStayOnTop;
	Vcl::Comctrls::TToolButton* TBHelp;
	Vcl::Comctrls::TToolButton* ToolButton2;
	Vcl::Comctrls::TToolButton* TBCopy;
	Vcl::Comctrls::TToolButton* TBPaste;
	Vcl::Comctrls::TToolButton* TBCut;
	Vcl::Comctrls::TToolButton* ToolButton10;
	Vcl::Comctrls::TToolButton* TBTemplate;
	Vcl::Comctrls::TToolButton* TBUndo;
	Vcl::Comctrls::TToolButton* TBRedo;
	Vcl::Comctrls::TToolButton* ToolButton4;
	Glsmemo::TGLSSynHiMemo* GLSLMemo;
	Vcl::Dialogs::TOpenDialog* OpenDialog;
	Vcl::Dialogs::TSaveDialog* SaveDialog;
	Vcl::Menus::TPopupMenu* TemplateMenu;
	Vcl::Menus::TMenuItem* GLSL120;
	Vcl::Menus::TMenuItem* GLSL330;
	Vcl::Menus::TMenuItem* GLSL400;
	Vcl::Menus::TMenuItem* N1;
	Vcl::Menus::TMenuItem* N2;
	Vcl::Stdctrls::TMemo* CompilatorLog;
	Vcl::Comctrls::TToolButton* TBIncIndent;
	Vcl::Comctrls::TToolButton* TBDecIndent;
	Vcl::Comctrls::TToolButton* TBComment;
	Vcl::Comctrls::TToolButton* TBUncoment;
	Vcl::Comctrls::TToolButton* ToolButton1;
	Vcl::Extctrls::TPanel* Panel1;
	Vcl::Stdctrls::TButton* CancelButton;
	Vcl::Stdctrls::TButton* OKButton;
	Vcl::Stdctrls::TButton* CheckButton;
	Vcl::Extctrls::TSplitter* Splitter1;
	void __fastcall FormCreate(System::TObject* Sender);
	void __fastcall FormDestroy(System::TObject* Sender);
	void __fastcall GLSLMemoGutterClick(System::TObject* Sender, int LineNo);
	void __fastcall GLSLMemoGutterDraw(System::TObject* Sender, Vcl::Graphics::TCanvas* ACanvas, int LineNo, const System::Types::TRect &rct);
	void __fastcall TBOpenClick(System::TObject* Sender);
	void __fastcall TBSaveClick(System::TObject* Sender);
	void __fastcall TBStayOnTopClick(System::TObject* Sender);
	void __fastcall TBUndoClick(System::TObject* Sender);
	void __fastcall GLSLMemoUndoChange(System::TObject* Sender, bool CanUndo, bool CanRedo);
	void __fastcall TBRedoClick(System::TObject* Sender);
	void __fastcall TBCopyClick(System::TObject* Sender);
	void __fastcall TBPasteClick(System::TObject* Sender);
	void __fastcall TBCutClick(System::TObject* Sender);
	void __fastcall CheckButtonClick(System::TObject* Sender);
	void __fastcall TBIncIndentClick(System::TObject* Sender);
	void __fastcall TBDecIndentClick(System::TObject* Sender);
	void __fastcall TBCommentClick(System::TObject* Sender);
	void __fastcall TBUncomentClick(System::TObject* Sender);
	void __fastcall FormShow(System::TObject* Sender);
	
private:
	int FLightLineStyle;
	System::Classes::TNotifyEvent FOnCheck;
	void __fastcall OnTemplateClick(System::TObject* Sender);
	
public:
	__property System::Classes::TNotifyEvent OnCheck = {read=FOnCheck, write=FOnCheck};
public:
	/* TCustomForm.Create */ inline __fastcall virtual TGLShaderEditor(System::Classes::TComponent* AOwner) : Vcl::Forms::TForm(AOwner) { }
	/* TCustomForm.CreateNew */ inline __fastcall virtual TGLShaderEditor(System::Classes::TComponent* AOwner, int Dummy) : Vcl::Forms::TForm(AOwner, Dummy) { }
	/* TCustomForm.Destroy */ inline __fastcall virtual ~TGLShaderEditor(void) { }
	
public:
	/* TWinControl.CreateParented */ inline __fastcall TGLShaderEditor(HWND ParentWindow) : Vcl::Forms::TForm(ParentWindow) { }
	
};


//-- var, const, procedure ---------------------------------------------------
extern PACKAGE TGLShaderEditor* __fastcall GLShaderEditorForm(void);
extern PACKAGE void __fastcall ReleaseGLShaderEditor(void);
}	/* namespace Fshadermemo */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_FSHADERMEMO)
using namespace Fshadermemo;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// FshadermemoHPP
