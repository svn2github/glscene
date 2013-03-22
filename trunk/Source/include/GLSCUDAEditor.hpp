// CodeGear C++Builder
// Copyright (c) 1995, 2012 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLSCUDAEditor.pas' rev: 24.00 (Win32)

#ifndef GlscudaeditorHPP
#define GlscudaeditorHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <System.Win.Registry.hpp>	// Pascal unit
#include <Winapi.Windows.hpp>	// Pascal unit
#include <Winapi.Messages.hpp>	// Pascal unit
#include <System.SysUtils.hpp>	// Pascal unit
#include <System.Variants.hpp>	// Pascal unit
#include <System.Classes.hpp>	// Pascal unit
#include <Vcl.Graphics.hpp>	// Pascal unit
#include <Vcl.Controls.hpp>	// Pascal unit
#include <Vcl.Forms.hpp>	// Pascal unit
#include <Vcl.Dialogs.hpp>	// Pascal unit
#include <Vcl.ImgList.hpp>	// Pascal unit
#include <Vcl.StdCtrls.hpp>	// Pascal unit
#include <Vcl.ComCtrls.hpp>	// Pascal unit
#include <Vcl.ToolWin.hpp>	// Pascal unit
#include <DesignIntf.hpp>	// Pascal unit
#include <VCLEditors.hpp>	// Pascal unit
#include <GLSCUDA.hpp>	// Pascal unit
#include <GLSCUDAFFTPlan.hpp>	// Pascal unit
#include <GLSCUDAGraphics.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Glscudaeditor
{
//-- type declarations -------------------------------------------------------
class DELPHICLASS TGLSCUDAEditorForm;
class PASCALIMPLEMENTATION TGLSCUDAEditorForm : public Vcl::Forms::TForm
{
	typedef Vcl::Forms::TForm inherited;
	
__published:
	Vcl::Comctrls::TToolBar* ToolBar1;
	Vcl::Comctrls::TToolButton* AddModuleButton;
	Vcl::Comctrls::TToolButton* DeleteButton;
	Vcl::Stdctrls::TListBox* ListBox1;
	Vcl::Controls::TImageList* ImageList1;
	Vcl::Comctrls::TToolButton* AddMemDataButton;
	Vcl::Comctrls::TToolButton* AddFFTPlanButton;
	Vcl::Comctrls::TToolButton* AddGeometryResButton;
	Vcl::Comctrls::TToolButton* AddImageResButton;
	void __fastcall AddItemButtonClick(System::TObject* Sender);
	void __fastcall DeleteButtonClick(System::TObject* Sender);
	void __fastcall FormCreate(System::TObject* Sender);
	void __fastcall ListBox1Click(System::TObject* Sender);
	void __fastcall FormDestroy(System::TObject* Sender);
	
private:
	System::Classes::TList* FClassList;
	Glscuda::TGLSCUDA* FCUDA;
	Designintf::_di_IDesigner FCurrentDesigner;
	
protected:
	virtual void __fastcall Notification(System::Classes::TComponent* AComponent, System::Classes::TOperation Operation);
	void __fastcall OnCUDAComponentNameChanged(System::TObject* Sender);
	
public:
	void __fastcall SetCUDAEditorClient(Glscuda::TGLSCUDA* Client, Designintf::_di_IDesigner Designer);
public:
	/* TCustomForm.Create */ inline __fastcall virtual TGLSCUDAEditorForm(System::Classes::TComponent* AOwner) : Vcl::Forms::TForm(AOwner) { }
	/* TCustomForm.CreateNew */ inline __fastcall virtual TGLSCUDAEditorForm(System::Classes::TComponent* AOwner, int Dummy) : Vcl::Forms::TForm(AOwner, Dummy) { }
	/* TCustomForm.Destroy */ inline __fastcall virtual ~TGLSCUDAEditorForm(void) { }
	
public:
	/* TWinControl.CreateParented */ inline __fastcall TGLSCUDAEditorForm(HWND ParentWindow) : Vcl::Forms::TForm(ParentWindow) { }
	
};


//-- var, const, procedure ---------------------------------------------------
extern PACKAGE TGLSCUDAEditorForm* __fastcall GLSCUDAEditorForm(void);
extern PACKAGE void __fastcall ReleaseGLSCUDAEditorForm(void);
}	/* namespace Glscudaeditor */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLSCUDAEDITOR)
using namespace Glscudaeditor;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// GlscudaeditorHPP
