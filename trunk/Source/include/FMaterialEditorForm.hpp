// CodeGear C++Builder
// Copyright (c) 1995, 2012 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'FMaterialEditorForm.pas' rev: 24.00 (Win32)

#ifndef FmaterialeditorformHPP
#define FmaterialeditorformHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <Winapi.Windows.hpp>	// Pascal unit
#include <Vcl.Forms.hpp>	// Pascal unit
#include <Vcl.ComCtrls.hpp>	// Pascal unit
#include <Vcl.StdCtrls.hpp>	// Pascal unit
#include <Vcl.Controls.hpp>	// Pascal unit
#include <Vcl.Buttons.hpp>	// Pascal unit
#include <FRMaterialPreview.hpp>	// Pascal unit
#include <FRColorEditor.hpp>	// Pascal unit
#include <FRFaceEditor.hpp>	// Pascal unit
#include <System.Classes.hpp>	// Pascal unit
#include <GLTexture.hpp>	// Pascal unit
#include <System.TypInfo.hpp>	// Pascal unit
#include <FRTextureEdit.hpp>	// Pascal unit
#include <GLViewer.hpp>	// Pascal unit
#include <GLMaterial.hpp>	// Pascal unit
#include <GLState.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Fmaterialeditorform
{
//-- type declarations -------------------------------------------------------
class DELPHICLASS TMaterialEditorForm;
class PASCALIMPLEMENTATION TMaterialEditorForm : public Vcl::Forms::TForm
{
	typedef Vcl::Forms::TForm inherited;
	
__published:
	Vcl::Comctrls::TPageControl* PageControl1;
	Vcl::Comctrls::TTabSheet* TSFront;
	Vcl::Comctrls::TTabSheet* TSBack;
	Vcl::Comctrls::TTabSheet* TSTexture;
	Frfaceeditor::TRFaceEditor* FEFront;
	Frfaceeditor::TRFaceEditor* FEBack;
	Vcl::Stdctrls::TGroupBox* GroupBox1;
	Frmaterialpreview::TRMaterialPreview* MPPreview;
	Vcl::Buttons::TBitBtn* BBOk;
	Vcl::Buttons::TBitBtn* BBCancel;
	Frtextureedit::TRTextureEdit* RTextureEdit;
	Vcl::Stdctrls::TComboBox* CBBlending;
	Vcl::Stdctrls::TLabel* Label1;
	Vcl::Stdctrls::TLabel* Label2;
	Vcl::Stdctrls::TComboBox* CBPolygonMode;
	void __fastcall OnMaterialChanged(System::TObject* Sender);
	
public:
	__fastcall virtual TMaterialEditorForm(System::Classes::TComponent* AOwner);
	bool __fastcall Execute(Glmaterial::TGLMaterial* AMaterial);
public:
	/* TCustomForm.CreateNew */ inline __fastcall virtual TMaterialEditorForm(System::Classes::TComponent* AOwner, int Dummy) : Vcl::Forms::TForm(AOwner, Dummy) { }
	/* TCustomForm.Destroy */ inline __fastcall virtual ~TMaterialEditorForm(void) { }
	
public:
	/* TWinControl.CreateParented */ inline __fastcall TMaterialEditorForm(HWND ParentWindow) : Vcl::Forms::TForm(ParentWindow) { }
	
};


//-- var, const, procedure ---------------------------------------------------
extern PACKAGE TMaterialEditorForm* __fastcall MaterialEditorForm(void);
extern PACKAGE void __fastcall ReleaseMaterialEditorForm(void);
}	/* namespace Fmaterialeditorform */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_FMATERIALEDITORFORM)
using namespace Fmaterialeditorform;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// FmaterialeditorformHPP
