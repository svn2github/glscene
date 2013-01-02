// CodeGear C++Builder
// Copyright (c) 1995, 2012 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'FLibMaterialPicker.pas' rev: 24.00 (Win32)

#ifndef FlibmaterialpickerHPP
#define FlibmaterialpickerHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <Vcl.Forms.hpp>	// Pascal unit
#include <Vcl.StdCtrls.hpp>	// Pascal unit
#include <Vcl.Buttons.hpp>	// Pascal unit
#include <Vcl.Controls.hpp>	// Pascal unit
#include <FRMaterialPreview.hpp>	// Pascal unit
#include <System.Classes.hpp>	// Pascal unit
#include <GLViewer.hpp>	// Pascal unit
#include <GLMaterial.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Flibmaterialpicker
{
//-- type declarations -------------------------------------------------------
class DELPHICLASS TLibMaterialPicker;
class PASCALIMPLEMENTATION TLibMaterialPicker : public Vcl::Forms::TForm
{
	typedef Vcl::Forms::TForm inherited;
	
__published:
	Vcl::Stdctrls::TListBox* LBMaterials;
	Vcl::Stdctrls::TLabel* Label1;
	Vcl::Stdctrls::TLabel* Label2;
	Vcl::Buttons::TBitBtn* BBOk;
	Vcl::Buttons::TBitBtn* BBCancel;
	Frmaterialpreview::TRMaterialPreview* MPPreview;
	void __fastcall LBMaterialsClick(System::TObject* Sender);
	void __fastcall LBMaterialsKeyPress(System::TObject* Sender, System::WideChar &Key);
	void __fastcall LBMaterialsDblClick(System::TObject* Sender);
	
public:
	bool __fastcall Execute(System::UnicodeString &materialName, Glmaterial::TGLAbstractMaterialLibrary* materialLibrary);
public:
	/* TCustomForm.Create */ inline __fastcall virtual TLibMaterialPicker(System::Classes::TComponent* AOwner) : Vcl::Forms::TForm(AOwner) { }
	/* TCustomForm.CreateNew */ inline __fastcall virtual TLibMaterialPicker(System::Classes::TComponent* AOwner, int Dummy) : Vcl::Forms::TForm(AOwner, Dummy) { }
	/* TCustomForm.Destroy */ inline __fastcall virtual ~TLibMaterialPicker(void) { }
	
public:
	/* TWinControl.CreateParented */ inline __fastcall TLibMaterialPicker(HWND ParentWindow) : Vcl::Forms::TForm(ParentWindow) { }
	
};


//-- var, const, procedure ---------------------------------------------------
extern PACKAGE TLibMaterialPicker* __fastcall LibMaterialPicker(void);
extern PACKAGE void __fastcall ReleaseLibMaterialPicker(void);
}	/* namespace Flibmaterialpicker */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_FLIBMATERIALPICKER)
using namespace Flibmaterialpicker;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// FlibmaterialpickerHPP
