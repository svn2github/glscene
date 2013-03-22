// CodeGear C++Builder
// Copyright (c) 1995, 2012 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'FRFaceEditor.pas' rev: 24.00 (Win32)

#ifndef FrfaceeditorHPP
#define FrfaceeditorHPP

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
#include <Vcl.ImgList.hpp>	// Pascal unit
#include <Vcl.Controls.hpp>	// Pascal unit
#include <FRTrackBarEdit.hpp>	// Pascal unit
#include <FRColorEditor.hpp>	// Pascal unit
#include <System.Classes.hpp>	// Pascal unit
#include <GLTexture.hpp>	// Pascal unit
#include <GLMaterial.hpp>	// Pascal unit
#include <GLState.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Frfaceeditor
{
//-- type declarations -------------------------------------------------------
class DELPHICLASS TRFaceEditor;
class PASCALIMPLEMENTATION TRFaceEditor : public Vcl::Forms::TFrame
{
	typedef Vcl::Forms::TFrame inherited;
	
__published:
	Vcl::Comctrls::TPageControl* PageControl;
	Vcl::Comctrls::TTabSheet* TSAmbient;
	Vcl::Comctrls::TTabSheet* TSDiffuse;
	Vcl::Comctrls::TTabSheet* TSEmission;
	Vcl::Comctrls::TTabSheet* TSSpecular;
	Frcoloreditor::TRColorEditor* CEAmbiant;
	Vcl::Stdctrls::TLabel* Label1;
	Frtrackbaredit::TRTrackBarEdit* TBEShininess;
	Vcl::Controls::TImageList* ImageList;
	Frcoloreditor::TRColorEditor* CEDiffuse;
	Frcoloreditor::TRColorEditor* CEEmission;
	Frcoloreditor::TRColorEditor* CESpecular;
	void __fastcall TBEShininessTrackBarChange(System::TObject* Sender);
	
private:
	System::Classes::TNotifyEvent FOnChange;
	bool updating;
	Glmaterial::TGLFaceProperties* FFaceProperties;
	void __fastcall SetGLFaceProperties(Glmaterial::TGLFaceProperties* const val);
	void __fastcall OnColorChange(System::TObject* Sender);
	
public:
	__fastcall virtual TRFaceEditor(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TRFaceEditor(void);
	__property System::Classes::TNotifyEvent OnChange = {read=FOnChange, write=FOnChange};
	__property Glmaterial::TGLFaceProperties* FaceProperties = {read=FFaceProperties, write=SetGLFaceProperties};
public:
	/* TWinControl.CreateParented */ inline __fastcall TRFaceEditor(HWND ParentWindow) : Vcl::Forms::TFrame(ParentWindow) { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Frfaceeditor */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_FRFACEEDITOR)
using namespace Frfaceeditor;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// FrfaceeditorHPP
