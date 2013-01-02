// CodeGear C++Builder
// Copyright (c) 1995, 2012 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'Info.pas' rev: 24.00 (Win32)

#ifndef InfoHPP
#define InfoHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <Winapi.Windows.hpp>	// Pascal unit
#include <Vcl.Forms.hpp>	// Pascal unit
#include <Vcl.Controls.hpp>	// Pascal unit
#include <Vcl.Buttons.hpp>	// Pascal unit
#include <Vcl.StdCtrls.hpp>	// Pascal unit
#include <Vcl.ComCtrls.hpp>	// Pascal unit
#include <Vcl.ExtCtrls.hpp>	// Pascal unit
#include <Vcl.Graphics.hpp>	// Pascal unit
#include <Vcl.Menus.hpp>	// Pascal unit
#include <Vcl.Imaging.jpeg.hpp>	// Pascal unit
#include <GLScene.hpp>	// Pascal unit
#include <System.Classes.hpp>	// Pascal unit
#include <System.UITypes.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Info
{
//-- type declarations -------------------------------------------------------
class DELPHICLASS TInfoForm;
class PASCALIMPLEMENTATION TInfoForm : public Vcl::Forms::TForm
{
	typedef Vcl::Forms::TForm inherited;
	
__published:
	Vcl::Stdctrls::TLabel* AccLabel;
	Vcl::Stdctrls::TLabel* AccumLabel;
	Vcl::Stdctrls::TLabel* AuxLabel;
	Vcl::Stdctrls::TLabel* ClipLabel;
	Vcl::Stdctrls::TLabel* ColorLabel;
	Vcl::Stdctrls::TLabel* CopyLabel;
	Vcl::Stdctrls::TLabel* DepthLabel;
	Vcl::Stdctrls::TLabel* DoubleLabel;
	Vcl::Stdctrls::TLabel* EvalLabel;
	Vcl::Extctrls::TImage* Image1;
	Vcl::Stdctrls::TLabel* Label1;
	Vcl::Stdctrls::TLabel* Label10;
	Vcl::Stdctrls::TLabel* Label11;
	Vcl::Stdctrls::TLabel* Label12;
	Vcl::Stdctrls::TLabel* Label13;
	Vcl::Stdctrls::TLabel* Label14;
	Vcl::Stdctrls::TLabel* Label15;
	Vcl::Stdctrls::TLabel* Label16;
	Vcl::Stdctrls::TLabel* Label17;
	Vcl::Stdctrls::TLabel* Label18;
	Vcl::Stdctrls::TLabel* Label2;
	Vcl::Stdctrls::TLabel* Label20;
	Vcl::Stdctrls::TLabel* Label23;
	Vcl::Stdctrls::TLabel* Label25;
	Vcl::Stdctrls::TLabel* Label26;
	Vcl::Stdctrls::TLabel* Label27;
	Vcl::Stdctrls::TLabel* Label28;
	Vcl::Stdctrls::TLabel* Label29;
	Vcl::Stdctrls::TLabel* Label3;
	Vcl::Stdctrls::TLabel* Label30;
	Vcl::Stdctrls::TLabel* Label31;
	Vcl::Stdctrls::TLabel* Label32;
	Vcl::Stdctrls::TLabel* Label33;
	Vcl::Stdctrls::TLabel* Label34;
	Vcl::Stdctrls::TLabel* Label35;
	Vcl::Stdctrls::TLabel* Label37;
	Vcl::Stdctrls::TLabel* Label4;
	Vcl::Stdctrls::TLabel* Label5;
	Vcl::Stdctrls::TLabel* Label6;
	Vcl::Stdctrls::TLabel* Label7;
	Vcl::Stdctrls::TLabel* Label8;
	Vcl::Stdctrls::TLabel* Label9;
	Vcl::Stdctrls::TLabel* LightLabel;
	Vcl::Stdctrls::TLabel* ListLabel;
	Vcl::Stdctrls::TMemo* Memo1;
	Vcl::Stdctrls::TMemo* Contributors;
	Vcl::Stdctrls::TLabel* ModelLabel;
	Vcl::Stdctrls::TLabel* NameLabel;
	Vcl::Stdctrls::TLabel* OverlayLabel;
	Vcl::Comctrls::TPageControl* PageControl;
	Vcl::Stdctrls::TLabel* PixelLabel;
	Vcl::Stdctrls::TLabel* ProjLabel;
	Vcl::Stdctrls::TLabel* RendererLabel;
	Vcl::Forms::TScrollBox* ScrollBox1;
	Vcl::Comctrls::TTabSheet* TabSheet4;
	Vcl::Stdctrls::TLabel* StencilLabel;
	Vcl::Stdctrls::TLabel* StereoLabel;
	Vcl::Stdctrls::TLabel* SubLabel;
	Vcl::Comctrls::TTabSheet* TabSheet2;
	Vcl::Comctrls::TTabSheet* TabSheet3;
	Vcl::Stdctrls::TLabel* TexSizeLabel;
	Vcl::Stdctrls::TLabel* TexStackLabel;
	Vcl::Stdctrls::TLabel* TexUnitsLabel;
	Vcl::Stdctrls::TLabel* UnderlayLabel;
	Vcl::Stdctrls::TLabel* VendorLabel;
	Vcl::Stdctrls::TLabel* VersionLabel;
	Vcl::Comctrls::TTabSheet* TabSheet5;
	Vcl::Stdctrls::TListBox* Extensions;
	Vcl::Menus::TPopupMenu* PMWebLink;
	Vcl::Menus::TMenuItem* MIRegistryLink;
	Vcl::Menus::TMenuItem* MIDelphi3D;
	Vcl::Comctrls::TTabSheet* TabSheet1;
	Vcl::Stdctrls::TButton* CloseButton;
	Vcl::Stdctrls::TLabel* VersionLbl;
	Vcl::Stdctrls::TLabel* ViewLabel;
	Vcl::Stdctrls::TLabel* WebsiteLbl;
	void __fastcall CloseButtonClick(System::TObject* Sender);
	void __fastcall FormCreate(System::TObject* Sender);
	void __fastcall FormKeyPress(System::TObject* Sender, System::WideChar &Key);
	void __fastcall FormClose(System::TObject* Sender, System::Uitypes::TCloseAction &Action);
	void __fastcall ExtensionsDblClick(System::TObject* Sender);
	void __fastcall ExtensionsClick(System::TObject* Sender);
	void __fastcall ExtensionsKeyPress(System::TObject* Sender, System::WideChar &Key);
	void __fastcall FormShow(System::TObject* Sender);
	void __fastcall MIDelphi3DClick(System::TObject* Sender);
	void __fastcall WebsiteLblClick(System::TObject* Sender);
	
protected:
	void __fastcall LoadContributors(void);
	System::UnicodeString __fastcall GetSceneVersion(void);
	
public:
	void __fastcall GetInfoFrom(Glscene::TGLSceneBuffer* aSceneBuffer);
public:
	/* TCustomForm.Create */ inline __fastcall virtual TInfoForm(System::Classes::TComponent* AOwner) : Vcl::Forms::TForm(AOwner) { }
	/* TCustomForm.CreateNew */ inline __fastcall virtual TInfoForm(System::Classes::TComponent* AOwner, int Dummy) : Vcl::Forms::TForm(AOwner, Dummy) { }
	/* TCustomForm.Destroy */ inline __fastcall virtual ~TInfoForm(void) { }
	
public:
	/* TWinControl.CreateParented */ inline __fastcall TInfoForm(HWND ParentWindow) : Vcl::Forms::TForm(ParentWindow) { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Info */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_INFO)
using namespace Info;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// InfoHPP
