// CodeGear C++Builder
// Copyright (c) 1995, 2012 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'FRTextureEdit.pas' rev: 24.00 (Win32)

#ifndef FrtextureeditHPP
#define FrtextureeditHPP

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
#include <System.Classes.hpp>	// Pascal unit
#include <System.TypInfo.hpp>	// Pascal unit
#include <GLGraphics.hpp>	// Pascal unit
#include <GLTextureFormat.hpp>	// Pascal unit
#include <GLTexture.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Frtextureedit
{
//-- type declarations -------------------------------------------------------
class DELPHICLASS TRTextureEdit;
class PASCALIMPLEMENTATION TRTextureEdit : public Vcl::Forms::TFrame
{
	typedef Vcl::Forms::TFrame inherited;
	
__published:
	Vcl::Stdctrls::TLabel* Label2;
	Vcl::Buttons::TSpeedButton* SBEditImage;
	Vcl::Stdctrls::TComboBox* CBMagFilter;
	Vcl::Stdctrls::TLabel* Label3;
	Vcl::Stdctrls::TLabel* Label4;
	Vcl::Stdctrls::TComboBox* CBMinFilter;
	Vcl::Stdctrls::TComboBox* CBTextureMode;
	Vcl::Stdctrls::TLabel* Label1;
	Vcl::Stdctrls::TLabel* Label5;
	Vcl::Stdctrls::TComboBox* CBTextureWrap;
	Vcl::Stdctrls::TCheckBox* CBDisabled;
	Vcl::Stdctrls::TComboBox* CBImageClass;
	Vcl::Stdctrls::TComboBox* CBImageAlpha;
	Vcl::Stdctrls::TLabel* Label6;
	Vcl::Stdctrls::TComboBox* CBFilteringQuality;
	Vcl::Stdctrls::TLabel* Label7;
	void __fastcall CBMagFilterChange(System::TObject* Sender);
	void __fastcall CBMinFilterChange(System::TObject* Sender);
	void __fastcall CBTextureModeChange(System::TObject* Sender);
	void __fastcall CBTextureWrapChange(System::TObject* Sender);
	void __fastcall CBDisabledClick(System::TObject* Sender);
	void __fastcall SBEditImageClick(System::TObject* Sender);
	void __fastcall CBImageClassChange(System::TObject* Sender);
	void __fastcall CBImageAlphaChange(System::TObject* Sender);
	void __fastcall CBFilteringQualityChange(System::TObject* Sender);
	
private:
	Gltexture::TGLTexture* FTexture;
	System::Classes::TNotifyEvent FOnChange;
	bool changeing;
	
protected:
	void __fastcall SetTexture(Gltexture::TGLTexture* const val);
	DYNAMIC void __fastcall DoOnChange(void);
	
public:
	__fastcall virtual TRTextureEdit(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TRTextureEdit(void);
	__property Gltexture::TGLTexture* Texture = {read=FTexture, write=SetTexture};
	__property System::Classes::TNotifyEvent OnChange = {read=FOnChange, write=FOnChange};
public:
	/* TWinControl.CreateParented */ inline __fastcall TRTextureEdit(HWND ParentWindow) : Vcl::Forms::TFrame(ParentWindow) { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Frtextureedit */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_FRTEXTUREEDIT)
using namespace Frtextureedit;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// FrtextureeditHPP
