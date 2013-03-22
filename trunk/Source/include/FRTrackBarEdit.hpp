// CodeGear C++Builder
// Copyright (c) 1995, 2012 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'FRTrackBarEdit.pas' rev: 24.00 (Win32)

#ifndef FrtrackbareditHPP
#define FrtrackbareditHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <Vcl.Forms.hpp>	// Pascal unit
#include <Vcl.StdCtrls.hpp>	// Pascal unit
#include <Vcl.ComCtrls.hpp>	// Pascal unit
#include <Vcl.Controls.hpp>	// Pascal unit
#include <System.Classes.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Frtrackbaredit
{
//-- type declarations -------------------------------------------------------
class DELPHICLASS TRTrackBarEdit;
class PASCALIMPLEMENTATION TRTrackBarEdit : public Vcl::Forms::TFrame
{
	typedef Vcl::Forms::TFrame inherited;
	
__published:
	Vcl::Comctrls::TTrackBar* TrackBar;
	Vcl::Stdctrls::TEdit* Edit;
	void __fastcall TrackBarChange(System::TObject* Sender);
	void __fastcall EditChange(System::TObject* Sender);
	
private:
	void __fastcall SetValue(const int val);
	int __fastcall GetValue(void);
	void __fastcall SetValueMin(const int val);
	int __fastcall GetValueMin(void);
	void __fastcall SetValueMax(const int val);
	int __fastcall GetValueMax(void);
	
public:
	__property int Value = {read=GetValue, write=SetValue, nodefault};
	__property int ValueMin = {read=GetValueMin, write=SetValueMin, nodefault};
	__property int ValueMax = {read=GetValueMax, write=SetValueMax, nodefault};
public:
	/* TCustomFrame.Create */ inline __fastcall virtual TRTrackBarEdit(System::Classes::TComponent* AOwner) : Vcl::Forms::TFrame(AOwner) { }
	
public:
	/* TScrollingWinControl.Destroy */ inline __fastcall virtual ~TRTrackBarEdit(void) { }
	
public:
	/* TWinControl.CreateParented */ inline __fastcall TRTrackBarEdit(HWND ParentWindow) : Vcl::Forms::TFrame(ParentWindow) { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Frtrackbaredit */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_FRTRACKBAREDIT)
using namespace Frtrackbaredit;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// FrtrackbareditHPP
