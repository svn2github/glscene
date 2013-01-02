// CodeGear C++Builder
// Copyright (c) 1995, 2012 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'ScreenSaver.pas' rev: 24.00 (Win32)

#ifndef ScreensaverHPP
#define ScreensaverHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <Winapi.Windows.hpp>	// Pascal unit
#include <System.Classes.hpp>	// Pascal unit
#include <Vcl.Controls.hpp>	// Pascal unit
#include <Vcl.Forms.hpp>	// Pascal unit
#include <Vcl.ExtCtrls.hpp>	// Pascal unit
#include <System.Types.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Screensaver
{
//-- type declarations -------------------------------------------------------
enum TScreenSaverOption : unsigned char { ssoAutoAdjustFormProperties, ssoAutoHookKeyboardEvents, ssoAutoHookMouseEvents, ssoEnhancedMouseMoveDetection };

typedef System::Set<TScreenSaverOption, TScreenSaverOption::ssoAutoAdjustFormProperties, TScreenSaverOption::ssoEnhancedMouseMoveDetection>  TScreenSaverOptions;

typedef void __fastcall (__closure *TScreenSaverPreviewEvent)(System::TObject* Sender, HWND previewHwnd);

class DELPHICLASS TScreenSaver;
class PASCALIMPLEMENTATION TScreenSaver : public System::Classes::TComponent
{
	typedef System::Classes::TComponent inherited;
	
private:
	int mouseEventsToIgnore;
	bool FHonourWindowsPassword;
	TScreenSaverOptions FOptions;
	System::Classes::TNotifyEvent FOnPropertiesRequested;
	System::Classes::TNotifyEvent FOnExecute;
	TScreenSaverPreviewEvent FOnPreview;
	Vcl::Forms::TCloseQueryEvent FOnCloseQuery;
	System::UnicodeString FAboutString;
	bool FInPreviewMode;
	Vcl::Extctrls::TTimer* mouseTimer;
	System::Types::TPoint lastMousePosition;
	NativeUInt FMutex;
	
protected:
	virtual void __fastcall Loaded(void);
	void __fastcall FormMouseMove(System::TObject* Sender, System::Classes::TShiftState Shift, int X, int Y);
	void __fastcall FormKeyPress(System::TObject* Sender, System::WideChar &Key);
	void __fastcall OnMouseTimer(System::TObject* Sender);
	void __fastcall ConfigureSaver(void);
	void __fastcall PreviewSaver(void);
	void __fastcall ExecuteSaver(void);
	
public:
	__fastcall virtual TScreenSaver(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TScreenSaver(void);
	void __fastcall SetPassword(void);
	bool __fastcall CloseSaver(void);
	__property bool InPreviewMode = {read=FInPreviewMode, nodefault};
	
__published:
	__property TScreenSaverOptions Options = {read=FOptions, write=FOptions, default=11};
	__property bool HonourWindowsPassword = {read=FHonourWindowsPassword, write=FHonourWindowsPassword, default=1};
	__property System::UnicodeString AboutString = {read=FAboutString, write=FAboutString};
	__property System::Classes::TNotifyEvent OnPropertiesRequested = {read=FOnPropertiesRequested, write=FOnPropertiesRequested};
	__property System::Classes::TNotifyEvent OnExecute = {read=FOnExecute, write=FOnExecute};
	__property TScreenSaverPreviewEvent OnPreview = {read=FOnPreview, write=FOnPreview};
	__property Vcl::Forms::TCloseQueryEvent OnCloseQuery = {read=FOnCloseQuery, write=FOnCloseQuery};
};


//-- var, const, procedure ---------------------------------------------------
#define cDefaultScreenSaverOptions (System::Set<TScreenSaverOption, TScreenSaverOption::ssoAutoAdjustFormProperties, TScreenSaverOption::ssoEnhancedMouseMoveDetection> () << TScreenSaverOption::ssoAutoAdjustFormProperties << TScreenSaverOption::ssoAutoHookKeyboardEvents << TScreenSaverOption::ssoEnhancedMouseMoveDetection )
extern PACKAGE void __fastcall SetScreenSaverPassword(void);
}	/* namespace Screensaver */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_SCREENSAVER)
using namespace Screensaver;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// ScreensaverHPP
