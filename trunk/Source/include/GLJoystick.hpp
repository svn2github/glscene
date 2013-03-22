// CodeGear C++Builder
// Copyright (c) 1995, 2012 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLJoystick.pas' rev: 24.00 (Win32)

#ifndef GljoystickHPP
#define GljoystickHPP

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
#include <System.Classes.hpp>	// Pascal unit
#include <Winapi.Messages.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Gljoystick
{
//-- type declarations -------------------------------------------------------
enum TJoystickButton : unsigned char { jbButton1, jbButton2, jbButton3, jbButton4 };

typedef System::Set<TJoystickButton, TJoystickButton::jbButton1, TJoystickButton::jbButton4>  TJoystickButtons;

enum TJoystickID : unsigned char { jidNoJoystick, jidJoystick1, jidJoystick2 };

enum TJoystickDesignMode : unsigned char { jdmInactive, jdmActive };

enum TJoyPos : unsigned char { jpMin, jpCenter, jpMax };

enum TJoyAxis : unsigned char { jaX, jaY, jaZ, jaR, jaU, jaV };

typedef void __fastcall (__closure *TJoystickEvent)(System::TObject* Sender, TJoystickID JoyID, TJoystickButtons Buttons, int XDeflection, int YDeflection);

class DELPHICLASS TGLJoystick;
class PASCALIMPLEMENTATION TGLJoystick : public System::Classes::TComponent
{
	typedef System::Classes::TComponent inherited;
	
private:
	HWND FWindowHandle;
	int FNumButtons;
	int FLastX;
	int FLastY;
	int FLastZ;
	unsigned FThreshold;
	unsigned FInterval;
	bool FCapture;
	bool FNoCaptureErrors;
	TJoystickID FJoystickID;
	System::StaticArray<System::StaticArray<int, 3>, 6> FMinMaxInfo;
	System::StaticArray<int, 5> FXPosInfo;
	System::StaticArray<int, 5> FYPosInfo;
	TJoystickEvent FOnJoystickButtonChange;
	TJoystickEvent FOnJoystickMove;
	int FXPosition;
	int FYPosition;
	TJoystickButtons FJoyButtons;
	void __fastcall SetCapture(bool AValue);
	void __fastcall SetInterval(unsigned AValue);
	void __fastcall SetJoystickID(TJoystickID AValue);
	void __fastcall SetThreshold(unsigned AValue);
	
protected:
	TJoystickButtons __fastcall MakeJoyButtons(unsigned Param);
	void __fastcall DoJoystickCapture(HWND AHandle, TJoystickID AJoystick);
	void __fastcall DoJoystickRelease(TJoystickID AJoystick);
	void __fastcall DoXYMove(System::Word Buttons, int XPos, int YPos);
	void __fastcall DoZMove(System::Word Buttons, int ZPos);
	void __fastcall ReapplyCapture(TJoystickID AJoystick);
	void __fastcall WndProc(Winapi::Messages::TMessage &Msg);
	virtual void __fastcall Loaded(void);
	
public:
	__fastcall virtual TGLJoystick(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TGLJoystick(void);
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	__property TJoystickButtons JoyButtons = {read=FJoyButtons, nodefault};
	__property int XPosition = {read=FXPosition, nodefault};
	__property int YPosition = {read=FYPosition, nodefault};
	
__published:
	__property bool Capture = {read=FCapture, write=SetCapture, default=0};
	__property bool NoCaptureErrors = {read=FNoCaptureErrors, write=FNoCaptureErrors, default=1};
	__property unsigned Interval = {read=FInterval, write=SetInterval, default=100};
	__property TJoystickID JoystickID = {read=FJoystickID, write=SetJoystickID, default=0};
	__property unsigned Threshold = {read=FThreshold, write=SetThreshold, default=1000};
	__property TJoystickEvent OnJoystickButtonChange = {read=FOnJoystickButtonChange, write=FOnJoystickButtonChange};
	__property TJoystickEvent OnJoystickMove = {read=FOnJoystickMove, write=FOnJoystickMove};
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Gljoystick */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLJOYSTICK)
using namespace Gljoystick;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// GljoystickHPP
