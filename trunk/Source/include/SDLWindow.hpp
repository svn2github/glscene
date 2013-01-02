// CodeGear C++Builder
// Copyright (c) 1995, 2012 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'SDLWindow.pas' rev: 24.00 (Win32)

#ifndef SdlwindowHPP
#define SdlwindowHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <System.Classes.hpp>	// Pascal unit
#include <System.SysUtils.hpp>	// Pascal unit
#include <SDL.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Sdlwindow
{
//-- type declarations -------------------------------------------------------
enum TSDLWindowPixelDepth : unsigned char { vpd16bits, vpd24bits };

enum TSDLWindowOption : unsigned char { voDoubleBuffer, voHardwareAccel, voOpenGL, voResizable, voFullScreen, voStencilBuffer };

typedef System::Set<TSDLWindowOption, TSDLWindowOption::voDoubleBuffer, TSDLWindowOption::voStencilBuffer>  TSDLWindowOptions;

typedef void __fastcall (__closure *TSDLEvent)(System::TObject* sender, const Sdl::TSDL_Event &event);

class DELPHICLASS TSDLWindow;
class PASCALIMPLEMENTATION TSDLWindow : public System::Classes::TComponent
{
	typedef System::Classes::TComponent inherited;
	
private:
	int FWidth;
	int FHeight;
	TSDLWindowPixelDepth FPixelDepth;
	TSDLWindowOptions FOptions;
	bool FActive;
	System::Classes::TNotifyEvent FOnOpen;
	System::Classes::TNotifyEvent FOnClose;
	System::Classes::TNotifyEvent FOnResize;
	TSDLEvent FOnSDLEvent;
	System::Classes::TNotifyEvent FOnEventPollDone;
	System::UnicodeString FCaption;
	int FThreadSleepLength;
	System::Classes::TThreadPriority FThreadPriority;
	bool FThreadedEventPolling;
	System::Classes::TThread* FThread;
	Sdl::TSDL_Surface *FSDLSurface;
	unsigned FWindowHandle;
	
protected:
	void __fastcall SetWidth(const int val);
	void __fastcall SetHeight(const int val);
	void __fastcall SetPixelDepth(const TSDLWindowPixelDepth val);
	void __fastcall SetOptions(const TSDLWindowOptions val);
	void __fastcall SetActive(const bool val);
	void __fastcall SetCaption(const System::UnicodeString val);
	void __fastcall SetThreadSleepLength(const int val);
	void __fastcall SetThreadPriority(const System::Classes::TThreadPriority val);
	void __fastcall SetThreadedEventPolling(const bool val);
	unsigned __fastcall BuildSDLVideoFlags(void);
	void __fastcall SetSDLGLAttributes(void);
	void __fastcall CreateOrRecreateSDLSurface(void);
	void __fastcall ResizeGLWindow(void);
	void __fastcall SetupSDLEnvironmentValues(void);
	void __fastcall StartThread(void);
	void __fastcall StopThread(void);
	
public:
	__fastcall virtual TSDLWindow(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TSDLWindow(void);
	void __fastcall Open(void);
	void __fastcall Close(void);
	void __fastcall UpdateWindow(void);
	void __fastcall SwapBuffers(void);
	void __fastcall PollEvents(void);
	__property bool Active = {read=FActive, write=SetActive, nodefault};
	__property Sdl::PSDL_Surface Surface = {read=FSDLSurface};
	__property unsigned WindowHandle = {read=FWindowHandle, write=FWindowHandle, nodefault};
	
__published:
	__property int Width = {read=FWidth, write=SetWidth, default=640};
	__property int Height = {read=FHeight, write=SetHeight, default=480};
	__property TSDLWindowPixelDepth PixelDepth = {read=FPixelDepth, write=SetPixelDepth, default=1};
	__property TSDLWindowOptions Options = {read=FOptions, write=SetOptions, default=15};
	__property System::UnicodeString Caption = {read=FCaption, write=SetCaption};
	__property bool ThreadedEventPolling = {read=FThreadedEventPolling, write=SetThreadedEventPolling, default=1};
	__property int ThreadSleepLength = {read=FThreadSleepLength, write=SetThreadSleepLength, default=1};
	__property System::Classes::TThreadPriority ThreadPriority = {read=FThreadPriority, write=SetThreadPriority, default=2};
	__property System::Classes::TNotifyEvent OnOpen = {read=FOnOpen, write=FOnOpen};
	__property System::Classes::TNotifyEvent OnClose = {read=FOnClose, write=FOnClose};
	__property System::Classes::TNotifyEvent OnResize = {read=FOnResize, write=FOnResize};
	__property TSDLEvent OnSDLEvent = {read=FOnSDLEvent, write=FOnSDLEvent};
	__property System::Classes::TNotifyEvent OnEventPollDone = {read=FOnEventPollDone, write=FOnEventPollDone};
};


class DELPHICLASS ESDLError;
#pragma pack(push,4)
class PASCALIMPLEMENTATION ESDLError : public System::Sysutils::Exception
{
	typedef System::Sysutils::Exception inherited;
	
public:
	/* Exception.Create */ inline __fastcall ESDLError(const System::UnicodeString Msg) : System::Sysutils::Exception(Msg) { }
	/* Exception.CreateFmt */ inline __fastcall ESDLError(const System::UnicodeString Msg, System::TVarRec const *Args, const int Args_Size) : System::Sysutils::Exception(Msg, Args, Args_Size) { }
	/* Exception.CreateRes */ inline __fastcall ESDLError(NativeUInt Ident)/* overload */ : System::Sysutils::Exception(Ident) { }
	/* Exception.CreateRes */ inline __fastcall ESDLError(System::PResStringRec ResStringRec)/* overload */ : System::Sysutils::Exception(ResStringRec) { }
	/* Exception.CreateResFmt */ inline __fastcall ESDLError(NativeUInt Ident, System::TVarRec const *Args, const int Args_Size)/* overload */ : System::Sysutils::Exception(Ident, Args, Args_Size) { }
	/* Exception.CreateResFmt */ inline __fastcall ESDLError(System::PResStringRec ResStringRec, System::TVarRec const *Args, const int Args_Size)/* overload */ : System::Sysutils::Exception(ResStringRec, Args, Args_Size) { }
	/* Exception.CreateHelp */ inline __fastcall ESDLError(const System::UnicodeString Msg, int AHelpContext) : System::Sysutils::Exception(Msg, AHelpContext) { }
	/* Exception.CreateFmtHelp */ inline __fastcall ESDLError(const System::UnicodeString Msg, System::TVarRec const *Args, const int Args_Size, int AHelpContext) : System::Sysutils::Exception(Msg, Args, Args_Size, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall ESDLError(NativeUInt Ident, int AHelpContext)/* overload */ : System::Sysutils::Exception(Ident, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall ESDLError(System::PResStringRec ResStringRec, int AHelpContext)/* overload */ : System::Sysutils::Exception(ResStringRec, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall ESDLError(System::PResStringRec ResStringRec, System::TVarRec const *Args, const int Args_Size, int AHelpContext)/* overload */ : System::Sysutils::Exception(ResStringRec, Args, Args_Size, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall ESDLError(NativeUInt Ident, System::TVarRec const *Args, const int Args_Size, int AHelpContext)/* overload */ : System::Sysutils::Exception(Ident, Args, Args_Size, AHelpContext) { }
	/* Exception.Destroy */ inline __fastcall virtual ~ESDLError(void) { }
	
};

#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------
#define cDefaultSDLWindowOptions (System::Set<TSDLWindowOption, TSDLWindowOption::voDoubleBuffer, TSDLWindowOption::voStencilBuffer> () << TSDLWindowOption::voDoubleBuffer << TSDLWindowOption::voHardwareAccel << TSDLWindowOption::voOpenGL << TSDLWindowOption::voResizable )
extern PACKAGE void __fastcall Register(void);
}	/* namespace Sdlwindow */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_SDLWINDOW)
using namespace Sdlwindow;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// SdlwindowHPP
