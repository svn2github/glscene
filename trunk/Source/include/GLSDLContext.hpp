// CodeGear C++Builder
// Copyright (c) 1995, 2012 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLSDLContext.pas' rev: 24.00 (Win32)

#ifndef GlsdlcontextHPP
#define GlsdlcontextHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <Winapi.Windows.hpp>	// Pascal unit
#include <System.Classes.hpp>	// Pascal unit
#include <System.SysUtils.hpp>	// Pascal unit
#include <GLContext.hpp>	// Pascal unit
#include <SDLWindow.hpp>	// Pascal unit
#include <GLScene.hpp>	// Pascal unit
#include <SDL.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Glsdlcontext
{
//-- type declarations -------------------------------------------------------
class DELPHICLASS TGLSDLViewer;
class PASCALIMPLEMENTATION TGLSDLViewer : public Glscene::TGLNonVisualViewer
{
	typedef Glscene::TGLNonVisualViewer inherited;
	
private:
	System::UnicodeString FCaption;
	Sdlwindow::TSDLEvent FOnSDLEvent;
	System::Classes::TNotifyEvent FOnEventPollDone;
	System::Classes::TNotifyEvent FOnResize;
	
protected:
	void __fastcall SetCaption(const System::UnicodeString val);
	void __fastcall DoOnOpen(System::TObject* sender);
	void __fastcall DoOnClose(System::TObject* sender);
	void __fastcall DoOnResize(System::TObject* sender);
	void __fastcall DoOnSDLEvent(System::TObject* sender, const Sdl::TSDL_Event &event);
	void __fastcall DoOnEventPollDone(System::TObject* sender);
	virtual void __fastcall DoBufferStructuralChange(System::TObject* Sender);
	DYNAMIC void __fastcall PrepareGLContext(void);
	
public:
	__fastcall virtual TGLSDLViewer(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TGLSDLViewer(void);
	virtual void __fastcall Render(Glscene::TGLBaseSceneObject* baseObject = (Glscene::TGLBaseSceneObject*)(0x0));
	bool __fastcall Active(void);
	
__published:
	__property System::UnicodeString Caption = {read=FCaption, write=SetCaption};
	__property System::Classes::TNotifyEvent OnResize = {read=FOnResize, write=FOnResize};
	__property Sdlwindow::TSDLEvent OnSDLEvent = {read=FOnSDLEvent, write=FOnSDLEvent};
	__property System::Classes::TNotifyEvent OnEventPollDone = {read=FOnEventPollDone, write=FOnEventPollDone};
};


class DELPHICLASS TGLSDLContext;
class PASCALIMPLEMENTATION TGLSDLContext : public Glcontext::TGLScreenControlingContext
{
	typedef Glcontext::TGLScreenControlingContext inherited;
	
private:
	Sdlwindow::TSDLWindow* FSDLWin;
	bool FSimulatedValidity;
	
protected:
	virtual void __fastcall DoCreateContext(HDC outputDevice);
	virtual void __fastcall DoCreateMemoryContext(HWND outputDevice, int width, int height, int BufferCount);
	virtual bool __fastcall DoShareLists(Glcontext::TGLContext* aContext);
	virtual void __fastcall DoDestroyContext(void);
	virtual void __fastcall DoActivate(void);
	virtual void __fastcall DoDeactivate(void);
	
public:
	__fastcall virtual TGLSDLContext(void);
	__fastcall virtual ~TGLSDLContext(void);
	virtual bool __fastcall IsValid(void);
	virtual void __fastcall SwapBuffers(void);
	virtual void * __fastcall RenderOutputDevice(void);
	__property Sdlwindow::TSDLWindow* SDLWindow = {read=FSDLWin};
};


//-- var, const, procedure ---------------------------------------------------
extern PACKAGE void __fastcall Register(void);
}	/* namespace Glsdlcontext */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLSDLCONTEXT)
using namespace Glsdlcontext;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// GlsdlcontextHPP
