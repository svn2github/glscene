// CodeGear C++Builder
// Copyright (c) 1995, 2012 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLWin32Context.pas' rev: 24.00 (Win32)

#ifndef Glwin32contextHPP
#define Glwin32contextHPP

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
#include <OpenGLTokens.hpp>	// Pascal unit
#include <OpenGLAdapter.hpp>	// Pascal unit
#include <GLContext.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Glwin32context
{
//-- type declarations -------------------------------------------------------
class DELPHICLASS TGLWin32Context;
class PASCALIMPLEMENTATION TGLWin32Context : public Glcontext::TGLContext
{
	typedef Glcontext::TGLContext inherited;
	
private:
	typedef System::DynamicArray<int> _TGLWin32Context__1;
	
	typedef System::DynamicArray<float> _TGLWin32Context__2;
	
	
protected:
	HDC FDC;
	HGLRC FRC;
	TGLWin32Context* FShareContext;
	int FHPBUFFER;
	_TGLWin32Context__1 FiAttribs;
	_TGLWin32Context__2 FfAttribs;
	bool FLegacyContextsOnly;
	bool FSwapBufferSupported;
	void __fastcall SpawnLegacyContext(HDC aDC);
	DYNAMIC void __fastcall CreateOldContext(HDC aDC);
	DYNAMIC void __fastcall CreateNewContext(HDC aDC);
	void __fastcall ClearIAttribs(void);
	void __fastcall AddIAttrib(int attrib, int value);
	void __fastcall ChangeIAttrib(int attrib, int newValue);
	void __fastcall DropIAttrib(int attrib);
	void __fastcall ClearFAttribs(void);
	void __fastcall AddFAttrib(float attrib, float value);
	void __fastcall DestructionEarlyWarning(System::TObject* sender);
	void __fastcall ChooseWGLFormat(HDC DC, unsigned nMaxFormats, System::PInteger piFormats, int &nNumFormats, int BufferCount = 0x1);
	virtual void __fastcall DoCreateContext(HDC ADeviceHandle);
	virtual void __fastcall DoCreateMemoryContext(HWND outputDevice, int width, int height, int BufferCount);
	virtual bool __fastcall DoShareLists(Glcontext::TGLContext* aContext);
	virtual void __fastcall DoDestroyContext(void);
	virtual void __fastcall DoActivate(void);
	virtual void __fastcall DoDeactivate(void);
	
public:
	__fastcall virtual TGLWin32Context(void);
	__fastcall virtual ~TGLWin32Context(void);
	virtual bool __fastcall IsValid(void);
	virtual void __fastcall SwapBuffers(void);
	virtual void * __fastcall RenderOutputDevice(void);
	__property HDC DC = {read=FDC, nodefault};
	__property HGLRC RC = {read=FRC, nodefault};
};


//-- var, const, procedure ---------------------------------------------------
extern PACKAGE System::ResourceString _cForwardContextFailed;
#define Glwin32context_cForwardContextFailed System::LoadResourceString(&Glwin32context::_cForwardContextFailed)
extern PACKAGE System::ResourceString _cBackwardContextFailed;
#define Glwin32context_cBackwardContextFailed System::LoadResourceString(&Glwin32context::_cBackwardContextFailed)
extern PACKAGE System::ResourceString _cFailHWRC;
#define Glwin32context_cFailHWRC System::LoadResourceString(&Glwin32context::_cFailHWRC)
extern PACKAGE System::ResourceString _glsTmpRC_Created;
#define Glwin32context_glsTmpRC_Created System::LoadResourceString(&Glwin32context::_glsTmpRC_Created)
extern PACKAGE System::ResourceString _glsDriverNotSupportFRC;
#define Glwin32context_glsDriverNotSupportFRC System::LoadResourceString(&Glwin32context::_glsDriverNotSupportFRC)
extern PACKAGE System::ResourceString _glsDriverNotSupportOESRC;
#define Glwin32context_glsDriverNotSupportOESRC System::LoadResourceString(&Glwin32context::_glsDriverNotSupportOESRC)
extern PACKAGE System::ResourceString _glsDriverNotSupportDebugRC;
#define Glwin32context_glsDriverNotSupportDebugRC System::LoadResourceString(&Glwin32context::_glsDriverNotSupportDebugRC)
extern PACKAGE System::ResourceString _glsOESvsForwardRC;
#define Glwin32context_glsOESvsForwardRC System::LoadResourceString(&Glwin32context::_glsOESvsForwardRC)
extern PACKAGE System::ResourceString _glsFRC_created;
#define Glwin32context_glsFRC_created System::LoadResourceString(&Glwin32context::_glsFRC_created)
extern PACKAGE System::ResourceString _glsOESRC_created;
#define Glwin32context_glsOESRC_created System::LoadResourceString(&Glwin32context::_glsOESRC_created)
extern PACKAGE System::ResourceString _glsPBufferRC_created;
#define Glwin32context_glsPBufferRC_created System::LoadResourceString(&Glwin32context::_glsPBufferRC_created)
extern PACKAGE bool vUseWindowTrackingHook;
extern PACKAGE HWND __fastcall CreateTempWnd(void);
}	/* namespace Glwin32context */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLWIN32CONTEXT)
using namespace Glwin32context;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Glwin32contextHPP
