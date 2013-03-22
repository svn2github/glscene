// CodeGear C++Builder
// Copyright (c) 1995, 2012 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLWin32Viewer.pas' rev: 24.00 (Win32)

#ifndef Glwin32viewerHPP
#define Glwin32viewerHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <Winapi.Windows.hpp>	// Pascal unit
#include <Vcl.Graphics.hpp>	// Pascal unit
#include <Vcl.Forms.hpp>	// Pascal unit
#include <Vcl.Controls.hpp>	// Pascal unit
#include <Winapi.Messages.hpp>	// Pascal unit
#include <System.Classes.hpp>	// Pascal unit
#include <GLScene.hpp>	// Pascal unit
#include <GLContext.hpp>	// Pascal unit
#include <System.Types.hpp>	// Pascal unit
#include <System.UITypes.hpp>	// Pascal unit
#include <Vcl.Menus.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Glwin32viewer
{
//-- type declarations -------------------------------------------------------
typedef void __fastcall (__closure *TTouchEvent)(int X, int Y, int TouchWidth, int TouchHeight, unsigned TouchID, bool MultiTouch);

class DELPHICLASS TGLSceneViewer;
class PASCALIMPLEMENTATION TGLSceneViewer : public Vcl::Controls::TWinControl
{
	typedef Vcl::Controls::TWinControl inherited;
	
private:
	Glscene::TGLSceneBuffer* FBuffer;
	Glcontext::TVSyncMode FVSync;
	HDC FOwnDC;
	System::Classes::TNotifyEvent FOnMouseEnter;
	System::Classes::TNotifyEvent FOnMouseLeave;
	bool FMouseInControl;
	System::Types::TPoint FLastScreenPos;
	TTouchEvent FOnTouchMove;
	TTouchEvent FOnTouchUp;
	TTouchEvent FOnTouchDown;
	HIDESBASE MESSAGE void __fastcall WMEraseBkgnd(Winapi::Messages::TWMEraseBkgnd &Message);
	HIDESBASE MESSAGE void __fastcall WMPaint(Winapi::Messages::TWMPaint &Message);
	HIDESBASE MESSAGE void __fastcall WMSize(Winapi::Messages::TWMSize &Message);
	MESSAGE void __fastcall WMGetDglCode(Winapi::Messages::TMessage &Message);
	HIDESBASE MESSAGE void __fastcall WMDestroy(Winapi::Messages::TWMNoParams &Message);
	MESSAGE void __fastcall WMTouch(Winapi::Messages::TMessage &Message);
	HIDESBASE MESSAGE void __fastcall CMMouseEnter(Winapi::Messages::TMessage &msg);
	HIDESBASE MESSAGE void __fastcall CMMouseLeave(Winapi::Messages::TMessage &msg);
	float __fastcall GetFieldOfView(void);
	void __fastcall SetFieldOfView(const float Value);
	bool __fastcall GetIsRenderingContextAvailable(void);
	
protected:
	void __fastcall SetBeforeRender(const System::Classes::TNotifyEvent val);
	System::Classes::TNotifyEvent __fastcall GetBeforeRender(void);
	void __fastcall SetPostRender(const System::Classes::TNotifyEvent val);
	System::Classes::TNotifyEvent __fastcall GetPostRender(void);
	void __fastcall SetAfterRender(const System::Classes::TNotifyEvent val);
	System::Classes::TNotifyEvent __fastcall GetAfterRender(void);
	void __fastcall SetCamera(Glscene::TGLCamera* const val);
	Glscene::TGLCamera* __fastcall GetCamera(void);
	void __fastcall SetBuffer(Glscene::TGLSceneBuffer* const val);
	virtual void __fastcall CreateParams(Vcl::Controls::TCreateParams &Params);
	virtual void __fastcall CreateWnd(void);
	virtual void __fastcall DestroyWnd(void);
	virtual void __fastcall Loaded(void);
	DYNAMIC void __fastcall DoBeforeRender(System::TObject* Sender);
	virtual void __fastcall DoBufferChange(System::TObject* Sender);
	DYNAMIC void __fastcall DoBufferStructuralChange(System::TObject* Sender);
	DYNAMIC void __fastcall MouseMove(System::Classes::TShiftState Shift, int X, int Y);
	
public:
	__fastcall virtual TGLSceneViewer(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TGLSceneViewer(void);
	virtual void __fastcall Notification(System::Classes::TComponent* AComponent, System::Classes::TOperation Operation);
	HIDESBASE void __fastcall RecreateWnd(void);
	__property bool IsRenderingContextAvailable = {read=GetIsRenderingContextAvailable, nodefault};
	float __fastcall LastFrameTime(void);
	float __fastcall FramesPerSecond(void);
	System::UnicodeString __fastcall FramesPerSecondText(int decimals = 0x1);
	void __fastcall ResetPerformanceMonitor(void);
	Vcl::Graphics::TBitmap* __fastcall CreateSnapShotBitmap(void);
	void __fastcall RegisterTouch(void);
	void __fastcall UnregisterTouch(void);
	__property HDC RenderDC = {read=FOwnDC, nodefault};
	__property bool MouseInControl = {read=FMouseInControl, nodefault};
	
__published:
	__property Glscene::TGLCamera* Camera = {read=GetCamera, write=SetCamera};
	__property Glcontext::TVSyncMode VSync = {read=FVSync, write=FVSync, default=1};
	__property System::Classes::TNotifyEvent BeforeRender = {read=GetBeforeRender, write=SetBeforeRender};
	__property System::Classes::TNotifyEvent PostRender = {read=GetPostRender, write=SetPostRender};
	__property System::Classes::TNotifyEvent AfterRender = {read=GetAfterRender, write=SetAfterRender};
	__property Glscene::TGLSceneBuffer* Buffer = {read=FBuffer, write=SetBuffer};
	__property float FieldOfView = {read=GetFieldOfView, write=SetFieldOfView};
	__property System::Classes::TNotifyEvent OnMouseLeave = {read=FOnMouseLeave, write=FOnMouseLeave};
	__property System::Classes::TNotifyEvent OnMouseEnter = {read=FOnMouseEnter, write=FOnMouseEnter};
	__property TTouchEvent OnTouchMove = {read=FOnTouchMove, write=FOnTouchMove};
	__property TTouchEvent OnTouchUp = {read=FOnTouchUp, write=FOnTouchUp};
	__property TTouchEvent OnTouchDown = {read=FOnTouchDown, write=FOnTouchDown};
	__property Align = {default=0};
	__property Anchors = {default=3};
	__property DragCursor = {default=-12};
	__property DragMode = {default=0};
	__property Enabled = {default=1};
	__property HelpContext = {default=0};
	__property Hint = {default=0};
	__property PopupMenu;
	__property Visible = {default=1};
	__property OnClick;
	__property OnDblClick;
	__property OnDragDrop;
	__property OnDragOver;
	__property OnStartDrag;
	__property OnEndDrag;
	__property OnMouseDown;
	__property OnMouseMove;
	__property OnMouseUp;
	__property OnMouseWheel;
	__property OnMouseWheelDown;
	__property OnMouseWheelUp;
	__property OnKeyDown;
	__property OnKeyUp;
	__property OnContextPopup;
	__property TabStop = {default=0};
	__property TabOrder = {default=-1};
	__property OnEnter;
	__property OnExit;
	__property OnGesture;
	__property Touch;
public:
	/* TWinControl.CreateParented */ inline __fastcall TGLSceneViewer(HWND ParentWindow) : Vcl::Controls::TWinControl(ParentWindow) { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Glwin32viewer */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLWIN32VIEWER)
using namespace Glwin32viewer;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Glwin32viewerHPP
