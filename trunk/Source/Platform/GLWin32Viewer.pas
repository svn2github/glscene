// GLWin32Viewer
{: Win32 specific Context.<p>

	<b>History : </b><font size=-1><ul>
      <li>11/06/03 - EG - Now uses ViewerBeforeChange to adjust VSync
      <li>29/10/02 - EG - Added MouseEnter/Leave/InControl
      <li>27/09/02 - EG - Added Ability to set display frequency
      <li>22/08/02 - EG - Added TGLSceneViewer.RecreateWnd
      <li>19/08/02 - EG - Added GetHandle
      <li>14/03/02 - EG - No longer invalidates while rendering
      <li>11/02/02 - EG - Fixed BeforeRender
      <li>29/01/02 - EG - New StayOnTop/Maximize logic (Richard Smuts)
      <li>22/01/02 - EG - Added TGLFullScreenViewer
      <li>28/12/01 - EG - Event persistence change (GliGli / Dephi bug)
	   <li>12/12/01 - EG - Creation (split from GLScene.pas)
	</ul></font>
}
unit GLWin32Viewer;

interface

{$i GLScene.inc}

uses Windows, Forms, Messages, Classes, GLScene, Controls, Menus;

type

   // TVSyncMode
   //
   TVSyncMode = (vsmSync, vsmNoSync);

   // TGLSceneViewer
   //
   {: Component where the GLScene objects get rendered.<p>
      This component delimits the area where OpenGL renders the scene,
      it represents the 3D scene viewed from a camera (specified in the
      camera property). This component can also render to a file or to a bitmap.<p>
      It is primarily a windowed component, but it can handle full-screen
      operations : simply make this component fit the whole screen (use a
      borderless form).<p>
      This viewer also allows to define rendering options such a fog, face culling,
      depth testing, etc. and can take care of framerate calculation.<p> }
   TGLSceneViewer = class (TWinControl)
      private
         { Private Declarations }
         FBuffer : TGLSceneBuffer;
         FVSync : TVSyncMode;
         FOwnDC : Cardinal;
			FOnMouseEnter, FOnMouseLeave : TNotifyEvent;
         FMouseInControl : Boolean;
         FIsOpenGLAvailable : Boolean;

         procedure WMEraseBkgnd(var Message: TWMEraseBkgnd); Message WM_ERASEBKGND;
         procedure WMPaint(var Message: TWMPaint); Message WM_PAINT;
         procedure WMSize(var Message: TWMSize); Message WM_SIZE;
         procedure WMDestroy(var Message: TWMDestroy); message WM_DESTROY;

	      procedure CMMouseEnter(var msg: TMessage); message CM_MOUSEENTER;
	      procedure CMMouseLeave(var msg: TMessage); message CM_MOUSELEAVE;

      protected
         { Protected Declarations }
         procedure SetBeforeRender(const val : TNotifyEvent);
         function GetBeforeRender : TNotifyEvent;
         procedure SetPostRender(const val : TNotifyEvent);
         function GetPostRender : TNotifyEvent;
         procedure SetAfterRender(const val : TNotifyEvent);
         function GetAfterRender : TNotifyEvent;
         procedure SetCamera(const val : TGLCamera);
         function GetCamera : TGLCamera;
         procedure SetBuffer(const val : TGLSceneBuffer);

         procedure CreateParams(var Params: TCreateParams); override;
         procedure CreateWnd; override;
         procedure DestroyWindowHandle; override;
         procedure Loaded; override;
         procedure DoBeforeRender(Sender : TObject); dynamic;
         procedure DoBufferChange(Sender : TObject); virtual;
         procedure DoBufferStructuralChange(Sender : TObject); dynamic;

      public
         { Public Declarations }
         constructor Create(AOwner: TComponent); override;
         destructor  Destroy; override;

         procedure Notification(AComponent: TComponent; Operation: TOperation); override;
         {: Makes TWinControl's RecreateWnd public.<p>
            This procedure allows to work around limitations in some OpenGL
            drivers (like MS Software OpenGL) that are not able to share lists
            between RCs that already have display lists. }
         procedure RecreateWnd;

         property IsOpenGLAvailable : Boolean read FIsOpenGLAvailable;

         function FramesPerSecond : Single;
         procedure ResetPerformanceMonitor;

         property RenderDC : Cardinal read FOwnDC;
         property MouseInControl : Boolean read FMouseInControl;

      published
         { Public Declarations }
         {: Camera from which the scene is rendered. }
         property Camera : TGLCamera read GetCamera write SetCamera;

         {: Specifies if the refresh should be synchronized with the VSync signal.<p>
            If the underlying OpenGL ICD does not support the WGL_EXT_swap_control
            extension, this property is ignored.  }
         property VSync : TVSyncMode read FVSync write FVSync default vsmNoSync;

         {: Triggered before the scene's objects get rendered.<p>
            You may use this event to execute your own OpenGL rendering. }
         property BeforeRender : TNotifyEvent read GetBeforeRender write SetBeforeRender;
         {: Triggered just after all the scene's objects have been rendered.<p>
            The OpenGL context is still active in this event, and you may use it
            to execute your own OpenGL rendering.<p> }
         property PostRender : TNotifyEvent read GetPostRender write SetPostRender;
         {: Called after rendering.<p>
            You cannot issue OpenGL calls in this event, if you want to do your own
            OpenGL stuff, use the PostRender event. }
         property AfterRender : TNotifyEvent read GetAfterRender write SetAfterRender;

         {: Access to buffer properties. }
         property Buffer : TGLSceneBuffer read FBuffer write SetBuffer;

			property OnMouseLeave : TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
			property OnMouseEnter : TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
         
         property Align;
         property Anchors;
         property DragCursor;
         property DragMode;
         property Enabled;
         property HelpContext;
         property Hint;
         property PopupMenu;
         property Visible;

         property OnClick;
         property OnDblClick;
         property OnDragDrop;
         property OnDragOver;
         property OnStartDrag;
         property OnEndDrag;
         property OnMouseDown;
         property OnMouseMove;
         property OnMouseUp;
{$ifdef GLS_COMPILER_5_UP}
         property OnContextPopup;
{$endif}
   end;

   // TGLScreenDepth
   //
   TGLScreenDepth = (sd8bits, sd16bits, sd24bits, sd32bits);

   // TGLFullScreenViewer
   //
   {: A FullScreen viewer.<p>
      This non visual viewer will, when activated, use the full screen as rendering
      surface. It will also switch/restore videomode depending on the required
      width/height.<br>
      This is performed by creating an underlying TForm and using its surface
      for rendering OpenGL, "decent" ICDs will automatically use PageFlipping
      instead of BlockTransfer (slower buffer flipping mode used for windowed
      OpenGL).<br>
      Note: if you terminate the application either via a kill or in the IDE,
      the original resolution isn't restored. }
   TGLFullScreenViewer = class (TGLNonVisualViewer)
      private
         { Private Declarations }
         FForm : TForm;
         FScreenDepth : TGLScreenDepth;
         FActive : Boolean;
         FSwitchedResolution : Boolean;
         FUpdateCount : Integer;
         FOnMouseDown : TMouseEvent;
         FOnMouseUp : TMouseEvent;
         FOnMouseMove : TMouseMoveEvent;
         FOnMouseWheel : TMouseWheelEvent;
         FOnClick, FOnDblClick : TNotifyEvent;
         FOnKeyDown : TKeyEvent;
         FOnKeyUp : TKeyEvent;
         FOnKeyPress : TKeyPressEvent;
         FOnClose : TCloseEvent;
         FOnCloseQuery : TCloseQueryEvent;
         FOldWndProc : TWndMethod;
         FStayOnTop : Boolean;
         FVSync : TVSyncMode;
         FRefreshRate : Integer;
         FCursor : TCursor;
         FPopupMenu : TPopupMenu;

      protected
         { Protected Declarations }
         procedure SetScreenDepth(const val : TGLScreenDepth);
         procedure SetActive(const val : Boolean);
         procedure SetOnMouseDown(const val : TMouseEvent);
         procedure SetOnMouseUp(const val : TMouseEvent);
         procedure SetOnMouseMove(const val : TMouseMoveEvent);
         procedure SetOnMouseWheel(const val : TMouseWheelEvent);
         procedure SetOnClick(const val : TNotifyEvent);
         procedure SetOnDblClick(const val : TNotifyEvent);
         procedure SetOnCloseQuery(const val : TCloseQueryEvent);
         procedure SetOnClose(const val : TCloseEvent);
         procedure SetOnKeyUp(const val : TKeyEvent);
         procedure SetOnKeyDown(const val : TKeyEvent);
         procedure SetOnKeyPress(const val : TKeyPressEvent);
         procedure SetStayOnTop(const val : Boolean);
         procedure SetCursor(const val : TCursor);
         procedure SetPopupMenu(const val : TPopupMenu);
         function  GetHandle : HWND;

         procedure DoBeforeRender(Sender : TObject);
         procedure DoBufferChange(Sender : TObject); override;
         procedure DoBufferStructuralChange(Sender : TObject); override;
         procedure PrepareGLContext; override;

         procedure Startup;
         procedure Shutdown;
         procedure BindFormEvents;
         procedure DoCloseQuery(Sender: TObject; var CanClose: Boolean);
         procedure DoPaint(Sender : TObject);
         procedure WndProc(var Message: TMessage);
         procedure DoActivate(Sender : TObject);
         procedure DoDeactivate(Sender : TObject);

      public
         { Public Declarations }
         constructor Create(AOwner: TComponent); override;
         destructor  Destroy; override;

         procedure Render; override;

         {: Adjusts property so that current resolution will be used.<p>
            Call this method if you want to make sure video mode isn't switched. }
         procedure UseCurrentResolution;

	      procedure BeginUpdate;
	      procedure EndUpdate;

         {: Activates/deactivates full screen mode.<p> }
         property Active : Boolean read FActive write SetActive;
         {: Read access to the underlying form handle.<p>
            Returns 0 (zero) if the viewer is not active or has not yet
            instantiated its form. }
         property Handle : HWND read GetHandle;

      published
         { Public Declarations }

         {: Requested ScreenDepth. }
         property ScreenDepth : TGLScreenDepth read FScreenDepth write SetScreenDepth default sd32bits;

         {: Specifies if the underlying form is "fsStayOnTop".<p>
            The benefit of StayOnTop is that it hides the windows bar and
            other background windows. The "fsStayOnTop" is automatically
            switched off/on when the underlying form loses/gains focus.<p>
            It is recommended not to use StayOnTop while running in the IDE
            or during the debugging phase.<p> }
         property StayOnTop : Boolean read FStayOnTop write SetStayOnTop default False;

         {: Specifies if the refresh should be synchronized with the VSync signal.<p>
            If the underlying OpenGL ICD does not support the WGL_EXT_swap_control
            extension, this property is ignored.  }
         property VSync : TVSyncMode read FVSync write FVSync default vsmSync;
         {: Screen refresh rate.<p>
            Use zero for system default. This property allows you to work around
            the WinXP bug that limits uses a refresh rate of 60Hz when changeing
            resolution. It is however suggested to give the user the opportunity
            to adjust it instead of having a fixed value (expecially beyond
            75Hz or for resolutions beyond 1024x768).<p>
            The value will be automatically clamped to the highest value
            *reported* compatible with the monitor. }
         property RefreshRate : Integer read FRefreshRate write FRefreshRate;

         property Cursor : TCursor read FCursor write SetCursor default crDefault;
         property PopupMenu : TPopupMenu read FPopupMenu write SetPopupMenu;

         property OnClose : TCloseEvent read FOnClose write SetOnClose;
         property OnKeyUp : TKeyEvent read FOnKeyUp write SetOnKeyUp;
         property OnKeyDown : TKeyEvent read FOnKeyDown write SetOnKeyDown;
         property OnKeyPress : TKeyPressEvent read FOnKeyPress write SetOnKeyPress;
         property OnCloseQuery : TCloseQueryEvent read FOnCloseQuery write SetOnCloseQuery;
         property OnClick : TNotifyEvent read FOnClick write SetOnClick;
         property OnDblClick : TNotifyEvent read FOnDblClick write SetOnDblClick;
         property OnMouseDown : TMouseEvent read FOnMouseDown write SetOnMouseDown;
         property OnMouseUp : TMouseEvent read FOnMouseUp write SetOnMouseUp;
         property OnMouseMove : TMouseMoveEvent read FOnMouseMove write SetOnMouseMove;
         property OnMouseWheel : TMouseWheelEvent read FOnMouseWheel write SetOnMouseWheel;
   end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

uses OpenGL12, SysUtils, GLWin32Context, GLCrossPlatform, GLScreen;

const
   cScreenDepthToBPP : array [sd8bits..sd32bits] of Integer = (8, 16, 24, 32);

// SetupVSync
//
procedure SetupVSync(vsync : TVSyncMode);
var
   i : Integer;
begin
   if WGL_EXT_swap_control then begin
      i:=wglGetSwapIntervalEXT;
      case VSync of
         vsmSync    : if i<>1 then wglSwapIntervalEXT(1);
         vsmNoSync  : if i<>0 then wglSwapIntervalEXT(0);
      else
         Assert(False);
      end;
   end;
end;

// ------------------
// ------------------ TGLSceneViewer ------------------
// ------------------

// Create
//
constructor TGLSceneViewer.Create(AOwner: TComponent);
begin
   FIsOpenGLAvailable:=InitOpenGL;
   inherited Create(AOwner);
   ControlStyle:=[csClickEvents, csDoubleClicks, csOpaque, csCaptureMouse];
   if csDesigning in ComponentState then
      ControlStyle:=ControlStyle+[csFramed];
   Width:=100;
   Height:=100;
   FVSync:=vsmNoSync;
   FBuffer:=TGLSceneBuffer.Create(Self);
   FBuffer.ViewerBeforeRender:=DoBeforeRender;
   FBuffer.OnChange:=DoBufferChange;
   FBuffer.OnStructuralChange:=DoBufferStructuralChange;
end;

// Destroy
//
destructor TGLSceneViewer.Destroy;
begin
   FBuffer.Free;
   inherited Destroy;
end;

// Notification
//
procedure TGLSceneViewer.Notification(AComponent: TComponent; Operation: TOperation);
begin
   if (Operation = opRemove) and (AComponent = Camera) then
      Camera:=nil;
   inherited;
end;

// RecreateWnd
//
procedure TGLSceneViewer.RecreateWnd;
begin
   inherited;
end;

// SetBeforeRender
//
procedure TGLSceneViewer.SetBeforeRender(const val : TNotifyEvent);
begin
   FBuffer.BeforeRender:=val;
end;

// GetBeforeRender
//
function TGLSceneViewer.GetBeforeRender : TNotifyEvent;
begin
   Result:=FBuffer.BeforeRender;
end;

// SetPostRender
//
procedure TGLSceneViewer.SetPostRender(const val : TNotifyEvent);
begin
   FBuffer.PostRender:=val;
end;

// GetPostRender
//
function TGLSceneViewer.GetPostRender : TNotifyEvent;
begin
   Result:=FBuffer.PostRender;
end;

// SetAfterRender
//
procedure TGLSceneViewer.SetAfterRender(const val : TNotifyEvent);
begin
   FBuffer.AfterRender:=val;
end;

// GetAfterRender
//
function TGLSceneViewer.GetAfterRender : TNotifyEvent;
begin
   Result:=FBuffer.AfterRender;
end;

// SetCamera
//
procedure TGLSceneViewer.SetCamera(const val : TGLCamera);
begin
   FBuffer.Camera:=val;
end;

// GetCamera
//
function TGLSceneViewer.GetCamera : TGLCamera;
begin
   Result:=FBuffer.Camera;
end;

// SetBuffer
//
procedure TGLSceneViewer.SetBuffer(const val : TGLSceneBuffer);
begin
   FBuffer.Assign(val);
end;

// CreateParams
//
procedure TGLSceneViewer.CreateParams(var Params: TCreateParams);
begin
   inherited CreateParams(Params);
   with Params do begin
      Style:=Style or WS_CLIPCHILDREN or WS_CLIPSIBLINGS;
      WindowClass.Style:=WindowClass.Style or CS_OWNDC;
   end;
end;

// CreateWnd
//
procedure TGLSceneViewer.CreateWnd;
begin
   inherited CreateWnd;
   if IsOpenGLAvailable then begin
      // initialize and activate the OpenGL rendering context
      // need to do this only once per window creation as we have a private DC
      FBuffer.Resize(Self.Width, Self.Height);
      FOwnDC:=GetDC(Handle);
      FBuffer.CreateRC(FOwnDC, False);
   end;
end;

// DestroyWindowHandle
//
procedure TGLSceneViewer.DestroyWindowHandle;
begin
   FBuffer.DestroyRC;
   if FOwnDC<>0 then begin
      ReleaseDC(Handle, FOwnDC);
      FOwnDC:=0;
   end;
   inherited;
end;

// WMEraseBkgnd
//
procedure TGLSceneViewer.WMEraseBkgnd(var Message: TWMEraseBkgnd);
begin
   if IsOpenGLAvailable then
      Message.Result:=1
   else inherited; 
end;

// WMSize
//
procedure TGLSceneViewer.WMSize(var Message: TWMSize);
begin
   inherited;
   FBuffer.Resize(Message.Width, Message.Height);
end;

// WMPaint
//
procedure TGLSceneViewer.WMPaint(var Message: TWMPaint);
var
   PS : TPaintStruct;
begin
   BeginPaint(Handle, PS);
   try
     if IsOpenGLAvailable then
         FBuffer.Render;
   finally
      EndPaint(Handle, PS);
      Message.Result:=0;
   end;
end;

// WMDestroy
//
procedure TGLSceneViewer.WMDestroy(var Message: TWMDestroy);
begin
   FBuffer.DestroyRC;
   if FOwnDC<>0 then begin
      ReleaseDC(Handle, FOwnDC);
      FOwnDC:=0;
   end;
   inherited;
end;

// CMMouseEnter
//
procedure TGLSceneViewer.CMMouseEnter(var msg: TMessage);
begin
   inherited;
   FMouseInControl:=True;
   if Assigned(FOnMouseEnter) then FOnMouseEnter(Self);
end;

// CMMouseLeave
//
procedure TGLSceneViewer.CMMouseLeave(var msg: TMessage);
begin
   inherited;
   FMouseInControl:=False;
   if Assigned(FOnMouseLeave) then FOnMouseLeave(Self);
end;

// Loaded
//
procedure TGLSceneViewer.Loaded;
begin
   inherited Loaded;
   // initiate window creation
   HandleNeeded;
end;

// DoBeforeRender
//
procedure TGLSceneViewer.DoBeforeRender(Sender : TObject);
begin
   SetupVSync(VSync);
end;

// DoBufferChange
//
procedure TGLSceneViewer.DoBufferChange(Sender : TObject);
begin
   if not Buffer.Rendering then
      Invalidate;
end;

// DoBufferStructuralChange
//
procedure TGLSceneViewer.DoBufferStructuralChange(Sender : TObject);
begin
   RecreateWnd;
end;

// FramesPerSecond
//
function TGLSceneViewer.FramesPerSecond : Single;
begin
   Result:=FBuffer.FramesPerSecond;
end;

// ResetPerformanceMonitor
//
procedure TGLSceneViewer.ResetPerformanceMonitor;
begin
   FBuffer.ResetPerformanceMonitor;
end;

// ------------------
// ------------------ TGLFullScreenViewer ------------------
// ------------------

// Create
//
constructor TGLFullScreenViewer.Create(AOwner: TComponent);
begin
   inherited Create(AOwner);
   Width:=800;
   Height:=600;
   FScreenDepth:=sd32bits;
   FVSync:=vsmSync;
   FCursor:=crDefault;
   Buffer.ViewerBeforeRender:=DoBeforeRender;
end;

// Destroy
//
destructor TGLFullScreenViewer.Destroy;
begin
   Active:=False;
   inherited Destroy;
end;

// DoBeforeRender
//
procedure TGLFullScreenViewer.DoBeforeRender(Sender : TObject);
begin
   SetupVSync(VSync);
end;

// DoBufferChange
//
procedure TGLFullScreenViewer.DoBufferChange(Sender : TObject);
begin
   if Assigned(FForm) and (not Buffer.Rendering) then
//      Render;
      FForm.Invalidate;
end;

// DoBufferStructuralChange
//
procedure TGLFullScreenViewer.DoBufferStructuralChange(Sender : TObject);
begin
   if Active and (FUpdateCount=0) then begin
      Shutdown;
      Startup;
   end;
end;

// PrepareGLContext
//
procedure TGLFullScreenViewer.PrepareGLContext;
begin
   // nothing yet
end;

// Render
//
procedure TGLFullScreenViewer.Render;
begin
   LoadOpenGL;
   if Buffer.RenderingContext=nil then begin
      Buffer.CreateRC(0, False);
   end;
   Buffer.Render;
end;

// BeginUpdate
//
procedure TGLFullScreenViewer.BeginUpdate;
begin
   Inc(FUpdateCount);
end;

// EndUpdate
//
procedure TGLFullScreenViewer.EndUpdate;
begin
   Dec(FUpdateCount);
   if FUpdateCount=0 then begin
      if Active then DoBufferStructuralChange(Self)
   end else if FUpdateCount<0 then begin
      FUpdateCount:=0;
      Assert(False, 'Unbalanced Begin/EndUpdate');
   end;
end;

// UseCurrentResolution
//
procedure TGLFullScreenViewer.UseCurrentResolution;
begin
   BeginUpdate;
   try
      Width:=Screen.Width;
      Height:=Screen.Height;
      case GetCurrentColorDepth of
         24 : ScreenDepth:=sd24bits;
         16 : ScreenDepth:=sd16bits;
         8 : ScreenDepth:=sd8bits;
      else
         // highest depth possible otherwise
         ScreenDepth:=sd32bits;
      end;
   finally
      EndUpdate;
   end;
end;

// SetActive
//
procedure TGLFullScreenViewer.SetActive(const val : Boolean);
begin
   if val<>FActive then begin
      if FActive then
         ShutDown
      else Startup;
   end;
end;

// Startup
//
procedure TGLFullScreenViewer.Startup;
var
   res : TResolution;
   dc : HDC;
begin
   Assert(FForm=nil);

   res:=GetIndexFromResolution(Width, Height, cScreenDepthToBPP[ScreenDepth]);
   if res=0 then
      raise Exception.Create('Unsupported video mode');

   FForm:=TForm.Create(nil);
   with FForm do begin
      if StayOnTop then begin
         FormStyle:=fsStayOnTop;
         FForm.Width:=Self.Width;
         FForm.Height:=Self.Height;
      end else begin
         FormStyle:=fsNormal;
         FForm.WindowState:=wsMaximized;
         FForm.Align:=alClient;
      end;

      BorderStyle:=bsNone;
      Cursor:=Self.Cursor;
      PopupMenu:=Self.PopupMenu;

      BindFormEvents;
      FOldWndProc:=WindowProc;
      WindowProc:=WndProc;
   end;

   if (Screen.Width<>Width) or (Screen.Height<>Height)
         or (GetCurrentColorDepth<>cScreenDepthToBPP[ScreenDepth]) then begin
      SetFullscreenMode(res, FRefreshRate);
      FSwitchedResolution:=True;
   end else FSwitchedResolution:=False;

   FForm.Show;

   Buffer.Resize(Width, Height);
   dc:=GetDC(FForm.Handle);
   Buffer.CreateRC(dc, False);

   // todo
   FActive:=True;
end;

// Shutdown
//
procedure TGLFullScreenViewer.Shutdown;
var
   f : TForm;
begin
   try
      Buffer.DestroyRC;
      f:=FForm;
      FForm:=nil;
      f.WindowProc:=FOldWndProc;
      f.Release;
   finally
      // attempt that, at the very least...
      if FSwitchedResolution then
         RestoreDefaultMode;
   end;
   FActive:=False;
end;

// BindFormEvents
//
procedure TGLFullScreenViewer.BindFormEvents;
begin
   if Assigned(FForm) then with FForm do begin
      OnMouseDown:=FOnMouseDown;
      OnMouseUp:=FOnMouseUp;
      OnMouseMove:=FOnMouseMove;
      OnMouseWheel:=FOnMouseWheel;
      OnClick:=FOnClick;
      OnDblClick:=FOnDblClick;
      OnPaint:=DoPaint;
      OnCloseQuery:=DoCloseQuery;
      OnClose:=FOnClose;
      OnActivate:=DoActivate;
      OnDeactivate:=DoDeactivate;
      OnKeyUp:=FOnKeyUp;
      OnKeyDown:=FOnKeyDown;
      OnKeyPress:=FOnKeyPress;
   end;
end;

// DoCloseQuery
//
procedure TGLFullScreenViewer.DoCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
   if Assigned(FOnCloseQuery) then
      FOnCloseQuery(Sender, CanClose);
   if CanClose then Shutdown;
end;

// DoPaint
//
procedure TGLFullScreenViewer.DoPaint(Sender : TObject);
begin
   if Assigned(FForm) then
      Render;
end;

// WndProc
//
procedure TGLFullScreenViewer.WndProc(var Message: TMessage);
begin
   case Message.Msg of
      WM_ERASEBKGND : begin
         Message.Result:=1;   // do nothing!
      end;
   else
      FOldWndProc(Message);
   end;
end;

// DoActivate
//
procedure TGLFullScreenViewer.DoActivate(Sender : TObject);
begin
   if Assigned(FForm) and StayOnTop then
      FForm.FormStyle:=fsStayOnTop;
end;

// DoDeactivate
//
procedure TGLFullScreenViewer.DoDeactivate(Sender : TObject);
begin
   if Assigned(FForm) and StayOnTop then
      FForm.FormStyle:=fsNormal;
end;

// SetScreenDepth
//
procedure TGLFullScreenViewer.SetScreenDepth(const val : TGLScreenDepth);
begin
   if FScreenDepth<>val then begin
      FScreenDepth:=val;
      DoBufferStructuralChange(Self);
   end;
end;

// SetStayOnTop
//
procedure TGLFullScreenViewer.SetStayOnTop(const val : Boolean);
begin
   if val<>FStayOnTop then begin
      FStayOnTop:=val;
      DoBufferStructuralChange(Self);
   end;
end;

// SetOnCloseQuery
//
procedure TGLFullScreenViewer.SetOnCloseQuery(const val : TCloseQueryEvent);
begin
   FOnCloseQuery:=val; // this one uses a special binding
end;

// SetOnClose
//
procedure TGLFullScreenViewer.SetOnClose(const val : TCloseEvent);
begin
   FOnClose:=val;
   BindFormEvents;
end;

// SetOnKeyPress
//
procedure TGLFullScreenViewer.SetOnKeyPress(const val : TKeyPressEvent);
begin
   FOnKeyPress:=val;
   BindFormEvents;
end;

// SetOnKeyUp
//
procedure TGLFullScreenViewer.SetOnKeyUp(const val : TKeyEvent);
begin
   FOnKeyUp:=val;
   BindFormEvents;
end;

// SetOnKeyDown
//
procedure TGLFullScreenViewer.SetOnKeyDown(const val : TKeyEvent);
begin
   FOnKeyDown:=val;
   BindFormEvents;
end;

// SetOnMouseWheel
//
procedure TGLFullScreenViewer.SetOnMouseWheel(const val : TMouseWheelEvent);
begin
   FOnMouseWheel:=val;
   BindFormEvents;
end;

// SetOnClick
//
procedure TGLFullScreenViewer.SetOnClick(const val : TNotifyEvent);
begin
   FOnClick:=val;
   BindFormEvents;
end;

// SetOnDblClick
//
procedure TGLFullScreenViewer.SetOnDblClick(const val : TNotifyEvent);
begin
   FOnDblClick:=val;
   BindFormEvents;
end;

// SetOnMouseMove
//
procedure TGLFullScreenViewer.SetOnMouseMove(const val : TMouseMoveEvent);
begin
   FOnMouseMove:=val;
   BindFormEvents;
end;

// SetOnMouseDown
//
procedure TGLFullScreenViewer.SetOnMouseDown(const val : TMouseEvent);
begin
   FOnMouseDown:=val;
   BindFormEvents;
end;

// SetOnMouseUp
//
procedure TGLFullScreenViewer.SetOnMouseUp(const val : TMouseEvent);
begin
   FOnMouseUp:=val;
   BindFormEvents;
end;

// SetCursor
//
procedure TGLFullScreenViewer.SetCursor(const val : TCursor);
begin
   if val<>FCursor then begin
      FCursor:=val;
      if Assigned(FForm) then
         FForm.Cursor:=val;
   end;
end;

// SetPopupMenu
//
procedure TGLFullScreenViewer.SetPopupMenu(const val : TPopupMenu);
begin
   if val<>FPopupMenu then begin
      FPopupMenu:=val;
      if Assigned(FForm) then
         FForm.PopupMenu:=val;
   end;
end;

// GetHandle
//
function TGLFullScreenViewer.GetHandle : HWND;
begin
   if Assigned(FForm) then
      Result:=FForm.Handle
   else Result:=0;
end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

   RegisterClasses([TGLSceneViewer, TGLFullScreenViewer]);

end.

