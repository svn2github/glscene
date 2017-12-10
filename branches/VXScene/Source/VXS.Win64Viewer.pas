//
// VXScene Component Library, based on GLScene http://glscene.sourceforge.net
//
{
   Cross-platform viewer.
}

unit VXS.Win64Viewer;

interface

uses
  Winapi.Windows,
  WinApi.Messages,
  Winapi.OpenGL,
  Winapi.OpenGLext,
  System.Classes,
  System.SysUtils,
  System.Types,
  FMX.Graphics,
  FMX.Forms,
  FMX.Controls,
  FMX.Dialogs.Win,
  FMX.Viewport3D,
  
  VXS.OpenGL1x,
  VXS.Scene,
  VXS.Win64Context,
  VXS.Context;

type
  TCreateParams = record
    Caption: PChar;
    Style: DWORD;
    ExStyle: DWORD;
    X, Y: Integer;
    Width, Height: Integer;
    //WndParent: HWnd; - not fmx
    Param: Pointer;
    //WindowClass: TWndClass;  - not fmx
    WinClassName: array[0..63] of Char;
  end;

  TTouchEvent = procedure(X, Y, TouchWidth, TouchHeight : integer; TouchID : Cardinal; MultiTouch : boolean) of object;

  { Component where the GLScene objects get rendered.
     This component delimits the area where OpenVX renders the scene,
     it represents the 3D scene viewed from a camera (specified in the
     camera property). This component can also render to a file or to a bitmap.
     It is primarily a windowed component, but it can handle full-screen
     operations : simply make this component fit the whole screen (use a
     borderless form).
     This viewer also allows to define rendering options such a fog, face culling,
     depth testing, etc. and can take care of framerate calculation.  }
  TVXSceneViewer = class(TViewPort3D)
  private
    FBuffer: TVXSceneBuffer;
    FVSync: TVXSyncMode;
    FOwnDC: HDC;
    FOnMouseEnter, FOnMouseLeave: TNotifyEvent;
    FMouseInControl: Boolean;
    FLastScreenPos: TPoint;
    FOnTouchMove: TTouchEvent;
    FOnTouchUp: TTouchEvent;
    FOnTouchDown: TTouchEvent;
    procedure WMEraseBkgnd(var Message: TWMEraseBkgnd); message WM_ERASEBKGND;
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
    procedure WMSize(var Message: TWMSize); message WM_SIZE;
    procedure WMGetDglCode(var Message: TMessage); message WM_GETDLGCODE;
    procedure WMDestroy(var Message: TWMDestroy); message WM_DESTROY;
    procedure WMTouch(var Message: TMessage); message WM_TOUCH;
    { TODO -oPW -cMessages : Convert message CM_MOUSEENTER  to FMX }
    procedure CMMouseEnter(var msg: TMessage);
    { TODO -oPW -cMessages :  Convert message CM_MOUSELEAVE;  to FMX }
    procedure CMMouseLeave(var msg: TMessage);
    function GetFieldOfView: single;
    procedure SetFieldOfView(const Value: single);
    function GetIsRenderingContextAvailable: Boolean;
  protected
    procedure SetBeforeRender(const val: TNotifyEvent);
    function GetBeforeRender: TNotifyEvent;
    procedure SetPostRender(const val: TNotifyEvent);
    function GetPostRender: TNotifyEvent;
    procedure SetAfterRender(const val: TNotifyEvent);
    function GetAfterRender: TNotifyEvent;
    procedure SetCamera(const val: TVXCamera);
    function GetCamera: TVXCamera;
    procedure SetBuffer(const val: TVXSceneBuffer);
    procedure CreateParams(var Params: TCreateParams); /// Vcl - override;
    procedure CreateWnd; /// Vcl - override;
    procedure DestroyWnd; /// Vcl - override;
    procedure Loaded; override;
    procedure DoBeforeRender(Sender: TObject); virtual;
    procedure DoBufferChange(Sender: TObject); virtual;
    procedure DoBufferStructuralChange(Sender: TObject); virtual;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); /// Vcl - override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    { Makes TWinControl's RecreateWnd public.
       This procedure allows to work around limitations in some OpenVX
       drivers (like MS Software OpenVX) that are not able to share lists
       between RCs that already have display lists. }
    procedure RecreateWnd;
    property IsRenderingContextAvailable: Boolean read GetIsRenderingContextAvailable;
    function LastFrameTime: Single;
    function FramesPerSecond: Single;
    function FramesPerSecondText(decimals: Integer = 1): string;
    procedure ResetPerformanceMonitor;
    function CreateSnapShotBitmap: TBitmap;
    procedure RegisterTouch;
    procedure UnregisterTouch;
    property RenderDC: HDC read FOwnDC;
    property MouseInControl: Boolean read FMouseInControl;
  published
    { Camera from which the scene is rendered. }
    property Camera: TVXCamera read GetCamera write SetCamera;
    { Specifies if the refresh should be synchronized with the VSync signal.
       If the underlying OpenVX ICD does not support the WGL_EXT_swap_control
       extension, this property is ignored.  }
    property VSync: TVXSyncMode read FVSync write FVSync default vsmNoSync;
    { Triggered before the scene's objects get rendered.
       You may use this event to execute your own OpenVX rendering. }
    property BeforeRender: TNotifyEvent read GetBeforeRender write SetBeforeRender;
    { Triggered just after all the scene's objects have been rendered.
       The OpenVX context is still active in this event, and you may use it
       to execute your own OpenVX rendering.  }
    property PostRender: TNotifyEvent read GetPostRender write SetPostRender;
    { Called after rendering.
       You cannot issue OpenVX calls in this event, if you want to do your own
       OpenVX stuff, use the PostRender event. }
    property AfterRender: TNotifyEvent read GetAfterRender write SetAfterRender;
    { Access to buffer properties. }
    property Buffer: TVXSceneBuffer read FBuffer write SetBuffer;
    { Returns or sets the field of view for the viewer, in degrees.
    This value depends on the camera and the width and height of the scene.
    The value isn't persisted, if the width/height or camera.focallength is
    changed, FieldOfView is changed also. }
    property FieldOfView: single read GetFieldOfView write SetFieldOfView;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnTouchMove: TTouchEvent read FOnTouchMove write FOnTouchMove;
    property OnTouchUp: TTouchEvent read FOnTouchUp write FOnTouchUp;
    property OnTouchDown: TTouchEvent read FOnTouchDown write FOnTouchDown;
    property Align;
    property Anchors;
///    property DragCursor; - Vcl
    property DragMode;
    property Enabled;
///    property HelpContext; - Vcl
    property Hint;
    property PopupMenu;
    property Visible;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
///    property OnStartDrag; - Vcl
///    property OnEndDrag; - Vcl
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
///    property OnMouseWheelDown; - Vcl
///    property OnMouseWheelUp; - Vcl
    property OnKeyDown;
    property OnKeyUp;
///    property OnContextPopup; - Vcl
    property TabStop;
    property TabOrder;
    property OnEnter;
    property OnExit;

    property OnGesture;
    property Touch;
  end;

procedure SetupVSync(const AVSyncMode : TVXSyncMode);

var
 Handle: HWND;

// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------

// ------------------
// ------------------ TVXSceneViewerFMX ------------------
// ------------------

procedure SetupVSync(const AVSyncMode : TVXSyncMode);
var
  I: Integer;
begin
  if WGL_EXT_swap_control then
  begin
  {!  TODO
    I := wglGetSwapIntervalEXT;
    case AVSyncMode of
      vsmSync  : if I <> 1 then  wglSwapIntervalEXT(1);
      vsmNoSync: if I <> 0 then  wglSwapIntervalEXT(0);
    else
       Assert(False);
    end;
    }
  end;
end;

constructor TVXSceneViewer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  { TODO -oPW -cStyles : ContrilStyle in FMX }
(*  - todo Vcl styles convert to FMX styles
  ControlStyle := [csClickEvents, csDoubleClicks, csOpaque, csCaptureMouse];
  if csDesigning in ComponentState then
    ControlStyle := ControlStyle + [csFramed];
*)
  Width := 100;
  Height := 100;
  FVSync := vsmNoSync;
  FBuffer := TVXSceneBuffer.Create(Self);
  FBuffer.ViewerBeforeRender := DoBeforeRender;
  FBuffer.OnChange := DoBufferChange;
  FBuffer.OnStructuralChange := DoBufferStructuralChange;
end;

destructor TVXSceneViewer.Destroy;
begin
  FBuffer.Free;
  FBuffer := nil;
  inherited Destroy;
end;

procedure TVXSceneViewer.Notification(AComponent: TComponent; Operation: TOperation);
begin
  if (Operation = opRemove) and (FBuffer <> nil) then
  begin
    if (AComponent = FBuffer.Camera) then
      FBuffer.Camera := nil;
  end;
  inherited;
end;

procedure TVXSceneViewer.RecreateWnd;
begin
  inherited;
end;

procedure TVXSceneViewer.RegisterTouch;
begin
  /// RegisterTouchWindow(Handle, 0); - Vcl Handle
end;

procedure TVXSceneViewer.SetBeforeRender(const val: TNotifyEvent);
begin
  FBuffer.BeforeRender := val;
end;

function TVXSceneViewer.GetBeforeRender: TNotifyEvent;
begin
  Result := FBuffer.BeforeRender;
end;

procedure TVXSceneViewer.SetPostRender(const val: TNotifyEvent);
begin
  FBuffer.PostRender := val;
end;

procedure TVXSceneViewer.UnregisterTouch;
begin
///  UnregisterTouchWindow(Handle); - Vcl Handle
end;

function TVXSceneViewer.GetPostRender: TNotifyEvent;
begin
  Result := FBuffer.PostRender;
end;

procedure TVXSceneViewer.SetAfterRender(const val: TNotifyEvent);
begin
  FBuffer.AfterRender := val;
end;

function TVXSceneViewer.GetAfterRender: TNotifyEvent;
begin
  Result := FBuffer.AfterRender;
end;

procedure TVXSceneViewer.SetCamera(const val: TVXCamera);
begin
  FBuffer.Camera := val;
end;

function TVXSceneViewer.GetCamera: TVXCamera;
begin
  Result := FBuffer.Camera;
end;

procedure TVXSceneViewer.SetBuffer(const val: TVXSceneBuffer);
begin
  FBuffer.Assign(val);
end;

procedure TVXSceneViewer.CreateParams(var Params: TCreateParams);
begin
  inherited;  /// Vcl -  inherited CreateParams(Params);
  with Params do
  begin
    Style := Style or WS_CLIPCHILDREN or WS_CLIPSIBLINGS;
    { TODO : E2003 Undeclared identifier: 'WindowClass'  in FMX.Forms.TCreateParams}
    (*WindowClass.Style := WindowClass.Style or CS_OWNDC;*)
  end;
end;

procedure TVXSceneViewer.CreateWnd;
begin
  inherited;  /// Vcl -  inherited CreateWnd;
  // initialize and activate the OpenGL rendering context
  // need to do this only once per window creation as we have a private DC
  FBuffer.Resize(0, 0, Round(Self.Width), Round(Self.Height));
  FOwnDC := GetDC(Handle);
  FBuffer.CreateRC(FOwnDC, False);
end;

procedure TVXSceneViewer.DestroyWnd;
begin
  FBuffer.DestroyRC;
  if FOwnDC <> 0 then
  begin

    ReleaseDC(Handle, FOwnDC);
    FOwnDC := 0;
  end;
  inherited;
end;

procedure TVXSceneViewer.WMEraseBkgnd(var Message: TWMEraseBkgnd);
begin
  if IsRenderingContextAvailable then
    Message.Result := 1
  else
    inherited;
end;

procedure TVXSceneViewer.WMSize(var Message: TWMSize);
begin
  inherited;
  FBuffer.Resize(0, 0, Message.Width, Message.Height);
end;

procedure TVXSceneViewer.WMTouch(var Message: TMessage);
  function TouchPointToPoint(const TouchPoint: TTouchInput): TPoint;
  begin
    Result := Point(TOUCH_COORD_TO_PIXEL(TouchPoint.X), TOUCH_COORD_TO_PIXEL(TouchPoint.Y));
    PhysicalToLogicalPoint(Handle, Result);
    { TODO -oPW -cIncompatibility : Incompatible Handle and Result in FMX }
    ///Result:=ScreenToClient(Result);
  end;

var
  TouchInputs: array of TTouchInput;
  TouchInput: TTouchInput;
  Handled: Boolean;
  Point: TPoint;
  Multitouch : boolean;
begin
  Handled := False;
  SetLength(TouchInputs, Message.WParam);
  Multitouch := Message.WParam > 1;
  GetTouchInputInfo(Message.LParam, Message.WParam, @TouchInputs[0],
    SizeOf(TTouchInput));
  try
    for TouchInput in TouchInputs do
    begin
      Point := TouchPointToPoint(TouchInput);

      if (TouchInput.dwFlags AND TOUCHEVENTF_MOVE) > 0 then
      if Assigned(OnTouchMove) then
      begin
        OnTouchMove(Point.X, Point.Y, TouchInput.cxContact, TouchInput.cyContact, TouchInput.dwID, Multitouch);
      end;

      if (TouchInput.dwFlags AND TOUCHEVENTF_DOWN) > 0 then
      if Assigned(OnTouchDown) then
      begin
        OnTouchDown(Point.X, Point.Y, TouchInput.cxContact, TouchInput.cyContact, TouchInput.dwID, Multitouch);
      end;

      if (TouchInput.dwFlags AND TOUCHEVENTF_UP) > 0 then
      if Assigned(OnTouchUp) then
      begin
        OnTouchUp(Point.X, Point.Y, TouchInput.cxContact, TouchInput.cyContact, TouchInput.dwID, Multitouch);
      end;
    end;

    Handled := True;
  finally
    if Handled then
      CloseTouchInputHandle(Message.LParam)
    else
      inherited;
  end;
end;

procedure TVXSceneViewer.WMPaint(var Message: TWMPaint);
var
  PS: TPaintStruct;
  p: TPoint;
begin
  { TODO -oPW -cUnworkable : FMX.Forms.IFMXWindowService.ClientToScreen in FMX }
///  p := ClientToScreen(Point(0, 0));
  if (FLastScreenPos.X <> p.X) or (FLastScreenPos.Y <> p.Y) then
  begin
    // Workaround for MS OpenGL "black borders" bug
    if FBuffer.RCInstantiated then
     { TODO -oPW -cUnworkable : Not applicable in FMX }
///      PostMessage(Handle, WM_SIZE, SIZE_RESTORED, Width + (Height shl 16));
    FLastScreenPos := p;
  end;
  BeginPaint(Handle, PS);
  try
    if IsRenderingContextAvailable and (Width > 0) and (Height > 0) then
      FBuffer.Render;
  finally
    EndPaint(Handle, PS);
    Message.Result := 0;
  end;
end;

procedure TVXSceneViewer.WMGetDglCode(var Message: TMessage);
begin
  Message.Result := Message.Result or DLGC_WANTARROWS;
end;

procedure TVXSceneViewer.WMDestroy(var Message: TWMDestroy);
begin
  if Assigned(FBuffer) then
  begin
    FBuffer.DestroyRC;
    if FOwnDC <> 0 then
    begin
      ReleaseDC(Handle, FOwnDC);
      FOwnDC := 0;
    end;
  end;
  inherited;
end;

procedure TVXSceneViewer.CMMouseEnter(var msg: TMessage);
begin
  inherited;
  FMouseInControl := True;
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self);
end;

procedure TVXSceneViewer.CMMouseLeave(var msg: TMessage);
begin
  inherited;
  FMouseInControl := False;
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self);
end;

procedure TVXSceneViewer.Loaded;
begin
  inherited Loaded;
  // initiate window creation
  { TODO -oPW -cUnworkable : HandleNeeded not found in FMX.Controls }
  ///HandleNeeded;
end;

procedure TVXSceneViewer.DoBeforeRender(Sender: TObject);
begin
  SetupVSync(VSync);
end;

procedure TVXSceneViewer.DoBufferChange(Sender: TObject);
begin
  if (not Buffer.Rendering) and (not Buffer.Freezed) then
  { TODO -oPW -cUnworkable : Invalidate not found in FMX.Controls }
  ///  Invalidate;
end;

procedure TVXSceneViewer.DoBufferStructuralChange(Sender: TObject);
begin
  RecreateWnd;
end;

procedure TVXSceneViewer.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
   { TODO -oPW -cIncompatibility : not found in FMX.Controls }
///  inherited;
///  if csDesignInteractive in ControlStyle then FBuffer.NotifyMouseMove(Shift, X, Y);
end;

function TVXSceneViewer.LastFrameTime: Single;
begin
  Result := FBuffer.LastFrameTime;
end;

function TVXSceneViewer.FramesPerSecond: Single;
begin
  Result := FBuffer.FramesPerSecond;
end;

function TVXSceneViewer.FramesPerSecondText(decimals: Integer = 1): string;
begin
  Result := Format('%.*f FPS', [decimals, FBuffer.FramesPerSecond]);
end;

procedure TVXSceneViewer.ResetPerformanceMonitor;
begin
  FBuffer.ResetPerformanceMonitor;
end;

function TVXSceneViewer.CreateSnapShotBitmap: TBitmap;
begin
  Result := TBitmap.Create;
  { TODO -oPW -cIncompatibility : Find analog of pf24bit in FMX }
///  Result.PixelFormat := pf24bit;
  Result.Width := Round(Width);
  Result.Height := Round(Height);
  { TODO -oPW -cUnworkable : Handle not found in FMX }
///  BitBlt(Result.Canvas.Handle, 0, 0, Width, Height, RenderDC, 0, 0, SRCCOPY);
end;

function TVXSceneViewer.GetFieldOfView: single;
begin
  if not Assigned(Camera) then
    result := 0
  else if Width < Height then
    result := Camera.GetFieldOfView(Width)
  else
    result := Camera.GetFieldOfView(Height);
end;

function TVXSceneViewer.GetIsRenderingContextAvailable: Boolean;
begin
  Result := FBuffer.RCInstantiated and FBuffer.RenderingContext.IsValid;
end;

procedure TVXSceneViewer.SetFieldOfView(const Value: single);
begin
  if Assigned(Camera) then
  begin
    if Width < Height then
      Camera.SetFieldOfView(Value, Width)
    else
      Camera.SetFieldOfView(Value, Height);
  end;
end;

// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------

  RegisterClass(TVXSceneViewer);

end.

