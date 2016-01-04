//
// This unit is part of the DGLEngine Project, http://glscene.org
//
{ @HTML ( DGLWin32Context<p>

      Win32 specific Scene viewer.<p>

 <b>History: </b><font size=-1><ul>
      <li>21/12/15 - JD -  Imported From GLScene
 </ul></font>
}
unit DGLWin32Viewer;

interface

{$I DGLEngine.inc}

uses
  WinApi.Windows, WinApi.Messages,
  System.Classes, System.SysUtils, System.Types,
  VCL.Graphics, VCL.Forms, VCL.Controls,

  // GLS
  DGLTypes, DGLScene, DGLWin32Context,  DGLContext;

type
  TTouchEvent = procedure(X, Y, TouchWidth, TouchHeight : integer; TouchID : Cardinal; MultiTouch : boolean) of object;

  // TDGLSceneViewer
  //
  { @HTML ( Component where the GLScene objects get rendered.<p>
     This component delimits the area where OpenGL renders the scene,
     it represents the 3D scene viewed from a camera (specified in the
     camera property). This component can also render to a file or to a bitmap.<p>
     It is primarily a windowed component, but it can handle full-screen
     operations : simply make this component fit the whole screen (use a
     borderless form).<p>
     This viewer also allows to define rendering options such a fog, face culling,
     depth testing, etc. and can take care of framerate calculation.<p> }
  TDGLSceneViewer = class(TWinControl)
  private
    { Private Declarations }
    FBuffer: TDGLSceneBuffer;
    FVSync: TVSyncMode;
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
    procedure CMMouseEnter(var msg: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var msg: TMessage); message CM_MOUSELEAVE;

    function GetFieldOfView: single;
    procedure SetFieldOfView(const Value: single);
    function GetIsRenderingContextAvailable: Boolean;

  protected
    { Protected Declarations }
    procedure SetBeforeRender(const val: TNotifyEvent);
    function GetBeforeRender: TNotifyEvent;
    procedure SetPostRender(const val: TNotifyEvent);
    function GetPostRender: TNotifyEvent;
    procedure SetAfterRender(const val: TNotifyEvent);
    function GetAfterRender: TNotifyEvent;
    procedure SetCamera(const val: TDGLCamera);
    function GetCamera: TDGLCamera;
    procedure SetBuffer(const val: TDGLSceneBuffer);

    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWnd; override;
    procedure DestroyWnd; override;
    procedure Loaded; override;
    procedure DoBeforeRender(Sender: TObject); dynamic;
    procedure DoBufferChange(Sender: TObject); virtual;
    procedure DoBufferStructuralChange(Sender: TObject); dynamic;

    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;

  public
    { Public Declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    { @HTML ( Makes TWinControl's RecreateWnd public.<p>
       This procedure allows to work around limitations in some OpenGL
       drivers (like MS Software OpenGL) that are not able to share lists
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
    { Published Declarations }
    { @HTML ( Camera from which the scene is rendered. }
    property Camera: TDGLCamera read GetCamera write SetCamera;

    { @HTML ( Specifies if the refresh should be synchronized with the VSync signal.<p>
       If the underlying OpenGL ICD does not support the WGL_EXT_swap_control
       extension, this property is ignored.  }
    property VSync: TVSyncMode read FVSync write FVSync default vsmNoSync;

    { @HTML ( Triggered before the scene's objects get rendered.<p>
       You may use this event to execute your own OpenGL rendering. }
    property BeforeRender: TNotifyEvent read GetBeforeRender write SetBeforeRender;
    { @HTML ( Triggered just after all the scene's objects have been rendered.<p>
       The OpenGL context is still active in this event, and you may use it
       to execute your own OpenGL rendering.<p> }
    property PostRender: TNotifyEvent read GetPostRender write SetPostRender;
    { @HTML ( Called after rendering.<p>
       You cannot issue OpenGL calls in this event, if you want to do your own
       OpenGL stuff, use the PostRender event. }
    property AfterRender: TNotifyEvent read GetAfterRender write SetAfterRender;

    { @HTML ( Access to buffer properties. }
    property Buffer: TDGLSceneBuffer read FBuffer write SetBuffer;

    { @HTML ( Returns or sets the field of view for the viewer, in degrees.<p>
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

    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;

    property OnKeyDown;
    property OnKeyUp;

    property OnContextPopup;
    property TabStop;
    property TabOrder;
    property OnEnter;
    property OnExit;

    property OnGesture;
    property Touch;

  end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
uses
  DGLViewer;

// ------------------
{ TDGLSceneViewer }
{$IFDEF GLS_REGIONS}{$REGION 'TDGLSceneViewer'}{$ENDIF}

constructor TDGLSceneViewer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := [csClickEvents, csDoubleClicks, csOpaque, csCaptureMouse];
  if csDesigning in ComponentState then
    ControlStyle := ControlStyle + [csFramed];
  Width := 100;
  Height := 100;
  FVSync := vsmNoSync;
  FBuffer := TDGLSceneBuffer.Create(Self);
  FBuffer.ViewerBeforeRender := DoBeforeRender;
  FBuffer.OnChange := DoBufferChange;
  FBuffer.OnStructuralChange := DoBufferStructuralChange;
end;

destructor TDGLSceneViewer.Destroy;
begin
  FBuffer.Free;
  FBuffer := nil;
  inherited Destroy;
end;

procedure TDGLSceneViewer.Notification(AComponent: TComponent; Operation: TOperation);
begin
  if (Operation = opRemove) and (FBuffer <> nil) then
  begin
    if (AComponent = FBuffer.Camera) then
      FBuffer.Camera := nil;
  end;
  inherited;
end;

procedure TDGLSceneViewer.RecreateWnd;
begin
  inherited;
end;

procedure TDGLSceneViewer.RegisterTouch;
begin
  RegisterTouchWindow(Handle, 0);
end;

procedure TDGLSceneViewer.SetBeforeRender(const val: TNotifyEvent);
begin
  FBuffer.BeforeRender := val;
end;

function TDGLSceneViewer.GetBeforeRender: TNotifyEvent;
begin
  Result := FBuffer.BeforeRender;
end;

procedure TDGLSceneViewer.SetPostRender(const val: TNotifyEvent);
begin
  FBuffer.PostRender := val;
end;

procedure TDGLSceneViewer.UnregisterTouch;
begin
  UnregisterTouchWindow(Handle);
end;

function TDGLSceneViewer.GetPostRender: TNotifyEvent;
begin
  Result := FBuffer.PostRender;
end;

procedure TDGLSceneViewer.SetAfterRender(const val: TNotifyEvent);
begin
  FBuffer.AfterRender := val;
end;

function TDGLSceneViewer.GetAfterRender: TNotifyEvent;
begin
  Result := FBuffer.AfterRender;
end;

procedure TDGLSceneViewer.SetCamera(const val: TDGLCamera);
begin
  FBuffer.Camera := val;
end;

function TDGLSceneViewer.GetCamera: TDGLCamera;
begin
  Result := FBuffer.Camera;
end;

procedure TDGLSceneViewer.SetBuffer(const val: TDGLSceneBuffer);
begin
  FBuffer.Assign(val);
end;

procedure TDGLSceneViewer.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  with Params do
  begin
    Style := Style or WS_CLIPCHILDREN or WS_CLIPSIBLINGS;
    WindowClass.Style := WindowClass.Style or CS_OWNDC;
  end;
end;

procedure TDGLSceneViewer.CreateWnd;
begin
  inherited CreateWnd;
  // initialize and activate the OpenGL rendering context
  // need to do this only once per window creation as we have a private DC
  FBuffer.Resize(0, 0, Self.Width, Self.Height);
  FOwnDC := GetDC(Handle);
  FBuffer.CreateRC(FOwnDC, False);
end;

procedure TDGLSceneViewer.DestroyWnd;
begin
  FBuffer.DestroyRC;
  if FOwnDC <> 0 then
  begin
    ReleaseDC(Handle, FOwnDC);
    FOwnDC := 0;
  end;
  inherited;
end;

procedure TDGLSceneViewer.WMEraseBkgnd(var Message: TWMEraseBkgnd);
begin
  if IsRenderingContextAvailable then
    Message.Result := 1
  else
    inherited;
end;

procedure TDGLSceneViewer.WMSize(var Message: TWMSize);
begin
  inherited;
  FBuffer.Resize(0, 0, Message.Width, Message.Height);
end;

procedure TDGLSceneViewer.WMTouch(var Message: TMessage);

  function TOUCH_COORD_TO_PIXEL(l : integer) : integer; // Delphi XE needs this
  begin
    result := l div 100;
  end;

  function TouchPointToPoint(const TouchPoint: TTouchInput): TPoint;
  begin
    Result := Point(TOUCH_COORD_TO_PIXEL(TouchPoint.X), TOUCH_COORD_TO_PIXEL(TouchPoint.Y));
    PhysicalToLogicalPoint(Handle, Result);
    Result:=ScreenToClient(Result);
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

procedure TDGLSceneViewer.WMPaint(var Message: TWMPaint);
var
  PS: TPaintStruct;
  p: TPoint;
begin
  p := ClientToScreen(Point(0, 0));
  if (FLastScreenPos.X <> p.X) or (FLastScreenPos.Y <> p.Y) then
  begin
    // Workaround for MS OpenGL "black borders" bug
    if FBuffer.RCInstantiated then
      PostMessage(Handle, WM_SIZE, SIZE_RESTORED,
        Width + (Height shl 16));
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

procedure TDGLSceneViewer.WMGetDglCode(var Message: TMessage);
begin
  Message.Result := Message.Result or DLGC_WANTARROWS;
end;

procedure TDGLSceneViewer.WMDestroy(var Message: TWMDestroy);
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

procedure TDGLSceneViewer.CMMouseEnter(var msg: TMessage);
begin
  inherited;
  FMouseInControl := True;
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self);
end;

procedure TDGLSceneViewer.CMMouseLeave(var msg: TMessage);
begin
  inherited;
  FMouseInControl := False;
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self);
end;

procedure TDGLSceneViewer.Loaded;
begin
  inherited Loaded;
  // initiate window creation
  HandleNeeded;
end;

procedure TDGLSceneViewer.DoBeforeRender(Sender: TObject);
begin
  SetupVSync(VSync);
end;

procedure TDGLSceneViewer.DoBufferChange(Sender: TObject);
begin
  if (not Buffer.Rendering) and (not Buffer.Freezed) then
    Invalidate;
end;

procedure TDGLSceneViewer.DoBufferStructuralChange(Sender: TObject);
begin
  RecreateWnd;
end;

procedure TDGLSceneViewer.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  if csDesignInteractive in ControlStyle then
    FBuffer.NotifyMouseMove(Shift, X, Y);
end;

function TDGLSceneViewer.LastFrameTime: Single;
begin
  Result := FBuffer.LastFrameTime;
end;

function TDGLSceneViewer.FramesPerSecond: Single;
begin
  Result := FBuffer.FramesPerSecond;
end;

function TDGLSceneViewer.FramesPerSecondText(decimals: Integer = 1): string;
begin
  Result := Format('%.*f FPS', [decimals, FBuffer.FramesPerSecond]);
end;

procedure TDGLSceneViewer.ResetPerformanceMonitor;
begin
  FBuffer.ResetPerformanceMonitor;
end;

function TDGLSceneViewer.CreateSnapShotBitmap: TBitmap;
begin
  Result := TBitmap.Create;
  Result.PixelFormat := pf24bit;
  Result.Width := Width;
  Result.Height := Height;

  BitBlt(Result.Canvas.Handle, 0, 0, Width, Height,
    RenderDC, 0, 0, SRCCOPY);
end;

function TDGLSceneViewer.GetFieldOfView: single;
begin
  if not Assigned(Camera) then
    result := 0

  else if Width < Height then
    result := Camera.GetFieldOfView(Width)

  else
    result := Camera.GetFieldOfView(Height);
end;

function TDGLSceneViewer.GetIsRenderingContextAvailable: Boolean;
begin
  Result := FBuffer.RCInstantiated and FBuffer.RenderingContext.IsValid;
end;

procedure TDGLSceneViewer.SetFieldOfView(const Value: single);
begin
  if Assigned(Camera) then
  begin
    if Width < Height then
      Camera.SetFieldOfView(Value, Width)

    else
      Camera.SetFieldOfView(Value, Height);
  end;
end;

{$IFDEF GLS_REGIONS}{$ENDREGION}{$ENDIF}

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization

  RegisterClass(TDGLSceneViewer);

end.

