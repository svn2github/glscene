﻿//
// This unit is part of the DGLEngine Project, http://glscene.org
//
{ : DGLSceneForm<p>

  <b>History : </b><font size=-1><ul>
    <li>23/08/10 - JD - Imported from GLScene
  </ul></font>
}

unit DGLSceneForm;

interface

{$I DGLEngine.inc}

uses
  Winapi.Windows,
  Winapi.Messages,
  System.Classes,
  VCL.Controls,
  VCL.Forms,
  //GLS
  DGLTypes,
  DGLScene,
  DGLContext,
  DGLCrossPlatform,
  DGLScreen,
  DGLViewer;

const
  lcl_major = 0;
  lcl_minor = 0;
  lcl_release = 0;

type

  TDGLSceneForm = class;

  // TDGLFullScreenResolution
  //
  { @HTML ( Defines how GLSceneForm will handle fullscreen request
     fcWindowMaximize: Use current resolution (just maximize form and hide OS bars)
     fcNearestResolution: Change to nearest valid resolution from current window size
     fcManualResolution: Use FFullScreenVideoMode settings }
  TDGLFullScreenResolution = (
    fcUseCurrent,
    fcNearestResolution,
    fcManualResolution);

  // TDGLFullScreenVideoMode
  //
  { @HTML ( Screen mode settings }
  TDGLFullScreenVideoMode = class(TPersistent)
  private
    FOwner: TDGLSceneForm;
    FEnabled: Boolean;
    FAltTabSupportEnable: Boolean;
    FWidth: Integer;
    FHeight: Integer;
    FColorDepth: Integer;
    FFrequency: Integer;
    FResolutionMode: TDGLFullScreenResolution;
    procedure SetEnabled(aValue: Boolean);
    procedure SetAltTabSupportEnable(aValue: Boolean);
  public
    constructor Create(AOwner: TDGLSceneForm);
  published
    property Enabled: Boolean read FEnabled write SetEnabled default False;
    property AltTabSupportEnable: Boolean read FAltTabSupportEnable
      write SetAltTabSupportEnable default False;
    property ResolutionMode: TDGLFullScreenResolution read FResolutionMode
      write FResolutionMode default fcUseCurrent;
    property Width: Integer read FWidth write FWidth;
    property Height: Integer read FHeight write FHeight;
    property ColorDepth: Integer read FColorDepth write FColorDepth;
    property Frequency: Integer read FFrequency write FFrequency;
  end;

  { TDGLSceneForm }

  TDGLSceneForm = class(TForm)
  private
    { Private Declarations }
    FBuffer: TDGLSceneBuffer;
    FVSync: TVSyncMode;
    FOwnDC: HDC;
    FFullScreenVideoMode: TDGLFullScreenVideoMode;
    procedure SetBeforeRender(const val: TNotifyEvent);
    function GetBeforeRender: TNotifyEvent;
    procedure SetPostRender(const val: TNotifyEvent);
    function GetPostRender: TNotifyEvent;
    procedure SetAfterRender(const val: TNotifyEvent);
    function GetAfterRender: TNotifyEvent;
    procedure SetCamera(const val: TDGLCamera);
    function GetCamera: TDGLCamera;
    procedure SetBuffer(const val: TDGLSceneBuffer);

    function GetFieldOfView: single;
    procedure SetFieldOfView(const Value: single);
    function GetIsRenderingContextAvailable: Boolean;
    procedure WMEraseBkgnd(var Message: TWMEraseBkgnd); message WM_ERASEBKGND;
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
    procedure WMSize(var Message: TWMSize); message WM_SIZE;
    procedure WMDestroy(var Message: TWMDestroy); message WM_DESTROY;
    procedure LastFocus(var Mess: TMessage); message WM_ACTIVATE;
    procedure SetFullScreenVideoMode(AValue: TDGLFullScreenVideoMode);
    procedure StartupFS;
    procedure ShutdownFS;
  protected
    { Protected Declarations }
    procedure Notification(AComponent: TComponent; Operation: TOperation);
      override;
    procedure CreateWnd; override;
    procedure Loaded; override;

    procedure DoBeforeRender(Sender: TObject); dynamic;
    procedure DoBufferChange(Sender: TObject); virtual;
    procedure DoBufferStructuralChange(Sender: TObject); dynamic;

    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure DestroyWnd; override;

    property IsRenderingContextAvailable: Boolean read
      GetIsRenderingContextAvailable;
    property RenderDC: HDC read FOwnDC;
  published
    { Published Declarations }
    { : Camera from which the scene is rendered. }
    property Camera: TDGLCamera read GetCamera write SetCamera;

    { : Specifies if the refresh should be synchronized with the VSync signal.<p>
      If the underlying OpenGL ICD does not support the WGL_EXT_swap_control
      extension, this property is ignored. }
    property VSync: TVSyncMode read FVSync write FVSync default vsmNoSync;

    { : Triggered before the scene's objects get rendered.<p>
      You may use this event to execute your own OpenGL rendering. }
    property BeforeRender: TNotifyEvent read GetBeforeRender write
      SetBeforeRender;
    { : Triggered just after all the scene's objects have been rendered.<p>
      The OpenGL context is still active in this event, and you may use it
      to execute your own OpenGL rendering.<p> }
    property PostRender: TNotifyEvent read GetPostRender write SetPostRender;
    { : Called after rendering.<p>
      You cannot issue OpenGL calls in this event, if you want to do your own
      OpenGL stuff, use the PostRender event. }
    property AfterRender: TNotifyEvent read GetAfterRender write SetAfterRender;

    { : Access to buffer properties. }
    property Buffer: TDGLSceneBuffer read FBuffer write SetBuffer;

    { : Returns or sets the field of view for the viewer, in degrees.<p>
      This value depends on the camera and the width and height of the scene.
      The value isn't persisted, if the width/height or camera.focallength is
      changed, FieldOfView is changed also. }
    property FieldOfView: single read GetFieldOfView write SetFieldOfView;

    property FullScreenVideoMode: TDGLFullScreenVideoMode read
      FFullScreenVideoMode
      write SetFullScreenVideoMode;
  end;

implementation

constructor TDGLSceneForm.Create(AOwner: TComponent);
begin
  FBuffer := TDGLSceneBuffer.Create(Self);
  FVSync := vsmNoSync;
  FBuffer.ViewerBeforeRender := DoBeforeRender;
  FBuffer.OnChange := DoBufferChange;
  FBuffer.OnStructuralChange := DoBufferStructuralChange;
  FFullScreenVideoMode := TDGLFullScreenVideoMode.Create(Self);
  inherited Create(AOwner);
end;

destructor TDGLSceneForm.Destroy;
begin
  FBuffer.Free;
  FBuffer := nil;
  FFullScreenVideoMode.Destroy;
  inherited Destroy;
end;

// Notification
//

procedure TDGLSceneForm.Notification(AComponent: TComponent; Operation:
  TOperation);
begin
  if (Operation = opRemove) and (FBuffer <> nil) then
  begin
    if (AComponent = FBuffer.Camera) then
      FBuffer.Camera := nil;
  end;
  inherited;
end;

// CreateWnd
//

procedure TDGLSceneForm.CreateWnd;
begin
  inherited CreateWnd;
  // initialize and activate the OpenGL rendering context
  // need to do this only once per window creation as we have a private DC
  FBuffer.Resize(0, 0, Self.Width, Self.Height);
  FOwnDC := GetDC(Handle);
  FBuffer.CreateRC(FOwnDC, false);
end;

// DestroyWnd
//

procedure TDGLSceneForm.DestroyWnd;
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

// Loaded
//

procedure TDGLSceneForm.Loaded;
begin
  inherited Loaded;
  // initiate window creation
  HandleNeeded;
  if not (csDesigning in ComponentState) then
  begin
    if FFullScreenVideoMode.FEnabled then
      StartupFS;
  end;
end;

// WMEraseBkgnd
//

procedure TDGLSceneForm.WMEraseBkgnd(var Message: TWMEraseBkgnd);
begin
  if GetIsRenderingContextAvailable then
    Message.Result := 1
  else
    inherited;
end;

// WMSize
//

procedure TDGLSceneForm.WMSize(var Message: TWMSize);
begin
  inherited;
  if Assigned(FBuffer) then
    FBuffer.Resize(0, 0, Message.Width, Message.Height);
end;

// WMPaint
//

procedure TDGLSceneForm.WMPaint(var Message: TWMPaint);
var
  PS: TPaintStruct;
begin
  BeginPaint(Handle, PS);
  try
    if GetIsRenderingContextAvailable and (Width > 0) and (Height > 0) then
      FBuffer.Render;
  finally
    EndPaint(Handle, PS);
    Message.Result := 0;
  end;
end;

// WMDestroy
//

procedure TDGLSceneForm.WMDestroy(var Message: TWMDestroy);
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

// LastFocus
//

procedure TDGLSceneForm.LastFocus(var Mess: TMessage);
begin
  if not (csDesigning in ComponentState)
    and FFullScreenVideoMode.FEnabled
    and FFullScreenVideoMode.FAltTabSupportEnable then
    begin
      if Mess.wParam = WA_INACTIVE then
      begin
        ShutdownFS;
      end
      else
      begin
        StartupFS;
      end;
    end;
  inherited;
end;

procedure TDGLFullScreenVideoMode.SetEnabled(aValue: Boolean);
begin
  if FEnabled <> aValue then
  begin
    FEnabled := aValue;
    if not ((csDesigning in FOwner.ComponentState)
      or (csLoading in FOwner.ComponentState)) then
    begin
      if FEnabled then
        FOwner.StartupFS
      else
        FOwner.ShutdownFS;
    end;
  end;
end;

constructor TDGLFullScreenVideoMode.Create(AOwner: TDGLSceneForm);
begin
  inherited Create;
  FOwner := AOwner;
  FEnabled := False;
  FAltTabSupportEnable := False;
  ReadVideoModes;
{$IFDEF MSWINDOWS}
  FWidth := vVideoModes[0].Width;
  FHeight := vVideoModes[0].Height;
  FColorDepth := vVideoModes[0].ColorDepth;
  FFrequency := vVideoModes[0].MaxFrequency;
{$ENDIF}
{$IFDEF GLS_X11_SUPPORT}
  FWidth := vVideoModes[0].vdisplay;
  FHeight := vVideoModes[0].hdisplay;
  FColorDepth := 32;
  FFrequency := 0;
{$ENDIF}
{$IFDEF DARWIN}
  FWidth := 1280;
  FHeight := 1024;
  FColorDepth := 32;
  FFrequency := 0;
  {$Message Hint 'Fullscreen mode not yet implemented for Darwin OSes' }
{$ENDIF}
  if FFrequency = 0 then
    FFrequency := 50;
  FResolutionMode := fcUseCurrent;
end;

procedure TDGLFullScreenVideoMode.SetAltTabSupportEnable(aValue: Boolean);
begin
  if FAltTabSupportEnable <> aValue then
    FAltTabSupportEnable := aValue;
end;

procedure TDGLSceneForm.StartupFS;
begin
  case FFullScreenVideoMode.FResolutionMode of
    fcNearestResolution:
      begin
        SetFullscreenMode(GetIndexFromResolution(ClientWidth, ClientHeight,
{$IFDEF MSWINDOWS}
        vVideoModes[0].ColorDepth));
{$ELSE}
        32));
{$ENDIF}
      end;
    fcManualResolution:
      begin
        SetFullscreenMode(GetIndexFromResolution(FFullScreenVideoMode.Width , FFullScreenVideoMode.Height, FFullScreenVideoMode.ColorDepth), FFullScreenVideoMode.Frequency);
      end;
  end;

  Left := 0;
  Top := 0;
  BorderStyle := bsNone;
  FormStyle := fsStayOnTop;
  BringToFront;
  WindowState := wsMaximized;

  Application.MainFormOnTaskBar := True;
end;

procedure TDGLSceneForm.ShutdownFS;
begin
  RestoreDefaultMode;
  SendToBack;
  WindowState := wsNormal;
  BorderStyle := bsSingle;
  FormStyle := fsNormal;
  Left := (Screen.Width div 2) - (Width div 2);
  Top := (Screen.Height div 2) - (Height div 2);
end;

// DoBeforeRender
//

procedure TDGLSceneForm.DoBeforeRender(Sender: TObject);
begin
  SetupVSync(VSync);
end;

// DoBufferChange
//

procedure TDGLSceneForm.DoBufferChange(Sender: TObject);
begin
  if (not Buffer.Rendering) and (not Buffer.Freezed) then
    Invalidate;
end;

// DoBufferStructuralChange
//

procedure TDGLSceneForm.DoBufferStructuralChange(Sender: TObject);
begin
  RecreateWnd;
end;

procedure TDGLSceneForm.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  if csDesignInteractive in ControlStyle then
    FBuffer.NotifyMouseMove(Shift, X, Y);
end;

// SetBeforeRender
//

procedure TDGLSceneForm.SetBeforeRender(const val: TNotifyEvent);
begin
  FBuffer.BeforeRender := val;
end;

// GetBeforeRender
//

function TDGLSceneForm.GetBeforeRender: TNotifyEvent;
begin
  Result := FBuffer.BeforeRender;
end;

// SetPostRender
//

procedure TDGLSceneForm.SetPostRender(const val: TNotifyEvent);
begin
  FBuffer.PostRender := val;
end;

// GetPostRender
//

function TDGLSceneForm.GetPostRender: TNotifyEvent;
begin
  Result := FBuffer.PostRender;
end;

// SetAfterRender
//

procedure TDGLSceneForm.SetAfterRender(const val: TNotifyEvent);
begin
  FBuffer.AfterRender := val;
end;

// GetAfterRender
//

function TDGLSceneForm.GetAfterRender: TNotifyEvent;
begin
  Result := FBuffer.AfterRender;
end;

// SetCamera
//

procedure TDGLSceneForm.SetCamera(const val: TDGLCamera);
begin
  FBuffer.Camera := val;
end;

// GetCamera
//

function TDGLSceneForm.GetCamera: TDGLCamera;
begin
  Result := FBuffer.Camera;
end;

// SetBuffer
//

procedure TDGLSceneForm.SetBuffer(const val: TDGLSceneBuffer);
begin
  FBuffer.Assign(val);
end;

// GetFieldOfView
//

function TDGLSceneForm.GetFieldOfView: single;
begin
  if not Assigned(Camera) then
    Result := 0
  else if Width < Height then
    Result := Camera.GetFieldOfView(Width)
  else
    Result := Camera.GetFieldOfView(Height);
end;

// SetFieldOfView
//

procedure TDGLSceneForm.SetFieldOfView(const Value: single);
begin
  if Assigned(Camera) then
  begin
    if Width < Height then
      Camera.SetFieldOfView(Value, Width)
    else
      Camera.SetFieldOfView(Value, Height);
  end;
end;

procedure TDGLSceneForm.SetFullScreenVideoMode(AValue: TDGLFullScreenVideoMode);
begin
end;

// GetIsRenderingContextAvailable
//

function TDGLSceneForm.GetIsRenderingContextAvailable: Boolean;
begin
  Result := FBuffer.RCInstantiated and FBuffer.RenderingContext.IsValid;
end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization

  // ------------------------------------------------------------------
  // ------------------------------------------------------------------
  // ------------------------------------------------------------------

  RegisterClass(TDGLSceneForm);

end.