//
// This unit is part of the GLScene Project, http://glscene.org
//
{: GLSceneForm<p>

   <b>History : </b><font size=-1><ul>
    <li>23/08/10 - Yar - Creation
 </ul></font>
}

unit GLSceneForm;

interface

{$I GLScene.inc}

uses
{$IFDEF MSWINDOWS}
  Windows,
{$ENDIF}
  Classes,
  Messages,
  Forms,
  GLScene,
  GLContext;

type

  TGLSceneForm = class(TForm)
  private
    { Private Declarations }
    FBuffer: TGLSceneBuffer;
    FVSync: TVSyncMode;
    FOwnDC: HDC;
    procedure SetBeforeRender(const val: TNotifyEvent);
    function GetBeforeRender: TNotifyEvent;
    procedure SetPostRender(const val: TNotifyEvent);
    function GetPostRender: TNotifyEvent;
    procedure SetAfterRender(const val: TNotifyEvent);
    function GetAfterRender: TNotifyEvent;
    procedure SetCamera(const val: TGLCamera);
    function GetCamera: TGLCamera;
    procedure SetBuffer(const val: TGLSceneBuffer);

    function GetFieldOfView: single;
    procedure SetFieldOfView(const Value: single);
    function GetIsRenderingContextAvailable: Boolean;

    procedure WMEraseBkgnd(var Message: TWMEraseBkgnd); message WM_ERASEBKGND;
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
    procedure WMSize(var Message: TWMSize); message WM_SIZE;
    procedure WMDestroy(var Message: TWMDestroy); message WM_DESTROY;
  protected
    { Protected Declarations }
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure CreateWnd; override;
    procedure DestroyWnd; override;
    procedure Loaded; override;

    procedure DoBeforeRender(Sender: TObject); dynamic;
    procedure DoBufferChange(Sender: TObject); virtual;
    procedure DoBufferStructuralChange(Sender: TObject); dynamic;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property RenderDC: HDC read FOwnDC;
  published
    { Published Declarations }
    {: Camera from which the scene is rendered. }
    property Camera: TGLCamera read GetCamera write SetCamera;

    {: Specifies if the refresh should be synchronized with the VSync signal.<p>
       If the underlying OpenGL ICD does not support the WGL_EXT_swap_control
       extension, this property is ignored.  }
    property VSync: TVSyncMode read FVSync write FVSync default vsmNoSync;

    {: Triggered before the scene's objects get rendered.<p>
       You may use this event to execute your own OpenGL rendering. }
    property BeforeRender: TNotifyEvent read GetBeforeRender write SetBeforeRender;
    {: Triggered just after all the scene's objects have been rendered.<p>
       The OpenGL context is still active in this event, and you may use it
       to execute your own OpenGL rendering.<p> }
    property PostRender: TNotifyEvent read GetPostRender write SetPostRender;
    {: Called after rendering.<p>
       You cannot issue OpenGL calls in this event, if you want to do your own
       OpenGL stuff, use the PostRender event. }
    property AfterRender: TNotifyEvent read GetAfterRender write SetAfterRender;

    {: Access to buffer properties. }
    property Buffer: TGLSceneBuffer read FBuffer write SetBuffer;

    {: Returns or sets the field of view for the viewer, in degrees.<p>
    This value depends on the camera and the width and height of the scene.
    The value isn't persisted, if the width/height or camera.focallength is
    changed, FieldOfView is changed also. }
    property FieldOfView: single read GetFieldOfView write SetFieldOfView;
  end;

implementation

uses
  GLViewer;

constructor TGLSceneForm.Create(AOwner: TComponent);
begin
  FBuffer := TGLSceneBuffer.Create(Self);
  FVSync := vsmNoSync;
  FBuffer.ViewerBeforeRender := DoBeforeRender;
  FBuffer.OnChange := DoBufferChange;
  FBuffer.OnStructuralChange := DoBufferStructuralChange;
  inherited Create(AOwner);
end;

destructor TGLSceneForm.Destroy;
begin
  FBuffer.Free;
  inherited Destroy;
end;

// Notification
//

procedure TGLSceneForm.Notification(AComponent: TComponent; Operation: TOperation);
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

procedure TGLSceneForm.CreateWnd;
begin
  inherited CreateWnd;
  // initialize and activate the OpenGL rendering context
  // need to do this only once per window creation as we have a private DC
  FBuffer.Resize(Self.Width, Self.Height);
  FOwnDC := GetDC(Handle);
  FBuffer.CreateRC(FOwnDC, False);
end;

// DestroyWnd
//

procedure TGLSceneForm.DestroyWnd;
begin
  FBuffer.DestroyRC;
  if FOwnDC <> 0 then
  begin
    ReleaseDC(Handle, FOwnDC);
    FOwnDC := 0;
  end;
  inherited;
end;

// Loaded
//

procedure TGLSceneForm.Loaded;
begin
  inherited Loaded;
  // initiate window creation
  HandleNeeded;
end;

// WMEraseBkgnd
//

procedure TGLSceneForm.WMEraseBkgnd(var Message: TWMEraseBkgnd);
begin
  if GetIsRenderingContextAvailable then
    Message.Result := 1
  else
    inherited;
end;

// WMSize
//

procedure TGLSceneForm.WMSize(var Message: TWMSize);
begin
  inherited;
  FBuffer.Resize(Message.Width, Message.Height);
end;

// WMPaint
//

procedure TGLSceneForm.WMPaint(var Message: TWMPaint);
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

procedure TGLSceneForm.WMDestroy(var Message: TWMDestroy);
begin
  FBuffer.DestroyRC;
  if FOwnDC <> 0 then
  begin
    ReleaseDC(Handle, FOwnDC);
    FOwnDC := 0;
  end;
  inherited;
end;

// DoBeforeRender
//

procedure TGLSceneForm.DoBeforeRender(Sender: TObject);
begin
  SetupVSync(VSync);
end;

// DoBufferChange
//

procedure TGLSceneForm.DoBufferChange(Sender: TObject);
begin
  if (not Buffer.Rendering) and (not Buffer.Freezed) then
    Invalidate;
end;

// DoBufferStructuralChange
//

procedure TGLSceneForm.DoBufferStructuralChange(Sender: TObject);
begin
  {$IFNDEF FPC}
   RecreateWnd;
  {$ELSE}
   DestroyWnd;
   CreateWnd;
  {$ENDIF}
end;

// SetBeforeRender
//

procedure TGLSceneForm.SetBeforeRender(const val: TNotifyEvent);
begin
  FBuffer.BeforeRender := val;
end;

// GetBeforeRender
//

function TGLSceneForm.GetBeforeRender: TNotifyEvent;
begin
  Result := FBuffer.BeforeRender;
end;

// SetPostRender
//

procedure TGLSceneForm.SetPostRender(const val: TNotifyEvent);
begin
  FBuffer.PostRender := val;
end;

// GetPostRender
//

function TGLSceneForm.GetPostRender: TNotifyEvent;
begin
  Result := FBuffer.PostRender;
end;

// SetAfterRender
//

procedure TGLSceneForm.SetAfterRender(const val: TNotifyEvent);
begin
  FBuffer.AfterRender := val;
end;

// GetAfterRender
//

function TGLSceneForm.GetAfterRender: TNotifyEvent;
begin
  Result := FBuffer.AfterRender;
end;

// SetCamera
//

procedure TGLSceneForm.SetCamera(const val: TGLCamera);
begin
  FBuffer.Camera := val;
end;

// GetCamera
//

function TGLSceneForm.GetCamera: TGLCamera;
begin
  Result := FBuffer.Camera;
end;

// SetBuffer
//

procedure TGLSceneForm.SetBuffer(const val: TGLSceneBuffer);
begin
  FBuffer.Assign(val);
end;

// GetFieldOfView
//

function TGLSceneForm.GetFieldOfView: single;
begin
  if not Assigned(Camera) then
    result := 0
  else if Width < Height then
    result := Camera.GetFieldOfView(Width)
  else
    result := Camera.GetFieldOfView(Height);
end;

// SetFieldOfView
//

procedure TGLSceneForm.SetFieldOfView(const Value: single);
begin
  if Assigned(Camera) then
  begin
    if Width < Height then
      Camera.SetFieldOfView(Value, Width)
    else
      Camera.SetFieldOfView(Value, Height);
  end;
end;

// GetIsRenderingContextAvailable
//

function TGLSceneForm.GetIsRenderingContextAvailable: Boolean;
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

  RegisterClass(TGLSceneForm);

end.

