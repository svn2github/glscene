// GLWin32Viewer
{: Win32 specific Context.<p>

	<b>History : </b><font size=-1><ul>
      <li>28/12/01 - EG - Event persistence change (GliGli / Dephi bug)
	   <li>12/12/01 - EG - Creation (split from GLScene.pas)
	</ul></font>
}
unit GLLinuxViewer;

interface

{$i GLScene.inc}

{$IFNDEF LINUX} {$MESSAGE Error 'Unit is Linux specific'} {$ENDIF}

uses
  Classes, GLScene, QControls, Qt;

type

   // TVSyncMode
   //
   TVSyncMode = (vsmSync, vsmNoSync);

   // TGLLinuxSceneViewer
   //
   {: Component where the GLScene objects get rendered.<p>
      This component delimits the area where OpenGL renders the scene,
      it represents the 3D scene viewed from a camera (specified in the
      camera property). This component can also render to a file or to a bitmap.<p>
      This viewer also allows to define rendering options such a fog, face culling,
      depth testing, etc. and can take care of framerate calculation.<p> }
   TGLLinuxSceneViewer = class(TWidgetControl)
      private
         { Private Declarations }
         FIsOpenGLAvailable : Boolean;
         FBuffer : TGLSceneBuffer;
         FBeforeRender : TNotifyEvent;
         FVSync : TVSyncMode;
      protected
         { Protected Declarations }
         procedure SetPostRender(const val : TNotifyEvent);
         function GetPostRender : TNotifyEvent;
         procedure SetAfterRender(const val : TNotifyEvent);
         function GetAfterRender : TNotifyEvent;
         procedure SetCamera(const val : TGLCamera);
         function GetCamera : TGLCamera;
         procedure SetBuffer(const val : TGLSceneBuffer);

         function WidgetFlags: Integer; override;

         procedure CreateWidget; override;
         procedure DestroyWidget; override;
         procedure Resize; override;
         procedure Loaded; override;

         procedure DoBeforeRender(Sender : TObject); dynamic;
         procedure DoBufferChange(Sender : TObject); virtual;
         procedure DoBufferStructuralChange(Sender : TObject); dynamic;

         procedure Painting(Sender: QObjectH; EventRegion: QRegionH); override;
      public
         { Public Declarations }
         constructor Create(AOwner: TComponent); override;
         destructor  Destroy; override;

         procedure Notification(AComponent: TComponent; Operation: TOperation); override;

         property IsOpenGLAvailable : Boolean read FIsOpenGLAvailable;

         function FramesPerSecond : Single;
         procedure ResetPerformanceMonitor;
         
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
         property BeforeRender : TNotifyEvent read FBeforeRender write FBeforeRender;
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

         property Align;
         property Anchors;
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

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

uses
  OpenGL12, SysUtils, GLLinuxContext;

// ------------------
// ------------------ TGLLinuxSceneViewer ------------------
// ------------------

// Create
//
constructor TGLLinuxSceneViewer.Create(AOwner: TComponent);
begin
   FIsOpenGLAvailable:=InitOpenGL;
   inherited Create(AOwner);
   ControlStyle:=[csClickEvents, csDoubleClicks, csOpaque, csCaptureMouse];
   if csDesigning in ComponentState then ControlStyle:=ControlStyle + [csFramed];
   Width:=100;
   Height:=100;
   FVSync:=vsmNoSync;
   FBuffer:=TGLSceneBuffer.Create(Self);
   FBuffer.BeforeRender:=DoBeforeRender;
   FBuffer.OnChange:=DoBufferChange;
   FBuffer.OnStructuralChange:=DoBufferStructuralChange;
end;

// Destroy
//
destructor TGLLinuxSceneViewer.Destroy;
begin
   FBuffer.Free;
   inherited Destroy;
end;

// Notification
//
procedure TGLLinuxSceneViewer.Notification(AComponent: TComponent; Operation: TOperation);
begin
   inherited;
   if (Operation = opRemove) and (AComponent = Camera) then
      Camera:=nil;
end;

// SetPostRender
//
procedure TGLLinuxSceneViewer.SetPostRender(const val : TNotifyEvent);
begin
   FBuffer.PostRender:=val;
end;

// GetPostRender
//
function TGLLinuxSceneViewer.GetPostRender : TNotifyEvent;
begin
   Result:=FBuffer.PostRender;
end;

// SetAfterRender
//
procedure TGLLinuxSceneViewer.SetAfterRender(const val : TNotifyEvent);
begin
   FBuffer.AfterRender:=val;
end;

// GetAfterRender
//
function TGLLinuxSceneViewer.GetAfterRender : TNotifyEvent;
begin
   Result:=FBuffer.AfterRender;
end;

// SetCamera
//
procedure TGLLinuxSceneViewer.SetCamera(const val : TGLCamera);
begin
   FBuffer.Camera:=val;
end;

// GetCamera
//
function TGLLinuxSceneViewer.GetCamera : TGLCamera;
begin
   Result:=FBuffer.Camera;
end;

// SetBuffer
//
procedure TGLLinuxSceneViewer.SetBuffer(const val : TGLSceneBuffer);
begin
   FBuffer.Assign(val);
end;

// Loaded
//
procedure TGLLinuxSceneViewer.Loaded;
begin
   inherited Loaded;
   // initiate window creation
   HandleNeeded;
end;

// DoBeforeRender
//
procedure TGLLinuxSceneViewer.DoBeforeRender(Sender : TObject);
//var
//   i : Integer;
begin
(*   if WGL_EXT_swap_control then begin
      i:=wglGetSwapIntervalEXT;
      case VSync of
         vsmSync    : if i<>1 then wglSwapIntervalEXT(1);
         vsmNoSync  : if i<>0 then wglSwapIntervalEXT(0);
      else
         Assert(False);
      end;
   end;*)
   if Assigned(FBeforeRender) and (not (csDesigning in ComponentState)) then
      FBeforeRender(Self);
end;

// DoBufferChange
//
procedure TGLLinuxSceneViewer.DoBufferChange(Sender : TObject);
begin
   Invalidate;
end;

// DoBufferStructuralChange
//
procedure TGLLinuxSceneViewer.DoBufferStructuralChange(Sender : TObject);
begin
  RecreateWidget;
end;

// FramesPerSecond
//
function TGLLinuxSceneViewer.FramesPerSecond : Single;
begin
   Result:=FBuffer.FramesPerSecond;
end;

// ResetPerformanceMonitor
//
procedure TGLLinuxSceneViewer.ResetPerformanceMonitor;
begin
   FBuffer.ResetPerformanceMonitor;
end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
procedure TGLLinuxSceneViewer.CreateWidget;
begin
  inherited;
   if IsOpenGLAvailable then begin
      // initialize and activate the OpenGL rendering context
      // need to do this only once per window creation as we have a private DC
      FBuffer.Resize(Self.Width, Self.Height);
      FBuffer.CreateRC(Integer(Handle), False);
   end;
end;

procedure TGLLinuxSceneViewer.DestroyWidget;
begin
  FBuffer.DestroyRC;
  inherited;
end;

procedure TGLLinuxSceneViewer.Resize;
begin
  inherited;
  FBuffer.Resize(Width, Height);
end;

function TGLLinuxSceneViewer.WidgetFlags: Integer;
begin
  Result := inherited WidgetFlags or Integer(WidgetFlags_WRepaintNoErase);
end;

procedure TGLLinuxSceneViewer.Painting(Sender: QObjectH; EventRegion: QRegionH);
begin
  if IsOpenGLAvailable then
  begin
    // Don't call render during design time (probably sync problem).
    if not (csDesigning in ComponentState) then
      FBuffer.Render;
  end
  else
    inherited;
end;

initialization
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

   RegisterClasses([TGLLinuxSceneViewer]);

end.

