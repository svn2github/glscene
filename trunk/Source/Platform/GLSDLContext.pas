{: GLSDLContext<p>

   SDL specific Context and Viewer.<p>

   <b>History : </b><font size=-1><ul>
      <li>12/12/01 - EG - Creation
   </ul></font>
}
unit GLSDLContext;

interface

uses Classes, SysUtils, GLContext, SDLWindow, GLScene, SDL;

type

   // TGLSDLViewer
   //
   {: A viewer using SDL.<p>
      Beware: only one at a time, no other viewers allowed!<br>
      Will also close the application when the window is closed! }
   TGLSDLViewer = class (TGLNonVisualViewer)
      private
         { Private Declarations }
         FOnSDLEvent : TSDLEvent;
         FOnEventPollDone : TNotifyEvent;

      protected
         { Protected Declarations }
         procedure DoOnOpen(sender : TObject);
         procedure DoOnClose(sender : TObject);
         procedure DoOnResize(sender : TObject);
         procedure DoOnSDLEvent(sender : TObject; const event : TSDL_Event);
         procedure DoOnEventPollDone(sender : TObject);

         procedure PrepareGLContext; override;

      public
         { Public Declarations }
         constructor Create(AOwner: TComponent); override;
         destructor  Destroy; override;

         procedure Render; override;

      published
         { Public Declarations }
         {: Fired whenever an SDL Event is polled.<p>
            SDL_QUITEV and SDL_VIDEORESIZE are not passed to this event handler,
            they are passed via OnClose and OnResize respectively. }
         property OnSDLEvent : TSDLEvent read FOnSDLEvent write FOnSDLEvent;
         {: Fired whenever an event polling completes with no events left to poll. }
         property OnEventPollDone : TNotifyEvent read FOnEventPollDone write FOnEventPollDone;
   end;

   // TGLSDLContext
   //
   {: A context driver for OpenGL via SDL (libsdl.org).<p>
      Due to limitations of SDL:<ul>
      <li>you may have only one SDL window opened at any time (you cannot
         have memory viewers)
      <li>closing the SDL window will terminate the application
      </ul> }
   TGLSDLContext = class (TGLScreenControlingContext)
      private
         { Private Declarations }
         FSDLWin : TSDLWindow;

      protected
         { Protected Declarations }
         procedure DoCreateContext(outputDevice : Integer); override;
         procedure DoCreateMemoryContext(outputDevice, width, height : Integer); override;
         procedure DoShareLists(aContext : TGLContext); override;
         procedure DoDestroyContext; override;
         procedure DoActivate; override;
         procedure DoDeactivate; override;

      public
         { Public Declarations }
         constructor Create; override;
         destructor Destroy; override;

         function IsValid : Boolean; override;
         procedure SwapBuffers; override;
   end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

uses OpenGL12, GLCrossPlatform;

// ------------------
// ------------------ TGLSDLViewer ------------------
// ------------------

// Create
//
constructor TGLSDLViewer.Create(AOwner: TComponent);
begin
   inherited Create(AOwner);
   Width:=640;
   Height:=480;
end;

// Destroy
//
destructor TGLSDLViewer.Destroy;
begin
   inherited Destroy;
end;

// PrepareGLContext
//
procedure TGLSDLViewer.PrepareGLContext;
begin
   with Buffer.RenderingContext as TGLSDLContext do begin
      Width:=Self.Width;
      Height:=Self.Height;
      with FSDLWin do begin
         OnOpen:=DoOnOpen;
         OnClose:=DoOnClose;
         OnResize:=DoOnResize;
         OnSDLEvent:=DoOnSDLEvent;
         OnEventPollDone:=DoOnEventPollDone;
      end;
   end;
end;

// Render
//
procedure TGLSDLViewer.Render;
begin
   if Buffer.RenderingContext=nil then begin
      Buffer.CreateRC(0, False);
   end;
   Buffer.Render;
end;

// DoOnOpen
//
procedure TGLSDLViewer.DoOnOpen(sender : TObject);
begin
   // nothing yet
end;

// DoOnClose
//
procedure TGLSDLViewer.DoOnClose(sender : TObject);
begin
   // nothing yet
end;

// DoOnResize
//
procedure TGLSDLViewer.DoOnResize(sender : TObject);
begin
   // nothing yet
end;

// DoOnSDLEvent
//
procedure TGLSDLViewer.DoOnSDLEvent(sender : TObject; const event : TSDL_Event);
begin
   if Assigned(FOnSDLEvent) then
      FOnSDLEvent(sender, event);
end;

// DoOnEventPollDone
//
procedure TGLSDLViewer.DoOnEventPollDone(sender : TObject);
begin
   if Assigned(FOnEventPollDone) then
      FOnEventPollDone(sender);
end;

// ------------------
// ------------------ TGLSDLContext ------------------
// ------------------

// Create
//
constructor TGLSDLContext.Create;
begin
   inherited Create;
   FSDLWin:=TSDLWindow.Create(nil);
end;

// Destroy
//
destructor TGLSDLContext.Destroy;
begin
   FSDLWin.Free;
   inherited Destroy;
end;

// DoCreateContext
//
procedure TGLSDLContext.DoCreateContext(outputDevice : Integer);
const
   cBoolToInt : array [False..True] of Integer = (GL_FALSE, GL_TRUE);
var
   sdlOpt : TSDLWindowOptions;
begin
   // Just in case it didn't happen already.
   if not InitOpenGL then RaiseLastOSError;

   FSDLWin.Width:=Width;
   FSDLWin.Height:=Height;
   if ColorBits>16 then
      FSDLWin.PixelDepth:=vpd24bits
   else FSDLWin.PixelDepth:=vpd16bits;

   sdlOpt:=[voOpenGL, voHardwareAccel];
   if FullScreen then
      sdlOpt:=sdlOpt+[voFullScreen]
   else sdlOpt:=sdlOpt+[voResizable];
   if rcoDoubleBuffered in Options then
      sdlOpt:=sdlOpt+[voDoubleBuffer];
   if StencilBits>0 then
      sdlOpt:=sdlOpt+[voStencilBuffer];

   FSDLWin.Open;
   if not FSDLWin.Active then
      raise Exception.Create('SDLWindow open failed.');
end;

// DoCreateMemoryContext
//
procedure TGLSDLContext.DoCreateMemoryContext(outputDevice, width, height : Integer);
begin
   raise Exception.Create(ClassName+': Memory contexts not supported');
end;

// DoShareLists
//
procedure TGLSDLContext.DoShareLists(aContext : TGLContext);
begin
   // nothing (only one context at all times... no need to share)
end;

// DoDestroyContext
//
procedure TGLSDLContext.DoDestroyContext;
begin
   // Beware, SDL will also terminate the application
   FSDLWin.Close;
end;

// DoActivate
//
procedure TGLSDLContext.DoActivate;
begin
   // nothing (only one context, always active)
end;

// Deactivate
//
procedure TGLSDLContext.DoDeactivate;
begin
   // ignore (only one context, always active)
end;

// IsValid
//
function TGLSDLContext.IsValid : Boolean;
begin
   Result:=FSDLWin.Active;
end;

// SwapBuffers
//
procedure TGLSDLContext.SwapBuffers;
begin
   FSDLWin.SwapBuffers;
end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

   RegisterGLContextClass(TGLSDLContext);

end.
