//
// GLScene on Vulkan, http://glscene.sourceforge.net 
//
{
   SDL specific Context and Viewer. 
   NOTA: SDL notifies use of context destruction *after* it happened, this prevents
         clean release of allocated stuff and requires a temporary switch to
         "ignore OpenGL errors" mode during destruction, thus potentially
         leaking memory (depending on hardware drivers willingness to perform
         automatic releases) 
}
unit GLS.SDLContext;

interface

uses
{$IFDEF MSWINDOWS}
  Winapi.Windows,
{$ENDIF}
  System.Classes,
  System.SysUtils,
  GLS.Context,
  GLS.SDLWindow,
  GLS.Scene,
  GLS.SDL;

type

  // TVKSDLViewer
  //
  { A viewer using SDL. 
     Beware: only one at a time, no other viewers allowed! 
     Will also close the application when the window is closed! }
  TVKSDLViewer = class(TVKNonVisualViewer)
  private
    { Private Declarations }
    FCaption: string;
    FOnSDLEvent: TSDLEvent;
    FOnEventPollDone: TNotifyEvent;
    FOnResize: TNotifyEvent;

  protected
    { Protected Declarations }
    procedure SetCaption(const val: string);

    procedure DoOnOpen(sender: TObject);
    procedure DoOnClose(sender: TObject);
    procedure DoOnResize(sender: TObject);
    procedure DoOnSDLEvent(sender: TObject; const event: TSDL_Event);
    procedure DoOnEventPollDone(sender: TObject);

    procedure DoBufferStructuralChange(Sender: TObject); override;
    procedure PrepareGLContext; override;

  public
    { Public Declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Render(baseObject: TVKBaseSceneObject = nil); override;

    function Active: Boolean;

  published
    { Public Declarations }
    property Caption: string read FCaption write SetCaption;

    property OnResize: TNotifyEvent read FOnResize write FOnResize;

    { Fired whenever an SDL Event is polled. 
       SDL_QUITEV and SDL_VIDEORESIZE are not passed to this event handler,
       they are passed via OnClose and OnResize respectively. }
    property OnSDLEvent: TSDLEvent read FOnSDLEvent write FOnSDLEvent;
    { Fired whenever an event polling completes with no events left to poll. }
    property OnEventPollDone: TNotifyEvent read FOnEventPollDone write FOnEventPollDone;
  end;

  // TVKSDLContext
  //
  { A context driver for OpenGL via SDL (libsdl.org). 
     Due to limitations of SDL: 
      you may have only one SDL window opened at any time (you cannot
        have memory viewers)
      closing the SDL window will terminate the application
       }
  TVKSDLContext = class(TVKScreenControlingContext)
  private
    { Private Declarations }
    FSDLWin: TSDLWindow;
    FSimulatedValidity: Boolean; // Hack around SDL's post-notified destruction of context

  protected
    { Protected Declarations }
    procedure DoCreateContext(outputDevice: HDC); override;
    procedure DoCreateMemoryContext(outputDevice: HWND; width, height: Integer; BufferCount: integer); override;
    function DoShareLists(aContext: TVKContext): Boolean; override;
    procedure DoDestroyContext; override;
    procedure DoActivate; override;
    procedure DoDeactivate; override;

  public
    { Public Declarations }
    constructor Create; override;
    destructor Destroy; override;

    function IsValid: Boolean; override;
    procedure SwapBuffers; override;

    function RenderOutputDevice: Pointer; override;

    property SDLWindow: TSDLWindow read FSDLWin;
  end;

procedure Register;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

uses
  GLS.OpenGLAdapter,
  GLS.CrossPlatform,
  GLS.XOpenGL;

procedure Register;
begin
  RegisterComponents('GLScene', [TVKSDLViewer]);
end;

// ------------------
// ------------------ TVKSDLViewer ------------------
// ------------------

// Create
//

constructor TVKSDLViewer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Width := 640;
  Height := 480;
end;

// Destroy
//

destructor TVKSDLViewer.Destroy;
begin
  inherited Destroy;
end;

// DoBufferStructuralChange
//

procedure TVKSDLViewer.DoBufferStructuralChange(Sender: TObject);
begin
  // ignore that, supporting it with SDL is not very praticable as of now...
end;

// PrepareGLContext
//

procedure TVKSDLViewer.PrepareGLContext;
begin
  with Buffer.RenderingContext as TVKSDLContext do
  begin
    Width := Self.Width;
    Height := Self.Height;
    with FSDLWin do
    begin
      Caption := Self.Caption;
      OnOpen := DoOnOpen;
      OnClose := DoOnClose;
      OnResize := DoOnResize;
      OnSDLEvent := DoOnSDLEvent;
      OnEventPollDone := DoOnEventPollDone;
    end;
  end;
end;

// Render
//

procedure TVKSDLViewer.Render(baseObject: TVKBaseSceneObject = nil);
begin
  LoadOpenGL;
  if Buffer.RenderingContext = nil then
  begin
    Buffer.CreateRC(0, False);
  end;
  Buffer.Render(baseObject);
end;

// Active
//

function TVKSDLViewer.Active: Boolean;
begin
  Result := Assigned(Buffer.RenderingContext) and Buffer.RenderingContext.IsValid;
end;

// SetCaption
//

procedure TVKSDLViewer.SetCaption(const val: string);
begin
  if val <> FCaption then
  begin
    FCaption := val;
    if Buffer.RenderingContext <> nil then
      with Buffer.RenderingContext as TVKSDLContext do
        if Assigned(FSDLWin) then
          FSDLWin.Caption := FCaption;
  end;
end;

// DoOnOpen
//

procedure TVKSDLViewer.DoOnOpen(sender: TObject);
begin
  // nothing yet
end;

// DoOnClose
//

procedure TVKSDLViewer.DoOnClose(sender: TObject);
begin
  // nothing yet
end;

// DoOnResize
//

procedure TVKSDLViewer.DoOnResize(sender: TObject);
begin
  with Buffer.RenderingContext as TVKSDLContext do
  begin
    Self.Width := FSDLWin.Width;
    Self.Height := FSDLWin.Height;
    Buffer.Resize(0, 0, FSDLWin.Width, FSDLWin.Height);
  end;
  if Assigned(FOnResize) then
    FOnResize(Self);
end;

// DoOnSDLEvent
//

procedure TVKSDLViewer.DoOnSDLEvent(sender: TObject; const event: TSDL_Event);
begin
  if Assigned(FOnSDLEvent) then
    FOnSDLEvent(sender, event);
end;

// DoOnEventPollDone
//

procedure TVKSDLViewer.DoOnEventPollDone(sender: TObject);
begin
  if Assigned(FOnEventPollDone) then
    FOnEventPollDone(sender);
end;

// ------------------
// ------------------ TVKSDLContext ------------------
// ------------------

// Create
//

constructor TVKSDLContext.Create;
begin
  inherited Create;
  FSDLWin := TSDLWindow.Create(nil);
end;

// Destroy
//

destructor TVKSDLContext.Destroy;
var
  oldIgnore: Boolean;
begin
  oldIgnore := vIgnoreOpenGLErrors;
  FSimulatedValidity := True;
  vIgnoreOpenGLErrors := True;
  try
    inherited Destroy;
  finally
    vIgnoreOpenGLErrors := oldIgnore;
    FSimulatedValidity := False;
  end;
  FreeAndNil(FSDLWin);
end;

// DoCreateContext
//

procedure TVKSDLContext.DoCreateContext(outputDevice: HDC);
var
  sdlOpt: TSDLWindowOptions;
begin
  // Just in case it didn't happen already.
  if not InitOpenGL then
    RaiseLastOSError;

  FSDLWin.Width := Width;
  FSDLWin.Height := Height;
  if ColorBits > 16 then
    FSDLWin.PixelDepth := vpd24bits
  else
    FSDLWin.PixelDepth := vpd16bits;

  sdlOpt := [voOpenGL, voHardwareAccel];
  if FullScreen then
    sdlOpt := sdlOpt + [voFullScreen]
  else
    sdlOpt := sdlOpt + [voResizable];
  if rcoDoubleBuffered in Options then
    sdlOpt := sdlOpt + [voDoubleBuffer];
  if StencilBits > 0 then
    sdlOpt := sdlOpt + [voStencilBuffer];

  FSDLWin.Open;
  if not FSDLWin.Active then
    raise Exception.Create('SDLWindow open failed.');

  FglInitialize;
  MakeGLCurrent;
end;

// DoCreateMemoryContext
//

procedure TVKSDLContext.DoCreateMemoryContext(outputDevice: HWND; width, height: Integer; BufferCount: integer);
begin
  raise Exception.Create(ClassName + ': Memory contexts not supported');
end;

// DoShareLists
//

function TVKSDLContext.DoShareLists(aContext: TVKContext): Boolean;
begin
  // nothing (only one context at all times... no need to share)
  Result := False;
end;

// DoDestroyContext
//

procedure TVKSDLContext.DoDestroyContext;
begin
  // Beware, SDL will also terminate the application
  FGL.Close;
  FSDLWin.Close;
end;

// DoActivate
//

procedure TVKSDLContext.DoActivate;
begin
  if not FGL.IsInitialized then
    FglInitialize;
end;

// Deactivate
//

procedure TVKSDLContext.DoDeactivate;
begin
  // nothing particular (only one context, always active)
end;

// IsValid
//

function TVKSDLContext.IsValid: Boolean;
begin
  Result := (Assigned(FSDLWin) and (FSDLWin.Active)) or FSimulatedValidity;
end;

// SwapBuffers
//

procedure TVKSDLContext.SwapBuffers;
begin
  FSDLWin.SwapBuffers;
end;

// RenderOutputDevice
//

function TVKSDLContext.RenderOutputDevice: Pointer;
begin
  // unsupported
  Result := nil;
end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization
  // ------------------------------------------------------------------
  // ------------------------------------------------------------------
  // ------------------------------------------------------------------

  RegisterClass(TVKSDLViewer);
  RegisterGLContextClass(TVKSDLContext);

end.

