//
// VXScene Component Library, based on GLScene http://glscene.sourceforge.net
//
{
   SDL specific Context and Viewer.
   NOTA: SDL notifies use of context destruction *after* it happened, this prevents
         clean release of allocated stuff and requires a temporary switch to
         "ignore OpenVX errors" mode during destruction, thus potentially
         leaking memory (depending on hardware drivers willingness to perform
         automatic releases)
}
unit VXS.SDLContext;

interface

uses
  Winapi.Windows,
  System.Classes,
  System.SysUtils,

  VXS.OpenGL1x,
  VXS.CrossPlatform,
  VXS.XOpenGL,
  VXS.Context,
  VXS.SDLWindow,
  VXS.Scene;

type

  { A viewer using SDL.
     Beware: only one at a time, no other viewers allowed!
     Will also close the application when the window is closed! }
  TVXSDLViewer = class(TVXNonVisualViewer)
  private
    FCaption: string;
    FOnSDLEvent: TVXSDLEvent;
    FOnEventPollDone: TNotifyEvent;
    FOnResize: TNotifyEvent;
  protected
    procedure SetCaption(const val: string);
    procedure DoOnOpen(sender: TObject);
    procedure DoOnClose(sender: TObject);
    procedure DoOnResize(sender: TObject);
    procedure DoOnSDLEvent(sender: TObject; const event: TVXSDLEvent);
    procedure DoOnEventPollDone(sender: TObject);
    procedure DoBufferStructuralChange(Sender: TObject); override;
    procedure PrepareVXContext; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Render(baseObject: TVXBaseSceneObject = nil); override;
    function Active: Boolean;
  published
    property Caption: string read FCaption write SetCaption;
    property OnResize: TNotifyEvent read FOnResize write FOnResize;
    { Fired whenever an SDL Event is polled.
       SDL_QUITEV and SDL_VIDEORESIZE are not passed to this event handler,
       they are passed via OnClose and OnResize respectively. }
    property OnSDLEvent: TVXSDLEvent read FOnSDLEvent write FOnSDLEvent;
    { Fired whenever an event polling completes with no events left to poll. }
    property OnEventPollDone: TNotifyEvent read FOnEventPollDone write FOnEventPollDone;
  end;

  { A context driver for OpenVX via SDL (libsdl.org).
     Due to limitations of SDL:
      you may have only one SDL window opened at any time (you cannot
        have memory viewers)
      closing the SDL window will terminate the application }
  TVXSDLContext = class(TVXScreenControlingContext)
  private
    FSDLWin: TVXSDLWindow;
    FSimulatedValidity: Boolean; // Hack around SDL's post-notified destruction of context
  protected
    procedure DoCreateContext(outputDevice: THandle); override; //HDC
    procedure DoCreateMemoryContext(OutputDevice: THandle; Width, Height: // VCL ->HWND
      Integer; BufferCount: Integer = 1); override;
    function DoShareLists(aContext: TVXContext): Boolean; override;
    procedure DoDestroyContext; override;
    procedure DoActivate; override;
    procedure DoDeactivate; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    function IsValid: Boolean; override;
    procedure SwapBuffers; override;
    function RenderOutputDevice: Pointer; override;
    property SDLWindow: TVXSDLWindow read FSDLWin;
  end;

procedure Register;

// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------

procedure Register;
begin
  RegisterComponents('VXScene', [TVXSDLViewer]);
end;

// ------------------
// ------------------ TVXSDLViewer ------------------
// ------------------

constructor TVXSDLViewer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Width := 640;
  Height := 480;
end;

destructor TVXSDLViewer.Destroy;
begin
  inherited Destroy;
end;

procedure TVXSDLViewer.DoBufferStructuralChange(Sender: TObject);
begin
  // ignore that, supporting it with SDL is not very praticable as of now...
end;

procedure TVXSDLViewer.PrepareVXContext;
begin
  with Buffer.RenderingContext as TVXSDLContext do
  begin
    Width := Self.Width;
    Height := Self.Height;
    with FSDLWin do
    begin
      Caption := Self.Caption;
      OnOpen := DoOnOpen;
      OnClose := DoOnClose;
      OnResize := DoOnResize;
///?      OnSDLEvent := DoOnSDLEvent;
      OnEventPollDone := DoOnEventPollDone;
    end;
  end;
end;

procedure TVXSDLViewer.Render(baseObject: TVXBaseSceneObject = nil);
begin
  InitOpenGL;
  if Buffer.RenderingContext = nil then
  begin
    Buffer.CreateRC(0, False);
  end;
  Buffer.Render(baseObject);
end;

function TVXSDLViewer.Active: Boolean;
begin
  Result := Assigned(Buffer.RenderingContext) and Buffer.RenderingContext.IsValid;
end;

procedure TVXSDLViewer.SetCaption(const val: string);
begin
  if val <> FCaption then
  begin
    FCaption := val;
    if Buffer.RenderingContext <> nil then
      with Buffer.RenderingContext as TVXSDLContext do
        if Assigned(FSDLWin) then
          FSDLWin.Caption := FCaption;
  end;
end;

procedure TVXSDLViewer.DoOnOpen(sender: TObject);
begin
  // nothing yet
end;

procedure TVXSDLViewer.DoOnClose(sender: TObject);
begin
  // nothing yet
end;

procedure TVXSDLViewer.DoOnResize(sender: TObject);
begin
  with Buffer.RenderingContext as TVXSDLContext do
  begin
    Self.Width := FSDLWin.Width;
    Self.Height := FSDLWin.Height;
    Buffer.Resize(0, 0, FSDLWin.Width, FSDLWin.Height);
  end;
  if Assigned(FOnResize) then
    FOnResize(Self);
end;

procedure TVXSDLViewer.DoOnSDLEvent(sender: TObject; const event: TVXSDLEvent);
begin
{
  if Assigned(FOnSDLEvent) then
    FOnSDLEvent(sender, event);
}
end;

procedure TVXSDLViewer.DoOnEventPollDone(sender: TObject);
begin
  if Assigned(FOnEventPollDone) then
    FOnEventPollDone(sender);
end;

// ------------------
// ------------------ TVXSDLContext ------------------
// ------------------

constructor TVXSDLContext.Create;
begin
  inherited Create;
  FSDLWin := TVXSDLWindow.Create(nil);
end;

destructor TVXSDLContext.Destroy;
var
  oldIgnore: Boolean;
begin
  oldIgnore := vIgnoreOpenVXErrors;
  FSimulatedValidity := True;
  vIgnoreOpenVXErrors := True;
  try
    inherited Destroy;
  finally
    vIgnoreOpenVXErrors := oldIgnore;
    FSimulatedValidity := False;
  end;
  FreeAndNil(FSDLWin);
end;

procedure TVXSDLContext.DoCreateContext(outputDevice: THandle);
var
  sdlOpt: TVXSDLWindowOptions;
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

  sdlOpt := [voOpenGL];
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

  FVX.Initialize;
  MakeGLCurrent;
end;

procedure TVXSDLContext.DoCreateMemoryContext(OutputDevice: THandle; Width, Height: // VCL ->HWND
      Integer; BufferCount: Integer = 1);
begin
  raise Exception.Create(ClassName + ': Memory contexts not supported');
end;

function TVXSDLContext.DoShareLists(aContext: TVXContext): Boolean;
begin
  // nothing (only one context at all times... no need to share)
  Result := False;
end;

procedure TVXSDLContext.DoDestroyContext;
begin
  // Beware, SDL will also terminate the application
  FVX.Close;
  FSDLWin.Close;
end;

procedure TVXSDLContext.DoActivate;
begin
  if not FVX.IsInitialized then
    FVX.Initialize;
end;

procedure TVXSDLContext.DoDeactivate;
begin
  // nothing particular (only one context, always active)
end;

function TVXSDLContext.IsValid: Boolean;
begin
  Result := (Assigned(FSDLWin) and (FSDLWin.Active)) or FSimulatedValidity;
end;

procedure TVXSDLContext.SwapBuffers;
begin
  FSDLWin.SwapBuffers;
end;

function TVXSDLContext.RenderOutputDevice: Pointer;
begin
  // unsupported
  Result := nil;
end;

// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------

  RegisterClass(TVXSDLViewer);
  RegisterVXContextClass(TVXSDLContext);

end.

