//
// This unit is part of the GLScene Project, http://glscene.org
//
{: GLWin32Context<p>

   Win32 specific Context.<p>

   <b>History : </b><font size=-1><ul>
      <li>23/08/10 - Yar - Replaced OpenGL1x to OpenGLTokens. Improved context creation.
      <li>18/06/10 - Yar - Changed context sharing method for similarity to GLX
      <li>06/06/10 - Yar - Moved forward context creation to DoCreateContext
                           make outputDevice HWND type
      <li>19/05/10 - Yar - Added choice between hardware and software acceleration
      <li>06/05/10 - Yar - Added vLastVendor clearing when multithreading is enabled
      <li>06/04/10 - Yar - Added DoGetHandles to TGLWin32Context (thanks Rustam Asmandiarov aka Predator)
      <li>28/03/10 - Yar - Added 3.3 forward context creation and eliminate memory leaks when multithreading
      <li>06/03/10 - Yar - Added forward context creation in TGLWin32Context.DoActivate
      <li>20/02/10 - DanB - Allow double-buffered memory viewers, if you want single
                            buffered, or no swapping, then change buffer options instead.
                            Some changes from Cardinal to the appropriate HDC /HGLRC type.
      <li>15/01/10 - DaStr - Bugfixed TGLWin32Context.ChooseWGLFormat()
                             (BugtrackerID = 2933081) (thanks YarUndeoaker)
      <li>08/01/10 - DaStr - Added more AntiAliasing modes (thanks YarUndeoaker)
      <li>13/12/09 - DaStr - Modified for multithread support (thanks Controller)
      <li>30/08/09 - DanB - vIgnoreContextActivationFailures renamed to
                            vContextActivationFailureOccurred + check removed.
      <li>06/11/07 - mrqzzz - Ignore ContextActivation failure
                   if GLContext.vIgnoreContextActivationFailures=true
      <li>15/02/07 - DaStr - Integer -> Cardinal because $R- was removed in GLScene.pas
      <li>11/09/06 - NC - Added support for Multiple-Render-Target
      <li>03/10/04 - NC - Added float texture support
      <li>03/07/02 - EG - ChooseWGLFormat Kyro fix (Patrick Chevalley)
      <li>13/03/02 - EG - aaDefault now prefers non-AA when possible
      <li>03/03/02 - EG - Fixed aaNone mode (AA specifically off)
      <li>01/03/02 - EG - Fixed CurrentPixelFormatIsHardwareAccelerated
      <li>22/02/02 - EG - Unified ChooseWGLFormat for visual & non-visual
      <li>21/02/02 - EG - AntiAliasing support *experimental* (Chris N. Strahm)
      <li>05/02/02 - EG - Fixed UnTrackWindow
      <li>03/02/02 - EG - Added experimental Hook-based window tracking
      <li>29/01/02 - EG - Improved recovery for ICDs without pbuffer  support
      <li>21/01/02 - EG - More graceful recovery for ICDs without pbuffer support
      <li>07/01/02 - EG - DoCreateMemoryContext now retrieved topDC when needed
      <li>15/12/01 - EG - Added support for AlphaBits
      <li>30/11/01 - EG - Hardware acceleration support now detected
      <li>20/11/01 - EG - New temp HWnd code for memory contexts (improved compat.)
      <li>04/09/01 - EG - Added ChangeIAttrib, support for 16bits depth buffer
      <li>25/08/01 - EG - Added pbuffer support and CreateMemoryContext interface
      <li>24/08/01 - EG - Fixed PropagateSharedContext
      <li>12/08/01 - EG - Handles management completed
      <li>22/07/01 - EG - Creation (glcontext.omm)
   </ul></font>
}
unit GLWin32Context;

interface

{$I GLScene.inc}

{$IFNDEF MSWINDOWS}{$MESSAGE Error 'Unit is Windows specific'}{$ENDIF}

uses
  Windows,
  Classes,
  SysUtils,
  OpenGLTokens,
  OpenGLAdapter,
  GLContext;

type

  // TGLWin32Context
  //
  {: A context driver for standard Windows OpenGL (via MS OpenGL). }
  TGLWin32Context = class(TGLContext)
  private
    { Private Declarations }
    FDC: HDC;
    FRC, FShareContext: HGLRC;
    FHPBUFFER: Integer;
    FiAttribs: packed array of Integer;
    FfAttribs: packed array of Single;
    FLegacyContextsOnly: Boolean;

    procedure SpawnLegacyContext(aDC: HDC); // used for WGL_pixel_format soup
    procedure CreateOldContext(aDC: HDC);
    procedure CreateNewContext(aDC: HDC);
  protected
    { Protected Declarations }
    procedure ClearIAttribs;
    procedure AddIAttrib(attrib, value: Integer);
    procedure ChangeIAttrib(attrib, newValue: Integer);
    procedure DropIAttrib(attrib: Integer);
    procedure ClearFAttribs;
    procedure AddFAttrib(attrib, value: Single);

    procedure DestructionEarlyWarning(sender: TObject);

    procedure ChooseWGLFormat(DC: HDC; nMaxFormats: Cardinal; piFormats:
      PInteger;
      var nNumFormats: Integer; BufferCount: integer = 1);
    procedure DoCreateContext(outputDevice: HWND); override;
    procedure DoCreateMemoryContext(outputDevice: HWND; width, height:
      Integer; BufferCount: integer); override;
    procedure DoShareLists(aContext: TGLContext); override;
    procedure DoDestroyContext; override;
    procedure DoActivate; override;
    procedure DoDeactivate; override;
    {: DoGetHandles must be implemented in child classes,
       and return the display + window }
{$IFDEF FPC}
    procedure DoGetHandles(outputDevice: Cardinal; out XWin: Cardinal); virtual;
      abstract;
{$ENDIF}
  public
    { Public Declarations }
    constructor Create; override;
    destructor Destroy; override;

    function IsValid: Boolean; override;
    procedure SwapBuffers; override;

    function RenderOutputDevice: Integer; override;

    property DC: HDC read FDC;
    property RC: HGLRC read FRC;
  end;

function CreateTempWnd: HWND;

var
  { This boolean controls a hook-based tracking of top-level forms destruction,
    with the purpose of being able to properly release OpenGL contexts before
    they are (improperly) released by some drivers upon top-level form
    destruction. }
  vUseWindowTrackingHook: Boolean = True;

  // ------------------------------------------------------------------
  // ------------------------------------------------------------------
  // ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

uses
  Forms,
  Messages,
  GLCrossPlatform,
  GLState,
  GLSLog;

resourcestring
  cForwardContextFailed = 'Can not create OpenGL 3.x Forward Context';
  cFailHWRC = 'Unable to create rendering context with hardware accelerati' +
    'on - down to software';

var
  vTrackingCount: Integer;
  vTrackedHwnd: array of HWND;
  vTrackedEvents: array of TNotifyEvent;
  vTrackingHook: HHOOK;

  // TrackHookProc
  //

function TrackHookProc(nCode: Integer; wParam: wParam; lParam: LPARAM): Integer;
  stdcall;
var
  i: Integer;
  p: PCWPStruct;
begin
  if nCode = HC_ACTION then
  begin
    p := PCWPStruct(lParam);
    //   if (p.message=WM_DESTROY) or (p.message=WM_CLOSE) then begin // destroy & close variant
    if p.message = WM_DESTROY then
    begin
      // special care must be taken by this loop, items may go away unexpectedly
      i := vTrackingCount - 1;
      while i >= 0 do
      begin
        if IsChild(p.hwnd, vTrackedHwnd[i]) then
        begin
          // got one, send notification
          vTrackedEvents[i](nil);
        end;
        Dec(i);
        while i >= vTrackingCount do
          Dec(i);
      end;
    end;
    CallNextHookEx(vTrackingHook, nCode, wParam, lParam);
    Result := 0;
  end
  else
    Result := CallNextHookEx(vTrackingHook, nCode, wParam, lParam);
end;

// TrackWindow
//

procedure TrackWindow(h: HWND; notifyEvent: TNotifyEvent);
begin
  if not IsWindow(h) then
    Exit;
  if vTrackingCount = 0 then
    vTrackingHook := SetWindowsHookEx(WH_CALLWNDPROC, @TrackHookProc, 0,
      GetCurrentThreadID);
  Inc(vTrackingCount);
  SetLength(vTrackedHwnd, vTrackingCount);
  vTrackedHwnd[vTrackingCount - 1] := h;
  SetLength(vTrackedEvents, vTrackingCount);
  vTrackedEvents[vTrackingCount - 1] := notifyEvent;
end;

// UnTrackWindows
//

procedure UnTrackWindow(h: HWND);
var
  i, k: Integer;
begin
  if not IsWindow(h) then
    Exit;
  if vTrackingCount = 0 then
    Exit;
  k := 0;
  for i := 0 to vTrackingCount - 1 do
    if vTrackedHwnd[i] <> h then
    begin
      vTrackedHwnd[k] := vTrackedHwnd[i];
      vTrackedEvents[k] := vTrackedEvents[i];
      Inc(k);
    end;
  Dec(vTrackingCount);
  SetLength(vTrackedHwnd, vTrackingCount);
  SetLength(vTrackedEvents, vTrackingCount);
  if vTrackingCount = 0 then
    UnhookWindowsHookEx(vTrackingHook);
end;

var
  vUtilWindowClass: TWndClass = (
    style: 0;
    lpfnWndProc: @DefWindowProc;
    cbClsExtra: 0;
    cbWndExtra: 0;
    hInstance: 0;
    hIcon: 0;
    hCursor: 0;
    hbrBackground: 0;
    lpszMenuName: nil;
    lpszClassName: 'GLSUtilWindow');

  // CreateTempWnd
  //

function CreateTempWnd: HWND;
var
  classRegistered: Boolean;
  tempClass: TWndClass;
begin
  vUtilWindowClass.hInstance := HInstance;
  classRegistered := GetClassInfo(HInstance, vUtilWindowClass.lpszClassName,
    tempClass);
  if not classRegistered then
    Windows.RegisterClass(vUtilWindowClass);
  Result := CreateWindowEx(WS_EX_TOOLWINDOW, vUtilWindowClass.lpszClassName,
    '', WS_POPUP, 0, 0, 0, 0, 0, 0, HInstance, nil);
end;

// ------------------
// ------------------ TGLWin32Context ------------------
// ------------------

  // Create
  //

constructor TGLWin32Context.Create;
begin
  inherited Create;
  ClearIAttribs;
  ClearFAttribs;
end;

// Destroy
//

destructor TGLWin32Context.Destroy;
begin
  inherited Destroy;
end;

// SetupPalette
//

function SetupPalette(DC: HDC; PFD: TPixelFormatDescriptor): HPalette;
var
  nColors, I: Integer;
  LogPalette: TMaxLogPalette;
  RedMask, GreenMask, BlueMask: Byte;
begin
  nColors := 1 shl Pfd.cColorBits;
  LogPalette.palVersion := $300;
  LogPalette.palNumEntries := nColors;
  RedMask := (1 shl Pfd.cRedBits) - 1;
  GreenMask := (1 shl Pfd.cGreenBits) - 1;
  BlueMask := (1 shl Pfd.cBlueBits) - 1;
  with LogPalette, PFD do
    for I := 0 to nColors - 1 do
    begin
      palPalEntry[I].peRed := (((I shr cRedShift) and RedMask) * 255) div
        RedMask;
      palPalEntry[I].peGreen := (((I shr cGreenShift) and GreenMask) * 255) div
        GreenMask;
      palPalEntry[I].peBlue := (((I shr cBlueShift) and BlueMask) * 255) div
        BlueMask;
      palPalEntry[I].peFlags := 0;
    end;

  Result := CreatePalette(PLogPalette(@LogPalette)^);
  if Result <> 0 then
  begin
    SelectPalette(DC, Result, False);
    RealizePalette(DC);
  end
  else
    RaiseLastOSError;
end;

// ClearIAttribs
//

procedure TGLWin32Context.ClearIAttribs;
begin
  SetLength(FiAttribs, 1);
  FiAttribs[0] := 0;
end;

// AddIAttrib
//

procedure TGLWin32Context.AddIAttrib(attrib, value: Integer);
var
  n: Integer;
begin
  n := Length(FiAttribs);
  SetLength(FiAttribs, n + 2);
  FiAttribs[n - 1] := attrib;
  FiAttribs[n] := value;
  FiAttribs[n + 1] := 0;
end;

// ChangeIAttrib
//

procedure TGLWin32Context.ChangeIAttrib(attrib, newValue: Integer);
var
  i: Integer;
begin
  i := 0;
  while i < Length(FiAttribs) do
  begin
    if FiAttribs[i] = attrib then
    begin
      FiAttribs[i + 1] := newValue;
      Exit;
    end;
    Inc(i, 2);
  end;
  AddIAttrib(attrib, newValue);
end;

// DropIAttrib
//

procedure TGLWin32Context.DropIAttrib(attrib: Integer);
var
  i: Integer;
begin
  i := 0;
  while i < Length(FiAttribs) do
  begin
    if FiAttribs[i] = attrib then
    begin
      Inc(i, 2);
      while i < Length(FiAttribs) do
      begin
        FiAttribs[i - 2] := FiAttribs[i];
        Inc(i);
      end;
      SetLength(FiAttribs, Length(FiAttribs) - 2);
      Exit;
    end;
    Inc(i, 2);
  end;
end;

// ClearFAttribs
//

procedure TGLWin32Context.ClearFAttribs;
begin
  SetLength(FfAttribs, 1);
  FfAttribs[0] := 0;
end;

// AddFAttrib
//

procedure TGLWin32Context.AddFAttrib(attrib, value: Single);
var
  n: Integer;
begin
  n := Length(FfAttribs);
  SetLength(FfAttribs, n + 2);
  FfAttribs[n - 1] := attrib;
  FfAttribs[n] := value;
  FfAttribs[n + 1] := 0;
end;

// DestructionEarlyWarning
//

procedure TGLWin32Context.DestructionEarlyWarning(sender: TObject);
begin
  DestroyContext;
end;

// ChooseWGLFormat
//

procedure TGLWin32Context.ChooseWGLFormat(DC: HDC; nMaxFormats: Cardinal;
  piFormats: PInteger;
  var nNumFormats: Integer; BufferCount: integer);
const
  cAAToSamples: array[aaNone..csa16xHQ] of Integer =
    (1, 2, 2, 4, 4, 6, 8, 16, 8, 8, 16, 16);
  cCSAAToSamples: array[csa8x..csa16xHQ] of Integer = (4, 8, 4, 8);

  procedure ChoosePixelFormat;
  begin
    if not wglChoosePixelFormatARB(DC, @FiAttribs[0], @FfAttribs[0],
      32, PGLint(piFormats), @nNumFormats) then
      nNumFormats := 0;
  end;

var
  float: boolean;

begin
  // request hardware acceleration
  case FAcceleration of
    chaUnknown: AddIAttrib(WGL_ACCELERATION_ARB, WGL_GENERIC_ACCELERATION_ARB);
    chaHardware: AddIAttrib(WGL_ACCELERATION_ARB, WGL_FULL_ACCELERATION_ARB);
    chaSoftware: AddIAttrib(WGL_ACCELERATION_ARB, WGL_NO_ACCELERATION_ARB);
  end;

  float := (ColorBits = 64) or (ColorBits = 128); // float_type

  if float then
  begin // float_type
    if WGL_ATI_pixel_format_float then
    begin // NV40 uses ATI_float, with linear filtering
      AddIAttrib(WGL_PIXEL_TYPE_ARB, WGL_TYPE_RGBA_FLOAT_ATI);
    end
    else
    begin
      AddIAttrib(WGL_PIXEL_TYPE_ARB, WGL_TYPE_RGBA_ARB);
      AddIAttrib(WGL_FLOAT_COMPONENTS_NV, GL_TRUE);
    end;
  end;

  if BufferCount > 1 then
    // 1 front buffer + (BufferCount-1) aux buffers
    AddIAttrib(WGL_AUX_BUFFERS_ARB, BufferCount - 1);

  AddIAttrib(WGL_COLOR_BITS_ARB, ColorBits);
  if AlphaBits > 0 then
    AddIAttrib(WGL_ALPHA_BITS_ARB, AlphaBits);
  AddIAttrib(WGL_DEPTH_BITS_ARB, DepthBits);
  if StencilBits > 0 then
    AddIAttrib(WGL_STENCIL_BITS_ARB, StencilBits);
  if AccumBits > 0 then
    AddIAttrib(WGL_ACCUM_BITS_ARB, AccumBits);
  if AuxBuffers > 0 then
    AddIAttrib(WGL_AUX_BUFFERS_ARB, AuxBuffers);
  if (AntiAliasing <> aaDefault) and WGL_ARB_multisample and FGL.ARB_multisample then
  begin
    if AntiAliasing = aaNone then
      AddIAttrib(WGL_SAMPLE_BUFFERS_ARB, GL_FALSE)
    else
    begin
      AddIAttrib(WGL_SAMPLE_BUFFERS_ARB, GL_TRUE);
      AddIAttrib(WGL_SAMPLES_ARB, cAAToSamples[AntiAliasing]);
      if (AntiAliasing >= csa8x) and (AntiAliasing <= csa16xHQ) then
        AddIAttrib(WGL_COLOR_SAMPLES_NV, cCSAAToSamples[AntiAliasing]);
    end;

  end;

  ClearFAttribs;
  ChoosePixelFormat;
  if (nNumFormats = 0) and (DepthBits >= 32) then
  begin
    // couldn't find 32+ bits depth buffer, 24 bits one available?
    ChangeIAttrib(WGL_DEPTH_BITS_ARB, 24);
    ChoosePixelFormat;
  end;
  if (nNumFormats = 0) and (DepthBits >= 24) then
  begin
    // couldn't find 24+ bits depth buffer, 16 bits one available?
    ChangeIAttrib(WGL_DEPTH_BITS_ARB, 16);
    ChoosePixelFormat;
  end;
  if (nNumFormats = 0) and (ColorBits >= 24) then
  begin
    // couldn't find 24+ bits color buffer, 16 bits one available?
    ChangeIAttrib(WGL_COLOR_BITS_ARB, 16);
    ChoosePixelFormat;
  end;
  if (nNumFormats = 0) and (AntiAliasing <> aaDefault) then
  begin
    // Restore DepthBits
    ChangeIAttrib(WGL_DEPTH_BITS_ARB, DepthBits);
    // couldn't find AA buffer, try without
    DropIAttrib(WGL_SAMPLE_BUFFERS_ARB);
    DropIAttrib(WGL_SAMPLES_ARB);
    if (AntiAliasing >= csa8x) and (AntiAliasing <= csa16xHQ) then
      DropIAttrib(WGL_COLOR_SAMPLES_NV);
    ChoosePixelFormat;
  end;
  // Check DepthBits again
  if (nNumFormats = 0) and (DepthBits >= 32) then
  begin
    // couldn't find 32+ bits depth buffer, 24 bits one available?
    ChangeIAttrib(WGL_DEPTH_BITS_ARB, 24);
    ChoosePixelFormat;
  end;
  if (nNumFormats = 0) and (DepthBits >= 24) then
  begin
    // couldn't find 24+ bits depth buffer, 16 bits one available?
    ChangeIAttrib(WGL_DEPTH_BITS_ARB, 16);
    ChoosePixelFormat;
  end;
  if (nNumFormats = 0) and (ColorBits >= 24) then
  begin
    // couldn't find 24+ bits color buffer, 16 bits one available?
    ChangeIAttrib(WGL_COLOR_BITS_ARB, 16);
    ChoosePixelFormat;
  end;
  if nNumFormats = 0 then
  begin
    // ok, last attempt: no AA, restored depth and color,
    // relaxed hardware-acceleration request
    ChangeIAttrib(WGL_COLOR_BITS_ARB, ColorBits);
    ChangeIAttrib(WGL_DEPTH_BITS_ARB, DepthBits);
    DropIAttrib(WGL_ACCELERATION_ARB);
    ChoosePixelFormat;
  end;
end;

procedure TGLWin32Context.CreateOldContext(aDC: HDC);
begin
  FRC := wglCreateContext(aDC);
  if FRC = 0 then
    RaiseLastOSError;
  FDC := aDC;

  if not wglMakeCurrent(FDC, FRC) then
    raise EGLContext.Create(Format(cContextActivationFailed,
      [GetLastError, SysErrorMessage(GetLastError)]));

  if not FLegacyContextsOnly then
  begin
    GLStates.ForwardContext := False;
    if FShareContext <> 0 then
      if not wglShareLists(FRC, FShareContext) then
        GLSLogger.LogError('DoCreateContext - Failed to share contexts');
    FGL.DebugMode := True; //rcoDebug in Options;
    FGL.Initialize;
    MakeGLCurrent;
    // If we are using AntiAliasing, adjust filtering hints
    if AntiAliasing in [aa2xHQ, aa4xHQ, csa8xHQ, csa16xHQ] then
      // Hint for nVidia HQ modes (Quincunx etc.)
      GLStates.MultisampleFilterHint := hintNicest
    else
      GLStates.MultisampleFilterHint := hintDontCare;
  end
  else
    GLSLogger.LogInfo('Temporary rendering context created');
end;

procedure TGLWin32Context.CreateNewContext(aDC: HDC);
var
  bSuccess: Boolean;
begin
  bSuccess := False;

  try
    ClearIAttribs;
    // Initialize forward context
    if GLStates.ForwardContext then
    begin
      if FGL.VERSION_4_1 then
      begin
        AddIAttrib(WGL_CONTEXT_MAJOR_VERSION_ARB, 4);
        AddIAttrib(WGL_CONTEXT_MINOR_VERSION_ARB, 1);
      end
      else if FGL.VERSION_4_0 then
      begin
        AddIAttrib(WGL_CONTEXT_MAJOR_VERSION_ARB, 4);
        AddIAttrib(WGL_CONTEXT_MINOR_VERSION_ARB, 0);
      end
      else if FGL.VERSION_3_3 then
      begin
        AddIAttrib(WGL_CONTEXT_MAJOR_VERSION_ARB, 3);
        AddIAttrib(WGL_CONTEXT_MINOR_VERSION_ARB, 3);
      end
      else if FGL.VERSION_3_2 then
      begin
        AddIAttrib(WGL_CONTEXT_MAJOR_VERSION_ARB, 3);
        AddIAttrib(WGL_CONTEXT_MINOR_VERSION_ARB, 2);
      end
      else if FGL.VERSION_3_1 then
      begin
        AddIAttrib(WGL_CONTEXT_MAJOR_VERSION_ARB, 3);
        AddIAttrib(WGL_CONTEXT_MINOR_VERSION_ARB, 1);
      end
      else if FGL.VERSION_3_0 then
      begin
        AddIAttrib(WGL_CONTEXT_MAJOR_VERSION_ARB, 3);
        AddIAttrib(WGL_CONTEXT_MINOR_VERSION_ARB, 0);
      end
      else
        Abort;
      AddIAttrib(WGL_CONTEXT_FLAGS_ARB, WGL_CONTEXT_FORWARD_COMPATIBLE_BIT_ARB);
    end;

//    if rcoDebug in Options then
    begin
      AddIAttrib(WGL_CONTEXT_FLAGS_ARB, WGL_CONTEXT_DEBUG_BIT_ARB);
    end;

    FRC := wglCreateContextAttribsARB(aDC, FShareContext, @FiAttribs[0]);

    if FRC = 0 then
    begin
      GLSLogger.LogError(cForwardContextFailed);
      Abort;
    end;

    FDC := aDC;

    if not wglMakeCurrent(FDC, FRC) then
      raise EGLContext.Create(Format(cContextActivationFailed,
        [GetLastError, SysErrorMessage(GetLastError)]));

    FGL.DebugMode := True;//rcoDebug in Options;
    FGL.Initialize;
    MakeGLCurrent;
    // If we are using AntiAliasing, adjust filtering hints
    if AntiAliasing in [aa2xHQ, aa4xHQ, csa8xHQ, csa16xHQ] then
      // Hint for nVidia HQ modes (Quincunx etc.)
      GLStates.MultisampleFilterHint := hintNicest
    else
      GLStates.MultisampleFilterHint := hintDontCare;

    if GLStates.ForwardContext then
      GLSLogger.LogInfo('Forward core context seccussfuly created');
    bSuccess := True;
  finally
    GLStates.ForwardContext := GLStates.ForwardContext and bSuccess;
    PipelineTransformation.FFPTransformation := not GLStates.ForwardContext;
  end;
end;

// DoCreateContext
//

procedure TGLWin32Context.DoCreateContext(outputDevice: HWND);
const
  cMemoryDCs = [OBJ_MEMDC, OBJ_METADC, OBJ_ENHMETADC];
  cBoolToInt: array[False..True] of Integer = (GL_FALSE, GL_TRUE);
var
  pfDescriptor: TPixelFormatDescriptor;
  pixelFormat, nbFormats, softwarePixelFormat: Integer;
  aType: DWORD;
  iFormats: array[0..31] of Integer;
  tempWnd: HWND;
  tempDC, outputDC: HDC;
  localDC: HDC;
  localRC, sharedRC: HGLRC;

  function CurrentPixelFormatIsHardwareAccelerated: Boolean;
  var
    localPFD: TPixelFormatDescriptor;
  begin
    Result := False;
    if pixelFormat = 0 then
      Exit;
    with localPFD do
    begin
      nSize := SizeOf(localPFD);
      nVersion := 1;
    end;
    DescribePixelFormat(outputDC, pixelFormat, SizeOf(localPFD), localPFD);
    Result := ((localPFD.dwFlags and PFD_GENERIC_FORMAT) = 0);
  end;

var
  i, iAttrib, iValue: Integer;
begin
{$IFDEF FPC}
  DoGetHandles(outputDevice, HDC(outputDC));
{$ELSE}
  outputDC := HDC(outputDevice);
{$ENDIF}

  if vUseWindowTrackingHook then
    TrackWindow(WindowFromDC(outputDC), DestructionEarlyWarning);

  // Just in case it didn't happen already.
  if not InitOpenGL then
    RaiseLastOSError;

  // Prepare PFD
  FillChar(pfDescriptor, SizeOf(pfDescriptor), 0);
  with PFDescriptor do
  begin
    nSize := SizeOf(PFDescriptor);
    nVersion := 1;
    dwFlags := PFD_SUPPORT_OPENGL;
    aType := GetObjectType(outputDC);
    if aType = 0 then
      RaiseLastOSError;
    if aType in cMemoryDCs then
      dwFlags := dwFlags or PFD_DRAW_TO_BITMAP
    else
      dwFlags := dwFlags or PFD_DRAW_TO_WINDOW;
    if rcoDoubleBuffered in Options then
      dwFlags := dwFlags or PFD_DOUBLEBUFFER;
    if rcoStereo in Options then
      dwFlags := dwFlags or PFD_STEREO;
    iPixelType := PFD_TYPE_RGBA;
    cColorBits := ColorBits;
    cDepthBits := DepthBits;
    cStencilBits := StencilBits;
    cAccumBits := AccumBits;
    cAlphaBits := AlphaBits;
    cAuxBuffers := AuxBuffers;
    iLayerType := PFD_MAIN_PLANE;
  end;
  pixelFormat := 0;

  // WGL_ARB_pixel_format is used if available
  //
  if not (IsMesaGL or FLegacyContextsOnly or (aType in cMemoryDCs)) then
  begin
    // the WGL mechanism is a little awkward: we first create a dummy context
    // on the TOP-level DC (ie. screen), to retrieve our pixelformat, create
    // our stuff, etc.
    tempWnd := CreateTempWnd;
    tempDC := GetDC(tempWnd);
    localDC := 0;
    localRC := 0;
    try
      SpawnLegacyContext(tempDC);
      try
        DoActivate;
        try
          FGL.ClearError;
          if WGL_ARB_pixel_format then
          begin
            // New pixel format selection via wglChoosePixelFormatARB
            ClearIAttribs;
            AddIAttrib(WGL_DRAW_TO_WINDOW_ARB, GL_TRUE);
            AddIAttrib(WGL_STEREO_ARB, cBoolToInt[rcoStereo in Options]);
            AddIAttrib(WGL_DOUBLE_BUFFER_ARB, cBoolToInt[rcoDoubleBuffered in
              Options]);

            ChooseWGLFormat(outputDC, 32, @iFormats, nbFormats);
            if nbFormats > 0 then
            begin
              if WGL_ARB_multisample and (AntiAliasing in [aaNone, aaDefault]) then
              begin
                // Pick first non AntiAliased for aaDefault and aaNone modes
                iAttrib := WGL_SAMPLE_BUFFERS_ARB;
                for i := 0 to nbFormats - 1 do
                begin
                  pixelFormat := iFormats[i];
                  iValue := GL_FALSE;
                  wglGetPixelFormatAttribivARB(outputDC, pixelFormat, 0, 1,
                    @iAttrib, @iValue);
                  if iValue = GL_FALSE then
                    Break;
                end;
              end
              else
                pixelFormat := iFormats[0];
              if GetPixelFormat(outputDC) <> pixelFormat then
              begin
                if not SetPixelFormat(outputDC, pixelFormat, @PFDescriptor) then
                  RaiseLastOSError;
              end;
            end;
          end;
        finally
          DoDeactivate;
        end;
      finally
        sharedRC := FShareContext;
        FLegacyContextsOnly := True;
        DoDestroyContext;
        FLegacyContextsOnly := False;
        FShareContext := sharedRC;
        GLSLogger.LogInfo('Temporary rendering context destroyed');
      end;
    finally
      ReleaseDC(0, tempDC);
      DestroyWindow(tempWnd);
      FDC := localDC;
      FRC := localRC;
    end;
  end;

  if pixelFormat = 0 then
  begin
    // Legacy pixel format selection
    pixelFormat := ChoosePixelFormat(outputDC, @PFDescriptor);
    if (not (aType in cMemoryDCs)) and (not
      CurrentPixelFormatIsHardwareAccelerated) then
    begin
      softwarePixelFormat := pixelFormat;
      pixelFormat := 0;
    end
    else
      softwarePixelFormat := 0;
    if pixelFormat = 0 then
    begin
      // Failed on default params, try with 16 bits depth buffer
      PFDescriptor.cDepthBits := 16;
      pixelFormat := ChoosePixelFormat(outputDC, @PFDescriptor);
      if not CurrentPixelFormatIsHardwareAccelerated then
        pixelFormat := 0;
      if pixelFormat = 0 then
      begin
        // Failed, try with 16 bits color buffer
        PFDescriptor.cColorBits := 16;
        pixelFormat := ChoosePixelFormat(outputDC, @PFDescriptor);
      end;
      if not CurrentPixelFormatIsHardwareAccelerated then
      begin
        // Fallback to original, should be supported by software
        pixelFormat := softwarePixelFormat;
      end;
      if pixelFormat = 0 then
        RaiseLastOSError;
    end;
  end;

  if GetPixelFormat(outputDC) <> pixelFormat then
  begin
    if not SetPixelFormat(outputDC, pixelFormat, @PFDescriptor) then
      RaiseLastOSError;
  end;

  // Check the properties we just set.
  DescribePixelFormat(outputDC, pixelFormat, SizeOf(PFDescriptor), PFDescriptor);
  with PFDescriptor do
    if (dwFlags and PFD_NEED_PALETTE) <> 0 then
      SetupPalette(outputDC, PFDescriptor);

  if not FLegacyContextsOnly then
  begin
    if ((pfDescriptor.dwFlags and PFD_GENERIC_FORMAT) > 0)
      and (FAcceleration = chaHardware) then
    begin
      FAcceleration := chaSoftware;
      GLSLogger.LogWarning(cFailHWRC);
    end;
  end;

  if not FLegacyContextsOnly and WGL_ARB_create_context and (FAcceleration = chaHardware) then
    CreateNewContext(outputDC)
  else
    CreateOldContext(outputDC);

  if not FLegacyContextsOnly then
  begin
    // Share identifiers with other context if it deffined
    if ResourceContext <> nil then
    begin
      if wglShareLists(TGLWin32Context(ResourceContext).FRC, FRC) then
      begin
{$IFNDEF GLS_MULTITHREAD}
        FSharedContexts.Add(ResourceContext);
        PropagateSharedContext;
{$ELSE}
        with FSharedContexts.LockList do
          try
            Add(ResourceContext);
            PropagateSharedContext;
          finally
            FSharedContexts.UnlockList;
          end;
{$ENDIF}
      end
      else
        GLSLogger.LogError('DoCreateContext - Failed to share contexts with resource context');
    end;
  end;
end;

// SpawnLegacyContext
//

procedure TGLWin32Context.SpawnLegacyContext(aDC: HDC);
begin
  try
    FLegacyContextsOnly := True;
    try
      DoCreateContext(aDC);
    finally
      FLegacyContextsOnly := False;
    end;
  except
    on E: Exception do
    begin
      raise Exception.Create(cUnableToCreateLegacyContext + #13#10
        + E.ClassName + ': ' + E.Message);
    end;
  end;
end;

// DoCreateMemoryContext
//

procedure TGLWin32Context.DoCreateMemoryContext(outputDevice: HWND; width,
  height: Integer; BufferCount: integer);
var
  nbFormats: Integer;
  iFormats: array[0..31] of Integer;
  iPBufferAttribs: array[0..0] of Integer;
  localHPBuffer: Integer;
  localRC, shareRC: HGLRC;
  localDC, tempDC: HDC;
  tempWnd: HWND;
  pfDescriptor: TPixelFormatDescriptor;
begin
  localHPBuffer := 0;
  localDC := 0;
  localRC := 0;
  // the WGL mechanism is a little awkward: we first create a dummy context
  // on the TOP-level DC (ie. screen), to retrieve our pixelformat, create
  // our stuff, etc.
  tempWnd := CreateTempWnd;
  tempDC := GetDC(tempWnd);
  try
    SpawnLegacyContext(tempDC);
    try
      DoActivate;
      try
        FGL.ClearError;
        if WGL_ARB_pixel_format and WGL_ARB_pbuffer then
        begin
          ClearIAttribs;
          AddIAttrib(WGL_DRAW_TO_PBUFFER_ARB, 1);
          ChooseWGLFormat(tempDC, 32, @iFormats, nbFormats, BufferCount);
          if nbFormats = 0 then
            raise
              Exception.Create('Format not supported for pbuffer operation.');
          iPBufferAttribs[0] := 0;

          localHPBuffer := wglCreatePbufferARB(tempDC, iFormats[0], width,
            height,
            @iPBufferAttribs[0]);
          if localHPBuffer = 0 then
            raise Exception.Create('Unabled to create pbuffer.');
          try
            localDC := wglGetPbufferDCARB(localHPBuffer);
            if localDC = 0 then
              raise Exception.Create('Unabled to create pbuffer''s DC.');
            try
              localRC := wglCreateContext(localDC);
              if localRC = 0 then
                raise Exception.Create('Unabled to create pbuffer''s RC.');
            except
              wglReleasePBufferDCARB(localHPBuffer, localDC);
              raise;
            end;
          except
            wglDestroyPBufferARB(localHPBuffer);
            raise;
          end;
        end
        else
          raise Exception.Create('WGL_ARB_pbuffer support required.');
        FGL.CheckError;
      finally
        DoDeactivate;
      end;
    finally
      shareRC := FShareContext;
      DoDestroyContext;
      FShareContext := shareRC;
    end;
  finally
    ReleaseDC(0, tempDC);
    DestroyWindow(tempWnd);
    FHPBUFFER := localHPBuffer;
    FDC := localDC;
    FRC := localRC;
  end;

  DescribePixelFormat(FDC, GetPixelFormat(FDC), SizeOf(PFDescriptor), PFDescriptor);
  if ((PFDescriptor.dwFlags and PFD_GENERIC_FORMAT) > 0)
    and (FAcceleration = chaHardware) then
  begin
    FAcceleration := chaSoftware;
    GLSLogger.LogWarning(cFailHWRC);
  end;

  if WGL_ARB_create_context then
    CreateNewContext(FDC);

  Activate;
  // Specific which color buffers are to be drawn into
  if BufferCount > 1 then
    FGL.DrawBuffers(BufferCount, @MRT_BUFFERS);
  Deactivate;
end;

// DoShareLists
//

procedure TGLWin32Context.DoShareLists(aContext: TGLContext);
var
  otherRC: HGLRC;
begin
  if aContext is TGLWin32Context then
  begin
    otherRC := TGLWin32Context(aContext).RC;
    if RC <> 0 then
    begin
      if RC <> otherRC then
      begin
        FShareContext := otherRC;
        wglShareLists(FRC, otherRC);
      end;
    end
    else
      FShareContext := otherRC;
  end
  else
    raise Exception.Create(cIncompatibleContexts);
end;

// DoDestroyContext
//

procedure TGLWin32Context.DoDestroyContext;
begin
  if vUseWindowTrackingHook then
    UnTrackWindow(WindowFromDC(FDC));

  if FRC <> 0 then
    if not wglDeleteContext(FRC) then
      raise EGLContext.Create(cDeleteContextFailed);
  if FHPBUFFER <> 0 then
  begin
    wglReleasePbufferDCARB(FHPBuffer, FDC);
    wglDestroyPbufferARB(FHPBUFFER);
    FHPBUFFER := 0;
  end;
  FRC := 0;
  FDC := 0;
  FShareContext := 0;
  if not FLegacyContextsOnly then
    FGL.Close;
end;

// DoActivate
//

procedure TGLWin32Context.DoActivate;
begin
  if not wglMakeCurrent(FDC, FRC) then
    raise EGLContext.Create(Format(cContextActivationFailed,
      [GetLastError, SysErrorMessage(GetLastError)]));

  if not FGL.IsInitialized then
    FGL.Initialize;
end;

// Deactivate
//

procedure TGLWin32Context.DoDeactivate;
begin
  if not wglMakeCurrent(0, 0) then
    raise Exception.Create(cContextDeactivationFailed);
end;

// IsValid
//

function TGLWin32Context.IsValid: Boolean;
begin
  Result := (FRC <> 0);
end;

// SwapBuffers
//

procedure TGLWin32Context.SwapBuffers;
begin
  if (FDC <> 0) and (rcoDoubleBuffered in Options) then
    Windows.SwapBuffers(FDC);
end;

// RenderOutputDevice
//

function TGLWin32Context.RenderOutputDevice: Integer;
begin
  Result := FDC;
end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization
  // ------------------------------------------------------------------
  // ------------------------------------------------------------------
  // ------------------------------------------------------------------

{$IFNDEF FPC}
  RegisterGLContextClass(TGLWin32Context);
{$ENDIF}

end.

