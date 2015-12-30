//
// This unit is part of the DGLEngine Project, http://DGLEngine.org
//
{: DGLWin32Context<p>

      Win32 specific Context.<p>

 <b>Historique : </b><font size=-1><ul>
      <li>21/12/15 - JD -  Simplyfied and Imported From GLScene and make compatible with dglOpenGL Library
 </ul></font>
}
unit DGLWin32Context;

interface

{$I DGLEngine.inc}

{$IFNDEF MSWINDOWS}{$MESSAGE Error 'Unit is Windows specific'}{$ENDIF}

uses
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Classes,
  Vcl.Forms,

  //DGLE
  dglOpenGL,
  DGLTypes,
  DGLResStrings,
  DGLContext,
  DGLCrossPlatform,
  DGLState,
  DGLSLog,
  DGLVectorMaths;
type

  // TGLWin32Context
  //
  {: A context driver for standard Windows OpenGL (via MS OpenGL). }
  TDGLWin32Context = class(TDGLContext)
  protected
    { Protected Declarations }
    FDC: HDC;
    FRC: HGLRC;
    FShareContext: TDGLWin32Context;
    FHPBUFFER: Integer;
    FiAttribs: packed array of Integer;
    FfAttribs: packed array of Single;
    FLegacyContextsOnly: Boolean;
    FSwapBufferSupported: Boolean;

    procedure SpawnLegacyContext(aDC: HDC); // used for WGL_pixel_format soup
    procedure CreateOldContext(aDC: HDC); dynamic;
    procedure CreateNewContext(aDC: HDC); dynamic;

    procedure ClearIAttribs;
    procedure AddIAttrib(attrib, value: Integer);
    procedure ChangeIAttrib(attrib, newValue: Integer);
    procedure DropIAttrib(attrib: Integer);
    procedure ClearFAttribs;
    procedure AddFAttrib(attrib, value: Single);

    procedure DestructionEarlyWarning(sender: TObject);

    procedure ChooseWGLFormat(DC: HDC; nMaxFormats: Cardinal; piFormats:PInteger; var nNumFormats: Integer; BufferCount: integer = 1);
    procedure DoCreateContext(ADeviceHandle: HDC); override;
    procedure DoCreateMemoryContext(outputDevice: HWND; width, height:Integer; BufferCount: integer); override;
    function DoShareLists(aContext: TDGLContext): Boolean; override;
    procedure DoDestroyContext; override;
    procedure DoActivate; override;
    procedure DoDeactivate; override;
    {: DoGetHandles must be implemented in child classes,
       and return the display + window }
  public
    { Public Declarations }
    constructor Create; override;
    destructor Destroy; override;

    function IsValid: Boolean; override;
    procedure SwapBuffers; override;

    function RenderOutputDevice: Pointer; override;

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


var
  vTrackingCount: Integer;
  vTrackedHwnd: array of HWND;
  vTrackedEvents: array of TNotifyEvent;
  vTrackingHook: HHOOK;

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
    lpszClassName: 'DGLEngineUtilWindows');


// ------------------
{ Helpers Functions }
{$IFDEF GLS_REGION}{$REGION 'Helpers Functions'}{$ENDIF}

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

procedure TrackWindow(h: HWND; notifyEvent: TNotifyEvent);
begin
  if not IsWindow(h) then Exit;
  if vTrackingCount = 0 then
    vTrackingHook := SetWindowsHookEx(WH_CALLWNDPROC, @TrackHookProc, 0, GetCurrentThreadID);
  Inc(vTrackingCount);
  SetLength(vTrackedHwnd, vTrackingCount);
  vTrackedHwnd[vTrackingCount - 1] := h;
  SetLength(vTrackedEvents, vTrackingCount);
  vTrackedEvents[vTrackingCount - 1] := notifyEvent;
end;

procedure UnTrackWindow(h: HWND);
var
  i, k: Integer;
begin
  if not IsWindow(h) then Exit;
  if vTrackingCount = 0 then Exit;
  k := 0;
  for i := 0 to MinInteger(vTrackingCount, Length(vTrackedHwnd)) - 1 do
  begin
    if vTrackedHwnd[i] <> h then
    begin
      if(k <> i) then
      begin
        vTrackedHwnd[k] := vTrackedHwnd[i];
        vTrackedEvents[k] := vTrackedEvents[i];
      end;
      Inc(k);
    end
  end;
  if(k >= vTrackingCount) then exit;
  Dec(vTrackingCount);
  SetLength(vTrackedHwnd, vTrackingCount);
  SetLength(vTrackedEvents, vTrackingCount);
  if vTrackingCount = 0 then UnhookWindowsHookEx(vTrackingHook);
end;


function CreateTempWnd: HWND;
var
  classRegistered: Boolean;
  tempClass: TWndClass;
begin
  vUtilWindowClass.hInstance := HInstance;
  classRegistered := GetClassInfo(HInstance, vUtilWindowClass.lpszClassName, tempClass);
  if not classRegistered then Winapi.Windows.RegisterClass(vUtilWindowClass);
  Result := CreateWindowEx(WS_EX_TOOLWINDOW, vUtilWindowClass.lpszClassName,'', WS_POPUP, 0, 0, 0, 0, 0, 0, HInstance, nil);
end;

{$IFDEF GLS_REGIONS}{$ENDREGION}{$ENDIF}

// ------------------
{ TDGLWin32Context }
{$IFDEF GLS_REGIONS}{$REGION 'TDGLWin32Context'}{$ENDIF}

constructor TDGLWin32Context.Create;
begin
  inherited Create;
  FInitialized:=False;
  ClearIAttribs;
  ClearFAttribs;
end;

destructor TDGLWin32Context.Destroy;
begin
  inherited Destroy;
end;

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

procedure TDGLWin32Context.ClearIAttribs;
begin
  SetLength(FiAttribs, 1);
  FiAttribs[0] := 0;
end;

procedure TDGLWin32Context.AddIAttrib(attrib, value: Integer);
var
  n: Integer;
begin
  n := Length(FiAttribs);
  SetLength(FiAttribs, n + 2);
  FiAttribs[n - 1] := attrib;
  FiAttribs[n] := value;
  FiAttribs[n + 1] := 0;
end;

procedure TDGLWin32Context.ChangeIAttrib(attrib, newValue: Integer);
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

procedure TDGLWin32Context.DropIAttrib(attrib: Integer);
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

procedure TDGLWin32Context.ClearFAttribs;
begin
  SetLength(FfAttribs, 1);
  FfAttribs[0] := 0;
end;

procedure TDGLWin32Context.AddFAttrib(attrib, value: Single);
var
  n: Integer;
begin
  n := Length(FfAttribs);
  SetLength(FfAttribs, n + 2);
  FfAttribs[n - 1] := attrib;
  FfAttribs[n] := value;
  FfAttribs[n + 1] := 0;
end;

procedure TDGLWin32Context.DestructionEarlyWarning(sender: TObject);
begin
  if IsValid then
    DestroyContext;
end;

procedure TDGLWin32Context.ChooseWGLFormat(DC: HDC; nMaxFormats: Cardinal; piFormats: PInteger; var nNumFormats: Integer; BufferCount: integer);
const
  cAAToSamples: array[aaNone..csa16xHQ] of Integer =
    (1, 2, 2, 4, 4, 6, 8, 16, 8, 8, 16, 16);
  cCSAAToSamples: array[csa8x..csa16xHQ] of Integer = (4, 8, 4, 8);

  procedure ChoosePixelFormat;
  begin
    if not WGLChoosePixelFormatARB(DC, @FiAttribs[0], @FfAttribs[0], 32, PGLint(piFormats), @nNumFormats) then nNumFormats := 0;
  end;

var
  float: boolean;
  aa: TDGLAntiAliasing;
begin
  float := (ColorBits = 64) or (ColorBits = 128); // float_type

  if float then
  begin // float_type
    if dglCheckExtension('WGL_ATI_pixel_format_float') then
    begin // NV40 uses ATI_float, with linear filtering
      AddIAttrib(WGL_PIXEL_TYPE_ARB, WGL_TYPE_RGBA_FLOAT_ATI);
    end
    else
    begin
      AddIAttrib(WGL_PIXEL_TYPE_ARB, WGL_TYPE_RGBA_ARB);
      AddIAttrib(WGL_FLOAT_COMPONENTS_NV, 1);//GL_TRUE
    end;
  end;

  if BufferCount > 1 then
    // 1 front buffer + (BufferCount-1) aux buffers
    AddIAttrib(WGL_AUX_BUFFERS_ARB, BufferCount - 1);

  AddIAttrib(WGL_COLOR_BITS_ARB, ColorBits);
  if AlphaBits > 0 then AddIAttrib(WGL_ALPHA_BITS_ARB, AlphaBits);
  AddIAttrib(WGL_DEPTH_BITS_ARB, DepthBits);
  if StencilBits > 0 then AddIAttrib(WGL_STENCIL_BITS_ARB, StencilBits);
  if AccumBits > 0 then AddIAttrib(WGL_ACCUM_BITS_ARB, AccumBits);
  if AuxBuffers > 0 then AddIAttrib(WGL_AUX_BUFFERS_ARB, AuxBuffers);
  if (AntiAliasing <> aaDefault) and WGL_ARB_multisample then
  begin
    if AntiAliasing = aaNone then AddIAttrib(WGL_SAMPLE_BUFFERS_ARB, 0) //GL_FALSE
    else
    begin
      AddIAttrib(WGL_SAMPLE_BUFFERS_ARB, 1);//GL_TRUE
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
    if (AntiAliasing >= csa8x) and (AntiAliasing <= csa16xHQ) then
    begin
      DropIAttrib(WGL_COLOR_SAMPLES_NV);
      case AntiAliasing of
        csa8x, csa8xHQ: AntiAliasing := aa8x;
        csa16x, csa16xHQ: AntiAliasing := aa16x;
      end;
      ChangeIAttrib(WGL_SAMPLES_ARB, cAAToSamples[AntiAliasing]);
    end;
    ChoosePixelFormat;

    if nNumFormats = 0 then
    begin
      aa := AntiAliasing;
      repeat
        Dec(aa);
        if aa = aaNone then
        begin
          // couldn't find AA buffer, try without
          DropIAttrib(WGL_SAMPLE_BUFFERS_ARB);
          DropIAttrib(WGL_SAMPLES_ARB);
          ChoosePixelFormat;
          break;
        end;
        ChangeIAttrib(WGL_SAMPLES_ARB, cAAToSamples[aa]);
        ChoosePixelFormat;
      until nNumFormats <> 0;
      AntiAliasing := aa;
    end;
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

procedure TDGLWin32Context.CreateOldContext(aDC: HDC);
begin
  if not FLegacyContextsOnly then
  begin
    case Layer of
      clUnderlay2: FRC := wglCreateLayerContext(aDC, -2);
      clUnderlay1: FRC := wglCreateLayerContext(aDC, -1);
      clMainPlane: FRC := wglCreateContext(aDC);
      clOverlay1: FRC := wglCreateLayerContext(aDC, 1);
      clOverlay2: FRC := wglCreateLayerContext(aDC, 2);
    end;
  end
  else
    FRC := wglCreateContext(aDC);

  if FRC = 0 then RaiseLastOSError;
  FDC := aDC;

  if not wglMakeCurrent(FDC, FRC) then
    raise EDGLContext.Create(Format(cContextActivationFailed, [GetLastError, SysErrorMessage(GetLastError)]));

  if not FLegacyContextsOnly then
  begin
    if Assigned(FShareContext) and (FShareContext.RC <> 0) then
    begin
      if not wglShareLists(FShareContext.RC, FRC) then
      {$IFDEF GLS_LOGGING}
        DGLSLogger.LogWarning(glsFailedToShare)
      {$ENDIF}
      else
      begin
        FSharedContexts.Add(FShareContext);
        PropagateSharedContext;
      end;
    end;
//    FGL.DebugMode := False;
//    FGL.Initialize;
//    MakeGLCurrent;
    // If we are using AntiAliasing, adjust filtering hints
    if AntiAliasing in [aa2xHQ, aa4xHQ, csa8xHQ, csa16xHQ] then
      // Hint for nVidia HQ modes (Quincunx etc.)
      GLStates.MultisampleFilterHint := hintNicest
    else
      GLStates.MultisampleFilterHint := hintDontCare;

    if rcoDebug in Options then DGLSLogger.LogWarning(glsDriverNotSupportDebugRC);
    if rcoOGL_ES in Options then DGLSLogger.LogWarning(glsDriverNotSupportOESRC);
//    if GLStates.ForwardContext then DGLSLogger.LogWarning(glsDriverNotSupportFRC);
//    GLStates.ForwardContext := False;
  end
  else
    DGLSLogger.LogInfo(glsTmpRC_Created);
end;

procedure TDGLWin32Context.CreateNewContext(aDC: HDC);
var
  bSuccess, bOES,OpenGLInitialized: Boolean;
begin
  bSuccess := False;
  bOES := False;
  OpenGLInitialized := False;

  try
    ClearIAttribs;
    // Initialize forward context
    // Set OpenGL 3.x and up attribs
    if GL_VERSION_4_5 then
    begin
      AddIAttrib(WGL_CONTEXT_MAJOR_VERSION_ARB, 4);
      AddIAttrib(WGL_CONTEXT_MINOR_VERSION_ARB, 5);
    end
    else
    if GL_VERSION_4_4 then
    begin
      AddIAttrib(WGL_CONTEXT_MAJOR_VERSION_ARB, 4);
      AddIAttrib(WGL_CONTEXT_MINOR_VERSION_ARB, 4);
    end
    else
    if GL_VERSION_4_3 then
    begin
      AddIAttrib(WGL_CONTEXT_MAJOR_VERSION_ARB, 4);
      AddIAttrib(WGL_CONTEXT_MINOR_VERSION_ARB, 3);
    end
    else
    if GL_VERSION_4_2 then
    begin
      AddIAttrib(WGL_CONTEXT_MAJOR_VERSION_ARB, 4);
      AddIAttrib(WGL_CONTEXT_MINOR_VERSION_ARB, 2);
    end
    else if GL_VERSION_4_1 then
    begin
      AddIAttrib(WGL_CONTEXT_MAJOR_VERSION_ARB, 4);
      AddIAttrib(WGL_CONTEXT_MINOR_VERSION_ARB, 1);
    end
    else if GL_VERSION_4_0 then
    begin
      AddIAttrib(WGL_CONTEXT_MAJOR_VERSION_ARB, 4);
      AddIAttrib(WGL_CONTEXT_MINOR_VERSION_ARB, 0);
    end
    else if GL_VERSION_3_3 then
    begin
      AddIAttrib(WGL_CONTEXT_MAJOR_VERSION_ARB, 3);
      AddIAttrib(WGL_CONTEXT_MINOR_VERSION_ARB, 3);
    end
    else
    begin
      MessageBox(0, 'It is not possible to create context. At least, minimum OpenGL version 3.3 is required', 'Error', MB_OK or MB_ICONERROR);
      Abort;
    end;

   AddIAttrib(WGL_CONTEXT_FLAGS_ARB, WGL_CONTEXT_FORWARD_COMPATIBLE_BIT_ARB);

   if rcoOGL_ES in Options then DGLSLogger.LogWarning(glsOESvsForwardRC);

    if rcoOGL_ES in Options then
    begin
      if dglCheckExtension('WGL_EXT_create_context_es2_profile') then
      begin
        AddIAttrib(WGL_CONTEXT_MAJOR_VERSION_ARB, 2);
        AddIAttrib(WGL_CONTEXT_MINOR_VERSION_ARB, 0);
        AddIAttrib(WGL_CONTEXT_FLAGS_ARB, WGL_CONTEXT_ES2_PROFILE_BIT_EXT);
        bOES := True;
      end
      else
        DGLSLogger.LogError(glsDriverNotSupportOESRC);
    end;

    if rcoDebug in Options then
    begin
      AddIAttrib(WGL_CONTEXT_FLAGS_ARB, WGL_CONTEXT_DEBUG_BIT_ARB);
      //FGL.DebugMode := True;
    end;

    case Layer of
      clUnderlay2: AddIAttrib(WGL_CONTEXT_LAYER_PLANE_ARB, -2);
      clUnderlay1: AddIAttrib(WGL_CONTEXT_LAYER_PLANE_ARB, -1);
      clOverlay1: AddIAttrib(WGL_CONTEXT_LAYER_PLANE_ARB, 1);
      clOverlay2: AddIAttrib(WGL_CONTEXT_LAYER_PLANE_ARB, 2);
    end;

    FRC := 0;
    if Assigned(FShareContext) then
    begin
      // Create OpenGL 3.3 context using the attribs
      FRC := WGLCreateContextAttribsARB(aDC, FShareContext.RC, @FiAttribs[0]);
      if FRC <> 0 then
      begin
        FSharedContexts.Add(FShareContext);
        OpenGLInitialized :=True;
      end
      else
        DGLSLogger.LogWarning(glsFailedToShare)
    end;

    if FRC = 0 then
    begin
      FRC := WGLCreateContextAttribsARB(aDC, 0, @FiAttribs[0]);
      if FRC = 0 then
      begin
        if OpenGLInitialized then
    //      DGLSLogger.LogErrorFmt(cForwardContextFailed, [GetLastError, SysErrorMessage(GetLastError)])
//        else
          DGLSLogger.LogErrorFmt(cBackwardContextFailed, [GetLastError, SysErrorMessage(GetLastError)]);
        Abort;
      end;
    end;

    FDC := aDC;

    if not wglMakeCurrent(FDC, FRC) then
    begin
      DGLSLogger.LogErrorFmt(cContextActivationFailed,[GetLastError, SysErrorMessage(GetLastError)]);
      Abort;
    end;

    //FGL.Initialize;
    //MakeGLCurrent;
    // If we are using AntiAliasing, adjust filtering hints
    if AntiAliasing in [aa2xHQ, aa4xHQ, csa8xHQ, csa16xHQ] then
      // Hint for nVidia HQ modes (Quincunx etc.)
      GLStates.MultisampleFilterHint := hintNicest
    else GLStates.MultisampleFilterHint := hintDontCare;

//    if GLStates.ForwardContext then
    if OpenGLInitialized then DGLSLogger.LogInfo(glsFRC_created);
    if bOES then DGLSLogger.LogInfo(glsOESRC_created);
    bSuccess := True;
  finally
    FInitialized := OpenGLInitialized and bSuccess;
    //PipelineTransformation.LoadMatricesEnabled := not GLStates.ForwardContext;
  end;
end;

procedure TDGLWin32Context.DoCreateContext(ADeviceHandle: HDC);
const
  cMemoryDCs = [OBJ_MEMDC, OBJ_METADC, OBJ_ENHMETADC];
 cBoolToInt: array[False..True] of Integer =(0,1);// (GL_FALSE, GL_TRUE);
  cLayerToSet: array[TDGLContextLayer] of Byte = (32, 16, 0, 1, 2);
var
  pfDescriptor    : TPixelFormatDescriptor;  // Settings for the OpenGL window
  pixelFormat     : Integer;    //: GLuint;    // Settings for the OpenGL rendering
  nbFormats       : Integer;
  softwarePixelFormat : Integer;

  aType: DWORD;
  iFormats: array[0..31] of Integer;
  tempWnd: HWND;
  tempDC: HDC;
  localDC: HDC;
  localRC: HGLRC;
  sharedRC: TDGLWin32Context;

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
    DescribePixelFormat(ADeviceHandle, pixelFormat, SizeOf(localPFD), localPFD);
    Result := ((localPFD.dwFlags and PFD_GENERIC_FORMAT) = 0);
  end;

var
  i, iAttrib, iValue: Integer;
begin
  if vUseWindowTrackingHook and not FLegacyContextsOnly then TrackWindow(WindowFromDC(ADeviceHandle), DestructionEarlyWarning);

  // New call to initialize and bind the OpenGL dll
  // Just in case it didn't happen already.
  if not InitOpenGL then RaiseLastOSError;

  // Prepare PFD
  FillChar(pfDescriptor, SizeOf(pfDescriptor), 0);
  with PFDescriptor do
  begin
    nSize := SizeOf(PFDescriptor);            // Size Of This Pixel Format Descriptor
    nVersion := 1;                            // The version of this data structure
    dwFlags := PFD_DRAW_TO_WINDOW             // Buffer supports drawing to window
               or PFD_SUPPORT_OPENGL          // Buffer supports OpenGL drawing
               or PFD_DOUBLEBUFFER;           // Supports double buffering
    aType := GetObjectType(ADeviceHandle);
    if aType = 0 then RaiseLastOSError;

    if aType in cMemoryDCs then dwFlags := dwFlags or PFD_DRAW_TO_BITMAP
    else dwFlags := dwFlags or PFD_DRAW_TO_WINDOW;
    if rcoDoubleBuffered in Options then dwFlags := dwFlags or PFD_DOUBLEBUFFER;

    if rcoStereo in Options then dwFlags := dwFlags or PFD_STEREO;

    iPixelType := PFD_TYPE_RGBA;             // RGBA color format
    cColorBits      := ColorBits;            // OpenGL color depth
//    cRedBits        := 0;                    // Number of red bitplanes
//    cRedShift       := 0;                    // Shift count for red bitplanes
//    cGreenBits      := 0;                    // Number of green bitplanes
//    cGreenShift     := 0;                    // Shift count for green bitplanes
//    cBlueBits       := 0;                    // Number of blue bitplanes
//    cBlueShift      := 0;                    // Shift count for blue bitplanes
    cAlphaBits      := AlphaBits;            // Aplha Bits
    cAlphaShift     := 0;                    // Not supported
    cAccumBits      := AccumBits;            // accumulation buffer
//    cAccumRedBits   := 0;                    // Number of red bits in a-buffer
//    cAccumGreenBits := 0;                    // Number of green bits in a-buffer
//    cAccumBlueBits  := 0;                    // Number of blue bits in a-buffer
//    cAccumAlphaBits := 0;                    // Number of alpha bits in a-buffer
    cDepthBits      := DepthBits;            // Specifies the depth of the depth buffer
    cStencilBits    := StencilBits;          // Specifies the depth of the stencil buffer
    cAuxBuffers     := AuxBuffers;           // Aux Buffer supported
//    iLayerType      := PFD_MAIN_PLANE;       // Ignored

    dwLayerMask     := 0;                    // Ignored
//    dwVisibleMask   := 0;                    // Transparent color of underlay plane
    dwDamageMask    := 0;                    // Ignored

    case Layer of
      clUnderlay2, clUnderlay1: iLayerType := Byte(PFD_UNDERLAY_PLANE);
      clMainPlane: iLayerType := PFD_MAIN_PLANE;
      clOverlay1, clOverlay2: iLayerType := PFD_OVERLAY_PLANE;
    end;
    bReserved := cLayerToSet[Layer];        // Number of overlay and underlay planes
    if Layer <> clMainPlane then
      dwFlags := dwFlags or PFD_SWAP_LAYER_BUFFERS;

  end;
  pixelFormat := 0;

  // WGL_ARB_pixel_format is used if available
  //
  if not (FLegacyContextsOnly or (aType in cMemoryDCs)) then
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
          ClearOpenGLError;
          if dglCheckExtension('WGL_ARB_pixel_format') then
          begin
            // New pixel format selection via wglChoosePixelFormatARB
            ClearIAttribs;
            AddIAttrib(WGL_DRAW_TO_WINDOW_ARB, 1);//GL_TRUE
            AddIAttrib(WGL_STEREO_ARB, cBoolToInt[rcoStereo in Options]);
            AddIAttrib(WGL_DOUBLE_BUFFER_ARB, cBoolToInt[rcoDoubleBuffered in Options]);

            ChooseWGLFormat(ADeviceHandle, 32, @iFormats, nbFormats);
            if nbFormats > 0 then
            begin
              if WGL_ARB_multisample and (AntiAliasing in [aaNone, aaDefault]) then
              begin
                // Pick first non AntiAliased for aaDefault and aaNone modes
                iAttrib := WGL_SAMPLE_BUFFERS_ARB;
                for i := 0 to nbFormats - 1 do
                begin
                  pixelFormat := iFormats[i];
                  iValue := 0;// GL_FALSE;
                  WGLGetPixelFormatAttribivARB(ADeviceHandle, pixelFormat, 0, 1, @iAttrib, @iValue);
                  if iValue = 0 then Break; //GL_FALSE
                end;
              end
              else
                pixelFormat := iFormats[0];
              if GetPixelFormat(ADeviceHandle) <> pixelFormat then
              begin
                if not SetPixelFormat(ADeviceHandle, pixelFormat, @PFDescriptor) then
                  RaiseLastOSError;
              end;
            end;
          end;
        finally
          DoDeactivate;
        end;
      finally
        sharedRC := FShareContext;
        DoDestroyContext;
        FShareContext := sharedRC;
        DGLSLogger.LogInfo('Temporary rendering context destroyed');
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
    pixelFormat := ChoosePixelFormat(ADeviceHandle, @PFDescriptor);
    if (not (aType in cMemoryDCs)) and (not CurrentPixelFormatIsHardwareAccelerated) then
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
      pixelFormat := ChoosePixelFormat(ADeviceHandle, @PFDescriptor);
      if not CurrentPixelFormatIsHardwareAccelerated then pixelFormat := 0;
      if pixelFormat = 0 then
      begin
        // Failed, try with 16 bits color buffer
        PFDescriptor.cColorBits := 16;
        pixelFormat := ChoosePixelFormat(ADeviceHandle, @PFDescriptor);
      end;
      if not CurrentPixelFormatIsHardwareAccelerated then
      begin
        // Fallback to original, should be supported by software
        pixelFormat := softwarePixelFormat;
      end;
      if pixelFormat = 0 then RaiseLastOSError;
    end;
  end;

  if GetPixelFormat(ADeviceHandle) <> pixelFormat then
  begin
    if not SetPixelFormat(ADeviceHandle, pixelFormat, @PFDescriptor) then RaiseLastOSError;
  end;

  // Check the properties we just set.
  DescribePixelFormat(ADeviceHandle, pixelFormat, SizeOf(PFDescriptor), PFDescriptor);
  with PFDescriptor do
  begin
    if (dwFlags and PFD_NEED_PALETTE) <> 0 then SetupPalette(ADeviceHandle, PFDescriptor);
    FSwapBufferSupported := (dwFlags and PFD_SWAP_LAYER_BUFFERS) <> 0;
    if bReserved = 0 then FLayer := clMainPlane;
  end;

  if not FLegacyContextsOnly then
  begin
    if ((pfDescriptor.dwFlags and PFD_GENERIC_FORMAT) > 0) then
    begin
      DGLSLogger.LogWarning(cFailHWRC);
      Exit;
    end;
  end;

  if not FLegacyContextsOnly and dglCheckExtension('WGL_ARB_create_context') then
    CreateNewContext(ADeviceHandle)
  else
    CreateOldContext(ADeviceHandle);

end;

procedure TDGLWin32Context.SpawnLegacyContext(aDC: HDC);
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

procedure TDGLWin32Context.DoCreateMemoryContext(outputDevice: HWND; width,
  height: Integer; BufferCount: integer);
var
  nbFormats: Integer;
  iFormats: array[0..31] of Integer;
  iPBufferAttribs: array[0..0] of Integer;
  localHPBuffer: Integer;
  localRC: HGLRC;
  localDC, tempDC: HDC;
  tempWnd: HWND;
  shareRC: TDGLWin32Context;
  pfDescriptor: TPixelFormatDescriptor;
  bOES: Boolean;
begin
  localHPBuffer := 0;
  localDC := 0;
  localRC := 0;
  bOES := False;
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
        ClearOpenGLError;
        if dglCheckExtension('WGL_ARB_pixel_format') and dglCheckExtension('WGL_ARB_pbuffer') then
        begin
          ClearIAttribs;
          AddIAttrib(WGL_DRAW_TO_PBUFFER_ARB, 1);
          ChooseWGLFormat(tempDC, 32, @iFormats, nbFormats, BufferCount);
          if nbFormats = 0 then raise
              EPBuffer.Create('Format not supported for pbuffer operation.');
          iPBufferAttribs[0] := 0;

          localHPBuffer := WGLCreatePbufferARB(tempDC, iFormats[0], width,
            height,
            @iPBufferAttribs[0]);
          if localHPBuffer = 0 then raise EPBuffer.Create('Unabled to create pbuffer.');
          try
            localDC := WGLGetPbufferDCARB(localHPBuffer);
            if localDC = 0 then raise EPBuffer.Create('Unabled to create pbuffer''s DC.');
            try
              if dglCheckExtension('WGL_ARB_create_context') then
              begin
                // Modern creation style
                ClearIAttribs;
                // Initialize forward context
//                if GLStates.ForwardContext then
//                begin
//                  if FGL.VERSION_4_2 then
//                  begin
//                    AddIAttrib(WGL_CONTEXT_MAJOR_VERSION_ARB, 4);
//                    AddIAttrib(WGL_CONTEXT_MINOR_VERSION_ARB, 2);
//                  else
//                    Abort;
                  AddIAttrib(WGL_CONTEXT_FLAGS_ARB, WGL_CONTEXT_FORWARD_COMPATIBLE_BIT_ARB);
                  if rcoOGL_ES in Options then
                    DGLSLogger.LogWarning(glsOESvsForwardRC);
              end
              else if rcoOGL_ES in Options then
              begin
                if dglCheckExtension('WGL_EXT_create_context_es2_profile') then
                begin
                  AddIAttrib(WGL_CONTEXT_MAJOR_VERSION_ARB, 2);
                  AddIAttrib(WGL_CONTEXT_MINOR_VERSION_ARB, 0);
                  AddIAttrib(WGL_CONTEXT_FLAGS_ARB, WGL_CONTEXT_ES2_PROFILE_BIT_EXT);
                end
                else
                  DGLSLogger.LogError(glsDriverNotSupportOESRC);
              end;

              if rcoDebug in Options then
              begin
                AddIAttrib(WGL_CONTEXT_FLAGS_ARB, WGL_CONTEXT_DEBUG_BIT_ARB);
                //FGL.DebugMode := True;
              end;

              case Layer of
                clUnderlay2: AddIAttrib(WGL_CONTEXT_LAYER_PLANE_ARB, -2);
                clUnderlay1: AddIAttrib(WGL_CONTEXT_LAYER_PLANE_ARB, -1);
                clOverlay1: AddIAttrib(WGL_CONTEXT_LAYER_PLANE_ARB, 1);
                clOverlay2: AddIAttrib(WGL_CONTEXT_LAYER_PLANE_ARB, 2);
              end;

              localRC := WGLCreateContextAttribsARB(localDC, 0, @FiAttribs[0]);
              if localRC = 0 then
              begin
                {$IFDEF GLS_LOGGING}
//                  if GLStates.ForwardContext then
//                    DGLSLogger.LogErrorFmt(cForwardContextFailed, [GetLastError, SysErrorMessage(GetLastError)])
//                  else
                DGLSLogger.LogErrorFmt(cBackwardContextFailed, [GetLastError, SysErrorMessage(GetLastError)]);
                Abort;
               {$ELSE}
                raise Exception.Create('Unabled to create pbuffer''s RC.');
               {$ENDIF}
              end
              else
              begin
                // Old creation style
                localRC := wglCreateContext(localDC);
                if localRC = 0 then
                begin
                  DGLSLogger.LogErrorFmt(cBackwardContextFailed,
                    [GetLastError, SysErrorMessage(GetLastError)]);
                  Abort;
                end;
              end;

            except
              WGLReleasePBufferDCARB(localHPBuffer, localDC);
              raise;
            end;
          except
            WGLDestroyPBufferARB(localHPBuffer);
            raise;
          end;
        end
        else
          raise EPBuffer.Create('WGL_ARB_pbuffer support required.');
        CheckOpenGLError;
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
  if ((PFDescriptor.dwFlags and PFD_GENERIC_FORMAT) > 0)  then
  begin
    DGLSLogger.LogWarning(cFailHWRC);
    exit;
  end;

  Activate;
  //Initialize;    --> procedure TGLExtensionsAndEntryPoints.Initialize(ATemporary: boolean);
  // If we are using AntiAliasing, adjust filtering hints
  if AntiAliasing in [aa2xHQ, aa4xHQ, csa8xHQ, csa16xHQ] then
    GLStates.MultisampleFilterHint := hintNicest
  else if AntiAliasing in [aa2x, aa4x, csa8x, csa16x] then
    GLStates.MultisampleFilterHint := hintFastest
  else GLStates.MultisampleFilterHint := hintDontCare;

  // Specific which color buffers are to be drawn into
  if BufferCount > 1 then
    glDrawBuffers(BufferCount, @MRT_BUFFERS);



  if Assigned(FShareContext) and (FShareContext.RC <> 0) then
  begin
    if not wglShareLists(FShareContext.RC, FRC) then
      DGLSLogger.LogWarning(glsFailedToShare)
    else
    begin
      FSharedContexts.Add(FShareContext);
      PropagateSharedContext;
    end;
  end;

  Deactivate;

  if bOES then DGLSLogger.LogInfo('PBuffer ' + glsOESRC_created);
  if not (bOES) then DGLSLogger.LogInfo(glsPBufferRC_created);
end;

function TDGLWin32Context.DoShareLists(aContext: TDGLContext): Boolean;
begin
  if aContext is TDGLWin32Context then
  begin
    FShareContext := TDGLWin32Context(aContext);
    if FShareContext.RC <> 0 then
      Result := wglShareLists(FShareContext.RC, RC)
    else
      Result := False;
  end
  else
    raise Exception.Create(cIncompatibleContexts);
end;

procedure TDGLWin32Context.DoDestroyContext;
begin
  // Makes current rendering context not current, and releases the device
  // context that is used by the rendering context.

  if vUseWindowTrackingHook then
    UnTrackWindow(WindowFromDC(FDC));

  if FHPBUFFER <> 0 then
  begin
    WGLReleasePbufferDCARB(FHPBuffer, FDC);
    WGLDestroyPbufferARB(FHPBUFFER);
    FHPBUFFER := 0;
  end;

  if FRC <> 0 then
    if not wglDeleteContext(FRC) then
    begin
      DGLSLogger.LogErrorFmt(cDeleteContextFailed,[GetLastError, SysErrorMessage(GetLastError)]);
      MessageBox(0, 'Release of rendering context failed!', 'Error', MB_OK or MB_ICONERROR);
    end;

  FRC := 0;
  FDC := 0;
  FShareContext := nil;
end;

procedure TDGLWin32Context.DoActivate;
begin
  if not wglMakeCurrent(FDC, FRC) then
  begin
    MessageBox(0, 'Activation of rendering context failed!', 'Error', MB_OK or MB_ICONERROR);
    DGLSLogger.LogErrorFmt(cContextActivationFailed,[GetLastError, SysErrorMessage(GetLastError)]);
    Abort;
  end;

//  if not FGL.IsInitialized then
//    FGL.Initialize(CurrentGLContext = nil);
end;

procedure TDGLWin32Context.DoDeactivate;
begin
  if not wglMakeCurrent(0, 0) then
  begin
    MessageBox(0, 'Desactivation of rendering context failed!', 'Error', MB_OK or MB_ICONERROR);
    DGLSLogger.LogErrorFmt(cContextDeactivationFailed, [GetLastError, SysErrorMessage(GetLastError)]);
    Abort;
  end;
end;

function TDGLWin32Context.IsValid: Boolean;
begin
  Result := (FRC <> 0);
end;

procedure TDGLWin32Context.SwapBuffers;
begin
  if (FDC <> 0) and (rcoDoubleBuffered in Options) then
    if FSwapBufferSupported then
    begin
      case Layer of
        clUnderlay2: wglSwapLayerBuffers(FDC, WGL_SWAP_UNDERLAY2);
        clUnderlay1: wglSwapLayerBuffers(FDC, WGL_SWAP_UNDERLAY1);
        clMainPlane: Winapi.Windows.SwapBuffers(FDC);
        clOverlay1: wglSwapLayerBuffers(FDC, WGL_SWAP_OVERLAY1);
        clOverlay2: wglSwapLayerBuffers(FDC, WGL_SWAP_OVERLAY2);
      end;
    end
    else
      Winapi.Windows.SwapBuffers(FDC);
end;

function TDGLWin32Context.RenderOutputDevice: Pointer;
begin
  Result := Pointer(FDC);
end;

{$IFDEF GLS_REGIONS}{$ENDREGION}{$ENDIF}


// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization

  RegisterDGLContextClass(TDGLWin32Context);

end.
