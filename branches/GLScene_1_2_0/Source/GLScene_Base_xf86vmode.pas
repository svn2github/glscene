{ $XFree86: xc/include/extensions/xf86vmode.h,v 3.30 2001/05/07 20:09:50 mvojkovi Exp $ }
{

Copyright 1995  Kaleb S. KEITHLEY

Permission is hereby granted, free of charge, to any person obtaining
a copy of this software and associated documentation files (the
"Software"), to deal in the Software without restriction, including
without limitation the rights to use, copy, modify, merge, publish,
distribute, sublicense, and/or sell copies of the Software, and to
permit persons to whom the Software is furnished to do so, subject to
the following conditions:

The above copyright notice and this permission notice shall be
included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
IN NO EVENT SHALL Kaleb S. KEITHLEY BE LIABLE FOR ANY CLAIM, DAMAGES 
OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE,
ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
OTHER DEALINGS IN THE SOFTWARE.

Except as contained in this notice, the name of Kaleb S. KEITHLEY 
shall not be used in advertising or otherwise to promote the sale, use 
or other dealings in this Software without prior written authorization
from Kaleb S. KEITHLEY

}
{ $Xorg: xf86vmode.h,v 1.3 2000/08/18 04:05:46 coskrey Exp $ }

{ THIS IS NOT AN X CONSORTIUM STANDARD OR AN X PROJECT TEAM SPECIFICATION }

Unit GLScene_Base_xf86vmode;

{$i GLScene.inc}

Interface

{$IFDEF GLS_X11_SUPPORT}

{$PACKRECORDS c}
{$DEFINE MACROS}

Uses
  ctypes, x, xlib, dynlibs;

Const
  libXxf86vm = 'libXxf86vm.so';

Type
  PINT32 = ^LongInt; {INT32 *}

Const
  X_XF86VidModeQueryVersion     = 0;
  X_XF86VidModeGetModeLine      = 1;
  X_XF86VidModeModModeLine      = 2;
  X_XF86VidModeSwitchMode       = 3;
  X_XF86VidModeGetMonitor       = 4;
  X_XF86VidModeLockModeSwitch   = 5;
  X_XF86VidModeGetAllModeLines  = 6;
  X_XF86VidModeAddModeLine      = 7;
  X_XF86VidModeDeleteModeLine   = 8;
  X_XF86VidModeValidateModeLine = 9;
  X_XF86VidModeSwitchToMode     = 10;
  X_XF86VidModeGetViewPort      = 11;
  X_XF86VidModeSetViewPort      = 12;
{ new for version 2.x of this extension }
  X_XF86VidModeGetDotClocks     = 13;
  X_XF86VidModeSetClientVersion = 14;
  X_XF86VidModeSetGamma         = 15;
  X_XF86VidModeGetGamma         = 16;
  X_XF86VidModeGetGammaRamp     = 17;
  X_XF86VidModeSetGammaRamp     = 18;
  X_XF86VidModeGetGammaRampSize = 19;
  X_XF86VidModeGetPermissions   = 20;

  CLKFLAG_PROGRAMABLE           = 1;

{$IFDEF XF86VIDMODE_EVENTS}
  XF86VidModeNotify             = 0;
  XF86VidModeNumberEvents       = (XF86VidModeNotify + 1);

  XF86VidModeNotifyMask         = $00000001;

  XF86VidModeNonEvent           = 0;
  XF86VidModeModeChange         = 1;
{$ELSE XF86VIDMODE_EVENTS}
  XF86VidModeNumberEvents       = 0;
{$ENDIF XF86VIDMODE_EVENTS}

  XF86VidModeBadClock           = 0;
  XF86VidModeBadHTimings        = 1;
  XF86VidModeBadVTimings        = 2;
  XF86VidModeModeUnsuitable     = 3;
  XF86VidModeExtensionDisabled  = 4;
  XF86VidModeClientNotLocal     = 5;
  XF86VidModeZoomLocked         = 6;
  XF86VidModeNumberErrors       = (XF86VidModeZoomLocked + 1);

  XF86VM_READ_PERMISSION  = 1;
  XF86VM_WRITE_PERMISSION = 2;

Type
  PXF86VidModeModeLine = ^TXF86VidModeModeLine;
  TXF86VidModeModeLine = Record
    hdisplay : cushort;
    hsyncstart : cushort;
    hsyncend : cushort;
    htotal : cushort;
    hskew : cushort;
    vdisplay : cushort;
    vsyncstart : cushort;
    vsyncend : cushort;
    vtotal : cushort;
    flags : cuint;
    privsize : cint;
    c_private : PINT32;
  End;

  PPPXF86VidModeModeInfo = ^PPXF86VidModeModeInfo;
  PPXF86VidModeModeInfo = ^PXF86VidModeModeInfo;
  PXF86VidModeModeInfo = ^TXF86VidModeModeInfo;
  TXF86VidModeModeInfo = Record
    dotclock : cuint;
    hdisplay : cushort;
    hsyncstart : cushort;
    hsyncend : cushort;
    htotal : cushort;
    hskew : cushort;
    vdisplay : cushort;
    vsyncstart : cushort;
    vsyncend : cushort;
    vtotal : cushort;
    flags : cuint;
    privsize : cint;
    c_private : PINT32;
  End;

  PXF86VidModeSyncRange = ^TXF86VidModeSyncRange;
  TXF86VidModeSyncRange = Record
    hi : cfloat;
    lo : cfloat;
  End;

  PXF86VidModeMonitor = ^TXF86VidModeMonitor;
  TXF86VidModeMonitor = Record
    vendor : PChar;
    model : PChar;
    EMPTY : cfloat;
    nhsync : cuchar;
    hsync : PXF86VidModeSyncRange;
    nvsync : cuchar;
    vsync : PXF86VidModeSyncRange;
  End;

  PXF86VidModeNotifyEvent = ^TXF86VidModeNotifyEvent;
  TXF86VidModeNotifyEvent = Record
    _type : cint;       { of event }
    serial : culong;    { # of last request processed by server }
    send_event : TBool; { true if this came from a SendEvent req }
    display : PDisplay; { Display the event was read from }
    root : TWindow;     { root window of event screen }
    state : cint;       { What happened }
    kind : cint;        { What happened }
    forced : TBool;     { extents of new region }
    time : TTime;       { event timestamp }
  End;

  PXF86VidModeGamma = ^TXF86VidModeGamma;
  TXF86VidModeGamma = Record
    red : cfloat;   { Red Gamma value }
    green : cfloat; { Green Gamma value }
    blue : cfloat;  { Blue Gamma value }
  End;

var
XF86VidModeQueryVersion : Function(
    dpy : PDisplay;
    majorVersion : Pcint;
    minorVersion : Pcint
  ) : TBoolResult; CDecl;

XF86VidModeQueryExtension : Function(
    dpy : PDisplay;
    event_base : Pcint;
    error_base : Pcint
  ) : TBoolResult; CDecl;

XF86VidModeSetClientVersion : Function(
    dpy : PDisplay
  ) : TBoolResult; CDecl;

XF86VidModeGetModeLine : Function(
    dpy : PDisplay;
    screen : cint;
    dotclock : Pcint;
    modeline : PXF86VidModeModeLine
  ) : TBoolResult; CDecl;

XF86VidModeGetAllModeLines : Function(
    dpy : PDisplay;
    screen : cint;
    modecount : Pcint;
    modelinesPtr : PPPXF86VidModeModeInfo
  ) : TBoolResult; CDecl;

XF86VidModeAddModeLine : Function(
    dpy : PDisplay;
    screen : cint;
    new_modeline : PXF86VidModeModeInfo;
    after_modeline : PXF86VidModeModeInfo
  ) : TBoolResult; CDecl;

XF86VidModeDeleteModeLine : Function(
    dpy : PDisplay;
    screen : cint;
    modeline : PXF86VidModeModeInfo
  ) : TBoolResult; CDecl;

XF86VidModeModModeLine : Function(
    dpy : PDisplay;
    screen : cint;
    modeline : PXF86VidModeModeLine
  ) : TBoolResult; CDecl;

XF86VidModeValidateModeLine : Function(
    dpy : PDisplay;
    screen : cint;
    modeline : PXF86VidModeModeInfo
  ) : TStatus; CDecl;

XF86VidModeSwitchMode : Function(
    dpy : PDisplay;
    screen : cint;
    zoom : cint
  ) : TBoolResult; CDecl;

XF86VidModeSwitchToMode : Function(
    dpy : PDisplay;
    screen : cint;
    modeline : PXF86VidModeModeInfo
  ) : TBoolResult; CDecl;

XF86VidModeLockModeSwitch : Function(
    dpy : PDisplay;
    screen : cint;
    lock : cint
  ) : TBoolResult; CDecl;

XF86VidModeGetMonitor : Function(
    dpy : PDisplay;
    screen : cint;
    monitor : PXF86VidModeMonitor
  ) : TBoolResult; CDecl;

XF86VidModeGetViewPort : Function(
    dpy : PDisplay;
    screen : cint;
    x_return : Pcint;
    y_return : Pcint
  ) : TBoolResult; CDecl;

XF86VidModeSetViewPort : Function(
    dpy : PDisplay;
    screen : cint;
    x : cint;
    y : cint
  ) : TBoolResult; CDecl;

XF86VidModeGetDotClocks : Function(
    dpy : PDisplay;
    screen : cint;
    flags_return : Pcint;
    number_of_clocks_return : Pcint;
    max_dot_clock_return : Pcint;
    clocks_return : PPcint
  ) : TBoolResult; CDecl;

XF86VidModeGetGamma : Function(
    dpy : PDisplay;
    screen : cint;
    Gamma : PXF86VidModeGamma
  ) : TBoolResult; CDecl;

XF86VidModeSetGamma : Function(
    dpy : PDisplay;
    screen : cint;
    Gamma : PXF86VidModeGamma
  ) : TBoolResult; CDecl;

XF86VidModeSetGammaRamp : Function(
    dpy : PDisplay;
    screen : cint;
    size : cint;
    red_array : Pcushort;
    green_array : Pcushort;
    blue_array : Pcushort
  ) : TBoolResult; CDecl;

XF86VidModeGetGammaRamp : Function(
    dpy : PDisplay;
    screen : cint;
    size : cint;
    red_array : Pcushort;
    green_array : Pcushort;
    blue_array : Pcushort
  ) : TBoolResult; CDecl;

XF86VidModeGetGammaRampSize : Function(
    dpy : PDisplay;
    screen : cint;
    size : Pcint
  ) : TBoolResult; CDecl;

XF86VidModeGetPermissions : Function(
    dpy : PDisplay;
    screen : cint;
    permissions : Pcint
  ) : TBoolResult; CDecl;

{$IFDEF MACROS}
Function XF86VidModeSelectNextMode (disp : PDisplay; scr : cint) : TBoolResult;
Function XF86VidModeSelectPrevMode(disp : PDisplay; scr : cint) : TBoolResult;
{$ENDIF MACROS}

procedure CloseLib;
function InitLib: boolean;
function InitFromLibrary(const libName: string): boolean;
function IsLibraryInitialized: boolean;
function xfGetProcAddress(ProcName: AnsiString): Pointer;

{$ENDIF}
Implementation
{$IFDEF GLS_X11_SUPPORT}

uses
  GLScene_Base_Log;

const
  INVALID_MODULEHANDLE = 0;

var
  vLibHandle: TLibHandle = 0; // Pointer;

  // InitOpenGL


  function InitLib: boolean;
  begin
    if (vLibHandle = INVALID_MODULEHANDLE) then
    begin
      Result := InitFromLibrary(libXxf86vm);
      if Result then
      begin

        XF86VidModeQueryVersion := xfGetProcAddress('XF86VidModeQueryVersion');

        XF86VidModeQueryExtension := xfGetProcAddress('XF86VidModeQueryExtension');

        XF86VidModeSetClientVersion := xfGetProcAddress('XF86VidModeSetClientVersion');

        XF86VidModeGetModeLine := xfGetProcAddress('XF86VidModeGetModeLine');

        XF86VidModeGetAllModeLines := xfGetProcAddress('XF86VidModeGetAllModeLines');

        XF86VidModeAddModeLine := xfGetProcAddress('XF86VidModeAddModeLine');

        XF86VidModeDeleteModeLine := xfGetProcAddress('XF86VidModeDeleteModeLine');

        XF86VidModeModModeLine := xfGetProcAddress('XF86VidModeModModeLine');

        XF86VidModeValidateModeLine := xfGetProcAddress('XF86VidModeValidateModeLine');

        XF86VidModeSwitchMode := xfGetProcAddress('XF86VidModeSwitchMode');

        XF86VidModeSwitchToMode := xfGetProcAddress('XF86VidModeSwitchToMode');

        XF86VidModeLockModeSwitch := xfGetProcAddress('XF86VidModeLockModeSwitch');

        XF86VidModeGetMonitor := xfGetProcAddress('XF86VidModeGetMonitor');

        XF86VidModeGetViewPort := xfGetProcAddress('XF86VidModeGetViewPort');

        XF86VidModeSetViewPort := xfGetProcAddress('XF86VidModeSetViewPort');

        XF86VidModeGetDotClocks := xfGetProcAddress('XF86VidModeGetDotClocks');

        XF86VidModeGetGamma := xfGetProcAddress('XF86VidModeGetGamma');

        XF86VidModeSetGamma := xfGetProcAddress('XF86VidModeSetGamma');

        XF86VidModeSetGammaRamp := xfGetProcAddress('XF86VidModeSetGammaRamp');

        XF86VidModeGetGammaRamp := xfGetProcAddress('XF86VidModeGetGammaRamp');

        XF86VidModeGetGammaRampSize := xfGetProcAddress('XF86VidModeGetGammaRampSize');

        XF86VidModeGetPermissions := xfGetProcAddress('XF86VidModeGetPermissions');
      end;
    end
    else
      Result := True;
  end;

  // InitFromLibrary


  function InitFromLibrary(const libName: string): boolean;
  begin
    Result := False;
    CloseLib;

    vLibHandle := LoadLibrary(PChar(libName));

    if (vLibHandle <> INVALID_MODULEHANDLE) then
    begin
      Result := True;
    end
    else
      begin
        CloseLib;
        GLSLogger.LogError('Xxf86vm library can not be loaded');
      end;
  end;

  // IsLInitialized


  function IsLibraryInitialized: boolean;
  begin
    Result := (vLibHandle <> INVALID_MODULEHANDLE);
  end;

  // Close


  procedure CloseLib;
  begin
    if vLibHandle <> INVALID_MODULEHANDLE then
    begin
      FreeLibrary(vLibHandle);
      vLibHandle := INVALID_MODULEHANDLE;
    end;
  end;

  function xfGetProcAddress(ProcName: AnsiString): Pointer;
  begin
    Result := GetProcAddress(vLibHandle, ProcName);
  end;

{$IFDEF MACROS}
Function XF86VidModeSelectNextMode(disp : PDisplay; scr : cint) : TBoolResult;

Begin
  XF86VidModeSelectNextMode := XF86VidModeSwitchMode(disp, scr, 1);
End;

Function XF86VidModeSelectPrevMode(disp : PDisplay; scr : cint) : TBoolResult;

Begin
  XF86VidModeSelectPrevMode := XF86VidModeSwitchMode(disp, scr, -1);
End;
{$ENDIF MACROS}
finalization

  CloseLib;
{$ENDIF}
End.
