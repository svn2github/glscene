{: GLGraphics<p>

	Cross platform support functions and types for GLScene.<p>

   Ultimately, *no* cross-platform or cross-version defines should be present
   in the core GLScene units, and moved here instead.<p>

	<b>Historique : </b><font size=-1><ul>
      <li>06/12/01 - EG - Added several abstraction calls
      <li>31/08/01 - EG - Creation
	</ul></font>
}
unit GLCrossPlatform;

interface

{$include GLScene.inc}

{$ifdef WIN32}
uses Windows, Graphics, Dialogs, SysUtils;//, GLWin32Context;
{$endif}
{$ifdef LINUX}

{$endif}

type

   TGLPoint = TPoint;
   PGLPoint = ^TGLPoint;
   TGLRect = TRect;
   PGLRect = ^TGLRect;

function GLPoint(const x, y : Integer) : TGLPoint;

{: Builds a TColor from Red Green Blue components. }
function RGB(const r, g, b : Byte) : TColor;

function GetRValue(rgb: DWORD): Byte;
function GetGValue(rgb: DWORD): Byte;
function GetBValue(rgb: DWORD): Byte;

{: Pops up a simple dialog with msg and an Ok button. }
procedure InformationDlg(const msg : String);

procedure RaiseLastOSError;

{$IFNDEF DELPHI5_UP}
procedure FreeAndNil(var anObject);
{$ENDIF DELPHI5_UP}

{: Number of pixels per logical inch along the screen width for the device.<p>
   Under Win32 awaits a HDC and returns its LOGPIXELSX. }
function GetDeviceLogicalPixelsX(device : Cardinal) : Integer;

{: Returns the current value of the highest-resolution counter.<p>
   If the platform has none, should return a value derived from the highest-time
   reference available. }
procedure QueryPerformanceCounter(var val : Int64);
{: Returns the frequency of the counter used by QueryPerformanceCounter.<p>
   Return value is in ticks per second (Hz). }
procedure QueryPerformanceFrequency(var val : Int64);

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
implementation
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

// GLPoint
//
function GLPoint(const x, y : Integer) : TGLPoint;
begin
   Result.X:=x;
   Result.Y:=y;
end;

// RGB
//
function RGB(const r, g, b : Byte) : TColor;
begin
   Result:=(b shl 16) or (g shl 8) or r;
end;

// GetRValue
//
function GetRValue(rgb: DWORD): Byte;
begin
   Result:=Byte(rgb);
end;

// GetGValue
//
function GetGValue(rgb: DWORD): Byte;
begin
   Result:=Byte(rgb shr 8);
end;

// GetBValue
//
function GetBValue(rgb: DWORD): Byte;
begin
   Result:=Byte(rgb shr 16);
end;

// InformationDlg
//
procedure InformationDlg(const msg : String);
begin
   ShowMessage(msg);
end;

// RaiseLastOSError
//
procedure RaiseLastOSError;
begin
   {$ifdef GLS_DELPHI_6_UP}
   SysUtils.RaiseLastOSError;
   {$else}
   RaiseLastWin32Error;
   {$endif}
end;

{$IFNDEF DELPHI5_UP}
// FreeAndNil
//
procedure FreeAndNil(var anObject);
var
  buf : TObject;
begin
  buf:=TObject(anObject);
  TObject(anObject):=nil;  // clear the reference before destroying the object
  buf.Free;
end;
{$ENDIF DELPHI5_UP}

// GetDeviceLogicalPixelsX
//
function GetDeviceLogicalPixelsX(device : Cardinal) : Integer;
begin
   Result:=GetDeviceCaps(device, LOGPIXELSX);
end;

// QueryPerformanceCounter
//
procedure QueryPerformanceCounter(var val : Int64);
begin
   Windows.QueryPerformanceCounter(val);
end;

// QueryPerformanceFrequency
//
procedure QueryPerformanceFrequency(var val : Int64);
begin
   Windows.QueryPerformanceFrequency(val);
end;

end.
