{: GLGraphics<p>

	Cross platform support functions and types for GLScene.<p>

   Ultimately, *no* cross-platform or cross-version defines should be present
   in the core GLScene units, and have all moved here instead.<p>

	<b>Historique : </b><font size=-1><ul>
      <li>22/01/02 - EG - Added OpenPictureDialog, ApplicationTerminated
      <li>07/01/02 - EG - Added QuestionDialog and SavePictureDialog,
                          Added PrecisionTimer funcs 
      <li>06/12/01 - EG - Added several abstraction calls
      <li>31/08/01 - EG - Creation
	</ul></font>
}
unit GLCrossPlatform;

interface

{$include GLScene.inc}

{$ifdef WIN32}
uses Windows, Graphics, Dialogs, SysUtils, ExtDlgs, Controls, Forms;
{$endif}
{$ifdef LINUX}
uses QGraphics;
{$endif}

type

   // Several aliases to shield us from the need of ifdef'ing between
   // the "almost cross-platform" units like Graphics/QGraphics etc.
   // Gives a little "alien" look to names, but that's the only way around :(

   TGLPoint = TPoint;
   PGLPoint = ^TGLPoint;
   TGLRect = TRect;
   PGLRect = ^TGLRect;
   TDelphiColor = TColor;

   TGLPicture = TPicture;
   TGLGraphic = TGraphic;
   TGLBitmap = TBitmap;

const
   glpf24bit = pf24bit;
   glpf32Bit = pf32bit;

function GLPoint(const x, y : Integer) : TGLPoint;

{: Builds a TColor from Red Green Blue components. }
function RGB(const r, g, b : Byte) : TColor;
{: Converts 'magic' colors to their RGB values. }
function ColorToRGB(color : TColor) : TColor;

function GetRValue(rgb: DWORD): Byte;
function GetGValue(rgb: DWORD): Byte;
function GetBValue(rgb: DWORD): Byte;

{: Pops up a simple dialog with msg and an Ok button. }
procedure InformationDlg(const msg : String);
{: Pops up a simple question dialog with msg and yes/no buttons.<p>
   Returns True if answer was "yes". }
function QuestionDlg(const msg : String) : Boolean;
{: Posp a simple dialog with a string input. }
function InputDlg(const aCaption, aPrompt, aDefault : String) : String;
{: Pops up a simple save picture dialog. }
function SavePictureDialog(var aFileName : String; const aTitle : String = '') : Boolean;
{: Pops up a simple open picture dialog. }
function OpenPictureDialog(var aFileName : String; const aTitle : String = '') : Boolean;

{: Returns True if the application has been terminated. }
function ApplicationTerminated : Boolean;

procedure RaiseLastOSError;

{$IFNDEF GLS_DELPHI5_UP}
procedure FreeAndNil(var anObject);
{$ENDIF GLS_DELPHI5_UP}

{: Number of pixels per logical inch along the screen width for the device.<p>
   Under Win32 awaits a HDC and returns its LOGPIXELSX. }
function GetDeviceLogicalPixelsX(device : Cardinal) : Integer;
{: Number of bits per pixel for the current desktop resolution. }
function GetCurrentColorDepth : Integer;

{: Suspends thread execution for length milliseconds.<p>
   If length is zero, only the remaining time in the current thread's time
   slice is relinquished. }
procedure Sleep(length : Cardinal);

{: Returns the current value of the highest-resolution counter.<p>
   If the platform has none, should return a value derived from the highest
   precision time reference available, avoiding, if possible, timers that
   allocate specific system resources. }
procedure QueryPerformanceCounter(var val : Int64);
{: Returns the frequency of the counter used by QueryPerformanceCounter.<p>
   Return value is in ticks per second (Hz). }
procedure QueryPerformanceFrequency(var val : Int64);

{: Starts a precision timer.<p>
   Returned value should just be considered as 'handle', even if it ain't so.
   Default platform implementation is to use QueryPerformanceCounter and
   QueryPerformanceFrequency, if higher precision references are available,
   they should be used. The timer will and must be stopped/terminated/released
   with StopPrecisionTimer. }
function StartPrecisionTimer : Int64;
{: Computes time elapsed since timer start.<p>
   Return time lap in seconds. }
function PrecisionTimerLap(const precisionTimer : Int64) : Double;
{: Computes time elapsed since timer start and stop timer.<p>
   Return time lap in seconds. }
function StopPrecisionTimer(const precisionTimer : Int64) : Double;

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

// ColorToRGB
//
function ColorToRGB(color : TColor) : TColor;
begin
   Result:=Graphics.ColorToRGB(color);
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

// QuestionDlg
//
function QuestionDlg(const msg : String) : Boolean;
begin
   Result:=(MessageDlg(msg, mtConfirmation, [mbYes, mbNo], 0)=mrYes);
end;

// InputDlg
//
function InputDlg(const aCaption, aPrompt, aDefault : String) : String;
begin
   Result:=InputBox(aCaption, aPrompt, aDefault);
end;

// SavePictureDialog
//
function SavePictureDialog(var aFileName : String; const aTitle : String = '') : Boolean;
var
   saveDialog : TSavePictureDialog;
begin
   saveDialog:=TSavePictureDialog.Create(Application);
   try
      with saveDialog do begin
         Options:=[ofHideReadOnly, ofNoReadOnlyReturn];
         if aTitle<>'' then
            Title:=aTitle;
         FileName:=aFileName;
         Result:=Execute;
         if Result then
            aFileName:=FileName;
      end;
   finally
      saveDialog.Free;
   end;
end;

// OpenPictureDialog
//
function OpenPictureDialog(var aFileName : String; const aTitle : String = '') : Boolean;
var
   openDialog : TOpenPictureDialog;
begin
   openDialog:=TOpenPictureDialog.Create(Application);
   try
      with openDialog do begin
         Options:=[ofHideReadOnly, ofNoReadOnlyReturn];
         if aTitle<>'' then
            Title:=aTitle;
         FileName:=aFileName;
         Result:=Execute;
         if Result then
            aFileName:=FileName;
      end;
   finally
      openDialog.Free;
   end;
end;

// ApplicationTerminated
//
function ApplicationTerminated : Boolean;
begin
   Result:=Application.Terminated;
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

{$IFNDEF GLS_DELPHI5_UP}
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
{$ENDIF GLS_DELPHI5_UP}

// GetDeviceLogicalPixelsX
//
function GetDeviceLogicalPixelsX(device : Cardinal) : Integer;
begin
   Result:=GetDeviceCaps(device, LOGPIXELSX);
end;

// GetCurrentColorDepth
//
function GetCurrentColorDepth : Integer;
var
   topDC : HDC;
begin
   topDC:=GetDC(0);
   try
      Result:=GetDeviceCaps(topDC, BITSPIXEL)*GetDeviceCaps(topDC, PLANES);
   finally
      ReleaseDC(0, topDC);
   end;
end;

// Sleep
//
procedure Sleep(length : Cardinal);
begin
   Windows.Sleep(length);
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

// StartPrecisionTimer
//
function StartPrecisionTimer : Int64;
begin
   QueryPerformanceCounter(Result);
end;

// PrecisionTimeLap
//
function PrecisionTimerLap(const precisionTimer : Int64) : Double;
var
   cur, freq : Int64;
begin
   QueryPerformanceCounter(cur);
   QueryPerformanceFrequency(freq);
   Result:=(cur-precisionTimer)/freq;
end;

// StopPrecisionTimer
//
function StopPrecisionTimer(const precisionTimer : Int64) : Double;
var
   cur, freq : Int64;
begin
   QueryPerformanceCounter(cur);
   QueryPerformanceFrequency(freq);
   Result:=(cur-precisionTimer)/freq;
end;

end.
