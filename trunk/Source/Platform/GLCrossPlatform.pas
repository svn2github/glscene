{: GLCrossPlatform<p>

	Cross platform support functions and types for GLScene.<p>

   Ultimately, *no* cross-platform or cross-version defines should be present
   in the core GLScene units, and have all moved here instead.<p>

	<b>Historique : </b><font size=-1><ul>
      <li>30/05/03 - EG - Added RDTSC and RDTSC-based precision timing for non-WIN32
      <li>22/01/02 - EG - Added OpenPictureDialog, ApplicationTerminated
      <li>07/01/02 - EG - Added QuestionDialog and SavePictureDialog,
                          Added PrecisionTimer funcs 
      <li>06/12/01 - EG - Added several abstraction calls
      <li>31/08/01 - EG - Creation
	</ul></font>
}
unit GLCrossPlatform;

interface

{$include ..\GLScene.inc}

{$ifdef MSWINDOWS}
uses Windows, Graphics, Dialogs, SysUtils, ExtDlgs, Controls, Forms;
{$endif}
{$ifdef LINUX}
uses QForms, QGraphics, QControls, QDialogs, Types, SysUtils, libc;
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

{$ifdef GLS_DELPHI_5}
   EGLOSError = EWin32Error;
{$else}
   {$ifdef FPC}
      EGLOSError = EWin32Error;
   {$else}
      EGLOSError = EOSError;
   {$endif}
{$endif}

const
{$ifdef WIN32}
   glpf24bit = pf24bit;
   glpf32Bit = pf32bit;
   glpfDevice = pfDevice;
{$endif}
{$ifdef LINUX}
   glpf24bit = pf32bit;
   glpf32Bit = pf32bit;
   glpfDevice = pf32bit;
{$endif}

   // standard colors
{$ifdef WIN32}
   clBtnFace = Graphics.clBtnFace;
{$endif}
{$ifdef LINUX}
   clBtnFace = QGraphics.clBtnFace;
{$endif}


function GLPoint(const x, y : Integer) : TGLPoint;

{: Builds a TColor from Red Green Blue components. }
function RGB(const r, g, b : Byte) : TColor;
{: Converts 'magic' colors to their RGB values. }
function ColorToRGB(color : TColor) : TColor;

function GetRValue(rgb: DWORD): Byte;
function GetGValue(rgb: DWORD): Byte;
function GetBValue(rgb: DWORD): Byte;

function GLRect(const aLeft, aTop, aRight, aBottom : Integer) : TGLRect;
{: Increases or decreases the width and height of the specified rectangle.<p>
   Adds dx units to the left and right ends of the rectangle and dy units to
   the top and bottom. }
procedure InflateGLRect(var aRect : TGLRect; dx, dy : Integer);
procedure IntersectGLRect(var aRect : TGLRect; const rect2 : TGLRect);

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

{$IFNDEF GLS_DELPHI_5_UP}
procedure FreeAndNil(var anObject);
{$ENDIF GLS_DELPHI_5_UP}

{: Number of pixels per logical inch along the screen width for the device.<p>
   Under Win32 awaits a HDC and returns its LOGPIXELSX. }
function GetDeviceLogicalPixelsX(device : Cardinal) : Integer;
{: Number of bits per pixel for the current desktop resolution. }
function GetCurrentColorDepth : Integer;
{: Returns the number of color bits associated to the given pixel format. }
function PixelFormatToColorBits(aPixelFormat : TPixelFormat) : Integer;

{: Returns the bitmap's scanline for the specified row. }
function BitmapScanLine(aBitmap : TGLBitmap; aRow : Integer) : Pointer;

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
   Return value is in ticks per second (Hz), returns False if no precision
   counter is available. }
function QueryPerformanceFrequency(var val : Int64) : Boolean;

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
{: Returns the number of CPU cycles since startup.<p>
   Use the similarly named CPU instruction. }
function RDTSC : Int64;

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
implementation
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

var
   vInvPerformanceCounterFrequency : Double;
   vInvPerformanceCounterFrequencyReady : Boolean = False;

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
   {$ifdef MSWINDOWS}
   if color<0 then
      Result:=GetSysColor(color and $FF)
   else Result:=color;
   {$else}
   Result:=QGraphics.ColorToRGB(color);
   {$endif}
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

// GLRect
//
function GLRect(const aLeft, aTop, aRight, aBottom : Integer) : TGLRect;
begin
   Result.Left:=aLeft;
   Result.Top:=aTop;
   Result.Right:=aRight;
   Result.Bottom:=aBottom;
end;

// InflateRect
//
procedure InflateGLRect(var aRect : TGLRect; dx, dy : Integer);
begin
   aRect.Left:=aRect.Left-dx;
   aRect.Right:=aRect.Right+dx;
   if aRect.Right<aRect.Left then
      aRect.Right:=aRect.Left;
   aRect.Top:=aRect.Top-dy;
   aRect.Bottom:=aRect.Bottom+dy;
   if aRect.Bottom<aRect.Top then
      aRect.Bottom:=aRect.Top;
end;

// IntersectGLRect
//
procedure IntersectGLRect(var aRect : TGLRect; const rect2 : TGLRect);
var
   a : Integer;
begin
   if (aRect.Left>rect2.Right) or (aRect.Right<rect2.Left)
      or (aRect.Top>rect2.Bottom) or (aRect.Bottom<rect2.Top) then begin
      // no intersection
      a:=0;
      aRect.Left:=a;
      aRect.Right:=a;
      aRect.Top:=a;
      aRect.Bottom:=a;
   end else begin
      if aRect.Left<rect2.Left then
         aRect.Left:=rect2.Left;
      if aRect.Right>rect2.Right then
         aRect.Right:=rect2.Right;
      if aRect.Top<rect2.Top then
         aRect.Top:=rect2.Top;
      if aRect.Bottom>rect2.Bottom then
         aRect.Bottom:=rect2.Bottom;
   end;
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
{$ifdef WIN32}
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
{$else}
begin
   InformationDlg('SavePictureDialog not supported on this platform.');
   Result:=False;
{$endif}
end;

// OpenPictureDialog
//
function OpenPictureDialog(var aFileName : String; const aTitle : String = '') : Boolean;
{$ifdef WIN32}
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
{$else}
begin
   InformationDlg('OpenPictureDialog not supported on this platform.');
   Result:=False;
{$endif}
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
var
   e : EGLOSError;
begin
   e:=EGLOSError.Create('OS Error : '+SysErrorMessage(GetLastError));
   raise e;
end;

{$IFNDEF GLS_DELPHI_5_UP}
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
{$ENDIF GLS_DELPHI_5_UP}

// GetDeviceLogicalPixelsX
//
function GetDeviceLogicalPixelsX(device : Cardinal) : Integer;
begin
   {$ifdef WIN32}
   Result:=GetDeviceCaps(device, LOGPIXELSX);
   {$else}
   Result:=96; // dunno how to do it properly, so I fake it
   {$endif}
end;

// GetCurrentColorDepth
//
function GetCurrentColorDepth : Integer;
{$ifdef WIN32}
var
   topDC : HDC;
begin
   topDC:=GetDC(0);
   try
      Result:=GetDeviceCaps(topDC, BITSPIXEL)*GetDeviceCaps(topDC, PLANES);
   finally
      ReleaseDC(0, topDC);
   end;
{$else}
begin
   Result:=32; // dunno how to do it properly, so I fake it
{$endif}
end;

// PixelFormatToColorBits
//
function PixelFormatToColorBits(aPixelFormat : TPixelFormat) : Integer;
begin
   case aPixelFormat of
      pfCustom {$ifdef WIN32}, pfDevice{$ENDIF} :  // use current color depth
         Result:=GetCurrentColorDepth;
      pf1bit  : Result:=1;
{$ifdef WIN32}
      pf4bit  : Result:=4;
      pf15bit : Result:=15;
{$endif}
      pf8bit  : Result:=8;
      pf16bit : Result:=16;
      pf32bit : Result:=32;
   else
      Result:=24;
   end;
end;

// BitmapScanLine
//
function BitmapScanLine(aBitmap : TGLBitmap; aRow : Integer) : Pointer;
begin
{$ifdef FPC}
   Assert(False, 'BitmapScanLine unsupported');
   Result:=nil;
{$else}
   Result:=aBitmap.ScanLine[aRow];
{$endif}
end;


// Sleep
//
procedure Sleep(length : Cardinal);
begin
{$ifdef WIN32}
   Windows.Sleep(length);
{$else}
   usleep(length*1000);
{$endif}
end;

// QueryPerformanceCounter
//
procedure QueryPerformanceCounter(var val : Int64);
begin
{$ifdef WIN32}
   Windows.QueryPerformanceCounter(val);
{$else}
   val:=RDTSC;
{$endif}
end;

// QueryPerformanceFrequency
//
function QueryPerformanceFrequency(var val : Int64) : Boolean;
{$ifndef WIN32}
var
   startCycles, endCycles : Int64;
   aTime, refTime : TDateTime;
{$endif}
begin
{$ifdef WIN32}
   Result:=Boolean(Windows.QueryPerformanceFrequency(val));
{$else}
   aTime:=Now;
   while aTime=Now do ;
   startCycles:=RDTSC;
   refTime:=Now;
   while refTime=Now do ;
   endCycles:=RDTSC;
   aTime:=Now;
   val:=Round((endCycles-startCycles)/((aTime-refTime)*(3600*24)));
   Result:=True;
{$endif}
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
begin
   // we can do this, because we don't really stop anything
   Result:=StopPrecisionTimer(precisionTimer);
end;

// StopPrecisionTimer
//
function StopPrecisionTimer(const precisionTimer : Int64) : Double;
var
   cur, freq : Int64;
begin
   QueryPerformanceCounter(cur);
   if not vInvPerformanceCounterFrequencyReady then begin
      QueryPerformanceFrequency(freq);
      vInvPerformanceCounterFrequency:=1.0/freq;
      vInvPerformanceCounterFrequencyReady:=True;
   end;
   Result:=(cur-precisionTimer)*vInvPerformanceCounterFrequency;
end;

// RDTSC
//
function RDTSC : Int64;
asm
   db $0f, $31
end;

end.
