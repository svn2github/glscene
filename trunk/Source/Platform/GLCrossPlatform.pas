{: GLGraphics<p>

	Cross platform support functions and types for GLScene.<p>

   Ultimately, *no* cross-platform or cross-version defines should be present
   in the core GLScene unit, and moved here instead.<p>

	<b>Historique : </b><font size=-1><ul>
      <li>31/08/01 - EG - Creation
	</ul></font>
}
unit GLCrossPlatform;

interface

{$include GLScene.inc}

{$ifdef WIN32}
uses Windows, Graphics, Dialogs, SysUtils, GLWin32Context;
{$endif}
{$ifdef LINUX}

{$endif}

type

   TGLRect = TRect;

{: Builds a TColor from Red Green Blue components. }
function RGB(const r, g, b : Byte) : TColor;

function GetRValue(rgb: DWORD): Byte;
function GetGValue(rgb: DWORD): Byte;
function GetBValue(rgb: DWORD): Byte;

{: Pops up a simple dialog with msg and an Ok button. }
procedure InformationDlg(const msg : String);

procedure RaiseLastOSError;

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
implementation
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

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
   Result := Byte(rgb);
end;

// GetGValue
//
function GetGValue(rgb: DWORD): Byte;
begin
   Result := Byte(rgb shr 8);
end;

// GetBValue
//
function GetBValue(rgb: DWORD): Byte;
begin
   Result := Byte(rgb shr 16);
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

end.
