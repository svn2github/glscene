// DXPGlobals
{
   Globals for DXPExpert.<p>

   Licensed under MPL (http://www.mozilla.org/MPL/)

   Copyright 2003 - Eric Grange
}
unit DXPGlobals;

interface

uses Windows;

var
   // FreePascal
   vFPC_BinaryPath : String;
   vFPC_TimeOut : Integer = 60*1000;
   vFPC_SourcePaths : String;   // semicolon-separated
   vFPC_ShowCompileLog : Boolean = False;

procedure StoreDXPGlobals;
procedure LoadDXPGlobals;

// -----------------------------------------------------------------
// -----------------------------------------------------------------
// -----------------------------------------------------------------
implementation
// -----------------------------------------------------------------
// -----------------------------------------------------------------
// -----------------------------------------------------------------

uses IniFiles, Dialogs;

const
   cINI_FreePascal = 'FreePascal';

   cBinaryPath = 'BinaryPath';
   cSourcePaths = 'SourcePaths';
   cCompileLog = 'CompileLog';

function IniFileName : String;
begin
   SetLength(Result, 512);
   SetLength(Result, GetWindowsDirectory(PChar(Result), 510));
   Result:=Result+'\DXP.ini';
end;

// StoreDXPGlobals
//
procedure StoreDXPGlobals;
var
   ini : TIniFile;
begin
   ini:=TIniFile.Create(IniFileName);
   try
      ini.WriteString(cINI_FreePascal, cBinaryPath,  vFPC_BinaryPath);
      ini.WriteString(cINI_FreePascal, cSourcePaths, vFPC_SourcePaths);
      ini.WriteBool  (cINI_FreePascal, cCompileLog,  vFPC_ShowCompileLog);
   finally
      ini.Free;
   end;
end;

// LoadDXPGlobals
//
procedure LoadDXPGlobals;
var
   ini : TIniFile;
begin
   try
      ini:=TIniFile.Create(IniFileName);
      try
         vFPC_BinaryPath:=ini.ReadString(cINI_FreePascal, cBinaryPath, '');
         vFPC_SourcePaths:=ini.ReadString(cINI_FreePascal, cSourcePaths, '');
         vFPC_ShowCompileLog:=ini.ReadBool(cINI_FreePascal, cCompileLog, False);
      finally
         ini.Free;
      end;
   except
      ShowMessage(IniFileName+' incorrect!');
   end;
end;

end.
