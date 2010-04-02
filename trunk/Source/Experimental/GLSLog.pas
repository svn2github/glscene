//
// This unit is part of the GLScene Project, http://glscene.org
//
{: GLSLog<p>

  <b>Historique : </b><font size=-1><ul>
      <li>24/03/10 - Yar - Added TGLSLoger component,
                           possibility to use a more than one of log,
                           limit the number of error messages
      <li>06/03/10 - Yar - Added to GLScene
  </ul></font>

 (C) 2004-2007 George "Mirage" Bakhtadze.
 <a href="http://www.casteng.com">www.casteng.com</a> <br>
 The source code may be used under either MPL 1.1 or LGPL 2.1 license.
 See included license.txt file <br>
 Unit contains some text file related utilities and logging class
}

unit GLSLog;

interface

{$I GLScene.inc}

uses
  Classes, SysUtils
{$IFDEF MSWINDOWS} , Windows{$ENDIF}
  ;

type
  {: Levels of importance of log messages }
  TLogLevel = (lkDebug, lkInfo, lkNotice, lkWarning, lkError, lkFatalError);
  {: Log level setting type }
  TLogLevels = set of TLogLevel;

const
  llMessageLimit: array[TLogLevel] of Integer =
    (MaxInt, MaxInt, MaxInt, 500, 100, 10);

  lkPrefix: array[TLogLevel] of string = (' (D)  ', ' (i)  ', ' (I)  ',
    '(WW)   ', '(EE)   ', '(!!) ');
  llMax: TLogLevels = [lkDebug, lkInfo, lkNotice, lkWarning, lkError,
    lkFatalError];
  llMedium: TLogLevels = [lkNotice, lkWarning, lkError, lkFatalError];
  llMin: TLogLevels = [lkError, lkFatalError];

type
  {: Log date and time setting type }
  TLogTimeFormat = ({: doesn't output any time information }
    lfNone,
    {: include date in the log }
    lfDate,
    {: include time in the log }
    lfTime,
    {: include date and time in the log }
    lfDateTime,
    {: include time elapsed since startup in the log }
    lfElapsed);

  {: Class reference to log session class }
  CLogSession = class of TLogSession;

  { @Abstract(Logger class) }
  TLogSession = class
  private
    LogFile: Text;
    FLogLevels: TLogLevels;
{$IFDEF GLS_MULTITHREAD}
    CriticalSection: _RTL_CRITICAL_SECTION;
{$ENDIF}
    LogKindCount: array[TLogLevel] of Integer;
    procedure SetMode(NewMode: TLogLevels);
  protected
    {: Log mode titles }
    ModeTitles: array[TLogLevel] of string;
    {: Determines which date or time to include in the log }
    TimeFormat: TLogTimeFormat;
    {: Startup timestamp in milliseconds }
    StartedMs: Cardinal;
    {: Appends a string to log. Thread-safe if GLS_MULTITHREAD defined }
    procedure AppendLog(const Desc: string; Level: TLogLevel = lkInfo); virtual;
  public
    { Initializes a log session with the specified log file name, time and level settings }
    constructor Init(const FileName: string; ATimeFormat: TLogTimeFormat;
      ALevels: TLogLevels); virtual;
    {: Destructor }
    destructor Shutdown; virtual;

    {: Logs a string <b>Desc</b> if <b>Level</b>
       matches current GLS_LOGGING level (see @Link(LogLevels)) }
    procedure Log(const Desc: string; Level: TLogLevel = lkInfo);
    procedure LogDebug(const Desc: string);
    procedure LogInfo(const Desc: string);
    procedure LogNotice(const Desc: string);
    procedure LogWarning(const Desc: string);
    procedure LogError(const Desc: string);
    procedure LogFatalError(const Desc: string);

    {: Set of levels which to include in the log }
    property LogLevels: TLogLevels read FLogLevels write SetMode;
  end;

  // TGLSLoger
  //

  {: Abstract class for control loging.<p> }

  TGLSLoger = class(TComponent)
  private
    { Private Declarations }
    FReplaceAssertion: Boolean;
    procedure SetReplaceAssertion(Value: Boolean);
  protected
    { Protected Declarations }

  public
    { Public Declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    { Published Declarations }
    property ReplaceAssertion: Boolean read FReplaceAssertion write
      SetReplaceAssertion default False;
  end;

  {: You may use this procedure for repalce native Assertion processing
     AssertErrorProc := LogedAssert; NOTE: default is AssertErrorHandler }
{$IFDEF GLS_LOGGING}
{$IFDEF FPC}
procedure LogedAssert(const Message, Filename: ShortString; LineNumber: Integer;
  ErrorAddr: Pointer);
{$ELSE}
 procedure LogedAssert(const Message, Filename: string; LineNumber: Integer;
  ErrorAddr: Pointer);
{$ENDIF}
{$ENDIF}

function SkipBeforeSTR(var TextFile: text; SkipSTR: string): boolean;
function ReadLine(var TextFile: text): string;

{: Replaces logger with a new one of the specified class }
procedure ChangeLoggerClass(NewClass: CLogSession);

var
  {: Current logger }
  Log: TLogSession;

implementation

{$IFDEF GLS_LOGGING}
uses
  Dialogs;
{$ENDIF}

var
  vAssertErrorHandler: TAssertErrorProc;

{$IFDEF FPC}
procedure LogedAssert(const Message, Filename: ShortString; LineNumber: Integer;
  ErrorAddr: Pointer);
begin
  Log.Log(Message + ': in ' + Filename + ' at line ' + IntToStr(LineNumber),
    lkError);
  Abort;
end;
{$ELSE}
procedure LogedAssert(const Message, Filename: string; LineNumber: Integer;
  ErrorAddr: Pointer);
begin
  Log.Log(Message + ': in ' + Filename + ' at line ' + IntToStr(LineNumber),
    lkError);
  Abort;
end;
{$ENDIF}


function SkipBeforeSTR(var TextFile: text; SkipSTR: string): boolean;
var
  s: string;
begin
  repeat
    readln(TextFile, s);
    if s = SkipSTR then
    begin
      Result := True;
      Exit;
    end;
  until False;
  Result := False;
end;

function ReadLine(var TextFile: Text): string;
var
  i: Word;
var
  s: string;
begin
  if EOF(TextFile) then
    exit;
  i := 1;
  repeat
    ReadLn(TextFile, s);
  until (s <> '') and (s[1] <> '#') or EOF(TextFile);
  if s <> '' then
  begin
    while s[i] = ' ' do
      inc(i);
    if i = Length(s) then
      s := ''
    else
      s := Copy(s, i, Length(s) - i + 1);
  end;
  Result := s;
end;

procedure ChangeLoggerClass(NewClass: CLogSession);
var
  NewLog: TLogSession;
begin
  if not Assigned(Log) or (NewClass = nil) then
    Exit;
  NewLog := NewClass.Create;

  NewLog.LogKindCount := Log.LogKindCount;
  NewLog.TimeFormat := Log.TimeFormat;
  NewLog.LogLevels := Log.LogLevels;
  NewLog.StartedMs := Log.StartedMs;
  AssignFile(NewLog.LogFile, TTextRec(Log.LogFile).Name);
{$IFDEF GLS_MULTITHREAD}
  NewLog.CriticalSection := Log.CriticalSection;
{$ENDIF}

{$IFDEF GLS_MULTITHREAD}
  EnterCriticalSection(NewLog.CriticalSection);
{$ENDIF}

  Log.Free;
  Log := NewLog;

{$IFDEF GLS_MULTITHREAD}
  LeaveCriticalSection(NewLog.CriticalSection);
{$ENDIF}

  Log.Log('Logger class changed to "' + NewClass.ClassName + '"', lkNotice);
end;

// ------------------
// ------------------ TGLSLogger ------------------
// ------------------

constructor TGLSLoger.Create(AOwner : TComponent);
begin
	inherited Create(AOwner);
  vAssertErrorHandler := AssertErrorProc;
end;

destructor TGLSLoger.Destroy;
begin
	inherited Destroy;
end;

procedure TGLSLoger.SetReplaceAssertion(Value: Boolean);
begin
  if Value<>FReplaceAssertion then
  begin
    FReplaceAssertion := Value;
    case FReplaceAssertion of
      True: AssertErrorProc := LogedAssert;
      False: AssertErrorProc := vAssertErrorHandler;
    end;
  end;
end;

// ------------------
// ------------------ TLogSession ------------------
// ------------------

procedure TLogSession.SetMode(NewMode: TLogLevels);
var
  ModeStr: string;
  i: Integer;
begin
{$IFDEF GLS_LOGGING}
  ModeStr := '[';
  for i := Ord(Low(TLogLevel)) to Ord(High(TLogLevel)) do
    if TLogLevel(i) in NewMode then
    begin
      if ModeStr <> '[' then
        ModeStr := ModeStr + ', ';
      ModeStr := ModeStr + ModeTitles[TLogLevel(i)] + ' ' +
        Trim(lkPrefix[TLogLevel(i)]);
    end;
  ModeStr := ModeStr + ']';
  if NewMode = [] then
    ModeStr := 'nothing';
  Log('LOGGING ' + ModeStr, lkNotice);
  FLogLevels := NewMode;
{$ENDIF}
end;

constructor TLogSession.Init(const FileName: string; ATimeFormat:
  TLogTimeFormat; ALevels: TLogLevels);
var
  i: Integer;
  ModeStr: string;
begin
{$IFDEF GLS_LOGGING}
{$IFDEF GLS_MULTITHREAD}
  InitializeCriticalSection(CriticalSection);
{$ENDIF}

  ModeTitles[lkDebug] := 'debug info';
  ModeTitles[lkInfo] := 'info';
  ModeTitles[lkNotice] := 'notices';
  ModeTitles[lkWarning] := 'warnings';
  ModeTitles[lkError] := 'errors';
  ModeTitles[lkFatalError] := 'fatal errors';

  if Pos(':', FileName) > 0 then
    AssignFile(LogFile, Filename)
  else
    AssignFile(LogFile, GetCurrentDir + '\' + Filename);
{$I-}
  Rewrite(LogFile);
  CloseFile(LogFile);
  if IOResult <> 0 then
    ALevels := [];

  StartedMs := GetTickCount;
  TimeFormat := ATimeFormat;
  LogLevels := ALevels;
  case TimeFormat of
    lfNone: ModeStr := 'no timestamp mode.';
    lfDate: ModeStr := 'date only mode.';
    lfTime: ModeStr := 'time only mode.';
    lfDateTime: ModeStr := 'date and time mode.';
    lfElapsed: ModeStr := 'elapsed time mode.';
  end;

  Log('Log subsystem started in ' + ModeStr, lkNotice);

  for i := Ord(Low(TLogLevel)) to Ord(High(TLogLevel)) do
    LogKindCount[TLogLevel(i)] := 0;
{$ENDIF}
end;

destructor TLogSession.Shutdown;
begin
{$IFDEF GLS_LOGGING}
  Log('Logged fatal errors: ' + IntToStr(LogKindCount[lkFatalError]) +
    ', errors: ' + IntToStr(LogKindCount[lkError]) +
    ', warnings: ' + IntToStr(LogKindCount[lkWarning]) +
    ', titles: ' + IntToStr(LogKindCount[lkNotice]) +
    ', infos: ' + IntToStr(LogKindCount[lkInfo]) +
    ', debug info: ' + IntToStr(LogKindCount[lkDebug]));
  Log('Log session shutdown');
  if Self = GLSLog.Log then
    GLSLog.Log := nil;
{$IFDEF GLS_MULTITHREAD}
  DeleteCriticalSection(CriticalSection);
{$ENDIF}
{$ENDIF}
end;

procedure TLogSession.Log(const Desc: string; Level: TLogLevel = lkInfo);
begin
{$IFDEF GLS_LOGGING}
  if not (Level in LogLevels) then
    Exit;
{$IFDEF GLS_MULTITHREAD}
  EnterCriticalSection(CriticalSection);
{$ENDIF}
  AppendLog(Desc, Level);
{$IFDEF GLS_MULTITHREAD}
  LeaveCriticalSection(CriticalSection);
{$ENDIF}
{$ENDIF}
end;

procedure TLogSession.LogDebug(const Desc: string);
begin
  Log(Desc, lkDebug);
end;

procedure TLogSession.LogInfo(const Desc: string);
begin
  Log(Desc, lkInfo);
end;

procedure TLogSession.LogNotice(const Desc: string);
begin
  Log(Desc, lkNotice);
end;

procedure TLogSession.LogWarning(const Desc: string);
begin
  Log(Desc, lkWarning);
end;

procedure TLogSession.LogError(const Desc: string);
begin
  Log(Desc, lkError);
end;

procedure TLogSession.LogFatalError(const Desc: string);
begin
  Log(Desc, lkFatalError);
end;

procedure TLogSession.AppendLog(const Desc: string; Level: TLogLevel = lkInfo);
begin
{$IFDEF GLS_LOGGING}
{$I-}
  Append(LogFile);
  if IOResult <> 0 then
    Exit;
  case TimeFormat of
    lfNone: WriteLn(LogFile, lkPrefix[Level] + Desc);
    lfDate: WriteLn(LogFile, DateToStr(Now) + #9 + lkPrefix[Level] + Desc);
    lfTime: WriteLn(LogFile, TimeToStr(Now) + #9 + lkPrefix[Level] + Desc);
    lfDateTime: WriteLn(LogFile, DateTimeToStr(Now) + #9 + lkPrefix[Level] +
      Desc);
    lfElapsed: WriteLn(LogFile, IntToStr(GetTickCount - StartedMs) + #9 +
      lkPrefix[Level] + Desc);
  end;
  CloseFile(LogFile);
  Inc(LogKindCount[Level]);
  if llMessageLimit[Level] < LogKindCount[Level] then
  begin
    // Show information (window)
    MessageDlg('Exceeded the number of messages in log!', mtError, [mbOk], 0);
    Shutdown;
    Halt;
  end;
{$ENDIF}
end;

initialization
{$IFDEF GLS_LOGGING}
  GLSLog.Log := TLogSession.Init(Copy(ExtractFileName(ParamStr(0)), 1,
    Length(ExtractFileName(ParamStr(0))) - Length(ExtractFileExt(ParamStr(0)))) +
    '.log', lfElapsed, llMax);
{$ELSE}
  GLSLog.Log := TLogSession.Init('', lfNone, llMin);
{$ENDIF}
finalization
  GLSLog.Log.Shutdown;

end.

