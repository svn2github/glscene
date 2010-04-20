//
// This unit is part of the GLScene Project, http://glscene.org
//
{: GLSLog<p>

  Activate GLS_LOGGING for turn on inner GLScene logger.<p>
  You may place on form only one exemplar of TGLSLogger<p>
  then call UserLog function from any module.<p>

  <b>Historique : </b><font size=-1><ul>
      <li>02/04/10 - Yar - Added properties TimeFormat, LogLevels to TGLSLogger
                           Added function UserLog. GLS_LOGGING now only turn on inner GLScene logger
      <li>24/03/10 - Yar - Added TGLSLogger component,
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
  Classes, SysUtils, Dialogs, GLCrossPlatform
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
    constructor OnlyCreate;
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

  TGLSLogger = class(TComponent)
  private
    { Private Declarations }
    FReplaceAssertion: Boolean;
    FTimeFormat: TLogTimeFormat;
    FLogLevels: TLogLevels;
    FLog: TLogSession;
    procedure SetReplaceAssertion(Value: Boolean);
    function GetLog: TLogSession;
  protected
    { Protected Declarations }

  public
    { Public Declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    {: Set component primary and then UserLog return it's log }
    procedure DoPrimary;
    property Log: TLogSession read GetLog;
  published
    { Published Declarations }
    property ReplaceAssertion: Boolean read FReplaceAssertion write
      SetReplaceAssertion default False;
    {: Only design time sets. Define Log initial properties }
    property TimeFormat: TLogTimeFormat read FTimeFormat write FTimeFormat
      default lfElapsed;
    property LogLevels: TLogLevels read FLogLevels write FLogLevels
      default [lkDebug, lkInfo, lkNotice, lkWarning, lkError, lkFatalError];
  end;

{: Return logger wich created by TGLSLogger component }
function UserLog: TLogSession;
function SkipBeforeSTR(var TextFile: text; SkipSTR: string): boolean;
function ReadLine(var TextFile: text): string;

var
  {: GLScene inner logger }
  GLSLogger: TLogSession;

implementation

var
  vAssertErrorHandler: TAssertErrorProc;
  vCurrentLogger: TGLSLogger;

function UserLog: TLogSession;
begin
  if Assigned(vCurrentLogger) then
    Result := vCurrentLogger.Log
  else
    Result := nil;
end;

{$IFDEF FPC}
procedure LogedAssert(const Message, Filename: ShortString; LineNumber: Integer;
  ErrorAddr: Pointer);
begin
  UserLog.Log(Message + ': in ' + Filename + ' at line ' + IntToStr(LineNumber),
    lkError);
  Abort;
end;
{$ELSE}
procedure LogedAssert(const Message, Filename: string; LineNumber: Integer;
  ErrorAddr: Pointer);
begin
  UserLog.Log(Message + ': in ' + Filename + ' at line ' + IntToStr(LineNumber),
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

// ------------------
// ------------------ TGLSLogger ------------------
// ------------------

constructor TGLSLogger.Create(AOwner : TComponent);
begin
	inherited Create(AOwner);
  FTimeFormat := lfElapsed;
  FLogLevels := llMax;
  vAssertErrorHandler := AssertErrorProc;
  vCurrentLogger := Self;
end;

destructor TGLSLogger.Destroy;
begin
  if vCurrentLogger = Self then
    vCurrentLogger := nil;
  if Assigned(FLog) then
    FLog.Shutdown;
	inherited Destroy;
end;

function TGLSLogger.GetLog: TLogSession;
begin
  if not Assigned(FLog) then
    FLog := TLogSession.Init(Name+'.log', FTimeFormat, FLogLevels);
  Result := FLog;
end;

procedure TGLSLogger.DoPrimary;
begin
  vCurrentLogger := Self;
end;

procedure TGLSLogger.SetReplaceAssertion(Value: Boolean);
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
{$IFNDEF GLS_LOGGING}
  if Self = GLSLogger then
    exit;
{$ENDIF}
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
end;

constructor TLogSession.Init(const FileName: string; ATimeFormat:
  TLogTimeFormat; ALevels: TLogLevels);
var
  i: Integer;
  ModeStr: string;
begin
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
    AssignFile(LogFile, IncludeTrailingPathDelimiter(GetCurrentDir) + Filename);
{$I-}
  Rewrite(LogFile);
  CloseFile(LogFile);
  if IOResult <> 0 then
    ALevels := [];

  StartedMs := GLGetTickCount;
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
end;

constructor TLogSession.OnlyCreate;
begin
  inherited;
end;

destructor TLogSession.Shutdown;
begin
{$IFNDEF GLS_LOGGING}
  if Self = GLSLogger then
    exit;
{$ENDIF}
  Log('Logged fatal errors: ' + IntToStr(LogKindCount[lkFatalError]) +
    ', errors: ' + IntToStr(LogKindCount[lkError]) +
    ', warnings: ' + IntToStr(LogKindCount[lkWarning]) +
    ', titles: ' + IntToStr(LogKindCount[lkNotice]) +
    ', infos: ' + IntToStr(LogKindCount[lkInfo]) +
    ', debug info: ' + IntToStr(LogKindCount[lkDebug]));
  Log('Log session shutdown');
  if Self = GLSLogger then
    GLSLogger := nil;
{$IFDEF GLS_MULTITHREAD}
  DeleteCriticalSection(CriticalSection);
{$ENDIF}
end;

procedure TLogSession.Log(const Desc: string; Level: TLogLevel = lkInfo);
begin
{$IFNDEF GLS_LOGGING}
  if Self = GLSLogger then
    exit;
{$ENDIF}
  if not (Level in LogLevels) then
    Exit;
{$IFDEF GLS_MULTITHREAD}
  EnterCriticalSection(CriticalSection);
{$ENDIF}
  AppendLog(Desc, Level);
{$IFDEF GLS_MULTITHREAD}
  LeaveCriticalSection(CriticalSection);
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
{$IFNDEF GLS_LOGGING}
  if Self = GLSLogger then
    exit;
{$ENDIF}
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
    lfElapsed: WriteLn(LogFile, IntToStr(GLGetTickCount - StartedMs) + #9 +
      lkPrefix[Level] + Desc);
  end;
  CloseFile(LogFile);
  Inc(LogKindCount[Level]);
  if llMessageLimit[Level] < LogKindCount[Level] then
  begin
    // Show information (window)
    MessageDlg('Exceeded the number of messages in log!', mtError, [mbOk], 0);
    Halt;
  end;
end;

initialization
{$IFDEF GLS_LOGGING}
  GLSLogger := TLogSession.Init(Copy(ExtractFileName(ParamStr(0)), 1,
    Length(ExtractFileName(ParamStr(0))) - Length(ExtractFileExt(ParamStr(0)))) +
    '.log', lfElapsed, llMax);
{$ELSE}
  GLSLogger := TLogSession.OnlyCreate;
{$ENDIF}
finalization
  GLSLogger.Shutdown;

end.

