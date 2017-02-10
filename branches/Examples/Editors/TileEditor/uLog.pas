unit uLog;

interface

uses
  Winapi.Windows,
  Classes,
  Variants,
  SysUtils;


// fix current time
procedure log_tick;
// get deltatime (ms)
function log_delta: single;
// tick count
function log_tcount: integer;

// log
procedure log(a_Msg:string; color:integer = 7);
// log if exp
function logExp(a_Exp:boolean; a_Msg:string): boolean;


implementation

var
  _cc: cardinal;
  _tc: integer = -1;


procedure log_tick;
begin
  _cc := GetTickCount();
  inc( _tc );
end;


function log_tcount: integer;
begin
  result := _tc;
end;


function log_delta: single;
begin
  result := (GetTickCount() - _cc) * 0.001;
end;


procedure log;
var
    sTime: TSystemTime;
begin
  if not isConsole then exit;
  GetLocalTime(sTime);
  with sTime do begin
    SetConsoleTextAttribute(GetStdHandle(STD_OUTPUT_HANDLE), color);
///    writeln(Format('[%.2d:%.2d:%.2d.%.3d] ' + a_Msg,[wHour,wMinute,wSecond,wMilliseconds]));
  end;

end;


function logExp(a_Exp:boolean; a_Msg:string): boolean;
begin

  result := a_Exp;
  if a_Exp then
    log(a_Msg, 14);

end;

end.
