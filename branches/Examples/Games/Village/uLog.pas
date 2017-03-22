unit uLog;

interface

uses

  Classes, Variants, Windows, SysUtils;


procedure log(a_Msg:string; color:integer = 7);
function logExp(a_Exp:boolean; a_Msg:string): boolean;


implementation


procedure log;
var
    sTime: TSystemTime;
begin

  if not isConsole then exit;

  GetLocalTime(sTime);
  with sTime do begin
    SetConsoleTextAttribute(GetStdHandle(STD_OUTPUT_HANDLE), color);
    writeln( Format('[%.2d:%.2d:%.2d.%.3d] ' + a_Msg,
      [wHour,wMinute,wSecond,wMilliseconds]));
  end;

end;


function logExp(a_Exp:boolean; a_Msg:string): boolean;
begin

  result := a_Exp;
  if a_Exp then
    log(a_Msg, 14);

end;

end.
