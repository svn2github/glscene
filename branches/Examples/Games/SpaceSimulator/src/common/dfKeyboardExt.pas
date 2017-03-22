unit dfKeyboardExt;

interface

uses
  Windows;

{Отслеживает одиночный клик мыши}
function IsMouseClicked(mb: Integer): Boolean;

{
 Отслеживает одиночный клик клавиши,

 aPressed должен указывать на переменную, валидную в течение всего времени
 отслеживания. Если клавиша отслеживается в течение всего цикла работы приложения
 то и данная переменная должна быть доступна весь цикл работы
}
function IsKeyPressed(aCode: Integer; aPressed: PBoolean): Boolean; overload;
function IsKeyPressed(aChar: Char; aPressed: PBoolean): Boolean; overload;

implementation

var
  bL, bM, bR: Boolean;

function IsMouseClicked(mb: Integer): Boolean;
begin
  Result := False;
  case mb of
    VK_LBUTTON:
      if bL and (GetAsyncKeyState(mb) >= 0) then
      begin
        Result := True;
        bL := False;
      end
      else if GetAsyncKeyState(mb) < 0 then
        bL := True;

    VK_MBUTTON:
      if bM and (GetAsyncKeyState(mb) >= 0) then
      begin
        Result := True;
        bM := False;
      end
      else if GetAsyncKeyState(mb) < 0 then
        bM := True;

    VK_RBUTTON:
      if bR and (GetAsyncKeyState(mb) >= 0) then
      begin
        Result := True;
        bR := False;
      end
      else if GetAsyncKeyState(mb) < 0 then
        bR := True;
  end
end;


function IsKeyPressed(aCode: Integer; aPressed: PBoolean): Boolean;
begin
  Result := False;

  if (not aPressed^) and (GetAsyncKeyState(aCode) < 0) then
  begin
    Result := True;
    aPressed^ := True;
  end;

  if (GetAsyncKeyState(aCode) >= 0) then
    aPressed^ := False;
end;


function IsKeyPressed(aChar: Char; aPressed: PBoolean): Boolean;
var
  aCode: Integer;
begin
  Result := False;

  aCode := VkKeyScan(aChar) and $FF;
  if aCode <> $FF then
  begin
    if (not aPressed^) and (GetAsyncKeyState(aCode) < 0) then
    begin
      Result := True;
      aPressed^ := True;
    end;

    if (GetAsyncKeyState(aCode) >= 0) then
      aPressed^ := False;

  end
  else
    Result := False;
end;

end.
