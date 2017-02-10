unit uNEATStrFunctions;

interface
  function GetBefore(sSubString, sLongString : string) : string;
  function GetAfter(sSubString, sLongString : string) : string;
  function GetReallyAfter(sSubString, sLongString : string) : string;
  function GetBetween(sSubString, sLongString : string) : string;
  function GetBetweenDifferent(sSubStringFirst,sSubStringSecond, sLongString : string) : string;
  function GetNTh(sSubString, sLongString : string; iNumber : integer) : string;
  function RightStr(sLongString : string; count : integer) : string;
  function LeftStr(sLongString : string; count : integer) : string;
  function LeftPadStr(sShortString, sLongString : string; count : integer) : string;
  function RightPadStr(sShortString, sLongString : string; count : integer) : string;
  function ReplaceStr(sFind, sReplace, sLongString : string) : string;
  function CL_IfThen(b : boolean; s_true, s_false : string) : string;overload;
  function CL_Format(const Format: string; const Args: array of const): string;

implementation

uses
  SysUtils;

function CL_IfThen(b : boolean; s_true, s_false : string) : string;
begin
  if b then
    result := s_true
  else
    result := s_false;
end;

function CL_Format(const Format: string; const Args: array of const): string;
var
  i : integer;
  s : string;
begin
  s := SysUtils.Format(Format, Args);

  for i := 1 to Length(s) do
    if s[i]='|' then
      s[i] := #9;

  result := s;
end;

function ReplaceStr(sFind, sReplace, sLongString : string) : string;
var
  i : integer;
begin
  result := '';
  i := Pos(sFind, sLongString);
  while i <> 0 do
  begin
    // Insert what comes before the found instance
    if (i>0) then
      result := result + Copy(sLongString, 1, i-1);

    // Add the replace part
    result := result + sReplace;

    // Remove the find part from the string
    Delete(sLongString, 1, i+length(sFind)-1);

    // Find the next instance
    i := Pos(sFind, sLongString);
  end;
  result := result + sLongString;
end;

function GetBefore(sSubString, sLongString : string) : string;
var
  i : integer;
begin
  i := Pos(sSubString, sLongString);
  if i <> 0 then
    GetBefore := Copy(sLongString,0, i-1)
  else
    GetBefore := '';
end;

function GetAfter(sSubString, sLongString : string) : string;
var
  i : integer;
begin
  i := Pos(sSubString, sLongString);
  if i <> 0 then
    GetAfter := Copy(sLongString,i+Length(sSubString), Length(sLongString)-i)
  else
    GetAfter := '';
end;

function GetReallyAfter(sSubString, sLongString : string) : string;
var
  i : integer;
begin
  i := Pos(sSubString, sLongString);
  if i <> 0 then
    Result := Copy(sLongString,i+Length(sSubString), Length(sLongString)-i)
  else
    Result := '';
end;

function GetBetween(sSubString, sLongString : string) : string;
begin
  GetBetween := GetBefore(sSubString,GetAfter(sSubString,sLongString));
end;

function GetBetweenDifferent(sSubStringFirst,sSubStringSecond, sLongString : string) : string;
begin
  GetBetweenDifferent := GetBefore(sSubStringSecond,GetReallyAfter(sSubStringFirst,sLongString));
end;

function GetNTh(sSubString, sLongString : string; iNumber : integer) : string;
var
  i           : integer;
  sLongLeft   : string;
  sTempResult : string;
begin
  sLongLeft := sLongString;
  sLongLeft := sLongLeft + sSubString;

  for i := 0 to iNumber do
  begin
    sTempResult := GetBefore(sSubString, sLongLeft);
    sLongLeft := GetAfter(sSubString, sLongLeft);
  end;

  GetNth := sTempResult;
end;

function RightStr(sLongString : string; count : integer) : string;
begin
  result := Copy(sLongString,length(sLongString)-count+1,count);
end;

function LeftStr(sLongString : string; count : integer) : string;
begin
  result := Copy(sLongString,1,count);
end;

function RightPadStr(sShortString, sLongString : string; count : integer) : string;
begin
  while Length(sLongString)<count do
    sLongString := sLongString+sShortString;

  result := sLongString;
end;

function LeftPadStr(sShortString, sLongString : string; count : integer) : string;
begin
  while Length(sLongString)<count do
    sLongString := sShortString+sLongString;

  result := sLongString;
end;

function PadOrgNr(s : string) : string;
begin
  result := LeftPadStr('0', s, 10);
end;

end.



