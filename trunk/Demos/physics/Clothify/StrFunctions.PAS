{

  StrFunctions by Mattias Fagerlund, mattias@cambrianlabs.com
}

unit StrFunctions;

interface
  function GetBefore(sSubString, sLongString : string) : string;
  function GetAfter(sSubString, sLongString : string) : string;
  function GetReallyAfter(sSubString, sLongString : string) : string;
  function GetBetween(sSubString, sLongString : string) : string;
  function GetBetweenDifferent(sSubStringFirst,sSubStringSecond, sLongString : string) : string;
  function GetNTh(sSubString, sLongString : string; iNumber : integer) : string;
  function RightStr(sLongString : string; count : integer) : string;
  function LeftStr(sLongString : string; count : integer) : string;
  function PadStr(sShortString, sLongString : string; count : integer) : string;
  function StringPartsEqual(const sShortString, sLongString : string; CaseSensitive : boolean=true) : boolean;
  function StringExists(sShortString, sLongString : string; CaseSensitive : boolean=true) : boolean;

  function CL_IfThen(b : boolean; s1, s2 : string) : string;

implementation

uses
  SysUtils, fClothify;

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

function PadStr(sShortString, sLongString : string; count : integer) : string;
begin
  while Length(sLongString)<count do
    sLongString := sShortString+sLongString;

  result := sLongString;
end;

function StringPartsEqual(const sShortString, sLongString : string; CaseSensitive : boolean) : boolean;
begin
  // We only care about part of the long string
  if CaseSensitive then
    result := (sShortString= Copy(sLongString,1, length(sShortString)))
  else
    result := (UpperCase(sShortString)=UpperCase( Copy(sLongString,1, length(sShortString))));
end;

function StringExists(sShortString, sLongString : string; CaseSensitive : boolean=true) : boolean;
begin
  if CaseSensitive then
    result := Pos(sShortString, sLongString)>0
  else
    result := Pos(UpperCase(sShortString),UpperCase(sLongString))>0;
end;

function CL_IfThen(b : boolean; s1, s2 : string) : string;
begin
  if b then
    result := s1
  else
    result := s2;
end;
end.



