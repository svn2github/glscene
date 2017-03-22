{
  DiF Engine

  Модуль для описания
  глобальных типов
  данных используемых
  повсеместно

  24/07/09 - daemon - Добавил 'common error names' для возвращения кодов в
                      модулях. Если хочешь, можно перенести в dfHModule,
                      на твое усмотрение, Romanus

  Copyright (c) 2009 Daemon, Romanus
  DiF Engine Team
}

unit dfHEngine;

interface

const
  //Допустимая погрешность сравнения с нулем
  cEPS = 0.0001;

type

  {$REGION 'Base Types'}

  //unicode string
  TdfString           =       WideString;
  TdfWideCharArray    =       array of PWideChar;
  TdfPString          =       PWideChar;
  //ansi string
  TdfStringA          =       AnsiString;
  TdfAnsiCharArray    =       array of PAnsiChar;
  TdfPAnsiString      =       PAnsiChar;
  //digitals
  TdfInteger          =       LongInt;
  TdfInt              =       Integer;
  //float
  TdfSingle           =       Single;
  TdfDouble           =       Double;
  TdfHandle           =       Cardinal;

  {$ENDREGION}

  {$REGION 'Common function types'}

  TdfGetCharProc = function: PChar;stdcall;
  //пустая функция для возвращения
  //целочисленной перменной
  TdfGetIntProc = function (): Integer;stdcall;
  //флаг
  TdfGetBoolProc = function (): Boolean;stdcall;

  //перекрываемые функции
  TdfSingleFunc = function (First:Pointer):Pointer;stdcall;
  TdfSecondFunc = function (First,Second:Pointer):Pointer;stdcall;
  TdfThirdFunc = function (First,Second,Third:Pointer):Pointer;stdcall;

  {$ENDREGION}

{$REGION 'Common error names'}

const
  cdfNoErrorA: PAnsiChar = 'No error';
  cdfNoErrorW: PWideChar = 'No error';
  cdfUnknownErrorA: PAnsiChar = 'Unknown error code';
  cdfUnknownErrorW: PWideChar = 'Unknown error code';

{$ENDREGION}

{$REGION 'Encode Functions'}

function PCharToPWide(AChar: PAnsiChar): PWideChar;
function PWideToPChar(pw: PWideChar): PAnsiChar;

{$ENDREGION}

{$REGION 'Pointer Function'}

//функция FreeAndNil из SysUtils
procedure AbsoluteFree(var Obj);

{$ENDREGION}

implementation

uses
  Windows;

{$REGION 'Encode Functions'}

function PCharToPWide(AChar: PAnsiChar): PWideChar;
var
  pw: PWideChar;
  iSize: integer;
begin
  iSize := Length(AChar) + 1;
  pw := AllocMem(iSize * 2);
  MultiByteToWideChar(CP_ACP, 0, AChar, iSize, pw, iSize * 2);

  Result := pw;
end;

function PWideToPChar(pw: PWideChar): PAnsiChar;
var
  p: PAnsiChar;
  iLen: integer;
begin
  iLen := lstrlenw(pw) + 1;
  GetMem(p, iLen);

  WideCharToMultiByte(CP_ACP, 0, pw, iLen, p, iLen * 2, nil, nil);

  Result := p;
  FreeMem(p, iLen);
end;

{$ENDREGION}

{$REGION 'Pointer Function'}

//функция FreeAndNil из SysUtils
procedure AbsoluteFree(var Obj);
var
  Temp: TObject;
begin
  Temp := TObject(Obj);
  Pointer(Obj) := nil;
  Temp.Free;
end;

{$ENDREGION}

initialization
  ReportMemoryLeaksOnShutDown := True;

end.