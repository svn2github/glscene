{
  DiF Engine

  Модуль логгинга

  Copyright (c) 2009 Romanus
  DiF Engine Team
}
unit dfLogger;

interface

uses
  Classes, dfList, dfHEngine;

const
  ConstNameEngine           =         'Space Simulator';
  //заголовок
  ConstFormatHeader         =         '/Main Thread ------------------';
  //низ
  ConstFormatBottom         =         '\Main Thread ------------------';
  //заголовок html
  ConstFormatHeaderHtml     =         ConstNameEngine + ' log start';
  //низ html
  ConstFormatBottomHtml     =         ConstNameEngine + ' log end';
  //внимание
  ConstFormatWarning        =         'Warning: ';
  //ошибка
  ConstFormatError          =         'Error: ';
  //html-льный пробел
  ConstFormatHtmlSpace      =         '&nbsp;';

  //перевод строки
  ConstEndl     : array[0..1] of Char
                            =         #13#10;
  //табулятор
  ConstTab      : Char =         #9;
  //константные символы
  ConstChars    : TdfString =  '[]():. <>';

type
  //базовая процедура
  //для вывода сообщений
  //(кодировка чистая анся)
  TdfUniProc = function (Msg:TdfStringA):TdfStringA;

  {$REGION 'dfLogger'}

  //базовый класс лога
  TdfLogger = class
  private
    //основной поток
    FStream:TMemoryStream;
    //имя файла
    FFileName:TdfStringA;
    //пишем в поток
    procedure WriteToStream(Buffer:TdfStringA);
  public
    //ссылка на поток
    property Stream:TMemoryStream read FStream;
    //имя файла
    property FileName:TdfStringA read FFileName;
    //конструктор
    constructor Create(const FName:TdfStringA='');
    //деструктор
    destructor Destroy;override;
    //Стандартные обработчики
    //сообщение
    procedure WriteMessage(Msg:TdfStringA);overload;
    //сообщение и перевод строки
    procedure WriteLnMessage(const Msg:TdfStringA = '');
    //сообщение с уникальной обработкой
    procedure WriteMessage(Msg:TdfStringA;UniProc:TdfUniProc);overload;
    //выводим конец строки
    procedure WriteEndl;
    //выводим табулятор
    procedure WriteTabChar;
    //выводим тэг
    procedure WriteTag(Msg:TdfStringA);
  end;

  {$ENDREGION}

  {$REGION 'dfFormatLogger'}

  TdfFormatlogger = class(TdfLogger)
  public
    //заголовок
    procedure WriteHeader(Msg:TdfStringA);virtual;
    //низ
    procedure WriteBottom(Msg:TdfStringA);virtual;
    //без формата
    procedure WriteUnFormated(Msg:TdfStringA);virtual;
    //das ahtung
    procedure WriteWarning(Msg:TdfStringA);virtual;
    //ошибка
    procedure WriteError(Msg:TdfStringA);virtual;
    //форматированный текст
    procedure WriteText(Msg:TdfStringA;UniProc:TdfUniProc);virtual;
    //выводим с табулятором
    procedure WriteTab(Msg:TdfStringA;const Count:byte = 1);virtual;
    //выводим с датой и временем
    procedure WriteDateTime(Msg:TdfStringA);virtual;
  end;

  {$ENDREGION}

  {$REGION 'dfHtmlLogger'}

  TdfHtmlLogger = class(TdfFormatlogger)
  public
    //заголовок
    procedure WriteHeader(Msg:TdfStringA);override;
    //низ
    procedure WriteBottom(Msg:TdfStringA);override;
    //без формата (остался неизменным)
    //procedure WriteUnFormated(Msg:TdfStringA);virtual;
    //das ahtung
    procedure WriteWarning(Msg:TdfStringA);override;
    //ошибка
    procedure WriteError(Msg:TdfStringA);override;
    //форматированный текст (остался неизменным)
    //procedure WriteText(Msg:TdfStringA;UniProc:TdfUniProc);virtual;
    //выводим с табулятором
    procedure WriteTab(Msg:TdfStringA;const Count:byte = 1);override;
    //выводим с датой и временем
    procedure WriteDateTime(Msg:TdfStringA);override;
  end;

  {$ENDREGION}



var
  //коллекция логеров
  LoggerCollection:TdfList;

//добавляем лог
function LoggerAddLog(FileName:PWideChar):Boolean;stdcall;
//удаляем лог (пишем его в файл)
function LoggerDelLog(FileName:PWideChar):Boolean;stdcall;
//ищем лог по имени
function LoggerFindLog(FileName:PWideChar):Integer;stdcall;
//пишем заголовок
function LoggerWriteHeader(Index:Integer;Msg:PWideChar):Boolean;stdcall;
//пишем низ
function LoggerWriteBottom(Index:Integer;Msg:PWideChar):Boolean;stdcall;
//пишем безформатную строку
function LoggerWriteUnFormated(Index:Integer;Msg:PWideChar):Boolean;stdcall;
//в зоне особого внимания
function LoggerWriteWarning(Index:Integer;Msg:PWideChar):Boolean;stdcall;
//пишем ошибку
function LoggerWriteError(Index:Integer;Msg:PWideChar):Boolean;stdcall;
//пишем форматированный текст
function LoggerWriteText(Index:Integer;Msg:PWideChar;Proc:TdfUniProc):Boolean;stdcall;
//пишем с табуляцией
function LoggerWriteTab(Index:Integer;Msg:PWideChar;Count:Byte):Boolean;stdcall;
//пишем с датой и временем
function LoggerWriteDateTime(Index:Integer;Msg:PWideChar):Boolean;stdcall;
//perfect.daemon:
//пишем конец строки
function LoggerWriteEndL(Index: Integer): Boolean; stdcall;


implementation

uses
  Windows, SysUtils;

{$REGION 'dfLogger'}

//пишем в поток
procedure TdfLogger.WriteToStream(Buffer:TdfStringA);
begin
  FStream.Write(PAnsiChar(Buffer)^,length(Buffer));
end;

//конструктор
constructor TdfLogger.Create(const FName:TdfStringA='');
begin
  FFileName:=FName;
  FStream:=TMemoryStream.Create;
end;

//деструктор
destructor TdfLogger.Destroy;
begin
  //если указан файл
  if FFileName <> '' then
    //сохраняем поток в файл
    FStream.SaveToFile(WideString(FFileName));
  //чистим за собой
  FStream.Free;
end;

//Стандартные обработчики
//сообщение
procedure TdfLogger.WriteMessage(Msg:TdfStringA);
begin
  Self.WriteToStream(Msg);
end;

//сообщение и перевод строки
procedure TdfLogger.WriteLnMessage(const Msg:TdfStringA = '');
begin
  Self.WriteToStream(Msg + ConstEndl);
end;

//сообщение с уникальной обработкой
procedure TdfLogger.WriteMessage(Msg:TdfStringA;UniProc:TdfUniProc);
var
  MarkedMsg:TdfStringA;
begin
  //выполняем обработку
  MarkedMsg:=UniProc(Msg);
  //пишем в поток
  Self.WriteMessage(MarkedMsg);
end;

//выводим конец строки
procedure TdfLogger.WriteEndl;
begin
  Self.WriteMessage(ConstEndl);
end;

//выводим табулятор
procedure TdfLogger.WriteTabChar;
begin
  Self.WriteMessage(ConstTab);
end;

//выводим тэг
procedure TdfLogger.WriteTag(Msg:TdfStringA);
begin
  Self.WriteMessage(ConstChars[7]+Msg+ConstChars[8]);
end;

{$ENDREGION}

{$REGION 'dfFormatLogger'}

//заголовок
procedure TdfFormatlogger.WriteHeader(Msg:TdfStringA);
begin
  Self.WriteLnMessage(ConstFormatHeader);
  Self.WriteLnMessage(Msg);
end;

//низ
procedure TdfFormatlogger.WriteBottom(Msg:TdfStringA);
begin
  Self.WriteLnMessage(Msg);
  Self.WriteLnMessage(ConstFormatBottom);
end;

//без формата
procedure TdfFormatlogger.WriteUnFormated(Msg:TdfStringA);
begin
  //униформатный вывод
  Self.WriteMessage(Msg); 
end;

//das ahtung
procedure TdfFormatlogger.WriteWarning(Msg:TdfStringA);
begin
  Self.WriteMessage(ConstFormatWarning);
  Self.WriteLnMessage(Msg);  
end;

//ошибка
procedure TdfFormatlogger.WriteError(Msg:TdfStringA);
begin
  Self.WriteMessage(ConstFormatError);
  Self.WriteLnMessage(Msg);  
end;

//форматированный текст
procedure TdfFormatlogger.WriteText(Msg:TdfStringA;UniProc:TdfUniProc);
begin
  Self.WriteMessage(Msg,UniProc);
end;

//выводим с табулятором
procedure TdfFormatlogger.WriteTab(Msg:TdfStringA;const Count:byte = 1);
var
  Pos:byte;
begin
  for Pos := 0 to Count do
    Self.WriteTabChar;
  Self.WriteMessage(Msg);
end;

//выводим с датой и временем
procedure TdfFormatlogger.WriteDateTime(Msg:TdfStringA);
var
  StrTime:String;
  ResStr:TdfStringA;
begin
  DateTimeToString(StrTime,'hh:nn:ss.zzz',Time);
  ResStr:=ConstChars[1] + DateToStr(Date);
  ResStr:=ResStr + ConstChars[7] + StrTime;
  ResStr:=ResStr + ConstChars[2];
  ResStr:=ResStr + Msg;
  Self.WriteMessage(ResStr);
end;

{$ENDREGION}

{$REGION 'dfHtmlLogger'}

//заголовок
procedure TdfHtmlLogger.WriteHeader(Msg:TdfStringA);
begin
  WriteLnMessage('<html>'#13#10'<header>' +
                  '<title>DiF Engine Log Html Output</title>'+
                  '</header>'#13#10'<body>'+
                  '<center><b>' + ConstFormatHeaderHtml +
                  '</b></center><br>');
  WriteLnMessage(Msg);
end;

//низ
procedure TdfHtmlLogger.WriteBottom(Msg:TdfStringA);
begin
  WriteLnMessage('<center>' + Msg + '</center>');
  WriteLnMessage('</body></html>');
end;

//das ahtung
procedure TdfHtmlLogger.WriteWarning(Msg:TdfStringA);
begin
  WriteLnMessage('<b style="color:blue;">'+
                 ConstFormatWarning + Msg + '</b>');
end;

//ошибка
procedure TdfHtmlLogger.WriteError(Msg:TdfStringA);
begin
  WriteLnMessage('<b style="color:red;">'+
                 ConstFormatError + Msg + '</b>');
end;

//выводим с табулятором
procedure TdfHtmlLogger.WriteTab(Msg:TdfStringA;const Count:byte = 1);
var
  NewCount,
  GetPos:Integer;
begin
  NewCount:=Count shl 1;
  for GetPos:=0 to NewCount do
    WriteLnMessage(ConstFormatHtmlSpace+ConstFormatHtmlSpace);
end;

//выводим с датой и временем
procedure TdfHtmlLogger.WriteDateTime(Msg:TdfStringA);
begin
  inherited WriteDateTime(Msg);
  WriteLnMessage('<br>');
end;

{$ENDREGION}

procedure FreeLoggerCollection;
var
  GetPos:Integer;
begin
  //чистим за собой
  for GetPos:=0 to LoggerCollection.Count-1 do
    TdfFormatLogger(LoggerCollection.Items[GetPos]).Free;
  LoggerCollection.Free;
end;

//добавляем лог
function LoggerAddLog(FileName:PWideChar):Boolean;
var
  Logger:TdfFormatlogger;
  LoggerHtml:TdfHtmlLogger;
  AString:TdfStringA;
begin
  Result:=false;
  //проверить использует
  //ли кто-то еще этот файл
  if LoggerFindLog(FileName) <> -1 then
    Exit;
  AString:=FileName;
  if Pos('html',FileName) <> 0 then
  begin
    LoggerHtml:=TdfHtmlLogger.Create(FileName);
    LoggerCollection.Add(LoggerHtml);
  end else
  begin
    Logger:=TdfFormatlogger.Create(AString);
    LoggerCollection.Add(Logger);
  end;
  Result:=true;
end;

//удаляем лог (пишем его в файл)
function LoggerDelLog(FileName:PWideChar):Boolean;
var
  Index:Integer;
begin
  Result:=false;
  Index:=LoggerFindLog(FileName);
  if Index = -1 then
    Exit;
  if TObject(LoggerCollection.Items[Index]) is TdfFormatLogger then
    TdfFormatLogger(LoggerCollection.Items[Index]).Free
  else
    TdfHtmlLogger(LoggerCollection.Items[Index]).Free;
  LoggerCollection.Delete(Index);
end;

//ищем лог по имени
function LoggerFindLog(FileName:PWideChar):Integer;
var
  GetPos:Integer;
begin
  Result:=-1;
  for GetPos:=0 to LoggerCollection.Count-1 do
  begin
    if TdfFormatLogger(LoggerCollection.Items[GetPos]).FileName = FileName then
    begin
      Result:=GetPos;
      Exit;
    end;
  end;
end;

//пишем заголовок
function LoggerWriteHeader(Index:Integer;Msg:PWideChar):Boolean;
begin
  if Index >= LoggerCollection.Count then
    Exit;
  TdfFormatLogger(LoggerCollection.Items[Index]).WriteHeader(Msg);
end;

//пишем низ
function LoggerWriteBottom(Index:Integer;Msg:PWideChar):Boolean;
begin
  if Index >= LoggerCollection.Count then
    Exit;
  TdfFormatLogger(LoggerCollection.Items[Index]).WriteBottom(PWideToPChar(Msg));
end;

//пишем безформатную строку
function LoggerWriteUnFormated(Index:Integer;Msg:PWideChar):Boolean;
begin
  if Index >= LoggerCollection.Count then
    Exit;
  TdfFormatLogger(LoggerCollection.Items[Index]).WriteUnFormated(PWideToPChar(Msg));
end;

//в зоне особого внимания
function LoggerWriteWarning(Index:Integer;Msg:PWideChar):Boolean;
begin
  if Index >= LoggerCollection.Count then
    Exit;
  TdfFormatLogger(LoggerCollection.Items[Index]).WriteWarning(PWideToPChar(Msg));
end;

//пишем ошибку
function LoggerWriteError(Index:Integer;Msg:PWideChar):Boolean;
begin
  if Index >= LoggerCollection.Count then
    Exit;
  TdfFormatLogger(LoggerCollection.Items[Index]).WriteError(PWideToPChar(Msg));
end;

//пишем форматированный текст
function LoggerWriteText(Index:Integer;Msg:PWideChar;Proc:TdfUniProc):Boolean;
begin
  if Index >= LoggerCollection.Count then
    Exit;
  TdfFormatLogger(LoggerCollection.Items[Index]).WriteText(PWideToPChar(Msg),Proc);
end;

//пишем с табуляцией
function LoggerWriteTab(Index:Integer;Msg:PWideChar;Count:Byte):Boolean;
begin
  if Index >= LoggerCollection.Count then
    Exit;
  TdfFormatLogger(LoggerCollection.Items[Index]).WriteTab(PWideToPChar(Msg),Count);
end;

//пишем с датой и временем
function LoggerWriteDateTime(Index:Integer;Msg:PWideChar):Boolean;
begin
  if Index >= LoggerCollection.Count then
    Exit;
  TdfFormatLogger(LoggerCollection.Items[Index]).WriteDateTime(PWideToPChar(Msg));
end;

//perfect.daemon:
//пишем конец строки
function LoggerWriteEndL(Index: Integer): Boolean; stdcall;
begin
  if Index >= LoggerCollection.Count then
    Exit;
  TdfFormatLogger(LoggerCollection.Items[Index]).WriteEndl();
end;

initialization

  LoggerCollection:=TdfList.Create;

finalization

  FreeLoggerCollection;

end.
