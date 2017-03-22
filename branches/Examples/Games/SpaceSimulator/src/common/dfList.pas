{
  DiF Engine

  Модуль для описания класса управления
  списком нетипизированных указателей

  TdfList
  Copyright (c) 1995-2005 Borland Software Corporation
  Modification by Romanus
  DiF Engine Team

  TdfOutStream
  Copyright (c) 2010 Romanus
  DiF Engine Team
}
unit dfList;

interface

uses
  Winapi.ActiveX,
  System.Classes,
  dfHEngine;

const
  //максимальная длина списка
  MaxListSizeRV = Maxint div 16;

type
  {$REGION 'TdfList'}

  {
    Модификация класса
    Delphi - TList

    Изменения:
      - класс отвязан от стандартной
        runtime обработки ошибок Delphi
        все методы(ну ладно пока не все)
        сделаны функциями и возвращают
        true или false как флаг
        завершения работы
      - Все типы данных и классы преименованы
        дабы не конфликтовать со стандартными
        (не считайте меня плагиатором)
  }

  PdfPointerList = ^TdfPointerList;
  TdfPointerList = array[0..MaxListSizeRV - 1] of Pointer;
  TdfListSortCompare = function (Item1, Item2: Pointer): TdfInteger;
  TdfListNotification = (lnAdded, lnExtracted, lnDeleted);

  TdfListAssignOp = (laCopy, laAnd, laOr, laXor, laSrcUnique, laDestUnique);

  TdfList = class;

  {$REGION 'TdfListEnum'}

  //Класс для поддержки
  //массивов коллекций
  //для цикла for in

  TdfListEnum = class
  private
    FIndex: TdfInteger;
    FList: TdfList;
  public
    constructor Create(AList: TdfList);
    function GetCurrent: Pointer;
    function MoveNext: Boolean;
    property Current: Pointer read GetCurrent;
  end;

  {$ENDREGION}

  TdfList = class(TObject)
  private
    FList: PdfPointerList;
    FCount: TdfInteger;
    FCapacity: TdfInteger;
  protected
    function Get(Index: TdfInteger): Pointer;
    procedure Grow; virtual;
    procedure Put(Index: Integer; Item: Pointer);
    procedure Notify(Ptr: Pointer; Action: TdfListNotification); virtual;
    procedure SetCapacity(NewCapacity: TdfInteger);
    procedure SetCount(NewCount: TdfInteger);
  public
    destructor Destroy; override;
    function Add(Item: Pointer): TdfInteger;
    procedure Clear; virtual;
    function Delete(Index: TdfInteger): Boolean;
    //меняем местами элементы
    function Exchange(Index1, Index2: TdfInteger): Boolean;
    function Expand: TdfList;
    function Extract(Item: Pointer): Pointer;
    function First: Pointer;
    function GetEnumerator: TdfListEnum;
    function IndexOf(Item: Pointer): TdfInteger;
    function Insert(Index: TdfInteger; Item: Pointer): Boolean;
    function Last: Pointer;
    //перемещаем элемент на новую
    //позицию в списке
    function Move(CurIndex, NewIndex: TdfInteger): Boolean;
    function Remove(Item: Pointer): TdfInteger;
    procedure Pack;
    procedure Sort(Compare: TdfListSortCompare);
    procedure Assign(ListA: TdfList; AOperator: TdfListAssignOp = laCopy; ListB: TdfList = nil);
    property Capacity: TdfInteger read FCapacity write SetCapacity;
    property Count: TdfInteger read FCount write SetCount;
    property Items[Index: TdfInteger]: Pointer read Get write Put; default;
    property List: PdfPointerList read FList;
  end;

  {$ENDREGION}

  {
    Класс для работы
    с потоком передаваемым
    из внешних библиотек DLL
    (архитектурных модулей)
  }

  TdfOutStream = class(TStream)
  protected
    //сам поток
    FStream:IStream;
    //получение размера
    //потока
    procedure SetSize(const NewSize: Int64); override;
  public
    //конструктор
    constructor Create(Source : IStream);
    //деструктор
    destructor Destroy; override;
    //читаем данные
    function Read(var Buffer; Count: Longint): Longint; override;
    //пишем данные
    function Write(const Buffer; Count: Longint): Longint; override;
    //переставляем позицию
    //в потоке
    function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; override;
  end;

//====================================================================
implementation
//====================================================================

{$REGION 'TdfListEnum'}

constructor TdfListEnum.Create(AList: TdfList);
begin
  inherited Create;
  FIndex := -1;
  FList := AList;
end;

function TdfListEnum.GetCurrent: Pointer;
begin
  Result := FList[FIndex];
end;

function TdfListEnum.MoveNext: Boolean;
begin
  Result := FIndex < FList.Count - 1;
  if Result then
    Inc(FIndex);
end;

{$ENDREGION}

{$REGION 'TdfList'}

{$REGION 'Защищенные методы класса'}

//получить элемент
function TdfList.Get(Index: TdfInteger): Pointer;
begin
  //если нечего возвращаем
  //пустоту
  Result:=nil;
  if (Index < 0) or (Index >= FCount) then
    Exit;
  Result := FList^[Index];
end;

procedure TdfList.Grow;
var
  Delta: TdfInteger;
begin
  if FCapacity > 64 then
    Delta := FCapacity div 4
  else
    if FCapacity > 8 then
      Delta := 16
    else
      Delta := 4;
  SetCapacity(FCapacity + Delta);
end;

procedure TdfList.Put(Index: TdfInteger; Item: Pointer);
var
  Temp: Pointer;
begin
  if (Index < 0) or (Index >= FCount) then
    Exit;
  if Item <> FList^[Index] then
  begin
    Temp := FList^[Index];
    FList^[Index] := Item;
    if Temp <> nil then
      Notify(Temp, lnDeleted);
    if Item <> nil then
      Notify(Item, lnAdded);
  end;
end;

procedure TdfList.Notify(Ptr: Pointer; Action: TdfListNotification);
begin
  //эмм тут видимо они не дописали что-то :)
end;

procedure TdfList.SetCapacity(NewCapacity: TdfInteger);
begin
  if (NewCapacity < FCount) or (NewCapacity > MaxListSizeRV) then
    Exit;
  if NewCapacity <> FCapacity then
  begin
    ReallocMem(FList, NewCapacity * SizeOf(Pointer));
    FCapacity := NewCapacity;      
  end;
end;

procedure TdfList.SetCount(NewCount: TdfInteger);
var
  I: TdfInteger;
begin
  if (NewCount < 0) or (NewCount > MaxListSizeRV) then
    Exit;
  if NewCount > FCapacity then
    SetCapacity(NewCount);
  if NewCount > FCount then
    FillChar(FList^[FCount], (NewCount - FCount) * SizeOf(Pointer), 0)
  else
    for I := FCount - 1 downto NewCount do
      Delete(I);
  FCount := NewCount;
end;

{$ENDREGION}

//деструктор
destructor TdfList.Destroy;
begin
  Clear;
end;

//добавляем элемент
function TdfList.Add(Item: Pointer): TdfInteger;
begin
  Result := FCount;
  if Result = FCapacity then
    Grow;
  FList^[Result] := Item;
  Inc(FCount);
  if Item <> nil then
    Notify(Item, lnAdded);
end;

//очищаем весь список
procedure TdfList.Clear;
begin
  SetCount(0);
  SetCapacity(0);
end;

//переделан под возвращаемое значение
//в случае ошибки
function TdfList.Delete(Index: TdfInteger): Boolean;
var
  Temp: Pointer;
begin
  Result:=False;
  if (Index < 0) or (Index >= FCount) then
    Exit;
  Temp := Items[Index];
  Dec(FCount);
  if Index < FCount then
    System.Move(FList^[Index + 1], FList^[Index],
      (FCount - Index) * SizeOf(Pointer));
  if Temp <> nil then
    Notify(Temp, lnDeleted);
  Result:=true;
end;

//меняем местами элементы
function TdfList.Exchange(Index1, Index2: TdfInteger): Boolean;
var
  Item: Pointer;
begin
  Result:=false;
  if (Index1 < 0) or (Index1 >= FCount) then
    Exit;
  if (Index2 < 0) or (Index2 >= FCount) then
    Exit;
  Item := FList^[Index1];
  FList^[Index1] := FList^[Index2];
  FList^[Index2] := Item;
  Result:=true;
end;

//расширить
function TdfList.Expand: TdfList;
begin
  if FCount = FCapacity then
    Grow;
  Result := Self;
end;

//возвращаем первый элемент
function TdfList.First: Pointer;
begin
  Result := Get(0);
end;

//создаем нумератор
function TdfList.GetEnumerator: TdfListEnum;
begin
  Result := TdfListEnum.Create(Self);
end;

//получить элемент по указателю
function TdfList.IndexOf(Item: Pointer): TdfInteger;
begin
  Result := 0;
  while (Result < FCount) and (FList^[Result] <> Item) do
    Inc(Result);
  if Result = FCount then
    Result := -1;
end;

//вставляем значение в уже
//существующий элемент
function TdfList.Insert(Index: TdfInteger; Item: Pointer): Boolean;
begin
  Result := False;
  if (Index < 0) or (Index > FCount) then
    Exit;
  if FCount = FCapacity then
    Grow;
  if Index < FCount then
    System.Move(FList^[Index], FList^[Index + 1],
      (FCount - Index) * SizeOf(Pointer));
  FList^[Index] := Item;
  Inc(FCount);
  if Item <> nil then
    Notify(Item, lnAdded);
  Result := True;
end;

//последний элемент
function TdfList.Last: Pointer;
begin
  Result := Get(FCount - 1);
end;

//переместить элемент
function TdfList.Move(CurIndex, NewIndex: Integer): Boolean;
var
  Item: Pointer;
begin
  Result := False;
  if CurIndex <> NewIndex then
  begin
    if (NewIndex < 0) or (NewIndex >= FCount) then
      Exit;
    Item := Get(CurIndex);
    FList^[CurIndex] := nil;
    Delete(CurIndex);
    Insert(NewIndex, nil);
    FList^[NewIndex] := Item;
    Result:=true;
  end;
end;

//удаляем элемент 
function TdfList.Remove(Item: Pointer): TdfInteger;
begin
  Result := IndexOf(Item);
  if Result >= 0 then
    Delete(Result);
end;

//пакуем
procedure TdfList.Pack;
var
  PackedCount : TdfInteger;
  StartIndex : TdfInteger;
  EndIndex : TdfInteger;
begin

  if FCount = 0 then
    Exit;

  PackedCount := 0;
  StartIndex := 0;
  Repeat
    // Locate the first/next non-nil element in the list
    While (FList^[StartIndex] = Nil) and (StartIndex < FCount) do
      Inc(StartIndex);

    if StartIndex < FCount then // There is nothing more to do
      begin
        // Locate the next nil pointer
        EndIndex := StartIndex;
        While (FList^[EndIndex] <> Nil) and (EndIndex < FCount) do
          Inc(EndIndex);
        Dec(EndIndex);

        // Move this block of non-null items to the index recorded in PackedToCount:
        // If this is a contiguous non-nil block at the start of the list then
        // StartIndex and PackedToCount will be equal (and 0) so don't bother with the move.
        if StartIndex > PackedCount then
          System.Move(FList^[StartIndex],
                      FList^[PackedCount],
                      (EndIndex - StartIndex + 1) * SizeOf(Pointer));

        // Set the PackedToCount to reflect the number of items in the list
        // that have now been packed.
        Inc(PackedCount, EndIndex - StartIndex + 1);

        // Reset StartIndex to the element following EndIndex
        StartIndex := EndIndex + 1;
      end;
  Until StartIndex >= FCount;

  // Set Count so that the 'free' item
  FCount := PackedCount;
end;

//функция быстрой сортировки
procedure QuickSort(SorTdfList: PdfPointerList; L, R: TdfInteger;
  SCompare: TdfListSortCompare);
var
  I, J: TdfInteger;
  P, T: Pointer;
begin
  repeat
    I := L;
    J := R;
    P := SorTdfList^[(L + R) shr 1];
    repeat
      while SCompare(SorTdfList^[I], P) < 0 do
        Inc(I);
      while SCompare(SorTdfList^[J], P) > 0 do
        Dec(J);
      if I <= J then
      begin
        T := SorTdfList^[I];
        SorTdfList^[I] := SorTdfList^[J];
        SorTdfList^[J] := T;
        Inc(I);
        Dec(J);
      end;
    until I > J;
    if L < J then
      QuickSort(SorTdfList, L, J, SCompare);
    L := I;
  until I >= R;
end;

//сортируем масив
procedure TdfList.Sort(Compare: TdfListSortCompare);
begin
  if (FList <> nil) and (Count > 0) then
    QuickSort(FList, 0, Count - 1, Compare);
end;

function TdfList.Extract(Item: Pointer): Pointer;
var
  I: Integer;
begin
  Result := nil;
  I := IndexOf(Item);
  if I >= 0 then
  begin
    Result := Item;
    FList^[I] := nil;
    Delete(I);
    Notify(Result, lnExtracted);
  end;
end;

//присвоить
procedure TdfList.Assign(ListA: TdfList; AOperator: TdfListAssignOp; ListB: TdfList);
var
  I: Integer;
  LTemp, LSource: TdfList;
begin
  // ListB given?
  if ListB <> nil then
  begin
    LSource := ListB;
    Assign(ListA);
  end
  else
    LSource := ListA;

  // on with the show
  case AOperator of

    // 12345, 346 = 346 : only those in the new list
    laCopy:
      begin
        Clear;
        Capacity := LSource.Capacity;
        for I := 0 to LSource.Count - 1 do
          Add(LSource[I]);
      end;

    // 12345, 346 = 34 : intersection of the two lists
    laAnd:
      for I := Count - 1 downto 0 do
        if LSource.IndexOf(Items[I]) = -1 then
          Delete(I);

    // 12345, 346 = 123456 : union of the two lists
    laOr:
      for I := 0 to LSource.Count - 1 do
        if IndexOf(LSource[I]) = -1 then
          Add(LSource[I]);

    // 12345, 346 = 1256 : only those not in both lists
    laXor:
      begin
        LTemp := TdfList.Create; // Temp holder of 4 byte values
        try
          LTemp.Capacity := LSource.Count;
          for I := 0 to LSource.Count - 1 do
            if IndexOf(LSource[I]) = -1 then
              LTemp.Add(LSource[I]);
          for I := Count - 1 downto 0 do
            if LSource.IndexOf(Items[I]) <> -1 then
              Delete(I);
          I := Count + LTemp.Count;
          if Capacity < I then
            Capacity := I;
          for I := 0 to LTemp.Count - 1 do
            Add(LTemp[I]);
        finally
          LTemp.Free;
        end;
      end;

    // 12345, 346 = 125 : only those unique to source
    laSrcUnique:
      for I := Count - 1 downto 0 do
        if LSource.IndexOf(Items[I]) <> -1 then
          Delete(I);

    // 12345, 346 = 6 : only those unique to dest
    laDestUnique:
      begin
        LTemp := TdfList.Create;
        try
          LTemp.Capacity := LSource.Count;
          for I := LSource.Count - 1 downto 0 do
            if IndexOf(LSource[I]) = -1 then
              LTemp.Add(LSource[I]);
          Assign(LTemp);
        finally
          LTemp.Free;
        end;
      end;
  end;
end;

{$ENDREGION}


//получение размера
//потока
procedure TdfOutStream.SetSize(const NewSize: Int64);
begin
  FStream.SetSize(NewSize);
end;

//конструктор
constructor TdfOutStream.Create(Source: IStream);
begin
  inherited Create;
  FStream:=Source;
end;

//деструктор
destructor TdfOutStream.Destroy;
begin
  FStream := nil;
  inherited;
end;

//читаем данные
function TdfOutStream.Read(var Buffer; Count: Integer): Longint;
begin
  if FStream.Read(@Buffer, Count, @Result) <> S_OK then
    Result:=0;
end;

//пишем данные
function TdfOutStream.Write(const Buffer; Count: Integer): Longint;
begin
  if FStream.Write(@Buffer, Count, @Result) <> S_OK then
    Result:=0;
end;

//Change position in stream
function TdfOutStream.Seek(const Offset: Int64; Origin: TSeekOrigin): Int64;
begin
  Result := Seek(Offset, TSeekOrigin(Origin));
end;

end.
