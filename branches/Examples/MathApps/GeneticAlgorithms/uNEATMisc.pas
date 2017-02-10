unit uNEATMisc;

interface

uses
  System.Classes;

type
  TSortedList = class(TList)
  private
    FDuplicates: TDuplicates;
    FSorted: boolean;
    function SortedFind(const Key: cardinal; var Index: Integer): Boolean;
    function UnsortedFind(const Key: cardinal; var Index: Integer): Boolean;
  public
    constructor Create(ASorted : boolean=true);

    function Add(Item : Pointer) : integer;
    function Find(const Key: cardinal; var Index: Integer): Boolean;
    function GetKeyForItem(Item : Pointer) : cardinal; virtual; abstract;
    function GetItemForKey(const Key: cardinal) : pointer;
  published
    property Sorted : boolean read FSorted;
    property Duplicates: TDuplicates read FDuplicates write FDuplicates;
  end;


implementation

{ TSortedList }

function TSortedList.Add(Item: Pointer) : integer;
var
  Key : cardinal;
begin
  // Is it sorted?
  if not FSorted then
  begin
    result := inherited Add(Item);
    exit;
  end;

  // If it's a sorted list, nil can't be added!
  Assert(Assigned(Item),'Sorted list can''t contain nil!');

  // Quick method for empty list
  if (Count=0) then
  begin
    result := inherited Add(Item);
    exit;
  end;

  // Retrieve the key of the sought item
  Key := GetKeyForItem(Item);

  // Quick method for sorted lists
  if GetKeyForItem(Items[Count-1])<Key then
  begin
    result := inherited Add(Item);
    exit;
  end;

  // We need to do a sorted insert
  if Find(Key, Result) then
    case Duplicates of
      dupIgnore: Exit;
      dupError: Error('Duplicates are not allowed!', 0);
    end;

  Insert(result, Item);
end;

constructor TSortedList.Create(ASorted: boolean);
begin
  inherited Create;
  FSorted := ASorted;
  FDuplicates := dupError;
end;

function TSortedList.Find(const Key: cardinal; var Index: Integer): Boolean;
begin
  if FSorted then
    result := SortedFind(Key, Index)
  else
    result := UnsortedFind(Key, Index);
end;

function TSortedList.GetItemForKey(const Key: cardinal): pointer;
var
  p : integer;
begin
  if Find(Key, p) then
    result := Items[p]
  else
    result := nil;
end;

function TSortedList.SortedFind(const Key: cardinal;
  var Index: Integer): Boolean;
var
  L, H, I, C: Integer;
  ThisKey : cardinal;
begin
  Result := False;
  L := 0;
  H := Count - 1;

  while L <= H do
  begin
    I := (L + H) shr 1;

    ThisKey := GetKeyForItem(Items[i]);

    if ThisKey=Key then
      C:=0
    else if Key<ThisKey then
      C := 1
    else
      C := -1;

    if C < 0 then L := I + 1 else
    begin
      H := I - 1;
      if C = 0 then
      begin
        Result := True;
        if Duplicates <> dupAccept then L := I;
      end;
    end;
  end;
  Index := L;
end;

function TSortedList.UnsortedFind(const Key: cardinal;
  var Index: Integer): Boolean;
var
  i : integer;
begin
  Index := -1;
  for i := 0 to Count-1 do
  begin
    if GetKeyForItem(Items[i]) = Key then
    begin
      Index := i;
      result := true;
      exit;
    end;
  end;
  result := False;
end;

end.
