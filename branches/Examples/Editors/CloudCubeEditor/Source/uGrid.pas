unit uGrid;
//Mattias Landscaper...
interface

uses
  System.Classes,
  System.SysUtils;

type
  TGridBasic = class
  private
    function GetSafeItems(const x, y: integer): TObject;
  protected
    FStorage : PPointerList;
    FTotalSize : integer;
    FCountX: integer;
    FCountY: integer;
    FOwnItems: boolean;
    function GetItems(const x, y: integer): TObject;
    procedure SetItems(const x, y: integer; const Value: TObject);
    procedure SetCountX(const Value: integer);
    procedure SetCountY(const Value: integer);
    procedure FreeStorage;
    procedure CreateStorage;
    procedure AssertInRange(const x,y : integer);
    procedure SetOwnItems(const Value: boolean); virtual;
  public
    constructor Create(const CountX, CountY : integer; OwnItems : boolean=false);
    destructor Destroy; override;
    function MapXYToID(const x,y : integer) : integer;
    procedure MapIDToXY(const id : integer; var x,y : integer);
    procedure FreeItems; virtual;
    procedure Delete(const x,y : integer);
    procedure Clear; virtual;
    function ItemsSet : integer;
    function InRange(const x,y : integer) : boolean;
    procedure CopyFrom(Grid : TGridBasic); virtual;
    property Items[const x,y : integer] : TObject read GetItems write SetItems; default;
    property CountX : integer read FCountX write SetCountX;
    property CountY : integer read FCountY write SetCountY;
    property TotalSize : integer read FTotalSize;

    // If OwnItems is true then items will be freed when grid is destroyed or
    // when storage is freed
    property OwnItems : boolean read FOwnItems write SetOwnItems;
  end;

  TGrid = class(TGridBasic)
  public
    function IndexOf(const Item : TObject) : integer;
    function LocationOf(const Item : TObject; var x,y : integer) : boolean;
    property SafeItems[const x,y : integer] : TObject read GetSafeItems;
    property Storage: PPointerList read FStorage;
  end;

  PSingleList = ^TSingleList;
  TSingleList = array[0..MaxListSize - 1] of single;
  TInterpolationMethod = (imLinear, imCos);

  TSingleGrid = class(TGridBasic)
  private
    function GetSingleItems(const x, y: integer): single;
    procedure SetSingleItems(const x, y: integer; const Value: single);
    function GetStorage: PSingleList;
    function GetSingleItemsSafe(const x, y: integer): single;
    function GetInterpolatedItems(const x, y: single; InterpolationMethod : TInterpolationMethod): single;
    function GetInterpolatedCosItems(const x, y: single): single;
    function GetInterpolatedLinearItems(const x, y: single): single;
  protected
    procedure SetOwnItems(const Value: boolean); override;
  public
    function InterpolateCos(a, b, x: single): single;
    function InterpolateLinear(a, b, x: single): single;
    function InterpolationCubic(v0, v1, v2, v3, x: single): single;
    procedure FreeItems; override;
    procedure Clear; override;
    procedure AntiAlias(BleedValue : single);
    procedure AntiAliasSweep;
    procedure AntiAliasSweepRight;
    procedure AntiAliasSweepDown;
    property SafeItems[const x,y : integer] : single read GetSingleItemsSafe;
    property Items[const x,y : integer] : single read GetSingleItems write SetSingleItems; default;
    property InterpolatedLinear[const x,y : single] : single read GetInterpolatedLinearItems;
    property InterpolatedCos[const x,y : single] : single read GetInterpolatedCosItems;
    property Storage: PSingleList read GetStorage;
  end;

//===========================================================================
implementation
//===========================================================================

{ TGridBasic }

procedure TGridBasic.AssertInRange(const x, y: integer);
begin
  Assert(x>=0,'X must be above or equal to zero!');
  Assert(y>=0,'Y must be above or equal to zero!');

  Assert(x<CountX,'X must be below CountX!');
  Assert(y<CountY,'Y must be below CountY!');
end;

constructor TGridBasic.Create(const CountX, CountY: integer; OwnItems : boolean=false);
begin
  FreeStorage;
  FCountX := CountX;
  FCountY := CountY;
  CreateStorage;
  self.OwnItems := OwnItems;
end;

procedure TGridBasic.CreateStorage;
var
  i : integer;
begin
  FTotalSize := FCountX * FCountY;

  Assert(FStorage=nil,'You must free storage before new storage can be created!');
  GetMem(FStorage, TotalSize * SizeOf(pointer));

  for i := 0 to TotalSize-1 do
    FStorage^[i] := nil;
end;

procedure TGridBasic.FreeStorage;
begin
  if Assigned(FStorage) then
  begin
    if FOwnItems then
      FreeItems;
    FreeMem(FStorage);
  end;
  FStorage := nil;
end;

function TGridBasic.GetItems(const x, y: integer): TObject;
begin
  AssertInRange(x,y);
  Result := FStorage^[MapXYToID(x,y)];
end;

procedure TGridBasic.SetCountX(const Value: integer);
begin
  FreeStorage;
  FCountX := Value;
  CreateStorage;
end;

procedure TGridBasic.SetCountY(const Value: integer);
begin
  FreeStorage;
  FCountY := Value;
  CreateStorage;
end;

procedure TGridBasic.SetItems(const x, y: integer; const Value: TObject);
begin
  AssertInRange(x,y);

  FStorage^[MapXYToID(x,y)] := Value;
end;

function TGridBasic.MapXYToID(const x, y: integer): integer;
begin
  result := x + y*CountX;
end;

procedure TGridBasic.MapIDToXY(const id: integer; var x, y: integer);
begin
  x := id mod CountX;
  y := id div CountX;
end;

procedure TGridBasic.Delete(const x, y: integer);
begin
  Items[x,y] := nil;
end;

procedure TGridBasic.FreeItems;
var
  i : integer;
begin
  for i := 0 to FTotalSize-1 do
  begin
    if Assigned(FStorage^[i]) then
    begin
      TObject(FStorage^[i]).Free;
      FStorage^[i] := nil;
    end;
  end;
end;

function TGridBasic.ItemsSet: integer;
var
  i : integer;
begin
  result := 0;

  for i := 0 to FTotalSize-1 do
    if Assigned(FStorage^[i]) then
      inc(result);
end;

destructor TGridBasic.Destroy;
begin
  FreeStorage;
  inherited;
end;

procedure TGridBasic.SetOwnItems(const Value: boolean);
begin
  FOwnItems := Value;
end;

function TGridBasic.InRange(const x, y: integer): boolean;
begin
  result := (x>=0) and (y>=0) and (x<CountX) and (y<CountY);
end;

function TGridBasic.GetSafeItems(const x, y: integer): TObject;
begin
  if InRange(x,y) then
    result := GetItems(x,y)
  else
    result := nil;
end;

procedure TGridBasic.Clear;
var
  i : integer;
begin
  for i := 0 to FTotalSize-1 do
  begin
    if Assigned(FStorage^[i]) then
    begin
      if OwnItems then
        TObject(FStorage^[i]).Free;

      FStorage^[i] := nil;
    end;
  end;
end;

procedure TGridBasic.CopyFrom(Grid: TGridBasic);
var
  i : integer;
begin
  Assert((CountX=Grid.CountX) and (CountY=Grid.CountY),'Grids must be of the same dimensions!');

  for i := 0 to TotalSize-1 do
    FStorage^[i] := Grid.FStorage^[i];
end;

{ TGrid }

function TGrid.IndexOf(const Item: TObject): integer;
var
  i : integer;
begin
  for i := 0 to TotalSize-1 do
    if FStorage^[i]=Item then
    begin
      result := i;
      exit;
    end;

  result := -1;
end;

function TGrid.LocationOf(const Item: TObject; var x, y: integer): boolean;
var
  i : integer;
begin
  i := IndexOf(Item);
  result := false;//ilh
  if i = -1 then
    result := false
  else
    MapIDToXY(i, x,y);
end;

{ TSingleGrid }

procedure TSingleGrid.AntiAlias(BleedValue : single);
var
  SingleGrid : TSingleGrid;
  x, y, gx, gy : integer;
  ValueSum : single;
  PartSum : single;
begin
    SingleGrid := TSingleGrid.Create(FCountX, FCountY);
  try
//    SingleGrid := TSingleGrid.Create(FCountX, FCountY);

    for x := 0 to FCountX-1 do
      for y := 0 to FCountY-1 do
      begin
        PartSum := 1;
        ValueSum := Items[x,y];

        for gx := x-1 to x+1 do
          for gy := y-1 to y+1 do
          begin
            if InRange(gx, gy) then
            begin
              ValueSum := ValueSum + BleedValue * Items[gx, gy];
              PartSum := PartSum + BleedValue;
            end;
          end;

        // Average the ValueSum over the parts added
        SingleGrid[x,y] := ValueSum / PartSum;
      end;

    // Copy the info to this grid
    for x := 0 to FCountX-1 do
      for y := 0 to FCountY-1 do
        Items[x,y] := SingleGrid[x,y];//}

  finally
    SingleGrid.Free;
  end;
end;

procedure TSingleGrid.Clear;
begin
  // These two are the same in a singlegrid
  FreeItems;
end;

procedure TSingleGrid.FreeItems;
var
  i : integer;
  f : single;
begin
  // They're all singles, so set them to zero
  f := 0;
  for i := 0 to FTotalSize-1 do
    FStorage^[i] := TObject(f);
end;


function TSingleGrid.InterpolateCos(a, b, x: single): single;
var
  ft,f : single;
begin
  ft := x * Pi;
  f := (1-cos(ft))*0.5;

  result := a*(1-f)+b*f;
end;

function TSingleGrid.InterpolateLinear(a, b, x: single): single;
begin
  result := a*(1-x)+b*x;
end;

function TSingleGrid.InterpolationCubic(v0, v1, v2, v3, x: single): single;
var
  P, Q, R, S : single;
begin
	P := (v3 - v2) - (v0 - v1);
	Q := (v0 - v1) - P;
	R := v2 - v0;
	S := v1;

	result := P*x*x*x + Q*x*x + R*x + S;
end;

function TSingleGrid.GetInterpolatedItems(const x, y: single;
  InterpolationMethod: TInterpolationMethod): single;
var
  intX, intY : integer;
  fractX, fractY : single;
  v1, v2, v3, v4, i1, i2 : single;
begin
result :=0;//ilh
  intX := trunc(x); fractX := x-intX;
  intY := trunc(y); fractY := y-intY;

  v1 := SafeItems[intX, intY];
  v2 := SafeItems[intX+1, intY];
  v3 := SafeItems[intX, intY+1];
  v4 := SafeItems[intX+1, intY+1];

  if InterpolationMethod = imCos then
  begin
    i1 := InterpolateCos(v1, v2, fractX);
    i2 := InterpolateCos(v3, v4, fractX);

    result := InterpolateCos(i1, i2, fractY);
  end else
  if InterpolationMethod = imLinear then
  begin
    i1 := InterpolateLinear(v1, v2, fractX);
    i2 := InterpolateLinear(v3, v4, fractX);

    result := InterpolateLinear(i1, i2, fractY);
  end else
    Assert(false,'Interpolation method not supported!');
end;

function TSingleGrid.GetInterpolatedCosItems(const x, y: single): single;
begin
  result := GetInterpolatedItems(x,y,imCos);
end;

function TSingleGrid.GetInterpolatedLinearItems(const x,
  y: single): single;
begin
  result := GetInterpolatedItems(x,y,imLinear);
end;

function TSingleGrid.GetSingleItems(const x, y: integer): single;
begin
  result := single(GetItems(x,y));
end;

function TSingleGrid.GetSingleItemsSafe(const x, y: integer): single;
begin
  if InRange(x,y) then
    result := GetSingleItems(x,y)
  else
    result := 0;
end;

function TSingleGrid.GetStorage: PSingleList;
begin
  result := PSingleList(FStorage);
end;

procedure TSingleGrid.SetOwnItems(const Value: boolean);
begin
  //inherited;
  // Can't own singles, since we can't free them!
  FOwnItems := false;
end;

procedure TSingleGrid.SetSingleItems(const x, y: integer;
  const Value: single);
begin
  SetItems(x,y, TObject(Value));
end;

procedure TSingleGrid.AntiAliasSweep;
begin
  AntiAliasSweepRight;
  AntiAliasSweepDown;
end;

procedure TSingleGrid.AntiAliasSweepRight;
var
  CurrentHeight, TargetHeight : single;
  x, y, StripLength, i  : integer;
  TempGrid : TSingleGrid;
begin
  TempGrid := TSingleGrid.Create(FCountX, FCountY);
  TargetHeight :=1;//ilh
  try
    for y := 0 to CountY-1 do
    begin
      x := 0;
      while x<CountX do
      begin
        // Find the strips of identical values
        // interpolate them from this value to the last one
        CurrentHeight := Items[x,y];

        // Copy this height without alteration
        TempGrid[x,y] := CurrentHeight;
        StripLength := 1;
        while x+StripLength<CountX do
        begin
          TargetHeight := Items[x+StripLength,y];
          if TargetHeight<>CurrentHeight then
            break;
          inc(StripLength);
        end;
        for i := 0 to StripLength - 1 do
          TempGrid[x+i,y] := InterpolateLinear(CurrentHeight, TargetHeight, i/StripLength);
        x := x + StripLength;
      end;
    end;
    CopyFrom(TempGrid);
  finally
    TempGrid.Free;
  end;
end;

procedure TSingleGrid.AntiAliasSweepDown;
var
  CurrentHeight, TargetHeight : single;
  x, y, StripLength, i  : integer;
  TempGrid : TSingleGrid;
begin
  TempGrid := TSingleGrid.Create(FCountX, FCountY);
  TargetHeight :=1;//ilh
  try
    for x := 0 to CountX-1 do
    begin
      y := 0;
      while y<CountY do
      begin
        // Find the strips of identical values
        // interpolate them from this value to the last one
        CurrentHeight := Items[x,y];
        // Copy this height without alteration
        TempGrid[x,y] := CurrentHeight;
        StripLength := 1;
        while y+StripLength<CountY do
        begin
          TargetHeight := Items[x,y+StripLength];
          if TargetHeight<>CurrentHeight then
            break;
          inc(StripLength);
        end;
        for i := 0 to StripLength - 1 do
          TempGrid[x,y+i] := InterpolateLinear(CurrentHeight, TargetHeight, i/StripLength);
        y := y + StripLength;
      end;
    end;
    CopyFrom(TempGrid);
  finally
    TempGrid.Free;
  end;
end;

end.
