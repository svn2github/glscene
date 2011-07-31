type
  TRBColor = (clRed, clBlack);
  TWHMRBNode = class
   Key: Double;
   Left, Right, Parent, Twin: TWHMRBNode;
   Color: TRBColor;
   Value: Integer;
  end;

  TDoubleCompareFunc = function(const Item1, Item2: Double): Integer;
  TIntegerCompareFunc = function(const Item1, Item2: Integer): Integer;
  TIntegerCompareFunc_ = function(const Item1, Item2: Integer): Boolean;
  TForEachProc = procedure (AKey: Double; AValue: Integer; out AContinue: Boolean);

  TVertexHashMap = class
  public
    { Private Declarations }
    FRoot: TWHMRBNode;
    FLeftmost: TWHMRBNode;
    FRightmost: TWHMRBNode;
    FLastFound: TWHMRBNode;
    FLastNode: TWHMRBNode;
    FCount: Integer;
    FKeyCompareFunc: TDoubleCompareFunc;
    FDuplicateKeys: Boolean;
    FValueCompareFunc: TIntegerCompareFunc_;
    FOnChange: TNotifyEvent;

    function FindNode(const key: Double): TWHMRBNode;
    procedure RotateLeft(var x: TWHMRBNode);
    procedure RotateRight(var x: TWHMRBNode);
    function Minimum(var x: TWHMRBNode): TWHMRBNode;
    function Maximum(var x: TWHMRBNode): TWHMRBNode;
    function GetFirst: Double;
    function GetLast: Double;
    procedure SetDuplicateKeys(Value: Boolean);
    class procedure FastErase(x: TWHMRBNode);

  public
    { Public Declarations }
    constructor Create(KeyCompare: TDoubleCompareFunc; ValueCompare: TIntegerCompareFunc_);
    destructor Destroy; override;

    procedure Clear;
    {: Find value by key. }
    function Find(const key: Double; out Value: Integer): Boolean;
    function NextKey(var key: Double; out Value: Integer): Boolean;
    function PrevKey(var key: Double; out Value: Integer): Boolean;
    function NextDublicate(out Value: Integer): Boolean;
    procedure Add(const key: Double; const Value: Integer);
    procedure Delete(const key: Double);
    procedure ForEach(AProc: TForEachProc);
    property Count: Integer read FCount;
    property First: Double read GetFirst;
    property Last: Double read GetLast;
    property DuplicateKeys: Boolean read FDuplicateKeys write SetDuplicateKeys;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TIIRBNode = class
   Key: Integer;
   Left, Right, Parent, Twin: TIIRBNode;
   Color: TRBColor;
   Value: Integer;
  end;

  TIntIntRBT = class
  public
    { Private Declarations }
    FRoot: TIIRBNode;
    FLeftmost: TIIRBNode;
    FRightmost: TIIRBNode;
    FLastFound: TIIRBNode;
    FLastNode: TIIRBNode;
    FCount: Integer;
    FKeyCompareFunc: TIntegerCompareFunc;
    FDuplicateKeys: Boolean;
    FValueCompareFunc: TIntegerCompareFunc_;
    FOnChange: TNotifyEvent;

    function FindNode(const key: Integer): TIIRBNode;
    procedure RotateLeft(var x: TIIRBNode);
    procedure RotateRight(var x: TIIRBNode);
    function Minimum(var x: TIIRBNode): TIIRBNode;
    function Maximum(var x: TIIRBNode): TIIRBNode;
    function GetFirst: Integer;
    function GetLast: Integer;
    procedure SetDuplicateKeys(Value: Boolean);
    class procedure FastErase(x: TIIRBNode);

  public
    { Public Declarations }
    constructor Create(KeyCompare: TIntegerCompareFunc; ValueCompare: TIntegerCompareFunc_);
    destructor Destroy; override;

    procedure Clear;
    {: Find value by key. }
    function Find(const key: Integer; out Value: Integer): Boolean;
    function NextKey(var key: Integer; out Value: Integer): Boolean;
    function PrevKey(var key: Integer; out Value: Integer): Boolean;
    function NextDublicate(out Value: Integer): Boolean;
    procedure Add(const key: Integer; const Value: Integer);
    procedure Delete(const key: Integer);
    procedure ForEach(AProc: TForEachProc);
    property Count: Integer read FCount;
    property First: Integer read GetFirst;
    property Last: Integer read GetLast;
    property DuplicateKeys: Boolean read FDuplicateKeys write SetDuplicateKeys;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

constructor TVertexHashMap.Create(KeyCompare: TDoubleCompareFunc; ValueCompare: TIntegerCompareFunc_);
begin
  inherited Create;
  Assert(Assigned(KeyCompare));
  FKeyCompareFunc := KeyCompare;
  FValueCompareFunc := ValueCompare;
  FRoot := nil;
  FLeftmost := nil;
  FRightmost := nil;
  FDuplicateKeys := Assigned(ValueCompare);
end;

destructor TVertexHashMap.Destroy;
begin
  Clear;
  inherited Destroy;
end;

class procedure TVertexHashMap.FastErase(x: TWHMRBNode);
var
  y: TWHMRBNode;
begin
  if (x.left <> nil) then
    FastErase(x.left);
  if (x.right <> nil) then
    FastErase(x.right);
  repeat
    y := x;
    x := x.Twin;
    y.Destroy;
  until x = nil;
end;

procedure TVertexHashMap.Clear;
begin
  if (FRoot <> nil) then
    FastErase(FRoot);
  FRoot := nil;
  FLeftmost := nil;
  FRightmost := nil;
  FCount := 0;
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

function TVertexHashMap.Find(const key: Double; out Value: Integer): Boolean;
begin
  FLastFound := FindNode(key);
  Result := Assigned(FLastFound);
  if Result then
    Value := FLastFound.Value;
end;

function TVertexHashMap.FindNode(const key: Double): TWHMRBNode;
var
  cmp: integer;
begin
  Result := FRoot;
  while (Result <> nil) do
  begin
    cmp := FKeyCompareFunc(Result.Key, key);
    if cmp < 0 then
    begin
      Result := Result.right;
    end
    else if cmp > 0 then
    begin
      Result := Result.left;
    end
    else
    begin
      break;
    end;
  end;
end;

function TVertexHashMap.NextDublicate(out Value: Integer): Boolean;
begin
  if Assigned(FLastFound) then
  begin
    if Assigned(FLastFound.Twin) then
    begin
      FLastFound := FLastFound.Twin;
      Value := FLastFound.Value;
      result := True;
      Exit;
    end;
  end;
  Result := False;
end;

procedure TVertexHashMap.RotateLeft(var x: TWHMRBNode);
var
  y: TWHMRBNode;
begin
  y := x.right;
  x.right := y.left;
  if (y.left <> nil) then
  begin
    y.left.parent := x;
  end;
  y.parent := x.parent;
  if (x = FRoot) then
  begin
    FRoot := y;
  end
  else if (x = x.parent.left) then
  begin
    x.parent.left := y;
  end
  else
  begin
    x.parent.right := y;
  end;
  y.left := x;
  x.parent := y;
end;

procedure TVertexHashMap.RotateRight(var x: TWHMRBNode);
var
  y: TWHMRBNode;
begin
  y := x.left;
  x.left := y.right;
  if (y.right <> nil) then
  begin
    y.right.parent := x;
  end;
  y.parent := x.parent;
  if (x = FRoot) then
  begin
    FRoot := y;
  end
  else if (x = x.parent.right) then
  begin
    x.parent.right := y;
  end
  else
  begin
    x.parent.left := y;
  end;
  y.right := x;
  x.parent := y;
end;

function TVertexHashMap.Minimum(var x: TWHMRBNode): TWHMRBNode;
begin
  Result := x;
  while (Result.left <> nil) do
    Result := Result.left;
end;

function TVertexHashMap.Maximum(var x: TWHMRBNode): TWHMRBNode;
begin
  Result := x;
  while (Result.right <> nil) do
    Result := Result.right;
end;

procedure TVertexHashMap.Add(const key: Double; const Value: Integer);
var
  x, y, z, zpp: TWHMRBNode;
  cmp: Integer;
begin
  z := TWHMRBNode.Create;

  { Initialize fields in new node z }
  z.Key := key;
  z.left := nil;
  z.right := nil;
  z.color := clRed;
  z.Value := Value;

  { Maintain FLeftmost and FRightmost nodes }
  if ((FLeftmost = nil) or (FKeyCompareFunc(key, FLeftmost.Key) < 0)) then
  begin
    FLeftmost := z;
  end;
  if ((FRightmost = nil) or (FKeyCompareFunc(FRightmost.Key, key) < 0)) then
  begin
    FRightmost := z;
  end;

  {: Insert node z }
  y := nil;
  x := FRoot;
  while (x <> nil) do
  begin
    y := x;
    cmp := FKeyCompareFunc(key, x.Key);
    if cmp < 0 then
      x := x.left
    else if cmp > 0 then
      x := x.right
    else
    begin
      {: Key already exists in tree. }
      if FDuplicateKeys then
      begin
        {: Check twins chain for value dublicate. }
        repeat
          if FValueCompareFunc(Value, x.Value) then
          begin
            y := nil;
            break;
          end;
          y := x;
          x := x.Twin;
        until x = nil;
        if Assigned(y) then
        begin
          {: Add dublicate key to end of twins chain. }
          y.Twin := z;
          Inc(FCount);
          if Assigned(FOnChange) then
            FOnChange(Self);
          exit;
        end;
        {: Value already exists in tree. }
      end;
      z.Destroy;
      //a jzombi: memory leak: if we don't put it in the tree, we shouldn't hold it in the memory
      exit;
    end;
  end;
  z.parent := y;
  if (y = nil) then
  begin
    FRoot := z;
  end
  else if (FKeyCompareFunc(key, y.Key) < 0) then
  begin
    y.left := z;
  end
  else
  begin
    y.right := z;
  end;

  { Rebalance tree }
  while ((z <> FRoot) and (z.parent.color = clRed)) do
  begin
    zpp := z.parent.parent;
    if (z.parent = zpp.left) then
    begin
      y := zpp.right;
      if ((y <> nil) and (y.color = clRed)) then
      begin
        z.parent.color := clBlack;
        y.color := clBlack;
        zpp.color := clRed;
        z := zpp;
      end
      else
      begin
        if (z = z.parent.right) then
        begin
          z := z.parent;
          rotateLeft(z);
        end;
        z.parent.color := clBlack;
        zpp.color := clRed;
        rotateRight(zpp);
      end;
    end
    else
    begin
      y := zpp.left;
      if ((y <> nil) and (y.color = clRed)) then
      begin
        z.parent.color := clBlack;
        y.color := clBlack;
        zpp.color := clRed; //c jzombi: zpp.color := clRed;
        z := zpp;
      end
      else
      begin
        if (z = z.parent.left) then
        begin
          z := z.parent;
          rotateRight(z);
        end;
        z.parent.color := clBlack;
        zpp.color := clRed; //c jzombi: zpp.color := clRed;
        rotateLeft(zpp);
      end;
    end;
  end;
  FRoot.color := clBlack;
  Inc(FCount);
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TVertexHashMap.Delete(const key: Double);
var
  w, x, y, z, x_parent: TWHMRBNode;
  tmpcol: TRBColor;
begin
  z := FindNode(key);
  if z = nil then
    exit;

  y := z;
  x := nil;
  x_parent := nil;

  if (y.left = nil) then
  begin { z has at most one non-null child. y = z. }
    x := y.right; { x might be null. }
  end
  else
  begin
    if (y.right = nil) then
    begin { z has exactly one non-null child. y = z. }
      x := y.left; { x is not null. }
    end
    else
    begin
      { z has two non-null children.  Set y to }
      y := y.right; {   z's successor.  x might be null. }
      while (y.left <> nil) do
      begin
        y := y.left;
      end;
      x := y.right;
    end;
  end;

  if (y <> z) then
  begin
    { "copy y's sattelite data into z" }
    { relink y in place of z.  y is z's successor }
    z.left.parent := y;
    y.left := z.left;
    if (y <> z.right) then
    begin
      x_parent := y.parent;
      if (x <> nil) then
      begin
        x.parent := y.parent;
      end;
      y.parent.left := x; { y must be a child of left }
      y.right := z.right;
      z.right.parent := y;
    end
    else
    begin
      x_parent := y;
    end;
    if (FRoot = z) then
    begin
      FRoot := y;
    end
    else if (z.parent.left = z) then
    begin
      z.parent.left := y;
    end
    else
    begin
      z.parent.right := y;
    end;
    y.parent := z.parent;
    tmpcol := y.color;
    y.color := z.color;
    z.color := tmpcol;
    y := z;
    { y now points to node to be actually deleted }
  end
  else
  begin { y = z }
    x_parent := y.parent;
    if (x <> nil) then
    begin
      x.parent := y.parent;
    end;
    if (FRoot = z) then
    begin
      FRoot := x;
    end
    else
    begin
      if (z.parent.left = z) then
      begin
        z.parent.left := x;
      end
      else
      begin
        z.parent.right := x;
      end;
    end;
    if (FLeftmost = z) then
    begin
      if (z.right = nil) then
      begin { z.left must be null also }
        FLeftmost := z.parent;
      end
      else
      begin
        FLeftmost := minimum(x);
      end;
    end;
    if (FRightmost = z) then
    begin
      if (z.left = nil) then
      begin { z.right must be null also }
        FRightmost := z.parent;
      end
      else
      begin { x == z.left }
        FRightmost := maximum(x);
      end;
    end;
  end;

  { Rebalance tree }
  if (y.color = clBlack) then
  begin
    while ((x <> FRoot) and ((x = nil) or (x.color = clBlack))) do
    begin
      if (x = x_parent.left) then
      begin
        w := x_parent.right;
        if (w.color = clRed) then
        begin
          w.color := clBlack;
          x_parent.color := clRed;
          rotateLeft(x_parent);
          w := x_parent.right;
        end;
        if (((w.left = nil) or
          (w.left.color = clBlack)) and
          ((w.right = nil) or
          (w.right.color = clBlack))) then
        begin
          w.color := clRed;
          x := x_parent;
          x_parent := x_parent.parent;
        end
        else
        begin
          if ((w.right = nil) or (w.right.color = clBlack)) then
          begin
            w.left.color := clBlack;
            w.color := clRed;
            rotateRight(w);
            w := x_parent.right;
          end;
          w.color := x_parent.color;
          x_parent.color := clBlack;
          if (w.right <> nil) then
          begin
            w.right.color := clBlack;
          end;
          rotateLeft(x_parent);
          x := FRoot; { break; }
        end
      end
      else
      begin
        { same as above, with right <. left. }
        w := x_parent.left;
        if (w.color = clRed) then
        begin
          w.color := clBlack;
          x_parent.color := clRed;
          rotateRight(x_parent);
          w := x_parent.left;
        end;
        if (((w.right = nil) or
          (w.right.color = clBlack)) and
          ((w.left = nil) or
          (w.left.color = clBlack))) then
        begin
          w.color := clRed;
          x := x_parent;
          x_parent := x_parent.parent;
        end
        else
        begin
          if ((w.left = nil) or (w.left.color = clBlack)) then
          begin
            w.right.color := clBlack;
            w.color := clRed;
            rotateLeft(w);
            w := x_parent.left;
          end;
          w.color := x_parent.color;
          x_parent.color := clBlack;
          if (w.left <> nil) then
          begin
            w.left.color := clBlack;
          end;
          rotateRight(x_parent);
          x := FRoot; { break; }
        end;
      end;
    end;
    if (x <> nil) then
    begin
      x.color := clBlack;
    end;
  end;
  while Assigned(y.Twin) do
  begin
    z := y;
    y := y.Twin;
    z.Destroy;
  end;
  y.Destroy;
  Dec(FCount);
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

function TVertexHashMap.NextKey(var key: Double; out Value: Integer): Boolean;
var
  x, y: TWHMRBNode;
begin
  if Assigned(FLastNode) and (FKeyCompareFunc(FLastNode.Key, key) = 0) then
    x := FLastNode
  else
    x := FindNode(key);
  if x = nil then
  begin
    Result := False;
    exit;
  end;
  if (x.right <> nil) then
  begin
    x := x.right;
    while (x.left <> nil) do
    begin
      x := x.left;
    end;
  end
  else if (x.parent <> nil) then
  begin
    y := x.parent;
    while Assigned(y) and (x = y.right) do
    begin
      x := y;
      y := y.parent;
    end;
    if (x.right <> y) then
      x := y;
  end
  else
    x := FRoot;
  if x = nil then
  begin
    Result := False;
    exit;
  end;
  key := x.Key;
  FLastNode := x;
  Value := x.Value;
  Result := True;
end;

function TVertexHashMap.PrevKey(var key: Double; out Value: Integer): Boolean;
var
  x, y: TWHMRBNode;
begin
  if Assigned(FLastNode) and (FKeyCompareFunc(FLastNode.Key, key) = 0) then
    x := FLastNode
  else
    x := FindNode(key);
  if x = nil then
  begin
    Result := False;
    exit;
  end;
  if (x.left <> nil) then
  begin
    y := x.left;
    while (y.right <> nil) do
    begin
      y := y.right;
    end;
    x := y;
  end
  else if (x.parent <> nil) then
  begin
    y := x.parent;
    while (x = y.left) do
    begin
      x := y;
      y := y.parent;
    end;
    x := y;
  end
  else
    x := FRoot;
  if x = nil then
  begin
    Result := False;
    exit;
  end;
  key := x.Key;
  FLastNode := x;
  Value := x.Value;
  Result := True;
end;

function TVertexHashMap.GetFirst: Double;
begin
  Result := FLeftMost.Key;
end;

function TVertexHashMap.GetLast: Double;
begin
  Result := FRightMost.Key;
end;

procedure TVertexHashMap.ForEach(AProc: TForEachProc);
var
  x, y, z: TWHMRBNode;
  cont: Boolean;
begin
  if Assigned(FLeftMost) then
  begin
    x := FLeftMost;
    repeat
      z := x;
      repeat
        AProc(z.Key, z.Value, cont);
        if not cont then
          exit;
        z := z.Twin;
      until z = nil;
      // Next node
      if (x.right <> nil) then
      begin
        x := x.right;
        while (x.left <> nil) do
        begin
          x := x.left;
        end;
      end
      else if (x.parent <> nil) then
      begin
        y := x.parent;
        while (x = y.right) do
        begin
          x := y;
          y := y.parent;
        end;
        if (x.right <> y) then
          x := y;
      end
      else
        x := FRoot;
    until x = FRightMost;
    if cont and (FLeftMost <> FRightMost) then
      AProc(FRightMost.Key, FRightMost.Value, cont);
  end;
end;

procedure TVertexHashMap.SetDuplicateKeys(Value: Boolean);
begin
  if Value and Assigned(FValueCompareFunc) then
    FDuplicateKeys := True
  else
    FDuplicateKeys := False;
end;

//===============================================================

constructor TIntIntRBT.Create(KeyCompare: TIntegerCompareFunc; ValueCompare: TIntegerCompareFunc_);
begin
  inherited Create;
  Assert(Assigned(KeyCompare));
  FKeyCompareFunc := KeyCompare;
  FValueCompareFunc := ValueCompare;
  FRoot := nil;
  FLeftmost := nil;
  FRightmost := nil;
  FDuplicateKeys := Assigned(ValueCompare);
end;

destructor TIntIntRBT.Destroy;
begin
  Clear;
  inherited Destroy;
end;

class procedure TIntIntRBT.FastErase(x: TIIRBNode);
var
  y: TIIRBNode;
begin
  if (x.left <> nil) then
    FastErase(x.left);
  if (x.right <> nil) then
    FastErase(x.right);
  repeat
    y := x;
    x := x.Twin;
    y.Destroy;
  until x = nil;
end;

procedure TIntIntRBT.Clear;
begin
  if (FRoot <> nil) then
    FastErase(FRoot);
  FRoot := nil;
  FLeftmost := nil;
  FRightmost := nil;
  FCount := 0;
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

function TIntIntRBT.Find(const key: Integer; out Value: Integer): Boolean;
begin
  FLastFound := FindNode(key);
  Result := Assigned(FLastFound);
  if Result then
    Value := FLastFound.Value;
end;

function TIntIntRBT.FindNode(const key: Integer): TIIRBNode;
var
  cmp: integer;
begin
  Result := FRoot;
  while (Result <> nil) do
  begin
    cmp := FKeyCompareFunc(Result.Key, key);
    if cmp < 0 then
    begin
      Result := Result.right;
    end
    else if cmp > 0 then
    begin
      Result := Result.left;
    end
    else
    begin
      break;
    end;
  end;
end;

function TIntIntRBT.NextDublicate(out Value: Integer): Boolean;
begin
  if Assigned(FLastFound) then
  begin
    if Assigned(FLastFound.Twin) then
    begin
      FLastFound := FLastFound.Twin;
      Value := FLastFound.Value;
      result := True;
      Exit;
    end;
  end;
  Result := False;
end;

procedure TIntIntRBT.RotateLeft(var x: TIIRBNode);
var
  y: TIIRBNode;
begin
  y := x.right;
  x.right := y.left;
  if (y.left <> nil) then
  begin
    y.left.parent := x;
  end;
  y.parent := x.parent;
  if (x = FRoot) then
  begin
    FRoot := y;
  end
  else if (x = x.parent.left) then
  begin
    x.parent.left := y;
  end
  else
  begin
    x.parent.right := y;
  end;
  y.left := x;
  x.parent := y;
end;

procedure TIntIntRBT.RotateRight(var x: TIIRBNode);
var
  y: TIIRBNode;
begin
  y := x.left;
  x.left := y.right;
  if (y.right <> nil) then
  begin
    y.right.parent := x;
  end;
  y.parent := x.parent;
  if (x = FRoot) then
  begin
    FRoot := y;
  end
  else if (x = x.parent.right) then
  begin
    x.parent.right := y;
  end
  else
  begin
    x.parent.left := y;
  end;
  y.right := x;
  x.parent := y;
end;

function TIntIntRBT.Minimum(var x: TIIRBNode): TIIRBNode;
begin
  Result := x;
  while (Result.left <> nil) do
    Result := Result.left;
end;

function TIntIntRBT.Maximum(var x: TIIRBNode): TIIRBNode;
begin
  Result := x;
  while (Result.right <> nil) do
    Result := Result.right;
end;

procedure TIntIntRBT.Add(const key: Integer; const Value: Integer);
var
  x, y, z, zpp: TIIRBNode;
  cmp: Integer;
begin
  z := TIIRBNode.Create;

  { Initialize fields in new node z }
  z.Key := key;
  z.left := nil;
  z.right := nil;
  z.color := clRed;
  z.Value := Value;

  { Maintain FLeftmost and FRightmost nodes }
  if ((FLeftmost = nil) or (FKeyCompareFunc(key, FLeftmost.Key) < 0)) then
  begin
    FLeftmost := z;
  end;
  if ((FRightmost = nil) or (FKeyCompareFunc(FRightmost.Key, key) < 0)) then
  begin
    FRightmost := z;
  end;

  {: Insert node z }
  y := nil;
  x := FRoot;
  while (x <> nil) do
  begin
    y := x;
    cmp := FKeyCompareFunc(key, x.Key);
    if cmp < 0 then
      x := x.left
    else if cmp > 0 then
      x := x.right
    else
    begin
      {: Key already exists in tree. }
      if FDuplicateKeys then
      begin
        {: Check twins chain for value dublicate. }
        repeat
          if FValueCompareFunc(Value, x.Value) then
          begin
            y := nil;
            break;
          end;
          y := x;
          x := x.Twin;
        until x = nil;
        if Assigned(y) then
        begin
          {: Add dublicate key to end of twins chain. }
          y.Twin := z;
          Inc(FCount);
          if Assigned(FOnChange) then
            FOnChange(Self);
          exit;
        end;
        {: Value already exists in tree. }
      end;
      z.Destroy;
      //a jzombi: memory leak: if we don't put it in the tree, we shouldn't hold it in the memory
      exit;
    end;
  end;
  z.parent := y;
  if (y = nil) then
  begin
    FRoot := z;
  end
  else if (FKeyCompareFunc(key, y.Key) < 0) then
  begin
    y.left := z;
  end
  else
  begin
    y.right := z;
  end;

  { Rebalance tree }
  while ((z <> FRoot) and (z.parent.color = clRed)) do
  begin
    zpp := z.parent.parent;
    if (z.parent = zpp.left) then
    begin
      y := zpp.right;
      if ((y <> nil) and (y.color = clRed)) then
      begin
        z.parent.color := clBlack;
        y.color := clBlack;
        zpp.color := clRed;
        z := zpp;
      end
      else
      begin
        if (z = z.parent.right) then
        begin
          z := z.parent;
          rotateLeft(z);
        end;
        z.parent.color := clBlack;
        zpp.color := clRed;
        rotateRight(zpp);
      end;
    end
    else
    begin
      y := zpp.left;
      if ((y <> nil) and (y.color = clRed)) then
      begin
        z.parent.color := clBlack;
        y.color := clBlack;
        zpp.color := clRed; //c jzombi: zpp.color := clRed;
        z := zpp;
      end
      else
      begin
        if (z = z.parent.left) then
        begin
          z := z.parent;
          rotateRight(z);
        end;
        z.parent.color := clBlack;
        zpp.color := clRed; //c jzombi: zpp.color := clRed;
        rotateLeft(zpp);
      end;
    end;
  end;
  FRoot.color := clBlack;
  Inc(FCount);
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TIntIntRBT.Delete(const key: Integer);
var
  w, x, y, z, x_parent: TIIRBNode;
  tmpcol: TRBColor;
begin
  z := FindNode(key);
  if z = nil then
    exit;

  y := z;
  x := nil;
  x_parent := nil;

  if (y.left = nil) then
  begin { z has at most one non-null child. y = z. }
    x := y.right; { x might be null. }
  end
  else
  begin
    if (y.right = nil) then
    begin { z has exactly one non-null child. y = z. }
      x := y.left; { x is not null. }
    end
    else
    begin
      { z has two non-null children.  Set y to }
      y := y.right; {   z's successor.  x might be null. }
      while (y.left <> nil) do
      begin
        y := y.left;
      end;
      x := y.right;
    end;
  end;

  if (y <> z) then
  begin
    { "copy y's sattelite data into z" }
    { relink y in place of z.  y is z's successor }
    z.left.parent := y;
    y.left := z.left;
    if (y <> z.right) then
    begin
      x_parent := y.parent;
      if (x <> nil) then
      begin
        x.parent := y.parent;
      end;
      y.parent.left := x; { y must be a child of left }
      y.right := z.right;
      z.right.parent := y;
    end
    else
    begin
      x_parent := y;
    end;
    if (FRoot = z) then
    begin
      FRoot := y;
    end
    else if (z.parent.left = z) then
    begin
      z.parent.left := y;
    end
    else
    begin
      z.parent.right := y;
    end;
    y.parent := z.parent;
    tmpcol := y.color;
    y.color := z.color;
    z.color := tmpcol;
    y := z;
    { y now points to node to be actually deleted }
  end
  else
  begin { y = z }
    x_parent := y.parent;
    if (x <> nil) then
    begin
      x.parent := y.parent;
    end;
    if (FRoot = z) then
    begin
      FRoot := x;
    end
    else
    begin
      if (z.parent.left = z) then
      begin
        z.parent.left := x;
      end
      else
      begin
        z.parent.right := x;
      end;
    end;
    if (FLeftmost = z) then
    begin
      if (z.right = nil) then
      begin { z.left must be null also }
        FLeftmost := z.parent;
      end
      else
      begin
        FLeftmost := minimum(x);
      end;
    end;
    if (FRightmost = z) then
    begin
      if (z.left = nil) then
      begin { z.right must be null also }
        FRightmost := z.parent;
      end
      else
      begin { x == z.left }
        FRightmost := maximum(x);
      end;
    end;
  end;

  { Rebalance tree }
  if (y.color = clBlack) then
  begin
    while ((x <> FRoot) and ((x = nil) or (x.color = clBlack))) do
    begin
      if (x = x_parent.left) then
      begin
        w := x_parent.right;
        if (w.color = clRed) then
        begin
          w.color := clBlack;
          x_parent.color := clRed;
          rotateLeft(x_parent);
          w := x_parent.right;
        end;
        if (((w.left = nil) or
          (w.left.color = clBlack)) and
          ((w.right = nil) or
          (w.right.color = clBlack))) then
        begin
          w.color := clRed;
          x := x_parent;
          x_parent := x_parent.parent;
        end
        else
        begin
          if ((w.right = nil) or (w.right.color = clBlack)) then
          begin
            w.left.color := clBlack;
            w.color := clRed;
            rotateRight(w);
            w := x_parent.right;
          end;
          w.color := x_parent.color;
          x_parent.color := clBlack;
          if (w.right <> nil) then
          begin
            w.right.color := clBlack;
          end;
          rotateLeft(x_parent);
          x := FRoot; { break; }
        end
      end
      else
      begin
        { same as above, with right <. left. }
        w := x_parent.left;
        if (w.color = clRed) then
        begin
          w.color := clBlack;
          x_parent.color := clRed;
          rotateRight(x_parent);
          w := x_parent.left;
        end;
        if (((w.right = nil) or
          (w.right.color = clBlack)) and
          ((w.left = nil) or
          (w.left.color = clBlack))) then
        begin
          w.color := clRed;
          x := x_parent;
          x_parent := x_parent.parent;
        end
        else
        begin
          if ((w.left = nil) or (w.left.color = clBlack)) then
          begin
            w.right.color := clBlack;
            w.color := clRed;
            rotateLeft(w);
            w := x_parent.left;
          end;
          w.color := x_parent.color;
          x_parent.color := clBlack;
          if (w.left <> nil) then
          begin
            w.left.color := clBlack;
          end;
          rotateRight(x_parent);
          x := FRoot; { break; }
        end;
      end;
    end;
    if (x <> nil) then
    begin
      x.color := clBlack;
    end;
  end;
  while Assigned(y.Twin) do
  begin
    z := y;
    y := y.Twin;
    z.Destroy;
  end;
  y.Destroy;
  Dec(FCount);
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

function TIntIntRBT.NextKey(var key: Integer; out Value: Integer): Boolean;
var
  x, y: TIIRBNode;
begin
  if Assigned(FLastNode) and (FKeyCompareFunc(FLastNode.Key, key) = 0) then
    x := FLastNode
  else
    x := FindNode(key);
  if x = nil then
  begin
    Result := False;
    exit;
  end;
  if (x.right <> nil) then
  begin
    x := x.right;
    while (x.left <> nil) do
    begin
      x := x.left;
    end;
  end
  else if (x.parent <> nil) then
  begin
    y := x.parent;
    while Assigned(y) and (x = y.right) do
    begin
      x := y;
      y := y.parent;
    end;
    if (x.right <> y) then
      x := y;
  end
  else
    x := FRoot;
  if x = nil then
  begin
    Result := False;
    exit;
  end;
  key := x.Key;
  FLastNode := x;
  Value := x.Value;
  Result := True;
end;

function TIntIntRBT.PrevKey(var key: Integer; out Value: Integer): Boolean;
var
  x, y: TIIRBNode;
begin
  if Assigned(FLastNode) and (FKeyCompareFunc(FLastNode.Key, key) = 0) then
    x := FLastNode
  else
    x := FindNode(key);
  if x = nil then
  begin
    Result := False;
    exit;
  end;
  if (x.left <> nil) then
  begin
    y := x.left;
    while (y.right <> nil) do
    begin
      y := y.right;
    end;
    x := y;
  end
  else if (x.parent <> nil) then
  begin
    y := x.parent;
    while (x = y.left) do
    begin
      x := y;
      y := y.parent;
    end;
    x := y;
  end
  else
    x := FRoot;
  if x = nil then
  begin
    Result := False;
    exit;
  end;
  key := x.Key;
  FLastNode := x;
  Value := x.Value;
  Result := True;
end;

function TIntIntRBT.GetFirst: Integer;
begin
  Result := FLeftMost.Key;
end;

function TIntIntRBT.GetLast: Integer;
begin
  Result := FRightMost.Key;
end;

procedure TIntIntRBT.ForEach(AProc: TForEachProc);
var
  x, y, z: TIIRBNode;
  cont: Boolean;
begin
  if Assigned(FLeftMost) then
  begin
    x := FLeftMost;
    repeat
      z := x;
      repeat
        AProc(z.Key, z.Value, cont);
        if not cont then
          exit;
        z := z.Twin;
      until z = nil;
      // Next node
      if (x.right <> nil) then
      begin
        x := x.right;
        while (x.left <> nil) do
        begin
          x := x.left;
        end;
      end
      else if (x.parent <> nil) then
      begin
        y := x.parent;
        while (x = y.right) do
        begin
          x := y;
          y := y.parent;
        end;
        if (x.right <> y) then
          x := y;
      end
      else
        x := FRoot;
    until x = FRightMost;
    if cont and (FLeftMost <> FRightMost) then
      AProc(FRightMost.Key, FRightMost.Value, cont);
  end;
end;

procedure TIntIntRBT.SetDuplicateKeys(Value: Boolean);
begin
  if Value and Assigned(FValueCompareFunc) then
    FDuplicateKeys := True
  else
    FDuplicateKeys := False;
end;




