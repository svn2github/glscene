//
// This unit is part of the GLScene Project, http://glscene.org
//
{: GLSRedBlackTree <p>

  USAGE
  The TRedBlackTree generic class behaves somewhat like a TList:
  it stores _Value_ by _Key_
  and uses the same comparison function as TList.Sort (TListSortCompare).
  Functions Clear, Add, Delete, First and Last are equivalent,
  except that First and Last return a _Key_ so they
  can be used for comparisons in loops. All _Values_ occur only once in the
  tree: when the same value is added twice, the second one is not stored.

  To be able to manage the tree, the Create constructor has a argument
  specifying the comparison function that should be used.

  The function Find can be used to find a _Value_ that was put in the tree,
  it searches for the given _Key_ using the comparison function given
  at time of object creation. It returns a TRBNodeP.

  The functions NextKey and PrevKey can be used to "walk" through the tree:
  given a _Key_, NextKey replace it with the smallest key that
  is larger than _Key_, PrevKey returns the largest key that is
  smaller than _Key_. For Last and First key result not returned.

   <b>History : </b><font size=-1><ul>
      <li>19/04/10 - Yar - Creation (based on grbtree jzombi aka Jani Matyas)
   </ul></font><p>
}

unit GLSRedBlackTree;

interface

{$I GLScene.inc}

type

  TRBColor = (clRed, clBlack);

  // TRedBlackTree
  //
{$IFDEF FPC}
  generic
{$ENDIF}
  GRedBlackTree < K, V > = class
  { Public Declarations }
  type {$IFDEF FPC}public{$ENDIF}
    TCompareFunc = function(const Item1, Item2: K): Integer;
    TForEachProc = procedure (AKey: K; AValue: V; out AContinue: Boolean);
    { Private Declarations }
  {$IFDEF GLS_COMPILER_2009_DOWN}
    type {$IFDEF FPC}private{$ENDIF}
      TRBNode = class
        Key: K;
        Left, Right, Parent: TRBNode;
        Color: TRBColor;
        Value: V;
      end;
  {$ELSE}
      TRBNode = ^TRBNodeRec;
      TRBNodeRec = record
        Key: K;
        Left, Right, Parent: TRBNode;
        Color: TRBColor;
        Value: V;
      end;
  {$ENDIF}
   var private
      FRoot: TRBNode;
      FLeftmost: TRBNode;
      FRightmost: TRBNode;
      FLastNode: TRBNode;
      FCompareFunc: TCompareFunc;

    function FindNode(const key: K): TRBNode;
    procedure RotateLeft(var x: TRBNode);
    procedure RotateRight(var x: TRBNode);
    function Minimum(var x: TRBNode): TRBNode;
    function Maximum(var x: TRBNode): TRBNode;
    function GetFirst: K;
    function GetLast: K;
    class procedure FastErase(x: TRBNode);

  public
    { Public Declarations }
    constructor Create(Compare: TCompareFunc);
    destructor Destroy; override;

    procedure Clear;

    function Find(const key: K; out Value: V): Boolean;
    function NextKey(var key: K): V;
    function PrevKey(var key: K): V;
    procedure Add(const key: K; const Value: V);
    procedure Delete(const key: K);
    procedure ForEach(AProc: TForEachProc);
    property First: K read GetFirst;
    property Last: K read GetLast;
  end;

implementation

constructor GRedBlackTree
{$IFNDEF FPC}<K, V>{$ENDIF} .Create(Compare: TCompareFunc);
begin
  inherited Create;
  FCompareFunc := Compare;
  FRoot := nil;
  FLeftmost := nil;
  FRightmost := nil;
end;

destructor GRedBlackTree
{$IFNDEF FPC}<K, V>{$ENDIF} .Destroy;
begin
  Clear;
  inherited Destroy;
end;

class procedure GRedBlackTree
{$IFNDEF FPC}<K, V>{$ENDIF} .FastErase(x: TRBNode);
begin
  if (x.left <> nil) then
    FastErase(x.left);
  if (x.right <> nil) then
    FastErase(x.right);
{$IFDEF GLS_COMPILER_2009_DOWN}
  x.Free;
{$ELSE}
  Dispose(x);
{$ENDIF}
end;

procedure GRedBlackTree
{$IFNDEF FPC}<K, V>{$ENDIF} .Clear;
begin
  if (FRoot <> nil) then
    FastErase(FRoot);
  FRoot := nil;
  FLeftmost := nil;
  FRightmost := nil;
end;

function GRedBlackTree
{$IFNDEF FPC}<K, V>{$ENDIF} .Find(const key: K; out Value: V): Boolean;
var
  pNode: TRBNode;
begin
  pNode := FindNode(key);
  Result := Assigned(pNode);
  if Result then
    Value := pNode.Value;
end;

function GRedBlackTree
{$IFNDEF FPC}<K, V>{$ENDIF} .FindNode(const key: K): TRBNode;
var
  cmp: integer;
begin
  Result := FRoot;
  while (Result <> nil) do
  begin
    cmp := FCompareFunc(Result.Key, key);
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

procedure GRedBlackTree
{$IFNDEF FPC}<K, V>{$ENDIF} .RotateLeft(var x: TRBNode);
var
  y: TRBNode;
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

procedure GRedBlackTree
{$IFNDEF FPC}<K, V>{$ENDIF} .RotateRight(var x: TRBNode);
var
  y: TRBNode;
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

function GRedBlackTree
{$IFNDEF FPC}<K, V>{$ENDIF} .Minimum(var x: TRBNode): TRBNode;
begin
  Result := x;
  while (Result.left <> nil) do
    Result := Result.left;
end;

function GRedBlackTree
{$IFNDEF FPC}<K, V>{$ENDIF} .Maximum(var x: TRBNode): TRBNode;
begin
  Result := x;
  while (Result.right <> nil) do
    Result := Result.right;
end;

procedure GRedBlackTree
{$IFNDEF FPC}<K, V>{$ENDIF} .Add(const key: K; const Value: V);
var
  x, y, z, zpp: TRBNode;
  cmp: Integer;
begin
{$IFDEF GLS_COMPILER_2009_DOWN}
  z := TRBNode.Create;
{$ELSE}
  New(z);
{$ENDIF}
  { Initialize fields in new node z }
  z.Key := key;
  z.left := nil;
  z.right := nil;
  z.color := clRed;
  z.Value := Value;

  { Maintain FLeftmost and FRightmost nodes }
  if ((FLeftmost = nil) or (FCompareFunc(key, FLeftmost.Key) < 0)) then
  begin
    FLeftmost := z;
  end;
  if ((FRightmost = nil) or (FCompareFunc(FRightmost.Key, key) < 0)) then
  begin
    FRightmost := z;
  end;

  { Insert node z }
  y := nil;
  x := FRoot;
  while (x <> nil) do
  begin
    y := x;
    cmp := FCompareFunc(key, x.Key);
    if (cmp < 0) then
    begin
      x := x.left;
    end
    else if (cmp > 0) then
    begin
      x := x.right;
    end
    else
    begin
      { Value already exists in tree. }
{$IFDEF GLS_COMPILER_2009_DOWN}
      z.Free;
{$ELSE}
      Dispose(z);
{$ENDIF}
      //a jzombi: memory leak: if we don't put it in the tree, we shouldn't hold it in the memory
      exit;
    end;
  end;
  z.parent := y;
  if (y = nil) then
  begin
    FRoot := z;
  end
  else if (FCompareFunc(key, y.Key) < 0) then
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
end;

procedure GRedBlackTree
{$IFNDEF FPC}<K, V>{$ENDIF} .Delete(const key: K);
var
  w, x, y, z, x_parent: TRBNode;
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
{$IFDEF GLS_COMPILER_2009_DOWN}
  y.Free;
{$ELSE}
  Dispose(y);
{$ENDIF}
end;

function GRedBlackTree
{$IFNDEF FPC}<K, V>{$ENDIF} .NextKey(var key: K): V;
var
  x, y: TRBNode;
begin
  if Assigned(FLastNode) and (FCompareFunc(FLastNode.Key, key) = 0) then
    x := FLastNode
  else
    x := FindNode(key);
  if x = nil then
    exit;
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
  key := x.Key;
  FLastNode := x;
  Result := x.Value;
end;

function GRedBlackTree
{$IFNDEF FPC}<K, V>{$ENDIF} .PrevKey(var key: K): V;
var
  x, y: TRBNode;
begin
  if Assigned(FLastNode) and (FCompareFunc(FLastNode.Key, key) = 0) then
    x := FLastNode
  else
    x := FindNode(key);
  if x = nil then
    exit;
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
  key := x.Key;
  FLastNode := x;
  Result := x.Value;
end;

function GRedBlackTree
{$IFNDEF FPC}<K, V>{$ENDIF}.GetFirst: K;
begin
  Result := FLeftMost.Key;
end;

function GRedBlackTree
{$IFNDEF FPC}<K, V>{$ENDIF}.GetLast: K;
begin
  Result := FRightMost.Key;
end;

procedure GRedBlackTree
{$IFNDEF FPC}<K, V>{$ENDIF}.ForEach(AProc: TForEachProc);
var
  x, y: TRBNode;
  cont: Boolean;
begin
  if Assigned(FLeftMost) then
  begin
    x := FLeftMost;
    repeat
      AProc(x.Key, x.Value, cont);
      if not cont then
        break;
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

end.

