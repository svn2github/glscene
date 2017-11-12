//
// This unit is part of the GLScene Project, http://glscene.org
//
{
   Defines vector types as advanced records.
   History:
     17/05/11 - PW - Creation.
     The whole history is logged in previous version of the unit
}
unit GLTypes;

interface

uses
  System.Types,
  System.SysUtils,
  System.Rtti,
  System.Math,
  System.Math.Vectors,
  GLVectorTypes;


type
  TAbstractVector = array of Extended;
  TAbstractMatrix = array of array of Extended;

  TxQuaternion = record
  private
    FData: array[0..3] of Extended;
    procedure SetElement(Index: Byte; Value: Extended);
    function GetElement(Index: Byte): Extended;
  public
    constructor Create(Q: TAbstractVector);
    class operator Multiply(Q1, Q2: TxQuaternion): TxQuaternion;
    class operator Multiply(Q: TxQuaternion; Sc: Extended): TxQuaternion;
    class operator Multiply(Scalar: Extended; Q: TxQuaternion): TxQuaternion;
    class operator Implicit(V: TAbstractVector): TxQuaternion;
    function Inv: TxQuaternion;
    function TruncateSTI: TxQuaternion;
    property Element[index: Byte]: Extended read GetElement
      write SetElement; default;
  end;

  PxVector = ^TxVector;
  TxVector = record
  private
    FData: TAbstractVector;
    FCount: Word;
    procedure SetElement(Index: Word; Value: Extended);
    function GetElement(Index: Word): Extended;
    procedure CheckUnique;
  public
    constructor Create(ElementsCount: Word); overload;
    constructor Create(V: TAbstractVector); overload;
    class operator Add(V1, V2: TxVector): TxVector;
    class operator Add(V: TxVector; Scalar: Extended): TxVector;
    class operator Add(Scalar: Extended; V: TxVector): TxVector;
    class operator Subtract(V1, V2: TxVector): TxVector;
    class operator Subtract(Scalar: Extended; V: TxVector): TxVector;
    class operator Subtract(V: TxVector; Scalar: Extended): TxVector;
    class operator Multiply(V1, V2: TxVector): TxVector;
    class operator Multiply(V: TxVector; Scalar: Extended): TxVector;
    class operator Multiply(Scalar: Extended; V: TxVector): TxVector;
    class operator Divide(V: TxVector; Scalar: Extended): TxVector;
    class operator Divide(V1, V2: TxVector): TxVector;
    class operator Implicit(V: TAbstractVector): TxVector;
    function Norm: Extended;
    function SumOfSquares: Extended;
    function SumOfElments: Extended;
    function TruncateSTI: TxVector;
    function ToQuat: TxQuaternion;
    procedure Fill(Value: Extended);
    function ScalarMult(V: TxVector): Extended;
    property Count: Word read FCount;
    property Elements[index: Word]: Extended read GetElement
      write SetElement; default;
  end;

  PxMatrix = ^TxMatrix;
  TxMatrix = record
  private
    FData: TAbstractMatrix;
    FRowsCount: Word;
    FColsCount: Word;
    procedure SetElement(Row, Col: Word; Value: Extended);
    function GetElement(Row, Col: Word): Extended;
    function GetRow(Row: Word): TxVector;
    procedure SetRow(Row: Word; Value: TxVector);
    function GetCol(Col: Word): TxVector;
    procedure SetCol(Col: Word; Value: TxVector);
    function Del(A: TxMatrix; I, J: Integer; M: Integer): TxMatrix;
    function Det(A: TxMatrix; M: Integer): Extended;
    procedure CheckUnique;
  public
    constructor Create(RowsCount, ColsCount: Word); overload;
    constructor CreateDiag(Dim: Word; Value: Extended = 1.0);
    constructor Create(M: TAbstractMatrix); overload;
    class operator Add(M1, M2: TxMatrix): TxMatrix;
    class operator Subtract(M1, M2: TxMatrix): TxMatrix;
    class operator Multiply(M1, M2: TxMatrix): TxMatrix;
    class operator Multiply(M: TxMatrix; V: TxVector): TxVector;
    class operator Multiply(V: TxVector; M: TxMatrix): TxVector;
    class operator Multiply(M: TxMatrix; Scalar: Extended): TxMatrix;
    class operator Multiply(Scalar: Extended; M: TxMatrix): TxMatrix;
    class operator Multiply(M: TxMatrix; Q: TxQuaternion): TxQuaternion;
    class operator Implicit(M: TAbstractMatrix): TxMatrix;
    function Transp: TxMatrix;
    function Inv: TxMatrix;
    function ToQuat: TxQuaternion;
    function Determinant: Extended;
    function TruncateSTI: TxMatrix;
    function Trace: Extended; // sum on diagonal elements
    procedure Fill(Scalar: Extended);
    property RowCount: Word read FRowsCount;
    property ColCount: Word read FColsCount;
    property Row[Row: Word]: TxVector read GetRow write SetRow;
    property Col[Col: Word]: TxVector read GetCol write SetCol;
    property Elements[Row, Col: Word]: Extended read GetElement
      write SetElement; default;
  end;

  TxQuatHelper = record helper for TxQuaternion
    function ToMatrix: TxMatrix;
  end;

  TVecHelper = record helper for TxVector
    function ToDiagMatrix: TxMatrix;
  end;

  TDim = class(TCustomAttribute)
  private
    FRowCount: Integer;
    FColCount: Integer;
  public
    constructor Create(ARowCount: Integer; AColCount: Integer = 0); overload;
    property RowCount: Integer read FRowCount;
    property ColCount: Integer read FColCount;
  end;

  function TVec(V: TAbstractVector): TxVector;
  function TMat(M: TAbstractMatrix): TxMatrix;
  function TQuat(Q: TAbstractVector): TxQuaternion;

  procedure Init(Obj, TypeInfoOfObj: Pointer; Offset: Integer = 0);


//-----------------------
// GLPoint types
//-----------------------
type
  TGLScalarValue = Single;
  TGLScalarField = function(X, Y, Z: Single): TGLScalarValue;

  // If data are made on integer XYZ index
  TGLScalarFieldInt = function(iX, iY, iZ: Integer): TGLScalarValue of object;

  TGLVertex = record
    P, N: TVector3f;  //Point and Normal
    Density: Single;
  end;

  TGLFace = record
    Normal: TVector3f;
    V1: TVector3f; // vertex 1
    V2: TVector3f; // vertex 2
    V3: TVector3f; // vertex 3
    Padding: array [0 .. 1] of Byte;
  end;

  PGLPoint2D = ^TGLPoint2D;
  TGLPoint2D = record
    X: Single;
    Y: Single;
    public
      function Create(X, Y : Single): TGLPoint2D;
      procedure SetPosition(const X, Y : Single);
      function Add(const APoint2D: TGLPoint2D): TGLPoint2D;
      function Length: Single; //distance from origin
      function Distance(const APoint2D : TGLPoint2D) : Single;
      class function PointInCircle(const Point, Center: TGLPoint2D;
        const Radius: Integer):Boolean; static; inline;
      procedure Offset(const ADeltaX, ADeltaY : Single);
  end;

  PGLPoint3D = ^TGLPoint3D;
  TGLPoint3D = record
    X: Single;
    Y: Single;
    Z: Single;
    public
      function Create(X, Y, Z: Single): TGLPoint3D;
      procedure SetPosition(const X, Y, Z: Single);
      function Add(const AGLPoint3D: TGLPoint3D): TGLPoint3D;
      function Length: Single; //distance to origin
      function Distance(const APoint3D : TGLPoint3D): Single;
      procedure Offset(const ADeltaX, ADeltaY, ADeltaZ: Single);
  end;

//GL Point Arrays
  TGLPoint2DArray = array of TGLPoint2D;
  TGLPoint3DArray = array of TGLPoint3D;


// GLVoxel types
  TGLVoxelStatus = (bpExternal, bpInternal);
  TGLVoxel = record
    P: TVector3f;
    Density: TGLScalarValue;
    Status: TGLVoxelStatus;
  end;
  PGLVoxel = ^TGLVoxel;

  TGLVoxelData = array [0 .. (MaxInt shr 8)] of TGLVoxel;
  PGLVoxelData = ^TGLVoxelData;


//-----------------------
// GLVector types
//-----------------------

  TGLVector2DType = array [0..1] of Single;
  TGLVector3DType = array [0..2] of Single;

  TGLVector2D = record
      function Create(const AX, AY, AW : Single): TGLVector2D;
      function Add(const AVector2D: TGLVector2D): TGLVector2D;
      function Length: Single;
      function Norm: Single;
      function Normalize: TGLVector2D;
      function CrossProduct(const AVector: TGLVector2D): TGLVector2D;
      function DotProduct(const AVector: TGLVector2D): Single;
    case Integer of
      0: (V: TGLVector2DType;);
      1: (X: Single;
          Y: Single;
          W: Single;)
  end;

  TGLVector3D = record
      function Create(const AX, AY, AZ, AW : Single): TGLVector3D;
      function Add(const AVector3D: TGLVector3D): TGLVector3D;
      function Length: Single;
      function Norm: Single;
      function Normalize: TGLVector3D;
      function CrossProduct(const AVector3D: TVector3D): TVector3D;
      function DotProduct(const AVector3D: TVector3D): Single; inline;
    case Integer of
      0: (V: TGLVector3DType;);
      1: (X: Single;
          Y: Single;
          Z: Single;
          W: Single;)
  end;

// GLVector Arrays
  TGLVector2DArray = array of TGLVector2D;
  TGLVector3DArray = array of TGLVector3D;

//-----------------------
// GLMatrix types
//-----------------------
  TGLMatrix2DType = array[0..3] of TGLVector2D;
  {.$NODEFINE TGLMatrix2DType}
  {.$HPPEMIT END OPENNAMESPACE}
  {.$HPPEMIT END 'typedef TGLVector2D TGLMatrix2DArray[4];'}
  {.$HPPEMIT END CLOSENAMESPACE}
  TGLMatrix3DType = array[0..3] of TGLVector3D;
  {.$NODEFINE TGLMatrix3DType}
  {.$HPPEMIT END OPENNAMESPACE}
  {.$HPPEMIT END 'typedef TGLVector3D TGLMatrix3DType[4];'}
  {.$HPPEMIT END CLOSENAMESPACE}

  TGLMatrix2D = record
  private
  public
    case Integer of
      0: (M: TGLMatrix2DType;);
      1: (e11, e12, e13: Single;
          e21, e22, e23: Single;
          e31, e32, e33: Single);
  end;

  TGLMatrix3D = record
  private
  public
    case Integer of
      0: (M: TGLMatrix3DType;);
      1: (e11, e12, e13, e14: Single;
          e21, e22, e23, e24: Single;
          e31, e32, e33, e34: Single;
          e41, e42, e43, e44: Single);
  end;

// GLMatrix Arrays
  TGLMatrix2DArray = array of TGLMatrix2D;
  TGLMatrix3DArray = array of TGLMatrix3D;


//-----------------------
// GLPolygon types
//-----------------------

  TGLPolygon2D = TGLPoint2DArray;

  TGLPolygon3D = TGLPoint3DArray;
{
  TGLPolygon3D = record
    Vertices: array of TGLPoint3D;
    function Area;
  end;
}

const
   ClosedPolygon2D: TGLPoint2D = (X: $FFFF; Y: $FFFF);
   ClosedPolygon3D: TGLPoint3D = (X: $FFFF; Y: $FFFF; Z: $FFFF);

type
  TGLVertexArray = array [0 .. (MaxInt shr 8)] of TGLVertex;
  PGLVertexArray = ^TGLVertexArray;

type
  TGLTriangle = record
    v1, v2, v3: Integer;
    ///Vertices: array[0..2] of TGLPoint3D;
    ///function Area;
  end;

  TGLTriangleArray = array [0 .. (MaxInt shr 8)] of TGLTriangle;
  PGLTriangleArray = ^TGLTriangleArray;



//-----------------------
// GLPolyhedron types
//-----------------------
type
  TGLPolyhedron = array of TGLPolygon3D;

{
  TGLPolyhedron = record
    Facets: array of TGLPolygon3D;
    function NetLength;
    function Area;
    function Volume;
  end;
}

//--------------------------
// GLMesh simple record types
//--------------------------
type
   TGLMesh2DVertex = record
    X, Y: Single;
    NX, NY: Single;
    tU, tV: Single;
  end;

   TGLMesh3DVertex = record
    X, Y, Z: Single;
    NX, NY, NZ: Single;
    tU, tV: Single;
  end;

  TGLMesh2D = array of TGLMesh2DVertex;
  TGLMesh3D = array of TGLMesh3DVertex;

//--------------------------
// GLQuaternion record types
//--------------------------
type

  TGLQuaternion = record
    ImPart: TGLVector3D;
    RePart: Single;
  end;

  TGLQuaternionArray = array of TGLQuaternion;


type
  TGLBox = record
    ALeft, ATop, ANear, ARight, ABottom, AFar: Single;
  end;


const
  sWRONG_ELEMENT = 'Wrong element';
  sWRONG_SIZE = 'Wrong size';
  sNOT_QUAD = 'Matrix not quadratic';
  sSINGULAR = 'Singular matrix founded';

//---------------------------------------------------------------
//---------------------------------------------------------------
//---------------------------------------------------------------
implementation
//---------------------------------------------------------------
//---------------------------------------------------------------
//---------------------------------------------------------------


function TVec(V: TAbstractVector): TxVector;
begin
  Result.Create(V);
end;

function TMat(M: TAbstractMatrix): TxMatrix;
begin
  Result.Create(M);
end;

function TQuat(Q: TAbstractVector): TxQuaternion;
begin
  Result.Create(Q);
end;

{$POINTERMATH ON}
function NotUnique(PArr: PCardinal): Boolean;
begin
  Result := (PArr - 2)^ > 1;
end;

{ TMatrix }

// Removing i-th row and j-th col
function TxMatrix.Del(A: TxMatrix; I, J: Integer; M: Integer): TxMatrix;
var
  K, G: Integer;
begin
  for G := I to M - 1 do
    for K := 1 to M do
      A[G, K] := A[G + 1, K];
  for G := J to M - 1 do
    for K := 1 to M - 1 do
      A[K, G] := A[K, G + 1];
  Result := A;
end;

// Recursive calculation of det for matrix
function TxMatrix.Det(A: TxMatrix; M: Integer): Extended;
var
  I: Integer;
  Buf: Extended;
begin
  Buf := 0;
  if M = 1 then
    Buf := A[1, 1]
  else
    for I := 1 to M do
      Buf := Buf + Power10(-1, I + 1) * A[I, 1] *
        Det(Del(A, I, 1, M), M - 1);
  Result := Buf;
end;

class operator TxMatrix.Add(M1, M2: TxMatrix): TxMatrix;
var
  I, J: Integer;
begin
  if (M1.FRowsCount <> M2.FRowsCount) or (M1.FColsCount <> M2.FColsCount) then
    raise EMathError.Create(sWRONG_SIZE);
  Result.Create(M1.FRowsCount, M1.FColsCount);
  for I := 0 to M1.FRowsCount - 1 do
    for J := 0 to M1.FColsCount - 1 do
      Result.FData[I, J] := M1.FData[I, J] + M2.FData[I, J];
end;

procedure TxMatrix.CheckUnique;
var
  I: Integer;
begin
  if NotUnique(@FData) then
    begin
      FData := Copy(FData);
      for I := 0 to Pred(FRowsCount) do
        FData[i] := Copy(FData[i]);
    end;
end;

constructor TxMatrix.Create(RowsCount, ColsCount: Word);
begin
  FRowsCount := RowsCount;
  FColsCount := ColsCount;
  FData := nil;
  SetLength(FData, FRowsCount, FColsCount);
end;

constructor TxMatrix.Create(M: TAbstractMatrix);
var
  I: Integer;
begin
  FRowsCount := Length(M);
  FColsCount := Length(M[0]);
  FData := nil;
  SetLength(FData, FRowsCount, FColsCount);
  for I := 0 to Pred(FRowsCount) do
    begin
      if Length(M[I]) <> FColsCount then
        raise EMathError.Create('Wrong matrix proportions');
      FData[I] := Copy(M[I]);
    end;
end;

constructor TxMatrix.CreateDiag(Dim: Word; Value: Extended = 1.0);
var
  I: Integer;
begin
  Create(Dim, Dim);
  for I := 0 to Dim - 1 do
    FData[I, I] := Value;
end;

function TxMatrix.Determinant: Extended;
begin
  if (FRowsCount <> FColsCount) then
    raise EMathError.Create(sNOT_QUAD);
  Result := Det(Self, FRowsCount);
end;

procedure TxMatrix.Fill(Scalar: Extended);
var
  I, J: Integer;
begin
  if Scalar = 0 then
  begin
    FData := nil;
    SetLength(FData, FRowsCount, FColsCount);
  end
  else
    for I := 0 to FRowsCount - 1 do
      for J := 0 to FColsCount - 1 do
        FData[I, J] := Scalar;
end;

function TxMatrix.GetCol(Col: Word): TxVector;
var
  I: Integer;
begin
  if (Col = 0) or (Col > FColsCount) then
    raise EMathError.Create(sWRONG_ELEMENT);
  Result.Create(FRowsCount);
  for I := 0 to FRowsCount - 1 do
    Result.FData[I] := FData[I, Col - 1];
end;

function TxMatrix.GetElement(Row, Col: Word): Extended;
begin
  {$R+}
  Result := FData[Pred(Row), Pred(Col)];
end;

function TxMatrix.GetRow(Row: Word): TxVector;
var
  I: Integer;
begin
  if (Row = 0) or (Row > FRowsCount) then
    raise EMathError.Create(sWRONG_ELEMENT);
  Result.Create(FColsCount);
  for I := 0 to FColsCount - 1 do
    Result.FData[I] := FData[Row - 1, I];
end;

class operator TxMatrix.Implicit(M: TAbstractMatrix): TxMatrix;
begin
  Result.Create(M);
end;

function TxMatrix.Inv: TxMatrix;
var
  Ipiv, Indxr, Indxc: array of Integer;
  DimMat, I, J, K, L, N, ICol, IRow: Integer;
  Big, Dum, Pivinv: Extended;
begin
  // Jordan algorithm
  if (FRowsCount <> FColsCount) then
    raise EMathError.Create(sNOT_QUAD);
  Result := Self;
  DimMat := FRowsCount;
  SetLength(Ipiv, DimMat);
  SetLength(Indxr, DimMat);
  SetLength(Indxc, DimMat);
  IRow := 1;
  ICol := 1;
  for I := 1 to DimMat do
  begin
    Big := 0;
    for J := 1 to DimMat do
      if (Ipiv[J - 1] <> 1) then
        for K := 1 to DimMat do
          if (Ipiv[K - 1] = 0) then
            if (Abs(Result[J, K]) >= Big) then
            begin
              Big := Abs(Result[J, K]);
              IRow := J;
              ICol := K;
            end;
    Ipiv[ICol - 1] := Ipiv[ICol - 1] + 1;
    if (IRow <> ICol) then
      for L := 1 to DimMat do
      begin
        Dum := Result[IRow, L];
        Result[IRow, L] := Result[ICol, L];
        Result[ICol, L] := Dum;
      end;
    Indxr[I - 1] := IRow;
    Indxc[I - 1] := ICol;
    if Result[ICol, ICol] = 0 then
      raise EMathError.Create(sSINGULAR);
    Pivinv := 1.0 / Result[ICol, ICol];
    Result[ICol, ICol] := 1.0;
    for L := 1 to DimMat do
      Result[ICol, L] := Result[ICol, L] * Pivinv;
    for N := 1 to DimMat do
      if (N <> ICol) then
      begin
        Dum := Result[N, ICol];
        Result[N, ICol] := 0.0;
        for L := 1 to DimMat do
          Result[N, L] := Result[N, L] - Result[ICol, L] * Dum;
      end;
  end;
  for L := DimMat downto 1 do
    if (Indxr[L - 1] <> Indxc[L - 1]) then
      for K := 1 to DimMat do
      begin
        Dum := Result[K, Indxr[L - 1]];
        Result[K, Indxr[L - 1]] := Result[K, Indxc[L - 1]];
        Result[K, Indxc[L - 1]] := Dum;
      end;
end;

function TxMatrix.ToQuat: TxQuaternion;
begin
    Result[0] := 0.5 * Sqrt(Abs(1 + Self[1,1] + Self[2,2] + Self[3,3]));
    Result[1] := 0.5 * Sqrt(Abs(1 + Self[1,1] - Self[2,2] - Self[3,3]));
    if Self[3,2] < Self[2,3] then
        Result[1] := -Result[1];
    Result[2] := 0.5 * Sqrt(Abs(1 - Self[1,1] + Self[2,2] - Self[3,3]));
    if Self[1,3] < Self[3,1] then
        Result[2] := -Result[2];
    Result[3] := 0.5 * Sqrt(Abs(1 - Self[1,1] - Self[2,2] + Self[3,3]));
    if Self[2,1] < Self[1,2] then
        Result[3] := -Result[3];
end;

class operator TxMatrix.Multiply(M: TxMatrix; Q: TxQuaternion): TxQuaternion;
var
  I, J: Integer;
begin
  if (M.FRowsCount <> 4) or (M.FRowsCount <> M.FColsCount) then
    raise EMathError.Create(sWRONG_SIZE);
  FillChar(Result.FData, SizeOf(Result.FData), 0);
  for I := 0 to 3 do
    for J := 0 to 3 do
      Result.FData[I] := Result.FData[I] + M.FData[I, J] * Q.FData[J];
end;

class operator TxMatrix.Multiply(Scalar: Extended; M: TxMatrix): TxMatrix;
begin
  Result := M * Scalar;
end;

class operator TxMatrix.Multiply(V: TxVector; M: TxMatrix): TxVector;
var
  I, J: Integer;
begin
  if (V.FCount <> M.FRowsCount) then
    raise EMathError.Create(sWRONG_SIZE);
  Result.Create(V.FCount);
  for I := 0 to V.FCount - 1 do
    for J := 0 to V.FCount - 1 do
      Result.FData[I] := Result.FData[I] + V.FData[J] * M.FData[J, I];
end;

class operator TxMatrix.Multiply(M: TxMatrix; V: TxVector): TxVector;
var
  I, J: Integer;
begin
  if (M.FColsCount <> V.FCount) then
    raise EMathError.Create(sWRONG_SIZE);
  Result.Create(M.FRowsCount);
  for I := 0 to M.FRowsCount - 1 do
    for J := 0 to M.FColsCount - 1 do
      Result.FData[I] := Result.FData[I] + M.FData[I, J] * V.FData[J];
end;

class operator TxMatrix.Multiply(M: TxMatrix; Scalar: Extended): TxMatrix;
var
  I, J: Integer;
begin
  Result.Create(M.FRowsCount, M.FColsCount);
  for I := 0 to M.FRowsCount - 1 do
    for J := 0 to M.FColsCount - 1 do
      Result.FData[I, J] := M.FData[I, J] * Scalar;
end;

class operator TxMatrix.Multiply(M1, M2: TxMatrix): TxMatrix;
var
  I, J, K: Integer;
begin
  if (M1.FColsCount <> M2.FRowsCount) then
    raise EMathError.Create(sWRONG_SIZE);
  Result.Create(M1.FRowsCount, M2.FColsCount);
  for I := 0 to M1.FRowsCount - 1 do
    for J := 0 to M2.FColsCount - 1 do
      for K := 0 to M1.FColsCount - 1 do
        Result.FData[I, J] := Result.FData[I, J] + M1.FData[I, K] * M2.FData[K, J];
end;

procedure TxMatrix.SetCol(Col: Word; Value: TxVector);
var
  I: Integer;
begin
  if (Col = 0) or (Col > FColsCount) then
    raise EMathError.Create(sWRONG_ELEMENT);
  if (Value.Count <> FRowsCount) then
    raise EMathError.Create(sWRONG_SIZE);
  for I := 0 to FRowsCount - 1 do
    FData[I, Col - 1] := Value.FData[I];
end;

procedure TxMatrix.SetElement(Row, Col: Word; Value: Extended);
begin
  {$R+}
  CheckUnique;
  FData[Pred(Row), Pred(Col)] := Value;
end;

procedure TxMatrix.SetRow(Row: Word; Value: TxVector);
var
  I: Integer;
begin
  if (Row = 0) or (Row > FRowsCount) then
    raise EMathError.Create(sWRONG_ELEMENT);
  if (Value.Count <> FColsCount) then
    raise EMathError.Create(sWRONG_SIZE);
  for I := 0 to FColsCount - 1 do
    FData[Row - 1, I] := Value.FData[I];
end;

class operator TxMatrix.Subtract(M1, M2: TxMatrix): TxMatrix;
var
  I, J: Integer;
begin
  if (M1.FColsCount <> M2.FColsCount) or (M1.FRowsCount <> M2.FRowsCount) then
    raise EMathError.Create(sWRONG_SIZE);
  Result.Create(M1.FRowsCount, M1.FColsCount);
  for I := 0 to M1.FRowsCount - 1 do
    for J := 0 to M1.FColsCount - 1 do
      Result.FData[I, J] := M1.FData[I, J] - M2.FData[I, J];
end;

function TxMatrix.Trace: Extended;
var
  I: Integer;
begin
  Result := 0;
  if FColsCount <> FRowsCount then
    raise EMathError.Create(sNOT_QUAD);
  for I := 0 to FColsCount - 1 do
    Result := Result + FData[I, I];
end;

function TxMatrix.Transp: TxMatrix;
var
  I, J: Integer;
begin
  Result.Create(FColsCount, FRowsCount);
  for I := 0 to FColsCount - 1 do
    for J := 0 to FRowsCount - 1 do
      Result.FData[I, J] := FData[J, I];
end;

function TxMatrix.TruncateSTI: TxMatrix;
const
  Int32Max: Double = Integer.MaxValue;
  Int32Min: Double = Integer.MinValue;
var
  I, J: Integer;
begin
  Result.Create(FRowsCount, FColsCount);
  for I := 0 to FRowsCount - 1 do
    for J := 0 to FColsCount - 1 do
    begin
      if (FData[I, J] >= Int32Min) and (FData[I, J] <= Int32Max) then
        Result.FData[I, J] := Trunc(FData[I, J])
      else
        if (FData[I, J] < Int32Min) then
          Result.FData[I, J] := Int32Min
        else
          Result.FData[I, J] := Int32Max;
    end;
end;


{ TxVector }

constructor TxVector.Create(V: TAbstractVector);
begin
  FCount := Length(V);
  FData := Copy(V);
end;

constructor TxVector.Create(ElementsCount: Word);
begin
  FCount := ElementsCount;
  FData := nil;
  SetLength(FData, FCount);
end;

class operator TxVector.Add(V1, V2: TxVector): TxVector;
var
  i: Integer;
begin
  if (V1.FCount <> V2.FCount) then
    raise EMathError.Create(sWRONG_SIZE);
  Result := TxVector.Create(V1.FCount);
  for i := 0 to V1.FCount - 1 do
    Result.FData[i] := V1.FData[i] + V2.FData[i];
end;

class operator TxVector.Add(V: TxVector; Scalar: Extended): TxVector;
var
  I: Integer;
begin
  Result.Create(V.FCount);
  for I := 0 to V.FCount - 1 do
    Result.FData[I] := V.FData[I] + Scalar;
end;

class operator TxVector.Add(Scalar: Extended; V: TxVector): TxVector;
begin
  Result := V + Scalar;
end;

procedure TxVector.CheckUnique;
begin
  if NotUnique(@FData) then
    FData := Copy(FData);
end;

class operator TxVector.Divide(V1, V2: TxVector): TxVector;
var
  I: Integer;
begin
  if (V1.FCount <> V2.FCount) then
    raise EMathError.Create(sWRONG_SIZE);
  Result.Create(V1.FCount);
  for I := 0 to V1.FCount - 1 do
    Result.FData[I] := V1.FData[I] / V2.FData[I];
end;

class operator TxVector.Divide(V: TxVector; Scalar: Extended): TxVector;
begin
  Result := V * (1 / Scalar);
end;

class operator TxVector.Implicit(V: TAbstractVector): TxVector;
begin
  Result.Create(V);
end;

procedure TxVector.Fill(Value: Extended);
var
  I: Integer;
begin
  if Value = 0 then
  begin
    FData := nil;
    SetLength(FData, FCount);
  end
  else
    for I := 0 to FCount - 1 do
      FData[I] := Value;
end;

function TxVector.GetElement(Index: Word): Extended;
begin
  if (Index = 0) or (Index > FCount) then
    raise EMathError.Create(sWRONG_ELEMENT);
  Result := FData[Pred(Index)];
end;

class operator TxVector.Multiply(V: TxVector; Scalar: Extended): TxVector;
var
  I: Integer;
begin
  Result.Create(V.FCount);
  for I := 0 to V.FCount - 1 do
    Result.FData[I] := V.FData[I] * Scalar;
end;

class operator TxVector.Multiply(Scalar: Extended; V: TxVector): TxVector;
begin
  Result := V * Scalar;
end;

function TxVector.Norm: Extended;
begin
  Result := System.Math.Norm(FData);
end;

class operator TxVector.Multiply(V1, V2: TxVector): TxVector;
begin
  if (V1.FCount <> 3) or (V2.FCount <> 3) then
    raise EMathError.Create(sWRONG_SIZE);
  Result.Create(V1.FCount);
  Result.FData[0] := V1.FData[1] * V2.FData[2] - V1.FData[2] * V2.FData[1];
  Result.FData[1] := V1.FData[2] * V2.FData[0] - V1.FData[0] * V2.FData[2];
  Result.FData[2] := V1.FData[0] * V2.FData[1] - V1.FData[1] * V2.FData[0];
end;

function TxVector.ScalarMult(V: TxVector): Extended;
var
  I: Integer;
begin
  if V.FCount <> FCount then
    raise EMathError.Create(sWRONG_SIZE);
  Result := 0.0;
  for I := 0 to FCount - 1 do
    Result := Result + FData[I] * V.FData[I];
end;

procedure TxVector.SetElement(Index: Word; Value: Extended);
begin
  if (Index = 0) or (Index > FCount) then
    raise EMathError.Create(sWRONG_ELEMENT);
  CheckUnique;
  FData[Pred(Index)] := Value;
end;

class operator TxVector.Subtract(V1, V2: TxVector): TxVector;
var
  I: Integer;
begin
  if (V1.FCount <> V2.FCount) then
    raise EMathError.Create(sWRONG_SIZE);
  Result.Create(V1.FCount);
  for I := 0 to V1.FCount - 1 do
    Result.FData[I] := V1.FData[I] - V2.FData[I];
end;

class operator TxVector.Subtract(Scalar: Extended; V: TxVector): TxVector;
var
  I: Integer;
begin
  Result.Create(V.FCount);
  for I := 0 to V.FCount - 1 do
    Result.FData[I] := Scalar - V.FData[I];
end;

class operator TxVector.Subtract(V: TxVector; Scalar: Extended): TxVector;
var
  I: Integer;
begin
  Result.Create(V.FCount);
  for I := 0 to V.Count - 1 do
    Result.FData[I] := V.FData[I] - Scalar;
end;

function TxVector.SumOfElments: Extended;
begin
  Result := Sum(FData);
end;

function TxVector.SumOfSquares: Extended;
begin
  Result := System.Math.SumOfSquares(FData);
end;

function TxVector.ToQuat: TxQuaternion;
var
  ModVec: Extended;
  C1, C2: Extended;
begin
  if (FCount <> 3) then
    raise EMathError.Create(sWRONG_SIZE);
  ModVec := Norm;
  C1 := Cos(ModVec / 2);
  if ModVec > 1e-15 then
    C2 := Sin(ModVec / 2) / ModVec
  else
    C2 := 1;
  Result := [C1, FData[0] * C2, FData[1] * C2, FData[2] * C2];
end;

function TxVector.TruncateSTI: TxVector;
const
  Int32Max: Double = Integer.MaxValue;
  Int32Min: Double = Integer.MinValue;
var
  I: Integer;
begin
  Result.Create(FCount);
  for I := 0 to FCount - 1 do
  begin
    if (FData[I] >= Int32Min) and (FData[I] <= Int32Max) then
      Result.FData[I] := Trunc(FData[I])
    else
      if (FData[I] < Int32Min) then
        Result.FData[I] := Int32Min
      else
        Result.FData[I] := Int32Max;
  end;
end;

{ TxQuatHelper }

function TxQuatHelper.ToMatrix: TxMatrix;
begin
  Result.Create(3, 3);
  Result[1, 1] := Sqr(FData[0]) + Sqr(FData[1]) - Sqr(FData[2]) - Sqr(FData[3]);
  Result[1, 2] := 2 * (FData[1] * FData[2] - FData[0] * FData[3]);
  Result[1, 3] := 2 * (FData[1] * FData[3] + FData[0] * FData[2]);
  Result[2, 1] := 2 * (FData[1] * FData[2] + FData[0] * FData[3]);
  Result[2, 2] := Sqr(FData[0]) - Sqr(FData[1]) + Sqr(FData[2]) - Sqr(FData[3]);
  Result[2, 3] := 2 * (FData[2] * FData[3] - FData[0] * FData[1]);
  Result[3, 1] := 2 * (FData[1] * FData[3] - FData[0] * FData[2]);
  Result[3, 2] := 2 * (FData[2] * FData[3] + FData[0] * FData[1]);
  Result[3, 3] := Sqr(FData[0]) - Sqr(FData[1]) - Sqr(FData[2]) + Sqr(FData[3]);
end;

{ TVecHelper }

function TVecHelper.ToDiagMatrix: TxMatrix;
var
  I: Integer;
begin
  Result.Create(FCount, FCount);
  for I := 0 to FCount - 1 do
    Result.FData[I, I] := FData[I];
end;

procedure Init(Obj, TypeInfoOfObj: Pointer; Offset: Integer = 0);
const
  DefaultRowCount = 3;
  DefaultColCount = 3;
  VectorTypeName = 'TVector';
  MatrixTypeName = 'TMatrix';
var
  RTTIContext: TRttiContext;
  Field : TRttiField;
  ArrFld: TRttiArrayType;
  I: Integer;
  Dim: TCustomAttribute;
  RowCount, ColCount: Integer;
  OffsetFromArray: Integer;
begin
  for Field in RTTIContext.GetType(TypeInfoOfObj).GetFields do
  begin
    if Field.FieldType <> nil then
    begin
      RowCount := DefaultRowCount;
      ColCount := DefaultColCount;
      for Dim in Field.GetAttributes do
      begin
        RowCount := (Dim as TDim).RowCount;
        ColCount := (Dim as TDim).ColCount;
      end;
      if Field.FieldType.TypeKind = tkArray then
      begin
        ArrFld := TRttiArrayType(Field.FieldType);
        if ArrFld.ElementType.TypeKind = tkRecord then
        begin
          for I := 0 to ArrFld.TotalElementCount - 1 do
          begin
            OffsetFromArray := I * ArrFld.ElementType.TypeSize;
            if ArrFld.ElementType.Name = VectorTypeName then
              PxVector(Integer(Obj) +
                      Field.Offset +
                      OffsetFromArray +
                      Offset)^ := TxVector.Create(RowCount)
            else if ArrFld.ElementType.Name = MatrixTypeName then
              PxMatrix(Integer(Obj) +
                      Field.Offset +
                      OffsetFromArray +
                      Offset)^ := TxMatrix.Create(RowCount, ColCount)
            else
              Init(Obj, ArrFld.ElementType.Handle, Field.Offset + OffsetFromArray);
          end;
        end;
      end
      else if Field.FieldType.TypeKind = tkRecord then
      begin
        if Field.FieldType.Name = VectorTypeName then
          PxVector(Integer(Obj) +
                  Field.Offset +
                  Offset)^ := TxVector.Create(RowCount)
        else if Field.FieldType.Name = MatrixTypeName then
          PxMatrix(Integer(Obj) +
                  Field.Offset +
                  Offset)^ := TxMatrix.Create(RowCount, ColCount)
        else
          Init(Obj, Field.FieldType.Handle, Field.Offset)
      end;
    end;
  end;
end;

{ TDim }

constructor TDim.Create(ARowCount: Integer; AColCount: Integer = 0);
begin
  FRowCount := ARowCount;
  FColCount := AColCount;
end;


{ TGLPoint2D }
//
function TGLPoint2D.Create(X, Y : Single): TGLPoint2D;
begin
  Result.X := X;
  Result.Y := Y;
end;

procedure TGLPoint2D.SetPosition(const X, Y: Single);
begin
  Self.X := X;
  Self.Y := Y;
end;

function TGLPoint2D.Length: Single;
begin
  Result := Sqrt(Self.X * Self.X + Self.Y * Self.Y);
end;

function TGLPoint2D.Add(const APoint2D: TGLPoint2D): TGLPoint2D;
begin
  Result.SetPosition(Self.X + APoint2D.X, Self.Y + APoint2D.Y);
end;

function TGLPoint2D.Distance(const APoint2D: TGLPoint2D): Single;
begin
  Result := Sqrt(Sqr(Self.X - APoint2D.X) +  Sqr(Self.Y - APoint2D.Y));
end;

procedure TGLPoint2D.Offset(const ADeltaX, ADeltaY: Single);
begin
  Self.X := Self.X + ADeltaX;
  Self.Y := Self.Y + ADeltaY;
end;

class function TGLPoint2D.PointInCircle(const Point, Center: TGLPoint2D;
  const Radius: Integer): Boolean;
begin
  Result := Point.Distance(Center) <= Radius;
end;

{ TGLPoint3D }
//
function TGLPoint3D.Create(X, Y, Z: Single): TGLPoint3D;
begin
  Result.X := X;
  Result.Y := Y;
  Result.Z := Z;
end;

function TGLPoint3D.Add(const AGLPoint3D: TGLPoint3D): TGLPoint3D;
begin
  Result.X := Self.X + AGLPoint3D.X;
  Result.Y := Self.Y + AGLPoint3D.Y;
  Result.Z := Self.Z + AGLPoint3D.Z;
end;

function TGLPoint3D.Distance(const APoint3D: TGLPoint3D): Single;
begin
  Result := Self.Length - APoint3D.Length;
end;

function TGLPoint3D.Length: Single;
begin
  Result := Sqrt(Self.X * Self.X + Self.Y * Self.Y + Self.Z * Self.Z);
end;

procedure TGLPoint3D.Offset(const ADeltaX, ADeltaY, ADeltaZ: Single);
begin
  Self.X := Self.X + ADeltaX;
  Self.Y := Self.Y + ADeltaY;
  Self.Z := Self.Z + ADeltaZ;
end;

procedure TGLPoint3D.SetPosition(const X, Y, Z: Single);
begin
  Self.X := X;
  Self.Y := Y;
  Self.Z := Z;
end;

{ TGLVector2D }
//
function TGLVector2D.Create(const AX, AY, AW: Single): TGLVector2D;
begin
  Result.X := AX;
  Result.Y := AY;
  Result.W := AW;
end;

function TGLVector2D.CrossProduct(const AVector: TGLVector2D): TGLVector2D;
begin
  Result.X := (Self.Y * AVector.W) - (Self.W * AVector.Y);
  Result.Y := (Self.W * AVector.X) - (Self.X * AVector.W);
  Result.W := (Self.X * AVector.Y) - (Self.Y * AVector.X);
end;

function TGLVector2D.DotProduct(const AVector: TGLVector2D): Single;
begin
  Result := (Self.X * AVector.X) + (Self.Y * AVector.Y) + (Self.W * AVector.W);
end;

function TGLVector2D.Add(const AVector2D: TGLVector2D): TGLVector2D;
begin
  Result.X := Self.X + AVector2D.X;
  Result.Y := Self.Y + AVector2D.Y;
  Result.W := 1.0;
end;

function TGLVector2D.Length: Single;
begin
  Result := Sqrt((Self.X * Self.X) + (Self.Y * Self.Y));
end;

function TGLVector2D.Norm: Single;
begin
  result := Sqr(Self.X) + Sqr(Self.Y);
end;

function TGLVector2D.Normalize: TGLVector2D;
var
  invLen: Single;
  vn: Single;
const
  Tolerance: Single = 1E-12;
begin
  vn := Self.Norm;
  if vn > Tolerance then
  begin
    invLen := 1/Sqrt(vn);
    Result.X := Self.X * invLen;
    Result.Y := Self.Y * invLen;
  end
  else
    Result := Self;
end;

//---------------------------------
{ TGLVector3D }
//---------------------------------
function TGLVector3D.Create(const AX, AY, AZ, AW: Single): TGLVector3D;
begin
  Result.X := AX;
  Result.Y := AY;
  Result.Z := AZ;
  Result.W := AW;
end;

function TGLVector3D.Add(const AVector3D: TGLVector3D): TGLVector3D;
begin
  Result.X := Self.X + AVector3D.X;
  Result.Y := Self.Y + AVector3D.Y;
  Result.Z := Self.Z + AVector3D.Z;
  Result.W := 1.0;
end;

function TGLVector3D.Norm: Single;
begin
  result := Self.X * Self.X + Self.Y * Self.Y + Self.Z * Self.Z;
end;

function TGLVector3D.Normalize: TGLVector3D;
var
  invLen: Single;
  vn: Single;
const
  Tolerance: Single = 1E-12;
begin
  vn := Self.Norm;
  if vn > 0 then
  begin
    invLen := 1/Sqrt(vn);
    Result.X := Self.X * invLen;
    Result.Y := Self.Y * invLen;
    Result.Z := Self.Z * invLen;
    Result.W := 0;
  end
  else
    Result := Self;
end;

function TGLVector3D.DotProduct(const AVector3D: TVector3D): Single;
begin
  Result := (Self.X * AVector3D.X) + (Self.Y * AVector3D.Y) + (Self.Z * AVector3D.Z);
end;

function TGLVector3D.CrossProduct(const AVector3D: TVector3D): TVector3D;
begin
  Result.X := (Self.Y * AVector3D.Z) - (Self.Z * AVector3D.Y);
  Result.Y := (Self.Z * AVector3D.X) - (Self.X * AVector3D.Z);
  Result.Z := (Self.X * AVector3D.Y) - (Self.Y * AVector3D.X);
end;

function TGLVector3D.Length: Single;
begin
  Result := Sqrt((Self.X * Self.X) + (Self.Y * Self.Y) + (Self.Z * Self.Z));
end;

//---------------------------------
{ TxQuaternion }
//---------------------------------

function TxQuaternion.GetElement(Index: Byte): Extended;
begin
  if (Index > 3) then
    raise EMathError.Create(sWRONG_ELEMENT);
  Result := FData[Index];
end;

class operator TxQuaternion.Implicit(V: TAbstractVector): TxQuaternion;
begin
  if (Length(V) <> 4) then
    raise EMathError.Create(sWRONG_SIZE);
  Move(V[0], Result.FData, SizeOf(Result.FData));
end;

function TxQuaternion.Inv: TxQuaternion;
begin
    Result := [FData[0], -FData[1], -FData[2], -FData[3]];
end;

class operator TxQuaternion.Multiply(Scalar: Extended; Q: TxQuaternion): TxQuaternion;
begin
  Result := Q * Scalar;
end;

class operator TxQuaternion.Multiply(Q: TxQuaternion; Sc: Extended): TxQuaternion;
begin
  Result := [Q.FData[0] * Sc, Q.FData[1] * Sc, Q.FData[2] * Sc, Q.FData[3] * Sc];
end;

class operator TxQuaternion.Multiply(Q1, Q2: TxQuaternion): TxQuaternion;
var
  Mat: TxMatrix;
begin
  Mat := [[Q1.FData[0], -Q1.FData[1], -Q1.FData[2], -Q1.FData[3]],
          [Q1.FData[1],  Q1.FData[0], -Q1.FData[3],  Q1.FData[2]],
          [Q1.FData[2],  Q1.FData[3],  Q1.FData[0], -Q1.FData[1]],
          [Q1.FData[3], -Q1.FData[2],  Q1.FData[1],  Q1.FData[0]]];
  Result := Mat * Q2;
end;

constructor TxQuaternion.Create(Q: TAbstractVector);
begin
  if Length(Q) <> 4 then
    raise EMathError.Create(sWRONG_SIZE);
  Move(Q[0], FData[0], SizeOf(FData));
end;

procedure TxQuaternion.SetElement(Index: Byte; Value: Extended);
begin
  if (Index > 3) then
    raise EMathError.Create(sWRONG_ELEMENT);
  FData[Index] := Value;
end;

function TxQuaternion.TruncateSTI: TxQuaternion;
const
  Int32Max: Double = Integer.MaxValue;
  Int32Min: Double = Integer.MinValue;

function xTrunc(Value: Extended): Double;
begin
   if (Value >= Int32Min) and (Value <= Int32Max) then
     Result := Trunc(Value)
   else
     if (Value < Int32Min) then
       Result := Int32Min
     else
       Result := Int32Max;
end;

begin
  Result[0] := xTrunc(FData[0]);
  Result[1] := xTrunc(FData[1]);
  Result[2] := xTrunc(FData[2]);
  Result[3] := xTrunc(FData[3]);
end;


end.