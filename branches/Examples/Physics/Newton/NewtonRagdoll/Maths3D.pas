unit Maths3D;
{
  General 3D Math Equations
}

interface

uses
  Winapi.Windows,
  Winapi.OpenGL;

type
  TGLMatrixf4 = array[0..3, 0..3] of Single;
  // Note: Declare the following in Projects->Options->Conditionals not in this unit! - Stucuk
  // {$DEFINE NEWTON_DOUBLE_PRECISION} // This is needed when you want to use double precision
{$IFDEF NEWTON_DOUBLE_PRECISION}
  Float = Double;
  TMatrix4 = TGLMatrixd4;
{$ELSE}
  Float = Single;
  TMatrix4 = TGLMatrixf4;
{$ENDIF}
  Long_double = Extended;

  Int = Integer;
  __int8 = ShortInt;
  __int16 = SmallInt;
  __int32 = LongInt;
  __int64 = Int64;
  NChar = ShortInt;
  Unsigned_char = Byte;
  Short = SmallInt;
  Unsigned_short = Word;
  Long = LongInt;
  Unsigned_long = LongWord;
  Unsigned_int = Cardinal;
  size_t = Cardinal;
  CharArray = Array [0 .. 255] of Char;

  PInt = ^Int;
  P__int8 = ^__int8;
  P__int16 = ^__int16;
  P__int32 = ^__int32;
  P__int64 = ^__int64;
  P2Char = ^NChar;
  PUnsigned_char = ^Unsigned_char;
  PShort = ^Short;
  PUnsigned_short = ^Unsigned_short;
  PLong = ^Long;
  PUnsigned_long = ^Unsigned_long;
  PUnsigned_int = ^Unsigned_int;
  Psize_t = ^size_t;
  PFloat = ^Float;
  PLong_double = ^Long_double;
  PCharArray = ^CharArray;

  TVec2 = Array [0 .. 1] of Float;
  TVec3 = Array [0 .. 2] of Float;
  TVec4 = Array [0 .. 3] of Float;

  TVec2f = Array [0 .. 1] of Single;
  TVec2d = Array [0 .. 1] of Double;
  TVec3f = Array [0 .. 2] of Single;
  TVec3d = Array [0 .. 2] of Double;

  TColour3 = Array [0 .. 2] of Byte;

const
  M_PI_180 = 0.017453292519943295769236907684886;
  M_TWO_PI = 6.283185307179586476925286766559;
  NullTVec3: TVec3 = (0, 0, 0);
  NullMatrix4: TMatrix4 = ((0, 0, 0, 0), (0, 0, 0, 0), (0, 0, 0, 0),
    (0, 0, 0, 0));
  IdentityMatrix: TMatrix4 = ((1, 0, 0, 0), (0, 1, 0, 0), (0, 0, 1, 0),
    (0, 0, 0, 1));

function SetVector(X, Y, Z: Float): TVec3; overload;
function SetVector(X, Y: Float): TVec2; overload;
function SetColour(R, G, B: Byte): TColour3;
function DEG2RAD(a: Float): Float;
function Normalize(var v: TVec3): Float;
function Normalize2(v: TVec3): TVec3;
function MultiplyVector(v: TVec3; s: Float): TVec3; overload;
function MultiplyVector(v, v2: TVec3): TVec3; overload;
function MultiplyVector(v: TVec2; s: Float): TVec2; overload;
function MultiplyVector(v, v2: TVec2): TVec2; overload;
function DivideVector(v: TVec3; s: Float): TVec3; overload;
function DivideVector(v: TVec2; s: Float): TVec2; overload;
function DivideVector(v, v2: TVec2): TVec2; overload;
function DivideVector(v, v2: TVec3): TVec3; overload;
function AddVector(v1, v2: TVec3): TVec3; overload;
function AddVector(v1, v2: TVec2): TVec2; overload;
function SubtractVector(v1, v2: TVec3): TVec3; overload;
function SubtractVector(v1, v2: TVec2): TVec2; overload;
function CrossProduct(v1, v2: TVec3): TVec3;
function PlaneNormal(v1, v2, v3: TVec3): TVec3;
function IsSameVec3(v1, v2: TVec3): Boolean;

procedure GetMinBB(var BB: TVec3; Const v: TVec3);
procedure GetMaxBB(var BB: TVec3; Const v: TVec3);

function GetCenterOfBounds(Bmax, Bmin: TVec3): TVec3;
function GetCenterAndSizeBounds(Bmax, Bmin: TVec3; var Size: TVec3)
  : TVec3; overload;
function GetCenterAndSizeBounds(Bmax, Bmin: TVec2; var Size: TVec2)
  : TVec2; overload;

procedure MatrixAddTransform(var Matrix: TMatrix4; Position: TVec3);
procedure MatrixSetTransform(var Matrix: TMatrix4; Position: TVec3);
function MatrixGetTransform(Matrix: TMatrix4): TVec3;

function ApplyMatrixToVec3(M: TMatrix4; v: TVec3): TVec3;

// function Matrix_Multiply(m1 : TMatrix4; m2 : TMatrix4) : TMatrix4;
function Matrix_MakeYawMatrix(Angle: Float): TMatrix4;
function Matrix_MakeRollMatrix(Angle: Float): TMatrix4;
function Matrix_MakePitchMatrix(Angle: Float): TMatrix4;

function MatrixMultiply(M1, M2: TMatrix4): TMatrix4;

procedure Matrix_Roll(var M: TMatrix4; Angle: Float);
procedure Matrix_Pitch(var M: TMatrix4; Angle: Float);
procedure Matrix_Yaw(var M: TMatrix4; Angle: Float);

function CreateRotationMatrix(Axis: TVec3; Angle: Float): TMatrix4;

function ConvertVec3(Value: TVec3f): TVec3d; overload;
function ConvertVec3(Value: TVec3d): TVec3f; overload;

function Matrix_Transpose(M: TMatrix4): TMatrix4;
function Matrix_Inverse(var M: TMatrix4): TMatrix4;

function TPointToTVec2(P: TPoint): TVec2;
function TPointToTVec3(P: TPoint): TVec3;
function TVec2ToTPoint(v: TVec2): TPoint;
function TVec3ToTPoint(v: TVec3): TPoint;

function IsNull(v: TVec2): Boolean; overload;
function IsNull(v: TVec3): Boolean; overload;

procedure FixBB(var Min, Max: TVec2); overload;
procedure FixBB(var Min, Max: TVec3); overload;

function GetRandomColour(NoDark: Boolean = False;
  NoLight: Boolean = False): TVec3;

//==================================================
implementation
//==================================================

function SetVector(X, Y, Z: Float): TVec3;
begin
  Result[0] := X;
  Result[1] := Y;
  Result[2] := Z;
end;

function SetVector(X, Y: Float): TVec2;
begin
  Result[0] := X;
  Result[1] := Y;
end;

function SetColour(R, G, B: Byte): TColour3;
begin
  Result[0] := R;
  Result[1] := G;
  Result[2] := B;
end;

function DEG2RAD(a: Float): Float;
begin
  Result := a * M_PI_180;
end;

function Normalize(var v: TVec3): Float;
var
  l: Float;
begin
  l := sqrt(v[0] * v[0] + v[1] * v[1] + v[2] * v[2]);

  if (l > 0) then
  begin
    v[0] := v[0] / l;
    v[1] := v[1] / l;
    v[2] := v[2] / l;
  end;
  Result := l;
end;

function Normalize2(v: TVec3): TVec3;
var
  l: Float;
begin
  l := sqrt(v[0] * v[0] + v[1] * v[1] + v[2] * v[2]);

  if (l > 0) then
  begin
    Result[0] := v[0] / l;
    Result[1] := v[1] / l;
    Result[2] := v[2] / l;
  end
  else
    Result := v;
end;

function MultiplyVector(v: TVec3; s: Float): TVec3;
begin
  Result[0] := v[0] * s;
  Result[1] := v[1] * s;
  Result[2] := v[2] * s;
end;

function MultiplyVector(v, v2: TVec3): TVec3;
begin
  Result[0] := v[0] * v2[0];
  Result[1] := v[1] * v2[1];
  Result[2] := v[2] * v2[2];
end;

function MultiplyVector(v: TVec2; s: Float): TVec2;
begin
  Result[0] := v[0] * s;
  Result[1] := v[1] * s;
end;

function MultiplyVector(v, v2: TVec2): TVec2;
begin
  Result[0] := v[0] * v2[0];
  Result[1] := v[1] * v2[1];
end;

function DivideVector(v: TVec3; s: Float): TVec3;
begin
  Result[0] := v[0] / s;
  Result[1] := v[1] / s;
  Result[2] := v[2] / s;
end;

function DivideVector(v, v2: TVec2): TVec2;
begin
  Result[0] := v[0] / v2[0];
  Result[1] := v[1] / v2[1];
end;

function DivideVector(v, v2: TVec3): TVec3;
begin
  Result[0] := v[0] / v2[0];
  Result[1] := v[1] / v2[1];
  Result[2] := v[2] / v2[2];
end;

function DivideVector(v: TVec2; s: Float): TVec2;
begin
  Result[0] := v[0] / s;
  Result[1] := v[1] / s;
end;

function AddVector(v1, v2: TVec3): TVec3;
begin
  Result[0] := v1[0] + v2[0];
  Result[1] := v1[1] + v2[1];
  Result[2] := v1[2] + v2[2];
end;

function AddVector(v1, v2: TVec2): TVec2;
begin
  Result[0] := v1[0] + v2[0];
  Result[1] := v1[1] + v2[1];
end;

function SubtractVector(v1, v2: TVec3): TVec3;
begin
  Result[0] := v1[0] - v2[0];
  Result[1] := v1[1] - v2[1];
  Result[2] := v1[2] - v2[2];
end;

function SubtractVector(v1, v2: TVec2): TVec2;
begin
  Result[0] := v1[0] - v2[0];
  Result[1] := v1[1] - v2[1];
end;

function CrossProduct(v1, v2: TVec3): TVec3;
begin
  Result[0] := v1[1] * v2[2] - v1[2] * v2[1];
  Result[1] := v1[2] * v2[0] - v1[0] * v2[2];
  Result[2] := v1[0] * v2[1] - v1[1] * v2[0];
end;

function PlaneNormal(v1, v2, v3: TVec3): TVec3;
begin
  Result := CrossProduct(SubtractVector(v2, v3), SubtractVector(v2, v1));
  Normalize(Result);
end;

function IsSameVec3(v1, v2: TVec3): Boolean;
begin
  Result := (v1[0] = v2[0]) and (v1[1] = v2[1]) and (v1[2] = v2[2]);
end;

procedure GetMinBB(var BB: TVec3; Const v: TVec3);
begin
  if v[0] < BB[0] then
    BB[0] := v[0];

  if v[1] < BB[1] then
    BB[1] := v[1];

  if v[2] < BB[2] then
    BB[2] := v[2];
end;

procedure GetMaxBB(var BB: TVec3; Const v: TVec3);
begin
  if v[0] > BB[0] then
    BB[0] := v[0];

  if v[1] > BB[1] then
    BB[1] := v[1];

  if v[2] > BB[2] then
    BB[2] := v[2];
end;

function GetCenterOfBounds(Bmax, Bmin: TVec3): TVec3;
begin
  Result := AddVector(DivideVector(SubtractVector(Bmax, Bmin), 2), Bmin);
end;

function GetCenterAndSizeBounds(Bmax, Bmin: TVec3; var Size: TVec3): TVec3;
begin
  Size := SubtractVector(Bmax, Bmin);
  Result := AddVector(DivideVector(Size, 2), Bmin);
end;

function GetCenterAndSizeBounds(Bmax, Bmin: TVec2; var Size: TVec2): TVec2;
begin
  Size := SubtractVector(Bmax, Bmin);
  Result := AddVector(DivideVector(Size, 2), Bmin);
end;

procedure MatrixAddTransform(var Matrix: TMatrix4; Position: TVec3);
begin
  Matrix[3, 0] := Matrix[3, 0] + Position[0];
  Matrix[3, 1] := Matrix[3, 1] + Position[1];
  Matrix[3, 2] := Matrix[3, 2] + Position[2];
end;

procedure MatrixSetTransform(var Matrix: TMatrix4; Position: TVec3);
begin
  Matrix[3, 0] := Position[0];
  Matrix[3, 1] := Position[1];
  Matrix[3, 2] := Position[2];
end;

function MatrixGetTransform(Matrix: TMatrix4): TVec3;
begin
  Result[0] := Matrix[3, 0];
  Result[1] := Matrix[3, 1];
  Result[2] := Matrix[3, 2];
end;

Function ApplyMatrixToVec3(M: TMatrix4; v: TVec3): TVec3;
begin
  Result[0] := (v[0] * M[0, 0] + v[1] * M[1, 0] + v[2] * M[2, 0] + M[3, 0]);
  Result[1] := (v[0] * M[0, 1] + v[1] * M[1, 1] + v[2] * M[2, 1] + M[3, 1]);
  Result[2] := (v[0] * M[0, 2] + v[1] * M[1, 2] + v[2] * M[2, 2] + M[3, 2]);
end;

function Matrix_Multiply(M1: TMatrix4; M2: TMatrix4): TMatrix4;
var
  R, c, i: Byte;
  t: TMatrix4;
begin
  // Multiply two matrices.
  t := NullMatrix4;
  for R := 0 to 3 do
    for c := 0 to 3 do
      for i := 0 to 3 do
        t[R, c] := t[R, c] + (M1[R, i] * M2[i, c]);
  Result := t;
end;

function Matrix_MakeYawMatrix(Angle: Float): TMatrix4;
var
  CA: Float;
  SA: Float;
begin
  SA := Sin(Angle);
  CA := Cos(Angle);

  Result := IdentityMatrix;
  Result[0, 0] := CA;
  Result[0, 2] := -SA;
  Result[1, 1] := 1;
  Result[2, 0] := SA;
  Result[2, 2] := CA;
  Result[3, 3] := 1;
end;

// =============================================================================
// Matrix_MakeRollMatrix
// =============================================================================
function Matrix_MakeRollMatrix(Angle: Float): TMatrix4;
var
  CA: Float;
  SA: Float;
begin
  SA := Sin(Angle);
  CA := Cos(Angle);
  Result := IdentityMatrix;
  Result[0, 0] := CA;
  Result[0, 1] := SA;
  Result[1, 0] := -SA;
  Result[1, 1] := CA;
  Result[2, 2] := 1;
  Result[3, 3] := 1;
end;

// =============================================================================
// Matrix_MakePitchMatrix
// =============================================================================
function Matrix_MakePitchMatrix(Angle: Float): TMatrix4;
var
  CA: Float;
  SA: Float;
begin
  SA := Sin(Angle);
  CA := Cos(Angle);
  Result := IdentityMatrix;
  Result[0, 0] := 1;
  Result[1, 1] := CA;
  Result[1, 2] := SA;
  Result[2, 1] := -SA;
  Result[2, 2] := CA;
  Result[3, 3] := 1;
end;

function MatrixMultiply(M1, M2: TMatrix4): TMatrix4;
// multiplies two 4x4 matrices
var
  i, J: Integer;
  TM: TMatrix4;

begin
  for i := 0 to 3 do
    for J := 0 to 3 do
      TM[i, J] := M1[i, 0] * M2[0, J] + M1[i, 1] * M2[1, J] + M1[i, 2] *
        M2[2, J] + M1[i, 3] * M2[3, J];
  Result := TM;
end;

procedure SinCos(Theta: Extended; var Sin, Cos: Extended); assembler;
// calculates sine and cosine from the given angle Theta
// EAX contains address of Sin
// EDX contains address of Cos
// Theta is passed over the stack
asm
  FLD  Theta
  FSINCOS
  FSTP TBYTE PTR [EDX]    // cosine
  FSTP TBYTE PTR [EAX]    // sine
  FWAIT
end;

// ----------------------------------------------------------------------------------------------------------------------

function CreateRotationMatrix(Axis: TVec3; Angle: Float): TMatrix4;
// Creates a rotation matrix along the given Axis by the given Angle in radians.
var
  cosine, sine, Len, one_minus_cosine: Extended;

begin
  SinCos(Angle, sine, cosine);
  one_minus_cosine := 1 - cosine;
  Len := Normalize(Axis);

  if Len = 0 then
    Result := IdentityMatrix
  else
  begin
    Result[0, 0] := (one_minus_cosine * Sqr(Axis[0])) + cosine;
    Result[0, 1] := (one_minus_cosine * Axis[0] * Axis[1]) - (Axis[2] * sine);
    Result[0, 2] := (one_minus_cosine * Axis[2] * Axis[0]) + (Axis[1] * sine);
    Result[0, 3] := 0;

    Result[1, 0] := (one_minus_cosine * Axis[0] * Axis[1]) + (Axis[2] * sine);
    Result[1, 1] := (one_minus_cosine * Sqr(Axis[1])) + cosine;
    Result[1, 2] := (one_minus_cosine * Axis[1] * Axis[2]) - (Axis[0] * sine);
    Result[1, 3] := 0;

    Result[2, 0] := (one_minus_cosine * Axis[2] * Axis[0]) - (Axis[1] * sine);
    Result[2, 1] := (one_minus_cosine * Axis[1] * Axis[2]) + (Axis[0] * sine);
    Result[2, 2] := (one_minus_cosine * Sqr(Axis[2])) + cosine;
    Result[2, 3] := 0;

    Result[3, 0] := 0;
    Result[3, 1] := 0;
    Result[3, 2] := 0;
    Result[3, 3] := 1;
  end;
end;

procedure Matrix_Roll(var M: TMatrix4; Angle: Float);
begin

  M := MatrixMultiply(Matrix_MakeRollMatrix(DEG2RAD(Angle)), M);
end;

procedure Matrix_Pitch(var M: TMatrix4; Angle: Float);
begin
  M := MatrixMultiply(Matrix_MakePitchMatrix(DEG2RAD(Angle)), M);
end;

procedure Matrix_Yaw(var M: TMatrix4; Angle: Float);
begin
  M := MatrixMultiply(Matrix_MakeYawMatrix(DEG2RAD(Angle)), M);
end;

function ConvertVec3(Value: TVec3f): TVec3d;
begin
  Result[0] := Value[0];
  Result[1] := Value[1];
  Result[2] := Value[2];
end;

function ConvertVec3(Value: TVec3d): TVec3f;
begin
  Result[0] := Value[0];
  Result[1] := Value[1];
  Result[2] := Value[2];
end;

function Matrix_Transpose(M: TMatrix4): TMatrix4;
var
  i, J: Integer;
  TM: TMatrix4;
begin
  for i := 0 to 3 do
    for J := 0 to 3 do
      TM[J, i] := M[i, J];
  Result := TM;
end;

// internal version for the determinant of a 3x3 matrix
function MatrixDetInternal(a1, a2, a3, b1, b2, b3, c1, c2, c3: Float): Float;
begin
  Result := a1 * (b2 * c3 - b3 * c2) - b1 * (a2 * c3 - a3 * c2) + c1 *
    (a2 * b3 - a3 * b2);
end;

const
  X = 0;
  a = 0;
  Y = 1;
  B = 1;
  Z = 2;
  c = 2;
  W = 3;
  D = 3;
  Epsilon = 0.0001;

  // Determinant of a 4x4 matrix
function MatrixDeterminant(M: TMatrix4): Float;
var
  a1, a2, a3, a4, b1, b2, b3, b4, c1, c2, c3, c4, d1, d2, d3, d4: Float;
begin
  a1 := M[X, X];
  b1 := M[X, Y];
  c1 := M[X, Z];
  d1 := M[X, W];
  a2 := M[Y, X];
  b2 := M[Y, Y];
  c2 := M[Y, Z];
  d2 := M[Y, W];
  a3 := M[Z, X];
  b3 := M[Z, Y];
  c3 := M[Z, Z];
  d3 := M[Z, W];
  a4 := M[W, X];
  b4 := M[W, Y];
  c4 := M[W, Z];
  d4 := M[W, W];
  Result := a1 * MatrixDetInternal(b2, b3, b4, c2, c3, c4, d2, d3, d4) - b1 *
    MatrixDetInternal(a2, a3, a4, c2, c3, c4, d2, d3, d4) + c1 *
    MatrixDetInternal(a2, a3, a4, b2, b3, b4, d2, d3, d4) - d1 *
    MatrixDetInternal(a2, a3, a4, b2, b3, b4, c2, c3, c4);
end;

// Adjoint of a 4x4 matrix - used in the computation of the inverse
// of a 4x4 matrix
procedure MatrixAdjoint(var M: TMatrix4);
var
  a1, a2, a3, a4, b1, b2, b3, b4, c1, c2, c3, c4, d1, d2, d3, d4: Float;
begin
  a1 := M[X, X];
  b1 := M[X, Y];
  c1 := M[X, Z];
  d1 := M[X, W];
  a2 := M[Y, X];
  b2 := M[Y, Y];
  c2 := M[Y, Z];
  d2 := M[Y, W];
  a3 := M[Z, X];
  b3 := M[Z, Y];
  c3 := M[Z, Z];
  d3 := M[Z, W];
  a4 := M[W, X];
  b4 := M[W, Y];
  c4 := M[W, Z];
  d4 := M[W, W];
  // row column labeling reversed since we transpose rows & columns
  M[X, X] := MatrixDetInternal(b2, b3, b4, c2, c3, c4, d2, d3, d4);
  M[Y, X] := -MatrixDetInternal(a2, a3, a4, c2, c3, c4, d2, d3, d4);
  M[Z, X] := MatrixDetInternal(a2, a3, a4, b2, b3, b4, d2, d3, d4);
  M[W, X] := -MatrixDetInternal(a2, a3, a4, b2, b3, b4, c2, c3, c4);
  M[X, Y] := -MatrixDetInternal(b1, b3, b4, c1, c3, c4, d1, d3, d4);
  M[Y, Y] := MatrixDetInternal(a1, a3, a4, c1, c3, c4, d1, d3, d4);
  M[Z, Y] := -MatrixDetInternal(a1, a3, a4, b1, b3, b4, d1, d3, d4);
  M[W, Y] := MatrixDetInternal(a1, a3, a4, b1, b3, b4, c1, c3, c4);
  M[X, Z] := MatrixDetInternal(b1, b2, b4, c1, c2, c4, d1, d2, d4);
  M[Y, Z] := -MatrixDetInternal(a1, a2, a4, c1, c2, c4, d1, d2, d4);
  M[Z, Z] := MatrixDetInternal(a1, a2, a4, b1, b2, b4, d1, d2, d4);
  M[W, Z] := -MatrixDetInternal(a1, a2, a4, b1, b2, b4, c1, c2, c4);
  M[X, W] := -MatrixDetInternal(b1, b2, b3, c1, c2, c3, d1, d2, d3);
  M[Y, W] := MatrixDetInternal(a1, a2, a3, c1, c2, c3, d1, d2, d3);
  M[Z, W] := -MatrixDetInternal(a1, a2, a3, b1, b2, b3, d1, d2, d3);
  M[W, W] := MatrixDetInternal(a1, a2, a3, b1, b2, b3, c1, c2, c3);
end;

// multiplies all elements of a 4x4 matrix with a factor
procedure MatrixScale(var M: TMatrix4; Factor: Float);
var
  i, J: Integer;
begin
  for i := 0 to 3 do
    for J := 0 to 3 do
      M[i, J] := M[i, J] * Factor;
end;

// finds the inverse of a 4x4 matrix
function Matrix_Inverse(var M: TMatrix4): TMatrix4;
var
  Det: Float;
begin
  Result := M;
  Det := MatrixDeterminant(Result);
  if Abs(Det) < Epsilon then
    Result := IdentityMatrix
  else
  begin
    MatrixAdjoint(Result);
    MatrixScale(Result, 1 / Det);
  end;
end;

function TPointToTVec2(P: TPoint): TVec2;
begin
  Result[0] := P.X;
  Result[1] := P.Y;
end;

function TPointToTVec3(P: TPoint): TVec3;
begin
  Result[0] := P.X;
  Result[1] := P.Y;
end;

function TVec2ToTPoint(v: TVec2): TPoint;
begin
  Result.X := Trunc(v[0]);
  Result.Y := Trunc(v[1]);
end;

function TVec3ToTPoint(v: TVec3): TPoint;
begin
  Result.X := Trunc(v[0]);
  Result.Y := Trunc(v[1]);
end;

function IsNull(v: TVec2): Boolean;
begin
  Result := (v[0] = 0) and (v[1] = 0);
end;

function IsNull(v: TVec3): Boolean;
begin
  Result := (v[0] = 0) and (v[1] = 0) and (v[2] = 0);
end;

procedure FixBB(var Min, Max: TVec2);
var
  Temp: Float;
  X: Byte;
begin
  for X := 0 to 1 do
    if Min[X] > Max[X] then
    begin
      Temp := Min[X];
      Min[X] := Max[X];
      Max[X] := Temp;
    end;
end;

procedure FixBB(var Min, Max: TVec3);
var
  Temp: Float;
  X: Byte;
begin
  for X := 0 to 2 do
    if Min[X] > Max[X] then
    begin
      Temp := Min[X];
      Min[X] := Max[X];
      Max[X] := Temp;
    end;
end;

function GetRandomColour(NoDark: Boolean = False;
  NoLight: Boolean = False): TVec3;
begin
  Repeat
    Result := SetVector(Random(256) / 255, Random(256) / 255,
      Random(256) / 255);
  Until ((not NoDark) and (not NoLight)) or
    (((Result[0] > 120 / 255) or (Result[1] > 120 / 255) or
    (Result[2] > 120 / 255)) and (Result[0] < 200 / 255) and
    (Result[1] < 200 / 255) and (Result[2] < 200 / 255));
end;

end.
