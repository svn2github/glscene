//
// This unit is part of the GLScene Project, http://glscene.org
//
{: VectorGeometryEXT.<p>

   This module is designed to store seldom-used algebraic functions.<p>

 <b>History : </b><font size=-1><ul>
     <li>27/04/10 - Yar - Maked TVector**EXT vectors with overloading operators
     <li>01/04/10 - Yar - Added CreateLookAtMatrix
    <li>27/02/10 - Yar - Creation
 </ul></font>
}

unit VectorGeometryEXT;

interface

{$I GLScene.inc}
{$IFDEF FPC}
{$mode objfpc}{$h+}
{$ENDIF}

uses
  VectorGeometry, VectorTypes;

type

  PVector2fEXT = ^TVector2fEXT;
  TVector2fEXT = packed {$IFNDEF FPC}record{$ELSE}object{$ENDIF}
    V: array[0..1] of Single;
    property X: Single read V[0] write V[0];
    property Y: Single read V[1] write V[1];
{$IFDEF GLS_COMPILER_2005_UP}
    class operator Add(const a, b: TVector2fEXT): TVector2fEXT; inline;
    class operator Subtract(const a, b: TVector2fEXT): TVector2fEXT; inline;
    class operator Multiply(const a, b: TVector2fEXT): TVector2fEXT; inline;
    class operator Multiply(const a: TVector2fEXT; b: Single): TVector2fEXT; inline;
    class operator Divide(const a, b: TVector2fEXT): TVector2fEXT; inline;
    class operator Divide(const a: TVector2fEXT; b: Single): TVector2fEXT; inline;
    class operator Equal(const a, b: TVector2fEXT): Boolean; inline;
    class operator Negative(const a: TVector2fEXT): TVector2fEXT; inline;
{$ENDIF}
    function Lerp(const a: TVector2fEXT; t: Single): TVector2fEXT; inline;
    function Dot(const a: TVector2fEXT): Single; inline;
    function Norm: Single; inline;
    function Normal: TVector2fEXT; inline;
    procedure Normalize;
    function Length: Single; inline;
    function Distance(const a: TVector2fEXT): Single;
    function IsNull: Boolean;
  end;

  PVector3f = ^TVector3fEXT;
  TVector3fEXT = packed {$IFNDEF FPC}record{$ELSE}object{$ENDIF}
    V: array[0..2] of Single;
    property X: Single read V[0] write V[0];
    property Y: Single read V[1] write V[1];
    property Z: Single read V[2] write V[2];
{$IFDEF GLS_COMPILER_2005_UP}
    class operator Implicit(const a: TVector3f): TVector3fEXT; inline;
    class operator Add(const a, b: TVector3fEXT): TVector3fEXT; inline;
    class operator Subtract(const a, b: TVector3fEXT): TVector3fEXT; inline;
    class operator Multiply(const a, b: TVector3fEXT): TVector3fEXT; inline;
    class operator Multiply(const a: TVector3fEXT; b: Single): TVector3fEXT; inline;
    class operator Divide(const a, b: TVector3fEXT): TVector3fEXT; inline;
    class operator Divide(const a: TVector3fEXT; b: Single): TVector3fEXT; inline;
    class operator Equal(const a, b: TVector3fEXT): Boolean; inline;
    class operator Negative(const a: TVector3fEXT): TVector3fEXT; inline;
{$ENDIF}
    function Lerp(const a: TVector3fEXT; t: Single): TVector3fEXT; inline;
    function Dot(const a: TVector3fEXT): Single; inline;
    function Cross(const a: TVector3fEXT): TVector3fEXT; inline;
    function Norm: Single; inline;
    function Normal: TVector3fEXT; inline;
    procedure Normalize;
    function Length: Single; inline;
    function Distance(const a: TVector3fEXT): Single;
    function IsNull: Boolean;
  end;

  PVector4f = ^TVector4fEXT;
  TVector4fEXT = packed {$IFNDEF FPC}record{$ELSE}object{$ENDIF}
  private
    function Get3f: TVector3fEXT; inline;
    procedure Set3f(Value: TVector3fEXT); inline;
  public
    V: array[0..3] of Single;
    property X: Single read V[0] write V[0];
    property Y: Single read V[1] write V[1];
    property Z: Single read V[2] write V[2];
    property W: Single read V[3] write V[3];
    property XYZ: TVector3fEXT read Get3f write Set3f;
{$IFDEF GLS_COMPILER_2005_UP}
    class operator Implicit(const a: TVector4f): TVector4fEXT; inline;
    class operator Add(const a, b: TVector4fEXT): TVector4fEXT; inline;
    class operator Subtract(const a, b: TVector4fEXT): TVector4fEXT; inline;
    class operator Multiply(const a, b: TVector4fEXT): TVector4fEXT; inline;
    class operator Multiply(const a: TVector4fEXT; b: Single): TVector4fEXT; inline;
    class operator Divide(const a, b: TVector4fEXT): TVector4fEXT; inline;
    class operator Divide(const a: TVector4fEXT; b: Single): TVector4fEXT; inline;
    class operator Equal(const a, b: TVector4fEXT): Boolean; inline;
    class operator Negative(const a: TVector4fEXT): TVector4fEXT; inline;
{$ENDIF}
    function Lerp(const a: TVector4fEXT; t: Single): TVector4fEXT; inline;
    function Dot(const a: TVector4fEXT): Single; inline;
    function Cross(const a: TVector4fEXT): TVector4fEXT; inline;
    function Norm: Single; inline;
    function Normal: TVector4fEXT; inline;
    procedure Normalize;
    function Length: Single; inline;
    function Distance(const a: TVector4fEXT): Single;
    function IsNull: Boolean;
  end;
  TVectorEXT = TVector4fEXT;

  PMatrix4fEXT = ^TMatrix4fEXT;
  TMatrix4fEXT = packed {$IFNDEF FPC}record{$ELSE}object{$ENDIF}
  private
    function  GetPos: TVector3fEXT;
    procedure SetPos(const vec: TVector3fEXT);
    function  GetRow(Index: Integer): TVector4fEXT;
    procedure SetRow(Index: Integer; const vec: TVector4fEXT);
  public
    V: array[0..15] of Single;
    property e00: Single read V[0] write V[0];
    property e10: Single read V[1] write V[1];
    property e20: Single read V[2] write V[2];
    property e30: Single read V[3] write V[3];
    property e01: Single read V[4] write V[4];
    property e11: Single read V[5] write V[5];
    property e21: Single read V[6] write V[6];
    property e31: Single read V[7] write V[7];
    property e02: Single read V[8] write V[8];
    property e12: Single read V[9] write V[9];
    property e22: Single read V[10] write V[10];
    property e32: Single read V[11] write V[11];
    property e03: Single read V[12] write V[12];
    property e13: Single read V[13] write V[13];
    property e23: Single read V[14] write V[14];
    property e33: Single read V[15] write V[15];
    property Row[Index: Integer]: TVector4fEXT read GetRow write SetRow;
{$IFDEF GLS_COMPILER_2005_UP}
    class operator Implicit(const a: TMatrix4f): TMatrix4fEXT;
    class operator Implicit(const a: TMatrix4fEXT): TMatrix4f;
    class operator Add(const a, b: TMatrix4fEXT): TMatrix4fEXT;
    class operator Multiply(const m: TMatrix4fEXT; const a: TVector3fEXT): TVector3fEXT;
    class operator Multiply(const m: TMatrix4fEXT; const a: TVector4fEXT): TVector4fEXT;
    class operator Multiply(const a, b: TMatrix4fEXT): TMatrix4fEXT;
    class operator Multiply(const a: TMatrix4fEXT; x: Single): TMatrix4fEXT;
    class operator Equal(const a, b: TMatrix4fEXT): Boolean; inline;
{$ENDIF}
    procedure Identity; inline;
    function Determinant: Single;
    function Inverse: TMatrix4fEXT;
    procedure Transpose;
    function FromVectorAngle(const vec: TVector3fEXT; Angle: Single): TMatrix4fEXT;
    procedure Ortho(Left, Right, Bottom, Top, ZNear, ZFar: Single);
    procedure Frustum(Left, Right, Bottom, Top, ZNear, ZFar: Single);
    procedure Perspective(FOV, Aspect, ZNear, ZFar: Single);
    procedure Translation(const vec: TVector3fEXT);
    procedure Rotation(Angle: Single; const vec: TVector3fEXT); overload;
    procedure Rotation(angleX, angleY, angleZ: Single); overload;
    procedure Scalable(const s: TVector3fEXT);
    procedure LookAt(const eye, center, normUp: TVectorEXT);
    property Position: TVector3fEXT read GetPos write SetPos;
  end;
  TMatrixEXT = TMatrix4fEXT;

function VectorMakeEXT(const x, y : Single) : TVector2fEXT; overload; inline;
function VectorMakeEXT(const x, y, z : Single) : TVector3fEXT; overload; inline;
function VectorMakeEXT(const x, y, z, w : Single) : TVector4fEXT; overload; inline;

{$IFDEF FPC}
operator +(const a, b: TVector2fEXT): TVector2fEXT; overload; inline;
operator -(const a, b: TVector2fEXT): TVector2fEXT; overload; inline;
operator *(const a, b: TVector2fEXT): TVector2fEXT; overload; inline;
operator *(const a: TVector2fEXT; b: Single): TVector2fEXT; overload; inline;
operator /(const a, b: TVector2fEXT): TVector2fEXT; overload; inline;
operator /(const a: TVector2fEXT; b: Single): TVector2fEXT; overload; inline;
operator =(const a, b: TVector2fEXT): Boolean; overload; inline;
operator -(const a: TVector2fEXT): TVector2fEXT; overload; inline;

operator := (const a: TVector3f): TVector3fEXT; overload; inline;
operator + (const a, b: TVector3fEXT): TVector3fEXT; overload; inline;
operator - (const a, b: TVector3fEXT): TVector3fEXT; overload; inline;
operator * (const a, b: TVector3fEXT): TVector3fEXT; overload; inline;
operator * (const a: TVector3fEXT; b: Single): TVector3fEXT; overload; inline;
operator / (const a, b: TVector3fEXT): TVector3fEXT; overload; inline;
operator / (const a: TVector3fEXT; b: Single): TVector3fEXT; overload; inline;
operator = (const a, b: TVector3fEXT): Boolean; overload; inline;
operator - (const a: TVector3fEXT): TVector3fEXT; overload; overload; inline;

operator :=(const a: TVector4f): TVector4fEXT; overload; inline;
operator +(const a, b: TVector4fEXT): TVector4fEXT; overload; inline;
operator -(const a, b: TVector4fEXT): TVector4fEXT; overload; inline;
operator *(const a, b: TVector4fEXT): TVector4fEXT; overload; inline;
operator *(const a: TVector4fEXT; b: Single): TVector4fEXT; overload; inline;
operator /(const a, b: TVector4fEXT): TVector4fEXT; overload; inline;
operator /(const a: TVector4fEXT; b: Single): TVector4fEXT; overload; inline;
operator =(const a, b: TVector4fEXT): Boolean; overload; inline;
operator -(const a: TVector4fEXT): TVector4fEXT; overload; inline;

operator :=(const a: TMatrix4f): TMatrix4fEXT; overload;
operator :=(const a: TMatrix4fEXT): TMatrix4f; overload;
operator +(const a, b: TMatrix4fEXT): TMatrix4fEXT; overload;
operator *(const m: TMatrix4fEXT; const a: TVector3fEXT): TVector3fEXT; overload;
operator *(const m: TMatrix4fEXT; const a: TVector4fEXT): TVector4fEXT; overload;
operator *(const a, b: TMatrix4fEXT): TMatrix4fEXT; overload;
operator *(const a: TMatrix4fEXT; x: Single): TMatrix4fEXT; overload;
operator =(const a, b: TMatrix4fEXT): Boolean; overload;
{$ENDIF}

implementation

function VectorMakeEXT(const x, y : Single) : TVector2fEXT;
begin
  Result.X := x;
  Result.Y := y;
end;

function VectorMakeEXT(const x, y, z : Single) : TVector3fEXT;
begin
  Result.X := x;
  Result.Y := y;
  Result.Z := z;
end;

function VectorMakeEXT(const x, y, z, w : Single) : TVector4fEXT;
begin
  Result.X := x;
  Result.Y := y;
  Result.Z := z;
  Result.W := w;
end;

{$IFDEF GLS_COMPILER_2005_UP}  {$region 'TVector2fEXT'} {$ENDIF}

{$IFDEF FPC}
operator + (const a, b: TVector2fEXT): TVector2fEXT;
{$ELSE}
class operator TVector2fEXT.Add(const a, b: TVector2fEXT): TVector2fEXT;
{$ENDIF}
begin
  Result.X := a.X+b.X;
  Result.Y := a.Y+b.Y;
end;

{$IFDEF FPC}
operator -(const a, b: TVector2fEXT): TVector2fEXT;
{$ELSE}
class operator TVector2fEXT.Subtract(const a, b: TVector2fEXT): TVector2fEXT;
{$ENDIF}
begin
  Result.X := a.X-b.X;
  Result.Y := a.Y-b.Y;
end;

{$IFDEF FPC}
operator *(const a, b: TVector2fEXT): TVector2fEXT;
{$ELSE}
class operator TVector2fEXT.Multiply(const a, b: TVector2fEXT): TVector2fEXT;
{$ENDIF}
begin
  Result.X := a.X*b.X;
  Result.Y := a.Y*b.Y;
end;

{$IFDEF FPC}
operator /(const a, b: TVector2fEXT): TVector2fEXT;
{$ELSE}
class operator TVector2fEXT.Divide(const a, b: TVector2fEXT): TVector2fEXT;
{$ENDIF}
begin
  Result.X := a.X/b.X;
  Result.Y := a.Y/b.Y;
end;

{$IFDEF FPC}
operator *(const a: TVector2fEXT; b: Single): TVector2fEXT;
{$ELSE}
class operator TVector2fEXT.Multiply(const a: TVector2fEXT; b: Single): TVector2fEXT;
{$ENDIF}
begin
  Result.X := a.X*b;
  Result.Y := a.Y*b;
end;

{$IFDEF FPC}
operator /(const a: TVector2fEXT; b: Single): TVector2fEXT;
{$ELSE}
class operator TVector2fEXT.Divide(const a: TVector2fEXT; b: Single): TVector2fEXT;
{$ENDIF}
begin
  Result.X := a.X/b;
  Result.Y := a.Y/b;
end;

{$IFDEF FPC}
operator =(const a, b: TVector2fEXT): Boolean;
{$ELSE}
class operator TVector2fEXT.Equal(const a, b: TVector2fEXT): Boolean;
{$ENDIF}
begin
  Result := (a.X = b.X) and (a.Y = b.Y);
end;

{$IFDEF FPC}
operator -(const a: TVector2fEXT): TVector2fEXT;
{$ELSE}
class operator TVector2fEXT.Negative(const a: TVector2fEXT): TVector2fEXT;
{$ENDIF}
begin
  Result.X := -a.X;
  Result.Y := -a.Y;
end;

function TVector2fEXT.Lerp(const a: TVector2fEXT; t: Single): TVector2fEXT;
begin
  Result.X := X+(a.X-X)*t;
  Result.Y := Y+(a.Y-Y)*t;
end;

function TVector2fEXT.Dot(const a: TVector2fEXT): Single;
begin
  Result := X*a.X+Y*a.Y;
end;

function TVector2fEXT.Norm: Single;
begin
  Result := X*X+Y*Y;
end;

function TVector2fEXT.Normal: TVector2fEXT;
var
  vn, invLen: Single;
begin
  vn:=Norm;
  if vn>0 then
  begin
    invLen:=RSqrt(vn);
    Result.X:=X*invLen;
    Result.Y:=Y*invLen;
  end;
end;

procedure TVector2fEXT.Normalize;
begin
  Self := Normal;
end;

function TVector2fEXT.Length: Single;
begin
  Result := Sqrt(Norm);
end;

function TVector2fEXT.Distance(const a: TVector2fEXT): Single;
var
  dif: TVector2fEXT;
begin
  dif := Self - a;
  Result := Sqrt(dif.Norm);
end;

function TVector2fEXT.IsNull: Boolean;
begin
  Result := (v[0]=0) and (v[1]=0);
end;
{$IFDEF GLS_COMPILER_2005_UP}  {$endregion} {$ENDIF}

{$IFDEF GLS_COMPILER_2005_UP}{$REGION 'TVector3fEXT'}{$ENDIF}

{$IFDEF FPC}
operator :=(const a: TVector3f): TVector3fEXT;
{$ELSE}
class operator TVector3fEXT.Implicit(const a: TVector3f): TVector3fEXT;
{$ENDIF}
begin
  Result.X := a[0];
  Result.Y := a[1];
  Result.Z := a[2];
end;

{$IFDEF FPC}
operator +(const a, b: TVector3fEXT): TVector3fEXT;
{$ELSE}
class operator TVector3fEXT.Add(const a, b: TVector3fEXT): TVector3fEXT;
{$ENDIF}
begin
  Result.X := a.X+b.X;
  Result.Y := a.Y+b.Y;
  Result.Z := a.Z+b.Z;
end;

{$IFDEF FPC}
operator -(const a, b: TVector3fEXT): TVector3fEXT;
{$ELSE}
class operator TVector3fEXT.Subtract(const a, b: TVector3fEXT): TVector3fEXT;
{$ENDIF}
begin
  Result.X := a.X-b.X;
  Result.Y := a.Y-b.Y;
  Result.Z := a.Z-b.Z;
end;

{$IFDEF FPC}
operator *(const a, b: TVector3fEXT): TVector3fEXT;
{$ELSE}
class operator TVector3fEXT.Multiply(const a, b: TVector3fEXT): TVector3fEXT;
{$ENDIF}
begin
  Result.X := a.X*b.X;
  Result.Y := a.Y*b.Y;
  Result.Z := a.Z*b.Z;
end;

{$IFDEF FPC}
operator /(const a, b: TVector3fEXT): TVector3fEXT;
{$ELSE}
class operator TVector3fEXT.Divide(const a, b: TVector3fEXT): TVector3fEXT;
{$ENDIF}
begin
  Result.X := a.X/b.X;
  Result.Y := a.Y/b.Y;
  Result.Z := a.Z/b.Z;
end;

{$IFDEF FPC}
operator *(const a: TVector3fEXT; b: Single): TVector3fEXT;
{$ELSE}
class operator TVector3fEXT.Multiply(const a: TVector3fEXT; b: Single): TVector3fEXT;
{$ENDIF}
begin
  Result.X := a.X*b;
  Result.Y := a.Y*b;
  Result.Z := a.Z*b;
end;

{$IFDEF FPC}
operator /(const a: TVector3fEXT; b: Single): TVector3fEXT;
{$ELSE}
class operator TVector3fEXT.Divide(const a: TVector3fEXT; b: Single): TVector3fEXT;
{$ENDIF}
begin
  Result.X := a.X/b;
  Result.Y := a.Y/b;
  Result.Z := a.Z/b;
end;

{$IFDEF FPC}
operator =(const a, b: TVector3fEXT): Boolean;
{$ELSE}
class operator TVector3fEXT.Equal(const a, b: TVector3fEXT): Boolean;
{$ENDIF}
begin
  Result := (a.X = b.X) and (a.Y = b.Y) and (a.Z = b.Z);
end;

{$IFDEF FPC}
operator -(const a: TVector3fEXT): TVector3fEXT;
{$ELSE}
class operator TVector3fEXT.Negative(const a: TVector3fEXT): TVector3fEXT;
{$ENDIF}
begin
  Result.X := -a.X;
  Result.Y := -a.Y;
  Result.Z := -a.Z
end;

function TVector3fEXT.Lerp(const a: TVector3fEXT; t: Single): TVector3fEXT;
begin
  Result.X := X+(a.X-X)*t;
  Result.Y := Y+(a.Y-Y)*t;
  Result.Z := Z+(a.Z-Z)*t;
end;

function TVector3fEXT.Dot(const a: TVector3fEXT): Single;
begin
  Result := X*a.X+Y*a.Y+Z*a.Z;
end;

function TVector3fEXT.Cross(const a: TVector3fEXT): TVector3fEXT;
begin
  Result.X:=Y*a.Z-Z*a.Y;
  Result.Y:=Z*a.X-X*a.Z;
  Result.Z:=X*a.Y-Y*a.X;
end;

function TVector3fEXT.Norm: Single;
begin
  Result := X*X+Y*Y+Z*Z;
end;

function TVector3fEXT.Normal: TVector3fEXT;
var
  vn, invLen: Single;
begin
  vn:=Norm;
  if vn>0 then
  begin
    invLen:=RSqrt(vn);
    Result.X:=X*invLen;
    Result.Y:=Y*invLen;
    Result.Z:=Z*invLen;
  end;
end;

procedure TVector3fEXT.Normalize;
begin
  Self := Normal;
end;

function TVector3fEXT.Length: Single;
begin
  Result := Sqrt(Norm);
end;

function TVector3fEXT.Distance(const a: TVector3fEXT): Single;
var
  dif: TVector3fEXT;
begin
  dif := Self - a;
  Result := Sqrt(dif.Norm);
end;

function TVector3fEXT.IsNull: Boolean;
begin
  Result := ((v[0]=0) and (v[1]=0) and (v[2]=0));
end;
{$IFDEF GLS_COMPILER_2005_UP}{$endregion}{$ENDIF}

{$IFDEF GLS_COMPILER_2005_UP}  {$region 'TVector4fEXT'} {$ENDIF}

{$IFDEF FPC}
operator :=(const a: TVector4f): TVector4fEXT;
{$ELSE}
class operator TVector4fEXT.Implicit(const a: TVector4f): TVector4fEXT;
{$ENDIF}
begin
  Result.X := a[0];
  Result.Y := a[1];
  Result.Z := a[2];
  Result.W := a[3];
end;

{$IFDEF FPC}
operator +(const a, b: TVector4fEXT): TVector4fEXT;
{$ELSE}
class operator TVector4fEXT.Add(const a, b: TVector4fEXT): TVector4fEXT;
{$ENDIF}
begin
  Result.X := a.X+b.X;
  Result.Y := a.Y+b.Y;
  Result.Z := a.Z+b.Z;
  Result.W := a.W+b.W;
end;

{$IFDEF FPC}
operator -(const a, b: TVector4fEXT): TVector4fEXT;
{$ELSE}
class operator TVector4fEXT.Subtract(const a, b: TVector4fEXT): TVector4fEXT;
{$ENDIF}
begin
  Result.X := a.X-b.X;
  Result.Y := a.Y-b.Y;
  Result.Z := a.Z-b.Z;
  Result.W := a.W-b.W;
end;

{$IFDEF FPC}
operator *(const a, b: TVector4fEXT): TVector4fEXT;
{$ELSE}
class operator TVector4fEXT.Multiply(const a, b: TVector4fEXT): TVector4fEXT;
{$ENDIF}
begin
  Result.X := a.X*b.X;
  Result.Y := a.Y*b.Y;
  Result.Z := a.Z*b.Z;
  Result.W := a.W*b.W;
end;

{$IFDEF FPC}
operator /(const a, b: TVector4fEXT): TVector4fEXT;
{$ELSE}
class operator TVector4fEXT.Divide(const a, b: TVector4fEXT): TVector4fEXT;
{$ENDIF}
begin
  Result.X := a.X/b.X;
  Result.Y := a.Y/b.Y;
  Result.Z := a.Z/b.Z;
  Result.W := a.W/b.W;
end;

{$IFDEF FPC}
operator *(const a: TVector4fEXT; b: Single): TVector4fEXT;
{$ELSE}
class operator TVector4fEXT.Multiply(const a: TVector4fEXT; b: Single): TVector4fEXT;
{$ENDIF}
begin
  Result.X := a.X*b;
  Result.Y := a.Y*b;
  Result.Z := a.Z*b;
  Result.W := a.W*b;
end;

{$IFDEF FPC}
operator /(const a: TVector4fEXT; b: Single): TVector4fEXT;
{$ELSE}
class operator TVector4fEXT.Divide(const a: TVector4fEXT; b: Single): TVector4fEXT;
{$ENDIF}
begin
  Result.X := a.X/b;
  Result.Y := a.Y/b;
  Result.Z := a.Z/b;
  Result.W := a.W/b;
end;

{$IFDEF FPC}
operator =(const a, b: TVector4fEXT): Boolean;
{$ELSE}
class operator TVector4fEXT.Equal(const a, b: TVector4fEXT): Boolean;
{$ENDIF}
begin
  Result := (a.X = b.X) and (a.Y = b.Y) and (a.Z = b.Z) and (a.W = b.W);
end;

{$IFDEF FPC}
operator -(const a: TVector4fEXT): TVector4fEXT;
{$ELSE}
class operator TVector4fEXT.Negative(const a: TVector4fEXT): TVector4fEXT;
{$ENDIF}
begin
  Result.X := -a.X;
  Result.Y := -a.Y;
  Result.Z := -a.Z;
  Result.W := -a.W;
end;

function TVector4fEXT.Get3f: TVector3fEXT;
begin
 Result.X := V[0];
 Result.Y := V[1];
 Result.Z := V[2];
end;

procedure TVector4fEXT.Set3f(Value: TVector3fEXT);
begin
 V[0] := Value.X;
 V[1] := Value.Y;
 V[2] := Value.Z;
end;

function TVector4fEXT.Lerp(const a: TVector4fEXT; t: Single): TVector4fEXT;
begin
  Result.X := X+(a.X-X)*t;
  Result.Y := Y+(a.Y-Y)*t;
  Result.Z := Z+(a.Z-Z)*t;
  Result.W := W+(a.W-W)*t;
end;

function TVector4fEXT.Dot(const a: TVector4fEXT): Single;
begin
  Result := X*a.X+Y*a.Y+Z*a.Z+W*a.W;
end;

function TVector4fEXT.Cross(const a: TVector4fEXT): TVector4fEXT;
begin
  Result.X:=Y*a.Z-Z*a.Y;
  Result.Y:=Z*a.X-X*a.Z;
  Result.Z:=X*a.Y-Y*a.X;
  Result.W:=0;
end;

function TVector4fEXT.Norm: Single;
begin
  Result := X*X+Y*Y+Z*Z;
end;

function TVector4fEXT.Normal: TVector4fEXT;
var
  vn, invLen: Single;
begin
  vn:=Norm;
  if vn>0 then
  begin
    invLen:=RSqrt(vn);
    Result.X:=X*invLen;
    Result.Y:=Y*invLen;
    Result.Z:=Z*invLen;
    Result.W := 0;
  end;
end;

procedure TVector4fEXT.Normalize;
begin
  Self := Normal;
end;

function TVector4fEXT.Length: Single;
begin
  Result := Sqrt(Norm);
end;

function TVector4fEXT.Distance(const a: TVector4fEXT): Single;
var
  dif: TVector4fEXT;
begin
  dif := Self - a;
  Result := Sqrt(dif.Norm);
end;

function TVector4fEXT.IsNull: Boolean;
begin
  Result := (v[0]=0) and (v[1]=0) and (v[2]=0) and (v[3]=0);
end;
{$IFDEF GLS_COMPILER_2005_UP}  {$endregion} {$ENDIF}

{$IFDEF GLS_COMPILER_2005_UP}  {$region 'TMatrix4fEXT'} {$ENDIF}

{$IFDEF FPC}
operator :=(const a: TMatrix4f): TMatrix4fEXT;
{$ELSE}
class operator TMatrix4fEXT.Implicit(const a: TMatrix4f): TMatrix4fEXT;
{$ENDIF}
begin
  Move(a[0][0], Result.V[0], SizeOf(TMatrix4f));
end;

{$IFDEF FPC}
operator :=(const a: TMatrix4fEXT): TMatrix4f;
{$ELSE}
class operator TMatrix4fEXT.Implicit(const a: TMatrix4fEXT): TMatrix4f;
{$ENDIF}
begin
  Move(a.V[0], Result[0][0], SizeOf(TMatrix4f));
end;

{$IFDEF FPC}
operator +(const a, b: TMatrix4fEXT): TMatrix4fEXT;
{$ELSE}
class operator TMatrix4fEXT.Add(const a, b: TMatrix4fEXT): TMatrix4fEXT;
{$ENDIF}
var
  i : Integer;
begin
  for i := 0 to 3 do
    Result.Row[i] := a.Row[i] + b.Row[i];
end;

{$IFDEF FPC}
operator *(const m: TMatrix4fEXT; const a: TVector3fEXT): TVector3fEXT;
{$ELSE}
class operator TMatrix4fEXT.Multiply(const m: TMatrix4fEXT; const a: TVector3fEXT): TVector3fEXT;
{$ENDIF}
begin
  with m do
    Result := VectorGeometryEXT.VectorMakeEXT(
      e00 * a.x + e01 * a.y + e02 * a.z + e03,
      e10 * a.x + e11 * a.y + e12 * a.z + e13,
      e20 * a.x + e21 * a.y + e22 * a.z + e23);
end;

{$IFDEF FPC}
operator *(const m: TMatrix4fEXT; const a: TVector4fEXT): TVector4fEXT;
{$ELSE}
class operator TMatrix4fEXT.Multiply(const m: TMatrix4fEXT; const a: TVector4fEXT): TVector4fEXT;
{$ENDIF}
begin
  with m do
    Result := VectorGeometryEXT.VectorMakeEXT(
      e00 * a.x + e01 * a.y + e02 * a.z + e03,
      e10 * a.x + e11 * a.y + e12 * a.z + e13,
      e20 * a.x + e21 * a.y + e22 * a.z + e23,
      e30 * a.x + e31 * a.y + e32 * a.z + e33);
end;

{$IFDEF FPC}
operator *(const a, b: TMatrix4fEXT): TMatrix4fEXT;
{$ELSE}
class operator TMatrix4fEXT.Multiply(const a, b: TMatrix4fEXT): TMatrix4fEXT;
{$ENDIF}
begin
  with Result do
  begin
    e00 := a.e00 * b.e00 + a.e01 * b.e10 + a.e02 * b.e20 + a.e03 * b.e30;
    e10 := a.e10 * b.e00 + a.e11 * b.e10 + a.e12 * b.e20 + a.e13 * b.e30;
    e20 := a.e20 * b.e00 + a.e21 * b.e10 + a.e22 * b.e20 + a.e23 * b.e30;
    e30 := a.e30 * b.e00 + a.e31 * b.e10 + a.e32 * b.e20 + a.e33 * b.e30;
    e01 := a.e00 * b.e01 + a.e01 * b.e11 + a.e02 * b.e21 + a.e03 * b.e31;
    e11 := a.e10 * b.e01 + a.e11 * b.e11 + a.e12 * b.e21 + a.e13 * b.e31;
    e21 := a.e20 * b.e01 + a.e21 * b.e11 + a.e22 * b.e21 + a.e23 * b.e31;
    e31 := a.e30 * b.e01 + a.e31 * b.e11 + a.e32 * b.e21 + a.e33 * b.e31;
    e02 := a.e00 * b.e02 + a.e01 * b.e12 + a.e02 * b.e22 + a.e03 * b.e32;
    e12 := a.e10 * b.e02 + a.e11 * b.e12 + a.e12 * b.e22 + a.e13 * b.e32;
    e22 := a.e20 * b.e02 + a.e21 * b.e12 + a.e22 * b.e22 + a.e23 * b.e32;
    e32 := a.e30 * b.e02 + a.e31 * b.e12 + a.e32 * b.e22 + a.e33 * b.e32;
    e03 := a.e00 * b.e03 + a.e01 * b.e13 + a.e02 * b.e23 + a.e03 * b.e33;
    e13 := a.e10 * b.e03 + a.e11 * b.e13 + a.e12 * b.e23 + a.e13 * b.e33;
    e23 := a.e20 * b.e03 + a.e21 * b.e13 + a.e22 * b.e23 + a.e23 * b.e33;
    e33 := a.e30 * b.e03 + a.e31 * b.e13 + a.e32 * b.e23 + a.e33 * b.e33;
  end;
end;

{$IFDEF FPC}
operator *(const a: TMatrix4fEXT; x: Single): TMatrix4fEXT;
{$ELSE}
class operator TMatrix4fEXT.Multiply(const a: TMatrix4fEXT; x: Single): TMatrix4fEXT;
{$ENDIF}
var
  i : Integer;
begin
  for i := 0 to 3 do
      Result.Row[i] := a.Row[i] * x;
end;

{$IFDEF FPC}
operator =(const a, b: TMatrix4fEXT): Boolean;
{$ELSE}
class operator TMatrix4fEXT.Equal(const a, b: TMatrix4fEXT): Boolean;
{$ENDIF}
var
  i : Integer;
begin
  Result := False;
  for i := 0 to 15 do
    if a.V[i] <> b.V[i] then
      exit;
  Result := True;
end;

function  TMatrix4fEXT.GetRow(Index: Integer): TVector4fEXT;
begin
  Result.X := V[Index*4];
  Result.Y := V[Index*4+1];
  Result.Z := V[Index*4+2];
  Result.W := V[Index*4+3];
end;

procedure TMatrix4fEXT.SetRow(Index: Integer; const vec: TVector4fEXT);
begin
  V[Index*4] := vec.X;
  V[Index*4+1] := vec.Y;
  V[Index*4+2] := vec.Z;
  V[Index*4+3] := vec.W;
end;

procedure TMatrix4fEXT.Identity;
begin
  Move(IdentityHmgMatrix, Self.V[0], SizeOf(TMatrix4f));
end;

function TMatrix4fEXT.Determinant: Single;
begin
  Result := e00 * (e11 * (e22 * e33 - e32 * e23) - e21 * (e12 * e33 - e32 * e13) + e31 * (e12 * e23 - e22 * e13)) -
            e10 * (e01 * (e22 * e33 - e32 * e23) - e21 * (e02 * e33 - e32 * e03) + e31 * (e02 * e23 - e22 * e03)) +
            e20 * (e01 * (e12 * e33 - e32 * e13) - e11 * (e02 * e33 - e32 * e03) + e31 * (e02 * e13 - e12 * e03)) -
            e30 * (e01 * (e12 * e23 - e22 * e13) - e11 * (e02 * e23 - e22 * e03) + e21 * (e02 * e13 - e12 * e03));
end;


function TMatrix4fEXT.Inverse: TMatrix4fEXT;
var
  D : Single;
begin
  D := 1 / Determinant;
  Result.e00 :=  (e11 * (e22 * e33 - e32 * e23) - e21 * (e12 * e33 - e32 * e13) + e31 * (e12 * e23 - e22 * e13)) * D;
  Result.e01 := -(e01 * (e22 * e33 - e32 * e23) - e21 * (e02 * e33 - e32 * e03) + e31 * (e02 * e23 - e22 * e03)) * D;
  Result.e02 :=  (e01 * (e12 * e33 - e32 * e13) - e11 * (e02 * e33 - e32 * e03) + e31 * (e02 * e13 - e12 * e03)) * D;
  Result.e03 := -(e01 * (e12 * e23 - e22 * e13) - e11 * (e02 * e23 - e22 * e03) + e21 * (e02 * e13 - e12 * e03)) * D;
  Result.e10 := -(e10 * (e22 * e33 - e32 * e23) - e20 * (e12 * e33 - e32 * e13) + e30 * (e12 * e23 - e22 * e13)) * D;
  Result.e11 :=  (e00 * (e22 * e33 - e32 * e23) - e20 * (e02 * e33 - e32 * e03) + e30 * (e02 * e23 - e22 * e03)) * D;
  Result.e12 := -(e00 * (e12 * e33 - e32 * e13) - e10 * (e02 * e33 - e32 * e03) + e30 * (e02 * e13 - e12 * e03)) * D;
  Result.e13 :=  (e00 * (e12 * e23 - e22 * e13) - e10 * (e02 * e23 - e22 * e03) + e20 * (e02 * e13 - e12 * e03)) * D;
  Result.e20 :=  (e10 * (e21 * e33 - e31 * e23) - e20 * (e11 * e33 - e31 * e13) + e30 * (e11 * e23 - e21 * e13)) * D;
  Result.e21 := -(e00 * (e21 * e33 - e31 * e23) - e20 * (e01 * e33 - e31 * e03) + e30 * (e01 * e23 - e21 * e03)) * D;
  Result.e22 :=  (e00 * (e11 * e33 - e31 * e13) - e10 * (e01 * e33 - e31 * e03) + e30 * (e01 * e13 - e11 * e03)) * D;
  Result.e23 := -(e00 * (e11 * e23 - e21 * e13) - e10 * (e01 * e23 - e21 * e03) + e20 * (e01 * e13 - e11 * e03)) * D;
  Result.e30 := -(e10 * (e21 * e32 - e31 * e22) - e20 * (e11 * e32 - e31 * e12) + e30 * (e11 * e22 - e21 * e12)) * D;
  Result.e31 :=  (e00 * (e21 * e32 - e31 * e22) - e20 * (e01 * e32 - e31 * e02) + e30 * (e01 * e22 - e21 * e02)) * D;
  Result.e32 := -(e00 * (e11 * e32 - e31 * e12) - e10 * (e01 * e32 - e31 * e02) + e30 * (e01 * e12 - e11 * e02)) * D;
  Result.e33 :=  (e00 * (e11 * e22 - e21 * e12) - e10 * (e01 * e22 - e21 * e02) + e20 * (e01 * e12 - e11 * e02)) * D;
end;

procedure TMatrix4fEXT.Transpose;
var
  f: Single;
begin
  f:=e01; e01:=e10; e10:=f;
  f:=e02; e02:=e20; e20:=f;
  f:=e03; e03:=e30; e30:=f;
  f:=e12; e12:=e21; e21:=f;
  f:=e13; e13:=e31; e31:=f;
  f:=e23; e23:=e32; e32:=f;
end;

function TMatrix4fEXT.FromVectorAngle(const vec: TVector3fEXT; Angle: Single): TMatrix4fEXT;
var
  s, c  : Extended;
  ic : Single;
  xy, yz, zx, xs, ys, zs, icxy, icyz, iczx : Single;
begin
  SinCos(Angle, s, c);
  ic := 1 - c;

  with Result, vec.Normal do
  begin
    xy := x * y;
    yz := y * z;
    zx := z * x;
    xs := x * s;
    ys := y * s;
    zs := z * s;
    icxy := ic * xy;
    icyz := ic * yz;
    iczx := ic * zx;
    e00 := ic * x * x + c;  e01 := icxy - zs;       e02 := iczx + ys;       e03 := 0.0;
    e10 := icxy + zs;       e11 := ic * y * y + c;  e12 := icyz - xs;       e13 := 0.0;
    e20 := iczx - ys;       e21 := icyz + xs;       e22 := ic * z * z + c;  e23 := 0.0;
    e30 := 0.0;             e31 := 0.0;             e32 := 0.0;             e33 := 1.0;
  end;
end;

procedure TMatrix4fEXT.Translation(const vec: TVector3fEXT);
var
  m : TMatrix4fEXT;
begin
  m.Identity;
  m.Position := vec;
  Self := Self * m;
end;

procedure TMatrix4fEXT.Rotation(Angle: Single; const vec: TVector3fEXT);
var
  m : TMatrix4fEXT;
begin
  m := m.FromVectorAngle(vec, DegToRad(Angle));
  Self := Self * m;
end;

procedure TMatrix4fEXT.Rotation(angleX, angleY, angleZ: Single);
var
  c1, c2, c3, s1, s2, s3: single;
begin
  SinCos(angleX, s1, c1);
  SinCos(angleY, s2, c2);
  SinCos(angleZ, s3, c3);
  FillChar(V[0], 16*SizeOf(Single), $00);
  e33 := 1.0;
  e00 := s3 * s2 * s1 + c3 * c1;
  e10 := c2 * s1;
  e20 := s3 * c1 - c3 * s2 * s1;
  e01 := s3 * s2 * c1 - c3 * s1;
  e11 := c2 * c1;
  e21 := -c3 * s2 * c1 - s3 * s1;
  e02 := -s3 * c2;
  e12 := s2;
  e22 := c3 * c2;
end;

procedure TMatrix4fEXT.Scalable(const s: TVector3fEXT);
var
  m : TMatrix4fEXT;
begin
  m.Identity;
  m.e00 := s.x;
  m.e11 := s.y;
  m.e22 := s.z;
  Self := m * Self;
end;

procedure TMatrix4fEXT.Ortho(Left, Right, Bottom, Top, ZNear, ZFar: Single);
begin
  e00 := 2 / (Right - Left);
  e10 := 0;
  e20 := 0;
  e30 := 0;

  e01 := 0;
  e11 := 2 / (Top - Bottom);
  e21 := 0;
  e31 := 0;

  e02 := 0;
  e12 := 0;
  e22 := -2 / (ZFar - ZNear);
  e32 := 0;

  e03 := (Right + Left) / (Right - Left);
  e13 := (Top + Bottom) / (Top - Bottom);
  e23 := (ZFar + ZNear) / (ZFar - ZNear);
  e33 := 1;
end;

procedure TMatrix4fEXT.Frustum(Left, Right, Bottom, Top, ZNear, ZFar: Single);
begin
  e00 := 2 * ZNear / (Right - Left);
  e10 := 0;
  e20 := 0;
  e30 := 0;

  e01 := 0;
  e11 := 2 * ZNear / (Top - Bottom);
  e21 := 0;
  e31 := 0;

  e02 := (Right + Left) / (Right - Left);
  e12 := (Top + Bottom) / (Top - Bottom);
  e22 := -(ZFar + ZNear) / (ZFar - ZNear);
  e32 := -1;

  e03 := 0;
  e13 := 0;
  e23 := -2 * ZFar * ZNear / (ZFar - ZNear);
  e33 := 0;
end;

procedure TMatrix4fEXT.Perspective(FOV, Aspect, ZNear, ZFar: Single);
var
  x, y : Single;
begin
  FOV := MinFloat(179.9, MaxFloat(0, FOV));
  y := ZNear * Tan(DegToRad(FOV) * 0.5);
  x := y * Aspect;
  Frustum(-x, x, -y, y, ZNear, ZFar);
end;

function TMatrix4fEXT.GetPos: TVector3fEXT;
begin
  Result := VectorGeometryEXT.VectorMakeEXT(e03, e13, e23);
end;

procedure TMatrix4fEXT.SetPos(const vec: TVector3fEXT);
begin
  e03 := vec.x;
  e13 := vec.y;
  e23 := vec.z;
end;

procedure TMatrix4fEXT.LookAt(const eye, center, normUp: TVectorEXT);
var
  XAxis, YAxis, ZAxis, negEye: TVectorEXT;
begin
  ZAxis := center - eye;
  ZAxis.Normalize;
  XAxis := ZAxis.Cross(normUp);
  XAxis.Normalize;
  YAxis := XAxis.Cross(ZAxis);
  Row[0] := XAxis;
  Row[1] := YAxis;
  Row[2] := -ZAxis;
  Row[3] := NullHmgPoint;
  Transpose;
  negEye := -eye;
  negEye.W := 1;
  negEye := Self * negEye;
  Row[3] := negEye;
end;
{$IFDEF GLS_COMPILER_2005_UP}  {$endregion} {$ENDIF}

end.
