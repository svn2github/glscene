//
// This unit is part of the GLScene Project, http://glscene.org
//
{: VectorGeometryEXT.<p>

   This module is designed to store seldom-used algebraic functions.<p>

	<b>History : </b><font size=-1><ul>
	   <li>27/02/10 - Yar - Creation
	</ul></font>
}

unit VectorGeometryEXT;

interface

{$I GLScene.inc}

uses
  VectorGeometry, VectorTypes;

type
  PLongWordVector = ^TLongWordVector;
  PLongWordArray = PLongWordVector;
  TLongWordVector = array[0..cMaxArray] of LongWord;
  TVector3lw = array[0..2] of LongWord;
  PVector3lw = ^TVector3lw;

procedure SetVector(var v: TVector4s; const x, y, z, w: integer); overload;
procedure SetVector(var v: TVector4s; const vSrc: TVector4s); overload;
procedure SetVector(var v: TVector2s; const vSrc: TVector2s); overload;
procedure SetVector(var v: TVector2s; const x, y: Integer); overload;
procedure SetVector(var v: TVector2f; const x, y: Single); overload;
function VectorMake(const x, y, z: Integer): TVector3i; overload;
function VectorMake(const x, y: Integer): TVector2s; overload;
function VectorAdd(const v1, v2: TVector3i): TVector3i; overload;
function VectorAdd(const v1, v2: TVector2s): TVector2s; overload;
function VectorSubtract(const v1, v2: TVector2s): TVector2s; overload;
procedure AddVector(var v1: TVector2s; const v2: TVector2s); overload;
procedure ScaleVector(var v: TVector2s; factor: Single); overload;

function CreateRotationMatrix(angleX, angleY, angleZ: single): TMatrix;
overload;
function CreateProjectionMatrix(const fov: single; aspect: single; znear:
  single; zfar: single): TMatrix; overload;
function CreateProjectionMatrix(const Rigth, Left, Top, Bottom: Integer):
  TMatrix; overload;

implementation

const
  // to be used as descriptive indices
  X = 0;
  Y = 1;
  Z = 2;
  W = 3;

// SetVector
//

procedure SetVector(var v: TVector4s; const x, y, z, w: integer);
begin
  v[X] := x;
  v[Y] := y;
  v[Z] := z;
  v[W] := w;
end;

procedure SetVector(var v: TVector4s; const vSrc: TVector4s);
begin
  v[X] := vSrc[X];
  v[Y] := vSrc[Y];
  v[Z] := vSrc[Z];
  v[W] := vSrc[W];
end;

procedure SetVector(var v: TVectorZs; const vSrc: TVectorZs);
begin
  v[X] := vSrc[X];
  v[Y] := vSrc[Y];
end;

procedure SetVector(var v: TVectorZs; const x, y: Integer); overload;
begin
  v[X] := x;
  v[Y] := y;
end;

procedure SetVector(var v: TVectorZf; const x, y: single);
begin
  v[X] := x;
  v[Y] := y;
end;

// VectorMake
//

function VectorMake(const x, y, z: Integer): TVector3i;
begin
  Result[X] := x;
  Result[Y] := y;
  Result[Z] := z;
end;

function VectorMake(const x, y: Integer): TVectorZs;
begin
  Result[X] := x;
  Result[Y] := y;
end;

// VectorAdd
//

function VectorAdd(const vY, vZ: TVector3i): TVector3i;
begin
  Result[X] := vY[X] + vZ[X];
  Result[Y] := vY[Y] + vZ[Y];
  Result[Z] := vY[Z] + vZ[Z];
end;

function VectorAdd(const vY, v2: TVector2s): TVector2s;
begin
  Result[X] := vY[X] + v2[X];
  Result[Y] := vY[Y] + v2[Y];
end;

// VectorSubtract
//

function VectorSubtract(const vY, v2: TVector2s): TVector2s;
begin
  Result[X] := vY[X] - v2[X];
  Result[Y] := vY[Y] - v2[Y];
end;

procedure AddVector(var vY: TVector2s; const v2: TVector2s);
begin
  vY[X] := vY[X] + v2[X];
  vY[Y] := vY[Y] + v2[Y];
end;

procedure ScaleVector(var v: TVector2s; factor: Single);
begin
  v[X] := Round(v[X] * factor);
  v[Y] := Round(v[Y] * factor);
end;

function CreateRotationMatrix(angleX, angleY, angleZ: single): TMatrix;
var
  c1, c2, c3, s1, s2, s3: single;
begin
  SinCos(angleX, s1, c1);
  SinCos(angleY, s2, c2);
  SinCos(angleZ, s3, c3);
  Result := EmptyHmgMatrix;
  Result[W, W] := 1.0;
  Result[X, X] := s3 * s2 * s1 + c3 * c1;
  Result[Y, X] := c2 * s1;
  Result[Z, X] := s3 * c1 - c3 * s2 * s1;
  Result[X, Y] := s3 * s2 * c1 - c3 * s1;
  Result[Y, Y] := c2 * c1;
  Result[Z, Y] := -c3 * s2 * c1 - s3 * s1;
  Result[X, Z] := -s3 * c2;
  Result[Y, Z] := s2;
  Result[Z, Z] := c3 * c2;
end;

function CreateProjectionMatrix(const fov: single; aspect: single; znear:
  single; zfar: single): TMatrix;
var
  f, zdif: Single;
begin
  f := 1.0 / TAN(DegToRad(fov) / 2.0);
  Result := IdentityHmgMatrix;
  Result[X, X] := f / aspect;
  Result[Y, Y] := f;
  zdif := znear - zfar;
  if zdif <> X then
  begin
    Result[Z, Z] := (zfar + znear) / zdif;
    Result[W, Z] := 2 * zfar * znear / zdif;
  end;
  Result[Z, W] := -1.0;
  Result[W, W] := 0.0;
end;

function CreateProjectionMatrix(const Rigth, Left, Top, Bottom: Integer):
  TMatrix;
begin
  Result := IdentityHmgMatrix;
  Result[X, X] := -2 / (Rigth - Left);
  Result[Y, Y] := -2 / (Top - Bottom);
  Result[Z, Z] := -1.0;
  Result[W, X] := (Rigth + Left) / (Rigth - Left);
  Result[W, Y] := (Top + Bottom) / (Top - Bottom);
  Result[W, Z] := -EPSILON;
  Result[W, W] := 1.0;
end;

end.

