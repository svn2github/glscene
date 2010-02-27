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

// SetVector
//

procedure SetVector(var v: TVector4s; const x, y, z, w: integer);
begin
  v[0] := x;
  v[1] := y;
  v[2] := z;
  v[3] := w;
end;

procedure SetVector(var v: TVector4s; const vSrc: TVector4s);
begin
  v[0] := vSrc[0];
  v[1] := vSrc[1];
  v[2] := vSrc[2];
  v[3] := vSrc[3];
end;

procedure SetVector(var v: TVector2s; const vSrc: TVector2s);
begin
  v[0] := vSrc[0];
  v[1] := vSrc[1];
end;

procedure SetVector(var v: TVector2s; const x, y: Integer); overload;
begin
  v[0] := x;
  v[1] := y;
end;

procedure SetVector(var v: TVector2f; const x, y: single);
begin
  v[0] := x;
  v[1] := y;
end;

// VectorMake
//

function VectorMake(const x, y, z: Integer): TVector3i;
begin
  Result[0] := x;
  Result[1] := y;
  Result[2] := z;
end;

function VectorMake(const x, y: Integer): TVector2s;
begin
  Result[0] := x;
  Result[1] := y;
end;

// VectorAdd
//

function VectorAdd(const v1, v2: TVector3i): TVector3i;
begin
  Result[0] := v1[0] + v2[0];
  Result[1] := v1[1] + v2[1];
  Result[2] := v1[2] + v2[2];
end;

function VectorAdd(const v1, v2: TVector2s): TVector2s;
begin
  Result[0] := v1[0] + v2[0];
  Result[1] := v1[1] + v2[1];
end;

// VectorSubtract
//

function VectorSubtract(const v1, v2: TVector2s): TVector2s;
begin
  Result[0] := v1[0] - v2[0];
  Result[1] := v1[1] - v2[1];
end;

procedure AddVector(var v1: TVector2s; const v2: TVector2s);
begin
  v1[0] := v1[0] + v2[0];
  v1[1] := v1[1] + v2[1];
end;

procedure ScaleVector(var v: TVector2s; factor: Single);
begin
  v[0] := Round(v[0] * factor);
  v[1] := Round(v[1] * factor);
end;

function CreateRotationMatrix(angleX, angleY, angleZ: single): TMatrix;
var
  c1, c2, c3, s1, s2, s3: single;
begin
  SinCos(angleX, s1, c1);
  SinCos(angleY, s2, c2);
  SinCos(angleZ, s3, c3);
  Result := EmptyHmgMatrix;
  Result[3, 3] := 1.0;
  Result[0, 0] := s3 * s2 * s1 + c3 * c1;
  Result[1, 0] := c2 * s1;
  Result[2, 0] := s3 * c1 - c3 * s2 * s1;
  Result[0, 1] := s3 * s2 * c1 - c3 * s1;
  Result[1, 1] := c2 * c1;
  Result[2, 1] := -c3 * s2 * c1 - s3 * s1;
  Result[0, 2] := -s3 * c2;
  Result[1, 2] := s2;
  Result[2, 2] := c3 * c2;
end;

function CreateProjectionMatrix(const fov: single; aspect: single; znear:
  single; zfar: single): TMatrix;
var
  f, zdif: Single;
begin
  f := 1.0 / TAN(DegToRad(fov) / 2.0);
  Result := IdentityHmgMatrix;
  Result[0, 0] := f / aspect;
  Result[1, 1] := f;
  zdif := znear - zfar;
  if zdif <> 0 then
  begin
    Result[2, 2] := (zfar + znear) / zdif;
    Result[3, 2] := 2 * zfar * znear / zdif;
  end;
  Result[2, 3] := -1.0;
  Result[3, 3] := 0.0;
end;

function CreateProjectionMatrix(const Rigth, Left, Top, Bottom: Integer):
  TMatrix;
begin
  Result := IdentityHmgMatrix;
  Result[0, 0] := -2 / (Rigth - Left);
  Result[1, 1] := -2 / (Top - Bottom);
  Result[2, 2] := -1.0;
  Result[3, 0] := (Rigth + Left) / (Rigth - Left);
  Result[3, 1] := (Top + Bottom) / (Top - Bottom);
  Result[3, 2] := -EPSILON;
  Result[3, 3] := 1.0;
end;

end.

