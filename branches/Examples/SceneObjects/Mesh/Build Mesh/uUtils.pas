unit uUtils;

interface

uses
  Winapi.Windows,
  System.SysUtils,
  Vcl.Dialogs,
  OpenGL1x,
  GLGraphics,
  GLColor,
  GLMaterial,
  GLVectorFileObjects,
  GLVectorTypes,
  GLVectorGeometry,
  GLWin32Viewer,
  uMesh;

type
  TCoordinates = record
    X,Y,Z: Single;
  end;

function  WorldToTex (const V: TVector; const Emin, Emax: TVector3f; w,h,d: integer): TVector;
function  TexToWorld (const V: TVector; const Emin, Emax: TVector3f; w,h,d: integer): TVector;

function  TestChanse(Sides: Integer): Boolean;

function  PointInRect(pnt: TVector; x1,y1,x2,y2: Integer): Boolean; overload;
function  PointInRect(pnt: TVector; r: TRect): Boolean; overload;

procedure Pixel322RGB(pix: TGlPixel32; var _r,_g,_b,_a: byte);
function  RGB2Pixel32(_r,_g,_b,_a: byte): TGlPixel32;
function  Sign(a: Real): Real;

function  UpString(Source: AnsiString): AnsiString;
function  GetStrINstr(Source: AnsiString; St: Char; En: Char): AnsiString;
function  StrReplace(S,ch1,ch2: AnsiString): AnsiString;
function  Ang(Angle: Single): Single;

procedure ActorAnimation(Actor: TGLActor; Animation: AnsiString);

// Matrix
function  CreateWorldMatrix(Scale, Pos, Axis: TVector; Angle: single): TMatrix;

// Mesh
procedure CopyMeshObj(SrcFF, DstFF: TMesh; m : TMatrix);

implementation

//

function WorldToTex (const V: TVector; const Emin, Emax: TVector3f; w,h,d: integer): TVector;
begin
  result.X := (v.X-emin.X)/(emax.X-emin.X)*w;
  result.Y := (v.Y-emin.Y)/(emax.Y-emin.Y)*h;
  result.Z := (v.Z-emin.Z)/(emax.Z-emin.Z)*d;
  result.W := 0;
end;

function TexToWorld (const V: TVector; const Emin, Emax: TVector3f; w,h,d: integer): TVector;
begin
  result.X := (v.X/w)*(emax.X-emin.X)+emin.X;
  result.Y := (v.Y/h)*(emax.Y-emin.Y)+emin.Y;
  result.Z := (v.Z/d)*(emax.Z-emin.Z)+emin.Z;
  result.W := 0;
end;

//

function TestChanse(Sides: Integer): Boolean;
var
  Side: Integer;
begin
  RandomIze;
  Side:= Random(Sides);

  Result:= False;
  if Side = 0 then Result:= True;
end;

function FullSearchStr(Source: AnsiString; St: Char; En: Char; var Temp: Integer): AnsiString;
Var
  i1,i2:Integer;
begin
  i1:= Pos(St, Source);
  i2:= Pos(En, Source);

  if i2 <> 0 then Temp:= i2 + 1
  else Temp:= Length(Source);

  if (i1 <> 0) and (i2 <> 0) then FullSearchStr:= Copy(Source,i1+1,i2-i1-1)
  else FullSearchStr:= '';
end;

function GetStrINstr(Source: AnsiString; St: Char; En: Char): AnsiString;
Var
  A: Integer;
begin
  GetStrinstr:= FullSearchStr(Source,St,En,A);
end;

function StrReplace(S,ch1,ch2: AnsiString): AnsiString;
var
  S2: AnsiString;
  i: Integer;
begin
  S2:= '';

  for i:= 1 to Length(S) do
    if Copy(S,i,1) <> ch1 then
      S2:= S2 + Copy(S,i,1)
    else
      S2:= S2 + ch2;

  Result:= S2;
end;

function UpString(Source: AnsiString): AnsiString;
Var
  Dest: AnsiString;
  i: Integer;
begin
 Dest:= '';

 for i:= 1 to length(Source) do
  Dest:= Dest + UpCase(Source[i]);

 UpString:= Dest;
end;

function Sign(a: Real): Real;
begin
  if a > 0 then result:= 1
  else if a < 0 then result:= -1
  else result:= 0;
end;

procedure Pixel322RGB(pix: TGlPixel32; var _r,_g,_b,_a: byte);
begin
  _r:= pix.r;
  _g:= pix.g;
  _b:= pix.b;
  _a:= pix.a;
end;

function RGB2Pixel32(_r,_g,_b,_a: byte): TGlPixel32;
begin
  result.r:= _r;
  result.g:= _g;
  result.b:= _b;
  result.a:= _a;
end;

function PointInRect(pnt: TVector; x1,y1,x2,y2: Integer): Boolean; overload;
begin
  result:= (pnt.X >= x1) and (pnt.Y >= y1) and (pnt.X <= x2) and (pnt.Y <= y2);
end;

function PointInRect (pnt: TVector; r: TRect): Boolean; overload;
begin
  result:= PointInRect(pnt, r.left, r.top, r.right, r.bottom);
end;

function Ang(Angle: Single): Single;
begin
  Result:= ((((Round(Angle) mod 360) + 540) mod 360) - 180);
end;

procedure ActorAnimation(Actor: TGLActor; Animation: AnsiString);
begin
  if Animation <> Actor.CurrentAnimation then
     Actor.SwitchToAnimation(Animation);
end;

function CreateWorldMatrix(Scale, Pos, Axis: TVector; Angle: single): TMatrix;
var wm, mt, ms, mr: TMatrix;
begin
  ms := CreateScaleMatrix(Scale);
  mt := CreateTranslationMatrix(Pos);
  mr := CreateRotationMatrix(Axis, Angle);
  wm := IdentityHmgMatrix;
  wm := MatrixMultiply(wm, mr);
  wm := MatrixMultiply(wm, ms);
  wm := MatrixMultiply(wm, mt);
  
  TransposeMatrix(wm);
  Result:= wm;
end;

function Kilo(Value: Single): Single;
begin
  Result:= Value/100;
end;

procedure CopyMeshObj(SrcFF, DstFF : TMesh; m : TMatrix);
var
  i,j          : integer;
  MObj,SrcMObj : TMeshObject;
  FG,srcFG     : TFGVertexNormalTexIndexList;
begin
  for i := 0 to SrcFF.MeshObjects.Count-1 do
  begin
    SrcMObj   := SrcFF.MeshObjects[i];
    MObj      := TMeshObject.CreateOwned(dstFF.MeshObjects);
    MObj.Mode := SrcMObj.Mode;

    for j := 0 to SrcMObj.Vertices.Count-1 do
      MObj.Vertices.Add(VectorTransform(SrcMObj.Vertices[j],m));

    for j := 0 to SrcMObj.Normals.Count-1 do
      MObj.Normals.Add(srcMObj.Normals[j]);

    MObj.TexCoords.Add(SrcMObj.TexCoords);

    for j := 0 to SrcFF.MeshObjects[i].FaceGroups.Count-1 do
    begin
      SrcFG           := TFGVertexNormalTexIndexList(SrcFF.MeshObjects[i].FaceGroups[j]);
      FG              := TFGVertexNormalTexIndexList.CreateOwned(MObj.FaceGroups);
      FG.Mode         := SrcFG.Mode;
      FG.MaterialName := SrcFF.MaterialData.Name;

      FG.VertexIndices  .Add (SrcFG.VertexIndices);
      FG.NormalIndices  .Add (SrcFG.NormalIndices);
      FG.TexCoordIndices.Add (SrcFG.TexCoordIndices);
    end;
  end;
end;

end.
