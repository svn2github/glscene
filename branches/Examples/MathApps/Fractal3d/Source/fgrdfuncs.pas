unit fgrdfuncs;  {from GLScene TexTerr}
{A very rudimentary unit to read Arcinfo Grid Ascii format
P.G. Scadden, 26/10/00 P.Scadden@gns.cri.nz}
interface
Type

TGrd = class
          pts:array of array of single;
          destructor destroy; override;
        Public
          height,width:integer;
          maxz:single;
          null:single;
          cellsize:single;
          originX,OriginY:double;
          Procedure Loadfromfile(filename:String);
       end;
{
ncols         200
nrows         225
xllcorner     2667000
yllcorner     6033000
cellsize      40
NODATA_value  -9999
}

implementation

uses Sysutils, Classes;

Type   TCharSet = TSysCharSet;


function WordPosition(const N: Integer; const S: string;
  const WordDelims: TCharSet): Integer;
{This function lifted straight out of RXlib}
var
  Count, I: Integer;
begin
  Count := 0;
  I := 1;
  Result := 0;
  while (I <= Length(S)) and (Count <> N) do begin
    { skip over delimiters }
    while (I <= Length(S)) and (S[I] in WordDelims) do Inc(I);
    { if we're not beyond end of S, we're at the start of a word }
    if I <= Length(S) then Inc(Count);
    { if not finished, find the end of the current word }
    if Count <> N then
      while (I <= Length(S)) and not (S[I] in WordDelims) do Inc(I)
    else Result := I;
  end;
end;

function ExtractWord(N: Integer; const S: string;
  const WordDelims: TCharSet): string;
{This function lifted straight out of RXlib}
var
  I: Integer;
  Len: Integer;
begin
  Len := 0;
  I := WordPosition(N, S, WordDelims);
  if I <> 0 then
    { find the end of the current word }
    while (I <= Length(S)) and not(S[I] in WordDelims) do begin
      { add the I'th character to result }
      Inc(Len);
      SetLength(Result, Len);
      Result[Len] := S[I];
      Inc(I);
    end;
  SetLength(Result, Len);
end;


procedure TGrd.Loadfromfile;
var
   f:TStringList;
    linestring,valstr : String;
    row,col:Integer;
    i, n:Integer;

   function ReadLine : String;
   begin
      Result:=f[n];
      Inc(n);
   end;

begin
  f := TStringList.create;
  f.LoadFromFile(filename);
  n:=0;
  try
     linestring := ReadLine;
     width := strtoInt(copy(linestring,6,50));
     linestring := ReadLine;
     height := strtoInt(copy(linestring,6,50));
     linestring := ReadLine;
     OriginX := strtofloat(copy(linestring,11,50));
     linestring := ReadLine;
{
ncols         200
nrows         225
xllcorner     2667000
yllcorner     6033000
cellsize      40
NODATA_value  -9999
}
     OriginY := strtofloat(copy(linestring,11,50));
     linestring := ReadLine;
     cellsize := strtofloat(copy(linestring,11,50));
     linestring := ReadLine;
     null := strtofloat(copy(linestring,15,50));
     setlength(pts,height,width);
     row := height - 1;
     col := 0;
     maxz := -3 * 10E38;
     while (n<f.Count) and (row >=0) do
     begin
       linestring := readline;
       i:= 1;
       valstr := extractword(i,linestring,[' ']);
       while valstr<>'' do
       begin
          pts [row,col] :=strtofloat(valstr);
          if pts[row,col]>maxz then maxz := pts[row,col];
          inc(col);
          If (col=width) then
          begin
            dec(row);
            col := 0;
          end;
          inc(i);
          valstr := extractword(i,linestring,[' ']);
       end;
     end;
  finally
    f.free;
  end;
end;

destructor Tgrd.destroy;
begin
  pts := nil;
  inherited destroy;
end;

end.
