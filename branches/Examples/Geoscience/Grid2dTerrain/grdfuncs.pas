unit GrdFuncs;

{ A class to read Arcinfo Grid Ascii format
  P.G.Scadden, 26/10/00 P.Scadden@gns.cri.nz
  and to read Surfer Grid Ascii format
  P.V.Vassiliev, 25/12/05 vassiliev@sf.users.net
}

interface

uses
  System.SysUtils,
  System.Classes,
   
  GLVectorGeometry;

type

  TGridType = (gtSurfer7, gtSurferGSBin, gtSurferGSASCII, gtArcInfoASCII, gtGMS,
    gtUnknown);

  TGrid2D = class(TObject)
  private
    FNodes: array of TSingleArray; // stores nodes: z=f(x,y)
    FBlankVal: Double; // Blankvalue used in surfer (z values > are "blank")
    FBlankSub: Double; // used to substitute for viewing
    FnBlanks: Integer; // number of blank nodes
    FGridType: TGridType;
    FNx: Integer; // number of X nodes
    FNy: Integer; // number of Y nodes
    FDx: Single; // spacing between adjacent X nodes
    FDy: Single; // spacing between adjacent Y nodes
    StrVal: String;
    StrLine: String;
    Sl, Tl: TStringList;
    // a case when value is out of range should never happen
    function GetNode(I, J: Integer): Single;
  public
    MaxZ: Single;
    NoData: Single;
    CellSize: Single; // for square cells only
    Xo, Xe: Single; // low and high X
    Yo, Ye: Single; // low and high Y
    Zo, Ze: Single; // low and high Z
    destructor Destroy; override;
    function WordPosition(const N: Integer; const S: string;
      const WordDelims: TSysCharSet): Integer;
    function ExtractWord(N: Integer; const S: string;
      const WordDelims: TSysCharSet): string;
    procedure LoadFromArcinfo(FileName: String);
    procedure LoadFromSurfer(FileName: String);

    property Nodes[I, J: Integer]: Single read GetNode;
    property BlankVal: Double read FBlankVal write FBlankVal;
    property BlankSub: Double read FBlankSub write FBlankSub;
    property Nblanks: Integer read FnBlanks write FnBlanks;
    property GridType: TGridType read FGridType write FGridType;
    property Nx: Integer read FNx write FNx;
    property Ny: Integer read FNy write FNy;
    property Dx: Single read FDx write FDx;
    property Dy: Single read FDy write FDy;

  end;

const
  dSURFBLANKVAL = 1.70141E38; // used in Surfer ASCII and Binary for blanking

  // -------------------------------------------------------------------------
  // -------------------------------------------------------------------------
  // -------------------------------------------------------------------------
implementation

// -------------------------------------------------------------------------
// -------------------------------------------------------------------------
// -------------------------------------------------------------------------

// WordPosition -----------------------------------------
// this function was obtained from RxLib
//
function TGrid2D.WordPosition(const N: Integer; const S: string;
  const WordDelims: TSysCharSet): Integer;
var
  Count, I: Integer;
begin
  Count := 0;
  I := 1;
  Result := 0;
  while ((I <= Length(S)) and (Count <> N)) do
  begin
    // skip over delimiters
    while (I <= Length(S)) and (S[I] in WordDelims) do
      Inc(I);
    // if we're not beyond end of S, we're at the start of a word
    if I <= Length(S) then
      Inc(Count);
    // if not finished, find the end of the current word
    if Count <> N then
      while (I <= Length(S)) and not(S[I] in WordDelims) do
        Inc(I)
    else
      Result := I;
  end;
end;

// ExtractWord ------------------------------------------
// this function was obtained from RxLib
//
function TGrid2D.ExtractWord(N: Integer; const S: string;
  const WordDelims: TSysCharSet): string;

var
  I, Len: Integer;

begin
  Len := 0;
  I := WordPosition(N, S, WordDelims);
  if (I <> 0) then
    // find the end of the current word
    while (I <= Length(S)) and not(S[I] in WordDelims) do
    begin
      // add the I'th character to result
      Inc(Len);
      SetLength(Result, Len);
      Result[Len] := S[I];
      Inc(I);
    end;
  SetLength(Result, Len);
end;

// GetNode
//
function TGrid2D.GetNode(I, J: Integer): Single;
begin
  try
    Result := FNodes[I, J];
  except
    Result := 0; // to avoid compilation warning
  end;
end;

// LoadFromArcinfo
//
procedure TGrid2D.LoadFromArcinfo;
var
  I: Integer;
  J: Integer;
  N: Integer;

begin
  Sl := TStringList.Create;
  Tl := TStringList.Create;
  Sl.LoadFromFile(FileName);
  try
    Tl.DelimitedText := Sl[0];
    Ny := StrToInt(Tl[1]); // ncols
    Tl.DelimitedText := Sl[1];
    Nx := StrToInt(Tl[1]); // nrows
    Tl.DelimitedText := Sl[2];
    Xo := StrToFloat(Tl[1]); // xllcorner
    Tl.DelimitedText := Sl[3];
    Yo := StrToFloat(Tl[1]); // yllcorner
    Tl.DelimitedText := Sl[4];
    CellSize := StrToFloat(Tl[1]); // cellsize
    Tl.DelimitedText := Sl[5];
    NoData := StrToFloat(Tl[1]); // NoData value

    MaxZ := -3 * 10E38;
    SetLength(FNodes, Nx, Ny);

    for I := 0 to Nx - 1 do
    begin
      Tl.DelimitedText := Sl[I + 6];
      for J := 0 to Ny - 1 do
      begin
        StrVal := Tl[J];
        FNodes[I, J] := StrToFloat(StrVal);
        if FNodes[I, J] > MaxZ then
          MaxZ := FNodes[I, J];
      end;
    end;
  finally
    Sl.Free;
    Tl.Free;
  end;
end;

// LoadFromSurfer
//
procedure TGrid2D.LoadFromSurfer(FileName: String);
var
  I, // I = row counter
  J, // J = column counter
  K, // K = counter used with extractWord
  N: Integer; // N = counter to increment through file

  { sub } function ReadLine: string;
  begin
    Result := Sl[N];
    Inc(N);
  end;

begin
  Sl := TStringList.Create;
  Tl := TStringList.Create;
  N := 0;
  try
    Sl.LoadFromFile(FileName); // need to LoadFromStream(aStream);
    Tl.DelimitedText := Copy(ReadLine, 1, 4);
    if (Tl[0] <> 'DSAA') then
    begin
      raise Exception.Create('Not a valid grd file !');
      Exit;
    end;

    MaxZ := -3 * 10E38;

    Tl.DelimitedText := ReadLine;
    Nx := StrToInt(Tl[0]);
    Ny := StrToInt(Tl[1]);

    Tl.DelimitedText := ReadLine;
    Xo := StrToFloat(Tl[0]);
    Xe := StrToFloat(Tl[1]);

    Tl.DelimitedText := ReadLine;
    Yo := StrToFloat(Tl[0]);
    Ye := StrToFloat(Tl[1]);

    Tl.DelimitedText := ReadLine;
    Zo := StrToFloat(Tl[0]);
    Ze := StrToFloat(Tl[1]);

    Dx := (Xe - Xo) / Nx;
    Dy := (Ye - Yo) / Ny;

    SetLength(FNodes, Ny, Nx);
    Nblanks := 0;
    BlankVal := dSURFBLANKVAL;
    NoData := BlankVal; // NoData value

    // loop over the Ny-1 Rows
    for I := 0 to Ny - 1 do
    begin
      J := 0;
      // reading lines until Nx-1 Cols entries have been obtained
      while J <= Nx - 1 do
      begin
        StrLine := ReadLine;
        K := 1;
        StrVal := ExtractWord(K, StrLine, [' ']);
        while (StrVal <> '') do
        begin
          if (J <= Nx - 1) then
            FNodes[I, J] := StrToFloat(StrVal);
          if FNodes[I, J] > MaxZ then
            MaxZ := FNodes[I, J];
          if (FNodes[I, J] >= BlankVal) then
            Nblanks := Nblanks + 1;

          Inc(J);
          Inc(K);
          StrVal := ExtractWord(K, StrLine, [' ']);
        end;
        if (J > Nx - 1) then
          Break;
      end;
    end;
  finally
    Tl.Free;
    Sl.Free;
  end;
end;

destructor TGrid2D.Destroy;
begin
  FNodes := nil;
  inherited Destroy;
end;

end.
