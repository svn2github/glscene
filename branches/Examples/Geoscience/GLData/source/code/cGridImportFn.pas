{ -------------------------------------------------------------------------------
  Unit Name: gridimportfn
  Author:    Aaron Hochwimmer (hochwimmera@pbworld.com)
  Latest Version Available: http://software.pbpower.net/download.html

  Purpose:   parsing data grids into an array of z(x,y) points.

  Information on surfer can be found at
  http://www.goldensoftware.com/products/surfer/surfer.shtml

  Updates:
  23-Jul-2002:  Version 1.01
  Replaces "blanked" values with BlankSub (default = 0)
  15-Jun-2003:  Version 1.1
  Added ArcInfo Support
  (thx Phil Scadden - p.scadden@gns.cri.nz)
  18-Jun-2003:  Version 1.2
  Fixed bug - surfer specs have xlo,xhi etc as doubles
  Hiding array variable away as private variables
  Remove commasplit dependency
  21-Jun-2003:  Version 1.3
  Remove dialogs dependency, added LoadGrid method, bug fix to
  GSASCII detection
  29-Jun-2003:  Version 1.31
  Identify grid type (via GridType property)
  14-Jul-2003:  Version 1.32
  Added GlobalStrToFloat (thx Philipp Pammler -pilli_willi@gmx.de)
  23-Jul-2003:  Version 1.33
  Updated for GlobalStrToFloat (GSASCII)
  24-Jul-2004:  Version 1.34
  Removed XScale,YScale,ConstructArrays - obsolete
  ------------------------------------------------------------------------------- }
unit cGridimportfn;

interface

uses
  System.Classes, System.SysUtils;

type
  TDouble1d = array of double;
  TSingle2d = array of array of Single;

  TGridType = (gtSurfer7, gtSurferGSBin, gtSurferGSASCII, gtArcInfoASCII, gtGMS,
    gtUnknown);

  TContourGridData = class(TObject)
  private
    FNodes: TSingle2d; // stores nodes: z=f(x,y)
    FBlankVal: double; // blankvalue used in surfer (z values > are "blank")
    FBlankSub: double; // used to substitute for viewing
    FnBlanks: Integer; // number of blank nodes
    FGlobalFloat: Boolean;
    FGridType: TGridType;
    FNx: SmallInt; // number of columns in the grid
    FNy: SmallInt; // number of rows in the grid
    FDx: Single; // spacing between adjacent x nodes
    FDy: Single; // spacing between adjacent y nodes
    FXlo: Single; // min x value of the grid
    FXhi: Single; // max x value of the grid
    FXRange: Single; // (xhi-xlo) - the x range
    FYlo: Single; // min y value of the grid
    FYhi: Single; // max y value of the grid
    FYRange: Single; // (yhi-ylo) - the y range
    FZlo: Single; // min z value of the grid
    FZhi: Single; // max z value of the grid
    FZRange: Single; // (zhi-zlo) - the z range
    StrLine, StrVal: string; // string current line and value
    procedure CalcDerived;
    procedure CalcDerivedNative;
    function ExtractWord(N: Integer; const S: string;
      const WordDelims: TSysCharSet): string;
    function WordPosition(const N: Integer; const S: string;
      const WordDelims: TSysCharSet): Integer;
    { ** attempts to open a surfer grid (.grd) in GS ASCII format.
      Integer return code:  0 = file processed ok
      1 = not a GS ASCII file }
    function LoadSurferGSASCII(FileName: TFileName): Integer;
    { ** attempts to open a surfer grid (.grd) in GS Binary format.
      Integer return code:  0 = file processed ok
      1 = not a GS Binary file }
    function LoadSurferGSBinary(FileName: TFileName): Integer;
    { ** attempts to open a surfer grid (.grd) in native binary format. This format
      is used by default by both Surfer 7 and Surfer 8.
      Integer return code:  0 = file processed ok
      1 = not a Surfer Grid file }
    function LoadSurferGridNative(FileName: TFileName): Integer;
  protected
    function GetNode(I, J: Integer): Single;
    function GlobalStrToFloat(const S: string): double;
  public
    constructor Create;
    destructor Destroy; override;
    { ** attempts to load an ArcInfo ASCII File Format.
      Integer return code: 0 = file processed ok
      1 = not a ArcInfo ASCII File Format }
    function LoadARCINFOASCII(FileName: TFileName): Integer;
    { ** attempts to open a surfer grid (.grd) in either GS ASCII, GS Binary, or
      Surfer 7 GRD format.
      Integer return code: 0 = file processed ok
      1 = not a surfer grid of any format }
    function LoadSurferGrid(FileName: TFileName): Integer;
    { ** attempts to open any grid (.grd) either a surfer grid or an ArcInfo one
      this is a easiest method for using the class }
    function LoadGrid(FileName: TFileName): Integer;

    property Nodes[I, J: Integer]: Single read GetNode;
    property BlankVal: double read FBlankVal write FBlankVal;
    property BlankSub: double read FBlankSub write FBlankSub;
    property GlobalFloat: Boolean read FGlobalFloat write FGlobalFloat;
    property GridType: TGridType read FGridType write FGridType;
    property Nblanks: Integer read FnBlanks write FnBlanks;
    property Nx: SmallInt read FNx write FNx;
    property Ny: SmallInt read FNy write FNy;
    property Xlo: Single read FXlo write FXlo;
    property Xhi: Single read FXhi write FXhi;
    property Xrange: Single read FXRange write FXRange;
    property Ylo: Single read FYlo write FYlo;
    property Yhi: Single read FYhi write FYhi;
    property Yrange: Single read FYRange write FYRange;
    property Dx: Single read FDx write FDx;
    property Dy: Single read FDy write FDy;
    property Zlo: Single read FZlo write FZlo;
    property Zhi: Single read FZhi write FZhi;
    property Zrange: Single read FZRange write FZRange;
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

// ------ TContourGridData.CalcDerived -----------------------------------------
// calculates derived quantities such as step sizes and ranges.
// Also sets up the points array
//
procedure TContourGridData.CalcDerived;

begin
  Xrange := Xhi - Xlo;
  Dx := Xrange / (Nx - 1);
  Yrange := Yhi - Ylo;
  Dy := Yrange / (Ny - 1);
  Zrange := Zhi - Zlo;
  SetLength(FNodes, Ny, Nx);
end;

// ------ TContourGridData.CalcDerivedNative -----------------------------------
{ calculates derived quantities such as ranges. Different to CalcDerived
  as the native Surfer Grid/Surfer 7 format has different parameters }
procedure TContourGridData.CalcDerivedNative;

begin
  Xrange := (Nx - 1) * Dx;
  Xhi := Xlo + Xrange;
  Yrange := (Ny - 1) * Dy;
  Yhi := Ylo + Yrange;
  Zrange := Zhi - Zlo;
  SetLength(FNodes, Ny, Nx);
end;

// ----- TContourGridData.ExtractWord ------------------------------------------
{ this function was obtained from RxLib }
function TContourGridData.ExtractWord(N: Integer; const S: string;
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

// ----- TContourGridData.GlobalStrToFloat -------------------------------------
{ code to attempt to convert a string to a float for either '.' or ','
  separators. This is used to attempt to read in grids that have been generated
  in different parts of the world - code from Philipp Pammler }
function TContourGridData.GlobalStrToFloat(const S: string): double;

var
  I: Integer;
  Separators: set of Char;
  Str: string;

begin
  Str := S;
  Separators := ['.', ','];
  for I := 0 to Length(Str) do
  begin
    if Str[I] in Separators then
      Str[I] := FormatSettings.DecimalSeparator;
    // local separator
  end;
  Result := StrToFloat(Str);
end;

// ----- TContourGridData.WordPosition -----------------------------------------
// this function was obtained from RxLib

function TContourGridData.WordPosition(const N: Integer; const S: string;
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

// ----- TContourGridData.LoadARCINFOASCII -------------------------------------
{ ARC/INFO ASCIIGRID file format
  ARC ASCIIGRID refers to a specific interchange format developed for ARC/INFO
  rasters. The format consists of a header that specifies the geographic domain
  and resolution, followed by the actual grid cell values.

  General Format:
  ---------------
  Lines 1-6 Geographic Header: Coordinates may be in decimal or integer format.
  DD:MM:SS format for geodatic coordinates is not supported.

  Line 1: ncols xxxxx
  ncols refers to the number of columns in the grid and xxxxx is the numerical
  value

  Line 2: nrows xxxxx
  nrows refers to the number of rows in the grid and xxxxx is the numerical
  value

  Line 3: xllcorner xxxxx
  xllcorner refers to the western edge of the grid and xxxxx is the numerical
  value

  Line 4: yllcorner xxxxx
  yllcorder refers to the southern edge of the grid and xxxxx is the numerical
  value

  Line 5: cellsize xxxxx
  cellsize refers to the resolution of the grid and xxxxx is the numerical value

  Line 6: nodata_value
  nodata_value refers to the value that represents missing data and xxxxx is the
  numerical value

  Record 7 --> End Of File:
  -------------------------
  xxx xxx xxx
  val(nox,noy)(f) = individual grid values, column varying fastest in integer
  format. Grid values are stored as integers but can be read as floating point
  values.

  xllcorner and yllcorner are given as the EDGES of the grid, NOT the centers of
  the edge cells.

  ARC/INFO supports other header strings that allow the centers of the edge cells
  to be given, but they are not supported here. The origin of the grid is the
  upper left and the terminus is the lower right.

  ARC format grids are single-band files.
}
function TContourGridData.LoadARCINFOASCII(FileName: TFileName): Integer;

var
  sl, tl: TStringList;
  iRow, iCol: Integer;
  I, J, N: Integer;

begin
  tl := TStringList.Create;
  sl := TStringList.Create;
  try
    sl.LoadFromFile(FileName);
    tl.DelimitedText := sl[0];
    if (tl[0] <> 'ncols') then // not ARC/INFO file
    begin
      sl.Free;
      tl.Free;
      Result := 1;
      Exit;
    end;
    Nx := StrToInt(tl[1]); // ncols
    tl.DelimitedText := sl[1];
    Ny := StrToInt(tl[1]); // nrows
    tl.DelimitedText := sl[2];
    Xlo := StrToInt(tl[1]); // xllcorner
    tl.DelimitedText := sl[3];
    Ylo := StrToInt(tl[1]); // yllcorner
    tl.DelimitedText := sl[4];
    Dx := StrToInt(tl[1]); // cellsize
    Dy := Dx;
    tl.DelimitedText := sl[5];
    BlankVal := StrToFloat(tl[1]); // blank value

    // temp as we don't know zlo and zhi yet...}
    Zlo := 0.0;
    Zhi := 0.0;

    { ** calculate the derived quantites - ranges and stuff etc }
    CalcDerivedNative;

    Zhi := -3 * 10E38; // set artificial bounds
    Zlo := 3 * 10E38;
    Nblanks := 0;

    {
      for iRow := Ny - 1 downto 0 do
      begin
      tl.DelimitedText := sl[iRow + 6];   // Read Rows
      for iCol := 0 to Nx - 1 do
      begin
      fPoints[iRow,iCol] := StrToFloat(tl[iCol]);    // Read Col
      if (fPoints[iRow,iCol] = BlankVal) then
      nblanks := nblanks + 1
      else
      begin
      if (fPoints[iRow,iCol] > zhi) then
      zhi := fPoints[iRow,iCol];
      if (fPoints[iRow,iCol] < zlo) then
      zlo := fPoints[iRow,iCol];
      end;
      end;
      end;
      { }

    iRow := Ny - 1;
    iCol := 0;
    N := 5;
    while (N < sl.Count) and (iRow >= 0) do
    begin
      Inc(N);
      tl.DelimitedText := sl[N]; // Read Line
      I := 1;
      StrVal := tl[I];
      while (StrVal <> '') do
      begin
        Inc(I);
        FNodes[iRow, iCol] := StrToFloat(StrVal);
        if (FNodes[iRow, iCol] = BlankVal) then
          Nblanks := Nblanks + 1
        else
        begin
          if (FNodes[iRow, iCol] > Zhi) then
            Zhi := FNodes[iRow, iCol];
          if (FNodes[iRow, iCol] < Zlo) then
            Zlo := FNodes[iRow, iCol];
        end;
        Inc(iCol);
        if (iCol = Nx) then
        begin
          Dec(iRow);
          iCol := 0;
        end;
        StrVal := ExtractWord(I, sl[N], [' ']);
      end;
    end;
  finally
    sl.Free;
    tl.Free;
    Zrange := Zhi - Zlo;
    Result := 0;
    GridType := gtArcInfoASCII;
  end;
end;

// ------ TContourGridData.LoadSurferGSASCII -----------------------------------
{ GS ASCII Grid File Format:
  GS ASCII GRid files [.grd] contain 5 header lines that provide information about
  the size limits of the grid, followed by a list of z values. The fields within
  a GS ASCII grid _must_ be space delimited.
  The listing of z values follows the header information in the file. The z values
  are stored in row-major order starting with the minimum Y coordinate. The first
  z value of the grid corresponds to the lower left corner of the map (min X,Y).
  When the maximum X value is reached in the row the list of z values continues
  with the next highest row, until all the rows have been included.

  General Format:

  id      = a four char id string "DSAA" which idicates GS ASCII grid file
  nx ny   = nx is the number of grid lines along the x axis (columns)
  ny is the number of grid lines along the y axis (rows)
  xlo xhi = xlo is the floating point minimum X value of the grid
  xhi is the floating point maximum X value of the grid
  ylo yhi = ylo is the floating point minimum Y value of the grid
  yhi is the floating point maximum Y value of the grid
  zlo zhi = zlo is the floating point minimum Z value of the grid
  = xhi is the floating point maximum Z value of the grid

  grid row 1
  grid row 2
  etc

  NB: each grid row has a constant Y coordinate. Grid row 1 corresponds to ylo
  and the last grid row corresponds to yhi. Within each row the Z values are
  arranged from xlo to xhi. When an ASCII grid file is created in surfer the
  program generates 10 z values per line for readability. This function will read
  files with any number of values per line.
}
function TContourGridData.LoadSurferGSASCII(FileName: TFileName): Integer;

var
  FileStrings: TStringList;
  tl: TStringList;
  I, // I = row counter
  J, // J = column counter
  K, // K = counter used with extractWord
  N: Integer; // N = counter to increment through file
  bNOTGSASCII: Boolean;
  LineNum: Integer;

  { sub } function ReadLine: string;
  begin
    Result := FileStrings[N];
    Inc(N);
  end;

begin
  FileStrings := TStringList.Create;
  tl := TStringList.Create;
  FileStrings.LoadFromFile(FileName);
  N := 0;
  try
    // check for valid GS ASCII header
    if (Copy(ReadLine, 1, 4) <> 'DSAA') then
      bNOTGSASCII := true
    else
    begin
      bNOTGSASCII := false;
      // read nx,ny
      StrLine := ReadLine;
      Nx := StrToInt(ExtractWord(1, StrLine, [' ']));
      Ny := StrToInt(ExtractWord(2, StrLine, [' ']));
      // read xlo,xhi
      StrLine := ReadLine;
      if GlobalFloat then
      begin
        Xlo := GlobalStrToFloat(ExtractWord(1, StrLine, [' ']));
        Xhi := GlobalStrToFloat(ExtractWord(2, StrLine, [' ']));
      end
      else
      begin
        Xlo := StrToFloat(ExtractWord(1, StrLine, [' ']));
        Xhi := StrToFloat(ExtractWord(2, StrLine, [' ']));
      end;
      // read ylo,yhi
      StrLine := ReadLine;
      if GlobalFloat then
      begin
        Ylo := GlobalStrToFloat(ExtractWord(1, StrLine, [' ']));
        Yhi := GlobalStrToFloat(ExtractWord(2, StrLine, [' ']));
      end
      else
      begin
        Ylo := StrToFloat(ExtractWord(1, StrLine, [' ']));
        Yhi := StrToFloat(ExtractWord(2, StrLine, [' ']));
      end;
      // read zlo,zhi
      StrLine := ReadLine;
      if GlobalFloat then
      begin
        Zlo := GlobalStrToFloat(ExtractWord(1, StrLine, [' ']));
        Zhi := GlobalStrToFloat(ExtractWord(2, StrLine, [' ']));
      end
      else
      begin
        Zlo := StrToFloat(ExtractWord(1, StrLine, [' ']));
        Zhi := StrToFloat(ExtractWord(2, StrLine, [' ']));
      end;

      CalcDerived; // calculates the derived quantites - step sizes etc

      Nblanks := 0;
      BlankVal := dSURFBLANKVAL;

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
              if GlobalFloat then
                FNodes[I, J] := GlobalStrToFloat(StrVal)
              else
                FNodes[I, J] := StrToFloat(StrVal);
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
    end;

  finally
    if bNOTGSASCII then
      Result := 1
    else
    begin
      Result := 0;
      GridType := gtSurferGSASCII;
    end;
    FileStrings.Free;
    tl.Free;
  end;
end;

// ------ TContourGridData.LoadSurferGSBinary ----------------------------------
{ GS Binary Grid File Format:
  GS Binary grid files [.grd] use a similar layout to the GS ASCII described
  above. The difference is in the ID string and that the files are binary :)

  Data types used:
  AnsiChar        -   single byte
  smallint    -   16 byte signed integer
  single      -   32 bit single precision floating point value
  double      -   64 bit double precision floating point value

  General Format:

  Element     Type      Description
  id          char      4 byte id string "DSBB" indicates GS Binary grid file
  nx          smallint  number of grid lines along the x axis (columns)
  ny          smallint  number of grid lines along the y axis (columns)
  xlo         double    minimum X value of the grid
  xhi         double    maximum X value of the grid
  ylo         double    minimum Y value of the grid
  yhi         double    maximum Y value of the grid
  zlo         double    minimum Z value of the grid
  zhi         double    maximum Z value of the grid
  z11,z12,... single    first row of the grid. Each row has constant Y coordinate
  first row corresponds to ylo, and last corresponds to yhi
  Within each row, the Z values are ordered from xlo -> xhi.
  z21,z22,... single    second row of the grid
  z31,z32,... single    third row of the grid
  ...         single    all other rows of the grid up to yhi
}
function TContourGridData.LoadSurferGSBinary(FileName: TFileName): Integer;

var
  BinFile: file;
  I, J: Integer;
  D: double;
  ZVal: Single;
  NDouble, NSingle, NSmallInt, col: Integer; // sizeof vars
  sType: array [1 .. 4] of AnsiChar; // Used instead of old Char type
  NRead: Integer; // number of bytes read - unused

begin
  AssignFile(BinFile, FileName);
  Reset(BinFile, 1); // record size of 1 byte
  // check to see if this is a GS Binary file
  BlockRead(BinFile, sType, 4, NRead);
  if (sType <> 'DSBB') then
  begin
    Result := 1;
    CloseFile(BinFile);
  end
  else
  begin
    NDouble := sizeof(D);
    NSingle := sizeof(ZVal);
    NSmallInt := sizeof(I);
    // read nx,ny
    BlockRead(BinFile, I, NSmallInt, NRead);
    Nx := I;
    BlockRead(BinFile, I, NSmallInt, NRead);
    Ny := I;
    // read xlo,xhi
    BlockRead(BinFile, D, NDouble, NRead);
    Xlo := D;
    BlockRead(BinFile, D, NDouble, NRead);
    Xhi := D;
    // read ylo,yhi
    BlockRead(BinFile, D, NDouble, NRead);
    Ylo := D;
    BlockRead(BinFile, D, NDouble, NRead);
    Yhi := D;
    // read zlo,zhi
    BlockRead(BinFile, D, NDouble, NRead);
    Zlo := D;
    BlockRead(BinFile, D, NDouble, NRead);
    Zhi := D;
    // calculate the derived quantities - step sizes etc
    CalcDerived;
    Nblanks := 0;
    BlankVal := dSURFBLANKVAL;
    // now read in the points
    for I := 0 to Ny - 1 do
      for J := 0 to Nx - 1 do
      begin
        BlockRead(BinFile, ZVal, NSingle, NRead);
        FNodes[I, J] := ZVal;
        if (ZVal >= BlankVal) then
          Nblanks := Nblanks + 1;
      end;
    Result := 0;
    GridType := gtSurferGSBin;
    CloseFile(BinFile);
  end;
end;

// ------ TContourGridData.LoadSurferGridNative --------------------------------
{ Surfer Grid and Surfer 7 Grid files [.GRD] use the same file format.
  Uses tag-based binary file format (allow for future enhancements)
  Each section is preceded by a tag structure - which indicates the type and size
  of the following data.

  If a program does not understand or want a type of data, it can read the tag
  and skip to the next section. Sections can appear in any order than the first
  (which must be the header section)

  Data types used:
  integer     -   32 bit signed integer
  double      -   64 bit double precision floating point value

  Each section is preceded by a tag structure with the following format:
  Element     Type        Description
  id          integer     The type of data in the following section.
  size        integer     The number of bytes in the section (not including this
  tag). Skipping this many bytes after reading this tag
  aligns the file pointer on the next tag.

  Tag Id values. The 0x prefix indicates a hexadecimal value:
  id          Description
  0x42525344  Header section:
  must be the first section within the file
  0x44495247  Grid section:
  describes a 2d matrix of Z values
  0x41544144  Data section:
  contains a variable amount of data.
  0x49544c46  Fault Info section:
  describes the fault traces used when creating a grid

  ** Header Section **
  The header section must be the first section in the file and has the following
  format.

  Element     Type        Description
  version     integer     Version number of the file format. Currently set to 1.

  ** Grid Section **
  The grid section consists of a header that describes a 2D matrix of values,
  followed by the matrix itself. This section encapsulates all of the data that
  was traditionally referred to as a grid:

  Element     Type        Description
  ny          integer     number of rows in the grid (Y direction)
  nx          integer     number of columns in the grid (X direction)
  xlo         double      X coordinate of the lower left corner of the grid
  ylo         double      Y coordinate of the lower right corner of the grid
  xstep       double      spacing between adjacent nodes in the X direction
  ystep       double      spacing between adjacent nodes in the Y direction
  zlo         double      minimum Z value within the grid
  zhi         double      maximum Z value within the grid
  rotation    double      not currently used
  blankval    double      nodes are blanked if >= this value

  A Data section containing the 2d matrix of values (doubles) must immediately
  follow a grid section. Within a data section the grid is stored in row-major
  order, with the lowest row (min Y) first.

  ** Fault Info Section ** (NOT USED IN THIS CLASS)

  Element     Type        Description
  nTraces     integer     number of fault traces (polylines)
  nVertices   integer     total number of vertices in all the traces

  A Data section containing an array of Trace structures and an array of Vertex
  structures must immediately follow a Fault Info section. The number of trace
  structures in the array is nTraces and the number of vertex structures is
  nVertices.

  Trace Structure
  Element     Type        Description
  iFirst      integer     0-based index into the vertex array for the first vertex
  of this trace
  nPts        integer     number of vertices in this trace

  Vertex Structure
  Element     Type        Description
  x           double      X coordinate of the vertex
  y           double      Y coordinate of the vertex
}

function TContourGridData.LoadSurferGridNative(FileName: TFileName): Integer;
const
  sHEADER = '42525344'; // Id for Header section
  sFAULTINFO = '49544c46'; // Id Fault info section
  sGRIDSECT = '44495247'; // Id indicating a grid section
  sDATASECT = '41544144'; // Id indicating a data section
  iSECTSIZE = 8;

var
  BinFile: file;
  buf: array of byte;
  dval: double;
  col, I, J, iHeaderID, iSize, iVal, iVersion, NDouble, nInteger,
    NRead: Integer;
  sSectionID, sSectionID2: AnsiString;

begin
  AssignFile(BinFile, FileName);
  Reset(BinFile, 1); // set the default record size to 1 byte
  nInteger := sizeof(iHeaderID); // just size of integer variable
  // read in the header
  BlockRead(BinFile, iHeaderID, nInteger, NRead);
  sSectionID := IntToHex(iHeaderID, iSECTSIZE);
  // check the header tag

  if (sSectionID <> sHEADER) then
  begin
    Result := 1;
    CloseFile(BinFile);
  end
  else

  begin
    NDouble := sizeof(dval); // just size of double variable
    BlockRead(BinFile, iSize, nInteger, NRead); // size of header section
    BlockRead(BinFile, iVersion, nInteger, NRead); // file version
    // the sections are in any order...
    while (not eof(BinFile)) do
    begin
      // what section is this?
      BlockRead(BinFile, iHeaderID, nInteger, NRead);
      sSectionID := IntToHex(iHeaderID, iSECTSIZE);
      // FAULT INFO SECTION
      if (sSectionID = sFAULTINFO) then
      begin
        BlockRead(BinFile, iSize, nInteger, NRead);
        // size of fault info section
        SetLength(buf, iSize); // set up the temp buffer
        BlockRead(BinFile, buf, iSize, NRead); // skip the section
        // from the specs a data section will follow - skip this one as well
        BlockRead(BinFile, iHeaderID, nInteger, NRead);
        sSectionID2 := IntToHex(iHeaderID, iSECTSIZE);
        if (sSectionID2 = sDATASECT) then
        begin
          BlockRead(BinFile, iSize, nInteger, NRead);
          SetLength(buf, iSize); // set up the temp buffer
          BlockRead(BinFile, buf, iSize, NRead); // skip the section}
        end;
        // GRID SECTION}
      end
      else if (sSectionID = sGRIDSECT) then
      begin
        BlockRead(BinFile, iSize, nInteger, NRead); // size of grid section
        BlockRead(BinFile, iVal, nInteger, NRead);
        Ny := iVal;
        BlockRead(BinFile, iVal, nInteger, NRead);
        Nx := iVal;
        BlockRead(BinFile, dval, NDouble, NRead);
        Xlo := dval;
        BlockRead(BinFile, dval, NDouble, NRead);
        Ylo := dval;
        BlockRead(BinFile, dval, NDouble, NRead);
        Dx := dval;
        BlockRead(BinFile, dval, NDouble, NRead);
        Dy := dval;
        BlockRead(BinFile, dval, NDouble, NRead);
        Zlo := dval;
        BlockRead(BinFile, dval, NDouble, NRead);
        Zhi := dval;
        BlockRead(BinFile, dval, NDouble, NRead); // rotation - not used here
        // blank value
        BlockRead(BinFile, dval, NDouble, NRead);
        BlankVal := dval;
        Nblanks := 0;
        CalcDerivedNative;
        // from the specs a data section will follow - skip this one as well
        BlockRead(BinFile, iHeaderID, nInteger, NRead);
        sSectionID2 := IntToHex(iHeaderID, iSECTSIZE);
        if (sSectionID2 = sDATASECT) then
        begin
          col := Nx - 1;
          BlockRead(BinFile, iSize, nInteger, NRead);
          // now read in the points
          for I := 0 to Ny - 1 do
          begin
            for J := 0 to col do
            begin
              BlockRead(BinFile, dval, NDouble, NRead);
              FNodes[I, J] := dval;
              if (dval >= BlankVal) then
                Nblanks := Nblanks + 1;
            end;
          end;
        end;
      end; // while not eof
    end;
    Result := 0;
    GridType := gtSurfer7;
    CloseFile(BinFile);
  end;
end;

// ------ TContourGridData.LoadSurferGrid --------------------------------------
function TContourGridData.LoadSurferGrid(FileName: TFileName): Integer;
begin
  // the native GRD format first - Surfer 7 or 8
  if (LoadSurferGridNative(FileName) = 0) then
    Result := 0
    // check if the GRD file is GS Binary
  else if (LoadSurferGSBinary(FileName) = 0) then
    Result := 0
    // check if the GRD file is GS ASCII
  else if (LoadSurferGSASCII(FileName) = 0) then
    Result := 0
  else // not a surfer grid file
  begin
    Result := 1;
    GridType := gtUnknown;
  end;
end;

// ----- TContourGridData.LoadGrid ---------------------------------------------
function TContourGridData.LoadGrid(FileName: TFileName): Integer;

begin
  if (LoadSurferGrid(FileName) = 0) then
    Result := 0
  else if (LoadARCINFOASCII(FileName) = 0) then
    Result := 0
  else
  begin
    Result := 1;
    GridType := gtUnknown;
  end;
end;

// TContourGridData.GetNode --------------------------------------
// a case when i,j are out of range should never happen

function TContourGridData.GetNode(I, J: Integer): Single;

begin
  try
    Result := FNodes[I, J];
  except
    Result := 0; // to avoid compilation warning
  end;
end;

// ------ TContourGridData.Create ----------------------------------------------
constructor TContourGridData.Create;

begin
  inherited Create;
  BlankSub := 0.0; // default for blanked values;
  FGlobalFloat := true;
end;

// ------ TContourGridData.destroy ---------------------------------------------
destructor TContourGridData.Destroy;
begin
  FNodes := nil;
  inherited Destroy;
end;

// =============================================================================
end.
