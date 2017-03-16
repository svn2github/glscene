{-------------------------------------------------------------------------------
 Unit Name: gridimportfn
 Author:    Aaron Hochwimmer (hochwimmera@pbworld.com)
  (derived from "karapiti" demo by Phil Scadden (p.scadden@gns.cri.nz)
 Purpose:   parsing data grids into an array of z(x,y) points.
  Information on surfer can be found at
    http://www.goldensoftware.com/products/surfer/surfer.shtml
-------------------------------------------------------------------------------}
unit Gridimportfn;

interface

uses
  System.Classes, System.Sysutils,
  Vcl.Dialogs;

type
  TSingle1d = array of single;
  TSingle2d = array of array of single;

  TContourGridData = class(TObject)
    points : TSingle2D; // stores z points: z=f(x,y)
    xscale : TSingle1D; // array of x points
    yscale : TSingle1D; // array of y points

  private
    fnx : smallint;   // number of columns in the grid
    fny : smallint;   // number of rows in the grid
    fxlo : single;    // min x value of the grid
    fxhi : single;    // max x value of the grid
    fxrange : single; // (xhi-xlo) - the x range
    fxstep : single;  // spacing between adjacent x nodes
    fylo : single;    // min y value of the grid
    fyhi : single;    // max y value of the grid
    fyrange :single;  // (yhi-ylo) - the y range
    fystep : single;  // spacing between adjacent y nodes
    fzlo : single;    // min z value of the grid
    fzhi : single;    // max z value of the grid
    fzrange : single; // (zhi-zlo) - the z range


    procedure CalcDerived;
    procedure CalcDerivedNative;
    procedure ConstructArrays;

{** attempts to open a surfer grid (.grd) in GS ASCII format.
Integer return code:  0 = file processed ok
                      1 = not a GS ASCII file}
    function LoadSurferGSASCII(sFileName:string):integer;

{** attempts to open a surfer grid (.grd) in GS Binary format.
Integer return code:  0 = file processed ok
                      1 = not a GS Binary file}
    function LoadSurferGSBinary(sFileName:string):integer;

{** attempts to open a surfer grid (.grd) in native binary format. This format
is used by default by both Surfer 7 and Surfer 8.
Integer return code:  0 = file processed ok
                      1 = not a Surfer Grid file}
    function LoadSurferGridNative(sFileName:string):integer;

  public
    destructor Destroy;override;
{** attempts to open a surfer grid (.grd) in either GS ASCII, GS Binary, or
Surfer 7 GRD format.
Integer return code: 0 = file processed ok
                     1 = not a surfer grid of any format}
    function LoadSurferGrid(sFileName:string):integer;

    property nx : smallint read fnx write fnx;
    property ny : smallint read fny write fny;
    property xlo : single read fxlo write fxlo;
    property xhi : single read fxhi write fxhi;
    property xrange : single read fxrange write fxrange;
    property xstep : single read fxstep write fxstep;
    property ylo : single read fylo write fylo;
    property yhi : single read fyhi write fyhi;
    property yrange : single read fyrange write fyrange;
    property ystep : single read fystep write fystep;
    property zlo : single read fzlo write fzlo;
    property zhi : single read fzhi write fzhi;
    property zrange : single read fzrange write fzrange;
  end;

implementation

uses
{** additional}
  CommaSplit;
// ------ TContourGridData.CalcDerived -----------------------------------------
{** calculates derived quantities such as step sizes and ranges. Also sets up
the points array}
procedure TContourGridData.CalcDerived;

begin
  xrange := xhi - xlo;
  xstep := xrange/(nx-1);

  yrange := yhi - ylo;
  ystep := yrange/(ny-1);

  zrange := zhi - zlo;

  ConstructArrays;
end;
// ------ TContourGridData.CalcDerivedNative -----------------------------------
{** calculates derived quantities such as ranges. Different to CalcDerived
as the native Surfer Grid/Surfer 7 format has different parameters}
procedure TContourGridData.CalcDerivedNative;


begin
  xrange := (nx-1)*xstep;
  xhi := xlo + xrange;

  yrange := (ny-1)*ystep;
  yhi := ylo + yrange;

  zrange := zhi - zlo;

  ConstructArrays;
end;
// ------ TContourGridData.ConstructArrays -------------------------------------
procedure TContourGridData.ConstructArrays;

var
  i:integer;

begin
  SetLength(points,ny,nx);
  SetLength(xscale,nx);
  SetLength(yscale,ny);

  xscale[0] := 0.0;
  for i:=1 to nx-1 do
    xscale[i] := xscale[i-1]+xstep;

  yscale[0] := 0.0;
  for i:=1 to ny-1 do
    yscale[i] := yscale[i-1]+ystep;
end;
// ------ TContourGridData.LoadSurferGSASCII -----------------------------------
{GS ASCII Grid File Format:
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
function TContourGridData.LoadSurferGSASCII(sFileName:string):integer;

var
// col = nx-1
// i = row counter
// j = column counter
// k = columns in line counter
// n = counter to increment through file

  filestrings : TStringList;
  col,i,j,k,n: integer;
  cs : TCommaSplitter;

  function ReadLine:string;

  begin
    result := filestrings[n];
    Inc(n);
  end;

begin
  filestrings := TStringList.Create;
  filestrings.LoadFromFile(sFileName);
  n := 0;

  cs := TCommaSplitter.Create(nil);
  cs.Delimiter := ' ';

  try
{** check for valid GS ASCII header}
    if (Copy(ReadLine,1,4) <> 'DSAA') then
    begin
      filestrings.Free;
      cs.Free;
      result := 1;
      exit;
    end;

{** read nx,ny}
    cs.Text := ReadLine;
    nx := StrToInt(cs.Items[0]);
    ny := StrToInt(cs.Items[1]);

{** read xlo,xhi}
    cs.Text := ReadLine;
    xlo := StrToFloat(cs.Items[0]);
    xhi := StrToFloat(cs.Items[1]);

{** read ylo,yhi}
    cs.Text := ReadLine;
    ylo := StrToFloat(cs.Items[0]);
    yhi := StrToFloat(cs.Items[1]);

{** read zlo,zhi}
    cs.Text := ReadLine;
    zlo := StrToFloat(cs.Items[0]);
    zhi := StrToFloat(cs.Items[1]);

{** calculate the derived quantites - step sizes etc}
    CalcDerived;

    col := nx-1;

{** loop over the rows - i}
    for i := 0 to ny-1 do
    begin
      j := 0;

{** keep reading lines until nx-1 (col) entries have been obtained}
      while j <= col do
      begin
        cs.Text := ReadLine;

        for k := 0 to cs.Items.Count-1 do
        begin
          if (j <= col) then
            points[i,j] := StrToFloat(cs.Items[k]);
          Inc(j);
        end;

        if (j > col) then
          break;
      end;
    end;

  finally
    result := 0;
    fileStrings.Free;
    cs.Free;
  end;
end;
// ------ TContourGridData.LoadSurferGSBinary ----------------------------------
{GS Binary Grid File Format:
GS Binary grid files [.grd] use a similar layout to the GS ASCII described
above. The difference is in the ID string and that the files are binary :)

Data types used:
char        -   single byte
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
function TContourGridData.LoadSurferGSBinary(sFileName:string):integer;

var
  binfile : file;
  i,j : smallint;
  d:double;
  zval : single;
  ndouble,nsingle,nsmallint,col:integer; // sizeof vars
  sType : array[1..4] of char;
  nread:integer; // number of bytes read - unused

begin
  AssignFile(binfile,sFileName);
  Reset(binfile,1); // record size of 1 byte

{** check to see if this is a GS Binary file}
  BlockRead(binfile,sType,4,nread);
  if (sType <> 'DSBB') then
  begin
    result := 1;
    CloseFile(binfile);
  end else
  begin
    ndouble := sizeof(d);
    nsingle := sizeof(zval);
    nsmallint := sizeof(i);

{** read nx,ny}
    BlockRead(binfile,i,nsmallint,nRead);
    nx := i;
    BlockRead(binfile,i,nsmallint,nRead);
    ny := i;

{** read xlo,xhi}
    BlockRead(binfile,d,ndouble,nread);
    xlo := d;
    BlockRead(binfile,d,ndouble,nread);
    xhi := d;

{** read ylo,yhi}
    BlockRead(binfile,d,ndouble,nread);
    ylo := d;
    BlockRead(binfile,d,ndouble,nread);
    yhi := d;

{** read zlo,zhi}
    BlockRead(binfile,d,ndouble,nread);
    zlo := d;
    BlockRead(binfile,d,ndouble,nread);
    zhi := d;

{** calculate the derived quantities - step sizes etc}
    CalcDerived;

    col := nx-1;

{** now read in the points}
    for i := 0 to ny-1 do
      for j := 0 to col do
      begin
        BlockRead(binfile,zval,nsingle,nRead);
        points[i,j] := zval;
      end;

    result := 0;
    CloseFile(binFile);
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

function TContourGridData.LoadSurferGridNative(sFileName:string):integer;

const
  sHEADER = '42525344';
  sFAULTINFO = '49544c46';
  sGRIDSECT = '44495247';
  sDATASECT = '41544144';
  iSECTSIZE = 8;

var
  binfile: file;
  buf: array of byte;
  dval : double;
  col,i,iHeaderID,iSize,iVal,iVersion,j,nDouble,nInteger,nRead : integer;
  sSectionID,sSectionID2 : string;

begin
  AssignFile(binfile,sFileName);
  Reset(binfile,1); {** set the default record size to 1 byte}

  nInteger := SizeOf(iHeaderID); {** just size of integer variable}

{** read in the header}
  BlockRead(binfile,iHeaderID,nInteger,nRead);
  sSectionID := IntToHex(iHeaderID,iSECTSIZE);
{** check the header tag}
  if (sSectionID <> sHEADER) then
  begin
    result := 1;
    CloseFile(binfile);
  end else
  begin
    nDouble := SizeOf(dVal); {** just size of double variable}
    BlockRead(binfile,iSize,nInteger,nRead);    {** size of header section}
    BlockRead(binfile,iVersion,nInteger,nRead); {** file version}
{** the sections are in any order...}
    while (not eof(binfile)) do
    begin
{** what section is this?}
      BlockRead(binfile,iHeaderID,nInteger,nRead);
      sSectionID := IntToHex(iHeaderID,iSECTSIZE);

{** FAULT INFO SECTION}
      if (sSectionID = sFAULTINFO) then
      begin
        BlockRead(binfile,iSize,nInteger,nRead); {** size of fault info section}
        SetLength(buf,iSize); {** set up the temp buffer}
        BlockRead(binfile,buf,iSize,nRead);      {** skip the section}
{** from the specs a data section will follow - skip this one as well}
        BlockRead(binfile,iHeaderID,nInteger,nRead);
        sSectionID2 := IntToHex(iHeaderID,iSECTSIZE);
        if (sSectionID2 = sDATASECT) then
        begin
          BlockRead(binfile,iSize,nInteger,nread);
          SetLength(buf,iSize); {** set up the temp buffer}
          BlockRead(binfile,buf,iSize,nRead); {** skip the section}
        end;
{** GRID SECTION}
      end else if (sSectionID = sGRIDSECT) then
      begin
        BlockRead(binfile,iSize,nInteger,nRead);  {** size of grid section}

        BlockRead(binfile,iVal,nInteger,nRead);
        ny := iVal;

        BlockRead(binFile,iVal,nInteger,nRead);
        nx := iVal;

        BlockRead(binfile,dVal,nDouble,nRead);
        xlo := dval;

        BlockRead(binfile,dVal,nDouble,nRead);
        ylo := dval;

        BlockRead(binFile,dVal,nDouble,nRead);
        xstep := dval;

        BlockRead(binfile,dVal,nDouble,nRead);
        ystep := dval;

        BlockRead(binfile,dval,nDouble,nread);
        zlo := dval;

        BlockRead(binfile,dval,nDouble,nread);
        zhi := dval;

        BlockRead(binfile,dval,nDouble,nread); {** rotation - not used here}
        BlockRead(binfile,dval,nDouble,nread); {** blankvalue - not used here}

        CalcDerivedNative;

{** from the specs a data section will follow - skip this one as well}
        BlockRead(binfile,iHeaderID,nInteger,nRead);
        sSectionID2 := IntToHex(iHeaderID,iSECTSIZE);
        if (sSectionID2 = sDATASECT) then
        begin
          col := nx-1;
          BlockRead(binfile,iSize,nInteger,nRead);

{** now read in the points}
          for i := 0 to ny-1 do
          begin
            for j := 0 to col do
            begin
              BlockRead(binfile,dval,ndouble,nRead);
              points[i,j] := dval;
            end;
          end;
        end;
      end; {while not eof}
    end;
    result := 0;
    CloseFile(binFile);
  end;
end;
// ------ TContourGridData.LoadSurferGrid --------------------------------------
function TContourGridData.LoadSurferGrid(sFileName:string):integer;

begin
{** native format first - Surfer 7 or 8}
  if (LoadSurferGridNative(sFileName) = 0) then
  begin
    result := 0;
    exit;
  end;

{** check if the grd file is GS Binary}
  if (LoadSurferGSBinary(sFileName) = 0) then
  begin
    result := 0;
    exit;
  end;

{** check if the grd file is GS ASCII}
  if (LoadSurferGSASCII(sFileName) = 0) then
  begin
    result := 0;
    exit;
  end;

{** not a surfer grid file}
  result := 1;
end;
// ------ TContourGridData.destroy ---------------------------------------------
destructor TContourGridData.destroy;
begin
  points := nil;
  xscale := nil;
  yscale := nil;
  inherited destroy;
end;
// =============================================================================
end.
