// Credit to Paul Bourke (pbourke@swin.edu.au) for the original Fortran 77 Program :))
// Conversion to Visual Basic by EluZioN (EluZioN@casesladder.com)
// Conversion from VB to Delphi6 by Dr Steve Evans (steve@lociuk.com)
/// ////////////////////////////////////////////////////////////////////////////
// June 2002 Update by Dr Steve Evans (steve@lociuk.com): Heap memory allocation
// added to prevent stack overflow when MaxVertices and MaxTriangles are very large.
// Additional Updates in June 2002:
// Bug in InCircle function fixed. Radius r := Sqrt(rsqr).
// Check for duplicate points added when inserting new point.
// For speed, all points pre-sorted in x direction using quicksort algorithm and
// triangles flagged when no longer needed. The circumcircle centre and radius of
// the triangles are now stored to improve calculation time.
/// ////////////////////////////////////////////////////////////////////////////
// You can use this code however you like providing the above credits remain in tact

{ ** Updated May 2003 - ARH code cleanups }

unit cDelaunay;

interface

uses
  System.Types, Vcl.Dialogs,
  GLMesh, GLTexture, GLVectorGeometry, GLVectorTypes, GLColor;

{ ** unit constants - set as applicable }
const
  MaxVertices = 500000;

const
  MaxTriangles = 1000000;

const
  ExPtTolerance = 0.000001;

  { ** points (vertices) }
type
  dVertex = record
    x: Double;
    y: Double;
    z: Double; { ** added to save height of terrain }
  end;

  { ** reated Triangles, vv# are the vertex pointers }
type
  dTriangle = record
    vv0: LongInt;
    vv1: LongInt;
    vv2: LongInt;
    precalc: Integer;
    xc, yc, r: Double;
  end;

type
  TDVertex = array [0 .. MaxVertices] of dVertex;

type
  PVertex = ^TDVertex;

type
  TDTriangle = array [0 .. MaxTriangles] of dTriangle;

type
  PTriangle = ^TDTriangle;

type
  TDComplete = array [0 .. MaxTriangles] of Boolean;

type
  PComplete = ^TDComplete;

type
  TDEdges = array [0 .. 2, 0 .. MaxTriangles * 3] of LongInt;

type
  PEdges = ^TDEdges;

type
  TDelaunay = class
  private
    fHowMany: Integer;
    ftPoints: Integer;
    fGLMesh: TGLMesh;
    function InCircle(xp, yp, x1, y1, x2, y2, x3, y3: Double; var xc: Double;
      var yc: Double; var r: Double; j: Integer): Boolean;
    function Triangulate(nvert: Integer): Integer;
    function WhichSide(xp, yp, x1, y1, x2, y2: Double): Integer;
  public
    Triangle: PTriangle;
    Vertex: PVertex;

    constructor Create;
    destructor Destroy;

    procedure Mesh;
    procedure AddPoint(x, y, z: single); overload;
    procedure AddPoint(x, y: single); overload;
    procedure QuickSort(var A: PVertex; Low, High: Integer);
    procedure Draw;

    property HowMany: Integer read fHowMany write fHowMany;
    { ** Variable for total number of points (vertices) }
    property tPoints: Integer read ftPoints write ftPoints;
    property GLMesh: TGLMesh read fGLMesh write fGLMesh;
  end;

implementation

// ----- TDelaunay.InCircle ----------------------------------------------------
{ ** return _TRUE_ if the point (xp,yp) lies inside the circumcircle made up by
  points (x1,y1) (x2,y2) (x3,y3).
  The circumcircle centre is returned in (xc,yc) and the radius r.
  Note: A point on the edge is inside the circumcircle }
function TDelaunay.InCircle(xp, yp, x1, y1, x2, y2, x3, y3: Double;
  var xc: Double; var yc: Double; var r: Double; j: Integer): Boolean;

const
  EPS = 1E-6;

var
  m1, m2, mx1, mx2, my1, my2, dx, dy, rsqr, drsqr: Double;

begin
  result := false;
  { ** check if xc,yc and r have already been calculated }
  if (Triangle^[j].precalc = 1) then
  begin
    xc := Triangle^[j].xc;
    yc := Triangle^[j].yc;
    r := Triangle^[j].r;
    rsqr := r * r;
    dx := xp - xc;
    dy := yp - yc;
    drsqr := dx * dx + dy * dy;
  end
  else
  begin
    if (abs(y1 - y2) < EPS) and (abs(y2 - y3) < EPS) then
    begin
      ShowMessage('InCircle - F - Points are coincident !');
      exit;
    end;

    if (abs(y2 - y1) < EPS) then
    begin
      m2 := -(x3 - x2) / (y3 - y2);
      mx2 := 0.5 * (x2 + x3);
      my2 := 0.5 * (y2 + y3);
      xc := 0.5 * (x2 + x1);
      yc := m2 * (xc - mx2) + my2;
    end
    else if (abs(y3 - y2) < EPS) then
    begin
      m1 := -(x2 - x1) / (y2 - y1);
      mx1 := 0.5 * (x1 + x2);
      my1 := 0.5 * (y1 + y2);
      xc := 0.5 * (x3 + x2);
      yc := m1 * (xc - mx1) + my1;
    end
    else
    begin
      m1 := -(x2 - x1) / (y2 - y1);
      m2 := -(x3 - x2) / (y3 - y2);
      mx1 := 0.5 * (x1 + x2);
      mx2 := 0.5 * (x2 + x3);
      my1 := 0.5 * (y1 + y2);
      my2 := 0.5 * (y2 + y3);
      if ((m1 - m2) <> 0) then
      begin
        xc := (m1 * mx1 - m2 * mx2 + my2 - my1) / (m1 - m2);
        yc := m1 * (xc - mx1) + my1;
      end
      else
      begin
        xc := (x1 + x2 + x3) / 3;
        yc := (y1 + y2 + y3) / 3;
      end;
    end;

    dx := x2 - xc;
    dy := y2 - yc;
    rsqr := dx * dx + dy * dy;
    r := sqrt(rsqr);
    dx := xp - xc;
    dy := yp - yc;
    drsqr := dx * dx + dy * dy;

    { ** store the xc,yc and r for later use }
    Triangle^[j].precalc := 1;
    Triangle^[j].xc := xc;
    Triangle^[j].yc := yc;
    Triangle^[j].r := r;
  end;

  if (drsqr <= rsqr) then
    result := True;
end;

// ----- TDelaunay.Triangulate -------------------------------------------------
{ ** takes as input NVERT vertices in arrays Vertex()
  Returned is a list of NTRI triangular faces in the raray Triangle(). These
  triangles are arranged in clockwise order }
function TDelaunay.Triangulate(nvert: Integer): Integer;

var
  complete: PComplete;
  edges: PEdges;
  nEdge: LongInt;
  { ** for super triangle }
  xmin, xmax, ymin, ymax, xmid, ymid, dx, dy, dmax, xc, yc, r: Double;
  { ** general variables }
  i, j, k, ntri: Integer;
  inc: Boolean;

begin
  { ** allocate memory }
  GetMem(complete, sizeof(complete^));
  GetMem(edges, sizeof(edges^));

  { ** Find the maximum and minimum vertex bounds. This is to allow calculation of
    the bounding triangle }
  xmin := Vertex^[1].x;
  ymin := Vertex^[1].y;
  xmax := xmin;
  ymax := ymin;

  for i := 2 To nvert do
  begin
    if Vertex^[i].x < xmin then
      xmin := Vertex^[i].x;
    if Vertex^[i].x > xmax then
      xmax := Vertex^[i].x;
    if Vertex^[i].y < ymin then
      ymin := Vertex^[i].y;
    if Vertex^[i].y > ymax then
      ymax := Vertex^[i].y;
  end;

  dx := xmax - xmin;
  dy := ymax - ymin;

  if (dx > dy) then
    dmax := dx
  else
    dmax := dy;

  xmid := Trunc(0.5 * (xmax + xmin));
  ymid := Trunc(0.5 * (ymax + ymin));

  // Set up the supertriangle
  // This is a triangle which encompasses all the sample points.
  // The supertriangle coordinates are added to the end of the
  // vertex list. The supertriangle is the first triangle in
  // the triangle list.

  Vertex^[nvert + 1].x := (xmid - 2 * dmax);
  Vertex^[nvert + 1].y := (ymid - dmax);
  Vertex^[nvert + 2].x := xmid;
  Vertex^[nvert + 2].y := (ymid + 2 * dmax);
  Vertex^[nvert + 3].x := (xmid + 2 * dmax);
  Vertex^[nvert + 3].y := (ymid - dmax);
  Triangle^[1].vv0 := nvert + 1;
  Triangle^[1].vv1 := nvert + 2;
  Triangle^[1].vv2 := nvert + 3;
  Triangle^[1].precalc := 0;

  complete[1] := false;
  ntri := 1;

  // Include each point one at a time into the existing mesh
  For i := 1 To nvert do
  begin
    nEdge := 0;
    // Set up the edge buffer.
    // If the point (Vertex(i).x,Vertex(i).y) lies inside the circumcircle then the
    // three edges of that triangle are added to the edge buffer.
    j := 0;
    repeat
      j := j + 1;
      If complete^[j] <> True Then
      begin
        inc := InCircle(Vertex^[i].x, Vertex^[i].y, Vertex^[Triangle^[j].vv0].x,
          Vertex^[Triangle^[j].vv0].y, Vertex^[Triangle^[j].vv1].x,
          Vertex^[Triangle^[j].vv1].y, Vertex^[Triangle^[j].vv2].x,
          Vertex^[Triangle^[j].vv2].y, xc, yc, r, j);
        // Include this if points are sorted by X
        If (xc + r) < Vertex[i].x Then //
          complete[j] := True //
        Else //
          If inc Then
          begin
            edges^[1, nEdge + 1] := Triangle^[j].vv0;
            edges^[2, nEdge + 1] := Triangle^[j].vv1;
            edges^[1, nEdge + 2] := Triangle^[j].vv1;
            edges^[2, nEdge + 2] := Triangle^[j].vv2;
            edges^[1, nEdge + 3] := Triangle^[j].vv2;
            edges^[2, nEdge + 3] := Triangle^[j].vv0;
            nEdge := nEdge + 3;
            Triangle^[j].vv0 := Triangle^[ntri].vv0;
            Triangle^[j].vv1 := Triangle^[ntri].vv1;
            Triangle^[j].vv2 := Triangle^[ntri].vv2;
            Triangle^[j].precalc := Triangle^[ntri].precalc;
            Triangle^[j].xc := Triangle^[ntri].xc;
            Triangle^[j].yc := Triangle^[ntri].yc;
            Triangle^[j].r := Triangle^[ntri].r;
            Triangle^[ntri].precalc := 0;
            complete^[j] := complete^[ntri];
            j := j - 1;
            ntri := ntri - 1;
          End;
      End;
    until j >= ntri;

    // Tag multiple edges
    // Note: if all triangles are specified anticlockwise then all
    // interior edges are opposite pointing in direction.
    For j := 1 To nEdge - 1 do
    begin
      If Not(edges^[1, j] = 0) And Not(edges^[2, j] = 0) Then
      begin
        For k := j + 1 To nEdge do
        begin
          If Not(edges^[1, k] = 0) And Not(edges^[2, k] = 0) Then
          begin
            If edges^[1, j] = edges^[2, k] Then
            begin
              If edges^[2, j] = edges^[1, k] Then
              begin
                edges^[1, j] := 0;
                edges^[2, j] := 0;
                edges^[1, k] := 0;
                edges^[2, k] := 0;
              End;
            End;
          End;
        end;
      End;
    end;

    // Form new triangles for the current point
    // Skipping over any tagged edges.
    // All edges are arranged in clockwise order.
    For j := 1 To nEdge do
    begin
      If Not(edges^[1, j] = 0) And Not(edges^[2, j] = 0) Then
      begin
        ntri := ntri + 1;
        Triangle^[ntri].vv0 := edges^[1, j];
        Triangle^[ntri].vv1 := edges^[2, j];
        Triangle^[ntri].vv2 := i;
        Triangle^[ntri].precalc := 0;
        complete^[ntri] := false;
      End;
    end;
  end;

  // Remove triangles with supertriangle vertices
  // These are triangles which have a vertex number greater than NVERT
  i := 0;
  repeat
    i := i + 1;
    If (Triangle^[i].vv0 > nvert) Or (Triangle^[i].vv1 > nvert) Or
      (Triangle^[i].vv2 > nvert) Then
    begin
      Triangle^[i].vv0 := Triangle^[ntri].vv0;
      Triangle^[i].vv1 := Triangle^[ntri].vv1;
      Triangle^[i].vv2 := Triangle^[ntri].vv2;
      i := i - 1;
      ntri := ntri - 1;
    End;
  until i >= ntri;

  Triangulate := ntri;

  // Free memory
  FreeMem(complete, sizeof(complete^));
  FreeMem(edges, sizeof(edges^));
End;

// ----- TDelaunay.Create ------------------------------------------------------
constructor TDelaunay.Create;

begin
  { ** Initialise total points to 1, using base 0 causes problems in the functions }
  ftPoints := 1;
  fHowMany := 0;

  { ** allocate memory for the arrays }
  GetMem(Triangle, sizeof(Triangle^));
  GetMem(Vertex, sizeof(Vertex^));
end;

// ----- TDelaunay.Destroy -----------------------------------------------------
destructor TDelaunay.Destroy;

begin
  { ** free memory for arrays }
  FreeMem(Triangle, sizeof(Triangle^));
  FreeMem(Vertex, sizeof(Vertex^));
end;

Function TDelaunay.WhichSide(xp, yp, x1, y1, x2, y2: Double): Integer;
// Determines which side of a line the point (xp,yp) lies.
// The line goes from (x1,y1) to (x2,y2)
// Returns -1 for a point to the left
// 0 for a point on the line
// +1 for a point to the right
var
  equation: Double;
begin
  equation := ((yp - y1) * (x2 - x1)) - ((y2 - y1) * (xp - x1));

  If equation > 0 Then
    WhichSide := -1
  Else If equation = 0 Then
    WhichSide := 0
  Else
    WhichSide := 1;

End;

procedure TDelaunay.Mesh;
begin
  QuickSort(Vertex, 1, tPoints - 1);
  If tPoints > 3 Then
    HowMany := Triangulate(tPoints - 1);
  // 'Returns number of triangles created.
end;

procedure TDelaunay.AddPoint(x, y, z: single);
var
  i, AE: Integer;
begin

  // Check for duplicate points
  AE := 0;
  i := 1;
  while i < tPoints do
  begin
    If (abs(x - Vertex^[i].x) < ExPtTolerance) and
      (abs(y - Vertex^[i].y) < ExPtTolerance) Then
      AE := 1;
    inc(i);
  end;

  if AE = 0 then
  begin
    // Set Vertex coordinates where you clicked the pic box
    Vertex^[tPoints].x := x;
    Vertex^[tPoints].y := y;
    Vertex^[tPoints].z := z;
    // Increment the total number of points
    tPoints := tPoints + 1;
  end;

end;

procedure TDelaunay.AddPoint(x, y: single);
var
  i, AE: Integer;
begin

  // Check for duplicate points
  AE := 0;
  i := 1;
  while i < tPoints do
  begin
    If (abs(x - Vertex^[i].x) < ExPtTolerance) and
      (abs(y - Vertex^[i].y) < ExPtTolerance) Then
      AE := 1;
    inc(i);
  end;

  if AE = 0 then
  begin
    // Set Vertex coordinates where you clicked the pic box
    Vertex^[tPoints].x := x;
    Vertex^[tPoints].y := y;
    // Increment the total number of points
    tPoints := tPoints + 1;
  end;

end;

procedure TDelaunay.QuickSort(var A: PVertex; Low, High: Integer);
// Sort all points by x
  procedure DoQuickSort(var A: PVertex; iLo, iHi: Integer);
  var
    Lo, Hi: Integer;
    Mid: Double;
    T: dVertex;
  begin
    Lo := iLo;
    Hi := iHi;
    Mid := A^[(Lo + Hi) div 2].x;
    repeat
      while A^[Lo].x < Mid do
        inc(Lo);
      while A^[Hi].x > Mid do
        Dec(Hi);
      if Lo <= Hi then
      begin
        T := A^[Lo];
        A^[Lo] := A^[Hi];
        A^[Hi] := T;
        inc(Lo);
        Dec(Hi);
      end;
    until Lo > Hi;
    if Hi > iLo then
      DoQuickSort(A, iLo, Hi);
    if Lo < iHi then
      DoQuickSort(A, Lo, iHi);
  end;

begin
  DoQuickSort(A, Low, High);
end;

procedure TDelaunay.Draw;
var
  // variable to hold how many triangles are created by the triangulate function
  i: Integer;
  p1, p2, p3: TAffineVector;

begin
  GLMesh.Vertices.Clear;

  // Draw the created triangles
  if (HowMany > 0) then
  begin
    For i := 1 To HowMany do
    begin
      with GLMesh.Vertices do
      begin
        SetVector(p1, Vertex^[Triangle^[i].vv0].x, Vertex^[Triangle^[i].vv0].y,
          Vertex^[Triangle^[i].vv0].z);
        SetVector(p2, Vertex^[Triangle^[i].vv1].x, Vertex^[Triangle^[i].vv1].y,
          Vertex^[Triangle^[i].vv1].z);
        SetVector(p3, Vertex^[Triangle^[i].vv2].x, Vertex^[Triangle^[i].vv2].y,
          Vertex^[Triangle^[i].vv2].z);
        AddVertex(p3, NullVector, clrGray);
        AddVertex(p2, NullVector, clrGray);
        AddVertex(p1, NullVector, clrGray);
      end;
    end;
  end;
end;

// =============================================================================
end.
