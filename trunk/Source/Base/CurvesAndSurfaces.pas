// CurvesAndSurfaces
{: Bezier and B-Spline Curve and Surface Routines.<p>

   <b>History : </b><font size=-1><ul>
      <li>10/07/03 - SG - Creation
   </ul></font>
}
unit CurvesAndSurfaces;

interface

uses
  SysUtils, Geometry, VectorLists;

type
  TBSplineContinuity = (bscUniformNonPeriodic, bscUniformPeriodic);

function BezierCurvePoint(t : single; n : integer; cp : PAffineVectorArray) : TAffineVector;
procedure GenerateBezierCurve(Steps : Integer; ControlPoints, Vertices : TAffineVectorList);

function BSplinePoint(t : single; n,k : integer; knots : PSingleArray; cp : PAffineVectorArray) : TAffineVector;
procedure GenerateBSpline(Order,Steps : Integer; KnotVector : TSingleList; ControlPoints, Vertices : TAffineVectorList);
procedure GenerateKnotVector(KnotVector : TSingleList; ControlPoints : TAffineVectorList;
                             Order : Integer; Continuity : TBSplineContinuity);

implementation

function Factorial(n : Integer) : Single;
var
  i : integer;
begin
  if (n<0) or (n>32) then
    Exception.Create('Invalid factorial parameter: n = '+IntToStr(n));

  Result:=1;
  for i:=2 to n do
    Result:=Result*i;
end;

// ------------------------------------------------------------
// Bezier curve routines
// ------------------------------------------------------------

function BezierCurvePoint(t : single; n : integer; cp : PAffineVectorArray) : TAffineVector;

  function Basis(n,i : Integer; t : Single) : Single;
  var
    ti, tni : Single;
  begin
    if (t=0) and (i=0) then ti:=1 else ti:=Power(t,i);
    if (n=i) and (t=1) then tni:=1 else tni:=Power(1-t,n-i);
    Result:=(Factorial(n)/(Factorial(i)*Factorial(n-i)))*ti*tni;
  end;

var
  i : integer;
  b : Single;
begin
  Result:=NullVector;
  for i:=0 to n do begin
    b:=Basis(n,i,t);
    Result[0]:=Result[0]+cp[i][0]*b;
    Result[1]:=Result[1]+cp[i][1]*b;
    Result[2]:=Result[2]+cp[i][2]*b;
  end;
end;

procedure GenerateBezierCurve(Steps : Integer; ControlPoints, Vertices : TAffineVectorList);
var
  i : Integer;
begin
  for i:=0 to Steps do
    Vertices.Add(BezierCurvePoint(i/Steps,ControlPoints.Count-1,ControlPoints.List));
end;

// ------------------------------------------------------------
// B-Spline routines
// ------------------------------------------------------------

function BSplinePoint(t : single; n,k : integer; knots : PSingleArray; cp : PAffineVectorArray) : TAffineVector;

  { Something not quite right in here.
    When u = 1 with a uniform non-periodic knot vector
    it returns 0 in every cp blend.
    When u -> 0 or u -> 1 with a uniform periodic knot vector
    the curve approaches the origin.
    This is an error I'm currently investigating. }
  function Basis(i,k : integer; u : Single) : Single;
  var
    t : single;
  begin
    Result:=0;
    if k=1 then begin
      if (u>=knots[i]) and (u<knots[i+1]) then Result:=1;
    end else begin
      t:=(knots[i+k-1]-knots[i]);
      if t<>0 then Result:=(u-knots[i])*Basis(i,k-1,u)/t;
      t:=(knots[i+k]-knots[i+1]);
      if t<>0 then Result:=Result+(knots[i+k]-u)*Basis(i+1,k-1,u)/t;
    end;
  end;

var
  i : integer;
  b : Single;
begin
  Result:=NullVector;
  for i:=0 to n do begin
    b:=Basis(i,k,t);
    Result[0]:=Result[0]+cp[i][0]*b;
    Result[1]:=Result[1]+cp[i][1]*b;
    Result[2]:=Result[2]+cp[i][2]*b;
  end;
end;

procedure GenerateBSpline(Order,Steps : Integer; KnotVector : TSingleList; ControlPoints, Vertices : TAffineVectorList);
var
  i : Integer;
begin
  for i:=0 to Steps do
    Vertices.Add(BSplinePoint(i/Steps,ControlPoints.Count-1,Order+1,KnotVector.List,ControlPoints.List));
end;

procedure GenerateKnotVector(KnotVector : TSingleList; ControlPoints : TAffineVectorList;
                             Order : Integer; Continuity : TBSplineContinuity);
var
  i,n,k : integer;
begin
  KnotVector.Clear;

  k:=Order+1;
  n:=ControlPoints.Count-1;

  case Continuity of

    // Open curve
    bscUniformNonPeriodic : begin
      for i:=0 to n+k do begin
        if i<k then KnotVector.Add(0)
        else if i>n then KnotVector.Add(n-k+2)
        else KnotVector.Add(i-k+1);
      end;
    end;

    // Closed curve
    bscUniformPeriodic : begin
      for i:=0 to n+k do begin
        KnotVector.Add(i);
      end;
    end;

  end;

  // Scale the values to range from 0 to 1
  KnotVector.Scale(1/KnotVector[KnotVector.Count-1]);
end;

end.