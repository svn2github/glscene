// CurvesAndSurfaces
{: Bezier and B-Spline Curve and Surface Routines.<p>

   <b>History : </b><font size=-1><ul>
      <li>20/08/03 - SG - Removed weights realizing it's an inefficient way
                          to do things, control points should be weighted
                          before being used to calculate a surface or curve.
      <li>18/08/03 - SG - Added weights to calculations.
      <li>17/07/03 - SG - Added surface routines. 
                          Minor changes to procedure parameters.
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
function BezierSurfacePoint(s,t : single; m,n : integer; cp : PAffineVectorArray) : TAffineVector;
procedure GenerateBezierCurve(Steps : Integer; ControlPoints, Vertices : TAffineVectorList);
procedure GenerateBezierSurface(Steps, Width, Height : Integer; ControlPoints, Vertices : TAffineVectorList);

function BSplinePoint(t : single; n,k : integer; knots : PSingleArray; cp : PAffineVectorArray) : TAffineVector;
function BSplineSurfacePoint(s,t : single; m,n,k1,k2 : integer; uknots, vknots : PSingleArray; cp : PAffineVectorArray) : TAffineVector;
procedure GenerateBSpline(Steps,Order : Integer; KnotVector : TSingleList; ControlPoints, Vertices : TAffineVectorList);
procedure GenerateBSplineSurface(Steps, UOrder, VOrder, Width, Height : Integer; UKnotVector, VKnotVector : TSingleList; ControlPoints, Vertices : TAffineVectorList);
procedure GenerateKnotVector(KnotVector : TSingleList; NumberOfPoints, Order : Integer; Continuity : TBSplineContinuity);

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
// Bezier routines
// ------------------------------------------------------------

function BernsteinBasis(n,i : Integer; t : Single) : Single;
var
  ti, tni : Single;
begin
  if (t=0) and (i=0) then ti:=1 else ti:=Power(t,i);
  if (n=i) and (t=1) then tni:=1 else tni:=Power(1-t,n-i);
  Result:=(Factorial(n)/(Factorial(i)*Factorial(n-i)))*ti*tni;
end;

function BezierCurvePoint(t : single; n : integer; cp : PAffineVectorArray) : TAffineVector;
var
  i : integer;
  b : Single;
begin
  Result:=NullVector;
  for i:=0 to n do begin
    b:=BernsteinBasis(n,i,t);
    Result[0]:=Result[0]+cp[i][0]*b;
    Result[1]:=Result[1]+cp[i][1]*b;
    Result[2]:=Result[2]+cp[i][2]*b;
  end;
end;

function BezierSurfacePoint(s,t : single; m,n : integer; cp : PAffineVectorArray) : TAffineVector;
var
  i,j : integer;
  b1,b2 : Single;
begin
  Result:=NullVector;
  for j:=0 to n do
    for i:=0 to m do begin
      b1:=BernsteinBasis(m,i,s);
      b2:=BernsteinBasis(n,j,t);
      Result[0]:=Result[0]+cp[j*(m+1)+i][0]*b1*b2;
      Result[1]:=Result[1]+cp[j*(m+1)+i][1]*b1*b2;
      Result[2]:=Result[2]+cp[j*(m+1)+i][2]*b1*b2;
    end;
end;

procedure GenerateBezierCurve(Steps : Integer; ControlPoints, Vertices : TAffineVectorList);
var
  i : Integer;
begin
  for i:=0 to Steps do
    Vertices.Add(BezierCurvePoint(i/Steps,ControlPoints.Count-1,ControlPoints.List));
end;

procedure GenerateBezierSurface(Steps, Width, Height : Integer; ControlPoints, Vertices : TAffineVectorList);
var
  i,j : Integer;
begin
  for j:=0 to Steps do
    for i:=0 to Steps do
      Vertices.Add(BezierSurfacePoint(i/Steps,j/Steps,Width-1,Height-1,ControlPoints.List));
end;

// ------------------------------------------------------------
// B-Spline routines
// ------------------------------------------------------------

function BSplineBasis(i,k : integer; u : Single; knots : PSingleArray) : Single;
var
  t : single;
begin
  Result:=0;
  if k=1 then begin
    if (u>=knots[i]) and (u<knots[i+1]) then Result:=1;
  end else begin
    t:=(knots[i+k-1]-knots[i]);
    if t<>0 then Result:=(u-knots[i])*BSplineBasis(i,k-1,u,knots)/t;
    t:=(knots[i+k]-knots[i+1]);
    if t<>0 then Result:=Result+(knots[i+k]-u)*BSplineBasis(i+1,k-1,u,knots)/t;
  end;
end;

function BSplinePoint(t : single; n,k : integer; knots : PSingleArray; cp : PAffineVectorArray) : TAffineVector;
var
  i : integer;
  b : Single;
begin
  Result:=NullVector;
  for i:=0 to n do begin
    b:=BSplineBasis(i,k,t,knots);
    Result[0]:=Result[0]+cp[i][0]*b;
    Result[1]:=Result[1]+cp[i][1]*b;
    Result[2]:=Result[2]+cp[i][2]*b;
  end;
end;

function BSplineSurfacePoint(s,t : single; m,n,k1,k2 : integer; uknots, vknots : PSingleArray; cp : PAffineVectorArray) : TAffineVector;
var
  i,j : integer;
  b1,b2 : Single;
begin
  Result:=NullVector;
  for j:=0 to n do
    for i:=0 to m do begin
      b1:=BSplineBasis(i,k1,s,uknots);
      b2:=BSplineBasis(j,k2,t,vknots);
      Result[0]:=Result[0]+cp[j*(m+1)+i][0]*b1*b2;
      Result[1]:=Result[1]+cp[j*(m+1)+i][1]*b1*b2;
      Result[2]:=Result[2]+cp[j*(m+1)+i][2]*b1*b2;
    end;
end;

procedure GenerateBSpline(Steps,Order : Integer; KnotVector : TSingleList; ControlPoints, Vertices : TAffineVectorList);
var
  i : Integer;
begin
  for i:=0 to Steps do
    Vertices.Add(BSplinePoint(i/Steps,ControlPoints.Count-1,Order+1,KnotVector.List,ControlPoints.List));
end;

procedure GenerateBSplineSurface(Steps, UOrder, VOrder, Width, Height : Integer; UKnotVector, VKnotVector : TSingleList; ControlPoints, Vertices : TAffineVectorList);
var
  i,j : Integer;
begin
  for j:=0 to Steps do
    for i:=0 to Steps do
      Vertices.Add(BSplineSurfacePoint(i/Steps,j/Steps,Width-1,Height-1,UOrder+1,VOrder+1,UKnotVector.List,VKnotVector.List,ControlPoints.List));
end;

procedure GenerateKnotVector(KnotVector : TSingleList; NumberOfPoints, Order : Integer; Continuity : TBSplineContinuity);
var
  i,n,k : integer;
begin
  KnotVector.Clear;

  k:=Order+1;
  n:=NumberOfPoints-1;

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
  // KnotVector.Scale(1/KnotVector[KnotVector.Count-1]);
end;

end.
