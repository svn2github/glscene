{: MathsDXF<p>

  DXF Maths<p>

  <b>History :</b><font size=-1><ul>
    <li>12/01/03 - DA - Unit creation
  </ul></font>
}
unit MathsDXF;

interface

uses
  System.Math,
  //GLS
  GLVectorGeometry,
  GLVectorTypes,
  TypesDXF;

{: Compute the angle between two points }
function Angle(P1, P2: T3DPoint): Extended;

{: This function returns a 3-D point at an angle and distance from another point.
  The given point is rotated by the angle specified in radians, then moved the
  specified distance from the point. }
function Polar(Point: T3DPoint; Angle, Distance: Single): T3DPoint;

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
implementation
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

// Angle
//
{: @return The angle between two points in degrees

   @param P1 The first point
   @param P2 the Second point }
function Angle(P1, P2: T3DPoint): Extended;
var
  Delta: T3DPoint;
begin
  Delta.X := P2.X - P1.X;
  Delta.Y := P2.Y - P1.Y;

  if (Delta.X = 0) then begin
    if ( P2.Y < P1.Y) then Result :=270.0 else Result := 90.0;
    Exit;
  end;

  if (Delta.Y = 0) then begin
    if (P2.X < P1.X) then Result := 180.0 else Result :=0.0;
    Exit;
  end;

  Result := RadToDeg(ArcTan(Delta.Y / Delta.X));

  if (Result <0) then Result := - Result;

  if (P2.X>P1.X) and (P2.Y>P1.Y) then Result :=   0.0 + Result;   // 1. Quadrant
  if (P2.X<P1.X) and (P2.Y>P1.Y) then Result := 180.0 - Result;   // 2. Quadrant
  if (P2.X<P1.X) and (P2.Y<P1.Y) then Result := 180.0 + Result;   // 3. Quadrant
  if (P2.X>P1.X) and (P2.Y<P1.Y) then Result := 360.0 - Result;   // 4. Quadrant
end;

// Polar
//
{: @return The new point

   @param Point the pole
   @param Angle Angle from the pole and the new point
   @param Distance Distance from the pole and the new point }
function Polar(Point: T3DPoint; Angle, Distance: Single): T3DPoint;
var
  ResultPoint: TAffineVector;
begin
  // move from the distance
  ResultPoint := AffineVectorMake(Distance, 0, 0);

  // rotate the point by the angle
  ResultPoint := VectorRotateAroundZ(ResultPoint, -Angle);

  // translate to the reference point
  AddVector(ResultPoint, AffineVectorMake(Point.X, Point.Y, Point.Z));

  Result.X := ResultPoint.X;
  Result.Y := ResultPoint.Y;
  Result.Z := ResultPoint.Z;
end;

end.
