{: GLEllipseCollision<p>

  Ellipsoid Collision

    Based on the Improved Collision detection and Response
    by Kasper Fauerby
    kasper@peroxide.dk
    http://www.peroxide.dk

  Known bugs:
  - Need to check if the point is embedded in the ellipsoid (procedure CheckPoint)

  <b>History : </b><font size=-1><ul>
    <li>03/09/04 - LucasG. - Creation and addons to support events and terrains
  </ul></font>
}

unit GLEllipseCollision;

interface

uses {$IFNDEF GLS_DELPHI_5}Types,{$ENDIF} VectorGeometry, GLTerrainRenderer;

type
  TECPlane = class
  public
    Equation: array [0..3] of Single;
    Origin: TAffineVector;
    Normal: TAffineVector;
    procedure MakePlane(const nOrigin, nNormal: TAffineVector); overload;
    procedure MakePlane(const p1, p2, p3: TAffineVector); overload;
    function isFrontFacingTo(const Direction: TAffineVector): Boolean;
    function signedDistanceTo(const Point: TAffineVector): Single;
  end;

  TECTriangle = record
    p1,p2,p3: TAffineVector;
    ObjectIndex: Integer;
    Friction: Single;
    Solid: Boolean;
  end;

  TECTriangleList = Array of TECTriangle;

  TECIntersection = record
    Point: TAffineVector;
    Normal: TAffineVector; //Surface normal
    Bounce: TAffineVector; //Surface relfection
    ObjectIndex: Integer;
    Friction, Distance: Single;
    Solid: Boolean;
    Nearest: Boolean;
  end;

  //csBox no implemented yet (detected as Ellipsoid for now)
  //When using csPoint the Size is the Normal
  TECColliderShape = (csEllipsoid, csBox, csPoint);
  TECCollider = record
    Position: TAffineVector;
    Size: TAffineVector;
    ObjectIndex: Integer;
    Friction: Single;
    Solid: Boolean;
    Shape: TECColliderShape;
  end;

  PTerrainRenderer = TGLTerrainRenderer;
  TECTerrain = record
    Terrain: PTerrainRenderer;
    Collider: TECCollider;
  end;

  TECIntersectionList = Array of TECIntersection;

  TECCollisionPacket = record
    {eRadius: TAffineVector; // ellipsoid radius
    // Information about the move being requested: (in R3)
    R3Velocity: TAffineVector;
    R3Position: TAffineVector;}
    // Information about the move being requested: (in eSpace)
    velocity: TAffineVector;
    normalizedVelocity: TAffineVector;
    basePoint: TAffineVector;
    // Hit information
    foundCollision: Boolean;
    nearestDistance: Single;
    intersectionPoint: TAffineVector;
    intersectionNormal: TAffineVector;
    objectIndex: Integer;
    Solid: boolean;
  end;

  TECMovementPacket = record
    //List of closest triangles to collide
    Triangles : TECTriangleList;
    //List of external (non-triangles) possible colliders
    Colliders : array of TECCollider;
    TerrainColliders : array of TECCollider;
    Terrains : array of TECTerrain;
    //Movement params
    Position  : TAffineVector;
    Velocity  : TAffineVector;
    Gravity   : TAffineVector;
    Radius    : TAffineVector;
    Solid     : Boolean; //Object can "walk thru walls" if = False
    //Internal
    collisionRecursionDepth : Byte;
    //Result
    FoundCollision: Boolean;
    ResultPos : TAffineVector;
    Distance  : Single;
    InGround  : Boolean;
    ColPoint: TAffineVector;
    ColNormal: TAffineVector; //Surface normal
    ColBounce: TAffineVector; //Surface Reflection
    ColObjectIndex: Integer;
    CollisionList: TECIntersectionList;
  end;

function VectorDivide(const v, divider : TAffineVector): TAffineVector;
procedure CollideAndSlide(var MovePack: TECMovementPacket);
procedure CollideWithWorld(var MovePack: TECMovementPacket; pos, vel: TAffineVector);

const unitsPerMeter : Single = 100;

//for DEBUG!!
//var d_v1, d_v2, d_v3: TAffineVector;

implementation

{ Utility functions }

function CheckPointInTriangle(point, pa, pb, pc: TAffineVector):boolean;
var
  e10, e20, vp : TAffineVector;
  a,b,c,d,e,ac_bb, x,y,z: Single;
begin
  e10 := VectorSubtract(pb,pa);
  e20 := VectorSubtract(pc,pa);
  a := VectorDotProduct(e10,e10);
  b := VectorDotProduct(e10,e20);
  c := VectorDotProduct(e20,e20);
  ac_bb := (a*c)-(b*b);
  vp := VectorSubtract(point,pa);
  d := VectorDotProduct(vp,e10);
  e := VectorDotProduct(vp,e20);
  x := (d*c)-(e*b);
  y := (e*a)-(d*b);
  z := x+y-ac_bb;
  result := ((z < 0) and not((x < 0) or (y < 0)));
end;

function getLowestRoot(a, b, c, maxR: Single; var Root: Single): Boolean;
var
  determinant: Single;
  sqrtD, r1,r2, temp: Single;
begin
  // No (valid) solutions
  result := false;
  // Check if a solution exists
  determinant := b*b - 4*a*c;
  // If determinant is negative it means no solutions.
  if (determinant < 0) then Exit;
  // calculate the two roots: (if determinant == 0 then
  // x1==x2 but let’s disregard that slight optimization)
  sqrtD := sqrt(determinant);
  r1 := (-b - sqrtD) / (2*a);
  r2 := (-b + sqrtD) / (2*a);
  // Sort so x1 <= x2
  if (r1 > r2) then
  begin
    temp := r2;
    r2 := r1;
    r1 := temp;
  end;
  // Get lowest root:
  if (r1 > 0) and (r1 < maxR) then
  begin
    root := r1;
    result := true;
    Exit;
  end;
  // It is possible that we want x2 - this can happen
  // if x1 < 0
  if (r2 > 0) and (r2 < maxR) then
  begin
    root := r2;
    result := true;
    Exit;
  end;
end;

function VectorDivide(const v, divider : TAffineVector): TAffineVector;
begin
   result[0]:=v[0]/divider[0];
   result[1]:=v[1]/divider[1];
   result[2]:=v[2]/divider[2];
end;

procedure VectorSetLength(var V: TAffineVector; Len: Single);
var l,l2: Single;
begin
  l2 := V[0]*V[0] + V[1]*V[1] + V[2]*V[2];
  l := sqrt(l2);
  if L <> 0 then
  begin
    Len := Len / l;
    V[0] :=  V[0] * Len;
    V[1] :=  V[1] * Len;
    V[2] :=  V[2] * Len;
  end;
end;

{ TECPlane }

procedure TECPlane.MakePlane(const nOrigin, nNormal: TAffineVector);
begin
  Normal := nNormal;
  Origin := nOrigin;
  Equation[0] := normal[0];
  Equation[1] := normal[1];
  Equation[2] := normal[2];
  Equation[3] := -(normal[0]*origin[0]+normal[1]*origin[1]+normal[2]*origin[2]);
end;

procedure TECPlane.MakePlane(const p1, p2, p3: TAffineVector);
begin
  Normal := CalcPlaneNormal(p1,p2,p3);
  Origin := p1;
  Equation[0] := normal[0];
  Equation[1] := normal[1];
  Equation[2] := normal[2];
  Equation[3] := -(normal[0]*origin[0]+normal[1]*origin[1]+normal[2]*origin[2]);
end;

function TECPlane.isFrontFacingTo(const Direction: TAffineVector): Boolean;
var Dot: Single;
begin
  Dot := VectorDotProduct(Normal,Direction);
  result := (Dot <= 0);
end;

function TECPlane.signedDistanceTo(const Point: TAffineVector): Single;
begin
  result := VectorDotProduct(Point,Normal) + Equation[3];
end;

{ Collision detection functions }

// Assumes: p1,p2 and p3 are given in ellisoid space:
function CheckTriangle(var colPackage: TECCollisionPacket;
                       const p1, p2, p3: TAffineVector;
                       var Intersection: TECIntersection): Boolean;
var
  trianglePlane: TECPlane;
  t0,t1: Double;
  embeddedInPlane: Boolean;
  signedDistToTrianglePlane: Double;
  normalDotVelocity: Single;
  temp: Double;

  collisionPoint : TAffineVector;
  foundCollison : Boolean;
  t : Single;

  planeIntersectionPoint, V: TAffineVector;

  velocity,base : TAffineVector;
  velocitySquaredLength : Single;
  a,b,c: Single; // Params for equation
  newT: Single;

  edge, baseToVertex : TAffineVector;
  edgeSquaredLength : Single;
  edgeDotVelocity : Single;
  edgeDotBaseToVertex : Single;

  distToCollision : Single;

begin
  result := False;
  // Make the plane containing this triangle.
  trianglePlane := TECPlane.Create;
  trianglePlane.MakePlane(p1,p2,p3);
  // Is triangle front-facing to the velocity vector?
  // We only check front-facing triangles
  // (your choice of course)
  if not (trianglePlane.isFrontFacingTo(colPackage.normalizedVelocity)) then
  begin
    trianglePlane.Free;
    Exit;
  end;//}
  // Get interval of plane intersection:
  embeddedInPlane := false;
  // Calculate the signed distance from sphere
  // position to triangle plane
  signedDistToTrianglePlane := trianglePlane.signedDistanceTo(colPackage.basePoint);
  // cache this as we’re going to use it a few times below:
  normalDotVelocity := VectorDotProduct(trianglePlane.normal,colPackage.velocity);
  // if sphere is travelling parrallel to the plane:
  if (normalDotVelocity = 0) then
  begin
    if (abs(signedDistToTrianglePlane) >= 1) then
    begin
      // Sphere is not embedded in plane.
      // No collision possible:
      trianglePlane.Free;
      Exit;
    end else
    begin
      // sphere is embedded in plane.
      // It intersects in the whole range [0..1]
      embeddedInPlane := true;
      t0 := 0;
      //t1 := 1;
    end;
  end else
  begin
    // N dot D is not 0. Calculate intersection interval:
    t0 := (-1 - signedDistToTrianglePlane)/normalDotVelocity;
    t1 := ( 1 - signedDistToTrianglePlane)/normalDotVelocity;
    // Swap so t0 < t1
    if (t0 > t1) then
    begin
      temp := t1;
      t1 := t0;
      t0 := temp;
    end;
    // Check that at least one result is within range:

    if (t0 > 1) or (t1 < 0) then
    begin
      trianglePlane.Free;
      Exit; // Both t values are outside values [0,1] No collision possible:
    end;
    // Clamp to [0,1]
    if (t0 < 0) then t0 := 0;
    if (t0 > 1) then t0 := 1;
    //if (t1 < 0) then t1 := 0;
    //if (t1 > 1) then t1 := 1;
  end;

  // OK, at this point we have two time values t0 and t1
  // between which the swept sphere intersects with the
  // triangle plane. If any collision is to occur it must
  // happen within this interval.
  foundCollison := false;
  t := 1;
  // First we check for the easy case - collision inside
  // the triangle. If this happens it must be at time t0
  // as this is when the sphere rests on the front side
  // of the triangle plane. Note, this can only happen if
  // the sphere is not embedded in the triangle plane.
  if (not embeddedInPlane) then
  begin
    planeIntersectionPoint := VectorSubtract(colPackage.basePoint,trianglePlane.Normal);
    V := VectorScale(colPackage.velocity,t0);
    VectorAdd(planeIntersectionPoint , V);
    if checkPointInTriangle(planeIntersectionPoint,p1,p2,p3) then
    begin
     foundCollison := true;
     t := t0;
     collisionPoint := planeIntersectionPoint;
    end;
  end;
  // if we haven’t found a collision already we’ll have to
  // sweep sphere against points and edges of the triangle.
  // Note: A collision inside the triangle (the check above)
  // will always happen before a vertex or edge collision!
  // This is why we can skip the swept test if the above
  // gives a collision!
  if (not foundCollison) then
  begin
    // some commonly used terms:
    velocity := colPackage.velocity;
    base := colPackage.basePoint;
    velocitySquaredLength := Sqr(VectorLength(velocity));
    // For each vertex or edge a quadratic equation have to
    // be solved. We parameterize this equation as
    // a*t^2 + b*t + c = 0 and below we calculate the
    // parameters a,b and c for each test.
    // Check against points:
    a := velocitySquaredLength;
    // P1
    V := VectorSubtract(base,p1);
    b := 2.0*(VectorDotProduct(velocity, V));
    c := Sqr(VectorLength(V)) - 1.0;
    if (getLowestRoot(a,b,c, t, newT)) then
    begin
      t := newT;
      foundCollison := true;
      collisionPoint := p1;
    end;
    // P2
    V := VectorSubtract(base,p2);
    b := 2.0*(VectorDotProduct(velocity, V));
    c := Sqr(VectorLength(V)) - 1.0;
    if (getLowestRoot(a,b,c, t, newT)) then
    begin
      t := newT;
      foundCollison := true;
      collisionPoint := p2;
    end;
    // P3
    V := VectorSubtract(base,p3);
    b := 2.0*(VectorDotProduct(velocity, V));
    c := Sqr(VectorLength(V)) - 1.0;
    if (getLowestRoot(a,b,c, t, newT)) then
    begin
      t := newT;
      foundCollison := true;
      collisionPoint := p3;
    end;

    // Check agains edges:
    // p1 -> p2:
    edge := VectorSubtract(p2,p1);
    baseToVertex := VectorSubtract(p1, base);
    edgeSquaredLength := Sqr(VectorLength(edge));
    edgeDotVelocity := VectorDotProduct(edge,velocity);
    edgeDotBaseToVertex := VectorDotProduct(edge,baseToVertex);
    // Calculate parameters for equation
    a := edgeSquaredLength * -velocitySquaredLength +
         edgeDotVelocity * edgeDotVelocity;
    b := edgeSquaredLength*(2 * VectorDotProduct(velocity,baseToVertex))-
         2.0*edgeDotVelocity*edgeDotBaseToVertex;
    c := edgeSquaredLength*(1- Sqr(VectorLength(baseToVertex)) )+
         edgeDotBaseToVertex*edgeDotBaseToVertex;
    // Does the swept sphere collide against infinite edge?
    if (getLowestRoot(a,b,c, t, newT)) then
    begin
      // Check if intersection is within line segment:
      temp := (edgeDotVelocity*newT-edgeDotBaseToVertex)/ edgeSquaredLength;
      if (temp >= 0) and (temp <= 1) then
      begin
        // intersection took place within segment.
        t := newT;
        foundCollison := true;
        collisionPoint := VectorAdd(p1, VectorScale(edge,temp));
      end;
    end;

    // p1 -> p2:
    edge := VectorSubtract(p3,p2);
    baseToVertex := VectorSubtract(p2, base);
    edgeSquaredLength := Sqr(VectorLength(edge));
    edgeDotVelocity := VectorDotProduct(edge,velocity);
    edgeDotBaseToVertex := VectorDotProduct(edge,baseToVertex);
    // Calculate parameters for equation
    a := edgeSquaredLength * -velocitySquaredLength +
         edgeDotVelocity * edgeDotVelocity;
    b := edgeSquaredLength*(2* VectorDotProduct(velocity,baseToVertex))-
         2.0*edgeDotVelocity*edgeDotBaseToVertex;
    c := edgeSquaredLength*(1- Sqr(VectorLength(baseToVertex)) )+
         edgeDotBaseToVertex*edgeDotBaseToVertex;
    // Does the swept sphere collide against infinite edge?
    if (getLowestRoot(a,b,c, t, newT)) then
    begin
      // Check if intersection is within line segment:
      temp := (edgeDotVelocity*newT-edgeDotBaseToVertex)/ edgeSquaredLength;
      if (temp >= 0) and (temp <= 1) then
      begin
        // intersection took place within segment.
        t := newT;
        foundCollison := true;
        collisionPoint := VectorAdd(p2, VectorScale(edge,temp));
      end;
    end;

    // p3 -> p1:
    edge := VectorSubtract(p1,p3);
    baseToVertex := VectorSubtract(p3, base);
    edgeSquaredLength := Sqr(VectorLength(edge));
    edgeDotVelocity := VectorDotProduct(edge,velocity);
    edgeDotBaseToVertex := VectorDotProduct(edge,baseToVertex);
    // Calculate parameters for equation
    a := edgeSquaredLength * -velocitySquaredLength +
         edgeDotVelocity * edgeDotVelocity;
    b := edgeSquaredLength*(2* VectorDotProduct(velocity,baseToVertex))-
         2.0*edgeDotVelocity*edgeDotBaseToVertex;
    c := edgeSquaredLength*(1- Sqr(VectorLength(baseToVertex)) )+
         edgeDotBaseToVertex*edgeDotBaseToVertex;
    // Does the swept sphere collide against infinite edge?
    if (getLowestRoot(a,b,c, t, newT)) then
    begin
      // Check if intersection is within line segment:
      temp := (edgeDotVelocity*newT-edgeDotBaseToVertex)/ edgeSquaredLength;
      if (temp >= 0) and (temp <= 1) then
      begin
        // intersection took place within segment.
        t := newT;
        foundCollison := true;
        collisionPoint := VectorAdd(p3, VectorScale(edge,temp));
      end;
    end;

  end;
  // Set result:
  if foundCollison then
  begin
    result := True;

    // distance to collision: ’t’ is time of collision
    distToCollision := t * VectorLength(colPackage.velocity);

    //Get the intersection values
    Intersection.Point := collisionPoint;
    Intersection.Normal := trianglePlane.Normal;
    Intersection.Bounce := VectorNormalize(VectorReflect(colPackage.velocity, trianglePlane.Normal));
    Intersection.Distance := distToCollision;

    // Does this triangle qualify for the closest hit?
    // it does if it’s the first hit or the closest
    // and check if it is solid
    if Intersection.Solid and colPackage.Solid
    and ((colPackage.foundCollision = false)
         or (distToCollision < colPackage.nearestDistance)) then
    begin
      // Collision information nessesary for sliding
      colPackage.nearestDistance := distToCollision;
      colPackage.intersectionPoint := collisionPoint;
      colPackage.intersectionNormal := trianglePlane.Normal;
      colPackage.foundCollision := true;
      colPackage.objectIndex := Intersection.ObjectIndex;
      //Intersection.Nearest := True;
    end;
  end;
  trianglePlane.Free;
end;

function CheckPoint(pPos, pNormal: TAffineVector;
                    var colPackage: TECCollisionPacket;
                    var Intersection: TECIntersection): Boolean;
var newPos: TAffineVector;
    Distance: Single;
    FoundCollision: Boolean;
begin
  newPos := VectorAdd(colPackage.basePoint, colPackage.Velocity);

  //*** Need to check if the ellipsoid is embedded ***
  Distance := VectorDistance(pPos,newPos) - 1;
  if Distance < 0 then
    Distance := 0;

  FoundCollision := Distance <= 0.0001; //Very small distance

  Result := FoundCollision;

  // Set result:
  if FoundCollision then
  begin
    //Get the intersection values
    Intersection.Point := pPos;
    Intersection.Normal := pNormal;
    Intersection.Bounce := VectorNormalize(VectorReflect(colPackage.velocity, pNormal));
    //Intersection.Nearest := False;

    // Does this point qualify for the closest hit?
    // it does if it’s the first hit or the closest
    // and check if it is solid
    if Intersection.Solid and colPackage.Solid
    and ((colPackage.foundCollision = false)
         or (Distance < colPackage.nearestDistance)) then
    begin
      // Collision information nessesary for sliding
      colPackage.nearestDistance := Distance;
      colPackage.intersectionPoint := pPos;
      colPackage.intersectionNormal := pNormal;
      colPackage.foundCollision := true;
      colPackage.objectIndex := Intersection.ObjectIndex;
      //Intersection.Nearest := True;
    end;
  end;

end;

function CheckEllipsoid(ePos, eRadius: TAffineVector;
                        var MovePack: TECMovementPacket;
                        var colPackage: TECCollisionPacket;
                        var Intersection: TECIntersection): Boolean;
var newPos, nA, rA, nB, rB, iPoint, iNormal: TAffineVector;
begin
  newPos := VectorAdd(colPackage.basePoint, colPackage.Velocity);

  //Find intersection
  nA := VectorNormalize(VectorDivide(VectorSubtract(ePos,newPos),eRadius));
  rA := VectorAdd(newPos,nA);

  nB := VectorNormalize(VectorDivide(VectorSubtract(rA,ePos),eRadius));
  rB[0] := ePos[0] + (eRadius[0] * nB[0]);
  rB[1] := ePos[1] + (eRadius[1] * nB[1]);
  rB[2] := ePos[2] + (eRadius[2] * nB[2]);

  iPoint := rB;
  iNormal := VectorNormalize(VectorDivide(VectorSubtract(newPos,ePos),eRadius));

  Result := CheckPoint(iPoint,iNormal,colPackage,Intersection);

end;


procedure CheckCollision(var MovePack: TECMovementPacket;
                         var collisionPackage: TECCollisionPacket);
var
   i,j,iindex : Integer;
   p1, p2, p3: TAffineVector;
   Radius: TAffineVector;
   isection : TECIntersection;
   ilist: TECIntersectionList;
begin
  Radius := MovePack.Radius;
  SetLength(ilist,0);
  for i := 0 to High(MovePack.Triangles) do
  begin
    //These are the vertices of the triangle to check
    p1 := VectorDivide( MovePack.Triangles[i].p1, Radius);
    p2 := VectorDivide( MovePack.Triangles[i].p2, Radius);
    p3 := VectorDivide( MovePack.Triangles[i].p3, Radius);

    //Get intersections
    isection.ObjectIndex := MovePack.Triangles[i].ObjectIndex;
    isection.Friction := MovePack.Triangles[i].Friction;
    isection.Solid := MovePack.Triangles[i].Solid;
    if CheckTriangle(collisionPackage,p1,p2,p3,isection) then
    begin

      //Add or update the ObjectIndex with less distance
      iindex := -1;
      for j := 0 to High(ilist) do
        if (ilist[j].ObjectIndex = isection.ObjectIndex) then
          iindex := j;

      if iindex < 0 then
      begin
        iindex := Length(ilist);
        SetLength(ilist,iindex+1);
        ilist[iindex] := isection;
      end else
        if (ilist[iindex].distance > isection.Distance) then
         ilist[iindex] := isection;

    end;

  end;

  //Add to collision list
  for i := 0 to High(ilist) do
  begin
    isection := ilist[i];
    ScaleVector(isection.Point,Radius);
    SetLength(MovePack.CollisionList, Length(MovePack.CollisionList)+1);
    MovePack.CollisionList[Length(MovePack.CollisionList)-1] := isection;
  end;
end;

procedure CheckExternalCollision(var MovePack: TECMovementPacket;
                         var Colliders: array of TECCollider;
                         var collisionPackage: TECCollisionPacket);
var
   i : Integer;
   p1,p2: TAffineVector;
   isection : TECIntersection;
   Collided: Boolean;
begin
  for i := 0 to High(Colliders) do
  begin
    //This the intersection point
    p1 := VectorDivide( Colliders[i].Position, MovePack.Radius);
    p2 := VectorDivide( Colliders[i].Size, MovePack.Radius);
    //Get intersections
    isection.ObjectIndex := Colliders[i].ObjectIndex;
    isection.Friction := Colliders[i].Friction;
    isection.Solid := Colliders[i].Solid;

    Collided := False;
    case Colliders[i].Shape of
      csEllipsoid, csBox:
         Collided := CheckEllipsoid(p1,p2,MovePack,collisionPackage,isection);
      csPoint: Collided := CheckPoint(p1,p2,collisionPackage,isection);
    end;

    if Collided then
    begin
      ScaleVector(isection.Point,MovePack.Radius);
      ScaleVector(isection.Normal,MovePack.Radius);
      ScaleVector(isection.Bounce,MovePack.Radius);
      //Add to collision list
      SetLength(MovePack.CollisionList, Length(MovePack.CollisionList)+1);
      MovePack.CollisionList[Length(MovePack.CollisionList)-1] := isection;
    end;

  end;
end;

procedure CheckTerrainCollision(var MovePack: TECMovementPacket;
                                var collisionPackage: TECCollisionPacket);
var
   i,count,j : Integer;
   x,y: Extended;
   v, closest: TAffineVector;
   p: array [0..7] of TAffineVector;
   ang,rang: Real;
   n,newPos, origin: TAffineVector;
   unitScale, veryCloseDistance: Single;
begin
  //Always remove the older possible colliders
  //Because this procedure is called in the recursive CollideWithWorld procedure
  SetLength(MovePack.TerrainColliders,0);
  //Next position
  newPos := VectorAdd(collisionPackage.basePoint, collisionPackage.Velocity);
  ScaleVector(newPos, MovePack.Radius);

  // All hard-coded distances in this function is
  // scaled to fit the setting above..
  unitScale := unitsPerMeter / 100;
  veryCloseDistance := 0.1 * unitScale;

  for i := 0 to High(MovePack.Terrains) do
  with MovePack.Terrains[i] do begin
    //Origin
    Origin := newPos;
    Origin[1] :=Terrain.AbsolutePosition[1] + Terrain.InterpolatedHeight(newPos);

    closest := Origin;

    //Test 8 angles
    ang := 0;
    for j := 0 to 7 do
    begin
      rang := ang * (3.1415926535897932385 / 180);  //Degree to Rad
      SinCos(rang,x,y);
      v[0] := x;
      v[1] := 0;
      v[2] := y;
      ScaleVector(v,veryCloseDistance);
      ang := ang + 45;
      p[j] := VectorAdd(newPos,v);
      p[j][1] :=Terrain.AbsolutePosition[1] + Terrain.InterpolatedHeight(p[j]);

      //Get the closest to the ellipsoid
      if VectorDistance(p[j], newPos) < VectorDistance(closest, newPos) then
        closest := p[j];
    end;

    CalcPlaneNormal(p[0],p[2],p[6],n);

    //Add as a possible point collider
    count := Length(MovePack.TerrainColliders);
    SetLength(MovePack.TerrainColliders,count+1);
    with MovePack.TerrainColliders[count] do
    begin
      Position := closest;
      Size := n;
      ObjectIndex := MovePack.Terrains[i].Collider.ObjectIndex;
      Friction := MovePack.Terrains[i].Collider.Friction;
      Solid := MovePack.Terrains[i].Collider.Solid;
      Shape := csPoint;
    end;

  end;
end;


procedure CollideAndSlide(var MovePack: TECMovementPacket);
var
  eSpacePosition, eSpaceVelocity: TAffineVector;
  Radius: TAffineVector;
begin
  // Do collision detection:
  SetLength(MovePack.CollisionList, 0);

  Radius := MovePack.Radius;

  MovePack.InGround := False;
  MovePack.FoundCollision := False;

  //CheckTerrainCollision(MovePack);

  // calculate position and velocity in eSpace
  eSpacePosition := VectorDivide(MovePack.Position, Radius);
  eSpaceVelocity := VectorDivide(MovePack.velocity, Radius);

  // Iterate until we have our final position.
  MovePack.collisionRecursionDepth := 0;
  collideWithWorld(MovePack, eSpacePosition,eSpaceVelocity);

  // Add gravity pull:
  // Set the new R3 position (convert back from eSpace to R3
  eSpaceVelocity := VectorDivide(MovePack.Gravity,Radius);
  eSpacePosition := MovePack.ResultPos;
  MovePack.collisionRecursionDepth := 0;
  MovePack.FoundCollision := False;
  collideWithWorld(MovePack, eSpacePosition,eSpaceVelocity);
  MovePack.InGround := MovePack.FoundCollision;
  // Convert final result back to R3:
  ScaleVector(Movepack.ResultPos, Radius);

end;

procedure CollideWithWorld(var MovePack: TECMovementPacket; pos, vel: TAffineVector);
var
  collisionPackage: TECCollisionPacket;
  unitScale, veryCloseDistance: Single;
  destinationPoint, newBasePoint, V : TAffineVector;

  slidePlaneOrigin,slidePlaneNormal : TAffineVector;
  slidingPlane : TECPlane;
  newDestinationPoint, newVelocityVector : TAffineVector;

begin
  // All hard-coded distances in this function is
  // scaled to fit the setting above..
  unitScale := unitsPerMeter / 100;
  veryCloseDistance := 0.005 * unitScale;

  MovePack.ResultPos := pos;
  // do we need to worry?
  if (MovePack.collisionRecursionDepth > 5) then Exit;
  // Ok, we need to worry:

  collisionPackage.foundCollision := false;
  collisionPackage.velocity := vel;
  collisionPackage.normalizedVelocity := vel;
  NormalizeVector(collisionPackage.normalizedVelocity);
  collisionPackage.basePoint := pos;
  collisionPackage.Solid := MovePack.Solid;
  //First get any possible terrain collider
  CheckTerrainCollision(MovePack,collisionPackage);
  //Now check for terrain collision
  CheckExternalCollision(MovePack, MovePack.TerrainColliders ,collisionPackage);
  // Check for external collision (non-triangles, fi. another Ellipsoid)
  CheckExternalCollision(MovePack, MovePack.Colliders ,collisionPackage);
  // Check for collision (calls the collision routines)
  CheckCollision(MovePack,collisionPackage);

  // If no collision we just move along the velocity
  if (not collisionPackage.foundCollision) then
  begin
    MovePack.ResultPos := VectorAdd(pos, vel);
    Exit;
  end;

  // *** Collision occured ***

  with Movepack do
  begin
    //if collisionPackage.nearestDistance < Distance then
    //begin
      ColPoint := collisionPackage.intersectionPoint;
      ColNormal := collisionPackage.intersectionNormal;
      ColBounce := VectorNormalize(VectorReflect(collisionPackage.velocity, ColNormal));
      ColObjectIndex := collisionPackage.objectIndex;
      Distance := collisionPackage.nearestDistance;
      FoundCollision := True;
    //end;
  end;

  // If not solid show the collision but move on
  if (not MovePack.Solid) then
  begin
    MovePack.ResultPos := VectorAdd(pos, vel);
    Exit;
  end;

  // The original destination point
  destinationPoint := VectorAdd(pos, vel);
  newBasePoint := pos;
  // only update if we are not already very close
  // and if so we only move very close to intersection..not
  // to the exact spot.
  if (collisionPackage.nearestDistance >= veryCloseDistance) then
  begin
    V := vel;
    VectorSetLength(V,collisionPackage.nearestDistance - veryCloseDistance);
    newBasePoint := VectorAdd(collisionPackage.BasePoint, V);
    // Adjust polygon intersection point (so sliding
    // plane will be unaffected by the fact that we
    // move slightly less than collision tells us)
    NormalizeVector(V);
    ScaleVector(V,veryCloseDistance);
    SubtractVector(collisionPackage.intersectionPoint, V);
  end;

  // Determine the sliding plane
  slidePlaneOrigin := collisionPackage.intersectionPoint;
  slidePlaneNormal := VectorSubtract( newBasePoint , collisionPackage.intersectionPoint );
  NormalizeVector(slidePlaneNormal);
  slidingPlane := TECPlane.Create;
  slidingPlane.MakePlane(slidePlaneOrigin,slidePlaneNormal);
  V := VectorScale(slidePlaneNormal, slidingPlane.signedDistanceTo(destinationPoint));
  newDestinationPoint := VectorSubtract( destinationPoint , V );
  // Generate the slide vector, which will become our new
  // velocity vector for the next iteration
  newVelocityVector := VectorSubtract( newDestinationPoint , collisionPackage.intersectionPoint);
  // Recurse:
  // dont recurse if the new velocity is very small
  if (VectorLength(newVelocityVector) < veryCloseDistance) then
  begin
    MovePack.ResultPos := newBasePoint;
    slidingPlane.Free;
    Exit;
  end;
  slidingPlane.Free;
  Inc(MovePack.collisionRecursionDepth);
  collideWithWorld(MovePack, newBasePoint,newVelocityVector);
end;

end.
