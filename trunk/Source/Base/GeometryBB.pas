{: GeometryBB<p>

	Calculations and manipulations on Bounding Boxes.<p>

	<b>History : </b><font size=-1><ul>
      <li>07/02/03 - EG - Added IntersectAABBsAbsoluteXY (Dan Bartlett) 
      <li>22/01/03 - EG - IntersectAABBs moved in (Bernd Klaiber)
      <li>04/09/03 - EG - New AABB functions
      <li>17/08/01 - EG - Removed "math" dependency
      <li>09/07/01 - EG - Added AABB types and functions
	   <li>31/03/01 - EG - Original Unit by Jacques Tur
	</ul></font>
}
unit GeometryBB;

interface

uses Geometry;

type

   THmgBoundingBox = array [0..7] of TVector; {JT}

   TAABB = record
      min, max : TAffineVector;
   end;

//------------------------------------------------------------------------------
// Bounding Box functions
//------------------------------------------------------------------------------

{: Adds a BB into another BB.<p>
   The original BB (c1) is extended if necessary to contain c2. }
function AddBB(var c1 : THmgBoundingBox; const c2 : THmgBoundingBox) : THmgBoundingBox;
procedure AddAABB(var aabb : TAABB; const aabb1 : TAABB);

procedure SetBB(var c : THmgBoundingBox; const v : TVector);
procedure SetAABB(var bb : TAABB; const v : TVector);

procedure BBTransform(var c : THmgBoundingBox; const m : TMatrix);
procedure AABBTransform(var bb : TAABB; const m : TMatrix);

function BBMinX(const c : THmgBoundingBox) : Single;
function BBMaxX(const c : THmgBoundingBox) : Single;
function BBMinY(const c : THmgBoundingBox) : Single;
function BBMaxY(const c : THmgBoundingBox) : Single;
function BBMinZ(const c : THmgBoundingBox) : Single;
function BBMaxZ(const c : THmgBoundingBox) : Single;

{: Resize the AABB if necessary to include p. }
procedure AABBInclude(var bb : TAABB; const p : TAffineVector);

{: Extract AABB information from a BB. }
function BBToAABB(const aBB : THmgBoundingBox) : TAABB;
{: Converts an AABB to its canonical BB. }
function AABBToBB(const anAABB : TAABB) : THmgBoundingBox; overload;
{: Transforms an AABB to a BB. }
function AABBToBB(const anAABB : TAABB; const m : TMatrix) : THmgBoundingBox; overload;

{: Determines if two AxisAlignedBoundingBoxes intersect.<p>
   The matrices are the ones that convert one point to the other's AABB system }
function IntersectAABBs(const aabb1, aabb2 : TAABB; const m1To2, m2To1 : TMatrix) : Boolean; overload;
{: Checks whether two Bounding boxes aligned with the world axes collide in the XY plane.<p> }
function IntersectAABBsAbsoluteXY(const aabb1, aabb2 : TAABB) : Boolean;

type
   TPlanIndices = array [0..3] of Integer;
   TPlanBB = array [0..5] of TPlanIndices;
   TDirPlan = array [0..5] of Integer;

const
   cBBFront :  TPlanIndices = (0, 1, 2, 3);
   cBBBack :   TPlanIndices = (4, 5, 6, 7);
   cBBLeft :   TPlanIndices = (0, 4, 7, 3);
   cBBRight :  TPlanIndices = (1, 5, 6, 2);
   cBBTop :    TPlanIndices = (0, 1, 5, 4);
   cBBBottom : TPlanIndices = (2, 3, 7, 6);
   cBBPlans : TPlanBB = ( (0, 1, 2, 3),
                          (4, 5, 6, 7),
                          (0, 4, 7, 3),
                          (1, 5, 6, 2),
                          (0, 1, 5, 4),
                          (2, 3, 7, 6) );
   cDirPlan : TDirPlan = ( 0, 0, 1, 1, 2, 2 );

//--------------------------------------------------------------
//--------------------------------------------------------------
//--------------------------------------------------------------
implementation
//--------------------------------------------------------------
//--------------------------------------------------------------
//--------------------------------------------------------------

//------------------------------------------------------------------------------
//----------------- BB functions -------------------------------------------
//------------------------------------------------------------------------------

// SetPlanBB
//
procedure SetPlanBB(var BB : THmgBoundingBox; const NumPlan : Integer; const Valeur : Double);
var
   i : Integer;
begin
   for i := 0 to 3 do
   begin
       BB[cBBPlans[NumPlan][i]][cDirPlan[NumPlan]] := Valeur;
       BB[cBBPlans[NumPlan][i]][3] := 1;
   end;
end;

// AddBB
//
function AddBB(var c1 : THmgBoundingBox; const c2 : THmgBoundingBox) : THmgBoundingBox;

var
   i, j : Integer;
begin
   for i:=0 to 7 do begin
      for j:=0 to 3 do
          if c1[cBBFront[j]][0]<c2[i][0] then SetPlanBB(c1, 0, c2[i][0]);
      for j:=0 to 3 do
          if c1[cBBBack[j]][0]>c2[i][0] then SetPlanBB(c1, 1, c2[i][0]);
      for j:=0 to 3 do
          if c1[cBBLeft[j]][1]<c2[i][1] then SetPlanBB(c1, 2, c2[i][1]);
      for j:=0 to 3 do
          if c1[cBBRight[j]][1]>c2[i][1] then SetPlanBB(c1, 3, c2[i][1]);
      for j:=0 to 3 do
          if c1[cBBTop[j]][2]<c2[i][2] then SetPlanBB(c1, 4, c2[i][2]);
      for j:=0 to 3 do
          if c1[cBBBottom[j]][2]>c2[i][2] then SetPlanBB(c1, 5, c2[i][2]);
   end;
   Result:=c1;
end;

// AddAABB
//
procedure AddAABB(var aabb : TAABB; const aabb1 : TAABB);
begin
   if aabb1.min[0]<aabb.min[0] then aabb.min[0]:=aabb1.min[0];
   if aabb1.min[1]<aabb.min[1] then aabb.min[1]:=aabb1.min[1];
   if aabb1.min[2]<aabb.min[2] then aabb.min[2]:=aabb1.min[2];
   if aabb1.max[0]>aabb.max[0] then aabb.max[0]:=aabb1.max[0];
   if aabb1.max[1]>aabb.max[1] then aabb.max[1]:=aabb1.max[1];
   if aabb1.max[2]>aabb.max[2] then aabb.max[2]:=aabb1.max[2];
end;

// SetBB
//
procedure SetBB( var c : THmgBoundingBox; const v : TVector );
begin
   SetPlanBB( c, 0, v[0] );
   SetPlanBB( c, 1, -v[0] );
   SetPlanBB( c, 2, v[1] );
   SetPlanBB( c, 3, -v[1] );
   SetPlanBB( c, 4, v[2] );
   SetPlanBB( c, 5, -v[2] );
end;

// SetAABB
//
procedure SetAABB(var bb : TAABB; const v : TVector);
begin
   bb.max[0]:=Abs(v[0]);
   bb.max[1]:=Abs(v[1]);
   bb.max[2]:=Abs(v[2]);
   bb.min[0]:=-bb.max[0];
   bb.min[1]:=-bb.max[1];
   bb.min[2]:=-bb.max[2];
end;

// BBTransform
//
procedure BBTransform( var c : THmgBoundingBox; const m : TMatrix );
var
   i : Integer;
begin
   for i:=0 to 7 do
      c[i]:=VectorTransform(c[i], m);
end;

// AABBTransform
//
procedure AABBTransform(var bb : TAABB; const m : TMatrix);
var
   oldMin, oldMax : TAffineVector;
begin
   oldMin:=bb.min;
   oldMax:=bb.max;
   bb.min:=VectorTransform(oldMin , m);
   bb.max:=bb.min;
   AABBInclude(bb, VectorTransform(AffineVectorMake(oldMin[0], oldMin[1], oldMax[2]), m));
   AABBInclude(bb, VectorTransform(AffineVectorMake(oldMin[0], oldMax[1], oldMin[2]), m));
   AABBInclude(bb, VectorTransform(AffineVectorMake(oldMin[0], oldMax[1], oldMax[2]), m));
   AABBInclude(bb, VectorTransform(AffineVectorMake(oldMax[0], oldMin[1], oldMin[2]), m));
   AABBInclude(bb, VectorTransform(AffineVectorMake(oldMax[0], oldMin[1], oldMax[2]), m));
   AABBInclude(bb, VectorTransform(AffineVectorMake(oldMax[0], oldMax[1], oldMin[2]), m));
   AABBInclude(bb, VectorTransform(oldMax , m));
end;

//BBMinX
//
function BBMinX(const c : THmgBoundingBox ) : Single;
var
   i : Integer;
begin
   Result:=c[0][0];
   for i:=1 to 7 do
      Result:=MinFloat(Result, c[i][0]);
end;

//BBMaxX
//
function BBMaxX(const c : THmgBoundingBox ) : Single;
var
   i : Integer;
begin
   result := c[0][0];
   for i := 1 to 7 do
      result := MaxFloat( Result, c[i][0] );
end;

//BBMinY
//
function BBMinY(const c : THmgBoundingBox ) : Single;
var
   i : Integer;
begin
   result := c[0][1];
   for i := 1 to 7 do
      Result := MinFloat( Result, c[i][1] );
end;

//BBMaxY
//
function BBMaxY(const c : THmgBoundingBox ) : Single;
var
   i : Integer;
begin
   Result := c[0][1];
   for i := 1 to 7 do
      Result := MaxFloat( Result, c[i][1] );
end;

//BBMinZ
//
function BBMinZ(const c : THmgBoundingBox ) : Single;
var
   i : Integer;
begin
   Result := c[0][2];
   for i := 1 to 7 do
      Result := MinFloat( Result, c[i][2] );
end;

// BBMaxZ
//
function BBMaxZ(const c : THmgBoundingBox ) : Single;
var
   i : Integer;
begin
   Result := c[0][2];
   for i := 1 to 7 do
      Result := MaxFloat( Result, c[i][2] );
end;

// AABBInclude
//
procedure AABBInclude(var bb : TAABB; const p : TAffineVector);
begin
   if p[0]<bb.min[0] then bb.min[0]:=p[0];
   if p[0]>bb.max[0] then bb.max[0]:=p[0];
   if p[1]<bb.min[1] then bb.min[1]:=p[1];
   if p[1]>bb.max[1] then bb.max[1]:=p[1];
   if p[2]<bb.min[2] then bb.min[2]:=p[2];
   if p[2]>bb.max[2] then bb.max[2]:=p[2];
end;

// BBToAABB
//
function BBToAABB(const aBB : THmgBoundingBox) : TAABB;
var
   i : Integer;
begin
   SetVector(Result.min, aBB[0]);
   SetVector(Result.max, aBB[0]);
   for i:=1 to 7 do begin
      if aBB[i][0]<Result.min[0] then
         Result.min[0]:=aBB[i][0];
      if aBB[i][0]>Result.max[0] then
         Result.max[0]:=aBB[i][0];
      if aBB[i][1]<Result.min[1] then
         Result.min[1]:=aBB[i][1];
      if aBB[i][1]>Result.max[1] then
         Result.max[1]:=aBB[i][1];
      if aBB[i][2]<Result.min[2] then
         Result.min[2]:=aBB[i][2];
      if aBB[i][2]>Result.max[2] then
         Result.max[2]:=aBB[i][2];
   end;
end;

// AABBToBB
//
function AABBToBB(const anAABB : TAABB) : THmgBoundingBox;
begin
   with anAABB do begin
      SetPlanBB( Result, 0, max[0] );
      SetPlanBB( Result, 1, min[0] );
      SetPlanBB( Result, 2, max[1] );
      SetPlanBB( Result, 3, min[1] );
      SetPlanBB( Result, 4, max[2] );
      SetPlanBB( Result, 5, min[2] );
   end;
end;

// AABBToBB
//
function AABBToBB(const anAABB : TAABB; const m : TMatrix) : THmgBoundingBox;
begin
   Result:=AABBToBB(anAABB);
   BBTransform(Result, m);
end;

// IntersectCubes (AABBs)
//
function IntersectAABBs(const aabb1, aabb2 : TAABB;
                        const m1To2, m2To1 : TMatrix) : boolean;
const
  cWires : array[0..11,0..1] of Integer //Points of the wire
         = ((0,1),(1,2),(2,3),(3,0),
            (4,5),(5,6),(6,7),(7,4),
            (0,4),(1,5),(2,6),(3,7));
  cPlanes : array[0..5,0..3] of Integer //points of the planes
         = ((1,2,6,5), (2,3,7,6), (0,1,2,3), (0,3,7,4), (0,1,5,4), (5,6,7,4));

   procedure MakeAABBPoints(const AABB : TAABB; var pt : array of TVertex);
   begin
      with AABB do begin
         SetVector(pt[0], min[0], min[1], min[2]);
         SetVector(pt[1], max[0], min[1], min[2]);
         SetVector(pt[2], max[0], max[1], min[2]);
         SetVector(pt[3], min[0], max[1], min[2]);
         SetVector(pt[4], min[0], min[1], max[2]);
         SetVector(pt[5], max[0], min[1], max[2]);
         SetVector(pt[6], max[0], max[1], max[2]);
         SetVector(pt[7], min[0], max[1], max[2]);
      end;
   end;

   procedure MakePlanes(const pt : array of TVertex; var planes : array of THmgPlane);
   var
      i : Integer;
   begin
      for i:=0 to 5 do
         planes[i]:=PlaneMake(pt[cPlanes[i, 0]], pt[cPlanes[i, 1]], pt[cPlanes[i, 2]]);
   end;

var
  pt1, pt2: array[0..7] of TVertex;
  pt:TVertex;
  Planes2: array[0..5] of THmgPlane;
  i, t: integer;
  V: TVertex;
  P: TVector;
begin
  result:= false;

  //Build Points
  MakeAABBPoints(AABB1, pt1);
  MakeAABBPoints(AABB2, pt2);
  for i:=0 to 7 do
  begin
    pt:= VectorTransform(pt2[i], m2To1);
    //check for inclusion (points of Obj2 in Obj1)
    if IsInRange(pt[0], AABB1.Min[0], AABB1.Max[0]) and
      IsInRange(pt[1], AABB1.Min[1], AABB1.Max[1]) and
      IsInRange(pt[2], AABB1.Min[2], AABB1.Max[2]) then
    begin
      result:= true;
      exit;
    end;
  end;

  for i:=0 to 7 do
  begin
    pt1[i]:= VectorTransform(pt1[i], m1To2);
    //check for inclusion (points of Obj1 in Obj2)
    if IsInRange(pt1[i][0], AABB2.Min[0], AABB2.Max[0]) and
      IsInRange(pt1[i][1], AABB2.Min[1], AABB2.Max[1]) and
      IsInRange(pt1[i][2], AABB2.Min[2], AABB2.Max[2]) then
    begin
      result:= true;
      exit;
    end;
  end;

  //Build Planes
  MakePlanes(pt2, Planes2);

  //Wire test
  for i:=0 to 11 do
  begin
    for t:=0 to 5 do
    begin
      //Build Vector of Ray
      V:= VectorSubtract(pt1[cWires[i,0]], pt1[cWires[i,1]]);
      if IntersectLinePlane(VectorMake(pt1[cWires[i,0]]), VectorMake(V), Planes2[t], @P) = 1 then
      begin
        //check point in Wire
        if IsInRange(P[0], pt1[cWires[i,0]][0], pt1[cWires[i,1]][0]) and
          IsInRange(P[1], pt1[cWires[i,0]][1], pt1[cWires[i,1]][1]) and
          IsInRange(P[2], pt1[cWires[i,0]][2], pt1[cWires[i,1]][2]) then
        begin
          //check point in Plane
          if IsInRange(P[0], pt2[cPlanes[t, 0]][0], pt2[cPlanes[t, 2]][0]) and
            IsInRange(P[1], pt2[cPlanes[t, 0]][1], pt2[cPlanes[t, 2]][1]) and
            IsInRange(P[2], pt2[cPlanes[t, 0]][2], pt2[cPlanes[t, 2]][2]) then
          begin
            result:= true;
            exit;
          end;
        end;
      end;
    end;
  end;
end;

// IntersectAABBsAbsoluteXY (AABBs)
//
function IntersectAABBsAbsoluteXY(const aabb1, aabb2 : TAABB) : Boolean;
begin
  result:= false;

  if (AABB2.min[0]>AABB1.max[0])or (AABB2.min[1]>AABB1.max[1]) then Exit
  else if (AABB2.max[0]<AABB1.min[0])or (AABB2.max[1]<AABB1.min[1]) then Exit
  else Result:=true;

end;

end.
