//
// This unit is part of the GLScene Project, http://glscene.org
//
{: GeometryBB<p>

	Calculations and manipulations on Bounding Boxes.<p>

	<b>History : </b><font size=-1><ul>
      <li>02/08/04 - LR, YHC - BCB corrections: use record instead array
                               changed type of THmgBoundingBox to record
      <li>22/06/03 - MF - Added TBSphere for bounding spheres and classes to
                          determine whether one aabb/bsphere contains another
                          aabb/bsphere
      <li>21/06/03 - MF - Added IntersectAABBsAbsolute
      <li>08/05/03 - DanB - Added Plane/Triangle-AABB collisions (Matheus Degiovani)
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

{$i GLScene.inc}

uses VectorGeometry;

type

   {: Structure for storing Bounding Boxes }
   THmgBoundingBox = record
      BBox : array [0..7] of TVector; {JT}
   end;

   {: Structure for storing Axis Aligned Bounding Boxes }
   TAABB = record
      min, max : TAffineVector;
   end;
   PAABB = ^TAABB;

   // TBSphere
   //
   {: Structure for storing BoundingSpheres. Similar to TAABB}
   TBSphere = record
      {: Center of Bounding Sphere }
      Center : TAffineVector;
      {: Radius of Bounding Sphere }
      Radius : single;
   end;

   // TClipRect
   //
   TClipRect = record
      Left, Top : Single;
      Right, Bottom : Single;
   end;

   {: Result type for space intersection tests, like AABBContainsAABB or
   BSphereContainsAABB }
   TSpaceContains = (scNoOverlap, scContainsFully, scContainsPartially);
   {: Structure for storing the corners of an AABB, used with ExtractAABBCorners}
   TAABBCorners = array[0..7] of TAffineVector;

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
procedure AABBScale(var bb : TAABB; const v : TAffineVector);

function BBMinX(const c : THmgBoundingBox) : Single;
function BBMaxX(const c : THmgBoundingBox) : Single;
function BBMinY(const c : THmgBoundingBox) : Single;
function BBMaxY(const c : THmgBoundingBox) : Single;
function BBMinZ(const c : THmgBoundingBox) : Single;
function BBMaxZ(const c : THmgBoundingBox) : Single;

{: Resize the AABB if necessary to include p. }
procedure AABBInclude(var bb : TAABB; const p : TAffineVector);
{: Make an AABB that is formed by sweeping a sphere (or AABB) from Start to Dest}
procedure AABBFromSweep(var SweepAABB:TAABB; const Start,Dest:TVector; const Radius:Single);
{: Returns the intersection AABB of two AABBs.<p>
   If the AABBs don't intersect, will return a degenerated AABB (plane, line or point). }
function AABBIntersection(const aabb1, aabb2 : TAABB) : TAABB;

{: Extract AABB information from a BB. }
function BBToAABB(const aBB : THmgBoundingBox) : TAABB;
{: Converts an AABB to its canonical BB. }
function AABBToBB(const anAABB : TAABB) : THmgBoundingBox; overload;
{: Transforms an AABB to a BB. }
function AABBToBB(const anAABB : TAABB; const m : TMatrix) : THmgBoundingBox; overload;

{: Adds delta to min and max of the AABB. }
procedure OffsetAABB(var aabb : TAABB; const delta : TAffineVector); overload;
procedure OffsetAABB(var aabb : TAABB; const delta : TVector); overload;

{: Determines if two AxisAlignedBoundingBoxes intersect.<p>
   The matrices are the ones that convert one point to the other's AABB system }
function IntersectAABBs(const aabb1, aabb2 : TAABB; const m1To2, m2To1 : TMatrix) : Boolean; overload;
{: Checks whether two Bounding boxes aligned with the world axes collide in the XY plane.<p> }
function IntersectAABBsAbsoluteXY(const aabb1, aabb2 : TAABB) : Boolean;
{: Checks whether two Bounding boxes aligned with the world axes collide in the XZ plane.<p> }
function IntersectAABBsAbsoluteXZ(const aabb1, aabb2 : TAABB) : Boolean;
{: Checks whether two Bounding boxes aligned with the world axes collide.<p> }
function IntersectAABBsAbsolute(const aabb1, aabb2 : TAABB) : Boolean;
{: Checks whether one Bounding box aligned with the world axes fits within
another Bounding box.<p> }
function AABBFitsInAABBAbsolute(const aabb1, aabb2 : TAABB) : Boolean;

{: Checks if a point "p" is inside an AABB}
function PointInAABB(const p : TAffineVector; const aabb : TAABB) : Boolean; overload;
function PointInAABB(const p : TVector; const aabb : TAABB) : Boolean; overload;

{: Checks if a plane (given by the normal+d) intersects the AABB}
function PlaneIntersectAABB(Normal: TAffineVector; d: single; aabb: TAABB): boolean;
{: Checks if a triangle (given by vertices v1, v2 and v3) intersects an AABB}
function TriangleIntersectAABB(const aabb: TAABB; v1, v2, v3: TAffineVector): boolean;

{: Extract the corners from an AABB}
procedure ExtractAABBCorners(const AABB: TAABB; var AABBCorners : TAABBCorners);

{: Convert an AABB to a BSphere}
procedure AABBToBSphere(const AABB : TAABB; var BSphere : TBSphere);
{: Convert a BSphere to an AABB }
procedure BSphereToAABB(const BSphere : TBSphere; var AABB : TAABB); overload;
function BSphereToAABB(const center : TAffineVector; radius : Single) : TAABB; overload;
function BSphereToAABB(const center : TVector; radius : Single) : TAABB; overload;

{: Determines to which extent one AABB contains another AABB}
function AABBContainsAABB(const mainAABB, testAABB : TAABB) : TSpaceContains;
{: Determines to which extent a BSphere contains an AABB}
function BSphereContainsAABB(const mainBSphere : TBSphere; const testAABB : TAABB) : TSpaceContains;
{: Determines to which extent one BSphere contains another BSphere}
function BSphereContainsBSphere(const mainBSphere, testBSphere : TBSphere) : TSpaceContains;
{: Determines to which extent an AABB contains a BSpher}
function AABBContainsBSphere(const mainAABB : TAABB; const testBSphere : TBSphere) : TSpaceContains;
{: Determines to which extent a plane contains a BSphere}
function PlaneContainsBSphere(const Location, Normal : TAffineVector; const testBSphere : TBSphere) : TSpaceContains;
{: Determines to which extent a frustum contains a BSphere}
function FrustumContainsBSphere(const Frustum : TFrustum; const testBSphere : TBSphere) : TSpaceContains;
{: Determines to which extent a frustum contains an AABB}
function FrustumContainsAABB(const Frustum : TFrustum; const testAABB : TAABB) : TSpaceContains;
{: Clips a position to an AABB }
function ClipToAABB(const v : TAffineVector; const AABB : TAABB) : TAffineVector;
{: Determines if one BSphere intersects another BSphere}
function BSphereIntersectsBSphere(const mainBSphere, testBSphere : TBSphere) : boolean;

{: Extend the clip rect to include given coordinate. }
procedure IncludeInClipRect(var clipRect : TClipRect; x, y : Single);
{: Projects an AABB and determines the extent of its projection as a clip rect. }
function AABBToClipRect(const aabb : TAABB; modelViewProjection : TMatrix;
                        viewportSizeX, viewportSizeY : Integer) : TClipRect;

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
       BB.BBox[cBBPlans[NumPlan][i]].Coord[cDirPlan[NumPlan]] := Valeur;
       BB.BBox[cBBPlans[NumPlan][i]].Coord[3] := 1;
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
          if c1.BBox[cBBFront[j]].Coord[0]<c2.BBox[i].Coord[0] then SetPlanBB(c1, 0, c2.BBox[i].Coord[0]);
      for j:=0 to 3 do
          if c1.BBox[cBBBack[j]].Coord[0]>c2.BBox[i].Coord[0] then SetPlanBB(c1, 1, c2.BBox[i].Coord[0]);
      for j:=0 to 3 do
          if c1.BBox[cBBLeft[j]].Coord[1]<c2.BBox[i].Coord[1] then SetPlanBB(c1, 2, c2.BBox[i].Coord[1]);
      for j:=0 to 3 do
          if c1.BBox[cBBRight[j]].Coord[1]>c2.BBox[i].Coord[1] then SetPlanBB(c1, 3, c2.BBox[i].Coord[1]);
      for j:=0 to 3 do
          if c1.BBox[cBBTop[j]].Coord[2]<c2.BBox[i].Coord[2] then SetPlanBB(c1, 4, c2.BBox[i].Coord[2]);
      for j:=0 to 3 do
          if c1.BBox[cBBBottom[j]].Coord[2]>c2.BBox[i].Coord[2] then SetPlanBB(c1, 5, c2.BBox[i].Coord[2]);
   end;
   Result:=c1;
end;

// AddAABB
//
procedure AddAABB(var aabb : TAABB; const aabb1 : TAABB);
begin
   if aabb1.min.Coord[0]<aabb.min.Coord[0] then aabb.min.Coord[0]:=aabb1.min.Coord[0];
   if aabb1.min.Coord[1]<aabb.min.Coord[1] then aabb.min.Coord[1]:=aabb1.min.Coord[1];
   if aabb1.min.Coord[2]<aabb.min.Coord[2] then aabb.min.Coord[2]:=aabb1.min.Coord[2];
   if aabb1.max.Coord[0]>aabb.max.Coord[0] then aabb.max.Coord[0]:=aabb1.max.Coord[0];
   if aabb1.max.Coord[1]>aabb.max.Coord[1] then aabb.max.Coord[1]:=aabb1.max.Coord[1];
   if aabb1.max.Coord[2]>aabb.max.Coord[2] then aabb.max.Coord[2]:=aabb1.max.Coord[2];
end;

// SetBB
//
procedure SetBB( var c : THmgBoundingBox; const v : TVector );
begin
   SetPlanBB( c, 0, v.Coord[0] );
   SetPlanBB( c, 1, -v.Coord[0] );
   SetPlanBB( c, 2, v.Coord[1] );
   SetPlanBB( c, 3, -v.Coord[1] );
   SetPlanBB( c, 4, v.Coord[2] );
   SetPlanBB( c, 5, -v.Coord[2] );
end;

// SetAABB
//
procedure SetAABB(var bb : TAABB; const v : TVector);
begin
   bb.max.Coord[0]:=Abs(v.Coord[0]);
   bb.max.Coord[1]:=Abs(v.Coord[1]);
   bb.max.Coord[2]:=Abs(v.Coord[2]);
   bb.min.Coord[0]:=-bb.max.Coord[0];
   bb.min.Coord[1]:=-bb.max.Coord[1];
   bb.min.Coord[2]:=-bb.max.Coord[2];
end;

// BBTransform
//
procedure BBTransform( var c : THmgBoundingBox; const m : TMatrix );
var
   i : Integer;
begin
   for i:=0 to 7 do
      c.BBox[i]:=VectorTransform(c.BBox[i], m);
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
   AABBInclude(bb, VectorTransform(AffineVectorMake(oldMin.Coord[0], oldMin.Coord[1], oldMax.Coord[2]), m));
   AABBInclude(bb, VectorTransform(AffineVectorMake(oldMin.Coord[0], oldMax.Coord[1], oldMin.Coord[2]), m));
   AABBInclude(bb, VectorTransform(AffineVectorMake(oldMin.Coord[0], oldMax.Coord[1], oldMax.Coord[2]), m));
   AABBInclude(bb, VectorTransform(AffineVectorMake(oldMax.Coord[0], oldMin.Coord[1], oldMin.Coord[2]), m));
   AABBInclude(bb, VectorTransform(AffineVectorMake(oldMax.Coord[0], oldMin.Coord[1], oldMax.Coord[2]), m));
   AABBInclude(bb, VectorTransform(AffineVectorMake(oldMax.Coord[0], oldMax.Coord[1], oldMin.Coord[2]), m));
   AABBInclude(bb, VectorTransform(oldMax , m));
end;

//AABBScale
//
procedure AABBScale(var bb : TAABB; const v : TAffineVector);
begin
  ScaleVector(bb.min,v);
  ScaleVector(bb.max,v);
end;

//BBMinX
//
function BBMinX(const c : THmgBoundingBox ) : Single;
var
   i : Integer;
begin
   Result:=c.BBox[0].Coord[0];
   for i:=1 to 7 do
      Result:=MinFloat(Result, c.BBox[i].Coord[0]);
end;

//BBMaxX
//
function BBMaxX(const c : THmgBoundingBox ) : Single;
var
   i : Integer;
begin
   result := c.BBox[0].Coord[0];
   for i := 1 to 7 do
      result := MaxFloat( Result, c.BBox[i].Coord[0] );
end;

//BBMinY
//
function BBMinY(const c : THmgBoundingBox ) : Single;
var
   i : Integer;
begin
   result := c.BBox[0].Coord[1];
   for i := 1 to 7 do
      Result := MinFloat( Result, c.BBox[i].Coord[1] );
end;

//BBMaxY
//
function BBMaxY(const c : THmgBoundingBox ) : Single;
var
   i : Integer;
begin
   Result := c.BBox[0].Coord[1];
   for i := 1 to 7 do
      Result := MaxFloat( Result, c.BBox[i].Coord[1] );
end;

//BBMinZ
//
function BBMinZ(const c : THmgBoundingBox ) : Single;
var
   i : Integer;
begin
   Result := c.BBox[0].Coord[2];
   for i := 1 to 7 do
      Result := MinFloat( Result, c.BBox[i].Coord[2] );
end;

// BBMaxZ
//
function BBMaxZ(const c : THmgBoundingBox ) : Single;
var
   i : Integer;
begin
   Result := c.BBox[0].Coord[2];
   for i := 1 to 7 do
      Result := MaxFloat( Result, c.BBox[i].Coord[2] );
end;

// AABBInclude
//
procedure AABBInclude(var bb : TAABB; const p : TAffineVector);
begin
   if p.Coord[0]<bb.min.Coord[0] then bb.min.Coord[0]:=p.Coord[0];
   if p.Coord[0]>bb.max.Coord[0] then bb.max.Coord[0]:=p.Coord[0];
   if p.Coord[1]<bb.min.Coord[1] then bb.min.Coord[1]:=p.Coord[1];
   if p.Coord[1]>bb.max.Coord[1] then bb.max.Coord[1]:=p.Coord[1];
   if p.Coord[2]<bb.min.Coord[2] then bb.min.Coord[2]:=p.Coord[2];
   if p.Coord[2]>bb.max.Coord[2] then bb.max.Coord[2]:=p.Coord[2];
end;

// AABBFromSweep
//
procedure AABBFromSweep(var SweepAABB:TAABB; const Start,Dest:TVector; const Radius:Single);
begin
   if Start.Coord[0]<Dest.Coord[0] then
   begin
     SweepAABB.min.Coord[0]:=Start.Coord[0]-radius;
     SweepAABB.max.Coord[0]:=Dest.Coord[0]+radius;
   end
   else
   begin
     SweepAABB.min.Coord[0]:=Dest.Coord[0]-radius;
     SweepAABB.max.Coord[0]:=Start.Coord[0]+radius;
   end;

   if Start.Coord[1]<Dest.Coord[1] then
   begin
     SweepAABB.min.Coord[1]:=Start.Coord[1]-radius;
     SweepAABB.max.Coord[1]:=Dest.Coord[1]+radius;
   end
   else
   begin
     SweepAABB.min.Coord[1]:=Dest.Coord[1]-radius;
     SweepAABB.max.Coord[1]:=Start.Coord[1]+radius;
   end;

   if Start.Coord[2]<Dest.Coord[2] then
   begin
     SweepAABB.min.Coord[2]:=Start.Coord[2]-radius;
     SweepAABB.max.Coord[2]:=Dest.Coord[2]+radius;
   end
   else
   begin
     SweepAABB.min.Coord[2]:=Dest.Coord[2]-radius;
     SweepAABB.max.Coord[2]:=Start.Coord[2]+radius;
   end;
end;

// AABBIntersection
//
function AABBIntersection(const aabb1, aabb2 : TAABB) : TAABB;
var
   i : Integer;
begin
   for i:=0 to 2 do begin
      Result.min.Coord[i]:=MaxFloat(aabb1.min.Coord[i], aabb2.min.Coord[i]);
      Result.max.Coord[i]:=MinFloat(aabb1.max.Coord[i], aabb2.max.Coord[i]);
   end;
end;

// BBToAABB
//
function BBToAABB(const aBB : THmgBoundingBox) : TAABB;
var
   i : Integer;
begin
   SetVector(Result.min, aBB.BBox[0]);
   SetVector(Result.max, aBB.BBox[0]);
   for i:=1 to 7 do begin
      if aBB.BBox[i].Coord[0]<Result.min.Coord[0] then
         Result.min.Coord[0]:=aBB.BBox[i].Coord[0];
      if aBB.BBox[i].Coord[0]>Result.max.Coord[0] then
         Result.max.Coord[0]:=aBB.BBox[i].Coord[0];
      if aBB.BBox[i].Coord[1]<Result.min.Coord[1] then
         Result.min.Coord[1]:=aBB.BBox[i].Coord[1];
      if aBB.BBox[i].Coord[1]>Result.max.Coord[1] then
         Result.max.Coord[1]:=aBB.BBox[i].Coord[1];
      if aBB.BBox[i].Coord[2]<Result.min.Coord[2] then
         Result.min.Coord[2]:=aBB.BBox[i].Coord[2];
      if aBB.BBox[i].Coord[2]>Result.max.Coord[2] then
         Result.max.Coord[2]:=aBB.BBox[i].Coord[2];
   end;
end;

// AABBToBB
//
function AABBToBB(const anAABB : TAABB) : THmgBoundingBox;
begin
   with anAABB do begin
      SetPlanBB( Result, 0, max.Coord[0] );
      SetPlanBB( Result, 1, min.Coord[0] );
      SetPlanBB( Result, 2, max.Coord[1] );
      SetPlanBB( Result, 3, min.Coord[1] );
      SetPlanBB( Result, 4, max.Coord[2] );
      SetPlanBB( Result, 5, min.Coord[2] );
   end;
end;

// AABBToBB
//
function AABBToBB(const anAABB : TAABB; const m : TMatrix) : THmgBoundingBox;
begin
   Result:=AABBToBB(anAABB);
   BBTransform(Result, m);
end;

// OffsetAABB
//
procedure OffsetAABB(var aabb : TAABB; const delta : TAffineVector);
begin
   AddVector(aabb.min, delta);
   AddVector(aabb.max, delta);
end;

// OffsetAABB
//
procedure OffsetAABB(var aabb : TAABB; const delta : TVector);
begin
   AddVector(aabb.min, delta);
   AddVector(aabb.max, delta);
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
         SetVector(pt[0], min.Coord[0], min.Coord[1], min.Coord[2]);
         SetVector(pt[1], max.Coord[0], min.Coord[1], min.Coord[2]);
         SetVector(pt[2], max.Coord[0], max.Coord[1], min.Coord[2]);
         SetVector(pt[3], min.Coord[0], max.Coord[1], min.Coord[2]);
         SetVector(pt[4], min.Coord[0], min.Coord[1], max.Coord[2]);
         SetVector(pt[5], max.Coord[0], min.Coord[1], max.Coord[2]);
         SetVector(pt[6], max.Coord[0], max.Coord[1], max.Coord[2]);
         SetVector(pt[7], min.Coord[0], max.Coord[1], max.Coord[2]);
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
    if IsInRange(pt.Coord[0], AABB1.Min.Coord[0], AABB1.Max.Coord[0]) and
      IsInRange(pt.Coord[1], AABB1.Min.Coord[1], AABB1.Max.Coord[1]) and
      IsInRange(pt.Coord[2], AABB1.Min.Coord[2], AABB1.Max.Coord[2]) then
    begin
      result:= true;
      exit;
    end;
  end;

  for i:=0 to 7 do
  begin
    pt1[i]:= VectorTransform(pt1[i], m1To2);
    //check for inclusion (points of Obj1 in Obj2)
    if IsInRange(pt1[i].Coord[0], AABB2.Min.Coord[0], AABB2.Max.Coord[0]) and
      IsInRange(pt1[i].Coord[1], AABB2.Min.Coord[1], AABB2.Max.Coord[1]) and
      IsInRange(pt1[i].Coord[2], AABB2.Min.Coord[2], AABB2.Max.Coord[2]) then
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
        if IsInRange(P.Coord[0], pt1[cWires[i,0]].Coord[0], pt1[cWires[i,1]].Coord[0]) and
          IsInRange(P.Coord[1], pt1[cWires[i,0]].Coord[1], pt1[cWires[i,1]].Coord[1]) and
          IsInRange(P.Coord[2], pt1[cWires[i,0]].Coord[2], pt1[cWires[i,1]].Coord[2]) then
        begin
          //check point in Plane
          if IsInRange(P.Coord[0], pt2[cPlanes[t, 0]].Coord[0], pt2[cPlanes[t, 2]].Coord[0]) and
            IsInRange(P.Coord[1], pt2[cPlanes[t, 0]].Coord[1], pt2[cPlanes[t, 2]].Coord[1]) and
            IsInRange(P.Coord[2], pt2[cPlanes[t, 0]].Coord[2], pt2[cPlanes[t, 2]].Coord[2]) then
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

  if (AABB2.min.Coord[0]>AABB1.max.Coord[0])or (AABB2.min.Coord[1]>AABB1.max.Coord[1]) then Exit
  else if (AABB2.max.Coord[0]<AABB1.min.Coord[0])or (AABB2.max.Coord[1]<AABB1.min.Coord[1]) then Exit
  else Result:=true;

end;

function IntersectAABBsAbsoluteXZ(const aabb1, aabb2 : TAABB) : Boolean;
begin
  result := 
   ((AABB1.min.Coord[0]<AABB2.max.Coord[0]) and
    (AABB1.min.Coord[2]<AABB2.max.Coord[2]) and

    (AABB2.min.Coord[0]<AABB1.max.Coord[0]) and
    (AABB2.min.Coord[2]<AABB1.max.Coord[2]));
end;

// IntersectAABBsAbsolute
//
function IntersectAABBsAbsolute(const aabb1, aabb2 : TAABB) : Boolean;
begin
  result := not
   ((AABB1.min.Coord[0]>AABB2.max.Coord[0]) or
    (AABB1.min.Coord[1]>AABB2.max.Coord[1]) or
    (AABB1.min.Coord[2]>AABB2.max.Coord[2]) or

    (AABB2.min.Coord[0]>AABB1.max.Coord[0]) or
    (AABB2.min.Coord[1]>AABB1.max.Coord[1]) or
    (AABB2.min.Coord[2]>AABB1.max.Coord[2]));
end;

// IntersectAABBsAbsolute
//
function AABBFitsInAABBAbsolute(const aabb1, aabb2 : TAABB) : Boolean;
begin
  // AABB1 fits completely inside AABB2?
  // AABB1 min must be >= to AABB2 min
  // AABB1 max must be <= to AABB2 max

  result :=
    (AABB1.min.Coord[0]>=AABB2.min.Coord[0]) and
    (AABB1.min.Coord[1]>=AABB2.min.Coord[1]) and
    (AABB1.min.Coord[2]>=AABB2.min.Coord[2]) and

    (AABB1.max.Coord[0]<=AABB2.max.Coord[0]) and
    (AABB1.max.Coord[1]<=AABB2.max.Coord[1]) and
    (AABB1.max.Coord[2]<=AABB2.max.Coord[2]);
end;

// PointInAABB (affine)
//
function PointInAABB(const p : TAffineVector; const aabb : TAABB) : Boolean;
begin
   Result:=    (p.Coord[0]<=aabb.max.Coord[0]) and (p.Coord[0]>=aabb.min.Coord[0])
           and (p.Coord[1]<=aabb.max.Coord[1]) and (p.Coord[1]>=aabb.min.Coord[1])
           and (p.Coord[2]<=aabb.max.Coord[2]) and (p.Coord[2]>=aabb.min.Coord[2]);
end;

// PointInAABB (hmg)
//
function PointInAABB(const p : TVector; const aabb : TAABB) : Boolean;
begin
   Result:=    (p.Coord[0]<=aabb.max.Coord[0]) and (p.Coord[0]>=aabb.min.Coord[0])
           and (p.Coord[1]<=aabb.max.Coord[1]) and (p.Coord[1]>=aabb.min.Coord[1])
           and (p.Coord[2]<=aabb.max.Coord[2]) and (p.Coord[2]>=aabb.min.Coord[2]);
end;

// PlaneIntersectAABB
//
function PlaneIntersectAABB(Normal: TAffineVector; d: single; aabb: TAABB): boolean;
var
vmax, vmin: TAffineVector;
i: integer;
begin
     result:= false;
     for i:= 0 to 2 do
          if normal.Coord[i] > 0.0 then begin
               vMin.Coord[i]:= aabb.min.Coord[i];
               vMax.Coord[i]:= aabb.max.Coord[i];
          end else begin
               vMin.Coord[i]:= aabb.max.Coord[i];
               vMax.Coord[i]:= aabb.min.Coord[i];
          end;

     if VectorDotProduct(normal, vmin) + d > 0 then Exit;
     if VectorDotProduct(normal, vmax) + d >= 0 then result:= true;
end;

// TriangleIntersectAABB
//
function TriangleIntersectAABB(const aabb: TAABB;
v1, v2, v3: TAffineVector): boolean;
var
d, min, max: single;
a, normal: TAffineVector;
f, e: array[1..3] of TAffineVector;
i, j: integer;
p1, p2, p3, r: single;
begin
     result:= false;

     VectorSubtract(v2, v1, f[1]);  //f0 = v1 - v0
     VectorSubtract(v3, v2, f[2]);  //f1 = v2 - v1
     VectorSubtract(v1, v3, f[3]);  //f2 = v0 - v2

     e[1]:= XVector;
     e[2]:= YVector;
     e[3]:= ZVector;

     //test 3
     min:= MinXYZComponent(affineVectorMake(v1.Coord[0], v2.Coord[0], v3.Coord[0]));
     max:= MaxXYZComponent(affineVectorMake(v1.Coord[0], v2.Coord[0], v3.Coord[0]));
     if (min > aabb.max.Coord[0]) or (max < aabb.min.Coord[0]) then exit;

     min:= MinXYZComponent(affineVectorMake(v1.Coord[1], v2.Coord[1], v3.Coord[1]));
     max:= MaxXYZComponent(affineVectorMake(v1.Coord[1], v2.Coord[1], v3.Coord[1]));
     if (min > aabb.max.Coord[1]) or (max < aabb.min.Coord[1]) then exit;

     min:= MinXYZComponent(affineVectorMake(v1.Coord[2], v2.Coord[2], v3.Coord[2]));
     max:= MaxXYZComponent(affineVectorMake(v1.Coord[2], v2.Coord[2], v3.Coord[2]));
     if (min > aabb.max.Coord[2]) or (max < aabb.min.Coord[2]) then exit;

     //test 2
     VectorCrossProduct(f[1], f[2], normal);
     d:= -VectorDotProduct(normal, v1);
     if not PlaneIntersectAABB(normal, d, aabb) then exit;

     //test 3
     for i:= 1 to 3 do
          for j:= 1 to 3 do begin
               VectorCrossProduct(e[i], f[j], a);
               p1:= VectorDotProduct(a, v1);
               p2:= VectorDotProduct(a, v2);
               p3:= VectorDotProduct(a, v3);
               r:= aabb.max.Coord[0] * abs(a.Coord[0]) + aabb.max.Coord[1] * abs(a.Coord[1]) + aabb.max.Coord[2] * abs(a.Coord[2]);
               if (MinFloat(p1, p2, p3) > r) or (maxFloat(p1, p2, p3) < -r) then exit;
          end;

     result:= true;
end;

// ExtractAABBCorners
//
procedure ExtractAABBCorners(const AABB: TAABB; var AABBCorners : TAABBCorners);
begin
  MakeVector(AABBCorners[0], AABB.min.Coord[0], AABB.min.Coord[1], AABB.min.Coord[2]);
  MakeVector(AABBCorners[1], AABB.min.Coord[0], AABB.min.Coord[1], AABB.max.Coord[2]);
  MakeVector(AABBCorners[2], AABB.min.Coord[0], AABB.max.Coord[1], AABB.min.Coord[2]);
  MakeVector(AABBCorners[3], AABB.min.Coord[0], AABB.max.Coord[1], AABB.max.Coord[2]);

  MakeVector(AABBCorners[4], AABB.max.Coord[0], AABB.min.Coord[1], AABB.min.Coord[2]);
  MakeVector(AABBCorners[5], AABB.max.Coord[0], AABB.min.Coord[1], AABB.max.Coord[2]);
  MakeVector(AABBCorners[6], AABB.max.Coord[0], AABB.max.Coord[1], AABB.min.Coord[2]);
  MakeVector(AABBCorners[7], AABB.max.Coord[0], AABB.max.Coord[1], AABB.max.Coord[2]);
end;

//  AABBToBSphere
//
procedure AABBToBSphere(const AABB : TAABB; var BSphere : TBSphere);
begin
  BSphere.Center := VectorScale(VectorAdd(AABB.min, AABB.max), 0.5);
  BSphere.Radius := VectorDistance(AABB.min, AABB.max) * 0.5;
end;

//  BSphereToAABB (bsphere)
//
procedure BSphereToAABB(const BSphere : TBSphere; var AABB : TAABB);
begin
   AABB.min:=VectorSubtract(BSphere.Center, BSphere.Radius);
   AABB.max:=VectorAdd(BSphere.Center, BSphere.Radius);
end;

// BSphereToAABB (affine center, radius)
//
function BSphereToAABB(const center : TAffineVector; radius : Single) : TAABB;
begin
   Result.min:=VectorSubtract(center, radius);
   Result.max:=VectorAdd(center, radius);
end;

// BSphereToAABB (hmg center, radius)
//
function BSphereToAABB(const center : TVector; radius : Single) : TAABB;
begin
   SetVector(Result.min, VectorSubtract(center, radius));
   SetVector(Result.max, VectorAdd(center, radius));
end;

//  AABBContainsAABB
//
function AABBContainsAABB(const mainAABB, testAABB : TAABB) : TSpaceContains;
begin
  // AABB1 fits completely inside AABB2?
  // AABB1 min must be >= to AABB2 min
  // AABB1 max must be <= to AABB2 max

  if
   ((mainAABB.min.Coord[0]<testAABB.max.Coord[0]) and
    (mainAABB.min.Coord[1]<testAABB.max.Coord[1]) and
    (mainAABB.min.Coord[2]<testAABB.max.Coord[2]) and

    (testAABB.min.Coord[0]<mainAABB.max.Coord[0]) and
    (testAABB.min.Coord[1]<mainAABB.max.Coord[1]) and
    (testAABB.min.Coord[2]<mainAABB.max.Coord[2])) then
  begin
    if(testAABB.min.Coord[0]>=mainAABB.min.Coord[0]) and
      (testAABB.min.Coord[1]>=mainAABB.min.Coord[1]) and
      (testAABB.min.Coord[2]>=mainAABB.min.Coord[2]) and

      (testAABB.max.Coord[0]<=mainAABB.max.Coord[0]) and
      (testAABB.max.Coord[1]<=mainAABB.max.Coord[1]) and
      (testAABB.max.Coord[2]<=mainAABB.max.Coord[2]) then
      result := scContainsFully
    else
      result := scContainsPartially;
  end else
    result := scNoOverlap;
end;

//  AABBContainsBSphere
//
function AABBContainsBSphere(const mainAABB : TAABB; const testBSphere : TBSphere) : TSpaceContains;
var
  testAABB : TAABB;
begin
  BSphereToAABB(testBSphere, testAABB);
  result := AABBContainsAABB(mainAABB, testAABB);
end;

function PlaneContainsBSphere(const Location, Normal : TAffineVector; const testBSphere : TBSphere) : TSpaceContains;
var
  Dist : single;
begin
  Dist := PointPlaneDistance(testBSphere.Center, Location, Normal);

  if Dist > testBSphere.Radius then
    result := scNoOverlap
  else if abs(Dist)<= testBSphere.Radius then
    result := scContainsPartially
  else
    result := scContainsFully;
end;

// FrustumContainsBSphere
//
function FrustumContainsBSphere(const Frustum : TFrustum; const testBSphere : TBSphere) : TSpaceContains;
var
  negRadius : Single;
  HitCount : integer;
  Distance : single;
  i : integer;
type
  TPlaneArray = array[0..5] of THmgPlane;
begin
  negRadius:=-testBSphere.Radius;

  HitCount := 0;

  // This would be fractionally faster to unroll, but oh so ugly!?
  for i := 0 to 5 do
  begin
    Distance := PlaneEvaluatePoint(TPlaneArray(frustum)[i], testBSphere.Center);
    if Distance<negRadius then begin
      result := scNoOverlap;
      exit;
    end else if Distance >= testBSphere.Radius then
      inc(HitCount);
  end;//}

  if HitCount=6 then
    result := scContainsFully
  else
    result := scContainsPartially;
end;

// FrustumContainsBSphere
// see http://www.flipcode.com/articles/article_frustumculling.shtml
function FrustumContainsAABB(const Frustum : TFrustum; const testAABB : TAABB) : TSpaceContains;
type
  TPlaneArray = array[0..5] of THmgPlane;
var
  iPlane, iCorner : integer;
  PointIn : boolean;
  AABBCorners : TAABBCorners;
  InCount : integer;
  TotalIn : integer;
begin
  ExtractAABBCorners(testAABB, AABBCorners);

  TotalIn := 0;
  // test all 8 corners against the 6 sides
	// if all points are behind 1 specific plane, we are out
	// if we are in with all points, then we are fully in

  // For each plane
  for iPlane := Low(TPlaneArray) to High(TPlaneArray) do begin
    // We're about to test 8 corners
    InCount := 8;
    PointIn := true;

    // For each corner
    for iCorner := Low(AABBCorners) to High(AABBCorners) do begin
      if PlaneEvaluatePoint(TPlaneArray(Frustum)[iPlane], AABBCorners[iCorner])<0 then begin
        PointIn := false;
        dec(InCount);
      end;
    end;

    if InCount=0 then begin
      result := scNoOverlap;
      exit;
    end

    else if PointIn then
      inc(TotalIn);
  end;

  if TotalIn=6 then
    result := scContainsFully
  else
    result := scContainsPartially;
end;

//  BSphereContainsAABB
//
function BSphereContainsAABB(const mainBSphere : TBSphere; const testAABB : TAABB) : TSpaceContains;
var
  r2: single;
  ClippedCenter : TAffineVector;

  AABBCorners : TAABBCorners;
  CornerHitCount : integer;
begin
  r2 := sqr(mainBSphere.Radius);

  ClippedCenter := ClipToAABB(mainBSphere.Center, testAABB);

  if VectorDistance2(ClippedCenter, mainBSphere.Center) < r2 then
  begin
    ExtractAABBCorners(testAABB, AABBCorners);

    CornerHitCount := 0;
    // BSphere fully contains aabb if all corners of aabb are within bsphere.
    if (VectorDistance2(mainBSphere.Center, AABBCorners[0]) < r2) then inc(CornerHitCount);
    if (VectorDistance2(mainBSphere.Center, AABBCorners[1]) < r2) then inc(CornerHitCount);
    if (VectorDistance2(mainBSphere.Center, AABBCorners[2]) < r2) then inc(CornerHitCount);
    if (VectorDistance2(mainBSphere.Center, AABBCorners[3]) < r2) then inc(CornerHitCount);
    if (VectorDistance2(mainBSphere.Center, AABBCorners[4]) < r2) then inc(CornerHitCount);
    if (VectorDistance2(mainBSphere.Center, AABBCorners[5]) < r2) then inc(CornerHitCount);
    if (VectorDistance2(mainBSphere.Center, AABBCorners[6]) < r2) then inc(CornerHitCount);
    if (VectorDistance2(mainBSphere.Center, AABBCorners[7]) < r2) then inc(CornerHitCount);

    if CornerHitCount=7 then
      result := scContainsFully
    else
      result := scContainsPartially;
  end else
    result := scNoOverlap;
end;

//  BSphereContainsBSphere
//
function BSphereContainsBSphere(const mainBSphere, testBSphere : TBSphere) : TSpaceContains;
var
  d2 : single;
begin
  d2 := VectorDistance2(mainBSphere.Center, testBSphere.Center);

  if d2<sqr(mainBSphere.Radius+testBSphere.Radius) then
  begin
    if d2<sqr(mainBSphere.Radius-testBSphere.Radius) then
      result := scContainsFully
    else
      result := scContainsPartially;
  end else
    result := scNoOverlap;
end;

//  BSphereIntersectsBSphere
//
function BSphereIntersectsBSphere(const mainBSphere, testBSphere : TBSphere) : boolean;
begin
  result := VectorDistance2(mainBSphere.Center, testBSphere.Center)<sqr(mainBSphere.Radius+testBSphere.Radius);
end;

//  ClipToAABB
//
function ClipToAABB(const v : TAffineVector; const AABB : TAABB) : TAffineVector;
begin
  result := v;

  if result.Coord[0]<AABB.min.Coord[0] then result.Coord[0] := AABB.min.Coord[0];
  if result.Coord[1]<AABB.min.Coord[1] then result.Coord[1] := AABB.min.Coord[1];
  if result.Coord[2]<AABB.min.Coord[2] then result.Coord[2] := AABB.min.Coord[2];

  if result.Coord[0]>AABB.max.Coord[0] then result.Coord[0] := AABB.max.Coord[0];
  if result.Coord[1]>AABB.max.Coord[1] then result.Coord[1] := AABB.max.Coord[1];
  if result.Coord[2]>AABB.max.Coord[2] then result.Coord[2] := AABB.max.Coord[2];
end;

// IncludeInClipRect
//
procedure IncludeInClipRect(var clipRect : TClipRect; x, y : Single);
begin
   with clipRect do begin
      if x<Left then Left:=x;
      if x>Right then Right:=x;
      if y<Top then Top:=y;
      if y>Bottom then Bottom:=y;
   end;
end;

// AABBToClipRect
//
function AABBToClipRect(const aabb : TAABB; modelViewProjection : TMatrix;
                        viewportSizeX, viewportSizeY : Integer) : TClipRect;
var
   i : Integer;
   v, vt : TVector;
   minmax : array [0..1] of PAffineVector;
begin
   minmax[0]:=@aabb.min;
   minmax[1]:=@aabb.max;
   v.Coord[3]:=1;
   for i:=0 to 7 do begin
      v.Coord[0]:=minmax[i and 1].Coord[0];
      v.Coord[1]:=minmax[(i shr 1) and 1].Coord[1];
      v.Coord[2]:=minmax[(i shr 2) and 1].Coord[2];

      // Project
      vt:=VectorTransform(v, modelViewProjection);
      ScaleVector(vt, 1/vt.Coord[3]);

      // Convert to screen coordinates
      if i>0 then
         IncludeInClipRect(Result, viewportSizeX*(vt.Coord[0]+1)*0.5, viewportSizeY*(vt.Coord[1]+1)*0.5)
      else begin
         Result.Left:=viewportSizeX*(vt.Coord[0]+1)*0.5;
         Result.Top:=viewportSizeY*(vt.Coord[1]+1)*0.5;
         Result.Right:=Result.Left;
         Result.Bottom:=Result.Top;
      end;
   end;
end;

end.
