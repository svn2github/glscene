{: GeometryBB<p>

	Calculations and manipulations on Bounding Boxes.<p>

	<b>History : </b><font size=-1><ul>
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

end.
