// GLCollision
{: Egg<p>

	Collision-detection management for GLScene<p>

	<b>Historique : </b><font size=-1><ul>
      <li>22/02/01 - Egg - Included new collision code by Uwe Raabe
      <li>08/08/00 - Egg - Fixed TGLBCollision.Assign
      <li>16/07/00 - Egg - Added support for all bounding modes (most are un-tested)
	   <li>23/05/00 - Egg - Creation
	</ul></font>
}
unit GLCollision;

interface

uses Classes, GLScene, XCollection, Geometry, VectorLists, GLVectorFileObjects, GeometryBB,
  GLCrossPlatform;

type

   TGLBCollision = class;

   TObjectCollisionEvent = procedure (Sender : TObject; object1, object2 : TGLBaseSceneObject) of object;

   // TCollisionBoundingMode
   //
   {: Defines how fine collision bounding is for a particular object.<p>
      Possible values are :<ul>
      <li>cbmPoint : the object is punctual and may only collide with volumes
      <li>cbmSphere : the object is defined by its bounding sphere (sphere radius
         is the max of axis-aligned dimensions)
      <li>cbmEllipsoid the object is defined by its bounding axis-aligned ellipsoid
      <li>cbmCube : the object is defined by a bounding axis-aligned "cube"
      <li>cbmFaces : the object is defined by its faces (needs object-level support,
         if unavalaible, uses cbmCube code)
      </ul> }
   TCollisionBoundingMode = (cbmPoint, cbmSphere, cbmEllipsoid, cbmCube, cbmFaces);

   TFastCollisionChecker = function (obj1, obj2 : TGLBaseSceneObject) : Boolean;
   PFastCollisionChecker = ^TFastCollisionChecker;

	// TCollisionManager
	//
	TCollisionManager = class (TComponent)
	   private
	      { Private Declarations }
         FClients : TList;
         FOnCollision : TObjectCollisionEvent;

	   protected
	      { Protected Declarations }
	      procedure RegisterClient(aClient : TGLBCollision);
	      procedure DeRegisterClient(aClient : TGLBCollision);
	      procedure DeRegisterAllClients;

	   public
	      { Public Declarations }
	      constructor Create(AOwner: TComponent); override;
         destructor Destroy; override;

	      procedure CheckCollisions;

		published
			{ Published Declarations }
         property OnCollision : TObjectCollisionEvent read FOnCollision write FOnCollision;
	end;

  	// TGLBCollision
	//
	{: Collision detection behaviour.<p>
		Allows an object to register to a TCollisionManager and be accounted for
      in collision-detection and distance calculation mechanisms.<p>
      An object may have multiple TGLBCollision, registered to multiple collision
      managers, however if multiple behaviours share the same manager, only one
      of them will be accounted for, others will be ignored. }
	TGLBCollision = class (TGLBehaviour)
		private
			{ Private Declarations }
         FBoundingMode : TCollisionBoundingMode;
         FManager : TCollisionManager;
         FManagerName : String; // NOT persistent, temporarily used for persistence
         FGroupIndex : Integer;

		protected
			{ Protected Declarations }
         procedure SetGroupIndex(const value : Integer);
         procedure SetManager(const val : TCollisionManager);

			procedure WriteToFiler(writer : TWriter); override;
         procedure ReadFromFiler(reader : TReader); override;
         procedure Loaded; override;

		public
			{ Public Declarations }
			constructor Create(aOwner : TXCollection); override;
			destructor Destroy; override;

         procedure Assign(Source: TPersistent); override;

			class function FriendlyName : String; override;
			class function FriendlyDescription : String; override;

		published
			{ Published Declarations }
         {: Refers the collision manager. }
         property Manager : TCollisionManager read FManager write SetManager;
         property BoundingMode : TCollisionBoundingMode read FBoundingMode write FBoundingMode;
         property GroupIndex : Integer read FGroupIndex write SetGroupIndex;
	end;

function FastCheckPointVsPoint(obj1, obj2 : TGLBaseSceneObject) : Boolean;
function FastCheckPointVsSphere(obj1, obj2 : TGLBaseSceneObject) : Boolean;
function FastCheckPointVsEllipsoid(obj1, obj2 : TGLBaseSceneObject) : Boolean;
function FastCheckPointVsCube(obj1, obj2 : TGLBaseSceneObject) : Boolean;
function FastCheckSphereVsPoint(obj1, obj2 : TGLBaseSceneObject) : Boolean;
function FastCheckSphereVsSphere(obj1, obj2 : TGLBaseSceneObject) : Boolean;
function FastCheckSphereVsEllipsoid(obj1, obj2 : TGLBaseSceneObject) : Boolean;
function FastCheckSphereVsCube(obj1, obj2 : TGLBaseSceneObject) : Boolean;
function FastCheckEllipsoidVsPoint(obj1, obj2 : TGLBaseSceneObject) : Boolean;
function FastCheckEllipsoidVsSphere(obj1, obj2 : TGLBaseSceneObject) : Boolean;
function FastCheckEllipsoidVsEllipsoid(obj1, obj2 : TGLBaseSceneObject) : Boolean;
function FastCheckEllipsoidVsCube(obj1, obj2 : TGLBaseSceneObject) : Boolean;
function FastCheckCubeVsPoint(obj1, obj2 : TGLBaseSceneObject) : Boolean;
function FastCheckCubeVsSphere(obj1, obj2 : TGLBaseSceneObject) : Boolean;
function FastCheckCubeVsEllipsoid(obj1, obj2 : TGLBaseSceneObject) : Boolean;
function FastCheckCubeVsCube(obj1, obj2 : TGLBaseSceneObject) : Boolean;
function FastCheckFaceVsFace(obj1, obj2 : TGLBaseSceneObject) : Boolean;
  //returns true when the bounding box cubes does intersect the other
  //also true when the one cube does contain the other completely
function IntersectCubes(Obj1, Obj2:TGLBaseSceneObject):boolean; overload;
  //this one only uses the AxisAlignedBoundingBoxes and the conversion Matrixes
  //to convert one point to the others AABB system
function IntersectCubes(const AABB1, AABB2: TAABB; const M1To2, M2To1: TMatrix):boolean; overload;

var
  UseTriCubes:boolean = true; //debugging

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

uses SysUtils, GLMisc;

const
   cEpsilon : Single = 1e-6;

const
   cFastCollisionChecker : array [cbmPoint..cbmFaces, cbmPoint..cbmFaces] of TFastCollisionChecker = (
       (FastCheckPointVsPoint,      FastCheckPointVsSphere,       FastCheckPointVsEllipsoid,       FastCheckPointVsCube,      FastCheckPointVsCube),
       (FastCheckSphereVsPoint,     FastCheckSphereVsSphere,      FastCheckSphereVsEllipsoid,      FastCheckSphereVsCube,     FastCheckSphereVsCube),
       (FastCheckEllipsoidVsPoint,  FastCheckEllipsoidVsSphere,   FastCheckEllipsoidVsEllipsoid,   FastCheckEllipsoidVsCube,  FastCheckEllipsoidVsCube),
       (FastCheckCubeVsPoint,       FastCheckCubeVsSphere,        FastCheckCubeVsEllipsoid,        IntersectCubes,            IntersectCubes),
       (FastCheckCubeVsPoint,       FastCheckCubeVsSphere,        FastCheckCubeVsEllipsoid,        IntersectCubes,            FastCheckFaceVsFace)
      );

// Collision utility routines

// Fast Collision detection routines
// by "fast" I mean they are heavily specialized and just return a boolean

function FastCheckPointVsPoint(obj1, obj2 : TGLBaseSceneObject) : Boolean;
begin
   Result:=(obj2.SqrDistanceTo(obj1.AbsolutePosition)<=cEpsilon);
end;

function FastCheckPointVsSphere(obj1, obj2 : TGLBaseSceneObject) : Boolean;
begin
   Result:=(obj2.SqrDistanceTo(obj1.AbsolutePosition)<=Sqr(obj2.BoundingSphereRadius));
end;

function FastCheckPointVsEllipsoid(obj1, obj2 : TGLBaseSceneObject) : Boolean;
var
   v : TVector;
begin
   // calc vector expressed in local coordinates (for obj2)
   v:=VectorTransform(obj1.AbsolutePosition, obj2.InvAbsoluteMatrix);
   // rescale to unit dimensions
   DivideVector(v, obj2.AxisAlignedDimensions);
   v[3]:=0;
   // if norm is below 1, collision
   Result:=(VectorNorm(v)<=1);
end;

function FastCheckPointVsCube(obj1, obj2 : TGLBaseSceneObject) : Boolean;
var
   v : TVector;
begin
   // calc vector expressed in local coordinates (for obj2)
   v:=VectorTransform(obj1.AbsolutePosition, obj2.InvAbsoluteMatrix);
   // rescale to unit dimensions
   DivideVector(v, obj2.AxisAlignedDimensions);
   // if abs() of all components are below 1, collision
   Result:=(MaxAbsXYZComponent(v)<=1);
end;

function FastCheckSphereVsPoint(obj1, obj2 : TGLBaseSceneObject) : Boolean;
begin
   Result:=(obj1.SqrDistanceTo(obj2.AbsolutePosition)<=Sqr(obj1.BoundingSphereRadius));
end;

function FastCheckSphereVsSphere(obj1, obj2 : TGLBaseSceneObject) : Boolean;
begin
   Result:=(obj1.SqrDistanceTo(obj2.AbsolutePosition)
            <= Sqr(obj1.BoundingSphereRadius+obj2.BoundingSphereRadius));
end;

function FastCheckSphereVsEllipsoid(obj1, obj2 : TGLBaseSceneObject) : Boolean;
var
   v : TVector;
   aad : TVector;
begin
   // express in local coordinates (for obj2)
   v:=VectorTransform(obj1.AbsolutePosition, obj2.InvAbsoluteMatrix);
   // calc local vector, and rescale to unit dimensions
   //VectorSubstract(pt1, obj2.AbsolutePosition, v);
   aad:=VectorAdd(obj2.AxisAlignedDimensions, obj1.BoundingSphereRadius);
   DivideVector(v, aad);
   v[3]:=0;
   // if norm is below 1, collision
   Result:=(VectorNorm(v)<=1);
end;

function FastCheckSphereVsCube(obj1, obj2 : TGLBaseSceneObject) : Boolean;
var
   v : TVector;
   aad : TVector;
   r,r2 : Single;
begin
   // express in local coordinates (for cube "obj2")
   // v gives the vector from obj2 to obj1 expressed in obj2's local system
   v := VectorTransform(obj1.AbsolutePosition, obj2.InvAbsoluteMatrix);
   // because of symmetry we can make abs(v)
   v[0] := abs(v[0]);
   v[1] := abs(v[1]);
   v[2] := abs(v[2]);
   aad := obj2.AxisAlignedDimensions; // should be abs at all!
   VectorSubtract(v, aad, v); // v holds the distance in each axis
   v[3] := 0;
   r := obj1.BoundingSphereRadius;
   r2 := Sqr(r);
   if (v[0]>0) then begin
     if (v[1]>0) then begin
       if (v[2]>0) then begin
         // v is outside axis parallel projection, so use distance to edge point
         result := (VectorNorm(v)<=r2);
       end else begin
         // v is inside z axis projection, but outside x-y projection
         result := (VectorNorm(v[0],v[1])<=r2);
       end
     end else begin
       if (v[2]>0) then begin
         // v is inside y axis projection, but outside x-z projection
         result := (VectorNorm(v[0],v[2])<=r2);
       end else begin
         // v is inside y-z axis projection, but outside x projection
         result := (v[0]<=r);
       end
     end
   end else begin
     if (v[1]>0) then begin
       if (v[2]>0) then begin
         // v is inside x axis projection, but outside y-z projection
         result := (VectorNorm(v[1],v[2])<=r2);
       end else begin
         // v is inside x-z projection, but outside y projection
         result := (v[1]<=r);
       end
     end else begin
       if (v[2]>0) then begin
         // v is inside x-y axis projection, but outside z projection
         result := (v[2]<=r);
       end else begin
         // v is inside all axes parallel projection, so it is inside cube
         result := true;
       end;
     end
   end;
end;

function FastCheckEllipsoidVsPoint(obj1, obj2 : TGLBaseSceneObject) : Boolean;
begin
   Result:=FastCheckPointVsEllipsoid(obj2, obj1);
end;

function FastCheckEllipsoidVsSphere(obj1, obj2 : TGLBaseSceneObject) : Boolean;
begin
   Result:=FastCheckSphereVsEllipsoid(obj2, obj1);
end;

function FastCheckEllipsoidVsEllipsoid(obj1, obj2 : TGLBaseSceneObject) : Boolean;
var
   v1, v2 : TVector;
begin
   // express in local coordinates (for obj2)
   v1:=VectorTransform(obj1.AbsolutePosition, obj2.InvAbsoluteMatrix);
   // calc local vector, and rescale to unit dimensions
   //VectorSubstract(pt, obj2.AbsolutePosition, v1);
   DivideVector(v1, obj2.AxisAlignedDimensions);
   v1[3]:=0;
   // express in local coordinates (for obj1)
   v2:=VectorTransform(obj2.AbsolutePosition, obj1.InvAbsoluteMatrix);
   // calc local vector, and rescale to unit dimensions
   //VectorSubstract(pt, obj1.AbsolutePosition, v2);
   DivideVector(v2, obj1.AxisAlignedDimensions);
   v2[3]:=0;
   // if sum of norms is below 2, collision
   Result:=(VectorNorm(v1)+VectorNorm(v2)<=2);
end;

function FastCheckEllipsoidVsCube(obj1, obj2 : TGLBaseSceneObject) : Boolean;
{ current implementation assumes Ellipsoid as Sphere }
var
   v : TVector;
   aad : TVector;
begin
   // express in local coordinates (for obj2)
   v:=VectorTransform(obj1.AbsolutePosition, obj2.InvAbsoluteMatrix);
   // calc local vector, and rescale to unit dimensions
   aad:=VectorAdd(obj2.AxisAlignedDimensions, obj1.BoundingSphereRadius);
   DivideVector(v, aad);
   v[3]:=0;
   // if norm is below 1, collision
   Result:=(VectorNorm(v)<=1);
end;

function FastCheckCubeVsPoint(obj1, obj2 : TGLBaseSceneObject) : Boolean;
begin
   Result:=FastCheckPointVsCube(obj2, obj1);
end;

function FastCheckCubeVsSphere(obj1, obj2 : TGLBaseSceneObject) : Boolean;
begin
   Result:=FastCheckSphereVsCube(obj2, obj1);
end;

function FastCheckCubeVsEllipsoid(obj1, obj2 : TGLBaseSceneObject) : Boolean;
begin
   Result:=FastCheckEllipsoidVsCube(obj2, obj1);
end;

procedure InitArray(v:TVector; var pt:array of TVector);
// calculate the cube edge points from the axis aligned dimension
begin
  pt[0] := VectorMake(-v[0],-v[1],-v[2],1);
  pt[1] := VectorMake( v[0],-v[1],-v[2],1);
  pt[2] := VectorMake( v[0], v[1],-v[2],1);
  pt[3] := VectorMake(-v[0], v[1],-v[2],1);
  pt[4] := VectorMake(-v[0],-v[1], v[2],1);
  pt[5] := VectorMake( v[0],-v[1], v[2],1);
  pt[6] := VectorMake( v[0], v[1], v[2],1);
  pt[7] := VectorMake(-v[0], v[1], v[2],1);
end;

function DoCubesIntersectPrim(obj1, obj2 : TGLBaseSceneObject) : Boolean;
// first check if any edge point of "cube" obj1 lies within "cube" obj2
// else, for each "wire" in then wireframe of the "cube" obj1, check if it
// intersects with one of the "planes" of "cube" obj2

  function CheckWire(p0,p1,pl:TVector):Boolean;
  // check "wire" line (p0,p1) for intersection with each plane, given from
  // axis aligned dimensions pl
  // - calculate "direction" d: p0 -> p1
  // - for each axis (0..2) do
  //   - calculate line parameter t of intersection with plane pl[I]
  //   - if not in range [0..1] (= not within p0->p1), no intersection
  //   - else
  //     - calculate intersection point s = p0 + t*d
  //     - for both other axes check if coordinates are within range
  //   - do the same for opposite plane -pl[I]
  var
    t : Single;
    d,s : TVector;
    i,j,k : Integer;
  begin
    result := true;
    VectorSubtract(p1, p0, d);    // d: direction p0 -> p1
    for i:=0 to 2 do begin
      if d[i]=0 then begin       // wire is parallel to plane
        // this case will be handled by the other planes
      end else begin
        j := (i+1) mod 3;
        k := (j+1) mod 3;
        t := (pl[i]-p0[i])/d[i];   // t: line parameter of intersection
        if IsInRange(t, 0, 1) then begin
          s := p0;
          CombineVector(s,d,t);    // calculate intersection
          // if the other two coordinates lie within the ranges, collision
          if IsInRange(s[j],-pl[j],pl[j]) and IsInRange(s[k],-pl[k],pl[k]) then Exit;
        end;
        t := (-pl[i]-p0[i])/d[i];   // t: parameter of intersection
        if IsInRange(t,0,1) then begin
          s := p0;
          CombineVector(s,d,t);    // calculate intersection
          // if the other two coordinates lie within the ranges, collision
          if IsInRange(s[j],-pl[j],pl[j]) and IsInRange(s[k],-pl[k],pl[k]) then Exit;
        end;
      end;
    end;
    result := false;
  end;

const
  cWires : array[0..11,0..1] of Integer
         = ((0,1),(1,2),(2,3),(3,0),
            (4,5),(5,6),(6,7),(7,4),
            (0,4),(1,5),(2,6),(3,7));
var
  pt1 : array[0..7] of TVector;
  M : TMatrix;
  I : Integer;
  aad : TVector;
begin
  result := true;
  aad := obj2.AxisAlignedDimensions;
  InitArray(obj1.AxisAlignedDimensions,pt1);
  // calculate the matrix to transform obj1 into obj2
  MatrixMultiply(obj1.AbsoluteMatrix,obj2.InvAbsoluteMatrix,M);
  for I:=0 to 7 do begin // transform points of obj1
    pt1[I] := VectorTransform(pt1[I],M);
    // check if point lies inside "cube" obj2, collision
    if IsInCube(pt1[I],aad) then Exit;
  end;
  for I:=0 to 11 do begin
    if CheckWire(pt1[cWires[I,0]],pt1[cWires[I,1]],aad) then Exit;
  end;
  result := false;
end;

function FastCheckCubeVsCube(obj1, obj2 : TGLBaseSceneObject) : Boolean;
var
  aad1,aad2 : TVector;
  D1,D2,D : Double;
begin
  aad1 := obj1.AxisAlignedDimensions;
  aad2 := obj2.AxisAlignedDimensions;
  D1 := VectorLength(aad1);
  D2 := VectorLength(aad2);
  D  := Sqrt(obj1.SqrDistanceTo(obj2.AbsolutePosition));
  if D>(D1+D2) then result := false
  else begin
    D1 := MinAbsXYZComponent(aad1);
    D2 := MinAbsXYZComponent(aad2);
    if D<(D1+D2) then result := true
    else begin
      result := DoCubesIntersectPrim(obj1,obj2) or
                DoCubesIntersectPrim(obj2,obj1);
    end;
  end;
end;

//this function does not check for rounds that results from Smoth rendering
//if anybody needs this, you are welcome to show a solution, but usually this should be good enough
function FastCheckFaceVsFace(obj1, obj2 : TGLBaseSceneObject) : Boolean;
type
   TTriangle = array [0..2] of TAffineVector;
   PTriangle = ^TTriangle;

   procedure KillDuplicates(src, newList : TAffineVectorList);
//   var
//      i : integer;
   begin
      if src.Count<1 then Exit;
//      src.Sort; // as triangles
{      for i:=src.Count-1 downto 1 do begin
         //adding the good ones to  anew list is better (faster) than deleting the bad ones
         if CompareTri(i, i-1) <> 0 then
            newList.Add(self[i]);
      end;
      newList.Add(self[0]);
      }
      newList.Assign(src);
   end;

var
  i:integer;
  tl: TAffineVectorList;
  TriListAll, TriList: TAffineVectorList;
  tri : PTriangle;
begin
  result:= false;
  if (obj1 is TGLFreeForm) and (obj2 is TGLFreeForm) then
  begin
    //check if we are initialized correct
    if not assigned(TGLFreeForm(obj1).Octree) then TGLFreeForm(obj1).BuildOctree;
    if not assigned(TGLFreeForm(obj2).Octree) then TGLFreeForm(obj2).BuildOctree;

    //Check triangles against the other object
    if UseTriCubes then
    begin //check only the one that are near the destination object (using octree of obj1)
      TriListAll:=TGLFreeForm(obj1).GetTrianglesInCube(obj2); //get the 'hot' ones using the tree
      TriList:=TAffineVectorList.Create;

      KillDuplicates(TriListAll, TriList); //kill the duplicates

      TriListAll.Free;
      //in the list originally are the local coords, TransformAsPoints-> now we have obj1 absolute coords
      TriList.TransformAsPoints(obj1.AbsoluteMatrix); //Transform to Absolute Coords
      try
         i:=0;
         while i<TriList.Count-2 do begin
            //here we pass absolute coords, then these are transformed with Obj2's InvAbsoluteMatrix to match the local Obj2 System
            Tri:=@TriList.List[i];
            // the next function will check the given Triangle against only these ones that are close (using the octree of obj2)
            if TGLFreeForm(obj2).OctreeTriangleIntersect(Tri[0], Tri[1], Tri[2]) then begin
               Result:= true;
               { TODO : Optimize, exit was disabled for performance checks }
               Exit;
            end;
            Inc(i, 3);
         end;
      finally
        TriList.Free;
      end;
    end else
    begin //the stupid way just check all triangles
      tl:=TGLFreeForm(obj1).MeshObjects.ExtractTriangles;
      tl.TransformAsPoints(obj1.AbsoluteMatrix);
      try
        for i:=0 to (tl.count div 3)-1 do
        begin
          //in the list originally are the local coords, TransformAsPoints-> we have obj1 absolute coords
          //here we pass absolute coords, then these are transformed with Obj2's AbsoluteMatrix to match the local Obj2 System
          if TGLFreeForm(obj2).OctreeTriangleIntersect(
            tl.Items[i*3],
            tl.Items[i*3+1],
            tl.Items[i*3+2]) then
          begin
            result:= true;
            { TODO : Optimize, exit was disabled for performance checks }
            exit;
          end;
        end;
      finally
        tl.Free;
      end;
    end;
  end
  else
    result:= IntersectCubes(obj1, obj2); //FaceVsFace does work only for two FreeForm Objects
end;

function IntersectCubes(Obj1, Obj2:TGLBaseSceneObject):boolean;
var
  AABB1, AABB2: TAABB;
  M1To2, M2To1: TMatrix;
begin
  //Calc AABBs
  AABB1:= Obj1.AxisAlignedBoundingBox;
  AABB2:= Obj2.AxisAlignedBoundingBox;
  //Calc Conversion Matrixes
  MatrixMultiply(obj1.AbsoluteMatrix,obj2.InvAbsoluteMatrix,M1To2);
  MatrixMultiply(obj2.AbsoluteMatrix,obj1.InvAbsoluteMatrix,M2To1);

  result:= IntersectCubes(AABB1, AABB2, M1To2, M2To1);
end;

function IntersectCubes(const AABB1, AABB2: TAABB;
  const M1To2, M2To1: TMatrix):boolean;
const
  cWires : array[0..11,0..1] of Integer //Points of the wire
         = ((0,1),(1,2),(2,3),(3,0),
            (4,5),(5,6),(6,7),(7,4),
            (0,4),(1,5),(2,6),(3,7));
  cPlanes : array[0..5,0..3] of Integer //points of the planes
         = ((1,2,6,5), (2,3,7,6), (0,1,2,3), (0,3,7,4), (0,1,5,4), (5,6,7,4));
  procedure MakeAABBPoints(const AABB: TAABB; var pt:array of TVertex);
  begin
    pt[0] := AffineVectorMake(AABB.min[0], AABB.min[1], AABB.min[2]);
    pt[1] := AffineVectorMake(AABB.max[0], AABB.min[1], AABB.min[2]);
    pt[2] := AffineVectorMake(AABB.max[0], AABB.max[1], AABB.min[2]);
    pt[3] := AffineVectorMake(AABB.min[0], AABB.max[1], AABB.min[2]);
    pt[4] := AffineVectorMake(AABB.min[0], AABB.min[1], AABB.max[2]);
    pt[5] := AffineVectorMake(AABB.max[0], AABB.min[1], AABB.max[2]);
    pt[6] := AffineVectorMake(AABB.max[0], AABB.max[1], AABB.max[2]);
    pt[7] := AffineVectorMake(AABB.min[0], AABB.max[1], AABB.max[2]);
  end;
  procedure MakePlanes(const pt:array of TVertex; var Planes:array of THmgPlane);
  var
    i:integer;
  begin
    for i:=0 to 5 do
    begin
      Planes[i]:= PlaneMake(pt[cPlanes[i,0]], pt[cPlanes[i,1]], pt[cPlanes[i,2]]);
    end;
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
    pt:= VectorTransform(pt2[i], M2To1);
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
    pt1[i]:= VectorTransform(pt1[i], M1To2);
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


// ------------------
// ------------------ TCollisionManager ------------------
// ------------------

// Create
//
constructor TCollisionManager.Create(AOwner: TComponent);
begin
	inherited Create(AOwner);
   FClients:=TList.Create;
   RegisterManager(Self);
























































end;

// Destroy
//
destructor TCollisionManager.Destroy;
begin
	DeRegisterAllClients;
   DeRegisterManager(Self);
   FClients.Free;
	inherited Destroy;

























































































end;
























// RegisterClient
//
procedure TCollisionManager.RegisterClient(aClient : TGLBCollision);
begin
   if Assigned(aClient) then
      if FClients.IndexOf(aClient)<0 then begin
         FClients.Add(aClient);
         aClient.FManager:=Self;
      end;
























































































end;

// DeRegisterClient
//
procedure TCollisionManager.DeRegisterClient(aClient : TGLBCollision);
begin
   if Assigned(aClient) then begin
      aClient.FManager:=nil;
      FClients.Remove(aClient);
   end;



end;

// DeRegisterAllClients
//
procedure TCollisionManager.DeRegisterAllClients;
var
   i : Integer;
begin
   // Fast deregistration
   for i:=0 to FClients.Count-1 do
      TGLBCollision(FClients[i]).FManager:=nil;
   FClients.Clear;
end;

// CheckCollisions
//
procedure TCollisionManager.CheckCollisions;
var
   obj1, obj2 : TGLBaseSceneObject;
   cli1, cli2 : TGLBCollision;
   grp1, grp2 : Integer;   // GroupIndex of collisions
   i, j : Integer;
begin
   if not Assigned(FOnCollision) then Exit;
   // if you know a code slower than current one, call me ;)
   { TODO : speed improvements & distance cacheing }
   for i:=0 to FClients.Count-2 do begin
      cli1:=TGLBCollision(FClients[i]);
      obj1:=cli1.OwnerBaseSceneObject;
      grp1:=cli1.GroupIndex;
      for j:=i+1 to FClients.Count-1 do begin
         cli2:=TGLBCollision(FClients[j]);
         obj2:=cli2.OwnerBaseSceneObject;
         grp2:=cli2.GroupIndex;
         // if either one GroupIndex=0 or both are different, check for collision
         if ((grp1=0) or (grp2=0) or (grp1<>grp2)) and (obj1.Visible) and (obj2.Visible) then begin
           if cFastCollisionChecker[cli1.BoundingMode, cli2.BoundingMode](obj1, obj2) then
              FOnCollision(Self, obj1, obj2);
         end;
      end;
   end;
end;

// ------------------
// ------------------ TGLBCollision ------------------
// ------------------

// Create
//
constructor TGLBCollision.Create(aOwner : TXCollection);
begin
   inherited Create(aOwner);

end;

// Destroy
//
destructor TGLBCollision.Destroy;
begin
   Manager:=nil;
   inherited Destroy;





















end;

// FriendlyName
//
class function TGLBCollision.FriendlyName : String;
begin
   Result:='Collision';




end;

// FriendlyDescription
//
class function TGLBCollision.FriendlyDescription : String;
begin
   Result:='Collision-detection registration';

end;

// WriteToFiler
//
procedure TGLBCollision.WriteToFiler(writer : TWriter);
begin
   with writer do begin
      WriteInteger(1); // ArchiveVersion 1, added FGroupIndex
      if Assigned(FManager) then
         WriteString(FManager.GetNamePath)
      else WriteString('');
      WriteInteger(Integer(BoundingMode));
      WriteInteger(FGroupIndex);
   end;
end;

// ReadFromFiler
//
procedure TGLBCollision.ReadFromFiler(reader : TReader);
var
   archiveVersion : Integer;
begin
   with reader do begin
      archiveVersion:=ReadInteger;
      Assert(archiveVersion in [0..1]);
      FManagerName:=ReadString;
      BoundingMode:=TCollisionBoundingMode(ReadInteger);
      Manager:=nil;
      if archiveVersion>=1 then
         FGroupIndex:=ReadInteger
      else FGroupIndex:=0;
   end;
end;

// Loaded
//
procedure TGLBCollision.Loaded;
var
   mng : TComponent;
begin
   inherited;
   if FManagerName<>'' then begin
      mng:=FindManager(TCollisionManager, FManagerName);
      if Assigned(mng) then
         Manager:=TCollisionManager(mng);
      FManagerName:='';
   end;
end;

// Assign
//
procedure TGLBCollision.Assign(Source: TPersistent);
begin
   if Source is TGLBCollision then begin
      Manager:=TGLBCollision(Source).Manager;
      BoundingMode:=TGLBCollision(Source).BoundingMode;
   end;
   inherited Assign(Source);







end;

// SetManager
//
procedure TGLBCollision.SetManager(const val : TCollisionManager);
begin
   if val<>FManager then begin
      if Assigned(FManager) then
         FManager.DeRegisterClient(Self);
      if Assigned(val) then
         val.RegisterClient(Self);



   end;
end;

// SetGroupIndex
//
procedure TGLBCollision.SetGroupIndex(const value : Integer);
begin
   FGroupIndex:=value;




end;




















// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

	// class registrations
	RegisterXCollectionItemClass(TGLBCollision);

end.

