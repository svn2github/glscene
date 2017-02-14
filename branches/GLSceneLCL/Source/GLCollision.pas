//
// This unit is part of the GLScene Project, http://glscene.org
//
{
   Collision-detection management for GLScene<p>

	History :  
       17/11/14 - PW - Refactored TCollisionManager to TGLCollisionManager
       10/11/12 - PW - Added CPP compatibility: changed vector arrays to records
       21/01/11 - DanB - Added "inherited" call to TGLBCollision.WriteToFiler
       03/04/07 - DaStr - Added "public" to TCollisionNode for FPC compatibility
       30/03/07 - DaStr - Added $I GLScene.inc
       19/10/06 - LC - Fixed memory leak in TCollisionManager.CheckCollisions. Bugtracker ID=1548618
       02/08/04 - LR, YHC - BCB corrections: use record instead array
       09/05/03 - DanB - fixed for collisions with bounding-box unproperly defined (min>max)
       09/05/03 - DanB - Added FastCheckCubeVsFace (Matheus Degiovani)
       13/02/03 - DanB - New collision code, and support for scaled objects
       22/02/01 - Egg - Included new collision code by Uwe Raabe
       08/08/00 - Egg - Fixed TGLBCollision.Assign
       16/07/00 - Egg - Added support for all bounding modes (most are un-tested)
	     23/05/00 - Egg - Creation
	 
}
unit GLCollision;

interface

{$I GLScene.inc}

uses
  Classes, SysUtils,
   
  GLScene, GLXCollection, GLVectorGeometry, GLVectorLists, GLVectorFileObjects,
  GLGeometryBB, GLCrossPlatform,
  GLManager , GLVectorTypes;


type

   TGLBCollision = class;

   TObjectCollisionEvent = procedure (Sender : TObject; object1, object2 : TGLBaseSceneObject) of object;

   // TCollisionBoundingMode
   //
   {: Defines how fine collision bounding is for a particular object.<p>
      Possible values are : 
       cbmPoint : the object is punctual and may only collide with volumes
       cbmSphere : the object is defined by its bounding sphere (sphere radius
         is the max of axis-aligned dimensions)
       cbmEllipsoid the object is defined by its bounding axis-aligned ellipsoid
       cbmCube : the object is defined by a bounding axis-aligned "cube"
       cbmFaces : the object is defined by its faces (needs object-level support,
         if unavalaible, uses cbmCube code)
        }
   TCollisionBoundingMode = (cbmPoint, cbmSphere, cbmEllipsoid, cbmCube, cbmFaces);

   TFastCollisionChecker = function (obj1, obj2 : TGLBaseSceneObject) : Boolean;
   PFastCollisionChecker = ^TFastCollisionChecker;

	// TGLCollisionManager
	//
	TGLCollisionManager = class (TComponent)
	   private
	       
         FClients : TList;
         FOnCollision : TObjectCollisionEvent;

	   protected
	       
	      procedure RegisterClient(aClient : TGLBCollision);
	      procedure DeRegisterClient(aClient : TGLBCollision);
	      procedure DeRegisterAllClients;

	   public
	       
	      constructor Create(AOwner: TComponent); override;
         destructor Destroy; override;

	      procedure CheckCollisions;

		published
			 
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
			 
         FBoundingMode : TCollisionBoundingMode;
         FManager : TGLCollisionManager;
         FManagerName : String; // NOT persistent, temporarily used for persistence
         FGroupIndex : Integer;

		protected
			 
         procedure SetGroupIndex(const value : Integer);
         procedure SetManager(const val : TGLCollisionManager);

			procedure WriteToFiler(writer : TWriter); override;
         procedure ReadFromFiler(reader : TReader); override;
         procedure Loaded; override;

		public
			 
			constructor Create(aOwner : TGLXCollection); override;
			destructor Destroy; override;

         procedure Assign(Source: TPersistent); override;

			class function FriendlyName : String; override;
			class function FriendlyDescription : String; override;

		published
			 
         {: Refers the collision manager. }
         property Manager : TGLCollisionManager read FManager write SetManager;
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
function FastCheckCubeVsFace(obj1, obj2 : TGLBaseSceneObject) : Boolean;   //experimental
function FastCheckFaceVsCube(obj1, obj2 : TGLBaseSceneObject) : Boolean;   //experimental
function FastCheckFaceVsFace(obj1, obj2 : TGLBaseSceneObject) : Boolean;

{: Returns true when the bounding box cubes does intersect the other.<p>
   Also true when the one cube does contain the other completely. }
function IntersectCubes(obj1, obj2 : TGLBaseSceneObject) : Boolean; overload;

{: Returns or creates the TGLBCollision within the given behaviours.<p>
	This helper function is convenient way to access a TGLBCollision. }
function GetOrCreateCollision(behaviours : TGLBehaviours) : TGLBCollision; overload;
{: Returns or creates the TGLBCollision within the given object's behaviours.<p>
	This helper function is convenient way to access a TGLBCollision. }
function GetOrCreateCollision(obj : TGLBaseSceneObject) : TGLBCollision; overload;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

const
   cEpsilon : Single = 1e-6;

const
   cFastCollisionChecker : array [cbmPoint..cbmFaces, cbmPoint..cbmFaces] of TFastCollisionChecker = (
       (FastCheckPointVsPoint,      FastCheckPointVsSphere,       FastCheckPointVsEllipsoid,       FastCheckPointVsCube,      FastCheckPointVsCube),
       (FastCheckSphereVsPoint,     FastCheckSphereVsSphere,      FastCheckSphereVsEllipsoid,      FastCheckSphereVsCube,     FastCheckSphereVsCube),
       (FastCheckEllipsoidVsPoint,  FastCheckEllipsoidVsSphere,   FastCheckEllipsoidVsEllipsoid,   FastCheckEllipsoidVsCube,  FastCheckEllipsoidVsCube),
       (FastCheckCubeVsPoint,       FastCheckCubeVsSphere,        FastCheckCubeVsEllipsoid,        FastCheckCubeVsCube,       FastCheckCubeVsFace),
       (FastCheckCubeVsPoint,       FastCheckCubeVsSphere,        FastCheckCubeVsEllipsoid,        FastCheckFaceVsCube,       FastCheckFaceVsFace)
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
//   DivideVector(v, obj2.Scale.AsVector);  //DanB - Scale removed in VectorTransform
   DivideVector(v, obj2.AxisAlignedDimensionsUnscaled);
//   ScaleVector(v,obj2.Scale.AsVector);
//   ScaleVector();
   v.V[3]:=0;
   // if norm is below 1, collision
   Result:=(VectorNorm(v)<=1{Sqr(obj2.BoundingSphereRadius)});  //DanB - since radius*radius = 1/2*1/2 = 1/4 for unit sphere
end;

function FastCheckPointVsCube(obj1, obj2 : TGLBaseSceneObject) : Boolean;
var
   v : TVector;
begin
   // calc vector expressed in local coordinates (for obj2)
   v:=VectorTransform(obj1.AbsolutePosition, obj2.InvAbsoluteMatrix);
   // rescale to unit dimensions
   DivideVector(v, obj2.AxisAlignedDimensionsUnscaled);
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
      ScaleVector(v,obj2.Scale.AsVector);  //by DanB
   v.V[3]:=0;
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
   v.V[0] := abs(v.V[0]);
   v.V[1] := abs(v.V[1]);
   v.V[2] := abs(v.V[2]);
   ScaleVector(v,obj2.Scale.AsVector);  //by DanB

   aad := obj2.AxisAlignedDimensions; // should be abs at all!

   VectorSubtract(v, aad, v); // v holds the distance in each axis
   v.V[3] := 0;

   r := obj1.BoundingSphereRadius{UnScaled};
   r2 := Sqr(r);
   if (v.V[0]>0) then begin
     if (v.V[1]>0) then begin
       if (v.V[2]>0) then begin
         // v is outside axis parallel projection, so use distance to edge point
         result := (VectorNorm(v)<=r2);
       end else begin
         // v is inside z axis projection, but outside x-y projection
         result := (VectorNorm(v.V[0],v.V[1])<=r2);
       end
     end else begin
       if (v.V[2]>0) then begin
         // v is inside y axis projection, but outside x-z projection
         result := (VectorNorm(v.V[0],v.V[2])<=r2);
       end else begin
         // v is inside y-z axis projection, but outside x projection
         result := (v.V[0]<=r);
       end
     end
   end else begin
     if (v.V[1]>0) then begin
       if (v.V[2]>0) then begin
         // v is inside x axis projection, but outside y-z projection
         result := (VectorNorm(v.V[1],v.V[2])<=r2);
       end else begin
         // v is inside x-z projection, but outside y projection
         result := (v.V[1]<=r);
       end
     end else begin
       if (v.V[2]>0) then begin
         // v is inside x-y axis projection, but outside z projection
         result := (v.V[2]<=r);
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
   v1.V[3]:=0;
   // express in local coordinates (for obj1)
   v2:=VectorTransform(obj2.AbsolutePosition, obj1.InvAbsoluteMatrix);
   // calc local vector, and rescale to unit dimensions
   //VectorSubstract(pt, obj1.AbsolutePosition, v2);
   DivideVector(v2, obj1.AxisAlignedDimensions);
   v2.V[3]:=0;
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
   aad:=VectorAdd(obj2.AxisAlignedDimensionsUnscaled, obj1.BoundingSphereRadius);
   DivideVector(v, aad);
   v.V[3]:=0;
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
  pt[0] := VectorMake(-v.V[0],-v.V[1],-v.V[2],1);
  pt[1] := VectorMake( v.V[0],-v.V[1],-v.V[2],1);
  pt[2] := VectorMake( v.V[0], v.V[1],-v.V[2],1);
  pt[3] := VectorMake(-v.V[0], v.V[1],-v.V[2],1);
  pt[4] := VectorMake(-v.V[0],-v.V[1], v.V[2],1);
  pt[5] := VectorMake( v.V[0],-v.V[1], v.V[2],1);
  pt[6] := VectorMake( v.V[0], v.V[1], v.V[2],1);
  pt[7] := VectorMake(-v.V[0], v.V[1], v.V[2],1);
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
      if d.V[i]=0 then begin       // wire is parallel to plane
        // this case will be handled by the other planes
      end else begin
        j := (i+1) mod 3;
        k := (j+1) mod 3;
        t := (pl.V[i]-p0.V[i])/d.V[i];   // t: line parameter of intersection
        if IsInRange(t, 0, 1) then begin
          s := p0;
          CombineVector(s,d,t);    // calculate intersection
          // if the other two coordinates lie within the ranges, collision
          if IsInRange(s.V[j],-pl.V[j],pl.V[j]) and
             IsInRange(s.V[k],-pl.V[k],pl.V[k]) then Exit;
        end;
        t := (-pl.V[i]-p0.V[i])/d.V[i];   // t: parameter of intersection
        if IsInRange(t,0,1) then begin
          s := p0;
          CombineVector(s,d,t);    // calculate intersection
          // if the other two coordinates lie within the ranges, collision
          if IsInRange(s.V[j],-pl.V[j],pl.V[j]) and
          IsInRange(s.V[k],-pl.V[k],pl.V[k]) then Exit;
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
  aad := obj2.AxisAlignedDimensionsUnscaled; //DanB experiment
  InitArray(obj1.AxisAlignedDimensionsUnscaled,pt1);
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
{var
  aad1,aad2 : TVector;
  D1,D2,D : Double;
}
begin
//DanB -this bit of code isn't needed (since collision code does BoundingBox elimination)
//also is incorrect when objects further up the "object tree" are scaled
{
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
}
//DanB

      result := DoCubesIntersectPrim(obj1,obj2) or
                DoCubesIntersectPrim(obj2,obj1);
{    end;
  end;
}
end;


//  FastCheckCubeVsFace
//
//  Behaviour - Checks for collisions between Faces and cube by Checking
//              whether triangles on the mesh have a point inside the cube,
//              or a triangle intersects the side
//
//  Issues -  Checks whether triangles on the mesh have a point inside the cube
//            1)  When the cube is completely inside a mesh, it will contain
//               no triangles hence no collision detected
//            2)  When the mesh is (almost) completely inside the cube
//               Octree.GetTrianglesInCube returns no points, why?
//

function FastCheckCubeVsFace(obj1, obj2 : TGLBaseSceneObject) : Boolean;
//var
//   triList : TAffineVectorList;
//   m1to2, m2to1 : TMatrix;
//   i:integer;
begin
   if (obj2 is TGLFreeForm) then begin
      //check if we are initialized correctly
      if not Assigned(TGLFreeForm(obj2).Octree) then
         TGLFreeForm(obj2).BuildOctree;

      Result:=TGLFreeForm(obj2).OctreeAABBIntersect(obj1.AxisAlignedBoundingBoxUnscaled,obj1.AbsoluteMatrix,obj1.InvAbsoluteMatrix)
      //could then analyse triangles and return contact points
   end else begin
      // CubeVsFace only works if one is FreeForm Object
      Result:=IntersectCubes(obj1, obj2);
   end;
end;

function FastCheckFaceVsCube(obj1, obj2 : TGLBaseSceneObject) : Boolean;
begin
  Result:=FastCheckCubeVsFace(obj2,obj1);
end;



//this function does not check for rounds that results from Smoth rendering
//if anybody needs this, you are welcome to show a solution, but usually this should be good enough
function FastCheckFaceVsFace(obj1, obj2 : TGLBaseSceneObject) : Boolean;
type
   TTriangle = array [0..2] of TAffineVector;
   PTriangle = ^TTriangle;

var
   i : Integer;
   triList : TAffineVectorList;
   tri : PTriangle;
   m1to2, m2to1 : TMatrix;
   AABB2:TAABB;
begin
   result:= false;
   if (obj1 is TGLFreeForm) and (obj2 is TGLFreeForm) then begin
      //check if we are initialized correctly
      if not Assigned(TGLFreeForm(obj1).Octree) then
         TGLFreeForm(obj1).BuildOctree;
      if not Assigned(TGLFreeForm(obj2).Octree) then
         TGLFreeForm(obj2).BuildOctree;

      // Check triangles against the other object
      // check only the one that are near the destination object (using octree of obj1)
      // get the 'hot' ones using the tree

      MatrixMultiply(obj2.AbsoluteMatrix, obj1.InvAbsoluteMatrix, m1to2);
      MatrixMultiply(obj1.AbsoluteMatrix, obj2.InvAbsoluteMatrix, m2to1);

      AABB2:=obj2.AxisAlignedBoundingBoxUnscaled;
      triList:=TGLFreeForm(obj1).Octree.GetTrianglesFromNodesIntersectingCube(AABB2, m1to2, m2to1);

      //in the list originally are the local coords, TransformAsPoints-> now we have obj1 absolute coords
      triList.TransformAsPoints(obj1.AbsoluteMatrix); //Transform to Absolute Coords
      try
         i:=0;
         while i<TriList.Count-2 do begin
            // here we pass absolute coords, then these are transformed with Obj2's InvAbsoluteMatrix to match the local Obj2 System
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
   end else begin
      // FaceVsFace does work only for two FreeForm Objects
      Result:=IntersectCubes(obj1, obj2);
   end;
end;

// IntersectCubes (objects)
//
function IntersectCubes(obj1, obj2 : TGLBaseSceneObject) : Boolean;
var
  aabb1, aabb2 : TAABB;
  m1To2, m2To1 : TMatrix;
begin
   // Calc AABBs
   aabb1:=Obj1.AxisAlignedBoundingBoxUnscaled;
   aabb2:=Obj2.AxisAlignedBoundingBoxUnscaled;

   // Calc Conversion Matrixes
   MatrixMultiply(obj1.AbsoluteMatrix, obj2.InvAbsoluteMatrix, m1To2);
   MatrixMultiply(obj2.AbsoluteMatrix, obj1.InvAbsoluteMatrix, m2To1);

   Result:=IntersectAABBs(aabb1, aabb2, m1To2, m2To1);
end;

// ------------------
// ------------------ TCollisionManager ------------------
// ------------------

// Create
//
constructor TGLCollisionManager.Create(AOwner: TComponent);
begin
	inherited Create(AOwner);
   FClients:=TList.Create;
   RegisterManager(Self);
end;

// Destroy
//
destructor TGLCollisionManager.Destroy;
begin
	DeRegisterAllClients;
   DeRegisterManager(Self);
   FClients.Free;
	inherited Destroy;
end;

// RegisterClient
//
procedure TGLCollisionManager.RegisterClient(aClient : TGLBCollision);
begin
   if Assigned(aClient) then
      if FClients.IndexOf(aClient)<0 then begin
         FClients.Add(aClient);
         aClient.FManager:=Self;
      end;
end;

// DeRegisterClient
//
procedure TGLCollisionManager.DeRegisterClient(aClient : TGLBCollision);
begin
   if Assigned(aClient) then begin
      aClient.FManager:=nil;
      FClients.Remove(aClient);
   end;
end;

// DeRegisterAllClients
//
procedure TGLCollisionManager.DeRegisterAllClients;
var
   i : Integer;
begin
   // Fast deregistration
   for i:=0 to FClients.Count-1 do
      TGLBCollision(FClients[i]).FManager:=nil;
   FClients.Clear;
end;

// Reference code
{
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
   // TODO : speed improvements & distance cacheing
   for i:=0 to FClients.Count-2 do begin
      cli1:=TGLBCollision(FClients[i]);
      obj1:=cli1.OwnerBaseSceneObject;
      grp1:=cli1.GroupIndex;
      for j:=i+1 to FClients.Count-1 do begin
         cli2:=TGLBCollision(FClients[j]);
         obj2:=cli2.OwnerBaseSceneObject;
         grp2:=cli2.GroupIndex;
         // if either one GroupIndex=0 or both are different, check for collision
         if ((grp1=0) or (grp2=0) or (grp1<>grp2)) then begin
           if cFastCollisionChecker[cli1.BoundingMode, cli2.BoundingMode](obj1, obj2) then
              FOnCollision(Self, obj1, obj2);
         end;
      end;
   end;
end;

}


// [---- new CheckCollisions / Dan Bartlett

// CheckCollisions  (By Dan Bartlett) - sort according to Z axis
//
//  Some comments:  Much faster than original, especially when objects are spread out.
//                  TODO:
//                  Try to make faster when objects are close
//                  Still more improvements can be made, better method (dynamic octree?)
//                  Faster sorting? (If a faster way than Delphi's QuickSort is available)
//                  Another Event called OnNoCollisionEvent could be added
//                  Fit bounding box methods into GLScene "Grand Scheme Of Things"
//
//  Behaviour:
//   If GroupIndex < 0 then it will not be checked for collisions against
//     any other object *** WARNING:  THIS IS DIFFERENT FROM PREVIOUS VERSION ***
//
//   If GroupIndex = 0 then object will be tested against all objects with GroupIndex >= 0
//   Collision Testing will only be performed on objects from different groups
//   Collision testing occurs even when an object is not visible, allowing low-triangle count
//      collision shapes to be used to model complex objects (Different to previous version)

type
   //only add collision node to list if GroupIndex>=0
   TCollisionNode = class
     public
      Collision:TGLBCollision;
      AABB:TAABB;
      constructor Create(Collision:TGLBCollision; AABB:TAABB);
   end;

constructor TCollisionNode.Create(Collision:TGLBCollision; AABB:TAABB);
begin
  inherited Create();
  Self.Collision:=Collision;
  Self.AABB:=AABB;
end;

   function CompareDistance(Item1, Item2: Pointer): Integer;
   var
      d : Extended;
   begin
      //  Z-axis sort
      d:=(TCollisionNode(Item2).AABB.min.V[2]-TCollisionNode(Item1).AABB.min.V[2]);
      if d>0 then Result:=-1 else if d<0 then Result:=1 else Result:=0;
   end;

procedure TGLCollisionManager.CheckCollisions;
var
  NodeList:TList;
  CollisionNode1, CollisionNode2:TCollisionNode;
  obj1, obj2 : TGLBaseSceneObject;
  cli1, cli2 : TGLBCollision;
  grp1, grp2 : Integer;   // GroupIndex of collisions
  i, j : Integer;
  box1:TAABB;
begin
  if not Assigned(FOnCollision) then Exit;

  //this next bit of code would be faster if bounding box was stored
  NodeList:=TList.Create;

  try
     NodeList.Count:=0;

     for i:=0 to FClients.Count-1 do begin
       cli1:=TGLBCollision(FClients[i]);
       grp1:=cli1.GroupIndex;
       if grp1<0 then        //if groupindex is negative don't add to list
         Continue;
       obj1:=cli1.OwnerBaseSceneObject;
       //TODO:  need to do different things for different objects, especially points (to improve speed)
       box1:=obj1.AxisAlignedBoundingBoxUnscaled;         //get obj1 axis-aligned bounding box
       if box1.min.V[2]>=box1.max.V[2] then continue;          //check for case where no bb exists
       AABBTransform(box1,obj1.AbsoluteMatrix);           //& transform it to world axis
       CollisionNode1:=TCollisionNode.Create(cli1,box1);
       NodeList.Add(CollisionNode1);
     end;

     if NodeList.Count<2 then Exit;
     NodeList.Sort(@CompareDistance);       //depth-sort bounding boxes (min bb.z values)

     for i:=0 to NodeList.Count-2 do begin
        CollisionNode1:=TCollisionNode(NodeList[i]);
        cli1:=CollisionNode1.Collision;
        grp1:=cli1.GroupIndex;

        for j:=i+1 to NodeList.Count-1 do begin
          CollisionNode2:=TCollisionNode(NodeList[j]);
          cli2:=CollisionNode2.Collision;

          //Check BBox1 and BBox2 overlap in the z-direction
          if (CollisionNode2.AABB.min.V[2]>CollisionNode1.AABB.max.V[2]) then
            Break;

          grp2:=cli2.GroupIndex;

          // if either one GroupIndex=0 or both are different, check for collision
          if ((grp1=0) or (grp2=0) or (grp1<>grp2))=false then Continue;

          //check whether box1 and box2 overlap in the XY Plane
          if IntersectAABBsAbsoluteXY(CollisionNode1.AABB,CollisionNode2.AABB) then begin
           obj1:=cli1.OwnerBaseSceneObject;
           obj2:=cli2.OwnerBaseSceneObject;
            if cFastCollisionChecker[cli1.BoundingMode, cli2.BoundingMode](obj1, obj2) then
              FOnCollision(Self, obj1, obj2);
          end;
        end;
      end;

   finally
      for i:=0 to NodeList.Count-1 do
      begin
        CollisionNode1 := NodeList.Items[i];
        CollisionNode1.Free;
      end;

      NodeList.Free;
  end;
end;

// new CheckCollisions / Dan Bartlett -----]



// ------------------
// ------------------ TGLBCollision ------------------
// ------------------

// Create
//
constructor TGLBCollision.Create(aOwner : TGLXCollection);
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
      // ArchiveVersion 1, added FGroupIndex
      // ArchiveVersion 2, added inherited call
      WriteInteger(2);
      inherited;
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
      Assert(archiveVersion in [0..2]);
      if archiveVersion>=2 then
        inherited;
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
      mng:=FindManager(TGLCollisionManager, FManagerName);
      if Assigned(mng) then
         Manager:=TGLCollisionManager(mng);
      FManagerName:='';
   end;
end;

 
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
procedure TGLBCollision.SetManager(const val : TGLCollisionManager);
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

// GetOrCreateCollision (TGLBehaviours)
//
function GetOrCreateCollision(behaviours : TGLBehaviours) : TGLBCollision;
var
	i : Integer;
begin
	i:=behaviours.IndexOfClass(TGLBCollision);
	if i>=0 then
		Result:=TGLBCollision(behaviours[i])
	else Result:=TGLBCollision.Create(behaviours);
end;

// GetOrCreateCollision (TGLBaseSceneObject)
//
function GetOrCreateCollision(obj : TGLBaseSceneObject) : TGLBCollision;
begin
	Result:=GetOrCreateCollision(obj.Behaviours);
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

finalization

	UnregisterXCollectionItemClass(TGLBCollision);

end.

