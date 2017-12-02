//
// VXScene Component Library, based on GLScene http://glscene.sourceforge.net
//
{
  Spatial partitioning related code that also uses scene objects
}

unit VXS.SpatialPartitioning;

interface

uses
  Winapi.OpenGL,
  Winapi.OpenGLext,

  VXS.Scene,
  VXS.Coordinates,
  VXS.Win64Viewer,
  VXS.SpacePartition,
  VXS.VectorGeometry,
  VXS.GeometryBB,
  VXS.RenderContextInfo,
  VXS.State;

type
  { Object for holding scene objects in a spatial partitioning }
  TVXSceneObj = class(TSpacePartitionLeaf)
  public
    Obj: TVXBaseSceneObject;
    procedure UpdateCachedAABBAndBSphere; override;
    constructor CreateObj(Owner: TSectoredSpacePartition; aObj: TVXBaseSceneObject);
    destructor Destroy; override;
  end;

  { Render a spacial partitioning descending from TSectoredSpacePartition
    (octree and quadtree) as a grid - great for debugging and visualisation }
procedure RenderSpatialPartitioning(var rci: TVXRenderContextInfo; const Space: TSectoredSpacePartition);

{ Create an extended frustum from a SceneViewer - this makes the unit
  specific to the windows platform! }
function ExtendedFrustumMakeFromSceneViewer(const AFrustum: TFrustum; const vWidth, vHeight: integer; AVKCamera: TVXCamera)
  : TExtendedFrustum; overload;

function ExtendedFrustumMakeFromSceneViewer(const AFrustum: TFrustum; const AVXSceneViewer: TVXSceneViewer)
  : TExtendedFrustum; overload;

{ Renders an AABB as a line }
procedure RenderAABB(var rci: TVXRenderContextInfo; AABB: TAABB; w, r, g, b: single); overload;
procedure RenderAABB(var rci: TVXRenderContextInfo; AABB: TAABB); overload;

// -------------------------------------------------------------------
// -------------------------------------------------------------------
// -------------------------------------------------------------------
implementation

// -------------------------------------------------------------------
// -------------------------------------------------------------------
// -------------------------------------------------------------------

uses
  VXS.VectorTypes,
  VXS.Context;

procedure RenderAABB(var rci: TVXRenderContextInfo; AABB: TAABB);
begin
  RenderAABB(rci, AABB, 1, 0.8, 0.8, 0.8);
end;

procedure RenderAABB(var rci: TVXRenderContextInfo; AABB: TAABB; w, r, g, b: single);
begin
  glColor3f(r, g, b);
  rci.VXStates.LineWidth := w;

  glBegin(GL_LINE_STRIP);
  glVertex3f(AABB.min.X, AABB.min.Y, AABB.min.Z);
  glVertex3f(AABB.min.X, AABB.max.Y, AABB.min.Z);
  glVertex3f(AABB.max.X, AABB.max.Y, AABB.min.Z);
  glVertex3f(AABB.max.X, AABB.min.Y, AABB.min.Z);
  glVertex3f(AABB.min.X, AABB.min.Y, AABB.min.Z);

  glVertex3f(AABB.min.X, AABB.min.Y, AABB.max.Z);
  glVertex3f(AABB.min.X, AABB.max.Y, AABB.max.Z);
  glVertex3f(AABB.max.X, AABB.max.Y, AABB.max.Z);
  glVertex3f(AABB.max.X, AABB.min.Y, AABB.max.Z);
  glVertex3f(AABB.min.X, AABB.min.Y, AABB.max.Z);
  glEnd;

  glBegin(GL_LINES);
  glVertex3f(AABB.min.X, AABB.max.Y, AABB.min.Z);
  glVertex3f(AABB.min.X, AABB.max.Y, AABB.max.Z);

  glVertex3f(AABB.max.X, AABB.max.Y, AABB.min.Z);
  glVertex3f(AABB.max.X, AABB.max.Y, AABB.max.Z);

  glVertex3f(AABB.max.X, AABB.min.Y, AABB.min.Z);
  glVertex3f(AABB.max.X, AABB.min.Y, AABB.max.Z);
  glEnd;
end;

procedure RenderSpatialPartitioning(var rci: TVXRenderContextInfo; const Space: TSectoredSpacePartition);

  procedure RenderSectorNode(Node: TSectorNode);
  var
    i: integer;
    AABB: TAABB;
  begin
    if Node.NoChildren then
    begin
      AABB := Node.AABB;
      if Node.RecursiveLeafCount > 0 then
        RenderAABB(rci, AABB, 1, 0, 0, 0)
      else
        RenderAABB(rci, AABB, 1, 0.8, 0.8, 0.8) // }
    end
    else
    begin
      for i := 0 to Node.ChildCount - 1 do
        RenderSectorNode(Node.Children[i]);
    end;
  end;

begin
  rci.VXStates.Disable(stLighting);
  RenderSectorNode(Space.RootNode);
end;

function ExtendedFrustumMakeFromSceneViewer(const AFrustum: TFrustum; const AVXSceneViewer: TVXSceneViewer): TExtendedFrustum;
// old version
begin
  Assert(Assigned(AVXSceneViewer.Camera), 'VXSceneViewer must have camera specified!');
  result := ExtendedFrustumMake(AFrustum, AVXSceneViewer.Camera.NearPlane, AVXSceneViewer.Camera.DepthOfView,
    AVXSceneViewer.FieldOfView, AVXSceneViewer.Camera.Position.AsAffineVector, AVXSceneViewer.Camera.Direction.AsAffineVector);
end;

function ExtendedFrustumMakeFromSceneViewer(const AFrustum: TFrustum; const vWidth, vHeight: integer; AVKCamera: TVXCamera)
  : TExtendedFrustum; // changed version
var
  buffov: single;
begin
  if vWidth < vHeight then
    buffov := AVKCamera.GetFieldOfView(vWidth)
  else
    buffov := AVKCamera.GetFieldOfView(vHeight);
  result := ExtendedFrustumMake(AFrustum, AVKCamera.NearPlane, AVKCamera.DepthOfView, buffov, AVKCamera.Position.AsAffineVector,
    AVKCamera.Direction.AsAffineVector);
end;

// --------- TVXSceneObj ------------
constructor TVXSceneObj.CreateObj(Owner: TSectoredSpacePartition; aObj: TVXBaseSceneObject);
begin
  Obj := aObj;
  inherited CreateOwned(Owner);
end;

destructor TVXSceneObj.Destroy;
begin
  inherited;
end;

procedure TVXSceneObj.UpdateCachedAABBAndBSphere;
begin
  FCachedAABB := Obj.AxisAlignedBoundingBox;
  FCachedAABB.min := Obj.LocalToAbsolute(FCachedAABB.min);
  FCachedAABB.max := Obj.LocalToAbsolute(FCachedAABB.max);
  FCachedBSphere.Radius := Obj.BoundingSphereRadius;
  FCachedBSphere.Center := AffineVectorMake(Obj.AbsolutePosition);
end;

end.
