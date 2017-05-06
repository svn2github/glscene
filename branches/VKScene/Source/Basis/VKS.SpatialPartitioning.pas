//
// VKScene Component Library, based on GLScene http://glscene.sourceforge.net 
//
{
   Spatial partitioning related code that also uses scene objects
}

unit VKS.SpatialPartitioning;

interface

uses
  Winapi.OpenGL,
  Winapi.OpenGLext,
  VKS.Scene,
  VKS.Win64Viewer,
  VKS.SpacePartition,
  VKS.VectorGeometry,
  VKS.GeometryBB,
  VKS.RenderContextInfo,
  VKS.State;

type
  { Object for holding scene objects in a spatial partitioning }
  TVKSceneObj = class(TSpacePartitionLeaf)
  public
    Obj: TVKBaseSceneObject;
    procedure UpdateCachedAABBAndBSphere; override;
    constructor CreateObj(Owner: TSectoredSpacePartition; aObj: TVKBaseSceneObject);
    destructor Destroy; override;
  end;

{ Render a spacial partitioning descending from TSectoredSpacePartition
  (octree and quadtree) as a grid - great for debugging and visualisation }
procedure RenderSpatialPartitioning(var rci: TVKRenderContextInfo;
  const Space: TSectoredSpacePartition);

{ Create an extended frustum from a SceneViewer - this makes the unit
specific to the windows platform!}
function ExtendedFrustumMakeFromSceneViewer(const AFrustum: TFrustum;
  const vWidth, vHeight: integer; AVKCamera: TVKCamera): TExtendedFrustum; overload;

function ExtendedFrustumMakeFromSceneViewer(const AFrustum: TFrustum;
  const AVKSceneViewer: TVKSceneViewer): TExtendedFrustum; overload;

{ Renders an AABB as a line }
procedure RenderAABB(var rci: TVKRenderContextInfo; AABB: TAABB; w, r, g, b: single); overload;
procedure RenderAABB(var rci: TVKRenderContextInfo; AABB: TAABB); overload;

//-------------------------------------------------------------------
//-------------------------------------------------------------------
//-------------------------------------------------------------------
implementation
//-------------------------------------------------------------------
//-------------------------------------------------------------------
//-------------------------------------------------------------------

uses
  VKS.VectorTypes,
  VKS.Context;

procedure RenderAABB(var rci: TVKRenderContextInfo; AABB: TAABB);
begin
  RenderAABB(rci, AABB, 1, 0.8, 0.8, 0.8);
end;

procedure RenderAABB(var rci: TVKRenderContextInfo; AABB: TAABB; w, r, g, b: single);
begin
  glColor3f(r, g, b);
  rci.VKStates.LineWidth := w;

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

procedure RenderSpatialPartitioning(var rci: TVKRenderContextInfo;
  const Space: TSectoredSpacePartition);

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
        RenderAABB(rci, AABB, 1, 0.8, 0.8, 0.8) //}
    end
    else
    begin
      for i := 0 to Node.ChildCount - 1 do
        RenderSectorNode(Node.Children[i]);
    end;
  end;
begin
  rci.VKStates.Disable(stLighting);
  RenderSectorNode(Space.RootNode);
end;

function ExtendedFrustumMakeFromSceneViewer(const AFrustum: TFrustum;
  const AVKSceneViewer: TVKSceneViewer): TExtendedFrustum; //old version
begin
  Assert(Assigned(AVKSceneViewer.Camera), 'VKSceneViewer must have camera specified!');
  result := ExtendedFrustumMake(AFrustum,
    AVKSceneViewer.Camera.NearPlane,
    AVKSceneViewer.Camera.DepthOfView,
    AVKSceneViewer.FieldOfView,
    AVKSceneViewer.Camera.Position.AsAffineVector,
    AVKSceneViewer.Camera.Direction.AsAffineVector);
end;

function ExtendedFrustumMakeFromSceneViewer(const AFrustum: TFrustum;
  const vWidth, vHeight: integer; AVKCamera: TVKCamera): TExtendedFrustum; //changed version
var
  buffov: single;
begin
  if vWidth < vHeight then
    buffov := AVKCamera.GetFieldOfView(vWidth)
  else
    buffov := AVKCamera.GetFieldOfView(vHeight);
  result := ExtendedFrustumMake(AFrustum,
    AVKCamera.NearPlane,
    AVKCamera.DepthOfView,
    buffov,
    AVKCamera.Position.AsAffineVector,
    AVKCamera.Direction.AsAffineVector);
end;

//--------- TVKSceneObj ------------
constructor TVKSceneObj.CreateObj(Owner: TSectoredSpacePartition; aObj: TVKBaseSceneObject);
begin
  Obj := aObj;
  inherited CreateOwned(Owner);
end;

destructor TVKSceneObj.Destroy;
begin
  inherited;
end;

procedure TVKSceneObj.UpdateCachedAABBAndBSphere;
begin
  FCachedAABB := Obj.AxisAlignedBoundingBox;
  FCachedAABB.min := Obj.LocalToAbsolute(FCachedAABB.min);
  FCachedAABB.max := Obj.LocalToAbsolute(FCachedAABB.max);
  FCachedBSphere.Radius := Obj.BoundingSphereRadius;
  FCachedBSphere.Center := AffineVectorMake(Obj.AbsolutePosition);
end;
end.

