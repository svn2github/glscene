//
// VKScene project, http://glscene.sourceforge.net 
//
{
   Spatial partitioning related code that also uses GLScene objects
   
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
  { Object for holding glscene objects in a spatial partitioning }
  TSceneObj = class(TSpacePartitionLeaf)
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

{ Create an extended frustum from a GLSceneViewer - this makes the unit
specific to the windows platform!}
function ExtendedFrustumMakeFromSceneViewer(const AFrustum: TFrustum;
  const vWidth, vHeight: integer; AGLCamera: TVKCamera): TExtendedFrustum; overload;

function ExtendedFrustumMakeFromSceneViewer(const AFrustum: TFrustum;
  const AGLSceneViewer: TVKSceneViewer): TExtendedFrustum; overload;

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

// RenderSpatialPartitioning
//
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
  const AGLSceneViewer: TVKSceneViewer): TExtendedFrustum; //old version
begin
  Assert(Assigned(AGLSceneViewer.Camera), 'GLSceneViewer must have camera specified!');
  result := ExtendedFrustumMake(AFrustum,
    AGLSceneViewer.Camera.NearPlane,
    AGLSceneViewer.Camera.DepthOfView,
    AGLSceneViewer.FieldOfView,
    AGLSceneViewer.Camera.Position.AsAffineVector,
    AGLSceneViewer.Camera.Direction.AsAffineVector);
end;

function ExtendedFrustumMakeFromSceneViewer(const AFrustum: TFrustum;
  const vWidth, vHeight: integer; AGLCamera: TVKCamera): TExtendedFrustum; //changed version
var
  buffov: single;
begin
  if vWidth < vHeight then
    buffov := AGLCamera.GetFieldOfView(vWidth)
  else
    buffov := AGLCamera.GetFieldOfView(vHeight);
  result := ExtendedFrustumMake(AFrustum,
    AGLCamera.NearPlane,
    AGLCamera.DepthOfView,
    buffov,
    AGLCamera.Position.AsAffineVector,
    AGLCamera.Direction.AsAffineVector);
end;

{ TSceneObj }

constructor TSceneObj.CreateObj(Owner: TSectoredSpacePartition; aObj: TVKBaseSceneObject);
begin
  Obj := aObj;
  inherited CreateOwned(Owner);
end;

destructor TSceneObj.Destroy;
begin
  inherited;
end;

procedure TSceneObj.UpdateCachedAABBAndBSphere;
begin
  FCachedAABB := Obj.AxisAlignedBoundingBox;
  FCachedAABB.min := Obj.LocalToAbsolute(FCachedAABB.min);
  FCachedAABB.max := Obj.LocalToAbsolute(FCachedAABB.max);
  FCachedBSphere.Radius := Obj.BoundingSphereRadius;
  FCachedBSphere.Center := AffineVectorMake(Obj.AbsolutePosition);
end;
end.

