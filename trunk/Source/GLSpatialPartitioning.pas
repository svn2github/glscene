{: GLSpatialPartitioning<p>

  <p>Spatial partitioning related code that also uses GLScene objects

	<b>History : </b><font size=-1><ul>
      <li>03/12/04 - MF - Created
  </ul></font>
}

unit GLSpatialPartitioning;

interface

uses
  SpatialPartitioning, GLScene, VectorGeometry, OpenGL1x, GeometryBB;

type
  {: Object for holding glscene objects in a spatial partitioning }
  TSceneObj = class(TSpacePartitionLeaf)
  public
    Obj : TGLBaseSceneObject;
    procedure UpdateCachedAABBAndBSphere; override;
    constructor CreateObj(Owner: TSectoredSpacePartition; aObj : TGLBaseSceneObject);
    destructor Destroy; override;
  end;

  {: Render a spacial partitioning descending from TSectoredSpacePartition
  (octree and quadtree) as a grid - great for debugging and visualisation }
  procedure RenderSpatialPartitioning(const Space : TSectoredSpacePartition);

implementation

procedure RenderSpatialPartitioning(const Space : TSectoredSpacePartition);

  procedure RenderAABB(AABB : TAABB; w, r,g,b : single);
  begin
    glColor3f(r,g,b);
    glLineWidth(w);

    glBegin(GL_LINE_STRIP);
      glVertex3f(AABB.min[0],AABB.min[1], AABB.min[2]);
      glVertex3f(AABB.min[0],AABB.max[1], AABB.min[2]);
      glVertex3f(AABB.max[0],AABB.max[1], AABB.min[2]);
      glVertex3f(AABB.max[0],AABB.min[1], AABB.min[2]);
      glVertex3f(AABB.min[0],AABB.min[1], AABB.min[2]);

      glVertex3f(AABB.min[0],AABB.min[1], AABB.max[2]);
      glVertex3f(AABB.min[0],AABB.max[1], AABB.max[2]);
      glVertex3f(AABB.max[0],AABB.max[1], AABB.max[2]);
      glVertex3f(AABB.max[0],AABB.min[1], AABB.max[2]);
      glVertex3f(AABB.min[0],AABB.min[1], AABB.max[2]);
    glEnd;

    glBegin(GL_LINES);
      glVertex3f(AABB.min[0],AABB.max[1], AABB.min[2]);
      glVertex3f(AABB.min[0],AABB.max[1], AABB.max[2]);

      glVertex3f(AABB.max[0],AABB.max[1], AABB.min[2]);
      glVertex3f(AABB.max[0],AABB.max[1], AABB.max[2]);

      glVertex3f(AABB.max[0],AABB.min[1], AABB.min[2]);
      glVertex3f(AABB.max[0],AABB.min[1], AABB.max[2]);
    glEnd;
  end;

  procedure RenderSectorNode(Node : TSectorNode);
  var
    i : integer;
    AABB : TAABB;
  begin
    if Node.NoChildren then
    begin
      AABB := Node.AABB;

      if Node.RecursiveLeafCount > 0 then
        RenderAABB(AABB, 1, 0, 0, 0)
      else
        RenderAABB(AABB, 1, 0.8, 0.8, 0.8)//}

    end else
    begin
      for i := 0 to Node.ChildCount-1 do
        RenderSectorNode(Node.Children[i]);
    end;
  end;
begin
  glPushAttrib(GL_ENABLE_BIT or GL_CURRENT_BIT or GL_LINE_BIT or GL_COLOR_BUFFER_BIT);
  glDisable(GL_LIGHTING);
  RenderSectorNode(Space.RootNode);
  glPopAttrib;
end;

{ TSceneObj }

constructor TSceneObj.CreateObj(Owner: TSectoredSpacePartition; aObj : TGLBaseSceneObject);
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
