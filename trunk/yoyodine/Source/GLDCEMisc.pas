{: GLDCEMisc<p>

  Dynamic Collision Engine Miscellaneous functions<p>

  <b>History : </b><font size=-1><ul>
    <li>03/09/04 - LucasG. - Creation
  </ul></font>
}

unit GLDCEMisc;

interface

uses GLVectorFileObjects, GLEllipseCollision, VectorGeometry, Octree,GLTerrainRenderer;

//Get triangles to ellispe collision only
procedure AddFreeFormToMovePack(var MovePack: TECMovementPacket; FreeForm: TGLFreeForm;
    AObjectIndex: Integer; AFriction: Single; ASolid: Boolean);

//Add the ellipsoid to the possible colliders list
procedure AddEllipsoidToMovePack(var MovePack: TECMovementPacket;
    ePos, eRadius: TAffineVector;
    AObjectIndex: Integer; AFriction: Single; ASolid: Boolean);

//Add a terrain to the possible colliders list
procedure AddTerrainToMovePack(var MovePack: TECMovementPacket; var TerrainRenderer: TGLTerrainRenderer;
    AObjectIndex: Integer; AFriction: Single; ASolid: Boolean);

implementation

procedure AddFreeFormToMovePack(var MovePack: TECMovementPacket; FreeForm: TGLFreeForm;
    AObjectIndex: Integer; AFriction: Single; ASolid: Boolean);
var Radius: Single;
  i, t, k, count : Integer;
  p: POctreeNode;
  p1, p2, p3: PAffineVector;
  Pos: TVector;
  N: TAffineVector;
begin
  Assert(Assigned(FreeForm.Octree), 'Octree must have been prepared and setup before use.');
  SetVector(Pos,  FreeForm.AbsoluteToLocal(MovePack.Position));
  N := VectorNormalize(MovePack.Radius);
  N.Coord[0] := N.Coord[0] * Abs(MovePack.Velocity.Coord[0]) + Abs(MovePack.Gravity.Coord[0]) + MovePack.Radius.Coord[0];
  N.Coord[1] := N.Coord[1] * Abs(MovePack.Velocity.Coord[1]) + Abs(MovePack.Gravity.Coord[1]) + MovePack.Radius.Coord[1];
  N.Coord[2] := N.Coord[2] * Abs(MovePack.Velocity.Coord[2]) + Abs(MovePack.Gravity.Coord[2]) + MovePack.Radius.Coord[2];
  Radius := MaxXYZComponent(N);

  count := Length(MovePack.Triangles);
  with FreeForm.Octree do
  begin
    WalkSphereToLeaf(RootNode, Pos, Radius);

    if not Assigned(resultarray) then exit;

    for i:=0 to High(resultarray) do begin
      p:=ResultArray[i];
      for t:=0 to High(p.TriArray) do begin
        k:=p.triarray[t];
        //These are the vertices of the triangle to check
        p1:=@trianglefiler.List[k];
        p2:=@trianglefiler.List[k+1];
        p3:=@trianglefiler.List[k+2];
        count := count + 1;
        SetLength(MovePack.Triangles,count);
        with MovePack do
        begin
          SetVector(Triangles[count-1].p1,FreeForm.LocalToAbsolute(p1^));
          SetVector(Triangles[count-1].p2,FreeForm.LocalToAbsolute(p2^));
          SetVector(Triangles[count-1].p3,FreeForm.LocalToAbsolute(p3^));
          Triangles[count-1].ObjectIndex := AObjectIndex;
          Triangles[count-1].Friction := AFriction;
          Triangles[count-1].Solid := ASolid;
        end;
      end;
    end;
  end;
end;


procedure AddEllipsoidToMovePack(var MovePack: TECMovementPacket;
    ePos, eRadius: TAffineVector;
    AObjectIndex: Integer; AFriction: Single; ASolid: Boolean);
var count : Integer;
begin
  //Add possible collider
  count := Length(MovePack.Colliders);
  SetLength(MovePack.Colliders,count+1);
  with MovePack.Colliders[count] do
  begin
    Position := ePos;
    Size := eRadius;
    ObjectIndex := AObjectIndex;
    Friction := AFriction;
    Solid := ASolid;
    Shape := csEllipsoid;
  end;

end;

procedure AddTerrainToMovePack(var MovePack: TECMovementPacket; var TerrainRenderer: TGLTerrainRenderer;
    AObjectIndex: Integer; AFriction: Single; ASolid: Boolean);
var count : Integer;
begin
  //Add the terrain to the list
  count := Length(MovePack.Terrains);
  SetLength(MovePack.Terrains,count+1);
  with MovePack.Terrains[count] do
  begin
    Terrain := TerrainRenderer;
    Collider.ObjectIndex := AObjectIndex;
    Collider.Friction := AFriction;
    Collider.Solid := ASolid;
  end;

end;


end.
