{: GLDCEMisc<p>

  Dynamic Collision Engine Miscellaneous functions<p>

  <b>History : </b><font size=-1><ul>
    <li>17/11/04 - LucasG. - Axis aligned static box colliders
    <li>17/11/04 - LucasG. - MultiProxy freeform support (by Roman Ganz)
    <li>14/11/04 - LucasG. - Proxy freeform support (by Roger Cao)
    <li>13/11/04 - LucasG. - Fixed triangle gathering (Radius := MaxXYZComponent(N) * 2;)
    <li>03/09/04 - LucasG. - Creation
  </ul></font>
}

unit GLDCEMisc;

interface

uses GLVectorFileObjects, GLProxyObjects, GLMultiProxy, GLObjects, GLScene, GLEllipseCollision, VectorGeometry, Octree,GLTerrainRenderer;

function AddRotatedVector(obj : TGLBaseSceneObject; V: TAffineVector): TAffineVector;

//Get triangles to ellispe collision only
procedure AddFreeFormToMovePack(var MovePack: TECMovementPacket; FreeForm:
  TGLBaseSceneObject; AObjectIndex: Integer; AFriction: Single; ASolid: Boolean);
//procedure AddFreeFormToMovePack(var MovePack: TECMovementPacket; FreeForm: TGLFreeForm;
//    AObjectIndex: Integer; AFriction: Single; ASolid: Boolean);

//Add the ellipsoid to the possible colliders list
procedure AddEllipsoidToMovePack(var MovePack: TECMovementPacket;
    ePos, eRadius: TAffineVector;
    AObjectIndex: Integer; AFriction: Single; ASolid: Boolean);

//Add a terrain to the possible colliders list
procedure AddTerrainToMovePack(var MovePack: TECMovementPacket; var TerrainRenderer: TGLTerrainRenderer;
    AObjectIndex: Integer; AFriction: Single; ASolid: Boolean);

//Add a box to the possible colliders list
procedure AddBoxToMovePack(var MovePack: TECMovementPacket; BoxObj: TGLBaseSceneObject; BoxSize: TAffineVector;
    AObjectIndex: Integer; AFriction: Single; ASolid: Boolean);

const
    DCEBox: array [0..35] of TAffineVector = (
      ( 1,-1,-1), ( 1, 1,-1), ( 1,-1, 1),
      ( 1, 1,-1), ( 1, 1, 1), ( 1,-1, 1),

      ( 1, 1,-1), (-1, 1,-1), (-1, 1, 1),
      ( 1, 1, 1), ( 1, 1,-1), (-1, 1, 1),

      (-1, 1, 1), (-1,-1, 1), ( 1,-1, 1),
      ( 1, 1, 1), (-1, 1, 1), ( 1,-1, 1),

      (-1,-1, 1), (-1, 1, 1), (-1, 1,-1),
      (-1,-1,-1), (-1,-1, 1), (-1, 1,-1),

      ( 1,-1, 1), (-1,-1, 1), ( 1,-1,-1),
      (-1,-1, 1), (-1,-1,-1), ( 1,-1,-1),

      ( 1, 1,-1), ( 1,-1,-1), (-1, 1,-1),
      ( 1,-1,-1), (-1,-1,-1), (-1, 1,-1)

    );

implementation

function AddRotatedVector(obj : TGLBaseSceneObject; V: TAffineVector): TAffineVector;
var Dir, Up, Left: TAffineVector;
begin
  Dir := obj.Direction.AsAffineVector;
  Up := obj.Up.AsAffineVector;
  Left := AffineVectorMake(obj.LeftVector);
  result[0] := (Left[0] * v[0]) + (Up[0] * v[1]) + (Dir[0] * v[2]);
  result[1] := (Left[1] * v[0]) + (Up[1] * v[1]) + (Dir[1] * v[2]);
  result[2] := (Left[2] * v[0]) + (Up[2] * v[1]) + (Dir[2] * v[2]);
end;

procedure AddFreeFormToMovePack(var MovePack: TECMovementPacket; FreeForm:
  TGLBaseSceneObject; AObjectIndex: Integer; AFriction: Single; ASolid: Boolean);
var Radius: Single;
  i, t, k, count : Integer;
  p: POctreeNode;
  p1, p2, p3: PAffineVector;
  Pos: TVector;
  N: TAffineVector;
  Master: TGLBaseSceneObject;
begin
  Master := FreeForm;
  while Master is TGLFreeFormProxy do
    Master := TGLFreeFormProxy(Master).MasterObject;
  while Master is TGLMultiProxy do
    if TGLMultiProxy(Master).Count > 0 then
      Master := TGLMultiProxy(Master).MasterObjects[0].MasterObject;

  Assert((Master is TGLFreeForm), 'Object must be freeform, freeformproxy or freeformbased Multiproxy.');
  Assert(Assigned(TGLFreeForm(Master).Octree), 'Octree must have been prepared and setup before use.');

  SetVector(Pos,  FreeForm.AbsoluteToLocal(MovePack.Position));
  N := VectorNormalize(MovePack.Radius);
  N[0] := N[0] * (Abs(MovePack.Velocity[0]) + Abs(MovePack.Gravity[0]) + MovePack.Radius[0]);
  N[1] := N[1] * (Abs(MovePack.Velocity[1]) + Abs(MovePack.Gravity[1]) + MovePack.Radius[1]);
  N[2] := N[2] * (Abs(MovePack.Velocity[2]) + Abs(MovePack.Gravity[2]) + MovePack.Radius[2]);
  Radius := MaxXYZComponent(N) * 2;

  count := Length(MovePack.Triangles);
  with TGLFreeForm(Master).Octree do
  begin
    WalkSphereToLeaf(RootNode, Pos, Radius);

    if not Assigned(resultarray) then
      exit;

    for i:=0 to High(resultarray) do
    begin
      p:=ResultArray[i];
      for t:=0 to High(p.TriArray) do
      begin
        k:=p.triarray[t];
        //These are the vertices of the triangle to check
        p1:=@trianglefiler.List[k];
        p2:=@trianglefiler.List[k+1];
        p3:=@trianglefiler.List[k+2];
        count := count + 1;
        SetLength(MovePack.Triangles,count);
        with MovePack do
        begin
          SetVector(Triangles[count-1].p1, FreeForm.LocalToAbsolute(p1^));
          SetVector(Triangles[count-1].p2, FreeForm.LocalToAbsolute(p2^));
          SetVector(Triangles[count-1].p3, FreeForm.LocalToAbsolute(p3^));
          Triangles[count-1].ObjectIndex := AObjectIndex;
          Triangles[count-1].Friction := AFriction;
          Triangles[count-1].Solid := ASolid;
          //Triangles[count-1].Collided := False; //Debug
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

procedure AddBoxToMovePack(var MovePack: TECMovementPacket; BoxObj: TGLBaseSceneObject; BoxSize: TAffineVector;
    AObjectIndex: Integer; AFriction: Single; ASolid: Boolean);
var count,i : Integer;
    p1, p2, p3, BoxPos: TAffineVector;
    Radius, BoxRadius: Single;
    N: TAffineVector;
begin

  N := VectorNormalize(MovePack.Radius);
  N[0] := N[0] * (Abs(MovePack.Velocity[0]) + Abs(MovePack.Gravity[0]) + MovePack.Radius[0]);
  N[1] := N[1] * (Abs(MovePack.Velocity[1]) + Abs(MovePack.Gravity[1]) + MovePack.Radius[1]);
  N[2] := N[2] * (Abs(MovePack.Velocity[2]) + Abs(MovePack.Gravity[2]) + MovePack.Radius[2]);
  Radius := MaxXYZComponent(N);
  BoxRadius := MaxXYZComponent(BoxSize);
  BoxPos := AffineVectorMake(BoxObj.AbsolutePosition);
  N := VectorAdd(MovePack.Position, MovePack.Velocity);
  if VectorDistance(N,BoxPos) > Radius + BoxRadius then
    exit;

  //Add the box to the triangle list
  count := Length(MovePack.Triangles);
  ScaleVector(BoxSize,0.5);
  i := 0;
  while i < 36 do
  begin

    count := count + 1;
    SetLength(MovePack.Triangles,count);

    with MovePack do
    begin
      p1 := DCEBox[i]; ScaleVector(p1,BoxSize); p1 := AddRotatedVector(BoxObj, p1); AddVector(p1,BoxPos);
      p2 := DCEBox[i+1]; ScaleVector(p2,BoxSize); p2 := AddRotatedVector(BoxObj, p2); AddVector(p2,BoxPos);
      p3 := DCEBox[i+2]; ScaleVector(p3,BoxSize); p3 := AddRotatedVector(BoxObj, p3); AddVector(p3,BoxPos);

      i := i + 3;

      SetVector(Triangles[count-1].p1, p1);
      SetVector(Triangles[count-1].p2, p2);
      SetVector(Triangles[count-1].p3, p3);
      Triangles[count-1].ObjectIndex := AObjectIndex;
      Triangles[count-1].Friction := AFriction;
      Triangles[count-1].Solid := ASolid;
    end;

  end;
end;

end.
