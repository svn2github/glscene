unit GLExplosionFx;

interface

uses
     OpenGL1x, VectorGeometry, glMisc, glScene, glVectorFileObjects, glTexture, VectorLists, XCollection;

{
TGLBExplosionFX Effect
Initial Design: Matheus Degiovani (matheus@tilt.net)
Initial release Date: 7/03/2002

Description: this effect explodes a mesh object into triangles
that fly over. You can define a default direction, in wich case
the pieces of the mesh will follow that direction, only rotating,
or if you define a null vector as the direction, a vector will be
calculated for each triangle, based on the normal vector of that
triangle, with a little random addition so things look better.
Pretty neat :)

Note: the owner of this behaviour should be any class that derives
from TGLBaseMesh class or any other class derived from TGLBaseMesh.
Also, the structure of the mesh is lost after the caching of information,
so if you'll need the mesh after exploding it, you'll have to save the
MeshObjects property of the mesh, OR load it again.
}

type TGLBExplosionFX = class(TGLObjectPreEffect)
     private
          FTriList: TAffineVectorList;
          FRotList: TAffineVectorList;
          FDirList: TAffineVectorList;
          FPosList: TAffineVectorList;
          FEnabled: boolean;
          FFaceCount: integer;
          FSpeed: single;
          FDirection: TGLCoordinates;
          FMaxSteps: integer;
          FStep: integer;

          procedure SetTriList(Value: TAffineVectorList);
          procedure SetRotList(Value: TAffineVectorList);
          procedure SetDirList(Value: TAffineVectorList);
          procedure SetPosList(Value: TAffineVectorList);
          procedure SetDirection(value: TGLCoordinates);
          procedure SetEnabled(value: boolean);
     protected
          property TriList: TAffineVectorList read FTriList write SetTriList;
          property RotList: TAffineVectorList read FRotList write SetRotList;
          property DirList: TAffineVectorList read FDirList write SetDirList;
          property PosList: TAffineVectorList read FPosList write SetPosList;
          property FaceCount: integer read FFAceCount write FFaceCount;

          procedure CacheInfo;
     public
          property Enabled: boolean read FEnabled write SetEnabled;
          property Step: integer read FStep;

          constructor Create(aOwner : TXCollection); override;
          destructor Destroy; override;
          procedure Render(sceneBuffer : TGLSceneBuffer;
                           var rci : TRenderContextInfo); override;
          {resets the behaviour, so the information can be re-cached and
          the mesh can be exploded again}
          procedure Reset;
          class function FriendlyName : String; override;
          class function FriendlyDescription : String; override;

     published
          //property X: single index 0 read FDirection[0] write FDirection[0];
          property MaxSteps: integer read FMaxSteps write FMaxSteps;
          property Speed: single read FSpeed write FSpeed;
          property Direction: TGLCoordinates read FDirection write SetDirection;
end;

implementation

constructor TGLBExplosionFx.Create(aOwner: TXCollection);
begin
     inherited Create(AOwner);

     FTriList:= TAffineVectorList.Create;
     FRotList:= TAffineVectorList.Create;
     FDirList:= TAffineVectorList.Create;
     FPosList:= TAffineVectorList.Create;
     FDirection:= TGlCoordinates.CreateInitialized(self, NullHmgVector);
     FDirection.SetVector(0, 0, 0, 0);
     FEnabled:= false;
end;

destructor TGLBExplosionFX.Destroy;
begin
     FEnabled:= false;

     FTriList.free;
     FRotList.Free;
     FDirList.Free;
     FPosList.Free;
     FDirection.Free;


     inherited Destroy;
end;

class function TGLBExplosionFX.FriendlyName : String;
begin
   Result:='ExplosionFx';
end;

class function TGLBExplosionFX.FriendlyDescription : String;
begin
   Result:='Explosion FX';
end;

procedure TGLBExplosionFx.SetTriList(Value: TAffineVectorList);
begin
     FTriList.Assign(value);
end;

procedure TGLBExplosionFx.SetRotList(Value: TAffineVectorList);
begin
     FRotList.Assign(value);
end;

procedure TGLBExplosionFx.SetDirList(Value: TAffineVectorList);
begin
     FDirList.Assign(value);
end;

procedure TGLBExplosionFx.SetPosList(Value: TAffineVectorList);
begin
     FPosList.Assign(value);
end;

procedure TGLBExplosionFx.SetDirection(value: TGLCoordinates);
begin
     value.Normalize;
     FDirection.setVector(value.x, value.y, value.z);
end;

procedure TGLBExplosionFx.SetEnabled(value: boolean);
begin
     Fenabled:= value;
end;

procedure TGLBExplosionFx.Reset;
begin
     FEnabled:= false;
     FStep:= 0;

     FTriList.Clear;
     FRotList.Clear;
     FDirList.Clear;
     FPosList.Clear;

     FFaceCount:= 0;
end;

procedure TGLBExplosionFx.CacheInfo;
var
face: integer;
p1, p2, p3, v1, v2, posi: TAffineVector;
normal: TVector;
begin
     //make sure we can explode this object
     if not OwnerBaseSceneObject.InheritsFrom(TGLBaseMesh) then begin
          enabled:= false;
          exit;
     end;

     //get all the triangles of all the meshObjects
     FTriList:= TGLBaseMesh(OwnerBaseSceneObject).MeshObjects.ExtractTriangles;
     FaceCount:= FTriList.Count div 3;

     //set initial direction, rotation and position
     for face:= 0 to facecount -1 do begin
          //get the vertices of the triangle
          SetVector(p1, triList.items[face * 3]);
          SetVector(p2, triList.items[face * 3 + 1]);
          SetVector(p3, triList.items[face * 3 + 2]);

          //if the direction property is a null vector, than the direction is
          //given by the normal of the face
          if VectorEquals(Fdirection.asvector, NullHmgVector) then begin
               v1:= VectorSubtract(p2, p1);
               v2:= VectorSubtract(p2, p3);
               VectorNormalize(v1);
               VEctorNormalize(v2);
               normal:= VectorMake(VectorCrossProduct(v1, v2));

               //randomly rotate the normal vector
               //so the faces are somewhat scattered
               case random(3) of
                    0: RotateVector(normal, XVector, degToRad(random(45)));
                    1: RotateVector(normal, YVector, degToRad(random(45)));
                    2: RotateVector(normal, ZVector, degToRad(random(45)));
               end;
               normal:= VectorNormalize(normal);
               dirList.Add(AffineVectorMake(normal));
          end else dirList.Add(AffineVectorMake(FDirection.asvector));

          //calculate the center (position) of the triangle
          //so it rotates around its center
          posi[0]:= (p1[0] + p2[0] + p3[0]) / 3;
          posi[1]:= (p1[1] + p2[1] + p3[1]) / 3;
          posi[2]:= (p1[2] + p2[2] + p3[2]) / 3;
          posList.add(posi);

          //random rotation (in degrees)
          rotList.add(degToRad(random(3)), DegToRad(random(3)),DegToRad(random(3)));
     end;

     //Dispose the struture of the mesh
     TGLBaseMesh(OwnerBaseSceneObject).MeshObjects.Clear;
     TGLBaseMesh(OwnerBaseSceneObject).StructureChanged;
end;

procedure TGLBExplosionFX.Render(sceneBuffer : TGLSceneBuffer; var rci : TRenderContextInfo);
var
face: integer;
dir, p1, p2, p3: TAffineVector;
mat: TMatrix;
begin
     if not enabled then exit;

     //cache de list of vertices
     if FTriList.Count <= 0 then begin
          CacheInfo;
          if not enabled then exit;
     end;

     //render explosion
     glDisable(GL_CULL_FACE);
     glBegin(GL_TRIANGLES);
     for face:= 0 to faceCount -1 do begin

          setVector(p1, triList.items[face * 3]);
          setVector(p2, triList.items[face * 3 + 1]);
          setVector(p3, triList.items[face * 3 + 2]);

          //rotate the face
          mat:= IdentityHmgMatrix;
          mat:= MatrixMultiply(mat, CreateRotationMatrixX(rotList.items[face][0]));
          mat:= MatrixMultiply(mat, CreateRotationMatrixY(rotList.items[face][1]));
          mat:= MatrixMultiply(mat, CreateRotationMatrixZ(rotList.items[face][2]));

          p1:= VectorSubtract(p1, posList.items[face]);
          p2:= VectorSubtract(p2, posList.items[face]);
          p3:= VectorSubtract(p3, posList.items[face]);

          p1:= VectorTransform(p1, mat);
          p2:= VectorTransform(p2, mat);
          p3:= VectorTransform(p3, mat);

          p1:= VectorAdd(p1, posList.items[face]);
          p2:= VectorAdd(p2, posList.items[face]);
          p3:= VectorAdd(p3, posList.items[face]);


          //move the face in the direction it is heading
          setVector(dir, dirList.items[face]);
          glNormal3f(dir[0], dir[1], dir[2]);
          dir:= VectorScale(dir, speed);
          p1:= VectorAdd(p1, dir);
          p2:= VectorAdd(p2, dir);
          p3:= VectorAdd(p3, dir);

          //also, move the center of the face
          posList.Items[face]:= vectorAdd(posList.items[face], dir);

          //save the changes
          trilist.items[face * 3]:= p1;
          trilist.items[face * 3 +1]:= p2;
          trilist.items[face * 3 +2]:= p3;

          glVertex3f(p1[0], p1[1], p1[2]);
          glVertex3f(p2[0], p2[1], p2[2]);
          glVertex3f(p3[0], p3[1], p3[2]);
     end;
     glEnd;
     glEnable(GL_CULL_FACE);

     if FMaxSteps <> 0 then begin
          inc(FStep);
          if FStep = FMaxSteps then enabled:= false;
     end;
end;

initialization

	// class registrations
	RegisterXCollectionItemClass(TGLBExplosionFX);

end.
