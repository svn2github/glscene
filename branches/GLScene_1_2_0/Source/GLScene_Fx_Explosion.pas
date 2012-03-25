//
// This unit is part of the GLScene Project, http://glscene.org
//
{ : GLExplosionFx<p>

  TGLBExplosionFX Effect<p>

  <b>History : </b><font size=-1><ul>
  <li>23/08/10 - Yar - Added OpenGLTokens to uses, replaced OpenGL1x functions to OpenGLAdapter
  <li>23/02/07 - DaStr - Fixed TGLBExplosionFx.Create (TGLCoordinatesStyle stuff)
  <li>23/12/04 - PhP - GLScene Headerized, replaced some VectorXXX functions with XXXVector procedures
  <li>07/03/04 - Matheus Degiovani - Creation
  </ul></font>

  Description: this effect explodes a mesh object into triangles
  that fly over. You can define a default direction, in wich case
  the pieces of the mesh will follow that direction, only rotating,
  or if you define a null vector as the direction, a vector will be
  calculated for each triangle, based on the normal vector of that
  triangle, with a little random addition so things look better.
  Pretty neat :)<p>

  Note: the owner of this behaviour should be any class that derives
  from TGLBaseMesh class or any other class derived from TGLBaseMesh.
  Also, the structure of the mesh is lost after the caching of information,
  so if you'll need the mesh after exploding it, you'll have to save the
  MeshObjects property of the mesh, OR load it again.
}
unit GLScene_Fx_Explosion;

interface

{$I GLScene.inc}

uses
  GLScene_Base_OpenGL_Tokens,
  GLScene_Base_Vector_Geometry,
  GLScene_Core,
  GLScene_Vector_FileObjects,
  GLScene_Base_Vector_Types,
  GLScene_Base_Vector_Lists,
  GLScene_Base_XCollection,
  GLScene_Base_Coordinates,
  GLScene_Base_Context_Info;

type
  TGLBExplosionFX = class(TGLObjectPreEffect)
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
    procedure SetDirection(Value: TGLCoordinates);
    procedure SetEnabled(Value: boolean);
  protected
    property TriList: TAffineVectorList read FTriList write SetTriList;
    property RotList: TAffineVectorList read FRotList write SetRotList;
    property DirList: TAffineVectorList read FDirList write SetDirList;
    property PosList: TAffineVectorList read FPosList write SetPosList;
    property FaceCount: integer read FFaceCount write FFaceCount;
    procedure CacheInfo;
  public
    property Enabled: boolean read FEnabled write SetEnabled;
    property Step: integer read FStep;
    constructor Create(aOwner: TXCollection); override;
    destructor Destroy; override;
    procedure Render(var rci: TRenderContextInfo); override;
    { resets the behaviour, so the information can be re-cached and
      the mesh can be exploded again }
    procedure Reset;
    class function FriendlyName: String; override;
    class function FriendlyDescription: String; override;
  published
    property MaxSteps: integer read FMaxSteps write FMaxSteps;
    property Speed: single read FSpeed write FSpeed;
    property Direction: TGLCoordinates read FDirection write SetDirection;
  end;

implementation

uses
  GLScene_Base_Context,
  GLScene_Base_GLStateMachine;

{ TGLBExplosionFx }

// Create
//
constructor TGLBExplosionFX.Create(aOwner: TXCollection);
begin
  inherited Create(aOwner);
  FTriList := TAffineVectorList.Create;
  FRotList := TAffineVectorList.Create;
  FDirList := TAffineVectorList.Create;
  FPosList := TAffineVectorList.Create;
  FDirection := TGLCoordinates.CreateInitialized(Self, NullHmgVector, csPoint);
end;

// Destroy
//
destructor TGLBExplosionFX.Destroy;
begin
  FEnabled := False;
  FTriList.Free;
  FRotList.Free;
  FDirList.Free;
  FPosList.Free;
  FDirection.Free;
  inherited Destroy;
end;

// FriendlyName
//
class function TGLBExplosionFX.FriendlyName: string;
begin
  Result := 'ExplosionFx';
end;

// FriendlyDescription
//
class function TGLBExplosionFX.FriendlyDescription: string;
begin
  Result := 'Explosion FX';
end;

// SetTriList
//
procedure TGLBExplosionFX.SetTriList(Value: TAffineVectorList);
begin
  FTriList.Assign(Value);
end;

// SetRotList
//
procedure TGLBExplosionFX.SetRotList(Value: TAffineVectorList);
begin
  FRotList.Assign(Value);
end;

// SetDirList
//
procedure TGLBExplosionFX.SetDirList(Value: TAffineVectorList);
begin
  FDirList.Assign(Value);
end;

// SetPosList
//
procedure TGLBExplosionFX.SetPosList(Value: TAffineVectorList);
begin
  FPosList.Assign(Value);
end;

// SetDirection
//
procedure TGLBExplosionFX.SetDirection(Value: TGLCoordinates);
begin
  Value.Normalize;
  FDirection.Assign(Value);
end;

// SetEnabled
//
procedure TGLBExplosionFX.SetEnabled(Value: boolean);
begin
  FEnabled := Value;
end;

// Reset
//
procedure TGLBExplosionFX.Reset;
begin
  FEnabled := False;
  FStep := 0;
  FTriList.Clear;
  FRotList.Clear;
  FDirList.Clear;
  FPosList.Clear;
  FFaceCount := 0;
end;

// CacheInfo
//
procedure TGLBExplosionFX.CacheInfo;
var
  Face: integer;
  p1, p2, p3, v1, v2, posi: TAffineVector;
  Normal: TVector;
begin
  // make sure we can explode this object
  if not OwnerBaseSceneObject.InheritsFrom(TGLBaseMesh) then
  begin
    FEnabled := False;
    Exit;
  end;
  FTriList.Free;
  // get all the triangles of all the meshObjects
  FTriList := TGLBaseMesh(OwnerBaseSceneObject).MeshObjects.ExtractTriangles;
  FaceCount := FTriList.Count div 3;
  // set initial direction, rotation and position
  for Face := 0 to FaceCount - 1 do
  begin
    // get the vertices of the triangle
    SetVector(p1, FTriList.Items[Face * 3]);
    SetVector(p2, FTriList.Items[Face * 3 + 1]);
    SetVector(p3, FTriList.Items[Face * 3 + 2]);
    // if the direction property is a null vector, than the direction is
    // given by the normal of the face
    if VectorEquals(FDirection.AsVector, NullHmgVector) then
    begin
      v1 := VectorSubtract(p2, p1);
      v2 := VectorSubtract(p2, p3);
      NormalizeVector(v1); // use of procedure is faster: PhP
      NormalizeVector(v2); // use of procedure is faster: PhP
      SetVector(Normal, VectorCrossProduct(v1, v2));
      // use of procedure is faster: PhP
      // randomly rotate the normal vector so the faces are somewhat scattered
      case Random(3) of
        0:
          RotateVector(Normal, XVector, DegToRad(45.0 * Random));
        1:
          RotateVector(Normal, YVector, DegToRad(45.0 * Random));
        2:
          RotateVector(Normal, ZVector, DegToRad(45.0 * Random));
      end;
      NormalizeVector(Normal);
      FDirList.Add(Normal);
    end
    else
      FDirList.Add(FDirection.AsVector);
    // calculate the center (position) of the triangle so it rotates around its center
    posi[0] := (p1[0] + p2[0] + p3[0]) / 3;
    posi[1] := (p1[1] + p2[1] + p3[1]) / 3;
    posi[2] := (p1[2] + p2[2] + p3[2]) / 3;
    FPosList.Add(posi);
    // random rotation (in degrees)
    FRotList.Add(DegToRad(3.0 * Random), DegToRad(3.0 * Random),
      DegToRad(3.0 * Random));
  end;
  // Dispose the struture of the mesh
  TGLBaseMesh(OwnerBaseSceneObject).MeshObjects.Clear;
  TGLBaseMesh(OwnerBaseSceneObject).StructureChanged;
end;

// Render
//
procedure TGLBExplosionFX.Render(var rci: TRenderContextInfo);
var
  Face: integer;
  dir, p1, p2, p3: TAffineVector;
  mat: TMatrix;

begin
  if not FEnabled then
    Exit;
  // cache de list of vertices
  if FTriList.Count <= 0 then
  begin
    CacheInfo;
    if not FEnabled then
      Exit;
  end;
  // render explosion
  rci.GLStates.Disable(stCullFace);
  GL.Begin_(GL_TRIANGLES);
  for Face := 0 to FaceCount - 1 do
  begin
    SetVector(p1, FTriList.Items[Face * 3]);
    SetVector(p2, FTriList.Items[Face * 3 + 1]);
    SetVector(p3, FTriList.Items[Face * 3 + 2]);
    // rotate the face
    mat := IdentityHmgMatrix;
    mat := MatrixMultiply(mat, CreateRotationMatrixX(FRotList.Items[Face][0]));
    mat := MatrixMultiply(mat, CreateRotationMatrixY(FRotList.Items[Face][1]));
    mat := MatrixMultiply(mat, CreateRotationMatrixZ(FRotList.Items[Face][2]));
    SubtractVector(p1, FPosList.Items[Face]); // use of procedure is faster: PhP
    SubtractVector(p2, FPosList.Items[Face]); // -''-
    SubtractVector(p3, FPosList.Items[Face]); // -''-
    p1 := VectorTransform(p1, mat);
    p2 := VectorTransform(p2, mat);
    p3 := VectorTransform(p3, mat);
    AddVector(p1, FPosList.Items[Face]); // use of procedure is faster: PhP
    AddVector(p2, FPosList.Items[Face]); // -''-
    AddVector(p3, FPosList.Items[Face]); // -''-
    // move the face in the direction it is heading
    SetVector(dir, FDirList.Items[Face]);
    GL.Normal3f(dir[0], dir[1], dir[2]);
    ScaleVector(dir, Speed);
    AddVector(p1, dir);
    AddVector(p2, dir);
    AddVector(p3, dir);
    // also, move the center of the face
    FPosList.Items[Face] := VectorAdd(FPosList.Items[Face], dir);

    // save the changes
    FTriList.Items[Face * 3] := p1;
    FTriList.Items[Face * 3 + 1] := p2;
    FTriList.Items[Face * 3 + 2] := p3;

    GL.Vertex3f(p1[0], p1[1], p1[2]);
    GL.Vertex3f(p2[0], p2[1], p2[2]);
    GL.Vertex3f(p3[0], p3[1], p3[2]);
  end;
  GL.End_;
  rci.GLStates.Enable(stCullFace);
  if FMaxSteps <> 0 then
  begin
    Inc(FStep);
    if FStep = FMaxSteps then
      FEnabled := False;
  end;
end;

initialization

// class registrations
RegisterXCollectionItemClass(TGLBExplosionFX);

finalization

UnregisterXCollectionItemClass(TGLBExplosionFX);

end.