// 03/02/03 - EG - Fixed CopyPosFromGeomToGL

unit ODEGL;

interface

{
  Here I collect random functions and procedures I've found useful when
  integrating ODE into GLScene. If you don't use GLScene, this unit won't be
  very useful to you. The unit is not intended as a sorted toolbox, but more
  as a place to put stuff until we figure out how to organize the integration.

  Mattias Fagerlund ( mattias@cambrianlabs.com ), 2002-09-26 

}

uses
  OpenGL12, Geometry, ODEImport, GLScene, VectorTypes, GLObjects;

  procedure DrawBox(Sides : TdVector3);
  procedure setTransform (pos : TdVector3; R : TdMatrix3);
  procedure dsDrawBox (pos : PdVector3; R : PdMatrix3; Sides : TdVector3); overload;
  procedure dsDrawBox (pos : TdVector3; R : TdMatrix3; Sides : TdVector3); overload;

  procedure ODERToGLSceneMatrix(var m : TMatrix; R : TdMatrix3; pos : TdVector3); overload;
  procedure ODERToGLSceneMatrix(var m : TMatrix; R : PdMatrix3; pos : PdVector3); overload;
  procedure ODERToGLSceneMatrix(var m : TMatrix; R : TdMatrix3_As3x4; pos : TdVector3); overload;

  function ConvertdVector3ToVector3f(R : TdVector3) : TVector3f; overload;
  function ConvertdVector3ToVector3f(R : PdVector3) : TVector3f; overload;

  function dVector3Length(R : TdVector3) : single; overload;
  function dVector3Length(R : PdVector3) : single; overload;

  function dBodyToBodyDistance(Body1, Body2 : PdxBody) : TdReal;

  procedure CopyPosFromGeomToGL(Geom : PdxGeom; GLBaseSceneObject : TGLBaseSceneObject);
  procedure PositionSceneObject(GLBaseSceneObject : TGLBaseSceneObject; Geom : PdxGeom);
  procedure PositionSceneObjectForGeom(Geom : PdxGeom);

  procedure CopyCubeSizeFromBox(Cube : TGLCube; Geom : PdxGeom);
  procedure CopyBodyFromCube(Body : PdxBody; var Geom : PdxGeom; Cube : TGLCube; Space : PdxSpace);

  function CreateGeomFromCube(Cube : TGLCube; Space : PdxSpace) : PdxGeom;
  function CreateBodyFromCube(var Geom : PdxGeom; Cube : TGLCube; World : PdxWorld; Space : PdxSpace) : PdxBody;

  function GLMatrixFromGeom(Geom : PdxGeom) : TMatrix;
  function GLDirectionFromGeom(Geom : PdxGeom) : TVector;

  function CreateODEPlaneFromGLPlane(Plane : TGLPlane; Space : PdxSpace) : PdxGeom;

  procedure RenderGeomList(GeomList : TGeomList);

  function RandomColorVector : TVector;
  // { $ EXTERNALSYM GL_ZERO} ?

implementation


procedure ODERToGLSceneMatrix(var m : TMatrix; R : TdMatrix3_As3x4; pos : TdVector3); overload;
begin
  m[0][0]:=r[0][0];
  m[0][1]:=r[0][1];
  m[0][2]:=r[0][2];
  m[0][3]:=0;
  m[1][0]:=r[1][0];
  m[1][1]:=r[1][1];
  m[1][2]:=r[1][2];
  m[1][3]:=0;
  m[2][0]:=r[2][0];
  m[2][1]:=r[2][1];
  m[2][2]:=r[2][2];
  m[2][3]:=0;
  m[3]:=NullHmgPoint;

  TransposeMatrix(m);
  m[3][0]:=pos[0];
  m[3][1]:=pos[1];
  m[3][2]:=pos[2];
  m[3][3]:=1;//}
end;

procedure ODERToGLSceneMatrix(var m : TMatrix; R : PdMatrix3; pos : PdVector3);
begin
  ODERToGLSceneMatrix(m, TdMatrix3_As3x4(R^), pos^);
end;

procedure ODERToGLSceneMatrix(var m : TMatrix; R : TdMatrix3; pos : TdVector3);
begin
  ODERToGLSceneMatrix(m, TdMatrix3_As3x4(R), pos);
end;

procedure DrawBox(Sides : TdVector3);
var
  lx, ly, lz : single;
begin
  lx := Sides[0]*0.5;
  ly := Sides[1]*0.5;
  lz := Sides[2]*0.5;

  // sides
  glBegin (GL_TRIANGLE_STRIP);
  glNormal3f (-1,0,0);
  glVertex3f (-lx,-ly,-lz);
  glVertex3f (-lx,-ly,lz);
  glVertex3f (-lx,ly,-lz);
  glVertex3f (-lx,ly,lz);
  glNormal3f (0,1,0);
  glVertex3f (lx,ly,-lz);
  glVertex3f (lx,ly,lz);
  glNormal3f (1,0,0);
  glVertex3f (lx,-ly,-lz);
  glVertex3f (lx,-ly,lz);
  glNormal3f (0,-1,0);
  glVertex3f (-lx,-ly,-lz);
  glVertex3f (-lx,-ly,lz);
  glEnd();

  // top face
  glBegin (GL_TRIANGLE_FAN);
  glNormal3f (0,0,1);
  glVertex3f (-lx,-ly,lz);
  glVertex3f (lx,-ly,lz);
  glVertex3f (lx,ly,lz);
  glVertex3f (-lx,ly,lz);
  glEnd();

  // bottom face
  glBegin (GL_TRIANGLE_FAN);
  glNormal3f (0,0,-1);
  glVertex3f (-lx,-ly,-lz);
  glVertex3f (-lx,ly,-lz);
  glVertex3f (lx,ly,-lz);
  glVertex3f (lx,-ly,-lz);
  glEnd();
end;

procedure dsDrawBox (pos : PdVector3; R : PdMatrix3; Sides : TdVector3);
begin
  dsDrawBox(pos^, r^, Sides);
end;

procedure dsDrawBox (pos : TdVector3; R : TdMatrix3; Sides : TdVector3);
begin
  setTransform (pos, R);
  drawBox (sides);
  glPopMatrix();
end;

procedure setTransform (pos : TdVector3; R : TdMatrix3);
var
  matrix : array[0..15] of GLfloat;
begin
  matrix[0] := R[0];
  matrix[1] := R[4];
  matrix[2] := R[8];
  matrix[3] := 0;
  matrix[4] := R[1];
  matrix[5] := R[5];
  matrix[6] := R[9];
  matrix[7] := 0;
  matrix[8] := R[2];
  matrix[9] := R[6];
  matrix[10] := R[10];
  matrix[11] := 0;
  matrix[12] := pos[0];
  matrix[13] := pos[1];
  matrix[14] := pos[2];
  matrix[15] := 1;
  glPushMatrix();
  glMultMatrixf (@matrix);
end;

function ConvertdVector3ToVector3f(R : TdVector3) : TVector3f;
begin
  result[0] := R[0];
  result[1] := R[1];
  result[2] := R[2];
end;

function ConvertdVector3ToVector3f(R : PdVector3) : TVector3f;
begin
  result[0] := R[0];
  result[1] := R[1];
  result[2] := R[2];
end;

procedure PositionSceneObjectForGeom(Geom : PdxGeom);
begin
  if Assigned(Geom.Data) then
    PositionSceneObject(TGLBaseSceneObject(Geom.Data), Geom);
end;

function GLMatrixFromGeom(Geom : PdxGeom) : TMatrix;
var
  pos, Pos2 : PdVector3;
  R,R2  : PdMatrix3;

  actual_pos : TdVector3;
  actual_R : TdMatrix3;

  TransformedGeom : PdxGeom;
  GeomClass : integer;
begin
  // Retrieve the position and rotation of the geom
  pos := dGeomGetPosition (geom);
  R := dGeomGetRotation (geom);

  // if the geom is a transform geom, it should be treated differently
  GeomClass := dGeomGetClass (Geom);

  if GeomClass = dGeomTransformClass then
  begin
    TransformedGeom := dGeomTransformGetGeom (Geom);

    // No transformed geom!?
    if TransformedGeom=nil then
      exit;

    // Retrieve the position and rotation of the transformed geom
    pos2 := dGeomGetPosition (TransformedGeom);
    R2 := dGeomGetRotation (TransformedGeom);

    dMULTIPLY0_331 (actual_pos, R^, pos2^);
    actual_pos := Vector3ADD(actual_pos, pos^);
    dMULTIPLY0_333 (actual_R,R^,R2^);

    ODERToGLSceneMatrix(result, actual_R, actual_pos);
  end else
  begin
    ODERToGLSceneMatrix(result, R, pos);
  end;
end;

function GLDirectionFromGeom(Geom : PdxGeom) : TVector;
var
  m : TMatrix;
begin
  m := GLMatrixFromGeom(Geom);

  result := VectorNormalize(m[2]);
end;

procedure PositionSceneObject(GLBaseSceneObject : TGLBaseSceneObject; Geom : PdxGeom);
begin
  GLBaseSceneObject.Matrix:=GLMatrixFromGeom(Geom);
end;

procedure CopyCubeSizeFromBox(Cube : TGLCube; Geom : PdxGeom);
var
  Sides : TdVector3;
begin
  dGeomBoxGetLengths(Geom, Sides);

  Cube.CubeWidth := Sides[0]; // 0
  Cube.CubeHeight := Sides[1]; // 1
  Cube.CubeDepth := Sides[2]; // 2
end;

procedure CopyPosFromGeomToGL(Geom : PdxGeom; GLBaseSceneObject : TGLBaseSceneObject);
var
  v : TVector;
  m : TMatrix;

  R : PdMatrix3;
  pos : PdVector3;
begin
  v := GLBaseSceneObject.AbsolutePosition;

  dGeomSetPosition(Geom, v[0], v[1], v[2]);

  R := dGeomGetRotation(Geom);
  pos := dgeomGetPosition(Geom);

  m := GLBaseSceneObject.AbsoluteMatrix;
  R[0] := m[0][0];
  R[4] := m[0][1];
  R[8] := m[0][2];
  R[1] := m[1][0];
  R[5] := m[1][1];
  R[9] := m[1][2];
  R[2] := m[2][0];
  R[6] := m[2][1];
  R[10] := m[2][2];
  pos[0] := m[3][0];
  pos[1] := m[3][1];
  pos[2] := m[3][2];//}

  dGeomSetRotation(Geom, R^);
end;

function CreateGeomFromCube(Cube : TGLCube; Space : PdxSpace) : PdxGeom;
var
  Geom : PdxGeom;
begin
  Geom := dCreateBox(Space, Cube.CubeWidth, Cube.CubeHeight, Cube.CubeDepth);
  CopyPosFromGeomToGL(Geom, Cube);

  result := Geom;
end;

function CreateBodyFromCube(var Geom : PdxGeom; Cube : TGLCube; World : PdxWorld; Space : PdxSpace) : PdxBody;
var
  Body : PdxBody;
begin
  Body := dBodyCreate(World);
  dBodySetLinearVel(Body, 0, 0, 0);

  CopyBodyFromCube(Body, Geom, Cube, Space);
end;

procedure CopyBodyFromCube(Body : PdxBody; var Geom : PdxGeom; Cube : TGLCube; Space : PdxSpace);
var
  m : TdMass;
begin
  // Stup the body
  dMassSetBox(m, 1, Cube.CubeWidth, Cube.CubeHeight, Cube.CubeDepth);
  dBodySetMass(Body, m);

  // Setup the geom
  Geom := CreateGeomFromCube(Cube, Space);
  dGeomSetBody (Geom,Body);

  CopyPosFromGeomToGL(Geom, Cube);


  Geom.data := Cube;
end;

function dBodyToBodyDistance(Body1, Body2 : PdxBody) : TdReal;
begin
  result := dVector3Length(Vector3SUB(Body1.pos, Body2.pos)); 
end;

function dVector3Length(R : TdVector3) : single;
begin
  result := Sqrt(sqr(R[0])+sqr(R[1])+sqr(R[2]));
end;

function dVector3Length(R : PdVector3) : single;
begin
  result := Sqrt(sqr(R[0])+sqr(R[1])+sqr(R[2]));
end;

procedure RenderGeomList(GeomList : TGeomList);
var
  i : integer;
begin
  for i := 0 to GeomList.Count-1 do
    if Assigned(GeomList[i].data) then
      PositionSceneObject(TGLBaseSceneObject(GeomList[i].data),GeomList[i]);
end;

function CreateODEPlaneFromGLPlane(Plane : TGLPlane; Space : PdxSpace) : PdxGeom;
var
  Pos, Direction : TVector;
  d : single;
begin
  Direction := Plane.AbsoluteDirection;
  Pos := Plane.AbsolutePosition;

  d := (Direction[0]*Pos[0]+Direction[1]*Pos[1]+Direction[2]*Pos[2]);

  result := dCreatePlane (space,Direction[0],Direction[1],Direction[2],d);
end;

function RandomColorVector : TVector;
begin
  result := VectorMake(Random, Random, Random, 0);
end;
end.
