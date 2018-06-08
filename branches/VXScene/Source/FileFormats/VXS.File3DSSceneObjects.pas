//
// VXScene Component Library, based on GLScene http://glscene.sourceforge.net
//
{
  3ds-specific scene objects.
}
unit VXS.File3DSSceneObjects;

interface

{$I VXScene.inc}

uses
  System.Classes,
  System.SysUtils,
  System.Math,

  VXS.OpenGL,
  VXS.VectorGeometry,
  VXS.Context,
  VXS.Scene,
  VXS.VectorFileObjects,
  VXS.VectorTypes,
  VXS.PersistentClasses,
  VXS.CrossPlatform,
  VXS.Coordinates,
  VXS.RenderContextInfo,
  VXS.State;

type
  TVXFile3DSLight = class(TVXLightSource)
  private
    FTargetPos: TVXCoordinates;
    FHotSpot: Single;
    FMultipler: Single;
  public
    constructor Create(AOwner: TComponent); override;
    procedure DoRender(var rci: TVXRenderContextInfo; renderSelf, renderChildren: Boolean); override;
    procedure CoordinateChanged(Sender: TVXCustomCoordinates); override;
    destructor Destroy; override;
  published
    property SpotTargetPos: TVXCoordinates read FTargetPos;
    property HotSpot: Single read FHotSpot write FHotSpot;
    property Multipler: Single read FMultipler write FMultipler;
  end;

  TVXFile3DSCamera = class(TVXCamera)
  private
    FTargetPos: TVXCoordinates;
    FQuadCyl: array[0..1] of PGLUquadricObj;
    FQuadDisk: array[0..1] of PGLUquadricObj;
  public
    constructor Create(AOwner: TComponent); override;
    procedure DoRender(var rci: TVXRenderContextInfo; renderSelf, renderChildren: Boolean); override;
    procedure CoordinateChanged(Sender: TVXCustomCoordinates); override;
    destructor Destroy; override;
  published
    property CameraTargetPos: TVXCoordinates read FTargetPos;
    property RollAngle;
  end;

  TVXFile3DSActor = class(TVXActor)
  private
    procedure ReadMesh(Stream: TStream);
    procedure WriteMesh(Stream: TStream);
  protected
    procedure DefineProperties(Filer: TFiler); override;
  end;

  TVXFile3DSFreeForm = class(TVXFreeForm)
  private
    FTransfMat, FScaleMat, ParentMatrix: TMatrix;

    FS_Rot3DS: TVXCoordinates4;
    FRot3DS: TVXCoordinates4;
    FScale3DS: TVXCoordinates4;
    procedure ReadMesh(Stream: TStream);
    procedure WriteMesh(Stream: TStream);
  protected
    procedure DefineProperties(Filer: TFiler); override;
  public
    FRefMat: TMatrix;
    constructor Create(AOWner: TComponent); override;
    destructor Destroy; override;
    procedure BuildList(var rci: TVXRenderContextInfo); override;
    procedure CoordinateChanged(Sender: TVXCustomCoordinates); override;
    function AxisAlignedDimensionsUnscaled: TVector; override;
    function BarycenterAbsolutePosition: TVector; override;
  published
    property S_Rot3DS: TVXCoordinates4 read FS_Rot3DS;
    property Rot3DS: TVXCoordinates4 read FRot3DS;
    property Scale3DS: TVXCoordinates4 read FScale3DS;
  end;

var
  vFile3DSSceneObjects_RenderCameraAndLights: Boolean = False;

//===============================================================
implementation
//===============================================================

function MakeRotationQuaternion(const axis: TAffineVector; angle: Single): TQuaternion;
var
  v: Tvector;
  halfAngle, invAxisLengthMult: Single;
begin
  halfAngle := (angle) / 2;
  invAxisLengthMult := 1 / VectorLength(axis) * sin(halfAngle);

  v.X := axis.X * invAxisLengthMult;
  v.Y := axis.Y * invAxisLengthMult;
  v.Z := axis.Z * invAxisLengthMult;
  v.W := cos(halfAngle);

  Result.ImagPart := AffineVectorMake(v);
  Result.RealPart := v.W;
end;

function QuaternionToRotateMatrix(const Quaternion: TQuaternion): TMatrix;
var
  wx, wy, wz, xx, yy, yz, xy, xz, zz, x2, y2, z2: Single;
  quat: TVector;
  m: TMatrix;
begin
  quat := VectorMake(Quaternion.ImagPart);
  quat.W := Quaternion.RealPart;

  x2 := quat.X + quat.X;
  y2 := quat.Y + quat.Y;
  z2 := quat.Z + quat.Z;
  xx := quat.X * x2;
  xy := quat.X * y2;
  xz := quat.X * z2;
  yy := quat.Y * y2;
  yz := quat.Y * z2;
  zz := quat.Z * z2;
  wx := quat.W * x2;
  wy := quat.W * y2;
  wz := quat.W * z2;

  m.X.X := 1.0 - (yy + zz);
  m.X.Y := xy - wz;
  m.X.Z := xz + wy;
  m.Y.X := xy + wz;
  m.Y.Y := 1.0 - (xx + zz);
  m.Y.Z := yz - wx;
  m.Z.X := xz - wy;
  m.Z.Y := yz + wx;
  m.Z.Z := 1.0 - (xx + yy);

  m.X.W := 0;
  m.Y.W := 0;
  m.Z.W := 0;
  m.W.X := 0;
  m.W.Y := 0;
  m.W.Z := 0;
  m.W.W := 1;

  Result := m;
end;

constructor TVXFile3DSLight.Create(AOwner: TComponent);
begin
  inherited;

  FTargetPos := TVXCoordinates.CreateInitialized(self, VectorMake(NullVector), csPoint);
  FHotSpot := 1;
  FMultipler := 1;
end;

procedure TVXFile3DSLight.DoRender(var rci: TVXRenderContextInfo; renderSelf, renderChildren: Boolean);

  procedure BuildFace;
  begin
    glBegin(GL_TRIANGLES);
    glVertex3f(0.03, 0, 0);
    glVertex3f(0, 0.03, 0);
    glVertex3f(0, 0, 0.07);
    glEnd;
  end;

var
  dv: Single;

begin
  inherited;
  if not vFile3DSSceneObjects_RenderCameraAndLights then
    Exit;

  rci.VXStates.PolygonMode := pmLines;
  glPushMatrix;

  dv := VectorDistance(Position.AsVector, rci.cameraPosition);
  glScalef(dv, dv, dv);

  // Up.
  BuildFace;
  glRotatef(90, 0, 0, 1);
  BuildFace;
  glRotatef(180, 0, 0, 1);
  BuildFace;
  glRotatef(270, 0, 0, 1);
  BuildFace;

  // Down.
  glRotatef(180, 0, 1, 0);
  BuildFace;
  glRotatef(90, 0, 0, 1);
  BuildFace;
  glRotatef(180, 0, 0, 1);
  BuildFace;
  glRotatef(270, 0, 0, 1);
  BuildFace;

  glPopMatrix;
end;

procedure TVXFile3DSLight.CoordinateChanged(Sender: TVXCustomCoordinates);
begin
  inherited;

  if (Sender = FTargetPos) or (Sender = Position) then
    SpotDirection.SetVector(VectorNormalize(VectorSubtract(FTargetPos.AsAffineVector, Position.AsAffineVector)));
end;

destructor TVXFile3DSLight.Destroy;
begin
  FTargetPos.Free;
  inherited;
end;

constructor TVXFile3DSCamera.Create(AOwner: TComponent);
var
  I: Integer;
begin
  inherited;

  FTargetPos := TVXCoordinates.CreateInitialized(self, VectorMake(NullVector), csPoint);

  for I := 0 to 1 do
  begin
    //    FQuadCyl[I] := gluNewQuadric;
    //    FQuadDisk[I] := gluNewQuadric;
    //    gluQuadricNormals(FQuadCyl[I], GLU_SMOOTH);
    //    gluQuadricNormals(FQuadDisk[I], GLU_SMOOTH);
  end;
end;

procedure TVXFile3DSCamera.DoRender(var rci: TVXRenderContextInfo; renderSelf, renderChildren: Boolean);

  procedure BuildCyl;
  begin
    //    gluCylinder(FQuadCyl[0], 1, 1, 0.5, 6, 1);
    //    glTranslatef(0, 0, 0.5);
    //    gluDisk(FQuadDisk[0], 0, 1, 6, 1);
    glTranslatef(0, 0, -0.5);
    rci.VXStates.InvertFrontFace;
    //    gluDisk(FQuadDisk[0], 0, 1, 6, 1);
    rci.VXStates.InvertFrontFace;
  end;

  procedure BuildFace;
  begin
    glRotatef(-90, 0, 1, 0);
    glRotatef(45, 0, 0, 1);
    glTranslatef(0, -0.5, 1);
    //    gluCylinder(FQuadCyl[0], 0.5, 1.3, 2.4, 4, 1);
    glTranslatef(0, 0, 2.4);
    //    gluDisk(FQuadDisk[0], 0, 1.3, 4, 1);
  end;

var
  dv, ang: Single;
  v, v1: TAffineVector;

begin
  inherited;
  if not vFile3DSSceneObjects_RenderCameraAndLights then
    Exit;

  v := VectorNormalize(VectorSubtract(FTargetPos.AsAffineVector, Position.AsAffineVector));

  v1 := AffineVectorMake(v.X, v.Y, 0);
  NormalizeVector(v1);
  ang := ArcCosine(VectorDotProduct(v, v1));

  rci.VXStates.PolygonMode := pmLines;

  glPushMatrix;
  glRotatef(ang * 180 / pi, 0, 0, 1);
  dv := VectorDistance(Position.AsVector, rci.cameraPosition);
  glScalef(dv / 25, dv / 25, dv / 25);

  glRotateF(90, 0, 1, 0);
  glTranslatef(0, 1, 0);
  BuildCyl;
  glTranslatef(1, -1, 0);
  BuildCyl;
  BuildFace;
  glPopMatrix;

  rci.VXStates.PolygonMode := pmFill;
end;

procedure TVXFile3DSCamera.CoordinateChanged(Sender: TVXCustomCoordinates);
begin
  inherited;

  if (Sender = FTargetPos) or (Sender = Position) then
  begin
    //    Up.AsAffineVector := ZVector;
    //    Direction.SetVector(VectorNormalize(VectorSubtract(FTargetPos.AsAffineVector, Position.AsAffineVector)));
  end;
end;

destructor TVXFile3DSCamera.Destroy;
var
  I: Integer;
begin
  inherited;
  FTargetPos.Free;
  for I := 0 to 1 do
  begin
    gluDeleteQuadric(@FQuadCyl[I]);
    gluDeleteQuadric(@FQuadDisk[I]);
  end;
end;

procedure TVXFile3DSActor.ReadMesh(Stream: TStream);
var
  virt: TBinaryReader;
begin
  virt := TBinaryReader.Create(Stream);
  MeshOBjects.ReadFromFiler(virt);
  virt.Free;
end;

procedure TVXFile3DSActor.WriteMesh(Stream: TStream);
var
  virt: TBinaryWriter;
begin
  virt := TBinaryWriter.Create(Stream);
  MeshOBjects.WriteToFiler(virt);
  virt.Free;
end;

procedure TVXFile3DSActor.DefineProperties(Filer: TFiler);
begin
  Filer.DefineBinaryProperty('MeshObjectsData', ReadMesh, WriteMesh, True);
end;

constructor TVXFile3DSFreeForm.Create(AOWner: TComponent);
begin
  inherited;

  FRefMat := IdentityHmgMatrix;
  FTransfMat := IdentityHmgMatrix;
  FScaleMat := IdentityHmgMatrix;
  FS_Rot3DS := TVXCoordinates4.CreateInitialized(self, VectorMake(1, 0, 0), csVector);
  FRot3DS := TVXCoordinates4.CreateInitialized(self, VectorMake(1, 0, 0), csVector);
  FScale3DS := TVXCoordinates4.CreateInitialized(self, VectorMake(1, 1, 1), csVector);

  ObjectStyle := [osDirectDraw];
end;

destructor TVXFile3DSFreeForm.Destroy;
begin
  FS_Rot3DS.Free;
  FRot3DS.Free;
  FScale3DS.Free;

  inherited;
end;

procedure TVXFile3DSFreeForm.ReadMesh(Stream: TStream);
var
  v: TVector;
  virt: TBinaryReader;
begin
  virt := TBinaryReader.Create(Stream);

  virt.read(FRefMat, sizeof(FRefMat));
  virt.read(v, sizeof(v));
  S_Rot3DS.SetVector(v);
  virt.read(v, sizeof(v));
  Rot3DS.SetVector(v);
  virt.read(v, sizeof(v));
  Scale3DS.SetVector(v);

  MeshOBjects.ReadFromFiler(virt);
  virt.Free;
end;

procedure TVXFile3DSFreeForm.WriteMesh(Stream: TStream);
var
  virt: TBinaryWriter;
  v: TVector;
begin
  virt := TBinaryWriter.Create(Stream);

  virt.write(FRefMat, sizeof(FRefMat));
  v := S_Rot3DS.AsVector;
  virt.write(v, sizeof(v));
  v := Rot3DS.AsVector;
  virt.write(v, sizeof(v));
  v := Scale3DS.AsVector;
  virt.write(v, sizeof(v));

  MeshOBjects.WriteToFiler(virt);
  virt.Free;
end;

procedure TVXFile3DSFreeForm.DefineProperties(Filer: TFiler);
begin
  Filer.DefineBinaryProperty('MeshObjectsData', ReadMesh, WriteMesh, True);
end;

procedure TVXFile3DSFreeForm.BuildList(var rci: TVXRenderContextInfo);
begin
  glMultMatrixf(@FTransfMat);
  glMultMatrixf(@FScaleMat);

  glPushMatrix;
  glMultMatrixf(@FRefMat);
  inherited;
  glPopMatrix;

  if parent is TVXFile3DSFreeForm then
    ParentMatrix := (parent as TVXFile3DSFreeForm).ParentMatrix
  else
    ParentMatrix := IdentityHmgMatrix;

  ParentMatrix := MatrixMultiply(FScaleMat, ParentMatrix);
  ParentMatrix := MatrixMultiply(FTransfMat, ParentMatrix);
end;

procedure TVXFile3DSFreeForm.CoordinateChanged(Sender: TVXCustomCoordinates);
var
  quat, quat1, quat2: TQuaternion;
begin
  inherited;

  if Sender.ClassType = FRot3DS.ClassType then
  begin
    quat1 := MakeRotationQuaternion(FS_Rot3DS.AsAffineVector, FS_Rot3DS.W);
    quat2 := MakeRotationQuaternion(FRot3DS.AsAffineVector, FRot3DS.W);

    quat := QuaternionMultiply(quat1, quat2);
    NormalizeQuaternion(quat);
    FTransfMat := QuaternionToRotateMatrix(quat);
    NormalizeMatrix(FTransfMat);
  end;
  if Sender.ClassType = FScale3DS.ClassType then
  begin
    FScaleMat := CreateScaleMatrix(FScale3DS.AsAffineVector);
  end;
end;

function TVXFile3DSFreeForm.AxisAlignedDimensionsUnscaled: TVector;
var
  dMin, dMax: TAffineVector;
  mat: TMatrix;
begin
  MeshObjects.GetExtents(dMin, dMax);
  mat := ParentMatrix;
  mat := MatrixMultiply(FRefMat, mat);
  if not IsInfinite(dMin.X) then
    dMin := VectorTransform(dMin, mat);
  if not IsInfinite(dMax.X) then
    dMax := VectorTransform(dMax, mat);

  Result.X := (dMax.X - dMin.X) / 2;
  Result.Y := (dMax.Y - dMin.Y) / 2;
  Result.Z := (dMax.Z - dMin.Z) / 2;
  Result.W := 0;
end;

// BarycenterAbsolutePosition
//

function TVXFile3DSFreeForm.BarycenterAbsolutePosition: TVector;
var
  dMin, dMax: TAffineVector;
  mat: TMatrix;
begin
  MeshObjects.GetExtents(dMin, dMax);
  mat := ParentMatrix;
  mat := MatrixMultiply(FRefMat, mat);
  if not IsInfinite(dMin.X) then
    dMin := VectorTransform(dMin, mat);
  if not IsInfinite(dMax.X) then
    dMax := VectorTransform(dMax, mat);

  Result.X := (dMax.X + dMin.X) / 2;
  Result.Y := (dMax.Y + dMin.Y) / 2;
  Result.Z := (dMax.Z + dMin.Z) / 2;
  Result.W := 1;

  Result := LocalToAbsolute(Result);
end;

initialization
  RegisterClasses([TVXFile3DSLight, TVXFile3DSCamera, TVXFile3DSActor, TVXFile3DSFreeForm]);

end.

