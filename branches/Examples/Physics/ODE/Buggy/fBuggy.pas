unit fBuggy;

{ *************************************************************************
  *                                                                       *
  * Open Dynamics Engine, Copyright (C) 2001,2002 Russell L. Smith.       *
  * All rights reserved.  Email: russ@q12.org   Web: www.q12.org          *
  *                                                                       *
  * This library is free software; you can redistribute it and/or         *
  * modify it under the terms of EITHER:                                  *
  *   (1) The GNU Lesser General Public License as published by the Free  *
  *       Software Foundation; either version 2.1 of the License, or (at  *
  *       your option) any later version. The text of the GNU Lesser      *
  *       General Public License is included with this library in the     *
  *       file LICENSE.TXT.                                               *
  *   (2) The BSD-style license that is included with this library in     *
  *       the file LICENSE-BSD.TXT.                                       *
  *                                                                       *
  * This library is distributed in the hope that it will be useful,       *
  * but WITHOUT ANY WARRANTY; without even the implied warranty of        *
  * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the files    *
  * LICENSE.TXT and LICENSE-BSD.TXT for more details.                     *
  *                                                                       *
  ************************************************************************* }

{

  buggy with suspension.
  this also shows you how to use geom groups.

  **************************

  This ODE example was converted to Delphi
  by Mattias Fagerlund ( mattias@cambrianlabs.com)

}

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Classes,
  System.Math,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.Imaging.jpeg,
  Vcl.ComCtrls,
  Vcl.ExtCtrls,
  Vcl.StdCtrls,
   
  GLScene,
  GLObjects,
  GLCadencer,
  ODEImport,
  GLTexture,
  GLVectorGeometry,
  GLWin32Viewer,
  GLKeyBoard,
  GLShadowPlane,
  GLGraph,
  GLVectorTypes,
  GLGeomObjects,
  GLMaterial,
  GLCoordinates,
  GLCrossPlatform,
  GLBaseClasses;

const
  cZOOM = 1;
  cZOOM_CUBED = cZOOM * cZOOM * cZOOM;
  cLENGTH = 0.7 * cZOOM; // chassis length
  cWIDTH = 0.5 * cZOOM; // chassis width
  cHEIGHT = 0.2 * cZOOM; // chassis height
  cRADIUS = 0.18 * cZOOM; // wheel radius
  cSTARTZ = 0.5 * cZOOM; // starting height of chassis
  cCMASS = 1 * cZOOM; // chassis mass
  cWMASS = 0.15 * cZOOM; // wheel mass
  cWHEEL_OFFSET = 0 * cZOOM;

  // Change these two values, and you change the entire suspension of the buggy,
  // for a funny rubberband car, try cSUSPENSION_CFM=5...
  cSUSPENSION_ERP = 0.4;
  cSUSPENSION_CFM = 0.8;

  // This sets up friction on the surfaces
  // cFRICTION = 0.1;

  // Try it, it's fun!
  cWHEEL_WOBBLE = 0.0;
  cHEGHT_FIELD_DX = -3;

type
  TfrmBuggy = class(TForm)
    GLSceneViewer1: TGLSceneViewer;
    GLCadencer1: TGLCadencer;
    GLScene1: TGLScene;
    DC_Shadowing: TGLDummyCube;
    GLCamera1: TGLCamera;
    GLLightSource1: TGLLightSource;
    GLMaterialLibrary1: TGLMaterialLibrary;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label_Friction: TLabel;
    TrackBar_Friction: TTrackBar;
    GLShadowPlane1: TGLShadowPlane;
    HeightField1: TGLHeightField;
    Button_AddBall: TButton;
    CheckBox_Shadows: TCheckBox;
    CheckBox_StencilBuffer: TCheckBox;
    Timer1: TTimer;
    Label4: TLabel;
    Label_FPS: TLabel;
    CheckBox_Headlights: TCheckBox;
    DC_LightHolder: TGLDummyCube;
    Sphere1: TGLSphere;
    Cube1: TGLCube;
    TrackBar_ShadowStrength: TTrackBar;
    Label5: TLabel;
    CheckBox_ScissorTest: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure GLCadencer1Progress(Sender: TObject;
      const deltaTime, newTime: Double);
    procedure GLSceneViewer1MouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure FormMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure TrackBar_FrictionChange(Sender: TObject);
    procedure HeightField1GetHeight(const X, Y: Single; var z: Single;
      var color: TVector4f; var texPoint: TTexPoint);
    procedure Button_AddBallClick(Sender: TObject);
    procedure CheckBox_ShadowsClick(Sender: TObject);
    procedure CheckBox_StencilBufferClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure CheckBox_HeadlightsClick(Sender: TObject);
    procedure TrackBar_ShadowStrengthChange(Sender: TObject);
    procedure CheckBox_ScissorTestClick(Sender: TObject);
  private
     
  public
     
    world: PdxWorld;
    space: PdxSpace;
    // 0 = chassis, 1..3 = wheels
    body: array [0 .. 3] of PdxBody;
    joint: array [0 .. 2] of TdJointID; // joint[0] is the front wheel
    contactgroup: TdJointGroupID;

    BodySpace: PdxSpace;
    ground { ,geom_group } : PdxGeom;
    box: array [0 .. 0] of PdxGeom;
    sphere: array [0 .. 2] of PdxGeom;
    ground_box: PdxGeom;

    MainBodyCube: TGLCube;
    Cubes: array [0 .. 7] of TGLCube;

    // user commands
    speed, steer: Single;

    FMouseX, FMouseY, FoldMouseX, FoldMouseY: Integer;
    FCurrentShift, FoldShift: TShiftState;

    GeomList: TGeomList;
    MainCarBody: TGLCube;

    procedure CreateNewBall(X, Y, z: Single);
    procedure ShowCorners;
  end;

  TCorner = class
    pos: TdVector3;
    Depth: Single;
  end;

var
  frmBuggy: TfrmBuggy;
  GlobalFriction: Single;

  CornerList: TList;
  CornerCache: array [0 .. 7] of TCorner;

implementation

uses ODEGL;

{$R *.dfm}

// this formula defines the heightfield for the collider
function ColliderFormula(X, Y: Single): Single;
begin
  Result := 1 - (Sqr(X) + Sqr(Y)) * 0.15;

  if Sqrt(Sqr(X) + Sqr(Y)) < 1 then
    Result := Result - Sqrt(Sqr(X) + Sqr(Y)) * 0.1;
  // Result:=-0.4*cos(x*1.5)*cos(y*1.5);
  // Result:=0;
end;

function ColliderFormulaNormal(X, Y: Single): TAffineVector;
const
  DELTA = 0.2;
begin
  Result := CalcPlaneNormal(AffineVectorMake(X, Y, ColliderFormula(X, Y)),
    AffineVectorMake(X + DELTA, Y, ColliderFormula(X + DELTA, Y)),
    AffineVectorMake(X, Y + DELTA, ColliderFormula(X, Y + DELTA))) // }
end;

function CustomColliderFnSphere(o1, o2: PdxGeom; flags: Integer;
  contact: PdContactGeom; skip: Integer): Integer; cdecl;

  procedure AddContact(X, Y, z: Single; var nb: Integer);
  var
    n: TAffineVector;
    zs: Single;
  begin
    if nb >= flags then
      Exit;
    zs := ColliderFormula(X - cHEGHT_FIELD_DX, Y);
    if z < zs then
    begin
      contact.pos[0] := X;
      contact.pos[1] := Y;
      contact.pos[2] := zs;
      contact.pos[3] := 1;
      n := ColliderFormulaNormal(X - cHEGHT_FIELD_DX, Y);
      contact.normal[0] := -n.X;
      contact.normal[1] := -n.Y;
      contact.normal[2] := -n.z;
      contact.normal[3] := 0;
      contact.Depth := zs - z;
      contact.g1 := o1;
      contact.g2 := o2;
      contact := PdContactGeom(Integer(contact) + skip);
      Inc(nb);
    end;
  end;

var
  pos: PdVector3;
  r, dx, dy, dz: Single;
  i: Integer;
begin
  // collide o2 (a sphere) against a formula
  pos := dGeomGetPosition(o2);
  r := dGeomSphereGetRadius(o2);
  Result := 0;

  // collide below sphere center
  AddContact(pos[0], pos[1], pos[2] - r, Result);
  // test corona at 0.4 and 0.8 radius
  for i := 0 to 5 do
  begin
    SinCosine(DegToRadian(i * 60), r * 0.4, dy, dx);
    dz := r - Sqrt(Sqr(r) - Sqr(dx) - Sqr(dy));
    AddContact(pos[0] + dx, pos[1] + dy, pos[2] - r + dz, Result);
    SinCosine(DegToRadian(i * 60), r * 0.8, dy, dx);
    dz := r - Sqrt(Sqr(r) - Sqr(dx) - Sqr(dy));
    AddContact(pos[0] + dx, pos[1] + dy, pos[2] - r + dz, Result);
  end;
end;

function CornerSort(Item1, Item2: Pointer): Integer;
var
  c1, c2: TCorner;
begin
  c1 := TCorner(Item1);
  c2 := TCorner(Item2);

  if c1.Depth > c2.Depth then
    Result := -1
  else if c1.Depth = c2.Depth then
    Result := 0
  else
    Result := 1;
end;

function CustomColliderFnBox(o1, o2: PdxGeom; flags: Integer;
  contact: PdContactGeom; skip: Integer): Integer; cdecl;

  procedure AddContact(X, Y, z: Single);
  var
    zs: Single;
    Corner: TCorner;
  begin
    zs := ColliderFormula(X - cHEGHT_FIELD_DX, Y);
    if z < zs then
    begin
      // Pick the next free corner
      Corner := CornerCache[CornerList.Count];

      Corner.pos[0] := X;
      Corner.pos[1] := Y;
      Corner.pos[2] := zs;
      Corner.Depth := zs - z;

      CornerList.Add(Corner);
    end;
  end;

  procedure KeepDeepest(var nb: Integer);
  var
    i: Integer;
    n: TAffineVector;
    Corner: TCorner;
  begin
    CornerList.Sort(CornerSort);

    for i := 0 to CornerList.Count - 1 do
    begin
      if nb >= flags then
        Exit;

      Corner := TCorner(CornerList[i]);

      contact.pos[0] := Corner.pos[0];
      contact.pos[1] := Corner.pos[1];
      contact.pos[2] := Corner.pos[2];
      // contact.pos[3]:=1;
      n := ColliderFormulaNormal(Corner.pos[0] - cHEGHT_FIELD_DX,
        Corner.pos[1]);
      contact.normal[0] := -n.X;
      contact.normal[1] := -n.Y;
      contact.normal[2] := -n.z;
      // contact.normal[3]:=0;
      contact.Depth := Corner.Depth;
      contact.g1 := o1;
      contact.g2 := o2;
      contact := PdContactGeom(Integer(contact) + skip);
      Inc(nb);
    end;
  end;

var
  pos: PdVector3;
  body: PdxBody;
  Sides, dPos: TdVector3;
begin
  // Make sure the corner list is empty
  CornerList.Clear;

  // collide o2 (a box) against a formula
  pos := dGeomGetPosition(o2);
  dGeomBoxGetLengths(o2, Sides);
  Result := 0;

  body := dGeomGetBody(o2);

  if not Assigned(body) then
    Exit;

  dGeomBoxGetLengths(o2, Sides);

  dBodyVectorToWorld(body, Sides[0] / 2, Sides[1] / 2, Sides[2] / 2, dPos);
  AddContact(pos[0] + dPos[0], pos[1] + dPos[1], pos[2] + dPos[2]);
  AddContact(pos[0] - dPos[0], pos[1] - dPos[1], pos[2] - dPos[2]);

  dBodyVectorToWorld(body, Sides[0] / 2, Sides[1] / 2, -Sides[2] / 2, dPos);
  AddContact(pos[0] + dPos[0], pos[1] + dPos[1], pos[2] + dPos[2]);
  AddContact(pos[0] - dPos[0], pos[1] - dPos[1], pos[2] - dPos[2]);

  dBodyVectorToWorld(body, Sides[0] / 2, -Sides[1] / 2, Sides[2] / 2, dPos);
  AddContact(pos[0] + dPos[0], pos[1] + dPos[1], pos[2] + dPos[2]);
  AddContact(pos[0] - dPos[0], pos[1] - dPos[1], pos[2] - dPos[2]);

  dBodyVectorToWorld(body, -Sides[0] / 2, Sides[1] / 2, Sides[2] / 2, dPos);
  AddContact(pos[0] + dPos[0], pos[1] + dPos[1], pos[2] + dPos[2]);
  AddContact(pos[0] - dPos[0], pos[1] - dPos[1], pos[2] - dPos[2]);

  KeepDeepest(Result);

  { for i := 0 to CornerList.Count-1 do
    TCorner(CornerList[i]).Free;// }

  CornerList.Clear;

  { // test corona at 0.4 and 0.8 radius
    for i:=0 to 5 do begin
    SinCos(DegToRad(i*60), r*0.4, dy, dx);
    dz:=r-Sqrt(Sqr(r)-Sqr(dx)-Sqr(dy));
    AddContact(pos[0]+dx,
    pos[1]+dy,
    pos[2]-r+dz, Result);
    SinCos(DegToRad(i*60), r*0.8, dy, dx);
    dz:=r-Sqrt(Sqr(r)-Sqr(dx)-Sqr(dy));
    AddContact(pos[0]+dx,
    pos[1]+dy,
    pos[2]-r+dz, Result);
    end;// }
end;

function CustomGetColliderFnFn(num: Integer): TdColliderFn; cdecl;
begin
  if num = dSphereClass then
    Result := CustomColliderFnSphere
  else if num = dBoxClass then
    Result := CustomColliderFnBox
  else
    Result := nil;
end;


// this is called by dSpaceCollide when two objects in space are
// potentially colliding.

procedure nearCallback(data: Pointer; o1, o2: PdxGeom); cdecl;
const
  cN = 10;

var
  i, n: Integer;
  b1, b2: PdxBody;
  c: TdJointID;

  contact: array [0 .. cN - 1] of TdContact;
begin
  // exit without doing anything if the two bodies are connected by a joint
  b1 := dGeomGetBody(o1);
  b2 := dGeomGetBody(o2);
  if (Assigned(b1) and Assigned(b2) and (dAreConnected(b1, b2) <> 0)) then
    Exit; // }

  n := dCollide(o1, o2, cN, contact[0].geom, sizeof(TdContact));
  if (n > 0) then
  begin
    for i := 0 to n - 1 do
    begin
      contact[i].surface.mode := ord(dContactSlip1) or ord(dContactSlip2) or
        ord(dContactSoftERP) or ord(dContactSoftCFM);

      contact[i].surface.mu := GlobalFriction; // dInfinity;

      contact[i].surface.slip1 := 0.1;
      contact[i].surface.slip2 := 0.1;
      contact[i].surface.soft_erp := 0.5;
      contact[i].surface.soft_cfm := 0.3;
      c := dJointCreateContact(frmBuggy.world, frmBuggy.contactgroup,
        @contact[i]);

      dJointAttach(c, dGeomGetBody(contact[i].geom.g1),
        dGeomGetBody(contact[i].geom.g2)); // }
    end;
  end;
end;

procedure TfrmBuggy.FormCreate(Sender: TObject);
var
  i: Integer;
  m: TdMass;
  q: TdQuaternion;
  a: PdVector3;
  r: TdMatrix3;

  Cube: TGLCube;
  DummyCube: TGLDummyCube;
  Cylinder: TGLCylinder;
  StrDir: String;

  procedure CreateBody;
  var
    i: Integer;
  begin
    // chassis body
    body[0] := dBodyCreate(world);
    dBodySetPosition(body[0], 0, 0, cSTARTZ);
    dMassSetBox(m, 1, cLENGTH, cWIDTH, cHEIGHT);
    dMassAdjust(m, cCMASS);
    dBodySetMass(body[0], @m);
    box[0] := dCreateBox(nil, cLENGTH, cWIDTH, cHEIGHT);

    dGeomSetBody(box[0], body[0]);

    // CREATE THE GLSCENE PARTS FOR THE CAR BODY
    Cube := TGLCube(DC_Shadowing.AddNewChild(TGLCube));
    MainCarBody := Cube;
    MainBodyCube := Cube;
    PdxGeom(box[0]).data := Cube;
    CopyCubeSizeFromBox(Cube, box[0]);

    Cube.Material.MaterialLibrary := GLMaterialLibrary1;
    Cube.Material.LibMaterialName := 'Solstickan';

    // Keep the camera glued to the body
    GLCamera1.TargetObject := Cube;

    // wheel bodies
    for i := 1 to 3 do
    begin
      body[i] := dBodyCreate(world);

      dQFromAxisAndAngle(q, 1, 0, 0, PI * 0.5);
      dBodySetQuaternion(body[i], q);
      dMassSetSphere(m, 1, cRADIUS);
      dMassAdjust(m, cWMASS);
      dBodySetMass(body[i], @m);
      sphere[i - 1] := dCreateSphere(nil, cRADIUS);
      dGeomSetBody(sphere[i - 1], body[i]);

      // CREATE THE GLSCENE PARTS FOR THE CAR BODY - WHEELS
      { GLSphere := TGLSphere(GLScene1.Objects.AddNewChild(TGLSphere));
        PdxGeom(sphere[i-1]).data := GLSphere;
        GLSphere.Radius := cRADIUS;
        GLSphere.Stacks := 8;
        GLSphere.Slices := 8;// }

      // Note that the wheels are spheres, but the glscene representation is
      // cylinders - it simply looks better. Cylinders in ODE can't do collission
      // with boxes, so they can't be used for wheels.
      DummyCube := TGLDummyCube(DC_Shadowing.AddNewChild(TGLDummyCube));
      PdxGeom(sphere[i - 1]).data := DummyCube;

      Cylinder := TGLCylinder(DummyCube.AddNewChild(TGLCylinder));

      Cylinder.BottomRadius := cRADIUS;
      Cylinder.TopRadius := cRADIUS;
      Cylinder.Height := 0.02;
      Cylinder.Stacks := 1;
      Cylinder.Slices := 12;
      Cylinder.Direction.X := 0;
      Cylinder.Direction.Y := 1;
      Cylinder.Direction.z := 0;

      Cylinder.Material.MaterialLibrary := GLMaterialLibrary1;
      Cylinder.Material.LibMaterialName := 'Wheel'; // }
    end;

    dBodySetPosition(body[1], 0.5 * cLENGTH, 0, cSTARTZ - cHEIGHT * 0.5 -
      cWHEEL_OFFSET);
    dBodySetPosition(body[2], -0.5 * cLENGTH, cWIDTH * 0.5,
      cSTARTZ - cHEIGHT * 0.5 - cWHEEL_OFFSET);
    dBodySetPosition(body[3], -0.5 * cLENGTH, -cWIDTH * 0.5,
      cSTARTZ - cHEIGHT * 0.5 - cWHEEL_OFFSET);

    // front wheel hinge
    // (this code is commented out in the c source)
    {
      joint[0] = dJointCreateHinge2 (world,0);
      dJointAttach (joint[0],body[0],body[1]);
      const dReal *a = dBodyGetPosition (body[1]);
      dJointSetHinge2Anchor (joint[0],a[0],a[1],a[2]);
      dJointSetHinge2Axis1 (joint[0],0,0,1);
      dJointSetHinge2Axis2 (joint[0],0,1,0);
    }

    // front and back wheel hinges
    for i := 0 to 2 do
    begin
      joint[i] := dJointCreateHinge2(world, 0);
      dJointAttach(joint[i], body[0], body[i + 1]);
      a := dBodyGetPosition(body[i + 1]);
      dJointSetHinge2Anchor(joint[i], a[0], a[1], a[2]);
      dJointSetHinge2Axis1(joint[i], 0, 0, 1);
      dJointSetHinge2Axis2(joint[i], 0, 1, 0);
    end;

    // set joint suspension
    for i := 0 to 2 do
    begin
      dJointSetHinge2Param(joint[i], dParamSuspensionERP, cSUSPENSION_ERP);
      dJointSetHinge2Param(joint[i], dParamSuspensionCFM, cSUSPENSION_CFM); // }
    end;

    // lock back wheels along the steering axis
    for i := 0 to 2 do
    begin
      // set stops to make sure wheels always stay in alignment
      dJointSetHinge2Param(joint[i], dParamLoStop, -cWHEEL_WOBBLE);
      dJointSetHinge2Param(joint[i], dParamHiStop, +cWHEEL_WOBBLE);
      // the following alternative method is no good as the wheels may get out
      // of alignment:
      // dJointSetHinge2Param (joint[i],dParamVel,0);
      // dJointSetHinge2Param (joint[i],dParamFMax,dInfinity);
    end;

    // GeomGroup has been deprecated, use Space instead!
    // create geometry group and add it to the space
    { geom_group := dCreateGeomGroup (space);
      dGeomGroupAdd (geom_group,box[0]);
      dGeomGroupAdd (geom_group,sphere[0]);
      dGeomGroupAdd (geom_group,sphere[1]);
      dGeomGroupAdd (geom_group,sphere[2]);// }
    BodySpace := dSimpleSpaceCreate(space);
    dSpaceSetCleanup(BodySpace, 0);
    dSpaceAdd(BodySpace, box[0]);
    dSpaceAdd(BodySpace, sphere[0]);
    dSpaceAdd(BodySpace, sphere[1]);
    dSpaceAdd(BodySpace, sphere[2]);
  end;

  procedure RegisterCollider;
  var
    customColliderClass: TdGeomClass;
    newClassNum: Integer;
    newCollider: PdxGeom;
  begin
    // register our custom collider class
    customColliderClass.bytes := 0; // I don't need any memory
    customColliderClass.collider := CustomGetColliderFnFn;
    customColliderClass.aabb := dInfiniteAABB;
    customColliderClass.aabb_test := nil;
    customColliderClass.dtor := nil;
    newClassNum := dCreateGeomClass(customColliderClass);

    // create a geom of that class and add it to the space
    newCollider := dCreateGeom(newClassNum);
    dSpaceAdd(space, newCollider);
  end;

begin
  for i := 0 to 7 do
  begin
    Cubes[i] := TGLCube(DC_Shadowing.AddNewChild(TGLCube));
    Cubes[i].CubeWidth := 0.1;
    Cubes[i].CubeHeight := 0.1;
    Cubes[i].CubeDepth := 0.1;
    Cubes[i].Position.z := -1;
  end;

  StrDir := GetCurrentDir;
  SetCurrentDir(StrDir);
  GLMaterialLibrary1.Materials[0].Material.Texture.Image.LoadFromFile
    ('\media\WheelOfHorror.bmp');
  GLMaterialLibrary1.Materials[1].Material.Texture.Image.LoadFromFile
    ('\media\beigemarble.jpg');
  GLMaterialLibrary1.Materials[2].Material.Texture.Image.LoadFromFile
    ('\media\solstickan.JPG');
  GLMaterialLibrary1.Materials[3].Material.Texture.Image.LoadFromFile
    ('\media\arrow.JPG');

  CornerList := TList.Create;

  // This way we won't have to create and destroy a lot of corners
  for i := 0 to 7 do
    CornerCache[i] := TCorner.Create;

  // create world
  world := dWorldCreate();
  space := dHashSpaceCreate(nil);
  contactgroup := dJointGroupCreate(0);
  dWorldSetGravity(world, 0, 0, -0.5);
  ground := dCreatePlane(space, 0, 0, 1, 0);

  CreateBody;

  // environment
  ground_box := dCreateBox(space, 2, 1.5, 1);

  dRFromAxisAndAngle(r, 0, 1, 0, -0.15);
  dGeomSetPosition(ground_box, 2, 0, -0.34);
  dGeomSetRotation(ground_box, r);

  // CREATE THE GLSCENE PARTS FOR THE GROUND
  Cube := TGLCube(DC_Shadowing.AddNewChild(TGLCube));
  PdxGeom(ground_box).data := Cube;

  CopyCubeSizeFromBox(Cube, ground_box);

  Cube.Material.MaterialLibrary := GLMaterialLibrary1;
  Cube.Material.LibMaterialName := 'Arrow';

  PositionSceneObject(TGLBaseSceneObject(PdxGeom(ground_box).data), ground_box);

  HeightField1.Position.X := cHEGHT_FIELD_DX;

  RegisterCollider;

  GeomList := TGeomList.Create;

  // CheckBox_HeadlightsClick(self);

  // Create a few random balls
  for i := 0 to 10 do
    CreateNewBall(GLShadowPlane1.Width * (random - 0.5), GLShadowPlane1.Height *
      (random - 0.5), 3 * random + 1);

  Show;
  TrackBar_FrictionChange(Sender);

  // Caption := Format('%s',[IntToHex(Space.SpaceType, 5)]);
  // Caption := Caption + Format('%s (%d to %d)',[IntToHex(PdxHashSpace(Space).SpaceType, 4), PdxHashSpace(Space).global_minlevel, PdxHashSpace(Space).global_maxlevel]);
end;

procedure TfrmBuggy.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  GLCadencer1.Enabled := false;

  dJointGroupDestroy(contactgroup);
  dSpaceDestroy(space);
  dWorldDestroy(world);
  dGeomDestroy(box[0]);
  dGeomDestroy(sphere[0]);
  dGeomDestroy(sphere[1]);
  dGeomDestroy(sphere[2]);
end;

procedure TfrmBuggy.GLCadencer1Progress(Sender: TObject;
  const deltaTime, newTime: Double);

  procedure HandleBuggySteering;
  const
    cACCEL = 0.3 / 5;
    cTURN_SPEED = 0.5 / 10;
  var
    MotorRunning: Boolean;
    v: TdReal;
  begin
    if IsKeyDown(VK_RIGHT) then
      steer := steer + cTURN_SPEED

    else if IsKeyDown(VK_LEFT) then
      steer := steer - cTURN_SPEED
    else
      steer := steer * 0.95;

    MotorRunning := true;

    if IsKeyDown(VK_UP) then
    begin
      if speed >= 0 then
        speed := speed + cACCEL
      else
        speed := 0;
    end

    else if IsKeyDown(VK_DOWN) then
    begin
      if speed <= 0 then
        speed := speed - cACCEL
      else
        speed := 0;
    end
    else
    begin
      MotorRunning := false;
      speed := speed * 0.95;
    end;

    if IsKeyDown(' ') then
    begin
      // HANDBRAKE!
      dJointSetHinge2Param(joint[1], dParamVel2, 0);
      dJointSetHinge2Param(joint[1], dParamFMax2, 0.15 * cZOOM_CUBED); // }

      dJointSetHinge2Param(joint[2], dParamVel2, 0);
      dJointSetHinge2Param(joint[2], dParamFMax2, 0.15 * cZOOM_CUBED); // }
    end
    else
    begin
      // Not handbraking
      dJointSetHinge2Param(joint[1], dParamFMax2, 0.0);
      dJointSetHinge2Param(joint[2], dParamFMax2, 0.0); // }
    end;

    if IsKeyDown('x') then
    begin
      dBodyAddForce(body[1], 0, 0, 0.45 * cZOOM);
    end;

    if MotorRunning then
    begin
      // motor
      dJointSetHinge2Param(joint[0], dParamVel2, -speed);
      dJointSetHinge2Param(joint[0], dParamFMax2, 0.1 * cZOOM_CUBED);

      // Uncomment for three wheel drive!
      { dJointSetHinge2Param (joint[1],dParamVel2,-speed);
        dJointSetHinge2Param (joint[1],dParamFMax2,0.1);

        dJointSetHinge2Param (joint[2],dParamVel2,-speed);
        dJointSetHinge2Param (joint[2],dParamFMax2,0.1);// }
    end
    else
    begin
      // the clutch is in! (not accel or breaking)
      dJointSetHinge2Param(joint[0], dParamVel2, 0);
      dJointSetHinge2Param(joint[0], dParamFMax2, 0.005 * cZOOM_CUBED);

      // Uncomment for three wheel drive!
      { dJointSetHinge2Param (joint[1],dParamVel2,0);
        dJointSetHinge2Param (joint[1],dParamFMax2,0.01);

        dJointSetHinge2Param (joint[2],dParamVel2,0);
        dJointSetHinge2Param (joint[2],dParamFMax2,0.01);// }
    end;

    if steer > 0.75 then
      steer := 0.75;

    if steer < -0.75 then
      steer := -0.75;

    if speed > 15 then
      speed := 15;

    if speed < -15 then
      speed := -15;

    // steering
    v := steer - dJointGetHinge2Angle1(joint[0]);
    if (v > 0.1) then
      v := 0.1;
    if (v < -0.1) then
      v := -0.1;
    v := v * 10.0;

    dJointSetHinge2Param(joint[0], dParamVel, v);
    dJointSetHinge2Param(joint[0], dParamFMax, 0.2 * cZOOM_CUBED);
    dJointSetHinge2Param(joint[0], dParamLoStop, -0.75);
    dJointSetHinge2Param(joint[0], dParamHiStop, 0.75);
    dJointSetHinge2Param(joint[0], dParamFudgeFactor, 0.1);
  end;

var
  i: Integer;
  Rect: TRect;
  aBuffer: TGLSceneBuffer;

begin
  HandleBuggySteering;

  if CheckBox_ScissorTest.Checked then
  begin
    Rect := GLShadowPlane1.ScreenRect(aBuffer);
    Caption := Format('Left=%d, Top=%d, Bottom=%d, Right=%d',
      [Rect.Left, Rect.Top, Rect.Bottom, Rect.Right]);
  end;

  if ssLeft in FCurrentShift then
    GLCamera1.MoveAroundTarget(FoldMouseY - FMouseY, FoldMouseX - FMouseX);

  FoldMouseX := FMouseX;
  FoldMouseY := FMouseY;

  dSpaceCollide(space, nil, nearCallback);
  dWorldQuickStep(world, 0.05);

  // remove all contact joints
  dJointGroupEmpty(contactgroup);

  for i := 0 to 3 do
    PositionSceneObject(TGLBaseSceneObject(PdxGeom(box[i]).data), box[i]);

  { if CheckBox_ShowCorners.Checked then
    ShowCorners;// }

  RenderGeomList(GeomList);
end;

procedure TfrmBuggy.GLSceneViewer1MouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
begin
  FoldShift := FCurrentShift;
  FCurrentShift := Shift;

  FoldMouseX := FMouseX;
  FoldMouseY := FMouseY;
  FMouseX := X;
  FMouseY := Y;
end;

procedure TfrmBuggy.FormMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  GLCamera1.AdjustDistanceToTarget(Power(1.1, WheelDelta / 120));
end;

procedure TfrmBuggy.TrackBar_FrictionChange(Sender: TObject);
begin
  GlobalFriction := TrackBar_Friction.Position / 100;
  Label_Friction.Caption := Format('Friction : %f', [GlobalFriction]);
  FocusControl(nil);
end;

// This is fairly pointless, but it's a demo of how to find the corners of a box,
// which is used in mesh collission detection
procedure TfrmBuggy.ShowCorners;
var
  dPos: TdVector3;
  pos: PdVector3;
  Sides: TdVector3;
begin
  pos := @(body[0].Posr);

  dGeomBoxGetLengths(box[0], Sides);

  dBodyVectorToWorld(body[0], Sides[0] / 2, Sides[1] / 2, Sides[2] / 2, dPos);
  Cubes[0].Position.SetPoint(pos[0] + dPos[0], pos[1] + dPos[1],
    pos[2] + dPos[2]);
  Cubes[1].Position.SetPoint(pos[0] - dPos[0], pos[1] - dPos[1],
    pos[2] - dPos[2]);

  dBodyVectorToWorld(body[0], Sides[0] / 2, Sides[1] / 2, -Sides[2] / 2, dPos);
  Cubes[2].Position.SetPoint(pos[0] + dPos[0], pos[1] + dPos[1],
    pos[2] + dPos[2]);
  Cubes[3].Position.SetPoint(pos[0] - dPos[0], pos[1] - dPos[1],
    pos[2] - dPos[2]);

  dBodyVectorToWorld(body[0], Sides[0] / 2, -Sides[1] / 2, Sides[2] / 2, dPos);
  Cubes[4].Position.SetPoint(pos[0] + dPos[0], pos[1] + dPos[1],
    pos[2] + dPos[2]);
  Cubes[5].Position.SetPoint(pos[0] - dPos[0], pos[1] - dPos[1],
    pos[2] - dPos[2]);

  dBodyVectorToWorld(body[0], -Sides[0] / 2, Sides[1] / 2, Sides[2] / 2, dPos);
  Cubes[6].Position.SetPoint(pos[0] + dPos[0], pos[1] + dPos[1],
    pos[2] + dPos[2]);
  Cubes[7].Position.SetPoint(pos[0] - dPos[0], pos[1] - dPos[1],
    pos[2] - dPos[2]);
end;

procedure TfrmBuggy.HeightField1GetHeight(const X, Y: Single; var z: Single;
  var color: TVector4f; var texPoint: TTexPoint);
begin
  z := ColliderFormula(X, Y);
end;

procedure TfrmBuggy.CheckBox_ShadowsClick(Sender: TObject);
begin
  if CheckBox_Shadows.Checked then
    GLShadowPlane1.ShadowedLight := GLLightSource1
  else
    GLShadowPlane1.ShadowedLight := nil;
end;

procedure TfrmBuggy.CheckBox_StencilBufferClick(Sender: TObject);
begin
  if CheckBox_StencilBuffer.Checked then
    GLSceneViewer1.Buffer.ContextOptions := GLSceneViewer1.Buffer.ContextOptions
      + [roStencilBuffer]
  else
    GLSceneViewer1.Buffer.ContextOptions := GLSceneViewer1.Buffer.ContextOptions
      - [roStencilBuffer];
end;

procedure TfrmBuggy.Timer1Timer(Sender: TObject);
begin
  Label_FPS.Caption := Format('%.1f FPS', [GLSceneViewer1.FramesPerSecond]);
  GLSceneViewer1.ResetPerformanceMonitor;
end;

procedure TfrmBuggy.CheckBox_HeadlightsClick(Sender: TObject);
begin
  GLLightSource1.Parent.Remove(GLLightSource1, false);

  if CheckBox_Headlights.Checked then
  begin
    MainBodyCube.AddChild(GLLightSource1);
    Cube1.Visible := true;
  end
  else
  begin
    DC_LightHolder.AddChild(GLLightSource1);
    Cube1.Visible := false;
  end;
end;

procedure TfrmBuggy.Button_AddBallClick(Sender: TObject);
begin
  // CreateNewBall(body[0].Posr[0], body[0].Posr[1], body[0].Posr[2] + 2);
  CreateNewBall(0, 0, 2);
end;

procedure TfrmBuggy.CreateNewBall(X, Y, z: Single);
var
  r: TdReal;
  m: TdMass;
  Ball: PdxBody;
  geom: PdxGeom;
begin
  r := (random * 0.7 + 0.2);
  dMassSetSphere(m, 1, r);

  Ball := dBodyCreate(world);

  Ball.Posr.pos[0] := X;
  Ball.Posr.pos[1] := Y;
  Ball.Posr.pos[2] := z;

  geom := dCreateSphere(space, r);

  dGeomSetBody(geom, Ball);
  dBodySetMass(Ball, @m);

  // Add the GLScene object
  geom.data := TGLSphere(DC_Shadowing.AddNewChild(TGLSphere));

  with TGLSphere(geom.data) do
  begin
    Position.z := 500; // avoid "birth" flicker
    Radius := r;
    Material.FrontProperties.Diffuse.color := VectorMake(random, random,
      random, 0);
  end;

  GeomList.Add(geom);
end;

procedure TfrmBuggy.TrackBar_ShadowStrengthChange(Sender: TObject);
begin
  GLShadowPlane1.ShadowColor.Alpha := (TrackBar_ShadowStrength.Position) / 100;
  FocusControl(nil);
end;

procedure TfrmBuggy.CheckBox_ScissorTestClick(Sender: TObject);
begin
  if CheckBox_ScissorTest.Checked then
    GLShadowPlane1.ShadowOptions := GLShadowPlane1.ShadowOptions + [spoScissor]
  else
    GLShadowPlane1.ShadowOptions := GLShadowPlane1.ShadowOptions - [spoScissor];
end;

end.
