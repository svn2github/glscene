{: Clothify demo.<p>

   Caution: this demo mixes several experimental thingies, and will probably be
            cleaned-up/splitted to be easier to follow, ad interim, you enter
            the jungle below at your own risks :) 
}
unit fClothify;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  GLObjects, GLScene, GLVectorFileObjects, GLWin32Viewer, GLMisc,
  GLFileMS3D, VerletClasses, VectorTypes, VectorLists, Geometry, GLTexture,
  OpenGL12, StdCtrls, GLFileSMD, GLCadencer, ExtCtrls, GLShadowPlane,
  GLVerletClothify, ComCtrls, jpeg, GLFile3DS, ODEImport, ODEGL;

type
  TfrmClothify = class(TForm)
    GLScene1: TGLScene;
    GLSceneViewer1: TGLSceneViewer;
    GLCamera1: TGLCamera;
    GLActor1: TGLActor;
    GLDummyCube1: TGLDummyCube;
    GLLightSource1: TGLLightSource;
    GLMaterialLibrary1: TGLMaterialLibrary;
    GLCadencer1: TGLCadencer;
    Label1: TLabel;
    Timer1: TTimer;
    Button_LoadMesh: TButton;
    Label2: TLabel;
    ComboBox_MeshName: TComboBox;
    ComboBox_ConstraintType: TComboBox;
    GLSphere1: TGLSphere;
    GLCylinder1: TGLCylinder;
    GLShadowPlane1: TGLShadowPlane;
    GLCube1: TGLCube;
    ComboBox_Collider: TComboBox;
    CheckBox_Pause: TCheckBox;
    Label4: TLabel;
    Label5: TLabel;
    TrackBar_Slack: TTrackBar;
    Label3: TLabel;
    GLCube_Stair1: TGLCube;
    GLDummyCube_Stairs: TGLDummyCube;
    GLCube_Stair2: TGLCube;
    GLCube_Stair3: TGLCube;
    GLCube_Stair4: TGLCube;
    GLDummyCube2: TGLDummyCube;
    TrackBar_Iterations: TTrackBar;
    Label6: TLabel;
    CheckBox_Weld: TCheckBox;
    GL_Capsule: TGLCylinder;
    GLSphere2: TGLSphere;
    GLSphere3: TGLSphere;
    TrackBar_Friction: TTrackBar;
    Label7: TLabel;
    GLDummyCube_Light: TGLDummyCube;
    procedure GLSceneViewer1MouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure GLCadencer1Progress(Sender: TObject; const deltaTime,
      newTime: Double);
    procedure Timer1Timer(Sender: TObject);
    procedure Button_LoadMeshClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure TrackBar_SlackChange(Sender: TObject);
    function GetSlack : single;
    procedure TrackBar_IterationsChange(Sender: TObject);
    procedure FormMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure TrackBar_FrictionChange(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    mx, my : integer;

    VerletAssembly : TVerletAssembly;
    EdgeDetector : TEdgeDetector;

    world : PdxWorld;
    space : PdxSpace;
    ODESphere: PdxGeom;
    body : PdxBody;
    contactgroup : TdJointGroupID;

    VCSphere : TVCSphere;
  end;

var
  frmClothify: TfrmClothify;

implementation

{$R *.dfm}

function myVectorMake(Vector : TVector) : TAffineVector;
begin
  result[0] := Vector[0];
  result[1] := Vector[1];
  result[2] := Vector[2];
end;

function myVectorMake2(x,y,z : single) : TAffineVector;
begin
  result[0] := x;
  result[1] := y;
  result[2] := z;
end;

procedure TfrmClothify.Button_LoadMeshClick(Sender: TObject);
var
  Floor : TVCFloor;
  Capsule : TVCCapsule;
  Sides : TAffineVector;
  Cube : TVCCube;
  s : string;
  f : single;
  p : Integer;

  procedure CreateCubeFromGLCube(GLCube : TGLCube);
  begin
    Cube := TVCCube.Create(VerletAssembly);
    Cube.Location := myVectorMake(GLCube.AbsolutePosition);
    Cube.FrictionRatio := 0.1;
    Sides[0] := GLCube.CubeWidth * 1.1;
    Sides[1] := GLCube.CubeHeight * 1.1;
    Sides[2] := GLCube.CubeDepth * 1.1;
    Cube.Sides := Sides;//}
  end;

  procedure CreateODEWorld;
  var
    m : TdMass;

  begin
    GLSphere1.Visible := true;
    world := dWorldCreate;
    dWorldSetGravity (world,0,-9.81,0);

    contactgroup := dJointGroupCreate (0);
    space := dHashSpaceCreate(nil);
    body := dBodyCreate(world);
    dMassSetSphere (m,0.1,GLSphere1.Radius);
    dCreatePlane (space,0,1,0,GLShadowPlane1.Position.Y);


    ODESphere := dCreateSphere (space, GLSphere1.Radius);

    dGeomSetBody (ODESphere, body);
    dBodySetMass (body,m);

    ODESphere.data := GLSphere1;

    PositionSceneObjectForGeom(ODESphere);
  end;
  
begin
  randomize;

  if world<>nil then
  begin
    dWorldDestroy(world);
    world := nil;
    GLSphere1.Position.AsAffineVector := NullVector;
  end;

  FreeAndNil(VerletAssembly);
  FreeAndNil(EdgeDetector);

  s := ComboBox_MeshName.Text;

  DecimalSeparator := '.';

  p:=Pos(',', s);
  if p>0 then begin
      f := StrToFloatDef(Trim(Copy(s, p+1, MaxInt)), 1);
      GLActor1.Scale.AsVector := VectorMake(f,f,f,0)
  end else GLActor1.Scale.AsVector := XYZHmgVector;

  GLActor1.AutoCentering := [macUseBarycenter];
  GLActor1.LoadFromFile(Trim(Copy(s, 1, p-1)));
  GLActor1.Reference := aarNone;

  GLActor1.Roll(random*360);
  GLActor1.Turn(random*360);//}

  GLSphere1.Visible := false;
  GLCylinder1.Visible := false;
  GLCube1.Visible := false;
  GLDummyCube_Stairs.Visible := False;
  GL_Capsule.Visible := False;

  case ComboBox_Collider.ItemIndex of
    0,-1 : GLSphere1.Visible := true;
    1 : GLCylinder1.Visible := true;
    2 : GLCube1.Visible := true;
    3 : GLDummyCube_Stairs.Visible := true;
    4 : GL_Capsule.Visible := true;
    5 : CreateODEWorld;
  end;

  EdgeDetector := TEdgeDetector.Create(GLActor1);

  if not CheckBox_Weld.Checked then
    EdgeDetector.WeldDistance := -1;
      
  EdgeDetector.ProcessMesh;

  if ComboBox_ConstraintType.ItemIndex=0 then
    VerletAssembly := EdgeDetector.CreateVAWithSticks(GetSlack)
  else
    VerletAssembly := EdgeDetector.CreateVAWithForces(1000,100, GetSlack);//}

  // VerletAssembly.Nodes[0].NailedDown := true;

  TVFGravity.Create(VerletAssembly);

  Floor := TVCFloor.Create(VerletAssembly);
  Floor.Location := VectorAdd(GLShadowPlane1.Position.AsAffineVector, myVectorMake2(0,0.1,0));
  Floor.Normal := GLShadowPlane1.Direction.AsAffineVector;

  Floor.FrictionRatio := 0.6;

  if GLSphere1.Visible then begin
     VCSphere := TVCSphere.Create(VerletAssembly);
     VCSphere.Radius := GLSphere1.Radius;
     VCSphere.Location := myVectorMake(GLSphere1.AbsolutePosition);
  end;

  if GLCube1.Visible then begin
    CreateCubeFromGLCube(GLCube1);
  end;

  if GLCylinder1.Visible then begin
     Capsule := TVCCapsule.Create(VerletAssembly);
     Capsule.Radius := GLCylinder1.TopRadius;
     Capsule.Location := AffineVectorMake(GLCylinder1.AbsolutePosition);
     Capsule.Axis := AffineVectorMake(GLCylinder1.AbsoluteUp);//}
     Capsule.Length := 20;
     Capsule.FrictionRatio := 0.6;
  end;

  if GL_Capsule.Visible then begin
     Capsule := TVCCapsule.Create(VerletAssembly);
     Capsule.Radius := GL_Capsule.TopRadius;
     Capsule.Location := AffineVectorMake(GL_Capsule.AbsolutePosition);
     Capsule.Axis := AffineVectorMake(GL_Capsule.AbsoluteUp);//}
     Capsule.Length := GL_Capsule.Height;
     Capsule.FrictionRatio := 0.6;
  end;

  if GLDummyCube_Stairs.Visible then begin
    CreateCubeFromGLCube(GLCube_Stair1);
    CreateCubeFromGLCube(GLCube_Stair2);
    CreateCubeFromGLCube(GLCube_Stair3);
    CreateCubeFromGLCube(GLCube_Stair4);
  end;

  VerletAssembly.SimTime := GLCadencer1.GetCurrentTime;
  VerletAssembly.MaxDeltaTime := 0.01;
  VerletAssembly.Iterations := TrackBar_Iterations.Position;

  TrackBar_FrictionChange(nil);
end;

procedure TfrmClothify.GLSceneViewer1MouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  if ssLeft in Shift then
    GLCamera1.MoveAroundTarget(my-y, mx-x);

  mx := x;
  my := y
end;

procedure nearCallback (data : pointer; o1, o2 : PdxGeom); cdecl;
const
  cCOL_MAX = 3;
var
  i : integer;
  b1, b2 : PdxBody;
  numc : integer;
  contact : array[0..cCOL_MAX-1] of TdContact;
  c : TdJointID;
begin
  // exit without doing anything if the two bodies are connected by a joint
  b1 := dGeomGetBody(o1);
  b2 := dGeomGetBody(o2);
  if (assigned(b1) and assigned(b2) and (dAreConnected (b1,b2)<>0)) then
    exit;//}

  for i :=0 to cCOL_MAX-1 do
  begin
    contact[i].surface.mode := dContactBounce;

    // This determines friction, play around with it!
    contact[i].surface.mu := 10e9; //dInfinity; SHOULD BE INFINITY!
    contact[i].surface.mu2 := 0;
    contact[i].surface.bounce := 0.5;//0.5;
    contact[i].surface.bounce_vel := 0.1;
  end;

  numc := dCollide (o1,o2,cCOL_MAX,contact[0].geom,sizeof(TdContact));
  if (numc>0) then
  begin
    // dMatrix3 RI;
    // dRSetIdentity (RI);
    // const dReal ss[3] = {0.02,0.02,0.02};
    for i := 0 to numc-1 do
    begin
      c := dJointCreateContact (frmClothify.world,frmClothify.contactgroup,contact[i]);
      dJointAttach (c,b1,b2);
      // dsDrawBox (contact[i].geom.pos,RI,ss);
    end;
  end;
end;

procedure TfrmClothify.GLCadencer1Progress(Sender: TObject;
  const deltaTime, newTime: Double);
var
   i : Integer;
begin
   if CheckBox_Pause.Checked then
      VerletAssembly.SimTime := newTime
   else begin
      if world <> nil then begin
         PositionSceneObjectForGeom(ODESphere);
         VCSphere.Location := GLSphere1.Position.AsAffineVector;

         dBodyAddForce(dGeomGetBody(ODESphere),
                       VCSphere.KickbackForce[0],
                       VCSphere.KickbackForce[1],
                       VCSphere.KickbackForce[2]);

         dSpaceCollide (space,nil,nearCallback);
         dWorldStep(World, VerletAssembly.MaxDeltaTime);
         dJointGroupEmpty (contactgroup);
      end;

      VerletAssembly.Progress(VerletAssembly.MaxDeltaTime, newTime);

      GLActor1.StructureChanged;
   end;
end;

procedure TfrmClothify.Timer1Timer(Sender: TObject);
begin
  Label1.Caption := Format('%2.1f FPS',[GLSceneViewer1.FramesPerSecond]);
  GLSceneViewer1.ResetPerformanceMonitor;
end;

procedure TfrmClothify.FormCreate(Sender: TObject);
begin
  SetCurrentDir('..\..\Media\');

  Button_LoadMesh.Click;
  TrackBar_IterationsChange(nil);
end;

procedure TfrmClothify.TrackBar_SlackChange(Sender: TObject);
var
  i : integer;
begin
  for i := 0 to VerletAssembly.Constraints.Count-1 do
  begin
    if VerletAssembly.Constraints[i] is TVCStick then
      TVCStick(VerletAssembly.Constraints[i]).Slack := GetSlack;
  end;
end;

function TfrmClothify.GetSlack: single;
begin
  result := TrackBar_Slack.Position/500;
end;

procedure TfrmClothify.TrackBar_IterationsChange(Sender: TObject);
begin
  VerletAssembly.Iterations := TrackBar_Iterations.Position;

  Label6.Caption := Format('Iterations %d',[TrackBar_Iterations.Position]);
end;

procedure TfrmClothify.FormMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
	GLCamera1.AdjustDistanceToTarget(Power(1.1, WheelDelta/120));
end;

procedure TfrmClothify.TrackBar_FrictionChange(Sender: TObject);
var
  i : integer;
begin
  for i := 0 to VerletAssembly.Constraints.Count-1 do
    if VerletAssembly.Constraints[i] is TVerletGlobalFrictionConstraint then
      TVerletGlobalFrictionConstraint(VerletAssembly.Constraints[i]).FrictionRatio := TrackBar_Friction.Position / 100;
end;

end.
