unit fClothify;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, GLObjects, GLScene, GLVectorFileObjects, GLWin32Viewer, GLMisc,
  GLFileMS3D, VerletClasses, VectorTypes, VectorLists, Geometry, GLTexture,
  OpenGL12, StdCtrls, GLFileSMD, GLCadencer, ExtCtrls, GLShadowPlane,
  GLVerletClothify, ComCtrls, jpeg, GLFile3DS, GLShadowVolume;

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
    procedure GLDirectOpenGL1Render(var rci: TRenderContextInfo);
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
    VerletAssembly : TVerletAssembly;
    EdgeDetector : TEdgeDetector;
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
  Sphere : TVCSphere;
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

begin
  randomize;

  if VerletAssembly<>nil then
    VerletAssembly.Free;

  s := ComboBox_MeshName.Text;

  DecimalSeparator := '.';

  p:=Pos(',', s);
  if p>0 then begin
      f := StrToFloatDef(Trim(Copy(s, p+1, MaxInt)), 1);
      GLActor1.Scale.AsVector := VectorMake(f,f,f,0)
  end else GLActor1.Scale.AsVector := XYZHmgVector;

  GLActor1.AutoCentering := [macUseBarycenter];
  GLActor1.LoadFromFile(Trim(Copy(s, 1, p-1)));

  GLActor1.Roll(random*360);
  GLActor1.Turn(random*360);//}

  GLSphere1.Visible := false;
  GLCylinder1.Visible := false;
  GLCube1.Visible := false;
  GLDummyCube_Stairs.Visible := False;
  GL_Capsule.Visible := False;

  case ComboBox_Collider.ItemIndex of
    0 : GLSphere1.Visible := true;
    1 : GLCylinder1.Visible := true;
    2 : GLCube1.Visible := true;
    3 : GLDummyCube_Stairs.Visible := true;
    4 : GL_Capsule.Visible := true;
  end;

  EdgeDetector := TEdgeDetector.Create(GLActor1);

  try
    if not CheckBox_Weld.Checked then
      EdgeDetector.WeldDistance := -1;
      
    EdgeDetector.ProcessMesh;

    if ComboBox_ConstraintType.ItemIndex=0 then
      VerletAssembly := EdgeDetector.CreateVAWithSticks(GetSlack)
    else
      VerletAssembly := EdgeDetector.CreateVAWithForces(1000,100, GetSlack);

    // VerletAssembly.Nodes[0].NailedDown := true;

    TVFGravity.Create(VerletAssembly);

    {AirResistance := TVFAirResistance.Create(VerletAssembly);
    AirResistance.DragCoeff := 0.00001;//}

    Floor := TVCFloor.Create(VerletAssembly);
    Floor.Location := VectorAdd(GLShadowPlane1.Position.AsAffineVector, myVectorMake2(0,0.1,0));
    Floor.Normal := GLShadowPlane1.Direction.AsAffineVector;

    Floor.FrictionRatio := 0.6;

    if GLSphere1.Visible then begin
       Sphere := TVCSphere.Create(VerletAssembly);
       Sphere.Radius := GLSphere1.Radius;
       Sphere.Location := myVectorMake(GLSphere1.AbsolutePosition);
    end;

    if GLCube1.Visible then begin
      CreateCubeFromGLCube(GLCube1);
    end;

    if GLCylinder1.Visible then begin
       {Cylinder := TVCCylinder.Create(VerletAssembly);
       Cylinder.Radius := GLCylinder1.TopRadius;
       Cylinder.Base := AffineVectorMake(GLCylinder1.AbsolutePosition);
       Cylinder.Axis := AffineVectorMake(GLCylinder1.AbsoluteUp);//}

       Capsule := TVCCapsule.Create(VerletAssembly);
       Capsule.Radius := GLCylinder1.TopRadius;
       Capsule.Base := AffineVectorMake(GLCylinder1.AbsolutePosition);
       Capsule.Axis := AffineVectorMake(GLCylinder1.AbsoluteUp);//}
       Capsule.Length := 20;
       Capsule.FrictionRatio := 0.6;
    end;

    if GL_Capsule.Visible then begin
       Capsule := TVCCapsule.Create(VerletAssembly);
       Capsule.Radius := GL_Capsule.TopRadius;
       Capsule.Base := AffineVectorMake(GL_Capsule.AbsolutePosition);
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

    Caption := Format('Edges = %d, EdgeDoublesSkipped = %d',[EdgeDetector.EdgeList.Count, EdgeDetector.EdgeDoublesSkipped]);
  finally
    EdgeDetector.Free;
  end;
end;

var
  mx, my : integer;
procedure TfrmClothify.GLDirectOpenGL1Render(var rci: TRenderContextInfo);
begin
  // EdgeDetector.RenderEdges(rci);
end;

procedure TfrmClothify.GLSceneViewer1MouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  if ssLeft in Shift then
    GLCamera1.MoveAroundTarget(my-y, mx-x);

  mx := x;
  my := y
end;

procedure TfrmClothify.GLCadencer1Progress(Sender: TObject;
  const deltaTime, newTime: Double);
begin
  if CheckBox_Pause.Checked then
    VerletAssembly.SimTime := newTime
  else
  begin
    VerletAssembly.Progress(deltaTime, newTime);
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
