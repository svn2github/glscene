unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, GLScene, GLObjects, GLWin32Viewer, GLMisc, GLODEManager,
  GLCadencer;

type
  TForm1 = class(TForm)
    GLScene1: TGLScene;
    GLSceneViewer1: TGLSceneViewer;
    GLCamera1: TGLCamera;
    GLDummyCube1: TGLDummyCube;
    GLODEManager1: TGLODEManager;
    GLODEJointList1: TGLODEJointList;
    Machine: TGLDummyCube;
    Axle: TGLCylinder;
    GLLightSource1: TGLLightSource;
    Wheel: TGLCylinder;
    Pin1: TGLCylinder;
    Arm: TGLCube;
    Slider: TGLCube;
    Pin2: TGLCylinder;
    GLCadencer1: TGLCadencer;
    procedure FormCreate(Sender: TObject);
    procedure GLSceneViewer1MouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure GLSceneViewer1MouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure GLCadencer1Progress(Sender: TObject; const deltaTime,
      newTime: Double);
  private
    { Private declarations }
  public
    { Public declarations }
    mx, my : integer;
  end;

var
  Form1: TForm1;

implementation

uses ODEImport;

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
var
  WheelBehaviour,
  ArmBehaviour,
  Pin2Behaviour  : TGLODEDynamicBehaviour;
begin
  WheelBehaviour:=TGLODEDynamicBehaviour.Create(Wheel.Behaviours);
  ArmBehaviour:=TGLODEDynamicBehaviour.Create(Arm.Behaviours);
  Pin2Behaviour:=TGLODEDynamicBehaviour.Create(Pin2.Behaviours);

  with WheelBehaviour do begin
    Manager:=GLODEManager1;
    with TODEElementCapsule(AddNewElement(TODEElementCapsule)) do begin
      Position.SetPoint(0,0,0);
      Radius:=Wheel.TopRadius;
      Length:=Wheel.Height;
    end;
    with TODEElementCapsule(AddNewElement(TODEElementCapsule)) do begin
      Position.AsVector:=Axle.Position.AsVector;
      Radius:=Axle.TopRadius;
      Length:=Axle.Height;
    end;
    CalibrateCenterOfMass;
  end;
  with ArmBehaviour do begin
    Manager:=GLODEManager1;
    with TODEElementBox(AddNewElement(TODEElementBox)) do begin
      Position.SetPoint(0,0,0);
      BoxWidth:=Arm.CubeWidth;
      BoxHeight:=Arm.CubeHeight;
      BoxDepth:=Arm.CubeDepth;
    end;
  end;
  with Pin2Behaviour do begin
    Manager:=GLODEManager1;
    with TODEElementCapsule(AddNewElement(TODEElementCapsule)) do begin
      Position.SetPoint(0,0,0);
      Radius:=Pin2.TopRadius;
      Length:=Pin2.Height;
    end;
  end;

  with TODEJointHinge.CreateOwned(GLODEJointList1.Joints) do begin
    Manager:=GLODEManager1;
    Attach(WheelBehaviour,nil);
    Axis.SetVector(0,1,0);
    Anchor.AsVector:=Wheel.AbsolutePosition;
  end;

  with TODEJointHinge.CreateOwned(GLODEJointList1.Joints) do begin
    Manager:=GLODEManager1;
    Attach(WheelBehaviour,ArmBehaviour);
    Axis.SetVector(0,1,0);
    Anchor.AsVector:=Pin1.AbsolutePosition;
  end;

  with TODEJointHinge.CreateOwned(GLODEJointList1.Joints) do begin
    Manager:=GLODEManager1;
    Attach(ArmBehaviour,Pin2Behaviour);
    Axis.SetVector(0,1,0);
    Anchor.AsVector:=Pin2.AbsolutePosition;
  end;

  with TODEJointSlider.CreateOwned(GLODEJointList1.Joints) do begin
    Manager:=GLODEManager1;
    Attach(Pin2Behaviour,nil);
    Axis.SetVector(1,0,0);
  end;

  //dBodyAddTorque(WheelBehaviour.Body,0,1000,0);
  dBodyAddForce(Pin2Behaviour.Body,-1000,0,0);
end;

procedure TForm1.GLSceneViewer1MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  mx:=x;
  my:=y;
end;

procedure TForm1.GLSceneViewer1MouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  if ssLeft in Shift then
    GLCamera1.MoveAroundTarget(my-y,mx-x);
  mx:=x;
  my:=y;
end;

procedure TForm1.GLCadencer1Progress(Sender: TObject; const deltaTime,
  newTime: Double);
begin
  GLODEManager1.Step(deltaTime);
end;

end.
