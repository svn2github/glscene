{: Creating ODE joints with GLODEManager.<p>
   
   This demo shows how to create connections between ODE objects using
   joints.<p>

   In this demo, the Dynamic Behaviour object was used to pass the
   control of GLScene objects to ODE. Joints are used to connect
   the objects together physically.<p>

   A joint can be fixed to space by attaching to a nothing, as done
   with the 'Wheel Fixed' and 'Pin2 Fixed'.<p>
}
unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, GLScene, GLObjects, GLWin32Viewer, GLMisc, GLODEManager,
  GLCadencer, GLGeomObjects;

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

uses VectorGeometry;

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
  TGLODEDynamicBehaviour(Pin2.Behaviours[0]).AddForce(AffineVectorMake(-50,0,0));
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
