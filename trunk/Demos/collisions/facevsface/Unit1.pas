{: Face vs Face collision detection.<p>

   This sample illustrates:<ul>
   <li>collisions between FreeForm Objects (Triangle-based)
   <li>collisions between Cubes
   <li>basic user-driven camera movements.
   <li>picking Objects
   <li>moving Objects
   </ul></li><p>

   Usage:<ul>
   <li>left Mouse will move Camera
   <li>right Mouse will move an object
   <li>left Mouse + shift will roll and pitch the object
   <li>Wheel scroll will zoom in/out
   </ul><p>
   Bernd Klaiber.
}
unit Unit1;

interface

uses
  Windows, Forms, GLScene, GLObjects, GLMisc, Classes, Controls, SysUtils, Graphics,
  GLWin32Viewer, ExtCtrls, Geometry, StdCtrls, GLSpaceText,
  ComCtrls, GLCollision, GLVectorFileObjects, GLCrossPlatform, VectorLists;

type
  TForm1 = class(TForm)
    GLScene1: TGLScene;
    GLSceneViewer1: TGLSceneViewer;
    GLLightSource1: TGLLightSource;
    DummyCube1: TGLDummyCube;
    Timer1: TTimer;
    GLCamera2: TGLCamera;
    Panel1: TPanel;
    txtX: TGLSpaceText;
    txtY: TGLSpaceText;
    txtZ: TGLSpaceText;
    CollisionManager1: TCollisionManager;
    cbCollisionMode: TRadioGroup;
    Bar: TGLCube;
    Teapot1: TGLFreeForm;
    Teapot2: TGLFreeForm;
    GLLightSource2: TGLLightSource;
    GLLightSource3: TGLLightSource;
    Shape1: TShape;
    Cube1: TGLCube;
    Cube2: TGLCube;
    rgObjects: TRadioGroup;
    Label1: TLabel;
    LATime: TLabel;
    procedure GLSceneViewer1MouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure GLSceneViewer1MouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure FormMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure Timer1Timer(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure CollisionManager1Collision(Sender: TObject; object1,
      object2: TGLBaseSceneObject);
    procedure cbCollisionModeClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure rgObjectsClick(Sender: TObject);
  private
    { Déclarations privées }
    mdx, mdy : Integer;
    CollisionDetected : Boolean;
  public
    CurrSO: TGLCustomSceneObject;
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

uses Math, GLFile3DS;

procedure TForm1.FormCreate(Sender: TObject);
begin
   TeaPot1.LoadFromFile('..\..\media\TeaPot.3ds');
   TeaPot1.BuildOctree;

   TeaPot2.LoadFromFile('..\..\media\TeaPot.3ds');
   TeaPot2.BuildOctree;

   rgObjectsClick(nil);
end;

procedure TForm1.FormShow(Sender: TObject);
begin
   //initialize
   CurrSO:= TeaPot1;
   cbCollisionModeClick(nil);
end;

procedure TForm1.Timer1Timer(Sender: TObject);
const
   cColor : array [False..True] of TColor = (clLime, clRed);
var
   t : Int64;
begin
   Timer1.Enabled:=False;
   CollisionDetected:=False;

   t:=StartPrecisionTimer;

   CollisionManager1.CheckCollisions;

   LATime.Caption:=Format('%.1f ms', [StopPrecisionTimer(t)*1000]);

   Shape1.Brush.Color:=cColor[CollisionDetected];
   Timer1.Enabled:=True;
end;

procedure TForm1.CollisionManager1Collision(Sender: TObject; object1,
  object2: TGLBaseSceneObject);
begin
   CollisionDetected:=True;
end;

procedure TForm1.cbCollisionModeClick(Sender: TObject);
begin
   TGLBCollision(TeaPot1.Behaviours[0]).BoundingMode:=TCollisionBoundingMode(cbCollisionMode.ItemIndex);
   TGLBCollision(TeaPot2.Behaviours[0]).BoundingMode:=TCollisionBoundingMode(cbCollisionMode.ItemIndex);
   TGLBCollision(Bar.Behaviours[0]).BoundingMode:=cbmCube;
end;

procedure TForm1.rgObjectsClick(Sender: TObject);
var
   teapots : Boolean;
begin
   teapots:=(rgObjects.ItemIndex<=0);

   Teapot1.Visible:=teapots;
   Teapot2.Visible:=teapots;

   cbCollisionMode.Visible:=teapots;

   Cube1.Visible:=not teapots;
   Cube2.Visible:=not teapots;
end;

procedure TForm1.GLSceneViewer1MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
   pick : TGLCustomSceneObject;
begin
   pick:=(GLSceneViewer1.Buffer.GetPickedObject(x, y) as TGLCustomSceneObject);
   if Assigned(Pick) then
      CurrSO:=pick;
   // store mouse coordinates when a button went down
   mdx:=x;
   mdy:=y;
end;

procedure TForm1.GLSceneViewer1MouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var
   dx, dy: Integer;
   VX, VY: TVector;
   Camera: TGLCamera;
begin
   Camera := GLSceneViewer1.Camera;
   // calculate delta since last move or last mousedown
   dx := mdx - x;
   dy := mdy - y;
   mdx := x;
   mdy := y;
   if ssLeft in Shift then begin
      if ssShift in Shift then begin
        // left button with shift rotates the object
        // (rotation happens around camera's axis)
        Camera.RotateObject(CurrSO, dy, dx);
      end else begin
        // left button without shift changes camera angle
        // (we're moving around the parent and target dummycube)
        Camera.MoveAroundTarget(dy, dx)
      end;
   end else if Shift=[ssRight] then begin
      //Moving the objects
      //Description:
      //1. via VectorPerpendicular we create a vector that is 90° to camera view and points to Y (Up)
      //   this is Y-direction of moving
      //2. now using VectorCrossProduct we create the vector that is 90° to camera view and to the other
      //   vector (VY), this is X-direction of moving
      VY := VectorMake(VectorPerpendicular(YVector, VectorNormalize(GLCamera2.Position.AsAffineVector)));
      VX := VectorCrossProduct(VY, VectorNormalize(GLCamera2.Position.AsVector));
      NormalizeVector(VY);
      NormalizeVector(VX);
      CurrSO.Position.Translate(VectorCombine(VX, VY, -dx * 0.132 * Camera.DistanceToTarget / Camera.FocalLength, dy * 0.132 * Camera.DistanceToTarget / Camera.FocalLength));
   end;
end;

procedure TForm1.FormMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
var
   Camera: TGLCamera;
begin
   Camera := GLSceneViewer1.Camera;
   // Note that 1 wheel-step induces a WheelDelta of 120,
   // this code adjusts the distance to target with a 10% per wheel-step ratio
   Camera.AdjustDistanceToTarget(Power(1.1, WheelDelta / 120));
end;

end.

