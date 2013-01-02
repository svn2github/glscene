{: This sample illustrates basic user-driven camera movements.<p>

	I'm using the GLScene built-in camera movement methods. The camera object is
	a child of its target dummy cube (this means that the camera is translated
	when its target is translate, which is good for flyover/scrolling movements).<p>

	Movements in this sample are done by moving the mouse with a button
	pressed, left button will translate the dummy cube (and the camera),
	right button will rotate the camera around the target, shift+right will
   rotate the object in camera's axis.<br>
	Mouse Wheel allows zooming in/out.<br>
	'7', '9' rotate around the X vector (in red, absolute).<br>
	'4', '6' rotate around the Y vector (in green, absolute).<br>
	'1', '3' rotate around the Z vector (in blue, absolute).<br>
}
unit Unit1;

interface

uses
  Windows, Forms, GLScene, GLObjects, Classes, Controls, GLTeapot,
  GLWin32Viewer, GLCrossPlatform, GLCoordinates, BaseClasses, StdCtrls,
  ExtCtrls, VectorGeometry, GLCadencer;

type
  TForm1 = class(TForm)
    GLScene1: TGLScene;
    GLSceneViewer1: TGLSceneViewer;
    GLCamera1: TGLCamera;
    Teapot1: TGLTeapot;
    GLLightSource1: TGLLightSource;
    DummyCube1: TGLDummyCube;
    RadioGroup1: TRadioGroup;
    RadioGroup2: TRadioGroup;
    GLCadencer1: TGLCadencer;
    procedure GLSceneViewer1MouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
	 procedure GLSceneViewer1MouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure FormMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure RadioGroup1Click(Sender: TObject);
    procedure RadioGroup2Click(Sender: TObject);
    procedure GLCamera1CustomPerspective(const viewport: TRectangle; width,
      height, DPI: Integer; var viewPortRadius: Single);
    procedure GLCadencer1Progress(Sender: TObject; const deltaTime,
      newTime: Double);
  private
    { Déclarations privées }
	 mdx, mdy: Integer;
   a: Double;
  public
	 { Déclarations publiques }
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

uses Math, GLContext;

procedure TForm1.GLSceneViewer1MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
	// store mouse coordinates when a button went down
	mdx:=x; mdy:=y;
end;

procedure TForm1.GLSceneViewer1MouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var
	dx, dy : Integer;
	v : TVector;
begin
	// calculate delta since last move or last mousedown
	dx:=mdx-x; dy:=mdy-y;
	mdx:=x; mdy:=y;
	if ssLeft in Shift then begin
      if ssShift in Shift then begin
         // right button with shift rotates the teapot
         // (rotation happens around camera's axis)
	   	GLCamera1.RotateObject(Teapot1, dy, dx);
      end else begin
   		// right button without shift changes camera angle
	   	// (we're moving around the parent and target dummycube)
		   GLCamera1.MoveAroundTarget(dy, dx)
      end;
	end else if Shift=[ssRight] then begin
		// left button moves our target and parent dummycube
		v:=GLCamera1.ScreenDeltaToVectorXY(dx, -dy,
							0.12*GLCamera1.DistanceToTarget/GLCamera1.FocalLength);
		DummyCube1.Position.Translate(v);
		// notify camera that its position/target has been changed
		GLCamera1.TransformationChanged;
	end;
end;

procedure TForm1.FormMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
	// Note that 1 wheel-step induces a WheelDelta of 120,
	// this code adjusts the distance to target with a 10% per wheel-step ratio
	GLCamera1.AdjustDistanceToTarget(Power(1.1, WheelDelta/120));
end;

procedure TForm1.FormKeyPress(Sender: TObject; var Key: Char);
begin
	with Teapot1 do case Key of
		'7' : RotateAbsolute(-15,  0,  0);
		'9' : RotateAbsolute(+15,  0,  0);
      '4' : RotateAbsolute(  0,-15,  0);
      '6' : RotateAbsolute(  0,+15,  0);
      '1' : RotateAbsolute(  0,  0,-15);
      '3' : RotateAbsolute(  0,  0,+15);
	end;
end;

procedure TForm1.RadioGroup1Click(Sender: TObject);
begin
  case RadioGroup1.ItemIndex of
    0: GLCamera1.CameraStyle := csPerspective;
    1: GLCamera1.CameraStyle := csInfinitePerspective;
    2: GLCamera1.CameraStyle := csPerspectiveKeepFOV;
    3: GLCamera1.CameraStyle := csCustom;
  end;
end;

procedure TForm1.RadioGroup2Click(Sender: TObject);
begin
  GLCamera1.KeepFOVMode := TGLCameraKeepFOVMode(RadioGroup2.ItemIndex);
end;

procedure TForm1.GLCamera1CustomPerspective(const viewport: TRectangle;
  width, height, DPI: Integer; var viewPortRadius: Single);
var
  Mat: TMatrix;
begin
  Mat :=  CreatePerspectiveMatrix(GLCamera1.GetFieldOfView(Width)/4,
    Width / Height, GLCamera1.NearPlaneBias, GLCamera1.DepthOfView);
  Mat := MatrixMultiply(Mat, CreateRotationMatrixZ(a));
  CurrentGLContext.PipelineTransformation.ProjectionMatrix := Mat;
end;

procedure TForm1.GLCadencer1Progress(Sender: TObject; const deltaTime,
  newTime: Double);
begin
  a := Pi * sin(newTime) / 18;
  GLSceneViewer1.Invalidate;
end;

end.
