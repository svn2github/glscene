unit Unit1;

interface

uses
  Winapi.Windows,
  Winapi.OpenGL,
  System.SysUtils,
  System.Classes,
  Vcl.Forms,
  Vcl.Controls,
  Vcl.ExtCtrls,
  //GLS
  GLScene,
  GLObjects,
  GLTexture,
  GLVectorGeometry,
  GLWin32Viewer,
  GLVectorTypes,
  GLGeomObjects,
  GLColor,
  GLCrossPlatform,
  GLCoordinates,
  GLBaseClasses;

type
  TForm1 = class(TForm)
    GLScene1: TGLScene;
    GLSceneViewer1: TGLSceneViewer;
    GLCamera1: TGLCamera;
    DummyCube1: TGLDummyCube;
    GLLightSource1: TGLLightSource;
    Sphere: TGLSphere;
	 Cylinder: TGLCylinder;
    Torus: TGLTorus;
    Cone: TGLCone;
	 Timer1: TTimer;
    TIPickTimer: TTimer;
	 procedure GLSceneViewer1MouseMove(Sender: TObject; Shift: TShiftState;
		X, Y: Integer);
	 procedure GLSceneViewer1MouseDown(Sender: TObject;
		Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
	 procedure Timer1Timer(Sender: TObject);
	 procedure SphereProgress(Sender: TObject; const deltaTime,
		newTime: Double);
    procedure TIPickTimerTimer(Sender: TObject);
  private
	 { Private declarations }
	 currentPick : TGLCustomSceneObject;
  public
	 { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

uses Dialogs;

procedure TForm1.GLSceneViewer1MouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
   TIPickTimer.Enabled:=True;
end;

procedure TForm1.TIPickTimerTimer(Sender: TObject);
var
   cp : TPoint;
begin
	// get what is under the mouse
   GetCursorPos(cp);
   cp:=GLSceneViewer1.ScreenToClient(cp);
	currentPick:=(GLSceneViewer1.Buffer.GetPickedObject(cp.x, cp.y) as TGLCustomSceneObject);
   TIPickTimer.Enabled:=False;
end;

procedure TForm1.GLSceneViewer1MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
	pick : TGLCustomSceneObject;
begin
	// if an object is picked...
	pick:=(GLSceneViewer1.Buffer.GetPickedObject(x, y) as TGLCustomSceneObject);
	if Assigned(pick) then begin
		// ...turn it to yellow and show its name
		pick.Material.FrontProperties.Emission.Color:=clrYellow;
		ShowMessage('You clicked the '+pick.Name);
	end;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
	// trigger progression (we don't use time in this sample)
	GLScene1.Progress(0, 0);
end;

procedure TForm1.SphereProgress(Sender: TObject; const deltaTime,
  newTime: Double);
var
	targetColor : TColorVector;
begin
	with Sender as TGLCustomSceneObject do begin
		// if we are picked, target color is red, else it is black (no emission)
		if Sender = currentPick then
			targetColor:=clrRed
		else targetColor:=clrBlack;
		// Set new color at 66% between current and target color
		with Material.FrontProperties.Emission do
			Color:=VectorLerp(targetColor, Color, 0.66)
	end;
end;

end.
