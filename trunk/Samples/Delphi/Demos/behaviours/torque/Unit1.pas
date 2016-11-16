unit Unit1;

interface

uses
  Winapi.OpenGL,
  System.Classes,
  System.SysUtils,
  Vcl.Forms,
  Vcl.StdCtrls,
  Vcl.ExtCtrls,
  Vcl.Controls,
  //GLS
  GLObjects,
  GLScene,
  GLCadencer,
  GLWin32Viewer,
  GLPolyhedron,
  GLCrossPlatform,
  GLCoordinates,
  GLBaseClasses,
  GLBehaviours;

type
  TForm1 = class(TForm)
	 GLSceneViewer1: TGLSceneViewer;
	 GLScene1: TGLScene;
	 GLCamera1: TGLCamera;
	 GLLightSource1: TGLLightSource;
    Hexahedron: TGLCube;
    DummyCube1: TGLDummyCube;
    GLCadencer1: TGLCadencer;
    Panel1: TPanel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    CheckBox1: TCheckBox;
    Panel2: TPanel;
    Label1: TLabel;
    Label5: TLabel;
  	Dodecahedron: TGLDodecahedron;
    Icosahedron: TGLIcosahedron;
    Octahedron: TGLOctahedron;
    Tetrahedron: TGLTetrahedron;
	 procedure GLSceneViewer1MouseMove(Sender: TObject; Shift: TShiftState;
		X, Y: Integer);
	 procedure FormCreate(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
    procedure GLCadencer1Progress(Sender: TObject; const deltaTime,
      newTime: Double);
  private
	 lastTime : Double;
	 pickedObject : TGLBaseSceneObject;
  public
	 //
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

procedure TForm1.FormCreate(Sender: TObject);
begin
	// Initialize last time
	lastTime:=Now*3600*24;
	// Initialize rotation dampings...
	// ...using properties...
	with GetOrCreateInertia(Hexahedron.Behaviours).RotationDamping do begin
		Constant:=1;
		Linear:=1;
		Quadratic:=0;
	end;
	// ...using helper function on the TGLBehaviours...
	GetOrCreateInertia(Dodecahedron.Behaviours).RotationDamping.SetDamping(10, 0, 0.01);
	// ...or using helper function directly on the TGLBaseSceneObject
	GetOrCreateInertia(Octahedron.Behaviours).RotationDamping.SetDamping(0, 0, 0.01);
	GetOrCreateInertia(Icosahedron.Behaviours).RotationDamping.SetDamping(0, 0, 0.01);
	GetOrCreateInertia(Tetrahedron.Behaviours).RotationDamping.SetDamping(0, 0, 0.01);
end;

procedure TForm1.GLSceneViewer1MouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
	// Mouse moved, get what's underneath
	pickedObject:=GLSceneViewer1.Buffer.GetPickedObject(x, y);
end;

procedure TForm1.GLCadencer1Progress(Sender: TObject; const deltaTime,
  newTime: Double);
begin
	// apply some "torque" to the pickedObject if any
	if Assigned(pickedObject) then
		GetOrCreateInertia(pickedObject).ApplyTorque(deltaTime, 200, 0, 0);
end;

procedure TForm1.CheckBox1Click(Sender: TObject);
var
	i : Integer;
	mass : Single;
begin
	if CheckBox1.Checked then
		mass:=2
	else
    mass:=1;
	// all our objects are child of the DummyCube1
	for i:=0 to DummyCube1.Count-1 do
		GetOrCreateInertia(DummyCube1.Children[i]).Mass:=mass;
end;

end.
