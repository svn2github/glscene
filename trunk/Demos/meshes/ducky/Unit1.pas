{: Loading NURBS into a GLScene FreeForm object<p>

   A very simple parametric model of a duck, comprised of 3 NURBS
   surfaces. The Nurbs format is essentially the NurbsSurface geometry 
   type used in VRML. One limitation at the moment is the Control points
   must each be on a separate line. Inverted surfaces are handled with 
   the ccw FALSE statement in the .nurbs file (duck3.nurbs uses this 
   setting).<p>
   
   Use the resolution slider to increase or decrease the models triangle
   count dynamically.<p>
}
unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, GLScene, GLVectorFileObjects, GLObjects, GLWin32Viewer, GLMisc,
  ExtCtrls, ComCtrls, StdCtrls;

type
  TForm1 = class(TForm)
    GLScene1: TGLScene;
    GLCamera1: TGLCamera;
    GLDummyCube1: TGLDummyCube;
    GLFreeForm1: TGLFreeForm;
    GLLightSource1: TGLLightSource;
    Panel1: TPanel;
    GLSceneViewer1: TGLSceneViewer;
    TrackBar1: TTrackBar;
    Label1: TLabel;
    procedure GLSceneViewer1MouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure GLSceneViewer1MouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure FormCreate(Sender: TObject);
    procedure TrackBar1Change(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    mx,my : Integer;
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

uses GLFileNurbs, GLParametricSurfaces, VectorGeometry, VectorLists;

procedure TForm1.FormCreate(Sender: TObject);
var
  cp : TAffineVectorList;
begin
  // Load the nurbs data
  GLFreeForm1.LoadFromFile('..\..\media\duck1.nurbs');
  GLFreeForm1.AddDataFromFile('..\..\media\duck2.nurbs');
  GLFreeForm1.AddDataFromFile('..\..\media\duck3.nurbs');

  { Translate FreeForm based on the first mesh object's average
    control point. Quick and dirty ... or maybe just dirty :P }
  cp:=TMOParametricSurface(GLFreeForm1.MeshObjects[0]).ControlPoints;
  GLFreeForm1.Position.Translate(VectorNegate(VectorScale(cp.Sum,1/cp.Count)));
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

procedure TForm1.TrackBar1Change(Sender: TObject);
var
  i : integer;
begin
  for i:=0 to 2 do
    TMOParametricSurface(GLFreeForm1.MeshObjects[i]).Resolution:=TrackBar1.Position;
  GLFreeForm1.StructureChanged;
end;

end.
