{ : Parallel projection demo.<p>

  This simple demo shows how to do parallel projection and blend some custom
  OpenGL calls into the scene.<br>
  You can change the viewpoint with left clic drags, change the plane orientation
  with right clic drags, and move the plane up/down with the wheel.<p>

  The points and plane are rendered directly with regular scene objects,
  but the projection lines between the points and the plane are computed
  and rendered on the fly in a TGLDirectOpenGL. This is a typical case where
  a little bit of custom code helps a lot: we could have used many TGLLines
  object to draw the lines, but this would have resulted in a lot of object
  creation and update code, and ultimately in rather poor performance.<br>
  Note the position of the plane in the scene hierarchy: it is last as it is
  a blended object. Try making it the first object, it will appear opaque
  (though it is still transparent!).
}
unit Unit1;

interface

uses
  Windows,
  Messages,
  SysUtils,
  Classes,
  Controls,
  Forms,

  GLScene.Material,
  GLScene.MaterialEx,
  GLScene.Cadencer,
  GLScene.Core,
  GLScene.Objects.GraphPlotting,
  GLScene.Objects,
  GLScene.ObjectsEx,
  GLScene.Base.Coordinates,
  GLScene.Platform,
  GLScene.Viewer.VCL,
  GLScene.Base.Context.Info;

type
  TForm1 = class(TForm)
    GLScene1: TGLScene;
    SceneViewer: TGLSceneViewer;
    GLCamera: TGLCamera;
    GLDummyCube: TGLDummyCube;
    GLPlane: TGLPlane;
    GLPoints: TGLPoints;
    DirectOpenGL: TGLDirectOpenGL;
    GLArrowLine1: TGLArrowLine;
    GLLightSource1: TGLLightSource;
    GLXYZGrid1: TGLXYZGrid;
    GLCadencer1: TGLCadencer;
    GLMaterialLibraryEx1: TGLMaterialLibraryEx;
    procedure FormCreate(Sender: TObject);
    procedure DirectOpenGLRender(Sender: TObject; var rci: TRenderContextInfo);
    procedure SceneViewerMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure SceneViewerMouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure FormMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure GLCadencer1Progress(Sender: TObject;
      const deltaTime, newTime: Double);
  private
    { Private declarations }
  public
    { Public declarations }
    mx, my: Integer;
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

uses
  GLScene.Base.Vector.Types,
  GLScene.Base.Vector.Geometry,
  GLScene.Base.GLStateMachine,
  GLScene.Base.OpenGL.Tokens,
  GLScene.Base.Context;

procedure TForm1.FormCreate(Sender: TObject);
var
  i: Integer;
begin
  GLPoints.MaterialLibrary := GLMaterialLibraryEx1;
  GLPoints.LibMaterialName := GLMaterialLibraryEx1.Materials[0].Name;
  // generate a bunch of random points
  for i := 1 to 100 do
    GLPoints.Positions.Add((Random - 0.5) * 5, (Random - 0.5) * 5,
      (Random - 0.5) * 5);
end;

procedure TForm1.DirectOpenGLRender(Sender: TObject;
  var rci: TRenderContextInfo);
var
  i: Integer;
  mat: TMatrix;
  p, pProj: TVector;
  planePoint, planeNormal: TVector;
  plane: THmgPlane;
begin
  // Here we recover our plane point and normal...
  planePoint := GLPlane.Position.AsVector;
  planeNormal := GLPlane.Direction.AsVector;
  // ...which we use to create a plane (equation)
  plane := PlaneMake(planePoint, planeNormal);
  // from that plane equation and our pojection direction
  // (which is here the plane normal)
  mat := MakeParallelProjectionMatrix(plane, planeNormal);

  // save state, turn off lighting and specify the lines color
  rci.GLStates.Disable(stLighting);

  with GL do
  begin
    Color3f(1, 1, 0);

    // we'll be drawing a bunch of lines, to specify a line in OpenGL,
    // you only need to specify the line start and end vertices
    Begin_(GL_LINES);
    for i := 0 to GLPoints.Positions.Count - 1 do
    begin
      // read the point coordinates, directly from the TGLPoints list
      MakePoint(p, GLPoints.Positions.List[i]);
      // project this point on the plane with the matrix
      pProj := VectorTransform(p, mat);
      // specify the two vertices
      Vertex3fv(@p);
      Vertex3fv(@pProj);
    end;
    End_;
  end;
end;

procedure TForm1.SceneViewerMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  mx := X;
  my := Y;
end;

procedure TForm1.SceneViewerMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
begin
  if Shift = [ssLeft] then
    GLCamera.MoveAroundTarget(my - Y, mx - X)
  else if Shift = [ssRight] then
    GLCamera.RotateObject(GLPlane, my - Y, mx - X);
  mx := X;
  my := Y;
end;

procedure TForm1.FormMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  GLPlane.Position.Y := GLPlane.Position.Y + WheelDelta * 0.001;
end;

procedure TForm1.GLCadencer1Progress(Sender: TObject;
  const deltaTime, newTime: Double);
begin
  SceneViewer.Invalidate;
end;

end.
