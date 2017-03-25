//Camera path demo
//
//This demo explains how to use a GLSpline to create a smooth detailed
//sequence of position nodes and how to run an object along this path
//
//by Paul van Dinther

unit campathdemomain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, GLScene, GLObjects, GLWin32Viewer, StdCtrls, GLSpline, GLVectorGeometry,
  GLCadencer, GLSkydome, GLCoordinates, GLCrossPlatform, GLBaseClasses;

type
  TForm1 = class(TForm)
    GLScene1: TGLScene;
    GLSceneViewer1: TGLSceneViewer;
    GLLightSource1: TGLLightSource;
    GLCamera1: TGLCamera;
    GLPlane1: TGLPlane;
    GLLines1: TGLLines;
    GLLines2: TGLLines;
    GLCube1: TGLCube;
    GLCadencer1: TGLCadencer;
    GLDummyCube1: TGLDummyCube;
    GLEarthSkyDome1: TGLEarthSkyDome;
    GLCamera2: TGLCamera;
    procedure GLCadencer1Progress(Sender: TObject; const deltaTime,
      newTime: Double);
    procedure FormCreate(Sender: TObject);
  private
    Step: Integer;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.GLCadencer1Progress(Sender: TObject; const deltaTime, newTime: Double);
begin
  if Step < GLLines2.Nodes.Count - 10 then
  begin
    GLCube1.Position.SetPoint(GLLines2.Nodes.Items[step].AsVector);
    GLDummyCube1.Position.SetPoint(GLLines2.Nodes.Items[step + 10].AsVector);

    //Points cube to the next point on the camera path
    GLCube1.PointTo(GLDummyCube1,YHmgVector);
    inc(Step);
  end
  else
  begin
    Step := 0;

    //Switch camera view
    if GLSceneViewer1.Camera = GLCamera1 then
      GLSceneViewer1.Camera := GLCamera2
    else
      GLSceneViewer1.Camera := GLCamera1;
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
var
  GLSpline : TCubicSpline;
  f : Single;
  i: Integer;
  a, b, c : Single;
begin
  //Clear the camera path
  GLLines2.Nodes.Clear;

  //Create a GLSpline object using the original GLLines object
  GLSpline := GLLines1.Nodes.CreateNewCubicSpline;
  f:=1/GLLines1.Division;

  //Create nodes in the camera path for every GLSpline coordinate
  //In this example there will be 900 nodes
  for i:=0 to (GLLines1.Nodes.Count-1)*GLLines1.Division do
  begin
    GLSpline.SplineXYZ(i*f, a, b, c);
    GLLines2.Nodes.AddNode(a, b, c);
  end;
  Step := 0;
end;

end.
