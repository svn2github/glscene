{ : A very simple demo of the TGLPipe object, to show what it can do.<p>

  The TGLPipe objects extrudes a circle along a trajectory (given by its node).
  You can specify a radius factor for each node and use spline smoothing.<p>

  Here we only use 3 control points, the top ones moves horizontally, and the
  middle one can be made fat/slim.<p>

  The current implementation is very limited when it comes to 3D pipes, as there
  is no "smooth" rotation interpolator, therefore, ou will have best results
  if your trajectory stays in the X/Y (local) plane.
}
unit Unit1;

interface

uses
  Windows,
  Messages,
  SysUtils,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  ExtCtrls,
  StdCtrls,

  GLScene_SimpleNavigation,
  GLScene_Cadencer,
  GLScene_Core,
  GLScene_Objects,
  GLScene_Objects_Extrusion,
  GLScene_Base_Coordinates,
  GLScene_Platform,
  GLScene_Viewer_VCL;

type
  TForm1 = class(TForm)
    GLScene1: TGLScene;
    GLSceneViewer1: TGLSceneViewer;
    GLCamera1: TGLCamera;
    GLLightSource1: TGLLightSource;
    Pipe1: TGLPipe;
    GLCadencer1: TGLCadencer;
    CBSpline: TCheckBox;
    DummyCube1: TGLDummyCube;
    CBFat: TCheckBox;
    GLSimpleNavigation1: TGLSimpleNavigation;
    Timer1: TTimer;
    procedure GLCadencer1Progress(Sender: TObject;
      const deltaTime, newTime: Double);
    procedure CBSplineClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

uses
  GLScene_Base_Vector_Geometry;

procedure TForm1.FormCreate(Sender: TObject);
begin
  Pipe1.ObjectStyle := Pipe1.ObjectStyle + [osStreamDraw];
end;

procedure TForm1.GLCadencer1Progress(Sender: TObject;
  const deltaTime, newTime: Double);
begin
  Pipe1.Nodes[2].X := 1 * Sin(newTime * 60 * cPIdiv180);
  if CBFat.Checked then
    TGLPipeNode(Pipe1.Nodes[1]).RadiusFactor :=
      1 + Cos(newTime * 30 * cPIdiv180)
  else
    TGLPipeNode(Pipe1.Nodes[1]).RadiusFactor := 1;
end;

procedure TForm1.CBSplineClick(Sender: TObject);
begin
  if CBSpline.Checked then
    Pipe1.SplineMode := lsmCubicSpline
  else
    Pipe1.SplineMode := lsmLines;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  with GLSceneViewer1 do
  begin
    Caption := Format('%d Triangles, %.1f FPS',
      [Pipe1.TriangleCount, FramesPerSecond]);
    ResetPerformanceMonitor;
  end;
end;

end.
