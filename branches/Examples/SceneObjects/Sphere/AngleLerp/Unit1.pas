unit Unit1;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Variants,
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  //GLS
  GLWin32Viewer,
  GLScene,
  GLVectorGeometry,
  GLObjects,
  GLCadencer,
  GLCoordinates,
  GLCrossPlatform,
  GLBaseClasses;

type
  TForm1 = class(TForm)
    GLScene1: TGLScene;
    GLSceneViewer1: TGLSceneViewer;
    GLCamera1: TGLCamera;
    GLLightSource1: TGLLightSource;
    MainSphere: TGLSphere;
    Sphere1: TGLSphere;
    Sphere2: TGLSphere;
    Sphere: TGLSphere;
    GLCadencer1: TGLCadencer;
    procedure GLCadencer1Progress(Sender: TObject; const deltaTime,
      newTime: Double);
    procedure FormCreate(Sender: TObject);
  private
     
  public
     
    p1, p2, v1, v2: TAffineVector;
    theta : Single;
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.GLCadencer1Progress(Sender: TObject; const deltaTime,
  newTime: Double);
var
  t, x, y: Single;
begin
  t := Frac(newTime);
  SinCosine(theta*t,y,x);
  Sphere.Position.SetPoint(VectorCombine(v1,v2,x,y));
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  p1 := Sphere1.Position.AsAffineVector;
  p2 := Sphere2.Position.AsAffineVector;

  theta := ArcCosine(VectorAngleCosine(p1,p2));
  // if R is radius of sphere, then theta*R  is the distance between points

  v1 := p1;
  v2 := p2;
  SubtractVector(v2,VectorScale(v1,VectorDotProduct(v1,v2)));
  NormalizeVector(v2);
end;

end.
