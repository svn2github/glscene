unit Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, GLWin32Viewer, GLCrossPlatform, GLBaseClasses, GLScene, GLCoordinates,
  GLObjects, GLCadencer, GLSimpleNavigation, GLGraph, GLGeomObjects;

type
  TForm1 = class(TForm)
    GLSceneViewer1: TGLSceneViewer;
    GLSimpleNavigation1: TGLSimpleNavigation;
    GLCadencer1: TGLCadencer;
    GLScene1: TGLScene;
    GLLightSource1: TGLLightSource;
    GLCamera1: TGLCamera;
    GLDummyCube1: TGLDummyCube;
    procedure FormCreate(Sender: TObject);
  private
     
    Superellipsoids: array[0..5, 0..5] of TGLSuperellipsoid;
  protected
  public
     
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

uses
  GLVectorGeometry;

procedure TForm1.FormCreate(Sender: TObject);
var
  i, j: integer;
  x, y, d: single;

begin
  d := 6;
  Randomize;
  for j := 0 to 5 do
  for i := 0 to 5 do
  begin
    x := -d*2.5 + d*i;
    y :=  d*2.5 - d*j;
    Superellipsoids[i, j] :=
    TGLSuperellipsoid(GLScene1.Objects.AddNewChild(TGLSuperellipsoid));

    with Superellipsoids[i, j] do
    begin
      Slices := 32;
      Stacks := 32;
      Scale.SetVector(5, 5, 5);
      Position.SetPoint(x, y, 0);
      Direction.SetVector(0, 1, 0);
      Up.SetVector(0, 0, 1);
      case i of
      0:VCurve := 0.2;
      1:VCurve := 0.8;
      2:VCurve := 1.0;
      3:VCurve := 1.5;
      4:VCurve := 2.0;
      5:VCurve := 3.0;
      end;
      case j of
      0:HCurve := 0.2;
      1:HCurve := 0.8;
      2:HCurve := 1.0;
      3:HCurve := 1.5;
      4:HCurve := 2.0;
      5:HCurve := 3.0;
      end;
      with Material.FrontProperties do
      begin
        Ambient.RandomColor;
        Diffuse.RandomColor;
        Specular.RandomColor;
        Shininess := 125;
      end;
    end;
  end;
end;


end.
