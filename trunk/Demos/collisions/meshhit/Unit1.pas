{: Basic raycast/mesh sample.<p>

   Demonstrating how to find the intersection point between eye-screen ray
   and a simple mesh (click on the mushroom and intersection point and normal
   will be calculated).<p>
}
unit Unit1;

interface

uses
  Windows, SysUtils, Classes, Controls, Forms,
  GLMisc, GLScene, GLVectorFileObjects, GLObjects, GLWin32Viewer;

type
  TForm1 = class(TForm)
    GLSceneViewer1: TGLSceneViewer;
    GLScene1: TGLScene;
    GLCamera1: TGLCamera;
    GLLightSource1: TGLLightSource;
    DummyCube1: TDummyCube;
    FreeForm1: TFreeForm;
    Sphere1: TSphere;
    ArrowLine1: TArrowLine;
    GLSceneViewer2: TGLSceneViewer;
    GLCamera2: TGLCamera;
    procedure FormCreate(Sender: TObject);
    procedure GLSceneViewer1MouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure GLSceneViewer1MouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure GLSceneViewer2MouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure GLSceneViewer2MouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

uses Geometry;

procedure TForm1.FormCreate(Sender: TObject);
begin
   // Load mushroom mesh
   FreeForm1.LoadFromFile('..\..\media\mushroom.3ds');
end;

procedure TForm1.GLSceneViewer1MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
   rayStart, rayVector, iPoint, iNormal : TAffineVector;
begin
   // retrieve raycasting data:
   //    rayStart is the eye (camera) position
   //    rayVector is computed from screen position
   // (note that (0, 0) is lower left for the Screen function, whereas Delphi
   //  uses top-left as origin, hence the Y inversion)
   SetVector(rayStart, GLCamera1.AbsolutePosition);
   SetVector(rayVector, GLSceneViewer1.Buffer.ScreenToVector(AffineVectorMake(x, GLSceneViewer1.Height-y, 0)));
   // Here we require RauCast intersection
   if FreeForm1.RayCastIntersect(rayStart, rayVector, @iPoint, @iNormal) then begin
      // got one, move the sphere there and orient it appropriately
      Sphere1.Position.AsAffineVector:=iPoint;
      Sphere1.Direction.AsAffineVector:=VectorNormalize(iNormal);
   end;
end;

procedure TForm1.GLSceneViewer1MouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
   // when mouse moves, recompute intersection
   if Shift<>[] then GLSceneViewer1MouseDown(Sender, mbLeft, Shift, x, y);
end;

// same as previously, but for the other camera & viewer

procedure TForm1.GLSceneViewer2MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
   rayStart, rayVector, iPoint, iNormal : TAffineVector;
begin
   SetVector(rayStart, GLCamera2.AbsolutePosition);
   SetVector(rayVector, GLSceneViewer2.Buffer.ScreenToVector(AffineVectorMake(x, GLSceneViewer2.Height-y, 0)));
   if FreeForm1.RayCastIntersect(rayStart, rayVector, @iPoint, @iNormal) then begin
      Sphere1.Position.AsAffineVector:=iPoint;
      Sphere1.Direction.AsAffineVector:=VectorNormalize(iNormal);
   end;
end;

procedure TForm1.GLSceneViewer2MouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
   if Shift<>[] then GLSceneViewer2MouseDown(Sender, mbLeft, Shift, x, y);
end;

end.
