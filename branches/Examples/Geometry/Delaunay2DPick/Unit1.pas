unit Unit1;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Variants,
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
   
  GLTriangulation,
  GLWin32Viewer, GLCrossPlatform, GLBaseClasses, GLScene;

type
  TForm1 = class(TForm)
    GLScene1: TGLScene;
    GLSceneViewer1: TGLSceneViewer;
    procedure FormCreate(Sender: TObject);
    procedure FormMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  private
     
    Z, U, V : Single;
    MatIndex : Integer;
    TempBuffer: TBitmap;
    procedure ClearBackPage;
    procedure FlipBackPage;
  public
     
    TheMesh: TGLDelaunay2D;
    procedure Draw;
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
  TheMesh := TGLDelaunay2D.Create;
  TempBuffer := TBitmap.Create;
  Form1.Caption := 'Click on the form!';
end;

procedure TForm1.ClearBackPage;
begin
  TempBuffer.Height := Form1.Height;
  TempBuffer.Width := Form1.Width;
  TempBuffer.Canvas.Brush.Color := clSilver;
  TempBuffer.Canvas.FillRect(Rect(0, 0, Form1.Width, Form1.Height));
end;

procedure TForm1.FlipBackPage;
var
  ARect: TRect;
begin
  ARect := Rect(0, 0, Form1.Width, Form1.Height);
  Form1.Canvas.CopyRect(ARect, TempBuffer.Canvas, ARect);
end;

procedure TForm1.Draw;
var
  // variable to hold how many triangles are created by the triangulate function
  i: Integer;
begin
  // Clear the form canvas
  ClearBackPage;

  TempBuffer.Canvas.Brush.Color := clTeal;
  // Draw the created triangles
  with TheMesh do
  begin
    if (TheMesh.HowMany > 0) then
    begin
      for i := 1 To TheMesh.HowMany do
      begin
        TempBuffer.Canvas.Polygon([Point(Trunc(Vertex[Triangle[i].vv0].x),
          Trunc(Vertex[Triangle[i].vv0].y)),
          Point(Trunc(Vertex[Triangle[i].vv1].x),
          Trunc(Vertex[Triangle[i].vv1].y)),
          Point(Trunc(Vertex[Triangle[i].vv2].x),
          Trunc(Vertex[Triangle[i].vv2].y))]);
      end;
    end;
  end;

  FlipBackPage;
end;


procedure TForm1.FormMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  TheMesh.AddPoint(X, Y, Z, U, V, MatIndex); // add a point to the mesh
  TheMesh.Mesh(True); // triangulate the mesh
  Draw; // draw the mesh on the forms canvas
  Form1.Caption := 'Points: ' + IntToStr(TheMesh.tPoints - 1) + '  Triangles: '
    + IntToStr(TheMesh.HowMany);
end;


end.
