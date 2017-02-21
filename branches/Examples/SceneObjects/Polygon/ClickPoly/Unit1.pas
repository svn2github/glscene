unit Unit1;

interface

uses
  Winapi.Windows, System.Classes, Vcl.Controls, Vcl.Forms,
   
  GLCadencer, GLWin32Viewer, GLScene, GLObjects, GLVectorGeometry,
  GLVectorTypes,
  GLGraph, GLGeomObjects, GLCoordinates, GLCrossPlatform, GLBaseClasses;

type

  TForm1 = class(TForm)
    GLScene1: TGLScene;
    vp: TGLSceneViewer;
    cad: TGLCadencer;
    cam: TGLCamera;
    dc: TGLDummyCube;
    poly_floor: TGLPolygon;
    point: TGLFrustrum;
    poly_wall: TGLPolygon;
    GLXYZGrid1: TGLXYZGrid;
    GLCube1: TGLCube;
    dc_point: TGLDummyCube;
    procedure cadProgress(Sender: TObject; const deltaTime, newTime: Double);
    procedure vpMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormCreate(Sender: TObject);
  public

    function rayCast(poly: TGLPolygon; var v, vn: TVector): boolean;

  end;

const
  floor: array [0 .. 7] of TVector3f = ((X: 0; Y: 3; Z: 0), (X: 0; Y: 3; Z: 4),
    (X: - 8; Y: 3; Z: 4), (X: - 8; Y: 3; Z: - 5), (X: 8; Y: 3; Z: - 5), (X: 8;
    Y: 3; Z: 8), (X: 5; Y: 3; Z: 8), (X: 5; Y: 3; Z: 0));
  wall: array [0 .. 13] of TVector3f = ((X: - 8; Y: 3; Z: - 5), (X: - 8; Y: 11;
    Z: - 5), (X: 8; Y: 11; Z: - 5), (X: 8; Y: 3; Z: - 5), (X: 0; Y: 3; Z: - 5),
    (X: 0; Y: 6; Z: - 5), (X: 5; Y: 6; Z: - 5), (X: 5; Y: 10; Z: - 5), (X: 0;
    Y: 10; Z: - 5), (X: 0; Y: 3; Z: - 5), (X: - 3; Y: 3; Z: - 5), (X: - 3; Y: 8;
    Z: - 5), (X: - 6; Y: 8; Z: - 5), (X: - 6; Y: 3; Z: - 5));

var
  Form1: TForm1;

  cur_pos: TVector = (X: 0; Y: 100; Z: 0; W: 1);
  cur_norm: TVector = (X: 0; Y: 1; Z: 0; W: 0);

implementation

{$R *.dfm}

//
// setup
//
procedure TForm1.FormCreate;
var
  i: Integer;
begin
  for i := 0 to high(floor) + 1 do
    poly_floor.Nodes.AddNode(floor[i mod (high(floor) + 1)]);
  for i := 0 to high(wall) + 1 do
    poly_wall.Nodes.AddNode(wall[i mod (high(wall) + 1)]);
  cad.FixedDeltaTime := 1 / GetDeviceCaps(getDC(Handle), 116);
end;

//
// cadProgress
//
procedure TForm1.cadProgress;
begin
  dc.Turn(deltaTime * 10);
  dc_point.AbsolutePosition := cur_pos;
  dc_point.AbsoluteUp := cur_norm;
end;

//
// rayCast (screen)
//
function TForm1.rayCast(poly: TGLPolygon; var v, vn: TVector): boolean;
var
  i, j, n0, n1: Integer;
  f1, f2, f3: single;
  mpos: TPoint;
begin
  mpos := vp.ScreenToClient(mouse.CursorPos);
  vn := VectorCrossProduct(VectorSubtract(poly.Nodes[0].AsVector,
    poly.Nodes[1].AsVector), VectorSubtract(poly.Nodes[2].AsVector,
    poly.Nodes[1].AsVector));

  vp.Buffer.ScreenVectorIntersectWithPlane
    (vectormake(mpos.X, vp.Height - mpos.Y, 0), poly.Nodes[0].AsVector, vn, v);

  j := 0;
  for i := 0 to poly.Nodes.Count - 1 do
  begin

    n0 := i;
    n1 := (i + 1) mod poly.Nodes.Count;

    f1 := VectorAngleCosine(VectorSubtract(v, poly.Nodes[n0].AsVector),
      VectorSubtract(poly.Nodes[n1].AsVector, poly.Nodes[n0].AsVector));
    f2 := VectorAngleCosine(VectorSubtract(v, poly.Nodes[n1].AsVector),
      VectorSubtract(poly.Nodes[n0].AsVector, poly.Nodes[n1].AsVector));
    f3 := VectorAngleCosine(vn, VectorCrossProduct(VectorSubtract(v,
      poly.Nodes[n0].AsVector), VectorSubtract(poly.Nodes[n1].AsVector,
      poly.Nodes[n0].AsVector)));

    if (f1 > 0) and (f2 > 0) then
      inc(j, sign(f3));

  end;
  result := j > 0;
end;

//
// click
//
procedure TForm1.vpMouseDown;

function test(p: TGLPolygon): boolean;
var
  v1, v2: TVector;
begin
  result := rayCast(p, v1, v2);
  if result then
  begin
    cur_pos := v1;
    cur_norm := v2;
  end;
end;

begin
  test(poly_floor);
  test(poly_wall);
end;

end.
