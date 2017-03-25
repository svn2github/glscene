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

  GLObjects,
  GLGraph,
  GLScene,
  GLCoordinates,
  GLCadencer,
  GLWin32Viewer,
  GLCrossPlatform,
  GLBaseClasses,
  GLVectorGeometry,
  GLVectorTypes,
  GLMaterial,
  GLSpline;

type
  TForm1 = class(TForm)
    GLScene1: TGLScene;
    vp: TGLSceneViewer;
    cad: TGLCadencer;
    dc_cam: TGLDummyCube;
    cam: TGLCamera;
    grid1: TGLXYZGrid;
    dc_world: TGLDummyCube;
    GLLightSource1: TGLLightSource;
    floor: TGLCube;
    dc_walls: TGLDummyCube;
    player: TGLSphere;
    target: TGLSphere;
    grid2: TGLXYZGrid;
    path: TGLLines;
    procedure FormResize(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure cadProgress(Sender: TObject; const deltaTime, newTime: Double);
  public
    procedure getpath;
  end;

  TSrchStep = record
    x, y: integer; // real pos
    xc, yc: integer; // cell pos
    dir: integer; // ray
    back: integer; // previous step
  end;

const
  arrw = 36;
  arrh = 20;

var
  Form1: TForm1;
  // map
  arr: array [0 .. arrw - 1] of array [0 .. arrh - 1] of integer;
  tarr: array of array of TVector4i; // template

  // current and path arrays
  carr: array of array of TVector4i;
  sp: array of TSrchStep;

  TCellNull: TVector4i = (x: 0; y: 0; Z: 0; W: 0);

  px: word = 1; // player (start)
  py: word = 9; //
  tx: word = 34; // target
  ty: word = 10; //

  pl_path: TCubicSpline; // path for motion

implementation

{$R *.dfm}

//
// setup
//
procedure TForm1.FormCreate;
var
  i, j, k, m, n: integer;
begin
  Randomize;
  // init arrays
  SetLength(carr, arrw * 3, arrh * 3);
  SetLength(tarr, arrw * 3, arrh * 3);
  SetLength(sp, 20000);
  // clear map
  for i := 0 to arrw - 1 do
    for j := 0 to arrh - 1 do
      arr[i][j] := 0;

  // fill map and template
  for i := 0 to 250 do
  begin

    j := random(arrw);
    k := random(arrh);

    if ((j < px - 1) or (j > px + 1) or (k < py - 1) or (k > py + 1)) and
      ((j < tx - 1) or (j > tx + 1) or (k < ty - 1) or (k > ty + 1)) then
    begin
       with TGLCube.CreateAsChild(dc_walls) do
      begin
        Position.SetPoint(vectormake(j, arr[j][k] + 0.5, k));
         {// hide cubes
          CubeHeight := 0.02;
          Material.BlendingMode := bmAdditive;
          Material.FrontProperties.Diffuse.Alpha := 0.1;}
      end;

      for m := -1 to 1 do
        for n := -1 to 1 do
        begin
          tarr[j * 3 + m + 1, k * 3 + n + 1].X := j * 3000 + m * 1000;
          tarr[j * 3 + m + 1, k * 3 + n + 1].Y := k * 3000 + n * 1000;
          tarr[j * 3 + m + 1, k * 3 + n + 1].Z := 1000;
          tarr[j * 3 + m + 1, k * 3 + n + 1].W := 0;
        end;
      inc(arr[j][k]);
    end;
  end;
  getpath;
end;

//
// resize
//
procedure TForm1.FormResize;
begin
  vp.FieldOfView := 145;
end;

//
// show
//
procedure TForm1.FormShow;
begin
  cad.Enabled := true;
end;

//
// getpath
//
procedure TForm1.getpath;
const
  r = 300;
  d0 = 1159;
  d1 = 1004;
  d2 = 580; // for player
  d: array [-2 .. 13] of TVector2i = ((x: d2; y: - d1), (x: d1; y: - d2),
    (x: d0; y: 0), (x: d1; y: d2), (x: d2; y: d1), (x: 0; y: d0), (x: - d2;
    y: d1), (x: - d1; y: d2), (x: - d0; y: 0), (x: - d1; y: - d2), (x: - d2;
    y: - d1), (x: 0; y: - d0), (x: d2; y: - d1), (x: d1; y: - d2), (x: d0;
    y: 0), (x: d1; y: d2));
  l: array [0 .. 4] of integer = (0, -1, 1, -2, 2);
  // rays order F,L30°,R30°,L60°,R60°
var
  i, j, m: integer;
  sp0, sp1, st: integer;
  sph: TGLSphere;
  b: boolean;
  s: string;

procedure drawpath;
var
  j: integer;
begin
  j := i;
  path.Nodes.AddNode(tx, 0.1, ty);
  path.Nodes.AddNode(sp[m].x / 3000, 0.1, sp[m].y / 3000);
  repeat
    path.Nodes.AddNode(sp[j].x / 3000, 0.1, sp[j].y / 3000);
    j := sp[j].back;
  until j = 0;
  path.Nodes.AddNode(px, 0.1, py);
  pl_path := path.Nodes.CreateNewCubicSpline;
  player.TagFloat := path.Nodes.Count - 1;
end;

function check(cx, cy: integer; x, y: integer): boolean; // raycast
var
  j, k: integer;
  v: TVector4i;
begin
  result := true;
  for j := cx - 1 to cx + 1 do
    for k := cy - 1 to cy + 1 do
    begin
      v := carr[j, k];
      if v.W = 2 then
      begin
        drawpath;
        st := 0;
        exit;
      end;
      if v.Z = 0 then
        continue;
      if ((v.Z + r) * (v.Z + r) * 0.9 > (v.x - x) * (v.x - x) + (v.y - y) *
        (v.y - y)) then
      begin
        result := false;
        exit;
      end;
    end;
end;

procedure setCArr(cx, cy: integer; x, y, r, t: integer); // fill
begin
  carr[cx, cy].x := x;
  carr[cx, cy].y := y;
  carr[cx, cy].Z := r; // marker radius
  carr[cx, cy].W := t; // marker type

  // show marker's
  { if t = 3 then begin

    sph := TGLSphere.CreateAsChild( dc_walls );
    sph.Radius := 0.05 - t / 200;
    sph.Position.SetPoint( (cx - 1) / 3, 0.2 - t / 15, (cy - 1) / 3 );
    sph.Material.BlendingMode := bmAdditive;
    sph.Material.FrontProperties.Diffuse.Alpha := 0.3;

    sph := TGLSphere.CreateAsChild( dc_walls );
    sph.Radius := 0.1 - t / 100;
    sph.Position.SetPoint( x / 3000, 0, y / 3000 );
    sph.Material.BlendingMode := bmAdditive;
    sph.Material.FrontProperties.Diffuse.Alpha := 0.3;
    end; }
end;

begin
  // fill array (copy from template)
  for i := 0 to arrw * 3 - 1 do
    move(tarr[i][0], carr[i][0], arrh * 48);

  setCArr(px * 3 + 1, py * 3 + 1, px * 3000, py * 3000, 300, 1);
  setCArr(tx * 3 + 1, ty * 3 + 1, tx * 3000, ty * 3000, 300, 2);

  // init 4 dir's
  for i := 0 to 3 do
  begin
    sp[i].dir := i * 3;
    sp[i].back := 0;
    sp[i].x := px * 3000;
    sp[i].xc := px * 3 + 1;
    sp[i].y := py * 3000;
    sp[i].yc := py * 3 + 1;
  end;

  s := '';
  sp0 := 0; // first ray index
  sp1 := 3; // last ray index
  st := 1;
  while true do
  begin
    m := sp1 + 1;
    for i := sp0 to sp1 do
      for j := 0 to 4 do
        with sp[m] do
        begin
          x := sp[i].x + d[sp[i].dir + l[j]].x;
          xc := round(3 * x / 3000 + 1);
          if (xc <= 0) or (xc > arrw * 3 - 2) then
            continue;
          y := sp[i].y + d[sp[i].dir + l[j]].y;
          yc := round(3 * y / 3000 + 1);
          if (yc <= 0) or (yc > arrh * 3 - 2) then
            continue;

          b := (carr[xc, yc].Z = 0) and check(xc, yc, x, y);
          if st = 0 then
            exit;
          if b then
          begin
            dir := (sp[i].dir + l[j] + 12) mod 12;
            back := i;
            setCArr(xc, yc, x, y, r, 3);
            inc(m);
          end;
        end;
    if m = sp1 + 1 then
      break;
    sp0 := sp1 + 1;
    sp1 := m - 1;
  end;
end;

procedure TForm1.cadProgress;
begin
  if path.Nodes.Count > 0 then
  begin
    player.Position.SetPoint(pl_path.SplineAffineVector(player.TagFloat));
    player.TagFloat := player.TagFloat - deltaTime * 3;
    if player.TagFloat < 0 then
      player.TagFloat := path.Nodes.Count - 1;
  end;
  dc_cam.Position.SetPoint(player.AbsolutePosition);
end;

end.
