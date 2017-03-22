unit Unit1;

interface

uses
  Windows, Messages,
  System.SysUtils, System.Variants,
  System.Math,  Classes,
  Vcl.Graphics, Vcl.Controls,
  Vcl.Forms, Vcl.Dialogs,
  GLCadencer,
  GLWin32Viewer,
  GLScene,
  GLObjects,
  GLVectorGeometry,
  GLKeyboard,
  GLTerrainRenderer,
  GLGeomObjects,
  GLMultiPolygon,
  GLExtrusion,
  GLAsyncTimer,
  GLHeightData,
  GLCoordinates,
  GLCrossPlatform,
  GLBaseClasses;

type
  TForm1 = class(TForm)
    GLScene1: TGLScene;
    vp: TGLSceneViewer;
    GLCadencer1: TGLCadencer;
    cam: TGLCamera;
    dc_world: TGLDummyCube;
    dc_player: TGLDummyCube;
    dc_cam: TGLDummyCube;
    dc_target: TGLDummyCube;
    ter: TGLTerrainRenderer;
    GLCustomHDS1: TGLCustomHDS;
    GLExtrusionSolid1: TGLExtrusionSolid;
    targ: TGLSprite;
    AsyncTimer1: TGLAsyncTimer;
    GLCamera1: TGLCamera;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure GLCadencer1Progress(Sender: TObject;
      const deltaTime, newTime: Double);
    procedure FormCreate(Sender: TObject);
    procedure GLCustomHDS1StartPreparingData(heightData: TGLHeightData);
    procedure AsyncTimer1Timer(Sender: TObject);
  private
     
  public
     
  end;

var
  Form1: TForm1;
  _m: array of array of single;
  dx, dz: Double;

implementation

{$R *.dfm}

procedure AreaInit;
var
  a1, a2, a3, x, y, d: integer;
  b1: single;
begin

  SetLength(_m, 256, 256);

  randomize;

  // generate landscape [fractal]
  for a1 := 0 to 7 do
    for a2 := 0 to 7 do
      _m[a1 shl 5, a2 shl 5] := (random - 0.5) * 256;
  for a3 := 0 to 4 do
  begin
    d := 16 shr a3;
    for a1 := 0 to (1 shl (a3 + 3)) - 1 do
      for a2 := 0 to (1 shl (a3 + 3)) - 1 do
      begin
        b1 := (random - 0.5) * (128 shr a3);
        x := d + a1 shl (5 - a3);
        y := a2 shl (5 - a3);
        _m[x, y] := (_m[x - d, y] + _m[(x + d) and 255, y]) / 2 + b1;
        b1 := (random - 0.5) * (128 shr a3);
        x := a1 shl (5 - a3);
        y := d + a2 shl (5 - a3);
        _m[x, y] := (_m[x, y - d] + _m[x, (y + d) and 255]) / 2 + b1;
        b1 := (random - 0.5) * (128 shr a3);
        x := d + a1 shl (5 - a3);
        y := d + a2 shl (5 - a3);
        _m[x, y] := (_m[x - d, y - d] + _m[x - d, (y + d) and 255] +
          _m[(x + d) and 255, y - d] + _m[(x + d) and 255, (y + d) and 255]
          ) / 4 + b1;
      end;
  end;

  // turn to planet type
  for a1 := 0 to 255 do
    for a2 := 0 to 255 do
      _m[a1, a2] := power(1.011, power((_m[a1, a2] + 384) * 0.101, 1.61));
end;

procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  AsyncTimer1.Enabled := false;
  GLCadencer1.Enabled := false;
  vp.Free;
end;

procedure TForm1.GLCadencer1Progress(Sender: TObject;
  const deltaTime, newTime: Double);
var
  v1: TVector;
  b1, b2: single;
begin
  // -- Y test --------------------------------------------------------------------
  v1 := VectorSubtract(dc_target.AbsolutePosition,
    VectorScale(dc_target.AbsoluteDirection, 25));
  dc_target.Position.y := dc_target.Position.y +
    (-dc_target.Position.y + 10 + ter.InterpolatedHeight(v1)) * deltaTime;

  // -- move ----------------------------------------------------------------------
  if iskeydown(vk_up) and (dz < 1) then
    dz := dz + deltaTime / (0.1 + dz * dz)
  else
    dz := dz * (1 - deltaTime * 2);
  dc_target.Move(-dz * deltaTime * 40);

  // -- turn ----------------------------------------------------------------------
  if iskeydown(vk_left) and (dx < 2) then
    dx := dx + deltaTime / (0.1 + dx * dx / 2);

  if iskeydown(vk_right) and (dx > -2) then
    dx := dx - deltaTime / (0.1 + dx * dx / 2);

  dx := dx * (1 - deltaTime);

  dc_target.Turn(-dx * deltaTime * 50);
  dc_player.AbsoluteDirection := dc_target.AbsoluteDirection;
  dc_player.RollAngle := dx * 45;

  // -- camera & player correction (blackmagic)------------------------------------
  b1 := dc_cam.DistanceTo(dc_target.AbsolutePosition);
  if abs(b1 - 4) > 0.1 then
    dc_cam.Position.Translate
      (VectorScale(VectorSubtract(dc_target.AbsolutePosition,
      dc_cam.AbsolutePosition), (b1 - 4) * deltaTime));
  if abs(dc_cam.Position.y - dc_target.Position.y - 0.5) > 0.1 then
    dc_cam.Position.y := dc_cam.Position.y -
      (dc_cam.Position.y - dc_target.Position.y - 0.5) * deltaTime * 10;

  b1 := dc_player.DistanceTo(dc_target.AbsolutePosition);
  if abs(b1 - 1) > 0.1 then
    dc_player.Position.Translate
      (VectorScale(VectorSubtract(dc_target.AbsolutePosition,
      dc_player.AbsolutePosition), (b1 - 1) * deltaTime * 2));

  dc_cam.PointTo(dc_target.AbsolutePosition, vectormake(0, 1, 0));

  // -- update scene --------------------------------------------------------------
  vp.Invalidate;

  // -- close if press Escape -----------------------------------------------------
  if iskeydown(vk_escape) then
    close;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  AreaInit;
end;

procedure TForm1.GLCustomHDS1StartPreparingData(heightData: TGLHeightData);
var
  a1, a2: integer;
  ln: PSingleArray;
begin
  heightData.DataState := hdsPreparing;
  with heightData do
  begin
    Allocate(hdtSingle);
    for a1 := ytop to ytop + size - 1 do
    begin
      ln := SingleRaster[a1 - ytop];
      for a2 := xleft to xleft + size - 1 do
        ln[a2 - xleft] := _m[a2 and 255, a1 and 255] * 0.1;
    end;
  end;
end;

procedure TForm1.AsyncTimer1Timer(Sender: TObject);
begin
  caption := vp.FramesPerSecondText(2);
  vp.ResetPerformanceMonitor;
end;

end.
