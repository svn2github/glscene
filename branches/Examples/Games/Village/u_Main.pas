unit u_Main;

interface

uses

  Winapi.Windows, Winapi.Messages,
  System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Forms, Vcl.Dialogs, Vcl.Controls,

  uLog, uDDSTex,

   
  GLFile3DS, GLAsyncTimer, GLMaterial, GLCadencer, GLWin32Viewer,
  GLCrossPlatform,
  GLBaseClasses, GLScene, GLCoordinates, GLObjects, GLVectorFileObjects,
  GLKeyboard, GLVectorGeometry, GLTexture;

type
  TForm1 = class(TForm)
    GLScene1: TGLScene;
    vp: TGLSceneViewer;
    cad: TGLCadencer;
    matlib: TGLMaterialLibrary;
    at: TGLAsyncTimer;
    cam: TGLCamera;
    ff: TGLFreeForm;
    dc_cam: TGLDummyCube;
    sky: TGLSphere;
    ff_boat: TGLFreeForm;
    procedure FormCreate(Sender: TObject);
    procedure atTimer(Sender: TObject);
    procedure cadProgress(Sender: TObject; const dt, nt: Double);
    procedure vpMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormShow(Sender: TObject);
  end;

  // for smooth navigation
  t_vpoint = record
    v0, v1: single;
  end;

var
  Form1: TForm1;

  m_turn: boolean;
  m_dx, m_dy: t_vpoint;
  m_pos: TPoint;

  sw2, sh2: Integer;
  water: TGLLibMaterial;

  cc: cardinal;
  fframe: Integer = -1;

implementation

{$R *.dfm}

//
// setup
//
procedure TForm1.FormCreate;
const // material list (from 3DS)
  c: array [0 .. 28] of string = ('d40839b9', 'hibara01', 'beton', 'hibara03',
    'ed79fe48', 'dom002', 'dom003', 'dom004', 'e1e075eb', '30e5522d', 'domedv',
    'dom005', 'dom009', 'dom008', 'stoun', 'tavern', 'shipyard', 'domahati',
    'pena', 'BackGrnd', 'viveski', 'plane', 'tovpalat', 'scaly', 'tovar01',
    'sunduki', 'osobnyk', 'statui01', '63d9d8cd');
var
  m: TGLLibMaterial;
  i, j: Integer;
begin

  sw2 := screen.Width div 2;
  sh2 := screen.Height div 2;

  SetCurrentDir('data');

  cc := GetTickCount();

  log('load texture list:', 10);
  for i := 0 to high(c) do
  begin
    log('"' + c[i] + '.dds' + '"');
    DDSTex(matlib, 'tex' + inttostr(i), c[i] + '.dds');
  end;

  water := matlib.LibMaterialByName('tex18');
  m := matlib.LibMaterialByName('tex19');
  if m <> nil then
  begin
    m.Material.BlendingMode := bmAlphaTest100;
    m.Material.Texture.TextureMode := tmReplace;
  end;

  log('"sky.dds"');
  with DDSTex(matlib, 'sky', 'sky.dds') do
  begin
    Material.Texture.MappingMode := tmmCubeMapNormal;
    Material.Texture.TextureWrap := twNone;
  end;
  sky.Material.LibMaterialName := 'sky';

  log('load models:', 10);
  log('"village.3ds"');
  ff.LoadFromFile('village.3ds');
  ff.BeginUpdate;
  for i := 0 to ff.MeshObjects.Count - 1 do
    for j := 0 to ff.MeshObjects[i].FaceGroups.Count - 1 do
      ff.MeshObjects[i].FaceGroups[j].MaterialName := 'tex' + inttostr(i);
  ff.MaterialLibrary := matlib;
  ff.EndUpdate;

  log('"boat.3ds"');
  ff_boat.LoadFromFile('boat.3ds');
  ff_boat.MeshObjects[0].FaceGroups[0].MaterialName := 'tex28';
  ff_boat.MaterialLibrary := matlib;
  ff_boat.StructureChangedNoPrepare;

  log('form create:', 10);
  log(format('%.3f sec', [(GetTickCount - cc) / 1000]));
  cc := GetTickCount();

end;

//
// cadProgress
//
procedure TForm1.cadProgress;
var
  spd, f: single;

  function _lerp(var md: t_vpoint): single;
  var
    f: single;
  begin
    f := lerp(md.v0, md.v1, dt * 10);
    result := f - md.v0;
    md.v0 := f;
  end;

begin

  // get first frame time
  if fframe < 1 then
  begin
    if fframe = 0 then
    begin
      log('first frame:', 10);
      log(format('%.3f sec', [(GetTickCount - cc) / 1000]));
    end;
    inc(fframe);
  end;

  // mouse
  if m_turn then
    with mouse.CursorPos do
    begin
      m_dx.v1 := m_dx.v1 + (X - sw2) * 0.3;
      m_dy.v1 := clampValue(m_dy.v1 + (sh2 - Y) * 0.3, -90, 90);
      if (X <> sw2) or (Y <> sh2) then
        mouse.CursorPos := point(sw2, sh2);
    end;
  dc_cam.Turn(_lerp(m_dx));
  cam.Pitch(_lerp(m_dy));

  if (not iskeydown(vk_rbutton)) and (m_turn) then
  begin
    m_turn := false;
    mouse.CursorPos := m_pos;
    showCursor(true);
  end;

  // keyboard
  if iskeydown(vk_shift) then
    spd := 10
  else
    spd := 5;

  if iskeydown(ord('W')) then
    f := 1
  else if iskeydown(ord('S')) then
    f := -1
  else
    f := 0;
  dc_cam.Position.Translate(vectorscale(cam.AbsoluteDirection, f * dt * spd));

  if iskeydown(ord('A')) then
    f := 1
  else if iskeydown(ord('D')) then
    f := -1
  else
    f := 0;
  dc_cam.Position.Translate(vectorscale(cam.AbsoluteRight, f * dt * spd));

  // animation
  ff_boat.PitchAngle := sin(nt) * 2 + 3;
  if water <> nil then
    water.TextureOffset.Y := -nt / 10;

  sky.AbsolutePosition := cam.AbsolutePosition;

end;

//
// mouseDown
//
procedure TForm1.vpMouseDown;
begin

  if (Button = TMouseButton.mbRight) then
  begin
    m_turn := true;
    m_pos := mouse.CursorPos;
    showCursor(false);
    mouse.CursorPos := point(sw2, sh2);
  end;

end;

//
// Timer
//
procedure TForm1.atTimer;
begin

  caption := 'Village: ' + vp.FramesPerSecondText(2);
  vp.ResetPerformanceMonitor;

end;

//
// show
//
procedure TForm1.FormShow;
begin

  cad.Enabled := true;

end;

end.
