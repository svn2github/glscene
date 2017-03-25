unit Unit1;

interface

uses
  System.SysUtils,
  System.Classes,
  Vcl.Controls,
  Vcl.Forms,

  GLAsyncTimer,
  GLCadencer,
  GLScene,
  GLWin32Viewer,
  GLObjects,
  GLParticles,
  GLCoordinates,
  GLCrossPlatform,
  GLBaseClasses,
  GLVectorTypes,
  GLVectorGeometry;

type
  TForm1 = class(TForm)
    vp: TGLSceneViewer;
    GLScene1: TGLScene;
    cad: TGLCadencer;
    at: TGLAsyncTimer;
    light_1: TGLLightSource;
    dc_world: TGLDummyCube;
    dc_meshes: TGLDummyCube;
    cam_1: TGLCamera;
    GLPoints1: TGLPoints;
    procedure atTimer(Sender: TObject);
    procedure cadProgress(Sender: TObject; const deltaTime, newTime: Double);
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

//
// setup
//
procedure TForm1.FormCreate(Sender: TObject);
var
  i, j: integer;
  m: TMemoryStream;
  c: TVector3b;
  f: TVector3f;
  r: single;
begin
  m := TMemoryStream.Create;
  m.LoadFromFile('head.data');
  m.ReadBuffer(j, 4);
  for i := 0 to j - 1 do
  begin
    m.ReadBuffer(c.X, 3);
    f.X := (c.X - 128) * 0.02;
    f.Y := -(c.Z - 128) * 0.02;
    f.Z := (c.Y - 128) * 0.02;
    GLPoints1.Positions.Add(f);
    r := (sqr(f.X) + sqr(f.Y) + sqr(f.Z)) * 0.08;
    GLPoints1.Colors.Add(r, r, r, 1);
  end;
  m.Free;
end;

//
// cadProgress
//
procedure TForm1.cadProgress;
begin
  dc_world.Turn(deltaTime * 20);
end;

//
// timer
//
procedure TForm1.atTimer;
begin
  caption := 'PointHead: ' + vp.FramesPerSecondText(2);
  vp.ResetPerformanceMonitor;
end;

//
// resize
//
procedure TForm1.FormResize(Sender: TObject);
begin
  vp.FieldOfView := 152;
  GLPoints1.size := height * width * 5E-6;
end;

end.
