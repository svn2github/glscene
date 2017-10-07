unit main;

interface

uses
  Winapi.Windows, Winapi.Messages,
  System.SysUtils, System.Classes, Vcl.Forms, Vcl.Controls,
   
  GLScene, GLObjects, GLWin32Viewer, GLCadencer, GLKeyboard, GLCrossPlatform,
  GLHUDObjects, GLCoordinates, GLBaseClasses, GLMaterial, GLTexture,
  GLAsyncTimer, GLAvi;

type
  TForm1 = class(TForm)
    GLScene1: TGLScene;
    Cam: TGLCamera;
    GLCadencer1: TGLCadencer;
    GLLightSource1: TGLLightSource;
    vp: TGLSceneViewer;
    vHUD: TGLHUDSprite;
    AsyncTimer1: TGLAsyncTimer;
    matlib: TGLMaterialLibrary;
    GLCube1: TGLCube;
    procedure GLCadencer1Progress(Sender: TObject;
      const deltaTime, newTime: Double);
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure AsyncTimer1Timer(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  end;

var
  Form1: TForm1;

  avi: TGLAvi;

implementation

{$R *.dfm}

// formCreate
//
procedure TForm1.FormCreate(Sender: TObject);
begin
  avi := TGLAvi.Create;
  avi.UserFrameRate := 25;
  avi.Filename := '1.avi';
  avi.Texture := matlib.LibMaterialByName('vid').Material.Texture;
end;

// GLCadencer1Progress
//
procedure TForm1.GLCadencer1Progress(Sender: TObject;
  const deltaTime, newTime: Double);
begin
  avi.update(newTime, deltaTime);
  GLCube1.TurnAngle := newTime * 20;
  vp.Refresh;
  if iskeydown(vk_escape) then
    close;
end;

// timer
//
procedure TForm1.AsyncTimer1Timer(Sender: TObject);
begin
  caption := vp.FramesPerSecondText(2);
  vp.ResetPerformanceMonitor;
end;

procedure TForm1.FormResize(Sender: TObject);
begin
  vHUD.Width := vp.Width;
  vHUD.Height := vp.Height;
  vHUD.Position.SetPoint(vp.Width div 2, vp.Height div 2, 0);
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  avi.Free;
end;

end.
