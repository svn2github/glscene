unit uMain;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.Menus,
  Vcl.ExtCtrls,
  Vcl.Imaging.JPeg,
  Vcl.StdCtrls,

  GLTexture,
  GLCadencer,
  GLWin32Viewer,
  GLScene,
  GLObjects,
  GLHUDObjects,
  GLAsyncTimer,
  GLFileTGA,
  GLMaterial,
  GLCoordinates,
  GLCrossPlatform,
  GLBaseClasses,

  OffSetAnim;


type
  TMain = class(TForm)
    Scene: TGLScene;
    Tick: TGLCadencer;
    tlp: TGLMaterialLibrary;
    Cam: TGLCamera;
    Cube1: TGLCube;
    AsyncTimer1: TGLAsyncTimer;
    hsp: TGLHUDSprite;
    SceneViewer: TGLSceneViewer;
    Button1: TButton;
    DummyCube: TGLDummyCube;
    Cube2: TGLCube;
    procedure FormCreate(Sender: TObject);
    procedure TickProgress(Sender: TObject; const deltaTime,
      newTime: Double);
    procedure AsyncTimer1Timer(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure Button1Click(Sender: TObject);
  end;

Const
  Go = 22;
  Stop = 23;

var
  Main: TMain;
  AM, AM2: TOffSetAnim;
  BaseDir: String;
  State: Integer;

implementation

{$R *.DFM}

procedure TMain.FormCreate(Sender: TObject);
var
  Mat1, Mat2: TGLLibMaterial;
begin
  //Create the animation manager...
  AM := TOffSetAnim.Create;
  AM2 := TOffSetAnim.Create;
  BaseDir := ExtractFilePath(ParamStr(0));

  //Load material and assign it to objects...
  Mat1:=tlp.AddTextureMaterial('Smoke', BaseDir + 'Smoke.tga', True);
  Mat2:=tlp.AddTextureMaterial('SmokeBlack', BaseDir + 'SmokeBlack.tga', True);

  Mat1.Material.BlendingMode:=bmTransparency;
  Mat1.Material.Texture.TextureMode:=tmModulate;

  Mat2.Material.BlendingMode:=bmTransparency;
  Mat2.Material.Texture.TextureMode:=tmModulate;

  Cube1.Material.LibMaterialName := 'SmokeBlack';
  hsp.Material.LibMaterialName := 'Smoke';


  //Create Animation...
  AM.MakeAnim(64, 64, 1, tlp, 24, apmLoop);
  AM2.MakeAnim(64, 64, 0, tlp, 24, apmOnce);
  State := Go;
end;

procedure TMain.TickProgress(Sender: TObject; const deltaTime,
  newTime: Double);
begin
  //just call this, and watch the magic happen... :)
  if State = Go then begin
    AM.Tick(newtime);
    AM2.Tick(newtime);
  end;
  DummyCube.Turn(-30*DeltaTime);
end;

procedure TMain.AsyncTimer1Timer(Sender: TObject);
begin
  Caption := 'OffSet Animation Demo - [' + SceneViewer.FramesPerSecondText(1) + ']';
  SceneViewer.ResetPerformanceMonitor;
end;

procedure TMain.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  State := Stop;
  AM.Free;
  AM2.Free;
end;

procedure TMain.Button1Click(Sender: TObject);
begin
  if AM2.CFrame = 0
  then AM2.SetFrame(1)
  else AM2.SetFrame(0);
end;

end.
