unit umain;

interface

uses
  Winapi.Windows,
  System.SysUtils, System.Classes,
  Vcl.Controls, Vcl.Forms, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.Graphics,
  //GLS
  GLWin32Viewer, GLCrossPlatform, GLBaseClasses, GLScene, GLObjects, GLMaterial,
  GLHUDObjects, GLCoordinates, GLMesh, GLVectorFileObjects, GLCadencer,
  GLRenderContextInfo, GLAsyncTimer, GLVectorTypes, GLVectorGeometry, openGL1x,
  GLTexture, GLContext, GLColor, GLFile3DS, GLFileDDS, GLCompositeImage,

  ucpuinst, ugpuinst, uglfreeform;


type
  TForm1 = class(TForm)
    GLScene1: TGLScene;
    cam: TGLCamera;
    dc_cam: TGLDummyCube;
    Bevel1: TBevel;
    cad: TGLCadencer;
    at: TGLAsyncTimer;
    back: TGLHUDSprite;
    vp: TGLSceneViewer;
    Button1: TButton;
    rb1: TRadioButton;
    rb2: TRadioButton;
    dc_world: TGLDummyCube;
    tmp_obj: TGLFreeForm;
    rb3: TRadioButton;
    rb4: TRadioButton;
    procedure cadProgress(Sender: TObject; const deltaTime,newTime: Double);
    procedure atTimer(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure rb2Click(Sender: TObject);
  public
    cpuinst_obj: TBenchCPUInst;
    gpuinst_obj: TBenchGPUInst;
    freeform_obj: TBenchGLFreeForm;
  end;

var
  Form1: TForm1;

  fps: array[0..5] of TPoint = ((x:0;y:0),(x:0;y:0),(x:0;y:0),(x:0;y:0),(x:0;y:0),(x:0;y:0));
  predelta: integer = 2;
  cur: integer = 0;

implementation

{$R *.dfm}

// setup
//
procedure TForm1.FormCreate(Sender: TObject);
begin
  cad.FixedDeltaTime := 1 / GetDeviceCaps(getDC(Handle), 116);
  vp.Buffer.RenderingContext.Activate;
  vp.width := 824;
  clientWidth := 1024;
  clientHeight := 512;
  tmp_obj.LoadFromFile( 'cube.3ds' );
  with tmp_obj.Material do begin
    with TextureEx[0].Texture do begin
      Image.LoadFromFile( 'logo.dds' );
      Disabled := false;
    end;
    with TextureEx[1].Texture do begin
      Image.LoadFromFile( 'shad.dds' );
      Disabled := false;
    end;
  end;

  cpuinst_obj := TBenchCPUInst.CreateAsChild( dc_world, tmp_obj );
  //cpuinst_obj.Visible := false;

  gpuinst_obj := TBenchGPUInst.CreateAsChild( dc_world, tmp_obj );
  gpuinst_obj.Visible := false;

  freeform_obj := TBenchGLFreeForm.CreateAsChild( dc_world, tmp_obj );
  freeform_obj.Visible := false;
end;


//
// cadProgress
//
procedure TForm1.cadProgress;
var
    dt: integer;
begin

  dc_cam.TurnAngle := -newtime*10;

  if predelta < 0 then begin
    dt := round(100/vp.LastFrameTime);
    fps[cur].X := (dt*9 + fps[cur].X) div 10;
    if dt > fps[cur].Y then
      fps[cur].Y := dt;
  end;
end;

//
// atTimer
//
procedure TForm1.atTimer;
var
    dt: integer;
begin
  dec(predelta);
  if predelta >= 0 then exit;

  canvas.FillRect( rect( 32,40+cur*72,160,80+cur*70 ));
  canvas.Font.Color := $00eeff;
  canvas.TextOut( 33,40+cur*72, format('AVR: %.2f',[fps[cur].X/100]));
  canvas.TextOut( 33,60+cur*72, format('MAX: %.2f',[fps[cur].Y/100]));

//  caption := cap + ' / fps: ' + format('%.2f', [vp.FramesPerSecond]);
  vp.ResetPerformanceMonitor;
end;


procedure TForm1.FormShow(Sender: TObject);
begin
  cad.Enabled := true;
  at.Enabled := true;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  freeform_obj.SaveToFile( 'qwe.ase' );
end;

procedure TForm1.rb2Click(Sender: TObject);
begin
  cpuinst_obj.Visible := sender = rb1;
  gpuinst_obj.Visible := sender = rb2;
  freeform_obj.Visible := sender = rb3;
 // vbo_obj.Visible := sender = rb4;
  cur := TControl(sender).tag;
end;

end.

