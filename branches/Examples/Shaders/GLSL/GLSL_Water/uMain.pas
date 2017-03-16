//----------------------------------------------------------------------------//
// ѕо каким-то не пон€тным причинам проект лечше работает в оконном режиме,   //
// поэтому в нем используетс€ не полноценный полноэкранный режим (мен€етс€    //
// только разрешение экрана). ћожете попробовать выставить свойство           //
// BorderStyle главной формы на bsNone и своими глазами увидеть падение       //
// производительности (у мен€ падает с 270 до 23 FPS)                         //
//----------------------------------------------------------------------------//
unit uMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, GLScene, GLVectorFileObjects, GLWin32Viewer, GLCadencer,
  GLSLWater, GLSkydome, GLTexture, GLBitmapFont, GLWindowsFont,
  GLHUDObjects, GLObjects, GLScreen, ExtCtrls, GLCoordinates, GLCrossPlatform,
  GLBaseClasses, GLMaterial;

type
  TMainForm = class(TForm)
    Scene: TGLScene;
    Cadencer: TGLCadencer;
    SceneViewer: TGLSceneViewer;
    Camera: TGLCamera;
    LightSource: TGLLightSource;
    ground: TGLFreeForm;
    forest: TGLFreeForm;
    mountains: TGLFreeForm;
    dScene: TGLDummyCube;
    FPSTimer: TTimer;
    FPS: TGLHUDText;
    procedure FormCreate(Sender: TObject);
    procedure CadencerProgress(Sender: TObject; const deltaTime,
      newTime: Double);
    procedure FPSTimerTimer(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
     
  public
     
    sw, sh, bpp, rr: integer;
    averageFPS, step, sumFPS: cardinal;
  end;

var
  MainForm: TMainForm;
  water: TGLSLWater;

implementation

uses
  GLFile3DS, TGA, GLKeyboard;

{$R *.dfm}

procedure TMainForm.FormCreate(Sender: TObject);
var
  res: TResolution;
begin
  step := 0;
  averageFPS := 0;
  sumFPS := 0;

  font.LoadFromFile('font.gsf');

  water := TGLSLWater(dScene.AddNewChild(TGLSLWater));
  water.Scene := scene;
  water.SceneViewer := sceneViewer;
  water.Cadencer := Cadencer;
  water.Camera := camera;
  water.WaterWidth := 100;
  water.WaterHeight := 100;
  water.BumpTexture := 'caustics.bmp';
  water.Initalize;

  ground.LoadFromFile('ground.3ds');
  ground.Material.Texture.Image.LoadFromFile('ground.bmp');
  ground.Material.Texture.Disabled := false;
  ground.Scale.SetVector(0.8, 0.8, 0.8);

  forest.LoadFromFile('forest.3ds');
  forest.Material.Texture.Image.LoadFromFile('forest.tga');
  forest.Material.BlendingMode := bmTransparency;
  forest.Material.Texture.TextureMode := tmModulate;
  forest.Material.Texture.TextureWrap := twHorizontal;
  forest.Material.Texture.Disabled := false;
  forest.Scale.SetVector(0.8, 0.8, 0.8);

  Mountains.LoadFromFile('Mountains.3ds');
  Mountains.Material.Texture.Image.LoadFromFile('Mountains.tga');
  Mountains.Material.BlendingMode := bmTransparency;
  Mountains.Material.Texture.TextureMode := tmModulate;
  Mountains.Material.Texture.TextureWrap := twHorizontal;
  Mountains.Material.Texture.Disabled := false;
  Mountains.Scale.SetVector(0.8, 0.8, 0.8);


  mountains.MoveTo(water.Reflection);

  forest.MoveTo(water.Reflection);

  ground.MoveTo(water.Reflection);


end;

procedure TMainForm.CadencerProgress(Sender: TObject; const deltaTime,
  newTime: Double);
begin
  SceneViewer.Invalidate;

  if IsKeyDown(vk_escape) then begin
    self.close;
  end;
end;

procedure TMainForm.FPSTimerTimer(Sender: TObject);
begin
  FPS.Text := 'FPS: ' + IntToStr(Round(SceneViewer.FramesPerSecond)) + #10#13 +
  'press escape to exit';

  step := step + 1;
  sumFPS := sumFPS + Round(SceneViewer.FramesPerSecond);
  AverageFPS := round(sumFPS / step);
end;

procedure TMainForm.FormShow(Sender: TObject);
var
  res: TResolution;
begin
  res := GetIndexFromResolution(sw, sh, bpp);
  SetFullscreenMode(res, rr);
  SetFocus;
end;

end.
