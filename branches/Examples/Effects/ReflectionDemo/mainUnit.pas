unit mainUnit;

interface

uses
  Winapi.Windows, Winapi.Messages,
  System.SysUtils, System.Variants, System.Classes, System.Math,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.Menus,
  Vcl.StdCtrls, Vcl.Imaging.Jpeg,

   
  GLScene, GLObjects, GLCadencer, GLTexture, GLWin32Viewer,
  GLVectorFileObjects, GLSkyBox, GLPhongShader, GLAsyncTimer,
  GLTexCombineShader, GLMultiMaterialShader, GLCelShader,
  GLHiddenLineShader, GLUserShader, GLMaterial, GLCustomShader, GLAsmShader,
  GLCoordinates, GLCrossPlatform, GLBaseClasses, GLColor,
  GLFile3ds, GLVectorGeometry;

type
  TDemoFrm = class(TForm)
    DemoScene: TGLScene;
    Display: TGLSceneViewer;
    MatLib: TGLMaterialLibrary;
    Camera: TGLCamera;
    LightSource: TGLLightSource;
    SkyBox: TGLSkyBox;
    plane: TGLPlane;
    CelShader: TGLCelShader;
    PhongShader: TGLPhongShader;
    Timer: TGLAsyncTimer;
    Cadencer: TGLCadencer;
    MainMenu1: TMainMenu;
    miFPS: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
    procedure CadencerProgress(Sender: TObject; const deltaTime,
      newTime: Double);
    procedure DisplayMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure FormMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
  private
    procedure CreateMaterials;
    procedure AddReflect;
    procedure AddTransparency;
    procedure AddTexture;
    procedure AddShader;
  public
     
  end;

var
  DemoFrm: TDemoFrm;
  mx, my: integer;
  md: boolean;
  car: TGLFreeForm;

  _initstart:boolean=true;

implementation

{$R *.dfm}

procedure TDemoFrm.FormCreate(Sender: TObject);
var
  i: integer;
begin
  CreateMaterials;

  car := TGLFreeForm(DemoScene.Objects.AddNewChild(TGLFreeForm));
  car.MaterialLibrary:=MatLib;
  car.Scale.SetVector(0.005, 0.005, 0.005);
  car.LoadFromFile('car\car.3ds');
  camera.TargetObject:=car;

  with SkyBox do begin
    MaterialLibrary := matLib;
    MatNameTop := 'sky5';
    MatNameLeft := 'sky1';
    MatNameFront := 'sky2';
    MatNameRight := 'sky3';
    MatNameBack := 'sky4';
    MatNameBottom := 'sky3';
  end;

  AddReflect;
  AddTexture;
  AddTransparency;
  AddShader;

  Cadencer.enabled:=true;
  Timer.Enabled:=true;
end;


procedure TDemoFrm.CreateMaterials;
begin
  MatLib.AddTextureMaterial('sky1', 'sky 7 1.jpg');
  MatLib.AddTextureMaterial('sky2', 'sky 7 2.jpg');
  MatLib.AddTextureMaterial('sky3', 'sky 7 3.jpg');
  MatLib.AddTextureMaterial('sky4', 'sky 7 4.jpg');
  MatLib.AddTextureMaterial('sky5', 'sky 7 5.jpg');
  MatLib.AddTextureMaterial('sky6', 'sky 7 6.jpg');

  with MatLib.AddTextureMaterial('reflectionMat', '') do begin
   Material.BlendingMode:=bmAdditive;
    with Material.Texture do begin
      Image.LoadFromFile('car\reflectionMap.jpg');
      MappingMode := tmmCubeMapReflection;
      EnvColor.Color := clrWhite;
      ImageBrightness := 0.3;
      TextureMode := tmBlend;
    end;
  end;
  Plane.Material.Texture.Enabled:=true;
  Plane.Material.Texture.Image.LoadFromFile('grycon3.jpg');
end;

procedure TDemoFrm.DisplayMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  if ssLeft in Shift then
    Camera.MoveAroundTarget(my-y, mx-x)
  else if shift = [ssRight] then
    Camera.AdjustDistanceToTarget(1.0 + (y - my) / 100);
  mx := x;
  my := y;
end;

procedure TDemoFrm.AddReflect;
begin
  with MatLib.LibMaterialByName('Metal') do begin
       Material.Texture.TextureMode := tmModulate;
       Texture2Name:='reflectionMat';
  end;
  with MatLib.LibMaterialByName('Inter') do begin
       Material.Texture.TextureMode := tmModulate;
  end;
  with MatLib.LibMaterialByName('Glass') do begin
       Material.Texture.TextureMode := tmModulate;
       Texture2Name:='reflectionMat';
  end;
  with MatLib.LibMaterialByName('Corp') do begin
       Material.Texture.TextureMode := tmModulate;
       Texture2Name:='reflectionMat';
  end;
end;

procedure TDemoFrm.AddTransparency;
begin
  with MatLib.LibMaterialByName('Glass') do begin
       Material.FrontProperties.Diffuse.Alpha:=0.5;
       Material.BlendingMode:=BmTransparency;
  end;
end;

procedure TDemoFrm.AddTexture;
begin
    MatLib.LibMaterialByName('Inter').Shader:=CelShader;
    MatLib.LibMaterialByName('Metal').Shader:=PhongShader;
end;

procedure TDemoFrm.AddShader;
begin
  with MatLib.LibMaterialByName('Corp') do begin
       Material.Texture.Enabled:=true;
       Material.Texture.Image.LoadFromFile('clouds.jpg');
  end;
end;


procedure TDemoFrm.FormMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  Camera.AdjustDistanceToTarget(Power(1.1, WheelDelta/120));
end;

procedure TDemoFrm.TimerTimer(Sender: TObject);
begin
  miFPS.Caption := Format('%.1f FPS', [Display.FramesPerSecond]) +
    '; Traingles: ' + IntToStr(car.MeshObjects.TriangleCount);
  Display.ResetPerformanceMonitor;
  if _initstart then
  begin
    _initstart := false;
    car.StructureChanged;
  end;
end;

procedure TDemoFrm.CadencerProgress(Sender: TObject; const deltaTime,
  newTime: Double);
begin
  Camera.MoveAroundTarget(0,30*deltatime);
end;

end.
