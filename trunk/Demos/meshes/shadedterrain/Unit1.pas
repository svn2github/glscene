{: Shaded terrain rendering demo.<p>

}
unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  GLScene, GLTerrainRenderer, GLObjects, GLMisc, jpeg, GLHeightData,
  ExtCtrls, GLCadencer, StdCtrls, GLTexture, GLHUDObjects, GLBitmapFont,
  GLSkydome, GLWin32Viewer, VectorGeometry, GLLensFlare, GLBumpmapHDS;

type
  TForm1 = class(TForm)
    GLSceneViewer1: TGLSceneViewer;
    GLBitmapHDS1: TGLBitmapHDS;
    GLScene1: TGLScene;
    GLCamera1: TGLCamera;
    DummyCube1: TGLDummyCube;
    TerrainRenderer1: TGLTerrainRenderer;
    Timer1: TTimer;
    GLCadencer1: TGLCadencer;
    GLMaterialLibrary1: TGLMaterialLibrary;
    SkyDome1: TGLSkyDome;
    SPSun: TGLSprite;
    GLLensFlare: TGLLensFlare;
    GLDummyCube1: TGLDummyCube;
    procedure GLSceneViewer1MouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure GLSceneViewer1MouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure Timer1Timer(Sender: TObject);
    procedure GLCadencer1Progress(Sender: TObject; const deltaTime,
      newTime: Double);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure GLSceneViewer1BeforeRender(Sender: TObject);
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
    bumpmapHDS : TGLBumpmapHDS;

    mx, my : Integer;
    fullScreen : Boolean;
    FCamHeight : Single;
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

uses Keyboard, OpenGL1x;

procedure TForm1.FormCreate(Sender: TObject);
begin
   SetCurrentDir(ExtractFilePath(Application.ExeName)+'..\..\media');
   // 8 MB height data cache
   // Note this is the data size in terms of elevation samples, it does not
   // take into account all the data required/allocated by the renderer
   GLBitmapHDS1.MaxPoolSize:=8*1024*1024;

   // specify height map data
   GLBitmapHDS1.Picture.LoadFromFile('terrain.bmp');

   bumpmapHDS:=TGLBumpmapHDS.Create(Self);
   bumpmapHDS.ElevationHDS:=GLBitmapHDS1;
   bumpmapHDS.BumpmapLibrary:=GLMaterialLibrary1;

   // load the texture maps
   GLMaterialLibrary1.Materials[0].Material.Texture.Image.LoadFromFile('detailmap.jpg');
   GLMaterialLibrary1.Materials[1].Material.Texture.Image.LoadFromFile('detailmap.jpg');
   SPSun.Material.Texture.Image.LoadFromFile('flare1.bmp');

   // apply texture map scale (our heightmap size is 256)
   TerrainRenderer1.TilesPerTexture:=1;//256/TerrainRenderer1.TileSize;
   TerrainRenderer1.HeightDataSource:=bumpmapHDS;
   TerrainRenderer1.MaterialLibrary:=GLMaterialLibrary1;

   // Could've been done at design time, but then it hurts the eyes ;)
   GLSceneViewer1.Buffer.BackgroundColor:=clWhite;
   // Initial camera height offset (controled with pageUp/pageDown)
   FCamHeight:=10;
end;

procedure TForm1.GLCadencer1Progress(Sender: TObject; const deltaTime,
  newTime: Double);
var
   speed : Single;
begin
   // handle keypresses
   if IsKeyDown(VK_SHIFT) then
      speed:=5*deltaTime
   else speed:=deltaTime;
   with GLCamera1.Position do begin
      if IsKeyDown(VK_UP) then
         DummyCube1.Translate(-X*speed, 0, -Z*speed);
      if IsKeyDown(VK_DOWN) then
         DummyCube1.Translate(X*speed, 0, Z*speed);
      if IsKeyDown(VK_LEFT) then
         DummyCube1.Translate(-Z*speed, 0, X*speed);
      if IsKeyDown(VK_RIGHT) then
         DummyCube1.Translate(Z*speed, 0, -X*speed);
      if IsKeyDown(VK_PRIOR) then
         FCamHeight:=FCamHeight+10*speed;
      if IsKeyDown(VK_NEXT) then
         FCamHeight:=FCamHeight-10*speed;
      if IsKeyDown(VK_ESCAPE) then Close;
   end;
   // don't drop through terrain!
   with DummyCube1.Position do
      Y:=TerrainRenderer1.InterpolatedHeight(AsVector)+FCamHeight;
end;

// Standard mouse rotation & FPS code below

procedure TForm1.GLSceneViewer1MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
   mx:=x;
   my:=y;
end;

procedure TForm1.GLSceneViewer1MouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
   if ssLeft in Shift then begin
      GLCamera1.MoveAroundTarget((my-y)*0.5, (mx-x)*0.5);
      mx:=x;
      my:=y;
   end;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
   Caption:=GLSceneViewer1.FramesPerSecondText;
   GLSceneViewer1.ResetPerformanceMonitor;
end;

procedure TForm1.FormKeyPress(Sender: TObject; var Key: Char);
begin
   case Key of
      'w', 'W' : with GLMaterialLibrary1.Materials[0].Material.FrontProperties do begin
         if PolygonMode=pmLines then
            PolygonMode:=pmFill
         else PolygonMode:=pmLines;
      end;
      '+' : if GLCamera1.DepthOfView<2000 then begin
         GLCamera1.DepthOfView:=GLCamera1.DepthOfView*1.2;
         with GLSceneViewer1.Buffer.FogEnvironment do begin
            FogEnd:=FogEnd*1.2;
            FogStart:=FogStart*1.2;
         end;
      end;
      '-' : if GLCamera1.DepthOfView>300 then begin
         GLCamera1.DepthOfView:=GLCamera1.DepthOfView/1.2;
         with GLSceneViewer1.Buffer.FogEnvironment do begin
            FogEnd:=FogEnd/1.2;
            FogStart:=FogStart/1.2;
         end;
      end;
      '*' : with TerrainRenderer1 do
         if CLODPrecision>20 then CLODPrecision:=Round(CLODPrecision*0.8);
      '/' : with TerrainRenderer1 do
         if CLODPrecision<1000 then CLODPrecision:=Round(CLODPrecision*1.2);
      '8' : with TerrainRenderer1 do
         if QualityDistance>40 then QualityDistance:=Round(QualityDistance*0.8);
      '9' : with TerrainRenderer1 do
         if QualityDistance<1000 then QualityDistance:=Round(QualityDistance*1.2);
      'l' : with GLLensFlare do Visible:=(not Visible) and SPSun.Visible;
   end;
   Key:=#0;
end;

procedure TForm1.GLSceneViewer1BeforeRender(Sender: TObject);
begin
   GLLensFlare.PreRender(Sender as TGLSceneBuffer);
end;

end.
