{: Basic terrain rendering demo.<p>

   The terrain HeightData is provided by a TGLBitmapHDS (HDS stands for
   "Height Data Source"), and displayed by a TTerrainRenderer.<p>

   The base terrain renderer uses a hybrid ROAM/brute-force approach to
   rendering terrain, by requesting height data tiles, then rendering them
   using either triangle strips (for those below "QualityDistance") or ROAM
   tessellation.<br>
   Note that if the terrain is wrapping in this sample (to reduce the required
   datasets size), the engine is *not* aware of it and does not exploit this
   fact in any way: it considers just an infinite terrain.<p>

   Controls:<ul>
   <li>Direction keys move the came nora (shift to speedup)
   <li>PageUp/PageDown move the camera up and down
   <li>Orient the camera freely by holding down the left button
   <li>Toggle wireframe mode with 'w'
   <li>Increase/decrease the viewing distance with '+'/'-'.
   <li>Increase/decrease CLOD precision with '*' and '/'.
   <li>Increase/decrease QualityDistance with '9' and '8'.
   <li>'n' turns on 'night' mode, 'd' turns back to 'day' mode.
   </ul><p>

   When increasing the range, or moving after having increased the range you
   may notice a one-time slowdown, this originates in the base height data
   being duplicated to create the illusion of an "infinite" terrain (at max
   range the visible area covers 1024x1024 height samples, and with tiles of
   size 16 or less, this is a lot of tiles to prepare).
}
unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  GLScene, GLTerrainRenderer, GLObjects, GLMisc, jpeg, GLHeightData,
  ExtCtrls, GLCadencer, StdCtrls, GLTexture, GLHUDObjects, GLBitmapFont,
  GLSkydome, GLWin32Viewer;

type
  TForm1 = class(TForm)
    GLSceneViewer1: TGLSceneViewer;
    GLBitmapHDS1: TGLBitmapHDS;
    GLScene1: TGLScene;
    GLCamera1: TGLCamera;
    DummyCube1: TDummyCube;
    GLLightSource1: TGLLightSource;
    TerrainRenderer1: TTerrainRenderer;
    Timer1: TTimer;
    GLCadencer1: TGLCadencer;
    GLMaterialLibrary1: TGLMaterialLibrary;
    BitmapFont1: TBitmapFont;
    HUDText1: THUDText;
    SkyDome1: TSkyDome;
    procedure GLSceneViewer1MouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure GLSceneViewer1MouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure Timer1Timer(Sender: TObject);
    procedure GLCadencer1Progress(Sender: TObject; const deltaTime,
      newTime: Double);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
    mx, my : Integer;
    fullScreen : Boolean;
    FCamHeight : Single;
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

uses Keyboard, OpenGL12;

procedure TForm1.FormCreate(Sender: TObject);
begin
   SetCurrentDir(ExtractFilePath(Application.ExeName)+'..\..\media');
   // 8 MB height data cache
   // Note this is the data size in terms of elevation samples, it does not
   // take into account all the data required/allocated by the renderer
   GLBitmapHDS1.MaxPoolSize:=8*1024*1024;
   // specify height map data
   GLBitmapHDS1.Picture.LoadFromFile('terrain.bmp');
   // load the texture maps
   GLMaterialLibrary1.Materials[0].Material.Texture.Image.LoadFromFile('snow512.jpg');
   GLMaterialLibrary1.Materials[1].Material.Texture.Image.LoadFromFile('detailmap.jpg');
   // apply texture map scale (our heightmap size is 256)
   TerrainRenderer1.TilesPerTexture:=256/TerrainRenderer1.TileSize;
   // load Bitmap Font
   BitmapFont1.Glyphs.LoadFromFile('darkgold_font.bmp');
   // Could've been done at design time, but it the, it hurts the eyes ;)
   GLSceneViewer1.Buffer.BackgroundColor:=clWhite;
   // Move camera starting point to an interesting hand-picked location
   DummyCube1.Position.X:=575;
   DummyCube1.Position.Z:=-390;
   DummyCube1.Turn(100);
   // Initial camera height offset (controled with pageUp/pageDown)
   FCamHeight:=0;
end;

procedure TForm1.GLCadencer1Progress(Sender: TObject; const deltaTime,
  newTime: Double);
var
   speed : Single;
begin
   // handle keypresses
   if IsKeyDown(VK_SHIFT) then
      speed:=6*deltaTime
   else speed:=2*deltaTime;
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
      Y:=TerrainRenderer1.InterpolatedHeight(AsVector)/128+FCamHeight;
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
      GLCamera1.MoveAroundTarget(my-y, mx-x);
      mx:=x;
      my:=y;
   end;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
   HUDText1.Text:=Format('%.1f FPS - %d',
                         [GLSceneViewer1.FramesPerSecond, TerrainRenderer1.LastTriangleCount]);
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
      'n', 'N' : with SkyDome1 do if Stars.Count=0 then begin
         // turn on 'night' mode
         Bands[1].StopColor.AsWinColor:=RGB(0, 0, 16);
         Bands[1].StartColor.AsWinColor:=RGB(0, 0, 0);
         Bands[0].StopColor.AsWinColor:=RGB(0, 0, 0);
         Bands[0].StartColor.AsWinColor:=RGB(0, 0, 32);
         Stars.AddRandomStars(700, clWhite);   // many white stars
         Stars.AddRandomStars(100, RGB(255, 200, 200));  // some redish ones
         Stars.AddRandomStars(100, RGB(200, 200, 255));  // some blueish ones
         Stars.AddRandomStars(100, RGB(255, 255, 200));  // some yellowish ones
         GLSceneViewer1.Buffer.BackgroundColor:=RGB(0, 0, 32);
         with GLSceneViewer1.Buffer.FogEnvironment do begin
            FogColor.AsWinColor:=clBlack;
            FogStart:=-FogStart; // Fog is used to make things darker
         end;
      end;
      'd', 'D' : with SkyDome1 do if Stars.Count>0 then begin
         // turn on 'day' mode
         Bands[1].StopColor.Color:=clrNavy;
         Bands[1].StartColor.Color:=clrBlue;
         Bands[0].StopColor.Color:=clrBlue;
         Bands[0].StartColor.Color:=clrWhite;
         Stars.Clear;
         GLSceneViewer1.Buffer.BackgroundColor:=clWhite;
         with GLSceneViewer1.Buffer.FogEnvironment do begin
            FogColor.AsWinColor:=clWhite;
            FogStart:=-FogStart;
         end;
         GLSceneViewer1.Buffer.FogEnvironment.FogStart:=0;
      end;
   end;
   Key:=#0;
end;

end.
