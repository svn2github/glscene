{: Getting a GLODEManager and GLTerrainRenderer talking.<p>

   The GLODETerrainCollider is a custom ODE collider that gets its
   collision information from an assigned TerrainRenderer. Currently
   only boxes and spheres collide with the terrain collider, but more 
   can/will be added in the future.<p>
   
   This demo is a slightly modified terrain demo. The camera controls
   are handled with a TGLNavigator and the sound has been removed. Use 
   the 'b','s' and 'c' keys to drop a boxes, spheres and composite 
   objects (a box and sphere combined) from just in front of the
   camera.<p>
}
unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  GLScene, GLTerrainRenderer, GLObjects, GLMisc, jpeg, GLHeightData,
  ExtCtrls, GLCadencer, StdCtrls, GLTexture, GLHUDObjects, GLBitmapFont,
  GLSkydome, GLWin32Viewer, VectorGeometry, GLLensFlare, GLODEManager,
  GLODECustomColliders, GLNavigator, GLGeomObjects;

type
  TForm1 = class(TForm)
    GLSceneViewer1: TGLSceneViewer;
    GLBitmapHDS1: TGLBitmapHDS;
    GLScene1: TGLScene;
    GLCamera1: TGLCamera;
    TerrainRenderer1: TGLTerrainRenderer;
    Timer1: TTimer;
    GLCadencer1: TGLCadencer;
    GLMaterialLibrary1: TGLMaterialLibrary;
    BitmapFont1: TGLBitmapFont;
    HUDText1: TGLHUDText;
    SkyDome1: TGLSkyDome;
    SPMoon: TGLSprite;
    SPSun: TGLSprite;
    GLLensFlare: TGLLensFlare;
    GLDummyCube1: TGLDummyCube;
    GLODEManager1: TGLODEManager;
    GLNavigator1: TGLNavigator;
    ODEDrop: TGLDummyCube;
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
    mx, my : Integer;

    procedure DropSphere;
    procedure DropBox;
    procedure DropComposite;
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

uses Keyboard, OpenGL1x;

procedure TForm1.FormCreate(Sender: TObject);
begin
   // Set up the visuals

   SetCurrentDir(ExtractFilePath(Application.ExeName)+'..\..\media');
   GLBitmapHDS1.MaxPoolSize:=8*1024*1024;
   GLBitmapHDS1.Picture.LoadFromFile('terrain.bmp');
   GLMaterialLibrary1.Materials[0].Material.Texture.Image.LoadFromFile('snow512.jpg');
   GLMaterialLibrary1.Materials[1].Material.Texture.Image.LoadFromFile('detailmap.jpg');
   SPMoon.Material.Texture.Image.LoadFromFile('moon.bmp');
   SPSun.Material.Texture.Image.LoadFromFile('flare1.bmp');
   TerrainRenderer1.TilesPerTexture:=256/TerrainRenderer1.TileSize;
   BitmapFont1.Glyphs.LoadFromFile('darkgold_font.bmp');
   GLSceneViewer1.Buffer.BackgroundColor:=clWhite;
   GLNavigator1.TurnHorizontal(90);

   // Set up the physics

   // Gravity must be set manually (for now)
   GLODEManager1.Gravity.SetVector(0,0,-9.81);

   // StepFast provides better stability
   GLODEManager1.StepFast:=True;

   // Create the terrain collider
   with TGLODETerrainCollider(GLScene1.Objects.AddNewChild(TGLODETerrainCollider)) do begin
     // Once a manager is assigned the collider gets initialized in ODE
     Manager:=GLODEManager1;
     // The terrain renderer provides the collider with the height and
     // normals required to generate contact joints in ODE
     TerrainRenderer:=TerrainRenderer1;
   end;
end;

procedure TForm1.GLCadencer1Progress(Sender: TObject; const deltaTime,
  newTime: Double);
var
   speed : Single;
begin
   // handle keypresses
   if IsKeyDown(VK_SHIFT) then
      speed:=50*deltaTime
   else speed:=10*deltaTime;
   with GLCamera1.Position do begin
      if IsKeyDown(VK_UP) then
        GLNavigator1.MoveForward(speed);
      if IsKeyDown(VK_DOWN) then
        GLNavigator1.MoveForward(-speed);
      if IsKeyDown(VK_LEFT) then
        GLNavigator1.StrafeHorizontal(-speed);
      if IsKeyDown(VK_RIGHT) then
        GLNavigator1.StrafeHorizontal(speed);
      if IsKeyDown(VK_PRIOR) then
         GLNavigator1.StrafeVertical(speed);
      if IsKeyDown(VK_NEXT) then
         GLNavigator1.StrafeVertical(-speed);
      if IsKeyDown(VK_ESCAPE) then Close;
   end;

   GLODEManager1.Step(deltaTime);
end;

// GLNavigator mouse controls & FPS code below

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
      GLNavigator1.TurnHorizontal(x-mx);
      GLNavigator1.TurnVertical(my-y);
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
         Bands[1].StartColor.AsWinColor:=RGB(0, 0, 8);
         Bands[0].StopColor.AsWinColor:=RGB(0, 0, 8);
         Bands[0].StartColor.AsWinColor:=RGB(0, 0, 0);
         with Stars do begin
            AddRandomStars(700, clWhite, True);   // many white stars
            AddRandomStars(100, RGB(255, 200, 200), True);  // some redish ones
            AddRandomStars(100, RGB(200, 200, 255), True);  // some blueish ones
            AddRandomStars(100, RGB(255, 255, 200), True);  // some yellowish ones
         end;
         GLSceneViewer1.Buffer.BackgroundColor:=clBlack;
         with GLSceneViewer1.Buffer.FogEnvironment do begin
            FogColor.AsWinColor:=clBlack;
            FogStart:=-FogStart; // Fog is used to make things darker
         end;
         SPMoon.Visible:=True;
         SPSun.Visible:=False;
         GLLensFlare.Visible:=False;
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
         SPMoon.Visible:=False;
         SPSun.Visible:=True;
      end;
      't' : with SkyDome1 do begin
         if sdoTwinkle in Options then
            Options:=Options-[sdoTwinkle]
         else Options:=Options+[sdoTwinkle];
      end;
      'l' : with GLLensFlare do Visible:=(not Visible) and SPSun.Visible;
      'b','B' : DropBox;
      's','S' : DropSphere;
      'c','C' : DropComposite;
   end;
   Key:=#0;
end;

procedure TForm1.GLSceneViewer1BeforeRender(Sender: TObject);
begin
   GLLensFlare.PreRender(Sender as TGLSceneBuffer);
end;

// ODE object dropping code

procedure TForm1.DropSphere;
begin
  // Add the dummy object to the scene
  with TGLODEDummy(GLScene1.Objects.AddNewChild(TGLODEDummy)) do begin
    // Initialize it in ODE
    Manager:=GLODEManager1;

    // Set the dummy's position to be just in front of the camera
    Position.AsVector:=ODEDrop.AbsolutePosition;

    // Add a collision element
    with TODEElementSphere(AddNewElement(TODEElementSphere)) do begin
      Radius:=2.5;
      Position.SetToZero;
    end;

    // Allow the element collision boundaries to be seen at runtime
    VisibleAtRunTime:=True;
  end;
end;

procedure TForm1.DropBox;
begin
  // Add the dummy object to the scene
  with TGLODEDummy(GLScene1.Objects.AddNewChild(TGLODEDummy)) do begin
    // Initialize it in ODE
    Manager:=GLODEManager1;

    // Set the dummy's position to be just in front of the camera
    Position.AsVector:=ODEDrop.AbsolutePosition;

    // Add a collision element
    with TODEElementBox(AddNewElement(TODEElementBox)) do begin
      BoxWidth:=5;
      BoxHeight:=5;
      BoxDepth:=5;
      Position.SetToZero;
    end;

    // Allow the element collision boundaries to be seen at runtime
    VisibleAtRunTime:=True;
  end;
end;

procedure TForm1.DropComposite;
begin
  // Add the dummy object to the scene
  with TGLODEDummy(GLScene1.Objects.AddNewChild(TGLODEDummy)) do begin
    // Initialize it in ODE
    Manager:=GLODEManager1;

    // Set the dummy's position to be just in front of the camera
    Position.AsVector:=ODEDrop.AbsolutePosition;

    // Add a collision elements
    with TODEElementBox(AddNewElement(TODEElementBox)) do begin
      BoxWidth:=5;
      BoxHeight:=5;
      BoxDepth:=5;
      Position.SetPoint(2*Random+1,2*Random+1,2*Random+1);
    end;
    with TODEElementSphere(AddNewElement(TODEElementSphere)) do begin
      Radius:=2.5;
      Position.SetPoint(2*Random+1,2*Random+1,2*Random+1);
    end;

    // Calibrating the center of mass moves the elements to get the
    // center of mass aligned with the center of the dummy object
    CalibrateCenterOfMass;

    // Allow the element collision boundaries to be seen at runtime
    VisibleAtRunTime:=True;
  end;
end;

end.
