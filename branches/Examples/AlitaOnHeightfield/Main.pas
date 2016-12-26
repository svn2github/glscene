unit Main;

interface

uses
  Winapi.OpenGL,
  Winapi.Windows,
//  Winapi.Messages,
  System.SysUtils,
  System.Classes, System.Math,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Vcl.Imaging.jpeg, Vcl.ExtCtrls, Vcl.StdCtrls, Vcl.ExtDlgs,
  Vcl.Buttons, Vcl.ComCtrls,
  // GLS
  GLScene,
  GLObjects,
  GLCadencer,
  GLVectorTypes,
  GLVectorFileObjects,
  GLKeyboard,
  GLGraph,
  GLVectorGeometry,
  GLFireFX,
  GLSkydome,
  GLTexture,
  GLMaterial,
  GLCoordinates,
  GLCrossPlatform,
  GLBaseClasses,
  GLWin32Viewer,
  GLFileMD2;

type
  TForm1 = class(TForm)
    Panel1: TPanel;
    GLCadencer1: TGLCadencer;
    TimeLabel: TLabel;
    CBAnimations: TComboBox;
    DistanceBar: TTrackBar;
    SBPlay: TSpeedButton;
    SBStop: TSpeedButton;
    SBFrameToFrame: TSpeedButton;
    GLSceneViewer1: TGLSceneViewer;
    GLScene1: TGLScene;
    GLLightSource1: TGLLightSource;
    GLCamera1: TGLCamera;
    Actor1: TGLActor;
    ActorCube: TGLDummyCube;
    Image1: TImage;
    Image2: TImage;
    HeightField1: TGLHeightField;
    AllObjects: TGLDummyCube;
    CameraCube: TGLDummyCube;
    GLFireFXManager1: TGLFireFXManager;
    SkyDome1: TGLSkyDome;
    AfterSky: TGLDummyCube;
    FogBox: TCheckBox;
    Label1: TLabel;
    Label2: TLabel;
    Memo1: TMemo;
    Plane1: TGLPlane;
    Sphere2: TGLSphere;
    GLMaterialLibrary1: TGLMaterialLibrary;
    Timer1: TTimer;
    OpenPictureDialog1: TOpenPictureDialog;
    procedure HandleKeys(const deltaTime: Double);
    procedure GLCadencer1Progress(Sender: TObject;
      const deltaTime, newTime: Double);
    procedure FormCreate(Sender: TObject);
    procedure SBPlayClick(Sender: TObject);
    procedure SBStopClick(Sender: TObject);
    procedure SBFrameToFrameClick(Sender: TObject);
    procedure CBAnimationsChange(Sender: TObject);
    procedure GLSceneViewer1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure GLSceneViewer1MouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure DistanceBarChange(Sender: TObject);
    procedure FormMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure ActorDo(action: string; perform: Boolean);
    procedure FogBoxClick(Sender: TObject);
    procedure Image2DblClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure Image1DblClick(Sender: TObject);
    procedure HeightField1GetHeight(const X, Y: Single; var z: Single;
      var Color: TVector4f; var TexPoint: TTexPoint);
  private
    { Private declarations }
  public
    { Public declarations }
    snowyMapActive: Boolean;
  end;

var
  Form1: TForm1;
  mx, my, mx2, my2: Integer;

implementation

{$R *.DFM}

procedure TForm1.FormCreate(Sender: TObject);
begin
  SetCurrentDir(ExtractFilePath(Application.ExeName));
  // ----Load the main Actor and skin----
  Actor1.LoadFromFile('.\alita\tris.md2');
  Actor1.Material.Texture.Image.LoadFromFile('.\alita\alita2.jpg');
  Actor1.Scale.SetVector(0.05, 0.05, 0.05, 0);
  Actor1.Animations.SetToStrings(CBAnimations.Items);
  CBAnimations.ItemIndex := 0;
  CBAnimationsChange(Self);

  Image1.Picture.LoadFromFile('terrain.bmp');
  Image2.Picture.LoadFromFile('snow512.jpg');
  snowyMapActive := True;

  HeightField1.Material.Texture.Image.assign(Image2.Picture.Graphic);
  HeightField1.Material.Texture.Disabled := False;

  // ----The Heightmap image can double as cloud-cover
  // ----It is tiled onto a large, transparent plane-object in the sky
  // ----(Its a cheap trick, I know, but it gives a good result with very little effort)
  with GLMaterialLibrary1.Materials[0].Material do
    Texture.Image.assign(Image1.Picture);
end;

procedure TForm1.HandleKeys(const deltaTime: Double);
const
  cTurnSpeed = 100;
  cMoveSpeed = 9;
var
  hgt: Single;
  HV: THomogeneousFltVector;
  tp: TTexPoint;
  xpos, zpos: Single;
begin
  if IsKeyDown(VK_LEFT) then
    ActorCube.Turn(-cTurnSpeed * deltaTime); // rotate actor left
  if IsKeyDown(VK_RIGHT) then
    ActorCube.Turn(cTurnSpeed * deltaTime); // rotate actor right

  if IsKeyDown(VK_UP) or IsKeyDown(VK_DOWN) then
  begin
    if IsKeyDown(VK_UP) then
      ActorCube.Move(cMoveSpeed * deltaTime)
    else
      ActorCube.Move(-cMoveSpeed * deltaTime);
    xpos := -(ActorCube.position.X / HeightField1.Scale.X);
    // calc actors position on heightfield
    zpos := (ActorCube.position.z / HeightField1.Scale.Y);
    HeightField1GetHeight(xpos, zpos, hgt, HV, tp);
    // get hgt ---this is the same procedure used to get the heightfieds heights
    ActorCube.position.Y := (hgt * HeightField1.Scale.z) +
      (HeightField1.position.Y) + 1.2; // place actor just above heightfield
    CameraCube.position := ActorCube.position; // move camera to actors position
    ActorDo('run0', True); // play 'run0' animation
  end
  else
    ActorDo('run0', False); // stop playing 'run0' animation

  // ----   Move camera around the target.
  // ----   This code is placed here so the camera-movements would not cause the Actor to pause
  if ((mx <> mx2) or (my <> my2)) then
  begin
    GLCamera1.MoveAroundTarget(my - my2, mx - mx2);
    // move the camera around the target if mouse was dragged
    mx := mx2;
    my := my2;
  end;

end;

procedure TForm1.ActorDo(action: string; perform: Boolean);
begin
  if ((action <> Actor1.CurrentAnimation) and (perform = True)) then
    Actor1.SwitchToAnimation(action);
  if ((action = Actor1.CurrentAnimation) and (perform = False)) then
    Actor1.SwitchToAnimation('stand0');
end;

procedure TForm1.GLCadencer1Progress(Sender: TObject;
  const deltaTime, newTime: Double);
begin
  HandleKeys(deltaTime);
  TimeLabel.caption := IntToStr(Round(deltaTime * 10000));
  with GLMaterialLibrary1.Materials[0].TextureOffset do
    AddScaledVector(deltaTime * 0.05, XVector);
end;

procedure TForm1.SBPlayClick(Sender: TObject);
begin
  Actor1.AnimationMode := aamLoop;
  SBPlay.Enabled := False;
  SBStop.Enabled := True;
  SBFrameToFrame.Enabled := False;
end;

procedure TForm1.SBStopClick(Sender: TObject);
begin
  Actor1.AnimationMode := aamNone;
  SBPlay.Enabled := True;
  SBStop.Enabled := False;
  SBFrameToFrame.Enabled := True;
end;

procedure TForm1.SBFrameToFrameClick(Sender: TObject);
begin
  Actor1.NextFrame;
end;

procedure TForm1.CBAnimationsChange(Sender: TObject);
begin
  Actor1.SwitchToAnimation(CBAnimations.Text);
end;

procedure TForm1.GLSceneViewer1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  mx := X;
  my := Y;
  mx2 := X;
  my2 := Y;
end;

procedure TForm1.GLSceneViewer1MouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
begin
  if ssLeft in Shift then
  begin
    mx2 := X;
    my2 := Y;
  end;
end;

procedure TForm1.DistanceBarChange(Sender: TObject);
var
  Dist, NewDist, cx, cy, cz: Single;
begin
  Dist := GLCamera1.DistanceToTarget;
  cx := GLCamera1.position.X;
  cy := GLCamera1.position.Y;
  cz := GLCamera1.position.z;
  NewDist := DistanceBar.position;
  GLCamera1.position.X := cx / Dist * NewDist;
  GLCamera1.position.Y := cy / Dist * NewDist;
  GLCamera1.position.z := cz / Dist * NewDist;
end;

procedure TForm1.HeightField1GetHeight(const X, Y: Single; var z: Single;
  var Color: TVector4f; var TexPoint: TTexPoint);
var
  val: Integer;
  xi, yi: Integer;
begin
  xi := Round(X * (Image1.Picture.Width - 1));
  // translate heightfield coordinate to Image1 pixel number
  yi := Round((1 - Y) * (Image1.Picture.Height - 1));
  val := (Image1.Picture.Bitmap.Canvas.Pixels[xi, yi]) AND $000000FF;
  // use brightness of heightmap pixel to calculate height of point x,y
  z := val * 0.05; // return the height of the landscape in z  at position x,y
end;

procedure TForm1.FormMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  GLCamera1.AdjustDistanceToTarget(Power(1.1, WheelDelta / 120));
  // Adjust Camera distance with mousewheel
end;

procedure TForm1.FogBoxClick(Sender: TObject);
begin
  GLSceneViewer1.Buffer.FogEnable := FogBox.Checked; // enable or disable fog
  Plane1.Visible := not FogBox.Checked; // remove clouds when fog is on
end;

procedure TForm1.Image2DblClick(Sender: TObject);
begin
  if OpenPictureDialog1.Execute then
    Image2.Picture.LoadFromFile(OpenPictureDialog1.FileName);
  HeightField1.Material.Texture.Image.assign(Image2.Picture.Graphic);
end;

procedure TForm1.Image1DblClick(Sender: TObject);
begin
  if OpenPictureDialog1.Execute then
    Image1.Picture.LoadFromFile(OpenPictureDialog1.FileName);
  HeightField1.StructureChanged;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  caption := Format('%.1f FPS', [GLSceneViewer1.FramesPerSecond]);
  GLSceneViewer1.ResetPerformanceMonitor;
end;

end.
