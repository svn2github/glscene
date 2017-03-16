{*******************************************************}
{                                                       }
{                      Tachyon Unit                     }
{    Vector Raster Geographic Information Synthesis     }
{                     VOICE  ..  Tracer                 }
{                     GRIP ICE .. Tongs                 }
{                Digital Terrain Mapping                }
{               Image Locatable Holographics            }
{                          SOS MAP                      }
{  Surreal Object Synthesis Multimedia Analysis Product }
{                   Fractal3D  Life MOW                 }
{       Copyright (c) 1995,2006  Ivan Lee Herring       }
{                                                       }
{*******************************************************}

{: Actor movement with two cameras (first-person and third-person)<p>

   The movement control is a little "doom-like" and keyboard only.<br>
   This demos mainly answers to "doom-like" movement questions and keyboard
   handling in GLScene.<p>
   The basic principle is to check which key are pressed, and for each movement
   key, multiply the movement by the deltaTime and use this value as delta
   position or angle.<p>
   The frame rate may not be that good on non-T&L accelerated board, mainly due
   to the mushrooms that are light on fillrate needs, but heavy on the polygons.<br>
   This demonstrates how badly viewport object-level clipping is needed in
   GLScene :), a fair share of rendering power is lost in projecting
   objects that are out of the viewing frustum.<p>

   TODO : 3rd person view with quaternion interpolation (smoother mvt)
          More mvt options (duck, jump...)
          Smooth animation transition for TGLActor
          HUD in 1st person view

   Carlos Arteaga Rivero <carteaga@superele.gov.bo>
}
unit GlsTreeFrm;

interface

uses
  Windows,  StdCtrls, Buttons, Controls, ExtCtrls, ComCtrls, Classes, Forms,
  Graphics,  Dialogs, ExtDlgs, SysUtils,

  GLCadencer, GLVectorFileObjects, GLScene, GLObjects,  GLFile3DS, GLMaterial,
  GLSkydome, GLWin32Viewer, GLNavigator, GLTexture, GLHUDObjects,
  GLGeomObjects, GLCoordinates, GLCrossPlatform, GLBaseClasses,
  GLRenderContextInfo, GLFileMD2;

type
  TATreeForm = class(TForm)
    GLScene1: TGLScene;
    GLCamera1: TGLCamera;
    DummyCube1: TGLDummyCube;
    Disk1: TGLDisk;
    GLSceneViewer1: TGLSceneViewer;
    Actor1: TGLActor;
    Actor2: TGLActor;
    GLCadencer1: TGLCadencer;
    Panel1: TPanel;
    Timer1: TTimer;
    GLCamera2: TGLCamera;
    Label3: TLabel;
    Label4: TLabel;
    DummyCube2: TGLDummyCube;
    FreeForm: TGLFreeForm;
    GLLightSource2: TGLLightSource;
    DummyCube3: TGLDummyCube;
    Label1: TLabel;
    SkyDome1: TGLSkyDome;
    GLNavigator1: TGLNavigator;
    GLUserInterface1: TGLUserInterface;
    CBMouseLook: TCheckBox;
    Panel2: TPanel;
    SpriteBtn: TSpeedButton;
    ObjectBtn: TSpeedButton;
    ClearBtn: TSpeedButton;
    CloseBtn: TSpeedButton;
    Tree3dLabel: TLabel;
    TreeSpriteLabel: TLabel;
    GLSprite1: TGLSprite;
    HelpBtn: TSpeedButton;
    TreeImageBtn: TSpeedButton;
    OpenPictureDialog1: TOpenPictureDialog;

    procedure FormCreate(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure HandleKeys(const deltaTime: Double);
    procedure GLCadencer1Progress(Sender: TObject; const deltaTime,
      newTime: Double);
    procedure CBMouseLookClick(Sender: TObject);
    procedure CloseBtnClick(Sender: TObject);
    procedure ClearBtnClick(Sender: TObject);
    procedure ObjectBtnClick(Sender: TObject);
    procedure SpriteBtnClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormShow(Sender: TObject);
    procedure HelpBtnClick(Sender: TObject);
    procedure TreeImageBtnClick(Sender: TObject);
  private
    procedure AddMushrooms(NbMushrooms:Integer);
    procedure AddSprites(Spriter:Integer);
  public
    { Déclarations privées }
     MushTreeCount, SpriteTreeCount:Integer;
  end;

var
  ATreeForm: TATreeForm;



implementation

{$R *.DFM}

uses
  GLVectorGeometry, nUGlobal, Jpeg, GLKeyboard;

const
  cWalkStep = 6;   // this is our walking speed, in 3D units / second
  cStrafeStep = 6; // this is our strafing speed, in 3D units / second
  cRotAngle = 60;  // this is our turning speed, in degrees / second
  cRunBoost = 2;   // speed boost when running
  cSpread = 90;
  cNbMushrooms = 5;

procedure TATreeForm.FormCreate(Sender: TObject);
var
  picName : String;
  AppPath: TFileName;

begin
  Timer1.Enabled := False;
  GLCadencer1.Enabled := False;
  MushTreeCount:=0;
  SpriteTreeCount:=0;
   // Load mushroom mesh
  AppPath := ExtractFileDir(Application.ExeName);
  FreeForm.LoadFromFile(AppPath+'\media\mushroom.3ds');
	picName:=AppPath+'\media\xtree0.bmp';
{	GLMaterialLibrary1.Materials[0]}
  GLSprite1.Material.Texture.Image.LoadFromFile(picName);
  GLSprite1.Material.blendingmode := bmTransparency;
  GLSprite1.Material.Texture.texturemode   := tmReplace;{tmModulate;}
  GLSprite1.Material.Texture.imageAlpha    := tiaSuperBlackTransparent;
 ///GLSprite1.NoZWrite:=True;
 GLSprite1.ObjectsSorting := osRenderBlendedLast;
   // Duplicate our reference mushroom (but not its mesh data !)
   AddMushrooms(1);
   AddSprites(1);
   // Load Actor into GLScene
   Actor1.LoadFromFile(AppPath+'\media\waste.md2');
   Actor1.Material.Texture.Image.LoadFromFile(AppPath+'\media\waste.jpg');
   Actor1.Animations.LoadFromFile(AppPath+'\media\Quake2Animations.aaf');
   Actor1.Scale.SetVector(0.04, 0.04, 0.04, 0);
   // Load weapon model and texture
   Actor2.LoadFromFile(AppPath+'\media\WeaponWaste.md2');
   Actor2.Material.Texture.Image.LoadFromFile(AppPath+'\media\WeaponWaste.jpg');
   Actor2.Animations.Assign(Actor1.Animations);

   // Define animation properties
   Actor1.AnimationMode:=aamLoop;
   Actor1.SwitchToAnimation('stand');
   Actor1.FrameInterpolation:=afpLinear;
   Actor2.Synchronize(Actor1);

   // Load Texture for ground disk
   Disk1.Material.Texture.Image.LoadFromFile(AppPath+'\media\clover.jpg');
end;

procedure TATreeForm.TreeImageBtnClick(Sender: TObject);
begin
  OpenPictureDialog1.InitialDir:=Extractfilepath(Application.exename);
  If OpenPictureDialog1.Execute then
  begin
{	GLMaterialLibrary1.Materials[0].}
  GLSprite1.Material.Texture.Image.LoadFromFile(OpenPictureDialog1.Filename);
  GLSprite1.Material.blendingmode := bmTransparency;
  GLSprite1.Material.Texture.texturemode   := tmReplace;{tmModulate;}
  GLSprite1.Material.Texture.imageAlpha    := tiaSuperBlackTransparent;
 ///GLSprite1.NoZWrite:=True;
 GLSprite1.ObjectsSorting := osRenderBlendedLast;
  end;
end;

procedure TATreeForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Timer1.Enabled := False;
  GLCadencer1.Enabled := False;
{  BirdLanded := True;
  BirdsFormY := AAABirdForm.top;
  BirdsFormX := AAABirdForm.left;}
  if ReallyGone then Action := caFree else Action := caHide;
end;

procedure TATreeForm.FormShow(Sender: TObject);
begin
  Timer1.Enabled := True;
  GLCadencer1.Enabled := True;
end;

procedure TATreeForm.CBMouseLookClick(Sender: TObject);
begin
   GLUserInterface1.MouseLookActive:=CBMouseLook.Checked;
end;

procedure TATreeForm.HandleKeys(const deltaTime: Double);
var
   moving : String;
   boost : Single;
begin
   // This function uses asynchronous keyboard check (see Keyboard.pas)
   if IsKeyDown(VK_ESCAPE) then Close;
   if IsKeyDown('A') then begin
      CBMouseLook.Checked:=True;
      CBMouseLookClick(Self);
   end;
   if IsKeyDown('D') then begin
      CBMouseLook.Checked:=False;
      CBMouseLookClick(Self);
   end;

   //Change Cameras
   if IsKeyDown(VK_F7) then begin
      GLSceneViewer1.Camera:=GLCamera1;
{      Actor1.Visible:=True;}
      Label4.Font.Style:=Label4.Font.Style-[fsBold];
      Label3.Font.Style:=Label3.Font.Style+[fsBold];
   end;
   if IsKeyDown(VK_F8) then begin
      GLSceneViewer1.Camera:=GLCamera2;
      Actor1.Visible:=False;
      Label4.Font.Style:=Label4.Font.Style+[fsBold];
      Label3.Font.Style:=Label3.Font.Style-[fsBold];
   end;

   // Move Actor in the scene

   // if nothing specified, we are standing
   moving:='stand';

   // first, are we running ? if yes give animation & speed a boost
   if IsKeyDown(VK_SHIFT) then begin
      Actor1.Interval:=100;
      boost:=cRunBoost*deltaTime
   end else begin
      Actor1.Interval:=150;
      boost:=deltaTime;
   end;
   Actor2.Interval:=Actor1.Interval;

   // are we advaning/backpedaling ?
   if IsKeyDown(VK_UP) then begin
      GLNavigator1.MoveForward(cWalkStep*boost);
      moving:='run';
   end;
   if IsKeyDown(VK_DOWN) then begin
      GLNavigator1.MoveForward(-cWalkStep*boost);
      moving:='run';
   end;

   // slightly more complex, depending on CTRL key, we either turn or strafe
   if IsKeyDown(VK_LEFT) then begin
      if IsKeyDown(VK_CONTROL) then
          GLNavigator1.StrafeHorizontal(-cStrafeStep*boost)
      else GLNavigator1.TurnHorizontal(-cRotAngle*boost);
      moving:='run';
   end;
   if IsKeyDown(VK_RIGHT) then begin
      if IsKeyDown(VK_CONTROL) then
          GLNavigator1.StrafeHorizontal(cStrafeStep*boost)
      else GLNavigator1.TurnHorizontal(cRotAngle*boost);
      moving:='run';
   end;

   // update animation (if required)
   // you can use faster methods (such as storing the last value of "moving")
   // but this ones shows off the brand new "CurrentAnimation" function :)
{   if Actor1.CurrentAnimation<>moving then begin
      Actor1.SwitchToAnimation(moving);
      Actor2.Synchronize(Actor1);
   end;}
end;

procedure TATreeForm.GLCadencer1Progress(Sender: TObject; const deltaTime,
  newTime: Double);
begin
   HandleKeys(deltaTime);
   GLUserInterface1.Mouselook;

   GLSceneViewer1.Invalidate;
   GLUserInterface1.MouseUpdate;
end;

// add a few mushrooms to make the "landscape"
procedure TATreeForm.AddMushrooms(NbMushrooms:Integer);
var
   i : Integer;
   proxy : TGLProxyObject;
   s : TVector;
   f : Single;
begin
   // spawn some more mushrooms using proxy objects
   for i:=0 to {cNbMushrooms}NbMushrooms-1 do begin
      // create a new proxy and set its MasterObject property
      proxy:=TGLProxyObject(DummyCube1.AddNewChild(TGLProxyObject));
      with proxy do begin
         ProxyOptions:=[pooObjects];
         MasterObject:=FreeForm;
         Visible:=True;
         // retrieve reference attitude
         Direction:=FreeForm.Direction;
         Up:=FreeForm.Up;
         // randomize scale
         s:=FreeForm.Scale.AsVector;
         f:=(1*Random+1);
         ScaleVector(s, f);
         Scale.AsVector:=s;
         // randomize position
         Position.SetPoint(Random(cSpread)-(cSpread/2),
                           FreeForm.Position.z+0.8*f,
                           Random(cSpread)-(cSpread/2));
         // randomize orientation
         RollAngle:=Random(360);
         TransformationChanged;
      end;
   end;
   inc(MushTreeCount,NbMushrooms);
   Tree3dLabel.Caption:=Inttostr(MushTreeCount) + ' 3ds Trees';
end;

procedure TATreeForm.Timer1Timer(Sender: TObject);
begin
   Caption:=Format('%.2f FPS', [GLSceneViewer1.FramesPerSecond]);
   GLSceneViewer1.ResetPerformanceMonitor;
end;

procedure TATreeForm.CloseBtnClick(Sender: TObject);
begin
  Close;
end;

procedure TATreeForm.ClearBtnClick(Sender: TObject);
var i:Integer; 
begin
  {Erase the Proxies}
  {DummyCube1.DeleteChildren;}
  for i:=MushTreeCount-1 downto 2 do
  begin
  DummyCube1.Children[i].Visible:=False;
  DummyCube1.Children[i].Free;
  end;
  MushTreeCount:=0;
  SpriteTreeCount:=0;
  Tree3dLabel.Caption:=Inttostr(MushTreeCount) + ' 3ds Trees';
  TreeSpriteLabel.Caption:=Inttostr(SpriteTreeCount) + ' Sprite Trees';
     { proxy:=TGLProxyObject(DummyCube1.AddNewChild(TGLProxyObject));}
end;

procedure TATreeForm.ObjectBtnClick(Sender: TObject);
begin
  AddMushrooms(5);
end;

procedure TATreeForm.SpriteBtnClick(Sender: TObject);
begin
  AddSprites(5);
end;
procedure TATreeForm.AddSprites(Spriter:Integer);
var
	i : Integer;
   s : TVector;
   f : Single;
	spr : TGLSprite;
begin
	// Load texture for sprite2, this is the hand-coded way using a PersistentImage
	// Sprite1 uses a PicFileImage, and so the image is automagically loaded by
	 cene when necessary (no code is required).
	// (Had I used two PicFileImage, I would have avoided this code)
{	picName:='..\..\media\flare1.bmp';
	GLMaterialLibrary1.Materials[0].Material.Texture.Image.LoadFromFile(picName);}
	// New sprites are created by duplicating the template "sprite2"
	for i:=1 to Spriter do begin
		spr:=TGLSprite(DummyCube1.AddNewChild(TGLSprite));
		spr.Assign(GLSprite1);
     with spr do
     begin
         Visible:=True;
         s:=GLSprite1.Scale.AsVector;
         f:=(1*Random+1);
         ScaleVector(s, f);
         Scale.AsVector:=s;
         // randomize position
         Position.SetPoint(Random(cSpread)-(cSpread/2),
                           GLSprite1.Position.z+0.8*f,
                           Random(cSpread)-(cSpread/2));
         // sprite size change
         SetSquareSize(2+cos(3*f));
         TransformationChanged;
     end;
	end;
   inc(SpriteTreeCount,Spriter);
   TreeSpriteLabel.Caption:=Inttostr(SpriteTreeCount) + ' Sprite Trees';
end;
{Compression
FilteringQuality
ImageClassName
MagFilter
MappingMode
MinFilter
TextureFormat
TextureMode
TextureWrap}




procedure TATreeForm.HelpBtnClick(Sender: TObject);
begin
  Application.HelpContext(9900);
end;



end.
