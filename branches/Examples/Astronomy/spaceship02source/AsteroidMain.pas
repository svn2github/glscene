unit AsteroidMain;
{
History:
========
- January 2004: Updated GLScene
- May to June 2003: Creation
}

interface

uses
  Winapi.Windows, Winapi.Messages, Winapi.MMSystem,
  System.SysUtils,
  System.Classes,
  System.INIFiles,
  System.Math,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.Imaging.jpeg,
  Vcl.ExtCtrls,
  Vcl.StdCtrls,

   
  GLCadencer, GLScene,
  GLGeomObjects,
  OpenGL1x,
  GLMesh,
  GLVectorGeometry,
  GLMaterial,
  ahGLSObjects, GLTexture, GLWin32Viewer, GLKeyboard, GLRenderContextInfo,
  GLVectorFileObjects, GLFireFX,
  GLFileTGA,
  GLCollision,
  GLCanvas,
  OpenGLTokens,
  GLObjects,
  GLJoystick,
  GLCoordinates,
  GLCrossPlatform,
  GLBaseClasses;

type
  tParticleCloud=class(tComponent)
  private
    P		:integer;
    Idle		:tList;
    Active	:tList;
    function GetParticles(index: integer): TGLDummyCube;
    procedure SetCloseDistance(const Value: single);
    procedure SetFarDistance(const Value: single);
    procedure SetTouchDistance(const Value: single);
    procedure SetClose(const index:integer);
    procedure SetFar(const index:integer);
    procedure SetTouch(const index:integer);
  protected
    fMax				:integer;
    fFar,fClose,fTouch	:single;
    MasterFar			:TGLFreeForm;  // All three masters should come with their OnProgress event
    MasterClose		:TGLFreeForm;
    procedure Appear(const x,y,z,vecx,vecy,vecz:single);
    procedure Disappear(const index:integer);
    procedure GenerateAsteroid(Asteroid:TGLDummyCube);
  public
    Density		:integer; // Number of particles desired
    Owner		:TGLBaseSceneObject;
    property CloseDistance:single read FClose write SetCloseDistance;
    constructor Create(const AOwner:TGLBaseSceneObject; const ffClose, ffFar: TGLFreeForm;
                       const aDensity,aMax: integer;
                       const CloseAngle,FarAngle:single);
    destructor Destroy;override;
    property FarDistance:single read FFar write SetFarDistance;
    property Particles[index:integer]:TGLDummyCube read GetParticles;
    property TouchDistance:single read FTouch write SetTouchDistance;
    procedure Update(const x,y,z,vecx,vecy,vecz: single; N: integer);
  end;//class

  tShip=class(tRealMovingCamera)
  private
  public
    Acceleration		:single;
    LargeReactorPlaying	:boolean;
    SmallReactorPlaying	:boolean;
    Mass				:single;
    BoosterPower		:single;
    JetPower			:single;
    Camera			:TGLCamera;
    Size				:single;
    Fuel				:single;
    Temperature		:single;
    Consumption		:single;
    Heat				:single;
    Cooling			:single;
    JetFuel			:single;
    JetConsumption		:single;
  end;

  fProcessInput=procedure(const DeltaTime:double) of object;

  TfAsteroidField = class(TForm)
    GLCadencer1: TGLCadencer;
    GLScene1: TGLScene;
    camPilot: TGLCamera;
    lsSun: TGLLightSource;
    dumWorld: TGLDummyCube;
    GLSceneViewer1: TGLSceneViewer;
    mlMaterials: TGLMaterialLibrary;
    dumShip: TGLDummyCube;
    oglStarDome: TGLDirectOpenGL;
    oglDust: TGLDirectOpenGL;
    oglSpeedVector: TGLDirectOpenGL;
    TimerLOD: TTimer;
    dumTrajectory: TGLDummyCube;
    oglReticle: TGLDirectOpenGL;
    objExplosion: TGLSphere;
    Hull: TGLFrustrum;
    dumSpaceShip: TGLDummyCube;
    camExternal: TGLCamera;
    StbdReactor: TGLCylinder;
    BotReactor: TGLFrustrum;
    oglDetector: TGLDirectOpenGL;
    lsReactor: TGLLightSource;
    dumMasters: TGLDummyCube;
    colAsteroid: TGLCollisionManager;
    dumCollisionAsteroid: TGLDummyCube;
    ffxExplosion: TGLFireFXManager;
    ReactorFlame: TGLCylinder;
    prtfwdFlame: TGLProxyObject;
    prtbakFlame: TGLProxyObject;
    stbfwdFlame: TGLProxyObject;
    stbbakFlame: TGLProxyObject;
    topfwdFlame: TGLProxyObject;
    topbakFlame: TGLProxyObject;
    Joystick1: TGLJoystick;
    TimerInput: TTimer;
    dumStation: TGLDummyCube;
    Center: TGLCylinder;
    Annulus: TGLAnnulus;
    testAnnulus: TGLAnnulus;
    Arm1: TGLCylinder;
    procedure FormCreate(Sender: TObject);
    procedure GLCadencer1Progress(Sender: TObject; const deltaTime,
      newTime: Double);
    procedure oglStarDomeRender(var rci: TGLRenderContextInfo);
    procedure oglDustRender(var rci: TGLRenderContextInfo);
    procedure oglSpeedVectorRender(var rci: TGLRenderContextInfo);
    procedure TimerLODTimer(Sender: TObject);
    procedure oglReticleRender(var rci: TGLRenderContextInfo);
    procedure FormActivate(Sender: TObject);
    procedure oglDetectorRender(var rci: TGLRenderContextInfo);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure colAsteroidCollision(Sender: TObject; object1,
      object2: TGLBaseSceneObject);
    procedure TimerInputTimer(Sender: TObject);
    procedure GLSceneViewer1PostRender(Sender: TObject);
  private
    JetStream	:boolean;
    ProcessInput:fProcessInput;
    procedure GameOver;
    procedure CheckBoosterFailure(const Power:single);
    procedure FireForwardReactor;
    procedure FireBackwardReactor;
    procedure FireDownwardReactor;
    procedure FireJetStream(const MinDelta:single);
    procedure ShutdownReactors;
    procedure ForwardThrust(const Power:single);
    procedure BackwardThrust(const Power:single);
    procedure UpwardThrust(const Power:single);
    procedure JetRoll(const Power:single);
    procedure JetPitch(const Power:single);
    procedure JetTurn(const Power:single);
    procedure View(const Direction:integer);
    procedure TranslateX(const Power:single);
    procedure TranslateY(const Power:single);
    procedure TranslateZ(const Power:single);
    procedure NoThrust;
    procedure CollisionDetection(const DeltaTime:double);
    procedure ProcessKeyboard(const DeltaTime:double);
    procedure ProcessJoystick(const DeltaTime:double);
    procedure ProcessNothing(const DeltaTime:double);
    procedure AsteroidOnProgress(Sender: TObject; const deltaTime,
      newTime: Double);
  public
    MasterAsteroidC	:TGLFreeForm;
    MasterAsteroidF	:TGLFreeForm;
    MasterDust		:TGLDirectOpenGL;
    Asteroids			:tParticleCloud;
    Ship				:tShip;
    ShipRotation		:single;
  end;

var
  fAsteroidField		:TfAsteroidField;
  ViewRadius		:integer=100000;
  AsteroidInView	:integer=200;  // Number of simultaneous asteroids
  CloseAngle		:single=4;     // Minimum angle of view to be seen in full details
  FarAngle		:single=2;     // Minimum angle of view to appear

implementation

{$R *.dfm}

var
  SimulationTime	:double;
  AsteroidSize	:double;

procedure TfAsteroidField.FormCreate(Sender: TObject);
var
  Temp	:TMeshObject;
  Seed	:integer;
  IniF	:TIniFile;
begin
  SetCurrentDir(ExtractFilePath(ParamStr(0)));
  IniF:=TIniFile.Create(ExtractFilePath(ParamStr(0))+'SpaceShip.ini');
  try
    ViewRadius:=IniF.ReadInteger('Environment','ViewRadius',100000);
    AsteroidInView:=IniF.ReadInteger('Environment','Asteroids',200);  // Number of simultaneous asteroids
    CloseAngle:=IniF.ReadFloat('Environment','CloseAngle',4);     // Minimum angle of view to be seen in full details
    FarAngle:=IniF.ReadFloat('Environment','FarAngle',2);     // Minimum angle of view to appear

    {Joystick connected?}
    try
      if Joystick1.Capture=True then begin
        //ShowMessage('Joystick detected.');
        ProcessInput:=ProcessJoystick;
        end
      else begin
        ProcessInput:=ProcessKeyboard;
      end;
    except
      ProcessInput:=ProcessKeyboard;
    end;

    {Load textures}
    mlMaterials.Materials[0].Material.Texture.Image.LoadFromFile('Floor.jpg');
    mlMaterials.Materials[2].Material.Texture.Image.LoadFromFile('txShip.jpg');
    mlMaterials.Materials[5].Material.Texture.Image.LoadFromFile('Flamme.tga');
    mlMaterials.Materials[7].Material.Texture.Image.LoadFromFile('StationCentre.jpg');

    {Init asteroids}
    Randomize;
    Seed:=RandSeed;
    MasterAsteroidC:=TGLFreeForm(dumCollisionAsteroid.AddNewChild(TGLFreeForm));
    Temp:=TMeshObject.CreateOwned(MasterAsteroidC.MeshObjects);
    BuildPotatoid(Temp,0.5,2);
    MasterAsteroidC.Material.MaterialLibrary:=fAsteroidField.mlMaterials;
    MasterAsteroidC.Material.LibMaterialName:='CloseAsteroid';
    dumCollisionAsteroid.OnProgress:=AsteroidOnProgress;
    MasterAsteroidC.BuildOctree;
    //MasterAsteroidC.ShowAxes:=True;

    RandSeed:=Seed; // Use the same seed to generate the same shape at a lesser resolution
    MasterAsteroidF:=TGLFreeForm(dumMasters.AddNewChild(TGLFreeForm));
    Temp:=TMeshObject.CreateOwned(MasterAsteroidF.MeshObjects);
    BuildPotatoid(Temp,0.5,0,2);
    MasterAsteroidF.Material.MaterialLibrary:=fAsteroidField.mlMaterials;
    MasterAsteroidF.Material.LibMaterialName:='FarAsteroid';

    MasterDust:=TGLDirectOpenGL(dumMasters.AddNewChild(TGLDirectOpenGL));

    AsteroidSize:=1000; //(ViewRadius*Power(FractionOccupied/AsteroidInView,1/3));
    Asteroids:=tParticleCloud.Create(dumWorld,MasterAsteroidC,MasterAsteroidF,
                                     AsteroidInView,AsteroidInView,
                                     CloseAngle,FarAngle);
    MasterAsteroidF.Visible:=False;
    MasterAsteroidC.Visible:=False;
    MasterDust.Visible:=False;

    {Init Ship}
    Ship:=tShip.Create;
    with Ship do begin
      Camera:=camPilot;
      Camera.DepthOfView:=ViewRadius;
      Mass:=IniF.ReadFloat('Ship','Mass',2000);
      InN:=2000;
      InU:=InN*IniF.ReadFloat('Ship','PitchInertia',1);
      InV:=InN*IniF.ReadFloat('Ship','YawInertia',1);
      BoosterPower:=IniF.ReadFloat('Ship','BoosterPower',40000);
      Acceleration:=BoosterPower/Mass;
      GLobject:=dumShip;
      Size:=20;
      dumShip.Position.SetPoint(x,y,z);
      Fuel:=100;
      Temperature:=0;
      Consumption:=IniF.ReadFloat('Ship','FuelConsumption',0.04); // % of fuel used per second of thrust
      Heat:=IniF.ReadFloat('Ship','Heating',0.4);        // % of heating per second of thrust
      Cooling:=IniF.ReadFloat('Ship','Cooling',1.01);    // Speed of booster cooling
      JetFuel:=100;
      JetConsumption:=IniF.ReadFloat('Ship','JetConsumption',0.04);
    end;//with

    {Init station}
    with dumStation do begin
      Position.AsVector:=VectorMake((random*2-1)*170000,
                                    (random*2-1)*170000,
                                    (random*2-1)*170000);
      Direction.SetVector(random*2-1,random*2-1,random*2-1);
      ShipRotation:=random*50;
      {Position.AsVector:=VectorMake(0,0,200);
      ShipRotation:=0;
      Direction.SetVector(0,1,0);
      Up.SetVector(0,0,1);}
    end;//with   }
  finally
    IniF.Free;
  end;//finally
end;

procedure TfAsteroidField.GLCadencer1Progress(Sender: TObject; const deltaTime,
  newTime: Double);
var
  s			:double;
  Size		:single;
  Detector	:TVector;
  Noise		:single;
begin
  {Update ship}
  with Ship do begin
    UpdateAll(DeltaTime); // Update position and attitude of the ship
    ComputeSpeed;
    if Speed<0.1 then begin
      sx:=0; sy:=0; sz:=0;
    end;//if
    if Speed>1000 then begin
      s:=1000/Speed;
      sx:=sx*s;
      sy:=sy*s;
      sz:=sz*s;
    end;//if
    s:=1/Speed;
    with dumTrajectory do begin
      Position.X:=x;
      Position.Y:=y;
      Position.Z:=z;
      Up:=DumShip.Up;
      Direction.SetVector(sx*s,sy*s,sz*s);
    end;//with
    with oglDetector do begin
      Position.X:=x;
      Position.Y:=y;
      Position.Z:=z;
      Up:=DumShip.Up;
      Detector.X:=dumStation.Position.X-x;
      Detector.Y:=dumStation.Position.y-y;
      Detector.Z:=dumStation.Position.z-z;
      Noise:= GLVectorGeometry.Log2(1+VectorLength(Detector));
      Direction.AsVector:=Detector;
      Scale.Z:=500;
      Scale.x:=sqr(Noise/1.5);
      Scale.y:=Scale.x;
    end;//with
    with dumShip do begin
      Position.X:=x+Vibx;
      Position.Y:=y+Viby;
      Position.Z:=z+Vibz;
      Direction.SetVector(nx,ny,nz);
      Up.SetVector(vx,vy,vz);
      Vibx:=0;
      Viby:=0;
      Vibz:=0;
    end;//with

    oglStarDome.Position:=dumShip.Position;

    {Update asteroids}
    try Asteroids.Update(x,y,z,sx,sy,sz,5); except end;
  end;//with

  {Collision}
  CollisionDetection(DeltaTime);

  dumStation.TurnAngle:=ShipRotation*NewTime;

  SimulationTime:=NewTime;
  //if DeltaTime<0.04 then Sleep(Round(20-DeltaTime*0.001));
end;

procedure TfAsteroidField.FireForwardReactor;
begin
  PlaySound('LargeReactor.Wav',0,SND_ASYNC);
  with lsReactor.Diffuse do begin Red:=0.2; Green:=0.5; Blue:=1; end;//with
  prtbakFlame.Visible:=False;
  stbbakFlame.Visible:=False;
  topbakFlame.Visible:=False;
  prtfwdFlame.Visible:=True;
  stbfwdFlame.Visible:=True;
  topfwdFlame.Visible:=True;
end;

procedure TfAsteroidField.FireBackwardReactor;
begin
  PlaySound('LargeReactor.Wav',0,SND_ASYNC);
  with lsReactor.Diffuse do begin Red:=0.2; Green:=0.5; Blue:=1; end;//with
  prtbakFlame.Visible:=True;
  stbbakFlame.Visible:=True;
  topbakFlame.Visible:=True;
  prtfwdFlame.Visible:=False;
  stbfwdFlame.Visible:=False;
  topfwdFlame.Visible:=False;
end;

procedure TfAsteroidField.ShutDownReactors;
begin
  with lsReactor.Diffuse do begin Red:=0; Green:=0; Blue:=0; end;//with
  ReactorFlame.Height:=0;
  prtbakFlame.Visible:=False;
  stbbakFlame.Visible:=False;
  topbakFlame.Visible:=False;
  prtfwdFlame.Visible:=False;
  stbfwdFlame.Visible:=False;
  topfwdFlame.Visible:=False;
end;

procedure TfAsteroidField.ProcessKeyboard(const DeltaTime: double);
var
  acc     :single; // Acceleration
  Reactor :boolean;
begin
  if IsKeyDown(VK_SHIFT) then acc:=2*Ship.Acceleration else acc:=Ship.Acceleration;
  acc:=acc*DeltaTime;
  Reactor:=False;
  with Ship do begin
    {Reactors}
    if IsKeyDown(ord(' ')) then ForwardThrust(-acc/1)
    else if IsKeyDown(ord('S')) then BackwardThrust(-acc/5)
    else NoThrust;
    if IsKeyDown(ord(VK_CONTROL)) then UpwardThrust(acc/10);

    {Attitude}
    if IsKeyDown(ord('W'))     then TranslateZ(-acc/40);
    if IsKeyDown(VK_UP)        then JetPitch(+acc)
    else if IsKeyDown(VK_DOWN) then JetPitch(-acc);
    if IsKeyDown(VK_RIGHT)     then JetTurn(-acc)
    else if IsKeyDown(VK_LEFT) then JetTurn(+acc);
    if IsKeyDown(ord('A'))     then JetRoll(+acc)
    else if IsKeyDown(ord('D'))then JetRoll(-acc);

    {View}
    if IsKeyDown(VK_NEXT)        then View(2)
    else if IsKeyDown(VK_DELETE) then View(6)
    else if IsKeyDown(VK_HOME)   then View(0)
    else if IsKeyDown(VK_END)    then View(4);

    if IsKeyDown(ord('X')) then begin
      with objExplosion do begin
        if not Visible then begin
          Visible:=True;
          Position.SetPoint(0,0,-50);
          Scale.SetVector(1,1,1);
          TagFloat:=SimulationTime; // Tagfloat contains explosion start time
        end;//if
      end;//with }
      with ffxExplosion do begin
        particlesize:=1;
        IsotropicExplosion(3,5,1);
        Disabled:=False;
      end;//with
      PlaySound('Explosn.Wav',0,SND_ASYNC);
    end;

    FireJetStream(acc);

    if IsKeyDown(VK_ESCAPE)then GameOver;
  end;//with   }
end;

procedure TfAsteroidField.ProcessNothing(const DeltaTime: double);
begin
  if IsKeyDown(VK_ESCAPE)then GameOver;
  if VectorLength(VectorSubtract(dumShip.AbsolutePosition,dumStation.AbsolutePosition))<10 then begin
    Ship.sx:=0;
    Ship.sy:=0;
    Ship.sz:=0;
  end;//if
end;

procedure TfAsteroidField.oglStarDomeRender(var rci: TGLRenderContextInfo);
begin
  glPushAttrib(GL_ALL_ATTRIB_BITS);
    glPushMatrix;
      glTexEnvf(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_DECAL);
      glDisable(GL_LIGHTING);
      glDisable(GL_BLEND);
      glScalef(ViewRadius*0.9,ViewRadius*0.9,ViewRadius*0.9);  // Make it large enough so as not passing before other objects
      BuildStarField(500);
    glPopMatrix;
  glPopAttrib;
end;

procedure TfAsteroidField.oglDustRender(var rci: TGLRenderContextInfo);
var
  grey	:glFloat;
  i		:integer;
begin
  glPushAttrib(GL_ALL_ATTRIB_BITS);
    glDisable(GL_LIGHTING);
    glTexEnvf(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_DECAL);
    glPointSize(1);
    glBegin(GL_POINTS);
      for i:=1 to 1000 do begin
        grey:=random*0.5+0.2;
        glColor3f(grey,grey,grey);
        glVertex3f((random*2-1)*ViewRadius,(random*2-1)*ViewRadius,(random*2-1)*ViewRadius);
      end;//for
    glEnd;
  glPopAttrib;
end;

procedure TfAsteroidField.oglSpeedVectorRender(var rci: TGLRenderContextInfo);
const
  sep=30;
  h=10;
var
  i		:integer;
  ii	:single;
  t		:double;
  e		:single;
begin
  with Ship do begin
    if Abs(Speed)<=0.15 then exit;
    //e:=1/Power(10,Trunc(log10(Speed/10)));
    e:=1;
    try
      glPushAttrib(GL_ALL_ATTRIB_BITS);
      glTexEnvf(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_BLEND);
      glDisable(GL_LIGHTING);
      glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
      glEnable(GL_BLEND);
      t:=SimulationTime*e;
      glColor4f(1,0,0,0.5);
      glBegin(GL_LINES);
        glVertex3f(-sep,-h,0);
        glVertex3f(-sep,-h,-Speed*3600);
        glVertex3f(+sep,-h,0);
        glVertex3f(+sep,-h,-Speed*3600);
      glEnd;
      for i:=0 to 10 do begin
        ii:=(i+(t-Trunc(t)))/e;
        glBegin(GL_LINES);
          glVertex3f(-sep,-h,-ii*Speed);
          glVertex3f(+sep,-h,-ii*Speed);
        glEnd;
      end;//for
      glColor4f(0,1,0,0.5);
      glBegin(GL_LINES);
        glVertex3f(-sep,-h,0);
        glVertex3f(-sep,-h,Speed*3600);
        glVertex3f(+sep,-h,0);
        glVertex3f(+sep,-h,Speed*3600);
      glEnd;
      for i:=1 to 12 do begin
        ii:=(i-(t-Trunc(t)))/e;
        //glRotate(-rn*logi/5,0,0,1);
        glBegin(GL_LINES);
          glVertex3f(-sep,-h,ii*Speed);
          glVertex3f(+sep,-h,ii*Speed);
        glEnd;
      end;//for
    finally
      glPopAttrib;
    end;//finally
  end;//with     }
end;

procedure TfAsteroidField.oglDetectorRender(var rci: TGLRenderContextInfo);
var
  x	:single;
begin
  glPushAttrib(GL_ALL_ATTRIB_BITS);
    glDisable(GL_LIGHTING);
    glTexEnvf(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_BLEND);
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
    glEnable(GL_BLEND);
    glDisable(GL_DEPTH_TEST);
    glColor4f(1,0,0,0.7);
    x:=Frac(SimulationTime/2)+1;
    glBegin(GL_LINES);
      glVertex3f(-x,-x,1); glVertex3f( x,-x,1);
      glVertex3f( x,-x,1); glVertex3f( x, x,1);
      glVertex3f( x, x,1); glVertex3f(-x, x,1);
      glVertex3f(-x, x,1); glVertex3f(-x,-x,1);
    glEnd;
  glPopAttrib;
end;

procedure TfAsteroidField.TimerLODTimer(Sender: TObject);
begin
//  lblDebug.Caption:=Format('Ship:(%.f, %.f, %.f) Speed:%f',
//  [Ship.x,Ship.y,Ship.z,Ship.Speed]);
  Caption:=Format('FPS:%.f',[GLSceneViewer1.FramesPerSecond]);
  with oglDust do begin
    if VectorLength(position.x-Ship.x,Position.y-Ship.y,Position.z-Ship.z)
       >ViewRadius/2 then begin
      Position.x:=Ship.x;
      Position.y:=Ship.y;
      Position.z:=Ship.z;
    end;//if
  end;//with
end;

procedure TfAsteroidField.oglReticleRender(var rci: TGLRenderContextInfo);
begin
  glPushAttrib(GL_ALL_ATTRIB_BITS);
    glDisable(GL_LIGHTING);
    glTexEnvf(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_BLEND);
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
    glEnable(GL_BLEND);
    glColor4f(1,1,0,0.7);
    BuildCross(0.03);
  glPopAttrib;
end;

procedure TfAsteroidField.CollisionDetection(const DeltaTime: double);
var
  rayStart, rayVector, iPoint, iNormal : TVector;
  i			:integer;
  Dist,Dist2	:double;
begin
  if Ship.Speed=0 then exit;

  {Asteroid}
  Dist:=(Ship.Speed*DeltaTime+Ship.Size/2); // Radius of potential collision
  Dist2:=sqr(Dist);
  if dumCollisionAsteroid.Visible then begin
    with dumCollisionAsteroid do begin
      if sqr(Position.X-Ship.x)+
        sqr(Position.Y-Ship.y)+
        sqr(Position.z-Ship.z)
        <= sqr((Dist+Children[0].TagFloat)) // Tagfloat contains the asteroid's radius
      then begin
        with Ship do begin
          SetVector(RayStart,dumShip.AbsolutePosition);
          SetVector(RayVector,sx,sy,sz);
          NormalizeVector(RayVector);
        end;//with
        if TGLFreeForm(Children[0]).OctreeRayCastIntersect(rayStart, rayVector, @iPoint, @iNormal) then begin
          if sqr(iPoint.X-Ship.x)+
            sqr(iPoint.Y-Ship.y)+
            sqr(iPoint.Z-Ship.z) <= Dist2
          then begin
            with objExplosion do begin
              if not Visible then begin
                Visible:=True;
                Position.AsVector:=iPoint;
                TagFloat:=SimulationTime; // Tagfloat contains explosion start time
              end;//if
            end;//with }
            with ffxExplosion do begin
              IsotropicExplosion(3,5,1);
              Disabled:=False;
            end;//with
            PlaySound('Explosn.Wav',0,SND_ASYNC);
            with Ship do begin
              rn:=random*180;
              ru:=random*180;
              rv:=random*180;
              sx:=-sx; sy:=-sy; sz:=-sz;
            end;//with  }
          end;//if
        end;//if
      end;//if
    end;//with
  end;//if

  {Spatial station}
  with dumStation do begin
    if sqr(Position.X-Ship.x)+
      sqr(Position.Y-Ship.y)+
      sqr(Position.z-Ship.z)
      <= sqr((Dist+Annulus.TopRadius))
    then begin
      with Ship do begin
        SetVector(RayStart,dumShip.AbsolutePosition);
        SetVector(RayVector,sx,sy,sz);
        NormalizeVector(RayVector);
      end;//with
      for i:=0 to dumStation.Count-1 do begin
        if TGLSceneObject(Children[i]).RayCastIntersect(rayStart, rayVector, @iPoint, @iNormal) then begin
          if sqr(iPoint.X-Ship.x)+
            sqr(iPoint.Y-Ship.y)+
            sqr(iPoint.Z-Ship.z) <= Dist2
          then begin
            if Children[i].Name='Center' then begin
              if (Ship.Speed<10)and
                 (VectorLength(VectorSubtract(Children[i].AbsolutePosition,iPoint))
                 <=TGLCylinder(Children[i]).Height/2*1.2)and
                 (Abs(VectorDotProduct(dumStation.Up.AsAffineVector,dumShip.Direction.AsAffineVector))>0.8)and
                 (Abs(VectorDotProduct(dumStation.Direction.AsAffineVector,dumShip.Up.AsAffineVector))>0.8)
              then begin
                dumShip.RollAngle:=TurnAngle;
                dumShip.Direction.AsAffineVector:=(Direction.AsAffineVector);
                dumShip.Up.AsAffineVector:=(Up.AsAffineVector);
                Ship.sx:=0;//dumShip.Direction.z;
                Ship.sy:=0;//dumShip.Direction.x;
                Ship.sz:=0;//dumShip.Direction.y;
                PlaySound('Open.Wav',0,SND_ASYNC);
                camPilot.Position.Z:=800;
                camPilot.Position.Y:=800;
                camPilot.Position.Y:=800;
                camPilot.TargetObject:=dumShip;
                oglDetector.Visible:=False;
                oglSpeedVector.Visible:=False;
                ProcessInput:=ProcessNothing;
                Break;
                end//if
              else begin
                {Ship.sx:=0;
                Ship.sy:=0;
                Ship.sz:=0;
                break;  }
              end;//else
            end;//if
            with objExplosion do begin
              if not Visible then begin
                Visible:=True;
                Position.AsVector:=iPoint;
                TagFloat:=SimulationTime; // Tagfloat contains explosion start time
              end;//if
            end;//with }
            with ffxExplosion do begin
              IsotropicExplosion(3,5,1);
              Disabled:=False;
            end;//with
            PlaySound('Explosn.Wav',0,SND_ASYNC);
            with Ship do begin
              rn:=random*180;
              ru:=random*180;
              rv:=random*180;
              sx:=-sx; sy:=-sy; sz:=-sz;
              //sx:=0; sy:=0; sz:=0;
            end;//with
            Break;
          end;//if in range
        end;//if Intersect
      end;//for i
    end;//if detection needed
  end;//with
end;

procedure TfAsteroidField.FormActivate(Sender: TObject);
begin
  Asteroids.Update(0,0,0,0,0,0,Asteroids.Density);
end;

{ tParticleCloud }

procedure tParticleCloud.Appear(const x, y, z, vecx, vecy, vecz: single);
var
  i,N		:integer;
  th,ph		:single;
  ct,st,cp,sp	:single;
  ax,ay,az	:single;
  sx1,sy1,sz1	:single;
  S			:single;
  Dist		:single;
begin
  if fAsteroidField.Ship.Speed<1e-3 then exit;
  S:=1/fAsteroidField.Ship.Speed;
  sx1:=vecx*s;
  sy1:=vecy*s;
  sz1:=vecz*s;
  N:=Density-Active.Count;
  if N<=0 then exit;
  if N>Idle.Count then N:=Idle.Count;
  for i:=0 to N-1 do begin
    Active.Add(Idle[i]);
    Idle[i]:=nil;
    with TGLDummyCube(Active[Active.Count-1]) do begin
      GenerateAsteroid(TGLDummyCube(Active[Active.Count-1]));
      Dist:=Children[0].TagFloat/fFar;
      th:=random*2*pi;
      ph:=random*pi*0.5;
      SinCosine(th,st,ct);
      SinCosine(ph,sp,cp);
      ax:=Dist*ct*cp;
      ay:=Dist*st*cp;
      az:=Dist*sp;
      with fAsteroidField.Ship do begin
        Position.X:=x- ax*sx1*sy1				-ay*sz1  	+az*sx1;
        Position.Y:=y+ ax*(sqr(sz1)+sqr(sx1))	+ay*0  	+az*sy1;
        Position.Z:=z- ax*sz1*sy1				+ay*sx1	+az*sz1;
      end;// with
      Up.SetVector(random,random,random); // Rotation axis
      TagFloat:=random*90;                // Rotation speed
      Visible:=True;
      Children[0].Visible:=False;
      Children[1].Visible:=True;  // Set to far view
    end;//with
  end;//for i
  Idle.Pack;
end;

constructor tParticleCloud.Create(const AOwner:TGLBaseSceneObject; const ffClose, ffFar: TGLFreeForm;
  const aDensity,aMax: integer; const CloseAngle,FarAngle:single);
var
  i		:integer;
  F,C	:TGLProxyObject;
  th,ph		:single;
  ct,st,cp,sp	:single;
  Dist		:single;
begin
  inherited Create(AOwner);
  Owner:=AOwner;
  fMax:=aMax;
  fTouch:=TouchDistance;
  fClose:=Tangent(DegToRadian(CloseAngle/2));
  fFar:=Tangent(DegToRadian(FarAngle/2));
  MasterClose:=ffClose;
  MasterFar:=ffFar;
  Density:=aDensity;
  Idle:=TList.Create;
  Active:=TList.Create;
  Idle.Capacity:=fMax;
  Active.Capacity:=fMax;

  {Create all particles}
  Idle.Clear;
  for i:=0 to fMax-1 do begin
    Idle.Add(Owner.AddNewChild(TGLDummyCube)); // Create DummyCube containing the two views
    with TGLDummyCube(Idle[i]) do begin
      OnProgress:=TGLDummyCube(MasterClose.Owner).OnProgress;
      C:=TGLProxyObject(AddNewChild(TGLProxyObject)); // Create close view
      C.MasterObject:=MasterClose;           // Proxy from supplied TGLFreeForm
      C.OnProgress:=nil;
      F:=TGLProxyObject(AddNewChild(TGLProxyObject)); // Create far view
      F.MasterObject:=MasterFar;           // Proxy from supplied TGLFreeForm
      F.OnProgress:=nil;
      Visible:=False;
    end;//with
  end;//for i

  {Activate some of them}
  Active.Clear;
  for i:=Density-1 downto 0 do begin
    Active.Add(Idle[i]);
    Idle[i]:=nil;
    with TGLDummyCube(Active[Active.Count-1]) do begin
      GenerateAsteroid(TGLDummyCube(Active[Active.Count-1]));
      th:=random*2*pi;
      ph:=random*pi-pi/2;
      SinCosine(th,st,ct);
      SinCosine(ph,sp,cp);
      Dist:=(random*0.7+0.3)*Children[0].TagFloat/fFar+Children[0].TagFloat;
      Position.X:=ct*cp*Dist;
      Position.Y:=st*cp*Dist;
      Position.Z:=sp*Dist;
      Up.SetVector(random,random,random);
      TagFloat:=random*90; // Speed of rotation
      Visible:=True;
      Children[0].Visible:=False;
      Children[1].Visible:=True;  // By default set all to far view
    end;//with
  end;//for i
  Idle.Pack;
end;

destructor tParticleCloud.Destroy;
begin
  Active.Free;
  Idle.Free;
  inherited;
end;

procedure tParticleCloud.Disappear(const index: integer);
begin
  with TGLDummyCube(Active[index]) do begin
    Children[0].Visible:=False;
    Children[1].Visible:=False;
    Visible:=False;
  end;//with
  Idle.Add(Active[index]);
  Active.Delete(index);
end;

procedure tParticleCloud.GenerateAsteroid(Asteroid: TGLDummyCube);
var
  S			:single;
begin
  with Asteroid do begin
    S:=(RandG(AsteroidSize,AsteroidSize/2));
    Scale.SetVector(S,RandG(S,S/2),RandG(S,S/2));
    Children[0].TagFloat:=MaxFloat(Scale.x,Scale.y,Scale.z); // Radius of the bounding sphere
  end;//with
end;

function tParticleCloud.GetParticles(index: integer): TGLDummyCube;
begin
  Result:=TGLDummyCube(Active[index]);
end;

procedure tParticleCloud.SetClose(const index: integer);
begin
  with TGLDummyCube(Active[index]) do begin
    Children[0].Visible:=True;
    Children[1].Visible:=False;
  end;//with
end;

procedure tParticleCloud.SetCloseDistance(const Value: single);
begin
  FClose := Value;
end;

procedure tParticleCloud.SetFar(const index: integer);
begin
  with TGLDummyCube(Active[index]) do begin
    Children[0].Visible:=False;
    Children[1].Visible:=True;
  end;//with
end;

procedure tParticleCloud.SetFarDistance(const Value: single);
begin
  FFar := Value;
end;

procedure tParticleCloud.SetTouch(const index: integer);
begin
  with TGLDummyCube(Active[index]) do begin
    Children[0].Visible:=True;
    Children[1].Visible:=False;
    fAsteroidField.dumCollisionAsteroid.Position:=Position;
    //fAsteroidField.dumCollisionAsteroid.Direction:=Direction;
    fAsteroidField.dumCollisionAsteroid.Up:=Up;
    fAsteroidField.dumCollisionAsteroid.Scale:=Scale;
    fAsteroidField.dumCollisionAsteroid.Children[0].Visible:=True;
    fAsteroidField.dumCollisionAsteroid.Children[0].TagFloat:=Children[0].TagFloat;
    fAsteroidField.dumCollisionAsteroid.TagFloat:=TagFloat;
    fAsteroidField.dumCollisionAsteroid.Visible:=True;
  end;//with
end;

procedure tParticleCloud.SetTouchDistance(const Value: single);
begin
  FTouch := Value;
end;

procedure tParticleCloud.Update(const x,y,z,vecx,vecy,vecz: single; N: integer);
var
  i		:integer;
  Dist	:single;
  Aspect	:single;  // Angle of view
  Closest:integer;
  MinDist:single;
begin
  if N>Active.Count then N:=Active.Count;
  if P>Active.Count-1 then P:=Active.Count-1;
  with fAsteroidField.dumCollisionAsteroid
  do MinDist:=VectorLength(Position.X-x,Position.Y-y,Position.Z-z);
  Closest:=-1;
  for i:=1 to N do begin
  try
    with TGLDummyCube(Active.Items[P]) do begin
      Dist:=VectorLength(Position.X-x,Position.Y-y,Position.Z-z);
      if Dist<MinDist then begin
        Closest:=P;
        MinDist:=Dist;
      end;//if
      Aspect:=Children[0].TagFloat/Dist;
      if Aspect>=fClose then SetClose(P)
      else if Aspect>=fFar then SetFar(P)
      else Disappear(P);
      Dec(P);
      if P<0 then P:=Active.Count+P;
    end;//with
    except end;
  end;//for
  if Closest<>-1 then with TGLDummyCube(Active.Items[Closest]) do begin
    {if MinDist<Children[0].TagFloat*3 then} SetTouch(Closest);
    {end//if
  else if MinDist>fAsteroidField.dumCollisionAsteroid.Children[0].TagFloat*3 then begin
    with fAsteroidField.dumCollisionAsteroid do begin
      Visible:=False;
    end;//with }
  end;//else
  if Active.Count<Density then try Appear(x,y,z,vecx,vecy,vecz) except end;
end;

procedure TfAsteroidField.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  GameOver;
end;

procedure TfAsteroidField.AsteroidOnProgress(Sender: TObject; const deltaTime,
  newTime: Double);
begin
  with TGLDummyCube(Sender) do TurnAngle:=TagFloat*NewTime;
end;

procedure TfAsteroidField.colAsteroidCollision(Sender: TObject; object1,
  object2: TGLBaseSceneObject);
begin
  if ((object1=dumShip) and (object2=dumCollisionAsteroid)) or
     ((object2=dumShip) and (object1=dumCollisionAsteroid))
  then begin
    with objExplosion do begin
      if not Visible then begin
        Visible:=True;
        Position:=dumShip.Position;
        Scale.SetVector(1,1,1);
        TagFloat:=SimulationTime; // Tagfloat contains explosion start time
        PlaySound('Explosn.Wav',0,SND_ASYNC);
        with Ship do begin
          rn:=random*180;
          ru:=random*180;
          rv:=random*180;
          sx:=-sx; sy:=-sy; sz:=-sz;
        end;//with
      end;//if
    end;//with
  end;//if
end;

procedure TfAsteroidField.BackwardThrust(const Power: single);
begin
  if Ship.Fuel<=0 then exit;
  Ship.Accelerate(0,0,-Power);
  ReactorFlame.Height:=ReactorFlame.Height+0.04;
  ReactorFlame.TopRadius:=random*0.03;
  prtfwdFlame.TurnAngle:=random*360;
  stbfwdFlame.TurnAngle:=random*360;
  topfwdFlame.TurnAngle:=random*360;
  if not prtfwdFlame.Visible then FireForwardReactor;
  if ReactorFlame.Height>1.5 then ReactorFlame.Height:=1.5;
  CheckBoosterFailure(Power);
end;

procedure TfAsteroidField.ForwardThrust(const Power: single);
begin
  if Ship.Fuel<=0 then exit;
  Ship.Accelerate(0,0,Power);
  ReactorFlame.Height:=ReactorFlame.Height+0.1;
  ReactorFlame.TopRadius:=random*0.1;
  prtbakFlame.TurnAngle:=random*360;
  stbbakFlame.TurnAngle:=random*360;
  topbakFlame.TurnAngle:=random*360;
  if not prtbakFlame.Visible then FireBackwardReactor;
  if ReactorFlame.Height>1 then ReactorFlame.Height:=1;
  CheckBoosterFailure(Power);
end;

procedure TfAsteroidField.JetPitch(const Power: single);
var
  i	:integer;
begin
  if Ship.JetFuel<=0 then exit;
  //Ship.ru:=Ship.ru+Power;
  //for i:=1 to 1 do
  Ship.GyroPitch(Power*10);
  JetStream:=True;
  with Ship do JetFuel:=JetFuel-JetConsumption*abs(Power);
end;

procedure TfAsteroidField.JetRoll(const Power: single);
var
  i	:integer;
begin
  if Ship.JetFuel<=0 then exit;
  //Ship.rn:=Ship.rn+Power;
  //for i:=1 to 1 do
  Ship.GyroRoll(Power*10);
  with Ship do JetFuel:=JetFuel-JetConsumption*abs(Power);
  JetStream:=True;
end;

procedure TfAsteroidField.JetTurn(const Power: single);
var
  i	:integer;
begin
  if Ship.JetFuel<=0 then exit;
  //Ship.rv:=Ship.rv+Power;
  //for i:=1 to 1 do
  Ship.GyroYaw(Power*10);
  with Ship do JetFuel:=JetFuel-JetConsumption*abs(Power);
  JetStream:=True;
end;

procedure TfAsteroidField.TranslateX(const Power: single);
begin
  Ship.Accelerate(Power,0,0);
  JetStream:=True;
end;

procedure TfAsteroidField.TranslateY(const Power: single);
begin
  Ship.Accelerate(0,Power,0);
  JetStream:=True;
end;

procedure TfAsteroidField.TranslateZ(const Power: single);
begin
  Ship.Accelerate(0,0,Power);
  JetStream:=True;
end;

procedure TfAsteroidField.View(const Direction: integer);
begin
  case Direction of
    0: camPilot.Direction.SetVector(0,0,-1);
    1: camPilot.Direction.SetVector(1,0,-1);
    2: camPilot.Direction.SetVector(1,0,0);
    3: camPilot.Direction.SetVector(1,0,1);
    4: camPilot.Direction.SetVector(0,0,1);
    5: camPilot.Direction.SetVector(-1,0,1);
    6: camPilot.Direction.SetVector(-1,0,0);
    7: camPilot.Direction.SetVector(-1,0,-1);
  end;//case
end;

procedure TfAsteroidField.NoThrust;
begin
  ReactorFlame.Height:=ReactorFlame.Height-0.3;
  if ReactorFlame.Height<=0 then ShutDownReactors;
  with Ship do Temperature:=Temperature/Cooling;
end;

procedure TfAsteroidField.FireDownwardReactor;
begin
  PlaySound('LargeReactor.Wav',0,SND_ASYNC);
  with lsReactor.Diffuse do begin Red:=0.5; Green:=0.1; Blue:=0; end;//with
end;

procedure TfAsteroidField.UpwardThrust(const Power: single);
begin
  if Ship.Fuel<=0 then exit;
  Ship.Accelerate(0,Power,0);
  FireDownwardReactor;
  CheckBoosterFailure(Power);
end;

procedure TfAsteroidField.FireJetStream(const MinDelta:single);
begin
  if JetStream then begin
    if abs(Ship.ru)<MinDelta then Ship.ru:=0;
    if abs(Ship.rv)<MinDelta then Ship.rv:=0;
    if abs(Ship.rn)<MinDelta then Ship.rn:=0;
    if not Ship.SmallReactorPlaying then begin
      PlaySound('SmallReactor.Wav',0,SND_ASYNC);
      Ship.SmallReactorPlaying:=True;
    end;//if
  end;
  JetStream:=False;
end;

procedure TfAsteroidField.ProcessJoystick(const DeltaTime:double);
var
  x,y,z,r	:integer;
  acc		:single;
begin
  acc:=Ship.Acceleration;
  acc:=acc*DeltaTime/5;
  with Joystick1 do
  begin
  (*  GetPosition;
    x:=Round(XPosition/10);
    y:=Round(YPosition/10);
    z:=Round(ZPosition/10);
    r:=Round(RPosition/10);
   *)
 {   if abs(x)=1 then x:=0;
    if abs(y)=1 then y:=0;
    if abs(z)=1 then z:=0;
    if abs(r)=1 then r:=0;

    {Reactors}
    if (jbButton3 in JoyButtons)and(z<>0) then begin
      if z<0 then ForwardThrust(z*acc/1)
      else BackwardThrust(-z*acc/5);
      end//if
    else NoThrust;
    if (jbButton4 in JoyButtons) then begin
      UpwardThrust(acc/5);
    end;//if

    {Jetstreams}
    if (jbButton2 in JoyButtons) then begin // Translation
      if x<>0 then TranslateX(-x*acc/40);
      if y<>0 then TranslateZ( y*acc/40);
      if r<>0 then TranslateY(-r*acc/40);
      end
    else begin // Rotation
      if x<>0 then JetTurn(-x*acc/2);
      if y<>0 then JetPitch(-y*acc/2);
      if r<>0 then JetRoll(-r*acc/4);
    end;//if
    if Abs(x)+Abs(y)+Abs(r)>0
    then FireJetStream(acc)
    else Ship.SmallReactorPlaying:=False;

    {Point of View}
    (*
    if PointOfView>=0 then begin
      View(Trunc(PointOfView/45));
    end;//if
    *)
  end;//with

  if IsKeyDown(VK_ESCAPE)then GameOver;
end;


procedure TfAsteroidField.TimerInputTimer(Sender: TObject);
begin
  ProcessInput(TimerInput.Interval/1000); // Call Keyboard or Joystick if up-and-running
end;

procedure TfAsteroidField.GLSceneViewer1PostRender(Sender: TObject);
const
  x0=10; // Offset
  wc=20; // Column width
  sp=5;  // Separation between columns
var
  y0	:integer; // Offset
  i		:integer;
  glc    :TGLCanvas;
begin
  glc:=TGLCanvas.Create(GLSceneViewer1.ClientWidth,GLSceneViewer1.ClientHeight);
  y0:=GLSceneViewer1.ClientHeight-10;
  try
    with glc do begin
      PenWidth:=1;
      PenAlpha:=0.5;
      with Ship do begin
        {Fuel}
        if Fuel<=10 then PenColor:=clRed else PenColor:=clGreen;
        for i:=1 to Round(Fuel/2) do begin
          Line(x0,y0-i*2,x0+wc-sp,y0-i*2);
        end;//for
        PenColor:=clDkGray;
        for i:=i to 50 do Line(x0,y0-i*2,x0+wc-sp,y0-i*2);

        {Temperature}
        if Temperature>=80 then PenColor:=clRed else PenColor:=clBlue;
        for i:=1 to Round(Temperature/2) do begin
          Line(x0+1*wc,y0-i*2,x0+1*wc+wc-sp,y0-i*2);
        end;//for
        PenColor:=clDkGray;
        for i:=i to 50 do Line(x0+1*wc,y0-i*2,x0+1*wc+wc-sp,y0-i*2);

        {Jet Fuel}
        if JetFuel<=10 then PenColor:=clRed else PenColor:=clAqua;
        for i:=1 to Round(JetFuel/2) do begin
          Line(x0+2*wc,y0-i*2,x0+2*wc+wc-sp,y0-i*2);
        end;//for
        PenColor:=clDkGray;
        for i:=i to 50 do Line(x0+2*wc,y0-i*2,x0+2*wc+wc-sp,y0-i*2);
      end;//with
    end;//with
  finally
    glc.Free;
  end;//finally
end;

procedure TfAsteroidField.CheckBoosterFailure(const Power:single);
begin
  with Ship do begin
    Fuel:=Fuel-Abs(Power)*Consumption;
    Temperature:=Temperature+Abs(Power)*Heat;
    if random<(Temperature-90)/1000 then Fuel:=0;
  end;//with
end;

procedure TfAsteroidField.GameOver;
begin
  WinExec(PAnsiChar(ExtractFilePath(ParamStr(0))+'SpaceShip.exe'),SW_NORMAL);
  Application.Terminate;
end;

end.
