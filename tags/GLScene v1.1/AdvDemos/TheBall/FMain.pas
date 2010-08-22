{: A simple ODE-based game of a ball rolling on a plane you can incline.<p>

   Under construction, missing:
   - main screen and time/score charts
   - levels and levels ordering
   - additionnal structures

   Eric Grange (egrange@glscene.org)
   http://glscene.org
}
unit FMain;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, GLScene,
  GLObjects, GLShadowPlane, GLWin32Viewer, GLTexture, GLMirror,
  odeimport, dynodegl, GLCadencer, ExtCtrls, Jpeg, VectorGeometry, GLSkydome,
  GLBitmapFont, GLWindowsFont, GLHUDObjects, StdCtrls, UTheBallStructures,
  GLParticleFX, GLKeyBoard, GLSound, GLSMBASS, Dialogs, GLGeomObjects, GLMaterial,
  GLCoordinates, GLCrossPlatform, BaseClasses, GLColor, GLFileWAV;

type
   TGameStatus = (gsLevelPreview, gsWarmup, gsPlaying, gsLevelWon, gsLevelLost); 

  TMain = class(TForm)
    Scene: TGLScene;
    SceneViewer: TGLSceneViewer;
    Camera: TGLCamera;
    DCTable: TGLDummyCube;
    SPTable: TGLShadowPlane;
    LSWorld: TGLLightSource;
    SPHBall: TGLSphere;
    MaterialLibrary: TGLMaterialLibrary;
    Mirror: TGLMirror;
    DCObstacles: TGLDummyCube;
    Cadencer: TGLCadencer;
    Timer: TTimer;
    DCBallAbsolute: TGLDummyCube;
    DCBallLag: TGLDummyCube;
    SkyDome: TGLSkyDome;
    SPRBallFlare: TGLSprite;
    HUDText: TGLHUDText;
    HUDText2: TGLHUDText;
    WindowsBitmapFont: TGLWindowsBitmapFont;
    DCMap: TGLDummyCube;
    HTTimer: TGLHUDText;
    PFXExit: TGLPointLightPFXManager;
    PFXRenderer: TGLParticleFXRenderer;
    HSTimerBkGnd: TGLHUDSprite;
    PFXFire: TGLPointLightPFXManager;
    ALStart: TGLArrowLine;
    DCMapTransparent: TGLDummyCube;
    GLSMBASS: TGLSMBASS;
    SoundLibrary: TGLSoundLibrary;
    PFXSteam: TGLPolygonPFXManager;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure CadencerProgress(Sender: TObject; const deltaTime,
      newTime: Double);
    procedure TimerTimer(Sender: TObject);
    procedure SceneViewerMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormResize(Sender: TObject);
    procedure SceneViewerMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure MirrorBeginRenderingMirrors(Sender: TObject);
    procedure MirrorEndRenderingMirrors(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    world : PdxWorld;
    space : PdxSpace;
    contactGroup, jointGroup : TdJointGroupID;
    tablePitchSpring, tableRollSpring, tableBall : TdJointID;
    levelWidth, levelDepth : Single;

    planeGeom : PdxGeom;
    planeBox : PdxGeom;

    ballGeom : PdxGeom;
    ballBody : PdxBody;

    mouseDownX, mouseDownY : Single;
    tablePitch, tableRoll : Single;
    isMouseDown : Boolean;

    currentLevelIdx : Integer;
    currentLevelStrucs : TTheBallStructures;
    currentLevelName : String;
    gameStatus : TGameStatus;

    deflateEnergy : Single;
    deflateVector : TAffineVector;
    verticalForce : Single;

    burnOut : Single;

    spawnTime, gameEndTime : TDateTime;
    cameraRefPos : TVector;

    procedure LoadLevel(const fileName : String);
    procedure ClearCurrentLevel;
    function  CurrentLevelSpecial(aSpecial : TTheBallStructureClass) : TTheBallStructure;
    procedure LevelWarmup;
    procedure SpawnBall;
    function  LevelWon : Boolean;
    function  LevelLost(const msg : String) : Boolean;
    procedure AdjustTable;
  end;

var
  Main: TMain;

implementation

{$R *.dfm}

uses GLUtils;

const
   cCameraPos : TVector = (10, 6, 0, 1);

procedure TMain.FormCreate(Sender: TObject);
begin
   InitODE('');
   
   currentLevelStrucs:=TTheBallStructures.Create;

   // create world
   world:=dWorldCreate;
   space:=dHashSpaceCreate(nil);
   contactGroup:=dJointGroupCreate(0);

   // initialize world
   dWorldSetGravity(world, 0, 0, 0);
   dWorldSetCFM(world, 1e-3);
   dWorldSetERP(world, 0.4);

   planeGeom:=dCreateGeomTransform(space);
   dGeomTransformSetCleanup(planeGeom, 1);

   planeBox:=dCreateBox(nil, 11, 4, 11);
   dGeomTransformSetGeom(planeGeom, planeBox);
   dGeomSetPosition(planeBox, 0, -2, 0);

   CopyPosFromGeomToGL(planeGeom, DCTable);

   if dBoxClass=-1 then
      dBoxClass:=dGeomGetClass(planeGeom);

   currentLevelIdx:=1;
   LoadLevel(ExtractFilePath(Application.ExeName)+Format('Level%.2d.txt', [currentLevelIdx]));
end;

procedure TMain.FormDestroy(Sender: TObject);
begin
   ClearCurrentLevel;

   dJointGroupDestroy(contactgroup);
   dSpaceDestroy(space);
   dWorldDestroy(world);

   currentLevelStrucs.Free;
   
   CloseODE;
end;

procedure TMain.ClearCurrentLevel;
var
   i : Integer;
begin
   for i:=0 to currentLevelStrucs.Count-1 do begin
      with currentLevelStrucs[i] do begin
         Release;
         Free;
      end;
   end;
   currentLevelStrucs.Clear;
end;

procedure TMain.LoadLevel(const fileName : String);
var
   i : Integer;
   pt : TProgressTimes;
   sp : TTBSpawnPoint;
begin
   if not FileExists(fileName) then begin
      ShowMessage('Level "'+fileName+'" not available... yet!');
      Close;
      Exit;
   end;

   ClearCurrentLevel;
   RandSeed:=0;
   levelWidth:=10;
   levelDepth:=10;
   ParseTheBallMap(String(LoadAnsiStringFromFile(fileName)), currentLevelStrucs, currentLevelName);
   pt.deltaTime:=0;
   pt.newTime:=0;
   for i:=0 to currentLevelStrucs.Count-1 do begin
      with TTheBallStructure(currentLevelStrucs[i]) do begin
         Instantiate;
         Progress(pt);
      end;
   end;
   tablePitch:=0;
   tableRoll:=0;
   AdjustTable;
   deflateEnergy:=0;
   burnOut:=0;
   with MaterialLibrary.Materials.GetLibMaterialByName('theball').Material do begin
      FrontProperties.Diffuse.Color:=clrGray80;
      FrontProperties.Ambient.Color:=clrGray20;
   end;

   // setup preview mode
   gameStatus:=gsLevelPreview;
   Mirror.Width:=levelWidth;
   Mirror.Height:=levelDepth;
   SPTable.Width:=levelWidth;
   SPTable.Height:=levelDepth;
   HTTimer.Text:='00:00.000';
   SPHBall.Visible:=False;
   SPHBall.Effects.Clear;
   sp:=TTBSpawnPoint(CurrentLevelSpecial(TTBSpawnPoint));
   if Assigned(sp) then begin
      SPHBall.AbsolutePosition:=PointMake(sp.Position);
      ALStart.Visible:=True;
      ALStart.Position.AsAffineVector:=sp.Position;
      ALStart.Move(1);
   end;
   DCBallAbsolute.Position.AsVector:=SPHBall.AbsolutePosition;
   Camera.TargetObject:=DCTable;
   HUDText.Visible:=True;
   HUDText.Text:='Level '+IntToStr(currentLevelIdx)+#13#10#13#10+currentLevelName;
   HUDText2.Text:=HUDText.Text;
end;

procedure TMain.LevelWarmup;
begin
   gameStatus:=gsWarmup;
   Camera.TargetObject:=DCBallLag;
   DCBallLag.Position:=DCTable.Position;
   cameraRefPos:=Camera.AbsolutePosition;
   spawnTime:=4;
end;

procedure TMain.SpawnBall;
var
   r : TdReal;
   m : TdMass;
   odeMat : TdMatrix3;
begin
   ALStart.Visible:=False;

   r:=0.3;
   SPHBall.Radius:=r;
   with SPHBall.Material.FrontProperties do begin
      Ambient.Color:=clrGray20;
      Diffuse.Color:=clrGray80;
      Emission.Color:=clrGray40;
   end;

   dMassSetSphere(m, 5, r);

   if ballGeom<>nil then
      dGeomDestroy(ballGeom);
   ballGeom:=dCreateSphere(space, r);

   if dSphereClass=-1 then
      dSphereClass:=dGeomGetClass(ballGeom);

   if ballBody<>nil then
      dBodyDestroy(ballBody);
   ballBody:=dBodyCreate(world);
   with DCBallAbsolute.Position do
      dBodySetPosition(ballBody, X, Y, Z);
   dRFromAxisAndAngle(odeMat, 0, 1, 0, 0);
   dBodySetRotation(ballBody, odeMat);

   dGeomSetBody(ballGeom, ballBody);
   dBodySetMass(ballBody, @m);

   DCBallLag.Position:=DCBallAbsolute.Position;

   HUDText.Visible:=False;
   Camera.Position.AsVector:=cCameraPos;
   SPHBall.Visible:=True;
   spawnTime:=Now;

   if GLSMBass.Active then begin
      with GetOrCreateSoundEmitter(Camera) do begin
         Source.SoundLibrary:=SoundLibrary;
         Source.SoundName:='thump1.wav';
         Playing:=True;
      end;
   end;

   gameStatus:=gsPlaying;
end;

function TMain.CurrentLevelSpecial(aSpecial : TTheBallStructureClass) : TTheBallStructure;
var
   i : Integer;
begin
   Result:=nil;
   for i:=0 to currentLevelStrucs.Count-1 do begin
      if currentLevelStrucs[i].InheritsFrom(aSpecial) then begin
         Result:=currentLevelStrucs[i];
         Break;
      end;
   end;
end;

function TMain.LevelWon : Boolean;
begin
   Result:=(gameStatus=gsLevelWon);
   if not Result then begin
      gameEndTime:=Now;
      HTTimer.Text:=FormatDateTime('nn:ss.zzz', gameEndTime-spawnTime);
      HUDText.Text:='Level won!';
      HUDText2.Text:='Level won!';
      HUDText.Visible:=True;
      if GLSMBass.Active then begin
         with GetOrCreateSoundEmitter(Camera) do begin
            Source.SoundLibrary:=SoundLibrary;
            Source.SoundName:='applause.wav';
            Playing:=True;
         end;
      end;
      gameStatus:=gsLevelWon;
   end;
end;

function TMain.LevelLost(const msg : String) : Boolean;
begin
   Result:=(gameStatus=gsLevelLost);
   if not Result then begin
      gameEndTime:=Now;
      HUDText.Text:=msg;
      HUDText2.Text:=msg;
      HUDText.Visible:=True;
      gameStatus:=gsLevelLost;
   end;
end;

procedure TMain.AdjustTable;
var
   a, b : TAffineVector;
   odeMat : TdMatrix3;
   bp : PdVector3;
   bpv : TAffineVector;
   d, hFix : Single;
begin
   DCTable.ResetAndPitchTurnRoll(tablePitch, 0, tableRoll);
   a:=DCTable.Direction.AsAffineVector;
   b:=DCTable.Up.AsAffineVector;
   dRFrom2Axes(odeMat, a[0], a[1], a[2], b[0], b[1], b[2]);
   dGeomSetRotation(planeGeom, odeMat);

   if Assigned(ballBody) then begin
      bp:=dBodyGetPosition(ballBody);
      bpv:=AffineVectorMake(bp[0], bp[1], bp[2]);
      d:=VectorDotProduct(bpv, a);
      if Abs(d)>levelWidth*0.5 then Exit;
      d:=VectorDotProduct(bpv, VectorCrossProduct(a, b));
      if Abs(d)>levelDepth*0.5 then Exit;
      hFix:=VectorDotProduct(bpv, b)-(SPHBall.Radius*0.2);
      if hFix>-5 then begin
         if hFix<0 then
            dBodySetPosition(ballBody, bp[0], bp[1]-hFix, bp[2]);
         hFix:=VectorDotProduct(bpv, b)-SPHBall.Radius;
         if hFix<0 then
            SPHBall.Position.Y:=SPHBall.Position.Y-hFix;
      end;
   end;
end;

// this is called by dSpaceCollide when two objects in space are
// potentially colliding.
procedure nearCallback(data : pointer; o1, o2 : PdxGeom); cdecl;
var
   i : Integer;
   b1, b2 : PdxBody;
   numc : Integer;
   contact : array [0..2] of TdContact;
   c : TdJointID;
begin
   // exit without doing anything if the two bodies are connected by a joint
   b1:=dGeomGetBody(o1);
   b2:=dGeomGetBody(o2);
   if Assigned(b1) and Assigned(b2) and (dAreConnected(b1, b2)<>0) then Exit;

   for i:=0 to 2 do begin
     contact[i].surface.mode:=dContactBounce or dContactApprox1;
     contact[i].surface.mu:=1000;
     contact[i].surface.mu2:=0;
     contact[i].surface.bounce:=0.5;
     contact[i].surface.bounce_vel:=0;
   end;

   numc:=dCollide(o1, o2, 3, contact[0].geom, SizeOf(TdContact));
   for i:=0 to numc-1 do begin
      c:=dJointCreateContact(Main.world, Main.contactgroup, @contact[i]);
      dJointAttach(c, b1, b2);
   end;
end;

procedure TMain.CadencerProgress(Sender: TObject; const deltaTime,
  newTime: Double);
var
   i, x, y : Integer;
   ballUp, ballDirection, tableUp : TVector;
   pt : TProgressTimes;
   mp : TPoint;
   dmx, dmy : Single;
   d : Single;
   struc : TTheBallStructure;
begin
   // table movement
   if gameStatus=gsPlaying then begin
      if isMouseDown then begin
         GetCursorPos(mp);
         d:=1;
         dmx:=ClampValue((mouseDownX-mp.X)*0.15, -d, d);
         mouseDownX:=mouseDownX-dmx*0.8;
         dmy:=ClampValue((mp.Y-mouseDownY)*0.15, -d, d);
         mouseDownY:=mouseDownY+dmy*0.8;
         x:=Screen.Width div 2;
         y:=Screen.Height div 2;
         SetCursorPos(x, y);
         mouseDownX:=mouseDownX-mp.X+x;
         mouseDownY:=mouseDownY-mp.Y+y;
      end else begin
         if IsKeyDown(VK_DOWN) then dmy:=1
         else if IsKeyDown(VK_UP) then dmy:=-1
         else dmy:=0;
         if IsKeyDown(VK_LEFT) then dmx:=1
         else if IsKeyDown(VK_RIGHT) then dmx:=-1
         else dmx:=0;
      end;

      tablePitch:=ClampValue(tablePitch+dmx*0.04, -20, 20);
      tableRoll:=ClampValue(tableRoll+dmy*0.04, -20, 20);

      // update ball position
      PositionSceneObject(DCBallAbsolute, ballGeom);
      SPHBall.AbsolutePosition:=DCBallAbsolute.Position.AsVector;
      ballUp:=DCObstacles.AbsoluteToLocal(DCBallAbsolute.Up.AsVector);
      ballDirection:=DCObstacles.AbsoluteToLocal(DCBallAbsolute.Direction.AsVector);
      SPHBall.Up.AsVector:=ballUp;
      SPHBall.Direction.AsVector:=ballDirection;

      // update ODE plane
      AdjustTable;
   end else if gameStatus=gsLevelPreview then begin
      Camera.MoveAroundTarget(0, deltaTime*8);

      ALStart.Position.Y:=3+Sin(newTime*10)*0.2;
   end else if gameStatus=gsWarmup then begin
      spawnTime:=spawnTime-deltaTime;
      if spawnTime>=1 then
         HUDText.Text:='Game begins in... '+IntToStr(Trunc(spawnTime))
      else HUDText.Text:='Go!';
      HUDText2.Text:=HUDText.Text;
      d:=spawnTime/4;
      DCBallLag.Position.AsVector:=VectorLerp(SPHBall.AbsolutePosition,
                                              DCTable.Position.AsVector, d);
      Camera.Position.AsVector:=VectorLerp(cCameraPos, cameraRefPos, d);

      ALStart.Material.FrontProperties.Diffuse.Alpha:=d;
      ALStart.Position.Y:=3+Sin(newTime*10)*0.2;
      if spawnTime<=0 then begin
         DCBallLag.Position.AsVector:=SPHBall.AbsolutePosition;
         Camera.Position.AsVector:=cCameraPos;
         GetCursorPos(mp);
         mouseDownX:=mp.X;
         mouseDownY:=mp.Y;
         SpawnBall;
      end;
   end;

   // update map strucs
   pt.deltaTime:=deltaTime;
   pt.newTime:=newTime;
   for i:=0 to currentLevelStrucs.Count-1 do begin
      struc:=currentLevelStrucs[i];
      if Assigned(struc) then
         struc.Progress(pt);
   end;
   currentLevelStrucs.Pack;

   if gameStatus=gsPlaying then begin
      // perform collision detection
      dSpaceCollide(space, nil, nearCallback);

      // add forces to the ball
      dBodyAddForce(ballBody, 0, -9.81, 0);
      if verticalForce<>0 then begin
         tableUp:=VectorScale(DCTable.AbsoluteUp, verticalForce);
         dBodyAddForce(ballBody, tableUp[0], tableUp[1], tableUp[2]);
         verticalForce:=0;
      end;
      if deflateEnergy>0 then begin
         dBodyAddForce(ballBody, 0, 9.81*(3-deflateEnergy)*0.3, 0);
         d:=deflateEnergy*10;
         dBodyAddRelForce(ballBody,
            deflateVector[0]*d, deflateVector[1]*d, deflateVector[2]*d);
         deflateEnergy:=deflateEnergy-deltaTime;
         if deflateEnergy<0.3 then
            LevelLost('You''ve been deflated!');
         SPHBall.Radius:=deflateEnergy*0.1;
      end;

      if burnOut>0 then begin
         d:=0.8*burnOut/3;
         with MaterialLibrary.Materials.GetLibMaterialByName('theball') do begin
            Material.FrontProperties.Diffuse.Color:=VectorMake(d, d, d, 1);
            d:=d*0.5;
            Material.FrontProperties.Emission.Color:=VectorMake(d, d, d, 1);
         end;
         burnOut:=burnOut-deltaTime;
         if burnOut<=0 then
            LevelLost('You''ve been burned!');
      end;

      // progress/solve the physics
      dWorldStep(world, deltaTime);

      // remove all contact joints
      dJointGroupEmpty(contactgroup);
   end;

   if gameStatus in [gsLevelWon, gsLevelLost] then begin
      SceneViewer.Invalidate;
      d:=(Now-gameEndTime)*3600*24;
      if d>=5 then begin
         if gameStatus=gsLevelWon then
            Inc(currentLevelIdx);
         LoadLevel(ExtractFilePath(Application.ExeName)+Format('Level%.2d.txt', [currentLevelIdx]));
      end;
   end else if gameStatus=gsPlaying then
       HTTimer.Text:=FormatDateTime('nn:ss.zzz', Now-spawnTime);

   if gameStatus=gsPlaying then begin
      d:=DCBallAbsolute.Position.Y;
      if d<-30 then begin
         if d>-35 then begin
            d:=Sqr(d+28);
         end else begin
            d:=50+(d+35);
            if d<0 then d:=0;
         end;
         if d>0 then begin
            SPRBallFlare.Visible:=True;
            SPRBallFlare.Width:=d;
            SPRBallFlare.Height:=d;
         end else begin
            SPRBallFlare.Visible:=False;
            LevelLost('You''ve fallen into the pit of doom!');
         end;
      end;

      DCBallLag.Position.AsVector:=VectorLerp(DCBallLag.Position.AsVector,
                                              SPHBall.AbsolutePosition, 0.2);
   end;
end;

procedure TMain.TimerTimer(Sender: TObject);
begin
   Caption:=Format('%.1f FPS', [SceneViewer.FramesPerSecond]);
   SceneViewer.ResetPerformanceMonitor;
end;

procedure TMain.SceneViewerMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
   mp : TPoint;
begin
   isMouseDown:=not isMouseDown;
   if isMouseDown then
      Screen.Cursor:=crNone
   else Screen.Cursor:=crDefault;

   GetCursorPos(mp);
   mouseDownX:=mp.X;
   mouseDownY:=mp.Y;

   if gameStatus=gsLevelPreview then
      LevelWarmup;
end;

procedure TMain.SceneViewerMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
//   Screen.Cursor:=crDefault;

//   isMouseDown:=False;
end;

procedure TMain.FormResize(Sender: TObject);
begin
   Camera.SceneScale:=ClientHeight/435;
   HUDText.Position.X:=ClientWidth*0.5;
   HUDText.Position.Y:=ClientHeight*0.5;
   HUDText2.Position.X:=HUDText.Position.X-1;
   HUDText2.Position.Y:=HUDText.Position.Y-1;

   if (Width>=Screen.Width) and (Height>=Screen.Height) then
      BorderStyle:=bsNone
   else BorderStyle:=bsSizeable;
end;

procedure TMain.FormKeyPress(Sender: TObject; var Key: Char);
begin
   case Key of
      #27 : Close;
      ' ', #13 : begin
         if gameStatus=gsLevelPreview then
            LevelWarmup;
      end;
   end;
end;

procedure TMain.MirrorBeginRenderingMirrors(Sender: TObject);
begin
   DCMapTransparent.Visible:=False;
end;

procedure TMain.MirrorEndRenderingMirrors(Sender: TObject);
begin
   DCMapTransparent.Visible:=True;
end;

end.
