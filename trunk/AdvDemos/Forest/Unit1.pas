unit Unit1;

interface

uses
   Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
   GLWin32Viewer, GLCadencer, GLTexture, GLScene, GLTerrainRenderer,
   GLHeightData, GLObjects, VectorGeometry, GLTree, JPEG, TGA, GLKeyboard,
   VectorLists, GLBitmapFont, GLContext, GLWindowsFont, GLHUDObjects, GLSkydome,
   GLImposter, GLParticleFX, GLGraphics, PersistentClasses, OpenGL1x, ExtCtrls,
   GLUtils, GLTextureCombiners, XOpenGL, GLHeightTileFileHDS, GLMaterial,
   GLCoordinates, GLCrossPlatform, BaseClasses, GLRenderContextInfo;

type
   TForm1 = class(TForm)
    SceneViewer: TGLSceneViewer;
    GLScene: TGLScene;
      MLTrees: TGLMaterialLibrary;
      MLTerrain: TGLMaterialLibrary;
    GLCadencer: TGLCadencer;
      Terrain: TGLTerrainRenderer;
      Camera: TGLCamera;
      Light: TGLLightSource;
      GLHUDText1: TGLHUDText;
      GLWindowsBitmapFont1: TGLWindowsBitmapFont;
    EarthSkyDome: TGLEarthSkyDome;
    GLRenderPoint: TGLRenderPoint;
    SIBTree: TGLStaticImposterBuilder;
      DOTrees: TGLDirectOpenGL;
      PFXTrees: TGLCustomPFXManager;
      RenderTrees: TGLParticleFXRenderer;
      Timer1: TTimer;
      MLWater: TGLMaterialLibrary;
    DOInitializeReflection: TGLDirectOpenGL;
    DOGLSLWaterPlane: TGLDirectOpenGL;
    DOClassicWaterPlane: TGLDirectOpenGL;
    GLHeightTileFileHDS: TGLHeightTileFileHDS;
      procedure FormCreate(Sender: TObject);
      procedure FormDestroy(Sender: TObject);
      procedure TerrainGetTerrainBounds(var l, t, r, b: Single);
      procedure GLCadencerProgress(Sender: TObject; const deltaTime,
        newTime: Double);
      procedure FormKeyDown(Sender: TObject; var Key: Word;
        Shift: TShiftState);
      procedure DOTreesRender(Sender: TObject; var rci: TRenderContextInfo);
      procedure PFXTreesBeginParticles(Sender: TObject;
        var rci: TRenderContextInfo);
      procedure PFXTreesCreateParticle(Sender: TObject;
        aParticle: TGLParticle);
      procedure PFXTreesEndParticles(Sender: TObject;
        var rci: TRenderContextInfo);
      procedure PFXTreesRenderParticle(Sender: TObject;
        aParticle: TGLParticle; var rci: TRenderContextInfo);
      procedure SIBTreeImposterLoaded(Sender: TObject;
        impostoredObject: TGLBaseSceneObject; destImposter: TImposter);
      function SIBTreeLoadingImposter(Sender: TObject;
        impostoredObject: TGLBaseSceneObject;
        destImposter: TImposter): TGLBitmap32;
      procedure Timer1Timer(Sender: TObject);
      procedure PFXTreesProgress(Sender: TObject;
        const progressTime: TProgressTimes; var defaultProgress: Boolean);
      function PFXTreesGetParticleCountEvent(Sender: TObject): Integer;
    procedure FormResize(Sender: TObject);
    procedure DOInitializeReflectionRender(Sender: TObject;
      var rci: TRenderContextInfo);
    procedure DOGLSLWaterPlaneRender(Sender: TObject;
      var rci: TRenderContextInfo);
    procedure DOClassicWaterPlaneRender(Sender: TObject;
      var rci: TRenderContextInfo);
    procedure FormDeactivate(Sender: TObject);
    procedure FormShow(Sender: TObject);
   private
      { Private declarations }
//      hscale, mapwidth, mapheight : Single;
      lmp : TPoint;
      camPitch, camTurn, camTime, curPitch, curTurn : Single;

      procedure SetupReflectionMatrix;
   public
      { Public declarations }
      TestTree : TGLTree;
      TreesShown : Integer;
      nearTrees : TPersistentObjectList;
      imposter : TImposter;
      densityBitmap : TBitmap;
      mirrorTexture : TGLTextureHandle;
      mirrorTexType : TGLEnum;
      reflectionProgram : TGLProgramHandle;
      supportsGLSL : Boolean;
      enableGLSL : Boolean;
      enableRectReflection, enableTex2DReflection : Boolean;
   end;

var
   Form1: TForm1;

implementation

{$R *.dfm}

uses GLScreen;

const
   cImposterCacheFile : String = 'media\imposters.bmp';
   cMapWidth : Integer = 1024;
   cMapHeight : Integer = 1024;
   cBaseSpeed : Single = 50;

procedure TForm1.FormCreate(Sender: TObject);
var
   density : TPicture;
begin
   // go to 1024x768x32
   SetFullscreenMode(GetIndexFromResolution(800, 600, 32), 85);
   Application.OnDeactivate:=FormDeactivate;

   SetCurrentDir(ExtractFilePath(Application.ExeName));

   with MLTerrain.AddTextureMaterial('Terrain', 'media\volcano_TX_low.jpg') do
      Texture2Name:='Detail';
   with MLTerrain.AddTextureMaterial('Detail', 'media\detailmap.jpg') do begin
      Material.Texture.TextureMode:=tmModulate;
      TextureScale.SetPoint(128,128,128);
   end;
   Terrain.Material.MaterialLibrary:=MLTerrain;
   Terrain.Material.LibMaterialName:='Terrain';

   // Load tree textures
   with MLTrees.AddTextureMaterial('Leaf','media\leaf.tga') do begin
      Material.Texture.TextureFormat:=tfRGBA;
      Material.Texture.TextureMode:=tmModulate;
      Material.Texture.MinFilter:=miNearestMipmapNearest;
      Material.BlendingMode:=bmAlphaTest50;
   end;
   with MLTrees.AddTextureMaterial('Bark','media\zbark_016.jpg') do
      Material.Texture.TextureMode:=tmModulate;

   // Create test tree
   Randomize;
   TestTree:=TGLTree(GLScene.Objects.AddNewChild(TGLTree));
   with TestTree do begin
      Visible:=False;
      MaterialLibrary:=MLTrees;
      LeafMaterialName:='Leaf';
      LeafBackMaterialName:='Leaf';
      BranchMaterialName:='Bark';
      Up.SetVector(ZHmgVector);
      Direction.SetVector(YHmgVector);
      Depth:=9;
      BranchFacets:=6;
      LeafSize:=0.50;
      BranchAngle:=0.65;
      BranchTwist:=135;
      ForceTotalRebuild;
   end;

   SIBTree.RequestImposterFor(TestTree);

   densityBitmap:=TBitmap.Create;
   try
      densityBitmap.PixelFormat:=pf24bit;
      Density:=TPicture.Create;
      try
         Density.LoadFromFile('media\volcano_trees.jpg');
         densityBitmap.Width:=Density.Width;
         densityBitmap.Height:=Density.Height;
         densityBitmap.Canvas.Draw(0, 0, Density.Graphic);
      finally
         Density.Free;
      end;
      PFXTrees.CreateParticles(10000);
   finally
      densityBitmap.Free;
   end;
   TreesShown:=2000;

   Light.Pitch(30);
   Camera.Position.Y:=Terrain.InterpolatedHeight(Camera.Position.AsVector)+10;

   lmp:=ClientToScreen(Point(Width div 2, Height div 2));
   SetCursorPos(lmp.X, lmp.Y);
   ShowCursor(False);

   nearTrees:=TPersistentObjectList.Create;

   camTurn:=-60;
   enableRectReflection:=False;
   enableTex2DReflection:=False;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
   RestoreDefaultMode;

   ShowCursor(True);
   nearTrees.Free;
end;

procedure TForm1.FormResize(Sender: TObject);
begin
   Camera.FocalLength:=Width*50/800;
end;

procedure TForm1.FormDeactivate(Sender: TObject);
begin
   Close;
end;

procedure TForm1.FormShow(Sender: TObject);
begin
   SetFocus;
end;

procedure TForm1.GLCadencerProgress(Sender: TObject; const deltaTime,
  newTime: Double);
var
   speed, z : Single;
   nmp : TPoint;
begin
   // Camera movement
   if IsKeyDown(VK_SHIFT) then
      speed:=deltaTime*cBaseSpeed*10
   else speed:=deltaTime*cBaseSpeed;

   if IsKeyDown(VK_UP) or IsKeyDown('W') or IsKeyDown('Z') then
      Camera.Move(speed)
   else if IsKeyDown(VK_DOWN) or IsKeyDown('S') then
      Camera.Move(-speed);
      
   if IsKeyDown(VK_LEFT) or IsKeyDown('A') or IsKeyDown('Q') then
      Camera.Slide(-speed)
   else if IsKeyDown(VK_RIGHT) or IsKeyDown('D') then
      Camera.Slide(speed);

   z:=Terrain.Position.Y+Terrain.InterpolatedHeight(Camera.Position.AsVector);
   if z<0 then z:=0;
   z:=z+10;
   if Camera.Position.Y<z then
      Camera.Position.Y:=z;

   GetCursorPos(nmp);
   camTurn:=camTurn-(lmp.X-nmp.X)*0.2;
   camPitch:=camPitch+(lmp.Y-nmp.Y)*0.2;
   camTime:=camTime+deltaTime;
   while camTime>0 do begin
      curTurn:=Lerp(curTurn, camTurn, 0.2);
      curPitch:=Lerp(curPitch, camPitch, 0.2);
      Camera.Position.Y:=Lerp(Camera.Position.Y, z, 0.2);
      camTime:=camTime-0.01;
   end;
   Camera.ResetRotations;
   Camera.Turn(curTurn);
   Camera.Pitch(curPitch);
   SetCursorPos(lmp.X, lmp.Y);

   SceneViewer.Invalidate;
end;

procedure TForm1.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
   case key of
      VK_ESCAPE : Form1.Close;
      VK_ADD : if TreesShown<PFXTrees.Particles.ItemCount then
         TreesShown:=TreesShown+100;
      VK_SUBTRACT : if TreesShown>0 then
          TreesShown:=TreesShown-100;
      Word('R') : enableTex2DReflection:=not enableTex2DReflection;
      Word('G') : if supportsGLSL then begin
         enableGLSL:=not enableGLSL;
         enableTex2DReflection:=True;
      end;
   end;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
var
   hud : String;
begin
   hud:=Format('%.1f FPS - %d trees'#13#10'Tree sort: %f ms',
               [SceneViewer.FramesPerSecond, TreesShown, RenderTrees.LastSortTime]);
   if enableTex2DReflection then begin
      hud:=hud+#13#10+'Water reflections';
      if enableRectReflection then
         hud:=hud+' (RECT)';
   end;
   if enableGLSL and enableTex2DReflection then
      hud:=hud+#13#10+'GLSL water';
   GLHUDText1.Text:=hud;
   SceneViewer.ResetPerformanceMonitor;
   Caption:=Format('%.2f', [RenderTrees.LastSortTime]);
end;

procedure TForm1.PFXTreesCreateParticle(Sender: TObject;
  aParticle: TGLParticle);
var
   u, v, p : Single;
//   x, y, i, j, dark : Integer;
   pixelX, pixelY : Integer;
begin
   repeat
      repeat
         u:=Random*0.88+0.06;
         v:=Random*0.88+0.06;
         pixelX:=Round(u*densityBitmap.Width);
         pixelY:=Round(v*densityBitmap.Height);
         p:=((densityBitmap.Canvas.Pixels[pixelX, pixelY] shr 8) and 255)/255;
      until p>Random;
      aParticle.PosX:=(0.5-u)*Terrain.Scale.X*cMapWidth;
      aParticle.PosY:=0;
      aParticle.PosZ:=(0.5-(1-v))*Terrain.Scale.Y*cMapHeight;
      aParticle.PosY:=Terrain.Position.Y+Terrain.InterpolatedHeight(aParticle.Position);
   until aParticle.PosY>=0;
   aParticle.Tag:=Random(360);

   // Remove probablility for current location
//   densityBitmap.Canvas.Pixels[pixelX, pixelY]:=
//      RGB(0, GetRValue(densityBitmap.Canvas.Pixels[pixelX, pixelY]) div 2, 0);

   // Blob shadow beneath tree
{   with MLTerrain.Materials[0].Material.Texture do begin
      with Image.GetBitmap32(GL_TEXTURE_2D) do begin
         x:=Round(u*(Image.Width-1));
         y:=Round((1-v)*(Image.Height-1));
         for i:=-8 to 8 do
            for j:=-8 to 8 do with ScanLine[y+j][x+i] do begin
               dark:=20;
               r:=MaxInteger(r-dark,0);
               g:=MaxInteger(g-dark,0);
               b:=MaxInteger(b-dark,0);
            end;
      end;
   end;}
end;

procedure TForm1.PFXTreesBeginParticles(Sender: TObject;
  var rci: TRenderContextInfo);
begin
   imposter:=SIBTree.ImposterFor(TestTree);
   imposter.BeginRender(rci);
end;

procedure TForm1.PFXTreesRenderParticle(Sender: TObject;
  aParticle: TGLParticle; var rci: TRenderContextInfo);
const
   cTreeCenteringOffset : TAffineVector = (0, 30, 0);
var
   d : Single;
   camPos : TVector;
begin
   if not IsVolumeClipped(VectorAdd(aParticle.Position, cTreeCenteringOffset), 30, rci.rcci) then begin;
      VectorSubtract(rci.cameraPosition, aParticle.Position, camPos);
      d:=VectorNorm(camPos);
      if d>Sqr(180) then begin
         RotateVectorAroundY(PAffineVector(@camPos)^, aParticle.Tag*cPIdiv180);
         imposter.Render(rci, VectorMake(aParticle.Position), camPos, 10);
      end else begin
         nearTrees.Add(aParticle);
      end;
   end;
end;

procedure TForm1.PFXTreesEndParticles(Sender: TObject;
  var rci: TRenderContextInfo);
var
   aParticle : TGLParticle;
   camPos : TVector;
begin
   // Only 20 trees max rendered at full res, force imposter'ing the others
   while nearTrees.Count>20 do begin
      aParticle:=TGLParticle(nearTrees.First);
      VectorSubtract(rci.cameraPosition, aParticle.Position, camPos);
      RotateVectorAroundY(PAffineVector(@camPos)^, aParticle.Tag*cPIdiv180);
      imposter.Render(rci, VectorMake(aParticle.Position), camPos, 10);
      nearTrees.Delete(0);
   end;

   imposter.EndRender(rci);
end;

procedure TForm1.DOTreesRender(Sender: TObject;
  var rci: TRenderContextInfo);
var
   i : Integer;
   particle : TGLParticle;
begin
   rci.GLStates.ResetAll;
   glDisable(Gl_BLEND);
   for i:=0 to nearTrees.Count-1 do begin
      particle:=TGLParticle(nearTrees[i]);
      glPushMatrix;
      glTranslatef(particle.PosX, particle.PosY, particle.PosZ);
      glScalef(10, 10, 10);
      glRotatef(-particle.Tag, 0, 1, 0);
      glRotatef(Cos(GLCadencer.CurrentTime+particle.ID*15)*0.2, 1, 0, 0);
      glRotatef(Cos(GLCadencer.CurrentTime*1.3+particle.ID*15)*0.2, 0, 0, 1);
      TestTree.Render(rci);
      glPopMatrix;
   end;
   nearTrees.Clear;
end;

procedure TForm1.TerrainGetTerrainBounds(var l, t, r, b: Single);
begin
   l:=0;
   t:=cMapHeight;
   r:=cMapWidth;
   b:=0;
end;

function TForm1.SIBTreeLoadingImposter(Sender: TObject;
  impostoredObject: TGLBaseSceneObject;
  destImposter: TImposter): TGLBitmap32;
var
   bmp : TBitmap;
   cacheAge, exeAge : TDateTime;
begin
   Tag:=1;
   Result:=nil;
   if not FileExists(cImposterCacheFile) then Exit;
   cacheAge:=FileDateToDateTime(FileAge(cImposterCacheFile));
   exeAge:=FileDateToDateTime(FileAge(Application.ExeName));
   if cacheAge<exeAge then Exit;

   Tag:=0;
   bmp:=TBitmap.Create;
   bmp.LoadFromFile(cImposterCacheFile);
   Result:=TGLBitmap32.Create;
   Result.Assign(bmp);
   bmp.Free;
end;

procedure TForm1.SIBTreeImposterLoaded(Sender: TObject;
  impostoredObject: TGLBaseSceneObject; destImposter: TImposter);
var
   bmp32 : TGLBitmap32;
   bmp : TBitmap;
begin
   if Tag=1 then begin
      bmp32:=TGLBitmap32.Create;
      bmp32.AssignFromTexture2D(SIBTree.ImposterFor(TestTree).Texture);
      bmp:=bmp32.Create32BitsBitmap;
      bmp.SaveToFile(cImposterCacheFile);
      bmp.Free;
      bmp32.Free;
   end;
end;

function TForm1.PFXTreesGetParticleCountEvent(Sender: TObject): Integer;
begin
   Result:=TreesShown;
end;

procedure TForm1.PFXTreesProgress(Sender: TObject;
  const progressTime: TProgressTimes; var defaultProgress: Boolean);
begin
   defaultProgress:=False;
end;

procedure TForm1.DOInitializeReflectionRender(Sender: TObject;
  var rci: TRenderContextInfo);
var
   w, h : Integer;
   refMat, curMat : TMatrix;
   cameraPosBackup, cameraDirectionBackup : TVector;
   frustumBackup : TFrustum;
   clipPlane : TDoubleHmgPlane;
begin
   supportsGLSL:=GL_ARB_shader_objects and GL_ARB_fragment_shader and GL_ARB_vertex_shader;
   enableRectReflection:=GL_NV_texture_rectangle and ((not enableGLSL) or GL_EXT_Cg_shader);

   if not enableTex2DReflection then Exit;

   if not Assigned(mirrorTexture) then
      mirrorTexture:=TGLTextureHandle.Create;

   glPushAttrib(GL_ENABLE_BIT);
   glPushMatrix;

   // Mirror coordinates
   glLoadMatrixf(@TGLSceneBuffer(rci.buffer).ModelViewMatrix);
   refMat:=MakeReflectionMatrix(NullVector, YVector);
   glMultMatrixf(@refMat);
   glGetFloatv(GL_MODELVIEW_MATRIX, @curMat);
   glLoadMatrixf(@TGLSceneBuffer(rci.buffer).ModelViewMatrix);
   TGLSceneBuffer(rci.buffer).PushModelViewMatrix(curMat);

   glFrontFace(GL_CW);

   glEnable(GL_CLIP_PLANE0);
   SetPlane(clipPlane, PlaneMake(AffineVectorMake(0, 1, 0), VectorNegate(YVector)));
   glClipPlane(GL_CLIP_PLANE0, @clipPlane); 

   cameraPosBackup:=rci.cameraPosition;
   cameraDirectionBackup:=rci.cameraDirection;
   frustumBackup:=rci.rcci.frustum;
   rci.cameraPosition:=VectorTransform(rci.cameraPosition, refMat);
   rci.cameraDirection:=VectorTransform(rci.cameraDirection, refMat);
   with rci.rcci.frustum do begin
      pLeft:=VectorTransform(pLeft, refMat);
      pRight:=VectorTransform(pRight, refMat);
      pTop:=VectorTransform(pTop, refMat);
      pBottom:=VectorTransform(pBottom, refMat);
      pNear:=VectorTransform(pNear, refMat);
      pFar:=VectorTransform(pFar, refMat);
   end;

   glLoadIdentity;
   Camera.Apply;
   glMultMatrixf(@refMat);

   EarthSkyDome.DoRender(rci, True, False);
   glMultMatrixf(PGLFloat(Terrain.AbsoluteMatrixAsAddress));
   Terrain.DoRender(rci, True, False);

   rci.cameraPosition:=cameraPosBackup;
   rci.cameraDirection:=cameraDirectionBackup;
   rci.rcci.frustum:=frustumBackup;

   // Restore to "normal"
   TGLSceneBuffer(rci.buffer).PopModelViewMatrix;
   glLoadMatrixf(@TGLSceneBuffer(rci.buffer).ModelViewMatrix);
   GLScene.SetupLights(TGLSceneBuffer(rci.buffer).LimitOf[limLights]);

   glFrontFace(GL_CCW);
   glPopMatrix;
   glPopAttrib;
   rci.GLStates.ResetGLMaterialColors;
   rci.GLStates.ResetGLCurrentTexture;

   if enableRectReflection then begin
      mirrorTexType:=GL_TEXTURE_RECTANGLE_NV;
      w:=SceneViewer.Width;
      h:=SceneViewer.Height;
   end else begin
      mirrorTexType:=GL_TEXTURE_2D;
      w:=RoundUpToPowerOf2(SceneViewer.Width);
      h:=RoundUpToPowerOf2(SceneViewer.Height);
   end;

   if mirrorTexture.Handle=0 then begin
      mirrorTexture.AllocateHandle;
      glBindTexture(mirrorTexType, mirrorTexture.Handle);

     	glTexParameteri(mirrorTexType, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
      glTexParameteri(mirrorTexType, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
     	glTexParameteri(mirrorTexType, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
	   glTexParameteri(mirrorTexType, GL_TEXTURE_MAG_FILTER, GL_LINEAR);

   	glCopyTexImage2d(mirrorTexType, 0, GL_RGBA8,
                       0, 0, w, h, 0);
   end else begin
      glBindTexture(mirrorTexType, mirrorTexture.Handle);

      glCopyTexSubImage2D(mirrorTexType, 0, 0, 0,
                          0, 0, w, h);
   end;

   glClear(GL_COLOR_BUFFER_BIT+GL_DEPTH_BUFFER_BIT+GL_STENCIL_BUFFER_BIT);
end;

procedure TForm1.DOClassicWaterPlaneRender(Sender: TObject;
  var rci: TRenderContextInfo);
const
   cWaveScale = 7;
   cWaveSpeed = 0.02;
   cSinScale = 0.02;
var
   tex0Matrix, tex1Matrix : TMatrix;
   tWave : Single;
   pos : TAffineVector;
   tex : TTexPoint;
   x, y : Integer;
begin
   if enableGLSL and enableTex2DReflection then Exit;
                                                     
   tWave:=GLCadencer.CurrentTime*cWaveSpeed;

   glPushAttrib(GL_ENABLE_BIT);

   glMatrixMode(GL_TEXTURE);

   tex0Matrix:=IdentityHmgMatrix;
   tex0Matrix[0][0]:=3*cWaveScale;
   tex0Matrix[1][1]:=4*cWaveScale;
   tex0Matrix[3][0]:=tWave*1.1;
   tex0Matrix[3][1]:=tWave*1.06;
   glLoadMatrixf(@tex0Matrix);
   glBindTexture(GL_TEXTURE_2D, MLWater.Materials[0].Material.Texture.Handle);
   glEnable(GL_TEXTURE_2D);

   glActiveTextureARB(GL_TEXTURE1_ARB);

   tex1Matrix:=IdentityHmgMatrix;
   tex1Matrix[0][0]:=cWaveScale;
   tex1Matrix[1][1]:=cWaveScale;
   tex1Matrix[3][0]:=tWave*0.83;
   tex1Matrix[3][1]:=tWave*0.79;
   glLoadMatrixf(@tex1Matrix);
   glBindTexture(GL_TEXTURE_2D, MLWater.Materials[0].Material.Texture.Handle);
   glEnable(GL_TEXTURE_2D);

   if enableTex2DReflection then begin
      glActiveTextureARB(GL_TEXTURE2_ARB);

      glBindTexture(mirrorTexType, mirrorTexture.Handle);
      glEnable(mirrorTexType);

      SetupReflectionMatrix;
   end;

   glActiveTextureARB(GL_TEXTURE0_ARB);

   glMatrixMode(GL_MODELVIEW);

   if enableTex2DReflection then begin
      SetupTextureCombiners( 'Tex0:=Tex1*Tex0;'#13#10
                            +'Tex1:=Tex0+Col;'#13#10
                            +'Tex2:=Tex1+Tex2-0.5;');
      glColor4f(0.0, 0.3, 0.3, 1);
   end else begin
      SetupTextureCombiners( 'Tex0:=Tex1*Tex0;'#13#10
                            +'Tex1:=Tex0+Col;');
      glColor4f(0.0, 0.4, 0.7, 1);
   end;

   glDisable(GL_CULL_FACE);
   for y:=-10 to 10-1 do begin
      glBegin(GL_QUAD_STRIP);
      for x:=-10 to 10 do begin
         SetVector(pos, x*1500, 0, y*1500);
         tex:=TexPointMake(x, y);
         glMultiTexCoord2fvARB(GL_TEXTURE0_ARB, @tex);
         glMultiTexCoord2fvARB(GL_TEXTURE1_ARB, @tex);
         glMultiTexCoord3fvARB(GL_TEXTURE2_ARB, @pos);
         glVertex3fv(@pos);
         SetVector(pos, x*1500, 0, (y+1)*1500);
         tex:=TexPointMake(x, (y+1));
         glMultiTexCoord3fvARB(GL_TEXTURE0_ARB, @tex);
         glMultiTexCoord3fvARB(GL_TEXTURE1_ARB, @tex);
         glMultiTexCoord3fvARB(GL_TEXTURE2_ARB, @pos);
         glVertex3fv(@pos);
      end;
      glEnd;
   end;

   glMatrixMode(GL_TEXTURE);

   if enableTex2DReflection then begin
      glActiveTextureARB(GL_TEXTURE2_ARB);
      glLoadIdentity;
   end;

   glActiveTextureARB(GL_TEXTURE1_ARB);
   glLoadIdentity;

   glActiveTextureARB(GL_TEXTURE0_ARB);
   glLoadIdentity;

   glMatrixMode(GL_MODELVIEW);

   glPopAttrib;
end;

procedure TForm1.DOGLSLWaterPlaneRender(Sender: TObject;
  var rci: TRenderContextInfo);
var
   x, y : Integer;
begin
   if not (enableGLSL and enableTex2DReflection) then Exit;

   if not Assigned(reflectionProgram) then begin
      reflectionProgram:=TGLProgramHandle.CreateAndAllocate;

      reflectionProgram.AddShader(TGLVertexShaderHandle, LoadAnsiStringFromFile('media\water_vp.glsl'));
      reflectionProgram.AddShader(TGLFragmentShaderHandle, LoadAnsiStringFromFile('media\water_fp.glsl'));
      if not reflectionProgram.LinkProgram then
         raise Exception.Create(reflectionProgram.InfoLog);
      if not reflectionProgram.ValidateProgram then
         raise Exception.Create(reflectionProgram.InfoLog);
   end;

{   glEnable(GL_STENCIL_TEST);
   glColorMask(False, False, False, False);
   glStencilFunc(GL_ALWAYS, 255, 255);
   glStencilOp(GL_KEEP, GL_KEEP, GL_REPLACE);

   for y:=-5 to 5-1 do begin
      glBegin(GL_QUAD_STRIP);
      for x:=-5 to 5 do begin
         glVertex3f(x*1500, 0, y*1500);
         glVertex3f(x*1500, 0, (y+1)*1500);
      end;
      glEnd;
   end;

   glColorMask(True, True, True, True);
   glStencilFunc(GL_EQUAL, 255, 255);
   glStencilOp(GL_KEEP, GL_KEEP, GL_KEEP);
   glDisable(GL_DEPTH_TEST); }

   glBindTexture(mirrorTexType, mirrorTexture.Handle);

   glMatrixMode(GL_TEXTURE);
   
   SetupReflectionMatrix;
   
   glMatrixMode(GL_MODELVIEW);

   reflectionProgram.UseProgramObject;
   reflectionProgram.Uniform1f['Time']:=GLCadencer.CurrentTime;
   reflectionProgram.Uniform4f['EyePos']:=Camera.AbsolutePosition;
   reflectionProgram.Uniform1i['ReflectionMap']:=0;
   glActiveTextureARB(GL_TEXTURE1_ARB);
   glBindTexture(GL_TEXTURE_2D, MLWater.Materials[1].Material.Texture.Handle);
   reflectionProgram.Uniform1i['WaveMap']:=1;
   glActiveTextureARB(GL_TEXTURE0_ARB);

//   reflectionProgram.EndUseProgramObject;

   for y:=-10 to 10-1 do begin
      glBegin(GL_QUAD_STRIP);
      for x:=-10 to 10 do begin
         glVertex3f(x*1500, 0, y*1500);
         glVertex3f(x*1500, 0, (y+1)*1500);
      end;
      glEnd;
   end;

   reflectionProgram.EndUseProgramObject;
   
   glMatrixMode(GL_TEXTURE);
      glLoadIdentity;
   glMatrixMode(GL_MODELVIEW);

   glDisable(GL_TEXTURE_2D);

   glEnable(GL_DEPTH_TEST);
   glDisable(GL_STENCIL_TEST);
end;

// SetupReflectionMatrix
//
procedure TForm1.SetupReflectionMatrix;
var
   w, h : Single;
begin
   if mirrorTexType=GL_TEXTURE_2D then begin
      w:=0.5*SceneViewer.Width/RoundUpToPowerOf2(SceneViewer.Width);
      h:=0.5*SceneViewer.Height/RoundUpToPowerOf2(SceneViewer.Height);
   end else begin
      w:=0.5*SceneViewer.Width;
      h:=0.5*SceneViewer.Height;
   end;

   glLoadIdentity;
   glTranslatef(w, h, 0);
   glScalef(w, h, 0);
   Camera.ApplyPerspective(SceneViewer.Buffer.ViewPort, SceneViewer.Width, SceneViewer.Height, 96);
   Camera.Apply;
   glScalef(1, -1, 1);
end;

end.
