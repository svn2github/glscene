unit Unit1;

interface

uses
   Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
   GLWin32Viewer, GLCadencer, GLTexture, GLMisc, GLScene, GLTerrainRenderer,
   GLHeightData, GLObjects, VectorGeometry, GLTree, GLL3DTHDS, L3DTFileIO,
   JPEG, TGA, Keyboard, VectorLists, GLBitmapFont,
   GLWindowsFont, GLHUDObjects, GLSkydome, GLImposter, GLParticleFX, GLGraphics,
   PersistentClasses, OpenGL1x, ExtCtrls, GLUtils, GLUserShader,
   GLTextureCombiners, XOpenGL;

type
   TForm1 = class(TForm)
      GLSceneViewer1: TGLSceneViewer;
    GLScene: TGLScene;
      MLTrees: TGLMaterialLibrary;
      MLTerrain: TGLMaterialLibrary;
    GLCadencer: TGLCadencer;
      Terrain: TGLTerrainRenderer;
      Camera: TGLCamera;
      Light: TGLLightSource;
      GLHUDText1: TGLHUDText;
      GLWindowsBitmapFont1: TGLWindowsBitmapFont;
      GLEarthSkyDome1: TGLEarthSkyDome;
    GLRenderPoint: TGLRenderPoint;
    SIBTree: TGLStaticImposterBuilder;
      DOTrees: TGLDirectOpenGL;
      PFXTrees: TGLCustomPFXManager;
      RenderTrees: TGLParticleFXRenderer;
      Timer1: TTimer;
      WaterPlane: TGLPlane;
      MLWater: TGLMaterialLibrary;
    WaterShader: TGLUserShader;
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
    procedure WaterShaderDoApply(Sender: TObject;
      var rci: TRenderContextInfo);
    procedure WaterShaderDoUnApply(Sender: TObject; Pass: Integer;
      var rci: TRenderContextInfo; var Continue: Boolean);
    procedure FormResize(Sender: TObject);
   private
      { Private declarations }
      hscale, vscale, mapwidth, mapheight : Single;
      lmp : TPoint;
      camPitch, camTurn, camTime, curPitch, curTurn : Single;
   public
      { Public declarations }
      L3DTHeightDataSource : TGLL3DTHDS;
      TestTree : TGLTree;
      TreesShown : Integer;
      nearTrees : TPersistentObjectList;
      imposter : TImposter;
      densityBitmap : TBitmap;
   end;

var
   Form1: TForm1;

implementation

{$R *.dfm}

const
   cImposterCacheFile : string = 'media\imposters.bmp';

procedure TForm1.FormCreate(Sender: TObject);
var
   heightmap : TL3DTHeightMap;
   Density : TPicture;
begin
  // Load L3DT map group
   L3DTHeightDataSource:=TGLL3DTHDS.Create(nil);
   hscale:=1;
   mapwidth:=0;
   mapheight:=0;
   with L3DTHeightDataSource do begin
      L3DTMapGroup.FileName:='media\trees.mgf';
      L3DTMapGroup.Load;
      MLTextures:=MLTerrain;
      heightmap:=TL3DTHeightMap(L3DTMapGroup.GetMapByPoint(0,0,TL3DTHeightMap));
      if Assigned(heightmap) then begin
         heightmap.Load;
         hscale:=heightmap.HorzScale;
         vscale:=heightmap.VertScale;
         mapwidth:=heightmap.Width;
         mapheight:=heightmap.Height;
         Terrain.Scale.SetVector(hscale,hscale,vscale*128);
         Terrain.Position.SetPoint(hscale*mapwidth/2,0,-hscale*mapheight/2);
      end;
   end;
   L3DTHeightDataSource.PreLoad(0, 0, Round(mapwidth), hdtSmallInt);
   WaterPlane.Width:=hscale*mapwidth*5;
   WaterPlane.Height:=hscale*mapwidth*5;

   with MLTerrain.AddTextureMaterial('Detail','media\detailmap.jpg') do begin
      Material.Texture.TextureMode:=tmModulate;
      TextureScale.SetVector(128,128,128);
   end;
   MLTerrain.Materials[0].Texture2Name:='Detail';

   // Assign height data source to the terrain renderer
   Terrain.HeightDataSource:=L3DTHeightDataSource;

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
         Density.LoadFromFile('media\treemap.jpg');
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
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
   ShowCursor(True);
   nearTrees.Free;
   L3DTHeightDataSource.Free;
end;

procedure TForm1.PFXTreesCreateParticle(Sender: TObject;
  aParticle: TGLParticle);
var
   u, v, p : Single;
begin
   repeat
      u:=Random*0.88+0.06;
      v:=Random*0.88+0.06;
      p:=((densityBitmap.Canvas.Pixels[Round(u*densityBitmap.Width),
                                       Round(v*densityBitmap.Height)] shr 8) and 255)/255;
   until p>ClampValue(Random, 0.3, 1);

   aParticle.PosX:=(0.5-u)*hscale*mapwidth-2;
   aParticle.PosY:=0;
   aParticle.PosZ:=(0.5-v)*hscale*mapheight;
   aParticle.PosY:=Terrain.InterpolatedHeight(aParticle.Position);
   aParticle.Tag:=Random(360);
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
      TestTree.Render(rci);
      glPopMatrix;
   end;
   nearTrees.Clear;
end;

procedure TForm1.TerrainGetTerrainBounds(var l, t, r, b: Single);
begin
  if L3DTHeightDataSource.L3DTMapGroup.IsLoaded then
    L3DTHeightDataSource.L3DTMapGroup.GetMapBounds(l,t,r,b);
end;

procedure TForm1.GLCadencerProgress(Sender: TObject; const deltaTime,
  newTime: Double);
var
   speed, z : Single;
   nmp : TPoint;
begin
   // Camera movement
   if IsKeyDown(VK_SHIFT) then
      speed:=deltaTime*150
   else speed:=deltaTime*15;

   if IsKeyDown(VK_UP) or IsKeyDown('W') or IsKeyDown('Z') then
      Camera.Move(speed)
   else if IsKeyDown(VK_DOWN) or IsKeyDown('S') then
      Camera.Move(-speed);
      
   if IsKeyDown(VK_LEFT) or IsKeyDown('A') or IsKeyDown('Q') then
      Camera.Slide(-speed)
   else if IsKeyDown(VK_RIGHT) or IsKeyDown('D') then
      Camera.Slide(speed);

   z:=Terrain.InterpolatedHeight(Camera.Position.AsVector);
   if z<0 then z:=0;
   if Camera.Position.Y<z+3 then
      Camera.Position.Y:=z+3;

   GetCursorPos(nmp);
   camTurn:=camTurn-(lmp.X-nmp.X)*0.2;
   camPitch:=camPitch+(lmp.Y-nmp.Y)*0.2;
   camTime:=camTime+deltaTime;
   while camTime>0 do begin
      curTurn:=Lerp(curTurn, camTurn, 0.2);
      curPitch:=Lerp(curPitch, camPitch, 0.2);
      Camera.Position.Y:=Lerp(Camera.Position.Y, z+3, 0.2);
      camTime:=camTime-0.01;
   end;
   Camera.ResetRotations;
   Camera.Turn(curTurn);
   Camera.Pitch(curPitch);
   SetCursorPos(lmp.X, lmp.Y);

   GLSceneViewer1.Invalidate;
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
   end;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
   GLHUDText1.Text:=Format('%.1f FPS - %d trees',
                           [GLSceneViewer1.FramesPerSecond, TreesShown]);
   GLSceneViewer1.ResetPerformanceMonitor;
   Caption:=Format('%.2f', [RenderTrees.LastSortTime]);
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

procedure TForm1.WaterShaderDoApply(Sender: TObject;
  var rci: TRenderContextInfo);
const
   cWaveScale = 7;
   cWaveSpeed = 0.02;
   cSinScale = 0.02;
var
   tex0Matrix, tex1Matrix : TMatrix;
   tWave : Single;
begin
   tWave:=GLCadencer.CurrentTime*cWaveSpeed;

   glMatrixMode(GL_TEXTURE);

   tex0Matrix:=IdentityHmgMatrix;
   tex0Matrix[0][0]:=3*cWaveScale;
   tex0Matrix[1][1]:=4*cWaveScale;
   tex0Matrix[3][0]:=tWave*1.1;
   tex0Matrix[3][1]:=tWave*1.06;
   glLoadMatrixf(@tex0Matrix);

   glActiveTextureARB(GL_TEXTURE1_ARB);

   tex1Matrix:=IdentityHmgMatrix;
   tex1Matrix[0][0]:=cWaveScale;
   tex1Matrix[1][1]:=cWaveScale;
   tex1Matrix[3][0]:=tWave*0.83;
   tex1Matrix[3][1]:=tWave*0.79;
   glLoadMatrixf(@tex1Matrix);
   glBindTexture(GL_TEXTURE_2D, MLWater.Materials[0].Material.Texture.Handle);
   glEnable(GL_TEXTURE_2D);

   glActiveTextureARB(GL_TEXTURE0_ARB);

   glMatrixMode(GL_MODELVIEW);

   SetupTextureCombiners( 'Tex0:=Tex1*Tex0;'#13#10
                         +'Tex1:=Tex0+Col;');
   xglMapTexCoordToDual;
end;

procedure TForm1.WaterShaderDoUnApply(Sender: TObject; Pass: Integer;
  var rci: TRenderContextInfo; var Continue: Boolean);
begin
   xglMapTexCoordToMain;

   glMatrixMode(GL_TEXTURE);

   glActiveTextureARB(GL_TEXTURE1_ARB);
   glDisable(GL_TEXTURE_2D);
   glLoadIdentity;

   glActiveTextureARB(GL_TEXTURE0_ARB);
   glLoadIdentity;

   glMatrixMode(GL_MODELVIEW);

   Continue:=False;
end;

procedure TForm1.FormResize(Sender: TObject);
begin
   Camera.FocalLength:=Width*50/800;
end;

end.
