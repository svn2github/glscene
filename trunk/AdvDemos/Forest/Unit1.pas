unit Unit1;

interface

uses
   Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
   GLWin32Viewer, GLCadencer, GLTexture, GLMisc, GLScene, GLNavigator,
   GLTerrainRenderer, GLHeightData, GLObjects, VectorGeometry, GLTree,
   GLL3DTHDS, L3DTFileIO, JPEG, TGA, Keyboard, VectorLists, GLBitmapFont,
   GLWindowsFont, GLHUDObjects, GLSkydome, GLImposter, GLParticleFX, GLGraphics,
   PersistentClasses, OpenGL1x, ExtCtrls, GLUtils;

type
   TForm1 = class(TForm)
      GLSceneViewer1: TGLSceneViewer;
      GLScene1: TGLScene;
      MLTrees: TGLMaterialLibrary;
      MLTerrain: TGLMaterialLibrary;
      GLCadencer1: TGLCadencer;
      Terrain: TGLTerrainRenderer;
      Camera: TGLCamera;
      Light: TGLLightSource;
      GLNavigator1: TGLNavigator;
      GLHUDText1: TGLHUDText;
      GLWindowsBitmapFont1: TGLWindowsBitmapFont;
      GLUserInterface1: TGLUserInterface;
      GLEarthSkyDome1: TGLEarthSkyDome;
    GLRenderPoint: TGLRenderPoint;
      GLStaticImposterBuilder1: TGLStaticImposterBuilder;
      DOTrees: TGLDirectOpenGL;
      PFXTrees: TGLCustomPFXManager;
      RenderTrees: TGLParticleFXRenderer;
      Timer1: TTimer;
      WaterPlane: TGLPlane;
      MLWater: TGLMaterialLibrary;
    Blended: TGLDummyCube;
      procedure FormCreate(Sender: TObject);
      procedure FormDestroy(Sender: TObject);
      procedure TerrainGetTerrainBounds(var l, t, r, b: Single);
      procedure GLCadencer1Progress(Sender: TObject; const deltaTime,
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
      procedure GLStaticImposterBuilder1ImposterLoaded(Sender: TObject;
        impostoredObject: TGLBaseSceneObject; destImposter: TImposter);
      function GLStaticImposterBuilder1LoadingImposter(Sender: TObject;
        impostoredObject: TGLBaseSceneObject;
        destImposter: TImposter): TGLBitmap32;
      procedure Timer1Timer(Sender: TObject);
      procedure PFXTreesProgress(Sender: TObject;
        const progressTime: TProgressTimes; var defaultProgress: Boolean);
      function PFXTreesGetParticleCountEvent(Sender: TObject): Integer;
    procedure BlendedProgress(Sender: TObject; const deltaTime,
      newTime: Double);
   private
      { Private declarations }
      hscale, vscale, mapwidth, mapheight : Single;
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
   L3DTHeightDataSource.PreLoad(0,0,Round(mapwidth),hdtSmallInt);
   WaterPlane.Width:=hscale*mapwidth;
   WaterPlane.Height:=hscale*mapwidth;

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
   TestTree:=TGLTree(GLScene1.Objects.AddNewChild(TGLTree));
   with TestTree do begin
      Visible:=False;
      MaterialLibrary:=MLTrees;
      LeafMaterialName:='Leaf';
      LeafBackMaterialName:='Leaf';
      BranchMaterialName:='Bark';
      Up.SetVector(0,0,1);
      Direction.SetVector(0,1,0);
      Depth:=9;
      BranchFacets:=5;
      LeafSize:=0.55;
      BranchAngle:=0.65;
      BranchTwist:=135;
      ForceTotalRebuild;
   end;

   GLStaticImposterBuilder1.RequestImposterFor(TestTree);

   Density:=TPicture.Create;
   try
      Density.LoadFromFile('media\treemap.jpg');
      densityBitmap:=TBitmap.Create;
      densityBitmap.PixelFormat:=pf24bit;
      densityBitmap.Width:=Density.Width;
      densityBitmap.Height:=Density.Height;
      densityBitmap.Canvas.Draw(0, 0, Density.Graphic);
   finally
      Density.Free;
   end;

   PFXTrees.CreateParticles(10000);
   TreesShown:=2000;

   densityBitmap.Free;

   Light.Pitch(30);
   Camera.Position.Y:=Terrain.InterpolatedHeight(Camera.Position.AsVector)+10;
   GLUserInterface1.MouseLookActive:=True;

   nearTrees:=TPersistentObjectList.Create;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
   nearTrees.Free;
   GLUserInterface1.MouseLookActive:=False;
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
   imposter:=GLStaticImposterBuilder1.ImposterFor(TestTree);
   imposter.BeginRender(rci);
end;

procedure TForm1.PFXTreesRenderParticle(Sender: TObject;
  aParticle: TGLParticle; var rci: TRenderContextInfo);
var
   d : Single;
   camPos : TVector;
begin
   if not IsVolumeClipped(aParticle.Position, 50, rci.rcci) then begin;
      VectorSubtract(rci.cameraPosition, aParticle.Position, camPos);
      d:=VectorNorm(camPos);
      if d>Sqr(200) then begin
         RotateVectorAroundY(PAffineVector(@camPos)^, aParticle.Tag*cPIdiv180);
         imposter.Render(rci, VectorMake(aParticle.Position), camPos, 10);
      end else begin
         nearTrees.Add(aParticle);
      end;
   end;
end;

procedure TForm1.PFXTreesEndParticles(Sender: TObject;
  var rci: TRenderContextInfo);
begin
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

procedure TForm1.GLCadencer1Progress(Sender: TObject; const deltaTime,
  newTime: Double);
var
   speed, z : Single;
begin
   // Camera movement
   speed:=25;
   if IsKeyDown(VK_SHIFT) then begin
      speed:=speed*10;
   end;
   if IsKeyDown(VK_UP) or IsKeyDown('W') or IsKeyDown('Z') then
      GLNavigator1.MoveForward(deltaTime*speed)
   else if IsKeyDown(VK_DOWN) or IsKeyDown('S') then
      GLNavigator1.MoveForward(-deltaTime*speed);
   if IsKeyDown(VK_LEFT) or IsKeyDown('A') or IsKeyDown('Q') then
      GLNavigator1.StrafeHorizontal(-deltaTime*speed)
   else if IsKeyDown(VK_RIGHT) or IsKeyDown('D') then
      GLNavigator1.StrafeHorizontal(deltaTime*speed);
   if IsKeyDown(VK_NEXT) then
      GLNavigator1.StrafeVertical(-deltaTime*speed)
   else if IsKeyDown(VK_PRIOR) then
      GLNavigator1.StrafeVertical(deltaTime*speed);

//   if Camera.Position.Y<Terrain.InterpolatedHeight(Camera.Position.AsVector)+3 then
   z:=Terrain.InterpolatedHeight(Camera.Position.AsVector);
   if z<0 then z:=0;
   Camera.Position.Y:=z+3;

   // Mouse update
   GLUserInterface1.MouseUpdate;
   GLUserInterface1.MouseLook;

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

function TForm1.GLStaticImposterBuilder1LoadingImposter(Sender: TObject;
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

procedure TForm1.GLStaticImposterBuilder1ImposterLoaded(Sender: TObject;
  impostoredObject: TGLBaseSceneObject; destImposter: TImposter);
var
   bmp32 : TGLBitmap32;
   bmp : TBitmap;
begin
   if Tag=1 then begin
      bmp32:=TGLBitmap32.Create;
      bmp32.AssignFromTexture2D(GLStaticImposterBuilder1.ImposterFor(TestTree).Texture);
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

procedure TForm1.BlendedProgress(Sender: TObject; const deltaTime,
  newTime: Double);
begin
   if (Camera.Position.Y<0) and not (Blended.Children[Blended.Count-1] = WaterPlane) then
      repeat WaterPlane.MoveDown until WaterPlane.Index = Blended.Count-1;
   if (Camera.Position.Y>0) and not (Blended.Children[0] = WaterPlane) then
      repeat WaterPlane.MoveUp until WaterPlane.Index = 0;
end;

end.
