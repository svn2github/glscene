// GLTerrainRenderer
{: GLScene's brute-force terrain renderer.<p>

   <b>History : </b><font size=-1><ul>
      <li>10/07/02 - EG - Added support for "holes" in the elevation data
      <li>16/06/02 - EG - Added support for multi-material terrains
      <li>24/02/02 - EG - Hybrid ROAM-stripifier engine
      <li>18/12/01 - EG - Vertex-cache aware stripifier (+10% on GeForce)
      <li>12/08/01 - EG - Completely rewritten handles management
      <li>21/07/01 - EG - Added Notication registration in SetHeightDataSource
      <li>04/03/01 - EG - Completed for first release
	   <li>12/02/01 - EG - Creation
	</ul></font><p>

   NOTA : multi-materials terrain support is not yet optimized to minimize
          texture switches (in case of resued tile textures).
}
unit GLTerrainRenderer;

interface

uses Classes, GLScene, GLHeightData, GLTexture, Geometry, GLContext, GLROAMPatch,
   VectorLists;

type

	// TTerrainRenderer
	//
   {: Basic terrain renderer.<p>
      This renderer uses no sophisticated meshing, it just builds and maintains
      a set of terrain tiles, performs basic visibility culling and renders its
      stuff. You can use it has a base class/sample for more specialized
      terrain renderers.<p>
      The Terrain heightdata is retrieved directly from a THeightDataSource, and
      expressed as z=f(x, y) data. }
	TTerrainRenderer = class (TGLSceneObject)
	   private
	      { Private Declarations }
         FHeightDataSource : THeightDataSource;
         FTileSize : Integer;
         FQualityDistance, FinvTileSize : Single;
         FLastTriangleCount : Integer;
         FTilesPerTexture : Single;
         FMaxCLODTriangles, FCLODPrecision : Integer;
         FBufferVertices : TAffineVectorList;
         FBufferTexPoints : TTexPointList;
         FBufferVertexIndices : TIntegerList;
         FMaterialLibrary : TGLMaterialLibrary;

	   protected
	      { Protected Declarations }
         FTilesHash : array [0..255] of TList;

         procedure MarkAllTilesAsUnused;
         procedure ReleaseAllUnusedTiles;
         procedure MarkHashedTileAsUsed(const tilePos : TAffineVector);
         function HashedTile(const tilePos : TAffineVector; canAllocate : Boolean = True) : THeightData; overload;
         function HashedTile(const xLeft, yTop : Integer; canAllocate : Boolean = True) : THeightData; overload;

         procedure SetHeightDataSource(const val : THeightDataSource);
         procedure SetTileSize(const val : Integer);
         procedure SetTilesPerTexture(const val : Single);
         procedure SetCLODPrecision(const val : Integer);
         procedure SetMaterialLibrary(const val : TGLMaterialLibrary);

         procedure Notification(AComponent: TComponent; Operation: TOperation); override;
			procedure DestroyHandle; override;

         procedure ReleaseAllTiles; dynamic;
         procedure OnTileDestroyed(sender : TObject); virtual;

         function GetPreparedPatch(const tilePos, eyePos : TAffineVector; texFactor : Single) : TGLROAMPatch;

	   public
	      { Public Declarations }
	      constructor Create(AOwner: TComponent); override;
         destructor Destroy; override;

			procedure BuildList(var rci : TRenderContextInfo); override;

         {: Interpolates height for the given point.<p>
            Expects a point expressed in absolute coordinates. }
         function InterpolatedHeight(const p : TVector) : Single; virtual;

         property LastTriangleCount : Integer read FLastTriangleCount;

	   published
	      { Published Declarations }

         {: Specifies the HeightData provider component. }
         property HeightDataSource : THeightDataSource read FHeightDataSource write SetHeightDataSource;
         {: Size of the terrain tiles.<p>
            Must be a power of two. }
         property TileSize : Integer read FTileSize write SetTileSize default 16;
         {: Number of tiles required for a full texture map. }
         property TilesPerTexture : Single read FTilesPerTexture write SetTilesPerTexture;
         {: Link to the material library holding terrain materials.<p>
            If unspecified, and for all terrain tiles with unspecified material,
            the terrain renderer's material is used. }
         property MaterialLibrary : TGLMaterialLibrary read FMaterialLibrary write SetMaterialLibrary;

         {: Quality distance hint.<p>
            This parameter gives an hint to the terrain renderer at which distance
            the terrain quality can be degraded to favor speed. The distance is
            expressed in absolute coordinates units.<p>
            All tiles closer than this distance are rendered without any LOD
            or mesh simplification. }
         property QualityDistance : Single read FQualityDistance write FQualityDistance;
         {: Maximum number of CLOD triangles per scene.<p>
            Triangles in high-resolution tiles (closer than QualityDistance) do
            not count toward this limit. }
         property MaxCLODTriangles : Integer read FMaxCLODTriangles write FMaxCLODTriangles default 65536;
         {: Precision of CLOD tiles.<p>
            The lower the value, the higher the precision and triangle count.
            Large values will result in coarse terrain.<br>
            high-resolution tiles (closer than QualityDistance) ignore this setting. }
         property CLODPrecision : Integer read FCLODPrecision write SetCLODPrecision default 100;
	end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

uses SysUtils, OpenGL12, GLMisc, XOpenGL;

// ------------------
// ------------------ TTerrainRenderer ------------------
// ------------------

// Create
//
constructor TTerrainRenderer.Create(AOwner: TComponent);
var
   i : Integer;
begin
	inherited Create(AOwner);
   for i:=0 to High(FTilesHash) do
      FTilesHash[i]:=TList.Create;
   ObjectStyle:=ObjectStyle+[osDirectDraw];
   FTileSize:=16;
   FinvTileSize:=1/16;
   FTilesPerTexture:=1;
   FMaxCLODTriangles:=65536;
   FCLODPrecision:=100;
   FBufferVertices:=TAffineVectorList.Create;
   FBufferTexPoints:=TTexPointList.Create;
   FBufferVertexIndices:=TIntegerList.Create;
end;

// Destroy
//
destructor TTerrainRenderer.Destroy;
var
   i : Integer;
begin
   FBufferVertices.Free;
   FBufferTexPoints.Free;
   FBufferVertexIndices.Free;
   ReleaseAllTiles;
   for i:=0 to High(FTilesHash) do
      FTilesHash[i]:=TList.Create;
	inherited Destroy;
end;

// Notification
//
procedure TTerrainRenderer.Notification(AComponent: TComponent; Operation: TOperation);
begin
   if Operation=opRemove then begin
      if AComponent=FHeightDataSource then
         HeightDataSource:=nil
      else if AComponent=FMaterialLibrary then
         MaterialLibrary:=nil;
   end;
   inherited;
end;

// DestroyHandle
//
procedure TTerrainRenderer.DestroyHandle;
begin
   inherited;
   ReleaseAllTiles;
   if Assigned(HeightDataSource) then
      HeightDataSource.Clear;
end;

// ReleaseAllTiles
//
procedure TTerrainRenderer.ReleaseAllTiles;
var
   i, k : Integer;
   hd : THeightData;
begin
   for i:=0 to High(FTilesHash) do with FTilesHash[i] do begin
      for k:=Count-1 downto 0 do begin
         hd:=THeightData(List[k]);
         hd.Release;
      end;
      Clear;
   end;
end;

// OnTileDestroyed
//
procedure TTerrainRenderer.OnTileDestroyed(sender : TObject);
var
   i : Integer;
begin
   with sender as THeightData do if ObjectTag<>nil then begin
      ObjectTag.Free;
      ObjectTag:=nil;
   end;
   for i:=0 to High(FTilesHash) do
      FTilesHash[i].Remove(Pointer(sender));
end;

// InterpolatedHeight
//
function TTerrainRenderer.InterpolatedHeight(const p : TVector) : Single;
var
   pLocal : TVector;
begin
   if Assigned(HeightDataSource) then begin
      pLocal:=AbsoluteToLocal(p);
      Result:=HeightDataSource.InterpolatedHeight(pLocal[0], pLocal[1])*Scale.Z*(1/128);
   end else Result:=0;
end;

// BuildList
//
procedure TTerrainRenderer.BuildList(var rci : TRenderContextInfo);
var
   vEye : TVector;
   tilePos, absTilePos, observer : TAffineVector;
   delta, n, rpIdxDelta : Integer;
   f, tileRadius, texFactor, maxTilePosX, tileDist, qDist : Single;
   patch, prevPatch : TGLROAMPatch;
   patchList, rowList, prevRow, buf : TList;
   rcci : TRenderContextClippingInfo;
   currentMaterialName : String;

   procedure ApplyMaterial(const materialName : String);
   begin
      if (MaterialLibrary<>nil) and (currentMaterialName<>materialName) then begin
         // unapply current
         if currentMaterialName='' then begin
            repeat
               // ... proper multipass support will be implemented later
            until not Material.UnApply(rci)
         end else begin
            repeat
               // ... proper multipass support will be implemented later
            until not MaterialLibrary.UnApplyMaterial(rci);
         end;
         // apply new
         if materialName='' then
            Material.Apply(rci)
         else MaterialLibrary.ApplyMaterial(materialName, rci);
         currentMaterialName:=materialName;
      end;
   end;

//   trackDetails : Boolean;
begin
   if csDesigning in ComponentState then Exit;
   if HeightDataSource=nil then Exit;
   currentMaterialName:='';
   // first project eye position into heightdata coordinates
   vEye:=VectorTransform(rci.cameraPosition, InvAbsoluteMatrix);
   SetVector(observer, vEye);
   vEye[0]:=Round(vEye[0]/TileSize-0.5)*TileSize+TileSize*0.5;
   vEye[1]:=Round(vEye[1]/TileSize-0.5)*TileSize+TileSize*0.5;
   tileRadius:=Sqrt(Sqr(TileSize*0.5*Scale.X)+Sqr(TileSize*0.5*Scale.Y)+Sqr(256*Scale.Z))*1.3;
   // now, we render a quad grid centered on eye position
   SetVector(tilePos, vEye);
   delta:=TileSize;
   tilePos[2]:=0;
   f:=(rci.rcci.farClippingDistance+tileRadius)/Scale.X;
   f:=Round(f/TileSize+1.0)*TileSize;
   maxTilePosX:=vEye[0]+f;
   texFactor:=1/(TilesPerTexture*TileSize);
   rcci:=rci.rcci;
   if QualityDistance>0 then
      qDist:=QualityDistance+tileRadius*0.5
   else qDist:=-1;
//   trackDetails:=Material.HasSecondaryTexture;

   SetROAMTrianglesCapacity(MaxCLODTriangles);
   n:=Sqr(TileSize+1)*2;
   FBufferVertices.Capacity:=n;
   FBufferTexPoints.Capacity:=n;

   glPushMatrix;
   glScalef(1, 1, 1/128);
   glTranslatef(-0.5*TileSize, -0.5*TileSize, 0);
   glEnableClientState(GL_VERTEX_ARRAY);
   xglEnableClientState(GL_TEXTURE_COORD_ARRAY);
   glDisableClientState(GL_COLOR_ARRAY);
   glDisableClientState(GL_NORMAL_ARRAY);

   glVertexPointer(3, GL_FLOAT, 0, FBufferVertices.List);
   xglTexCoordPointer(2, GL_FLOAT, 0, FBufferTexPoints.List);
   FLastTriangleCount:=0;

   patchList:=TList.Create;
   rowList:=TList.Create;
   prevRow:=TList.Create;

   MarkAllTilesAsUnused;

   tilePos[1]:=vEye[1]-f;
   while tilePos[1]<=vEye[1]+f do begin
      tilePos[0]:=vEye[0]-f;
      prevPatch:=nil;
      n:=0;
      while tilePos[0]<=maxTilePosX do begin
         absTilePos:=VectorTransform(tilePos, DirectAbsoluteMatrix^);
         if not IsVolumeClipped(absTilePos, tileRadius, rcci) then begin
            patch:=GetPreparedPatch(tilePos, observer, texFactor);

            if patch<>nil then begin

               tileDist:=VectorDistance(PAffineVector(@rcci.origin)^, absTilePos);
               patch.HighRes:=(tileDist<qDist);
   //            patch.NoDetails:=trackDetails and (tileDist>QualityDistance);

               if Assigned(prevPatch) then
                  patch.ConnectToTheWest(prevPatch);
               if prevRow.Count>n then
                  if (prevRow.List[n]<>nil) then
                     patch.ConnectToTheNorth(TGLROAMPatch(prevRow.List[n]));
               prevPatch:=patch;
               rowList.Add(patch);

               if patch.HighRes then begin
                  // high-res patches are issued immediately
                  ApplyMaterial(patch.HeightData.MaterialName);
                  patch.Render(FBufferVertices, FBufferVertexIndices, FBufferTexPoints);
                  FLastTriangleCount:=FLastTriangleCount+patch.TriangleCount;
               end else begin
                  // CLOD patches are issued after tesselation
                  patchList.Add(patch);
               end;
               
            end else begin

               prevPatch:=nil;
               rowList.Add(nil);
               
            end;
         end else begin
            MarkHashedTileAsUsed(tilePos);
            prevPatch:=nil;
            rowList.Add(nil);
         end;
         tilePos[0]:=tilePos[0]+delta;
         Inc(n);
      end;
      tilePos[1]:=tilePos[1]+delta;
      buf:=prevRow;
      prevRow:=rowList;
      rowList:=buf;
      rowList.Count:=0;
   end;

   rpIdxDelta:=Round(2*f/delta)+2;

   for n:=0 to patchList.Count-1+rpIdxDelta do begin
      if n<patchList.Count then begin
         patch:=TGLROAMPatch(patchList[n]);
         if Assigned(patch) then
            patch.Tesselate;
      end;
      if n>=rpIdxDelta then begin
         patch:=TGLROAMPatch(patchList[n-rpIdxDelta]);
         if Assigned(patch) then begin
            ApplyMaterial(patch.HeightData.MaterialName);
            patch.Render(FBufferVertices, FBufferVertexIndices, FBufferTexPoints);
            FLastTriangleCount:=FLastTriangleCount+patch.TriangleCount;
         end;
      end;
   end;

   glDisableClientState(GL_VERTEX_ARRAY);
   xglDisableClientState(GL_TEXTURE_COORD_ARRAY);

   glPopMatrix;

   ReleaseAllUnusedTiles;
   HeightDataSource.CleanUp;

   rowList.Free;
   prevRow.Free;
   patchList.Free;
   ApplyMaterial('');
end;

// MarkAllTilesAsUnused
//
procedure TTerrainRenderer.MarkAllTilesAsUnused;
var
   i, j : Integer;
begin
   for i:=Low(FTilesHash) to High(FTilesHash) do with FTilesHash[i] do
      for j:=Count-1 downto 0 do THeightData(List[j]).Tag:=0;
end;

// ReleaseAllUnusedTiles
//
procedure TTerrainRenderer.ReleaseAllUnusedTiles;
var
   i, j : Integer;
begin
   for i:=Low(FTilesHash) to High(FTilesHash) do with FTilesHash[i] do
      for j:=Count-1 downto 0 do with THeightData(List[j]) do
         if Tag=0 then begin
            Delete(j);
            Release;
         end;
end;

// MarkHashedTileAsUsed
//
procedure TTerrainRenderer.MarkHashedTileAsUsed(const tilePos : TAffineVector);
var
   hd : THeightData;
begin
   hd:=HashedTile(tilePos);
   if Assigned(hd) then hd.Tag:=1;
end;

// HashedTile
//
function TTerrainRenderer.HashedTile(const tilePos : TAffineVector; canAllocate : Boolean = True) : THeightData;
var
   xLeft, yTop : Integer;
begin
   xLeft:=Round(tilePos[0]/(TileSize)-0.5)*(TileSize);
   yTop:=Round(tilePos[1]/(TileSize)-0.5)*(TileSize);
   Result:=HashedTile(xLeft, yTop, canAllocate);
end;

// HashedTile
//
function TTerrainRenderer.HashedTile(const xLeft, yTop : Integer; canAllocate : Boolean = True) : THeightData;
var
   i, hash : Integer;
   hd : THeightData;
begin
   // is the tile already in our list?
   hash:=( xLeft+(xLeft shr 8)+(xLeft shr 16)
          +yTop+(yTop shr 8)+(yTop shr 16)) and 255;
   Result:=nil;
   with FTilesHash[hash] do begin
      for i:=0 to Count-1 do begin
         hd:=THeightData(List[i]);
         if (hd.XLeft=xLeft) and (hd.YTop=yTop) then begin
            Result:=hd;
            Break;
         end;
      end;
   end;
   // if not, request it
   if canAllocate and (not Assigned(Result)) then begin
      Result:=HeightDataSource.GetData(xLeft, yTop, TileSize+1, hdtByte);
      Result.RegisterUse;
      Result.OnDestroy:=OnTileDestroyed;
      if Result.DataState<>hdsNone then
         Result.DataType:=hdtSmallInt;
      FTilesHash[hash].Add(Result);
   end;
end;

// GetPreparedPatch
//
function TTerrainRenderer.GetPreparedPatch(const tilePos, eyePos : TAffineVector; texFactor : Single) : TGLROAMPatch;
var
   tile : THeightData;
   patch : TGLROAMPatch;
   xLeft, yTop : Integer;
begin
   xLeft:=Round(tilePos[0]*FinvTileSize-0.5)*TileSize;
   yTop:=Round(tilePos[1]*FinvTileSize-0.5)*TileSize;
   tile:=HashedTile(xLeft, yTop);
   if tile.DataState=hdsNone then begin
      Result:=nil;
   end else begin
      tile.Tag:=1;
      patch:=TGLROAMPatch(tile.ObjectTag);
      if not Assigned(patch) then begin
         // spawn ROAM patch
         patch:=TGLROAMPatch.Create;
         tile.ObjectTag:=patch;
         patch.HeightData:=tile;
         patch.VertexScale:=XYZVector;
         patch.VertexOffset:=tilePos;
         patch.TextureScale:=AffineVectorMake(texFactor, -texFactor, texFactor);
         patch.TextureOffset:=AffineVectorMake(xLeft*texFactor, 1-yTop*texFactor, 0);
         patch.ComputeVariance(FCLODPrecision);
      end;
      PAffineIntVector(@patch.ObserverPosition)[0]:=Round(eyePos[0]-tilePos[0]);
      PAffineIntVector(@patch.ObserverPosition)[1]:=Round(eyePos[1]-tilePos[1]);
      PAffineIntVector(@patch.ObserverPosition)[2]:=Round(eyePos[2]-tilePos[2]);
      patch.ResetTessellation;
      Result:=patch;
   end;
end;

// SetHeightDataSource
//
procedure TTerrainRenderer.SetHeightDataSource(const val : THeightDataSource);
begin
   if FHeightDataSource<>val then begin
      if Assigned(FHeightDataSource) then begin
         FHeightDataSource.RemoveFreeNotification(Self);
         ReleaseAllTiles;
         FHeightDataSource.Clear;
      end;
      FHeightDataSource:=val;
      if Assigned(FHeightDataSource) then
         FHeightDataSource.FreeNotification(Self);
      StructureChanged;
   end;
end;

// SetTileSize
//
procedure TTerrainRenderer.SetTileSize(const val : Integer);
begin
   if val<>FTileSize then begin
      if val<8 then
         FTileSize:=8
      else FTileSize:=RoundUpToPowerOf2(val);
      FinvTileSize:=1/FTileSize;
      ReleaseAllTiles;
      StructureChanged;
   end;
end;

// SetTilesPerTexture
//
procedure TTerrainRenderer.SetTilesPerTexture(const val : Single);
begin
   if val<>FTilesPerTexture then begin
      FTilesPerTexture:=val;
      StructureChanged;
   end;
end;

// SetCLODPrecision
//
procedure TTerrainRenderer.SetCLODPrecision(const val : Integer);
var
   i, k : Integer;
   hd : THeightData;
begin
   if val<>FCLODPrecision then begin
      FCLODPrecision:=val;
      // drop all ROAM data (CLOD has changed, rebuild required)
      for i:=0 to High(FTilesHash) do with FTilesHash[i] do begin
         for k:=Count-1 downto 0 do begin
            hd:=THeightData(List[k]);
            if Assigned(hd.ObjectTag) then begin
               (hd.ObjectTag as TGLROAMPatch).Free;
               hd.ObjectTag:=nil;
            end;
         end;
         Clear;
      end;
   end;
end;

// SetMaterialLibrary
//
procedure TTerrainRenderer.SetMaterialLibrary(const val : TGLMaterialLibrary);
begin
   if val<>FMaterialLibrary then begin
      FMaterialLibrary:=val;
      StructureChanged;
   end;
end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

	// class registrations
   RegisterClass(TTerrainRenderer);

end.
