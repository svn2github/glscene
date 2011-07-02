
// This unit is part of the GLScene Project, http://glscene.org

{: GLTerrainRenderer<p>

   GLScene's brute-force terrain renderer.<p>

   <b>History : </b><font size=-1><ul>
      <li>02/07/11 - DaStr - Added an experimental TGLVBOSimpleTerrainRenderer (thanks DungeoLords)
                             Reformated code
      <li>23/08/10 - Yar - Added OpenGLTokens to uses, replaced OpenGL1x functions to OpenGLAdapter
      <li>15/08/10 - Yar - Return missing part of code in BuildList
      <li>20/05/10 - Yar - Fixes for Linux x64
      <li>20/07/07 - LC - Fixed a problem when camera is far away from the terrain bounds.
                          (Bugtracker ID = 1757733)
      <li>30/03/07 - DaStr - Added $I GLScene.inc
      <li>28/03/07 - DaStr - Cosmetic fixes for FPC compatibility
      <li>27/03/07 - Lin- Added TileManagement flags. - Helps prevent tile cache fushes.
      <li>19/03/07 - Lin- Added IgnoredByRenderer flag to THeightData.
                          Helps manage duplicate tiles, when a dirty tile is being replaced.
      <li>16/03/07 - DaStr - Added explicit pointer dereferencing
                             (thanks Burkhard Carstens) (Bugtracker ID = 1678644)
      <li>08/02/07 - Lin- Ignore tiles that are not hdsReady (Prevents crashes when threading)
      <li>30/01/07 - Lin- Added HashedTileCount - Counts the tiles in the buffer
      <li>19/10/06 - LC - Changed the behaviour of OnMaxCLODTrianglesReached
      <li>09/10/06 - Lin- Added OnMaxCLODTrianglesReached event.(Rene Lindsay)
      <li>01/09/04 - SG - Fix for RayCastIntersect (Alan Rose)
      <li>25/04/04 - EG - Occlusion testing support
      <li>13/01/04 - EG - Leak fix (Phil Scadden)
      <li>05/11/03 - SG - Fixed minuscule bug in RayCastIntersect (thanks Michael)
      <li>06/02/03 - EG - Fixed speculative range computation, better hashkey
      <li>14/01/03 - EG - RayCastIntersect normals fix (Stuart Gooding)
      <li>24/09/02 - EG - Added RayCastIntersect (Stuart Gooding)
      <li>28/08/02 - EG - Now longer wrongly requests hdtByte (Phil Scadden),
                          Terrain bounds limiting event (Erazem Polutnik)
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

{$I GLScene.inc}

uses
  // VCL.
  Classes, Graphics,

  // GLScene.
  GLScene, GLHeightData, GLMaterial, VectorGeometry, GLContext,
  GLROAMPatch, VectorLists, GLRenderContextInfo;

const
  cTilesHashSize = 255;

type

  TGetTerrainBoundsEvent = procedure(var l, t, r, b: Single) of object;
  TPatchPostRenderEvent = procedure(var rci: TRenderContextInfo;
    const patches: TList) of object;
  THeightDataPostRenderEvent = procedure(var rci: TRenderContextInfo;
    const heightDatas: TList) of object;
  TMaxCLODTrianglesReachedEvent = procedure(var rci: TRenderContextInfo) of object;

  TTerrainHighResStyle = (hrsFullGeometry, hrsTesselated);
  TTerrainOcclusionTesselate = (totTesselateAlways, totTesselateIfVisible);

  TTileManagementFlag = (tmClearUsedFlags, tmMarkUsedTiles, tmReleaseUnusedTiles,
    tmAllocateNewTiles, tmWaitForPreparing);
  TTileManagementFlags = set of TTileManagementFlag;

  // TGLTerrainRenderer

   {: Basic terrain renderer.<p>
      This renderer uses no sophisticated meshing, it just builds and maintains
      a set of terrain tiles, performs basic visibility culling and renders its
      stuff. You can use it has a base class/sample for more specialized
      terrain renderers.<p>
      The Terrain heightdata is retrieved directly from a THeightDataSource, and
      expressed as z=f(x, y) data. }
  //TGLTerrainRenderer = class (TGLSceneObject)
  TGLTerrainRenderer = class(TGLSceneObject)
  private
    { Private Declarations }
    FHeightDataSource: THeightDataSource;
    FTileSize: Integer;
    FQualityDistance, FinvTileSize: Single;
    FLastTriangleCount: Integer;
    FTilesPerTexture: Single;
    FMaxCLODTriangles, FCLODPrecision: Integer;
    FBufferVertices: TAffineVectorList;
    FBufferTexPoints: TTexPointList;
    FBufferVertexIndices: TIntegerList;
    FMaterialLibrary: TGLMaterialLibrary;
    FOnGetTerrainBounds: TGetTerrainBoundsEvent;
    FOnPatchPostRender: TPatchPostRenderEvent;
    FOnHeightDataPostRender: THeightDataPostRenderEvent;
    FOnMaxCLODTrianglesReached: TMaxCLODTrianglesReachedEvent;

    FQualityStyle: TTerrainHighResStyle;
    FOcclusionFrameSkip: Integer;
    FOcclusionTesselate: TTerrainOcclusionTesselate;

  protected
    { Protected Declarations }
    FTilesHash: packed array [0..cTilesHashSize] of TList;

    procedure MarkAllTilesAsUnused;
    procedure ReleaseAllUnusedTiles;
    procedure MarkHashedTileAsUsed(const tilePos: TAffineVector);
    function HashedTile(const tilePos: TAffineVector;
      canAllocate: Boolean = True): THeightData; overload;
    function HashedTile(const xLeft, yTop: Integer;
      canAllocate: Boolean = True): THeightData; overload;

    procedure SetHeightDataSource(const Val: THeightDataSource);
    procedure SetTileSize(const Val: Integer);
    procedure SetTilesPerTexture(const Val: Single);
    procedure SetCLODPrecision(const Val: Integer);
    procedure SetMaterialLibrary(const Val: TGLMaterialLibrary);
    procedure SetQualityStyle(const Val: TTerrainHighResStyle);
    procedure SetOcclusionFrameSkip(Val: Integer);

    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure DestroyHandle; override;

    procedure ReleaseAllTiles; dynamic;
    procedure OnTileDestroyed(Sender: TObject); virtual;
    function GetPreparedPatch(const tilePos, eyePos: TAffineVector;
      texFactor: Single; hdList: TList): TGLROAMPatch;

  public
    { Public Declarations }

         {:TileManagement flags can be used to turn off various Tile cache management features.
          This helps to prevent unnecessary tile cache flushes, when rendering from multiple cameras.}
    TileManagement: TTileManagementFlags;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure BuildList(var rci: TRenderContextInfo); override;
    function RayCastIntersect(const rayStart, rayVector: TVector;
      intersectPoint: PVector = nil; intersectNormal: PVector = nil): Boolean;
      override;

         {: Interpolates height for the given point.<p>
            Expects a point expressed in absolute coordinates. }
    function InterpolatedHeight(const p: TVector): Single; overload; virtual;
    function InterpolatedHeight(const p: TAffineVector): Single; overload;
    {: Triangle count for the last render. }
    property LastTriangleCount: Integer read FLastTriangleCount;
    function HashedTileCount: Integer;

  published
    { Published Declarations }
    {: Specifies the HeightData provider component. }
    property HeightDataSource: THeightDataSource
      read FHeightDataSource write SetHeightDataSource;
         {: Size of the terrain tiles.<p>
            Must be a power of two. }
    property TileSize: Integer read FTileSize write SetTileSize default 16;
    {: Number of tiles required for a full texture map. }
    property TilesPerTexture: Single read FTilesPerTexture write SetTilesPerTexture;
         {: Link to the material library holding terrain materials.<p>
            If unspecified, and for all terrain tiles with unspecified material,
            the terrain renderer's material is used. }
    property MaterialLibrary: TGLMaterialLibrary
      read FMaterialLibrary write SetMaterialLibrary;

         {: Quality distance hint.<p>
            This parameter gives an hint to the terrain renderer at which distance
            the terrain quality can be degraded to favor speed. The distance is
            expressed in absolute coordinates units.<p>
            All tiles closer than this distance are rendered according to
            QualityStyle and with a static resolution. }
    property QualityDistance: Single read FQualityDistance write FQualityDistance;
         {: Determines how high-res tiles (closer than QualityDistance) are rendered.<p>
            hrsFullGeometry (default value) means that the high-res tiles are rendered
            with full-geometry, and no LOD of any kind, while hrsTesselated means
            the tiles will be tesselated once, with the best output for the
            CLODPrecision, and the result of that tesselation will be reused
            in further frames without any adpative tesselation. }
    property QualityStyle: TTerrainHighResStyle
      read FQualityStyle write SetQualityStyle default hrsFullGeometry;
         {: Maximum number of CLOD triangles per scene.<p>
            Triangles in high-resolution tiles (closer than QualityDistance) do
            not count toward this limit. }
    property MaxCLODTriangles: Integer read FMaxCLODTriangles
      write FMaxCLODTriangles default 65536;
         {: Precision of CLOD tiles.<p>
            The lower the value, the higher the precision and triangle count.
            Large values will result in coarse terrain.<br>
            high-resolution tiles (closer than QualityDistance) ignore this setting. }
    property CLODPrecision: Integer
      read FCLODPrecision write SetCLODPrecision default 100;
         {: Numbers of frames to skip for a tile when occlusion testing found it invisible.<p>
            Occlusion testing can help reduce CPU, T&L and fillrate requirements
            when tiles are occluded, either by the terrain itself (tiles behind
            a mountain or a cliff) or by geometry that was rendered before the
            terrain (large buildings). If there is little occlusion in your scene
            (such as in top down or high-altitude view), turning occlusion on
            may have a slightly negative effect on framerate.<br>
            It works by turning off rendering of tiles for the specified number
            of frames if it has been found invisible, after FrameSkip number
            of frames have been skipped, it will be rendered again, and a new
            occlusion testing made. This makes occlusion-testing a frame-to-frame
            coherency optimization, and as such, shouldn't be used for static
            rendering (ie. leave value to its default of zero).<br>
            This optimization requires the hardware to support GL_NV_occlusion_query. }
    property OcclusionFrameSkip: Integer read FOcclusionFrameSkip
      write SetOcclusionFrameSkip default 0;
         {: Determines if and how occlusion testing affects tesselation.<p>
            Turning off tesselation of tiles determined invisible can improve
            performance, however, it may result in glitches since the tesselation
            of an ivisible tile can have a slight effect on the tesselation
            of its adjacent tiles (by forcing higher resolution at the border
            for instance). This negative effect can be lessened by increasing
            the QualityDistance, so that glitches will apear farther away
            (this will mean increasing your triangle count though, so you'll
            trade CPU power against T&L power). }
    property OcclusionTesselate: TTerrainOcclusionTesselate
      read FOcclusionTesselate write FOcclusionTesselate default totTesselateIfVisible;

         {: Allows to specify terrain bounds.<p>
            Default rendering bounds will reach depth of view in all direction,
            with this event you can chose to specify a smaller rendered
            terrain area. }
    property OnGetTerrainBounds: TGetTerrainBoundsEvent
      read FOnGetTerrainBounds write FOnGetTerrainBounds;
         {: Invoked for each rendered patch after terrain render has completed.<p>
            The list holds TGLROAMPatch objects and allows per-patch
            post-processings, like waters, trees... It is invoked *before*
            OnHeightDataPostRender. }
    property OnPatchPostRender: TPatchPostRenderEvent
      read FOnPatchPostRender write FOnPatchPostRender;
         {: Invoked for each heightData not culled out by the terrain renderer.<p>
            The list holds THeightData objects and allows per-patch
            post-processings, like waters, trees... It is invoked *after*
            OnPatchPostRender. }
    property OnHeightDataPostRender: THeightDataPostRenderEvent
      read FOnHeightDataPostRender write FOnHeightDataPostRender;
         {: Invoked whenever the MaxCLODTriangles limit was reached during last rendering.<p>
            This forced the terrain renderer to resize the buffer, which affects performance.
            If this event is fired frequently, one should increase MaxCLODTriangles.
         }
    property OnMaxCLODTrianglesReached: TMaxCLODTrianglesReachedEvent
      read FOnMaxCLODTrianglesReached write FOnMaxCLODTrianglesReached;
  end;

  {: A fast an simple way to render terrain.
    Might give a preformance boost if you need to render all terrain at once.
    DaStr: 3 things trouble me:
    1) Normal TGLTerrainRenderer might still be faster in most cases.
    2) VBO or non-VBO should be a Global GLScene setting, not a component-specific extension.
    3) Accepts only bitmaps as input map type

    So consider this component experimental, which may be deleted at any time.
  }
  TGLVBOSimpleTerrainRenderer = class(TGLSceneObject)
  private
    FNeedToFreeMemory: Boolean;
    FVBOBuilt: Boolean;
    HMap: array of array of Byte;
    HMSize: Integer;

    FBufferVertices: TAffineVectorList;
    FNormals: TAffineVectorList;
    FTexCoords: TAffineVectorList;
    FBufferVertexIndices: TIntegerList;

    FVBOElementArrayHandle: TGLVBOElementArrayHandle;
    vId, nid, tid: TGLVBOArrayBufferHandle;
    procedure CreateAndFillTStripBuff(var Posi, Scale: TAffineVector);

  public
    procedure LoadHMap(const AFileName: string);

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure BuildList(var rci: TRenderContextInfo); override;

    property NeedToFreeMemory: Boolean read FNeedToFreeMemory write FNeedToFreeMemory;

    property BufferVertices: TAffineVectorList read FBufferVertices;
    property BufferVertexIndices: TIntegerList read FBufferVertexIndices;
  end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

uses SysUtils, OpenGLTokens, XOpenGL, GLUtils {$IFDEF GLS_DELPHI}, VectorTypes{$ENDIF};

// HashKey

function HashKey(const xLeft, yTop: Integer): Integer;
begin
  Result := (xLeft + (xLeft shr 8) + (xLeft shr 16) + (yTop shl 1) +
    (yTop shr 9) + (yTop shr 17)) and cTilesHashSize;
end;


// ------------------
// ------------------ TGLTerrainRenderer ------------------
// ------------------

// Create

constructor TGLTerrainRenderer.Create(AOwner: TComponent);
var
  I: Integer;
begin
  inherited Create(AOwner);
  for I := 0 to cTilesHashSize do
    FTilesHash[I] := TList.Create;
  ObjectStyle := ObjectStyle + [osDirectDraw];
  FTileSize := 16;
  FinvTileSize := 1 / 16;
  FTilesPerTexture := 1;
  FMaxCLODTriangles := 65536;
  FCLODPrecision := 100;
  FOcclusionTesselate := totTesselateIfVisible;
  FBufferVertices := TAffineVectorList.Create;
  FBufferTexPoints := TTexPointList.Create;
  FBufferVertexIndices := TIntegerList.Create;
  TileManagement := [tmClearUsedFlags, tmMarkUsedTiles, tmReleaseUnusedTiles,
    tmAllocateNewTiles];
end;

// Destroy

destructor TGLTerrainRenderer.Destroy;
var
  I: Integer;
begin
  FBufferVertices.Free;
  FBufferTexPoints.Free;
  FBufferVertexIndices.Free;
  ReleaseAllTiles;
  for I := 0 to cTilesHashSize do
  begin
    FTilesHash[I].Free;
    FTilesHash[I] := nil;
  end;
  inherited Destroy;
end;

// Notification

procedure TGLTerrainRenderer.Notification(AComponent: TComponent; Operation: TOperation);
begin
  if Operation = opRemove then
  begin
    if AComponent = FHeightDataSource then
      HeightDataSource := nil
    else if AComponent = FMaterialLibrary then
      MaterialLibrary := nil;
  end;
  inherited;
end;

// DestroyHandle

procedure TGLTerrainRenderer.DestroyHandle;
begin
  inherited;
  ReleaseAllTiles;
  if Assigned(HeightDataSource) then
    HeightDataSource.Clear;
end;

// RayCastIntersect

function TGLTerrainRenderer.RayCastIntersect(const rayStart, rayVector: TVector;
  intersectPoint: PVector = nil; intersectNormal: PVector = nil): Boolean;
var
  p1, d, p2, p3: TVector;
  step, I, h, minH, maxH, p1height: Single;
  startedAbove:  Boolean;
  failSafe:      Integer;
  AbsX, AbsY, AbsZ: TVector;
begin
  Result := False;
  if Assigned(HeightDataSource) then
  begin
    step := (Scale.X + Scale.Y); //Initial step size guess
    I := step;
    d := VectorNormalize(rayVector);
    AbsZ := VectorNormalize(LocalToAbsolute(ZHMGVector));
    startedAbove := ((InterpolatedHeight(rayStart) -
      VectorDotProduct(rayStart, AbsZ)) < 0);
    maxH := Scale.Z * 256;
    minH := -Scale.Z * 256;
    failSafe := 0;
    while True do
    begin
      p1 := VectorCombine(rayStart, d, 1, I);
      h := InterpolatedHeight(p1);
      p1height := VectorDotProduct(AbsZ, p1);
      if Abs(h - p1height) < 0.1 then
      begin //Need a tolerance variable here (how close is good enough?)
        Result := True;
        Break;
      end
      else
      begin
        if startedAbove then
        begin
          if h < p1height then
            I := I + step;
          if (h - p1height) > 0 then
          begin
            step := step * 0.5;
            I := I - step;
          end;
        end
        else
        begin
          if h > p1height then
            I := I + step;
        end;
      end;
      Inc(failSafe);
      if failSafe > 1024 then
        Break;
      if VectorDotProduct(AbsZ, d) < 0 then
      begin
        if h < minH then
          Exit;
      end
      else if h > maxH then
        Exit;
    end;

    if Result then
    begin
      p1 := VectorAdd(p1, VectorScale(AbsZ, InterpolatedHeight(p1) -
        VectorDotProduct(p1, AbsZ)));
      if Assigned(intersectPoint) then
        intersectPoint^ := p1;

      // Calc Normal
      if Assigned(intersectNormal) then
      begin
        // Get 2 nearby points for cross-product
        AbsX := VectorNormalize(LocalToAbsolute(XHMGVector));
        AbsY := VectorNormalize(LocalToAbsolute(YHMGVector));
        p2 := VectorAdd(p1, VectorScale(AbsX, 0.1));
        p2 := VectorAdd(p2, VectorScale(AbsZ,
          InterpolatedHeight(p2) - VectorDotProduct(p2, AbsZ)));
        p3 := VectorAdd(p1, VectorScale(AbsY, 0.1));
        p3 := VectorAdd(p3, VectorScale(AbsZ,
          InterpolatedHeight(p3) - VectorDotProduct(p3, AbsZ)));

        intersectNormal^ :=
          VectorNormalize(VectorCrossProduct(VectorSubtract(p1, p2),
          VectorSubtract(
          p3, p1)));
      end;
    end;
  end;
end;

// ReleaseAllTiles

procedure TGLTerrainRenderer.ReleaseAllTiles;
var
  I, K: Integer;
  hd:   THeightData;
begin
  for I := 0 to cTilesHashSize do
    with FTilesHash[I] do
    begin
      for K := Count - 1 downto 0 do
      begin
        hd := THeightData(List^[K]);
        OnTileDestroyed(hd);
        hd.OnDestroy := nil;
        hd.Release;
      end;
      Clear;
    end;
end;

// OnTileDestroyed

procedure TGLTerrainRenderer.OnTileDestroyed(Sender: TObject);
var
  list: TList;
begin
  with Sender as THeightData do
  begin
    if ObjectTag <> nil then
    begin
      ObjectTag.Free;
      ObjectTag := nil;
    end;
    list := FTilesHash[HashKey(XLeft, YTop)];
    Assert(Assigned(list));
    list.Remove(Sender);
  end;
end;

// InterpolatedHeight (hmg)

function TGLTerrainRenderer.InterpolatedHeight(const p: TVector): Single;
var
  pLocal: TVector;
begin
  if Assigned(HeightDataSource) then
  begin
    pLocal := AbsoluteToLocal(p);
    Result := HeightDataSource.InterpolatedHeight(pLocal[0], pLocal[1],
      TileSize + 1) * Scale.Z * (1 / 128);
  end
  else
    Result := 0;
end;

// InterpolatedHeight (affine)

function TGLTerrainRenderer.InterpolatedHeight(const p: TAffineVector): Single;
begin
  Result := InterpolatedHeight(PointMake(p));
end;

// BuildList

procedure TGLTerrainRenderer.BuildList(var rci: TRenderContextInfo);
var
  vEye, vEyeDirection: TVector;
  tilePos, absTilePos, observer: TAffineVector;
  deltaX, nbX, iX: Integer;
  deltaY, nbY, iY: Integer;
  n, rpIdxDelta, accumCount: Integer;
  f, tileRadius, tileGroundRadius, texFactor, tileDist, qDist: Single;
  patch, prevPatch: TGLROAMPatch;
  patchList, rowList, prevRow, buf: TList;
  postRenderPatchList, postRenderHeightDataList: TList;
  rcci: TRenderContextClippingInfo;
  currentMaterialName: string;
  maxTilePosX, maxTilePosY, minTilePosX, minTilePosY: Single;
  t_l, t_t, t_r, t_b: Single;

  procedure ApplyMaterial(const materialName: string);
  begin
    if (MaterialLibrary = nil) or (currentMaterialName = materialName) then
      Exit;
    // flush whatever is in progress
    TGLROAMPatch.FlushAccum(FBufferVertices, FBufferVertexIndices, FBufferTexPoints);
    // unapply current
    if currentMaterialName = '' then
    begin
      repeat
        // ... proper multipass support will be implemented later
      until not Material.UnApply(rci);
    end
    else
    begin
      repeat
        // ... proper multipass support will be implemented later
      until not MaterialLibrary.UnApplyMaterial(rci);
    end;
    // apply new
    if materialName = '' then
      Material.Apply(rci)
    else
      MaterialLibrary.ApplyMaterial(materialName, rci);
    currentMaterialName := materialName;
  end;

begin
  if csDesigning in ComponentState then
    Exit;
  if HeightDataSource = nil then
    Exit;

  currentMaterialName := '';
  // first project eye position into heightdata coordinates
  vEye := VectorTransform(rci.cameraPosition, InvAbsoluteMatrix);
  vEyeDirection := VectorTransform(rci.cameraDirection, InvAbsoluteMatrix);
  SetVector(observer, vEye);
  vEye[0] := Round(vEye[0] * FinvTileSize - 0.5) * TileSize + TileSize * 0.5;
  vEye[1] := Round(vEye[1] * FinvTileSize - 0.5) * TileSize + TileSize * 0.5;
  tileGroundRadius := Sqr(TileSize * 0.5 * Scale.X) + Sqr(TileSize * 0.5 * Scale.Y);
  tileRadius := Sqrt(tileGroundRadius + Sqr(256 * Scale.Z));
  tileGroundRadius := Sqrt(tileGroundRadius);
  // now, we render a quad grid centered on eye position
  SetVector(tilePos, vEye);
  tilePos[2] := 0;
  f := (rci.rcci.farClippingDistance + tileGroundRadius) / Scale.X;
  f := Round(f * FinvTileSize + 1.0) * TileSize;
  maxTilePosX := vEye[0] + f;
  maxTilePosY := vEye[1] + f;
  minTilePosX := vEye[0] - f;
  minTilePosY := vEye[1] - f;

  if Assigned(FOnGetTerrainBounds) then
  begin
    // User-specified terrain bounds, may override ours
    t_l := minTilePosX;
    t_t := maxTilePosY;
    t_r := maxTilePosX;
    t_b := minTilePosY;

    FOnGetTerrainBounds(t_l, t_t, t_r, t_b);

    t_l := Round(t_l / TileSize - 0.5) * TileSize + TileSize * 0.5;
    t_t := Round(t_t / TileSize - 0.5) * TileSize - TileSize * 0.5;
    t_r := Round(t_r / TileSize - 0.5) * TileSize - TileSize * 0.5;
    t_b := Round(t_b / TileSize - 0.5) * TileSize + TileSize * 0.5;

    if maxTilePosX > t_r then
      maxTilePosX := t_r;
    if maxTilePosY > t_t then
      maxTilePosY := t_t;
    if minTilePosX < t_l then
      minTilePosX := t_l;
    if minTilePosY < t_b then
      minTilePosY := t_b;
  end;
  // if max is less than min, we have nothing to render
  if (maxTilePosX < minTilePosX) or (maxTilePosY < minTilePosY) then
    Exit;

  nbX := Round((maxTilePosX - minTilePosX) / TileSize);
  nbY := Round((maxTilePosY - minTilePosY) / TileSize);


  texFactor := 1 / (TilesPerTexture * TileSize);
  rcci := rci.rcci;
  if QualityDistance > 0 then
    qDist := QualityDistance + tileRadius * 0.5
  else
    qDist := -1;

  SetROAMTrianglesCapacity(MaxCLODTriangles);
  n := MaxInteger(MaxCLODTriangles * 2, Integer(Sqr(TileSize + 1) * 2));
  FBufferVertices.Capacity := n;
  FBufferTexPoints.Capacity := n;

  xgl.PushState;
  try
    if GL.ARB_multitexture then
      xgl.MapTexCoordToDual
    else
      xgl.MapTexCoordToMain;

    GL.PushMatrix;
    GL.Scalef(1, 1, 1 / 128);
    GL.Translatef(-0.5 * TileSize, -0.5 * TileSize, 0);
    GL.EnableClientState(GL_VERTEX_ARRAY);
    xgl.EnableClientState(GL_TEXTURE_COORD_ARRAY);
    GL.DisableClientState(GL_COLOR_ARRAY);
    GL.DisableClientState(GL_NORMAL_ARRAY);

    GL.VertexPointer(3, GL_FLOAT, 0, FBufferVertices.List);
    xgl.TexCoordPointer(2, GL_FLOAT, 0, FBufferTexPoints.List);
  finally
    xgl.PopState;
  end;

  HeightDataSource.Data.LockList;  //Lock out the HDS thread while rendering

  FLastTriangleCount := 0;
  patchList := TList.Create;
  patchList.Capacity := (nbX + 1) * (nbY + 1);
  rowList := TList.Create;
  prevRow := TList.Create;
  if Assigned(FOnPatchPostRender) then
    postRenderPatchList := TList.Create
  else
    postRenderPatchList := nil;
  if Assigned(FOnHeightDataPostRender) then
    postRenderHeightDataList := TList.Create
  else
    postRenderHeightDataList := nil;

  MarkAllTilesAsUnused;
  AbsoluteMatrix; // makes sure it is available

  // determine orientation (to render front-to-back)
  if vEyeDirection[0] >= 0 then
    deltaX := TileSize
  else
  begin
    deltaX := -TileSize;
    minTilePosX := maxTilePosX;
  end;
  if vEyeDirection[1] >= 0 then
    deltaY := TileSize
  else
  begin
    deltaY := -TileSize;
    minTilePosY := maxTilePosY;
  end;

  tileRadius := tileRadius;

  tilePos[1] := minTilePosY;
  for iY := 0 to nbY - 1 do
  begin
    tilePos[0] := minTilePosX;
    prevPatch := nil;
    n := 0;
    for iX := 0 to nbX do
    begin
      absTilePos := VectorTransform(tilePos, DirectAbsoluteMatrix^);
      if not IsVolumeClipped(absTilePos, tileRadius, rcci.frustum) then
      begin
        patch := GetPreparedPatch(tilePos, observer, texFactor,
          postRenderHeightDataList);

        if patch <> nil then
        begin

          tileDist := VectorDistance(PAffineVector(@rcci.origin)^, absTilePos);
          patch.HighRes := (tileDist < qDist);

          if not patch.HighRes then
            patch.ResetTessellation;
          if Assigned(prevPatch) then
          begin
            if deltaX > 0 then
              patch.ConnectToTheWest(prevPatch)
            else
              prevPatch.ConnectToTheWest(patch);
          end;
          if (prevRow.Count > n) and (prevRow.List^[n] <> nil) then
          begin
            if deltaY > 0 then
              patch.ConnectToTheNorth(TGLROAMPatch(prevRow.List^[n]))
            else
              TGLROAMPatch(prevRow.List^[n]).ConnectToTheNorth(patch);
          end;

          if patch.HighRes then
          begin
            // high-res patches are issued immediately
            ApplyMaterial(patch.HeightData.MaterialName);
            patch.RenderHighRes(FBufferVertices, FBufferVertexIndices,
              FBufferTexPoints,
              (QualityStyle = hrsTesselated));
            FLastTriangleCount := FLastTriangleCount + patch.TriangleCount;
          end
          else
          begin
            // CLOD patches are issued after tesselation
            patchList.Add(patch);
          end;

          prevPatch := patch;
          rowList.Add(patch);

          if Assigned(postRenderPatchList) then
            postRenderPatchList.Add(patch);
        end
        else
        begin
          prevPatch := nil;
          rowList.Add(nil);
        end;
      end
      else
      begin
        MarkHashedTileAsUsed(tilePos);
        prevPatch := nil;
        rowList.Add(nil);
      end;
      tilePos[0] := tilePos[0] + deltaX;
      Inc(n);
    end;
    tilePos[1] := tilePos[1] + deltaY;
    buf := prevRow;
    prevRow := rowList;
    rowList := buf;
    rowList.Count := 0;
  end;

  accumCount := FBufferVertexIndices.Capacity shr 3;

  // Interleave Tesselate and Render so we can send some work to the hardware
  // while the CPU keeps working
  rpIdxDelta := Round(2 * f / TileSize) + 2;
  for n := 0 to patchList.Count - 1 + rpIdxDelta do
  begin
    if n < patchList.Count then
    begin
      patch := TGLROAMPatch(patchList[n]);
      if Assigned(patch) then
      begin
        if (patch.LastOcclusionTestPassed) or
          (patch.OcclusionCounter <= 0) or (OcclusionTesselate = totTesselateAlways) then
          patch.SafeTesselate;
      end;
    end;
    if n >= rpIdxDelta then
    begin
      patch := TGLROAMPatch(patchList[n - rpIdxDelta]);
      if Assigned(patch) then
      begin
        ApplyMaterial(patch.HeightData.MaterialName);
        patch.RenderAccum(FBufferVertices, FBufferVertexIndices, FBufferTexPoints,
          accumCount);
        Inc(FLastTriangleCount, patch.TriangleCount);
      end;
    end;
  end;

  if (GetROAMTrianglesCapacity > MaxCLODTriangles) and
    Assigned(FOnMaxCLODTrianglesReached) then
  begin
    FOnMaxCLODTrianglesReached(rci);
    //Fire an event if the MaxCLODTriangles limit was reached
  end;

  TGLROAMPatch.FlushAccum(FBufferVertices, FBufferVertexIndices, FBufferTexPoints);

  xgl.PushState;
  try
    if GL.ARB_multitexture then
      xgl.MapTexCoordToDual
    else
      xgl.MapTexCoordToMain;

    GL.DisableClientState(GL_VERTEX_ARRAY);
    xgl.DisableClientState(GL_TEXTURE_COORD_ARRAY);
  finally
    xgl.PopState;
  end;

  ApplyMaterial('');
  if Assigned(postRenderPatchList) then
  begin
    FOnPatchPostRender(rci, postRenderPatchList);
    postRenderPatchList.Free;
  end;
  if Assigned(postRenderHeightDataList) then
  begin
    FOnHeightDataPostRender(rci, postRenderHeightDataList);
    postRenderHeightDataList.Free;
  end;

  GL.PopMatrix;

  if (tmReleaseUnusedTiles in TileManagement) then
  begin  //Tile cache management option
    ReleaseAllUnusedTiles;
    HeightDataSource.CleanUp;
  end;

  rowList.Free;
  prevRow.Free;
  patchList.Free;

  HeightDataSource.Data.UnLockList;
end;


// MarkAllTilesAsUnused
procedure TGLTerrainRenderer.MarkAllTilesAsUnused;
var
  I, J, zero: Integer;
  pList:      PPointerList;
begin
  if not (tmClearUsedFlags in TileManagement) then
    Exit;  //Tile cache management option
  for I := 0 to cTilesHashSize do
    with FTilesHash[I] do
    begin
      pList := List;
      zero := 0;
      for J := Count - 1 downto 0 do
        THeightData(pList^[J]).Tag := zero;
    end;
end;

// ReleaseAllUnusedTiles
procedure TGLTerrainRenderer.ReleaseAllUnusedTiles;
var
  I, J: Integer;
  hashList: TList;
  hd: THeightData;
begin
  for I := 0 to cTilesHashSize do
  begin
    hashList := FTilesHash[I];
    for J := hashList.Count - 1 downto 0 do
    begin
      hd := THeightData(hashList.List^[J]);
      if hd.Tag = 0 then
      begin
        hashList.Delete(J);
        OnTileDestroyed(hd);
        hd.OnDestroy := nil;
        hd.Release;
      end;
    end;
  end;
end;

//HashedTileCount

function TGLTerrainRenderer.HashedTileCount: Integer;
var
  I:   Integer;
  hashList: TList;
  cnt: Integer;
begin
  cnt := 0;
  for I := 0 to cTilesHashSize do
  begin
    hashList := FTilesHash[I]; //get the number of tiles in each list
    cnt := cnt + hashList.Count; //Add the current list's count to the total
  end;
  Result := cnt;
end;


// MarkHashedTileAsUsed

procedure TGLTerrainRenderer.MarkHashedTileAsUsed(const tilePos: TAffineVector);
var
  hd: THeightData;
  canAllocate: Boolean;
begin
  if not (tmMarkUsedTiles in TileManagement) then
    Exit;  //Mark used tiles option
  canAllocate := tmAllocateNewTiles in TileManagement;
  //Allocate tile if not in the list
  hd := HashedTile(tilePos, canAllocate);
  if Assigned(hd) then
    hd.Tag := 1;
end;

// HashedTile

function TGLTerrainRenderer.HashedTile(const tilePos: TAffineVector;
  canAllocate: Boolean = True): THeightData;
var
  xLeft, yTop: Integer;
begin
  xLeft := Round(tilePos[0] * FinvTileSize - 0.5) * (TileSize);
  yTop := Round(tilePos[1] * FinvTileSize - 0.5) * (TileSize);
  Result := HashedTile(xLeft, yTop, canAllocate);
end;

// HashedTile

function TGLTerrainRenderer.HashedTile(const xLeft, yTop: Integer;
  canAllocate: Boolean = True): THeightData;
var
  I:     Integer;
  hd:    THeightData;
  hashList: TList;
  pList: PPointerList;
begin
  // is the tile already in our list?
  hashList := FTilesHash[HashKey(xLeft, yTop)];
  pList := hashList.List;
  for I := hashList.Count - 1 downto 0 do
  begin
    hd := THeightData(pList^[I]);
    if (hd.XLeft = xLeft) and (hd.YTop = yTop) then
    begin
      if hd.DontUse then
      begin
        hashlist.Remove(hd);
        //This tile has now been replaced. Remove it from the hash-table.
      end
      else
      begin
        Result := hd;
        Exit;
      end;
    end;
  end;
  // if not, request it
  if canAllocate then
  begin
    Result := HeightDataSource.GetData(xLeft, yTop, TileSize + 1, hdtSmallInt);
    Result.RegisterUse;
    Result.OnDestroy := OnTileDestroyed;
    if Result.DataState <> hdsNone then
      Result.DataType := hdtSmallInt;
    FTilesHash[HashKey(xLeft, yTop)].Add(Result);
  end
  else
    Result := nil;
end;

// GetPreparedPatch

function TGLTerrainRenderer.GetPreparedPatch(const tilePos, eyePos: TAffineVector;
  texFactor: Single; hdList: TList): TGLROAMPatch;
var
  tile:  THeightData;
  patch: TGLROAMPatch;
  xLeft, yTop: Integer;
  canAllocate: Boolean;
begin
  canAllocate := tmAllocateNewTiles in TileManagement;
  xLeft := Round(tilePos[0] * FinvTileSize - 0.5) * TileSize;
  yTop := Round(tilePos[1] * FinvTileSize - 0.5) * TileSize;
  tile := HashedTile(xLeft, yTop, canAllocate);
  Result := nil;
  if not assigned(tile) then
    Exit;

  if (tmClearUsedFlags in TileManagement) //Tile cache management option
  then
    tile.Tag := 1; //mark tile as used
  if Assigned(hdList) then
    hdList.Add(tile);

  //if tile.DataState=hdsNone then begin
  if tile.DataState <> hdsReady then
  begin
    Result := nil;                //if the tile is still not hdsReady, then skip it
  end
  else
  begin
    patch := TGLROAMPatch(tile.ObjectTag);
    if not Assigned(patch) then
    begin
      // spawn ROAM patch
      patch := TGLROAMPatch.Create;
      tile.ObjectTag := patch;
      patch.HeightData := tile;
      patch.VertexScale := XYZVector;
      patch.VertexOffset := tilePos;
      patch.OcclusionSkip := OcclusionFrameSkip;
      case tile.TextureCoordinatesMode of
        tcmWorld:
        begin
          patch.TextureScale := AffineVectorMake(texFactor, -texFactor, texFactor);
          patch.TextureOffset :=
            AffineVectorMake(xLeft * texFactor, 1 - yTop * texFactor, 0);
        end;
        tcmLocal:
        begin
          with tile.TextureCoordinatesScale do
            patch.TextureScale :=
              AffineVectorMake(texFactor * S, -texFactor * T, texFactor);
          with tile.TextureCoordinatesOffset do
            patch.TextureOffset := AffineVectorMake(0 + S, 1 + T, 0);
        end;
        else
          Assert(False);
      end;
      patch.ComputeVariance(FCLODPrecision);
    end;
    patch.ObserverPosition := VectorSubtract(eyePos, tilePos);
    Result := patch;
  end;
end;

// SetHeightDataSource

procedure TGLTerrainRenderer.SetHeightDataSource(const Val: THeightDataSource);
begin
  if FHeightDataSource <> Val then
  begin
    if Assigned(FHeightDataSource) then
    begin
      FHeightDataSource.RemoveFreeNotification(Self);
      ReleaseAllTiles;
      FHeightDataSource.Clear;
    end;
    FHeightDataSource := Val;
    if Assigned(FHeightDataSource) then
      FHeightDataSource.FreeNotification(Self);
    StructureChanged;
  end;
end;

// SetTileSize

procedure TGLTerrainRenderer.SetTileSize(const Val: Integer);
begin
  if Val <> FTileSize then
  begin
    if Val < 8 then
      FTileSize := 8
    else
      FTileSize := RoundUpToPowerOf2(Val);
    FinvTileSize := 1 / FTileSize;
    ReleaseAllTiles;
    StructureChanged;
  end;
end;

// SetTilesPerTexture

procedure TGLTerrainRenderer.SetTilesPerTexture(const Val: Single);
begin
  if Val <> FTilesPerTexture then
  begin
    FTilesPerTexture := Val;
    StructureChanged;
  end;
end;

// SetCLODPrecision

procedure TGLTerrainRenderer.SetCLODPrecision(const Val: Integer);
var
  I, K: Integer;
  hd:   THeightData;
begin
  if Val <> FCLODPrecision then
  begin
    FCLODPrecision := Val;
    if FCLODPrecision < 1 then
      FCLODPrecision := 1;
    // drop all ROAM data (CLOD has changed, rebuild required)
    for I := 0 to cTilesHashSize do
      with FTilesHash[I] do
      begin
        for K := Count - 1 downto 0 do
        begin
          hd := THeightData(List^[K]);
          if Assigned(hd.ObjectTag) then
          begin
            (hd.ObjectTag as TGLROAMPatch).Free;
            hd.ObjectTag := nil;
          end;
        end;
        Clear;
      end;
  end;
end;

// SetMaterialLibrary

procedure TGLTerrainRenderer.SetMaterialLibrary(const Val: TGLMaterialLibrary);
begin
  if Val <> FMaterialLibrary then
  begin
    FMaterialLibrary := Val;
    StructureChanged;
  end;
end;

// SetQualityStyle

procedure TGLTerrainRenderer.SetQualityStyle(const Val: TTerrainHighResStyle);
begin
  if Val <> FQualityStyle then
  begin
    FQualityStyle := Val;
    StructureChanged;
  end;
end;

// SetOcclusionFrameSkip

procedure TGLTerrainRenderer.SetOcclusionFrameSkip(Val: Integer);
var
  I, K: Integer;
  hd:   THeightData;
begin
  if Val < 0 then
    Val := 0;
  if FOcclusionFrameSkip <> Val then
  begin
    FOcclusionFrameSkip := Val;
    for I := 0 to cTilesHashSize do
      with FTilesHash[I] do
      begin
        for K := Count - 1 downto 0 do
        begin
          hd := THeightData(List^[K]);
          if hd.ObjectTag <> nil then
            TGLROAMPatch(hd.ObjectTag).OcclusionSkip := OcclusionFrameSkip;
        end;
      end;
    NotifyChange(Self);
  end;
end;


constructor TGLVBOSimpleTerrainRenderer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

destructor TGLVBOSimpleTerrainRenderer.Destroy;
begin
  if not FNeedToFreeMemory then
  begin
    FBufferVertices.Free;
    FNormals.Free;
    FTexCoords.Free;
    FBufferVertexIndices.Free;
  end;

  vID.Free;
  nId.Free;
  tId.Free;
  FVBOElementArrayHandle.Free;
  inherited;
end;

procedure TGLVBOSimpleTerrainRenderer.LoadHMap(const AFileName: string);
var
  bmp:  TBitmap;
  I, J: Integer;
  p:    PByteArray;
  bpp:  Byte;

  c: Integer;
  scal, posi: TAffineVector;
begin
  FVBOBuilt := False;

  bmp := TBitmap.Create;
  bmp.LoadFromFile(AFileName);
  case bmp.PixelFormat of
    pf24bit: bpp := 3;
    pf32bit: bpp := 4;
    else
      bpp := 1;
  end;
  // HMSize:=min(bmp.Width,bmp.Height); :
  if bmp.Width < bmp.Height then
    HMSize := bmp.Width
  else
    HMSize := bmp.Height;

  SetLength(hMap, HMSize, HMSize);
  for I := 0 to HMSize - 1 do
  begin
    p := bmp.ScanLine[I];
    for J := 0 to HMSize - 1 do
      hMap[I, J] := p[J * bpp];
  end;
  bmp.Free;

  c := HMSize div 2;

  setvector(posi, -c, 0, -c);
  setvector(scal, 1, 0.2, 1);

  CreateAndFillTStripBuff(posi, scal);
end;

procedure TGLVBOSimpleTerrainRenderer.CreateAndFillTStripBuff(var Posi: TVector3f;
  var Scale: TVector3f);
var
  I, J:    Integer;
  v, t, n: TAffineVector;
  i1, i2:  Integer;
  v1, v2, v3: TAffineVector;
begin
  FBufferVertices := TAffineVectorList.Create;
  FTexCoords := TAffineVectorList.Create;
  FNormals := TAffineVectorList.Create;
  FBufferVertexIndices := TIntegerList.Create;

  for I := 0 to HMSize - 1 do
  begin
    for J := 0 to HMSize - 1 do
    begin
      v[0] := J + Posi[0];
      v[1] := HMap[I, J] + Posi[1];
      v[2] := I + Posi[2];
      v := VectorScale(v, Scale);
      t[0] := J / HMSize;
      t[1] := I / HMSize;
      t[2] := 0;
      FBufferVertices.Add(v);
      FTexCoords.Add(t);
      i1 := I * HMSize + J;
      i2 := (I + 1) * (HMSize) + J;
      if I < HMSize - 1 then
        FBufferVertexIndices.Add(i1, i2);
    end;
    i1 := FBufferVertexIndices[FBufferVertexIndices.Count - 1];
    i2 := (I + 1) * (HMSize);
    FBufferVertexIndices.Add(i1, i2);
  end;
  FNormals.Count := FBufferVertices.Count;
  for I := 0 to FBufferVertexIndices.Count - 4 do
  begin
    v1 := FBufferVertices[FBufferVertexIndices[I]];
    v2 := FBufferVertices[FBufferVertexIndices[I + 1]];
    v3 := FBufferVertices[FBufferVertexIndices[I + 2]];
    n := CalcPlaneNormal(v2, v3, v1);
    FNormals[FBufferVertexIndices[I]] := n;
  end;


{  vId:=TGLVBOArrayBufferHandle.CreateFromData(Vertexes.List,sizeof(GLFLoat)*3*Vertexes.Count,GL_STATIC_DRAW);

  if FNormals.Count>0 then
  begin
    nid:=TGLVBOArrayBufferHandle.CreateFromData( FNormals.List,sizeof(GLFLoat)*3*FNormals.Count,GL_STATIC_DRAW);
  end;

  if FTexCoords.Count>0 then
  begin
  tid:=TGLVBOArrayBufferHandle.CreateFromData(FTexCoords.List,sizeof(GLFLoat)*3*FTexCoords.Count, GL_STATIC_DRAW);
  end;

  if Indices.Count>0 then begin
   FVBOElementArrayHandle:=TGLVBOElementArrayHandle.CreateFromData(Indices.list,sizeof(GLUint)*Indices.Count,GL_STATIC_DRAW)
  end;}
end;

procedure TGLVBOSimpleTerrainRenderer.BuildList(var rci: TRenderContextInfo);
begin
  if FVBOBuilt = False then
  begin
    vId := TGLVBOArrayBufferHandle.CreateFromData(FBufferVertices.List,
      sizeof(GLFLoat) * 3 * FBufferVertices.Count, GL_STATIC_DRAW);
    FVBOElementArrayHandle := TGLVBOElementArrayHandle.CreateFromData(
      FBufferVertexIndices.list, sizeof(GLUint) * FBufferVertexIndices.Count, GL_STATIC_DRAW);

    if FNormals.Count > 0 then
    begin
      nid := TGLVBOArrayBufferHandle.CreateFromData(
        FNormals.List, sizeof(GLFLoat) * 3 * FNormals.Count, GL_STATIC_DRAW);
    end;

    if FTexCoords.Count > 0 then
    begin
      tid := TGLVBOArrayBufferHandle.CreateFromData(FTexCoords.List, sizeof(
        GLFLoat) * 3 * FTexCoords.Count, GL_STATIC_DRAW);
    end;

    if FNeedToFreeMemory then
    begin
      FBufferVertices.Free;
      FNormals.Free;
      FTexCoords.Free;
    end;

    FVBOBuilt := True;
  end;


  if assigned(nId) then
  begin
    gl.EnableClientState(GL_NORMAL_ARRAY);
    gl.BindBuffer(GL_ARRAY_BUFFER, nId.Handle);
    gl.NormalPointer(GL_FLOAT, 0, nil);
  end;

  if assigned(tId) then
  begin
    gl.EnableClientState(GL_TEXTURE_COORD_ARRAY);
    gl.BindBuffer(GL_ARRAY_BUFFER, tId.Handle);
    gl.TexCoordPointer(3, GL_FLOAT, 0, nil);
  end;

  gl.EnableClientState(GL_VERTEX_ARRAY);
  gl.BindBuffer(GL_ARRAY_BUFFER, vId.Handle);
  gl.VertexPointer(3, GL_FLOAT, 0, nil);


  gl.BindBuffer(GL_ELEMENT_ARRAY_BUFFER, FVBOElementArrayHandle.Handle);

  gl.DrawElements(GL_TRIANGLE_STRIP, FBufferVertexIndices.Count - 1,
    GL_UNSIGNED_INT, nil);

  gl.BindBuffer(GL_ELEMENT_ARRAY_BUFFER, 0);


  gl.DisableClientState(GL_VERTEX_ARRAY);
  if assigned(nId) then
    gl.DisableClientState(GL_NORMAL_ARRAY);
  if assigned(tId) then
    gl.DisableClientState(GL_TEXTURE_COORD_ARRAY);
end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization
  // ------------------------------------------------------------------
  // ------------------------------------------------------------------
  // ------------------------------------------------------------------

  // class registrations
  RegisterClass(TGLTerrainRenderer);

end.

