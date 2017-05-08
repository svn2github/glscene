//
// VXScene Component Library, based on GLScene http://glscene.sourceforge.net 
//
{  
  Implements a HDS that automatically generates an elevation bumpmap.
  The object-space elevation bumpmap can be used for dynamic terrain lighting. 
  A bumpmap texture is generated for each terrain tile, and placed into a TVXMaterialLibrary.
      
       

}
unit VXS.BumpmapHDS;

interface

{$I VXScene.inc}

uses
  Winapi.OpenGL,
  Winapi.OpenGLext,
  System.Classes,
  System.SysUtils,
  System.SyncObjs,

  VXS.HeightData,
  VXS.Graphics,
  VXS.VectorGeometry,
  VXS.Texture,
  VXS.Material,
  VXS.Utils,
  VXS.VectorTypes;

type
  TVXBumpmapHDS = class;

  TNewTilePreparedEvent = procedure(Sender: TVXBumpmapHDS;
    heightData: TVXHeightData; normalMapMaterial: TVXLibMaterial) of object;

  { An Height Data Source that generates elevation bumpmaps automatically.
    The HDS must be connected to another HDS, which will provide the elevation
    data, and to a MaterialLibrary where bumpmaps will be placed. }
  TVXBumpmapHDS = class(TVXHeightDataSourceFilter)
  private
    // FElevationHDS : TVXHeightDataSource;
    FBumpmapLibrary: TVXMaterialLibrary;
    FOnNewTilePrepared: TNewTilePreparedEvent;
    FBumpScale: Single;
    FSubSampling: Integer;
    FMaxTextures: Integer;
    Uno: TCriticalSection;
  protected
    procedure SetBumpmapLibrary(const val: TVXMaterialLibrary);
    procedure SetBumpScale(const val: Single);
    function StoreBumpScale: Boolean;
    procedure SetSubSampling(const val: Integer);
    procedure Trim(MaxTextureCount: Integer);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Release(aHeightData: TVXHeightData); override;
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
    procedure GenerateNormalMap(heightData: TVXHeightData; normalMap: TVXBitmap32;
      scale: Single);
   { This will repeatedly delete the oldest unused texture from the TVXMaterialLibrary,
     until the texture count drops to MaxTextureCount.
     DONT use this if you used TVXHeightData.MaterialName to link your terrain textures.
     Either use with TVXHeightData.LibMaterial, or manually delete unused Normal-Map textures. }
    procedure TrimTextureCache(MaxTextureCount: Integer);
    // procedure  TileTextureCoordinates(heightData : TVXHeightData; TextureScale:TTexPoint; TextureOffset:TTexPoint);
    procedure PreparingData(heightData: TVXHeightData); override;
  published
    property BumpmapLibrary: TVXMaterialLibrary read FBumpmapLibrary
      write SetBumpmapLibrary;
    property OnNewTilePrepared: TNewTilePreparedEvent read FOnNewTilePrepared
      write FOnNewTilePrepared;
    property BumpScale: Single read FBumpScale write SetBumpScale
      stored StoreBumpScale;
    { Specifies the amount of subsampling for the bump texture.
      This value must be a power of 2, and is used to divide the height
      tile resolution to determine the bump texture resolution (f.i.
      a tile size of 128 with a subsampling of 4 will result in textures
      of a resolution of 32x32. SubSampling won't allow texture resolution
      to get below 16x16 (minimal bumpmap resolution). }
    property SubSampling: Integer read FSubSampling write SetSubSampling
      default 1;
    property MaxPoolSize;
    { If MaxTextures>0 then the Bumpmap library is trimmed down to size whenever
      the texture count is larger than MaxTextures. The oldest, unused texture is trimmed first.
      However, if you used TVXHeightData.MaterialName, instead of TVXHeightData.LibMaterial,
      then the TVXHeightData component does not register the texture as being used.
      So, if you use TVXHeightData.MaterialName then make sure MaxTextures=0.
      If MaxTextures=0 or if treads(VXS.AsyncHDS) are used, then the texture cache
      is NOT trimmed automatically.
      You will have to manually trim the cache from the main thread, by
      calling 'TrimTextureCache'. (VXS.AsyncHDS.OnIdle is a good place.) }
    property MaxTextures: Integer read FMaxTextures write FMaxTextures;
    property OnSourceDataFetched;
  end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

const
  cDefaultBumpScale = 0.01;

// ------------------
// ------------------ TVXBumpmapHDS ------------------
// ------------------
constructor TVXBumpmapHDS.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FBumpScale := cDefaultBumpScale;
  FSubSampling := 1;
  Uno := TCriticalSection.Create;
end;

destructor TVXBumpmapHDS.Destroy;
begin
  BumpmapLibrary := nil;
  Uno.Free;
  inherited Destroy;
end;

procedure TVXBumpmapHDS.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  if Operation = opRemove then
  begin
    if AComponent = FBumpmapLibrary then
      BumpmapLibrary := nil;
  end;
  inherited;
end;

procedure TVXBumpmapHDS.Release(aHeightData: TVXHeightData);
var
  libMat: TVXLibMaterial;
begin
  libMat := aHeightData.LibMaterial;
  aHeightData.MaterialName := '';
  if (FMaxTextures > 0) and (assigned(libMat)) and (libMat.IsUsed = false) then
    libMat.free;
  inherited;
end;

procedure TVXBumpmapHDS.TrimTextureCache(MaxTextureCount: Integer);
// Thread-safe Version
begin
  if assigned(self) then
  begin
    Uno.Acquire;
    Trim(MaxTextureCount);
    Uno.Release;
  end;
end;

procedure TVXBumpmapHDS.Trim(MaxTextureCount: Integer); // internal use only
var
  matLib: TVXMaterialLibrary;
  libMat: TVXLibMaterial;
  i: Integer;
  cnt: Integer;
begin
  matLib := FBumpmapLibrary;
  if matLib <> nil then
  begin
    cnt := matLib.Materials.Count;
    i := 0;
    while (i < cnt) and (cnt >= MaxTextureCount) do
    begin
      libMat := matLib.Materials[i];
      if libMat.IsUsed then
        i := i + 1
      else
        libMat.free;
      cnt := matLib.Materials.Count;
    end;
  end;
end;

procedure TVXBumpmapHDS.PreparingData(heightData: TVXHeightData);
var
  TmpHD: TVXHeightData;
  libMat: TVXLibMaterial;
  bmp32: TVXBitmap32;
  MatName: string;
begin
  if not assigned(FBumpmapLibrary) then
    exit;
  // --Generate Normal Map for tile--
  heightData.TextureCoordinatesMode := tcmLocal;
  heightData.TextureCoordinatesOffset := NullTexPoint;
  heightData.TextureCoordinatesScale := XYTexPoint;
  MatName := 'BumpHDS_x' + IntToStr(heightData.XLeft) + 'y' +
    IntToStr(heightData.YTop) + '.';
  // name contains xy coordinates of the current tile
  Uno.Acquire;
  libMat := FBumpmapLibrary.Materials.GetLibMaterialByName(MatName);
  // Check if Tile Texture already exists
  if libMat = nil then
  begin
    if (FMaxTextures > 0) then
    begin
      if heightData.Thread = nil { //Dont trim the cache from a sub-thread; }
      then
        TrimTextureCache(FMaxTextures)
        // Trim unused textures from the material library
    end;
    // Generate new NormalMap texture for this tile
    libMat := FBumpmapLibrary.Materials.Add;
    libMat.Name := MatName;
    // Transfer tile texture coordinates to generated texture
    libMat.TextureScale.X := heightData.TextureCoordinatesScale.S;
    libMat.TextureScale.Y := heightData.TextureCoordinatesScale.T;
    libMat.TextureOffset.X := heightData.TextureCoordinatesOffset.S;
    libMat.TextureOffset.Y := heightData.TextureCoordinatesOffset.T;
    // ------------------------------------------------------
    // --Set up new Normalmap texture for the current tile--
    libMat.Material.MaterialOptions := [moNoLighting];
    with libMat.Material.Texture do
    begin
      ImageClassName := TVXBlankImage.ClassName;
      Enabled := True;
      MinFilter := miNearestMipmapNearest;
      MagFilter := maLinear; // MagFilter:=maNearest;
      TextureMode := tmReplace;
      TextureWrap := twNone;
      TextureFormat := tfRGB16;
      // TextureFormat:=tfRGBA16;
      bmp32 := (Image as TVXBlankImage).GetBitmap32;
      TmpHD := HeightDataSource.GetData(heightData.XLeft - 1,
        heightData.YTop - 1, heightData.Size + 1, heightData.DataType);
      GenerateNormalMap(TmpHD, bmp32, FBumpScale);
      TmpHD.Release;
    end;
    // ----------------------------------------------------
  end;
  // HD.MaterialName:=LibMat.Name;
  heightData.LibMaterial := libMat; // attach texture to current tile
  if assigned(FOnNewTilePrepared) then
    FOnNewTilePrepared(self, heightData, libMat);
  Uno.Release;
end;

procedure TVXBumpmapHDS.GenerateNormalMap(heightData: TVXHeightData;
  normalMap: TVXBitmap32; scale: Single);
var
  MapSize: Integer;
  HD: TVXHeightData;
  X, Y: Integer;
  scaleVec: TAffineVector;
  vec: TAffineVector;
  nmRow: PGLPixel32Array;
  px, py: Integer;
begin
  HD := heightData;
  MapSize := (HD.Size - 1);
  MapSize := MapSize div SubSampling;
  normalMap.Height := MapSize;
  normalMap.Width := MapSize;
  normalMap.Blank := false;
  SetVector(scaleVec, 1, 1, FBumpScale);
  for Y := 0 to MapSize - 1 do
  begin
    nmRow := normalMap.ScanLine[MapSize - 1 - Y];
    for X := 0 to MapSize - 1 do
    begin
      px := X * SubSampling;
      py := Y * SubSampling;
      vec := HD.NormalAtNode(px, py, scaleVec);
      nmRow[X].r := round(128 + 127 * vec.X); // nmRow[x].r:=0;         //Red
      nmRow[X].g := round(128 + 127 * vec.Y);
      // nmRow[x].g:=0;         //Green
      nmRow[X].b := round(128 + 127 * vec.Z);
      // nmRow[x].b:=0;         //Blue
      nmRow[X].a := 255;
    end;
  end;
end;

procedure TVXBumpmapHDS.SetBumpmapLibrary(const val: TVXMaterialLibrary);
begin
  if val <> FBumpmapLibrary then
  begin
    if assigned(FBumpmapLibrary) then
      FBumpmapLibrary.RemoveFreeNotification(self);
    FBumpmapLibrary := val;
    if assigned(FBumpmapLibrary) then
      FBumpmapLibrary.FreeNotification(self);
    MarkDirty;
  end;
end;

procedure TVXBumpmapHDS.SetBumpScale(const val: Single);
begin
  if FBumpScale <> val then
  begin
    FBumpScale := val;
    MarkDirty;
  end;
end;

function TVXBumpmapHDS.StoreBumpScale: Boolean;
begin
  Result := (FBumpScale <> cDefaultBumpScale);
end;

procedure TVXBumpmapHDS.SetSubSampling(const val: Integer);
begin
  if val <> FSubSampling then
  begin
    FSubSampling := RoundDownToPowerOf2(val);
    if FSubSampling < 1 then
      FSubSampling := 1;
    MarkDirty;
  end;
end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

RegisterClass(TVXBumpmapHDS);

end.
