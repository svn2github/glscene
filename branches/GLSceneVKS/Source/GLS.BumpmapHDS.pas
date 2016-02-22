//
// GLScene on Vulkan, http://glscene.sourceforge.net 
//
{  
  Implements a HDS that automatically generates an elevation bumpmap.
  The object-space elevation bumpmap can be used for dynamic terrain lighting. 
  A bumpmap texture is generated for each terrain tile, and placed into a TVKMaterialLibrary.
      
       
   
}
unit GLS.BumpmapHDS;

interface

{$I GLScene.inc}

uses
  System.Classes, System.SysUtils, System.SyncObjs,
  //GLS
  GLS.HeightData, GLS.Graphics, GLS.VectorGeometry,
  GLS.Texture, GLS.Material, Winapi.OpenGL, Winapi.OpenGLext,  GLS.Utils, GLS.VectorTypes;

type
  TVKBumpmapHDS = class;

  // TNewTilePreparedEvent
  //
  TNewTilePreparedEvent = procedure(Sender: TVKBumpmapHDS;
    heightData: THeightData; normalMapMaterial: TVKLibMaterial) of object;

  // TVKBumpmapHDS
  //
  { An Height Data Source that generates elevation bumpmaps automatically. 
    The HDS must be connected to another HDS, which will provide the elevation
    data, and to a MaterialLibrary where bumpmaps will be placed. }
  TVKBumpmapHDS = class(THeightDataSourceFilter)
  private
    { Private Declarations }
    // FElevationHDS : THeightDataSource;
    FBumpmapLibrary: TVKMaterialLibrary;
    FOnNewTilePrepared: TNewTilePreparedEvent;
    FBumpScale: Single;
    FSubSampling: Integer;
    FMaxTextures: Integer;
    Uno: TCriticalSection;
  protected
    { Protected Declarations }
    procedure SetBumpmapLibrary(const val: TVKMaterialLibrary);
    procedure SetBumpScale(const val: Single);
    function StoreBumpScale: Boolean;
    procedure SetSubSampling(const val: Integer);
    procedure Trim(MaxTextureCount: Integer);
  public
    { Public Declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Release(aHeightData: THeightData); override;
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
    procedure GenerateNormalMap(heightData: THeightData; normalMap: TVKBitmap32;
      scale: Single);
    procedure TrimTextureCache(MaxTextureCount: Integer);
    // procedure  TileTextureCoordinates(heightData : THeightData; TextureScale:TTexPoint; TextureOffset:TTexPoint);
    procedure PreparingData(heightData: THeightData); override;
  published
    { Published Declarations }
    property BumpmapLibrary: TVKMaterialLibrary read FBumpmapLibrary
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
      However, if you used THeightData.MaterialName, instead of THeightData.LibMaterial,
      then the THeightData component does not register the texture as being used.
      So, if you use THeightData.MaterialName then make sure MaxTextures=0.
      If MaxTextures=0 or if treads(GLS.AsyncHDS) are used, then the texture cache
      is NOT trimmed automatically.
      You will have to manually trim the cache from the main thread, by
      calling 'TrimTextureCache'. (GLS.AsyncHDS.OnIdle is a good place.) }
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
  // ------------------ TVKBumpmapHDS ------------------
  // ------------------

  // Create
  //

constructor TVKBumpmapHDS.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FBumpScale := cDefaultBumpScale;
  FSubSampling := 1;
  Uno := TCriticalSection.Create;
end;

// Destroy
//

destructor TVKBumpmapHDS.Destroy;
begin
  BumpmapLibrary := nil;
  Uno.Free;
  inherited Destroy;
end;

// Notification
//

procedure TVKBumpmapHDS.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  if Operation = opRemove then
  begin
    if AComponent = FBumpmapLibrary then
      BumpmapLibrary := nil;
  end;
  inherited;
end;

// Release
//

procedure TVKBumpmapHDS.Release(aHeightData: THeightData);
var
  libMat: TVKLibMaterial;
begin
  libMat := aHeightData.LibMaterial;
  aHeightData.MaterialName := '';
  if (FMaxTextures > 0) and (assigned(libMat)) and (libMat.IsUsed = false) then
    libMat.free;
  inherited;
end;

// TrimTextureCache
//
// This will repeatedly delete the oldest unused texture from the TVKMaterialLibrary,
// until the texture count drops to MaxTextureCount.
// DONT use this if you used THeightData.MaterialName to link your terrain textures.
// Either use with THeightData.LibMaterial, or manually delete unused Normal-Map textures.
//

procedure TVKBumpmapHDS.TrimTextureCache(MaxTextureCount: Integer);
// Thread-safe Version
begin
  if assigned(self) then
  begin
    Uno.Acquire;
    Trim(MaxTextureCount);
    Uno.Release;
  end;
end;

procedure TVKBumpmapHDS.Trim(MaxTextureCount: Integer); // internal use only
var
  matLib: TVKMaterialLibrary;
  libMat: TVKLibMaterial;
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

// PreparingData
//

procedure TVKBumpmapHDS.PreparingData(heightData: THeightData);
var
  TmpHD: THeightData;
  libMat: TVKLibMaterial;
  bmp32: TVKBitmap32;
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
      ImageClassName := TVKBlankImage.ClassName;
      Enabled := True;
      MinFilter := miNearestMipmapNearest;
      MagFilter := maLinear; // MagFilter:=maNearest;
      TextureMode := tmReplace;
      TextureWrap := twNone;
      TextureFormat := tfRGB16;
      // TextureFormat:=tfRGBA16;
      bmp32 := (Image as TVKBlankImage).GetBitmap32;
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

// GenerateNormalMap
//

procedure TVKBumpmapHDS.GenerateNormalMap(heightData: THeightData;
  normalMap: TVKBitmap32; scale: Single);
var
  MapSize: Integer;
  HD: THeightData;
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

// SetBumpmapLibrary
//

procedure TVKBumpmapHDS.SetBumpmapLibrary(const val: TVKMaterialLibrary);
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

// SetBumpScale
//

procedure TVKBumpmapHDS.SetBumpScale(const val: Single);
begin
  if FBumpScale <> val then
  begin
    FBumpScale := val;
    MarkDirty;
  end;
end;

// StoreBumpScale
//

function TVKBumpmapHDS.StoreBumpScale: Boolean;
begin
  Result := (FBumpScale <> cDefaultBumpScale);
end;

// SetSubSampling
//

procedure TVKBumpmapHDS.SetSubSampling(const val: Integer);
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

// class registrations
RegisterClass(TVKBumpmapHDS);

end.
