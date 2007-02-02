// GLBumpmapHDS
{: Implements and HDS that automatically generates an elevation bumpmap.<p>

   The object-space elevation bumpmap can be used for dynamic terrain
   lighting.<p>

	<b>History : </b><font size=-1><ul>

      <li>01/02/07 - LIN- Added 'MaxTextures' property.
                         if the MaterialLibrary.Materials.Count > MaxTextures, then unused textures are deleted.
                         Set MaxTextures=0 to disable Auto-deletes, and manage your normal-map textures manually.

                         WARNING: If you use THeightData.MaterialName, instead of THeightData.LibMaterial,
                                  then HeightData does NOT register the texture as being used.
                                  So make sure MaxTextures=0 if you use MaterialName.

      <li>25/01/07 - LIN- Replaced 'StartPreparingData' and 'GenerateBumpmap' functions.
                          Now supports a TGLBitmap with multiple tiles.
                          Now works with HeightTileFileHDS.
                          World texture coordinates for individual textures are now calculated,
                          (TGLLibMaterial.TextureOffset and TGLLibMaterial.TextureScale)
                          Bugfix: Terrain position no longer jumps when InfiniteWrap is turned off.
      <li>15/04/04 - EG - Fixed hdsNone support (Phil Scadden)
      <li>20/03/04 - EG - Works, reasonnably seamless but still quite inefficient
      <li>20/02/04 - EG - Creation
	</ul></font>
}
unit GLBumpmapHDS;

interface

uses Classes, GLHeightData, GLGraphics, VectorGeometry, GLTexture, Dialogs, Forms;

type
   TGLBumpmapHDS = class;

   // TNewTilePreparedEvent
   //
   TNewTilePreparedEvent = procedure (Sender : TGLBumpmapHDS; heightData : THeightData;
                                      normalMapMaterial : TGLLibMaterial) of object;

	// TGLBumpmapHDS
	//
   {: An Height Data Source that generates elevation bumpmaps automatically.<p>
      The HDS must be connected to another HDS, which will provide the elevation
      data, and to a MaterialLibrary where bumpmaps will be placed. }
	TGLBumpmapHDS = class (THeightDataSource)
	   private
	      { Private Declarations }
         FElevationHDS : THeightDataSource;
         FBumpmapLibrary : TGLMaterialLibrary;
         FOnNewTilePrepared : TNewTilePreparedEvent;
         FBumpScale : Single;
         FSubSampling : Integer;
         FMaxTextures : integer;
	   protected
	      { Protected Declarations }
         procedure SetElevationHDS(const val : THeightDataSource);
         procedure SetBumpmapLibrary(const val : TGLMaterialLibrary);
         procedure SetBumpScale(const val : Single);
         function  StoreBumpScale : Boolean;
         procedure SetSubSampling(const val : Integer);
         procedure StartPreparingData(heightData : THeightData); override;
	   public
	      { Public Declarations }
	      constructor Create(AOwner: TComponent); override;
         destructor Destroy; override;
         procedure  GenerateNormalMap(heightData : THeightData; normalMap : TGLBitmap32; scale : Single);
         procedure  Notification(AComponent: TComponent; Operation: TOperation); override;
         procedure  Release(aHeightData : THeightData); override;
         procedure  TrimTextureCache(MaxTextureCount:integer);
         //procedure  TileTextureCoordinates(heightData : THeightData; TextureScale:TTexPoint; TextureOffset:TTexPoint);
	   published
	      { Published Declarations }
         property ElevationHDS : THeightDataSource read FElevationHDS write SetElevationHDS;
         property BumpmapLibrary : TGLMaterialLibrary read FBumpmapLibrary write SetBumpmapLibrary;
         property OnNewTilePrepared : TNewTilePreparedEvent read FOnNewTilePrepared write FOnNewTilePrepared;
         property BumpScale : Single read FBumpScale write SetBumpScale stored StoreBumpScale;
         {: Specifies the amount of subsampling for the bump texture.<p>
            This value must be a power of 2, and is used to divide the height
            tile resolution to determine the bump texture resolution (f.i.
            a tile size of 128 with a subsampling of 4 will result in textures
            of a resolution of 32x32. SubSampling won't allow texture resolution
            to get below 16x16 (minimal bumpmap resolution). }
         property SubSampling : Integer read FSubSampling write SetSubSampling default 1;

         property MaxPoolSize;
         {: If MaxTextures>0 then the Bumpmap library is trimmed down to size whenever
            the texture count is larger than MaxTextures. The oldest, unused texture is trimmed first.
            However, if you use THeightData.MaterialName, instead of THeightData.LibMaterial,
            then the THeightData component does not register the texture as being used.
            So, if you use THeightData.MaterialName then make sure MaxTextures=0.
            If MaxTextures=0 then the Texture cache is not trimmed.}
         property MaxTextures :integer read FMaxTextures write FMaxTextures;
	end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

uses SysUtils, OpenGL1x, GLUtils;

const
   cDefaultBumpScale = 0.1;

// ------------------
// ------------------ TGLBumpmapHDS ------------------
// ------------------

// Create
//
constructor TGLBumpmapHDS.Create(AOwner: TComponent);
begin
	inherited Create(AOwner);
   FBumpScale:=cDefaultBumpScale;
   FSubSampling:=1;
end;

// Destroy
//
destructor TGLBumpmapHDS.Destroy;
begin
   ElevationHDS:=nil;
   BumpmapLibrary:=nil;
	inherited Destroy;
end;

// Notification
//
procedure TGLBumpmapHDS.Notification(AComponent: TComponent; Operation: TOperation);
begin
   if Operation=opRemove then begin
      if      AComponent=FElevationHDS   then ElevationHDS:=nil
      else if AComponent=FBumpmapLibrary then BumpmapLibrary:=nil;
   end;
   inherited;
end;

// Release
//
procedure TGLBumpmapHDS.Release(aHeightData : THeightData);
var libMat : TGLLibMaterial;
begin
  {
  //WARNING: Only materials that were attached, using THeightData.LibMaterial
  //will be freed here, and ONLY if the material is not used by another registered object.
  //
  if Assigned(FBumpmapLibrary)and(FMaxTextures>0) then begin
    libMat:=aHeightData.LibMaterial;
    if assigned(LibMat)and(libMat.IsUsed=false)
      then LibMat.free;
  end;
  }
  inherited;
end;

// TrimTextureCache
//
// This will repeatedly delete the oldest unused texture from the TGLMaterialLibrary,
// until the texture count drops to MaxTextureCount

// DONT use this if you used THeightData.MaterialName to link your terrain textures.
// Either use THeightData.MaterialName instead, or manually delete unused Normal-Map textures.
//
procedure TGLBumpmapHDS.TrimTextureCache(MaxTextureCount:integer);
var matLib: TGLMaterialLibrary;
    libMat: TGLLibMaterial;
    i:integer;
    cnt:integer;
begin
  matLib:=FBumpmapLibrary;
  cnt:=matlib.Materials.Count;
  i:=0;while (i<cnt)and(cnt>=MaxTextureCount) do begin
    libMat:=matlib.Materials[i];
    if libMat.IsUsed then i:=i+1
    else libmat.Free;
    cnt:=matlib.Materials.Count;
  end;
end;

// StartPreparingData
//
procedure TGLBumpmapHDS.StartPreparingData(heightData : THeightData);
var HDS   : THeightDataSource;
    htfHD : THeightData;
    HD    : THeightData;
    //MatLib: TGLMaterialLibrary;
    libMat: TGLLibMaterial;
    bmp32 : TGLBitmap32;
    //TexScale:TTexPoint;
    //TexOffset:TTexPoint;
    MatName:string;
begin
  if not Assigned(FElevationHDS) then begin
    heightData.DataState:=hdsNone;
    exit;
  end;

  //---Height Data--
  HD:=HeightData;
  HD.DataType:=hdtSmallInt;
  HD.Allocate(hdtSmallInt);
  HDS:=self.FElevationHDS;
  //HD.FTextureCoordinatesMode:=tcmWorld;
  HD.TextureCoordinatesMode:=tcmWorld;
  htfHD:=HDS.GetData(HD.XLeft,HD.YTop,HD.Size,HD.DataType);
  if htfHD.DataState=hdsNone then HD.DataState:=hdsNone
  else begin HD.DataState:=hdsPreparing;
    Move(htfHD.SmallIntData^, HD.SmallIntData^, htfHD.DataSize);

    if Assigned(FBumpmapLibrary) then begin
      MatName:='BumpHDS_x'+IntToStr(HD.XLeft)+'y'+IntToStr(HD.YTop)+'.'; //name contains xy coordinates of the current tile
      LibMat:=FBumpmapLibrary.Materials.GetLibMaterialByName(MatName);   //Check if Tile Texture already exists
      if LibMat=nil then begin
        if (FMaxTextures>0) then TrimTextureCache(FMaxTextures); //Trim unused textures from the material library

        //Generate new NormalMap texture for this tile
        libMat:=FBumpmapLibrary.Materials.Add;
        libMat.Name:=MatName;
        //--Texture coordinates for current tile--
        libMat.TextureScale.X :=HDS.Width/(HD.Size-1);
        libMat.TextureScale.Y :=HDS.Height/(HD.Size-1);
        libMat.TextureOffset.X:=-((HD.XLeft/HDS.Width )*libMat.TextureScale.X);
        libMat.TextureOffset.Y:=-(HDS.Height-(HD.YTop+HD.Size-1))/(HD.Size-1);
        //----------------------------------------
        //--Set up new Normalmap texture for the current tile--
        libMat.Material.MaterialOptions:=[moNoLighting];
        with libMat.Material.Texture do begin
          ImageClassName:=TGLBlankImage.ClassName;
          Enabled:=True;
          //MagFilter:=maNearest;
          //MinFilter:=miNearestMipmapNearest;
          MagFilter:=maLinear;
          MinFilter:=miLinearMipmapNearest;
          TextureMode:=tmReplace;
          TextureWrap:=twNone;
          TextureFormat:=tfRGBA16;
          bmp32:=(Image as TGLBlankImage).GetBitmap32(GL_TEXTURE_2D);
          GenerateNormalMap(HD , bmp32, FBumpScale);
        end;
        //----------------------------------------------------
      end;
      //HD.MaterialName:=LibMat.Name;
      HD.LibMaterial:=LibMat;  //attach texture to current tile
      if Assigned(FOnNewTilePrepared) then FOnNewTilePrepared(Self,HD,libMat);
    end;

    FElevationHDS.Release(htfHD);
    HD.HeightMin:=htfHD.HeightMin;
    HD.HeightMax:=htfHD.HeightMax;
    HD.DataState:=hdsReady;
  end;
  //----------------
end;
{
// TileTextureCoordinates
//
procedure TGLBumpmapHDS.TileTextureCoordinates(heightData : THeightData; TextureScale:TTexPoint; TextureOffset:TTexPoint);
var HD:THeightData;
begin
  HD:=heightData;
  HDS:=FElevationHDS;
  TextureScale.X :=HDS.Width/(HD.Size-1);
  TextureScale.Y :=HDS.Height/(HD.Size-1);
  TextureOffset.X:=-((HD.XLeft/HDS.Width )*TextureScale.X);
  TextureOffset.Y:=-(HDS.Height-(HD.YTop+HD.Size-1))/(HD.Size-1);
end;
}
// GenerateNormalMap
//
procedure TGLBumpmapHDS.GenerateNormalMap(heightData : THeightData;
                                          normalMap : TGLBitmap32;
                                          scale : Single);
var mapSize:integer;
    HD : THeightData;
    x,y:integer;
    scaleVec:TAffineVector;
    vec   : TAffineVector;
    nmRow : PGLPixel32Array;
    px,py:integer;
begin
  HD:=HeightData;
  MapSize:=(HD.Size-1);
  mapSize:=mapSize div SubSampling;
  normalMap.Height:=mapSize;
  normalMap.Width :=mapSize;
  SetVector(ScaleVec,-1,1,-FBumpScale);
  for y:=0 to mapSize-1 do begin
    nmRow:=normalMap.ScanLine[mapSize-1-y];
    for x:=0 to mapSize-1 do begin
      px:=x*subsampling;
      py:=y*subsampling;
      if x<=0 then px:=1;  // clamp x, to hide seams
      if y<=0 then py:=1;  // clamp x, to hide seams
      vec:=HD.Normal(px,py,ScaleVec);
      nmRow[x].r:=round(128+127*vec[0]);
      nmRow[x].g:=round(128+127*vec[1]);
      nmRow[x].b:=round(128+127*vec[2]);
      nmRow[x].a:=255;
    end;
  end;
end;

// SetElevationHDS
//
procedure TGLBumpmapHDS.SetElevationHDS(const val : THeightDataSource);
begin
   if val<>FElevationHDS then begin
      if Assigned(FElevationHDS) then
         FElevationHDS.RemoveFreeNotification(Self);
      FElevationHDS:=val;
      if Assigned(FElevationHDS) then
         FElevationHDS.FreeNotification(Self);
      MarkDirty;
   end;
end;

// SetBumpmapLibrary
//
procedure TGLBumpmapHDS.SetBumpmapLibrary(const val : TGLMaterialLibrary);
begin
   if val<>FBumpmapLibrary then begin
      if Assigned(FBumpmapLibrary) then
         FBumpmapLibrary.RemoveFreeNotification(Self);
      FBumpmapLibrary:=val;
      if Assigned(FBumpmapLibrary) then
         FBumpmapLibrary.FreeNotification(Self);
      MarkDirty;
   end;
end;

// SetBumpScale
//
procedure TGLBumpmapHDS.SetBumpScale(const val : Single);
begin
   if FBumpScale<>val then begin
      FBumpScale:=val;
      MarkDirty;
   end;
end;

// StoreBumpScale
//
function TGLBumpmapHDS.StoreBumpScale : Boolean;
begin
   Result:=(FBumpScale<>cDefaultBumpScale);
end;

// SetSubSampling
//
procedure TGLBumpmapHDS.SetSubSampling(const val : Integer);
begin
   if val<>FSubSampling then begin
      FSubSampling:=RoundDownToPowerOf2(val);
      if FSubSampling<1 then
         FSubSampling:=1;
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
   RegisterClass(TGLBumpmapHDS);

end.
