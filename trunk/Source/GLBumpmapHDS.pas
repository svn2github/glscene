// GLBumpmapHDS
{: Implements and HDS that can automatically generates an elevation bumpmap.<p>

   The object-space elevation bumpmap can be used for dynamic terrain
   lighting.<p>

	<b>History : </b><font size=-1><ul>
      <li>25/04/07 - LIN- Replaced 'StartPreparingData' and 'GenerateBumpmap' functions.
                          Now supports a TGLBitmap with multiple tiles.
                          Now works with HeightTileFileHDS.
                          World texture coordinates for individual textures are now calculated,
                          (TGLLibMaterial.TextureOffset and TGLLibMaterial.TextureScale)
                          Bugfix: Terrain position no longer jumps when InfiniteWrap is turned off.

                          BUG: It seems Released textures are sometimes still used by the TerrainRenderer.
                          So, to prevent Access Violations, the Release method has been disabled.
                          Textures will have to be released MANUALLY for now.
                          NOT releasing the textures will cause the  TextureLibrary to become overrun.

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
   // Releasing materials causes access violations later on
   {
   if Assigned(FBumpmapLibrary) then begin
      libMat:=FBumpmapLibrary.LibMaterialByName('BumpHDS_'+IntToHex(Int64(aHeightData), 16));
      if assigned(libMat) then FBumpmapLibrary.Materials.Delete(libMat.Index);
      //libMat.Free;
   end;
   }
   inherited;
end;

// GenerateNormalMap
//
procedure TGLBumpmapHDS.StartPreparingData(heightData : THeightData);
var HDS   : THeightDataSource;
    htfHD : THeightData;
    HD    : THeightData;
    //MatLib: TGLMaterialLibrary;
    libMat: TGLLibMaterial;
    bmp32 : TGLBitmap32;
    TexScale:TTexPoint;
    TexOffset:TTexPoint;
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

      //---If a Material with the same name exists then replace---
      //MatName:='BumpHDS_x'+IntToStr(HD.XLeft)+'y'+IntToStr(HD.YTop)+'.'; //name contains xy coordinates of the current tile
      MatName:='BumpHDS_'+IntToHex(Int64(heightData), 16);
      libMat:=FBumpmapLibrary.Materials.GetLibMaterialByName(MatName);
      if assigned(libMat) then FBumpmapLibrary.Materials.Delete(libmat.Index);
      //----------------------------------------------------------

      libMat:=FBumpmapLibrary.Materials.Add;
      libMat.Name:='BumpHDS_'+IntToHex(Int64(heightData), 16);

      //--Calculate World Texture coordinates for tiled terrain--
      libMat.TextureScale.X :=HDS.Width/(HD.Size-1);
      libMat.TextureScale.Y :=HDS.Height/(HD.Size-1);
      libMat.TextureOffset.X:=-((HD.XLeft/HDS.Width )*libMat.TextureScale.X);
      libMat.TextureOffset.Y:=-(HDS.Height-(HD.YTop+HD.Size-1))/(HD.Size-1);
      //---------------------------------------------------------

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
        GenerateNormalMap(htfHD, bmp32, FBumpScale);
      end;

      // scale and translate the bumpmap to reduce seams
      // libMat.TextureOffset.SetVector(0.5/bmp32.Width, 0.5/bmp32.Width, 1);
      // libMat.TextureScale.SetVector((bmp32.Width-1)/bmp32.Width, (bmp32.Width-1)/bmp32.Width, 1);
    end else libMat:=nil;

    if Assigned(libMat) then begin
      if Assigned(FOnNewTilePrepared)
        then FOnNewTilePrepared(Self, HD, libMat)
        else HD.MaterialName:=libMat.Name;
    end;

    FElevationHDS.Release(htfHD);
    HD.HeightMin:=htfHD.HeightMin;
    HD.HeightMax:=htfHD.HeightMax;
    HD.DataState:=hdsReady;
  end;
  //----------------
end;

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
    nmH,nmW:integer;
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
