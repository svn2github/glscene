// GLBumpmapHDS
{: Implements and HDS that can automatically generates an elevation bumpmap.<p>

   ***** IN PROGRESS, NOt FUNCTIONAL YET *****

   The object-space elevation bumpmap can be used for dynamic terrain
   lighting.<p>

	<b>History : </b><font size=-1><ul>
      <li>20/02/04 - EG - Creation
	</ul></font>
}
unit GLBumpmapHDS;

interface

uses Classes, GLHeightData, GLGraphics, VectorGeometry, GLTexture;

type

	// TGLBumpmapHDS
	//
   {: An Height Data Source that generates elevation bumpmaps automatically.<p>
      The HDS must be connected to another HDS, which will provide the elevation
      datat, and to a MaterialLibrary where bumpmaps will be placed. }
	TGLBumpmapHDS = class (THeightDataSource)
	   private
	      { Private Declarations }
         FElevationHDS : THeightDataSource;
         FBumpmapLibrary : TGLMaterialLibrary;

	   protected
	      { Protected Declarations }
         procedure SetElevationHDS(const val : THeightDataSource);
         procedure SetBumpmapLibrary(const val : TGLMaterialLibrary);

         procedure StartPreparingData(heightData : THeightData); override;

         procedure GenerateNormalMap(heightData : THeightData;
                                     normalMap : TGLBitmap32; scale : Single);

	   public
	      { Public Declarations }
	      constructor Create(AOwner: TComponent); override;
         destructor Destroy; override;

         procedure Notification(AComponent: TComponent; Operation: TOperation); override;
         procedure Release(aHeightData : THeightData); override;

	   published
	      { Published Declarations }
         property ElevationHDS : THeightDataSource read FElevationHDS write SetElevationHDS;
         property BumpmapLibrary : TGLMaterialLibrary read FBumpmapLibrary write SetBumpmapLibrary;

         property MaxPoolSize;
	end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

uses SysUtils, OpenGL1x;

// ------------------
// ------------------ TGLBumpmapHDS ------------------
// ------------------

// Create
//
constructor TGLBumpmapHDS.Create(AOwner: TComponent);
begin
	inherited Create(AOwner);
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
      if AComponent=FElevationHDS then
         ElevationHDS:=nil
      else if AComponent=FBumpmapLibrary then
         BumpmapLibrary:=nil;
   end;
   inherited;
end;

// StartPreparingData
//
procedure TGLBumpmapHDS.StartPreparingData(heightData : THeightData);
var
   htfHD : THeightData;
   libMat : TGLLibMaterial;
   bmp32 : TGLBitmap32;
begin
   if Assigned(FElevationHDS) then begin
      with heightData do
         htfHD:=FElevationHDS.GetData(XLeft, YTop, Size, DataType);
      if (htfHD.DataState=hdsNone) then
         heightData.DataState:=hdsNone
      else begin
         heightData.MaterialName:=htfHD.MaterialName;
         heightData.TextureCoordinatesMode:=tcmLocal;
         heightData.TextureCoordinatesOffset:=NullTexPoint;
         heightData.TextureCoordinatesScale:=XYTexPoint;
         heightData.DataType:=hdtSmallInt;
         htfHD.DataType:=hdtSmallInt;
         heightData.Allocate(hdtSmallInt);
         Move(htfHD.SmallIntData^, heightData.SmallIntData^, htfHD.DataSize);
         heightData.DataState:=hdsReady;
         heightData.HeightMin:=htfHD.HeightMin;
         heightData.HeightMax:=htfHD.HeightMax;
      end;
      FElevationHDS.Release(htfHD);
   end;
   if Assigned(FBumpmapLibrary) then begin
      libMat:=FBumpmapLibrary.Materials.Add;
      libMat.Name:='BumpHDS_'+IntToHex(Int64(heightData), 16);
      with libMat.Material.Texture do begin
         ImageClassName:=TGLBlankImage.ClassName;
         Enabled:=True;
         TextureMode:=tmReplace;
         TextureWrap:=twNone;
         bmp32:=(Image as TGLBlankImage).GetBitmap32(GL_TEXTURE_2D);
         GenerateNormalMap(heightData, bmp32, 0.1);
      end;
      heightData.MaterialName:=libMat.Name;
   end;
end;

// Release
//
procedure TGLBumpmapHDS.Release(aHeightData : THeightData);
var
   libMat : TGLLibMaterial;
begin
   if Assigned(FBumpmapLibrary) then begin
      libMat:=FBumpmapLibrary.LibMaterialByName('BumpHDS_'+IntToHex(Int64(aHeightData), 16));
      libMat.Free;      
   end;
   inherited;
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

// GenerateNormalMap
//
procedure TGLBumpmapHDS.GenerateNormalMap(heightData : THeightData;
                                          normalMap : TGLBitmap32;
                                          scale : Single);
var
   x, y : Integer;
   p1, p2, p3 : Single;
   dcx, dcy : Single;
   invLen, scaleDiv255 : Single;
   curRow, nextRow : PSmallIntArray;
   nmRow : PGLPixel32Array;
begin
   heightData.DataType:=hdtSmallInt;
   nextRow:=heightData.SmallIntRaster[0];

   normalMap.Height:=heightData.Size-1;
   normalMap.Width:=heightData.Size-1;

   scaleDiv255:=scale*(1/255);
   for y:=0 to normalMap.Height-1 do begin
      curRow:=nextRow;
      nextRow:=heightData.SmallIntRaster[y+1];
      nmRow:=normalMap.ScanLine[normalMap.Height-1-y];
      for x:=0 to normalMap.Width-1 do begin
         p1:=curRow[x];
         p2:=nextRow[x];
         p3:=curRow[x+1];

         dcx:=scaleDiv255*(p3-p1);
         dcy:=scaleDiv255*(p1-p2);

         invLen:=RSqrt(Sqr(dcx)+Sqr(dcy)+1);

         with nmRow[x] do begin
            r:=Round(128+127*ClampValue(dcx*invLen, -1, 1));
            g:=Round(128+127*ClampValue(dcy*invLen, -1, 1));
            b:=Round(128+127*invLen);
            a:=255;
         end;
      end;
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
