//
// This unit is part of the GLScene Project, http://glscene.org
//
{: DDS<p>

   Simple DDS (Microsoft DirectDraw Surface) format support
   for Delphi.<p>
   
   Note:<br>
   Does not support DXTC compression and only the main
   surface is loaded (mipmap levels, volume textures and
   cubic environment maps are currently ignored).<p>

   Supported pixel formats:<ul>
      <li>16 bit (565, 1555, 4444)
      <li>24 bit (888)
      <li>32 bit (8888)
   </ul>

   <b>History : </b><font size=-1><ul>
      <li>31/08/04 - SG - Added support for 16 bit textures,
                          it should support most uncompressed files now
      <li>31/08/04 - SG - Creation
   </ul></font>
}
unit DDS;

interface

{$i GLScene.inc}

uses Classes, SysUtils, GLCrossPlatform;

type

   TDDSImage = class (TGLBitmap)
      public
         { Public Declarations }
         procedure LoadFromStream(stream : TStream); override;
         procedure SaveToStream(stream : TStream); override;
	end;

   EDDSException = class(Exception)
   end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

const
   DDSD_CAPS        = $00000001;
   DDSD_HEIGHT      = $00000002;
   DDSD_WIDTH       = $00000004;
   DDSD_PITCH       = $00000008;
   DDSD_PIXELFORMAT = $00001000;
   DDSD_MIPMAPCOUNT = $00020000;
   DDSD_LINEARSIZE  = $00080000;
   DDSD_DEPTH       = $00800000;

   DDPF_ALPHAPIXELS = $00000001;
   DDPF_FOURCC      = $00000004;
   DDPF_RGB         = $00000040;

   DDSCAPS_COMPLEX  = $00000008;
   DDSCAPS_TEXTURE  = $00001000;
   DDSCAPS_MIPMAP   = $00400000;

   DDSCAPS2_CUBEMAP           = $00000200;
   DDSCAPS2_CUBEMAP_POSITIVEX = $00000400;
   DDSCAPS2_CUBEMAP_NEGATIVEX = $00000800;
   DDSCAPS2_CUBEMAP_POSITIVEY = $00001000;
   DDSCAPS2_CUBEMAP_NEGATIVEY = $00002000;
   DDSCAPS2_CUBEMAP_POSITIVEZ = $00004000;
   DDSCAPS2_CUBEMAP_NEGATIVEZ = $00008000;
   DDSCAPS2_VOLUME            = $00200000;


type
   TDDPIXELFORMAT = record
      dwSize,
      dwFlags,
      dwFourCC,
      dwRGBBitCount,
      dwRBitMask,
      dwGBitMask,
      dwBBitMask,
      dwRGBAlphaBitMask : Cardinal;
   end;

   TDDCAPS2 = record
      dwCaps1,
      dwCaps2 : Cardinal;
      Reserved : array[0..1] of Cardinal;
   end;

   TDDSURFACEDESC2 = record
      dwSize,
      dwFlags,
      dwHeight,
      dwWidth,
      dwPitchOrLinearSize,
      dwDepth,
      dwMipMapCount : Cardinal;
      dwReserved1 : array[0..10] of Cardinal;
      ddpfPixelFormat : TDDPIXELFORMAT;
      ddsCaps : TDDCAPS2;
      dwReserved2 : Cardinal;
   end;

   TDDSHeader = record
      Magic : Cardinal;
      SurfaceFormat : TDDSURFACEDESC2;
   end;

   TFOURCC = array[0..3] of char;

// ------------------
// ------------------ TDDSImage ------------------
// ------------------

// LoadFromStream
//
procedure TDDSImage.LoadFromStream(stream : TStream);

   function GetBitsFromMask(Mask : Cardinal) : Byte;
   var
      i, temp : Integer;
   begin
      if Mask=0 then begin
        Result:=0;
        exit;
      end;

      temp:=Mask;
      for i:=0 to 31 do begin
         if (Temp and 1) = 1 then
            break;
         temp:=temp shr 1;
      end;
      Result:=i;
   end;

var
   header : TDDSHeader;
   i, j, rowSize, ddsPixelSize, imgPixelSize : Integer;
   buf : PByteArray;
   col : PCardinal;
   RedShift, RedMult,
   GreenShift, GreenMult,
   BlueShift, BlueMult,
   AlphaShift, AlphaMult : Byte;
begin
   stream.Read(header, Sizeof(TDDSHeader));

   with header.SurfaceFormat do begin
      if (ddsCaps.dwCaps1 and DDSCAPS_TEXTURE)=0 then
         raise EDDSException.Create('Unsupported DDSCAPS settings');
      if (ddpfPixelFormat.dwFlags and DDPF_FOURCC)>0 then
         raise EDDSException.Create('DXTC compression is not yet supported');

      case ddpfPixelFormat.dwRGBBitCount of
         16 : begin
            if (ddpfPixelFormat.dwFlags and DDPF_ALPHAPIXELS)>0 then begin
               Transparent:=True;
               PixelFormat:=glpf32bit;
            end else begin
               Transparent:=False;
               PixelFormat:=glpf24bit;
            end;
         end;
         24 : PixelFormat:=glpf24bit;
         32 : begin
            PixelFormat:=glpf32bit;
            if (ddpfPixelFormat.dwFlags and DDPF_ALPHAPIXELS)>0 then begin
               Transparent:=True;
               PixelFormat:=glpf32bit;
            end else begin
               Transparent:=False;
               PixelFormat:=glpf24bit;
            end;
         end;
      else
         raise EDDSException.Create('Unsupported DDS pixel format');
      end;

      case PixelFormat of
         glpf24bit : imgPixelSize:=3;
         glpf32bit : imgPixelSize:=4;
      end;

      Width:=dwWidth;
      Height:=dwHeight;

      ddsPixelSize:=(ddpfPixelFormat.dwRGBBitCount div 8);
      rowSize:=ddsPixelSize*dwWidth;

      RedShift:=GetBitsFromMask(ddpfPixelFormat.dwRBitMask);
      GreenShift:=GetBitsFromMask(ddpfPixelFormat.dwGBitMask);
      BlueShift:=GetBitsFromMask(ddpfPixelFormat.dwBBitMask);
      if Transparent then
         AlphaShift:=GetBitsFromMask(ddpfPixelFormat.dwRGBAlphaBitMask);

      RedMult  :=255 div (ddpfPixelFormat.dwRBitMask shr RedShift);
      GreenMult:=255 div (ddpfPixelFormat.dwGBitMask shr GreenShift);
      BlueMult :=255 div(ddpfPixelFormat.dwBBitMask shr BlueShift);
      if Transparent then
         AlphaMult:=255 div (ddpfPixelFormat.dwRGBAlphaBitMask shr AlphaShift);

      GetMem(buf, rowSize);
      for j:=0 to Height-1 do begin
         Stream.Read(buf[0], rowSize);
         for i:=0 to Width-1 do begin
            col:=@buf[ddsPixelSize*i];
            PByteArray(ScanLine[j])^[imgPixelSize*i+0]:=
               BlueMult*(col^ and ddpfPixelFormat.dwBBitMask) shr BlueShift;
            PByteArray(ScanLine[j])^[imgPixelSize*i+1]:=
               GreenMult*(col^ and ddpfPixelFormat.dwGBitMask) shr GreenShift;
            PByteArray(ScanLine[j])^[imgPixelSize*i+2]:=
               RedMult*(col^ and ddpfPixelFormat.dwRBitMask) shr RedShift;            if Transparent then begin
            if Transparent then
               PByteArray(ScanLine[j])^[imgPixelSize*i+3]:=
                  AlphaMult*(col^ and ddpfPixelFormat.dwRGBAlphaBitMask) shr AlphaShift;            end;
         end;
      end;
      FreeMem(buf);
   end;
end;

// SaveToStream
//
procedure TDDSImage.SaveToStream(stream : TStream);
var
   header : TDDSHeader;
   i, rowSize : Integer;
begin
   FillChar(header, SizeOf(TDDSHeader), 0);
   with header.SurfaceFormat do begin
      dwSize:=124;
      dwFlags:=DDSD_CAPS +
               DDSD_PIXELFORMAT +
               DDSD_WIDTH +
               DDSD_HEIGHT +
               DDSD_PITCH;
      dwWidth:=Width;
      dwHeight:=Height;
      case PixelFormat of
         glpf24bit : begin
            ddpfPixelFormat.dwFlags:=DDPF_RGB;
            ddpfPixelFormat.dwRGBBitCount:=24;
            ddpfPixelFormat.dwRBitMask:=$00FF0000;
            ddpfPixelFormat.dwGBitMask:=$0000FF00;
            ddpfPixelFormat.dwBBitMask:=$000000FF;
         end;
         glpf32bit : begin
            ddpfPixelFormat.dwFlags:=DDPF_RGB;
            ddpfPixelFormat.dwRGBBitCount:=32;
            ddpfPixelFormat.dwRBitMask:=$00FF0000;
            ddpfPixelFormat.dwGBitMask:=$0000FF00;
            ddpfPixelFormat.dwBBitMask:=$000000FF;
            if Transparent then begin
               ddpfPixelFormat.dwFlags:=ddpfPixelFormat.dwFlags + DDPF_ALPHAPIXELS;
               ddpfPixelFormat.dwRGBAlphaBitMask:=$FF000000;
            end;
         end;
      else
         raise EDDSException.Create('Unsupported pixel format format');
      end;
      rowSize:=(ddpfPixelFormat.dwRGBBitCount div 8)*dwWidth;
      dwPitchOrLinearSize:=dwHeight*Cardinal(rowSize);
      ddsCaps.dwCaps1:=DDSCAPS_TEXTURE;
      stream.Write(header, SizeOf(TDDSHeader));
      for i:=0 to Height-1 do
         stream.Write(ScanLine[i]^, rowSize);
   end;
end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

   TGLPicture.RegisterFileFormat(
     'dds', 'Microsoft DirectDraw Surface', TDDSImage);

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
finalization
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

   TGLPicture.UnregisterGraphicClass(TDDSImage);

end.
