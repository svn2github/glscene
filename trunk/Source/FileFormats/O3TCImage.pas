//
// This unit is part of the GLScene Project, http://glscene.org
//
{: O3TCImage<p>
    Good for preview picture in OpenDialog, 
    so you may include both O3TCImage (preview) and GLFileO3TC (loading)

      <li>23/08/10 - Yar - Changes after PBuffer upgrade
      <li>21/03/10 - Yar - Added Linux support
                           (thanks to Rustam Asmandiarov aka Predator)
      <li>24/01/10 - Yar - Improved FPC compatibility
      <li>21/01/10 - Yar - Creation 
   </ul></font>
}

unit O3TCImage;

interface

{$i GLScene.inc}

uses
   {$IFDEF MSWINDOWS} Windows,  {$ENDIF}  Classes, SysUtils, GLCrossPlatform, VectorGeometry, GLGraphics,
  OpenGLTokens, GLPBuffer;

type

  TO3TCImage = class (TGLBitmap)
  public
   { Public Declarations }
   procedure LoadFromStream(stream : TStream); override;
   procedure SaveToStream(stream : TStream); override;
	end;

implementation

uses
  {$IFDEF FPC} graphtype, {$ENDIF}
  GLFileO3TC, GLTextureFormat;

// ------------------
// ------------------ TO3TCImage ------------------
// ------------------

// LoadFromStream
//
procedure TO3TCImage.LoadFromStream(stream : TStream);
var
  FullO3TC : TGLO3TCImage;
  size: integer;
  tempBuff: PGLubyte;
  {$IFNDEF FPC}
  src, dst: PGLubyte;
  y: Integer;
  {$ELSE}
  RIMG: TRawImage;
  {$ENDIF}
begin
  FullO3TC := TGLO3TCImage.Create;
  try
    FullO3TC.LoadFromStream( stream );
  except
    FullO3TC.Free;
    raise;
  end;

  if PBufferService.TextureID = 0 then
    PBufferService.Initialize(1, 1);
  PBufferService.Enable;

  // Setup texture
  PBufferService.GL.Enable       ( GL_TEXTURE_2D );
  PBufferService.GL.BindTexture  ( GL_TEXTURE_2D, PBufferService.TextureID);
  // copy texture to video memory
  size := ((FullO3TC.Width + 3) div 4)
        * ((FullO3TC.Height + 3) div 4)
        * FullO3TC.ElementSize;
  PBufferService.GL.CompressedTexImage2D( GL_TEXTURE_2D, 0,
    InternalFormatToOpenGLFormat(FullO3TC.InternalFormat),
    FullO3TC.Width, FullO3TC.Height, 0, size,
    FullO3TC.GetLevelData(0));

  PBufferService.GL.CheckError;

  GetMem( tempBuff, FullO3TC.Width*FullO3TC.Height*4 );
  // get texture from video memory in simple format
  PBufferService.GL.GetTexImage( GL_TEXTURE_2D, 0, GL_BGRA, GL_UNSIGNED_BYTE, tempBuff);

  Width       := FullO3TC.Width;
  Height      := FullO3TC.Height;
  Transparent := true;
  PixelFormat := glpf32bit;


{$IFNDEF FPC}
  src := tempBuff;
  for y := 0 to Height - 1 do begin
    dst := ScanLine[Height - 1 - y];
    Move(src^, dst^, Width*4);
    Inc(src, Width*4);
  end;
{$ELSE}
  RIMG.Init;
  rimg.Description.Init_BPP32_B8G8R8A8_BIO_TTB(Width, Height);
  rimg.Description.RedShift := 16;
  rimg.Description.BlueShift := 0;
  rimg.Description.LineOrder := riloBottomToTop;
  RIMG.DataSize := Width*Height*4;
  rimg.Data := PByte(tempBuff);
  LoadFromRawImage(rimg, false);
{$ENDIF}
  FullO3TC.Free;
  FreeMem( tempBuff );
  PBufferService.Disable;
end;

// SaveToStream
//
procedure TO3TCImage.SaveToStream(stream : TStream);
begin
  Assert(False, 'Not supported');
end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

   TGLPicture.RegisterFileFormat(
     'o3tc', 'oZone3D Texture Compression', TO3TCImage);

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
finalization
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

   TGLPicture.UnregisterGraphicClass(TO3TCImage);

end.
