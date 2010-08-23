//
// This unit is part of the GLScene Project, http://glscene.org
//
{: HDRImage<p>
    Good for preview picture in OpenDialog,
    so you may include both HDRImage (preview) and GLFileHDR (loading)

      <li>23/08/10 - Yar - Changes after PBuffer upgrade
      <li>21/03/10 - Yar - Added Linux support
                           (thanks to Rustam Asmandiarov aka Predator)
      <li>24/01/10 - Yar - Improved FPC compatibility
      <li>21/01/10 - Yar - Creation
   </ul></font>
}

unit HDRImage;

interface

{$I GLScene.inc}

uses
{$IFDEF MSWINDOWS}Windows,
{$ENDIF}Classes,
  SysUtils,
  GLCrossPlatform,
  VectorGeometry,
  GLGraphics,
  OpenGLTokens,
  GLPBuffer;

type

  THDRImage = class(TGLBitmap)
  public
    { Public Declarations }
    procedure LoadFromStream(stream: TStream); override;
    procedure SaveToStream(stream: TStream); override;
  end;

implementation

uses
{$IFDEF FPC}graphtype,
{$ENDIF}
  GLFileHDR,
  GLTextureFormat;

// ------------------
// ------------------ THDRImage ------------------
// ------------------

// LoadFromStream
//

procedure THDRImage.LoadFromStream(stream: TStream);
var
  FullHDR: TGLHDRImage;
  tempBuff: PGLubyte;
{$IFNDEF FPC}
  src, dst: PGLubyte;
  y: integer;
{$ELSE}
  RIMG: TRawImage;
{$ENDIF}
begin
  FullHDR := TGLHDRImage.Create;
  try
    FullHDR.LoadFromStream(stream);
  except
    FullHDR.Free;
    raise;
  end;

  if PBufferService.TextureID = 0 then
    PBufferService.Initialize(1, 1);
  PBufferService.Enable;

  // Setup texture
  PBufferService.GL.Enable(GL_TEXTURE_2D);
  PBufferService.GL.BindTexture(GL_TEXTURE_2D, PBufferService.TextureID);
  // copy texture to video memory
  PBufferService.GL.TexImage2D(GL_TEXTURE_2D, 0,
    InternalFormatToOpenGLFormat(FullHDR.InternalFormat), FullHDR.Width,
    FullHDR.Height, 0, FullHDR.ColorFormat, FullHDR.DataType,
    FullHDR.GetLevelData(0));

  PBufferService.GL.CheckError;

  GetMem(tempBuff, FullHDR.Width * FullHDR.Height * 3);
  // get texture from video memory in simple format
  PBufferService.GL.GetTexImage(GL_TEXTURE_2D, 0, GL_BGR, GL_UNSIGNED_BYTE, tempBuff);

  Width := FullHDR.Width;
  Height := FullHDR.Height;
  Transparent := false;
  PixelFormat := glpf24bit;

{$IFNDEF FPC}
  src := tempBuff;
  for y := 0 to Height - 1 do
  begin
    dst := ScanLine[Height - 1 - y];
    Move(src^, dst^, Width * 3);
    Inc(src, Width * 3);
  end;
{$ELSE}
  RIMG.Init;
  rimg.Description.Init_BPP24_B8G8R8_BIO_TTB(Width, Height);
  rimg.Description.RedShift := 16;
  rimg.Description.BlueShift := 0;
  rimg.Description.LineOrder := riloBottomToTop;
  RIMG.DataSize := Width * Height * 3;
  rimg.Data := PByte(tempBuff);
  LoadFromRawImage(rimg, false);
{$ENDIF}
  FullHDR.Free;
  FreeMem(tempBuff);
  PBufferService.Disable;
end;

// SaveToStream
//

procedure THDRImage.SaveToStream(stream: TStream);
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
    'HDR', 'High Dynamic Range Image', THDRImage);

  // ------------------------------------------------------------------
  // ------------------------------------------------------------------
  // ------------------------------------------------------------------
finalization
  // ------------------------------------------------------------------
  // ------------------------------------------------------------------
  // ------------------------------------------------------------------

  TGLPicture.UnregisterGraphicClass(THDRImage);

end.

