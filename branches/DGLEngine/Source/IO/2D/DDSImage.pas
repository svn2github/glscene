//
// This unit is part of the DGLEngine Project, http://glscene.org
//
{: DDSImage<p>
    Alternative for DDS unit with more supported formats of flat image:
    Alpha8, Luminance8, R3G3B2, RGB5A1, RGBA4, Alpha8Luminance8, Luminance16, R5G6B5,
    RGB8, R10G10B10A2, RGBA8, RGBA16, R16F, RGBA16F, R32F, RGBA32F, GR16, GR16F, GR32F,
    Compressed RGB S3TC DXT1, Compressed RGBA S3TC DXT1, Compressed RGBA S3TC DXT3,
    Compressed RGBA S3TC DXT5
    But it down color to RGBA8 because becomes to TGLBitmap
    Good for preview picture in OpenDialog,
    so you may include both DDSImage (preview) and GLFileDDS (loading)

 <b>History : </b><font size=-1><ul>
        <li>28/12/15 - JD - Imported from GLScene
   </ul><p>
}

unit DDSImage;

interface

{$I DGLEngine.inc}

uses
{$IFDEF MSWINDOWS}Windows,{$ENDIF}
  Classes,
  SysUtils,
  DGLCrossPlatform,
  DGLVectorMaths,
  DGLGraphics,
  dglOpenGL,
  DGLContext;

type

  TDDSImage = class(TDGLBitmap)
  public
    { Public Declarations }
    procedure LoadFromStream(stream: TStream); override;
    procedure SaveToStream(stream: TStream); override;
  end;

  EDDSException = class(Exception);

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

uses
  DXTC,
  DGLFileDDS,
  DGLTextureFormat;


// ------------------
{ TDDSImage }
{$IFDEF GLS_REGION}{$REGION TDDSImage'}{$ENDIF}
procedure TDDSImage.LoadFromStream(stream: TStream);
var
  FullDDS: TDGLDDSImage;
  bCubeMap: Boolean;
  src, dst: PGLubyte;
  y: integer;
begin
  FullDDS := TDGLDDSImage.Create;
  try
    FullDDS.LoadFromStream(stream);
  except
    FullDDS.Free;
    raise;
  end;

  bCubeMap := FullDDS.CubeMap;
  FullDDS.Narrow;

  PixelFormat := glpf32bit;
  Transparent := True;
  Width := FullDDS.LevelWidth[0];
  Height := FullDDS.LevelHeight[0];

  src := PGLubyte(FullDDS.Data);
  if bCubeMap then
    for y := 0 to Height - 1 do
    begin
      dst := ScanLine[y];
      BGRA32ToRGBA32(src, dst, Width);
      Inc(src, Width * 4);
    end
  else
    for y := 0 to Height - 1 do
    begin
      dst := ScanLine[Height - 1 - y];
      BGRA32ToRGBA32(src, dst, Width);
      Inc(src, Width * 4);
    end;
  FullDDS.Free;
end;

procedure TDDSImage.SaveToStream(stream: TStream);
const
  Magic: array[0..3] of AnsiChar = 'DDS ';
var
  header: TDDSHeader;
  rowSize: integer;
  i: Integer;

begin
  FillChar(header, SizeOf(TDDSHeader), 0);
  header.magic := cardinal(Magic);
  with header.SurfaceFormat do
  begin
    dwSize := sizeof(TDDSURFACEDESC2);
    dwFlags := DDSD_CAPS + DDSD_PIXELFORMAT + DDSD_WIDTH + DDSD_HEIGHT + DDSD_PITCH;
    dwWidth := Width;
    dwHeight := Height;
    ddpf.dwSize := sizeof(TDDPIXELFORMAT);
    case PixelFormat of
{$IFDEF MSWINDOWS}
      glpf24bit:
        begin
          ddpf.dwFlags := DDPF_RGB;
          ddpf.dwRGBBitCount := 24;
          ddpf.dwRBitMask := $00FF0000;
          ddpf.dwGBitMask := $0000FF00;
          ddpf.dwBBitMask := $000000FF;
        end;
{$ENDIF}
      glpf32bit:
        begin
          ddpf.dwFlags := DDPF_RGB;
          ddpf.dwRGBBitCount := 32;
          ddpf.dwRBitMask := $00FF0000;
          ddpf.dwGBitMask := $0000FF00;
          ddpf.dwBBitMask := $000000FF;
          if Transparent then
          begin
            ddpf.dwFlags := ddpf.dwFlags + DDPF_ALPHAPIXELS;
            ddpf.dwRGBAlphaBitMask := $FF000000;
          end;
        end;
    else
      raise EDDSException.Create('Unsupported pixel format');
    end;
    rowSize := (ddpf.dwRGBBitCount div 8) * dwWidth;
    dwPitchOrLinearSize := dwHeight * cardinal(rowSize);
    dwCaps := DDSCAPS_TEXTURE;
    stream.Write(header, SizeOf(TDDSHeader));
    for i := 0 to Height - 1 do
      stream.Write(ScanLine[i]^, rowSize);
  end;
end;

{$IFDEF GLS_REGIONS}{$ENDREGION}{$ENDIF}

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization

  TDGLPicture.RegisterFileFormat( 'dds', 'Microsoft DirectDraw Surface', TDDSImage);

finalization

  TDGLPicture.UnregisterGraphicClass(TDDSImage);

end.

