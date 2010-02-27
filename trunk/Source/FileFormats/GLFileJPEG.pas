//
// This unit is part of the GLScene Project, http://glscene.org
//
{: GLFileJPEG<p>

  <b>History : </b><font size=-1><ul>
      <li>27/02/10 - Yar - Creation
  </ul><p>
}
unit GLFileJPEG;

interface

{$I GLScene.inc}

uses
  Classes, SysUtils,
  OpenGL1x, GLContext, GLGraphics, GLTextureFormat,
  ApplicationFileIO;

type

  TGLJPEGImage = class(TGLBaseImage)
  private
    FAbortLoading: Boolean;
    FDivScale: LongWord;
    FDither: Boolean;
  public
    constructor Create; override;
    class function Capabilities: TDataFileCapabilities; override;

    procedure LoadFromFile(const filename: string); override;
    procedure SaveToFile(const filename: string); override;
    procedure LoadFromStream(stream: TStream); override;
    procedure SaveToStream(stream: TStream); override;

    {: Assigns from any Texture.}
    procedure AssignFromTexture(textureContext: TGLContext;
      const textureHandle: TGLenum;
      textureTarget: TGLenum;
      const CurrentFormat: Boolean;
      const intFormat: TGLInternalFormat); override;

    property Data: PGLPixel32Array read FData;
    property Width: Integer read fWidth;
    property Height: Integer read fHeight;
    property Depth: Integer read fDepth;
    property MipLevels: Integer read fMipLevels;
    property ColorFormat: GLenum read fColorFormat;
    property InternalFormat: TGLInternalFormat read fInternalFormat;
    property DataType: GLenum read fDataType;
    property ElementSize: Integer read fElementSize;
    property CubeMap: Boolean read fCubeMap;
    property TextureArray: Boolean read fTextureArray;
    property DivScale: LongWord read FDivScale write FDivScale;
    property Dither: Boolean read FDither write FDither;
  end;

implementation

uses
  JPG, VectorGeometry;

// ------------------
// ------------------ TGLJPEGImage ------------------
// ------------------

constructor TGLJPEGImage.Create;
begin
  inherited;
  FAbortLoading := false;
  FDivScale := 1;
  FDither := false;
end;

// LoadFromFile
//

procedure TGLJPEGImage.LoadFromFile(const filename: string);
var
  fs: TStream;
begin
  if FileStreamExists(fileName) then
  begin
    fs := CreateFileStream(fileName, fmOpenRead);
    try
      LoadFromStream(fs);
    finally
      fs.Free;
      ResourceName := filename;
    end;
  end
  else
    raise EInvalidRasterFile.CreateFmt('File %s not found', [filename]);
end;

// SaveToFile
//

procedure TGLJPEGImage.SaveToFile(const filename: string);
var
  fs: TStream;
begin
  fs := CreateFileStream(fileName, fmOpenWrite or fmCreate);
  try
    SaveToStream(fs);
  finally
    fs.Free;
  end;
  ResourceName := filename;
end;

// LoadFromStream
//

procedure TGLJPEGImage.LoadFromStream(stream: TStream);
var
  LinesPerCall, LinesRead: Integer;
  DestScanLine: Pointer;
  PtrInc: Integer; // DestScanLine += PtrInc * LinesPerCall
  jc: TJPEGContext;
begin
  if Stream.Size > 0 then
  begin
    FAbortLoading := False;

    FillChar(jc, sizeof(jc), 0);
    jc.err := jpeg_std_error;
    jc.common.err := @jc.err;

    jpeg_CreateDecompress(@jc.d, JPEG_LIB_VERSION, SizeOf(jc.d));
    try
      jc.progress.progress_monitor := @JPEGLIBCallback;
      jc.progress.instance := Self;
      jc.common.progress := @jc.progress;

      jpeg_stdio_src(@jc.d, Stream);
      jpeg_read_header(@jc.d, True); // require Image
      jc.d.scale_num := 1;
      jc.d.scale_denom := FDivScale;
      jc.d.do_block_smoothing := True;
      jc.d.dct_method := JDCT_ISLOW;
      // Standard. Float ist unwesentlich besser, aber 20 Prozent langsamer
      if FDither then
        jc.d.dither_mode := JDITHER_FS
          // Ordered ist nicht schneller, aber häßlicher
      else
        jc.d.dither_mode := JDITHER_NONE;
      jc.FinalDCT := jc.d.dct_method;
      jc.FinalTwoPassQuant := jc.d.two_pass_quantize; // True
      jc.FinalDitherMode := jc.d.dither_mode; // FS

      if jpeg_has_multiple_scans(@jc.d) then
      begin // maximaler Speed beim Progressing, Hi Q erst beim letzten Scan
        jc.d.enable_2pass_quant := jc.d.two_pass_quantize;
        jc.d.dct_method := JDCT_IFAST;
        jc.d.two_pass_quantize := False;
        jc.d.dither_mode := JDITHER_ORDERED;
        jc.d.buffered_image := True;
      end;

      // Format des Bitmaps und Palettenbestimmung
      if jc.d.out_color_space = JCS_GRAYSCALE then
      begin
        fColorFormat := GL_LUMINANCE;
        fInternalFormat := tfLUMINANCE8;
        fElementSize := 1;
      end
      else
      begin
        fColorFormat := GL_BGR;
        fInternalFormat := tfRGB8;
        fElementSize := 3;
      end;
      fDataType := GL_UNSIGNED_BYTE;

      jpeg_start_decompress(@jc.d); // liefert erst einmal JPGInfo
      fWidth := jc.d.output_width;
      fHeight := jc.d.output_height;
      fDepth := 0;
      fMipLevels := 1;
      fCubeMap := false;
      fTextureArray := false;
      ReallocMem(fData, DataSize);
      fLevels.Clear;
      DestScanLine := fData;
      PtrInc := Integer(fData) - Integer(DestScanline) + fWidth * fElementSize;
      if (PtrInc > 0) and ((PtrInc and 3) = 0) then
        LinesPerCall := jc.d.rec_outbuf_height // mehrere Scanlines pro Aufruf
      else
        LinesPerCall := 1; // dabei wird's wohl einstweilen bleiben...

      if jc.d.buffered_image then
        // progressiv. Decoding mit Min Quality (= max speed)
      begin
        while jpeg_consume_input(@jc.d) <> JPEG_REACHED_EOI do
        begin
          jpeg_start_output(@jc.d, jc.d.input_scan_number);
          // ein kompletter Pass. Reset Oberkante progressives Display
          DestScanLine := fData;
          while (jc.d.output_scanline < jc.d.output_height) do
          begin
            LinesRead := jpeg_read_scanlines(@jc.d, @DestScanline,
              LinesPerCall);
            Inc(Integer(DestScanline), PtrInc * LinesRead);
            if FAbortLoading then
              Exit;
          end;
          jpeg_finish_output(@jc.d);
        end;
        // für den letzten Pass die tatsächlich gewünschte Ausgabequalität
        jc.d.dct_method := jc.FinalDCT;
        jc.d.dither_mode := jc.FinalDitherMode;
        if jc.FinalTwoPassQuant then
        begin
          jc.d.two_pass_quantize := True;
          jc.d.colormap := nil;
        end;
        jpeg_start_output(@jc.d, jc.d.input_scan_number);
        DestScanLine := fData;
      end;

      // letzter Pass für progressive JPGs, erster & einziger für Baseline-JPGs
      while (jc.d.output_scanline < jc.d.output_height) do
      begin
        LinesRead := jpeg_read_scanlines(@jc.d, @DestScanline, LinesPerCall);
        Inc(Integer(DestScanline), PtrInc * LinesRead);
        if FAbortLoading then
          Exit;
      end;

      if jc.d.buffered_image then
        jpeg_finish_output(@jc.d);
      jpeg_finish_decompress(@jc.d);

    finally
      if jc.common.err <> nil then
        jpeg_destroy(@jc.common);
      jc.common.err := nil;
    end;
  end;
end;

procedure TGLJPEGImage.SaveToStream(stream: TStream);
begin
  //
end;

// AssignFromTexture
//

procedure TGLJPEGImage.AssignFromTexture(textureContext: TGLContext;
  const textureHandle: TGLenum;
  textureTarget: TGLenum;
  const CurrentFormat: Boolean;
  const intFormat: TGLInternalFormat);
begin
  //
end;

// Capabilities
//

class function TGLJPEGImage.Capabilities: TDataFileCapabilities;
begin
  Result := [dfcRead {, dfcWrite}];
end;

initialization
  { Register this Fileformat-Handler with GLScene }
  RegisterRasterFormat('jpg', 'Joint Photographic Experts Group Image',
    TGLJPEGImage);
  RegisterRasterFormat('jpeg', 'Joint Photographic Experts Group Image',
    TGLJPEGImage);
   RegisterRasterFormat('jpe', 'Joint Photographic Experts Group Image',
    TGLJPEGImage);
end.

