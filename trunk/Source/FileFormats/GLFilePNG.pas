//
// This unit is part of the GLScene Project, http://glscene.org
//
{: GLFilePNG<p>

 <b>History : </b><font size=-1><ul>
        <li>05/03/10 - Yar - Creation
   </ul><p>
}
unit GLFilePNG;

interface

{$I GLScene.inc}

uses
  Classes, SysUtils,
  OpenGL1x, GLContext, GLGraphics, GLTextureFormat,
  ApplicationFileIO;

type

  TGLPNGImage = class(TGLBaseImage)
  private
  public
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
    property ColorFormat: GLenum read fColorFormat;
    property InternalFormat: TGLInternalFormat read fInternalFormat;
    property DataType: GLenum read fDataType;
    property ElementSize: Integer read fElementSize;
  end;

implementation

uses
  libpng;

resourcestring
  sLIBPNGerror = 'LIBPNG error';

// ------------------
// ------------------ TGLPNGImage ------------------
// ------------------

// LoadFromFile
//

procedure TGLPNGImage.LoadFromFile(const filename: string);
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

procedure TGLPNGImage.SaveToFile(const filename: string);
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

procedure TGLPNGImage.LoadFromStream(stream: TStream);
var
  sig: array[0..7] of Byte;
  png_ptr: png_structp;
  info_ptr: png_infop;
  colorType, bitDepth: Integer;
  rowBytes: Integer;
  rowPointers: array of PGLUbyte;
  ii: Integer;
  use16: Boolean;
begin
  stream.Read(sig, 8);
  if _png_sig_cmp(@sig, 0, 8) <> 0 then
    raise EInvalidRasterFile.Create('Invalid PNG file');

  png_ptr := _png_create_read_struct(ZLIB_VERSION, nil, nil, nil);
  if not Assigned(png_ptr) then
    raise EInvalidRasterFile.Create(sLIBPNGerror);

  info_ptr := _png_create_info_struct(png_ptr);
  if not Assigned(png_ptr) then
  begin
    _png_destroy_read_struct(@png_ptr, nil, nil);
    raise EInvalidRasterFile.Create(sLIBPNGerror);
  end;

  try
    {: Need to override the standard I/O methods since libPNG
       may be linked against a different run-time }
    _png_set_read_fn(png_ptr, stream, pngReadFn);
    // skip the sig bytes
    _png_set_sig_bytes(png_ptr, 8);
    // automagically read everything to the image data
    _png_read_info(png_ptr, info_ptr);

    fWidth := _png_get_image_width(png_ptr, info_ptr);
    fHeight := _png_get_image_height(png_ptr, info_ptr);
    // using the convention of depth = 0 for 2D images
    fDepth := 0;

    colorType := _png_get_color_type(png_ptr, info_ptr);
    bitDepth := _png_get_bit_depth(png_ptr, info_ptr);

    {: Setup the read transforms
       expand palette images to RGB and low-bit-depth grayscale images to 8 bits
       convert transparency chunks to full alpha channel }
    if colorType = PNG_COLOR_TYPE_PALETTE then
      _png_set_palette_to_rgb(png_ptr);

    if (colorType = PNG_COLOR_TYPE_GRAY) and (bitDepth < 8) then
      _png_set_expand_gray_1_2_4_to_8(png_ptr);

    if _png_get_valid(png_ptr, info_ptr, PNG_INFO_tRNS) <> 0 then
      _png_set_tRNS_to_alpha(png_ptr);

    // now configure for reading, and allocate the memory
    _png_read_update_info(png_ptr, info_ptr);

    rowBytes := _png_get_rowbytes(png_ptr, info_ptr);

    ReallocMem(fData, rowBytes * fHeight);

    SetLength(rowPointers, fHeight);

    // set up the row pointers
    for ii := 0 to fHeight - 1 do
      rowPointers[ii] := PGLUbyte(Integer(fData) + (fHeight - 1 - ii) *
        rowBytes);

    // read the image
    _png_read_image(png_ptr, @rowPointers[0]);

    use16 := bitDepth > 8;

    if use16 then
      fDataType := GL_UNSIGNED_SHORT
    else
      fDataType := GL_UNSIGNED_BYTE;

    case _png_get_channels(png_ptr, info_ptr) of
      1:
        begin
          fColorFormat := GL_LUMINANCE;
          if use16 then
          begin
            fInternalFormat := tfLUMINANCE16;
            fElementSize := 2;
          end
          else
          begin
            fInternalFormat := tfLUMINANCE8;
            fElementSize := 1;
          end;
        end;
      2:
        begin
          fColorFormat := GL_LUMINANCE_ALPHA;
          if use16 then
          begin
            fInternalFormat := tfLUMINANCE16_ALPHA16;
            fElementSize := 4;
          end
          else
          begin
            fInternalFormat := tfLUMINANCE8_ALPHA8;
            fElementSize := 2;
          end;
        end;
      3:
        begin
          fColorFormat := GL_RGB;
          if use16 then
          begin
            fInternalFormat := tfR16G16B16;
            fElementSize := 6;
          end
          else
          begin
            fInternalFormat := tfRGB8;
            fElementSize := 3;
          end;
        end;
      4:
        begin
          fColorFormat := GL_RGBA;
          if use16 then
          begin
            fInternalFormat := tfR16G16B16A16;
            fElementSize := 8;
          end
          else
          begin
            fInternalFormat := tfRGBA8;
            fElementSize := 4;
          end;
        end;
    end;

    fCubeMap := false;
    fTextureArray := false;
    fLevels.Clear;
    fLevels.Add(nil);

    _png_read_end(png_ptr, nil);
  finally
    _png_destroy_read_struct(@png_ptr, @info_ptr, nil);
  end;
end;

// SaveToStream
//

procedure TGLPNGImage.SaveToStream(stream: TStream);
var
  png_ptr: png_structp;
  info_ptr: png_infop;
  bit_depth, color_type, rowBytes: Integer;
  canSave: Boolean;
  rowPointers: array of PGLUbyte;
  ii: Integer;
begin
  png_ptr := _png_create_write_struct(ZLIB_VERSION, nil, nil, nil);
  if not Assigned(png_ptr) then
    raise EInvalidRasterFile.Create(sLIBPNGerror);

  info_ptr := _png_create_info_struct(png_ptr);
  if not Assigned(png_ptr) then
  begin
    _png_destroy_write_struct(@png_ptr, nil);
    raise EInvalidRasterFile.Create(sLIBPNGerror);
  end;

  try
    {: Need to override the standard I/O methods since
      libPNG may be linked against a different run-time }
    _png_set_write_fn(png_ptr, stream, pngWriteFn, nil);

    bit_depth := fElementSize * 8;
    color_type := PNG_COLOR_TYPE_GRAY;
    rowBytes := fWidth * fElementSize;
    canSave := true;
    case fDataType of
      GL_UNSIGNED_BYTE: bit_depth := 8;
      GL_UNSIGNED_SHORT: bit_depth := 16;
    else
      canSave := false;
    end;

    case fColorFormat of
      GL_LUMINANCE: color_type := PNG_COLOR_TYPE_GRAY;
      GL_LUMINANCE_ALPHA: color_type := PNG_COLOR_TYPE_GRAY_ALPHA;
      GL_RGB: color_type := PNG_COLOR_TYPE_RGB;
      GL_RGBA: color_type := PNG_COLOR_TYPE_RGB_ALPHA;
    else
      canSave := false;
    end;

    if not canSave then
      raise
        EInvalidRasterFile.Create('These image format do not match the PNG format specification.');

    _png_set_IHDR(png_ptr, info_ptr, fWidth, fHeight, bit_depth, color_type,
      PNG_INTERLACE_NONE, PNG_COMPRESSION_TYPE_DEFAULT,
        PNG_FILTER_TYPE_DEFAULT);

    // write the file header information
    _png_write_info(png_ptr, info_ptr);

    SetLength(rowPointers, fHeight);

    // set up the row pointers
    for ii := 0 to fHeight - 1 do
      rowPointers[ii] := PGLUbyte(Integer(fData) + (fHeight - 1 - ii) *
        rowBytes);

    _png_write_image(png_ptr, @rowPointers[0]);
    _png_write_end(png_ptr, info_ptr);
  finally
    _png_destroy_write_struct(@png_ptr, @info_ptr);
  end;
end;

// AssignFromTexture
//

procedure TGLPNGImage.AssignFromTexture(textureContext: TGLContext;
  const textureHandle: TGLenum;
  textureTarget: TGLenum;
  const CurrentFormat: Boolean;
  const intFormat: TGLInternalFormat);
var
  oldContext: TGLContext;
  contextActivate: Boolean;
  texFormat, texResident: Cardinal;
  residentFormat: TGLInternalFormat;
begin
  if not ((textureTarget = GL_TEXTURE_2D)
    or (textureTarget = GL_TEXTURE_RECTANGLE)) then
    Exit;

  oldContext := CurrentGLContext;
  contextActivate := (oldContext <> textureContext);
  if contextActivate then
  begin
    if Assigned(oldContext) then
      oldContext.Deactivate;
    textureContext.Activate;
  end;

  try
    textureContext.GLStates.SetGLCurrentTexture(0, textureTarget,
      textureHandle);
    //Check for texture is resident in texture memory
    glGetTexParameteriv(textureTarget, GL_TEXTURE_RESIDENT, @texResident);
    if texResident = GL_TRUE then
    begin
      fMipLevels := 0;
      fCubeMap := false;
      fTextureArray := false;
      // Check level existence
      glGetTexLevelParameteriv(textureTarget, 0, GL_TEXTURE_INTERNAL_FORMAT,
        @texFormat);
      if texFormat > 1 then
      begin
        glGetTexLevelParameteriv(textureTarget, 0, GL_TEXTURE_WIDTH, @fWidth);
        glGetTexLevelParameteriv(textureTarget, 0, GL_TEXTURE_HEIGHT, @fHeight);
        fDepth := 0;
        residentFormat := OpenGLFormatToInternalFormat(texFormat);
        if CurrentFormat then
          fInternalFormat := residentFormat
        else
          fInternalFormat := intFormat;
        FindCompatibleDataFormat(fInternalFormat, fColorFormat, fDataType);
        Inc(fMipLevels);
      end;
      if fMipLevels > 0 then
      begin
        fElementSize := GetTextureElementSize(fColorFormat, fDataType);
        ReallocMem(FData, DataSize);
        fLevels.Clear;
        fLevels.Add(fData);
        glGetTexImage(textureTarget, 0, fColorFormat, fDataType, fData);
      end
      else
        fMipLevels := 1;
    end;
    CheckOpenGLError;
  finally
    if contextActivate then
    begin
      textureContext.Deactivate;
      if Assigned(oldContext) then
        oldContext.Activate;
    end;
  end;
end;

// Capabilities
//

class function TGLPNGImage.Capabilities: TDataFileCapabilities;
begin
  Result := [dfcRead, dfcWrite];
end;

initialization
  { Register this Fileformat-Handler with GLScene }
  RegisterRasterFormat('png', 'Portable Network Graphic', TGLPNGImage);

end.
