//
// VXScene Component Library, based on GLScene http://glscene.sourceforge.net 
//

unit VXS.FilePNG;

interface

{$I VXScene.inc}

uses
  Winapi.OpenGL,
  Winapi.OpenGLext,
  System.Classes,
  System.SysUtils,
  
  VXS.OpenGLAdapter,
  VXS.CrossPlatform,
  VXS.Context,
  VXS.Graphics,
  VXS.TextureFormat,
  VXS.ApplicationFileIO;

type

  TVXPNGImage = class(TVXBaseImage)
  private
  public
    class function Capabilities: TVXDataFileCapabilities; override;

    procedure LoadFromFile(const filename: string); override;
    procedure SaveToFile(const filename: string); override;
    procedure LoadFromStream(stream: TStream); override;
    procedure SaveToStream(stream: TStream); override;

    { Assigns from any Texture.}
    procedure AssignFromTexture(textureContext: TVXContext;
      const textureHandle: GLuint;
      textureTarget: TVXTextureTarget;
      const CurrentFormat: Boolean;
      const intFormat: TVXInternalFormat); reintroduce;
  end;

//==============================================================
implementation
//==============================================================


// ------------------
// ------------------ TVXPNGImage ------------------
// ------------------

procedure TVXPNGImage.LoadFromFile(const filename: string);
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

procedure TVXPNGImage.SaveToFile(const filename: string);
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

procedure TVXPNGImage.LoadFromStream(stream: TStream);
begin
  //Do nothing
end;

procedure TVXPNGImage.SaveToStream(stream: TStream);
begin
  //Do nothing
end;

procedure TVXPNGImage.AssignFromTexture(textureContext: TVXContext;
  const textureHandle: GLuint;
  textureTarget: TVXTextureTarget;
  const CurrentFormat: Boolean;
  const intFormat: TVXInternalFormat);
var
  oldContext: TVXContext;
  contextActivate: Boolean;
  texFormat: Cardinal;
  residentFormat: TVXInternalFormat;
  glTarget: GLEnum;
begin
  if not ((textureTarget = ttTexture2D)
    or (textureTarget = ttTextureRect)) then
    Exit;

  oldContext := CurrentVKContext;
  contextActivate := (oldContext <> textureContext);
  if contextActivate then
  begin
    if Assigned(oldContext) then
      oldContext.Deactivate;
    textureContext.Activate;
  end;
  glTarget := DecodeTextureTarget(textureTarget);

  try
    textureContext.VKStates.TextureBinding[0, textureTarget] := textureHandle;
    fLevelCount := 0;
    fCubeMap := false;
    fTextureArray := false;
    // Check level existence
    glGetTexLevelParameteriv(glTarget, 0, GL_TEXTURE_INTERNAL_FORMAT,
      @texFormat);
    if texFormat > 1 then
    begin
      glGetTexLevelParameteriv(glTarget, 0, GL_TEXTURE_WIDTH, @FLOD[0].Width);
      glGetTexLevelParameteriv(glTarget, 0, GL_TEXTURE_HEIGHT, @FLOD[0].Height);
      FLOD[0].Depth := 0;
      residentFormat := OpenVXFormatToInternalFormat(texFormat);
      if CurrentFormat then
        fInternalFormat := residentFormat
      else
        fInternalFormat := intFormat;
      FindCompatibleDataFormat(fInternalFormat, fColorFormat, fDataType);
      Inc(fLevelCount);
    end;
    if fLevelCount > 0 then
    begin
      fElementSize := GetTextureElementSize(fColorFormat, fDataType);
      ReallocMem(FData, DataSize);
      glGetTexImage(glTarget, 0, fColorFormat, fDataType, fData);
    end
    else
      fLevelCount := 1;
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

class function TVXPNGImage.Capabilities: TVXDataFileCapabilities;
begin
  Result := [dfcRead, dfcWrite];
end;

//----------------------------------------------------------
initialization
//----------------------------------------------------------

  { Register this Fileformat-Handler with VXScene }
  RegisterRasterFormat('png', 'Portable Network Graphic', TVXPNGImage);

end.
