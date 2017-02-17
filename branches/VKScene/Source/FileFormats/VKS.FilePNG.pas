//
// VKScene Component Library, based on GLScene http://glscene.sourceforge.net 
//

unit VKS.FilePNG;

interface

{$I VKScene.inc}

uses
  Winapi.OpenGL,
  Winapi.OpenGLext,
  System.Classes,
  System.SysUtils,
  
  VKS.OpenGLAdapter,
  VKS.CrossPlatform,
  VKS.Context,
  VKS.Graphics,
  VKS.TextureFormat,
  VKS.ApplicationFileIO;

type

  TVKPNGImage = class(TVKBaseImage)
  private
  public
    class function Capabilities: TVKDataFileCapabilities; override;

    procedure LoadFromFile(const filename: string); override;
    procedure SaveToFile(const filename: string); override;
    procedure LoadFromStream(stream: TStream); override;
    procedure SaveToStream(stream: TStream); override;

    { Assigns from any Texture.}
    procedure AssignFromTexture(textureContext: TVKContext;
      const textureHandle: GLuint;
      textureTarget: TVKTextureTarget;
      const CurrentFormat: Boolean;
      const intFormat: GLinternalFormat); reintroduce;
  end;

implementation


// ------------------
// ------------------ TVKPNGImage ------------------
// ------------------

// LoadFromFile
//

procedure TVKPNGImage.LoadFromFile(const filename: string);
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

procedure TVKPNGImage.SaveToFile(const filename: string);
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

procedure TVKPNGImage.LoadFromStream(stream: TStream);
begin
  //Do nothing
end;

// SaveToStream
//

procedure TVKPNGImage.SaveToStream(stream: TStream);
begin
  //Do nothing
end;

// AssignFromTexture
//

procedure TVKPNGImage.AssignFromTexture(textureContext: TVKContext;
  const textureHandle: GLuint;
  textureTarget: TVKTextureTarget;
  const CurrentFormat: Boolean;
  const intFormat: GLinternalFormat);
var
  oldContext: TVKContext;
  contextActivate: Boolean;
  texFormat: Cardinal;
  residentFormat: GLinternalFormat;
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
      residentFormat := VulkanFormatToInternalFormat(texFormat);
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

// Capabilities
//

class function TVKPNGImage.Capabilities: TVKDataFileCapabilities;
begin
  Result := [dfcRead, dfcWrite];
end;

initialization
  { Register this Fileformat-Handler with GLScene }
  RegisterRasterFormat('png', 'Portable Network Graphic', TVKPNGImage);

end.
