//
// This unit is part of the GLScene Project, http://glscene.org
//
{: GLFilePGM<p>

 <b>History : </b><font size=-1><ul>
        <li>04/02/10 - Yar - Creation
   </ul><p>
}
unit GLFilePGM;

{$I GLScene.inc}

interface

uses
  Classes, SysUtils,
  OpenGL1x, GLContext, GLGraphics, GLTextureFormat,
  ApplicationFileIO;

type

  TGLPGMImage = class ( TGLBaseImage )
  public
    class function Capabilities : TDataFileCapabilities; override;

    procedure LoadFromFile(const  filename: string); override;
    procedure SaveToFile(const filename: string); override;
    procedure LoadFromStream(stream : TStream); override;
    procedure SaveToStream(stream : TStream); override;

    procedure AssignFromTexture(textureContext: TGLContext;
                                const textureHandle: TGLenum;
                                textureTarget: TGLenum;
                                const CurrentFormat: Boolean;
                                const intFormat: TGLInternalFormat); override;

    property Data           : PGLPixel32Array read FData;
    property Width          : Integer read fWidth;
    property Height         : Integer read fHeight;
    property ColorFormat    : GLenum  read fColorFormat;
    property InternalFormat : TGLInternalFormat read fInternalFormat;
    property DataType       : GLenum  read fDataType;
    property ElementSize    : Integer read fElementSize;
	end;

implementation

uses
  GLS_CUDA_Utility, VectorGeometry;

resourcestring
  cCUTILFailed = 'Can not initialize cutil32.dll';

// ------------------
// ------------------ TGLPGMImage ------------------
// ------------------

// LoadFromFile
//
procedure TGLPGMImage.LoadFromFile(const filename: string);
var
  w, h: Integer;
  cutBuffer: PSingle;
begin
  if FileExists(fileName) then
  begin
    if not IsCUTILInitialized then
      if not InitCUTIL then
      begin
        EInvalidRasterFile.Create(cCUTILFailed);
        exit;
      end;
    cutBuffer := nil;
    if cutLoadPGMf(PAnsiChar(AnsiString(filename)), cutBuffer, w, h) then
    begin
      ResourceName  := filename;
      fWidth := w;
      fHeight := h;
      fDepth := 0;
      fColorFormat    := GL_LUMINANCE;
      fInternalFormat := tfLUMINANCE_FLOAT32;
      fDataType       := GL_FLOAT;
      fCubeMap        := false;
      fTextureArray   := false;
      fMipLevels      := 1;
      fLevels.Clear;  fLevels.Add(nil);
      fElementSize    := GetTextureElementSize(tfLUMINANCE_FLOAT32);
      ReallocMem(fData, DataSize);
      Move(cutBuffer^, fData^, DataSize);
      cutFree(cutBuffer);
    end;
  end
  else raise EInvalidRasterFile.CreateFmt('File %s not found', [filename]);
end;

// SaveToFile
//
procedure TGLPGMImage.SaveToFile(const filename: string);
begin
  if not IsCUTILInitialized then
    if not InitCUTIL then
    begin
      EInvalidRasterFile.Create(cCUTILFailed);
      exit;
    end;
  if not cutSavePGMf(PAnsiChar(AnsiString(filename)), PSingle(fData), fWidth, fHeight) then
    raise EInvalidRasterFile.Create('Saving to file failed');
end;

procedure TGLPGMImage.LoadFromStream(stream : TStream);
begin
  Assert(False, 'Stream loading not supported');
end;

procedure TGLPGMImage.SaveToStream(stream : TStream);
begin
  Assert(False, 'Stream saving not supported');
end;

// AssignFromTexture
//
procedure TGLPGMImage.AssignFromTexture(textureContext: TGLContext;
                                        const textureHandle: TGLenum;
                                        textureTarget: TGLenum;
                                        const CurrentFormat: Boolean;
                                        const intFormat: TGLInternalFormat);
var
  oldContext : TGLContext;
  contextActivate : Boolean;
  texFormat, texResident: Cardinal;
  residentFormat : TGLInternalFormat;

begin
  if not ((textureTarget=GL_TEXTURE_2D)
  or (textureTarget=GL_TEXTURE_RECTANGLE)) then Exit;

  oldContext:=CurrentGLContext;
  contextActivate:=(oldContext<>textureContext);
  if contextActivate then
  begin
    if Assigned(oldContext) then oldContext.Deactivate;
    textureContext.Activate;
  end;

  try
    textureContext.GLStates.SetGLCurrentTexture(0, textureTarget, textureHandle);
    //Check for texture is resident in texture memory
    glGetTexParameteriv(textureTarget, GL_TEXTURE_RESIDENT, @texResident);
    if texResident=GL_TRUE then
    begin
      fMipLevels := 0;
      fCubeMap  := false;
      fTextureArray := false;
      fColorFormat := GL_LUMINANCE;
      fDataType := GL_FLOAT;
      // Check level existence
      glGetTexLevelParameteriv(textureTarget, 0, GL_TEXTURE_INTERNAL_FORMAT, @texFormat);
      if texFormat > 1 then
      begin
        glGetTexLevelParameteriv(textureTarget, 0, GL_TEXTURE_WIDTH, @fWidth);
        glGetTexLevelParameteriv(textureTarget, 0, GL_TEXTURE_HEIGHT, @fHeight);
        fDepth:=0;
        residentFormat := OpenGLFormatToInternalFormat( texFormat );
        if CurrentFormat then
          fInternalFormat := residentFormat
        else
          fInternalFormat := intFormat;
        Inc(fMipLevels);
      end;
      if fMipLevels>0 then
      begin
        fElementSize := GetTextureElementSize(fColorFormat, fDataType);
        ReallocMem(FData, DataSize);
        fLevels.Clear;
        fLevels.Add(fData);
        glGetTexImage(textureTarget, 0, fColorFormat, fDataType, fData);
      end
      else fMipLevels:=1;
    end;
    CheckOpenGLError;
  finally
    if contextActivate then
    begin
      textureContext.Deactivate;
      if Assigned(oldContext) then oldContext.Activate;
    end;
  end;
end;

// Capabilities
//
class function TGLPGMImage.Capabilities : TDataFileCapabilities;
begin
   Result:=[dfcRead, dfcWrite];
end;

initialization
   { Register this Fileformat-Handler with GLScene }
   RegisterRasterFormat('pgm','Portable Graymap', TGLPGMImage);

end.
