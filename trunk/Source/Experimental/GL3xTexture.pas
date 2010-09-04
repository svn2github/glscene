//
// This unit is part of the GLScene Project, http://glscene.org
//
{: GL3xTexture <p>

   <b>History : </b><font size=-1><ul>
      <li>23/08/10 - Yar - Creation
   </ul></font><p>
}

unit GL3xTexture;

interface

uses
{$IFDEF MSWINDOWS}
  Windows,
{$ENDIF}
  Classes,
  BaseClasses,
  ApplicationFileIO,
{$IFDEF FPC}
  LResources,
{$ENDIF}
  OpenGLTokens,
  GLContext,
  GLColor,
  GLRenderContextInfo,
  GLState,
  GLGraphics,
  GLShaderManager,
  GLTextureFormat;

type

  TGL3xTextureName = string;

  TGLTextureFilter = (filterNearest, filterLinear);

  TGLTextureAddress =
    (
    taRepeat,
    taClamp,
    taClampToEdge,
    taClampToBorder,
    taMirrorRepeat,
    taMirrorClamp,
    taMirrorClampToEdge,
    taMirrorClampToBorder
    );

  // Specifies the texture comparison mode for currently bound depth textures.
  // That is, a texture whose internal format is tfDEPTH_COMPONENT*
  TGLTextureCompareMode = (tcmNone, tcmCompareRtoTexture);

  TGLTextureColor = class(TGLColor) end;

  TGLSamplerParam = class(TDataFile)
  private
    { Private Declarations }
    FFilter: TGLTextureFilter;
    FLODBias: Single;
    FAddress: array[0..2] of TGLTextureAddress;
    FBorderColor: TGLTextureColor;
    FCompareMode: TGLTextureCompareMode;
    FCompareFunc: TDepthFunction;
    procedure SetFilter(Value: TGLTextureFilter);
    procedure SetLODBias(Value: Single);
    function GetAddress(Index: Integer): TGLTextureAddress;
    procedure SetAddress(Index: Integer; Value: TGLTextureAddress);
    procedure SetBorderColor(const Value: TGLTextureColor);
    procedure SetCompareMode(Value: TGLTextureCompareMode);
    procedure SetCompareFunc(Value: TDepthFunction);
  public
    { Public Declarations }
    constructor Create(AOwner: TPersistent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;

    procedure LoadFromFile(const fileName: string); override;
    procedure SaveToFile(const fileName: string); override;
    procedure LoadFromStream(stream: TStream); override;
    procedure SaveToStream(stream: TStream); override;
    class function Capabilities: TDataFileCapabilities; override;

    procedure ApplyToTexture(ATarget: TGLEnum);
    procedure ApplyToSampler(GLName: TGLuint);

    property CompareMode: TGLTextureCompareMode read FCompareMode write SetCompareMode;
    property CompareFunc: TDepthFunction read FCompareFunc write SetCompareFunc;
  published
    { Published Declarations }

    {: Texture filter. }
    property Filter: TGLTextureFilter read FFilter write SetFilter default
      filterLinear;
    {: Texture LOD bias }
    property LodBias: Single read FLODBias write SetLODBias;
    {: Address mode for the texture. }
    property AddressX: TGLTextureAddress index 0 read GetAddress write
      SetAddress default taRepeat;
    property AddressY: TGLTextureAddress index 1 read GetAddress write
      SetAddress default taRepeat;
    property AddressZ: TGLTextureAddress index 2 read GetAddress write
      SetAddress default taRepeat;
    {: Texture Border color. }
    property BorderColor: TGLTextureColor read FBorderColor write SetBorderColor;
  end;

  {
      TEXTURE_BORDER_COLOR       0,0,0,0                | Border color
      TEXTURE_MIN_FILTER         NEAREST_MIPMAP_LINEAR  | Minification function
      TEXTURE_MAG_FILTER         LINEAR                 | Magnification function
      TEXTURE_WRAP_S             REPEAT                 | Texcoord s wrap mode
      TEXTURE_WRAP_T             REPEAT                 | Texcoord t wrap mode
      TEXTURE_WRAP_R             REPEAT                 | Texcoord r wrap mode
      TEXTURE_MIN_LOD            -1000                  | Minimum level of detail
      TEXTURE_MAX_LOD            1000                   | Maximum level of detail
      TEXTURE_LOD_BIAS           0.0                    | Texture level of detail
      TEXTURE_COMPARE_MODE       NONE                   | Comparison mode
      TEXTURE_COMPARE_FUNC       LEQUAL                 | Comparison function
      TEXTURE_MAX_ANISTROPY_EXT
  }

  TGL3xTexture = class(TDataFile)
  private
    { Private Declarations }
    FHandle: TGLTextureHandle;
    FName: TGL3xTextureName;
    FHashCode: Integer;
    FImage: TGLImage;
    FTarget: TGLTextureTarget;
    FWidth: Integer;
    FHeight: Integer;
    FDepth: Integer;
    FTextureFormat: TGLInternalFormat;
    FSampler: TGLSamplerParam;
    FSamplerChanged: Boolean;
    procedure SetName(const AName: string);
    function GetReady: Boolean;
  protected
    { Protected Declarations }
  public
    { Public Declarations }
    constructor Create(AOwner: TPersistent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure NotifyChange(Sender: TObject); override;

    procedure LoadFromFile(const fileName: string); override;
    procedure SaveToFile(const fileName: string); override;
    procedure LoadFromStream(stream: TStream); override;
    procedure SaveToStream(stream: TStream); override;
    procedure Initialize; override;
    class function Capabilities: TDataFileCapabilities; override;

    procedure Apply(UnitIndex: TGLuint);
    property Name: TGL3xTextureName read FName write SetName;
    property HashCode: Integer read FHashCode;
    property IsReadyToWork: Boolean read GetReady;
    property Sampler: TGLSamplerParam read FSampler;
  end;

implementation

uses
  GL3xMaterial,
  SysUtils,
  GLStrings,
  GLSLog;

const
  cTextureMagFilter: array[maNearest..maLinear] of TGLEnum =
    (GL_NEAREST, GL_LINEAR);

  cTextureMinFilter: array[miNearest..miLinearMipmapLinear] of TGLEnum =
    (GL_NEAREST, GL_LINEAR, GL_NEAREST_MIPMAP_NEAREST,
    GL_LINEAR_MIPMAP_NEAREST, GL_NEAREST_MIPMAP_LINEAR,
    GL_LINEAR_MIPMAP_LINEAR);

  //  cFilteringQuality: array[tfIsotropic..tfAnisotropic] of Integer = (1, 2);

  cTextureAddressMode: array[taRepeat..taMirrorClampToBorder] of TGLenum =
    (GL_REPEAT, GL_CLAMP, GL_CLAMP_TO_EDGE, GL_CLAMP_TO_BORDER,
    GL_MIRRORED_REPEAT, GL_MIRROR_CLAMP_ATI,
    GL_MIRROR_CLAMP_TO_EDGE_ATI, GL_MIRROR_CLAMP_TO_BORDER_EXT);

  cTextureCompareMode: array[tcmNone..tcmCompareRtoTexture] of TGLenum =
    (GL_NONE, GL_COMPARE_R_TO_TEXTURE);

  // ------------------
  // ------------------ TGLSamplerParam ------------------
  // ------------------

{$IFDEF GLS_COMPILER_2005_UP}{$REGION 'TGLSamplerParam'}{$ENDIF}

constructor TGLSamplerParam.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner);
  FFilter := filterLinear;
  FLODBias := 0.0;
  FAddress[0] := taRepeat;
  FAddress[1] := taRepeat;
  FAddress[2] := taRepeat;
  FBorderColor := TGLTextureColor.CreateInitialized(Self, clrTransparent);
  FCompareMode := tcmNone;
  FCompareFunc := cfLequal;
end;

destructor TGLSamplerParam.Destroy;
begin
  FBorderColor.Free;
  inherited;
end;

procedure TGLSamplerParam.Assign(Source: TPersistent);
var
  Sampler: TGLSamplerParam;
begin
  if Source is TGLSamplerParam then
  begin
    Sampler := TGLSamplerParam(Source);
    FFilter := Sampler.FFilter;
    FLODBias := Sampler.FLODBias;
    FAddress := Sampler.FAddress;
    FBorderColor.Assign(Sampler.BorderColor);
    NotifyChange(Self);
  end
  else
    inherited;
end;

procedure TGLSamplerParam.LoadFromFile(const fileName: string);
var
  fs: TStream;
begin
  if FileStreamExists(fileName) then
  begin
    fs := CreateFileStream(fileName, fmOpenRead);
    ResourceName := filename;
    try
      LoadFromStream(fs);
    finally
      fs.Free;
    end;
  end
  else
    GLSLogger.LogError(Format('File %s not found', [filename]));
end;

procedure TGLSamplerParam.SaveToFile(const fileName: string);
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

procedure TGLSamplerParam.LoadFromStream(stream: TStream);
var
  Temp: Integer;
begin
  stream.ReadBuffer(Temp, SizeOf(Integer)); // reserved
  stream.ReadBuffer(Temp, SizeOf(Integer));
  FFilter := TGLTextureFilter(Temp);
  stream.ReadBuffer(FLODBias, SizeOf(Single));
  stream.ReadBuffer(Temp, SizeOf(Integer));
  FAddress[0] := TGLTextureAddress(Temp);
  stream.ReadBuffer(Temp, SizeOf(Integer));
  FAddress[1] := TGLTextureAddress(Temp);
  stream.ReadBuffer(Temp, SizeOf(Integer));
  FAddress[2] := TGLTextureAddress(Temp);
  FBorderColor.ReadData(stream);
  stream.ReadBuffer(Temp, SizeOf(Integer));
  FCompareMode := TGLTextureCompareMode(Temp);
  stream.ReadBuffer(Temp, SizeOf(Integer));
  FCompareFunc := TDepthFunction(Temp);
  NotifyChange(Self);
end;

procedure TGLSamplerParam.SaveToStream(stream: TStream);
var
  Temp: Integer;
begin
  Temp := 0;
  stream.WriteBuffer(Temp, SizeOf(Integer)); // Reserved
  Temp := Integer(FFilter);
  stream.WriteBuffer(Temp, SizeOf(Integer));
  stream.WriteBuffer(FLODBias, SizeOf(Single));
  Temp := Integer(FAddress[0]);
  stream.WriteBuffer(Temp, SizeOf(Integer));
  Temp := Integer(FAddress[1]);
  stream.WriteBuffer(Temp, SizeOf(Integer));
  Temp := Integer(FAddress[2]);
  stream.WriteBuffer(Temp, SizeOf(Integer));
  Temp := Integer(FCompareMode);
  FBorderColor.WriteData(stream);
  stream.WriteBuffer(Temp, SizeOf(Integer));
  Temp := Integer(FCompareFunc);
  stream.WriteBuffer(Temp, SizeOf(Integer));
end;

class function TGLSamplerParam.Capabilities: TDataFileCapabilities;
begin
  Result := [dfcRead, dfcWrite];
end;

procedure TGLSamplerParam.SetFilter(Value: TGLTextureFilter);
begin
  if Value <> FFilter then
  begin
    FFilter := Value;
    NotifyChange(Self);
  end;
end;

procedure TGLSamplerParam.SetLODBias(Value: Single);
begin
  if Value <> FLODBias then
  begin
    FLODBias := Value;
    NotifyChange(Self);
  end;
end;

function TGLSamplerParam.GetAddress(Index: Integer): TGLTextureAddress;
begin
  Result := FAddress[Index];
end;

procedure TGLSamplerParam.SetAddress(Index: Integer; Value: TGLTextureAddress);
begin
  if Value <> FAddress[Index] then
  begin
    FAddress[Index] := Value;
    NotifyChange(Self);
  end;
end;

procedure TGLSamplerParam.SetBorderColor(const Value: TGLTextureColor);
begin
  FBorderColor.Assign(Value);
  NotifyChange(Self);
end;

procedure TGLSamplerParam.SetCompareMode(Value: TGLTextureCompareMode);
begin
  if Value <> FCompareMode then
  begin
    FCompareMode := Value;
    NotifyChange(Self);
  end;
end;

procedure TGLSamplerParam.SetCompareFunc(Value: TDepthFunction);
begin
  if Value <> FCompareFunc then
  begin
    FCompareFunc := Value;
    NotifyChange(Self);
  end;
end;

procedure TGLSamplerParam.ApplyToTexture(ATarget: TGLEnum);
var
  vMin: TGLMinFilter;
  vMag: TGLMagFilter;
begin
  if not ((ATarget = GL_TEXTURE_2D_MULTISAMPLE)
    or (ATarget = GL_TEXTURE_2D_MULTISAMPLE_ARRAY)) then
  begin
    // Choose filter
    case FFilter of
      filterLinear:
        begin
          vMin := miLinear;
          vMag := maLinear;
        end;
    else
      begin
        vMin := miNearest;
        vMag := maNearest;
      end;
    end;
    // Setup paprameters
    GL.TexParameterfv(ATarget, GL_TEXTURE_BORDER_COLOR, FBorderColor.AsAddress);
    GL.TexParameteri(ATarget, GL_TEXTURE_WRAP_S, cTextureAddressMode[FAddress[0]]);
    GL.TexParameteri(ATarget, GL_TEXTURE_WRAP_T, cTextureAddressMode[FAddress[1]]);
    GL.TexParameteri(ATarget, GL_TEXTURE_WRAP_R, cTextureAddressMode[FAddress[2]]);
    GL.TexParameteri(ATarget, GL_TEXTURE_MIN_FILTER, cTextureMinFilter[vMin]);
    GL.TexParameteri(ATarget, GL_TEXTURE_MAG_FILTER, cTextureMagFilter[vMag]);
    GL.TexParameteri(ATarget, GL_TEXTURE_MAX_ANISOTROPY_EXT, 0);
    GL.TexParameteri(ATarget, GL_TEXTURE_COMPARE_MODE, cTextureCompareMode[FCompareMode]);
    GL.TexParameteri(ATarget, GL_TEXTURE_COMPARE_FUNC, cGLComparisonFunctionToGLEnum[FCompareFunc]);
  end;
end;

procedure TGLSamplerParam.ApplyToSampler(GLName: TGLuint);
var
  vMin: TGLMinFilter;
  vMag: TGLMagFilter;
begin
  // Choose filter
  case FFilter of
    filterLinear:
      begin
        vMin := miLinear;
        vMag := maLinear;
      end;
  else
    begin
      vMin := miNearest;
      vMag := maNearest;
    end;
  end;
  // Setup paprameters
  GL.SamplerParameterfv(GLName, GL_TEXTURE_BORDER_COLOR, FBorderColor.AsAddress);
  GL.SamplerParameteri(GLName, GL_TEXTURE_WRAP_S, cTextureAddressMode[AddressX]);
  GL.SamplerParameteri(GLName, GL_TEXTURE_WRAP_T, cTextureAddressMode[AddressY]);
  GL.SamplerParameteri(GLName, GL_TEXTURE_WRAP_R, cTextureAddressMode[AddressZ]);
  GL.SamplerParameteri(GLName, GL_TEXTURE_MIN_FILTER, cTextureMinFilter[vMin]);
  GL.SamplerParameteri(GLName, GL_TEXTURE_MAG_FILTER, cTextureMagFilter[vMag]);
  GL.SamplerParameteri(GLName, GL_TEXTURE_MAX_ANISOTROPY_EXT, 0);
  GL.SamplerParameteri(GLName, GL_TEXTURE_COMPARE_MODE, cTextureCompareMode[FCompareMode]);
  GL.SamplerParameteri(GLName, GL_TEXTURE_COMPARE_FUNC, cGLComparisonFunctionToGLEnum[FCompareFunc]);
end;

{$IFDEF GLS_COMPILER_2005_UP}{$ENDREGION}{$ENDIF}

// ------------------
// ------------------ TGL3xTexture ------------------
// ------------------

{$IFDEF GLS_COMPILER_2005_UP}{$REGION 'TGL3xTexture'}{$ENDIF}

constructor TGL3xTexture.Create(AOwner: TPersistent);
begin
  inherited;
  FHandle := TGLTextureHandle.Create;
  FSampler := TGLSamplerParam.Create(Self);
end;

destructor TGL3xTexture.Destroy;
begin
  FHandle.Free;
  FImage.Free;
  FSampler.Free;
  inherited;
end;

procedure TGL3xTexture.Assign(Source: TPersistent);
var
  Tex: TGL3xTexture;
begin
  if Source is TGL3xTexture then
  begin
    Tex := TGL3xTexture(Source);
    FSampler.Assign(Tex.FSampler);
    if Assigned(Tex.FImage) then
    begin
      if not Assigned(FImage) then
        FImage := TGLImage.Create;
      FImage.Assign(Tex.FImage);
    end
    else
    begin
      FreeAndNil(FImage);
    end;
    ResourceName := Tex.ResourceName;
    FHandle.NotifyChangesOfData;
  end
  else
    inherited;
end;

procedure TGL3xTexture.NotifyChange(Sender: TObject);
begin
  if Sender = FSampler then
  begin
    FSamplerChanged := True;
  end
  else
    inherited;
end;

// LoadFromFile
//

procedure TGL3xTexture.LoadFromFile(const fileName: string);
var
  BaseImageClass: TGLBaseImageClass;
  tempImage: TGLBaseImage;
  rStream: {$IFNDEF FPC}TResourceStream{$ELSE}TLazarusResourceStream{$ENDIF};
begin
  if filename = '' then
  begin
    if FName = glsDIFFUSEMAP then
    begin
      rStream := CreateResourceStream(glsDIFFUSEMAP, GLS_RC_DDS_Type);
      LoadFromStream(rStream);
      rStream.Free;
    end
    else if FName = glsNORMALMAP then
    begin
      rStream := CreateResourceStream(glsNORMALMAP, GLS_RC_DDS_Type);
      LoadFromStream(rStream);
      rStream.Free;
    end
    else
      GLSLogger.LogWarning(Format('Trying to load empty file name to texture "%s"', [Name]));
    exit;
  end;

  BaseImageClass := GetRasterFileFormats.FindFromFileName(filename);
  tempImage := BaseImageClass.Create;
  try
    tempImage.LoadFromFile(fileName);
    if not Assigned(FImage) then
      FImage := TGLImage.Create;
    FImage.Assign(tempImage); // DOTO: Decide how to eliminate unnecessary movement of memory
    FWidth := FImage.Width;
    FHeight := FImage.Height;
    FDepth := FImage.Depth;
    FTextureFormat := FImage.InternalFormat;
    ResourceName := filename;
    NotifyChange(Self);
  finally
    tempImage.Free;
  end;
end;

// SaveToFile
//

procedure TGL3xTexture.SaveToFile(const fileName: string);
var
  fs: TStream;
begin
  if filename = '' then
  begin
    GLSLogger.LogWarning(Format('Trying to save empty file name of texture "%s"', [Name]));
    exit;
  end;
  fs := CreateFileStream(fileName, fmOpenWrite or fmCreate);
  ResourceName := filename;
  try
    SaveToStream(fs);
    MaterialManager.NotifyResourcesChanged;
  finally
    fs.Free;
  end;
end;

procedure TGL3xTexture.LoadFromStream(stream: TStream);
var
  tempImage: TGLBaseImage;
begin
  if (not Assigned(stream)) or (stream.Size - stream.Position < 200) then
    exit;
  with GetRasterFileFormats do
    tempImage := FindFromStream(stream).Create;
  try
    tempImage.LoadFromStream(stream);
    if not Assigned(FImage) then
      FImage := TGLImage.Create;
    FImage.Assign(tempImage);
    FTextureFormat := FImage.InternalFormat;
    FWidth := FImage.Width;
    FHeight := FImage.Height;
    FDepth := FImage.Depth;
    ResourceName := '';
    FHandle.NotifyChangesOfData;
  finally
    tempImage.Free;
  end;
end;

procedure TGL3xTexture.SaveToStream(stream: TStream);
var
  RC: TGLContext;
  BaseImageClass: TGLBaseImageClass;
  tempImage: TGLBaseImage;
begin
  RC := SafeCurrentGLContext;
  BaseImageClass := GetRasterFileFormats.FindFromFileName(ResourceName);
  tempImage := BaseImageClass.Create;
  if FHandle.Handle <> 0 then
    tempImage.AssignFromTexture(
      RC,
      FHandle.Handle,
      FTarget,
      false,
      FTextureFormat)
  else
    tempImage.Assign(FImage);
  try
    tempImage.SaveToStream(stream);
  finally
    tempImage.Free;
  end;
end;

procedure TGL3xTexture.Initialize;
var
  vTarget: TGLTextureTarget;
  glTarget, glFormat: TGLEnum;
  vMin: TGLMinFilter;
  rStream: {$IFNDEF FPC}TResourceStream{$ELSE}TLazarusResourceStream{$ENDIF};
begin
  if not Assigned(FImage) then
    LoadFromFile(ResourceName);

  if Assigned(FImage) then
  begin
    // Choose a texture target
    vTarget := ttTexture2D;
    if FImage.Height = 1 then
      vTarget := ttTexture1D;
    if FImage.CubeMap then
      vTarget := ttTextureCube;
    if FImage.IsVolume then
      vTarget := ttTexture3D;
    if FImage.TextureArray then
    begin
      if (FImage.Depth < 2) then
        vTarget := ttTexture1Darray
      else
        vTarget := ttTexture2DArray;
      if FImage.CubeMap then
        vTarget := ttTextureCube;
    end;
    if ((FImage.InternalFormat >= tfFLOAT_R16)
      and (FImage.InternalFormat <= tfFLOAT_RGBA32)) then
      vTarget := ttTextureRect;

    if vTarget <> FTarget then
      FHandle.DestroyHandle;
    FTarget := vTarget;
    glTarget := DecodeGLTextureTarget(FTarget);
    if not IsTargetSupported(glTarget) then
      exit;

    FHandle.AllocateHandle;

    with CurrentGLContext.GLStates do
    begin
      TextureBinding[0, FTarget] := FHandle.Handle;
      UnpackAlignment := 1;
      UnpackRowLength := 0;
      UnpackSkipRows := 0;
      UnpackSkipPixels := 0;
    end;

    FSampler.ApplyToTexture(glTarget);
    FSamplerChanged := False;

    glFormat := InternalFormatToOpenGLFormat(FTextureFormat);

    case FSampler.FFilter of
      filterLinear: vMin := miLinear;
    else
      vMin := miNearest;
    end;

    GL.ClearError;

    FImage.RegisterAsOpenGLTexture(
      glTarget,
      vMin,
      glFormat,
      FWidth,
      FHeight,
      FDepth);

    if GL.GetError <> GL_NO_ERROR then
    begin
      rStream := CreateResourceStream('TEXTUREERROR', GLS_RC_JPG_Type);
      FImage.LoadFromStream(rStream);
      rStream.Free;
    end
    else
    begin
      FHandle.NotifyDataUpdated;
      if FHandle.IsDataComplitelyUpdated then
        FreeAndNil(FImage);
    end;

  end;
end;

procedure TGL3xTexture.Apply(UnitIndex: TGLuint);
var
  h: TGLuint;
  vUniform: TGLSLUniform;
begin
  if FHandle.IsDataNeedUpdate then
    Initialize;

  if FHandle.IsDataNeedUpdate then
    h := 0
  else
    h := FHandle.Handle;

  if FSamplerChanged and (h > 0) then
  begin
    FSampler.ApplyToTexture(h);
    FSamplerChanged := False;
  end;

  case UnitIndex of
    0: vUniform := uniformTexUnit0;
    1: vUniform := uniformTexUnit1;
    2: vUniform := uniformTexUnit2;
    3: vUniform := uniformTexUnit3;
    4: vUniform := uniformTexUnit4;
    5: vUniform := uniformTexUnit5;
    6: vUniform := uniformTexUnit6;
    7: vUniform := uniformTexUnit7;
  else
    begin
      vUniform := uniformTexUnit0;
      h := 0;
      UnitIndex := 0;
    end;
  end;

  ShaderManager.UniformSampler(vUniform, h, UnitIndex);
end;

procedure TGL3xTexture.SetName(const AName: string);
var
  i, n: Integer;
begin
  if (Length(AName) > 0) and (Length(FName) = 0) then
  begin
    FName := AName;
    n := Length(AName);
    FHashCode := n;
    for i := 1 to n do
      FHashCode := (FHashCode shl 1) + Byte(AName[i]);
  end;
end;

function TGL3xTexture.GetReady: Boolean;
begin
  Result := FHandle.IsDataNeedUpdate;
end;

class function TGL3xTexture.Capabilities: TDataFileCapabilities;
begin
  Result := [dfcRead, dfcWrite];
end;

{$IFDEF GLS_COMPILER_2005_UP}{$ENDREGION}{$ENDIF}

end.

