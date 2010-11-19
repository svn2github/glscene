//
// This unit is part of the GLScene Project, http://glscene.org
//
{ : GL3xTexture <p>

  <b>History : </b><font size=-1><ul>
  <li>10/10/10 - Yar - Added texture streaming, not complete yet
  <li>23/08/10 - Yar - Creation
  </ul></font><p>
}

// TODO: Texture swizzling for luminance, alpha and intensity image format
// TODO: Workaround when sampler object not supported and texture has sampler override
// TODO: Workaround when NV VTC texture compression
// DONE: Streaming from packed archive - separate levels storing
// TODO: Calculation of required level of detail in sreaming process
// DONE: Format cast when it unsupported

unit GL3xTexture;

interface

uses
  Classes,
  BaseClasses,
  ApplicationFileIO,
{$IFDEF FPC}
  LResources,
{$ENDIF}
  OpenGLTokens,
  GLContext,
  GLColor,
  GLState,
  GLGraphics,
  GLShaderManager,
  GLTextureFormat;

type

  TGLTextureFilter = (tfNearest, tfLinear);
  TTextureTexelCast = (tcDefault, tcNormalXYZ, tcNormalXY, tcYCoCg);

  TGLTextureColor = class(TGLColor)
  public
  end;

  // TGL3xSamplerName
  //
  TGL3xSamplerName = class(TGLAbstractName)
  protected
    function GetInheritorClass: TGLAbstractNameClass; override;
  public
    function GetManager: TGLSAbstractManagerClass; override;
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
    TEXTURE_MAX_ANISTROPY_EXT  1.0                    | Maximum degree of anisotropy
  }

  TGL3xSampler = class(TDataFile)
  private
    { Private Declarations }
    FHandle: TGLSamplerHandle;
    FName: TGL3xSamplerName;
    FMinMagFilter: TGLTextureFilter;
    FLODFilter: TGLTextureFilter;
    FFilteringQuality: TGLTextureFilteringQuality;
    FLODBias: Integer;
    FLODBiasFract: Single;
    FWrap: array[0..2] of TGLSeparateTextureWrap;
    FBorderColor: TGLTextureColor;
    FCompareMode: TGLTextureCompareMode;
    FCompareFunc: TDepthFunction;
    FChanged: Boolean;
    procedure SetMinMagFilter(Value: TGLTextureFilter);
    procedure SetLODFilter(Value: TGLTextureFilter);
    procedure SetLODBias(Value: Integer);
    procedure SetFilteringQuality(Value: TGLTextureFilteringQuality);
    function GetWrap(Index: Integer): TGLSeparateTextureWrap;
    procedure SetWrap(Index: Integer; Value: TGLSeparateTextureWrap);
    procedure SetBorderColor(const Value: TGLTextureColor);
    procedure SetCompareMode(Value: TGLTextureCompareMode);
    procedure SetCompareFunc(Value: TDepthFunction);
    procedure SetLODBiasFract(Value: Single);
  public
    { Public Declarations }
    constructor Create(AOwner: TPersistent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure NotifyChange(Sender: TObject); override;

    procedure Initialize; override;

    procedure LoadFromFile(const fileName: string); override;
    procedure SaveToFile(const fileName: string); override;
    procedure LoadFromStream(stream: TStream); override;
    procedure SaveToStream(stream: TStream); override;
    class function Capabilities: TDataFileCapabilities; override;

    procedure Apply(AUnitIndex: TGLuint; ATarget: TGLTextureTarget; AsTexParam: Boolean = False);
    property Name: TGL3xSamplerName read FName;

    property CompareMode: TGLTextureCompareMode read FCompareMode
      write SetCompareMode;
    property CompareFunc: TDepthFunction read FCompareFunc write SetCompareFunc;
    property LODBiasFract: Single read FLODBiasFract write SetLODBiasFract;
  published
    { Published Declarations }

    { : Texture filter. }
    property MinMagFilter: TGLTextureFilter read FMinMagFilter
      write SetMinMagFilter default tfLinear;
    property LODFilter: TGLTextureFilter read FLODFilter write SetLODFilter
      default tfLinear;
    property FilteringQuality: TGLTextureFilteringQuality read FFilteringQuality
      write SetFilteringQuality default tfAnisotropic;
    { : Texture LOD bias. }
    property LodBias: Integer read FLODBias write SetLODBias;
    { : Address mode for the texture. }
    property WrapX: TGLSeparateTextureWrap index 0 read GetWrap write SetWrap
      default twRepeat;
    property WrapY: TGLSeparateTextureWrap index 1 read GetWrap write SetWrap
      default twRepeat;
    property WrapZ: TGLSeparateTextureWrap index 2 read GetWrap write SetWrap
      default twRepeat;
    { : Texture Border color. }
    property BorderColor: TGLTextureColor read FBorderColor
      write SetBorderColor;
  end;

  // TGL3xTextureName
  //
  TGL3xTextureName = class(TGLAbstractName)
  protected
    function GetInheritorClass: TGLAbstractNameClass; override;
  public
    function GetManager: TGLSAbstractManagerClass; override;
  end;

  TGL3xTexture = class(TDataFile)
  private
    { Private Declarations }
    FHandle: TGLTextureHandle;
    FName: TGL3xTextureName;
    FImage: TGLImage;
    FTexelCast: TTextureTexelCast;
    FSampler: TGL3xSampler;
    FNeverStream: Boolean;
    FImageStream: TGLTextureImageStream;
    FBaseLevel: Integer;
    FMaxLevel: Integer;
    FCompressed: Boolean;
    FChanged: Boolean;
    FLastTime: Double;
    function GetReady: Boolean;
    procedure SetNeverStream(AValue: Boolean);
    procedure SetTexelCast(AValue: TTextureTexelCast);
    function GetFormat: TGLInternalFormat;
    function GetSourceFile: string;
  protected
    { Protected Declarations }
    function GetTarget: TGLTextureTarget;
    function ImageMaxFace: Integer;
    procedure CalcLODRange(out AFirstLOD, ALastLOD: Integer);
    procedure CalcLODSizes;
    procedure Bind(var Already: Boolean);
    procedure DoStreamTransfer;
    procedure DoFullTransfer;
  public
    { Public Declarations }
    constructor Create(AOwner: TPersistent); override;
    destructor Destroy; override;
    procedure NotifyChange(Sender: TObject); override;
    procedure Assign(Source: TPersistent); override;

    procedure LoadFromFile(const fileName: string); override;
    procedure SaveToFile(const fileName: string); override;
    procedure LoadFromStream(stream: TStream); override;
    procedure SaveToStream(stream: TStream); override;
    procedure ShowAsError;

    procedure ImportFromFile(const fileName: string);
    procedure ExportToFile(const fileName: string);
    procedure ImportFromStream(stream: TStream);
    procedure ExportToStream(stream: TStream);

    procedure Initialize; override;
    class function Capabilities: TDataFileCapabilities; override;

    function Apply(const AUniform: TGLSLUniform; UseOwnSampler: Boolean): TGLuint;
    property Name: TGL3xTextureName read FName;
    property IsReadyToWork: Boolean read GetReady;
    property Target: TGLTextureTarget read GetTarget;
  published
    property InternalFormat: TGLInternalFormat read GetFormat;
    property Sampler: TGL3xSampler read FSampler;
    property TexelCast: TTextureTexelCast read FTexelCast
      write SetTexelCast default tcDefault;
    property NeverStream: Boolean read FNeverStream
      write SetNeverStream default True;
    property SourceFile: string read GetSourceFile;
  end;

implementation

uses
  GLCrossPlatform,
  VectorGeometry,
  GL3xMaterial,
  SysUtils,
  GLStrings,
  GLSLog;

const
  cTextureMinFilter: array[tfNearest..tfLinear, tfNearest..tfLinear]
    of TGLEnum = ((GL_NEAREST_MIPMAP_NEAREST, GL_LINEAR_MIPMAP_NEAREST),
    (GL_NEAREST_MIPMAP_LINEAR, GL_LINEAR_MIPMAP_LINEAR));

  cTextureMagFilter: array[tfNearest..tfLinear] of TGLEnum = (GL_NEAREST,
    GL_LINEAR);

  cTextureWrapMode: array[twRepeat..twMirrorClampToBorder] of TGLEnum =
    (GL_REPEAT, GL_CLAMP_TO_EDGE, GL_CLAMP_TO_BORDER,
    GL_MIRRORED_REPEAT, GL_MIRROR_CLAMP_TO_EDGE_ATI,
    GL_MIRROR_CLAMP_TO_BORDER_EXT);

  cTextureCompareMode: array[tcmNone..tcmCompareRtoTexture] of TGLEnum =
    (GL_NONE, GL_COMPARE_R_TO_TEXTURE);

procedure Div2(var Value: Integer); inline;
begin
  Value := Value div 2;
  if Value = 0 then
    Value := 1;
end;
// ------------------
// ------------------ TGL3xSampler------------------
// ------------------

{$REGION 'TGLSampler'{$ENDIF}

function TGL3xSamplerName.GetInheritorClass: TGLAbstractNameClass;
begin
  Result := TGLAbstractNameClass(ClassType);
end;

function TGL3xSamplerName.GetManager: TGLSAbstractManagerClass;
begin
  Result := MaterialManager;
end;

constructor TGL3xSampler.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner);
  FHandle := TGLSamplerHandle.Create;
  FMinMagFilter := tfLinear;
  FLODFilter := tfLinear;
  FFilteringQuality := tfAnisotropic;
  FLODBias := 0;
  FWrap[0] := twRepeat;
  FWrap[1] := twRepeat;
  FWrap[2] := twRepeat;
  FBorderColor := TGLTextureColor.CreateInitialized(Self, clrTransparent);
  FCompareMode := tcmNone;
  FCompareFunc := cfLequal;
  if not Assigned(AOwner) then
  begin
    FName := TGL3xSamplerName.Create;
    FName._AddRef;
  end;
end;

destructor TGL3xSampler.Destroy;
begin
  FHandle.Destroy;
  if Assigned(FName) then
    FName._Release;
  FBorderColor.Free;
  inherited;
end;

procedure TGL3xSampler.Assign(Source: TPersistent);
var
  rSampler: TGL3xSampler;
begin
  if Source is TGL3xSampler then
  begin
    rSampler := TGL3xSampler(Source);
    FLODFilter := rSampler.FLODFilter;
    FLODBias := rSampler.FLODBias;
    FWrap := rSampler.FWrap;
    FBorderColor.Assign(rSampler.BorderColor);
    FCompareMode := rSampler.FCompareMode;
    FCompareFunc := rSampler.FCompareFunc;
    NotifyChange(Self);
  end
  else
    inherited;
end;

procedure TGL3xSampler.NotifyChange(Sender: TObject);
begin
  FHandle.NotifyChangesOfData;
  FChanged := True;
  inherited;
end;

procedure TGL3xSampler.LoadFromFile(const fileName: string);
var
  fs: TStream;
begin
  if FileStreamExists(fileName) then
  begin
    fs := CreateFileStream(fileName, fmOpenRead);
    ResourceName := fileName;
    try
      LoadFromStream(fs);
    finally
      fs.Free;
    end;
  end
  else
    GLSLogger.LogError(Format(glsFileNotFound, [fileName]));
end;

procedure TGL3xSampler.SaveToFile(const fileName: string);
var
  fs: TStream;
begin
  fs := CreateFileStream(fileName, fmOpenWrite or fmCreate);
  try
    SaveToStream(fs);
  finally
    fs.Free;
  end;
  ResourceName := fileName;
end;

procedure TGL3xSampler.LoadFromStream(stream: TStream);
var
  Temp: Integer;
begin
  stream.ReadBuffer(Temp, SizeOf(Integer)); // Version
  if Temp > 0 then
  begin
    GLSLogger.LogError(Format(glsUnknownArchive, [Self.ClassName, Temp]));
    Abort;
  end;
  stream.ReadBuffer(Temp, SizeOf(Integer));
  FMinMagFilter := TGLTextureFilter(Temp);
  stream.ReadBuffer(Temp, SizeOf(Integer));
  FLODFilter := TGLTextureFilter(Temp);
  stream.ReadBuffer(FLODBias, SizeOf(Integer));
  stream.ReadBuffer(Temp, SizeOf(Integer));
  FFilteringQuality := TGLTextureFilteringQuality(Temp);
  stream.ReadBuffer(Temp, SizeOf(Integer));
  FWrap[0] := TGLSeparateTextureWrap(Temp);
  stream.ReadBuffer(Temp, SizeOf(Integer));
  FWrap[1] := TGLSeparateTextureWrap(Temp);
  stream.ReadBuffer(Temp, SizeOf(Integer));
  FWrap[2] := TGLSeparateTextureWrap(Temp);
  FBorderColor.ReadData(stream);
  stream.ReadBuffer(Temp, SizeOf(Integer));
  FCompareMode := TGLTextureCompareMode(Temp);
  stream.ReadBuffer(Temp, SizeOf(Integer));
  FCompareFunc := TDepthFunction(Temp);
  NotifyChange(Self);

  if (Length(ResourceName) = 0) and (stream is TFileStream) then
    ResourceName := TFileStream(stream).FileName;
end;

procedure TGL3xSampler.SaveToStream(stream: TStream);
var
  Temp: Integer;
begin
  Temp := 0;
  stream.WriteBuffer(Temp, SizeOf(Integer)); // Version
  Temp := Integer(FMinMagFilter);
  stream.WriteBuffer(Temp, SizeOf(Integer));
  Temp := Integer(FLODFilter);
  stream.WriteBuffer(Temp, SizeOf(Integer));
  Temp := Integer(FFilteringQuality);
  stream.WriteBuffer(Temp, SizeOf(Integer));
  stream.WriteBuffer(FLODBias, SizeOf(Integer));
  Temp := Integer(FWrap[0]);
  stream.WriteBuffer(Temp, SizeOf(Integer));
  Temp := Integer(FWrap[1]);
  stream.WriteBuffer(Temp, SizeOf(Integer));
  Temp := Integer(FWrap[2]);
  stream.WriteBuffer(Temp, SizeOf(Integer));
  Temp := Integer(FCompareMode);
  FBorderColor.WriteData(stream);
  stream.WriteBuffer(Temp, SizeOf(Integer));
  Temp := Integer(FCompareFunc);
  stream.WriteBuffer(Temp, SizeOf(Integer));

  if (Length(ResourceName) = 0) and (stream is TFileStream) then
    ResourceName := TFileStream(stream).fileName;

  FChanged := False;
end;

class function TGL3xSampler.Capabilities: TDataFileCapabilities;
begin
  Result := [dfcRead, dfcWrite];
end;

procedure TGL3xSampler.SetMinMagFilter(Value: TGLTextureFilter);
begin
  if Value <> FMinMagFilter then
  begin
    FMinMagFilter := Value;
    NotifyChange(Self);
  end;
end;

procedure TGL3xSampler.SetLODFilter(Value: TGLTextureFilter);
begin
  if Value <> FLODFilter then
  begin
    FLODFilter := Value;
    NotifyChange(Self);
  end;
end;

procedure TGL3xSampler.SetLODBias(Value: Integer);
begin
  if Value <> FLODBias then
  begin
    FLODBias := Value;
    NotifyChange(Self);
  end;
end;

procedure TGL3xSampler.SetFilteringQuality(Value: TGLTextureFilteringQuality);
begin
  if Value <> FFilteringQuality then
  begin
    FFilteringQuality := Value;
    NotifyChange(Self);
  end;
end;

function TGL3xSampler.GetWrap(Index: Integer): TGLSeparateTextureWrap;
begin
  Result := FWrap[Index];
end;

procedure TGL3xSampler.SetWrap(Index: Integer; Value: TGLSeparateTextureWrap);
begin
  if Value <> FWrap[Index] then
  begin
    FWrap[Index] := Value;
    NotifyChange(Self);
  end;
end;

procedure TGL3xSampler.SetBorderColor(const Value: TGLTextureColor);
begin
  FBorderColor.Assign(Value);
  NotifyChange(Self);
end;

procedure TGL3xSampler.SetCompareMode(Value: TGLTextureCompareMode);
begin
  if Value <> FCompareMode then
  begin
    FCompareMode := Value;
    NotifyChange(Self);
  end;
end;

procedure TGL3xSampler.SetCompareFunc(Value: TDepthFunction);
begin
  if Value <> FCompareFunc then
  begin
    FCompareFunc := Value;
    NotifyChange(Self);
  end;
end;

procedure TGL3xSampler.SetLODBiasFract(Value: Single);
begin
  if Value <> FLODBiasFract then
  begin
    FLODBiasFract := Value;
    NotifyChange(Self);
  end;
end;

procedure TGL3xSampler.Initialize;
var
  ID: TGLuint;
begin
  if FHandle.IsSupported then
    with GL do
    begin
      FHandle.AllocateHandle;
      ID := FHandle.Handle;
      SamplerParameterfv(ID, GL_TEXTURE_BORDER_COLOR, FBorderColor.AsAddress);
      SamplerParameteri(ID, GL_TEXTURE_WRAP_S, cTextureWrapMode[WrapX]);
      SamplerParameteri(ID, GL_TEXTURE_WRAP_T, cTextureWrapMode[WrapY]);
      SamplerParameteri(ID, GL_TEXTURE_WRAP_R, cTextureWrapMode[WrapZ]);
      SamplerParameterf(ID, GL_TEXTURE_LOD_BIAS, FLODBias + FLODBiasFract);
      SamplerParameteri(ID, GL_TEXTURE_MIN_FILTER, cTextureMinFilter[FMinMagFilter, FLODFilter]);
      SamplerParameteri(ID, GL_TEXTURE_MAG_FILTER, cTextureMagFilter[FMinMagFilter]);
      if (FFilteringQuality = tfAnisotropic) { and not IsDesignTime } then
        SamplerParameteri(ID, GL_TEXTURE_MAX_ANISOTROPY_EXT,
          CurrentGLContext.GLStates.MaxTextureAnisotropy)
      else
        SamplerParameteri(ID, GL_TEXTURE_MAX_ANISOTROPY_EXT, 1);
      SamplerParameteri(ID, GL_TEXTURE_COMPARE_MODE,
        cTextureCompareMode[FCompareMode]);
      SamplerParameteri(ID, GL_TEXTURE_COMPARE_FUNC,
        cGLComparisonFunctionToGLEnum[FCompareFunc]);
      FHandle.NotifyDataUpdated;
    end;
end;

procedure TGL3xSampler.Apply(AUnitIndex: TGLuint; ATarget: TGLTextureTarget; AsTexParam: Boolean);
var
  glTarget: TGLEnum;

  procedure SetupParams;
  begin
    with GL do
    begin
      glTarget := DecodeGLTextureTarget(ATarget);
      if not IsTargetSupported(glTarget) then
        exit;
      CurrentGLContext.GLStates.ActiveTexture := AUnitIndex;
      TexParameterfv(glTarget, GL_TEXTURE_BORDER_COLOR, FBorderColor.AsAddress);
      TexParameteri(glTarget, GL_TEXTURE_WRAP_S, cTextureWrapMode[WrapX]);
      TexParameteri(glTarget, GL_TEXTURE_WRAP_T, cTextureWrapMode[WrapY]);
      TexParameteri(glTarget, GL_TEXTURE_WRAP_R, cTextureWrapMode[WrapZ]);
      TexParameterf(glTarget, GL_TEXTURE_LOD_BIAS, FLODBias + FLODBiasFract);
      TexParameteri(glTarget, GL_TEXTURE_MIN_FILTER,
        cTextureMinFilter[FMinMagFilter, FLODFilter]);
      TexParameteri(glTarget, GL_TEXTURE_MAG_FILTER,
        cTextureMagFilter[FMinMagFilter]);
      if (FFilteringQuality = tfAnisotropic) and not IsDesignTime then
        TexParameteri(glTarget, GL_TEXTURE_MAX_ANISOTROPY_EXT,
          CurrentGLContext.GLStates.MaxTextureAnisotropy)
      else
        TexParameteri(glTarget, GL_TEXTURE_MAX_ANISOTROPY_EXT, 1);
      TexParameteri(glTarget, GL_TEXTURE_COMPARE_MODE,
        cTextureCompareMode[FCompareMode]);
      TexParameteri(glTarget, GL_TEXTURE_COMPARE_FUNC,
        cGLComparisonFunctionToGLEnum[FCompareFunc]);
    end;
  end;

begin
  if AsTexParam then
  begin
    if FHandle.IsSupported then
      CurrentGLContext.GLStates.SamplerBinding[AUnitIndex] := 0;
    if FHandle.IsDataNeedUpdate then
    begin
      CurrentGLContext.GLStates.ActiveTexture := AUnitIndex;
      SetupParams;
      FHandle.NotifyDataUpdated;
    end;
  end
  else if FHandle.IsSupported then
  begin
    if FHandle.IsDataNeedUpdate then
      Initialize;
    CurrentGLContext.GLStates.SamplerBinding[AUnitIndex] := FHandle.Handle;
  end
  else
  begin
    if FHandle.IsDataNeedUpdate then
    begin
      SetupParams;
      FHandle.NotifyDataUpdated;
    end;
  end;
end;

{$ENDREGION{$ENDIF}
// ------------------
// ------------------ TGL3xTexture ------------------
// ------------------

{$REGION 'TGL3xTexture'{$ENDIF}

function TGL3xTextureName.GetInheritorClass: TGLAbstractNameClass;
begin
  Result := TGLAbstractNameClass(ClassType);
end;

function TGL3xTextureName.GetManager: TGLSAbstractManagerClass;
begin
  Result := MaterialManager;
end;

constructor TGL3xTexture.Create(AOwner: TPersistent);
begin
  inherited;
  FHandle := TGLTextureHandle.Create;
  FSampler := TGL3xSampler.Create(Self);
  FImageStream := TGLTextureImageStream.Create;
  FTexelCast := tcDefault;
  FNeverStream := True;
  FName := TGL3xTextureName.Create;
  FName._AddRef;
end;

destructor TGL3xTexture.Destroy;
begin
  FHandle.Destroy;
  FImage.Free;
  FSampler.Destroy;
  FName._Release;
  FImageStream.Destroy;
  inherited;
end;

procedure TGL3xTexture.NotifyChange(Sender: TObject);
begin
  FChanged := True;
  inherited NotifyChange(Sender);
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

procedure TGL3xTexture.LoadFromFile(const fileName: string);
var
  fs: TStream;
  L, F, Size: Integer;
  ptr: PByte;
begin
  if FileStreamExists(fileName) then
  begin
    fs := CreateFileStream(fileName, fmOpenRead);
    ResourceName := fileName;
    try
      // Load texture header
      LoadFromStream(fs);
      // Load level of detal chain
      if Assigned(FImage) and (FNeverStream or IsDesignTime) then
      begin
        for L := FImage.LODCount - 1 downto 0 do
        begin
          fs.Destroy;
          fs := CreateFileStream(fileName + IntToHex(L, 2), fmOpenRead);
          Size := FImage.LevelSize(L);
          for F := 0 to ImageMaxFace do
          begin
            ptr := PByte(FImage.GetLevelData(L, F + GL_TEXTURE_CUBE_MAP_POSITIVE_X));
            fs.Read(ptr^, Size);
          end;
        end;
      end;
    finally
      fs.Free;
    end;
  end
  else
    raise EInvalidRasterFile.CreateFmt(glsFileNotFound, [fileName]);
end;

procedure TGL3xTexture.SaveToFile(const fileName: string);
var
  fs: TStream;
  L, F, Size: Integer;
  ptr: PByte;
begin
  fs := CreateFileStream(fileName, fmOpenWrite or fmCreate);
  ResourceName := fileName;
  try
    // Save texture header
    SaveToStream(fs);
    // Save level of detail chain
    if Assigned(FImage) then
    begin
      for L := FImage.LODCount - 1 downto 0 do
      begin
        fs.Destroy;
        fs := CreateFileStream(fileName + IntToHex(L, 2), fmOpenWrite or fmCreate);
        Size := FImage.LevelSize(L);
        for F := 0 to ImageMaxFace do
        begin
          ptr := PByte(FImage.GetLevelData(L, F+GL_TEXTURE_CUBE_MAP_POSITIVE_X));
          fs.Write(ptr^, Size);
        end;
      end;
    end;
  finally
    fs.Free;
  end;
end;

procedure TGL3xTexture.LoadFromStream(stream: TStream);
var
  Temp, L, F: Integer;
begin
  stream.ReadBuffer(Temp, SizeOf(Integer)); // Version
  if Temp > 0 then
  begin
    GLSLogger.LogError(Format(glsUnknownArchive, [Self.ClassName, Temp]));
    Abort;
  end;

  stream.ReadBuffer(Temp, SizeOf(Integer));
  FNeverStream := Temp = 0;

  FSampler.LoadFromStream(stream);

  stream.ReadBuffer(Temp, SizeOf(Integer));

  if Temp > 0 then
  begin
    if not Assigned(FImage) then
      FImage := TGLImage.Create;
    // Load LOD info
    for L := 0 to 15 do
      for F := 0 to 5 do
      begin
        stream.ReadBuffer(FImageStream.LevelOfDetail[L, F].Width, SizeOf(Integer));
        stream.ReadBuffer(FImageStream.LevelOfDetail[L, F].Height, SizeOf(Integer));
        stream.ReadBuffer(FImageStream.LevelOfDetail[L, F].Depth, SizeOf(Integer));
        stream.ReadBuffer(FImageStream.LevelOfDetail[L, F].Size, SizeOf(Integer));
      end;
    FImage.LoadFromStream(stream);
    FImage.ResourceName := ResourceName;
  end
  else
    FreeAndNil(FImage);
end;

procedure TGL3xTexture.SaveToStream(stream: TStream);
var
  Temp, L, F: Integer;
begin
  Temp := 0;
  stream.WriteBuffer(Temp, SizeOf(Integer)); // Version

  if FNeverStream then
    Temp := 0
  else
    Temp := MaxInt;
  stream.WriteBuffer(Temp, SizeOf(Integer));

  FSampler.SaveToStream(stream);
  if Assigned(FImage) then
  begin
    Temp := 1;
    stream.WriteBuffer(Temp, SizeOf(Integer));
    // LOD info for streaming
    for L := 0 to 15 do
      for F := 0 to 5 do
      begin
        stream.WriteBuffer(FImageStream.LevelOfDetail[L, F].Width, SizeOf(Integer));
        stream.WriteBuffer(FImageStream.LevelOfDetail[L, F].Height, SizeOf(Integer));
        stream.WriteBuffer(FImageStream.LevelOfDetail[L, F].Depth, SizeOf(Integer));
        stream.WriteBuffer(FImageStream.LevelOfDetail[L, F].Size, SizeOf(Integer));
      end;
    FImage.SaveToStream(stream);
    FImage.ResourceName := ResourceName;
  end
  else
  begin
    Temp := 0;
    stream.WriteBuffer(Temp, SizeOf(Integer));
  end;

  FChanged := False;
end;

procedure TGL3xTexture.ShowAsError;
begin
  if not Assigned(FImage) then
    FImage := TGLImage.Create;
  FImage.SetErrorImage;
end;

// LoadFromFile
//

procedure TGL3xTexture.ImportFromFile(const fileName: string);
var
  BaseImageClass: TGLBaseImageClass;
  tempImage: TGLBaseImage;
  rStream: {$IFNDEF FPC}TResourceStream{$ELSE}TLazarusResourceStream{$ENDIF};
begin
  if fileName = '' then
  begin
    if FName.Value = glsDIFFUSEMAP then
    begin
      rStream := CreateResourceStream(glsDIFFUSEMAP, GLS_RC_DDS_Type);
      LoadFromStream(rStream);
      rStream.Free;
    end
    else if FName.Value = glsNORMALMAP then
    begin
      rStream := CreateResourceStream(glsNORMALMAP, GLS_RC_DDS_Type);
      LoadFromStream(rStream);
      rStream.Free;
    end
    else
    begin
      GLSLogger.LogWarning
        (Format('Trying to load empty file name to texture "%s"', [Name]));
      ShowAsError;
    end;
    exit;
  end;

  BaseImageClass := GetRasterFileFormats.FindFromFileName(fileName);
  tempImage := BaseImageClass.Create;
  try
    tempImage.LoadFromFile(fileName);
    if not Assigned(FImage) then
      FImage := TGLImage.Create;
    FImage.Assign(tempImage);
    if FImage.LODCount = 1 then
      FImage.GenerateMipmap;
    CalcLODSizes;

    FImage.ResourceName := fileName;
    FHandle.NotifyChangesOfData;
    NotifyChange(Self);
  finally
    tempImage.Free;
  end;
end;

// SaveToFile
//

procedure TGL3xTexture.ExportToFile(const fileName: string);
var
  fs: TStream;
begin
  if fileName = '' then
  begin
    GLSLogger.LogWarning
      (Format('Trying to save empty file name of texture "%s"', [Name]));
    exit;
  end;
  fs := CreateFileStream(fileName, fmOpenWrite or fmCreate);
  ResourceName := fileName;
  try
    SaveToStream(fs);
  finally
    fs.Free;
  end;
end;

procedure TGL3xTexture.ImportFromStream(stream: TStream);
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
    if FImage.LODCount = 1 then
      FImage.GenerateMipmap;

    CalcLODSizes;
    FHandle.NotifyChangesOfData;
    NotifyChange(Self);
  finally
    tempImage.Free;
  end;
end;

procedure TGL3xTexture.ExportToStream(stream: TStream);
var
  RC: TGLContext;
  BaseImageClass: TGLBaseImageClass;
  tempImage: TGLBaseImage;
begin
  RC := SafeCurrentGLContext;
  BaseImageClass := GetRasterFileFormats.FindFromFileName(ResourceName);
  tempImage := BaseImageClass.Create;
  if FHandle.Handle <> 0 then
    tempImage.AssignFromTexture(RC, FHandle.Handle, FHandle.Target,
      false, Fimage.InternalFormat)
  else
    tempImage.Assign(FImage);
  try
    tempImage.SaveToStream(stream);
  finally
    tempImage.Free;
  end;
end;

function TGL3xTexture.GetTarget: TGLTextureTarget;
begin
  if GetReady or not Assigned(FImage) then
    Result := FHandle.Target
  else
    Result := FImage.GetTextureTarget;
end;

function TGL3xTexture.ImageMaxFace: Integer;
begin
  if FImage.CubeMap then
    exit(5)
  else
    exit(0);
end;

procedure TGL3xTexture.CalcLODRange(out AFirstLOD, ALastLOD: Integer);
var
  I, MaxLODSize, MinLODSize, MaxLODZSize: Integer;
begin
  if IsDesignTime then
  begin
    if FHandle.Target <> ttTexture3D then
    begin
      MaxLODSize := 1024;
      MinLODSize := 256;
      MaxLODZSize := 65536;
    end
    else
    begin
      MaxLODSize := 128;
      MinLODSize := 1;
      MaxLODZSize := 128;
    end;
  end
  else
  begin

    case FHandle.Target of
      ttTexture3D:
        begin
          MaxLODSize := CurrentGLContext.GLStates.Max3DTextureSize;
          MaxLODZSize := MaxLODSize;
        end;

      ttTextureCube:
        begin
          MaxLODSize := CurrentGLContext.GLStates.MaxCubeTextureSize;
          MaxLODZSize := 0;
        end;

      ttTexture1DArray,
        ttTexture2DArray,
        ttTextureCubeArray,
        ttTexture2DMultisampleArray:
        begin
          MaxLODSize := CurrentGLContext.GLStates.MaxTextureSize;
          MaxLODZSize := CurrentGLContext.GLStates.MaxArrayTextureSize;
        end;

    else
      begin
        MaxLODSize := CurrentGLContext.GLStates.MaxTextureSize;
        MaxLODZSize := 0;
      end;
    end;

    MinLODSize := 1;
  end;
  AFirstLOD := 0;

  for I := 0 to High(TGLTextureStreamLevels) do
  begin
    if (FImageStream.LevelOfDetail[I, 0].Width <= MaxLODSize)
      and (FImageStream.LevelOfDetail[I, 0].Height <= MaxLODSize)
      and (FImageStream.LevelOfDetail[I, 0].Depth <= MaxLODZSize) then
      break;
    Inc(AFirstLOD);
  end;

  AFirstLOD := MinInteger(AFirstLOD, FImage.LODCount - 1);
  ALastLOD := AFirstLOD;

  for I := AFirstLOD to High(FImageStream.LevelOfDetail) do
  begin
    if (FImageStream.LevelOfDetail[I, 0].Width < MinLODSize)
      or (FImageStream.LevelOfDetail[I, 0].Height < MinLODSize) then
      break;
      Inc(ALastLOD);
  end;
  ALastLOD := MinInteger(ALastLOD, FImage.LODCount - 1);
end;

procedure TGL3xTexture.CalcLODSizes;
var
  prevL, L, F: Integer;
  bDivDepth: Boolean;
begin
  with FImageStream.LevelOfDetail[0, 0] do
  begin
    Width := FImage.Width;
    Height := FImage.Height;
    Depth := FImage.Depth;
    Size := FImage.LevelSize(0);
  end;

  bDivDepth := (FImage.Depth > 0) and not FImage.TextureArray;

  L := 0;
  with FImageStream do
    repeat
      prevL := L;
      Inc(L);
      LevelOfDetail[L, 0].Width := LevelOfDetail[prevL, 0].Width;
      LevelOfDetail[L, 0].Height := LevelOfDetail[prevL, 0].Height;
      LevelOfDetail[L, 0].Depth := LevelOfDetail[prevL, 0].Depth;
      LevelOfDetail[L, 0].Size := FImage.LevelSize(L);

      Div2(LevelOfDetail[L, 0].Width);
      Div2(LevelOfDetail[L, 0].Height);
      if bDivDepth then
        Div2(LevelOfDetail[L, 0].Depth);
    until (LevelOfDetail[L, 0].Width <= 1) and (LevelOfDetail[L, 0].Height <= 1);

  if FImage.CubeMap then
    for L := 0 to L do
      with FImageStream do
      begin
        for F := 1 to 5 do
        begin
          LevelOfDetail[L, F].Width := LevelOfDetail[L, 0].Width;
          LevelOfDetail[L, F].Height := LevelOfDetail[L, 0].Height;
          LevelOfDetail[L, F].Depth := LevelOfDetail[L, 0].Depth;
          LevelOfDetail[L, F].Size := LevelOfDetail[L, 0].Size;
        end;
      end;
end;

procedure TGL3xTexture.Bind(var Already: Boolean);
var
  rowSize: Integer;
begin
  if not Already then
  begin
    with CurrentGLContext.GLStates do
    begin
      ActiveTexture := 0;
      TextureBinding[0, FHandle.Target] := FHandle.Handle;
      if FImage.CubeMap and GL.ARB_seamless_cube_map then
        GL.Enable(GL_TEXTURE_CUBE_MAP_SEAMLESS);
      UnpackRowLength := 0;
      UnpackSkipRows := 0;
      UnpackSkipPixels := 0;
      rowSize := FImage.Width * FImage.ElementSize;
      if (rowSize mod 8 = 0) and (FImage.ElementSize > 4) then
        UnpackAlignment := 8
      else
      if rowSize mod 4 = 0 then
        UnpackAlignment := 4
      else if rowSize mod 2 = 0 then
        UnpackAlignment := 2
      else
        UnpackAlignment := 1;
    end;
    Already := True;
  end;
end;

procedure TGL3xTexture.DoStreamTransfer;
var
  glTarget, glInternalFormat: TGLEnum;
  OldBaseLevel, Level, Face: Integer;
  bBinded, bContinueStreaming: Boolean;
  newTime: Double;
begin

  glTarget := DecodeGLTextureTarget(FHandle.Target);
  if not IsTargetSupported(glTarget) then
    exit;

  bBinded := False;

  OldBaseLevel := FBaseLevel;
  CalcLODRange(FBaseLevel, FMaxLevel);

  bContinueStreaming := False;
  for Level := FMaxLevel downto FBaseLevel do
  begin
    for Face := ImageMaxFace downto 0 do
    begin
      case FImageStream.LevelOfDetail[Level, Face].State of

        ssKeeping:
          begin
            if FBaseLevel < Level then
              FBaseLevel := FMaxLevel;

            FImageStream.LevelOfDetail[Level, Face].State := ssLoading;
            FImage.DoStreaming;
            bContinueStreaming := True;
          end;

        ssLoading:
          begin
            FImage.DoStreaming;
            bContinueStreaming := True;
            if FBaseLevel < Level then
              FBaseLevel := FMaxLevel;
          end;

        ssLoaded:
          begin
            FImageStream.LevelOfDetail[Level, Face].PBO.AllocateHandle;
            FImageStream.LevelOfDetail[Level, Face].PBO.Bind;
            glInternalFormat := InternalFormatToOpenGLFormat(FImage.InternalFormat);
            case glTarget of
{$REGION 'GL_TEXTURE_1D'}
              GL_TEXTURE_1D:
              begin
                Bind(bBinded);
                if FCompressed then
                  GL.CompressedTexImage1D(
                    glTarget,
                    Level,
                    glInternalFormat,
                    FImageStream.LevelOfDetail[Level, Face].Width, 0,
                    FImageStream.LevelOfDetail[Level, Face].Size, nil)
                else
                  GL.TexImage1D(
                    glTarget,
                    Level,
                    glInternalFormat,
                    FImageStream.LevelOfDetail[Level, Face].Width, 0,
                    FImage.ColorFormat,
                    FImage.DataType, nil);
              end;
{$ENDREGION}
{$REGION 'GL_TEXTURE_2D'}
              GL_TEXTURE_2D:
              begin
                Bind(bBinded);
                if FCompressed then
                  GL.CompressedTexImage2D(
                    glTarget,
                    Level,
                    glInternalFormat,
                    FImageStream.LevelOfDetail[Level, Face].Width,
                    FImageStream.LevelOfDetail[Level, Face].Height,
                    0,
                    FImageStream.LevelOfDetail[Level, Face].Size, nil)
                else
                  GL.TexImage2D(
                    glTarget,
                    Level,
                    glInternalFormat,
                    FImageStream.LevelOfDetail[Level, Face].Width,
                    FImageStream.LevelOfDetail[Level, Face].Height,
                    0,
                    FImage.ColorFormat,
                    FImage.DataType, nil);
              end;
{$ENDREGION}
{$REGION 'GL_TEXTURE_RECTANGLE'}
              GL_TEXTURE_RECTANGLE:
                ;
{$ENDREGION}
{$REGION 'GL_TEXTURE_3D'}
              GL_TEXTURE_3D:
              begin
                Bind(bBinded);
                if FCompressed then
                  GL.CompressedTexImage3D(glTarget, Level, glInternalFormat,
                    FImageStream.LevelOfDetail[Level, Face].Width,
                    FImageStream.LevelOfDetail[Level, Face].Height,
                    FImageStream.LevelOfDetail[Level, Face].Depth,
                    0,
                    FImageStream.LevelOfDetail[Level, Face].Size, nil)
                else
                  GL.TexImage3D(glTarget, Level, glInternalFormat,
                    FImageStream.LevelOfDetail[Level, Face].Width,
                    FImageStream.LevelOfDetail[Level, Face].Height,
                    FImageStream.LevelOfDetail[Level, Face].Depth,
                    0,
                    FImage.ColorFormat,
                    FImage.DataType, nil);
              end;
{$ENDREGION}
{$REGION 'GL_TEXTURE_CUBE_MAP'}
              GL_TEXTURE_CUBE_MAP:
              begin
                Bind(bBinded);
                if FCompressed then
                  GL.CompressedTexImage2D(
                    5 - Face + GL_TEXTURE_CUBE_MAP_POSITIVE_X,
                    Level,
                    glInternalFormat,
                    FImageStream.LevelOfDetail[Level, Face].Width,
                    FImageStream.LevelOfDetail[Level, Face].Height,
                    0,
                    FImageStream.LevelOfDetail[Level, Face].Size,
                    nil)
                else
                  GL.TexImage2D(
                    5 - Face + GL_TEXTURE_CUBE_MAP_POSITIVE_X,
                    Level,
                    glInternalFormat,
                    FImageStream.LevelOfDetail[Level, Face].Width,
                    FImageStream.LevelOfDetail[Level, Face].Height,
                    0,
                    FImage.ColorFormat,
                    FImage.DataType,
                    nil);
                end;
{$ENDREGION}
{$REGION 'GL_TEXTURE_1D_ARRAY'}
              GL_TEXTURE_1D_ARRAY:
              begin
                Bind(bBinded);
                if FCompressed then
                  GL.CompressedTexImage2D(glTarget, Level, glInternalFormat,
                    FImageStream.LevelOfDetail[Level, Face].Width,
                    FImageStream.LevelOfDetail[Level, Face].Height,
                    0,
                    FImageStream.LevelOfDetail[Level, Face].Size,
                    nil)
                else
                  GL.TexImage2D(glTarget, Level, glInternalFormat,
                    FImageStream.LevelOfDetail[Level, Face].Width,
                    FImageStream.LevelOfDetail[Level, Face].Height,
                    0,
                    FImage.ColorFormat,
                    FImage.DataType,
                    nil);
              end;
{$ENDREGION}
{$REGION 'GL_TEXTURE_2D_ARRAY'}
              GL_TEXTURE_2D_ARRAY:
                begin
                  Bind(bBinded);
                  if FCompressed then
                    GL.CompressedTexImage3D(glTarget,
                      Level,
                      glInternalFormat,
                      FImageStream.LevelOfDetail[Level, Face].Width,
                      FImageStream.LevelOfDetail[Level, Face].Height,
                      FImageStream.LevelOfDetail[Level, Face].Depth,
                      0,
                      FImageStream.LevelOfDetail[Level, Face].Size,
                      nil)
                  else
                    GL.TexImage3D(
                      glTarget,
                      Level,
                      glInternalFormat,
                      FImageStream.LevelOfDetail[Level, Face].Width,
                      FImageStream.LevelOfDetail[Level, Face].Height,
                      FImageStream.LevelOfDetail[Level, Face].Depth,
                      0,
                      FImage.ColorFormat,
                      FImage.DataType,
                      nil);
                end;
{$ENDREGION}
{$REGION 'GL_TEXTURE_CUBE_MAP_ARRAY'}
              GL_TEXTURE_CUBE_MAP_ARRAY:
                ;
{$ENDREGION}
            end;

            with FImageStream.LevelOfDetail[Level, Face] do
            begin
              PBO.UnBind;
              State := ssTransfered;
              GLSLogger.LogDebug(Format('Texture "%s" level %d of face %d loaded', [FName.GetValue, Level, Face]));
            end;

          end;

        ssTransfered:
          with FImageStream.LevelOfDetail[Level, Face] do
          begin
            if PBO.IsAllocatedForContext then
              PBO.DestroyHandle;
            if Face > 0 then
              continue;
            FBaseLevel := Level;
          end;
      end; // of case
      if bContinueStreaming then
        break;
    end; // for Face
    if bContinueStreaming then
      break;
  end; // For Level

  if bContinueStreaming then
  begin
    Bind(bBinded);
    GL.TexParameteri(glTarget, GL_TEXTURE_MAX_LEVEL, FMaxLevel);
    GL.TexParameteri(glTarget, GL_TEXTURE_BASE_LEVEL, FBaseLevel);
  end;

  // Smooth transition between levels
  newTime := GLSTime;
  if FSampler.LODBiasFract > 0 then
    FSampler.LODBiasFract := FSampler.LODBiasFract - 0.05 * (newTime - FLastTime)
  else if FSampler.LODBiasFract < 0 then
    FSampler.LODBiasFract := 0;
  FLastTime := newTime;
  if OldBaseLevel > FBaseLevel then
    FSampler.LODBiasFract := FSampler.LODBiasFract + (OldBaseLevel - FBaseLevel);
end;

procedure TGL3xTexture.DoFullTransfer;
var
  glTarget: TGLEnum;
  Level, Face: Integer;
  vtcBuffer, top, bottom: PGLubyte;
  I, J, K, cw, ch: Integer;
  bBinded: Boolean;
  glInternalFormat: TGLEnum;
  lImage: TGLImage;

  function blockOffset(x, y, z: Integer): Integer;
  begin
    if z >= (FImageStream.LevelOfDetail[Level, 0].Depth and -4) then
      Result := FImage.ElementSize *
        (cw * ch * (FImageStream.LevelOfDetail[Level, 0].Depth and -4) + x + cw *
        (y + ch * (z - 4 * ch)))
    else
      Result := FImage.ElementSize * (4 * (x + cw * (y + ch * floor(z / 4))) +
        (z and 3));
    Result := MinInteger(0, Result);
  end;

begin

  glTarget := DecodeGLTextureTarget(FHandle.Target);
  if not IsTargetSupported(glTarget) then
    exit;

  bBinded := False;
  Bind(bBinded);

  CalcLODRange(FBaseLevel, FMaxLevel);
  vtcBuffer := nil;

  if not IsFormatSupported(InternalFormat) then
  begin
    lImage := TGLImage.Create;
    lImage.Assign(FImage);
    lImage.SetColorFormatDataType(GL_RGBA, Gl_UNSIGNED_BYTE);
    glInternalFormat := GL_RGBA8;
  end
  else
  begin
    glInternalFormat := InternalFormatToOpenGLFormat(FImage.InternalFormat);
    lImage := FImage;
  end;

  with lImage do
  begin
    case glTarget of
{$REGION 'GL_TEXTURE_1D'}
      GL_TEXTURE_1D:
        for Level := FMaxLevel downto FBaseLevel do
        begin
          if FCompressed then
            GL.CompressedTexImage1D(glTarget, Level, glInternalFormat,
              FImageStream.LevelOfDetail[Level, 0].Width,
              0,
              FImageStream.LevelOfDetail[Level, 0].Size,
              GetLevelData(Level))
          else
            GL.TexImage1D(glTarget, Level, glInternalFormat,
              FImageStream.LevelOfDetail[Level, 0].Width,
              0,
              ColorFormat,
              DataType,
              GetLevelData(Level));
        end;
{$ENDREGION}
{$REGION 'GL_TEXTURE_2D'}
      GL_TEXTURE_2D:
        for Level := FMaxLevel downto FBaseLevel do
        begin
          if FCompressed then
            GL.CompressedTexImage2D(glTarget, Level, glInternalFormat,
              FImageStream.LevelOfDetail[Level, 0].Width,
              FImageStream.LevelOfDetail[Level, 0].Height,
              0,
              FImageStream.LevelOfDetail[Level, 0].Size,
              GetLevelData(Level))
          else
            GL.TexImage2D(glTarget, Level, glInternalFormat,
              FImageStream.LevelOfDetail[Level, 0].Width,
              FImageStream.LevelOfDetail[Level, 0].Height,
              0,
              ColorFormat,
              DataType,
              GetLevelData(Level));
        end;
{$ENDREGION}
{$REGION 'GL_TEXTURE_RECTANGLE'}
      GL_TEXTURE_RECTANGLE:
        ;
{$ENDREGION}
{$REGION 'GL_TEXTURE_3D'}
      GL_TEXTURE_3D:
        for Level := FMaxLevel downto FBaseLevel do
        begin

          if GL.NV_texture_compression_vtc and FCompressed then
          begin
            // Shufle blocks for Volume Texture Compression
            cw := (FImageStream.LevelOfDetail[Level, 0].Width + 3) div 4;
            ch := (FImageStream.LevelOfDetail[Level, 0].Height + 3) div 4;
            if vtcBuffer = nil then
              GetMem(vtcBuffer, LevelSize(0));
            top := PGLubyte(GetLevelData(Level));
            for k := 0 to FImageStream.LevelOfDetail[Level, 0].Depth - 1 do
              for I := 0 to ch - 1 do
                for j := 0 to cw - 1 do
                begin
                  bottom := vtcBuffer;
                  Inc(bottom, blockOffset(j, I, k));
                  Move(top^, bottom^, ElementSize);
                  Inc(top, ElementSize);
                end;
            GL.CompressedTexImage3D(glTarget, Level, glInternalFormat,
              FImageStream.LevelOfDetail[Level, 0].Width,
              FImageStream.LevelOfDetail[Level, 0].Height,
              FImageStream.LevelOfDetail[Level, 0].Depth,
              0,
              FImageStream.LevelOfDetail[Level, 0].Size,
              vtcBuffer);
          end
          else
          begin
            // Normal compression
            if FCompressed then
              GL.CompressedTexImage3D(glTarget, Level, glInternalFormat,
                FImageStream.LevelOfDetail[Level, 0].Width,
                FImageStream.LevelOfDetail[Level, 0].Height,
                FImageStream.LevelOfDetail[Level, 0].Depth,
                0,
                FImageStream.LevelOfDetail[Level, 0].Size,
                GetLevelData(Level))
            else
              GL.TexImage3D(glTarget, Level, glInternalFormat,
                FImageStream.LevelOfDetail[Level, 0].Width,
                FImageStream.LevelOfDetail[Level, 0].Height,
                FImageStream.LevelOfDetail[Level, 0].Depth,
                0,
                ColorFormat,
                DataType,
                GetLevelData(Level));
          end;
        end;
{$ENDREGION}
{$REGION 'GL_TEXTURE_CUBE_MAP'}
      GL_TEXTURE_CUBE_MAP:
        for Level := FMaxLevel downto FBaseLevel do
        begin
          for Face := 0 to 5 do
          begin
            if FCompressed then
              GL.CompressedTexImage2D(Face + GL_TEXTURE_CUBE_MAP_POSITIVE_X,
                Level, glInternalFormat,
                FImageStream.LevelOfDetail[Level, Face].Width,
                FImageStream.LevelOfDetail[Level, Face].Height,
                0,
                FImageStream.LevelOfDetail[Level, Face].Size,
                GetLevelData(Level, Face + GL_TEXTURE_CUBE_MAP_POSITIVE_X))
            else
              GL.TexImage2D(Face + GL_TEXTURE_CUBE_MAP_POSITIVE_X,
                Level, glInternalFormat,
                FImageStream.LevelOfDetail[Level, Face].Width,
                FImageStream.LevelOfDetail[Level, Face].Height,
                0,
                ColorFormat,
                DataType,
                GetLevelData(Level, Face + GL_TEXTURE_CUBE_MAP_POSITIVE_X));
          end;
        end;
{$ENDREGION}
{$REGION 'GL_TEXTURE_1D_ARRAY'}
      GL_TEXTURE_1D_ARRAY:
        for Level := FMaxLevel downto FBaseLevel do
        begin
          if FCompressed then
            GL.CompressedTexImage2D(glTarget, Level, glInternalFormat,
              FImageStream.LevelOfDetail[Level, 0].Width,
              FImageStream.LevelOfDetail[Level, 0].Height,
              0,
              FImageStream.LevelOfDetail[Level, 0].Size,
              GetLevelData(Level))
          else
            GL.TexImage2D(glTarget, Level, glInternalFormat,
              FImageStream.LevelOfDetail[Level, 0].Width,
              FImageStream.LevelOfDetail[Level, 0].Height,
              0,
              ColorFormat, DataType,
              GetLevelData(Level));
        end;
{$ENDREGION}
{$REGION 'GL_TEXTURE_2D_ARRAY'}
      GL_TEXTURE_2D_ARRAY:
        for Level := FMaxLevel downto FBaseLevel do
        begin
          if FCompressed then
            GL.CompressedTexImage3D(glTarget, Level, glInternalFormat,
              FImageStream.LevelOfDetail[Level, 0].Width,
              FImageStream.LevelOfDetail[Level, 0].Height,
              FImageStream.LevelOfDetail[Level, 0].Depth,
              0,
              FImageStream.LevelOfDetail[Level, 0].Size,
              GetLevelData(Level))
          else
            GL.TexImage3D(glTarget, Level, glInternalFormat,
              FImageStream.LevelOfDetail[Level, 0].Width,
              FImageStream.LevelOfDetail[Level, 0].Height,
              FImageStream.LevelOfDetail[Level, 0].Depth,
              0,
              ColorFormat,
              DataType,
              GetLevelData(Level));
        end;
{$ENDREGION}
{$REGION 'GL_TEXTURE_CUBE_MAP_ARRAY'}
      GL_TEXTURE_CUBE_MAP_ARRAY:
        ;
{$ENDREGION}
    end;
  end;

  GL.TexParameteri(glTarget, GL_TEXTURE_MAX_LEVEL, FMaxLevel);
  GL.TexParameteri(glTarget, GL_TEXTURE_BASE_LEVEL, FBaseLevel);

  if lImage <> FImage then
    lImage.Free;
  if Assigned(vtcBuffer) then
    FreeMem(vtcBuffer);
end;

procedure TGL3xTexture.Initialize;
var
  vTarget: TGLTextureTarget;
  rStream: TGLSResourceStream;

begin
  FHandle.AllocateHandle;
  if not FHandle.IsDataNeedUpdate then
    exit;

  GL.ClearError;

  if not Assigned(FImage) then
  begin
    if Name.GetIndex = 0 then
    begin
      rStream := CreateResourceStream(glsDIFFUSEMAP, GLS_RC_DDS_Type);
      ImportFromStream(rStream);
      rStream.Destroy;
    end
    else if Name.GetIndex = 1 then
    begin
      rStream := CreateResourceStream(glsNORMALMAP, GLS_RC_DDS_Type);
      ImportFromStream(rStream);
      rStream.Destroy;
    end
    else
      LoadFromFile(ResourceName);
    if not Assigned(FImage) then
    begin
      FImage := TGLImage.Create;
      FImage.SetErrorImage;
    end;
  end;

  // Cast luminance, alpha and intensity to red, red-green
  FImage.Uniformat;

  // Choose a texture target
  vTarget := GetTarget;
  if vTarget <> FHandle.Target then
  begin
    FHandle.DestroyHandle;
    FHandle.AllocateHandle;
    FHandle.Target := vTarget;
  end;

  FCompressed := IsCompressedFormat(FImage.InternalFormat);

  if FNeverStream or IsDesignTime then
    DoFullTransfer
  else
  begin
    FImage.BeginStreaming(FImageStream);
    FLastTime := GLSTime;
    DoStreamTransfer;
  end;

  if GL.GetError <> GL_NO_ERROR then
  begin
    FNeverStream := True;
    ShowAsError;
  end
  else
  begin
    FHandle.NotifyDataUpdated;
    if FHandle.IsDataComplitelyUpdated and NeverStream and not IsDesignTime then
      FreeAndNil(FImage);
  end;
end;

function TGL3xTexture.Apply(const AUniform: TGLSLUniform; UseOwnSampler: Boolean): TGLuint;
var
  ID: TGLuint;
begin
  if FHandle.IsDataNeedUpdate then
    Initialize;

  if FHandle.IsDataNeedUpdate then
    ID := 0
  else
  begin
    if not (FNeverStream or IsDesignTime) then
      DoStreamTransfer;
    ID := FHandle.Handle;
  end;

  Result := ShaderManager.UniformSampler(AUniform, ID);

  if UseOwnSampler and (ID > 0) and (Result <> $FFFFFFFF) then
    FSampler.Apply(Result, FHandle.Target, True);
end;

function TGL3xTexture.GetReady: Boolean;
begin
  Result := FHandle.IsDataComplitelyUpdated;
end;

function TGL3xTexture.GetFormat: TGLInternalFormat;
begin
  Result := FImage.InternalFormat;
end;

function TGL3xTexture.GetSourceFile: string;
begin
  if Assigned(FImage) then
    Result := FImage.ResourceName
  else
    Result := '';
end;

procedure TGL3xTexture.SetNeverStream(AValue: Boolean);
begin
  if FNeverStream <> AValue then
  begin
    FNeverStream := AValue;
    NotifyChange(Self);
  end;
end;

procedure TGL3xTexture.SetTexelCast(AValue: TTextureTexelCast);
begin
  if FTexelCast <> AValue then
  begin
    FTexelCast := AValue;
    FHandle.NotifyChangesOfData;
    NotifyChange(Self);
  end;
end;

class function TGL3xTexture.Capabilities: TDataFileCapabilities;
begin
  Result := [dfcRead, dfcWrite];
end;

{$ENDREGION{$ENDIF}

end.

