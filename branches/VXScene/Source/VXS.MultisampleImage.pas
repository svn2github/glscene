//
// VXScene Component Library, based on GLScene http://glscene.sourceforge.net 
//
{
    This unit provides support for two new types of "multisample
    textures" - two-dimensional and two-dimensional array - as well as
    mechanisms to fetch a specific sample from such a texture in a shader,
    and to attach such textures to FBOs for rendering.
    
}
unit VXS.MultisampleImage;

interface

{$I VXScene.inc}

uses
  System.Classes,

  Winapi.OpenGL, Winapi.OpenGLext, 
  VXS.Context,
  VXS.Texture,
  VXS.Graphics,
  VXS.TextureFormat;

type

  // TVXMultisampleImage
  //
  TVXMultisampleImage = class(TVXTextureImage)
  private
    
    FBitmap: TVXBitmap32;
    FSamplesCount: Integer;
    FWidth, FHeight, FDepth: Integer;
    FFixedSamplesLocation: GLboolean;
    procedure SetWidth(val: Integer);
    procedure SetHeight(val: Integer);
    procedure SetDepth(val: Integer);
    procedure SetSamplesCount(val: Integer);
    procedure SetFixedSamplesLocation(val: GLboolean);
  protected
    
    function GetWidth: Integer; override;
    function GetHeight: Integer; override;
    function GetDepth: Integer; override;
    function GetTextureTarget: TVXTextureTarget; override;
  public
    
    constructor Create(AOwner: TPersistent); override;
    destructor Destroy; override;

    procedure Assign(Source: TPersistent); override;

    class function IsSelfLoading: Boolean; override;
    procedure LoadTexture(AInternalFormat: TVXInternalFormat); override;
    function GetBitmap32: TVXBitmap32; override;
    procedure ReleaseBitmap32; override;

    procedure SaveToFile(const fileName: string); override;
    procedure LoadFromFile(const fileName: string); override;
    class function FriendlyName: string; override;
    class function FriendlyDescription: string; override;

    property NativeTextureTarget;

  published
    
    { Width of the blank image (for memory allocation). }
    property Width: Integer read GetWidth write SetWidth default 256;
    { Width of the blank image (for memory allocation). }
    property Height: Integer read GetHeight write SetHeight default 256;
    property Depth: Integer read GetDepth write SetDepth default 0;
    property SamplesCount: Integer read FSamplesCount write SetSamplesCount
      default 0;
    property FixedSamplesLocation: GLboolean read FFixedSamplesLocation write
      SetFixedSamplesLocation;
  end;

implementation

// ------------------
// ------------------ TVXMultisampleImage ------------------
// ------------------

{$IFDEF VXS_REGIONS}{$REGION 'TVXMultisampleImage'}{$ENDIF}

// Create
//

constructor TVXMultisampleImage.Create(AOwner: TPersistent);
begin
  inherited;
  FWidth := 256;
  FHeight := 256;
  FDepth := 0;
  FSamplesCount := 0;
end;

// Destroy
//

destructor TVXMultisampleImage.Destroy;
begin
  ReleaseBitmap32;
  inherited Destroy;
end;

// Assign
//

procedure TVXMultisampleImage.Assign(Source: TPersistent);
begin
  if Assigned(Source) then
  begin
    if (Source is TVXMultisampleImage) then
    begin
      FWidth := TVXMultisampleImage(Source).FWidth;
      FHeight := TVXMultisampleImage(Source).FHeight;
      FDepth := TVXMultisampleImage(Source).FDepth;
      FSamplesCount := TVXMultisampleImage(Source).FSamplesCount;
      Invalidate;
    end
    else
      inherited;
  end
  else
    inherited;
end;

// SetWidth
//

procedure TVXMultisampleImage.SetWidth(val: Integer);
begin
  if val <> FWidth then
  begin
    FWidth := val;
    if FWidth < 1 then
      FWidth := 1;
    Invalidate;
  end;
end;

// GetWidth
//

function TVXMultisampleImage.GetWidth: Integer;
begin
  Result := FWidth;
end;

// SetHeight
//

procedure TVXMultisampleImage.SetHeight(val: Integer);
begin
  if val <> FHeight then
  begin
    FHeight := val;
    if FHeight < 1 then
      FHeight := 1;
    Invalidate;
  end;
end;

// GetHeight
//

function TVXMultisampleImage.GetHeight: Integer;
begin
  Result := FHeight;
end;

// GetDepth
//

function TVXMultisampleImage.GetDepth: Integer;
begin
  Result := FDepth;
end;

// SetHeight
//

procedure TVXMultisampleImage.SetDepth(val: Integer);
begin
  if val <> FDepth then
  begin
    FDepth := val;
    if FDepth < 0 then
      FDepth := 0;
    Invalidate;
  end;
end;

// SetSamplesCount
//

procedure TVXMultisampleImage.SetSamplesCount(val: Integer);
begin
  if val < 0 then
    val := 0;

  if val <> FSamplesCount then
  begin
    FSamplesCount := val;
    Invalidate;
  end;
end;

// SetFixedSamplesLocation
//

procedure TVXMultisampleImage.SetFixedSamplesLocation(val: GLboolean);
begin
  if val <> FFixedSamplesLocation then
  begin
    FFixedSamplesLocation := val;
    Invalidate;
  end;
end;

// GetBitmap32
//

function TVXMultisampleImage.GetBitmap32: TVXBitmap32;
begin
  if not Assigned(FBitmap) then
  begin
    FBitmap := TVXBitmap32.Create;
    FBitmap.Blank := true;
    FBitmap.Width := FWidth;
    FBitmap.Height := FHeight;
  end;
  Result := FBitmap;
end;

// ReleaseBitmap32
//

procedure TVXMultisampleImage.ReleaseBitmap32;
begin
  FBitmap.Free;
  FBitmap := nil;
end;

// SaveToFile
//

procedure TVXMultisampleImage.SaveToFile(const fileName: string);
begin
end;

// LoadFromFile
//

procedure TVXMultisampleImage.LoadFromFile(const fileName: string);
begin
end;

// FriendlyName
//

class function TVXMultisampleImage.FriendlyName: string;
begin
  Result := 'Multisample Image';
end;

// FriendlyDescription
//

class function TVXMultisampleImage.FriendlyDescription: string;
begin
  Result := 'Image for rendering to texture with antialiasing';
end;

// GetTextureTarget
//

function TVXMultisampleImage.GetTextureTarget: TVXTextureTarget;
begin
  if fDepth > 0 then
    Result := ttTexture2DMultisampleArray
  else
    Result := ttTexture2DMultisample;
end;

class function TVXMultisampleImage.IsSelfLoading: Boolean;
begin
  Result := True;
end;

procedure TVXMultisampleImage.LoadTexture(AInternalFormat: TVXInternalFormat);
var
  target: TVXTextureTarget;
  maxSamples, maxSize: GLint;
begin
  // Check smaples count range
  glGetIntegerv(GL_MAX_SAMPLES, @maxSamples);
  if FSamplesCount > maxSamples then
    FSamplesCount := maxSamples;
  if IsDepthFormat(AInternalFormat) then
  begin
    glGetIntegerv(GL_MAX_DEPTH_TEXTURE_SAMPLES, @maxSamples);
    if FSamplesCount > maxSamples then
      FSamplesCount := maxSamples;
  end
  else
  begin
    glGetIntegerv(GL_MAX_COLOR_TEXTURE_SAMPLES, @maxSamples);
    if FSamplesCount > maxSamples then
      FSamplesCount := maxSamples;
  end;
  // Check texture size
  glGetIntegerv(GL_MAX_TEXTURE_SIZE, @maxSize);
  if FWidth > maxSize then
    FWidth := maxSize;
  if FHeight > maxSize then
    FHeight := maxSize;

  target := NativeTextureTarget;
  case target of

    ttTexture2DMultisample:
      glTexImage2DMultisample(
        DecodeTextureTarget(target),
        SamplesCount,
        InternalFormatToOpenVXFormat(AInternalFormat),
        Width,
        Height,
        FFixedSamplesLocation);

    ttTexture2DMultisampleArray:
      glTexImage3DMultisample(
        DecodeTextureTarget(target),
        SamplesCount,
        InternalFormatToOpenVXFormat(AInternalFormat),
        Width,
        Height,
        Depth,
        FFixedSamplesLocation);
  end;
end;

{$IFDEF VXS_REGIONS}{$ENDREGION}{$ENDIF}

initialization
  RegisterTextureImageClass(TVXMultisampleImage);

end.