//
// VKScene project based on GLScene library, http://glscene.sourceforge.net 
//
{
    This class is required for loading images such classes as in DDSImage,
    TVKO3TCImage, TVKHDRImage etc.
     
}

unit VKS.CompositeImage;

interface

uses
  System.Classes,
  VKS.OpenGLTokens, VKS.Graphics, VKS.Texture, VKS.TextureFormat, VKS.Context;


type

  // TVKCompositeImage
  //

  TVKCompositeImage = class(TVKTextureImage)
  private
    FBitmap: TVKBitmap32;
    FWidth, FHeight, FDepth: integer;
  protected
    procedure SetWidth(val: Integer);
    procedure SetHeight(val: Integer);
    procedure SetDepth(val: Integer);
    function GetWidth: Integer; override;
    function GetHeight: Integer; override;
    function GetDepth: Integer; override;
    function GetTextureTarget: TVKTextureTarget; override;
  public

    constructor Create(AOwner: TPersistent); override;
    destructor Destroy; override;

    procedure Assign(Source: TPersistent); override;

    function GetBitmap32: TVKBitmap32; override;
    procedure ReleaseBitmap32; override;

    procedure SaveToFile(const fileName: string); override;
    procedure LoadFromFile(const fileName: string); override;
    procedure LoadFromStream(const AStream: TStream);
    class function FriendlyName: string; override;
    class function FriendlyDescription: string; override;
    property NativeTextureTarget;

  published
    property Width: Integer read GetWidth write SetWidth;
    property Height: Integer read GetHeight write SetHeight;
    property Depth: Integer read GetDepth write SetDepth;
  end;

implementation

// ------------------
// ------------------ TVKCompositeImage ------------------
// ------------------

// Create
//

constructor TVKCompositeImage.Create(AOwner: TPersistent);
begin
  inherited;
  FWidth := 256;
  FHeight := 256;
  FDepth := 0;
end;

// Destroy
//

destructor TVKCompositeImage.Destroy;
begin
  ReleaseBitmap32;
  inherited Destroy;
end;

// Assign
//

procedure TVKCompositeImage.Assign(Source: TPersistent);
begin
  if Assigned(Source) then
  begin
    if not Assigned(FBitmap) then
      FBitmap := TVKBitmap32.Create;
    if (Source is TVKCompositeImage) then
    begin
      FBitmap.Assign(TVKCompositeImage(Source).FBitmap);
    end
    else
      FBitmap.Assign(Source);

    FWidth := FBitmap.Width;
    FHeight := FBitmap.Height;
    FDepth := FBitmap.Depth;
    FResourceFile := FBitmap.ResourceName;
    // Composite image always rewrite texture format
    if Assigned(FOwnerTexture) then
      TVKTexture(FOwnerTexture).TextureFormatEx := FBitmap.InternalFormat;
    NotifyChange(Self);
  end
  else
    inherited;
end;

// SetWidth
//

procedure TVKCompositeImage.SetWidth(val: Integer);
begin
  if val <> FWidth then
  begin
    if val < 1 then
      val := 1;
    FWidth := val;
    Invalidate;
  end;
end;

// GetWidth
//

function TVKCompositeImage.GetWidth: Integer;
begin
  Result := FWidth;
end;

// SetHeight
//

procedure TVKCompositeImage.SetHeight(val: Integer);
begin
  if val <> FHeight then
  begin
    if val < 1 then
      val := 1;
    FHeight := val;
    Invalidate;
  end;
end;

// GetHeight
//

function TVKCompositeImage.GetHeight: Integer;
begin
  Result := FHeight;
end;

// SetDepth
//

procedure TVKCompositeImage.SetDepth(val: Integer);
begin
  if val <> FDepth then
  begin
    if val < 0 then
      val := 0;
    FDepth := val;
    Invalidate;
  end;
end;

// GetDepth
//

function TVKCompositeImage.GetDepth: Integer;
begin
  Result := FDepth;
end;

// GetBitmap32
//

function TVKCompositeImage.GetBitmap32: TVKBitmap32;
begin
  if not Assigned(FBitmap) then
  begin
    FBitmap := TVKBitmap32.Create;
    FBitmap.Blank := true;
    FWidth := 256;
    FHeight := 256;
    FDepth := 0;
    FBitmap.Width := FWidth;
    FBitmap.Height := FHeight;
    FBitmap.Depth := FDepth;
  end;
  Result := FBitmap;
end;

// ReleaseBitmap32
//

procedure TVKCompositeImage.ReleaseBitmap32;
begin
  if Assigned(FBitmap) then
  begin
    FBitmap.Free;
    FBitmap := nil;
  end;
end;

// SaveToFile
//

procedure TVKCompositeImage.SaveToFile(const fileName: string);
var
  BaseImageClass: TVKBaseImageClass;
  tempImage: TVKBaseImage;
  LOwner: TVKTexture;
begin
  if filename = '' then
    exit;
  BaseImageClass := GetRasterFileFormats.FindFromFileName(filename);
  tempImage := BaseImageClass.Create;
  if Assigned(FOwnerTexture) then
  begin
    LOwner := TVKTexture(FOwnerTexture);
    if not tempImage.AssignFromTexture(
      LOwner.TextureHandle, False, LOwner.TextureFormatEx) then
        tempImage.Assign(fBitmap);
  end
  else
    tempImage.Assign(fBitmap);
  try
    tempImage.SaveToFile(fileName);
    FResourceFile := fileName;
  finally
    tempImage.Free;
  end;
end;

// LoadFromFile
//

procedure TVKCompositeImage.LoadFromFile(const fileName: string);
var
  BaseImageClass: TVKBaseImageClass;
  tempImage: TVKBaseImage;
begin
  if filename = '' then
    exit;
  BaseImageClass := GetRasterFileFormats.FindFromFileName(filename);
  tempImage := BaseImageClass.Create;
  try
    tempImage.LoadFromFile(fileName);
    if not Assigned(FBitmap) then
      FBitmap := TVKBitmap32.Create;
    FBitmap.Assign(tempImage);
    FWidth := FBitmap.Width;
    FHeight := FBitmap.Height;
    FDepth := FBitmap.Depth;
    FResourceFile := FBitmap.ResourceName;
    // Internal image always rewrite texture format
    if Assigned(FOwnerTexture) then
      TVKTexture(FOwnerTexture).TextureFormatEx := FBitmap.InternalFormat;
    NotifyChange(Self);
  finally
    tempImage.Free;
  end;
end;

// LoadFromStream
//

procedure TVKCompositeImage.LoadFromStream(const AStream: TStream);
var
  tempImage: TVKBaseImage;
begin
  if (not Assigned(AStream)) or (AStream.Size - AStream.Position < 200) then
    exit;
  with GetRasterFileFormats do
    tempImage := FindFromStream(AStream).Create;
  try
    tempImage.LoadFromStream(AStream);
    if not Assigned(FBitmap) then
      FBitmap := TVKBitmap32.Create;
    FBitmap.Assign(tempImage);
    FWidth := FBitmap.Width;
    FHeight := FBitmap.Height;
    FDepth := FBitmap.Depth;
    FResourceFile := '';
    if Assigned(FOwnerTexture) then
      TVKTexture(FOwnerTexture).TextureFormatEx := FBitmap.InternalFormat;
    NotifyChange(Self);
  finally
    tempImage.Free;
  end;
end;

// FriendlyName
//

class function TVKCompositeImage.FriendlyName: string;
begin
  Result := 'Composite Image';
end;

// FriendlyDescription
//

class function TVKCompositeImage.FriendlyDescription: string;
begin
  Result := 'Image contained any internal formats of OpenGL textures';
end;

// GetTextureTarget
//

function TVKCompositeImage.GetTextureTarget: TVKTextureTarget;
begin
  if Assigned(fBitmap) then
    Result := fBitmap.GetTextureTarget
  else
    Result := ttNoShape;
end;

initialization
  RegisterGLTextureImageClass(TVKCompositeImage);

end.

