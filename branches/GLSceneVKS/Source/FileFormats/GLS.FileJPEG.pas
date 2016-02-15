//
// GLScene on Vulkan, http://glscene.sourceforge.net 
//

unit GLS.FileJPEG;

interface

{$I GLScene.inc}

uses
  System.Classes, System.SysUtils,
  GLS.CrossPlatform, GLS.OpenGLTokens, GLS.Context, GLS.Graphics, GLS.TextureFormat,
  GLS.ApplicationFileIO;

type

  TVKJPEGImage = class(TVKBaseImage)
  private
    FAbortLoading: boolean;
    FDivScale: longword;
    FDither: boolean;
    FSmoothing: boolean;
    FProgressiveEncoding: boolean;
    procedure SetSmoothing(const AValue: boolean);
  public
    constructor Create; override;
    class function Capabilities: TVKDataFileCapabilities; override;

    procedure LoadFromFile(const filename: string); override;
    procedure SaveToFile(const filename: string); override;
    procedure LoadFromStream(stream: TStream); override;
    procedure SaveToStream(stream: TStream); override;

    { Assigns from any Texture.}
    procedure AssignFromTexture(textureContext: TVKContext;
      const textureHandle: GLuint;
      textureTarget: TVKTextureTarget;
      const CurrentFormat: boolean;
      const intFormat: GLinternalFormat); reintroduce;

    property DivScale: longword read FDivScale write FDivScale;
    property Dither: boolean read FDither write FDither;
    property Smoothing: boolean read FSmoothing write SetSmoothing;
    property ProgressiveEncoding: boolean read FProgressiveEncoding;
  end;

implementation

uses
   GLS.VectorGeometry;

// ------------------
// ------------------ TVKJPEGImage ------------------
// ------------------

constructor TVKJPEGImage.Create;
begin
  inherited;
  FAbortLoading := False;
  FDivScale := 1;
  FDither := False;
end;

// LoadFromFile


procedure TVKJPEGImage.LoadFromFile(const filename: string);
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


procedure TVKJPEGImage.SaveToFile(const filename: string);
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

procedure TVKJPEGImage.LoadFromStream(stream: TStream);
begin
  //Do nothing
end;


procedure TVKJPEGImage.SaveToStream(stream: TStream);
begin
  //Do nothing
end;

// AssignFromTexture


procedure TVKJPEGImage.AssignFromTexture(textureContext: TVKContext;
  const textureHandle: GLuint; textureTarget: TVKTextureTarget;
  const CurrentFormat: boolean; const intFormat: GLinternalFormat);
begin

end;

procedure TVKJPEGImage.SetSmoothing(const AValue: boolean);
begin
  if FSmoothing <> AValue then
    FSmoothing := AValue;
end;

// Capabilities


class function TVKJPEGImage.Capabilities: TVKDataFileCapabilities;
begin
  Result := [dfcRead {, dfcWrite}];
end;

initialization

  { Register this Fileformat-Handler with GLScene }
  RegisterRasterFormat('jpg', 'Joint Photographic Experts Group Image',
    TVKJPEGImage);
  RegisterRasterFormat('jpeg', 'Joint Photographic Experts Group Image',
    TVKJPEGImage);
  RegisterRasterFormat('jpe', 'Joint Photographic Experts Group Image',
    TVKJPEGImage);
end.

