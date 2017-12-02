//
// VXScene Component Library, based on GLScene http://glscene.sourceforge.net
//

unit VXS.FileJPEG;

interface

{$I VXScene.inc}

uses
  Winapi.OpenGL,
  Winapi.OpenGLext,
  System.Classes,
  System.SysUtils,
  VXS.VectorGeometry,
  VXS.CrossPlatform,
  VXS.Context,
  VXS.Graphics,
  VXS.TextureFormat,
  VXS.ApplicationFileIO;

type

  TVXJPEGImage = class(TVXBaseImage)
  private
    FAbortLoading: boolean;
    FDivScale: longword;
    FDither: boolean;
    FSmoothing: boolean;
    FProgressiveEncoding: boolean;
    procedure SetSmoothing(const AValue: boolean);
  public
    constructor Create; override;
    class function Capabilities: TVXDataFileCapabilities; override;
    procedure LoadFromFile(const filename: string); override;
    procedure SaveToFile(const filename: string); override;
    procedure LoadFromStream(stream: TStream); override;
    procedure SaveToStream(stream: TStream); override;
    { Assigns from any Texture. }
    procedure AssignFromTexture(textureContext: TVXContext; const textureHandle: GLuint; textureTarget: TVXTextureTarget;
      const CurrentFormat: boolean; const intFormat: TVXInternalFormat); reintroduce;
    property DivScale: longword read FDivScale write FDivScale;
    property Dither: boolean read FDither write FDither;
    property Smoothing: boolean read FSmoothing write SetSmoothing;
    property ProgressiveEncoding: boolean read FProgressiveEncoding;
  end;

  // ===================================================================
implementation

// ===================================================================

// ------------------
// ------------------ TVXJPEGImage ------------------
// ------------------

constructor TVXJPEGImage.Create;
begin
  inherited;
  FAbortLoading := False;
  FDivScale := 1;
  FDither := False;
end;

procedure TVXJPEGImage.LoadFromFile(const filename: string);
var
  fs: TStream;
begin
  if FileStreamExists(filename) then
  begin
    fs := CreateFileStream(filename, fmOpenRead);
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

procedure TVXJPEGImage.SaveToFile(const filename: string);
var
  fs: TStream;
begin
  fs := CreateFileStream(filename, fmOpenWrite or fmCreate);
  try
    SaveToStream(fs);
  finally
    fs.Free;
  end;
  ResourceName := filename;
end;

procedure TVXJPEGImage.LoadFromStream(stream: TStream);
begin
  // Do nothing
end;

procedure TVXJPEGImage.SaveToStream(stream: TStream);
begin
  // Do nothing
end;

procedure TVXJPEGImage.AssignFromTexture(textureContext: TVXContext; const textureHandle: GLuint;
  textureTarget: TVXTextureTarget; const CurrentFormat: boolean; const intFormat: TVXInternalFormat);
begin
  //
end;

procedure TVXJPEGImage.SetSmoothing(const AValue: boolean);
begin
  if FSmoothing <> AValue then
    FSmoothing := AValue;
end;

class function TVXJPEGImage.Capabilities: TVXDataFileCapabilities;
begin
  Result := [dfcRead { , dfcWrite } ];
end;

// ------------------------------------------------------------------
initialization

// ------------------------------------------------------------------

{ Register this Fileformat-Handler }
RegisterRasterFormat('jpg', 'Joint Photographic Experts Group Image', TVXJPEGImage);
RegisterRasterFormat('jpeg', 'Joint Photographic Experts Group Image', TVXJPEGImage);
RegisterRasterFormat('jpe', 'Joint Photographic Experts Group Image', TVXJPEGImage);

end.
