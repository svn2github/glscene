//
// This unit is part of the GLScene Project, http://glscene.org
//
{: HDRImage<p>
    Good for preview picture in OpenDialog,
    so you may include both HDRImage (preview) and GLFileHDR (loading)

      <li>23/10/10 - Yar - Removed PBuffer    
      <li>23/08/10 - Yar - Changes after PBuffer upgrade
      <li>21/03/10 - Yar - Added Linux support
                           (thanks to Rustam Asmandiarov aka Predator)
      <li>24/01/10 - Yar - Improved FPC compatibility
      <li>21/01/10 - Yar - Creation
   </ul></font>
}

unit HDRImage;

interface

{$I GLScene.inc}

uses
 {$IFDEF MSWINDOWS}Winapi.Windows, {$ENDIF}
  System.Classes,
  System.SysUtils,
  GLS.CrossPlatform,
  GLS.VectorGeometry,
  GLS.Graphics,
  GLS.OpenGLTokens;

type

  THDRImage = class(TGLBitmap)
  public
    { Public Declarations }
    procedure LoadFromStream(stream: TStream); override;
    procedure SaveToStream(stream: TStream); override;
  end;

implementation

uses
  GLS.FileHDR,
  GLS.TextureFormat;

// ------------------
// ------------------ THDRImage ------------------
// ------------------

// LoadFromStream
//

procedure THDRImage.LoadFromStream(stream: TStream);
var
  FullHDR: TGLHDRImage;
  src, dst: PGLubyte;
  y: integer;
begin
  FullHDR := TGLHDRImage.Create;
  try
    FullHDR.LoadFromStream(stream);
  except
    FullHDR.Free;
    raise;
  end;

  FullHDR.Narrow;

  Width := FullHDR.LevelWidth[0];
  Height := FullHDR.LevelHeight[0];
  Transparent := false;
  PixelFormat := glpf32bit;

  src := PGLubyte(FullHDR.Data);
  for y := 0 to Height - 1 do
  begin
    dst := ScanLine[Height - 1 - y];
    Move(src^, dst^, Width * 4);
    Inc(src, Width * 4);
  end;
  FullHDR.Free;
end;

// SaveToStream
//

procedure THDRImage.SaveToStream(stream: TStream);
begin
  Assert(False, 'Not supported');
end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization
  // ------------------------------------------------------------------
  // ------------------------------------------------------------------
  // ------------------------------------------------------------------

  TGLPicture.RegisterFileFormat(
    'HDR', 'High Dynamic Range Image', THDRImage);

  // ------------------------------------------------------------------
  // ------------------------------------------------------------------
  // ------------------------------------------------------------------
finalization
  // ------------------------------------------------------------------
  // ------------------------------------------------------------------
  // ------------------------------------------------------------------

  TGLPicture.UnregisterGraphicClass(THDRImage);

end.

