//
// GLScene on Vulkan, http://glscene.sourceforge.net 
//
{
    Good for preview picture in OpenDialog,
    so you may include both HDRImage (preview) and GLFileHDR (loading)
      
}

unit HDRImage;

interface

{$I GLScene.inc}

uses
  Winapi.Windows,
  Winapi.OpenGL,
  System.Classes,
  System.SysUtils,
  GLS.CrossPlatform,
  GLS.VectorGeometry,
  GLS.Graphics;

type

  THDRImage = class(TVKBitmap)
  public
    { Public Declarations }
    { TODO : E2170 Cannot override a non-virtual method }

    procedure LoadFromStream(stream: TStream); //in VCL override;
    procedure SaveToStream(stream: TStream); //in VCL override;

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
  FullHDR: TVKHDRImage;
  src, dst: PGLubyte;
  y: integer;
begin
  FullHDR := TVKHDRImage.Create;
  try
    FullHDR.LoadFromStream(stream);
  except
    FullHDR.Free;
    raise;
  end;

  FullHDR.Narrow;

  Width := FullHDR.LevelWidth[0];
  Height := FullHDR.LevelHeight[0];
  { TODO : E2064 Left side cannot be assigned to }
  (*
  Transparent := false;
  PixelFormat := glpf32bit;
  *)

  src := PGLubyte(FullHDR.Data);
  for y := 0 to Height - 1 do
  begin
    { TODO : E2003 Undeclared identifier: 'ScanLine' }
    (*dst := ScanLine[Height - 1 - y];*)
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
  { TODO : E2003 Undeclared identifier: 'RegisterFileFormat', it needs to be added }
  (*TVKPicture.RegisterFileFormat('HDR', 'High Dynamic Range Image', THDRImage);*)

  // ------------------------------------------------------------------
  // ------------------------------------------------------------------
  // ------------------------------------------------------------------
finalization
  // ------------------------------------------------------------------
  // ------------------------------------------------------------------
  // ------------------------------------------------------------------
  { TODO : E2003 Undeclared identifier: 'UnregisterFileFormat', it needs to be added }
  (*TVKPicture.UnregisterGraphicClass(THDRImage);*)

end.

