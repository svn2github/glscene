//
// This unit is part of the DGLEngine Project, http://glscene.org
//
{ @HTML ( HDRImage<p>
    Good for preview picture in OpenDialog,
    so you may include both HDRImage (preview) and GLFileHDR (loading)

      <li>28/12/15 - JD - Imported from GLScene
   </ul></font>
}

unit DGLHDRImage;

interface

{$I DGLEngine.inc}

uses
{$IFDEF MSWINDOWS}Windows,
{$ENDIF}Classes,
  SysUtils,
  DGLCrossPlatform,
  DGLTypes,
  DGLVectorMaths,
  DGLGraphics,
  dglOpenGL;

type

  THDRImage = class(TDGLBitmap)
  public
    { Public Declarations }
    procedure LoadFromStream(stream: TStream); override;
    procedure SaveToStream(stream: TStream); override;
  end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

uses
  DGLFileHDR,
  DGLTextureFormat;

// ------------------
// ------------------ THDRImage ------------------
// ------------------

// ------------------
{ THDRImage }
{$IFDEF GLS_REGIONS}{$REGION 'THDRImage'}{$ENDIF}

procedure THDRImage.LoadFromStream(stream: TStream);
var
  FullHDR: TDGLHDRImage;
  src, dst: PGLubyte;
  y: integer;
begin
  FullHDR := TDGLHDRImage.Create;
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

procedure THDRImage.SaveToStream(stream: TStream);
begin
  Assert(False, 'Not supported');
end;

{$IFDEF GLS_REGIONS}{$ENDREGION}{$ENDIF}

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization


  TDGLPicture.RegisterFileFormat('HDR', 'High Dynamic Range Image', THDRImage);

finalization

  TDGLPicture.UnregisterGraphicClass(THDRImage);

end.

