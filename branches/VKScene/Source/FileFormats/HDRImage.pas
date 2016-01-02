//
// This unit is part of the GLScene Project   
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

{$I VKScene.inc}

uses
 {$IFDEF MSWINDOWS}Winapi.Windows, {$ENDIF}
  System.Classes,
  System.SysUtils,
  VKS.CrossPlatform,
  VKS.VectorGeometry,
  VKS.Graphics,
  VKS.OpenGLTokens;

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
  VKS.FileHDR,
  VKS.TextureFormat;

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

