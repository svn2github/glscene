//
// This unit is part of the GLScene Project, http://glscene.org
//
{ : ImageUtils<p>

   Main purpose is as a fallback in cases where there is no other way to process images.<p>

  <b>Historique : </b><font size=-1><ul>
      <li>04/11/10 - DaStr - Added $I GLScene.inc
      <li>22/10/10 - Yar - Created
  </ul></font>
}
unit ImageUtils;

// TODO: ConvertImage
// DONE: S3TC decompression
// DONE: LATC decompression
// DONE: RGTC decompression
// TODO: BPTC decompression
// TODO: S3TC compression
// TODO: LATC compression
// TODO: RGTC compression
// TODO: BPTC compression
// DONE: ResizeImage
// TODO: Build2DMipmap
// TODO: Build3DMipmap

interface

{$I GLScene.inc}

uses
  SysUtils,
  GLCrossPlatform,
  OpenGLTokens,
  GLTextureFormat,
  VectorGeometry;

const
  cFilterWidth = 5; // Relative sample radius for filtering

type

  TIntermediateFormat = record
    R, G, B, A: Single;
  end;

  PRGBA32F = ^TIntermediateFormat;
  TIntermediateFormatArray = array [0 .. MaxInt div (2 * SizeOf(TIntermediateFormat))] of TIntermediateFormat;
  PIntermediateFormatArray = ^TIntermediateFormatArray;

  TU48BitBlock = array [0 .. 3, 0 .. 3] of Byte;
  T48BitBlock = array [0 .. 3, 0 .. 3] of SmallInt;

  EGLImageUtils = class(Exception);

  TImageFilterFunction = function(Value: Single): Single;

function ImageBoxFilter(Value: Single): Single;
function ImageTriangleFilter(Value: Single): Single;
function ImageHermiteFilter(Value: Single): Single;
function ImageBellFilter(Value: Single): Single;
function ImageSplineFilter(Value: Single): Single;
function ImageLanczos3Filter(Value: Single): Single;
function ImageMitchellFilter(Value: Single): Single;

procedure ConvertImage(const ASrc: Pointer; const ADst: Pointer; ASrcColorFormat, ADstColorFormat: TGLEnum; ASrcDataType, ADstDataType: TGLEnum; AWidth, AHeight: Integer);

procedure RescaleImage(const ASrc: Pointer; const ADst: Pointer; AColorFormat: TGLEnum; ADataType: TGLEnum; AFilter: TImageFilterFunction; ASrcWidth, ASrcHeight, ADstWidth, ADstHeight: Integer);

implementation

resourcestring
  strInvalidType = 'Invalid data type';

const
  cSuperBlack: TIntermediateFormat = (R:0.0; G:0.0; B:0.0; A:0.0);

type
  TConvertToImfProc = procedure(ASource: Pointer; ADest: PIntermediateFormatArray; AColorFormat: TGLEnum; AWidth, AHeight: Integer);
  TConvertFromInfProc = procedure(ASource: PIntermediateFormatArray; ADest: Pointer; AColorFormat: TGLEnum; AWidth, AHeight: Integer);

{$IFDEF GLS_COMPILER_2005_UP}{$REGION 'OpenGL format image to RGBA Float'}{$ENDIF}

procedure UnsupportedToImf(ASource: Pointer; ADest: PIntermediateFormatArray; AColorFormat: TGLEnum; AWidth, AHeight: Integer);
begin
  raise EGLImageUtils.Create('Unimplemented type of conversion');
end;

procedure UbyteToImf(ASource: Pointer; ADest: PIntermediateFormatArray; AColorFormat: TGLEnum; AWidth, AHeight: Integer);
var
  pSource: PByte;
  n: Integer;
  c0: Single;

  function GetChannel: Single;
{$IFDEF GLS_INLINE} inline;
{$ENDIF}
  begin
    Result := pSource^;
    Inc(pSource);
  end;

begin
  pSource := PByte(ASource);

  case AColorFormat of
{$INCLUDE ImgUtilCaseGL2Imf.inc}
  else
    raise EGLImageUtils.Create(strInvalidType);
  end;
end;

procedure Ubyte332ToImf(ASource: Pointer; ADest: PIntermediateFormatArray; AColorFormat: TGLEnum; AWidth, AHeight: Integer);
var
  pSource: PByte;
  c0, c1, c2, c3: Byte;
  n: Integer;

  procedure GetChannel;
{$IFDEF GLS_INLINE} inline;
{$ENDIF}
  begin
    c0 := pSource^;
    c1 := $E0 and c0;
    c2 := $E0 and (c0 shl 3);
    c3 := $C0 and (c0 shl 6);
    Inc(pSource);
  end;

begin
  pSource := PByte(ASource);

  case AColorFormat of

    GL_RGB:
      for n := 0 to AWidth * AHeight - 1 do
      begin
        GetChannel;
        ADest[n].R := c1;
        ADest[n].G := c2;
        ADest[n].B := c3;
      end;

    GL_BGR:
      for n := 0 to AWidth * AHeight - 1 do
      begin
        GetChannel;
        ADest[n].B := c1;
        ADest[n].G := c2;
        ADest[n].R := c3;
      end;
  else
    raise EGLImageUtils.Create(strInvalidType);
  end;
end;

procedure Ubyte233RToImf(ASource: Pointer; ADest: PIntermediateFormatArray; AColorFormat: TGLEnum; AWidth, AHeight: Integer);
var
  pSource: PByte;
  c0, c1, c2, c3: Byte;
  n: Integer;

  procedure GetChannel;
{$IFDEF GLS_INLINE} inline;
{$ENDIF}
  begin
    c0 := pSource^;
    c3 := $E0 and c0;
    c2 := $E0 and (c0 shl 3);
    c1 := $C0 and (c0 shl 6);
    Inc(pSource);
  end;

begin
  pSource := PByte(ASource);

  case AColorFormat of

    GL_RGB:
      for n := 0 to AWidth * AHeight - 1 do
      begin
        GetChannel;
        ADest[n].R := c1;
        ADest[n].G := c2;
        ADest[n].B := c3;
      end;

    GL_BGR:
      for n := 0 to AWidth * AHeight - 1 do
      begin
        GetChannel;
        ADest[n].B := c1;
        ADest[n].G := c2;
        ADest[n].R := c3;
      end;
  else
    raise EGLImageUtils.Create(strInvalidType);
  end;
end;

procedure ByteToImf(ASource: Pointer; ADest: PIntermediateFormatArray; AColorFormat: TGLEnum; AWidth, AHeight: Integer);
var
  pSource: PShortInt;
  n: Integer;
  c0: Single;

  function GetChannel: Single;
{$IFDEF GLS_INLINE} inline;
{$ENDIF}
  begin
    Result := pSource^;
    Inc(pSource);
  end;

begin
  pSource := PShortInt(ASource);

  case AColorFormat of
{$INCLUDE ImgUtilCaseGL2Imf.inc}
  else
    raise EGLImageUtils.Create(strInvalidType);
  end;
end;

procedure UShortToImf(ASource: Pointer; ADest: PIntermediateFormatArray; AColorFormat: TGLEnum; AWidth, AHeight: Integer);
var
  pSource: PWord;
  n: Integer;
  c0: Single;

  function GetChannel: Single;
{$IFDEF GLS_INLINE} inline;
{$ENDIF}
  begin
    Result := pSource^ / $100;
    Inc(pSource);
  end;

begin
  pSource := PWord(ASource);

  case AColorFormat of
{$INCLUDE ImgUtilCaseGL2Imf.inc}
  else
    raise EGLImageUtils.Create(strInvalidType);
  end;
end;

procedure ShortToImf(ASource: Pointer; ADest: PIntermediateFormatArray; AColorFormat: TGLEnum; AWidth, AHeight: Integer);
var
  pSource: PSmallInt;
  n: Integer;
  c0: Single;

  function GetChannel: Single;
{$IFDEF GLS_INLINE} inline;
{$ENDIF}
  begin
    Result := pSource^ / $100;
    Inc(pSource);
  end;

begin
  pSource := PSmallInt(ASource);

  case AColorFormat of
{$INCLUDE ImgUtilCaseGL2Imf.inc}
  else
    raise EGLImageUtils.Create(strInvalidType);
  end;
end;

procedure UIntToImf(ASource: Pointer; ADest: PIntermediateFormatArray; AColorFormat: TGLEnum; AWidth, AHeight: Integer);
var
  pSource: PLongWord;
  n: Integer;
  c0: Single;

  function GetChannel: Single;
{$IFDEF GLS_INLINE} inline;
{$ENDIF}
  begin
    Result := pSource^ / $1000000;
    Inc(pSource);
  end;

begin
  pSource := PLongWord(ASource);

  case AColorFormat of
{$INCLUDE ImgUtilCaseGL2Imf.inc}
  else
    raise EGLImageUtils.Create(strInvalidType);
  end;
end;

procedure IntToImf(ASource: Pointer; ADest: PIntermediateFormatArray; AColorFormat: TGLEnum; AWidth, AHeight: Integer);
var
  pSource: PLongInt;
  n: Integer;
  c0: Single;

  function GetChannel: Single;
{$IFDEF GLS_INLINE} inline;
{$ENDIF}
  begin
    Result := pSource^ / $1000000;
    Inc(pSource);
  end;

begin
  pSource := PLongInt(ASource);

  case AColorFormat of
{$INCLUDE ImgUtilCaseGL2Imf.inc}
  else
    raise EGLImageUtils.Create(strInvalidType);
  end;
end;

procedure FloatToImf(ASource: Pointer; ADest: PIntermediateFormatArray; AColorFormat: TGLEnum; AWidth, AHeight: Integer);
var
  pSource: PSingle;
  n: Integer;
  c0: Single;

  function GetChannel: Single;
{$IFDEF GLS_INLINE} inline;
{$ENDIF}
  begin
    Result := pSource^ * 255.0;
    Inc(pSource);
  end;

begin
  pSource := PSingle(ASource);

  case AColorFormat of
{$INCLUDE ImgUtilCaseGL2Imf.inc}
  else
    raise EGLImageUtils.Create(strInvalidType);
  end;
end;

procedure HalfFloatToImf(ASource: Pointer; ADest: PIntermediateFormatArray; AColorFormat: TGLEnum; AWidth, AHeight: Integer);
var
  pSource: PHalfFloat;
  n: Integer;
  c0: Single;

  function GetChannel: Single;
{$IFDEF GLS_INLINE} inline;
{$ENDIF}
  begin
    Result := HalfToFloat(pSource^) * 255.0;
    Inc(pSource);
  end;

begin
  pSource := PHalfFloat(ASource);

  case AColorFormat of
{$INCLUDE ImgUtilCaseGL2Imf.inc}
  else
    raise EGLImageUtils.Create(strInvalidType);
  end;
end;

procedure UInt8888ToImf(ASource: Pointer; ADest: PIntermediateFormatArray; AColorFormat: TGLEnum; AWidth, AHeight: Integer);
var
  pSource: PByte;
  n: Integer;
  c0, c1, c2, c3: Byte;

  procedure GetChannel;
{$IFDEF GLS_INLINE} inline;
{$ENDIF}
  begin
    c0 := pSource^;
    Inc(pSource);
    c1 := pSource^;
    Inc(pSource);
    c2 := pSource^;
    Inc(pSource);
    c3 := pSource^;
    Inc(pSource);
  end;

begin
  pSource := PByte(ASource);

  case AColorFormat of

    GL_RGBA, GL_RGBA_INTEGER:
      for n := 0 to AWidth * AHeight - 1 do
      begin
        GetChannel;
        ADest[n].R := c0;
        ADest[n].G := c1;
        ADest[n].B := c2;
        ADest[n].A := c3;
      end;

    GL_BGRA, GL_BGRA_INTEGER:
      for n := 0 to AWidth * AHeight - 1 do
      begin
        GetChannel;
        ADest[n].B := c0;
        ADest[n].G := c1;
        ADest[n].R := c2;
        ADest[n].A := c3;
      end;
  else
    raise EGLImageUtils.Create(strInvalidType);
  end;
end;

procedure UInt8888RevToImf(ASource: Pointer; ADest: PIntermediateFormatArray; AColorFormat: TGLEnum; AWidth, AHeight: Integer);
var
  pSource: PByte;
  n: Integer;
  c0, c1, c2, c3: Byte;

  procedure GetChannel;
{$IFDEF GLS_INLINE} inline;
{$ENDIF}
  begin
    c3 := pSource^;
    Inc(pSource);
    c2 := pSource^;
    Inc(pSource);
    c1 := pSource^;
    Inc(pSource);
    c0 := pSource^;
    Inc(pSource);
  end;

begin
  pSource := PByte(ASource);

  case AColorFormat of

    GL_RGBA, GL_RGBA_INTEGER:
      for n := 0 to AWidth * AHeight - 1 do
      begin
        GetChannel;
        ADest[n].R := c0;
        ADest[n].G := c1;
        ADest[n].B := c2;
        ADest[n].A := c3;
      end;

    GL_BGRA, GL_BGRA_INTEGER:
      for n := 0 to AWidth * AHeight - 1 do
      begin
        GetChannel;
        ADest[n].B := c0;
        ADest[n].G := c1;
        ADest[n].R := c2;
        ADest[n].A := c3;
      end;
  else
    raise EGLImageUtils.Create(strInvalidType);
  end;
end;

procedure UShort4444ToImf(ASource: Pointer; ADest: PIntermediateFormatArray; AColorFormat: TGLEnum; AWidth, AHeight: Integer);
var
  pSource: PByte;
  n: Integer;
  c0, c1, c2, c3, c4: Byte;

  procedure GetChannel;
{$IFDEF GLS_INLINE} inline;
{$ENDIF}
  begin
    c0 := pSource^;
    c3 := $F0 and (c0 shl 4);
    c4 := $F0 and c0;
    Inc(pSource);
    c0 := pSource^;
    c1 := $F0 and (c0 shl 4);
    c2 := $F0 and c0;
    Inc(pSource);
  end;

begin
  pSource := PByte(ASource);

  case AColorFormat of

    GL_RGBA, GL_RGBA_INTEGER:
      for n := 0 to AWidth * AHeight - 1 do
      begin
        GetChannel;
        ADest[n].R := c1;
        ADest[n].G := c2;
        ADest[n].B := c3;
        ADest[n].A := c4;
      end;

    GL_BGRA, GL_BGRA_INTEGER:
      for n := 0 to AWidth * AHeight - 1 do
      begin
        GetChannel;
        ADest[n].R := c1;
        ADest[n].G := c2;
        ADest[n].B := c3;
        ADest[n].A := c4;
      end;
  else
    raise EGLImageUtils.Create(strInvalidType);
  end;
end;

procedure UShort4444RevToImf(ASource: Pointer; ADest: PIntermediateFormatArray; AColorFormat: TGLEnum; AWidth, AHeight: Integer);
var
  pSource: PByte;
  n: Integer;
  c0, c1, c2, c3, c4: Byte;

  procedure GetChannel;
{$IFDEF GLS_INLINE} inline;
{$ENDIF}
  begin
    c0 := pSource^;
    c1 := $F0 and (c0 shl 4);
    c2 := $F0 and c0;
    Inc(pSource);
    c0 := pSource^;
    c3 := $F0 and (c0 shl 4);
    c4 := $F0 and c0;
    Inc(pSource);
  end;

begin
  pSource := PByte(ASource);

  case AColorFormat of

    GL_RGBA, GL_RGBA_INTEGER:
      for n := 0 to AWidth * AHeight - 1 do
      begin
        GetChannel;
        ADest[n].R := c1;
        ADest[n].G := c2;
        ADest[n].B := c3;
        ADest[n].A := c4;
      end;

    GL_BGRA, GL_BGRA_INTEGER:
      for n := 0 to AWidth * AHeight - 1 do
      begin
        GetChannel;
        ADest[n].B := c1;
        ADest[n].G := c2;
        ADest[n].R := c3;
        ADest[n].A := c4;
      end;
  else
    raise EGLImageUtils.Create(strInvalidType);
  end;
end;

procedure UShort565ToImf(ASource: Pointer; ADest: PIntermediateFormatArray; AColorFormat: TGLEnum; AWidth, AHeight: Integer);
var
  pSource: PWord;
  n: Integer;
  c0: Word;
  c1, c2, c3: Byte;

  procedure GetChannel;
{$IFDEF GLS_INLINE} inline;
{$ENDIF}
  begin
    c0 := pSource^;
    c3 := (c0 and $001F) shl 3;
    c2 := (c0 and $07E0) shr 3;
    c1 := (c0 and $F800) shr 8;
    Inc(pSource);
  end;

begin
  pSource := PWord(ASource);

  case AColorFormat of

    GL_RGB, GL_RGB_INTEGER:
      for n := 0 to AWidth * AHeight - 1 do
      begin
        GetChannel;
        ADest[n].R := c1;
        ADest[n].G := c2;
        ADest[n].B := c3;
      end;

    GL_BGR, GL_BGR_INTEGER:
      for n := 0 to AWidth * AHeight - 1 do
      begin
        GetChannel;
        ADest[n].B := c1;
        ADest[n].G := c2;
        ADest[n].R := c3;
      end;
  else
    raise EGLImageUtils.Create(strInvalidType);
  end;
end;

procedure UShort565RevToImf(ASource: Pointer; ADest: PIntermediateFormatArray; AColorFormat: TGLEnum; AWidth, AHeight: Integer);
var
  pSource: PWord;
  n: Integer;
  c0: Word;
  c1, c2, c3: Byte;

  procedure GetChannel;
{$IFDEF GLS_INLINE} inline;
{$ENDIF}
  begin
    c0 := pSource^;
    c1 := (c0 and $001F) shl 3;
    c2 := (c0 and $07E0) shr 3;
    c3 := (c0 and $F800) shr 8;
    Inc(pSource);
  end;

begin
  pSource := PWord(ASource);

  case AColorFormat of

    GL_RGB, GL_RGB_INTEGER:
      for n := 0 to AWidth * AHeight - 1 do
      begin
        GetChannel;
        ADest[n].R := c1;
        ADest[n].G := c2;
        ADest[n].B := c3;
      end;

    GL_BGR, GL_BGR_INTEGER:
      for n := 0 to AWidth * AHeight - 1 do
      begin
        GetChannel;
        ADest[n].B := c1;
        ADest[n].G := c2;
        ADest[n].R := c3;
      end;
  else
    raise EGLImageUtils.Create(strInvalidType);
  end;
end;

procedure UShort5551ToImf(ASource: Pointer; ADest: PIntermediateFormatArray; AColorFormat: TGLEnum; AWidth, AHeight: Integer);
var
  pSource: PWord;
  n: Integer;
  c0: Word;
  c1, c2, c3, c4: Byte;

  procedure GetChannel;
{$IFDEF GLS_INLINE} inline;
{$ENDIF}
  begin
    c0 := pSource^;
    c4 := (c0 and $001F) shl 3;
    c3 := (c0 and $03E0) shr 2;
    c2 := (c0 and $7C00) shr 7;
    c1 := (c0 and $8000) shr 8;
    Inc(pSource);
  end;

begin
  pSource := PWord(ASource);

  case AColorFormat of

    GL_RGBA, GL_RGBA_INTEGER:
      for n := 0 to AWidth * AHeight - 1 do
      begin
        GetChannel;
        ADest[n].R := c1;
        ADest[n].G := c2;
        ADest[n].B := c3;
        ADest[n].A := c4;
      end;

    GL_BGRA, GL_BGRA_INTEGER:
      for n := 0 to AWidth * AHeight - 1 do
      begin
        GetChannel;
        ADest[n].B := c1;
        ADest[n].G := c2;
        ADest[n].R := c3;
        ADest[n].A := c4;
      end;
  else
    raise EGLImageUtils.Create(strInvalidType);
  end;
end;

procedure UShort5551RevToImf(ASource: Pointer; ADest: PIntermediateFormatArray; AColorFormat: TGLEnum; AWidth, AHeight: Integer);
var
  pSource: PWord;
  n: Integer;
  c0: Word;
  c1, c2, c3, c4: Byte;

  procedure GetChannel;
{$IFDEF GLS_INLINE} inline;
{$ENDIF}
  begin
    c0 := pSource^;
    c1 := (c0 and $001F) shl 3;
    c2 := (c0 and $03E0) shr 2;
    c3 := (c0 and $7C00) shr 7;
    c4 := (c0 and $8000) shr 8;
    Inc(pSource);
  end;

begin
  pSource := PWord(ASource);

  case AColorFormat of

    GL_RGBA, GL_RGBA_INTEGER:
      for n := 0 to AWidth * AHeight - 1 do
      begin
        GetChannel;
        ADest[n].R := c1;
        ADest[n].G := c2;
        ADest[n].B := c3;
        ADest[n].A := c4;
      end;

    GL_BGRA, GL_BGRA_INTEGER:
      for n := 0 to AWidth * AHeight - 1 do
      begin
        GetChannel;
        ADest[n].B := c1;
        ADest[n].G := c2;
        ADest[n].R := c3;
        ADest[n].A := c4;
      end;
  else
    raise EGLImageUtils.Create(strInvalidType);
  end;
end;

procedure UInt_10_10_10_2_ToImf(ASource: Pointer; ADest: PIntermediateFormatArray; AColorFormat: TGLEnum; AWidth, AHeight: Integer);
var
  pSource: PLongWord;
  n: Integer;
  c0: LongWord;
  c1, c2, c3, c4: Word;

  procedure GetChannel;
{$IFDEF GLS_INLINE} inline;
{$ENDIF}
  begin
    c0 := pSource^;
    c1 := (c0 and $000003FF) shl 6;
    c2 := (c0 and $000FFC00) shr 4;
    c3 := (c0 and $3FF00000) shr 14;
    c4 := (c0 and $C0000000) shr 16;
    Inc(pSource);
  end;

begin
  pSource := PLongWord(ASource);

  case AColorFormat of

    GL_RGBA, GL_RGBA_INTEGER:
      for n := 0 to AWidth * AHeight - 1 do
      begin
        GetChannel;
        ADest[n].R := c1 / $100;
        ADest[n].G := c2 / $100;
        ADest[n].B := c3 / $100;
        ADest[n].A := c4;
      end;

    GL_BGRA, GL_BGRA_INTEGER:
      for n := 0 to AWidth * AHeight - 1 do
      begin
        GetChannel;
        ADest[n].B := c1 / $100;
        ADest[n].G := c2 / $100;
        ADest[n].R := c3 / $100;
        ADest[n].A := c4;
      end;
  else
    raise EGLImageUtils.Create(strInvalidType);
  end;
end;

procedure UInt_10_10_10_2_Rev_ToImf(ASource: Pointer; ADest: PIntermediateFormatArray; AColorFormat: TGLEnum; AWidth, AHeight: Integer);
var
  pSource: PLongWord;
  n: Integer;
  c0: LongWord;
  c1, c2, c3, c4: Word;

  procedure GetChannel;
{$IFDEF GLS_INLINE} inline;
{$ENDIF}
  begin
    c0 := pSource^;
    c1 := (c0 and $000003FF) shl 6;
    c2 := (c0 and $000FFC00) shr 4;
    c3 := (c0 and $3FF00000) shr 14;
    c4 := (c0 and $C0000000) shr 16;
    Inc(pSource);
  end;

begin
  pSource := PLongWord(ASource);

  case AColorFormat of

    GL_RGBA, GL_RGBA_INTEGER:
      for n := 0 to AWidth * AHeight - 1 do
      begin
        GetChannel;
        ADest[n].R := c1 / $100;
        ADest[n].G := c2 / $100;
        ADest[n].B := c3 / $100;
        ADest[n].A := c4;
      end;

    GL_BGRA, GL_BGRA_INTEGER:
      for n := 0 to AWidth * AHeight - 1 do
      begin
        GetChannel;
        ADest[n].B := c1 / $100;
        ADest[n].G := c2 / $100;
        ADest[n].R := c3 / $100;
        ADest[n].A := c4;
      end;
  else
    raise EGLImageUtils.Create(strInvalidType);
  end;
end;

{$IFDEF GLS_COMPILER_2005_UP}{$ENDREGION}{$ENDIF}
{$IFDEF GLS_COMPILER_2005_UP}{$REGION 'Compression'}{$ENDIF}

procedure DecodeColor565(col: Word; out R, G, B: Byte);
begin
  R := col and $1F;
  G := (col shr 5) and $3F;
  B := (col shr 11) and $1F;
end;

procedure DXT1_ToImf(ASource: Pointer; ADest: PIntermediateFormatArray; AColorFormat: TGLEnum; AWidth, AHeight: Integer);
var
  x, y, i, j, k, select, offset: Integer;
  col0, col1: Word;
  colors: TU48BitBlock;
  bitmask: Cardinal;
  temp: PGLubyte;
  r0, g0, b0, r1, g1, b1: Byte;
begin

  temp := PGLubyte(ASource);
  for y := 0 to (AHeight div 4) - 1 do
  begin
    for x := 0 to (AWidth div 4) - 1 do
    begin
      col0 := PWord(temp)^;
      Inc(temp, 2);
      col1 := PWord(temp)^;
      Inc(temp, 2);
      bitmask := PCardinal(temp)^;
      Inc(temp, 4);

      DecodeColor565(col0, r0, g0, b0);
      DecodeColor565(col1, r1, g1, b1);

      colors[0][0] := r0 shl 3;
      colors[0][1] := g0 shl 2;
      colors[0][2] := b0 shl 3;
      colors[0][3] := $FF;
      colors[1][0] := r1 shl 3;
      colors[1][1] := g1 shl 2;
      colors[1][2] := b1 shl 3;
      colors[1][3] := $FF;

      if col0 > col1 then
      begin
        colors[2][0] := (2 * colors[0][0] + colors[1][0] + 1) div 3;
        colors[2][1] := (2 * colors[0][1] + colors[1][1] + 1) div 3;
        colors[2][2] := (2 * colors[0][2] + colors[1][2] + 1) div 3;
        colors[2][3] := $FF;
        colors[3][0] := (colors[0][0] + 2 * colors[1][0] + 1) div 3;
        colors[3][1] := (colors[0][1] + 2 * colors[1][1] + 1) div 3;
        colors[3][2] := (colors[0][2] + 2 * colors[1][2] + 1) div 3;
        colors[3][3] := $FF;
      end
      else
      begin
        colors[2][0] := (colors[0][0] + colors[1][0]) div 2;
        colors[2][1] := (colors[0][1] + colors[1][1]) div 2;
        colors[2][2] := (colors[0][2] + colors[1][2]) div 2;
        colors[2][3] := $FF;
        colors[3][0] := (colors[0][0] + 2 * colors[1][0] + 1) div 3;
        colors[3][1] := (colors[0][1] + 2 * colors[1][1] + 1) div 3;
        colors[3][2] := (colors[0][2] + 2 * colors[1][2] + 1) div 3;
        colors[3][3] := 0;
      end;

      k := 0;
      for j := 0 to 3 do
      begin
        for i := 0 to 3 do
        begin
          select := (bitmask and (3 shl (k * 2))) shr (k * 2);
          if ((4 * x + i) < AWidth) and ((4 * y + j) < AHeight) then
          begin
            offset := ((4 * y + j) * AWidth + (4 * x + i));
            ADest[offset].B := colors[select][0];
            ADest[offset].G := colors[select][1];
            ADest[offset].R := colors[select][2];
            ADest[offset].A := colors[select][3];
          end;
          Inc(k);
        end;
      end;

    end;
  end;
end;

procedure DXT3_ToImf(ASource: Pointer; ADest: PIntermediateFormatArray; AColorFormat: TGLEnum; AWidth, AHeight: Integer);
var
  x, y, i, j, k, select: Integer;
  col0, col1, wrd: Word;
  colors: TU48BitBlock;
  bitmask, offset: Cardinal;
  temp: PGLubyte;
  r0, g0, b0, r1, g1, b1: Byte;
  alpha: array [0 .. 3] of Word;
begin
  temp := PGLubyte(ASource);
  for y := 0 to (AHeight div 4) - 1 do
  begin
    for x := 0 to (AWidth div 4) - 1 do
    begin
      alpha[0] := PWord(temp)^;
      Inc(temp, 2);
      alpha[1] := PWord(temp)^;
      Inc(temp, 2);
      alpha[2] := PWord(temp)^;
      Inc(temp, 2);
      alpha[3] := PWord(temp)^;
      Inc(temp, 2);
      col0 := PWord(temp)^;
      Inc(temp, 2);
      col1 := PWord(temp)^;
      Inc(temp, 2);
      bitmask := PCardinal(temp)^;
      Inc(temp, 4);

      DecodeColor565(col0, r0, g0, b0);
      DecodeColor565(col1, r1, g1, b1);

      colors[0][0] := r0 shl 3;
      colors[0][1] := g0 shl 2;
      colors[0][2] := b0 shl 3;
      colors[0][3] := $FF;
      colors[1][0] := r1 shl 3;
      colors[1][1] := g1 shl 2;
      colors[1][2] := b1 shl 3;
      colors[1][3] := $FF;
      colors[2][0] := (2 * colors[0][0] + colors[1][0] + 1) div 3;
      colors[2][1] := (2 * colors[0][1] + colors[1][1] + 1) div 3;
      colors[2][2] := (2 * colors[0][2] + colors[1][2] + 1) div 3;
      colors[2][3] := $FF;
      colors[3][0] := (colors[0][0] + 2 * colors[1][0] + 1) div 3;
      colors[3][1] := (colors[0][1] + 2 * colors[1][1] + 1) div 3;
      colors[3][2] := (colors[0][2] + 2 * colors[1][2] + 1) div 3;
      colors[3][3] := $FF;

      k := 0;
      for j := 0 to 3 do
      begin
        for i := 0 to 3 do
        begin
          select := (bitmask and (3 shl (k * 2))) shr (k * 2);
          if ((4 * x + i) < AWidth) and ((4 * y + j) < AHeight) then
          begin
            offset := ((4 * y + j) * AWidth + (4 * x + i));
            ADest[offset].B := colors[select][0];
            ADest[offset].G := colors[select][1];
            ADest[offset].R := colors[select][2];
            ADest[offset].A := colors[select][3];
          end;
          Inc(k);
        end;
      end;

      for j := 0 to 3 do
      begin
        wrd := alpha[j];
        for i := 0 to 3 do
        begin
          if (((4 * x + i) < AWidth) and ((4 * y + j) < AHeight)) then
          begin
            offset := ((4 * y + j) * AWidth + (4 * x + i));
            r0 := wrd and $0F;
            ADest[offset].A := r0 or (r0 shl 4);
          end;
          wrd := wrd shr 4;
        end;
      end;

    end;
  end;
end;

procedure DXT5_ToImf(ASource: Pointer; ADest: PIntermediateFormatArray; AColorFormat: TGLEnum; AWidth, AHeight: Integer);
var
  x, y, i, j, k, select, offset: Integer;
  col0, col1: Word;
  colors: TU48BitBlock;
  bits, bitmask: Cardinal;
  temp, alphamask: PGLubyte;
  r0, g0, b0, r1, g1, b1: Byte;
  alphas: array [0 .. 7] of Byte;
begin
  temp := PGLubyte(ASource);
  for y := 0 to (AHeight div 4) - 1 do
  begin
    for x := 0 to (AWidth div 4) - 1 do
    begin
      alphas[0] := temp^;
      Inc(temp);
      alphas[1] := temp^;
      Inc(temp);
      alphamask := temp;
      Inc(temp, 6);
      col0 := PWord(temp)^;
      Inc(temp, 2);
      col1 := PWord(temp)^;
      Inc(temp, 2);
      bitmask := PCardinal(temp)^;
      Inc(temp, 4);

      DecodeColor565(col0, r0, g0, b0);
      DecodeColor565(col1, r1, g1, b1);

      colors[0][0] := r0 shl 3;
      colors[0][1] := g0 shl 2;
      colors[0][2] := b0 shl 3;
      colors[0][3] := $FF;
      colors[1][0] := r1 shl 3;
      colors[1][1] := g1 shl 2;
      colors[1][2] := b1 shl 3;
      colors[1][3] := $FF;
      colors[2][0] := (2 * colors[0][0] + colors[1][0] + 1) div 3;
      colors[2][1] := (2 * colors[0][1] + colors[1][1] + 1) div 3;
      colors[2][2] := (2 * colors[0][2] + colors[1][2] + 1) div 3;
      colors[2][3] := $FF;
      colors[3][0] := (colors[0][0] + 2 * colors[1][0] + 1) div 3;
      colors[3][1] := (colors[0][1] + 2 * colors[1][1] + 1) div 3;
      colors[3][2] := (colors[0][2] + 2 * colors[1][2] + 1) div 3;
      colors[3][3] := $FF;

      k := 0;
      for j := 0 to 3 do
      begin
        for i := 0 to 3 do
        begin
          select := (bitmask and (3 shl (k * 2))) shr (k * 2);
          if ((4 * x + i) < AWidth) and ((4 * y + j) < AHeight) then
          begin
            offset := ((4 * y + j) * AWidth + (4 * x + i));
            ADest[offset].B := colors[select][0];
            ADest[offset].G := colors[select][1];
            ADest[offset].R := colors[select][2];
          end;
          Inc(k);
        end;
      end;

      if (alphas[0] > alphas[1]) then
      begin
        alphas[2] := (6 * alphas[0] + 1 * alphas[1] + 3) div 7;
        alphas[3] := (5 * alphas[0] + 2 * alphas[1] + 3) div 7;
        alphas[4] := (4 * alphas[0] + 3 * alphas[1] + 3) div 7;
        alphas[5] := (3 * alphas[0] + 4 * alphas[1] + 3) div 7;
        alphas[6] := (2 * alphas[0] + 5 * alphas[1] + 3) div 7;
        alphas[7] := (1 * alphas[0] + 6 * alphas[1] + 3) div 7;
      end
      else
      begin
        alphas[2] := (4 * alphas[0] + 1 * alphas[1] + 2) div 5;
        alphas[3] := (3 * alphas[0] + 2 * alphas[1] + 2) div 5;
        alphas[4] := (2 * alphas[0] + 3 * alphas[1] + 2) div 5;
        alphas[5] := (1 * alphas[0] + 4 * alphas[1] + 2) div 5;
        alphas[6] := 0;
        alphas[7] := $FF;
      end;

      bits := PCardinal(alphamask)^;
      for j := 0 to 1 do
      begin
        for i := 0 to 3 do
        begin
          if (((4 * x + i) < AWidth) and ((4 * y + j) < AHeight)) then
          begin
            offset := ((4 * y + j) * AWidth + (4 * x + i));
            ADest[offset].A := alphas[bits and 7];
          end;
          bits := bits shr 3;
        end;
      end;

      Inc(alphamask, 3);
      bits := PCardinal(alphamask)^;
      for j := 2 to 3 do
      begin
        for i := 0 to 3 do
        begin
          if (((4 * x + i) < AWidth) and ((4 * y + j) < AHeight)) then
          begin
            offset := ((4 * y + j) * AWidth + (4 * x + i));
            ADest[offset].A := alphas[bits and 7];
          end;
          bits := bits shr 3;
        end;
      end;

    end;
  end;
end;

procedure Decode48BitBlock(ACode: Int64; out ABlock: TU48BitBlock); overload;
var
  x, y: Byte;
begin
  for y := 0 to 3 do
    for x := 0 to 3 do
    begin
      ABlock[x][y] := Byte(ACode and $03);
      ACode := ACode shr 2;
    end;
end;

procedure Decode48BitBlock(ACode: Int64; out ABlock: T48BitBlock); overload;
var
  x, y: Byte;
begin
  for y := 0 to 3 do
    for x := 0 to 3 do
    begin
      ABlock[x][y] := SmallInt(ACode and $03);
      ACode := ACode shr 2;
    end;
end;

procedure LATC1_ToImf(ASource: Pointer; ADest: PIntermediateFormatArray; AColorFormat: TGLEnum; AWidth, AHeight: Integer);
var
  x, y, i, j, offset: Integer;
  LUM0, LUM1: Byte;
  lum: Single;
  colors: TU48BitBlock;
  bitmask: Int64;
  temp: PGLubyte;
begin

  temp := PGLubyte(ASource);
  for y := 0 to (AHeight div 4) - 1 do
  begin
    for x := 0 to (AWidth div 4) - 1 do
    begin
      LUM0 := temp^;
      Inc(temp);
      LUM1 := temp^;
      Inc(temp);
      bitmask := PInt64(temp)^;
      Inc(temp, 6);
      Decode48BitBlock(bitmask, colors);

      for j := 0 to 3 do
      begin
        for i := 0 to 3 do
        begin
          if LUM0 > LUM1 then
            case colors[j, i] of
              0:
                colors[j, i] := LUM0;
              1:
                colors[j, i] := LUM1;
              2:
                colors[j, i] := (6 * LUM0 + LUM1) div 7;
              3:
                colors[j, i] := (5 * LUM0 + 2 * LUM1) div 7;
              4:
                colors[j, i] := (4 * LUM0 + 3 * LUM1) div 7;
              5:
                colors[j, i] := (3 * LUM0 + 4 * LUM1) div 7;
              6:
                colors[j, i] := (2 * LUM0 + 5 * LUM1) div 7;
              7:
                colors[j, i] := (LUM0 + 6 * LUM1) div 7;
            end
          else
            case colors[j, i] of
              0:
                colors[j, i] := LUM0;
              1:
                colors[j, i] := LUM1;
              2:
                colors[j, i] := (4 * LUM0 + LUM1) div 5;
              3:
                colors[j, i] := (3 * LUM0 + 2 * LUM1) div 5;
              4:
                colors[j, i] := (2 * LUM0 + 3 * LUM1) div 5;
              5:
                colors[j, i] := (LUM0 + 4 * LUM1) div 5;
              6:
                colors[j, i] := 0;
              7:
                colors[j, i] := 255;
            end;
          if ((4 * x + i) < AWidth) and ((4 * y + j) < AHeight) then
          begin
            offset := ((4 * y + j) * AWidth + (4 * x + i));
            lum := colors[j, i];
            ADest[offset].R := lum;
            ADest[offset].G := lum;
            ADest[offset].B := lum;
            ADest[offset].A := 255.0;
          end;
        end;

      end;
    end;
  end;
end;

procedure SLATC1_ToImf(ASource: Pointer; ADest: PIntermediateFormatArray; AColorFormat: TGLEnum; AWidth, AHeight: Integer);
var
  x, y, i, j, offset: Integer;
  LUM0, LUM1: SmallInt;
  lum: Single;
  colors: T48BitBlock;
  bitmask: Int64;
  temp: PGLubyte;
begin

  temp := PGLubyte(ASource);
  for y := 0 to (AHeight div 4) - 1 do
  begin
    for x := 0 to (AWidth div 4) - 1 do
    begin
      LUM0 := PSmallInt(temp)^;
      Inc(temp);
      LUM1 := PSmallInt(temp)^;
      Inc(temp);
      bitmask := PInt64(temp)^;
      Inc(temp, 6);
      Decode48BitBlock(bitmask, colors);

      for j := 0 to 3 do
      begin
        for i := 0 to 3 do
        begin
          if LUM0 > LUM1 then
            case colors[j, i] of
              0:
                colors[j, i] := LUM0;
              1:
                colors[j, i] := LUM1;
              2:
                colors[j, i] := (6 * LUM0 + LUM1) div 7;
              3:
                colors[j, i] := (5 * LUM0 + 2 * LUM1) div 7;
              4:
                colors[j, i] := (4 * LUM0 + 3 * LUM1) div 7;
              5:
                colors[j, i] := (3 * LUM0 + 4 * LUM1) div 7;
              6:
                colors[j, i] := (2 * LUM0 + 5 * LUM1) div 7;
              7:
                colors[j, i] := (LUM0 + 6 * LUM1) div 7;
            end
          else
            case colors[j, i] of
              0:
                colors[j, i] := LUM0;
              1:
                colors[j, i] := LUM1;
              2:
                colors[j, i] := (4 * LUM0 + LUM1) div 5;
              3:
                colors[j, i] := (3 * LUM0 + 2 * LUM1) div 5;
              4:
                colors[j, i] := (2 * LUM0 + 3 * LUM1) div 5;
              5:
                colors[j, i] := (LUM0 + 4 * LUM1) div 5;
              6:
                colors[j, i] := -127;
              7:
                colors[j, i] := 127;
            end;
          if ((4 * x + i) < AWidth) and ((4 * y + j) < AHeight) then
          begin
            offset := ((4 * y + j) * AWidth + (4 * x + i));
            lum := 2 * colors[j, i];
            ADest[offset].R := lum;
            ADest[offset].G := lum;
            ADest[offset].B := lum;
            ADest[offset].A := 127.0;
          end;
        end;

      end;
    end;
  end;
end;

procedure LATC2_ToImf(ASource: Pointer; ADest: PIntermediateFormatArray; AColorFormat: TGLEnum; AWidth, AHeight: Integer);
var
  x, y, i, j, offset: Integer;
  LUM0, LUM1: Byte;
  lum: Single;
  colors: TU48BitBlock;
  bitmask: Int64;
  temp: PGLubyte;
begin

  temp := PGLubyte(ASource);
  for y := 0 to (AHeight div 4) - 1 do
  begin
    for x := 0 to (AWidth div 4) - 1 do
    begin
      LUM0 := temp^;
      Inc(temp);
      LUM1 := temp^;
      Inc(temp);
      bitmask := PInt64(temp)^;
      Inc(temp, 6);
      Decode48BitBlock(bitmask, colors);

      for j := 0 to 3 do
      begin
        for i := 0 to 3 do
        begin
          if LUM0 > LUM1 then
            case colors[j, i] of
              0:
                colors[j, i] := LUM0;
              1:
                colors[j, i] := LUM1;
              2:
                colors[j, i] := (6 * LUM0 + LUM1) div 7;
              3:
                colors[j, i] := (5 * LUM0 + 2 * LUM1) div 7;
              4:
                colors[j, i] := (4 * LUM0 + 3 * LUM1) div 7;
              5:
                colors[j, i] := (3 * LUM0 + 4 * LUM1) div 7;
              6:
                colors[j, i] := (2 * LUM0 + 5 * LUM1) div 7;
              7:
                colors[j, i] := (LUM0 + 6 * LUM1) div 7;
            end
          else
            case colors[j, i] of
              0:
                colors[j, i] := LUM0;
              1:
                colors[j, i] := LUM1;
              2:
                colors[j, i] := (4 * LUM0 + LUM1) div 5;
              3:
                colors[j, i] := (3 * LUM0 + 2 * LUM1) div 5;
              4:
                colors[j, i] := (2 * LUM0 + 3 * LUM1) div 5;
              5:
                colors[j, i] := (LUM0 + 4 * LUM1) div 5;
              6:
                colors[j, i] := 0;
              7:
                colors[j, i] := 255;
            end;
          if ((4 * x + i) < AWidth) and ((4 * y + j) < AHeight) then
          begin
            offset := ((4 * y + j) * AWidth + (4 * x + i));
            lum := colors[j][i];
            ADest[offset].R := lum;
            ADest[offset].G := lum;
            ADest[offset].B := lum;
          end;
        end; // for i
      end; // for j

      LUM0 := temp^;
      Inc(temp);
      LUM1 := temp^;
      Inc(temp);
      bitmask := PInt64(temp)^;
      Inc(temp, 6);
      Decode48BitBlock(bitmask, colors);

      for j := 0 to 3 do
      begin
        for i := 0 to 3 do
        begin
          if LUM0 > LUM1 then
            case colors[j, i] of
              0:
                colors[j, i] := LUM0;
              1:
                colors[j, i] := LUM1;
              2:
                colors[j, i] := (6 * LUM0 + LUM1) div 7;
              3:
                colors[j, i] := (5 * LUM0 + 2 * LUM1) div 7;
              4:
                colors[j, i] := (4 * LUM0 + 3 * LUM1) div 7;
              5:
                colors[j, i] := (3 * LUM0 + 4 * LUM1) div 7;
              6:
                colors[j, i] := (2 * LUM0 + 5 * LUM1) div 7;
              7:
                colors[j, i] := (LUM0 + 6 * LUM1) div 7;
            end
          else
            case colors[j, i] of
              0:
                colors[j, i] := LUM0;
              1:
                colors[j, i] := LUM1;
              2:
                colors[j, i] := (4 * LUM0 + LUM1) div 5;
              3:
                colors[j, i] := (3 * LUM0 + 2 * LUM1) div 5;
              4:
                colors[j, i] := (2 * LUM0 + 3 * LUM1) div 5;
              5:
                colors[j, i] := (LUM0 + 4 * LUM1) div 5;
              6:
                colors[j, i] := 0;
              7:
                colors[j, i] := 255;
            end;
          if ((4 * x + i) < AWidth) and ((4 * y + j) < AHeight) then
            ADest[((4 * y + j) * AWidth + (4 * x + i))].A := colors[j][i];
        end;
      end;

    end;
  end;
end;

procedure SLATC2_ToImf(ASource: Pointer; ADest: PIntermediateFormatArray; AColorFormat: TGLEnum; AWidth, AHeight: Integer);
var
  x, y, i, j, offset: Integer;
  LUM0, LUM1: SmallInt;
  lum: Single;
  colors: T48BitBlock;
  bitmask: Int64;
  temp: PGLubyte;
begin

  temp := PGLubyte(ASource);
  for y := 0 to (AHeight div 4) - 1 do
  begin
    for x := 0 to (AWidth div 4) - 1 do
    begin
      LUM0 := PSmallInt(temp)^;
      Inc(temp);
      LUM1 := PSmallInt(temp)^;
      Inc(temp);
      bitmask := PInt64(temp)^;
      Inc(temp, 6);
      Decode48BitBlock(bitmask, colors);

      for j := 0 to 3 do
      begin
        for i := 0 to 3 do
        begin
          if LUM0 > LUM1 then
            case colors[j, i] of
              0:
                colors[j, i] := LUM0;
              1:
                colors[j, i] := LUM1;
              2:
                colors[j, i] := (6 * LUM0 + LUM1) div 7;
              3:
                colors[j, i] := (5 * LUM0 + 2 * LUM1) div 7;
              4:
                colors[j, i] := (4 * LUM0 + 3 * LUM1) div 7;
              5:
                colors[j, i] := (3 * LUM0 + 4 * LUM1) div 7;
              6:
                colors[j, i] := (2 * LUM0 + 5 * LUM1) div 7;
              7:
                colors[j, i] := (LUM0 + 6 * LUM1) div 7;
            end
          else
            case colors[j, i] of
              0:
                colors[j, i] := LUM0;
              1:
                colors[j, i] := LUM1;
              2:
                colors[j, i] := (4 * LUM0 + LUM1) div 5;
              3:
                colors[j, i] := (3 * LUM0 + 2 * LUM1) div 5;
              4:
                colors[j, i] := (2 * LUM0 + 3 * LUM1) div 5;
              5:
                colors[j, i] := (LUM0 + 4 * LUM1) div 5;
              6:
                colors[j, i] := -127;
              7:
                colors[j, i] := 127;
            end;
          if ((4 * x + i) < AWidth) and ((4 * y + j) < AHeight) then
          begin
            offset := ((4 * y + j) * AWidth + (4 * x + i));
            lum := 2 * colors[j][i];
            ADest[offset].R := lum;
            ADest[offset].G := lum;
            ADest[offset].B := lum;
          end;
        end;
      end;

      LUM0 := PSmallInt(temp)^;
      Inc(temp);
      LUM1 := PSmallInt(temp)^;
      Inc(temp);
      bitmask := PInt64(temp)^;
      Inc(temp, 6);
      Decode48BitBlock(bitmask, colors);

      for j := 0 to 3 do
      begin
        for i := 0 to 3 do
        begin
          if LUM0 > LUM1 then
            case colors[j, i] of
              0:
                colors[j, i] := LUM0;
              1:
                colors[j, i] := LUM1;
              2:
                colors[j, i] := (6 * LUM0 + LUM1) div 7;
              3:
                colors[j, i] := (5 * LUM0 + 2 * LUM1) div 7;
              4:
                colors[j, i] := (4 * LUM0 + 3 * LUM1) div 7;
              5:
                colors[j, i] := (3 * LUM0 + 4 * LUM1) div 7;
              6:
                colors[j, i] := (2 * LUM0 + 5 * LUM1) div 7;
              7:
                colors[j, i] := (LUM0 + 6 * LUM1) div 7;
            end
          else
            case colors[j, i] of
              0:
                colors[j, i] := LUM0;
              1:
                colors[j, i] := LUM1;
              2:
                colors[j, i] := (4 * LUM0 + LUM1) div 5;
              3:
                colors[j, i] := (3 * LUM0 + 2 * LUM1) div 5;
              4:
                colors[j, i] := (2 * LUM0 + 3 * LUM1) div 5;
              5:
                colors[j, i] := (LUM0 + 4 * LUM1) div 5;
              6:
                colors[j, i] := -127;
              7:
                colors[j, i] := 127;
            end;
          if ((4 * x + i) < AWidth) and ((4 * y + j) < AHeight) then
          begin
            ADest[((4 * y + j) * AWidth + (4 * x + i))].A := 2 * colors[j][i];
          end;
        end;
      end;
    end;
  end;
end;

procedure RGTC1_ToImf(ASource: Pointer; ADest: PIntermediateFormatArray; AColorFormat: TGLEnum; AWidth, AHeight: Integer);
var
  x, y, i, j, offset: Integer;
  RED0, RED1: Byte;
  lum: Single;
  colors: TU48BitBlock;
  bitmask: Int64;
  temp: PGLubyte;
begin

  temp := PGLubyte(ASource);
  for y := 0 to (AHeight div 4) - 1 do
  begin
    for x := 0 to (AWidth div 4) - 1 do
    begin
      RED0 := temp^;
      Inc(temp);
      RED1 := temp^;
      Inc(temp);
      bitmask := PInt64(temp)^;
      Inc(temp, 6);
      Decode48BitBlock(bitmask, colors);

      for j := 0 to 3 do
      begin
        for i := 0 to 3 do
        begin
          if RED0 > RED1 then
            case colors[j, i] of
              0:
                colors[j, i] := RED0;
              1:
                colors[j, i] := RED1;
              2:
                colors[j, i] := (6 * RED0 + RED1) div 7;
              3:
                colors[j, i] := (5 * RED0 + 2 * RED1) div 7;
              4:
                colors[j, i] := (4 * RED0 + 3 * RED1) div 7;
              5:
                colors[j, i] := (3 * RED0 + 4 * RED1) div 7;
              6:
                colors[j, i] := (2 * RED0 + 5 * RED1) div 7;
              7:
                colors[j, i] := (RED0 + 6 * RED1) div 7;
            end
          else
            case colors[j, i] of
              0:
                colors[j, i] := RED0;
              1:
                colors[j, i] := RED1;
              2:
                colors[j, i] := (4 * RED0 + RED1) div 5;
              3:
                colors[j, i] := (3 * RED0 + 2 * RED1) div 5;
              4:
                colors[j, i] := (2 * RED0 + 3 * RED1) div 5;
              5:
                colors[j, i] := (RED0 + 4 * RED1) div 5;
              6:
                colors[j, i] := 0;
              7:
                colors[j, i] := 255;
            end;
          if ((4 * x + i) < AWidth) and ((4 * y + j) < AHeight) then
          begin
            offset := ((4 * y + j) * AWidth + (4 * x + i)) * 4;
            lum := colors[j][i];
            ADest[offset].R := lum;
            ADest[offset].G := 0.0;
            ADest[offset].B := 0.0;
            ADest[offset].A := 255.0;
          end;
        end;

      end;
    end;
  end;
end;

procedure SRGTC1_ToImf(ASource: Pointer; ADest: PIntermediateFormatArray; AColorFormat: TGLEnum; AWidth, AHeight: Integer);
var
  x, y, i, j, offset: Integer;
  RED0, RED1: SmallInt;
  lum: Single;
  colors: T48BitBlock;
  bitmask: Int64;
  temp: PGLubyte;
begin

  temp := PGLubyte(ASource);
  for y := 0 to (AHeight div 4) - 1 do
  begin
    for x := 0 to (AWidth div 4) - 1 do
    begin
      RED0 := PSmallInt(temp)^;
      Inc(temp);
      RED1 := PSmallInt(temp)^;
      Inc(temp);
      bitmask := PInt64(temp)^;
      Inc(temp, 6);
      Decode48BitBlock(bitmask, colors);

      for j := 0 to 3 do
      begin
        for i := 0 to 3 do
        begin
          if RED0 > RED1 then
            case colors[j, i] of
              0:
                colors[j, i] := RED0;
              1:
                colors[j, i] := RED1;
              2:
                colors[j, i] := (6 * RED0 + RED1) div 7;
              3:
                colors[j, i] := (5 * RED0 + 2 * RED1) div 7;
              4:
                colors[j, i] := (4 * RED0 + 3 * RED1) div 7;
              5:
                colors[j, i] := (3 * RED0 + 4 * RED1) div 7;
              6:
                colors[j, i] := (2 * RED0 + 5 * RED1) div 7;
              7:
                colors[j, i] := (RED0 + 6 * RED1) div 7;
            end
          else
            case colors[j, i] of
              0:
                colors[j, i] := RED0;
              1:
                colors[j, i] := RED1;
              2:
                colors[j, i] := (4 * RED0 + RED1) div 5;
              3:
                colors[j, i] := (3 * RED0 + 2 * RED1) div 5;
              4:
                colors[j, i] := (2 * RED0 + 3 * RED1) div 5;
              5:
                colors[j, i] := (RED0 + 4 * RED1) div 5;
              6:
                colors[j, i] := -127;
              7:
                colors[j, i] := 127;
            end;
          if ((4 * x + i) < AWidth) and ((4 * y + j) < AHeight) then
          begin
            offset := ((4 * y + j) * AWidth + (4 * x + i));
            lum := 2 * colors[j][i];
            ADest[offset].R := lum;
            ADest[offset].G := 0.0;
            ADest[offset].B := 0.0;
            ADest[offset].A := 127.0;
          end;
        end;

      end;
    end;
  end;
end;

procedure RGTC2_ToImf(ASource: Pointer; ADest: PIntermediateFormatArray; AColorFormat: TGLEnum; AWidth, AHeight: Integer);
var
  x, y, i, j, offset: Integer;
  RED0, RED1: Byte;
  colors: TU48BitBlock;
  bitmask: Int64;
  temp: PGLubyte;
begin

  temp := PGLubyte(ASource);
  for y := 0 to (AHeight div 4) - 1 do
  begin
    for x := 0 to (AWidth div 4) - 1 do
    begin
      RED0 := temp^;
      Inc(temp);
      RED1 := temp^;
      Inc(temp);
      bitmask := PInt64(temp)^;
      Inc(temp, 6);
      Decode48BitBlock(bitmask, colors);

      for j := 0 to 3 do
      begin
        for i := 0 to 3 do
        begin
          if RED0 > RED1 then
            case colors[j, i] of
              0:
                colors[j, i] := RED0;
              1:
                colors[j, i] := RED1;
              2:
                colors[j, i] := (6 * RED0 + RED1) div 7;
              3:
                colors[j, i] := (5 * RED0 + 2 * RED1) div 7;
              4:
                colors[j, i] := (4 * RED0 + 3 * RED1) div 7;
              5:
                colors[j, i] := (3 * RED0 + 4 * RED1) div 7;
              6:
                colors[j, i] := (2 * RED0 + 5 * RED1) div 7;
              7:
                colors[j, i] := (RED0 + 6 * RED1) div 7;
            end
          else
            case colors[j, i] of
              0:
                colors[j, i] := RED0;
              1:
                colors[j, i] := RED1;
              2:
                colors[j, i] := (4 * RED0 + RED1) div 5;
              3:
                colors[j, i] := (3 * RED0 + 2 * RED1) div 5;
              4:
                colors[j, i] := (2 * RED0 + 3 * RED1) div 5;
              5:
                colors[j, i] := (RED0 + 4 * RED1) div 5;
              6:
                colors[j, i] := 0;
              7:
                colors[j, i] := 255;
            end;
          if ((4 * x + i) < AWidth) and ((4 * y + j) < AHeight) then
          begin
            offset := ((4 * y + j) * AWidth + (4 * x + i));
            ADest[offset].R := colors[j][i];
            ADest[offset].B := 0.0;
          end;
        end;
      end;

      RED0 := temp^;
      Inc(temp);
      RED1 := temp^;
      Inc(temp);
      bitmask := PInt64(temp)^;
      Inc(temp, 6);
      Decode48BitBlock(bitmask, colors);

      for j := 0 to 3 do
      begin
        for i := 0 to 3 do
        begin
          if RED0 > RED1 then
            case colors[j, i] of
              0:
                colors[j, i] := RED0;
              1:
                colors[j, i] := RED1;
              2:
                colors[j, i] := (6 * RED0 + RED1) div 7;
              3:
                colors[j, i] := (5 * RED0 + 2 * RED1) div 7;
              4:
                colors[j, i] := (4 * RED0 + 3 * RED1) div 7;
              5:
                colors[j, i] := (3 * RED0 + 4 * RED1) div 7;
              6:
                colors[j, i] := (2 * RED0 + 5 * RED1) div 7;
              7:
                colors[j, i] := (RED0 + 6 * RED1) div 7;
            end
          else
            case colors[j, i] of
              0:
                colors[j, i] := RED0;
              1:
                colors[j, i] := RED1;
              2:
                colors[j, i] := (4 * RED0 + RED1) div 5;
              3:
                colors[j, i] := (3 * RED0 + 2 * RED1) div 5;
              4:
                colors[j, i] := (2 * RED0 + 3 * RED1) div 5;
              5:
                colors[j, i] := (RED0 + 4 * RED1) div 5;
              6:
                colors[j, i] := 0;
              7:
                colors[j, i] := 255;
            end;
          if ((4 * x + i) < AWidth) and ((4 * y + j) < AHeight) then
          begin
            offset := ((4 * y + j) * AWidth + (4 * x + i));
            ADest[offset].G := colors[j][i];
            ADest[offset].A := 255.0;
          end;
        end;
      end;
    end;
  end;
end;

procedure SRGTC2_ToImf(ASource: Pointer; ADest: PIntermediateFormatArray; AColorFormat: TGLEnum; AWidth, AHeight: Integer);
var
  x, y, i, j, offset: Integer;
  RED0, RED1: SmallInt;
  lum: Single;
  colors: T48BitBlock;
  bitmask: Int64;
  temp: PGLubyte;
begin

  temp := PGLubyte(ASource);
  for y := 0 to (AHeight div 4) - 1 do
  begin
    for x := 0 to (AWidth div 4) - 1 do
    begin
      RED0 := PSmallInt(temp)^;
      Inc(temp);
      RED1 := PSmallInt(temp)^;
      Inc(temp);
      bitmask := PInt64(temp)^;
      Inc(temp, 6);
      Decode48BitBlock(bitmask, colors);

      for j := 0 to 3 do
      begin
        for i := 0 to 3 do
        begin
          if RED0 > RED1 then
            case colors[j, i] of
              0:
                colors[j, i] := RED0;
              1:
                colors[j, i] := RED1;
              2:
                colors[j, i] := (6 * RED0 + RED1) div 7;
              3:
                colors[j, i] := (5 * RED0 + 2 * RED1) div 7;
              4:
                colors[j, i] := (4 * RED0 + 3 * RED1) div 7;
              5:
                colors[j, i] := (3 * RED0 + 4 * RED1) div 7;
              6:
                colors[j, i] := (2 * RED0 + 5 * RED1) div 7;
              7:
                colors[j, i] := (RED0 + 6 * RED1) div 7;
            end
          else
            case colors[j, i] of
              0:
                colors[j, i] := RED0;
              1:
                colors[j, i] := RED1;
              2:
                colors[j, i] := (4 * RED0 + RED1) div 5;
              3:
                colors[j, i] := (3 * RED0 + 2 * RED1) div 5;
              4:
                colors[j, i] := (2 * RED0 + 3 * RED1) div 5;
              5:
                colors[j, i] := (RED0 + 4 * RED1) div 5;
              6:
                colors[j, i] := -127;
              7:
                colors[j, i] := 127;
            end;
          if ((4 * x + i) < AWidth) and ((4 * y + j) < AHeight) then
          begin
            offset := ((4 * y + j) * AWidth + (4 * x + i));
            lum := 2 * colors[j][i];
            ADest[offset].R := lum;
            ADest[offset].B := 0.0;
          end;
        end;
      end;

      RED0 := PSmallInt(temp)^;
      Inc(temp);
      RED1 := PSmallInt(temp)^;
      Inc(temp);
      bitmask := PInt64(temp)^;
      Inc(temp, 6);
      Decode48BitBlock(bitmask, colors);

      for j := 0 to 3 do
      begin
        for i := 0 to 3 do
        begin
          if RED0 > RED1 then
            case colors[j, i] of
              0:
                colors[j, i] := RED0;
              1:
                colors[j, i] := RED1;
              2:
                colors[j, i] := (6 * RED0 + RED1) div 7;
              3:
                colors[j, i] := (5 * RED0 + 2 * RED1) div 7;
              4:
                colors[j, i] := (4 * RED0 + 3 * RED1) div 7;
              5:
                colors[j, i] := (3 * RED0 + 4 * RED1) div 7;
              6:
                colors[j, i] := (2 * RED0 + 5 * RED1) div 7;
              7:
                colors[j, i] := (RED0 + 6 * RED1) div 7;
            end
          else
            case colors[j, i] of
              0:
                colors[j, i] := RED0;
              1:
                colors[j, i] := RED1;
              2:
                colors[j, i] := (4 * RED0 + RED1) div 5;
              3:
                colors[j, i] := (3 * RED0 + 2 * RED1) div 5;
              4:
                colors[j, i] := (2 * RED0 + 3 * RED1) div 5;
              5:
                colors[j, i] := (RED0 + 4 * RED1) div 5;
              6:
                colors[j, i] := -127;
              7:
                colors[j, i] := 127;
            end;
          if ((4 * x + i) < AWidth) and ((4 * y + j) < AHeight) then
          begin
            offset := ((4 * y + j) * AWidth + (4 * x + i));
            lum := 2 * colors[j][i];
            ADest[offset].G := lum;
            ADest[offset].A := 127.0;
          end;
        end;
      end;
    end;
  end;
end;

{$IFDEF GLS_COMPILER_2005_UP}{$ENDREGION 'Compression'}{$ENDIF}
{$IFDEF GLS_COMPILER_2005_UP}{$REGION 'RGBA Float to OpenGL format image'}{$ENDIF}

procedure UnsupportedFromImf(ASource: PIntermediateFormatArray; ADest: Pointer; AColorFormat: TGLEnum; AWidth, AHeight: Integer);
begin
  raise EGLImageUtils.Create('Unimplemented type of conversion');
end;

procedure ImfToUbyte(ASource: PIntermediateFormatArray; ADest: Pointer; AColorFormat: TGLEnum; AWidth, AHeight: Integer);
var
  pDest: PByte;
  n: Integer;

  procedure SetChannel(AValue: Single);
{$IFDEF GLS_INLINE} inline;
{$ENDIF}
  begin
    pDest^ := Trunc(ClampValue(AValue, 0.0, 255.0));
    Inc(pDest);
  end;

  procedure SetChannelI(AValue: Single);
{$IFDEF GLS_INLINE} inline;
{$ENDIF}
  begin
    pDest^ := Trunc(AValue);
    Inc(pDest);
  end;

begin
  pDest := PByte(ADest);

  case AColorFormat of
{$INCLUDE ImgUtilCaseImf2GL.inc}
  else
    raise EGLImageUtils.Create(strInvalidType);
  end;
end;

{$IFDEF GLS_COMPILER_2005_UP}{$ENDREGION 'RGBA Float to OpenGL format image'}{$ENDIF}
{$IFDEF GLS_COMPILER_2005_UP}{$REGION 'Image filters'}{$ENDIF}

function ImageBoxFilter(Value: Single): Single;
begin
  if (Value > -0.5) and (Value <= 0.5) then
    Result := 1.0
  else
    Result := 0.0;
end;

function ImageTriangleFilter(Value: Single): Single;
begin
  if Value < 0.0 then
    Value := -Value;
  if Value < 1.0 then
    Result := 1.0 - Value
  else
    Result := 0.0;
end;

function ImageHermiteFilter(Value: Single): Single;
begin
  if Value < 0.0 then
    Value := -Value;
  if Value < 1 then
    Result := (2 * Value - 3) * Sqr(Value) + 1
  else
    Result := 0;
end;

function ImageBellFilter(Value: Single): Single;
begin
  if Value < 0.0 then
    Value := -Value;
  if Value < 0.5 then
    Result := 0.75 - Sqr(Value)
  else if Value < 1.5 then
  begin
    Value := Value - 1.5;
    Result := 0.5 * Sqr(Value);
  end
  else
    Result := 0.0;
end;

function ImageSplineFilter(Value: Single): Single;
var
  temp: Single;
begin
  if Value < 0.0 then
    Value := -Value;
  if Value < 1.0 then
  begin
    temp := Sqr(Value);
    Result := 0.5 * temp * Value - temp + 2.0 / 3.0;
  end
  else if Value < 2.0 then
  begin
    Value := 2.0 - Value;
    Result := Sqr(Value) * Value / 6.0;
  end
  else
    Result := 0.0;
end;

function ImageLanczos3Filter(Value: Single): Single;
const
  Radius = 3.0;
begin
  Result := 1;
  if Value = 0 then
    Exit;
  if Value < 0.0 then
    Value := -Value;
  if Value < Radius then
  begin
    Value := Value * pi;
    Result := Radius * Sin(Value) * Sin(Value / Radius) / (Value * Value);
  end
  else
    Result := 0.0;
end;

function ImageMitchellFilter(Value: Single): Single;
const
  B = 1.0 / 3.0;
  C = 1.0 / 3.0;
var
  temp: Single;
begin
  if Value < 0.0 then
    Value := -Value;
  temp := Sqr(Value);
  if Value < 1.0 then
  begin
    Value := (((12.0 - 9.0 * B - 6.0 * C) * (Value * temp)) + ((-18.0 + 12.0 * B + 6.0 * C) * temp) + (6.0 - 2.0 * B));
    Result := Value / 6.0;
  end
  else if Value < 2.0 then
  begin
    Value := (((-B - 6.0 * C) * (Value * temp)) + ((6.0 * B + 30.0 * C) * temp) + ((-12.0 * B - 48.0 * C) * Value) + (8.0 * B + 24.0 * C));
    Result := Value / 6.0;
  end
  else
    Result := 0.0;
end;

type
  // Contributor for a pixel
  TContributor = record
    pixel: Integer; // Source pixel
    weight: Single; // Pixel weight
  end;

  TContributorList = array [0 .. MaxInt div (2*SizeOf(TContributor))] of TContributor;
  PContributorList = ^TContributorList;

  // List of source pixels contributing to a destination pixel
  TCList = record
    n: Integer;
    p: PContributorList;
  end;

  TCListList = array [0 .. MaxInt div (2*SizeOf(TCList))] of TCList;
  PCListList = ^TCListList;

{$IFDEF GLS_COMPILER_2005_UP}{$ENDREGION 'Image filters'}{$ENDIF}
{$IFDEF GLS_COMPILER_2005_UP}{$REGION 'Data type conversion table'}{$ENDIF}

type
  TConvertTableRec = record
    type_: TGLEnum;
    proc1: TConvertToImfProc;
    proc2: TConvertFromInfProc;
  end;

const
  cConvertTable: array [0 .. 36] of TConvertTableRec = ((type_: GL_UNSIGNED_BYTE; proc1: UbyteToImf; proc2: ImfToUbyte),

    (type_: GL_UNSIGNED_BYTE_3_3_2; proc1: Ubyte332ToImf; proc2: UnsupportedFromImf),

    (type_: GL_UNSIGNED_BYTE_2_3_3_REV; proc1: Ubyte233RToImf; proc2: UnsupportedFromImf),

    (type_: GL_BYTE; proc1: ByteToImf; proc2: UnsupportedFromImf),

    (type_: GL_UNSIGNED_SHORT; proc1: UShortToImf; proc2: UnsupportedFromImf),

    (type_: GL_SHORT; proc1: ShortToImf; proc2: UnsupportedFromImf),

    (type_: GL_UNSIGNED_INT; proc1: UIntToImf; proc2: UnsupportedFromImf),

    (type_: GL_INT; proc1: IntToImf; proc2: UnsupportedFromImf),

    (type_: GL_FLOAT; proc1: FloatToImf; proc2: UnsupportedFromImf),

    (type_: GL_HALF_FLOAT; proc1: HalfFloatToImf; proc2: UnsupportedFromImf),

    (type_: GL_UNSIGNED_INT_8_8_8_8; proc1: UInt8888ToImf; proc2: UnsupportedFromImf),

    (type_: GL_UNSIGNED_INT_8_8_8_8_REV; proc1: UInt8888RevToImf; proc2: UnsupportedFromImf),

    (type_: GL_UNSIGNED_SHORT_4_4_4_4; proc1: UShort4444ToImf; proc2: UnsupportedFromImf),

    (type_: GL_UNSIGNED_SHORT_4_4_4_4_REV; proc1: UShort4444RevToImf; proc2: UnsupportedFromImf),

    (type_: GL_UNSIGNED_SHORT_5_6_5; proc1: UShort565ToImf; proc2: UnsupportedFromImf),

    (type_: GL_UNSIGNED_SHORT_5_6_5_REV; proc1: UShort565RevToImf; proc2: UnsupportedFromImf),

    (type_: GL_UNSIGNED_SHORT_5_5_5_1; proc1: UShort5551ToImf; proc2: UnsupportedFromImf),

    (type_: GL_UNSIGNED_SHORT_1_5_5_5_REV; proc1: UShort5551RevToImf; proc2: UnsupportedFromImf),

    (type_: GL_UNSIGNED_INT_10_10_10_2; proc1: UInt_10_10_10_2_ToImf; proc2: UnsupportedFromImf),

    (type_: GL_UNSIGNED_INT_2_10_10_10_REV; proc1: UInt_10_10_10_2_Rev_ToImf; proc2: UnsupportedFromImf),

    (type_: GL_COMPRESSED_RGB_S3TC_DXT1_EXT; proc1: DXT1_ToImf; proc2: UnsupportedFromImf),

    (type_: GL_COMPRESSED_RGBA_S3TC_DXT1_EXT; proc1: DXT1_ToImf; proc2: UnsupportedFromImf),

    (type_: GL_COMPRESSED_RGBA_S3TC_DXT3_EXT; proc1: DXT3_ToImf; proc2: UnsupportedFromImf),

    (type_: GL_COMPRESSED_RGBA_S3TC_DXT5_EXT; proc1: DXT5_ToImf; proc2: UnsupportedFromImf),

    (type_: GL_COMPRESSED_SRGB_S3TC_DXT1_EXT; proc1: UnsupportedToImf; proc2: UnsupportedFromImf),

    (type_: GL_COMPRESSED_SRGB_ALPHA_S3TC_DXT1_EXT; proc1: UnsupportedToImf; proc2: UnsupportedFromImf),

    (type_: GL_COMPRESSED_SRGB_ALPHA_S3TC_DXT3_EXT; proc1: UnsupportedToImf; proc2: UnsupportedFromImf),

    (type_: GL_COMPRESSED_SRGB_ALPHA_S3TC_DXT5_EXT; proc1: UnsupportedToImf; proc2: UnsupportedFromImf),

    (type_: GL_COMPRESSED_LUMINANCE_LATC1_EXT; proc1: LATC1_ToImf; proc2: UnsupportedFromImf),

    (type_: GL_COMPRESSED_SIGNED_LUMINANCE_LATC1_EXT; proc1: SLATC1_ToImf; proc2: UnsupportedFromImf),

    (type_: GL_COMPRESSED_LUMINANCE_ALPHA_LATC2_EXT; proc1: LATC2_ToImf; proc2: UnsupportedFromImf),

    (type_: GL_COMPRESSED_SIGNED_LUMINANCE_ALPHA_LATC2_EXT; proc1: SLATC2_ToImf; proc2: UnsupportedFromImf),

    (type_: GL_COMPRESSED_LUMINANCE_ALPHA_3DC_ATI; proc1: UnsupportedToImf; proc2: UnsupportedFromImf),

    (type_: GL_COMPRESSED_RED_RGTC1; proc1: RGTC1_ToImf; proc2: UnsupportedFromImf),

    (type_: GL_COMPRESSED_SIGNED_RED_RGTC1; proc1: SRGTC1_ToImf; proc2: UnsupportedFromImf),

    (type_: GL_COMPRESSED_RG_RGTC2; proc1: RGTC2_ToImf; proc2: UnsupportedFromImf),

    (type_: GL_COMPRESSED_SIGNED_RG_RGTC2; proc1: SRGTC2_ToImf; proc2: UnsupportedFromImf));

{$IFDEF GLS_COMPILER_2005_UP}{$ENDREGION 'Data type conversion table'}{$ENDIF}

procedure ConvertImage(const ASrc: Pointer; const ADst: Pointer; ASrcColorFormat, ADstColorFormat: TGLEnum; ASrcDataType, ADstDataType: TGLEnum; AWidth, AHeight: Integer);
var
  ConvertToIntermediateFormat: TConvertToImfProc;
  ConvertFromIntermediateFormat: TConvertFromInfProc;
  i, size: Integer;
  tempBuf: PIntermediateFormatArray;
begin
  if AWidth < 1 then
    Exit;
  AHeight := MaxInteger(1, AHeight);
  // Allocate memory
  size := AWidth * AHeight * SizeOf(TIntermediateFormat);
  GetMem(tempBuf, size);
  FillChar(tempBuf^, size, $00);

  // Find function to convert external format to intermediate format
  ConvertToIntermediateFormat := UnsupportedToImf;
  for i := 0 to high(cConvertTable) do
  begin
    if ASrcDataType = cConvertTable[i].type_ then
    begin
      ConvertToIntermediateFormat := cConvertTable[i].proc1;
      break;
    end;
  end;

  try
    ConvertToIntermediateFormat(ASrc, tempBuf, ASrcColorFormat, AWidth, AHeight);
  except
    FreeMem(tempBuf);
    raise;
  end;

  // Find function to convert intermediate format to external format
  ConvertFromIntermediateFormat := UnsupportedFromImf;
  for i := 0 to high(cConvertTable) do
  begin
    if ADstDataType = cConvertTable[i].type_ then
    begin
      ConvertFromIntermediateFormat := cConvertTable[i].proc2;
      break;
    end;
  end;

  try
    ConvertFromIntermediateFormat(tempBuf, ADst, ADstColorFormat, AWidth, AHeight);
  except
    FreeMem(tempBuf);
    raise;
  end;

  FreeMem(tempBuf);
end;

procedure RescaleImage(const ASrc: Pointer; const ADst: Pointer; AColorFormat: TGLEnum; ADataType: TGLEnum; AFilter: TImageFilterFunction; ASrcWidth, ASrcHeight, ADstWidth, ADstHeight: Integer);
var
  ConvertToIntermediateFormat: TConvertToImfProc;
  ConvertFromIntermediateFormat: TConvertFromInfProc;
  i, j, k, n, size: Integer;
  tempBuf1, tempBuf2, SourceLine, DestLine: PIntermediateFormatArray;
  contrib: PCListList;
  xscale, yscale: Single; // Zoom scale factors
  width, fscale, weight: Single; // Filter calculation variables
  center: Single;		// Filter calculation variables
  left, right: Integer; // Filter calculation variables
  color1, color2: TIntermediateFormat;
begin
  if (ASrcWidth < 1) or (ADstWidth < 1) then
    Exit;
  ASrcHeight := MaxInteger(1, ASrcHeight);
  ADstHeight := MaxInteger(1, ADstHeight);

  // Allocate memory
  size := ASrcWidth * ASrcHeight * SizeOf(TIntermediateFormat);
  GetMem(tempBuf1, size);
  FillChar(tempBuf1^, size, $00);

  // Find function to convert external format to intermediate format
  ConvertToIntermediateFormat := UnsupportedToImf;
  for i := 0 to high(cConvertTable) do
  begin
    if ADataType = cConvertTable[i].type_ then
    begin
      ConvertToIntermediateFormat := cConvertTable[i].proc1;
      ConvertFromIntermediateFormat := cConvertTable[i].proc2;
      break;
    end;
  end;

  try
    ConvertToIntermediateFormat(ASrc, tempBuf1, AColorFormat, ASrcWidth, ASrcHeight);
  except
    FreeMem(tempBuf1);
    raise;
  end;

  // Rescale

  if ASrcWidth = 1 then
    xscale := ADstWidth / ASrcWidth
  else
    xscale := (ADstWidth - 1) / (ASrcWidth - 1);
  if ASrcHeight = 1 then
    yscale := ADstHeight / ASrcHeight
  else
    yscale := (ADstHeight - 1) / (ASrcHeight - 1);
  // Pre-calculate filter contributions for a row
  GetMem(contrib, ADstWidth * SizeOf(TCList));
  // Horizontal sub-sampling
  // Scales from bigger to smaller width
  if xscale < 1.0 then
  begin
    width := cFilterWidth / xscale;
    fscale := 1.0 / xscale;
    for i := 0 to ADstWidth-1 do
    begin
      contrib^[i].n := 0;
      GetMem(contrib^[i].p, trunc(width * 2.0 + 1) * sizeof(TContributor));
      center := i / xscale;
      left := floor(center - width);
      right := ceil(center + width);
      for j := left to right do
      begin
        weight := AFilter((center - j) / fscale) / fscale;
        if weight = 0.0 then
          continue;
        if (j < 0) then
          n := -j
        else if (j >= ASrcWidth) then
          n := ASrcWidth - j + ASrcWidth - 1
        else
          n := j;
        k := contrib^[i].n;
        contrib^[i].n := contrib^[i].n + 1;
        contrib^[i].p^[k].pixel := n;
        contrib^[i].p^[k].weight := weight;
      end;
    end;
  end
  else
  // Horizontal super-sampling
  // Scales from smaller to bigger width
  begin
    for i := 0 to ADstWidth-1 do
    begin
      contrib^[i].n := 0;
      GetMem(contrib^[i].p, Trunc(cFilterWidth * 2.0 + 1) * SizeOf(TContributor));
      center := i / xscale;
      left := Floor(center - cFilterWidth);
      right := Ceil(center + cFilterWidth);
      for j := left to right do
      begin
        weight := AFilter(center - j);
        if weight = 0.0 then
          continue;
        if (j < 0) then
          n := -j
        else if (j >= ASrcWidth) then
          n := ASrcWidth - j + ASrcWidth - 1
        else
          n := j;
        k := contrib^[i].n;
        contrib^[i].n := contrib^[i].n + 1;
        contrib^[i].p^[k].pixel := n;
        contrib^[i].p^[k].weight := weight;
      end;
    end;
  end;

  size := ADstWidth * ASrcHeight * SizeOf(TIntermediateFormat);
  GetMem(tempBuf2, size);

  // Apply filter to sample horizontally from Src to Work
  for K := 0 to ASrcHeight-1 do
  begin
    SourceLine := @tempBuf1[k*ASrcWidth];
    DestLine := @tempBuf2[k*ADstWidth];
    for I := 0 to ADstWidth-1 do
    begin
      color1 := cSuperBlack;
      for J := 0 to contrib^[i].n-1 do
      begin
        weight := contrib^[i].p^[j].weight;
        if weight = 0.0 then
          continue;
        color2 := SourceLine[contrib^[i].p^[j].pixel];
        color1.r := color1.r + color2.r * weight;
        color1.g := color1.g + color2.g * weight;
        color1.b := color1.b + color2.b * weight;
        color1.a := color1.a + color2.a * weight;
      end;
      // Set new pixel value
      DestLine[I] := color1;
    end;
  end;

  // Free the memory allocated for horizontal filter weights
  for I := 0 to ADstWidth-1 do
    FreeMem(contrib^[I].p);
  FreeMem(contrib);

  // Pre-calculate filter contributions for a column
  GetMem(contrib, ADstHeight * SizeOf(TCList));
  // Vertical sub-sampling
  // Scales from bigger to smaller height
  if yscale < 1.0 then
  begin
    width := cFilterWidth / yscale;
    fscale := 1.0 / yscale;
    for I := 0 to ADstHeight-1 do
    begin
      contrib^[i].n := 0;
      GetMem(contrib^[i].p, Trunc(width * 2.0 + 1) * SizeOf(TContributor));
      center := i / yscale;
      left := Floor(center - width);
      right := Ceil(center + width);
      for J := left to right do
      begin
        weight := AFilter((center - j) / fscale) / fscale;
        if weight = 0.0 then
          continue;
        if (j < 0) then
          n := -j
        else if (j >= ASrcHeight) then
          n := ASrcHeight - j + ASrcHeight - 1
        else
          n := j;
        k := contrib^[i].n;
        contrib^[i].n := contrib^[i].n + 1;
        contrib^[i].p^[k].pixel := n;
        contrib^[i].p^[k].weight := weight;
      end;
    end
  end else
  // Vertical super-sampling
  // Scales from smaller to bigger height
  begin
    for I := 0 to ADstHeight-1 do
    begin
      contrib^[i].n := 0;
      GetMem(contrib^[i].p, trunc(cFilterWidth * 2.0 + 1) * SizeOf(TContributor));
      center := i / yscale;
      left := Floor(center - cFilterWidth);
      right := Ceil(center + cFilterWidth);
      for j := left to right do
      begin
        weight := AFilter(center - j);
        if weight = 0.0 then
          continue;
        if j < 0 then
          n := -j
        else if (j >= ASrcHeight) then
          n := ASrcHeight - j + ASrcHeight - 1
        else
          n := j;
        k := contrib^[i].n;
        contrib^[i].n := contrib^[i].n + 1;
        contrib^[i].p^[k].pixel := n;
        contrib^[i].p^[k].weight := weight;
      end;
    end;
  end;

  size := ADstWidth * ADstHeight * SizeOf(TIntermediateFormat);
  ReallocMem(tempBuf1, size);

  // Apply filter to sample vertically from Work to Dst
  for K := 0 to ADstWidth-1 do
  begin
    for I := 0 to ADstHeight-1 do
    begin
      color1 := cSuperBlack;
      for J := 0 to contrib^[i].n-1 do
      begin
        weight := contrib^[i].p^[j].weight;
        if weight = 0.0 then
          continue;
        color2 := tempBuf2[K + contrib^[i].p^[j].pixel * ADstWidth];
        color1.r := color1.r + color2.r * weight;
        color1.g := color1.g + color2.g * weight;
        color1.b := color1.b + color2.b * weight;
        color1.a := color1.a + color2.a * weight;
      end;
      tempBuf1[K + I * ADstWidth] := color1;
    end;
  end;

  // Free the memory allocated for vertical filter weights
  for i := 0 to ADstHeight-1 do
    FreeMem(contrib^[i].p);

  FreeMem(contrib);

  FreeMem(tempBuf2);
  // Back to native image format
  try
    ConvertFromIntermediateFormat(tempBuf1, ADst, AColorFormat, ADstWidth, ADstHeight);
  except
    FreeMem(tempBuf1);
    raise;
  end;
  FreeMem(tempBuf1);
end;

end.
